"""
Emission-geometry forward model for plumeIR v3.

Three-layer radiative transfer for an upward-looking plume against cold sky:

    L = L_dist · T_plume · T_slice · T_prox
        + (L_plume + L_slice) · T_prox
        + L_prox

Layers (by altitude): proximal (observer → plume bottom), slice (background
atmosphere co-located with the plume), distal (plume top → space).  The
atmospheric layer radiances are reconstructed from the pre-computed effective
emission temperature with an analytic offset:

    L_layer(dT) = rad(ν, T_eff + dT) · (1 − T_layer)

so each layer's temperature is adjustable without a 2-D LUT (see
AtmLayerLUT).

Atmospheric **gas** columns are not free parameters here, with one exception.
H2O is scaled across all three layers by ``H2O_sc``; everything else is fixed
by the profile — except gases in ``standards.EMISSION_DISTAL_GASES`` (O3),
which get an independent distal column scaling ``{gas}_sc`` applied additively
in OD space.  v2 instead scaled every atmospheric gas inside the plume *slice*,
which carries only 12.3% of the column optical depth and 0.3% of the O3.

Plume self-emission uses **absorption** OD (emissivity = absorptivity):

    L_plume = rad(ν, plume_temp) · (1 − exp(−OD_abs))

while plume transmittance of background light uses **extinction** OD.

Output / fitting modes:
  - radiance (default)
  - brightness temperature (``use_bbt``) — output transform via bbt
  - difference (``fit_difference``) — fits ``L_model − L_clear`` against a
    field clear-sky spectrum (distinct fitting space)
"""

from __future__ import annotations

import logging

import numpy as np

from plumeIR.forward.base import (
    ForwardModel,
    apply_ils,
    apply_shift_offset,
    build_background_poly,
    gas_optical_depth,
)
from plumeIR.forward.ils import ILSKernel, kernel_trim_fov, make_ils
from plumeIR.config.objects import EmissionGeometry, FitWindowConfig, Retrieval
from plumeIR.parameters.parameter import ParameterKind, ParameterSet
from plumeIR.physics import bbt, rad
from plumeIR.reference.reference import Reference

logger = logging.getLogger(__name__)


class EmissionForwardModel(ForwardModel):
    """
    Three-layer emission forward model.

    Parameters
    ----------
    reference : Reference
        Emission reference: ``atm_layers`` (prox/slice/dist), ``L_clear``,
        plume gas LUTs, and plume aerosol LUTs (with absorption).
    params : ParameterSet
        Immutable parameter set.
    geometry : EmissionGeometry
        Emission geometry (plume height/thickness/temp, elevation, …).
    retrieval : Retrieval
        Retrieval configuration (ILS, apodization).
    window : FitWindowConfig
        Spectral window and species configuration.
    wn_meas : np.ndarray
        Measurement wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    """

    def __init__(
        self,
        reference: Reference,
        params:    ParameterSet,
        geometry:  EmissionGeometry,
        retrieval: Retrieval,
        window:    FitWindowConfig,
        wn_meas:   np.ndarray,
    ) -> None:
        self.ref       = reference
        self.params    = params
        self.geometry  = geometry
        self.retrieval = retrieval
        self.window    = window
        self.wn_meas   = np.asarray(wn_meas, dtype=float)

        if not reference.atm_layers:
            raise ValueError(
                "EmissionForwardModel requires a reference with atm_layers "
                "(build with build_emission_reference)"
            )

        # Output / fitting modes from the geometry
        self.use_bbt        = geometry.use_bbt
        self.fit_difference = geometry.fit_difference
        # Off by default: every result before 2026-08-26 was produced without
        # it, and paired comparison needs the term switchable.
        self.model_aero_scattering = bool(
            getattr(geometry, "model_aero_scattering", False)
        )
        # Prefer the reference's resolved plume temperature (ambient from
        # the profile when the config left it unset); fall back to the
        # geometry for references built before the field existed.
        self._plume_temp = (
            reference.plume_temp if getattr(reference, "plume_temp", None)
            is not None else geometry.plume_temp
        )
        if self.fit_difference and reference.L_clear is None:
            raise ValueError("fit_difference=True requires reference.L_clear")

    # ------------------------------------------------------------------
    # Plume optical depths (extinction for transmittance; absorption for
    # self-emission)
    # ------------------------------------------------------------------

    def _plume_optical_depths(self, p: dict) -> tuple[np.ndarray, np.ndarray]:
        """
        Return ``(OD_ext, OD_abs)`` for the plume layer.

        Gases absorb without significant IR scattering, so their extinction
        and absorption OD are equal.  Aerosols differ: extinction (MEC) vs
        absorption (MAC).
        """
        n = self.ref.wn.size
        OD_ext = np.zeros(n)
        OD_abs = np.zeros(n)

        # Plume gases at the plume temperature — fitted when fit_tplume is
        # set, otherwise the ambient a priori from the profile.  The SAME
        # temperature drives the Planck self-emission term in model_highres:
        # Kirchhoff, for a single near-isothermal layer.
        gas_temp = p.get("plume_temp", self._plume_temp)
        for species in self.window.plume_gases:
            od = gas_optical_depth(self.ref.gas_luts[species], p[species], gas_temp)
            OD_ext = OD_ext + od
            OD_abs = OD_abs + od

        # Plume aerosols: extinction and absorption tables
        for species in self.window.plume_aero:
            lut     = self.ref.aero_luts[species]
            scaling = p[species]
            r0      = p[f"r0_{species}"]
            wt      = p.get(f"wt_{species}")
            OD_ext = OD_ext + scaling * lut.query(r0, wt)
            OD_abs = OD_abs + scaling * lut.query_abs(r0, wt)

        return OD_ext, OD_abs

    # ------------------------------------------------------------------
    # High-resolution model
    # ------------------------------------------------------------------

    def _scattering_source(self, wn: np.ndarray) -> np.ndarray:
        """
        Radiance scattered *into* the beam by plume aerosol, per unit
        scattering emissivity.

        **Variant (a): graybody at the surface ambient temperature.**  This is
        deliberately the crudest of three candidates, because it discriminates.

        The forward is otherwise pure absorption/emission: aerosol enters as
        extinction in ``T_plume`` and absorption in ``L_plume``, so the ~40% of
        extinction that is *redirection* is removed from the beam and never
        returned.  Measured at episode peak on 2019-09-11: single-scattering
        albedo 0.38 (ash, r0 1.69 µm) and 0.43 (H2SO4), scattering optical
        depth ~0.19 — about 17% of the beam — with asymmetry parameter g =
        +0.56, i.e. a mean scattering angle near 56°, so the redistribution is
        wide rather than a narrow forward cone.

        In the thermal IR the sky is far brighter at low elevation than at the
        25° line of sight, so what is scattered in is preferentially *warm,
        low-elevation* radiance.  That has two separable consequences:

          1. a radiance-level boost   -> drives dt_prox / dt_dist
          2. an imported line spectrum from a longer, warmer path
                                      -> drives H2O_sc / O3_sc

        A graybody supplies only (1).  So if this term collapses the dt_*
        excursions but leaves H2O_sc / O3_sc largely intact, the mechanism is
        confirmed AND the graybody is identified as the limiting approximation
        — which points at the more expensive variants:

          (b) ``reference.L_clear``      — has line structure, but for the
              nominal 25° cold-sky path, i.e. the wrong elevation and level.
          (c) a dedicated low-elevation clear — correct structure and level,
              at one extra RFM run per H2O_sc node.

        Replacing this method is the whole of that change; the assembly in
        ``model_highres`` does not move.
        """
        t_src = self.ref.T_surface
        if t_src is None:
            raise ValueError(
                "model_aero_scattering=True requires Reference.T_surface "
                "(rebuild the reference: schema >= 3)"
            )
        return rad(wn, t_src)

    def model_highres(self, p_vec: np.ndarray, plume: bool = True) -> np.ndarray:
        """
        High-resolution model on the model grid (pre-ILS).

        Returns radiance, brightness temperature, or difference spectrum
        depending on the geometry's output mode.

        Parameters
        ----------
        p_vec : np.ndarray
            Varied-parameter vector.
        plume : bool
            ``False`` removes the plume layer (zero optical depth, no emission,
            no scattering), leaving the clear sky at the same atmospheric state
            (:meth:`continuum`).
        """
        p  = self.params.unpack(p_vec)
        wn = self.ref.wn

        poly_coeffs = [p[n] for n in self.params.names_by_kind(ParameterKind.POLYNOMIAL)]
        bg = build_background_poly(poly_coeffs, wn)

        h2o_sc = p.get("H2O_sc", 1.0)

        # Background atmospheric layers (analytic temperature offsets via T_eff)
        L_prox,  T_prox  = self.ref.atm_layers["prox"].emission(h2o_sc, p.get("dt_prox", 0.0))
        has_slice = "slice" in self.ref.atm_layers
        if has_slice:
            # dt_slice tied to 0 — no fitted parameter touches the slice.
            L_slice, T_slice = self.ref.atm_layers["slice"].emission(h2o_sc, 0.0)
        L_dist,  _T_dist = self.ref.atm_layers["dist"].emission(
            h2o_sc, p.get("dt_dist", 0.0), extra_od=self._distal_extra_od(p),
        )

        # Plume layer
        if plume:
            OD_ext, OD_abs = self._plume_optical_depths(p)
        else:
            OD_ext = OD_abs = np.zeros_like(wn)
        T_plume   = np.exp(-OD_ext)
        emiss_abs = 1.0 - np.exp(-OD_abs)
        plume_temp = p.get("plume_temp", self._plume_temp)
        L_plume = rad(wn, plume_temp) * emiss_abs

        # Aerosol single scattering into the beam.  OD_ext - OD_abs is exactly
        # the aerosol scattering OD: plume gases are added to BOTH sums (gases
        # do not scatter), so they cancel in the difference.
        if self.model_aero_scattering:
            OD_scat  = np.maximum(OD_ext - OD_abs, 0.0)
            L_plume = L_plume + self._scattering_source(wn) * (
                1.0 - np.exp(-OD_scat)
            )

        # Three-layer radiative transfer
        if has_slice:
            # 3-layer: plume and slice are co-located and treated as additive
            # emitters — neither attenuates the other.
            L = (
                L_dist * T_plume * T_slice * T_prox
                + (L_plume + L_slice) * T_prox
                + L_prox
            )
        else:
            # 2-layer: exact sequential RT, plume strictly between prox and dist.
            L = L_dist * T_plume * T_prox + L_plume * T_prox + L_prox

        # Output mode
        if self.use_bbt:
            L = bbt(wn, L)

        if self.fit_difference:
            L_clear = self.ref.L_clear
            if self.use_bbt:
                L_clear = bbt(wn, L_clear)
            return (L - L_clear) * bg

        return L * bg

    # ------------------------------------------------------------------
    # Independent per-gas column scaling on the distal layer
    # ------------------------------------------------------------------

    def _distal_extra_od(self, p: dict) -> np.ndarray | None:
        """
        Extra distal optical depth from independently scaled gases.

        ``OD_extra = Σ (s_gas − 1) · OD_gas,dist``, exact because optical depth
        is additive across species and these gases neither scale with
        ``H2O_sc`` nor carry a self-continuum.  Returned for addition **before**
        the exponential so saturated cores stay saturated.

        Returns ``None`` when no such gas is active, leaving the layer
        untouched.
        """
        table = self.ref.atm_layer_gas_od.get("dist") if self.ref.atm_layer_gas_od else None
        if not table:
            return None

        extra = None
        for gas, od_gas in table.items():
            scaling = p.get(f"{gas}_sc")
            if scaling is None:
                continue
            term = (float(scaling) - 1.0) * od_gas
            extra = term if extra is None else extra + term
        return extra

    # ------------------------------------------------------------------
    # ILS kernel
    # ------------------------------------------------------------------

    def _make_kernel(self, p: dict) -> ILSKernel:
        """Build the ILS kernel."""
        wn_model  = self.ref.wn
        nper_wn   = 1.0 / float(np.mean(np.diff(wn_model)))
        wn_center = 0.5 * (wn_model[0] + wn_model[-1])
        return make_ils(
            fov=p["fov"], max_opd=p.get("max_opd", self.retrieval.max_opd),
            nper_wn=nper_wn, wn_center=wn_center,
            apod_type=self.retrieval.apod_type,
            trim_opd=self.retrieval.max_opd,   # fixed length while max_opd varies
            trim_fov=kernel_trim_fov(self.params["fov"]),   # fixed length while fov varies
        )

    # ------------------------------------------------------------------
    # Callable interface
    # ------------------------------------------------------------------

    def __call__(self, p_vec: np.ndarray) -> np.ndarray:
        p = self.params.unpack(p_vec)
        F = self.model_highres(p_vec)
        F = apply_ils(F, self._make_kernel(p))
        return apply_shift_offset(
            F, self.ref.wn, self.wn_meas,
            shift=p.get("nu_shift", 0.0), offset=p.get("offset", 0.0),
        )

    def continuum(self, p_vec: np.ndarray) -> np.ndarray:
        """
        The clear sky: the fitted model with the plume removed.

        ``L_dist·T_prox + L_prox`` at the fitted atmospheric state (``H2O_sc``,
        ``dt_prox``, ``dt_dist``, distal scalings) and polynomial, through the
        same ILS, shift and offset as the model, so the gap between the two is
        the plume's contribution.  The emission counterpart of v2's ``bkg``,
        which re-evaluated the model with only its atmospheric and spectral
        parameters (``plumeIR_v2_archive/analysis.py``, lines 2091-2097).  The
        polynomial alone would not do here: it multiplies the whole radiance.
        """
        p = self.params.unpack(p_vec)
        F = self.model_highres(p_vec, plume=False)
        F = apply_ils(F, self._make_kernel(p))
        return apply_shift_offset(
            F, self.ref.wn, self.wn_meas,
            shift=p.get("nu_shift", 0.0), offset=p.get("offset", 0.0),
        )
