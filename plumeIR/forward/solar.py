"""
Solar-occultation forward model for plumeIR v3.

Pure transmittance — the solar beam attenuated by the slant atmospheric
column and an optional plume:

    F = solar_continuum(poly) · exp(−m · (OD_atm + OD_plume))   →  ILS, shift, offset

where ``m = 1/sin(elev)`` is the airmass.

Key points:
  - Atmospheric gases are the retrieval *targets* (total-column).  Each is a
    vertical OD spectrum (``reference.atm_gas_od``) scaled linearly by its
    column-scaling parameter.  This is a profile-scaling retrieval (à la
    COCCON/PROFFAST).
  - **Airmass is a fixed per-spectrum input**, not a fit parameter — fitting
    it would be degenerate with the gas column scalings.  Each spectrum
    supplies its own ``airmass`` (from the ephemeris/tracker) at construction.
  - No emission term (solar beam dominates).
  - The solar continuum is carried by the background polynomial; a Fraunhofer
    solar line list is a deferred refinement.
  - An optional plume (volcanic occultation) reuses the plume gas/aerosol
    LUTs; its slant path is airmass-scaled like the atmosphere.
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
from plumeIR.config.objects import FitWindowConfig, Retrieval, SolarGeometry
from plumeIR.parameters.parameter import ParameterKind, ParameterSet
from plumeIR.reference.reference import Reference

logger = logging.getLogger(__name__)


class SolarForwardModel(ForwardModel):
    """
    Solar-occultation transmittance forward model.

    Parameters
    ----------
    reference : Reference
        Solar reference: ``atm_gas_od`` (vertical OD per atmospheric gas) and
        optional plume gas/aerosol LUTs.
    params : ParameterSet
        Immutable parameter set.
    geometry : SolarGeometry
        Solar geometry.
    retrieval : Retrieval
        Retrieval configuration (ILS, apodization).
    window : FitWindowConfig
        Spectral window and species configuration.
    wn_meas : np.ndarray
        Measurement wavenumber grid [cm⁻¹].
    airmass : float
        Slant-path airmass ``m = 1/sin(elev)`` for THIS spectrum.  A fixed,
        known per-spectrum input (default 1.0 = vertical).  Not a fit
        parameter.
    """

    def __init__(
        self,
        reference: Reference,
        params:    ParameterSet,
        geometry:  SolarGeometry,
        retrieval: Retrieval,
        window:    FitWindowConfig,
        wn_meas:   np.ndarray,
        airmass:   float = 1.0,
    ) -> None:
        self.ref       = reference
        self.params    = params
        self.geometry  = geometry
        self.retrieval = retrieval
        self.window    = window
        self.wn_meas   = np.asarray(wn_meas, dtype=float)
        self.airmass   = float(airmass)

        if not reference.atm_gas_od and not window.plume_gases:
            raise ValueError(
                "SolarForwardModel requires atm_gas_od or plume gases "
                "(build with build_solar_reference)"
            )

        # Plume gas temperature (fixed) for absorption-shape queries
        self._plume_temp = (
            geometry.plume_temp
            if geometry.plume_temp is not None
            else float(np.median([300.0, 1000.0]))
        )

    # ------------------------------------------------------------------
    # Optical depths
    # ------------------------------------------------------------------

    def _total_optical_depth(self, p: dict) -> np.ndarray:
        """
        Total vertical optical depth (atmosphere + plume), pre-airmass.

        Atmospheric gases scale their vertical OD linearly; plume gases and
        aerosols use their LUTs.
        """
        OD = np.zeros(self.ref.wn.size)

        # Atmospheric gases — column scaling of the vertical OD.  Lines and the
        # foreign continuum scale linearly with column (∝ s); the H2O
        # self-continuum scales as column² (∝ s²).  For continuum gases the
        # reference carries the self-continuum component B (atm_gas_od_cont) so
        # the OD is exactly OD(s) = s·od + (s²−s)·B; non-continuum gases reduce
        # to plain linear s·od.  See build_solar_reference.
        for gas, od_ref in self.ref.atm_gas_od.items():
            s  = p[gas]
            OD = OD + s * od_ref
            od_cont = self.ref.atm_gas_od_cont.get(gas)
            if od_cont is not None:
                OD = OD + (s * s - s) * od_cont

        # Optional plume gases (queried at the plume temperature)
        for species in self.window.plume_gases:
            OD = OD + gas_optical_depth(
                self.ref.gas_luts[species], p[species], self._plume_temp
            )

        # Optional plume aerosols (extinction)
        for species in self.window.plume_aero:
            lut     = self.ref.aero_luts[species]
            scaling = p[species]
            r0      = p[f"r0_{species}"]
            wt      = p.get(f"wt_{species}")
            OD = OD + scaling * lut.query(r0, wt)

        return OD

    # ------------------------------------------------------------------
    # High-resolution model
    # ------------------------------------------------------------------

    def model_highres(self, p_vec: np.ndarray) -> np.ndarray:
        """Transmittance × solar-continuum polynomial on the model grid (pre-ILS)."""
        p  = self.params.unpack(p_vec)
        wn = self.ref.wn

        poly_coeffs = [p[n] for n in self.params.names_by_kind(ParameterKind.POLYNOMIAL)]
        bg = build_background_poly(poly_coeffs, wn)

        OD = self._total_optical_depth(p)
        T  = np.exp(-self.airmass * OD)
        return T * bg

    def continuum(self, p_vec: np.ndarray) -> np.ndarray:
        """The solar-continuum polynomial (+ offset)."""
        return self._polynomial_continuum(p_vec)

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
