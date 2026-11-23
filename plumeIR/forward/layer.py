"""
Layer-geometry forward models for plumeIR v3.

Two variants, selected at build time:

    LayerForwardModel
        Full single-beam model for a plume in front of a bright IR source.
        F = E_source · res · T_plume · T_atm · poly  [+ emission term]

    TransmittanceForwardModel
        Used when a no-gas reference spectrum exists.  Preprocessing has
        already divided plume_sbm / no_gas_sbm, cancelling source,
        instrument response, and atmospheric background — what remains is
        plume transmittance only.
        F = T_plume · poly
        (Not exact for a plume hot enough to self-emit in the window.)

Both implement the :class:`~plumeIR.forward.base.ForwardModel` interface and
hold read-only references to a Reference, ParameterSet, geometry, and
Retrieval.  They never mutate their inputs.

Two plume temperatures (when ``model_plume_emission`` is on):
    plume_temp  — indexes the gas LUT for the *absorption* OD shape
    plumeE_temp — separate parameter for the *self-emission* level, entering
                both the emission OD and the Planck ratio
                B(plumeE_temp) / B(source_temp).
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
    rad,
)
from plumeIR.forward.ils import ILSKernel, broad_kernel_args, kernel_trim_fov, make_ils
from plumeIR.config.objects import FitWindowConfig, LayerGeometry, Retrieval
from plumeIR.parameters.parameter import ParameterKind, ParameterSet
from plumeIR.reference.reference import Reference

logger = logging.getLogger(__name__)


# =============================================================================
# Shared mixin: optical depth assembly
# =============================================================================

class _LayerODMixin:
    """
    Shared optical-depth assembly for layer-geometry forward models.

    Subclasses provide ``self.ref``, ``self.window``, ``self.geometry``,
    ``self.model_plume_emission`` and call :meth:`_optical_depths`.
    """

    ref:                Reference
    window:             FitWindowConfig
    geometry:           LayerGeometry
    model_plume_emission: bool

    def _gas_od(self, p: dict, name: str, temp: float) -> np.ndarray:
        """
        Optical depth of one gas parameter at temperature ``temp``.

        ``name`` is the parameter name, which is also the LUT key: the species
        for a single-role gas, ``{GAS}_atm`` for a dual gas's ambient part.
        """
        return gas_optical_depth(self.ref.gas_luts[name], p[name], temp)

    def _aero_od(self, p: dict, species: str) -> np.ndarray:
        """Optical depth for one aerosol species."""
        lut     = self.ref.aero_luts[species]
        scaling = p[species]
        r0      = p[f"r0_{species}"]
        if lut.is_2d:                       # H2SO4 (r0, weight_fraction)
            wt = p[f"wt_{species}"]
            return scaling * lut.query(r0, wt)
        return scaling * lut.query(r0)

    def _atm_optical_depth(self, p: dict) -> np.ndarray:
        """Total atmospheric-gas optical depth at ``atm_temp``."""
        OD_atm   = np.zeros(self.ref.wn.size)
        atm_temp = p.get("atm_temp", self.geometry.atm_temp)
        for species in self.window.atm_gases:
            OD_atm = OD_atm + self._gas_od(p, self.window.atm_name(species), atm_temp)
        return OD_atm

    def _plume_optical_depth(self, p: dict) -> tuple[np.ndarray, np.ndarray]:
        """
        Plume optical depth (gases + aerosols) and the plume self-emission OD.

        Returns
        -------
        (OD_plume, OD_plume_emission)
            ``OD_plume_emission`` is zero unless ``model_plume_emission`` is on.
        """
        n = self.ref.wn.size
        OD_plume  = np.zeros(n)
        OD_plumeE = np.zeros(n)

        plume_temp  = p.get("plume_temp", self.geometry.plume_temp)
        plumeE_temp = p.get("plumeE_temp", plume_temp)
        for species in self.window.plume_gases:
            OD_plume = OD_plume + self._gas_od(p, species, plume_temp)
            # Emission OD at plumeE_temp, through the same 1-D/2-D dispatch as the
            # absorption.  A continuum gas (2-D LUT, e.g. plume H2O) used to be
            # skipped here, silently dropping its self-emission.
            if self.model_plume_emission:
                OD_plumeE = OD_plumeE + self._gas_od(p, species, plumeE_temp)

        for species in self.window.plume_aero:
            OD_plume = OD_plume + self._aero_od(p, species)

        return OD_plume, OD_plumeE

    def _make_kernel(self, p: dict) -> ILSKernel:
        """Build the ILS kernel for the current spectral parameters."""
        wn_model = self.ref.wn
        nper_wn  = 1.0 / float(np.mean(np.diff(wn_model)))
        wn_center = 0.5 * (wn_model[0] + wn_model[-1])
        return make_ils(
            fov      = p["fov"],
            max_opd  = p.get("max_opd", self.retrieval.max_opd),
            nper_wn  = nper_wn,
            wn_center= wn_center,
            apod_type= self.retrieval.apod_type,
            trim_opd = self.retrieval.max_opd,  # fixed length while max_opd varies
            trim_fov = kernel_trim_fov(self.params["fov"]),  # fixed length while fov varies
            **broad_kernel_args(p, self.params, self.retrieval),
        )


# =============================================================================
# LayerForwardModel
# =============================================================================

class LayerForwardModel(_LayerODMixin, ForwardModel):
    """
    Full single-beam transmission forward model for the layer geometry.

    Parameters
    ----------
    reference : Reference
        Pre-computed gas and aerosol LUTs for this window.
    params : ParameterSet
        Immutable parameter set defining the varied/fixed parameters.
    geometry : LayerGeometry
        Layer geometry (T, P, pathlength, plume thickness, flags).
    retrieval : Retrieval
        Retrieval configuration (ILS params, apodization, …).
    window : FitWindowConfig
        Spectral window and species configuration.
    wn_meas : np.ndarray
        Measurement wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    e_model : np.ndarray or float
        Source emissivity on the model grid.  Default ``1.0`` (graybody —
        the source continuum shape is carried by the background polynomial).
    res_model : np.ndarray or float
        Instrument response residual on the model grid.  Default ``1.0``.
    """

    def __init__(
        self,
        reference: Reference,
        params:    ParameterSet,
        geometry:  LayerGeometry,
        retrieval: Retrieval,
        window:    FitWindowConfig,
        wn_meas:   np.ndarray,
        e_model:   np.ndarray | float = 1.0,
        res_model: np.ndarray | float = 1.0,
    ) -> None:
        self.ref       = reference
        self.params    = params
        self.geometry  = geometry
        self.retrieval = retrieval
        self.window    = window
        self.wn_meas   = np.asarray(wn_meas, dtype=float)
        self.e_model   = e_model
        self.res_model = res_model

        # Per-window override for model_plume_emission
        self.model_plume_emission = (
            window.model_plume_emission
            if window.model_plume_emission is not None
            else geometry.model_plume_emission
        )

    def model_highres(self, p_vec: np.ndarray) -> np.ndarray:
        """
        High-resolution model spectrum on the model grid (pre-ILS).

        Exposed for testing and diagnostics.

        Parameters
        ----------
        p_vec : np.ndarray
            Varied-parameter vector, shape ``(n_varied,)``.

        Returns
        -------
        np.ndarray
            Model radiance/transmittance on ``self.ref.wn``, shape ``(n_model,)``.
        """
        p        = self.params.unpack(p_vec)
        wn_model = self.ref.wn

        # Background polynomial (carries source × instrument continuum shape)
        poly_coeffs = [p[n] for n in self.params.names_by_kind(ParameterKind.POLYNOMIAL)]
        bg = build_background_poly(poly_coeffs, wn_model)

        OD_atm           = self._atm_optical_depth(p)
        OD_plume, OD_plumeE = self._plume_optical_depth(p)

        T_atm   = np.exp(-OD_atm)
        T_plume = np.exp(-OD_plume)

        F = self.e_model * self.res_model * T_plume * T_atm * bg

        if self.model_plume_emission:
            E_plume = 1.0 - np.exp(-OD_plumeE)
            source_temp = p.get("source_temp", self.geometry.source_temp)
            plumeE_temp   = p.get("plumeE_temp", p.get("plume_temp", self.geometry.plume_temp))
            Bg = rad(wn_model, plumeE_temp) / rad(wn_model, source_temp)
            F = F + Bg * E_plume * self.res_model * T_atm * bg

        return F

    def __call__(self, p_vec: np.ndarray) -> np.ndarray:
        p = self.params.unpack(p_vec)
        F = self.model_highres(p_vec)
        F = apply_ils(F, self._make_kernel(p))
        return apply_shift_offset(
            F, self.ref.wn, self.wn_meas,
            shift=p.get("shift", 0.0), offset=p.get("offset", 0.0),
        )

    def continuum(self, p_vec: np.ndarray) -> np.ndarray:
        """Polynomial × source emissivity × instrument residual (+ offset), as v2."""
        return self._polynomial_continuum(p_vec, self.e_model * self.res_model)


# =============================================================================
# TransmittanceForwardModel
# =============================================================================

class TransmittanceForwardModel(_LayerODMixin, ForwardModel):
    """
    Simplified forward model for plume transmittance.

    Selected when a no-gas reference spectrum exists: preprocessing has
    already isolated plume transmittance via ``plume_sbm / no_gas_sbm``, so
    source, instrument response, and atmospheric background are cancelled.

        F = T_plume · poly

    Only plume gases and aerosols contribute (no atmospheric layer, no
    source emissivity, no Planck terms).

    .. note::
       This assumes the plume does not self-emit significantly in the
       window.  For hot plumes a self-emission correction term is needed
       even here.  Not yet implemented.

    Parameters
    ----------
    reference, params, geometry, retrieval, window, wn_meas
        As for :class:`LayerForwardModel`.
    """

    def __init__(
        self,
        reference: Reference,
        params:    ParameterSet,
        geometry:  LayerGeometry,
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
        # Transmittance model never includes the emission term
        self.model_plume_emission = False

    def model_highres(self, p_vec: np.ndarray) -> np.ndarray:
        """High-resolution plume transmittance × polynomial (pre-ILS)."""
        p        = self.params.unpack(p_vec)
        wn_model = self.ref.wn

        poly_coeffs = [p[n] for n in self.params.names_by_kind(ParameterKind.POLYNOMIAL)]
        bg = build_background_poly(poly_coeffs, wn_model)

        OD_plume, _ = self._plume_optical_depth(p)
        T_plume = np.exp(-OD_plume)
        return T_plume * bg

    def continuum(self, p_vec: np.ndarray) -> np.ndarray:
        """The background polynomial (+ offset)."""
        return self._polynomial_continuum(p_vec)

    def __call__(self, p_vec: np.ndarray) -> np.ndarray:
        p = self.params.unpack(p_vec)
        F = self.model_highres(p_vec)
        F = apply_ils(F, self._make_kernel(p))
        return apply_shift_offset(
            F, self.ref.wn, self.wn_meas,
            shift=p.get("shift", 0.0), offset=p.get("offset", 0.0),
        )
