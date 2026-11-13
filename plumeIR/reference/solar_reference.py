"""
build_solar_reference — reference pre-computation for the solar occultation
geometry (Phase 4c).

Ground-based sun/moon tracking.  Pure transmittance: the solar beam is
attenuated by the slant atmospheric column (and an optional plume).

Builds, for one fit window:
  - Vertical optical depth per atmospheric gas (``atm_gas_od``) at the a
    priori column, via zenith RFM through the full profile.  The forward
    applies ``OD = scaling · atm_gas_od[gas] · airmass``.  Atmospheric gases
    are the retrieval *targets* (total-column GHG).
  - For continuum gases (H2O), the self-continuum component (``atm_gas_od_cont``)
    via a second zenith RFM run at scaling=2; the self-continuum scales as
    column² so the forward uses ``OD(s) = s·od + (s²−s)·B`` instead of a plain
    linear ``s·od`` (which is wrong by tens of % away from the a priori; measured
    against direct RFM runs).
  - Vertical total column per atmospheric gas (``atm_gas_refcol``) for the
    Xgas conversion (column / dry-air column, dry-air from surface pressure).
  - Optional plume gas / aerosol LUTs (volcanic occultation) — reuses the
    layer-geometry plume machinery.

Airmass is a fixed per-spectrum input applied in the forward, NOT here — the
LUTs are elevation-independent (vertical).

NOTE: RFM-calling paths are not unit-tested in CI (need the binary).
"""

from __future__ import annotations

import logging
import tempfile
from pathlib import Path

import numpy as np

from plumeIR.config.objects import (
    FitWindowConfig, GeometryType, ModelConfig, Retrieval, SolarGeometry,
)
from plumeIR.parameters.parameter import ParameterSet
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference, _resolve_cache_dir
from plumeIR.exceptions import ReferenceComputationError
from plumeIR.standards import AERO_DEFAULTS, GAS_DEFAULTS, AERO_R0_NODES, plume_temp_nodes_for
from plumeIR.units import vmr_to_scd

logger = logging.getLogger(__name__)


# =============================================================================
# RFM helper — vertical optical depth of one gas through the full profile
# =============================================================================

def _vertical_gas_od(
    atm:      dict,
    gas:      str,
    h_obs:    float,
    wn_start: float,
    wn_stop:  float,
    nperwn:   float,
    scaling:  float = 1.0,
) -> tuple[np.ndarray, np.ndarray]:
    """
    Vertical (zenith, elev=90°) optical depth of a single gas through the
    atmosphere above ``h_obs``, with the gas profile column-scaled by
    ``scaling`` (1.0 = a priori).

    Returns ``(wn, OD)``.
    """
    from rfmwrapper import RFM, adjustAtm, writeAtm

    if scaling != 1.0:
        # adjustAtm scales the whole profile proportionally to match the
        # surface VMR → a true column scaling.
        atm = adjustAtm(atm, gas, float(atm[gas][0]) * scaling)

    with tempfile.NamedTemporaryFile(suffix=".atm", delete=False) as tf:
        atm_path = Path(tf.name)
    try:
        writeAtm(atm, str(atm_path))
        model = RFM(
            type="zenith",
            wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
            gas=gas, elev=90.0, h_obs=h_obs,
            atm_path=str(atm_path),
            out="OPT", silent=True,
        )
        model.run()
        opt = model.getSpectra("opt")
        return opt.wn, opt.data[0]
    finally:
        atm_path.unlink(missing_ok=True)


def _vertical_total_column(atm: dict, gas: str, h_obs: float) -> float:
    """
    Vertical total column [molec/cm²] of a gas above the observer, integrated
    over the profile from ``h_obs`` to the top of atmosphere.
    """
    hgt = np.asarray(atm["HGT"], dtype=float)
    mask = hgt >= h_obs
    if mask.sum() < 2:
        return 0.0
    return float(vmr_to_scd(
        np.asarray(atm[gas])[mask], hgt[mask],
        temp=np.asarray(atm["TEM"])[mask], pres=np.asarray(atm["PRE"])[mask],
        elev=90.0,
    ))


# =============================================================================
# Top-level builder
# =============================================================================

def build_solar_reference(
    window:    FitWindowConfig,
    geometry:  SolarGeometry,
    retrieval: Retrieval,
    model:     ModelConfig,
    params:    ParameterSet,
    cache_dir: Path | None = None,
    recompute: bool = False,
) -> Reference:
    """
    Build the solar-geometry :class:`Reference` for a fit window.

    Parameters
    ----------
    window : FitWindowConfig
        Spectral window and species configuration.  ``atm_gases`` are the
        retrieval targets (total-column).
    geometry : SolarGeometry
        Solar geometry (observer height, atmosphere path, optional plume).
    retrieval : Retrieval
        Retrieval flags.
    model : ModelConfig
        Model resolution.
    params : ParameterSet
        Parameter set for this window.
    cache_dir : Path or None
        Cache directory.
    recompute : bool
        Ignore cache and recompute.

    Returns
    -------
    Reference
        With ``atm_gas_od`` (vertical OD per atmospheric gas),
        ``atm_gas_refcol`` (vertical total columns), optional plume gas /
        aerosol LUTs, and ``geometry_type == SOLAR``.
    """
    from rfmwrapper import readAtm

    cache_dir = _resolve_cache_dir(cache_dir, model)

    wn_start = window.wn_start - model.model_padding
    wn_stop  = window.wn_stop  + model.model_padding
    nperwn   = model.nperwn

    atm = readAtm(str(geometry.atm_path))
    logger.info(
        "Building solar Reference for '%s' [%.0f–%.0f cm⁻¹], obs=%.2f km",
        window.label, window.wn_start, window.wn_stop, geometry.obs_height,
    )

    # ------------------------------------------------------------------
    # Atmospheric gases: vertical OD + total column (targets)
    # ------------------------------------------------------------------
    atm_gas_od:      dict[str, np.ndarray] = {}
    atm_gas_od_cont: dict[str, np.ndarray] = {}
    atm_gas_refcol:  dict[str, float]      = {}
    wn_out = None
    for gas in window.atm_gases:
        wn_out, od = _vertical_gas_od(
            atm, gas, geometry.obs_height, wn_start, wn_stop, nperwn
        )
        atm_gas_od[gas]     = od
        atm_gas_refcol[gas] = _vertical_total_column(atm, gas, geometry.obs_height)

        # Continuum gases (H2O): the self-continuum scales as column², so a
        # single linear OD is wrong away from the a priori.  A second RFM run
        # at scaling=2 lets us split the OD exactly into a linear part A
        # (lines + foreign continuum, ∝ column) and a quadratic self-continuum
        # part B (∝ column²):
        #     OD(1) = A + B ,  OD(2) = 2A + 4B  ⇒  B = (OD(2) − 2·OD(1)) / 2 .
        # The forward then applies OD(s) = s·od + (s²−s)·B (exact).  Validated:
        # against direct RFM, linear-scaling continuum error reached tens of %
        # at s=0.5/2.0; this removes it.
        if gas in window.continuum_gases:
            _, od2 = _vertical_gas_od(
                atm, gas, geometry.obs_height, wn_start, wn_stop, nperwn,
                scaling=2.0,
            )
            atm_gas_od_cont[gas] = 0.5 * (od2 - 2.0 * od)

    if wn_out is None:
        # No atmospheric gases — synthesise the model grid
        wn_out = np.arange(wn_start, wn_stop, 1.0 / nperwn)

    # ------------------------------------------------------------------
    # Optional plume layer (volcanic occultation)
    # ------------------------------------------------------------------
    gas_luts:  dict[str, GasLUT]     = {}
    aero_luts: dict[str, AerosolLUT] = {}

    if window.plume_gases or window.plume_aero:
        plume_thickness = geometry.plume_thickness or 0.5
        # Plume temperature: explicit, else from the profile at plume altitude
        if geometry.plume_temp is not None:
            plume_temp = geometry.plume_temp
        elif geometry.plume_height is not None:
            plume_temp = float(np.interp(
                geometry.plume_height, atm["HGT"], atm["TEM"]
            ))
        else:
            raise ReferenceComputationError(
                f"Solar window '{window.label}' models a plume "
                f"({window.plume_gases + window.plume_aero}) but the geometry "
                f"sets neither plume_temp nor plume_height, so its temperature "
                f"is unknown. A plume near equilibrium takes the profile "
                f"temperature at its altitude: set plume_height (or plume_temp)."
            )
        plume_pres = 1013.0

        for species in window.plume_gases:
            if species in params:
                ref_scd = params[species].ref_column
            else:
                ref_conc = GAS_DEFAULTS.get(species, {}).get("conc", 1.0)
                ref_scd  = vmr_to_scd(ref_conc, plume_thickness,
                                      temp=plume_temp, pres=plume_pres)
            gas_luts[species] = GasLUT.load_or_compute(
                species=species, wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
                temp_nodes=plume_temp_nodes_for(geometry, plume_temp), pathlength=plume_thickness,
                pres=plume_pres, ref_scd=float(ref_scd),
                continuum=species in window.continuum_gases,
                cache_dir=cache_dir, recompute=recompute,
            )

        for species in window.plume_aero:
            if species in params:
                ref_mass = params[species].ref_column
            else:
                ref_conc = AERO_DEFAULTS.get(species, {}).get("conc", 1e-3)
                ref_mass = ref_conc * plume_thickness * 1e3
            if species == "H2SO4":
                aero_luts[species] = AerosolLUT.build_h2so4_2d(
                    wn=wn_out, ref_mass_g_m2=float(ref_mass), with_absorption=False,
                )
            else:
                aero_luts[species] = AerosolLUT.build_1d(
                    species=species, wn=wn_out, r0_nodes_um=AERO_R0_NODES,
                    ref_mass_g_m2=float(ref_mass), with_absorption=False,
                    cache_dir=cache_dir, recompute=recompute,
                )

    ref = Reference(
        wn=wn_out,
        gas_luts=gas_luts,
        aero_luts=aero_luts,
        geometry_type=GeometryType.SOLAR,
        atm_gas_od=atm_gas_od,
        atm_gas_od_cont=atm_gas_od_cont,
        atm_gas_refcol=atm_gas_refcol,
    )
    logger.info("✓ Solar Reference for '%s' built", window.label)
    return ref
