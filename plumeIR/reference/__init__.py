"""plumeIR.reference — pre-computed spectral calibration objects."""

from plumeIR.reference.gas_lut import GasLUT, _ATM_TEMP_NODES, _PLUME_TEMP_NODES, _H2O_VMR_NODES
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.atm_lut import AtmLayerLUT, derive_t_eff
from plumeIR.reference.acidity import h2so4_acidity
from plumeIR.reference.reference import Reference, build_reference
from plumeIR.reference.emission_reference import build_emission_reference
from plumeIR.reference.solar_reference import build_solar_reference


def build_reference_for(geometry, window, retrieval, model, params, *,
                        cache_dir=None, recompute=False) -> Reference:
    """
    Build the Reference for a window using the geometry-appropriate builder.

    Dispatches on the geometry type so the pipeline stays geometry-agnostic:
    layer → build_reference, emission → build_emission_reference, solar →
    build_solar_reference.  All share the same signature.
    """
    from plumeIR.config.objects import (
        LayerGeometry, EmissionGeometry, SolarGeometry,
    )
    if isinstance(geometry, EmissionGeometry):
        builder = build_emission_reference
    elif isinstance(geometry, SolarGeometry):
        builder = build_solar_reference
    elif isinstance(geometry, LayerGeometry):
        builder = build_reference
    else:
        raise ValueError(
            f"No reference builder for geometry {type(geometry).__name__}"
        )
    return builder(window, geometry, retrieval, model, params,
                   cache_dir=cache_dir, recompute=recompute)


__all__ = [
    "GasLUT", "AerosolLUT", "AtmLayerLUT", "derive_t_eff",
    "Reference", "build_reference", "build_emission_reference",
    "build_solar_reference", "build_reference_for",
    "h2so4_acidity",
    "_ATM_TEMP_NODES", "_PLUME_TEMP_NODES", "_H2O_VMR_NODES",
]
