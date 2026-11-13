"""
build_forward_model — construct the right ForwardModel for a geometry.

The forward variant is chosen from the reference's geometry type, so the run
orchestrator stays geometry-agnostic: it builds a forward per (spectrum, window)
without knowing which subclass it is.

    LAYER     → LayerForwardModel
    EMISSION  → EmissionForwardModel
    SOLAR     → SolarForwardModel  (per-spectrum airmass)

Note: the ``TransmittanceForwardModel`` variant (layer geometry with a no-gas
reference) is a preprocessing-driven choice, not a geometry one — it will be
selected upstream when the divided spectrum is prepared; this factory builds the
full-single-beam ``LayerForwardModel`` for the plain layer case.
"""

from __future__ import annotations

import numpy as np

from plumeIR.forward.base import ForwardModel
from plumeIR.config.objects import GeometryType


def build_forward_model(
    reference,
    params,
    geometry,
    retrieval,
    window,
    wn_meas: np.ndarray,
    *,
    airmass: float = 1.0,
) -> ForwardModel:
    """
    Build the forward model matching ``reference.geometry_type``.

    Parameters
    ----------
    reference, params, geometry, retrieval, window, wn_meas
        The usual forward-model inputs.
    airmass : float
        Solar geometry only: the per-spectrum slant-path airmass.

    Returns
    -------
    ForwardModel
    """
    gtype = reference.geometry_type
    if gtype is GeometryType.LAYER:
        from plumeIR.forward.layer import LayerForwardModel
        return LayerForwardModel(reference, params, geometry, retrieval, window, wn_meas)
    if gtype is GeometryType.EMISSION:
        from plumeIR.forward.emission import EmissionForwardModel
        return EmissionForwardModel(reference, params, geometry, retrieval, window, wn_meas)
    if gtype is GeometryType.SOLAR:
        from plumeIR.forward.solar import SolarForwardModel
        return SolarForwardModel(reference, params, geometry, retrieval, window,
                                 wn_meas, airmass=airmass)
    raise ValueError(f"No forward model for geometry type {gtype!r}")
