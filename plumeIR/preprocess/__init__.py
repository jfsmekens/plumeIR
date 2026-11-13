"""
plumeIR.preprocess — per-spectrum preparation between file I/O and fitting.

Preprocessing runs as a stage in the pipeline: a pure
``list[Spectrum] -> list[Spectrum]`` map applied once per run, before
``run_retrieval``.  It works on the full spectral axis; the per-window trim
happens later, inside the fit loop.

Layer preprocessing is self-emission subtraction; emission preprocessing is
two-point blackbody radiometric calibration.  Solar (division by a solar
reference) follows; until then those spectra pass through untouched.

The two geometries treat instrument radiance differently, and deliberately so:
layer subtracts a measured self background, while emission gets it for free as
the calibration's ``L_inst`` — the radiance at which the instrument reads zero
counts.  See :mod:`plumeIR.preprocess.emission`.
"""

from __future__ import annotations

import logging

from plumeIR.config.objects import EmissionGeometry, LayerGeometry
from plumeIR.preprocess.drift import DriftingSpectrum
from plumeIR.preprocess.emission import (
    BlackbodyCalibration, build_blackbody_calibration, calibrate_spectrum,
    pair_blackbodies, preprocess_emission,
)
from plumeIR.preprocess.layer import (
    SelfBackground, build_self_background, clip_negatives, find_self_spectra,
    preprocess_layer, subtract_self_emission,
)

logger = logging.getLogger(__name__)


def preprocess_spectra(spectra: list, geometry, data_config) -> list:
    """
    Apply the geometry-appropriate preprocessing to a run's spectra.

    Dispatches on the geometry type so the pipeline stays geometry-agnostic.

    Parameters
    ----------
    spectra : list of Spectrum
        Scene spectra, in acquisition order.
    geometry : LayerGeometry, EmissionGeometry or SolarGeometry
        Run geometry; supplies the preprocessing flags.
    data_config : DataConfig
        Data location and file-selection settings.

    Returns
    -------
    list[Spectrum]
        Preprocessed spectra, same length and order as the input.
    """
    if isinstance(geometry, LayerGeometry):
        return preprocess_layer(spectra, geometry, data_config)

    if isinstance(geometry, EmissionGeometry):
        return preprocess_emission(spectra, geometry, data_config)

    logger.debug(
        "No preprocessing implemented for %s; passing spectra through",
        type(geometry).__name__,
    )
    return spectra


__all__ = [
    "preprocess_spectra",
    # layer
    "preprocess_layer", "subtract_self_emission", "build_self_background",
    "find_self_spectra", "clip_negatives", "SelfBackground",
    # emission
    "preprocess_emission", "build_blackbody_calibration", "calibrate_spectrum",
    "pair_blackbodies", "BlackbodyCalibration",
    # shared
    "DriftingSpectrum",
]
