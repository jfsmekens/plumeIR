"""
Preprocessing for the layer (transmission) geometry.

Turns a raw single beam into the spectrum the forward model expects.  The step
implemented here is **self-emission subtraction**: the instrument's own thermal
emission is an *additive* offset on every single beam, so it must be removed
before fitting.  Leaving it in biases retrieved columns — the forward model's
multiplicative background polynomial can absorb part of an additive term over a
narrow window, which flatters the residual while distorting the gas signal.

The background is measured directly (a blocked-beam / cold-view acquisition
saved alongside the data, by convention with ``self`` in the filename).  When a
dataset holds several, the background is fitted as a low-order polynomial in
time per wavenumber, so slow instrument drift over an acquisition is tracked.

The drift machinery itself is not specific to self-emission — the emission
geometry's blackbody and clear-sky references need exactly the same thing — so
it lives in :mod:`plumeIR.preprocess.drift`, along with the notes on where it
departs from v2.

Ref: plumeIR v2 ``analysis.py::makeSelfEmission`` (lines 1484-1528) and
``analysis.py::preProcess`` (lines 1659-1690).  One departure from v2 that is
specific to this module: the background is built on the full spectral axis, not
per fit window.  v2 rebuilt it inside each window's Analyser; subtraction is
pointwise, so the result within any window is the same.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import numpy as np

from plumeIR.exceptions import PlumeIRError
from plumeIR.spectra.discovery import discover_spectra
from plumeIR.spectra.spectrum import Spectrum, read_spectrum
from plumeIR.preprocess.drift import MAX_DRIFT_DEGREE, DriftingSpectrum

logger = logging.getLogger(__name__)


# =============================================================================
# Self-emission background
# =============================================================================

@dataclass(frozen=True)
class SelfBackground(DriftingSpectrum):
    """
    Instrument self-emission background, optionally time-resolved.

    A :class:`~plumeIR.preprocess.drift.DriftingSpectrum` with names that read
    naturally at the call site: ``spectra`` for the samples, ``files`` for
    their sources.  Built from one or more measured self spectra — static from
    a single spectrum, a per-wavenumber polynomial in time from several.
    """

    @property
    def spectra(self) -> np.ndarray:
        """The measured self spectra, shape ``(n_ref, n_wn)``."""
        return self.values

    @property
    def files(self) -> tuple[Path, ...]:
        """Source files, in time order."""
        return self.sources


def find_self_spectra(
    data_dir: str | Path,
    self_file: str | Path | None = None,
    filter_ext: str | None = None,
) -> list[Path]:
    """
    Locate the self-emission spectra for a dataset.

    Parameters
    ----------
    data_dir : str or Path
        Directory holding the spectral data.
    self_file : str, Path or None
        Explicit path, bypassing discovery.  ``None`` → search ``data_dir``
        for files whose stem contains ``'self'``.
    filter_ext : str or None
        Restrict discovery to this extension (the run's ``filter_ext``).

    Returns
    -------
    list[Path]
        Matching files.  Empty if none were found.

    Raises
    ------
    PlumeIRError
        If ``self_file`` is given but does not exist.
    """
    if self_file is not None:
        path = Path(self_file)
        if not path.is_file():
            raise PlumeIRError(f"self_file not found: {path}")
        return [path]

    # exclude_reference=False: these *are* the reference files we want.
    return discover_spectra(
        data_dir, filter_string="self", filter_ext=filter_ext,
        exclude_reference=False,
    )


def build_self_background(paths: list[Path]) -> SelfBackground:
    """
    Read the self spectra and build the background model.

    A single spectrum gives a static background.  Several give a drift fit of
    degree ``min(n - 1, MAX_DRIFT_DEGREE)`` in time, per wavenumber — unless
    timestamps are unavailable, in which case the spectra are averaged.

    Parameters
    ----------
    paths : list of Path
        Self-emission spectrum files.

    Returns
    -------
    SelfBackground

    Raises
    ------
    PlumeIRError
        If ``paths`` is empty, or the spectra do not share a spectral axis.
    """
    if not paths:
        raise PlumeIRError("No self-emission spectra to build a background from")

    return SelfBackground.from_spectra(
        [read_spectrum(p) for p in paths],
        label="self-emission background",
    )


# =============================================================================
# Per-spectrum preprocessing
# =============================================================================

def clip_negatives(y: np.ndarray, mode: str = "cutoff") -> np.ndarray:
    """
    Remove non-positive values left by background subtraction.

    Ref: v2 ``analysis.py::preProcess`` lines 1685-1694.

    Parameters
    ----------
    y : np.ndarray
        Spectrum values, shape ``(n_wn,)``.
    mode : {'cutoff', 'offset', 'none'}
        ``'cutoff'`` replaces every value ``<= 0`` with the smallest strictly
        positive value (v2's default).  ``'offset'`` shifts the whole spectrum
        up by ``1.01 * |min|``.  ``'none'`` leaves it untouched.

    Returns
    -------
    np.ndarray
        New array, shape ``(n_wn,)``.

    Raises
    ------
    PlumeIRError
        For an unknown mode, or if ``'cutoff'`` finds no positive value.
    """
    if mode == "none":
        return y.copy()

    if mode == "cutoff":
        positive = y > 0
        if not positive.any():
            raise PlumeIRError(
                "Spectrum has no positive values after background subtraction "
                "— the self-emission background is probably not from this "
                "instrument or acquisition"
            )
        return np.where(positive, y, y[positive].min())

    if mode == "offset":
        low = y.min()
        return y + abs(low * 1.01) if low < 0 else y.copy()

    raise PlumeIRError(
        f"Unknown negative-value mode {mode!r} (expected 'cutoff', 'offset' "
        f"or 'none')"
    )


def subtract_self_emission(
    spectrum: Spectrum,
    background: SelfBackground,
    clip_mode: str = "cutoff",
) -> Spectrum:
    """
    Subtract the self-emission background from one spectrum.

    Pure: returns a new :class:`Spectrum`, leaving the input untouched.  The
    spectral type is unchanged — the result is still a single beam, just
    without the instrument's additive offset.

    Parameters
    ----------
    spectrum : Spectrum
        Raw single-beam spectrum.
    background : SelfBackground
        Background model, evaluated at this spectrum's timestamp.
    clip_mode : {'cutoff', 'offset', 'none'}
        Non-positive value handling, see :func:`clip_negatives`.

    Returns
    -------
    Spectrum
        New spectrum with the background removed.
    """
    bkg = background.at(spectrum.timestamp, x=spectrum.x)
    y = clip_negatives(spectrum.y - bkg, mode=clip_mode)

    return Spectrum(
        x=spectrum.x.copy(),
        y=y,
        x_unit=spectrum.x_unit,
        timestamp=spectrum.timestamp,
        filename=spectrum.filename,
        spec_type=spectrum.spec_type,
        metadata=spectrum.metadata,
        is_blackbody=spectrum.is_blackbody,
        bb_temp=spectrum.bb_temp,
        ils=spectrum.ils,          # instrument metadata survives preprocessing
    )


def preprocess_layer(
    spectra: list[Spectrum],
    preprocess,
    data_config,
    clip_mode: str = "cutoff",
) -> list[Spectrum]:
    """
    Apply layer-geometry preprocessing to a run's spectra.

    Currently self-emission subtraction, controlled by
    ``PreprocessConfig.subtract_self`` (``*PREPROCESS``; in ``*GEOMETRY``
    until 2026-11-15).  A no-op when the flag is off, so the stage is always
    safe to call.

    Parameters
    ----------
    spectra : list of Spectrum
        Scene spectra, in acquisition order.
    preprocess : PreprocessConfig
        Supplies ``subtract_self`` and the optional ``self_file`` override.
    data_config : DataConfig
        Supplies ``data_dir`` and ``filter_ext`` for discovery.
    clip_mode : {'cutoff', 'offset', 'none'}
        Non-positive value handling, see :func:`clip_negatives`.

    Returns
    -------
    list[Spectrum]
        Preprocessed spectra, same length and order as the input.

    Raises
    ------
    PlumeIRError
        If ``subtract_self`` is enabled but no self-emission spectrum exists.
        This is a hard error rather than a warning: silently fitting raw single
        beams is exactly the bias the flag exists to remove.
    """
    if not preprocess.subtract_self:
        return spectra

    paths = find_self_spectra(
        data_config.data_dir,
        self_file=preprocess.self_file,
        filter_ext=data_config.filter_ext,
    )
    if not paths:
        raise PlumeIRError(
            f"subtract_self is enabled but no self-emission spectrum was found "
            f"in '{data_config.data_dir}'. Expected a file with 'self' in its "
            f"name, or set PREPROCESS.self_file explicitly (or subtract_self: "
            f"False to fit raw single beams)."
        )

    background = build_self_background(paths)
    out = [subtract_self_emission(s, background, clip_mode=clip_mode)
           for s in spectra]
    logger.info(
        "Subtracted self-emission background from %d spectra", len(out),
    )
    return out
