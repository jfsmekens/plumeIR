"""
Additive background of a layer single beam, measured from its opaque bands.

Where the ambient path is black, a single beam holds only what is added to the
source light: the instrument's own emission and a zero-level offset.  Today's
preprocessing subtracts a fixed self spectrum S_self at 100% and clips values
≤ 0.  Measured on the layer validation set (418
spectra, four opaque bands), per spectrum:

    model             opaque rms   held-out rms  [counts]
    c                 1852         2426
    a·S_self + c       324          524          a = 0.84 [0.73, 0.93]
    + b·continuum      313          852          overfits (b < 0)

so the self file has the right shape but is over-subtracted by ~16%, varying
between spectra.  This module measures, per spectrum,

    B(ν) = a·S_self(ν) + z(ν)

with ``a`` and a constant ``c`` fitted to the opaque points (each band weighted
equally), and ``z`` the zero level: the mean of ``y − a·S_self`` in each band,
linear between band centres and flat beyond the outer ones.  ``z`` is not
constrained between bands; the bands nearest the fit windows matter (the CO2
ν3 core at 2266-2367 cm⁻¹ sits ~0.5% of the continuum below ``a·S + c``, a
local level whose cause is open).

Opaque bands come from the run's template (:mod:`plumeIR.preprocess.template`),
never from fixed limits.  Without a self spectrum the model is ``z`` alone.

Report mode: the result is
measured and written; :func:`remove_background` is what an apply mode will
use (``y − B``, no clipping).
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field, replace

import numpy as np

from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Plausible range of the self-emission scale ``a``.  Outside it the self file
#: is likely stale or from another setup (measured 0.73-0.93, 5-95%).
SELF_SCALE_RANGE = (0.5, 1.2)

#: Held-out residual (rms over bands, each band predicted by a fit without
#: it) above which the background model is flagged, as a fraction of the
#: spectrum's continuum (its 95th percentile).  Measured: 524 counts against
#: a ~1.1e5 continuum, 0.5%.
HELDOUT_FLAG_FRACTION = 0.01

#: Fewest opaque bands for a measurement; with one, ``a`` and ``c`` cannot be
#: checked by leaving a band out.
MIN_BANDS = 2


@dataclass(frozen=True)
class BackgroundResult:
    """
    The additive background of one spectrum.

    Attributes
    ----------
    a : float
        Scale of the self spectrum (NaN without one).
    c : float
        Constant fitted with ``a`` [counts].
    bands : tuple of (float, float)
        Opaque bands used [cm⁻¹].
    centres : np.ndarray
        Band centres [cm⁻¹], shape ``(n_bands,)``.
    z : np.ndarray
        Zero level per band, mean of ``y − a·S`` [counts], shape ``(n_bands,)``.
    rms : np.ndarray
        Rms of ``y − a·S − z`` within each band [counts], shape ``(n_bands,)``.
    heldout : np.ndarray
        Mean residual of each band under an ``a·S + c`` fit to the other bands
        [counts], shape ``(n_bands,)``.
    continuum : float
        The spectrum's 95th percentile [counts], the scale of the flags.
    flags : tuple of str
        Problems found (empty when none).
    """
    a:         float
    c:         float
    bands:     tuple
    centres:   np.ndarray = field(repr=False)
    z:         np.ndarray = field(repr=False)
    rms:       np.ndarray = field(repr=False)
    heldout:   np.ndarray = field(repr=False)
    continuum: float = np.nan
    flags:     tuple = ()

    @property
    def heldout_rms(self) -> float:
        """Rms over bands of the held-out residuals [counts]."""
        return float(np.sqrt(np.mean(self.heldout ** 2))) if self.heldout.size else np.nan

    def zero_level(self, wn: np.ndarray) -> np.ndarray:
        """``z(ν)``: linear between band centres, flat beyond the outer ones."""
        return np.interp(wn, self.centres, self.z)

    def background(self, wn: np.ndarray, self_spectrum: np.ndarray | None) -> np.ndarray:
        """``B(ν) = a·S_self(ν) + z(ν)`` on ``wn``."""
        b = self.zero_level(wn)
        if self_spectrum is not None and np.isfinite(self.a):
            b = b + self.a * self_spectrum
        return b

    def row(self) -> dict:
        """Flat summary for a results table: one entry per band."""
        out = {"bkg_a": self.a, "bkg_c": self.c, "bkg_heldout_rms": self.heldout_rms,
               "bkg_continuum": self.continuum, "bkg_flags": ";".join(self.flags)}
        for (lo, hi), z, r, h in zip(self.bands, self.z, self.rms, self.heldout):
            tag = f"{0.5 * (lo + hi):.0f}"
            out[f"bkg_z_{tag}"] = z
            out[f"bkg_rms_{tag}"] = r
            out[f"bkg_heldout_{tag}"] = h
        return out


def _band_labels(wn: np.ndarray, bands) -> np.ndarray:
    """Band index per point (−1 outside every band)."""
    lab = np.full(wn.size, -1)
    for k, (lo, hi) in enumerate(bands):
        lab[(wn >= lo) & (wn <= hi)] = k
    return lab


def _weighted_lstsq(X: np.ndarray, y: np.ndarray, w: np.ndarray) -> np.ndarray:
    sw = np.sqrt(w)
    coef, *_ = np.linalg.lstsq(X * sw[:, None], y * sw, rcond=None)
    return coef


def measure_background(
    wn: np.ndarray,
    y: np.ndarray,
    bands,
    self_spectrum: np.ndarray | None = None,
    opaque: np.ndarray | None = None,
) -> BackgroundResult:
    """
    Measure ``a``, ``c`` and the per-band zero level of one single beam.

    Parameters
    ----------
    wn, y : np.ndarray
        The **raw** single beam (no self subtraction, no clipping), shape
        ``(n,)``.
    bands : sequence of (float, float)
        Opaque bands [cm⁻¹] (:meth:`Template.opaque_regions` on ``wn``).
    self_spectrum : np.ndarray or None
        The self background on ``wn`` at this spectrum's time, shape ``(n,)``;
        ``None`` measures the zero level alone.
    opaque : np.ndarray of bool or None
        Points to use inside the bands (e.g. template transmittance below the
        threshold); ``None`` uses every point in a band.

    Returns
    -------
    BackgroundResult

    Raises
    ------
    PlumeIRError
        With fewer than ``MIN_BANDS`` bands holding points, or arrays of
        different shapes.
    """
    wn, y = np.asarray(wn, dtype=float), np.asarray(y, dtype=float)
    if y.shape != wn.shape or (self_spectrum is not None
                               and np.shape(self_spectrum) != wn.shape):
        raise PlumeIRError("wn, y and the self spectrum must have the same shape")
    lab = _band_labels(wn, bands)
    if opaque is not None:
        lab = np.where(opaque, lab, -1)
    used = [k for k in range(len(bands)) if np.any(lab == k)]
    if len(used) < MIN_BANDS:
        raise PlumeIRError(
            f"{len(used)} opaque band(s) with points in this spectrum; the "
            f"background needs at least {MIN_BANDS}"
        )
    bands = tuple(bands[k] for k in used)
    lab = np.select([lab == k for k in used], list(range(len(used))), -1)
    sel = lab >= 0
    w = np.zeros(wn.size)
    for k in range(len(used)):
        w[lab == k] = 1.0 / np.sum(lab == k)

    has_self = self_spectrum is not None
    S = np.asarray(self_spectrum, dtype=float) if has_self else np.zeros_like(y)
    X = np.vstack([S, np.ones_like(y)]).T if has_self else np.ones((y.size, 1))
    coef = _weighted_lstsq(X[sel], y[sel], w[sel])
    a, c = (float(coef[0]), float(coef[1])) if has_self else (np.nan, float(coef[0]))

    nb = len(used)
    resid = y - (a * S if has_self else 0.0)
    z = np.array([resid[lab == k].mean() for k in range(nb)])
    rms = np.array([np.sqrt(np.mean((resid[lab == k] - z[k]) ** 2)) for k in range(nb)])
    heldout = np.empty(nb)
    for k in range(nb):
        tr = sel & (lab != k)
        ck = _weighted_lstsq(X[tr], y[tr], w[tr])
        heldout[k] = np.mean(y[lab == k] - X[lab == k] @ ck)
    centres = np.array([0.5 * (lo + hi) for lo, hi in bands])
    continuum = float(np.percentile(y, 95))

    flags = []
    if has_self and not SELF_SCALE_RANGE[0] <= a <= SELF_SCALE_RANGE[1]:
        flags.append(f"self scale {a:.3g} outside {SELF_SCALE_RANGE}")
    ho = float(np.sqrt(np.mean(heldout ** 2)))
    if continuum > 0 and ho > HELDOUT_FLAG_FRACTION * continuum:
        flags.append(f"held-out residual {ho:.3g} > {HELDOUT_FLAG_FRACTION:.0%} "
                     f"of the continuum")
    return BackgroundResult(a=a, c=c, bands=bands, centres=centres, z=z, rms=rms,
                            heldout=heldout, continuum=continuum, flags=tuple(flags))


def remove_background(spectrum, result: BackgroundResult,
                      self_spectrum: np.ndarray | None):
    """
    The spectrum with ``B`` subtracted, unclipped (apply mode).

    Parameters
    ----------
    spectrum : Spectrum
        The raw single beam the result was measured on.
    result : BackgroundResult
    self_spectrum : np.ndarray or None
        As passed to :func:`measure_background`.

    Returns
    -------
    Spectrum
        A copy with ``y − B``.  Values in the opaque bands scatter about zero
        and may be negative; that is information, not an artefact.
    """
    return replace(spectrum, y=spectrum.y - result.background(spectrum.wn, self_spectrum))
