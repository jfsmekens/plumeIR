"""
Envelope metrics of a whole spectrum: hulls, peak, and relative changes.

Model-free measurements made on every spectrum before the fits:

- **top and bottom hulls**: a running maximum and a running minimum over
  ``HULL_WIDTH`` cm⁻¹, each smoothed over the same width.  The top hull sits
  on the line-free maxima of each 25 cm⁻¹ (the transmitted continuum, with
  what the atmosphere removes from it); the bottom hull runs through the
  line cores and, in a layer single beam, through the black bands (a
  model-free view of the additive background).  A global hull was tried
  first and rejected (2026-11-16): the upper convex hull of ``(ν, ln y)``
  bridges the wide black bands (1300-1900, 2250-2400 cm⁻¹ in a layer single
  beam) with chords far above the data, and in emission rides noise spikes.
- **peak**: the top hull's maximum and its wavenumber (refined by a parabola
  through the peak sample), and the integrated top hull.  In a single beam
  the peak position is set mostly by the instrument response: its changes
  over a series are informative, its value is not a colour temperature.
- **relative scale and temperature** (:func:`relative_planck`): each top hull
  against a reference hull (the median of the series' first spectra),
  ``ln(H/H_ref) = ln f + ln[B(ν,T)/B(ν,T_ref)]``.  The instrument response
  and the source emissivity cancel in the ratio; ``f`` is a broadband scale
  (fill fraction × emissivity, and any broadband attenuation such as
  aerosol), ``ΔT = T − T_ref`` a relative source temperature for an assumed
  ``T_ref``.  ΔT is noisy per spectrum (an earlier test from clear
  micro-windows: ~56 K) and is for plots only.
- **opaque brightness temperature** (emission): in the opaque bands a
  calibrated sky radiance is the emission of the air next to the
  instrument.  The 90th percentile of the brightness temperature of the
  spectrum (running median over ~2.5 cm⁻¹) lands on that plateau: measured
  on the 2019-09-11 set, the CO2 ν2 band (660-690 cm⁻¹) reads 301-303 K and
  the H2O ν2 band (1450-1600) 300-301 K in the same spectra.  The hull's
  maximum is not used: it rides noise (313 K at 1505 cm⁻¹).
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field

import numpy as np
from scipy.ndimage import (maximum_filter1d, median_filter, minimum_filter1d,
                           uniform_filter1d)
from scipy.optimize import minimize_scalar

from plumeIR.exceptions import PlumeIRError
from plumeIR.physics import bbt, rad

logger = logging.getLogger(__name__)

#: Width [cm⁻¹] of the running maximum / minimum and of their smoothing.
HULL_WIDTH = 25.0
#: Running-median width [cm⁻¹] of the spectrum whose brightness temperature
#: gives the opaque-band temperature, and the percentile taken.
BT_SMOOTH, BT_PERCENTILE = 2.5, 90.0
#: Points of the hulls used in the relative fit: both above this fraction of
#: their maximum, so the decaying tails (where the ratio is noise) are out.
RELATIVE_FLOOR = 0.05
#: Search range of ΔT around T_ref [K].
DT_RANGE = (-0.6, 1.0)          # × T_ref


@dataclass(frozen=True)
class Envelope:
    """
    Hulls and peak of one spectrum.

    Attributes
    ----------
    wn : np.ndarray
        Wavenumber [cm⁻¹], shape ``(n,)``.
    top, bottom : np.ndarray
        Top and bottom hulls on ``wn``, shape ``(n,)``.
    peak, peak_wn : float
        The top hull's maximum and its wavenumber [cm⁻¹].
    integral : float
        The top hull integrated over ``wn`` (spectrum units × cm⁻¹).
    """
    wn:       np.ndarray = field(repr=False)
    top:      np.ndarray = field(repr=False)
    bottom:   np.ndarray = field(repr=False)
    peak:     float = np.nan
    peak_wn:  float = np.nan
    integral: float = np.nan

    def row(self) -> dict:
        """Flat summary for a results table."""
        return {"env_peak": self.peak, "env_peak_wn": self.peak_wn,
                "env_integral": self.integral}


def _points(width: float, wn: np.ndarray) -> int:
    return max(3, int(round(width / float(np.mean(np.diff(wn))))) | 1)


def _hull(wn: np.ndarray, y: np.ndarray, which: str) -> np.ndarray:
    n = _points(HULL_WIDTH, wn)
    finite = np.isfinite(y)
    if not finite.any():
        raise PlumeIRError("no finite values to take a hull of")
    fill = np.nanmin(y) if which == "top" else np.nanmax(y)
    filled = np.where(finite, y, fill)
    run = (maximum_filter1d if which == "top" else minimum_filter1d)(filled, n, mode="nearest")
    return uniform_filter1d(run, n, mode="nearest")


def top_hull(wn: np.ndarray, y: np.ndarray) -> np.ndarray:
    """Running maximum over ``HULL_WIDTH`` cm⁻¹, smoothed over the same width."""
    return _hull(wn, y, "top")


def bottom_hull(wn: np.ndarray, y: np.ndarray) -> np.ndarray:
    """Running minimum over ``HULL_WIDTH`` cm⁻¹, smoothed over the same width."""
    return _hull(wn, y, "bottom")


def envelope(wn: np.ndarray, y: np.ndarray) -> Envelope:
    """
    Hulls, peak and integral of one spectrum.

    Parameters
    ----------
    wn, y : np.ndarray
        Ascending wavenumber [cm⁻¹] and the spectrum, shape ``(n,)``.

    Returns
    -------
    Envelope
    """
    wn, y = np.asarray(wn, dtype=float), np.asarray(y, dtype=float)
    top = top_hull(wn, y)
    k = int(np.argmax(top))
    peak_wn, peak = float(wn[k]), float(top[k])
    if 0 < k < wn.size - 1:
        a, b, c = top[k - 1], top[k], top[k + 1]
        den = a - 2 * b + c
        if den < 0:
            frac = 0.5 * (a - c) / den
            peak_wn = float(wn[k] + frac * (wn[k + 1] - wn[k]))
            peak = float(b - 0.25 * (a - c) * frac)
    integral = float(np.trapezoid(top, wn) if hasattr(np, "trapezoid") else np.trapz(top, wn))
    return Envelope(wn=wn, top=top, bottom=bottom_hull(wn, y), peak=peak,
                    peak_wn=peak_wn, integral=integral)


def reference_hull(envelopes, n: int) -> np.ndarray:
    """
    The median top hull of the first ``n`` envelopes (on the first one's grid).

    Raises
    ------
    PlumeIRError
        If there are no envelopes or ``n < 1``.
    """
    if n < 1 or not envelopes:
        raise PlumeIRError(f"a reference needs at least one spectrum (n={n})")
    grid = envelopes[0].wn
    tops = [np.interp(grid, e.wn, e.top) for e in envelopes[:n]]
    return np.median(tops, axis=0)


@dataclass(frozen=True)
class RelativePlanck:
    """Relative broadband scale ``f`` and temperature change ``dT`` [K]."""
    f:      float
    dT:     float
    rms:    float
    n:      int


def relative_planck(wn: np.ndarray, top: np.ndarray, ref: np.ndarray,
                    t_ref: float, mask: np.ndarray | None = None) -> RelativePlanck:
    """
    Fit ``ln(H/H_ref) = ln f + ln[B(ν,T)/B(ν,T_ref)]`` for ``f`` and ``T``.

    Parameters
    ----------
    wn, top, ref : np.ndarray
        Grid, the spectrum's top hull and the reference hull, shape ``(n,)``.
    t_ref : float
        Assumed temperature of the reference [K]; ``dT`` depends on it.
    mask : np.ndarray of bool or None
        Points allowed in the fit (``True``); use it to leave out the plume's
        absorption bands, which otherwise drive both ``f`` and ``dT``.

    Returns
    -------
    RelativePlanck
        ``f`` and ``dT = T − t_ref``; NaN with ``n = 0`` when too few points
        are usable.
    """
    use = ((top > RELATIVE_FLOOR * np.max(top)) & (ref > RELATIVE_FLOOR * np.max(ref))
           & np.isfinite(top) & np.isfinite(ref))
    if mask is not None:
        use &= mask
    if use.sum() < 10:
        return RelativePlanck(np.nan, np.nan, np.nan, 0)
    nu, r = wn[use], np.log(top[use] / ref[use])
    b_ref = np.log(rad(nu, t_ref))

    def cost(T: float) -> float:
        g = np.log(rad(nu, T)) - b_ref
        return float(np.sum((r - g - np.mean(r - g)) ** 2))

    res = minimize_scalar(cost, bounds=(t_ref * (1 + DT_RANGE[0]), t_ref * (1 + DT_RANGE[1])),
                          method="bounded", options={"xatol": 1e-3})
    g = np.log(rad(nu, res.x)) - b_ref
    lnf = float(np.mean(r - g))
    return RelativePlanck(f=float(np.exp(lnf)), dT=float(res.x - t_ref),
                          rms=float(np.sqrt(np.mean((r - g - lnf) ** 2))), n=int(use.sum()))


def opaque_brightness_temperature(wn: np.ndarray, y: np.ndarray) -> float:
    """
    The air temperature next to the instrument, from a calibrated radiance [K].

    The ``BT_PERCENTILE`` percentile of the brightness temperature of ``y``
    after a running median over ``BT_SMOOTH`` cm⁻¹ (emission only: ``y`` in
    mW/(m² sr cm⁻¹)).  NaN when no point is positive.
    """
    wn, y = np.asarray(wn, dtype=float), np.asarray(y, dtype=float)
    finite = np.isfinite(y)
    if not finite.any():
        return np.nan
    ys = median_filter(np.where(finite, y, np.nanmedian(y)), size=_points(BT_SMOOTH, wn))
    ok = finite & (ys > 0)
    if not ok.any():
        return np.nan
    return float(np.percentile(bbt(wn[ok], ys[ok]), BT_PERCENTILE))
