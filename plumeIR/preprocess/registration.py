"""
Wavenumber registration: the shift and scale of a spectrum's axis, measured
before any fit by cross-correlation with a synthetic spectrum.

A measured spectrum is the template displaced by ``lag(ν) = s0 + ε·ν``:
``y(ν) ≈ T(ν − lag(ν))``.  ε is a scale error of the wavenumber axis (laser
wavelength, off-axis rays), s0 a constant shift.  Measured:

- layer (MIDAC): a pure scale, ε = −1.534e-4 over 1900-3300 cm⁻¹, intercept
  +0.002 cm⁻¹, r = 0.98 per spectrum with ε from seven fitted windows
  (−1.503e-4);
- emission (Bruker): shift **plus** scale, slope −2.2e-4 with a non-zero
  constant, so one ``shift`` over 750-1250 cm⁻¹ leaves ~±0.055 cm⁻¹ at the
  window edges.

Method, per spectrum:

1. Sub-bands across the range.  Layer: the single beam divided by its
   continuum envelope (through the points the template calls clear), 40 cm⁻¹
   sub-bands with under 20% opaque points and line structure in the
   template; only non-opaque points used.  Emission: the radiance itself,
   25 cm⁻¹ sub-bands, those centred in plume bands left out (the clear-sky
   template has no plume).
2. In each, the lag maximising the normalised correlation of the measurement
   with the template at ν − lag, both high-passed (layer: a 2 cm⁻¹ running
   mean removed, so lines and not a band envelope set the lag; emission: a
   straight line removed), on a 0.005 cm⁻¹ grid within ±0.8 cm⁻¹, refined by
   a parabola through the peak.
3. ``lag = s0 + ε·ν`` by least squares over sub-bands with a correlation peak
   above a minimum and a lag off the grid's edge, rejecting those more than
   0.1 cm⁻¹ from the line (three passes); a fit through the origin
   alongside.

Each spectrum is registered on its own, with the free fit (s0 + ε·ν), and
nothing is decided or smoothed across the series (changed 2026-11-16): measured with the registration
applied, per spectrum against a 15-spectrum running median changes no column
(×1.0000, 5-95% within ±0.1% layer, ±0.3% emission SO2) and leaves less to
``shift`` (its scatter 1.4-3.3× smaller), and a constant s0 is what
``shift`` fits anyway, so the run-level choice between a pure scale and
shift + scale changed nothing either.  So the registration needs no later
spectra, as real-time processing requires.  The fit through the origin is
still reported, as a diagnostic of the instrument.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field

import numpy as np
from scipy.interpolate import CubicSpline
from scipy.ndimage import median_filter, uniform_filter1d

from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Lag search range and step [cm⁻¹].  The measured lags are ≤ 0.6 cm⁻¹ at
#: 4000 cm⁻¹ (ε 1.5e-4); a lag at the range's edge is rejected.
LAG_MAX, LAG_STEP = 0.8, 0.005
#: Sub-band widths [cm⁻¹] (prototype values).
LAYER_SUBBAND, EMISSION_SUBBAND = 40.0, 25.0
#: High-pass width [cm⁻¹] (0: linear detrend only).  Measured 2026-11-16.
#: Layer: a band envelope inside a sub-band pulls the lag (SO2 at 2530 cm⁻¹
#: +0.20 cm⁻¹ off the line, the H2O wing above 3300 −0.10 to −0.14); with a
#: 2 cm⁻¹ running mean removed they drop to −0.005 and −0.01 to −0.06, and ε
#: over the whole MIDAC range (530-4470) is −1.506e-4 against −1.503e-4 from
#: the fitted windows (−1.636e-4 without).  Emission: radiance structure is
#: broad, and the same high-pass biases ε to −1.0e-4 to −1.3e-4 against a
#: fitted stretch of −2.27e-4 (−2.19e-4 without it).
LAYER_HIGHPASS, EMISSION_HIGHPASS = 2.0, 0.0
#: Minimum correlation peak for a sub-band to be used.  Layer sub-bands are
#: line forests on a normalised continuum (median peak ~0.9); emission
#: radiance sub-bands correlate at > 0.97 where the model is right.
LAYER_MIN_PEAK, EMISSION_MIN_PEAK = 0.5, 0.9
#: Layer sub-band selection: template transmittance below which a point is
#: opaque, above which clear, the most opaque points a sub-band may hold, and
#: the least template variation (line structure) it must have.
OPAQUE_T, CLEAR_T, MAX_OPAQUE, MIN_STRUCTURE = 1e-3, 0.95, 0.2, 0.01
#: Distance from the fitted line beyond which a sub-band is rejected [cm⁻¹].
REJECT = 0.1
#: Fewest sub-bands for a measurement.
MIN_SUBBANDS = 4


@dataclass(frozen=True)
class SubbandLags:
    """Per-sub-band lags of one spectrum: centres [cm⁻¹], lags [cm⁻¹], peaks."""
    centres: np.ndarray
    lags:    np.ndarray
    peaks:   np.ndarray


@dataclass(frozen=True)
class RegistrationResult:
    """
    The wavenumber registration of one spectrum.

    Attributes
    ----------
    eps, eps_err : float
        Scale ε and its 1σ error, free-intercept fit.
    s0, s0_err : float
        Intercept [cm⁻¹] and its 1σ error, free-intercept fit.
    eps_origin, eps_origin_err : float
        Scale through the origin (s0 = 0) and its error.
    rms : float
        Rms of the used lags about the free fit [cm⁻¹].
    nu_mid, lag_mid, lag_mid_err : float
        The mean wavenumber of the used sub-bands [cm⁻¹] and the fitted lag
        there, with its error [cm⁻¹].  Unlike ``s0`` (the lag at ν = 0, far
        outside the data) it is uncorrelated with ``eps``, so it is the
        shift diagnostic to read: on the emission set ``s0`` swings by up to
        0.25 cm⁻¹ at the aerosol events while the lag at mid-range moves by
        0.004 (2026-11-16).
    n_used, n_bands : int
        Sub-bands used / measured.
    subbands : SubbandLags
        The per-sub-band measurements.
    used : np.ndarray of bool
        Which sub-bands entered the fit.
    """
    eps:            float
    eps_err:        float
    s0:             float
    s0_err:         float
    eps_origin:     float
    eps_origin_err: float
    rms:            float
    nu_mid:         float
    lag_mid:        float
    lag_mid_err:    float
    n_used:         int
    n_bands:        int
    subbands:       SubbandLags = field(repr=False)
    used:           np.ndarray = field(repr=False)

    def lag(self, wn: np.ndarray) -> np.ndarray:
        """The displacement ``s0 + ε·ν`` at ``wn`` [cm⁻¹]."""
        return self.s0 + self.eps * np.asarray(wn, dtype=float)

    def row(self) -> dict:
        """Flat summary for a results table."""
        return {"reg_eps": self.eps, "reg_eps_err": self.eps_err, "reg_s0": self.s0,
                "reg_s0_err": self.s0_err, "reg_eps_origin": self.eps_origin,
                "reg_eps_origin_err": self.eps_origin_err, "reg_nu_mid": self.nu_mid,
                "reg_lag_mid": self.lag_mid, "reg_lag_mid_err": self.lag_mid_err,
                "reg_rms": self.rms,
                "reg_n_used": self.n_used, "reg_n_bands": self.n_bands}


# =============================================================================
# Lag of one sub-band
# =============================================================================

def _detrend_rows(x: np.ndarray, M: np.ndarray) -> np.ndarray:
    """Remove a least-squares line in ``x`` from each row of ``M``."""
    X = np.vstack([np.ones_like(x), x - x.mean()]).T
    coef, *_ = np.linalg.lstsq(X, M.T, rcond=None)
    return M - (X @ coef).T


def _highpass_rows(M: np.ndarray, n: int) -> np.ndarray:
    """Each row minus its running mean over ``n`` points (edges reflected)."""
    return M - uniform_filter1d(M, size=n, axis=-1, mode="reflect")


def subband_lag(wn: np.ndarray, y: np.ndarray, template: CubicSpline,
                highpass: float = 0.0) -> tuple[float, float]:
    """
    Lag ℓ maximising corr(y(ν), T(ν − ℓ)) over one sub-band.

    Parameters
    ----------
    wn, y : np.ndarray
        The sub-band's measured points.
    template : CubicSpline
        The template as a function of ν.
    highpass : float
        Width [cm⁻¹] of a running mean subtracted from both before
        correlating, so lines rather than a band envelope set the lag;
        0 removes a straight line only.

    Returns
    -------
    lag : float
        [cm⁻¹], refined by a parabola through the peak (the grid value at the
        search edge).
    peak : float
        The normalised correlation at the best grid lag.
    """
    lags = np.arange(-LAG_MAX, LAG_MAX + LAG_STEP / 2, LAG_STEP)
    n_hp = int(round(highpass / np.mean(np.diff(wn)))) if highpass > 0 else 0
    filt = (lambda M: _highpass_rows(M, n_hp)) if n_hp >= 3 else (lambda M: _detrend_rows(wn, M))
    m = filt(y[None, :])[0]
    sm = m.std()
    if not sm > 1e-9 * max(float(np.max(np.abs(y))), np.finfo(float).tiny):
        return 0.0, np.nan          # no structure to correlate
    m = (m - m.mean()) / sm
    T = filt(template(wn[None, :] - lags[:, None]))
    st = T.std(axis=1)
    with np.errstate(invalid="ignore", divide="ignore"):
        cc = ((T - T.mean(axis=1, keepdims=True)) / st[:, None]) @ m / m.size
    cc = np.where(st > 0, cc, -np.inf)
    k = int(np.argmax(cc))
    if 0 < k < lags.size - 1 and np.all(np.isfinite(cc[k - 1:k + 2])):
        a, b, c = cc[k - 1], cc[k], cc[k + 1]
        den = a - 2 * b + c
        frac = 0.5 * (a - c) / den if den != 0 else 0.0
        return float(lags[k] + frac * LAG_STEP), float(cc[k])
    return float(lags[k]), float(cc[k])


# =============================================================================
# Sub-bands per geometry
# =============================================================================

def _continuum_envelope(wn: np.ndarray, y: np.ndarray, clear: np.ndarray) -> np.ndarray:
    """``y`` through the clear points, median-smoothed over ~25 cm⁻¹."""
    env = np.interp(wn, wn[clear], y[clear])
    return median_filter(env, size=max(3, int(25.0 / np.mean(np.diff(wn)))))


def layer_subbands(
    wn: np.ndarray,
    y: np.ndarray,
    template,
    wn_range: tuple[float, float] | None = None,
    width: float = LAYER_SUBBAND,
    highpass: float = LAYER_HIGHPASS,
) -> SubbandLags:
    """
    Sub-band lags of a layer single beam against the run's template.

    Parameters
    ----------
    wn, y : np.ndarray
        The (preprocessed) single beam.
    template : Template
        :class:`plumeIR.preprocess.template.Template`; its ``total``
        (ambient + plume) transmittance is the reference.
    wn_range : (float, float) or None
        Range to use; ``None`` uses the template's, 20 cm⁻¹ inside its ends.
    width : float
        Sub-band width [cm⁻¹].

    Returns
    -------
    SubbandLags
    """
    lo, hi = wn_range or (template.wn[0] + 20.0, template.wn[-1] - 20.0)
    sel = (wn > lo) & (wn < hi) & np.isfinite(y)
    wn, y = wn[sel], y[sel]
    f = CubicSpline(template.wn, template.total)
    t_on = f(wn)
    clear = t_on > CLEAR_T
    if clear.sum() < 2:
        raise PlumeIRError("the template has no clear points in this range; no "
                           "continuum to normalise the single beam by")
    r = y / _continuum_envelope(wn, y, clear)
    centres, lags, peaks = [], [], []
    for a in np.arange(lo, hi - width + 1e-9, width):
        b = (wn >= a) & (wn < a + width)
        if b.sum() < 20 or np.mean(t_on[b] < OPAQUE_T) > MAX_OPAQUE \
                or np.std(t_on[b]) < MIN_STRUCTURE:
            continue
        ok = b & (t_on > OPAQUE_T)
        lag, peak = subband_lag(wn[ok], r[ok], f, highpass)
        centres.append(a + width / 2)
        lags.append(lag)
        peaks.append(peak)
    return SubbandLags(np.array(centres), np.array(lags), np.array(peaks))


def emission_subbands(
    wn: np.ndarray,
    y: np.ndarray,
    wn_model: np.ndarray,
    radiance: np.ndarray,
    exclude=(),
    width: float = EMISSION_SUBBAND,
    highpass: float = EMISSION_HIGHPASS,
) -> SubbandLags:
    """
    Sub-band lags of a calibrated emission spectrum against a clear-sky model.

    Parameters
    ----------
    wn, y : np.ndarray
        The calibrated radiance spectrum.
    wn_model, radiance : np.ndarray
        The template: the clear-sky radiance on the model grid,
        ILS-convolved (FOV broadening only).
    exclude : sequence of (float, float)
        Ranges [cm⁻¹] not in the template (plume bands); a sub-band whose
        centre lies in one is skipped.  (Skipping any overlap also dropped
        1210-1235 cm⁻¹, which holds 1 cm⁻¹ of the SO2 band's edge, and that
        one sub-band moved ε from −2.19e-4 to −1.94e-4: 2026-11-16.)
    width : float
        Sub-band width [cm⁻¹].

    Returns
    -------
    SubbandLags
    """
    lo, hi = wn_model[0] + 10.0, wn_model[-1] - 10.0
    sel = (wn > lo) & (wn < hi) & np.isfinite(y)
    wn, y = wn[sel], y[sel]
    f = CubicSpline(wn_model, radiance)
    centres, lags, peaks = [], [], []
    for a in np.arange(lo, hi - width + 1e-9, width):
        if any(b_lo <= a + width / 2 <= b_hi for b_lo, b_hi in exclude):
            continue
        b = (wn >= a) & (wn < a + width)
        if b.sum() < 20:
            continue
        lag, peak = subband_lag(wn[b], y[b], f, highpass)
        centres.append(a + width / 2)
        lags.append(lag)
        peaks.append(peak)
    return SubbandLags(np.array(centres), np.array(lags), np.array(peaks))


def plume_bands(
    wn: np.ndarray,
    od_plume: np.ndarray,
    fraction: float = 0.1,
    min_gap: float = 10.0,
) -> list[tuple[float, float]]:
    """
    Ranges where the plume's optical depth exceeds ``fraction`` of its peak.

    Gaps narrower than ``min_gap`` cm⁻¹ are bridged, so a band's line forest
    comes out as one range.

    Parameters
    ----------
    wn, od_plume : np.ndarray
        Model grid and the plume gases' optical depth (any column: only its
        shape is used).
    """
    if not np.any(od_plume > 0):
        return []
    on = np.flatnonzero(od_plume > fraction * od_plume.max())
    out = []
    start = prev = wn[on[0]]
    for v in wn[on[1:]]:
        if v - prev > min_gap:
            out.append((float(start), float(prev)))
            start = v
        prev = v
    out.append((float(start), float(prev)))
    return out


# =============================================================================
# The fit lag = s0 + ε·ν
# =============================================================================

def _ols(X: np.ndarray, y: np.ndarray) -> tuple[np.ndarray, np.ndarray, float]:
    """Coefficients, their 1σ errors and the residual rms."""
    coef, *_ = np.linalg.lstsq(X, y, rcond=None)
    resid = y - X @ coef
    dof = max(y.size - X.shape[1], 1)
    s2 = float(resid @ resid) / dof
    cov = s2 * np.linalg.inv(X.T @ X)
    return coef, np.sqrt(np.diag(cov)), float(np.sqrt(np.mean(resid ** 2)))


def fit_registration(lags: SubbandLags, min_peak: float) -> RegistrationResult:
    """
    ``lag = s0 + ε·ν`` over the sub-bands, with outlier rejection.

    Parameters
    ----------
    lags : SubbandLags
    min_peak : float
        Minimum correlation peak (``LAYER_MIN_PEAK`` / ``EMISSION_MIN_PEAK``).

    Returns
    -------
    RegistrationResult

    Raises
    ------
    PlumeIRError
        With fewer than ``MIN_SUBBANDS`` usable sub-bands.
    """
    nu, lg, pk = lags.centres, lags.lags, lags.peaks
    good = (np.nan_to_num(pk, nan=-1.0) > min_peak) & (np.abs(lg) < LAG_MAX - LAG_STEP)
    m = good.copy()
    for _ in range(3):
        if m.sum() < MIN_SUBBANDS:
            break
        c = np.polyfit(nu[m], lg[m], 1)
        m = good & (np.abs(lg - np.polyval(c, nu)) < REJECT)
    if m.sum() < MIN_SUBBANDS:
        raise PlumeIRError(
            f"{int(m.sum())} usable sub-band(s) of {nu.size}; the registration "
            f"needs at least {MIN_SUBBANDS}"
        )
    (s0, eps), (s0_e, eps_e), rms = _ols(np.vstack([np.ones(m.sum()), nu[m]]).T, lg[m])
    (eps0,), (eps0_e,), _ = _ols(nu[m][:, None], lg[m])
    nu_mid = float(np.mean(nu[m]))
    (lag_mid, _), (lag_mid_e, _), _ = _ols(np.vstack([np.ones(m.sum()), nu[m] - nu_mid]).T, lg[m])
    return RegistrationResult(eps=float(eps), eps_err=float(eps_e), s0=float(s0),
                              s0_err=float(s0_e), eps_origin=float(eps0),
                              eps_origin_err=float(eps0_e), rms=rms, nu_mid=nu_mid,
                              lag_mid=float(lag_mid), lag_mid_err=float(lag_mid_e),
                              n_used=int(m.sum()), n_bands=int(nu.size),
                              subbands=lags, used=m)
