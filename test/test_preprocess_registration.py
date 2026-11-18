"""
Tests for the wavenumber registration (``plumeIR.preprocess.registration``).

Validated first on real data (2026-11-16):

- layer, 418 MIDAC spectra, the run's own template over the whole instrument
  range (530-4470 cm⁻¹): ε through the origin −1.506e-4 against −1.503e-4 from
  seven fitted windows, r = 0.97 per spectrum, intercept −0.014 ± 0.011 cm⁻¹
  (a pure scale);
- emission, 300 Bruker spectra against the convolved clear-sky radiance, plume
  bands found from the reference (1093-1211 cm⁻¹, the prototype's 1090-1210):
  slope −2.19e-4, intercept +0.248 cm⁻¹ (prototype −2.18e-4, +0.247); a fitted
  stretch gives −2.27e-4.

These tests codify the method on synthetic line forests.

Run with:  pytest test/test_preprocess_registration.py -v
"""

from __future__ import annotations

import numpy as np
import pytest
from scipy.interpolate import CubicSpline

from plumeIR.preprocess.registration import (
    LAYER_MIN_PEAK, MIN_SUBBANDS, SubbandLags, emission_subbands, fit_registration,
    plume_bands, subband_lag,
)
from plumeIR.exceptions import PlumeIRError

WN_MODEL = np.arange(1800.0, 3400.0, 0.04)
WN_MEAS = np.arange(1850.0, 3350.0, 0.241)


def _forest(wn: np.ndarray, seed: int = 0, n: int = 1500, width: float = 0.25) -> np.ndarray:
    """Transmittance of a random line forest (lines as wide as the ILS)."""
    rng = np.random.default_rng(seed)
    centres = rng.uniform(wn[0], wn[-1], n)
    depth = rng.uniform(0.05, 0.6, n)
    od = np.zeros_like(wn)
    for c, d in zip(centres, depth):
        sel = np.abs(wn - c) < 5 * width
        od[sel] += d * np.exp(-((wn[sel] - c) / width) ** 2)
    return np.exp(-od)


TEMPLATE = _forest(WN_MODEL)
SPLINE = CubicSpline(WN_MODEL, TEMPLATE)


def _measured(s0: float, eps: float, noise: float = 0.0, seed: int = 1) -> np.ndarray:
    y = SPLINE(WN_MEAS - (s0 + eps * WN_MEAS))
    if noise:
        y = y + noise * np.random.default_rng(seed).standard_normal(y.size)
    return y


def _lags(y: np.ndarray, width: float = 40.0, highpass: float = 2.0) -> SubbandLags:
    centres, lags, peaks = [], [], []
    for a in np.arange(1870.0, 3330.0 - width, width):
        b = (WN_MEAS >= a) & (WN_MEAS < a + width)
        lag, peak = subband_lag(WN_MEAS[b], y[b], SPLINE, highpass)
        centres.append(a + width / 2)
        lags.append(lag)
        peaks.append(peak)
    return SubbandLags(np.array(centres), np.array(lags), np.array(peaks))


class TestLag:

    @pytest.mark.parametrize("shift", [-0.6, -0.123, 0.0, 0.0371, 0.45])
    def test_pure_shift_recovered(self, shift):
        b = (WN_MEAS >= 2500.0) & (WN_MEAS < 2540.0)
        lag, peak = subband_lag(WN_MEAS[b], _measured(shift, 0.0)[b], SPLINE)
        assert lag == pytest.approx(shift, abs=3e-3)
        assert peak > 0.99

    def test_highpass_removes_an_envelope(self):
        """A smooth envelope missing from the template pulls a detrended lag;
        the high-pass makes the lines set it."""
        b = (WN_MEAS >= 2500.0) & (WN_MEAS < 2540.0)
        wn = WN_MEAS[b]
        y = _measured(0.1, 0.0)[b] * (1.0 - 0.6 * np.exp(-((wn - 2512.0) / 6.0) ** 2))
        detrended, _ = subband_lag(wn, y, SPLINE, highpass=0.0)
        filtered, _ = subband_lag(wn, y, SPLINE, highpass=2.0)
        assert abs(filtered - 0.1) < 0.01
        assert abs(filtered - 0.1) < abs(detrended - 0.1)

    def test_flat_measurement_gives_no_peak(self):
        lag, peak = subband_lag(WN_MEAS[:100], np.ones(100), SPLINE)
        assert np.isnan(peak)


class TestFit:

    def test_scale_recovered(self):
        r = fit_registration(_lags(_measured(0.0, -1.5e-4, noise=0.005)), LAYER_MIN_PEAK)
        assert r.eps_origin == pytest.approx(-1.5e-4, abs=3e-6)
        assert abs(r.s0) < 3 * r.s0_err + 0.01
        assert r.n_used == r.n_bands

    def test_shift_and_scale_recovered(self):
        r = fit_registration(_lags(_measured(0.25, -2.2e-4, noise=0.005)), LAYER_MIN_PEAK)
        assert r.eps == pytest.approx(-2.2e-4, abs=5e-6)
        assert r.s0 == pytest.approx(0.25, abs=0.015)
        np.testing.assert_allclose(r.lag(np.array([2000.0])), 0.25 - 2.2e-4 * 2000.0,
                                   atol=0.01)
        # the lag at mid-range is the same line, and better determined than s0
        assert r.lag_mid == pytest.approx(float(r.lag(np.array([r.nu_mid]))[0]), abs=1e-12)
        assert r.lag_mid_err < r.s0_err

    def test_outlier_subband_rejected(self):
        lags = _lags(_measured(0.0, -1.5e-4))
        bad = lags.lags.copy()
        bad[5] += 0.3
        r = fit_registration(SubbandLags(lags.centres, bad, lags.peaks), LAYER_MIN_PEAK)
        assert not r.used[5]
        assert r.eps_origin == pytest.approx(-1.5e-4, abs=2e-6)

    def test_low_peaks_and_edge_lags_not_used(self):
        lags = _lags(_measured(0.0, -1.5e-4))
        peaks, lg = lags.peaks.copy(), lags.lags.copy()
        peaks[0] = 0.2
        lg[1] = -0.8
        r = fit_registration(SubbandLags(lags.centres, lg, peaks), LAYER_MIN_PEAK)
        assert not r.used[0] and not r.used[1]

    def test_too_few_subbands(self):
        lags = _lags(_measured(0.0, -1.5e-4))
        k = MIN_SUBBANDS - 1
        with pytest.raises(PlumeIRError, match="at least"):
            fit_registration(SubbandLags(lags.centres[:k], lags.lags[:k], lags.peaks[:k]),
                             LAYER_MIN_PEAK)


class TestEmission:

    def test_plume_bands_bridge_gaps(self):
        wn = np.arange(1000.0, 1300.0, 0.1)
        od = np.where((wn > 1100) & (wn < 1200) & (np.round(wn) % 3 == 0), 1.0, 0.0)
        (lo, hi), = plume_bands(wn, od)
        assert 1100.0 <= lo < 1101.5 and 1198.5 < hi < 1200.0
        assert plume_bands(wn, np.zeros_like(wn)) == []

    def test_subbands_centred_in_a_plume_band_skipped(self):
        wn = np.arange(700.0, 1300.0, 0.04)
        L = 50.0 * _forest(wn, seed=2, n=600, width=0.3)
        wm = np.arange(720.0, 1280.0, 0.241)
        y = CubicSpline(wn, L)(wm - 0.1)
        got = emission_subbands(wm, y, wn, L, exclude=[(1090.0, 1210.0)])
        assert not np.any((got.centres >= 1090.0) & (got.centres <= 1210.0))
        assert np.any((got.centres > 1200.0) & (got.centres < 1225.0))   # 1210-1235 kept
        np.testing.assert_allclose(got.lags[got.peaks > 0.9], 0.1, atol=5e-3)
