"""
Tests for the envelope metrics (``plumeIR.preprocess.envelope``).

Validated first on the validation sets (2026-11-16):
layer top-hull peak 1.93e5 counts at 988 cm⁻¹; relative scale f 0.99
[0.88, 1.10] against the median of the first 10 spectra, ΔT scatter ~105 K per
spectrum with the plume bands masked (plots only); emission opaque-band
brightness temperature 301.6 K [300.3, 302.4], the CO2 ν2 and H2O ν2 bands
agreeing within ~2 K.  These tests use synthetic spectra with known answers.

Run with:  pytest test/test_preprocess_envelope.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

from plumeIR.exceptions import PlumeIRError
from plumeIR.physics import rad
from plumeIR.preprocess.envelope import (
    Envelope, bottom_hull, envelope, opaque_brightness_temperature, reference_hull,
    relative_planck, top_hull,
)

WN = np.arange(600.0, 4400.0, 0.24)


def _beam(T: float = 1300.0, f: float = 1.0, lines: bool = True, seed: int = 0) -> np.ndarray:
    """f × Planck(T) × a smooth response, with absorption lines and a black band."""
    response = np.exp(-((WN - 1800.0) / 1400.0) ** 2)
    y = f * rad(WN, T) * response
    if lines:
        rng = np.random.default_rng(seed)
        od = np.zeros_like(WN)
        for c, d in zip(rng.uniform(600, 4400, 600), rng.uniform(0.1, 2.0, 600)):
            od += d * np.exp(-((WN - c) / 0.3) ** 2)
        y = y * np.exp(-od)
        y[(WN > 2280) & (WN < 2380)] = 0.0
    return y


class TestHulls:

    def test_hulls_bracket_the_spectrum(self):
        y = _beam()
        top, bot = top_hull(WN, y), bottom_hull(WN, y)
        assert np.all(bot <= top + 1e-9 * top.max())   # round-off in the black band
        assert np.mean(top >= y - 1e-12) > 0.95
        assert np.mean(bot <= y + 1e-12) > 0.95

    def test_top_hull_follows_the_continuum(self):
        """Away from the black band, the top hull is near the line-free beam."""
        y, clean = _beam(), _beam(lines=False)
        top = top_hull(WN, y)
        away = (np.abs(WN - 2330) > 60) & (WN > 700) & (WN < 4300)
        ratio = top[away] / clean[away]
        assert np.median(ratio) == pytest.approx(1.0, abs=0.05)

    def test_bottom_hull_runs_through_the_black_band(self):
        bot = bottom_hull(WN, _beam())
        assert np.all(bot[(WN > 2300) & (WN < 2360)] < 1e-3 * _beam().max())

    def test_nan_is_tolerated(self):
        y = _beam()
        y[100:110] = np.nan
        assert np.isfinite(top_hull(WN, y)).all()
        with pytest.raises(PlumeIRError):
            top_hull(WN, np.full(WN.size, np.nan))


class TestPeak:

    def test_peak_of_a_smooth_beam(self):
        y = _beam(lines=False)
        e = envelope(WN, y)
        k = int(np.argmax(y))
        assert e.peak_wn == pytest.approx(WN[k], abs=3.0)
        assert e.peak == pytest.approx(y[k], rel=0.01)
        assert e.integral > 0
        assert set(e.row()) == {"env_peak", "env_peak_wn", "env_integral"}


class TestRelative:

    @pytest.mark.parametrize("dT, f", [(0.0, 1.0), (+120.0, 0.8), (-80.0, 1.25)])
    def test_scale_and_temperature_recovered(self, dT, f):
        t_ref = 1300.0
        ref = top_hull(WN, _beam(t_ref))
        top = top_hull(WN, _beam(t_ref + dT, f))
        r = relative_planck(WN, top, ref, t_ref)
        assert r.dT == pytest.approx(dT, abs=3.0)
        assert r.f == pytest.approx(f, rel=0.01)

    def test_mask_leaves_points_out(self):
        ref = top_hull(WN, _beam())
        top = ref.copy()
        top[(WN > 2400) & (WN < 2600)] *= 0.5            # a "plume band"
        free = relative_planck(WN, top, ref, 1300.0)
        masked = relative_planck(WN, top, ref, 1300.0,
                                 mask=~((WN > 2380) & (WN < 2620)))
        assert abs(masked.dT) < 1.0 and masked.f == pytest.approx(1.0, rel=1e-3)
        assert abs(free.dT) > abs(masked.dT)

    def test_reference_is_the_median_of_the_first_n(self):
        envs = [Envelope(wn=WN, top=np.full(WN.size, v), bottom=np.zeros(WN.size))
                for v in (1.0, 3.0, 2.0, 100.0)]
        np.testing.assert_allclose(reference_hull(envs, 3), 2.0)
        with pytest.raises(PlumeIRError):
            reference_hull(envs, 0)

    def test_too_few_points(self):
        r = relative_planck(WN[:5], np.ones(5), np.ones(5), 1300.0)
        assert np.isnan(r.f) and r.n == 0


class TestOpaqueBrightnessTemperature:

    def test_blackbody_with_windows(self):
        """Opaque bands at the air temperature, windows colder; with noise."""
        wn = np.arange(600.0, 2000.0, 0.12)
        T_air = 301.0
        emis = np.where((wn < 720) | (wn > 1400), 1.0, 0.3)
        rng = np.random.default_rng(4)
        y = emis * rad(wn, T_air) + 0.3 * rng.standard_normal(wn.size)
        assert opaque_brightness_temperature(wn, y) == pytest.approx(T_air, abs=0.5)

    def test_nothing_positive(self):
        assert np.isnan(opaque_brightness_temperature(WN, -np.ones(WN.size)))
