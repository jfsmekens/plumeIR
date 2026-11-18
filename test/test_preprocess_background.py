"""
Tests for the additive-background product (``plumeIR.preprocess.background``).

Validated first against the prototype (2026-11-16): on the 418 layer
validation spectra, with the prototype's four opaque bands, ``a`` agrees with
the prototype's model M1 to 9e-4 (median 0.845), ``c`` to 0.6
counts and the per-band held-out residuals to 9 counts (−548 / 368 / −600 /
219), the differences coming from band-edge points (the template no longer
carries the FOV shift).  These tests codify the method on synthetic spectra.

Run with:  pytest test/test_preprocess_background.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from plumeIR.preprocess.background import (
    HELDOUT_FLAG_FRACTION, measure_background, remove_background,
)
from plumeIR.exceptions import PlumeIRError
from plumeIR.spectra.spectrum import Spectrum, SpectrumType

WN = np.arange(600.0, 4400.0, 0.25)
BANDS = [(670.0, 705.0), (1360.0, 1850.0), (2266.0, 2367.0), (3535.0, 3936.0)]
#: Self spectrum shaped like the measured one: large at low ν, ~0 at high.
SELF = 5000.0 * np.exp(-(WN - 600.0) / 400.0) + 5.0
#: A source continuum, black inside the bands.
OPEN = np.ones_like(WN, dtype=bool)
for lo, hi in BANDS:
    OPEN &= ~((WN >= lo) & (WN <= hi))
SOURCE = np.where(OPEN, 1.2e5 * np.exp(-((WN - 2600.0) / 1200.0) ** 2), 0.0)


def _beam(a: float, c: float, z=None, noise: float = 0.0, seed: int = 0) -> np.ndarray:
    """Source + a·S + c, plus a level ``z[k]`` confined to band k."""
    y = SOURCE + a * SELF + c
    for (lo, hi), level in zip(BANDS, z or ()):
        y = y + np.where((WN >= lo) & (WN <= hi), level, 0.0)
    if noise:
        y = y + noise * np.random.default_rng(seed).standard_normal(WN.size)
    return y


def _spectrum(y: np.ndarray) -> Spectrum:
    return Spectrum(x=WN, y=y, x_unit="wn", timestamp=None,
                    filename=Path("plume_0001_sbm.spc"), spec_type=SpectrumType.SBM)


class TestRecovery:

    def test_scale_and_constant_recovered(self):
        r = measure_background(WN, _beam(0.84, 80.0), BANDS, SELF)
        assert r.a == pytest.approx(0.84, rel=1e-10)
        assert r.c == pytest.approx(80.0, abs=1e-6)
        np.testing.assert_allclose(r.z, 80.0, atol=1e-6)
        np.testing.assert_allclose(r.heldout, 0.0, atol=1e-6)
        assert r.flags == ()

    def test_local_zero_level_shows_in_its_band(self):
        """A level confined to one band: z carries it, the held-out misses it.
        (a·S + c is fitted to all four bands, so the level biases a and c a
        little and z shows it relative to the others.)"""
        z_true = [0.0, 0.0, -600.0, 0.0]
        r = measure_background(WN, _beam(0.84, 80.0, z=z_true), BANDS, SELF)
        others = np.delete(r.z, 2)
        assert r.z[2] < np.median(others) - 400.0
        assert r.heldout[2] < -400.0
        # z(ν) follows the band levels and is flat beyond the outer bands
        zl = r.zero_level(np.array([600.0, 2316.5, 4300.0]))
        assert zl[1] == pytest.approx(r.z[2])
        assert (zl[0], zl[2]) == (pytest.approx(r.z[0]), pytest.approx(r.z[-1]))

    def test_noise_does_not_bias(self):
        r = measure_background(WN, _beam(0.9, 50.0, noise=200.0, seed=3), BANDS, SELF)
        assert r.a == pytest.approx(0.9, abs=0.01)
        assert r.c == pytest.approx(50.0, abs=20.0)

    def test_without_self_spectrum_measures_the_level(self):
        r = measure_background(WN, _beam(0.0, 120.0), BANDS)
        assert np.isnan(r.a)
        assert r.c == pytest.approx(120.0, abs=1e-6)
        np.testing.assert_allclose(r.z, 120.0, atol=1e-6)


class TestRemove:

    def test_removal_leaves_zero_in_bands_and_does_not_clip(self):
        y = _beam(0.84, 80.0)
        r = measure_background(WN, y, BANDS, SELF)
        out = remove_background(_spectrum(y), r, SELF)
        np.testing.assert_allclose(out.y[~OPEN], 0.0, atol=1e-6)
        np.testing.assert_allclose(out.y[OPEN], SOURCE[OPEN], atol=1e-6)

    def test_negative_values_kept(self):
        y = _beam(0.84, 80.0, noise=50.0, seed=1)
        r = measure_background(WN, y, BANDS, SELF)
        out = remove_background(_spectrum(y), r, SELF)
        assert (out.y[~OPEN] < 0).sum() > 0.3 * (~OPEN).sum()


class TestFlags:

    def test_scale_outside_range_flagged(self):
        r = measure_background(WN, _beam(1.6, 0.0), BANDS, SELF)
        assert any("self scale" in f for f in r.flags)

    def test_large_heldout_flagged(self):
        big = 3.0 * HELDOUT_FLAG_FRACTION * 1.2e5
        r = measure_background(WN, _beam(0.84, 0.0, z=[big, 0, -big, 0]), BANDS, SELF)
        assert any("held-out" in f for f in r.flags)


class TestInputs:

    def test_too_few_bands(self):
        with pytest.raises(PlumeIRError, match="at least"):
            measure_background(WN, _beam(0.84, 80.0), BANDS[:1], SELF)

    def test_bands_without_points_are_dropped(self):
        r = measure_background(WN, _beam(0.84, 80.0), BANDS + [(5000.0, 5100.0)], SELF)
        assert len(r.bands) == 4

    def test_shape_mismatch(self):
        with pytest.raises(PlumeIRError, match="shape"):
            measure_background(WN, _beam(0.84, 80.0), BANDS, SELF[:-1])

    def test_row_has_one_entry_per_band(self):
        row = measure_background(WN, _beam(0.84, 80.0), BANDS, SELF).row()
        assert {"bkg_a", "bkg_c", "bkg_heldout_rms", "bkg_flags"} <= set(row)
        assert sum(k.startswith("bkg_z_") for k in row) == 4
