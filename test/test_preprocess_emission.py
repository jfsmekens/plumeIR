"""
Tests for emission-geometry preprocessing (two-point blackbody calibration).

Split from ``test_preprocess.py``, which mirrors ``preprocess/layer.py``; this
file mirrors ``preprocess/emission.py`` and ``preprocess/drift.py``.

The calibration is exercised against **synthetic single beams built from a
known IRF and instrument radiance**, so the recovered radiance has an exact
expected value rather than a tolerance.  The real-data behaviour it codifies was validated
against ``data/2019-09-11/`` and against v2's algebra (agreement to 2e-15).
"""

from __future__ import annotations

from datetime import datetime, timedelta
from pathlib import Path

import numpy as np
import pytest

from plumeIR.exceptions import PlumeIRError
from plumeIR.spectra.spectrum import Spectrum, SpectrumType
from plumeIR.config.objects import DataConfig, EmissionGeometry
from plumeIR.physics import bbt, rad
from plumeIR.preprocess import preprocess_spectra
from plumeIR.preprocess.drift import DriftingSpectrum
from plumeIR.preprocess.emission import (
    BlackbodyCalibration, build_blackbody_calibration, calibrate_spectrum,
    pair_blackbodies, preprocess_emission,
)

WN = np.linspace(700.0, 1300.0, 128)
T0 = datetime(2019, 9, 11, 18, 27, 51)

T_COLD, T_HOT = 306.15, 323.15

# A plausible instrument: an instrument response function (IRF) rolling off
# across the band, and a positive instrument radiance L_inst — the scene
# radiance giving zero counts.  Magnitude matches the 2019-09-11 Bruker
# (7-40 mW/(m2 sr cm-1) across 700-1300 cm-1).
IRF    = 0.01 * (1.0 - 0.3 * (WN - WN[0]) / (WN[-1] - WN[0]))
L_INST = rad(WN, 215.0)


def single_beam(radiance, irf=IRF, L_inst=L_INST):
    """Counts this instrument would record for a given scene radiance."""
    return irf * (radiance - L_inst)


def make_bb(temp, timestamp=T0, name=None, irf=IRF, L_inst=L_INST, x=WN):
    """A synthetic blackbody acquisition of a target at ``temp``."""
    name = name or ("BBC.0" if temp < 0.5 * (T_COLD + T_HOT) else "BBH.0")
    return Spectrum(
        x=np.asarray(x, dtype=float),
        y=single_beam(rad(x, temp), irf, L_inst),
        x_unit="wn", timestamp=timestamp, filename=Path(name),
        spec_type=SpectrumType.SBM, is_blackbody=True, bb_temp=temp,
    )


def make_scene(radiance, timestamp=T0, name="plume_0001.0",
               irf=IRF, L_inst=L_INST):
    """A synthetic scene spectrum whose true radiance is known."""
    return Spectrum(
        x=WN.copy(), y=single_beam(radiance, irf, L_inst), x_unit="wn",
        timestamp=timestamp, filename=Path(name), spec_type=SpectrumType.SBM,
    )


@pytest.fixture
def patched_reader(monkeypatch):
    """Patch read_spectrum in the emission module to read from a dict."""
    registry: dict[str, Spectrum] = {}

    def fake_read(path):
        return registry[Path(path).name]

    monkeypatch.setattr("plumeIR.preprocess.emission.read_spectrum", fake_read)
    return registry


@pytest.fixture
def calibration(patched_reader):
    """A static calibration built from one synthetic hot/cold pair."""
    patched_reader["BBC.0"] = make_bb(T_COLD)
    patched_reader["BBH.0"] = make_bb(T_HOT)
    return build_blackbody_calibration([Path("BBC.0"), Path("BBH.0")])


def emission_geometry(**kwargs):
    """An EmissionGeometry with the fields preprocessing actually reads."""
    kwargs.setdefault("fit_difference", False)
    return EmissionGeometry(
        plume_height=1.0, plume_thickness=0.1, elev=30.0, obs_height=0.1,
        atm_path=Path("/nonexistent/atm.atm"), **kwargs,
    )


# =============================================================================
# Pairing hot and cold targets
# =============================================================================

class TestPairBlackbodies:

    def test_pairs_by_temperature_not_filename(self):
        """
        Hot/cold is decided by target temperature.  Here the *names* are
        swapped relative to the temperatures; the pairing must follow the
        physics, not the label (v2 matched on 'bbc'/'bbh' substrings).
        """
        cold = make_bb(T_COLD, name="BBH.0")   # deliberately mislabelled
        hot  = make_bb(T_HOT,  name="BBC.0")

        (got_cold, got_hot), = pair_blackbodies([cold, hot])

        assert got_cold.bb_temp == T_COLD
        assert got_hot.bb_temp == T_HOT

    def test_pairs_in_time_order(self):
        later  = T0 + timedelta(minutes=30)
        specs  = [make_bb(T_HOT, later), make_bb(T_COLD, T0),
                  make_bb(T_HOT, T0), make_bb(T_COLD, later)]

        pairs = pair_blackbodies(specs)

        assert len(pairs) == 2
        assert [c.timestamp for c, _ in pairs] == [T0, later]
        assert [h.timestamp for _, h in pairs] == [T0, later]

    def test_missing_target_temperature_raises(self):
        cold = make_bb(T_COLD)
        hot  = make_bb(T_HOT)
        object.__setattr__(hot, "bb_temp", None)

        with pytest.raises(PlumeIRError, match="no target temperature"):
            pair_blackbodies([cold, hot])

    def test_three_temperatures_raise(self):
        specs = [make_bb(T_COLD), make_bb(T_HOT), make_bb(315.0)]

        with pytest.raises(PlumeIRError, match="exactly 2 blackbody target"):
            pair_blackbodies(specs)

    def test_targets_too_close_raise(self):
        specs = [make_bb(T_COLD), make_bb(T_COLD + 0.5)]

        with pytest.raises(PlumeIRError, match="apart"):
            pair_blackbodies(specs)

    def test_unbalanced_sets_raise(self):
        specs = [make_bb(T_COLD), make_bb(T_COLD, T0 + timedelta(minutes=1)),
                 make_bb(T_HOT)]

        with pytest.raises(PlumeIRError, match="Unbalanced"):
            pair_blackbodies(specs)


# =============================================================================
# Building the calibration
# =============================================================================

class TestBuildCalibration:

    def test_recovers_the_irf_and_instrument_radiance(self, calibration):
        """The whole point: invert the synthetic instrument exactly."""
        np.testing.assert_allclose(calibration.irf.at(T0), IRF, rtol=1e-12)
        np.testing.assert_allclose(calibration.L_inst.at(T0), L_INST, rtol=1e-12)
        assert (calibration.L_inst.at(T0) > 0).all()

    def test_calibrating_the_targets_returns_planck(self, calibration):
        """A two-point calibration must reproduce its own two points."""
        for temp in (T_COLD, T_HOT):
            got = calibration.radiance(make_bb(temp))
            np.testing.assert_allclose(got, rad(WN, temp), rtol=1e-10)

    def test_single_pair_is_static(self, calibration):
        assert calibration.is_static
        assert calibration.n_sets == 1
        assert calibration.temps == (T_COLD, T_HOT)

    def test_drift_is_fitted_across_sets(self, patched_reader):
        """
        An IRF drifting linearly in time must be tracked, not averaged: a
        scene acquired between two calibration sets is calibrated with the
        interpolated response.
        """
        t1 = T0 + timedelta(minutes=40)
        for name, temp, when, scale in [
            ("BBC_0.0", T_COLD, T0, 1.0), ("BBH_0.0", T_HOT, T0, 1.0),
            ("BBC_1.0", T_COLD, t1, 1.2), ("BBH_1.0", T_HOT, t1, 1.2),
        ]:
            patched_reader[name] = make_bb(temp, when, name=name,
                                           irf=IRF * scale)

        cal = build_blackbody_calibration([Path(n) for n in patched_reader])

        assert not cal.is_static
        assert cal.n_sets == 2
        midpoint = T0 + timedelta(minutes=20)
        np.testing.assert_allclose(cal.irf.at(midpoint), IRF * 1.1, rtol=1e-10)

    def test_drift_disabled_averages(self, patched_reader):
        t1 = T0 + timedelta(minutes=40)
        for name, temp, when, scale in [
            ("BBC_0.0", T_COLD, T0, 1.0), ("BBH_0.0", T_HOT, T0, 1.0),
            ("BBC_1.0", T_COLD, t1, 1.2), ("BBH_1.0", T_HOT, t1, 1.2),
        ]:
            patched_reader[name] = make_bb(temp, when, name=name,
                                           irf=IRF * scale)

        cal = build_blackbody_calibration([Path(n) for n in patched_reader],
                                          fit_drift=False)

        assert cal.is_static
        np.testing.assert_allclose(cal.irf.at(t1), IRF * 1.1, rtol=1e-10)

    def test_singular_points_are_masked_not_patched(self, patched_reader):
        """
        Where both targets give the same counts the calibration carries no
        information.  v2 substituted the smallest non-zero denominator,
        inventing a response; the result must be NaN instead.
        """
        cold = make_bb(T_COLD)
        hot  = make_bb(T_HOT)
        hot.y[10] = cold.y[10]                      # a dead detector point
        patched_reader["BBC.0"], patched_reader["BBH.0"] = cold, hot

        cal = build_blackbody_calibration([Path("BBC.0"), Path("BBH.0")])

        assert not cal.valid[10]
        assert cal.valid.sum() == WN.size - 1
        got = cal.radiance(make_scene(rad(WN, 280.0)))
        assert np.isnan(got[10])
        assert np.isfinite(np.delete(got, 10)).all()

    def test_mismatched_axes_raise(self, patched_reader):
        short = np.linspace(700.0, 1300.0, 64)
        patched_reader["BBC.0"] = make_bb(T_COLD)
        patched_reader["BBH.0"] = make_bb(
            T_HOT, x=short, irf=np.full(64, 0.01), L_inst=rad(short, 215.0),
        )

        with pytest.raises(PlumeIRError, match="spectral axis"):
            build_blackbody_calibration([Path("BBC.0"), Path("BBH.0")])

    def test_empty_paths_raise(self):
        with pytest.raises(PlumeIRError, match="No blackbody spectra"):
            build_blackbody_calibration([])


# =============================================================================
# Calibrating a scene spectrum
# =============================================================================

class TestCalibrateSpectrum:

    def test_recovers_known_radiance(self, calibration):
        truth = rad(WN, 265.0)

        out = calibrate_spectrum(make_scene(truth), calibration)

        np.testing.assert_allclose(out.y, truth, rtol=1e-10)
        assert out.spec_type is SpectrumType.RAD

    def test_bbt_mode(self, calibration):
        truth = rad(WN, 265.0)

        out = calibrate_spectrum(make_scene(truth), calibration, use_bbt=True)

        np.testing.assert_allclose(out.y, np.full_like(WN, 265.0), rtol=1e-8)
        assert out.spec_type is SpectrumType.BBT

    def test_input_is_not_mutated(self, calibration):
        spec = make_scene(rad(WN, 265.0))
        before = spec.y.copy()

        calibrate_spectrum(spec, calibration)

        np.testing.assert_array_equal(spec.y, before)

    def test_negative_radiance_is_kept(self, calibration):
        """
        Radiance is signed.  Unlike a single beam, a negative value after
        calibration is data (noise about a near-zero cold band), not an
        artefact to clip — the layer path's clip_negatives must not apply.
        """
        truth = rad(WN, 265.0)
        truth[:5] = -0.5

        out = calibrate_spectrum(make_scene(truth), calibration)

        np.testing.assert_allclose(out.y[:5], -0.5, rtol=1e-8)

    def test_axis_unit_mismatch_raises(self, calibration):
        spec = make_scene(rad(WN, 265.0))
        object.__setattr__(spec, "x_unit", "wl")

        with pytest.raises(PlumeIRError, match="axis"):
            calibration.radiance(spec)


# =============================================================================
# The preprocessing stage
# =============================================================================

class TestPreprocessEmission:

    def _write_bb_files(self, tmp_path):
        """Real files on disk, so discovery is exercised too."""
        return tmp_path

    def test_dispatches_from_preprocess_spectra(self, monkeypatch, tmp_path,
                                                patched_reader):
        for name, temp in [("BBC.0", T_COLD), ("BBH.0", T_HOT)]:
            (tmp_path / name).touch()
            patched_reader[name] = make_bb(temp, name=name)
        truth = rad(WN, 265.0)

        out = preprocess_spectra(
            [make_scene(truth)], emission_geometry(),
            DataConfig(data_dir=tmp_path),
        )

        assert out[0].spec_type is SpectrumType.RAD
        np.testing.assert_allclose(out[0].y, truth, rtol=1e-10)

    def test_subtract_self_is_inert(self, tmp_path, patched_reader):
        """
        Self-emission is a transmission-geometry concept.  In emission the
        instrument radiance *is* the calibration's L_inst, so the flag must have
        no effect at all rather than subtracting the term twice.
        """
        for name, temp in [("BBC.0", T_COLD), ("BBH.0", T_HOT)]:
            (tmp_path / name).touch()
            patched_reader[name] = make_bb(temp, name=name)
        scene, cfg = [make_scene(rad(WN, 265.0))], DataConfig(data_dir=tmp_path)

        on  = preprocess_emission(scene, emission_geometry(subtract_self=True), cfg)
        off = preprocess_emission(scene, emission_geometry(subtract_self=False), cfg)

        np.testing.assert_array_equal(on[0].y, off[0].y)

    def test_fit_difference_raises(self, tmp_path):
        with pytest.raises(PlumeIRError, match="fit_difference"):
            preprocess_emission(
                [make_scene(rad(WN, 265.0))],
                emission_geometry(fit_difference=True),
                DataConfig(data_dir=tmp_path),
            )

    def test_missing_blackbodies_raise(self, tmp_path):
        with pytest.raises(PlumeIRError, match="No blackbody calibration"):
            preprocess_emission(
                [make_scene(rad(WN, 265.0))], emission_geometry(),
                DataConfig(data_dir=tmp_path),
            )

    def test_finds_uppercase_filenames(self, tmp_path, patched_reader):
        """
        Discovery must be case-insensitive for reference files: real Bruker
        datasets ship 'BBC.0'/'BBH.0', while the markers are lowercase.
        """
        for name, temp in [("BBC.0", T_COLD), ("BBH.0", T_HOT)]:
            (tmp_path / name).touch()
            patched_reader[name] = make_bb(temp, name=name)

        out = preprocess_emission(
            [make_scene(rad(WN, 265.0))], emission_geometry(),
            DataConfig(data_dir=tmp_path),
        )

        assert len(out) == 1


# =============================================================================
# The shared drift model
# =============================================================================

class TestDriftingSpectrum:

    def test_linear_drift_is_recovered(self):
        times  = [T0 + timedelta(minutes=m) for m in (0, 10, 20)]
        values = np.vstack([np.full(4, v) for v in (1.0, 2.0, 3.0)])

        d = DriftingSpectrum.from_samples(
            np.arange(4.0), values, times, "test quantity",
        )

        assert not d.is_static
        np.testing.assert_allclose(d.at(T0 + timedelta(minutes=5)),
                                   np.full(4, 1.5), atol=1e-10)

    def test_single_sample_is_static(self):
        d = DriftingSpectrum.from_samples(
            np.arange(4.0), np.ones((1, 4)), [T0], "test quantity",
        )

        assert d.is_static
        assert d.n_ref == 1

    def test_missing_timestamps_average(self):
        values = np.vstack([np.full(4, 1.0), np.full(4, 3.0)])

        d = DriftingSpectrum.from_samples(
            np.arange(4.0), values, [T0, None], "test quantity",
        )

        assert d.is_static
        np.testing.assert_allclose(d.at(T0), np.full(4, 2.0))

    def test_length_mismatch_raises(self):
        with pytest.raises(PlumeIRError, match="timestamps"):
            DriftingSpectrum.from_samples(
                np.arange(4.0), np.ones((2, 4)), [T0], "test quantity",
            )
