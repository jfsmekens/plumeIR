"""
Tests for the preprocessing stage (Phase 6).

Covers layer self-emission subtraction end to end: reference-file discovery and
exclusion, the static and drift background models, negative-value handling, and
the pipeline dispatch.  Spectrum objects are built in memory and file reads are
monkeypatched, so nothing here needs real instrument files.

Run with:  pytest test/test_preprocess.py -v
"""

from __future__ import annotations

from datetime import datetime, timedelta
from pathlib import Path

import numpy as np
import pytest

from plumeIR.exceptions import PlumeIRError
from plumeIR.spectra.discovery import discover_spectra, is_reference_file
from plumeIR.spectra.spectrum import Spectrum, SpectrumType
from plumeIR.config.objects import (
    DataConfig, EmissionGeometry, LayerGeometry, PreprocessConfig,
)
from plumeIR.preprocess import preprocess_spectra
from plumeIR.preprocess.layer import (
    SelfBackground, build_self_background, clip_negatives, find_self_spectra,
    preprocess_layer, subtract_self_emission,
)

WN = np.linspace(2400.0, 2600.0, 64)
T0 = datetime(2021, 8, 11, 0, 36, 1)


def make_spectrum(y, timestamp=T0, name="plume_0001_sbm.spc", x=WN):
    """A single-beam Spectrum with the given values."""
    return Spectrum(
        x=np.asarray(x, dtype=float), y=np.asarray(y, dtype=float),
        x_unit="wn", timestamp=timestamp, filename=Path(name),
        spec_type=SpectrumType.SBM,
    )


@pytest.fixture
def patched_reader(monkeypatch):
    """
    Patch read_spectrum so build_self_background reads from a dict of
    {filename: Spectrum} instead of the filesystem.
    """
    registry: dict[str, Spectrum] = {}

    def fake_read(path):
        return registry[Path(path).name]

    monkeypatch.setattr("plumeIR.preprocess.layer.read_spectrum", fake_read)
    return registry


# =============================================================================
# Reference-file identification and exclusion
# =============================================================================

class TestReferenceFiles:

    def test_is_reference_file_matches_self(self):
        assert is_reference_file("self.spc")
        assert is_reference_file("self_sbm.spc")
        assert is_reference_file(Path("/data/SELF.0"))          # case-insensitive
        assert not is_reference_file("plume_0001_sbm.spc")

    def test_discovery_excludes_self_by_default(self, tmp_path):
        (tmp_path / "self.spc").write_bytes(b"")
        for i in range(3):
            (tmp_path / f"plume_{i}.spc").write_bytes(b"")

        found = discover_spectra(tmp_path, filter_ext="spc")
        assert sorted(p.name for p in found) == [
            "plume_0.spc", "plume_1.spc", "plume_2.spc"
        ]

    def test_discovery_excludes_self_that_matches_filter_string(self, tmp_path):
        """
        The v2 trap: 'self_sbm.spc' contains 'sbm', so a filter_string of 'sbm'
        would otherwise pull the self spectrum into the fit list.
        """
        (tmp_path / "self_sbm.spc").write_bytes(b"")
        (tmp_path / "vent_0001_sbm.spc").write_bytes(b"")

        found = discover_spectra(tmp_path, filter_string="sbm", filter_ext="spc")
        assert [p.name for p in found] == ["vent_0001_sbm.spc"]

    def test_discovery_can_opt_in_to_reference_files(self, tmp_path):
        (tmp_path / "self.spc").write_bytes(b"")
        (tmp_path / "plume_0.spc").write_bytes(b"")

        found = discover_spectra(tmp_path, filter_ext="spc",
                                 exclude_reference=False)
        assert len(found) == 2


class TestFindSelfSpectra:

    def test_finds_by_convention(self, tmp_path):
        (tmp_path / "self.spc").write_bytes(b"")
        (tmp_path / "plume_0.spc").write_bytes(b"")

        found = find_self_spectra(tmp_path, filter_ext="spc")
        assert [p.name for p in found] == ["self.spc"]

    def test_explicit_file_overrides_discovery(self, tmp_path):
        (tmp_path / "self.spc").write_bytes(b"")
        explicit = tmp_path / "background.spc"
        explicit.write_bytes(b"")

        found = find_self_spectra(tmp_path, self_file=explicit)
        assert found == [explicit]

    def test_missing_explicit_file_raises(self, tmp_path):
        with pytest.raises(PlumeIRError, match="self_file not found"):
            find_self_spectra(tmp_path, self_file=tmp_path / "absent.spc")

    def test_returns_empty_when_none_present(self, tmp_path):
        (tmp_path / "plume_0.spc").write_bytes(b"")
        assert find_self_spectra(tmp_path, filter_ext="spc") == []


# =============================================================================
# Background construction
# =============================================================================

class TestBuildSelfBackground:

    def test_single_spectrum_is_static(self, patched_reader):
        y = np.full_like(WN, 3.0)
        patched_reader["self.spc"] = make_spectrum(y, name="self.spc")

        bkg = build_self_background([Path("self.spc")])

        assert bkg.is_static
        assert bkg.n_ref == 1
        assert bkg.coeffs is None
        np.testing.assert_allclose(bkg.at(T0), y)

    def test_multiple_spectra_fit_drift(self, patched_reader):
        """Background rising 1.0 per 100 s recovers exactly at each node."""
        for i in range(3):
            patched_reader[f"self_{i}.spc"] = make_spectrum(
                np.full_like(WN, 2.0 + i), timestamp=T0 + timedelta(seconds=100 * i),
                name=f"self_{i}.spc",
            )

        bkg = build_self_background([Path(f"self_{i}.spc") for i in range(3)])

        assert not bkg.is_static
        assert bkg.n_ref == 3
        assert bkg.coeffs.shape == (3, len(WN))         # deg 2 → 3 coefficients
        for i in range(3):
            np.testing.assert_allclose(
                bkg.at(T0 + timedelta(seconds=100 * i)),
                np.full_like(WN, 2.0 + i), atol=1e-8,
            )

    def test_drift_interpolates_between_nodes(self, patched_reader):
        for i in range(2):
            patched_reader[f"self_{i}.spc"] = make_spectrum(
                np.full_like(WN, 2.0 + 2.0 * i),
                timestamp=T0 + timedelta(seconds=100 * i), name=f"self_{i}.spc",
            )

        bkg = build_self_background([Path(f"self_{i}.spc") for i in range(2)])

        # Two spectra → degree 1; halfway in time is halfway in value.
        assert bkg.coeffs.shape == (2, len(WN))
        np.testing.assert_allclose(
            bkg.at(T0 + timedelta(seconds=50)), np.full_like(WN, 3.0), atol=1e-8,
        )

    def test_spectra_sorted_by_time_regardless_of_input_order(self, patched_reader):
        for i in range(3):
            patched_reader[f"self_{i}.spc"] = make_spectrum(
                np.full_like(WN, 2.0 + i), timestamp=T0 + timedelta(seconds=100 * i),
                name=f"self_{i}.spc",
            )

        shuffled = [Path("self_2.spc"), Path("self_0.spc"), Path("self_1.spc")]
        bkg = build_self_background(shuffled)

        assert [f.name for f in bkg.files] == [
            "self_0.spc", "self_1.spc", "self_2.spc"
        ]
        np.testing.assert_allclose(bkg.times, [0.0, 100.0, 200.0])

    def test_no_timestamps_averages(self, patched_reader):
        patched_reader["self_a.spc"] = make_spectrum(
            np.full_like(WN, 2.0), timestamp=None, name="self_a.spc")
        patched_reader["self_b.spc"] = make_spectrum(
            np.full_like(WN, 4.0), timestamp=None, name="self_b.spc")

        bkg = build_self_background([Path("self_a.spc"), Path("self_b.spc")])

        assert bkg.is_static
        np.testing.assert_allclose(bkg.at(T0), np.full_like(WN, 3.0))

    def test_mismatched_axes_raise(self, patched_reader):
        patched_reader["self_a.spc"] = make_spectrum(
            np.full_like(WN, 2.0), name="self_a.spc")
        other = np.linspace(2400.0, 2600.0, 32)
        patched_reader["self_b.spc"] = make_spectrum(
            np.full(32, 2.0), name="self_b.spc", x=other)

        with pytest.raises(PlumeIRError, match="do not share a spectral axis"):
            build_self_background([Path("self_a.spc"), Path("self_b.spc")])

    def test_empty_list_raises(self):
        with pytest.raises(PlumeIRError, match="No self-emission spectra"):
            build_self_background([])


class TestBackgroundQuery:

    def test_interpolates_onto_a_different_grid(self, patched_reader):
        y = np.linspace(1.0, 2.0, len(WN))
        patched_reader["self.spc"] = make_spectrum(y, name="self.spc")
        bkg = build_self_background([Path("self.spc")])

        coarse = np.linspace(2400.0, 2600.0, 11)
        out = bkg.at(T0, x=coarse)

        assert out.shape == coarse.shape
        np.testing.assert_allclose(out, np.interp(coarse, WN, y))

    def test_missing_timestamp_falls_back_to_mean(self, patched_reader, caplog):
        for i in range(2):
            patched_reader[f"self_{i}.spc"] = make_spectrum(
                np.full_like(WN, 2.0 + 2.0 * i),
                timestamp=T0 + timedelta(seconds=100 * i), name=f"self_{i}.spc",
            )
        bkg = build_self_background([Path(f"self_{i}.spc") for i in range(2)])

        with caplog.at_level("WARNING"):
            out = bkg.at(None)

        np.testing.assert_allclose(out, np.full_like(WN, 3.0))
        assert "no timestamp" in caplog.text

    def test_extrapolation_warns(self, patched_reader, caplog):
        for i in range(2):
            patched_reader[f"self_{i}.spc"] = make_spectrum(
                np.full_like(WN, 2.0 + i),
                timestamp=T0 + timedelta(seconds=100 * i), name=f"self_{i}.spc",
            )
        bkg = build_self_background([Path(f"self_{i}.spc") for i in range(2)])

        with caplog.at_level("WARNING"):
            bkg.at(T0 + timedelta(seconds=1000))

        assert "extrapolating" in caplog.text


# =============================================================================
# Negative-value handling
# =============================================================================

class TestClipNegatives:

    def test_cutoff_replaces_with_min_positive(self):
        y = np.array([-1.0, 0.0, 2.0, 5.0])
        np.testing.assert_allclose(
            clip_negatives(y, "cutoff"), [2.0, 2.0, 2.0, 5.0],
        )

    def test_cutoff_leaves_all_positive_untouched(self):
        y = np.array([1.0, 2.0, 3.0])
        np.testing.assert_allclose(clip_negatives(y, "cutoff"), y)

    def test_cutoff_all_negative_raises(self):
        with pytest.raises(PlumeIRError, match="no positive values"):
            clip_negatives(np.array([-1.0, -2.0]), "cutoff")

    def test_offset_shifts_whole_spectrum(self):
        y = np.array([-2.0, 0.0, 3.0])
        out = clip_negatives(y, "offset")
        assert out.min() > 0
        np.testing.assert_allclose(np.diff(out), np.diff(y))     # shape preserved

    def test_none_is_a_passthrough_copy(self):
        y = np.array([-1.0, 2.0])
        out = clip_negatives(y, "none")
        np.testing.assert_allclose(out, y)
        assert out is not y

    def test_unknown_mode_raises(self):
        with pytest.raises(PlumeIRError, match="Unknown negative-value mode"):
            clip_negatives(np.array([1.0]), "clamp")


# =============================================================================
# Subtraction
# =============================================================================

class TestSubtractSelfEmission:

    def test_subtracts_and_preserves_metadata(self, patched_reader):
        patched_reader["self.spc"] = make_spectrum(
            np.full_like(WN, 2.0), name="self.spc")
        bkg = build_self_background([Path("self.spc")])

        spec = make_spectrum(np.full_like(WN, 5.0))
        out = subtract_self_emission(spec, bkg)

        np.testing.assert_allclose(out.y, np.full_like(WN, 3.0))
        assert out.spec_type is SpectrumType.SBM      # still a single beam
        assert out.timestamp == spec.timestamp
        assert out.filename == spec.filename

    def test_is_pure(self, patched_reader):
        patched_reader["self.spc"] = make_spectrum(
            np.full_like(WN, 2.0), name="self.spc")
        bkg = build_self_background([Path("self.spc")])

        spec = make_spectrum(np.full_like(WN, 5.0))
        original = spec.y.copy()
        subtract_self_emission(spec, bkg)

        np.testing.assert_allclose(spec.y, original)

    def test_resamples_background_onto_spectrum_grid(self, patched_reader):
        """Self spectrum on a coarser grid than the scene spectrum."""
        coarse = np.linspace(2400.0, 2600.0, 16)
        patched_reader["self.spc"] = make_spectrum(
            np.full(16, 2.0), name="self.spc", x=coarse)
        bkg = build_self_background([Path("self.spc")])

        out = subtract_self_emission(make_spectrum(np.full_like(WN, 5.0)), bkg)

        assert out.y.shape == WN.shape
        np.testing.assert_allclose(out.y, np.full_like(WN, 3.0))

    def test_negatives_are_clipped(self, patched_reader):
        patched_reader["self.spc"] = make_spectrum(
            np.full_like(WN, 5.0), name="self.spc")
        bkg = build_self_background([Path("self.spc")])

        y = np.full_like(WN, 4.0)
        y[10] = 9.0                                   # only point above the background
        out = subtract_self_emission(make_spectrum(y), bkg)

        assert np.all(out.y > 0)
        np.testing.assert_allclose(out.y, np.full_like(WN, 4.0))


# =============================================================================
# Stage dispatch
# =============================================================================

def make_layer_geometry(**kwargs):
    return LayerGeometry(
        pathlength=1.22, atm_temp=284.0, atm_pres=973.0,
        plume_temp=450.0, plume_pres=973.0, **kwargs,
    )


class TestPreprocessLayer:

    def test_flag_off_is_a_no_op(self, tmp_path):
        spectra = [make_spectrum(np.full_like(WN, 5.0))]
        out = preprocess_layer(
            spectra, PreprocessConfig(subtract_self=False),
            DataConfig(data_dir=tmp_path),
        )
        assert out is spectra

    def test_missing_self_spectrum_raises(self, tmp_path):
        (tmp_path / "plume_0.spc").write_bytes(b"")
        with pytest.raises(PlumeIRError, match="no self-emission spectrum"):
            preprocess_layer(
                [make_spectrum(np.full_like(WN, 5.0))],
                PreprocessConfig(subtract_self=True),
                DataConfig(data_dir=tmp_path, filter_ext="spc"),
            )

    def test_end_to_end(self, tmp_path, patched_reader):
        (tmp_path / "self.spc").write_bytes(b"")
        patched_reader["self.spc"] = make_spectrum(
            np.full_like(WN, 2.0), name="self.spc")

        spectra = [make_spectrum(np.full_like(WN, 5.0 + i)) for i in range(3)]
        out = preprocess_layer(
            spectra, PreprocessConfig(subtract_self=True),
            DataConfig(data_dir=tmp_path, filter_ext="spc"),
        )

        assert len(out) == 3
        for i, spec in enumerate(out):
            np.testing.assert_allclose(spec.y, np.full_like(WN, 3.0 + i))

    def test_explicit_self_file_is_used(self, tmp_path, patched_reader):
        explicit = tmp_path / "background.spc"
        explicit.write_bytes(b"")
        patched_reader["background.spc"] = make_spectrum(
            np.full_like(WN, 1.0), name="background.spc")

        out = preprocess_layer(
            [make_spectrum(np.full_like(WN, 5.0))],
            PreprocessConfig(subtract_self=True, self_file=explicit),
            DataConfig(data_dir=tmp_path, filter_ext="spc"),
        )
        np.testing.assert_allclose(out[0].y, np.full_like(WN, 4.0))


class TestPreprocessDispatch:

    def test_layer_dispatches_to_layer_preprocessing(self, tmp_path, patched_reader):
        (tmp_path / "self.spc").write_bytes(b"")
        patched_reader["self.spc"] = make_spectrum(
            np.full_like(WN, 2.0), name="self.spc")

        out = preprocess_spectra(
            [make_spectrum(np.full_like(WN, 5.0))],
            make_layer_geometry(),
            DataConfig(data_dir=tmp_path, filter_ext="spc"),
            PreprocessConfig(subtract_self=True),
        )
        np.testing.assert_allclose(out[0].y, np.full_like(WN, 3.0))

    def test_emission_does_not_take_the_layer_path(self, tmp_path):
        """
        Emission dispatches to its own preprocessing even with subtract_self
        set: the flag is a transmission-geometry concept and must not pull an
        emission run into self-emission subtraction.  With no blackbody files
        present this surfaces as the emission error, not a layer one.

        Emission preprocessing proper is covered in
        ``test_preprocess_emission.py``.
        """
        geom = EmissionGeometry(
            plume_height=1.0, plume_thickness=0.1, elev=30.0, obs_height=0.1,
            atm_path=Path("/nonexistent/atm.atm"), fit_difference=False,
        )
        spectra = [make_spectrum(np.full_like(WN, 5.0))]

        with pytest.raises(PlumeIRError, match="No blackbody calibration"):
            preprocess_spectra(spectra, geom, DataConfig(data_dir=tmp_path),
                               PreprocessConfig(subtract_self=True))
