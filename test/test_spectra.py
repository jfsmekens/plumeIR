"""
Tests for plumeIR.spectra — spectrum readers and file discovery.

Tests against real instrument files where available, plus synthetic
checks for the timestamp and dispatcher logic.

Run with:  pytest test/test_spectra.py -v
"""

from __future__ import annotations

import shutil
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from plumeIR.spectra.spectrum import (
    Spectrum,
    SpectrumType,
    _timestamp_from_name,
    _timestamp_from_bruker,
    read_bruker,
    read_midac,
    read_spectrum,
)
from plumeIR.spectra.discovery import (
    discover_spectra,
    sort_by_acquisition_time,
    detect_corrupted,
    rename_with_timestamp,
)
from plumeIR.exceptions import SpectrumIOError

# ---------------------------------------------------------------------------
# Real data paths — skipped if not present on this machine
# ---------------------------------------------------------------------------

from data_paths import (
    BRUKER_DIR     as _BRUKER_DIR_1,
    BRUKER_NUM_DIR as _BRUKER_DIR_2,
    MIDAC_DIR      as _MIDAC_DIR,
    BRUKER_PLUME   as _BRUKER_PLUME,
    BRUKER_BBC     as _BRUKER_BBC,
    BRUKER_BBH     as _BRUKER_BBH,
    BRUKER_NUM     as _BRUKER_NUM,   # numbered extension
    MIDAC_FILE     as _MIDAC_FILE,
)

def _brukeropusreader_available() -> bool:
    try:
        import brukeropusreader  # noqa: F401
        return True
    except ModuleNotFoundError:
        return False


# Bruker OPUS support is an optional extra ([bruker]); skip if absent.
_has_bruker  = pytest.mark.skipif(
    not (_BRUKER_PLUME.exists() and _brukeropusreader_available()),
    reason="Bruker test data or optional 'brukeropusreader' not available")
_has_bruker2 = pytest.mark.skipif(
    not (_BRUKER_NUM.exists() and _brukeropusreader_available()),
    reason="Bruker numbered-extension data or optional 'brukeropusreader' not available")

# MIDAC .spc support is built in (plumeIR.spectra.spc); gate on the data only.
_has_midac   = pytest.mark.skipif(not _MIDAC_FILE.exists(),
                                   reason="MIDAC test data not available")


# =============================================================================
# Timestamp helpers
# =============================================================================

class TestTimestampFromName:

    def test_bruker_style_name(self):
        p = Path("plume2019_09_11_1829_53_253000.0")
        dt = _timestamp_from_name(p)
        assert dt is not None
        assert dt.year == 2019
        assert dt.month == 9
        assert dt.day == 11
        assert dt.hour == 18
        assert dt.minute == 29

    def test_midac_style_name(self):
        p = Path("2021_08_11_0036_01_544_sbm.spc")
        dt = _timestamp_from_name(p)
        assert dt is not None
        assert dt.year == 2021
        assert dt.month == 8
        assert dt.day == 11
        assert dt.hour == 0
        assert dt.minute == 36

    def test_no_digits_returns_none(self):
        assert _timestamp_from_name(Path("plume.0")) is None

    def test_too_few_digits_returns_none(self):
        assert _timestamp_from_name(Path("data_123.0")) is None


# =============================================================================
# Bruker reader
# =============================================================================

class TestReadBruker:

    @_has_bruker
    def test_returns_spectrum(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert isinstance(spec, Spectrum)

    @_has_bruker
    def test_x_ascending(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert np.all(np.diff(spec.x) > 0)

    @_has_bruker
    def test_x_unit_wn(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert spec.x_unit == "wn"

    @_has_bruker
    def test_x_y_same_length(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert len(spec.x) == len(spec.y)

    @_has_bruker
    def test_wavenumber_range(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert spec.x.min() > 400
        assert spec.x.max() < 5000

    @_has_bruker
    def test_spec_type_sbm(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert spec.spec_type == SpectrumType.SBM

    @_has_bruker
    def test_timestamp_extracted(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert spec.timestamp is not None
        assert isinstance(spec.timestamp, datetime)
        assert spec.timestamp.year == 2019

    @_has_bruker
    def test_metadata_has_optik(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert "Optik" in spec.metadata

    @_has_bruker
    def test_not_blackbody(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert spec.is_blackbody is False
        assert spec.bb_temp is None

    @_has_bruker
    def test_bbc_is_blackbody(self):
        spec = read_bruker(_BRUKER_BBC)
        assert spec.is_blackbody is True

    @_has_bruker
    def test_bbh_is_blackbody(self):
        spec = read_bruker(_BRUKER_BBH)
        assert spec.is_blackbody is True

    @_has_bruker2
    def test_numbered_extension(self):
        spec = read_bruker(_BRUKER_NUM)
        assert isinstance(spec, Spectrum)
        assert np.all(np.diff(spec.x) > 0)

    def test_missing_file_raises(self):
        with pytest.raises(SpectrumIOError):
            read_bruker(Path("/nonexistent/file.0"))

    @_has_bruker
    def test_wn_property(self):
        spec = read_bruker(_BRUKER_PLUME)
        assert np.allclose(spec.wn, spec.x)  # already in wn


# =============================================================================
# MIDAC reader
# =============================================================================

class TestReadMidac:

    @_has_midac
    def test_returns_spectrum(self):
        spec = read_midac(_MIDAC_FILE)
        assert isinstance(spec, Spectrum)

    @_has_midac
    def test_x_ascending(self):
        spec = read_midac(_MIDAC_FILE)
        assert np.all(np.diff(spec.x) > 0)

    @_has_midac
    def test_x_unit_wn(self):
        spec = read_midac(_MIDAC_FILE)
        assert spec.x_unit == "wn"

    @_has_midac
    def test_x_y_same_length(self):
        spec = read_midac(_MIDAC_FILE)
        assert len(spec.x) == len(spec.y)

    @_has_midac
    def test_spec_type_sbm(self):
        spec = read_midac(_MIDAC_FILE)
        assert spec.spec_type == SpectrumType.SBM

    @_has_midac
    def test_timestamp_from_filename(self):
        spec = read_midac(_MIDAC_FILE)
        assert spec.timestamp is not None
        assert spec.timestamp.year == 2021
        assert spec.timestamp.month == 8
        assert spec.timestamp.day == 11

    @_has_midac
    def test_not_blackbody(self):
        spec = read_midac(_MIDAC_FILE)
        assert spec.is_blackbody is False

    def test_missing_file_raises(self):
        with pytest.raises(SpectrumIOError):
            read_midac(Path("/nonexistent/file.spc"))


# =============================================================================
# Dispatcher
# =============================================================================

class TestReadSpectrum:

    @_has_bruker
    def test_dispatches_bruker(self):
        spec = read_spectrum(_BRUKER_PLUME)
        assert isinstance(spec, Spectrum)

    @_has_bruker2
    def test_dispatches_bruker_numbered(self):
        spec = read_spectrum(_BRUKER_NUM)
        assert isinstance(spec, Spectrum)

    @_has_midac
    def test_dispatches_midac(self):
        spec = read_spectrum(_MIDAC_FILE)
        assert isinstance(spec, Spectrum)

    def test_unknown_extension_raises(self):
        with pytest.raises(SpectrumIOError, match="Unrecognised"):
            read_spectrum(Path("file.xyz"))


# =============================================================================
# Spectrum methods
# =============================================================================

class TestSpectrumMethods:

    def _make(self) -> Spectrum:
        x = np.linspace(600, 2000, 500)
        y = np.random.default_rng(42).random(500)
        return Spectrum(x=x, y=y, x_unit="wn",
                        timestamp=datetime(2024, 1, 1),
                        filename=Path("test.0"),
                        spec_type=SpectrumType.SBM)

    def test_ascending_on_init(self):
        x = np.linspace(2000, 600, 500)   # descending
        y = np.ones(500)
        spec = Spectrum(x=x, y=y, x_unit="wn",
                        timestamp=None, filename=Path("test.0"),
                        spec_type=SpectrumType.SBM)
        assert spec.x[0] < spec.x[-1]

    def test_npts(self):
        assert self._make().npts == 500

    def test_trim(self):
        spec   = self._make()
        trimmed = spec.trim(800, 1200)
        assert trimmed.x.min() >= 800
        assert trimmed.x.max() <= 1200
        assert len(trimmed.x) == len(trimmed.y)
        assert len(trimmed.x) < len(spec.x)

    def test_trim_preserves_metadata(self):
        spec    = self._make()
        trimmed = spec.trim(800, 1200)
        assert trimmed.spec_type == spec.spec_type
        assert trimmed.timestamp == spec.timestamp

    def test_resample(self):
        spec      = self._make()
        new_grid  = np.linspace(700, 1900, 100)
        resampled = spec.resample(new_grid)
        assert len(resampled.x) == 100
        assert np.allclose(resampled.x, new_grid)

    def test_wn_property_passthrough(self):
        spec = self._make()
        assert np.allclose(spec.wn, spec.x)

    def test_wn_property_converts_wavelength(self):
        x_wl = np.array([2.0, 5.0, 10.0])  # µm
        spec = Spectrum(x=x_wl, y=np.ones(3), x_unit="wl",
                        timestamp=None, filename=Path("test.0"),
                        spec_type=SpectrumType.SBM)
        wn = spec.wn
        # __post_init__ keeps x_wl ascending [2, 5, 10]; wn = 1e4/x = [5000, 2000, 1000]
        assert np.allclose(wn, 1e4 / x_wl)


# =============================================================================
# Discovery
# =============================================================================

class TestDiscoverSpectra:

    @pytest.mark.skipif(not _BRUKER_DIR_1.exists(), reason="Bruker data not available")
    def test_finds_bruker_files(self):
        paths = discover_spectra(_BRUKER_DIR_1)
        exts  = {p.suffix.lstrip(".") for p in paths}
        assert all(e.isdigit() for e in exts)
        assert len(paths) > 0

    @pytest.mark.skipif(not _BRUKER_DIR_1.exists(), reason="Bruker data not available")
    def test_filter_string(self):
        paths = discover_spectra(_BRUKER_DIR_1, filter_string="plume")
        assert all("plume" in p.stem for p in paths)

    @pytest.mark.skipif(not _BRUKER_DIR_1.exists(), reason="Bruker data not available")
    def test_filter_string_excludes_bb(self):
        paths = discover_spectra(_BRUKER_DIR_1, filter_string="plume")
        names = [p.stem for p in paths]
        assert not any("BBC" in n or "BBH" in n or "clear" in n for n in names)

    @pytest.mark.skipif(not _MIDAC_DIR.exists(), reason="MIDAC data not available")
    def test_finds_midac_files(self):
        paths = discover_spectra(_MIDAC_DIR, filter_ext="spc")
        assert all(p.suffix == ".spc" for p in paths)
        assert len(paths) > 0

    def test_missing_directory_raises(self):
        with pytest.raises(NotADirectoryError):
            discover_spectra(Path("/nonexistent/dir"))

    def test_max_spec(self, tmp_path):
        # Create dummy Bruker-like files
        for i in range(10):
            (tmp_path / f"spec_{i:04d}.0").write_bytes(b"")
        # max_spec limits are applied before sorting so we just check count
        paths = discover_spectra(tmp_path, max_spec=3)
        assert len(paths) <= 3


class TestExplainNoSpectra:
    """Why discovery found nothing, named by the config key to change."""

    @staticmethod
    def _data(path, filter_files=False, filter_string=None, filter_ext=None):
        from types import SimpleNamespace
        return SimpleNamespace(data_dir=path, filter_files=filter_files,
                               filter_string=filter_string, filter_ext=filter_ext)

    @staticmethod
    def _touch(folder, *names):
        for name in names:
            (folder / name).write_bytes(b"")

    def test_missing_folder(self, tmp_path):
        from plumeIR.spectra import explain_no_spectra
        msg = explain_no_spectra(self._data(tmp_path / "nope"))
        assert "does not exist" in msg and "DATA.data_dir" in msg

    def test_empty_folder(self, tmp_path):
        from plumeIR.spectra import explain_no_spectra
        assert "holds no files" in explain_no_spectra(self._data(tmp_path))

    def test_wrong_extension_lists_what_is_there(self, tmp_path):
        from plumeIR.spectra import explain_no_spectra
        self._touch(tmp_path, "a_sbm.spc", "b_sbm.spc", "notes.txt")
        msg = explain_no_spectra(self._data(tmp_path, filter_ext="SPC"))
        assert "none with extension '.SPC'" in msg
        assert ".spc (2)" in msg and ".txt (1)" in msg

    def test_name_filter_excludes_everything(self, tmp_path):
        # The case found on 2026-11-13: the example's 'vent' filter on a
        # folder of 2021_08_11_..._sbm.spc files.
        from plumeIR.spectra import explain_no_spectra
        self._touch(tmp_path, "2021_08_11_0036_01_544_sbm.spc",
                    "2021_08_11_0036_03_806_sbm.spc", "self.spc")
        msg = explain_no_spectra(self._data(
            tmp_path, filter_files=True, filter_string="vent", filter_ext="spc"))
        assert "3 file(s) with extension '.spc'" in msg
        assert "contains 'vent'" in msg and "filter_files: False" in msg

    def test_only_reference_files(self, tmp_path):
        from plumeIR.spectra import explain_no_spectra
        self._touch(tmp_path, "self_sbm.spc")
        msg = explain_no_spectra(self._data(
            tmp_path, filter_files=True, filter_string="sbm", filter_ext="spc"))
        assert "reference measurements" in msg and "'self'" in msg

    def test_files_found_but_unreadable(self, tmp_path):
        from plumeIR.spectra import explain_no_spectra
        self._touch(tmp_path, "vent_sbm.spc")
        msg = explain_no_spectra(self._data(tmp_path, filter_ext="spc"))
        assert "1 file(s) matched, but none could be read" in msg

    def test_agrees_with_discovery(self, tmp_path):
        # Whenever discovery finds files, the explanation must not claim a
        # rule removed them: it falls through to "none could be read".
        from plumeIR.spectra import explain_no_spectra
        self._touch(tmp_path, "x_vent.spc", "self.spc")
        data = self._data(tmp_path, filter_files=True, filter_string="vent",
                          filter_ext="spc")
        assert discover_spectra(tmp_path, filter_string="vent", filter_ext="spc")
        assert "none could be read" in explain_no_spectra(data)


class TestSortByAcquisitionTime:

    @pytest.mark.skipif(not _BRUKER_DIR_1.exists(), reason="Bruker data not available")
    def test_sorted_ascending(self):
        paths  = discover_spectra(_BRUKER_DIR_1, filter_string="plume")
        sorted_paths = sort_by_acquisition_time(paths[:10])
        # Read timestamps and verify order
        from plumeIR.spectra.spectrum import read_spectrum
        times = [read_spectrum(p).timestamp for p in sorted_paths if
                 read_spectrum(p).timestamp is not None]
        assert times == sorted(times)


# =============================================================================
# Instrument line shape read from the data file
# =============================================================================

class TestInstrumentILS:
    """
    ``max_opd`` and ``apod_type`` belong to the interferometer and must come
    from the file, not from a config.

    Both instruments here had a hand-entered ILS that was wrong.  ``max_opd``
    was set to ``1/RES`` — which is the TOTAL double-sided path ``2L``, not the
    maximum OPD — giving 2.0 cm (Bruker, RES 0.5) and 1.8 cm (MIDAC) against a
    true 1.037 cm.  MIDAC's apodization was set to "NB medium" while its files
    say "triangle".  A fitted ``fov`` absorbed the error, running to 85 mrad
    against a 30 mrad nominal (Bruker) and 52 mrad against 21 (MIDAC).

    Correcting both on the MIDAC brought ``fov`` back to 15.9 / 28.2 mrad
    against the 21 mrad nominal, and moved HCl by 10-12%.

    (2026-11-21) Halving the record holds for the double-sided Bruker only.
    The MIDAC interferograms are single-sided (``IGRAMSIDE Single``), with
    ``ZPD - SKIP POINTS`` = 256 points before ZPD, so their max OPD is
    (32768 - 256) / LWN = 2.058 cm.
    """

    @_has_bruker
    def test_bruker_derives_max_opd(self):
        from plumeIR.spectra.spectrum import read_bruker
        ils = read_bruker(_BRUKER_PLUME).ils
        assert ils is not None
        # 2L = 2.074 cm  ->  max_opd = 1.037 cm, NOT the 1/RES = 2.0 configured
        assert ils.max_opd == pytest.approx(1.037, abs=0.005)
        assert ils.zff == 2
        assert ils.lwn == pytest.approx(15799.575, abs=0.01)

    @_has_bruker
    def test_bruker_maps_apodization_code(self):
        from plumeIR.spectra.spectrum import read_bruker
        # OPUS APF 'NBM' -> the vocabulary forward.ils substring-matches
        assert read_bruker(_BRUKER_PLUME).ils.apod_type == "NB medium"

    @_has_midac
    def test_midac_derives_max_opd_and_apodization(self):
        from plumeIR.spectra.spectrum import read_midac
        ils = read_midac(_MIDAC_FILE).ils
        assert ils is not None
        assert ils.single_sided is True and ils.n_pre_zpd == 256
        assert ils.max_opd == pytest.approx(2.058, abs=0.001)
        assert ils.apod_type == "triangle"      # config said "NB medium"
        assert ils.n_points == 32768            # 2**15, the transform length
        assert ils.zff == 2

    @_has_bruker
    @_has_midac
    def test_same_record_length_different_sidedness(self):
        """Same 32768-point record at a ~15798 HeNe; Bruker double-sided, MIDAC not."""
        from plumeIR.spectra.spectrum import read_bruker, read_midac
        a, b = read_bruker(_BRUKER_PLUME).ils, read_midac(_MIDAC_FILE).ils
        assert a.single_sided is False and b.single_sided is True
        assert 2 * a.max_opd == pytest.approx(b.max_opd * 32768 / (32768 - 256),
                                              rel=1e-3)

    @_has_midac
    def test_derivation_agrees_with_points_over_lwn(self):
        """
        The two independent routes to the record length must agree:
        ``1/(dnu*ZFF)`` from the stored grid, and ``POINTS/LWN``.
        """
        from plumeIR.spectra.spectrum import read_midac
        ils = read_midac(_MIDAC_FILE).ils
        assert (ils.n_points - ils.n_pre_zpd) / ils.lwn == pytest.approx(ils.max_opd,
                                                                          rel=1e-3)

    def test_single_sided_derivation(self, caplog):
        from plumeIR.spectra.spectrum import _derive_max_opd
        x = 500.0 + 15798.0 / 65536 * np.arange(1000)     # 32768 points, ZFF 2
        two_l = _derive_max_opd(x, 2, n_points=32768, lwn=15798.0)
        assert two_l == pytest.approx(32768 / 15798.0 / 2)
        one = _derive_max_opd(x, 2, n_points=32768, lwn=15798.0,
                              single_sided=True, n_pre_zpd=256)
        assert one == pytest.approx((32768 - 256) / 15798.0)
        with caplog.at_level("WARNING"):
            assert _derive_max_opd(x, 2, n_points=32768, lwn=15798.0,
                                   single_sided=True, n_pre_zpd=None) is None
        assert "before ZPD" in caplog.text

    @_has_bruker
    def test_single_sided_bruker_gives_no_max_opd(self, caplog):
        """OPUS AQM S*: the pre-ZPD length is not read, so no value is guessed."""
        from plumeIR.spectra import spectrum as sp
        s = sp.read_bruker(_BRUKER_PLUME)
        assert s.metadata["Acquisition"]["AQM"] == "DD"       # the real file
        assert sp._bruker_ils(s.metadata, s.x, "f").single_sided is False
        meta = {**s.metadata, "Acquisition": {**s.metadata["Acquisition"], "AQM": "SN"}}
        with caplog.at_level("WARNING"):
            ils = sp._bruker_ils(meta, s.x, "f")
        assert ils.single_sided is True and ils.max_opd is None
        assert "single-sided Bruker" in caplog.text

    def test_derive_returns_none_without_zff(self):
        from plumeIR.spectra.spectrum import _derive_max_opd
        assert _derive_max_opd(np.linspace(600, 2000, 100), None) is None

    @_has_bruker
    def test_ils_survives_trim_and_resample(self):
        """Preprocessing and trimming must not drop the instrument metadata."""
        from plumeIR.spectra.spectrum import read_bruker
        s = read_bruker(_BRUKER_PLUME)
        assert s.trim(800, 1200).ils is s.ils
        assert s.resample(np.linspace(800, 1200, 500)).ils is s.ils


class TestResolveInstrumentILS:
    """Priority: explicit config > value from the file > warned fallback."""

    def _retrieval(self, **kw):
        from plumeIR.config.objects import Retrieval, GeometryType
        return Retrieval(geometry_type=GeometryType.EMISSION, **kw)

    @_has_bruker
    def test_file_wins_when_config_is_silent(self):
        from plumeIR.spectra.spectrum import read_bruker, resolve_instrument_ils
        s = read_bruker(_BRUKER_PLUME)
        out = resolve_instrument_ils(self._retrieval(max_opd=2.0), [s])
        assert out.max_opd == pytest.approx(1.037, abs=0.005)

    @_has_bruker
    def test_explicit_config_overrides_the_file(self):
        from plumeIR.spectra.spectrum import read_bruker, resolve_instrument_ils
        s = read_bruker(_BRUKER_PLUME)
        out = resolve_instrument_ils(
            self._retrieval(max_opd=2.0, max_opd_explicit=True), [s])
        assert out.max_opd == 2.0

    @_has_midac
    def test_apod_type_from_file(self):
        from plumeIR.spectra.spectrum import read_midac, resolve_instrument_ils
        s = read_midac(_MIDAC_FILE)
        out = resolve_instrument_ils(self._retrieval(apod_type="NB medium"), [s])
        assert out.apod_type == "triangle"

    @_has_bruker
    def test_fallback_warns_when_nothing_is_available(self, caplog):
        import dataclasses
        from plumeIR.spectra.spectrum import (read_bruker, resolve_instrument_ils,
                                         FALLBACK_MAX_OPD_CM)
        s = dataclasses.replace(read_bruker(_BRUKER_PLUME), ils=None)
        with caplog.at_level("WARNING"):
            out = resolve_instrument_ils(self._retrieval(), [s])
        assert out.max_opd == FALLBACK_MAX_OPD_CM
        assert any("max_opd not available" in r.message for r in caplog.records)

    @_has_bruker
    def test_disagreeing_spectra_warn(self, caplog):
        import dataclasses
        from plumeIR.spectra.spectrum import read_bruker, resolve_instrument_ils
        a = read_bruker(_BRUKER_PLUME)
        b = dataclasses.replace(
            a, ils=dataclasses.replace(a.ils, max_opd=a.ils.max_opd * 2))
        with caplog.at_level("WARNING"):
            resolve_instrument_ils(self._retrieval(), [a, b])
        assert any("disagree on max_opd" in r.message for r in caplog.records)
