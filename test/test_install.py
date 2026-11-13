"""
Tests for the installation helpers: the RFM start-up check
(plumeIR.rfm_setup) and the package versions in the run record
(plumeIR.provenance).
"""

from __future__ import annotations

import logging
from pathlib import Path

import pytest

import plumeIR.__main__ as cli
from plumeIR.provenance import RECORDED, describe, package_versions
from plumeIR.rfm_setup import (
    RFM_CITATION, RFM_URL, RFMNotFoundError, check_rfm, require_rfm,
)

from test_cli import _layer_ini, patched_cli  # noqa: F401  (fixture)


def _rfm_tree(root: Path, *, exe=True, executable=True, hitran=True) -> Path:
    (root / "source").mkdir(parents=True)
    (root / "Hitran").mkdir()
    if exe:
        f = root / "source" / "rfm"
        f.write_text("#!/bin/sh\n")
        f.chmod(0o755 if executable else 0o644)
    if hitran:
        (root / "Hitran" / "hitran_2020.bin").write_bytes(b"\0")
    return root


@pytest.fixture
def rfm_env(monkeypatch, tmp_path):
    """Point rfmwrapper at a folder under tmp_path, ignoring the user's config."""
    def point(root: Path, hitran: Path | None = None):
        monkeypatch.setenv("RFM_DIR", str(root))
        if hitran is None:
            monkeypatch.delenv("HITRAN_PATH", raising=False)
            monkeypatch.setattr("rfmwrapper.config._cfg_get", lambda key: None)
        else:
            monkeypatch.setenv("HITRAN_PATH", str(hitran))
    return point


class TestRFMCheck:

    def test_complete_tree(self, tmp_path, rfm_env):
        root = _rfm_tree(tmp_path / "RFM")
        rfm_env(root)
        status = require_rfm()
        assert status.ok and status.executable == root / "source" / "rfm"
        assert status.hitran == root / "Hitran" / "hitran_2020.bin"

    @pytest.mark.parametrize("kw, match", [
        ({"exe": False}, "executable not found"),
        ({"executable": False}, "not executable"),
        ({"hitran": False}, "No HITRAN binary"),
    ])
    def test_missing_part(self, tmp_path, rfm_env, kw, match):
        rfm_env(_rfm_tree(tmp_path / "RFM", **kw))
        status = check_rfm()
        assert not status.ok and any(match in p for p in status.problems)

    def test_missing_directory_gives_instructions(self, tmp_path, rfm_env):
        rfm_env(tmp_path / "nowhere")
        with pytest.raises(RFMNotFoundError) as err:
            require_rfm()
        text = str(err.value)
        assert "RFM directory not found" in text
        assert RFM_URL in text and RFM_CITATION in text
        assert "python -m rfmwrapper configure" in text

    def test_hitran_path_override(self, tmp_path, rfm_env):
        root = _rfm_tree(tmp_path / "RFM", hitran=False)
        rfm_env(root, hitran=tmp_path / "missing.bin")
        assert any("HITRAN binary not found" in p for p in check_rfm().problems)

    def test_cli_fails_before_creating_a_run_folder(self, tmp_path, rfm_env, caplog):
        rfm_env(tmp_path / "nowhere")
        out = tmp_path / "runs"
        with caplog.at_level(logging.ERROR, logger="plumeIR"):
            assert cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(out)]) == 1
        assert not out.exists() or not any(out.iterdir())
        assert "RFMNotFoundError" in caplog.text and RFM_URL in caplog.text


class TestProvenance:

    def test_not_installed(self):
        assert describe("no-such-distribution-xyz") == "not installed"

    def test_every_recorded_package(self):
        versions = package_versions()
        assert tuple(versions) == RECORDED
        assert all(v and v != "not installed" for v in versions.values())

    def test_summary_lists_them(self, tmp_path, patched_cli):
        out = tmp_path / "runs"
        assert cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(out)]) == 0
        summary = next(out.iterdir()).joinpath("summary.txt").read_text()
        for name in RECORDED:
            assert any(line.startswith(name + " ") for line in summary.splitlines())


EXAMPLES = Path(__file__).parent.parent / "examples"


class TestExamples:
    """The shipped configs (README "Quick start") build; their data may be absent."""

    @pytest.mark.parametrize("name, geometry", [("layer", "layer"),
                                                ("emission", "emission")])
    def test_builds(self, name, geometry):
        from plumeIR.config.builder import build_from_raw
        from plumeIR.config.parser import read_config
        from plumeIR.config.paths import resolve_config_paths

        path = EXAMPLES / f"{name}.ini"
        raw = resolve_config_paths(read_config(path), path.parent)
        if not Path(raw["DATA"]["data_dir"]).is_dir():
            pytest.skip(f"example data for {name} not present")
        cfg = build_from_raw(raw, base_dir=path.parent)
        assert cfg["retrieval"].geometry_type.name.lower() == geometry
        assert cfg["plot"].ratios
        assert cfg["model"].cache_dir is None          # the default cache

    def test_layer_example_is_the_guide_setup(self):
        # Five windows, polydeg 2 (v2's "3"), temperatures handed on in order.
        from plumeIR.config.builder import build_from_raw
        from plumeIR.config.parser import read_config
        from plumeIR.config.paths import resolve_config_paths

        path = EXAMPLES / "layer.ini"
        raw = resolve_config_paths(read_config(path), path.parent)
        raw["DATA"]["data_dir"] = str(path.parent)     # build without the data
        windows = build_from_raw(raw, base_dir=path.parent)["windows"]
        assert [w.label for w in windows] == ["FIT0", "FIT1", "FIT2", "FIT3", "FIT4"]
        assert all(w.polydeg == 2 for w in windows)
        assert [w.plume_temp_from for w in windows] == [None, "FIT0", "FIT0", "FIT0", "FIT0"]
        assert [w.atm_temp_from for w in windows] == [None, None, "FIT1", "FIT1", "FIT1"]

    def test_no_machine_paths(self):
        from plumeIR.config.parser import read_config
        from plumeIR.config.paths import PATH_KEYS

        for path in EXAMPLES.glob("*.ini"):
            raw = read_config(path)
            for section, key in PATH_KEYS:
                value = (raw.get(section) or {}).get(key)
                assert value is None or not Path(str(value)).is_absolute(), \
                    f"{path.name}: {section}.{key} is an absolute path"
