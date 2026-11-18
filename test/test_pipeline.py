"""
Tests for the Phase 6 end-to-end pipeline glue.

Covers the wiring that ties config → references → spectra → run_retrieval,
without RFM/mieEODG or real data: the heavy steps are monkeypatched, so these
tests check the orchestration logic (dispatch, keying by window label, the
config→run handoff), not the physics (covered elsewhere).

Run with:  pytest test/test_pipeline.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

import plumeIR.pipeline as pipeline
import plumeIR.reference as reference
from plumeIR.config.builder import build_data_config
from plumeIR.exceptions import PlumeIRError
from plumeIR.config.objects import (
    DataConfig, EmissionGeometry, FitWindowConfig, GeometryType,
    LayerGeometry, ModelConfig, PreprocessConfig, Retrieval, SolarGeometry,
)
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.pipeline import run_from_config
from plumeIR.reference import build_reference_for


# =============================================================================
# build_data_config
# =============================================================================

def test_build_data_config_full():
    dc = build_data_config({
        "data_dir": "/tmp/x", "filter_files": True, "filter_string": "plume",
        "filter_ext": 0, "max_spec": 5,
    })
    assert dc.data_dir == Path("/tmp/x")
    assert dc.filter_files is True
    assert dc.filter_string == "plume"
    assert dc.filter_ext == "0"            # coerced to str
    assert dc.max_spec == 5


def test_build_data_config_minimal():
    dc = build_data_config({"data_dir": "/d"})
    assert dc.data_dir == Path("/d")
    assert dc.filter_files is False
    assert dc.filter_string is None
    assert dc.filter_ext is None
    assert dc.max_spec is None


# =============================================================================
# build_reference_for — dispatch on geometry type
# =============================================================================

def _layer():
    return LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                         plume_temp=450.0, plume_pres=1000.0, plume_thickness=0.05)


def test_build_reference_for_dispatch(monkeypatch):
    seen = {}
    monkeypatch.setattr(reference, "build_reference",
                        lambda *a, **k: seen.setdefault("calls", []).append("layer"))
    monkeypatch.setattr(reference, "build_emission_reference",
                        lambda *a, **k: seen.setdefault("calls", []).append("emission"))
    monkeypatch.setattr(reference, "build_solar_reference",
                        lambda *a, **k: seen.setdefault("calls", []).append("solar"))

    em = EmissionGeometry(plume_height=2.0, plume_thickness=1.0, elev=90.0,
                          obs_height=0.5, atm_path=Path("/x.atm"))
    so = SolarGeometry(obs_height=0.5, atm_path=Path("/x.atm"), elev=45.0)

    build_reference_for(_layer(), None, None, None, None)
    build_reference_for(em, None, None, None, None)
    build_reference_for(so, None, None, None, None)
    assert seen["calls"] == ["layer", "emission", "solar"]


def test_build_reference_for_unknown_geometry():
    with pytest.raises(ValueError, match="No reference builder"):
        build_reference_for(object(), None, None, None, None)


# =============================================================================
# run_from_config — wiring
# =============================================================================

def _fake_cfg():
    window = FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                             atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                             targets=("SO2",), polydeg=0)
    pset = ParameterSet([
        Parameter("SO2", 1.0, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
    ])
    return {
        "data":      DataConfig(data_dir=Path("/d")),
        "model":     ModelConfig(),
        "geometry":  _layer(),
        "preprocess": PreprocessConfig(),
        "retrieval": Retrieval(geometry_type=GeometryType.LAYER),
        "windows":   [window],
        "params":    [pset],
    }, window, pset


def _patch_heavy_steps(monkeypatch, cfg, spectra=("S1", "S2")):
    monkeypatch.setattr(pipeline, "build_from_config", lambda p: cfg)
    monkeypatch.setattr(pipeline, "build_reference_for",
                        lambda geom, win, *a, **k: f"REF::{win.label}")
    monkeypatch.setattr(pipeline, "load_spectra", lambda data: list(spectra))


def test_run_from_config_wiring(monkeypatch):
    cfg, window, pset = _fake_cfg()
    captured = {}
    _patch_heavy_steps(monkeypatch, cfg)

    def fake_iter(spectra, refs, params, windows, geom, retr, **kw):
        captured.update(spectra=spectra, refs=refs, params=params,
                        windows=windows, kw=kw)
        return iter([])

    def fake_collect(items, windows, params, geom):
        captured["items"] = list(items)
        return "RESULTS"

    monkeypatch.setattr(pipeline, "iter_retrieval", fake_iter)
    monkeypatch.setattr(pipeline, "collect_results", fake_collect)

    out = run_from_config("dummy.ini", parallel=True, n_workers=3)
    assert out == "RESULTS"
    # References / params / windows keyed by window label.
    assert captured["refs"] == {"FIT0": "REF::FIT0"}
    assert captured["windows"] == {"FIT0": window}
    assert captured["params"] == {"FIT0": pset}
    assert captured["spectra"] == ["S1", "S2"]
    # parallel options forwarded.
    assert captured["kw"]["parallel"] is True
    assert captured["kw"]["n_workers"] == 3


def test_prepare_run_accepts_built_config(monkeypatch):
    """A client holding a built config (build_from_raw) skips the file read."""
    cfg, window, pset = _fake_cfg()
    _patch_heavy_steps(monkeypatch, cfg)
    monkeypatch.setattr(pipeline, "build_from_config",
                        lambda p: pytest.fail("must not read a file"))
    setup = pipeline.prepare_run(cfg)
    assert setup.windows == {"FIT0": window}
    assert setup.params == {"FIT0": pset}
    assert setup.references == {"FIT0": "REF::FIT0"}
    assert setup.n_spectra == 2


def test_stream_run_and_collect_run(monkeypatch):
    """stream_run forwards the setup; collect_run takes a partial item list."""
    cfg, window, pset = _fake_cfg()
    _patch_heavy_steps(monkeypatch, cfg)
    setup = pipeline.prepare_run("dummy.ini")
    seen = {}

    def fake_iter(spectra, refs, params, windows, geom, retr, **kw):
        seen.update(spectra=spectra, refs=refs, kw=kw)
        return iter(["A", "B"])

    monkeypatch.setattr(pipeline, "iter_retrieval", fake_iter)
    monkeypatch.setattr(pipeline, "collect_results",
                        lambda items, w, p, g: (list(items), w, p, g))
    assert list(pipeline.stream_run(setup, sigma=0.1)) == ["A", "B"]
    assert seen["refs"] == {"FIT0": "REF::FIT0"} and seen["kw"]["sigma"] == 0.1
    items, w, p, g = pipeline.collect_run(setup, ["A"])
    assert items == ["A"] and w == setup.windows and g is setup.geometry


def test_run_from_config_no_spectra(monkeypatch):
    cfg, _, _ = _fake_cfg()
    _patch_heavy_steps(monkeypatch, cfg, spectra=())
    with pytest.raises(PlumeIRError, match="No spectra"):
        run_from_config("dummy.ini")


def test_no_spectra_fails_before_any_reference_is_built(monkeypatch):
    # Discovery runs first, so a wrong data_dir or filter fails in seconds
    # instead of after the reference build (minutes on a first run).
    cfg, _, _ = _fake_cfg()
    _patch_heavy_steps(monkeypatch, cfg, spectra=())

    def must_not_build(*a, **k):
        raise AssertionError("reference built before the spectra were checked")

    monkeypatch.setattr(pipeline, "build_reference_for", must_not_build)
    with pytest.raises(PlumeIRError, match="No spectra found in"):
        run_from_config("dummy.ini")
