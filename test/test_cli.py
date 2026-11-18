"""
Tests for M2: the command line, the run folder, emission reference caching and
the config guards.

The CLI tests replace ``prepare_run`` / ``stream_run`` with synthetic stand-ins
(no RFM, no data); ``collect_run`` and everything the CLI writes are real.

Run with:  pytest test/test_cli.py -v
"""

from __future__ import annotations

import dataclasses
import logging
import pickle
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

import plumeIR.__main__ as cli
import plumeIR.reference.emission_reference as em_ref
from plumeIR.config.builder import build_from_raw
from plumeIR.config.objects import (
    DataConfig, EmissionGeometry, FitWindowConfig, GeometryType, LayerGeometry,
    ModelConfig, Retrieval, SolarGeometry,
)
from plumeIR.config.parser import read_config
from plumeIR.exceptions import ConfigError, FitError
from plumeIR.output import (
    RunInfo, make_run_dir, run_summary, write_results,
)
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.pipeline import RunSetup, stream_run, warn_experimental
from plumeIR.reference.reference import _reference_cache_key
from plumeIR.retrieval.results import FitResult
from plumeIR.retrieval.run import SpectrumFits, iter_retrieval
from plumeIR.spectra.spectrum import resolve_instrument_ils


# =============================================================================
# Helpers
# =============================================================================

_LAYER_INI = """
*DATA
data_dir : {data_dir}

*MODEL
*GEOMETRY
type        : layer
pathlength  : 1.0
atm_temp    : 280
atm_pres    : 1000
plume_temp  : 400
plume_pres  : 1000
{geom}

*RETRIEVAL
{retr}

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : {atm}
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 2
"""


def _layer_ini(tmp_path: Path, geom: str = "", retr: str = "",
               atm: str = "H2O") -> Path:
    path = tmp_path / "run.ini"
    path.write_text(_LAYER_INI.format(data_dir=tmp_path, geom=geom, retr=retr,
                                      atm=atm), encoding="utf-8")
    return path


def _build(tmp_path: Path, **kw) -> dict:
    path = _layer_ini(tmp_path, **kw)
    return build_from_raw(read_config(path), base_dir=tmp_path)


def _fit(label: str, sid: str, so2: float, success: bool = True) -> FitResult:
    wn = np.linspace(2400.0, 2550.0, 50)
    meas = 1.0 + 0.1 * np.sin(wn / 7.0)
    return FitResult(
        success=success, method="nlls", varied_names=("poly0", "SO2"),
        values={"poly0": 1.0, "SO2": so2}, perror={"poly0": 0.01, "SO2": 0.001},
        covariance=None, wn=wn, measured=meas, model=meas + 1e-3,
        rmse=1e-3, nfev=10, message="ok", window_label=label, spectrum_id=sid,
    )


def _fake_setup(n: int = 3) -> RunSetup:
    window = FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                             atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                             targets=("SO2",), polydeg=0)
    pset = ParameterSet([
        Parameter("poly0", 1.0, True, (-np.inf, np.inf), ParameterKind.POLYNOMIAL),
        Parameter("SO2", 0.01, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
    ])
    geometry = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                             plume_temp=450.0, plume_pres=1000.0)
    retrieval = Retrieval(geometry_type=GeometryType.LAYER, max_opd=1.037,
                          apod_type="triangle", ils_resolved=True)
    return RunSetup(data=DataConfig(data_dir=Path("/data")), model=ModelConfig(),
                    geometry=geometry, retrieval=retrieval,
                    windows={"FIT0": window}, params={"FIT0": pset},
                    references={"FIT0": None}, spectra=[f"S{i}" for i in range(n)])


def _items(setup: RunSetup, fail_index: int | None = None):
    n = setup.n_spectra
    for i in range(n):
        sid = f"2021-08-11T10:00:{i:02d}"
        yield SpectrumFits(i, n, f"S{i}",
                           (_fit("FIT0", sid, 0.01 * (i + 1), success=i != fail_index),))


@pytest.fixture
def patched_cli(monkeypatch):
    """CLI with the reference build, loading and fitting replaced."""
    setup = _fake_setup()
    seen: dict = {}

    def fake_prepare(cfg, *, recompute=False, cache_dir=None):
        seen["recompute"] = recompute
        seen["cfg"] = cfg
        return setup

    def fake_stream(s, *, parallel=False, n_workers=None, sigma=None):
        seen.update(parallel=parallel, n_workers=n_workers)
        return _items(s, fail_index=seen.get("fail_index"))

    monkeypatch.setattr(cli, "prepare_run", fake_prepare)
    monkeypatch.setattr(cli, "require_rfm", lambda: None)
    monkeypatch.setattr(cli, "stream_run", fake_stream)
    return seen


def _only_run_dir(parent: Path) -> Path:
    runs = [p for p in parent.iterdir() if p.is_dir()]
    assert len(runs) == 1, runs
    return runs[0]


# =============================================================================
# Command line
# =============================================================================

class TestCommandLine:

    def test_preprocess_only_fits_nothing(self, tmp_path, patched_cli, monkeypatch):
        import pandas as _pd
        from dataclasses import replace as _replace
        from plumeIR.preprocess.products import PreprocessReport
        reg = {k: [v, v] for k, v in dict(
            reg_eps=-1.5e-4, reg_eps_origin=-1.5e-4, reg_s0=0.0, reg_s0_err=0.01,
            reg_nu_mid=2500.0, reg_lag_mid=-0.375,
            reg_rms=0.03, reg_n_used=50, reg_n_bands=55).items()}
        report = PreprocessReport("off", "report", _pd.DataFrame(
            {"spectrum_id": ["a", "b"], **reg}))
        setup = _replace(_fake_setup(), preprocess_report=report)
        monkeypatch.setattr(cli, "prepare_run", lambda cfg, **kw: setup)
        monkeypatch.setattr(cli, "stream_run", lambda *a, **k: pytest.fail("fitted"))
        out = tmp_path / "runs"
        assert cli.main([str(_layer_ini(tmp_path)), "--preprocess-only",
                         "--output-dir", str(out)]) == 0
        run_dir = _only_run_dir(out)
        assert (run_dir / "preprocess.csv").is_file()
        assert not (run_dir / "results_long.csv").exists()
        summary = (run_dir / "summary.txt").read_text()
        assert "no fits" in summary and "preprocess      background off" in summary
        assert "registration    eps median -1.5000e-04" in summary

    def test_preprocess_only_refuses_plots(self, tmp_path, patched_cli):
        with pytest.raises(SystemExit):
            cli.main([str(_layer_ini(tmp_path)), "--preprocess-only", "--plot"])

    def test_run_writes_the_run_folder(self, tmp_path, patched_cli):
        cfg = _layer_ini(tmp_path)
        out = tmp_path / "runs"
        code = cli.main([str(cfg), "--parallel", "--workers", "3",
                         "--output-dir", str(out), "--recompute"])
        assert code == 0
        assert patched_cli["parallel"] is True and patched_cli["n_workers"] == 3
        assert patched_cli["recompute"] is True

        run_dir = _only_run_dir(out)
        assert run_dir.name.startswith("run_")
        for name in ("config.ini", "results_long.csv", "results_wide.csv",
                     "summary.txt", "run.log"):
            assert (run_dir / name).is_file(), name

        long = pd.read_csv(run_dir / "results_long.csv")
        assert set(long.parameter) == {"poly0", "SO2"}
        wide = pd.read_csv(run_dir / "results_wide.csv", index_col=0)
        assert list(wide.columns) == ["SO2"]
        np.testing.assert_allclose(wide["SO2"].to_numpy(), [1e15, 2e15, 3e15])

        # The written config is the config as run: it rebuilds to the same objects.
        rebuilt = build_from_raw(read_config(run_dir / "config.ini"), base_dir=run_dir)
        assert rebuilt["windows"] == patched_cli["cfg"]["windows"]
        assert rebuilt["geometry"] == patched_cli["cfg"]["geometry"]

        summary = (run_dir / "summary.txt").read_text()
        assert "spectra loaded  3" in summary
        assert "3/3 (100.0%)" in summary
        assert "max_opd 1.03700 cm" in summary

    def test_handlers_are_removed_after_the_run(self, tmp_path, patched_cli):
        def state():
            return {n: (list(logging.getLogger(n).handlers), logging.getLogger(n).level)
                    for n in cli._LOGGERS}
        before = state()
        cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(tmp_path / "r")])
        assert state() == before

    def test_failed_fit_is_counted(self, tmp_path, patched_cli):
        patched_cli["fail_index"] = 1
        assert cli.main([str(_layer_ini(tmp_path)),
                         "--output-dir", str(tmp_path / "r")]) == 0
        summary = (_only_run_dir(tmp_path / "r") / "summary.txt").read_text()
        assert "2/3 (66.7%)" in summary

    def test_interrupt_keeps_partial_results(self, tmp_path, monkeypatch, patched_cli):
        setup = _fake_setup()

        def interrupted(s, **kw):
            items = _items(s)
            yield next(items)
            raise KeyboardInterrupt

        monkeypatch.setattr(cli, "stream_run", interrupted)
        code = cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(tmp_path / "r")])
        assert code == 130
        run_dir = _only_run_dir(tmp_path / "r")
        long = pd.read_csv(run_dir / "results_long.csv")
        assert long.spectrum_id.nunique() == 1
        assert "stopped before the end" in (run_dir / "summary.txt").read_text()
        assert setup.n_spectra == 3

    def test_workers_requires_parallel(self, tmp_path):
        with pytest.raises(SystemExit) as exc:
            cli.main([str(_layer_ini(tmp_path)), "--workers", "2"])
        assert exc.value.code == 2

    def test_analyse_after_the_run_then_inspect(self, tmp_path, patched_cli, monkeypatch):
        import plumeIR.analysis.cli as acli
        order = []

        def fake_analysis(dirs, out_dir, settings=None, *, plots=True):
            order.append(("analyse", Path(dirs[0]), Path(out_dir)))
            Path(out_dir).mkdir(exist_ok=True)
            (Path(out_dir) / "analysis.txt").write_text("composition\n")
            return [Path(out_dir) / "analysis.txt"]

        monkeypatch.setattr(acli, "run_analysis", fake_analysis)
        monkeypatch.setattr(cli, "inspect", lambda d: order.append(("inspect", d)) or 0)
        out = tmp_path / "runs"
        assert cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(out),
                         "--analyse", "--inspect"]) == 0
        run_dir = _only_run_dir(out)
        assert order == [("analyse", run_dir, run_dir / "analysis"), ("inspect", run_dir)]
        # the folder, analysed as it is: the same call
        order.clear()
        assert cli.main([str(run_dir), "--analyze"]) == 0
        assert order == [("analyse", run_dir, run_dir / "analysis")]

    def test_failed_analysis_keeps_the_run(self, tmp_path, patched_cli):
        # the fake run has one target: a composition needs two species
        out = tmp_path / "runs"
        assert cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(out),
                         "--analyse"]) == 1
        run_dir = _only_run_dir(out)
        assert (run_dir / "results_long.csv").is_file()
        assert "Composition analysis failed" in (run_dir / "run.log").read_text()

    def test_stopped_run_is_not_analysed(self, tmp_path, monkeypatch, patched_cli):
        import plumeIR.analysis.cli as acli

        def interrupted(s, **kw):
            yield next(_items(s))
            raise KeyboardInterrupt

        monkeypatch.setattr(cli, "stream_run", interrupted)
        monkeypatch.setattr(acli, "run_analysis", lambda *a, **k: pytest.fail("analysed"))
        assert cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(tmp_path / "r"),
                         "--analyse"]) == 130

    def test_analyse_flag_guards(self, tmp_path, patched_cli):
        with pytest.raises(SystemExit):
            cli.main([str(_layer_ini(tmp_path)), "--preprocess-only", "--analyse"])
        with pytest.raises(SystemExit):
            cli.main([str(tmp_path), "--analyse", "--parallel"])
        with pytest.raises(SystemExit):
            cli.main([str(tmp_path)])

    def test_run_without_fits_loads_for_analysis(self, tmp_path, patched_cli):
        from plumeIR.exceptions import RunFolderError
        from plumeIR.output import load_run
        out = tmp_path / "runs"
        cli.main([str(_layer_ini(tmp_path)), "--output-dir", str(out)])
        run_dir = _only_run_dir(out)
        assert load_run(run_dir).fits is not None
        (run_dir / "fits.npz").unlink()         # a run written before 2026-11-12
        with pytest.raises(RunFolderError, match="fits.npz"):
            load_run(run_dir)
        run = load_run(run_dir, require_fits=False)
        assert run.fits is None and len(run.table.df)

    def test_config_error_creates_no_folder(self, tmp_path, patched_cli):
        cfg = _layer_ini(tmp_path, geom="no_gas : True")
        out = tmp_path / "runs"
        assert cli.main([str(cfg), "--output-dir", str(out)]) == 1
        assert not out.exists()

    def test_missing_config(self, tmp_path):
        assert cli.main([str(tmp_path / "absent.ini")]) == 1

    def test_update_params_with_parallel_is_refused_early(self, tmp_path, patched_cli):
        cfg = _layer_ini(tmp_path, retr="update_params : True")
        out = tmp_path / "runs"
        assert cli.main([str(cfg), "--parallel", "--output-dir", str(out)]) == 1
        assert not out.exists()
        assert "cfg" not in patched_cli          # stopped before prepare_run


# =============================================================================
# Run folder
# =============================================================================

class TestRunFolder:

    def test_make_run_dir_never_reuses_a_folder(self, tmp_path):
        when = datetime(2026, 11, 10, 12, 0, 0)
        a = make_run_dir(tmp_path, "cfg", when)
        b = make_run_dir(tmp_path, "cfg", when)
        assert a.name == "cfg_20261110-120000"
        assert b.name == "cfg_20261110-120000_2"

    def test_summary_per_window(self, tmp_path):
        from plumeIR.pipeline import collect_run
        setup = _fake_setup()
        table = collect_run(setup, list(_items(setup, fail_index=0)))
        long_csv, wide_csv = write_results(table, tmp_path)
        assert long_csv.is_file() and wide_csv.is_file()
        now = datetime(2026, 11, 10, 12, 0, 0)
        text = run_summary(setup, table, RunInfo(
            config_path=Path("/c.ini"), started=now, finished=now,
            parallel=False, n_workers=None))
        assert "FIT0" in text and "2400-2550" in text
        assert "2/3 (66.7%)" in text
        assert "serial" in text

    def test_summary_columns_never_touch(self):
        # 1020 spectra once printed "2450-25501020/1020 (100.0%)".
        from plumeIR.output import _table
        lines = _table(["window", "range [cm-1]", "converged", "R2 median"],
                       [["FIT0", "2450-2550", "1020/1020 (100.0%)", "0.99679"],
                        ["CO2_atm/H2O_atm", "20-21", "9/9 (100.0%)", "-"]])
        assert "2450-2550  1020/1020 (100.0%)" in lines[1]
        assert lines[2].startswith("CO2_atm/H2O_atm  ")
        # Right-aligned columns end at the same place on every line.
        assert len({len(line) for line in lines[:2]}) == 1


# =============================================================================
# Config guards (item 9)
# =============================================================================

class TestConfigGuards:

    def test_gas_in_both_roles_is_dual_in_layer(self, tmp_path):
        # The M2 guard, lifted by 14b: in the layer geometry a gas in both
        # roles becomes an ambient part {GAS}_atm and a plume part {GAS}.
        cfg = _build(tmp_path, atm="H2O, SO2")
        pset = cfg["params"][0]
        assert pset["SO2_atm"].kind is ParameterKind.GAS_ATM
        assert pset["SO2"].kind is ParameterKind.GAS_PLUME
        assert pset["SO2_atm"].ref_column > pset["SO2"].ref_column
        assert cfg["windows"][0].dual_gases == ("SO2",)

    def test_no_gas_is_refused(self, tmp_path):
        with pytest.raises(ConfigError, match="no_gas"):
            _build(tmp_path, geom="no_gas : True")

    def test_no_gas_false_is_fine(self, tmp_path):
        assert _build(tmp_path, geom="no_gas : False")["geometry"].no_gas is False

    def test_sigma_prior_reaches_parameters(self, tmp_path):
        cfg = _build(tmp_path, retr="sigma_prior_SO2 : 0.5\nsigma_prior_fov : 0.003")
        pset = cfg["params"][0]
        assert pset["SO2"].sigma_prior == 0.5
        assert pset["fov"].sigma_prior == 0.003
        assert pset["H2O"].sigma_prior is None

    def test_sigma_prior_unknown_name(self, tmp_path):
        with pytest.raises(ConfigError, match="no parameter"):
            _build(tmp_path, retr="sigma_prior_HCl : 0.5")

    def test_sigma_prior_must_be_positive(self, tmp_path):
        with pytest.raises(ConfigError, match="positive"):
            _build(tmp_path, retr="sigma_prior_SO2 : -1")


# =============================================================================
# Run-time warnings and update_params wiring
# =============================================================================

class TestRunTimeChecks:

    def test_oe_and_solar_warn(self, caplog):
        caplog.set_level(logging.WARNING, logger="plumeIR")
        solar = SolarGeometry(obs_height=0.5, atm_path=Path("/x.atm"))
        warn_experimental(solar, Retrieval(geometry_type=GeometryType.SOLAR,
                                           retrieval_method="oe"))
        text = caplog.text
        assert "Solar geometry is experimental" in text
        assert "Optimal estimation is experimental" in text

    def test_nlls_layer_does_not_warn(self, caplog):
        caplog.set_level(logging.WARNING, logger="plumeIR")
        layer = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                              plume_temp=450.0, plume_pres=1000.0)
        warn_experimental(layer, Retrieval(geometry_type=GeometryType.LAYER))
        assert caplog.text == ""

    def test_dual_gas_warns(self, caplog):
        caplog.set_level(logging.WARNING, logger="plumeIR")
        layer = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                              plume_temp=450.0, plume_pres=1000.0)
        window = FitWindowConfig(label="FIT3", wn_start=2020.0, wn_stop=2150.0,
                                 atm_gases=("H2O", "CO2"), plume_gases=("CO", "CO2"),
                                 plume_aero=(), targets=("CO",), polydeg=2)
        warn_experimental(layer, Retrieval(geometry_type=GeometryType.LAYER),
                          [window])
        assert "FIT3: ['CO2'] fitted as ambient and plume parts (CO2_atm + CO2)" \
            in caplog.text

    @pytest.mark.parametrize("fit, geom, gtype, warns", [
        # fits atm_temp in the window, targets H2O: the degenerate case
        ({"fit_atm_temp": True, "atm_gases": ["H2O", "CO2"], "targets": ["H2O"]},
         {}, "layer", True),
        # inherits fit_atm_temp from GEOMETRY
        ({"atm_gases": ["H2O"], "targets": ["H2O", "CO"]},
         {"fit_atm_temp": True}, "layer", True),
        # window override switches it off
        ({"fit_atm_temp": False, "atm_gases": ["H2O"], "targets": ["H2O"]},
         {"fit_atm_temp": True}, "layer", False),
        # H2O fitted but not a target (the validation FIT3 setup)
        ({"fit_atm_temp": True, "atm_gases": ["H2O", "CO2"], "targets": ["CO"]},
         {}, "layer", False),
        # dual H2O: the ambient part is the one that trades with atm_temp
        ({"fit_atm_temp": True, "atm_gases": ["H2O"], "plume_gases": ["H2O"],
          "targets": ["H2O_atm"]}, {}, "layer", True),
        # not the layer geometry
        ({"fit_atm_temp": True, "atm_gases": ["H2O"], "targets": ["H2O"]},
         {}, "emission", False),
    ])
    def test_atm_temp_with_h2o_target_warns(self, caplog, fit, geom, gtype, warns):
        from plumeIR.config.validator import _as_list, _warn_atm_temp_with_h2o
        caplog.set_level(logging.WARNING, logger="plumeIR")
        fit = {"_label": "FIT4", **fit}
        dual = sorted(set(_as_list(fit.get("atm_gases")))
                      & set(_as_list(fit.get("plume_gases"))))
        _warn_atm_temp_with_h2o(fit, geom, gtype, dual, fit["targets"])
        assert ("FIT4: fits atm_temp and targets" in caplog.text) is warns

    def test_oe_without_sigma_warns(self, caplog):
        caplog.set_level(logging.WARNING, logger="plumeIR")
        retr = Retrieval(geometry_type=GeometryType.LAYER, retrieval_method="oe")
        iter_retrieval([], {}, {}, {}, None, retr).close()
        assert "posterior errors are not calibrated" in caplog.text
        caplog.clear()
        iter_retrieval([], {}, {}, {}, None, retr, sigma=0.1).close()
        assert caplog.text == ""

    def test_stream_run_passes_update_params(self):
        setup = dataclasses.replace(
            _fake_setup(),
            retrieval=Retrieval(geometry_type=GeometryType.LAYER, update_params=True),
        )
        with pytest.raises(FitError, match="update_params requires serial"):
            stream_run(setup, parallel=True)

    def test_ils_resolution_is_idempotent(self, caplog):
        retr = Retrieval(geometry_type=GeometryType.LAYER)
        caplog.set_level(logging.INFO, logger="plumeIR")
        once = resolve_instrument_ils(retr, [])
        assert once.ils_resolved
        caplog.clear()
        assert resolve_instrument_ils(once, []) is once
        assert caplog.text == ""


# =============================================================================
# Reference cache keys and the emission cache (item 8)
# =============================================================================

def _key_inputs(**geom_kw):
    window = FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                             atm_gases=("H2O",), plume_gases=("SO2",),
                             plume_aero=(), targets=("SO2",), polydeg=0)
    geometry = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                             plume_temp=450.0, plume_pres=1000.0, **geom_kw)
    retrieval = Retrieval(geometry_type=GeometryType.LAYER)
    pset = ParameterSet([Parameter("SO2", 0.01, True, (0.0, np.inf),
                                   ParameterKind.GAS_PLUME, ref_column=1e17)])
    return window, geometry, retrieval, ModelConfig(), pset


class TestReferenceKey:

    def test_key_is_stable(self):
        assert _reference_cache_key(*_key_inputs()) == _reference_cache_key(*_key_inputs())

    def test_key_sees_plume_thickness(self):
        assert (_reference_cache_key(*_key_inputs(plume_thickness=0.05))
                != _reference_cache_key(*_key_inputs(plume_thickness=0.1)))

    def test_key_sees_gas_roles_and_continuum(self):
        w, g, r, m, p = _key_inputs()
        base = _reference_cache_key(w, g, r, m, p)
        swapped = dataclasses.replace(w, atm_gases=("SO2",), plume_gases=("H2O",))
        cont = dataclasses.replace(w, continuum_gases=("H2O",))
        assert _reference_cache_key(swapped, g, r, m, p) != base
        assert _reference_cache_key(cont, g, r, m, p) != base

    def test_key_sees_reference_columns(self):
        w, g, r, m, p = _key_inputs()
        p2 = ParameterSet([dataclasses.replace(p["SO2"], ref_column=2e17)])
        assert _reference_cache_key(w, g, r, m, p) != _reference_cache_key(w, g, r, m, p2)

    def test_key_sees_atmosphere_file_contents(self, tmp_path):
        atm = tmp_path / "x.atm"
        atm.write_text("one")
        w, _, r, m, p = _key_inputs()
        geom = EmissionGeometry(plume_height=1.0, plume_thickness=0.4, elev=25.0,
                                obs_height=0.9, atm_path=atm)
        k1 = _reference_cache_key(w, geom, r, m, p)
        atm.write_text("two")
        assert _reference_cache_key(w, geom, r, m, p) != k1


class TestEmissionAtmosphereCache:

    @pytest.fixture
    def counted(self, monkeypatch):
        calls = {"lev": 0}

        def fake_layers(atm, h_obs, h1, h2, elev, wn_start, wn_stop, nperwn,
                        gases, nodes, n_layers=2):
            calls["lev"] += 1
            wn = np.arange(wn_start, wn_stop, 1.0)
            return {"prox": "P", "dist": "D"}, np.ones_like(wn), wn

        monkeypatch.setattr(em_ref, "_build_atm_layers_lev", fake_layers)
        return calls

    def _call(self, atm_path, cache_dir, recompute=False):
        return em_ref._atm_layers_cached(
            {}, atm_path, 0.9, 0.8, 1.2, 25.0, 700.0, 710.0, 25.0, "H2O", [], 2,
            cache_dir=cache_dir, recompute=recompute,
        )

    def test_second_build_loads_from_cache(self, tmp_path, counted):
        atm = tmp_path / "x.atm"
        atm.write_text("profile")
        first = self._call(atm, tmp_path)
        second = self._call(atm, tmp_path)
        assert counted["lev"] == 1
        assert first[0] == second[0]
        np.testing.assert_array_equal(first[2], second[2])
        assert len(list(tmp_path.glob("emission_atm_*.pkl"))) == 1

    def test_recompute_and_edited_profile_rebuild(self, tmp_path, counted):
        atm = tmp_path / "x.atm"
        atm.write_text("profile")
        self._call(atm, tmp_path)
        self._call(atm, tmp_path, recompute=True)
        assert counted["lev"] == 2
        atm.write_text("edited profile")
        self._call(atm, tmp_path)
        assert counted["lev"] == 3
