"""
Tests for the path-length diagnostic (``GEOMETRY.path_tracer``).

Validated first on the layer validation set (2026-11-15): CH4 in a 2880-2990 cm⁻¹ window implies a 2.003 km
path (2.0% per spectrum) against the configured 1.22 km, N2O 2.01 km, and
rebuilding the ambient tables at 2.0 km leaves every plume column identical.
These tests codify the arithmetic, the window choice, the report and the
config rules on synthetic results tables.

Run with:  pytest test/test_path_tracer.py -v
"""

from __future__ import annotations

import logging
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from plumeIR.config.objects import LayerGeometry
from plumeIR.exceptions import ConfigError
from plumeIR.path_tracer import COL_TRACER_PATH, tracer_report
from plumeIR.retrieval.results import ResultsTable
from plumeIR.units import scd_to_vmr, vmr_to_scd

T, P, L_CFG, L_TRUE, CH4 = 284.0, 973.0, 1.22, 2.0, 1.9


def _geometry(tracer: str | None = "CH4", pathlength: float = L_CFG) -> LayerGeometry:
    return LayerGeometry(pathlength=pathlength, atm_temp=T, atm_pres=P,
                         plume_temp=450.0, plume_pres=P, path_tracer=tracer)


def _row(sid, window, param, kind, scd, rel_err, success=True):
    return dict(spectrum_id=sid, window=window, parameter=param, species=param,
                kind=kind, varied=True, value=1.0, error=0.1, scd=scd,
                scd_error=scd * rel_err, pres=P, temp=T, is_target=False,
                success=success, rmse=0.0, r2=1.0)


def _setup(n: int = 20, seed: int = 0):
    """
    Two windows with ambient CH4 over a true 2.0 km path: W5 precise (1%),
    W1 noisy (5%); plume HCl in W5 correlated with nothing; ambient H2O at
    10000 ppmv.  Returns (table, windows, params).
    """
    rng = np.random.default_rng(seed)
    ch4_true = vmr_to_scd(CH4, L_TRUE, temp=T, pres=P)
    h2o_true = vmr_to_scd(1.0e4, L_TRUE, temp=T, pres=P)
    rows = []
    for i in range(n):
        sid = f"S{i:02d}"
        rows.append(_row(sid, "W5", "CH4", "GAS_ATM", ch4_true, 0.01))
        rows.append(_row(sid, "W5", "H2O", "GAS_ATM", h2o_true, 0.01))
        rows.append(_row(sid, "W5", "HCl", "GAS_PLUME",
                         1e18 * (1 + rng.random()), 0.02))
        rows.append(_row(sid, "W1", "CH4", "GAS_ATM",
                         ch4_true * (1 + 0.05 * rng.standard_normal()), 0.05))
    # A failed fit far off: must be ignored.
    rows.append(_row("S99", "W5", "CH4", "GAS_ATM", 10 * ch4_true, 0.01,
                     success=False))
    table = ResultsTable(pd.DataFrame(rows))
    windows = {"W1": SimpleNamespace(atm_gases=("CH4",)),
               "W5": SimpleNamespace(atm_gases=("CH4", "H2O"))}
    ref = vmr_to_scd(CH4, L_CFG, temp=T, pres=P)
    params = {w: {"CH4": SimpleNamespace(ref_column=ref)} for w in windows}
    return table, windows, params


class TestTracerReport:

    def test_unset_tracer_gives_no_report(self):
        table, windows, params = _setup()
        assert tracer_report(table, windows, params, _geometry(None)) is None

    def test_implied_path_and_best_window(self):
        table, windows, params = _setup()
        rep = tracer_report(table, windows, params, _geometry())
        assert rep.best == "W5"                         # the smaller fit error
        assert rep.path_km == pytest.approx(L_TRUE, rel=1e-10)
        assert rep.background_ppmv == pytest.approx(CH4, rel=1e-10)
        assert rep.ratio == pytest.approx(L_TRUE / L_CFG, rel=1e-10)
        assert rep.windows["W1"].rel_error == pytest.approx(0.05)
        assert len(rep.windows["W5"].path_km) == 20     # the failed fit dropped

    def test_ambient_vmrs_at_both_paths(self):
        table, windows, params = _setup()
        rep = tracer_report(table, windows, params, _geometry())
        h2o = next(a for a in rep.ambient if a.window == "W5" and a.gas == "H2O")
        assert h2o.implied_ppmv == pytest.approx(1.0e4, rel=1e-10)
        assert h2o.configured_ppmv == pytest.approx(1.0e4 * L_TRUE / L_CFG, rel=1e-10)
        ch4 = next(a for a in rep.ambient if a.window == "W5" and a.gas == "CH4")
        assert ch4.implied_ppmv == pytest.approx(CH4, rel=1e-10)

    def test_intercept_as_background(self):
        table, windows, params = _setup()
        rep = tracer_report(table, windows, params, _geometry())
        column = vmr_to_scd(415.0, L_TRUE, temp=T, pres=P)
        at_cfg, at_imp = rep.background_vmr(column)
        assert at_imp == pytest.approx(415.0, rel=1e-10)
        assert at_cfg == pytest.approx(415.0 * L_TRUE / L_CFG, rel=1e-10)

    def test_per_spectrum_series(self):
        table, windows, params = _setup()
        s = tracer_report(table, windows, params, _geometry()).per_spectrum()
        assert s.name == COL_TRACER_PATH and len(s) == 20
        np.testing.assert_allclose(s.to_numpy(), L_TRUE, rtol=1e-10)

    def test_crosstalk_names_the_plume_gas(self):
        table, windows, params = _setup()
        df = table.df.copy()
        ch4 = (df.window == "W5") & (df.parameter == "CH4") & df.success
        hcl = (df.window == "W5") & (df.parameter == "HCl")
        # CH4 following HCl: perfect cross-talk.
        df.loc[ch4, "scd"] = df.loc[ch4, "scd"].to_numpy() * (
            1 + 1e-3 * df.loc[hcl, "scd"].to_numpy() / 1e18)
        w5 = tracer_report(ResultsTable(df), windows, params,
                           _geometry()).windows["W5"]
        assert w5.crosstalk_gas == "HCl"
        assert w5.crosstalk_r == pytest.approx(1.0)

    def test_constant_tracer_has_no_crosstalk_figure(self):
        table, windows, params = _setup()
        w5 = tracer_report(table, windows, params, _geometry()).windows["W5"]
        assert w5.crosstalk_gas is None and np.isnan(w5.crosstalk_r)

    def test_warning_only_beyond_the_threshold(self, caplog):
        table, windows, params = _setup()
        with caplog.at_level(logging.WARNING, logger="plumeIR.path_tracer"):
            tracer_report(table, windows, params, _geometry())
        assert "Path check" in caplog.text
        caplog.clear()
        with caplog.at_level(logging.WARNING, logger="plumeIR.path_tracer"):
            # Configured 1.9 km, implied 2.0 km: 5%, no warning.  The
            # reference column follows the configured path, as the builder's.
            params = {w: {"CH4": SimpleNamespace(
                ref_column=vmr_to_scd(CH4, 1.9, temp=T, pres=P))} for w in windows}
            tracer_report(table, windows, params, _geometry(pathlength=1.9))
        assert "Path check" not in caplog.text


class TestSummaryAndWideTable:

    def test_summary_block(self):
        from plumeIR.output import _tracer_lines
        table, windows, params = _setup()
        rep = tracer_report(table, windows, params, _geometry())
        ratios = {("CO2", "SO2"): SimpleNamespace(intercept=1.9e21),
                  ("H2SO4", "SO2"): SimpleNamespace(intercept=1.4)}
        text = "\n".join(_tracer_lines(rep, ratios))
        assert "implied path    2.000 km (W5" in text
        assert "CO2/SO2" in text
        assert "H2SO4/SO2" not in text          # an aerosol has no mixing ratio

    def test_wide_table_gets_the_path_column(self, tmp_path: Path):
        from plumeIR.output import WIDE_NAME, write_results
        table, windows, params = _setup()
        # A target so the wide table has rows.
        df = table.df.copy()
        df.loc[df.parameter == "HCl", "is_target"] = True
        table = ResultsTable(df)
        rep = tracer_report(table, windows, params, _geometry())
        write_results(table, tmp_path, rep)
        wide = pd.read_csv(tmp_path / WIDE_NAME, index_col=0)
        assert COL_TRACER_PATH in wide.columns
        np.testing.assert_allclose(wide[COL_TRACER_PATH], L_TRUE, rtol=1e-10)

    def test_no_column_without_a_tracer(self, tmp_path: Path):
        from plumeIR.output import WIDE_NAME, write_results
        table, *_ = _setup()
        df = table.df.copy()
        df.loc[df.parameter == "HCl", "is_target"] = True
        write_results(ResultsTable(df), tmp_path, None)
        wide = pd.read_csv(tmp_path / WIDE_NAME, index_col=0)
        assert COL_TRACER_PATH not in wide.columns


_LAYER = """
*DATA
data_dir    : {data_dir}
filter_ext  : spc
*GEOMETRY
type        : layer
pathlength  : 1.22
atm_temp    : 284
atm_pres    : 973
plume_temp  : 450
plume_pres  : 973
{geom}
*FIT0
fit         : True
fit_window  : 2450, 2550
atm_gases   : H2O, N2O
plume_gases : SO2
targets     : SO2
*FIT5
fit         : True
fit_window  : 2880, 2990
atm_gases   : {fit5_atm}
plume_gases : {fit5_plume}
targets     : {fit5_targets}
"""


def _build(tmp_path: Path, geom="path_tracer : CH4", fit5_atm="CH4, H2O, N2O",
           fit5_plume="HCl", fit5_targets="HCl"):
    from plumeIR.config.builder import build_from_raw
    from plumeIR.config.parser import read_config
    path = tmp_path / "run.ini"
    path.write_text(_LAYER.format(data_dir=tmp_path, geom=geom, fit5_atm=fit5_atm,
                                  fit5_plume=fit5_plume, fit5_targets=fit5_targets),
                    encoding="utf-8")
    return build_from_raw(read_config(path), base_dir=tmp_path)


class TestConfigRules:

    def test_valid_tracer_builds(self, tmp_path):
        assert _build(tmp_path)["geometry"].path_tracer == "CH4"

    def test_default_is_off(self, tmp_path):
        assert _build(tmp_path, geom="")["geometry"].path_tracer is None

    def test_n2o_is_allowed(self, tmp_path):
        assert _build(tmp_path, geom="path_tracer : N2O")["geometry"].path_tracer == "N2O"

    def test_unknown_tracer_rejected(self, tmp_path):
        with pytest.raises(ConfigError, match="path_tracer"):
            _build(tmp_path, geom="path_tracer : CO2")

    def test_tracer_must_be_ambient_somewhere(self, tmp_path):
        with pytest.raises(ConfigError, match="not an atm_gas of any active window"):
            _build(tmp_path, fit5_atm="H2O, N2O")

    def test_tracer_may_not_be_a_plume_gas(self, tmp_path):
        with pytest.raises(ConfigError, match="is a plume gas"):
            _build(tmp_path, fit5_plume="HCl, CH4")

    def test_tracer_may_not_be_a_target(self, tmp_path):
        with pytest.raises(ConfigError, match="is a target"):
            _build(tmp_path, fit5_targets="HCl, CH4")


def test_scd_to_vmr_is_inverse_in_path():
    """The implied path relies on VMR ∝ 1/L at a fixed column."""
    col = vmr_to_scd(1.9, 2.0, temp=T, pres=P)
    assert scd_to_vmr(col, 1.0, temp=T, pres=P) / 1.9 == pytest.approx(2.0, rel=1e-12)
