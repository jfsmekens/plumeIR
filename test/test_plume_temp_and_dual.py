"""
Tests for staged plume temperature across windows and gases fitted in both
the atmospheric and the plume role.

No RFM: the forward-model test uses synthetic Gaussian-line LUTs.

Run with:  pytest test/test_plume_temp_and_dual.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from plumeIR.config.builder import build_from_raw
from plumeIR.config.objects import (
    FitWindowConfig, LayerGeometry, PlotConfig, Retrieval, GeometryType,
)
from plumeIR.config.parser import read_config
from plumeIR.exceptions import ConfigError
from plumeIR.forward.layer import LayerForwardModel
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.plotting.state import RunState
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference
from plumeIR.retrieval.results import FitResult, ResultsTable
from plumeIR.retrieval.run import _staged_temps


# =============================================================================
# Helpers
# =============================================================================

_INI = """
*DATA
data_dir : {data_dir}

*MODEL
*GEOMETRY
type        : {gtype}
pathlength  : 1.0
atm_temp    : 280
atm_pres    : 1000
plume_temp  : 400
plume_pres  : 1000
{geom}

*RETRIEVAL

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : H2O
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 2
{fit0}

*FIT1
fit         : True
fit_window  : 2690, 2830
atm_gases   : H2O
plume_gases : HCl
plume_aero  : None
targets     : HCl
polydeg     : 2
{fit1}

*FIT2
fit         : True
fit_window  : 2020, 2150
atm_gases   : H2O, CO2
plume_gases : CO
plume_aero  : None
targets     : CO
polydeg     : 2
{fit2}
"""


def _build(tmp_path: Path, gtype: str = "layer", geom: str = "",
           fit0: str = "", fit1: str = "", fit2: str = "") -> dict:
    path = tmp_path / "run.ini"
    path.write_text(_INI.format(data_dir=tmp_path, gtype=gtype, geom=geom,
                                fit0=fit0, fit1=fit1, fit2=fit2), encoding="utf-8")
    return build_from_raw(read_config(path), base_dir=tmp_path)


def _window(cfg: dict, label: str) -> tuple[FitWindowConfig, ParameterSet]:
    i = [w.label for w in cfg["windows"]].index(label)
    return cfg["windows"][i], cfg["params"][i]


def _fit(label: str, values: dict, success: bool = True) -> FitResult:
    wn = np.linspace(2400.0, 2550.0, 20)
    return FitResult(
        success=success, method="nlls", varied_names=tuple(values),
        values=dict(values), perror={k: 1.0 for k in values}, covariance=None,
        wn=wn, measured=np.ones_like(wn), model=np.ones_like(wn), rmse=0.0,
        nfev=1, message="", window_label=label, spectrum_id="S0",
    )


# =============================================================================
# 14a — plume_temp_from
# =============================================================================

class TestPlumeTempFromConfig:

    def test_receiver_holds_gas_temp_fixed(self, tmp_path):
        cfg = _build(tmp_path, fit0="fit_gas_temp : True",
                     fit1="plume_temp_from : FIT0")
        w1, p1 = _window(cfg, "FIT1")
        assert w1.plume_temp_from == "FIT0"
        assert p1["gas_temp"].vary is False
        assert p1["gas_temp"].value == 400.0
        assert _window(cfg, "FIT0")[1]["gas_temp"].vary is True

    def test_receiver_overrides_inherited_geometry_flag(self, tmp_path):
        cfg = _build(tmp_path, geom="fit_gas_temp : True",
                     fit2="plume_temp_from : FIT0")
        assert _window(cfg, "FIT2")[1]["gas_temp"].vary is False
        assert _window(cfg, "FIT1")[1]["gas_temp"].vary is True   # unaffected

    def test_every_window_may_fit_its_own(self, tmp_path):
        cfg = _build(tmp_path, geom="fit_gas_temp : True")
        assert all(p["gas_temp"].vary for p in cfg["params"])

    def test_explicit_fit_and_receive_contradict(self, tmp_path):
        with pytest.raises(ConfigError, match="contradict"):
            _build(tmp_path, fit0="fit_gas_temp : True",
                   fit1="plume_temp_from : FIT0\nfit_gas_temp : True")

    def test_source_must_fit_gas_temp(self, tmp_path):
        with pytest.raises(ConfigError, match="does not fit gas_temp"):
            _build(tmp_path, fit1="plume_temp_from : FIT0")

    def test_source_must_come_first(self, tmp_path):
        with pytest.raises(ConfigError, match="must come before"):
            _build(tmp_path, fit2="fit_gas_temp : True",
                   fit1="plume_temp_from : FIT2")

    def test_source_must_exist(self, tmp_path):
        with pytest.raises(ConfigError, match="not a window label"):
            _build(tmp_path, fit1="plume_temp_from : FIT9")

    def test_no_chains(self, tmp_path):
        with pytest.raises(ConfigError, match="receives its own"):
            _build(tmp_path, fit0="fit_gas_temp : True",
                   fit1="plume_temp_from : FIT0", fit2="plume_temp_from : FIT1")


class TestStagedPlumeTemp:

    def _pset(self) -> ParameterSet:
        return ParameterSet([
            Parameter("HCl", 0.01, True, (0.0, np.inf), ParameterKind.GAS_PLUME),
            Parameter("gas_temp", 450.0, False, (300.0, 1000.0),
                      ParameterKind.TEMPERATURE),
        ])

    def _window(self, src: str | None) -> FitWindowConfig:
        return FitWindowConfig(label="FIT1", wn_start=1.0, wn_stop=2.0,
                               atm_gases=(), plume_gases=("HCl",), plume_aero=(),
                               targets=("HCl",), polydeg=0, plume_temp_from=src)

    def test_takes_this_spectrums_source_value(self):
        out = _staged_temps(self._pset(), self._window("FIT0"),
                                 {"FIT0": _fit("FIT0", {"gas_temp": 512.5})}, "S0")
        assert out["gas_temp"].value == 512.5
        assert out["gas_temp"].vary is False

    def test_failed_source_keeps_a_priori(self):
        out = _staged_temps(self._pset(), self._window("FIT0"),
                                 {"FIT0": _fit("FIT0", {"gas_temp": 512.5},
                                               success=False)}, "S0")
        assert out["gas_temp"].value == 450.0

    def test_clipped_to_bounds(self):
        out = _staged_temps(self._pset(), self._window("FIT0"),
                                 {"FIT0": _fit("FIT0", {"gas_temp": 1200.0})}, "S0")
        assert out["gas_temp"].value == 1000.0

    def test_not_staged_is_untouched(self):
        ps = self._pset()
        assert _staged_temps(ps, self._window(None), {}, "S0") is ps


# =============================================================================
# atm_temp_from — the ambient counterpart (2026-11-13)
# =============================================================================

class TestAtmTempFromConfig:
    """Same rules as plume_temp_from, for atm_temp."""

    def test_receiver_holds_atm_temp_fixed(self, tmp_path):
        cfg = _build(tmp_path, fit0="fit_atm_temp : True",
                     fit1="atm_temp_from : FIT0")
        w1, p1 = _window(cfg, "FIT1")
        assert w1.atm_temp_from == "FIT0"
        assert p1["atm_temp"].vary is False
        assert p1["atm_temp"].value == 280.0
        assert _window(cfg, "FIT0")[1]["atm_temp"].vary is True
        assert "atm_temp" not in _window(cfg, "FIT2")[1]   # neither fits nor receives

    def test_receiver_overrides_inherited_geometry_flag(self, tmp_path):
        cfg = _build(tmp_path, geom="fit_atm_temp : True",
                     fit2="atm_temp_from : FIT0")
        assert _window(cfg, "FIT2")[1]["atm_temp"].vary is False
        assert _window(cfg, "FIT1")[1]["atm_temp"].vary is True

    def test_explicit_fit_and_receive_contradict(self, tmp_path):
        with pytest.raises(ConfigError, match="fit_atm_temp: True and atm_temp_from"):
            _build(tmp_path, fit0="fit_atm_temp : True",
                   fit1="atm_temp_from : FIT0\nfit_atm_temp : True")

    def test_source_must_fit_atm_temp(self, tmp_path):
        with pytest.raises(ConfigError, match="does not fit atm_temp"):
            _build(tmp_path, fit1="atm_temp_from : FIT0")

    def test_source_must_come_first(self, tmp_path):
        with pytest.raises(ConfigError, match="atm_temp_from: window FIT2 must come before"):
            _build(tmp_path, fit2="fit_atm_temp : True", fit1="atm_temp_from : FIT2")

    def test_no_chains(self, tmp_path):
        with pytest.raises(ConfigError, match="receives its own atm_temp"):
            _build(tmp_path, fit0="fit_atm_temp : True",
                   fit1="atm_temp_from : FIT0", fit2="atm_temp_from : FIT1")

    def test_independent_of_plume_temp_from(self, tmp_path):
        # Each temperature from its own source window.
        cfg = _build(tmp_path, fit0="fit_gas_temp : True",
                     fit1="fit_atm_temp : True",
                     fit2="plume_temp_from : FIT0\natm_temp_from : FIT1")
        p2 = _window(cfg, "FIT2")[1]
        assert p2["gas_temp"].vary is False and p2["atm_temp"].vary is False

    def test_receiving_window_does_not_trigger_h2o_warning(self, tmp_path, caplog):
        import logging
        caplog.set_level(logging.WARNING, logger="plumeIR")
        _build(tmp_path, geom="fit_atm_temp : True",
               fit2="atm_temp_from : FIT0\ntargets : CO, H2O")
        assert "FIT2: fits atm_temp and targets" not in caplog.text


class TestStagedAtmTemp:

    def _pset(self) -> ParameterSet:
        return ParameterSet([
            Parameter("CO", 0.01, True, (0.0, np.inf), ParameterKind.GAS_PLUME),
            Parameter("gas_temp", 450.0, False, (300.0, 1000.0),
                      ParameterKind.TEMPERATURE),
            Parameter("atm_temp", 284.0, False, (250.0, 350.0),
                      ParameterKind.TEMPERATURE),
        ])

    def _window(self, plume: str | None, atm: str | None) -> FitWindowConfig:
        return FitWindowConfig(label="FIT2", wn_start=1.0, wn_stop=2.0,
                               atm_gases=("H2O",), plume_gases=("CO",),
                               plume_aero=(), targets=("CO",), polydeg=0,
                               plume_temp_from=plume, atm_temp_from=atm)

    def test_takes_this_spectrums_source_value(self):
        out = _staged_temps(self._pset(), self._window(None, "FIT1"),
                            {"FIT1": _fit("FIT1", {"atm_temp": 283.6})}, "S0")
        assert out["atm_temp"].value == 283.6 and out["atm_temp"].vary is False
        assert out["gas_temp"].value == 450.0

    def test_both_from_their_own_sources(self):
        done = {"FIT0": _fit("FIT0", {"gas_temp": 512.5}),
                "FIT1": _fit("FIT1", {"atm_temp": 290.0})}
        out = _staged_temps(self._pset(), self._window("FIT0", "FIT1"), done, "S0")
        assert out["gas_temp"].value == 512.5 and out["atm_temp"].value == 290.0

    def test_failed_source_keeps_a_priori(self):
        out = _staged_temps(self._pset(), self._window(None, "FIT1"),
                            {"FIT1": _fit("FIT1", {"atm_temp": 290.0},
                                          success=False)}, "S0")
        assert out["atm_temp"].value == 284.0

    def test_clipped_to_bounds(self):
        out = _staged_temps(self._pset(), self._window(None, "FIT1"),
                            {"FIT1": _fit("FIT1", {"atm_temp": 400.0})}, "S0")
        assert out["atm_temp"].value == 350.0


# =============================================================================
# 14b — dual gases
# =============================================================================

class TestDualConfig:

    def test_dual_gas_gets_two_parameters(self, tmp_path):
        cfg = _build(tmp_path, fit2="plume_gases : CO, CO2")
        w, p = _window(cfg, "FIT2")
        assert w.dual_gases == ("CO2",)
        assert w.atm_name("CO2") == "CO2_atm" and w.atm_name("H2O") == "H2O"
        assert p["CO2_atm"].kind is ParameterKind.GAS_ATM
        assert p["CO2"].kind is ParameterKind.GAS_PLUME
        # Ambient over the whole path at atm_temp, plume over plume_thickness.
        assert p["CO2_atm"].ref_column > 10 * p["CO2"].ref_column
        assert p["CO2_atm"].value == 1.0 and p["CO2"].value == 0.01

    def test_ambient_part_may_be_a_target(self, tmp_path):
        cfg = _build(tmp_path, fit2="plume_gases : CO, CO2\ntargets : CO, CO2, CO2_atm")
        assert "CO2_atm" in _window(cfg, "FIT2")[0].targets

    def test_ambient_name_of_single_role_gas_is_not_a_target(self, tmp_path):
        with pytest.raises(ConfigError, match="unknown names"):
            _build(tmp_path, fit2="targets : CO, CO2_atm")

    def test_continuum_is_2d_for_the_ambient_part_only(self, tmp_path):
        cfg = _build(tmp_path, fit2="plume_gases : CO, H2O\ncontinuum : H2O")
        w, p = _window(cfg, "FIT2")
        assert w.uses_2d("H2O", plume=False) is True
        assert w.uses_2d("H2O", plume=True) is False
        # 2-D: bounds from the VMR grid; 1-D: unbounded above.
        assert np.isfinite(p["H2O_atm"].bounds[1])
        assert p["H2O"].bounds == (0.0, np.inf)

    def test_single_role_continuum_gas_stays_2d(self, tmp_path):
        cfg = _build(tmp_path, fit2="continuum : H2O")
        w, p = _window(cfg, "FIT2")
        assert w.uses_2d("H2O", plume=False) is True
        assert np.isfinite(p["H2O"].bounds[1])

    def test_refused_outside_layer(self, tmp_path):
        with pytest.raises(ConfigError, match="only in the layer geometry"):
            _build(tmp_path, gtype="solar", fit2="plume_gases : CO, CO2")


def _gauss_lut(species: str, wn: np.ndarray, centres: list[float],
               strength_hot: float) -> GasLUT:
    """A 1-D LUT whose line strengths change with temperature, line by line."""
    temps = np.array([250.0, 300.0, 350.0, 400.0, 600.0, 800.0, 1000.0])
    od = np.zeros((temps.size, wn.size))
    for i, t in enumerate(temps):
        # First line weakens with T, second strengthens: a shape change.
        w = (t - temps[0]) / (temps[-1] - temps[0])
        for c, s in zip(centres, (1.0 - 0.5 * w, 0.3 + strength_hot * w)):
            od[i] += s * np.exp(-0.5 * ((wn - c) / 0.3) ** 2)
    return GasLUT(species=species, wn=wn, temp_nodes=temps, od_table=od, ref_scd=1.0)


class TestDualForward:

    def test_atm_and_plume_parts_add_in_optical_depth(self):
        wn = np.arange(2000.0, 2100.0, 0.04)
        atm = _gauss_lut("CO2", wn, [2040.0, 2060.0], 1.0)
        plume = _gauss_lut("CO2", wn, [2040.0, 2060.0], 1.0)
        ref = Reference(wn=wn, gas_luts={"CO2_atm": atm, "CO2": plume})
        window = FitWindowConfig(label="FIT0", wn_start=2010.0, wn_stop=2090.0,
                                 atm_gases=("CO2",), plume_gases=("CO2",),
                                 plume_aero=(), targets=("CO2",), polydeg=0)
        geom = LayerGeometry(pathlength=1.0, atm_temp=280.0, atm_pres=1000.0,
                             plume_temp=600.0, plume_pres=1000.0)
        retr = Retrieval(geometry_type=GeometryType.LAYER, fit_fov=False,
                         fit_shift=False, max_opd=1.037, apod_type="triangle",
                         ils_resolved=True)
        ps = ParameterSet([
            Parameter("poly0", 1.0, True, (-np.inf, np.inf), ParameterKind.POLYNOMIAL),
            Parameter("CO2_atm", 1.0, True, (0.0, np.inf), ParameterKind.GAS_ATM),
            Parameter("CO2", 0.4, True, (0.0, np.inf), ParameterKind.GAS_PLUME),
            Parameter("fov", 0.02, False, (0.0, 0.2), ParameterKind.SPECTRAL),
        ])
        fm = LayerForwardModel(ref, ps, geom, retr, window, wn[100:-100])
        F = fm.model_highres(ps.p0_vector())
        expected = np.exp(-(atm.query(280.0) + 0.4 * plume.query(600.0)))
        np.testing.assert_allclose(F, expected, rtol=1e-12)


# =============================================================================
# Ratio background of a dual target
# =============================================================================

def _table(dual: bool) -> ResultsTable:
    rows = []
    for i in range(5):
        sid = f"S{i}"
        for param, kind, scd in (
            ("SO2", "GAS_PLUME", 1e19 * (i + 1)),
            ("CO2", "GAS_PLUME", 5e19 * (i + 1)),
            *((("CO2_atm", "GAS_ATM", 1.27e21),) if dual else ()),
        ):
            rows.append(dict(spectrum_id=sid, window="FIT0", parameter=param,
                             species=param, kind=kind, varied=True, value=1.0,
                             error=0.1, scd=scd, scd_error=scd * 0.01,
                             pres=1000.0, temp=300.0,
                             is_target=param in ("SO2", "CO2"),
                             success=True, rmse=0.0, r2=1.0))
    return ResultsTable(pd.DataFrame(rows))


class TestDualRatioBackground:

    GEOM = LayerGeometry(pathlength=1.0, atm_temp=280.0, atm_pres=1000.0,
                         plume_temp=450.0, plume_pres=1000.0)

    def test_dual_target_has_zero_background(self):
        state = RunState.from_table(_table(True), PlotConfig(ratios=(("CO2", "SO2"),)),
                                    geometry=self.GEOM)
        assert state.dual == {"CO2"}
        assert state.background_column("CO2") == 0.0

    def test_single_role_target_keeps_ambient_background(self):
        state = RunState.from_table(_table(False), PlotConfig(ratios=(("CO2", "SO2"),)),
                                    geometry=self.GEOM)
        assert state.dual == set()
        assert state.background_column("CO2") > 1e21
