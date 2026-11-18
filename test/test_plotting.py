"""
Tests for M3: ratio settings in the config, the run state, the panels, the
command line's canvas and its --plot / --save-plots flags; and for M3c: the
stored fits (fits.npz), RunState.from_run_dir / select, the inspector and
--inspect.

Synthetic throughout: two windows (SO2 in FIT0, HCl in FIT1) whose columns
follow a known ratio, so every regression has an exact answer.  The canvas is
only ever built off-screen here (``interactive=False``); the live window needs
a display.

Run with:  pytest test/test_plotting.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from matplotlib.figure import Figure
from plumeIR.regression import york_fit

import plumeIR.__main__ as cli
from plumeIR.config.builder import build_from_raw
from plumeIR.config.objects import (
    DataConfig, FitWindowConfig, GeometryType, LayerGeometry, ModelConfig,
    PlotConfig, Retrieval,
)
from plumeIR.config.parser import read_config, write_config
from plumeIR.exceptions import ConfigError
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.pipeline import RunSetup, collect_run
from plumeIR.plotting.canvas import AnalysisCanvas, save_final_figures, window_titles
from plumeIR.plotting.panels import FitPanel, RatioPanel, TimeSeriesPanel
from plumeIR.plotting.state import RunState, fit_ratio
from plumeIR.retrieval.results import FitResult, ResultsTable
from plumeIR.retrieval.run import SpectrumFits

#: True ratio HCl/SO2 and background HCl column of the synthetic run.
RATIO, BACKGROUND = 0.04, 2e17
SO2_REF, HCL_REF = 1e18, 1e17


# =============================================================================
# Synthetic run
# =============================================================================

_INI = """
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

*RETRIEVAL

{plot}

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : H2O
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 1

*FIT1
fit         : True
fit_window  : 2700, 2800
atm_gases   : H2O
plume_gases : HCl
plume_aero  : None
targets     : HCl
polydeg     : 1
"""


def _ini(tmp_path: Path, plot: str = "") -> Path:
    path = tmp_path / "run.ini"
    path.write_text(_INI.format(data_dir=tmp_path, plot=plot), encoding="utf-8")
    return path


def _so2(i: int) -> float:
    """SO2 column of spectrum i [molec/cm2], varied so the ratio is defined."""
    return (1.0 + 0.5 * np.sin(i) + 0.05 * i) * 1e18


def _fit(label: str, sid: str, gas: str, column: float, ref: float,
         success: bool = True) -> FitResult:
    wn = np.linspace(2400.0, 2550.0, 40) if label == "FIT0" else np.linspace(2700.0, 2800.0, 40)
    meas = 1.0 + 0.1 * np.sin(wn / 7.0)
    return FitResult(
        success=success, method="nlls", varied_names=("poly0", gas),
        values={"poly0": 1.0, gas: column / ref, "fov": 0.021, "nu_shift": 0.01},
        perror={"poly0": 0.01, gas: 0.01 * column / ref},
        covariance=None, wn=wn, measured=meas, model=meas + 1e-3,
        rmse=1e-3, nfev=10, message="ok", window_label=label, spectrum_id=sid,
    )


def _setup(n: int = 12, plot: PlotConfig | None = None) -> RunSetup:
    windows, params = {}, {}
    for label, gas, ref, lo, hi in (("FIT0", "SO2", SO2_REF, 2400.0, 2550.0),
                                    ("FIT1", "HCl", HCL_REF, 2700.0, 2800.0)):
        windows[label] = FitWindowConfig(
            label=label, wn_start=lo, wn_stop=hi, atm_gases=(),
            plume_gases=(gas,), plume_aero=(), targets=(gas,), polydeg=0)
        params[label] = ParameterSet([
            Parameter("poly0", 1.0, True, (-np.inf, np.inf), ParameterKind.POLYNOMIAL),
            Parameter(gas, 1.0, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                      ref_column=ref),
        ])
    return RunSetup(
        data=DataConfig(data_dir=Path("/data")), model=ModelConfig(),
        geometry=LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                               plume_temp=450.0, plume_pres=1000.0),
        retrieval=Retrieval(geometry_type=GeometryType.LAYER, max_opd=1.037,
                            apod_type="triangle", ils_resolved=True),
        windows=windows, params=params, references={"FIT0": None, "FIT1": None},
        spectra=[f"S{i}" for i in range(n)],
        plot=plot or PlotConfig(ratios=(("HCl", "SO2"),), scroll=5,
                                regression="odr"))


def _item(i: int, n: int, fail: bool = False) -> SpectrumFits:
    sid = f"2021-08-11T10:00:{i:02d}"
    so2 = _so2(i)
    hcl = RATIO * so2 + BACKGROUND
    return SpectrumFits(i, n, f"S{i}", (
        _fit("FIT0", sid, "SO2", so2, SO2_REF),
        _fit("FIT1", sid, "HCl", hcl, HCL_REF, success=not fail),
    ))


def _fed(setup: RunSetup, order=None, fail: int | None = None) -> RunState:
    state = RunState.for_run(setup)
    n = setup.n_spectra
    for i in (order if order is not None else range(n)):
        state.add(_item(i, n, fail=i == fail))
    return state


# =============================================================================
# Item 10: ratio settings in the config
# =============================================================================

class TestPlotConfig:

    def test_defaults_when_section_absent(self, tmp_path):
        cfg = build_from_raw(read_config(_ini(tmp_path)), base_dir=tmp_path)
        assert cfg["plot"] == PlotConfig()
        assert cfg["plot"].scroll == 200 and cfg["plot"].regression == "siegel"

    def test_section_is_built(self, tmp_path):
        plot = "*PLOT\nratios : HCl:SO2\nscroll : 50\nregression : ODR"
        cfg = build_from_raw(read_config(_ini(tmp_path, plot)), base_dir=tmp_path)
        assert cfg["plot"] == PlotConfig(ratios=(("HCl", "SO2"),), scroll=50,
                                         regression="odr")

    def test_several_ratios(self, tmp_path):
        plot = "*PLOT\nratios : HCl:SO2, SO2:HCl"
        cfg = build_from_raw(read_config(_ini(tmp_path, plot)), base_dir=tmp_path)
        assert cfg["plot"].ratios == (("HCl", "SO2"), ("SO2", "HCl"))

    @pytest.mark.parametrize("plot, message", [
        ("ratios : HCl/SO2", "is not a ratio"),
        ("ratios : HCl:SO2:CO", "is not a ratio"),
        ("ratios : CO:SO2", "needs ['CO']"),
        ("ratios : SO2:SO2", "with itself"),
        ("scroll : 2", "at least 3"),
        ("scroll : 10.5", "at least 3"),
        ("regression : ols", "PLOT.regression"),
    ])
    def test_invalid_settings_are_config_errors(self, tmp_path, plot, message):
        with pytest.raises(ConfigError, match=message.replace("[", r"\[").replace("]", r"\]")):
            build_from_raw(read_config(_ini(tmp_path, "*PLOT\n" + plot)),
                           base_dir=tmp_path)

    def test_inactive_window_target_is_rejected(self, tmp_path):
        text = _ini(tmp_path, "*PLOT\nratios : HCl:SO2").read_text()
        text = text.replace("*FIT1\nfit         : True", "*FIT1\nfit         : False")
        path = tmp_path / "inactive.ini"
        path.write_text(text, encoding="utf-8")
        with pytest.raises(ConfigError, match="needs"):
            build_from_raw(read_config(path), base_dir=tmp_path)

    def test_background_inputs(self, tmp_path):
        plot = "*PLOT\nratios : HCl:SO2\nbackground_CO2 : 415\nbackground_ch4 : 1.95\nrh : 60"
        cfg = build_from_raw(read_config(_ini(tmp_path, plot)), base_dir=tmp_path)
        assert cfg["plot"].background == {"CO2": 415.0, "CH4": 1.95}
        assert cfg["plot"].rh == 60.0

    @pytest.mark.parametrize("plot, message", [
        ("background_XYZ : 1", "unknown gas"),
        ("background_CO2 : -1", "VMR in ppmv"),
        ("rh : 0", "relative humidity"),
        ("rh : 120", "relative humidity"),
    ])
    def test_invalid_background_inputs(self, tmp_path, plot, message):
        with pytest.raises(ConfigError, match=message):
            build_from_raw(read_config(_ini(tmp_path, "*PLOT\n" + plot)),
                           base_dir=tmp_path)

    def test_round_trip(self, tmp_path):
        raw = read_config(_ini(tmp_path, "*PLOT\nratios : HCl:SO2\nscroll : 20"))
        out = tmp_path / "out.ini"
        write_config(raw, out)
        assert read_config(out) == raw

    def test_setup_carries_the_plot_config(self, tmp_path, monkeypatch):
        import plumeIR.pipeline as pipeline
        cfg = build_from_raw(read_config(_ini(tmp_path, "*PLOT\nratios : HCl:SO2")),
                             base_dir=tmp_path)
        monkeypatch.setattr(pipeline, "build_reference_for", lambda *a, **k: None)
        monkeypatch.setattr(pipeline, "load_spectra", lambda data: ["S0"])
        monkeypatch.setattr(pipeline, "preprocess_spectra", lambda s, g, d, p: s)
        monkeypatch.setattr(pipeline, "resolve_instrument_ils", lambda r, s: r)
        assert pipeline.prepare_run(cfg).plot.ratios == (("HCl", "SO2"),)


# =============================================================================
# Item 11: run state
# =============================================================================

class TestFitRatio:

    @pytest.mark.parametrize("method", ["siegel", "theil", "odr"])
    def test_exact_line(self, method):
        x = np.linspace(1e18, 5e18, 30)
        fit = fit_ratio(x, RATIO * x + BACKGROUND, method=method)
        assert fit.slope == pytest.approx(RATIO, rel=1e-9)
        assert fit.intercept == pytest.approx(BACKGROUND, rel=1e-6)
        assert fit.n == 30 and fit.method == method

    def test_odr_is_york(self):
        """Weighted ODR is plumeIR.regression.york_fit, with its MSWD."""
        rng = np.random.default_rng(1)
        x = np.linspace(1e18, 5e18, 60) + rng.normal(0, 1e17, 60)
        y = RATIO * x + BACKGROUND + rng.normal(0, 5e15, 60)
        sx, sy = np.full(60, 1e17), np.full(60, 5e15)
        ref = york_fit(x, y, sx, sy)
        fit = fit_ratio(x, y, sx, sy, "odr")
        assert (fit.slope, fit.intercept, fit.slope_err, fit.intercept_err,
                fit.mswd) == (ref.slope, ref.intercept, ref.slope_err,
                              ref.intercept_err, ref.mswd)

    def test_odr_unweighted_without_usable_errors(self):
        x = np.linspace(1.0, 10.0, 20)
        y = 2.0 * x + 1.0 + np.sin(x)
        sx = np.full(20, 0.1)
        sy = np.full(20, 0.1)
        sy[3] = 0.0
        fit = fit_ratio(x, y, sx, sy, "odr")
        ref = york_fit(x, y)
        assert fit.slope == ref.slope and np.isnan(fit.mswd)

    def test_too_few_points_or_no_spread(self):
        assert fit_ratio(np.array([1.0, 2.0, np.nan]), np.array([1.0, 2.0, 3.0])) is None
        assert fit_ratio(np.ones(5), np.arange(5.0)) is None

    def test_unknown_method(self):
        with pytest.raises(ValueError, match="Unknown"):
            fit_ratio(np.arange(5.0), np.arange(5.0), method="ols")


class TestRunState:

    def test_columns_follow_the_results_table(self):
        setup = _setup()
        state = _fed(setup)
        so2, err = state.column("SO2")
        np.testing.assert_allclose(so2, [_so2(i) for i in range(12)], rtol=1e-12)
        np.testing.assert_allclose(err, 0.01 * so2, rtol=1e-12)
        assert state.n_received == 12 and state.n_spectra == 12
        assert set(state.latest_fits) == {"FIT0", "FIT1"}
        assert np.issubdtype(state.times().dtype, np.datetime64)

    def test_parallel_arrival_order_gives_the_same_series(self):
        setup = _setup()
        order = [3, 0, 7, 1, 11, 2, 5, 4, 10, 6, 9, 8]
        a, b = _fed(setup), _fed(setup, order=order)
        for sp in ("SO2", "HCl"):
            np.testing.assert_array_equal(a.column(sp)[0], b.column(sp)[0])
        assert a.ratio_fit(("HCl", "SO2")) == b.ratio_fit(("HCl", "SO2"))
        assert b.latest_id == "2021-08-11T10:00:08"      # the last to arrive
        assert b.newest_point(("HCl", "SO2"))[0] == pytest.approx(_so2(8))

    def test_ratio_over_all_and_over_scroll(self):
        state = _fed(_setup())
        full = state.ratio_fit(("HCl", "SO2"))
        assert full.slope == pytest.approx(RATIO, rel=1e-9) and full.n == 12
        latest = state.ratio_fit(("HCl", "SO2"), last=state.scroll)
        assert latest.n == 5
        x, _, _, _ = state.ratio_points(("HCl", "SO2"), last=5)
        np.testing.assert_allclose(x, [_so2(i) for i in range(7, 12)])
        assert state.final_ratios() == {("HCl", "SO2"): full}

    def test_failed_fit_is_left_out(self):
        state = _fed(_setup(), fail=4)
        hcl, _ = state.column("HCl")
        assert np.isnan(hcl[4]) and np.isfinite(np.delete(hcl, 4)).all()
        assert state.ratio_fit(("HCl", "SO2")).n == 11

    def test_from_table_matches_the_live_state(self):
        setup = _setup()
        live = _fed(setup, order=[1, 0, 2, 3, 5, 4, 6, 7, 8, 9, 11, 10])
        items = [_item(i, 12) for i in range(12)]
        table = collect_run(setup, items)
        loaded = RunState.from_table(table, setup.plot)
        assert loaded.targets == live.targets
        for sp in ("SO2", "HCl"):
            np.testing.assert_array_equal(loaded.column(sp)[0], live.column(sp)[0])
        assert loaded.final_ratios() == live.final_ratios()
        assert loaded.latest_fits == {}

    def test_from_csv(self, tmp_path):
        setup = _setup()
        table = collect_run(setup, [_item(i, 12) for i in range(12)])
        table.to_csv(tmp_path / "results_long.csv")
        loaded = RunState.from_table(ResultsTable.from_csv(tmp_path / "results_long.csv"),
                                     setup.plot)
        assert loaded.ratio_fit(("HCl", "SO2")).slope == pytest.approx(RATIO, rel=1e-9)

    def test_mass_ratio_with_an_aerosol(self):
        state = RunState(["SO2", "ASH"], ratios=[("ASH", "SO2")])
        assert state.is_mass_ratio(("ASH", "SO2"))
        assert not RunState(["SO2", "HCl"]).is_mass_ratio(("HCl", "SO2"))

    def test_background_columns(self):
        from plumeIR.config.objects import EmissionGeometry
        from plumeIR.units import rh_to_vmr, vmr_to_scd
        geom = _setup().geometry
        state = RunState(["SO2", "CO2", "H2O", "HCl"], geometry=geom,
                         background={"CO2": 415.0}, rh=50.0)
        expected = vmr_to_scd(415.0, geom.pathlength, temp=geom.atm_temp,
                              pres=geom.atm_pres)
        assert state.background_column("CO2") == pytest.approx(expected, rel=1e-12)
        h2o = vmr_to_scd(rh_to_vmr(50.0, pres=geom.atm_pres, temp=geom.atm_temp),
                         geom.pathlength, temp=geom.atm_temp, pres=geom.atm_pres)
        assert state.background_column("H2O") == pytest.approx(h2o, rel=1e-12)
        assert np.isnan(state.background_column("HCl"))          # no background
        x_bg, y_bg = state.ratio_background(("CO2", "SO2"))
        assert np.isnan(x_bg) and y_bg == pytest.approx(expected)     # SO2: none
        # The typical default applies when no value is given; no RH, no H2O.
        default = RunState(["CO2", "H2O"], geometry=geom)
        assert default.background_column("CO2") == pytest.approx(expected * 420 / 415)
        assert np.isnan(default.background_column("H2O"))
        # Emission: no single ambient path.
        emis = RunState(["CO2"], geometry=EmissionGeometry(
            plume_height=1.0, plume_thickness=0.4, elev=25.0, obs_height=0.5,
            atm_path=Path("x.atm")))
        assert np.isnan(emis.background_column("CO2"))

    def test_only_a_live_run_has_spectra(self):
        setup = _setup()
        loaded = RunState.from_table(collect_run(setup, [_item(i, 12) for i in range(12)]),
                                     setup.plot)
        assert _fed(setup).has_spectra and not loaded.has_spectra

    def test_ratio_species_must_be_targets(self):
        with pytest.raises(ValueError, match="not targets"):
            RunState(["SO2"], ratios=[("HCl", "SO2")])

    def test_add_needs_the_run(self):
        state = RunState.from_table(collect_run(_setup(), [_item(0, 1)]))
        with pytest.raises(RuntimeError, match="for_run"):
            state.add(_item(1, 2))


# =============================================================================
# Item 12: panels
# =============================================================================

class TestPanels:

    def test_fit_panel(self):
        state = _fed(_setup())
        fig = Figure()
        ax, ax_res = fig.subplots(2, 1)
        panel = FitPanel(ax, ax_res, "FIT1", "FIT1: HCl")
        panel.update(state)
        fr = state.latest_fits["FIT1"]
        np.testing.assert_array_equal(ax.lines[0].get_ydata(), fr.measured)
        np.testing.assert_array_equal(ax.lines[1].get_ydata(), fr.model)
        np.testing.assert_allclose(ax_res.lines[0].get_ydata(), fr.residual)
        assert ax.get_xlim()[0] > ax.get_xlim()[1]        # wavenumber descending
        metrics = next(t for t in ax.texts if "R^2" in t.get_text())
        assert "RMSE" not in metrics.get_text()                 # R² only
        assert metrics.get_transform() == ax.transAxes          # anchored to the axes
        assert ax.get_title() == "FIT1: HCl"                    # centred title
        assert ax.get_legend() is None and ax_res.get_legend() is None
        assert ax.lines[1].get_linestyle() == "-"                # model solid
        assert len(ax.lines[2].get_xdata()) == 0                 # no continuum given

    def test_fit_panel_draws_the_continuum(self):
        import dataclasses
        state = _fed(_setup())
        fr = state.latest_fits["FIT0"]
        state.latest_fits["FIT0"] = dataclasses.replace(
            fr, continuum=np.full(fr.wn.shape, 1.2))
        fig = Figure()
        ax, ax_res = fig.subplots(2, 1)
        FitPanel(ax, ax_res, "FIT0").update(state)
        assert ax.lines[2].get_linestyle() == "--"
        np.testing.assert_array_equal(ax.lines[2].get_ydata(), 1.2)
        handles, labels = FitPanel.legend_handles()
        assert labels == ["Measured", "Model", "Background polynomial"]

    def test_fit_panel_waits_for_its_window(self):
        fig = Figure()
        ax, ax_res = fig.subplots(2, 1)
        FitPanel(ax, ax_res, "FIT0").update(RunState(["SO2"]))
        assert len(ax.lines[0].get_xdata()) == 0

    def test_time_series_panel(self):
        state = _fed(_setup())
        fig = Figure()
        ax = fig.add_subplot()
        TimeSeriesPanel(ax, live=True).update(state)
        so2 = next(l for l in ax.lines if "SO" in l.get_label())
        assert len(so2.get_ydata()) == state.scroll
        assert np.nanmax(so2.get_ydata()) == pytest.approx(1.0)
        assert len({l.get_marker() for l in ax.lines}) == 2   # not colour alone
        from matplotlib import colors as mcolors
        from plumeIR.constants import PLOT_COLORS
        assert mcolors.to_hex(so2.get_color()) == PLOT_COLORS["SO2"].lower()
        whole = Figure().add_subplot()
        TimeSeriesPanel(whole, live=False).update(state)
        assert len(whole.lines[0].get_ydata()) == 12

    def test_time_series_aerosols_dashed_and_open(self):
        state = RunState(["SO2", "ASH"])
        ax = Figure().add_subplot()
        TimeSeriesPanel(ax).update(state)
        so2, ash = ax.lines
        assert so2.get_linestyle() == "-" and ash.get_linestyle() == "--"
        assert ash.get_markerfacecolor() == "white"
        from matplotlib import colors as mcolors
        from plumeIR.constants import PLOT_COLORS
        assert mcolors.to_hex(ash.get_color()) == PLOT_COLORS["ASH"].lower()

    def test_ratio_panel_shows_the_state_slope(self):
        state = _fed(_setup())
        fig = Figure()
        ax = fig.add_subplot()
        RatioPanel(ax, ("HCl", "SO2"), live=True).update(state)
        assert len(ax.containers) == 2          # faint older points + window, with errors
        assert ax.containers[0].lines[0].get_alpha() == RatioPanel.OUTSIDE_ALPHA
        fit = state.ratio_fit(("HCl", "SO2"), last=state.scroll)
        assert ax.get_legend() is None
        title = ax.get_title(loc="left")
        assert f"{fit.slope:.4g}" in title
        assert "\n" not in title                                   # one line
        assert RatioPanel.scope(state) == "Ratios: ODR, latest 5 of 5 spectra"
        intercept = next(l for l in ax.lines if l.get_label().startswith("Intercept"))
        assert intercept.get_ydata()[0] == pytest.approx(fit.intercept)
        assert intercept.get_linestyle() == "--"
        right = ax.get_title(loc="right")
        assert "intercept" in right and "ambient" not in right     # HCl: none
        whole = Figure().add_subplot()
        RatioPanel(whole, ("HCl", "SO2"), live=False).update(state)
        assert RatioPanel.scope(state, live=False) == "Ratios: ODR, all 12 spectra"


# =============================================================================
# Item 13: canvas
# =============================================================================

    def test_ratio_panel_background_line(self):
        state = RunState(["SO2", "HCl"], ratios=[("HCl", "SO2")],
                         geometry=_setup().geometry, background={"HCl": 0.01})
        fig = Figure()
        ax = fig.add_subplot()
        RatioPanel(ax, ("HCl", "SO2")).update(state)
        bg = next(l for l in ax.lines if l.get_linestyle() == ":"
                  and np.isfinite(l.get_ydata()[0]))
        assert bg.get_ydata()[0] == pytest.approx(state.background_column("HCl"))
        ambient = next(l for l in ax.lines if l.get_label().startswith("Ambient"))
        # SO2 has no background, so the no-plume point sits at x = 0.
        assert ambient.get_xdata()[0] == 0.0
        assert ambient.get_ydata()[0] == pytest.approx(state.background_column("HCl"))
        assert "ambient " in ax.get_title(loc="right")
        _, labels = RatioPanel.legend_handles()
        assert "Ambient (predicted)" in labels and "Newest" in labels
        _, labels = RatioPanel.legend_handles(intercept=False, ambient=False, live=False)
        assert labels == ["Spectra", "Regression"]


    def test_no_intercept_line_when_x_has_a_background(self):
        state = _fed(_setup())
        state.geometry = _setup().geometry
        state.background_vmr = {"SO2": 1.0}          # pretend SO2 has an ambient column
        fig = Figure()
        ax = fig.add_subplot()
        RatioPanel(ax, ("HCl", "SO2")).update(state)
        right = ax.get_title(loc="right")
        assert "intercept" not in right and "ambient (" in right


class TestCanvas:

    def test_layout_and_update(self, tmp_path):
        setup = _setup()
        state = RunState.for_run(setup)
        canvas = AnalysisCanvas(state, window_titles(setup.windows), label="layer")
        # 2 fit + 2 residual axes, time series, one ratio.
        assert len(canvas.fig.axes) >= 6
        # One key for every plot, outside them; none inside.
        assert len(canvas.fig.legends) == 1
        labels = [t.get_text() for t in canvas.fig.legends[0].get_texts()]
        assert labels[:3] == ["Measured", "Model", "Background polynomial"]
        assert "Regression" in labels
        for i in range(12):
            state.add(_item(i, 12))
            canvas.update(state)
        assert canvas.fig._suptitle.get_text().startswith("Spectrum 12 of 12")
        assert canvas.fig.legends[0].get_title().get_text().startswith("Ratios: ODR")
        assert canvas.save(tmp_path / "c.png").stat().st_size > 0

    def test_emission_key_names_the_clear_sky(self):
        from plumeIR.config.objects import EmissionGeometry
        state = RunState(["SO2"], windows={"FIT0": None}, geometry=EmissionGeometry(
            plume_height=1.0, plume_thickness=0.4, elev=25.0, obs_height=0.5,
            atm_path=Path("x.atm")))
        canvas = AnalysisCanvas(state)
        labels = [t.get_text() for t in canvas.fig.legends[0].get_texts()]
        assert labels == ["Measured", "Model", "Clear sky (model)"]

    def test_final_figures_live_run(self, tmp_path):
        setup = _setup()
        state = _fed(setup)
        written = save_final_figures(state, tmp_path / "plots",
                                     window_titles(setup.windows), "layer")
        names = sorted(p.name for p in written)
        assert names == ["canvas.png", "fit_FIT0.png", "fit_FIT1.png",
                         "ratio_HCl_SO2.png", "timeseries.png"]
        assert all(p.stat().st_size > 0 for p in written)

    def test_final_figures_from_a_saved_table(self, tmp_path):
        setup = _setup()
        collect_run(setup, [_item(i, 12) for i in range(12)]).to_csv(tmp_path / "r.csv")
        state = RunState.from_table(ResultsTable.from_csv(tmp_path / "r.csv"), setup.plot)
        written = save_final_figures(state, tmp_path / "plots")
        assert sorted(p.name for p in written) == ["canvas.png", "ratio_HCl_SO2.png",
                                                   "timeseries.png"]


# =============================================================================
# Item 14: command line
# =============================================================================

@pytest.fixture
def patched(monkeypatch):
    setup = _setup(n=6)

    monkeypatch.setattr(cli, "prepare_run", lambda cfg, **kw: setup)
    monkeypatch.setattr(cli, "require_rfm", lambda: None)
    monkeypatch.setattr(cli, "stream_run",
                        lambda s, **kw: (_item(i, 6) for i in (2, 0, 1, 3, 5, 4)))
    return setup


def _run_dir(parent: Path) -> Path:
    (run,) = [p for p in parent.iterdir() if p.is_dir()]
    return run


class TestCommandLinePlots:

    def test_save_plots_headless(self, tmp_path, patched, monkeypatch):
        built = []
        real = AnalysisCanvas.__init__

        def spy(self, *a, **kw):
            built.append(kw.get("interactive", False))
            real(self, *a, **kw)
        monkeypatch.setattr(AnalysisCanvas, "__init__", spy)

        out = tmp_path / "runs"
        assert cli.main([str(_ini(tmp_path)), "--save-plots", "--parallel",
                         "--output-dir", str(out)]) == 0
        assert built == [False, False]          # live canvas and final: no window
        plots = _run_dir(out) / "plots"
        # Redraws throttled (default 1 s): the first spectrum, then the end.
        assert sorted(p.name for p in (plots / "frames").iterdir()) == \
            ["canvas_0001.png", "canvas_0006.png"]
        for name in ("canvas.png", "fit_FIT0.png", "fit_FIT1.png",
                     "timeseries.png", "ratio_HCl_SO2.png"):
            assert (plots / name).is_file(), name

    def test_redraw_interval_zero_draws_every_spectrum(self, tmp_path, patched):
        out = tmp_path / "runs"
        assert cli.main([str(_ini(tmp_path)), "--save-plots", "--redraw-interval", "0",
                         "--output-dir", str(out)]) == 0
        frames = sorted(p.name for p in (_run_dir(out) / "plots" / "frames").iterdir())
        assert frames == [f"canvas_{i:04d}.png" for i in range(1, 7)]

    def test_redraws_are_throttled(self, tmp_path, monkeypatch):
        """Every result enters the state; the canvas redraws once per interval."""
        import argparse
        clock = {"t": 0.0}
        monkeypatch.setattr(cli.time, "monotonic", lambda: clock["t"])
        setup = _setup(n=10)
        state = RunState.for_run(setup)
        args = argparse.Namespace(plot=False, save_plots=False, redraw_interval=1.0)
        plots = cli._Plots(args, setup, state, tmp_path)
        drawn = []
        plots.canvas = type("C", (), {"update": lambda self, st: drawn.append(st.n_received),
                                      "interactive": False, "closed": False})()
        for i in range(10):                    # one result every 0.3 s
            state.add(_item(i, 10))
            plots.update()
            clock["t"] += 0.3
        plots.finish()
        assert drawn == [1, 5, 9, 10]          # 0 s, 1.2 s, 2.4 s, then the end
        assert state.n_received == 10

    def test_slow_fits_draw_once_per_result_and_never_idle(self, tmp_path, monkeypatch):
        """Redraws follow arrivals: results every 7 s are each drawn once, no more."""
        import argparse
        clock = {"t": 0.0}
        monkeypatch.setattr(cli.time, "monotonic", lambda: clock["t"])
        setup = _setup(n=4)
        state = RunState.for_run(setup)
        plots = cli._Plots(argparse.Namespace(plot=False, save_plots=False,
                                              redraw_interval=1.0), setup, state, tmp_path)
        drawn = []
        plots.canvas = type("C", (), {"update": lambda self, st: drawn.append(st.n_received),
                                      "interactive": False, "closed": False})()
        for i in range(4):
            state.add(_item(i, 4))
            plots.update()
            clock["t"] += 7.0
        plots.finish()                         # last result already drawn: no extra redraw
        assert drawn == [1, 2, 3, 4]

    def test_negative_redraw_interval_is_refused(self, capsys):
        with pytest.raises(SystemExit):
            cli.main(["x.ini", "--redraw-interval", "-1"])

    def test_ratios_in_summary_without_plotting(self, tmp_path, patched):
        out = tmp_path / "runs"
        assert cli.main([str(_ini(tmp_path)), "--output-dir", str(out)]) == 0
        run = _run_dir(out)
        assert not (run / "plots").exists()
        summary = (run / "summary.txt").read_text()
        line = next(l for l in summary.splitlines() if l.startswith("HCl/SO2"))
        assert line.split()[1:3] == ["odr", "6"]
        assert float(line.split()[3]) == pytest.approx(RATIO, rel=1e-5)

    def test_plotting_failure_keeps_the_results(self, tmp_path, patched, monkeypatch,
                                                caplog):
        def broken(self, state):
            raise RuntimeError("no display")
        monkeypatch.setattr(AnalysisCanvas, "update", broken)
        out = tmp_path / "runs"
        assert cli.main([str(_ini(tmp_path)), "--save-plots",
                         "--output-dir", str(out)]) == 0
        long = pd.read_csv(_run_dir(out) / "results_long.csv")
        assert long.spectrum_id.nunique() == 6
        assert "Plotting failed" in caplog.text

    def test_flags_parse(self):
        args = cli.build_parser().parse_args(["x.ini", "--plot", "--save-plots"])
        assert args.plot and args.save_plots


# =============================================================================
# M3c: stored fits and the post-fit inspector
# =============================================================================

from types import SimpleNamespace

from plumeIR.exceptions import RunFolderError
from plumeIR.output import FITS_NAME, read_fits, write_fits
from plumeIR.plotting.inspector import Inspector
from plumeIR.plotting.panels import target_name


def _inspect_run(tmp_path: Path, patched) -> Path:
    """A finished synthetic run's folder (six spectra, parallel order)."""
    out = tmp_path / "runs"
    assert cli.main([str(_ini(tmp_path, "*PLOT\nratios : HCl:SO2\nregression : odr")),
                     "--output-dir", str(out)]) == 0
    return _run_dir(out)


class TestStoredFits:

    def test_round_trip_in_input_order(self, tmp_path):
        items = [_item(i, 5) for i in (3, 0, 4, 1, 2)]
        write_fits(items, tmp_path)
        archive = read_fits(tmp_path)
        assert archive.windows == ["FIT0", "FIT1"]
        assert list(archive.spectrum_index) == [0, 1, 2, 3, 4]
        by_index = {it.index: it for it in items}
        for k in range(5):
            for fr in by_index[k].fits:
                got = archive.fit(k, fr.window_label)
                assert got.spectrum_id == fr.spectrum_id
                assert got.success == fr.success
                np.testing.assert_array_equal(got.wn, fr.wn)
                np.testing.assert_array_equal(got.measured, fr.measured)
                np.testing.assert_array_equal(got.model, fr.model)
                assert got.r2 == fr.r2
                assert got.continuum is None

    def test_continuum_and_unequal_grids(self, tmp_path):
        import dataclasses
        a, b = _item(0, 2), _item(1, 2)
        f0 = a.fits[0]
        short = dataclasses.replace(f0, wn=f0.wn[:30], measured=f0.measured[:30],
                                    model=f0.model[:30], continuum=f0.model[:30] * 0.9)
        a = SpectrumFits(0, 2, "S0", (short, a.fits[1]))
        write_fits([a, b], tmp_path)
        archive = read_fits(tmp_path)
        got = archive.fit(0, "FIT0")
        np.testing.assert_array_equal(got.wn, short.wn)
        np.testing.assert_array_equal(got.continuum, short.continuum)
        assert archive.fit(1, "FIT0").continuum is None     # a fit without one
        np.testing.assert_array_equal(archive.fit(1, "FIT0").wn, b.fits[0].wn)

    def test_failed_fit_is_flagged(self, tmp_path):
        write_fits([_item(0, 2), _item(1, 2, fail=True)], tmp_path)
        archive = read_fits(tmp_path)
        assert archive.fit(1, "FIT0").success
        assert not archive.fit(1, "FIT1").success

    def test_missing_file_names_the_problem(self, tmp_path):
        with pytest.raises(RunFolderError, match="fits.npz"):
            read_fits(tmp_path)

    @staticmethod
    def _with_cov(item, rho):
        import dataclasses
        fits = []
        for fr in item.fits:
            sd = np.array([0.01, fr.perror[fr.varied_names[1]]])
            cov = np.array([[1.0, rho], [rho, 1.0]]) * np.outer(sd, sd)
            fits.append(dataclasses.replace(fr, covariance=cov))
        return SpectrumFits(item.index, item.n_spectra, item.spectrum, tuple(fits))

    def test_covariance_round_trip(self, tmp_path):
        items = [self._with_cov(_item(i, 3), rho) for i, rho in enumerate((0.5, -0.2, 0.9))]
        items[1] = SpectrumFits(1, 3, "S1", (items[1].fits[0], _item(1, 3).fits[1]))
        write_fits(items, tmp_path)
        archive = read_fits(tmp_path)
        assert archive.has_covariance
        np.testing.assert_allclose(archive.correlation("FIT0", "poly0", "SO2"),
                                   [0.5, -0.2, 0.9])
        r = archive.correlation("FIT1", "HCl", "poly0")
        np.testing.assert_allclose(r[[0, 2]], [0.5, 0.9])
        assert np.isnan(r[1])                      # that fit has no covariance
        with pytest.raises(KeyError, match="varied"):
            archive.correlation("FIT0", "poly0", "HCl")

    def test_format_1_is_still_read(self, tmp_path):
        write_fits([_item(0, 2), _item(1, 2)], tmp_path)
        with np.load(tmp_path / FITS_NAME) as npz:
            arrays = {k: npz[k] for k in npz.files if not k.endswith((".varied", ".covariance"))}
        arrays["format"] = np.asarray(1)
        np.savez_compressed(tmp_path / FITS_NAME, **arrays)
        archive = read_fits(tmp_path)
        assert len(archive) == 2 and not archive.has_covariance
        with pytest.raises(KeyError, match="format 1"):
            archive.correlation("FIT0", "poly0", "SO2")


class TestRunStateCorrelation:
    """A ratio of two gases from one window uses their fit's error correlation."""

    def test_same_window_ratio(self):
        from plumeIR.plotting.state import fit_correlation
        from plumeIR.regression import york_fit
        w = FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0, atm_gases=(),
                            plume_gases=("SO2", "HCl"), plume_aero=(), targets=("SO2", "HCl"),
                            polydeg=0)
        ps = ParameterSet([
            Parameter("poly0", 1.0, True, (-np.inf, np.inf), ParameterKind.POLYNOMIAL),
            Parameter("SO2", 1.0, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                      ref_column=SO2_REF),
            Parameter("HCl", 1.0, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                      ref_column=HCL_REF)])
        state = RunState(["SO2", "HCl"], ratios=[("HCl", "SO2")], regression="odr",
                         windows={"FIT0": w}, params={"FIT0": ps})
        rho = -0.6
        wn = np.linspace(2400.0, 2550.0, 40)
        for i in range(8):
            so2 = _so2(i)
            hcl = RATIO * so2 + BACKGROUND + 1e15 * np.sin(3 * i)
            vals = {"poly0": 1.0, "SO2": so2 / SO2_REF, "HCl": hcl / HCL_REF}
            sd = np.array([0.01, 0.01 * vals["SO2"], 0.01 * vals["HCl"]])
            c = np.eye(3)
            c[1, 2] = c[2, 1] = rho
            fr = FitResult(success=True, method="nlls", varied_names=("poly0", "SO2", "HCl"),
                           values=vals, perror=dict(zip(vals, sd)),
                           covariance=c * np.outer(sd, sd), wn=wn, measured=wn * 0 + 1,
                           model=wn * 0 + 1, rmse=0.0, nfev=1, message="ok",
                           window_label="FIT0", spectrum_id=f"2021-08-11T10:00:{i:02d}")
            assert fit_correlation(fr, "HCl", "SO2") == pytest.approx(rho)
            state.add(SpectrumFits(i, 8, f"S{i}", (fr,)))
        np.testing.assert_allclose(state.ratio_correlation(("HCl", "SO2")), rho)
        x, y, sx, sy = state.ratio_points(("HCl", "SO2"))
        assert state.ratio_fit(("HCl", "SO2")).slope == pytest.approx(
            york_fit(x, y, sx, sy, rho).slope, rel=1e-10)


class TestRunStateFromRunDir:

    def test_cli_run_writes_fits(self, tmp_path, patched):
        run = _inspect_run(tmp_path, patched)
        assert (run / FITS_NAME).is_file()
        assert FITS_NAME in (run / "summary.txt").read_text()

    def test_matches_the_live_state(self, tmp_path, patched):
        run = _inspect_run(tmp_path, patched)
        state = RunState.from_run_dir(run)
        live = _fed(_setup(n=6, plot=PlotConfig(ratios=(("HCl", "SO2"),),
                                                regression="odr")),
                    order=(2, 0, 1, 3, 5, 4))
        for sp in ("SO2", "HCl"):
            np.testing.assert_allclose(state.column(sp)[0], live.column(sp)[0],
                                       rtol=1e-12)
        assert state.ratio_fit(("HCl", "SO2")).slope == pytest.approx(
            live.ratio_fit(("HCl", "SO2")).slope, rel=1e-10)
        assert isinstance(state.geometry, LayerGeometry)
        assert state.window_labels == ["FIT0", "FIT1"]
        assert state.selected == 0 and state.can_select

    def test_select_shows_that_spectrum(self, tmp_path, patched):
        state = RunState.from_run_dir(_inspect_run(tmp_path, patched))
        assert state.select(4) == 4
        expected = _item(4, 6)
        for fr in expected.fits:
            np.testing.assert_array_equal(state.latest_fits[fr.window_label].measured,
                                          fr.measured)
        assert state.latest_id == expected.fits[0].spectrum_id
        x, y = state.newest_point(("HCl", "SO2"))
        assert x == pytest.approx(_so2(4))
        assert y == pytest.approx(RATIO * _so2(4) + BACKGROUND)
        assert state.select(99) == 5 and state.select(-3) == 0      # clipped

    def test_nearest_in_time(self, tmp_path, patched):
        state = RunState.from_run_dir(_inspect_run(tmp_path, patched))
        t = state.times()
        assert state.nearest(t[3] + np.timedelta64(400, "ms")) == 3
        assert state.nearest(t[3] + np.timedelta64(600, "ms")) == 4

    def test_live_state_cannot_select(self):
        with pytest.raises(RuntimeError):
            _fed(_setup(n=3)).select(0)

    def test_old_folder_without_fits(self, tmp_path, patched):
        run = _inspect_run(tmp_path, patched)
        (run / FITS_NAME).unlink()
        with pytest.raises(RunFolderError, match="before runs stored their fits"):
            RunState.from_run_dir(run)

    def test_table_and_fits_disagree(self, tmp_path, patched):
        run = _inspect_run(tmp_path, patched)
        write_fits([_item(i, 6) for i in range(4)], run)
        with pytest.raises(RunFolderError, match="same spectra"):
            RunState.from_run_dir(run)

    def test_not_a_run_folder(self, tmp_path):
        with pytest.raises(RunFolderError, match="config.ini is missing"):
            RunState.from_run_dir(tmp_path)

    def test_dual_target_is_labelled_plume(self):
        state = RunState(["CO2", "SO2"], dual=["CO2"])
        assert target_name(state, "CO2").endswith("(plume)")
        assert "(plume)" not in target_name(state, "SO2")


class TestInspector:

    def test_steps_and_title(self, tmp_path, patched):
        insp = Inspector.from_run_dir(_inspect_run(tmp_path, patched),
                                      interactive=False)
        assert isinstance(insp.canvas.fig, Figure)
        assert insp.canvas._title.get_text().startswith("Spectrum 1 of 6")
        ev = lambda key: SimpleNamespace(key=key, canvas=insp.canvas.fig.canvas)
        insp._on_key(ev("right"))
        assert insp.state.selected == 1
        insp._on_key(ev("pagedown"))
        assert insp.state.selected == 5                  # clipped at the end
        insp._on_key(ev("home"))
        assert insp.state.selected == 0
        insp._on_key(ev("end"))
        assert insp.state.selected == 5
        assert insp.canvas._title.get_text().startswith("Spectrum 6 of 6")
        fit_panel = insp.canvas.panels[0]
        np.testing.assert_array_equal(fit_panel._measured.get_ydata(),
                                      _item(5, 6).fits[0].measured)
        assert insp.save(tmp_path / "inspect.png").is_file()

    def test_click_selects(self, tmp_path, patched):
        insp = Inspector.from_run_dir(_inspect_run(tmp_path, patched),
                                      interactive=False)
        insp.canvas.fig.canvas.draw()
        ts = next(p for p in insp.canvas.panels if isinstance(p, TimeSeriesPanel))
        import matplotlib.dates as mdates
        x = mdates.date2num(insp.state.times()[2].astype("datetime64[us]").item())
        assert insp.spectrum_at(ts.ax, x, 0.5) == 2
        rp = next(p for p in insp.canvas.panels if isinstance(p, RatioPanel))
        px, py, _, _ = insp.state.ratio_points(rp.pair)
        assert insp.spectrum_at(rp.ax, px[4], py[4]) == 4
        far = rp.ax.transData.inverted().transform(
            rp.ax.transData.transform([[px[4], py[4]]])[0] + [200, 200])
        assert insp.spectrum_at(rp.ax, *far) is None

    def test_needs_stored_fits(self):
        with pytest.raises(ValueError):
            Inspector(_fed(_setup(n=3)), interactive=False)


class TestCommandLineInspect:

    def test_folder_needs_inspect(self, tmp_path):
        with pytest.raises(SystemExit):
            cli.main([str(tmp_path)])

    def test_folder_refuses_run_flags(self, tmp_path):
        with pytest.raises(SystemExit):
            cli.main([str(tmp_path), "--inspect", "--parallel"])

    def test_old_folder_fails_cleanly(self, tmp_path, patched, caplog):
        run = _inspect_run(tmp_path, patched)
        (run / FITS_NAME).unlink()
        assert cli.main([str(run), "--inspect"]) == 1
        assert "fits.npz" in caplog.text

    def test_folder_opens_inspector(self, tmp_path, patched, monkeypatch):
        run = _inspect_run(tmp_path, patched)
        opened = []
        monkeypatch.setattr(Inspector, "hold", lambda self: opened.append(self.n_spectra))
        monkeypatch.setattr(Inspector, "close", lambda self: None)
        real = Inspector.__init__
        monkeypatch.setattr(Inspector, "__init__",
                            lambda self, st, **kw: real(self, st, **{**kw, "interactive": False}))
        assert cli.main([str(run), "--inspect"]) == 0
        assert opened == [6]

    def test_config_runs_then_inspects(self, tmp_path, patched, monkeypatch):
        seen = []
        monkeypatch.setattr(cli, "inspect", lambda run_dir: seen.append(run_dir) or 0)
        out = tmp_path / "runs"
        assert cli.main([str(_ini(tmp_path)), "--inspect", "--output-dir", str(out)]) == 0
        assert seen == [_run_dir(out)]
        assert (seen[0] / FITS_NAME).is_file()

    def test_no_inspect_without_flag(self, tmp_path, patched, monkeypatch):
        monkeypatch.setattr(cli, "inspect", lambda run_dir: pytest.fail("inspected"))
        assert cli.main([str(_ini(tmp_path)), "--output-dir", str(tmp_path / "r")]) == 0
