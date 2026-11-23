"""
Tests for the composition analysis (``plumeIR.analysis``): the composition
table, moving-window ratios and their plots.

Validated first on the layer validation run (2026-11-16): per-run York lines
from the composition table reproduce the recorded ratios to every printed
digit (HCl/SO2 0.0357592 ± 0.00054, CO/SO2 0.00515979, CO2/SO2 7.50307 ±
0.213); moving windows of 1-10 minutes run in 0.1 s for 418 spectra.  These
tests use a synthetic run with known answers.

Run with:  pytest test/test_analysis.py -v
"""

from __future__ import annotations

import dataclasses
from datetime import datetime, timedelta
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from plumeIR.analysis.data import (
    BACKGROUND_KEY,
    CompositionTable, ambient_column, composition_table, default_strategy,
)
from plumeIR.analysis.ratios import moving_ratio, pooled_ratio, run_ratios, window_indices
from plumeIR.config.objects import EmissionGeometry, FitWindowConfig, LayerGeometry, PlotConfig
from plumeIR.exceptions import PlumeIRError
from plumeIR.retrieval.results import ResultsTable

GEOM = LayerGeometry(pathlength=2.0, atm_temp=284.0, atm_pres=973.0,
                     plume_temp=450.0, plume_pres=973.0, plume_thickness=0.05)
WINDOWS = [
    FitWindowConfig(label="FIT0", wn_start=2450.0, wn_stop=2550.0, atm_gases=(),
                    plume_gases=("SO2",), plume_aero=(), targets=("SO2",), polydeg=2),
    FitWindowConfig(label="FIT1", wn_start=2690.0, wn_stop=2830.0, atm_gases=(),
                    plume_gases=("HCl",), plume_aero=(), targets=("HCl",), polydeg=2),
    FitWindowConfig(label="FIT3", wn_start=2020.0, wn_stop=2150.0, atm_gases=("CO2",),
                    plume_gases=("CO2",), plume_aero=(), targets=("CO2",), polydeg=2),
]
CONFIG = {"geometry": GEOM, "windows": WINDOWS, "plot": PlotConfig()}
T0 = datetime(2021, 8, 11, 1, 0, 0)
N, STEP = 300, 4.0          # spectra, seconds
GAP_AT = 200                # a 30-minute gap before this spectrum
R1, R2 = 0.03, 0.05         # HCl/SO2 before / after spectrum STEP_AT
STEP_AT = 100
HCL_OFFSET = 5e17           # a steady second source


def _times(n=N, t0=T0):
    t = [t0 + timedelta(seconds=STEP * i) for i in range(n)]
    return [ti + (timedelta(minutes=30) if i >= GAP_AT else timedelta(0))
            for i, ti in enumerate(t)]


def _results(seed=0, n=N, t0=T0, r2_low=()):
    """A long results table: SO2, HCl (ratio steps at STEP_AT), CO2 (dual)."""
    rng = np.random.default_rng(seed)
    so2 = rng.uniform(1e19, 1.5e20, n)
    ratio = np.where(np.arange(n) < STEP_AT, R1, R2)
    hcl = ratio * so2 + HCL_OFFSET
    co2 = 5.0 * so2
    co2_atm = np.full(n, 2.0e21)
    rows = []
    for i, t in enumerate(_times(n, t0)):
        sid = t.isoformat()
        r2 = 0.5 if i in r2_low else 0.99
        for w, par, sp, v, rel in (("FIT0", "SO2", "SO2", so2[i], 0.01),
                                   ("FIT1", "HCl", "HCl", hcl[i], 0.01),
                                   ("FIT3", "CO2", "CO2", co2[i], 0.02),
                                   ("FIT3", "CO2_atm", "CO2", co2_atm[i], 0.01),
                                   ("FIT0", "plume_temp", "plume_temp", 450.0, 0.01)):
            e = rel * abs(v)
            noisy = v + (rng.normal(0, e) if par in ("SO2", "HCl", "CO2") else 0.0)
            rows.append(dict(spectrum_id=sid, window=w, parameter=par, species=sp,
                             kind="GAS_PLUME", varied=True, value=noisy, error=e,
                             scd=noisy, scd_error=e, pres=973.0, temp=284.0,
                             is_target=par in ("SO2", "HCl", "CO2", "plume_temp"),
                             success=True, rmse=0.01, r2=r2))
    return ResultsTable(df=pd.DataFrame(rows))


@pytest.fixture(scope="module")
def comp():
    return composition_table(_results(), CONFIG, run="A")


class TestTable:

    def test_gases_and_provenance(self, comp):
        assert comp.gases == ["SO2", "HCl", "CO2"]          # plume_temp left out
        assert comp.window == {"SO2": "FIT0", "HCl": "FIT1", "CO2": "FIT3"}
        assert len(comp) == N and comp.time.is_monotonic_increasing

    def test_strategies_by_class(self, comp):
        # CO2 is fitted in both roles here: its target is the plume part
        assert comp.strategy == {"SO2": "zero", "HCl": "zero", "CO2": "zero"}
        assert default_strategy("CO2", GEOM, set(), None, None) == "ambient"
        assert default_strategy("H2O", GEOM, set(), None, None) == "zero"
        assert default_strategy("H2O", GEOM, set(), None, 50.0) == "ambient"
        em = EmissionGeometry.__new__(EmissionGeometry)
        assert default_strategy("CO2", em, set(), None, None) == "zero"
        assert default_strategy("CO2", object(), set(), None, None) == "none"

    def test_ambient_column(self):
        c = ambient_column("CO2", GEOM)
        assert c == pytest.approx(ambient_column("CO2", GEOM, {"CO2": 420.0}))
        assert ambient_column("CO2", GEOM, {"CO2": 840.0}) == pytest.approx(2 * c)
        assert np.isnan(ambient_column("SO2", GEOM))

    def test_live_view_uses_the_same_ambient_column(self):
        """``RunState.background_column`` (the live view's ratio panels) and the
        composition table's ``ambient`` background are one computation."""
        from plumeIR.plotting.state import RunState
        from plumeIR.units import molec_cm2_to_g_m2
        bg = {"CO2": 415.0, "CO": 0.12}
        state = RunState(["SO2", "CO2", "CO", "H2O", "HCl"], geometry=GEOM, background=bg, rh=60.0)
        for gas in ("CO2", "CO", "H2O", "HCl", "SO2", "N2O"):
            ours = ambient_column(gas, GEOM, state.background_vmr, 60.0)
            theirs = state.background_column(gas)
            assert (np.isnan(ours) and np.isnan(theirs)) or theirs == ours, gas
        assert state.background_column("CO2", mass=True) == pytest.approx(
            molec_cm2_to_g_m2(ambient_column("CO2", GEOM, bg), "CO2"))
        # a gas fitted in both roles: the target is the plume part
        assert RunState(["CO2"], geometry=GEOM, dual={"CO2"}).background_column("CO2") == 0.0
        # and the table's ambient background is the same number
        t = composition_table(_results(), {**CONFIG, "plot": PlotConfig()},
                              background={"SO2": "zero", "HCl": "zero", "CO2": "ambient"})
        default = RunState(["CO2"], geometry=GEOM)
        np.testing.assert_allclose(t.background["CO2"], default.background_column("CO2"),
                                   rtol=1e-12)

    def test_overrides(self):
        res = _results()
        t = composition_table(res, CONFIG, background={"CO2": "fitted", "HCl": "quantile"})
        np.testing.assert_allclose(t.background["CO2"], 2.0e21)
        assert t.strategy["HCl"] == "quantile"
        assert np.isfinite(t.background["HCl"]).all()
        assert (t.background["HCl"].iloc[:GAP_AT].nunique(), t.background["HCl"].iloc[GAP_AT:].nunique()) == (1, 1)
        with pytest.raises(PlumeIRError, match="unknown background"):
            composition_table(res, CONFIG, background={"SO2": "guess"})
        with pytest.raises(PlumeIRError, match="_atm"):
            composition_table(res, CONFIG, background={"SO2": "fitted"})
        with pytest.raises(PlumeIRError, match="no ambient column"):
            composition_table(res, CONFIG, background={"SO2": "ambient"})

    def test_excess(self):
        t = composition_table(_results(), CONFIG, background={"CO2": "fitted"})
        v, e = t.column("CO2", excess=True)
        np.testing.assert_allclose(v, t.raw["CO2"] - 2.0e21)
        assert (e > t.error["CO2"].to_numpy()).all()
        t2 = composition_table(_results(), CONFIG, background={"SO2": "none"})
        with pytest.raises(PlumeIRError, match="no background"):
            t2.column("SO2", excess=True)

    def test_segments_and_quality(self):
        t = composition_table(_results(r2_low=(5, 6)), CONFIG, min_r2=0.9)
        assert list(np.unique(t.segment)) == [0, 1]
        assert (t.segment[:GAP_AT] == 0).all() and (t.segment[GAP_AT:] == 1).all()
        v, _ = t.column("SO2")
        assert np.isnan(v[[5, 6]]).all() and np.isfinite(v[7])

    def test_concat_orders_and_separates_runs(self, comp):
        later = composition_table(_results(seed=1, t0=T0 + timedelta(days=1)), CONFIG, run="B")
        both = CompositionTable.concat([later, comp])
        assert len(both) == 2 * N
        assert list(pd.unique(both.run)) == ["A", "B"]
        assert len(np.unique(both.segment)) == 4


class TestRatios:

    def test_run_ratio_recovers_the_mixture(self, comp):
        # one line over a step 0.03 -> 0.05 lands between, with a large MSWD
        r = run_ratios(comp, "HCl", "SO2", by="segment")
        assert len(r) == 2
        assert r.loc[1, "slope"] == pytest.approx(R2, rel=0.02)     # after the gap: 0.05 only
        assert r.loc[1, "intercept"] == pytest.approx(HCL_OFFSET, rel=0.3)
        assert R1 < r.loc[0, "slope"] < R2 and r.loc[0, "mswd"] > 5

    def test_centred_window_follows_the_step(self, comp):
        m = moving_ratio(comp, "HCl", "SO2", 2.0).table   # 2 min = 30 spectra
        assert m.slope.iloc[20:80].median() == pytest.approx(R1, rel=0.02)
        assert m.slope.iloc[120:190].median() == pytest.approx(R2, rel=0.02)
        assert m.mswd.iloc[20:80].median() < 3                # one composition per window
        assert m.intercept.iloc[20:80].median() == pytest.approx(HCL_OFFSET, rel=0.3)

    def test_trailing_lags_by_half_a_window(self, comp):
        c = moving_ratio(comp, "HCl", "SO2", 30, unit="spectra").table.slope
        t = moving_ratio(comp, "HCl", "SO2", 30, unit="spectra", placement="trailing").table.slope
        mid = 0.5 * (R1 + R2)
        cross = lambda s: int(np.argmax(s.iloc[:GAP_AT].to_numpy() > mid))  # noqa: E731
        assert abs(cross(c) - STEP_AT) <= 3
        assert abs(cross(t) - (STEP_AT + 15)) <= 4

    def test_windows_do_not_cross_the_gap(self, comp):
        m = moving_ratio(comp, "HCl", "SO2", 60.0).table           # wider than a segment
        assert m.n.iloc[0] == GAP_AT and m.n.iloc[-1] == N - GAP_AT
        t = moving_ratio(comp, "HCl", "SO2", 10, unit="spectra", placement="trailing").table
        assert np.isnan(t.slope.iloc[GAP_AT]) and t.n.iloc[GAP_AT] == 1

    def test_background_does_not_change_the_slope(self):
        """A constant background without an error of its own moves only the
        intercept.  (One with its own error, such as ``fitted``, also changes
        the weights, and so the slope slightly.)"""
        t = composition_table(_results(), CONFIG, background={"CO2": "ambient"})
        raw = moving_ratio(t, "CO2", "SO2", 2.0).table
        exc = moving_ratio(t, "CO2", "SO2", 2.0, excess=True).table
        np.testing.assert_allclose(raw.slope, exc.slope, rtol=1e-9)
        np.testing.assert_allclose(raw.intercept - exc.intercept,
                                   ambient_column("CO2", GEOM), rtol=1e-6)

    def test_x_span_reports_leverage(self, comp):
        m = moving_ratio(comp, "HCl", "SO2", 2.0).table
        assert (m.x_span_snr.dropna() > 10).all()

    @pytest.mark.parametrize("kwargs, match", [
        (dict(width=0.0), "positive"), (dict(width=2.0, unit="hours"), "unit"),
        (dict(width=2.0, placement="ahead"), "placement"),
    ])
    def test_invalid_arguments(self, comp, kwargs, match):
        with pytest.raises(PlumeIRError, match=match):
            moving_ratio(comp, "HCl", "SO2", **kwargs)


    def test_window_indices_match_the_fitted_windows(self, comp):
        for placement in ("centred", "trailing"):
            m = moving_ratio(comp, "HCl", "SO2", 2.0, placement=placement).table
            for at in (0, 50, STEP_AT, GAP_AT - 1, GAP_AT, N - 1):
                w = window_indices(comp, at, 2.0, placement=placement)
                assert w.size == m.n.iloc[at]
                assert (comp.segment[w] == comp.segment[at]).all()
                assert at in w
        with pytest.raises(PlumeIRError, match="at must be"):
            window_indices(comp, N, 2.0)


class TestPlots:
    """Drawing into a bare ``Figure`` (Agg); no pyplot."""

    @staticmethod
    def _ax():
        from matplotlib.figure import Figure
        return Figure().add_subplot()

    def test_time_values(self, comp):
        from plumeIR.plotting.analysis import time_values
        x, per_min = time_values(comp.time, "elapsed")
        assert per_min == 1.0 and x[0] == 0.0
        assert x[1] == pytest.approx(STEP / 60.0)
        xc, per_min_c = time_values(comp.time, "clock")
        assert per_min_c == pytest.approx(1 / 1440)
        assert (xc[1] - xc[0]) / per_min_c == pytest.approx(STEP / 60.0, rel=1e-6)
        xi, none = time_values(pd.DatetimeIndex([pd.NaT, pd.NaT]), "clock")
        assert none is None and list(xi) == [0.0, 1.0]
        with pytest.raises(PlumeIRError, match="time must be"):
            time_values(comp.time, "utc")

    def test_moving_ratio_breaks_at_the_gap(self, comp):
        from plumeIR.plotting.analysis import draw_moving_ratio
        ax = self._ax()
        mr = moving_ratio(comp, "HCl", "SO2", 2.0)
        draw_moving_ratio(ax, mr, time="elapsed", reference=0.04, reference_err=0.001)
        curve = next(l for l in ax.get_lines() if l.get_label().startswith("2 min"))
        y = np.asarray(curve.get_ydata(), float)
        assert y.size == N + 1 and np.isnan(y[GAP_AT])          # one break, at the gap
        np.testing.assert_allclose(np.delete(y, GAP_AT), mr.table.slope, equal_nan=True)
        assert any(l.get_label().startswith("run 0.04") for l in ax.get_lines())
        assert "HCl" in ax.get_ylabel() and ax.get_xlabel() == "Elapsed time [min]"

    def test_window_quality(self, comp):
        from plumeIR.plotting.analysis import draw_window_quality
        mr = moving_ratio(comp, "HCl", "SO2", 2.0)
        ax = self._ax()
        draw_window_quality(ax, mr, "mswd")
        assert ax.get_yscale() == "log"
        ax = self._ax()
        draw_window_quality(ax, mr, "x_span_snr")
        assert ax.get_ylim()[0] == 0
        with pytest.raises(PlumeIRError, match="which"):
            draw_window_quality(ax, mr, "r2")

    def test_run_ratios_one_point_per_run(self, comp):
        from plumeIR.plotting.analysis import draw_run_ratios
        ax = self._ax()
        table = run_ratios(comp, "HCl", "SO2", by="segment")
        draw_run_ratios(ax, table, time="elapsed")
        line = ax.containers[0].lines[0]
        x = np.asarray(line.get_xdata(), float)
        np.testing.assert_allclose(line.get_ydata(), table.slope)
        # each point sits at the middle of its segment's span
        mid0 = 0.5 * (GAP_AT - 1) * STEP / 60.0
        assert x[0] == pytest.approx(mid0)
        assert len(ax.texts) == 2 and ax.texts[0].get_text().startswith("MSWD")

    def test_window_scatter_draws_the_fitted_line(self, comp):
        from plumeIR.plotting.analysis import draw_window_scatter
        mr = moving_ratio(comp, "HCl", "SO2", 2.0)
        at = 50
        ax = self._ax()
        draw_window_scatter(ax, comp, mr, at)
        line = next(l for l in ax.get_lines() if l.get_label().startswith("York"))
        x, y = (np.asarray(v, float) for v in line.get_data())
        row = mr.table.iloc[at]
        np.testing.assert_allclose(y, row.intercept + row.slope * x)
        other = composition_table(_results(n=N - 10), CONFIG)
        with pytest.raises(PlumeIRError, match="rows"):
            draw_window_scatter(ax, other, mr, at)

    def test_save_analysis_figures(self, comp, tmp_path):
        from plumeIR.plotting.analysis import default_pairs, save_analysis_figures
        assert default_pairs(comp) == [("HCl", "SO2"), ("CO2", "SO2")]
        with pytest.raises(PlumeIRError, match="reference"):
            default_pairs(comp, "CO")
        out = save_analysis_figures(comp, tmp_path, pairs=[("HCl", "SO2")], widths=(1.0, 2.0))
        names = sorted(p.name for p in out)
        # two segments, one run: the per-segment figure is written
        assert names == ["analysis_ratio_HCl_SO2.png", "analysis_runs.png",
                         "analysis_window_HCl_SO2.png"]
        assert all(p.stat().st_size > 0 for p in out)


@pytest.fixture(scope="module")
def two_days(comp):
    """Run A and a run B a day later."""
    later = composition_table(_results(seed=1, t0=T0 + timedelta(days=1)), CONFIG, run="B")
    return CompositionTable.concat([comp, later])


class TestRuns:
    """Comparing runs: each run's line with the spread of its window slopes;
    moving-window curves drawn per run."""

    def test_select(self, two_days):
        mr = moving_ratio(two_days, "HCl", "SO2", 2.0)
        sub = mr.select(two_days.run == "B")
        assert len(sub.table) == N and (sub.table.run == "B").all()
        pd.testing.assert_frame_equal(
            sub.table, mr.table[mr.table.run == "B"].reset_index(drop=True))
        with pytest.raises(PlumeIRError, match="mask"):
            mr.select(np.ones(3, bool))

    def test_window_spread(self, comp, two_days):
        from plumeIR.analysis.ratios import window_spread
        mr = moving_ratio(comp, "HCl", "SO2", 2.0)
        sp = window_spread(mr, by="segment")
        assert list(sp.index) == [0, 1]
        # segment 0 holds the step 0.03 -> 0.05, segment 1 only 0.05
        assert sp.loc[0, "lower"] == pytest.approx(R1, rel=0.05)
        assert sp.loc[0, "upper"] == pytest.approx(R2, rel=0.05)
        assert sp.loc[1, "upper"] - sp.loc[1, "lower"] < 0.1 * (R2 - R1)
        assert sp.loc[1, "n_windows"] == N - GAP_AT
        s = mr.table.slope.iloc[:GAP_AT]
        assert sp.loc[0, "median"] == pytest.approx(s.median())
        both = window_spread(moving_ratio(two_days, "HCl", "SO2", 2.0))
        assert list(both.index) == ["A", "B"]
        with pytest.raises(PlumeIRError, match="quantiles"):
            window_spread(mr, quantiles=(0.9, 0.1))
        with pytest.raises(PlumeIRError, match="by"):
            window_spread(mr, by="day")

    def test_run_plot_draws_the_spread(self, two_days):
        from matplotlib.figure import Figure

        from plumeIR.analysis.ratios import window_spread
        from plumeIR.plotting.analysis import draw_run_ratios
        mr = moving_ratio(two_days, "HCl", "SO2", 2.0)
        table, sp = run_ratios(two_days, "HCl", "SO2"), window_spread(mr)
        ax = Figure().add_subplot()
        draw_run_ratios(ax, table, spread=sp, spread_label="spread")
        bars = next(c for c in ax.collections if c.get_label() == "spread")
        ends = np.array([seg[:, 1] for seg in bars.get_segments()])
        np.testing.assert_allclose(ends, sp[["lower", "upper"]].to_numpy())
        with pytest.raises(PlumeIRError, match="indexed"):
            draw_run_ratios(ax, table, spread=window_spread(mr, by="segment"))

    def test_figures_per_run(self, two_days, tmp_path):
        from plumeIR.plotting.analysis import save_analysis_figures
        out = save_analysis_figures(two_days, tmp_path, pairs=[("HCl", "SO2")])
        assert sorted(p.name for p in out) == ["analysis_ratio_HCl_SO2_run01.png",
                                               "analysis_ratio_HCl_SO2_run02.png",
                                               "analysis_runs.png",
                                               "analysis_window_HCl_SO2.png"]
        with pytest.raises(PlumeIRError, match="widths"):
            save_analysis_figures(two_days, tmp_path, widths=())


class TestPooled:
    """The pooled (within-window) ratio against a background that follows
    the plume level slowly, as measured on the validation run."""

    @staticmethod
    def _drifting():
        """SO2 level rising over the run; HCl background following it."""
        res = _results(seed=3)
        df = res.df.copy()
        sids = df.spectrum_id.unique()
        level = np.linspace(1.0, 2.0, sids.size)          # slow, over the whole run
        k = dict(zip(sids, level))
        f = df.spectrum_id.map(k).to_numpy()
        so2 = df.parameter == "SO2"
        hcl = df.parameter == "HCl"
        so2_val = dict(zip(df.spectrum_id[so2], df.value[so2] * f[so2]))
        new_so2 = (df.value[so2] * f[so2]).to_numpy()
        # one ratio (R2) throughout; the HCl background follows the level
        base = df.spectrum_id[hcl].map(so2_val).to_numpy()
        new_hcl = (R2 * base + 3e18 * f[hcl]
                   + np.random.default_rng(4).normal(0, df.error[hcl].to_numpy()))
        for col in ("value", "scd"):
            df.loc[so2, col] = new_so2
            df.loc[hcl, col] = new_hcl
        return composition_table(ResultsTable(df=df), CONFIG, run="A")

    def test_pooled_follows_the_within_ratio(self):
        comp = self._drifting()
        run = run_ratios(comp, "HCl", "SO2", by="segment")
        pooled = pooled_ratio(comp, "HCl", "SO2", 1.0, by="segment")
        assert list(pooled.index) == [0, 1]
        for k in (0, 1):
            assert pooled.loc[k, "slope"] == pytest.approx(R2, rel=0.02)
            assert pooled.loc[k, "n_windows"] >= 2
        assert run.loc[0, "slope"] > R2 * 1.03                 # the single line is biased

    def test_windows_respect_min_points_and_segments(self, comp):
        p = pooled_ratio(comp, "HCl", "SO2", 2.0, by="segment")
        # 2-minute windows of 4 s spectra: 30 per window, never across the gap
        assert p.loc[0, "n_windows"] == int(np.ceil(GAP_AT / 30))
        p8 = pooled_ratio(comp, "HCl", "SO2", 10, unit="spectra", min_points=11)
        assert np.isnan(p8.iloc[0].slope) and p8.iloc[0].n_windows == 0
        with pytest.raises(PlumeIRError, match="by"):
            pooled_ratio(comp, "HCl", "SO2", 2.0, by="day")

    def test_run_plot_draws_pooled(self, two_days):
        from matplotlib.figure import Figure

        from plumeIR.plotting.analysis import draw_run_ratios
        table = run_ratios(two_days, "HCl", "SO2")
        pooled = pooled_ratio(two_days, "HCl", "SO2", 2.0)
        ax = Figure().add_subplot()
        draw_run_ratios(ax, table, pooled=pooled, pooled_label="pooled")
        c = next(c for c in ax.containers if c.get_label() == "pooled")
        np.testing.assert_allclose(c.lines[0].get_ydata(), pooled.slope)
        with pytest.raises(PlumeIRError, match="pooled"):
            draw_run_ratios(ax, table, pooled=pooled_ratio(two_days, "HCl", "SO2", 2.0,
                                                           by="segment"))


class TestComponents:
    """The weighted multi-gas component fit (validated 2026-11-16: two gases reproduce York and the pooled
    ratio to 3e-10 with χ²/dof equal to the MSWD; jackknife errors match
    Monte Carlo scatter to 0.97-1.05; two synthetic sources show as a rank-2
    drop of 1.5e5 against 398 for noise, the fitted plane holding both to
    0.07°)."""

    GASES = ["SO2", "HCl", "CO", "CO2"]
    VENT = np.array([1.0, 0.035, 0.005, 7.5])

    @classmethod
    def _table(cls, X, E):
        t = pd.DatetimeIndex(pd.Timestamp(T0) + pd.to_timedelta(np.arange(len(X)) * STEP, "s"))
        df = lambda a: pd.DataFrame(a, columns=cls.GASES)  # noqa: E731
        return CompositionTable(time=t, spectrum_id=np.array([str(x) for x in t]),
                                run=np.array(["S"] * len(X)), segment=np.zeros(len(X), int),
                                raw=df(X), error=df(E), background=df(np.zeros_like(X)),
                                background_error=df(np.zeros_like(X)),
                                valid=df(np.ones_like(X, bool)),
                                strategy={g: "zero" for g in cls.GASES},
                                window={g: "FIT" for g in cls.GASES})

    @classmethod
    def _draw(cls, seed, *, drift=0.0, second=0.0, n=300):
        rng = np.random.default_rng(seed)
        level = 1.0 + 0.4 * np.arange(n) / n
        true = np.outer(8e19 * level * rng.uniform(0.4, 1.6, n), cls.VENT)
        true += np.array([0, 4e17, 3e17, 1.27e21])
        true += np.outer(drift * (level - 1.0), [0, 1.0, 0.2, 300.0]) * 1e18
        true[:, 1] += second * rng.gamma(2.0, 0.5, n)          # an HCl-only source
        E = np.broadcast_to([1e18, 3e16, 1e17, 6.25e19], true.shape).copy()
        return cls._table(true + rng.normal(0, E), E)

    def test_two_gases_are_york(self, comp):
        from plumeIR.analysis.components import composition
        pr = pooled_ratio(comp, "HCl", "SO2", 2.0)
        win = composition(comp, ["SO2", "HCl"], intercept="window", width=2.0, jackknife=False)
        assert win.ratios.ratio["HCl"] == pytest.approx(pr.iloc[0].slope, rel=1e-8)
        assert win.fit.reduced_chi2 == pytest.approx(pr.iloc[0].mswd, rel=1e-8)
        from plumeIR.regression import york_fit
        t = self._draw(1)
        one = composition(t, ["SO2", "HCl"], intercept="group", jackknife=False)
        f = york_fit(*(t.column(g)[0] for g in ("SO2", "HCl")),
                     t.column("SO2")[1], t.column("HCl")[1])
        assert one.ratios.ratio["HCl"] == pytest.approx(f.slope, rel=1e-8)
        assert one.fit.reduced_chi2 == pytest.approx(f.mswd, rel=1e-8)

    def test_window_offsets_absorb_a_drifting_background(self):
        from plumeIR.analysis.components import composition
        t = self._draw(2, drift=1.0)
        win = composition(t, intercept="window", width=2.0)
        grp = composition(t, intercept="group")
        hcl = lambda c: c.ratios.ratio["HCl"] / self.VENT[1] - 1  # noqa: E731
        assert abs(hcl(win)) < 3 * win.ratios.ratio_err["HCl"] / self.VENT[1]
        assert hcl(grp) > 3 * grp.ratios.ratio_err["HCl"] / self.VENT[1]
        assert win.ratios.loc["SO2", "ratio"] == 1.0 and win.reference == "SO2"

    def test_rank_table_counts_sources(self):
        from plumeIR.analysis.components import rank_table
        one = rank_table(self._draw(3), intercept="window", width=2.0)
        two = rank_table(self._draw(3, second=1.5e18), intercept="window", width=2.0)
        assert list(one.index) == [0, 1, 2, 3]
        # one source: rank 2 removes about what noise would
        assert one.loc[2, "delta_chi2"] == pytest.approx(one.loc[2, "delta_dof"], rel=0.3)
        assert one.loc[1, "reduced_chi2"] == pytest.approx(1.0, abs=0.15)
        # two sources: rank 2 removes far more, and fits to the noise
        assert two.loc[2, "delta_chi2"] > 50 * two.loc[2, "delta_dof"]
        assert two.loc[2, "reduced_chi2"] == pytest.approx(1.0, abs=0.15)

    def test_rank_two_plane_holds_both_sources(self):
        from plumeIR.analysis.components import fit_components
        t = self._draw(4, second=1.5e18)
        X = t.raw.to_numpy()
        E = t.error.to_numpy()
        f = fit_components(X, E, 2, groups=np.zeros(len(X), int), intercept="group")
        s = E[0]
        Q, _ = np.linalg.qr(f.loadings / s[:, None])
        for v in (self.VENT, np.array([0.0, 1.0, 0.0, 0.0])):
            u = (v / s) / np.linalg.norm(v / s)
            assert np.linalg.norm(Q.T @ u) > np.cos(np.radians(1.0))

    def test_jackknife_error_is_realistic(self):
        from plumeIR.analysis.components import composition
        c = composition(self._draw(5), intercept="group")
        # independent errors: the jackknife sees about the formal scatter
        assert c.ratios.n_blocks.iloc[0] == 20
        err = c.ratios.ratio_err["HCl"]
        assert 2e-5 < err < 2e-4                   # Monte Carlo scatter ≈ 6e-5

    def test_refusals(self, comp):
        from plumeIR.analysis.components import composition, fit_components
        with pytest.raises(PlumeIRError, match="two species"):
            composition(comp, ["SO2"])
        with pytest.raises(PlumeIRError, match="reference"):
            composition(comp, ["SO2", "HCl"], reference="CO")
        X = np.ones((10, 3)); E = np.ones((10, 3))
        with pytest.raises(PlumeIRError, match="rank"):
            fit_components(X, E, 3)
        with pytest.raises(PlumeIRError, match="needs groups"):
            fit_components(X, E, 1)
        with pytest.raises(PlumeIRError, match="intercept"):
            fit_components(X, E, 1, intercept="day")

    def test_moving_composition_of_two_gases_is_the_moving_ratio(self, comp):
        from plumeIR.analysis.components import moving_composition
        for placement in ("centred", "trailing"):
            mc = moving_composition(comp, 2.0, ["SO2", "HCl"], placement=placement,
                                    jackknife_blocks=0)
            mr = moving_ratio(comp, "HCl", "SO2", 2.0, placement=placement).table
            v, e = mc.ratio("HCl")
            np.testing.assert_allclose(v, mr.slope, rtol=1e-7, equal_nan=True)
            np.testing.assert_allclose(mc.table.reduced_chi2, mr.mswd, rtol=1e-7,
                                       equal_nan=True)
            assert np.isnan(e).all()
        assert (mc.ratio("SO2")[0][np.isfinite(v)] == 1.0).all()
        with pytest.raises(KeyError):
            mc.ratio("CO")

    def test_moving_composition_follows_a_step(self):
        from plumeIR.analysis.components import moving_composition
        t = self._draw(6)
        X = t.raw.copy()
        X.iloc[150:, 1] *= 0.05 / 0.035          # HCl/SO2 steps up halfway (bg 4e17 kept small)
        t2 = self._table(X.to_numpy(), t.error.to_numpy())
        mc = moving_composition(t2, 2.0)
        v, e = mc.ratio("HCl")
        assert np.nanmedian(v[30:120]) == pytest.approx(0.035, rel=0.03)
        assert np.nanmedian(v[180:270]) == pytest.approx(0.05, rel=0.03)
        assert np.nanmedian(mc.ratio("CO2")[0]) == pytest.approx(7.5, rel=0.03)
        assert (e[np.isfinite(e)] > 0).all() and np.isfinite(e[30:270]).mean() > 0.95

    def test_component_plots_draw(self):
        from matplotlib.figure import Figure

        from plumeIR.analysis.components import (
            composition, moving_composition, normalised_residuals, rank_table,
        )
        from plumeIR.plotting.components import (
            draw_moving_composition, draw_rank_table, draw_ratio_estimates, draw_residuals,
        )
        t = self._draw(7, second=1.5e18)
        ax = Figure().add_subplot()
        mc = moving_composition(t, 2.0, jackknife_blocks=0)
        draw_moving_composition(ax, mc, "HCl", time="elapsed")
        np.testing.assert_allclose(ax.get_lines()[0].get_ydata(), mc.ratio("HCl")[0])
        ax = Figure().add_subplot()
        rt = rank_table(t, intercept="group")
        draw_rank_table(ax, rt)
        assert [p.get_height() for p in ax.patches] == pytest.approx(rt.delta_chi2.dropna().tolist())
        c = composition(t, intercept="group", jackknife=False)
        res = normalised_residuals(t, c)
        assert res["HCl"].std() > 3 * res["CO"].std()       # the second source is in HCl
        draw_residuals(Figure().add_subplot(), res, t.time, "HCl")
        draw_ratio_estimates(Figure().add_subplot(), {"a": (1.0, 0.1), "b": (1.1, 0.2)})

    def test_fractions_sum_to_one_with_errors(self):
        from plumeIR.analysis.components import composition, moving_composition
        t = self._draw(8)
        c = composition(t, intercept="window", width=2.0)
        assert c.ratios.fraction.sum() == pytest.approx(1.0, rel=1e-12)
        true = self.VENT / self.VENT.sum()
        np.testing.assert_allclose(c.ratios.fraction.to_numpy(), true, rtol=0.03)
        assert (c.ratios.fraction_err > 0).all()
        # fraction errors come from the copies, not the ratios: SO2's ratio has
        # no error (it is the reference), its fraction does
        assert np.isclose(c.ratios.ratio_err["SO2"], 0) and c.ratios.fraction_err["SO2"] > 0
        mc = moving_composition(t, 2.0)
        F = np.column_stack([mc.fraction(g)[0] for g in self.GASES])
        ok = np.isfinite(F).all(axis=1)
        np.testing.assert_allclose(F[ok].sum(axis=1), 1.0, rtol=1e-12)
        assert np.isfinite(mc.fraction("CO2")[1][ok]).mean() > 0.95

    def test_fraction_plots_draw(self):
        from matplotlib.figure import Figure

        from plumeIR.analysis.components import composition, moving_composition
        from plumeIR.plotting.components import draw_stacked_fractions
        t = self._draw(9)
        mc = moving_composition(t, 2.0, jackknife_blocks=0)
        ax = Figure().add_subplot()
        draw_stacked_fractions(ax, mc)
        assert ax.get_yscale() == "log" and ax.get_ylim()[1] == 1.0
        # bottom to top by median fraction: CO, HCl, SO2, CO2
        assert [h.get_label() for h in ax.collections] == ["$CO$", "$HCl$", "$SO_2$", "$CO_2$"]
        ax = Figure().add_subplot()
        draw_stacked_fractions(ax, mc, log=False, order=["SO2", "HCl", "CO", "CO2"])
        assert ax.get_ylim() == (0.0, 1.0)

    def test_minor_gases_split_at_the_largest_gap(self):
        from plumeIR.plotting.components import minor_gases
        f = pd.Series({"H2O": 0.909, "CO2": 0.0764, "SO2": 0.0138, "HF": 5.8e-4,
                       "HCl": 5.1e-4, "CO": 9.0e-5})
        assert minor_gases(f) == ["HF", "HCl", "CO"]
        assert minor_gases(f, 0.05) == ["SO2", "HF", "HCl", "CO"]
        assert minor_gases(f, 1e-4) == []                    # one gas cannot be "other"
        assert minor_gases(f[["H2O", "CO2"]]) == []
        # at least two go into "other": the c-d gap (4 decades) is not
        # allowed, so the split falls at the largest remaining one (a-b)
        assert minor_gases(pd.Series({"a": 0.9, "b": 0.09, "c": 0.0099, "d": 1e-6})) \
            == ["b", "c", "d"]

    def test_donut_labels_every_gas(self):
        from matplotlib.figure import Figure

        from plumeIR.analysis.components import composition
        from plumeIR.plotting.components import draw_composition_donut
        t = self._draw(10)
        c = composition(t, intercept="window", width=2.0, jackknife=False)
        ax = Figure().add_subplot()
        draw_composition_donut(ax, c, legend=True)
        # outer ring: CO2, SO2, other (HCl + CO); inner ring: HCl, CO
        assert len(ax.patches) == 5
        other = c.ratios.fraction[["HCl", "CO"]].sum()
        assert ax.patches[2].theta2 - ax.patches[2].theta1 == pytest.approx(360 * other)
        inner = ax.patches[3:]
        share = c.ratios.fraction["HCl"] / other
        assert inner[0].theta2 - inner[0].theta1 == pytest.approx(360 * share)
        text = " ".join(x.get_text() for x in ax.texts).replace("\n", " ")
        for gas, name in (("CO2", "$CO_2$"), ("SO2", "$SO_2$"), ("HCl", "$HCl$"), ("CO", "$CO$")):
            assert name in text, gas                     # every gas is labelled
            assert f"{100 * c.ratios.fraction[gas]:.3g}%" in text
        assert "other" in text
        assert len(ax.get_legend().get_texts()) == 4
        # nothing to split: one ring, no "other"
        ax = Figure().add_subplot()
        draw_composition_donut(ax, c, threshold=1e-9)
        assert len(ax.patches) == 4 and "other" not in " ".join(x.get_text() for x in ax.texts)

    def test_donut_skips_a_negative_fraction(self, caplog):
        """A species that falls as the plume rises has no slice, but is named."""
        import dataclasses
        import logging

        from matplotlib.figure import Figure

        from plumeIR.analysis.components import composition
        from plumeIR.plotting.components import draw_composition_donut
        c = composition(self._draw(10), intercept="window", width=2.0, jackknife=False)
        ratios = c.ratios.copy()
        ratios.loc["CO", "fraction"] = -abs(ratios.loc["CO", "fraction"])
        c = dataclasses.replace(c, ratios=ratios)
        ax = Figure().add_subplot()
        with caplog.at_level(logging.WARNING, logger="plumeIR.plotting.components"):
            draw_composition_donut(ax, c, threshold=1e-9)
        assert len(ax.patches) == 3                      # CO2, SO2, HCl; no CO slice
        text = " ".join(x.get_text() for x in ax.texts)
        assert "not drawn" in text and "$CO$" in text
        assert "CO not drawn" in caplog.text


class TestReport:
    """The analysis of finished runs as tables, summary, figures and the
    ``plumeIR analyse`` command (checked 2026-11-16 on the layer validation
    and example runs: 7 s for one run, 12 s for two, figures read)."""

    @staticmethod
    def _analysis(seed=11, label="runA", t0=T0):
        from plumeIR.analysis.report import AnalysisSettings, analyse_table
        t = TestComponents._draw(seed)
        t = CompositionTable(**{**t.__dict__,
                                "time": t.time - t.time[0] + pd.Timestamp(t0),
                                "run": np.array([label] * len(t))})
        return analyse_table(t, label, Path("/nowhere") / label, AnalysisSettings())

    def test_analyse_table(self):
        a = self._analysis()
        assert a.reference == "SO2" and a.gases == TestComponents.GASES
        r = a.ratios
        assert np.isnan(r.loc["SO2", "pooled"]) and r.fraction.sum() == pytest.approx(1.0)
        # each column is the function it names
        assert r.loc["HCl", "pooled"] == pytest.approx(
            pooled_ratio(a.comp, "HCl", "SO2", 2.0).iloc[0].slope)
        assert r.loc["HCl", "run_line"] == pytest.approx(
            run_ratios(a.comp, "HCl", "SO2").iloc[0].slope)
        assert r.loc["HCl", "all_gas"] == pytest.approx(0.035, rel=0.03)
        assert r.loc["HCl", "window_p05"] < r.loc["HCl", "window_median"] < r.loc["HCl", "window_p95"]

    def test_analyse_table_refuses(self):
        from plumeIR.analysis.report import AnalysisSettings, analyse_table
        t = TestComponents._draw(12)
        with pytest.raises(PlumeIRError, match="are not targets"):
            analyse_table(t, "x", Path("."), AnalysisSettings(gases=("SO2", "HF")))
        with pytest.raises(PlumeIRError, match="reference"):
            analyse_table(t, "x", Path("."), AnalysisSettings(gases=("SO2", "HCl"),
                                                              reference="CO"))
        with pytest.raises(PlumeIRError, match="at least two"):
            analyse_table(t, "x", Path("."), AnalysisSettings(gases=("SO2",)))

    def test_exclude_leaves_a_species_out(self, caplog):
        """--exclude names what is left out; nothing is inferred (2026-11-19)."""
        import logging

        from plumeIR.analysis.cli import build_parser
        from plumeIR.analysis.report import AnalysisSettings, analyse_table
        t = TestComponents._draw(12)
        a = analyse_table(t, "x", Path("."), AnalysisSettings(exclude=("CO",)))
        assert "CO" not in a.gases and "CO" not in a.ratios.index
        assert a.ratios.fraction.sum() == pytest.approx(1.0)
        # absent from this run's targets (a comparison of several): a warning
        with caplog.at_level(logging.WARNING, logger="plumeIR.analysis.report"):
            a = analyse_table(t, "x", Path("."), AnalysisSettings(exclude=("WATER",)))
        assert a.gases == TestComponents.GASES and "excluded ['WATER']" in caplog.text
        args = build_parser().parse_args(["run", "--exclude", "WATER", "ICE"])
        assert tuple(args.exclude) == ("WATER", "ICE")
        assert build_parser().parse_args(["run"]).exclude == ()

    def test_write_and_figures(self, tmp_path):
        from plumeIR.analysis.report import (
            AnalysisSettings, analysis_summary, write_analysis,
        )
        from plumeIR.plotting.analysis_report import save_composition_figures
        one = [self._analysis()]
        s = AnalysisSettings()
        paths = write_analysis(one, tmp_path / "one", s, {"plumeIR": "test"})
        assert sorted(p.name for p in paths) == ["analysis.txt", "moving_composition.csv",
                                                 "rank.csv", "ratios.csv"]
        ratios = pd.read_csv(tmp_path / "one" / "ratios.csv", index_col=[0, 1])
        assert ratios.loc[("runA", "HCl"), "pooled"] == pytest.approx(
            one[0].ratios.loc["HCl", "pooled"], rel=1e-5)
        text = analysis_summary(one, s)
        assert "no H2O target: dry gases" in text and "pooled" in text
        figs = save_composition_figures(one, tmp_path / "one", s)
        names = {p.name for p in figs}
        assert {"composition.png", "rank.png", "analysis_ratio_HCl_SO2.png"} <= names
        assert "composition_runs.png" not in names
        # two runs, overlapping in time: one subfolder each, compared in run order
        two = [self._analysis(11, "runA"), self._analysis(13, "runB")]
        figs = save_composition_figures(two, tmp_path / "two", s)
        rel = {str(p.relative_to(tmp_path / "two")) for p in figs}
        assert {"runA/composition.png", "runB/composition.png", "composition_runs.png",
                "ratios_runs.png"} <= rel

    def test_cli(self, tmp_path, monkeypatch, capsys):
        import plumeIR.analysis.cli as cli
        from plumeIR.__main__ import main as plumeir_main
        runs = [tmp_path / "runA", tmp_path / "runB"]
        for r in runs:
            r.mkdir()
        monkeypatch.setattr(cli, "analyse_runs", lambda dirs, settings: [
            self._analysis(11 + 2 * i, Path(d).name) for i, d in enumerate(dirs)])
        # one run: written to RUN/analysis; dispatched from the plumeIR command
        assert plumeir_main(["analyse", str(runs[0]), "--no-plots"]) == 0
        assert (runs[0] / "analysis" / "analysis.txt").is_file()
        assert "plumeIR composition analysis" in capsys.readouterr().out
        # several runs need --output-dir
        with pytest.raises(SystemExit):
            cli.main([str(r) for r in runs])
        assert cli.main([*map(str, runs), "--output-dir", str(tmp_path / "out"),
                         "--no-plots"]) == 0
        assert (tmp_path / "out" / "ratios.csv").is_file()
        for bad in (["--width", "0", str(runs[0])], [str(tmp_path / "missing")]):
            with pytest.raises(SystemExit):
                cli.main(bad)


class TestCumulativeFractions:
    """Edges of the stacked fractions and their errors, from the jackknife
    copies (2026-11-16)."""

    def test_cumulative_errors_from_the_copies(self):
        from plumeIR.analysis.components import moving_composition
        t = TestComponents._draw(14)
        mc = moving_composition(t, 2.0)
        assert mc.replicates.shape == (len(t), 8, 4)
        order = ["CO", "HCl", "SO2", "CO2"]
        cum, err = mc.cumulative_fraction(order)
        ok = np.isfinite(err[:, 0])
        assert ok.mean() > 0.95
        f, e = mc.fraction("CO")
        np.testing.assert_allclose(cum[:, 0], f, equal_nan=True)
        np.testing.assert_allclose(err[ok, 0], e[ok], rtol=1e-10)    # the bottom edge is CO alone
        np.testing.assert_allclose(cum[ok, -1], 1.0, rtol=1e-12)
        assert np.nanmax(err[:, -1]) < 1e-12                          # the top edge is 1
        # correlated, not added: CO+HCl's error is not the sum of their errors
        _, e_hcl = mc.fraction("HCl")
        assert not np.allclose(err[ok, 1], (e + e_hcl)[ok])
        with pytest.raises(KeyError):
            mc.cumulative_fraction(["CO", "HCl"])
        assert moving_composition(t, 2.0, jackknife_blocks=0).replicates is None

    def test_stacked_plot_draws_edges_and_strips(self):
        from matplotlib.figure import Figure

        from plumeIR.analysis.components import moving_composition
        from plumeIR.plotting.components import STACK_ALPHA, draw_stacked_fractions
        t = TestComponents._draw(15)
        ax = Figure().add_subplot()
        draw_stacked_fractions(ax, moving_composition(t, 2.0))
        bands = [c for c in ax.collections if c.get_label() and not c.get_label().startswith("_")]
        assert len(bands) == 4 and all(b.get_alpha() == STACK_ALPHA for b in bands)
        assert len(ax.collections) == 4 + 3 and len(ax.get_lines()) == 3   # 3 inner edges
        assert all(line.get_color() == "white" for line in ax.get_lines())   # over the strips
        assert all(line.get_zorder() > c.get_zorder() for line in ax.get_lines()
                   for c in ax.collections)
        ax = Figure().add_subplot()
        draw_stacked_fractions(ax, moving_composition(t, 2.0, jackknife_blocks=0), alpha=1.0)
        assert len(ax.collections) == 4                                    # no strips


class TestHonestErrors:
    """Block-jackknife errors of the run line and the pooled ratio
    (validated 2026-11-16: calibrated on synthetic
    data, error / scatter 0.94-1.00 with independent or AR(1) noise; on the
    validation run 1.4-1.9× York's)."""

    def test_columns_and_quoted_error(self, comp):
        from plumeIR.analysis.ratios import quoted_error
        rr = run_ratios(comp, "HCl", "SO2")              # one run, 11 two-minute blocks
        assert {"slope_err_jk", "n_blocks"} <= set(rr.columns)
        assert rr.n_blocks.iloc[0] == 11 and np.isfinite(rr.slope_err_jk).all()
        np.testing.assert_allclose(quoted_error(rr), rr.slope_err_jk)
        seg = run_ratios(comp, "HCl", "SO2", by="segment")   # the second segment: 4 blocks
        assert list(seg.n_blocks) == [7, 4] and np.isnan(seg.slope_err_jk.iloc[1])
        assert quoted_error(seg)[1] == seg.slope_err.iloc[1]
        none = run_ratios(comp, "HCl", "SO2", jackknife_width=None)
        assert np.isnan(none.slope_err_jk).all()
        np.testing.assert_allclose(quoted_error(none), none.slope_err)     # falls back to York
        pr = pooled_ratio(comp, "HCl", "SO2", 2.0)
        assert np.isfinite(pr.slope_err_jk).all()
        # the York columns are unchanged: the validation record still holds
        np.testing.assert_allclose(none.slope, rr.slope)

    def test_too_few_blocks(self, comp):
        rr = run_ratios(comp, "HCl", "SO2", jackknife_width=20.0)     # 1-2 blocks per segment
        assert np.isnan(rr.slope_err_jk).all() and (rr.n_blocks < 5).all()

    def test_jackknife_is_calibrated_on_independent_noise(self):
        rng = np.random.default_rng(21)
        t = TestComponents._draw(16)
        x_true = t.raw["SO2"].to_numpy()
        sx, sy = t.error["SO2"].to_numpy(), t.error["HCl"].to_numpy()
        slopes, jk = [], []
        for _ in range(150):
            X = t.raw.copy()
            X["SO2"] = x_true + rng.normal(0, sx)
            X["HCl"] = 0.035 * x_true + 4e17 + rng.normal(0, sy)
            tt = TestComponents._table(X.to_numpy(), t.error.to_numpy())
            r = run_ratios(tt, "HCl", "SO2").iloc[0]
            slopes.append(r.slope)
            jk.append(r.slope_err_jk)
        assert np.median(jk) / np.std(slopes) == pytest.approx(1.0, abs=0.2)

    def test_one_gas_with_aerosols_is_analysed_by_mass(self, monkeypatch, tmp_path):
        """An emission-like run (SO2 plus an aerosol, 2026-11-16): ratios and
        fractions by mass; no molar fractions; an aerosol-only table fails."""
        import types

        import plumeIR.output as output
        from plumeIR.analysis.report import AnalysisSettings, analyse_runs, analysis_summary
        df = _results().df
        so2 = df[df.parameter == "SO2"]
        noise = 1 + 0.02 * np.random.default_rng(5).normal(size=len(so2))
        aero = so2.assign(parameter="H2SO4_aq", species="H2SO4_aq", kind="AERO_PLUME",
                          scd=so2.scd * 1e-19 * noise, scd_error=so2.scd_error * 2e-19,
                          value=so2.value * 1e-19 * noise, error=so2.error * 2e-19)
        wt = so2.assign(parameter="wt_H2SO4_aq", species="H2SO4_aq", kind="AERO_PLUME",
                        value=0.65, error=np.nan, scd=np.nan, scd_error=np.nan,
                        is_target=False, varied=False)
        table = ResultsTable(df=pd.concat([so2, aero, wt], ignore_index=True))
        (tmp_path / "em").mkdir()
        monkeypatch.setattr(output, "load_run", lambda d, **kw: types.SimpleNamespace(
            run_dir=Path(d), table=table, config=CONFIG))
        (a,) = analyse_runs([tmp_path / "em"])
        # the acid as moles: 0.65 of the particle mass, over the molar mass of H2SO4
        from plumeIR import species
        expect = species.component_columns("H2SO4_aq", aero.scd.to_numpy(), 0.65)["H2SO4"]
        np.testing.assert_allclose(np.sort(a.comp.raw["H2SO4_aq"].to_numpy()), np.sort(expect),
                                   rtol=1e-12)
        assert a.molar_gases == ["SO2"] and a.aerosols == ["H2SO4_aq"]
        assert a.ratios.loc["H2SO4_aq", "unit"] == "mol/mol"          # acid moles
        text = analysis_summary([a], AnalysisSettings())
        assert "mass fraction (of every" in text and "molar fraction (of" not in text
        monkeypatch.setattr(output, "load_run", lambda d, **kw: types.SimpleNamespace(
            run_dir=Path(d), table=ResultsTable(df=aero), config=CONFIG))
        with pytest.raises(PlumeIRError, match="at least two species"):
            analyse_runs([tmp_path / "em"])



class TestMass:
    """Aerosols (g m⁻²) beside gases: mass ratios, mass fractions
    (2026-11-16; checked on the example run, H2SO4/SO2 0.0062 g/g by every
    estimator, the gas ratios moved ≤ 0.1% by adding it)."""

    H2SO4_PER_SO2 = 0.02            # g g⁻¹

    @classmethod
    def _table(cls, seed=31):
        from plumeIR.analysis.data import mass_factor
        t = TestComponents._draw(seed)
        cols = [*TestComponents.GASES, "ASH"]
        X, E = t.raw.copy(), t.error.copy()
        so2_mass = X["SO2"].to_numpy() * mass_factor("SO2")
        rng = np.random.default_rng(seed)
        e = 0.01 * np.median(so2_mass) * cls.H2SO4_PER_SO2
        X["ASH"] = cls.H2SO4_PER_SO2 * so2_mass + rng.normal(0, e, len(X))
        E["ASH"] = e
        df = lambda a: pd.DataFrame(a, columns=cols)  # noqa: E731
        n = len(X)
        return CompositionTable(
            time=t.time, spectrum_id=t.spectrum_id, run=t.run, segment=t.segment,
            raw=df(X[cols].to_numpy()), error=df(E[cols].to_numpy()),
            background=df(np.zeros((n, 5))), background_error=df(np.zeros((n, 5))),
            valid=df(np.ones((n, 5), bool)), strategy={g: "zero" for g in cols},
            window={g: "FIT" for g in cols})

    def test_species_and_mass_columns(self):
        from plumeIR.analysis.data import mass_factor
        t = self._table()
        assert t.gases == TestComponents.GASES and t.aerosols == ["ASH"]
        assert t.species == [*TestComponents.GASES, "ASH"]
        v, _ = t.column("SO2", mass=True)
        np.testing.assert_allclose(v, t.raw["SO2"] * mass_factor("SO2"))
        np.testing.assert_allclose(t.column("ASH", mass=True)[0], t.column("ASH")[0])
        assert mass_factor("SO2") == pytest.approx(64.066 * 1e4 / 6.02214076e23, rel=1e-3)

    def test_mass_ratio_is_automatic_for_aerosols(self):
        from plumeIR.analysis.ratios import mass_ratio
        t = self._table()
        assert mass_ratio(t, "ASH", "SO2") and not mass_ratio(t, "HCl", "SO2")
        p = pooled_ratio(t, "ASH", "SO2", 2.0).iloc[0]
        assert p.slope == pytest.approx(self.H2SO4_PER_SO2, rel=0.02)
        m = moving_ratio(t, "ASH", "SO2", 2.0)
        assert m.mass and np.nanmedian(m.table.slope) == pytest.approx(self.H2SO4_PER_SO2,
                                                                         rel=0.03)

    def test_mass_fractions(self):
        from plumeIR.analysis.components import composition, moving_composition
        from plumeIR.analysis.data import mass_factor
        t = self._table()
        c = composition(t, intercept="window", width=2.0)
        r = c.ratios
        assert r.loc["ASH", "unit"] == "g/g" and r.loc["HCl", "unit"] == "mol/mol"
        assert r.loc["ASH", "ratio"] == pytest.approx(self.H2SO4_PER_SO2, rel=0.03)
        assert np.isnan(r.loc["ASH", "fraction"])
        assert r.fraction.sum() == pytest.approx(1.0)
        assert r.mass_fraction.sum() == pytest.approx(1.0)
        m = TestComponents.VENT * np.array([mass_factor(g) for g in TestComponents.GASES])
        m = np.append(m, self.H2SO4_PER_SO2 * m[0])
        np.testing.assert_allclose(r.mass_fraction, m / m.sum(), rtol=0.05)
        assert (r.mass_fraction_err > 0).all()
        mc = moving_composition(t, 2.0)
        assert mc.molar_species == TestComponents.GASES
        order = ["ASH", "CO", "HCl", "SO2", "CO2"]
        cum, _ = mc.cumulative_fraction(order, mass=True)
        ok = np.isfinite(cum[:, -1])
        np.testing.assert_allclose(cum[ok, -1], 1.0)
        with pytest.raises(KeyError):
            mc.cumulative_fraction(order)                    # molar: gases only

    def test_mass_plots(self):
        from matplotlib.figure import Figure

        from plumeIR.analysis.components import composition, moving_composition
        from plumeIR.plotting.components import draw_composition_donut, draw_stacked_fractions
        t = self._table()
        c = composition(t, intercept="window", width=2.0, jackknife=False)
        ax = Figure().add_subplot()
        draw_composition_donut(ax, c, mass=True)
        n_mass = len(ax.patches)
        ax = Figure().add_subplot()
        draw_composition_donut(ax, c)                     # molar: the aerosol is not in it
        assert n_mass == len(ax.patches) + 1
        mc = moving_composition(t, 2.0, jackknife_blocks=0)
        ax = Figure().add_subplot()
        draw_stacked_fractions(ax, mc, mass=True)
        bands = [b for b in ax.collections if not b.get_label().startswith("_")]
        assert ax.get_ylabel().startswith("Mass") and len(bands) == 5


class TestCorrelatedErrors:
    """Error correlation of species fitted in one window (validated 2026-11-18:
    the general component fit equals the diagonal one to 2e-12 at negligible
    correlation and York with per-point ρ to 5e-10; with ρ = ±0.7 it is
    unbiased and its χ²/dof is 1.00)."""

    GASES = ["SO2", "CO", "CO2"]

    @classmethod
    def _table(cls, seed=5, n=240, rho=-0.7, strategy="zero"):
        """CO and CO2 in one window, errors correlated at ``rho``; SO2 apart."""
        rng = np.random.default_rng(seed)
        t = pd.DatetimeIndex(pd.Timestamp(T0) + pd.to_timedelta(np.arange(n) * STEP, "s"))
        so2 = 8e19 * rng.uniform(0.3, 1.7, n)
        true = np.column_stack([so2, 0.005 * so2 + 3e17, 7.5 * so2 + 1.27e21])
        E = np.broadcast_to([1e18, 1e17, 6.25e19], true.shape).copy()
        C = np.array([[1, 0, 0], [0, 1, rho], [0, rho, 1]])
        L = np.linalg.cholesky(C * np.outer(E[0], E[0]))
        X = true + rng.standard_normal((n, 3)) @ L.T
        df = lambda a: pd.DataFrame(a, columns=cls.GASES)  # noqa: E731
        bkg = np.zeros_like(X) if strategy == "zero" else np.tile([0, 3e17, 1.27e21], (n, 1))
        return CompositionTable(
            time=t, spectrum_id=np.array([str(x) for x in t]), run=np.array(["S"] * n),
            segment=np.zeros(n, int), raw=df(X), error=df(E), background=df(bkg),
            background_error=df(np.where(bkg > 0, 0.5 * E, 0.0)),
            valid=df(np.ones_like(X, bool)), strategy={g: strategy for g in cls.GASES},
            window={"SO2": "FIT0", "CO": "FIT3", "CO2": "FIT3"},
            correlation={("CO", "CO2"): np.full(n, rho)})

    def test_error_correlation(self):
        t = self._table()
        np.testing.assert_allclose(t.error_correlation("CO", "CO2"), -0.7)
        np.testing.assert_allclose(t.error_correlation("CO2", "CO"), -0.7)
        assert not np.any(t.error_correlation("SO2", "CO"))
        # an independent background (σ_b = σ/2) dilutes it: −0.7 / (1 + 1/4)
        t = self._table(strategy="quantile")
        np.testing.assert_allclose(t.error_correlation("CO", "CO2", excess=True), -0.7 / 1.25)

    def test_fitted_background_enters_with_its_correlation(self):
        n = 50
        t = self._table(n=n)
        t = dataclasses.replace(
            t, strategy={**t.strategy, "CO2": "fitted"},
            background_error=t.background_error.assign(CO2=3e19),
            correlation={("CO", "CO2"): np.full(n, 0.2),
                         ("CO2", "CO2" + BACKGROUND_KEY): np.full(n, -0.6),
                         ("CO", "CO2" + BACKGROUND_KEY): np.full(n, 0.1)})
        e, b = 6.25e19, 3e19
        _, s = t.column("CO2", excess=True)
        np.testing.assert_allclose(s, np.sqrt(e ** 2 + b ** 2 + 2 * 0.6 * e * b))
        expect = (0.2 * 1e17 * e - 0.1 * 1e17 * b) / (1e17 * s)
        np.testing.assert_allclose(t.error_correlation("CO", "CO2", excess=True), expect)

    def test_concat_keeps_and_pads(self):
        a = self._table(n=40)
        b = self._table(seed=6, n=30)
        b = dataclasses.replace(b, correlation={}, time=b.time + pd.Timedelta(hours=1))
        both = CompositionTable.concat([a, b])
        r = both.error_correlation("CO", "CO2")
        assert np.sum(r == -0.7) == 40 and np.sum(r == 0) == 30

    def test_ratios_use_it_and_r_overrides(self):
        from plumeIR.regression import york_fit
        t = self._table()
        x, sx = t.column("CO")
        y, sy = t.column("CO2")
        line = run_ratios(t, "CO2", "CO", jackknife_width=None).iloc[0]
        assert line.slope == pytest.approx(york_fit(x, y, sx, sy, -0.7).slope, rel=1e-10)
        zero = run_ratios(t, "CO2", "CO", jackknife_width=None, r=0.0).iloc[0]
        assert zero.slope == pytest.approx(york_fit(x, y, sx, sy).slope, rel=1e-10)
        assert zero.slope != pytest.approx(line.slope, rel=1e-6)
        mr = moving_ratio(t, "CO2", "CO", 5.0)
        assert np.isfinite(mr.table.slope).all()

    def test_components_match_york_with_correlation(self):
        from plumeIR.analysis.components import composition
        t = self._table()
        pr = pooled_ratio(t, "CO2", "CO", 2.0)
        c = composition(t, ["CO", "CO2"], reference="CO", intercept="window", width=2.0,
                        jackknife=False)
        assert c.ratios.ratio["CO2"] == pytest.approx(pr.iloc[0].slope, rel=1e-8)
        assert c.fit.reduced_chi2 == pytest.approx(pr.iloc[0].mswd, rel=1e-8)

    def test_negligible_correlation_is_the_diagonal_fit(self):
        from plumeIR.analysis.components import fit_components
        t = self._table(rho=0.0)
        X = np.column_stack([t.column(g)[0] for g in self.GASES])
        E = np.column_stack([t.column(g)[1] for g in self.GASES])
        C = np.tile(np.eye(3), (len(X), 1, 1))
        C[:, 1, 2] = C[:, 2, 1] = 1e-12
        g = np.arange(len(X)) // 20
        for rank in (1, 2):
            a = fit_components(X, E, rank, groups=g)
            b = fit_components(X, E, rank, groups=g, corr=C)
            assert b.chi2 == pytest.approx(a.chi2, rel=1e-9)
            pa, pb = (f.loadings @ np.linalg.pinv(f.loadings) for f in (a, b))
            np.testing.assert_allclose(pa, pb, atol=1e-8)

    def test_rank_table_and_moving_composition_run(self):
        from plumeIR.analysis.components import moving_composition, rank_table
        t = self._table()
        rk = rank_table(t, intercept="window", width=2.0)
        assert (rk.chi2.diff().dropna() < 0).all()
        assert rk.loc[1, "reduced_chi2"] == pytest.approx(1.0, abs=0.25)
        mc = moving_composition(t, 5.0, jackknife_blocks=0)
        assert np.isfinite(mc.table["CO2"]).mean() > 0.9
