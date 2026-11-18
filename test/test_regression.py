"""
Tests for plumeIR.regression.york_fit (York et al. 2004).

The published case is Pearson's data with York's weights, results from
Cantrell (2008), ACP 8, 5477, Table 2 (Williamson-York row): slope
-0.48053 ± 0.0706, intercept 5.4799 ± 0.359.  Those errors are scaled by
sqrt(S/(n-2)) (Cantrell, eq. 6).
"""

from __future__ import annotations

import warnings

import numpy as np
import pytest

from plumeIR.regression import LineFit, RegressionError, york_fit, york_fit_grouped

# Cantrell (2008), Table 1.
PX = np.array([0.0, 0.9, 1.8, 2.6, 3.3, 4.4, 5.2, 6.1, 6.5, 7.4])
PY = np.array([5.9, 5.4, 4.4, 4.6, 3.5, 3.7, 2.8, 2.8, 2.4, 1.5])
WX = np.array([1000.0, 1000.0, 500.0, 800.0, 200.0, 80.0, 60.0, 20.0, 1.8, 1.0])
WY = np.array([1.0, 1.8, 4.0, 8.0, 20.0, 20.0, 70.0, 70.0, 100.0, 500.0])


def _objective(b, a, x, y, sx, sy):
    return np.sum((y - b * x - a) ** 2 / (sy ** 2 + b * b * sx ** 2))


class TestPublishedCase:

    def test_pearson_york(self):
        f = york_fit(PX, PY, 1 / np.sqrt(WX), 1 / np.sqrt(WY))
        assert round(f.slope, 5) == -0.48053
        assert round(f.intercept, 4) == 5.4799
        assert round(f.slope_err, 4) == 0.0706
        assert round(f.intercept_err, 3) == 0.359
        assert f.mswd > 1          # so the quoted errors are the scaled ones
        assert f.slope_err == pytest.approx(f.slope_err_internal * np.sqrt(f.mswd))

    def test_symmetric_in_x_and_y(self):
        f = york_fit(PX, PY, 1 / np.sqrt(WX), 1 / np.sqrt(WY))
        g = york_fit(PY, PX, 1 / np.sqrt(WY), 1 / np.sqrt(WX))
        assert 1 / g.slope == pytest.approx(f.slope, rel=1e-12)

    def test_minimises_the_odr_objective(self):
        sx, sy = 1 / np.sqrt(WX), 1 / np.sqrt(WY)
        f = york_fit(PX, PY, sx, sy)
        s0 = _objective(f.slope, f.intercept, PX, PY, sx, sy)
        for db in (-1e-6, 1e-6):
            for da in (-1e-6, 0.0, 1e-6):
                assert _objective(f.slope + db, f.intercept + da, PX, PY, sx, sy) > s0


class TestAgainstScipyOdr:
    """scipy.odr while it exists (removed in SciPy 1.19)."""

    @pytest.fixture
    def odr(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            return pytest.importorskip("scipy.odr")

    def _ref(self, odr, x, y, sx=None, sy=None):
        data = odr.RealData(x, y, sx=sx, sy=sy) if sx is not None else odr.RealData(x, y)
        # ODRPACK stops at sqrt(eps) by default; tightened, it lands on York.
        return odr.ODR(data, odr.unilinear, beta0=np.polyfit(x, y, 1),
                       sstol=1e-15, partol=1e-15, maxit=1000).run()

    def test_weighted_column_magnitudes(self, odr):
        rng = np.random.default_rng(1)
        x = rng.uniform(1e17, 3e18, 200)
        y = 0.036 * x + 6e17 + rng.normal(0, 2e16, 200)
        sx = 5e16 * rng.uniform(0.5, 2, 200)
        sy = 2e15 * rng.uniform(0.5, 2, 200)
        f, o = york_fit(x, y, sx, sy), self._ref(odr, x, y, sx, sy)
        assert f.mswd > 1
        assert f.slope == pytest.approx(o.beta[0], rel=1e-6)
        assert f.intercept == pytest.approx(o.beta[1], rel=1e-6)
        assert f.slope_err == pytest.approx(o.sd_beta[0], rel=1e-6)
        assert f.intercept_err == pytest.approx(o.sd_beta[1], rel=1e-6)
        assert f.mswd == pytest.approx(o.res_var, rel=1e-6)

    def test_unweighted(self, odr):
        rng = np.random.default_rng(2)
        x = rng.uniform(1e17, 3e18, 100)
        y = 0.036 * x + 6e17 + rng.normal(0, 2e16, 100)
        f, o = york_fit(x, y), self._ref(odr, x, y)
        assert not f.weighted
        assert f.slope == pytest.approx(o.beta[0], rel=1e-6)
        assert f.slope_err == pytest.approx(o.sd_beta[0], rel=1e-6)


class TestCorrelatedErrors:

    @pytest.mark.parametrize("r", [0.0, 0.8, -0.8])
    def test_unbiased_and_errors_cover(self, r):
        """Monte Carlo: internal errors match the scatter; MSWD ~ 1."""
        rng = np.random.default_rng(7)
        n, b0, a0, trials = 60, 2.5, 4e20, 400
        xt = rng.uniform(1e19, 2e20, n)
        sx = rng.uniform(0.5, 1.5, n) * 5e18
        sy = rng.uniform(0.5, 1.5, n) * 2e19
        slopes, errs, mswd = [], [], []
        for _ in range(trials):
            z1 = rng.normal(size=n)
            z2 = r * z1 + np.sqrt(1 - r * r) * rng.normal(size=n)
            f = york_fit(xt + sx * z1, b0 * xt + a0 + sy * z2, sx, sy, r)
            slopes.append(f.slope)
            errs.append(f.slope_err_internal)
            mswd.append(f.mswd)
        sd = np.std(slopes)
        assert abs(np.mean(slopes) - b0) < 4 * sd / np.sqrt(trials)
        assert np.mean(errs) == pytest.approx(sd, rel=0.1)
        assert np.mean(mswd) == pytest.approx(1.0, abs=0.05)


class TestQuotedErrors:

    def test_not_scaled_down_when_mswd_below_one(self):
        x = np.linspace(0, 10, 20)
        y = 2 * x + 1 + 0.01 * np.sin(7 * x)
        f = york_fit(x, y, np.full(20, 0.5), np.full(20, 0.5))
        assert f.mswd < 1
        assert f.slope_err == f.slope_err_internal

    def test_exact_line(self):
        x = np.linspace(1e18, 5e18, 30)
        f = york_fit(x, 0.04 * x + 3e17)
        assert f.slope == pytest.approx(0.04, rel=1e-12)
        assert isinstance(f, LineFit) and f.n == 30


class TestRefusals:

    @pytest.mark.parametrize("x, y, kw, match", [
        ([1.0, 2.0], [1.0, 2.0], {}, "at least 3"),
        ([1.0, 1.0, 1.0], [1.0, 2.0, 3.0], {}, "no spread"),
        ([1.0, 2.0, np.nan], [1.0, 2.0, 3.0], {}, "finite"),
        ([1.0, 2.0, 3.0], [1.0, 2.0, 3.0], {"sx": [1.0, 1.0, 1.0]}, "both"),
        ([1.0, 2.0, 3.0], [1.0, 2.0, 3.0], {"sx": [1.0, 0.0, 1.0],
                                            "sy": [1.0, 1.0, 1.0]}, "positive"),
        ([1.0, 2.0, 3.0], [1.0, 2.0, 3.0], {"sx": [1.0] * 3, "sy": [1.0] * 3,
                                            "r": 1.0}, "correlations"),
    ])
    def test_refused(self, x, y, kw, match):
        with pytest.raises(RegressionError, match=match):
            york_fit(np.array(x), np.array(y), **kw)


class TestGrouped:
    """One slope, an intercept per group (validated 2026-11-16: identical to
    york_fit for one group; Monte Carlo over 2000 draws unbiased, scatter /
    internal error 0.98, 1σ coverage 0.682, MSWD 1.00)."""

    @staticmethod
    def _data(rng, n_groups=20, per=16, drift=6e17):
        g = np.repeat(np.arange(n_groups), per)
        xt = rng.uniform(2e19, 1.5e20, g.size)
        sx, sy = 0.015 * xt, np.full(g.size, 8e16)
        a = 4e17 + np.linspace(0, drift, n_groups)[g]
        return g, xt, sx, sy, a

    @pytest.mark.parametrize("r", [0.0, 0.6])
    def test_one_group_is_york(self, r):
        rng = np.random.default_rng(0)
        g, xt, sx, sy, a = self._data(rng, n_groups=1, per=200, drift=0)
        x, y = xt + rng.normal(0, sx), 0.035 * xt + a + rng.normal(0, sy)
        f, gf = york_fit(x, y, sx, sy, r), york_fit_grouped(x, y, sx, sy, g, r)
        assert gf.slope == pytest.approx(f.slope, rel=1e-12)
        assert gf.slope_err_internal == pytest.approx(f.slope_err_internal, rel=1e-12)
        assert gf.mswd == pytest.approx(f.mswd, rel=1e-12)
        assert gf.intercepts[0] == pytest.approx(f.intercept, rel=1e-10)

    def test_errors_match_the_scatter(self):
        rng = np.random.default_rng(1)
        g, xt, sx, sy, a = self._data(rng)
        fits = []
        for _ in range(400):
            x, y = xt + rng.normal(0, sx), 0.035 * xt + a + rng.normal(0, sy)
            fits.append(york_fit_grouped(x, y, sx, sy, g))
        slopes = np.array([f.slope for f in fits])
        err = np.median([f.slope_err_internal for f in fits])
        assert slopes.mean() == pytest.approx(0.035, abs=3 * slopes.std() / 20)
        assert slopes.std() / err == pytest.approx(1.0, abs=0.1)
        assert np.mean([f.mswd for f in fits]) == pytest.approx(1.0, abs=0.03)

    def test_intercepts_absorb_a_background_that_tracks_the_plume(self):
        # the background rises with each group's mean x: a single line is
        # biased, the grouped slope is not
        rng = np.random.default_rng(2)
        g = np.repeat(np.arange(10), 30)
        level = np.linspace(3e19, 1.2e20, 10)[g]
        xt = level * rng.uniform(0.7, 1.3, g.size)
        sx, sy = 0.01 * xt, np.full(g.size, 3e16)
        y_true = 0.035 * xt + 0.006 * level
        x, y = xt + rng.normal(0, sx), y_true + rng.normal(0, sy)
        gf = york_fit_grouped(x, y, sx, sy, g)
        assert gf.slope == pytest.approx(0.035, rel=0.01)
        assert york_fit(x, y, sx, sy).slope > 0.039
        np.testing.assert_allclose(gf.intercepts, 0.006 * np.unique(level), rtol=0.2)

    def test_refusals(self):
        x = np.arange(10.0); s = np.ones(10)
        with pytest.raises(RegressionError, match="at least 2"):
            york_fit_grouped(x, x, s, s, np.r_[np.zeros(9), 1])
        with pytest.raises(RegressionError, match="no spread"):
            york_fit_grouped(np.repeat([1.0, 2.0], 5), x, s, s, np.repeat([0, 1], 5))
        with pytest.raises(RegressionError, match="degree of freedom"):
            york_fit_grouped(x[:2], x[:2], s[:2], s[:2], np.zeros(2))
        with pytest.raises(RegressionError, match="sx"):
            york_fit_grouped(x, x, -s, s, np.zeros(10))
