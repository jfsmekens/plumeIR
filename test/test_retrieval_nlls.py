"""
Tests for the NLLS retrieval backend.

Synthetic round-trip: build a spectrum from a forward model at known
parameters, then check NonLinearLeastSquares recovers them.  Uses a synthetic
banded GasLUT and a delta ILS kernel (no RFM / mieEODG), so the recovery is
exact in the noiseless case.  Also covers the FitResult contract (picklable,
residual, error accessors) needed for the parallel run layer.

Run with:  pytest test/test_retrieval_nlls.py -v
"""

from __future__ import annotations

import pickle

import numpy as np
import pytest

from plumeIR.forward.layer import TransmittanceForwardModel
from plumeIR.config.objects import FitWindowConfig, GeometryType, LayerGeometry
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference
from plumeIR.retrieval import FitResult, NonLinearLeastSquares

_WN = np.linspace(2400.0, 2550.0, 200)

# Varied-parameter order follows ParameterSet declaration order: [poly0, SO2].
_TRUE = np.array([1.2, 2.0])      # poly0 = 1.2, SO2 scaling = 2.0


def _banded_so2_lut() -> GasLUT:
    """SO2 LUT with a Gaussian absorption band, constant over temperature."""
    band = 0.8 * np.exp(-((_WN - 2475.0) / 15.0) ** 2)
    od_table = np.tile(band, (5, 1))                       # (n_temp, n_wn)
    return GasLUT(species="SO2", wn=_WN,
                  temp_nodes=np.linspace(300.0, 1000.0, 5),
                  od_table=od_table, ref_scd=1e17)


def _params() -> ParameterSet:
    # Initial guess deliberately off the truth so the fit has to move.
    return ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL),
        Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])


@pytest.fixture
def forward(monkeypatch):
    """TransmittanceForwardModel with a delta ILS kernel (no smearing)."""
    monkeypatch.setattr(TransmittanceForwardModel, "_make_kernel",
                        lambda self, p: np.array([1.0]))
    ref = Reference(wn=_WN, gas_luts={"SO2": _banded_so2_lut()},
                    geometry_type=GeometryType.LAYER)
    geom = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                         plume_temp=450.0, plume_pres=1000.0, plume_thickness=0.05)
    window = FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                             atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                             targets=("SO2",), polydeg=0)

    class _Retr:
        apod_type = "NB medium"
        max_opd = 1.8

    return TransmittanceForwardModel(ref, _params(), geom, _Retr(), window, _WN)


def test_roundtrip_noiseless(forward):
    measured = forward(_TRUE)
    res = NonLinearLeastSquares().fit(forward, _params(), measured,
                                      window_label="FIT0")
    assert res.success
    assert res.value("poly0") == pytest.approx(1.2, abs=1e-6)
    assert res.value("SO2") == pytest.approx(2.0, abs=1e-6)
    assert res.rmse < 1e-8
    assert res.window_label == "FIT0"
    assert res.method == "nlls"


def test_roundtrip_noisy(forward):
    rng = np.random.default_rng(0)
    measured = forward(_TRUE) + rng.normal(0.0, 1e-3, _WN.size)
    res = NonLinearLeastSquares().fit(forward, _params(), measured)
    assert res.success
    # Recover within a few σ of the truth.
    assert res.value("SO2") == pytest.approx(2.0, abs=0.05)
    assert res.value("poly0") == pytest.approx(1.2, abs=0.02)
    # Uncertainties are populated and finite.
    assert res.perror is not None
    assert res.error("SO2") is not None and res.error("SO2") > 0
    assert res.covariance.shape == (2, 2)


def test_fixed_param_passed_through(forward):
    res = NonLinearLeastSquares().fit(forward, _params(), forward(_TRUE))
    # 'fov' is fixed → not in varied_names but present in values at its prior.
    assert "fov" not in res.varied_names
    assert res.value("fov") == pytest.approx(0.02)
    assert res.varied_names == ("poly0", "SO2")


def test_fitresult_picklable(forward):
    res = NonLinearLeastSquares().fit(forward, _params(), forward(_TRUE))
    clone = pickle.loads(pickle.dumps(res))     # must cross a process boundary
    assert clone.value("SO2") == pytest.approx(res.value("SO2"))
    assert np.allclose(clone.residual, res.residual)


def test_residual_and_grid(forward):
    measured = forward(_TRUE)
    res = NonLinearLeastSquares().fit(forward, _params(), measured)
    assert np.allclose(res.residual, res.measured - res.model)
    assert np.allclose(res.wn, _WN)


# ---------------------------------------------------------------------------
# Multi-start (weak-gas collapse guard)
# ---------------------------------------------------------------------------

def test_elevated_start_raises_plume_gas():
    """_elevated_start lifts varied GAS_PLUME params to the restart scaling."""
    backend = NonLinearLeastSquares(plume_restart_scaling=50.0)
    params = _params()                       # varied order [poly0, SO2]
    p0 = params.p0_vector()
    x0 = backend._elevated_start(params, p0)
    assert x0 is not None
    assert x0[0] == pytest.approx(p0[0])     # poly0 untouched
    assert x0[1] == pytest.approx(50.0)      # SO2 (plume gas) raised


def test_elevated_start_clamped_to_upper_bound():
    """The elevated scaling is clamped to the plume gas' upper bound."""
    backend = NonLinearLeastSquares(plume_restart_scaling=50.0)
    params = ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL),
        Parameter("CO", 0.01, True, (0.0, 10.0), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
    ])
    x0 = backend._elevated_start(params, params.p0_vector())
    assert x0[1] == pytest.approx(10.0)      # clamped to upper bound, not 50


def test_elevated_start_none_when_not_needed():
    """No restart vector when disabled, no plume gas, or already elevated."""
    p_no_plume = ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL),
    ])
    assert NonLinearLeastSquares()._elevated_start(
        p_no_plume, p_no_plume.p0_vector()) is None
    # disabled
    assert NonLinearLeastSquares(plume_restart_scaling=None)._elevated_start(
        _params(), _params().p0_vector()) is None
    # plume gas already at/above the restart scaling
    p_high = ParameterSet([
        Parameter("SO2", 80.0, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
    ])
    assert NonLinearLeastSquares(plume_restart_scaling=50.0)._elevated_start(
        p_high, p_high.p0_vector()) is None


def test_multistart_matches_single_on_convex(forward):
    """On a well-conditioned (convex) problem the restart descends to the same
    global minimum, so multi-start equals single-start — it never harms."""
    measured = forward(_TRUE)
    with_restart = NonLinearLeastSquares(plume_restart_scaling=50.0).fit(
        forward, _params(), measured)
    no_restart = NonLinearLeastSquares(plume_restart_scaling=None).fit(
        forward, _params(), measured)
    assert with_restart.value("SO2") == pytest.approx(no_restart.value("SO2"),
                                                      abs=1e-6)
    assert with_restart.rmse <= no_restart.rmse + 1e-12



# ---------------------------------------------------------------------------
# Convergence on a badly scaled problem (2026-11-14)
# ---------------------------------------------------------------------------

def _line_forest_case():
    """
    A single-beam fit as in a layer window: a cubic baseline of ~1e5, an
    order-1 gas scaling over a forest of narrow lines, and a real ILS with
    ``fov`` and ``shift`` free.  Noiseless, so the minimum is the truth.
    """
    wn = np.arange(2350.0, 2600.0, 0.04)
    rng = np.random.default_rng(1)
    centres, strengths = rng.uniform(2420, 2530, 120), rng.lognormal(0, 1, 120)
    band = sum(s * 0.02 * np.exp(-((wn - c) / 0.03) ** 2)
               for s, c in zip(strengths, centres))
    lut = GasLUT(species="SO2", wn=wn, temp_nodes=np.linspace(300.0, 1000.0, 5),
                 od_table=np.tile(band, (5, 1)), ref_scd=1e17)
    params = ParameterSet(
        [Parameter(f"poly{i}", v, True, (-np.inf, np.inf), ParameterKind.POLYNOMIAL)
         for i, v in enumerate((0.9e5, 0.0, 0.0, 0.0))]
        + [Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                     ref_column=1e17),
           Parameter("fov", 0.02, True, (0.001, 0.2), ParameterKind.SPECTRAL),
           Parameter("shift", 0.0, True, (-1.0, 1.0), ParameterKind.SPECTRAL)])
    ref = Reference(wn=wn, gas_luts={"SO2": lut}, geometry_type=GeometryType.LAYER)
    geom = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                         plume_temp=450.0, plume_pres=1000.0, plume_thickness=0.05)
    window = FitWindowConfig(label="FIT0", wn_start=2450.0, wn_stop=2550.0,
                             atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                             targets=("SO2",), polydeg=3)

    class _Retr:
        apod_type = "triangle"
        max_opd = 1.037

    forward = TransmittanceForwardModel(ref, params, geom, _Retr(), window,
                                        np.arange(2450.0, 2550.0, 0.25))
    truth = np.array([1.0e5, -3.0e4, 7.0e3, 3.0e2, 2.0, 0.03, 0.05])
    return forward, params, forward(truth)


def test_converges_with_single_beam_scale():
    # Package defaults (x_scale='jac', tolerances 1e-8) reach the truth.
    forward, params, measured = _line_forest_case()
    res = NonLinearLeastSquares(plume_restart_scaling=None).fit(forward, params, measured)
    assert res.value("SO2") == pytest.approx(2.0, abs=1e-6)
    assert res.value("fov") == pytest.approx(0.03, abs=1e-6)
    assert res.value("shift") == pytest.approx(0.05, abs=1e-6)


def test_rc3_settings_stop_short():
    # The settings up to rc3 stop early: the unscaled xtol test compares each
    # step with ||x|| ~ 1e5, set by the baseline.  Measured while the ILS
    # kernel carried the FOV shift (fov and shift partly collinear): SO2
    # 1.974, rmse 5.5 against 9e-10.  Since convention A (2026-11-16, the FOV
    # only broadens) this case is well conditioned and the early stop costs
    # no column (SO2 2.0 to 5 digits), but it still stops ~1e4 short in rmse
    # (6e-7 against 4.5e-11).  Keeps the reason for the defaults on record.
    forward, params, measured = _line_forest_case()
    res = NonLinearLeastSquares(ftol=1e-6, xtol=1e-6, x_scale=1.0,
                                plume_restart_scaling=None).fit(forward, params, measured)
    ref = NonLinearLeastSquares(plume_restart_scaling=None).fit(forward, params, measured)
    assert res.rmse > 100.0 * ref.rmse


# ---------------------------------------------------------------------------
# Errors when a parameter has no effect (2026-11-16)
# ---------------------------------------------------------------------------

class TestUnconstrainedParameter:
    """An aerosol at zero leaves its size parameter without effect; the
    errors of every other parameter must survive (emission validation run:
    230 of 300 spectra lost all errors before)."""

    def test_inverse_normal(self):
        from plumeIR.retrieval.nlls import _inverse_normal
        rng = np.random.default_rng(0)
        j = rng.normal(size=(50, 3))
        np.testing.assert_array_equal(_inverse_normal(j), np.linalg.inv(j.T @ j))  # unchanged
        dead = j.copy()
        dead[:, 1] = 0.0
        cov = _inverse_normal(dead)
        assert np.isnan(cov[1]).all() and np.isnan(cov[:, 1]).all()
        live = dead[:, [0, 2]]
        np.testing.assert_allclose(cov[np.ix_([0, 2], [0, 2])], np.linalg.inv(live.T @ live),
                                   rtol=1e-12)
        tiny = j.copy()
        tiny[:, 1] *= 1e-30                       # an effect far below the others
        cov = _inverse_normal(tiny)
        assert np.isfinite(cov).all() and cov[1, 1] > 1e55
        np.testing.assert_allclose(cov[0, 0], np.linalg.inv(j.T @ j)[0, 0], rtol=1e-10)

    def test_fit_keeps_the_other_errors(self):
        x = np.linspace(0.0, 1.0, 100)

        class Forward:
            wn_meas = x

            def __call__(self, p):        # p = [offset, slope, r0]; r0 has no effect
                return p[0] + p[1] * x + 0.0 * p[2]

        params = ParameterSet([
            Parameter("poly0", 0.5, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL),
            Parameter("poly1", 0.5, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL),
            Parameter("r0_ASH", 2.0, True, (0.1, 10.0), ParameterKind.AERO_PLUME),
        ])
        measured = 1.0 + 2.0 * x + np.random.default_rng(1).normal(0, 0.01, x.size)
        res = NonLinearLeastSquares().fit(Forward(), params, measured)
        assert res.success and res.perror is not None
        assert np.isnan(res.error("r0_ASH"))
        assert 0 < res.error("poly0") < 0.01 and 0 < res.error("poly1") < 0.02
