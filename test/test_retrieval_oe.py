"""
Tests for the OE (optimal-estimation) retrieval backend.

Same synthetic round-trip substrate as the NLLS tests (banded GasLUT + delta
ILS, no RFM).  OE additionally needs finite priors, so the parameters carry
explicit ``sigma_prior``; with a weak prior and low measurement noise the
posterior recovers the truth.  Also checks the OE-only diagnostics (averaging
kernel, DOF) and the finite-prior guard.

Requires ``pyOptimalEstimation``; skipped where it is not installed (e.g. the
base env).  Run in the project env:  pytest test/test_retrieval_oe.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

pytest.importorskip("pyOptimalEstimation")

from plumeIR.exceptions import FitError
from plumeIR.forward.layer import TransmittanceForwardModel
from plumeIR.config.objects import FitWindowConfig, GeometryType, LayerGeometry
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference
from plumeIR.retrieval import OptimalEstimation

_WN = np.linspace(2400.0, 2550.0, 120)
_TRUE = np.array([1.2, 2.0])      # [poly0, SO2]


def _banded_so2_lut() -> GasLUT:
    band = 0.8 * np.exp(-((_WN - 2475.0) / 15.0) ** 2)
    return GasLUT(species="SO2", wn=_WN, temp_nodes=np.linspace(300.0, 1000.0, 5),
                  od_table=np.tile(band, (5, 1)), ref_scd=1e17)


def _params(so2_prior: float | None = 5.0) -> ParameterSet:
    return ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL,
                  sigma_prior=2.0),
        Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17, sigma_prior=so2_prior),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])


@pytest.fixture
def forward(monkeypatch):
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


def test_oe_roundtrip(forward):
    measured = forward(_TRUE)
    res = OptimalEstimation(default_noise=1e-3).fit(
        forward, _params(), measured, window_label="FIT0", spectrum_id="t0")
    assert res.success
    assert res.method == "oe"
    # Weak prior + low noise → posterior tracks the truth.
    assert res.value("SO2") == pytest.approx(2.0, abs=0.05)
    assert res.value("poly0") == pytest.approx(1.2, abs=0.02)
    assert res.window_label == "FIT0" and res.spectrum_id == "t0"


def test_oe_diagnostics(forward):
    res = OptimalEstimation(default_noise=1e-3).fit(forward, _params(), forward(_TRUE))
    # OE-only outputs populated.
    assert res.averaging_kernel is not None
    assert res.averaging_kernel.shape == (2, 2)
    assert res.dof is not None and 0.0 < res.dof <= 2.0
    assert res.covariance.shape == (2, 2)
    assert res.error("SO2") is not None and res.error("SO2") > 0


def test_oe_requires_finite_prior(forward):
    # SO2 unbounded with no sigma_prior → infinite prior variance → clear error.
    bad = ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL,
                  sigma_prior=2.0),
        Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),                      # no sigma_prior
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])
    with pytest.raises(FitError, match="finite prior"):
        OptimalEstimation().fit(forward, bad, forward(_TRUE))


def test_oe_fitresult_picklable(forward):
    import pickle
    res = OptimalEstimation(default_noise=1e-3).fit(forward, _params(), forward(_TRUE))
    clone = pickle.loads(pickle.dumps(res))
    assert clone.value("SO2") == pytest.approx(res.value("SO2"))
    assert np.allclose(clone.averaging_kernel, res.averaging_kernel)
