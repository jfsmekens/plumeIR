"""
Emission-geometry synthetic round-trip — the last forward-validation gap.

Generate a radiance spectrum from the three-layer EmissionForwardModel at known
parameters, then check NLLS recovers them.  Closes the deferred emission
forward validation now that a fitter exists (Phase 4b left this pending on
Phase 5).

Synthetic, no RFM: flat background atmospheric layers plus a *banded* plume gas
(so its column is spectrally identifiable, not degenerate with the smooth
background), and a delta ILS kernel.  The plume self-emission term (dt_plume)
is exercised separately — it is the emission-specific physics.

Run with:  pytest test/test_retrieval_emission_roundtrip.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from plumeIR.forward.emission import EmissionForwardModel
from plumeIR.config.objects import EmissionGeometry, FitWindowConfig, GeometryType
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.physics import rad
from plumeIR.reference.atm_lut import AtmLayerLUT, derive_t_eff
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference
from plumeIR.retrieval import NonLinearLeastSquares

_WN = np.linspace(700.0, 1300.0, 220)


def _atm_layer(name: str, temp: float, trans: float) -> AtmLayerLUT:
    """Flat (smooth) background layer at constant temperature."""
    nodes = np.array([0.5, 1.0, 2.0])
    L_table = np.tile(rad(_WN, temp) * (1.0 - trans), (3, 1))
    T_table = np.full((3, _WN.size), trans)
    od_table = np.full((3, _WN.size), -np.log(trans))
    return AtmLayerLUT(name, _WN, nodes, od_table, derive_t_eff(_WN, L_table, T_table))


def _banded_so2() -> GasLUT:
    band = 0.9 * np.exp(-((_WN - 1100.0) / 25.0) ** 2)     # distinct absorption band
    return GasLUT(species="SO2", wn=_WN, temp_nodes=np.linspace(250.0, 1000.0, 5),   # covers the plume_temp bounds
                  od_table=np.tile(band, (5, 1)), ref_scd=1e17)


def _ref() -> Reference:
    return Reference(
        wn=_WN, gas_luts={"SO2": _banded_so2()}, geometry_type=GeometryType.EMISSION,
        atm_layers={"prox":  _atm_layer("prox", 280.0, 0.80),
                    "slice": _atm_layer("slice", 270.0, 0.90),
                    "dist":  _atm_layer("dist", 240.0, 0.70)},
        L_clear=None,
    )


def _geom() -> EmissionGeometry:
    return EmissionGeometry(plume_height=2.0, plume_thickness=0.4, elev=30.0,
                            obs_height=0.2, atm_path=Path("/tmp/x.atm"),
                            plume_temp=400.0, fit_difference=False, use_bbt=False)


def _window() -> FitWindowConfig:
    return FitWindowConfig(label="FIT0", wn_start=750.0, wn_stop=1250.0,
                           atm_gases=("H2O",), plume_gases=("SO2",), plume_aero=(),
                           targets=("SO2",), polydeg=0)


class _Retr:
    apod_type = "NB medium"
    max_opd = 1.8


def _params(fit_temp: bool = False) -> ParameterSet:
    plist = [
        Parameter("poly0", 1.0, True, (-10.0, 10.0), ParameterKind.POLYNOMIAL),
        Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17),
    ]
    if fit_temp:
        plist.append(Parameter("plume_temp", 295.0, True, (285.0, 315.0),
                               ParameterKind.TEMPERATURE))
    plist.append(Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL))
    return ParameterSet(plist)


@pytest.fixture
def no_ils(monkeypatch):
    monkeypatch.setattr(EmissionForwardModel, "_make_kernel",
                        lambda self, p: np.array([1.0]))


def _forward(params) -> EmissionForwardModel:
    return EmissionForwardModel(_ref(), params, _geom(), _Retr(), _window(), _WN)


def test_emission_roundtrip_plume_gas(no_ils):
    truth = np.array([1.2, 2.0])                  # poly0, SO2
    measured = _forward(_params())(truth)
    res = NonLinearLeastSquares().fit(_forward(_params()), _params(), measured,
                                      window_label="FIT0")
    assert res.success
    assert res.value("poly0") == pytest.approx(1.2, abs=1e-4)
    assert res.value("SO2") == pytest.approx(2.0, abs=1e-4)
    assert res.rmse < 1e-6


def test_emission_roundtrip_with_self_emission(no_ils):
    # plume_temp drives absorption AND self-emission together; check it is
    # recovered alongside the plume column.  Bounded to a physical range about
    # ambient, as build_parameter_set does — an advected plume sits within a
    # few K of ambient.
    truth = np.array([1.1, 2.5, 305.0])           # poly0, SO2, plume_temp [K]
    measured = _forward(_params(fit_temp=True))(truth)
    res = NonLinearLeastSquares().fit(_forward(_params(fit_temp=True)), _params(fit_temp=True),
                                      measured, window_label="FIT0")
    assert res.success
    assert res.value("SO2") == pytest.approx(2.5, abs=1e-2)
    assert res.value("plume_temp") == pytest.approx(305.0, abs=1.0)
    assert res.rmse < 1e-5
