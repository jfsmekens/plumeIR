"""
Tests for the solar-occultation forward model.

Uses synthetic vertical OD spectra (no RFM) and a synthetic ILS kernel.

Run with:  pytest test/test_forward_solar.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from plumeIR.forward.solar import SolarForwardModel
from plumeIR.config.objects import FitWindowConfig, GeometryType, SolarGeometry
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference

_WN = np.linspace(2000.0, 2200.0, 200)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def _band(center: float, depth: float, width: float = 10.0) -> np.ndarray:
    return depth * np.exp(-((_WN - center) / width) ** 2)


def _solar_ref(atm_gases=("CO2", "CH4"), plume=()) -> Reference:
    atm_gas_od = {
        "CO2": _band(2050.0, 0.6),
        "CH4": _band(2150.0, 0.4),
    }
    atm_gas_od = {g: atm_gas_od[g] for g in atm_gases}
    atm_gas_refcol = {g: 1e21 for g in atm_gases}   # vertical total column

    gas_luts = {}
    for sp in plume:
        gas_luts[sp] = GasLUT(species=sp, wn=_WN,
                              temp_nodes=np.linspace(250, 320, 4),
                              od_table=np.tile(_band(2100.0, 0.5), (4, 1)),
                              ref_scd=1e17)

    return Reference(wn=_WN, gas_luts=gas_luts, geometry_type=GeometryType.SOLAR,
                     atm_gas_od=atm_gas_od, atm_gas_refcol=atm_gas_refcol)


def _geom(**kw) -> SolarGeometry:
    base = dict(obs_height=0.5, atm_path=Path("/tmp/x.atm"), elev=45.0)
    base.update(kw)
    return SolarGeometry(**base)


def _window(atm=("CO2", "CH4"), plume=(), polydeg=0) -> FitWindowConfig:
    return FitWindowConfig(label="FIT0", wn_start=2000.0, wn_stop=2200.0,
                           atm_gases=atm, plume_gases=plume, plume_aero=(),
                           targets=atm, polydeg=polydeg)


class _Retr:
    apod_type = "NB medium"
    max_opd   = 1.8


def _params(atm=("CO2", "CH4"), plume=(), polydeg=0):
    plist = [Parameter(f"poly{i}", 1.0 if i == 0 else 0.0, True, (-10, 10),
                       ParameterKind.POLYNOMIAL) for i in range(polydeg + 1)]
    for g in atm:
        plist.append(Parameter(g, 1.0, True, (0, np.inf),
                               ParameterKind.GAS_ATM, ref_column=1e21))
    for sp in plume:
        plist.append(Parameter(sp, 1.0, True, (0, np.inf),
                               ParameterKind.GAS_PLUME, ref_column=1e17))
    plist.append(Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL))
    return ParameterSet(plist)


@pytest.fixture
def no_ils(monkeypatch):
    monkeypatch.setattr(SolarForwardModel, "_make_kernel",
                        lambda self, p: np.array([1.0]))


# =============================================================================
# Construction
# =============================================================================

class TestConstruction:

    def test_requires_atm_or_plume(self):
        ref = Reference(wn=_WN, geometry_type=GeometryType.SOLAR)  # nothing
        with pytest.raises(ValueError, match="atm_gas_od or plume"):
            SolarForwardModel(ref, _params(atm=()), _geom(), _Retr(),
                              _window(atm=()), _WN)


# =============================================================================
# Transmittance + airmass
# =============================================================================

class TestTransmittance:

    def _build(self, airmass=1.0):
        ref = _solar_ref()
        fm = SolarForwardModel(ref, _params(), _geom(), _Retr(), _window(),
                               _WN, airmass=airmass)
        return fm, fm.params

    def test_vertical_transmittance(self):
        fm, params = self._build(airmass=1.0)
        F = fm.model_highres(params.p0_vector())
        expected = np.exp(-(_band(2050, 0.6) + _band(2150, 0.4)))
        assert np.allclose(F, expected, rtol=1e-9)

    def test_airmass_increases_absorption(self):
        fm1, p1 = self._build(airmass=1.0)
        fm2, p2 = self._build(airmass=2.0)
        F1 = fm1.model_highres(p1.p0_vector())
        F2 = fm2.model_highres(p2.p0_vector())
        # Airmass 2 → twice the OD → deeper absorption (lower transmittance)
        assert np.all(F2 <= F1 + 1e-12)
        # At the CO2 band centre the difference is substantial
        i_centre = np.argmin(np.abs(_WN - 2050.0))
        assert F2[i_centre] < F1[i_centre] - 0.05

    def test_airmass_exact_scaling(self):
        # OD scales exactly with airmass: F(m) = exp(-m·OD_vert)
        fm1, p = self._build(airmass=1.0)
        fm3, _ = self._build(airmass=3.0)
        OD_vert = -np.log(fm1.model_highres(p.p0_vector()))  # poly=1
        F3 = fm3.model_highres(p.p0_vector())
        assert np.allclose(F3, np.exp(-3.0 * OD_vert), rtol=1e-9)

    def test_column_scaling(self):
        fm, params = self._build()
        p0 = params.p0_vector().copy()
        i_co2 = params.varied_names().index("CO2")
        p0[i_co2] = 2.0   # double CO2 column
        F = fm.model_highres(p0)
        expected = np.exp(-(2.0 * _band(2050, 0.6) + _band(2150, 0.4)))
        assert np.allclose(F, expected, rtol=1e-9)

    def test_polynomial_continuum(self):
        fm, params = self._build()
        p0 = params.p0_vector().copy()
        i_poly = params.varied_names().index("poly0")
        p0[i_poly] = 0.8
        F = fm.model_highres(p0)
        T = np.exp(-(_band(2050, 0.6) + _band(2150, 0.4)))
        assert np.allclose(F, 0.8 * T, rtol=1e-9)


# =============================================================================
# H2O self-continuum: quadratic column scaling  OD(s) = s·od + (s²−s)·B
# =============================================================================

class TestContinuumScaling:
    """The H2O self-continuum scales as column²; the forward must apply the
    quadratic term carried by ``atm_gas_od_cont``."""

    def _ref(self):
        # H2O: linear (line+foreign) part A + self-continuum B.  od_ref = A + B.
        A   = _band(2050.0, 0.5)                 # linear part
        B   = 0.2 * np.ones_like(_WN)            # flat self-continuum baseline
        od  = A + B
        return Reference(
            wn=_WN, geometry_type=GeometryType.SOLAR,
            atm_gas_od={"H2O": od},
            atm_gas_od_cont={"H2O": B},
            atm_gas_refcol={"H2O": 1e22},
        ), A, B

    def _build(self, ref, airmass=1.0):
        win    = _window(atm=("H2O",))
        params = _params(atm=("H2O",))
        return SolarForwardModel(ref, params, _geom(), _Retr(), win, _WN,
                                 airmass=airmass)

    def test_apriori_exact(self):
        # At s=1 the quadratic term vanishes (s²−s = 0) → OD = od_ref.
        ref, A, B = self._ref()
        fm = self._build(ref)
        F = fm.model_highres(fm.params.p0_vector())
        assert np.allclose(F, np.exp(-(A + B)), rtol=1e-9)

    def test_quadratic_continuum(self):
        # At s≠1: line part ∝ s, self-continuum ∝ s².
        ref, A, B = self._ref()
        fm = self._build(ref)
        p0 = fm.params.p0_vector().copy()
        i = fm.params.varied_names().index("H2O")
        for s in (0.5, 1.7, 2.3):
            p0[i] = s
            F = fm.model_highres(p0)
            expected = np.exp(-(s * A + s * s * B))   # exact physics
            assert np.allclose(F, expected, rtol=1e-9), f"s={s}"

    def test_airmass_multiplies_quadratic(self):
        # Airmass is a path-length factor: multiplies the whole OD, including
        # the quadratic continuum term.
        ref, A, B = self._ref()
        fm = self._build(ref, airmass=2.0)
        p0 = fm.params.p0_vector().copy()
        i = fm.params.varied_names().index("H2O")
        p0[i] = 1.5
        F = fm.model_highres(p0)
        expected = np.exp(-2.0 * (1.5 * A + 1.5 ** 2 * B))
        assert np.allclose(F, expected, rtol=1e-9)

    def test_no_cont_component_is_linear(self):
        # Without atm_gas_od_cont the forward must be plain linear s·od.
        ref, A, B = self._ref()
        ref.atm_gas_od_cont = {}            # drop the continuum component
        fm = self._build(ref)
        p0 = fm.params.p0_vector().copy()
        i = fm.params.varied_names().index("H2O")
        p0[i] = 2.0
        F = fm.model_highres(p0)
        assert np.allclose(F, np.exp(-2.0 * (A + B)), rtol=1e-9)


# =============================================================================
# Plume in the slant path
# =============================================================================

class TestPlume:

    def test_plume_adds_absorption_airmass_scaled(self):
        ref = _solar_ref(plume=("SO2",))
        params = _params(plume=("SO2",))
        win    = _window(plume=("SO2",))
        fm = SolarForwardModel(ref, params, _geom(plume_height=3.0, plume_temp=280.0),
                               _Retr(), win, _WN, airmass=2.0)
        F = fm.model_highres(params.p0_vector())
        # OD_total = CO2 + CH4 + SO2 (plume), all airmass-scaled
        OD = _band(2050, 0.6) + _band(2150, 0.4) + _band(2100, 0.5)
        assert np.allclose(F, np.exp(-2.0 * OD), rtol=1e-9)


# =============================================================================
# Callable
# =============================================================================

class TestCallable:

    def test_callable_matches_highres(self, no_ils):
        ref = _solar_ref()
        fm = SolarForwardModel(ref, _params(), _geom(), _Retr(), _window(),
                               _WN, airmass=1.5)
        model = fm(fm.params.p0_vector())
        assert np.allclose(model, fm.model_highres(fm.params.p0_vector()), atol=1e-6)

    def test_synthetic_roundtrip(self, no_ils):
        from scipy.optimize import curve_fit
        ref = _solar_ref()
        params = _params()
        fm = SolarForwardModel(ref, params, _geom(), _Retr(), _window(),
                               _WN, airmass=1.8)
        truth = np.array([1.02, 1.5, 0.7])   # poly0, CO2, CH4 scalings
        y = fm(truth)
        f = fm.curve_fit_callable()
        popt, _ = curve_fit(f, _WN, y, p0=[1.0, 1.0, 1.0])
        assert np.allclose(popt, truth, rtol=1e-4)
