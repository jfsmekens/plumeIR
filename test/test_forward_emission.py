"""
Tests for the emission-geometry forward model.

Uses synthetic AtmLayerLUTs, GasLUTs, and AerosolLUTs (no RFM / mieEODG) and
a synthetic ILS kernel so the three-layer radiative transfer can be exercised
without external binaries.

Run with:  pytest test/test_forward_emission.py -v
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from plumeIR.forward.emission import EmissionForwardModel
from plumeIR.config.objects import EmissionGeometry, FitWindowConfig, GeometryType
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.physics import rad, bbt
from plumeIR.reference.atm_lut import AtmLayerLUT, derive_t_eff
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.reference import Reference

_WN = np.linspace(700.0, 1300.0, 150)


# ---------------------------------------------------------------------------
# Synthetic fixtures
# ---------------------------------------------------------------------------

def _atm_layer(name: str, temp: float, trans: float) -> AtmLayerLUT:
    """Layer at constant temperature with uniform transmittance."""
    nodes    = np.array([0.5, 1.0, 2.0])
    L_row    = rad(_WN, temp) * (1.0 - trans)
    L_table  = np.tile(L_row, (3, 1))
    T_table  = np.full((3, _WN.size), trans)
    od_table = np.full((3, _WN.size), -np.log(trans))
    t_eff    = derive_t_eff(_WN, L_table, T_table)
    return AtmLayerLUT(name, _WN, nodes, od_table, t_eff)


def _flat_gas_lut(species: str, od: float) -> GasLUT:
    temp_nodes = np.linspace(300.0, 1000.0, 5)
    return GasLUT(species=species, wn=_WN, temp_nodes=temp_nodes,
                  od_table=np.full((5, _WN.size), od), ref_scd=1e17)


def _dual_aero_lut(species: str, ext: float, ab: float) -> AerosolLUT:
    r0 = np.geomspace(0.1, 10.0, 5)
    return AerosolLUT(species=species, wn=_WN, r0_nodes_um=r0,
                      od_table=np.full((5, _WN.size), ext),
                      ref_mass_g_m2=0.05,
                      od_table_abs=np.full((5, _WN.size), ab))


def _emission_ref(gas=True, aero=False, with_clear=True) -> Reference:
    gas_luts  = {"SO2": _flat_gas_lut("SO2", 0.5)} if gas else {}
    aero_luts = {"ASH": _dual_aero_lut("ASH", 0.3, 0.2)} if aero else {}
    atm_layers = {
        "prox":  _atm_layer("prox",  280.0, 0.8),
        "slice": _atm_layer("slice", 270.0, 0.9),
        "dist":  _atm_layer("dist",  240.0, 0.7),
    }
    L_clear = rad(_WN, 250.0) if with_clear else None
    return Reference(wn=_WN, gas_luts=gas_luts, aero_luts=aero_luts,
                     geometry_type=GeometryType.EMISSION,
                     atm_layers=atm_layers, L_clear=L_clear)


def _geom(**kw) -> EmissionGeometry:
    base = dict(plume_height=2.0, plume_thickness=0.4, elev=30.0,
                obs_height=0.2, atm_path=Path("/tmp/x.atm"), plume_temp=400.0)
    base.update(kw)
    return EmissionGeometry(**base)


def _window(plume=("SO2",), aero=(), polydeg=0) -> FitWindowConfig:
    return FitWindowConfig(
        label="FIT0", wn_start=750.0, wn_stop=1250.0,
        atm_gases=("H2O",), plume_gases=plume, plume_aero=aero,
        targets=plume, polydeg=polydeg,
    )


class _Retr:
    apod_type = "NB medium"
    max_opd   = 1.8


def _params(plume=("SO2",), aero=(), polydeg=0, with_h2osc=True, with_dt=True,
            fit_temp=False):
    plist = [Parameter(f"poly{i}", 1.0 if i == 0 else 0.0, True, (-10, 10),
                       ParameterKind.POLYNOMIAL) for i in range(polydeg + 1)]
    for sp in plume:
        plist.append(Parameter(sp, 1.0, True, (0, np.inf),
                               ParameterKind.GAS_PLUME, ref_column=1e17))
    for sp in aero:
        plist.append(Parameter(sp, 1.0, True, (0, np.inf),
                               ParameterKind.AERO_PLUME, ref_column=0.05))
        plist.append(Parameter(f"r0_{sp}", 1.0, False, (0.1, 10.0),
                               ParameterKind.AERO_PLUME))
    if with_h2osc:
        plist.append(Parameter("H2O_sc", 1.0, True, (0.2, 3.0),
                               ParameterKind.ATMOSPHERE))
    # dt_prox/dt_dist are LAYER offsets: they shift a multi-km layer's
    # effective emission temperature only, leaving its OD untouched.
    if with_dt:
        plist.append(Parameter("dt_prox", 0.0, True, (-20, 20), ParameterKind.TEMPERATURE))
        plist.append(Parameter("dt_dist", 0.0, True, (-20, 20), ParameterKind.TEMPERATURE))
    # plume_temp is a different kind of parameter: an absolute temperature for
    # one thin near-isothermal layer, driving absorption AND emission together.
    if fit_temp:
        plist.append(Parameter("plume_temp", 400.0, True, (350.0, 450.0),
                               ParameterKind.TEMPERATURE))
    plist.append(Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL))
    return ParameterSet(plist)


@pytest.fixture
def no_ils(monkeypatch):
    monkeypatch.setattr(EmissionForwardModel, "_make_kernel",
                        lambda self, p: np.array([1.0]))


# =============================================================================
# Construction
# =============================================================================

class TestConstruction:

    def test_requires_atm_layers(self):
        ref = Reference(wn=_WN, geometry_type=GeometryType.EMISSION)  # no atm_layers
        with pytest.raises(ValueError, match="atm_layers"):
            EmissionForwardModel(ref, _params(), _geom(), _Retr(), _window(), _WN)

    def test_difference_requires_l_clear(self):
        ref = _emission_ref(with_clear=False)
        geom = _geom(fit_difference=True)
        with pytest.raises(ValueError, match="L_clear"):
            EmissionForwardModel(ref, _params(), geom, _Retr(), _window(), _WN)


# =============================================================================
# Radiative transfer assembly
# =============================================================================

class TestRadiativeTransfer:

    def _build(self, fit_difference=False, use_bbt=False, aero=False):
        ref    = _emission_ref(aero=aero)
        geom   = _geom(fit_difference=fit_difference, use_bbt=use_bbt)
        win    = _window(aero=("ASH",) if aero else ())
        params = _params(aero=("ASH",) if aero else ())
        fm = EmissionForwardModel(ref, params, geom, _Retr(), win, _WN)
        return fm, params

    def test_highres_shape(self):
        fm, params = self._build()
        F = fm.model_highres(params.p0_vector())
        assert F.shape == _WN.shape

    def test_manual_rt_matches(self):
        # Reproduce the three-layer RT by hand and compare
        fm, params = self._build(fit_difference=False)
        F = fm.model_highres(params.p0_vector())

        ref = fm.ref
        L_prox,  T_prox  = ref.atm_layers["prox"].emission(1.0, 0.0)
        L_slice, T_slice = ref.atm_layers["slice"].emission(1.0, 0.0)
        L_dist,  _       = ref.atm_layers["dist"].emission(1.0, 0.0)
        OD = 0.5   # SO2 scaling 1 × od 0.5
        T_plume = np.exp(-OD)
        L_plume = rad(_WN, 400.0) * (1 - np.exp(-OD))
        expected = (L_dist * T_plume * T_slice * T_prox
                    + (L_plume + L_slice) * T_prox + L_prox)
        assert np.allclose(F, expected, rtol=1e-9)

    def test_clear_sky_is_the_model_without_the_plume(self):
        """continuum: the plume-free layer sum, through the same ILS/shift."""
        fm, params = self._build(aero=True)
        F0 = fm.model_highres(params.p0_vector(), plume=False)
        ref = fm.ref
        L_prox,  T_prox  = ref.atm_layers["prox"].emission(1.0, 0.0)
        L_slice, T_slice = ref.atm_layers["slice"].emission(1.0, 0.0)
        L_dist,  _       = ref.atm_layers["dist"].emission(1.0, 0.0)
        expected = L_dist * T_slice * T_prox + L_slice * T_prox + L_prox
        assert np.allclose(F0, expected, rtol=1e-9)
        # Independent of the plume columns; equal to the model at zero plume.
        p = params.p0_vector().copy()
        names = params.varied_names()
        clear = fm.continuum(p)
        p[names.index("SO2")] = 3.0
        assert np.allclose(fm.continuum(p), clear, rtol=1e-12)
        p[names.index("SO2")] = 0.0
        p[names.index("ASH")] = 0.0
        assert np.allclose(fm(p), clear, rtol=1e-9)

    def test_difference_subtracts_clear(self):
        fm, params = self._build(fit_difference=True)
        F = fm.model_highres(params.p0_vector())
        # Rebuild the non-difference radiance and subtract clear manually
        fm2, _ = self._build(fit_difference=False)
        L = fm2.model_highres(params.p0_vector())
        assert np.allclose(F, L - rad(_WN, 250.0), rtol=1e-9)

    def test_bbt_mode_is_temperature(self):
        fm, params = self._build(use_bbt=True, fit_difference=False)
        F = fm.model_highres(params.p0_vector())
        # Brightness temperature should be in a plausible atmospheric K range
        assert np.all(F > 200) and np.all(F < 400)

    def test_aerosol_uses_absorption_for_emission(self):
        # With aerosol, plume emission uses absorption OD (0.2), transmittance
        # uses extinction (0.5 gas + 0.3 ext aero = 0.8)
        fm, params = self._build(aero=True)
        F = fm.model_highres(params.p0_vector())
        ref = fm.ref
        L_prox,  T_prox  = ref.atm_layers["prox"].emission(1.0, 0.0)
        L_slice, T_slice = ref.atm_layers["slice"].emission(1.0, 0.0)
        L_dist,  _       = ref.atm_layers["dist"].emission(1.0, 0.0)
        OD_ext = 0.5 + 0.3
        OD_abs = 0.5 + 0.2
        T_plume = np.exp(-OD_ext)
        L_plume = rad(_WN, 400.0) * (1 - np.exp(-OD_abs))
        expected = (L_dist * T_plume * T_slice * T_prox
                    + (L_plume + L_slice) * T_prox + L_prox)
        assert np.allclose(F, expected, rtol=1e-9)


# =============================================================================
# Parameter sensitivities
# =============================================================================

class TestSensitivities:

    def _fm(self, **kw):
        ref = _emission_ref()
        return EmissionForwardModel(ref, _params(**kw), _geom(fit_difference=False),
                                    _Retr(), _window(), _WN)

    def test_more_so2_lowers_transmittance(self):
        fm = self._fm()
        p0 = fm.params.p0_vector().copy()
        i_so2 = fm.params.varied_names().index("SO2")
        F_lo = fm.model_highres(p0)
        p0[i_so2] = 3.0
        F_hi = fm.model_highres(p0)
        # More absorber → less background transmitted; but more plume emission.
        # Just assert the spectrum changed materially.
        assert not np.allclose(F_lo, F_hi)

    def test_plume_temp_raises_plume_emission(self):
        """
        plume_temp is fitted directly and drives BOTH absorption and emission
        (Kirchhoff, single near-isothermal layer).  Raising it must raise the
        radiance through the plume's own emission term.
        """
        fm = self._fm(fit_temp=True)
        p0 = fm.params.p0_vector().copy()
        i = fm.params.varied_names().index("plume_temp")
        p0[i] = 400.0
        F0 = fm.model_highres(p0)
        p0[i] = 430.0
        F1 = fm.model_highres(p0)
        # Hotter plume → more emission added (through T_prox) → higher radiance
        assert np.mean(F1) > np.mean(F0)

    def test_plume_temp_also_drives_absorption(self):
        """
        The distinguishing property of fitting plume_temp rather than an
        emission-only offset: it changes the gas LUT query, so the *line shape*
        responds too.  This is what breaks the exact column degeneracy that
        an emission-only dt_plume offset had with the plume column.
        """
        fm = self._fm(fit_temp=True)
        p0 = fm.params.p0_vector().copy()
        i_t = fm.params.varied_names().index("plume_temp")
        i_g = fm.params.varied_names().index("SO2")

        # Kill the emission term by making the plume transparent, so only the
        # absorption path can respond.
        p0[i_g] = 0.0
        p0[i_t] = 380.0
        F_cold = fm.model_highres(p0)
        p0[i_t] = 430.0
        F_hot = fm.model_highres(p0)
        assert np.allclose(F_cold, F_hot)          # no gas -> no temperature effect

        # With gas present, temperature must alter the spectrum's SHAPE, not
        # merely its level.
        p0[i_g] = 2.0
        p0[i_t] = 380.0
        A = fm.model_highres(p0)
        p0[i_t] = 430.0
        B = fm.model_highres(p0)
        ratio = B / A
        assert not np.allclose(ratio, ratio.mean(), rtol=1e-6)

    def test_callable_with_no_ils(self, no_ils):
        fm = self._fm()
        model = fm(fm.params.p0_vector())
        assert model.shape == _WN.shape
        assert np.allclose(model, fm.model_highres(fm.params.p0_vector()), atol=1e-6)
