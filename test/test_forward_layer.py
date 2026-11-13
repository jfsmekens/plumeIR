"""
Tests for the layer-geometry forward models.

Uses synthetic LUTs (no RFM / mieEODG) and a synthetic ILS kernel so the
radiative-transfer assembly can be exercised without external binaries.

Covers:
  - base.py pure functions (rad, bbt, apply_ils, shift/offset)
  - LayerForwardModel: transmittance assembly, polynomial, emission term
  - TransmittanceForwardModel: T_plume · poly only
  - The ForwardModel interface (callable, curve_fit adapter)

Run with:  pytest test/test_forward_layer.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

from plumeIR.forward.base import (
    rad, bbt, apply_ils, apply_shift_offset, build_background_poly,
)
from plumeIR.forward.layer import LayerForwardModel, TransmittanceForwardModel
from plumeIR.config.objects import FitWindowConfig, LayerGeometry
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.reference import Reference
from plumeIR.config.objects import GeometryType

# ---------------------------------------------------------------------------
# Synthetic fixtures
# ---------------------------------------------------------------------------

_WN = np.linspace(2400.0, 2550.0, 200)


def _flat_gas_lut(species: str, od_level: float) -> GasLUT:
    """1-D gas LUT with constant OD = od_level at all T (easy to reason about)."""
    temp_nodes = np.linspace(300.0, 1000.0, 5)
    od_table   = np.full((5, _WN.size), od_level)
    return GasLUT(species=species, wn=_WN, temp_nodes=temp_nodes,
                  od_table=od_table, ref_scd=1e17)


def _flat_aero_lut(species: str, od_level: float) -> AerosolLUT:
    r0_nodes = np.geomspace(0.05, 10.0, 6)
    od_table = np.full((6, _WN.size), od_level)
    return AerosolLUT(species=species, wn=_WN, r0_nodes_um=r0_nodes,
                      od_table=od_table, ref_mass_g_m2=0.05)


def _geom(**kw) -> LayerGeometry:
    base = dict(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                plume_temp=450.0, plume_pres=1000.0, plume_thickness=0.05)
    base.update(kw)
    return LayerGeometry(**base)


def _window(atm=(), plume=(), aero=(), polydeg=0, **kw) -> FitWindowConfig:
    return FitWindowConfig(
        label="FIT0", wn_start=2400.0, wn_stop=2550.0,
        atm_gases=atm, plume_gases=plume, plume_aero=aero,
        targets=plume, polydeg=polydeg, **kw,
    )


class _Retr:
    """Minimal stand-in for Retrieval (only fields the forward reads)."""
    apod_type = "NB medium"
    max_opd   = 1.8


# Monkeypatch target: replace _make_kernel with a trivial delta kernel so
# apply_ils is a no-op (lets us check the radiative-transfer math exactly).
@pytest.fixture
def no_ils(monkeypatch):
    delta = np.array([1.0])
    monkeypatch.setattr(LayerForwardModel, "_make_kernel", lambda self, p: delta)
    monkeypatch.setattr(TransmittanceForwardModel, "_make_kernel", lambda self, p: delta)


# =============================================================================
# base.py
# =============================================================================

class TestBaseFunctions:

    def test_rad_positive(self):
        assert np.all(rad(_WN, 1200.0) > 0)

    def test_rad_bbt_roundtrip(self):
        L = rad(_WN, 800.0)
        T = bbt(_WN, L)
        assert np.allclose(T, 800.0, atol=1e-6)

    def test_rad_hotter_brighter(self):
        assert np.all(rad(_WN, 1500.0) > rad(_WN, 1000.0))

    def test_apply_ils_normalised_kernel_preserves_flat(self):
        s = np.ones(100)
        k = np.ones(7) / 7
        out = apply_ils(s, k)
        assert np.allclose(out[20:80], 1.0)

    def test_apply_ils_preserves_flat_to_the_edges(self):
        """Edge padding, not zeros: a flat spectrum stays flat at the grid ends."""
        from plumeIR.forward.ils import make_ils
        k = make_ils(fov=0.03, max_opd=1.037, nper_wn=25.0, wn_center=2760.0,
                     apod_type="triangle")
        out = apply_ils(np.full(500, 3.0), k)
        assert np.allclose(out, 3.0, atol=1e-12)

    def test_broad_mean_matches_direct_average(self):
        from plumeIR.forward.base import _broad_mean
        s = np.random.default_rng(1).normal(size=300)
        h = 7
        direct = np.convolve(s, np.ones(2 * h + 1) / (2 * h + 1), mode="valid")
        assert np.allclose(_broad_mean(s, h)[h:-h], direct)

    def test_build_poly_empty_is_one(self):
        assert build_background_poly([], _WN) == 1.0

    def test_build_poly_constant(self):
        assert np.allclose(build_background_poly([3.0], _WN), 3.0)

    def test_shift_offset_identity(self):
        F = np.cos(_WN / 50.0)
        out = apply_shift_offset(F, _WN, _WN, 0.0, 0.0)
        assert np.allclose(out, F, atol=1e-6)

    def test_offset_added(self):
        F = np.ones(_WN.size)
        out = apply_shift_offset(F, _WN, _WN, 0.0, 0.25)
        assert np.allclose(out, 1.25, atol=1e-6)


# =============================================================================
# LayerForwardModel — transmittance assembly
# =============================================================================

class TestLayerForwardModel:

    def _build(self, with_aero=False, polydeg=0, model_gas_emission=False):
        gas_luts = {"SO2": _flat_gas_lut("SO2", 0.5)}
        aero_luts = {}
        aero = ()
        if with_aero:
            aero_luts["ASH"] = _flat_aero_lut("ASH", 0.3)
            aero = ("ASH",)
        ref = Reference(wn=_WN, gas_luts=gas_luts, aero_luts=aero_luts,
                        geometry_type=GeometryType.LAYER)

        plist = [Parameter(f"poly{i}", 1.0 if i == 0 else 0.0, True,
                           (-10, 10), ParameterKind.POLYNOMIAL)
                 for i in range(polydeg + 1)]
        plist.append(Parameter("SO2", 1.0, True, (0, np.inf),
                               ParameterKind.GAS_PLUME, ref_column=1e17))
        if with_aero:
            plist.append(Parameter("ASH", 1.0, True, (0, np.inf),
                                   ParameterKind.AERO_PLUME, ref_column=0.05))
            plist.append(Parameter("r0_ASH", 1.0, False, (0.05, 10.0),
                                   ParameterKind.AERO_PLUME))
        plist.append(Parameter("fov", 0.02, False, (0.001, 0.2),
                               ParameterKind.SPECTRAL))
        params = ParameterSet(plist)

        geom = _geom(model_gas_emission=model_gas_emission)
        win  = _window(plume=("SO2",), aero=aero, polydeg=polydeg,
                       model_gas_emission=model_gas_emission)
        fm = LayerForwardModel(ref, params, geom, _Retr(), win, _WN)
        return fm, params

    def test_highres_shape(self):
        fm, params = self._build()
        F = fm.model_highres(params.p0_vector())
        assert F.shape == _WN.shape

    def test_transmittance_value(self):
        # Single plume gas OD=0.5, scaling=1, poly=1 → F = exp(-0.5)
        fm, params = self._build()
        F = fm.model_highres(params.p0_vector())
        assert np.allclose(F, np.exp(-0.5), atol=1e-9)

    def test_scaling_doubles_od(self):
        fm, params = self._build()
        p0 = params.p0_vector().copy()
        # SO2 scaling is the first varied after poly0
        i_so2 = params.varied_names().index("SO2")
        p0[i_so2] = 2.0
        F = fm.model_highres(p0)
        assert np.allclose(F, np.exp(-1.0), atol=1e-9)   # OD = 2 × 0.5

    def test_aerosol_adds_od(self):
        fm, params = self._build(with_aero=True)
        F = fm.model_highres(params.p0_vector())
        # OD = SO2 0.5 + ASH 0.3 = 0.8
        assert np.allclose(F, np.exp(-0.8), atol=1e-9)

    def test_polynomial_scales(self):
        fm, params = self._build(polydeg=0)
        p0 = params.p0_vector().copy()
        i_poly = params.varied_names().index("poly0")
        p0[i_poly] = 2.0
        F = fm.model_highres(p0)
        assert np.allclose(F, 2.0 * np.exp(-0.5), atol=1e-9)

    def test_continuum_is_the_gas_free_polynomial(self):
        """continuum = polynomial × source terms, without absorption (v2's bkg)."""
        fm, params = self._build(polydeg=1)
        p0 = params.p0_vector().copy()
        names = params.varied_names()
        p0[names.index("poly0")], p0[names.index("poly1")] = 2.0, 0.5
        expected = build_background_poly([2.0, 0.5], _WN)
        assert np.allclose(fm.continuum(p0), expected, atol=1e-9)
        # The model is the continuum times the plume transmittance here.
        assert np.allclose(fm.model_highres(p0), expected * np.exp(-0.5), atol=1e-9)

    def test_emission_term_raises_floor(self):
        # With emission, absorption lines don't go as deep (added emission)
        fm_no,  p_no  = self._build(model_gas_emission=False)
        fm_em,  p_em  = self._build(model_gas_emission=True)
        F_no = fm_no.model_highres(p_no.p0_vector())
        # emission build needs gasE_temp + source_temp params; add them
        # (rebuild with the extra temperature params)
        F_em = fm_em.model_highres(p_em.p0_vector())
        # Emission adds a positive term, so F_em >= F_no everywhere
        assert np.all(F_em >= F_no - 1e-12)

    def test_continuum_plume_gas_emits_at_gasE_temp(self):
        """
        A continuum plume gas (2-D T x VMR LUT, e.g. plume H2O) contributes to
        the self-emission OD, queried at gasE_temp.  It used to be skipped, so
        its emission was silently dropped while its absorption was kept.
        """
        temp_nodes = np.linspace(300.0, 1000.0, 8)
        vmr_nodes  = np.geomspace(1e2, 1e5, 4)
        # OD ∝ VMR, with a temperature dependence, so the query temperature shows
        od_table = (vmr_nodes[None, :, None] * 1e-5
                    * (1.0 + temp_nodes[:, None, None] / 1000.0)
                    * np.ones(_WN.size)[None, None, :])
        h2o = GasLUT(species="H2O", wn=_WN, temp_nodes=temp_nodes,
                     od_table=od_table, ref_scd=1e20,
                     vmr_nodes=vmr_nodes, ref_vmr=1e3)
        ref = Reference(wn=_WN, gas_luts={"H2O": h2o}, aero_luts={},
                        geometry_type=GeometryType.LAYER)
        params = ParameterSet([
            Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
            Parameter("H2O", 2.0, True, (0, np.inf), ParameterKind.GAS_PLUME,
                      ref_column=1e20),
            Parameter("gas_temp", 800.0, False, (300, 1000), ParameterKind.TEMPERATURE),
            Parameter("gasE_temp", 500.0, False, (300, 1000), ParameterKind.TEMPERATURE),
            Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
        ])
        win = _window(plume=("H2O",), continuum_gases=("H2O",),
                      model_gas_emission=True)
        fm = LayerForwardModel(ref, params, _geom(model_gas_emission=True),
                               _Retr(), win, _WN)

        p = params.unpack(params.p0_vector())
        OD_abs, OD_em = fm._plume_optical_depth(p)
        assert np.allclose(OD_abs, h2o.query(800.0, 2.0 * 1e3))
        assert np.allclose(OD_em, h2o.query(500.0, 2.0 * 1e3))
        assert np.all(OD_em > 0) and not np.allclose(OD_em, OD_abs)

    def test_callable_interface(self, no_ils):
        fm, params = self._build()
        model = fm(params.p0_vector())
        assert model.shape == _WN.shape
        # delta kernel + no shift/offset → equals highres
        assert np.allclose(model, np.exp(-0.5), atol=1e-6)

    def test_curve_fit_callable(self, no_ils):
        fm, params = self._build()
        f = fm.curve_fit_callable()
        model = f(_WN, *params.p0_vector())
        assert model.shape == _WN.shape


# =============================================================================
# TransmittanceForwardModel
# =============================================================================

class TestTransmittanceForwardModel:

    def _build(self):
        ref = Reference(wn=_WN, gas_luts={"SO2": _flat_gas_lut("SO2", 0.5)},
                        aero_luts={}, geometry_type=GeometryType.LAYER)
        params = ParameterSet([
            Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
            Parameter("SO2", 1.0, True, (0, np.inf),
                      ParameterKind.GAS_PLUME, ref_column=1e17),
            Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
        ])
        geom = _geom()
        win  = _window(plume=("SO2",))
        return TransmittanceForwardModel(ref, params, geom, _Retr(), win, _WN), params

    def test_no_atm_no_source(self):
        # Even with an atm gas defined, the transmittance model ignores it.
        ref = Reference(
            wn=_WN,
            gas_luts={"SO2": _flat_gas_lut("SO2", 0.5),
                      "H2O": _flat_gas_lut("H2O", 1.0)},
            aero_luts={}, geometry_type=GeometryType.LAYER,
        )
        params = ParameterSet([
            Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
            Parameter("SO2", 1.0, True, (0, np.inf),
                      ParameterKind.GAS_PLUME, ref_column=1e17),
            Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
        ])
        # window lists H2O as atm gas, but transmittance model only uses plume
        win = _window(atm=("H2O",), plume=("SO2",))
        fm  = TransmittanceForwardModel(ref, params, _geom(), _Retr(), win, _WN)
        F   = fm.model_highres(params.p0_vector())
        # Only SO2 (0.5) contributes; H2O atm is ignored → exp(-0.5)
        assert np.allclose(F, np.exp(-0.5), atol=1e-9)

    def test_transmittance_value(self):
        fm, params = self._build()
        F = fm.model_highres(params.p0_vector())
        assert np.allclose(F, np.exp(-0.5), atol=1e-9)

    def test_callable(self, no_ils):
        fm, params = self._build()
        model = fm(params.p0_vector())
        assert np.allclose(model, np.exp(-0.5), atol=1e-6)


# =============================================================================
# Synthetic round-trip (forward invertibility)
# =============================================================================

class TestSyntheticRoundTrip:
    """
    Generate a spectrum from the forward at known parameters, then recover
    those parameters with scipy.curve_fit.  Validates that the forward is
    well-conditioned and invertible (precursor to the Phase 5 fitter).

    Uses a wavenumber-dependent gas LUT so the fit has real spectral
    structure to lock onto (a flat OD would make scaling and polynomial
    degenerate).
    """

    def _structured_gas_lut(self) -> GasLUT:
        temp_nodes = np.linspace(300.0, 1000.0, 5)
        # OD with a Gaussian absorption band centred in the window
        band = 0.8 * np.exp(-((_WN - 2475.0) / 15.0) ** 2)
        od_table = np.tile(band, (5, 1))
        return GasLUT(species="SO2", wn=_WN, temp_nodes=temp_nodes,
                      od_table=od_table, ref_scd=1e17)

    def test_recover_scaling_and_poly(self, no_ils):
        from scipy.optimize import curve_fit

        ref = Reference(wn=_WN, gas_luts={"SO2": self._structured_gas_lut()},
                        aero_luts={}, geometry_type=GeometryType.LAYER)
        params = ParameterSet([
            Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
            Parameter("poly1", 0.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
            Parameter("SO2", 1.0, True, (0.0, np.inf),
                      ParameterKind.GAS_PLUME, ref_column=1e17),
            Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
        ])
        fm = LayerForwardModel(ref, params, _geom(), _Retr(),
                               _window(plume=("SO2",), polydeg=1), _WN)

        # Truth parameters (varied order: poly0, poly1, SO2)
        truth = np.array([1.05, -0.1, 2.5])
        y = fm(truth)

        # Fit back from a different start
        f = fm.curve_fit_callable()
        popt, _ = curve_fit(f, _WN, y, p0=[1.0, 0.0, 1.0])
        assert np.allclose(popt, truth, rtol=1e-4)
