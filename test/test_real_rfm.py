"""
Real-RFM integration tests — validate the LUT / reference / forward machinery
against direct RFM (and mieEODG) runs.

These are SKIPPED automatically unless RFM is configured (rfmwrapper installed
and its executable resolvable), so the standard CI suite stays green on
machines without the RFM binary.  Run locally with:

    pytest test/test_real_rfm.py -v

Validated against RFM v5.21_18DEC, HITRAN 2020.

Coverage:
  A  direct rfmwrapper layer run (binary + HITRAN + wrapper chain)
  B  1-D GasLUT vs direct RFM: exact at nodes, bounded interpolation error
  C  2-D H2O continuum GasLUT builds and is monotonic in VMR
  E  build_reference (layer) → LayerForwardModel vs direct multi-gas RFM
  F  emission 3-layer T_eff assembly vs a direct full-column RFM radiance
  G  solar chain: H2O continuum decomposition + SolarForwardModel vs direct RFM
  D  aerosol AerosolLUT ↔ mieEODG: 1-D exact at node, MAC≤MEC, interp, 2-D H2SO4
  H  end-to-end run_from_config on a real layer dataset (smoke test)
"""

from __future__ import annotations

import numpy as np
import pytest

# ---------------------------------------------------------------------------
# Skip the whole module unless RFM is configured
# ---------------------------------------------------------------------------

def _rfm_available() -> bool:
    try:
        import os
        from rfmwrapper import config
        return os.path.isfile(config.get_exec_path())
    except Exception:
        return False


pytestmark = pytest.mark.skipif(
    not _rfm_available(),
    reason="RFM not configured (rfmwrapper executable not found)",
)


# ===========================================================================
# A — direct RFM smoke
# ===========================================================================

def test_a_direct_rfm_layer():
    from rfmwrapper import RFM
    m = RFM(type="layer", wn_start=2400, wn_stop=2550, nperwn=25,
            gas="SO2", pathlength=0.05, pres=1013, temp=450,
            vmr={"SO2": 5.0}, out="OPT", silent=True)
    m.run()
    opt = m.getSpectra("opt")
    assert len(opt.wn) > 100
    assert opt.data[0].max() > 0                  # absorption present
    assert 2480 < opt.wn[np.argmax(opt.data[0])] < 2540   # SO2 ν3 band region


# ===========================================================================
# B — 1-D GasLUT vs direct RFM
# ===========================================================================

@pytest.fixture(scope="module")
def so2_lut():
    from plumeIR.reference.gas_lut import GasLUT
    return GasLUT.compute(
        species="SO2", wn_start=2400, wn_stop=2550, nperwn=25,
        temp_nodes=np.linspace(300, 800, 5), pathlength=0.05, pres=1013,
        ref_scd=1e17, max_workers=4,
    )


def _direct_so2(T: float, ref_scd: float = 1e17) -> np.ndarray:
    from rfmwrapper import RFM
    from plumeIR.units import scd_to_vmr
    vmr = scd_to_vmr(ref_scd, 0.05, temp=T, pres=1013)
    m = RFM(type="layer", wn_start=2400, wn_stop=2550, nperwn=25, gas="SO2",
            pathlength=0.05, pres=1013, temp=T, vmr={"SO2": vmr},
            out="OPT", silent=True)
    m.run()
    return m.getSpectra("opt").data[0]


def test_b_lut_exact_at_node(so2_lut):
    T = float(so2_lut.temp_nodes[2])           # interior node
    od_lut = so2_lut.query(T)
    od_dir = _direct_so2(T)
    # At a node the LUT stores the exact RFM output
    assert np.allclose(od_lut, od_dir, atol=1e-12)


def test_b_lut_interpolation_bounded(so2_lut):
    # Off-node midpoints: linear-interp error bounded for 125 K node spacing.
    for T in (362.5, 487.5):
        od_lut = so2_lut.query(T)
        od_dir = _direct_so2(T)
        rel = np.abs(od_lut - od_dir).max() / od_dir.max()
        assert rel < 0.05, f"interp error {rel:.3f} too large at {T} K"


def test_b_scaling_is_linear(so2_lut):
    # OD scales linearly with the scaling factor (Beer-Lambert).
    T = 450.0
    od1 = 1.0 * so2_lut.query(T)
    od3 = 3.0 * so2_lut.query(T)
    assert np.allclose(od3, 3.0 * od1)


# ===========================================================================
# C — 2-D H2O continuum GasLUT
# ===========================================================================

def test_c_h2o_continuum_builds():
    from plumeIR.reference.gas_lut import GasLUT
    from plumeIR.standards import H2O_VMR_NODES
    lut = GasLUT.compute(
        species="H2O", wn_start=1200, wn_stop=1350, nperwn=25,
        temp_nodes=np.linspace(250, 320, 3),
        pathlength=1.0, pres=1013, ref_scd=5e22,
        vmr_nodes=H2O_VMR_NODES, continuum=True, max_workers=4,
    )
    assert lut.is_2d
    assert lut.od_table.shape == (3, len(H2O_VMR_NODES), len(lut.wn))
    # More water → more absorption: OD increases with VMR at fixed T
    od_lo = lut.query(285.0, float(H2O_VMR_NODES[1]))
    od_hi = lut.query(285.0, float(H2O_VMR_NODES[-2]))
    assert od_hi.sum() > od_lo.sum()


# ===========================================================================
# E — end-to-end: build_reference (layer) → LayerForwardModel vs direct RFM
# ===========================================================================

def test_e_layer_forward_vs_direct_rfm(tmp_path):
    """
    Build a layer Reference for an SO2 window, evaluate LayerForwardModel at
    the model grid, and compare the plume transmittance to a direct RFM
    multi-gas run at the same conditions.  Validates the full chain:
    build_reference → GasLUTs → forward OD assembly.
    """
    from plumeIR.config.objects import LayerGeometry, FitWindowConfig, Retrieval, GeometryType
    from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
    from plumeIR.config.objects import ModelConfig
    from plumeIR.reference.reference import build_reference
    from plumeIR.forward.layer import LayerForwardModel
    from plumeIR.standards import PLUME_TEMP_NODES
    from rfmwrapper import RFM
    from plumeIR.units import scd_to_vmr

    # Use an exact LUT node temperature so the forward query involves no
    # interpolation — this isolates the build→forward chain (interpolation
    # accuracy is covered separately in test B).
    # Selected by value: the index of a node moves when the axis is extended.
    plume_temp = float(PLUME_TEMP_NODES[np.argmin(np.abs(PLUME_TEMP_NODES - 591.67))])
    geom = LayerGeometry(pathlength=0.5, atm_temp=290, atm_pres=1013,
                         plume_temp=plume_temp, plume_pres=1013, plume_thickness=0.05)
    window = FitWindowConfig(label="FIT0", wn_start=2450, wn_stop=2550,
                             atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                             targets=("SO2",), polydeg=0)
    retr = Retrieval(geometry_type=GeometryType.LAYER, fit_fov=False, fit_shift=False)
    model = ModelConfig(nperwn=25.0, model_padding=20.0, cache_dir=tmp_path)

    ref_scd = 5e16
    params = ParameterSet([
        Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
        Parameter("SO2", 1.0, True, (0, np.inf), ParameterKind.GAS_PLUME, ref_column=ref_scd),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])

    ref = build_reference(window, geom, retr, model, params, cache_dir=tmp_path)
    fm = LayerForwardModel(ref, params, geom, retr, window, ref.wn)

    # High-res model = poly(1) · T_plume = exp(-OD_SO2) at scaling 1
    F = fm.model_highres(params.p0_vector())
    od_lut = -np.log(F)                          # recovered SO2 OD

    # Direct RFM at the plume conditions and the same SCD
    vmr = scd_to_vmr(ref_scd, geom.plume_thickness, temp=geom.plume_temp, pres=geom.plume_pres)
    m = RFM(type="layer", wn_start=ref.wn[0], wn_stop=ref.wn[-1], nperwn=25,
            gas="SO2", pathlength=geom.plume_thickness, pres=geom.plume_pres,
            temp=geom.plume_temp, vmr={"SO2": vmr}, out="OPT", silent=True)
    m.run()
    od_dir = m.getSpectra("opt").data[0]

    # Same grid? (both built at nperwn=25 over the padded window)
    assert od_lut.shape == od_dir.shape
    rel = np.abs(od_lut - od_dir).max() / od_dir.max()
    assert rel < 1e-6, f"forward OD vs direct RFM rel err {rel:.2e}"


# ===========================================================================
# F — emission *LEV layer build → 3-layer assembly vs direct full-column RFM
# ===========================================================================

def test_f_emission_assembly_vs_direct_rfm():
    """
    Validate the emission reference + forward end-to-end against direct RFM.

    The layers are built the production way — one RFM ``*LEV`` zenith run per
    H2O_sc node, with each layer's ``(OD, isolated radiance)`` recovered by
    differencing the cumulative columns (``_build_atm_layers_lev``).  The
    three layers, reconstructed from the effective emission temperature
    (``L = rad(T_eff)·(1−T)``) and stacked with

        L = L_dist·T_slice·T_prox + L_slice·T_prox + L_prox

    are evaluated with no plume at ``H2O_sc = 1`` and zero offsets.

    Two checks:
      1. the assembly reproduces the builder's clear-sky radiance (the *LEV
         observer column) to numerical precision — the layer decomposition,
         T_eff round-trip, and forward composition are mutually exact;
      2. it matches an *independent* direct full-column RFM run.  Because the
         *LEV taps share one integration with that column, the agreement is now
         tight (no sliced-vs-full discretization gap — the per-slice build had
         a ~1.5% bias here).
    """
    import os
    from rfmwrapper import readAtm, config as rcfg
    from plumeIR.reference.emission_reference import (
        _build_atm_layers_lev, _zenith_layer_spectra,
    )
    from plumeIR.reference.reference import Reference
    from plumeIR.forward.emission import EmissionForwardModel
    from plumeIR.config.objects import (
        EmissionGeometry, FitWindowConfig, Retrieval, GeometryType,
    )
    from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet

    # Coarse, narrow TIR window keeps the RFM runs fast.
    wn_start, wn_stop, nperwn = 1000.0, 1050.0, 5.0
    gases = "H2O CO2"
    elev  = 90.0
    obs_h, plume_h, plume_thick = 0.5, 2.0, 1.0
    h1 = plume_h - plume_thick / 2.0          # plume bottom
    h2 = plume_h + plume_thick / 2.0          # plume top

    atm_path = os.path.join(rcfg.get_atm_dir(), "std.atm")
    atm = readAtm(atm_path)

    # Build the three layers + clear-sky radiance via one *LEV run per node.
    sc_nodes = np.array([0.5, 1.0, 1.5])      # 1.0 is a node (exact at a priori)
    layers, L_clear, wn = _build_atm_layers_lev(
        atm, obs_h, h1, h2, elev, wn_start, wn_stop, nperwn, gases, sc_nodes,
    )

    ref = Reference(
        wn=wn, geometry_type=GeometryType.EMISSION,
        atm_layers=layers, L_clear=L_clear,
    )
    geom = EmissionGeometry(
        plume_height=plume_h, plume_thickness=plume_thick, elev=elev,
        obs_height=obs_h, atm_path=atm_path,
        plume_temp=350.0, fit_difference=False, use_bbt=False,
    )
    window = FitWindowConfig(
        label="FIT0", wn_start=wn_start, wn_stop=wn_stop,
        atm_gases=("H2O", "CO2"), plume_gases=(), plume_aero=(),
        targets=(), polydeg=0,
    )
    retr = Retrieval(geometry_type=GeometryType.EMISSION, fit_fov=False, fit_shift=False)
    params = ParameterSet([
        Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])

    fm = EmissionForwardModel(ref, params, geom, retr, window, wn)

    # No-plume radiance from the 3-layer assembly (pre-ILS, poly = 1).
    L_model = fm.model_highres(params.p0_vector())
    denom = np.maximum(np.abs(L_clear), L_clear.max() * 1e-6)

    # (1) Assembly reproduces the builder's clear-sky radiance to precision.
    rel_self = np.abs(L_model - L_clear) / denom
    assert rel_self.max() < 1e-5, f"assembly vs L_clear max rel {rel_self.max():.2e}"

    # (2) Independent direct full-column RFM run (plain zenith, no *LEV taps).
    _, L_native, _ = _zenith_layer_spectra(
        atm, obs_h, elev, wn_start, wn_stop, nperwn, gases, h2o_sc=1.0,
    )
    rel_native = np.abs(L_model - L_native) / denom
    assert rel_native.mean() < 5e-3, f"vs direct full column, mean rel {rel_native.mean():.2e}"


# ===========================================================================
# F2 — plume assembly: 2-layer vs 3-layer vs direct RFM
# ===========================================================================

def test_f2_plume_assembly_vs_direct_rfm():
    """
    The 2-layer emission geometry must reproduce a direct RFM run with a real
    plume inserted; the dropped 3-layer form must not.

    Both decompositions are exact cuts of the same clear-sky integration, so a
    clear-sky check cannot separate them (both 1.1e-16).  Inserting a plume
    does: the 3-layer form summed ``(L_plume + L_slice)*T_prox``, letting the
    plume's own emission reach the observer without passing through the slice
    physically in front of half of it.

    Measured against a direct RFM run with an SO2 plume inserted (mls.atm,
    750-1250 cm-1):
    3-layer is wrong by a flat ~14.1% of the plume signal at every opacity,
    2-layer by 0.07% (thin) to 0.97% (mean OD 3.3).

    Two subtleties this test must respect, both of which produced wrong numbers
    first time round:
      * mls.atm carries a background SO2 profile — zero it, or the "plume" OD
        is mostly background.
      * insertPlume RE-GRIDS the atmosphere (50 -> 59 levels).  Differencing a
        plume run against an un-regridded background contaminates OD_plume with
        a discretisation artifact of every gas (mean |dOD| 0.0118, max 0.965,
        with no SO2 present at all).  Difference against a ZERO-plume insert on
        the same grid.
    """
    import os

    import numpy as np
    from rfmwrapper import readAtm, insertPlume
    from rfmwrapper import config as rcfg
    from plumeIR.physics import rad
    from plumeIR.reference.emission_reference import (
        _decompose_layers, _decompose_two_layers, _lev_column_spectra,
    )

    wn0, wn1, nperwn = 750.0, 1250.0, 25.0
    h_obs, h_p, th, elev = 0.2, 1.0, 0.4, 25.0
    h1, h2 = h_p - th / 2, h_p + th / 2
    levels = [h1, h_p, h2]
    gases = "H2O CO2 N2O CH4 O3 SO2"

    atm = dict(readAtm(os.path.join(rcfg.get_atm_dir(), "mls.atm")))
    atm["SO2"] = np.zeros_like(np.asarray(atm["HGT"], dtype=float))
    T_c = float(np.interp(h_p, np.asarray(atm["HGT"], dtype=float),
                          np.asarray(atm["TEM"], dtype=float)))

    base = insertPlume(atm, h_p, th, {"SO2": 0.0}, gradient_flag=0)
    wn, alts, L0, OD0 = _lev_column_spectra(
        base, h_obs, elev, wn0, wn1, nperwn, gases, levels, 1.0)
    i_obs = int(np.argmin(np.abs(alts - h_obs)))
    L_clear = L0[i_obs]

    l3, _ = _decompose_layers(alts, L0, OD0, h_obs, h1, h2)
    l2, _ = _decompose_two_layers(alts, L0, OD0, h_obs, h_p)

    def assemble(lay, OD_plume, n):
        T_p = np.exp(-OD_plume)
        L_p = rad(wn, T_c) * (1.0 - T_p)
        T_prox = np.exp(-lay["prox"][0])
        if n == 3:
            T_sl = np.exp(-lay["slice"][0])
            return (lay["dist"][1] * T_p * T_sl * T_prox
                    + (L_p + lay["slice"][1]) * T_prox + lay["prox"][1])
        return lay["dist"][1] * T_p * T_prox + L_p * T_prox + lay["prox"][1]

    # Clear sky: both exact, and therefore unable to discriminate.
    zero = np.zeros_like(wn)
    for n, lay in ((3, l3), (2, l2)):
        err = np.abs(assemble(lay, zero, n) - L_clear).max() / np.abs(L_clear).max()
        assert err < 1e-12, f"{n}-layer clear-sky assembly off by {err:.2e}"

    # With a plume: 2-layer must track the truth, 3-layer must not.
    for ppm in (2.0, 200.0):
        atm_p = insertPlume(atm, h_p, th, {"SO2": ppm}, gradient_flag=0)
        _w, alts_p, L1, OD1 = _lev_column_spectra(
            atm_p, h_obs, elev, wn0, wn1, nperwn, gases, levels, 1.0)
        j = int(np.argmin(np.abs(alts_p - h_obs)))
        L_direct = L1[j]
        OD_plume = OD1[j] - OD0[i_obs]

        signal = np.abs(L_direct - L_clear).mean()
        e3 = np.abs(assemble(l3, OD_plume, 3) - L_direct).mean() / signal
        e2 = np.abs(assemble(l2, OD_plume, 2) - L_direct).mean() / signal

        assert e2 < 0.02, f"2-layer off by {e2:.1%} of plume signal at {ppm} ppmv"
        assert e3 > 10 * e2, (
            f"3-layer ({e3:.1%}) should be far worse than 2-layer ({e2:.1%}) "
            f"at {ppm} ppmv — if not, the assembly difference has been lost"
        )


# ===========================================================================
# G — solar occultation chain vs direct RFM
# ===========================================================================

def test_g_solar_h2o_continuum_decomposition_vs_rfm():
    """
    The solar H2O column-scaling decomposition ``OD(s) = s·A + s²·B`` (A =
    lines + foreign continuum ∝ column; B = self-continuum ∝ column²) matches
    direct RFM at scalings away from the a priori, where the naive linear
    ``s·OD_ref`` does not.  B is built exactly as ``build_solar_reference``
    does: a second zenith run at scaling=2.
    """
    import os
    from rfmwrapper import readAtm, config as rcfg
    from plumeIR.reference.solar_reference import _vertical_gas_od

    wn_start, wn_stop, nperwn = 1000.0, 1100.0, 5.0
    h_obs = 0.5
    atm = readAtm(os.path.join(rcfg.get_atm_dir(), "std.atm"))

    wn, od1 = _vertical_gas_od(atm, "H2O", h_obs, wn_start, wn_stop, nperwn, scaling=1.0)
    _,  od2 = _vertical_gas_od(atm, "H2O", h_obs, wn_start, wn_stop, nperwn, scaling=2.0)
    B = 0.5 * (od2 - 2.0 * od1)                      # self-continuum component

    for s in (0.5, 1.5):
        _, od_true = _vertical_gas_od(atm, "H2O", h_obs, wn_start, wn_stop, nperwn, scaling=s)
        od_corrected = s * od1 + (s * s - s) * B     # the forward's formula
        od_linear    = s * od1                       # naive (wrong for continuum)

        # Between-line continuum baseline (where the self-continuum dominates).
        cont = (od_true <= np.percentile(od_true, 40)) & (od_true > 0)
        err_corr = np.mean(np.abs(od_corrected - od_true)[cont] /
                           np.maximum(od_true[cont], 1e-30))
        err_lin  = np.mean(np.abs(od_linear - od_true)[cont] /
                           np.maximum(od_true[cont], 1e-30))
        assert err_corr < 5e-3, f"corrected continuum err {err_corr:.2e} at s={s}"
        # The fix must be a large improvement over linear column scaling.
        assert err_lin > 10 * err_corr, f"s={s}: linear {err_lin:.2e} not ≫ corrected {err_corr:.2e}"


def test_g_solar_forward_vs_direct_rfm(tmp_path):
    """
    build_solar_reference → SolarForwardModel reproduces a direct RFM column.
    At airmass = 1 and poly = 1 the forward is ``exp(−OD_vertical)``; compare
    the recovered OD (CO2 at the a priori + H2O scaled to 1.5, exercising the
    continuum term) to direct single-gas RFM runs summed (optical depths add).
    """
    import os
    from pathlib import Path
    from rfmwrapper import readAtm, config as rcfg
    from plumeIR.reference.solar_reference import build_solar_reference, _vertical_gas_od
    from plumeIR.forward.solar import SolarForwardModel
    from plumeIR.config.objects import (
        SolarGeometry, FitWindowConfig, Retrieval, GeometryType, ModelConfig,
    )
    from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet

    wn_start, wn_stop, nperwn, pad = 1000.0, 1100.0, 5.0, 20.0
    h_obs = 0.5
    atm_path = os.path.join(rcfg.get_atm_dir(), "std.atm")
    atm = readAtm(atm_path)

    geom   = SolarGeometry(obs_height=h_obs, atm_path=Path(atm_path), elev=90.0)
    window = FitWindowConfig(
        label="FIT0", wn_start=wn_start, wn_stop=wn_stop,
        atm_gases=("CO2", "H2O"), plume_gases=(), plume_aero=(),
        targets=("CO2", "H2O"), continuum_gases=("H2O",), polydeg=0,
    )
    retr   = Retrieval(geometry_type=GeometryType.SOLAR)
    model  = ModelConfig(nperwn=nperwn, model_padding=pad, cache_dir=tmp_path)
    params = ParameterSet([
        Parameter("poly0", 1.0, True, (-10, 10), ParameterKind.POLYNOMIAL),
        Parameter("CO2", 1.0, True, (0, np.inf), ParameterKind.GAS_ATM, ref_column=1e22),
        Parameter("H2O", 1.0, True, (0, np.inf), ParameterKind.GAS_ATM, ref_column=1e22),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])

    ref = build_solar_reference(window, geom, retr, model, params, cache_dir=tmp_path)
    fm  = SolarForwardModel(ref, params, geom, retr, window, ref.wn, airmass=1.0)

    # H2O column scaled to 1.5 (CO2 at the a priori); poly = 1, airmass = 1.
    p0 = params.p0_vector().copy()
    p0[params.varied_names().index("H2O")] = 1.5
    F = fm.model_highres(p0)
    od_model = -np.log(F)

    # Direct RFM: single-gas vertical OD on the padded model grid; ODs add.
    wn, od_co2 = _vertical_gas_od(atm, "CO2", h_obs, wn_start - pad, wn_stop + pad, nperwn, scaling=1.0)
    _,  od_h2o = _vertical_gas_od(atm, "H2O", h_obs, wn_start - pad, wn_stop + pad, nperwn, scaling=1.5)
    od_direct = od_co2 + od_h2o

    assert od_model.shape == od_direct.shape
    rel = np.abs(od_model - od_direct) / od_direct.max()
    assert rel.max() < 2e-3, f"solar forward OD vs direct RFM, max rel {rel.max():.2e}"


# ===========================================================================
# D — aerosol optical properties vs direct mieEODG
# ===========================================================================
# These validate the AerosolLUT ↔ mieEODG integration (RI loading from ARIA,
# MEC/MAC extraction, scaling, interpolation), not Mie physics itself (mieEODG
# owns that).  mieEODG is a core plumeIR dependency, so it is always present.

_AERO_WN = np.linspace(900.0, 1100.0, 30)
_AERO_R0 = np.geomspace(0.1, 5.0, 4)
_AERO_SCALE = 1e-3 * 1e-4          # ref_mass_g_m2 × 1e-4 (build_1d convention)


@pytest.fixture(scope="module")
def ash_lut():
    from plumeIR.reference.aero_lut import AerosolLUT
    return AerosolLUT.build_1d(
        "ASH", _AERO_WN, _AERO_R0, ref_mass_g_m2=1e-3, with_absorption=True,
    )


def _direct_mie(aria_key: str, r0: float):
    """Direct mieEODG MEC/MAC at r0 on the test grid (the validation truth)."""
    from mieEODG.refindex import ARIADatabase
    from mieEODG.constants import ARIA_DEFAULTS
    from mieEODG import AerosolLUT as _Mie
    ri = ARIADatabase().read(ARIA_DEFAULTS[aria_key], wavenumber=_AERO_WN,
                             out_of_range="clip")
    m = _Mie.load_or_compute(ri=ri, r0_um=_AERO_R0, s=1.5, wn=_AERO_WN, progress=False)
    res = m.forward(float(r0))
    return res["MEC"], res["MAC"]


def test_d_aero_1d_exact_at_node(ash_lut):
    # At an r0 node the LUT stores direct mieEODG MEC × scale exactly.
    mec, _ = _direct_mie("volcanic_ash", _AERO_R0[1])
    assert np.allclose(ash_lut.query(float(_AERO_R0[1])), mec * _AERO_SCALE, atol=1e-18)


def test_d_aero_1d_absorption_le_extinction(ash_lut):
    # Physical: absorption (MAC) ≤ extinction (MEC) everywhere.
    for r0 in _AERO_R0:
        ext  = ash_lut.query(float(r0))
        absn = ash_lut.query_abs(float(r0))
        assert np.all(absn <= ext + 1e-12)


def test_d_aero_1d_interpolation_bounded(ash_lut):
    # Off-node (log-linear in r0) vs direct mieEODG — bounded on the coarse
    # 4-node grid (production uses 24 nodes → far smaller).
    r0 = float(np.sqrt(_AERO_R0[1] * _AERO_R0[2]))
    mec, _ = _direct_mie("volcanic_ash", r0)
    rel = np.abs(ash_lut.query(r0) - mec * _AERO_SCALE).max() / (mec * _AERO_SCALE).max()
    assert rel < 0.1, f"aerosol r0 interpolation error {rel:.3f}"


def test_d_aero_deferred_species_raises():
    # Species with no ARIA refractive-index source fail cleanly (no placeholder).
    from plumeIR.reference.aero_lut import AerosolLUT
    from plumeIR.exceptions import ReferenceComputationError
    for sp in ("SOOT", "OC"):
        with pytest.raises(ReferenceComputationError):
            AerosolLUT.build_1d(sp, _AERO_WN, _AERO_R0, ref_mass_g_m2=1e-3)


def test_d_aero_h2so4_2d_builds_and_queries():
    # 2-D (r0, weight-fraction) build via the canonical ARIA source (Myhre
    # 2003); query at a node is positive and absorption ≤ extinction.
    from plumeIR.reference.aero_lut import AerosolLUT
    from plumeIR.standards import H2SO4_WT_NODES
    r0n = np.geomspace(0.1, 5.0, 3)
    lut = AerosolLUT.build_h2so4_2d(
        wn=_AERO_WN, r0_nodes_um=r0n, wt_nodes=H2SO4_WT_NODES,
        ref_mass_g_m2=1e-3, with_absorption=True,
    )
    assert lut.is_2d and lut.has_absorption
    ext  = lut.query(float(r0n[1]), float(H2SO4_WT_NODES[2]))
    absn = lut.query_abs(float(r0n[1]), float(H2SO4_WT_NODES[2]))
    assert ext.shape == (_AERO_WN.size,)
    assert np.all(ext > 0)
    assert np.all(absn <= ext + 1e-12)


# ===========================================================================
# H — end-to-end run_from_config on a real layer dataset (smoke test)
# ===========================================================================

from data_paths import MIDAC_DIR

_IMO_DATA = str(MIDAC_DIR)


def test_h_run_from_config_end_to_end(tmp_path):
    """
    Drive the whole pipeline on real data: a real layer config (SO2 + HCl
    windows, gas-temperature fitting) pointed at the IMO 2021-08-11 single-beam
    dataset → references (real RFM) → fits → ResultsTable.  A smoke test: it
    checks the chain runs and produces physically sane output, not exact values
    (quantitative v2 comparison is separate).

    The config enables ``subtract_self``, so this also exercises the layer
    preprocessing stage against the dataset's real ``self.spc``.
    """
    import os
    from pathlib import Path
    import numpy as np

    if not os.path.isdir(_IMO_DATA):
        pytest.skip("IMO 2021-08-11 dataset not available")

    from plumeIR.config.parser import read_config, write_config
    from plumeIR.pipeline import run_from_config

    template = Path(__file__).parent / "configs" / "test_layer.ini"
    raw = read_config(str(template))
    raw["DATA"]["data_dir"]     = _IMO_DATA
    raw["DATA"]["filter_files"] = True
    raw["DATA"]["filter_string"] = "sbm"          # plume single-beam (skips self.spc)
    raw["DATA"]["filter_ext"]   = "spc"
    raw["DATA"]["max_spec"]     = 5
    cfg_path = tmp_path / "smoke_imo.ini"
    write_config(raw, cfg_path)

    # Isolated cache: reference pickles are NumPy-version specific, so an
    # env-shared default cache can break across NumPy 1.x/2.x.
    rt = run_from_config(cfg_path, cache_dir=tmp_path)
    df = rt.to_dataframe()

    # Both windows ran for all five spectra.
    assert {"FIT0", "FIT1"} <= set(df["window"].unique())
    assert df["spectrum_id"].nunique() == 5

    # SO2 (FIT0 target) recovered for every spectrum: finite, positive column.
    so2 = rt.species_series("SO2", "scd")
    assert len(so2) == 5
    assert np.all(np.isfinite(so2.values)) and np.all(so2.values > 0)

    # Gas temperature was fitted in FIT0, within the LUT node range.
    gt = df[df["parameter"] == "plume_temp"]
    assert len(gt) >= 1
    assert gt["varied"].all()
    assert gt["value"].between(300.0, 1000.0).all()

    # Most fits converged.
    assert df["success"].mean() > 0.5
