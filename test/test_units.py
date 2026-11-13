"""
Unit tests for plumeIR.units.

Each function is tested against:
  - A known analytic result (where derivable)
  - Round-trip consistency (forward then inverse recovers the input)
  - Broadcasting: scalar in → scalar out; array in → array out, same shape

Run with:  pytest test/test_units.py -v
"""

import numpy as np
import pytest

from plumeIR.units import (
    vmr_to_scd,
    scd_to_vmr,
    ppmm_to_molec_cm2,
    molec_cm2_to_ppmm,
    molec_cm2_to_g_m2,
    g_m2_to_molec_cm2,
    saturation_pressure,
    rh_to_vmr,
    vmr_to_rh,
    c_to_k, k_to_c,
    f_to_c, c_to_f,
    f_to_k, k_to_f,
)

# Physical constants (must match units.py exactly)
_N_A = 6.02214076e23
_R   = 8.314462618e-2

ATOL = 1e-6   # relative tolerance for floating-point comparisons
RTOL = 1e-6


# =============================================================================
# vmr_to_scd / scd_to_vmr
# =============================================================================

class TestVmrToScd:

    def test_molec_cm2_known_value(self):
        # Analytic check: 1 ppmv SO2, 1 km path, 298 K, 1013 mbar, zenith
        # n_tot = 1013 / (8.314e-2 * 298) = 40.89 mol/m³
        # n_gas = 1e-6 * n_tot * N_A = 2.463e16 molec/m³
        # SCD = n_gas * 1000 m * 1e-4 = 2.463e15 molec/cm²
        vmr, L, T, P = 1.0, 1.0, 298.0, 1013.0
        scd = vmr_to_scd(vmr, L, temp=T, pres=P, elev=90.0)
        n_tot = P / _R / T
        n_gas = vmr * 1e-6 * n_tot * _N_A
        expected = n_gas * L * 1e3 * 1e-4
        assert abs(scd - expected) / expected < RTOL

    def test_ppmm_output(self):
        # ppmm = vmr [ppm] * pathlength [km] * 1e3 → [ppm·m]
        assert vmr_to_scd(2.0, 0.5, output="ppmm") == pytest.approx(2.0 * 0.5 * 1e3)

    def test_ppmm_ignores_tp(self):
        # T and P must not affect ppmm output
        a = vmr_to_scd(1.0, 1.0, temp=200.0, pres=500.0, output="ppmm")
        b = vmr_to_scd(1.0, 1.0, temp=300.0, pres=1013.0, output="ppmm")
        assert a == pytest.approx(b)

    def test_elev_correction(self):
        # Slant path at 30° elevation should be 2× the zenith SCD (sin 30° = 0.5)
        scd_zen  = vmr_to_scd(1.0, 1.0, elev=90.0)
        scd_slant = vmr_to_scd(1.0, 1.0, elev=30.0)
        assert scd_slant == pytest.approx(scd_zen * 2.0, rel=RTOL)

    def test_profile_vs_homogeneous(self):
        # Uniform profile of constant VMR over 1 km should equal homogeneous result
        vmr_scalar = 1.0
        L_scalar = 1.0
        alt = np.array([0.0, 0.5, 1.0])   # km
        vmr_prof = np.full(3, vmr_scalar)
        T_prof = np.full(3, 298.0)
        P_prof = np.full(3, 1013.0)

        scd_hom  = vmr_to_scd(vmr_scalar, L_scalar, temp=298.0, pres=1013.0)
        scd_prof = vmr_to_scd(vmr_prof, alt, temp=T_prof, pres=P_prof)
        assert scd_prof == pytest.approx(scd_hom, rel=1e-4)

    def test_roundtrip_molec_cm2(self):
        vmr0 = 5.0   # ppmv
        scd  = vmr_to_scd(vmr0, 2.0, temp=250.0, pres=800.0)
        vmr1 = scd_to_vmr(scd, 2.0, temp=250.0, pres=800.0)
        assert vmr1 == pytest.approx(vmr0, rel=RTOL)

    def test_roundtrip_ppmm(self):
        vmr0 = 3.0
        scd  = vmr_to_scd(vmr0, 1.5, output="ppmm")
        vmr1 = scd_to_vmr(scd, 1.5, input_unit="ppmm")
        assert vmr1 == pytest.approx(vmr0, rel=RTOL)

    def test_array_input(self):
        vmr = np.array([1.0, 2.0, 3.0])
        scd = vmr_to_scd(vmr, 1.0)
        assert scd.shape == (3,)
        assert np.allclose(scd, scd[0] * np.array([1, 2, 3]), rtol=RTOL)


class TestScdToVmr:

    def test_g_m2_requires_species(self):
        with pytest.raises(ValueError, match="species"):
            scd_to_vmr(1.0, 1.0, input_unit="g/m2")

    def test_g_m2_roundtrip(self):
        vmr0 = 2.0
        scd_mcm = vmr_to_scd(vmr0, 1.0)
        scd_gm2 = molec_cm2_to_g_m2(scd_mcm, "SO2")
        vmr1    = scd_to_vmr(scd_gm2, 1.0, input_unit="g/m2", species="SO2")
        assert vmr1 == pytest.approx(vmr0, rel=RTOL)


# =============================================================================
# ppmm_to_molec_cm2 / molec_cm2_to_ppmm
# =============================================================================

class TestPpmmMolecCm2:

    def test_roundtrip_scalar(self):
        ppmm0 = 10.0
        mcm  = ppmm_to_molec_cm2(ppmm0, pres=900.0, temp=280.0)
        ppmm1 = molec_cm2_to_ppmm(mcm, pres=900.0, temp=280.0)
        assert ppmm1 == pytest.approx(ppmm0, rel=RTOL)

    def test_known_value(self):
        # At STP-ish (1013 mbar, 298 K): 1 ppm·m = ?
        # mcm = 1e-6 * 1013 / R / 298 * N_A * 1e-4
        ppmm = 1.0
        mcm = ppmm_to_molec_cm2(ppmm, pres=1013.0, temp=298.0)
        expected = 1e-6 * 1013.0 / _R / 298.0 * _N_A * 1e-4
        assert mcm == pytest.approx(expected, rel=RTOL)

    def test_profile_equals_homogeneous_for_uniform(self):
        # Uniform T and P profile over 1 km should match single-layer result
        alt = np.array([0.0, 0.5, 1.0])
        ppmm_scalar = 5.0
        T_scalar, P_scalar = 298.0, 1013.0

        mcm_hom  = ppmm_to_molec_cm2(ppmm_scalar, pres=P_scalar, temp=T_scalar)
        # Profile: ppmm per layer = total ppmm / path, but for a uniform layer
        # we supply the same ppmm at each level and integrate over 1 km
        ppmm_prof = np.full(3, ppmm_scalar / 1e3)  # ppm·m per metre → uniform density
        T_prof = np.full(3, T_scalar)
        P_prof = np.full(3, P_scalar)
        mcm_prof = ppmm_to_molec_cm2(ppmm_prof, pres=P_prof, temp=T_prof, pathlength=alt)
        # Both should give the same integrated column
        assert mcm_prof == pytest.approx(mcm_hom, rel=1e-3)

    def test_array_input(self):
        ppmm = np.array([1.0, 2.0, 4.0])
        mcm  = ppmm_to_molec_cm2(ppmm)
        assert mcm.shape == (3,)
        assert np.allclose(mcm / mcm[0], np.array([1.0, 2.0, 4.0]), rtol=RTOL)


# =============================================================================
# molec_cm2_to_g_m2 / g_m2_to_molec_cm2
# =============================================================================

class TestMassConversions:

    @pytest.mark.parametrize("species", ["SO2", "HCl", "H2O", "CO2", "H2SO4"])
    def test_roundtrip(self, species):
        mcm0 = 1e16
        gm2  = molec_cm2_to_g_m2(mcm0, species)
        mcm1 = g_m2_to_molec_cm2(gm2, species)
        assert mcm1 == pytest.approx(mcm0, rel=RTOL)

    def test_so2_known_value(self):
        # SO2 molecular weight = 64.064 g/mol
        # 1e16 molec/cm² × 1e4 cm²/m² / N_A × 64.064 g/mol
        mcm   = 1e16
        gm2   = molec_cm2_to_g_m2(mcm, "SO2")
        expected = mcm * 1e4 / _N_A * 64.064
        assert gm2 == pytest.approx(expected, rel=RTOL)

    def test_array_input(self):
        mcm = np.array([1e15, 1e16, 1e17])
        gm2 = molec_cm2_to_g_m2(mcm, "SO2")
        assert gm2.shape == (3,)


# =============================================================================
# saturation_pressure / rh_to_vmr / vmr_to_rh
# =============================================================================

class TestSaturationPressure:

    def test_liquid_branch_at_300k(self):
        # At 300 K saturation pressure is ~35.4 mbar (within ~1% of tabulated)
        psat = saturation_pressure(300.0)
        assert 34.0 < psat < 37.0

    def test_ice_branch_at_260k(self):
        # At 260 K saturation pressure over ice is ~2.0 mbar
        psat = saturation_pressure(260.0)
        assert 1.5 < psat < 2.5

    def test_scalar_returns_float(self):
        assert isinstance(saturation_pressure(300.0), float)

    def test_array_returns_array(self):
        result = saturation_pressure(np.array([260.0, 280.0, 300.0]))
        assert result.shape == (3,)

    def test_branch_transition(self):
        # Values either side of 273.16 K should be close (no discontinuity)
        p_above = saturation_pressure(273.17)
        p_below = saturation_pressure(273.15)
        assert abs(p_above - p_below) / p_above < 0.01  # < 1% jump


class TestRhVmr:

    def test_roundtrip(self):
        rh0  = 60.0
        vmr  = rh_to_vmr(rh0, pres=1013.0, temp=298.0)
        rh1  = vmr_to_rh(vmr, pres=1013.0, temp=298.0)
        assert rh1 == pytest.approx(rh0, rel=RTOL)

    def test_100_percent_rh(self):
        # At 100% RH the VMR equals Psat/P × 1e6
        pres, temp = 1013.0, 298.0
        vmr = rh_to_vmr(100.0, pres=pres, temp=temp)
        psat = saturation_pressure(temp)
        assert vmr == pytest.approx(psat / pres * 1e6, rel=RTOL)

    def test_array_input(self):
        rh = np.array([20.0, 50.0, 80.0])
        vmr = rh_to_vmr(rh)
        assert vmr.shape == (3,)
        assert np.all(vmr > 0)


# =============================================================================
# Temperature conversions
# =============================================================================

class TestTemperatureConversions:

    def test_c_to_k(self):
        assert c_to_k(0.0)   == pytest.approx(273.15)
        assert c_to_k(100.0) == pytest.approx(373.15)

    def test_k_to_c(self):
        assert k_to_c(273.15) == pytest.approx(0.0)
        assert k_to_c(373.15) == pytest.approx(100.0)

    def test_c_k_roundtrip(self):
        assert k_to_c(c_to_k(25.0)) == pytest.approx(25.0)

    def test_f_to_c(self):
        assert f_to_c(32.0)  == pytest.approx(0.0)
        assert f_to_c(212.0) == pytest.approx(100.0)

    def test_c_to_f(self):
        assert c_to_f(0.0)   == pytest.approx(32.0)
        assert c_to_f(100.0) == pytest.approx(212.0)

    def test_f_c_roundtrip(self):
        assert c_to_f(f_to_c(98.6)) == pytest.approx(98.6)

    def test_f_to_k(self):
        assert f_to_k(32.0) == pytest.approx(273.15)

    def test_k_to_f(self):
        assert k_to_f(273.15) == pytest.approx(32.0, abs=1e-4)

    def test_k_f_roundtrip(self):
        assert k_to_f(f_to_k(98.6)) == pytest.approx(98.6)

    def test_array_broadcasting(self):
        temps_c = np.array([0.0, 25.0, 100.0])
        temps_k = c_to_k(temps_c)
        assert temps_k.shape == (3,)
        assert np.allclose(k_to_c(temps_k), temps_c)
