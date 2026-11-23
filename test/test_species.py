"""
Tests for the species registry (``plumeIR.species``, 2026-11-17).

Run with:  pytest test/test_species.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

from plumeIR import species as sp


class TestRegistry:

    def test_no_name_is_both_gas_and_aerosol(self):
        assert not set(sp.gases()) & set(sp.aerosols())
        assert "HNO3" in sp.gases() and "HNO3_aq" in sp.aerosols()
        assert "H2SO4" not in sp.REGISTRY and "H2SO4_aq" in sp.aerosols()

    def test_old_lists_are_views(self):
        from plumeIR import constants as c
        assert c.RFM_GASES == sp.gases(rfm_only=True)
        assert c.POSSIBLE_AEROSOLS == sp.aerosols()
        assert c.WEIGHTS == dict(sp.MOLAR_MASS)
        # the order RFM_GASES had before the registry (configs and LUTs follow it)
        assert c.RFM_GASES[:4] == ["H2O", "CO2", "SO2", "CO"]

    def test_molar_masses_agree_with_formulas(self):
        for formula, m in sp.MOLAR_MASS.items():
            assert m == pytest.approx(sp.formula_mass(formula), rel=2e-4), formula

    def test_every_gas_has_a_molar_mass_and_elements(self):
        for name in sp.gases():
            s = sp.get(name)
            assert s.molar_mass > 0 and s.elements
        assert sp.get("SO2").elements == {"S": 1, "O": 2}
        assert sp.get("OCS").elements == {"O": 1, "C": 1, "S": 1}
        assert sp.get("ClO").elements == {"Cl": 1, "O": 1}

    def test_parse_formula_refuses(self):
        for bad in ("", "h2o", "Xx2", "H2SO4_aq"):
            with pytest.raises(ValueError):
                sp.parse_formula(bad)

    def test_unknown_species(self):
        with pytest.raises(KeyError, match="known"):
            sp.get("SiF4")

    def test_aerosol_types_and_sources(self):
        for name in sp.aerosols():
            s = sp.get(name)
            assert s.aerosol_type in sp.AEROSOL_TYPES
            assert s.molar_mass is None and s.elements == {}
        assert sp.get("H2SO4_aq").solute.formula == "H2SO4"
        assert sp.get("HNO3_aq").solute.fraction == 0.50
        assert sp.get("ASH").components == () and sp.get("ASH").solute is None
        assert sp.get("SOOT").ri_source is None


class TestComponents:

    def test_solution_with_a_parameter(self):
        f = sp.component_fractions("H2SO4_aq", 0.65)
        assert f == {"H2SO4": 0.65, "H2O": pytest.approx(0.35)}
        with pytest.raises(ValueError, match="wt_H2SO4_aq"):
            sp.component_fractions("H2SO4_aq")
        wt = np.array([0.5, 0.7])
        m = sp.component_masses("H2SO4_aq", np.array([2.0, 1.0]), wt)
        np.testing.assert_allclose(m["H2SO4"], [1.0, 0.7])
        np.testing.assert_allclose(m["H2O"], [1.0, 0.3])

    def test_fixed_solution_pure_and_mineral(self):
        assert sp.component_fractions("HNO3_aq") == {"HNO3": 0.5, "H2O": 0.5}
        assert sp.component_fractions("WATER") == {"H2O": 1.0}
        assert sp.component_fractions("ASH") == {}

    def test_columns_are_molecular(self):
        # 1 g m⁻² of H2SO4 = 1e-4 g cm⁻² / 98.079 g mol⁻¹ × N_A
        c = sp.component_columns("H2SO4_aq", 1.0 / 0.65, 0.65)
        assert c["H2SO4"] == pytest.approx(1e-4 / 98.079 * sp.N_A)
        # and back through plumeIR.units
        from plumeIR.units import molec_cm2_to_g_m2
        assert molec_cm2_to_g_m2(c["H2SO4"], "H2SO4") == pytest.approx(1.0)

    def test_density_from_mieeodg(self):
        pytest.importorskip("mieEODG")
        assert sp.density("H2SO4_aq", 0.65) == pytest.approx(1.5108, abs=1e-3)
        assert sp.density("HNO3_aq") == pytest.approx(1.31, abs=0.01)
        assert sp.density("ASH") == pytest.approx(2.6)
        assert sp.density("WATER") == pytest.approx(1.0)
        with pytest.raises(ValueError):
            sp.density("SO2")


class TestResultsComponents:
    """Acid and water content of an aerosol in the results tables."""

    @staticmethod
    def _table():
        import pandas as pd

        from plumeIR.retrieval.results import ResultsTable
        rows = []
        for i, (mass, wt) in enumerate([(2.0, 0.5), (1.0, 0.7), (0.0, 0.65)]):
            sid = f"s{i}"
            base = dict(spectrum_id=sid, window="FIT0", kind="AERO_PLUME", varied=True,
                        pres=900.0, temp=280.0, success=True, rmse=0.01, r2=0.99)
            rows += [dict(base, parameter="H2SO4_aq", species="H2SO4_aq", value=mass, error=0.1,
                          scd=mass, scd_error=0.1, is_target=True),
                     dict(base, parameter="wt_H2SO4_aq", species="H2SO4_aq", value=wt,
                          error=np.nan, scd=np.nan, scd_error=np.nan, is_target=False),
                     dict(base, parameter="ASH", species="ASH", value=1.0, error=0.1,
                          scd=1.0, scd_error=0.1, is_target=True)]
        return ResultsTable(df=pd.DataFrame(rows))

    def test_acid_and_water(self):
        c = self._table().aerosol_components()
        np.testing.assert_allclose(c["H2SO4_aq[H2SO4]_g_m2"], [1.0, 0.7, 0.0])
        np.testing.assert_allclose(c["H2SO4_aq[H2O]_g_m2"], [1.0, 0.3, 0.0])
        np.testing.assert_allclose(c["H2SO4_aq[H2SO4]_g_m2_err"], [0.05, 0.07, 0.065])
        np.testing.assert_allclose(c["H2SO4_aq[H2SO4]_molec_cm2"],
                                   np.array([1.0, 0.7, 0.0]) / 98.079 * sp.N_A / 1e4)
        assert not any(col.startswith("ASH") for col in c.columns)   # mass only

    def test_in_the_wide_table(self, tmp_path):
        import pandas as pd

        from plumeIR.output import WIDE_NAME, write_results
        write_results(self._table(), tmp_path)
        wide = pd.read_csv(tmp_path / WIDE_NAME, index_col=0)
        assert {"H2SO4_aq", "ASH", "H2SO4_aq[H2SO4]_g_m2", "H2SO4_aq[H2SO4]_molec_cm2"} <= set(
            wide.columns)


class TestPerWindowAerosolFit:
    """fit_r0 / fit_wt per window (2026-11-17, developer)."""

    def test_window_overrides_the_retrieval_section(self):
        from pathlib import Path

        from plumeIR.config.builder import build_from_raw
        from plumeIR.config.parser import read_config
        raw = read_config("test/configs/test_emission.ini")
        base = Path("test/configs")
        ps = build_from_raw(raw, base_dir=base)["params"][0]
        assert ps["r0_H2SO4_aq"].vary and not ps["wt_H2SO4_aq"].vary          # inherited
        raw["fits"][0].update(fit_r0=False, fit_wt=True)
        ps = build_from_raw(raw, base_dir=base)["params"][0]
        assert not ps["r0_H2SO4_aq"].vary and not ps["r0_ASH"].vary
        assert ps["wt_H2SO4_aq"].vary

    def test_lists_name_the_aerosols(self):
        from pathlib import Path

        from plumeIR.config.builder import aerosol_switch, build_from_raw
        from plumeIR.config.parser import read_config
        from plumeIR.exceptions import ConfigError
        assert aerosol_switch(None, True, "ASH") and not aerosol_switch(False, True, "ASH")
        assert aerosol_switch(None, ("ASH",), "ASH") and not aerosol_switch(None, ("ASH",), "H2SO4_aq")
        assert aerosol_switch(("H2SO4_aq",), ("ASH",), "H2SO4_aq")      # the window wins
        raw = read_config("test/configs/test_emission.ini")
        base = Path("test/configs")
        raw["RETRIEVAL"]["fit_r0"] = ["ASH"]          # ash's size free, sulphate's held
        ps = build_from_raw(raw, base_dir=base)["params"][0]
        assert ps["r0_ASH"].vary and not ps["r0_H2SO4_aq"].vary
        raw["fits"][0]["fit_r0"] = ["H2SO4_aq"]
        raw["fits"][0]["fit_wt"] = ["H2SO4_aq"]
        ps = build_from_raw(raw, base_dir=base)["params"][0]
        assert not ps["r0_ASH"].vary and ps["r0_H2SO4_aq"].vary and ps["wt_H2SO4_aq"].vary
        raw["fits"][0]["fit_r0"] = ["SMOKE"]
        with pytest.raises(ConfigError, match="unknown aerosol"):
            build_from_raw(raw, base_dir=base)

    def test_text_round_trip_and_legacy_names(self, tmp_path):
        from plumeIR.config.parser import read_config, write_config
        raw = read_config("test/configs/test_emission.ini")
        raw["RETRIEVAL"]["fit_r0"] = ["H2SO4", "ASH"]       # former name
        raw["fits"][0]["fit_wt"] = True
        write_config(raw, tmp_path / "c.ini")
        back = read_config(tmp_path / "c.ini")
        assert back["RETRIEVAL"]["fit_r0"] == ["H2SO4_aq", "ASH"]
        assert back["fits"][0]["fit_wt"] is True
