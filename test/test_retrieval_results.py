"""
Tests for ResultsTable — aggregation, SCD conversion, target de-duplication.

Synthetic FitResults (no fitting) across 2 spectra × 2 windows, with SO2 a
target in FIT0 but an interferer in FIT1, and HCl a target in FIT1.

Run with:  pytest test/test_retrieval_results.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

from plumeIR.constants import WEIGHTS
from plumeIR.config.objects import FitWindowConfig, LayerGeometry
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.retrieval import FitResult, ResultsTable
from plumeIR.units import molec_cm2_to_g_m2, molec_cm2_to_ppmm

_DUMMY = np.array([1.0, 1.0])


def _fr(window, spectrum, varied, values, errors):
    return FitResult(
        success=True, method="nlls", varied_names=tuple(varied), values=values,
        perror=errors, covariance=None, wn=_DUMMY, measured=_DUMMY, model=_DUMMY,
        rmse=0.01, nfev=10, message="ok", window_label=window, spectrum_id=spectrum,
    )


@pytest.fixture
def setup():
    windows = {
        "FIT0": FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                                atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                                targets=("SO2",), polydeg=0),
        "FIT1": FitWindowConfig(label="FIT1", wn_start=2700.0, wn_stop=2900.0,
                                atm_gases=(), plume_gases=("SO2", "HCl"), plume_aero=(),
                                targets=("HCl",), polydeg=0),
    }
    G = ParameterKind.GAS_PLUME
    P = ParameterKind.POLYNOMIAL
    param_sets = {
        "FIT0": ParameterSet([
            Parameter("poly0", 1.0, True, (-5, 5), P),
            Parameter("SO2", 0.01, True, (0, np.inf), G, ref_column=1e17),
        ]),
        "FIT1": ParameterSet([
            Parameter("poly0", 1.0, True, (-5, 5), P),
            Parameter("SO2", 0.01, True, (0, np.inf), G, ref_column=1e17),
            Parameter("HCl", 0.01, True, (0, np.inf), G, ref_column=2e16),
        ]),
    }
    fits = []
    for spec, so2_t, so2_i, hcl in (("t0", 2.0, 1.8, 3.0), ("t1", 2.5, 1.9, 3.5)):
        fits.append(_fr("FIT0", spec, ["poly0", "SO2"],
                        {"poly0": 1.0, "SO2": so2_t}, {"poly0": 0.01, "SO2": 0.1}))
        fits.append(_fr("FIT1", spec, ["poly0", "SO2", "HCl"],
                        {"poly0": 1.0, "SO2": so2_i, "HCl": hcl},
                        {"poly0": 0.01, "SO2": 0.1, "HCl": 0.2}))
    return ResultsTable.from_fits(fits, windows, param_sets)


def test_long_format_row_count(setup):
    # 2 spectra × (2 params FIT0 + 3 params FIT1) = 10 rows.
    assert len(setup) == 10


def test_scd_conversion(setup):
    df = setup.to_dataframe()
    row = df[(df.spectrum_id == "t0") & (df.window == "FIT0") & (df.parameter == "SO2")]
    assert float(row.scd.iloc[0]) == pytest.approx(2.0 * 1e17)
    assert float(row.scd_error.iloc[0]) == pytest.approx(0.1 * 1e17)
    # Polynomial has no SCD.
    poly = df[(df.parameter == "poly0")]
    assert poly.scd.isna().all()


def test_target_dedup(setup):
    tgt = setup.targets()
    # SO2 only from FIT0 (its target window), HCl only from FIT1.
    assert set(tgt[tgt.species == "SO2"].window) == {"FIT0"}
    assert set(tgt[tgt.species == "HCl"].window) == {"FIT1"}
    # The FIT1 SO2 interferer is excluded from targets but present in the full table.
    assert not ((tgt.window == "FIT1") & (tgt.species == "SO2")).any()
    full = setup.to_dataframe()
    assert ((full.window == "FIT1") & (full.species == "SO2")).any()


def test_species_series(setup):
    so2 = setup.species_series("SO2")               # trusted (FIT0) SCD per spectrum
    assert so2.loc["t0"] == pytest.approx(2.0e17)
    assert so2.loc["t1"] == pytest.approx(2.5e17)
    hcl = setup.species_series("HCl")
    assert hcl.loc["t0"] == pytest.approx(3.0 * 2e16)


def test_wide_view(setup):
    # rows = spectra, columns = species (trusted target SCDs).
    w = setup.wide()
    assert list(w.index) == ["t0", "t1"]
    assert set(w.columns) == {"SO2", "HCl"}
    assert w.loc["t0", "SO2"] == pytest.approx(2.0e17)
    assert w.loc["t1", "HCl"] == pytest.approx(3.5 * 2e16)


def test_to_csv(setup, tmp_path):
    p = tmp_path / "results.csv"
    setup.to_csv(p)
    assert p.exists() and p.stat().st_size > 0


# ---------------------------------------------------------------------------
# Derived converters + CSV round-trip (must work on a *loaded* table)
# ---------------------------------------------------------------------------

_PLUME_PRES, _PLUME_TEMP = 950.0, 420.0


@pytest.fixture
def setup_geo():
    G, P = ParameterKind.GAS_PLUME, ParameterKind.POLYNOMIAL
    windows = {"FIT0": FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                                       atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                                       targets=("SO2",), polydeg=0)}
    param_sets = {"FIT0": ParameterSet([
        Parameter("poly0", 1.0, True, (-5, 5), P),
        Parameter("SO2", 0.01, True, (0, np.inf), G, ref_column=1e17),
    ])}
    geom = LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                         plume_temp=_PLUME_TEMP, plume_pres=_PLUME_PRES,
                         plume_thickness=0.05)
    fits = [_fr("FIT0", "t0", ["poly0", "SO2"],
                {"poly0": 1.0, "SO2": 2.0}, {"poly0": 0.01, "SO2": 0.1})]
    return ResultsTable.from_fits(fits, windows, param_sets,
                                  geometries={"FIT0": geom})


def test_mass_loading_no_external_input(setup_geo):
    ml = setup_geo.mass_loading()
    df = setup_geo.to_dataframe()
    i = df.index[df.parameter == "SO2"][0]
    expected = molec_cm2_to_g_m2(2.0 * 1e17, "SO2")
    assert ml[i] == pytest.approx(expected)
    assert WEIGHTS["SO2"] > 0                          # came from the registry


def test_to_ppm_m_uses_recorded_pt(setup_geo):
    df = setup_geo.to_dataframe()
    i = df.index[df.parameter == "SO2"][0]
    # P/T were recorded from the geometry, not re-supplied.
    assert df.loc[i, "pres"] == _PLUME_PRES and df.loc[i, "temp"] == _PLUME_TEMP
    ppmm = setup_geo.to_ppm_m()
    expected = molec_cm2_to_ppmm(2.0 * 1e17, pres=_PLUME_PRES, temp=_PLUME_TEMP)
    assert ppmm[i] == pytest.approx(expected)


def test_fixed_params_are_recorded():
    # Fixed parameters (here gas_temp) must appear with varied=False and no
    # error; varied ones (SO2) with varied=True and an error.
    G, P, T = ParameterKind.GAS_PLUME, ParameterKind.POLYNOMIAL, ParameterKind.TEMPERATURE
    windows = {"FIT0": FitWindowConfig(label="FIT0", wn_start=2400.0, wn_stop=2550.0,
                                       atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                                       targets=("SO2",), polydeg=0)}
    param_sets = {"FIT0": ParameterSet([
        Parameter("poly0", 1.0, True, (-5, 5), P),
        Parameter("SO2", 0.01, True, (0, np.inf), G, ref_column=1e17),
        Parameter("gas_temp", 450.0, False, (300, 1000), T),   # FIXED
    ])}
    fr = FitResult(
        success=True, method="nlls", varied_names=("poly0", "SO2"),
        values={"poly0": 1.0, "SO2": 2.0, "gas_temp": 450.0},
        perror={"poly0": 0.01, "SO2": 0.1}, covariance=None,
        wn=_DUMMY, measured=_DUMMY, model=_DUMMY, rmse=0.01, nfev=10,
        message="ok", window_label="FIT0", spectrum_id="t0",
    )
    df = ResultsTable.from_fits([fr], windows, param_sets).to_dataframe()
    gt = df[df.parameter == "gas_temp"].iloc[0]
    assert gt.value == pytest.approx(450.0)        # K, recorded
    assert not bool(gt.varied)
    assert np.isnan(gt.error)
    so2 = df[df.parameter == "SO2"].iloc[0]
    assert bool(so2.varied) and so2.error == pytest.approx(0.1)
    # All three parameters present (poly0, SO2, gas_temp).
    assert set(df.parameter) == {"poly0", "SO2", "gas_temp"}


def test_converters_survive_csv_roundtrip(setup_geo, tmp_path):
    p = tmp_path / "r.csv"
    setup_geo.to_csv(p)
    loaded = ResultsTable.from_csv(p)
    # Same derived quantities from the reloaded table — nothing re-supplied.
    assert np.allclose(loaded.mass_loading().dropna(),
                       setup_geo.mass_loading().dropna())
    assert np.allclose(loaded.to_ppm_m().dropna(),
                       setup_geo.to_ppm_m().dropna())
    # Bool columns coerced back correctly.
    assert loaded.targets().species.tolist() == ["SO2"]
