"""
Tests for the preprocess_report template (``plumeIR.preprocess.template``).

Validated first against the prototypes (2026-11-16): with the inputs of
the registration prototype the template's convolved transmittance is
bit-identical to the prototype's ``model.npz``; with those of
the zero-level prototype it finds the same four opaque bands on
the MIDAC grid (670-706, 1358-1851, 2266-2367, 3535-3936 cm⁻¹).  These tests
codify the parts that need no RFM; the last one runs RFM and skips without it.

Run with:  pytest test/test_preprocess_template.py -v
"""

from __future__ import annotations

from types import SimpleNamespace as NS

import numpy as np
import pytest

import plumeIR.preprocess.template as tmod
from plumeIR.preprocess.template import (
    Template, TemplateSpec, build_template, convolve_blockwise, instrument_range,
    template_spec,
)
from plumeIR.config.objects import LayerGeometry
from plumeIR.forward.base import apply_ils
from plumeIR.forward.ils import make_ils
from plumeIR.units import vmr_to_scd

GEOM = LayerGeometry(pathlength=2.0, atm_temp=284.0, atm_pres=973.0,
                     plume_temp=450.0, plume_pres=973.0, plume_thickness=0.05)
SPEC = TemplateSpec(2000.0, 2200.0, 25, (("CO2", 420.0), ("H2O", 9600.0)),
                    2.0, 284.0, 973.0, (("SO2", 1e18),), 0.05, 450.0, 973.0)
RETR = NS(fov=0.021, max_opd=1.037, apod_type="triangle")


class _Window(NS):
    def atm_name(self, gas: str) -> str:
        return f"{gas}_atm" if gas in self.plume_gases else gas


def _params(**ref_columns) -> dict:
    return {name: NS(ref_column=v) for name, v in ref_columns.items()}


def _ambient_scd(vmr: float) -> float:
    return float(vmr_to_scd(vmr, GEOM.pathlength, temp=GEOM.atm_temp, pres=GEOM.atm_pres))


class TestSpec:

    def test_key_is_stable_and_content_sensitive(self):
        same = TemplateSpec(**SPEC.__dict__)
        assert same.key() == SPEC.key()
        for change in (dict(pathlength=2.1), dict(wn_stop=2201.0),
                       dict(ambient_vmr=(("CO2", 421.0), ("H2O", 9600.0))),
                       dict(plume_scd=())):
            assert TemplateSpec(**{**SPEC.__dict__, **change}).key() != SPEC.key()

    def test_from_windows_unions_gases(self):
        windows = {
            "FIT0": _Window(atm_gases=("H2O",), plume_gases=("SO2",)),
            "FIT1": _Window(atm_gases=("H2O", "CH4"), plume_gases=("HCl",)),
        }
        params = {
            "FIT0": _params(H2O=_ambient_scd(9600.0), SO2=1e17),
            "FIT1": _params(H2O=_ambient_scd(9600.0), CH4=_ambient_scd(1.9), HCl=4e16),
        }
        spec = template_spec(GEOM, windows, params, NS(nperwn=25), (500.0, 4500.0))
        amb = dict(spec.ambient_vmr)
        assert set(amb) == {"H2O", "CH4"}
        assert amb["H2O"] == pytest.approx(9600.0, rel=1e-9)
        assert amb["CH4"] == pytest.approx(1.9, rel=1e-9)
        assert dict(spec.plume_scd) == {"SO2": 1e17, "HCl": 4e16}
        assert (spec.pathlength, spec.plume_thickness) == (2.0, 0.05)

    def test_dual_gas_ambient_part_uses_its_own_column(self):
        windows = {"FIT3": _Window(atm_gases=("CO2",), plume_gases=("CO2",))}
        params = {"FIT3": _params(CO2_atm=_ambient_scd(420.0), CO2=5e20)}
        spec = template_spec(GEOM, windows, params, NS(nperwn=25), (2000.0, 2200.0))
        assert dict(spec.ambient_vmr)["CO2"] == pytest.approx(420.0, rel=1e-9)
        assert dict(spec.plume_scd)["CO2"] == 5e20

    def test_conflicting_columns_keep_the_first_and_warn(self, caplog):
        windows = {"FIT0": _Window(atm_gases=("H2O",), plume_gases=()),
                   "FIT1": _Window(atm_gases=("H2O",), plume_gases=())}
        params = {"FIT0": _params(H2O=_ambient_scd(9600.0)),
                  "FIT1": _params(H2O=_ambient_scd(19000.0))}
        spec = template_spec(GEOM, windows, params, NS(nperwn=25), (2000.0, 2200.0))
        assert dict(spec.ambient_vmr)["H2O"] == pytest.approx(9600.0, rel=1e-9)
        assert "differs from an earlier window" in caplog.text

    def test_non_layer_geometry_refused(self):
        with pytest.raises(NotImplementedError, match="layer"):
            template_spec(NS(), {}, {}, NS(nperwn=25), (1.0, 2.0))

    def test_inverted_range_refused(self):
        with pytest.raises(ValueError, match="increasing"):
            template_spec(GEOM, {}, {}, NS(nperwn=25), (2.0, 1.0))


def test_instrument_range_is_the_common_span():
    spectra = [NS(wn=np.linspace(500, 4500, 10)), NS(wn=np.linspace(510, 4400, 10))]
    assert instrument_range(spectra, margin=5.0) == (515.0, 4395.0)
    with pytest.raises(ValueError):
        instrument_range(spectra, margin=3000.0)


class TestConvolution:

    def test_zero_fov_matches_one_kernel(self):
        """With no FOV broadening every block's kernel is the same."""
        wn = np.arange(1800.0, 2600.0, 0.04)
        f = 1.0 - 0.5 * np.exp(-((wn[:, None] - np.linspace(1810, 2590, 300)) / 0.05) ** 2).sum(1).clip(0, 1)
        k = make_ils(fov=0.0, max_opd=1.037, nper_wn=25, wn_center=2200.0,
                     apod_type="triangle", trim_opd=1.037, trim_fov=0.0)
        got = convolve_blockwise(wn, f, 0.0, 1.037, "triangle", 25)
        np.testing.assert_allclose(got, apply_ils(f, k), rtol=0, atol=1e-14)

    def test_block_uses_its_own_kernel(self):
        """A line's convolved shape follows the kernel at its block's centre."""
        wn = np.arange(2000.0, 2500.0, 0.04)
        f = np.ones_like(wn)
        f[np.argmin(np.abs(wn - 2400.0))] = 0.0
        got = convolve_blockwise(wn, f, 0.1, 1.037, "Hann", 25, block=250.0)
        k = make_ils(fov=0.1, max_opd=1.037, nper_wn=25, wn_center=2375.0,
                     apod_type="Hann", trim_opd=1.037, trim_fov=0.1)
        np.testing.assert_allclose(got[wn >= 2250.0], apply_ils(f, k)[wn >= 2250.0],
                                   atol=1e-14)


def _fake_template(t_ambient: np.ndarray, wn: np.ndarray) -> Template:
    z = np.zeros_like(wn)
    return Template(spec=SPEC, wn=wn, od_ambient=z, od_plume=z,
                    ambient=t_ambient, total=t_ambient, ils={})


class TestOpaqueRegions:

    def test_regions_and_minimum_width(self):
        wn = np.arange(1000.0, 2000.0, 0.5)
        t = np.ones_like(wn)
        t[(wn >= 1200) & (wn <= 1300)] = 1e-5          # 100 cm⁻¹: kept
        t[(wn >= 1500) & (wn <= 1510)] = 1e-5          # 10 cm⁻¹: dropped
        t[(wn >= 1800) & (wn <= 1830)] = 5e-3          # above the threshold
        assert _fake_template(t, wn).opaque_regions() == [(1200.0, 1300.0)]

    def test_on_a_measurement_grid(self):
        wn = np.arange(1000.0, 2000.0, 0.04)
        t = np.where((wn >= 1200) & (wn <= 1300), 1e-6, 1.0)
        wm = np.arange(1010.0, 1990.0, 0.241)
        (lo, hi), = _fake_template(t, wn).opaque_regions(wm)
        assert 1200.0 <= lo < 1200.25 and 1299.75 < hi <= 1300.0

    def test_grid_outside_refused(self):
        wn = np.arange(1000.0, 2000.0, 0.5)
        with pytest.raises(ValueError, match="outside"):
            _fake_template(np.ones_like(wn), wn).on_grid(np.array([900.0, 1500.0]))


class TestCache:

    def test_rfm_runs_once_per_spec(self, tmp_path, monkeypatch):
        calls = []
        wn = np.arange(2000.0, 2200.0, 0.04)

        def fake(spec):
            calls.append(spec)
            return wn, np.full_like(wn, 0.1), np.full_like(wn, 0.2)

        monkeypatch.setattr(tmod, "compute_optical_depths", fake)
        a = build_template(SPEC, RETR, tmp_path)
        b = build_template(SPEC, RETR, tmp_path)
        assert len(calls) == 1
        np.testing.assert_array_equal(a.total, b.total)
        np.testing.assert_allclose(a.total[200:-200], np.exp(-0.3), rtol=1e-12)
        build_template(SPEC, RETR, tmp_path, recompute=True)
        build_template(TemplateSpec(**{**SPEC.__dict__, "pathlength": 1.0}), RETR, tmp_path)
        assert len(calls) == 3

    def test_unreadable_cache_is_recomputed(self, tmp_path, monkeypatch):
        wn = np.arange(2000.0, 2200.0, 0.04)
        monkeypatch.setattr(tmod, "compute_optical_depths",
                            lambda s: (wn, np.zeros_like(wn), np.zeros_like(wn)))
        (tmp_path / f"template_{SPEC.key()[:16]}.npz").write_bytes(b"not a zip")
        t = build_template(SPEC, RETR, tmp_path)
        assert t.wn.size == wn.size


def _rfm_available() -> bool:
    try:
        import os
        from rfmwrapper import config
        return os.path.isfile(config.get_exec_path())
    except Exception:  # noqa: BLE001 — any failure means "not configured"
        return False


@pytest.mark.skipif(not _rfm_available(), reason="RFM not configured")
def test_real_rfm_co2_nu3_core_is_opaque(tmp_path):
    """The CO2 ν3 core over 2 km is black; the window around it is not."""
    spec = TemplateSpec(2150.0, 2450.0, 25, (("CO2", 420.0),), 2.0, 284.0, 973.0)
    t = build_template(spec, RETR, tmp_path)
    regions = t.opaque_regions(t.wn[(t.wn > 2160) & (t.wn < 2440)])
    assert len(regions) == 1
    lo, hi = regions[0]
    assert 2240.0 < lo < 2300.0 and 2350.0 < hi < 2400.0
    assert t.total.max() > 0.5
