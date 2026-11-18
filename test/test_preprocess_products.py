"""
Tests for the whole-spectrum preprocessing (``plumeIR.preprocess.products``) and its
config keys.

Validated first through the command line (2026-11-16): with both products in
report mode (the default) the layer and emission validation runs reproduce
their records (layer bit for bit, emission to 1.6e-14); in apply mode the
layer ``nu_shift`` leftovers fall to ≤ 0.012 cm⁻¹ and the emission R² rises
0.99754 → 0.99771.  These tests use a synthetic run: a line forest with opaque
bands, a self spectrum, and spectra displaced by a known scale.

Run with:  pytest test/test_preprocess_products.py -v
"""

from __future__ import annotations

from datetime import datetime, timedelta
from pathlib import Path
from types import SimpleNamespace as NS

import numpy as np
import pytest
from scipy.interpolate import CubicSpline

import plumeIR.preprocess.products as products
from plumeIR.preprocess.products import preprocess_products
from plumeIR.preprocess.template import Template, TemplateSpec
from plumeIR.config.objects import LayerGeometry, PreprocessConfig, SolarGeometry
from plumeIR.exceptions import PlumeIRError
from plumeIR.output import PREP_NAME, write_preprocess
from plumeIR.spectra.spectrum import Spectrum, SpectrumType

WN_MODEL = np.arange(1000.0, 4000.0, 0.04)
WN_MEAS = np.arange(1020.0, 3980.0, 0.241)
BANDS = [(1400.0, 1500.0), (2300.0, 2360.0), (3500.0, 3600.0)]
EPS = -1.5e-4
A_SELF, C_ZERO = 0.84, 80.0
T0 = datetime(2021, 8, 11, 1, 52, 55)


def _forest() -> np.ndarray:
    rng = np.random.default_rng(0)
    od = np.zeros_like(WN_MODEL)
    for c, d in zip(rng.uniform(1000, 4000, 4000), rng.uniform(0.05, 0.6, 4000)):
        sel = np.abs(WN_MODEL - c) < 1.25
        od[sel] += d * np.exp(-((WN_MODEL[sel] - c) / 0.25) ** 2)
    t = np.exp(-od)
    for lo, hi in BANDS:
        t[(WN_MODEL >= lo) & (WN_MODEL <= hi)] = 0.0
    return t


T_MODEL = _forest()
SELF = 5000.0 * np.exp(-(WN_MEAS - 1000.0) / 400.0) + 5.0


def _template() -> Template:
    spec = TemplateSpec(1000.0, 4000.0, 25, (("H2O", 1e4),), 2.0, 284.0, 973.0)
    z = np.zeros_like(WN_MODEL)
    return Template(spec=spec, wn=WN_MODEL, od_ambient=z, od_plume=z,
                    ambient=T_MODEL, total=T_MODEL, ils={})


def _raw(k: int) -> Spectrum:
    """Source × template displaced by ε·ν, plus the self spectrum and a level."""
    y = 1.2e5 * CubicSpline(WN_MODEL, T_MODEL)(WN_MEAS - EPS * WN_MEAS)
    y = y + A_SELF * SELF + C_ZERO
    return Spectrum(x=WN_MEAS.copy(), y=y, x_unit="wn", timestamp=T0 + timedelta(seconds=4 * k),
                    filename=Path(f"plume_{k:04d}_sbm.spc"), spec_type=SpectrumType.SBM)


def _setup(n: int = 5, **pre) -> NS:
    raw = [_raw(k) for k in range(n)]
    geom = LayerGeometry(pathlength=2.0, atm_temp=284.0, atm_pres=973.0,
                         plume_temp=450.0, plume_pres=973.0, plume_thickness=0.05)
    return NS(geometry=geom, data=NS(data_dir=".", filter_ext=None), model=NS(cache_dir=None),
              retrieval=NS(fov=0.0, max_opd=1.037, apod_type="triangle"), windows={},
              params={}, references={}, preprocess=PreprocessConfig(**pre),
              raw=raw, spectra=[Spectrum(**{**s.__dict__, "y": s.y - SELF}) for s in raw])


@pytest.fixture(autouse=True)
def _fake_heavy_steps(monkeypatch):
    monkeypatch.setattr(products, "_layer_template", lambda setup, cache: _template())
    monkeypatch.setattr(products, "_self_background",
                        lambda data, pre: NS(at=lambda t, x=None: SELF.copy()))


class TestModes:

    def test_off_returns_the_spectra_untouched(self):
        s = _setup(background="off", registration="off")
        out, char = preprocess_products(s)
        assert all(a is b for a, b in zip(out, s.spectra))
        assert (char.background, char.registration) == ("off", "off")
        assert list(char.table.columns) == ["spectrum_id"]

    def test_report_measures_and_changes_nothing(self):
        s = _setup()
        out, char = preprocess_products(s)
        assert all(a is b for a, b in zip(out, s.spectra))
        t = char.table
        assert len(t) == 5
        np.testing.assert_allclose(t.bkg_a, A_SELF, rtol=1e-6)
        np.testing.assert_allclose(t.reg_eps, EPS, atol=3e-6)
        np.testing.assert_allclose(t.reg_s0, 0.0, atol=0.01)

    def test_apply_registration_relabels_the_axis(self):
        s = _setup(background="off", registration="apply")
        out, char = preprocess_products(s)
        t = char.table
        for sp, orig, s0, e in zip(out, s.spectra, t.reg_s0, t.reg_eps):
            np.testing.assert_allclose(sp.x, orig.x - (s0 + e * orig.x))
            np.testing.assert_array_equal(sp.y, orig.y)
        # the relabelled spectrum lines up with the template
        sp = out[0]
        expect = 1.2e5 * CubicSpline(WN_MODEL, T_MODEL)(sp.x) + C_ZERO
        open_ = T_MODEL[np.searchsorted(WN_MODEL, sp.x).clip(0, WN_MODEL.size - 1)] > 0.1
        resid = (sp.y + SELF - A_SELF * SELF - expect)[open_]
        assert np.sqrt(np.mean(resid ** 2)) < 0.02 * 1.2e5

    def test_each_spectrum_registered_on_its_own(self):
        """No smoothing: a spectrum's value does not depend on the others."""
        s = _setup(background="off")
        alone = _setup(n=1, background="off")
        _, char = preprocess_products(s)
        _, one = preprocess_products(alone)
        assert char.table.reg_eps.iloc[0] == one.table.reg_eps.iloc[0]

    def test_failed_spectrum_keeps_its_axis(self, monkeypatch):
        real = products.fit_registration
        calls = {"n": 0}

        def flaky(lags, min_peak):
            calls["n"] += 1
            if calls["n"] == 2:
                raise PlumeIRError("too few sub-bands")
            return real(lags, min_peak)

        monkeypatch.setattr(products, "fit_registration", flaky)
        s = _setup(background="off", registration="apply")
        out, char = preprocess_products(s)
        np.testing.assert_array_equal(out[1].x, s.spectra[1].x)
        assert not np.array_equal(out[0].x, s.spectra[0].x)
        assert np.isnan(char.table.reg_eps.iloc[1])
        assert any("failed for 1 spectra" in n for n in char.notes)

    def test_apply_background_subtracts_unclipped(self):
        s = _setup(background="apply", registration="off")
        out, _ = preprocess_products(s)
        for sp, raw in zip(out, s.raw):
            band = (raw.wn >= 2302.0) & (raw.wn <= 2358.0)   # inside the displaced edges
            assert abs(np.mean(sp.y[band])) < 1.0
            np.testing.assert_allclose(sp.y, raw.y - A_SELF * SELF - C_ZERO, atol=1e-3)


class TestEnvelope:

    def test_layer_columns_and_relative_table(self):
        _, char = preprocess_products(_setup())
        t = char.table
        for col in ("env_peak", "env_peak_wn", "env_integral", "env_scale"):
            assert t[col].notna().all(), col
        assert "dT" not in t and not any("dT" in c for c in t.columns)
        assert list(char.relative.columns) == ["f", "dT", "rms", "n"]
        # identical spectra: scale 1 and no temperature change
        np.testing.assert_allclose(char.relative.f, 1.0, rtol=1e-6)
        np.testing.assert_allclose(char.relative.dT, 0.0, atol=1.0)
        assert char.reference_spectra == 10
        assert any(line.startswith("envelope") for line in char.summary_lines())

    def test_reference_spectra_sets_the_reference(self):
        s = _setup(reference_spectra=2)
        s.spectra[3] = Spectrum(**{**s.spectra[3].__dict__, "y": 0.5 * s.spectra[3].y})
        _, char = preprocess_products(s)
        assert char.relative.f.iloc[3] == pytest.approx(0.5, rel=1e-3)
        assert char.relative.f.iloc[0] == pytest.approx(1.0, rel=1e-6)


class TestFailures:

    def test_report_failure_is_noted(self, monkeypatch):
        def broken(*a, **k):
            raise RuntimeError("RFM not found")
        monkeypatch.setattr(products, "_layer_template", broken)
        s = _setup()
        out, char = preprocess_products(s)
        assert all(a is b for a, b in zip(out, s.spectra))
        assert any("template not built" in n for n in char.notes)

    def test_apply_failure_stops_the_run(self, monkeypatch):
        def broken(*a, **k):
            raise RuntimeError("RFM not found")
        monkeypatch.setattr(products, "_layer_template", broken)
        with pytest.raises(RuntimeError, match="RFM"):
            preprocess_products(_setup(registration="apply"))

    def test_solar_has_no_products(self):
        s = _setup()
        s.geometry = SolarGeometry.__new__(SolarGeometry)
        out, char = preprocess_products(s)
        assert (char.background, char.registration) == ("off", "off")


def _run_setup(s, spectra, report):
    """What write_preprocess reads from a RunSetup."""
    from plumeIR.config.objects import FitWindowConfig, GeometryType
    w = FitWindowConfig(label="FIT0", wn_start=2450.0, wn_stop=2550.0, atm_gases=(),
                        plume_gases=("SO2",), plume_aero=(), targets=("SO2",), polydeg=2)
    return NS(preprocess_report=report, spectra=spectra, windows={"FIT0": w},
              retrieval=NS(geometry_type=GeometryType.LAYER))


class TestOutput:

    def test_summary_and_files(self, tmp_path):
        s = _setup()
        spectra, char = preprocess_products(s)
        text = "\n".join(char.summary_lines())
        assert "background report, registration report" in text
        assert "self scale a median 0.840" in text
        assert "registration    eps median -1.50" in text
        written = write_preprocess(_run_setup(s, spectra, char), tmp_path)
        assert [p.name for p in written] == [PREP_NAME, "preprocess.npz"]

    def test_nothing_written_when_off(self, tmp_path):
        s = _setup(background="off", registration="off")
        spectra, char = preprocess_products(s)
        assert write_preprocess(_run_setup(s, spectra, char), tmp_path) == []
        assert write_preprocess(NS(preprocess_report=None), tmp_path) == []


class TestArchive:

    @pytest.fixture
    def archive(self, tmp_path):
        from plumeIR.preprocess.archive import read_preprocess_archive
        s = _setup()
        spectra, char = preprocess_products(s)
        write_preprocess(_run_setup(s, spectra, char), tmp_path)
        return spectra, char, read_preprocess_archive(tmp_path)

    def test_round_trip(self, archive):
        spectra, char, arc = archive
        assert len(arc) == 5 and arc.geometry == "layer"
        assert arc.spectrum_ids == list(char.table.spectrum_id)
        wn, y = arc.spectrum(2)
        np.testing.assert_array_equal(wn, spectra[2].wn)
        np.testing.assert_allclose(y, spectra[2].y, rtol=1e-6)      # float32
        assert arc.windows == {"FIT0": (2450.0, 2550.0)}
        assert arc.coarse("top", 0).shape == arc.get("wn_coarse").shape
        np.testing.assert_allclose(arc.relative.f, char.relative.f, rtol=1e-12)
        c, lags, peaks, used = arc.subbands(1)
        assert c.size == lags.size == used.size > 0
        assert arc.table.loc[arc.spectrum_ids[0], "bkg_a"] == pytest.approx(
            char.table.bkg_a.iloc[0])

    def test_coarse_background_follows_the_raw_beam(self, archive):
        _, _, arc = archive
        wc = arc.get("wn_coarse")
        band = (wc > 2302) & (wc < 2358)
        np.testing.assert_allclose(arc.coarse("raw", 0)[band], arc.coarse("background", 0)[band],
                                   atol=1.0)

    def test_figures(self, archive, tmp_path):
        import pandas as pd
        from plumeIR.plotting.preprocess import save_preprocess_figures
        _, char, arc = archive
        targets = pd.DataFrame({"SO2": np.linspace(1e18, 5e18, 5)}, index=arc.spectrum_ids)
        written = save_preprocess_figures(arc, tmp_path / "plots", targets, "SO2")
        names = {p.name for p in written}
        assert {"preprocess_spectra.png", "preprocess_registration.png",
                "preprocess_background.png", "preprocess_relative.png",
                "preprocess_series.png", "preprocess_scatter.png"} == names
        assert all(p.stat().st_size > 0 for p in written)

    def test_missing_or_unknown(self, tmp_path):
        from plumeIR.exceptions import RunFolderError
        from plumeIR.preprocess.archive import read_preprocess_archive
        with pytest.raises(RunFolderError, match="no preprocess"):
            read_preprocess_archive(tmp_path)
        np.savez(tmp_path / "preprocess.npz", format=np.asarray(99))
        (tmp_path / PREP_NAME).write_text("spectrum_id\n")
        with pytest.raises(RunFolderError, match="layout version 99"):
            read_preprocess_archive(tmp_path)


class TestConfigKeys:

    def _build(self, **pre):
        from plumeIR.config.builder import build_preprocess
        from plumeIR.config.objects import GeometryType
        return build_preprocess(pre, GeometryType.LAYER)

    def test_defaults_report(self):
        p = self._build()
        assert (p.background, p.registration, p.reference_spectra) == ("report", "report", 10)

    def test_values_read(self):
        p = self._build(background="APPLY", registration="off")
        assert (p.background, p.registration) == ("apply", "off")

    @pytest.mark.parametrize("pre, match", [
        ({"background": "maybe"}, "background"),
        ({"registration": "smooth"}, "registration"),
        ({"reference_spectra": 0}, "reference_spectra"),
    ])
    def test_invalid_values_rejected(self, pre, match):
        from plumeIR.config.validator import _check_section
        errors: list[str] = []
        _check_section(errors, pre, "PREPROCESS", "PREPROCESS", "layer")
        assert any(match in e for e in errors)
