"""
Tests for the run orchestrator (serial / parallel × NLLS / OE).

Synthetic, no RFM: a banded SO2 GasLUT drives a LayerForwardModel (which, with
unit source/response and no atmospheric gases, reduces to T_plume·poly).  Each
spectrum's measurement is generated from the forward at a known truth, so the
run must recover it; serial and parallel must agree.

Run with:  pytest test/test_retrieval_run.py -v
(OE-via-orchestrator case needs pyOptimalEstimation — the project env.)
"""

from __future__ import annotations

from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from plumeIR.forward.factory import build_forward_model
from plumeIR.spectra.spectrum import Spectrum, SpectrumType
from plumeIR.config.objects import (
    FitWindowConfig, GeometryType, LayerGeometry, Retrieval,
)
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import Reference
from plumeIR.retrieval import run_retrieval

_MODEL_WN = np.arange(2390.0, 2560.0, 0.04)    # model grid (0.04 cm⁻¹, real ILS)
_MEAS_WN  = np.linspace(2410.0, 2540.0, 300)   # measurement grid (inside window)


def _so2_lut() -> GasLUT:
    band = 0.8 * np.exp(-((_MODEL_WN - 2475.0) / 15.0) ** 2)
    return GasLUT(species="SO2", wn=_MODEL_WN,
                  temp_nodes=np.linspace(300.0, 1000.0, 5),
                  od_table=np.tile(band, (5, 1)), ref_scd=1e17)


def _params(method: str = "nlls") -> ParameterSet:
    so2_prior = 5.0 if method == "oe" else None
    poly_prior = 2.0 if method == "oe" else None
    return ParameterSet([
        Parameter("poly0", 1.0, True, (-5.0, 5.0), ParameterKind.POLYNOMIAL,
                  sigma_prior=poly_prior),
        Parameter("SO2", 0.5, True, (0.0, np.inf), ParameterKind.GAS_PLUME,
                  ref_column=1e17, sigma_prior=so2_prior),
        Parameter("fov", 0.02, False, (0.001, 0.2), ParameterKind.SPECTRAL),
    ])


def _geom() -> LayerGeometry:
    return LayerGeometry(pathlength=1.0, atm_temp=290.0, atm_pres=1000.0,
                         plume_temp=450.0, plume_pres=1000.0, plume_thickness=0.05)


def _retrieval(method: str = "nlls") -> Retrieval:
    return Retrieval(geometry_type=GeometryType.LAYER, retrieval_method=method,
                     fit_fov=False, fit_shift=False)


def _make_spectrum(truth: np.ndarray, ts: datetime, method: str = "nlls") -> Spectrum:
    """A spectrum whose y over the window = forward(truth)."""
    ref = Reference(wn=_MODEL_WN, gas_luts={"SO2": _so2_lut()},
                    geometry_type=GeometryType.LAYER)
    fm = build_forward_model(ref, _params(method), _geom(), _retrieval(method),
                             _window(), _MEAS_WN)
    y = fm(truth)
    return Spectrum(x=_MEAS_WN.copy(), y=y, x_unit="wn", timestamp=ts,
                    filename=Path(f"spec_{ts:%H%M%S}.spc"), spec_type=SpectrumType.SBM)


def _window(label: str = "FIT0") -> FitWindowConfig:
    return FitWindowConfig(label=label, wn_start=2400.0, wn_stop=2550.0,
                           atm_gases=(), plume_gases=("SO2",), plume_aero=(),
                           targets=("SO2",), polydeg=0)


def _setup(method: str = "nlls"):
    truths = {"08:00": np.array([1.2, 2.0]), "08:01": np.array([1.1, 3.5])}
    spectra = [_make_spectrum(t, datetime(2021, 8, 11, *map(int, k.split(":")), 0), method)
               for k, t in truths.items()]
    refs   = {"FIT0": Reference(wn=_MODEL_WN, gas_luts={"SO2": _so2_lut()},
                                geometry_type=GeometryType.LAYER)}
    params = {"FIT0": _params(method)}
    windows = {"FIT0": _window()}
    return spectra, refs, params, windows, _geom(), _retrieval(method), truths


def test_run_serial_nlls_recovers_truth():
    spectra, refs, params, windows, geom, retr, truths = _setup()
    rt = run_retrieval(spectra, refs, params, windows, geom, retr, parallel=False)
    so2 = rt.wide(quantity="value")["SO2"]      # fitted scaling per spectrum
    assert so2.iloc[0] == pytest.approx(2.0, abs=1e-3)
    assert so2.iloc[1] == pytest.approx(3.5, abs=1e-3)
    # SCD column = scaling × ref_column
    assert rt.wide(quantity="scd")["SO2"].iloc[1] == pytest.approx(3.5e17, rel=1e-3)


def test_run_serial_matches_parallel():
    spectra, refs, params, windows, geom, retr, _ = _setup()
    serial   = run_retrieval(spectra, refs, params, windows, geom, retr, parallel=False)
    parallel = run_retrieval(spectra, refs, params, windows, geom, retr,
                             parallel=True, n_workers=2)
    s = serial.wide(quantity="value")["SO2"].sort_index()
    p = parallel.wide(quantity="value")["SO2"].sort_index()
    assert np.allclose(s.values, p.values, atol=1e-6)


def test_run_oe_serial():
    pytest.importorskip("pyOptimalEstimation")
    spectra, refs, params, windows, geom, retr, _ = _setup(method="oe")
    rt = run_retrieval(spectra, refs, params, windows, geom, retr,
                       parallel=False, sigma=1e-3)
    so2 = rt.wide(quantity="value")["SO2"]
    assert so2.iloc[0] == pytest.approx(2.0, abs=0.05)
    # method recorded as OE
    assert (rt.to_dataframe()["window"] == "FIT0").all()
    assert len(rt.to_dataframe()[rt.to_dataframe()["parameter"] == "SO2"]) == 2


# =============================================================================
# The run as a stream (iter_retrieval)
# =============================================================================

def _many_spectra(n: int):
    """n synthetic spectra with distinct, known SO2 scalings (1.0, 1.5, ...)."""
    truths = [np.array([1.0 + 0.05 * k, 1.0 + 0.5 * k]) for k in range(n)]
    spectra = [_make_spectrum(t, datetime(2021, 8, 11, 8, k, 0)) for k, t in enumerate(truths)]
    return spectra, truths


def test_stream_serial_yields_in_order():
    from plumeIR.retrieval import SpectrumFits, iter_retrieval
    spectra, refs, params, windows, geom, retr, _ = _setup()
    items = list(iter_retrieval(spectra, refs, params, windows, geom, retr))
    assert [it.index for it in items] == [0, 1]
    assert all(isinstance(it, SpectrumFits) and it.n_spectra == 2 for it in items)
    assert items[1].spectrum is spectra[1]
    assert [fr.window_label for fr in items[0].fits] == ["FIT0"]
    assert items[1].fits[0].values["SO2"] == pytest.approx(3.5, abs=1e-3)


def test_stream_runs_nothing_until_read(monkeypatch):
    """Creating the stream must not fit anything: a client may never read it."""
    import plumeIR.retrieval.run as run
    from plumeIR.retrieval import iter_retrieval
    calls = []
    monkeypatch.setattr(run, "_fit_spectrum", lambda *a, **k: calls.append(1) or [])
    spectra, refs, params, windows, geom, retr, _ = _setup()
    gen = iter_retrieval(spectra, refs, params, windows, geom, retr)
    assert calls == []
    gen.close()
    assert calls == []


def test_stream_rejects_bad_arguments_eagerly():
    """Argument errors surface when the stream is created, not at first read."""
    from plumeIR.exceptions import FitError
    from plumeIR.retrieval import iter_retrieval
    spectra, refs, params, windows, geom, retr, _ = _setup()
    with pytest.raises(FitError, match="update_params"):
        iter_retrieval(spectra, refs, params, windows, geom, retr,
                       parallel=True, update_params=True)


def test_stream_parallel_stop_mid_run_keeps_partial_results():
    """
    A client that stops reading a parallel run gets the results it read, and
    the worker pool is gone once the stream is closed.
    """
    import multiprocessing as mp
    from plumeIR.retrieval import collect_results, iter_retrieval

    spectra, truths = _many_spectra(8)
    _, refs, params, windows, geom, retr, _ = _setup()

    stream = iter_retrieval(spectra, refs, params, windows, geom, retr,
                            parallel=True, n_workers=2)
    kept = []
    for item in stream:
        kept.append(item)
        if len(kept) == 2:
            break                                  # the STOP button
    assert mp.active_children(), "pool should still be up before close()"
    stream.close()
    assert mp.active_children() == []              # pool shut down, workers joined

    assert len(kept) == 2 and all(it.n_spectra == 8 for it in kept)
    for it in kept:                                # each partial result is a real fit
        assert it.fits[0].values["SO2"] == pytest.approx(truths[it.index][1], abs=1e-3)

    table = collect_results(kept, windows, params, geom)
    so2 = table.wide(quantity="value")["SO2"]
    assert len(so2) == 2
    assert sorted(so2.values) == pytest.approx(
        sorted(truths[it.index][1] for it in kept), abs=1e-3)


def test_collect_results_orders_by_input_index():
    """Parallel items arrive in completion order; the table is in input order."""
    from plumeIR.retrieval import collect_results, iter_retrieval
    spectra, refs, params, windows, geom, retr, _ = _setup()
    items = list(iter_retrieval(spectra, refs, params, windows, geom, retr))
    table = collect_results(reversed(items), windows, params, geom)
    ids = list(dict.fromkeys(table.to_dataframe()["spectrum_id"]))   # row order
    assert ids == [s.timestamp.isoformat() for s in spectra]
