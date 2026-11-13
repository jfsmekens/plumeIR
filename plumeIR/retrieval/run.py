"""
iter_retrieval / run_retrieval — fits over spectra × windows (serial or parallel).

Backend-agnostic (NLLS or OE, chosen from ``Retrieval.retrieval_method``) and
concurrency is selectable: ``parallel=False`` runs in-process; ``parallel=True``
uses a spawn ``ProcessPoolExecutor`` that parallelises **across spectra** — each
worker fits all windows for one spectrum.

This parallelises cleanly because forward evaluation is pure NumPy at fit time
(RFM / mieEODG run only when the ``Reference`` is built, beforehand) and the
``Reference`` / ``ParameterSet`` / ``FitResult`` are immutable — no shared
mutable state.  Workers receive the references via a one-time initializer
(spawn has no shared memory, so each worker holds its own copy; for large LUTs
this bounds the worker count — a future optimisation is to load them from the
content-addressed cache per worker instead of pickling through initargs).

A run is a **stream**: :func:`iter_retrieval` is a generator yielding one
:class:`SpectrumFits` per spectrum as soon as its fits finish (in completion
order when parallel).  A client that stops reading — a STOP button, a ``break``
— closes the generator, which shuts the worker pool down.  :func:`run_retrieval`
is the stream collected into one ``ResultsTable`` (:func:`collect_results`).
"""

from __future__ import annotations

import dataclasses

import logging
import multiprocessing as mp
from collections.abc import Iterable, Iterator, Sequence
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass

import numpy as np

from plumeIR.exceptions import FitError
from plumeIR.forward.factory import build_forward_model
from plumeIR.config.objects import FitWindowConfig
from plumeIR.parameters.parameter import ParameterKind, ParameterSet
from plumeIR.retrieval.nlls import NonLinearLeastSquares
from plumeIR.retrieval.oe import OptimalEstimation
from plumeIR.retrieval.results import FitResult
from plumeIR.retrieval.results import ResultsTable

logger = logging.getLogger(__name__)


def _seed_polynomial(params, measured: np.ndarray):
    """
    Seed the constant background term ``poly0`` from the measured spectrum's
    level, returning a new ParameterSet.

    The background polynomial is unbounded and carries the continuum in the
    spectrum's native units, so it must start at the right magnitude — ~counts
    for a single beam, ~radiance for emission, ~1 for transmittance.  Seeding
    from the data is mode-agnostic (no normalisation, which would be invalid for
    radiance/transmittance).
    """
    poly = params.names_by_kind(ParameterKind.POLYNOMIAL)
    if not poly:
        return params
    return params.seeded({poly[0]: float(np.nanmedian(measured))})


def build_backend(method: str, max_nfev: int | None = 4000):
    """
    Instantiate a retrieval backend from ``retrieval_method``.

    ``max_nfev`` caps NLLS function evaluations — high enough that a strong
    plume's gas scaling can climb several orders of magnitude from the prior,
    low enough that a genuinely non-converging fit gives up rather than hangs.
    """
    m = method.lower()
    if m == "nlls":
        return NonLinearLeastSquares(max_nfev=max_nfev)
    if m == "oe":
        return OptimalEstimation()
    raise FitError(f"unknown retrieval_method {method!r} (expected 'nlls' or 'oe')")


def _spectrum_id(spectrum) -> str:
    """Stable identifier for a spectrum: timestamp, else filename stem."""
    ts = getattr(spectrum, "timestamp", None)
    if ts is not None:
        return ts.isoformat()
    fn = getattr(spectrum, "filename", None)
    return fn.stem if fn is not None else str(id(spectrum))


def _fit_spectrum(spectrum, *, references, params, windows, geometry,
                  retrieval, backend, sigma, seeds=None) -> list[FitResult]:
    """
    Fit every window for one spectrum; returns one FitResult per window.

    ``seeds`` (window label → values dict, for update_params warm-starting)
    pre-loads the varied parameters from a previous spectrum's fit; ``poly0`` is
    always re-seeded from *this* spectrum's continuum afterwards.
    """
    sid = _spectrum_id(spectrum)
    out: list[FitResult] = []
    done: dict[str, FitResult] = {}
    for label, window in windows.items():
        base = params[label]
        if seeds and label in seeds:
            warm = {n: seeds[label][n] for n in base.varied_names()
                    if n in seeds[label]}
            base = base.seeded(warm)
        base = _staged_temps(base, window, done, sid)
        trimmed = spectrum.trim(window.wn_start, window.wn_stop)
        pset = _seed_polynomial(base, trimmed.y)            # per-spectrum poly0
        forward = build_forward_model(
            references[label], pset, geometry, retrieval, window, trimmed.wn
        )
        fr = backend.fit(
            forward, pset, trimmed.y,
            sigma=sigma, window_label=label, spectrum_id=sid,
        )
        # The gas-free continuum at the fitted values, for display (v2's bkg).
        p_fit = np.array([fr.values[n] for n in fr.varied_names], dtype=float)
        continuum = forward.continuum(p_fit)
        if continuum is not None:
            fr = dataclasses.replace(fr, continuum=continuum)
        out.append(fr)
        done[label] = fr
    return out


def _staged_temps(base: ParameterSet, window: FitWindowConfig,
                  done: dict[str, FitResult], sid: str) -> ParameterSet:
    """
    Hold staged temperatures at the source windows' fits for this spectrum.

    ``window.plume_temp_from`` (``gas_temp``) and ``window.atm_temp_from``
    (``atm_temp``) each name an earlier window (the validator checks the
    order), whose result for the same spectrum is in ``done``.  A failed or
    missing source fit leaves the a priori in place, logged at DEBUG; the
    receiving window's results then record which temperature was used.

    Parameters
    ----------
    base : ParameterSet
        The receiving window's parameters (staged temperatures fixed).
    window : FitWindowConfig
        The receiving window.
    done : dict[str, FitResult]
        This spectrum's fits so far, by window label.
    sid : str
        Spectrum identifier, for the log.

    Returns
    -------
    ParameterSet
        ``base`` itself when nothing is staged, else a seeded copy.
    """
    staged = {}
    for name, src in (("gas_temp", window.plume_temp_from),
                      ("atm_temp", window.atm_temp_from)):
        if src is None:
            continue
        source = done.get(src)
        if source is None or not source.success or name not in source.values:
            logger.debug("%s %s: no %s from %s; holding the a priori %.1f K",
                         sid, window.label, name, src, base[name].value)
            continue
        lo, hi = base[name].bounds
        staged[name] = min(max(float(source.values[name]), lo), hi)
    return base.seeded(staged) if staged else base


# ---------------------------------------------------------------------------
# Parallel worker state — set once per worker process by the initializer.
# ---------------------------------------------------------------------------
_WORKER: dict = {}


def _init_worker(references, params, windows, geometry, retrieval, method,
                 sigma, max_nfev) -> None:
    _WORKER.clear()
    _WORKER.update(
        references=references, params=params, windows=windows,
        geometry=geometry, retrieval=retrieval,
        backend=build_backend(method, max_nfev), sigma=sigma,
    )


def _worker_fit(spectrum) -> list[FitResult]:
    return _fit_spectrum(spectrum, **_WORKER)


@dataclass(frozen=True)
class SpectrumFits:
    """
    One item of a run's stream: every window's fit for one spectrum.

    Attributes
    ----------
    index : int
        Position of the spectrum in the run's (chronological) input order.
        Parallel runs yield in completion order, so use this to re-order.
    n_spectra : int
        Number of spectra in the run, for progress display.
    spectrum : Spectrum
        The measured spectrum that was fitted (full range, not trimmed).
    fits : tuple of FitResult
        One result per window, in window order.
    """
    index:     int
    n_spectra: int
    spectrum:  object
    fits:      tuple[FitResult, ...]


def _iter_parallel(spectra: Sequence, n_workers: int | None,
                   initargs: tuple) -> Iterator[tuple[int, list[FitResult]]]:
    """
    Fit spectra in a spawn process pool, yielding ``(index, fits)`` as each
    spectrum finishes.

    The pool is created on the first ``next()`` and shut down in ``finally``,
    so it is released however the consumer leaves: exhausted, ``close()``-d,
    garbage-collected or by an exception.  On an early stop, queued spectra are
    cancelled and only those already handed to a worker run to completion.
    """
    pool = ProcessPoolExecutor(
        max_workers=n_workers, mp_context=mp.get_context("spawn"),
        initializer=_init_worker, initargs=initargs,
    )
    try:
        futures = {pool.submit(_worker_fit, s): i for i, s in enumerate(spectra)}
        for fut in as_completed(futures):
            yield futures[fut], fut.result()
    finally:
        pool.shutdown(wait=True, cancel_futures=True)


def iter_retrieval(
    spectra:    Sequence,
    references: dict,
    params:     dict,
    windows:    dict,
    geometry,
    retrieval,
    *,
    parallel:      bool = False,
    n_workers:     int | None = None,
    sigma=None,
    update_params: bool = False,
    max_nfev:      int = 4000,
) -> Iterator[SpectrumFits]:
    """
    Fit all spectra and windows, streaming each spectrum's results as they finish.

    Arguments are checked at once; no fit runs until the first ``next()``.
    Serial runs yield in input order;
    parallel runs in completion order (see ``SpectrumFits.index``).  To stop a
    run, stop reading and close the generator (``gen.close()``, or leave a
    ``for`` loop over it and drop it): the worker pool is shut down, queued
    spectra are cancelled, and fits already running in a worker finish first.

    Parameters
    ----------
    spectra : sequence of Spectrum
        Measured spectra to fit (chronological order matters for
        ``update_params``).
    references, params, windows : dict
        Keyed by window label: the ``Reference``, ``ParameterSet`` and
        ``FitWindowConfig`` for each window.
    geometry, retrieval
        The run geometry and retrieval config (the latter selects the backend
        via ``retrieval_method``).
    parallel : bool
        Run fits in a spawn process pool across spectra.
    n_workers : int or None
        Worker count for the pool (``None`` → executor default).
    sigma : np.ndarray, float or None
        Measurement 1σ passed to each fit (used by OE; weights NLLS).
    update_params : bool
        Warm-start each spectrum's fit from the previous spectrum's successful
        result (per window).  Sequential, so it requires ``parallel=False``.
    max_nfev : int
        NLLS function-evaluation cap (see :func:`build_backend`).

    Returns
    -------
    Iterator[SpectrumFits]
        A generator yielding one item per spectrum.

    Raises
    ------
    FitError
        If ``update_params`` is combined with ``parallel``, or the retrieval
        method is unknown.
    """
    # Checked here, eagerly, rather than inside the generator, where it would
    # only surface on the first next().
    if parallel and update_params:
        raise FitError("update_params requires serial execution (parallel=False)")
    if retrieval.retrieval_method.lower() not in ("nlls", "oe"):
        raise FitError(f"unknown retrieval_method {retrieval.retrieval_method!r} "
                       "(expected 'nlls' or 'oe')")
    if retrieval.retrieval_method.lower() == "oe" and sigma is None:
        logger.warning(
            "OE without a measurement noise: every point is given sigma = 1.0 "
            "in the spectrum's units, so the posterior errors are not "
            "calibrated. Pass sigma to read them as uncertainties")
    return _stream(spectra, references, params, windows, geometry, retrieval,
                   parallel=parallel, n_workers=n_workers, sigma=sigma,
                   update_params=update_params, max_nfev=max_nfev)


def _stream(spectra, references, params, windows, geometry, retrieval, *,
            parallel, n_workers, sigma, update_params,
            max_nfev) -> Iterator[SpectrumFits]:
    """The generator behind :func:`iter_retrieval` (arguments as there)."""
    # Resolve the ILS from the data files before anything is fitted.  A wrong
    # max_opd is silently absorbed by a fitted fov and biases retrieved
    # columns by tens of percent, so this must not be left to the config.
    from plumeIR.spectra.spectrum import resolve_instrument_ils
    retrieval = resolve_instrument_ils(retrieval, spectra)

    method = retrieval.retrieval_method
    n = len(spectra)
    done = 0
    mode = "parallel" if parallel else "serial"
    try:
        if parallel:
            source = _iter_parallel(
                spectra, n_workers,
                (references, params, windows, geometry, retrieval,
                 method, sigma, max_nfev),
            )
            try:
                for i, fits in source:
                    done += 1
                    yield SpectrumFits(i, n, spectra[i], tuple(fits))
            finally:
                source.close()          # shuts the pool down now, not at GC
        else:
            backend = build_backend(method, max_nfev)
            seeds: dict = {}                   # window label → last good fit values
            for i, spectrum in enumerate(spectra):
                fits = _fit_spectrum(
                    spectrum, references=references, params=params,
                    windows=windows, geometry=geometry, retrieval=retrieval,
                    backend=backend, sigma=sigma,
                    seeds=seeds if update_params else None,
                )
                if update_params:
                    for fr in fits:
                        if fr.success:
                            seeds[fr.window_label] = fr.values
                done += 1
                yield SpectrumFits(i, n, spectrum, tuple(fits))
    except GeneratorExit:
        logger.info("Retrieval stopped after %d of %d spectra (%s)", done, n, mode)
        raise

    logger.info(
        "Retrieval complete: %d spectra × %d windows = %d fits (%s, %s%s)",
        n, len(windows), n * len(windows), method, mode,
        ", warm-start" if update_params else "",
    )


def collect_results(
    stream:   Iterable[SpectrumFits],
    windows:  dict,
    params:   dict,
    geometry,
) -> ResultsTable:
    """
    Collect a run's stream, or the part of it already read, into one table.

    Parameters
    ----------
    stream : iterable of SpectrumFits
        A generator from :func:`iter_retrieval` (consumed here), or the items a
        client kept while reading it.  Rows are put in input order
        (``SpectrumFits.index``), whatever order they arrived in.
    windows, params : dict
        ``FitWindowConfig`` and ``ParameterSet`` keyed by window label.
    geometry
        The run geometry, which supplies the P/T for unit conversions.

    Returns
    -------
    ResultsTable
    """
    items = sorted(stream, key=lambda item: item.index)
    fits = [fr for item in items for fr in item.fits]
    geometries = {label: geometry for label in windows}
    return ResultsTable.from_fits(fits, windows, params, geometries=geometries)


def run_retrieval(
    spectra,
    references: dict,
    params:     dict,
    windows:    dict,
    geometry,
    retrieval,
    *,
    parallel:      bool = False,
    n_workers:     int | None = None,
    sigma=None,
    update_params: bool = False,
    max_nfev:      int = 4000,
) -> ResultsTable:
    """
    Run the retrieval over all spectra and windows and collect the results.

    :func:`iter_retrieval` read to the end and passed to
    :func:`collect_results`.

    Parameters
    ----------
    spectra : sequence of Spectrum
        Measured spectra to fit (chronological order matters for
        ``update_params``).
    references, params, windows : dict
        Keyed by window label: the ``Reference``, ``ParameterSet`` and
        ``FitWindowConfig`` for each window.
    geometry, retrieval
        The run geometry and retrieval config (the latter selects the backend
        via ``retrieval_method``).
    parallel : bool
        Run fits in a spawn process pool across spectra.
    n_workers : int or None
        Worker count for the pool (``None`` → executor default).
    sigma : np.ndarray, float or None
        Measurement 1σ passed to each fit (used by OE; weights NLLS).
    update_params : bool
        Warm-start each spectrum's fit from the previous spectrum's successful
        result (per window).  Sequential, so it requires ``parallel=False``.
    max_nfev : int
        NLLS function-evaluation cap (see :func:`build_backend`).

    Returns
    -------
    ResultsTable
        One table for the whole run.
    """
    stream = iter_retrieval(
        spectra, references, params, windows, geometry, retrieval,
        parallel=parallel, n_workers=n_workers, sigma=sigma,
        update_params=update_params, max_nfev=max_nfev,
    )
    return collect_results(stream, windows, params, geometry)
