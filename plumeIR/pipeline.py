"""
The end-to-end plumeIR pipeline, as a stream of results.

    config → build objects → build a Reference per window (RFM/mieEODG)
           → load spectra → preprocess → fit each spectrum (serial/parallel ×
             NLLS/OE) → ResultsTable

Clients (the command line, a GUI) use it in three steps:

    setup = prepare_run("run.ini")            # everything before the first fit
    items = []
    for item in stream_run(setup, parallel=True):
        items.append(item)                    # one SpectrumFits per spectrum
        ...                                   # draw, update progress, maybe break
    table = collect_run(setup, items)         # what finished, as a table

A client stops a run by no longer reading the stream and closing it; the worker
pool is shut down (``plumeIR.retrieval.run.iter_retrieval``).
:func:`run_from_config` is the three steps for a client that wants only the
final table.

The reference build is the only RFM/mieEODG step and runs once per window
(cached and content-addressed); fitting is pure NumPy and parallelises across
spectra.  The heavy steps are imported as module-level names so they can be
substituted in tests.
"""

from __future__ import annotations

import logging
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from pathlib import Path

from plumeIR.config.builder import build_from_config
from plumeIR.config.objects import PlotConfig, SolarGeometry
from plumeIR.exceptions import PlumeIRError
from plumeIR.preprocess import preprocess_spectra
from plumeIR.reference import build_reference_for
from plumeIR.retrieval import ResultsTable
from plumeIR.retrieval.run import SpectrumFits, collect_results, iter_retrieval
from plumeIR.spectra import explain_no_spectra, load_spectra
from plumeIR.spectra.spectrum import resolve_instrument_ils

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class RunSetup:
    """
    Everything a run needs before its first fit.

    Attributes
    ----------
    data, model, geometry, retrieval
        The typed config objects (see ``plumeIR.config.objects``).
        ``retrieval`` carries ``max_opd`` / ``apod_type`` as resolved from the
        data files.
    windows : dict
        ``FitWindowConfig`` per active window, keyed by label, in config order.
    params : dict
        ``ParameterSet`` per window label.
    references : dict
        ``Reference`` per window label.
    spectra : list of Spectrum
        Loaded and preprocessed spectra, in chronological order.
    plot : PlotConfig
        Ratio and display settings (``*PLOT``), read by the run state.
    """
    data:       object
    model:      object
    geometry:   object
    retrieval:  object
    windows:    dict
    params:     dict
    references: dict
    spectra:    list
    plot:       PlotConfig = PlotConfig()

    @property
    def n_spectra(self) -> int:
        """Number of spectra the run will fit."""
        return len(self.spectra)


def warn_experimental(geometry, retrieval, windows=()) -> None:
    """
    Log a WARNING for each experimental option a run uses.  (v2 options v3
    never implemented, such as seed_param, were removed from the config on
    2026-11-12; the validator warns about them as unknown keys.)

    Supported in v3.0: layer and emission geometries with NLLS.  Solar
    occultation and optimal estimation run but are not validated on real data.

    Parameters
    ----------
    geometry
        The run's geometry object.
    retrieval
        The run's ``Retrieval``.
    windows : iterable of FitWindowConfig
        The run's windows, for options set per window.
    """
    if isinstance(geometry, SolarGeometry):
        logger.warning(
            "Solar geometry is experimental: the forward model and reference "
            "are built, but there is no solar preprocessing and it has not "
            "been run on real solar data (P6-3, DEFER-1)")
    if retrieval.retrieval_method == "oe":
        logger.warning(
            "Optimal estimation is experimental: it has not been validated on "
            "real data, and prior widths default to a quarter of each "
            "parameter's bounds unless sigma_prior is set (P11-7)")
    for window in windows:
        if window.dual_gases:
            logger.warning(
                "%s: %s fitted as ambient and plume parts (%s) is experimental: "
                "both parts are recovered from synthetic spectra, but on the "
                "only real dataset tested the ambient CO2 stays 1.5-1.6x the "
                "expected column", window.label,
                list(window.dual_gases),
                ", ".join(f"{window.atm_name(g)} + {g}" for g in window.dual_gases))


def prepare_run(
    config: str | Path | dict,
    *,
    recompute: bool = False,
    cache_dir: str | Path | None = None,
) -> RunSetup:
    """
    Load spectra and build references: every step before fitting.

    The spectra come first, so a data folder or file filter that matches
    nothing fails at once, with :func:`~plumeIR.spectra.explain_no_spectra`
    naming the key to change, rather than after the reference build.

    Parameters
    ----------
    config : str, Path or dict
        A config file, or the dict already built from one by
        :func:`~plumeIR.config.builder.build_from_config` or
        :func:`~plumeIR.config.builder.build_from_raw`.
    recompute : bool
        Ignore cached references and rebuild them.
    cache_dir : str, Path or None
        Reference cache directory (overrides the model/default location).

    Returns
    -------
    RunSetup

    Raises
    ------
    PlumeIRError
        If no spectra are found (or a subclass, from the config or reference
        steps).
    """
    cfg = build_from_config(config) if isinstance(config, (str, Path)) else config
    data, model, geometry, retrieval = (
        cfg["data"], cfg["model"], cfg["geometry"], cfg["retrieval"]
    )
    warn_experimental(geometry, retrieval, cfg["windows"])

    # Spectra first: a wrong data_dir or file filter fails in seconds, not
    # after minutes of reference building.
    spectra = load_spectra(data)
    if not spectra:
        raise PlumeIRError(explain_no_spectra(data))
    spectra = preprocess_spectra(spectra, geometry, data)
    # max_opd / apod_type from the data files, once for the run, so the setup
    # carries the values the fits will use.  The references are built from the
    # config's retrieval settings, as before, so their cache keys are unchanged.
    resolved = resolve_instrument_ils(retrieval, spectra)

    references: dict = {}
    params:     dict = {}
    windows:    dict = {}
    for window, pset in zip(cfg["windows"], cfg["params"]):
        logger.info("Building reference for window '%s'", window.label)
        references[window.label] = build_reference_for(
            geometry, window, retrieval, model, pset,
            cache_dir=cache_dir, recompute=recompute,
        )
        params[window.label] = pset
        windows[window.label] = window

    return RunSetup(data=data, model=model, geometry=geometry,
                    retrieval=resolved, windows=windows, params=params,
                    references=references, spectra=list(spectra),
                    plot=cfg.get("plot", PlotConfig()))


def stream_run(
    setup: RunSetup,
    *,
    parallel:  bool = False,
    n_workers: int | None = None,
    sigma=None,
) -> Iterator[SpectrumFits]:
    """
    Fit a prepared run, yielding each spectrum's results as they finish.

    Parameters
    ----------
    setup : RunSetup
        From :func:`prepare_run`.
    parallel : bool
        Run fits in a spawn process pool across spectra.  Items then arrive in
        completion order; ``SpectrumFits.index`` gives the input order.
    n_workers : int or None
        Worker count for the pool.
    sigma : np.ndarray, float or None
        Measurement 1σ passed to each fit (used by OE; weights NLLS).

    Returns
    -------
    Iterator[SpectrumFits]
        Close it (or stop iterating and drop it) to stop the run.

    Raises
    ------
    FitError
        If the config sets ``update_params`` (warm start from the previous
        spectrum, which is sequential) and ``parallel`` is True.
    """
    return iter_retrieval(
        setup.spectra, setup.references, setup.params, setup.windows,
        setup.geometry, setup.retrieval,
        parallel=parallel, n_workers=n_workers, sigma=sigma,
        update_params=setup.retrieval.update_params,
    )


def collect_run(setup: RunSetup, items: Iterable[SpectrumFits]) -> ResultsTable:
    """
    Turn a run's stream, or the items read from it so far, into one table.

    Parameters
    ----------
    setup : RunSetup
        The run the items belong to.
    items : iterable of SpectrumFits
        A stream from :func:`stream_run` (consumed), or a list of items kept
        while reading one — a stopped run gives a table of what finished.

    Returns
    -------
    ResultsTable
        Rows in input order.
    """
    return collect_results(items, setup.windows, setup.params, setup.geometry)


def run_from_config(
    path: str | Path,
    *,
    parallel:  bool = False,
    n_workers: int | None = None,
    recompute: bool = False,
    cache_dir: str | Path | None = None,
    sigma=None,
    save_csv:  str | Path | None = None,
) -> ResultsTable:
    """
    Run a full retrieval from a config file and return the collected table.

    Parameters
    ----------
    path : str or Path
        Path to the ``.ini`` config file.
    parallel : bool
        Run fits in a spawn process pool across spectra.
    n_workers : int or None
        Worker count for the pool.
    recompute : bool
        Ignore cached references and rebuild them.
    cache_dir : str, Path or None
        Reference cache directory (overrides the model/default location).
    sigma : np.ndarray, float or None
        Measurement 1σ passed to each fit (used by OE; weights NLLS).
    save_csv : str, Path or None
        If given, write the results table to this CSV path.

    Returns
    -------
    ResultsTable
        One table for the whole run.
    """
    setup = prepare_run(path, recompute=recompute, cache_dir=cache_dir)
    results = collect_run(
        setup, stream_run(setup, parallel=parallel, n_workers=n_workers, sigma=sigma)
    )
    if save_csv is not None:
        results.to_csv(save_csv)
        logger.info("Results written to '%s'", save_csv)
    return results
