"""
The run output folder: one directory per run, written by any client.

    <output_dir>/<config stem>_<YYYYmmdd-HHMMSS>/
        config.ini         the config as run, every path resolved to absolute
        results_long.csv   ResultsTable: one row per spectrum × window × parameter
        results_wide.csv   target columns, one row per spectrum, one column per
                           species (gases molec/cm², aerosols g/m²)
        summary.txt        package versions (plumeIR, rfmwrapper, mieEODG, with
                           git commits where known), spectra loaded and
                           fitted, ILS used, convergence and R² per window,
                           final ratio regressions, and with
                           a *PATH window the path check, and the
                           whole-spectrum preprocessing's run-level results
        preprocess.csv  one row per spectrum: the background and
                           registration measured before the fits (when either
                           *PREPROCESS product is not off)
        preprocess.npz  the spectra as fitted, hulls, background, sub-band
                           lags: what the preprocessing plots draw
        fits.npz           every fit's measured, model and reference-continuum
                           spectra, per window (M3c), for ``--inspect``, and
                           its parameter covariance, for the error
                           correlation of species fitted in one window
        run.log            written by the client's log handler, not here
        plots/             with ``--save-plots``: final figures (canvas.png,
                           fit_<window>.png, timeseries.png, ratio_<y>_<x>.png)
        plots/frames/      canvas_NNNN.png, the canvas at each redraw (--redraw-interval)

The command line uses it; a GUI can call the same functions.  Nothing here
attaches a log handler (the client does) or draws anything.

A finished folder is reopened with :func:`load_run` (config, results table
and stored fits), which ``RunState.from_run_dir`` turns into the state the
inspector draws.
"""

from __future__ import annotations

import logging
import os
from collections.abc import Iterable
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import numpy as np

from plumeIR import __version__
from plumeIR.config.parser import read_config, write_config
from plumeIR.constants import LEGACY_PARAMETER_NAMES, POSSIBLE_AEROSOLS
from plumeIR.exceptions import ConfigError, RunFolderError
from plumeIR.path_tracer import PathReport
from plumeIR.provenance import package_versions
from plumeIR.retrieval.results import (
    COL_R2, COL_SPECTRUM, COL_SUCCESS, COL_WINDOW, ResultsTable, r_squared,
)

logger = logging.getLogger(__name__)

CONFIG_NAME  = "config.ini"
LONG_NAME    = "results_long.csv"
WIDE_NAME    = "results_wide.csv"
SUMMARY_NAME = "summary.txt"
LOG_NAME     = "run.log"
FITS_NAME    = "fits.npz"
PREP_NAME    = "preprocess.csv"            # = plumeIR.preprocess.archive.TABLE_NAME
PLOTS_DIR    = "plots"
FRAMES_DIR   = "frames"


@dataclass(frozen=True)
class RunInfo:
    """
    How a run was executed, for the summary.

    Attributes
    ----------
    config_path : Path or None
        The config file the run was started from (``None`` for a config built
        from data).
    started, finished : datetime
        Wall-clock start (before the reference build) and end.
    parallel : bool
        Whether fits ran in a process pool.
    n_workers : int or None
        Pool size, ``None`` for the executor default.
    stopped : bool
        The run was stopped before every spectrum was fitted.
    """
    config_path: Path | None
    started:     datetime
    finished:    datetime
    parallel:    bool
    n_workers:   int | None
    stopped:     bool = False


def make_run_dir(output_dir: str | Path, name: str,
                 when: datetime | None = None) -> Path:
    """
    Create a new, empty folder for one run.

    Parameters
    ----------
    output_dir : str or Path
        Parent directory; created if missing.
    name : str
        Stem of the folder name, normally the config file's stem.
    when : datetime or None
        Time stamp for the name; now if ``None``.

    Returns
    -------
    Path
        ``<output_dir>/<name>_<YYYYmmdd-HHMMSS>``, with ``_2``, ``_3``, ...
        appended if a run in the same second already took the name.  Never an
        existing folder, so one run cannot overwrite another.
    """
    when = when or datetime.now()
    parent = Path(output_dir).expanduser()
    parent.mkdir(parents=True, exist_ok=True)
    base = f"{name}_{when:%Y%m%d-%H%M%S}"
    candidate, n = parent / base, 1
    while True:
        try:
            candidate.mkdir()
            return candidate
        except FileExistsError:
            n += 1
            candidate = parent / f"{base}_{n}"


def write_resolved_config(raw: dict, run_dir: Path) -> Path:
    """
    Write the config as run into the run folder.

    Parameters
    ----------
    raw : dict
        The raw config with paths already resolved
        (:func:`~plumeIR.config.paths.resolve_config_paths`), so the copy can be
        rerun from where it sits.
    run_dir : Path
        The run folder.

    Returns
    -------
    Path
        The written file.
    """
    path = run_dir / CONFIG_NAME
    write_config(raw, path)
    return path


def write_results(table: ResultsTable, run_dir: Path,
                  tracer: PathReport | None = None) -> tuple[Path, Path]:
    """
    Write the long and wide results tables.

    Parameters
    ----------
    table : ResultsTable
        The run's results (possibly of a stopped run).
    run_dir : Path
        The run folder.
    tracer : PathReport or None
        The path diagnostic (the ``*PATH`` window); its per-spectrum
        implied path is added to the wide table as ``tracer_path_km``.

    Returns
    -------
    tuple of Path
        ``(long_csv, wide_csv)``.  The wide table holds each species' column
        from the window where it is a declared target (``ResultsTable.wide``),
        and the molecular content of each aerosol (acid and water of a
        solution, ``ResultsTable.aerosol_components``); errors, fixed
        parameters and non-target windows are in the long table.
    """
    long_csv, wide_csv = run_dir / LONG_NAME, run_dir / WIDE_NAME
    table.to_csv(long_csv)
    wide = table.wide()
    components = table.aerosol_components()
    if not components.empty:      # acid and water content of the aerosols
        wide = wide.join(components, how="left")
    if tracer is not None and tracer.best is not None:
        wide = wide.join(tracer.per_spectrum(), how="left")
    wide.to_csv(wide_csv)
    return long_csv, wide_csv


def write_preprocess(setup, run_dir: Path) -> list[Path]:
    """
    Write the whole-spectrum preprocessing: ``preprocess.csv`` and ``.npz``.

    Parameters
    ----------
    setup : RunSetup
        Its ``preprocess_report``, ``spectra`` and ``windows`` are written.
    run_dir : Path

    Returns
    -------
    list of Path
        The files written; empty when both products were off.
    """
    from plumeIR.preprocess.archive import write_preprocess_archive
    report = getattr(setup, "preprocess_report", None)
    if report is None or (report.background == "off" and report.registration == "off"):
        return []
    path = run_dir / PREP_NAME
    report.table.to_csv(path, index=False)
    written = [path]
    archive = write_preprocess_archive(
        report, setup.spectra, setup.windows,
        setup.retrieval.geometry_type.name.lower(), run_dir)
    if archive is not None:
        written.append(archive)
    return written


def preprocess_summary(setup, info: RunInfo) -> str:
    """
    The summary of a ``--preprocess-only`` run: versions, spectra, the ILS and
    the whole-spectrum preprocessing, no fits.

    Parameters
    ----------
    setup : RunSetup
    info : RunInfo
    """
    retrieval = setup.retrieval
    lines = [
        f"plumeIR {__version__} preprocessing summary (no fits)",
        "",
        *(f"{name:<16}{desc}" for name, desc in package_versions().items()),
        "",
        f"config          {info.config_path if info.config_path else '(built from data)'}",
        f"started         {info.started:%Y-%m-%d %H:%M:%S}",
        f"geometry        {retrieval.geometry_type.name.lower()}",
        f"data            {setup.data.data_dir}",
        f"spectra loaded  {setup.n_spectra}",
        f"ILS             max_opd {retrieval.max_opd:.5f} cm, "
        f"apod {retrieval.apod_type!r}, fov a priori {retrieval.fov:g} rad",
        "",
    ]
    report = getattr(setup, "preprocess_report", None)
    if report is not None:
        lines += report.summary_lines()
    return "\n".join(lines) + "\n"


def run_summary(setup, table: ResultsTable, info: RunInfo,
                ratios: dict | None = None,
                tracer: PathReport | None = None) -> str:
    """
    A short plain-text account of a run.

    Parameters
    ----------
    setup : RunSetup
        From :func:`plumeIR.pipeline.prepare_run`.
    table : ResultsTable
        The run's results.
    info : RunInfo
        How it was run.
    ratios : dict or None
        Final ratio regressions, ``(y, x)`` → ``RatioFit`` or ``None``
        (``RunState.final_ratios``), over every fitted spectrum.
    tracer : PathReport or None
        The path diagnostic (:func:`plumeIR.path_tracer.tracer_report`):
        the implied path per window, ambient VMRs and ratio intercepts at
        both paths.

    Returns
    -------
    str
        Spectra loaded and fitted, the instrument line shape used, per window
        the convergence count and R² (median and 5th percentile), and the
        final gas ratios.
    """
    df = table.df
    retrieval = setup.retrieval
    n_fitted = df[COL_SPECTRUM].nunique() if len(df) else 0
    seconds = (info.finished - info.started).total_seconds()
    mode = "parallel" if info.parallel else "serial"
    if info.parallel:
        mode += f", {info.n_workers} workers" if info.n_workers else ", default workers"

    lines = [
        f"plumeIR {__version__} run summary",
        "",
        *(f"{name:<16}{desc}" for name, desc in package_versions().items()),
        "",
        f"config          {info.config_path if info.config_path else '(built from data)'}",
        f"started         {info.started:%Y-%m-%d %H:%M:%S}",
        f"finished        {info.finished:%Y-%m-%d %H:%M:%S}  ({seconds:.0f} s)",
        f"geometry        {retrieval.geometry_type.name.lower()}",
        f"method          {retrieval.retrieval_method}, {mode}"
        + (", warm start (update_params)" if retrieval.update_params else ""),
        f"data            {setup.data.data_dir}",
        f"spectra loaded  {setup.n_spectra}",
        f"spectra fitted  {n_fitted}"
        + ("  (stopped before the end)" if info.stopped else ""),
        f"ILS             max_opd {retrieval.max_opd:.5f} cm, "
        f"apod {retrieval.apod_type!r}, fov a priori {retrieval.fov:g} rad"
        + (" (fitted)" if retrieval.fit_fov else " (fixed)"),
        "",
    ]
    rows = []
    for label, window in setup.windows.items():
        per_fit = (df[df[COL_WINDOW] == label]
                   .drop_duplicates(COL_SPECTRUM)[[COL_SUCCESS, COL_R2]])
        n = len(per_fit)
        ok = int(per_fit[COL_SUCCESS].sum())
        r2 = per_fit[COL_R2].to_numpy(dtype=float)
        r2 = r2[np.isfinite(r2)]
        rows.append([
            label,
            f"{window.wn_start:g}-{window.wn_stop:g}",
            f"{ok}/{n} ({100.0 * ok / n:.1f}%)" if n else "0/0",
            f"{np.median(r2):.5f}" if r2.size else "-",
            f"{np.percentile(r2, 5):.5f}" if r2.size else "-",
        ])
    lines += _table(["window", "range [cm-1]", "converged", "R2 median", "R2 p5"],
                    rows)
    if ratios:
        rows = []
        for (y, x), fit in ratios.items():
            name = f"{y}/{x}"
            if fit is None:
                rows.append([name, "", "", "(fewer than 3 points)", "", "", ""])
                continue
            rows.append([name, fit.method, str(fit.n), f"{fit.slope:.6g}",
                         f"{fit.slope_err:.3g}", f"{fit.intercept:.4g}",
                         f"{fit.mswd:.3g}"])
        lines += [""] + _table(["ratio", "method", "n", "slope", "1 sigma",
                                "intercept", "mswd"], rows)
    if tracer is not None:
        lines += [""] + _tracer_lines(tracer, ratios)
    char = getattr(setup, "preprocess_report", None)
    if char is not None and (char.background != "off" or char.registration != "off"):
        lines += [""] + char.summary_lines()
    lines += [
        "",
        f"{LONG_NAME:<18}every parameter of every fit, with errors",
        f"{WIDE_NAME:<18}target species per spectrum (gases molec/cm2, aerosols g/m2)",
        f"{FITS_NAME:<18}every fit's spectra, for plumeIR RUN_FOLDER --inspect",
    ]
    if char is not None and (char.background != "off" or char.registration != "off"):
        lines.append(f"{PREP_NAME:<18}background and registration per spectrum, "
                     f"measured before the fits")
    return "\n".join(lines) + "\n"


def _tracer_lines(tracer: PathReport, ratios: dict | None) -> list[str]:
    """The path-diagnostic block of the summary."""
    lines = [f"path check      {tracer.tracer} at {tracer.background_ppmv:.4g} ppmv; "
             f"configured pathlength {tracer.configured_km:.4g} km "
             f"(diagnostic only: no fit uses it)"]
    if tracer.best is None:
        where = (f"the {tracer.path_window} window has" if tracer.designated
                 else "no window has")
        return lines + [f"                {where} {tracer.tracer} fitted in "
                        "enough spectra"]
    rows = []
    for label, w in tracer.windows.items():
        talk = (f"{w.crosstalk_gas} {w.crosstalk_r:+.2f}"
                if w.crosstalk_gas else "-")
        rows.append([label + (" *" if label == tracer.best else ""),
                     str(len(w.path_km)), f"{w.median:.3f}",
                     f"{w.quantile(0.05):.3f}-{w.quantile(0.95):.3f}",
                     f"{100 * w.rel_error:.1f}%", talk])
    lines += _table(["window", "n", "path [km]", "p5-p95", "fit error",
                     "r plume gas"], rows)
    why = "the path window" if tracer.designated else "smallest fit error"
    lines += [f"implied path    {tracer.path_km:.3f} km ({tracer.best}, {why}), "
              f"{tracer.ratio:.2f}× the configured path"]
    rows = [[a.window, a.gas, f"{a.configured_ppmv:.4g}", f"{a.implied_ppmv:.4g}"]
            for a in tracer.ambient]
    lines += [""] + _table(["window", "ambient gas", "VMR at pathlength",
                            "VMR at implied path"], rows)
    if ratios:
        rows = []
        for (y, x), fit in ratios.items():
            # A gas's intercept is a column along the path; an aerosol's is a
            # mass, with no mixing ratio.
            if (fit is None or not np.isfinite(fit.intercept)
                    or y in POSSIBLE_AEROSOLS):
                continue
            at_cfg, at_imp = tracer.background_vmr(fit.intercept)
            rows.append([f"{y}/{x}", f"{fit.intercept:.4g}", f"{at_cfg:.4g}",
                         f"{at_imp:.4g}"])
        if rows:
            lines += [""] + _table(["ratio", "intercept [molec/cm2]",
                                    "as VMR at pathlength [ppmv]",
                                    "at implied path [ppmv]"], rows)
    return lines


def write_summary(text: str, run_dir: Path) -> Path:
    """
    Write the run summary.

    Parameters
    ----------
    text : str
        From :func:`run_summary`.
    run_dir : Path
        The run folder.

    Returns
    -------
    Path
        The written file.
    """
    path = run_dir / SUMMARY_NAME
    path.write_text(text, encoding="utf-8")
    return path


# =============================================================================
# Stored fits (fits.npz)
# =============================================================================

#: Layout version of ``fits.npz``; bumped when its keys change meaning.
#: 2 (2026-11-18): adds each fit's parameter covariance (``W.varied``,
#: ``W.covariance``).  Version 1 files are still read, without it.
FITS_FORMAT = 2

#: Layout versions :func:`read_fits` reads.
FITS_FORMATS_READ = (1, 2)


@dataclass(frozen=True)
class StoredFit:
    """
    One window's fit of one spectrum, as stored in ``fits.npz``.

    Carries what a fit panel draws, nothing a refit would need: the arrays of
    :class:`~plumeIR.retrieval.results.FitResult` on the measurement grid, and
    its status.  The fitted parameters are in ``results_long.csv``.

    Attributes
    ----------
    window_label, spectrum_id : str
    success : bool
    wn, measured, model : np.ndarray
        Shape ``(n_wn,)``, exactly as fitted (float64).
    continuum : np.ndarray or None
        ``FitResult.continuum``: background polynomial (layer, solar) or
        clear-sky model (emission); ``None`` where the forward model gives none.
    """
    window_label: str
    spectrum_id:  str
    success:      bool
    wn:           np.ndarray
    measured:     np.ndarray
    model:        np.ndarray
    continuum:    np.ndarray | None = None

    @property
    def residual(self) -> np.ndarray:
        """Measured − model, shape ``(n_wn,)``."""
        return self.measured - self.model

    @property
    def r2(self) -> float:
        """Coefficient of determination, as ``FitResult.r2``."""
        return r_squared(self.measured, self.model)


def _pad(rows: list[np.ndarray | None], width: int) -> np.ndarray:
    """Stack rows of unequal length into ``(n, width)``, NaN-padded."""
    out = np.full((len(rows), width), np.nan)
    for i, r in enumerate(rows):
        if r is not None:
            out[i, :len(r)] = r
    return out


def _covariances(fits: list) -> tuple[list[str], np.ndarray]:
    """
    The varied parameters of a window's fits and their covariances.

    Parameters
    ----------
    fits : list of FitResult or None
        One window's fit of each spectrum (``None`` where it has none).

    Returns
    -------
    varied : list of str
        Every varied name, in the order of the first fit that has it.
    covariance : np.ndarray
        Shape ``(n, k, k)``; NaN where a fit has no covariance or did not
        vary a parameter.
    """
    varied: list[str] = []
    for fr in fits:
        for name in (() if fr is None else fr.varied_names):
            if name not in varied:
                varied.append(name)
    pos = {name: i for i, name in enumerate(varied)}
    out = np.full((len(fits), len(varied), len(varied)), np.nan)
    for i, fr in enumerate(fits):
        if fr is None or fr.covariance is None:
            continue
        c = np.asarray(fr.covariance, dtype=float)
        if c.shape != (len(fr.varied_names),) * 2:
            continue
        j = np.asarray([pos[name] for name in fr.varied_names], dtype=int)
        out[i][np.ix_(j, j)] = c
    return varied, out


def write_fits(items: Iterable, run_dir: Path) -> Path:
    """
    Store every fit of a run in ``fits.npz``, for inspection after the run.

    Parameters
    ----------
    items : iterable of SpectrumFits
        The run's stream items, in any order (a parallel run's completion
        order); stored in input order.  A stopped run stores what finished.
    run_dir : Path
        The run folder.

    Returns
    -------
    Path
        The written file.

    Notes
    -----
    Keys, for ``n`` spectra and each window label ``W`` (float64, NaN-padded
    where a spectrum's grid is shorter than the longest)::

        format          int, :data:`FITS_FORMAT`
        windows         (n_windows,) str, in config order
        spectrum_index  (n,) int, position in the run's input order
        spectrum_id     (n,) str
        spec_type       (n,) str, ``SpectrumType`` name ('' if unknown)
        W.wn            (n_wn,), or (n, n_wn) when grids differ
        W.n_wn          (n,) int, points of each spectrum's grid
        W.success       (n,) bool
        W.measured, W.model    (n, n_wn)
        W.continuum     (n, n_wn), absent when no fit had one; NaN rows
                        where a fit had none
        W.varied        (k,) str, the varied parameters of the window, in
                        the optimizer order of its first fit (format 2)
        W.covariance    (n, k, k), each fit's parameter covariance
                        (``FitResult.covariance``) in that order; NaN where a
                        fit has none or did not vary that parameter
                        (format 2)

    Written to a temporary name and renamed, so a folder never holds half a
    file.
    """
    items = sorted(items, key=lambda it: it.index)
    windows: list[str] = []
    for it in items:
        for fr in it.fits:
            if fr.window_label not in windows:
                windows.append(fr.window_label)

    arrays: dict[str, np.ndarray] = {
        "format": np.asarray(FITS_FORMAT),
        "windows": np.asarray(windows, dtype=str),
        "spectrum_index": np.asarray([it.index for it in items], dtype=int),
        "spectrum_id": np.asarray(
            [str(it.fits[0].spectrum_id) if it.fits else str(it.index)
             for it in items], dtype=str),
        "spec_type": np.asarray(
            [getattr(getattr(it.spectrum, "spec_type", None), "name", "")
             for it in items], dtype=str),
    }
    for w in windows:
        fits = [next((fr for fr in it.fits if fr.window_label == w), None)
                for it in items]
        n_wn = np.asarray([0 if fr is None else len(fr.wn) for fr in fits], dtype=int)
        width = int(n_wn.max()) if len(n_wn) else 0
        grids = [fr.wn for fr in fits if fr is not None]
        same = all(len(g) == len(grids[0]) and np.array_equal(g, grids[0])
                   for g in grids)
        arrays[f"{w}.wn"] = (np.asarray(grids[0], dtype=float) if grids and same
                             else _pad([None if fr is None else fr.wn
                                        for fr in fits], width))
        arrays[f"{w}.n_wn"] = n_wn
        arrays[f"{w}.success"] = np.asarray(
            [fr is not None and bool(fr.success) for fr in fits], dtype=bool)
        arrays[f"{w}.measured"] = _pad([None if fr is None else fr.measured
                                        for fr in fits], width)
        arrays[f"{w}.model"] = _pad([None if fr is None else fr.model
                                     for fr in fits], width)
        conts = [None if fr is None else fr.continuum for fr in fits]
        if any(c is not None for c in conts):
            arrays[f"{w}.continuum"] = _pad(conts, width)
        varied, cov = _covariances(fits)
        arrays[f"{w}.varied"] = np.asarray(varied, dtype=str)
        arrays[f"{w}.covariance"] = cov

    path = run_dir / FITS_NAME
    tmp = run_dir / (FITS_NAME + ".part")
    with open(tmp, "wb") as fh:
        np.savez_compressed(fh, **arrays)
    os.replace(tmp, path)
    return path


class FitArchive:
    """
    The fits of a finished run, read from ``fits.npz``.

    Spectra are indexed ``0 … n-1`` in input order, the order of
    ``results_long.csv``.  Arrays are loaded once; :meth:`fits` slices them.

    Parameters
    ----------
    arrays : dict[str, np.ndarray]
        The file's contents (:func:`read_fits`).
    """

    def __init__(self, arrays: dict[str, np.ndarray]) -> None:
        self._a = arrays
        self.windows: list[str] = [str(w) for w in arrays["windows"]]
        self.spectrum_index: np.ndarray = arrays["spectrum_index"]
        self.spectrum_ids: list[str] = [str(s) for s in arrays["spectrum_id"]]
        self.spec_types: list[str] = [str(s) for s in arrays["spec_type"]]

    def __len__(self) -> int:
        return len(self.spectrum_ids)

    def fit(self, k: int, window: str) -> StoredFit:
        """Window ``window``'s fit of the ``k``-th spectrum (input order)."""
        a = self._a
        n = int(a[f"{window}.n_wn"][k])
        wn = a[f"{window}.wn"]
        wn = wn[:n] if wn.ndim == 1 else wn[k, :n]
        cont = a.get(f"{window}.continuum")
        cont = None if cont is None else cont[k, :n]
        if cont is not None and not np.any(np.isfinite(cont)):
            cont = None
        return StoredFit(window_label=window, spectrum_id=self.spectrum_ids[k],
                         success=bool(a[f"{window}.success"][k]), wn=wn,
                         measured=a[f"{window}.measured"][k, :n],
                         model=a[f"{window}.model"][k, :n], continuum=cont)

    def fits(self, k: int) -> dict[str, StoredFit]:
        """Every window's fit of the ``k``-th spectrum, by label, in window order."""
        return {w: self.fit(k, w) for w in self.windows}

    @property
    def has_covariance(self) -> bool:
        """Whether the file holds the fits' covariances (format 2 onwards)."""
        return all(f"{w}.covariance" in self._a for w in self.windows)

    def correlation(self, window: str, a: str, b: str) -> np.ndarray:
        """
        Correlation of the fit errors of parameters ``a`` and ``b`` in
        ``window``, per spectrum (input order).

        Parameters
        ----------
        window : str
        a, b : str
            Varied parameter names.

        Returns
        -------
        np.ndarray
            Shape ``(n,)``; NaN where a fit has no covariance, did not vary
            either parameter, or a variance is not positive.

        Raises
        ------
        KeyError
            For a window without stored covariances (a format 1 file), or a
            parameter the window never varied.
        """
        key = f"{window}.covariance"
        if key not in self._a:
            raise KeyError(f"{window}: no covariances stored (fits.npz format 1)")
        # A run folder written before 2026-11-19 stores former parameter names.
        names = [LEGACY_PARAMETER_NAMES.get(str(v), str(v))
                 for v in self._a[f"{window}.varied"]]
        for name in (a, b):
            if name not in names:
                raise KeyError(f"{window}: {name!r} is not a varied parameter "
                               f"(varied: {names})")
        c = self._a[key]
        i, j = names.index(a), names.index(b)
        with np.errstate(invalid="ignore", divide="ignore"):
            var_a, var_b = c[:, i, i], c[:, j, j]
            ok = (var_a > 0) & (var_b > 0)
            return np.where(ok, c[:, i, j] / np.sqrt(np.where(ok, var_a * var_b, 1.0)), np.nan)


def read_fits(run_dir: str | Path) -> FitArchive:
    """
    Read a run folder's ``fits.npz``.

    Parameters
    ----------
    run_dir : str or Path

    Returns
    -------
    FitArchive

    Raises
    ------
    RunFolderError
        If the file is missing (a run written before fits were stored) or of
        an unknown layout.
    """
    path = Path(run_dir).expanduser() / FITS_NAME
    if not path.is_file():
        raise RunFolderError(
            f"{path.parent} has no {FITS_NAME}: it was written before runs "
            f"stored their fits (plumeIR M3c, 2026-11-12), so its fits cannot "
            f"be shown. Its tables are complete; rerun its {CONFIG_NAME} to "
            f"inspect the fits")
    with np.load(path, allow_pickle=False) as npz:
        arrays = {k: npz[k] for k in npz.files}
    version = int(arrays.get("format", -1))
    if version not in FITS_FORMATS_READ:
        raise RunFolderError(f"{path}: layout version {version}, this plumeIR "
                             f"reads versions {FITS_FORMATS_READ}")
    return FitArchive(arrays)


@dataclass(frozen=True)
class StoredRun:
    """
    A finished run, reopened from its folder.

    Attributes
    ----------
    run_dir : Path
    config : dict
        The typed config objects (``build_from_raw``) of the run's
        ``config.ini``: ``'geometry'``, ``'retrieval'``, ``'windows'``,
        ``'plot'``, ...
    table : ResultsTable
        ``results_long.csv``.
    fits : FitArchive or None
        ``fits.npz``; ``None`` only when loaded with ``require_fits=False``
        from a run written before fits were stored (2026-11-12).
    """
    run_dir: Path
    config:  dict
    table:   ResultsTable
    fits:    FitArchive | None


def load_run(run_dir: str | Path, *, require_fits: bool = True) -> StoredRun:
    """
    Reopen a finished run's folder: its config, results table and fits.

    Nothing is refitted and no reference is built.  The config is rebuilt
    from ``config.ini`` (written with every path resolved), so the files it
    names (e.g. an emission ``atm_path``) must still exist.

    Parameters
    ----------
    run_dir : str or Path
    require_fits : bool
        ``False`` accepts a folder without ``fits.npz`` (a run written
        before fits were stored), with ``fits`` ``None``: enough for the
        composition analysis, which reads the tables, not for inspection.

    Returns
    -------
    StoredRun

    Raises
    ------
    RunFolderError
        If the folder or one of its files is missing, its config no longer
        validates, or its table and fits hold different spectra.
    """
    from plumeIR.config.builder import build_from_raw

    run_dir = Path(run_dir).expanduser().resolve()
    if not run_dir.is_dir():
        raise RunFolderError(f"Run folder not found: {run_dir}")
    for name in (CONFIG_NAME, LONG_NAME):
        if not (run_dir / name).is_file():
            raise RunFolderError(f"{run_dir} is not a complete plumeIR run "
                                 f"folder: {name} is missing")
    fits = read_fits(run_dir) if require_fits or (run_dir / FITS_NAME).is_file() \
        else None
    try:
        config = build_from_raw(read_config(run_dir / CONFIG_NAME),
                                base_dir=run_dir)
    except ConfigError as exc:
        raise RunFolderError(f"{run_dir / CONFIG_NAME} no longer builds "
                             f"(a file it names may have moved): {exc}") from exc
    table = ResultsTable.from_csv(run_dir / LONG_NAME)

    ids = [str(s) for s in table.df[COL_SPECTRUM].drop_duplicates()]
    if fits is not None and ids != fits.spectrum_ids:
        raise RunFolderError(
            f"{run_dir}: {LONG_NAME} ({len(ids)} spectra) and {FITS_NAME} "
            f"({len(fits)} spectra) do not hold the same spectra in the same "
            f"order")
    return StoredRun(run_dir=run_dir, config=config, table=table, fits=fits)


def _table(header: list[str], rows: list[list[str]], gap: int = 2) -> list[str]:
    """
    Plain-text table: first column left-aligned, the rest right-aligned.

    Column widths come from the widest cell, so no value runs into its
    neighbour (a four-digit spectrum count once did, in ``converged``).

    Parameters
    ----------
    header : list of str
        Column titles.
    rows : list of list of str
        Cell text, one list per row, as long as ``header``.
    gap : int
        Spaces between columns.

    Returns
    -------
    list of str
        The header line followed by one line per row.
    """
    widths = [max(len(cell) for cell in column) for column in zip(header, *rows)]

    def line(cells: list[str]) -> str:
        first = f"{cells[0]:<{widths[0]}}"
        rest = "".join(" " * gap + f"{c:>{w}}" for c, w in zip(cells[1:], widths[1:]))
        return (first + rest).rstrip()

    return [line(header)] + [line(row) for row in rows]
