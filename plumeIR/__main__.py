"""
The plumeIR command line: ``python -m plumeIR CONFIG`` or ``plumeIR CONFIG``.

    plumeIR CONFIG [--parallel] [--workers N] [--output-dir DIR] [--recompute]
                   [--plot] [--save-plots] [--redraw-interval SECONDS] [--inspect]
                   [--analyse]
    plumeIR RUN_FOLDER [--analyse] [--inspect]
    plumeIR analyse RUN_FOLDER [RUN_FOLDER ...] [options]   (plumeIR.analysis.cli)

Runs the whole retrieval for one config and writes one folder per run
(``plumeIR.output``): the config as run, the long and wide results tables,
every fit (``fits.npz``), a summary (with the final gas ratios of ``*PLOT``)
and the log.

Inspection (M3c, ``plumeIR.plotting.inspector``): ``--inspect`` on a run
folder reopens it — fits, time series and ratios — and steps through its
spectra without refitting (← / →, PgUp / PgDn, Home / End, or click a point).
With a config, the run comes first and its folder is inspected at the end.
A ``--plot`` run also turns its window into the inspector when it finishes.

Composition analysis (``plumeIR.analysis``): ``--analyse`` writes the
analysis of the run to ``RUN_FOLDER/analysis/`` once its results are
written, with the defaults of ``plumeIR analyse`` (2-minute windows, ratios
against SO2, every target).  It reads the run from its folder, as
``plumeIR analyse RUN_FOLDER`` does afterwards, so the two give the same
files.  On a run folder it analyses the folder.  With ``--inspect`` the
analysis comes first.  A run stopped by Ctrl-C is not analysed, and a
failed analysis is logged and never costs the run's results.

Plotting (``plumeIR.plotting``): ``--plot`` opens a window showing the fits,
target time series and gas ratios, as v2's analysis canvas, and stays open at
the end until closed.  ``--save-plots`` writes a snapshot of the canvas at each
redraw (``plots/frames/``) and the final figures (``plots/``) to the run folder.
Every result enters the run state as it arrives, but the canvas is redrawn at
most once per ``--redraw-interval`` seconds (default 1) and once at the end:
drawing costs ~0.2 s per redraw and ~0.3 s per frame on the layer canvas,
against ~0.05 s per spectrum for 8 parallel workers, so redrawing every
spectrum made the display, not the fitting, set the pace of a run.  ``0`` redraws after every spectrum, as v2.  ``--save-plots`` without ``--plot``
is the headless mode: figures are written, no window is opened and no GUI
backend is loaded, so it runs on a server without a display.  Only this
process draws, so both work with ``--parallel``.  It is the first client of the library's run interface
(``plumeIR.pipeline``): ``prepare_run`` → ``stream_run``, read item by item →
``collect_run``.  It attaches the log handlers itself; the library attaches
none.

Ctrl-C stops the run: fits already running finish, the worker pool shuts down,
and what finished is written out as usual.

Exit status: 0 on success, 1 on a configuration, data or reference error or
a failed analysis, 130 when stopped by Ctrl-C.
"""

from __future__ import annotations

import argparse
import logging
import sys
import time
from datetime import datetime
from pathlib import Path

from plumeIR import __version__
from plumeIR.config.builder import build_from_raw
from plumeIR.config.parser import read_config
from plumeIR.config.paths import resolve_config_paths
from plumeIR.exceptions import PlumeIRError
from plumeIR.output import (
    FRAMES_DIR, LOG_NAME, PLOTS_DIR, RunInfo, make_run_dir, run_summary,
    preprocess_summary, write_fits, write_preprocess,
    write_resolved_config, write_results, write_summary,
)
from plumeIR.path_tracer import tracer_report
from plumeIR.pipeline import collect_run, prepare_run, stream_run
from plumeIR.plotting.state import RunState
from plumeIR.provenance import package_versions
from plumeIR.rfm_setup import require_rfm

# Not __name__: under ``python -m`` that is "__main__", outside the 'plumeIR'
# logger tree the handlers are attached to.
logger = logging.getLogger("plumeIR.cli")

#: Loggers whose records reach the console and the run log.  The companion
#: packages log RFM and Mie progress and failures, which belong in the record
#: of the run.
_LOGGERS = ("plumeIR", "rfmwrapper", "mieEODG")

_CONSOLE_FORMAT = "%(asctime)s %(levelname)-7s %(message)s"
_FILE_FORMAT    = "%(asctime)s %(levelname)-7s %(name)s: %(message)s"

DEFAULT_OUTPUT_DIR = Path("plumeIR_runs")
#: First arguments that start the composition analysis instead of a run.
ANALYSE_COMMANDS = ("analyse", "analyze")


def build_parser() -> argparse.ArgumentParser:
    """
    The command-line parser.

    Returns
    -------
    argparse.ArgumentParser
    """
    p = argparse.ArgumentParser(
        prog="plumeIR",
        description="Retrieve plume gas and aerosol columns from OP-FTIR "
                    "spectra, as set out in a plumeIR config file.",
    )
    p.add_argument("config", type=Path, metavar="CONFIG",
                   help="the run's .ini config file, or with --inspect or "
                        "--analyse a finished run's folder")
    p.add_argument("--parallel", action="store_true",
                   help="fit spectra in a process pool (not with update_params)")
    p.add_argument("--workers", type=int, metavar="N",
                   help="pool size for --parallel (default: one per CPU)")
    p.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR,
                   metavar="DIR",
                   help="parent of the run folder (default: ./%(default)s)")
    p.add_argument("--recompute", action="store_true",
                   help="rebuild the references instead of loading them from cache")
    p.add_argument("--plot", action="store_true",
                   help="show the fits, time series and ratios live in a window")
    p.add_argument("--save-plots", action="store_true",
                   help="write canvas snapshots and final figures to the run "
                        "folder; without --plot no window is opened (headless)")
    p.add_argument("--redraw-interval", type=float, default=1.0, metavar="SECONDS",
                   help="redraw the canvas (and save a frame) at most this often; "
                        "every result still counts. 0 redraws after every "
                        "spectrum (default: %(default)s)")
    p.add_argument("--inspect", action="store_true",
                   help="step through a run's fits, time series and ratios in "
                        "a window: a run folder is opened as is, a config is "
                        "run first")
    p.add_argument("--analyse", "--analyze", action="store_true", dest="analyse",
                   help="write the composition analysis (ratios, molar fractions) "
                        "to RUN_FOLDER/analysis/ when the run is done, with the "
                        "defaults of 'plumeIR analyse'; a run folder is analysed "
                        "as is")
    p.add_argument("--preprocess-only", action="store_true",
                   help="prepare the run (spectra, references, whole-spectrum "
                        "background and registration) and write preprocess.csv "
                        "and the summary, without fitting: to look at the "
                        "preprocessing before choosing *PREPROCESS modes")
    p.add_argument("--version", action="version", version=f"plumeIR {__version__}")
    return p


def _attach_handler(handler: logging.Handler, level: int, fmt: str) -> dict[str, int]:
    """
    Attach one handler to every logger in :data:`_LOGGERS`.

    The loggers are opened to DEBUG so the handler's own level decides what
    passes.  Returns the levels they had, for :func:`_detach_handler`.
    """
    handler.setLevel(level)
    handler.setFormatter(logging.Formatter(fmt, datefmt="%H:%M:%S"))
    previous = {}
    for name in _LOGGERS:
        lg = logging.getLogger(name)
        previous[name] = lg.level
        lg.setLevel(logging.DEBUG)
        lg.addHandler(handler)
    return previous


def _detach_handler(handler: logging.Handler, previous: dict[str, int]) -> None:
    """Remove and close a handler, and restore the logger levels it changed."""
    for name in _LOGGERS:
        lg = logging.getLogger(name)
        lg.removeHandler(handler)
        lg.setLevel(previous[name])
    handler.close()


def _progress(item) -> str:
    """One line per fitted spectrum: index, time stamp, R² and status per window."""
    parts = [f"{fr.window_label} R2={fr.r2:.4f}{'' if fr.success else ' FAILED'}"
             for fr in item.fits]
    sid = item.fits[0].spectrum_id if item.fits else ""
    return f"[{item.index + 1}/{item.n_spectra}] {sid}  " + "  ".join(parts)


class _Plots:
    """
    The command line's plotting: a live window, saved figures, both or neither.

    A drawing failure is logged and turns plotting off; it never stops the run
    or loses its results.
    """

    def __init__(self, args: argparse.Namespace, setup, state: RunState,
                 run_dir: Path) -> None:
        self.state, self.save = state, args.save_plots
        self.interval = args.redraw_interval
        self._last_draw = float("-inf")     # monotonic time the last redraw ended
        self.n_frames = 0
        self._drawn_count = -1               # n_received at the last redraw
        self.plots_dir = run_dir / PLOTS_DIR
        self.label = setup.retrieval.geometry_type.name.lower()
        self.canvas = None
        self.titles: dict = {}
        if not (args.plot or args.save_plots):
            return
        # Imported here: a parallel run's workers re-import this module and
        # must not load the plotting stack.
        from plumeIR.plotting.canvas import AnalysisCanvas, window_titles
        self.titles = window_titles(setup.windows)
        self.canvas = AnalysisCanvas(state, self.titles, label=self.label,
                                     interactive=args.plot)
        if self.save:
            (self.plots_dir / FRAMES_DIR).mkdir(parents=True, exist_ok=True)

    def _failed(self, exc: Exception) -> None:
        logger.error("Plotting failed and is switched off for this run "
                     "(the retrieval continues): %s: %s", type(exc).__name__, exc)
        logger.debug("Plotting failure", exc_info=True)
        self.canvas = None

    def update(self, force: bool = False) -> None:
        """
        Redraw, and save a frame with --save-plots, if the interval has passed.

        The interval runs from the end of the previous redraw, so drawing
        takes at most about draw / (draw + interval) of the main process.
        """
        if self.canvas is None:
            return
        if not force and time.monotonic() - self._last_draw < self.interval:
            return
        try:
            self.canvas.update(self.state)
            self._drawn_count = self.state.n_received
            if self.save:
                from plumeIR.plotting.canvas import FRAME_DPI
                frame = (self.plots_dir / FRAMES_DIR
                         / f"canvas_{self.state.n_received:04d}.png")
                self.canvas.save(frame, dpi=FRAME_DPI)
                self.n_frames += 1
        except Exception as exc:     # a figure must never cost the results
            self._failed(exc)
        self._last_draw = time.monotonic()

    def finish(self) -> None:
        """Draw the last state (and its frame); write the final figures with --save-plots."""
        if self.state.n_received == 0:
            return
        if self.canvas is not None and self._drawn_count != self.state.n_received:
            self.update(force=True)
        if not self.save:
            return
        try:
            from plumeIR.plotting.canvas import save_final_figures
            written = save_final_figures(self.state, self.plots_dir,
                                         self.titles, self.label)
            logger.info("Figures written to %s (%d final, %d frames for %d spectra)",
                        self.plots_dir, len(written), self.n_frames,
                        self.state.n_received)
        except Exception as exc:
            self._failed(exc)

    def close_window(self) -> bool:
        """
        Close the --plot window, if one is open, before the inspector opens.

        Returns
        -------
        bool
            Whether a window was open (the user had not closed it).
        """
        if self.canvas is None or not self.canvas.interactive or self.canvas.closed:
            return False
        self.canvas.close()
        return True


def _save_preprocess_plots(run_dir: Path, table, windows: dict | None = None) -> None:
    """The whole-spectrum preprocessing figures, in ``plots/`` (never fatal)."""
    from plumeIR.preprocess.archive import ARCHIVE_NAME, read_preprocess_archive
    if not (run_dir / ARCHIVE_NAME).is_file():
        return
    try:
        from plumeIR.plotting.preprocess import save_preprocess_figures
        targets = table.wide() if table is not None and len(table.df) else None
        primary = next((w.targets[0] for w in (windows or {}).values() if w.targets), None)
        written = save_preprocess_figures(read_preprocess_archive(run_dir),
                                          run_dir / PLOTS_DIR, targets, primary)
        logger.info("Preprocessing figures written to %s (%d)", run_dir / PLOTS_DIR,
                    len(written))
    except Exception as exc:     # a figure must never cost the results
        logger.error("Preprocessing figures not written: %s: %s", type(exc).__name__, exc)


def inspect(run_dir: Path) -> int:
    """
    Open a finished run's folder in the inspector and wait until it is closed.

    Parameters
    ----------
    run_dir : Path
        A run folder holding ``fits.npz``.

    Returns
    -------
    int
        Exit status: 0, or 1 when the folder cannot be reopened.
    """
    from plumeIR.plotting.inspector import Inspector
    try:
        inspector = Inspector.from_run_dir(run_dir, interactive=True)
    except PlumeIRError as exc:
        logger.error("%s: %s", type(exc).__name__, exc)
        return 1
    logger.info("Inspecting %s (%d spectra): left/right to step, close the "
                "window to exit", run_dir, inspector.n_spectra)
    try:
        inspector.hold()
    except KeyboardInterrupt:
        pass
    finally:
        inspector.close()
    return 0


def analyse(run_dir: Path, plots: bool = True) -> int:
    """
    Write a run folder's composition analysis to ``RUN_FOLDER/analysis/``.

    Parameters
    ----------
    run_dir : Path
        A finished run folder.
    plots : bool
        Write the figures as well as the tables.

    Returns
    -------
    int
        Exit status: 0, or 1 when the analysis fails.  A failure is logged;
        the run's own files are never touched.
    """
    from plumeIR.analysis.cli import ANALYSIS_DIR, run_analysis
    from plumeIR.analysis.report import SUMMARY_NAME
    out_dir = run_dir / ANALYSIS_DIR
    try:
        run_analysis([run_dir], out_dir, plots=plots)
    except PlumeIRError as exc:
        logger.error("Composition analysis failed: %s: %s", type(exc).__name__, exc)
        return 1
    except Exception as exc:     # an analysis must never cost the run's results
        logger.error("Composition analysis failed: %s: %s", type(exc).__name__, exc)
        logger.debug("Analysis failure", exc_info=True)
        return 1
    for line in (out_dir / SUMMARY_NAME).read_text(encoding="utf-8").rstrip().splitlines():
        logger.info(line)
    return 0


def run(args: argparse.Namespace) -> int:
    """
    Execute one run from parsed arguments.

    Parameters
    ----------
    args : argparse.Namespace
        From :func:`build_parser`.

    Returns
    -------
    int
        Exit status.
    """
    config_path = args.config.expanduser().resolve()
    if not config_path.is_file():
        logger.error("Config file not found: %s", config_path)
        return 1

    # Validate before creating anything on disk, so a typo leaves no folder.
    raw = resolve_config_paths(read_config(config_path), config_path.parent)
    cfg = build_from_raw(raw, base_dir=config_path.parent)
    if args.parallel and cfg["retrieval"].update_params:
        logger.error("update_params (warm start from the previous spectrum) is "
                     "sequential: run without --parallel, or set "
                     "update_params: False")
        return 1
    # RFM is not distributed with plumeIR: fail here, with the steps to get
    # it, rather than inside the reference build (and before the run folder).
    require_rfm()

    started = datetime.now()
    run_dir = make_run_dir(args.output_dir, config_path.stem, started)
    file_handler = logging.FileHandler(run_dir / LOG_NAME, encoding="utf-8")
    file_levels = _attach_handler(file_handler, logging.DEBUG, _FILE_FORMAT)
    try:
        logger.info("plumeIR %s, config %s", __version__, config_path)
        logger.info("Packages: %s", ", ".join(
            f"{name} {desc}" for name, desc in package_versions().items()))
        logger.info("Run folder %s", run_dir)
        write_resolved_config(raw, run_dir)

        setup = prepare_run(cfg, recompute=args.recompute)
        if args.preprocess_only:
            write_preprocess(setup, run_dir)
            if args.save_plots:
                _save_preprocess_plots(run_dir, None)
            summary = preprocess_summary(setup, RunInfo(
                config_path=config_path, started=started, finished=datetime.now(),
                parallel=False, n_workers=None, stopped=False))
            write_summary(summary, run_dir)
            for line in summary.rstrip().splitlines():
                logger.info(line)
            logger.info("Preprocessing written to %s (no fits: --preprocess-only)", run_dir)
            return 0
        state = RunState.for_run(setup)
        plots = _Plots(args, setup, state, run_dir)

        items, stopped = [], False
        stream = stream_run(setup, parallel=args.parallel, n_workers=args.workers)
        try:
            for item in stream:
                items.append(item)
                state.add(item)
                logger.info(_progress(item))
                plots.update()
        except KeyboardInterrupt:
            stopped = True
            logger.warning("Interrupted: stopping the run")
        finally:
            stream.close()

        table = collect_run(setup, items)
        tracer = tracer_report(table, setup.windows, setup.params, setup.geometry)
        write_results(table, run_dir, tracer)
        write_preprocess(setup, run_dir)
        if args.save_plots:
            _save_preprocess_plots(run_dir, table, setup.windows)
        try:
            write_fits(items, run_dir)
        except OSError as exc:      # the tables are written; only inspection is lost
            logger.error("Could not write the fits for inspection: %s", exc)
        info = RunInfo(config_path=config_path, started=started,
                       finished=datetime.now(), parallel=args.parallel,
                       n_workers=args.workers,
                       stopped=stopped or len(items) < setup.n_spectra)
        summary = run_summary(setup, table, info, state.final_ratios(), tracer)
        write_summary(summary, run_dir)
        for line in summary.rstrip().splitlines():
            logger.info(line)
        plots.finish()
        logger.info("Results written to %s", run_dir)
        if stopped:
            if args.analyse:
                logger.info("Run stopped: not analysed (plumeIR analyse %s "
                            "analyses what finished)", run_dir)
            return 130
        status = analyse(run_dir) if args.analyse else 0
        # A --plot window becomes the inspector once the run is done, unless
        # the user closed it during the run.
        if plots.close_window() or args.inspect:
            return inspect(run_dir) or status
        return status
    except PlumeIRError as exc:
        logger.error("%s: %s", type(exc).__name__, exc)
        return 1
    finally:
        _detach_handler(file_handler, file_levels)


def main(argv: list[str] | None = None) -> int:
    """
    Entry point of ``python -m plumeIR`` and the ``plumeIR`` console script.

    Parameters
    ----------
    argv : list of str or None
        Arguments without the program name; ``sys.argv[1:]`` if ``None``.

    Returns
    -------
    int
        Exit status.
    """
    argv = sys.argv[1:] if argv is None else list(argv)
    # ``plumeIR analyse ...``: the composition analysis of finished runs.  A
    # config file named "analyse" in the working directory keeps its meaning.
    if argv and argv[0] in ANALYSE_COMMANDS and not Path(argv[0]).exists():
        from plumeIR.analysis.cli import main as analyse_main
        return analyse_main(argv[1:])
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.workers is not None and not args.parallel:
        parser.error("--workers requires --parallel")
    if args.workers is not None and args.workers < 1:
        parser.error("--workers must be at least 1")
    if args.redraw_interval < 0:
        parser.error("--redraw-interval must be 0 or more seconds")
    if args.preprocess_only and (args.plot or args.inspect or args.analyse):
        parser.error("--preprocess-only fits nothing: it does not combine with "
                     "--plot, --inspect or --analyse (--save-plots writes its "
                     "figures)")
    is_folder = args.config.expanduser().is_dir()
    if is_folder and not (args.inspect or args.analyse):
        parser.error(f"{args.config} is a folder: add --inspect to open a "
                     f"finished run or --analyse to analyse it, or give a "
                     f"config file to run")
    if is_folder:
        run_flags = [flag for flag, on in (
            ("--parallel", args.parallel), ("--plot", args.plot),
            ("--save-plots", args.save_plots), ("--recompute", args.recompute),
            ("--preprocess-only", args.preprocess_only))
            if on]
        if run_flags:
            parser.error(f"{', '.join(run_flags)} apply to a run, not to "
                         f"a finished run folder")

    console = logging.StreamHandler(sys.stderr)
    levels = _attach_handler(console, logging.INFO, _CONSOLE_FORMAT)
    try:
        if is_folder:
            run_dir = args.config.expanduser().resolve()
            status = analyse(run_dir) if args.analyse else 0
            if args.inspect:
                return inspect(run_dir) or status
            return status
        return run(args)
    except PlumeIRError as exc:
        # Config errors, raised before the run folder exists.
        logger.error("%s: %s", type(exc).__name__, exc)
        return 1
    finally:
        _detach_handler(console, levels)


if __name__ == "__main__":
    sys.exit(main())
