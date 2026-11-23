"""
The composition analysis on the command line:

    plumeIR analyse RUN_FOLDER [RUN_FOLDER ...] [--output-dir DIR] [--width W]
                    [--spectra] [--reference GAS] [--gases GAS ...]
                    [--exclude SPECIES ...] [--min-r2 R2]
                    [--no-jackknife] [--independent-errors] [--no-plots]
    python -m plumeIR.analysis ...

Reads finished run folders (nothing is refitted) and writes, by default to
``RUN_FOLDER/analysis/`` for one run, or to ``--output-dir`` (required for
several): ``ratios.csv``, ``moving_composition.csv``, ``rank.csv``,
``analysis.txt`` (:mod:`plumeIR.analysis.report`) and the figures
(:mod:`plumeIR.plotting.analysis_report`).  Several runs are analysed each
on its own and compared (``composition_runs.png``, ``analysis_runs.png``).

A run can also be analysed as it finishes: ``plumeIR CONFIG --analyse``
(or ``plumeIR RUN_FOLDER --analyse``) calls :func:`run_analysis` with the
default settings, writing to ``RUN_FOLDER/analysis/``.

Exit status: 0 on success, 1 on an error (a folder that is not a finished
run, an unknown gas, ...).
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

from plumeIR import __version__
from plumeIR.analysis.report import AnalysisSettings, analyse_runs, write_analysis
from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger("plumeIR.cli.analyse")

#: The analysis folder inside a run folder (one run, no --output-dir).
ANALYSIS_DIR = "analysis"


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="plumeIR analyse",
        description="Gas composition of finished plumeIR runs: ratios, molar "
                    "fractions and their evolution (nothing is refitted).")
    p.add_argument("runs", type=Path, nargs="+", metavar="RUN_FOLDER",
                   help="finished run folders (several are analysed each on its own "
                        "and compared)")
    p.add_argument("--output-dir", type=Path, metavar="DIR",
                   help="where to write; default RUN_FOLDER/analysis for one run, "
                        "required for several")
    p.add_argument("--width", type=float, default=AnalysisSettings.width, metavar="W",
                   help="window width, minutes (default %(default)g): the pooled "
                        "ratio, the moving windows and the background offsets")
    p.add_argument("--spectra", action="store_true",
                   help="--width counts spectra, not minutes")
    p.add_argument("--reference", metavar="GAS",
                   help="gas the ratios are quoted against (default SO2, else the first)")
    p.add_argument("--gases", nargs="+", metavar="GAS",
                   help="gases to analyse (default every target gas of the run)")
    p.add_argument("--exclude", nargs="+", metavar="SPECIES", default=(),
                   help="species to leave out (e.g. WATER where it falls as the plume "
                        "rises); never inferred from the data")
    p.add_argument("--min-r2", type=float, metavar="R2",
                   help="leave out spectra whose target window fitted below this R²")
    p.add_argument("--no-jackknife", action="store_true",
                   help="skip the jackknife errors of the all-gas fits (faster)")
    p.add_argument("--independent-errors", action="store_true",
                   help="ignore the error correlation of species fitted in one window "
                        "(for comparison with analyses before 2026-11-18)")
    p.add_argument("--no-plots", action="store_true", help="write the tables only")
    p.add_argument("--version", action="version", version=f"plumeIR {__version__}")
    return p


def run_analysis(run_dirs: list[str | Path], out_dir: str | Path,
                 settings: AnalysisSettings | None = None, *, plots: bool = True
                 ) -> list[Path]:
    """
    Analyse run folders and write the tables, summary and figures.

    Parameters
    ----------
    run_dirs : list of str or Path
        Finished run folders.
    out_dir : str or Path
        Where to write; created if missing.
    settings : AnalysisSettings or None
        The defaults when ``None``.
    plots : bool
        Write the figures as well as the tables.

    Returns
    -------
    list of Path
        The files written; ``analysis.txt`` among them.

    Raises
    ------
    PlumeIRError
        For a folder that is not a finished run, an unknown gas, ...
    """
    from plumeIR.provenance import package_versions
    settings = settings or AnalysisSettings()
    out_dir = Path(out_dir)
    analyses = analyse_runs(run_dirs, settings)
    paths = write_analysis(analyses, out_dir, settings, package_versions())
    if plots:
        from plumeIR.plotting.analysis_report import save_composition_figures
        paths += save_composition_figures(analyses, out_dir, settings)
    logger.info("%d files written to %s", len(paths), out_dir)
    return paths


def main(argv: list[str] | None = None) -> int:
    """
    Entry point of ``plumeIR analyse`` and ``python -m plumeIR.analysis``.

    Parameters
    ----------
    argv : list of str or None
        Arguments after ``analyse``; ``sys.argv[1:]`` if ``None``.

    Returns
    -------
    int
        Exit status.
    """
    parser = build_parser()
    args = parser.parse_args(argv)
    if not args.width > 0:
        parser.error("--width must be positive")
    if len(args.runs) > 1 and args.output_dir is None:
        parser.error("several run folders need --output-dir")
    for d in args.runs:
        if not d.expanduser().is_dir():
            parser.error(f"{d} is not a folder")
    settings = AnalysisSettings(width=args.width,
                                unit="spectra" if args.spectra else "minutes",
                                reference=args.reference,
                                gases=tuple(args.gases) if args.gases else None,
                                exclude=tuple(args.exclude),
                                min_r2=args.min_r2, jackknife=not args.no_jackknife,
                                correlated=not args.independent_errors)
    out_dir = (args.output_dir or args.runs[0] / ANALYSIS_DIR).expanduser().resolve()

    console = logging.StreamHandler(sys.stderr)
    console.setFormatter(logging.Formatter("%(asctime)s %(levelname)-7s %(message)s",
                                           datefmt="%H:%M:%S"))
    root = logging.getLogger("plumeIR")
    previous = root.level
    root.addHandler(console)
    root.setLevel(logging.INFO)
    try:
        run_analysis(args.runs, out_dir, settings, plots=not args.no_plots)
        sys.stdout.write((out_dir / "analysis.txt").read_text(encoding="utf-8"))
        return 0
    except PlumeIRError as exc:
        logger.error("%s: %s", type(exc).__name__, exc)
        return 1
    finally:
        root.removeHandler(console)
        root.setLevel(previous)
