"""
The composition analysis of finished runs, as tables and a text summary
(the command line is :mod:`plumeIR.analysis.cli`).

:func:`analyse_runs` reads run folders and computes, per run: the York line
and the pooled ratio of every gas against the reference, the spread of the
moving-window slopes, the all-gas (rank-1) composition with its molar
fractions, the moving all-gas composition, and the rank table.
:func:`write_analysis` writes them as CSV with ``analysis.txt``.  Nothing is
refitted; no figure is drawn here (``plumeIR.plotting.analysis_report``).

Settings and their defaults (decided 2026-11-16):
windows of 2 minutes, centred (finished runs), offsets per window (a
background that drifts slowly with the plume level; the pooled ratio is
the one to quote), the reference SO2 when present.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field, replace
from pathlib import Path

import numpy as np
import pandas as pd

from plumeIR.analysis.components import (
    Composition, MovingComposition, composition, moving_composition, rank_table,
)
from plumeIR.analysis.data import CompositionTable, composition_table, molecule
from plumeIR.analysis.ratios import (
    moving_ratio, pooled_ratio, quoted_error, run_ratios, window_spread,
)
from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

RATIOS_NAME = "ratios.csv"
MOVING_NAME = "moving_composition.csv"
RANK_NAME = "rank.csv"
SUMMARY_NAME = "analysis.txt"
#: A further component removing this many times what noise would is reported.
RANK_FLAG = 5.0


@dataclass(frozen=True)
class AnalysisSettings:
    """
    Settings of a composition analysis.

    Attributes
    ----------
    width : float
        Window width in ``unit``, for the pooled ratio, the moving windows
        and the all-gas fit's offsets.
    unit : {'minutes', 'spectra'}
    reference : str or None
        Gas the ratios are quoted against; SO2 when present, else the first.
    gases : tuple of str or None
        Gases to analyse; every target gas of the run when ``None``.
    min_r2 : float or None
        Spectra whose target window fitted below this R² are left out.
    jackknife : bool
        Jackknife errors of the all-gas fits (the moving ones cost most).
    correlated : bool
        Use the error correlation of species fitted in one window (from the
        run's stored fit covariances); ``False`` treats every error as
        independent, as before 2026-11-18, for comparison.
    """
    width:      float = 2.0
    unit:       str = "minutes"
    reference:  str | None = None
    gases:      tuple | None = None
    min_r2:     float | None = None
    jackknife:  bool = True
    correlated: bool = True


@dataclass(frozen=True)
class RunAnalysis:
    """
    The analysis of one run.

    Attributes
    ----------
    label : str
        The run folder's name.
    run_dir : Path
    comp : CompositionTable
    reference : str
    ratios : pandas.DataFrame
        One row per species other than the reference (aerosols by mass,
        g g⁻¹; gases molar; ``unit``): ``run_line``,
        ``run_line_err`` (block jackknife over windows of ``width``, York's
        where it cannot be computed), ``run_line_err_york``,
        ``run_line_mswd``, ``pooled``, ``pooled_err`` (the same),
        ``pooled_err_york``, ``pooled_mswd``, ``window_median``, ``window_p05``, ``window_p95``,
        ``all_gas``, ``all_gas_err``; and one row per species (reference
        included) of ``fraction``, ``fraction_err`` (molar, over the gases;
        NaN for aerosols), ``mass_fraction``, ``mass_fraction_err`` (over
        every species).
    composition : Composition
    moving : MovingComposition
    rank : pandas.DataFrame
    """
    label:       str
    run_dir:     Path
    comp:        CompositionTable = field(repr=False)
    reference:   str
    ratios:      pd.DataFrame = field(repr=False)
    composition: Composition = field(repr=False)
    moving:      MovingComposition = field(repr=False)
    rank:        pd.DataFrame = field(repr=False)

    @property
    def gases(self) -> list[str]:
        """Every species analysed, gases and aerosols."""
        return list(self.composition.ratios.index)

    @property
    def molar_gases(self) -> list[str]:
        """The gases: what the molar fractions are of."""
        return [g for g in self.gases if g in self.comp.gases]

    @property
    def aerosols(self) -> list[str]:
        return [g for g in self.gases if g in self.comp.aerosols]


def analyse_table(comp: CompositionTable, label: str, run_dir: Path,
                  settings: AnalysisSettings) -> RunAnalysis:
    """
    Analyse one run's composition table.

    Parameters
    ----------
    comp : CompositionTable
    label : str
    run_dir : Path
    settings : AnalysisSettings

    Returns
    -------
    RunAnalysis

    Raises
    ------
    PlumeIRError
        For fewer than two species, no gas, or an unknown reference or
        species.
    """
    gases = list(settings.gases) if settings.gases else list(comp.species)
    missing = [g for g in gases if g not in comp.species]
    if missing:
        raise PlumeIRError(f"{label}: {missing} are not targets of the run "
                           f"(targets: {comp.species})")
    if len(gases) < 2:
        raise PlumeIRError(f"{label}: a composition needs at least two species, got {gases}")
    molar = [g for g in gases if g in comp.gases]
    if not molar:
        raise PlumeIRError(f"{label}: no gas among {gases}: ratios and fractions need a "
                           f"reference gas")
    reference = settings.reference or ("SO2" if "SO2" in molar else molar[0])
    if reference not in molar:
        raise PlumeIRError(f"{label}: reference {reference!r} must be a gas among {molar}")
    w, unit = settings.width, settings.unit
    c = composition(comp, gases, reference=reference, intercept="window", width=w, unit=unit,
                    jackknife=settings.jackknife)
    mc = moving_composition(comp, w, gases, reference=reference, unit=unit,
                            jackknife_blocks=8 if settings.jackknife else 0)
    rank = rank_table(comp, gases, intercept="window", width=w, unit=unit)
    rows = {}
    for g in gases:
        row = dict(unit=c.ratios.loc[g, "unit"], fraction=c.ratios.loc[g, "fraction"],
                   fraction_err=c.ratios.loc[g, "fraction_err"],
                   mass_fraction=c.ratios.loc[g, "mass_fraction"],
                   mass_fraction_err=c.ratios.loc[g, "mass_fraction_err"])
        if g != reference:
            rt = run_ratios(comp, g, reference, jackknife_width=w, unit=unit)
            pt = pooled_ratio(comp, g, reference, w, unit=unit)
            rr, pr = rt.iloc[0], pt.iloc[0]
            sp = window_spread(moving_ratio(comp, g, reference, w, unit=unit)).iloc[0]
            row.update(run_line=rr.slope, run_line_err=quoted_error(rt)[0],
                       run_line_err_york=rr.slope_err, run_line_mswd=rr.mswd,
                       pooled=pr.slope, pooled_err=quoted_error(pt)[0],
                       pooled_err_york=pr.slope_err, pooled_mswd=pr.mswd,
                       window_median=sp["median"], window_p05=sp["lower"],
                       window_p95=sp["upper"], all_gas=c.ratios.loc[g, "ratio"],
                       all_gas_err=c.ratios.loc[g, "ratio_err"])
        rows[g] = row
    ratios = pd.DataFrame.from_dict(rows, orient="index")
    ratios.index.name = "gas"
    return RunAnalysis(label=label, run_dir=Path(run_dir), comp=comp, reference=reference,
                       ratios=ratios, composition=c, moving=mc, rank=rank)


def analyse_runs(run_dirs: list[str | Path], settings: AnalysisSettings | None = None
                 ) -> list[RunAnalysis]:
    """
    Analyse finished run folders, one by one, in time order.

    Parameters
    ----------
    run_dirs : list of str or Path
    settings : AnalysisSettings or None

    Returns
    -------
    list of RunAnalysis
        Sorted by each run's first spectrum.

    Raises
    ------
    PlumeIRError
        For a folder that is not a finished run (``RunFolderError``), two
        folders with the same name, or a run that cannot be analysed.
    """
    from plumeIR.output import load_run

    settings = settings or AnalysisSettings()
    names = [Path(d).expanduser().resolve().name for d in run_dirs]
    if len(set(names)) != len(names):
        raise PlumeIRError(f"run folders must have distinct names, got {names}")
    out = []
    for d in run_dirs:
        run = load_run(d, require_fits=False)
        label = run.run_dir.name
        fits = getattr(run, "fits", None)
        if fits is None:
            logger.info("%s: no stored fits (a run written before 2026-11-12): "
                        "errors treated as independent", label)
        comp = composition_table(run.table, run.config, run=label, min_r2=settings.min_r2,
                                 fits=fits)
        if not settings.correlated and comp.correlation:
            comp = replace(comp, correlation={})
            logger.info("%s: errors treated as independent (--independent-errors)", label)
        logger.info("%s: %d spectra, gases %s", label, len(comp), comp.gases)
        out.append(analyse_table(comp, label, run.run_dir, settings))
    return sorted(out, key=lambda a: a.comp.time.min())


def write_analysis(analyses: list[RunAnalysis], out_dir: str | Path,
                   settings: AnalysisSettings, versions: dict | None = None) -> list[Path]:
    """
    Write the tables and the summary of an analysis.

    Parameters
    ----------
    analyses : list of RunAnalysis
    out_dir : str or Path
        Created if missing.
    settings : AnalysisSettings
    versions : dict or None
        Package versions for the summary (``plumeIR.provenance``).

    Returns
    -------
    list of Path
        ``ratios.csv`` (per run and gas), ``moving_composition.csv`` (per
        spectrum), ``rank.csv`` (per run and rank), ``analysis.txt``.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    ratios = pd.concat({a.label: a.ratios for a in analyses}, names=["run"])
    moving = pd.concat([a.moving.table.assign(spectrum_id=a.comp.spectrum_id)
                        for a in analyses], ignore_index=True)
    rank = pd.concat({a.label: a.rank for a in analyses}, names=["run"])
    paths = [out_dir / RATIOS_NAME, out_dir / MOVING_NAME, out_dir / RANK_NAME]
    ratios.to_csv(paths[0], float_format="%.6g")
    moving.to_csv(paths[1], index=False, float_format="%.6g")
    rank.to_csv(paths[2], float_format="%.6g")
    paths.append(out_dir / SUMMARY_NAME)
    paths[-1].write_text(analysis_summary(analyses, settings, versions), encoding="utf-8")
    return paths


def _pm(v: float, e: float, digits: int = 4) -> str:
    if not np.isfinite(v):
        return "n/a"
    return f"{v:.{digits}g}" + (f" ± {e:.2g}" if np.isfinite(e) else "")


def analysis_summary(analyses: list[RunAnalysis], settings: AnalysisSettings,
                     versions: dict | None = None) -> str:
    """The text of ``analysis.txt``."""
    w = f"{settings.width:g} {'min' if settings.unit == 'minutes' else 'spectra'}"
    lines = ["plumeIR composition analysis", "",
             f"windows         {w}, centred; offsets per window (a background "
             f"constant within each)",
             "quote           the pooled ratio (one slope, an intercept per window); the "
             "run line is biased",
             "                high where the background drifts with the plume level "
             "(README, Composition analysis)",
             f"errors          block jackknife, one block per {w} window (York's, "
             "√MSWD-scaled, are",
             "                in ratios.csv: 1.4-1.9x smaller on the validation run, "
             "missing slow variation)"]
    if settings.min_r2 is not None:
        lines.append(f"min R²          {settings.min_r2:g}")
    if versions:
        lines.append("versions        " + ", ".join(f"{k} {v}" for k, v in versions.items()))
    for a in analyses:
        t = a.comp.time
        lines += ["", f"== {a.label}", f"folder          {a.run_dir}",
                  f"spectra         {len(a.comp)}, {t.min():%Y-%m-%d %H:%M:%S} to "
                  f"{t.max():%H:%M:%S}, {len(np.unique(a.comp.segment))} segment(s)",
                  f"gases           {', '.join(a.molar_gases)} (molar fractions are of these "
                  "only" + ("" if "H2O" in a.molar_gases else "; no H2O target: dry gases")
                  + ")"]
        if a.aerosols:
            desc = []
            for g in a.aerosols:
                m = molecule(g)
                desc.append(f"{g} (by mass)" if m is None else
                            f"{g} (as its {m}: molar ratio, mass fraction of the {m})")
            lines.append(f"aerosols        {'; '.join(desc)}")
        usable = {g: int(np.sum(np.isfinite(a.comp.column(g)[1]) & (a.comp.column(g)[1] > 0)))
                  for g in a.gases}
        if min(usable.values()) < len(a.comp):
            lines.append("usable          spectra with a valid value and error: "
                         + ", ".join(f"{g} {n}" for g, n in usable.items())
                         + f" of {len(a.comp)}")
        lines += ["", f"ratio to {a.reference:<6}  run line            pooled              "
                  f"all species         window 5-95%       unit"]
        for g, r in a.ratios.drop(index=a.reference).iterrows():
            spread = f"{r.window_p05:.4g} - {r.window_p95:.4g}"
            lines.append(f"  {g:<14}{_pm(r.run_line, r.run_line_err):<20}"
                         f"{_pm(r.pooled, r.pooled_err):<20}{_pm(r.all_gas, r.all_gas_err):<20}"
                         f"{spread:<20}{r.unit}")
        if len(a.molar_gases) >= 2:
            lines += ["", "molar fraction (of the gases above)"]
            for g, r in a.ratios.dropna(subset=["fraction"]).sort_values(
                    "fraction", ascending=False).iterrows():
                lines.append(f"  {g:<14}{100 * r.fraction:.4g} ± {100 * r.fraction_err:.2g} %")
        if a.aerosols:
            lines += ["", "mass fraction (of every species above)"]
            for g, r in a.ratios.sort_values("mass_fraction", ascending=False).iterrows():
                lines.append(f"  {g:<14}{100 * r.mass_fraction:.4g} ± "
                             f"{100 * r.mass_fraction_err:.2g} %")
        rk = a.rank.dropna(subset=["delta_chi2"])
        excess = rk.delta_chi2 / rk.delta_dof
        lines += ["", "components      χ² removed / expected from noise: "
                  + ", ".join(f"rank {k} {v:.3g}×" for k, v in excess.items())]
        extra = [k for k, v in excess.items() if k >= 2 and v > RANK_FLAG]
        if extra:
            lines.append(f"                more than one source, or errors that understate "
                         f"the scatter: rank {max(extra)} still removes "
                         f"{excess[max(extra)]:.3g}× noise; the single-source composition "
                         f"mixes them")
    return "\n".join(lines) + "\n"
