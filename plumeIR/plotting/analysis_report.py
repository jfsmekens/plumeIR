"""
The figures of a composition analysis (``plumeIR.analysis.report``), for
``plumeIR analyse``.

:func:`save_composition_figures` writes, per run, the molar composition
(donut beside the stacked fractions), the rank table and, through
:func:`plumeIR.plotting.analysis.save_analysis_figures`, the moving-ratio
figures per pair; with two or more runs, the runs' donuts side by side and
their ratios.  Each run is drawn from its own composition table, never from
the runs stacked into one, since runs may overlap in time (the same spectra
under two configs).  Figures are built with
``matplotlib.figure.Figure``; no pyplot, no window.
"""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np
import pandas as pd
from matplotlib.figure import Figure

from plumeIR._style import ANNOTATION_SIZE, MARKERS, style_context
from plumeIR.analysis.data import is_mass_only
from plumeIR.analysis.ratios import moving_ratio, pooled_ratio, run_ratios, window_spread
from plumeIR.analysis.report import AnalysisSettings, RunAnalysis
from plumeIR.plotting.analysis import draw_run_ratios, ratio_label, save_analysis_figures
from plumeIR.plotting.components import (
    draw_composition_donut, draw_rank_table, draw_stacked_fractions, gas_colors,
)

logger = logging.getLogger(__name__)

#: Donuts per row in the comparison of runs.
DONUTS_PER_ROW = 4


def save_composition_figures(analyses: list[RunAnalysis], out_dir: str | Path,
                             settings: AnalysisSettings) -> list[Path]:
    """
    Write the figures of a composition analysis.

    Parameters
    ----------
    analyses : list of RunAnalysis
        From :func:`~plumeIR.analysis.report.analyse_runs`.
    out_dir : str or Path
        Created if missing.
    settings : AnalysisSettings

    Returns
    -------
    list of Path
        Per run, in ``out_dir`` for one run and in ``out_dir/<run>/`` for
        several: ``composition.png`` (donut and stacked fractions),
        ``rank.png`` and the figures of
        :func:`~plumeIR.plotting.analysis.save_analysis_figures` (moving
        ratio and one window, per pair).  With several runs, in ``out_dir``:
        ``composition_runs.png`` (the donuts side by side) and
        ``ratios_runs.png`` (each run's line, window spread and pooled ratio,
        in run order when the runs overlap in time).
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    written: list[Path] = []
    w = f"{settings.width:g}-{'min' if settings.unit == 'minutes' else 'spectra'}"
    every = sorted({g for a in analyses for g in a.gases})
    colors = gas_colors(every)

    def save(fig: Figure, name: str) -> None:
        written.append(out_dir / name)
        fig.savefig(written[-1])

    single = len(analyses) == 1
    with style_context():
        for a in analyses:
            # Several runs: each in its own subfolder, from its own table (runs
            # may overlap in time, e.g. the same spectra under two configs).
            run_out = out_dir if single else out_dir / a.label
            run_out.mkdir(parents=True, exist_ok=True)
            # Molar over the gases (two or more), by mass over every species
            # (when there are aerosols).
            kinds = ([(False, "composition.png", "Molar composition of the measured gases")]
                     if len(a.molar_gases) >= 2 else [])
            if a.aerosols:
                kinds.append((True, "composition_mass.png",
                              "Mass composition of the measured gases and aerosols"))
            for mass, name, title in kinds:
                fig = Figure(figsize=(14.0, 5.6), layout="constrained")
                gs = fig.add_gridspec(1, 2, width_ratios=[1.25, 2.0])
                draw_composition_donut(fig.add_subplot(gs[0]), a.composition, colors=colors,
                                       mass=mass)
                ax = fig.add_subplot(gs[1])
                draw_stacked_fractions(ax, a.moving, colors=colors, mass=mass)
                ax.set_title(f"{w} centred windows; band edges are cumulative fractions, "
                             "smallest at the bottom", fontsize=9, loc="left")
                fig.suptitle(f"{title}: {a.label} (all-species fit, {w} offsets)")
                written.append(run_out / name)
                fig.savefig(written[-1])

            fig = Figure(figsize=(6.4, 4.4), layout="constrained")
            ax = fig.add_subplot()
            draw_rank_table(ax, a.rank, color=colors[a.reference])
            ax.plot([], [], "_", ms=12, mew=2, color="k", label="expected from noise alone")
            ax.legend(fontsize=ANNOTATION_SIZE + 1)
            ax.set_title(f"{a.label}: what each further component explains "
                         f"({w} offsets); labels: removed / expected", fontsize=9)
            written.append(run_out / "rank.png")
            fig.savefig(written[-1])

            pairs = [(g, a.reference) for g in a.gases if g != a.reference]
            written += save_analysis_figures(a.comp, run_out, pairs=pairs,
                                             widths=(settings.width,), unit=settings.unit,
                                             pooled_width=settings.width)

        if not single:
            n = len(analyses)
            cols = min(n, DONUTS_PER_ROW)
            rows = int(np.ceil(n / cols))
            fig = Figure(figsize=(5.2 * cols, 5.6 * rows), layout="constrained")
            axes = np.atleast_1d(fig.subplots(rows, cols, squeeze=False)).ravel()
            mass = any(len(a.molar_gases) < 2 for a in analyses)
            for ax, a in zip(axes, analyses):
                draw_composition_donut(ax, a.composition, colors=colors, mass=mass)
                ax.set_title(f"{a.label}\n{a.comp.time.min():%Y-%m-%d %H:%M}",
                             fontsize=ANNOTATION_SIZE + 2)
            for ax in axes[n:]:
                ax.set_axis_off()
            fig.suptitle(f"{'Mass' if mass else 'Molar'} composition per run (fractions of "
                         f"each run's measured {'species' if mass else 'gases'})")
            save(fig, "composition_runs.png")
            fig = _runs_figure(analyses, settings, colors)
            if fig is not None:
                save(fig, "ratios_runs.png")
    logger.info("analysis figures: %d written to %s", len(written), out_dir)
    return written


def _overlap(analyses: list[RunAnalysis]) -> bool:
    """Whether any two runs share a stretch of time."""
    spans = sorted((a.comp.time.min(), a.comp.time.max()) for a in analyses)
    return any(nxt[0] <= prev[1] for prev, nxt in zip(spans, spans[1:]))


def _runs_figure(analyses: list[RunAnalysis], settings: AnalysisSettings,
                 colors: dict) -> Figure | None:
    """
    Each run's ratios against the reference: run line ±1σ, the spread of its
    moving-window slopes, and its pooled ratio, every run computed from its
    own table.  Runs that overlap in time are drawn in run order.
    """
    ref = analyses[0].reference
    if any(a.reference != ref for a in analyses):
        logger.warning("runs have different reference gases: no ratio comparison drawn")
        return None
    gases = [g for g in analyses[0].gases if g != ref and all(g in a.gases for a in analyses)]
    if not gases:
        return None
    by_order = _overlap(analyses)
    w, unit = settings.width, settings.unit
    fig = Figure(figsize=(6.4 if not by_order else 1.6 * len(analyses) + 3.0,
                          2.7 * len(gases)), layout="constrained")
    axes = np.atleast_1d(fig.subplots(len(gases), 1, sharex=True, squeeze=False)[:, 0])
    for i, (ax, g) in enumerate(zip(axes, gases)):
        line = pd.concat([run_ratios(a.comp, g, ref, jackknife_width=w, unit=unit)
                          .rename(index=lambda _: a.label)
                          for a in analyses])
        spread = pd.concat([window_spread(moving_ratio(a.comp, g, ref, w, unit=unit))
                            .rename(index=lambda _: a.label) for a in analyses])
        pooled = pd.concat([pooled_ratio(a.comp, g, ref, w, unit=unit)
                            .rename(index=lambda _: a.label) for a in analyses])
        draw_run_ratios(ax, line, color=colors[g], marker=MARKERS[i % len(MARKERS)],
                        label="run line ±1σ", spread=spread, pooled=pooled,
                        spread_label=f"{w:g} {'min' if unit == 'minutes' else 'spectra'} "
                                     "windows: median, 5%-95%",
                        pooled_label="pooled ±1σ", by_order=by_order)
        ax.set_ylabel(ratio_label(g, ref, mass=is_mass_only(g)))
        if ax is not axes[-1]:
            ax.set_xlabel("")
    axes[0].legend(loc="lower left", bbox_to_anchor=(0.0, 1.0), frameon=False, ncol=3,
                   fontsize=ANNOTATION_SIZE + 1)
    fig.suptitle("Ratios per run" + (" (runs overlap in time: in run order)" if by_order else ""))
    return fig
