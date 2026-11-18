"""
Plots of the composition analysis (``plumeIR.analysis``).

Drawing functions take an ``Axes`` and draw into it; they never create
figures or call ``pyplot``.  :func:`save_analysis_figures` builds the
standard set with ``matplotlib.figure.Figure``; a GUI calls the drawing
functions into its own panels.

- :func:`draw_moving_ratio`: a moving-window ratio against time, with its
  ±1σ band, the window width as a bar, and optionally the run's own line.
- :func:`draw_window_quality`: the windows' MSWD or plume leverage
  (``x_span_snr``) against time, to stack under the ratio.
- :func:`draw_run_ratios`: one ratio per run or segment against time, with
  the spread of its moving-window slopes, for comparing runs and days.
  Moving-window curves are drawn per run, never overlaid across runs.
- :func:`draw_window_scatter`: the points of one window with its line, the
  rest of the run faint behind: what lies behind one point of the curve.

**Time axis.** ``time='clock'`` plots the acquisition time as stamped in
the spectra (the instrument clock; no time zone is applied);
``time='elapsed'`` plots minutes since ``t0`` (the first spectrum unless
given, so several curves share an origin).  A table without times falls
back to the spectrum number.  A gap between acquisition segments breaks
every line.

**Error band.** The quoted slope error, i.e. York's internal error scaled
by √MSWD where MSWD > 1 (:func:`plumeIR.regression.york_fit`), so the band
is the real uncertainty of the line; the MSWD panel shows how much of it is
excess scatter.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Sequence

import matplotlib.dates as mdates
import numpy as np
import pandas as pd
from matplotlib.figure import Figure
from matplotlib.transforms import blended_transform_factory, offset_copy

from plumeIR._style import (
    ANNOTATION_SIZE, LINESTYLES, MARKERS, discrete_colors, lighten, style_context,
)
from plumeIR.analysis.data import is_mass_only
from plumeIR.analysis.ratios import (
    moving_ratio, pooled_ratio, quoted_error, run_ratios, window_indices, window_spread,
)
from plumeIR.constants import PRETTY_NAMES
from plumeIR.exceptions import PlumeIRError

if TYPE_CHECKING:
    from matplotlib.axes import Axes

    from plumeIR.analysis.data import CompositionTable
    from plumeIR.analysis.ratios import MovingRatio

logger = logging.getLogger(__name__)

TIME_MODES: tuple[str, ...] = ("clock", "elapsed")
#: Legends are dropped beyond this many entries (house style).
MAX_LEGEND_ENTRIES = 12

#: Run lines are annotated with their MSWD up to this many runs.
MAX_ANNOTATED_RUNS = 12
#: Fraction of the band alpha (house style keeps bands light).
_BAND_ALPHA = 0.35
_QUALITY_LABELS = {"mswd": "MSWD", "x_span_snr": "Plume span / x error"}


def pretty(species: str) -> str:
    """Display name of a species (mathtext), or the name itself."""
    return PRETTY_NAMES.get(species, species)


def ratio_label(y: str, x: str, excess: bool = False, mass: bool = False) -> str:
    """Axis label of the ratio ``y / x``: molar, or by mass (g g⁻¹)."""
    unit = "g g$^{-1}$" if mass else "mol mol$^{-1}$"
    return f"{pretty(y)}/{pretty(x)}{' excess' if excess else ''} [{unit}]"


def width_label(width: float, unit: str) -> str:
    """``'2 min'`` or ``'30 spectra'``."""
    return f"{width:g} min" if unit == "minutes" else f"{width:g} spectra"


# =============================================================================
# Time axis
# =============================================================================

def time_values(time: pd.DatetimeIndex, mode: str = "clock",
                t0: pd.Timestamp | None = None) -> tuple[np.ndarray, float | None]:
    """
    Plot coordinates of acquisition times.

    Parameters
    ----------
    time : pandas.DatetimeIndex
    mode : {'clock', 'elapsed'}
    t0 : pandas.Timestamp or None
        Origin for ``'elapsed'``; the first time when ``None``.

    Returns
    -------
    x : numpy.ndarray
        Matplotlib date numbers (clock), minutes since ``t0`` (elapsed), or
        the position (no times).
    per_minute : float or None
        Units of ``x`` per minute; ``None`` when ``x`` is a position.

    Raises
    ------
    PlumeIRError
        For an unknown mode.
    """
    if mode not in TIME_MODES:
        raise PlumeIRError(f"time must be one of {TIME_MODES}, got {mode!r}")
    time = pd.DatetimeIndex(time)
    if len(time) == 0 or time.isna().any():
        return np.arange(len(time), dtype=float), None
    if mode == "clock":
        return mdates.date2num(time.to_numpy()), 1.0 / 1440.0
    origin = time.min() if t0 is None else pd.Timestamp(t0)
    return (time - origin).total_seconds().to_numpy(float) / 60.0, 1.0


def format_time_axis(ax: "Axes", mode: str, per_minute: float | None) -> None:
    """Locator, formatter and label for an axis drawn by :func:`time_values`."""
    if per_minute is None:
        ax.set_xlabel("Spectrum number")
    elif mode == "clock":
        locator = mdates.AutoDateLocator()
        ax.xaxis.set_major_locator(locator)
        ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))
        ax.set_xlabel("Time (instrument clock)")
    else:
        ax.set_xlabel("Elapsed time [min]")


def _break_segments(x: np.ndarray, segment: np.ndarray, *ys: np.ndarray) -> list[np.ndarray]:
    """``x`` and ``ys`` with a NaN inserted at every change of segment."""
    cut = np.flatnonzero(np.diff(segment) != 0) + 1
    return [np.insert(np.asarray(a, float), cut, np.nan) for a in (x, *ys)]


# =============================================================================
# Drawing
# =============================================================================

def draw_moving_ratio(
    ax: "Axes",
    mr: "MovingRatio",
    *,
    time: str = "clock",
    t0: pd.Timestamp | None = None,
    color=None,
    linestyle: str = "-",
    label: str | None = None,
    band: bool = True,
    reference: float | None = None,
    reference_err: float | None = None,
    width_bar: bool = True,
    bar_row: int = 0,
) -> None:
    """
    A moving-window ratio against time.

    Parameters
    ----------
    ax : Axes
    mr : MovingRatio
    time : {'clock', 'elapsed'}
    t0 : pandas.Timestamp or None
        Origin for ``'elapsed'``.
    color
        Line colour; the first discrete colour when ``None``.
    linestyle : str
    label : str or None
        Legend label; ``'<width> <placement>'`` when ``None``.
    band : bool
        Draw the ±1σ band of the slope.
    reference, reference_err : float or None
        A ratio to draw as a horizontal line (typically the run's own, from
        :func:`~plumeIR.analysis.ratios.run_ratios`), in a lighter colour,
        with its error in the legend.
    width_bar : bool
        Draw the window width as a bar at the top left (time axes only).
    bar_row : int
        Stacks the bars of several curves on one axis.
    """
    color = discrete_colors(1)[0] if color is None else color
    t = mr.table
    x, per_minute = time_values(pd.DatetimeIndex(t["time"]), time, t0)
    slope, err = t["slope"].to_numpy(float), t["slope_err"].to_numpy(float)
    xb, sb, eb = _break_segments(x, t["segment"].to_numpy(), slope, err)
    if label is None:
        label = f"{width_label(mr.width, mr.unit)} {mr.placement}"
    if band:
        ax.fill_between(xb, sb - eb, sb + eb, color=lighten(color, 0.5),
                        alpha=_BAND_ALPHA, lw=0, zorder=1)
    ax.plot(xb, sb, color=color, ls=linestyle, lw=0.9, label=label, zorder=2)
    if reference is not None and np.isfinite(reference):
        txt = f"run {reference:.4g}" + (
            f" ± {reference_err:.2g}" if reference_err is not None and np.isfinite(reference_err)
            else "")
        ax.axhline(reference, color=lighten(color, 0.35), ls=":", lw=1.2, label=txt, zorder=1)
    if width_bar and per_minute is not None and np.isfinite(x).any():
        w = mr.width * per_minute if mr.unit == "minutes" else \
            mr.width * float(np.nanmedian(np.diff(x)))
        x0 = float(np.nanmin(x))
        yb = 0.94 - 0.07 * bar_row
        tr = blended_transform_factory(ax.transData, ax.transAxes)
        ax.plot([x0, x0 + w], [yb, yb], color=color, ls=linestyle, lw=2.0, transform=tr,
                solid_capstyle="butt", zorder=3)
        ax.text(x0 + w, yb, f" {width_label(mr.width, mr.unit)}", transform=tr,
                va="center", ha="left", fontsize=ANNOTATION_SIZE, color=color)
    ax.set_ylabel(ratio_label(mr.y, mr.x, mr.excess, mr.mass))
    format_time_axis(ax, time, per_minute)


def draw_window_quality(
    ax: "Axes",
    mr: "MovingRatio",
    which: str = "mswd",
    *,
    time: str = "clock",
    t0: pd.Timestamp | None = None,
    color=None,
    linestyle: str = "-",
    label: str | None = None,
) -> None:
    """
    A per-window diagnostic against time.

    Parameters
    ----------
    ax : Axes
    mr : MovingRatio
    which : {'mswd', 'x_span_snr'}
        MSWD (log axis, a line at 1: scatter as expected from the errors) or
        the plume span over the median x error (how much plume the window
        holds; a small value determines the slope poorly whatever its
        formal error).
    time, t0, color, linestyle, label
        As for :func:`draw_moving_ratio`.

    Raises
    ------
    PlumeIRError
        For an unknown ``which``.
    """
    if which not in _QUALITY_LABELS:
        raise PlumeIRError(f"which must be one of {tuple(_QUALITY_LABELS)}, got {which!r}")
    color = discrete_colors(1)[0] if color is None else color
    t = mr.table
    x, per_minute = time_values(pd.DatetimeIndex(t["time"]), time, t0)
    xb, vb = _break_segments(x, t["segment"].to_numpy(), t[which].to_numpy(float))
    ax.plot(xb, vb, color=color, ls=linestyle, lw=0.9, label=label)
    if which == "mswd":
        ax.set_yscale("log")
        ax.axhline(1.0, color="k", ls=":", lw=0.6, zorder=1)
    else:
        ax.set_ylim(bottom=0)
    ax.set_ylabel(_QUALITY_LABELS[which])
    format_time_axis(ax, time, per_minute)


def draw_run_ratios(
    ax: "Axes",
    table: pd.DataFrame,
    *,
    time: str = "clock",
    t0: pd.Timestamp | None = None,
    color=None,
    marker: str = "o",
    label: str | None = None,
    annotate_mswd: bool = True,
    by_order: bool = False,
    spread: pd.DataFrame | None = None,
    spread_label: str | None = None,
    pooled: pd.DataFrame | None = None,
    pooled_label: str | None = None,
) -> None:
    """
    One ratio per run (or segment) against time.

    Each run is a point at the middle of its span, with a horizontal bar
    over the span and a vertical ±1σ bar.  With ``annotate_mswd`` and at
    most :data:`MAX_ANNOTATED_RUNS` runs, each point carries its MSWD.
    With ``spread``, a wide light bar behind each point spans the run's
    moving-window slopes (lower to upper quantile) and a tick marks their
    median: how much the composition moved within the run, which the run's
    single line and its error do not show (decided with the developer,
    2026-11-16, in place of overlaying the runs' moving-window curves).
    With ``pooled``, an open diamond beside each point gives the run's
    pooled (within-window) ratio ±1σ, the estimator robust to a background
    that drifts with the plume level.

    Parameters
    ----------
    ax : Axes
    table : pandas.DataFrame
        From :func:`~plumeIR.analysis.ratios.run_ratios` (``start``,
        ``stop``, ``slope``, ``slope_err``, ``mswd``; ``slope_err_jk``,
        when present and finite, is the error drawn: ``quoted_error``).
    time : {'clock', 'elapsed'}
    t0 : pandas.Timestamp or None
        Origin for ``'elapsed'``; the earliest start when ``None``.
    color, marker, label
        Style of the points.
    annotate_mswd : bool
    by_order : bool
        One position per run, labelled with its index (``table``'s), instead
        of a time axis: for runs that overlap in time (the same spectra under
        two configs), which a time axis would draw on top of each other.
    spread : pandas.DataFrame or None
        From :func:`~plumeIR.analysis.ratios.window_spread`, with the same
        index as ``table`` (same ``by``).
    spread_label : str or None
        Legend label of the spread bars.
    pooled : pandas.DataFrame or None
        From :func:`~plumeIR.analysis.ratios.pooled_ratio`, indexed like
        ``table``.
    pooled_label : str or None
        Legend label of the pooled points.

    Raises
    ------
    PlumeIRError
        When ``spread`` or ``pooled`` is not indexed like ``table``.
    """
    color = discrete_colors(1)[0] if color is None else color
    start = pd.DatetimeIndex(table["start"])
    stop = pd.DatetimeIndex(table["stop"])
    if t0 is None and len(start) and not start.isna().any():
        t0 = start.min()
    xs, per_minute = time_values(start, time, t0)
    xe, _ = time_values(stop, time, t0)
    if per_minute is None or by_order:      # one run per position
        xs = xe = np.arange(len(table), dtype=float)
    xc = 0.5 * (xs + xe)
    slope = table["slope"].to_numpy(float)
    if spread is not None:
        if not spread.index.equals(table.index):
            raise PlumeIRError("spread is not indexed like the run table: compute both "
                               "with the same 'by' from the same table")
        ax.vlines(xc, spread["lower"].to_numpy(float), spread["upper"].to_numpy(float),
                  color=lighten(color, 0.6), lw=7, zorder=1, label=spread_label)
        ax.plot(xc, spread["median"].to_numpy(float), "_", ms=12, mew=1.2,
                color=lighten(color, 0.2), zorder=2)
    ax.errorbar(xc, slope, yerr=quoted_error(table),
                xerr=[xc - xs, xe - xc], fmt=marker, ms=4, color=color,
                ecolor=lighten(color, 0.3), elinewidth=0.9, label=label, zorder=3)
    if pooled is not None:
        if not pooled.index.equals(table.index):
            raise PlumeIRError("pooled is not indexed like the run table: compute both "
                               "with the same 'by' from the same table")
        ax.errorbar(xc, pooled["slope"].to_numpy(float),
                    yerr=quoted_error(pooled), fmt="D", ms=4, mfc="white",
                    mec=color, color=color, ecolor=lighten(color, 0.3), elinewidth=0.9,
                    zorder=3, label=pooled_label,
                    transform=offset_copy(ax.transData, fig=ax.figure, x=7, units="points"))
    if annotate_mswd and len(table) <= MAX_ANNOTATED_RUNS:
        for xi, yi, m in zip(xc, slope, table["mswd"].to_numpy(float)):
            if np.isfinite(yi) and np.isfinite(m):
                ax.annotate(f"MSWD {m:.2g}", (xi, yi), xytext=(4, 4),
                            textcoords="offset points", fontsize=ANNOTATION_SIZE,
                            color=color)
    if by_order:
        ax.set_xticks(xc, [str(k) for k in table.index], rotation=20, ha="right",
                      fontsize=ANNOTATION_SIZE + 1)
        ax.set_xlim(-0.5, len(table) - 0.5)
        ax.set_xlabel("Run")
    else:
        format_time_axis(ax, time, per_minute)


def draw_window_scatter(
    ax: "Axes",
    comp: "CompositionTable",
    mr: "MovingRatio",
    at: int,
    *,
    color=None,
    context: bool = True,
) -> None:
    """
    The points of one window with its line, as fitted by ``mr``.

    The line is the one :func:`~plumeIR.analysis.ratios.moving_ratio` fitted
    for spectrum ``at`` (``mr.table`` row ``at``), not a refit, so it is the
    point the curve shows.

    Parameters
    ----------
    ax : Axes
    comp : CompositionTable
        The table ``mr`` was computed from.
    mr : MovingRatio
    at : int
        Position of the spectrum whose window is shown.
    color
        Colour of the window's points and line.
    context : bool
        Draw every other valid point of the table faintly behind.

    Raises
    ------
    PlumeIRError
        When ``mr`` does not belong to ``comp`` (different length).
    """
    if len(mr.table) != len(comp):
        raise PlumeIRError(f"the moving ratio has {len(mr.table)} rows, the table {len(comp)}: "
                           f"compute it from this table")
    color = discrete_colors(1)[0] if color is None else color
    xv, sx = comp.column(mr.x, mr.excess, mr.mass)
    yv, sy = comp.column(mr.y, mr.excess, mr.mass)
    w = window_indices(comp, at, mr.width, unit=mr.unit, placement=mr.placement)
    ok = np.isfinite(xv) & np.isfinite(yv)
    if context:
        rest = ok.copy()
        rest[w] = False
        ax.plot(xv[rest], yv[rest], ".", ms=3, color=lighten("0.4", 0.55), zorder=1,
                label="rest of the table")
    wv = w[ok[w]]
    ax.errorbar(xv[wv], yv[wv], xerr=sx[wv], yerr=sy[wv], fmt="o", ms=3, color=color,
                ecolor=lighten(color, 0.4), elinewidth=0.7, zorder=2,
                label=f"window ({wv.size} spectra)")
    row = mr.table.iloc[at]
    if np.isfinite(row["slope"]) and wv.size:
        xl = np.array([np.nanmin(xv[wv]), np.nanmax(xv[wv])])
        ax.plot(xl, row["intercept"] + row["slope"] * xl, color="k", lw=1.2, zorder=3,
                label=f"York {row['slope']:.4g} ± {row['slope_err']:.2g}")
        ax.text(0.02, 0.97, f"MSWD {row['mswd']:.3g}\nintercept {row['intercept']:.3g}",
                transform=ax.transAxes, va="top", fontsize=ANNOTATION_SIZE)
    suffix = " excess" if mr.excess else ""
    unit = "g m$^{-2}$" if mr.mass else "molec cm$^{-2}$"
    ax.set_xlabel(f"{pretty(mr.x)}{suffix} [{unit}]")
    ax.set_ylabel(f"{pretty(mr.y)}{suffix} [{unit}]")


# =============================================================================
# The figure set
# =============================================================================

def _stamp(comp: "CompositionTable", at: int) -> str:
    """The time of spectrum ``at`` to the second, or its id."""
    t = comp.time[at]
    return comp.spectrum_id[at] if pd.isna(t) else t.strftime("%Y-%m-%d %H:%M:%S")


def default_pairs(comp: "CompositionTable", reference: str | None = None) -> list[tuple[str, str]]:
    """
    ``(y, x)`` pairs: every gas and aerosol against the reference gas.

    The reference defaults to SO2 when present, else the first gas.
    """
    gases = comp.gases
    if reference is None:
        reference = "SO2" if "SO2" in gases else (gases[0] if gases else None)
    if reference not in gases:
        raise PlumeIRError(f"reference {reference!r} is not a gas of the table (gases: {gases})")
    return [(g, reference) for g in comp.species if g != reference]


def _run_slug(k: int, n: int) -> str:
    """File-name suffix of the k-th of n runs (none for a single run)."""
    return "" if n == 1 else f"_run{k + 1:02d}"


def save_analysis_figures(
    comp: "CompositionTable",
    out_dir: Path,
    *,
    pairs: Sequence[tuple[str, str]] | None = None,
    reference: str | None = None,
    widths: Sequence[float] = (1.0, 5.0),
    unit: str = "minutes",
    placement: str = "centred",
    excess: bool = False,
    time: str = "clock",
    quantiles: tuple[float, float] = (0.05, 0.95),
    pooled_width: float | None = None,
) -> list[Path]:
    """
    Write the composition-analysis figures.

    Moving-window curves are drawn **per run**, each on its own time axis;
    runs are compared through ``analysis_runs.png`` (each run's line with
    the spread of its window slopes), never by overlaying curves, which
    leaves short runs a sliver of a long axis (decided with the developer,
    2026-11-16).

    Parameters
    ----------
    comp : CompositionTable
    out_dir : Path
        Created if missing.
    pairs : sequence of (y, x) or None
        Ratios to draw; every gas against ``reference`` when ``None``.
    reference : str or None
        See :func:`default_pairs`.
    widths : sequence of float
        Window widths, one curve each; the first also sets the spread in
        ``analysis_runs.png`` and the window scatter.
    unit, placement, excess
        As for :func:`~plumeIR.analysis.ratios.moving_ratio`.
    time : {'clock', 'elapsed'}
        Time axis of the per-run figures (elapsed from each run's start).
    quantiles : (float, float)
        Of the window slopes, for the spread.
    pooled_width : float or None
        Window of :func:`~plumeIR.analysis.ratios.pooled_ratio`, in
        ``unit``; the first width when ``None``.  Drawn as a dash-dot line in
        the per-run figures and as open diamonds in ``analysis_runs.png``.

    Returns
    -------
    list of Path
        Per pair and run ``analysis_ratio_<y>_<x>[_runNN].png`` (ratio, MSWD
        and plume span against time; the suffix numbers runs in time order
        when there are several) and per pair
        ``analysis_window_<y>_<x>.png`` (the first width's window around the
        spectrum with the most ``x``); with two or more runs (else segments)
        ``analysis_runs.png``.
    """
    if not widths:
        raise PlumeIRError("widths must hold at least one window width")
    pooled_width = widths[0] if pooled_width is None else pooled_width
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    pairs = default_pairs(comp, reference) if pairs is None else list(pairs)
    written: list[Path] = []
    colors = discrete_colors(len(widths))
    runs = list(pd.unique(comp.run))
    by = "run" if len(runs) >= 2 else ("segment" if len(np.unique(comp.segment)) >= 2 else None)
    first_mr: dict[tuple[str, str], "MovingRatio"] = {}

    def save(fig: Figure, name: str) -> None:
        written.append(out_dir / name)
        fig.savefig(written[-1])

    with style_context():
        for y, x in pairs:
            per_run = run_ratios(comp, y, x, excess=excess)
            pool = pooled_ratio(comp, y, x, pooled_width, unit=unit, excess=excess)
            mrs = [moving_ratio(comp, y, x, w, unit=unit, placement=placement, excess=excess)
                   for w in widths]
            first_mr[(y, x)] = mrs[0]
            for k, name in enumerate(runs):
                mask = comp.run == name
                t0 = None if comp.time[mask].isna().any() else comp.time[mask].min()
                ref = per_run.loc[name]
                fig = Figure(figsize=(10.0, 6.4), layout="constrained")
                axes = fig.subplots(3, 1, sharex=True, height_ratios=[2, 1, 1])
                for i, mr in enumerate(mrs):
                    sub = mr.select(mask)
                    style = dict(time=time, t0=t0, color=colors[i],
                                 linestyle=LINESTYLES[i % len(LINESTYLES)])
                    draw_moving_ratio(axes[0], sub, bar_row=i, **style)
                    draw_window_quality(axes[1], sub, "mswd", **style)
                    draw_window_quality(axes[2], sub, "x_span_snr", **style)
                if np.isfinite(ref.slope):
                    axes[0].axhline(ref.slope, color="0.35", ls=":", lw=1.2, zorder=1,
                                    label=f"run {ref.slope:.4g} ± "
                                          f"{quoted_error(per_run.loc[[name]])[0]:.2g}")
                pr = pool.loc[name]
                if np.isfinite(pr.slope):
                    axes[0].axhline(pr.slope, color="0.35", ls="-.", lw=1.0, zorder=1,
                                    label=f"pooled ({width_label(pooled_width, unit)}) "
                                          f"{pr.slope:.4g} ± "
                                          f"{quoted_error(pool.loc[[name]])[0]:.2g}")
                for ax in axes[:-1]:
                    ax.set_xlabel("")
                n_entries = len(axes[0].get_legend_handles_labels()[0])
                if 2 <= n_entries <= MAX_LEGEND_ENTRIES:
                    axes[0].legend(loc="lower left", bbox_to_anchor=(0.0, 1.0), frameon=False,
                                   ncol=min(n_entries, 4), fontsize=ANNOTATION_SIZE + 1)
                which = f" — {name}" if len(runs) > 1 else ""
                fig.suptitle(f"Moving-window ratio {pretty(y)}/{pretty(x)}{which} ({placement}; "
                             f"band ±1σ, scaled by √MSWD above 1)")
                save(fig, f"analysis_ratio_{y}_{x}{_run_slug(k, len(runs))}.png")

            xv, _ = comp.column(x, excess)
            if np.isfinite(xv).any():
                at = int(np.nanargmax(xv))
                fig = Figure(figsize=(6.4, 4.4), layout="constrained")
                ax = fig.add_subplot()
                draw_window_scatter(ax, comp, mrs[0], at, color=colors[0])
                ax.legend(loc="lower right", fontsize=ANNOTATION_SIZE)
                ax.set_title(f"{width_label(mrs[0].width, unit)} {placement} window at "
                             f"{_stamp(comp, at)}", fontsize=9, loc="left")
                save(fig, f"analysis_window_{y}_{x}.png")

        if by is not None and pairs:
            fig = Figure(figsize=(6.4, 2.7 * len(pairs)), layout="constrained")
            axes = np.atleast_1d(fig.subplots(len(pairs), 1, sharex=True, squeeze=False)[:, 0])
            pc = discrete_colors(len(pairs))
            lo, hi = quantiles
            for i, (ax, (y, x)) in enumerate(zip(axes, pairs)):
                mr = first_mr[(y, x)]
                draw_run_ratios(ax, run_ratios(comp, y, x, by=by, excess=excess), time=time,
                                color=pc[i], marker=MARKERS[i % len(MARKERS)],
                                label=f"{by} line ±1σ",
                                spread=window_spread(mr, by=by, quantiles=quantiles),
                                spread_label=(f"{width_label(mr.width, mr.unit)} windows: "
                                              f"median, {lo:.0%}-{hi:.0%}"),
                                pooled=pooled_ratio(comp, y, x, pooled_width, unit=unit, by=by,
                                                    excess=excess),
                                pooled_label=f"pooled ({width_label(pooled_width, unit)}) ±1σ")
                ax.set_ylabel(ratio_label(y, x, excess,
                                          is_mass_only(y) or is_mass_only(x)))
                if ax is not axes[-1]:
                    ax.set_xlabel("")
            axes[0].legend(loc="lower left", bbox_to_anchor=(0.0, 1.0), frameon=False,
                           ncol=3, fontsize=ANNOTATION_SIZE + 1)
            fig.suptitle(f"Ratio per {by}")
            save(fig, "analysis_runs.png")
    logger.info("analysis figures: %d written to %s", len(written), out_dir)
    return written
