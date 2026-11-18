"""
Plots of the multi-gas composition (``plumeIR.analysis.components``).

Drawing functions take an ``Axes`` and draw into it; no figures, no
``pyplot``.

- :func:`draw_moving_composition`: one gas's ratio to the reference from
  the moving rank-1 fit, against time, with its jackknife band.
- :func:`draw_rank_table`: the χ² each added component removes, against
  what noise alone would remove.
- :func:`draw_residuals`: one gas's normalised rank-1 residuals against
  time.
- :func:`draw_ratio_estimates`: one gas's ratio by several estimators,
  side by side.
- :func:`draw_composition_donut`: a run's molar composition as a donut,
  the whole to scale outside and the trace gases split out on an inner
  ring, every slice labelled with its gas and percentage (developer,
  2026-11-16: trace gases are invisible as slices of one ring).
- :func:`draw_stacked_fractions`: the molar fractions against time, stacked,
  on a log axis by default (developer, 2026-11-16).

Molar fractions are of the gases considered only: a gas not fitted as a
target (H2O, when it is only an ambient gas) is not in them, and every
fraction would change if it were.  Gases keep their package colours
(``constants.PLOT_COLORS``) across every figure.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Mapping

import numpy as np
import pandas as pd
from matplotlib.colors import to_rgb
from matplotlib.ticker import MaxNLocator

from plumeIR._style import ANNOTATION_SIZE, MARKERS, discrete_colors, lighten
from plumeIR.constants import PLOT_COLORS
from plumeIR.analysis.data import is_mass_only
from plumeIR.plotting.analysis import (
    _break_segments, format_time_axis, pretty, ratio_label, time_values,
)

if TYPE_CHECKING:
    from matplotlib.axes import Axes

    from plumeIR.analysis.components import Composition, MovingComposition


def draw_moving_composition(
    ax: "Axes",
    mc: "MovingComposition",
    gas: str,
    *,
    time: str = "clock",
    t0: pd.Timestamp | None = None,
    color=None,
    linestyle: str = "-",
    label: str | None = None,
    band: bool = True,
) -> None:
    """
    The ratio of ``gas`` to the reference from a moving rank-1 fit.

    Parameters
    ----------
    ax : Axes
    mc : MovingComposition
    gas : str
    time : {'clock', 'elapsed'}
    t0 : pandas.Timestamp or None
    color, linestyle, label
        Line style; the label defaults to ``'<width> all gases'``.
    band : bool
        Draw the ±1σ jackknife band (where computed).
    """
    color = discrete_colors(1)[0] if color is None else color
    t = mc.table
    x, per_minute = time_values(pd.DatetimeIndex(t["time"]), time, t0)
    v, e = mc.ratio(gas)
    xb, vb, eb = _break_segments(x, t["segment"].to_numpy(), v, e)
    unit = f"{mc.width:g} min" if mc.unit == "minutes" else f"{mc.width:g} spectra"
    if band and np.isfinite(eb).any():
        ax.fill_between(xb, vb - eb, vb + eb, color=lighten(color, 0.5), alpha=0.35, lw=0,
                        zorder=1)
    ax.plot(xb, vb, color=color, ls=linestyle, lw=0.9, zorder=2,
            label=label if label is not None else f"{unit}, all gases")
    ax.set_ylabel(ratio_label(gas, mc.reference, mc.excess, is_mass_only(gas)))
    format_time_axis(ax, time, per_minute)


def draw_rank_table(ax: "Axes", table: pd.DataFrame, *, color=None, label: str | None = None,
                    offset: float = 0.0) -> None:
    """
    The χ² each added component removes (bars), against the parameters it
    adds (what noise alone would remove; markers).

    Parameters
    ----------
    ax : Axes
    table : pandas.DataFrame
        From :func:`~plumeIR.analysis.components.rank_table`.
    color, label
        Style of the bars.
    offset : float
        Horizontal shift of the bars, to set several tables side by side.
    """
    color = discrete_colors(1)[0] if color is None else color
    t = table.dropna(subset=["delta_chi2"])
    k = t.index.to_numpy(float) + offset
    ax.bar(k, t["delta_chi2"], width=0.35, color=lighten(color, 0.2), label=label, zorder=2)
    ax.plot(k, t["delta_dof"], "_", ms=16, mew=2.0, color="k", zorder=3)
    for ki, d, n in zip(k, t["delta_chi2"], t["delta_dof"]):
        ax.text(ki, d, f"{d / n:.3g}×", ha="center", va="bottom", fontsize=ANNOTATION_SIZE)
    ax.set_yscale("log")
    ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    ax.set_xlabel("Component added (rank)")
    ax.set_ylabel(r"$\chi^2$ removed")


def draw_residuals(ax: "Axes", residuals: pd.DataFrame, times: pd.DatetimeIndex, gas: str, *,
                   time: str = "clock", segment: np.ndarray | None = None, color=None) -> None:
    """
    One gas's normalised residuals against time, with ±1 (the errors' own
    scale) and the residuals' rms marked.

    Parameters
    ----------
    ax : Axes
    residuals : pandas.DataFrame
        From :func:`~plumeIR.analysis.components.normalised_residuals`.
    times : pandas.DatetimeIndex
        Acquisition times, aligned with ``residuals``.
    gas : str
    time : {'clock', 'elapsed'}
    segment : numpy.ndarray or None
        Breaks the line between segments.
    color
    """
    color = discrete_colors(1)[0] if color is None else color
    x, per_minute = time_values(times, time)
    r = residuals[gas].to_numpy(float)
    seg = np.zeros(len(r), int) if segment is None else segment
    xb, rb = _break_segments(x, seg, r)
    ax.plot(xb, rb, color=color, lw=0.6)
    ax.axhspan(-1, 1, color="0.85", zorder=0, lw=0)
    rms = float(np.sqrt(np.nanmean(r ** 2)))
    ax.text(0.995, 0.92, f"{pretty(gas)}: rms {rms:.2g}", transform=ax.transAxes, ha="right",
            va="top", fontsize=ANNOTATION_SIZE + 1)
    ax.set_ylabel("Residual / error")
    format_time_axis(ax, time, per_minute)


def draw_ratio_estimates(ax: "Axes", estimates: Mapping[str, tuple[float, float]], *,
                         color=None) -> None:
    """
    One gas's ratio by several estimators, as points with ±1σ.

    Parameters
    ----------
    ax : Axes
    estimates : mapping of label → (ratio, error)
        In display order.
    color
    """
    color = discrete_colors(1)[0] if color is None else color
    labels = list(estimates)
    y = np.arange(len(labels))[::-1]
    for i, (yi, lab) in enumerate(zip(y, labels)):
        v, e = estimates[lab]
        ax.errorbar(v, yi, xerr=e, fmt=MARKERS[i % len(MARKERS)], ms=4, color=color,
                    ecolor=lighten(color, 0.3), elinewidth=0.9)
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    ax.xaxis.set_major_locator(MaxNLocator(4))
    ax.grid(axis="y", visible=False)


def gas_colors(gases: list[str]) -> dict[str, object]:
    """Each gas's package colour, with discrete colours for any not listed."""
    fallback = iter(discrete_colors(len(gases)))
    return {g: PLOT_COLORS.get(g) or next(fallback) for g in gases}


def _percent(f: float, e: float | None = None) -> str:
    """``'90.9%'`` or ``'0.0507 ± 0.0021%'``: three significant figures."""
    txt = f"{100 * f:.3g}"
    if e is not None and np.isfinite(e):
        txt += f" ± {100 * e:.2g}"
    return txt + "%"


def minor_gases(fractions: pd.Series, threshold: float | str = "auto") -> list[str]:
    """
    The gases to merge into "other".

    Parameters
    ----------
    fractions : pandas.Series
        Fraction per gas.
    threshold : float or 'auto'
        Gases below it; ``'auto'``: those below the largest gap in log10
        fraction between consecutive gases (the largest kept out), so the
        split falls where the composition has a natural break (on the
        example run between SO2 at 1.4% and HF at 0.06%).  At least two
        gases go into "other" and at least one stays, or none is merged.

    Returns
    -------
    list of str
        Largest first.
    """
    f = fractions.sort_values(ascending=False)
    f = f[f > 0]
    if threshold != "auto":
        minor = list(f[f < float(threshold)].index)
        return minor if len(minor) >= 2 and len(minor) < len(f) else []
    if len(f) < 3:
        return []
    gaps = -np.diff(np.log10(f.to_numpy(float)))
    gaps[-1] = -np.inf                       # leave at least two in "other"
    k = int(np.argmax(gaps))                 # split after position k
    return list(f.index[k + 1:])


#: Fractions below this are numerically zero (an aerosol fitted to its bound).
NUMERICAL_ZERO = 1e-8
#: Opacity of the stacked bands (their edges and error strips are opaque).
STACK_ALPHA = 0.5
#: How much the ±1σ strip darkens its gas's colour (0: none, 1: black), and
#: its opacity.  The edge itself is a white line over the strip (developer,
#: 2026-11-16, over strips in the plain colour, a pale halo, dashed lines, a
#: white strip and a black one: the plain-colour strip hid the edge).
STRIP_DARKEN, STRIP_ALPHA = 0.35, 0.85
#: Width of the white edge line.
EDGE_LW = 1.0

#: Colour of the merged "other" slice.
OTHER_COLOR = "0.72"


#: Ring radii (outer ring: the whole composition; inner ring: "other").
_OUTER_R, _OUTER_W = 1.0, 0.27
_INNER_R, _INNER_W = 0.67, 0.31
#: Narrowest slice [degrees] labelled on its ring; narrower ones are labelled
#: beside it with a leader line.
_MIN_ON_RING = {"outer": 14.0, "inner": 36.0}
#: Smallest vertical gap between labels placed beside their slices.
_LABEL_GAP = 0.13


def _darken(color, amount: float) -> tuple[float, float, float]:
    """``color`` mixed towards black by ``amount`` (0 unchanged, 1 black)."""
    return tuple(float(v) * (1.0 - amount) for v in to_rgb(color))


def _text_color(color) -> str:
    """White on dark fills, black on light ones."""
    r, g, b = to_rgb(color)
    return "white" if 0.2126 * r + 0.7152 * g + 0.0722 * b < 0.55 else "black"


def _spread(ys: list[float], gap: float) -> list[float]:
    """Push sorted label heights apart to at least ``gap``, keeping their mean."""
    if not ys:
        return ys
    order = np.argsort(ys)
    y = np.asarray(ys, float)[order]
    for _ in range(50):
        moved = False
        for i in range(1, y.size):
            d = y[i] - y[i - 1]
            if d < gap:
                shift = 0.5 * (gap - d)
                y[i - 1] -= shift
                y[i] += shift
                moved = True
        if not moved:
            break
    out = np.empty_like(y)
    out[order] = y
    return list(out)


def _label_ring(ax: "Axes", wedges, texts: list[str], ring: str) -> None:
    """Label each slice on its ring, or beside it with a leader line when thin."""
    r_out = _OUTER_R if ring == "outer" else _INNER_R
    width = _OUTER_W if ring == "outer" else _INNER_W
    r_mid = r_out - width / 2
    beside = {+1: [], -1: []}
    for w, txt in zip(wedges, texts):
        span = w.theta2 - w.theta1
        a = np.radians(0.5 * (w.theta1 + w.theta2))
        if span >= _MIN_ON_RING[ring]:
            ax.text(r_mid * np.cos(a), r_mid * np.sin(a), txt, ha="center", va="center",
                    fontsize=ANNOTATION_SIZE, color=_text_color(w.get_facecolor()),
                    linespacing=1.1)
        else:
            side = 1 if np.cos(a) >= 0 else -1
            beside[side].append((a, txt))
    leader = dict(arrowstyle="-", color="0.4", lw=0.6, shrinkA=0, shrinkB=0)
    if ring == "outer":
        # Outside the donut, on the slice's side, pushed apart vertically.
        r_txt = r_out + 0.22
        for side, items in beside.items():
            ys = _spread([r_txt * np.sin(a) for a, _ in items], _LABEL_GAP)
            for (a, txt), y in zip(items, ys):
                x = side * max(r_txt * abs(np.cos(a)), 0.8)
                ax.annotate(txt.replace("\n", " "), xy=(r_out * np.cos(a), r_out * np.sin(a)),
                            xytext=(x, y), ha="left" if side > 0 else "right", va="center",
                            fontsize=ANNOTATION_SIZE, arrowprops=leader)
        return
    # In the hole, centred, stacked above (or below) the centre text.
    hole = r_out - width
    items = beside[+1] + beside[-1]
    for sign in (+1, -1):
        row = 0
        for a, txt in sorted(items, key=lambda it: -abs(np.sin(it[0]))):
            if (np.sin(a) >= 0) != (sign > 0):
                continue
            y = sign * (0.62 * hole + row * _LABEL_GAP)
            ax.annotate(txt.replace("\n", " "), xy=(hole * np.cos(a), hole * np.sin(a)),
                        xytext=(0.0, y), ha="center", va="center",
                        fontsize=ANNOTATION_SIZE, arrowprops=leader)
            row += 1


def draw_composition_donut(
    ax: "Axes",
    c: "Composition",
    *,
    threshold: float | str = "auto",
    colors: dict | None = None,
    legend: bool = False,
    mass: bool = False,
) -> None:
    """
    A run's molar (or mass) composition as a donut, trace species split out
    inside.

    The outer ring is the whole composition to scale, with the gases below
    ``threshold`` (by default at the largest gap in log fraction,
    :func:`minor_gases`) merged into a grey "other" slice.  The inner ring
    opens "other" to the full circle, each trace gas taking its share of
    it.  Every slice is labelled with its gas and its share of the whole
    (``"HCl\n0.0479%"``): on the ring where it fits, beside it with a leader
    line where it does not.  The centre names the total of "other".  With
    fewer than two trace gases there is no inner ring and no "other".
    (Developer, 2026-11-16, over a pie, a pie of pie, and rings the other
    way round.)

    Parameters
    ----------
    ax : Axes
    c : Composition
        From :func:`~plumeIR.analysis.components.composition`.
    threshold : float or 'auto'
        As for :func:`minor_gases`.
    colors : dict or None
        Gas → colour; the package colours when ``None``.
    legend : bool
        Also list every gas with its share and jackknife error below the
        donut (the labels carry no errors).
    mass : bool
        Mass fractions over every species, aerosols included, instead of
        molar fractions over the gases.
    """
    col, col_err = ("mass_fraction", "mass_fraction_err") if mass else ("fraction", "fraction_err")
    t = c.ratios[np.isfinite(c.ratios[col])].sort_values(col, ascending=False)
    t = t.rename(columns={col: "_f", col_err: "_e"})
    colors = gas_colors(list(t.index)) if colors is None else colors
    minor = minor_gases(t["_f"], threshold)
    major = [g for g in t.index if g not in minor]
    other = float(t.loc[minor, "_f"].sum()) if minor else 0.0
    values = list(t.loc[major, "_f"]) + ([other] if minor else [])
    fills = [colors[g] for g in major] + ([OTHER_COLOR] if minor else [])
    wo, _ = ax.pie(values, radius=_OUTER_R, colors=fills, startangle=90, counterclock=False,
                   wedgeprops=dict(width=_OUTER_W, edgecolor="white", linewidth=0.8))
    texts = [f"{pretty(g)}\n{_percent(t.loc[g, '_f'])}" for g in major]
    if minor:
        texts.append(f"other\n{_percent(other)}")
    _label_ring(ax, wo, texts, "outer")
    handles, names = list(wo[:len(major)]), list(major)
    if minor:
        share = t.loc[minor, "_f"].to_numpy(float) / other
        wi, _ = ax.pie(share, radius=_INNER_R, colors=[colors[g] for g in minor], startangle=90,
                       counterclock=False,
                       wedgeprops=dict(width=_INNER_W, edgecolor="white", linewidth=0.8))
        _label_ring(ax, wi, [f"{pretty(g)}\n{_percent(t.loc[g, '_f'])}" for g in minor],
                    "inner")
        ax.text(0, 0, f"other\n{_percent(other)}", ha="center", va="center",
                fontsize=ANNOTATION_SIZE + 2, color="0.3", linespacing=1.2)
        handles += list(wi)
        names += minor
    ax.set_aspect("equal")
    ax.set_xlim(-1.45, 1.45)
    ax.set_ylim(-1.25, 1.25)
    if legend:
        ax.legend(handles, [f"{pretty(g)}  {_percent(t.loc[g, '_f'], t.loc[g, '_e'])}"
                            for g in names], loc="upper center", bbox_to_anchor=(0.5, 0.0),
                  ncol=2, frameon=False, fontsize=ANNOTATION_SIZE + 1, handlelength=1.0)


def draw_stacked_fractions(
    ax: "Axes",
    mc: "MovingComposition",
    *,
    order: list[str] | None = None,
    log: bool = True,
    floor: float | None = None,
    time: str = "clock",
    t0: pd.Timestamp | None = None,
    colors: dict | None = None,
    legend: bool = True,
    alpha: float = STACK_ALPHA,
    errors: bool = True,
    mass: bool = False,
) -> None:
    """
    The molar fractions (or with ``mass`` the mass fractions) against time,
    stacked to 1.

    Bands are stacked smallest gas at the bottom.  On the log axis (the
    default) a band's thickness is not its fraction; but with the smallest
    gases at the bottom, each band's **upper edge** is that gas's fraction
    plus all the smaller ones below it, which for fractions spread over
    decades is close to the gas's own fraction, so the edges read off the
    axis directly.  Where neighbouring gases are of similar size (HF and HCl
    on the example run, 5.8e-4 and 5.1e-4) the edge is their sum: read the
    pie or the fractions table for those.  The bottom band starts at
    ``floor``.

    Bands are half transparent (``alpha``), so the grid shows through, and
    each upper edge is a white line.  With ``errors``, the edge sits on a
    ±1σ strip in the gas's colour darkened (the jackknife error of the
    cumulative fraction, :meth:`MovingComposition.cumulative_fraction`,
    not of the gas alone); on a log axis the strip's width is a relative
    error, the same at every level (developer, 2026-11-16).

    Parameters
    ----------
    ax : Axes
    mc : MovingComposition
    order : list of str or None
        Bottom to top; by median fraction, smallest first, when ``None``.
    log : bool
        Log y axis; linear, from 0, when False.
    floor : float or None
        Lower edge of the bottom band on a log axis; when ``None``, the
        power of ten below the smallest species' 5th percentile, counting
        only fractions above :data:`NUMERICAL_ZERO`.
    time, t0
        As for :func:`draw_moving_composition`.
    colors : dict or None
        Gas → colour; the package colours when ``None``.
    legend : bool
        Gases top to bottom, as stacked.
    alpha : float
        Opacity of the bands.
    errors : bool
        Draw the ±1σ strip of each edge (when the jackknife was computed).
    mass : bool
        Mass fractions over every species, aerosols included; molar over
        the gases otherwise.
    """
    members = mc.gases if mass else mc.molar_species
    if order is None:
        med = {g: np.nanmedian(mc.fraction(g, mass)[0]) for g in members}
        order = sorted(members, key=lambda g: med[g])
    colors = gas_colors(list(order)) if colors is None else colors
    t = mc.table
    x, per_minute = time_values(pd.DatetimeIndex(t["time"]), time, t0)
    F = np.column_stack([mc.fraction(g, mass)[0] for g in order])
    cum, cum_err = mc.cumulative_fraction(list(order), mass)
    if log:
        if floor is None:
            # The power of ten below the smallest species' typical level,
            # ignoring values at numerical zero (an aerosol between events
            # fits to ~1e-24, which would stretch the axis over decades).
            levels = [np.nanpercentile(f[f > NUMERICAL_ZERO], 5) for f in F.T
                      if np.any(f > NUMERICAL_ZERO)]
            smallest = min(levels) if levels else np.nan
            floor = 10.0 ** np.floor(np.log10(smallest)) if np.isfinite(smallest) \
                and smallest > 0 else 1e-6
        base = floor
    else:
        base = 0.0
    seg = t["segment"].to_numpy()
    lower = np.full(len(t), base)
    handles = []
    for k, g in enumerate(order):
        upper = cum[:, k]
        xb, lb, ub = _break_segments(x, seg, lower, upper)
        handles.append(ax.fill_between(xb, lb, ub, color=colors[g], alpha=alpha, lw=0,
                                       label=pretty(g)))
        if k < len(order) - 1:                 # the top edge is 1 by construction
            if errors and np.isfinite(cum_err[:, k]).any():
                _, eb = _break_segments(x, seg, cum_err[:, k])
                ax.fill_between(xb, np.maximum(ub - eb, base if log else 0.0), ub + eb,
                                color=_darken(colors[g], STRIP_DARKEN), alpha=STRIP_ALPHA,
                                lw=0, zorder=2)
            ax.plot(xb, ub, color="white", lw=EDGE_LW, zorder=3)
        lower = upper
    if log:
        ax.set_yscale("log")
        ax.set_ylim(base, 1.0)
    else:
        ax.set_ylim(0.0, 1.0)
    ax.set_ylabel(f"{'Mass' if mass else 'Molar'} fraction (stacked)")
    if legend:
        ax.legend(handles[::-1], [h.get_label() for h in handles[::-1]], loc="center left",
                  bbox_to_anchor=(1.0, 0.5), frameon=False, fontsize=ANNOTATION_SIZE + 1)
    format_time_axis(ax, time, per_minute)
