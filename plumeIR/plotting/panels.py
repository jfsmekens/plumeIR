"""
Panels: one view of a run each, drawn into Axes they are given.

    FitPanel         measured and model spectrum of one window, residual beneath
    TimeSeriesPanel  target columns against time, each scaled to its range
    RatioPanel       one gas ratio: every point, the latest ``scroll``, the
                     newest, and the regression line with its slope

A panel creates its artists once, in the Axes passed to it, and
``update(state)`` moves them to a :class:`~plumeIR.plotting.state.RunState`.

No legend sits inside a plot (developer's choice, 2026-11-10: maximum
visibility).  Each panel's numbers — R², the ratio slope, intercept and
ambient column, each species' maximum — go into its titles, or for the time
series into a legend above its axes.  What the line styles and markers mean is
the same for every panel of a kind, so a panel only *offers* that key,
:meth:`legend_handles`, and the layout places one key per kind outside the
plots (``canvas.py``: one figure legend for the fits, one for the ratios).
It never creates a figure, never imports ``pyplot`` and never draws to screen:
the layout that owns the Axes does that (``plotting/canvas.py`` for the
command line; a GUI puts each panel in its own window).  The numbers all come
from the state, so every layout shows the same.

The look follows v2's analysis canvas
(``plumeIR_v2_archive/plotting.py::makeAnalysisCanvas``, line 40, and
``updateAnalysisCanvas``, line 216).  Create and update panels inside
``plumeIR._style.style_context()``.

``live=False`` gives a panel's whole-run form, used for the final figures and
for a finished run loaded with ``ResultsTable.from_csv``: every spectrum shown
and regressed, no scroll highlight.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

import numpy as np
import matplotlib.dates as mdates

from plumeIR._style import (
    ANNOTATION_SIZE, MARKERS, _add_top_axis, discrete_colors,
)
from plumeIR.constants import PLOT_COLORS, PRETTY_NAMES
from plumeIR.plotting.spectrum import _Y_LABELS
from plumeIR.plotting.state import RunState

if TYPE_CHECKING:
    from matplotlib.axes import Axes

logger = logging.getLogger(__name__)

#: Column units, by whether the quantity is a mass column.
_COLUMN_UNITS = {False: r"molec cm$^{-2}$", True: r"g m$^{-2}$"}
_WN_LABEL = r"Wavenumber [cm$^{-1}$]"
_REGRESSION_NAMES = {"siegel": "Siegel", "theil": "Theil-Sen", "odr": "ODR"}

_DARK2 = discrete_colors(4)
_C_MEASURED, _C_RESIDUAL, _C_LINE = "k", "0.35", "k"
# Fit colours follow v2 (constants.plot_colors): model red, background red
# dashed, so model and background differ by line style as well as colour.
_C_MODEL = "tab:red"
# Ratio panels keep v2's scatter colours (the developer's choice, 2026-11-10):
# blue points, red newest.
_C_RATIO, _C_RATIO_NEWEST = "tab:blue", "tab:red"
_C_BACKGROUND = "0.3"
_C_CONTINUUM = "tab:red"


def pretty(species: str) -> str:
    """Display name of a species (mathtext), or the name itself."""
    return PRETTY_NAMES.get(species, species)


def target_name(state: RunState, species: str) -> str:
    """
    Display name of a target column: :func:`pretty`, marked "(plume)" for a
    gas fitted in both roles (``state.dual``), whose bare name is the plume
    part only, its ambient part being ``{GAS}_atm``.
    """
    return pretty(species) + (" (plume)" if species in state.dual else "")


def _fmt(v: float, digits: int = 3) -> str:
    """Compact number for a legend; ``n/a`` for NaN."""
    return "n/a" if not np.isfinite(v) else f"{v:.{digits}g}"


# =============================================================================
# Fit
# =============================================================================

class FitPanel:
    """
    The latest fit of one window: measured and model spectra, residual beneath.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes for the spectra.
    ax_residual : matplotlib.axes.Axes
        Axes for the residual (beneath ``ax``, sharing its wavenumber range).
    window : str
        Window label, e.g. ``'FIT0'``.
    title : str or None
        Panel title; the window label if ``None``.
    """

    def __init__(self, ax: "Axes", ax_residual: "Axes", window: str,
                 title: str | None = None) -> None:
        self.ax, self.ax_residual, self.window = ax, ax_residual, window
        (self._measured,) = ax.plot([], [], color=_C_MEASURED, ls="-",
                                    lw=0.9, label="Measured")
        # Solid red (v2), wider than the measured line.
        (self._model,) = ax.plot([], [], color=_C_MODEL, ls="-",
                                 lw=1.2, label="Model")
        # v2's dashed red background line (bkg): the gas-free continuum, or
        # for emission the clear sky.
        (self._continuum,) = ax.plot([], [], color=_C_CONTINUUM, ls="--",
                                     lw=0.9, label="Background polynomial")
        (self._residual,) = ax_residual.plot([], [], color=_C_RESIDUAL, lw=0.6)
        ax_residual.axhline(0.0, color="k", lw=0.5, zorder=1)
        ax.set_title(title or window)
        # Goodness of fit inside the plot, anchored to the axes corner (axes
        # coordinates, not data), so it stays put whatever the data do.
        self._metrics = ax.text(0.99, 0.03, "", transform=ax.transAxes,
                                ha="right", va="bottom",
                                fontsize=ANNOTATION_SIZE + 1, zorder=6,
                                bbox=dict(boxstyle="round,pad=0.25", fc="white",
                                          ec="0.6", lw=0.5, alpha=0.85))
        ax.tick_params(labelbottom=False)
        ax_residual.set_xlabel(_WN_LABEL)
        ax.set_ylabel("Spectrum")
        ax_residual.set_ylabel("Residual")
        self._top_axis = False

    @staticmethod
    def legend_handles(continuum: str | None = "Background polynomial"
                       ) -> tuple[list, list[str]]:
        """
        The key of every fit panel: measured, model and the dashed reference
        line, named ``continuum`` (``None`` leaves it out).  It is the
        background polynomial for layer and solar, the clear-sky model for
        emission (``canvas.continuum_label``).
        """
        from matplotlib.lines import Line2D
        handles = [Line2D([], [], color=_C_MEASURED, lw=0.9),
                   Line2D([], [], color=_C_MODEL, lw=1.2)]
        labels = ["Measured", "Model"]
        if continuum:
            handles.append(Line2D([], [], color=_C_CONTINUUM, ls="--", lw=0.9))
            labels.append(continuum)
        return handles, labels

    def update(self, state: RunState) -> None:
        """Show the window's latest fit, if the state has one."""
        fr = state.latest_fits.get(self.window)
        if fr is None:
            return
        self._measured.set_data(fr.wn, fr.measured)
        self._model.set_data(fr.wn, fr.model)
        cont = getattr(fr, "continuum", None)
        if cont is None:
            self._continuum.set_data([], [])
        else:
            self._continuum.set_data(fr.wn, cont)
        self._residual.set_data(fr.wn, fr.residual)

        label = _Y_LABELS.get(state.spec_type, "Spectrum")
        self.ax.set_ylabel(label)

        # Wavenumber descending (wavelength ascending), the house convention.
        lo, hi = float(np.min(fr.wn)), float(np.max(fr.wn))
        for a in (self.ax, self.ax_residual):
            a.set_xlim(hi, lo)
            a.relim()
            a.autoscale_view(scalex=False)
        if not self._top_axis:
            _add_top_axis(self.ax)
            self._top_axis = True

        # R² only: RMSE is in the spectrum's units, which change from window
        # to window (it stays in results_long.csv).
        text = f"$R^2$ = {fr.r2:.4f}"
        self._metrics.set_text(text if fr.success else "fit FAILED\n" + text)


# =============================================================================
# Time series
# =============================================================================

class TimeSeriesPanel:
    """
    Target columns against time, each scaled to its own range.

    Columns of different species differ by orders of magnitude and in units,
    so each is scaled ``(y − min) / (max − min)`` over the spectra shown, 0
    at its minimum and 1 at its maximum; the legend, above the axes so it
    never covers the data, gives both.  Changed 2026-11-18 (developer) from
    v2's ``y / max``, which squashed a series with a large background (H2O,
    CO2: a few percent of variation near 1) against the top of the axes, so
    one could not see whether it moved with the others.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
    species : sequence of str or None
        Targets to show; all the state's targets if ``None`` (set on the first
        update).
    live : bool
        Show the latest ``state.scroll`` spectra (``True``) or all.
    """

    def __init__(self, ax: "Axes", species=None, live: bool = True) -> None:
        self.ax, self.live = ax, live
        self.species = list(species) if species is not None else None
        self._lines: dict = {}
        self._dates = False
        self._styled = False
        self._cursor = None     # the inspected spectrum (RunState.select)
        ax.set_ylabel("Column, scaled to its range")
        ax.set_xlabel("Time")
        if self.species is not None:
            self._make_lines()

    def _make_lines(self) -> None:
        # Each species keeps its own colour on every plot (constants.PLOT_COLORS,
        # chosen 2026-11-10); Dark2 by position only for a species without one.
        fallback = discrete_colors(len(self.species))
        for i, sp in enumerate(self.species):
            color = PLOT_COLORS.get(sp, fallback[i])
            (line,) = self.ax.plot(
                [], [], color=color, ls="-",
                marker=MARKERS[i % len(MARKERS)], ms=2.5, lw=0.9,
                label=pretty(sp))
            self._lines[sp] = line

    def update(self, state: RunState) -> None:
        """Redraw from the state."""
        if self.species is None:
            self.species = list(state.targets)
            self._make_lines()
        if not self._styled:
            # Aerosols dashed with open markers, gases solid and filled, so a
            # particle reads as one without its colour.
            for sp, line in self._lines.items():
                if state.is_aerosol(sp):
                    line.set_linestyle("--")
                    line.set_markerfacecolor("white")
            self._styled = True
        if state.n_received == 0:
            return
        last = state.scroll if self.live else None
        t = state.times(last)
        is_time = np.issubdtype(t.dtype, np.datetime64)
        for sp, line in self._lines.items():
            y, _ = state.column(sp, last)
            finite = np.isfinite(y)
            lo = np.nanmin(y) if finite.any() else np.nan
            hi = np.nanmax(y) if finite.any() else np.nan
            span = hi - lo
            # A constant series (or a single spectrum) has no range: drawn at
            # mid-height rather than dropped, so it stays visible.
            scaled = ((y - lo) / span if np.isfinite(span) and span > 0
                      else np.where(finite, 0.5, np.nan))
            line.set_data(t, scaled)
            unit = _COLUMN_UNITS[state.is_aerosol(sp)]
            line.set_label(f"{target_name(state, sp)}, {_fmt(lo)} to {_fmt(hi)} {unit}")
        if state.selected is not None and not self.live:
            sel = state.times()[state.selected]
            if self._cursor is None:
                # Red dashes above the series, as the selected point's red
                # circle on the ratio panels (developer, 2026-11-18; was a
                # black dotted line beneath the data, easy to miss).
                self._cursor = self.ax.axvline(sel, color=_C_RATIO_NEWEST, ls="--",
                                               lw=1.5, zorder=3)
                # A dot on each frame edge, styled as the ratio panels' selected
                # point.  Axes-fraction y (0 bottom, 1 top), so the dots stay on
                # the edges whatever the autoscaled limits, never enter
                # relim, and, unclipped, sit across the frame instead of being
                # cut in half.
                (self._cursor_ends,) = self.ax.plot(
                    [sel, sel], [0.0, 1.0], ls="", marker="o", ms=5,
                    mfc=_C_RATIO_NEWEST, mec="k", mew=0.5, zorder=4,
                    transform=self.ax.get_xaxis_transform(), clip_on=False)
            self._cursor.set_xdata([sel, sel])
            self._cursor_ends.set_xdata([sel, sel])
        if is_time and not self._dates:
            locator = mdates.AutoDateLocator()
            self.ax.xaxis.set_major_locator(locator)
            self.ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))
            self.ax.set_xlabel("Time")
            self._dates = True
        elif not is_time:
            self.ax.set_xlabel("Spectrum number")
        self.ax.relim()
        self.ax.autoscale_view()
        # A legend only for two or more series; one series is named in the title.
        n_series = len(self._lines)
        if n_series >= 2:
            self.ax.legend(loc="lower left", bbox_to_anchor=(0.0, 1.0),
                           ncol=min(n_series, 4), frameon=False,
                           borderaxespad=0.2, fontsize=ANNOTATION_SIZE + 1)
        elif self._lines:
            (line,) = self._lines.values()
            self.ax.set_title(line.get_label(), loc="left",
                              fontsize=ANNOTATION_SIZE + 1)


# =============================================================================
# Ratio
# =============================================================================

class RatioPanel:
    """
    One gas ratio: y column against x column, with its regression.

    Every spectrum is a blue circle with its 1σ fit errors on both axes, faint
    outside the latest ``state.scroll``; the newest is a red circle.  The
    regression (``state.regression``) over the latest ``scroll`` is the black
    dashed line, its slope in the legend; its intercept is a blue dashed
    horizontal line.  Where the ambient background column of a gas is known
    (``RunState.background_column``), a grey dotted line marks it: horizontal
    for y, vertical for x.  A grey square marks the predicted ambient point,
    where a spectrum with no plume would fall: the background of each gas, or
    zero for a gas without one (SO2, HCl).  The axes and the regression line
    reach it, so the line's extrapolation can be compared with it directly.
    The intercept line is drawn only when the x gas has no background: when it
    has one (e.g. CO against CO2), x = 0 lies far outside anything measurable
    and the intercept means nothing, while the line at the ambient x does.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
    pair : (str, str)
        ``(y, x)``, e.g. ``('HCl', 'SO2')``.
    live : bool
        ``False`` for the whole-run form: regression over every spectrum, all
        points at full strength, no newest highlight.  When inspecting
        (``state.selected`` set) the selected spectrum is the red circle.
    """

    #: Faintness of the points outside the scroll window.
    OUTSIDE_ALPHA = 0.25

    def __init__(self, ax: "Axes", pair: tuple[str, str], live: bool = True) -> None:
        self.ax, self.pair, self.live = ax, tuple(pair), live
        self._points: list = []          # error-bar containers, redrawn each update
        (self._newest,) = ax.plot([], [], ls="", marker="o", ms=5,
                                  mfc=_C_RATIO_NEWEST, mec="k", mew=0.5,
                                  zorder=4, label="Newest")
        (self._line,) = ax.plot([], [], ls="--", color=_C_LINE, lw=1.2, zorder=3,
                                label=f"{self._name()} = waiting...")
        self._intercept = ax.axhline(np.nan, ls="--", color=_C_RATIO, lw=0.9,
                                     zorder=3, label="Intercept")
        self._bg_y = ax.axhline(np.nan, ls=":", color=_C_BACKGROUND, lw=1.2,
                                zorder=3, label="Background")
        self._bg_x = ax.axvline(np.nan, ls=":", color=_C_BACKGROUND, lw=1.2,
                                zorder=3)
        (self._ambient,) = ax.plot([], [], ls="", marker="s", ms=6,
                                   mfc=_C_BACKGROUND, mec="k", mew=0.6,
                                   zorder=5, label="Ambient")
        if not live:
            self._newest.set_visible(False)
        self._labelled = False

    def _name(self) -> str:
        return f"{pretty(self.pair[0])}/{pretty(self.pair[1])}"

    @staticmethod
    def scope(state: RunState, live: bool = True) -> str:
        """The regression method and window, common to every ratio panel."""
        method = _REGRESSION_NAMES[state.regression]
        if live:
            n = min(state.n_received, state.scroll)
            return f"Ratios: {method}, latest {n} of {state.scroll} spectra"
        return f"Ratios: {method}, all {state.n_received} spectra"

    @staticmethod
    def legend_handles(intercept: bool = True, ambient: bool = True,
                       live: bool = True, selected: bool = False
                       ) -> tuple[list, list[str]]:
        """
        The key of every ratio panel.

        Parameters
        ----------
        intercept, ambient : bool
            Include the intercept line / predicted ambient point (when at least
            one panel shows it).
        live : bool
            Include the scroll window and newest point (live form).
        selected : bool
            Include the inspected spectrum's point (whole-run form while
            inspecting, ``RunState.select``).
        """
        from matplotlib.lines import Line2D
        handles = [Line2D([], [], ls="", marker="o", ms=3, color=_C_RATIO,
                          mec="none")]
        labels = ["Spectra in the regression" if live else "Spectra"]
        if live:
            handles += [Line2D([], [], ls="", marker="o", ms=3, color=_C_RATIO,
                               mec="none", alpha=RatioPanel.OUTSIDE_ALPHA),
                        Line2D([], [], ls="", marker="o", ms=5,
                               mfc=_C_RATIO_NEWEST, mec="k", mew=0.5)]
            labels += ["Older spectra", "Newest"]
        elif selected:
            handles.append(Line2D([], [], ls="", marker="o", ms=5,
                                  mfc=_C_RATIO_NEWEST, mec="k", mew=0.5))
            labels.append("Selected spectrum")
        handles.append(Line2D([], [], ls="--", color=_C_LINE, lw=1.2))
        labels.append("Regression")
        if intercept:
            handles.append(Line2D([], [], ls="--", color=_C_RATIO, lw=0.9))
            labels.append("Intercept")
        if ambient:
            handles.append(Line2D([], [], ls=":", marker="s", ms=5,
                                  color=_C_BACKGROUND, mfc=_C_BACKGROUND,
                                  mec="k", mew=0.6, lw=1.2))
            labels.append("Ambient (predicted)")
        return handles, labels

    def _draw_points(self, x, y, sx, sy, alpha: float) -> None:
        if not np.any(np.isfinite(x) & np.isfinite(y)):
            return
        self._points.append(self.ax.errorbar(
            x, y, xerr=sx, yerr=sy, fmt="o", ms=2.5, color=_C_RATIO,
            mec="none", ecolor=_C_RATIO, elinewidth=0.5, capsize=0,
            alpha=alpha, zorder=2))

    def update(self, state: RunState) -> None:
        """Redraw from the state."""
        if not self._labelled:
            unit = _COLUMN_UNITS[state.is_mass_ratio(self.pair)]
            self.ax.set_xlabel(f"{target_name(state, self.pair[1])} [{unit}]")
            self.ax.set_ylabel(f"{target_name(state, self.pair[0])} [{unit}]")
            self._labelled = True

        for container in self._points:
            container.remove()
        self._points = []
        x_all, y_all, sx_all, sy_all = state.ratio_points(self.pair)
        last = state.scroll if self.live else None
        if self.live:
            # Older than the scroll window: faint.  In the window: full.
            n_old = max(len(x_all) - state.scroll, 0)
            self._draw_points(x_all[:n_old], y_all[:n_old], sx_all[:n_old],
                              sy_all[:n_old], self.OUTSIDE_ALPHA)
            self._draw_points(x_all[n_old:], y_all[n_old:], sx_all[n_old:],
                              sy_all[n_old:], 1.0)
            self._newest.set_data(*([v] for v in state.newest_point(self.pair)))
        else:
            self._draw_points(x_all, y_all, sx_all, sy_all, 1.0)
            if state.selected is not None:
                self._newest.set_data(*([v] for v in state.newest_point(self.pair)))
                self._newest.set_visible(True)

        fit = state.ratio_fit(self.pair, last)
        if fit is None:
            self._line.set_data([], [])
            self._intercept.set_ydata([np.nan, np.nan])
            headline = f"{self._name()} = waiting..."
        else:
            xs = np.array([np.nanmin(x_all), np.nanmax(x_all)])
            self._line.set_data(xs, fit.slope * xs + fit.intercept)
            err = "" if not np.isfinite(fit.slope_err) else f" ± {_fmt(fit.slope_err, 2)}"
            headline = f"{self._name()} = {_fmt(fit.slope, 4)}{err}"
            self._intercept.set_ydata([fit.intercept, fit.intercept])

        x_bg, y_bg = state.ratio_background(self.pair)
        show_intercept = fit is not None and not np.isfinite(x_bg)
        if not show_intercept:
            self._intercept.set_ydata([np.nan, np.nan])
        self._bg_y.set_ydata([y_bg, y_bg])
        self._bg_x.set_xdata([x_bg, x_bg])
        has_bg = bool(np.isfinite(y_bg) or np.isfinite(x_bg))
        if has_bg:
            # No-plume point: each gas at its background, or zero without one.
            x_amb = x_bg if np.isfinite(x_bg) else 0.0
            y_amb = y_bg if np.isfinite(y_bg) else 0.0
            self._ambient.set_data([x_amb], [y_amb])
            if fit is not None:
                xs = np.array([min(np.nanmin(x_all), x_amb),
                               max(np.nanmax(x_all), x_amb)])
                self._line.set_data(xs, fit.slope * xs + fit.intercept)
        else:
            self._ambient.set_data([], [])
        # One title line each side; the method and window are the same for
        # every ratio, so the layout states them once (RatioPanel.scope).
        details = []
        if show_intercept:
            details.append(f"intercept {_fmt(fit.intercept)}")
        if has_bg:
            details.append(f"ambient {_fmt(y_amb)}" if x_amb == 0.0
                           else f"ambient ({_fmt(x_amb)}, {_fmt(y_amb)})")
        self.ax.set_title(headline, loc="left", fontsize=ANNOTATION_SIZE + 2)
        self.ax.set_title(", ".join(details), loc="right",
                          fontsize=ANNOTATION_SIZE + 1)
        self.ax.relim()
        self.ax.autoscale_view()
