"""
The command line's analysis canvas: every panel in one window, as v2.

Replicates the look of v2's ``makeAnalysisCanvas`` / ``updateAnalysisCanvas``
(``plumeIR_v2_archive/plotting.py``, lines 40 and 216) — three bands, redrawn
after every spectrum: the fits (one column per window, residual beneath), the
target time series, and one panel per ratio — but not their structure: the
numbers live in a :class:`~plumeIR.plotting.state.RunState`, the drawing in
the panels (``plotting/panels.py``), and this module only arranges them and
owns the figure.  A GUI arranges the same panels its own way.

This is the only plotting module that uses ``pyplot``, and only for the live
window (``interactive=True``).  A canvas that is only saved — snapshots,
final figures, a headless run — is a plain ``matplotlib.figure.Figure`` and
never touches a GUI backend, so it works on a server without a display.

Only the process that reads the run's stream draws, so a parallel run's
workers never import any of this.
"""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np
from matplotlib.figure import Figure

from plumeIR._style import ANNOTATION_SIZE, style_context
from plumeIR.config.objects import EmissionGeometry
from plumeIR.plotting.panels import (
    FitPanel, RatioPanel, TimeSeriesPanel, pretty,
)
from plumeIR.plotting.state import RunState

logger = logging.getLogger(__name__)

#: Resolution of the canvas snapshots written after each spectrum.
FRAME_DPI = 100


def window_titles(windows: dict) -> dict[str, str]:
    """
    Panel titles per window: ``'FIT0: SO2'``.

    Parameters
    ----------
    windows : dict
        ``FitWindowConfig`` per label (``RunSetup.windows``).
    """
    out = {}
    for label, w in windows.items():
        names = [pretty(t) for t in w.targets]
        out[label] = f"{label}: {', '.join(names)}" if names else label
    return out


def continuum_label(geometry) -> str:
    """Key entry for the dashed reference line of a geometry's fits."""
    if isinstance(geometry, EmissionGeometry):
        return "Clear sky (model)"
    return "Background polynomial"


def _ratio_key(state: RunState) -> dict[str, bool]:
    """Which optional entries the ratio key needs: intercept and ambient."""
    bgs = [state.ratio_background(pair) for pair in state.ratios]
    return {"intercept": any(not np.isfinite(x) for x, _ in bgs),
            "ambient": any(np.isfinite(x) or np.isfinite(y) for x, y in bgs)}


def _figsize(n_fits: int, n_ratios: int) -> tuple[float, float]:
    """
    Canvas size [in]: 16:10, widening with the number of columns.

    Sized to fit a standard screen (developer, 2026-11-10): at most
    15 × 9.4 in, i.e. 1500 × 940 px at 100 dpi, inside 1920 × 1080 and
    1440 × 900.  Replaces v2's 2:1 sizes (``makeAnalysisCanvas``, lines 87-92),
    which left the ratio band too short once the legends moved out of the plots.
    """
    columns = max(n_fits, n_ratios, 1)
    width = 10.0 if columns == 1 else 12.8 if columns == 2 else 15.0
    return width, width / 1.6


class AnalysisCanvas:
    """
    One figure with a fit panel per window, the time series and the ratios.

    Parameters
    ----------
    state : RunState
        Supplies the windows, targets and ratios to lay out.
    titles : dict or None
        Fit-panel title per window label (:func:`window_titles`).
    label : str
        Shown in the top-left corner (the geometry, as v2).
    interactive : bool
        Open a live window (pyplot).  ``False`` builds an off-screen figure
        for saving only.
    live : bool
        Panels in their live form (latest ``scroll`` regressed and
        highlighted) or their whole-run form.
    """

    def __init__(self, state: RunState, titles: dict | None = None,
                 label: str = "", interactive: bool = False,
                 live: bool = True) -> None:
        self.interactive = interactive
        self.closed = False
        windows = state.window_labels if state.has_spectra else list(state.latest_fits)
        titles = titles or {}
        self.live = live
        n_fits, n_ratios = len(windows), len(state.ratios)

        with style_context():
            if interactive:
                import matplotlib.pyplot as plt
                self.fig = plt.figure(figsize=_figsize(n_fits, n_ratios),
                                      layout="constrained")
                self.fig.canvas.mpl_connect("close_event", self._on_close)
            else:
                self.fig = Figure(figsize=_figsize(n_fits, n_ratios),
                                  layout="constrained")

            # Bands top to bottom: fits, time series, ratios (v2's 5:2:3, ratios
            # a little taller for their two-line titles).
            # A finished run loaded from its table has no fits.
            bands = ([5] if n_fits else []) + [2] + ([3.5] if n_ratios else [])
            gs = self.fig.add_gridspec(len(bands), 1, height_ratios=bands)
            row = iter(range(len(bands)))
            self.panels: list = []
            if n_fits:
                gs_fit = gs[next(row)].subgridspec(2, n_fits, height_ratios=[4, 1])
                for i, w in enumerate(windows):
                    ax = self.fig.add_subplot(gs_fit[0, i])
                    ax_res = self.fig.add_subplot(gs_fit[1, i], sharex=ax)
                    self.panels.append(FitPanel(ax, ax_res, w, titles.get(w)))
            self.panels.append(TimeSeriesPanel(self.fig.add_subplot(gs[next(row)]),
                                               state.targets, live=live))
            if n_ratios:
                gs_ratio = gs[next(row)].subgridspec(1, n_ratios)
                for i, pair in enumerate(state.ratios):
                    self.panels.append(RatioPanel(
                        self.fig.add_subplot(gs_ratio[0, i]), pair, live=live))

            # One key for every plot, below the figure (the developer's choice,
            # 2026-11-10): no legend inside a plot.
            handles, labels = [], []
            if n_fits:
                h, l = FitPanel.legend_handles(continuum_label(state.geometry))
                handles += h; labels += l
            if n_ratios:
                h, l = RatioPanel.legend_handles(**_ratio_key(state), live=live,
                                                 selected=state.can_select)
                handles += h; labels += l
            self._key = None
            if handles:
                self._key = self.fig.legend(handles, labels, loc="outside lower center",
                                            ncol=min(len(handles), 9), frameon=False,
                                            title_fontsize=ANNOTATION_SIZE + 2)
            self._title = self.fig.suptitle("Waiting for spectrum...",
                                            fontweight="bold")
            if label:
                self.fig.text(0.005, 0.995, label.upper(), ha="left", va="top",
                              fontsize=ANNOTATION_SIZE + 3, fontstyle="italic")
        if interactive:
            plt.show(block=False)

    def _on_close(self, _event) -> None:
        self.closed = True
        logger.info("Plot window closed; the run continues")

    def update(self, state: RunState) -> None:
        """Move every panel to ``state`` and redraw (the window, if live)."""
        with style_context():
            for panel in self.panels:
                panel.update(state)
            if self._key is not None and state.ratios:
                self._key.set_title(RatioPanel.scope(state, self.live))
            n = state.n_spectra or state.n_received
            if not state.n_received:
                text = "Waiting for spectrum..."
            elif state.selected is not None:
                text = f"Spectrum {state.selected + 1} of {n}: {state.latest_id}"
            elif self.live:
                text = f"Spectrum {state.n_received} of {n}: {state.latest_id}"
            else:
                text = f"{state.n_received} of {n} spectra"
            self._title.set_text(text)
            if self.interactive and not self.closed:
                self.fig.canvas.draw_idle()
                self.fig.canvas.flush_events()

    def save(self, path: Path, dpi: int | None = None) -> Path:
        """Write the canvas as it stands to ``path``."""
        with style_context():
            self.fig.savefig(path, dpi=dpi)
        return path

    def hold(self) -> None:
        """Keep a live window open until the user closes it (blocks)."""
        if self.interactive and not self.closed:
            import matplotlib.pyplot as plt
            plt.show()

    def close(self) -> None:
        """Release the figure."""
        if self.interactive:
            import matplotlib.pyplot as plt
            plt.close(self.fig)
        self.closed = True


def save_final_figures(state: RunState, out_dir: Path,
                       titles: dict | None = None, label: str = "") -> list[Path]:
    """
    Write a run's final figures: the whole-run canvas and one per panel.

    Every spectrum is shown and regressed (the panels' whole-run form).  Works
    the same for a live run's state and for a finished run loaded with
    :meth:`RunState.from_table`, which has no spectra, so no fit figures.

    Parameters
    ----------
    state : RunState
    out_dir : Path
        Created if missing.
    titles : dict or None
        Fit-panel titles (:func:`window_titles`).
    label : str
        Corner label of the canvas.

    Returns
    -------
    list of Path
        Files written: ``canvas.png``, ``fit_<window>.png``,
        ``timeseries.png``, ``ratio_<y>_<x>.png``.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    titles = titles or {}
    written: list[Path] = []

    canvas = AnalysisCanvas(state, titles, label=label, live=False)
    canvas.update(state)
    written.append(canvas.save(out_dir / "canvas.png"))

    with style_context():
        for w, fr in state.latest_fits.items():
            fig = Figure(figsize=(10.0, 4.2), layout="constrained")
            gs = fig.add_gridspec(2, 1, height_ratios=[4, 1])
            ax = fig.add_subplot(gs[0])
            panel = FitPanel(ax, fig.add_subplot(gs[1], sharex=ax), w,
                             titles.get(w))
            panel.update(state)
            fig.legend(*FitPanel.legend_handles(
                           continuum_label(state.geometry) if fr.continuum is not None
                           else None),
                       loc="outside lower center",
                       ncol=3, frameon=False)
            fig.suptitle(f"Last spectrum: {fr.spectrum_id}")
            written.append(out_dir / f"fit_{w}.png")
            fig.savefig(written[-1])

        fig = Figure(figsize=(10.0, 4.2), layout="constrained")
        TimeSeriesPanel(fig.add_subplot(), state.targets, live=False).update(state)
        written.append(out_dir / "timeseries.png")
        fig.savefig(written[-1])

        for pair in state.ratios:
            fig = Figure(figsize=(6.4, 4.4), layout="constrained")
            RatioPanel(fig.add_subplot(), pair, live=False).update(state)
            x_bg, y_bg = state.ratio_background(pair)
            fig.legend(*RatioPanel.legend_handles(
                           intercept=not np.isfinite(x_bg),
                           ambient=bool(np.isfinite(x_bg) or np.isfinite(y_bg)),
                           live=False),
                       loc="outside lower center", ncol=4, frameon=False,
                       title=RatioPanel.scope(state, live=False))
            written.append(out_dir / f"ratio_{pair[0]}_{pair[1]}.png")
            fig.savefig(written[-1])
    return written
