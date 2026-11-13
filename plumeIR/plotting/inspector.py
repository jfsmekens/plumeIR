"""
The post-fit inspector: step through a finished run's fits, time series and
ratios without refitting.

The layout is the command line's analysis canvas (``plotting/canvas.py``) in
its whole-run form, driven by a :class:`~plumeIR.plotting.state.RunState`
reopened from a run folder (:meth:`RunState.from_run_dir`, which reads
``fits.npz``).  Moving to another spectrum is :meth:`RunState.select` followed
by a redraw, so a GUI drives the same state with its own controls; this module
only adds the command line's keyboard and mouse bindings (decided with the
developer, 2026-11-12):

    ← / →            previous / next spectrum
    PgUp / PgDn      10 spectra back / forward
    Home / End       first / last spectrum
    click            on the time series: the spectrum nearest in time;
                     on a ratio panel: the nearest point (within a few pixels)

Every other key keeps Matplotlib's meaning (``s`` save, ``f`` full screen,
``q`` quit, ``o`` / ``p`` zoom / pan).  Clicks are ignored while zoom or pan
is active.  Like the canvas, the window is the only part that uses
``pyplot``, and only when ``interactive``; off-screen, the inspector is a plain
``Figure`` that tests and scripts step through and save.
"""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np

from plumeIR._style import ANNOTATION_SIZE, style_context
from plumeIR.config.objects import EmissionGeometry, SolarGeometry
from plumeIR.plotting.canvas import AnalysisCanvas, window_titles
from plumeIR.plotting.panels import RatioPanel, TimeSeriesPanel
from plumeIR.plotting.state import RunState

logger = logging.getLogger(__name__)

#: Keys that move the selection, and by how much.
STEPS = {"right": 1, "left": -1, "pagedown": 10, "pageup": -10}

#: A click on a ratio panel selects the nearest point within this distance [px].
PICK_RADIUS_PX = 15.0

_HELP = "←/→ spectrum   PgUp/PgDn ±10   Home/End   click a point"


def geometry_label(geometry) -> str:
    """Corner label of the canvas: the geometry's name, as the live canvas."""
    if isinstance(geometry, EmissionGeometry):
        return "emission"
    if isinstance(geometry, SolarGeometry):
        return "solar"
    return "layer"


class Inspector:
    """
    The analysis canvas of a finished run, one selected spectrum at a time.

    Parameters
    ----------
    state : RunState
        From :meth:`RunState.from_run_dir` (it must hold the stored fits).
    titles : dict or None
        Fit-panel title per window; from ``state.window_configs`` if ``None``.
    label : str or None
        Corner label; the geometry's name if ``None``.
    interactive : bool
        Open a window (pyplot) and bind keys and clicks; ``False`` builds an
        off-screen figure, stepped with :meth:`show` / :meth:`step`.

    Raises
    ------
    ValueError
        If the state holds no stored fits.
    """

    def __init__(self, state: RunState, titles: dict | None = None,
                 label: str | None = None, interactive: bool = True) -> None:
        if not state.can_select:
            raise ValueError("The inspector needs a RunState holding a run's "
                             "stored fits: use RunState.from_run_dir")
        self.state = state
        if titles is None:
            titles = window_titles(state.window_configs)
        if label is None:
            label = geometry_label(state.geometry)
        if state.selected is None:
            state.select(0)
        self.canvas = AnalysisCanvas(state, titles, label=label,
                                     interactive=interactive, live=False)
        fig = self.canvas.fig
        with style_context():
            fig.text(0.995, 0.995, _HELP, ha="right", va="top",
                     fontsize=ANNOTATION_SIZE, color="0.35")
        if interactive:
            # Matplotlib binds ←/→ and Home to view history; take those keys
            # and hand every other one back to its handler.
            manager = fig.canvas.manager
            handler_id = getattr(manager, "key_press_handler_id", None)
            if handler_id is not None:
                fig.canvas.mpl_disconnect(handler_id)
            fig.canvas.mpl_connect("key_press_event", self._on_key)
            fig.canvas.mpl_connect("button_press_event", self._on_click)
        self.canvas.update(state)

    @classmethod
    def from_run_dir(cls, run_dir: str | Path, interactive: bool = True
                     ) -> "Inspector":
        """Reopen a run folder and inspect it (:meth:`RunState.from_run_dir`)."""
        return cls(RunState.from_run_dir(run_dir), interactive=interactive)

    @property
    def n_spectra(self) -> int:
        return len(self.state.archive)

    # ------------------------------------------------------------------
    # Moving
    # ------------------------------------------------------------------

    def show(self, k: int) -> int:
        """Select the ``k``-th spectrum (input order, clipped) and redraw."""
        k = self.state.select(k)
        self.canvas.update(self.state)
        return k

    def step(self, delta: int) -> int:
        """Move the selection by ``delta`` spectra and redraw."""
        return self.show(self.state.selected + delta)

    def _on_key(self, event) -> None:
        key = event.key
        if key in STEPS:
            self.step(STEPS[key])
        elif key == "home":
            self.show(0)
        elif key == "end":
            self.show(self.n_spectra - 1)
        else:
            from matplotlib.backend_bases import key_press_handler
            key_press_handler(event, event.canvas, getattr(event.canvas, "toolbar", None))

    def spectrum_at(self, ax, x: float, y: float) -> int | None:
        """
        The spectrum a click at data coordinates ``(x, y)`` in ``ax`` points to.

        Returns
        -------
        int or None
            Input-order index, or ``None`` when ``ax`` is not a time series or
            ratio panel, or no ratio point lies within :data:`PICK_RADIUS_PX`.
        """
        for panel in self.canvas.panels:
            if panel.ax is not ax:
                continue
            if isinstance(panel, TimeSeriesPanel):
                times = self.state.times()
                if np.issubdtype(times.dtype, np.datetime64):
                    import matplotlib.dates as mdates
                    x = np.datetime64(mdates.num2date(x).replace(tzinfo=None), "ns")
                return self.state.nearest(x)
            if isinstance(panel, RatioPanel):
                px, py, _, _ = self.state.ratio_points(panel.pair)
                pts = ax.transData.transform(np.column_stack([px, py]))
                click = ax.transData.transform([[x, y]])[0]
                d = np.hypot(*(pts - click).T)
                if not np.any(np.isfinite(d)):
                    return None
                k = int(np.nanargmin(d))
                return k if d[k] <= PICK_RADIUS_PX else None
        return None

    def _on_click(self, event) -> None:
        if event.button != 1 or event.inaxes is None or event.xdata is None:
            return
        toolbar = getattr(event.canvas, "toolbar", None)
        if toolbar is not None and getattr(toolbar, "mode", ""):
            return                         # zooming or panning
        k = self.spectrum_at(event.inaxes, event.xdata, event.ydata)
        if k is not None:
            self.show(k)

    # ------------------------------------------------------------------
    # Window
    # ------------------------------------------------------------------

    def save(self, path: Path, dpi: int | None = None) -> Path:
        """Write the canvas as it stands."""
        return self.canvas.save(path, dpi=dpi)

    def hold(self) -> None:
        """Keep the window open until the user closes it (blocks)."""
        self.canvas.hold()

    def close(self) -> None:
        self.canvas.close()
