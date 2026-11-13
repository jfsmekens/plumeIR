"""
Shared plot style, colours and axis utilities for plumeIR v3.

DEPENDENCY RULE: This module imports only numpy and matplotlib.
It must never import from any other plumeIR module.  plumeIR.plotting
imports from here; keeping this a strict leaf leaves the plotting code
free to import plumeIR.spectra without a cycle.  It does not import
``pyplot`` either: drawing code takes the Axes it is given, and only a
client layout (``plumeIR.plotting.canvas``) touches pyplot.

The style is one dict, :data:`STYLE`, applied with :func:`style_context`
(``matplotlib.rc_context``) around figure creation and drawing.
:func:`apply_style` sets it globally, for scripts.

Ported from mieEODG/mieEODG/plotting.py with minor adaptations.
Original: mieEODG/mieEODG/plotting.py::_add_top_axis, lines 98–151.
"""

from __future__ import annotations

from contextlib import AbstractContextManager

import numpy as np
import matplotlib as mpl
from matplotlib import colors as mcolors
from matplotlib.axes import Axes

# ---------------------------------------------------------------------------
# rcParams — consistent figure style across all plumeIR plots
# Ported from mieEODG/mieEODG/plotting.py, lines 40–57; extended 2026-11-10
# to the house style (ticks on four sides, legend size, Dark2 cycle).
# ---------------------------------------------------------------------------

STYLE: dict[str, object] = {
    "font.size":                     11,
    "axes.labelsize":                9,
    "axes.titlesize":                12,
    "figure.titlesize":              12,
    "legend.fontsize":               8,
    "legend.title_fontsize":         8,
    "xtick.labelsize":               9,
    "xtick.direction":               "in",
    "xtick.top":                     True,
    "ytick.labelsize":               9,
    "ytick.direction":               "in",
    "ytick.right":                   True,
    "lines.linewidth":               0.9,
    "axes.grid":                     True,
    "grid.linewidth":                0.5,
    "grid.alpha":                    0.5,
    "grid.linestyle":                "--",
    "axes.axisbelow":                True,
    "axes.prop_cycle":               mpl.cycler(color=mpl.colormaps["Dark2"].colors),
    "figure.constrained_layout.use": True,
    "axes.formatter.limits":         (-4, 4),
    "errorbar.capsize":              2,
    "savefig.dpi":                   150,
    "savefig.bbox":                  "tight",
}

#: Former name of :data:`STYLE`.
_RCPARAMS = STYLE

#: Font size for annotations (house style).
ANNOTATION_SIZE = 7


def style_context() -> AbstractContextManager:
    """
    Context manager applying :data:`STYLE` (``matplotlib.rc_context``).

    Wrap figure creation and every redraw in it; global rcParams are left
    untouched.
    """
    return mpl.rc_context(STYLE)


def apply_style() -> None:
    """
    Apply the plumeIR matplotlib style globally.

    For scripts; library and client code use :func:`style_context` instead.
    Safe to call multiple times.
    """
    mpl.rcParams.update(STYLE)


# ---------------------------------------------------------------------------
# Colours
# ---------------------------------------------------------------------------

#: Markers paired with the discrete colours, so a series is identifiable
#: without colour (greyscale print, colour-vision deficiency).
MARKERS: tuple[str, ...] = ("o", "s", "^", "D", "v", "P", "X", "*")
LINESTYLES: tuple[str, ...] = ("-", "--", "-.", ":")


def discrete_colors(n: int) -> list[tuple[float, float, float]]:
    """
    ``n`` colours for discrete series: Dark2 (8), then tab20 beyond 8.

    Parameters
    ----------
    n : int
        Number of series.

    Returns
    -------
    list of RGB tuples
    """
    cmap = mpl.colormaps["Dark2"] if n <= 8 else mpl.colormaps["tab20"]
    base = list(cmap.colors)
    return [base[i % len(base)] for i in range(n)]


def lighten(color, amount: float = 0.6) -> tuple[float, float, float]:
    """
    A lighter version of ``color``, for background or reference data.

    Parameters
    ----------
    color
        Any Matplotlib colour.
    amount : float
        0 returns the colour, 1 returns white.

    Returns
    -------
    tuple of float
        RGB.
    """
    rgb = np.asarray(mcolors.to_rgb(color))
    return tuple(float(v) for v in rgb + (1.0 - rgb) * amount)


# ---------------------------------------------------------------------------
# Wavelength tick candidates
# Ported from mieEODG/mieEODG/plotting.py, lines 63–75
# ---------------------------------------------------------------------------

_WL_TICKS_UM: list[float] = [
    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
    1.25, 1.5, 2.0, 2.5,
    3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20,
    25, 30, 50, 75, 100, 150, 200,
]
_WL_TICKS_NM: list[float] = [
    400, 450, 500, 550, 600, 650, 700,
    750, 800, 900, 1000, 1200,
    1500, 2000, 2500, 3000, 5000,
]
_WL_TICKS_MAX: int = 12


# ---------------------------------------------------------------------------
# Conversion helpers (self-inverse for secondary_xaxis)
# Ported from mieEODG/mieEODG/plotting.py, lines 82–91
# ---------------------------------------------------------------------------

def _fw(x):
    """Wavenumber (cm⁻¹) ↔ wavelength (µm).  Self-inverse."""
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(np.asarray(x) != 0, 1.0e4 / np.asarray(x), np.nan)


def _fn(x):
    """Wavenumber (cm⁻¹) ↔ wavelength (nm).  Self-inverse."""
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(np.asarray(x) != 0, 1.0e7 / np.asarray(x), np.nan)


# ---------------------------------------------------------------------------
# Secondary top-axis
# Ported from mieEODG/mieEODG/plotting.py::_add_top_axis, lines 98–151
# ---------------------------------------------------------------------------

def _add_top_axis(
    ax: Axes,
    units: str = "um",
) -> Axes | None:
    """
    Attach a secondary x-axis on top showing wavelength.

    The primary axis is assumed to show wavenumber [cm⁻¹].  Tick
    positions are chosen from a pre-defined candidate list and thinned
    if too many would be shown.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Primary (wavenumber) axes.
    units : {'um', 'nm'}
        Units for the secondary axis.  Default ``'um'`` (µm).

    Returns
    -------
    matplotlib.axes.Axes or None
        The secondary axes, or ``None`` if primary limits are not
        strictly positive.
    """
    lo, hi = ax.get_xlim()
    wn_lo, wn_hi = min(lo, hi), max(lo, hi)
    if not (wn_lo > 0 and wn_hi > 0):
        return None

    if units == "nm":
        conv        = _fn
        ticks_cand  = _WL_TICKS_NM
        xlabel      = "Wavelength [nm]"
    else:
        conv        = _fw
        ticks_cand  = _WL_TICKS_UM
        xlabel      = "Wavelength [µm]"

    secax = ax.secondary_xaxis("top", functions=(conv, conv))

    wl_lo, wl_hi = sorted([conv(wn_lo), conv(wn_hi)])
    visible = [t for t in ticks_cand if wl_lo <= t <= wl_hi]
    if len(visible) > _WL_TICKS_MAX:
        step    = (len(visible) + _WL_TICKS_MAX - 1) // _WL_TICKS_MAX
        visible = visible[::step]
    if visible:
        secax.xaxis.set_ticks(visible)

    secax.set_xlabel(xlabel, fontsize=9)
    return secax
