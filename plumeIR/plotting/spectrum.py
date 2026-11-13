"""
Draw a single measured spectrum into a given Axes.

Replaces the former ``Spectrum.plot`` method, which created its own figure and
called ``plt.show()``: the library draws, the client (CLI layout, GUI window,
script) owns figures, style and display.  Nothing here imports ``pyplot``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from plumeIR._style import _add_top_axis
from plumeIR.constants import UNITS
from plumeIR.spectra.spectrum import Spectrum, SpectrumType

if TYPE_CHECKING:
    from matplotlib.axes import Axes


#: Radiance per wavenumber, house style (``Quantity [unit]``, upright units).
_RAD_UNIT = r"[mW m$^{-2}$ sr$^{-1}$ (cm$^{-1}$)$^{-1}$]"

_Y_LABELS: dict[SpectrumType, str] = {
    SpectrumType.SBM:  "Single beam [a.u.]",
    SpectrumType.IFG:  "Interferogram [a.u.]",
    SpectrumType.BBT:  "Brightness temperature [K]",
    SpectrumType.RAD:  f"Radiance\n{_RAD_UNIT}",
    SpectrumType.TRA:  "Transmittance",
    SpectrumType.ABS:  "Absorbance",
    SpectrumType.DIFF: f"Radiance difference\n{_RAD_UNIT}",
}


def plot_spectrum(
    spectrum: Spectrum,
    ax: "Axes",
    x_unit: str | None = None,
    title: str | None = None,
    **kwargs,
) -> "Axes":
    """
    Draw a spectrum into an existing Axes.

    Parameters
    ----------
    spectrum : Spectrum
        The spectrum to draw.
    ax : matplotlib.axes.Axes
        Axes to draw into.  Required: this function never creates a figure.
    x_unit : str or None
        ``'wn'`` or ``'wl'``.  If ``None``, uses the spectrum's native
        ``x_unit``.
    title : str or None
        Axes title.  If ``None``, uses the filename and timestamp.
    **kwargs
        Passed to ``ax.plot()`` (default ``color='k'``, ``lw=0.8``).

    Returns
    -------
    matplotlib.axes.Axes
        The same ``ax``, for chaining.
    """
    target_unit = x_unit or spectrum.x_unit
    if target_unit != spectrum.x_unit and {target_unit, spectrum.x_unit} == {"wn", "wl"}:
        x_disp = 1e4 / spectrum.x
    else:
        x_disp = spectrum.x

    kwargs.setdefault("color", "k")
    kwargs.setdefault("lw", 0.8)
    ax.plot(x_disp, spectrum.y, **kwargs)
    ax.set_xlabel(UNITS.get(target_unit, target_unit))
    ax.set_ylabel(_Y_LABELS.get(spectrum.spec_type, "Intensity"))

    if target_unit == "wn":
        # Descending wavenumber (wavelength ascending), converse axis on top.
        ax.set_xlim(x_disp.max(), x_disp.min())
        _add_top_axis(ax, units="nm" if x_disp.max() > 10000 else "um")

    if title is None:
        ts = spectrum.timestamp
        ts_str = ts.strftime("%Y-%m-%d %H:%M:%S") if ts else "no timestamp"
        bb_str = (f"  [BB {spectrum.bb_temp:.0f} K]"
                  if spectrum.is_blackbody and spectrum.bb_temp else "")
        name = spectrum.filename.name if spectrum.filename is not None else "spectrum"
        title = f"{name}  —  {ts_str}{bb_str}"
    ax.set_title(title, fontsize=9)
    return ax
