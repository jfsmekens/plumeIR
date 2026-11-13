"""plumeIR.plotting — plot style, axis utilities and drawing functions.

Drawing functions take an ``Axes`` and draw into it; they never create
figures, call ``pyplot`` or show anything.  The client owns those.
"""

from plumeIR._style import apply_style, _add_top_axis
from plumeIR.plotting.spectrum import plot_spectrum

__all__ = ["apply_style", "_add_top_axis", "plot_spectrum"]
