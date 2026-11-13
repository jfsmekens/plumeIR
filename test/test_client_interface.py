"""
Tests for what the library leaves to its clients .

- Logging: importing plumeIR attaches no console or file handler.
- Drawing: plot_spectrum draws into a given Axes and creates no figure.
- Defaults: the tables in plumeIR.standards cannot be changed at run time.

Run with:  pytest test/test_client_interface.py -v
"""

from __future__ import annotations

import logging
import subprocess
import sys
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from plumeIR import standards


# =============================================================================
# Logging is configured by the client
# =============================================================================

def test_import_attaches_no_handlers(tmp_path):
    """
    A fresh interpreter importing plumeIR gets only a NullHandler on the
    'plumeIR' logger, nothing on the root logger, and no ~/.plumeIR log file.
    """
    code = (
        "import logging, plumeIR, plumeIR.pipeline\n"
        "h = logging.getLogger('plumeIR').handlers\n"
        "print(len(h), all(isinstance(x, logging.NullHandler) for x in h),"
        " len(logging.getLogger().handlers))\n"
    )
    out = subprocess.run(
        [sys.executable, "-c", code], capture_output=True, text=True, check=True,
        cwd=Path(__file__).resolve().parents[1], env={"HOME": str(tmp_path),
                                                      "PATH": "/usr/bin:/bin"},
    )
    assert out.stdout.split() == ["1", "True", "0"]
    assert not (tmp_path / ".plumeIR" / "plumeIR.log").exists()
    assert "INFO" not in out.stderr and "DEBUG" not in out.stderr


def test_package_logger_level_left_to_client():
    """The library does not set a level that would override the client's."""
    assert logging.getLogger("plumeIR").level == logging.NOTSET


# =============================================================================
# No drawing inside the library
# =============================================================================

def test_spectrum_has_no_plot_method():
    from plumeIR.spectra import Spectrum
    assert not hasattr(Spectrum, "plot")


def test_plot_spectrum_draws_into_given_axes():
    mpl = pytest.importorskip("matplotlib")
    mpl.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.figure import Figure

    from plumeIR.plotting import plot_spectrum
    from plumeIR.spectra import Spectrum, SpectrumType

    wn = np.linspace(2400.0, 2550.0, 50)
    spec = Spectrum(x=wn, y=np.ones_like(wn), x_unit="wn",
                    timestamp=datetime(2021, 8, 11, 8, 0, 0),
                    filename=Path("plume_0001.spc"), spec_type=SpectrumType.RAD)

    fig = Figure()                       # not pyplot-managed
    ax = fig.add_subplot()
    n_before = len(plt.get_fignums())
    out = plot_spectrum(spec, ax, color="r")

    assert out is ax
    assert len(plt.get_fignums()) == n_before        # no figure created
    assert len(ax.lines) == 1
    np.testing.assert_array_equal(ax.lines[0].get_xdata(), wn)
    assert ax.get_xlim() == (wn.max(), wn.min())      # wavenumber descending
    assert ax.get_ylabel().startswith("Radiance")
    assert "plume_0001.spc" in ax.get_title()


# =============================================================================
# Read-only defaults
# =============================================================================

_TABLES = ["GAS_DEFAULTS", "AERO_DEFAULTS", "ILS_DEFAULTS", "TEMP_DEFAULTS"]
_NODES = ["ATM_TEMP_NODES", "PLUME_TEMP_NODES", "EMISSION_PLUME_TEMP_NODES",
          "H2O_VMR_NODES", "H2O_SC_NODES", "AERO_R0_NODES", "H2SO4_R0_NODES",
          "H2SO4_WT_NODES"]


@pytest.mark.parametrize("name", _TABLES)
def test_default_tables_are_read_only(name):
    table = getattr(standards, name)
    key = next(iter(table))
    with pytest.raises(TypeError):
        table[key] = {}
    with pytest.raises(TypeError):
        table[key][next(iter(table[key]))] = 0.0      # nested tables too
    with pytest.raises(AttributeError):
        table.update({})


@pytest.mark.parametrize("name", _NODES)
def test_node_arrays_are_read_only(name):
    nodes = getattr(standards, name)
    with pytest.raises(ValueError, match="read-only"):
        nodes[0] = 0.0


def test_freeze_copies_and_nests():
    src = {"a": {"x": 1.0}}
    frozen = standards.freeze(src)
    src["a"]["x"] = 2.0                  # later edits to the source do not show
    assert frozen["a"]["x"] == 1.0
    with pytest.raises(TypeError):
        frozen["a"]["x"] = 3.0
