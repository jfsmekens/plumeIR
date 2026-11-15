"""
Path length from a well-mixed ambient tracer (layer geometry): a diagnostic.

In the layer geometry ``pathlength`` (source to instrument) is often an
estimate: a vent position in a new lava field, a source whose distance is not
surveyed.  A gas that is well mixed at a known background and absent from the
plume measures it: its fitted ambient column ``N`` gives

    L_implied = N / (VMR_bg · n_air(T, P))

with ``VMR_bg`` the gas's reference concentration (``*DEFAULTS
<GAS>_conc``, the same value that sets its reference column) and ``n_air``
the air number density at the window's ambient temperature and pressure.

**It changes no fit.**  A 1-D gas table built at a fixed reference VMR has the
same optical depth per molecule at any path (to RFM's output precision,
~1e-5), so the plume columns do not depend on ``pathlength`` (measured, layer
tables rebuilt at 2.0 km instead of 1.22 km: every target's median ×1.00000,
R² unchanged; the round-off tips a few fits that sit in flat minima, in the
six-window example ≤ 2.5% of the median column for SO2 / HCl / CO / CO2 and
more for the poorly determined H2SO4 and HF).  What the path
does set is the reading of *ambient* columns as mixing ratios: background
VMRs of the fitted ambient gases, and of a ratio's intercept, the column of
``y`` at zero ``x``.  This module reports both at the configured and at the
implied path, and warns when they disagree by more than
:data:`PATH_WARN_FRACTION`.

Choice of tracer (``GEOMETRY.path_tracer``, ``config.keys.PATH_TRACERS``).
Measured on the layer validation set (MIDAC, 418 spectra, 2026-11-15):

- **CH4**, ν3 P-branch 2880-2990 cm⁻¹: 2.00 km, fit error 2.0% per spectrum,
  little cross-talk with the plume (r −0.32 with SO2).  ±10 K in ``atm_temp``
  moves it ±1.5%; the background VMR enters inversely (1.95 against 1.9 ppmv
  −2.6%).
- **N2O**, 2ν1 band 2540-2600 cm⁻¹: 2.01 km, fit error 5.8%, but it follows
  the plume (r +0.64 with SO2): a check on the series median, not per spectrum.
  The strong ν3 P-branch (2175-2225 cm⁻¹) is not usable (r +0.86, plume CO in
  the same lines).

The tracer must be the ambient background only, never a plume gas or a
target (``config.validator._check_path_tracer``).
"""

from __future__ import annotations

import logging
from dataclasses import dataclass

import numpy as np
import pandas as pd

from plumeIR.retrieval.results import (
    COL_KIND, COL_PARAM, COL_PRES, COL_SCD, COL_SCD_ERR, COL_SPECIES,
    COL_SPECTRUM, COL_SUCCESS, COL_TEMP, COL_WINDOW, ResultsTable,
)
from plumeIR.units import scd_to_vmr

logger = logging.getLogger(__name__)

#: Relative difference between the implied and the configured path above which
#: a warning is logged.  The CH4 path is precise to ~2% per spectrum and ~3%
#: in its inputs (temperature, background VMR), so 20% is far outside them.
PATH_WARN_FRACTION = 0.2

#: Column of the per-spectrum implied path [km] in the wide results table.
COL_TRACER_PATH = "tracer_path_km"

#: Kind labels in the results table.
_ATM = "GAS_ATM"
_PLUME = "GAS_PLUME"

#: Fewest successful fits a window needs to be reported.
_MIN_FITS = 3


@dataclass(frozen=True)
class WindowPath:
    """
    The implied path from one window.

    Attributes
    ----------
    window : str
        Window label.
    path_km : pandas.Series
        Implied path [km] per spectrum (successful fits), indexed by spectrum id.
    rel_error : float
        Median relative fit error of the tracer column.
    crosstalk_gas : str or None
        The plume gas of the window whose column correlates most with the
        tracer's, ``None`` when the window has none.
    crosstalk_r : float
        That correlation (Pearson r); NaN without a plume gas.
    """
    window: str
    path_km: pd.Series
    rel_error: float
    crosstalk_gas: str | None
    crosstalk_r: float

    @property
    def median(self) -> float:
        return float(self.path_km.median())

    def quantile(self, q: float) -> float:
        return float(self.path_km.quantile(q))


@dataclass(frozen=True)
class AmbientVMR:
    """Median ambient VMR of one fitted gas, at the configured and implied path."""
    window: str
    gas: str
    configured_ppmv: float
    implied_ppmv: float


@dataclass(frozen=True)
class PathReport:
    """
    The path diagnostic for a run.

    Attributes
    ----------
    tracer : str
        The tracer gas.
    background_ppmv : float
        Its background VMR (the reference concentration).
    configured_km : float
        ``GEOMETRY.pathlength``.
    windows : dict of str → WindowPath
        Every active window where the tracer is an ambient gas.
    best : str or None
        The window with the smallest median relative fit error; ``None`` when
        no window has :data:`_MIN_FITS` successful fits.
    ambient : list of AmbientVMR
        Every fitted ambient gas of every window.
    atm_temp, atm_pres : float
        The ambient state used for intercept conversions [K, mbar].
    """
    tracer: str
    background_ppmv: float
    configured_km: float
    windows: dict[str, WindowPath]
    best: str | None
    ambient: list[AmbientVMR]
    atm_temp: float
    atm_pres: float

    @property
    def path_km(self) -> float:
        """The implied path [km]: the best window's median; NaN without one."""
        return self.windows[self.best].median if self.best else float("nan")

    @property
    def ratio(self) -> float:
        """Implied over configured path."""
        return self.path_km / self.configured_km

    def per_spectrum(self) -> pd.Series:
        """The best window's per-spectrum path [km], named :data:`COL_TRACER_PATH`."""
        if self.best is None:
            return pd.Series(dtype=float, name=COL_TRACER_PATH)
        return self.windows[self.best].path_km.rename(COL_TRACER_PATH)

    def background_vmr(self, column: float) -> tuple[float, float]:
        """
        A column along the path (a ratio intercept) as a background VMR.

        Parameters
        ----------
        column : float
            Column [molec/cm²].

        Returns
        -------
        tuple of float
            VMR [ppmv] at the configured and at the implied path.
        """
        def at(path_km: float) -> float:
            return float(scd_to_vmr(column, path_km, temp=self.atm_temp,
                                    pres=self.atm_pres))

        implied = at(self.path_km) if np.isfinite(self.path_km) else float("nan")
        return at(self.configured_km), implied


def _vmr_1km(rows: pd.DataFrame) -> pd.Series:
    """VMR [ppmv] each row's column would have over a 1 km path."""
    return pd.Series(scd_to_vmr(rows[COL_SCD].to_numpy(float), 1.0,
                                temp=rows[COL_TEMP].to_numpy(float),
                                pres=rows[COL_PRES].to_numpy(float)),
                     index=rows.index)


def tracer_report(table: ResultsTable, windows: dict, params: dict,
                  geometry) -> PathReport | None:
    """
    The path diagnostic of a finished layer run.

    Parameters
    ----------
    table : ResultsTable
        The run's results.
    windows : dict
        ``FitWindowConfig`` per active window label.
    params : dict
        ``ParameterSet`` per window label (for the tracer's reference column).
    geometry : LayerGeometry
        The run geometry; ``path_tracer`` selects the tracer.

    Returns
    -------
    PathReport or None
        ``None`` when ``path_tracer`` is unset (or the geometry has none).
    """
    tracer = getattr(geometry, "path_tracer", None)
    if tracer is None:
        return None
    L0, T0, P0 = geometry.pathlength, geometry.atm_temp, geometry.atm_pres
    df = table.df
    if len(df):
        df = df[df[COL_SUCCESS].astype(bool)]

    background = None
    per_window: dict[str, WindowPath] = {}
    for label, window in windows.items():
        if tracer not in window.atm_gases:
            continue
        if background is None:
            ref = params[label][tracer].ref_column
            background = float(scd_to_vmr(ref, L0, temp=T0, pres=P0))
        rows = df[(df[COL_WINDOW] == label) & (df[COL_PARAM] == tracer)]
        rows = rows.set_index(COL_SPECTRUM)
        if len(rows) < _MIN_FITS:
            continue
        path = _vmr_1km(rows) / background
        rel = (rows[COL_SCD_ERR] / rows[COL_SCD]).abs()
        gas, r = None, float("nan")
        plume = df[(df[COL_WINDOW] == label) & (df[COL_KIND] == _PLUME)]
        for g, grp in plume.groupby(COL_PARAM):
            col = grp.set_index(COL_SPECTRUM)[COL_SCD].reindex(rows.index)
            ok = np.isfinite(col.to_numpy(float)) & np.isfinite(rows[COL_SCD].to_numpy(float))
            # ptp, not std: std of identical floats is not exactly zero.
            if (ok.sum() < _MIN_FITS or np.ptp(col[ok]) == 0
                    or np.ptp(rows[COL_SCD][ok]) == 0):
                continue
            rg = float(np.corrcoef(rows[COL_SCD][ok], col[ok])[0, 1])
            if gas is None or abs(rg) > abs(r):
                gas, r = g, rg
        per_window[label] = WindowPath(label, path.rename(COL_TRACER_PATH),
                                       float(np.nanmedian(rel)), gas, r)

    best = (min(per_window, key=lambda k: per_window[k].rel_error)
            if per_window else None)
    implied = per_window[best].median if best else float("nan")

    ambient = []
    atm = df[df[COL_KIND] == _ATM] if len(df) else df
    for (label, gas), grp in atm.groupby([COL_WINDOW, COL_PARAM], sort=False):
        v1 = _vmr_1km(grp)
        ambient.append(AmbientVMR(label, gas, float(v1.median() / L0),
                                  float(v1.median() / implied)
                                  if np.isfinite(implied) else float("nan")))

    report = PathReport(tracer, background if background is not None else float("nan"),
                        L0, per_window, best, ambient, T0, P0)
    if best is not None and abs(report.ratio - 1.0) > PATH_WARN_FRACTION:
        logger.warning(
            "Path check: %s in %s implies a %.3f km path, %.2f× the configured "
            "pathlength %.3g km (at %.3g ppmv %s). The fitted plume columns do "
            "not depend on it; ambient VMRs and ratio intercepts do (summary).",
            tracer, best, implied, report.ratio, L0, report.background_ppmv,
            tracer)
    return report
