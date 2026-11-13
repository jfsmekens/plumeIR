"""
H2SO4 aerosol acidity lookup.

Provides an estimate of the H2SO4 weight fraction in H2SO4-H2O aerosols
from ambient temperature and water vapour partial pressure.

Based on Steele & Hamill (1981) and Russell & Hamill (1984), as used in
plumeIR v2 (plumeIR_v2/acidity.py).

Note
----
This model was derived for stratospheric conditions. Its applicability
to volcanic plumes is uncertain — plume acidity is better set directly
in ``*DEFAULTS`` rather than derived from this model.  The function is
retained as a convenience for background atmospheric H2SO4 aerosols.
"""

from __future__ import annotations

import logging

import numpy as np
from scipy.interpolate import RegularGridInterpolator

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Lookup table from Steele & Hamill (1981) / Russell & Hamill (1984)
# Rows: water vapor pressure [mbar],  Columns: temperature [K]
# Values: H2SO4 weight fraction (0–1)
# ---------------------------------------------------------------------------

_WATER_PRES_MBAR = np.array([
    1e-5, 2e-5, 5e-5, 1e-4, 2e-4, 5e-4,
    1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2,
    1e-1, 2e-1, 5e-1,
])

_TEMP_K = np.array([
    185, 190, 195, 200, 205, 210, 215, 220,
    225, 230, 235, 240, 245, 250, 255, 260,
    265, 270, 275, 280, 285, 290, 295, 300,
])

# Weight fraction table: shape (n_pres, n_temp)
# Source: plumeIR_v2/acidity.py (original transcription)
_WF_TABLE = np.array([
    [0.78, 0.77, 0.76, 0.75, 0.74, 0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55],
    [0.77, 0.76, 0.75, 0.74, 0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54],
    [0.76, 0.75, 0.74, 0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53],
    [0.75, 0.74, 0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52],
    [0.74, 0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51],
    [0.73, 0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50],
    [0.72, 0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49],
    [0.71, 0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48],
    [0.70, 0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47],
    [0.69, 0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46],
    [0.68, 0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45],
    [0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45, 0.44],
    [0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45, 0.44, 0.43],
    [0.65, 0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45, 0.44, 0.43, 0.42],
    [0.64, 0.63, 0.62, 0.61, 0.60, 0.59, 0.58, 0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45, 0.44, 0.43, 0.42, 0.41],
])

# Build interpolator once at module import
_INTERP = RegularGridInterpolator(
    (np.log10(_WATER_PRES_MBAR), _TEMP_K),
    _WF_TABLE,
    method="linear",
    bounds_error=False,
    fill_value=None,   # extrapolate at edges
)


def h2so4_acidity(water_pres_mbar: float, temp_k: float) -> float:
    """
    Estimate H2SO4 weight fraction from ambient water vapour pressure and
    temperature using the Steele & Hamill (1981) lookup table.

    Parameters
    ----------
    water_pres_mbar : float
        Water vapour partial pressure [mbar].
    temp_k : float
        Ambient temperature [K].

    Returns
    -------
    float
        H2SO4 weight fraction (0–1).

    Notes
    -----
    Valid range: 185–300 K, 1×10⁻⁵ to 5×10⁻¹ mbar H2O.  Outside this
    range the interpolator extrapolates linearly, which may be unreliable.
    For volcanic plume conditions the acidity model is physically uncertain;
    prefer setting ``H2SO4_comp`` directly in ``*DEFAULTS``.
    """
    log_pres = np.log10(float(water_pres_mbar))
    wf = float(_INTERP([[log_pres, float(temp_k)]])[0])
    wf = float(np.clip(wf, 0.05, 0.98))

    if temp_k < _TEMP_K[0] or temp_k > _TEMP_K[-1]:
        logger.warning(
            "h2so4_acidity: temperature %.1f K outside lookup range "
            "[%.0f, %.0f] K — extrapolating", temp_k, _TEMP_K[0], _TEMP_K[-1]
        )
    if water_pres_mbar < _WATER_PRES_MBAR[0] or water_pres_mbar > _WATER_PRES_MBAR[-1]:
        logger.warning(
            "h2so4_acidity: water pressure %.2e mbar outside lookup range "
            "[%.1e, %.1e] mbar — extrapolating",
            water_pres_mbar, _WATER_PRES_MBAR[0], _WATER_PRES_MBAR[-1]
        )

    return wf
