"""
Unit conversion functions for plumeIR v3.

All functions are broadcasting-safe (accept scalars or NumPy arrays).

Conventions
-----------
- Distances (pathlength, altitude) always in **km**.
- Temperature in **Kelvin**, pressure in **mbar** throughout.
- "ppmm" means ppm·m (the standard spectroscopic unit); the km→m
  conversion is handled internally so the caller always passes km.

Physical constants (CODATA 2018)
---------------------------------
    N_A = 6.02214076e23  mol⁻¹
    R   = 8.314462618e-2 m³·mbar·K⁻¹·mol⁻¹
"""

from __future__ import annotations

import numpy as np

from plumeIR.constants import WEIGHTS

# ---------------------------------------------------------------------------
# NumPy compatibility: ``np.trapezoid`` was added in NumPy 2.0 and ``np.trapz``
# removed in later 2.x.  Prefer ``trapezoid``; only touch ``trapz`` on NumPy <2
# (a plain ``getattr(np, "trapezoid", np.trapz)`` fails because the default is
# evaluated eagerly and ``np.trapz`` no longer exists).
# ---------------------------------------------------------------------------
_trapz = np.trapezoid if hasattr(np, "trapezoid") else np.trapz

# ---------------------------------------------------------------------------
# Physical constants
# ---------------------------------------------------------------------------

_N_A: float = 6.02214076e23        # Avogadro constant [mol⁻¹]  CODATA 2018
_R: float = 8.314462618e-2         # Gas constant [m³·mbar·K⁻¹·mol⁻¹]


# =============================================================================
# VMR ↔ SCD
# =============================================================================

def vmr_to_scd(
    vmr: float | np.ndarray,
    pathlength: float | np.ndarray,
    temp: float | np.ndarray = 298.0,
    pres: float | np.ndarray = 1013.0,
    elev: float = 90.0,
    output: str = "molec/cm2",
) -> float | np.ndarray:
    """
    Convert volume mixing ratio (VMR) to slant column density (SCD).

    For a homogeneous layer the path is ``L / sin(elev)``.
    For a vertical profile the column is integrated over altitude and then
    divided by ``sin(elev)`` (constant-elevation slant geometry).

    Parameters
    ----------
    vmr : float or np.ndarray
        Gas VMR [ppmv].  Scalar for a homogeneous layer; 1-D array of
        values at successive altitudes for a vertical profile.
    pathlength : float or np.ndarray
        Homogeneous layer: geometric path length [km].
        Vertical profile: altitude array [km], same shape as ``vmr``.
    temp : float or np.ndarray
        Temperature [K].  Scalar or profile array matching ``vmr``.
    pres : float or np.ndarray
        Pressure [mbar].  Scalar or profile array matching ``vmr``.
    elev : float
        Elevation angle of the line of sight [degrees].  Default 90° (zenith).
        Applied as ``1 / sin(elev)`` after integration.
    output : str
        ``'molec/cm2'`` (default) or ``'ppmm'``.

    Returns
    -------
    float or np.ndarray
        SCD in the requested unit.

    Notes
    -----
    ``output='ppmm'`` ignores T, P, and elevation and returns
    ``vmr * pathlength * 1e3`` [ppm·m] (pathlength converted km → m).
    """
    if output == "ppmm":
        return vmr * np.asarray(pathlength) * 1e3  # [ppm·m]

    n_tot = pres / _R / temp            # [mol/m³]
    n_gas = vmr * 1e-6 * n_tot * _N_A  # [molec/m³]

    is_profile = np.ndim(pathlength) > 0

    if is_profile:
        # Integrate over altitude profile; pathlength is altitude array [km → m]
        scd_m2 = _trapz(n_gas, x=np.asarray(pathlength) * 1e3)  # [molec/m²]
        scd = scd_m2 / np.sin(np.deg2rad(elev)) * 1e-4           # [molec/cm²]
    else:
        # Homogeneous layer
        scd = (
            n_gas * pathlength * 1e3                     # [molec/m²]
            / np.sin(np.deg2rad(elev))
            * 1e-4                                        # [molec/cm²]
        )

    return scd


def scd_to_vmr(
    scd: float | np.ndarray,
    pathlength: float,
    temp: float = 298.0,
    pres: float = 1013.0,
    input_unit: str = "molec/cm2",
    species: str | None = None,
) -> float | np.ndarray:
    """
    Convert slant column density (SCD) to volume mixing ratio (VMR).

    Assumes a homogeneous layer geometry.

    Parameters
    ----------
    scd : float or np.ndarray
        Slant column density in the unit given by ``input_unit``.
    pathlength : float
        Geometric path length [km].
    temp : float
        Temperature [K].  Default 298 K.
    pres : float
        Pressure [mbar].  Default 1013 mbar.
    input_unit : str
        Unit of the input SCD: ``'molec/cm2'``, ``'ppmm'``, or ``'g/m2'``.
    species : str or None
        Species name (required when ``input_unit='g/m2'``).

    Returns
    -------
    float or np.ndarray
        VMR [ppmv].
    """
    if input_unit == "ppmm":
        return scd / (pathlength * 1e3)  # [ppmv]; pathlength km → m

    if input_unit == "g/m2":
        if species is None:
            raise ValueError("species must be provided when input_unit='g/m2'")
        scd = g_m2_to_molec_cm2(scd, species=species)

    # [molec/cm²] → [molec/m³] → VMR
    n_gas = scd * 1e4 / (pathlength * 1e3)  # [molec/m³]
    n_tot = pres / _R / temp * _N_A          # [molec/m³]
    return n_gas / n_tot * 1e6               # [ppmv]


# =============================================================================
# [ppm·m] ↔ [molec/cm²]
# =============================================================================

def ppmm_to_molec_cm2(
    ppmm: float | np.ndarray,
    pres: float | np.ndarray = 1013.0,
    temp: float | np.ndarray = 298.0,
    pathlength: float | np.ndarray | None = None,
) -> float | np.ndarray:
    """
    Convert SCD from [ppm·m] to [molec/cm²].

    Homogeneous layer: ``pres`` and ``temp`` are scalars; ``pathlength``
    is ignored.

    Vertical profile: pass ``pres``, ``temp``, and ``pathlength`` as
    1-D arrays of equal length.  ``ppmm`` must also be a 1-D array of
    the same shape (local [ppm·m] per layer).  The profile is integrated
    over altitude using the trapezoid rule.

    Parameters
    ----------
    ppmm : float or np.ndarray
        SCD [ppm·m] (scalar) or local [ppm·m] per layer (array).
    pres : float or np.ndarray
        Pressure [mbar].
    temp : float or np.ndarray
        Temperature [K].
    pathlength : float or np.ndarray or None
        Altitude array [km] for profile integration.  Pass ``None``
        (default) for the homogeneous single-layer case.

    Returns
    -------
    float or np.ndarray
        SCD [molec/cm²].
    """
    # Homogeneous layer: direct conversion
    if pathlength is None or np.ndim(pathlength) == 0:
        return np.asarray(ppmm) * 1e-6 * pres / _R / temp * _N_A * 1e-4

    # Vertical profile: integrate local number density over altitude
    ppmm = np.asarray(ppmm)
    pres = np.asarray(pres)
    temp = np.asarray(temp)
    alt = np.asarray(pathlength)

    n_gas = ppmm * 1e-6 * pres / _R / temp * _N_A  # [molec/m³] per layer
    scd_m2 = _trapz(n_gas, x=alt * 1e3)            # [molec/m²]
    return scd_m2 * 1e-4                            # [molec/cm²]


def molec_cm2_to_ppmm(
    mcm2: float | np.ndarray,
    pres: float | np.ndarray = 1013.0,
    temp: float | np.ndarray = 298.0,
    pathlength: float | np.ndarray | None = None,
) -> float | np.ndarray:
    """
    Convert SCD from [molec/cm²] to [ppm·m].

    For a homogeneous layer this is the exact inverse of
    :func:`ppmm_to_molec_cm2`.  A profile branch is provided for
    symmetry but note that inverting a total column SCD to a profile of
    [ppm·m] values is underdetermined; this branch returns a
    height-weighted average [ppm·m] rather than a profile.

    Parameters
    ----------
    mcm2 : float or np.ndarray
        SCD [molec/cm²].
    pres : float or np.ndarray
        Pressure [mbar].
    temp : float or np.ndarray
        Temperature [K].
    pathlength : float or np.ndarray or None
        Altitude array [km] for the profile case.  Pass ``None``
        (default) for the homogeneous single-layer case.

    Returns
    -------
    float or np.ndarray
        SCD [ppm·m] (homogeneous) or height-weighted average [ppm·m]
        (profile case).
    """
    if pathlength is None or np.ndim(pathlength) == 0:
        return np.asarray(mcm2) * 1e4 / _N_A * _R * temp / pres * 1e6

    # Profile: compute total number density column, then express SCD as ppm·m
    pres = np.asarray(pres)
    temp = np.asarray(temp)
    alt = np.asarray(pathlength)

    n_tot_col = _trapz(pres / _R / temp * _N_A, x=alt * 1e3) * 1e-4  # [molec/cm²]
    vmr_mean = np.asarray(mcm2) / n_tot_col * 1e6                     # mean VMR [ppmv]
    dz_m = (alt[-1] - alt[0]) * 1e3                                   # total depth [m]
    return vmr_mean * dz_m                                             # [ppm·m]


# =============================================================================
# [molec/cm²] ↔ [g/m²]
# =============================================================================

def molec_cm2_to_g_m2(
    mcm2: float | np.ndarray,
    species: str,
) -> float | np.ndarray:
    """
    Convert SCD from [molec/cm²] to [g/m²] using molecular weight.

    Parameters
    ----------
    mcm2 : float or np.ndarray
        SCD [molec/cm²].
    species : str
        Species name (looked up in :data:`~plumeIR.constants.WEIGHTS`).

    Returns
    -------
    float or np.ndarray
        Mass column [g/m²].
    """
    return np.asarray(mcm2) * 1e4 / _N_A * WEIGHTS[species]


def g_m2_to_molec_cm2(
    gm2: float | np.ndarray,
    species: str,
) -> float | np.ndarray:
    """
    Convert mass column from [g/m²] to [molec/cm²] using molecular weight.

    Parameters
    ----------
    gm2 : float or np.ndarray
        Mass column [g/m²].
    species : str
        Species name (looked up in :data:`~plumeIR.constants.WEIGHTS`).

    Returns
    -------
    float or np.ndarray
        SCD [molec/cm²].
    """
    return np.asarray(gm2) / WEIGHTS[species] * _N_A / 1e4


# =============================================================================
# Water vapour: saturation pressure and VMR ↔ relative humidity
# =============================================================================

def saturation_pressure(temp: float | np.ndarray) -> float | np.ndarray:
    """
    Saturation vapour pressure over liquid water or ice [mbar].

    Uses the Wagner & Pruss (2002) formulation for T > 273.16 K and
    the Murphy & Koop (2005) formulation for T ≤ 273.16 K.

    Parameters
    ----------
    temp : float or np.ndarray
        Temperature [K].

    Returns
    -------
    float or np.ndarray
        Saturation vapour pressure [mbar].

    Notes
    -----
    The ice branch uses the Murphy & Koop (2005) triple-point anchor
    ``Pn = 6.11657 mbar`` at ``Tn = 273.16 K``.  An alternative commonly
    cited value is ``Pn = 6.1078 mbar`` (WMO standard).  The difference
    is < 0.01% at 273 K and negligible for typical atmospheric retrievals.
    """
    temp = np.asarray(temp, dtype=float)
    scalar = temp.ndim == 0
    temp = np.atleast_1d(temp)
    psat = np.empty_like(temp)

    # --- liquid water (Wagner & Pruss 2002) ---
    liq = temp > 273.16
    if np.any(liq):
        Tc, Pc = 647.096, 220640.0  # critical T [K], critical P [mbar]
        t = 1.0 - temp[liq] / Tc
        poly = (Tc / temp[liq]) * (
            -7.85951783  * t
            + 1.84408259 * t ** 1.5
            - 11.7866497 * t ** 3
            + 22.6807411 * t ** 3.5
            - 15.9618719 * t ** 4
            +  1.80122502 * t ** 7.5
        )
        psat[liq] = np.exp(poly) * Pc

    # --- ice (Murphy & Koop 2005) ---
    ice = ~liq
    if np.any(ice):
        Pn, Tn = 6.11657, 273.16   # [mbar], [K] — see Notes for alternative
        t = temp[ice] / Tn
        poly = -13.928169 * (1.0 - t ** -1.5) + 34.707823 * (1.0 - t ** -1.25)
        psat[ice] = np.exp(poly) * Pn

    return float(psat[0]) if scalar else psat


def rh_to_vmr(
    rh: float | np.ndarray,
    pres: float | np.ndarray = 1013.0,
    temp: float | np.ndarray = 298.0,
) -> float | np.ndarray:
    """
    Convert relative humidity to water vapour VMR [ppmv].

    Parameters
    ----------
    rh : float or np.ndarray
        Relative humidity [%].
    pres : float or np.ndarray
        Total pressure [mbar].
    temp : float or np.ndarray
        Temperature [K].

    Returns
    -------
    float or np.ndarray
        Water vapour VMR [ppmv].
    """
    return rh / 100.0 * saturation_pressure(temp) / pres * 1e6


def vmr_to_rh(
    vmr: float | np.ndarray,
    pres: float | np.ndarray = 1013.0,
    temp: float | np.ndarray = 298.0,
) -> float | np.ndarray:
    """
    Convert water vapour VMR to relative humidity [%].

    Parameters
    ----------
    vmr : float or np.ndarray
        Water vapour VMR [ppmv].
    pres : float or np.ndarray
        Total pressure [mbar].
    temp : float or np.ndarray
        Temperature [K].

    Returns
    -------
    float or np.ndarray
        Relative humidity [%].
    """
    return vmr * pres / saturation_pressure(temp) * 100.0 / 1e6


# =============================================================================
# Temperature conversions
# =============================================================================

def c_to_k(deg_c: float | np.ndarray) -> float | np.ndarray:
    """Celsius → Kelvin."""
    return np.asarray(deg_c) + 273.15


def k_to_c(kelvin: float | np.ndarray) -> float | np.ndarray:
    """Kelvin → Celsius."""
    return np.asarray(kelvin) - 273.15


def f_to_c(deg_f: float | np.ndarray) -> float | np.ndarray:
    """Fahrenheit → Celsius."""
    return (np.asarray(deg_f) - 32.0) / 1.8


def c_to_f(deg_c: float | np.ndarray) -> float | np.ndarray:
    """Celsius → Fahrenheit."""
    return np.asarray(deg_c) * 1.8 + 32.0


def f_to_k(deg_f: float | np.ndarray) -> float | np.ndarray:
    """Fahrenheit → Kelvin."""
    return f_to_c(deg_f) + 273.15


def k_to_f(kelvin: float | np.ndarray) -> float | np.ndarray:
    """Kelvin → Fahrenheit."""
    return c_to_f(k_to_c(kelvin))
