"""
Default species parameters for plumeIR v3.

Each entry defines the prior value (initial guess), valid bounds, and
size distribution parameters used when building a ParameterSet.
These are starting points — config files override them per fit window.

Units
-----
Gas concentrations  : ppmv (VMR)
Aerosol mass column : dimensionless scaling factor relative to ref_column
Aerosol size        : effective radius [µm]

Read-only
---------
Every table and node array here is read-only: the dicts are
``MappingProxyType`` views (nested ones too) and the arrays have
``writeable=False``.  A process that runs several analyses (a GUI session,
a notebook) must not let one analysis change the defaults the next one is
built from.  To try a different value, set it in the config (``*DEFAULTS``)
or, for a one-off experiment script, rebind the module attribute to a new
table built with :func:`freeze`.
"""

from __future__ import annotations

from collections.abc import Mapping
from types import MappingProxyType

import numpy as np


def freeze(table: Mapping) -> Mapping:
    """
    Return a read-only view of a (possibly nested) mapping.

    Parameters
    ----------
    table : Mapping
        Mapping whose values may themselves be mappings.  It is copied, so
        later changes to ``table`` do not show through.

    Returns
    -------
    Mapping
        A ``MappingProxyType``; nested mappings are frozen the same way.
    """
    return MappingProxyType({
        k: freeze(v) if isinstance(v, Mapping) else v for k, v in table.items()
    })


def _readonly(arr: np.ndarray) -> np.ndarray:
    """Mark a node array read-only, so in-place edits raise."""
    arr.flags.writeable = False
    return arr

# ---------------------------------------------------------------------------
# Gas species defaults
# Keys: species name
# Values: conc [ppmv] (reference concentration used to compute ref_SCD)
#
# Gas parameters are dimensionless SCALING factors, not concentrations.
# The bounds on the scaling factor default to (0, +inf): Beer-Lambert is
# linear, so any column amount is physical.  The reference SCD is derived
# from `conc` at build time using the geometry's pathlength, T, and P.
#
# Optional `min`/`max` keys override the default scaling bounds if present
# (e.g. via the *DEFAULTS config section).  H2O is special — see
# H2O_VMR_NODES below — its scaling bound is set from the LUT VMR range.
# ---------------------------------------------------------------------------

GAS_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    # Atmospheric background gases — `conc` is the reference VMR [ppmv]
    "H2O":   {"conc": 1.9e4},
    "CO2":   {"conc": 420.0},
    "O3":    {"conc": 0.05},
    "N2O":   {"conc": 0.33},
    "CH4":   {"conc": 1.9},
    "O2":    {"conc": 2.1e5},
    "NO":    {"conc": 0.001},
    "NO2":   {"conc": 0.001},
    "HNO3":  {"conc": 0.001},
    "N2":    {"conc": 7.8e5},
    "ClO":   {"conc": 0.001},
    "SF6":   {"conc": 1.0e-5},
    "OH":    {"conc": 1.0e-4},
    # Volcanic / plume gases
    "SO2":   {"conc": 2.0},
    "SO3":   {"conc": 0.01},
    "HCl":   {"conc": 0.5},
    "HF":    {"conc": 0.1},
    "H2S":   {"conc": 0.1},
    "OCS":   {"conc": 0.5e-3},
    "CO":    {"conc": 0.1},
    "HBr":   {"conc": 0.01},
    "HI":    {"conc": 0.01},
    # Wildfire / anthropogenic
    "NH3":   {"conc": 0.01},
})

# ---------------------------------------------------------------------------
# Aerosol species defaults
# Keys: species name
# Values: conc — reference mass CONCENTRATION [g/m³]; the reference mass
#                column is derived as conc × plume_thickness so the scaling
#                factor (= 1 at the reference) is physically grounded and
#                yields a consistent number density (mirrors the gas ref_SCD
#                convention).  MEC is intensive (independent of N), so this
#                affects interpretation/output, not the fitted OD.
#         size [µm], sigma (log-normal std dev), min_size/max_size [µm],
#         comp (species-specific composition: weight or mass fraction)
# ---------------------------------------------------------------------------

AERO_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    "H2SO4_aq": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.1,
        # 2.0, not the LUT's 10.0: sulphate droplets in a volcanic plume are
        # sub-micron to ~1 um, and above ~3 um the TIR extinction goes grey and
        # becomes degenerate with dt_dist and the background polynomial.  The
        # LUT still spans 10 um; this bounds the FIT.  See config/builder.py.
        "max_size": 2.0,
        "comp":     0.65,   # acid weight fraction
    },
    "ASH": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.1,
        "max_size": 10.0,
        "comp":     0.60,   # SiO2 mass fraction (andesitic proxy)
    },
    "WATER": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.1,
        "max_size": 10.0,
        "comp":     None,
    },
    "ICE": {
        "conc":     1.0e-3,
        "size":     2.0,
        "sigma":    1.5,
        "min_size": 1.0,
        "max_size": 20.0,
        "comp":     None,
    },
    "SOOT": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.01,
        "max_size": 10.0,
        "comp":     None,
    },
    "DUST": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.1,
        "max_size": 10.0,
        "comp":     0.60,   # SiO2 mass fraction
    },
    "OC": {
        "conc":     1.0e-3,
        "size":     1.0,
        "sigma":    1.5,
        "min_size": 0.01,
        "max_size": 10.0,
        "comp":     None,
    },
})

# ---------------------------------------------------------------------------
# Spectral / ILS parameter fit bounds
#
# Bounds only.  The a priori values are config keys (fov, max_opd, nu_shift,
# offset in *RETRIEVAL) and their defaults live in the key catalogue,
# config/keys.py (M3d, 2026-11-12); the unused "value" entries that duplicated
# them were removed then.
# ---------------------------------------------------------------------------

ILS_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    "fov":      {"min": 0.001, "max": 0.2},    # [rad]
    "max_opd":  {"min": 0.5,   "max": 5.0},    # [cm]
    "nu_shift": {"min": -2.0,  "max": 2.0},    # [cm⁻¹]
    "offset":   {"min": -0.5,  "max": 0.5},    # [a.u. or K]
})

# ---------------------------------------------------------------------------
# Temperature parameter fit bounds
#
# Bounds only, as for ILS_DEFAULTS: source_temp, dt_prox and dt_dist a priori
# values are config keys (config/keys.py); gas_temp and atm_temp start from
# the plume_temp / atm_temp keys.
# ---------------------------------------------------------------------------

TEMP_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    "gas_temp":    {"min": 300.0, "max": 1000.0},  # [K]
    "atm_temp":    {"min": 250.0, "max":  350.0},  # [K]
    "source_temp": {"min": 800.0, "max": 2000.0},  # [K]
    # Plume temperature is fitted DIRECTLY (not as an offset), so it feeds
    # both the gas LUT query and the Planck self-emission term — Kirchhoff
    # for a single near-isothermal layer.  Bounds are relative to the
    # ambient temperature at plume_height, resolved from the profile: an
    # advected plume cools adiabatically to within ~10 K of ambient within
    # minutes.  Replaces v2/v3-early "dt_plume", which shifted only the
    # emission level and was therefore exactly degenerate with the column.
    "plume_temp":  {"below": 5.0,   "above": 15.0},               # [K] about ambient
    "dt_prox":     {"min": -20.0, "max":  20.0},   # [K] proximal layer offset
    "dt_dist":     {"min": -20.0, "max":  20.0},   # [K] distal layer offset
})

# ---------------------------------------------------------------------------
# LUT node ranges — single source of truth
#
# These arrays define BOTH the LUT grid (full array, used by the reference
# generators) AND the strict bounds on LUT-indexing parameters (endpoints
# only, used by the config builder).  A parameter that indexes a LUT axis
# (temperature, effective radius, weight fraction) is bounded to
# [nodes[0], nodes[-1]] so the optimizer can never request a value outside
# the tabulated range — no extrapolation is permitted.
#
# To extend a range, edit here and rebuild the affected LUTs.
# ---------------------------------------------------------------------------

# Gas temperature grids [K]
ATM_TEMP_NODES:   np.ndarray = _readonly(np.linspace(250.0, 350.0, 21))   # atmospheric layer
PLUME_TEMP_NODES: np.ndarray = _readonly(np.linspace(300.0, 1000.0, 25))  # hot volcanic gas

# Emission and solar geometries: the plume is a thin layer near ambient
# temperature (an advected plume cools to within a few K of ambient within
# minutes, and a high-altitude plume is cold), so its gas LUT needs a COLD axis,
# not the hot-gas one above, which belongs to the layer geometry (hot gas in
# front of a lava source).  With the 300-1000 K axis an
# ambient plume (289.7 K at 1 km over Stromboli) lay outside the table and the
# query silently clamped it to the 300 K node while the Planck term used the
# true temperature.  Queries outside an axis now raise (GasLUT.query), so each
# geometry builds the axis it needs: see plume_temp_nodes_for().
EMISSION_PLUME_TEMP_NODES: np.ndarray = _readonly(np.arange(220.0, 401.0, 10.0))   # 220-400 K
                                                                        # (emission + solar)
_EMISSION_NODE_STEP: float = 10.0

def plume_temp_nodes_for(geometry, plume_temp: float | None = None) -> np.ndarray:
    """
    Temperature axis of the plume gas LUTs for a run's geometry.

    The layer geometry (a hot gas in front of a lava source) uses the hot-gas
    axis, ``PLUME_TEMP_NODES``.  The emission and solar geometries, whose plume
    is near the ambient temperature at its altitude, use
    ``EMISSION_PLUME_TEMP_NODES`` (220-400 K), widened in 10 K steps if the run
    needs more: a fitted plume temperature is bounded about the ambient a
    priori, so the axis must contain ``[plume_temp - below, plume_temp +
    above]``, and a fixed one must contain ``plume_temp``.  Because the axis is
    part of the LUT cache key, a widened axis makes the table rebuild; it is
    never clamped to fit.

    Parameters
    ----------
    geometry : LayerGeometry, EmissionGeometry or SolarGeometry
        The run geometry.  The layer geometry is recognised by its
        ``fit_gas_temp`` field; ``fit_tplume`` (emission only) marks a fitted
        plume temperature.
    plume_temp : float, optional
        The plume temperature actually used, where the caller has resolved it
        (solar reads it from the profile); defaults to ``geometry.plume_temp``.

    Returns
    -------
    np.ndarray
        Ascending temperature nodes [K], shape (n_temp,).
    """
    if hasattr(geometry, "fit_gas_temp"):            # layer geometry
        return PLUME_TEMP_NODES

    t = plume_temp if plume_temp is not None else getattr(geometry, "plume_temp", None)
    if t is None:                                    # unresolved: the default axis
        return EMISSION_PLUME_TEMP_NODES
    lo = hi = float(t)
    if getattr(geometry, "fit_tplume", False):
        spec = TEMP_DEFAULTS["plume_temp"]
        lo, hi = lo - spec["below"], hi + spec["above"]
    step = _EMISSION_NODE_STEP
    lo = min(float(EMISSION_PLUME_TEMP_NODES[0]), step * np.floor(lo / step))
    hi = max(float(EMISSION_PLUME_TEMP_NODES[-1]), step * np.ceil(hi / step))
    return np.arange(lo, hi + 0.5 * step, step)


# H2O VMR grid [ppmv], log-spaced — the second axis of the 2-D H2O LUT
H2O_VMR_NODES:    np.ndarray = _readonly(np.geomspace(200.0, 50000.0, 16))

# Atmospheric H2O column-scaling grid (emission geometry) — the axis of the
# 1-D atmospheric-layer LUTs (L_layer, T_layer over H2O_sc).  Dimensionless
# multiplier on the background H2O profile (daily column fluctuation).
H2O_SC_NODES:     np.ndarray = _readonly(np.linspace(0.2, 3.0, 15))

# Atmospheric gases that get an independent **distal-layer** column scaling in
# the emission geometry, on top of the H2O_sc that scales H2O across all three
# layers.  Measured on data/2019-09-11 over 750-1250 cm-1 (mean OD by layer):
#
#     gas    prox    slice    dist   column   share
#     H2O   0.773    0.390   1.307    2.470   74.9%   <- H2O_sc, all layers
#     O3    0.003    0.002   0.650    0.656   19.9%   <- 99.2% distal
#     CO2   0.015    0.009   0.087    0.111    3.4%
#     CH4   0.004    0.003   0.038    0.044    1.3%
#     N2O   0.001    0.001   0.013    0.014    0.4%
#
# O3 is the only non-H2O gas carrying enough optical depth to be worth a lever,
# and it is essentially all stratospheric — hence *distal*, not v2's plume
# slice, where only 0.3% of it sits.  Each entry costs one extra RFM run.
# ---------------------------------------------------------------------------
# Typical ambient background VMRs [ppmv] for the ratio panels (M3, 2026-11-10)
#
# Turned into the column the ambient air alone would give over the layer path
# (pathlength at atm_pres, atm_temp) and drawn as a reference line on ratio
# panels.  Typical clean-air values; set the measured value per campaign with
# *PLOT background_<gas>.  H2O has no default: it needs the relative humidity
# at collection (*PLOT rh).  Not used by the retrieval.
# ---------------------------------------------------------------------------

BACKGROUND_VMR: Mapping[str, float] = freeze({
    "CO2": 420.0,
    "CO":  0.1,
    "CH4": 1.9,
})

EMISSION_DISTAL_GASES: tuple[str, ...] = ("O3",)

# Bounds for a distal gas column scaling.  Wider than H2O_SC_NODES because it
# is not a LUT index — the term is applied analytically in OD space, so there
# is no node grid to stay inside.
DISTAL_GAS_SC_BOUNDS: tuple[float, float] = (0.2, 3.0)

# Aerosol effective radius grids [µm], log-spaced
AERO_R0_NODES:    np.ndarray = _readonly(np.geomspace(0.05, 10.0, 24))    # general 1-D species
H2SO4_R0_NODES:   np.ndarray = _readonly(np.geomspace(0.05, 10.0, 24))    # H2SO4 2-D r0 axis

# H2SO4 weight fraction grid — the second axis of the 2-D H2SO4 LUT.
# These are the measured composition points of the canonical ARIA source
# (Lund Myhre et al. 2003): 38/48/58/65/72 wt%.  mieEODG's single-source LUT is
# locked to a source's actual data points, so the retrievable range is
# 0.38–0.72; the old linspace(0.38, 0.80, 11) assumed interpolable RI that the
# current mieEODG API does not provide.
H2SO4_WT_NODES:   np.ndarray = _readonly(np.array([0.38, 0.48, 0.58, 0.65, 0.72]))

# Fixed temperature for H2SO4 aerosol RI evaluation [K]
H2SO4_TEMP_K:     float = 298.0
