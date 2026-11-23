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
# Bounds only.  The a priori values are config keys (fov, max_opd, shift,
# offset in *RETRIEVAL) and their defaults live in the key catalogue,
# config/keys.py (M3d, 2026-11-12); the unused "value" entries that duplicated
# them were removed then.
# ---------------------------------------------------------------------------

ILS_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    "fov":      {"min": 0.001, "max": 0.2},    # [rad]
    "max_opd":  {"min": 0.5,   "max": 5.0},    # [cm]
    "shift": {"min": -2.0,  "max": 2.0},    # [cm⁻¹]
    "offset":   {"min": -0.5,  "max": 0.5},    # [a.u. or K]
    # Broad line-shape component (2026-11-23): fraction and Gaussian σ.
    "broad_frac":  {"min": 0.0,  "max": 0.9},   # [-]
    "broad_width": {"min": 0.05, "max": 5.0},   # [cm⁻¹]
})

# ---------------------------------------------------------------------------
# Temperature parameter fit bounds
#
# Bounds only, as for ILS_DEFAULTS: source_temp, dt_prox and dt_dist a priori
# values are config keys (config/keys.py); plume_temp and atm_temp start from
# the plume_temp / atm_temp keys.  In the layer geometry plume_temp and
# atm_temp index a gas LUT, so their bounds are the LUT node ranges
# (PLUME_TEMP_NODES, ATM_TEMP_NODES; config/builder.py), not this table; the
# layer's former "gas_temp" entry here (300-1000 K) was never read and went
# with the rename to plume_temp (2026-11-19).
# ---------------------------------------------------------------------------

TEMP_DEFAULTS: Mapping[str, Mapping[str, object]] = freeze({
    "atm_temp":    {"min": 250.0, "max":  350.0},  # [K]
    "source_temp": {"min": 800.0, "max": 2000.0},  # [K]
    # Plume temperature is fitted DIRECTLY (not as an offset), so it feeds
    # both the gas LUT query and the Planck self-emission term — Kirchhoff
    # for a single near-isothermal layer.  Bounds are relative to the
    # ambient temperature at plume_height, resolved from the profile: an
    # advected plume cools adiabatically to within ~10 K of ambient within
    # minutes.  Replaces v2/v3-early "dt_plume", which shifted only the
    # emission level and was therefore exactly degenerate with the column.
    "plume_temp":  {"below": 5.0,   "above": 15.0},  # [K] about ambient (emission, solar)
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

# Gas temperature axes [K]: a fixed range, and a node spacing the user sets
# (*MODEL plume_temp_step / atm_temp_step; developer, 2026-11-21).  Linear
# interpolation between nodes errs as the square of the spacing (×4 per
# halving, measured): at the former 29.2 K plume spacing (25 nodes) up to
# 0.28% of the optical depth at the strongest lines and 0.025% in a target
# column, 40% in the dual-gas plume CO2; at 10 K ~0.03%, ~0.003% and ~5%
# (docs/CONFIG_GUIDE.md, `*MODEL`).
ATM_TEMP_RANGE:   tuple[float, float] = (250.0, 350.0)    # atmospheric layer
PLUME_TEMP_RANGE: tuple[float, float] = (300.0, 1000.0)   # hot volcanic gas (layer)
DEFAULT_ATM_TEMP_STEP:   float = 5.0    # [K], the catalogue default (config/keys.py)
DEFAULT_PLUME_TEMP_STEP: float = 10.0   # [K]


def temp_axis(lo: float, hi: float, step: float) -> np.ndarray:
    """
    Temperature nodes from ``lo`` to ``hi`` inclusive, at most ``step`` apart.

    Evenly spaced; the spacing equals ``step`` when it divides the range
    (``ceil`` of the interval count otherwise, so never coarser than asked).

    Parameters
    ----------
    lo, hi : float
        Range ends [K], ``lo < hi``.
    step : float
        Largest node spacing [K], positive.

    Returns
    -------
    np.ndarray
        Ascending nodes [K], read-only.

    Raises
    ------
    ValueError
        For a non-positive step or an empty range.
    """
    if not step > 0 or not hi > lo:
        raise ValueError(f"temperature axis needs step > 0 and hi > lo, got "
                         f"step {step}, range {lo}-{hi} K")
    n = int(np.ceil((hi - lo) / step - 1e-9))
    return _readonly(np.linspace(lo, hi, n + 1))


def atm_temp_nodes(step: float = DEFAULT_ATM_TEMP_STEP) -> np.ndarray:
    """Temperature axis of the ambient (atmospheric) gas LUTs [K]."""
    return temp_axis(*ATM_TEMP_RANGE, step)


#: The axes at the default spacings (the shipped grids; a run builds its own
#: from its *MODEL steps).
ATM_TEMP_NODES:   np.ndarray = atm_temp_nodes()
PLUME_TEMP_NODES: np.ndarray = temp_axis(*PLUME_TEMP_RANGE, DEFAULT_PLUME_TEMP_STEP)

# Emission and solar geometries: the plume is a thin layer near ambient
# temperature (an advected plume cools to within a few K of ambient within
# minutes, and a high-altitude plume is cold), so its gas LUT needs a COLD axis,
# not the hot-gas one above, which belongs to the layer geometry (hot gas in
# front of a lava source).  With the 300-1000 K axis an
# ambient plume (289.7 K at 1 km over Stromboli) lay outside the table and the
# query silently clamped it to the 300 K node while the Planck term used the
# true temperature.  Queries outside an axis now raise (GasLUT.query), so each
# geometry builds the axis it needs: see plume_temp_nodes_for().
EMISSION_PLUME_TEMP_RANGE: tuple[float, float] = (220.0, 400.0)   # emission + solar
EMISSION_PLUME_TEMP_NODES: np.ndarray = temp_axis(*EMISSION_PLUME_TEMP_RANGE,
                                                  DEFAULT_PLUME_TEMP_STEP)


def plume_temp_nodes_for(geometry, plume_temp: float | None = None,
                         step: float = DEFAULT_PLUME_TEMP_STEP) -> np.ndarray:
    """
    Temperature axis of the plume gas LUTs for a run's geometry.

    The layer geometry (a hot gas in front of a lava source) uses the hot-gas
    range, ``PLUME_TEMP_RANGE`` (300-1000 K).  The emission and solar
    geometries, whose plume is near the ambient temperature at its altitude,
    use ``EMISSION_PLUME_TEMP_RANGE`` (220-400 K), widened to multiples of
    ``step`` if the run needs more: a fitted plume temperature is bounded about the ambient a
    priori, so the axis must contain ``[plume_temp - below, plume_temp +
    above]``, and a fixed one must contain ``plume_temp``.  Because the axis is
    part of the LUT cache key, a widened axis makes the table rebuild; it is
    never clamped to fit.

    Parameters
    ----------
    geometry : LayerGeometry, EmissionGeometry or SolarGeometry
        The run geometry.  For the emission and solar geometries,
        ``fit_plume_temp`` marks a fitted plume temperature.
    plume_temp : float, optional
        The plume temperature actually used, where the caller has resolved it
        (solar reads it from the profile); defaults to ``geometry.plume_temp``.
    step : float
        Largest node spacing [K] (*MODEL ``plume_temp_step``).

    Returns
    -------
    np.ndarray
        Ascending temperature nodes [K], shape (n_temp,).
    """
    from plumeIR.config.objects import LayerGeometry   # config imports standards
    if isinstance(geometry, LayerGeometry):
        return temp_axis(*PLUME_TEMP_RANGE, step)

    base_lo, base_hi = EMISSION_PLUME_TEMP_RANGE
    t = plume_temp if plume_temp is not None else getattr(geometry, "plume_temp", None)
    if t is None:                                    # unresolved: the default range
        return temp_axis(base_lo, base_hi, step)
    lo = hi = float(t)
    if getattr(geometry, "fit_plume_temp", False):
        spec = TEMP_DEFAULTS["plume_temp"]
        lo, hi = lo - spec["below"], hi + spec["above"]
    lo = min(base_lo, step * np.floor(lo / step))
    hi = max(base_hi, step * np.ceil(hi / step))
    return temp_axis(lo, hi, step)


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
