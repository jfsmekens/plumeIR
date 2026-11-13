"""
Species registry, plot styling, spectral window presets, and unit notation
for plumeIR v3.

Adapted from rfmwrapper/constants.py (April 2025) with additions for the
broader plumeIR species set and retrieval diagnostics.
"""

from matplotlib import colormaps as cm
import cmcrameri.cm as cmc


# =============================================================================
# File format registries
# =============================================================================

MIDAC_EXTENSIONS: list[str] = ["spc", "sbm", "ifg", "sb", "abs"]
BRUKER_EXTENSIONS: list[str] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
INVALID_EXTENSIONS: list[str] = [
    "pkl", "csv", "txt", "ini", "dat", "xls", "xlsx",
    "doc", "docx", "jpg", "png", "pdf", "zip", "tar",
]

# Filename markers (matched case-insensitively against the file stem) that
# identify a *reference* measurement rather than a scene spectrum: the
# instrument self-emission background, the blackbody calibration targets and
# the clear-sky reference.  These files live alongside the data
# and are consumed by preprocessing, never fitted, so file discovery skips
# them regardless of the user's `filter_string`.
#
# Convention inherited from v2 (analysis.py::makeSelfEmission,
# ::makeCalibration, ::makeClear).  Matching is case-insensitive on the file
# stem, so a scene file whose name happens to contain one of these substrings
# would be skipped — keep the markers specific enough that that is unlikely.
REFERENCE_MARKERS: list[str] = [
    "self",             # layer: instrument self-emission background
    "bbc", "bbh",       # emission: cold / hot blackbody calibration targets
    "clear", "sky",     # emission: clear-sky reference
]


# =============================================================================
# Species registries
# =============================================================================

# Gas species handled by RFM via HITRAN line data
RFM_GASES: list[str] = [
    "H2O", "CO2", "SO2", "CO", "NO", "O3", "N2O", "NO2", "NH3", "HNO3",
    "CH4", "O2", "HCl", "HF", "HBr", "HI", "H2S", "N2", "ClO", "SF6",
    "SO3", "OCS", "OH",
]

# All gas species recognised by plumeIR (RFM only for now; SiF4 deferred)
POSSIBLE_GASES: list[str] = RFM_GASES

# Aerosol / particulate species.
# Treated as dimensionless scaling factors in ATM profiles, not VMRs.
# 'AEROSOL' is RFM molecule slot 100 (native km⁻¹); all others require the
# synthetic multi-tabulation pre-scaling approach (see RFMwrapper docs).
AEROSOL_SPECIES: set[str] = {
    "AEROSOL",  # RFM molecule 100 — native km⁻¹ slot
    "ASH",      # volcanic ash
    "H2SO4",    # sulphuric acid aerosol
    "HNO3",     # nitric acid aerosol (distinct from gas-phase HNO3)
    "WATER",    # liquid water droplets
    "ICE",      # water ice
    "SOOT",     # black carbon / soot
    "DUST",     # mineral dust
    "OC",       # organic carbon (wildfire smoke)
}

POSSIBLE_AEROSOLS: list[str] = sorted(AEROSOL_SPECIES)

# Gases treated via scaling of a continuum/line-mixing reference rather than
# a simple temperature-dependent polynomial (currently only H2O).
CONTINUUM_GASES: list[str] = ["H2O"]

# Atmosphere profile fields used in RFM .atm files
ATM_FIELDS: dict[str, str] = {"HGT": "Height", "PRE": "Pressure", "TEM": "Temperature"}
ATM_UNITS: dict[str, str] = {"HGT": "[km]", "PRE": "[mb]", "TEM": "[K]"}


# =============================================================================
# Molecular weights [g/mol]  —  IUPAC 2021 standard atomic weights
# =============================================================================

WEIGHTS: dict[str, float] = {
    "H2O":    18.01528,
    "CO2":    44.009,
    "O3":     47.997,
    "N2O":    44.013,
    "CH4":    16.043,
    "O2":     31.998,
    "SO2":    64.064,
    "SO3":    80.063,
    "HCl":    36.461,
    "HF":     20.006,
    "CO":     28.010,
    "OCS":    60.075,
    "NH3":    17.031,
    "NO":     30.006,
    "NO2":    46.005,
    "HNO3":   63.013,
    "HBr":    80.912,
    "HI":    127.912,
    "N2":     28.014,
    "ClO":    51.452,
    "SF6":   146.055,
    "OH":     17.007,
    "H2S":    34.081,
    "H2SO4":  98.079,
    "ASH":    65.0,       # bulk proxy; not a true molecular weight
    "WATER":  18.01528,   # H2O aerosol
    "ICE":    18.01528,   # H2O ice aerosol
    "SOOT":   12.011,     # carbon proxy
    "DUST":   60.08,      # SiO2 proxy
    "OC":    200.0,       # organic carbon bulk proxy
}


# =============================================================================
# ILS apodization types
# =============================================================================

APOD_TYPES: list[str] = [
    "Boxcar", "Uniform", "Triangular", "Blackman-Harris", "Happ-Genzel",
    "Hamming", "Lorenz", "Gaussian", "NB weak", "NB medium", "NB strong", "Cosine",
]


# =============================================================================
# Species ratios of interest
# Shown as scatter plots when both numerator and denominator are fitted targets.
# Format: "numerator:denominator"
# =============================================================================

INTERESTING_RATIOS: list[str] = [
    "CO2:SO2", "H2O:SO2", "H2O:CO2", "CO:SO2",
    "SO2:HCl", "SO2:HF", "SO2:H2S", "SO2:SO3",
    "HCl:HF", "SO2:H2SO4", "H2SO4:ASH", "H2O:WATER",
    "CO2:CH4", "CH4:CO", "CO:CO2",         # wildfire / anthropogenic
    "NH3:SO2", "NO2:SO2",                   # anthropogenic
    "N/A:N/A",
]


# =============================================================================
# Spectral window presets  [cm⁻¹]
# =============================================================================

# Primary retrieval windows — broadband, one species dominates
PRIMARY_WINDOWS: dict[str, list[float]] = {
    "SO2":  [2400, 2550],
    "HCl":  [2600, 2900],
    "HF":   [4000, 4100],
    "CO2":  [2020, 2150],
    "CO":   [2050, 2200],
    "CH4":  [2900, 3200],
    "NH3":  [930,  980],
    "SO3":  [1340, 1410],
}

# Secondary windows — alternative ranges
SECONDARY_WINDOWS: dict[str, list[float]] = {
    "SO2":  [1080, 1120],
    "HCl":  [5730, 5780],
    "HF":   [4000, 4100],
    "CO2":  [2020, 2150],
    "CH4":  [6000, 6100],
}

# Micro-windows — narrow sub-ranges for line-by-line retrievals
MICRO_WINDOWS: dict[str, list] = {
    "SO2": [2400, 2550],
    "HCl": [
        [2727.0, 2728.5], [2775.0, 2776.5], [2818.75, 2820.35],
        [2820.75, 2822.35], [2843.0, 2844.4], [2903.35, 2904.85],
        [2923.0, 2924.5], [2925.0, 2926.75], [2942.0, 2943.5],
        [2960.3, 2961.825], [2962.3, 2964.0], [2995.0, 2996.5],
    ],
    "HF":  [4000, 4100],
    "CO2": [2020, 2150],
}


# =============================================================================
# Plot colors
# Per-species colors for time series, scatter plots, and fit overlays.
# Two palettes: light mode (PLOT_COLORS) and dark mode (DARK_COLORS).
#
# Light-mode gas and aerosol colours chosen 2026-11-10 (developer: option "C,
# families", over v2's plot_colors, the previous table, Paul Tol muted,
# cmcrameri batlowS and each species' own cmcrameri map).  Hues follow
# chemistry, from the colour-blind-safe Okabe-Ito set where possible:
# sulphur warm (SO2 vermillion), halogens green / purple, carbon dark
# (CO2 near-black, CO brown, CH4 violet), water blue, aerosols by family
# (developer: aerosols get their own scheme, below).  Minimum CIELAB ΔE76
# between species plotted together, normal / deuteranopia / protanopia
# (Machado 2009):
#   layer    SO2 HCl CO2 CO HF                 33 / 18 / 21
#   volcanic SO2 HCl HF CO2 H2O H2S CO         33 / 17 / 21
# against v2 28 / 4 / 8 and the previous table 32 / 5.5 / 6 on the last group.
# Every colour has CIELAB L* <= 71, so thin lines stay visible on white
# (batlowS put HCl at 87).
#
# Aerosols have their own scheme (developer, 2026-11-10: option "P2", after a
# dark-green sulphate proved too close to a dim-grey ash as thin dashed lines,
# ΔE 31 for normal vision): magenta sulphate, charcoal ash, blues for water
# and ice, plum organic carbon, and mineral tones for dust and soot, a part
# of colour space the gases leave free.  Drawn dashed with open markers, so
# gas and particle differ without colour.
#   emission SO2 H2SO4 ASH                     76 / 24 / 25
#   SO2 H2O H2SO4 ASH WATER                    44 / 24 / 16
#   aerosols alone (7)                         24 / 16 / 21
#   sulphate vs ash                            78 / 24 / 25
# Known weak pair: magenta sulphate vs HCl's bluish green, 7.5 under
# deuteranopia (the magenta/green confusion), relevant only if both are
# plotted together.  Next: ice vs O3 9.7, organic carbon vs CH4 9.8.
# cmcrameri stays the choice for 2-D maps (PLOT_CMAPS) and ordered series.
# =============================================================================

_FALLBACK_COLOR: str = "#555555"
_FALLBACK_CMAP = cmc.grayC_r

PLOT_COLORS: dict[str, str] = {
    # Gas species
    "H2O":        "#0072B2",    # blue (Okabe-Ito)
    "H2O_sc":     "lightblue",  # H2O scaling factor
    "OH":         "tab:blue",
    "CO2":        "#333333",    # near black
    "O3":         "#56B4E9",    # sky blue (Okabe-Ito)
    "O2":         "tab:red",
    "N2O":        "#E69F00",    # orange (Okabe-Ito)
    "N2":         "darkgreen",
    "NO":         "brown",
    "NO2":        "brown",
    "CH4":        "#7B3294",    # violet
    "SO2":        "#D55E00",    # vermillion (Okabe-Ito)
    "SO3":        "#E8956B",    # light vermillion
    "CO":         "#8B4513",    # saddle brown
    "OCS":        "#C9A227",    # dark gold
    "HCl":        "#009E73",    # bluish green (Okabe-Ito)
    "HF":         "#CC79A7",    # reddish purple (Okabe-Ito)
    "NH3":        "#6B8E23",    # olive green
    "HNO3":       "goldenrod",
    "H2S":        "#BCBD22",    # olive yellow (tab:olive)
    # Aerosol species
    "H2SO4":      "#E7298A",    # magenta (Dark2)
    "ASH":        "#4D4D4D",    # charcoal
    "WATER":      "#5063E6",    # periwinkle blue, near water vapour on purpose
    "ICE":        "#8DA0CB",    # steel blue
    "SOOT":       "#392E4C",    # dark slate (apart from near-black CO2)
    "DUST":       "#8C643F",    # raw umber
    "OC":         "#7A0177",    # plum
    # Retrieval diagnostics
    "source_temp": "r",
    "gas_temp":    "orange",
    "gasE_temp":   "darkorange",
    "E_frac":      "k",
    "fov":         "k",
    "nu_shift":    "k",
    "max_opd":     "k",
    "R2":          "k",
    "RMSE":        "k",
    "N/A":         "w",
    # Plot element roles
    "meas":        "k",
    "model":       "r",
    "bkg":         "r",
    "res":         "k",
    "scat_all":    "tab:grey",
    "scat_scroll": "tab:red",
    "scat_last":   "b",
    "regress":     "k",
    "intercept":   "darkcyan",
    "confidence":  "k",
    "pres":        "k",
    "temp":        "k",
}

DARK_COLORS: dict[str, str] = {
    "H2O":         "tab:blue",
    "H2O_sc":      "lightblue",
    "CO2":         "tab:orange",
    "O3":          "tab:pink",
    "N2O":         "gold",
    "CH4":         "darkviolet",
    "SO2":         "tab:red",
    "SO3":         "tab:orange",
    "CO":          "maroon",
    "OCS":         "goldenrod",
    "HCl":         "turquoise",
    "HF":          "magenta",
    "NH3":         "yellow",
    "H2S":         "olivedrab",
    "H2SO4":       "tab:green",
    "ASH":         "tab:grey",
    "WATER":       "deepskyblue",
    "ICE":         "powderblue",
    "SOOT":        "silver",
    "DUST":        "wheat",
    "OC":          "burlywood",
    "source_temp": "r",
    "gas_temp":    "orange",
    "gasE_temp":   "darkorange",
    "fov":         "w",
    "nu_shift":    "w",
    "max_opd":     "w",
    "R2":          "w",
    "RMSE":        "w",
    "N/A":         "k",
    "meas":        "w",
    "model":       "r",
    "bkg":         "r",
    "res":         "w",
    "scat_all":    "w",
    "scat_scroll": "tab:pink",
    "scat_last":   "dodgerblue",
    "regress":     "w",
    "intercept":   "tab:cyan",
    "confidence":  "w",
}


# =============================================================================
# Per-species colormaps for 2D plots (e.g. time–wavenumber heatmaps)
# =============================================================================

PLOT_CMAPS: dict = {
    "H2O":    cmc.nuuk_r,
    "CO2":    cmc.bilbao_r,
    "O3":     cmc.acton_r,
    "SO2":    cmc.lajolla_r,
    "CH4":    cmc.devon_r,
    "CO":     cmc.tokyo_r,
    "HCl":    cmc.davos_r,
    "HF":     cmc.lipari_r,
    "N2O":    cmc.hawaii_r,
    "NH3":    cmc.bamako_r,
    "H2S":    cmc.turku_r,
    "H2SO4":  cm["Greens"],
    "ASH":    cm["Greys"],
    "WATER":  cmc.nuuk_r,
    "ICE":    cmc.oslo_r,
    "SOOT":   cm["Greys"],
    "DUST":   cm["Oranges"],
    "OC":     cm["YlOrBr"],
    # Diverging map for covariance matrices
    "cov":    cmc.vik,
    # Diagnostics
    "R2":     cm["Greys"],
    "RMSE":   cm["Greys"],
    "temp":   cmc.hawaii_r,
    "pres":   cmc.bamako_r,
}


def gas_cmap(species: str):
    """Per-species colormap, with a neutral fallback for unknown species."""
    return PLOT_CMAPS.get(species, _FALLBACK_CMAP)


def gas_color(species: str) -> str:
    """Per-species plot color, with a neutral fallback for unknown species."""
    return PLOT_COLORS.get(species, _FALLBACK_COLOR)


# =============================================================================
# Display names (Matplotlib mathtext)
# =============================================================================

PRETTY_NAMES: dict[str, str] = {
    "H2O":         r"$H_2O$",
    "OH":          r"$OH$",
    "SO2":         r"$SO_2$",
    "SO3":         r"$SO_3$",
    "CO2":         r"$CO_2$",
    "NH3":         r"$NH_3$",
    "HNO3":        r"$HNO_3$",
    "O2":          r"$O_2$",
    "O3":          r"$O_3$",
    "N2":          r"$N_2$",
    "N2O":         r"$N_2O$",
    "NO":          r"$NO$",
    "NO2":         r"$NO_2$",
    "CH4":         r"$CH_4$",
    "CO":          r"$CO$",
    "OCS":         r"$OCS$",
    "HCl":         r"$HCl$",
    "ClO":         r"$ClO$",
    "HF":          r"$HF$",
    "H2S":         r"$H_2S$",
    "H2SO4":       r"$SO_4^{2-}$ aerosol",
    "ASH":         r"$ash$",
    "WATER":       r"$H_2O$ aerosol",
    "ICE":         r"$ice$",
    "SOOT":        r"$soot$",
    "DUST":        r"$dust$",
    "OC":          r"$OC$",
    "time":        "Local Time",
    "R2":          r"$R^2$",
    "RMSE":        "RMS error",
    "source_temp": r"$T_{source}$",
    "gas_temp":    r"$T_{gas}$",
    "gasE_temp":   r"$T_{gas}(E)$",
    "E_frac":      r"$X_{\varepsilon}$",
    "fov":         r"$FOV$",
    "max_opd":     r"$OPD_{max}$",
    "nu_shift":    r"$\nu_{shift}$",
    "N/A":         r"$n/a$",
}

# Derived: pretty ratio labels built from PRETTY_NAMES
PRETTY_RATIOS: dict[str, str] = {}
for _ratio in INTERESTING_RATIOS:
    _y, _x = _ratio.split(":")
    if _y in PRETTY_NAMES and _x in PRETTY_NAMES:
        PRETTY_RATIOS[_ratio] = f"{PRETTY_NAMES[_y]}:{PRETTY_NAMES[_x]}"


# =============================================================================
# Unit notation strings (Matplotlib mathtext)
# =============================================================================

UNITS: dict[str, str] = {
    "rad":          r"$mW\,/\,(m^{2}\cdot sr\cdot cm^{-1})$",
    "bbt":          r"$K$",
    "opt":          r"$a.u.$",
    "wn":           r"$cm^{-1}$",
    "um":           r"$\mu m$",
    "nm":           r"$nm$",
    "molec.cm^-2":  r"$molec\cdot cm^{-2}$",
    "ppmm":         r"$ppm\cdot m$",
    "g.m^-2":       r"$g\cdot m^{-2}$",
    "ppmv":         r"$ppmv$",
    "relh":         r"$\%$",
    "N_density":    r"$cm^{-3}$",
    "N_density_SI": r"$m^{-3}$",
    "g.cm^-3":      r"$g\cdot cm^{-3}$",
    "g.m^-3":       r"$g\cdot m^{-3}$",
    "fov":          r"$mrad$",
    "nu_shift":     r"$cm^{-1}$",
    "gas_temp":     r"$K$",
    "gasE_temp":    r"$K$",
    "source_temp":  r"$K$",
    "max_opd":      r"$cm$",
}


# =============================================================================
# Branding
# =============================================================================

LOGO_TEXT: str = "plumeIR"
VERSION_TEXT: str = "v3.0"
LOGO_FONT: dict = {
    "family": "Palatino",
    "color":  "tab:red",
    "weight": "bold",
    "style":  "italic",
    "size":   18,
}
VERSION_FONT: dict = {
    "family": "Palatino",
    "color":  "tab:red",
    "weight": "normal",
    "style":  "italic",
    "size":   10,
}
