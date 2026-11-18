"""
The config key catalogue: one :class:`Key` per config key.

Everything plumeIR knows about a single config key lives here: its section,
type, unit, default (per geometry where they differ), limits or choices, the
geometries that read it or require it, whether it is a path, and its help text.
The parser converts values by the declared type (:mod:`.parser`), the
validator runs the per-key checks (:mod:`.validator`), the builder takes its
defaults (:mod:`.builder`), :mod:`.paths` resolves the path keys, and
``write_config`` and the generated reference (:mod:`.keydoc`) take their text
from it.  The config dataclasses (:mod:`.objects`) read their defaults through
:func:`default`.

Rules that involve more than one key stay as code in the validator
(``plume_temp_from``, ``atm_temp_from``, gases in both roles, one target per species, ratio
pairs, ``no_gas``).

Physics tables stay in :mod:`plumeIR.standards`: per-species priors
(``GAS_DEFAULTS``, ``AERO_DEFAULTS``), fit bounds (``ILS_DEFAULTS``,
``TEMP_DEFAULTS``), LUT node grids and ``BACKGROUND_VMR``.  The ``*DEFAULTS``
fields below override those tables, and their ``default`` is only the last
fallback for a species the table does not list.

Name patterns (keys whose name carries a species or parameter) are declared
once, in :data:`PATTERNS` and :data:`DEFAULTS_FIELDS`.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, field
from types import MappingProxyType

from plumeIR.constants import (
    APOD_TYPES, CONTINUUM_GASES, POSSIBLE_AEROSOLS, POSSIBLE_GASES,
)

LAYER, EMISSION, SOLAR = "layer", "emission", "solar"
#: Geometry names, in the order the documentation lists them.
GEOMETRIES: tuple[str, ...] = (LAYER, EMISSION, SOLAR)
ALL: frozenset[str] = frozenset(GEOMETRIES)

#: Value types a key may declare.  ``list`` is a comma-separated list of
#: names, ``window`` a ``start, stop`` pair of numbers, ``flags`` True, False
#: or a list of names (a switch for all, or only for those named); every
#: other type holds one value, and only ``list``, ``window`` and ``flags``
#: split on commas.
KEY_TYPES: tuple[str, ...] = ("bool", "int", "float", "str", "path", "list", "window",
                              "flags")

#: Sections in file order.  ``FIT`` stands for every ``*FITn``.
SECTIONS: tuple[str, ...] = ("DATA", "MODEL", "GEOMETRY", "PREPROCESS", "RETRIEVAL",
                             "DEFAULTS", "PLOT", "FIT")

#: Gases that can serve as ``path_tracer`` (layer): well mixed at a known
#: background, absent from volcanic plumes.  CH4 on its ν3 P-branch is the
#: precise one; N2O (2ν1, 2540-2600 cm⁻¹) a series-median check where CH4 is
#: itself a target (seeps, mud volcanoes, fires).  Measured 2026-11-15 on the
#: layer validation set (docs/CONFIG_GUIDE.md, section 3.1).
PATH_TRACERS: tuple[str, ...] = ("CH4", "N2O")

#: Regression methods for gas ratios: Siegel repeated medians (robust, the
#: default), Theil-Sen (robust), orthogonal distance regression (errors on
#: both axes, weighted by the fit errors).  Re-exported by ``objects``.
RATIO_REGRESSIONS: tuple[str, ...] = ("siegel", "theil", "odr")

#: Modes of a preprocessing product (``*PREPROCESS``): not measured, measured and written only, or
#: measured and applied to the spectra the windows fit.
PREPROCESS_MODES: tuple[str, ...] = ("off", "report", "apply")

@dataclass(frozen=True)
class Key:
    """
    One config key.

    Parameters
    ----------
    section : str
        One of :data:`SECTIONS`.
    name : str
        Key name as written in the file (the parser lower-cases names).
    type : str
        One of :data:`KEY_TYPES`.
    default : object
        Value when the key is absent or ``None``, unless ``geometry_defaults``
        names the geometry.
    help : str
        One-line description, written as the key's comment by
        ``write_config`` and in the generated reference.
    unit : str or None
        Unit, written in square brackets after the help text.
    none : str or None
        What ``None`` (the default) means, when that is not obvious.
    low, high : float or None
        Numeric limits; ``low_open`` / ``high_open`` exclude the limit itself.
    choices : tuple or None
        Allowed values; for a ``list`` key, allowed items.
    ignore_case : bool
        Compare choices case-blind (the builder lower-cases the value).
    geometries : frozenset of str
        Geometries that read the key.  Elsewhere it is ignored with a warning.
    required : frozenset of str
        Geometries in which the key must be set.  In a ``*FITn`` section a
        required key is required of active windows only.
    geometry_defaults : mapping
        Per-geometry defaults that replace ``default``.
    must_exist : str or None
        ``'file'`` or ``'path'``: the value must exist on disk.
    rfm_atm : bool
        An RFM atmosphere profile, also looked for in ``<rfm_dir>/atm``.
    expect : str or None
        Wording of the expected value in error messages, replacing the one
        derived from the type and limits.
    item : str
        What a ``list`` key's items are, for error messages.
    inert : str or None
        Set when the key is read and checked but nothing acts on it in this
        version: why, for the comment and the reference.
    """
    section: str
    name: str
    type: str
    default: object = None
    help: str = ""
    unit: str | None = None
    none: str | None = None
    low: float | None = None
    high: float | None = None
    low_open: bool = False
    high_open: bool = False
    choices: tuple | None = None
    ignore_case: bool = False
    geometries: frozenset[str] = ALL
    required: frozenset[str] = frozenset()
    geometry_defaults: Mapping[str, object] = field(default_factory=dict)
    must_exist: str | None = None
    rfm_atm: bool = False
    expect: str | None = None
    item: str = "name"
    inert: str | None = None

    def __post_init__(self) -> None:
        if self.type not in KEY_TYPES:
            raise ValueError(f"{self.section}.{self.name}: unknown type {self.type!r}")
        if self.section not in SECTIONS:
            raise ValueError(f"{self.name}: unknown section {self.section!r}")
        object.__setattr__(self, "geometry_defaults",
                           MappingProxyType(dict(self.geometry_defaults)))

    @property
    def is_path(self) -> bool:
        """Whether the value is a file-system path."""
        return self.type == "path"

    def default_for(self, geometry: str | None = None) -> object:
        """Default value in ``geometry`` (``None``: the general default)."""
        if geometry is not None and geometry in self.geometry_defaults:
            return self.geometry_defaults[geometry]
        return self.default

    def used_in(self, geometry: str) -> bool:
        """Whether ``geometry`` reads this key."""
        return geometry in self.geometries

    def geometry_note(self) -> str:
        """``'layer only'``, ``'emission and solar'``, or ``''`` for all."""
        if self.geometries == ALL:
            return ""
        names = [g for g in GEOMETRIES if g in self.geometries]
        return f"{names[0]} only" if len(names) == 1 else " and ".join(names)

    def comment(self) -> str:
        """The key's one-line comment: help, unit, meaning of None, geometries."""
        text = self.help
        if self.unit:
            text += f" [{self.unit}]"
        if self.none:
            text += f". None = {self.none}"
        if self.inert:
            text += f". {self.inert}"
        note = self.geometry_note()
        if note:
            text += f"  ({note})"
        return text

    def limits_text(self) -> str:
        """The allowed values in words, e.g. ``'in (0, 90]'``, or ``''``."""
        if self.choices is not None:
            shown = ", ".join(str(c) for c in self.choices)
            return f"one of {shown}" if len(self.choices) <= 12 else ""
        if self.low is not None and self.high is not None:
            left = "(" if self.low_open else "["
            right = ")" if self.high_open else "]"
            return f"in {left}{self.low:g}, {self.high:g}{right}"
        if self.low is not None:
            if self.low == 0 and self.low_open:
                return "> 0"
            return f"{'>' if self.low_open else '>='} {self.low:g}"
        if self.high is not None:
            return f"{'<' if self.high_open else '<='} {self.high:g}"
        return ""

    def expectation(self, name: str = "") -> str:
        """What a valid value is, for error messages (``{name}`` filled in)."""
        if self.expect is not None:
            return self.expect.format(name=name)
        noun = {"bool": "True or False", "int": "an integer", "float": "a number",
                "str": "a string", "path": "a path string",
                "list": f"a list of {self.item} names",
                "flags": f"True, False or a list of {self.item} names",
                "window": "wn_start, wn_stop"}[self.type]
        if self.type in ("int", "float") and self.low == 0 and self.low_open \
                and self.high is None:
            return f"a positive {'integer' if self.type == 'int' else 'number'}"
        limits = self.limits_text()
        return f"{noun} {limits}" if limits else noun

@dataclass(frozen=True)
class KeyPattern:
    """
    A family of keys ``<prefix><name>``, all with the value spec ``key``.

    Parameters
    ----------
    prefix : str
        Lower-case prefix, e.g. ``'sigma_prior_'``.
    key : Key
        Value specification; its ``name`` is the pattern as documented, e.g.
        ``'sigma_prior_<param>'``.
    names : tuple of str or None
        Allowed ``<name>`` parts (matched case-blind); ``None``: any name, the
        builder checks it.
    name_kind : str
        What ``<name>`` is, for error messages.
    """
    prefix: str
    key: Key
    names: tuple[str, ...] | None = None
    name_kind: str = "name"

    def match(self, key: str) -> str | None:
        """The ``<name>`` part of ``key``, or ``None`` if it does not match."""
        lowered = key.lower()
        if lowered.startswith(self.prefix) and len(lowered) > len(self.prefix):
            return key[len(self.prefix):]
        return None

    def canonical(self, name: str) -> str | None:
        """The registry spelling of ``name``, or ``None`` if not allowed."""
        if self.names is None:
            return name
        return {n.lower(): n for n in self.names}.get(name.lower())

def _keys(*keys: Key) -> Mapping[tuple[str, str], Key]:
    table: dict[tuple[str, str], Key] = {}
    for k in keys:
        if (k.section, k.name) in table:
            raise ValueError(f"duplicate key {k.section}.{k.name}")
        table[(k.section, k.name)] = k
    return MappingProxyType(table)

_POSITIVE = dict(low=0.0, low_open=True)
#: Read and checked, but nothing acts on it in v3.0 (developer, 2026-11-12:
#: kept for the planned proximal-layer prior).
_PLANNED_PROX = ("Not used in v3.0; planned as a prior for the proximal "
                 "layer, with obs_temp and obs_rh")
_GEO_LAYER = frozenset({LAYER})
_GEO_EMISSION = frozenset({EMISSION})
_GEO_SOLAR = frozenset({SOLAR})
_GEO_EM_SOLAR = frozenset({EMISSION, SOLAR})

# =============================================================================
# The catalogue
# =============================================================================

#: Every config key, by ``(section, name)``, in file order.
KEYS: Mapping[tuple[str, str], Key] = _keys(

    # --- *DATA ---------------------------------------------------------------
    Key("DATA", "data_dir", "path", required=ALL,
        help="Directory containing the spectral data files"),
    Key("DATA", "filter_files", "bool", False,
        help="Filter files by name with filter_string"),
    Key("DATA", "filter_string", "str", None, none="keep all",
        help="Only keep files whose name contains this string"),
    Key("DATA", "filter_ext", "str", None, none="all extensions",
        help="Only keep files with this extension (e.g. 0, spc)"),
    Key("DATA", "max_spec", "int", None, none="all", low=0.0, low_open=True,
        help="Maximum number of spectra to process (the earliest by acquisition time)"),

    # --- *MODEL --------------------------------------------------------------
    # RFM and HITRAN are located by RFMwrapper's own configuration
    # (python -m rfmwrapper configure); the mieEODG ARIA database is the one
    # bundled with mieEODG.  Neither is a plumeIR key.
    Key("MODEL", "nperwn", "float", 25.0, unit="points per cm⁻¹", **_POSITIVE,
        help="Model spectral sampling"),
    Key("MODEL", "model_padding", "float", 50.0, unit="cm⁻¹", **_POSITIVE,
        help="Padding added to each side of the fit window for ILS convolution"),
    Key("MODEL", "cache_dir", "path", None, none="~/.plumeIR/cache/",
        help="Directory for caching pre-computed references"),

    # --- *GEOMETRY, shared ---------------------------------------------------
    Key("GEOMETRY", "type", "str", required=ALL, choices=GEOMETRIES,
        ignore_case=True, help="Measurement geometry"),
    Key("GEOMETRY", "plume_thickness", "float", None, unit="km", **_POSITIVE,
        required=_GEO_EMISSION, geometry_defaults={LAYER: 0.05},
        none="no plume (solar)",
        help="Thickness of the plume layer"),
    Key("GEOMETRY", "plume_temp", "float", None, unit="K", **_POSITIVE,
        required=_GEO_LAYER,
        none="ambient at plume_height, from the atmosphere profile (emission, solar)",
        help="Plume gas temperature (a priori when fitted)"),

    # --- *GEOMETRY, layer ----------------------------------------------------
    Key("GEOMETRY", "pathlength", "float", unit="km", **_POSITIVE,
        geometries=_GEO_LAYER, required=_GEO_LAYER,
        help="Total geometric path length through plume and atmosphere"),
    Key("GEOMETRY", "atm_temp", "float", unit="K", **_POSITIVE,
        geometries=_GEO_LAYER, required=_GEO_LAYER,
        help="Ambient atmospheric temperature"),
    Key("GEOMETRY", "atm_pres", "float", unit="mbar", **_POSITIVE,
        geometries=_GEO_LAYER, required=_GEO_LAYER,
        help="Ambient atmospheric pressure"),
    Key("GEOMETRY", "plume_pres", "float", unit="mbar", **_POSITIVE,
        geometries=_GEO_LAYER, required=_GEO_LAYER,
        help="Plume gas pressure"),
    Key("GEOMETRY", "path_tracer", "str", None, choices=PATH_TRACERS,
        geometries=_GEO_LAYER, none="no path diagnostic",
        help="Well-mixed ambient gas whose fitted column checks pathlength "
             "(a diagnostic; it changes no fit). Must be an ambient-only, "
             "non-target gas of an active window"),
    Key("GEOMETRY", "fit_gas_temp", "bool", False, geometries=_GEO_LAYER,
        help="Fit the plume gas temperature (gas_temp)"),
    Key("GEOMETRY", "fit_atm_temp", "bool", False, geometries=_GEO_LAYER,
        help="Fit the atmospheric temperature (atm_temp)"),
    Key("GEOMETRY", "model_gas_emission", "bool", False, geometries=_GEO_LAYER,
        help="Model thermal emission from the hot plume gas (TIR windows only)"),
    Key("GEOMETRY", "fit_source_temp", "bool", False, geometries=_GEO_LAYER,
        help="Fit the background source temperature"),
    Key("GEOMETRY", "source_temp", "float", 1500.0, unit="K", **_POSITIVE,
        geometries=_GEO_LAYER, help="A priori source temperature"),
    Key("GEOMETRY", "no_gas", "bool", False, geometries=_GEO_LAYER,
        help="No-gas reference spectrum: not available in v3.0, must be False"),

    # --- *PREPROCESS: what is done to the spectra before the fits -----------
    # (moved from *GEOMETRY 2026-11-15; the old place is still read, with a
    # warning: parser.migrate_moved_keys)
    Key("PREPROCESS", "subtract_self", "bool", None,
        geometries=frozenset({LAYER, EMISSION}),
        geometry_defaults={LAYER: False, EMISSION: True},
        help="Subtract the instrument self-emission spectrum before fitting"),
    Key("PREPROCESS", "self_file", "path", None, geometries=_GEO_LAYER,
        must_exist="file", none="find '*self*' files in the data directory",
        help="Self-emission spectrum"),
    Key("PREPROCESS", "background", "str", "report", choices=PREPROCESS_MODES,
        ignore_case=True, geometries=_GEO_LAYER,
        help="Additive background from the opaque bands (self-emission scale "
             "and zero level): off, report (measured and written), or apply "
             "(subtracted instead of the self spectrum, unclipped)"),
    Key("PREPROCESS", "registration", "str", "report", choices=PREPROCESS_MODES,
        ignore_case=True, geometries=frozenset({LAYER, EMISSION}),
        help="Wavenumber shift and scale by cross-correlation with a model "
             "spectrum: off, report (measured and written), or apply (the "
             "wavenumber axis corrected before the fits, per spectrum; "
             "nu_shift still fitted)"),
    Key("PREPROCESS", "reference_spectra", "int", 10, low=1, geometries=_GEO_LAYER,
        help="Number of first spectra whose median top hull is the reference "
             "for the relative scale and temperature (taken from the start of "
             "the series, so it also works in real time)"),

    # --- *GEOMETRY, emission and solar ---------------------------------------
    Key("GEOMETRY", "plume_height", "float", None, unit="km", **_POSITIVE,
        geometries=_GEO_EM_SOLAR, required=_GEO_EMISSION,
        none="no plume (solar)",
        help="Height of the plume centre above sea level"),
    Key("GEOMETRY", "elev", "float", unit="degrees", low=0.0, high=90.0,
        low_open=True, geometries=_GEO_EM_SOLAR, required=_GEO_EM_SOLAR,
        geometry_defaults={SOLAR: 45.0},
        help="Elevation angle of the line of sight above the horizon"),
    Key("GEOMETRY", "obs_height", "float", unit="km", **_POSITIVE,
        geometries=_GEO_EM_SOLAR, required=_GEO_EM_SOLAR,
        help="Observer altitude above sea level"),
    Key("GEOMETRY", "atm_path", "path", geometries=_GEO_EM_SOLAR,
        required=_GEO_EM_SOLAR, must_exist="file", rfm_atm=True,
        help="RFM-format atmosphere profile (a bare name is also looked for in RFM's atm directory)"),

    # --- *GEOMETRY, emission -------------------------------------------------
    Key("GEOMETRY", "obs_temp", "float", None, unit="K", **_POSITIVE,
        geometries=_GEO_EMISSION,
        help="Observer-level ambient temperature",
        inert=_PLANNED_PROX),
    Key("GEOMETRY", "obs_rh", "float", None, unit="%", low=0.0, high=100.0,
        geometries=_GEO_EMISSION,
        help="Observer-level relative humidity",
        inert=_PLANNED_PROX),
    Key("GEOMETRY", "fit_tplume", "bool", False, geometries=_GEO_EMISSION,
        help="Fit the plume temperature (needs an explicit plume_temp)"),
    Key("GEOMETRY", "fit_tprox", "bool", True, geometries=_GEO_EMISSION,
        help="Fit the proximal layer temperature offset"),
    Key("GEOMETRY", "dt_prox", "float", 0.0, unit="K", geometries=_GEO_EMISSION,
        help="A priori proximal layer temperature offset"),
    Key("GEOMETRY", "fit_tdist", "bool", True, geometries=_GEO_EMISSION,
        help="Fit the distal layer temperature offset"),
    Key("GEOMETRY", "dt_dist", "float", 0.0, unit="K", geometries=_GEO_EMISSION,
        help="A priori distal layer temperature offset"),
    Key("GEOMETRY", "h2o_scaling", "int", 3, choices=(0, 1, 2, 3),
        geometries=_GEO_EMISSION,
        help="Number of atmospheric layers the shared H2O_sc parameter scales (0 disables it)"),
    Key("GEOMETRY", "model_aero_scattering", "bool", False, geometries=_GEO_EMISSION,
        help="Add the aerosol single-scattering source term to the plume"),
    Key("GEOMETRY", "fit_difference", "bool", False, geometries=_GEO_EMISSION,
        help="Fit plume minus clear sky (not supported in v3.0)"),
    Key("GEOMETRY", "use_bbt", "bool", False, geometries=_GEO_EMISSION,
        help="Convert spectra to brightness temperature before fitting"),
    Key("GEOMETRY", "bb_drift", "bool", True, geometries=_GEO_EMISSION,
        help="Correct for blackbody reference drift over time"),
    Key("GEOMETRY", "clear_drift", "bool", True, geometries=_GEO_EMISSION,
        help="Correct for clear-sky reference drift over time"),

        # --- *RETRIEVAL ----------------------------------------------------------
    Key("RETRIEVAL", "retrieval_method", "str", "nlls", choices=("nlls", "oe"),
        ignore_case=True,
        help="Retrieval backend: nlls (non-linear least squares) or oe (optimal estimation)"),
    Key("RETRIEVAL", "use_bounds", "bool", True,
        help="Apply parameter bounds during optimisation"),
    Key("RETRIEVAL", "update_params", "bool", False,
        help="Seed each fit with the previous spectrum's result (serial runs only)"),
    Key("RETRIEVAL", "fit_size_aero", "flags", True, choices=tuple(POSSIBLE_AEROSOLS),
        item="aerosol",
        help="Fit the aerosol effective radius (r0_SPECIES): True, False, or the "
             "aerosols whose size is fitted (the others held)"),
    Key("RETRIEVAL", "fit_aero_composition", "flags", False,
        choices=tuple(POSSIBLE_AEROSOLS), item="aerosol",
        help="Fit the aerosol composition (wt_SPECIES, H2SO4_aq only): True, False, "
             "or the aerosols whose composition is fitted"),
    Key("RETRIEVAL", "use_babs_aero", "bool", True,
        help="Use the aerosol absorption (Babs) rather than extinction (Bext)"),
    Key("RETRIEVAL", "fit_fov", "bool", True,
        help="Fit the instrument field of view"),
    Key("RETRIEVAL", "fov", "float", 0.02, unit="rad", low=0.0, high=1.0,
        low_open=True, high_open=True,
        help="A priori field of view"),
    Key("RETRIEVAL", "fit_max_opd", "bool", False,
        help="Fit the maximum optical path difference"),
    Key("RETRIEVAL", "max_opd", "float", 1.8, unit="cm", **_POSITIVE,
        none="read from the data files; 1.8 where they do not record it",
        help="Maximum optical path difference"),
    Key("RETRIEVAL", "apod_type", "str", "NB medium", choices=tuple(APOD_TYPES),
        none="read from the data files; NB medium where they do not record it",
        help="ILS apodization function"),
    Key("RETRIEVAL", "fit_shift", "bool", True,
        help="Fit the wavenumber shift"),
    Key("RETRIEVAL", "nu_shift", "float", 0.0, unit="cm⁻¹",
        help="A priori wavenumber shift"),
    Key("RETRIEVAL", "fit_offset", "bool", False,
        help="Fit an additive spectral offset"),
    Key("RETRIEVAL", "offset", "float", 0.0,
        help="A priori spectral offset"),

    # --- *PLOT ---------------------------------------------------------------
    Key("PLOT", "ratios", "list", None, item="ratio", none="no ratios",
        help="Species ratios y:x, e.g. HCl:SO2, CO2:SO2 (targets of active windows)"),
    Key("PLOT", "scroll", "int", 200, low=3.0,
        expect="an integer of at least 3 (a regression needs three points)",
        help="Number of latest spectra highlighted and regressed while a run is live"),
    Key("PLOT", "regression", "str", "siegel", choices=RATIO_REGRESSIONS,
        ignore_case=True, help="Ratio regression: Siegel repeated medians, Theil-Sen, or odr "
             "(York et al. 2004, weighted by both columns' fit errors)"),
    Key("PLOT", "rh", "float", None, unit="%", low=0.0, high=100.0, low_open=True,
        expect="a relative humidity in (0, 100] %", none="no H2O background",
        help="Relative humidity at collection, for the H2O background on ratio panels (layer)"),

    # --- *FITn ---------------------------------------------------------------
    Key("FIT", "fit", "bool", False,
        help="Include this window in the analysis"),
    Key("FIT", "fit_window", "window", unit="cm⁻¹", required=ALL,
        help="Spectral fit window: wn_start, wn_stop"),
    Key("FIT", "atm_gases", "list", None, choices=tuple(POSSIBLE_GASES),
        item="species", none="none",
        help="Atmospheric gases. A gas also in plume_gases is fitted twice (layer only): "
             "ambient part GAS_atm over pathlength at atm_temp, plume part GAS"),
    Key("FIT", "plume_gases", "list", None, choices=tuple(POSSIBLE_GASES),
        item="species", none="none",
        help="Plume gases. A gas also in atm_gases keeps its name for the plume part "
             "(targets, ratios) and gets GAS_atm for the ambient part (layer only)"),
    Key("FIT", "plume_aero", "list", None, choices=tuple(POSSIBLE_AEROSOLS),
        item="species", none="no aerosols",
        help="Plume aerosols"),
    Key("FIT", "targets", "list", None, item="name", none="none",
        help="Species and parameters reported as primary outputs (a species in one window only)"),
    Key("FIT", "polydeg", "int", 3, low=0.0, expect="a non-negative integer",
        help="Degree of the multiplicative background polynomial (polydeg + 1 "
             "coefficients; in v2 it was the coefficient count, so v2's 3 is v3's 2)"),
    Key("FIT", "continuum", "list", None, choices=tuple(CONTINUUM_GASES),
        item="continuum gas", none="continuum scaled linearly with the column",
        help="Gases whose continuum follows VMR on a 2-D (T, VMR) table (e.g. H2O). "
             "Every table includes RFM's continuum. For a gas in both roles, the "
             "ambient part only"),
    Key("FIT", "fit_gas_temp", "bool", None, geometries=_GEO_LAYER,
        none="inherit from *GEOMETRY",
        help="Per-window override: fit the plume gas temperature"),
    Key("FIT", "fit_atm_temp", "bool", None, geometries=_GEO_LAYER,
        none="inherit from *GEOMETRY",
        help="Per-window override: fit the atmospheric temperature"),
    Key("FIT", "fit_size_aero", "flags", None, choices=tuple(POSSIBLE_AEROSOLS),
        item="aerosol", none="inherit from *RETRIEVAL",
        help="Per-window override: fit the effective radius (r0_SPECIES) of every "
             "aerosol in this window (True / False) or of those named"),
    Key("FIT", "fit_aero_composition", "flags", None, choices=tuple(POSSIBLE_AEROSOLS),
        item="aerosol", none="inherit from *RETRIEVAL",
        help="Per-window override: fit the composition (wt_SPECIES, H2SO4_aq only) "
             "of every aerosol in this window (True / False) or of those named"),
    Key("FIT", "model_gas_emission", "bool", None, geometries=_GEO_LAYER,
        none="inherit from *GEOMETRY",
        help="Per-window override: model plume gas emission"),
    Key("FIT", "plume_temp_from", "str", None, geometries=_GEO_LAYER,
        none="the window's own gas_temp",
        help="Label of an earlier window fitting gas_temp (e.g. FIT0); its "
             "per-spectrum value is held fixed here"),
    Key("FIT", "atm_temp_from", "str", None, geometries=_GEO_LAYER,
        none="the window's own atm_temp",
        help="Label of an earlier window fitting atm_temp (e.g. FIT0); its "
             "per-spectrum value is held fixed here"),
)

# =============================================================================
# Name patterns
# =============================================================================

#: Keys whose name carries a parameter or species, by section.
PATTERNS: Mapping[str, KeyPattern] = MappingProxyType({
    "sigma_prior": KeyPattern(
        prefix="sigma_prior_",
        key=Key("RETRIEVAL", "sigma_prior_<param>", "float", None, **_POSITIVE,
                expect="a positive number (the prior 1-sigma of parameter '{name}')",
                help="OE prior 1-sigma of any parameter by name (e.g. sigma_prior_fov); "
                     "wins over a *DEFAULTS sigma_prior"),
        name_kind="parameter"),
    "background": KeyPattern(
        prefix="background_",
        key=Key("PLOT", "background_<gas>", "float", None, unit="ppmv", low=0.0,
                expect="a VMR in ppmv (>= 0)",
                none="standards.BACKGROUND_VMR (CO2 420, CO 0.1, CH4 1.9 ppmv)",
                help="Ambient VMR shown on ratio panels (layer)"),
        names=tuple(POSSIBLE_GASES), name_kind="gas"),
})

GAS, AEROSOL = "gas", "aerosol"

#: ``*DEFAULTS`` fields, written ``<SPECIES>_<field>``, by species kind.  They
#: override ``standards.GAS_DEFAULTS`` / ``AERO_DEFAULTS``; ``default`` is the
#: fallback for a species those tables do not list.
DEFAULTS_FIELDS: Mapping[str, Mapping[str, Key]] = MappingProxyType({
    GAS: MappingProxyType({k.name: k for k in (
        Key("DEFAULTS", "conc", "float", 1.0, unit="ppmv", **_POSITIVE,
            help="Reference concentration: the column the scaling factor is relative to"),
        Key("DEFAULTS", "min", "float", 0.0,
            help="Lower bound of the scaling factor (not for a 2-D continuum table)"),
        Key("DEFAULTS", "max", "float", None, none="unbounded",
            help="Upper bound of the scaling factor (not for a 2-D continuum table)"),
        Key("DEFAULTS", "sigma_prior", "float", None, **_POSITIVE,
            none="derived from the bounds", help="OE prior 1-sigma of the scaling factor"),
    )}),
    AEROSOL: MappingProxyType({k.name: k for k in (
        Key("DEFAULTS", "conc", "float", 1e-3, unit="g m⁻³", **_POSITIVE,
            help="Reference concentration: the mass column the scaling factor is relative to"),
        Key("DEFAULTS", "size", "float", 1.0, unit="µm", **_POSITIVE,
            help="A priori median radius r0 (clipped to the size bounds)"),
        Key("DEFAULTS", "sigma", "float", None, **_POSITIVE,
            inert="Not used in v3.0; kept because the repository configs set it",
            help="Geometric standard deviation of the log-normal size distribution"),
        Key("DEFAULTS", "min_size", "float", None, unit="µm", **_POSITIVE,
            none="lowest LUT radius", help="Lower bound of r0"),
        Key("DEFAULTS", "max_size", "float", None, unit="µm", **_POSITIVE,
            none="highest LUT radius", help="Upper bound of r0"),
        Key("DEFAULTS", "comp", "float", 0.75, low=0.0, high=1.0,
            help="A priori weight fraction (H2SO4; clipped to the LUT range)"),
        Key("DEFAULTS", "sigma_prior", "float", None, **_POSITIVE,
            none="derived from the bounds", help="OE prior 1-sigma of the mass scaling factor"),
    )}),
})

#: Every ``*DEFAULTS`` field name, longest first.  Order matters: field names
#: contain underscores, so 'max_size' must be tried before 'size'.
_FIELDS_BY_LENGTH: tuple[str, ...] = tuple(sorted(
    set(DEFAULTS_FIELDS[GAS]) | set(DEFAULTS_FIELDS[AEROSOL]), key=len, reverse=True))

#: Section header comments, for ``write_config`` and the reference.
SECTION_HELP: Mapping[str, str] = MappingProxyType({
    "DATA":      "Point to the data directory and specify file filtering",
    "MODEL":     "Forward model engine settings (RFM and mieEODG)",
    "GEOMETRY":  "Measurement geometry and geometry-specific forward model flags",
    "PREPROCESS": "What is done to the spectra before the fits (optional section)",
    "RETRIEVAL": "Retrieval method and parameters passed to the forward model",
    "DEFAULTS":  "Per-species prior overrides, over the built-in tables in standards.py",
    "PLOT":      "Gas ratios and live plotting (optional section)",
    "FIT":       "Fit window configuration (one *FIT0, *FIT1, ... section per window)",
})

# =============================================================================
# Lookup
# =============================================================================

def lookup(section: str, name: str) -> Key | None:
    """
    The :class:`Key` for ``name`` in ``section``, or the pattern's value spec.

    Parameters
    ----------
    section : str
        Section name (``'FIT'`` for any ``*FITn``).
    name : str
        Key name, any case.

    Returns
    -------
    Key or None
        ``None`` for a key the catalogue does not know.  ``*DEFAULTS`` keys
        are resolved with :func:`defaults_field`, not here.
    """
    key = KEYS.get((section, name.lower()))
    if key is not None:
        return key
    pattern = match_pattern(section, name)
    return pattern[0].key if pattern is not None else None

def match_pattern(section: str, name: str) -> tuple[KeyPattern, str] | None:
    """``(pattern, <name> part)`` for a pattern key of ``section``, else ``None``."""
    for pattern in PATTERNS.values():
        if pattern.key.section != section:
            continue
        part = pattern.match(name)
        if part is not None:
            return pattern, part
    return None

def section_keys(section: str) -> tuple[Key, ...]:
    """Every catalogued key of ``section``, in file order."""
    return tuple(k for (sec, _), k in KEYS.items() if sec == section)

def path_keys() -> tuple[Key, ...]:
    """Every path-valued key."""
    return tuple(k for k in KEYS.values() if k.is_path)

def default(section: str, name: str, geometry: str | None = None) -> object:
    """
    The default of ``section.name`` (in ``geometry`` where it differs).

    The config dataclasses take their field defaults from here.
    """
    return KEYS[(section, name)].default_for(geometry)

_SPECIES_BY_LOWER: Mapping[str, str] = MappingProxyType(
    {s.lower(): s for s in (*POSSIBLE_GASES, *POSSIBLE_AEROSOLS)})

def species_kind(species: str) -> str | None:
    """``'gas'``, ``'aerosol'`` or ``None`` for an unknown species."""
    if species in POSSIBLE_AEROSOLS:
        return AEROSOL
    if species in POSSIBLE_GASES:
        return GAS
    return None

def split_species_field(key: str) -> tuple[str, str] | None:
    """
    Split a ``*DEFAULTS`` key into ``(SPECIES, field)``.

    The boundary is found by matching ``field`` against the known field names,
    longest match first, rather than by splitting positionally on the last
    underscore.  A positional split reads ``ASH_max_size`` as species
    ``ASH_MAX`` field ``size``, which silently made ``min_size`` and
    ``max_size`` unreachable from config.

    The union of gas and aerosol fields is used for the split; whether the
    field is legal *for that species* is a separate check, so that a gas given
    ``max_size`` reports an unknown field rather than an unknown species.

    Parameters
    ----------
    key : str
        A ``*DEFAULTS`` key, e.g. ``'ash_max_size'``.

    Returns
    -------
    tuple[str, str] or None
        ``(SPECIES, field)``, the species in its registry spelling (matched
        case-blind, since the parser lower-cases keys: ``hcl_conc`` is HCl;
        upper-cased when unknown) and the field lower-cased, or *None* if the
        key ends in no recognised field name.
    """
    lowered = key.lower()
    for name in _FIELDS_BY_LENGTH:
        suffix = "_" + name
        if lowered.endswith(suffix) and len(key) > len(suffix):
            species = key[: -len(suffix)]
            return _SPECIES_BY_LOWER.get(species.lower(), species.upper()), name
    return None

def defaults_field(key: str) -> Key | None:
    """
    The value spec of a ``*DEFAULTS`` key.

    The species' own field where it has one; otherwise any field of that name
    (for the value type only: whether the field is legal for the species is
    the validator's check).
    """
    split = split_species_field(key)
    if split is None:
        return None
    species, name = split
    kind = species_kind(species)
    if kind is not None and name in DEFAULTS_FIELDS[kind]:
        return DEFAULTS_FIELDS[kind][name]
    return DEFAULTS_FIELDS[GAS].get(name) or DEFAULTS_FIELDS[AEROSOL][name]
