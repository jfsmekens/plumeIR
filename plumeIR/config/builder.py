"""
Config builder for plumeIR v3.

Constructs typed objects from a validated raw config dict.

Every section object takes its values, defaults and conversions from the key
catalogue (:mod:`plumeIR.config.keys`, M3d): a field of a config dataclass
with a catalogue key is filled from the config, or from the key's default for
the geometry, and converted by the key's type.  Only what is not one key's
value is written out here (fit window bounds, ratios, ``sigma_prior_*``,
``background_*``, whether ``max_opd`` / ``apod_type`` were set).

Three-tier species parameter priority (lowest → highest):
    1. ``standards.py``     — package-level baseline defaults
    2. ``*DEFAULTS``        — global overrides in this config file
    3. ``*FITn`` (future)   — reserved for per-window species overrides

The builder resolves the priority chain for every species parameter
before constructing the ParameterSet for each fit window.
"""

from __future__ import annotations

import logging
import math
from dataclasses import fields, replace
from pathlib import Path

import numpy as np

from plumeIR.exceptions import ConfigError
from plumeIR.config.keys import (
    AEROSOL, DEFAULTS_FIELDS, GAS, KEYS, PATTERNS, Key, split_species_field,
)
from plumeIR.config.validator import parse_ratio
from plumeIR.config.objects import (
    DataConfig,
    EmissionGeometry,
    FitWindowConfig,
    GeometryType,
    LayerGeometry,
    ModelConfig,
    PlotConfig,
    Retrieval,
    SolarGeometry,
)
from plumeIR import standards
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.standards import (
    AERO_DEFAULTS, GAS_DEFAULTS, ILS_DEFAULTS,
    ATM_TEMP_NODES, PLUME_TEMP_NODES, H2O_VMR_NODES, H2O_SC_NODES,
    plume_temp_nodes_for,
    AERO_R0_NODES, H2SO4_R0_NODES, H2SO4_WT_NODES,
    EMISSION_DISTAL_GASES, DISTAL_GAS_SC_BOUNDS,
)
from plumeIR.units import vmr_to_scd

logger = logging.getLogger(__name__)


# =============================================================================
# Top-level entry point
# =============================================================================

def _resolve_plume_temp(geometry):
    """
    Fill an unset ``EmissionGeometry.plume_temp`` from the atmosphere profile.

    An advected plume cools adiabatically to ambient within minutes, so the
    temperature at ``plume_height`` is the right a priori.  Resolved here, in
    the orchestrator, rather than in :func:`build_geometry` — that stays a pure
    constructor — so that every downstream consumer (parameter bounds, the
    forward model, the results layer) sees a float.

    A profile that cannot be read is left unresolved rather than raised on:
    ``build_emission_reference`` resolves it again from the atmosphere it loads
    anyway, and that is the authoritative path.
    """
    if not isinstance(geometry, EmissionGeometry) or geometry.plume_temp is not None:
        return geometry

    try:
        from rfmwrapper import readAtm
        atm  = readAtm(str(geometry.atm_path))
        temp = float(np.interp(
            geometry.plume_height,
            np.asarray(atm["HGT"], dtype=float),
            np.asarray(atm["TEM"], dtype=float),
        ))
    except Exception as exc:
        logger.debug(
            "Could not resolve plume_temp from '%s' (%s); deferring to the "
            "reference build", geometry.atm_path, exc,
        )
        return geometry

    logger.info("plume_temp not set; using ambient %.2f K at %.2f km",
                temp, geometry.plume_height)
    return replace(geometry, plume_temp=temp)


def build_from_config(path: str | Path) -> dict:
    """
    Parse, validate, and build all objects from a plumeIR config file.

    Reads the file with :func:`~plumeIR.config.parser.read_config` and hands
    the result to :func:`build_from_raw`, resolving relative paths against the
    config file's directory.

    Parameters
    ----------
    path : str or Path
        Path to the ``.ini`` config file.

    Returns
    -------
    dict
        As :func:`build_from_raw`.
    """
    from plumeIR.config.parser import read_config

    return build_from_raw(read_config(path), base_dir=Path(path).resolve().parent)


def build_from_raw(raw: dict, base_dir: str | Path | None = None) -> dict:
    """
    Validate and build all objects from a raw config dict.

    The in-memory counterpart of :func:`build_from_config`, for a client that
    holds the config as data (a GUI config editor) rather than as a file.

    Parameters
    ----------
    raw : dict
        Nested dict in the structure returned by
        :func:`~plumeIR.config.parser.read_config`: sections ``'DATA'``,
        ``'MODEL'``, ``'GEOMETRY'``, ``'RETRIEVAL'``, ``'DEFAULTS'`` and
        ``'fits'`` (a list of window dicts, each with a ``'_label'``).  Text
        values are converted by the catalogue's types first, as the parser
        would (:func:`~plumeIR.config.parser.normalise`).  Not modified.
    base_dir : str, Path or None
        Directory that relative paths in ``raw`` are resolved against
        (:func:`~plumeIR.config.paths.resolve_config_paths`).  ``None`` means
        the current working directory.

    Returns
    -------
    dict with keys:
        ``'data'``     : :class:`~plumeIR.config.objects.DataConfig`
        ``'model'``    : :class:`~plumeIR.config.objects.ModelConfig`
        ``'geometry'`` : LayerGeometry | EmissionGeometry | SolarGeometry
        ``'retrieval'``: :class:`~plumeIR.config.objects.Retrieval`
        ``'windows'``  : list of :class:`~plumeIR.config.objects.FitWindowConfig`
        ``'params'``   : list of :class:`~plumeIR.parameters.parameter.ParameterSet`,
                         one per active fit window (same order as ``'windows'``)
        ``'plot'``     : :class:`~plumeIR.config.objects.PlotConfig`

    Raises
    ------
    ConfigError
        If the config fails validation.
    """
    from plumeIR.config.parser import normalise
    from plumeIR.config.paths import resolve_config_paths
    from plumeIR.config.validator import validate

    base = Path(base_dir) if base_dir is not None else Path.cwd()
    raw = resolve_config_paths(normalise(raw), base)
    validate(raw)

    data     = build_data_config(raw.get("DATA", {}))
    model    = build_model(raw["MODEL"])
    geometry = _resolve_plume_temp(build_geometry(raw["GEOMETRY"]))
    gtype    = _geometry_type(raw["GEOMETRY"])
    retrieval = build_retrieval(raw["RETRIEVAL"], gtype)
    resolved_defaults = _resolve_defaults(raw.get("DEFAULTS", {}))

    windows: list[FitWindowConfig] = []
    params:  list[ParameterSet]    = []

    for fit_raw in raw["fits"]:
        if not fit_raw.get("fit"):
            continue
        window = build_fit_window(fit_raw)
        windows.append(window)
        params.append(
            build_parameter_set(window, geometry, retrieval, resolved_defaults)
        )
    params = _apply_sigma_priors(params, retrieval.sigma_prior)

    logger.info(
        "Config loaded: geometry=%s, method=%s, %d active window(s)",
        gtype.name, retrieval.retrieval_method, len(windows),
    )

    return {
        "data":      data,
        "model":     model,
        "geometry":  geometry,
        "retrieval": retrieval,
        "windows":   windows,
        "params":    params,
        "plot":      build_plot(raw.get("PLOT", {})),
    }


def _apply_sigma_priors(
    params: list[ParameterSet],
    sigma_prior: dict[str, float],
) -> list[ParameterSet]:
    """
    Apply the ``*RETRIEVAL`` ``sigma_prior_<name>`` keys to the parameter sets.

    These set the OE prior 1σ of any parameter by name, including those that
    are not species (``fov``, ``nu_shift``, ``poly0``), which the per-species
    ``sigma_prior`` field in ``*DEFAULTS`` cannot reach.  Where both name the
    same parameter, the ``*RETRIEVAL`` key wins.  Names match case-blind,
    because the parser lowercases keys (``sigma_prior_SO2`` arrives as
    ``so2``).

    Parameters
    ----------
    params : list of ParameterSet
        One per active window.
    sigma_prior : dict[str, float]
        Parameter name → prior 1σ, from ``Retrieval.sigma_prior``.

    Returns
    -------
    list of ParameterSet
        New sets with ``Parameter.sigma_prior`` replaced where named.

    Raises
    ------
    ConfigError
        If a name matches no parameter in any active window.
    """
    if not sigma_prior:
        return params
    wanted = {name.lower(): float(sigma) for name, sigma in sigma_prior.items()}
    known = {name.lower() for pset in params for name in pset.names()}
    unknown = sorted(set(wanted) - known)
    if unknown:
        raise ConfigError(
            f"RETRIEVAL: sigma_prior_ keys name no parameter of any active "
            f"window: {unknown}. Known parameters: "
            f"{sorted({n for pset in params for n in pset.names()})}"
        )
    return [
        ParameterSet([replace(p, sigma_prior=wanted[p.name.lower()])
                      if p.name.lower() in wanted else p for p in pset])
        for pset in params
    ]


# =============================================================================
# Section builders
# =============================================================================

def _convert(key: Key, value: object) -> object:
    """A validated config value as the type its key declares."""
    if key.type == "bool":
        return bool(value)
    if key.type == "int":
        return int(value)
    if key.type == "float":
        return float(value)
    if key.type == "path":
        return Path(value)
    if key.type == "str":
        return str(value).lower() if key.ignore_case else str(value)
    return value


def _key_value(key: Key, raw: dict, geometry: str | None = None) -> object:
    """
    The value of ``key`` from a raw section, else its default, converted.

    ``None`` and an empty path both mean "not set".
    """
    value = raw.get(key.name)
    if value is None or (key.is_path and value == ""):
        value = key.default_for(geometry)
    return None if value is None else _convert(key, value)


def _section_kwargs(section: str, raw: dict, cls: type,
                    geometry: str | None = None,
                    skip: tuple[str, ...] = ()) -> dict[str, object]:
    """
    Keyword arguments for config dataclass ``cls`` from a raw section.

    Every field of ``cls`` that has a catalogue key in ``section`` is filled
    by :func:`_key_value`, except those in ``skip``; other fields are left to
    the caller.
    """
    return {f.name: _key_value(KEYS[(section, f.name)], raw, geometry)
            for f in fields(cls)
            if (section, f.name) in KEYS and f.name not in skip}


def build_data_config(raw: dict) -> DataConfig:
    """Build :class:`DataConfig` from the raw *DATA section dict."""
    return DataConfig(**_section_kwargs("DATA", raw, DataConfig))


def build_plot(raw: dict) -> PlotConfig:
    """Build :class:`PlotConfig` from the raw (optional) *PLOT section dict."""
    entries = raw.get("ratios")
    if entries is None:
        entries = []
    elif isinstance(entries, str):
        entries = [entries]
    pattern = PATTERNS["background"]
    background: dict[str, float] = {}
    for name, value in raw.items():
        part = pattern.match(name)
        if part is not None:
            # Keys arrive lower-cased (background_co2); restore the registry name.
            background[pattern.canonical(part)] = float(value)
    return PlotConfig(
        ratios     = tuple(parse_ratio(r) for r in entries),
        background = background,
        **_section_kwargs("PLOT", raw, PlotConfig, skip=("ratios",)),
    )


def build_model(raw: dict) -> ModelConfig:
    """Build :class:`ModelConfig` from the raw *MODEL section dict."""
    return ModelConfig(**_section_kwargs("MODEL", raw, ModelConfig))


_GEOMETRY_CLASSES = {
    GeometryType.LAYER:    (LayerGeometry, "layer"),
    GeometryType.EMISSION: (EmissionGeometry, "emission"),
    GeometryType.SOLAR:    (SolarGeometry, "solar"),
}


def _geometry_type(raw: dict) -> GeometryType:
    gtype = str(raw.get("type", "")).lower()
    return {
        "layer":    GeometryType.LAYER,
        "emission": GeometryType.EMISSION,
        "solar":    GeometryType.SOLAR,
    }[gtype]


def build_geometry(
    raw: dict,
) -> LayerGeometry | EmissionGeometry | SolarGeometry:
    """Build the appropriate geometry dataclass from the raw *GEOMETRY dict."""
    try:
        cls, name = _GEOMETRY_CLASSES[_geometry_type(raw)]
    except KeyError:
        raise ConfigError(f"Unknown geometry type: {raw.get('type')}") from None
    return cls(**_section_kwargs("GEOMETRY", raw, cls, name))


def build_retrieval(raw: dict, geometry_type: GeometryType) -> Retrieval:
    """Build :class:`Retrieval` from the raw *RETRIEVAL dict."""
    pattern = PATTERNS["sigma_prior"]
    sigma_prior: dict[str, float] = {}
    for key, val in raw.items():
        param_name = pattern.match(key)
        if param_name is not None and isinstance(val, (int, float)):
            sigma_prior[param_name] = float(val)

    return Retrieval(
        geometry_type      = geometry_type,
        # Whether the config set these: when not, the data file's values win
        # (spectra.resolve_instrument_ils).
        max_opd_explicit   = raw.get("max_opd")   is not None,
        apod_type_explicit = raw.get("apod_type") is not None,
        sigma_prior        = sigma_prior,
        **_section_kwargs("RETRIEVAL", raw, Retrieval),
    )


def build_fit_window(raw: dict) -> FitWindowConfig:
    """Build :class:`FitWindowConfig` from a raw *FITn section dict."""

    def _to_tuple(value: object) -> tuple[str, ...]:
        if value is None:
            return ()
        if isinstance(value, str):
            return (value,)
        return tuple(str(s) for s in value)

    fw = raw["fit_window"]
    return FitWindowConfig(
        label              = raw.get("_label", "FIT?"),
        wn_start           = float(fw[0]),
        wn_stop            = float(fw[1]),
        atm_gases          = _to_tuple(raw.get("atm_gases")),
        plume_gases        = _to_tuple(raw.get("plume_gases")),
        plume_aero         = _to_tuple(raw.get("plume_aero")),
        targets            = _to_tuple(raw.get("targets")),
        polydeg            = _key_value(KEYS[("FIT", "polydeg")], raw),
        fit_gas_temp       = raw.get("fit_gas_temp"),
        fit_atm_temp       = raw.get("fit_atm_temp"),
        model_gas_emission = raw.get("model_gas_emission"),
        continuum_gases    = _to_tuple(raw.get("continuum")),
        plume_temp_from    = (str(raw["plume_temp_from"])
                              if raw.get("plume_temp_from") is not None else None),
        atm_temp_from      = (str(raw["atm_temp_from"])
                              if raw.get("atm_temp_from") is not None else None),
    )


# =============================================================================
# Parameter resolution helpers
# =============================================================================

def _resolve_defaults(raw_defaults: dict) -> dict[str, dict[str, object]]:
    """
    Flatten the ``*DEFAULTS`` section into a nested dict:
        ``{SPECIES: {field: value, …}, …}``

    Keys use the ``{SPECIES}_{field}`` format.  The species/field boundary is
    found by :func:`plumeIR.config.keys.split_species_field`, which is
    also what the validator uses, so the two cannot disagree about where a key
    divides.

    Parameters
    ----------
    raw_defaults : dict
        The parsed ``*DEFAULTS`` section, keys lower-cased by the parser.

    Returns
    -------
    dict[str, dict[str, object]]
        Species (upper-case) → field (lower-case) → value.

    Raises
    ------
    ConfigError
        If a key does not end in a recognised field name.  Unknown keys are
        rejected rather than ignored: a silently dropped override is the exact
        failure this function used to produce.
    """
    resolved: dict[str, dict[str, object]] = {}
    for key, value in raw_defaults.items():
        split = split_species_field(key)
        if split is None:
            raise ConfigError(
                f"Unrecognised *DEFAULTS key '{key}': it ends in no known "
                f"field name. Expected {{SPECIES}}_{{field}}."
            )
        species, field = split
        resolved.setdefault(species, {})[field] = value
    return resolved


def _fallback(kind: str, field: str) -> object:
    """
    Last-resort value of a ``*DEFAULTS`` field, for a species that neither the
    config nor the ``standards`` table sets (``keys.DEFAULTS_FIELDS``).
    """
    return DEFAULTS_FIELDS[kind][field].default


def _species_param(
    species: str,
    field: str,
    window_overrides: dict,
    global_defaults: dict,
    standards_dict: dict,
    fallback: object = None,
) -> object:
    """
    Resolve a single species parameter using the three-tier priority chain.

    Priority (highest wins):
        window_overrides  → global_defaults → standards_dict → fallback
    """
    # Tier 1 placeholder: per-window species overrides (not yet implemented)
    val = window_overrides.get(species, {}).get(field)
    if val is not None:
        return val
    # Tier 2: *DEFAULTS section
    val = global_defaults.get(species, {}).get(field)
    if val is not None:
        return val
    # Tier 3: standards.py
    val = standards_dict.get(species, {}).get(field)
    if val is not None:
        return val
    return fallback


def _ref_conditions(
    geometry: LayerGeometry | EmissionGeometry | SolarGeometry,
) -> tuple[float, float, float, float, float, float]:
    """
    Return reference conditions for SCD conversion:
    ``(atm_path, atm_temp, atm_pres, plume_path, plume_temp, plume_pres)``.

    The atmospheric gas fills the full observer→source path, so its
    reference column uses ``pathlength``.  The plume gas occupies only the
    plume layer, so its reference column uses ``plume_thickness`` — this
    keeps the plume as a physically separate layer with its own temperature
    and (for the eventual SCD→concentration conversion) its own thickness.

    For :class:`LayerGeometry` these come directly from the geometry.  For
    emission/solar geometries — whose reference columns are handled
    differently (Phase 3b/4b) — nominal values are returned so the
    ParameterSet can still be constructed.
    """
    if isinstance(geometry, LayerGeometry):
        return (
            geometry.pathlength,      geometry.atm_temp,   geometry.atm_pres,
            geometry.plume_thickness, geometry.plume_temp, geometry.plume_pres,
        )
    # Placeholder for non-layer geometries (revisited in Phase 3b/4b)
    return (1.0, 288.0, 1013.0, 0.05, 288.0, 1013.0)


def _make_gas_param(
    species:      str,
    kind:         ParameterKind,
    init_scaling: float,
    temp:         float,
    pres:         float,
    pathlength:   float,
    sp_fn,
    continuum:    bool = False,
) -> Parameter:
    """
    Build a gas :class:`Parameter` under the scaling-factor convention.

    The parameter VALUE is a dimensionless scaling factor (actual_SCD /
    ref_SCD).  ``ref_column`` holds the reference SCD [molec/cm²], derived
    from the reference concentration via :func:`vmr_to_scd`.

    Bounds default to ``(0, +inf)`` — Beer-Lambert is linear, so any column
    amount is physical.  Explicit ``min``/``max`` in ``*DEFAULTS`` override
    this.  H2O is special: as a 2-D (T, VMR) LUT species, its scaling maps
    to an absolute VMR that must stay within the LUT VMR node range, so its
    bounds are ``(vmr_min / ref_vmr, vmr_max / ref_vmr)``.
    """
    ref_vmr = float(sp_fn(species, "conc", GAS_DEFAULTS, _fallback(GAS, "conc")))   # [ppmv]
    ref_scd = float(vmr_to_scd(ref_vmr, pathlength, temp=temp, pres=pres))
    sigma   = sp_fn(species, "sigma_prior", GAS_DEFAULTS, _fallback(GAS, "sigma_prior"))

    if continuum:
        # H2O scaling bound constrained by the LUT VMR range (2-D continuum
        # LUT in layer/emission geometry).
        lo = float(H2O_VMR_NODES[0])  / ref_vmr
        hi = float(H2O_VMR_NODES[-1]) / ref_vmr
        init = float(min(max(init_scaling, lo), hi))
    else:
        lo = float(sp_fn(species, "min", GAS_DEFAULTS, _fallback(GAS, "min")))
        hi = sp_fn(species, "max", GAS_DEFAULTS, _fallback(GAS, "max"))
        hi = math.inf if hi is None else float(hi)
        init = init_scaling

    return Parameter(
        name        = species,
        value       = float(init),
        vary        = True,
        bounds      = (lo, hi),
        kind        = kind,
        ref_column  = ref_scd,
        sigma_prior = float(sigma) if sigma is not None else None,
    )


def build_parameter_set(
    window: FitWindowConfig,
    geometry: LayerGeometry | EmissionGeometry | SolarGeometry,
    retrieval: Retrieval,
    resolved_defaults: dict[str, dict],
) -> ParameterSet:
    """
    Build a :class:`ParameterSet` for one fit window.

    Parameter order matches the forward model convention:
        polynomial → atm gases → plume gases → aerosols → spectral → temperature

    Per-window flags override geometry-level flags where applicable.
    """
    params: list[Parameter] = []

    def _sp(species: str, field: str, standards: dict, fallback: object = None):
        return _species_param(species, field, {}, resolved_defaults, standards, fallback)

    # ------------------------------------------------------------------
    # 1. Polynomial background
    # ------------------------------------------------------------------
    # Background polynomial carries the source × instrument continuum, whose
    # scale is set by the spectrum's native units (single-beam counts, radiance,
    # or transmittance).  It is therefore UNBOUNDED — bounding it would clamp it
    # to the wrong magnitude — and ``poly0`` is seeded per spectrum from the
    # measured continuum at fit time (see retrieval.run).  Normalising the
    # spectrum is not an option: it is invalid for radiance/transmittance modes.
    for i in range(window.polydeg + 1):
        params.append(Parameter(
            name       = f"poly{i}",
            value      = 1.0 if i == 0 else 0.0,
            vary       = True,
            bounds     = (float("-inf"), float("inf")),
            kind       = ParameterKind.POLYNOMIAL,
        ))

    # Reference conditions for SCD conversion (layer geometry only;
    # emission/solar reference columns are handled in Phase 3b/4b).
    (atm_path, atm_temp, atm_pres,
     plume_path, plume_temp, plume_pres) = _ref_conditions(geometry)

    # The 2-D (T, VMR) table is a per-window choice (window.continuum_gases),
    # geometry-agnostic; window.uses_2d resolves it per role (a dual gas's
    # plume part stays 1-D).  Every table includes RFM's continuum either way.

    # ------------------------------------------------------------------
    # 2. Atmospheric gases  (scaling factor around 1.0; full path)
    #
    # NOT for the emission geometry.  There the atmospheric column is carried
    # by the three AtmLayerLUTs: H2O by H2O_sc across all layers, everything
    # else fixed by the profile (bar the distal O3 term added in section 6).
    # `window.atm_gases` still selects which gases enter the RFM layer runs.
    # These parameters were previously built for emission and never read by
    # EmissionForwardModel — five of sixteen free parameters with identically
    # zero Jacobian columns, reporting the prior as if it were retrieved.
    # ------------------------------------------------------------------
    if not isinstance(geometry, EmissionGeometry):
        for species in window.atm_gases:
            # A dual gas (also a plume gas, 14b) is named {GAS}_atm here; its
            # reference column is the same ambient one, from the gas's defaults.
            params.append(replace(_make_gas_param(
                species, kind=ParameterKind.GAS_ATM, init_scaling=1.0,
                temp=atm_temp, pres=atm_pres, pathlength=atm_path, sp_fn=_sp,
                continuum=window.uses_2d(species, plume=False),
            ), name=window.atm_name(species)))

    # ------------------------------------------------------------------
    # 3. Plume gases  (scaling factor around 0.01; plume thickness only)
    # ------------------------------------------------------------------
    for species in window.plume_gases:
        params.append(_make_gas_param(
            species, kind=ParameterKind.GAS_PLUME, init_scaling=0.01,
            temp=plume_temp, pres=plume_pres, pathlength=plume_path, sp_fn=_sp,
            continuum=window.uses_2d(species, plume=True),
        ))

    # ------------------------------------------------------------------
    # 4. Plume aerosols
    #    - mass scaling: dimensionless, bounds (0, +inf)
    #    - r0_{SPECIES}: effective radius, bounds STRICTLY from LUT r0 nodes
    #    - wt_{SPECIES}: weight fraction (H2SO4 only), bounds from LUT wt nodes
    # ------------------------------------------------------------------
    for species in window.plume_aero:
        size = _sp(species, "size", AERO_DEFAULTS, _fallback(AEROSOL, "size"))
        sp   = _sp(species, "sigma_prior", AERO_DEFAULTS, _fallback(AEROSOL, "sigma_prior"))

        # Reference mass column [g/m²] = reference concentration [g/m³] ×
        # plume thickness [m].  Mirrors the gas ref_SCD convention so the
        # scaling factor (=1 at reference) maps to a fixed concentration.
        ref_conc = float(_sp(species, "conc", AERO_DEFAULTS, _fallback(AEROSOL, "conc")))   # [g/m³]
        ref_mass_col = ref_conc * plume_path * 1e3                     # [g/m²]

        # Mass column scaling factor — unbounded above, ≥ 0.  Starts at 1.0.
        params.append(Parameter(
            name        = species,
            value       = 1.0,
            vary        = True,
            bounds      = (0.0, math.inf),
            kind        = ParameterKind.AERO_PLUME,
            ref_column  = ref_mass_col,
            sigma_prior = float(sp) if sp is not None else None,
        ))

        # Effective radius — the LUT node range intersected with the species'
        # physical size range (`min_size`/`max_size`).
        #
        # The LUT span is a hard ceiling: outside it the query clamps, so a
        # fitted r0 beyond it is meaningless.  But the LUT span alone is too
        # permissive as a *fit* bound.  H2SO4's extinction goes grey once the
        # size parameter x = 2*pi*r/lambda >> 1 (r ~ 3-5 um in the TIR), and a
        # featureless continuum is degenerate with dt_dist and the background
        # polynomial.  Measured on 2019-09-11 with r0 free to 10 um: one
        # spectrum retrieved H2SO4 = 79.3 +- 51.5 with r0 railed at 10 and
        # dt_dist railed at +20 K, at an R2 (0.99829) BETTER than the series
        # median — a degeneracy the fit slides along, not a failed fit.
        # dt_dist hit its bound in 2/50 spectra with aerosols vs 0/50 without.
        r0_nodes = H2SO4_R0_NODES if species == "H2SO4" else AERO_R0_NODES
        r0_lo = max(float(r0_nodes[0]),
                    float(_sp(species, "min_size", AERO_DEFAULTS, r0_nodes[0])))
        r0_hi = min(float(r0_nodes[-1]),
                    float(_sp(species, "max_size", AERO_DEFAULTS, r0_nodes[-1])))
        if not r0_lo < r0_hi:
            raise ConfigError(
                f"{species}: min_size ({r0_lo}) must be below max_size "
                f"({r0_hi}) after intersecting with the LUT range "
                f"[{r0_nodes[0]:.3g}, {r0_nodes[-1]:.3g}] um"
            )
        r0_init = float(min(max(size, r0_lo), r0_hi))
        params.append(Parameter(
            name   = f"r0_{species}",
            value  = r0_init,
            vary   = retrieval.fit_size_aero,
            bounds = (r0_lo, r0_hi),
            kind   = ParameterKind.AERO_PLUME,
        ))

        # Composition (H2SO4 only) — the 2-D LUT always covers weight
        # fraction; this parameter is varied only if fit_aero_composition.
        if species == "H2SO4":
            wt_lo, wt_hi = float(H2SO4_WT_NODES[0]), float(H2SO4_WT_NODES[-1])
            wt_init = _sp(species, "comp", AERO_DEFAULTS, _fallback(AEROSOL, "comp"))
            wt_init = float(min(max(wt_init, wt_lo), wt_hi))
            params.append(Parameter(
                name   = f"wt_{species}",
                value  = wt_init,
                vary   = retrieval.fit_aero_composition,
                bounds = (wt_lo, wt_hi),
                kind   = ParameterKind.AERO_PLUME,
            ))

    # ------------------------------------------------------------------
    # 5. Spectral / ILS parameters
    # ------------------------------------------------------------------
    def _ils(key: str) -> dict:
        return ILS_DEFAULTS[key]

    if retrieval.fit_fov:
        params.append(Parameter(
            name   = "fov",
            value  = retrieval.fov,
            vary   = True,
            bounds = (_ils("fov")["min"], _ils("fov")["max"]),
            kind   = ParameterKind.SPECTRAL,
        ))
    else:
        params.append(Parameter(
            name   = "fov",
            value  = retrieval.fov,
            vary   = False,
            bounds = (_ils("fov")["min"], _ils("fov")["max"]),
            kind   = ParameterKind.SPECTRAL,
        ))

    if retrieval.fit_max_opd:
        params.append(Parameter(
            name   = "max_opd",
            value  = retrieval.max_opd,
            vary   = True,
            bounds = (_ils("max_opd")["min"], _ils("max_opd")["max"]),
            kind   = ParameterKind.SPECTRAL,
        ))

    if retrieval.fit_shift:
        params.append(Parameter(
            name   = "nu_shift",
            value  = retrieval.nu_shift,
            vary   = True,
            bounds = (_ils("nu_shift")["min"], _ils("nu_shift")["max"]),
            kind   = ParameterKind.SPECTRAL,
        ))

    if retrieval.fit_offset:
        params.append(Parameter(
            name   = "offset",
            value  = retrieval.offset,
            vary   = True,
            bounds = (_ils("offset")["min"], _ils("offset")["max"]),
            kind   = ParameterKind.SPECTRAL,
        ))

    # ------------------------------------------------------------------
    # 6. Temperature parameters (geometry-specific)
    # ------------------------------------------------------------------
    def _td(key: str) -> dict:
        # Looked up on the module at call time, not bound at import, so that
        # standards.plume_temp_nodes_for (which also reads it at call time)
        # and the bounds here always see the same table.
        return standards.TEMP_DEFAULTS[key]

    # Resolve per-window overrides for temperature fit flags
    fit_gas_temp = (
        window.fit_gas_temp
        if window.fit_gas_temp is not None
        else getattr(geometry, "fit_gas_temp", False)
    )
    fit_atm_temp = (
        window.fit_atm_temp
        if window.fit_atm_temp is not None
        else getattr(geometry, "fit_atm_temp", False)
    )

    # Temperature parameters that INDEX a gas LUT (gas_temp, atm_temp,
    # gasE_temp) get bounds STRICTLY from the LUT node range — no
    # extrapolation.  source_temp / dt_* do not index a LUT and keep their
    # TEMP_DEFAULTS bounds.
    plume_t_lo, plume_t_hi = float(PLUME_TEMP_NODES[0]), float(PLUME_TEMP_NODES[-1])
    atm_t_lo,   atm_t_hi   = float(ATM_TEMP_NODES[0]),   float(ATM_TEMP_NODES[-1])

    if isinstance(geometry, LayerGeometry):
        # Per-window override for model_gas_emission
        model_gas_emission = (
            window.model_gas_emission
            if window.model_gas_emission is not None
            else geometry.model_gas_emission
        )

        if window.plume_temp_from is not None:
            # Staged plume temperature: a FIXED gas_temp whose
            # value retrieval.run replaces per spectrum with the source window's
            # fit.  Fixed rather than absent so the forward reads it like any
            # other value and the results record the temperature actually used.
            # An inherited GEOMETRY.fit_gas_temp is overridden here; an explicit
            # per-window fit_gas_temp: True is refused by the validator.
            params.append(Parameter(
                name   = "gas_temp",
                value  = float(min(max(geometry.plume_temp, plume_t_lo), plume_t_hi)),
                vary   = False,
                bounds = (plume_t_lo, plume_t_hi),
                kind   = ParameterKind.TEMPERATURE,
            ))
        elif fit_gas_temp:
            params.append(Parameter(
                name   = "gas_temp",
                value  = float(min(max(geometry.plume_temp, plume_t_lo), plume_t_hi)),
                vary   = True,
                bounds = (plume_t_lo, plume_t_hi),
                kind   = ParameterKind.TEMPERATURE,
            ))
        if window.atm_temp_from is not None or fit_atm_temp:
            # Staged ambient temperature, as gas_temp above: FIXED, its value
            # replaced per spectrum by retrieval.run with the source window's
            # fit; an inherited GEOMETRY.fit_atm_temp is overridden.
            params.append(Parameter(
                name   = "atm_temp",
                value  = float(min(max(geometry.atm_temp, atm_t_lo), atm_t_hi)),
                vary   = window.atm_temp_from is None,
                bounds = (atm_t_lo, atm_t_hi),
                kind   = ParameterKind.TEMPERATURE,
            ))
        # gasE_temp: separate free parameter controlling plume self-emission
        # level (Planck term + emission OD). Indexes the gas LUT, so bounds
        # come from the plume temperature node range.
        if model_gas_emission:
            params.append(Parameter(
                name   = "gasE_temp",
                value  = float(min(max(geometry.plume_temp, plume_t_lo), plume_t_hi)),
                vary   = True,
                bounds = (plume_t_lo, plume_t_hi),
                kind   = ParameterKind.TEMPERATURE,
            ))
        if geometry.fit_source_temp:
            params.append(Parameter(
                name   = "source_temp",
                value  = geometry.source_temp,
                vary   = True,
                bounds = (_td("source_temp")["min"], _td("source_temp")["max"]),
                kind   = ParameterKind.TEMPERATURE,
            ))

    elif isinstance(geometry, EmissionGeometry):
        # H2O_sc: shared background-atmosphere H2O column scaling.  Indexes
        # the 1-D atmospheric-layer LUTs → strict bounds from H2O_SC_NODES.
        if geometry.h2o_scaling > 0:
            h2o_lo, h2o_hi = float(H2O_SC_NODES[0]), float(H2O_SC_NODES[-1])
            params.append(Parameter(
                name   = "H2O_sc",
                value  = float(min(max(1.0, h2o_lo), h2o_hi)),
                vary   = True,
                bounds = (h2o_lo, h2o_hi),
                kind   = ParameterKind.ATMOSPHERE,
            ))

        # Independent distal column scaling for gases whose absorption is
        # essentially stratospheric, so H2O_sc cannot reach it.  Applied
        # additively in OD space by EmissionForwardModel; see
        # standards.EMISSION_DISTAL_GASES for the measured justification.
        for species in window.atm_gases:
            if species not in EMISSION_DISTAL_GASES:
                continue
            lo, hi = DISTAL_GAS_SC_BOUNDS
            params.append(Parameter(
                name   = f"{species}_sc",
                value  = 1.0,
                vary   = True,
                bounds = (float(lo), float(hi)),
                kind   = ParameterKind.ATMOSPHERE,
            ))

        # Plume temperature, fitted DIRECTLY (no offset parameter).  It drives
        # absorption and emission together, so unlike a pure emission-level
        # offset it is not exactly degenerate with the plume column.  Bounds
        # sit about the ambient value already resolved into geometry.plume_temp.
        if geometry.fit_tplume:
            if geometry.plume_temp is None:
                raise ConfigError(
                    "GEOMETRY.fit_tplume is True but plume_temp is not set. "
                    "The fit bounds are defined relative to the a priori, so it "
                    "must be given explicitly (leaving it unset means 'ambient "
                    "from the profile', which is only resolved at reference-build "
                    "time and cannot be fitted)."
                )
            t_amb = float(geometry.plume_temp)
            spec  = _td("plume_temp")
            # The same axis the reference build uses, so the bounds are always
            # inside the table that will be queried.
            axis = plume_temp_nodes_for(geometry)
            lo = max(t_amb - spec["below"], float(axis[0]))
            hi = min(t_amb + spec["above"], float(axis[-1]))
            params.append(Parameter(
                name   = "plume_temp",
                value  = t_amb,
                vary   = True,
                bounds = (lo, hi),
                kind   = ParameterKind.TEMPERATURE,
            ))
        # Proximal / distal layer temperature offsets (applied analytically
        # to each layer's effective emission temperature T_eff)
        if geometry.fit_tprox:
            params.append(Parameter(
                name   = "dt_prox",
                value  = geometry.dt_prox,
                vary   = True,
                bounds = (_td("dt_prox")["min"], _td("dt_prox")["max"]),
                kind   = ParameterKind.TEMPERATURE,
            ))
        if geometry.fit_tdist:
            params.append(Parameter(
                name   = "dt_dist",
                value  = geometry.dt_dist,
                vary   = True,
                bounds = (_td("dt_dist")["min"], _td("dt_dist")["max"]),
                kind   = ParameterKind.TEMPERATURE,
            ))

    return ParameterSet(params)
