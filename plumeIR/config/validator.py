"""
Config validator for plumeIR v3.

Validates the raw dict from ``parser.py`` and raises ``ConfigError``
listing *all* errors found rather than stopping at the first one.

Per-key checks (type, range, choices, required per geometry, files that must
exist) come from the key catalogue, :mod:`plumeIR.config.keys`.  Rules that involve more than one key stay as code here: active windows,
targets (valid names, one window per species), gases in both roles,
``continuum`` against the window's gases, ``plume_temp_from`` and
``atm_temp_from``, ratio pairs and
``no_gas``.

Keys the catalogue does not know, and keys the geometry does not read, are
logged as warnings and otherwise ignored.
"""

from __future__ import annotations

import logging
from pathlib import Path

from plumeIR.config.keys import (
    ALL, DEFAULTS_FIELDS, GEOMETRIES, KEYS, Key, lookup, match_pattern, section_keys,
    species_kind, split_species_field,
)
from plumeIR.config.objects import DUAL_ATM_SUFFIX
from plumeIR.constants import POSSIBLE_AEROSOLS, POSSIBLE_GASES
from plumeIR.exceptions import ConfigError

logger = logging.getLogger(__name__)

__all__ = ["validate", "parse_ratio", "split_species_field"]

# Targets may include species names or diagnostic parameter names
_VALID_TARGET_EXTRAS = {
    "gas_temp", "atm_temp", "source_temp",
    "dt_prox",
    "fov", "max_opd", "nu_shift", "offset",
}


def _e(errors: list[str], msg: str) -> None:
    errors.append(msg)


def _check_value(errors: list[str], where: str, key: Key, val: object,
                 name: str = "") -> None:
    """
    Check one value against its catalogue entry; ``None`` always passes.

    Parameters
    ----------
    errors : list[str]
        Accumulated error messages (appended to).
    where : str
        ``SECTION.key`` (or ``FITn.key``) for the message.
    key : Key
        The value specification.
    val : object
        The value as parsed.
    name : str
        The ``<name>`` part of a pattern key, for its message.
    """
    if val is None:
        return
    bad = f"{where}: must be {key.expectation(name)}, got {val!r}"
    kind = key.type
    if kind == "bool":
        if not isinstance(val, bool):
            _e(errors, f"{where}: expected True or False, got {val!r}")
        return
    if kind == "window":
        _check_window(errors, where, val)
        return
    if kind == "list":
        items = [val] if isinstance(val, str) else val
        if not isinstance(items, (list, tuple)) or \
                not all(isinstance(v, str) for v in items):
            _e(errors, f"{where}: expected {key.expectation(name)}, got {val!r}")
            return
        if key.choices is not None:
            unknown = [v for v in items if v not in key.choices]
            if unknown:
                valid = (f"; valid: {list(key.choices)}"
                         if len(key.choices) <= 12 else "")
                _e(errors, f"{where}: unknown {key.item} {unknown}{valid}")
        return
    if kind == "path":
        if not isinstance(val, (str, Path)):
            _e(errors, f"{where}: expected a path string, got {val!r}")
        return
    if kind in ("int", "float"):
        is_int = isinstance(val, int) and not isinstance(val, bool)
        ok = is_int or (kind == "float" and isinstance(val, float))
        if not ok or not _within(key, val):
            _e(errors, bad)
            return
    elif kind == "str" and (isinstance(val, (bool, list, tuple, dict))):
        _e(errors, bad)
        return
    if key.choices is not None:
        probe = str(val).lower() if key.ignore_case else val
        allowed = ([str(c).lower() for c in key.choices]
                   if key.ignore_case else list(key.choices))
        if probe not in allowed:
            _e(errors, f"{where}: expected one of {list(key.choices)}, got {val!r}")


def _within(key: Key, val: float) -> bool:
    """Whether a number lies within the key's limits."""
    if key.low is not None and (val <= key.low if key.low_open else val < key.low):
        return False
    if key.high is not None and (val >= key.high if key.high_open else val > key.high):
        return False
    return True


def _check_window(errors: list[str], where: str, fw: object) -> None:
    """A fit window: two numbers, increasing, positive."""
    if isinstance(fw, (list, tuple)) and len(fw) == 2:
        lo, hi = fw
        numbers = all(isinstance(v, (int, float)) and not isinstance(v, bool)
                      for v in (lo, hi))
        if not numbers:
            _e(errors, f"{where}: both bounds must be numbers")
        elif lo >= hi:
            _e(errors, f"{where}: lower bound {lo} >= upper bound {hi}")
        elif lo <= 0:
            _e(errors, f"{where}: wavenumbers must be positive")
    else:
        _e(errors, f"{where}: expected [wn_start, wn_stop], got {fw!r}")


def _check_section(errors: list[str], raw: dict, section: str, label: str,
                   gtype: str | None, check_required: bool = True) -> None:
    """
    Run the catalogue checks over one section.

    Parameters
    ----------
    errors : list[str]
        Accumulated error messages (appended to).
    raw : dict
        The section as parsed.
    section : str
        Catalogue section (``'FIT'`` for a window).
    label : str
        Name used in messages (the section, or the window's label).
    gtype : str or None
        Geometry type, or ``None`` when it is not valid: every key is then
        checked, and only keys required in every geometry are required.
    check_required : bool
        Report required keys that are missing.
    """
    for name, val in raw.items():
        if name == "_label":
            continue
        where = f"{label}.{name}"
        key = lookup(section, name)
        if key is None:
            logger.warning("%s: unknown key, ignored", where)
            continue
        if gtype is not None and not key.used_in(gtype):
            if val is not None:
                logger.warning("%s: not used by the %s geometry, ignored",
                               where, gtype)
            continue
        part = ""
        matched = (None if (section, name.lower()) in KEYS
                   else match_pattern(section, name))
        if matched is not None:
            pattern, part = matched
            if pattern.canonical(part) is None:
                _e(errors, f"{where}: unknown {pattern.name_kind} '{part}'")
                continue
        _check_value(errors, where, key, val, part)
        if key.must_exist and isinstance(val, (str, Path)) and not Path(val).exists():
            _e(errors, f"{where}: {key.must_exist} not found: {val}")
    if not check_required:
        return
    for key in section_keys(section):
        needed = (gtype in key.required if gtype is not None
                  else key.required == ALL)
        if needed and raw.get(key.name) is None:
            _e(errors, f"{label}.{key.name}: required field is missing")


def _check_defaults(errors: list[str], defaults: dict) -> None:
    """``*DEFAULTS``: ``SPECIES_field`` names, then each value."""
    fields = sorted(set(DEFAULTS_FIELDS["gas"]) | set(DEFAULTS_FIELDS["aerosol"]))
    for name, val in defaults.items():
        split = split_species_field(name)
        if split is None:
            _e(errors, f"DEFAULTS.{name}: expected format SPECIES_field "
               f"(e.g. SO2_conc, H2SO4_size), where field is one of {fields}")
            continue
        species, field = split
        kind = species_kind(species)
        if kind is None:
            _e(errors, f"DEFAULTS.{name}: unknown species '{species}'")
            kind = "gas"
        valid = DEFAULTS_FIELDS[kind]
        if field not in valid:
            _e(errors, f"DEFAULTS.{name}: unknown field '{field}' for {species}. "
               f"Valid: {sorted(valid)}")
            continue
        _check_value(errors, f"DEFAULTS.{name}", valid[field], val)


def parse_ratio(entry: object) -> tuple[str, str] | None:
    """
    Split a ``*PLOT`` ratio entry ``'HCl:SO2'`` into ``('HCl', 'SO2')`` (y, x).

    Returns ``None`` when the entry is not two non-empty names separated by
    one colon.
    """
    if not isinstance(entry, str):
        return None
    parts = [p.strip() for p in entry.split(":")]
    if len(parts) != 2 or not all(parts):
        return None
    return parts[0], parts[1]


def _as_list(value: object) -> list:
    """A species list as written in a config: ``None``, one name, or a list."""
    if value is None:
        return []
    if isinstance(value, str):
        return [value]
    return list(value) if isinstance(value, list) else []


def _warn_atm_temp_with_h2o(fit: dict, geom: dict, gtype: str,
                            dual: list[str], targets: list[str]) -> None:
    """
    Warn when a layer window fits ``atm_temp`` and targets ambient H2O.

    In that case the ambient temperature and the H2O column are degenerate:
    hot plume water is taken up as a warmer atmosphere, and the H2O column
    falls as it rises. Measured 2026-11-13 on the 418 layer validation spectra
    (CO window, 2020-2150 cm⁻¹): r(H2O, atm_temp) = −0.97, atm_temp following
    SO2 (r = +0.82), H2O/SO2 slope −47.5 against v2's +49.9. The developer's
    rerun with atm_temp fixed gave a physically sensible H2O/SO2 (not yet
    recorded with numbers). A warning, not an error: the fit runs, but
    the H2O column is not interpretable. Layer geometry only.

    Parameters
    ----------
    fit : dict
        The raw ``*FIT`` section.
    geom : dict
        The raw ``*GEOMETRY`` section (``fit_atm_temp`` default).
    gtype : str
        Geometry type, lower case.
    dual : list of str
        Gases of this window in both roles.
    targets : list of str
        The window's targets.
    """
    if gtype != "layer" or "H2O" not in _as_list(fit.get("atm_gases")):
        return
    fits_temp = fit.get("fit_atm_temp")
    if fits_temp is None:
        fits_temp = geom.get("fit_atm_temp", False)
    if fits_temp is not True or fit.get("atm_temp_from") is not None:
        return
    ambient_name = "H2O" + DUAL_ATM_SUFFIX if "H2O" in dual else "H2O"
    if ambient_name not in targets and "H2O" not in targets:
        return
    logger.warning(
        "%s: fits atm_temp and targets %s. The ambient temperature and the H2O "
        "column trade against each other in a layer window (validation data: "
        "r = -0.97, and H2O/SO2 changes sign), so this H2O column is not "
        "interpretable. Fix atm_temp here (fit_atm_temp: False, ideally at the "
        "temperature measured at collection), or fit it in a window without "
        "H2O as a target.", fit.get("_label", "FIT?"),
        ambient_name if ambient_name in targets else "H2O")


#: Staged temperatures: config key -> (parameter it passes, the flag that fits it).
_TEMP_FROM: dict[str, tuple[str, str]] = {
    "plume_temp_from": ("gas_temp", "fit_gas_temp"),
    "atm_temp_from":   ("atm_temp", "fit_atm_temp"),
}


def _check_temp_from(errors: list[str], fits: list[dict], gtype: str,
                     geom: dict, key: str) -> None:
    """
    Check every window's staged temperature, ``plume_temp_from`` or
    ``atm_temp_from``.

    The source must be an active window earlier in the file, so it is fitted
    first for each spectrum; it must fit the temperature itself (its own flag,
    or the GEOMETRY one it inherits) and must not receive one in turn.  A
    receiving window may not ask to fit that temperature explicitly: the two
    contradict each other.  Layer geometry only, the one with ``gas_temp`` and
    ``atm_temp``.

    Parameters
    ----------
    errors : list[str]
        Accumulated error messages (appended to).
    fits : list[dict]
        The raw ``*FIT`` sections, in file order.
    gtype : str
        Geometry type, lower case.
    geom : dict
        The raw ``*GEOMETRY`` section.
    key : str
        ``'plume_temp_from'`` or ``'atm_temp_from'``.
    """
    param, flag = _TEMP_FROM[key]
    order = {f.get("_label"): i for i, f in enumerate(fits)}
    by_label = {f.get("_label"): f for f in fits}
    for i, fit in enumerate(fits):
        src = fit.get(key)
        if src is None or fit.get("fit") is not True:
            continue
        label = fit.get("_label", "FIT?")
        if gtype != "layer":
            _e(errors, f"{label}.{key}: only the layer geometry fits "
               f"{param}; got geometry {gtype!r}")
            continue
        if not isinstance(src, str) or src not in by_label:
            _e(errors, f"{label}.{key}: {src!r} is not a window "
               f"label; windows are {sorted(k for k in by_label if k)}")
            continue
        source = by_label[src]
        if source.get("fit") is not True:
            _e(errors, f"{label}.{key}: window {src} is not active")
        elif order[src] >= i:
            _e(errors, f"{label}.{key}: window {src} must come before "
               f"{label} in the config, so it is fitted first")
        source_fits = source.get(flag)
        if source_fits is None:
            source_fits = geom.get(flag, False)
        if source_fits is not True:
            _e(errors, f"{label}.{key}: window {src} does not fit "
               f"{param} (set {flag}: True there)")
        if source.get(key) is not None:
            _e(errors, f"{label}.{key}: window {src} receives its own "
               f"{param} from {source.get(key)}; name the window "
               f"that fits it")
        if fit.get(flag) is True:
            _e(errors, f"{label}: {flag}: True and {key}: {src} "
               f"contradict each other; a window either fits {param} or "
               f"receives it")


def _check_path_tracer(errors: list[str], fits: list[dict], geom: dict,
                       gtype: str) -> None:
    """
    ``path_tracer`` must be an ambient-only, non-target gas of an active window.

    Its column measures the path only if it is the well-mixed background:
    a tracer that is also a plume gas (dual) or a target is a variable of the
    scene (a seep, a fire), not a reference.  Layer only (the catalogue
    rejects it elsewhere).
    """
    tracer = geom.get("path_tracer")
    if tracer is None or gtype != "layer":
        return
    active = [f for f in fits if f.get("fit") is True]
    ambient = [f.get("_label", "FIT?") for f in active
               if tracer in _as_list(f.get("atm_gases"))]
    plume = [f.get("_label", "FIT?") for f in active
             if tracer in _as_list(f.get("plume_gases"))]
    target = [f.get("_label", "FIT?") for f in active
              if tracer in _as_list(f.get("targets"))]
    if not ambient:
        _e(errors, f"GEOMETRY.path_tracer: '{tracer}' is not an atm_gas of any "
           f"active window; add it to one (for CH4, the ν3 P-branch "
           f"2880-2990 cm⁻¹ is the precise choice)")
    if plume:
        _e(errors, f"GEOMETRY.path_tracer: '{tracer}' is a plume gas in "
           f"{plume}; a tracer must be the ambient background only")
    if target:
        _e(errors, f"GEOMETRY.path_tracer: '{tracer}' is a target in {target}; "
           f"a tracer must not be a quantity of interest (choose another, "
           f"e.g. N2O when CH4 is a target)")


def validate(raw: dict) -> None:
    """
    Validate a parsed config dict.

    Parameters
    ----------
    raw : dict
        Output of :func:`plumeIR.config.parser.read_config`.

    Raises
    ------
    ConfigError
        All validation errors collected and reported together.
    """
    errors: list[str] = []

    # ------------------------------------------------------------------
    # *GEOMETRY type first: the other sections' checks depend on it
    # ------------------------------------------------------------------
    geom = raw.get("GEOMETRY", {})
    _check_section(errors, {"type": geom.get("type")}, "GEOMETRY", "GEOMETRY", None)
    gtype = str(geom.get("type", "")).lower()
    known = gtype if gtype in GEOMETRIES else None

    # ------------------------------------------------------------------
    # Per-key checks from the catalogue
    # ------------------------------------------------------------------
    for section in ("DATA", "MODEL", "RETRIEVAL", "PLOT"):
        _check_section(errors, raw.get(section, {}), section, section, known)
    if known is not None:
        _check_section(errors, geom, "GEOMETRY", "GEOMETRY", known)
    _check_defaults(errors, raw.get("DEFAULTS", {}))

    # The no-plume reference path (TransmittanceForwardModel) is
    # deferred to after v3.0: neither the reference spectrum's discovery
    # nor the division in preprocessing exists, so the flag would be
    # ignored.  It is planned to return as `no_plume`.
    if gtype == "layer" and geom.get("no_gas") is True:
        _e(errors, "GEOMETRY.no_gas: the no-gas (no-plume) reference "
           "path is not available in v3.0 (planned for active sources "
           "after the release, P6-4). Set no_gas: False to fit the "
           "full single beam")

    # ------------------------------------------------------------------
    # *FIT sections
    # ------------------------------------------------------------------
    fits = raw.get("fits", [])
    if not fits:
        _e(errors, "No *FIT sections found — at least one is required")

    all_species_list = POSSIBLE_GASES + POSSIBLE_AEROSOLS
    valid_targets    = set(all_species_list) | _VALID_TARGET_EXTRAS
    species_set      = set(all_species_list)
    target_windows: dict[str, list[str]] = {}   # species → windows it targets
    active = 0

    for fit in fits:
        label = fit.get("_label", "FIT?")
        _check_section(errors, {"fit": fit.get("fit")}, "FIT", label, None,
                       check_required=False)
        if fit.get("fit") is not True:
            continue
        active += 1
        _check_section(errors, fit, "FIT", label, known)

        # A gas in both roles is "dual" (experimental): an ambient
        # part {GAS}_atm and a plume part {GAS}, each with its own LUT.  Layer
        # only: emission carries its atmosphere in the AtmLayerLUTs and solar
        # in vertical OD profiles, neither of which has a per-gas parameter
        # that could be split this way.
        both = sorted(set(_as_list(fit.get("atm_gases")))
                      & set(_as_list(fit.get("plume_gases"))))
        if both and gtype != "layer":
            _e(errors, f"{label}: {both} listed in both atm_gases and "
               f"plume_gases. A gas can take both roles (an ambient and a "
               f"plume part) only in the layer geometry; got {gtype!r}")

        # Targets (species + diagnostic names)
        t_list = _as_list(fit.get("targets"))
        dual_targets = {g + DUAL_ATM_SUFFIX for g in both}
        unknown_t = [t for t in t_list
                     if t not in valid_targets and t not in dual_targets]
        if unknown_t:
            _e(errors, f"{label}.targets: unknown names {unknown_t}")
        # Track which windows target each species (for the uniqueness check).
        for t in t_list:
            if t in species_set:
                target_windows.setdefault(t, []).append(label)

        _warn_atm_temp_with_h2o(fit, geom, gtype, both, t_list)

        # continuum: the gases must also be gases of this window (the
        # catalogue checks they have a continuum treatment at all).
        window_gases = (set(_as_list(fit.get("atm_gases")))
                        | set(_as_list(fit.get("plume_gases"))))
        for sp in _as_list(fit.get("continuum")):
            if sp not in window_gases:
                _e(errors, f"{label}.continuum: '{sp}' is not an atm_gas or "
                   f"plume_gas in this window")

    if fits and active == 0:
        _e(errors, "All *FIT sections have fit=False — at least one must be active")

    for key in _TEMP_FROM:
        _check_temp_from(errors, fits, gtype, geom, key)

    _check_path_tracer(errors, fits, geom, gtype)

    # Each species may be a target in at most one window, so the results table
    # has a single trusted retrieval per species (no ambiguous de-duplication).
    for species, wins in target_windows.items():
        if len(wins) > 1:
            _e(errors, f"'{species}' is a target in multiple windows {wins}; "
               f"a species may be a target in only one window (it can still be "
               f"an interferer in the others)")

    # ------------------------------------------------------------------
    # *PLOT ratios: pairs of targets of active windows
    # ------------------------------------------------------------------
    plot = raw.get("PLOT", {})
    for entry in _as_list(plot.get("ratios")):
        pair = parse_ratio(entry)
        if pair is None:
            _e(errors, f"PLOT.ratios: {entry!r} is not a ratio; write it y:x, "
               f"e.g. HCl:SO2")
            continue
        missing = [sp for sp in pair if sp not in target_windows]
        if missing:
            _e(errors, f"PLOT.ratios: {entry!r} needs {missing} as the target "
               f"of an active window; targets are {sorted(target_windows)}")
        elif pair[0] == pair[1]:
            _e(errors, f"PLOT.ratios: {entry!r} compares a species with itself")

    # ------------------------------------------------------------------
    # Report
    # ------------------------------------------------------------------
    if errors:
        bullet_list = "\n".join(f"  • {e}" for e in errors)
        raise ConfigError(
            f"Config validation failed with {len(errors)} error(s):\n{bullet_list}"
        )

    logger.debug("Config validation passed (%d active fit window(s))", active)
