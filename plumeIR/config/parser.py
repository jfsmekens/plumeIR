"""
Config file parser for plumeIR v3.

Reads and writes plumeIR ``.ini`` config files.
No validation is performed here — that is the job of ``validator.py``.

File format
-----------
- Lines starting with ``#`` are comments; ignored on read.
- Section headers: ``*SECTIONNAME``
- Key-value pairs: ``key  :  value  # optional inline comment``
- Inline comments must be preceded by whitespace: ``# comment`` not ``#comment``,
  so paths like ``/data/#2024/`` are parsed correctly.
- ``*FIT0``, ``*FIT1``, … sections are collected as an ordered list under
  the key ``'fits'``; so is ``*PATH`` (the path window, layer), in file
  order, with the window keys and its ``tracer``.
- ``*DEFAULTS`` entries use ``SPECIES_field`` keys (e.g. ``SO2_conc``).

Values are converted by the type the key catalogue declares
(:mod:`plumeIR.config.keys`, M3d): ``None`` (any case) is ``None`` for every
key; ``bool`` keys read ``True`` / ``False``, ``int`` and ``float`` keys a
number; ``str`` and ``path`` keys keep the text as written (``filter_string :
2021`` stays a string, and a path may contain commas); only ``list`` keys
(always a list, even of one item) and ``window`` keys (``start, stop``) split
on commas.  A value that does not convert is kept as text, for the validator
to report.  A key the catalogue does not know is converted by guessing, as
before M3d, and the validator warns about it.

The comments ``write_config`` writes come from the catalogue.
"""

from __future__ import annotations

import logging
import re
from pathlib import Path

from plumeIR.config.keys import (
    AEROSOL, DEFAULTS_FIELDS, GAS, PATH_SECTION, Key,
    defaults_field, lookup, split_species_field,
)
from plumeIR.constants import (
    LEGACY_AEROSOL_NAMES, LEGACY_PARAMETER_NAMES, POSSIBLE_GASES,
)

logger = logging.getLogger(__name__)

_NAMED_SECTIONS = {"DATA", "MODEL", "GEOMETRY", "PREPROCESS", "RETRIEVAL", "DEFAULTS",
                   "PLOT"}

#: Keys that moved section: (old section, key) -> new section.  A config that
#: still has them in the old place is read, with a warning, by
#: :func:`migrate_moved_keys`.
MOVED_KEYS: dict[tuple[str, str], str] = {
    ("GEOMETRY", "subtract_self"): "PREPROCESS",    # 2026-11-15
    ("GEOMETRY", "self_file"):     "PREPROCESS",    # 2026-11-15
}

#: Keys renamed in place (2026-11-19, developer; ``docs/WINDOW_CONTROLS.md``
#: section 6): every fit flag is ``fit_`` plus the parameter it controls, and
#: plume quantities are named ``plume...``.  Old names are read, with a
#: warning, by :func:`migrate_renamed_keys`, in whichever section they stand.
#: ``fit_gas_temp`` (layer) and ``fit_tplume`` (emission) become one key.
RENAMED_KEYS: dict[str, str] = {
    "fit_gas_temp":         "fit_plume_temp",
    "fit_tplume":           "fit_plume_temp",
    "fit_tprox":            "fit_dt_prox",
    "fit_tdist":            "fit_dt_dist",
    "fit_size_aero":        "fit_r0",
    "fit_aero_composition": "fit_wt",
    "model_gas_emission":   "model_plume_emission",
    "nu_shift":             "shift",
}

# Column at which inline comments start in the written file


# ---------------------------------------------------------------------------
# Parsing helpers
# ---------------------------------------------------------------------------

def _guess(token: str) -> None | bool | int | float | str:
    """Convert a token to the most specific type (keys the catalogue lacks)."""
    t = token.strip()
    if t.lower() == "none":
        return None
    if t.lower() == "true":
        return True
    if t.lower() == "false":
        return False
    try:
        i = int(t)
        if str(i) == t:
            return i
    except ValueError:
        pass
    try:
        return float(t)
    except ValueError:
        pass
    return t


def _number(text: str, kind: type) -> int | float | str:
    """``text`` as ``kind`` (int or float), or unchanged if it is not one."""
    try:
        return kind(text)
    except ValueError:
        return text


def convert(key: Key | None, text: str) -> object:
    """
    Convert one value's text by the key's declared type.

    Parameters
    ----------
    key : Key or None
        The catalogue entry; ``None`` for an unknown key, which is guessed.
    text : str
        The value as written, inline comment removed.

    Returns
    -------
    object
        ``None`` for ``None``; else a value of the declared type, or the text
        unchanged where it does not convert (the validator reports it).
    """
    t = text.strip()
    if t.lower() == "none":
        return None
    if key is None:
        if "," in t:
            coerced = [_guess(tok) for tok in t.split(",") if tok.strip()]
            return coerced[0] if len(coerced) == 1 else coerced
        return _guess(t)
    if key.type == "list":
        return [tok.strip() for tok in t.split(",") if tok.strip()]
    if key.type == "window":
        return [_number(tok.strip(), float) for tok in t.split(",") if tok.strip()]
    if key.type == "flags":
        flag = {"true": True, "false": False}.get(t.lower())
        return flag if flag is not None else [tok.strip() for tok in t.split(",")
                                              if tok.strip()]
    if key.type == "bool":
        return {"true": True, "false": False}.get(t.lower(), t)
    if key.type == "int":
        return _number(t, int)
    if key.type == "float":
        return _number(t, float)
    return t


def key_spec(section: str, name: str) -> Key | None:
    """
    The catalogue entry for ``name`` in ``section`` (``'FIT'`` for ``*FITn``).

    Covers plain keys, name patterns and ``*DEFAULTS`` fields.
    """
    if section == "DEFAULTS":
        return defaults_field(name)
    return lookup(section, name)


def _parse_value(section: str, key: str, raw: str) -> object:
    """
    Parse a raw value string (after the ``:`` separator).

    Strips inline comments — only ``#`` preceded by whitespace is treated
    as a comment marker, so paths like ``/data/#2024/`` are safe.
    """
    raw = re.split(r'\s+#', raw, maxsplit=1)[0].strip()
    return convert(key_spec(section, key), raw)


def migrate_moved_keys(raw: dict) -> dict:
    """
    Move keys found in their former section (:data:`MOVED_KEYS`) to the new one.

    Returns a copy; the input is not modified.  Each moved key logs a warning
    naming its new place.  When a key is set in both places the new section
    wins, and the old value is dropped with a warning.

    Parameters
    ----------
    raw : dict
        A raw config dict (:func:`read_config` output, or a client's).

    Returns
    -------
    dict
        The config with every moved key in its current section.
    """
    out = dict(raw)
    for (old, name), new in MOVED_KEYS.items():
        section = out.get(old)
        if not isinstance(section, dict) or name not in section:
            continue
        old_section = dict(section)
        value = old_section.pop(name)
        new_section = dict(out.get(new) or {})
        if name in new_section:
            logger.warning("%s.%s is also set in *%s; the *%s value is used and "
                           "the *%s one ignored", old, name, new, new, old)
        else:
            new_section[name] = value
            logger.warning("%s.%s has moved to *%s; read from *%s here. Move it "
                           "to *%s in the config file.", old, name, new, old, new)
        out[old] = old_section
        out[new] = new_section
    return out


def _rename_keys(section: dict, where: str, renamed: set[str]) -> dict:
    """One section with :data:`RENAMED_KEYS` applied (a copy); the new name wins."""
    out: dict = {}
    for key, value in section.items():
        new = RENAMED_KEYS.get(key)
        if new is None:
            if key.startswith("sigma_prior_"):
                param = key[len("sigma_prior_"):]
                new_param = LEGACY_PARAMETER_NAMES.get(param)
                if new_param is not None:
                    new = "sigma_prior_" + new_param
        if new is None:
            if key not in out:
                out[key] = value
            continue
        if new in section:
            logger.warning("%s.%s is also set as %s; the %s value is used and the "
                           "%s one ignored", where, key, new, new, key)
            continue
        renamed.add(f"{key} → {new}")
        out[new] = value
    return out


def migrate_renamed_keys(raw: dict) -> dict:
    """
    Read keys and parameter names renamed on 2026-11-19 under their new names.

    Keys in :data:`RENAMED_KEYS` are renamed in every section and window, as
    are ``sigma_prior_<param>`` keys and the window ``targets`` that name a
    parameter in :data:`~plumeIR.constants.LEGACY_PARAMETER_NAMES`
    (``gas_temp`` is ``plume_temp`` now).  One warning lists every rename.
    Where a key is set under both names the new one is used, with a warning.

    Parameters
    ----------
    raw : dict
        A raw config dict (:func:`read_config` output, or a client's).

    Returns
    -------
    dict
        A copy with the new names; the input is not modified.
    """
    out = dict(raw)
    renamed: set[str] = set()
    for name in _NAMED_SECTIONS:
        if isinstance(raw.get(name), dict):
            out[name] = _rename_keys(raw[name], name, renamed)
    if isinstance(raw.get("fits"), list):
        fits = []
        for f in raw["fits"]:
            if not isinstance(f, dict):
                fits.append(f)
                continue
            f = _rename_keys(f, f.get("_label", "FIT"), renamed)
            targets = f.get("targets")
            if isinstance(targets, (list, tuple)):
                new = [LEGACY_PARAMETER_NAMES.get(t, t) if isinstance(t, str) else t
                       for t in targets]
                for old, n in zip(targets, new):
                    if old != n:
                        renamed.add(f"target {old} → {n}")
                f["targets"] = new
            elif isinstance(targets, str) and targets in LEGACY_PARAMETER_NAMES:
                renamed.add(f"target {targets} → {LEGACY_PARAMETER_NAMES[targets]}")
                f["targets"] = LEGACY_PARAMETER_NAMES[targets]
            fits.append(f)
        out["fits"] = fits
    if renamed:
        logger.warning("Former names renamed (2026-11-19: fit flags are fit_<parameter>, "
                       "plume quantities plume..., nu_shift is shift): %s. Update the "
                       "config file.", ", ".join(sorted(renamed)))
    return out


def _legacy(name: str, aerosols_here: set[str]) -> str | None:
    """
    The current name of a former aerosol name, or ``None``.

    ``name`` may carry an ``r0_`` / ``wt_`` prefix.  A former name that is
    also a gas (``HNO3``) is renamed only where it was used as an aerosol
    (``aerosols_here``); ``H2SO4`` is always an aerosol in a config.
    """
    for prefix in ("", "r0_", "wt_"):
        if not name.startswith(prefix):
            continue
        base = name[len(prefix):]
        new = LEGACY_AEROSOL_NAMES.get(base) or LEGACY_AEROSOL_NAMES.get(base.upper())
        if new is None:
            continue
        legacy = base.upper() if base.upper() in LEGACY_AEROSOL_NAMES else base
        if legacy in POSSIBLE_GASES and legacy not in aerosols_here:
            return None
        return prefix + new
    return None


def migrate_species_names(raw: dict) -> dict:
    """
    Rename former aerosol names (:data:`~plumeIR.constants.LEGACY_AEROSOL_NAMES`)
    to the current ones: ``H2SO4`` → ``H2SO4_aq``, ``HNO3`` → ``HNO3_aq``.

    Returns a copy; logs one warning listing what was renamed.  Covers a
    window's ``plume_aero`` and ``targets`` (also ``r0_`` / ``wt_``
    parameters), ``*DEFAULTS`` fields (``h2so4_size`` → ``h2so4_aq_size``),
    ``*RETRIEVAL`` ``sigma_prior_<param>`` and ``*PLOT`` ``ratios``.
    ``HNO3`` is also a gas: it is renamed only where the config uses it as an
    aerosol (in some window's ``plume_aero``), and its ``*DEFAULTS`` fields
    only for aerosol-only fields.

    Parameters
    ----------
    raw : dict
        A raw config dict.

    Returns
    -------
    dict
    """
    out = dict(raw)
    renamed: set[str] = set()
    aerosols = {str(a) for f in raw.get("fits") or [] for a in (f.get("plume_aero") or [])
                if isinstance(a, str)}
    aerosols = {a.upper() if a.upper() in LEGACY_AEROSOL_NAMES else a for a in aerosols}

    def rename(name):
        if not isinstance(name, str):
            return name
        new = _legacy(name, aerosols)
        if new is None:
            return name
        renamed.add(f"{name} → {new}")
        return new

    def rename_list(values):
        if isinstance(values, (list, tuple)):
            return [rename(v) for v in values]
        return rename(values) if isinstance(values, str) else values

    fits = []
    for f in raw.get("fits") or []:
        f = dict(f)
        here = {a.upper() for a in (f.get("plume_aero") or []) if isinstance(a, str)}
        if "plume_aero" in f:
            f["plume_aero"] = rename_list(f["plume_aero"])
        for key in ("fit_r0", "fit_wt"):   # lists of aerosols
            if isinstance(f.get(key), (list, tuple, str)) and not isinstance(f[key], bool):
                f[key] = rename_list(f[key])
        if "targets" in f and isinstance(f["targets"], (list, tuple)):
            # a target is renamed where its window uses it as an aerosol
            f["targets"] = [rename(t) if isinstance(t, str) and _legacy(t, here) else t
                            for t in f["targets"]]
        fits.append(f)
    if "fits" in raw:
        out["fits"] = fits
    aero_fields = set(DEFAULTS_FIELDS[AEROSOL]) - set(DEFAULTS_FIELDS[GAS])
    if isinstance(raw.get("DEFAULTS"), dict):
        section = {}
        for key, value in raw["DEFAULTS"].items():
            new_key = key
            split = split_species_field(key)
            if split is not None:
                species, field = split
                legacy = species.upper()
                if legacy in LEGACY_AEROSOL_NAMES and (
                        legacy not in POSSIBLE_GASES
                        or (legacy in aerosols and field in aero_fields)):
                    new_key = f"{LEGACY_AEROSOL_NAMES[legacy].lower()}_{field}"
                    renamed.add(f"{key} → {new_key}")
            section[new_key] = value
        out["DEFAULTS"] = section
    if isinstance(raw.get("RETRIEVAL"), dict):
        section = {}
        for key, value in raw["RETRIEVAL"].items():
            if key in ("fit_r0", "fit_wt") and \
                    isinstance(value, (list, tuple, str)) and not isinstance(value, bool):
                value = rename_list(value)
            new_key = key
            if key.startswith("sigma_prior_"):
                new = _legacy(key[len("sigma_prior_"):], aerosols)
                if new is not None:
                    new_key = "sigma_prior_" + new.lower()
                    renamed.add(f"{key} → {new_key}")
            section[new_key] = value
        out["RETRIEVAL"] = section
    if isinstance(raw.get("PLOT"), dict) and raw["PLOT"].get("ratios"):
        plot = dict(raw["PLOT"])
        ratios = plot["ratios"]
        items = ratios if isinstance(ratios, (list, tuple)) else [ratios]
        new_items = []
        for item in items:
            if isinstance(item, str) and ":" in item:
                y, x = (part.strip() for part in item.split(":", 1))
                new_items.append(f"{rename(y)}:{rename(x)}")
            else:
                new_items.append(item)
        plot["ratios"] = new_items if isinstance(ratios, (list, tuple)) else new_items[0]
        out["PLOT"] = plot
    if renamed:
        logger.warning("Former aerosol names renamed (acid aerosols are now <ACID>_aq, "
                       "2026-11-17): %s. Update the config file.", ", ".join(sorted(renamed)))
    return out


def normalise(raw: dict) -> dict:
    """
    Convert the text values of a raw config dict by their declared types.

    For a raw dict held as data (a GUI's), whose values may still be text:
    each ``str`` value is converted as the parser would convert it, and a
    single name given to a ``list`` key becomes a one-item list.  Values that
    are already typed are left alone, so this is idempotent and a no-op on
    :func:`read_config` output.

    Parameters
    ----------
    raw : dict
        As returned by :func:`read_config`.  Not modified.

    Returns
    -------
    dict
        A converted copy (sections copied, values shared).
    """
    raw = migrate_species_names(migrate_moved_keys(migrate_renamed_keys(raw)))
    out = dict(raw)
    for section in _NAMED_SECTIONS:
        if isinstance(raw.get(section), dict):
            out[section] = {k: _normalise_value(section, k, v)
                            for k, v in raw[section].items()}
    if isinstance(raw.get("fits"), list):
        out["fits"] = [
            {k: (v if k == "_label" else _normalise_value("FIT", k, v))
             for k, v in fit.items()} if isinstance(fit, dict) else fit
            for fit in raw["fits"]
        ]
    return out


def _normalise_value(section: str, name: str, value: object) -> object:
    key = key_spec(section, name)
    if key is None or not isinstance(value, str):
        return value
    return convert(key, value)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def read_config(path: str | Path) -> dict:
    """
    Parse a plumeIR ``.ini`` config file into a raw nested dict.

    Parameters
    ----------
    path : str or Path
        Path to the config file.

    Returns
    -------
    dict with keys:
        ``'DATA'``, ``'MODEL'``, ``'GEOMETRY'``, ``'RETRIEVAL'``,
        ``'DEFAULTS'``, ``'fits'`` (list of dicts, one per ``*FITn`` or
        ``*PATH``), and
        ``'PLOT'`` only when the file has that (optional) section.
        Each section dict maps lower-case key names to parsed values.

    Raises
    ------
    FileNotFoundError
        If the config file does not exist.
    """
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(f"Config file not found: {path}")

    result: dict = {
        "DATA":      {},
        "MODEL":     {},
        "GEOMETRY":  {},
        "RETRIEVAL": {},
        "DEFAULTS":  {},
        "fits":      [],
    }

    current_section: dict | None = None
    current_name: str = ""

    with path.open(encoding="utf-8") as fh:
        for lineno, line in enumerate(fh, start=1):
            line = line.strip()
            if not line or line.startswith("#"):
                continue

            # Section header
            if line.startswith("*"):
                section_name = line.split()[0][1:].upper()
                if section_name in _NAMED_SECTIONS:
                    current_section = result.setdefault(section_name, {})
                elif section_name.startswith("FIT") or section_name == PATH_SECTION:
                    new_fit: dict = {"_label": section_name}
                    result["fits"].append(new_fit)
                    current_section = new_fit
                else:
                    logger.warning(
                        "Line %d: unknown section '*%s' — ignoring all lines until "
                        "the next section header", lineno, section_name
                    )
                    current_section = None
                current_name = section_name
                continue

            # Key-value pair — must contain ':'
            if ":" not in line:
                logger.warning(
                    "Line %d: no ':' separator found — line ignored: %r", lineno, line
                )
                continue

            if current_section is None:
                logger.warning(
                    "Line %d: key-value pair outside any recognised section — ignored",
                    lineno
                )
                continue

            key, _, raw_value = line.partition(":")
            key = key.strip().lower()
            spec_section = ("FIT" if current_name.startswith("FIT")
                            or current_name == PATH_SECTION else current_name)
            # A renamed key is parsed by its new key's type and keeps its old
            # name until migrate_renamed_keys warns about it.
            current_section[key] = _parse_value(spec_section,
                                                RENAMED_KEYS.get(key, key), raw_value)

    n_fits = len(result["fits"])
    logger.debug("Parsed '%s': %d fit window(s)", path.name, n_fits)
    return migrate_species_names(migrate_moved_keys(migrate_renamed_keys(result)))


#: Width of the key column in a written config: the longest catalogued key
#: name is 20 characters.
_KEY_WIDTH = 20

#: First line of a written config.
CONFIG_HEADER = "# plumeIR config; every key is described in docs/CONFIG_REFERENCE.md"

#: Section order of a written config (the windows follow, in their order).
_WRITE_ORDER: tuple[str, ...] = ("DATA", "MODEL", "GEOMETRY", "PREPROCESS",
                                 "RETRIEVAL", "DEFAULTS", "PLOT")


def write_config(config: dict, path: str | Path) -> None:
    """
    Write a config dict as a plumeIR ``.ini`` file: values only.

    One header line points to the key reference (no version: a config
    outlives releases, and a run's ``summary.txt`` records versions); then
    each section with ``key : value`` lines aligned on the colon, the fit
    windows (``*FITn``, ``*PATH``) last, in their order.  Nothing describes
    the keys: their help lives in the key catalogue
    (:mod:`plumeIR.config.keys`) and the reference generated from it,
    ``docs/CONFIG_REFERENCE.md`` (developer, 2026-11-18; the file carried
    each key's help as a comment until then).  :func:`read_config` reads the
    file back to the same dict.

    Parameters
    ----------
    config : dict
        Nested dict in the same structure returned by :func:`read_config`.
    path : str or Path
        Destination path (will be overwritten).
    """
    path = Path(path)

    def _fmt(value: object) -> str:
        if value is None:
            return "None"
        if isinstance(value, (list, tuple)):
            return ", ".join(_fmt(v) for v in value)
        return str(value)

    def _section(name: str, items: dict) -> list[str]:
        lines = ["", f"*{name}"]
        lines += [f"{key:<{_KEY_WIDTH}s} : {_fmt(value)}".rstrip()
                  for key, value in items.items() if not key.startswith("_")]
        return lines

    output = [CONFIG_HEADER]
    for section in _WRITE_ORDER:
        if config.get(section):
            output += _section(section, config[section])
    for fit in config.get("fits", []):
        output += _section(fit.get("_label", "FIT?"), fit)

    path.write_text("\n".join(output) + "\n", encoding="utf-8")
    logger.debug("Wrote config to '%s'", path)
