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
  the key ``'fits'``.
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
    DEFAULTS_FIELDS, PATTERNS, SECTION_HELP, Key, defaults_field, lookup,
)

logger = logging.getLogger(__name__)

_NAMED_SECTIONS = {"DATA", "MODEL", "GEOMETRY", "RETRIEVAL", "DEFAULTS", "PLOT"}

# Column at which inline comments start in the written file
_COMMENT_COL = 48


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
        ``'DEFAULTS'``, ``'fits'`` (list of dicts, one per ``*FITn``), and
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
                elif section_name.startswith("FIT"):
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
            spec_section = "FIT" if current_name.startswith("FIT") else current_name
            current_section[key] = _parse_value(spec_section, key, raw_value)

    n_fits = len(result["fits"])
    logger.debug("Parsed '%s': %d fit window(s)", path.name, n_fits)
    return result


def section_comment(section: str) -> list[str]:
    """
    The comment lines written above a section header (``'FIT'`` for windows).

    The section's help from the catalogue, plus how to write the name-pattern
    keys the section takes.
    """
    lines = [SECTION_HELP[section]]
    if section == "DEFAULTS":
        for kind, label in (("gas", "Gas fields    "), ("aerosol", "Aerosol fields")):
            fields = ", ".join(f"{k.name} [{k.unit}]" if k.unit else k.name
                               for k in DEFAULTS_FIELDS[kind].values())
            lines.append(f"  {label}: {fields}")
        lines.append("  Format        : SPECIES_field : value   (e.g. SO2_conc : 5.0)")
    for pattern in PATTERNS.values():
        if pattern.key.section == section:
            lines.append(f"  {pattern.key.name} : {pattern.key.comment()}")
    return lines


def write_config(config: dict, path: str | Path) -> None:
    """
    Write a config dict back to a commented plumeIR ``.ini`` file.

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
        if isinstance(value, bool):
            return str(value)
        if isinstance(value, list):
            return ", ".join(_fmt(v) for v in value)
        return str(value)

    def _kv_line(section: str, key: str, value: object) -> str:
        """Format a key-value line with an aligned inline comment."""
        val_str = _fmt(value)
        base = f"{key:<20s} : {val_str}"
        spec = key_spec(section, key)
        if spec is not None and spec.help:
            pad = max(1, _COMMENT_COL - len(base))
            return f"{base}{' ' * pad}# {spec.comment()}"
        return base

    def _section_header(name: str) -> list[str]:
        """Return the section header lines including a description comment."""
        return [f"# {line}" for line in section_comment(name)] + [f"*{name}"]

    output: list[str] = []

    # File banner
    banner_width = 100
    output.append("#" * banner_width)
    output.append(f"#{'plumeIR v3 — Configuration File':^{banner_width - 2}}#")
    output.append(f"#{'Variable name':^20s}{'':2s}{'Value':^20s}{'Description':^{banner_width - 44}}#")
    output.append("#" * banner_width)
    output.append("")

    section_order = ["DATA", "MODEL", "GEOMETRY", "RETRIEVAL", "DEFAULTS", "PLOT"]

    for section in section_order:
        data = config.get(section, {})
        if not data:
            continue
        output.append("")
        output.extend(_section_header(section))
        output.append("")
        for key, value in data.items():
            output.append(_kv_line(section, key, value))
        output.append("")

    for fit in config.get("fits", []):
        label = fit.get("_label", "FIT?")
        output.append("")
        output.extend(_section_header("FIT"))
        output[-1] = f"*{label}"   # replace generic *FIT with actual label
        output.append("")
        for key, value in fit.items():
            if key == "_label":
                continue
            output.append(_kv_line("FIT", key, value))
        output.append("")

    path.write_text("\n".join(output), encoding="utf-8")
    logger.debug("Wrote config to '%s'", path)
