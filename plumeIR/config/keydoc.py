"""
Markdown config reference, generated from the key catalogue.

    python -m plumeIR.config.keydoc docs/CONFIG_REFERENCE.md

writes the reference that M4's README links to (item 19); without an argument
it prints it.  ``test/test_config_keys.py`` checks the committed file matches,
so the reference cannot drift from :mod:`plumeIR.config.keys`.
"""

from __future__ import annotations

import sys
from pathlib import Path

from plumeIR.config.keys import (
    ALL, AEROSOL, DEFAULTS_FIELDS, GAS, GEOMETRIES, PATTERNS, SECTION_HELP,
    Key, section_keys,
)

_INTRO = """\
# plumeIR config reference

Generated from `plumeIR/config/keys.py` by `python -m plumeIR.config.keydoc`;
do not edit by hand.

A config is a text file of sections, `*DATA`, `*MODEL`, `*GEOMETRY`, the
optional `*PREPROCESS`, `*RETRIEVAL`, `*DEFAULTS`, the optional `*PLOT`,
one `*FIT0`, `*FIT1`, ... per fit window, and the optional `*PATH` (layer),
each followed by `key : value` lines. `#` starts a comment
(inside a value only after a space, so a path may contain `#`). Key names are
case-blind.

Values are read by the key's type:

| Type | Written | Notes |
|---|---|---|
| bool | `True` / `False` | |
| int, float | `3`, `0.02`, `1e-3` | |
| str | text as written | never split, never read as a number (`filter_string : 2021` is text) |
| path | a file or directory | `~` and `$VAR` expanded; a relative path is taken from the config file's directory, then the working directory (RFM profiles also from `<rfm_dir>/atm`) |
| list | `H2O, CO2` | always a list, also of one item |
| window | `2020, 2150` | start and stop |

`None` means "not set" for every key: the default applies. A key the
geometry does not read is ignored with a warning, as is a key plumeIR does not
know.
"""


def _code(value: object) -> str:
    return f"`{value}`"


def _cell(text: str) -> str:
    return text.replace("|", "\\|")


def _default_text(key: Key) -> str:
    parts = []
    if key.required:
        where = ("" if key.required == ALL else
                 " in " + ", ".join(g for g in GEOMETRIES if g in key.required))
        parts.append(f"required{where}")
    per = {g: key.geometry_defaults[g] for g in GEOMETRIES
           if g in key.geometry_defaults and g not in key.required}
    if per:
        parts.append("; ".join(f"{g}: {_code(v)}" for g, v in per.items()))
    rest = [g for g in GEOMETRIES if key.used_in(g)
            and g not in key.required and g not in per]
    if rest:
        prefix = "else " if parts else ""
        parts.append(prefix + _code(key.default))
    return "; ".join(parts)


def _type_text(key: Key) -> str:
    text = key.type
    if key.must_exist:
        text += " (must exist)"
    return text


def _allowed(key: Key) -> str:
    if key.choices is not None and len(key.choices) > 12:
        return f"one of {len(key.choices)} names (see `plumeIR.constants`)"
    return key.limits_text()


def _description(key: Key) -> str:
    text = key.help
    if key.none:
        text += f". None = {key.none}"
    if key.inert:
        text += f". **{key.inert}**"
    return text


def _key_table(keys: tuple[Key, ...], geometry_column: bool = True) -> list[str]:
    head = ["Key", "Type", "Default", "Unit", "Allowed"]
    if geometry_column:
        head.append("Geometries")
    head.append("Description")
    lines = ["| " + " | ".join(head) + " |", "|" + "---|" * len(head)]
    for k in keys:
        row = [_code(k.name), _type_text(k), _default_text(k), k.unit or "",
               _allowed(k)]
        if geometry_column:
            row.append(k.geometry_note().replace(" only", "") or "all")
        row.append(_description(k))
        lines.append("| " + " | ".join(_cell(c) for c in row) + " |")
    return lines


def render_reference() -> str:
    """The whole reference, as Markdown."""
    out = [_INTRO]
    for section in ("DATA", "MODEL", "GEOMETRY", "PREPROCESS", "RETRIEVAL", "PLOT", "FIT"):
        title = "*FITn" if section == "FIT" else f"*{section}"
        out += [f"## `{title}`", "", SECTION_HELP[section] + ".", ""]
        if section == "FIT":
            out += ["Only active windows (`fit : True`) are checked beyond `fit`; "
                    "\"required\" means required of an active window.", "",
                    "`*PATH` (layer, at most one) is a fit window whose only purpose "
                    "is the path check: it takes the keys below, requires `tracer`, "
                    "has no `targets`, and is labelled `PATH` in the results. The "
                    "live display and the inspector show no fit panel for it.", ""]
        keys = section_keys(section)
        out += _key_table(keys, geometry_column=any(k.geometries != ALL for k in keys))
        patterns = [p for p in PATTERNS.values() if p.key.section == section]
        if patterns:
            out += ["", "Keys named by pattern:", ""]
            out += _key_table(tuple(p.key for p in patterns), geometry_column=False)
        out.append("")
    out += ["## `*DEFAULTS`", "", SECTION_HELP["DEFAULTS"] + ".", "",
            "Keys are `<SPECIES>_<field>`, e.g. `SO2_conc : 5.0`, `ASH_max_size : 5`. "
            "Priority, highest first: this section, then `standards.GAS_DEFAULTS` / "
            "`AERO_DEFAULTS`, then the default below.", ""]
    for kind, title in ((GAS, "Gas fields"), (AEROSOL, "Aerosol fields")):
        out += [f"### {title}", ""]
        out += _key_table(tuple(DEFAULTS_FIELDS[kind].values()), geometry_column=False)
        out.append("")
    return "\n".join(out).rstrip() + "\n"


def main(argv: list[str] | None = None) -> int:
    """Write the reference to the path given, or print it."""
    args = sys.argv[1:] if argv is None else argv
    text = render_reference()
    if args:
        Path(args[0]).write_text(text, encoding="utf-8")
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
