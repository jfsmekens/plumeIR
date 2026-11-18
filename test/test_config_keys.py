"""
Tests for the config key catalogue (plumeIR.config.keys).

The catalogue is the one description of every config key: these tests hold
the parser, validator, builder, paths, ``write_config``, the config
dataclasses and the generated reference to it.

Run with:  pytest test/test_config_keys.py -v
"""

from __future__ import annotations

import dataclasses
import logging
from pathlib import Path

import pytest

from plumeIR import standards
from plumeIR.config import keys
from plumeIR.config.builder import build_from_raw
from plumeIR.config.keydoc import render_reference
from plumeIR.config.objects import (
    DataConfig, EmissionGeometry, LayerGeometry, ModelConfig, PlotConfig,
    PreprocessConfig, Retrieval, SolarGeometry,
)
from plumeIR.config.parser import convert, normalise, read_config, write_config
from plumeIR.config.paths import PATH_KEYS, RFM_ATM_KEYS
from plumeIR.config.validator import _check_value, validate
from plumeIR.exceptions import ConfigError

_ROOT = Path(__file__).resolve().parents[1]

_LAYER = """
*DATA
data_dir      : {data_dir}
filter_files  : True
filter_string : 2021
{extra_data}

*MODEL
*GEOMETRY
type        : layer
pathlength  : 1.0
atm_temp    : 280
atm_pres    : 1000
plume_temp  : 400
plume_pres  : 1000
{extra_geom}

*RETRIEVAL
{extra_retr}

*DEFAULTS
{extra_defaults}

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : H2O
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 2
{extra_fit}

*FIT1
fit         : True
fit_window  : 2650, 2850
atm_gases   : H2O, CH4
plume_gases : HCl
targets     : HCl
"""


def _write(tmp_path: Path, **extra: str) -> Path:
    fields = dict(extra_data="", extra_geom="", extra_retr="", extra_defaults="",
                  extra_fit="")
    fields.update(extra)
    path = tmp_path / "run.ini"
    path.write_text(_LAYER.format(data_dir=tmp_path, **fields), encoding="utf-8")
    return path


def _build(tmp_path: Path, **extra: str) -> dict:
    return build_from_raw(read_config(_write(tmp_path, **extra)), base_dir=tmp_path)


# =============================================================================
# The catalogue matches the config dataclasses
# =============================================================================

#: Dataclass fields that are not config keys.
_NOT_KEYS = {
    "Retrieval": {"geometry_type", "max_opd_explicit", "apod_type_explicit",
                  "ils_resolved", "sigma_prior"},
    "PlotConfig": {"background"},
}


@pytest.mark.parametrize("cls, section, geometry", [
    (DataConfig, "DATA", None),
    (ModelConfig, "MODEL", None),
    (LayerGeometry, "GEOMETRY", "layer"),
    (EmissionGeometry, "GEOMETRY", "emission"),
    (SolarGeometry, "GEOMETRY", "solar"),
    (Retrieval, "RETRIEVAL", None),
    (PlotConfig, "PLOT", None),
    (PreprocessConfig, "PREPROCESS", "layer"),
])
def test_dataclass_fields_are_the_keys_the_geometry_reads(cls, section, geometry):
    field_names = {f.name for f in dataclasses.fields(cls)} - _NOT_KEYS.get(cls.__name__, set())
    key_names = {k.name for k in keys.section_keys(section)
                 if k.name != "type" and (geometry is None or k.used_in(geometry))}
    assert field_names == key_names


@pytest.mark.parametrize("cls, geometry", [
    (LayerGeometry, "layer"), (EmissionGeometry, "emission"), (SolarGeometry, "solar"),
])
def test_dataclass_defaults_come_from_the_catalogue(cls, geometry):
    for f in dataclasses.fields(cls):
        key = keys.KEYS[("GEOMETRY", f.name)]
        if f.default is not dataclasses.MISSING:
            assert f.default == key.default_for(geometry), f.name
            assert type(f.default) is type(key.default_for(geometry)), f.name


def test_per_geometry_defaults():
    assert PreprocessConfig().subtract_self is False
    assert keys.default("PREPROCESS", "subtract_self", "emission") is True
    assert keys.default("GEOMETRY", "plume_thickness", "layer") == 0.05
    assert keys.default("GEOMETRY", "plume_thickness", "solar") is None


def test_every_default_passes_its_own_check():
    for key in keys.KEYS.values():
        for geometry in keys.GEOMETRIES:
            errors: list[str] = []
            _check_value(errors, key.name, key, key.default_for(geometry))
            assert not errors, errors
    for kind in (keys.GAS, keys.AEROSOL):
        for key in keys.DEFAULTS_FIELDS[kind].values():
            errors = []
            _check_value(errors, key.name, key, key.default)
            assert not errors, errors


def test_keys_are_well_formed():
    for key in keys.KEYS.values():
        assert key.help, key.name
        assert key.required <= key.geometries, key.name
        assert set(key.geometry_defaults) <= key.geometries, key.name
        assert key.name == key.name.lower(), key.name


def test_physics_tables_hold_bounds_not_config_defaults():
    """ILS/TEMP a priori values are config keys now; the tables keep bounds."""
    for table in (standards.ILS_DEFAULTS, standards.TEMP_DEFAULTS):
        for name, entry in table.items():
            assert "value" not in entry, name


@pytest.mark.parametrize("section, line", [
    # v2 options v3 never implemented, removed 2026-11-12 (developer)
    ("extra_retr", "seed_param : SO2"),
    ("extra_retr", "use_residual : True"),
    ("extra_geom", "use_source_e : True"),
    ("extra_geom", "sounding : sonde.txt"),
])
def test_removed_keys_are_unknown(tmp_path, caplog, section, line):
    """A carried-over config still builds; the removed key is named in a warning."""
    with caplog.at_level(logging.WARNING, logger="plumeIR.config.validator"):
        _build(tmp_path, **{section: line})
    assert f".{line.split()[0]}: unknown key" in caplog.text


def test_only_the_proximal_prior_keys_are_inert():
    inert = {(k.section, k.name) for k in keys.KEYS.values() if k.inert}
    assert inert == {("GEOMETRY", "obs_temp"), ("GEOMETRY", "obs_rh")}
    # the aerosol *DEFAULTS sigma field stays only while the repository
    # configs set it (developer to decide, 2026-11-12)
    assert [k.name for k in keys.DEFAULTS_FIELDS["aerosol"].values() if k.inert] == ["sigma"]


# =============================================================================
# Name patterns
# =============================================================================

def test_patterns():
    assert keys.lookup("RETRIEVAL", "sigma_prior_fov") is keys.PATTERNS["sigma_prior"].key
    assert keys.lookup("PLOT", "background_co2") is keys.PATTERNS["background"].key
    assert keys.lookup("PLOT", "sigma_prior_fov") is None
    assert keys.PATTERNS["background"].canonical("co2") == "CO2"
    assert keys.PATTERNS["background"].canonical("xyz") is None


@pytest.mark.parametrize("key, expected", [
    ("so2_conc", ("SO2", "conc")),
    ("ash_max_size", ("ASH", "max_size")),
    ("h2so4_size", ("H2SO4", "size")),
    ("h2so4_aq_size", ("H2SO4_aq", "size")),
    ("hno3_aq_max_size", ("HNO3_aq", "max_size")),
    ("hcl_conc", ("HCl", "conc")),     # registry spelling, not HCL
    ("xyz_conc", ("XYZ", "conc")),
    ("badkey", None),
])
def test_split_species_field(key, expected):
    assert keys.split_species_field(key) == expected


def test_mixed_case_species_default_reaches_the_parameter(tmp_path):
    """HCl_conc used to read as species HCL, which the registry does not know."""
    base = _build(tmp_path)["params"][1]["HCl"].ref_column
    cfg = _build(tmp_path, extra_defaults="HCl_conc : 1.0")
    assert cfg["params"][1]["HCl"].ref_column == pytest.approx(base * 2.0, rel=1e-9)


# =============================================================================
# Typed reading (14f)
# =============================================================================

@pytest.mark.parametrize("section, name, text, expected", [
    ("FIT", "atm_gases", "H2O", ["H2O"]),             # one item: still a list
    ("FIT", "atm_gases", "H2O, CO2", ["H2O", "CO2"]),
    ("FIT", "fit_window", "2020, 2150", [2020.0, 2150.0]),
    ("DATA", "filter_string", "2021", "2021"),        # a string stays a string
    ("DATA", "data_dir", "/data/a,b/run", "/data/a,b/run"),   # paths never split
    ("RETRIEVAL", "apod_type", "NB medium", "NB medium"),
    ("MODEL", "nperwn", "25", 25.0),
    ("DATA", "max_spec", "10", 10),
    ("DATA", "max_spec", "1e3", "1e3"),               # not an int: kept for the validator
    ("RETRIEVAL", "use_bounds", "false", False),
    ("RETRIEVAL", "use_bounds", "maybe", "maybe"),
    ("MODEL", "cache_dir", "None", None),
    ("FIT", "plume_aero", "none", None),
])
def test_convert_by_declared_type(section, name, text, expected):
    value = convert(keys.lookup(section, name), text)
    assert value == expected
    assert type(value) is type(expected)


def test_unknown_key_is_guessed_and_warned(tmp_path, caplog):
    path = _write(tmp_path, extra_geom="plume_thicknes : 0.1")
    raw = read_config(path)
    assert raw["GEOMETRY"]["plume_thicknes"] == 0.1
    with caplog.at_level(logging.WARNING, logger="plumeIR.config.validator"):
        build_from_raw(raw, base_dir=tmp_path)
    assert "GEOMETRY.plume_thicknes: unknown key" in caplog.text


def test_key_of_another_geometry_is_ignored_with_a_warning(tmp_path, caplog):
    with caplog.at_level(logging.WARNING, logger="plumeIR.config.validator"):
        cfg = _build(tmp_path, extra_geom="elev : 500")
    assert "GEOMETRY.elev: not used by the layer geometry" in caplog.text
    assert not hasattr(cfg["geometry"], "elev")


def test_numeric_filter_string_is_a_string(tmp_path):
    assert _build(tmp_path)["data"].filter_string == "2021"


def test_text_values_in_a_raw_dict_build_like_the_file(tmp_path):
    """A GUI may hold values as text: normalise gives what the parser gives."""
    from_file = _build(tmp_path)
    raw = read_config(_write(tmp_path))
    as_text = {sec: ({k: (", ".join(map(str, v)) if isinstance(v, list) else str(v))
                      for k, v in raw[sec].items()} if isinstance(raw[sec], dict) else raw[sec])
               for sec in raw}
    as_text["fits"] = [{k: (v if k == "_label" else
                            ", ".join(map(str, v)) if isinstance(v, list) else str(v))
                        for k, v in fit.items()} for fit in raw["fits"]]
    assert normalise(as_text) == raw
    from_text = build_from_raw(as_text, base_dir=tmp_path)
    for name in ("data", "model", "geometry", "retrieval", "windows", "plot"):
        assert from_text[name] == from_file[name], name
    assert [list(p) for p in from_text["params"]] == [list(p) for p in from_file["params"]]


def test_normalise_is_idempotent(tmp_path):
    raw = read_config(_write(tmp_path))
    assert normalise(normalise(raw)) == raw


# =============================================================================
# Per-key checks (14g)
# =============================================================================

@pytest.mark.parametrize("extra, message", [
    (dict(extra_geom="plume_thickness : -1"), "GEOMETRY.plume_thickness: must be a positive number"),
    (dict(extra_geom="source_temp : hot"), "GEOMETRY.source_temp: must be a positive number"),
    (dict(extra_geom="fit_source_temp : 1"), "GEOMETRY.fit_source_temp: expected True or False"),
    (dict(extra_retr="fov : 1.5"), "RETRIEVAL.fov: must be a number in (0, 1)"),
    (dict(extra_data="max_spec : 0"), "DATA.max_spec: must be a positive integer"),
    (dict(extra_fit="polydeg : -1"), "FIT0.polydeg: must be a non-negative integer"),
    (dict(extra_fit="continuum : CO2"), "FIT0.continuum: unknown continuum gas ['CO2']"),
    (dict(extra_defaults="ASH_comp : 2"), "DEFAULTS.ash_comp: must be a number in [0, 1]"),
    (dict(extra_defaults="SO2_max_size : 5"), "unknown field 'max_size' for SO2"),
    (dict(extra_retr="sigma_prior_fov : 0"), "the prior 1-sigma of parameter 'fov'"),
])
def test_per_key_errors(tmp_path, extra, message):
    with pytest.raises(ConfigError) as info:
        _build(tmp_path, **extra)
    assert message in str(info.value)


def test_required_per_geometry(tmp_path):
    raw = read_config(_write(tmp_path))
    raw["GEOMETRY"] = {"type": "emission", "elev": 30.0}
    with pytest.raises(ConfigError) as info:
        validate(raw)
    text = str(info.value)
    for name in ("plume_height", "plume_thickness", "obs_height", "atm_path"):
        assert f"GEOMETRY.{name}: required field is missing" in text
    assert "GEOMETRY.elev" not in text


def test_inactive_window_is_not_checked(tmp_path):
    path = _write(tmp_path, extra_fit="")
    text = path.read_text().replace("*FIT1\nfit         : True",
                                    "*FIT1\nfit         : False\npolydeg : -4")
    path.write_text(text)
    assert len(build_from_raw(read_config(path), base_dir=tmp_path)["windows"]) == 1


# =============================================================================
# Paths, write_config and the reference (14g, 14h)
# =============================================================================

def test_path_keys_come_from_the_catalogue():
    assert set(PATH_KEYS) == {
        ("DATA", "data_dir"), ("MODEL", "cache_dir"),
        ("GEOMETRY", "atm_path"), ("PREPROCESS", "self_file"),
    }
    assert RFM_ATM_KEYS == {("GEOMETRY", "atm_path")}


def test_written_comments_come_from_the_catalogue(tmp_path):
    out = tmp_path / "out.ini"
    write_config(read_config(_write(tmp_path, extra_defaults="SO2_conc : 2")), out)
    text = out.read_text(encoding="utf-8")
    assert "# " + keys.KEYS[("GEOMETRY", "pathlength")].comment() in text
    assert "# " + keys.KEYS[("FIT", "fit_window")].comment() in text
    assert "# " + keys.SECTION_HELP["DEFAULTS"] in text


def test_parser_has_no_comment_table():
    import plumeIR.config.parser as parser
    assert not hasattr(parser, "_COMMENTS")


def test_reference_is_up_to_date():
    """docs/CONFIG_REFERENCE.md is generated; regenerate it when keys change:
    python -m plumeIR.config.keydoc docs/CONFIG_REFERENCE.md"""
    committed = (_ROOT / "docs" / "CONFIG_REFERENCE.md").read_text(encoding="utf-8")
    assert committed == render_reference()


def test_reference_lists_every_key():
    text = render_reference()
    for key in keys.KEYS.values():
        assert f"| `{key.name}` |" in text, key.name
    for pattern in keys.PATTERNS.values():
        assert f"| `{pattern.key.name}` |" in text


# =============================================================================
# *PREPROCESS (2026-11-15): subtract_self / self_file moved from *GEOMETRY
# =============================================================================

def test_preprocess_section_builds(tmp_path):
    cfg = _build(tmp_path, extra_geom="", extra_retr="")
    assert cfg["preprocess"] == PreprocessConfig()


def test_moved_keys_read_from_geometry_with_a_warning(tmp_path, caplog):
    import logging
    with caplog.at_level(logging.WARNING, logger="plumeIR.config.parser"):
        cfg = _build(tmp_path, extra_geom="subtract_self : True")
    assert cfg["preprocess"].subtract_self is True
    assert "has moved to *PREPROCESS" in caplog.text


def test_preprocess_section_wins_over_the_old_place(tmp_path, caplog):
    import logging
    from plumeIR.config.parser import migrate_moved_keys
    raw = {"GEOMETRY": {"subtract_self": False}, "PREPROCESS": {"subtract_self": True}}
    with caplog.at_level(logging.WARNING, logger="plumeIR.config.parser"):
        out = migrate_moved_keys(raw)
    assert out["PREPROCESS"]["subtract_self"] is True
    assert "subtract_self" not in out["GEOMETRY"]
    assert raw["GEOMETRY"] == {"subtract_self": False}      # input untouched
    assert "also set in *PREPROCESS" in caplog.text


def test_moved_keys_are_written_in_the_new_section(tmp_path):
    from plumeIR.config.parser import read_config, write_config
    src = _write(tmp_path, extra_geom="subtract_self : True")
    raw = read_config(src)
    out = tmp_path / "rewritten.ini"
    write_config(raw, out)
    text = out.read_text()
    assert "*PREPROCESS" in text
    assert text.index("subtract_self") > text.index("*PREPROCESS")
