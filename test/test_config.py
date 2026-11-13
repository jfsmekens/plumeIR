"""
Tests for plumeIR.config: parser, validator, builder, and ParameterSet.

Run with:  pytest test/test_config.py -v
"""

from __future__ import annotations

import os
import tempfile
from pathlib import Path

import numpy as np
import pytest

from plumeIR.config.parser import read_config, write_config
from plumeIR.config.validator import validate
from plumeIR.config.builder import (
    build_from_config,
    build_from_raw,
    build_fit_window,
    build_parameter_set,
    build_retrieval,
    _resolve_defaults,
)
from plumeIR.exceptions import ConfigError
from plumeIR.config.objects import (
    GeometryType,
    LayerGeometry,
    EmissionGeometry,
    FitWindowConfig,
    Retrieval,
)
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_HERE = Path(__file__).parent
_LAYER_CONFIG = _HERE / "configs" / "test_layer.ini"
_EMISSION_CONFIG = _HERE / "configs" / "test_emission.ini"


def _write_tmp(content: str) -> Path:
    """Write a config string to a temp file and return its path."""
    fd, name = tempfile.mkstemp(suffix=".ini")
    os.close(fd)
    Path(name).write_text(content, encoding="utf-8")
    return Path(name)


def _minimal_emission_config(atm_path: Path, extra_geom="") -> str:
    """Return a minimal valid emission config string (atm_path must exist)."""
    return f"""
*DATA
data_dir : /tmp

*MODEL

*GEOMETRY
type            : emission
plume_height    : 2.0
plume_thickness : 0.4
elev            : 30
obs_height      : 0.2
atm_path        : {atm_path}
{extra_geom}

*RETRIEVAL

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : H2O
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 2
"""


def _minimal_layer_config(extra_data="", extra_geom="", extra_retr="",
                           extra_fit="") -> str:
    """Return a minimal valid layer config string."""
    return f"""
*DATA
data_dir : /tmp

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

*FIT0
fit         : True
fit_window  : 2400, 2550
atm_gases   : H2O
plume_gases : SO2
plume_aero  : None
targets     : SO2
polydeg     : 2
{extra_fit}
"""


# =============================================================================
# Parser tests
# =============================================================================

class TestParser:

    def test_parses_layer_config(self):
        raw = read_config(_LAYER_CONFIG)
        assert raw["GEOMETRY"]["type"] == "layer"
        assert raw["DATA"]["data_dir"] == "/tmp/plumeIR_test_data"
        assert len(raw["fits"]) == 3

    def test_active_fits_parsed(self):
        raw = read_config(_LAYER_CONFIG)
        active = [f for f in raw["fits"] if f.get("fit") is True]
        assert len(active) == 2

    def test_fit_window_is_list(self):
        raw = read_config(_LAYER_CONFIG)
        fw = raw["fits"][0]["fit_window"]
        assert isinstance(fw, list)
        assert len(fw) == 2

    def test_bool_coercion(self):
        raw = read_config(_LAYER_CONFIG)
        assert raw["RETRIEVAL"]["use_bounds"] is True
        assert raw["RETRIEVAL"]["update_params"] is False

    def test_none_coercion(self):
        raw = read_config(_LAYER_CONFIG)
        assert raw["GEOMETRY"].get("e_file") is None or \
               raw["GEOMETRY"].get("e_file") == "None" or \
               "e_file" not in raw["GEOMETRY"]
        # plume_aero None in FIT1
        assert raw["fits"][1]["plume_aero"] is None

    def test_defaults_section_parsed(self):
        raw = read_config(_LAYER_CONFIG)
        assert "so2_conc" in raw["DEFAULTS"]
        assert raw["DEFAULTS"]["so2_conc"] == 2.0

    def test_model_section_parsed(self):
        # 25 points/cm-1 = 0.04 cm-1 sampling, matching v2.  A previous note
        # here argued 10 was enough because the instrument resolves ~0.25 cm-1.
        # That holds for the layer MIR windows but not for the 500 cm-1 emission
        # window, where nperwn 10 biases the SO2 column by 6% (measured
        # against nperwn 40 on 50 real spectra per geometry).
        raw = read_config(_LAYER_CONFIG)
        assert raw["MODEL"]["nperwn"] == 25

    def test_missing_file_raises(self):
        with pytest.raises(FileNotFoundError):
            read_config("/nonexistent/path/config.ini")

    def test_write_roundtrip(self, tmp_path):
        raw = read_config(_LAYER_CONFIG)
        out = tmp_path / "roundtrip.ini"
        write_config(raw, out)
        raw2 = read_config(out)
        assert raw2["GEOMETRY"]["type"] == raw["GEOMETRY"]["type"]
        assert raw2["DATA"]["data_dir"] == raw["DATA"]["data_dir"]
        assert len(raw2["fits"]) == len(raw["fits"])


# =============================================================================
# Validator tests
# =============================================================================

class TestValidator:

    def test_valid_layer_config_passes(self):
        raw = read_config(_LAYER_CONFIG)
        validate(raw)  # should not raise

    def test_missing_data_dir(self):
        cfg = _write_tmp(_minimal_layer_config())
        raw = read_config(cfg)
        del raw["DATA"]["data_dir"]
        with pytest.raises(ConfigError, match="DATA.data_dir"):
            validate(raw)

    def test_invalid_geometry_type(self):
        cfg = _write_tmp(_minimal_layer_config(
            extra_geom="type : invalid"
        ).replace("type        : layer", ""))
        raw = read_config(cfg)
        raw["GEOMETRY"]["type"] = "invalid"
        with pytest.raises(ConfigError, match="GEOMETRY.type"):
            validate(raw)

    def test_missing_layer_field(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        del raw["GEOMETRY"]["pathlength"]
        with pytest.raises(ConfigError, match="GEOMETRY.pathlength"):
            validate(raw)

    def test_invalid_retrieval_method(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["RETRIEVAL"]["retrieval_method"] = "lm"
        with pytest.raises(ConfigError, match="RETRIEVAL.retrieval_method"):
            validate(raw)

    def test_invalid_apod_type(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["RETRIEVAL"]["apod_type"] = "Hann"
        with pytest.raises(ConfigError, match="RETRIEVAL.apod_type"):
            validate(raw)

    def test_unknown_gas_species(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["fits"][0]["atm_gases"] = ["UNOBTANIUM"]
        with pytest.raises(ConfigError, match="unknown species"):
            validate(raw)

    def test_inverted_fit_window(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["fits"][0]["fit_window"] = [2550, 2400]
        with pytest.raises(ConfigError, match="lower bound"):
            validate(raw)

    def test_all_fits_inactive(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["fits"][0]["fit"] = False
        with pytest.raises(ConfigError, match="at least one must be active"):
            validate(raw)

    def test_no_fits(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["fits"] = []
        with pytest.raises(ConfigError, match="No \\*FIT sections"):
            validate(raw)

    def test_species_target_in_multiple_windows(self):
        # A species may be a target in only one window (so the results table
        # has a single trusted retrieval per species).
        import copy
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["fits"][0]["targets"] = ["SO2"]
        second = copy.deepcopy(raw["fits"][0])
        second["_label"] = "FIT1"
        second["fit_window"] = [2700, 2850]
        second["targets"] = ["SO2"]
        raw["fits"].append(second)
        with pytest.raises(ConfigError, match="multiple windows"):
            validate(raw)

    def test_invalid_defaults_key_format(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["DEFAULTS"]["badkey"] = 1.0
        with pytest.raises(ConfigError, match="DEFAULTS.badkey"):
            validate(raw)

    def test_invalid_defaults_species(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["DEFAULTS"]["xyz_conc"] = 1.0
        with pytest.raises(ConfigError, match="unknown species 'XYZ'"):
            validate(raw)

    def test_all_errors_reported_together(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["GEOMETRY"]["pathlength"] = -1.0
        raw["fits"][0]["fit_window"] = [2550, 2400]
        with pytest.raises(ConfigError) as exc_info:
            validate(raw)
        msg = str(exc_info.value)
        assert "pathlength" in msg
        assert "lower bound" in msg


# =============================================================================
# ParameterSet tests
# =============================================================================

class TestParameterSet:

    def _make_params(self) -> ParameterSet:
        return ParameterSet([
            Parameter("poly0",    1.0,  True,  (-10., 10.), ParameterKind.POLYNOMIAL),
            Parameter("poly1",    0.0,  True,  (-10., 10.), ParameterKind.POLYNOMIAL),
            Parameter("H2O",      1e4,  True,  (0., 4e4),   ParameterKind.GAS_ATM,    ref_column=1e4),
            Parameter("SO2",      2.0,  True,  (0., 5000.), ParameterKind.GAS_PLUME,  ref_column=2.0),
            Parameter("fov",      0.02, True,  (0.001, 0.2),ParameterKind.SPECTRAL),
            Parameter("nu_shift", 0.0,  False, (-2., 2.),   ParameterKind.SPECTRAL),
        ])

    def test_len(self):
        ps = self._make_params()
        assert len(ps) == 6

    def test_getitem(self):
        ps = self._make_params()
        assert ps["SO2"].value == 2.0

    def test_contains(self):
        ps = self._make_params()
        assert "SO2" in ps
        assert "HF"  not in ps

    def test_duplicate_names_raise(self):
        with pytest.raises(ValueError, match="Duplicate"):
            ParameterSet([
                Parameter("SO2", 1.0, True, (0., 10.), ParameterKind.GAS_PLUME),
                Parameter("SO2", 2.0, True, (0., 10.), ParameterKind.GAS_PLUME),
            ])

    def test_immutable(self):
        ps = self._make_params()
        with pytest.raises(AttributeError):
            ps.new_attr = 42

    def test_invalid_bounds_raise(self):
        with pytest.raises(ValueError, match="lower bound"):
            Parameter("SO2", 1.0, True, (10., 0.), ParameterKind.GAS_PLUME)

    def test_p0_vector_only_varied(self):
        ps = self._make_params()
        p0 = ps.p0_vector()
        # nu_shift has vary=False → excluded
        assert len(p0) == 5
        assert "nu_shift" not in ps.varied_names()

    def test_bounds_tuple_shape(self):
        ps = self._make_params()
        lo, hi = ps.bounds_tuple()
        assert lo.shape == hi.shape == (len(ps.varied()),)

    def test_unpack_roundtrip(self):
        ps   = self._make_params()
        p0   = ps.p0_vector()
        p0_2 = p0 * 1.1
        d    = ps.unpack(p0_2)
        for param, val in zip(ps.varied(), p0_2):
            assert d[param.name] == pytest.approx(val)

    def test_unpack_includes_fixed(self):
        ps = self._make_params()
        d  = ps.unpack(ps.p0_vector())
        assert "nu_shift" in d
        assert d["nu_shift"] == pytest.approx(0.0)

    def test_unpack_wrong_length(self):
        ps = self._make_params()
        with pytest.raises(ValueError, match="vector length"):
            ps.unpack(np.array([1.0, 2.0]))

    def test_by_kind(self):
        ps    = self._make_params()
        gases = ps.by_kind(ParameterKind.GAS_ATM)
        assert len(gases) == 1
        assert gases[0].name == "H2O"

    def test_prior_covariance_shape(self):
        ps  = self._make_params()
        cov = ps.prior_covariance()
        n   = len(ps.varied())
        assert cov.shape == (n, n)
        # Diagonal
        off = cov - np.diag(np.diag(cov))
        assert np.all(off == 0)


# =============================================================================
# Builder tests
# =============================================================================

class TestBuilder:

    def test_build_from_config_layer(self, tmp_path):
        # Create dummy paths expected by the validator
        cfg_text = _minimal_layer_config()
        cfg_path = tmp_path / "test.ini"
        cfg_path.write_text(cfg_text)

        result = build_from_config(cfg_path)
        assert isinstance(result["geometry"], LayerGeometry)
        assert isinstance(result["retrieval"], Retrieval)
        assert len(result["windows"]) == 1
        assert len(result["params"]) == 1

    def test_geometry_type_layer(self, tmp_path):
        cfg_text = _minimal_layer_config()
        cfg_path = tmp_path / "test.ini"
        cfg_path.write_text(cfg_text)

        result = build_from_config(cfg_path)
        assert result["retrieval"].geometry_type == GeometryType.LAYER

    def test_defaults_override_standards(self, tmp_path):
        # SO2_conc sets the *reference concentration* (ppmv), which feeds
        # ref_column (SCD).  The parameter VALUE is the scaling factor (0.01
        # for plume gases), unaffected by conc.  Overriding conc should raise
        # ref_column relative to the standards default (2.0 ppmv).
        from plumeIR.units import vmr_to_scd

        # Baseline (no override) — standards SO2 conc = 2.0 ppmv
        base_cfg = _minimal_layer_config()
        base_path = tmp_path / "base.ini"
        base_path.write_text(base_cfg)
        base_ref = build_from_config(base_path)["params"][0]["SO2"].ref_column

        # Override conc to 99.0 ppmv
        cfg_text = _minimal_layer_config() + "\n*DEFAULTS\nSO2_conc : 99.0\n"
        cfg_path = tmp_path / "test.ini"
        cfg_path.write_text(cfg_text)

        result = build_from_config(cfg_path)
        so2    = result["params"][0]["SO2"]
        # Value stays the plume scaling default
        assert so2.value == pytest.approx(0.01)
        # ref_column reflects the higher reference concentration
        assert so2.ref_column > base_ref
        assert so2.ref_column == pytest.approx(base_ref * 99.0 / 2.0, rel=1e-6)

    def test_per_window_fit_gas_temp_override(self, tmp_path):
        # Geometry says fit_gas_temp=False; FIT0 overrides to True
        cfg_text = _minimal_layer_config(extra_fit="fit_gas_temp : True")
        cfg_path = tmp_path / "test.ini"
        cfg_path.write_text(cfg_text)

        result = build_from_config(cfg_path)
        ps = result["params"][0]
        assert "gas_temp" in ps

    # --- Scaling-factor convention ---

    def test_gas_param_is_scaling_factor(self, tmp_path):
        import math
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(_minimal_layer_config())
        ps = build_from_config(cfg_path)["params"][0]
        # plume gas SO2: value is scaling 0.01, bounds (0, inf), ref_column is SCD
        so2 = ps["SO2"]
        assert so2.value == pytest.approx(0.01)
        assert so2.bounds[0] == 0.0
        assert math.isinf(so2.bounds[1])
        assert so2.ref_column > 1e10   # an SCD in molec/cm², not ppmv

    def test_atm_gas_scaling_starts_at_one(self, tmp_path):
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(_minimal_layer_config())
        ps = build_from_config(cfg_path)["params"][0]
        assert ps["H2O"].value == pytest.approx(1.0)

    def test_h2o_continuum_bounds_when_enabled(self, tmp_path):
        # With `continuum : H2O`, H2O scaling bounds = (vmr_min/ref, vmr_max/ref)
        import math
        from plumeIR.standards import H2O_VMR_NODES, GAS_DEFAULTS
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(_minimal_layer_config(extra_fit="continuum : H2O"))
        ps = build_from_config(cfg_path)["params"][0]
        h2o = ps["H2O"]
        ref_vmr = GAS_DEFAULTS["H2O"]["conc"]
        assert not math.isinf(h2o.bounds[1])
        assert h2o.bounds[0] == pytest.approx(H2O_VMR_NODES[0] / ref_vmr)
        assert h2o.bounds[1] == pytest.approx(H2O_VMR_NODES[-1] / ref_vmr)

    def test_h2o_linear_by_default(self, tmp_path):
        # Without `continuum`, H2O is linear: bounds (0, +inf)
        import math
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(_minimal_layer_config())
        ps = build_from_config(cfg_path)["params"][0]
        h2o = ps["H2O"]
        assert h2o.bounds[0] == 0.0
        assert math.isinf(h2o.bounds[1])

    def test_temp_bounds_from_lut_nodes(self, tmp_path):
        # gas_temp / atm_temp bounds come from node ranges, NOT TEMP_DEFAULTS
        from plumeIR.standards import PLUME_TEMP_NODES, ATM_TEMP_NODES
        cfg_text = _minimal_layer_config(
            extra_geom="fit_gas_temp : True\nfit_atm_temp : True"
        )
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        assert ps["gas_temp"].bounds == (
            pytest.approx(PLUME_TEMP_NODES[0]), pytest.approx(PLUME_TEMP_NODES[-1])
        )
        assert ps["atm_temp"].bounds == (
            pytest.approx(ATM_TEMP_NODES[0]), pytest.approx(ATM_TEMP_NODES[-1])
        )

    def test_only_layer_uses_the_hot_gas_axis(self):
        """
        Layer plume LUTs keep the original 300-1000 K axis, unchanged; solar
        does not use it (a high-altitude occultation plume is cold).
        """
        from types import SimpleNamespace
        from plumeIR.standards import (EMISSION_PLUME_TEMP_NODES,
                                       PLUME_TEMP_NODES, plume_temp_nodes_for)
        assert np.allclose(PLUME_TEMP_NODES, np.linspace(300.0, 1000.0, 25))
        layer = SimpleNamespace(plume_temp=450.0, fit_gas_temp=False)
        assert np.array_equal(plume_temp_nodes_for(layer), PLUME_TEMP_NODES)

        solar = SimpleNamespace(plume_temp=None)           # no layer-only fields
        assert np.array_equal(plume_temp_nodes_for(solar), EMISSION_PLUME_TEMP_NODES)
        # the temperature the solar reference resolved from the profile counts
        cold = plume_temp_nodes_for(solar, plume_temp=205.0)
        assert cold[0] <= 205.0 <= cold[-1]

    def test_emission_axis_default_covers_ambient(self):
        """
        The emission plume axis is cold (220-400 K), not the hot-gas axis.

        An ambient plume (289.7 K at 1 km over Stromboli) lay outside the old
        300-1000 K table and was silently clamped to its 300 K node.
        """
        from types import SimpleNamespace
        from plumeIR.standards import EMISSION_PLUME_TEMP_NODES, plume_temp_nodes_for
        for fit in (False, True):
            ax = plume_temp_nodes_for(SimpleNamespace(plume_temp=289.7, fit_tplume=fit))
            assert np.array_equal(ax, EMISSION_PLUME_TEMP_NODES)
            assert ax[0] <= 220.0 and ax[-1] >= 400.0
            assert np.all(np.diff(ax) == 10.0)
        # an unresolved a priori falls back to the default axis
        none = SimpleNamespace(plume_temp=None, fit_tplume=True)
        assert np.array_equal(plume_temp_nodes_for(none), EMISSION_PLUME_TEMP_NODES)

    def test_emission_axis_widens_for_a_colder_or_hotter_run(self, monkeypatch):
        """
        The axis is derived from what the run needs, never clamped to fit.

        A plume colder than the default axis, or a fitted temperature whose
        upper bound passes 400 K, widens the axis in 10 K steps; the changed axis
        is part of the LUT cache key, so the table rebuilds.
        """
        from types import SimpleNamespace
        from plumeIR import standards
        cold = standards.plume_temp_nodes_for(
            SimpleNamespace(plume_temp=207.0, fit_tplume=False))
        assert cold[0] <= 207.0 <= cold[-1] and cold[0] < 220.0
        assert cold[-1] == 400.0

        monkeypatch.setattr(standards, "TEMP_DEFAULTS", standards.freeze({
            **standards.TEMP_DEFAULTS, "plume_temp": {"below": 5.0, "above": 150.0},
        }))
        hot = standards.plume_temp_nodes_for(
            SimpleNamespace(plume_temp=289.7, fit_tplume=True))
        assert hot[-1] >= 289.7 + 150.0 and hot[0] == 220.0
        assert np.all(np.diff(hot) == 10.0)
        # a FIXED temperature does not pull the axis out to the fit bound
        fixed = standards.plume_temp_nodes_for(
            SimpleNamespace(plume_temp=289.7, fit_tplume=False))
        assert fixed[-1] == 400.0

    def test_free_plume_temp_starts_inside_its_bounds(self, tmp_path):
        """
        A fitted ambient plume temperature starts within its bounds.

        Regression: with the plume axis starting at 300 K the lower bound was
        max(ambient - 5, 300) = 300 K, above the 289.7 K a priori, and every
        fit was rejected with "Initial guess is outside of provided bounds".
        """
        atm = tmp_path / "mls.atm"
        atm.write_text("! dummy atmosphere\n")
        cfg_path = tmp_path / "emis.ini"
        cfg_path.write_text(_minimal_emission_config(
            atm, extra_geom="plume_temp : 289.7\nfit_tplume : True"))
        ps = build_from_config(cfg_path)["params"][0]
        p = ps["plume_temp"]
        assert p.bounds[0] <= p.value <= p.bounds[1]
        assert p.bounds[0] < p.value < p.bounds[1]
        # ...and inside the table that the reference build will query
        from plumeIR.standards import plume_temp_nodes_for
        axis = plume_temp_nodes_for(build_from_config(cfg_path)["geometry"])
        assert axis[0] <= p.bounds[0] and p.bounds[1] <= axis[-1]

    def test_aero_r0_bounds_intersect_lut_and_physical_range(self, tmp_path):
        """
        r0 bounds are the LUT span intersected with min_size/max_size.

        The LUT span alone is too permissive as a *fit* bound: H2SO4's TIR
        extinction goes grey above r0 ~3 um, and a featureless continuum is
        degenerate with dt_dist and the background polynomial.  min_size /
        max_size were declared in AERO_DEFAULTS and validated but never read
        (like `sounding`, P6-2e); builder.py now honours them.
        """
        from plumeIR.standards import AERO_R0_NODES, AERO_DEFAULTS
        cfg_text = _minimal_layer_config().replace(
            "plume_aero  : None", "plume_aero  : ASH"
        )
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        lo = max(float(AERO_R0_NODES[0]),  AERO_DEFAULTS["ASH"]["min_size"])
        hi = min(float(AERO_R0_NODES[-1]), AERO_DEFAULTS["ASH"]["max_size"])
        assert ps["r0_ASH"].bounds == (pytest.approx(lo), pytest.approx(hi))
        # mass scaling unbounded above
        import math
        assert math.isinf(ps["ASH"].bounds[1])

    def test_h2so4_r0_capped_below_the_lut_span(self, tmp_path):
        """H2SO4 is capped at 2 um, well inside the LUT's 10 um."""
        from plumeIR.standards import H2SO4_R0_NODES
        cfg_text = _minimal_layer_config().replace(
            "plume_aero  : None", "plume_aero  : H2SO4"
        )
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        assert ps["r0_H2SO4"].bounds == (pytest.approx(0.1), pytest.approx(2.0))
        assert ps["r0_H2SO4"].bounds[1] < float(H2SO4_R0_NODES[-1])

    def test_config_can_override_the_size_bounds(self, tmp_path):
        cfg_text = _minimal_layer_config().replace(
            "plume_aero  : None", "plume_aero  : ASH"
        ) + "\n*DEFAULTS\nASH_max_size : 4.0\n"
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        assert ps["r0_ASH"].bounds[1] == pytest.approx(4.0)

    def test_h2so4_composition_param(self, tmp_path):
        from plumeIR.standards import H2SO4_WT_NODES
        cfg_text = _minimal_layer_config().replace(
            "plume_aero  : None", "plume_aero  : H2SO4"
        ) + "\n"
        # fit_aero_composition default False → wt fixed
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        assert "wt_H2SO4" in ps
        assert ps["wt_H2SO4"].vary is False
        assert ps["wt_H2SO4"].bounds == (
            pytest.approx(H2SO4_WT_NODES[0]), pytest.approx(H2SO4_WT_NODES[-1])
        )

    def test_aero_ref_mass_from_concentration_and_thickness(self, tmp_path):
        # ref_column (mass column) = ref_conc [g/m³] × plume_thickness [m]
        from plumeIR.standards import AERO_DEFAULTS
        cfg_text = _minimal_layer_config(
            extra_geom="plume_thickness : 0.05"
        ).replace("plume_aero  : None", "plume_aero  : ASH")
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        ref_conc = AERO_DEFAULTS["ASH"]["conc"]          # g/m³
        expected = ref_conc * 0.05 * 1e3                 # g/m²
        assert ps["ASH"].value == pytest.approx(1.0)     # scaling
        assert ps["ASH"].ref_column == pytest.approx(expected)

    def test_aero_ref_mass_scales_with_thickness(self, tmp_path):
        # Doubling plume_thickness doubles the reference mass column
        def _ref(thick):
            cfg = _minimal_layer_config(
                extra_geom=f"plume_thickness : {thick}"
            ).replace("plume_aero  : None", "plume_aero  : ASH")
            path = tmp_path / f"t_{thick}.ini"
            path.write_text(cfg)
            return build_from_config(path)["params"][0]["ASH"].ref_column
        assert _ref(0.10) == pytest.approx(2.0 * _ref(0.05), rel=1e-6)

    def test_h2so4_composition_free_when_flagged(self, tmp_path):
        cfg_text = _minimal_layer_config(
            extra_retr="fit_aero_composition : True"
        ).replace("plume_aero  : None", "plume_aero  : H2SO4")
        cfg_path = tmp_path / "t.ini"
        cfg_path.write_text(cfg_text)
        ps = build_from_config(cfg_path)["params"][0]
        assert ps["wt_H2SO4"].vary is True

    # --- Emission geometry parameters ---

    def test_emission_builds_atmosphere_and_temp_offsets(self, tmp_path):
        from plumeIR.standards import H2O_SC_NODES
        from plumeIR.parameters.parameter import ParameterKind
        atm = tmp_path / "mls.atm"
        atm.write_text("! dummy atmosphere\n")
        cfg_path = tmp_path / "emis.ini"
        cfg_path.write_text(_minimal_emission_config(atm))
        ps = build_from_config(cfg_path)["params"][0]
        # H2O_sc present, ATMOSPHERE kind, bounds from H2O_SC_NODES
        assert "H2O_sc" in ps
        assert ps["H2O_sc"].kind == ParameterKind.ATMOSPHERE
        assert ps["H2O_sc"].bounds == (
            pytest.approx(H2O_SC_NODES[0]), pytest.approx(H2O_SC_NODES[-1])
        )
        # dt_prox and dt_dist both present and separate
        assert "dt_prox" in ps
        assert "dt_dist" in ps

    def test_emission_h2o_scaling_zero_disables(self, tmp_path):
        atm = tmp_path / "mls.atm"
        atm.write_text("! dummy\n")
        cfg_path = tmp_path / "emis.ini"
        cfg_path.write_text(_minimal_emission_config(atm, extra_geom="h2o_scaling : 0"))
        ps = build_from_config(cfg_path)["params"][0]
        assert "H2O_sc" not in ps

    def test_emission_dt_dist_can_be_disabled(self, tmp_path):
        atm = tmp_path / "mls.atm"
        atm.write_text("! dummy\n")
        cfg_path = tmp_path / "emis.ini"
        cfg_path.write_text(_minimal_emission_config(atm, extra_geom="fit_tdist : False"))
        ps = build_from_config(cfg_path)["params"][0]
        assert "dt_dist" not in ps
        assert "dt_prox" in ps   # still present

    def test_parameter_set_has_polynomial(self, tmp_path):
        cfg_text = _minimal_layer_config()
        cfg_path = tmp_path / "test.ini"
        cfg_path.write_text(cfg_text)

        result = build_from_config(cfg_path)
        ps    = result["params"][0]
        polys = ps.by_kind(ParameterKind.POLYNOMIAL)
        assert len(polys) == 3  # polydeg=2 → poly0, poly1, poly2

    def test_resolve_defaults(self):
        raw = {"so2_conc": 5.0, "h2so4_size": 2.0, "h2o_max": 3e4}
        resolved = _resolve_defaults(raw)
        assert resolved["SO2"]["conc"] == 5.0
        assert resolved["H2SO4"]["size"] == 2.0
        assert resolved["H2O"]["max"] == 3e4


# =============================================================================
# Config as data: exact round-trip and build_from_raw
# =============================================================================

def _diff_keys(a: dict, b: dict) -> list[str]:
    """
    Paths of entries that differ between two raw configs.

    Reports key paths only, never values, so a failure does not print config
    contents.
    """
    out: list[str] = []
    for section in sorted(set(a) | set(b)):
        sa, sb = a.get(section), b.get(section)
        if section == "fits":
            if len(sa or []) != len(sb or []):
                out.append("fits: window count")
                continue
            for i, (fa, fb) in enumerate(zip(sa, sb)):
                out += [f"fits[{i}].{k}" for k in sorted(set(fa) | set(fb))
                        if fa.get(k, ...) != fb.get(k, ...)]
        elif not isinstance(sa, dict) or not isinstance(sb, dict):
            if sa != sb:
                out.append(section)
        else:
            out += [f"{section}.{k}" for k in sorted(set(sa) | set(sb))
                    if sa.get(k, ...) != sb.get(k, ...)]
            if list(sa) != list(sb):
                out.append(f"{section}: key order")
    return out


class TestConfigAsData:

    @pytest.mark.parametrize("cfg", [_LAYER_CONFIG, _EMISSION_CONFIG],
                             ids=["layer", "emission"])
    def test_write_read_roundtrip_exact(self, cfg, tmp_path):
        """read_config(write_config(read_config(x))) == read_config(x), exactly."""
        raw = read_config(cfg)
        out = tmp_path / "roundtrip.ini"
        write_config(raw, out)
        diffs = _diff_keys(raw, read_config(out))
        assert not diffs, f"entries changed by the round-trip: {diffs}"

        # Writing is a fixed point: a second pass produces the same file.
        again = tmp_path / "again.ini"
        write_config(read_config(out), again)
        assert again.read_text(encoding="utf-8") == out.read_text(encoding="utf-8")

    def test_roundtrip_value_types(self, tmp_path):
        """Every value type the parser produces survives a write and re-read.

        A list key reads back as a list even with one item (M3d, 14f).
        """
        raw = {
            "DATA": {"data_dir": "/data/#2024/run 1/", "filter_files": True,
                     "filter_string": None, "max_spec": 5},
            "MODEL": {"nperwn": 25, "model_padding": 50.5},
            "GEOMETRY": {"type": "layer", "pathlength": 1e-3, "atm_temp": 280.25,
                         "fit_gas_temp": False},
            "RETRIEVAL": {"apod_type": "NB medium", "fov": 0.0213},
            "DEFAULTS": {"so2_conc": 5.0, "h2so4_size": 0.7},
            "fits": [{"_label": "FIT0", "fit": True, "fit_window": [2400, 2550.5],
                      "atm_gases": ["H2O", "CO2"], "plume_gases": ["SO2"],
                      "plume_aero": None, "polydeg": 2}],
        }
        out = tmp_path / "types.ini"
        write_config(raw, out)
        assert read_config(out) == raw

    def test_build_from_raw_matches_build_from_config(self):
        """The file entry point is read_config + build_from_raw, nothing more."""
        from_file = build_from_config(_LAYER_CONFIG)
        from_data = build_from_raw(read_config(_LAYER_CONFIG),
                                   base_dir=_LAYER_CONFIG.resolve().parent)
        for key in ("data", "model", "geometry", "retrieval", "windows"):
            assert from_file[key] == from_data[key], key
        assert [list(p) for p in from_file["params"]] == \
               [list(p) for p in from_data["params"]]

    def test_build_from_raw_does_not_modify_input(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        snapshot = repr(raw)
        build_from_raw(raw)
        assert repr(raw) == snapshot

    def test_build_from_raw_resolves_relative_paths(self, tmp_path):
        (tmp_path / "spectra").mkdir()
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["DATA"]["data_dir"] = "spectra"
        cfg = build_from_raw(raw, base_dir=tmp_path)
        assert cfg["data"].data_dir == tmp_path / "spectra"

    def test_build_from_raw_validates(self):
        raw = read_config(_write_tmp(_minimal_layer_config()))
        raw["GEOMETRY"]["type"] = "invalid"
        with pytest.raises(ConfigError, match="GEOMETRY.type"):
            build_from_raw(raw)
