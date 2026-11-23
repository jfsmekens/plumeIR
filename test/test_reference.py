"""
Tests for plumeIR.reference — GasLUT, AerosolLUT, Reference.

GasLUT.compute() and AerosolLUT.build_*() require RFM / mieEODG and are
not tested here.  These tests cover:
  - GasLUT query interpolation (1-D and 2-D) with synthetic tables
  - GasLUT save / load round-trip
  - GasLUT cache key uniqueness
  - AerosolLUT query interpolation (1-D and 2-D) with synthetic tables
  - Reference dataclass construction

Run with:  pytest test/test_reference.py -v
"""

from __future__ import annotations

import tempfile
from pathlib import Path
from unittest import mock

import numpy as np
import pytest

from plumeIR.reference.gas_lut import (
    GasLUT, _physics_key, _cache_name, _decode_name,
)
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.atm_lut import AtmLayerLUT, derive_t_eff
from plumeIR.reference.reference import Reference
from plumeIR.physics import rad, bbt
from plumeIR.config.objects import GeometryType


# =============================================================================
# Helpers
# =============================================================================

def _make_gas_lut_1d(n_temp: int = 10, n_wn: int = 50) -> GasLUT:
    """Synthetic 1-D GasLUT with linearly increasing OD over temperature."""
    temp_nodes = np.linspace(200.0, 400.0, n_temp)
    wn         = np.linspace(2400.0, 2550.0, n_wn)
    # OD proportional to temperature index for easy testing
    od_table   = np.outer(np.linspace(1.0, 2.0, n_temp), np.ones(n_wn))
    return GasLUT(
        species="SO2", wn=wn, temp_nodes=temp_nodes,
        od_table=od_table, ref_scd=1e17,
    )


def _make_gas_lut_2d(n_temp: int = 8, n_vmr: int = 6, n_wn: int = 40) -> GasLUT:
    """Synthetic 2-D GasLUT (H2O-like)."""
    temp_nodes = np.linspace(200.0, 400.0, n_temp)
    vmr_nodes  = np.geomspace(500.0, 30000.0, n_vmr)
    wn         = np.linspace(1200.0, 1350.0, n_wn)
    # OD = temp_factor * vmr_factor (both linear for easy testing)
    t_fac      = np.linspace(1.0, 2.0, n_temp)
    v_fac      = np.linspace(0.5, 3.0, n_vmr)
    od_table   = np.einsum("i,j->ij", t_fac, v_fac)[:, :, np.newaxis] * np.ones(n_wn)
    return GasLUT(
        species="H2O", wn=wn, temp_nodes=temp_nodes,
        od_table=od_table, ref_scd=5e22, vmr_nodes=vmr_nodes,
    )


def _make_aero_lut_1d(n_r0: int = 8, n_wn: int = 40) -> AerosolLUT:
    r0_nodes = np.geomspace(0.1, 10.0, n_r0)
    wn       = np.linspace(750.0, 1250.0, n_wn)
    od_table = np.outer(np.linspace(0.01, 0.2, n_r0), np.ones(n_wn))
    return AerosolLUT(
        species="ASH", wn=wn, r0_nodes_um=r0_nodes,
        od_table=od_table, ref_mass_g_m2=1e-3,
    )


def _make_aero_lut_2d(n_r0: int = 6, n_wt: int = 5, n_wn: int = 40) -> AerosolLUT:
    r0_nodes = np.geomspace(0.05, 5.0, n_r0)
    wt_nodes = np.linspace(0.40, 0.75, n_wt)
    wn       = np.linspace(750.0, 1250.0, n_wn)
    r_fac    = np.linspace(0.01, 0.1, n_r0)
    w_fac    = np.linspace(0.8, 1.2, n_wt)
    od_table = np.einsum("i,j->ij", r_fac, w_fac)[:, :, np.newaxis] * np.ones(n_wn)
    return AerosolLUT(
        species="H2SO4_aq", wn=wn, r0_nodes_um=r0_nodes,
        od_table=od_table, ref_mass_g_m2=1e-3, wt_nodes=wt_nodes,
    )


# =============================================================================
# GasLUT — 1-D
# =============================================================================

class TestGasLUT1D:

    def test_is_1d(self):
        lut = _make_gas_lut_1d()
        assert not lut.is_2d

    def test_query_at_node_exact(self):
        lut = _make_gas_lut_1d()
        # At the first temperature node the OD should equal od_table[0]
        od = lut.query(lut.temp_nodes[0])
        assert np.allclose(od, lut.od_table[0])

    def test_query_at_last_node(self):
        lut = _make_gas_lut_1d()
        od = lut.query(lut.temp_nodes[-1])
        assert np.allclose(od, lut.od_table[-1])

    def test_query_midpoint(self):
        # Midpoint between first two nodes should be average of their rows
        lut = _make_gas_lut_1d()
        T_mid = (lut.temp_nodes[0] + lut.temp_nodes[1]) / 2
        od    = lut.query(T_mid)
        expected = (lut.od_table[0] + lut.od_table[1]) / 2
        assert np.allclose(od, expected, rtol=1e-6)

    def test_query_below_range_raises(self):
        # Not clamped: a clamp made the absorption use a different temperature
        # from the Planck term (emission geometry) with no signal anywhere.
        lut = _make_gas_lut_1d()
        with pytest.raises(ValueError, match="outside the LUT range"):
            lut.query(lut.temp_nodes[0] - 50)

    def test_query_above_range_raises(self):
        lut = _make_gas_lut_1d()
        with pytest.raises(ValueError, match="outside the LUT range"):
            lut.query(lut.temp_nodes[-1] + 50)

    def test_query_error_names_the_range(self):
        lut = _make_gas_lut_1d()
        lo = lut.temp_nodes[0]
        with pytest.raises(ValueError) as exc:
            lut.query(lo - 10)
        assert lut.species in str(exc.value)
        assert f"{lo:g}" in str(exc.value)

    def test_query_at_edges_allowed_within_roundoff(self):
        lut = _make_gas_lut_1d()
        eps = 1e-12 * (lut.temp_nodes[-1] - lut.temp_nodes[0])
        assert np.allclose(lut.query(lut.temp_nodes[0] - eps), lut.od_table[0])
        assert np.allclose(lut.query(lut.temp_nodes[-1] + eps), lut.od_table[-1])

    def test_query_requires_no_vmr(self):
        lut = _make_gas_lut_1d()
        od = lut.query(300.0)       # no vmr arg — should work
        assert od.shape == (lut.n_wn,)

    def test_output_shape(self):
        lut = _make_gas_lut_1d()
        assert lut.query(300.0).shape == (lut.n_wn,)

    def test_save_load_roundtrip(self, tmp_path):
        lut  = _make_gas_lut_1d()
        path = tmp_path / "so2.npz"
        lut.save(path)
        lut2 = GasLUT.load(path)
        assert lut2.species == lut.species
        assert np.allclose(lut2.wn, lut.wn)
        assert np.allclose(lut2.od_table, lut.od_table)
        assert lut2.ref_scd == pytest.approx(lut.ref_scd)
        assert lut2.vmr_nodes is None


# =============================================================================
# GasLUT — 2-D
# =============================================================================

class TestGasLUT2D:

    def test_is_2d(self):
        lut = _make_gas_lut_2d()
        assert lut.is_2d

    def test_query_requires_vmr(self):
        lut = _make_gas_lut_2d()
        with pytest.raises(ValueError, match="vmr must be provided"):
            lut.query(300.0)

    def test_query_at_node(self):
        lut = _make_gas_lut_2d()
        # At the first (T, VMR) node exactly
        od = lut.query(lut.temp_nodes[0], float(lut.vmr_nodes[0]))
        assert np.allclose(od, lut.od_table[0, 0])

    def test_query_at_last_node(self):
        lut = _make_gas_lut_2d()
        od  = lut.query(lut.temp_nodes[-1], float(lut.vmr_nodes[-1]))
        assert np.allclose(od, lut.od_table[-1, -1])

    def test_output_shape(self):
        lut = _make_gas_lut_2d()
        assert lut.query(300.0, 5000.0).shape == (lut.n_wn,)

    def test_temperature_outside_range_raises(self):
        lut = _make_gas_lut_2d()
        v = float(lut.vmr_nodes[0])
        with pytest.raises(ValueError, match="outside the LUT range"):
            lut.query(lut.temp_nodes[0] - 50, v)
        with pytest.raises(ValueError, match="outside the LUT range"):
            lut.query(lut.temp_nodes[-1] + 50, v)

    def test_vmr_clamped(self):
        lut = _make_gas_lut_2d()
        od_low  = lut.query(300.0, lut.vmr_nodes[0] * 0.01)
        od_min  = lut.query(300.0, float(lut.vmr_nodes[0]))
        assert np.allclose(od_low, od_min)

    def test_save_load_roundtrip(self, tmp_path):
        lut  = _make_gas_lut_2d()
        path = tmp_path / "h2o.npz"
        lut.save(path)
        lut2 = GasLUT.load(path)
        assert lut2.species == "H2O"
        assert lut2.vmr_nodes is not None
        assert np.allclose(lut2.vmr_nodes, lut.vmr_nodes)
        assert np.allclose(lut2.od_table, lut.od_table)


# =============================================================================
# GasLUT — cache key
# =============================================================================

class TestGasLUTCacheKey:

    def _key(self, **kwargs) -> str:
        defaults = dict(
            species="SO2", nperwn=25.0,
            temp_nodes=np.linspace(200, 400, 10),
            pathlength=1.0, pres=1013.0, ref_scd=1e17,
            vmr_nodes=None, continuum=False,
        )
        defaults.update(kwargs)
        return _physics_key(**defaults)

    def test_same_inputs_same_key(self):
        assert self._key() == self._key()

    def test_different_species(self):
        assert self._key(species="SO2") != self._key(species="HCl")

    def test_wn_range_is_not_in_the_physics_key(self):
        # The range lives in the filename so a wider build can be sliced;
        # two ranges with the same physics must share a key but not a name.
        assert self._key() == self._key()
        n1 = _cache_name("SO2", self._key(), 2400.0, 2550.0)
        n2 = _cache_name("SO2", self._key(), 2450.0, 2550.0)
        assert n1 != n2

    def test_different_pathlength(self):
        assert self._key(pathlength=1.0) != self._key(pathlength=2.0)

    def test_different_ref_scd(self):
        assert self._key(ref_scd=1e17) != self._key(ref_scd=2e17)

    def test_with_vmr_nodes(self):
        vmr = np.geomspace(500, 30000, 8)
        assert self._key(vmr_nodes=None) != self._key(vmr_nodes=vmr)

    def test_continuum_flag(self):
        assert self._key(continuum=False) != self._key(continuum=True)


# =============================================================================
# GasLUT — sub-window slicing and the covering cache lookup
# =============================================================================

class TestGasLUTSlicing:
    """
    A LUT built over a wider range already contains a narrower one's grid
    points — RFM evaluates the same monochromatic function on a commensurate
    grid — so a sub-window is served by exact slicing, no interpolation and no
    RFM.  Validated against a direct RFM build (SO2, 750-1250 sliced from
    700-1300 at nperwn 25): OD bit-identical (max |d| = 0.0), grid agreeing to
    2.3e-13 against a 0.04 step, served 896x faster than rebuilding.

    Interpolation would NOT be safe here: the emission window carries
    stratospheric lines ~0.002 cm-1 wide that no practical model grid resolves.
    Hence the refusals below — an off-grid request must raise, never approximate.
    """

    NPERWN = 25.0

    def _wide(self, wn_start=700.0, wn_stop=1300.0) -> GasLUT:
        wn = wn_start + np.arange(
            int(round((wn_stop - wn_start) * self.NPERWN)) + 1) / self.NPERWN
        temps = np.linspace(300.0, 1000.0, 5)
        # A recognisable per-point signature, so a mis-slice cannot pass.
        od = np.sin(wn / 7.0)[None, :] * np.linspace(1.0, 2.0, 5)[:, None]
        return GasLUT(species="SO2", wn=wn, temp_nodes=temps,
                      od_table=od, ref_scd=1e18)

    def test_slice_matches_the_parent_pointwise(self):
        wide = self._wide()
        sub  = wide.slice_window(750.0, 1250.0)
        assert sub.n_wn == 12501
        i = int(np.argmin(np.abs(wide.wn - 750.0)))
        assert np.array_equal(sub.od_table, wide.od_table[:, i:i + sub.n_wn])
        assert np.allclose(sub.wn, wide.wn[i:i + sub.n_wn], atol=1e-9)

    def test_slice_preserves_metadata(self):
        sub = self._wide().slice_window(750.0, 1250.0)
        assert sub.species == "SO2" and sub.ref_scd == 1e18
        assert np.array_equal(sub.temp_nodes, np.linspace(300.0, 1000.0, 5))

    def test_full_range_slice_is_a_no_op(self):
        wide = self._wide()
        same = wide.slice_window(700.0, 1300.0)
        assert np.array_equal(same.od_table, wide.od_table)

    def test_incommensurate_bounds_refuse(self):
        """Off-grid bounds must raise rather than silently interpolate."""
        with pytest.raises(ValueError, match="incommensurate"):
            self._wide().slice_window(750.017, 1250.0)

    def test_uncovered_range_refuses(self):
        with pytest.raises(ValueError):
            self._wide().slice_window(650.0, 1300.0)

    def test_2d_table_slices_on_the_trailing_axis(self):
        wide = self._wide()
        od2  = np.repeat(wide.od_table[:, None, :], 3, axis=1)
        lut  = GasLUT(species="H2O", wn=wide.wn,
                      temp_nodes=wide.temp_nodes, od_table=od2,
                      ref_scd=1e18, vmr_nodes=np.geomspace(200, 5e4, 3))
        sub = lut.slice_window(750.0, 1250.0)
        assert sub.od_table.shape == (5, 3, 12501)


class TestGasLUTCoveringLookup:
    """
    The cache key splits into a physics hash (filename stem) and the wavenumber
    range (filename suffix), so ``load_or_compute`` can find a *covering* build
    and slice it instead of re-running RFM.
    """

    PHYS = dict(species="SO2", nperwn=25.0,
                temp_nodes=np.linspace(300.0, 1000.0, 5),
                pathlength=0.4, pres=902.0, ref_scd=1e18,
                vmr_nodes=None, continuum=False)

    def _seed(self, cache_dir, wn_start, wn_stop, marker: float = 0.0) -> Path:
        """
        Write a cache file the lookup should find.

        ``marker`` offsets the whole table so a test can tell *which* cached
        build a slice came from — two covering builds otherwise carry identical
        values at shared grid points, which is exactly the property that makes
        slicing safe but also makes a naive test unfalsifiable.
        """
        wn = wn_start + np.arange(
            int(round((wn_stop - wn_start) * 25.0)) + 1) / 25.0
        lut = GasLUT(species="SO2", wn=wn,
                     temp_nodes=self.PHYS["temp_nodes"],
                     od_table=np.sin(wn / 7.0)[None, :] * np.ones((5, 1)) + marker,
                     ref_scd=1e18)
        path = Path(cache_dir) / _cache_name(
            "SO2", _physics_key(**self.PHYS), wn_start, wn_stop)
        lut.save(path)
        return path

    def _load(self, cache_dir, wn_start, wn_stop) -> GasLUT:
        """load_or_compute with the RFM path fused: a miss is a test failure."""
        def _boom(*a, **k):
            raise AssertionError("fell through to compute(); cache lookup failed")
        with mock.patch.object(GasLUT, "compute", staticmethod(_boom)):
            return GasLUT.load_or_compute(
                wn_start=wn_start, wn_stop=wn_stop,
                cache_dir=cache_dir, **self.PHYS)

    def test_exact_range_hits(self, tmp_path):
        self._seed(tmp_path, 700.0, 1300.0)
        assert self._load(tmp_path, 700.0, 1300.0).n_wn == 15001

    def test_sub_window_is_sliced_from_a_wider_build(self, tmp_path):
        seeded = self._seed(tmp_path, 700.0, 1300.0)
        sub    = self._load(tmp_path, 750.0, 1250.0)
        assert sub.n_wn == 12501
        # Served from the cache without writing a second file.
        assert list(Path(tmp_path).glob("*.npz")) == [seeded]

    def test_sliced_values_match_the_seeded_table(self, tmp_path):
        self._seed(tmp_path, 700.0, 1300.0)
        wide = self._load(tmp_path, 700.0, 1300.0)
        sub  = self._load(tmp_path, 750.0, 1250.0)
        i = int(np.argmin(np.abs(wide.wn - 750.0)))
        assert np.array_equal(sub.od_table, wide.od_table[:, i:i + sub.n_wn])

    def test_narrowest_covering_candidate_wins(self, tmp_path):
        """Least to load, least to discard — and the marker proves which."""
        self._seed(tmp_path, 600.0, 1400.0, marker=10.0)
        self._seed(tmp_path, 740.0, 1260.0, marker=20.0)   # the tighter cover
        assert len(list(Path(tmp_path).glob("*.npz"))) == 2
        sub = self._load(tmp_path, 750.0, 1250.0)
        assert sub.n_wn == 12501
        assert sub.od_table.min() > 15.0, "slice came from the wider build"

    def test_a_wider_cover_is_still_used_when_it_is_the_only_one(self, tmp_path):
        self._seed(tmp_path, 600.0, 1400.0, marker=10.0)
        sub = self._load(tmp_path, 750.0, 1250.0)
        assert sub.n_wn == 12501 and sub.od_table.min() > 5.0

    def test_a_non_covering_cache_is_not_used(self, tmp_path):
        self._seed(tmp_path, 800.0, 1200.0)          # too narrow
        with pytest.raises(AssertionError, match="fell through to compute"):
            self._load(tmp_path, 750.0, 1250.0)

    def test_different_physics_does_not_match(self, tmp_path):
        self._seed(tmp_path, 700.0, 1300.0)
        with mock.patch.object(
            GasLUT, "compute",
            staticmethod(lambda *a, **k: (_ for _ in ()).throw(
                AssertionError("fell through")))
        ):
            with pytest.raises(AssertionError):
                GasLUT.load_or_compute(
                    wn_start=750.0, wn_stop=1250.0, cache_dir=tmp_path,
                    **{**self.PHYS, "pres": 500.0})

    def test_recompute_bypasses_the_lookup(self, tmp_path):
        self._seed(tmp_path, 700.0, 1300.0)
        with mock.patch.object(
            GasLUT, "compute",
            staticmethod(lambda *a, **k: (_ for _ in ()).throw(
                AssertionError("fell through")))
        ):
            with pytest.raises(AssertionError):
                GasLUT.load_or_compute(
                    wn_start=700.0, wn_stop=1300.0, cache_dir=tmp_path,
                    recompute=True, **self.PHYS)

    def test_filename_round_trip(self):
        name = _cache_name("SO2", "0" * 16, 750.0, 1250.25)
        lo, hi = _decode_name(Path(name))
        assert (lo, hi) == (750.0, 1250.25)


# =============================================================================
# AerosolLUT — 1-D
# =============================================================================

class TestAerosolLUT1D:

    def test_is_1d(self):
        assert not _make_aero_lut_1d().is_2d

    def test_query_at_node(self):
        lut = _make_aero_lut_1d()
        od  = lut.query(float(lut.r0_nodes_um[0]))
        assert np.allclose(od, lut.od_table[0])

    def test_query_at_last_node(self):
        lut = _make_aero_lut_1d()
        od  = lut.query(float(lut.r0_nodes_um[-1]))
        assert np.allclose(od, lut.od_table[-1])

    def test_output_shape(self):
        lut = _make_aero_lut_1d()
        assert lut.query(1.0).shape == (lut.od_table.shape[-1],)

    def test_clamped_below(self):
        lut = _make_aero_lut_1d()
        od_low = lut.query(lut.r0_nodes_um[0] * 0.01)
        od_min = lut.query(float(lut.r0_nodes_um[0]))
        assert np.allclose(od_low, od_min)


# =============================================================================
# AerosolLUT — 2-D (H2SO4)
# =============================================================================

class TestAerosolLUT2D:

    def test_is_2d(self):
        assert _make_aero_lut_2d().is_2d

    def test_query_requires_wt_frac(self):
        lut = _make_aero_lut_2d()
        with pytest.raises(ValueError, match="wt_frac must be provided"):
            lut.query(1.0)

    def test_query_at_corner(self):
        lut = _make_aero_lut_2d()
        od  = lut.query(float(lut.r0_nodes_um[0]), float(lut.wt_nodes[0]))
        assert np.allclose(od, lut.od_table[0, 0])

    def test_output_shape(self):
        lut = _make_aero_lut_2d()
        assert lut.query(1.0, 0.55).shape == (lut.od_table.shape[-1],)

    def test_wt_frac_clamped(self):
        lut = _make_aero_lut_2d()
        od_low = lut.query(1.0, lut.wt_nodes[0] - 0.5)
        od_min = lut.query(1.0, float(lut.wt_nodes[0]))
        assert np.allclose(od_low, od_min)


# =============================================================================
# AerosolLUT — extinction + absorption (emission)
# =============================================================================

class TestAerosolAbsorption:

    def _make_dual(self) -> AerosolLUT:
        r0 = np.geomspace(0.1, 10.0, 6)
        wn = np.linspace(750.0, 1250.0, 30)
        ext = np.outer(np.linspace(0.05, 0.2, 6), np.ones(30))
        abs_ = ext * 0.4   # absorption < extinction
        return AerosolLUT(species="ASH", wn=wn, r0_nodes_um=r0,
                          od_table=ext, ref_mass_g_m2=0.05, od_table_abs=abs_)

    def test_has_absorption(self):
        assert self._make_dual().has_absorption
        # default (no abs table) → False
        lut = AerosolLUT(species="ASH", wn=np.linspace(750, 1250, 30),
                         r0_nodes_um=np.geomspace(0.1, 10, 6),
                         od_table=np.zeros((6, 30)), ref_mass_g_m2=0.05)
        assert not lut.has_absorption

    def test_query_returns_extinction(self):
        lut = self._make_dual()
        assert np.allclose(lut.query(float(lut.r0_nodes_um[0])), lut.od_table[0])

    def test_query_abs_returns_absorption(self):
        lut = self._make_dual()
        assert np.allclose(lut.query_abs(float(lut.r0_nodes_um[0])),
                           lut.od_table_abs[0])

    def test_query_abs_raises_without_table(self):
        lut = AerosolLUT(species="ASH", wn=np.linspace(750, 1250, 30),
                         r0_nodes_um=np.geomspace(0.1, 10, 6),
                         od_table=np.zeros((6, 30)), ref_mass_g_m2=0.05)
        with pytest.raises(ValueError, match="no absorption table"):
            lut.query_abs(1.0)


# =============================================================================
# AtmLayerLUT (emission) — T_eff derivation and analytic temperature offset
# =============================================================================

class TestAtmLayerLUT:

    _WN = np.linspace(700.0, 1300.0, 60)

    def _synthetic(self, T_true=260.0, trans=0.6, n_sc=4):
        """Layer at constant T_true with uniform transmittance `trans`."""
        L_row    = rad(self._WN, T_true) * (1.0 - trans)
        L_table  = np.tile(L_row, (n_sc, 1))
        T_table  = np.full((n_sc, self._WN.size), trans)
        od_table = np.full((n_sc, self._WN.size), -np.log(trans))
        t_eff    = derive_t_eff(self._WN, L_table, T_table)
        nodes    = np.linspace(0.2, 3.0, n_sc)
        return AtmLayerLUT("slice", self._WN, nodes, od_table, t_eff), L_row

    def test_t_eff_recovers_true_temperature(self):
        _, _ = self._synthetic(T_true=275.0)
        L_table = np.tile(rad(self._WN, 275.0) * 0.4, (3, 1))
        T_table = np.full((3, self._WN.size), 0.6)
        t_eff   = derive_t_eff(self._WN, L_table, T_table)
        assert np.allclose(t_eff, 275.0, atol=1e-3)

    def test_transparent_region_uses_fill(self):
        # Fully transparent layer (T=1) → emission 0, T_eff = fill placeholder
        L_table = np.zeros((2, self._WN.size))
        T_table = np.ones((2, self._WN.size))
        t_eff   = derive_t_eff(self._WN, L_table, T_table, fill_temp=250.0)
        assert np.allclose(t_eff, 250.0)

    def test_emission_reconstructs_input(self):
        lut, L_row = self._synthetic(T_true=260.0, trans=0.6)
        L_q, T_q = lut.emission(1.0, dT=0.0)
        assert np.allclose(L_q, L_row, rtol=1e-5)
        assert np.allclose(T_q, 0.6)

    def test_positive_dt_raises_emission(self):
        lut, _ = self._synthetic(T_true=260.0, trans=0.6)
        L0, _ = lut.emission(1.0, dT=0.0)
        L1, _ = lut.emission(1.0, dT=15.0)
        assert np.all(L1 > L0)

    def test_query_interpolates_h2o_sc(self):
        # OD linear in H2O_sc across 3 nodes → quadratic interp recovers it
        # exactly.  OD nodes 0.4, 0.8, 1.2 at sc 0.5, 1.25, 2.0.
        nodes    = np.array([0.5, 1.25, 2.0])
        od_vals  = np.array([0.4, 0.8, 1.2])
        od_table = np.vstack([np.full(self._WN.size, v) for v in od_vals])
        L_table  = np.tile(rad(self._WN, 260.0) * 0.3, (3, 1))
        t_eff    = derive_t_eff(self._WN, L_table, np.exp(-od_table))
        lut = AtmLayerLUT("prox", self._WN, nodes, od_table, t_eff)
        # Query between nodes 0 and 1 (sc=0.875 → OD linearly = 0.6)
        T_q, _ = lut.query(0.875)
        assert np.allclose(T_q, np.exp(-0.6), atol=1e-6)


# =============================================================================
# *LEV layer decomposition (emission reference) — pure algebra, no RFM
# =============================================================================

class TestMieGrid:
    """
    Aerosol optical properties are SMOOTH in wavenumber — broad resonances, no
    lines — so Mie is computed on a coarse grid and interpolated onto the model
    grid.  Measured on H2SO4 (r0 1 um, 65 wt%, 700-1300 cm-1): 1 cm-1 sampling
    costs 0.019% max, against ~25x fewer Mie evaluations than the 0.04 cm-1
    model grid.  These are pure-function guards; no mieEODG call.
    """

    def test_grid_brackets_the_model_grid(self):
        """Covers the model grid, so interpolating back never extrapolates."""
        from plumeIR.reference.aero_lut import _mie_grid
        wn = np.arange(700.0, 1300.0 + 1e-9, 0.04)
        g = _mie_grid(wn)
        assert g[0] <= wn[0] and g[-1] >= wn[-1]
        assert g.size < wn.size / 10
        assert np.all(np.diff(g) > 0)

    def test_grid_is_independent_of_nperwn(self):
        """
        The grid is anchored to absolute wavenumbers, NOT the model grid's
        endpoints — which drift with nperwn (1299.900 / 1299.960 / 1299.975 at
        nperwn 10 / 25 / 40).  mieEODG hashes the wavenumber array by content,
        so inheriting that drift invalidated the Mie cache on every nperwn
        change, for physics that has no nperwn dependence.
        """
        from plumeIR.reference.aero_lut import _mie_grid
        grids = [_mie_grid(np.arange(700.0, 1300.0, 1.0 / n))
                 for n in (10, 25, 40)]
        assert all(np.array_equal(grids[0], g) for g in grids[1:])
        assert grids[0][0] == 700.0 and grids[0][-1] == 1300.0

    def test_grid_is_independent_of_window_padding(self):
        """A padding change must not move the grid off its integer anchor."""
        from plumeIR.reference.aero_lut import _mie_grid
        g = _mie_grid(np.arange(710.3, 1287.6, 0.04))
        assert g[0] == 710.0 and g[-1] == 1288.0
        assert np.allclose(np.diff(g), 1.0)

    def test_falls_back_when_model_grid_is_already_coarse(self):
        """A model grid coarser than the Mie spacing is used unchanged."""
        from plumeIR.reference.aero_lut import _mie_grid
        wn = np.arange(700.0, 1300.0, 5.0)
        assert _mie_grid(wn) is wn or np.array_equal(_mie_grid(wn), wn)

    def test_interpolation_is_exact_for_a_smooth_function(self):
        """
        The approximation the coarse grid rests on.  A smooth Lorentzian-like
        resonance of realistic width must survive 1 cm-1 sampling; the same
        test on a 0.05 cm-1 feature would fail, which is why gas LUTs keep the
        fine grid.
        """
        from plumeIR.reference.aero_lut import _mie_grid, _interp_to_model_grid
        wn = np.arange(700.0, 1300.0 + 1e-9, 0.04)
        smooth = lambda x: 1.0 + 0.5 / (1 + ((x - 1000.0) / 60.0) ** 2)
        g = _mie_grid(wn)
        approx = _interp_to_model_grid(smooth(g)[None, :], g, wn)[0]
        err = np.abs(approx - smooth(wn)) / smooth(wn)
        assert err.max() < 1e-3, f"max error {err.max():.2e}"

    def test_interpolation_preserves_table_shape(self):
        """2-D H2SO4 tables are (n_r0, n_wt, n_wn); only the last axis moves."""
        from plumeIR.reference.aero_lut import _interp_to_model_grid
        wn   = np.linspace(700.0, 1300.0, 2001)
        g    = np.linspace(700.0, 1300.0, 601)
        tab  = np.random.default_rng(0).random((4, 3, g.size))
        out  = _interp_to_model_grid(tab, g, wn)
        assert out.shape == (4, 3, wn.size)
        # nodes that coincide must be reproduced exactly
        np.testing.assert_allclose(out[..., 0], tab[..., 0])
        np.testing.assert_allclose(out[..., -1], tab[..., -1])


class TestLevDecomposition:
    """``_decompose_layers`` recovers per-layer (OD, isolated radiance) from the
    cumulative *LEV columns.  Round-trip: compose known layers into cumulative
    columns, then check exact recovery."""

    _WN = np.linspace(1000.0, 1100.0, 64)

    def _layers(self):
        # Distinct synthetic per-layer OD and isolated radiance.
        OD = {"prox": 0.3 + 0.0 * self._WN,
              "slice": 0.5 + 1e-3 * (self._WN - 1000.0),
              "dist": 0.2 + 0.0 * self._WN}
        L  = {"prox": 80.0 + 0.1 * (self._WN - 1000.0),
              "slice": 50.0 + 0.0 * self._WN,
              "dist": 30.0 + 0.0 * self._WN}
        return OD, L

    def _compose(self, OD, L):
        # Cumulative columns from each altitude to space (the *LEV outputs).
        OD_h2  = OD["dist"]
        OD_h1  = OD["slice"] + OD["dist"]
        OD_obs = OD["prox"] + OD["slice"] + OD["dist"]
        L_h2   = L["dist"]
        L_h1   = L["slice"] + np.exp(-OD["slice"]) * L_h2
        L_obs  = L["prox"]  + np.exp(-OD["prox"])  * L_h1
        # Altitude axis ascending [obs, h1, h2]
        altitudes = np.array([0.5, 1.5, 2.5])
        OD_alt = np.vstack([OD_obs, OD_h1, OD_h2])
        L_alt  = np.vstack([L_obs,  L_h1,  L_h2])
        return altitudes, L_alt, OD_alt

    def test_two_layer_roundtrip_recovers_layers(self):
        """
        The 2-layer split (prox/dist meeting at the plume centre) must recover
        its layers exactly from the same cumulative columns.  Composed from a
        two-layer truth, since a 2-tap run has no slice.
        """
        from plumeIR.reference.emission_reference import _decompose_two_layers
        OD = {"prox": 0.4 + 0.0 * self._WN,
              "dist": 0.7 + 1e-3 * (self._WN - 1000.0)}
        L  = {"prox": 90.0 + 0.1 * (self._WN - 1000.0),
              "dist": 40.0 + 0.0 * self._WN}
        L_hp   = L["dist"]
        OD_hp  = OD["dist"]
        L_obs  = L["prox"] + np.exp(-OD["prox"]) * L_hp
        OD_obs = OD["prox"] + OD["dist"]

        altitudes = np.array([0.5, 2.0])
        layers, obs = _decompose_two_layers(
            altitudes, np.vstack([L_obs, L_hp]), np.vstack([OD_obs, OD_hp]),
            0.5, 2.0,
        )
        for name in ("prox", "dist"):
            assert np.allclose(layers[name][0], OD[name], atol=1e-12), f"{name} OD"
            assert np.allclose(layers[name][1], L[name],  atol=1e-10), f"{name} L"
        assert np.allclose(obs, L_obs, atol=1e-12)

    def test_two_layer_and_three_layer_agree_on_the_clear_column(self):
        """
        Both decompositions are exact cuts of the SAME integration, so with no
        plume they must reassemble to identical clear-sky radiance.  This is
        why the clear-sky test cannot arbitrate between them — only a run with
        a plume inserted can.
        """
        from plumeIR.reference.emission_reference import (
            _decompose_layers, _decompose_two_layers,
        )
        OD, L = self._layers()
        altitudes, L_alt, OD_alt = self._compose(OD, L)
        # Add a mid tap at the plume centre so both splits are available.
        h_p = 2.0
        OD_hp = OD["dist"] + 0.5 * OD["slice"]
        L_hp  = 0.5 * L["slice"] + np.exp(-0.5 * OD["slice"]) * L["dist"]
        alts4 = np.array([0.5, 1.5, h_p, 2.5])
        OD4   = np.vstack([OD_alt[0], OD_alt[1], OD_hp, OD_alt[2]])
        L4    = np.vstack([L_alt[0],  L_alt[1],  L_hp,  L_alt[2]])

        l3, obs3 = _decompose_layers(alts4, L4, OD4, 0.5, 1.5, 2.5)
        l2, obs2 = _decompose_two_layers(alts4, L4, OD4, 0.5, h_p)
        assert np.allclose(obs3, obs2, atol=1e-12)

        def assemble3(lay):
            Tp = np.exp(-lay["prox"][0]); Ts = np.exp(-lay["slice"][0])
            return (lay["dist"][1] * Ts * Tp + lay["slice"][1] * Tp + lay["prox"][1])

        def assemble2(lay):
            Tp = np.exp(-lay["prox"][0])
            return lay["dist"][1] * Tp + lay["prox"][1]

        assert np.allclose(assemble3(l3), obs3, atol=1e-10)
        assert np.allclose(assemble2(l2), obs2, atol=1e-10)

    def test_roundtrip_recovers_layers(self):
        from plumeIR.reference.emission_reference import _decompose_layers
        OD, L = self._layers()
        altitudes, L_alt, OD_alt = self._compose(OD, L)
        layers, L_obs = _decompose_layers(altitudes, L_alt, OD_alt, 0.5, 1.5, 2.5)
        for name in ("prox", "slice", "dist"):
            assert np.allclose(layers[name][0], OD[name], atol=1e-12), f"{name} OD"
            assert np.allclose(layers[name][1], L[name],  atol=1e-10), f"{name} L"
        # Observer column is the clear-sky radiance.
        assert np.allclose(L_obs, L_alt[0], atol=1e-12)

    def test_layer_od_sums_to_observer_column(self):
        from plumeIR.reference.emission_reference import _decompose_layers
        OD, L = self._layers()
        altitudes, L_alt, OD_alt = self._compose(OD, L)
        layers, _ = _decompose_layers(altitudes, L_alt, OD_alt, 0.5, 1.5, 2.5)
        total = layers["prox"][0] + layers["slice"][0] + layers["dist"][0]
        assert np.allclose(total, OD_alt[0], atol=1e-12)

    def test_unsorted_altitude_axis(self):
        # _decompose_layers locates rows by altitude, so row order is irrelevant.
        from plumeIR.reference.emission_reference import _decompose_layers
        OD, L = self._layers()
        altitudes, L_alt, OD_alt = self._compose(OD, L)
        perm = [2, 0, 1]
        layers, _ = _decompose_layers(altitudes[perm], L_alt[perm], OD_alt[perm],
                                      0.5, 1.5, 2.5)
        assert np.allclose(layers["prox"][0], OD["prox"], atol=1e-12)
        assert np.allclose(layers["dist"][1], L["dist"], atol=1e-10)


# =============================================================================
# Reference dataclass
# =============================================================================

class TestReference:

    def test_construct(self):
        wn  = np.linspace(2400, 2550, 100)
        ref = Reference(
            wn=wn,
            gas_luts={"SO2": _make_gas_lut_1d()},
            aero_luts={},
            geometry_type=GeometryType.LAYER,
        )
        assert "SO2" in ref.gas_luts
        assert ref.geometry_type == GeometryType.LAYER

    def test_empty_aero(self):
        wn  = np.linspace(2400, 2550, 100)
        ref = Reference(wn=wn)
        assert ref.gas_luts  == {}
        assert ref.aero_luts == {}
