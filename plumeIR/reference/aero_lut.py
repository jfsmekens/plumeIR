"""
AerosolLUT — wrapper around mieEODG LUTs for use in plumeIR v3.

Provides a unified query interface consistent with GasLUT:

    od = aero_lut.query(r0)                    # 1-D species
    od = aero_lut.query(r0, wt_frac)           # H2SO4 (2-D)

    od_actual = od * scaling   where scaling = actual_mass / ref_mass

The underlying MEC [cm²/g] from mieEODG is baked into the table by
multiplying by ``ref_mass_g_m2 × 1e-4`` at build time, so that query()
returns optical depth directly.

Tables are stored on the coarse Mie grid (``MIE_GRID_SPACING_WN``) and
interpolated onto the model grid on first query — see ``AerosolLUT``.

Species handled
---------------
    1-D : ASH, WATER, SOOT, DUST, ICE  →  mieEODG.AerosolLUT
    2-D : H2SO4                         →  mieEODG.MultiDimLUT (r0, weight_fraction)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field, replace
from pathlib import Path

import numpy as np

from plumeIR import species as _species
from plumeIR.constants import H2SO4_AQ
from plumeIR.standards import (
    AERO_R0_NODES as _R0_NODES_UM,
    H2SO4_R0_NODES as _H2SO4_R0_NODES_UM,
    H2SO4_WT_NODES as _H2SO4_WT_NODES,
    H2SO4_TEMP_K as _H2SO4_TEMP_K,
)

logger = logging.getLogger(__name__)


# plumeIR aerosol species → ARIA refractive-index category key (mieEODG
# ``ARIA_DEFAULTS``).  These are the 1-D species (single refractive index);
# H2SO4 is 2-D (r0, weight-fraction) and handled by ``build_h2so4_2d``.
# The refractive-index source of each 1-D aerosol (the registry,
# plumeIR.species); H2SO4_aq has its own 2-D build (build_h2so4_2d).
_SPECIES_TO_ARIA: dict[str, str] = {
    name: _species.get(name).ri_source for name in _species.aerosols()
    if _species.get(name).ri_source and name != H2SO4_AQ
}

# Species in the registry with no usable ARIA refractive-index source.  Treated
# like SiF4: an explicit error rather than a silent placeholder.
_DEFERRED_AEROSOLS: frozenset = frozenset(
    name for name in _species.aerosols() if not _species.get(name).ri_source)


#: Wavenumber spacing [cm⁻¹] for the Mie calculation.
#
# Aerosol optical properties are SMOOTH in wavenumber — broad resonances, no
# lines — so there is nothing at the 0.04 cm⁻¹ model spacing to resolve.  Mie
# is computed on this coarser grid and interpolated onto the model grid.
# Measured on H2SO4 (r0 1 µm, 65 wt%, 700-1300 cm⁻¹) against a 0.2 cm⁻¹
# reference:
#
#     spacing   points   max err   mean err
#       1 cm⁻¹     601    0.019%     0.001%
#       5 cm⁻¹     121    0.179%     0.028%
#      10 cm⁻¹      61    0.693%     0.105%
#
# 1 cm⁻¹ costs 0.019% and is ~25x fewer Mie evaluations than the model grid.
MIE_GRID_SPACING_WN: float = 1.0


def _mie_grid(wn: np.ndarray, spacing: float = MIE_GRID_SPACING_WN) -> np.ndarray:
    """
    Canonical coarse wavenumber grid for the Mie calculation, covering ``wn``.

    Anchored to integer multiples of ``spacing`` in **absolute** wavenumber,
    NOT to the model grid's endpoints.  This matters for cache reuse: the model
    grid is ``arange(wn_start, wn_stop, 1/nperwn)``, whose last point drifts
    with ``nperwn`` (1299.900 / 1299.960 / 1299.975 at nperwn 10 / 25 / 40).
    mieEODG hashes the wavenumber array by content, so inheriting that drift
    invalidated the Mie cache on every ``nperwn`` change — for physics that has
    no ``nperwn`` dependence at all.

    The returned grid brackets ``wn`` (``lo <= wn[0]``, ``hi >= wn[-1]``), so
    interpolating back onto the model grid never extrapolates.  Falls back to
    ``wn`` itself when the model grid is already coarser than ``spacing``.
    """
    wn = np.asarray(wn, dtype=float)
    if wn.size < 4 or float(np.mean(np.diff(wn))) >= spacing:
        return wn
    lo = np.floor(wn[0]  / spacing) * spacing
    hi = np.ceil( wn[-1] / spacing) * spacing
    n  = int(round((hi - lo) / spacing)) + 1
    return lo + spacing * np.arange(n, dtype=float)


def _warn_if_outside_ri(ri_wn: np.ndarray, wn: np.ndarray, species: str) -> None:
    """
    Warn when the requested grid runs past the refractive index's own support.

    ``ARIADatabase.read(..., out_of_range="clip")`` extends the RI as a constant
    rather than failing, which is silent and wrong for a resonant species.  The
    canonical grid can reach up to one ``spacing`` beyond the model grid, so
    this is checked against the grid actually handed to Mie.
    """
    ri_wn = np.asarray(ri_wn, dtype=float)
    lo, hi = float(np.min(ri_wn)), float(np.max(ri_wn))
    if wn[0] < lo - 1e-9 or wn[-1] > hi + 1e-9:
        logger.warning(
            "%s refractive index covers %.1f-%.1f cm-1 but the Mie grid spans "
            "%.1f-%.1f cm-1; the RI is clipped (held constant) outside its "
            "support", species, lo, hi, float(wn[0]), float(wn[-1]),
        )


def _interp_to_model_grid(table: np.ndarray, wn_mie: np.ndarray,
                          wn: np.ndarray) -> np.ndarray:
    """Interpolate a LUT table's trailing wavenumber axis onto the model grid."""
    if wn_mie.shape == wn.shape and np.allclose(wn_mie, wn):
        return table
    flat = table.reshape(-1, table.shape[-1])
    out  = np.empty((flat.shape[0], wn.size), dtype=float)
    for i in range(flat.shape[0]):
        out[i] = np.interp(wn, wn_mie, flat[i])
    return out.reshape(*table.shape[:-1], wn.size)


@dataclass
class AerosolLUT:
    """
    Aerosol optical depth look-up table.

    Wraps mieEODG ``AerosolLUT`` (1-D) or ``MultiDimLUT`` (2-D H2SO4).
    Stores OD at ``ref_mass_g_m2`` so the forward model can write:

        OD = aero_lut.query(r0[, wt_frac]) * scaling

    Parameters
    ----------
    species : str
        Aerosol species name (e.g. ``'ASH'``, ``'H2SO4'``).
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    r0_nodes_um : np.ndarray
        Effective radius axis [µm], log-spaced.
    od_table : np.ndarray
        **Extinction** OD table (from MEC) at ref_mass_g_m2 — used for beam
        transmittance ``T_plume`` in all geometries.
        1-D: shape ``(n_r0, n_wn)``; 2-D: shape ``(n_r0, n_wt, n_wn)``.
    ref_mass_g_m2 : float
        Reference mass column [g/m²].
    wt_nodes : np.ndarray or None
        Weight fraction axis for H2SO4 2-D LUT.  ``None`` for 1-D.
    od_table_abs : np.ndarray or None
        **Absorption** OD table (from MAC), same shape as ``od_table``.
        Populated only for the emission geometry (used for aerosol self-
        emission ``L_plume``).  ``None`` for layer/transmittance geometry.
    wn_model : np.ndarray or None
        The model grid ``query()`` returns on.  ``None`` means ``wn`` is
        already the model grid (the case for directly-constructed LUTs).

    Notes
    -----
    Transmittance uses *extinction* (scattering + absorption); aerosol
    self-emission uses *absorption* (emissivity = absorptivity).  Keeping
    the two tables separate lets the emission forward use each correctly.

    **The tables are stored on the coarse Mie grid, not the model grid.**
    Aerosol optical properties carry no line structure, so the stored physics
    is grid-independent: the same LUT serves any ``nperwn`` and any window it
    covers (``resample``).  It is also ~30x smaller — the H2SO4 2-D tables are
    1.2 MB coarse against 29 MB on a 0.04 cm⁻¹ grid — which is what a
    ``Reference`` pickles and ships to each parallel worker.

    Interpolation onto the model grid happens **once**, on first query, and is
    memoised; per-query cost is therefore identical to storing the model-grid
    table directly.  The memo is dropped on pickling so it is rebuilt per
    process rather than shipped.
    """

    species:        str
    wn:             np.ndarray
    r0_nodes_um:    np.ndarray
    od_table:       np.ndarray
    ref_mass_g_m2:  float
    wt_nodes:       np.ndarray | None = None
    od_table_abs:   np.ndarray | None = None
    wn_model:       np.ndarray | None = None

    def __post_init__(self) -> None:
        # Memoised model-grid tables; not a field (never compared, never
        # pickled) — see __getstate__.
        self._model_tables: tuple[np.ndarray, np.ndarray | None] | None = None

    # ------------------------------------------------------------------
    # Grid handling
    # ------------------------------------------------------------------

    @property
    def model_grid(self) -> np.ndarray:
        """The grid ``query()`` returns on."""
        return self.wn if self.wn_model is None else self.wn_model

    def resample(self, wn_model: np.ndarray) -> AerosolLUT:
        """
        Return a view of this LUT that queries onto a different model grid.

        The stored coarse tables are shared, not copied — only the target grid
        and the (dropped) memo differ.  ``wn_model`` must lie within the Mie
        grid's span, since interpolation does not extrapolate meaningfully.
        """
        wn_model = np.asarray(wn_model, dtype=float)
        if wn_model[0] < self.wn[0] - 1e-9 or wn_model[-1] > self.wn[-1] + 1e-9:
            raise ValueError(
                f"AerosolLUT for {self.species} covers "
                f"{self.wn[0]:.2f}-{self.wn[-1]:.2f} cm-1; cannot resample onto "
                f"{wn_model[0]:.2f}-{wn_model[-1]:.2f} cm-1"
            )
        return replace(self, wn_model=wn_model)

    def _tables_on_model_grid(self) -> tuple[np.ndarray, np.ndarray | None]:
        """Interpolate coarse → model grid once, then memoise."""
        if self._model_tables is None:
            wn_m = self.model_grid
            self._model_tables = (
                _interp_to_model_grid(self.od_table, self.wn, wn_m),
                None if self.od_table_abs is None
                else _interp_to_model_grid(self.od_table_abs, self.wn, wn_m),
            )
        return self._model_tables

    def __getstate__(self) -> dict:
        state = self.__dict__.copy()
        state["_model_tables"] = None      # rebuilt per process, not shipped
        return state

    def __setstate__(self, state: dict) -> None:
        # Tolerate pickles predating wn_model / the memo.
        state.setdefault("wn_model", None)
        state["_model_tables"] = None
        self.__dict__.update(state)

    @property
    def is_2d(self) -> bool:
        return self.wt_nodes is not None

    @property
    def has_absorption(self) -> bool:
        """True if the absorption table is available (emission builds)."""
        return self.od_table_abs is not None

    # ------------------------------------------------------------------
    # Query
    # ------------------------------------------------------------------

    def query(self, r0_um: float, wt_frac: float | None = None) -> np.ndarray:
        """
        Interpolate the **extinction** OD (for transmittance) at the given
        effective radius (and composition).

        Parameters
        ----------
        r0_um : float
            Effective radius [µm].
        wt_frac : float or None
            H2SO4 weight fraction.  Required for 2-D LUT; ignored for 1-D.

        Returns
        -------
        np.ndarray
            Extinction optical depth at ``ref_mass_g_m2``, shape ``(n_wn,)``.
        """
        ext, _ = self._tables_on_model_grid()
        return self._query_table(ext, r0_um, wt_frac)

    def query_abs(self, r0_um: float, wt_frac: float | None = None) -> np.ndarray:
        """
        Interpolate the **absorption** OD (for aerosol self-emission).

        Only available when the LUT was built with absorption (emission
        geometry).  Raises if the absorption table is absent.
        """
        if self.od_table_abs is None:
            raise ValueError(
                f"AerosolLUT for {self.species} has no absorption table "
                f"(build with with_absorption=True for emission geometry)"
            )
        _, absn = self._tables_on_model_grid()
        return self._query_table(absn, r0_um, wt_frac)

    def _query_table(self, table: np.ndarray, r0_um: float,
                     wt_frac: float | None) -> np.ndarray:
        """Interpolate an arbitrary OD table (extinction or absorption)."""
        if self.is_2d:
            if wt_frac is None:
                raise ValueError(
                    f"AerosolLUT for {self.species} is 2-D: wt_frac must be provided"
                )
            return self._interp_2d(table, r0_um, wt_frac)
        return self._interp_1d(table, r0_um)

    def _interp_1d(self, table: np.ndarray, r0_um: float) -> np.ndarray:
        """Log-linear interpolation over r0."""
        log_r = np.log(self.r0_nodes_um)
        log_q = np.log(float(np.clip(r0_um, self.r0_nodes_um[0], self.r0_nodes_um[-1])))
        i = int(np.searchsorted(log_r, log_q)) - 1
        i = int(np.clip(i, 0, len(self.r0_nodes_um) - 2))
        alpha = float(np.clip(
            (log_q - log_r[i]) / (log_r[i + 1] - log_r[i]), 0.0, 1.0
        ))
        return (1.0 - alpha) * table[i] + alpha * table[i + 1]

    def _interp_2d(self, table: np.ndarray, r0_um: float,
                   wt_frac: float) -> np.ndarray:
        """Bilinear interpolation: log-linear in r0, linear in wt_frac."""
        log_r = np.log(self.r0_nodes_um)
        log_q = np.log(float(np.clip(r0_um, self.r0_nodes_um[0], self.r0_nodes_um[-1])))
        i_r = int(np.searchsorted(log_r, log_q)) - 1
        i_r = int(np.clip(i_r, 0, len(self.r0_nodes_um) - 2))
        alpha_r = float(np.clip(
            (log_q - log_r[i_r]) / (log_r[i_r + 1] - log_r[i_r]), 0.0, 1.0
        ))

        wt = float(np.clip(wt_frac, self.wt_nodes[0], self.wt_nodes[-1]))
        i_w = int(np.searchsorted(self.wt_nodes, wt)) - 1
        i_w = int(np.clip(i_w, 0, len(self.wt_nodes) - 2))
        alpha_w = float(np.clip(
            (wt - self.wt_nodes[i_w]) / (self.wt_nodes[i_w + 1] - self.wt_nodes[i_w]),
            0.0, 1.0,
        ))

        od_00 = table[i_r,     i_w    ]
        od_01 = table[i_r,     i_w + 1]
        od_10 = table[i_r + 1, i_w    ]
        od_11 = table[i_r + 1, i_w + 1]

        return (
            (1.0 - alpha_r) * (1.0 - alpha_w) * od_00
            + (1.0 - alpha_r) * alpha_w        * od_01
            +        alpha_r  * (1.0 - alpha_w) * od_10
            +        alpha_r  * alpha_w          * od_11
        )

    # ------------------------------------------------------------------
    # Build
    # ------------------------------------------------------------------

    @classmethod
    def build_1d(
        cls,
        species:         str,
        wn:              np.ndarray,
        r0_nodes_um:     np.ndarray,
        ref_mass_g_m2:   float,
        with_absorption: bool = False,
        dist_sigma:      float = 1.5,
        cache_dir:       str | Path | None = None,
        recompute:       bool = False,
    ) -> AerosolLUT:
        """
        Build a 1-D AerosolLUT using mieEODG.AerosolLUT.

        ``od_table`` is always the **extinction** OD (MEC) — used for beam
        transmittance.  When ``with_absorption=True`` (emission geometry),
        ``od_table_abs`` is also populated with the **absorption** OD (MAC)
        for aerosol self-emission.

        Parameters
        ----------
        species : str
            Aerosol species name (ARIA category key or mieEODG default).
        wn : np.ndarray
            Wavenumber grid [cm⁻¹].
        r0_nodes_um : np.ndarray
            Effective radius nodes [µm], log-spaced.
        ref_mass_g_m2 : float
            Reference mass column [g/m²].
        with_absorption : bool
            Also build the absorption (MAC) table.  Required for emission.
        dist_sigma : float
            Log-normal size distribution geometric standard deviation.
        cache_dir : Path or None
            mieEODG LUT cache directory.
        recompute : bool
            Bypass mieEODG cache and recompute.
        """
        from mieEODG import AerosolLUT as _MieLUT
        from mieEODG.refindex import ARIADatabase
        from mieEODG.constants import ARIA_DEFAULTS
        from plumeIR.exceptions import ReferenceComputationError

        key = species.upper()
        if key not in _SPECIES_TO_ARIA:
            deferred = " (deferred — no ARIA source)" if key in _DEFERRED_AEROSOLS else ""
            raise ReferenceComputationError(
                f"No ARIA refractive-index source for aerosol '{species}'{deferred}. "
                f"Supported 1-D species: {sorted(_SPECIES_TO_ARIA)}; "
                f"{H2SO4_AQ} uses build_h2so4_2d."
            )

        # Load the refractive index from the ARIA database onto the model grid,
        # then build the Mie LUT.  mieEODG resolves the particle density from
        # the RI metadata (density=None → auto).
        wn     = np.asarray(wn, dtype=float)
        wn_mie = _mie_grid(wn)
        aria   = ARIA_DEFAULTS[_SPECIES_TO_ARIA[key]]
        db     = ARIADatabase()
        _warn_if_outside_ri(db.read(aria).wavenumber, wn_mie, species)
        ri = db.read(aria, wavenumber=wn_mie, out_of_range="clip")
        lut = _MieLUT.load_or_compute(
            ri=ri, r0_um=r0_nodes_um, s=dist_sigma, wn=wn_mie,
            cache_dir=cache_dir, recompute=recompute, progress=False,
        )

        scale = ref_mass_g_m2 * 1e-4
        od_ext = np.zeros((len(r0_nodes_um), len(wn_mie)))
        od_abs = np.zeros((len(r0_nodes_um), len(wn_mie))) if with_absorption else None
        for i, r0 in enumerate(r0_nodes_um):
            result = lut.forward(r0)
            od_ext[i] = result["MEC"] * scale          # extinction
            if with_absorption:
                od_abs[i] = result["MAC"] * scale      # absorption

        # Tables stay on the coarse Mie grid; query() interpolates onto
        # wn_model once, on demand.
        logger.info(
            "Built 1-D AerosolLUT for %s: %d r0 nodes on a %.2f cm-1 Mie grid "
            "(%d pts -> %d model pts)%s",
            species, len(r0_nodes_um), MIE_GRID_SPACING_WN, wn_mie.size, wn.size,
            " (+absorption)" if with_absorption else "",
        )
        return cls(
            species=species, wn=wn_mie, r0_nodes_um=r0_nodes_um,
            od_table=od_ext, ref_mass_g_m2=ref_mass_g_m2,
            od_table_abs=od_abs, wn_model=wn,
        )

    @classmethod
    def build_h2so4_2d(
        cls,
        wn:             np.ndarray,
        r0_nodes_um:    np.ndarray  = _H2SO4_R0_NODES_UM,
        wt_nodes:       np.ndarray  = _H2SO4_WT_NODES,
        temp_k:          float       = _H2SO4_TEMP_K,
        ref_mass_g_m2:   float       = 1e-3,
        with_absorption: bool        = False,
        dist_sigma:      float       = 1.5,
        max_workers:     int         = 4,
        cache_dir:       str | Path | None = None,
        recompute:       bool        = False,
    ) -> AerosolLUT:
        """
        Build a 2-D H2SO4 AerosolLUT over (r0, weight_fraction).

        ``od_table`` is the extinction OD (MEC); when ``with_absorption=True``
        the absorption OD (MAC) is also stored for emission self-emission.

        Uses mieEODG.MultiDimLUT with the canonical ARIA source for sulphuric
        acid ("Myhre 2003"), sweeping over r0 and composition.  Composition is
        carried internally to mieEODG in **wt%** (the ARIA convention); the LUT
        stores and queries it as the weight **fraction** ``wt_nodes`` (plumeIR
        convention).  ``wt_nodes`` must lie within the source's measured
        composition points (Myhre 2003: 0.38–0.72).

        Parameters
        ----------
        wn : np.ndarray
            Wavenumber grid [cm⁻¹].
        r0_nodes_um : np.ndarray
            Effective radius nodes [µm].
        wt_nodes : np.ndarray
            H2SO4 weight fraction nodes (0–1).
        temp_k : float
            Reserved; temperature is fixed by the ARIA source, not a free axis.
        ref_mass_g_m2 : float
            Reference mass column [g/m²].
        with_absorption : bool
            Also build the absorption (MAC) table.  Required for emission.
        max_workers : int
            **Currently unused.**  mieEODG parallelises via ``backend``, not a
            worker count, and its spawn path has a live deadlock on macOS (a
            worker blocks reading its payload and never starts).  Left in the
            signature rather than removed because that is an API change.
        cache_dir : str, Path or None
            On-disk cache for the mieEODG Mie LUT.  ``None`` uses mieEODG's
            default location.
        recompute : bool
            Bypass the mieEODG cache and recompute.
        """
        from mieEODG.lut_multi import MultiDimLUT, LUTAxis

        # mieEODG/ARIA expresses composition in wt%; plumeIR uses the fraction.
        wt_pct = np.asarray(wt_nodes, dtype=float) * 100.0

        axes = [
            LUTAxis("r0",          r0_nodes_um, units="µm",
                    log_spacing=True,  fixed_for_ri_load=True),
            LUTAxis("composition", wt_pct,      units="wt%",
                    log_spacing=False, fixed_for_ri_load=False),
        ]

        # Single-source LUT (canonical Myhre 2003); temperature is pinned by the
        # source.  s (size-distribution width) is passed directly.
        #
        # load_or_compute, NOT from_aria_database + compute: this is a 120-node
        # Mie grid (24 r0 x 5 wt), so it must persist to mieEODG's on-disk
        # LUT cache.  NOTE: that cache silently no-ops without netCDF4
        # installed — it is declared in mieEODG's pyproject and in
        # environment.yml, but a missing install only logs a warning and
        # recomputes.  Mie now runs on a ~1 cm-1 grid (see MIE_GRID_SPACING_WN),
        # ~25x fewer evaluations than the model grid for 0.019% error.
        wn     = np.asarray(wn, dtype=float)
        wn_mie = _mie_grid(wn)
        mdlut = MultiDimLUT.load_or_compute(
            substance="Sulphuric acid",
            source="Myhre 2003",
            axes=axes,
            wavenumber=wn_mie,
            s=dist_sigma,
            cache_dir=cache_dir,
            recompute=recompute,
            progress=False,
        )

        scale  = ref_mass_g_m2 * 1e-4
        shape  = (len(r0_nodes_um), len(wt_nodes), len(wn_mie))
        od_ext = np.zeros(shape)
        od_abs = np.zeros(shape) if with_absorption else None
        for i_r, r0 in enumerate(r0_nodes_um):
            for i_w, wt in enumerate(wt_nodes):
                result = mdlut.forward(r0=r0, composition=float(wt) * 100.0)
                od_ext[i_r, i_w] = result["MEC"] * scale
                if with_absorption:
                    od_abs[i_r, i_w] = result["MAC"] * scale

        logger.info(
            "Built 2-D H2SO4 AerosolLUT: %d r0 × %d wt nodes on a %.2f cm-1 "
            "Mie grid (%d pts -> %d model pts)%s",
            len(r0_nodes_um), len(wt_nodes), MIE_GRID_SPACING_WN,
            wn_mie.size, wn.size,
            " (+absorption)" if with_absorption else "",
        )
        return cls(
            species=H2SO4_AQ, wn=wn_mie, r0_nodes_um=r0_nodes_um,
            od_table=od_ext, ref_mass_g_m2=ref_mass_g_m2,
            wt_nodes=wt_nodes, od_table_abs=od_abs, wn_model=wn,
        )
