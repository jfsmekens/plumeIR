"""
GasLUT — pre-computed optical depth look-up table for a single gas species.

Two variants:
    1-D  (most gases)   : axes = [temperature]
                          OD is built at fixed SCD by converting ref_SCD → VMR
                          at each temperature node.  The forward model scales:
                          OD_actual = lut.query(T) * (actual_SCD / ref_SCD)

    2-D  (H2O)          : axes = [temperature, VMR]
                          OD is built over a grid of (T, VMR) combinations.
                          The forward model queries directly:
                          OD_actual = lut.query(T, VMR)

Both variants include RFM's continuum (CTM): every build passes
``physics={'ctm': True}`` explicitly.  What differs is how the continuum
follows the column.  A 1-D table holds it at the reference VMR and scales it
linearly, which is exact for lines and the foreign continuum but not for the
H2O self-continuum (∝ VMR²); a 2-D table follows it on the VMR grid.  Measured
2026-11-12 in the layer CO window (2020-2150 cm⁻¹, ambient H2O over 1.22 km at
284 K, column ×0.81 of the reference): the continuum removes 7.4% of the
transmittance and linear scaling leaves 0.43% rms (max 1.7%) in it, 0.39%
after a degree-3 polynomial.  Before 2026-11-12 the flag was left to
RFMwrapper's default, which is also on, so no table built earlier changes.

Build time is paid once.  Results are content-addressed and cached to disk.

All distances are in km; temperatures in K; pressures in mbar;
VMR in ppmv; SCD in molec/cm².
"""

from __future__ import annotations

import hashlib
import logging
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Sequence

import numpy as np

from plumeIR.standards import (
    ATM_TEMP_NODES as _ATM_TEMP_NODES,
    PLUME_TEMP_NODES as _PLUME_TEMP_NODES,
    H2O_VMR_NODES as _H2O_VMR_NODES,
)
from plumeIR.units import scd_to_vmr, vmr_to_scd

logger = logging.getLogger(__name__)


# =============================================================================
# GasLUT
# =============================================================================

@dataclass
class GasLUT:
    """
    Optical depth look-up table for a single gas species.

    Parameters
    ----------
    species : str
        Gas species name (e.g. ``'SO2'``, ``'H2O'``).
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    temp_nodes : np.ndarray
        Temperature axis [K], shape ``(n_temp,)``.
    od_table : np.ndarray
        Optical depth table.
        1-D: shape ``(n_temp, n_wn)`` — each row at ``ref_scd`` molecules.
        2-D: shape ``(n_temp, n_vmr, n_wn)`` — rows at (T, VMR) node.
    ref_scd : float
        Reference slant column density [molec/cm²].  Scaling reference for
        1-D gases; stored for unit conversion for 2-D gases.
    vmr_nodes : np.ndarray or None
        VMR axis [ppmv] for 2-D LUTs, log-spaced.  ``None`` for 1-D.
    ref_vmr : float or None
        Reference VMR [ppmv] for 2-D gases — the concentration at which the
        scaling factor equals 1.  The forward model maps the H2O scaling
        parameter to an absolute VMR via ``vmr = scaling × ref_vmr`` before
        querying.  ``None`` for 1-D gases.
    """

    species:    str
    wn:         np.ndarray
    temp_nodes: np.ndarray
    od_table:   np.ndarray
    ref_scd:    float
    vmr_nodes:  np.ndarray | None = None
    ref_vmr:    float | None      = None

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def is_2d(self) -> bool:
        """True if this LUT has an independent VMR axis."""
        return self.vmr_nodes is not None

    @property
    def n_temp(self) -> int:
        return len(self.temp_nodes)

    @property
    def n_vmr(self) -> int:
        return len(self.vmr_nodes) if self.vmr_nodes is not None else 1

    @property
    def n_wn(self) -> int:
        return len(self.wn)

    # ------------------------------------------------------------------
    # Query
    # ------------------------------------------------------------------

    def query(self, temp: float, vmr: float | None = None) -> np.ndarray:
        """
        Interpolate the optical depth spectrum at the given conditions.

        For a 1-D LUT, returns OD at ``ref_scd``.  The caller multiplies by
        ``actual_SCD / ref_SCD`` to get the true optical depth.

        For a 2-D LUT (H2O), returns OD at the given ``vmr`` [ppmv] directly.
        No further scaling is needed.

        Parameters
        ----------
        temp : float
            Temperature [K].  Must lie within the temperature node range;
            there is no clamping or extrapolation.
        vmr : float or None
            Water vapour VMR [ppmv].  Required for 2-D LUTs; ignored for 1-D.

        Returns
        -------
        np.ndarray
            Optical depth spectrum, shape ``(n_wn,)``.

        Raises
        ------
        ValueError
            If ``temp`` lies outside the temperature node range, or a 2-D LUT
            is queried without ``vmr``.
        """
        if self.is_2d:
            if vmr is None:
                raise ValueError(
                    f"GasLUT for {self.species} is 2-D: vmr must be provided"
                )
            return self._query_2d(temp, vmr)
        return self._query_1d(temp)

    def _bracket_temp(self, temp: float) -> tuple[int, float]:
        """
        Locate ``temp`` on the temperature axis.

        Returns the lower node index and the linear weight of the upper node.
        Raises rather than clamping: a silent clamp makes the absorption use a
        different temperature from every other term that sees the true value
        (the Planck term, in the emission geometry), and nothing downstream
        can tell.  A tolerance of 1e-9 of the span admits float round-off at
        the edges only.

        Raises
        ------
        ValueError
            If ``temp`` is outside ``[temp_nodes[0], temp_nodes[-1]]``.
        """
        lo, hi = float(self.temp_nodes[0]), float(self.temp_nodes[-1])
        tol = 1e-9 * (hi - lo)
        if not (lo - tol <= temp <= hi + tol):
            raise ValueError(
                f"GasLUT {self.species}: temperature {temp:.2f} K is outside "
                f"the LUT range [{lo:g}, {hi:g}] K. Queries are never clamped "
                f"or extrapolated; extend the node range in "
                f"plumeIR/standards.py and rebuild the LUT."
            )
        i = int(np.clip(np.searchsorted(self.temp_nodes, temp) - 1,
                        0, self.n_temp - 2))
        t0, t1 = self.temp_nodes[i], self.temp_nodes[i + 1]
        return i, float(np.clip((temp - t0) / (t1 - t0), 0.0, 1.0))

    def _query_1d(self, temp: float) -> np.ndarray:
        """Vectorised linear interpolation over the temperature axis."""
        i, alpha = self._bracket_temp(temp)
        return (1.0 - alpha) * self.od_table[i] + alpha * self.od_table[i + 1]

    def _query_2d(self, temp: float, vmr: float) -> np.ndarray:
        """
        Bilinear interpolation: linear in T; cross-section (OD/VMR) linear
        in VMR.

        The VMR axis interpolates the effective cross-section ``g = OD/VMR``
        linearly in VMR, then multiplies by the query VMR.  ``g`` is constant
        for line absorption (∝ VMR) and linear for the self-continuum (∝ VMR²),
        so linear interpolation of ``g`` is near-exact for both — far better
        than log-linear interpolation of OD, which overestimates the linear
        line component at the geometric midpoint (~AM/GM−1).  Validated against
        direct RFM: line error 1.7%→0.05%, continuum error 5%→~0% (keeping the
        log VMR node spacing, which resolves the continuum at low VMR).
        """
        # Temperature bracket (linear; raises outside the node range)
        i_t, alpha_t = self._bracket_temp(temp)

        # VMR bracket (nodes sorted ascending; spacing may be log)
        vmr_c = float(np.clip(vmr, self.vmr_nodes[0], self.vmr_nodes[-1]))
        i_v = int(np.searchsorted(self.vmr_nodes, vmr_c)) - 1
        i_v = int(np.clip(i_v, 0, self.n_vmr - 2))
        v0, v1 = self.vmr_nodes[i_v], self.vmr_nodes[i_v + 1]
        # Linear weight in VMR for the cross-section g = OD/VMR
        alpha_v = float(np.clip((vmr_c - v0) / (v1 - v0), 0.0, 1.0))

        # Cross-section at the 4 corners (OD / node VMR)
        g_00 = self.od_table[i_t,     i_v    ] / v0
        g_01 = self.od_table[i_t,     i_v + 1] / v1
        g_10 = self.od_table[i_t + 1, i_v    ] / v0
        g_11 = self.od_table[i_t + 1, i_v + 1] / v1

        g = (
            (1.0 - alpha_t) * (1.0 - alpha_v) * g_00
            + (1.0 - alpha_t) * alpha_v        * g_01
            +        alpha_t  * (1.0 - alpha_v) * g_10
            +        alpha_t  * alpha_v          * g_11
        )
        return vmr_c * g

    # ------------------------------------------------------------------
    # Sub-window slicing
    # ------------------------------------------------------------------

    def slice_window(self, wn_start: float, wn_stop: float) -> GasLUT:
        """
        Return this LUT restricted to a sub-window, by **exact slicing**.

        A LUT built over a wider range already contains the narrower one's grid
        points: RFM evaluates the same monochromatic function on a commensurate
        grid, so no interpolation is involved and the result is bit-identical to
        a direct build.  Verified against direct RFM (SO2, 750-1250 sliced from
        700-1300 at nperwn 25): max deviation 0.0000%, grid agreement 2e-13.

        Interpolation would **not** be safe here — the emission window carries
        stratospheric lines ~0.002 cm⁻¹ wide that no practical model grid
        resolves — so a request that does not land exactly on the cached grid
        raises rather than approximating.

        Raises
        ------
        ValueError
            If this LUT does not cover the requested range, or its grid is
            incommensurate with the requested bounds.
        """
        wn   = np.asarray(self.wn, dtype=float)
        step = float(np.median(np.diff(wn)))
        tol  = 0.1 * step

        mask = (wn >= wn_start - tol) & (wn <= wn_stop + tol)
        if not mask.any():
            raise ValueError(
                f"GasLUT for {self.species} covers {wn[0]:.4f}-{wn[-1]:.4f} "
                f"cm-1; no points in {wn_start:.4f}-{wn_stop:.4f}"
            )
        sub = wn[mask]
        if abs(sub[0] - wn_start) > tol or abs(sub[-1] - wn_stop) > tol:
            raise ValueError(
                f"GasLUT for {self.species}: requested {wn_start:.4f}-"
                f"{wn_stop:.4f} cm-1 does not land on the cached grid "
                f"({sub[0]:.4f}-{sub[-1]:.4f}, step {step:.6f}) — "
                f"incommensurate bounds, refusing to interpolate"
            )
        idx = np.flatnonzero(mask)
        return replace(self, wn=wn[idx], od_table=self.od_table[..., idx])

    # ------------------------------------------------------------------
    # Serialisation
    # ------------------------------------------------------------------

    def save(self, path: str | Path) -> None:
        """
        Save the LUT to a ``.npz`` file.

        Parameters
        ----------
        path : str or Path
            Destination file (will be overwritten).
        """
        path = Path(path)
        arrays: dict = {
            "wn":         self.wn,
            "temp_nodes": self.temp_nodes,
            "od_table":   self.od_table,
            "ref_scd":    np.array([self.ref_scd]),
        }
        if self.vmr_nodes is not None:
            arrays["vmr_nodes"] = self.vmr_nodes
        if self.ref_vmr is not None:
            arrays["ref_vmr"] = np.array([self.ref_vmr])
        np.savez_compressed(path, species=np.array([self.species]), **arrays)
        logger.debug("Saved GasLUT '%s' → '%s'", self.species, path.name)

    @classmethod
    def load(cls, path: str | Path) -> GasLUT:
        """
        Load a GasLUT from a ``.npz`` file.

        Parameters
        ----------
        path : str or Path
            Path to the ``.npz`` file.

        Returns
        -------
        GasLUT
        """
        path = Path(path)
        data = np.load(path, allow_pickle=False)
        return cls(
            species    = str(data["species"][0]),
            wn         = data["wn"],
            temp_nodes = data["temp_nodes"],
            od_table   = data["od_table"],
            ref_scd    = float(data["ref_scd"][0]),
            vmr_nodes  = data["vmr_nodes"] if "vmr_nodes" in data else None,
            ref_vmr    = float(data["ref_vmr"][0]) if "ref_vmr" in data else None,
        )

    # ------------------------------------------------------------------
    # Compute  (builds the LUT from RFM calls)
    # ------------------------------------------------------------------

    @classmethod
    def compute(
        cls,
        species:      str,
        wn_start:     float,
        wn_stop:      float,
        nperwn:       float,
        temp_nodes:   np.ndarray,
        pathlength:   float,
        pres:         float,
        ref_scd:      float,
        vmr_nodes:    np.ndarray | None = None,
        ref_vmr:      float | None = None,
        continuum:    bool = False,
        max_workers:  int  = 4,
    ) -> GasLUT:
        """
        Build a GasLUT by running RFM via RFMwrapper.

        For 1-D gases: runs one RFM job per temperature node.  At each node,
        the VMR is back-calculated from ``ref_scd`` so that the OD corresponds
        to exactly ``ref_scd`` molecules regardless of temperature.

        For 2-D gases (H2O): runs one RFM job per (T, VMR) combination.

        Parameters
        ----------
        species : str
            RFM gas name (e.g. ``'SO2'``, ``'H2O'``).
        wn_start, wn_stop : float
            Spectral range [cm⁻¹].
        nperwn : float
            Model resolution [points per cm⁻¹].
        temp_nodes : np.ndarray
            Temperature nodes [K].
        pathlength : float
            Layer path length [km].
        pres : float
            Layer pressure [mbar].
        ref_scd : float
            Reference slant column density [molec/cm²].  For 1-D gases this
            is the SCD at which all LUT nodes are computed.
        vmr_nodes : np.ndarray or None
            VMR nodes [ppmv] for 2-D LUTs.  ``None`` for 1-D gases.
        continuum : bool
            Marks a table built for the 2-D continuum treatment.  Recorded in
            the cache key only: RFM's continuum (CTM) is on for every build,
            1-D or 2-D, and the 2-D grid is selected by ``vmr_nodes``.  Kept
            so existing cache files keep their names.
        max_workers : int
            Number of parallel RFM processes for ``runBatch()``.

        Returns
        -------
        GasLUT
        """
        from rfmwrapper import RFM, runBatch

        is_2d = vmr_nodes is not None
        # Explicit rather than RFMwrapper's default (also on), so a change of
        # default there cannot silently drop the continuum from every table.
        physics = {"ctm": True}

        if is_2d:
            # Build grid: all (T, VMR) combinations
            jobs, indices = [], []
            for i_t, T in enumerate(temp_nodes):
                for i_v, vmr in enumerate(vmr_nodes):
                    jobs.append(RFM(
                        type="layer",
                        wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
                        gas=species,
                        pathlength=pathlength, pres=pres, temp=T,
                        vmr={species: float(vmr)},
                        physics=physics,
                        out="OPT",
                        silent=True,
                    ))
                    indices.append((i_t, i_v))

            logger.info(
                "Building 2-D GasLUT for %s: %d × %d = %d RFM jobs",
                species, len(temp_nodes), len(vmr_nodes), len(jobs),
            )
            results = runBatch(jobs, max_workers=max_workers)

            # Assemble table
            n_wn   = len(results[0].getSpectra("opt").wn)
            wn_out = results[0].getSpectra("opt").wn
            od_table = np.zeros((len(temp_nodes), len(vmr_nodes), n_wn))
            for job_result, (i_t, i_v) in zip(results, indices):
                od_table[i_t, i_v] = job_result.getSpectra("opt").data[0]

        else:
            # 1-D: compute VMR at each T node to hold SCD constant
            vmr_per_temp = np.array([
                scd_to_vmr(ref_scd, pathlength, temp=float(T), pres=pres)
                for T in temp_nodes
            ])

            jobs = [
                RFM(
                    type="layer",
                    wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
                    gas=species,
                    pathlength=pathlength, pres=pres, temp=float(T),
                    vmr={species: float(vmr)},
                    physics=physics,
                    out="OPT",
                    silent=True,
                )
                for T, vmr in zip(temp_nodes, vmr_per_temp)
            ]

            logger.info(
                "Building 1-D GasLUT for %s: %d RFM jobs", species, len(jobs)
            )
            results = runBatch(jobs, max_workers=max_workers)

            wn_out   = results[0].getSpectra("opt").wn
            n_wn     = len(wn_out)
            od_table = np.zeros((len(temp_nodes), n_wn))
            for i, result in enumerate(results):
                od_table[i] = result.getSpectra("opt").data[0]

        lut = cls(
            species    = species,
            wn         = wn_out,
            temp_nodes = temp_nodes,
            od_table   = od_table,
            ref_scd    = float(ref_scd),
            vmr_nodes  = vmr_nodes,
            ref_vmr    = float(ref_vmr) if ref_vmr is not None else None,
        )
        logger.info(
            "✓ GasLUT %s built: shape %s", species, od_table.shape
        )
        return lut

    # ------------------------------------------------------------------
    # Cached load-or-compute
    # ------------------------------------------------------------------

    @classmethod
    def load_or_compute(
        cls,
        species:     str,
        wn_start:    float,
        wn_stop:     float,
        nperwn:      float,
        temp_nodes:  np.ndarray,
        pathlength:  float,
        pres:        float,
        ref_scd:     float,
        vmr_nodes:   np.ndarray | None = None,
        ref_vmr:     float | None = None,
        continuum:   bool = False,
        max_workers: int  = 4,
        cache_dir:   str | Path | None = None,
        recompute:   bool = False,
    ) -> GasLUT:
        """
        Load a cached GasLUT if available; otherwise compute and cache it.

        The cache key is a SHA-256 hash of all inputs that affect the
        computed spectra.  Any change triggers a recompute.

        Parameters
        ----------
        cache_dir : str or Path or None
            Directory for ``.npz`` cache files.
            Defaults to ``~/.plumeIR/lut_cache/``.
        recompute : bool
            If ``True``, ignore any cached file and recompute.

        Returns
        -------
        GasLUT
        """
        cache_dir = Path(cache_dir) if cache_dir else (
            Path.home() / ".plumeIR" / "lut_cache"
        )
        cache_dir.mkdir(parents=True, exist_ok=True)

        phash = _physics_key(
            species, nperwn, temp_nodes, pathlength, pres, ref_scd,
            vmr_nodes, continuum,
        )
        cache_file = cache_dir / _cache_name(species, phash, wn_start, wn_stop)

        def _finish(lut: GasLUT) -> GasLUT:
            # ref_vmr is derived (not in the cache key); set it on load
            if ref_vmr is not None and lut.ref_vmr is None:
                lut.ref_vmr = float(ref_vmr)
            return lut

        if not recompute:
            if cache_file.exists():
                logger.info("Loading cached GasLUT %s from '%s'",
                            species, cache_file.name)
                return _finish(cls.load(cache_file))

            # No exact match — a wider cached build covers this window and can
            # be sliced exactly (see GasLUT.slice_window).  Prefer the
            # narrowest covering candidate: least to load, least to discard.
            tol        = 0.5 / float(nperwn)
            candidates = []
            for cand in cache_dir.glob(f"gas_{species}_{phash[:16]}_*.npz"):
                rng = _decode_name(cand)
                if rng is None:
                    continue
                lo, hi = rng
                if lo <= wn_start + tol and hi >= wn_stop - tol:
                    candidates.append((hi - lo, cand))

            for _, cand in sorted(candidates):
                try:
                    sliced = cls.load(cand).slice_window(wn_start, wn_stop)
                except ValueError as exc:
                    logger.debug("Cannot slice '%s': %s", cand.name, exc)
                    continue
                logger.info(
                    "Sliced cached GasLUT %s %.2f-%.2f cm-1 from '%s'",
                    species, wn_start, wn_stop, cand.name,
                )
                return _finish(sliced)

        lut = cls.compute(
            species=species,
            wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
            temp_nodes=temp_nodes,
            pathlength=pathlength, pres=pres, ref_scd=ref_scd,
            vmr_nodes=vmr_nodes,
            ref_vmr=ref_vmr,
            continuum=continuum,
            max_workers=max_workers,
        )
        lut.save(cache_file)
        return lut


# =============================================================================
# Cache key
# =============================================================================

def _physics_key(
    species:    str,
    nperwn:     float,
    temp_nodes: np.ndarray,
    pathlength: float,
    pres:       float,
    ref_scd:    float,
    vmr_nodes:  np.ndarray | None,
    continuum:  bool,
) -> str:
    """
    SHA-256 over everything that determines the LUT content **except** the
    wavenumber range.

    The range is carried in the filename instead, so a cached LUT can serve any
    sub-window it covers (see ``GasLUT.slice_window``).  Two LUTs sharing this
    key differ only in extent, never in value at a shared grid point.

    ``pathlength`` is kept even though a 1-D LUT holds SCD fixed and is
    therefore path-independent to RFM's output precision (measured 0.0012% over
    a 5x path change).  Dropping it is a separate change; for a 2-D LUT the
    path is a genuine linear factor.
    """
    h = hashlib.sha256()
    h.update(species.encode())
    for val in (nperwn, pathlength, pres, ref_scd):
        h.update(np.float64(val).tobytes())
    h.update(np.asarray(temp_nodes, dtype=np.float64).tobytes())
    if vmr_nodes is not None:
        h.update(np.asarray(vmr_nodes, dtype=np.float64).tobytes())
    h.update(bytes([int(continuum)]))
    return h.hexdigest()


#: Wavenumber bounds are encoded in the cache filename at this resolution
#: [cm⁻¹], fine enough for any realistic window bound.
_WN_ENCODE_RES: float = 0.01


def _encode_wn(v: float) -> int:
    return int(round(float(v) / _WN_ENCODE_RES))


def _cache_name(species: str, phash: str, wn_start: float, wn_stop: float) -> str:
    return (f"gas_{species}_{phash[:16]}_"
            f"{_encode_wn(wn_start)}_{_encode_wn(wn_stop)}.npz")


def _decode_name(path: Path) -> tuple[float, float] | None:
    """Recover (wn_start, wn_stop) from a cache filename; None if unparseable."""
    parts = path.stem.rsplit("_", 2)
    if len(parts) != 3:
        return None
    try:
        return (int(parts[1]) * _WN_ENCODE_RES, int(parts[2]) * _WN_ENCODE_RES)
    except ValueError:
        return None
