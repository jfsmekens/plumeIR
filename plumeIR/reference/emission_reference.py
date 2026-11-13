"""
build_emission_reference — Phase 3b reference pre-computation for the
emission geometry.

Builds, for one fit window:
  - Three background atmospheric layer LUTs (proximal / slice / distal),
    each 1-D over H2O column scaling, storing optical depth and effective
    emission temperature (see AtmLayerLUT).
  - A clear-sky radiance spectrum (full atmosphere, no plume) for difference
    mode.
  - Plume gas LUTs (queried at plume temperature) and plume aerosol LUTs
    with both extinction and absorption tables.

The three layers and the clear-sky radiance come from **one RFM ``*LEV``
zenith run per H2O_sc node** (output taps at the plume bottom/top): the
cumulative columns at [obs, h1, h2] are differenced to recover each layer's
(OD, isolated radiance), and the observer column is the clear-sky radiance.
Because it is a single continuous integration, the layer optical depths add
exactly to the full column and L_clear is consistent with the layer assembly
— no sliced-vs-full discretization gap (cf. the earlier per-slice approach,
retained as ``_build_atm_layer`` for the interpolation-audit demos).

All radiative transfer is delegated to RFMwrapper (H2O scaling, zenith
radiance/transmittance, ``*LEV`` output levels).  plumeIR owns only the
layer decomposition, the T_eff derivation, and the caching.

NOTE: like GasLUT.compute, the RFM-calling paths here are not unit-tested in
CI (they require the RFM binary).  The pure assembly/derivation logic is
tested separately with synthetic inputs.
"""

from __future__ import annotations

import logging
import tempfile
from pathlib import Path

import numpy as np

from plumeIR.config.objects import (
    EmissionGeometry, FitWindowConfig, GeometryType, ModelConfig, Retrieval,
)
from plumeIR.exceptions import ReferenceComputationError
from plumeIR.parameters.parameter import ParameterSet
from plumeIR.reference.atm_lut import AtmLayerLUT, derive_t_eff
from plumeIR.reference.aero_lut import AerosolLUT
from plumeIR.reference.gas_lut import GasLUT
from plumeIR.reference.reference import (
    Reference, _REFERENCE_SCHEMA_VERSION, _file_digest, _load_reference,
    _reference_cache_key, _resolve_cache_dir, _save_reference,
    load_cached_reference,
)
from plumeIR.standards import (
    AERO_DEFAULTS, EMISSION_DISTAL_GASES, GAS_DEFAULTS, H2O_SC_NODES,
    PLUME_TEMP_NODES, AERO_R0_NODES, plume_temp_nodes_for,
)
from plumeIR.units import vmr_to_scd

logger = logging.getLogger(__name__)


# =============================================================================
# RFM helper (isolated so the pure logic above can be tested separately)
# =============================================================================

def _zenith_layer_spectra(
    atm_slice: dict,
    h_obs:     float,
    elev:      float,
    wn_start:  float,
    wn_stop:   float,
    nperwn:    float,
    gases:     str,
    h2o_sc:    float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Run a single zenith RFM through one atmospheric slice at a given H2O
    column scaling.

    Returns ``(wn, L, OD)`` — wavenumber grid, radiance, and optical depth
    (RFM 'opt', taken directly to avoid a −log(T) conversion and its
    precision loss at strong lines) — each 1-D over the RFM output grid.
    """
    from rfmwrapper import RFM, adjustAtm, writeAtm

    # Scale H2O by multiplying its surface VMR (adjustAtm preserves profile
    # shape: new = old / old[0] * value).
    scaled = atm_slice
    if "H2O" in atm_slice and h2o_sc != 1.0:
        scaled = adjustAtm(atm_slice, "H2O", float(atm_slice["H2O"][0]) * h2o_sc)

    with tempfile.NamedTemporaryFile(suffix=".atm", delete=False) as tf:
        atm_path = Path(tf.name)
    try:
        writeAtm(scaled, str(atm_path))
        model = RFM(
            type="zenith",
            wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
            gas=gases, elev=elev, h_obs=h_obs,
            atm_path=str(atm_path),
            out="RAD OPT", silent=True,
        )
        model.run()
        rad = model.getSpectra("rad")
        opt = model.getSpectra("opt")
        return rad.wn, rad.data[0], opt.data[0]
    finally:
        atm_path.unlink(missing_ok=True)


def _build_atm_layer(
    name:       str,
    atm_slice:  dict,
    h_obs:      float,
    elev:       float,
    wn_start:   float,
    wn_stop:    float,
    nperwn:     float,
    gases:      str,
    h2o_sc_nodes: np.ndarray,
) -> AtmLayerLUT:
    """
    Build one AtmLayerLUT by sweeping H2O scaling through zenith RFM runs and
    deriving the effective emission temperature.
    """
    wn_out = None
    L_rows, OD_rows = [], []
    for sc in h2o_sc_nodes:
        wn_out, L, OD = _zenith_layer_spectra(
            atm_slice, h_obs, elev, wn_start, wn_stop, nperwn, gases, float(sc)
        )
        L_rows.append(L)
        OD_rows.append(OD)

    L_table  = np.vstack(L_rows)
    OD_table = np.vstack(OD_rows)
    # T_eff needs transmittance: T = exp(−OD)
    T_eff = derive_t_eff(wn_out, L_table, np.exp(-OD_table))

    logger.info("✓ AtmLayerLUT '%s' built: %d H2O_sc nodes", name, len(h2o_sc_nodes))
    return AtmLayerLUT(
        name=name, wn=wn_out, h2o_sc_nodes=np.asarray(h2o_sc_nodes, dtype=float),
        od_table=OD_table, T_eff_table=T_eff,
    )


# =============================================================================
# *LEV builder — all three layers from one zenith run per H2O_sc node
# =============================================================================

def _lev_column_spectra(
    atm:      dict,
    h_obs:    float,
    elev:     float,
    wn_start: float,
    wn_stop:  float,
    nperwn:   float,
    gases:    str,
    levels:   list[float],
    h2o_sc:   float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """
    One zenith RFM from ``h_obs`` to space with RFM ``*LEV`` output taps at
    ``levels``.  All taps come from a single continuous integration, so the
    per-altitude columns are exact cumulative cuts of the same path.

    Returns ``(wn, altitudes, L_alt, OD_alt)`` where ``altitudes`` is ascending
    ``[h_obs, *sorted(levels)]`` and ``L_alt`` / ``OD_alt`` are
    ``(n_alt, n_wn)`` — each row the integrated radiance / optical depth of the
    column from that altitude to space.  H2O is column-scaled by ``h2o_sc``.
    """
    from rfmwrapper import RFM, adjustAtm, writeAtm

    scaled = atm
    if "H2O" in atm and h2o_sc != 1.0:
        scaled = adjustAtm(atm, "H2O", float(atm["H2O"][0]) * h2o_sc)

    with tempfile.NamedTemporaryFile(suffix=".atm", delete=False) as tf:
        atm_path = Path(tf.name)
    try:
        writeAtm(scaled, str(atm_path))
        model = RFM(
            type="zenith",
            wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
            gas=gases, elev=elev, h_obs=h_obs, levels=list(levels),
            atm_path=str(atm_path),
            out="RAD OPT", silent=True,
        )
        model.run()
        rad = model.getSpectra("rad")
        opt = model.getSpectra("opt")
        # Sort both onto a common ascending-altitude axis (getSpectra labels the
        # suffix-less observer output and each *LEV tap on the 'altitude' axis).
        o_order = np.argsort(np.asarray(opt.labels, dtype=float))
        r_order = np.argsort(np.asarray(rad.labels, dtype=float))
        alts = np.asarray(opt.labels, dtype=float)[o_order]
        if not np.allclose(alts, np.asarray(rad.labels, dtype=float)[r_order]):
            raise ReferenceComputationError(
                "rad/opt *LEV altitude axes disagree: "
                f"{alts} vs {np.asarray(rad.labels)[r_order]}"
            )
        return rad.wn, alts, rad.data[r_order], opt.data[o_order]
    finally:
        atm_path.unlink(missing_ok=True)


def _decompose_layers(
    altitudes: np.ndarray,
    L_alt:     np.ndarray,
    OD_alt:    np.ndarray,
    h_obs:     float,
    h1:        float,
    h2:        float,
) -> tuple[dict[str, tuple[np.ndarray, np.ndarray]], np.ndarray]:
    """
    Recover each layer's ``(OD, isolated radiance)`` from the cumulative
    columns of a ``*LEV`` run.

    The taps are the columns *from* ``[h_obs, h1, h2]`` *to* space, so the three
    layers — proximal ``[h_obs, h1]``, slice ``[h1, h2]``, distal ``[h2, ∞)`` —
    are recovered by differencing (OD) and back-substitution (radiance)::

        OD_prox  = OD@obs − OD@h1     OD_slice = OD@h1 − OD@h2     OD_dist = OD@h2
        L_dist   = L@h2
        L_slice  = L@h1 − T_slice·L@h2
        L_prox   = L@obs − T_prox·L@h1

    Returns ``({'prox': (OD, L), 'slice': …, 'dist': …}, L_obs)`` where ``L_obs``
    is the observer column — the clear-sky radiance, consistent with the
    layer assembly by construction (pure function; unit-tested without RFM).
    """
    alt = np.asarray(altitudes, dtype=float)
    row = lambda z: int(np.argmin(np.abs(alt - z)))
    io, i1, i2 = row(h_obs), row(h1), row(h2)

    OD_prox  = OD_alt[io] - OD_alt[i1]
    OD_slice = OD_alt[i1] - OD_alt[i2]
    OD_dist  = OD_alt[i2]

    T_prox  = np.exp(-OD_prox)
    T_slice = np.exp(-OD_slice)

    L_dist  = L_alt[i2]
    L_slice = L_alt[i1] - T_slice * L_alt[i2]
    L_prox  = L_alt[io] - T_prox  * L_alt[i1]

    layers = {
        "prox":  (OD_prox,  L_prox),
        "slice": (OD_slice, L_slice),
        "dist":  (OD_dist,  L_dist),
    }
    return layers, L_alt[io]


def _decompose_two_layers(
    altitudes: np.ndarray,
    L_alt:     np.ndarray,
    OD_alt:    np.ndarray,
    h_obs:     float,
    h_p:       float,
) -> tuple[dict[str, tuple[np.ndarray, np.ndarray]], np.ndarray]:
    """
    Two-layer variant: split the atmosphere at ``h_p`` instead of bracketing a
    slice around the plume.

    The plume then sits strictly *between* proximal ``[h_obs, h_p]`` and distal
    ``[h_p, ∞)``, so the forward becomes exact sequential radiative transfer —
    every term attenuated by precisely what lies between it and the observer::

        L = L_dist·T_plume·T_prox + L_plume·T_prox + L_prox

    The three-layer form instead treats the plume and the co-located slice as
    *additive* emitters, neither attenuating the other, which overstates the
    plume emission term (measured ~18% over 750-1250 cm⁻¹ on 2019-09-11).

    Same differencing as :func:`_decompose_layers` with one tap fewer.
    """
    alt = np.asarray(altitudes, dtype=float)
    row = lambda z: int(np.argmin(np.abs(alt - z)))
    io, ip = row(h_obs), row(h_p)

    OD_prox = OD_alt[io] - OD_alt[ip]
    OD_dist = OD_alt[ip]
    T_prox  = np.exp(-OD_prox)

    L_dist = L_alt[ip]
    L_prox = L_alt[io] - T_prox * L_alt[ip]

    return {"prox": (OD_prox, L_prox), "dist": (OD_dist, L_dist)}, L_alt[io]


def _build_atm_layers_lev(
    atm:          dict,
    h_obs:        float,
    h1:           float,
    h2:           float,
    elev:         float,
    wn_start:     float,
    wn_stop:      float,
    nperwn:       float,
    gases:        str,
    h2o_sc_nodes: np.ndarray,
    n_layers:     int = 2,
) -> tuple[dict[str, AtmLayerLUT], np.ndarray, np.ndarray]:
    """
    Build the three emission ``AtmLayerLUT`` s and the clear-sky radiance from
    **one** ``*LEV`` zenith run per H2O_sc node.

    Replaces three separate sliced runs per node: the proximal / slice / distal
    layers come from differencing one continuous integration, so their optical
    depths add exactly to the full column and the clear-sky radiance (the
    observer column) is consistent with the layer assembly — eliminating the
    sliced-vs-full discretization gap.

    Returns ``(atm_layers, L_clear, wn)``.  ``L_clear`` is captured at the node
    closest to ``H2O_sc = 1`` (the a priori).
    """
    if n_layers not in (2, 3):
        raise ReferenceComputationError(
            f"n_layers must be 2 or 3, got {n_layers}"
        )
    # 3 layers: taps bracket the plume band, plume superimposed on the slice.
    # 2 layers: one tap at the plume centre, plume strictly between prox/dist.
    h_p    = 0.5 * (h1 + h2)
    levels = [float(h1), float(h2)] if n_layers == 3 else [h_p]
    names  = ("prox", "slice", "dist") if n_layers == 3 else ("prox", "dist")
    od_rows: dict[str, list] = {n: [] for n in names}
    L_rows:  dict[str, list] = {n: [] for n in names}
    sc_arr = np.asarray(h2o_sc_nodes, dtype=float)
    i_clear = int(np.argmin(np.abs(sc_arr - 1.0)))

    wn_out: np.ndarray | None = None
    L_clear: np.ndarray | None = None
    for k, sc in enumerate(sc_arr):
        wn_out, alts, L_alt, OD_alt = _lev_column_spectra(
            atm, h_obs, elev, wn_start, wn_stop, nperwn, gases,
            levels, float(sc),
        )
        layers, L_obs = (
            _decompose_layers(alts, L_alt, OD_alt, h_obs, h1, h2)
            if n_layers == 3 else
            _decompose_two_layers(alts, L_alt, OD_alt, h_obs, h_p)
        )
        for n in names:
            od_rows[n].append(layers[n][0])
            L_rows[n].append(layers[n][1])
        if k == i_clear:
            L_clear = L_obs

    atm_layers: dict[str, AtmLayerLUT] = {}
    for n in names:
        OD_table = np.vstack(od_rows[n])
        L_table  = np.vstack(L_rows[n])
        T_eff    = derive_t_eff(wn_out, L_table, np.exp(-OD_table))
        atm_layers[n] = AtmLayerLUT(
            name=n, wn=wn_out, h2o_sc_nodes=sc_arr,
            od_table=OD_table, T_eff_table=T_eff,
        )
    logger.info("✓ %d AtmLayerLUTs built from %d *LEV runs (1 per H2O_sc node)",
                n_layers, len(sc_arr))
    return atm_layers, L_clear, wn_out


def _atm_layers_cached(
    atm:          dict,
    atm_path:     Path,
    h_obs:        float,
    h1:           float,
    h2:           float,
    elev:         float,
    wn_start:     float,
    wn_stop:      float,
    nperwn:       float,
    gases:        str,
    distal_gases: list[str],
    n_layers:     int,
    *,
    cache_dir:    Path,
    recompute:    bool,
) -> tuple[dict[str, AtmLayerLUT], np.ndarray, np.ndarray,
           dict[str, dict[str, np.ndarray]]]:
    """
    The RFM part of an emission reference, cached under its own inputs.

    The ``*LEV`` runs (one per ``H2O_sc`` node) and the per-gas distal optical
    depths are the slow part of an emission build, ~5 min of RFM.  They depend
    only on the atmosphere, the observing geometry and the grid, so they are
    keyed on exactly those: a change elsewhere (a species list, a fit flag)
    reassembles the reference from this cache instead of rerunning RFM.

    Parameters
    ----------
    atm : dict
        The profile read from ``atm_path``.
    atm_path : Path
        The profile file; its contents enter the key.
    h_obs, h1, h2 : float
        Observer height and plume bottom/top [km].
    elev : float
        Elevation angle [degrees].
    wn_start, wn_stop, nperwn : float
        Model grid.
    gases : str
        Space-separated gases for the layer runs.
    distal_gases : list of str
        Gases given an independent distal column scaling.
    n_layers : int
        2 (standard) or 3.
    cache_dir : Path
        Cache directory.
    recompute : bool
        Ignore the cache.

    Returns
    -------
    tuple
        ``(atm_layers, L_clear, wn, atm_layer_gas_od)``.
    """
    import hashlib
    import pickle

    h = hashlib.sha256()
    h.update(f"atm-schema{_REFERENCE_SCHEMA_VERSION}".encode())
    h.update(_file_digest(atm_path))
    h.update(repr((float(h_obs), float(h1), float(h2), float(elev),
                   float(wn_start), float(wn_stop), float(nperwn), gases,
                   tuple(distal_gases), int(n_layers))).encode())
    h.update(np.asarray(H2O_SC_NODES, dtype=float).tobytes())
    cache_file = cache_dir / f"emission_atm_{h.hexdigest()[:16]}.pkl"

    if cache_file.exists() and not recompute:
        logger.info("Loading cached emission atmosphere from '%s'", cache_file.name)
        return _load_reference(cache_file)

    # ------------------------------------------------------------------
    # Background atmospheric layers + clear-sky radiance.
    #
    # One *LEV zenith run per H2O_sc node yields the cumulative columns at
    # [obs, h1, h2]; differencing recovers each layer's (OD, isolated radiance)
    # and the observer column gives L_clear — all from a single integration, so
    # the layers and L_clear are mutually consistent (no sliced-vs-full
    # discretization gap).  See _build_atm_layers_lev.
    # ------------------------------------------------------------------
    atm_layers, L_clear, wn_out = _build_atm_layers_lev(
        atm, h_obs, h1, h2, elev, wn_start, wn_stop, nperwn, gases,
        H2O_SC_NODES, n_layers=n_layers,
    )

    # ------------------------------------------------------------------
    # Per-gas distal optical depth, for gases given an independent column
    # scaling on top of H2O_sc.
    #
    # OD is additive across species and these gases do not scale with H2O_sc
    # (nor carry a self-continuum), so the forward can apply
    # ``OD_dist + (s - 1)·OD_gas`` exactly — no extra LUT axis, one RFM run per
    # gas.  Same idiom as SolarForwardModel's atm_gas_od.  See
    # standards.EMISSION_DISTAL_GASES for why O3 and why distal.
    # ------------------------------------------------------------------
    atm_layer_gas_od: dict[str, dict[str, np.ndarray]] = {}
    h_p = 0.5 * (h1 + h2)
    for gas in distal_gases:
        _wn_g, alts_g, L_g, OD_g = _lev_column_spectra(
            atm, h_obs, elev, wn_start, wn_stop, nperwn,
            gas, ([float(h1), float(h2)] if n_layers == 3 else [h_p]), 1.0,
        )
        layers_g, _ = (
            _decompose_layers(alts_g, L_g, OD_g, h_obs, h1, h2)
            if n_layers == 3 else
            _decompose_two_layers(alts_g, L_g, OD_g, h_obs, h_p)
        )
        atm_layer_gas_od.setdefault("dist", {})[gas] = layers_g["dist"][0]
        logger.info(
            "✓ distal %s optical depth for independent scaling (mean OD %.4f)",
            gas, float(layers_g["dist"][0].mean()),
        )

    out = (atm_layers, L_clear, wn_out, atm_layer_gas_od)
    with cache_file.open("wb") as fh:
        pickle.dump(out, fh, protocol=pickle.HIGHEST_PROTOCOL)
    return out


# =============================================================================
# Top-level builder
# =============================================================================

def build_emission_reference(
    window:    FitWindowConfig,
    geometry:  EmissionGeometry,
    retrieval: Retrieval,
    model:     ModelConfig,
    params:    ParameterSet,
    cache_dir: Path | None = None,
    recompute: bool = False,
    n_layers:  int = 2,
) -> Reference:
    """
    Build (or load) the emission-geometry :class:`Reference` for a fit window.

    ``n_layers`` selects the atmospheric decomposition.  **2 is the default**:
    the atmosphere splits at the plume centre so the plume sits strictly
    between proximal and distal, giving exact sequential radiative transfer.
    3 brackets the plume band with a co-located slice and treats plume and
    slice as additive emitters neither attenuating the other — measured against
    direct RFM, that costs a flat **14.1% of the plume signal** at every
    opacity, against 0.07-0.97% for 2 layers (15-189x better).  The 3-layer
    path is retained only for that comparison; see
    ``test_real_rfm.py::test_f2_plume_assembly_vs_direct_rfm``.

    Parameters
    ----------
    window : FitWindowConfig
        Spectral window and species configuration.
    geometry : EmissionGeometry
        Emission geometry (plume height/thickness, elevation, obs height,
        atmosphere path).
    retrieval : Retrieval
        Retrieval flags.
    model : ModelConfig
        Model resolution (nperwn, padding, cache dir).
    params : ParameterSet
        Parameter set for this window (provides ref columns).
    cache_dir : Path or None
        Cache directory.  Falls back to ``model.cache_dir`` then default.
    recompute : bool
        Ignore cache and recompute.

    Returns
    -------
    Reference
        With ``atm_layers`` (prox/slice/dist), ``L_clear``, gas and aerosol
        LUTs, and ``geometry_type == EMISSION``.
    """
    from rfmwrapper import readAtm

    cache_dir = _resolve_cache_dir(cache_dir, model)

    # Whole-reference cache, keyed like the layer one.  plume_temp_nodes_for
    # depends on the geometry and on TEMP_DEFAULTS, so its output is hashed
    # rather than the tables behind it.
    cache_key = _reference_cache_key(
        window, geometry, retrieval, model, params,
        extra=(H2O_SC_NODES, plume_temp_nodes_for(geometry), AERO_R0_NODES,
               PLUME_TEMP_NODES, tuple(EMISSION_DISTAL_GASES), n_layers),
    )
    cache_file = cache_dir / f"reference_{cache_key[:16]}.pkl"
    cached = load_cached_reference(cache_file, recompute, window.label)
    if cached is not None:
        return cached

    wn_start = window.wn_start - model.model_padding
    wn_stop  = window.wn_stop  + model.model_padding
    nperwn   = model.nperwn
    elev     = geometry.elev

    # Layer altitude bounds [km]
    h1 = geometry.plume_height - geometry.plume_thickness / 2.0   # plume bottom
    h2 = geometry.plume_height + geometry.plume_thickness / 2.0   # plume top

    atm = readAtm(str(geometry.atm_path))

    # Plume temperature: the geometry's value, or ambient at plume_height.  An
    # advected plume cools adiabatically to ambient within minutes, so ambient
    # is the right a priori — and it is what makes a fitted plume_temp readable
    # as "ambient plus a few K".  v2 derived it the same way.
    plume_temp = geometry.plume_temp
    if plume_temp is None:
        plume_temp = float(np.interp(
            geometry.plume_height, np.asarray(atm["HGT"], dtype=float),
            np.asarray(atm["TEM"], dtype=float),
        ))
        logger.info("plume_temp not set; using ambient %.2f K at %.2f km",
                    plume_temp, geometry.plume_height)
    gases_str = " ".join(window.atm_gases) if window.atm_gases else "H2O"

    logger.info(
        "Building emission Reference for '%s': obs=%.2f h1=%.2f h2=%.2f km",
        window.label, geometry.obs_height, h1, h2,
    )

    # Background atmospheric layers, clear-sky radiance and distal per-gas
    # optical depths: the RFM part, cached on its own inputs.
    distal_gases = [g for g in window.atm_gases if g in EMISSION_DISTAL_GASES]
    atm_layers, L_clear, wn_out, atm_layer_gas_od = _atm_layers_cached(
        atm, Path(geometry.atm_path), geometry.obs_height, h1, h2, elev,
        wn_start, wn_stop, nperwn, gases_str, distal_gases, n_layers,
        cache_dir=cache_dir, recompute=recompute,
    )

    # ------------------------------------------------------------------
    # Plume gas LUTs (queried at plume temperature)
    # ------------------------------------------------------------------
    gas_luts: dict[str, GasLUT] = {}
    plume_path = geometry.plume_thickness
    # Pressure at the plume centre, read from the profile.  Was hardcoded at
    # 1013 mbar ("plume is near-surface"), which is wrong for any elevated
    # plume: mls.atm gives 902 mbar at the 1.0 km used on 2019-09-11, 12% high.
    # This sets the SO2 pressure broadening in the LUT, so it changes line
    # shapes — not the column, which comes from ref_scd.
    plume_pres = float(np.interp(
        geometry.plume_height, np.asarray(atm["HGT"], dtype=float),
        np.asarray(atm["PRE"], dtype=float),
    ))
    logger.info(
        "Plume pressure at %.2f km from '%s': %.1f mbar",
        geometry.plume_height, Path(geometry.atm_path).name, plume_pres,
    )
    # Surface ambient temperature, for the aerosol single-scattering source.
    # Read here for the same reason plume_temp is: the atmosphere is already
    # open, and config objects do no file I/O.  Stored regardless of
    # model_aero_scattering so the flag is fit-time only.
    T_surface = float(np.interp(
        geometry.obs_height, np.asarray(atm["HGT"], dtype=float),
        np.asarray(atm["TEM"], dtype=float),
    ))
    logger.info("Surface temperature at %.2f km from '%s': %.2f K",
                geometry.obs_height, Path(geometry.atm_path).name, T_surface)

    plume_temp_ref = float(np.median(PLUME_TEMP_NODES))

    for species in window.plume_gases:
        if species in params:
            ref_scd = params[species].ref_column
        else:
            ref_conc = GAS_DEFAULTS.get(species, {}).get("conc", 1.0)
            ref_scd  = vmr_to_scd(ref_conc, plume_path, temp=plume_temp_ref, pres=plume_pres)

        is_continuum = species in window.continuum_gases
        gas_luts[species] = GasLUT.load_or_compute(
            species=species, wn_start=wn_start, wn_stop=wn_stop, nperwn=nperwn,
            temp_nodes=plume_temp_nodes_for(geometry), pathlength=plume_path, pres=plume_pres,
            ref_scd=float(ref_scd), continuum=is_continuum,
            cache_dir=cache_dir, recompute=recompute,
        )

    # ------------------------------------------------------------------
    # Plume aerosol LUTs — with absorption (needed for L_plume self-emission)
    # ------------------------------------------------------------------
    aero_luts: dict[str, AerosolLUT] = {}
    for species in window.plume_aero:
        if species in params:
            ref_mass = params[species].ref_column
        else:
            ref_conc = AERO_DEFAULTS.get(species, {}).get("conc", 1e-3)
            ref_mass = ref_conc * geometry.plume_thickness * 1e3

        if species == "H2SO4":
            aero_luts[species] = AerosolLUT.build_h2so4_2d(
                wn=wn_out, ref_mass_g_m2=float(ref_mass), with_absorption=True,
            )
        else:
            aero_luts[species] = AerosolLUT.build_1d(
                species=species, wn=wn_out, r0_nodes_um=AERO_R0_NODES,
                ref_mass_g_m2=float(ref_mass), with_absorption=True,
                cache_dir=cache_dir, recompute=recompute,
            )

    ref = Reference(
        wn=wn_out,
        gas_luts=gas_luts,
        aero_luts=aero_luts,
        geometry_type=GeometryType.EMISSION,
        plume_temp=plume_temp,
        T_surface=T_surface,
        atm_layers=atm_layers,
        atm_layer_gas_od=atm_layer_gas_od,
        L_clear=L_clear,
    )
    _save_reference(ref, cache_file)
    logger.info("✓ Emission Reference for '%s' built and cached", window.label)
    return ref
