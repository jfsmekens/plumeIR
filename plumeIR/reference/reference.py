"""
Reference — pre-computed spectral calibration object for a single fit window.

Holds all GasLUTs and AerosolLUTs needed by the forward model.
Content-addressed caching: any change to species, spectral range,
temperature grids, or geometry triggers a full recompute.

Phase 3 scope: layer geometry only.
Emission and solar radiance LUTs are deferred to Phase 3b.
"""

from __future__ import annotations

import dataclasses
import hashlib
import logging
import pickle
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

from plumeIR.constants import H2SO4_AQ, POSSIBLE_AEROSOLS
from plumeIR.config.objects import (
    FitWindowConfig,
    GeometryType,
    LayerGeometry,
    ModelConfig,
    Retrieval,
)
from plumeIR.parameters.parameter import ParameterSet
from plumeIR.reference.gas_lut import GasLUT, _ATM_TEMP_NODES, _PLUME_TEMP_NODES, _H2O_VMR_NODES
from plumeIR.reference.aero_lut import AerosolLUT, _R0_NODES_UM
from plumeIR.standards import GAS_DEFAULTS, AERO_DEFAULTS
from plumeIR.units import scd_to_vmr, vmr_to_scd

logger = logging.getLogger(__name__)


# =============================================================================
# Reference dataclass
# =============================================================================

@dataclass
class Reference:
    """
    Pre-computed spectral calibration for one fit window.

    Parameters
    ----------
    wn : np.ndarray
        Model wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    gas_luts : dict[str, GasLUT]
        One GasLUT per gas species.  Key is the species name.
    aero_luts : dict[str, AerosolLUT]
        One AerosolLUT per aerosol species.  Key is the species name.
    T_surface : float or None
        Surface-level ambient temperature [K], read from the atmosphere
        profile at the observer altitude.  Source temperature for the aerosol
        single-scattering term (emission geometry).  Stored **unconditionally**,
        whether or not ``model_aero_scattering`` is set, so the flag can be
        toggled without rebuilding the Reference.
    geometry_type : GeometryType
        The geometry this Reference was built for.
    atm_layers : dict[str, AtmLayerLUT]
        Emission geometry only: the three background atmospheric layers
        keyed ``'prox'``, ``'slice'``, ``'dist'``.  Empty for layer geometry.
    atm_layer_gas_od : dict
        Emission only.  ``{layer: {gas: OD}}`` — the per-gas optical depth of
        one atmospheric layer, for gases given an independent column scaling on
        top of ``H2O_sc``.  Currently the distal O3 term (see
        ``standards.EMISSION_DISTAL_GASES``).  Applied additively in OD space.
    plume_temp : float or None
        Emission only.  Plume temperature actually used [K] — the geometry's
        value, or ambient from the profile at ``plume_height`` when that was
        left unset.  Resolved here because this is where the atmosphere is
        read; config objects do no file I/O.
    L_clear : np.ndarray or None
        Emission geometry only: clear-sky radiance (full atmosphere, no
        plume) on the model grid, for difference mode.  ``None`` otherwise.
    atm_gas_od : dict[str, np.ndarray]
        Solar geometry only: vertical optical depth per atmospheric gas at
        the a priori column (``scaling=1``), shape ``(n_wn,)`` each.  For a
        gas with ``atm_gas_od_cont`` the forward applies
        ``OD = airmass · (s·atm_gas_od[gas] + (s²−s)·atm_gas_od_cont[gas])``;
        otherwise the plain linear ``OD = airmass · s · atm_gas_od[gas]``.
    atm_gas_od_cont : dict[str, np.ndarray]
        Solar geometry only: the **self-continuum** component ``B`` of the
        vertical OD for continuum gases (H2O), shape ``(n_wn,)`` each.  Stored
        only for gases in the window's ``continuum_gases``.  Because the
        self-continuum scales as column² while lines (and the foreign
        continuum) scale linearly, the per-wavenumber OD is exactly
        ``OD(s) = s·A + s²·B`` with ``A = atm_gas_od − B``; storing ``B`` lets
        the forward apply ``s·atm_gas_od + (s²−s)·B`` (exact, reduces to
        linear when ``B = 0``).  Absent ⇒ plain linear column scaling.
    atm_gas_refcol : dict[str, float]
        Solar geometry only: vertical total column [molec/cm²] per atmospheric
        gas at the a priori profile.  Used to convert the retrieved scaling to
        an absolute column → Xgas.  The retrieved column is ``scaling·refcol``
        regardless of the continuum split (column ∝ scaling; the quadratic
        term is an optical-depth effect, not a molecule-count effect).
    """
    wn:              np.ndarray
    gas_luts:        dict[str, GasLUT]     = field(default_factory=dict)
    aero_luts:       dict[str, AerosolLUT] = field(default_factory=dict)
    T_surface:       float | None = None
    geometry_type:   GeometryType          = GeometryType.LAYER
    atm_layers:      dict                  = field(default_factory=dict)
    L_clear:         np.ndarray | None      = None
    plume_temp:      float | None           = None
    atm_layer_gas_od: dict                 = field(default_factory=dict)
    atm_gas_od:      dict                  = field(default_factory=dict)
    atm_gas_od_cont: dict                  = field(default_factory=dict)
    atm_gas_refcol:  dict                  = field(default_factory=dict)


# =============================================================================
# Builder
# =============================================================================

def build_reference(
    window:      FitWindowConfig,
    geometry:    LayerGeometry,
    retrieval:   Retrieval,
    model:       ModelConfig,
    params:      ParameterSet,
    cache_dir:   Path | None = None,
    recompute:   bool = False,
) -> Reference:
    """
    Build (or load from cache) a :class:`Reference` for a fit window.

    Parameters
    ----------
    window : FitWindowConfig
        Spectral window and species configuration.
    geometry : LayerGeometry
        Layer geometry parameters (pathlength, T, P).
    retrieval : Retrieval
        Retrieval flags (fit_size_aero, fit_aero_composition, use_babs_aero).
    model : ModelConfig
        Model resolution (nperwn, model_padding, cache_dir).
    params : ParameterSet
        Parameter set for this window (used for ref_scd values).
    cache_dir : Path or None
        Cache directory.  Falls back to ``model.cache_dir`` then
        ``~/.plumeIR/cache/``.
    recompute : bool
        Ignore cache and recompute everything.

    Returns
    -------
    Reference
    """
    if not isinstance(geometry, LayerGeometry):
        raise NotImplementedError(
            "build_reference currently only supports LayerGeometry. "
            "Emission and solar support is planned for Phase 3b."
        )

    cache_dir = _resolve_cache_dir(cache_dir, model)

    # Check top-level cache
    cache_key  = _reference_cache_key(
        window, geometry, retrieval, model, params,
        extra=(_ATM_TEMP_NODES, _PLUME_TEMP_NODES, _H2O_VMR_NODES, _R0_NODES_UM),
    )
    cache_file = cache_dir / f"reference_{cache_key[:16]}.pkl"
    cached = load_cached_reference(cache_file, recompute, window.label)
    if cached is not None:
        return cached

    logger.info("Building Reference for window '%s' [%.0f–%.0f cm⁻¹]",
                window.label, window.wn_start, window.wn_stop)

    # Model wavenumber grid
    wn_start = window.wn_start - model.model_padding
    wn_stop  = window.wn_stop  + model.model_padding
    nperwn   = model.nperwn

    gas_luts:  dict[str, GasLUT]     = {}
    aero_luts: dict[str, AerosolLUT] = {}

    # ------------------------------------------------------------------
    # Gas LUTs
    # ------------------------------------------------------------------
    # One LUT per (gas, role), keyed by parameter name.  A dual gas (14b) gets
    # two: '{GAS}_atm' on the ambient axis and path, '{GAS}' on the plume's.
    roles = ([(window.atm_name(g), g, True) for g in window.atm_gases]
             + [(g, g, False) for g in window.plume_gases])

    for key, species, is_atm in roles:
        # Decide temperature grid and reference path based on layer role.
        # Atmospheric gas fills the full path; plume gas occupies only the
        # plume thickness (matching the ParameterSet ref_column convention).
        if is_atm:
            temp_nodes = _ATM_TEMP_NODES
            temp_ref   = geometry.atm_temp
            pres       = geometry.atm_pres
            pathlength = geometry.pathlength
        else:
            temp_nodes = _PLUME_TEMP_NODES
            temp_ref   = geometry.plume_temp
            pres       = geometry.plume_pres
            pathlength = geometry.plume_thickness

        # Reference SCD: from ParameterSet if present, else from standards
        if key in params:
            ref_scd = params[key].ref_column
        else:
            ref_conc_ppm = GAS_DEFAULTS.get(species, {}).get("conc", 1.0)
            ref_scd = vmr_to_scd(
                ref_conc_ppm, pathlength, temp=temp_ref, pres=pres
            )

        # Continuum (2-D T,VMR) treatment is a per-window choice.
        is_continuum = window.uses_2d(species, plume=not is_atm)
        vmr_nodes    = _H2O_VMR_NODES if is_continuum else None
        # Reference VMR (concentration at scaling=1) for 2-D continuum gases;
        # recovered exactly from ref_scd via the inverse conversion.
        ref_vmr = (
            scd_to_vmr(ref_scd, pathlength, temp=temp_ref, pres=pres)
            if is_continuum else None
        )

        gas_luts[key] = GasLUT.load_or_compute(
            species    = species,
            wn_start   = wn_start,
            wn_stop    = wn_stop,
            nperwn     = nperwn,
            temp_nodes = temp_nodes,
            pathlength = pathlength,
            pres       = pres,
            ref_scd    = float(ref_scd),
            vmr_nodes  = vmr_nodes,
            ref_vmr    = float(ref_vmr) if ref_vmr is not None else None,
            continuum  = is_continuum,
            cache_dir  = cache_dir,
            recompute  = recompute,
        )

    # ------------------------------------------------------------------
    # Aerosol LUTs (layer geometry only)
    # ------------------------------------------------------------------
    # Shared wavenumber grid (materialised from one of the gas LUTs)
    if gas_luts:
        wn_out = next(iter(gas_luts.values())).wn
    else:
        # No gas LUTs — synthesise grid
        wn_out = np.arange(wn_start, wn_stop, 1.0 / nperwn)

    for species in window.plume_aero:
        # Reference mass column [g/m²]: from the ParameterSet (= ref
        # concentration × plume_thickness) if present, else derived here.
        if species in params:
            ref_mass = params[species].ref_column
        else:
            ref_conc = AERO_DEFAULTS.get(species, {}).get("conc", 1e-3)
            ref_mass = ref_conc * geometry.plume_thickness * 1e3

        # Layer geometry needs only extinction (transmittance); the emission
        # builder requests absorption too (with_absorption=True).
        if species == H2SO4_AQ:
            aero_luts[species] = AerosolLUT.build_h2so4_2d(
                wn            = wn_out,
                ref_mass_g_m2 = float(ref_mass),
                with_absorption = False,
            )
        else:
            aero_luts[species] = AerosolLUT.build_1d(
                species         = species,
                wn              = wn_out,
                r0_nodes_um     = _R0_NODES_UM,
                ref_mass_g_m2   = float(ref_mass),
                with_absorption = False,
                cache_dir       = cache_dir,
                recompute       = recompute,
            )

    ref = Reference(
        wn            = wn_out,
        gas_luts      = gas_luts,
        aero_luts     = aero_luts,
        geometry_type = GeometryType.LAYER,
    )

    _save_reference(ref, cache_file)
    logger.info("✓ Reference for '%s' built and cached", window.label)
    return ref


# =============================================================================
# Cache helpers
# =============================================================================

def _resolve_cache_dir(cache_dir: Path | None, model: ModelConfig) -> Path:
    d = cache_dir or model.cache_dir or (Path.home() / ".plumeIR" / "cache")
    d = Path(d)
    d.mkdir(parents=True, exist_ok=True)
    return d


#: Bumped whenever the *layout* of a cached ``Reference`` — or of any LUT it
#: holds — changes, as opposed to its inputs.  Input hashing alone cannot see a
#: schema change: the same config would resolve to a pickle whose fields no
#: longer mean what the current code expects, or whose classes no longer exist
#: at the recorded module path.
#:
#:   1  original layout
#:   2  AerosolLUT stores its tables on the coarse Mie grid, plus ``wn_model``
#:      (a version-2 reader loads version 1 correctly; the bump only makes old
#:      references regenerate into the smaller format)
#:   3  Reference stores the surface ambient temperature for the aerosol
#:      single-scattering source (4648b2c, 2026-08-26)
#:   4  config dataclasses moved from ``plumeIR.parameters.geometry`` to
#:      ``plumeIR.config.objects`` (2026-11-10): older pickles name a module
#:      that no longer exists and cannot be loaded
_REFERENCE_SCHEMA_VERSION: int = 4


def _file_digest(path: Path) -> bytes:
    """SHA-256 of a file's contents, so an edited file changes the key."""
    return hashlib.sha256(Path(path).read_bytes()).digest()


def _reference_cache_key(
    window:    FitWindowConfig,
    geometry,
    retrieval: Retrieval,
    model:     ModelConfig,
    params:    ParameterSet,
    extra:     tuple = (),
) -> str:
    """
    SHA-256 of every input that determines a Reference, for any geometry.

    Shared by the layer, emission and solar builders.  It is deliberately
    conservative: **every** geometry field enters, fit flags included, because
    choosing by hand which fields reach the LUTs is how the original layer key
    came to miss ``plume_thickness``, the continuum set and the configured
    concentrations — each of which changes the reference while leaving the key
    unchanged.  A flag that does not affect the LUTs costs only a reassembly,
    since the expensive parts (``GasLUT``, the Mie tables, the emission
    atmospheric layers) are cached separately under their own keys.

    Parameters
    ----------
    window : FitWindowConfig
        Range and species, each list in its role and order.
    geometry
        Any geometry dataclass.  A ``Path`` field naming an existing file
        contributes the file's contents (an edited atmosphere profile must
        rebuild), not only its name.
    retrieval : Retrieval
        Only the flags that change LUT content.
    model : ModelConfig
        Grid spacing and padding.
    params : ParameterSet
        Each parameter's ``ref_column``, which carries the configured reference
        concentrations into the LUTs.
    extra : tuple
        Builder-specific inputs (node grids, layer count), hashed by ``repr``.

    Returns
    -------
    str
        Hex digest.
    """
    h = hashlib.sha256()
    h.update(f"schema{_REFERENCE_SCHEMA_VERSION}|{type(geometry).__name__}".encode())
    h.update(repr((float(window.wn_start), float(window.wn_stop),
                   tuple(window.atm_gases), tuple(window.plume_gases),
                   tuple(window.plume_aero), tuple(window.continuum_gases))).encode())
    for f in dataclasses.fields(geometry):
        value = getattr(geometry, f.name)
        h.update(f"|{f.name}={value!r}".encode())
        if isinstance(value, Path) and value.is_file():
            h.update(_file_digest(value))
    h.update(repr((float(model.nperwn), float(model.model_padding))).encode())
    h.update(repr((bool(retrieval.use_babs_aero),
                   bool(retrieval.fit_aero_composition))).encode())
    h.update(repr([(p.name, float(p.ref_column)) for p in params]).encode())
    for item in extra:
        h.update(b"|")
        h.update(item.tobytes() if isinstance(item, np.ndarray) else repr(item).encode())
    return h.hexdigest()


def load_cached_reference(cache_file: Path, recompute: bool, label: str) -> Reference | None:
    """
    Load a cached Reference, or return ``None`` when it must be built.

    Parameters
    ----------
    cache_file : Path
        The content-addressed pickle.
    recompute : bool
        Ignore the cache.
    label : str
        Window label, for the log.

    Returns
    -------
    Reference or None
    """
    if recompute or not cache_file.exists():
        return None
    logger.info("Loading cached Reference for '%s' from '%s'", label, cache_file.name)
    return _load_reference(cache_file)


def _save_reference(ref: Reference, path: Path) -> None:
    with path.open("wb") as fh:
        pickle.dump(ref, fh, protocol=pickle.HIGHEST_PROTOCOL)
    logger.debug("Saved Reference → '%s'", path.name)


def _load_reference(path: Path) -> Reference:
    with path.open("rb") as fh:
        return pickle.load(fh)
