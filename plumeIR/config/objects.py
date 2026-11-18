"""
The config objects: one frozen dataclass per config file section.

Built by :mod:`plumeIR.config.builder`; they do no file I/O.  (They lived in
``plumeIR/parameters/geometry.py`` until 2026-11-10, named after one of them.)

    DataConfig       ← *DATA
    ModelConfig      ← *MODEL
    LayerGeometry    ← *GEOMETRY  (type: layer)
    EmissionGeometry ← *GEOMETRY  (type: emission)
    SolarGeometry    ← *GEOMETRY  (type: solar)
    Retrieval        ← *RETRIEVAL
    FitWindowConfig  ← *FIT0, *FIT1, …
    PlotConfig       ← *PLOT (optional)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path

from plumeIR.config.keys import RATIO_REGRESSIONS, default

# Config defaults are declared once, in the key catalogue (config/keys.py,
# M3d); the fields below read them through ``default``.  Fields without a
# config key (Retrieval.geometry_type, the *_explicit flags, PlotConfig.ratios
# and background) keep theirs here.


# =============================================================================
# GeometryType
# =============================================================================

class GeometryType(Enum):
    LAYER    = auto()
    EMISSION = auto()
    SOLAR    = auto()


# =============================================================================
# ModelConfig  (*MODEL)
# =============================================================================

@dataclass(frozen=True)
class DataConfig:
    """
    Spectral-data location and file selection (``*DATA`` section).

    Parameters
    ----------
    data_dir : Path
        Directory containing the spectral data files.
    filter_files : bool
        Whether to apply ``filter_string`` to file names.
    filter_string : str or None
        Keep only files whose stem contains this string (when ``filter_files``).
    filter_ext : str or None
        Keep only files with this extension (e.g. ``'spc'``, ``'0'``).
    max_spec : int or None
        Process at most this many spectra (the earliest by acquisition time).
    """
    data_dir:      Path
    filter_files:  bool = default("DATA", "filter_files")
    filter_string: str | None = default("DATA", "filter_string")
    filter_ext:    str | None = default("DATA", "filter_ext")
    max_spec:      int | None = default("DATA", "max_spec")


@dataclass(frozen=True)
class ModelConfig:
    """
    Per-project forward model settings.

    RFM executable and HITRAN paths are resolved by RFMwrapper's own
    configuration chain (``~/.rfmwrapper/config.json`` → env vars →
    defaults).  They are not duplicated here.  The mieEODG ARIA database
    is always the bundled version shipped with mieEODG and is not
    configurable.

    Parameters
    ----------
    nperwn : float
        Model spectral resolution [points per cm⁻¹].  Default 25.
    model_padding : float
        Padding added to each side of the fit window for ILS convolution
        [cm⁻¹].  Default 50.
    cache_dir : Path or None
        Directory for caching pre-computed Reference objects.  If ``None``,
        defaults to ``~/.plumeIR/cache/``.
    """
    nperwn:        float       = default("MODEL", "nperwn")
    model_padding: float       = default("MODEL", "model_padding")
    cache_dir:     Path | None = default("MODEL", "cache_dir")


# =============================================================================
# Geometry dataclasses  (*GEOMETRY)
# =============================================================================

@dataclass(frozen=True)
class LayerGeometry:
    """
    Geometry and forward-model flags for transmission (layer) measurements.

    The plume is observed against a bright IR source.  Two homogeneous
    layers are modelled: atmospheric background and plume.

    Parameters
    ----------
    pathlength : float
        Total geometric path length [km].
    atm_temp : float
        Ambient atmospheric temperature [K].
    atm_pres : float
        Ambient atmospheric pressure [mbar].
    plume_temp : float
        Plume gas temperature [K].
    plume_pres : float
        Plume gas pressure [mbar].
    plume_thickness : float
        Thickness of the plume layer [km].
    fit_gas_temp : bool
        Fit the plume gas temperature as a free parameter.
    fit_atm_temp : bool
        Fit the atmospheric temperature as a free parameter.
    model_gas_emission : bool
        Include thermal emission from the hot plume gas in the forward model.
    fit_source_temp : bool
        Fit the background source temperature as a free parameter.
    source_temp : float
        A priori source temperature [K].
    no_gas : bool
        Derive plume transmittance using a reference "no-gas" spectrum
        rather than subtracting a background.
    path_tracer : str or None
        Ambient gas whose fitted column checks ``pathlength`` after the fits
        (:mod:`plumeIR.path_tracer`); a diagnostic, it changes no fit.
        ``None`` → no diagnostic.
    """
    pathlength:          float
    atm_temp:            float
    atm_pres:            float
    plume_temp:          float
    plume_pres:          float
    plume_thickness:     float = default("GEOMETRY", "plume_thickness", "layer")
    fit_gas_temp:        bool  = default("GEOMETRY", "fit_gas_temp")
    fit_atm_temp:        bool  = default("GEOMETRY", "fit_atm_temp")
    model_gas_emission:  bool  = default("GEOMETRY", "model_gas_emission")
    fit_source_temp:     bool  = default("GEOMETRY", "fit_source_temp")
    source_temp:         float = default("GEOMETRY", "source_temp")
    no_gas:              bool  = default("GEOMETRY", "no_gas")
    path_tracer:         str | None  = default("GEOMETRY", "path_tracer")


@dataclass(frozen=True)
class EmissionGeometry:
    """
    Geometry and forward-model flags for self-emission measurements.

    The instrument looks upward at the plume against a cold sky background.
    Three layers are modelled: proximal atmosphere (observer → plume),
    plume slice, and distal atmosphere (above plume).

    Parameters
    ----------
    plume_height : float
        Height of the plume centre above sea level [km].
    plume_thickness : float
        Thickness of the plume layer [km].
    elev : float
        Elevation angle [degrees from horizontal].
    obs_height : float
        Observer altitude above sea level [km].
    atm_path : Path
        Path to the RFM-format atmosphere profile file.
    plume_temp : float or None
        A priori plume temperature [K].  ``None`` (the default) resolves it
        from the atmosphere profile at ``plume_height`` — the plume is assumed
        to be at ambient temperature, which an advected plume reaches within
        minutes through adiabatic cooling.  A config value overrides it.

        It feeds **both** the gas LUT query (absorption line strengths) and the
        Planck self-emission term, as Kirchhoff requires for a single
        near-isothermal layer.  When ``fit_tplume`` is set it is fitted
        directly, bounded about ambient by ``TEMP_DEFAULTS['plume_temp']``.
        There is deliberately no ``dt_plume`` offset: an offset that moved only
        the emission level was exactly degenerate with the plume column: a trace
        column with a hot plume mimics a real column at ambient, and fitted
        on real spectra it ran to its bound and collapsed the SO2 column.
    obs_temp : float or None
        Observer-level ambient temperature [K].  Not used in v3.0: kept for
        the planned proximal-layer prior.
    obs_rh : float or None
        Observer-level relative humidity [%].  Not used in v3.0, as
        ``obs_temp``.
    fit_tplume : bool
        Fit ``plume_temp`` as a free parameter.  Default False — see above.
    fit_tprox : bool
        Fit the proximal layer temperature offset as a free parameter.
    dt_prox : float
        A priori proximal layer temperature offset [K].  Applied analytically
        to the proximal effective emission temperature ``T_eff``.
    fit_tdist : bool
        Fit the distal layer temperature offset as a free parameter.
    dt_dist : float
        A priori distal layer temperature offset [K].  Applied analytically
        to the distal effective emission temperature ``T_eff``.
    h2o_scaling : int
        Number of atmospheric layers (0–3) in which the background H2O column
        is scaled by the shared ``H2O_sc`` parameter.  0 disables H2O_sc.
    model_aero_scattering : bool
        Add a single-scattering source term to the plume layer.  Default
        False — the forward is pure absorption/emission without it, which is
        the behaviour every result before 2026-08-26 was produced with.

        Aerosol enters the forward as extinction (``T_plume``) and absorption
        (``L_plume``); the ~40% of extinction that is *redirection* is removed
        from the beam and never returned.  At episode peak on 2019-09-11 the
        scattering optical depth reaches ~0.19 with single-scattering albedo
        0.38 (ash) / 0.43 (H2SO4), i.e. ~17% of the beam, so this is not a
        negligible term when aerosol is present.

        See ``EmissionForwardModel._scattering_source``.
    fit_difference : bool
        Fit the difference spectrum (plume − clear sky).  Default False:
        absolute radiance is the validated retrieval.  Difference mode is not
        supported on ``main`` (preprocessing raises); the experimental
        implementation is parked on branch ``park/difference-mode``
        (2026-11-10).
    use_bbt : bool
        Convert spectra to brightness temperature before fitting.
    bb_drift : bool
        Correct for blackbody reference drift over time.
    clear_drift : bool
        Correct for clear-sky reference drift over time.
    """
    plume_height:    float
    plume_thickness: float
    elev:            float
    obs_height:      float
    atm_path:        Path
    plume_temp:      float | None = default("GEOMETRY", "plume_temp")
    obs_temp:        float | None = default("GEOMETRY", "obs_temp")
    obs_rh:          float | None = default("GEOMETRY", "obs_rh")
    fit_tplume:      bool  = default("GEOMETRY", "fit_tplume")
    fit_tprox:       bool  = default("GEOMETRY", "fit_tprox")
    dt_prox:         float = default("GEOMETRY", "dt_prox")
    fit_tdist:       bool  = default("GEOMETRY", "fit_tdist")
    dt_dist:         float = default("GEOMETRY", "dt_dist")
    h2o_scaling:     int   = default("GEOMETRY", "h2o_scaling")
    model_aero_scattering: bool = default("GEOMETRY", "model_aero_scattering")
    fit_difference:  bool  = default("GEOMETRY", "fit_difference")
    use_bbt:         bool  = default("GEOMETRY", "use_bbt")
    bb_drift:        bool  = default("GEOMETRY", "bb_drift")
    clear_drift:     bool  = default("GEOMETRY", "clear_drift")


@dataclass(frozen=True)
class PreprocessConfig:
    """
    What is done to the spectra before the fits (``*PREPROCESS``).

    The defaults depend on the geometry; build it with
    :func:`plumeIR.config.builder.build_preprocess`.  The instance defaults are
    the layer geometry's.

    Parameters
    ----------
    subtract_self : bool
        Layer: subtract the instrument self-emission background (a spectrum
        taken looking at cold sky) from each single beam.  Emission: ignored
        with a warning, since the blackbody calibration already removes the
        instrument radiance.  Moved from ``*GEOMETRY`` 2026-11-15.
    self_file : Path or None
        Layer: an explicit self-emission spectrum, overriding discovery.
        ``None`` → files whose stem contains ``'self'`` in ``data_dir`` (the v2
        convention).
    background : {'off', 'report', 'apply'}
        Layer: the additive background measured from the opaque bands
        (:mod:`plumeIR.preprocess.background`).  ``apply`` subtracts it,
        unclipped, in place of the self spectrum.
    registration : {'off', 'report', 'apply'}
        Layer and emission: the wavenumber shift and scale measured by
        cross-correlation (:mod:`plumeIR.preprocess.registration`).
        ``apply`` corrects each spectrum's wavenumber axis before the fits.
    reference_spectra : int
        Layer: the median top hull of the first this-many spectra is the
        reference for the relative scale and temperature.
    """
    subtract_self:       bool        = default("PREPROCESS", "subtract_self", "layer")
    self_file:           Path | None = default("PREPROCESS", "self_file")
    background:          str         = default("PREPROCESS", "background")
    registration:        str         = default("PREPROCESS", "registration")
    reference_spectra:   int         = default("PREPROCESS", "reference_spectra")


@dataclass(frozen=True)
class SolarGeometry:
    """
    Geometry for solar (or lunar) occultation measurements.

    Ground-based sun/moon-tracking FTIR.  The measured signal is the solar
    beam attenuated by the slant atmospheric column (and an optional plume).
    Pure transmittance — no emission term.

    Atmospheric LUTs are built in **vertical** geometry; the per-spectrum
    slant path is applied as an airmass factor ``m = 1/sin(elev)`` in the
    forward.  The airmass is a **fixed, known per-spectrum input** (from the
    ephemeris / tracker), not a fit parameter — fitting it would be degenerate
    with the gas column scalings.

    Parameters
    ----------
    obs_height : float
        Observer altitude above sea level [km].
    atm_path : Path
        Path to the RFM-format atmosphere profile file.
    elev : float
        Nominal solar elevation angle [degrees from horizontal].  Used as a
        fallback; per-spectrum elevation comes from the ephemeris/tracker
        (astropy integration deferred).
    plume_height : float or None
        Plume centre altitude [km] if a volcanic plume is in the path.
        ``None`` for clean total-column retrievals.
    plume_thickness : float or None
        Plume layer thickness [km].
    plume_temp : float or None
        Plume temperature [K].  ``None`` → read from the profile at
        ``plume_height`` (near thermal equilibrium for occultation).
    """
    obs_height:      float
    atm_path:        Path
    elev:            float = default("GEOMETRY", "elev", "solar")
    plume_height:    float | None = default("GEOMETRY", "plume_height")
    plume_thickness: float | None = default("GEOMETRY", "plume_thickness")
    plume_temp:      float | None = default("GEOMETRY", "plume_temp")


# =============================================================================
# Retrieval  (*RETRIEVAL)
# Geometry-agnostic: applies to all geometry types.
# =============================================================================

@dataclass(frozen=True)
class Retrieval:
    """
    Retrieval configuration: arguments passed to the forward model and fitter.

    All fields here are independent of geometry type.  Geometry-specific
    flags live in the geometry dataclasses above.

    Parameters
    ----------
    geometry_type : GeometryType
        Set by the builder from *GEOMETRY; not in the config file directly.
    retrieval_method : str
        ``'nlls'`` (Levenberg-Marquardt via scipy) or ``'oe'`` (Optimal
        Estimation via pyOptimalEstimation).
    use_bounds : bool
        Apply parameter bounds during optimisation.
    update_params : bool
        Seed each fit with the result of the previous spectrum.
    fit_size_aero : bool or tuple of str
        Fit aerosol effective radius (``r0_{SPECIES}``) as a free parameter:
        for every aerosol (True / False) or for those named (2026-11-17).
        A window's own value overrides it (``FitWindowConfig``).
    fit_aero_composition : bool or tuple of str
        Fit aerosol composition (``wt_{SPECIES}``, weight fraction) as a free
        parameter, likewise.  Currently only meaningful for H2SO4.  The 2D (r0, wt)
        LUT is always built regardless; this flag controls whether composition
        is varied or fixed at the ``*DEFAULTS`` value at run time.
    use_babs_aero : bool
        Use absorption optical depth (Babs) rather than extinction (Bext)
        for aerosol species.
    fit_fov : bool
        Fit the instrument FOV as a free parameter.
    fov : float
        A priori FOV [rad].
    fit_max_opd : bool
        Fit the maximum optical path difference as a free parameter.
    max_opd : float
        A priori maximum OPD [cm].
    apod_type : str
        ILS apodization function name (see constants.APOD_TYPES).
    fit_shift : bool
        Fit wavenumber shift as a free parameter.
    nu_shift : float
        A priori wavenumber shift [cm⁻¹].
    fit_offset : bool
        Fit additive spectral offset as a free parameter.
    offset : float
        A priori spectral offset.
    sigma_prior : dict[str, float]
        Per-parameter 1-sigma prior uncertainty for the OE backend.
        Keys are parameter names; absent keys are derived from bounds.
    """
    geometry_type:    GeometryType

    retrieval_method: str   = default("RETRIEVAL", "retrieval_method")
    use_bounds:       bool  = default("RETRIEVAL", "use_bounds")
    update_params:    bool  = default("RETRIEVAL", "update_params")

    fit_size_aero:        bool | tuple[str, ...] = default("RETRIEVAL", "fit_size_aero")
    fit_aero_composition: bool | tuple[str, ...] = default("RETRIEVAL", "fit_aero_composition") # adds wt_{SPECIES} as a free parameter
    use_babs_aero:        bool  = default("RETRIEVAL", "use_babs_aero")

    fit_fov:          bool  = default("RETRIEVAL", "fit_fov")
    fov:              float = default("RETRIEVAL", "fov")
    fit_max_opd:      bool  = default("RETRIEVAL", "fit_max_opd")
    max_opd:          float = default("RETRIEVAL", "max_opd")
    apod_type:        str   = default("RETRIEVAL", "apod_type")
    # Whether the config set these explicitly.  When False the value above is
    # only a fallback and the data file wins — see
    # spectra.resolve_instrument_ils, which sets ``ils_resolved`` on the copy it
    # returns so a second call (prepare_run, then iter_retrieval) is a no-op.
    max_opd_explicit:   bool = False
    apod_type_explicit: bool = False
    ils_resolved:       bool = False
    fit_shift:        bool  = default("RETRIEVAL", "fit_shift")
    nu_shift:         float = default("RETRIEVAL", "nu_shift")
    fit_offset:       bool  = default("RETRIEVAL", "fit_offset")
    offset:           float = default("RETRIEVAL", "offset")

    sigma_prior:      dict[str, float] = field(default_factory=dict)


# =============================================================================
# FitWindowConfig  (*FIT0, *FIT1, …)
# =============================================================================

@dataclass(frozen=True)
class FitWindowConfig:
    """
    Configuration for a single spectral fit window.

    Parameters
    ----------
    label : str
        Window identifier, e.g. ``'FIT0'``.
    wn_start : float
        Lower wavenumber bound [cm⁻¹].
    wn_stop : float
        Upper wavenumber bound [cm⁻¹].
    atm_gases : tuple[str, ...]
        Atmospheric gas species to model.
    plume_gases : tuple[str, ...]
        Plume gas species to model.
    plume_aero : tuple[str, ...]
        Plume aerosol species to model.
    targets : tuple[str, ...]
        Species or diagnostic parameters to report as primary outputs.
    polydeg : int
        Degree of the multiplicative background polynomial.
    fit_gas_temp : bool or None
        Per-window override of ``LayerGeometry.fit_gas_temp``.
        ``None`` means inherit from the geometry.
    fit_atm_temp : bool or None
        Per-window override of ``LayerGeometry.fit_atm_temp``.
    model_gas_emission : bool or None
        Per-window override of ``LayerGeometry.model_gas_emission``.
    fit_size_aero : bool, tuple of str or None
        Per-window override of ``RetrievalConfig.fit_size_aero``: True /
        False for every aerosol in the window, or the aerosols whose
        effective radius is fitted (the others held); ``None`` inherits.
    fit_aero_composition : bool, tuple of str or None
        Per-window override of ``RetrievalConfig.fit_aero_composition``,
        likewise.
    continuum_gases : tuple[str, ...]
        Gases in this window that use the 2-D (T, VMR) continuum LUT instead
        of simple linear column scaling.  Physically a property of the window,
        not the geometry: enable for TIR windows where the H2O continuum is
        strong (and where H2O/H2SO4 is a target); leave empty (linear) where
        H2O is a weak interferer (e.g. the 2600–3000 cm⁻¹ HCl region).  Must
        be a subset of the global ``CONTINUUM_GASES`` registry.  Default empty.
    plume_temp_from : str or None
        Layer only.  Label of an earlier window that fits ``gas_temp``; each
        spectrum's fitted value is passed to this window and held fixed there
        (staged plume temperature).  A receiving window never
        fits ``gas_temp`` itself.  ``None`` (default): the window fits
        ``gas_temp`` or holds the ``plume_temp`` a priori, as its flags say.
    atm_temp_from : str or None
        Layer only.  Label of an earlier window that fits ``atm_temp``; each
        spectrum's fitted value is held fixed here, as ``plume_temp_from``
        does for ``gas_temp``.  Measured 2026-11-13: the HCl window
        (2690-2830 cm⁻¹) fits the ambient temperature without following the
        plume, the CO window does not.  ``None`` (default): the window fits
        ``atm_temp`` or holds the ``atm_temp`` a priori, as its flags say.

    Notes
    -----
    A gas in both ``atm_gases`` and ``plume_gases`` is **dual** (layer only,
    experimental): it is fitted as two parameters querying two
    LUTs of the same gas, an ambient part ``{GAS}_atm`` over ``pathlength`` at
    ``atm_temp`` and a plume part ``{GAS}`` over ``plume_thickness`` at the
    plume temperature.  The bare name is the plume part, as for every plume
    gas, so targets and ratios name the plume part.  :meth:`atm_name` gives
    the parameter (and LUT key) of an atmospheric gas.  A dual gas listed in
    ``continuum_gases`` gets the 2-D table for its ambient part only; the
    plume part stays 1-D (:meth:`uses_2d`).
    """
    label:              str
    wn_start:           float
    wn_stop:            float
    atm_gases:          tuple[str, ...]
    plume_gases:        tuple[str, ...]
    plume_aero:         tuple[str, ...]
    targets:            tuple[str, ...]
    polydeg:            int
    fit_gas_temp:       bool | None = None
    fit_atm_temp:       bool | None = None
    model_gas_emission: bool | None = None
    fit_size_aero:      bool | tuple[str, ...] | None = None
    fit_aero_composition: bool | tuple[str, ...] | None = None
    continuum_gases:    tuple[str, ...] = ()
    plume_temp_from:    str | None = None
    atm_temp_from:      str | None = None

    @property
    def dual_gases(self) -> tuple[str, ...]:
        """Gases in both ``atm_gases`` and ``plume_gases``, in atm order."""
        return tuple(g for g in self.atm_gases if g in self.plume_gases)

    def atm_name(self, species: str) -> str:
        """
        Parameter name and LUT key of atmospheric gas ``species``.

        ``{species}_atm`` for a dual gas, else the bare species name.
        """
        return species + DUAL_ATM_SUFFIX if species in self.plume_gases else species

    def uses_2d(self, species: str, plume: bool) -> bool:
        """
        Whether ``species`` in the given role uses the 2-D (T, VMR) table.

        True for a gas in ``continuum_gases``, except the plume part of a dual
        gas, which is always 1-D.  Measured 2026-11-12 in the layer CO window:
        50 m of plume carries a continuum OD of 0.0003 (0.03% transmittance),
        so the 2-D table adds nothing there, while its VMR grid (top node over
        the reference VMR) would cap the plume scaling at 2.6, below the ~3.8
        that wet spectra reach.  The 1-D plume table still contains the
        continuum, at its reference VMR.

        Parameters
        ----------
        species : str
        plume : bool
            The plume role (``True``) or the atmospheric role.
        """
        if species not in self.continuum_gases:
            return False
        return not (plume and species in self.atm_gases)


#: Suffix of the ambient part of a gas fitted in both roles (14b).
DUAL_ATM_SUFFIX: str = "_atm"


# =============================================================================
# PlotConfig  (*PLOT)
# =============================================================================

# RATIO_REGRESSIONS (regression methods for gas ratios) is declared in
# config/keys.py and imported above, so objects.RATIO_REGRESSIONS still works.


@dataclass(frozen=True)
class PlotConfig:
    """
    Ratio and display settings (``*PLOT`` section, optional).

    The ratios are computed by the run state (``plumeIR.plotting.state``)
    whether or not anything is drawn, and their final slopes are logged.

    Parameters
    ----------
    ratios : tuple of (str, str)
        ``(y, x)`` species pairs; written ``HCl:SO2`` in the config, i.e. HCl
        against SO2, slope HCl/SO2.  Both must be targets of active windows.
        A pair involving an aerosol is a mass ratio [g m⁻² / g m⁻²]; a gas pair
        is a molar ratio.
    scroll : int
        Number of most recent spectra highlighted in the ratio panels, regressed
        while a run is live, and shown in the live time series (v2's
        ``scroll``).  Final figures and the logged final ratios use every
        spectrum.
    regression : str
        One of :data:`RATIO_REGRESSIONS`.
    background : dict[str, float]
        Ambient VMR [ppmv] per gas (``background_<gas>`` keys), overriding the
        typical values in ``standards.BACKGROUND_VMR``.  Shown on ratio panels
        as the column the ambient air alone would give (layer geometry only).
    rh : float or None
        Relative humidity at collection [%], for the H2O background.
    """
    ratios:     tuple[tuple[str, str], ...] = ()
    scroll:     int = default("PLOT", "scroll")
    regression: str = default("PLOT", "regression")
    background: dict[str, float] = field(default_factory=dict)
    rh:         float | None = default("PLOT", "rh")
