# plumeIR config reference

Generated from `plumeIR/config/keys.py` by `python -m plumeIR.config.keydoc`;
do not edit by hand.

A config is a text file of sections, `*DATA`, `*MODEL`, `*GEOMETRY`, the
optional `*PREPROCESS`, `*RETRIEVAL`, `*DEFAULTS`, the optional `*PLOT`, and
one `*FIT0`, `*FIT1`, ...
per fit window, each followed by `key : value` lines. `#` starts a comment
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

## `*DATA`

Point to the data directory and specify file filtering.

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `data_dir` | path | required |  |  | Directory containing the spectral data files |
| `filter_files` | bool | `False` |  |  | Filter files by name with filter_string |
| `filter_string` | str | `None` |  |  | Only keep files whose name contains this string. None = keep all |
| `filter_ext` | str | `None` |  |  | Only keep files with this extension (e.g. 0, spc). None = all extensions |
| `max_spec` | int | `None` |  | > 0 | Maximum number of spectra to process (the earliest by acquisition time). None = all |

## `*MODEL`

Forward model engine settings (RFM and mieEODG).

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `nperwn` | float | `25.0` | points per cm⁻¹ | > 0 | Model spectral sampling |
| `model_padding` | float | `50.0` | cm⁻¹ | > 0 | Padding added to each side of the fit window for ILS convolution |
| `cache_dir` | path | `None` |  |  | Directory for caching pre-computed references. None = ~/.plumeIR/cache/ |

## `*GEOMETRY`

Measurement geometry and geometry-specific forward model flags.

| Key | Type | Default | Unit | Allowed | Geometries | Description |
|---|---|---|---|---|---|---|
| `type` | str | required |  | one of layer, emission, solar | all | Measurement geometry |
| `plume_thickness` | float | required in emission; layer: `0.05`; else `None` | km | > 0 | all | Thickness of the plume layer. None = no plume (solar) |
| `plume_temp` | float | required in layer; else `None` | K | > 0 | all | Plume gas temperature (a priori when fitted). None = ambient at plume_height, from the atmosphere profile (emission, solar) |
| `pathlength` | float | required in layer | km | > 0 | layer | Total geometric path length through plume and atmosphere |
| `atm_temp` | float | required in layer | K | > 0 | layer | Ambient atmospheric temperature |
| `atm_pres` | float | required in layer | mbar | > 0 | layer | Ambient atmospheric pressure |
| `plume_pres` | float | required in layer | mbar | > 0 | layer | Plume gas pressure |
| `path_tracer` | str | `None` |  | one of CH4, N2O | layer | Well-mixed ambient gas whose fitted column checks pathlength (a diagnostic; it changes no fit). Must be an ambient-only, non-target gas of an active window. None = no path diagnostic |
| `fit_gas_temp` | bool | `False` |  |  | layer | Fit the plume gas temperature (gas_temp) |
| `fit_atm_temp` | bool | `False` |  |  | layer | Fit the atmospheric temperature (atm_temp) |
| `model_gas_emission` | bool | `False` |  |  | layer | Model thermal emission from the hot plume gas (TIR windows only) |
| `fit_source_temp` | bool | `False` |  |  | layer | Fit the background source temperature |
| `source_temp` | float | `1500.0` | K | > 0 | layer | A priori source temperature |
| `no_gas` | bool | `False` |  |  | layer | No-gas reference spectrum: not available in v3.0, must be False |
| `plume_height` | float | required in emission; else `None` | km | > 0 | emission and solar | Height of the plume centre above sea level. None = no plume (solar) |
| `elev` | float | required in emission, solar | degrees | in (0, 90] | emission and solar | Elevation angle of the line of sight above the horizon |
| `obs_height` | float | required in emission, solar | km | > 0 | emission and solar | Observer altitude above sea level |
| `atm_path` | path (must exist) | required in emission, solar |  |  | emission and solar | RFM-format atmosphere profile (a bare name is also looked for in RFM's atm directory) |
| `obs_temp` | float | `None` | K | > 0 | emission | Observer-level ambient temperature. **Not used in v3.0; planned as a prior for the proximal layer, with obs_temp and obs_rh** |
| `obs_rh` | float | `None` | % | in [0, 100] | emission | Observer-level relative humidity. **Not used in v3.0; planned as a prior for the proximal layer, with obs_temp and obs_rh** |
| `fit_tplume` | bool | `False` |  |  | emission | Fit the plume temperature (needs an explicit plume_temp) |
| `fit_tprox` | bool | `True` |  |  | emission | Fit the proximal layer temperature offset |
| `dt_prox` | float | `0.0` | K |  | emission | A priori proximal layer temperature offset |
| `fit_tdist` | bool | `True` |  |  | emission | Fit the distal layer temperature offset |
| `dt_dist` | float | `0.0` | K |  | emission | A priori distal layer temperature offset |
| `h2o_scaling` | int | `3` |  | one of 0, 1, 2, 3 | emission | Number of atmospheric layers the shared H2O_sc parameter scales (0 disables it) |
| `model_aero_scattering` | bool | `False` |  |  | emission | Add the aerosol single-scattering source term to the plume |
| `fit_difference` | bool | `False` |  |  | emission | Fit plume minus clear sky (not supported in v3.0) |
| `use_bbt` | bool | `False` |  |  | emission | Convert spectra to brightness temperature before fitting |
| `bb_drift` | bool | `True` |  |  | emission | Correct for blackbody reference drift over time |
| `clear_drift` | bool | `True` |  |  | emission | Correct for clear-sky reference drift over time |

## `*PREPROCESS`

What is done to the spectra before the fits (optional section).

| Key | Type | Default | Unit | Allowed | Geometries | Description |
|---|---|---|---|---|---|---|
| `subtract_self` | bool | layer: `False`; emission: `True` |  |  | layer and emission | Subtract the instrument self-emission spectrum before fitting |
| `self_file` | path (must exist) | `None` |  |  | layer | Self-emission spectrum. None = find '*self*' files in the data directory |
| `background` | str | `report` |  | one of off, report, apply | layer | Additive background from the opaque bands (self-emission scale and zero level): off, report (measured and written), or apply (subtracted instead of the self spectrum, unclipped) |
| `registration` | str | `report` |  | one of off, report, apply | layer and emission | Wavenumber shift and scale by cross-correlation with a model spectrum: off, report (measured and written), or apply (the wavenumber axis corrected before the fits, per spectrum; nu_shift still fitted) |
| `reference_spectra` | int | `10` |  | >= 1 | layer | Number of first spectra whose median top hull is the reference for the relative scale and temperature (taken from the start of the series, so it also works in real time) |

## `*RETRIEVAL`

Retrieval method and parameters passed to the forward model.

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `retrieval_method` | str | `nlls` |  | one of nlls, oe | Retrieval backend: nlls (non-linear least squares) or oe (optimal estimation) |
| `use_bounds` | bool | `True` |  |  | Apply parameter bounds during optimisation |
| `update_params` | bool | `False` |  |  | Seed each fit with the previous spectrum's result (serial runs only) |
| `fit_size_aero` | flags | `True` |  | one of AEROSOL, ASH, DUST, H2SO4_aq, HNO3_aq, ICE, OC, SOOT, WATER | Fit the aerosol effective radius (r0_SPECIES): True, False, or the aerosols whose size is fitted (the others held) |
| `fit_aero_composition` | flags | `False` |  | one of AEROSOL, ASH, DUST, H2SO4_aq, HNO3_aq, ICE, OC, SOOT, WATER | Fit the aerosol composition (wt_SPECIES, H2SO4_aq only): True, False, or the aerosols whose composition is fitted |
| `use_babs_aero` | bool | `True` |  |  | Use the aerosol absorption (Babs) rather than extinction (Bext) |
| `fit_fov` | bool | `True` |  |  | Fit the instrument field of view |
| `fov` | float | `0.02` | rad | in (0, 1) | A priori field of view |
| `fit_max_opd` | bool | `False` |  |  | Fit the maximum optical path difference |
| `max_opd` | float | `1.8` | cm | > 0 | Maximum optical path difference. None = read from the data files; 1.8 where they do not record it |
| `apod_type` | str | `NB medium` |  | one of Boxcar, Uniform, Triangular, Blackman-Harris, Happ-Genzel, Hamming, Lorenz, Gaussian, NB weak, NB medium, NB strong, Cosine | ILS apodization function. None = read from the data files; NB medium where they do not record it |
| `fit_shift` | bool | `True` |  |  | Fit the wavenumber shift |
| `nu_shift` | float | `0.0` | cm⁻¹ |  | A priori wavenumber shift |
| `fit_offset` | bool | `False` |  |  | Fit an additive spectral offset |
| `offset` | float | `0.0` |  |  | A priori spectral offset |

Keys named by pattern:

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `sigma_prior_<param>` | float | `None` |  | > 0 | OE prior 1-sigma of any parameter by name (e.g. sigma_prior_fov); wins over a *DEFAULTS sigma_prior |

## `*PLOT`

Gas ratios and live plotting (optional section).

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `ratios` | list | `None` |  |  | Species ratios y:x, e.g. HCl:SO2, CO2:SO2 (targets of active windows). None = no ratios |
| `scroll` | int | `200` |  | >= 3 | Number of latest spectra highlighted and regressed while a run is live |
| `regression` | str | `siegel` |  | one of siegel, theil, odr | Ratio regression: Siegel repeated medians, Theil-Sen, or odr (York et al. 2004, weighted by both columns' fit errors) |
| `rh` | float | `None` | % | in (0, 100] | Relative humidity at collection, for the H2O background on ratio panels (layer). None = no H2O background |

Keys named by pattern:

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `background_<gas>` | float | `None` | ppmv | >= 0 | Ambient VMR shown on ratio panels (layer). None = standards.BACKGROUND_VMR (CO2 420, CO 0.1, CH4 1.9 ppmv) |

## `*FITn`

Fit window configuration (one *FIT0, *FIT1, ... section per window).

Only active windows (`fit : True`) are checked beyond `fit`; "required" means required of an active window.

| Key | Type | Default | Unit | Allowed | Geometries | Description |
|---|---|---|---|---|---|---|
| `fit` | bool | `False` |  |  | all | Include this window in the analysis |
| `fit_window` | window | required | cm⁻¹ |  | all | Spectral fit window: wn_start, wn_stop |
| `atm_gases` | list | `None` |  | one of 23 names (see `plumeIR.constants`) | all | Atmospheric gases. A gas also in plume_gases is fitted twice (layer only): ambient part GAS_atm over pathlength at atm_temp, plume part GAS. None = none |
| `plume_gases` | list | `None` |  | one of 23 names (see `plumeIR.constants`) | all | Plume gases. A gas also in atm_gases keeps its name for the plume part (targets, ratios) and gets GAS_atm for the ambient part (layer only). None = none |
| `plume_aero` | list | `None` |  | one of AEROSOL, ASH, DUST, H2SO4_aq, HNO3_aq, ICE, OC, SOOT, WATER | all | Plume aerosols. None = no aerosols |
| `targets` | list | `None` |  |  | all | Species and parameters reported as primary outputs (a species in one window only). None = none |
| `polydeg` | int | `3` |  | >= 0 | all | Degree of the multiplicative background polynomial (polydeg + 1 coefficients; in v2 it was the coefficient count, so v2's 3 is v3's 2) |
| `continuum` | list | `None` |  | one of H2O | all | Gases whose continuum follows VMR on a 2-D (T, VMR) table (e.g. H2O). Every table includes RFM's continuum. For a gas in both roles, the ambient part only. None = continuum scaled linearly with the column |
| `fit_gas_temp` | bool | `None` |  |  | layer | Per-window override: fit the plume gas temperature. None = inherit from *GEOMETRY |
| `fit_atm_temp` | bool | `None` |  |  | layer | Per-window override: fit the atmospheric temperature. None = inherit from *GEOMETRY |
| `fit_size_aero` | flags | `None` |  | one of AEROSOL, ASH, DUST, H2SO4_aq, HNO3_aq, ICE, OC, SOOT, WATER | all | Per-window override: fit the effective radius (r0_SPECIES) of every aerosol in this window (True / False) or of those named. None = inherit from *RETRIEVAL |
| `fit_aero_composition` | flags | `None` |  | one of AEROSOL, ASH, DUST, H2SO4_aq, HNO3_aq, ICE, OC, SOOT, WATER | all | Per-window override: fit the composition (wt_SPECIES, H2SO4_aq only) of every aerosol in this window (True / False) or of those named. None = inherit from *RETRIEVAL |
| `model_gas_emission` | bool | `None` |  |  | layer | Per-window override: model plume gas emission. None = inherit from *GEOMETRY |
| `plume_temp_from` | str | `None` |  |  | layer | Label of an earlier window fitting gas_temp (e.g. FIT0); its per-spectrum value is held fixed here. None = the window's own gas_temp |
| `atm_temp_from` | str | `None` |  |  | layer | Label of an earlier window fitting atm_temp (e.g. FIT0); its per-spectrum value is held fixed here. None = the window's own atm_temp |

## `*DEFAULTS`

Per-species prior overrides, over the built-in tables in standards.py.

Keys are `<SPECIES>_<field>`, e.g. `SO2_conc : 5.0`, `ASH_max_size : 5`. Priority, highest first: this section, then `standards.GAS_DEFAULTS` / `AERO_DEFAULTS`, then the default below.

### Gas fields

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `conc` | float | `1.0` | ppmv | > 0 | Reference concentration: the column the scaling factor is relative to |
| `min` | float | `0.0` |  |  | Lower bound of the scaling factor (not for a 2-D continuum table) |
| `max` | float | `None` |  |  | Upper bound of the scaling factor (not for a 2-D continuum table). None = unbounded |
| `sigma_prior` | float | `None` |  | > 0 | OE prior 1-sigma of the scaling factor. None = derived from the bounds |

### Aerosol fields

| Key | Type | Default | Unit | Allowed | Description |
|---|---|---|---|---|---|
| `conc` | float | `0.001` | g m⁻³ | > 0 | Reference concentration: the mass column the scaling factor is relative to |
| `size` | float | `1.0` | µm | > 0 | A priori median radius r0 (clipped to the size bounds) |
| `sigma` | float | `None` |  | > 0 | Geometric standard deviation of the log-normal size distribution. **Not used in v3.0; kept because the repository configs set it** |
| `min_size` | float | `None` | µm | > 0 | Lower bound of r0. None = lowest LUT radius |
| `max_size` | float | `None` | µm | > 0 | Upper bound of r0. None = highest LUT radius |
| `comp` | float | `0.75` |  | in [0, 1] | A priori weight fraction (H2SO4; clipped to the LUT range) |
| `sigma_prior` | float | `None` |  | > 0 | OE prior 1-sigma of the mass scaling factor. None = derived from the bounds |
