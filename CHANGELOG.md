# Changelog

## 3.0.0 — unreleased (preview: 3.0.0rc6)

plumeIR v3 is a rewrite of v2, not an update of it: no file is carried over.
v2 remains available at its tag (`v2.0`) in `jfsmekens/plumeIR`. This entry
lists what changes for a v2 user. Results change for four reasons: the
physics fixes below, the instrument line shape (ILS) settings now read from
the files, the forward models that replace v2's, and the default settings. The
measured size of each change is given where it is known.

### Changes that move retrieved columns

**Optional broad line-shape component** (`plumeIR/forward/ils.py`)

- New `*RETRIEVAL` keys `broad_frac`, `broad_width`, `fit_broad_frac`,
  `fit_broad_width`: a fraction of the line shape spread by a Gaussian, for
  instruments whose lines have a broader base than a field-of-view box gives.
  **Off by default; results without it are unchanged to the bit.** On a
  MIDAC with corroded mirrors it lowered the fitted `fov` to that of a
  second, sound MIDAC and raised the columns (HCl ×1.10, CO ×1.14, CO2
  ×1.07); on the sound instrument it changed nothing. Fit it once per
  instrument and hold it; do not fit it in the SO2 window.

**Maximum OPD of single-sided MIDAC files** (`plumeIR/spectra/spectrum.py`)

- The MIDAC interferograms are single-sided (`IGRAMSIDE Single` in the SPC
  log), so the maximum OPD is nearly the whole record, not half of it:
  (POINTS − (ZPD − SKIP POINTS)) / LWN = **2.058 cm**, where 1.037 cm was
  read before. Bruker files record their acquisition mode (`AQM`): the
  double-sided ones keep the halving (1.037 cm here), and a single-sided one
  gives no value, so `max_opd` must be set in the config. On the layer
  validation data the fitted `fov` comes off its lower bound and agrees
  across windows (0.050-0.052), R² improves (CO window 0.98778 → 0.99201),
  and the columns fall: SO2 ×0.981, HCl ×0.929, CO ×0.801, CO2 ×0.829.

**Instrument line shape** (`plumeIR/forward/ils.py`)

- *ILS at the window centre.* v2 passed half the fit window's **width** where
  the window's **centre** wavenumber belonged. That shrank the field-of-view
  (FOV) broadening about 40-fold, which in effect switched it off. This is the
  origin of the v2/v3 column ratios, which order by line sharpness: HCl ×1.75,
  CO2 ×1.15, CO ×1.09, SO2 ×0.99 from this fault alone.
- *FOV self-apodisation is analytic* (a sinc in the OPD domain), so the
  kernel is smooth in `fov`. v2's integer-width boxcar never changed during a
  fit, leaving `fov` as a second, redundant wavenumber shift. HCl ratio
  scatter fell from 0.276 to 0.091.
- *The kernel is centred on its grid.* v2's kernel sat half a sample off
  centre (a full sample for odd grid lengths). A free `nu_shift` absorbed it,
  so no column changed, but `nu_shift` itself moves by +0.04 cm⁻¹ at the
  default grid.
- *The kernel holds the whole FOV box*, and the OPD grid no longer depends on
  `max_opd`. Previously a fitted FOV near its upper bound lost ~50% of an HCl
  column; now ≤ 1%.
- *Exact pedestal; the truncated tail goes to the tails.* v2 estimated the
  pedestal and renormalised the truncated kernel, which deepened every line.
  Layer columns were 2.4-4.7% low from this truncation alone; they are now
  converged to ≤ 0.15%.
- *The kernel length follows the line shape, not `fov`.* It used to be
  narrower when `fov` was held than when it was fitted (±4.9 against
  ±15 cm⁻¹ for a triangle window), so holding `fov` moved layer SO2 by 1.1%
  at the same value. The core now grows until ≤ 0.25% of the line shape lies
  outside it (triangle ±38 cm⁻¹ at 1.037 cm; NB and Hann unchanged), and a
  held `fov` uses the same length as a fitted one. Layer columns with `fov`
  fitted move ≤ 0.15%; emission ≤ 0.1%.
- *The FOV shift is exact.* It was a linear interpolation of the kernel,
  which made the line shape slightly broader at fractional-sample shifts, so
  a fitted `fov` settled where the shift is a whole number of model samples
  (0.0175 rad in the layer CO window, from any start). Now a phase factor on
  the interferogram. Layer CO and CO2 no longer split between two solutions:
  their correlation with v2 rises from 0.80 to 0.99 and the CO/SO2 and
  CO2/SO2 ratio scatter nearly halves; median columns unchanged.
- *The FOV only broadens.* A finite FOV also moves lines by ν·fov²/16; that
  displacement is no longer in the kernel and is taken up by `nu_shift`, so
  `fov` measures line width alone and is no longer tied to `nu_shift`
  (correlation in the layer SO2 and HCl windows +0.91 / +0.95 before, −0.23 /
  −0.72 after). Columns are unchanged (layer ×1.00000); fitted `fov` and
  `nu_shift` values differ from earlier runs and are not comparable with them.
- *`max_opd` and `apod_type` come from the data files.* The maximum OPD is
  half of `1/RES` for a double-sided interferogram (1.037 cm for the
  validation Bruker) and nearly the whole record for a single-sided one
  (2.058 cm for the validation MIDAC; see the first entry above). v2 configs
  pinned it at 2.0 cm (Bruker) or 1.8 cm (MIDAC), and a fitted FOV absorbed
  the error (85.6 mrad against a 30 mrad nominal on the Bruker).

Net effect on the layer validation dataset (418 spectra, `atm_temp` held at
the measured 284 K), v3/v2: SO2 1.007, HCl 1.902, CO 1.691, CO2 1.465
(re-recorded 2026-11-21 with the MIDAC maximum OPD, and 2026-11-23 with the
10 K plume temperature axis). The remaining HCl, CO and CO2 factors have not been
explained.

**Emission geometry**

- *Two atmospheric layers instead of three.* v2's structure put a slice
  beside the plume and summed their emissions without either attenuating the
  other. That is wrong by a flat 14.1% of the plume signal, measured against a
  direct RFM run with a plume inserted. Below-plume and above-plume layers with
  the plume between them give exact sequential radiative transfer: 0.07-0.97%
  error, 15-189× better.
- *One RFM run per water-vapour scaling, read at the layer boundaries*
  (`*LEV`), instead of separate runs per layer. The layers now add up exactly
  to the full column, and the clear-sky radiance is consistent with them by
  construction. The per-slice build had a ~1.5% bias.
- *Layer optical depth is interpolated quadratically in the H2O scaling.*
  Radiance error falls from 3.6% to 0.15%, continuum baseline error from
  0.9% to 0.002%. This matters because aerosol sits on that baseline.
- *Layer temperature offsets are applied through an effective emission
  temperature per wavenumber*, keeping RFM's vertical structure. v2 discarded
  its RFM layer radiance in favour of a single-temperature graybody.
- *Aerosols carry extinction and absorption separately*: extinction attenuates,
  absorption sets the emission.
- *A distal O3 scaling.* On the validation data it retrieves 0.684: the
  standard profile's ozone is ~32% too high there.

**Plume temperature**

- `dt_plume` (an offset moving only the emission level) is removed. A single
  `plume_temp` sets both the absorption line strengths and the Planck term, as
  Kirchhoff's law requires. The offset was degenerate with the column: it
  pinned at its bound in 3 of 5 spectra, and SO2 swung 140× against a measured
  2.9× change.
- `plume_temp : None` (the emission default) is the ambient temperature at
  `plume_height` from the profile, e.g. 289.7 K at 1 km for `mls.atm`. The
  former fixed 350 K was ~60 K too hot. The plume pressure is likewise read
  from the profile (902 mbar at 1 km), not fixed at 1013 mbar.
- Plume gas tables have a temperature axis per geometry: 300-1000 K for layer,
  220-400 K for emission and solar. Queries outside the axis raise; they no
  longer clamp. The old clamp gave absorption at 300 K for a 289.7 K plume.
- Layer: gas self-emission (`model_gas_emission`) belongs in thermal-infrared
  windows only. Elsewhere it makes the column unconstrained (CO-SO2
  correlation 0.79 → 0.24). A continuum gas (plume H2O) now contributes to the
  self-emission; it was silently dropped before.

**Lookup tables**

- *2-D (temperature × VMR) H2O tables interpolate the cross-section OD/VMR
  linearly in VMR*, on log-spaced nodes, not the OD log-linearly. Line error
  falls from 1.7% to 0.05%, continuum error from 5% to ~0.
- *Solar H2O self-continuum scales quadratically with the column.* Plain
  linear scaling was wrong by tens of percent away from the a priori; the
  error is now ~1e-5.
- *Ambient gases are computed at the ambient pressure, over a 250-350 K axis,
  and queried at `atm_temp`.* Interpolation is linear between RFM nodes, where
  v2 used a degree-10 polynomial in temperature. Every gas table includes RFM's
  continuum.
- *Aerosol tables are computed on a 1 cm⁻¹ grid anchored to absolute
  wavenumbers*: ≤ 0.04% from a 0.2 cm⁻¹ reference, and 25-30× smaller.
- *The model grid is 25 points per cm⁻¹ (0.04 cm⁻¹).* Measured against 40, it
  is within 0.1% (layer) and 2% (emission).

**Emission calibration** (blackbody two-point, `preprocess/emission.py`)

- Same quantities as v2 (`irf`, `L_inst`; agreement 1e-15), computed with one
  guarded division instead of two.
- Hot and cold targets are identified by their temperature in the file, not
  by filename.
- Points with no calibration information become NaN; v2 invented a response.
  Radiance is not clipped at zero.
- With several blackbody samples and drift fitting off, the samples are
  averaged. v2 took the last one.

**Gas ratios**

- Orthogonal-distance regression is solved by York et al. (2004), weighted by
  both columns' fit errors. Its quoted error is scaled by √MSWD when the
  scatter exceeds those errors, and the MSWD is reported. On the layer
  validation data the slopes equal `scipy.odr`'s within 1.6e-6.
- Two species fitted in the same window have correlated errors, and the
  ratio regression now uses that correlation, spectrum by spectrum, from the
  fit's covariance (stored in `fits.npz`). Pairs from different windows are
  unchanged. On the emission validation data H2SO4_aq and ash errors
  correlate at −0.71; on the layer data the pairs in one window stay below
  0.1 and nothing moves.

**Fit errors** (`retrieval/nlls.py`)

- A parameter the spectrum does not constrain no longer removes every error
  of its fit. When an aerosol fits to zero its size has no effect, and the
  parameter covariance could not be inverted, so SO2's error was lost too: in
  230 of 300 spectra of the emission validation data. Now only that
  parameter's error is missing. Fitted values are unchanged to the bit, and so
  are errors that were already reported.

### New

- Command line `plumeIR CONFIG`, writing one folder per run (CSV results, a
  summary, all fits), and fitting in parallel. Ctrl-C keeps what is already
  fitted.
- A live display of fits, column time series and gas-ratio regressions
  (`--plot`), saved figures (`--save-plots`), and an inspector for finished
  runs (`--inspect`).
- `plumeIR analyse --exclude SPECIES ...` leaves species out of the
  composition (e.g. WATER on the layer example, which falls as the plume
  rises). The composition donut no longer fails on a negative fraction: the
  species is named under it instead of drawn.
- A composition analysis of finished runs (`plumeIR analyse RUN_FOLDER`, or
  `--analyse` at the end of a run). It gives each species' ratio against a
  reference gas three ways: the run's regression line, a pooled ratio with a
  background per window, and an all-species fit. It also gives the molar and
  mass fractions, moving windows through the run, and a test of whether the
  plume holds more than one source. Errors are block-jackknife errors.
  Species fitted in the same window carry their error correlation, from fit
  covariances that runs now store (`fits.npz` format 2). Runs without them
  are analysed with independent errors.
- Whole-spectrum preprocessing (`*PREPROCESS background`, `registration`):
  each spectrum's additive background and wavenumber shift and scale,
  measured before the fits and reported in `preprocess.csv` (the default
  `report` leaves the fits unchanged), with `--preprocess-only` and figures.
- Optimal estimation as a second retrieval backend (experimental).
- Solar occultation geometry (experimental; no preprocessing yet).
- Layer: a gas in both the ambient air and the plume, fitted as `{GAS}_atm` +
  `{GAS}`; a plume temperature passed between windows (`plume_temp_from`).
  Both are opt-in.
- Plume aerosols through mieEODG (any species in its refractive-index
  database), with the H2SO4_aq weight fraction optionally fitted. The size
  and composition fits (`fit_size_aero`, `fit_aero_composition`) take True,
  False or a list of aerosols (only those fitted), in `*RETRIEVAL` and per
  window in a `*FITn` section. `results_wide.csv` carries
  the acid and water content of each acid aerosol (`H2SO4_aq[H2SO4]_g_m2`,
  `..._molec_cm2`, with errors).
- References are cached by content in `~/.plumeIR/cache/`: a changed input
  rebuilds, an unchanged one loads in seconds. A narrower window is cut from a
  wider gas table that is already built, bit for bit, without rerunning RFM.
- Each run records the plumeIR, rfmwrapper and mieEODG versions and git
  commits.
- A start-up check for RFM that stops with instructions.
- Layer: a check on the path length, the `*PATH` section: a fit window with
  `tracer : CH4` (or `N2O`) and no targets, labelled `PATH` in the results.
  After the fits, the tracer's ambient column gives the path; the summary
  reports it, with every other window fitting the tracer as a cross-check and
  ambient mixing ratios and ratio intercepts at both paths, `results_wide.csv`
  gains `tracer_path_km`, and the log warns when it is more than 20% off
  `pathlength`. It changes no fit: no plume column depends on the path. The
  live display and the inspector show no fit panel for the path window. The
  layer example has one (2880-2990 cm⁻¹); on the example data it gives
  2.00 km against the first estimate of 1.22 km, and the example now uses
  2.0 km. `GEOMETRY.path_tracer`, its first form (rc4-rc5), still works with
  a deprecation warning.
- The layer example fits `WATER` beside `H2SO4_aq` in its thermal-infrared
  window, as v2 did (docs/CONFIG_GUIDE.md section 5.3 has its effect).
- Configs are written with values only: one header line pointing to
  `docs/CONFIG_REFERENCE.md`, then `key : value` lines. Run folders'
  `config.ini` and the examples no longer repeat each key's description; the
  reference has it. Older commented configs read as before.
- The time series is scaled to each series' range, `(y − min)/(max − min)`,
  instead of v2's `y / max`, so a series with a large background (H2O, CO2)
  is no longer squashed against the top; the legend gives both ends.

### Config changes

- **The temperature spacing of the gas tables is a setting**:
  `*MODEL plume_temp_step` (default 10 K) and `atm_temp_step` (default 5 K),
  the largest gap between temperature nodes. Interpolating between nodes errs
  as the square of the gap; at 10 K that is about 0.03% of the optical depth
  at the strongest lines and below 0.003% in a target column. A finer step
  costs table build time and disk space, not fit time. The layer plume axis
  goes from 25 nodes (29 K apart) to 71: layer SO2 ×0.999, other columns
  unchanged; emission unchanged.

- **Acid aerosols are `H2SO4_aq` and `HNO3_aq`** (were `H2SO4`, `HNO3`), with
  their parameters `r0_H2SO4_aq`, `wt_H2SO4_aq`. They are binary solutions
  of acid and water, and the old names were also gas names (`HNO3` was both).
  Old configs are read with a warning naming each rename (`plume_aero`,
  `targets`, `*DEFAULTS` fields, `sigma_prior_` keys, `*PLOT` ratios), and
  old run folders load with their results renamed; `HNO3` as a gas keeps its
  name.

- **Fit flags are `fit_<parameter>`, and plume quantities are named
  `plume…` in every geometry.** The layer plume temperature `gas_temp` is
  `plume_temp`, the name emission already used, and its self-emission
  temperature `gasE_temp` is `plumeE_temp`; the wavenumber shift `nu_shift` is
  `shift`. Renamed keys:

  | was | is |
  |---|---|
  | `fit_gas_temp` (layer), `fit_tplume` (emission) | `fit_plume_temp` |
  | `model_gas_emission` | `model_plume_emission` |
  | `fit_tprox`, `fit_tdist` | `fit_dt_prox`, `fit_dt_dist` |
  | `fit_size_aero` | `fit_r0` |
  | `fit_aero_composition` | `fit_wt` |
  | `nu_shift` | `shift` (`fit_shift` keeps its name) |

  Old configs are read with one warning listing each rename (keys in any
  section, `targets`, `sigma_prior_` keys); old run folders load with their
  parameters renamed. Results are unchanged.

- **Every key is in one catalogue**, `plumeIR/config/keys.py`, with its type,
  unit, default, limits and geometries. The reference
  (`docs/CONFIG_REFERENCE.md`) is generated from it. Values are read by type,
  and the whole config is checked before a run starts. An unknown key is a
  warning, not an error, so a v2 config can be read and its leftover keys
  are named.
- **New sections:** `*MODEL` (`nperwn`, `model_padding`, `cache_dir`),
  `*PREPROCESS` (what is done to the spectra before the fits: `subtract_self`
  and `self_file`, moved from `*GEOMETRY`, where they are still read with a
  warning; and the whole-spectrum preprocessing, `background` and
  `registration`, which measures each spectrum's additive background and
  wavenumber shift and scale before the fits, spectrum by spectrum, reports
  them by default in `preprocess.csv` with envelope metrics (peak, a
  relative broadband scale, the emission air temperature; `reference_spectra`),
  stores what its plots need in `preprocess.npz`, draws them with
  `--save-plots`, and applies them on request) and `*PLOT` (`ratios`, `scroll`, `regression`, `rh`,
  `background_<gas>`).
- **New keys:** `retrieval_method` (`nlls` | `oe`, with `sigma_prior_<param>`),
  `fit_aero_composition`, `model_aero_scattering`, `self_file`, `dt_dist` /
  `fit_tdist`, and per window `continuum`, `plume_temp_from` and
  `atm_temp_from` (a window's plume or ambient temperature held at another
  window's fit, spectrum by spectrum).
- **Removed:** `dt_plume` (see "Plume temperature"); `layer_model`,
  `sounding`, `e_file`, `use_source_e`, `force_param`, `seed_param`, `df_dir`,
  `use_residual`, `res_dir`, which nothing in v3 acts on.
- **Changed defaults:** `plume_temp` (emission) is ambient at `plume_height`
  instead of 350 K; `fit_difference` is False.
- **`polydeg` is now the polynomial's degree.** v2 built `polydeg`
  coefficients, so v2's `polydeg : 3` was a quadratic; v3 builds `polydeg + 1`,
  so the same line is a cubic. Use `polydeg : 2` in v3 to match a v2 config's
  `3`. v3 also evaluates the polynomial on a normalised [−1, 1] axis over the
  window, where v2 used absolute wavenumber. Measured on the layer validation
  data (August 2026, `fov` fixed): cubic against quadratic, SO2 ×1.000, HCl
  ×0.995, CO ×1.015 (per-spectrum r 0.89), CO2 ×0.977; R² changes by under
  5e-4.
- **Aerosol size parameters are radii** (`r0_<species>`, µm, mieEODG's
  convention), where v2 used diameters (`deff_`).
- **Refused at config time:** `fit_difference : True` (radiance difference
  fitting, parked on branch `park/difference-mode`) and `no_gas : True` (a
  no-plume reference, planned for after v3.0).
- Paths: `~` and `$VAR` are expanded, and relative paths are resolved from the
  config file's folder. An RFM profile can be named on its own
  (`atm_path : mls.atm`).

### Removed

- Radiance difference fitting, and v2's `bbtdiff`, which subtracted a radiance
  from a brightness temperature.
- SiF4: no adequate high-resolution cross-section exists.
- Excel output (CSV only).
- v2's sulphate acidity lookup (`plumeIR.reference.h2so4_acidity` in the
  previews): its table was not v2's data, and nothing used it. The
  sulphate weight fraction stays a held value (`*DEFAULTS`
  `H2SO4_aq_comp`), since the spectra do not constrain it.
- RFM inside the repository. RFM is obtained from its author (README, "Setting
  up RFM"). The v2 public history that carried it was purged on 2026-08-16.
- Dependencies: PyMieScatt (replaced by mieEODG), scikit-learn, jcamp,
  pyspectra / spc-spectra (MIDAC `.spc` files are read by plumeIR itself), and
  `scipy.odr` (removed in SciPy 1.19; replaced by the York regression).
