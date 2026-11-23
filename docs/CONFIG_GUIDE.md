# Writing a plumeIR config

This guide explains how to fill in a config file for a retrieval: what each
section is for, which values to measure, which to leave at their defaults, and
the choices that change the results.

**Two documents, two jobs.** This guide is the *how and why*: advice,
measured reasons, window design, worked examples. The *what* — every key with
its type, unit, default, limits and the geometries that read it — is in
[CONFIG_REFERENCE.md](CONFIG_REFERENCE.md), generated from the code so it is
always complete and current. The tables below name only the keys that need a
decision, with advice; look a key up in the reference for its exact type and
default. The worked examples in section 9 are checked against the code by the
test suite.

> **`polydeg` is the polynomial's degree in v3, not its number of
> coefficients as in v2.** `polydeg : 2` is a quadratic (3 coefficients) and
> matches a v2 config's `polydeg : 3`. A v2 `polydeg : 3` copied unchanged
> gives a cubic. See section 5.5.

The quickest start is to copy one of the shipped examples
(`examples/layer.ini`, `examples/emission.ini`) or one from section 9, and
change it.

---

## 1. How a config file is read

A config is plain text, split into sections. A section starts with a line
beginning with `*`; every line after it, up to the next section, is
`key : value`.

```
*DATA
data_dir      : ~/data/2021-08-11      # where the spectra are
filter_ext    : spc
```

- **Sections:** `*DATA`, `*MODEL`, `*GEOMETRY`, `*RETRIEVAL`, `*DEFAULTS`,
  the optional `*PLOT`, and one `*FIT0`, `*FIT1`, ... per fit window. Windows
  are fitted in the order they appear.
- **Comments:** `#` starts a comment, but inside a value only after a space,
  so a path containing `#` is read whole.
- **Key names** are not case-sensitive. **Values** are: `filter_string : vent`
  does not match a file called `Vent...`.
- **`None`** means "not set": the default applies.
- **Types:** `True` / `False` for switches; numbers as `3`, `0.02`, `1e-3`;
  lists as `H2O, CO2` (a single item is still a list); a fit window as
  `2020, 2150`. Text keys are never read as numbers (`filter_string : 2021`
  is the text "2021").
- **Paths:** `~` and `$VAR` are expanded. A relative path is looked for next
  to the config file first, then in the folder you run from. An RFM
  atmosphere profile can be given by name alone (`atm_path : mls.atm`); it is
  then looked for in RFM's `atm` folder.

**The whole config is checked before anything runs.** Every error is listed
at once, each naming its section and key, and nothing is computed until they
are fixed. A key plumeIR does not know, or one the chosen geometry does not
use, is only a warning: the run goes ahead and the key is ignored. That is how
a config carried over from v2 can be read; the warnings name its leftover
keys.

---

## 2. The order to fill it in

1. `*GEOMETRY type`: the measurement you made decides most of what follows.
2. `*DATA`: where the spectra are and which files to use.
3. `*GEOMETRY`: the physical setup, measured at collection.
4. `*PREPROCESS`: what is done to the spectra before the fits (layer: the
   instrument's self-emission; both geometries: the whole-spectrum preprocessing, which
   by default only reports).
5. `*FITn`: one section per spectral window: its range, the gases and
   aerosols in it, and the targets.
6. `*RETRIEVAL`: usually only the instrument line shape needs a thought.
7. `*PLOT`: the gas ratios to show and report.
8. `*DEFAULTS`: only to change a species' reference amount or bounds.
9. `*MODEL`: leave as is unless you have a reason.

Then try it on a few spectra (`max_spec : 20` in `*DATA`) before the full run.

---

## 3. `*GEOMETRY`: what you measured

`type` is one of:

| type | Measurement | Spectra are fitted as | Status in v3.0 |
|---|---|---|---|
| `layer` | plume between the spectrometer and a hot source (lava, lamp) | single-beam intensity | validated |
| `emission` | spectrometer looking at the sky through the plume, no source | calibrated radiance | validated |
| `solar` | spectrometer tracking the sun through the plume | transmittance | experimental: never run on real solar data, no preprocessing |

### 3.1 Layer geometry

The model is a path of ambient air, `pathlength` long, with a hot plume of
thickness `plume_thickness` somewhere along it, in front of a hot source.

| Key | What to put | Notes |
|---|---|---|
| `pathlength` | total distance spectrometer to source [km] | the ambient gases are computed over all of it; no plume column depends on it (see below) |
| `plume_thickness` | depth of plume along the path [km] | sets the plume's optical path; default 0.05 |
| `atm_temp` | air temperature at collection [K] | **measure it**; see below |
| `atm_pres` | air pressure at collection [mbar] | from a barometer, or from altitude |
| `plume_temp` | best guess of the plume gas temperature [K] | the starting value when `plume_temp` is fitted |
| `plume_pres` | plume pressure [mbar] | normally the same as `atm_pres` |
| `source_temp` | source temperature [K] | a priori; default 1500 K |
| `fit_plume_temp` | fit the plume temperature | usually in the SO2 window only (section 5.6) |
| `fit_atm_temp` | fit the ambient temperature | **usually False**; see below |
| `fit_source_temp` | fit the source temperature | |
| `model_plume_emission` | add the hot gas's own emission | **thermal-infrared windows only** (section 5.7) |
| `no_gas` | not available in v3.0 | must be False |
| `path_tracer` | deprecated: use a `*PATH` section | see below |

**Ambient temperature: measure it and keep it fixed.** `atm_temp` is the air
temperature along the path, and a thermometer reading at collection is better
than a fitted value. Fitting it is risky: plume gases are hot, and a fit can
explain their hot lines with a warmer *atmosphere* instead. In particular,
**never fit `atm_temp` in a window that targets H2O**. The ambient
temperature and the H2O column then trade against each other (on the layer
validation data their correlation was −0.97), and the H2O/SO2 ratio can even
change sign. plumeIR warns about this combination when it checks the config.
If the air temperature must be fitted, fit it in the HCl window (2690-2830),
which measured it without following the plume; section 5.6 has the
comparison and how to pass it to the other windows.
**Path length: check it with CH4.** The distance to a lava source is often a
guess. No plume column depends on it: the gas tables give the same optical
depth per molecule at any path, so the fitted plume columns and R² stay the
same if `pathlength` is wrong (measured with 1.22 km and 2.0 km: every median
identical; rounding in the tables moves a few individual fits).
What it sets is the reading of ambient columns as mixing ratios: the
background of a fitted ambient gas, and a ratio's intercept read as a
background. A `*PATH` section checks it: a fit window like a `*FITn`, whose
only purpose is the path, with `tracer : CH4` and no targets. After the fits,
its ambient CH4 column (well mixed at the `*DEFAULTS` `CH4_conc`, absent from
volcanic plumes) gives the path. `summary.txt` reports it, with every other
window fitting CH4 beside it as a cross-check and the ambient VMRs and ratio
intercepts at both paths; `results_wide.csv` gets a `tracer_path_km` column;
the log warns when it differs from `pathlength` by more than 20%. The window
is labelled `PATH` in the results, and the live display and the inspector
show no fit panel for it. A config has at most one. The ν3 P-branch,
2880-2990 cm⁻¹, gives about 2% per spectrum (section 9.1); the HCl window's
weak CH4 lines about 5%. (The older `GEOMETRY.path_tracer : CH4` still
works, with a deprecation warning: every window fitting the gas is reported
and the one with the smallest fit error gives the path.) Set `CH4_conc` to the background of the
site and year (about 1.93 ppmv in 2021 at northern mid-latitudes; the default
is 1.9): the path scales inversely with it. Where CH4 is itself part of the
scene (a seep, a mud volcano, a fire), `N2O` in a 2540-2600 cm⁻¹ window is
the alternative: on the layer test data its series median agrees with CH4's
(1.996 against 2.003 km), but it follows the plume more, so trust the median
rather than single spectra. **Not yet usable as shipped:** with `fov` fitted,
that window usually lands in a wrong minimum (R² ≈ 0.6, `fov` far above the
other windows', smeared lines); check the window's R² before reading its
path. The tracer may not be a plume gas or a
target.
The `*GEOMETRY` flags apply to every window; a window can override them with
its own `fit_atm_temp`, `fit_plume_temp` or `model_plume_emission` (section 5.6).

### 3.2 Emission geometry

The atmosphere is computed with RFM from a profile, split at the plume into a
**proximal** layer (spectrometer to plume) and a **distal** layer (plume to
space); the plume sits between them.

| Key | What to put | Notes |
|---|---|---|
| `elev` | elevation of the line of sight [degrees above horizon] | sets the slant path |
| `obs_height` | spectrometer altitude [km above sea level] | |
| `plume_height` | height of the plume centre [km above sea level] | |
| `plume_thickness` | plume thickness [km] | |
| `atm_path` | RFM atmosphere profile, e.g. `mls.atm` | a name alone is looked for in RFM's `atm` folder |
| `plume_temp` | leave unset | None = the ambient temperature at `plume_height`, read from the profile |
| `fit_plume_temp` | leave False | a free plume temperature trades against the column; fitting it needs an explicit `plume_temp` |
| `fit_dt_prox`, `dt_prox` | proximal-layer temperature offset (fitted by default) | |
| `fit_dt_dist`, `dt_dist` | distal-layer temperature offset (fitted by default) | |
| `h2o_scaling` | how many layers the shared water scaling applies to (0-3) | default 3 |
| `model_aero_scattering` | add aerosol single scattering | default False |
| `bb_drift`, `clear_drift` | correct blackbody and clear-sky drift over the series | default True |
| `use_bbt` | fit brightness temperature instead of radiance | |
| `fit_difference` | not supported in v3.0 | must be False |
| `obs_temp`, `obs_rh` | not used in v3.0 | |

**Emission data files.** The spectra are calibrated with two blackbody
measurements and compared with a clear-sky spectrum, all found in the data
folder by name: files whose names contain `bbc` and `bbh` are the blackbody
targets (which one is hot and which cold is read from the temperature in the
file, not from the name), and files containing `clear` or `sky` are the
clear-sky reference. These files are never fitted as scene spectra.

### 3.3 `*PREPROCESS`: before the fits

Optional. What is done to the spectra after they are read and before any
window is fitted.

| Key | What to put | Notes |
|---|---|---|
| `subtract_self` | layer: subtract the instrument's own emission | needs a spectrum with "self" in its name in the data folder, or `self_file`; default False. Emission: no effect (the blackbody calibration already removes the instrument's emission) |
| `self_file` | layer: that spectrum, if it is named otherwise | |

Both keys were in `*GEOMETRY` before; a config that still has them there is
read, with a warning naming the new place.

**Whole-spectrum preprocessing** (new, 2026-11-16): before the fits, each spectrum is
measured as a whole against a synthetic spectrum the run builds once (one RFM
run each for the ambient path and a plume, over the instrument's range;
cached). Each product is `off`, `report` (the default: measured and written
to `preprocess.csv` and `summary.txt`, the fits unchanged) or `apply`.

| Key | What it does | Notes |
|---|---|---|
| `background` | layer: the additive background of each single beam, from the bands the ambient path makes black: the self-emission scale `a` (the self file is ~16% too strong on the validation set) and a zero level per band | `apply` subtracts it instead of the self spectrum, without clipping. Flags: `a` outside 0.5-1.2, or a held-out band missed by > 1% of the continuum |
| `reference_spectra` | layer: how many spectra, from the start of the series, make the reference for the relative scale | default 10. The scale `env_scale` is each spectrum's broadband level against that reference (fill fraction × emissivity × broadband attenuation such as aerosol) |
| `registration` | layer and emission: the shift and scale of the wavenumber axis, by cross-correlation in sub-bands | `apply` corrects each spectrum's axis before the fits, with that spectrum's own shift and scale; `shift` is still fitted and then holds only what is left (~0.01 cm⁻¹) |

Whenever preprocessing runs, `preprocess.csv` also describes each spectrum:
its top-hull peak, where it is and the integrated hull (`env_peak`,
`env_peak_wn`, `env_integral`); layer, the relative scale `env_scale`;
emission, `env_bt_opaque`, the brightness temperature of the opaque bands,
which is the air temperature next to the instrument in kelvin.

Measured on the validation sets: `apply` moves layer columns by ≤ 1%
(background: SO2 ×0.993, HCl ×0.992; registration: CO ×0.994) and raises the
emission R² from 0.99754 to 0.99771. The stage adds about 35 s per 418
layer spectra (5 s per 300 emission spectra) to a run. Report mode lets you
check the numbers on your own data before switching a product to `apply`;
`plumeIR CONFIG --preprocess-only` does only that (spectra, references, these
measurements, `preprocess.csv` and the summary) and fits nothing.

---

## 4. `*DATA`: which files

| Key | Use |
|---|---|
| `data_dir` | the folder holding the spectra (not searched recursively) |
| `filter_ext` | keep only this extension: `spc` for MIDAC, `0` (or the number used) for Bruker. Exact and case-sensitive |
| `filter_files`, `filter_string` | with `filter_files : True`, keep only files whose name contains `filter_string` (case-sensitive) |
| `max_spec` | fit only the earliest N spectra, by acquisition time: for trial runs |

Spectra are sorted by acquisition time (from the file for Bruker, from the
name for MIDAC). Files whose name contains `self`, `bbc`, `bbh`, `clear` or
`sky` are **reference measurements** and are always left out of the scene
spectra, whatever the filter says; they are used by the preprocessing.

**If no spectra are found,** the run stops at once and says why: the folder
does not exist or is empty, no file has the extension, the name filter
removed every file, or only reference files matched. The most common cause
is a `filter_string` copied from another dataset: the shipped layer example
keeps only files containing `vent`, which matches its own dataset's names and
nothing else. Change it to a string your scene files share, or set
`filter_files : False`.

---

## 5. `*FITn`: the fit windows

Each window is fitted independently for every spectrum, in the order the
sections appear. A window only runs with `fit : True`.

### 5.1 Range: `fit_window`

`fit_window : 2450, 2550` in cm⁻¹. Choose a range where the target absorbs
clearly and the other absorbers can be modelled. Wider is not always better:
more interfering lines, a less flat background and more parameters.
Windows used for the validated results:

| Geometry | Window [cm⁻¹] | Target | Other gases in the window |
|---|---|---|---|
| layer | 2450-2550 | SO2 | ambient H2O, N2O, CH4 |
| layer | 2690-2830 | HCl | ambient H2O, N2O, CH4 |
| layer | 2020-2150 | CO, CO2 | ambient N2O, H2O, CO2 |
| emission | 750-1250 | SO2, H2SO4_aq and ash aerosol | water and other atmospheric gases from the profile |

List a gas only in windows where it has lines. N2O has none worth fitting
in 2020-2150 or 750-1250 cm⁻¹: its fit error there exceeds its whole ambient
column, so the example leaves it out of those windows (taking it out changed
no target, every median ×1.0000).

### 5.2 Gases: `atm_gases` and `plume_gases`

These two lists say **where a gas is**, which decides its path and
temperature:

| List | Layer geometry | Emission geometry |
|---|---|---|
| `atm_gases` | ambient air: over the whole `pathlength`, at `atm_temp`, `atm_pres`; each gets its own fitted column | the gases included in the RFM atmosphere; their amounts come from the profile. Only H2O is fitted (`H2O_sc`, across `h2o_scaling` layers) and O3 in the distal layer (`O3_sc`) |
| `plume_gases` | the plume: over `plume_thickness`, at the plume temperature | the plume layer |

Include **every gas that absorbs noticeably in the window**, not only the
targets: a missing interferer ends up in the background polynomial or, worse,
in a target's column. Gases are named as in `plumeIR.constants` (e.g. `H2O`,
`CO2`, `SO2`, `HCl`, `HF`, `CO`, `N2O`, `CH4`, `O3`, `NH3`).

**A gas in both lists** (layer only, experimental) is fitted twice: an
ambient part named `{GAS}_atm` (e.g. `CO2_atm`) and a plume part keeping the
plain name (`CO2`). The two parts can only be told apart through how their
lines change with temperature, so this works only when the plume is much
hotter than the air, and is still under test. A plain gas name in targets and
ratios then means the plume part.

### 5.3 Aerosols: `plume_aero`

Aerosols in the plume, e.g. `H2SO4_aq`, `HNO3_aq`, `ASH`, `WATER`, `ICE`.
Each adds a mass column (named like the species) and an effective radius
`r0_<species>` [µm]. The acid aerosols are binary solutions of acid and
water, named `<ACID>_aq` so that no name is both a gas and an aerosol (gas
`HNO3` exists, and `H2SO4` can be a gas); their mass column is the whole
droplet, acid and water. `H2SO4_aq` also has a composition `wt_H2SO4_aq`
(acid weight fraction), held fixed unless `fit_wt` is True;
`HNO3_aq` is fixed at 50 wt% by its refractive index. The former names
`H2SO4` and `HNO3` (as an aerosol) are still read, with a warning. Aerosols are broad features: use them in wide
windows where their spectral shape is constrained (validated in the emission
geometry, 750-1250 cm⁻¹).

The layer example fits `WATER` beside `H2SO4_aq` in its 750-1250 cm⁻¹
window, as v2 did, so its results compare with v2's. On the example data
(418 spectra) the window's median R² rises from 0.9654 to 0.9779 and the
H2SO4_aq/SO2 line moves towards the origin (intercept 1.36 → 0.19 g m⁻²),
but the slope doubles (0.0063 → 0.0124) and the WATER column does not follow
the plume (r −0.51 with SO2), with its radius at the 0.1 µm lower bound in
89% of spectra: read it as a broad continuum term more than as a measured
water cloud.

### 5.4 Outputs: `targets`

The species (and parameters) reported as results of this window: the columns
in `results_wide.csv`, the time series and the ratios.

- A species can be a target in **one window only**; choose the window where
  it is best constrained.
- Besides species, `plume_temp`, `atm_temp`, `source_temp`, `dt_prox`, `fov`,
  `max_opd`, `shift` and `offset` can be targets, to report them.
- For a gas in both roles, `{GAS}_atm` (the ambient part) can be a target
  too.

Every fitted parameter is still in `results_long.csv`, target or not.

### 5.5 Background: `polydeg`

The multiplicative polynomial carries the smooth background (source
spectrum, instrument response). **`polydeg` is its degree:**

| `polydeg` | polynomial | coefficients |
|---|---|---|
| `0` | constant | 1 (`poly0`) |
| `1` | straight line | 2 |
| `2` | quadratic | 3 (`poly0`..`poly2`) |
| `3` | cubic | 4 |

**This is a change from v2.** v2's `polydeg` was the number of
coefficients, so v2's `polydeg : 3` was a *quadratic*. To reproduce a v2
setup, write **`polydeg : 2`** in v3. Copied unchanged, a v2 `3` gives a
cubic: on the layer validation data that moved SO2 by 0.02%, HCl by 0.5%, CO
by 1.5% (and its spectrum-to-spectrum values noticeably, r 0.89) and CO2 by
2.3%, for a change in R² under 5e-4. The default when the key is left out is
3 (a cubic); writing it explicitly in every window avoids any doubt.

Raise the degree only if the residual shows broad curvature; a narrow window
rarely needs more than 2. v3 also evaluates the polynomial on a normalised
axis (−1 to 1 across the window), where v2 used absolute wavenumber, so
every coefficient is well conditioned.

### 5.6 Temperatures per window

`fit_plume_temp`, `fit_atm_temp` and `model_plume_emission` can be set per
window, overriding `*GEOMETRY` (leave them out to inherit). Likewise
`fit_r0` and `fit_wt` override `*RETRIEVAL` in one
window. Both take `True`, `False`, or a list of aerosols: only those named
are fitted, the others held, e.g.

    fit_r0 : ASH          # ash's radius fitted, the sulphate's held
    fit_wt : H2SO4_aq

The emission data do not constrain the sulphate composition: hold it at a
value set from temperature and humidity rather than fitting it.

**`plume_temp_from : FIT0`** (layer only) holds this window's plume
temperature at the value fitted in another window, spectrum by spectrum. The
other window must come earlier in the file, be active, and fit `plume_temp`
itself; this window must not fit `plume_temp`. Use it when one window
constrains the plume temperature well (typically SO2) and the others should
use that value rather than fit their own. Windows that each fit their own
temperature are allowed too, and often disagree with each other.

**`atm_temp_from : FIT1`** (layer only) does the same for the ambient
temperature: this window holds `atm_temp` at the value fitted in another
window, spectrum by spectrum, with the same rules (the source comes earlier,
is active and fits `atm_temp` itself; this window does not). A window may
receive both temperatures, each from its own source.

Which window should fit `atm_temp`? Measured on the layer validation data,
fitting it in one window at a time:

| window | fitted `atm_temp` | follows SO2? | verdict |
|---|---|---|---|
| HCl, 2690-2830 | 283.6 K (1σ 2.0 K) | no (r −0.002) | **measures the air temperature**: its signal comes from ambient N2O, CH4 and H2O lines |
| SO2, 2450-2550 | stuck at the 250 K table edge (1σ 18.6 K) | — | unconstrained |
| CO, 2020-2150 | 298.8 K | yes (r +0.82) | absorbs plume heat; CO −5%, CO2 −6%, H2O ratio changes sign |

So if `atm_temp` is fitted at all, fit it in the HCl window. Two cautions:

- **Order.** A source must come earlier in the file. If the HCl window
  receives `plume_temp` from the SO2 window and also supplies `atm_temp` to the
  others, the order is: SO2 window first (fits `plume_temp`, keeps the measured
  `atm_temp`), HCl window second (receives `plume_temp`, fits `atm_temp`), then
  the rest (receive both). Two windows cannot each take a temperature
  from the other; plumeIR refuses the loop
  ("must come before"). Running the SO2 window at the measured `atm_temp`
  costs little: on the validation data, using the HCl window's fitted value
  there instead changed SO2 by 0.13% (sd between spectra, median −0.001%)
  and the plume temperature it passes on by 0.8 K (sd).
- **Noise.** Each spectrum's fitted `atm_temp` carries its fit error (about
  2 K in the HCl window), and passing it on passes that noise to the other
  windows. On the validation data most of the spread was noise (2.5 K total,
  2.0 K of it fit error), and passing it per spectrum scattered CO by 15% and
  CO2 by 6% between spectra. Where the air temperature does not change much
  during a series, **fixing `atm_temp` at a measured value, or at the HCl
  window's median from a first run, is better**. Pass it per spectrum when
  the air temperature really changes (long series, day-night). Section 9.1
  keeps the measured value in every window and fits `atm_temp` in the HCl
  window only as a check: compare its median with the value you configured.
  To pass it on instead, add `atm_temp_from : FIT1` to the later windows.

### 5.7 Hot-gas emission: `model_plume_emission`

Adds the plume gas's own thermal emission to the layer model. **Use it only in
thermal-infrared windows** (roughly below 1400 cm⁻¹). At shorter wavelengths
the plume's emission is negligible, and the extra term lets the column grow
while emission refills the line: the column stops being constrained (in the
CO window this cut the CO-SO2 correlation from 0.79 to 0.24 at no gain in fit
quality).

### 5.8 Water continuum: `continuum`

`continuum : H2O` makes the water continuum follow the water amount
correctly, which matters wherever water is a strong absorber (its
self-continuum grows with the square of the amount). Every gas table includes
the continuum either way; without this key it is scaled linearly from the
reference amount, which is fine where water is a weak interferer. For water
in both roles, it applies to the ambient part.

---

## 6. `*RETRIEVAL`: fitting and the instrument

| Key | Recommendation |
|---|---|
| `retrieval_method` | `nlls` (least squares). `oe` (optimal estimation) is for adding prior information the spectrum lacks; it needs priors (`sigma_prior_<param>`) and is slower |
| `max_opd`, `apod_type` | **leave them out**: they are read from the data files. Set them only for files that do not record them (plumeIR warns and uses 1.8 cm and NB medium then) |
| `fit_max_opd` | leave False |
| `fit_fov`, `fov` | fit it (the default), starting at the instrument's nominal field of view in radians (about 0.030 for the Bruker and 0.021 for the MIDAC used in validation). It varies for physical reasons, e.g. with aerosol load; do not fix it to steady a fit |
| `fit_shift`, `shift` | fit the wavenumber shift (default) |
| `broad_frac`, `broad_width`, `fit_broad_frac`, `fit_broad_width` | an optional broad component of the line shape: a fraction `broad_frac` of the light spread by a Gaussian of `broad_width` cm⁻¹. Leave it off (`broad_frac` 0) unless the instrument needs it: the sign is a fitted `fov` well above nominal and a residual that is the same in every gas, a core deeper and narrower than modelled with saturated cores not reaching zero (seen on an instrument with corroded mirrors). Then fit both once on a set of good spectra, in windows of isolated lines (not the SO2 band, where the width runs to its bound), and hold the median values for the runs |
| `fit_offset`, `offset` | an additive offset; usually off |
| `use_bounds` | keep True |
| `update_params` | start each fit from the previous spectrum's result; serial runs only, so not with `--parallel` |
| `fit_r0` | fit the aerosol radius (default True) |
| `fit_wt` | fit the H2SO4_aq weight fraction (default False) |
| `use_babs_aero` | has no effect in v3.0: the emission model always attenuates with aerosol extinction and emits with aerosol absorption |

---

## 7. `*PLOT`: ratios

```
*PLOT
ratios     : HCl:SO2, CO2:SO2
regression : odr
rh         : 65
```

| Key | Use |
|---|---|
| `ratios` | pairs `y:x`; both must be targets of active windows (they may be in different windows) |
| `regression` | `siegel` (robust, default), `theil` (robust), or `odr` (York et al. 2004, weighted by both columns' errors; the slope's error is scaled by the scatter) |
| `scroll` | how many of the latest spectra are highlighted and regressed during a live run |
| `rh` | relative humidity at collection [%], to draw the ambient H2O point on H2O ratio panels (layer) |
| `background_<gas>` | ambient mixing ratio [ppmv] for a gas's ambient point, e.g. `background_CO2 : 425`. CO2 (420), CO (0.1) and CH4 (1.9 ppmv) have built-in values |

The ambient point marks where a ratio line should pass with no plume: the
gas's ambient column over the path, computed from the layer geometry. For a
gas fitted in both roles the target is the plume part alone, so its ambient
point is zero. The final slopes of all ratios are written to `summary.txt`.

---

## 8. `*DEFAULTS` and `*MODEL`

**`*DEFAULTS`** changes a species' reference values, as `<SPECIES>_<field>`:

```
*DEFAULTS
SO2_conc     : 5.0       # ppmv
HCl_max      : 50
ASH_max_size : 5         # µm
```

A gas column is fitted as a **scaling factor** of a reference column: the
reference concentration `conc` (ppmv) over the gas's path (`pathlength` for
ambient gases, `plume_thickness` for plume gases). Results are reported as
columns, so `conc` changes only the starting point and the meaning of
`min` / `max`, which bound the scaling factor. Change `conc` when the plume is
far stronger or weaker than the built-in value, so the fit starts nearby.
Aerosols work the same way with a mass concentration (g m⁻³), plus the size
fields `size`, `min_size`, `max_size` and the composition `comp`.

**`*MODEL`** can usually be omitted. `nperwn` (model points per cm⁻¹,
default 25) was chosen by measurement: coarser grids bias the emission
columns by several percent, finer ones cost proportionally more time for
little gain. `model_padding` (cm⁻¹ on each side of a window) leaves room for
the line shape. `plume_temp_step` and `atm_temp_step` (K, default 10 and 5)
are the largest spacing between the temperature nodes of the plume and the
ambient gas tables. A fitted or held temperature is interpolated linearly
between nodes, and the error goes as the square of the spacing: at 10 K
about 0.03% of the optical depth at the strongest lines, well below the
fit errors of the target columns. A finer step costs table build time and
disk space, not fit time; it matters most where two parts of one gas are
separated by temperature alone (a gas in both `atm_gases` and
`plume_gases`). `cache_dir` is where computed references are kept (default
`~/.plumeIR/cache/`); they are reused whenever the inputs that define them
match, so only the first run of a setup is slow.

---

## 9. Worked examples

These are the two shipped examples, `examples/layer.ini` and
`examples/emission.ini`, in full (a test keeps them identical). Each key is
described in [CONFIG_REFERENCE.md](CONFIG_REFERENCE.md). For your own data,
change at least `data_dir` and the `*GEOMETRY` values (path, temperatures
and pressures measured at collection; heights and elevation for emission),
and `fov` to your instrument's nominal field of view.

### 9.1 Layer

MIDAC spectra of a lava source. Five fit windows, the plume temperature
fitted once in the SO2 window and passed to every other window
(`plume_temp_from`), the ambient temperature held at the measured value
everywhere and fitted in the HCl window only as a check (section 5.6),
`polydeg : 2` everywhere to match v2's quadratic background, and a `*PATH`
window whose ambient CH4 checks the path length (section 3.1).
`pathlength : 2.0` is the path that check measured on these data; the first
estimate was 1.22 km. The thermal-infrared window fits `WATER` beside
`H2SO4_aq` as v2 did (section 5.3).

```
# plumeIR config; every key is described in docs/CONFIG_REFERENCE.md

*DATA
data_dir             : ../data/2021-08-11
filter_files         : False
filter_ext           : spc

*MODEL
nperwn               : 25.0
model_padding        : 50.0
plume_temp_step      : 10.0
atm_temp_step        : 5.0

*GEOMETRY
type                 : layer
pathlength           : 2.0
atm_temp             : 284.0
atm_pres             : 973.0
plume_temp           : 450.0
plume_pres           : 973.0
plume_thickness      : 0.05
fit_plume_temp       : False
fit_atm_temp         : False
model_plume_emission : False
fit_source_temp      : False
source_temp          : 1500.0
no_gas               : False

*PREPROCESS
subtract_self        : True
self_file            : None
background           : report
reference_spectra    : 10
registration         : report

*RETRIEVAL
retrieval_method     : nlls
use_bounds           : True
update_params        : False
fit_r0               : True
use_babs_aero        : True
fit_fov              : True
fov                  : 0.021
fit_max_opd          : False
fit_shift            : True
shift                : 0.0
fit_offset           : False
offset               : 0.0

*DEFAULTS
so2_conc             : 2.0
so2_max              : 5000.0
h2so4_aq_size        : 1.0
water_size           : 1.0

*PLOT
ratios               : HCl:SO2, CO2:SO2, H2O:SO2, HF:SO2, H2SO4_aq:SO2
regression           : odr

*FIT0
fit                  : True
plume_aero           : None
polydeg              : 2
fit_window           : 2450.0, 2550.0
atm_gases            : H2O, N2O, CH4
plume_gases          : SO2
targets              : SO2, plume_temp
fit_plume_temp       : True

*FIT1
fit                  : True
plume_aero           : None
polydeg              : 2
fit_window           : 2690.0, 2830.0
atm_gases            : H2O, N2O, CH4
plume_gases          : HCl
targets              : HCl, atm_temp
plume_temp_from      : FIT0
fit_atm_temp         : True

*FIT2
fit                  : True
plume_aero           : H2SO4_aq, WATER
polydeg              : 2
fit_window           : 750.0, 1250.0
atm_gases            : H2O, CO2, CH4
plume_gases          : SO2
targets              : H2SO4_aq, WATER
plume_temp_from      : FIT0

*FIT3
fit                  : True
plume_aero           : None
polydeg              : 2
fit_window           : 2020.0, 2150.0
atm_gases            : H2O, CO2
plume_gases          : CO
targets              : CO, CO2, H2O
plume_temp_from      : FIT0

*FIT4
fit                  : True
plume_aero           : None
polydeg              : 2
fit_window           : 4020.0, 4060.0
atm_gases            : H2O
plume_gases          : HF
targets              : HF
plume_temp_from      : FIT0

*PATH
fit                  : True
plume_aero           : None
polydeg              : 2
fit_window           : 2880.0, 2990.0
atm_gases            : CH4, H2O, N2O
plume_gases          : HCl
tracer               : CH4
plume_temp_from      : FIT0
```

### 9.2 Emission

Bruker spectra of the sky, calibrated with the hot and cold blackbody
files in `data_dir`: one window, 750-1250 cm⁻¹, with SO2, sulphate and ash.
`max_spec : 300` keeps the run to the 300 spectra of its validation record;
remove it to fit every spectrum.

```
# plumeIR config; every key is described in docs/CONFIG_REFERENCE.md

*DATA
data_dir             : ../data/2019-09-11
filter_files         : True
filter_string        : plume
filter_ext           : 0
max_spec             : 300

*MODEL
nperwn               : 25.0
model_padding        : 50.0
plume_temp_step      : 10.0
atm_temp_step        : 5.0
cache_dir            : None

*GEOMETRY
type                 : emission
plume_height         : 1.0
plume_thickness      : 0.4
elev                 : 25.0
obs_height           : 0.2
atm_path             : mls.atm
plume_temp           : None
fit_plume_temp       : False
fit_dt_prox          : True
dt_prox              : 0.0
fit_dt_dist          : True
dt_dist              : 0.0
h2o_scaling          : 3
fit_difference       : False
use_bbt              : False
bb_drift             : True
clear_drift          : True
model_aero_scattering : False

*PREPROCESS
subtract_self        : False
registration         : report

*RETRIEVAL
retrieval_method     : nlls
use_bounds           : True
update_params        : False
fit_r0               : True
use_babs_aero        : True
fit_fov              : True
fov                  : 0.03
fit_max_opd          : False
fit_shift            : True
shift                : 0.0
fit_offset           : False
offset               : 0.0

*DEFAULTS
so2_conc             : 2.0
so2_max              : 5000.0
h2so4_aq_size        : 1.0
ash_size             : 1.0

*PLOT
ratios               : H2SO4_aq:SO2, ASH:SO2

*FIT0
fit                  : True
fit_window           : 750.0, 1250.0
atm_gases            : H2O, CO2, N2O, CH4, O3
plume_gases          : SO2
plume_aero           : H2SO4_aq, ASH
targets              : SO2, H2SO4_aq, ASH
polydeg              : 3
continuum            : H2O
```

---

## 10. Before the full run

1. **Check the config** by starting a run: errors are reported before any
   computation, and warnings name ignored keys and risky combinations.
2. **Trial on a few spectra** with `max_spec : 20`. The first run builds the
   references (minutes to tens of minutes); later runs reuse them.
3. **Look at the fits**: `plumeIR RUN_FOLDER --inspect` steps through every
   spectrum's fit and residual. A residual with structure at known line
   positions means a missing gas or a wrong temperature; broad curvature
   means the polynomial degree is too low.
4. **Read `summary.txt`**: the fraction of converged fits and the median R²
   per window, and the ratio slopes.

## 11. Common mistakes

| Symptom | Likely cause |
|---|---|
| "No spectra found" | `filter_string` or `filter_ext` does not match your file names (both are case-sensitive); the message names the key |
| A key is reported as ignored | a v2 key v3 does not use, a typo, or a key the geometry does not read |
| "is a target in multiple windows" | the same species listed under `targets` in two windows |
| A ratio is refused | one of its two species is not a target of an active window |
| H2O/SO2 negative or implausible | `atm_temp` fitted in the window that targets H2O (section 3.1) |
| A column that does not follow the others, in a mid-infrared window | `model_plume_emission` switched on outside the thermal infrared (section 5.7) |
| Columns different from v2 | expected: v3 corrects v2's instrument line shape and other faults. On the layer validation data SO2 agrees within 1%, while HCl, CO and CO2 are 1.5-1.9 times v2's; the CHANGELOG gives each change and its size |
| v2 config gives a different background | `polydeg` is now the degree: write v2's `3` as `2` (section 5.5) |
| `fov` runs to a bound | `max_opd` set by hand to a wrong value; leave it out so it is read from the files |
