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
4. `*FITn`: one section per spectral window: its range, the gases and
   aerosols in it, and the targets.
5. `*RETRIEVAL`: usually only the instrument line shape needs a thought.
6. `*PLOT`: the gas ratios to show and report.
7. `*DEFAULTS`: only to change a species' reference amount or bounds.
8. `*MODEL`: leave as is unless you have a reason.

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
| `pathlength` | total distance spectrometer to source [km] | the ambient gases are computed over all of it |
| `plume_thickness` | depth of plume along the path [km] | sets the plume's optical path; default 0.05 |
| `atm_temp` | air temperature at collection [K] | **measure it**; see below |
| `atm_pres` | air pressure at collection [mbar] | from a barometer, or from altitude |
| `plume_temp` | best guess of the plume gas temperature [K] | the starting value when `gas_temp` is fitted |
| `plume_pres` | plume pressure [mbar] | normally the same as `atm_pres` |
| `source_temp` | source temperature [K] | a priori; default 1500 K |
| `fit_gas_temp` | fit the plume temperature | usually in the SO2 window only (section 5.6) |
| `fit_atm_temp` | fit the ambient temperature | **usually False**; see below |
| `fit_source_temp` | fit the source temperature | |
| `model_gas_emission` | add the hot gas's own emission | **thermal-infrared windows only** (section 5.7) |
| `subtract_self` | subtract the instrument's own emission | needs a spectrum with "self" in its name in the data folder, or `self_file` |
| `self_file` | that spectrum, if it is named otherwise | |
| `no_gas` | not available in v3.0 | must be False |

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
The `*GEOMETRY` flags apply to every window; a window can override them with
its own `fit_atm_temp`, `fit_gas_temp` or `model_gas_emission` (section 5.6).

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
| `fit_tplume` | leave False | a free plume temperature trades against the column; fitting it needs an explicit `plume_temp` |
| `fit_tprox`, `dt_prox` | proximal-layer temperature offset (fitted by default) | |
| `fit_tdist`, `dt_dist` | distal-layer temperature offset (fitted by default) | |
| `h2o_scaling` | how many layers the shared water scaling applies to (0-3) | default 3 |
| `model_aero_scattering` | add aerosol single scattering | default False |
| `bb_drift`, `clear_drift` | correct blackbody and clear-sky drift over the series | default True |
| `use_bbt` | fit brightness temperature instead of radiance | |
| `fit_difference` | not supported in v3.0 | must be False |
| `subtract_self` | has no effect in emission | the blackbody calibration already removes the instrument's emission |
| `obs_temp`, `obs_rh` | not used in v3.0 | |

**Emission data files.** The spectra are calibrated with two blackbody
measurements and compared with a clear-sky spectrum, all found in the data
folder by name: files whose names contain `bbc` and `bbh` are the blackbody
targets (which one is hot and which cold is read from the temperature in the
file, not from the name), and files containing `clear` or `sky` are the
clear-sky reference. These files are never fitted as scene spectra.

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
| emission | 750-1250 | SO2, H2SO4 and ash aerosol | water and other atmospheric gases from the profile |

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

Aerosols in the plume, e.g. `H2SO4`, `ASH`, `WATER`, `ICE`. Each adds a mass
column (named like the species) and an effective radius `r0_<species>` [µm];
H2SO4 also has a composition `wt_H2SO4` (weight fraction), held fixed unless
`fit_aero_composition` is True. Aerosols are broad features: use them in wide
windows where their spectral shape is constrained (validated in the emission
geometry, 750-1250 cm⁻¹).

### 5.4 Outputs: `targets`

The species (and parameters) reported as results of this window: the columns
in `results_wide.csv`, the time series and the ratios.

- A species can be a target in **one window only**; choose the window where
  it is best constrained.
- Besides species, `gas_temp`, `atm_temp`, `source_temp`, `dt_prox`, `fov`,
  `max_opd`, `nu_shift` and `offset` can be targets, to report them.
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

`fit_gas_temp`, `fit_atm_temp` and `model_gas_emission` can be set per
window, overriding `*GEOMETRY` (leave them out to inherit).

**`plume_temp_from : FIT0`** (layer only) holds this window's plume
temperature at the value fitted in another window, spectrum by spectrum. The
other window must come earlier in the file, be active, and fit `gas_temp`
itself; this window must not fit `gas_temp`. Use it when one window
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
  receives `gas_temp` from the SO2 window and also supplies `atm_temp` to the
  others, the order is: SO2 window first (fits `gas_temp`, keeps the measured
  `atm_temp`), HCl window second (receives `gas_temp`, fits `atm_temp`), then
  the rest (receive both). Section 9.1 is set up this way. Two windows
  cannot each take a temperature from the other; plumeIR refuses the loop
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
  the air temperature really changes (long series, day-night).

### 5.7 Hot-gas emission: `model_gas_emission`

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
| `fit_shift`, `nu_shift` | fit the wavenumber shift (default) |
| `fit_offset`, `offset` | an additive offset; usually off |
| `use_bounds` | keep True |
| `update_params` | start each fit from the previous spectrum's result; serial runs only, so not with `--parallel` |
| `fit_size_aero` | fit the aerosol radius (default True) |
| `fit_aero_composition` | fit the H2SO4 weight fraction (default False) |
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
the line shape. `cache_dir` is where computed references are kept (default
`~/.plumeIR/cache/`); they are reused whenever the inputs that define them
match, so only the first run of a setup is slow.

---

## 9. Worked examples

These are complete configs with illustrative values; replace every value
marked in a comment with your own.

### 9.1 Layer

Five windows, the plume temperature fitted once and passed to every other
window, the ambient temperature fitted in the HCl window and passed on (see
the cautions in section 5.6), and `polydeg : 2` everywhere to match v2's
quadratic background.

```
*DATA
data_dir      : ~/data/my_campaign       # your folder of spectra
filter_ext    : spc                      # MIDAC files
filter_files  : False                    # use every scene file in the folder

*GEOMETRY
type          : layer
pathlength    : 1.22                     # km, spectrometer to source
plume_thickness : 0.05                   # km
atm_temp      : 284                      # K, measured at collection
atm_pres      : 973                      # mbar, measured at collection
plume_temp    : 450                      # K, starting value
plume_pres    : 973                      # mbar
subtract_self : True                     # needs a *self* spectrum in data_dir

*RETRIEVAL
retrieval_method : nlls
fov           : 0.021                    # rad, instrument nominal

*FIT0
fit           : True
fit_window    : 2450, 2550
atm_gases     : H2O, N2O, CH4
plume_gases   : SO2
targets       : SO2, gas_temp
polydeg       : 2                        # quadratic (v2's "3")
fit_gas_temp  : True                     # plume temperature fitted here only

*FIT1
fit           : True
fit_window    : 2690, 2830
atm_gases     : H2O, N2O, CH4
plume_gases   : HCl
targets       : HCl, atm_temp
polydeg       : 2
plume_temp_from : FIT0                   # plume temperature from FIT0
fit_atm_temp  : True                     # ambient temperature fitted here only

*FIT2
fit           : True
fit_window    : 750, 1250
atm_gases     : H2O, CO2, N2O, CH4
plume_gases   : SO2
plume_aero    : H2SO4
targets       : H2SO4
polydeg       : 2
plume_temp_from : FIT0
atm_temp_from : FIT1

*FIT3
fit           : True
fit_window    : 2020, 2150
atm_gases     : N2O, H2O, CO2
plume_gases   : CO
targets       : CO, CO2, H2O
polydeg       : 2
plume_temp_from : FIT0
atm_temp_from : FIT1                     # never fit atm_temp where H2O is a target

*FIT4
fit           : True
fit_window    : 4020, 4060
atm_gases     : H2O
plume_gases   : HF
targets       : HF
polydeg       : 2
plume_temp_from : FIT0
atm_temp_from : FIT1

*PLOT
ratios        : HCl:SO2, CO2:SO2, H2O:SO2, HF:SO2, H2SO4:SO2
regression    : odr
rh            : 65                       # % at collection, for the H2O background
```

### 9.2 Emission

```
*DATA
data_dir      : ~/data/my_sky_series     # spectra plus BBC / BBH / clear files
filter_ext    : 0                        # Bruker files ending in .0

*GEOMETRY
type          : emission
elev          : 25                       # degrees above the horizon
obs_height    : 0.4                      # km, your spectrometer's altitude
plume_height  : 1.0                      # km
plume_thickness : 0.4                    # km
atm_path      : mls.atm                  # RFM profile

*RETRIEVAL
retrieval_method : nlls
fov           : 0.030                    # rad, instrument nominal

*FIT0
fit           : True
fit_window    : 750, 1250
atm_gases     : H2O, CO2, O3
plume_gases   : SO2
plume_aero    : H2SO4, ASH
targets       : SO2, H2SO4, ASH

*PLOT
ratios        : H2SO4:SO2, ASH:SO2
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
| A column that does not follow the others, in a mid-infrared window | `model_gas_emission` switched on outside the thermal infrared (section 5.7) |
| Columns different from v2 | expected: v3 corrects v2's instrument line shape and other faults. On the layer validation data SO2 is 3% higher, HCl, CO and CO2 by much more; the CHANGELOG gives each change and its size |
| v2 config gives a different background | `polydeg` is now the degree: write v2's `3` as `2` (section 5.5) |
| `fov` runs to a bound | `max_opd` set by hand to a wrong value; leave it out so it is read from the files |
