# Changelog

## 3.0.0 — unreleased (preview: 3.0.0rc3)

plumeIR v3 is a rewrite of v2, not an update of it: no file is carried over.
v2 remains available at its tag (`v2.0`) in `jfsmekens/plumeIR`. This entry
lists what changes for a v2 user. Results change for four reasons: the
physics fixes below, the instrument line shape (ILS) settings now read from
the files, the forward models that replace v2's, and the default settings. The
measured size of each change is given where it is known.

### Changes that move retrieved columns

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
- *`max_opd` and `apod_type` come from the data files.* The maximum OPD is
  half of `1/RES` (1.037 cm for both validation instruments). v2 configs
  pinned it at 2.0 cm (Bruker) or 1.8 cm (MIDAC), and a fitted FOV absorbed
  the error (85.6 mrad against a 30 mrad nominal). Correcting it moves HCl by
  +12%, layer SO2 by +1.6% and emission SO2 by −2.6%.

Net effect on the layer validation dataset (418 spectra), v3/v2:
SO2 1.026, HCl 2.046, CO 1.619, CO2 1.481. The remaining HCl factor has not
been explained.

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

### New

- Command line `plumeIR CONFIG`, writing one folder per run (CSV results, a
  summary, all fits), and fitting in parallel. Ctrl-C keeps what is already
  fitted.
- A live display of fits, column time series and gas-ratio regressions
  (`--plot`), saved figures (`--save-plots`), and an inspector for finished
  runs (`--inspect`).
- Optimal estimation as a second retrieval backend (experimental).
- Solar occultation geometry (experimental; no preprocessing yet).
- Layer: a gas in both the ambient air and the plume, fitted as `{GAS}_atm` +
  `{GAS}`; a plume temperature passed between windows (`plume_temp_from`).
  Both are opt-in.
- Plume aerosols through mieEODG (any species in its refractive-index
  database), with the H2SO4 weight fraction optionally fitted.
- References are cached by content in `~/.plumeIR/cache/`: a changed input
  rebuilds, an unchanged one loads in seconds. A narrower window is cut from a
  wider gas table that is already built, bit for bit, without rerunning RFM.
- Each run records the plumeIR, rfmwrapper and mieEODG versions and git
  commits.
- A start-up check for RFM that stops with instructions.

### Config changes

- **Every key is in one catalogue**, `plumeIR/config/keys.py`, with its type,
  unit, default, limits and geometries. The reference
  (`docs/CONFIG_REFERENCE.md`) is generated from it. Values are read by type,
  and the whole config is checked before a run starts. An unknown key is a
  warning, not an error, so a v2 config can be read and its leftover keys
  are named.
- **New sections:** `*MODEL` (`nperwn`, `model_padding`, `cache_dir`) and
  `*PLOT` (`ratios`, `scroll`, `regression`, `rh`, `background_<gas>`).
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
- RFM inside the repository. RFM is obtained from its author (README, "Setting
  up RFM"). The v2 public history that carried it was purged on 2026-08-16.
- Dependencies: PyMieScatt (replaced by mieEODG), scikit-learn, jcamp,
  pyspectra / spc-spectra (MIDAC `.spc` files are read by plumeIR itself), and
  `scipy.odr` (removed in SciPy 1.19; replaced by the York regression).
