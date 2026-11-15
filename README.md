# plumeIR

> **Pre-release (3.0.0rc4).** This is a preview of plumeIR v3 on the
> `v3-preview` branch, published so it can be tried before release. Results
> and config keys may still change. The stable version is v2.0, on `main`
> and at tag `v2.0`.

plumeIR retrieves gas and aerosol column amounts from open-path Fourier
transform infrared (OP-FTIR) spectra, with volcanic plumes as its main
application. It fits a physical forward model to each measured spectrum:
line-by-line gas optical depths from the Reference Forward Model (RFM),
aerosol optical properties from Mie theory, and the instrument line shape
(ILS) of the spectrometer. The output is a column per species per spectrum,
with gas ratios from regression across spectra.

Version 3 is a ground-up rewrite of plumeIR v2. It separates the forward
models from the fitting engine, so geometries are interchangeable and spectra
can be fitted in parallel. It also corrects several physics faults in v2 that
changed retrieved columns ([CHANGELOG.md](CHANGELOG.md)).

## Contents

- [What v3.0 supports](#what-v30-supports)
- [Installation](#installation)
- [Quick start](#quick-start)
- [The command line](#the-command-line)
- [Config files](#config-files)
- [Output](#output)
- [Validation](#validation)
- [Citing](#citing)
- [Licence](#licence)
- [Development](#development)

## What v3.0 supports

**Measurement geometries**

| Geometry | Measurement | Forward model |
|---|---|---|
| `layer` | a plume in front of a hot source (lava, a lamp), single-beam spectra, mid-infrared windows | source × plume transmittance × ambient transmittance × polynomial, optional plume gas self-emission |
| `emission` | an upward-looking spectrometer viewing a plume against cold sky, absolute calibrated radiance (thermal infrared) | two atmospheric layers (below and above the plume) from one RFM run per water-vapour scaling, plume gases and aerosols in between |

**Supported**
- NLLS retrieval (`scipy.optimize`), fitting spectra one after another or in parallel.
- MIDAC (`.spc`) and Bruker OPUS instruments. ILS settings (maximum OPD, apodisation) are read from the files.
- Plume aerosols with Mie optical properties: sulphuric acid, volcanic ash and the other entries of mieEODG's refractive-index database.
- A command-line program with a live display of the fits, the column time series and the gas-ratio regressions. A finished run can be reopened in an inspector.

**Shipped but experimental** (solar, optimal estimation and dual gases log a warning at run time)
- Solar occultation geometry: the forward model and references are built, but there is no preprocessing and it has never been run on real solar data.
- Optimal estimation (`retrieval_method : oe`): works, but how to specify priors is still open.
- Layer geometry only: a gas fitted in both the ambient air and the plume (`{GAS}_atm` + `{GAS}`), and a plume temperature passed from one fit window to the next (`plume_temp_from`).

**Not in v3.0**
- Fitting the difference between plume and clear-sky radiance (`fit_difference`): refused at config time.
- A layer retrieval against a no-plume reference spectrum (`no_gas`): refused at config time.
- A graphical interface.
- SiF4, for which no adequate high-resolution cross-section is available.

## Installation

### Requirements

- **Python 3.11 or later.** A fresh conda or virtual environment is recommended.
- **A Fortran compiler** for mieEODG's Mie kernel: `brew install gcc` (macOS), `sudo apt install gfortran` (Debian/Ubuntu), or `conda install -c conda-forge gfortran`. Without one, mieEODG falls back to a much slower pure-Python kernel.
- **git**, to install the companion packages from GitHub.
- **RFM** and a **HITRAN** line list, which you obtain yourself (see [Setting up RFM](#setting-up-rfm)).

### Steps

Create and activate an environment, for example with conda:

```bash
conda create -n plumeir -c conda-forge python=3.12 gfortran
```

```bash
conda activate plumeir
```

Install the two companion packages, which are not on PyPI, from GitHub:

```bash
pip install "git+https://github.com/jfsmekens/RFMwrapper.git"
```

```bash
pip install "mieEODG[lut] @ git+https://github.com/jfsmekens/mieEODG.git"
```

The `[lut]` extra (xarray, netCDF4) is required: without it mieEODG cannot
save its aerosol tables, and rebuilds them on every run.

Then install plumeIR from a copy of this repository, with the Bruker reader:

```bash
pip install "./plumeIR[bruker]"
```

where `./plumeIR` is the folder holding this README. Leave out `[bruker]` if
you only use MIDAC files. To work on the code, install it in editable mode
instead: `pip install -e ".[bruker,test]"` from inside the folder.

### Updating the companion packages

plumeIR asks only for a minimum version of rfmwrapper and mieEODG, so fixes
to either reach you without a new plumeIR. To update, rerun the same command
with `--upgrade`, e.g.
`pip install --upgrade "git+https://github.com/jfsmekens/RFMwrapper.git"`.
Every run records the installed versions and their git commits in its
`summary.txt` ([Output](#output)), so a result can always be traced to the
code that made it.

### Setting up RFM

**RFM is not distributed with plumeIR, and must not be redistributed.** It is
written and maintained by Anu Dudhia (University of Oxford), who supplies it
on request: see the [RFM home page](https://eodg.atm.ox.ac.uk/RFM/) and follow
the [RFM installation guide](https://eodg.atm.ox.ac.uk/RFM/sum/rfm_installation.html).
RFM also needs a HITRAN line list, which you obtain from
[hitran.org](https://hitran.org) under HITRAN's terms and convert to RFM's
binary format with RFM's own tools.

rfmwrapper expects the standard RFM tree:

```
<rfm_dir>/
    source/rfm          the compiled executable
    atm/                atmosphere profiles (e.g. mls.atm)
    Hitran/hitran*.bin  the binary line list
```

Point rfmwrapper at it once; the setting is stored in
`~/.config/rfmwrapper/config.ini` and shared by every environment:

```bash
python -m rfmwrapper configure
```

Or set the environment variable `RFM_DIR` (and `HITRAN_PATH` if the line list
is elsewhere). `python -m rfmwrapper show` prints what rfmwrapper will use.
`plumeIR` checks this before every run. If anything is missing it stops, before
creating a run folder, and prints what it looked for and these steps.

If you use plumeIR, please cite RFM: Dudhia, A. (2017), The Reference Forward
Model (RFM), *J. Quant. Spectrosc. Radiat. Transfer* **186**, 243-253.

## Quick start

The `examples/` folder holds one config per geometry: `layer.ini` (MIDAC,
layer geometry, six windows: SO2, HCl, sulphate aerosol in the thermal
infrared, CO with CO2 and H2O, HF, and CH4 as a check on the path length; the
plume temperature fitted in the SO2 window and passed to the others, the
ambient temperature held at the measured value; `polydeg : 2` throughout, as
in v2) and `emission.ini` (Bruker,
emission geometry, SO2 with sulphate and ash aerosol). Both are explained in
[docs/CONFIG_GUIDE.md](docs/CONFIG_GUIDE.md). The spectra they were validated
on (2021-08-11 MIDAC, 2019-09-11 Bruker) are not in this repository; they are
archived on Zenodo alongside the release.
<!-- Developer: add the example-data record's DOI here at publication. -->
Set `data_dir` in each to the folder holding your spectra, then:

```bash
plumeIR examples/layer.ini --plot
```

The first run builds the references (RFM and Mie calculations), which takes
minutes to tens of minutes; they are cached in `~/.plumeIR/cache/` and later
runs load them in seconds. The `--plot` window shows each fit, the target
columns and the gas ratios as spectra are fitted, and turns into the inspector
when the run ends. Results go to `plumeIR_runs/layer_<date>-<time>/`.

To fit in parallel and save figures without opening a window:

```bash
plumeIR examples/emission.ini --parallel --save-plots
```

To reopen a finished run and step through its spectra:

```bash
plumeIR plumeIR_runs/emission_20261113-101500 --inspect
```

## The command line

```
plumeIR CONFIG [--parallel [--workers N]] [--output-dir DIR] [--recompute]
               [--plot] [--save-plots] [--redraw-interval SECONDS] [--inspect]
plumeIR RUN_FOLDER --inspect
```

`python -m plumeIR` is the same program.

| Option | Effect |
|---|---|
| `--parallel` | fit spectra in a process pool (not with `update_params`) |
| `--workers N` | pool size (default: one per CPU) |
| `--output-dir DIR` | parent of the run folder (default `./plumeIR_runs`) |
| `--recompute` | rebuild the references instead of loading them from the cache |
| `--plot` | live window: fits, column time series, gas ratios |
| `--save-plots` | write the canvas and final figures to the run folder; without `--plot`, no window opens |
| `--redraw-interval SECONDS` | redraw (and save a frame) at most this often, default 1 s; 0 redraws after every spectrum |
| `--inspect` | step through a run's fits in a window (←/→, PgUp/PgDn, Home/End, or click a point); a config is run first |

The config is checked completely before anything runs: a mistake stops the
run with a message naming the key, before a run folder is created. Ctrl-C
stops a run and keeps the spectra already fitted.

## Config files

A config is a text file of sections (`*DATA`, `*MODEL`, `*GEOMETRY`,
`*RETRIEVAL`, `*DEFAULTS`, an optional `*PLOT`, and one `*FIT0`, `*FIT1`, ...
per spectral window), each followed by `key : value` lines.

**How to write one: [docs/CONFIG_GUIDE.md](docs/CONFIG_GUIDE.md)** — what each
section is for, which values to measure and which to leave alone, how to
choose windows, gases and temperatures, two complete worked examples, and the
common mistakes. **Every key in detail:
[docs/CONFIG_REFERENCE.md](docs/CONFIG_REFERENCE.md).**

> **`polydeg` is the polynomial's degree in v3. In v2 it was the number of
> coefficients.** v2's `polydeg : 3` was a quadratic; the same line in v3 is
> a cubic. To match a v2 config, use **`polydeg : 2`**.

```
*DATA
data_dir      : ~/data/2021-08-11
filter_ext    : spc

*GEOMETRY
type          : layer
pathlength    : 1.22        # km
...

*FIT0
fit           : True
fit_window    : 2450, 2550
plume_gases   : SO2
atm_gases     : H2O, CO2
targets       : SO2
polydeg       : 2           # the degree: a quadratic, as v2's "3"

*PLOT
ratios        : HCl:SO2, CO2:SO2
regression    : odr
```

The reference is generated from the key catalogue in
`plumeIR/config/keys.py`, so it always matches the code; the guide's worked
examples are checked against the code by the test suite.

Points worth knowing:
- Paths may be relative to the config file, and may use `~` and `$VAR`. An RFM atmosphere profile can be given by name (`atm_path : mls.atm`) and is then found in `<rfm_dir>/atm`.
- The ILS's maximum OPD and apodisation are read from the spectrum files. The field of view is fitted, starting from the instrument's nominal value.
- A gas ratio regression (`*PLOT regression`) is Siegel (default), Theil-Sen, or `odr`. `odr` is weighted orthogonal-distance regression solved by York et al. (2004), *Am. J. Phys.* **72**, 367, weighted by both columns' fit errors. Its quoted error is scaled up by √MSWD when the points scatter more than those errors allow; the MSWD is in `summary.txt`.
- **Reported errors are fit errors only.** A column's error comes from the fit covariance, so it assumes the forward model is exact and the residual is white noise. It contains nothing from fixed parameters (temperatures, path length, atmosphere profile), line-list intensities or calibration, and spectrally structured residuals make it too small. Treat it as a lower bound. On the layer validation data, the HCl/SO2 points scatter about 4.6 times more than their errors predict (MSWD 21).
- An unknown key, or one the geometry does not use, is logged as a warning, not an error.

## Output

Each run writes one folder, `<output-dir>/<config name>_<YYYYmmdd-HHMMSS>/`:

| File | Content |
|---|---|
| `config.ini` | the config as run, every path resolved to absolute |
| `results_long.csv` | every parameter of every fit, with its error, one row per spectrum × window × parameter |
| `results_wide.csv` | the target columns, one row per spectrum (gases in molec cm⁻², aerosols in g m⁻²) |
| `summary.txt` | package versions and git commits, spectra loaded and fitted, the ILS, convergence and R² per window, the final gas ratios |
| `fits.npz` | every fit's measured and model spectra, for `--inspect` |
| `run.log` | the full log, including RFM and Mie progress |
| `plots/` | with `--save-plots`: the final canvas, fits, time series and ratio figures, and canvas frames |

## Validation

v3.0 has been checked on two real datasets against independent results.

| Geometry | Data | Reference | Result |
|---|---|---|---|
| layer, single beam | 418 MIDAC spectra against a lava source, 2021-08-11, windows SO2 / HCl / CO+CO2 | plumeIR v2 rerun with matched settings | SO2 v3/v2 1.028 (R 0.997). HCl, CO and CO2 differ by v2's known ILS faults (CHANGELOG) |
| emission, absolute radiance | 300 Bruker spectra, Stromboli, 2019-09-11, 750-1250 cm⁻¹, SO2 + ash + H2SO4 | the earlier emission code's fits of the same spectra | SO2 r = 0.97, v3/earlier 0.83; ash events agree in timing (r 0.85) |

All spectra converge in both datasets. The reasons for each difference from
v2 are given in the [CHANGELOG](CHANGELOG.md).

## Citing

If you use plumeIR in published work, please cite:

- plumeIR v3: the Zenodo record of the release (DOI to be added at release).
- The method papers:
  - Smekens, J.-F., Mather, T. A., Burton, M. R., La Spina, A., Kabbabe, K.,
    Esse, B., Varnam, M., & Grainger, R. G. (2023). Quantification of gas,
    ash, and sulphate aerosols in volcanic plumes from open path Fourier
    transform infrared (OP-FTIR) emission measurements at Stromboli volcano,
    Italy. *Frontiers in Earth Science*, 10, 1005738.
    [doi:10.3389/feart.2022.1005738](https://doi.org/10.3389/feart.2022.1005738)
    (emission geometry).
  - Smekens, J.-F., Mather, T. A., Burton, M. R., Varnam, M., & Pfeffer, M. A.
    (2024). Rapid primary sulfate aerosol generation observed with OP-FTIR in
    the eruptive plume of the Fagradalsfjall basaltic eruption, Iceland, 2021.
    *Journal of Geophysical Research: Atmospheres*, 129(12), e2023JD040574.
    [doi:10.1029/2023JD040574](https://doi.org/10.1029/2023JD040574)
    (layer geometry).
- FTIR_FIT, the original IDL code on which plumeIR is based: Burton, M. R.,
  Allard, P., Muré, F., & La Spina, A. (2007). Magmatic gas composition reveals
  the source depth of slug-driven Strombolian explosive activity. *Science*,
  317(5835), 227-230.
  [doi:10.1126/science.1141900](https://doi.org/10.1126/science.1141900)
- RFM: Dudhia, A. (2017), *J. Quant. Spectrosc. Radiat. Transfer* **186**, 243-253.
- HITRAN, in the edition your line list comes from (see [hitran.org](https://hitran.org)).
- The refractive-index sources of the aerosols you fit, as listed by mieEODG.

## Licence

MIT; see [LICENSE](LICENSE). RFM and HITRAN are not part of plumeIR and are
covered by their own terms. The optional Bruker reader, `brukeropusreader`,
is GPLv3. plumeIR only imports it, and neither includes nor redistributes it.

## Development

```bash
pip install -e ".[bruker,test]"
```

```bash
python -m pytest test -q
```

Tests that need RFM or the local validation datasets skip when they are
absent. After changing a config key in `plumeIR/config/keys.py`, regenerate
the reference (a test fails while it is stale):

```bash
python -m plumeIR.config.keydoc docs/CONFIG_REFERENCE.md
```

The code map is [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md).
