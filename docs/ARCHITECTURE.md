# plumeIR v3 — Code map

The map of the package as it is: what each part does, how a run calls it, and
where to look for a given thing. It describes the current code; what changed
from v2 is in `CHANGELOG.md`. Section 5 lists every module, and a test
(`test/test_architecture.py`) fails when a module is missing from it.

---

## 1. The pipeline in one line

```
config ─▶ spectra ─▶ preprocess ─▶ reference ─▶ forward model ─▶ fit ─▶ results ─▶ run folder ─▶ analysis
```

Every run is this sequence, and each step is one subpackage.
`plumeIR/pipeline.py` holds the first seven steps in three short functions:
`prepare_run` (steps 1-4, and the whole-spectrum preprocessing), `stream_run`
(steps 5-6, yielding each spectrum's fits as they finish) and `collect_run`
(step 7); `run_from_config` chains them. Read it first. The command line
(`plumeIR/__main__.py`) is the same three calls, then the run folder
(`plumeIR/output.py`) and, on request, the composition analysis
(`plumeIR/analysis/`) and the inspector.

| Step | Subpackage | Entry point | Produces |
|---|---|---|---|
| 1. Read and check the config | `config/` | `build_from_config(path)` | typed config objects, one `ParameterSet` per active window |
| 2. Find and read spectra | `spectra/` | `load_spectra(data_config)` | `Spectrum` objects, sorted by time |
| 3. Prepare spectra | `preprocess/` | `preprocess_spectra(spectra, geometry, data, preprocess)` | self-subtracted (layer) or calibrated (emission) `Spectrum` |
| 4. Pre-compute cross-sections | `reference/` | `build_reference_for(geometry, window, ...)` | one `Reference` per window (cached on disk) |
| 5. Model a spectrum | `forward/` | `build_forward_model(reference, params, ...)` | a `ForwardModel`: parameter vector → model spectrum |
| 6. Fit | `retrieval/` | `iter_retrieval(spectra, references, ...)` | a stream of `SpectrumFits`: one `FitResult` per window, per spectrum |
| 7. Collect | `retrieval/run.py` | `collect_results(stream, ...)` | the run's `ResultsTable`: columns in molec/cm², ppm·m, g/m² |
| 8. Write | `output.py` | `write_results`, `write_fits`, `write_summary` | the run folder |
| 9. Analyse | `analysis/` | `run_analysis(run_dirs, out_dir)` | gas ratios and composition of a finished run |

Steps 1-4 happen once per run; steps 5-6 once per spectrum and window. Step 4
and the whole-spectrum preprocessing at the end of `prepare_run`
(`preprocess/products.py`, with its synthetic template) are the only steps
that call RFM (through `rfmwrapper`) or Mie code (through `mieEODG`), and the
only slow ones on a first run. Step 9 reads the run folder and never refits.

---

## 2. How a run is called

The call trees below follow each entry point down to the modules it uses.
Indentation is "calls"; `module::function` names the code.

### 2.1 `plumeIR CONFIG [--plot] [--save-plots] [--analyse] [--inspect]`

```
__main__::main                       arguments; console log handler
└─ __main__::run
   ├─ config/parser::read_config      the file → raw dict
   ├─ config/paths::resolve_config_paths
   ├─ config/builder::build_from_raw  (§2.2)        errors here leave no run folder
   ├─ rfm_setup::require_rfm          stops with the steps to get RFM
   ├─ output::make_run_dir            <output-dir>/<config>_<time>/, run.log handler
   ├─ output::write_resolved_config   config.ini (values only, paths resolved)
   ├─ pipeline::prepare_run           (§2.3)  → RunSetup
   ├─ plotting/state::RunState.for_run
   │  __main__::_Plots                plotting/canvas::AnalysisCanvas, with --plot / --save-plots
   ├─ pipeline::stream_run            (§2.4)  for each SpectrumFits:
   │     RunState.add(item); _Plots.update()   redraw at most every --redraw-interval
   ├─ pipeline::collect_run           retrieval/run::collect_results → ResultsTable.from_fits
   ├─ path_tracer::tracer_report      the *PATH window's implied path (layer)
   ├─ output::write_results           results_long.csv, results_wide.csv
   ├─ output::write_preprocess        preprocess.csv, preprocess.npz (preprocess/archive)
   ├─ output::write_fits              fits.npz: every fit's spectra and covariance
   ├─ output::run_summary, write_summary          summary.txt (provenance::package_versions)
   ├─ _Plots.finish                   plotting/canvas::save_final_figures, with --save-plots
   ├─ __main__::analyse               with --analyse (§2.6)
   └─ __main__::inspect               with --inspect, or when a --plot window is still open (§2.5)
```

A run stopped with Ctrl-C keeps what finished: the stream is closed, the
tables and `fits.npz` are written, the analysis is skipped, exit status 130.

`--preprocess-only` stops after `prepare_run`: it writes `preprocess.csv`,
`preprocess.npz` and a summary (`output::preprocess_summary`), and with
`--save-plots` the preprocessing figures (`plotting/preprocess.py`); nothing
is fitted.

### 2.2 The config: `config/builder::build_from_raw`

```
config/parser::normalise             typed values; migrate_moved_keys, migrate_species_names
config/paths::resolve_config_paths   ~, $VAR, relative to the config, RFM's atm folder
config/validator::validate           per-key checks from config/keys::KEYS, then cross-key rules
build_data_config, build_model, build_geometry, build_retrieval, build_preprocess
                                     → config/objects (frozen dataclasses)
for each active *FITn / *PATH:
   build_fit_window                  → FitWindowConfig
   build_parameter_set               → parameters/parameter::ParameterSet
                                       (defaults from standards, *DEFAULTS, ILS and temperatures)
```

### 2.3 Before the fits: `pipeline::prepare_run`

```
pipeline::warn_experimental
spectra/discovery::load_spectra      discover_spectra → spectra/spectrum::read_spectrum
                                     (read_midac via spectra/spc, read_bruker); sorted by time
                                     none found → explain_no_spectra names the key
preprocess::preprocess_spectra
   layer:    preprocess/layer::preprocess_layer        self spectrum subtracted, clipped
   emission: preprocess/emission::preprocess_emission  blackbody calibration to radiance
                                                       (preprocess/drift for slow drift)
spectra/spectrum::resolve_instrument_ils               max_opd and apodisation from the files
for each window: reference::build_reference_for
   layer:    reference/reference::build_reference
   emission: reference/emission_reference::build_emission_reference
                                     + AtmLayerLUT (reference/atm_lut), one RFM *LEV run per H2O node
   solar:    reference/solar_reference::build_solar_reference
      gas tables:     reference/gas_lut::GasLUT.load_or_compute       RFM, cached
      aerosol tables: reference/aero_lut::AerosolLUT.build_1d / build_h2so4_2d   mieEODG, cached
preprocess/products::preprocess_products       the whole-spectrum preprocessing (layer and emission)
   preprocess/template::build_template         synthetic ambient + plume spectrum (RFM, cached)
   preprocess/background::measure_background   additive background from the opaque bands (layer)
   preprocess/registration::fit_registration   wavenumber shift and scale by cross-correlation
                                               (layer: with the template; emission: with the
                                               reference's clear-sky model)
   preprocess/envelope::envelope               hull peak, relative scale
   report: measured and written; apply: the spectra the windows fit are corrected
→ RunSetup
```

### 2.4 One spectrum's fits: `retrieval/run::iter_retrieval`

Serial, or a spawn process pool across spectra (`--parallel`); either way
each spectrum goes through `_fit_spectrum`, windows in config order:

```
retrieval/run::_fit_spectrum
for each window:
   _staged_temps                      plume_temp / atm_temp from an earlier window (plume_temp_from, atm_temp_from)
   Spectrum.trim(wn_start, wn_stop)
   _seed_polynomial                   poly0 from this spectrum's level
   forward/factory::build_forward_model
      → forward/layer::LayerForwardModel | forward/emission::EmissionForwardModel | forward/solar::SolarForwardModel
   backend.fit(forward, params, y)    retrieval/nlls::NonLinearLeastSquares (default) or retrieval/oe::OptimalEstimation
      each call of the forward model:
         model_highres(p)             optical depths: GasLUT.query, AerosolLUT.query (forward/base::gas_optical_depth),
                                      the background polynomial (forward/base::build_background_poly)
         forward/ils::make_ils        the line shape at this fov, max_opd
         forward/base::apply_ils      convolution, then apply_shift_offset onto the measurement grid
   → retrieval/results::FitResult    values, errors, covariance, measured and model spectra, R²
→ SpectrumFits (one per spectrum; index = input order)
```

### 2.5 `plumeIR RUN_FOLDER --inspect`

```
__main__::inspect
└─ plotting/inspector::Inspector.from_run_dir
   └─ plotting/state::RunState.from_run_dir
      └─ output::load_run              config.ini (rebuilt), results_long.csv, fits.npz (output::read_fits)
   AnalysisCanvas in its whole-run form; ←/→, PgUp/PgDn, Home/End, clicks → RunState.select → redraw
```

Nothing is refitted and no reference is built. A folder without `fits.npz`
(written before 2026-11-12) cannot be inspected.

### 2.6 `plumeIR RUN_FOLDER --analyse`, `plumeIR analyse RUN_FOLDER [...]`

```
__main__::analyse  (or analysis/cli::main for "plumeIR analyse", with options)
└─ analysis/cli::run_analysis(run_dirs, out_dir)
   ├─ analysis/report::analyse_runs
   │  ├─ output::load_run(require_fits=False)        fits.npz optional: without it, errors independent
   │  ├─ analysis/data::composition_table             columns, backgrounds by gas class, error correlation
   │  └─ analysis/report::analyse_table
   │     ├─ analysis/components::composition, moving_composition, rank_table   (analysis/mlpca::fit_components)
   │     └─ analysis/ratios::run_ratios, pooled_ratio, moving_ratio, window_spread   (regression::york_fit_grouped)
   ├─ analysis/report::write_analysis               ratios.csv, moving_composition.csv, rank.csv, analysis.txt
   └─ plotting/analysis_report::save_composition_figures   (plotting/components, plotting/analysis)
```

At the end of a run, `--analyse` writes into `RUN_FOLDER/analysis/` with the
default settings; `plumeIR analyse` takes the options and several runs.

### 2.7 From Python

```python
from plumeIR.pipeline import prepare_run, stream_run, collect_run, run_from_config

table = run_from_config("my.ini")                 # the whole run, no run folder

setup = prepare_run("my.ini")                     # or step by step, as the CLI does
items = list(stream_run(setup, parallel=True))
table = collect_run(setup, items)
```

`prepare_run` also takes the dict from `build_from_config` / `build_from_raw`,
so a client can build a config in memory. The run folder is written only by
the functions of `output.py`, which any client may call.

---

## 3. Rows are steps, columns are geometries

The three measurement geometries share the pipeline and differ in three steps.
To work on one geometry, read its column:

| Step | Layer (transmission) | Emission (sky) | Solar (occultation) |
|---|---|---|---|
| preprocess | `preprocess/layer.py` | `preprocess/emission.py` | none yet |
| whole-spectrum preprocessing | template, background, registration, envelope | registration (against the clear-sky model), envelope | none |
| reference | `reference/reference.py` | `reference/emission_reference.py`, `reference/atm_lut.py` | `reference/solar_reference.py` |
| forward | `forward/layer.py` | `forward/emission.py` | `forward/solar.py` |
| path check (`*PATH`) | `path_tracer.py` | not defined | not defined |
| status | **supported** | **supported** (absolute radiance) | experimental |

Everything else is shared by all three.

---

## 4. The objects that carry a run

| Object | Defined in | Made by | What it is |
|---|---|---|---|
| `DataConfig`, `ModelConfig`, `LayerGeometry` / `EmissionGeometry` / `SolarGeometry`, `PreprocessConfig`, `Retrieval`, `FitWindowConfig`, `PlotConfig` | `config/objects.py` | `config/builder.py` | the config, typed and validated. Frozen; no file I/O. A `FitWindowConfig` with `is_path` is the `*PATH` window |
| `Key` | `config/keys.py` | `KEYS` | one config key: type, unit, default, limits, geometries, help |
| `Parameter`, `ParameterSet` | `parameters/parameter.py` | `config/builder.py` | what is fitted: value, bounds, free or fixed, reference column. Immutable |
| `Species` | `species.py` | the registry | one gas or aerosol: formula, molar mass, phase; for an aerosol its type, refractive index source, components |
| `Spectrum` | `spectra/spectrum.py` | `spectra/` readers | one measurement: wavenumber, values, time, file, type, instrument line-shape fields |
| `Reference` (holding `GasLUT`, `AerosolLUT`, `AtmLayerLUT`) | `reference/` | `build_reference_for` | pre-computed optical depths and radiances for one window. Immutable, cached |
| `RunSetup` | `pipeline.py` | `prepare_run` | everything before the first fit: config objects, references, preprocessed spectra, the preprocessing report |
| `PreprocessReport` | `preprocess/products.py` | `preprocess_products` | the whole-spectrum preprocessing: modes, per-spectrum table, notes |
| `ForwardModel` (`LayerForwardModel`, `EmissionForwardModel`, `SolarForwardModel`) | `forward/` | `build_forward_model` | a callable: parameter vector → model spectrum. Knows nothing about fitting |
| `ILSKernel` | `forward/ils.py` | `make_ils` | the instrument line shape |
| `FitResult` | `retrieval/results.py` | the backends (`nlls.py`, `oe.py`) | one fit: values, errors, covariance, model and measured spectra, R² |
| `SpectrumFits` | `retrieval/run.py` | `iter_retrieval` | one item of a run's stream: a spectrum, its index and its fits, one per window |
| `ResultsTable` | `retrieval/results.py` | `ResultsTable.from_fits` | all fits of a run as one pandas table, with unit conversions |
| `PathReport` | `path_tracer.py` | `tracer_report` | the path check: implied path per window, ambient VMRs at both paths |
| `RunState` | `plotting/state.py` | `RunState.for_run` + `add`, `from_table`, `from_run_dir` | what a display shows: latest fit per window, target columns, ratio regressions, the selected spectrum. No matplotlib |
| `StoredRun`, `FitArchive`, `StoredFit` | `output.py` | `load_run`, `read_fits` | a finished run folder reopened: config, table, every stored fit |
| `CompositionTable` | `analysis/data.py` | `composition_table` | a run's columns, errors, backgrounds and error correlations, for the analysis |
| `RunAnalysis`, `AnalysisSettings` | `analysis/report.py` | `analyse_runs` | one run's ratios, composition, moving composition and rank |

Two rules hold throughout: these objects are **not modified after creation**
(a fit returns a new `FitResult`, it never edits parameters), and the
**optimizer never knows the geometry** (it only sees the `ForwardModel`
callable).

---

## 5. Every module

`test/test_architecture.py` checks that every module of `plumeIR/` appears
here.

**Top level**

| Module | Holds |
|---|---|
| `__init__.py` | the version, a `NullHandler` on the `plumeIR` logger |
| `__main__.py` | the command line: arguments, log handlers, the run loop (§2.1), `inspect`, `analyse` |
| `pipeline.py` | the run interface (`prepare_run`, `stream_run`, `collect_run`, `run_from_config`), warnings for experimental options |
| `output.py` | the run folder: resolved config, CSVs, `fits.npz`, preprocessing files, summary; `load_run` to reopen one |
| `path_tracer.py` | the path check from the `*PATH` window's ambient tracer (layer): `tracer_report` |
| `species.py` | the species registry: phase, formula, molar mass, elements; aerosol type, refractive-index source, components, density rule |
| `constants.py` | views of the registry (`RFM_GASES`, `POSSIBLE_AEROSOLS`, `WEIGHTS`), display names and colours, spectral window presets |
| `standards.py` | default values: species concentrations and sizes, ILS and temperature bounds, LUT node grids. Read-only |
| `units.py` | unit conversions (column ↔ concentration, ppm·m, g/m²) |
| `physics.py` | Planck radiance and brightness temperature |
| `regression.py` | straight line with errors in both coordinates, York et al. (2004): `york_fit`, `york_fit_grouped` |
| `exceptions.py` | `PlumeIRError` and its subclasses: `ConfigError`, `ReferenceComputationError`, `SpectrumIOError`, `FitError`, `RunFolderError` |
| `rfm_setup.py` | the RFM start-up check (`require_rfm`) |
| `provenance.py` | installed versions and git commits of plumeIR, rfmwrapper and mieEODG, for `summary.txt` |
| `_style.py` | plot style and axis helpers (a dependency leaf: imports only numpy and matplotlib) |

**`config/`** — the config file, from text to objects

| Module | Holds |
|---|---|
| `config/keys.py` | the key catalogue `KEYS` (one `Key` per key), name patterns, section descriptions, `PATH_SECTION` |
| `config/parser.py` | `read_config`, `write_config` (values only), `normalise`, migration of moved keys and renamed species |
| `config/paths.py` | resolution of path-valued keys (`resolve_config_paths`) |
| `config/validator.py` | `validate`: per-key checks from the catalogue, then cross-key rules (targets, dual gases, temperatures passed between windows, `*PATH`) |
| `config/builder.py` | `build_from_config`, `build_from_raw`: config objects and one `ParameterSet` per window |
| `config/objects.py` | the frozen config dataclasses |
| `config/keydoc.py` | `docs/CONFIG_REFERENCE.md`, generated from the catalogue (`python -m plumeIR.config.keydoc`) |

**`parameters/`**

| Module | Holds |
|---|---|
| `parameters/parameter.py` | `Parameter`, `ParameterKind`, `ParameterSet` |

**`spectra/`** — finding and reading files

| Module | Holds |
|---|---|
| `spectra/discovery.py` | `discover_spectra`, `load_spectra`, `explain_no_spectra`; sorting by acquisition time, reference files (blackbodies, self, clear) |
| `spectra/spectrum.py` | `Spectrum`; `read_spectrum`, `read_midac`, `read_bruker`; `resolve_instrument_ils` |
| `spectra/spc.py` | the `.spc` (GRAMS) file reader, no dependency |

**`preprocess/`** — spectra before the fits

| Module | Holds |
|---|---|
| `preprocess/__init__.py` | `preprocess_spectra`: the geometry's preprocessing |
| `preprocess/layer.py` | self-spectrum subtraction and clipping (layer) |
| `preprocess/emission.py` | blackbody calibration to radiance (emission) |
| `preprocess/drift.py` | `DriftingSpectrum`: slow drift of a reference (self, blackbody, clear) |
| `preprocess/products.py` | `preprocess_products`: the whole-spectrum preprocessing of a run, report or apply |
| `preprocess/template.py` | the synthetic template spectrum (ambient path and plume, RFM, cached) |
| `preprocess/background.py` | the additive background from the opaque bands (layer) |
| `preprocess/registration.py` | the wavenumber shift and scale by cross-correlation with the template |
| `preprocess/envelope.py` | hull peak and relative scale of a whole spectrum |
| `preprocess/archive.py` | `preprocess.npz`, what the preprocessing plots draw |

**`reference/`** — what is pre-computed per window

| Module | Holds |
|---|---|
| `reference/__init__.py` | `build_reference_for`: the geometry's reference builder |
| `reference/reference.py` | `Reference`; `build_reference` (layer); the reference cache key and schema version |
| `reference/emission_reference.py` | `build_emission_reference`: atmospheric layers from one RFM `*LEV` run per H2O node |
| `reference/solar_reference.py` | `build_solar_reference`: vertical optical depths of the atmosphere |
| `reference/gas_lut.py` | `GasLUT`: optical depth tables over temperature (and VMR, for a continuum gas), from RFM, cached |
| `reference/aero_lut.py` | `AerosolLUT`: extinction and absorption tables over size (and composition), from mieEODG, cached |
| `reference/atm_lut.py` | `AtmLayerLUT`: radiance, optical depth and effective temperature of one atmospheric layer over H2O scaling (emission) |

**`forward/`** — parameters to model spectrum

| Module | Holds |
|---|---|
| `forward/base.py` | `ForwardModel`; `apply_ils`, `apply_shift_offset`, `gas_optical_depth`, `build_background_poly` |
| `forward/factory.py` | `build_forward_model`: the geometry's forward model |
| `forward/layer.py` | `LayerForwardModel` (and the transmittance variant) |
| `forward/emission.py` | `EmissionForwardModel`: the 2-layer radiance with a plume between |
| `forward/solar.py` | `SolarForwardModel`: solar transmittance with airmass |
| `forward/ils.py` | the instrument line shape (`make_ils`, `ILSKernel`, `core_support`, `kernel_trim_fov`, `broad_kernel_args` for the optional broad component) |

**`retrieval/`** — the fits

| Module | Holds |
|---|---|
| `retrieval/run.py` | `iter_retrieval` (serial or parallel, stoppable), `_fit_spectrum`, `_staged_temps`, `collect_results`, `run_retrieval` |
| `retrieval/nlls.py` | `NonLinearLeastSquares` (`scipy.optimize.least_squares`), the default backend |
| `retrieval/oe.py` | `OptimalEstimation` (pyOptimalEstimation) |
| `retrieval/results.py` | `FitResult`, `ResultsTable`, `r_squared` |

**`analysis/`** — composition of finished runs

| Module | Holds |
|---|---|
| `analysis/__init__.py`, `analysis/__main__.py` | the package; `python -m plumeIR.analysis` |
| `analysis/cli.py` | `plumeIR analyse`: options, `run_analysis` |
| `analysis/report.py` | `analyse_runs`, `analyse_table`, `write_analysis`, `analysis_summary` |
| `analysis/data.py` | `composition_table`: columns, backgrounds by gas class, error correlations |
| `analysis/ratios.py` | `moving_ratio`, `run_ratios`, `pooled_ratio`, `window_spread`, jackknife errors |
| `analysis/components.py` | the all-species composition: `composition`, `moving_composition`, `rank_table`, molar and mass fractions |
| `analysis/mlpca.py` | the weighted low-rank fit (`fit_components`, `fit_offsets`), with correlated errors |

**`plotting/`** — figures, never computing results

| Module | Holds |
|---|---|
| `plotting/state.py` | `RunState`: the numbers a display shows; the live ratio regressions |
| `plotting/panels.py` | `FitPanel`, `TimeSeriesPanel`, `RatioPanel`: one `Axes` each |
| `plotting/canvas.py` | `AnalysisCanvas` (the live window and saved canvas), `save_final_figures` |
| `plotting/inspector.py` | `Inspector`: the canvas stepped through a finished run |
| `plotting/spectrum.py` | `plot_spectrum(spectrum, ax)` |
| `plotting/preprocess.py` | the whole-spectrum preprocessing figures |
| `plotting/analysis.py` | moving-ratio and window figures of the analysis |
| `plotting/components.py` | composition donut, stacked fractions, rank |
| `plotting/analysis_report.py` | `save_composition_figures`: the analysis figures of `plumeIR analyse` |

**`gui/`** — a stub; the GUI is not written yet (section 8).

---

## 6. Where do I look for…

| Question | File |
|---|---|
| How do I write a config? | `docs/CONFIG_GUIDE.md`, and the two configs in `examples/` (the guide shows them in full) |
| What does a config key mean / what are its defaults? | `docs/CONFIG_REFERENCE.md`, generated from `config/keys.py` by `python -m plumeIR.config.keydoc docs/CONFIG_REFERENCE.md`; per-species priors and fit bounds in `standards.py`. A written config carries values only |
| How do I add a config key? | one `Key` in `config/keys.py`, a field in the dataclass in `config/objects.py` (default `default(...)`), regenerate the reference; the parser, per-key checks and the builder follow. A rule involving several keys goes in `config/validator.py` |
| Why was my config rejected? | `config/validator.py` |
| How are relative paths in a config resolved? | `config/paths.py` |
| How is a file's time stamp read? | `spectra/spectrum.py` (readers), `spectra/discovery.py` (sorting) |
| Why were no spectra found? | `spectra/discovery.py` (`explain_no_spectra` names the rule that removed every file) |
| How are emission spectra calibrated to radiance? | `preprocess/emission.py` |
| How is instrument drift handled? | `preprocess/drift.py` |
| The whole-spectrum preprocessing (`*PREPROCESS background / registration`)? | `preprocess/products.py`, called at the end of `pipeline.py::prepare_run` (§2.3); plots in `plotting/preprocess.py` |
| Which RFM runs are made, and how are they cached? | `reference/gas_lut.py`, `reference/emission_reference.py`, `preprocess/template.py`; the reference cache key is `reference/reference.py::_reference_cache_key` |
| Aerosol optical properties? | `reference/aero_lut.py` (wraps `mieEODG`) |
| The radiative transfer equation of a geometry? | `forward/<geometry>.py`, `model_highres` and `__call__` |
| The instrument line shape? | `forward/ils.py`, applied in `forward/base.py::apply_ils` |
| How the fit itself runs? | `retrieval/run.py::_fit_spectrum` (§2.4), `retrieval/nlls.py` (default), `retrieval/oe.py` |
| A gas in both roles (`{GAS}_atm` + `{GAS}`)? | `config/objects.py` (`FitWindowConfig.atm_name`, `uses_2d`), `reference/reference.py`, `forward/layer.py` |
| A temperature passed between windows (`plume_temp_from`, `atm_temp_from`)? | checked in `config/validator.py::_check_temp_from`, held fixed by `config/builder.py`, set per spectrum in `retrieval/run.py::_staged_temps` |
| The path-length check (`*PATH`)? | parsed as a window (`config/parser.py`, `config/keys.py::PATH_SECTION`), checked in `config/validator.py::_check_path_tracer`, reported by `path_tracer.py`, left out of the fit panels by `plotting/canvas.py` |
| Serial vs parallel fitting? How is a run stopped? | `retrieval/run.py` (`iter_retrieval`) |
| What does a run write, and where? | `output.py` (§2.1); the command line in `__main__.py` |
| Gas ratios shown during a run? | `*PLOT` (`PlotConfig`); `plotting/state.py` (`RunState.ratio_fit`); the `odr` line in `regression.py` |
| Gas ratios and composition of a finished run? | `analysis/` (§2.6); the method in its module docstrings |
| The live window, saved figures, the inspector? | `plotting/canvas.py` (layout), `plotting/panels.py` (the panels), `plotting/inspector.py`; flags in `__main__.py` |
| How do I draw a spectrum? | `plotting/spectrum.py` (`plot_spectrum(spectrum, ax)`) |
| Plot style, colours? | `_style.py` (`STYLE`, `style_context`, `discrete_colors`, `lighten`) |
| Unit conversion of results? | `retrieval/results.py`, `units.py` |
| Why a design is the way it is? | the module docstrings, and `CHANGELOG.md` for what changed from v2 and by how much |

---

## 7. Dependency direction

What each part imports from the rest of plumeIR (measured from the code,
2026-11-18). There are no cycles between modules; read the table from the
bottom up for the order in which the parts build on each other.

| Part | Imports |
|---|---|
| `__main__` (the client) | `pipeline`, `output`, `path_tracer`, `analysis`, `plotting`, `config`, `preprocess`, `provenance`, `rfm_setup` |
| `analysis` | `output`, `retrieval`, `config`, `regression`, `standards`, `units`, `constants`, `species`, `provenance`; `plotting` only in `analysis/cli.py`, inside a function |
| `plotting` | `analysis`, `output`, `retrieval`, `preprocess`, `spectra`, `config`, `regression`, `physics`, `standards`, `units`, `constants`, `_style` |
| `output` | `path_tracer`, `preprocess`, `retrieval`, `config`, `provenance`, `constants` |
| `pipeline` | `preprocess`, `reference`, `retrieval`, `spectra`, `config` |
| `path_tracer` | `retrieval`, `units` |
| `preprocess` | per spectrum (`layer`, `emission`, `drift`): `spectra`, `config`; whole spectrum (`products`, `template`, `registration`): also `forward`, `reference`, `retrieval`, `physics` |
| `retrieval` | `forward`, `parameters`, `spectra`, `config`, `units`, `constants`, `species` |
| `forward` | `reference`, `parameters`, `config`, `physics` |
| `reference` | `parameters`, `config`, `physics`, `standards`, `units`, `constants`, `species` |
| `config` | `parameters`, `standards`, `units`, `constants`, `species` |
| `spectra` | `constants` |
| `rfm_setup` | `exceptions` |
| `units` | `constants` |
| `constants` | `species` |
| leaves | `species`, `standards`, `physics`, `regression`, `exceptions`, `provenance`, `_style`, `parameters` |

(Every part may also import `exceptions`.)

- The whole-spectrum preprocessing builds a model spectrum with the line
  shape and the references, so it sits above `forward`, `reference` and
  `retrieval`; the per-spectrum preprocessing sits just above `spectra`.
- `analysis` computes from run folders (through `output`) without drawing;
  `plotting` imports it for its figures and for
  `analysis/data::ambient_column`, the run state's ratio backgrounds.
- `spectra` never imports `plotting`. Nothing in the library imports a
  client.

---

## 8. Clients: the command line and the GUI

The library computes; **clients** decide what to show and where. The command
line is the client that exists; a GUI (`gui/`, not yet written) is meant to
be a second client of the same small interface, so adding it adds files and
does not change the library. The interface has three parts.

**A run is a stream of results.** `pipeline.stream_run(setup)` (over
`retrieval/run.py::iter_retrieval`) is a generator yielding one `SpectrumFits`
per spectrum as its fits finish — input order when serial, completion order
when parallel (`SpectrumFits.index` restores input order). A client stops a
run by no longer reading and closing the stream (`close()`, or `break` and
drop it): queued spectra are cancelled, fits already in a worker finish, and
the pool is shut down before `close()` returns. `collect_run(setup, items)`
turns the stream, or the items read before a stop, into a `ResultsTable`;
`run_from_config` is prepare → stream → collect. The CLI reads the stream in
its main loop; a GUI would read it in a worker thread that posts to the Tk
thread.

**Plotting is state → panels → layouts,** so the same panels can sit in one
command-line window or each in its own GUI window:

| Layer | Knows about | Lives in | Used by |
|---|---|---|---|
| **run state** | numbers only: latest fit per window, column time series, ratio regressions (Siegel / Theil-Sen / ODR, scrolling window) | `plotting/state.py` | every panel, CLI and GUI alike |
| **panels** | one `Axes` each: fit (spectrum, model, residual), time series, ratio. `update(state)` redraws its own artists | `plotting/panels.py` | any layout |
| **layouts** | where panels go, and the toolkit | CLI: `plotting/canvas.py` (one window of all panels, `pyplot`) and `plotting/inspector.py` (the same canvas, stepped through a finished run). GUI (not yet written): one Tk window or tab per panel, in `gui/` | the client only |

A panel never creates a figure or window, never calls `pyplot`, and never
draws itself to screen; the layout does. The regressions live in the state,
not in the panels, so both clients show the same slope. The time series
scales each column to its range, `(y − min) / (max − min)`; the `*PATH`
window gets no fit panel.

The CLI feeds `RunState.add(item)` in its stream loop, always
(the final ratios go into `summary.txt` whether or not anything is drawn), and
redraws the canvas at most once per `--redraw-interval` (default 1 s) and at
the end: drawing costs far more than the state. A GUI would do
the same with a Tk timer, which can also redraw between results.
`AnalysisCanvas(state, interactive=...)` is a `pyplot` window only for
`--plot`; for `--save-plots` alone (headless) it is a bare
`matplotlib.figure.Figure`, so no GUI backend is loaded. Panels have a live
form (latest `scroll` regressed and highlighted) and a whole-run form
(`live=False`), which `save_final_figures` uses, for a live run's state or a
finished run's (`RunState.from_table(ResultsTable.from_csv(...))`, which has
columns and ratios but no spectra, so no fit panels). The state reads columns
through `ResultsTable.from_fits`, the same conversion as the CSV, so a run and
its reloaded table give identical slopes. A GUI would build one `RunState`
per run, feed it from its worker thread's items, and give each panel its own
`Figure` in a Tk window.

Every run also writes `fits.npz` (`output.write_fits`: per window, each
spectrum's measured, model and reference-continuum spectra on its measurement
grid and the fit's covariance, float64, in input order). `output.load_run(run_dir)`
reopens a folder — its `config.ini` rebuilt into config objects, its
`results_long.csv`, its fits — without refitting or building a reference, and
`RunState.from_run_dir` turns that into a state that holds every fit:
`select(k)` makes the k-th spectrum's fits the ones the fit panels show and
the highlighted ratio point, and the time series marks it with a red dashed
line. The inspector (`plotting/inspector.py`) is the canvas in its whole-run
form plus key and click bindings that call `select` and redraw; a GUI would
bind its own controls to the same `select`. The inspector needs `fits.npz`
(a folder written before 2026-11-12 fails with a `RunFolderError` saying
so); the composition analysis does not (`load_run(require_fits=False)`).

**Logging is configured by the client.** The library only creates loggers,
plus a `NullHandler` on `'plumeIR'`; importing it writes nothing anywhere. The
CLI attaches a console handler and a log file in the run folder (to the
`plumeIR`, `rfmwrapper` and `mieEODG` loggers, removed when the run ends); a
GUI would attach its log-console handler. A script calls `logging.basicConfig(level=logging.INFO)`.

Two more things that keep the library safe for a long-lived client such as a
GUI:
- a config built from data as well as from a file:
  `build_from_raw(raw, base_dir)`, which `build_from_config(path)` calls after
  `read_config`; `prepare_run` takes either a path or the built dict.
  `read_config` / `write_config` round-trip exactly (tested on both examples);
- default tables in `standards.py` that cannot be modified at run time, since a
  GUI process runs many analyses in one session and a change would leak from one
  to the next. They are `MappingProxyType` views and non-writeable arrays; an
  experiment script that needs another value rebinds the module attribute to
  `standards.freeze({...})`.
