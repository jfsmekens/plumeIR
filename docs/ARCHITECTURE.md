# plumeIR v3 — Code map

The map of the package as it is: what each part does, the order data flows
through it, and where to look for a given thing. It describes the current
code; what changed from v2 is in `CHANGELOG.md`.

---

## 1. The pipeline in one line

```
config ─▶ spectra ─▶ preprocess ─▶ reference ─▶ forward model ─▶ fit ─▶ results
```

Every run is this sequence, once per fit window, and each step is one
subpackage. `plumeIR/pipeline.py` is the whole sequence in three short
functions — `prepare_run` (steps 1-4 and loading), `stream_run` (steps 5-6,
yielding each spectrum's fits as they finish) and `collect_run` (step 7);
`run_from_config` chains them. Read it first. The command line
(`plumeIR/__main__.py`, `python -m plumeIR CONFIG`) is the same three calls plus
the run folder (`plumeIR/output.py`).

| Step | Subpackage | Entry point | Produces |
|---|---|---|---|
| 1. Read and check the config | `config/` | `build_from_config(path)` | typed config objects, one `ParameterSet` per window |
| 2. Find and read spectra | `spectra/` | `load_spectra(data_config)` | `Spectrum` objects, sorted by time |
| 3. Prepare spectra | `preprocess/` | `preprocess_spectra(spectra, geometry, data, preprocess)` | calibrated / background-corrected `Spectrum` |
| 4. Pre-compute cross-sections | `reference/` | `build_reference_for(geometry, window, ...)` | one `Reference` per window (cached on disk) |
| 5. Model a spectrum | `forward/` | `build_forward_model(reference, params, ...)` | a `ForwardModel`: parameter vector → model spectrum |
| 6. Fit | `retrieval/` | `iter_retrieval(spectra, references, ...)` | a stream of `SpectrumFits`: one `FitResult` per window, per spectrum |
| 7. Collect | `retrieval/run.py` | `collect_results(stream, ...)` | the run's table: columns in molec/cm², ppm·m, g/m² |

Steps 1-4 happen once per run; steps 5-6 once per spectrum. Step 4 and the
whole-spectrum preprocessing that ends `prepare_run` (`preprocess/`, its
synthetic template) are the only steps that call RFM (through `rfmwrapper`)
or Mie code (through `mieEODG`), and the only slow ones on a first run.

---

## 2. Rows are steps, columns are geometries

The three measurement geometries share the pipeline and differ in three steps.
To work on one geometry, read its column:

| Step | Layer (transmission) | Emission (sky) | Solar (occultation) |
|---|---|---|---|
| preprocess | `preprocess/layer.py` | `preprocess/emission.py` | none yet |
| reference | `reference/reference.py` | `reference/emission_reference.py` | `reference/solar_reference.py` |
| forward | `forward/layer.py` | `forward/emission.py` | `forward/solar.py` |
| status | **supported** | **supported** (absolute radiance) | experimental |

Everything else is shared by all three.

---

## 3. The objects that carry a run

| Object | Defined in | Made by | What it is |
|---|---|---|---|
| `DataConfig`, `ModelConfig`, `LayerGeometry` / `EmissionGeometry` / `SolarGeometry`, `Retrieval`, `FitWindowConfig`, `PlotConfig` | `config/objects.py` | `config/builder.py` | the config, typed and validated. Frozen; no file I/O |
| `Parameter`, `ParameterSet` | `parameters/parameter.py` | `config/builder.py` | what is fitted: value, bounds, free or fixed, reference column. Immutable |
| `Spectrum` | `spectra/spectrum.py` | `spectra/` readers | one measurement: wavenumber, values, time, file, type |
| `Reference` (holding `GasLUT`, `AerosolLUT`, `AtmLayerLUT`) | `reference/` | `build_reference_for` | pre-computed optical depths and radiances for one window. Immutable, cached |
| `ForwardModel` (`LayerForwardModel`, `EmissionForwardModel`, `SolarForwardModel`) | `forward/` | `build_forward_model` | a callable: parameter vector → model spectrum. Knows nothing about fitting |
| `ILSKernel` | `forward/ils.py` | `make_ils` | the instrument line shape |
| `FitResult` | `retrieval/results.py` | the backends (`nlls.py`, `oe.py`) | one fit: values, errors, covariance, model and measured spectra, R² |
| `SpectrumFits` | `retrieval/run.py` | `iter_retrieval` | one item of a run's stream: a spectrum, its index and its fits, one per window |
| `RunSetup` | `pipeline.py` | `prepare_run` | everything before the first fit: config objects, references, preprocessed spectra |
| `ResultsTable` | `retrieval/results.py` | `ResultsTable.from_fits` | all fits of a run as one pandas table, with unit conversions |
| `RunState` | `plotting/state.py` | `RunState.for_run` + `add`, or `RunState.from_table` | what a display shows: latest fit per window, target columns, ratio regressions. Accumulates; no matplotlib |

Two rules hold throughout: these objects are **not modified after creation**
(a fit returns a new `FitResult`, it never edits parameters), and the
**optimizer never knows the geometry** (it only sees the `ForwardModel`
callable).

---

## 4. Shared helpers (top level)

| Module | Holds |
|---|---|
| `species.py` | the species registry: phase, formula, molar mass, elements; aerosol type, refractive-index source, components, density rule |
| `constants.py` | views of the registry (`RFM_GASES`, `POSSIBLE_AEROSOLS`, `WEIGHTS`), display names and colours, spectral window presets |
| `standards.py` | default values: species concentrations and sizes, ILS defaults, temperature grids, LUT node grids. Read-only (`MappingProxyType`, non-writeable arrays) |
| `units.py` | unit conversions (column ↔ concentration, ppm·m, g/m²) |
| `physics.py` | Planck radiance and brightness temperature |
| `regression.py` | straight line with errors in both coordinates, York et al. (2004) (`york_fit`): the `odr` gas-ratio regression |
| `exceptions.py` | `PlumeIRError` and its subclasses: `ConfigError`, `ReferenceComputationError`, `SpectrumIOError`, `FitError`, `RunFolderError` |
| `rfm_setup.py` | the RFM start-up check (`require_rfm`): stops a run with the steps to obtain and configure RFM |
| `provenance.py` | installed versions and git commits of plumeIR, rfmwrapper and mieEODG, for `summary.txt` |
| `pipeline.py` | the run interface (`prepare_run`, `stream_run`, `collect_run`) and the run-time warnings for experimental options |
| `output.py` | the run folder: resolved config, long and wide CSVs, summary (any client) |
| `__main__.py` | the command line (`python -m plumeIR`, `plumeIR`): arguments, log handlers, the main loop |
| `_style.py` | plot style and axis helpers (a dependency leaf: imports only numpy and matplotlib) |

---

## 5. Where do I look for…

| Question | File |
|---|---|
| How do I write a config? | `docs/CONFIG_GUIDE.md` (and the two configs in `examples/`) |
| What does a config key mean / what are its defaults? | `config/keys.py` (`KEYS`, one `Key` per key: type, unit, default, limits, geometries, help); `docs/CONFIG_REFERENCE.md`, generated from it by `python -m plumeIR.config.keydoc docs/CONFIG_REFERENCE.md`; per-species priors and fit bounds in `standards.py` |
| How do I add a config key? | one `Key` in `config/keys.py`, a field in the dataclass in `config/objects.py` (default `default(...)`), regenerate the reference; the parser, per-key checks and the builder follow. A rule involving several keys goes in `config/validator.py` |
| Why was my config rejected? | `config/validator.py` |
| How are relative paths in a config resolved? | `config/paths.py` |
| How is a file's time stamp read? | `spectra/spectrum.py` (readers), `spectra/discovery.py` (sorting) |
| How is a MIDAC `.spc` file decoded? | `spectra/spc.py` (`read_spc`, no dependency), used by `spectra/spectrum.py::read_midac` |
| How are emission spectra calibrated to radiance? | `preprocess/emission.py` |
| How is instrument drift handled? | `preprocess/drift.py` |
| Gas composition of finished runs? | `analysis/data.py` (`composition_table`, backgrounds by gas class), `analysis/ratios.py` (`moving_ratio`, `run_ratios`, `pooled_ratio`, `window_spread`), `analysis/components.py` (multi-gas `composition`, `moving_composition`, `rank_table`, molar fractions) on the weighted low-rank fit in `analysis/mlpca.py` (`fit_components`, `fit_offsets`; correlated errors between species); the grouped York line in `regression.py::york_fit_grouped`; plots in `plotting/analysis.py` (ratios) and `plotting/components.py` (composition, donut, stacked fractions); the `plumeIR analyse` command in `analysis/cli.py` (tables and summary `analysis/report.py`, figures `plotting/analysis_report.py`) |
| The whole-spectrum preprocessing (whole-spectrum measurements before the fits, `*PREPROCESS background / registration`)? | `preprocess/products.py::preprocess_products`, called at the end of `pipeline.py::prepare_run`; `preprocess/`; the synthetic template of the ambient path and plume, over the instrument's range, in `preprocess/template.py`; the additive background from the opaque bands in `preprocess/background.py`; the wavenumber shift and scale by cross-correlation in `preprocess/registration.py`; `preprocess.npz` in `preprocess/archive.py`; its plots in `plotting/preprocess.py` |
| Which RFM runs are made, and how are they cached? | `reference/gas_lut.py`, `reference/emission_reference.py`; the reference cache key is `reference/reference.py::_reference_cache_key` |
| What does a run write, and where? | `output.py`; the command line in `__main__.py` |
| Aerosol optical properties? | `reference/aero_lut.py` (wraps `mieEODG`) |
| The radiative transfer equation of a geometry? | `forward/<geometry>.py`, `__call__` / `model_highres` |
| The instrument line shape? | `forward/ils.py`, applied in `forward/base.py::apply_ils` |
| How the fit itself runs? | `retrieval/nlls.py` (default), `retrieval/oe.py` |
| A gas in both roles (`{GAS}_atm` + `{GAS}`)? | `config/objects.py` (`FitWindowConfig.atm_name`, `uses_2d`), `reference/reference.py`, `forward/layer.py` |
| A temperature passed between windows (`plume_temp_from`, `atm_temp_from`)? | checked in `config/validator.py::_check_temp_from`, held fixed by `config/builder.py`, set per spectrum in `retrieval/run.py::_staged_temps` |
| Why were no spectra found? | `spectra/discovery.py` (`discover_spectra`; `explain_no_spectra` names the rule that removed every file) |
| Serial vs parallel fitting? How is a run stopped? | `retrieval/run.py` (`iter_retrieval`) |
| How do I draw a spectrum? | `plotting/spectrum.py` (`plot_spectrum(spectrum, ax)`) |
| Gas ratios: which, how regressed? | `*PLOT` config section (`PlotConfig`); `plotting/state.py` (`fit_ratio`, `RunState.ratio_fit`); the `odr` line in `regression.py` |
| The live window, saved figures? | `plotting/canvas.py` (layout), `plotting/panels.py` (the panels); flags in `__main__.py` |
| Plot style, colours? | `_style.py` (`STYLE`, `style_context`, `discrete_colors`, `lighten`) |
| Unit conversion of results? | `retrieval/results.py`, `units.py` |
| Why a design is the way it is? | the module docstrings, and `CHANGELOG.md` for what changed from v2 and by how much |

---

## 6. Dependency direction

Imports only flow one way, in pipeline order, with no cycles:

```
constants, exceptions, physics, standards, units, regression, _style   (leaves)
        ▲
parameters ◀── config
        ▲
spectra ◀── preprocess
        ▲
reference ◀── forward ◀── retrieval ◀── pipeline ◀── clients (CLI, GUI)

spectra, config, retrieval ◀── plotting (state reads results; panels take an Axes)
```

`spectra` never imports `plotting`; `plotting` may import `spectra`, `config`
and `retrieval` (the run state builds its columns with `ResultsTable`).
Nothing in the library imports a client.

---

## 7. Clients: the command line and the GUI

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
not in the panels, so both clients show the same slope.

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

Every run also writes `fits.npz` (`output.write_fits`: per
window, each spectrum's measured, model and reference-continuum spectra on
its measurement grid, float64, in input order). `output.load_run(run_dir)`
reopens a folder — its `config.ini` rebuilt into config objects, its
`results_long.csv`, its fits — without refitting or building a reference, and
`RunState.from_run_dir` turns that into a state that holds every fit:
`select(k)` makes the k-th spectrum's fits the ones the fit panels show and
the highlighted ratio point, and the time series marks it. The inspector
(`plotting/inspector.py`) is the canvas in its whole-run form plus key and
click bindings that call `select` and redraw; a GUI would bind its own
controls to the same `select`. `python -m plumeIR RUN_FOLDER --inspect` opens one; a
config with `--inspect`, or any `--plot` run, opens its own folder when it
ends. A folder without `fits.npz` (written by an earlier version) fails with
a `RunFolderError` saying so.

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
  `read_config` / `write_config` round-trip exactly (tested on both configs);
- default tables in `standards.py` that cannot be modified at run time, since a
  GUI process runs many analyses in one session and a change would leak from one
  to the next. They are `MappingProxyType` views and non-writeable arrays; an
  experiment script that needs another value rebinds the module attribute to
  `standards.freeze({...})`.
