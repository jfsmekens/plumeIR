"""
Run state: the numbers a live display shows, with no drawing.

A :class:`RunState` is fed one streamed result at a time
(``pipeline.stream_run`` → :meth:`RunState.add`), a finished run's table
(:meth:`RunState.from_table`), or a finished run's folder
(:meth:`RunState.from_run_dir`, which also holds every stored fit, so
:meth:`RunState.select` can show any spectrum: the inspector), and holds

* the latest fit per window (measured and model spectra, R²), or the
  selected spectrum's when inspecting,
* the target columns of every spectrum received, in input order,
* the gas-ratio regressions over the latest ``scroll`` spectra or over all,
* the ambient background column of each gas, where it is known (layer
  geometry: a VMR over the path at the ambient P and T).

It imports no matplotlib.  The panels (``plotting/panels.py``) read it, and so
does any other client (the command line, a GUI), so all show the same slopes.
Columns come from :class:`~plumeIR.retrieval.results.ResultsTable`, the same
conversion that writes the run's CSV, so a live run and the same run reloaded
from ``results_long.csv`` give identical numbers.

The ratio regressions follow v2's analysis canvas
(``plumeIR_v2_archive/plotting.py::updateAnalysisCanvas``, lines 330-393):
Siegel repeated medians by default, Theil-Sen, or orthogonal distance
regression; at least three points.  Two departures: failed fits are left out
(v2 kept whatever value a failed fit had), and ODR is weighted by both
columns' fit errors when every error is finite and positive.  ODR is solved
by York et al. (2004) (``plumeIR.regression.york_fit``), which minimises the
same objective as ``scipy.odr`` for a straight line; ``scipy.odr`` is removed
in SciPy 1.19.
"""

from __future__ import annotations

import logging
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import siegelslopes, theilslopes

from plumeIR.config.objects import (
    DUAL_ATM_SUFFIX, RATIO_REGRESSIONS, LayerGeometry, PlotConfig,
)
from plumeIR.constants import POSSIBLE_AEROSOLS, POSSIBLE_GASES
from plumeIR.regression import RegressionError, york_fit
from plumeIR.retrieval.results import (
    COL_KIND, COL_PARAM, COL_SCD, COL_SCD_ERR, COL_SPECIES, COL_SPECTRUM,
    COL_SUCCESS, COL_WINDOW, ResultsTable,
)
from plumeIR.spectra.spectrum import SpectrumType
from plumeIR.analysis.data import ambient_column
from plumeIR.standards import BACKGROUND_VMR
from plumeIR.units import molec_cm2_to_g_m2

logger = logging.getLogger(__name__)

#: 95% two-sided normal quantile: converts Theil-Sen's 95% slope interval to 1σ.
_Z95 = 1.959963984540054


# =============================================================================
# Regression
# =============================================================================

@dataclass(frozen=True)
class RatioFit:
    """
    A straight line ``y = slope·x + intercept`` through one ratio's points.

    Attributes
    ----------
    slope : float
        The ratio y/x (molar, or mass when an aerosol is involved).
    intercept : float
        Column of y at zero x: the background.
    slope_err : float
        1σ of the slope: for ODR the York error, scaled by ``sqrt(mswd)``
        when ``mswd > 1`` or the fit is unweighted
        (:class:`plumeIR.regression.LineFit`); Theil-Sen's 95% interval /
        3.92; NaN for Siegel, which gives none.
    intercept_err : float
        1σ of the intercept (ODR only; NaN otherwise).
    n : int
        Points used.
    method : str
        One of ``RATIO_REGRESSIONS``.
    mswd : float
        ODR only: reduced chi-square of the weighted residuals, expected 1
        when the column errors explain the scatter (NaN otherwise, and for
        an unweighted ODR, where it has no absolute meaning).
    """
    slope:         float
    intercept:     float
    slope_err:     float
    intercept_err: float
    n:             int
    method:        str
    mswd:          float = float("nan")


def fit_ratio(
    x: np.ndarray,
    y: np.ndarray,
    sx: np.ndarray | None = None,
    sy: np.ndarray | None = None,
    method: str = "siegel",
    r: np.ndarray | float = 0.0,
) -> RatioFit | None:
    """
    Regress ``y`` on ``x`` with a free intercept.

    Parameters
    ----------
    x, y : np.ndarray
        Columns, shape ``(n,)``.  Non-finite pairs are dropped.
    sx, sy : np.ndarray or None
        1σ errors of ``x`` and ``y``.  Used by ODR only, and only when every
        retained error is finite and positive; otherwise ODR is unweighted.
    method : str
        ``'siegel'``, ``'theil'`` or ``'odr'``.
    r : np.ndarray or float
        Correlation of the errors of ``x`` and ``y`` (per point, or one
        value), for weighted ODR; 0 for species fitted in different windows.

    Returns
    -------
    RatioFit or None
        ``None`` with fewer than three usable points, or when ``x`` does not
        vary.

    Raises
    ------
    ValueError
        For an unknown method.
    """
    if method not in RATIO_REGRESSIONS:
        raise ValueError(f"Unknown ratio regression {method!r}; "
                         f"expected one of {RATIO_REGRESSIONS}")
    x, y = np.asarray(x, dtype=float), np.asarray(y, dtype=float)
    ok = np.isfinite(x) & np.isfinite(y)
    if ok.sum() < 3 or np.ptp(x[ok]) == 0:
        return None
    xo, yo = x[ok], y[ok]
    nan = float("nan")

    if method == "siegel":
        res = siegelslopes(yo, xo)
        return RatioFit(float(res.slope), float(res.intercept), nan, nan,
                        int(ok.sum()), method)

    if method == "theil":
        res = theilslopes(yo, xo)
        err = (res.high_slope - res.low_slope) / (2 * _Z95)
        return RatioFit(float(res.slope), float(res.intercept), float(err), nan,
                        int(ok.sum()), method)

    # ODR: York et al. (2004), weighted when every retained error is usable.
    sxo = None if sx is None else np.asarray(sx, dtype=float)[ok]
    syo = None if sy is None else np.asarray(sy, dtype=float)[ok]
    ro = np.nan_to_num(np.broadcast_to(np.asarray(r, dtype=float), ok.shape)[ok], nan=0.0)
    weighted = (sxo is not None and syo is not None
                and np.all(np.isfinite(sxo)) and np.all(sxo > 0)
                and np.all(np.isfinite(syo)) and np.all(syo > 0))
    try:
        line = york_fit(xo, yo, sxo, syo, ro) if weighted else york_fit(xo, yo)
    except RegressionError as exc:
        logger.warning("ODR ratio fit failed: %s", exc)
        return None
    return RatioFit(line.slope, line.intercept, line.slope_err,
                    line.intercept_err, line.n, method,
                    line.mswd if weighted else nan)


# =============================================================================
# Run state
# =============================================================================

#: Error correlations are clipped to ±this (York needs |ρ| < 1).
MAX_CORRELATION = 0.999


def fit_correlation(fit, a: str, b: str) -> float:
    """
    Correlation of the errors of parameters ``a`` and ``b`` in one fit, from
    its covariance; NaN when it has none or did not vary both.

    Parameters
    ----------
    fit : FitResult
    a, b : str
    """
    names = list(fit.varied_names)
    c = fit.covariance
    if c is None or a not in names or b not in names:
        return np.nan
    i, j = names.index(a), names.index(b)
    v = c[i, i] * c[j, j]
    return float(c[i, j] / np.sqrt(v)) if np.isfinite(v) and v > 0 else np.nan


def _column_targets(targets: Iterable[str]) -> list[str]:
    """Targets that are species with a column (not diagnostics like ``gas_temp``)."""
    species = set(POSSIBLE_GASES) | set(POSSIBLE_AEROSOLS)
    out: list[str] = []
    for t in targets:
        if t in species and t not in out:
            out.append(t)
    return out


class RunState:
    """
    Accumulated results of a run, as a live display needs them.

    Parameters
    ----------
    targets : sequence of str
        Species whose columns are tracked, in display order.
    ratios : sequence of (str, str)
        ``(y, x)`` pairs to regress.  Both species must be in ``targets``.
    scroll : int
        Number of latest spectra regressed while live.
    regression : str
        One of ``RATIO_REGRESSIONS``.
    n_spectra : int or None
        Spectra the run will fit, if known.
    windows, params, geometry
        The run's ``FitWindowConfig`` and ``ParameterSet`` per window label and
        its geometry, needed by :meth:`add` to turn fits into columns.  Not
        needed for :meth:`from_table`.  The geometry also places the ambient
        backgrounds (layer only).
    background : dict[str, float] or None
        Ambient VMR [ppmv] per gas, overriding ``standards.BACKGROUND_VMR``.
    rh : float or None
        Relative humidity [%] for the H2O background.
    dual : iterable of str or None
        Target gases fitted in both roles in their window (14b), whose target
        is the plume part alone: background 0.  Found from ``windows`` when
        those are given; :meth:`from_table` finds them from the table.

    Notes
    -----
    Spectra are kept in input order (``SpectrumFits.index``) whatever order
    they arrive in, so a parallel run's series and scroll window are the same
    as a serial run's once it completes.  The *newest* point is the one that
    arrived last.
    """

    def __init__(
        self,
        targets: Sequence[str],
        ratios: Sequence[tuple[str, str]] = (),
        scroll: int = 200,
        regression: str = "siegel",
        n_spectra: int | None = None,
        windows: dict | None = None,
        params: dict | None = None,
        geometry=None,
        background: dict | None = None,
        rh: float | None = None,
        dual: Iterable[str] | None = None,
    ) -> None:
        self.targets: list[str] = _column_targets(targets)
        missing = sorted({sp for pair in ratios for sp in pair} - set(self.targets))
        if missing:
            raise ValueError(f"Ratio species {missing} are not targets "
                             f"{self.targets}")
        if regression not in RATIO_REGRESSIONS:
            raise ValueError(f"Unknown ratio regression {regression!r}")
        self.ratios: list[tuple[str, str]] = [tuple(p) for p in ratios]
        self.scroll = int(scroll)
        self.regression = regression
        self.n_spectra = n_spectra
        self.window_labels: list[str] = list(windows) if windows else []

        self._windows = windows or {}
        self._params = params or {}
        self._geometries = {label: geometry for label in self._windows}
        self.geometry = geometry
        self.background_vmr: dict[str, float] = {**BACKGROUND_VMR, **(background or {})}
        self.rh = rh
        self.dual: set[str] = set(dual or ()) | {
            g for w in self._windows.values() if w is not None
            for g in w.dual_gases if g in w.targets}

        self.latest_fits: dict = {}          # window label → FitResult
        self.latest_spectrum = None          # Spectrum of the newest item
        self.latest_id: str | None = None    # its spectrum_id
        self.latest_position: int | None = None   # its input index

        # Inspection (from_run_dir): every stored fit, and the spectrum shown.
        self.archive = None                  # output.FitArchive
        self.run_dir: Path | None = None     # the folder it was read from
        self.window_configs: dict = {}       # label → FitWindowConfig, for titles
        self.selected: int | None = None     # input-order index shown
        self._spec_type = None               # SpectrumType of the selected one

        self._order: list[int] = []
        self._ids: list[str] = []
        self._scd: dict[str, list[float]] = {sp: [] for sp in self.targets}
        self._err: dict[str, list[float]] = {sp: [] for sp in self.targets}
        self._is_aerosol: dict[str, bool] = {sp: sp in POSSIBLE_AEROSOLS
                                             for sp in self.targets}
        # Target window of each species, and the error correlation of each
        # ratio whose two species share one (from the fit covariance).
        self._window_of: dict[str, str] = {}
        self._rho: dict[tuple[str, str], list[float]] = {p: [] for p in self.ratios}
        self._sorted: tuple[int, np.ndarray] | None = None
        self._fit_cache: dict = {}

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------

    @classmethod
    def for_run(cls, setup, plot: PlotConfig | None = None) -> "RunState":
        """
        An empty state for a prepared run, to be fed with :meth:`add`.

        Parameters
        ----------
        setup : RunSetup
            From ``pipeline.prepare_run``.
        plot : PlotConfig or None
            Ratio settings; ``setup.plot`` if ``None``.
        """
        plot = plot or setup.plot
        targets = [t for w in setup.windows.values() for t in w.targets]
        return cls(targets, ratios=plot.ratios, scroll=plot.scroll,
                   regression=plot.regression, n_spectra=setup.n_spectra,
                   windows=setup.windows, params=setup.params,
                   geometry=setup.geometry, background=plot.background,
                   rh=plot.rh)

    @classmethod
    def from_table(cls, table: ResultsTable, plot: PlotConfig | None = None,
                   geometry=None) -> "RunState":
        """
        The state of a finished run, from its results table.

        Fits (spectra and models) are not in a table, so ``latest_fits`` stays
        empty; the columns and ratios are complete.

        Parameters
        ----------
        table : ResultsTable
            E.g. ``ResultsTable.from_csv(run_dir / "results_long.csv")``.
            Rows are taken to be in input order, as ``collect_run`` writes them.
        plot : PlotConfig or None
            Ratio settings; no ratios if ``None``.
        geometry : optional
            The run's geometry, for the background columns (a table does not
            record the path length).  No backgrounds if ``None``.
        """
        plot = plot or PlotConfig()
        rows = table.targets()
        targets = list(dict.fromkeys(rows[COL_SPECIES]))
        df = table.df
        dual = [sp for sp in targets
                if ((df[COL_PARAM] == sp + DUAL_ATM_SUFFIX)
                    & df[COL_WINDOW].isin(rows.loc[rows[COL_SPECIES] == sp,
                                                   COL_WINDOW])).any()]
        state = cls(targets, ratios=plot.ratios, scroll=plot.scroll,
                    regression=plot.regression, geometry=geometry,
                    background=plot.background, rh=plot.rh, dual=dual)
        groups = rows.groupby(COL_SPECTRUM, sort=False)
        state.n_spectra = groups.ngroups
        for position, (sid, group) in enumerate(groups):
            state._add_rows(group, position, sid)
        return state

    @classmethod
    def from_run_dir(cls, run_dir: str | Path, plot: PlotConfig | None = None
                     ) -> "RunState":
        """
        The state of a finished run, from its folder, ready to inspect.

        Columns and ratios as :meth:`from_table`, with the run's own ``*PLOT``
        settings and geometry (from its ``config.ini``), plus every stored fit
        (``fits.npz``).  The first spectrum is selected; :meth:`select` moves
        to another.  Nothing is refitted.

        Parameters
        ----------
        run_dir : str or Path
            A folder written by a run (``plumeIR.output``).
        plot : PlotConfig or None
            Ratio settings to use instead of the run's own.

        Raises
        ------
        RunFolderError
            If the folder cannot be reopened, e.g. it has no ``fits.npz``
            (``plumeIR.output.load_run``).
        """
        from plumeIR.output import load_run

        run = load_run(run_dir)
        cfg = run.config
        state = cls.from_table(run.table, plot or cfg.get("plot") or PlotConfig(),
                               geometry=cfg["geometry"])
        windows = {w.label: w for w in cfg["windows"]}
        state.window_configs = {w: windows[w] for w in run.fits.windows
                                if w in windows}
        state.dual |= {g for w in state.window_configs.values()
                       for g in w.dual_gases if g in w.targets}
        state.window_labels = list(run.fits.windows)
        state.archive = run.fits
        if run.fits.has_covariance:
            for pair in state.ratios:
                w = state._shared_window(pair)
                if w is not None and w in run.fits.windows:
                    try:
                        state._rho[pair] = list(run.fits.correlation(w, *pair))
                    except KeyError:
                        pass
        state.run_dir = run.run_dir
        if len(run.fits):
            state.select(0)
        return state

    # ------------------------------------------------------------------
    # Inspection
    # ------------------------------------------------------------------

    @property
    def can_select(self) -> bool:
        """Whether every spectrum's fits are held (:meth:`from_run_dir`)."""
        return self.archive is not None

    def select(self, k: int) -> int:
        """
        Show the ``k``-th spectrum (input order): its fits become
        :attr:`latest_fits`, and it is the highlighted ratio point.

        Parameters
        ----------
        k : int
            Clipped to ``0 … n_received-1``.

        Returns
        -------
        int
            The index selected.

        Raises
        ------
        RuntimeError
            If the state holds no stored fits.
        """
        if self.archive is None:
            raise RuntimeError("This RunState holds no stored fits; build it "
                               "with RunState.from_run_dir to select spectra")
        k = int(np.clip(k, 0, len(self.archive) - 1))
        self.selected = k
        self.latest_fits = self.archive.fits(k)
        self.latest_id = self.archive.spectrum_ids[k]
        self.latest_position = int(self.archive.spectrum_index[k])
        name = self.archive.spec_types[k]
        self._spec_type = SpectrumType[name] if name in SpectrumType.__members__ else None
        return k

    @property
    def spec_type(self):
        """``SpectrumType`` of the spectrum whose fits are shown, or ``None``."""
        if self.selected is not None:
            return self._spec_type
        return getattr(self.latest_spectrum, "spec_type", None)

    def nearest(self, t) -> int:
        """
        Input-order index of the spectrum nearest in time (or number) to ``t``.

        Parameters
        ----------
        t : float or numpy.datetime64
            On the axis of :meth:`times`.
        """
        times = self.times()
        if np.issubdtype(times.dtype, np.datetime64):
            d = np.abs((times - np.datetime64(t, "ns")).astype("int64").astype(float))
        else:
            d = np.abs(times - float(t))
        return int(np.argmin(d))

    # ------------------------------------------------------------------
    # Feeding
    # ------------------------------------------------------------------

    def add(self, item) -> None:
        """
        Add one streamed result (a ``SpectrumFits``).

        Raises
        ------
        RuntimeError
            If the state was built without the run's windows and parameters
            (:meth:`from_table`).
        """
        if not self._windows:
            raise RuntimeError("This RunState has no windows or parameters; "
                               "build it with RunState.for_run to add results")
        fits = list(item.fits)
        rows = ResultsTable.from_fits(fits, self._windows, self._params,
                                      self._geometries).targets()
        sid = fits[0].spectrum_id if fits else str(item.index)
        self._add_rows(rows, item.index, sid, {fr.window_label: fr for fr in fits})
        for fr in fits:
            self.latest_fits[fr.window_label] = fr
        self.latest_spectrum = item.spectrum
        if item.n_spectra:
            self.n_spectra = item.n_spectra

    def _add_rows(self, rows: pd.DataFrame, position: int, sid: str,
                  fits: dict | None = None) -> None:
        """
        Record one spectrum's target rows; a failed fit records NaN.  With
        its fits (by window label), the error correlation of every ratio
        whose species share a window.
        """
        self._order.append(int(position))
        self._ids.append(str(sid))
        for sp in self.targets:
            sel = rows[rows[COL_SPECIES] == sp]
            if not sel.empty:
                self._window_of.setdefault(sp, str(sel[COL_WINDOW].iloc[0]))
            if sel.empty or not bool(sel[COL_SUCCESS].iloc[0]):
                self._scd[sp].append(np.nan)
                self._err[sp].append(np.nan)
                continue
            r = sel.iloc[0]
            self._scd[sp].append(float(r[COL_SCD]))
            self._err[sp].append(float(r[COL_SCD_ERR]))
            if r[COL_KIND] == "AERO_PLUME":
                self._is_aerosol[sp] = True
        for pair in self.ratios:
            w = self._shared_window(pair)
            fr = None if (w is None or fits is None) else fits.get(w)
            self._rho[pair].append(np.nan if fr is None else fit_correlation(fr, *pair))
        self.latest_id = str(sid)
        self.latest_position = int(position)

    def _shared_window(self, pair: tuple[str, str]) -> str | None:
        """The window both species of a ratio are fitted in, or ``None``."""
        wy, wx = self._window_of.get(pair[0]), self._window_of.get(pair[1])
        return wy if wy is not None and wy == wx else None

    # ------------------------------------------------------------------
    # Reading
    # ------------------------------------------------------------------

    @property
    def has_spectra(self) -> bool:
        """Whether fits (spectra, models) arrive: a live run, not a loaded table."""
        return bool(self._windows)

    @property
    def n_received(self) -> int:
        """Spectra received so far."""
        return len(self._order)

    def is_aerosol(self, species: str) -> bool:
        """Whether ``species`` is an aerosol (column in g m⁻², not molec cm⁻²)."""
        return self._is_aerosol[species]

    def is_mass_ratio(self, pair: tuple[str, str]) -> bool:
        """A ratio involving an aerosol is taken in mass units (as v2)."""
        return any(self._is_aerosol[sp] for sp in pair)

    def _sort(self) -> np.ndarray:
        """Indices that put the received spectra in input order (cached)."""
        if self._sorted is None or self._sorted[0] != self.n_received:
            self._sorted = (self.n_received,
                            np.argsort(np.asarray(self._order), kind="stable"))
        return self._sorted[1]

    def _take(self, values: list[float], last: int | None) -> np.ndarray:
        arr = np.asarray(values, dtype=float)[self._sort()]
        return arr if last is None else arr[-last:]

    def times(self, last: int | None = None) -> np.ndarray:
        """
        Time axis of the received spectra, in input order.

        ``datetime64`` when every spectrum id is a time stamp, else the
        1-based spectrum number.
        """
        ids = [self._ids[i] for i in self._sort()]
        if last is not None:
            ids = ids[-last:]
        t = pd.to_datetime(pd.Series(ids, dtype=object), errors="coerce")
        if len(t) and not t.isna().any():
            return t.to_numpy(dtype="datetime64[ns]")
        order = np.asarray(self._order)[self._sort()] + 1
        return (order if last is None else order[-last:]).astype(float)

    def column(self, species: str, last: int | None = None,
               mass: bool = False) -> tuple[np.ndarray, np.ndarray]:
        """
        One target's column and 1σ error, in input order.

        Parameters
        ----------
        species : str
            A target.
        last : int or None
            Only the latest ``last`` spectra (in input order).
        mass : bool
            Gas columns in g m⁻² instead of molec cm⁻² (aerosols always g m⁻²).

        Returns
        -------
        (np.ndarray, np.ndarray)
            Column and error; NaN where the fit failed.
        """
        scd = self._take(self._scd[species], last)
        err = self._take(self._err[species], last)
        if mass and not self._is_aerosol[species]:
            scd = molec_cm2_to_g_m2(scd, species)
            err = molec_cm2_to_g_m2(err, species)
        return scd, err

    def ratio_points(self, pair: tuple[str, str], last: int | None = None
                     ) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        """
        ``(x, y, sx, sy)`` of a ratio ``(y_species, x_species)``.

        Mass units when :meth:`is_mass_ratio`, otherwise molec cm⁻².
        """
        mass = self.is_mass_ratio(pair)
        y, sy = self.column(pair[0], last, mass)
        x, sx = self.column(pair[1], last, mass)
        return x, y, sx, sy

    def ratio_correlation(self, pair: tuple[str, str], last: int | None = None) -> np.ndarray:
        """
        Correlation of the errors of a ratio's two columns, in input order:
        from the fit covariance where both are fitted in one window, else 0.
        """
        rho = self._rho.get(tuple(pair))
        if rho is None or len(rho) != self.n_received:
            n = self.n_received if last is None else min(last, self.n_received)
            return np.zeros(n)
        r = np.nan_to_num(self._take(rho, last), nan=0.0)
        return np.clip(r, -MAX_CORRELATION, MAX_CORRELATION)

    def newest_point(self, pair: tuple[str, str]) -> tuple[float, float]:
        """
        The ratio's ``(x, y)`` for the spectrum that arrived last, or for the
        selected one when inspecting (:meth:`select`).
        """
        if not self._order:
            return np.nan, np.nan
        mass = self.is_mass_ratio(pair)
        i = len(self._order) - 1
        if self.selected is not None:
            i = int(self._sort()[self.selected])
        out = []
        for sp in (pair[1], pair[0]):
            v = self._scd[sp][i]
            if mass and not self._is_aerosol[sp]:
                v = float(molec_cm2_to_g_m2(v, sp))
            out.append(v)
        return out[0], out[1]

    def background_column(self, species: str, mass: bool = False) -> float:
        """
        Column the ambient air alone would give for ``species``, or NaN.

        The background VMR (``*PLOT background_<gas>``, else
        ``standards.BACKGROUND_VMR``; H2O from ``rh``) over the layer path at
        the ambient pressure and temperature.  NaN for gases without one, for
        aerosols, and outside the layer geometry (emission models its ambient
        gases as separate layers, so there is no single path).  Zero for a
        gas fitted in both roles (:attr:`dual`): its target is the plume part,
        the ambient part being a separate parameter.

        Parameters
        ----------
        species : str
        mass : bool
            In g m⁻² instead of molec cm⁻².
        """
        # One computation of the ambient column, shared with the composition
        # analysis.
        if not isinstance(self.geometry, LayerGeometry) or species in POSSIBLE_AEROSOLS:
            return np.nan
        if species in self.dual:
            return 0.0
        column = ambient_column(species, self.geometry, self.background_vmr, self.rh)
        if not np.isfinite(column):
            return np.nan
        return float(molec_cm2_to_g_m2(column, species)) if mass else column

    def ratio_background(self, pair: tuple[str, str]) -> tuple[float, float]:
        """Background ``(x, y)`` columns of a ratio, in its units; NaN where unknown."""
        mass = self.is_mass_ratio(pair)
        return (self.background_column(pair[1], mass),
                self.background_column(pair[0], mass))

    def ratio_fit(self, pair: tuple[str, str],
                  last: int | None = None) -> RatioFit | None:
        """
        The regression of one ratio, over the latest ``last`` spectra or all.

        Cached until the next spectrum arrives.
        """
        key = (tuple(pair), last)
        hit = self._fit_cache.get(key)
        if hit is not None and hit[0] == self.n_received:
            return hit[1]
        x, y, sx, sy = self.ratio_points(pair, last)
        fit = fit_ratio(x, y, sx, sy, self.regression, self.ratio_correlation(pair, last))
        self._fit_cache[key] = (self.n_received, fit)
        return fit

    def final_ratios(self) -> dict[tuple[str, str], RatioFit | None]:
        """Every configured ratio regressed over all spectra received."""
        return {pair: self.ratio_fit(pair) for pair in self.ratios}
