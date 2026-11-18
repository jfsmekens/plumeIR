"""
Gas ratios over moving windows and per run.

:func:`moving_ratio` fits, for every spectrum, the York line of one gas
against another over the spectra in a window around it, giving the ratio at
the acquisition rate: slope, its error (scaled by √MSWD when above 1, as
:func:`plumeIR.regression.york_fit` quotes it), intercept, MSWD and n.  The
window is a width in minutes or in spectra, **centred** (finished runs: the
best estimate, using later spectra) or **trailing** (live: only the past,
lagging a real change by half a width) — developer, 2026-11-16.  A window
never crosses a gap in the acquisition (:attr:`CompositionTable.segment`).

Each window also reports how much plume it spans in ``x``: the 10-90%
range of ``x`` over the median ``x`` error.  A window spanning little plume
determines its slope poorly whatever the formal error says; ``x_span_snr``
lets a plot or a filter show that.

Raw columns serve: a background constant within the window goes into the
intercept, so the slope is unchanged; ``excess=True`` uses the table's
backgrounds instead (the intercept then measures what the background did not
remove, e.g. a second, steady source).

:func:`run_ratios` gives one line per run (or acquisition segment), for
comparing runs and days; :func:`pooled_ratio` one slope per run from the
covariation within short stretches, robust to a background that drifts
slowly with the plume level; :func:`window_spread` the range of the moving
windows' slopes within each run.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field, replace

import numpy as np
import pandas as pd

from plumeIR.analysis.data import CompositionTable, is_mass_only
from plumeIR.exceptions import PlumeIRError
from plumeIR.regression import RegressionError, york_fit, york_fit_grouped

logger = logging.getLogger(__name__)

PLACEMENTS: tuple[str, ...] = ("centred", "trailing")
UNITS: tuple[str, ...] = ("minutes", "spectra")
#: Fewest valid points for a window's line.
MIN_POINTS = 8
#: Block width of the jackknife errors [minutes]: one block per window of
#: the analysis, as the all-gas fit uses.
JACKKNIFE_WIDTH = 2.0
#: Fewest blocks for a jackknife error.
MIN_BLOCKS = 5

COLUMNS = ("slope", "slope_err", "intercept", "intercept_err", "mswd", "n", "x_span",
           "x_span_snr")


@dataclass(frozen=True)
class MovingRatio:
    """
    A ratio ``y / x`` at each spectrum, from a moving window.

    Attributes
    ----------
    y, x : str
    width : float
    unit, placement : str
    excess : bool
        Background removed before fitting.
    mass : bool
        The ratio is in mass (g g⁻¹), not moles.
    table : pandas.DataFrame
        One row per spectrum of the composition table (same order), with
        ``time``, ``run``, ``segment`` and :data:`COLUMNS`; NaN where the
        window held fewer than ``min_points`` valid points or the line failed.
    """
    y:         str
    x:         str
    width:     float
    unit:      str
    placement: str
    excess:    bool
    table:     pd.DataFrame = field(repr=False)
    mass:      bool = False

    def select(self, mask: np.ndarray) -> "MovingRatio":
        """
        The rows where ``mask`` is true (one run, one segment), for plots
        that overlay runs; the windows are those already fitted.
        """
        mask = np.asarray(mask, dtype=bool)
        if mask.shape != (len(self.table),):
            raise PlumeIRError(f"mask has shape {mask.shape}, the table {len(self.table)} rows")
        return replace(self, table=self.table.loc[mask].reset_index(drop=True))


def _window_bounds(pos: np.ndarray, width: float, placement: str,
                   by_count: bool) -> tuple[np.ndarray, np.ndarray]:
    """Start and stop (exclusive) indices into ``pos`` of each point's window."""
    if by_count:
        n = int(round(width))
        idx = np.arange(pos.size)
        if placement == "centred":
            half = n // 2
            return np.maximum(idx - half, 0), np.minimum(idx + (n - half), pos.size)
        return np.maximum(idx - n + 1, 0), idx + 1
    if placement == "centred":
        return (np.searchsorted(pos, pos - width / 2, side="left"),
                np.searchsorted(pos, pos + width / 2, side="right"))
    return (np.searchsorted(pos, pos - width, side="right"),
            np.searchsorted(pos, pos, side="right"))


def _check_window(comp: CompositionTable, width: float, unit: str,
                  placement: str) -> tuple[bool, np.ndarray]:
    """Validate window arguments; ``(by_count, seconds)``."""
    if placement not in PLACEMENTS:
        raise PlumeIRError(f"placement must be one of {PLACEMENTS}, got {placement!r}")
    if unit not in UNITS:
        raise PlumeIRError(f"unit must be one of {UNITS}, got {unit!r}")
    if not width > 0:
        raise PlumeIRError(f"width must be positive, got {width}")
    by_count = unit == "spectra"
    seconds = comp.seconds()
    if not by_count and np.isnan(seconds).any():
        raise PlumeIRError("a window in minutes needs acquisition times: use unit='spectra'")
    return by_count, seconds


def window_indices(comp: CompositionTable, at: int, width: float, *,
                   unit: str = "minutes", placement: str = "centred") -> np.ndarray:
    """
    Positions of the spectra in the window around spectrum ``at``.

    The same window :func:`moving_ratio` fits for that spectrum, so a plot
    of one window shows exactly the points behind the curve.

    Parameters
    ----------
    comp : CompositionTable
    at : int
        Position of the spectrum in ``comp``.
    width, unit, placement
        As for :func:`moving_ratio`.

    Returns
    -------
    numpy.ndarray of int
        Positions into ``comp``, in time order, valid or not.

    Raises
    ------
    PlumeIRError
        For invalid window arguments or ``at`` out of range.
    """
    by_count, seconds = _check_window(comp, width, unit, placement)
    if not 0 <= at < len(comp):
        raise PlumeIRError(f"at must be in [0, {len(comp)}), got {at}")
    idx = np.flatnonzero(comp.segment == comp.segment[at])
    pos = idx.astype(float) if by_count else seconds[idx]
    lo, hi = _window_bounds(pos, width if by_count else 60.0 * width, placement, by_count)
    j = int(np.searchsorted(idx, at))
    return idx[lo[j]:hi[j]]


def _error_correlation(comp: CompositionTable, y: str, x: str, excess: bool,
                       r: float | None) -> np.ndarray:
    """
    Per-point correlation of the errors of ``x`` and ``y``: from the table
    (:meth:`CompositionTable.error_correlation`) when ``r`` is ``None``,
    else ``r`` everywhere.
    """
    if r is None:
        return comp.error_correlation(x, y, excess)
    return np.full(len(comp), float(r))


def _line(x, y, sx, sy, r, min_points) -> dict:
    ok = np.isfinite(x) & np.isfinite(y) & (sx > 0) & (sy > 0)
    out = dict.fromkeys(COLUMNS, np.nan)
    out["n"] = int(ok.sum())
    if ok.sum() < min_points:
        return out
    x, y, sx, sy, r = x[ok], y[ok], sx[ok], sy[ok], np.broadcast_to(r, ok.shape)[ok]
    span = float(np.percentile(x, 90) - np.percentile(x, 10))
    out["x_span"] = span
    out["x_span_snr"] = span / float(np.median(sx))
    try:
        f = york_fit(x, y, sx, sy, r)
    except RegressionError:
        return out
    out.update(slope=f.slope, slope_err=f.slope_err, intercept=f.intercept,
               intercept_err=f.intercept_err, mswd=f.mswd)
    return out


def mass_ratio(comp: CompositionTable, y: str, x: str) -> bool:
    """
    Whether the ratio ``y / x`` is taken in mass (g g⁻¹) rather than moles:
    when either is a mass-only aerosol (ash, dust), which has no moles.  An
    acid or water aerosol counts its acid or water molecules
    (``data.molecule``), so its ratio to a gas is molar.
    """
    return is_mass_only(y) or is_mass_only(x)


def moving_ratio(
    comp: CompositionTable,
    y: str,
    x: str,
    width: float,
    *,
    unit: str = "minutes",
    placement: str = "centred",
    excess: bool = False,
    min_points: int = MIN_POINTS,
    r: float | None = None,
    mass: bool | None = None,
) -> MovingRatio:
    """
    The York line of ``y`` against ``x`` in a window around every spectrum.

    Parameters
    ----------
    comp : CompositionTable
    y, x : str
        Gases (``y / x`` is the ratio).
    width : float
        Window width, in ``unit``.
    unit : {'minutes', 'spectra'}
    placement : {'centred', 'trailing'}
        Centred for finished runs, trailing for live processing.
    excess : bool
        Fit background-removed columns (the table's strategies).
    min_points : int
        Fewest valid points for a line.
    r : float or None
        Correlation of the two columns' errors.  ``None``: per spectrum from
        the table (:meth:`CompositionTable.error_correlation`), nonzero for
        species fitted in one window; a number overrides it everywhere
        (0 treats the errors as independent).
    mass : bool or None
        Take the ratio in mass (g g⁻¹, gases converted with their molar
        mass) instead of moles; ``None``: in mass when either species is an
        aerosol (:func:`mass_ratio`).

    Returns
    -------
    MovingRatio

    Raises
    ------
    PlumeIRError
        For an unknown unit or placement, a non-positive width, or a time
        window when the table has no times.
    """
    by_count, seconds = _check_window(comp, width, unit, placement)
    mass = mass_ratio(comp, y, x) if mass is None else mass
    xv, sx = comp.column(x, excess, mass)
    yv, sy = comp.column(y, excess, mass)
    rv = _error_correlation(comp, y, x, excess, r)
    rows = [None] * len(comp)
    for k in np.unique(comp.segment):
        idx = np.flatnonzero(comp.segment == k)
        pos = idx.astype(float) if by_count else seconds[idx]
        lo, hi = _window_bounds(pos, width if by_count else 60.0 * width, placement, by_count)
        for j, i in enumerate(idx):
            w = idx[lo[j]:hi[j]]
            rows[i] = _line(xv[w], yv[w], sx[w], sy[w], rv[w], min_points)
    table = pd.DataFrame(rows, columns=list(COLUMNS))
    table.insert(0, "segment", comp.segment)
    table.insert(0, "run", comp.run)
    table.insert(0, "time", comp.time)
    table.insert(0, "spectrum_id", comp.spectrum_id)
    return MovingRatio(y=y, x=x, width=float(width), unit=unit, placement=placement, mass=mass,
                       excess=excess, table=table)


def _block_jackknife(slope_without, labels: np.ndarray) -> tuple[float, int]:
    """
    Delete-one-block jackknife error of a slope.

    ``slope_without(mask)`` refits with the points where ``mask`` is True
    left out; ``labels`` holds each point's block.  NaN with fewer than
    :data:`MIN_BLOCKS` blocks or when a refit fails.
    """
    blocks = np.unique(labels)
    if blocks.size < MIN_BLOCKS:
        return np.nan, int(blocks.size)
    reps = []
    for b in blocks:
        try:
            reps.append(slope_without(labels == b))
        except RegressionError:
            return np.nan, int(blocks.size)
    reps = np.asarray(reps, float)
    n = reps.size
    return float(np.sqrt((n - 1) / n * np.sum((reps - reps.mean()) ** 2))), int(n)


def quoted_error(table: pd.DataFrame) -> np.ndarray:
    """
    The error to quote for each line of a :func:`run_ratios` or
    :func:`pooled_ratio` table: the jackknife where it exists, else York's.
    """
    york = table["slope_err"].to_numpy(float)
    if "slope_err_jk" not in table:
        return york
    jk = table["slope_err_jk"].to_numpy(float)
    return np.where(np.isfinite(jk), jk, york)


def run_ratios(comp: CompositionTable, y: str, x: str, *, by: str = "run",
               excess: bool = False, min_points: int = MIN_POINTS,
               r: float | None = None, jackknife_width: float | None = JACKKNIFE_WIDTH,
               unit: str = "minutes", mass: bool | None = None) -> pd.DataFrame:
    """
    One York line of ``y`` against ``x`` per run (or acquisition segment).

    The York error (``slope_err``, √MSWD-scaled above 1) assumes one
    composition over the run and independent points.  ``slope_err_jk`` is a
    delete-one-block jackknife over consecutive blocks of
    ``jackknife_width``: it also carries slow variation between stretches
    of the run and correlation between neighbouring spectra, and is the
    error to quote.  Measured on the layer validation run (2026-11-16):
    1.4-1.9× the York error, where noise
    correlated at the observed lag-1 0.35 would give 1.07×; on synthetic
    data both are calibrated (error / scatter 0.94-1.00).

    Parameters
    ----------
    by : {'run', 'segment'}
    jackknife_width : float or None
        Block width in ``unit``; ``None`` for no jackknife (NaN).
    unit : {'minutes', 'spectra'}
    r : float or None
        As for :func:`moving_ratio`.
    mass : bool or None
        As for :func:`moving_ratio`.

    Returns
    -------
    pandas.DataFrame
        Indexed by run label (or segment), with ``start``, ``stop``,
        :data:`COLUMNS`, ``slope_err_jk`` and ``n_blocks``.
    """
    if by not in ("run", "segment"):
        raise PlumeIRError(f"by must be 'run' or 'segment', got {by!r}")
    key = comp.run if by == "run" else comp.segment
    mass = mass_ratio(comp, y, x) if mass is None else mass
    xv, sx = comp.column(x, excess, mass)
    yv, sy = comp.column(y, excess, mass)
    rv = _error_correlation(comp, y, x, excess, r)
    ok = np.isfinite(xv) & np.isfinite(yv) & (sx > 0) & (sy > 0)
    blocks = window_labels(comp, jackknife_width, unit=unit) if jackknife_width else None
    out = {}
    for k in pd.unique(key):
        sel = key == k
        row = _line(xv[sel], yv[sel], sx[sel], sy[sel], rv[sel], min_points)
        row.update(slope_err_jk=np.nan, n_blocks=0)
        use = sel & ok
        if blocks is not None and np.isfinite(row["slope"]):
            xs, ys, sxs, sys_, rs, lab = (xv[use], yv[use], sx[use], sy[use], rv[use],
                                          blocks[use])
            row["slope_err_jk"], row["n_blocks"] = _block_jackknife(
                lambda drop: york_fit(xs[~drop], ys[~drop], sxs[~drop], sys_[~drop],
                                      rs[~drop]).slope, lab)
        t = comp.time[sel]
        row.update(start=t.min(), stop=t.max())
        out[k] = row
    df = pd.DataFrame.from_dict(out, orient="index")
    df.index.name = by
    return df[["start", "stop", *COLUMNS, "slope_err_jk", "n_blocks"]]


def window_spread(mr: MovingRatio, *, by: str = "run",
                  quantiles: tuple[float, float] = (0.05, 0.95)) -> pd.DataFrame:
    """
    The spread of the moving-window slopes within each run (or segment).

    A run's single York line hides how much its composition moved: on the
    layer validation run HCl/SO2 is 0.0358 ± 0.0005 over the run while its
    1-minute windows range over 0.029-0.040 (5-95%).  This gives that range
    beside :func:`run_ratios`, for comparing runs whatever their length.

    Parameters
    ----------
    mr : MovingRatio
        Its spread depends on its width: narrower windows follow faster
        changes and carry more noise.
    by : {'run', 'segment'}
    quantiles : (float, float)
        Lower and upper quantiles of the window slopes.

    Returns
    -------
    pandas.DataFrame
        Indexed like :func:`run_ratios` (run label or segment), with
        ``median``, ``lower``, ``upper`` (the quantiles) and ``n_windows``
        (windows with a slope).  NaN where a run has no window with a slope.

    Raises
    ------
    PlumeIRError
        For an unknown ``by`` or quantiles outside [0, 1] or out of order.
    """
    if by not in ("run", "segment"):
        raise PlumeIRError(f"by must be 'run' or 'segment', got {by!r}")
    lo, hi = quantiles
    if not 0.0 <= lo <= hi <= 1.0:
        raise PlumeIRError(f"quantiles must satisfy 0 <= lower <= upper <= 1, got {quantiles}")
    t = mr.table
    out = {}
    for k in pd.unique(t[by]):
        s = t.loc[t[by] == k, "slope"].dropna()
        out[k] = dict(median=s.median() if len(s) else np.nan,
                      lower=s.quantile(lo) if len(s) else np.nan,
                      upper=s.quantile(hi) if len(s) else np.nan,
                      n_windows=len(s))
    df = pd.DataFrame.from_dict(out, orient="index")
    df.index.name = by
    return df


def window_labels(comp: CompositionTable, width: float, *, unit: str = "minutes") -> np.ndarray:
    """
    Label of the consecutive, non-overlapping window each spectrum falls in.

    Windows of ``width`` are anchored at the start of each acquisition
    segment and never cross one; the label is unique across the table.

    Parameters
    ----------
    comp : CompositionTable
    width : float
    unit : {'minutes', 'spectra'}

    Returns
    -------
    numpy.ndarray of int64
        One label per spectrum.

    Raises
    ------
    PlumeIRError
        For invalid window arguments.
    """
    by_count, seconds = _check_window(comp, width, unit, "centred")
    tile = np.full(len(comp), -1, dtype=np.int64)
    for k in np.unique(comp.segment):
        idx = np.flatnonzero(comp.segment == k)
        pos = np.arange(idx.size, dtype=float) if by_count else seconds[idx] - seconds[idx[0]]
        step = width if by_count else 60.0 * width
        tile[idx] = int(k) * 10**9 + np.floor(pos / step).astype(np.int64)
    return tile


POOLED_COLUMNS = ("slope", "slope_err", "slope_err_internal", "slope_err_jk", "mswd", "n",
                  "n_windows")


def pooled_ratio(
    comp: CompositionTable,
    y: str,
    x: str,
    width: float,
    *,
    unit: str = "minutes",
    by: str = "run",
    excess: bool = False,
    min_points: int = MIN_POINTS,
    r: float | None = None,
    mass: bool | None = None,
) -> pd.DataFrame:
    """
    One ratio per run (or segment) from the covariation within short windows.

    The run is cut into consecutive, non-overlapping windows of ``width``
    (anchored at the start of each acquisition segment, never crossing one);
    one slope is fitted through all of them with an intercept per window
    (:func:`plumeIR.regression.york_fit_grouped`).  A background that changes
    between windows goes into the intercepts instead of the slope.

    Why (measured on the layer validation run):
    the window intercepts of every gas rise over the run together with the
    mean SO2, so a single line over the run (:func:`run_ratios`) is steeper
    than the composition inside any stretch: HCl/SO2 0.03576 against
    0.0346-0.0352 pooled (1-5 minute windows), CO/SO2 0.00516 against
    0.00488-0.00500.  Pooling is the estimator for the composition of the
    varying part of the plume; it needs the background to be steady only
    within a window.

    Parameters
    ----------
    comp : CompositionTable
    y, x : str
        Gases (``y / x`` is the ratio).
    width : float
        Window width in ``unit``: long enough to hold a range of plume
        levels, short enough that the background holds still.
    unit : {'minutes', 'spectra'}
    by : {'run', 'segment'}
    excess : bool
        Fit background-removed columns.  A background constant within each
        window changes nothing (it goes into the intercepts).
    min_points : int
        Fewest valid points for a window to be used; smaller windows are
        left out.
    r : float or None
        As for :func:`moving_ratio`.
    mass : bool or None
        As for :func:`moving_ratio`.

    Returns
    -------
    pandas.DataFrame
        Indexed like :func:`run_ratios`, with ``start``, ``stop`` and
        :data:`POOLED_COLUMNS` (``n`` points and ``n_windows`` windows used;
        ``slope_err_jk`` a delete-one-window jackknife, the error to quote,
        as for :func:`run_ratios`; NaN below :data:`MIN_BLOCKS` windows);
        NaN slope where fewer than two windows qualify or the fit fails.

    Raises
    ------
    PlumeIRError
        For invalid window arguments or ``by``.
    """
    if by not in ("run", "segment"):
        raise PlumeIRError(f"by must be 'run' or 'segment', got {by!r}")
    mass = mass_ratio(comp, y, x) if mass is None else mass
    xv, sx = comp.column(x, excess, mass)
    yv, sy = comp.column(y, excess, mass)
    rv = _error_correlation(comp, y, x, excess, r)
    ok = np.isfinite(xv) & np.isfinite(yv) & (sx > 0) & (sy > 0)
    tile = window_labels(comp, width, unit=unit)
    key = comp.run if by == "run" else comp.segment
    out = {}
    for k in pd.unique(key):
        sel = (key == k) & ok
        labels, counts = np.unique(tile[sel], return_counts=True)
        keep = np.isin(tile, labels[counts >= min_points]) & sel
        row = dict.fromkeys(POOLED_COLUMNS, np.nan)
        row.update(n=int(keep.sum()), n_windows=int(np.sum(counts >= min_points)))
        if row["n_windows"] >= 2:
            xs, ys, sxs, sys_, rs, lab = (xv[keep], yv[keep], sx[keep], sy[keep], rv[keep],
                                          tile[keep])
            try:
                f = york_fit_grouped(xs, ys, sxs, sys_, lab, rs)
                row.update(slope=f.slope, slope_err=f.slope_err,
                           slope_err_internal=f.slope_err_internal, mswd=f.mswd)
                row["slope_err_jk"], _ = _block_jackknife(
                    lambda drop: york_fit_grouped(xs[~drop], ys[~drop], sxs[~drop],
                                                  sys_[~drop], lab[~drop], rs[~drop]).slope, lab)
            except RegressionError as exc:
                logger.warning("pooled %s/%s for %s %s: %s", y, x, by, k, exc)
        t = comp.time[key == k]
        row.update(start=t.min(), stop=t.max())
        out[k] = row
    df = pd.DataFrame.from_dict(out, orient="index")
    df.index.name = by
    return df[["start", "stop", *POOLED_COLUMNS]]
