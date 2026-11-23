"""
Gas columns of one or more runs, arranged for composition analysis.

A :class:`CompositionTable` holds, per spectrum and per target gas, the
fitted slant column and its error (from the window where the gas is the
declared target), a background with its error, and a quality mask, with the
acquisition time, the run it came from and the contiguous acquisition
segment (a gap in time starts a new one, so no moving window spans a night).
Nothing here refits or reads spectra: it is built from a run's results table
and config, and several runs stack into one table (comparing runs and days).

Background, chosen per gas by class, overridable:

    none      no background: slope methods (a moving-window ratio's
              intercept takes any background constant within the window)
    zero      gases absent from ambient air (SO2, HCl, HF, ...) and the
              plume part of a gas fitted in both roles; the default for the
              emission geometry, whose ambient gases are separate layers
    ambient   background VMR × path × air density (layer): CO2, CO, CH4,
              and H2O when a relative humidity is given
    fitted    the run's own ambient parameter of a gas fitted in both roles
              (``{GAS}_atm``), per spectrum
    quantile  a low quantile of the gas over each acquisition segment, for
              data with plume-free moments

Defaults by class (developer, 2026-11-16): layer, ``ambient`` for gases
with a background VMR (``standards.BACKGROUND_VMR``, ``*PLOT
background_<gas>``, H2O with ``*PLOT rh``), ``zero`` otherwise and for the
plume part of a dual gas; emission, ``zero``; solar (total columns),
``none``.

The intercept of a ratio regression is not the background once a second
source contributes (a steady HCl-rich source shows as an intercept), which is
why the background here is a chosen input, never a fitted intercept.

Error correlation (2026-11-18): two species fitted
in one window have correlated errors, through the fit covariance (on the
emission validation run H2SO4_aq and ASH at a median −0.71).  Built with the run's stored fits
(``fits.npz`` format 2), the table carries that correlation per spectrum
(:meth:`CompositionTable.error_correlation`); species from different
windows, and runs stored without covariances, are independent (0).
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from plumeIR.config.objects import EmissionGeometry, LayerGeometry
from plumeIR import species as _species
from plumeIR.constants import POSSIBLE_AEROSOLS, POSSIBLE_GASES
from plumeIR.exceptions import PlumeIRError
from plumeIR.retrieval.results import (
    COL_PARAM, COL_R2, COL_SCD, COL_SCD_ERR, COL_SPECIES, COL_SPECTRUM, COL_SUCCESS,
    COL_TARGET, COL_WINDOW,
)
from plumeIR.standards import BACKGROUND_VMR
from plumeIR.units import molec_cm2_to_g_m2, rh_to_vmr, vmr_to_scd

logger = logging.getLogger(__name__)

BACKGROUND_STRATEGIES: tuple[str, ...] = ("none", "zero", "ambient", "fitted", "quantile")

#: Quantile and the fewest valid points per segment for ``quantile``.
BACKGROUND_QUANTILE, QUANTILE_MIN_POINTS = 0.05, 20

#: A time step longer than this many times the median step starts a new
#: acquisition segment (unless ``gap`` is given).
GAP_FACTOR = 10.0

#: Suffix of a fitted background in :attr:`CompositionTable.correlation`
#: keys: ``"CO2" + BACKGROUND_KEY`` is CO2's background (strategy
#: ``fitted``, the parameter ``CO2_atm``).
BACKGROUND_KEY = "|background"

#: An error correlation is clipped to ±this: York's line needs |ρ| < 1, and a
#: fit covariance can put two parameters at ±1 to round-off.
MAX_CORRELATION = 0.999


def ambient_column(species: str, geometry, background_vmr: dict | None = None,
                   rh: float | None = None) -> float:
    """
    Column the ambient air alone gives for ``species`` [molec cm⁻²], or NaN.

    The background VMR (``background_vmr``, else ``standards.BACKGROUND_VMR``;
    H2O from ``rh``) over the layer path at the ambient pressure and
    temperature.  NaN for aerosols, gases without a VMR, and outside the
    layer geometry (no single path).

    Parameters
    ----------
    species : str
    geometry : LayerGeometry or other
    background_vmr : dict or None
        ``*PLOT background_<gas>`` values [ppmv], over the standard ones.
    rh : float or None
        Relative humidity [%] for H2O.
    """
    if not isinstance(geometry, LayerGeometry) or species in POSSIBLE_AEROSOLS:
        return np.nan
    vmrs = {**BACKGROUND_VMR, **(background_vmr or {})}
    if species == "H2O" and "H2O" not in vmrs:
        if rh is None:
            return np.nan
        vmr = float(rh_to_vmr(rh, pres=geometry.atm_pres, temp=geometry.atm_temp))
    elif species in vmrs:
        vmr = float(vmrs[species])
    else:
        return np.nan
    return float(vmr_to_scd(vmr, geometry.pathlength, temp=geometry.atm_temp,
                            pres=geometry.atm_pres))


def molecule(species: str) -> str | None:
    """
    The molecule a species' column counts in a composition table: the gas
    itself; the acid of a solution aerosol (``H2SO4_aq`` → ``H2SO4``); H2O for
    a pure water or ice aerosol; ``None`` for a mass-only aerosol (ash, dust).
    """
    if not _species.is_aerosol(species):
        return species
    s = _species.get(species)
    if s.solute is not None:
        return s.solute.formula
    pure = [c.formula for c in s.components if c.fraction == 1.0]
    return pure[0] if pure else None


def is_mass_only(species: str) -> bool:
    """An aerosol without molecular content: its column is a mass [g m⁻²]."""
    return molecule(species) is None


def mass_factor(species: str) -> float:
    """
    g m⁻² per unit of the species' column in a composition table: the
    molecule's molar mass for a molecular column [molec cm⁻²] (gases, and an
    aerosol's acid or water); 1 for a mass-only aerosol (already g m⁻²).
    """
    m = molecule(species)
    return 1.0 if m is None else float(molec_cm2_to_g_m2(1.0, m))


def default_strategy(species: str, geometry, dual: set[str], background_vmr: dict | None,
                     rh: float | None) -> str:
    """The background strategy of ``species`` by gas class (module docstring)."""
    if species in POSSIBLE_AEROSOLS:      # no aerosol in ambient air at these columns
        return "zero" if isinstance(geometry, (LayerGeometry, EmissionGeometry)) else "none"
    if isinstance(geometry, LayerGeometry):
        if species in dual:
            return "zero"
        return "ambient" if np.isfinite(ambient_column(species, geometry, background_vmr, rh)) \
            else "zero"
    if isinstance(geometry, EmissionGeometry):
        return "zero"
    return "none"


@dataclass(frozen=True)
class CompositionTable:
    """
    Per-spectrum gas columns of one or more runs, with backgrounds.

    All frames are indexed by position ``0 … n−1`` in time order, with one
    column per gas.

    Attributes
    ----------
    time : pandas.DatetimeIndex
        Acquisition time of each spectrum (NaT where unknown).
    spectrum_id, run : np.ndarray of str
        Spectrum identifier and run label.
    segment : np.ndarray of int
        Contiguous acquisition segment: a new one at each run and at each gap.
    raw, error : pandas.DataFrame
        Fitted slant column and its 1σ error [molec cm⁻²].
    background, background_error : pandas.DataFrame
        Background and its error (NaN with strategy ``none``).
    valid : pandas.DataFrame of bool
        Quality mask: the fit converged, the value is finite, and R² is above
        the minimum asked for.
    strategy : dict
        Background strategy per gas.
    window : dict
        Target window per gas (provenance); per run when they differ.
    correlation : dict
        Per-spectrum correlation of the fit errors of two quantities fitted
        in the same window, keyed by the sorted pair: a species, or a
        species + :data:`BACKGROUND_KEY` for a fitted background.  A pair
        that is absent is independent.  Use :meth:`error_correlation`.
    """
    time:             pd.DatetimeIndex
    spectrum_id:      np.ndarray
    run:              np.ndarray
    segment:          np.ndarray
    raw:              pd.DataFrame = field(repr=False)
    error:            pd.DataFrame = field(repr=False)
    background:       pd.DataFrame = field(repr=False)
    background_error: pd.DataFrame = field(repr=False)
    valid:            pd.DataFrame = field(repr=False)
    strategy:         dict = field(default_factory=dict)
    window:           dict = field(default_factory=dict)
    correlation:      dict = field(default_factory=dict, repr=False)

    def __len__(self) -> int:
        return len(self.spectrum_id)

    @property
    def species(self) -> list[str]:
        """
        Every column: gases [molec cm⁻²]; aerosols with molecular content as
        their acid or water [molec cm⁻²] (:func:`molecule`); mass-only
        aerosols [g m⁻²].
        """
        return list(self.raw.columns)

    @property
    def gases(self) -> list[str]:
        """The gas columns [molec cm⁻²]: what a molar composition is made of."""
        return [g for g in self.raw.columns if g not in POSSIBLE_AEROSOLS]

    @property
    def aerosols(self) -> list[str]:
        """The aerosol columns (molecular or mass, :func:`is_mass_only`)."""
        return [g for g in self.raw.columns if g in POSSIBLE_AEROSOLS]

    def column(self, gas: str, excess: bool = False,
               mass: bool = False) -> tuple[np.ndarray, np.ndarray]:
        """
        ``(values, errors)`` of a species; with ``excess``, background removed
        and its error added in quadrature.  Invalid points are NaN.

        Parameters
        ----------
        gas : str
            Gas or aerosol.
        excess : bool
        mass : bool
            In g m⁻²: a molecular column (a gas, an aerosol's acid or water)
            converted with its molar mass; a mass-only aerosol's column is a
            mass already.

        Raises
        ------
        KeyError
            For a species not in the table.
        PlumeIRError
            For ``excess`` with strategy ``none``.
        """
        if gas not in self.raw:
            raise KeyError(f"{gas} is not in the table (species: {self.species})")
        v, e = self.raw[gas].to_numpy(float), self.error[gas].to_numpy(float)
        if excess:
            if self.strategy.get(gas) == "none":
                raise PlumeIRError(f"{gas} has no background (strategy 'none'): "
                                   f"choose one to take its excess")
            v = v - self.background[gas].to_numpy(float)
            be = self.background_error[gas].to_numpy(float)
            # a fitted background comes from the same fit as the column
            rho = self._rho(gas, gas + BACKGROUND_KEY)
            e = np.sqrt(np.maximum(e ** 2 + be ** 2 - 2.0 * rho * e * be, 0.0))
        if mass:
            k = mass_factor(gas)
            v, e = v * k, e * k
        bad = ~self.valid[gas].to_numpy(bool)
        return np.where(bad, np.nan, v), np.where(bad, np.nan, e)

    def _rho(self, a: str, b: str) -> np.ndarray:
        """Stored correlation of quantities ``a`` and ``b``; 0 where unknown."""
        r = self.correlation.get(tuple(sorted((a, b))))
        if r is None:
            return np.zeros(len(self))
        return np.nan_to_num(np.asarray(r, dtype=float), nan=0.0)

    def error_correlation(self, a: str, b: str, excess: bool = False) -> np.ndarray:
        """
        Correlation of the errors of species ``a`` and ``b``, per spectrum.

        Nonzero only for two species fitted in one window (from that fit's
        covariance), or, with ``excess``, where a fitted background shares a
        fit with the other species.  A unit conversion (``mass``) does not
        change it: the conversion is a positive factor.

        Parameters
        ----------
        a, b : str
            Species of the table.
        excess : bool
            Of the background-removed columns (:meth:`column`): a background
            from outside the fit is independent and dilutes the
            correlation; a fitted one enters with its own correlations.

        Returns
        -------
        np.ndarray
            Shape ``(n,)``, in ``[−MAX_CORRELATION, MAX_CORRELATION]``; 0
            where unknown or invalid.

        Raises
        ------
        KeyError
            For a species not in the table.
        """
        for g in (a, b):
            if g not in self.raw:
                raise KeyError(f"{g} is not in the table (species: {self.species})")
        if a == b:
            return np.full(len(self), MAX_CORRELATION)
        ea, eb = self.error[a].to_numpy(float), self.error[b].to_numpy(float)
        cov = self._rho(a, b) * ea * eb
        if excess:
            ka, kb = a + BACKGROUND_KEY, b + BACKGROUND_KEY
            ba = self.background_error[a].to_numpy(float)
            bb = self.background_error[b].to_numpy(float)
            # cov(a − Ba, b − Bb); an independent background adds nothing
            # but its variance, which column() puts in the totals
            with np.errstate(invalid="ignore"):
                cov = (cov - np.nan_to_num(self._rho(a, kb) * ea * bb)
                       - np.nan_to_num(self._rho(ka, b) * ba * eb)
                       + np.nan_to_num(self._rho(ka, kb) * ba * bb))
        _, sa = self.column(a, excess)
        _, sb = self.column(b, excess)
        with np.errstate(invalid="ignore", divide="ignore"):
            r = cov / (sa * sb)
        return np.clip(np.nan_to_num(r, nan=0.0, posinf=0.0, neginf=0.0),
                       -MAX_CORRELATION, MAX_CORRELATION)

    def seconds(self) -> np.ndarray:
        """Seconds since the first spectrum (NaN where the time is unknown)."""
        t = self.time
        if t.isna().all():
            return np.full(len(self), np.nan)
        return (t - t.min()).total_seconds().to_numpy(float)

    @staticmethod
    def concat(tables: list["CompositionTable"]) -> "CompositionTable":
        """
        Stack tables (several runs or days) in time order.

        Segments are renumbered so a run boundary always starts a new one.
        Gases missing from a run are NaN and invalid there.
        """
        if not tables:
            raise PlumeIRError("no tables to stack")
        frames = {}
        for name in ("raw", "error", "background", "background_error"):
            frames[name] = pd.concat([getattr(t, name) for t in tables], ignore_index=True)
        valid = pd.concat([t.valid for t in tables], ignore_index=True).fillna(False).astype(bool)
        time = pd.DatetimeIndex(np.concatenate([t.time.to_numpy() for t in tables]))
        sid = np.concatenate([t.spectrum_id for t in tables])
        run = np.concatenate([t.run for t in tables])
        seg, offset = [], 0
        for t in tables:
            seg.append(t.segment + offset)
            offset += int(t.segment.max()) + 1 if len(t) else 0
        segment = np.concatenate(seg)
        order = np.argsort(time.to_numpy(), kind="stable")
        keys = list(dict.fromkeys(k for t in tables for k in t.correlation))
        correlation = {k: np.concatenate([
            np.asarray(t.correlation[k], dtype=float) if k in t.correlation
            else np.zeros(len(t)) for t in tables])[order] for k in keys}
        strategy, window = {}, {}
        for t in tables:
            for g, s in t.strategy.items():
                if strategy.setdefault(g, s) != s:
                    logger.warning("%s: background strategy differs between runs "
                                   "(%s, %s); each run keeps its own", g, strategy[g], s)
            for g, w in t.window.items():
                window.setdefault(g, w)
        pick = lambda df: df.iloc[order].reset_index(drop=True)  # noqa: E731
        return CompositionTable(
            time=time[order], spectrum_id=sid[order], run=run[order], segment=segment[order],
            raw=pick(frames["raw"]), error=pick(frames["error"]),
            background=pick(frames["background"]),
            background_error=pick(frames["background_error"]),
            valid=valid.iloc[order].reset_index(drop=True), strategy=strategy, window=window,
            correlation=correlation)


def _segments(time: pd.DatetimeIndex, gap: float | None) -> np.ndarray:
    """Segment number per spectrum: a new segment after each gap [s]."""
    if len(time) < 2 or time.isna().any():
        return np.zeros(len(time), dtype=int)
    dt = np.diff(time.to_numpy()).astype("timedelta64[ns]").astype(float) / 1e9
    limit = gap if gap is not None else GAP_FACTOR * float(np.median(dt))
    return np.concatenate([[0], np.cumsum(dt > limit)]).astype(int)


def composition_table(
    results,
    config: dict,
    *,
    run: str = "",
    background: dict | None = None,
    min_r2: float | None = None,
    gap: float | None = None,
    fits=None,
) -> CompositionTable:
    """
    Build a :class:`CompositionTable` from a run's results.

    Parameters
    ----------
    results : ResultsTable
        The run's table (``results_long.csv``).
    config : dict
        The run's typed config objects (``prepare_run``'s setup as a dict, or
        ``load_run(...).config``): ``'geometry'``, ``'windows'`` (sequence or
        dict of ``FitWindowConfig``) and ``'plot'`` (background VMRs, rh).
    run : str
        Label of this run (e.g. the run folder's name).
    background : dict or None
        Strategy per gas overriding the class default (``BACKGROUND_STRATEGIES``).
    min_r2 : float or None
        Spectra whose target window fitted below this R² are invalid for that
        gas.
    gap : float or None
        Seconds between spectra that start a new segment; ``None`` uses
        ``GAP_FACTOR`` × the median step.
    fits : FitArchive or None
        The run's stored fits (``load_run(...).fits``), for the error
        correlation of species fitted in one window.  Without it, or for a
        run stored without covariances (``fits.npz`` format 1), every error
        is treated as independent, with a warning where two targets share a
        window.

    Returns
    -------
    CompositionTable

    Raises
    ------
    PlumeIRError
        For an unknown strategy, ``fitted`` for a gas without an ambient
        parameter, or ``ambient`` where no background VMR or path exists.
    """
    geometry = config["geometry"]
    windows = config["windows"]
    windows = list(windows.values()) if isinstance(windows, dict) else list(windows)
    plot = config.get("plot")
    bg_vmr = getattr(plot, "background", None) or {}
    rh = getattr(plot, "rh", None)
    dual = {g for w in windows for g in w.dual_gases if g in w.targets}

    df = results.df
    # Gases [molec cm⁻²] and aerosol mass columns [g m⁻²]; target parameters
    # such as plume_temp, and aerosol size / composition (r0_, wt_), are out.
    tg = df[df[COL_TARGET] & (df[COL_PARAM] == df[COL_SPECIES])
            & df[COL_SPECIES].isin(list(POSSIBLE_GASES) + list(POSSIBLE_AEROSOLS))]
    gases = list(dict.fromkeys(tg[COL_SPECIES]))
    ids = list(dict.fromkeys(df[COL_SPECTRUM].astype(str)))
    time = pd.to_datetime(pd.Index(ids), errors="coerce")
    order = np.argsort(time.to_numpy(), kind="stable") if not time.isna().any() \
        else np.arange(len(ids))
    ids = [ids[i] for i in order]
    time = pd.DatetimeIndex(time[order])

    def per_spectrum(rows: pd.DataFrame, col: str) -> np.ndarray:
        rows = rows.drop_duplicates(COL_SPECTRUM)
        return rows.set_index(rows[COL_SPECTRUM].astype(str))[col].reindex(ids).to_numpy()

    raw, err, valid, window = {}, {}, {}, {}
    components = results.aerosol_components()
    for g in gases:
        rows = tg[tg[COL_SPECIES] == g]
        window[g] = str(rows[COL_WINDOW].iloc[0])
        v = per_spectrum(rows, COL_SCD).astype(float)
        e = per_spectrum(rows, COL_SCD_ERR).astype(float)
        m = molecule(g)
        if _species.is_aerosol(g) and m is not None:
            # the aerosol's acid (or water) as a molecular column [molec cm⁻²]
            key = f"{g}[{m}]_molec_cm2"
            col = components.reindex(ids)
            v, e = col[key].to_numpy(float), col[f"{key}_err"].to_numpy(float)
        ok = per_spectrum(rows, COL_SUCCESS).astype(bool) & np.isfinite(v)
        if min_r2 is not None:
            ok &= np.nan_to_num(per_spectrum(rows, COL_R2).astype(float), nan=-np.inf) >= min_r2
        raw[g], err[g], valid[g] = v, e, ok

    segment = _segments(time, gap)
    bkg, bkg_err, strategy = {}, {}, {}
    for g in gases:
        s = (background or {}).get(g) or default_strategy(g, geometry, dual, bg_vmr, rh)
        if s not in BACKGROUND_STRATEGIES:
            raise PlumeIRError(f"{g}: unknown background strategy {s!r} "
                               f"(choose from {BACKGROUND_STRATEGIES})")
        strategy[g] = s
        b, be = np.full(len(ids), np.nan), np.full(len(ids), np.nan)
        if s == "zero":
            b[:], be[:] = 0.0, 0.0
        elif s == "ambient":
            col = ambient_column(g, geometry, bg_vmr, rh)
            if not np.isfinite(col):
                raise PlumeIRError(f"{g}: no ambient column (no background VMR, or not "
                                   f"the layer geometry): choose another strategy")
            b[:], be[:] = col, 0.0
        elif s == "fitted":
            rows = df[df[COL_PARAM] == f"{g}_atm"]
            if rows.empty:
                raise PlumeIRError(f"{g}: no fitted ambient part ({g}_atm): it is not "
                                   f"fitted in both roles")
            rows = rows[rows[COL_WINDOW] == window[g]] if (rows[COL_WINDOW] == window[g]).any() \
                else rows
            b = per_spectrum(rows, COL_SCD).astype(float)
            be = per_spectrum(rows, COL_SCD_ERR).astype(float)
        elif s == "quantile":
            for k in np.unique(segment):
                sel = (segment == k) & valid[g]
                if sel.sum() >= QUANTILE_MIN_POINTS:
                    q = np.quantile(raw[g][sel], BACKGROUND_QUANTILE)
                    lowest = sel & (raw[g] <= q)
                    b[segment == k] = q
                    be[segment == k] = (np.std(raw[g][lowest]) / np.sqrt(max(lowest.sum(), 1))
                                        if lowest.sum() > 1 else 0.0)
        bkg[g], bkg_err[g] = b, be

    correlation = _correlations(gases, window, strategy, ids, fits, label=run)
    frame = lambda d: pd.DataFrame(d, columns=gases)  # noqa: E731
    return CompositionTable(
        time=time, spectrum_id=np.asarray(ids, dtype=str),
        run=np.full(len(ids), run, dtype=object).astype(str), segment=segment,
        raw=frame(raw), error=frame(err), background=frame(bkg),
        background_error=frame(bkg_err), valid=frame(valid).astype(bool),
        strategy=strategy, window=window, correlation=correlation)


def _correlations(gases: list[str], window: dict, strategy: dict, ids: list[str],
                  fits, label: str = "") -> dict:
    """
    Error correlations of the quantities fitted in one window, per spectrum.

    Quantities are the species (parameter = its name) and fitted
    backgrounds (``{GAS}_atm``).  An aerosol's acid or water column is its
    mass column times a fixed fraction, so it keeps the mass column's
    correlation; where the weight fraction is fitted too this is an
    approximation (the product's correlation also depends on the weight
    fraction's).

    Returns
    -------
    dict
        ``{(a, b): (n,) array}`` for sorted pairs sharing a window, in the
        table's spectrum order (NaN where the fit has none).
    """
    quantities = [(g, window[g], g) for g in gases]
    quantities += [(g + BACKGROUND_KEY, window[g], f"{g}_atm") for g in gases
                   if strategy.get(g) == "fitted"]
    pairs = [(qa, qb) for i, qa in enumerate(quantities) for qb in quantities[i + 1:]
             if qa[1] == qb[1]]
    if not pairs:
        return {}
    if fits is None or not fits.has_covariance:
        shared = sorted({w for (_, w, _), _ in pairs})
        logger.warning("%s: no fit covariances (%s): errors of species fitted in "
                       "the same window (%s) are treated as independent", label or "run",
                       "no stored fits" if fits is None else "fits.npz format 1",
                       ", ".join(shared))
        return {}
    pos = {sid: i for i, sid in enumerate(fits.spectrum_ids)}
    take = np.asarray([pos.get(str(sid), -1) for sid in ids])
    out = {}
    for (ka, w, pa), (kb, _, pb) in pairs:
        try:
            r = fits.correlation(w, pa, pb)
        except KeyError as exc:
            logger.debug("no correlation for %s, %s: %s", ka, kb, exc)
            continue
        out[tuple(sorted((ka, kb)))] = np.where(take >= 0, r[np.maximum(take, 0)], np.nan)
    return out
