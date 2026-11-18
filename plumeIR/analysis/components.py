"""
Plume composition from every species at once, with no reference gas.

A ratio to SO2 treats SO2 as a clean reference.  Here every species is
treated alike: a composition table's columns are fitted by a weighted
rank-k model (:func:`plumeIR.analysis.mlpca.fit_components`), each
spectrum's vector being offsets plus ``Σ_k t_ik · v_k``: ``v_k`` a source's
composition, ``t_ik`` how much of it spectrum ``i`` holds.  Offsets per
window (``intercept='window'``, a background that drifts slowly, as on the
validation run — §5, "Window slopes against the run line"), per run or
segment (``'group'``), or none (``'none'``, background-removed columns).
Errors are weighted with their correlation between species fitted in one
spectral window (``CompositionTable.error_correlation``; on the emission
run H2SO4_aq and ASH at a median −0.71).

Rank 1 gives the composition of the single source; its ratios can be quoted
against any gas, and no gas's errors are privileged.  For rank ≥ 2 the
sources are determined only up to a rotation within the fitted subspace
(the factorisation with non-negativity that resolves it is §4.3); what
rank ≥ 2 gives here is how much of the scatter a further source explains
(:func:`rank_table`).

Errors: a block jackknife over windows (or over contiguous blocks), which
carries the excess scatter (MSWD ≫ 1 on real data) and the correlation
between neighbouring spectra that formal errors miss.

:func:`moving_composition` gives the rank-1 composition in a window around
every spectrum, the multi-gas form of
:func:`~plumeIR.analysis.ratios.moving_ratio`.
"""

from __future__ import annotations

import logging
import warnings
from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from plumeIR.analysis.data import CompositionTable, is_mass_only, mass_factor
from plumeIR.analysis.mlpca import INTERCEPTS, ComponentFit, fit_components, fit_offsets
from plumeIR.analysis.ratios import (
    MIN_POINTS, PLACEMENTS, _check_window, _window_bounds, window_labels,
)
from plumeIR.constants import POSSIBLE_AEROSOLS
from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Re-exported: the engine lives in :mod:`plumeIR.analysis.mlpca`.
__all__ = ["INTERCEPTS", "ComponentFit", "Composition", "MovingComposition", "composition",
           "fit_components", "moving_composition", "normalised_residuals", "rank_table"]

#: Contiguous blocks for the jackknife when the offsets are not per window.
JACKKNIFE_BLOCKS = 20

# =============================================================================
# On a composition table
# =============================================================================

def _jackknife_err(reps: np.ndarray) -> np.ndarray:
    """Block-jackknife standard error over the first axis of ``reps``."""
    b = reps.shape[0]
    return np.sqrt((b - 1) / b * np.sum((reps - reps.mean(axis=0)) ** 2, axis=0))


def _derive(loadings: np.ndarray, species: list[str], r: int
            ) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Ratios, molar fractions and mass fractions from rank-1 loadings.

    ``loadings`` (last axis one per species) are in each column's own unit:
    molec cm⁻² per unit score for a gas and for an aerosol's acid or water,
    g m⁻² for a mass-only aerosol.  Ratios to the reference gas ``r`` are
    molar (mol mol⁻¹), by mass (g g⁻¹) only for a mass-only aerosol.  Molar
    fractions are over the gases only (NaN for every aerosol: another
    phase); mass fractions over every species, an acid aerosol by its acid
    mass.
    """
    k = np.array([mass_factor(g) for g in species])
    aero = np.array([g in POSSIBLE_AEROSOLS for g in species])
    mass_only = np.array([is_mass_only(g) for g in species])
    m = loadings * k
    ref, ref_m = loadings[..., r:r + 1], m[..., r:r + 1]
    with np.errstate(invalid="ignore", divide="ignore"):
        ratio = np.where(mass_only, m / ref_m, loadings / ref)
        gas = np.where(aero, 0.0, loadings)
        frac = np.where(aero, np.nan, gas / np.sum(gas, axis=-1, keepdims=True))
        mfrac = m / np.sum(m, axis=-1, keepdims=True)
    return ratio, frac, mfrac


def _check_species(comp: CompositionTable, species: list[str] | None,
                   reference: str | None) -> tuple[list[str], str]:
    """The species of a composition (every one by default) and its reference gas."""
    species = list(comp.species) if species is None else list(species)
    if len(species) < 2:
        raise PlumeIRError(f"a composition needs at least two species, got {species}")
    gases = [g for g in species if g not in POSSIBLE_AEROSOLS]
    if reference is None:
        reference = "SO2" if "SO2" in gases else (gases[0] if gases else None)
    if reference not in gases:
        raise PlumeIRError(f"reference {reference!r} must be a gas among {species}")
    return species, reference


def _matrix(comp: CompositionTable, gases: list[str], excess: bool) -> tuple[np.ndarray, np.ndarray]:
    cols = [comp.column(gas, excess) for gas in gases]
    return np.column_stack([c[0] for c in cols]), np.column_stack([c[1] for c in cols])


def _corr_matrix(comp: CompositionTable, gases: list[str], excess: bool) -> np.ndarray | None:
    """
    Per-spectrum error correlation between the gases, ``(n, p, p)``, from
    the table (:meth:`CompositionTable.error_correlation`); ``None`` when
    every pair is independent.
    """
    p = len(gases)
    C = np.broadcast_to(np.eye(p), (len(comp), p, p)).copy()
    any_pair = False
    for a in range(p):
        for b in range(a + 1, p):
            r = comp.error_correlation(gases[a], gases[b], excess)
            if np.any(r):
                C[:, a, b] = C[:, b, a] = r
                any_pair = True
    return C if any_pair else None


def _groups(comp: CompositionTable, intercept: str, width: float, unit: str,
            by: str) -> np.ndarray | None:
    if intercept == "window":
        return window_labels(comp, width, unit=unit)
    if intercept == "group":
        key = comp.run if by == "run" else comp.segment
        return pd.factorize(key)[0]
    return None


def _thin_windows(groups: np.ndarray | None, X: np.ndarray, min_points: int) -> np.ndarray:
    """Rows whose window holds at least ``min_points`` spectra with every gas valid."""
    ok = np.isfinite(X).all(axis=1)
    if groups is None:
        return ok
    labels, counts = np.unique(groups[ok], return_counts=True)
    return ok & np.isin(groups, labels[counts >= min_points])


@dataclass(frozen=True)
class Composition:
    """
    Single-source composition of a table, from a rank-1 fit, with
    jackknife errors.

    Attributes
    ----------
    reference : str
        Gas the ratios are quoted against (presentation only: the fit
        treats every gas alike).
    ratios : pandas.DataFrame
        Indexed by gas: ``ratio`` (to ``reference``), ``ratio_err``
        (block jackknife), ``fraction`` (molar fraction of the gases
        considered; they sum to 1), ``fraction_err`` (block jackknife of the
        fractions themselves, which are correlated through their sum, so
        not propagated from the ratios), ``n_blocks``.
    fit : ComponentFit
    """
    reference: str
    ratios:    pd.DataFrame
    fit:       ComponentFit = field(repr=False)


def composition(
    comp: CompositionTable,
    gases: list[str] | None = None,
    *,
    reference: str | None = None,
    intercept: str = "window",
    width: float = 2.0,
    unit: str = "minutes",
    by: str = "run",
    excess: bool = False,
    min_points: int = MIN_POINTS,
    jackknife: bool = True,
) -> Composition:
    """
    The composition of a single source, from every gas at once.

    Parameters
    ----------
    comp : CompositionTable
    gases : list of str or None
        Every gas of the table when ``None``.
    reference : str or None
        Gas to quote ratios against; SO2 when present, else the first gas.
    intercept : {'window', 'group', 'none'}
        Offsets per window of ``width`` (a background that drifts slowly;
        the multi-gas form of :func:`~plumeIR.analysis.ratios.pooled_ratio`),
        per run or segment (``by``; the form of a single York line), or
        none (``excess`` columns, a line through the background).
    width, unit
        Windows for ``intercept='window'``.
    by : {'run', 'segment'}
        Groups for ``intercept='group'``.
    excess : bool
        Background-removed columns; with ``intercept='none'`` it should be
        True.
    min_points : int
        Windows with fewer spectra (every gas valid) are left out.
    jackknife : bool
        Compute the block-jackknife errors (a fit per block).

    Returns
    -------
    Composition

    Raises
    ------
    PlumeIRError
        For fewer than two gases, an unknown reference, or a failed fit.
    """
    gases, reference = _check_species(comp, gases, reference)
    if by not in ("run", "segment"):
        raise PlumeIRError(f"by must be 'run' or 'segment', got {by!r}")
    X, E = _matrix(comp, gases, excess)
    C = _corr_matrix(comp, gases, excess)
    groups = _groups(comp, intercept, width, unit, by)
    keep = _thin_windows(groups, X, min_points)
    X, E = np.where(keep[:, None], X, np.nan), np.where(keep[:, None], E, np.nan)
    fit = fit_components(X, E, 1, groups=groups, gases=gases, intercept=intercept, corr=C)
    r = gases.index(reference)
    ratio, frac, mfrac = _derive(fit.loadings[:, 0], gases, r)
    err = np.full(len(gases), np.nan)
    frac_err = np.full(len(gases), np.nan)
    mfrac_err = np.full(len(gases), np.nan)
    n_blocks = 0
    if jackknife:
        if intercept == "window":
            blocks = np.where(keep, groups, -1)
        else:
            idx = np.flatnonzero(keep)
            blocks = np.full(len(comp), -1)
            blocks[idx] = np.arange(idx.size) * JACKKNIFE_BLOCKS // max(idx.size, 1)
        labels = np.unique(blocks[blocks >= 0])
        n_blocks = labels.size
        if n_blocks >= 3:
            reps = []
            for lab in labels:
                drop = blocks == lab
                f = fit_components(np.where(drop[:, None], np.nan, X), E, 1, groups=groups,
                                   gases=gases, intercept=intercept, init=fit.loadings, corr=C)
                reps.append(f.loadings[:, 0])
            r_rep, f_rep, m_rep = _derive(np.array(reps), gases, r)
            err, frac_err, mfrac_err = (_jackknife_err(r_rep), _jackknife_err(f_rep),
                                        _jackknife_err(m_rep))
    table = pd.DataFrame({"ratio": ratio, "ratio_err": err,
                          "unit": ["g/g" if is_mass_only(g) else "mol/mol" for g in gases],
                          "fraction": frac, "fraction_err": frac_err,
                          "mass_fraction": mfrac, "mass_fraction_err": mfrac_err,
                          "n_blocks": n_blocks},
                         index=pd.Index(gases, name="gas"))
    return Composition(reference=reference, ratios=table, fit=fit)


def rank_table(
    comp: CompositionTable,
    gases: list[str] | None = None,
    *,
    intercept: str = "window",
    width: float = 2.0,
    unit: str = "minutes",
    by: str = "run",
    excess: bool = False,
    min_points: int = MIN_POINTS,
) -> pd.DataFrame:
    """
    How much of the scatter each further component explains.

    For every rank from 1 to ``n_gases − 1``: ``chi2``, ``dof``,
    ``reduced_chi2`` and ``delta_chi2`` (the drop from the rank below; for
    rank 1, from the offsets alone, rank 0).  Under pure noise a further
    component lowers ``chi2`` by about its added parameters
    (``delta_dof``); a drop far larger says the data hold another source,
    or a systematic error with a direction of its own.  With errors that
    understate the scatter (MSWD ≫ 1) compare drops relative to each other,
    not to 1.

    Parameters
    ----------
    As for :func:`composition`.

    Returns
    -------
    pandas.DataFrame
        Indexed by rank (0 … n_gases − 1).
    """
    gases = list(comp.species) if gases is None else list(gases)
    X, E = _matrix(comp, gases, excess)
    C = _corr_matrix(comp, gases, excess)
    groups = _groups(comp, intercept, width, unit, by)
    keep = _thin_windows(groups, X, min_points)
    X, E = np.where(keep[:, None], X, np.nan), np.where(keep[:, None], E, np.nan)
    rows = {}
    S0, dof0 = fit_offsets(X, E, groups=groups, intercept=intercept, corr=C)
    rows[0] = dict(chi2=S0, dof=dof0)
    for k in range(1, len(gases)):
        f = fit_components(X, E, k, groups=groups, gases=gases, intercept=intercept, corr=C)
        rows[k] = dict(chi2=f.chi2, dof=f.dof)
    df = pd.DataFrame.from_dict(rows, orient="index")
    df.index.name = "rank"
    df["reduced_chi2"] = df.chi2 / df.dof
    df["delta_chi2"] = -df.chi2.diff()
    df["delta_dof"] = -df.dof.diff()
    return df


#: Contiguous blocks for the jackknife inside each moving window.
WINDOW_JACKKNIFE_BLOCKS = 8


@dataclass(frozen=True)
class MovingComposition:
    """
    Rank-1 composition in a moving window around every spectrum.

    Attributes
    ----------
    gases : list of str
    reference : str
    width : float
    unit, placement : str
    excess : bool
    table : pandas.DataFrame
        One row per spectrum of the composition table (same order):
        ``time``, ``run``, ``segment``, ``n`` (spectra in the window with
        every gas valid), ``reduced_chi2``, and per gas ``<gas>`` (ratio to
        the reference), ``<gas>_err`` (block jackknife inside the window;
        NaN when not computed), ``<gas>_frac`` (molar fraction of the gases
        considered) and ``<gas>_frac_err`` (jackknife of the fraction).
        NaN where the window held fewer than ``min_points`` spectra or the
        fit failed.
    replicates : numpy.ndarray or None
        The jackknife copies of the fractions, shape ``(n_spectra,
        n_blocks, n_gases)`` (NaN where not computed), so that the error of
        any combination of fractions (:meth:`cumulative_fraction`) is
        computed from the copies, not propagated; ``None`` without the
        jackknife.
    """
    gases:      list
    reference:  str
    width:      float
    unit:       str
    placement:  str
    excess:     bool
    table:      pd.DataFrame = field(repr=False)
    replicates: np.ndarray | None = field(default=None, repr=False)
    mass_replicates: np.ndarray | None = field(default=None, repr=False)

    @property
    def molar_species(self) -> list[str]:
        """The gases: the species a molar fraction is defined for."""
        return [g for g in self.gases if g not in POSSIBLE_AEROSOLS]

    def ratio(self, gas: str) -> tuple[np.ndarray, np.ndarray]:
        """``(ratio, error)`` of ``gas`` to the reference, per spectrum."""
        if gas not in self.gases:
            raise KeyError(f"{gas} is not among {self.gases}")
        return self.table[gas].to_numpy(float), self.table[f"{gas}_err"].to_numpy(float)

    def fraction(self, gas: str, mass: bool = False) -> tuple[np.ndarray, np.ndarray]:
        """
        ``(fraction, error)`` of ``gas`` per spectrum: molar among the gases
        (NaN for an aerosol), or with ``mass`` by mass among every species.
        """
        if gas not in self.gases:
            raise KeyError(f"{gas} is not among {self.gases}")
        key = "mfrac" if mass else "frac"
        return (self.table[f"{gas}_{key}"].to_numpy(float),
                self.table[f"{gas}_{key}_err"].to_numpy(float))

    def cumulative_fraction(self, order: list[str],
                            mass: bool = False) -> tuple[np.ndarray, np.ndarray]:
        """
        Fractions summed in ``order``, with their jackknife errors.

        The k-th column is the sum of the first k+1 gases of ``order``: the
        upper edge of the k-th band of a stacked plot.  Its error comes from
        the jackknife copies of that sum, since the fractions are correlated
        (they sum to 1; the last column is 1 with no error).

        Parameters
        ----------
        order : list of str
            Bottom to top: every gas (molar), or every species (``mass``).
        mass : bool
            Mass fractions over every species instead of molar fractions
            over the gases.

        Returns
        -------
        cumulative, error : numpy.ndarray
            Shape ``(n_spectra, n_gases)``; the errors are NaN without the
            jackknife.

        Raises
        ------
        KeyError
            When ``order`` is not a permutation of the gases.
        """
        members = self.gases if mass else self.molar_species
        if sorted(order) != sorted(members):
            raise KeyError(f"order {order} must hold every one of {members} once")
        idx = [self.gases.index(g) for g in order]
        key = "mfrac" if mass else "frac"
        F = np.column_stack([self.table[f"{g}_{key}"].to_numpy(float) for g in order])
        cum = np.cumsum(F, axis=1)
        err = np.full_like(cum, np.nan)
        source = self.mass_replicates if mass else self.replicates
        if source is not None:
            reps = np.cumsum(source[:, :, idx], axis=2)      # (n, B, p)
            b = np.sum(np.isfinite(reps[:, :, 0]), axis=1)
            # spectra with no valid copy (b = 0) get NaN, set below
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", RuntimeWarning)
                mean = np.nanmean(reps, axis=1, keepdims=True) if reps.size else reps
            with np.errstate(invalid="ignore", divide="ignore"):
                err = np.sqrt((b - 1)[:, None] / b[:, None]
                              * np.nansum((reps - mean) ** 2, axis=1))
            err[b < 3] = np.nan
        return cum, err


def moving_composition(
    comp: CompositionTable,
    width: float,
    gases: list[str] | None = None,
    *,
    reference: str | None = None,
    unit: str = "minutes",
    placement: str = "centred",
    excess: bool = False,
    min_points: int = MIN_POINTS,
    jackknife_blocks: int = WINDOW_JACKKNIFE_BLOCKS,
) -> MovingComposition:
    """
    The rank-1 composition of every gas, in a window around each spectrum.

    Each window gets its own offset per gas (a background constant within
    the window), as :func:`~plumeIR.analysis.ratios.moving_ratio` does for
    one pair; for two gases the ratios are that function's slopes.  Windows
    never cross an acquisition segment.

    Parameters
    ----------
    comp : CompositionTable
    width : float
        Window width, in ``unit``.
    gases : list of str or None
        Every gas of the table when ``None``.
    reference : str or None
        Gas the ratios are quoted against; SO2 when present, else the first.
    unit : {'minutes', 'spectra'}
    placement : {'centred', 'trailing'}
        Centred for finished runs, trailing for live processing.
    excess : bool
        Background-removed columns (changes only the offsets).
    min_points : int
        Fewest spectra with every gas valid for a window's fit.
    jackknife_blocks : int
        Contiguous blocks for each window's jackknife error; 0 for none
        (about ``jackknife_blocks`` times faster).

    Returns
    -------
    MovingComposition

    Raises
    ------
    PlumeIRError
        For invalid window arguments, fewer than two gases or an unknown
        reference.
    """
    gases, reference = _check_species(comp, gases, reference)
    if placement not in PLACEMENTS:
        raise PlumeIRError(f"placement must be one of {PLACEMENTS}, got {placement!r}")
    by_count, seconds = _check_window(comp, width, unit, placement)
    X, E = _matrix(comp, gases, excess)
    C = _corr_matrix(comp, gases, excess)
    ok = (np.isfinite(X) & np.isfinite(E) & (E > 0)).all(axis=1)
    r = gases.index(reference)
    p = len(gases)
    ratio = np.full((len(comp), p), np.nan)
    err = np.full((len(comp), p), np.nan)
    frac = np.full((len(comp), p), np.nan)
    mfrac = np.full((len(comp), p), np.nan)
    frac_err = np.full((len(comp), p), np.nan)
    mfrac_err = np.full((len(comp), p), np.nan)
    replicates = (np.full((len(comp), jackknife_blocks, p), np.nan)
                  if jackknife_blocks >= 3 else None)
    mass_replicates = None if replicates is None else replicates.copy()
    chi2 = np.full(len(comp), np.nan)
    n_used = np.zeros(len(comp), dtype=int)
    init = None
    for k in np.unique(comp.segment):
        idx = np.flatnonzero(comp.segment == k)
        pos = idx.astype(float) if by_count else seconds[idx]
        lo, hi = _window_bounds(pos, width if by_count else 60.0 * width, placement, by_count)
        for j, i in enumerate(idx):
            w = idx[lo[j]:hi[j]]
            w = w[ok[w]]
            n_used[i] = w.size
            if w.size < max(min_points, 3):
                continue
            Xw, Ew = X[w], E[w]
            Cw = None if C is None else C[w]
            zero = np.zeros(w.size, dtype=int)
            try:
                f = fit_components(Xw, Ew, 1, groups=zero, intercept="group", init=init, corr=Cw)
            except PlumeIRError:
                continue
            init = f.loadings
            ratio[i], frac[i], mfrac[i] = _derive(f.loadings[:, 0], gases, r)
            chi2[i] = f.reduced_chi2
            if jackknife_blocks >= 3 and w.size >= 2 * jackknife_blocks:
                blocks = np.arange(w.size) * jackknife_blocks // w.size
                reps = []
                for b in range(jackknife_blocks):
                    keep = blocks != b
                    try:
                        fb = fit_components(Xw[keep], Ew[keep], 1, groups=zero[keep],
                                            intercept="group", init=f.loadings,
                                            corr=None if Cw is None else Cw[keep])
                    except PlumeIRError:
                        reps = []
                        break
                    reps.append(fb.loadings[:, 0])
                if reps:
                    r_rep, f_rep, m_rep = _derive(np.array(reps), gases, r)
                    err[i], frac_err[i], mfrac_err[i] = (
                        _jackknife_err(r_rep), _jackknife_err(f_rep), _jackknife_err(m_rep))
                    replicates[i], mass_replicates[i] = f_rep, m_rep
    table = pd.DataFrame({"time": comp.time, "run": comp.run, "segment": comp.segment,
                          "n": n_used, "reduced_chi2": chi2})
    for j, gas in enumerate(gases):
        table[gas] = ratio[:, j]
        table[f"{gas}_err"] = err[:, j]
        table[f"{gas}_frac"] = frac[:, j]
        table[f"{gas}_frac_err"] = frac_err[:, j]
        table[f"{gas}_mfrac"] = mfrac[:, j]
        table[f"{gas}_mfrac_err"] = mfrac_err[:, j]
    return MovingComposition(gases=gases, reference=reference, width=float(width), unit=unit,
                             placement=placement, excess=excess, table=table,
                             replicates=replicates, mass_replicates=mass_replicates)


def normalised_residuals(comp: CompositionTable, c: Composition, excess: bool = False) -> pd.DataFrame:
    """
    Residuals of a composition's rank-1 fit, in units of each value's error.

    Under the model they are unit-variance noise; a gas whose residuals are
    wider, or structured in time, carries what one source does not explain.
    Each gas is scaled by its own error only: where gases fitted in one
    window have correlated errors, their residuals are correlated too.

    Parameters
    ----------
    comp : CompositionTable
        The table ``c`` was computed from.
    c : Composition
    excess : bool
        As passed to :func:`composition`.

    Returns
    -------
    pandas.DataFrame
        One column per gas, one row per spectrum; NaN for spectra left out.
    """
    X, E = _matrix(comp, c.fit.gases, excess)
    f = c.fit
    R = (X - f.offsets - f.scores @ f.loadings.T) / E
    R[~f.used] = np.nan
    return pd.DataFrame(R, columns=f.gases)
