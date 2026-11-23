"""
FitResult and ResultsTable — the results of a run.

``FitResult`` is the immutable result of one (spectrum, window) fit by either
backend (NLLS or OE).  It is a plain, **picklable** dataclass — no closures, no
live forward-model references — so it can be returned across a process-pool
boundary in a parallel run, and it carries no unit conversions, so it stays
light.  (It lived in ``retrieval/result.py`` until 2026-11-10; merged here so the
two result types are not one letter apart.)

A run produces one ``FitResult`` per (spectrum, window).  ``ResultsTable``
collects them into a **long-format** pandas DataFrame (one row per
spectrum × window × varied parameter) and provides typed accessors.  Long
format keeps every column name a fixed literal — no per-species f-string
columns anywhere — and naturally handles species sets that differ between
windows.

Two derived concepts:

* **Physical column** — for column-scaling parameters (gas SCD, aerosol mass
  column) the fitted dimensionless scaling is converted to a physical quantity
  via ``Parameter.ref_column``: ``scd = value · ref_column``.  Size/composition
  (``r0_*``, ``wt_*``), polynomial, spectral and temperature parameters have no
  such conversion and report ``NaN`` in the ``scd`` column.
* **Target de-duplication** — a species may be retrieved in several windows (as
  the target in one, an interferer in others).  ``targets()`` returns only the
  rows from the window where the species is a declared target
  (``FitWindowConfig.targets``), i.e. the trusted retrieval.

Xgas (column-averaged dry-air mole fraction) for the solar geometry is a
further results-layer transform and is added when the solar run path is wired.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from plumeIR.constants import LEGACY_AEROSOL_NAMES, LEGACY_PARAMETER_NAMES, WEIGHTS
from plumeIR.config.objects import DUAL_ATM_SUFFIX, FitWindowConfig
from plumeIR.parameters.parameter import Parameter, ParameterKind, ParameterSet
from plumeIR.units import molec_cm2_to_g_m2, molec_cm2_to_ppmm


# =============================================================================
# FitResult
# =============================================================================

def r_squared(measured: np.ndarray, model: np.ndarray) -> float:
    """
    Coefficient of determination of a fit, ``1 − SS_res/SS_tot``.

    Shared by :attr:`FitResult.r2` and the fits a finished run stores
    (``plumeIR.output.StoredFit``), so both give the same number.

    Parameters
    ----------
    measured, model : np.ndarray
        Spectra on the same grid, shape ``(n_wn,)``.

    Returns
    -------
    float
        NaN when ``measured`` is constant.
    """
    ss_res = float(np.sum((measured - model) ** 2))
    ss_tot = float(np.sum((measured - measured.mean()) ** 2))
    return 1.0 - ss_res / ss_tot if ss_tot > 0 else float("nan")


@dataclass(frozen=True)
class FitResult:
    """
    Outcome of one retrieval fit.

    Parameters
    ----------
    success : bool
        Whether the optimizer converged.  Routine non-convergence is recorded
        here (not raised) so a batch run is not aborted by one bad spectrum.
    method : str
        Backend identifier: ``'nlls'`` or ``'oe'``.
    varied_names : tuple[str, ...]
        Names of the varied parameters, in optimizer order.
    values : dict[str, float]
        Fitted value for **every** parameter (varied and fixed), by name.
    perror : dict[str, float] or None
        1σ uncertainty per varied parameter (``sqrt(diag(covariance))``), or
        ``None`` when the covariance is unavailable (e.g. failed fit).
    covariance : np.ndarray or None
        Parameter covariance matrix, shape ``(n_varied, n_varied)``.
    wn : np.ndarray
        Measurement wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    measured : np.ndarray
        Measured spectrum that was fitted, shape ``(n_wn,)``.
    model : np.ndarray
        Best-fit model spectrum on the measurement grid, shape ``(n_wn,)``.
    rmse : float
        Root-mean-square of the residual (``measured − model``).
    nfev : int
        Number of forward evaluations the optimizer used.
    message : str
        Optimizer termination message.
    window_label : str or None
        Fit-window label, attached by the run layer for aggregation.
    spectrum_id : str or None
        Spectrum identifier (e.g. timestamp or index), attached by the run
        layer so results from many spectra can be grouped.
    averaging_kernel : np.ndarray or None
        OE only: averaging-kernel matrix ``A``, shape ``(n_varied, n_varied)``
        (``None`` for NLLS).
    dof : float or None
        OE only: degrees of freedom for signal (``trace(A)``).  ``None`` for
        NLLS.
    continuum : np.ndarray or None
        The fitted model's reference continuum on the measurement grid,
        shape ``(n_wn,)`` (``ForwardModel.continuum``), attached by the run
        layer for display: the gas-free background polynomial × source terms +
        offset (layer, solar), or the clear sky, the model with the plume
        removed (emission).
    """

    success:      bool
    method:       str
    varied_names: tuple[str, ...]
    values:       dict[str, float]
    perror:       dict[str, float] | None
    covariance:   np.ndarray | None
    wn:           np.ndarray
    measured:     np.ndarray
    model:        np.ndarray
    rmse:         float
    nfev:         int
    message:      str
    window_label:     str | None = None
    spectrum_id:      str | None = None
    averaging_kernel: np.ndarray | None = None
    dof:              float | None = None
    continuum:        np.ndarray | None = None

    @property
    def residual(self) -> np.ndarray:
        """Measured − model, shape ``(n_wn,)``."""
        return self.measured - self.model

    @property
    def r2(self) -> float:
        """
        Coefficient of determination, ``1 − SS_res/SS_tot`` — a scale-free
        goodness-of-fit (1 = perfect).  Quantifies fit quality without a plot.
        """
        return r_squared(self.measured, self.model)

    @property
    def n_varied(self) -> int:
        return len(self.varied_names)

    def value(self, name: str) -> float:
        """Fitted value for a parameter by name."""
        return self.values[name]

    def error(self, name: str) -> float | None:
        """1σ uncertainty for a varied parameter, or ``None`` if unavailable."""
        if self.perror is None:
            return None
        return self.perror.get(name)


# =============================================================================
# ResultsTable
# =============================================================================

# Fixed long-format column names (centralised; never built per-species).
COL_SPECTRUM = "spectrum_id"
COL_WINDOW   = "window"
COL_PARAM    = "parameter"
COL_SPECIES  = "species"
COL_KIND     = "kind"
COL_VARIED   = "varied"        # was this parameter fitted (True) or fixed?
COL_VALUE    = "value"
COL_ERROR    = "error"
COL_SCD      = "scd"
COL_SCD_ERR  = "scd_error"
COL_PRES     = "pres"          # P/T the retrieval used — recorded so unit
COL_TEMP     = "temp"          # conversions (ppm·m) need no re-supplied guess
COL_TARGET   = "is_target"
COL_SUCCESS  = "success"
COL_RMSE     = "rmse"
COL_R2       = "r2"            # scale-free goodness-of-fit per (spectrum, window)

_COLUMNS = [
    COL_SPECTRUM, COL_WINDOW, COL_PARAM, COL_SPECIES, COL_KIND, COL_VARIED,
    COL_VALUE, COL_ERROR, COL_SCD, COL_SCD_ERR, COL_PRES, COL_TEMP,
    COL_TARGET, COL_SUCCESS, COL_RMSE, COL_R2,
]

_GAS_KINDS = ("GAS_ATM", "GAS_PLUME")

_NAN = float("nan")


def _pt_for(geometry, kind: ParameterKind, values: dict[str, float]) -> tuple[float, float]:
    """
    Pressure/temperature [mbar, K] for a gas parameter's ppm·m conversion.

    Temperature is the **actual** value the retrieval used: the fitted/fixed
    ``plume_temp`` (plume) or ``atm_temp`` (atmospheric) parameter
    if present, else the geometry a priori.  An unresolved a priori yields NaN
    rather than an exception.  Pressure never varies, so it is the geometry
    value.  Returns ``(NaN, NaN)`` for non-gas kinds (no ppm·m conversion).
    """
    def _num(x) -> float:
        # A missing a priori must degrade to NaN (the conversion is skipped),
        # never crash a run that has already fitted successfully.
        return _NAN if x is None else float(x)

    if kind is ParameterKind.GAS_ATM:
        pres = getattr(geometry, "atm_pres", _NAN)
        temp = values.get("atm_temp", getattr(geometry, "atm_temp", _NAN))
        return _num(pres), _num(temp)
    if kind is ParameterKind.GAS_PLUME:
        pres = getattr(geometry, "plume_pres", _NAN)
        # The fitted or held plume_temp (every geometry), else the a priori.
        temp = values.get("plume_temp", getattr(geometry, "plume_temp", _NAN))
        return _num(pres), _num(temp)
    return _NAN, _NAN

_COLUMN_SCALING_KINDS = (ParameterKind.GAS_ATM, ParameterKind.GAS_PLUME,
                         ParameterKind.AERO_PLUME)


def _rename_legacy_aerosols(df: pd.DataFrame) -> pd.DataFrame:
    """
    Rows of a former aerosol name (``H2SO4``, ``HNO3``, until 2026-11-17),
    renamed to the current one (``H2SO4_aq``, ``HNO3_aq``), with its ``r0_`` /
    ``wt_`` parameters.  Only aerosol rows (``kind`` ``AERO_PLUME``) are
    touched: gas ``HNO3`` keeps its name.  Keeps old run folders readable
    against their configs, which the parser renames the same way.
    """
    if COL_KIND not in df or COL_SPECIES not in df:
        return df
    aero = (df[COL_KIND] == ParameterKind.AERO_PLUME.name) & df[COL_SPECIES].isin(
        list(LEGACY_AEROSOL_NAMES))
    if not aero.any():
        return df
    df = df.copy()
    old = df.loc[aero, COL_SPECIES]
    new = old.map(LEGACY_AEROSOL_NAMES)
    params = df.loc[aero, COL_PARAM].astype(str)
    df.loc[aero, COL_PARAM] = [p[: len(p) - len(o)] + n if p.endswith(o) else p
                               for p, o, n in zip(params, old, new)]
    df.loc[aero, COL_SPECIES] = new
    return df


def _rename_legacy_parameters(df: pd.DataFrame) -> pd.DataFrame:
    """
    Rows of a former parameter name (``gas_temp``, ``gasE_temp``,
    ``nu_shift``, until 2026-11-19) under the current one (``plume_temp``,
    ``plumeE_temp``, ``shift``; :data:`~plumeIR.constants.LEGACY_PARAMETER_NAMES`),
    in the parameter and species columns.  Keeps old run folders readable
    against the renamed code, as their configs are by the parser.
    """
    if COL_PARAM not in df:
        return df
    hit = df[COL_PARAM].isin(list(LEGACY_PARAMETER_NAMES))
    if COL_SPECIES in df:
        hit |= df[COL_SPECIES].isin(list(LEGACY_PARAMETER_NAMES))
    if not hit.any():
        return df
    df = df.copy()
    df.loc[hit, COL_PARAM] = df.loc[hit, COL_PARAM].replace(LEGACY_PARAMETER_NAMES)
    if COL_SPECIES in df:
        df.loc[hit, COL_SPECIES] = df.loc[hit, COL_SPECIES].replace(LEGACY_PARAMETER_NAMES)
    return df


def _species_of(name: str) -> str:
    """Species a parameter refers to: strip the ``r0_`` / ``wt_`` aerosol prefix."""
    for prefix in ("r0_", "wt_"):
        if name.startswith(prefix):
            return name[len(prefix):]
    return name


def _is_column_scaling(param: Parameter) -> bool:
    """
    True when the parameter is a column-scaling factor whose ``ref_column``
    converts it to a physical column (gas SCD or aerosol mass column).

    Gas scalings always qualify; for aerosols only the bare mass scaling
    ``{SPECIES}`` does — ``r0_*`` (radius) and ``wt_*`` (composition) are
    physical values in their own units, not scalings.
    """
    if param.kind in (ParameterKind.GAS_ATM, ParameterKind.GAS_PLUME):
        return True
    if param.kind is ParameterKind.AERO_PLUME:
        return not param.name.startswith(("r0_", "wt_"))
    return False


@dataclass
class ResultsTable:
    """Long-format results table with typed accessors."""

    df: pd.DataFrame

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------

    @classmethod
    def from_fits(
        cls,
        fits:       list[FitResult],
        windows:    dict[str, FitWindowConfig],
        param_sets: dict[str, ParameterSet],
        geometries: dict[str, object] | None = None,
    ) -> "ResultsTable":
        """
        Build a table from a flat list of FitResults.

        Parameters
        ----------
        fits : list[FitResult]
            Results across all (spectrum, window) fits.  Each must carry
            ``window_label`` (and ``spectrum_id`` for grouping).
        windows : dict[str, FitWindowConfig]
            Window configs by label — supplies the target species set.
        param_sets : dict[str, ParameterSet]
            Parameter set by window label — supplies ``kind`` and
            ``ref_column`` for the SCD conversion.
        geometries : dict[str, object] or None
            Geometry by window label — supplies the P/T the retrieval used, so
            ppm·m conversion needs no re-supplied values.  Optional.
        """
        geometries = geometries or {}
        rows: list[dict] = []
        for fr in fits:
            wcfg    = windows.get(fr.window_label)
            pset    = param_sets.get(fr.window_label)
            geom    = geometries.get(fr.window_label)
            targets = set(wcfg.targets) if wcfg is not None else set()
            varied  = set(fr.varied_names)

            # Record EVERY parameter (varied and fixed): value always; error
            # only for varied params.  Fixed temps / poly / spectral are part
            # of the retrieval state and are needed to reconstruct the forward.
            names = pset.names() if pset is not None else list(fr.varied_names)
            for name in names:
                if name not in fr.values:
                    continue
                param   = pset[name] if (pset is not None and name in pset) else None
                value   = float(fr.values[name])
                error   = fr.error(name)
                err_val = np.nan if error is None else float(error)

                scd = scd_err = np.nan
                if param is not None and _is_column_scaling(param):
                    scd = value * param.ref_column
                    scd_err = err_val * param.ref_column     # NaN propagates

                kind = param.kind if param is not None else None
                pres, temp = _pt_for(geom, kind, fr.values) if kind is not None \
                    else (np.nan, np.nan)

                rows.append({
                    COL_SPECTRUM: fr.spectrum_id,
                    COL_WINDOW:   fr.window_label,
                    COL_PARAM:    name,
                    COL_SPECIES:  _species_of(name),
                    COL_KIND:     kind.name if kind is not None else "",
                    COL_VARIED:   name in varied,
                    COL_VALUE:    value,
                    COL_ERROR:    err_val,
                    COL_SCD:      scd,
                    COL_SCD_ERR:  scd_err,
                    COL_PRES:     pres,
                    COL_TEMP:     temp,
                    COL_TARGET:   _species_of(name) in targets,
                    COL_SUCCESS:  fr.success,
                    COL_RMSE:     fr.rmse,
                    COL_R2:       fr.r2,
                })

        return cls(pd.DataFrame(rows, columns=_COLUMNS))

    @classmethod
    def from_csv(cls, path: str | Path) -> "ResultsTable":
        """
        Load a table previously written by :meth:`to_csv`.

        All derived-quantity accessors work identically on a loaded table:
        they depend only on stored columns (``scd``, ``species``, ``kind``,
        ``pres``, ``temp``) and physical constants — nothing is re-supplied.
        """
        df = pd.read_csv(Path(path))
        # CSV round-trips bools as strings — coerce back robustly.
        for col in (COL_VARIED, COL_TARGET, COL_SUCCESS):
            if col in df.columns:
                df[col] = df[col].isin([True, "True", "TRUE", 1, "1"])
        return cls(_rename_legacy_parameters(_rename_legacy_aerosols(df)))

    # ------------------------------------------------------------------
    # Accessors
    # ------------------------------------------------------------------

    def to_dataframe(self) -> pd.DataFrame:
        """The full long-format table (a copy)."""
        return self.df.copy()

    def targets(self) -> pd.DataFrame:
        """
        Rows for species in the window where they are the declared target —
        the trusted, de-duplicated retrieval per species.
        """
        return self.df[self.df[COL_TARGET]].reset_index(drop=True)

    def species_series(self, species: str, quantity: str = COL_SCD) -> pd.Series:
        """
        Per-spectrum series of ``quantity`` for a target species (indexed by
        ``spectrum_id``), taken from its target window.

        Parameters
        ----------
        species : str
            Species name (e.g. ``'SO2'``).
        quantity : str
            Column to extract — ``'scd'`` (default), ``'value'``, ``'error'``…
        """
        sel = self.targets()
        sel = sel[sel[COL_SPECIES] == species]
        return sel.set_index(COL_SPECTRUM)[quantity]

    # ------------------------------------------------------------------
    # Derived-quantity converters (self-contained — no re-supplied inputs)
    # ------------------------------------------------------------------

    def mass_loading(self) -> pd.Series:
        """
        Mass loading [g/m²] per row, aligned to the table index.

        Gas SCDs convert via molecular weight (``WEIGHTS`` registry, keyed by
        the stored ``species``); aerosol ``scd`` is already a mass column;
        everything else is ``NaN``.  Needs no external input — molar mass is a
        physical constant.
        """
        def _row(r) -> float:
            kind, scd, sp = r[COL_KIND], r[COL_SCD], r[COL_SPECIES]
            if not np.isfinite(scd):
                return np.nan
            if kind == "AERO_PLUME":
                return float(scd)                 # scd is already g/m²
            if kind == "GAS_ATM":
                sp = sp.removesuffix(DUAL_ATM_SUFFIX)   # ambient part of a dual gas
            if kind in _GAS_KINDS and sp in WEIGHTS:
                return float(molec_cm2_to_g_m2(scd, sp))
            return np.nan

        return self.df.apply(_row, axis=1)

    def to_ppm_m(self) -> pd.Series:
        """
        Gas column in [ppm·m] per row, aligned to the table index.

        Uses the ``pres``/``temp`` recorded from the run geometry (path length
        cancels), so no P/T need be re-supplied.  Returns ``NaN`` for non-gas
        rows or where P/T were not recorded (e.g. solar — use Xgas there).
        """
        def _row(r) -> float:
            kind, scd = r[COL_KIND], r[COL_SCD]
            pres, temp = r[COL_PRES], r[COL_TEMP]
            if (kind in _GAS_KINDS and np.isfinite(scd)
                    and np.isfinite(pres) and np.isfinite(temp)):
                return float(molec_cm2_to_ppmm(scd, pres=pres, temp=temp))
            return np.nan

        return self.df.apply(_row, axis=1)

    def wide(self, quantity: str = COL_SCD, targets_only: bool = True) -> pd.DataFrame:
        """
        Pivot to the familiar **rows = spectra, columns = species** view.

        Parameters
        ----------
        quantity : str
            Column to tabulate — ``'scd'`` (default), ``'value'``, ``'error'``…
        targets_only : bool
            If ``True`` (default), use only the trusted target rows, so each
            species appears once per spectrum (config uniqueness guarantees
            this).  If ``False``, include all rows (a shared interferer
            retrieved in several windows is averaged — use the long table or a
            per-window pivot for window-resolved values).

        Returns
        -------
        pandas.DataFrame
            Indexed by ``spectrum_id``, one column per species.
        """
        src = self.targets() if targets_only else self.df
        return src.pivot_table(index=COL_SPECTRUM, columns=COL_SPECIES,
                               values=quantity, aggfunc="first")

    def aerosol_components(self) -> pd.DataFrame:
        """
        Molecular content of each target aerosol, per spectrum.

        For an aerosol with molecular components (``plumeIR.species``): a
        solution (``H2SO4_aq``: the acid at its ``wt_`` parameter's weight
        fraction in that spectrum, water the rest; ``HNO3_aq`` at its fixed
        fraction) or a pure particle (``WATER``, ``ICE``: H2O).  Columns, for
        aerosol ``A`` and component ``X``: ``A[X]_g_m2`` (mass column),
        ``A[X]_g_m2_err``, ``A[X]_molec_cm2`` (molecular column, comparable
        with a gas slant column: acid moles against SO2) and
        ``A[X]_molec_cm2_err``.  The errors scale the particle mass error by
        the fraction: exact for a fixed fraction, without the fraction's own
        error where it is fitted (the fit covariance is not in the results).

        Returns
        -------
        pandas.DataFrame
            Indexed by ``spectrum_id``; no column for an aerosol without
            molecular content (ash, dust) or without a target.
        """
        from plumeIR import species as _sp
        t = self.targets()
        frames = []
        for name in pd.unique(t[COL_SPECIES]):
            if not _sp.is_aerosol(name) or not _sp.get(name).components:
                continue
            rows = t[(t[COL_SPECIES] == name) & (t[COL_PARAM] == name)]
            mass = rows.set_index(COL_SPECTRUM)[COL_SCD].astype(float)
            err = rows.set_index(COL_SPECTRUM)[COL_SCD_ERR].astype(float)
            wt = None
            param = next((c.parameter for c in _sp.get(name).components if c.parameter), None)
            if param is not None:
                windows = set(rows[COL_WINDOW])
                w = self.df[(self.df[COL_PARAM] == param) & self.df[COL_WINDOW].isin(windows)]
                wt = w.drop_duplicates(COL_SPECTRUM).set_index(COL_SPECTRUM)[COL_VALUE] \
                    .astype(float).reindex(mass.index).to_numpy()
            frac = _sp.component_fractions(name, wt)
            out = {}
            for formula, f in frac.items():
                k = _sp.N_A / _sp.MOLAR_MASS[formula] / 1e4
                key = f"{name}[{formula}]"
                out[f"{key}_g_m2"] = mass.to_numpy() * f
                out[f"{key}_g_m2_err"] = err.to_numpy() * f
                out[f"{key}_molec_cm2"] = mass.to_numpy() * f * k
                out[f"{key}_molec_cm2_err"] = err.to_numpy() * f * k
            frames.append(pd.DataFrame(out, index=mass.index))
        if not frames:
            return pd.DataFrame(index=pd.Index([], name=COL_SPECTRUM))
        return pd.concat(frames, axis=1)

    def to_csv(self, path: str | Path) -> None:
        """Write the long-format table to CSV."""
        self.df.to_csv(Path(path), index=False)

    def __len__(self) -> int:
        return len(self.df)
