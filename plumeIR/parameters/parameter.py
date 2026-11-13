"""
Parameter model for plumeIR v3.

ParameterKind  — enum classifying what each parameter represents
Parameter      — immutable single-parameter descriptor
ParameterSet   — immutable ordered collection used by forward models and fitters
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from typing import Iterator, Sequence

import numpy as np


# =============================================================================
# ParameterKind
# =============================================================================

class ParameterKind(Enum):
    """Classification of a fit parameter."""
    POLYNOMIAL  = auto()   # background polynomial coefficient
    GAS_ATM     = auto()   # atmospheric gas scaling factor
    GAS_PLUME   = auto()   # plume gas scaling factor
    AERO_PLUME  = auto()   # plume aerosol scaling factor or size
    SPECTRAL    = auto()   # ILS / wavenumber parameter (fov, opd, shift, offset)
    TEMPERATURE = auto()   # temperature parameter (gas_temp, atm_temp, dt_*, source_temp)
    ATMOSPHERE  = auto()   # background atmospheric state (H2O_sc — emission geometry)


# =============================================================================
# Parameter
# =============================================================================

@dataclass(frozen=True)
class Parameter:
    """
    Immutable descriptor for a single fit parameter.

    Parameters
    ----------
    name : str
        Unique identifier within its ParameterSet (e.g. ``'SO2'``,
        ``'poly0'``, ``'fov'``).
    value : float
        Prior / initial guess.
    vary : bool
        Whether this parameter is free during optimisation.
    bounds : tuple[float, float]
        ``(lower, upper)`` bounds.  Used by NLLS and as prior width for OE.
    kind : ParameterKind
        Classification of this parameter.
    ref_column : float
        Reference column amount the dimensionless scaling factor multiplies.
        Gases: reference slant column density [molec/cm²].
        Aerosols: reference mass column [g/m²] (= ref concentration ×
        plume_thickness).  ``1.0`` for parameters that are not scaling
        factors (polynomials, spectral, temperature).
    sigma_prior : float or None
        1-sigma prior uncertainty for the OE backend.  If ``None``, the
        OE backend derives it from ``bounds`` as ``(upper - lower) / 4``.
    """
    name: str
    value: float
    vary: bool
    bounds: tuple[float, float]
    kind: ParameterKind
    ref_column: float = 1.0
    sigma_prior: float | None = None

    def __post_init__(self) -> None:
        lo, hi = self.bounds
        if lo > hi:
            raise ValueError(
                f"Parameter '{self.name}': lower bound {lo} > upper bound {hi}"
            )

    @property
    def effective_sigma_prior(self) -> float:
        """Prior uncertainty for OE; derived from bounds if not explicit."""
        if self.sigma_prior is not None:
            return self.sigma_prior
        lo, hi = self.bounds
        return (hi - lo) / 4.0


# =============================================================================
# ParameterSet
# =============================================================================

class ParameterSet:
    """
    Immutable ordered collection of :class:`Parameter` objects.

    Provides the interface required by both the forward model
    (``p0_vector``, ``bounds_tuple``) and the fitter (``unpack``).

    Parameters are stored in insertion order.  The set is frozen at
    construction: no parameters can be added, removed, or modified.
    """

    def __init__(self, params: Sequence[Parameter]) -> None:
        # Store as tuple (immutable) and build a name→index lookup dict
        self._params: tuple[Parameter, ...] = tuple(params)
        self._index: dict[str, int] = {p.name: i for i, p in enumerate(self._params)}
        if len(self._index) != len(self._params):
            dupes = [p.name for p in self._params
                     if sum(1 for q in self._params if q.name == p.name) > 1]
            raise ValueError(f"Duplicate parameter names: {sorted(set(dupes))}")

    # ------------------------------------------------------------------
    # Immutability guard
    # ------------------------------------------------------------------

    def __setattr__(self, name: str, value: object) -> None:
        if name.startswith("_"):
            super().__setattr__(name, value)
        else:
            raise AttributeError("ParameterSet is immutable")

    # ------------------------------------------------------------------
    # Sequence-like access
    # ------------------------------------------------------------------

    def __len__(self) -> int:
        return len(self._params)

    def __iter__(self) -> Iterator[Parameter]:
        return iter(self._params)

    def __contains__(self, name: object) -> bool:
        return name in self._index

    def __getitem__(self, name: str) -> Parameter:
        return self._params[self._index[name]]

    def __repr__(self) -> str:
        lines = [f"ParameterSet ({len(self._params)} params):"]
        for p in self._params:
            vary_str = "free" if p.vary else "fixed"
            lines.append(
                f"  {p.name:<20s} {p.value:>12.4g}  [{vary_str}]"
                f"  bounds=({p.bounds[0]:.4g}, {p.bounds[1]:.4g})"
                f"  {p.kind.name}"
            )
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Derivation (immutability-preserving)
    # ------------------------------------------------------------------

    def seeded(self, updates: dict[str, float]) -> "ParameterSet":
        """
        Return a **new** ParameterSet with the initial values of the named
        parameters replaced.

        Used for per-spectrum polynomial seeding (and parameter chaining): the
        set stays immutable — this never mutates ``self``.  Unknown names are
        ignored.

        Parameters
        ----------
        updates : dict[str, float]
            Parameter name → new initial value.

        Returns
        -------
        ParameterSet
        """
        from dataclasses import replace
        new = [replace(p, value=float(updates[p.name])) if p.name in updates else p
               for p in self._params]
        return ParameterSet(new)

    # ------------------------------------------------------------------
    # Queries
    # ------------------------------------------------------------------

    def names(self) -> list[str]:
        """All parameter names in order."""
        return [p.name for p in self._params]

    def varied(self) -> list[Parameter]:
        """Parameters with ``vary=True``, in order."""
        return [p for p in self._params if p.vary]

    def varied_names(self) -> list[str]:
        """Names of varied parameters, in order."""
        return [p.name for p in self._params if p.vary]

    def by_kind(self, kind: ParameterKind) -> list[Parameter]:
        """All parameters of a given :class:`ParameterKind`, in order."""
        return [p for p in self._params if p.kind == kind]

    def names_by_kind(self, kind: ParameterKind) -> list[str]:
        """Names of all parameters of a given kind, in order."""
        return [p.name for p in self._params if p.kind == kind]

    # ------------------------------------------------------------------
    # Optimizer interface
    # ------------------------------------------------------------------

    def p0_vector(self) -> np.ndarray:
        """
        Initial-guess vector for varied parameters.

        Returns
        -------
        np.ndarray
            Shape ``(n_varied,)``.
        """
        return np.array([p.value for p in self._params if p.vary])

    def bounds_tuple(self) -> tuple[np.ndarray, np.ndarray]:
        """
        Lower and upper bound arrays for varied parameters.

        Returns
        -------
        tuple[np.ndarray, np.ndarray]
            ``(lower, upper)``, each shape ``(n_varied,)``.
        """
        varied = self.varied()
        lower = np.array([p.bounds[0] for p in varied])
        upper = np.array([p.bounds[1] for p in varied])
        return lower, upper

    def prior_covariance(self) -> np.ndarray:
        """
        Diagonal prior covariance matrix for the OE backend.

        Uses :attr:`Parameter.effective_sigma_prior` for each varied
        parameter.

        Returns
        -------
        np.ndarray
            Shape ``(n_varied, n_varied)``.
        """
        sigmas = np.array([p.effective_sigma_prior for p in self.varied()])
        return np.diag(sigmas ** 2)

    def unpack(self, vector: np.ndarray) -> dict[str, float]:
        """
        Map a varied-parameter vector back to a name → value dict.

        Fixed parameters are included at their prior values so the
        forward model always receives the full parameter set.

        Parameters
        ----------
        vector : np.ndarray
            Shape ``(n_varied,)``.

        Returns
        -------
        dict[str, float]
            All parameters by name.
        """
        if len(vector) != len(self.varied()):
            raise ValueError(
                f"vector length {len(vector)} does not match "
                f"{len(self.varied())} varied parameters"
            )
        result: dict[str, float] = {p.name: p.value for p in self._params}
        for param, val in zip(self.varied(), vector):
            result[param.name] = float(val)
        return result
