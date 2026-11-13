"""
NonLinearLeastSquares — NLLS retrieval backend (wraps scipy.optimize.least_squares).

A single-fit, geometry-agnostic optimizer: it receives a ``ForwardModel``
callable and a ``ParameterSet`` and returns a ``FitResult``.  It has **no
awareness of parallelism** — running many (spectrum, window) fits concurrently
is the job of the run-orchestration layer, which keeps each fit single-threaded
and parallelises across spectra.  Because the forward model is pure NumPy at
fit time (RFM / mieEODG run only when the ``Reference`` is built), many of these
fits can run concurrently with no shared mutable state.

Bounds are taken from the ``ParameterSet`` and handed to ``least_squares``'
trust-region-reflective solver.  Routine non-convergence is recorded in
``FitResult.success`` rather than raised; only structural problems raise
``FitError``.
"""

from __future__ import annotations

import logging

import numpy as np
from scipy.optimize import least_squares

from plumeIR.exceptions import FitError
from plumeIR.forward.base import ForwardModel
from plumeIR.parameters.parameter import ParameterKind, ParameterSet
from plumeIR.retrieval.results import FitResult

logger = logging.getLogger(__name__)


def _covariance(res, n_data: int, n_par: int, absolute: bool) -> np.ndarray | None:
    """
    Parameter covariance from a least_squares result (mirrors curve_fit).

    ``cov = (JᵀJ)⁻¹``, scaled by the reduced chi-square when the measurement
    noise is unknown (``absolute=False``), matching ``curve_fit``'s
    ``absolute_sigma=False`` behaviour.
    """
    try:
        cov = np.linalg.inv(res.jac.T @ res.jac)
    except np.linalg.LinAlgError:
        return None
    if not absolute:
        dof = max(1, n_data - n_par)
        cov = cov * (2.0 * res.cost / dof)   # 2*cost = Σ residual²
    return cov


class NonLinearLeastSquares:
    """
    Non-linear least-squares backend.

    Parameters
    ----------
    ftol, xtol : float
        Convergence tolerances passed to the solver.
    max_nfev : int or None
        Maximum forward evaluations (``None`` → solver default).
    plume_restart_scaling : float or None
        Elevated initial scaling for a second optimisation start of the plume
        gases (a multi-start to escape the weak-gas near-zero local minimum —
        see :meth:`fit`).  ``None`` disables the restart.  The lower-cost of the
        default-start and elevated-start fits is kept, so a restart can never
        worsen the result.
    """

    def __init__(self, ftol: float = 1e-6, xtol: float = 1e-6,
                 max_nfev: int | None = None,
                 plume_restart_scaling: float | None = 50.0) -> None:
        self.ftol = ftol
        self.xtol = xtol
        self.max_nfev = max_nfev
        self.plume_restart_scaling = plume_restart_scaling

    def fit(
        self,
        forward:      ForwardModel,
        params:       ParameterSet,
        measured:     np.ndarray,
        *,
        sigma:        np.ndarray | None = None,
        window_label: str | None = None,
        spectrum_id:  str | None = None,
    ) -> FitResult:
        """
        Fit one measured spectrum with one forward model.

        Parameters
        ----------
        forward : ForwardModel
            Forward model for this (geometry, window, spectrum).  Its
            ``wn_meas`` must match ``measured``.
        params : ParameterSet
            Immutable parameter set; supplies the initial guess and bounds.
        measured : np.ndarray
            Measured spectrum on the forward's measurement grid, shape
            ``(n_wn,)``.
        sigma : np.ndarray or None
            Per-point 1σ measurement noise.  When given, the covariance is
            returned in absolute units (``absolute_sigma=True``).
        window_label : str or None
            Window label to carry into the result for the run layer.

        Returns
        -------
        FitResult
        """
        measured = np.asarray(measured, dtype=float)
        xdata = np.asarray(forward.wn_meas, dtype=float)
        if xdata.shape != measured.shape:
            raise FitError(
                f"measured spectrum shape {measured.shape} does not match the "
                f"forward measurement grid {xdata.shape}"
            )

        names = tuple(params.varied_names())
        p0 = params.p0_vector()
        lower, upper = params.bounds_tuple()

        # least_squares residual (model − measured), optionally noise-weighted.
        if sigma is not None:
            sig = np.broadcast_to(np.asarray(sigma, dtype=float), measured.shape)
            def _resid(p):
                return (forward(np.asarray(p)) - measured) / sig
        else:
            def _resid(p):
                return forward(np.asarray(p)) - measured

        def _solve(x0):
            return least_squares(
                _resid, x0, bounds=(lower, upper), method="trf",
                ftol=self.ftol, xtol=self.xtol, max_nfev=self.max_nfev,
            )

        try:
            res = _solve(p0)
            # Multi-start: a weak plume gas on a strong background (e.g. CO under
            # CO₂) can leave the optimiser stuck at a near-zero column from the
            # tiny default prior — ascent is ill-conditioned, descent is not.  A
            # second start with the plume gases elevated reaches the true column
            # (climbing the short remaining distance, or descending if the gas
            # is genuinely absent).  Keep whichever start gives the lower cost,
            # so the restart can only help.  ``cost = ½·Σ residual²``.
            x0_hi = self._elevated_start(params, p0)
            if x0_hi is not None:
                res_hi = _solve(x0_hi)
                if res_hi.cost < res.cost:
                    res = res_hi
            popt = res.x                       # last/best iterate — kept even at max_nfev
            success = res.status > 0           # 0 = max_nfev (soft); >0 = converged
            nfev = int(res.nfev)
            message = str(res.message)
            covariance = _covariance(res, measured.size, popt.size, sigma is not None)
        except Exception as exc:               # structural numerical failure
            logger.warning("NLLS fit errored (%s): %s", window_label, exc)
            popt, covariance = p0, None
            success, nfev, message = False, 0, f"least_squares errored: {exc}"

        model = forward(popt)
        resid = measured - model
        rmse = float(np.sqrt(np.mean(resid ** 2)))

        if covariance is not None and np.all(np.isfinite(covariance)):
            perror = {n: float(e) for n, e in zip(names, np.sqrt(np.diag(covariance)))}
        else:
            perror, covariance = None, None

        return FitResult(
            success=success, method="nlls", varied_names=names,
            values=params.unpack(popt), perror=perror, covariance=covariance,
            wn=xdata, measured=measured, model=model, rmse=rmse,
            nfev=nfev, message=message, window_label=window_label,
            spectrum_id=spectrum_id,
        )

    def _elevated_start(self, params: ParameterSet,
                        p0: np.ndarray) -> np.ndarray | None:
        """
        Build the multi-start initial vector with varied plume gases raised to
        ``plume_restart_scaling``.

        Returns ``None`` when no restart is warranted — the restart is disabled,
        there are no varied plume gases, or every varied plume gas already
        starts at or above the elevated scaling (clamped to its upper bound) so
        the second start would duplicate the first.
        """
        s = self.plume_restart_scaling
        if not s:
            return None
        x0 = p0.copy()
        changed = False
        for j, p in enumerate(params.varied()):
            if p.kind is ParameterKind.GAS_PLUME:
                target = min(float(s), float(p.bounds[1]))
                if target > x0[j]:
                    x0[j] = target
                    changed = True
        return x0 if changed else None
