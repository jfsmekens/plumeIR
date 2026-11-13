"""
OptimalEstimation — OE retrieval backend (wraps pyOptimalEstimation).

Same single-fit, parallelism-agnostic contract as ``NonLinearLeastSquares``:
``fit(forward, params, measured) -> FitResult``.  Interchangeable with NLLS —
the run-orchestration layer selects the backend.

OE differs from NLLS in needing a **proper Bayesian prior**: a finite prior
covariance ``S_a`` (from ``ParameterSet.prior_covariance()``) and a measurement
covariance ``S_y``.  Parameters with unbounded ``(0, inf)`` bounds and no
explicit ``sigma_prior`` yield an infinite prior variance — invalid for OE — so
``fit`` checks for that and raises a clear ``FitError`` rather than letting
pyOptimalEstimation fail on a singular-matrix assert.

The posterior carries OE-specific diagnostics — the averaging kernel and the
degrees of freedom for signal — which are stored on the ``FitResult``.

pyOptimalEstimation does its own (finite-difference) Jacobian internally; the
shared analytic-Jacobian utility is a later refinement (the same one that will
speed up both backends).

NOTE: ``pyOptimalEstimation`` builds a dense ``(n_wn, n_wn)`` measurement
covariance, so OE is much heavier per fit than NLLS on wide windows.
"""

from __future__ import annotations

import logging

import numpy as np

from plumeIR.exceptions import FitError
from plumeIR.forward.base import ForwardModel
from plumeIR.parameters.parameter import ParameterSet
from plumeIR.retrieval.results import FitResult

logger = logging.getLogger(__name__)


class OptimalEstimation:
    """
    Optimal-estimation backend.

    Parameters
    ----------
    max_iter : int
        Maximum Gauss-Newton iterations.  Default 30: a real layer-geometry
        window (11 parameters, state spanning ~6 orders of magnitude) needs
        appreciably more steps than the synthetic problems in the unit tests.
        At 10 only half the spectra converged; at 30 all of them did, and 100
        changed nothing further — so 30 is past the knee, not a cap.  Fits that
        converge sooner still stop sooner, so the cost is ~1 s/spectrum.
    perturbation : float
        Fractional state perturbation for pyOptimalEstimation's internal
        finite-difference Jacobian.
    default_noise : float
        Fallback per-point measurement 1σ when ``sigma`` is not supplied to
        ``fit``.  A realistic value matters for the posterior covariance.
    normalize : bool
        Solve in prior-normalized state space ``z = (x - x_a) / sigma_a``,
        where the prior covariance is the identity.  Required in practice —
        without it the retrieval fails in all but the best-conditioned window
        (see :meth:`fit`).  Set ``False`` only to reproduce the unnormalized
        behaviour for debugging.
    """

    def __init__(self, max_iter: int = 30, perturbation: float = 0.1,
                 default_noise: float = 1.0, normalize: bool = True) -> None:
        self.max_iter = max_iter
        self.perturbation = perturbation
        self.default_noise = default_noise
        self.normalize = normalize

    def fit(
        self,
        forward:      ForwardModel,
        params:       ParameterSet,
        measured:     np.ndarray,
        *,
        sigma:        np.ndarray | float | None = None,
        window_label: str | None = None,
        spectrum_id:  str | None = None,
    ) -> FitResult:
        """
        Retrieve one spectrum by optimal estimation.

        Parameters
        ----------
        forward, params, measured
            As for :meth:`NonLinearLeastSquares.fit`.
        sigma : np.ndarray, float or None
            Measurement 1σ (per-point or scalar).  Defaults to
            ``default_noise``.  Sets the diagonal measurement covariance.
        window_label, spectrum_id : str or None
            Identity tags carried into the result.
        """
        import pyOptimalEstimation as poe

        measured = np.asarray(measured, dtype=float)
        xdata = np.asarray(forward.wn_meas, dtype=float)
        if xdata.shape != measured.shape:
            raise FitError(
                f"measured spectrum shape {measured.shape} does not match the "
                f"forward measurement grid {xdata.shape}"
            )

        names = list(params.varied_names())
        x_a = params.p0_vector()
        S_a = params.prior_covariance()

        # OE needs a proper (finite) prior — unbounded params with no explicit
        # sigma_prior give an infinite prior variance.
        bad = [n for n, s in zip(names, np.diag(S_a)) if not np.isfinite(s)]
        if bad:
            raise FitError(
                f"OE requires a finite prior for every varied parameter, but "
                f"{bad} have none (unbounded with no sigma_prior). Set "
                f"sigma_prior on these parameters to retrieve them with OE."
            )

        # Diagonal measurement covariance from the per-point noise.
        noise = self.default_noise if sigma is None else sigma
        noise = np.broadcast_to(np.asarray(noise, dtype=float), measured.shape)
        S_y = np.diag(noise ** 2)

        lower, upper = params.bounds_tuple()

        # --------------------------------------------------------------
        # State normalization:  z = (x - x_a) / sigma_a
        #
        # A real retrieval state spans several orders of magnitude (poly0
        # ~3e4, a gas scaling ~1e2, fov ~5e-2), so S_a and
        # K^T S_y^-1 K + S_a^-1 are badly conditioned and the Gauss-Newton
        # step fails.  In z-space the prior covariance is the identity by
        # construction, which removes the problem exactly.
        #
        # Measured in the CO window (2020-2150, the ill-conditioned one),
        # 25 spectra, max_iter=30:
        #
        #     prior        unnormalized      normalized
        #     100 +/- 100     0/25             15/25
        #     100 +/- 30      1/25             15/25
        #     300 +/- 300     2/25             23/25
        #
        # The SO2 window converges either way, which is why this only shows
        # up once a harder window is tried.  Retrieved values agree with the
        # unnormalized solution where both converge.
        #
        # Valid for pyOptimalEstimation because its Jacobian perturbation is
        # ADDITIVE and already scaled by the prior error (pyOEcore:
        # xb + xb_err * perturbation, with useFactorInJac=False).  A
        # multiplicative perturbation would break on the zero-centred state.
        # --------------------------------------------------------------
        sigma_a = np.sqrt(np.diag(S_a))
        if self.normalize:
            if np.any(sigma_a <= 0):
                zero = [n for n, sg in zip(names, sigma_a) if sg <= 0]
                raise FitError(
                    f"OE state normalization needs a positive prior sigma for "
                    f"every varied parameter, but {zero} have zero or negative."
                )
            x_start, S_start = np.zeros_like(x_a), np.eye(len(x_a))
            lo_s, hi_s = (lower - x_a) / sigma_a, (upper - x_a) / sigma_a
        else:
            x_start, S_start = x_a, S_a
            lo_s, hi_s = lower, upper

        x_low = {n: float(lo) for n, lo in zip(names, lo_s) if np.isfinite(lo)}
        x_up = {n: float(hi) for n, hi in zip(names, hi_s) if np.isfinite(hi)}

        def _to_physical(v: np.ndarray) -> np.ndarray:
            """Map a (possibly normalized) state back to physical units."""
            return x_a + sigma_a * v if self.normalize else v

        # pyOE calls forward(x) with x a pandas Series indexed by `names`.
        #
        # It evaluates the forward model at UNCLAMPED trial states — including
        # ones outside x_lowerLimit/x_upperLimit, which it only resets
        # afterwards.  A forward model that validates its inputs (make_ils
        # rejects a negative fov, for instance) would then raise and abort the
        # whole retrieval.  Clamping here keeps the forward well defined
        # everywhere, and matches what pyOE does to the state anyway.
        def _forward(x_series):
            v = np.array([x_series[n] for n in names], dtype=float)
            return forward(np.clip(_to_physical(v), lower, upper))

        y_vars = list(range(measured.size))
        oe = poe.optimalEstimation(
            names, x_start, S_start, y_vars, measured, S_y, _forward,
            x_lowerLimit=x_low, x_upperLimit=x_up, perturbation=self.perturbation,
        )

        try:
            converged = bool(oe.doRetrieval(maxIter=self.max_iter))
            # When the retrieval does not converge, pyOE leaves x_op / x_op_err
            # / S_op as bare floats (NaN) rather than the Series/DataFrame it
            # returns on success, so indexing them by name raises
            # "'float' object is not subscriptable".  Treat that as a failed
            # fit with a clear message instead of an opaque TypeError.
            if not hasattr(oe.x_op, "__getitem__") or np.isscalar(oe.x_op):
                raise FitError(
                    f"OE did not converge in {self.max_iter} iterations "
                    f"(pyOptimalEstimation returned no solution vector)"
                )
            x_op = _to_physical(np.array([oe.x_op[n] for n in names], dtype=float))
            # Errors and covariance carry the same scaling:
            #   sigma_x = sigma_a * sigma_z,  S_x = diag(sigma_a) S_z diag(sigma_a).
            # The averaging kernel and DOF are INVARIANT under a diagonal
            # rescaling (A = dx_hat/dx_true), so they need no transform.
            scale = sigma_a if self.normalize else np.ones_like(sigma_a)
            perr = {n: float(oe.x_op_err[n]) * float(sc)
                    for n, sc in zip(names, scale)}
            covariance = np.asarray(oe.S_op, dtype=float) * np.outer(scale, scale)
            # oe.A_i is the per-iteration list of averaging kernels — take the
            # converged (last) one.
            averaging_kernel = (np.asarray(oe.A_i[-1], dtype=float)
                                if getattr(oe, "A_i", None) else None)
            dof = float(oe.dgf) if np.isscalar(oe.dgf) or np.ndim(oe.dgf) == 0 else None
            n_iter = int(getattr(oe, "convI", self.max_iter) or self.max_iter)
            message = f"OE {'converged' if converged else 'did not converge'} " \
                      f"in {n_iter} iterations"
        except Exception as exc:                       # numerical failure
            logger.warning("OE fit failed (%s): %s", window_label, exc)
            x_op = x_a
            perr = covariance = averaging_kernel = None
            dof, n_iter = None, 0
            converged, message = False, f"OE failed: {exc}"

        model = forward(x_op)
        resid = measured - model
        rmse = float(np.sqrt(np.mean(resid ** 2)))

        return FitResult(
            success=converged, method="oe", varied_names=tuple(names),
            values=params.unpack(x_op), perror=perr, covariance=covariance,
            wn=xdata, measured=measured, model=model, rmse=rmse,
            nfev=n_iter, message=message,
            window_label=window_label, spectrum_id=spectrum_id,
            averaging_kernel=averaging_kernel, dof=dof,
        )
