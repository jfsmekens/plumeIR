"""
Straight-line regression with errors in both coordinates (York et al. 2004).

A gas ratio is the slope of one plume column against another, and both
columns carry fit errors of comparable relative size, so ordinary least
squares (errors in ``y`` only) biases the slope towards zero.  The maximum
likelihood line for independent Gaussian errors in ``x`` and ``y``, possibly
correlated point by point, minimises

    S = Σ W_i (y_i − b·x_i − a)²,
    W_i = ω(x_i)·ω(y_i) / (ω(x_i) + b²·ω(y_i) − 2·b·r_i·α_i),
    ω = 1/σ²,  α_i = sqrt(ω(x_i)·ω(y_i)),

which is the objective of weighted orthogonal distance regression (ODRPACK,
``scipy.odr``) for a straight line when ``r_i = 0``.  York et al. (2004)
give the solution as a fixed-point iteration on ``b`` with closed-form
standard errors; this module implements it.

References
----------
York, D., Evensen, N. M., López Martínez, M. and De Basabe Delgado, J.
(2004).  Unified equations for the slope, intercept, and standard errors of
the best straight line.  Am. J. Phys. 72, 367-375.

Cantrell, C. A. (2008).  Technical Note: Review of methods for linear
least-squares fitting of data and application to atmospheric chemistry
problems.  Atmos. Chem. Phys. 8, 5477-5487.  Table 2 is the published test
case used by ``test/test_regression.py``.

Notes
-----
**Two kinds of standard error.**  The York errors (``slope_err_internal``)
assume the input σ are the whole story: they follow from the input errors
alone.  ``mswd = S/(n − 2)`` measures whether they are: its expected value is
1.  Where the points scatter more than their errors allow (``mswd > 1``) —
in plume data the usual case, since the composition itself varies and fit
errors omit systematic terms — the internal errors understate the
uncertainty of the line, and the quoted error is scaled up by
``sqrt(mswd)``.  It is never scaled *down*: an ``mswd`` below 1 says the
input errors are pessimistic, not that the line is better known than they
imply.  Without input errors (unit weights) the internal errors carry no
absolute scale, and the quoted error is always the scaled one.

``scipy.odr`` (ODRPACK) always scales by ``mswd``, below 1 as well; with
``mswd ≥ 1`` the two agree.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass

import numpy as np

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class LineFit:
    """
    Result of :func:`york_fit`: ``y = slope·x + intercept``.

    Attributes
    ----------
    slope, intercept : float
    slope_err, intercept_err : float
        Quoted 1σ errors: the internal errors scaled by ``sqrt(mswd)`` when
        ``mswd > 1`` or when the fit is unweighted, unscaled otherwise.
    slope_err_internal, intercept_err_internal : float
        1σ errors from the input errors alone (York et al. 2004, eqs. 13).
    mswd : float
        ``S / (n − 2)``, the reduced chi-square of the weighted residuals.
    n : int
        Points used.
    weighted : bool
        False when the line was fitted with unit weights (no input errors).
    iterations : int
        Fixed-point iterations to convergence.
    """
    slope:                  float
    intercept:              float
    slope_err:              float
    intercept_err:          float
    slope_err_internal:     float
    intercept_err_internal: float
    mswd:                   float
    n:                      int
    weighted:               bool
    iterations:             int


class RegressionError(ValueError):
    """The line cannot be fitted (too few points, no spread, no convergence)."""


def york_fit(
    x: np.ndarray,
    y: np.ndarray,
    sx: np.ndarray | None = None,
    sy: np.ndarray | None = None,
    r: np.ndarray | float = 0.0,
    *,
    rtol: float = 1e-12,
    max_iter: int = 200,
) -> LineFit:
    """
    Best straight line through points with errors in ``x`` and ``y``.

    Parameters
    ----------
    x, y : np.ndarray
        Coordinates, shape ``(n,)``, finite.  ``n ≥ 3``.
    sx, sy : np.ndarray or None
        1σ errors of ``x`` and ``y``, shape ``(n,)``, finite and positive.
        Give both or neither; with neither, every point has unit weight in
        both coordinates (unweighted orthogonal regression, which depends on
        the relative scale of ``x`` and ``y``).
    r : np.ndarray or float
        Correlation coefficient between the errors of ``x_i`` and ``y_i``, in
        ``(−1, 1)``; scalar or shape ``(n,)``.  Zero for columns fitted in
        separate spectral windows.  Ignored when unweighted.
    rtol : float
        Convergence: relative change of the slope between iterations.
    max_iter : int
        Iterations before :class:`RegressionError`.

    Returns
    -------
    LineFit

    Raises
    ------
    RegressionError
        Fewer than three points, no spread in ``x``, invalid errors or
        correlations, or no convergence.
    """
    # Ref: York et al. (2004), section III, steps 1-8.
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    if x.shape != y.shape or x.ndim != 1:
        raise RegressionError(f"x and y must be 1-D and of equal length, "
                              f"got shapes {x.shape} and {y.shape}")
    n = x.size
    if n < 3:
        raise RegressionError(f"A line with errors needs at least 3 points, got {n}")
    if not (np.all(np.isfinite(x)) and np.all(np.isfinite(y))):
        raise RegressionError("x and y must be finite")
    if np.ptp(x) == 0:
        raise RegressionError("x has no spread; the slope is undefined")

    if (sx is None) != (sy is None):
        raise RegressionError("Give both sx and sy, or neither")
    weighted = sx is not None
    if weighted:
        sx = np.broadcast_to(np.asarray(sx, dtype=float), x.shape)
        sy = np.broadcast_to(np.asarray(sy, dtype=float), x.shape)
        for name, s in (("sx", sx), ("sy", sy)):
            if not (np.all(np.isfinite(s)) and np.all(s > 0)):
                raise RegressionError(f"{name} must be finite and positive")
        rr = np.broadcast_to(np.asarray(r, dtype=float), x.shape)
        if not np.all(np.abs(rr) < 1):
            raise RegressionError("Error correlations r must lie in (-1, 1)")
        # Work in units of each coordinate's smallest error, so that columns
        # of ~1e18 with errors of ~1e16 give O(1) weights rather than ~1e-32.
        # The weights below are 1/σ² in those units; the line and its
        # errors are converted back at the end.
        kx, ky = sx.min(), sy.min()
        wx = (kx / sx) ** 2
        wy = (ky / sy) ** 2
    else:
        wx = wy = np.ones_like(x)
        rr = np.zeros_like(x)
        kx = ky = 1.0
    X, Y = x / kx, y / ky
    alpha = np.sqrt(wx * wy)

    b = np.polyfit(X, Y, 1)[0]                          # step 1: OLS start
    for it in range(1, max_iter + 1):
        W = wx * wy / (wx + b * b * wy - 2.0 * b * rr * alpha)   # step 3
        Xbar = np.sum(W * X) / np.sum(W)                         # step 4
        Ybar = np.sum(W * Y) / np.sum(W)
        U, V = X - Xbar, Y - Ybar                                # step 5
        beta = W * (U / wy + b * V / wx - (b * U + V) * rr / alpha)
        b_new = np.sum(W * beta * V) / np.sum(W * beta * U)      # step 6
        converged = abs(b_new - b) <= rtol * max(abs(b_new), np.finfo(float).tiny)
        b = b_new
        if converged:
            break
    else:
        raise RegressionError(f"York regression did not converge in {max_iter} "
                              f"iterations (last slope {b * ky / kx:.6g})")

    # Steps 7-8 at the converged slope.
    W = wx * wy / (wx + b * b * wy - 2.0 * b * rr * alpha)
    Xbar = np.sum(W * X) / np.sum(W)
    Ybar = np.sum(W * Y) / np.sum(W)
    U, V = X - Xbar, Y - Ybar
    beta = W * (U / wy + b * V / wx - (b * U + V) * rr / alpha)
    a = Ybar - b * Xbar
    xfit = Xbar + beta                                   # adjusted x
    xbar = np.sum(W * xfit) / np.sum(W)
    u = xfit - xbar
    var_b = 1.0 / np.sum(W * u * u)
    var_a = 1.0 / np.sum(W) + xbar * xbar * var_b

    S = np.sum(W * (Y - b * X - a) ** 2)
    mswd = S / (n - 2)                                   # dimensionless
    # Back to the caller's units (x = kx·X, y = ky·Y).
    slope = b * ky / kx
    intercept = a * ky
    se_b = np.sqrt(var_b) * ky / kx
    se_a = np.sqrt(var_a) * ky
    scale = np.sqrt(mswd) if (not weighted or mswd > 1.0) else 1.0
    return LineFit(slope=float(slope), intercept=float(intercept),
                   slope_err=float(se_b * scale), intercept_err=float(se_a * scale),
                   slope_err_internal=float(se_b), intercept_err_internal=float(se_a),
                   mswd=float(mswd), n=int(n), weighted=weighted, iterations=it)
