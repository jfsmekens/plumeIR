"""
Forward model base class and shared spectral utilities for plumeIR v3.

The ``ForwardModel`` ABC defines the single interface the optimizer sees:
a callable mapping a varied-parameter vector to a model spectrum on the
measurement grid.

Shared pure functions (no side effects, no object state):
    rad             blackbody radiance
    bbt     radiance -> brightness temperature
    apply_ils          convolve a model spectrum with an ILS kernel
    apply_shift_offset shift the model grid and add an offset, then
                       interpolate onto the measurement grid

ILS generation lives in :mod:`plumeIR.forward.ils`; this module owns only the
*application* (convolution + resampling).  Generation moved out of RFMwrapper
because plumeIR fits the ILS parameters and so needs a kernel that is smooth in
``fov`` and ``max_opd`` — see that module's docstring.
"""

from __future__ import annotations

from abc import ABC, abstractmethod

import numpy as np
from scipy.interpolate import interp1d
from scipy.signal import fftconvolve

# Planck functions live in the leaf module plumeIR.physics (numpy-only) so
# both plumeIR.reference and plumeIR.forward can use them without a cycle.
# Re-exported here for backwards compatibility / convenience.
from plumeIR.physics import rad, bbt
from plumeIR.forward.ils import ILSKernel
from plumeIR.parameters.parameter import ParameterKind


# =============================================================================
# Spectral application helpers
# =============================================================================

def _edge_padded(spectrum: np.ndarray, n_k: int) -> np.ndarray:
    """Pad for a 'valid' convolution with an ``n_k``-sample kernel, holding
    the edge values, so the output has the input's length."""
    left = n_k // 2
    return np.pad(spectrum, (left, n_k - 1 - left), mode="edge")


def _broad_mean(spectrum: np.ndarray, half_width: int) -> np.ndarray:
    """Running mean over ``±half_width`` samples, edge values held beyond the
    grid; O(n) through a cumulative sum."""
    padded = np.pad(spectrum, half_width + 1, mode="edge")
    c = np.cumsum(padded)
    w = 2 * half_width + 1
    return (c[w:] - c[:-w])[: spectrum.size] / w


def apply_ils(spectrum: np.ndarray, kernel: np.ndarray | ILSKernel) -> np.ndarray:
    """
    Convolve a model spectrum with an ILS kernel.

    Uses FFT-based convolution (``scipy.signal.fftconvolve``), O(n log n).  The
    spectrum is padded with its edge values rather than zeros, so samples within
    a kernel half-width of the model grid's ends are not pulled towards zero (for
    a model padding wider than the kernel, as plumeIR's is, this does not change
    any value inside the fit window).

    Given an :class:`~plumeIR.forward.ils.ILSKernel`, the line shape's mass
    beyond the trimmed support (``tail_mass``) is applied as that fraction of a
    broad running mean of the spectrum, which is what the far tails sample,
    instead of being folded into the line core by renormalisation:

        F_ils = (1 - m)·(F ⊛ k) + m·mean(F, ±tail_halfwidth)

    Measured on synthetic HCl (2690-2830 cm⁻¹, max_opd 1.037 cm): column bias
    with a triangle window −0.83% before (estimated pedestal, renormalised),
    −0.02 to +0.04% after; NB medium and boxcar ≤ 0.02%.

    Parameters
    ----------
    spectrum : np.ndarray
        High-resolution model spectrum, shape ``(n_model,)``.
    kernel : ILSKernel or np.ndarray
        Kernel from :func:`plumeIR.forward.ils.make_ils`, or a bare array
        normalised to unit sum (no tail term; used by tests).

    Returns
    -------
    np.ndarray
        Convolved spectrum, same shape as ``spectrum``.
    """
    if isinstance(kernel, ILSKernel):
        core = fftconvolve(_edge_padded(spectrum, kernel.kernel.size),
                           kernel.kernel, mode="valid")
        m = kernel.tail_mass
        if m == 0.0 or kernel.tail_halfwidth <= 0:
            return core
        return (1.0 - m) * core + m * _broad_mean(spectrum, kernel.tail_halfwidth)

    kernel = np.asarray(kernel)
    return fftconvolve(_edge_padded(spectrum, kernel.size), kernel, mode="valid")


def apply_shift_offset(
    spectrum:  np.ndarray,
    wn_model:  np.ndarray,
    wn_meas:   np.ndarray,
    shift:     float,
    offset:    float,
) -> np.ndarray:
    """
    Apply a wavenumber shift and additive offset, then interpolate onto the
    measurement grid.

    The shift is applied to the *model* grid (``wn_model + shift``) and the
    shifted model is interpolated onto the measurement grid ``wn_meas``.
    The offset is added after interpolation.

    Parameters
    ----------
    spectrum : np.ndarray
        Model spectrum on ``wn_model``, shape ``(n_model,)``.
    wn_model : np.ndarray
        Model wavenumber grid [cm⁻¹], shape ``(n_model,)``.
    wn_meas : np.ndarray
        Measurement wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    shift : float
        Wavenumber shift [cm⁻¹] applied to the model grid.
    offset : float
        Additive offset applied after interpolation.

    Returns
    -------
    np.ndarray
        Model spectrum on the measurement grid, shape ``(n_wn,)``.
    """
    f = interp1d(
        wn_model + shift, spectrum,
        kind="cubic", bounds_error=False, fill_value="extrapolate",
    )
    return f(wn_meas) + offset


def gas_optical_depth(lut, scaling: float, temp: float) -> np.ndarray:
    """
    Optical depth for one gas species, handling 1-D and 2-D (continuum) LUTs.

    For a 1-D gas LUT the OD is ``scaling × lut.query(temp)``.  For a 2-D
    continuum gas (H2O), the scaling maps to an absolute VMR and the LUT
    returns OD directly: ``lut.query(temp, scaling × lut.ref_vmr)``.

    Parameters
    ----------
    lut : GasLUT
        The gas look-up table.
    scaling : float
        Dimensionless scaling factor (the fit parameter).
    temp : float
        Temperature [K] at which to query the absorption shape.

    Returns
    -------
    np.ndarray
        Optical depth spectrum, shape ``(n_wn,)``.
    """
    if lut.is_2d:
        return lut.query(temp, scaling * lut.ref_vmr)
    return scaling * lut.query(temp)


def build_background_poly(
    coeffs: list[float] | np.ndarray,
    wn_model: np.ndarray,
) -> np.ndarray | float:
    """
    Construct a multiplicative background polynomial on the model grid.

    Parameters
    ----------
    coeffs : list[float] or np.ndarray
        Polynomial coefficients in ascending order ``[poly0, poly1, …]``.
        Empty -> returns scalar ``1.0`` (no background).
    wn_model : np.ndarray
        Model wavenumber grid [cm⁻¹].

    Returns
    -------
    np.ndarray or float
        Background polynomial evaluated on ``wn_model``, or ``1.0`` if no
        coefficients were given.

    Notes
    -----
    The polynomial is evaluated on a grid centred and scaled to [-1, 1]
    across the window to keep the coefficients well-conditioned, matching
    the convention used when the polynomial parameters are defined.
    """
    coeffs = list(coeffs)
    if len(coeffs) == 0:
        return 1.0
    # Centre and scale the grid to [-1, 1] for conditioning
    lo, hi = wn_model[0], wn_model[-1]
    x = 2.0 * (wn_model - lo) / (hi - lo) - 1.0
    # coeffs in ascending order; np.polyval expects descending
    return np.polyval(coeffs[::-1], x)


# =============================================================================
# ForwardModel ABC
# =============================================================================

class ForwardModel(ABC):
    """
    Abstract base class for all geometry forward models.

    A forward model is a callable that maps a varied-parameter vector to a
    model spectrum on the measurement grid.  It holds read-only references
    to a ``Reference``, ``ParameterSet``, geometry, and ``Retrieval`` — it
    never mutates them.

    Subclasses implement :meth:`__call__`.
    """

    @abstractmethod
    def __call__(self, params: np.ndarray) -> np.ndarray:
        """
        Evaluate the model spectrum for a varied-parameter vector.

        Parameters
        ----------
        params : np.ndarray
            Varied-parameter vector, shape ``(n_varied,)``.

        Returns
        -------
        np.ndarray
            Model spectrum on the measurement grid, shape ``(n_wn,)``.
        """
        raise NotImplementedError

    def continuum(self, params: np.ndarray) -> np.ndarray | None:
        """
        The gas-free continuum the model carries, on the measurement grid.

        What a spectrum would look like with every absorber removed: the
        background polynomial times whatever multiplies it (layer: source
        emissivity and instrument residual), shifted and offset like the model.
        Drawn under the fit as v2's dashed red ``bkg`` line
        (``plumeIR_v2_archive/analysis.py``, lines 2099-2117).  Smooth, so no
        ILS is applied.  Emission overrides this with its clear sky (the model
        with the plume removed), since there the polynomial scales the whole
        radiance.  ``None`` where a geometry defines neither.

        Parameters
        ----------
        params : np.ndarray
            Varied-parameter vector, shape ``(n_varied,)``.
        """
        return None

    def _polynomial_continuum(self, params: np.ndarray,
                              factor: np.ndarray | float = 1.0) -> np.ndarray:
        """``factor × polynomial`` on the measurement grid, shifted and offset."""
        p = self.params.unpack(params)
        coeffs = [p[n] for n in self.params.names_by_kind(ParameterKind.POLYNOMIAL)]
        bg = build_background_poly(coeffs, self.ref.wn) * factor
        bg = np.broadcast_to(bg, self.ref.wn.shape).astype(float)
        return apply_shift_offset(bg, self.ref.wn, self.wn_meas,
                                  shift=p.get("shift", 0.0),
                                  offset=p.get("offset", 0.0))

    def curve_fit_callable(self):
        """
        Return a ``(wn, *params) -> model`` callable for ``scipy.curve_fit``.

        scipy.optimize.curve_fit calls ``f(xdata, *popt)``.  This adapter
        ignores ``xdata`` (the measurement grid is already held internally)
        and unpacks the positional parameters into a vector.
        """
        def _f(wn_meas, *p):
            return self(np.asarray(p))
        return _f
