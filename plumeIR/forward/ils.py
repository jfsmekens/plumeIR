"""
Instrument line shape (ILS) generation for plumeIR v3.

The ILS describes the *spectrometer*, not the atmosphere, so it belongs to
plumeIR's measurement model.  It was previously delegated to
``rfmwrapper.makeILS``; ownership moved here because plumeIR **fits** the ILS
parameters and therefore needs a strictly stronger guarantee than a
convolution-only consumer does: the kernel must be a smooth function of ``fov``
and ``max_opd``, so that a finite-difference Jacobian can see them.

Ported from ``rfmwrapper.functions.makeILS`` (which in turn descends from
plumeIR v2 ``analysis.py::makeILS``, lines 88-186).  The apodization functions
are unchanged.  Two things are fixed, both discretization bugs that made the
kernel a *staircase* in its own parameters:

1. **FOV broadening is applied analytically in the OPD domain.**  Convolving by
   a rectangle of width ``W`` in wavenumber is exactly multiplication by
   ``sinc(W·x)`` in optical path difference.  The old code built a boxcar of
   ``n_box = ceil(W · nper_wn)`` grid points and convolved — an integer count,
   so the width only changed when ``fov`` moved by ~1e-3 (for a 2760 cm⁻¹
   window), while ``scipy.optimize.least_squares`` probes with a step of
   ~7e-10.  The width contributed *exactly zero* to the Jacobian; only the
   FOV-induced shift did, and that is collinear with ``nu_shift`` — measured at
   R = 0.95-0.97 on real data.  ``fov`` was in effect a redundant shift
   parameter, and its per-spectrum wander degraded retrieved gas ratios by a
   factor of ~2.5.  The sinc form is exact rather than discretized, smooth at
   any magnitude, and has no sub-grid floor.

2. **The OPD grid no longer depends on ``max_opd``.**  The old code built
   ``n = int(max_opd · nper_wn)`` samples (forced odd, so effectively stepping
   by two) spanning ``[0, max_opd]``, which quantized both the sample count and
   the spacing.  Here the grid is fixed and ``max_opd`` enters only through the
   apodization window's argument, with a fractional weight on the sample
   straddling the truncation edge.  ``fit_max_opd`` was never enabled in
   practice, so this defect was latent, but it is the same class of bug.

3. **The kernel is centred on its own wavenumber grid.**  The transform used
   a manual roll by ``n_tot // 2`` against a ``linspace`` grid whose zero sits
   at ``(n_tot - 1) / 2``, leaving the kernel off-centre by half a sample for
   even ``n_tot`` and a full sample for odd — and ``n_tot = nper_wn**2`` is odd
   exactly when ``nper_wn`` is.  A free ``nu_shift`` absorbed it, so gas columns
   were largely protected, but ``nu_shift`` was measuring the model grid rather
   than the instrument, and the odd-``nper_wn`` case cost real fits: at
   ``nper_wn = 25`` four of fifty emission spectra failed with ``fov`` pinned at
   its lower bound, against none at 10 or 40.  Both halves of the pair are now
   ``fftshift``-ed, so DC and zero coincide by construction.

The kernel is trimmed to a length set by ``trim_opd`` (default: the nominal
``max_opd``) rather than the live ``max_opd``, so that the kernel *length* stays
constant while its *shape* varies during a fit.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass

import numpy as np

from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Norton & Beer (1976), J. Opt. Soc. Am. 66, 259.
#: The 'weak' coefficients sum to 0.964 at OPD=0, not 1.0 — a property of the
#: published values, not an error.
_NB_COEFFS = {
    "weak":   [0.348093, -0.087577, 0.703484, 0.0],
    "strong": [0.045335,  0.0,      0.554883, 0.399782],
    "medium": [0.152442, -0.136176, 0.983734, 0.0],
}

#: Sets the OPD grid length: ``n_tot = factor * nper_wn**2``.
#:
#: **This must stay 1.0.**  The kernel's wavenumber spacing is
#: ``1 / (n_tot * dx) = 1 / (factor * nper_wn)``, and convolution requires that
#: to equal the model grid spacing ``1 / nper_wn``.  The constant is kept named
#: rather than inlined only because it documents that relation; it is not a
#: tunable knob, and :func:`make_ils` asserts it.  (A wide FOV box widens the
#: wavenumber span by sampling the OPD more finely, which keeps that spacing —
#: see ``_oversampling``.)
_DEFAULT_KERNEL_OPD_FACTOR = 1.0

#: Half-width of the untrimmed ILS core, in units of ``1 / trim_opd``: the
#: kernel keeps +/-5 sinc limbs of the nominal OPD.
_CORE_LIMBS = 5.0

#: Half-width of the broad mean standing in for the far tails, in units of the
#: kernel support S.  A 1/x² tail beyond S has its median weight at 2S.  The
#: synthetic HCl column bias it leaves is -0.02% (1S) to +0.04% (2S) for a
#: triangle window, <= 0.02% for boxcar and NB medium.
_TAIL_MEAN_FACTOR = 2.0

_WARNED_UNBOUNDED_FOV = False


def _oversampling(support: float, nper_wn: float) -> int:
    """
    Integer OPD oversampling that keeps a kernel of half-width ``support`` clear
    of the FFT grid's edges.

    The OPD spacing ``dx = 1 / (k·nper_wn)`` with ``n_tot = k·nper_wn²`` samples
    leaves the wavenumber spacing at ``1 / nper_wn`` and widens the span to
    ``±k·nper_wn / 2``.  The span must hold the kernel (else it wraps round), and
    its outer half must be empty, because the baseline is estimated from the
    first quarter of the grid.  Hence ``k·nper_wn / 2 >= 2·support``.

    Parameters
    ----------
    support : float
        Kernel half-width to accommodate [cm⁻¹].
    nper_wn : float
        Model grid points per cm⁻¹.

    Returns
    -------
    int
        Oversampling factor, at least 1.  It is 1 at every FOV met in practice
        (the default ±5-limb core is 4.8 cm⁻¹ against a 12.5 cm⁻¹ half-span at
        ``nper_wn`` 25), so the usual kernel is unchanged.
    """
    return max(1, int(np.ceil(4.0 * support / nper_wn - 1e-9)))


def kernel_trim_fov(fov_param) -> float | None:
    """
    The FOV that fixes a forward model's kernel length for a whole fit.

    The kernel support must cover the widest FOV box the fit can reach, and must
    not change length while ``fov`` is perturbed (a sample appearing at the edge
    would be a step in the finite-difference Jacobian).  A varied ``fov``
    therefore uses its upper bound; a fixed one its value.

    Parameters
    ----------
    fov_param : Parameter
        The ``fov`` entry of a :class:`ParameterSet`.

    Returns
    -------
    float or None
        FOV [rad] for :func:`make_ils`'s ``trim_fov``; ``None`` (support follows
        the live ``fov``) when a varied ``fov`` has no finite upper bound.
    """
    if not fov_param.vary:
        return float(fov_param.value)
    upper = float(fov_param.bounds[1])
    if not np.isfinite(upper):
        global _WARNED_UNBOUNDED_FOV
        if not _WARNED_UNBOUNDED_FOV:          # called per forward evaluation
            logger.warning(
                "fov is fitted with no finite upper bound; the ILS kernel "
                "length will follow the fitted value, which can step the "
                "Jacobian."
            )
            _WARNED_UNBOUNDED_FOV = True
        return None
    return upper


@dataclass(frozen=True)
class ILSKernel:
    """
    A computed instrument line shape.

    Parameters
    ----------
    kernel : np.ndarray
        Convolution kernel normalised to unit sum, shape ``(n_k,)``.
    wn_grid : np.ndarray
        Kernel wavenumber offsets [cm⁻¹], shape ``(n_k,)``, centred on zero.
    opd_grid : np.ndarray
        Optical path difference grid [cm], shape ``(n_tot,)``.
    apod : np.ndarray
        Apodization window on ``opd_grid`` including FOV self-apodization,
        shape ``(n_tot,)``.
    fov_width : float
        FOV broadening width [cm⁻¹].
    fov, max_opd, wn_center : float
        The parameters the kernel was built from.
    apod_type : str
        Apodization function name.
    tail_mass : float
        Fraction of the full (untrimmed) line shape lying beyond the kernel's
        support, signed.  ~0.4% for a triangle window, ~0 for NB medium or Hann.
    tail_halfwidth : int
        Half-width [samples] of the broad mean that stands in for those far
        tails in :func:`plumeIR.forward.base.apply_ils`.
    """
    kernel:    np.ndarray
    wn_grid:   np.ndarray
    opd_grid:  np.ndarray
    apod:      np.ndarray
    fov_width: float
    fov:       float
    max_opd:   float
    wn_center: float
    apod_type: str
    tail_mass: float = 0.0
    tail_halfwidth: int = 0


def _apodization(x: np.ndarray, apod_type: str, max_opd: float,
                 fwhm_wn: float | None) -> np.ndarray:
    """
    Evaluate the apodization window on normalised OPD ``x = opd / max_opd``.

    Every branch is a continuous function of ``x``, so the window is smooth in
    ``max_opd``.  Values outside ``x <= 1`` are handled by the caller's edge
    weighting, not here.

    Parameters
    ----------
    x : np.ndarray
        Normalised OPD, shape ``(n_tot,)``.  May exceed 1.
    apod_type : str
        Apodization name (case-insensitive, matched by substring).
    max_opd : float
        Maximum optical path difference [cm]; only the Gaussian branch uses it.
    fwhm_wn : float or None
        Target spectral FWHM [cm⁻¹] for the Gaussian branch.

    Returns
    -------
    np.ndarray
        Window values, shape ``(n_tot,)``.
    """
    key = apod_type.lower()

    if "boxcar" in key or "uniform" in key:
        return np.ones_like(x)

    if "triang" in key:
        return 1.0 - np.abs(x)

    if "nb" in key or "norton" in key or "beer" in key:
        which = "strong" if "strong" in key else ("weak" if "weak" in key else "medium")
        c = _NB_COEFFS[which]
        t = x ** 2
        return sum(c[i] * (1 - t) ** i for i in range(4))

    if "hann" in key:
        return 0.5 + 0.5 * np.cos(np.pi * x)

    if "hamming" in key:
        return 0.54 + 0.46 * np.cos(np.pi * x)

    if "happ" in key or "genzel" in key:
        alpha = 2.5
        return np.exp(-alpha * np.abs(x)) * (0.5 + 0.5 * np.cos(np.pi * x))

    if "blackman" in key or "harris" in key:
        return 0.42 + 0.5 * np.cos(np.pi * x) + 0.08 * np.cos(2 * np.pi * x)

    if "nuttall" in key:
        return (0.355768 + 0.487396 * np.cos(np.pi * x)
                + 0.144232 * np.cos(2 * np.pi * x)
                + 0.012604 * np.cos(3 * np.pi * x))

    if "kaiser" in key:
        beta = 8.6
        return np.i0(beta * np.sqrt(np.clip(1 - x ** 2, 0.0, None))) / np.i0(beta)

    if "tukey" in key:
        r = 0.5
        w = np.ones_like(x)
        mask = x > 1 - r
        w[mask] = 0.5 * (1 - np.cos(np.pi * (1 - x[mask]) / r))
        return w

    if "cos" in key:
        return np.cos(np.pi * x / 2)

    if "lorenz" in key or "lorentz" in key:
        return np.exp(-np.abs(x))

    if "gauss" in key:
        if fwhm_wn is not None:
            alpha = np.pi * max_opd * fwhm_wn / (2.0 * np.sqrt(np.log(2.0)))
        else:
            alpha = 2.24
        return np.exp(-(alpha * x) ** 2)

    raise PlumeIRError(
        f"Unknown apod_type {apod_type!r}. Expected one of: Boxcar, Triangular, "
        f"NB weak/medium/strong, Hann, Hamming, Happ-Genzel, Blackman-Harris, "
        f"Nuttall, Kaiser, Tukey, Cosine, Lorenz, Gaussian."
    )


def _truncation_weight(opd: np.ndarray, max_opd: float, dx: float) -> np.ndarray:
    """
    Continuous truncation of the interferogram at ``max_opd``.

    Returns 1 for samples wholly inside the OPD limit, 0 for those wholly
    outside, and the fractional overlap for the sample straddling the edge.
    This is what makes the window smooth in ``max_opd``: a hard ``opd <=
    max_opd`` mask would step by a whole sample and reintroduce the staircase.

    Parameters
    ----------
    opd : np.ndarray
        OPD grid [cm], shape ``(n_tot,)``.
    max_opd : float
        Truncation point [cm].
    dx : float
        OPD sample spacing [cm].

    Returns
    -------
    np.ndarray
        Weights in [0, 1], shape ``(n_tot,)``.
    """
    # Overlap of each sample's cell [opd - dx/2, opd + dx/2] with [0, max_opd].
    return np.clip((max_opd - opd) / dx + 0.5, 0.0, 1.0)


def make_ils(
    fov:       float = 0.03,
    max_opd:   float = 1.8,
    nper_wn:   float = 25.0,
    wn_center: float = 1000.0,
    apod_type: str = "NB medium",
    fwhm_wn:   float | None = None,
    trim_opd:  float | None = None,
    trim_fov:  float | None = None,
) -> ILSKernel:
    """
    Compute an FT-IR instrument line shape kernel.

    Smooth in both ``fov`` and ``max_opd``, so the kernel can be differentiated
    numerically during a fit — see the module docstring for why that matters.

    Parameters
    ----------
    fov : float
        Full-angle field of view [rad].  For an extended source this is the
        *effective* FOV — the aperture fraction the source fills — which is a
        genuine per-spectrum quantity for a dynamic source.
    max_opd : float
        Maximum optical path difference [cm].
    nper_wn : float
        Kernel grid points per cm⁻¹ (the reciprocal of the model spacing).
    wn_center : float
        Centre wavenumber of the window [cm⁻¹], setting the FOV width via
        ``fov_width = wn_center · (fov/2)² / 2``.  Note this is the window
        *centre*, not its width — plumeIR v2 passed half the window width here,
        which collapsed the FOV effect to nothing (``n_box = 1``).
    apod_type : str
        Apodization function name.
    fwhm_wn : float or None
        Target spectral FWHM [cm⁻¹]; Gaussian apodization only.
    trim_opd : float or None
        OPD used to set the kernel's trimmed length (``±5 / trim_opd``).
        ``None`` uses ``max_opd``.  Pass the *nominal* max_opd when fitting, so
        the kernel length stays fixed while its shape varies.
    trim_fov : float or None
        FOV [rad] whose box width is added to the kernel's half-width, so the
        kernel holds the whole FOV box (the shift puts it entirely on one side
        of zero).  ``None`` uses ``fov``.  Pass the fitted ``fov``'s upper bound
        when fitting (:func:`kernel_trim_fov`), so the length stays fixed.  A
        live ``fov`` above ``trim_fov`` widens the support to fit it.

    Returns
    -------
    ILSKernel

    Raises
    ------
    PlumeIRError
        For non-positive ``max_opd``/``nper_wn``/``wn_center``, negative ``fov``,
        or an unknown ``apod_type``.
    """
    if max_opd <= 0:
        raise PlumeIRError(f"max_opd must be positive, got {max_opd}")
    if nper_wn <= 0:
        raise PlumeIRError(f"nper_wn must be positive, got {nper_wn}")
    if wn_center <= 0:
        raise PlumeIRError(f"wn_center must be positive, got {wn_center}")
    if fov < 0:
        raise PlumeIRError(f"fov must be non-negative, got {fov}")

    # ------------------------------------------------------------------
    # Kernel support: the +/-5-limb core of the nominal OPD plus the widest
    # FOV box the kernel must hold.  Set by trim_opd/trim_fov, not the live
    # parameters, so the length is constant during a fit.
    # ------------------------------------------------------------------
    trim = float(trim_opd) if trim_opd is not None else max_opd
    box_fov = fov if trim_fov is None else max(float(trim_fov), fov)
    box_width = wn_center * (box_fov / 2.0) ** 2 / 2.0   # [cm⁻¹]
    support = _CORE_LIMBS / trim + box_width

    # ------------------------------------------------------------------
    # Fixed OPD grid — independent of max_opd, so nothing about the
    # sampling changes as the parameter varies during a fit.
    # ------------------------------------------------------------------
    if _DEFAULT_KERNEL_OPD_FACTOR != 1.0:
        raise PlumeIRError(
            "_DEFAULT_KERNEL_OPD_FACTOR must be 1.0: the kernel's wavenumber "
            "spacing is 1/(factor*nper_wn) and must match the model grid's "
            f"1/nper_wn, but factor={_DEFAULT_KERNEL_OPD_FACTOR}."
        )
    k = _oversampling(support, nper_wn)
    dx = 1.0 / (k * nper_wn)                             # OPD spacing [cm]
    n_tot = int(round(k * _DEFAULT_KERNEL_OPD_FACTOR * nper_wn * nper_wn))
    opd = np.arange(n_tot) * dx                          # [cm]

    # ------------------------------------------------------------------
    # Apodization window, truncated continuously at max_opd.
    # ------------------------------------------------------------------
    x = opd / max_opd
    apod = _apodization(x, apod_type, max_opd, fwhm_wn)
    apod = apod * _truncation_weight(opd, max_opd, dx)

    # ------------------------------------------------------------------
    # FOV self-apodization, applied analytically.
    #
    # Broadening by a rectangle of width fov_width in wavenumber is exactly
    # multiplication by sinc(fov_width · opd) in the OPD domain (np.sinc is
    # the normalised sin(pi z)/(pi z)).  Exact, smooth in fov, no grid floor.
    # ------------------------------------------------------------------
    fov_width = wn_center * (fov / 2.0) ** 2 / 2.0       # [cm⁻¹]
    apod = apod * np.sinc(fov_width * opd)

    # ------------------------------------------------------------------
    # OPD -> wavenumber.
    # ------------------------------------------------------------------
    # fftshift BOTH halves of the transform pair, so the kernel's DC component
    # and wn_grid's zero coincide by construction.
    #
    # The previous form rolled by ``mid = n_tot // 2`` but built the grid with
    # ``linspace(..., n_tot)``, which puts zero at index ``(n_tot - 1) / 2``.
    # Those differ by half a sample for even ``n_tot`` and a FULL sample for
    # odd — and ``n_tot = nper_wn**2`` is odd exactly when ``nper_wn`` is.
    # Measured with ``fov = 0`` (no intentional shift, so the peak must sit at
    # zero) the offset was exactly 0.500 and 1.000 samples.
    #
    # The real part of the FFT of a ONE-sided interferogram is (T + a0)/2,
    # where T is the two-sided line shape and a0 = apod[0]: every bin carries an
    # exact pedestal of a0/2.  It used to be ESTIMATED as the mean of the
    # grid's outer quarter, which also holds real line-shape tail — for a
    # triangle window (1/x² sinc² tails) that subtracted part of the kernel
    # itself: synthetic HCl column bias −0.83% against −0.39% with the exact
    # pedestal.  The full (untrimmed) mass of T/2 is then exactly n_tot·a0/2.
    a0 = float(apod[0])
    if not a0 > 0.0:
        raise PlumeIRError(
            f"apodization at zero OPD is {a0}; check apod_type={apod_type!r}."
        )
    kernel  = np.fft.fftshift(np.fft.fft(apod).real) - 0.5 * a0
    wn_grid = np.fft.fftshift(np.fft.fftfreq(n_tot, d=dx))
    full_mass = 0.5 * a0 * n_tot

    # ------------------------------------------------------------------
    # FOV shift: a finite FOV displaces line centres as well as broadening
    # them.  Smooth in fov already (linear interpolation of a shifted grid).
    # ------------------------------------------------------------------
    shift = fov_width / 2.0
    kernel = np.interp(wn_grid, wn_grid - shift, kernel)

    # ------------------------------------------------------------------
    # Trim to the support (symmetric, so zero stays the centre sample that
    # apply_ils's centred convolution assumes), then normalise.  Trimming at the
    # core alone cut the shifted FOV box: 31-57% of the kernel at fov 0.15.
    # ------------------------------------------------------------------
    keep = np.abs(wn_grid) <= support
    if not keep.any():
        raise PlumeIRError(
            f"ILS trim removed every sample (trim_opd={trim}, "
            f"grid spacing={1.0 / nper_wn:.4g} cm⁻¹) — nper_wn is too coarse "
            f"for this OPD."
        )
    wn_grid, kernel = wn_grid[keep], kernel[keep]

    total = kernel.sum()
    if not np.isfinite(total) or total == 0.0:
        raise PlumeIRError(
            f"ILS kernel does not normalise (sum={total}); check fov={fov}, "
            f"max_opd={max_opd}, apod_type={apod_type!r}."
        )
    # The trimmed kernel is reported at unit sum; the mass it lacks is carried
    # separately, so apply_ils can give it to the far tails instead of the line
    # core.  Renormalising alone hands it to the core, deepening every line:
    # the triangle window's −0.4% column bias at this support.
    tail_mass = 1.0 - total / full_mass
    kernel = kernel / total

    return ILSKernel(
        kernel=kernel, wn_grid=wn_grid, opd_grid=opd, apod=apod,
        fov_width=float(fov_width), fov=float(fov), max_opd=float(max_opd),
        wn_center=float(wn_center), apod_type=apod_type,
        tail_mass=float(tail_mass),
        tail_halfwidth=int(round(_TAIL_MEAN_FACTOR * support * nper_wn)),
    )
