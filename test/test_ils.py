"""
Tests for plumeIR's instrument line shape generation.

Beyond the usual correctness checks, these codify the property the module
exists for: the kernel must be **smooth in fov and max_opd**, because those
parameters are fitted and a finite-difference Jacobian has to see them.

The regression at the heart of this file: the previous implementation built the
FOV broadening as a boxcar of ``ceil(fov_width * nper_wn)`` grid points, so the
kernel only changed when fov moved by ~1e-3, while least_squares probes with
~7e-10.  ``test_fov_sensitivity_at_optimizer_step`` fails against that
implementation and passes against this one.

Run with:  pytest test/test_ils.py -v
"""

from __future__ import annotations

import numpy as np
import pytest

from plumeIR.exceptions import PlumeIRError
from plumeIR.forward.ils import ILSKernel, make_ils

# NumPy compatibility: ``np.trapezoid`` was added in NumPy 2.0 and ``np.trapz``
# removed in later 2.x.  Mirrors the shim in ``plumeIR.units``.
_trapz = np.trapezoid if hasattr(np, "trapezoid") else np.trapz

#: Representative of the IMO HCl window: 2690-2830 cm⁻¹ at 0.04 cm⁻¹ spacing.
WN_CENTER = 2760.0
NPER_WN = 25.0
MAX_OPD = 1.8
FOV = 0.045

#: scipy.optimize.least_squares '2-point' relative step.
LSQ_REL_STEP = np.sqrt(np.finfo(float).eps)

APOD_TYPES = [
    "Boxcar", "Triangular", "NB weak", "NB medium", "NB strong", "Hann",
    "Hamming", "Happ-Genzel", "Blackman-Harris", "Nuttall", "Kaiser",
    "Tukey", "Cosine", "Lorenz", "Gaussian",
]


def build(**kwargs) -> ILSKernel:
    opts = dict(fov=FOV, max_opd=MAX_OPD, nper_wn=NPER_WN,
                wn_center=WN_CENTER, apod_type="NB medium")
    opts.update(kwargs)
    return make_ils(**opts)


# =============================================================================
# Basic properties
# =============================================================================

class TestKernelProperties:

    def test_normalised_to_unit_sum(self):
        assert build().kernel.sum() == pytest.approx(1.0)

    @pytest.mark.parametrize("apod", APOD_TYPES)
    def test_every_apodization_builds_and_normalises(self, apod):
        k = build(apod_type=apod)
        assert np.all(np.isfinite(k.kernel))
        assert k.kernel.sum() == pytest.approx(1.0)

    def test_grids_are_consistent(self):
        k = build()
        assert k.kernel.shape == k.wn_grid.shape
        assert k.opd_grid[0] == 0.0
        dx = np.diff(k.opd_grid)
        np.testing.assert_allclose(dx, dx[0])
        # n_tot·dx = nper_wn keeps the kernel's spacing at the model grid's.
        assert k.opd_grid.size * dx[0] == pytest.approx(NPER_WN)

    def test_apodization_is_case_insensitive(self):
        np.testing.assert_allclose(build(apod_type="nb medium").kernel,
                                   build(apod_type="NB MEDIUM").kernel)

    def test_zero_fov_gives_no_broadening(self):
        """sinc(0) = 1, so a zero FOV must leave the apodization untouched."""
        k = build(fov=0.0)
        assert k.fov_width == 0.0
        assert k.kernel.sum() == pytest.approx(1.0)

    def test_fov_width_formula(self):
        k = build(fov=0.045)
        assert k.fov_width == pytest.approx(WN_CENTER * (0.045 / 2) ** 2 / 2)


# =============================================================================
# Smoothness in fov — the reason this module exists
# =============================================================================

class TestFovOnlyBroadens:
    """
    Convention A (2026-11-16): the kernel models the FOV's broadening only;
    its displacement of line centres (ν·fov²/16) is left to the wavenumber
    registration and ``nu_shift``.  Before that the shift was in the kernel,
    first by interpolation, which smoothed it by an amount set by its
    fractional part so that fitted fov snapped to whole-sample shifts (0.0175
    rad and the 0.001 bound in the layer CO window), then exactly by a phase
    factor (2026-11-14 to 2026-11-16).
    """

    @pytest.mark.parametrize("fov", [0.0, 0.0124, 0.05, 0.15])
    def test_centroid_stays_at_zero(self, fov):
        """No displacement at any fov.  (Hann: NB and boxcar ring as 1/x, so
        their centroid depends on where the kernel is cut.)"""
        dnu = 1.0 / NPER_WN
        k = build(fov=fov, max_opd=1.037, wn_center=2085.0, apod_type="Hann",
                  trim_opd=1.037, trim_fov=0.2)
        assert abs((k.wn_grid * k.kernel).sum()) < 1e-6 * dnu

    @pytest.mark.parametrize("fov", [0.0124, 0.05])
    def test_kernel_symmetric_at_finite_fov(self, fov):
        k = build(fov=fov, max_opd=1.037, wn_center=2085.0, trim_fov=0.2)
        assert np.allclose(k.kernel, k.kernel[::-1], atol=1e-12 * k.kernel.max())

    def test_fitted_fov_does_not_snap_to_whole_sample_shifts(self):
        from scipy.optimize import least_squares
        from plumeIR.forward.base import apply_ils, apply_shift_offset
        nu = 2085.0
        wn = np.arange(1985.0, 2185.0, 1.0 / NPER_WN)
        wm = np.arange(2020.0, 2150.0, 0.24106)           # MIDAC grid spacing
        rng = np.random.default_rng(1)
        centres = rng.uniform(1990.0, 2180.0, 80)
        strength = 10 ** rng.uniform(-0.5, 2.0, 80)       # up to saturated
        od = (strength[:, None] * 0.08 ** 2
              / ((wn - centres[:, None]) ** 2 + 0.08 ** 2)).sum(axis=0)
        F = np.exp(-od)

        def model(p):
            k = build(fov=p[0], max_opd=1.037, wn_center=nu, apod_type="triangle",
                      trim_opd=1.037, trim_fov=0.2)
            return p[2] * apply_shift_offset(apply_ils(F, k), wn, wm,
                                             shift=p[1], offset=0.0)

        true_fov = 0.0124                  # a half-sample FOV shift at 2085 cm⁻¹
        y = model((true_fov, -0.3, 1.0))
        for start in (0.005, 0.021, 0.03):  # the old kernel: 0.001-0.0175
            r = least_squares(lambda p: model(p) - y, [start, -0.25, 1.0],
                              bounds=([0.001, -2.0, 0.5], [0.2, 2.0, 1.5]),
                              x_scale="jac", ftol=1e-12, xtol=1e-12)
            assert r.x[0] == pytest.approx(true_fov, rel=1e-3)


class TestFovSmoothness:

    def test_fov_sensitivity_at_optimizer_step(self):
        """
        The kernel must respond to a fov perturbation the size least_squares
        actually uses.  The old integer-boxcar implementation returned a
        BITWISE IDENTICAL kernel here — the width contributed exactly zero to
        the Jacobian, leaving fov degenerate with nu_shift.
        """
        step = LSQ_REL_STEP * FOV                      # ~6.7e-10
        base = build(fov=FOV).kernel
        stepped = build(fov=FOV + step).kernel

        assert not np.array_equal(base, stepped), (
            "kernel is invariant to a finite-difference fov step — the FOV "
            "effect is invisible to the optimizer"
        )
        # And the response must scale sensibly, not be numerical noise.
        assert np.abs(stepped - base).max() > 0

    def test_kernel_changes_continuously_across_a_former_step_boundary(self):
        """
        Old n_box = ceil(fov_width * nper_wn) jumped by a whole grid point at
        integer crossings.  Sample finely across such a boundary; the change
        per sub-step must stay bounded, with no jump an order of magnitude
        above its neighbours.
        """
        # fov where fov_width * nper_wn crosses an integer
        target = 17.0 / NPER_WN                       # fov_width giving n_box=17
        fov_edge = 2 * np.sqrt(2 * target / WN_CENTER)

        fovs = np.linspace(fov_edge * 0.995, fov_edge * 1.005, 41)
        # trim_fov fixes the length, as every forward model does while fitting.
        kernels = np.array([build(fov=f, trim_fov=fovs[-1]).kernel for f in fovs])
        deltas = np.abs(np.diff(kernels, axis=0)).max(axis=1)

        assert np.all(np.isfinite(deltas))
        assert deltas.max() < 10 * np.median(deltas), (
            "kernel jumps discontinuously across a former boxcar boundary"
        )

    def test_broadening_increases_monotonically_with_fov(self):
        """
        A wider FOV must broaden the ILS core — the physics fov is meant to
        carry.

        Width is measured as the equivalent width ``1 / peak`` (the kernel has
        unit area, so a broader core means a lower peak).  A second moment is
        NOT usable here: it is dominated by the sinc ringing lobes, which FOV
        self-apodization suppresses even as the core broadens, so it moves the
        wrong way.
        """
        def equivalent_width(fov):
            k = build(fov=fov)
            return 1.0 / k.kernel.max()

        widths = [equivalent_width(f) for f in (0.01, 0.02, 0.03, 0.045, 0.06)]
        assert all(b > a for a, b in zip(widths, widths[1:])), widths

    def test_no_sub_grid_floor(self):
        """
        Below one grid spacing the old boxcar collapsed to n_box=1 and the FOV
        effect vanished entirely.  The sinc form has no such floor.
        """
        tiny = 0.005                                   # fov_width << grid spacing
        assert not np.array_equal(build(fov=0.0).kernel, build(fov=tiny).kernel)


# =============================================================================
# Kernel support — a wide FOV box must not be cut or wrapped
# =============================================================================

def _on_common_grid(k: ILSKernel, ref: ILSKernel) -> np.ndarray:
    """Place a kernel on a reference kernel's (wider, same-spacing) grid."""
    out = np.zeros(ref.wn_grid.size)
    idx = np.rint(k.wn_grid * NPER_WN).astype(int) + ref.wn_grid.size // 2
    out[idx] = k.kernel
    return out


class TestWideFovSupport:
    """
    The kernel used to be trimmed to +/-5/max_opd *after* the FOV shift, which
    put the whole box on one side of zero: at fov 0.15 near 2800 cm⁻¹ that cut
    ~80% of the kernel, and a box wider than the FFT grid's +/-nper_wn/2 span
    wrapped round.  Measured against a near-untruncated reference (support
    100 cm⁻¹), L1 distance 79% (old) against 1.8% (new).
    """

    @pytest.mark.parametrize("fov", [0.09, 0.15, 0.2])
    def test_matches_untruncated_reference(self, fov):
        opd = 1.037
        ref = build(fov=fov, max_opd=opd, trim_opd=0.05)       # support 100 cm-1
        k = build(fov=fov, max_opd=opd)
        common = _on_common_grid(k, ref)
        assert np.abs(common - ref.kernel).sum() < 0.03
        centroid = lambda w: (w * ref.wn_grid).sum() / w.sum()
        assert abs(centroid(common) - centroid(ref.kernel)) < 0.05

    def test_support_holds_the_fov_box(self):
        k = build(fov=0.15, max_opd=1.037)
        assert -k.wn_grid[0] >= k.fov_width + 5.0 / 1.037 - 1.0 / NPER_WN

    def test_wide_box_keeps_model_grid_spacing(self):
        k = build(fov=0.2, max_opd=1.037)                     # needs k > 1
        assert k.opd_grid.size > NPER_WN ** 2
        assert np.allclose(np.diff(k.wn_grid), 1.0 / NPER_WN)

    def test_minimum_oversampling(self):
        """OPD sampling is converged whatever the support (NB ends in a step)."""
        from plumeIR.forward.ils import _MIN_OVERSAMPLING
        k = build(fov=0.03, max_opd=1.037)
        assert k.opd_grid.size == int(_MIN_OVERSAMPLING * NPER_WN ** 2)

    def test_fov_above_trim_fov_widens_support(self):
        narrow = build(fov=0.05, trim_opd=MAX_OPD, trim_fov=0.05).kernel.size
        wider = build(fov=0.15, trim_opd=MAX_OPD, trim_fov=0.05).kernel.size
        assert wider > narrow


def _line_spectrum() -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Synthetic transmittance: 60 Lorentzian lines on 2600-2900 cm⁻¹."""
    wn = np.arange(2600.0, 2900.0, 1.0 / NPER_WN)
    rng = np.random.default_rng(0)
    centres = rng.uniform(2610.0, 2890.0, 60)
    strength = rng.uniform(0.05, 0.6, 60)
    od = (strength[:, None] * 0.08 ** 2
          / ((wn - centres[:, None]) ** 2 + 0.08 ** 2)).sum(axis=0)
    return wn, np.exp(-od), (wn > 2690.0) & (wn < 2830.0)


class TestPedestalAndTail:
    """
    The real part of the FFT of a one-sided interferogram is (T + a0)/2: an
    exact pedestal of a0/2 per bin.  It used to be estimated from the grid's
    outer quarter, which still holds line-shape tail for a triangle window, and
    the trimmed kernel was renormalised, handing the cut tail mass (~0.4%) to
    the line core.  Synthetic HCl column bias for a triangle window: -0.83%
    before, <= 0.08% after.
    """

    @pytest.mark.parametrize("apod", ["triangle", "NB medium", "boxcar"])
    def test_wide_kernel_has_no_missing_mass(self, apod):
        """With the exact pedestal, a +/-250 cm-1 kernel holds the full mass."""
        k = build(fov=0.03, max_opd=1.037, apod_type=apod, trim_opd=5.0 / 250.0)
        assert abs(k.tail_mass) < 1e-3

    @pytest.mark.parametrize("apod", ["triangle", "boxcar", "NB weak",
                                      "NB medium", "Hann"])
    @pytest.mark.parametrize("trim_fov", [0.015, 0.2])
    def test_cut_mass_within_tolerance(self, apod, trim_fov):
        """The core grows until the cut mass is within _TAIL_MASS_TOL, whether
        the support is set by a held fov or by a fitted fov's bound."""
        from plumeIR.forward.ils import _TAIL_MASS_TOL
        k = build(fov=0.015, max_opd=1.037, wn_center=2085.0, apod_type=apod,
                  trim_opd=1.037, trim_fov=trim_fov)
        assert abs(k.tail_mass) <= _TAIL_MASS_TOL

    def test_core_follows_line_shape_not_fov(self):
        from plumeIR.forward.ils import core_support
        tri = core_support("triangle", 1.037, NPER_WN)
        assert 30.0 < tri < 45.0                       # 1/x² tails: ~38 cm-1
        assert core_support("Hann", 1.037, NPER_WN) == pytest.approx(5.0 / 1.037,
                                                                     abs=0.05)

    @pytest.mark.parametrize("apod", ["triangle", "Hann"])
    def test_spectrum_converged_in_support(self, apod):
        """
        A wider support no longer changes the convolved spectrum, for windows
        that reach zero at max_opd.  (NB and boxcar end in a step; their 1/x
        ringing does not converge in support, ~7e-3 of line depth between
        ±5 and ±19 cm⁻¹, which is why the forward models give a held fov the
        same support as a fitted one.)  Triangle at the old ±5-limb core:
        3.7e-3; measured now 1.1e-4.
        """
        from plumeIR.forward.base import apply_ils
        wn, F, inner = _line_spectrum()
        kw = dict(fov=0.015, max_opd=1.037, wn_center=2760.0, apod_type=apod,
                  trim_opd=1.037)
        held = apply_ils(F, build(trim_fov=0.015, **kw))[inner]
        fitted = apply_ils(F, build(trim_fov=0.2, **kw))[inner]
        depth = (1.0 - fitted).max()
        assert np.abs(held - fitted).max() / depth < 2e-4

    def test_tail_term_reproduces_wide_kernel_for_triangle(self):
        from plumeIR.forward.base import apply_ils
        wn, F, inner = _line_spectrum()
        kw = dict(fov=0.03, max_opd=1.037, wn_center=2760.0, apod_type="triangle")
        wide = build(trim_opd=5.0 / 250.0, **kw)
        short = build(trim_opd=1.037, trim_fov=0.2, **kw)
        ref = apply_ils(F, wide)[inner]
        depth = (1.0 - ref).max()
        err_tail = np.abs(apply_ils(F, short)[inner] - ref).max() / depth
        err_renorm = np.abs(apply_ils(F, short.kernel)[inner] - ref).max() / depth
        assert err_tail < 1e-3                 # measured 3.9e-4
        assert err_tail < err_renorm / 5.0     # measured 10x


class TestKernelTrimFov:

    def _param(self, vary, value=0.03, upper=0.2):
        from plumeIR.parameters.parameter import Parameter, ParameterKind
        return Parameter("fov", value, vary, (0.001, upper), ParameterKind.SPECTRAL)

    def test_varied_uses_upper_bound(self):
        from plumeIR.forward.ils import kernel_trim_fov
        assert kernel_trim_fov(self._param(True)) == 0.2

    def test_fixed_uses_upper_bound(self):
        """A held fov sizes the kernel like a fitted one (2026-11-14)."""
        from plumeIR.forward.ils import kernel_trim_fov
        assert kernel_trim_fov(self._param(False)) == 0.2

    def test_fixed_unbounded_uses_value(self):
        from plumeIR.forward.ils import kernel_trim_fov
        assert kernel_trim_fov(self._param(False, upper=np.inf)) == 0.03

    @pytest.mark.parametrize("apod", ["triangle", "boxcar", "NB medium"])
    def test_held_and_fitted_fov_build_the_same_kernel(self, apod):
        """
        Regression (2026-11-14): a held fov sized the support from its value, a
        fitted one from its 0.2 bound, so the same fov meant a ±4.9 or a
        ±15 cm⁻¹ triangle kernel and the layer SO2 column moved 1.1%.
        """
        from plumeIR.forward.ils import kernel_trim_fov
        kw = dict(fov=0.015, max_opd=1.037, wn_center=2085.0, apod_type=apod,
                  trim_opd=1.037)
        held = build(trim_fov=kernel_trim_fov(self._param(False, value=0.015)), **kw)
        fitted = build(trim_fov=kernel_trim_fov(self._param(True, value=0.015)), **kw)
        np.testing.assert_array_equal(held.kernel, fitted.kernel)
        assert held.tail_mass == fitted.tail_mass

    def test_unbounded_follows_live_fov(self):
        from plumeIR.forward.ils import kernel_trim_fov
        assert kernel_trim_fov(self._param(True, upper=np.inf)) is None


# =============================================================================
# Smoothness in max_opd — same defect class, previously latent
# =============================================================================

class TestMaxOpdSmoothness:

    def test_max_opd_sensitivity_at_optimizer_step(self):
        step = LSQ_REL_STEP * MAX_OPD
        base = build(max_opd=MAX_OPD).kernel
        stepped = build(max_opd=MAX_OPD + step).kernel
        assert not np.array_equal(base, stepped), (
            "kernel is invariant to a finite-difference max_opd step — "
            "fit_max_opd would be unfittable"
        )

    def test_kernel_length_is_fixed_by_trim_opd(self):
        """
        A kernel whose LENGTH changes with max_opd would make the convolution
        discontinuous.  With trim_opd pinned, only the shape may vary.
        """
        lengths = {build(max_opd=L, trim_opd=MAX_OPD).kernel.size
                   for L in (1.5, 1.8, 2.1)}
        assert len(lengths) == 1, f"kernel length varied with max_opd: {lengths}"

    def test_kernel_length_is_fixed_by_trim_fov(self):
        """The support covers the FOV box, so it must not move while fov does."""
        lengths = {build(fov=f, trim_opd=MAX_OPD, trim_fov=0.2).kernel.size
                   for f in np.linspace(0.001, 0.2, 50)}
        assert len(lengths) == 1, f"kernel length varied with fov: {lengths}"

    def test_resolution_improves_with_max_opd(self):
        """Longer OPD -> narrower ILS."""
        def width(L):
            k = build(max_opd=L, trim_opd=3.0)
            w = np.abs(k.kernel); w = w / w.sum()
            centre = (k.wn_grid * w).sum()
            return np.sqrt(((k.wn_grid - centre) ** 2 * w).sum())

        assert width(1.0) > width(1.8) > width(2.5)

    def test_truncation_weight_is_continuous(self):
        """Sweeping max_opd across a sample boundary must not jump."""
        dx = 1.0 / NPER_WN
        edge = 1.8 - (1.8 % dx) + dx                   # a sample boundary
        opds = np.linspace(edge - 0.3 * dx, edge + 0.3 * dx, 31)
        kernels = np.array([build(max_opd=L, trim_opd=3.0).kernel for L in opds])
        deltas = np.abs(np.diff(kernels, axis=0)).max(axis=1)
        assert deltas.max() < 10 * np.median(deltas)


# =============================================================================
# Grid centring
# =============================================================================

class TestCentring:
    """
    The kernel must sit on its own wavenumber grid.  A manual roll by
    ``n_tot // 2`` against a ``linspace`` grid (zero at ``(n_tot-1)/2``) left it
    off by half a sample for even ``n_tot`` and a full sample for odd — and
    ``n_tot = nper_wn**2`` is odd exactly when ``nper_wn`` is.  A free
    ``nu_shift`` hid it in the fits, but it cost four of fifty emission spectra
    at ``nper_wn = 25``.
    """

    @staticmethod
    def _peak_wn(k):
        y = k.kernel
        i = int(np.argmax(y))
        d = 0.5 * (y[i-1] - y[i+1]) / (y[i-1] - 2*y[i] + y[i+1])
        return k.wn_grid[i] + d * (k.wn_grid[1] - k.wn_grid[0])

    @pytest.mark.parametrize("nper_wn", [5, 10, 20, 25, 30, 40, 50])
    def test_peak_lands_on_zero_when_fov_is_zero(self, nper_wn):
        """
        With fov=0 there is no intentional shift, so the peak must be at wn=0.
        Parametrised across both parities of n_tot = nper_wn**2.
        """
        k = build(fov=0.0, nper_wn=nper_wn, trim_opd=MAX_OPD)
        offset_samples = self._peak_wn(k) * nper_wn
        assert abs(offset_samples) < 1e-6, (
            f"kernel off its grid by {offset_samples:+.4f} samples "
            f"(n_tot={nper_wn**2}, {'odd' if nper_wn % 2 else 'even'})"
        )

    @pytest.mark.parametrize("nper_wn", [10, 25, 40])
    def test_kernel_symmetric_about_zero(self, nper_wn):
        """With fov=0 the kernel is a real even function -> symmetric about 0."""
        k = build(fov=0.0, nper_wn=nper_wn, trim_opd=MAX_OPD)
        i = int(np.argmin(np.abs(k.wn_grid)))
        n = min(i, len(k.kernel) - 1 - i)
        left  = k.kernel[i-n:i][::-1]
        right = k.kernel[i+1:i+1+n]
        assert np.abs(left - right).max() / k.kernel.max() < 1e-9

    @pytest.mark.parametrize("nper_wn", [10, 25, 40])
    def test_wn_spacing_matches_model_grid(self, nper_wn):
        """Convolution requires the kernel to be on the model grid spacing."""
        k = build(nper_wn=nper_wn, trim_opd=MAX_OPD)
        np.testing.assert_allclose(np.diff(k.wn_grid), 1.0 / nper_wn, rtol=1e-12)

    def test_shape_does_not_depend_on_nper_wn_parity(self):
        """
        Adjacent odd/even grids must give the same kernel.  This is the
        non-monotonicity that exposed the bug: nper_wn=25 was far worse than
        either 24 or 26 because 625 is odd.
        """
        common = np.linspace(-2.0, 2.0, 2001)
        def prof(nper):
            k = build(fov=0.0, nper_wn=nper, trim_opd=MAX_OPD)
            y = np.interp(common, k.wn_grid, k.kernel, left=0.0, right=0.0)
            return y / _trapz(y, common)
        a, b, c = prof(24), prof(25), prof(26)
        peak = max(a.max(), b.max(), c.max())
        assert np.abs(b - a).max() / peak < 0.02
        assert np.abs(b - c).max() / peak < 0.02


# =============================================================================
# Agreement with the previous implementation
# =============================================================================

class TestBackwardAgreement:

    def test_matches_rfmwrapper_when_fov_is_zero(self):
        """
        With no FOV the sinc term is unity, so the only remaining differences
        from rfmwrapper.makeILS are the OPD-grid changes.  The kernels should
        still agree closely in shape.
        """
        rfm = pytest.importorskip("rfmwrapper")
        old = rfm.makeILS(fov=0.0, max_opd=MAX_OPD, nper_wn=NPER_WN,
                          wn_center=WN_CENTER, apod_type="NB medium")
        new = build(fov=0.0)

        # Compare on the overlapping wavenumber support.
        lo = max(new.wn_grid.min(), old["wn_grid"].min())
        hi = min(new.wn_grid.max(), old["wn_grid"].max())
        grid = np.linspace(lo, hi, 201)
        a = np.interp(grid, old["wn_grid"], old["kernel"] / old["kernel"].sum())
        b = np.interp(grid, new.wn_grid, new.kernel)

        # Normalised peak-relative agreement.
        assert np.abs(a - b).max() / b.max() < 0.05


# =============================================================================
# Validation
# =============================================================================

class TestValidation:

    @pytest.mark.parametrize("kwargs, match", [
        ({"max_opd": 0.0},    "max_opd must be positive"),
        ({"max_opd": -1.0},   "max_opd must be positive"),
        ({"nper_wn": 0.0},    "nper_wn must be positive"),
        ({"wn_center": 0.0},  "wn_center must be positive"),
        ({"fov": -0.01},      "fov must be non-negative"),
    ])
    def test_invalid_inputs_raise(self, kwargs, match):
        with pytest.raises(PlumeIRError, match=match):
            build(**kwargs)

    def test_unknown_apodization_raises(self):
        with pytest.raises(PlumeIRError, match="Unknown apod_type"):
            build(apod_type="Bartlett")
