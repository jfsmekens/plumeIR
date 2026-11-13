"""
Slow instrument drift shared by every preprocessing reference.

Three preprocessing inputs are the same kind of object: a per-wavenumber
quantity measured a handful of times over a run, which must be evaluated at
each scene spectrum's acquisition time.  They are the layer geometry's
self-emission background, and the emission geometry's blackbody calibration
(gain and offset) and clear-sky reference.

:class:`DriftingSpectrum` is that object.  With one sample it is static; with
several it is a low-order polynomial in time per wavenumber, so slow drift over
an acquisition is tracked rather than averaged away.

Ref: plumeIR v2 ``analysis.py``, which implemented this three times over —
``makeSelfEmission`` (lines 1484-1528), ``makeCalibration`` (lines 1320-1414)
and ``makeClear`` (lines 1420-1478).  Two deliberate departures from v2:

1. Timestamps are centred on the first sample before ``polyfit``.  v2 fit
   against raw Unix timestamps (~1.7e9), which is badly conditioned for a
   quadratic; centring changes only the conditioning, not the fitted curve.
2. When drift fitting is disabled but several samples exist, the samples are
   **averaged**.  v2 took the chronologically last one (``bbc[-1]``,
   ``clear_list[-1]``).  Averaging uses all the information and is not
   sensitive to which file happened to be acquired last; it differs from v2
   only for datasets carrying several references with drift turned off.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import numpy as np

from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Highest polynomial degree used for the drift fit (v2: ``min(n - 1, 2)``).
MAX_DRIFT_DEGREE = 2


@dataclass(frozen=True)
class DriftingSpectrum:
    """
    A spectral quantity sampled at a few times, evaluated at any time.

    Parameters
    ----------
    x : np.ndarray
        Spectral axis shared by every sample, shape ``(n_wn,)``.
    values : np.ndarray
        The samples, shape ``(n_ref, n_wn)``, in time order.
    times : np.ndarray
        Sample times as seconds since ``t0``, shape ``(n_ref,)``.  All zeros
        when the samples carry no timestamps.
    t0 : float
        Reference epoch (Unix seconds) the times are centred on.
    coeffs : np.ndarray or None
        Polynomial coefficients over centred time, shape ``(deg + 1, n_wn)``
        in ``np.polyval`` convention.  ``None`` when the quantity is static.
    label : str
        Human-readable name, used in log messages ("clear-sky reference").
    sources : tuple of Path
        Files the samples came from, in time order.  Empty when the quantity
        is derived rather than read (blackbody gain, for instance).
    """
    x:       np.ndarray
    values:  np.ndarray
    times:   np.ndarray
    t0:      float
    coeffs:  np.ndarray | None
    label:   str
    sources: tuple[Path, ...] = ()

    @property
    def n_ref(self) -> int:
        """Number of samples the quantity was built from."""
        return len(self.values)

    @property
    def is_static(self) -> bool:
        """True if the quantity carries no time dependence."""
        return self.coeffs is None

    def at(
        self,
        timestamp: datetime | None = None,
        x: np.ndarray | None = None,
    ) -> np.ndarray:
        """
        Evaluate the quantity for one scene spectrum.

        Parameters
        ----------
        timestamp : datetime or None
            Scene acquisition time.  ``None`` (or a static quantity) returns
            the mean of the samples.
        x : np.ndarray or None
            Spectral axis to interpolate onto, shape ``(n_out,)``.  ``None``
            returns the quantity on its own axis.

        Returns
        -------
        np.ndarray
            Values, shape ``(n_out,)`` or ``(n_wn,)``.
        """
        if self.is_static or timestamp is None:
            if timestamp is None and not self.is_static:
                logger.warning(
                    "Spectrum has no timestamp; using the mean of %d %s "
                    "samples instead of the drift fit", self.n_ref, self.label,
                )
            y = self.values.mean(axis=0)
        else:
            t = timestamp.timestamp() - self.t0
            if not (self.times.min() <= t <= self.times.max()):
                logger.warning(
                    "Spectrum at %s is outside the %s time span (%+.0f s "
                    "beyond); the drift fit is extrapolating",
                    timestamp.isoformat(), self.label,
                    t - np.clip(t, self.times.min(), self.times.max()),
                )
            y = np.polyval(self.coeffs, t)

        if x is None:
            return y
        return np.interp(x, self.x, y)

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------

    @classmethod
    def from_samples(
        cls,
        x:          np.ndarray,
        values:     np.ndarray,
        timestamps: list[datetime | None],
        label:      str,
        sources:    tuple[Path, ...] = (),
        fit_drift:  bool = True,
        max_degree: int = MAX_DRIFT_DEGREE,
    ) -> DriftingSpectrum:
        """
        Build from samples already on a common spectral axis.

        Samples are sorted into time order.  A drift fit of degree
        ``min(n - 1, max_degree)`` is made when there is more than one sample,
        every sample is timestamped, and ``fit_drift`` is set; otherwise the
        result is static.

        Parameters
        ----------
        x : np.ndarray
            Common spectral axis, shape ``(n_wn,)``.
        values : np.ndarray
            Samples, shape ``(n_ref, n_wn)``.
        timestamps : list of datetime or None
            Acquisition time per sample, length ``n_ref``.
        label : str
            Human-readable name for log messages.
        sources : tuple of Path
            Source files, length ``n_ref`` or empty.
        fit_drift : bool
            Whether to fit drift at all (the ``bb_drift`` / ``clear_drift``
            config flags).  ``False`` forces a static result.
        max_degree : int
            Ceiling on the polynomial degree.

        Returns
        -------
        DriftingSpectrum

        Raises
        ------
        PlumeIRError
            If there are no samples, or the lengths are inconsistent.
        """
        values = np.atleast_2d(np.asarray(values, dtype=float))
        x      = np.asarray(x, dtype=float)

        if values.shape[0] == 0:
            raise PlumeIRError(f"No samples to build the {label} from")
        if len(timestamps) != values.shape[0]:
            raise PlumeIRError(
                f"{label}: {values.shape[0]} samples but "
                f"{len(timestamps)} timestamps"
            )
        if sources and len(sources) != values.shape[0]:
            raise PlumeIRError(
                f"{label}: {values.shape[0]} samples but "
                f"{len(sources)} source files"
            )

        have_times = all(t is not None for t in timestamps)

        if have_times:
            order      = np.argsort([t.timestamp() for t in timestamps])
            values     = values[order]
            timestamps = [timestamps[i] for i in order]
            sources    = tuple(sources[i] for i in order) if sources else ()
            t0    = timestamps[0].timestamp()
            times = np.array([t.timestamp() - t0 for t in timestamps],
                             dtype=float)
        else:
            t0    = 0.0
            times = np.zeros(values.shape[0], dtype=float)

        n      = values.shape[0]
        coeffs = None
        if n > 1 and have_times and fit_drift:
            deg    = min(n - 1, max_degree)
            coeffs = np.polyfit(times, values, deg)
            logger.info(
                "%s: %d samples over %.0f s, degree-%d drift fit",
                label.capitalize(), n, times[-1], deg,
            )
        elif n > 1 and not fit_drift:
            logger.info(
                "%s: %d samples, drift fitting disabled; averaging",
                label.capitalize(), n,
            )
        elif n > 1:
            logger.warning(
                "%s: %d samples but no timestamps; averaging instead of "
                "fitting drift", label.capitalize(), n,
            )
        else:
            logger.info("%s: static, from a single sample", label.capitalize())

        return cls(x=x, values=values, times=times, t0=t0, coeffs=coeffs,
                   label=label, sources=tuple(sources))

    @classmethod
    def from_spectra(
        cls,
        spectra:    list,
        label:      str,
        fit_drift:  bool = True,
        max_degree: int = MAX_DRIFT_DEGREE,
    ) -> DriftingSpectrum:
        """
        Build from :class:`~plumeIR.spectra.spectrum.Spectrum` objects.

        Parameters
        ----------
        spectra : list of Spectrum
            Samples, in any order.  All must share a spectral axis.
        label : str
            Human-readable name for log messages.
        fit_drift : bool
            Whether to fit drift, see :meth:`from_samples`.
        max_degree : int
            Ceiling on the polynomial degree.

        Returns
        -------
        DriftingSpectrum

        Raises
        ------
        PlumeIRError
            If ``spectra`` is empty or the spectra do not share an axis.
        """
        if not spectra:
            raise PlumeIRError(f"No spectra to build the {label} from")

        check_common_axis(spectra, label)

        return cls.from_samples(
            x=spectra[0].x,
            values=np.vstack([s.y for s in spectra]),
            timestamps=[s.timestamp for s in spectra],
            label=label,
            sources=tuple(s.filename for s in spectra),
            fit_drift=fit_drift,
            max_degree=max_degree,
        )


def check_common_axis(spectra: list, label: str) -> np.ndarray:
    """
    Verify that a group of spectra share one spectral axis.

    Parameters
    ----------
    spectra : list of Spectrum
        Spectra to check.  Must be non-empty.
    label : str
        Name used in the error message.

    Returns
    -------
    np.ndarray
        The common axis, shape ``(n_wn,)``.

    Raises
    ------
    PlumeIRError
        If the axes differ in length or value.
    """
    x = spectra[0].x
    for spec in spectra[1:]:
        if spec.x.shape != x.shape or not np.allclose(spec.x, x):
            raise PlumeIRError(
                f"{label}: spectra do not share a spectral axis — "
                f"'{spectra[0].filename.name}' has {len(x)} points, "
                f"'{spec.filename.name}' has {len(spec.x)}"
            )
    return x
