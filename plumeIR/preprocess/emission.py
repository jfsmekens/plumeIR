"""
Preprocessing for the emission geometry.

Turns a raw single beam into calibrated spectral radiance, which is what
:class:`~plumeIR.forward.emission.EmissionForwardModel` produces.  The step is
**two-point blackbody radiometric calibration**: two reference acquisitions of
targets at known temperatures fix the instrument's linear response, and every
scene spectrum is mapped through it.

Writing the detector response as ``S = irf · (L − L_inst)``:

    irf    = (S_hot − S_cold) / (L_hot − L_cold)      [counts per radiance]
    L_inst = L_cold − S_cold / irf                    [radiance]
    L      = S / irf + L_inst

``irf`` is the **instrument response function** and ``L_inst`` the
**instrument radiance** — v2's ``irf`` and ``ri``, kept under the same names
(``ri`` spelled out, and deliberately not "gain"/"offset": ``offset`` already
means the additive spectral offset fit parameter).

``L_inst`` is the scene radiance at which the instrument records **zero
counts**, and it is *positive* — measured here at 7-40 mW/(m²·sr·cm⁻¹),
equivalent to a brightness temperature of ~207-226 K.  An AC-coupled
interferometer modulates only the *difference* between the scene and its own
emission, so ``L_inst`` is precisely that instrument contribution: a scene
colder than it produces negative counts (the cold/hot count ratio measured
here is 0.764 against a Planck ratio of 0.779 at 900-1100 cm⁻¹, confirming the
instrument term subtracts).  That is why the emission geometry has no
self-emission subtraction step and
ignores ``subtract_self`` — in a transmission measurement a "self" file is a
low-intensity acquisition capturing mostly instrument radiance, but in emission
*every* spectrum is a low-intensity sky spectrum and the instrument radiance is
precisely what the calibration's ``L_inst`` measures.  Subtracting a self
background as well would remove that term twice.

Ref: plumeIR v2 ``analysis.py::makeCalibration`` (lines 1320-1414) and
``::preProcess`` (lines 1706-1712).  Three deliberate departures from v2:

1. **Hot and cold are identified by target temperature, not filename.**  v2
   matched the substrings ``'bbc'``/``'bbh'`` and zipped the two globs by list
   position.  The target temperature is read from the file (Bruker
   ``Instrument.TSM``) and is needed for the calibration regardless, so it also
   settles which target is which; pairing is by acquisition time.
2. **The algebra is the well-conditioned form.**  v2 computed
   ``ri = (S_H·L_C − S_C·L_H)/(S_H − S_C)`` and ``irf = S_H/(L_H − ri)``,
   dividing by the small difference twice and again by ``L_H − ri``.  The form
   above is algebraically identical with a single guarded division — the terms
   are the *same two quantities*, verified against v2 at 1e-15 (``irf``) and
   9e-15 (``L_inst``).  Results agree to rounding, not bit-for-bit.
3. **Singular points are masked, not patched.**  Where the two targets give the
   same counts the calibration carries no information.  v2 replaced those
   denominators with the smallest non-zero value in the array, silently
   inventing a response; here they become NaN and are logged.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path

import numpy as np

from plumeIR.exceptions import PlumeIRError
from plumeIR.spectra.discovery import find_reference_spectra
from plumeIR.spectra.spectrum import Spectrum, SpectrumType, read_spectrum
from plumeIR.physics import bbt, rad
from plumeIR.preprocess.drift import DriftingSpectrum, check_common_axis

logger = logging.getLogger(__name__)

#: Filename markers identifying the blackbody calibration targets.
BLACKBODY_MARKERS: list[str] = ["bbc", "bbh"]

#: Filename markers identifying the clear-sky reference.
CLEAR_MARKERS: list[str] = ["clear", "sky"]

#: A calibration point is singular when ``|S_hot − S_cold|`` falls below this
#: fraction of the band's median.  Small enough to catch only genuinely
#: uninformative points (dead detector edges), not merely weak ones.
SINGULARITY_RTOL: float = 1e-6

#: Minimum separation between the two target temperatures [K].
MIN_TARGET_SEPARATION: float = 1.0


# =============================================================================
# Blackbody calibration
# =============================================================================

@dataclass(frozen=True)
class BlackbodyCalibration:
    """
    Two-point radiometric calibration, optionally time-resolved.

    Parameters
    ----------
    irf : DriftingSpectrum
        Instrument response function ``(S_hot − S_cold)/(L_hot − L_cold)``, in
        counts per ``mW/(m²·sr·cm⁻¹)``.  v2's ``irf``.
    L_inst : DriftingSpectrum
        Instrument radiance ``L_cold − S_cold/irf`` [``mW/(m²·sr·cm⁻¹)``] —
        the scene radiance giving zero counts.  v2's ``ri``.  Positive in
        practice, and it *is* the instrument's own emission: the detector sees
        only the difference between scene and instrument, so a colder scene
        reads negative counts.
    valid : np.ndarray
        Boolean mask, shape ``(n_wn,)``, False where the two targets gave
        indistinguishable counts and the calibration carries no information.
    temps : tuple of float
        ``(T_cold, T_hot)`` target temperatures [K].
    x_unit : str
        Spectral axis unit of the calibration (``'wn'`` or ``'wl'``).
    """
    irf:    DriftingSpectrum
    L_inst: DriftingSpectrum
    valid:  np.ndarray
    temps:  tuple[float, float]
    x_unit: str

    @property
    def n_sets(self) -> int:
        """Number of hot/cold pairs the calibration was built from."""
        return self.irf.n_ref

    @property
    def is_static(self) -> bool:
        """True if the calibration carries no time dependence."""
        return self.irf.is_static

    def radiance(self, spectrum: Spectrum) -> np.ndarray:
        """
        Convert one single-beam spectrum to spectral radiance.

        Parameters
        ----------
        spectrum : Spectrum
            Raw single beam, on any axis within the calibration's range.

        Returns
        -------
        np.ndarray
            Spectral radiance [``mW/(m²·sr·cm⁻¹)``], shape ``(n_wn,)``.  NaN
            at points where the calibration is singular.

        Raises
        ------
        PlumeIRError
            If the spectrum's axis unit differs from the calibration's.
        """
        if spectrum.x_unit != self.x_unit:
            raise PlumeIRError(
                f"Cannot calibrate '{spectrum.filename.name}': spectrum axis "
                f"is in '{spectrum.x_unit}' but the blackbody calibration was "
                f"built in '{self.x_unit}'"
            )

        irf    = self.irf.at(spectrum.timestamp,    x=spectrum.x)
        L_inst = self.L_inst.at(spectrum.timestamp, x=spectrum.x)
        valid  = np.interp(spectrum.x, self.irf.x,
                           self.valid.astype(float)) > 0.999

        with np.errstate(divide="ignore", invalid="ignore"):
            L = spectrum.y / irf + L_inst
        return np.where(valid, L, np.nan)


def pair_blackbodies(spectra: list[Spectrum]) -> list[tuple[Spectrum, Spectrum]]:
    """
    Sort blackbody acquisitions into (cold, hot) pairs.

    Targets are separated by temperature — a calibration campaign uses exactly
    two — and then matched in acquisition order.

    Parameters
    ----------
    spectra : list of Spectrum
        Blackbody acquisitions, in any order.  Every one must carry a
        ``bb_temp``.

    Returns
    -------
    list of (Spectrum, Spectrum)
        ``(cold, hot)`` pairs in time order.

    Raises
    ------
    PlumeIRError
        If target temperatures are missing, there are not exactly two of them,
        they are too close together, or the two groups differ in size.
    """
    missing = [s.filename.name for s in spectra if s.bb_temp is None]
    if missing:
        raise PlumeIRError(
            f"Blackbody files carry no target temperature: "
            f"{', '.join(sorted(missing))}. The two-point calibration needs "
            f"it (Bruker: Instrument.TSM); check these are calibration "
            f"acquisitions and not scene spectra named like them."
        )

    temps = np.unique(np.round([s.bb_temp for s in spectra], 3))
    if temps.size != 2:
        raise PlumeIRError(
            f"Expected exactly 2 blackbody target temperatures, found "
            f"{temps.size}: {np.round(temps, 2).tolist()} K. A two-point "
            f"calibration needs one cold and one hot target."
        )
    if temps[1] - temps[0] < MIN_TARGET_SEPARATION:
        raise PlumeIRError(
            f"Blackbody targets are only {temps[1] - temps[0]:.2f} K apart "
            f"({temps[0]:.2f} / {temps[1]:.2f} K); the calibration would be "
            f"singular. They must differ by at least "
            f"{MIN_TARGET_SEPARATION:.1f} K."
        )

    def in_time_order(group: list[Spectrum]) -> list[Spectrum]:
        # Undated spectra sort last; with a single pair the order is moot.
        return sorted(group, key=lambda s: (s.timestamp is None,
                                            s.timestamp or 0))

    cold = in_time_order([s for s in spectra if s.bb_temp == temps[0]])
    hot  = in_time_order([s for s in spectra if s.bb_temp == temps[1]])

    if len(cold) != len(hot):
        raise PlumeIRError(
            f"Unbalanced blackbody sets: {len(cold)} cold ({temps[0]:.1f} K) "
            f"but {len(hot)} hot ({temps[1]:.1f} K). Each cold acquisition "
            f"needs a matching hot one."
        )

    return list(zip(cold, hot))


def build_blackbody_calibration(
    paths:     list[Path],
    fit_drift: bool = True,
) -> BlackbodyCalibration:
    """
    Read the blackbody files and derive the radiometric calibration.

    One pair gives a static calibration.  Several give a per-wavenumber drift
    fit of ``irf`` and ``L_inst`` in time (v2's ``bb_drift``).

    Parameters
    ----------
    paths : list of Path
        Blackbody spectrum files (both targets, all sets).
    fit_drift : bool
        Fit the calibration's drift over time when several sets exist.

    Returns
    -------
    BlackbodyCalibration

    Raises
    ------
    PlumeIRError
        If ``paths`` is empty, the files cannot be paired, or they do not
        share a spectral axis.
    """
    if not paths:
        raise PlumeIRError("No blackbody spectra to build a calibration from")

    spectra = [read_spectrum(p) for p in paths]
    x = check_common_axis(spectra, "blackbody calibration")

    units = {s.x_unit for s in spectra}
    if len(units) != 1:
        raise PlumeIRError(
            f"Blackbody spectra mix axis units: {sorted(units)}"
        )
    x_unit = units.pop()

    pairs = pair_blackbodies(spectra)

    # Planck curves for the two targets, evaluated per index so the arrays
    # line up with `x` whichever unit it carries.
    wn = spectra[0].wn
    T_cold, T_hot = pairs[0][0].bb_temp, pairs[0][1].bb_temp
    L_cold, L_hot = rad(wn, T_cold), rad(wn, T_hot)
    dL = L_hot - L_cold

    irfs, radiances, stamps = [], [], []
    valid = np.ones(x.size, dtype=bool)

    for cold, hot in pairs:
        den   = hot.y - cold.y
        scale = np.median(np.abs(den))
        ok    = np.abs(den) > SINGULARITY_RTOL * scale
        valid &= ok

        # Keep the arithmetic finite at singular points; they are masked out
        # on output rather than carried into the drift fit as NaN.
        den_safe = np.where(ok, den, 1.0)
        irf      = den_safe / dL
        L_inst   = L_cold - cold.y / irf

        irfs.append(irf)
        radiances.append(L_inst)
        stamps.append(_pair_timestamp(cold, hot))

    n_bad = int((~valid).sum())
    if n_bad:
        bad_x = x[~valid]
        logger.warning(
            "Blackbody calibration is singular at %d of %d points "
            "(%.2f-%.2f %s): the two targets gave indistinguishable counts "
            "there. Those points are set to NaN.",
            n_bad, x.size, bad_x.min(), bad_x.max(), x_unit,
        )

    logger.info(
        "Blackbody calibration: %d set(s), targets %.2f / %.2f K",
        len(pairs), T_cold, T_hot,
    )

    return BlackbodyCalibration(
        irf=DriftingSpectrum.from_samples(
            x, np.vstack(irfs), stamps, "instrument response function",
            fit_drift=fit_drift,
        ),
        L_inst=DriftingSpectrum.from_samples(
            x, np.vstack(radiances), stamps, "instrument radiance",
            fit_drift=fit_drift,
        ),
        valid=valid,
        temps=(float(T_cold), float(T_hot)),
        x_unit=x_unit,
    )


def _pair_timestamp(cold: Spectrum, hot: Spectrum):
    """Representative time for a hot/cold pair: the midpoint of the two."""
    if cold.timestamp is None or hot.timestamp is None:
        return cold.timestamp or hot.timestamp
    from datetime import datetime
    return datetime.fromtimestamp(
        0.5 * (cold.timestamp.timestamp() + hot.timestamp.timestamp())
    )


# =============================================================================
# Per-spectrum preprocessing
# =============================================================================

def calibrate_spectrum(
    spectrum:    Spectrum,
    calibration: BlackbodyCalibration,
    use_bbt:     bool = False,
) -> Spectrum:
    """
    Calibrate one single beam to radiance, optionally as brightness temperature.

    Pure: returns a new :class:`Spectrum`, leaving the input untouched.  No
    negative-value clipping is applied — unlike a single beam, radiance is a
    signed physical quantity and a near-zero value in a cold band is real data,
    not an artefact to be patched.

    Parameters
    ----------
    spectrum : Spectrum
        Raw single-beam spectrum.
    calibration : BlackbodyCalibration
        Calibration, evaluated at this spectrum's timestamp.
    use_bbt : bool
        Convert the result to brightness temperature [K].

    Returns
    -------
    Spectrum
        New spectrum, ``spec_type`` ``RAD`` (or ``BBT``).
    """
    y = calibration.radiance(spectrum)
    spec_type = SpectrumType.RAD

    if use_bbt:
        with np.errstate(divide="ignore", invalid="ignore"):
            y = np.where(y > 0, bbt(spectrum.wn, y), np.nan)
        spec_type = SpectrumType.BBT

    return Spectrum(
        x=spectrum.x.copy(),
        y=y,
        x_unit=spectrum.x_unit,
        timestamp=spectrum.timestamp,
        filename=spectrum.filename,
        spec_type=spec_type,
        metadata=spectrum.metadata,
        is_blackbody=spectrum.is_blackbody,
        bb_temp=spectrum.bb_temp,
        ils=spectrum.ils,          # instrument metadata survives preprocessing
    )


def preprocess_emission(
    spectra:     list[Spectrum],
    geometry,
    data_config,
) -> list[Spectrum]:
    """
    Apply emission-geometry preprocessing to a run's spectra.

    Radiometric calibration against the blackbody references, and the optional
    brightness-temperature transform.  ``subtract_self`` is ignored: the
    calibration's ``L_inst`` term already *is* the instrument radiance (see
    the module docstring).

    Parameters
    ----------
    spectra : list of Spectrum
        Scene spectra, in acquisition order.
    geometry : EmissionGeometry
        Supplies ``bb_drift``, ``use_bbt`` and ``fit_difference``.
    data_config : DataConfig
        Supplies ``data_dir`` and ``filter_ext`` for discovery.

    Returns
    -------
    list[Spectrum]
        Calibrated spectra, same length and order as the input.

    Raises
    ------
    PlumeIRError
        If no blackbody references exist, or ``fit_difference`` is enabled
        (not yet wired through preprocessing).
    """
    if geometry.fit_difference:
        raise PlumeIRError(
            "fit_difference is not yet supported in emission preprocessing. "
            "The forward model would fit 'L_model - L_clear' while the "
            "measurement is absolute radiance, which is silently wrong rather "
            "than merely unimplemented. Set GEOMETRY.fit_difference: False to "
            "fit calibrated radiance directly. An experimental implementation "
            "is parked on branch 'park/difference-mode'."
        )

    if getattr(geometry, "subtract_self", False):
        logger.debug(
            "subtract_self is set but does not apply to the emission "
            "geometry; the instrument radiance (L_inst) is carried by the "
            "blackbody calibration"
        )

    paths = find_reference_spectra(
        data_config.data_dir, BLACKBODY_MARKERS,
        filter_ext=data_config.filter_ext,
    )
    if not paths:
        raise PlumeIRError(
            f"No blackbody calibration spectra found in "
            f"'{data_config.data_dir}'. The emission geometry needs a cold and "
            f"a hot target acquisition, by convention named with 'BBC' and "
            f"'BBH' in the filename, to convert single beams to radiance."
        )

    calibration = build_blackbody_calibration(
        paths, fit_drift=geometry.bb_drift,
    )
    out = [calibrate_spectrum(s, calibration, use_bbt=geometry.use_bbt)
           for s in spectra]

    logger.info(
        "Calibrated %d spectra to %s", len(out),
        "brightness temperature" if geometry.use_bbt else "radiance",
    )
    return out
