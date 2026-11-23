
"""
Spectrum dataclass and file readers for plumeIR v3.

Supports:
  - Bruker OPUS files (any numeric extension: .0, .0000, .0001, …)
  - MIDAC SPC files (.spc, .sbm, .ifg, .sb, .abs)

All readers return a :class:`Spectrum` with the spectral axis in
ascending order.  The caller never needs to know which instrument
produced the file.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto
from pathlib import Path

import numpy as np
from scipy.interpolate import interp1d

from plumeIR.exceptions import SpectrumIOError
from plumeIR.spectra.spc import SPCFile, read_spc

logger = logging.getLogger(__name__)


# =============================================================================
# SpectrumType
# =============================================================================

class SpectrumType(Enum):
    """Classification of the spectral data type."""
    SBM  = auto()   # single-beam (raw intensity, arbitrary units)
    IFG  = auto()   # interferogram
    BBT  = auto()   # brightness temperature [K]
    RAD  = auto()   # calibrated radiance [mW/(m²·sr·cm⁻¹)]
    TRA  = auto()   # transmittance (dimensionless)
    ABS  = auto()   # absorbance
    DIFF = auto()   # radiance difference (emission geometry)


# Mapping from MIDAC ylabel strings to SpectrumType
_MIDAC_YLABEL_MAP: dict[str, SpectrumType] = {
    "Arbitrary Intensity": SpectrumType.SBM,
    "Interferogram":       SpectrumType.IFG,
    "Transmittance":       SpectrumType.TRA,
    "Transmission":        SpectrumType.TRA,   # SPC fytype 128
    "Absorbance":          SpectrumType.ABS,
    "Emission":            SpectrumType.RAD,
}

# Mapping from filename suffix to SpectrumType (MIDAC fallback)
_MIDAC_SUFFIX_MAP: dict[str, SpectrumType] = {
    "sbm": SpectrumType.SBM,
    "ifg": SpectrumType.IFG,
    "abs": SpectrumType.ABS,
    "tra": SpectrumType.TRA,
    "sb":  SpectrumType.SBM,
}


# =============================================================================
# Spectrum dataclass
# =============================================================================

@dataclass(frozen=True)
class InstrumentILS:
    """
    Instrument line shape parameters read from the data file itself.

    These describe the interferometer, so they belong to the measurement, not
    to a config file.  Hand-entering them is how both instruments in this
    project ended up with a wrong ILS:

    * ``max_opd`` was configured as ``1/RES`` — 2.0 cm for the Bruker
      (RES 0.5) and 1.8 cm for the MIDAC.  ``1/RES`` is the **total
      double-sided path** ``2L``; the maximum OPD is half of it.  Both
      instruments are 32768-point transforms at a ~15798 cm-1 HeNe, giving
      ``max_opd = 1.037 cm``.  The 2x error was absorbed by a fitted ``fov``,
      which ran to 85 mrad against a 30 mrad nominal (Bruker) and 52-56 mrad
      against 21 mrad (MIDAC) — and moved retrieved SO2 by 30-46%.
    * ``apod_type`` was configured as "NB medium" for the MIDAC, whose files
      say ``triangle``.

    * (2026-11-21) the halving above holds for a **double-sided**
      interferogram only.  The MIDAC files are single-sided (``IGRAMSIDE
      Single``, Mertz phase correction), so their max OPD is not half the
      transformed record but nearly all of it: 2.058 cm, not 1.037.

    Derivation.  The stored grid spacing gives the length ``S`` of the
    transformed interferogram record::

        S = 1 / (dnu_displayed * ZFF)       # = POINTS * interval / LWN

    ``dnu_displayed`` is the point spacing of the stored spectrum, finer than
    the true resolution by the zero-fill factor.  Then::

        double-sided (record spans -L..+L):        max_opd = S / 2
        single-sided (record starts n_pre points   max_opd = S * (POINTS - n_pre) / POINTS
                      before ZPD):

    MIDAC (Essential FTIR) records ``ZPD`` and ``SKIP POINTS`` in the SPC
    log, and ``ZPD - SKIP POINTS`` is 256 in every local file: the record
    keeps 256 points before ZPD for the Mertz correction, so ``n_pre = ZPD -
    SKIP POINTS`` and max_opd = (32768 - 256) / 15798.04 = 2.058 cm (developer,
    2026-11-21; CHANGELOG, "Maximum OPD of single-sided MIDAC files").  Bruker OPUS records the
    acquisition mode (``Acquisition.AQM``, ``D*`` double-sided, ``S*``
    single-sided); a single-sided OPUS file gives no ``max_opd``, since the
    pre-ZPD length is not read here.

    Parameters
    ----------
    max_opd : float or None
        Maximum optical path difference [cm].
    apod_type : str or None
        Apodization function name, in the vocabulary ``forward.ils`` accepts
        (substring-matched: 'triangle', 'NB medium', 'boxcar', ...).
    lwn : float or None
        Laser wavenumber [cm-1].
    zff : int or None
        Zero-fill factor.
    n_points : int or None
        Transform length, where the format reports it.
    single_sided : bool or None
        Whether the interferogram is single-sided (``None``: not recorded).
    n_pre_zpd : int or None
        Points of a single-sided record before ZPD.
    source : str
        Which file/keys this came from, for provenance in logs.
    """

    max_opd:   float | None = None
    apod_type: str | None   = None
    lwn:       float | None = None
    zff:       int | None   = None
    n_points:  int | None   = None
    single_sided: bool | None = None
    n_pre_zpd: int | None   = None
    source:    str          = ""

    @property
    def is_complete(self) -> bool:
        """True when both fitted-ILS-relevant fields were recovered."""
        return self.max_opd is not None and self.apod_type is not None


def _derive_max_opd(x: np.ndarray, zff: int | None,
                    n_points: int | None = None,
                    lwn: float | None = None,
                    *,
                    single_sided: bool = False,
                    n_pre_zpd: int | None = None,
                    interval: float = 1.0) -> float | None:
    """
    Maximum optical path difference [cm] from the stored spectrum grid.

    The grid spacing gives the transformed record's length ``S = 1 / (dnu *
    zff)``.  Double-sided: ``max_opd = S / 2``.  Single-sided, with
    ``n_pre_zpd`` of its ``n_points`` before ZPD: ``max_opd = S * (n_points -
    n_pre_zpd) / n_points`` (:class:`InstrumentILS`).  When ``n_points`` and
    ``lwn`` are both available they give an independent ``S = n_points *
    interval / lwn`` and a mismatch beyond 1% is logged: one of the header
    fields is then not what it is assumed to be.

    Parameters
    ----------
    x : np.ndarray
        Stored wavenumber grid [cm-1].
    zff : int or None
        Zero-fill factor.
    n_points : int or None
        Transformed record length [points].
    lwn : float or None
        Laser wavenumber [cm-1].
    single_sided : bool
        The interferogram is single-sided.
    n_pre_zpd : int or None
        Points of a single-sided record before ZPD.
    interval : float
        Sampling interval in laser fringes (1: one sample per fringe).

    Returns
    -------
    float or None
        ``None`` when the grid or a needed header field is missing (a
        single-sided record without ``n_points`` and ``n_pre_zpd``).
    """
    x = np.asarray(x, dtype=float)
    if x.size < 2 or not zff:
        return None
    dnu = float(np.median(np.diff(np.sort(x))))
    if not np.isfinite(dnu) or dnu <= 0:
        return None
    span = 1.0 / (dnu * float(zff))

    if n_points and lwn:
        span_alt = float(n_points) * float(interval) / float(lwn)
        if abs(span_alt - span) / span > 0.01:
            logger.warning(
                "Interferogram length estimates disagree: %.5f cm from grid "
                "spacing vs %.5f cm from POINTS x interval / LWN (%.1f%%) — check "
                "ZFF and the transform length",
                span, span_alt, abs(span_alt - span) / span * 100,
            )
    if not single_sided:
        return span / 2.0
    if not n_points or n_pre_zpd is None or not 0 <= n_pre_zpd < n_points:
        logger.warning("Single-sided interferogram without a usable point count "
                       "before ZPD (POINTS %s, before ZPD %s): max_opd not derived",
                       n_points, n_pre_zpd)
        return None
    return span * (n_points - n_pre_zpd) / n_points


#: OPUS ``Fourier Transformation.APF`` codes -> ``forward.ils`` vocabulary.
_OPUS_APF_MAP: dict[str, str] = {
    "BX":  "boxcar",
    "TR":  "triangle",
    "NBW": "NB weak",
    "NBM": "NB medium",
    "NBS": "NB strong",
    "HG":  "Happ-Genzel",
    "B3":  "Blackman-Harris",
    "B4":  "Blackman-Harris",
    "HAM": "Hamming",
}


@dataclass
class Spectrum:
    """
    Normalised spectral data container.

    The spectral axis ``x`` is always in ascending order.  Use the
    :attr:`wn` property to get wavenumbers [cm⁻¹] regardless of whether
    the file was stored in wavenumber or wavelength units.

    Parameters
    ----------
    x : np.ndarray
        Spectral axis (ascending).  Units given by ``x_unit``.
    y : np.ndarray
        Spectral values, same length as ``x``.
    x_unit : str
        ``'wn'`` for wavenumber [cm⁻¹] or ``'wl'`` for wavelength [µm].
    timestamp : datetime or None
        Acquisition time (local, naive).  ``None`` if not recoverable.
    filename : Path
        Source file path.
    spec_type : SpectrumType
        Spectral data type.
    metadata : dict
        Instrument metadata (Bruker section dicts; empty for MIDAC).
    is_blackbody : bool
        True if the file is a blackbody calibration measurement.
    bb_temp : float or None
        Blackbody target temperature [K].  None if not a blackbody file.
    """
    x:            np.ndarray
    y:            np.ndarray
    x_unit:       str
    timestamp:    datetime | None
    filename:     Path
    spec_type:    SpectrumType
    metadata:     dict        = field(default_factory=dict)
    is_blackbody: bool        = False
    bb_temp:      float | None = None
    ils:          InstrumentILS | None = None

    def __post_init__(self) -> None:
        # Ensure ascending spectral axis
        if len(self.x) > 1 and self.x[0] > self.x[-1]:
            self.x = self.x[::-1].copy()
            self.y = self.y[::-1].copy()

    # ------------------------------------------------------------------
    # Convenience properties
    # ------------------------------------------------------------------

    @property
    def wn(self) -> np.ndarray:
        """Wavenumber axis [cm⁻¹], converting from wavelength if needed."""
        if self.x_unit == "wn":
            return self.x
        return 1e4 / self.x  # µm → cm⁻¹ (note: reverses order)

    @property
    def npts(self) -> int:
        """Number of spectral points."""
        return len(self.x)

    @property
    def spacing(self) -> float:
        """Mean spectral spacing along the x axis."""
        return float(np.mean(np.diff(self.x)))

    # ------------------------------------------------------------------
    # Manipulation (each returns a new Spectrum)
    # ------------------------------------------------------------------

    def trim(self, x_start: float, x_stop: float) -> Spectrum:
        """
        Return a new Spectrum trimmed to ``[x_start, x_stop]``.

        Bounds are in the same units as ``x_unit``.
        """
        mask = (self.x >= x_start) & (self.x <= x_stop)
        return Spectrum(
            x=self.x[mask].copy(),
            y=self.y[mask].copy(),
            x_unit=self.x_unit,
            timestamp=self.timestamp,
            filename=self.filename,
            spec_type=self.spec_type,
            metadata=self.metadata,
            is_blackbody=self.is_blackbody,
            bb_temp=self.bb_temp,
            ils=self.ils,
        )

    def resample(self, x_grid: np.ndarray) -> Spectrum:
        """
        Return a new Spectrum resampled onto ``x_grid``.

        Uses linear interpolation.  ``x_grid`` must be in the same units
        as ``x_unit`` and within the range of the current axis.
        """
        f = interp1d(self.x, self.y, kind="linear",
                     bounds_error=False, fill_value=np.nan)
        return Spectrum(
            x=x_grid.copy(),
            y=f(x_grid),
            x_unit=self.x_unit,
            timestamp=self.timestamp,
            filename=self.filename,
            spec_type=self.spec_type,
            metadata=self.metadata,
            is_blackbody=self.is_blackbody,
            bb_temp=self.bb_temp,
            ils=self.ils,
        )

    # ------------------------------------------------------------------
    # Convenience constructors
    # ------------------------------------------------------------------

    @classmethod
    def from_file(cls, path: str | Path) -> Spectrum:
        """
        Read a spectrum file and return a :class:`Spectrum`.

        Shorthand for :func:`read_spectrum`.  Format is auto-detected
        from the file extension.

        Parameters
        ----------
        path : str or Path
            Path to a Bruker OPUS or MIDAC SPC file.

        Returns
        -------
        Spectrum
        """
        return read_spectrum(path)


# =============================================================================
# Timestamp extraction helpers
# =============================================================================

def _timestamp_from_name(path: Path) -> datetime | None:
    """
    Extract a timestamp from a filename by collecting all digit characters.

    Works for filenames like::

        plume2019_09_11_1829_53_253000.0   → 20190911182953253000
        2021_08_11_0036_01_544_sbm.spc     → 20210811003601544000

    The digit string is zero-padded to 20 characters and parsed as
    ``%Y%m%d%H%M%S%f``.

    Returns ``None`` if no valid timestamp can be extracted.
    """
    stem = path.stem
    digits = re.sub(r"\D", "", stem)
    if len(digits) < 8:
        return None
    digits = digits[:20].ljust(20, "0")
    try:
        return datetime.strptime(digits, "%Y%m%d%H%M%S%f")
    except ValueError:
        return None


def _timestamp_from_bruker(opus: dict) -> datetime | None:
    """
    Extract timestamp from a Bruker OPUS dict.

    Uses ``ScSm Data Parameter['DAT']`` (DD/MM/YYYY) and
    ``ScSm Data Parameter['TIM']`` (HH:MM:SS.mmm (GMT±n)).
    The timezone offset is ignored; local time is returned.
    """
    try:
        sdp  = opus["ScSm Data Parameter"]
        date = sdp["DAT"]                    # e.g. '11/09/2019'
        time = sdp["TIM"].split()[0] + "000" # strip timezone, pad µs
        return datetime.strptime(date + time, "%d/%m/%Y%H:%M:%S.%f")
    except (KeyError, ValueError, IndexError):
        return None


def _timestamp_from_spc(spc: "SPCFile") -> datetime | None:
    """The SPC header date (to the minute), or ``None`` when unset."""
    return spc.timestamp


# =============================================================================
# Bruker reader
# =============================================================================

def _bruker_ils(metadata: dict, x: np.ndarray, name: str) -> InstrumentILS:
    """
    ILS parameters of an OPUS file from its metadata sections.

    Parameters
    ----------
    metadata : dict
        The file's ``Fourier Transformation``, ``Instrument`` and
        ``Acquisition`` sections (as kept by :func:`read_bruker`).
    x : np.ndarray
        Stored wavenumber grid [cm-1].
    name : str
        File name, for the log and the provenance string.

    Returns
    -------
    InstrumentILS
        ``max_opd`` is ``None`` for a single-sided acquisition (``AQM S*``).
    """
    ft   = metadata.get("Fourier Transformation", {})
    inst = metadata.get("Instrument", {})
    zff  = ft.get("ZFF")
    apf  = str(ft.get("APF", "")).strip().upper()
    lwn  = inst.get("LWN")
    # Acquisition mode: D* double-sided, S* single-sided (OPUS AQM).  The
    # pre-ZPD length of a single-sided OPUS record is not read, so such a file
    # gives no max_opd (set it in the config).
    aqm  = str(metadata.get("Acquisition", {}).get("AQM", "")).strip().upper()
    single = aqm.startswith("S") if aqm else None
    if single:
        logger.warning("%s: single-sided Bruker interferogram (AQM %s); max_opd is "
                       "not derived from it — set max_opd in the config",
                       name, aqm)
    ils  = InstrumentILS(
        max_opd   = (None if single else
                     _derive_max_opd(x, int(zff) if zff else None,
                                     lwn=float(lwn) if lwn else None)),
        apod_type = _OPUS_APF_MAP.get(apf),
        lwn       = float(lwn) if lwn else None,
        zff       = int(zff) if zff else None,
        single_sided = single,
        source    = f"{name}: Fourier Transformation.ZFF/APF, Instrument.LWN, "
                    f"Acquisition.AQM",
    )
    return ils


def read_bruker(path: str | Path) -> Spectrum:
    """
    Read a Bruker OPUS file.

    Parameters
    ----------
    path : str or Path
        Path to the OPUS file (any numeric extension).

    Returns
    -------
    Spectrum
        Wavenumber axis in cm⁻¹, ascending.

    Raises
    ------
    plumeIR.exceptions.SpectrumIOError
        If the file cannot be read or contains no single-beam data.
    """
    from plumeIR.exceptions import SpectrumIOError

    try:
        from brukeropusreader import read_file as read_opus
    except ModuleNotFoundError as exc:
        raise SpectrumIOError(
            "Reading Bruker OPUS files requires the optional 'brukeropusreader' "
            "package (pip install brukeropusreader, or install plumeIR with the "
            "[bruker] extra)."
        ) from exc

    path = Path(path)
    try:
        opus = read_opus(str(path))
    except Exception as exc:
        raise SpectrumIOError(f"Cannot read Bruker file '{path.name}': {exc}") from exc

    if "ScSm" not in opus:
        raise SpectrumIOError(
            f"Bruker file '{path.name}' contains no single-beam data (ScSm block missing)"
        )

    # Spectral data — get_range returns the wavenumber array (may be descending)
    y  = np.asarray(opus["ScSm"], dtype=float)
    x  = np.asarray(opus.get_range("ScSm"), dtype=float)

    # Timestamp: file metadata first, filename fallback
    timestamp = _timestamp_from_bruker(opus) or _timestamp_from_name(path)
    if timestamp is None:
        logger.warning("Could not extract timestamp from '%s'", path.name)

    # Blackbody detection via Optik channel flag
    is_bb   = False
    bb_temp = None
    try:
        if opus["Optik"]["CHN"] == "Reference":
            is_bb = True
            tsm   = opus["Instrument"].get("TSM")
            if tsm is not None:
                bb_temp = float(tsm) + 273.15  # °C → K
    except KeyError:
        pass

    # Preserve key metadata sections
    metadata: dict = {}
    for section in ("Optik", "Fourier Transformation", "Sample", "Instrument", "Acquisition"):
        if section in opus:
            metadata[section] = dict(opus[section])

    ils = _bruker_ils(metadata, x, path.name)

    return Spectrum(
        x=x,
        y=y,
        x_unit="wn",
        timestamp=timestamp,
        filename=path,
        spec_type=SpectrumType.SBM,
        ils=ils,
        metadata=metadata,
        is_blackbody=is_bb,
        bb_temp=bb_temp,
    )


# =============================================================================
# MIDAC reader
# =============================================================================

def _midac_ils(spc_obj: "SPCFile", x: np.ndarray, path: Path) -> InstrumentILS:
    """
    Recover ILS parameters from an SPC log block.

    MIDAC/Essential FTIR writes them as free text: the ``key = value`` lines
    carry 'Laser Wavenumber', 'Laser Sampling Interval', 'Zero Fill' and
    'Apodization'; others read ``POINTS <n>``, ``ZPD <n>``, ``SKIP POINTS <n>``
    and ``IGRAMSIDE Single|Double``.  A single-sided record keeps ``ZPD -
    SKIP POINTS`` points before ZPD (256 in every local file;
    :class:`InstrumentILS`).  Missing keys give ``None`` rather than a guess;
    a file without ``IGRAMSIDE`` is taken as double-sided, with a warning.
    """
    log = {k.strip().lower(): v.strip() for k, v in spc_obj.log.items()}

    def _get(name):
        for k, v in log.items():
            if name in k:
                return v
        return None

    lwn = zff = n_points = zpd = skip = None
    side = None
    try:
        raw = _get("laser wavenumber")
        lwn = float(raw) if raw else None
    except ValueError:
        pass
    try:
        raw = _get("laser sampling interval")
        interval = float(raw) if raw else 1.0
    except ValueError:
        interval = 1.0
    try:
        raw = _get("zero fill")            # e.g. "2 x"
        zff = int(str(raw).split()[0]) if raw else None
    except (ValueError, IndexError):
        pass
    def _int(text: str) -> int | None:
        try:
            return int(text.split()[-1])
        except (ValueError, IndexError):
            return None

    for line in spc_obj.log_lines:
        text = line.strip()
        upper = text.upper()
        if upper.startswith("POINTS") and n_points is None:
            n_points = _int(text)
        elif upper.startswith("ZPD") and zpd is None:
            zpd = _int(text)
        elif upper.startswith("SKIP POINTS") and skip is None:
            skip = _int(text)
        elif upper.startswith("IGRAMSIDE") and side is None:
            side = text.split()[-1].lower() if len(text.split()) > 1 else None

    if side is None:
        logger.warning("%s: no IGRAMSIDE in the SPC log; taken as double-sided "
                       "for max_opd", path.name)
    single = side == "single"
    n_pre = (zpd - skip) if single and zpd is not None and skip is not None else None

    apod = _get("apodization")
    # The SPC name is already in ils.py's substring vocabulary ('triangle',
    # 'boxcar', ...); pass it through rather than mapping and losing anything
    # unrecognised silently.
    return InstrumentILS(
        max_opd   = _derive_max_opd(x, zff, n_points=n_points, lwn=lwn,
                                    single_sided=single, n_pre_zpd=n_pre,
                                    interval=interval),
        apod_type = apod or None,
        lwn       = lwn,
        zff       = zff,
        n_points  = n_points,
        single_sided = None if side is None else single,
        n_pre_zpd = n_pre,
        source    = f"{path.name}: SPC log (Laser Wavenumber / Zero Fill / "
                    f"Apodization / POINTS / ZPD / SKIP POINTS / IGRAMSIDE)",
    )


def read_midac(path: str | Path) -> Spectrum:
    """
    Read a MIDAC SPC file.

    Parameters
    ----------
    path : str or Path
        Path to the ``.spc`` file.

    Returns
    -------
    Spectrum
        Wavenumber axis in cm⁻¹, ascending.

    Raises
    ------
    plumeIR.exceptions.SpectrumIOError
        If the file cannot be read.
    """
    path = Path(path)
    spc_obj = read_spc(path)          # raises SpectrumIOError
    if len(spc_obj.subfiles) != 1:
        raise SpectrumIOError(
            f"MIDAC file '{path.name}' holds {len(spc_obj.subfiles)} spectra; "
            f"plumeIR expects one spectrum per file")

    x = np.asarray(spc_obj.x, dtype=float)
    y = np.asarray(spc_obj.y, dtype=float)

    # spec_type: ylabel first, filename suffix fallback
    spec_type = _MIDAC_YLABEL_MAP.get(
        getattr(spc_obj, "ylabel", ""),
        _MIDAC_SUFFIX_MAP.get(path.stem.rsplit("_", 1)[-1].lower(), SpectrumType.SBM),
    )

    # Timestamp: filename first (most reliable), SPC metadata fallback
    timestamp = _timestamp_from_name(path) or _timestamp_from_spc(spc_obj)
    if timestamp is None:
        logger.warning("Could not extract timestamp from '%s'", path.name)

    ils = _midac_ils(spc_obj, x, path)

    return Spectrum(
        x=x,
        y=y,
        x_unit="wn",
        timestamp=timestamp,
        filename=path,
        spec_type=spec_type,
        metadata={},
        ils=ils,
    )


# =============================================================================
# Dispatcher
# =============================================================================

def read_spectrum(path: str | Path) -> Spectrum:
    """
    Read a spectrum file, auto-detecting format from the extension.

    Bruker files have purely numeric extensions (e.g. ``.0``, ``.0001``).
    MIDAC files have known text extensions (``.spc``, ``.sbm``, etc.).

    Parameters
    ----------
    path : str or Path
        Path to the spectrum file.

    Returns
    -------
    Spectrum

    Raises
    ------
    plumeIR.exceptions.SpectrumIOError
        If the format is unrecognised or the file cannot be read.
    """
    from plumeIR.constants import MIDAC_EXTENSIONS
    from plumeIR.exceptions import SpectrumIOError

    path = Path(path)
    ext  = path.suffix.lstrip(".")

    if ext in MIDAC_EXTENSIONS:
        return read_midac(path)

    if ext.isdigit():
        return read_bruker(path)

    raise SpectrumIOError(
        f"Unrecognised file format for '{path.name}' (extension: '{ext}'). "
        f"Expected a numeric extension (Bruker) or one of {MIDAC_EXTENSIONS} (MIDAC)."
    )


# =============================================================================
# Instrument ILS resolution
# =============================================================================

#: Fallback maximum OPD [cm] when no file supplies one: the double-sided
#: value for a 32768-point record at a ~15798 cm-1 HeNe (2L = 2.074 cm), as
#: on the Bruker here; the single-sided MIDAC files give 2.058 cm from their
#: own headers.  A *documented guess* for formats that carry no ILS metadata,
#: and using it is always warned about — a silent default is exactly how the
#: 2x error survived.
FALLBACK_MAX_OPD_CM: float = 1.037


def resolve_instrument_ils(retrieval, spectra):
    """
    Resolve ``max_opd`` and ``apod_type`` for a run, and return a new
    ``Retrieval`` carrying the resolved values.

    Priority, highest first:

      1. an explicit value in the config  (``*_explicit`` set by the builder)
      2. the value read from the data files
      3. ``FALLBACK_MAX_OPD_CM`` / the config default, with a warning

    Resolved **once per run, not per spectrum**: a run is one instrument, and
    letting the ILS drift between fits would be unphysical.  Spectra that
    disagree are warned about and the first value is used.

    Parameters
    ----------
    retrieval : Retrieval
        Run retrieval config.
    spectra : sequence of Spectrum
        The measured spectra; only their ``ils`` field is read.

    Returns
    -------
    Retrieval
        A copy with ``max_opd`` / ``apod_type`` resolved and ``ils_resolved``
        set.  A retrieval already resolved is returned unchanged.
    """
    from dataclasses import replace as _replace

    if retrieval.ils_resolved:
        return retrieval
    found = [s.ils for s in spectra if getattr(s, "ils", None) is not None]

    def _consensus(attr):
        vals = [getattr(i, attr) for i in found if getattr(i, attr) is not None]
        if not vals:
            return None, None
        first = vals[0]
        if isinstance(first, float):
            odd = [v for v in vals if abs(v - first) > 1e-3 * max(abs(first), 1)]
        else:
            odd = [v for v in vals if v != first]
        if odd:
            logger.warning(
                "Spectra disagree on %s (%r vs %r in %d of %d) — using the "
                "first; a run should be one instrument",
                attr, first, odd[0], len(odd), len(vals),
            )
        src = next((i.source for i in found if getattr(i, attr) is not None), "")
        return first, src

    out = retrieval

    if retrieval.max_opd_explicit:
        logger.info("max_opd %.5f cm (from config, overriding the file)",
                    retrieval.max_opd)
    else:
        val, src = _consensus("max_opd")
        if val is not None:
            logger.info("max_opd %.5f cm (from %s)", val, src)
            out = _replace(out, max_opd=float(val))
        else:
            logger.warning(
                "max_opd not available from the data files and not set in the "
                "config; falling back to %.3f cm. Verify it — a wrong OPD is "
                "absorbed by a fitted fov and biases retrieved columns.",
                FALLBACK_MAX_OPD_CM,
            )
            out = _replace(out, max_opd=FALLBACK_MAX_OPD_CM)

    if retrieval.apod_type_explicit:
        logger.info("apod_type %r (from config, overriding the file)",
                    retrieval.apod_type)
    else:
        val, src = _consensus("apod_type")
        if val is not None:
            logger.info("apod_type %r (from %s)", val, src)
            out = _replace(out, apod_type=str(val))
        else:
            logger.warning("apod_type not available from the data files; "
                           "using the config default %r", retrieval.apod_type)

    return _replace(out, ils_resolved=True)
