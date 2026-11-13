"""
Reader for Galactic / Thermo GRAMS ``.spc`` files, new little-endian format.

Replaces the ``pyspectra`` → ``spc_spectra`` dependency (decided 2026-11-13):
``spc_spectra`` is published as a source distribution only,
its ``setup.py`` imports numpy and ``distutils`` at build time and so fails
under pip's build isolation, and its licence is undeclared.  plumeIR needs a
small part of the format, which the specification makes straightforward.

Supported (format byte ``0x4B``, the format every current instrument writes):

* x axis evenly spaced (``ffirst``, ``flast``, ``fnpts``), a shared float32
  x array (``TXVALS``), or one x array per sub-spectrum (``TXYXYS``, with or
  without a sub-file directory);
* y as float32 (exponent ``0x80``) or scaled integers, 32-bit or 16-bit
  (``TSPREC``), with the file exponent or, for ``TMULTI`` files, each
  sub-spectrum's own;
* any number of sub-spectra;
* the header date and the log block (text lines, ``key = value`` pairs).

Refused with :class:`~plumeIR.exceptions.SpectrumIOError`: the old format
(``0x4D``, before 1996), big-endian new format (``0x4C``) and anything else.

Validation (2026-11-13): on the 1440 MIDAC files under ``data/`` the arrays
are bit-identical to ``spc_spectra`` 0.4.0 and the log entries equal.  Every
one of those files is the same variant (single spectrum, even x, float32 y),
so the other variants are checked only against files written by
``test/test_spc.py`` and read by both readers.  Three departures from
``spc_spectra``, each following the specification, none affecting the MIDAC
variant: integer-scaling exponents are signed (``spc_spectra`` sets negative
ones to zero); ``TXYXYS`` x arrays are float32 (``spc_spectra`` decodes them as
scaled integers); the log text ends at the log block's end or its first NUL
(``spc_spectra`` reads ``logsizd`` bytes past the text offset).

References
----------
Thermo Galactic, "GRAMS SPC file format" (``SPC.H``, 1996-1997), the
specification behind both implementations.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

import numpy as np

from plumeIR.exceptions import SpectrumIOError

# Main header, 512 bytes (SPC.H, SPCHDR).  Signed exponent ('b').
_HEAD = struct.Struct("<BBBbiddiBBBBi9s9sh32s130s30siiBBhf48sfifB187s")
_SUBHEAD = struct.Struct("<Bbhfffiif4s")          # 32 bytes (SUBHDR)
_LOGHEAD = struct.Struct("<iiiii44s")             # 64 bytes (LOGSTC)
_DIRENT = struct.Struct("<iif")                   # 12 bytes (SSFSTC)
assert _HEAD.size == 512 and _SUBHEAD.size == 32 and _LOGHEAD.size == 64

_FLOAT_EXP = -128                                  # 0x80 as a signed byte

# ftflgs bits
_TSPREC, _TCGRAM, _TMULTI, _TRANDM, _TORDRD, _TALABS, _TXYXYS, _TXVALS = (
    1 << i for i in range(8))

_VERSIONS = {0x4B: "new LSB", 0x4C: "new MSB", 0x4D: "old"}

#: y-axis unit names by ``fytype`` (SPC.H), as ``spc_spectra`` labels them.
YTYPE_LABELS: dict[int, str] = {
    0: "Arbitrary Intensity", 1: "Interferogram", 2: "Absorbance",
    3: "Kubelka-Munk", 4: "Counts", 5: "Volts", 6: "Degrees", 7: "Milliamps",
    8: "Millimeters", 9: "Millivolts", 10: "Log(1/R)", 11: "Percent",
    12: "Intensity", 13: "Relative Intensity", 14: "Energy", 16: "Decibel",
    19: "Temperature (F)", 20: "Temperature (C)", 21: "Temperature (K)",
    22: "Index of Refraction [N]", 23: "Extinction Coeff. [K]", 24: "Real",
    25: "Imaginary", 26: "Complex",
    128: "Transmission", 129: "Reflectance",
    130: "Arbitrary or Single Beam with Valley Peaks", 131: "Emission",
}

#: x-axis unit names by ``fxtype`` (SPC.H).
XTYPE_LABELS: dict[int, str] = {
    0: "Arbitrary", 1: "Wavenumber (cm-1)", 2: "Micrometers (um)",
    3: "Nanometers (nm)", 4: "Seconds", 5: "Minutes", 6: "Hertz (Hz)",
    7: "Kilohertz (KHz)", 8: "Megahertz (MHz)", 9: "Mass (M/z)",
    10: "Parts per million (PPM)", 11: "Days", 12: "Years",
    13: "Raman Shift (cm-1)", 14: "eV", 16: "Diode Number", 17: "Channel",
    18: "Degrees", 19: "Temperature (F)", 20: "Temperature (C)",
    21: "Temperature (K)", 22: "Data Points", 23: "Milliseconds (mSec)",
    24: "Microseconds (uSec)", 25: "Nanoseconds (nSec)", 26: "Gigahertz (GHz)",
    27: "Centimeters (cm)", 28: "Meters (m)", 29: "Millimeters (mm)",
    30: "Hours",
}


@dataclass(frozen=True)
class SPCSubfile:
    """One spectrum of an ``.spc`` file."""
    x: np.ndarray
    y: np.ndarray


@dataclass(frozen=True)
class SPCFile:
    """
    Contents of an ``.spc`` file.

    Attributes
    ----------
    subfiles : list of SPCSubfile
        Every spectrum, in file order.
    xtype, ytype : int
        Axis unit codes (``fxtype``, ``fytype``).
    xlabel, ylabel : str
        Their names; the file's own text labels when it carries them
        (``TALABS``).
    timestamp : datetime or None
        The header date (to the minute), ``None`` when unset.
    comment : str
        The header comment (``fcmnt``).
    log_lines : list of str
        Text of the log block, one entry per line (empty without a log).
    log : dict[str, str]
        ``key = value`` lines of the log, both sides stripped.
    flags : int
        The raw ``ftflgs`` byte.
    """
    subfiles:  list[SPCSubfile]
    xtype:     int
    ytype:     int
    xlabel:    str
    ylabel:    str
    timestamp: datetime | None
    comment:   str
    log_lines: list[str] = field(default_factory=list)
    log:       dict[str, str] = field(default_factory=dict)
    flags:     int = 0

    @property
    def x(self) -> np.ndarray:
        """x axis of the first spectrum."""
        return self.subfiles[0].x

    @property
    def y(self) -> np.ndarray:
        """y values of the first spectrum."""
        return self.subfiles[0].y


def _text(raw: bytes) -> str:
    return raw.split(b"\x00", 1)[0].decode("latin-1").strip()


def _decode_y(data: bytes, pos: int, npts: int, exp: int, short: bool,
              where: str) -> tuple[np.ndarray, int]:
    """y values at ``pos``: float32, or integers scaled by 2**(exp - bits)."""
    if exp == _FLOAT_EXP:
        dtype, bits = np.dtype("<f4"), 0
    elif short:
        dtype, bits = np.dtype("<i2"), 16
    else:
        dtype, bits = np.dtype("<i4"), 32
    end = pos + npts * dtype.itemsize
    if end > len(data):
        raise SpectrumIOError(f"{where}: y data runs past the end of the file "
                              f"({npts} points at byte {pos})")
    raw = np.frombuffer(data, dtype=dtype, count=npts, offset=pos)
    y = raw.astype(float) if bits == 0 else raw.astype(float) * 2.0 ** (exp - bits)
    return y, end


def _decode_x(data: bytes, pos: int, npts: int, where: str) -> tuple[np.ndarray, int]:
    """A float32 x array at ``pos``."""
    end = pos + 4 * npts
    if end > len(data):
        raise SpectrumIOError(f"{where}: x data runs past the end of the file "
                              f"({npts} points at byte {pos})")
    return np.frombuffer(data, dtype="<f4", count=npts, offset=pos).astype(float), end


def _date(packed: int) -> datetime | None:
    # SPC.H fdate: minute 6 bits, hour 5, day 5, month 4, year 12.
    year = packed >> 20
    if year == 0:
        return None
    month, day = (packed >> 16) & 0xF, (packed >> 11) & 0x1F
    hour, minute = (packed >> 6) & 0x1F, packed & 0x3F
    try:
        return datetime(year, month, day, hour, minute)
    except ValueError:
        return None


def read_spc(path: str | Path) -> SPCFile:
    """
    Read an ``.spc`` file (new little-endian format).

    Parameters
    ----------
    path : str or Path

    Returns
    -------
    SPCFile

    Raises
    ------
    plumeIR.exceptions.SpectrumIOError
        Unreadable file, unsupported format version, or a header that does
        not match the data.
    """
    path = Path(path)
    try:
        data = path.read_bytes()
    except OSError as exc:
        raise SpectrumIOError(f"Cannot read '{path}': {exc}") from exc
    where = path.name
    if len(data) < 2:
        raise SpectrumIOError(f"{where}: too short to be an SPC file")
    version = data[1]
    if version != 0x4B:
        name = _VERSIONS.get(version, f"unknown (0x{version:02X})")
        raise SpectrumIOError(
            f"{where}: SPC format '{name}' is not supported; plumeIR reads the "
            f"new little-endian format (0x4B), which current instruments write")
    if len(data) < _HEAD.size:
        raise SpectrumIOError(f"{where}: truncated SPC header")

    (flags, _, _fexper, fexp, fnpts, ffirst, flast, fnsub, fxtype, fytype,
     _fztype, _fpost, fdate, _fres, _fsource, _fpeakpt, _fspare, fcmnt,
     fcatxt, flogoff, *_rest) = _HEAD.unpack_from(data)

    short, multi = bool(flags & _TSPREC), bool(flags & _TMULTI)
    xyxys, xvals = bool(flags & _TXYXYS), bool(flags & _TXVALS)
    if fnsub < 1:
        raise SpectrumIOError(f"{where}: header declares {fnsub} spectra")

    subfiles: list[SPCSubfile] = []
    pos = _HEAD.size
    if xyxys:
        # Each spectrum carries its own float32 x array.  With a directory,
        # fnpts is the directory's byte offset (SPC.H, TXYXYS).
        if fnpts > 0:
            offsets = []
            for i in range(fnsub):
                ent = fnpts + i * _DIRENT.size
                if ent + _DIRENT.size > len(data):
                    raise SpectrumIOError(f"{where}: sub-file directory truncated")
                offsets.append(_DIRENT.unpack_from(data, ent)[0])
        else:
            offsets = None
        for i in range(fnsub):
            start = offsets[i] if offsets is not None else pos
            if start + _SUBHEAD.size > len(data):
                raise SpectrumIOError(f"{where}: sub-file {i} header truncated")
            sub = _SUBHEAD.unpack_from(data, start)
            subexp, subnpts = sub[1], sub[6]
            exp = subexp if multi else fexp
            x, p = _decode_x(data, start + _SUBHEAD.size, subnpts, where)
            y, pos = _decode_y(data, p, subnpts, exp, short, where)
            subfiles.append(SPCSubfile(x, y))
    else:
        if fnpts < 1:
            raise SpectrumIOError(f"{where}: header declares {fnpts} points")
        if xvals:
            x, pos = _decode_x(data, pos, fnpts, where)
        else:
            x = np.linspace(ffirst, flast, num=fnpts)
        for i in range(fnsub):
            if pos + _SUBHEAD.size > len(data):
                raise SpectrumIOError(f"{where}: sub-file {i} header truncated")
            subexp = _SUBHEAD.unpack_from(data, pos)[1]
            exp = subexp if multi else fexp
            y, pos = _decode_y(data, pos + _SUBHEAD.size, fnpts, exp, short, where)
            subfiles.append(SPCSubfile(x, y))

    xlabel = XTYPE_LABELS.get(fxtype, "Unknown")
    ylabel = YTYPE_LABELS.get(fytype, "Unknown")
    if flags & _TALABS:
        parts = fcatxt.split(b"\x00")
        if len(parts) > 2:
            xl, yl = (p.decode("latin-1").strip() for p in parts[:2])
            xlabel, ylabel = xl or xlabel, yl or ylabel

    log_lines: list[str] = []
    log: dict[str, str] = {}
    if flogoff:
        if flogoff + _LOGHEAD.size > len(data):
            raise SpectrumIOError(f"{where}: log block offset past the end of the file")
        logsizd, _logsizm, logtxto, *_ = _LOGHEAD.unpack_from(data, flogoff)
        text_end = min(flogoff + logsizd, len(data)) if logsizd > 0 else len(data)
        raw = data[flogoff + logtxto:text_end].split(b"\x00", 1)[0]
        log_lines = raw.replace(b"\r", b"").decode("latin-1").split("\n")
        for line in log_lines:
            key, sep, value = line.partition("=")
            if sep:
                log[key.strip()] = value.strip()

    return SPCFile(subfiles=subfiles, xtype=fxtype, ytype=fytype,
                   xlabel=xlabel, ylabel=ylabel, timestamp=_date(fdate),
                   comment=_text(fcmnt), log_lines=log_lines, log=log,
                   flags=flags)
