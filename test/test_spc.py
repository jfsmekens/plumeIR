"""
Tests for plumeIR.spectra.spc.read_spc.

Files are written here from known values with a minimal SPC writer that
follows SPC.H, then read back.  Where ``spc_spectra`` is installed and
follows the same part of the specification, it reads the same files as an
independent second implementation.  On 1440 real MIDAC files
the reader was checked bit-identical to spc_spectra 0.4.0.
"""

from __future__ import annotations

import contextlib
import io
import struct
import warnings
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from plumeIR.exceptions import SpectrumIOError
from plumeIR.spectra.spc import read_spc

_HEAD = struct.Struct("<BBBbiddiBBBBi9s9sh32s130s30siiBBhf48sfifB187s")
_SUB = struct.Struct("<Bbhfffiif4s")
TSPREC, TMULTI, TALABS, TXYXYS, TXVALS = 0x01, 0x04, 0x20, 0x40, 0x80


def _date(d: datetime) -> int:
    return (d.year << 20) | (d.month << 16) | (d.day << 11) | (d.hour << 6) | d.minute


def _y_bytes(y: np.ndarray, exp: int, short: bool) -> bytes:
    if exp == -128:
        return y.astype("<f4").tobytes()
    if short:
        return np.round(y / 2.0 ** (exp - 16)).astype("<i2").tobytes()
    return np.round(y / 2.0 ** (exp - 32)).astype("<i4").tobytes()


def write_spc(path: Path, ys: list[np.ndarray], *, x=None, xs=None,
              first=500.0, last=1500.0, flags=0, fexp=-128, subexps=None,
              ytype=0, date=None, log: str | None = None, catxt=b"",
              directory=False, version=0x4B) -> Path:
    """Minimal SPC writer (SPC.H) for the variants plumeIR reads."""
    n = len(ys[0]) if xs is None else 0
    body = b""
    if x is not None:
        body += np.asarray(x, "<f4").tobytes()
    subs = []
    for i, y in enumerate(ys):
        exp = subexps[i] if subexps is not None else fexp
        npts = len(y) if xs is not None else 0
        sub = _SUB.pack(0, exp, i, 0.0, 0.0, 0.0, npts, 0, 0.0, b"\0" * 4)
        if xs is not None:
            sub += np.asarray(xs[i], "<f4").tobytes()
        sub += _y_bytes(np.asarray(y, float), exp, bool(flags & TSPREC))
        subs.append(sub)
    start = 512 + len(body)
    offsets = [start + sum(len(s) for s in subs[:i]) for i in range(len(subs))]
    body += b"".join(subs)
    fnpts = n
    if directory:
        fnpts = 512 + len(body)
        body += b"".join(struct.pack("<iif", o, len(s), 0.0) for o, s in zip(offsets, subs))
    logoff = 0
    if log is not None:
        logoff = 512 + len(body)
        text = log.encode("latin-1") + b"\0"
        body += struct.pack("<iiiii44s", 64 + len(text), 0, 64, 0, 0, b"") + text
    head = _HEAD.pack(flags, version, 0, fexp, fnpts, first, last, len(ys), 1, ytype,
                      0, 0, _date(date) if date else 0, b"", b"", 0, b"",
                      b"a comment", catxt, logoff, 0, 0, 0, 0, 0.0, b"", 0.0, 0,
                      0.0, 0, b"")
    path.write_bytes(head + body)
    return path


def _spc_spectra(path: Path):
    spc = pytest.importorskip("spc_spectra")
    with warnings.catch_warnings(), contextlib.redirect_stdout(io.StringIO()):
        warnings.simplefilter("ignore")
        return spc.File(str(path))


Y = np.array([1.5, -2.25, 3.0, 0.125, 1000.0])


class TestVariants:

    def test_even_x_float_y(self, tmp_path):
        f = read_spc(write_spc(tmp_path / "a.spc", [Y], first=500.0, last=504.0))
        assert np.array_equal(f.x, np.linspace(500.0, 504.0, 5))
        assert np.array_equal(f.y, Y.astype("<f4").astype(float))
        assert len(f.subfiles) == 1 and f.ylabel == "Arbitrary Intensity"
        assert f.comment == "a comment"
        ref = _spc_spectra(tmp_path / "a.spc")
        assert np.array_equal(ref.x, f.x) and np.array_equal(ref.sub[0].y, f.y)

    def test_shared_x_array(self, tmp_path):
        x = np.array([500.0, 501.5, 502.0, 504.0, 510.0])
        f = read_spc(write_spc(tmp_path / "b.spc", [Y], x=x, flags=TXVALS))
        assert np.array_equal(f.x, x)
        ref = _spc_spectra(tmp_path / "b.spc")
        assert np.array_equal(ref.x, f.x) and np.array_equal(ref.sub[0].y, f.y)

    @pytest.mark.parametrize("short, exp", [(False, 11), (True, 11), (False, 3)])
    def test_integer_y(self, tmp_path, short, exp):
        # Values span half the integer range the exponent allows (2**(exp-1)).
        y = Y / np.abs(Y).max() * 2.0 ** (exp - 2)
        p = write_spc(tmp_path / "c.spc", [y], fexp=exp, flags=TSPREC if short else 0)
        f = read_spc(p)
        step = 2.0 ** (exp - (16 if short else 32))
        assert np.allclose(f.y, y, rtol=0, atol=step)
        ref = _spc_spectra(p)
        assert np.array_equal(np.asarray(ref.sub[0].y, float), f.y)

    def test_negative_exponent_is_signed(self, tmp_path):
        """SPC.H stores the exponent as a signed byte; spc_spectra zeroes it."""
        y = np.array([0.001, -0.002, 0.0005])
        f = read_spc(write_spc(tmp_path / "d.spc", [y], fexp=-3))
        assert np.allclose(f.y, y, atol=2.0 ** -35)

    def test_several_spectra_with_own_exponents(self, tmp_path):
        ys = [Y, 2 * Y, -Y]
        p = write_spc(tmp_path / "e.spc", ys, flags=TMULTI, subexps=[11, 12, 11])
        f = read_spc(p)
        assert len(f.subfiles) == 3
        for sub, y in zip(f.subfiles, ys):
            assert np.allclose(sub.y, y, atol=2.0 ** -20)
        ref = _spc_spectra(p)
        for r, sub in zip(ref.sub, f.subfiles):
            assert np.array_equal(np.asarray(r.y, float), sub.y)

    @pytest.mark.parametrize("directory", [False, True])
    def test_own_x_per_spectrum(self, tmp_path, directory):
        xs = [np.array([1.0, 2.0, 3.0]), np.array([10.0, 20.0, 30.0, 40.0])]
        ys = [np.array([5.0, 6.0, 7.0]), np.array([1.0, 2.0, 3.0, 4.0])]
        f = read_spc(write_spc(tmp_path / "f.spc", ys, xs=xs, flags=TXYXYS | TMULTI,
                               subexps=[-128, -128], directory=directory))
        for sub, x, y in zip(f.subfiles, xs, ys):
            assert np.array_equal(sub.x, x) and np.array_equal(sub.y, y)


class TestMetadata:

    def test_date_and_log(self, tmp_path):
        log = ("POINTS 32768\r\nLaser Wavenumber = 15798.037884\r\n"
               "Zero Fill = 2 x\r\nApodization = triangle\r\nnote = a=b")
        f = read_spc(write_spc(tmp_path / "g.spc", [Y], date=datetime(2021, 8, 11, 2, 12),
                               log=log))
        assert f.timestamp == datetime(2021, 8, 11, 2, 12)
        assert f.log_lines[0] == "POINTS 32768"
        assert f.log["Laser Wavenumber"] == "15798.037884"
        assert f.log["note"] == "a=b"           # split on the first '=' only

    def test_no_date(self, tmp_path):
        assert read_spc(write_spc(tmp_path / "h.spc", [Y])).timestamp is None

    def test_text_labels(self, tmp_path):
        f = read_spc(write_spc(tmp_path / "i.spc", [Y], flags=TALABS,
                               catxt=b"cm-1\0Counts here\0z\0"))
        assert (f.xlabel, f.ylabel) == ("cm-1", "Counts here")

    def test_transmission_label(self, tmp_path):
        assert read_spc(write_spc(tmp_path / "j.spc", [Y], ytype=128)).ylabel == "Transmission"


class TestRefusals:

    @pytest.mark.parametrize("version, name", [(0x4D, "old"), (0x4C, "new MSB"),
                                               (0xCF, "unknown")])
    def test_other_formats(self, tmp_path, version, name):
        p = write_spc(tmp_path / "k.spc", [Y], version=version)
        with pytest.raises(SpectrumIOError, match=name):
            read_spc(p)

    def test_truncated(self, tmp_path):
        p = write_spc(tmp_path / "l.spc", [Y])
        p.write_bytes(p.read_bytes()[:-8])
        with pytest.raises(SpectrumIOError, match="past the end"):
            read_spc(p)

    def test_missing_file(self, tmp_path):
        with pytest.raises(SpectrumIOError, match="Cannot read"):
            read_spc(tmp_path / "none.spc")

    def test_read_midac_wants_one_spectrum(self, tmp_path):
        from plumeIR.spectra.spectrum import read_midac
        p = write_spc(tmp_path / "m_sbm.spc", [Y, Y])
        with pytest.raises(SpectrumIOError, match="2 spectra"):
            read_midac(p)
