"""
Start-up check for RFM, the line-by-line radiative transfer code plumeIR
builds its references with (through rfmwrapper).

RFM has no published licence and is not redistributable: its author,
Anu Dudhia (University of Oxford), supplies it by email request.  A first run
would otherwise fail deep inside a reference build, so the command line calls
:func:`require_rfm` before it creates a run folder, and fails with the steps
to obtain and configure RFM.  A GUI or script can call it the same way.

Reference: Dudhia, A. (2017).  The Reference Forward Model (RFM).  J. Quant.
Spectrosc. Radiat. Transfer 186, 243-253.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

from plumeIR.exceptions import PlumeIRError

RFM_URL = "https://eodg.atm.ox.ac.uk/RFM/"
RFM_CITATION = ("Dudhia, A. (2017). The Reference Forward Model (RFM). "
                "J. Quant. Spectrosc. Radiat. Transfer 186, 243-253.")


class RFMNotFoundError(PlumeIRError):
    """RFM, or the HITRAN file it reads, is not where rfmwrapper looks."""


@dataclass(frozen=True)
class RFMStatus:
    """
    Where rfmwrapper looks for RFM, and what it finds.

    Attributes
    ----------
    rfm_dir : Path
        rfmwrapper's ``rfm_dir`` (``RFM_DIR``, then
        ``~/.config/rfmwrapper/config.ini``, then ``~/RFM``).
    executable : Path
        ``<rfm_dir>/source/rfm``.
    hitran : Path or None
        The HITRAN binary rfmwrapper resolves, if any.
    problems : tuple of str
        What is missing; empty when RFM is usable.
    """
    rfm_dir:    Path
    executable: Path
    hitran:     Path | None
    problems:   tuple[str, ...]

    @property
    def ok(self) -> bool:
        return not self.problems


def check_rfm() -> RFMStatus:
    """
    Look for the RFM executable and its HITRAN file where rfmwrapper will.

    Returns
    -------
    RFMStatus
    """
    from rfmwrapper.config import get_exec_path, get_hitran_path, get_rfm_dir

    rfm_dir = Path(get_rfm_dir())
    exe = Path(get_exec_path())
    hitran_raw = get_hitran_path()
    hitran = Path(hitran_raw) if hitran_raw else None

    problems = []
    if not rfm_dir.is_dir():
        problems.append(f"RFM directory not found: {rfm_dir}")
    if not exe.is_file():
        problems.append(f"RFM executable not found: {exe}")
    elif not os.access(exe, os.X_OK):
        problems.append(f"RFM executable is not executable: {exe}")
    if hitran is None:
        problems.append(f"No HITRAN binary (hitran*.bin) under {rfm_dir / 'Hitran'}, "
                        f"and HITRAN_PATH / hitran_path are unset")
    elif not hitran.is_file():
        problems.append(f"HITRAN binary not found: {hitran}")
    return RFMStatus(rfm_dir, exe, hitran, tuple(problems))


def instructions(status: RFMStatus) -> str:
    """The message :func:`require_rfm` raises with."""
    found = "\n".join(f"  - {p}" for p in status.problems)
    return (
        f"RFM is not set up:\n{found}\n\n"
        f"plumeIR computes its references with RFM, which is not distributed "
        f"with plumeIR.\n"
        f"  1. Request RFM from its author, Anu Dudhia (University of Oxford): "
        f"{RFM_URL}\n"
        f"  2. Build it so the executable is <rfm_dir>/source/rfm, and convert "
        f"a HITRAN line list to RFM's binary format (hitran*.bin under "
        f"<rfm_dir>/Hitran/) with RFM's own tools.\n"
        f"  3. Point rfmwrapper at it: python -m rfmwrapper configure "
        f"(or set RFM_DIR, and HITRAN_PATH if the binary is elsewhere).\n"
        f"Cite RFM as: {RFM_CITATION}"
    )


def require_rfm() -> RFMStatus:
    """
    :func:`check_rfm`, raising when RFM is not usable.

    Returns
    -------
    RFMStatus

    Raises
    ------
    RFMNotFoundError
        With the steps to obtain and configure RFM.
    """
    status = check_rfm()
    if not status.ok:
        raise RFMNotFoundError(instructions(status))
    return status
