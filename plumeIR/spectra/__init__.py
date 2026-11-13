"""plumeIR.spectra — spectrum I/O and file discovery."""

from plumeIR.spectra.spectrum import (
    InstrumentILS,
    resolve_instrument_ils,
    Spectrum,
    SpectrumType,
    read_spectrum,
    read_bruker,
    read_midac,
)
from plumeIR.spectra.discovery import (
    discover_spectra,
    explain_no_spectra,
    load_spectra,
    sort_by_acquisition_time,
    detect_corrupted,
    move_corrupted,
    rename_with_timestamp,
)

__all__ = [
    "Spectrum", "SpectrumType",
    "read_spectrum", "read_bruker", "read_midac",
    "discover_spectra", "explain_no_spectra", "load_spectra", "sort_by_acquisition_time",
    "detect_corrupted", "move_corrupted", "rename_with_timestamp",
]
