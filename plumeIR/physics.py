"""
Fundamental radiometric physics for plumeIR v3.

DEPENDENCY RULE: this module imports only numpy — never from any other
plumeIR module.  It is a strict leaf so that both ``plumeIR.reference`` and
``plumeIR.forward`` can use the Planck functions without creating a circular
dependency (reference → forward would otherwise be backwards).

Naming follows the RFMwrapper convention (``rad`` for the Planck radiance,
``bbt`` for brightness temperature), so the two packages read alike.  Note the
signatures are not identical: RFMwrapper's ``rad`` carries a ``wl`` switch for
wavelength output; plumeIR works on the wavenumber model grid only.
"""

from __future__ import annotations

import numpy as np

# ---------------------------------------------------------------------------
# Radiometric constants (CODATA 2018), in units giving radiance in
# mW / (m² sr cm⁻¹) when wavenumber is in cm⁻¹ and temperature in K.
# ---------------------------------------------------------------------------
_C1: float = 1.191042972e-5   # 2 h c²   [mW cm² / (m² sr)]
_C2: float = 1.4387768775     # h c / k_B [cm K]


def rad(wn: np.ndarray, temp: float, norm: bool = False) -> np.ndarray:
    """
    Planck blackbody spectral radiance.

    Parameters
    ----------
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    temp : float
        Temperature [K].
    norm : bool
        If ``True``, normalise the curve to its peak value.

    Returns
    -------
    np.ndarray
        Spectral radiance [mW / (m²·sr·cm⁻¹)], shape ``(n_wn,)``.
    """
    wn = np.asarray(wn, dtype=float)
    out = _C1 * wn ** 3 / np.expm1(_C2 * wn / temp)
    if norm:
        out = out / out.max()
    return out


def bbt(wn: np.ndarray, rad_in: np.ndarray) -> np.ndarray:
    """
    Brightness temperature from spectral radiance (inverse Planck).

    Parameters
    ----------
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    rad_in : np.ndarray
        Spectral radiance [mW / (m²·sr·cm⁻¹)], shape ``(n_wn,)``.

    Returns
    -------
    np.ndarray
        Brightness temperature [K], shape ``(n_wn,)``.
    """
    wn     = np.asarray(wn, dtype=float)
    rad_in = np.asarray(rad_in, dtype=float)
    return _C2 * wn / np.log1p(_C1 * wn ** 3 / rad_in)
