"""
``preprocess.npz``: the whole-spectrum preprocessing of a run, for plots.

``preprocess.csv`` holds one row of numbers per spectrum; this file holds
what a spectral plot needs: each spectrum as the windows fit it (full
resolution, float32), its hulls, the raw beam and the background in the
opaque bands (layer), the registration's sub-band lags, the reference the
registration correlated with, and the bands involved.  Smooth arrays live on
a coarse grid (``products.COARSE_STEP``, 2 cm⁻¹), so the file stays a few
tens of MB for a 418-spectrum layer run.

Written by the run folder (:mod:`plumeIR.output`), read by
:func:`read_preprocess_archive` for the plots (:mod:`plumeIR.plotting.preprocess`)
and a GUI.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from plumeIR.exceptions import RunFolderError

ARCHIVE_NAME = "preprocess.npz"
TABLE_NAME = "preprocess.csv"
#: Layout version; bumped when a key changes meaning.
ARCHIVE_FORMAT = 1


def write_preprocess_archive(report, spectra, windows: dict, geometry: str,
                             run_dir: Path) -> Path | None:
    """
    Write ``preprocess.npz``.

    Parameters
    ----------
    report : PreprocessReport
        ``RunSetup.preprocess_report``.
    spectra : list of Spectrum
        The spectra the windows fit (``RunSetup.spectra``), in the order of
        ``report.table``.
    windows : dict
        ``FitWindowConfig`` per label (their ranges are drawn on the plots).
    geometry : str
        ``'layer'``, ``'emission'`` or ``'solar'``.
    run_dir : Path

    Returns
    -------
    Path or None
        The file; ``None`` when nothing was measured.

    Notes
    -----
    Keys (``n`` spectra; ``m`` coarse points; ``k`` sub-bands at most)::

        format, geometry
        spectrum_id     (n,) str
        window_label    (w,) str;  window_range (w, 2)
        wn              (n_wn,) or (n, n_wn) when grids differ;  n_wn (n,)
        y               (n, n_wn) float32, the spectra as fitted
        wn_coarse       (m,)
        top, bottom     (n, m) float32 hulls
        raw, background (n, m) float32, layer, when the background ran
        reference       (m,) float32, layer, the reference top hull
        opaque_bands    (b, 2); relative_mask_bands (p, 2); excluded (e, 2)
        reg_centres, reg_lags, reg_peaks (n, k), reg_used (n, k) bool
        template_wn, template   the registration's reference spectrum
        relative_f, relative_dT, relative_rms  (n,), layer

    Written under a temporary name and renamed.
    """
    if report is None or not report.arrays:
        return None
    a = dict(report.arrays)
    n_wn = np.asarray([sp.wn.size for sp in spectra], dtype=int)
    width = int(n_wn.max())
    same = all(sp.wn.size == spectra[0].wn.size and np.array_equal(sp.wn, spectra[0].wn)
               for sp in spectra)
    y = np.full((len(spectra), width), np.nan, dtype=np.float32)
    for i, sp in enumerate(spectra):
        y[i, :sp.y.size] = sp.y
    if same:
        wn = np.asarray(spectra[0].wn, float)
    else:
        wn = np.full((len(spectra), width), np.nan)
        for i, sp in enumerate(spectra):
            wn[i, :sp.wn.size] = sp.wn
    arrays = {
        "format": np.asarray(ARCHIVE_FORMAT),
        "geometry": np.asarray(geometry),
        "spectrum_id": np.asarray(report.table["spectrum_id"].astype(str), dtype=str),
        "window_label": np.asarray(list(windows), dtype=str),
        "window_range": np.asarray([(w.wn_start, w.wn_stop) for w in windows.values()],
                                   float).reshape(-1, 2),
        "wn": wn, "n_wn": n_wn, "y": y,
        "opaque_bands": np.asarray(report.bands, float).reshape(-1, 2),
        "excluded": np.asarray(report.excluded, float).reshape(-1, 2),
        **a,
    }
    if report.relative is not None:
        rel = report.relative.reindex(report.table["spectrum_id"])
        for col in ("f", "dT", "rms"):
            arrays[f"relative_{col}"] = rel[col].to_numpy(float)
    path = Path(run_dir) / ARCHIVE_NAME
    tmp = Path(run_dir) / (ARCHIVE_NAME + ".part")
    with open(tmp, "wb") as fh:
        np.savez_compressed(fh, **arrays)
    os.replace(tmp, path)
    return path


@dataclass
class PreprocessArchive:
    """
    A run's preprocessing, read from its folder.

    Attributes
    ----------
    arrays : dict
        Everything in ``preprocess.npz``.
    table : pandas.DataFrame
        ``preprocess.csv``, indexed by ``spectrum_id``, in the archive's order.
    """
    arrays: dict
    table:  pd.DataFrame

    def __len__(self) -> int:
        return len(self.spectrum_ids)

    @property
    def geometry(self) -> str:
        return str(self.arrays["geometry"])

    @property
    def spectrum_ids(self) -> list[str]:
        return [str(s) for s in self.arrays["spectrum_id"]]

    @property
    def windows(self) -> dict[str, tuple[float, float]]:
        """Window ranges [cm⁻¹] by label."""
        return {str(lab): (float(r[0]), float(r[1])) for lab, r in
                zip(self.arrays["window_label"], self.arrays["window_range"])}

    def get(self, key: str):
        """An array, or ``None`` when the run did not produce it."""
        return self.arrays.get(key)

    def bands(self, key: str) -> list[tuple[float, float]]:
        """``opaque_bands``, ``excluded`` or ``relative_mask_bands`` as pairs."""
        b = self.arrays.get(key)
        return [] if b is None else [(float(lo), float(hi)) for lo, hi in b]

    def spectrum(self, k: int) -> tuple[np.ndarray, np.ndarray]:
        """``(wn, y)`` of the ``k``-th spectrum, as fitted."""
        n = int(self.arrays["n_wn"][k])
        wn = self.arrays["wn"]
        wn = wn[:n] if wn.ndim == 1 else wn[k, :n]
        return wn, self.arrays["y"][k, :n].astype(float)

    def coarse(self, key: str, k: int) -> np.ndarray | None:
        """Row ``k`` of a coarse per-spectrum array (``top``, ``raw``, ...)."""
        a = self.arrays.get(key)
        return None if a is None else a[k].astype(float)

    def subbands(self, k: int):
        """``(centres, lags, peaks, used)`` of the ``k``-th spectrum, or ``None``."""
        if "reg_centres" not in self.arrays:
            return None
        c = self.arrays["reg_centres"][k]
        ok = np.isfinite(c)
        return (c[ok], self.arrays["reg_lags"][k][ok], self.arrays["reg_peaks"][k][ok],
                self.arrays["reg_used"][k][ok].astype(bool))

    @property
    def relative(self) -> pd.DataFrame | None:
        """Layer: ``f``, ``dT`` [K] and ``rms`` per spectrum."""
        if "relative_f" not in self.arrays:
            return None
        return pd.DataFrame({c: self.arrays[f"relative_{c}"] for c in ("f", "dT", "rms")},
                            index=pd.Index(self.spectrum_ids, name="spectrum_id"))


def read_preprocess_archive(run_dir: str | Path) -> PreprocessArchive:
    """
    Read a run folder's ``preprocess.npz`` and ``preprocess.csv``.

    Raises
    ------
    RunFolderError
        If either file is missing or of an unknown layout.
    """
    run_dir = Path(run_dir).expanduser()
    path, table = run_dir / ARCHIVE_NAME, run_dir / TABLE_NAME
    if not path.is_file() or not table.is_file():
        raise RunFolderError(f"{run_dir} has no {ARCHIVE_NAME} / {TABLE_NAME}: the run "
                             f"wrote no whole-spectrum preprocessing")
    with np.load(path, allow_pickle=False) as npz:
        arrays = {k: npz[k] for k in npz.files}
    version = int(arrays.get("format", -1))
    if version != ARCHIVE_FORMAT:
        raise RunFolderError(f"{path}: layout version {version}, this plumeIR reads "
                             f"version {ARCHIVE_FORMAT}")
    t = pd.read_csv(table).set_index("spectrum_id")
    t = t.reindex([str(s) for s in arrays["spectrum_id"]])
    return PreprocessArchive(arrays=arrays, table=t)
