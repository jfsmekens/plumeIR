"""
The whole-spectrum preprocessing of a run: measure, write, and optionally apply.

Called by :func:`plumeIR.pipeline.prepare_run` after the references are
built, with the raw and the preprocessed spectra.  Per product and ``*PREPROCESS`` mode:

- ``background`` (layer): measured on each **raw** single beam from the
  template's opaque bands.  ``apply`` replaces the preprocessed spectrum by
  ``raw − B`` (self-emission scale plus zero level, unclipped).
- ``registration`` (layer, emission): measured on each spectrum the
  windows will fit (after the background, when that is applied), on its
  own: ``lag = s0 + ε·ν``.  ``apply`` relabels the spectrum's wavenumber
  axis, ``ν' = ν − lag(ν)``, so its lines sit where the model puts them;
  ``shift`` stays free for what is left.  A spectrum that cannot be
  registered keeps its axis (a warning; its ``shift`` fits the shift).

Every product is per spectrum: nothing needs later spectra, so the same
steps serve real-time processing.  Only the template is built once per run.

``report`` measures and writes only: the spectra the windows fit are the
preprocessed ones, unchanged (bit for bit).  A product that fails in report
mode is logged and left out; in apply mode the failure stops the run, since
the fits would otherwise run on something other than what was asked for.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field, replace

import numpy as np
import pandas as pd

from plumeIR.preprocess.background import measure_background
from plumeIR.preprocess.envelope import (
    envelope, opaque_brightness_temperature, reference_hull, relative_planck,
)
from plumeIR.preprocess.registration import (
    EMISSION_MIN_PEAK, LAYER_MIN_PEAK, emission_subbands, fit_registration,
    layer_subbands, plume_bands,
)
from plumeIR.preprocess.template import (
    build_template, convolve_blockwise, instrument_range, template_spec,
)
from plumeIR.config.objects import EmissionGeometry, LayerGeometry
from plumeIR.exceptions import PlumeIRError

logger = logging.getLogger(__name__)

#: Margin [cm⁻¹] kept inside the instrument range for the template.
RANGE_MARGIN = 10.0

#: Each opaque band is shrunk by this much [cm⁻¹] at both ends before the
#: background is measured: a spectrum is displaced from the template by
#: ε·ν (up to 0.7 cm⁻¹ at 4500 cm⁻¹ on the layer set), so the template's
#: band edges can hold source light in the measurement.  Found on a synthetic
#: run with sharp band edges (a 0.55 against 0.84, 2026-11-16).
BAND_MARGIN = 1.0

#: Plume bands left out of the relative Planck fit: where the template's plume
#: optical depth exceeds this fraction of its peak, widened by the margin
#: [cm⁻¹].  Without them ΔT follows SO2 (r +0.52 on the layer set; −0.09
#: with them, 2026-11-16).
RELATIVE_PLUME_FRACTION, RELATIVE_PLUME_MARGIN = 1e-3, 15.0

COL_SPECTRUM = "spectrum_id"

#: Spacing [cm⁻¹] of the coarse grid on which the smooth per-spectrum arrays
#: are kept for plots and ``preprocess.npz`` (hulls are smoothed over 25 cm⁻¹;
#: the raw beam and background are cell means), so 418 layer spectra cost a
#: few MB rather than the full grid's ~30 MB per array.
COARSE_STEP = 2.0


@dataclass(frozen=True)
class PreprocessReport:
    """
    What the stage measured and did, for one run.

    Attributes
    ----------
    background, registration : str
        The modes run (``'off'`` when the geometry has no such product).
    table : pandas.DataFrame
        One row per spectrum (``spectrum_id`` column): ``bkg_*``, ``reg_*``
        and ``env_*`` measurements; ``reg_eps`` / ``reg_s0`` are what the
        registration applies (or would apply).  ``env_peak`` /
        ``env_peak_wn`` / ``env_integral`` from the top hull; layer
        ``env_scale`` (the relative broadband scale f), emission
        ``env_bt_opaque`` (near-field air temperature, K).
    relative : pandas.DataFrame or None
        Layer: per spectrum ``f``, ``dT`` [K] and the fit rms of the relative
        Planck fit against the reference (for plots: ΔT is too noisy per
        spectrum to be written, ~100 K; developer, 2026-11-16).
    reference_spectra : int
        Number of first spectra whose median top hull is the reference.
    arrays : dict
        Per-spectrum arrays for plots and ``preprocess.npz``
        (:mod:`plumeIR.preprocess.archive`): ``wn_coarse`` and, on it,
        ``top`` / ``bottom`` (hulls), ``raw`` / ``background`` (layer, cell
        means of the raw beam and of B), ``reference`` (the reference top
        hull); ``reg_centres`` / ``reg_lags`` / ``reg_peaks`` / ``reg_used``
        (sub-bands, NaN-padded); ``template_wn`` / ``template`` (the
        registration's reference spectrum); ``relative_mask_bands`` (plume
        bands left out of f and ΔT).  Empty when nothing ran.
    bands : tuple of (float, float)
        Opaque bands of the background product [cm⁻¹].
    excluded : tuple of (float, float)
        Plume bands left out of the emission registration [cm⁻¹].
    notes : tuple of str
        Products skipped or failed, and why.
    """
    background:         str
    registration:       str
    table:              pd.DataFrame = field(repr=False)
    relative:           pd.DataFrame | None = field(default=None, repr=False)
    reference_spectra:  int = 0
    arrays:             dict = field(default_factory=dict, repr=False)
    bands:              tuple = ()
    excluded:           tuple = ()
    notes:              tuple = ()

    def summary_lines(self) -> list[str]:
        """Lines for ``summary.txt``."""
        t = self.table
        lines = [f"preprocess      background {self.background}, registration "
                 f"{self.registration}"]
        if "bkg_a" in t and t["bkg_a"].notna().any():
            flagged = int((t["bkg_flags"].fillna("") != "").sum())
            band_txt = ", ".join(f"{lo:.0f}-{hi:.0f}" for lo, hi in self.bands)
            lines.append(
                f"background      self scale a median {t['bkg_a'].median():.3f} "
                f"[{t['bkg_a'].quantile(.05):.3f}, {t['bkg_a'].quantile(.95):.3f}]; "
                f"held-out rms median {t['bkg_heldout_rms'].median():.4g} counts; "
                f"{flagged}/{len(t)} flagged; bands {band_txt} cm-1")
        if "reg_eps" in t and t["reg_eps"].notna().any():
            lines.append(
                f"registration    eps median {t['reg_eps'].median():.4e} (sd "
                f"{t['reg_eps'].std():.1e}), lag at {t['reg_nu_mid'].median():.0f} cm-1 "
                f"median {t['reg_lag_mid'].median():+.4f} (sd {t['reg_lag_mid'].std():.4f}), "
                f"s0 median {t['reg_s0'].median():+.4f} +- {t['reg_s0_err'].median():.4f} "
                f"cm-1 (through the origin: eps "
                f"{t['reg_eps_origin'].median():.4e}); lag rms "
                f"{t['reg_rms'].median():.4f} cm-1; sub-bands "
                f"{t['reg_n_used'].median():.0f}/{t['reg_n_bands'].median():.0f}")
        if "env_peak" in t and t["env_peak"].notna().any():
            line = (f"envelope        top-hull peak median {t['env_peak'].median():.4g} at "
                    f"{t['env_peak_wn'].median():.1f} cm-1 (sd {t['env_peak_wn'].std():.2f})")
            if "env_scale" in t and t["env_scale"].notna().any():
                line += (f"; scale f median {t['env_scale'].median():.3f} "
                         f"[{t['env_scale'].quantile(.05):.3f}, "
                         f"{t['env_scale'].quantile(.95):.3f}] against the median of "
                         f"the first {self.reference_spectra} spectra")
            if "env_bt_opaque" in t and t["env_bt_opaque"].notna().any():
                bt = t["env_bt_opaque"]
                line += (f"; opaque-band brightness temperature median {bt.median():.2f} K "
                         f"[{bt.quantile(.05):.2f}, {bt.quantile(.95):.2f}]")
            lines.append(line)
        lines += [f"                {n}" for n in self.notes]
        return lines


def _coarse_grid(spectra) -> np.ndarray | None:
    """The common coarse grid; ``None`` when the spectra have no wavenumber axis."""
    if not spectra or not all(hasattr(sp, "wn") for sp in spectra):
        return None
    lo = max(float(np.min(sp.wn)) for sp in spectra)
    hi = min(float(np.max(sp.wn)) for sp in spectra)
    return np.arange(np.ceil(lo), hi, COARSE_STEP)


def _cell_means(wn: np.ndarray, y: np.ndarray, grid: np.ndarray) -> np.ndarray:
    """Mean of ``y`` in each cell of width COARSE_STEP centred on ``grid``."""
    idx = np.floor((wn - (grid[0] - COARSE_STEP / 2)) / COARSE_STEP).astype(int)
    ok = (idx >= 0) & (idx < grid.size) & np.isfinite(y)
    s = np.bincount(idx[ok], weights=y[ok], minlength=grid.size)
    n = np.bincount(idx[ok], minlength=grid.size)
    with np.errstate(invalid="ignore", divide="ignore"):
        return np.where(n > 0, s / np.maximum(n, 1), np.nan)


def _padded(rows: list, width: int, dtype=float) -> np.ndarray:
    out = np.full((len(rows), width), np.nan if dtype is float else False, dtype=dtype)
    for i, r in enumerate(rows):
        if r is not None:
            out[i, :len(r)] = r
    return out


def _ids(spectra) -> list[str]:
    from plumeIR.retrieval.run import _spectrum_id
    return [_spectrum_id(s) for s in spectra]


def _layer_template(setup, cache_dir):
    spec = template_spec(setup.geometry, setup.windows, setup.params, setup.model,
                         instrument_range(setup.raw, RANGE_MARGIN))
    return build_template(spec, setup.retrieval, cache_dir)


def _self_background(data, preprocess):
    from plumeIR.preprocess.layer import build_self_background, find_self_spectra
    paths = find_self_spectra(data.data_dir, self_file=preprocess.self_file,
                              filter_ext=data.filter_ext)
    return build_self_background(paths) if paths else None


def _background(setup, template, spectra, notes, grid=None):
    """Measure the background of each raw beam matching ``spectra``."""
    raw_by_name = {s.filename: s for s in setup.raw}
    bkg = _self_background(setup.data, setup.preprocess)
    if bkg is None:
        notes.append("background: no self spectrum found; zero level only")
    wn0 = setup.raw[0].wn
    bands = [(lo + BAND_MARGIN, hi - BAND_MARGIN) for lo, hi in template.opaque_regions(
        wn0[(wn0 >= template.wn[0]) & (wn0 <= template.wn[-1])])
        if hi - lo > 2 * BAND_MARGIN]
    rows, applied, raw_c, bkg_c = {}, [], [], []
    for sid, sp in zip(_ids(spectra), spectra):
        raw = raw_by_name.get(sp.filename)
        if raw is None:
            raise PlumeIRError(f"background: no raw spectrum for {sp.filename}")
        S = bkg.at(raw.timestamp, x=raw.x) if bkg is not None else None
        res = measure_background(raw.wn, raw.y, bands, S)
        rows[sid] = res.row()
        B = res.background(raw.wn, S)
        applied.append(replace(raw, y=raw.y - B))
        if grid is not None:
            raw_c.append(_cell_means(raw.wn, raw.y, grid))
            bkg_c.append(_cell_means(raw.wn, B, grid))
    arrays = ({"raw": np.asarray(raw_c, np.float32), "background": np.asarray(bkg_c, np.float32)}
              if grid is not None else {})
    return rows, applied, tuple(bands), arrays


def _registration(setup, template, spectra):
    """Per-spectrum registration of the spectra the windows will fit."""
    excluded: tuple = ()
    if isinstance(setup.geometry, LayerGeometry):
        reference = (template.wn, template.total)

        def measure(sp):
            return fit_registration(layer_subbands(sp.wn, sp.y, template), LAYER_MIN_PEAK)
    else:
        ref = max(setup.references.values(), key=lambda r: r.wn[-1] - r.wn[0])
        if ref.L_clear is None:
            raise PlumeIRError("registration: the emission reference has no clear-sky "
                               "radiance to correlate with")
        r = setup.retrieval
        nper = 1.0 / float(np.mean(np.diff(ref.wn)))
        L = convolve_blockwise(ref.wn, ref.L_clear, r.fov, r.max_opd, r.apod_type, nper)
        od_p = sum((lut.query(ref.plume_temp) for lut in ref.gas_luts.values()),
                   np.zeros_like(ref.wn))
        excluded = tuple(plume_bands(ref.wn, od_p))
        reference = (ref.wn, L)

        def measure(sp):
            return fit_registration(emission_subbands(sp.wn, sp.y, ref.wn, L, excluded),
                                    EMISSION_MIN_PEAK)
    results = {}
    for sid, sp in zip(_ids(spectra), spectra):
        try:
            results[sid] = measure(sp)
        except PlumeIRError as exc:
            logger.warning("registration of %s failed: %s", sid, exc)
            results[sid] = None
    return results, excluded, reference


def _envelopes(setup, template, spectra, notes, grid=None):
    """Hulls and peak of each spectrum; layer: the relative scale, emission: the air BT."""
    ids = _ids(spectra)
    envs = [envelope(sp.wn, sp.y) for sp in spectra]
    rows = {sid: e.row() for sid, e in zip(ids, envs)}
    relative = None
    arrays: dict = {}
    if grid is not None:
        arrays["top"] = np.asarray([np.interp(grid, e.wn, e.top) for e in envs], np.float32)
        arrays["bottom"] = np.asarray([np.interp(grid, e.wn, e.bottom) for e in envs],
                                      np.float32)
    if isinstance(setup.geometry, LayerGeometry):
        grid0 = envs[0].wn
        ref = reference_hull(envs, int(setup.preprocess.reference_spectra))
        if grid is not None:
            arrays["reference"] = np.interp(grid, grid0, ref).astype(np.float32)
        mask = None
        if template is not None:
            plume = plume_bands(template.wn, template.od_plume, RELATIVE_PLUME_FRACTION)
            mask = ~np.any([(grid0 >= lo - RELATIVE_PLUME_MARGIN) & (grid0 <= hi + RELATIVE_PLUME_MARGIN)
                            for lo, hi in plume], axis=0) if plume else None
            arrays["relative_mask_bands"] = np.asarray(
                [(lo - RELATIVE_PLUME_MARGIN, hi + RELATIVE_PLUME_MARGIN) for lo, hi in plume],
                float).reshape(-1, 2)
        else:
            notes.append("relative scale: no template, plume bands not masked")
        t_ref = float(setup.geometry.source_temp)
        arrays["t_ref"] = np.asarray(t_ref)
        rel = {}
        for sid, e in zip(ids, envs):
            r = relative_planck(grid0, np.interp(grid0, e.wn, e.top), ref, t_ref, mask)
            rows[sid]["env_scale"] = r.f
            rel[sid] = {"f": r.f, "dT": r.dT, "rms": r.rms, "n": r.n}
        relative = pd.DataFrame.from_dict(rel, orient="index")
        relative.index.name = COL_SPECTRUM
    elif isinstance(setup.geometry, EmissionGeometry):
        for sid, sp in zip(ids, spectra):
            rows[sid]["env_bt_opaque"] = opaque_brightness_temperature(sp.wn, sp.y)
    return rows, relative, arrays


def _relabel(spectrum, s0: float, eps: float):
    """The spectrum with its axis corrected, ν' = ν − (s0 + ε·ν)."""
    if spectrum.x_unit != "wn":
        raise PlumeIRError(f"registration needs a wavenumber axis, got {spectrum.x_unit!r}")
    return replace(spectrum, x=spectrum.x - (s0 + eps * spectrum.x))


def preprocess_products(setup, cache_dir=None) -> tuple[list, PreprocessReport]:
    """
    Run the stage for a prepared run.

    Parameters
    ----------
    setup : object
        With ``geometry``, ``data``, ``model``, ``retrieval`` (resolved),
        ``windows``, ``params``, ``references``, ``preprocess``, ``raw`` (the
        loaded spectra) and ``spectra`` (preprocessed).
    cache_dir : Path or None
        Template cache; ``None`` uses the model's / default cache directory.

    Returns
    -------
    spectra : list of Spectrum
        What the windows fit: ``setup.spectra`` unchanged unless a product
        is applied.
    PreprocessReport
    """
    from plumeIR.reference.reference import _resolve_cache_dir
    pre = setup.preprocess
    geometry = setup.geometry
    is_layer = isinstance(geometry, LayerGeometry)
    bkg_mode = pre.background if is_layer else "off"
    reg_mode = (pre.registration if isinstance(geometry, (LayerGeometry, EmissionGeometry))
                else "off")
    spectra = list(setup.spectra)
    table = pd.DataFrame({COL_SPECTRUM: _ids(spectra)}).set_index(COL_SPECTRUM)
    notes: list[str] = []
    bands, excluded = (), ()
    arrays: dict = {}
    grid = _coarse_grid(spectra)
    if bkg_mode == "off" and reg_mode == "off":
        return spectra, PreprocessReport("off", "off", table.reset_index())

    template = None
    if is_layer:
        try:
            template = _layer_template(setup, _resolve_cache_dir(cache_dir, setup.model))
        except Exception as exc:        # noqa: BLE001 — reported, or re-raised in apply
            if "apply" in (bkg_mode, reg_mode):
                raise
            notes.append(f"template not built ({exc}); products skipped")
            logger.warning("Preprocessing template not built: %s", exc)
            bkg_mode, reg_mode = "off", "off"

    if bkg_mode != "off":
        try:
            rows, applied, bands, bkg_arrays = _background(setup, template, spectra, notes,
                                                           grid)
            arrays.update(bkg_arrays)
            table = table.join(pd.DataFrame.from_dict(rows, orient="index"))
            if bkg_mode == "apply":
                spectra = applied
                logger.info("Background applied to %d spectra (self scale and "
                            "zero level, unclipped)", len(spectra))
        except PlumeIRError as exc:
            if bkg_mode == "apply":
                raise
            notes.append(f"background not measured: {exc}")
            logger.warning("Background not measured: %s", exc)

    if reg_mode != "off":
        try:
            results, excluded, reg_ref = _registration(setup, template, spectra)
            failed = [sid for sid, r in results.items() if r is None]
            if len(failed) == len(results):
                raise PlumeIRError("no spectrum could be registered")
            if failed:
                notes.append(f"registration failed for {len(failed)} spectra "
                             f"(axis left unchanged)")
            reg = pd.DataFrame.from_dict(
                {sid: (r.row() if r is not None else {}) for sid, r in results.items()},
                orient="index").reindex(table.index).astype(float)
            table = table.join(reg)
            k_max = max(r.subbands.centres.size for r in results.values() if r is not None)
            ordered = [results[sid] for sid in table.index]
            arrays.update(
                reg_centres=_padded([r.subbands.centres if r else None for r in ordered], k_max),
                reg_lags=_padded([r.subbands.lags if r else None for r in ordered], k_max),
                reg_peaks=_padded([r.subbands.peaks if r else None for r in ordered], k_max),
                reg_used=_padded([r.used if r else None for r in ordered], k_max, bool),
                template_wn=np.asarray(reg_ref[0], float),
                template=np.asarray(reg_ref[1], np.float32))
            if reg_mode == "apply":
                spectra = [sp if not np.isfinite(e) else _relabel(sp, s, e) for sp, s, e in
                           zip(spectra, table["reg_s0"], table["reg_eps"])]
                logger.info("Registration applied per spectrum (%d of %d)",
                            len(results) - len(failed), len(results))
        except PlumeIRError as exc:
            if reg_mode == "apply":
                raise
            notes.append(f"registration not measured: {exc}")
            logger.warning("Registration not measured: %s", exc)

    relative = None
    try:
        env_rows, relative, env_arrays = _envelopes(setup, template, spectra, notes, grid)
        arrays.update(env_arrays)
        table = table.join(pd.DataFrame.from_dict(env_rows, orient="index"))
    except Exception as exc:            # noqa: BLE001 — reported only, never stops a run
        notes.append(f"envelope metrics not measured: {exc}")
        logger.warning("Envelope metrics not measured: %s", exc)

    char = PreprocessReport(background=bkg_mode, registration=reg_mode,
                            table=table.reset_index(), relative=relative,
                            reference_spectra=int(pre.reference_spectra),
                            arrays=arrays if grid is None else {"wn_coarse": grid, **arrays},
                            bands=tuple(bands), excluded=excluded, notes=tuple(notes))
    for line in char.summary_lines():
        logger.info(line)
    return spectra, char
