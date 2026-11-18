"""
Plots of the whole-spectrum preprocessing (``preprocess.npz`` / ``.csv``).

Drawing functions take an ``Axes`` and a
:class:`~plumeIR.preprocess.archive.PreprocessArchive` and draw into it; they
never create figures or call ``pyplot``.  :func:`save_preprocess_figures`
builds the standard set with ``matplotlib.figure.Figure`` for
``--save-plots``; a GUI calls the drawing functions into its own panels (the
full-spectrum panel above the windows is :func:`draw_spectrum`).

Per spectrum (spectral): :func:`draw_spectrum` (spectrum, hulls, peak, opaque
and plume bands, FITn windows), :func:`draw_registration` (lag per sub-band
and the fitted line), :func:`draw_background` (raw beam and background in one
opaque band, layer), :func:`draw_relative` (top hull against the reference,
layer).  Over the series (analysis): :func:`draw_series` and
:func:`draw_scatter`.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING

import numpy as np
import pandas as pd
from matplotlib.figure import Figure

from plumeIR._style import ANNOTATION_SIZE, discrete_colors, lighten, style_context
from plumeIR.physics import rad
from plumeIR.plotting.spectrum import _RAD_UNIT

if TYPE_CHECKING:
    from matplotlib.axes import Axes

    from plumeIR.preprocess.archive import PreprocessArchive

logger = logging.getLogger(__name__)

_WN_LABEL = "Wavenumber [cm$^{-1}$]"

#: Column of ``preprocess.csv`` → axis label, for series and scatter plots.
LABELS: dict[str, str] = {
    "bkg_a": "Self-emission scale a",
    "bkg_heldout_rms": "Background held-out rms [counts]",
    "reg_eps": "Wavenumber scale ε",
    "reg_lag_mid": "Lag at mid-range [cm$^{-1}$]",
    "reg_s0": "Shift s0 [cm$^{-1}$]",
    "reg_rms": "Lag rms [cm$^{-1}$]",
    "env_peak": "Top-hull peak",
    "env_peak_wn": "Peak position [cm$^{-1}$]",
    "env_integral": "Integrated top hull",
    "env_scale": "Relative scale f",
    "env_bt_opaque": "Opaque-band air temperature [K]",
    "dT": "Relative temperature ΔT [K]",
}


def _corr(x, y) -> float:
    """Pearson r over the finite pairs; NaN when either side is constant."""
    x, y = np.asarray(x, float), np.asarray(y, float)
    ok = np.isfinite(x) & np.isfinite(y)
    if ok.sum() < 3 or np.std(x[ok]) == 0 or np.std(y[ok]) == 0:
        return np.nan
    return float(np.corrcoef(x[ok], y[ok])[0, 1])


def _y_label(arc: "PreprocessArchive") -> str:
    return "Single beam [counts]" if arc.geometry == "layer" else f"Radiance {_RAD_UNIT}"


def times(arc: "PreprocessArchive") -> tuple[np.ndarray, str]:
    """Minutes from the first spectrum, or the spectrum index when not timestamps."""
    try:
        t = pd.to_datetime(pd.Index(arc.spectrum_ids))
        return ((t - t.min()).total_seconds() / 60).to_numpy(), "Minutes from the first spectrum"
    except (ValueError, TypeError):
        return np.arange(len(arc), dtype=float), "Spectrum"


def series_value(arc: "PreprocessArchive", column: str) -> pd.Series | None:
    """A ``preprocess.csv`` column, or ``dT`` from the archive; ``None`` if absent."""
    if column == "dT":
        rel = arc.relative
        return None if rel is None else rel["dT"]
    if column in arc.table and arc.table[column].notna().any():
        return arc.table[column]
    return None


# =============================================================================
# Per spectrum
# =============================================================================

def draw_spectrum(ax: "Axes", arc: "PreprocessArchive", k: int, *, hulls: bool = True,
                  bands: bool = True, windows: bool = True) -> None:
    """
    One spectrum as fitted, with its hulls, peak, bands and the FITn windows.

    Grey spans: opaque bands (background). Light spans: plume bands left out
    of the relative scale (layer) or of the registration (emission).  Windows
    are marked along the top with their labels.
    """
    c = discrete_colors(4)
    wn, y = arc.spectrum(k)
    if bands:
        for lo, hi in arc.bands("opaque_bands"):
            ax.axvspan(lo, hi, color="0.88", lw=0, zorder=0)
        for lo, hi in arc.bands("relative_mask_bands") + arc.bands("excluded"):
            ax.axvspan(lo, hi, color=lighten(c[3], 0.93), lw=0, zorder=0)
    if windows:
        for label, (lo, hi) in arc.windows.items():
            ax.axvspan(lo, hi, ymin=0.96, ymax=1.0, color=c[2], lw=0)
            ax.text(0.5 * (lo + hi), 1.0, label, ha="center", va="bottom",
                    fontsize=ANNOTATION_SIZE, transform=ax.get_xaxis_transform())
    ax.plot(wn, y, lw=0.4, color="0.45", label="spectrum (as fitted)")
    wc = arc.get("wn_coarse")
    if hulls and wc is not None:
        ax.plot(wc, arc.coarse("top", k), lw=1.2, color=c[0], label="top hull")
        ax.plot(wc, arc.coarse("bottom", k), lw=1.2, color=c[1], ls="--", label="bottom hull")
        row = arc.table.iloc[k]
        if np.isfinite(row.get("env_peak", np.nan)):
            ax.plot(row["env_peak_wn"], row["env_peak"], "v", color=c[0], ms=6,
                    label=f"peak {row['env_peak']:.3g} at {row['env_peak_wn']:.1f} cm$^{{-1}}$")
    ax.set_xlabel(_WN_LABEL)
    ax.set_ylabel(_y_label(arc))


def draw_registration(ax: "Axes", arc: "PreprocessArchive", k: int, color=None) -> None:
    """Lag per sub-band against ν (×: not used) and the fitted s0 + ε·ν."""
    sb = arc.subbands(k)
    if sb is None:
        ax.text(0.5, 0.5, "no registration", ha="center", transform=ax.transAxes)
        return
    centres, lags, _, used = sb
    color = color if color is not None else discrete_colors(1)[0]
    row = arc.table.iloc[k]
    ax.plot(centres[used], lags[used], "o", ms=4, color=color,
            label=f"{arc.spectrum_ids[k]}: ε {row['reg_eps']:.3e}, lag "
                  f"{row['reg_lag_mid']:+.3f} at {row['reg_nu_mid']:.0f} cm$^{{-1}}$")
    ax.plot(centres[~used], lags[~used], "x", ms=5, color=color)
    if np.isfinite(row["reg_eps"]) and centres.size:
        x = np.array([centres.min(), centres.max()])
        ax.plot(x, row["reg_s0"] + row["reg_eps"] * x, lw=1, color=color)
    ax.set_xlabel("Sub-band centre [cm$^{-1}$]")
    ax.set_ylabel("Lag [cm$^{-1}$]")


def draw_background(ax: "Axes", arc: "PreprocessArchive", k: int,
                    band: tuple[float, float], margin: float = 30.0) -> None:
    """Raw beam (2 cm⁻¹ cell means) and the measured background around one opaque band."""
    wc, raw, bkg = arc.get("wn_coarse"), arc.coarse("raw", k), arc.coarse("background", k)
    if wc is None or raw is None:
        ax.text(0.5, 0.5, "no background", ha="center", transform=ax.transAxes)
        return
    c = discrete_colors(2)
    lo, hi = band
    sel = (wc >= lo - margin) & (wc <= hi + margin)
    inside = (wc >= lo) & (wc <= hi)
    ax.axvspan(lo, hi, color="0.9", lw=0, zorder=0)
    ax.plot(wc[sel], raw[sel], ".", ms=3, color="0.35", label="raw beam")
    ax.plot(wc[sel], bkg[sel], lw=1.2, color=c[0], label="background a·S + z")
    vals = np.concatenate([raw[inside], bkg[inside]])
    vals = vals[np.isfinite(vals)]
    if vals.size:
        pad = 0.5 * (vals.max() - vals.min()) + 1.0
        ax.set_ylim(vals.min() - pad, vals.max() + pad)
    ax.axhline(0.0, color="0.6", lw=0.6)
    ax.set_xlim(lo - margin, hi + margin)
    ax.set_title(f"{lo:.0f}-{hi:.0f} cm$^{{-1}}$", fontsize=9)
    ax.set_xlabel(_WN_LABEL)


def draw_relative(ax: "Axes", arc: "PreprocessArchive", ks) -> None:
    """ln(top / reference) for spectra ``ks``, with the fitted ln f + ln B(T)/B(T_ref)."""
    wc, ref, rel = arc.get("wn_coarse"), arc.get("reference"), arc.relative
    if wc is None or ref is None or rel is None:
        ax.text(0.5, 0.5, "no relative scale", ha="center", transform=ax.transAxes)
        return
    t_ref = float(arc.get("t_ref")) if arc.get("t_ref") is not None else np.nan
    mask = np.ones(wc.size, bool)
    for lo, hi in arc.bands("relative_mask_bands"):
        mask &= ~((wc >= lo) & (wc <= hi))
    c = discrete_colors(max(len(ks), 1))
    for i, k in enumerate(ks):
        top = arc.coarse("top", k)
        use = (top > 0.05 * np.nanmax(top)) & (ref > 0.05 * np.nanmax(ref))
        r = np.full(wc.size, np.nan)
        r[use] = np.log(top[use] / ref[use])
        f, dT = rel["f"].iloc[k], rel["dT"].iloc[k]
        ax.plot(wc, np.where(mask, r, np.nan), lw=0.8, color=c[i],
                label=f"{arc.spectrum_ids[k]}: f {f:.3f}, ΔT {dT:+.0f} K")
        ax.plot(wc, np.where(~mask, r, np.nan), lw=0.8, color=lighten(c[i], 0.7))
        if np.isfinite(t_ref) and np.isfinite(dT):
            model = np.log(f) + np.log(rad(wc, t_ref + dT) / rad(wc, t_ref))
            ax.plot(wc[use], model[use], lw=1.2, ls="--", color=c[i])
    ax.set_xlabel(_WN_LABEL)
    ax.set_ylabel("ln(top hull / reference)")


# =============================================================================
# Over the series
# =============================================================================

def draw_series(ax: "Axes", arc: "PreprocessArchive", column: str,
                against: pd.Series | None = None, against_name: str = "") -> None:
    """One metric against time; with ``against``, its correlation is annotated."""
    s = series_value(arc, column)
    t, _ = times(arc)
    if s is None:
        ax.text(0.5, 0.5, f"{column}: not measured", ha="center", transform=ax.transAxes)
        return
    ax.plot(t, s.to_numpy(float), lw=0.7, color=discrete_colors(1)[0])
    if column == "bkg_a" and "bkg_flags" in arc.table:
        flagged = (arc.table["bkg_flags"].fillna("") != "").to_numpy()
        ax.plot(t[flagged], s.to_numpy(float)[flagged], "x", ms=4,
                color=discrete_colors(2)[1], label="flagged")
        if flagged.any():
            ax.legend(fontsize=ANNOTATION_SIZE)
    r = _corr(s.to_numpy(float), against) if against is not None else np.nan
    if np.isfinite(r):
        ax.text(0.99, 0.88, f"r with {against_name} {r:+.2f}", transform=ax.transAxes,
                ha="right", fontsize=ANNOTATION_SIZE)
    ax.set_ylabel(LABELS.get(column, column), fontsize=8)


def draw_scatter(ax: "Axes", x: pd.Series, y: pd.Series, x_label: str, y_label: str) -> None:
    """``y`` against ``x`` with their correlation."""
    xv, yv = np.asarray(x, float), np.asarray(y, float)
    ok = np.isfinite(xv) & np.isfinite(yv)
    ax.plot(xv[ok], yv[ok], "o", ms=3, alpha=0.6, color=discrete_colors(1)[0])
    r = _corr(xv, yv)
    if np.isfinite(r):
        ax.text(0.02, 0.92, f"r {r:+.2f}  (n {ok.sum()})",
                transform=ax.transAxes, fontsize=ANNOTATION_SIZE)
    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)


def default_series(arc: "PreprocessArchive") -> list[str]:
    """The series worth showing for this run, in display order."""
    cols = ["bkg_a", "reg_eps", "reg_lag_mid", "env_peak", "env_scale", "dT", "env_bt_opaque"]
    return [c for c in cols if series_value(arc, c) is not None]


# =============================================================================
# The figure set (--save-plots)
# =============================================================================

def save_preprocess_figures(arc: "PreprocessArchive", out_dir: Path,
                            targets: pd.DataFrame | None = None,
                            primary: str | None = None) -> list[Path]:
    """
    Write the preprocessing figures.

    Parameters
    ----------
    arc : PreprocessArchive
    out_dir : Path
        Created if missing.
    targets : pandas.DataFrame or None
        The run's target columns per spectrum (``ResultsTable.wide()``,
        indexed by spectrum id); adds correlations and the scatter figure.
    primary : str or None
        The target to correlate with (the first window's first target);
        ``None`` or absent from ``targets`` uses its first column.

    Returns
    -------
    list of Path
        ``preprocess_spectra.png``, ``preprocess_registration.png``,
        ``preprocess_series.png``; layer ``preprocess_background.png`` and
        ``preprocess_relative.png``; with targets ``preprocess_scatter.png``.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    written: list[Path] = []
    n = len(arc)
    picks = sorted({0, n - 1})
    first = None
    if targets is not None and len(targets.columns):
        targets = targets.reindex(arc.spectrum_ids)
        first = primary if primary in targets.columns else targets.columns[0]

    def save(fig: Figure, name: str) -> None:
        written.append(out_dir / name)
        fig.savefig(written[-1])

    with style_context():
        fig = Figure(figsize=(10.0, 3.6 * len(picks)), layout="constrained")
        axes = fig.subplots(len(picks), 1, sharex=True, squeeze=False)[:, 0]
        for ax, k in zip(axes, picks):
            draw_spectrum(ax, arc, k)
            ax.set_title(arc.spectrum_ids[k], fontsize=9, loc="left")
        axes[0].legend(loc="upper right", fontsize=ANNOTATION_SIZE)
        fig.suptitle("Spectra with hulls; grey: opaque bands; shaded: plume bands left out")
        save(fig, "preprocess_spectra.png")

        if arc.get("reg_centres") is not None:
            fig = Figure(figsize=(8.0, 4.4), layout="constrained")
            ax = fig.add_subplot()
            for i, k in enumerate(picks):
                draw_registration(ax, arc, k, color=discrete_colors(len(picks))[i])
            ax.legend(fontsize=ANNOTATION_SIZE)
            ax.set_title("Registration: lag per sub-band (×: not used)", fontsize=10)
            save(fig, "preprocess_registration.png")

        bands = arc.bands("opaque_bands")
        if arc.get("raw") is not None and bands:
            fig = Figure(figsize=(3.0 * len(bands), 3.2), layout="constrained")
            axes = fig.subplots(1, len(bands), squeeze=False)[0]
            for ax, band in zip(axes, bands):
                draw_background(ax, arc, 0, band)
            axes[0].set_ylabel("Single beam [counts]")
            axes[0].legend(fontsize=ANNOTATION_SIZE)
            fig.suptitle(f"Background in the opaque bands: {arc.spectrum_ids[0]}")
            save(fig, "preprocess_background.png")

        rel = arc.relative
        if rel is not None and rel["dT"].notna().sum() >= 3:
            order = np.argsort(rel["dT"].to_numpy())
            ks = [int(order[int(q * (len(order) - 1))]) for q in (0.05, 0.5, 0.95)]
            fig = Figure(figsize=(10.0, 4.2), layout="constrained")
            ax = fig.add_subplot()
            draw_relative(ax, arc, ks)
            ax.legend(fontsize=ANNOTATION_SIZE)
            ax.set_title("Top hull against the reference (dashed: fitted ln f + "
                         "ln B(T)/B(T$_{ref}$); faint: plume bands, not fitted)", fontsize=10)
            save(fig, "preprocess_relative.png")

        cols = default_series(arc)
        if cols:
            against = targets[first] if first is not None else None
            rows = cols + ([first] if first is not None else [])
            fig = Figure(figsize=(10.0, 1.6 * len(rows) + 0.8), layout="constrained")
            axes = fig.subplots(len(rows), 1, sharex=True, squeeze=False)[:, 0]
            for ax, col in zip(axes, cols):
                draw_series(ax, arc, col, against, first or "")
            if first is not None:
                t, _ = times(arc)
                axes[-1].plot(t, against.to_numpy(float), lw=0.7, color="0.3")
                axes[-1].set_ylabel(f"{first} (fitted)", fontsize=8)
            axes[-1].set_xlabel(times(arc)[1])
            fig.suptitle("Preprocessing over the series")
            save(fig, "preprocess_series.png")

        if first is not None:
            pairs = [(c, first) for c in ("env_scale", "reg_lag_mid", "dT", "env_bt_opaque")
                     if series_value(arc, c) is not None]
            if "bkg_a" in arc.table and series_value(arc, "env_peak") is not None:
                pairs.append(("bkg_a", "env_peak"))
            if pairs:
                ncol = min(2, len(pairs))
                nrow = int(np.ceil(len(pairs) / ncol))
                fig = Figure(figsize=(3.4 * ncol + 0.6, 3.0 * nrow), layout="constrained")
                axes = fig.subplots(nrow, ncol, squeeze=False).ravel()
                for ax, (ycol, xcol) in zip(axes, pairs):
                    x = targets[xcol] if xcol == first else series_value(arc, xcol)
                    draw_scatter(ax, x, series_value(arc, ycol),
                                 f"{xcol} (fitted)" if xcol == first else LABELS.get(xcol, xcol),
                                 LABELS.get(ycol, ycol))
                for ax in axes[len(pairs):]:
                    ax.set_visible(False)
                fig.suptitle("Preprocessing against the fits")
                save(fig, "preprocess_scatter.png")
    return written
