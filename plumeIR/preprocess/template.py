"""
Synthetic template spectrum for the whole-spectrum preprocessing (layer geometry).

One per run, over the instrument's whole range: the ambient path and a
representative plume, from direct RFM runs, convolved with the instrument line
shape.  It serves both preprocessing products:

- **opaque bands** (background product): where the ambient path alone is
  black, the measured signal is the additive background.  Found from physics,
  never from fixed limits, because they move with path length and humidity.
- **cross-correlation reference** (registration product): line positions in
  sub-bands across the range.

Neither needs accurate columns: a band is opaque or not, and a line is where
it is.  So the template uses the configured a priori — ambient gases at their
reference columns over ``pathlength``, plume gases at their reference columns
over ``plume_thickness`` — and nothing fitted.  Plume aerosols are left out:
they are smooth and carry no line position.

The RFM part (optical depths) is cached by content, keyed on everything sent
to RFM; the ILS is applied on load, since it is cheap and depends on the
resolved instrument.  Convolution follows convention A (the FOV only
broadens; ``plumeIR.forward.ils``), in blocks of ``ILS_BLOCK`` cm⁻¹ each
with its own kernel, because the FOV broadening grows with ν.
"""

from __future__ import annotations

import hashlib
import json
import logging
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

from plumeIR.config.objects import LayerGeometry
from plumeIR.exceptions import ReferenceComputationError
from plumeIR.forward.base import apply_ils
from plumeIR.forward.ils import make_ils
from plumeIR.units import scd_to_vmr

logger = logging.getLogger(__name__)

#: Bumped when the cached file's layout or the RFM request changes meaning.
_TEMPLATE_SCHEMA_VERSION = 1

#: Width [cm⁻¹] of the blocks convolved with one ILS kernel.  The FOV width
#: at the block's edge differs from its centre by ±ILS_BLOCK/(2ν): ±3% at
#: 4000 cm⁻¹, negligible for line positions and opaque masks.
ILS_BLOCK = 250.0

#: Transmittance below which a point is opaque (prototype value: the CO2 ν2,
#: H2O ν2, CO2 ν3 and H2O stretch bands over 2 km).
OPAQUE_THRESHOLD = 1e-3

#: Narrowest opaque region kept [cm⁻¹]: the four main bands are 35-490 cm⁻¹
#: wide; their edge fragments carry no new information (prototype: 100 MIDAC
#: points, 24 cm⁻¹).
MIN_OPAQUE_WIDTH = 24.0


@dataclass(frozen=True)
class TemplateSpec:
    """
    Everything sent to RFM for one template; its hash is the cache key.

    Attributes
    ----------
    wn_start, wn_stop : float
        Range of the RFM runs [cm⁻¹].
    nperwn : int
        Model grid points per cm⁻¹.
    ambient_vmr : tuple of (str, float)
        Ambient gases and their VMR [ppmv], sorted by name.
    pathlength : float
        Ambient path [km].
    atm_temp, atm_pres : float
        Ambient temperature [K] and pressure [mbar].
    plume_scd : tuple of (str, float)
        Plume gases and their slant column [molec/cm²], sorted by name.
    plume_thickness : float
        Plume path [km].
    plume_temp, plume_pres : float
        Plume temperature [K] and pressure [mbar].
    """
    wn_start:        float
    wn_stop:         float
    nperwn:          int
    ambient_vmr:     tuple
    pathlength:      float
    atm_temp:        float
    atm_pres:        float
    plume_scd:       tuple = ()
    plume_thickness: float = 0.05
    plume_temp:      float = 450.0
    plume_pres:      float = 1013.0

    def key(self) -> str:
        """Content hash of the spec (hex), including the schema version."""
        payload = json.dumps(
            {"schema": _TEMPLATE_SCHEMA_VERSION,
             **{k: (list(map(list, v)) if isinstance(v, tuple) else v)
                for k, v in self.__dict__.items()}},
            sort_keys=True,
        )
        return hashlib.sha256(payload.encode()).hexdigest()


@dataclass(frozen=True)
class Template:
    """
    A synthetic spectrum on the model grid, ILS-convolved.

    Attributes
    ----------
    spec : TemplateSpec
        What was sent to RFM.
    wn : np.ndarray
        Model grid [cm⁻¹], shape ``(n,)``.
    od_ambient, od_plume : np.ndarray
        Monochromatic optical depths, shape ``(n,)``.
    ambient : np.ndarray
        ILS-convolved transmittance of the ambient path alone, shape ``(n,)``.
    total : np.ndarray
        ILS-convolved transmittance of ambient path and plume, shape ``(n,)``.
    ils : dict
        The ILS settings used (``fov``, ``max_opd``, ``apod_type``).
    """
    spec:       TemplateSpec
    wn:         np.ndarray = field(repr=False)
    od_ambient: np.ndarray = field(repr=False)
    od_plume:   np.ndarray = field(repr=False)
    ambient:    np.ndarray = field(repr=False)
    total:      np.ndarray = field(repr=False)
    ils:        dict = field(default_factory=dict)

    def on_grid(self, wn: np.ndarray, which: str = "total") -> np.ndarray:
        """
        The convolved transmittance interpolated onto ``wn``.

        Parameters
        ----------
        wn : np.ndarray
            Target grid (e.g. a measurement's), inside the template's range.
        which : {'total', 'ambient'}
            Ambient path and plume, or ambient path alone.

        Raises
        ------
        ValueError
            If ``wn`` reaches outside the template, or ``which`` is unknown.
        """
        if which not in ("total", "ambient"):
            raise ValueError(f"which must be 'total' or 'ambient', got {which!r}")
        wn = np.asarray(wn, dtype=float)
        if wn.min() < self.wn[0] or wn.max() > self.wn[-1]:
            raise ValueError(
                f"grid {wn.min():.2f}-{wn.max():.2f} cm⁻¹ reaches outside the "
                f"template's {self.wn[0]:.2f}-{self.wn[-1]:.2f} cm⁻¹"
            )
        return np.interp(wn, self.wn, getattr(self, which))

    def opaque_regions(
        self,
        wn: np.ndarray | None = None,
        threshold: float = OPAQUE_THRESHOLD,
        min_width: float = MIN_OPAQUE_WIDTH,
    ) -> list[tuple[float, float]]:
        """
        Regions where the ambient path alone is black.

        Parameters
        ----------
        wn : np.ndarray or None
            Grid to evaluate on (a measurement's); ``None`` uses the model
            grid.  Edges are those of the grid points inside the region.
        threshold : float
            Convolved ambient transmittance below which a point is opaque.
        min_width : float
            Narrowest region kept [cm⁻¹].

        Returns
        -------
        list of (float, float)
            ``(start, stop)`` of each region [cm⁻¹], ascending.
        """
        grid = self.wn if wn is None else np.asarray(wn, dtype=float)
        t = self.ambient if wn is None else self.on_grid(grid, "ambient")
        opaque = t < threshold
        edges = np.flatnonzero(np.diff(np.r_[0, opaque.astype(np.int8), 0]))
        out = []
        for a, b in zip(edges[::2], edges[1::2]):
            lo, hi = float(grid[a]), float(grid[b - 1])
            if hi - lo >= min_width:
                out.append((lo, hi))
        return out


def template_spec(
    geometry: LayerGeometry,
    windows: dict,
    params: dict,
    model,
    wn_range: tuple[float, float],
) -> TemplateSpec:
    """
    The template's RFM inputs from a run's configuration.

    Ambient gases are the union of every active window's ``atm_gases``, each
    at the VMR its reference column implies over ``pathlength``; plume gases
    the union of ``plume_gases`` at their reference columns.  A gas that more
    than one window configures with different reference columns takes the
    first window's, with a warning (a line position does not depend on it).

    Parameters
    ----------
    geometry : LayerGeometry
    windows : dict
        ``FitWindowConfig`` per window label, in config order.
    params : dict
        ``ParameterSet`` per window label (reference columns).
    model : ModelConfig
        ``nperwn``.
    wn_range : (float, float)
        Range to cover [cm⁻¹], usually the instrument's
        (:func:`instrument_range`).

    Returns
    -------
    TemplateSpec

    Raises
    ------
    NotImplementedError
        For a geometry other than layer.
    ValueError
        For an empty or inverted ``wn_range``.
    """
    if not isinstance(geometry, LayerGeometry):
        raise NotImplementedError(
            "The preprocess_report template is built for the layer geometry; "
            f"got {type(geometry).__name__}."
        )
    lo, hi = float(wn_range[0]), float(wn_range[1])
    if not hi > lo:
        raise ValueError(f"wn_range must be increasing, got {wn_range}")

    ambient: dict[str, float] = {}
    plume: dict[str, float] = {}
    for label, w in windows.items():
        ps = params[label]
        for gas in w.atm_gases:
            scd = ps[w.atm_name(gas)].ref_column
            vmr = float(scd_to_vmr(scd, geometry.pathlength,
                                   temp=geometry.atm_temp, pres=geometry.atm_pres))
            _merge(ambient, gas, vmr, label, "ambient VMR [ppmv]")
        for gas in w.plume_gases:
            _merge(plume, gas, float(ps[gas].ref_column), label,
                   "plume column [molec/cm²]")
    if not ambient:
        raise ReferenceComputationError(
            "No ambient gas in any active window: the template has nothing to "
            "show the opaque bands or line positions."
        )
    return TemplateSpec(
        wn_start=lo, wn_stop=hi, nperwn=int(model.nperwn),
        ambient_vmr=tuple(sorted(ambient.items())),
        pathlength=float(geometry.pathlength),
        atm_temp=float(geometry.atm_temp), atm_pres=float(geometry.atm_pres),
        plume_scd=tuple(sorted(plume.items())),
        plume_thickness=float(geometry.plume_thickness),
        plume_temp=float(geometry.plume_temp), plume_pres=float(geometry.plume_pres),
    )


def _merge(into: dict, gas: str, value: float, label: str, what: str) -> None:
    if gas not in into:
        into[gas] = value
    elif not np.isclose(into[gas], value, rtol=1e-6):
        logger.warning("%s: %s %.4g differs from an earlier window's %.4g; the "
                       "template uses the earlier value", label, gas, value, into[gas])


def instrument_range(spectra, margin: float = 0.0) -> tuple[float, float]:
    """
    The wavenumber range every spectrum covers, shrunk by ``margin`` [cm⁻¹].

    Raises
    ------
    ValueError
        If there are no spectra or the margin leaves nothing.
    """
    if not spectra:
        raise ValueError("no spectra")
    lo = max(float(np.min(s.wn)) for s in spectra) + margin
    hi = min(float(np.max(s.wn)) for s in spectra) - margin
    if not hi > lo:
        raise ValueError(f"no common range left after a {margin} cm⁻¹ margin")
    return lo, hi


def _rfm_od(gases: dict[str, float], spec: TemplateSpec, pathlength: float,
            temp: float, pres: float):
    """One RFM layer run of ``gases`` (VMR [ppmv]); its job object."""
    from rfmwrapper import RFM
    return RFM(type="layer", wn_start=spec.wn_start, wn_stop=spec.wn_stop,
               nperwn=spec.nperwn, gas=" ".join(gases), pathlength=pathlength,
               pres=pres, temp=temp, vmr={g: float(v) for g, v in gases.items()},
               physics={"ctm": True}, out="OPT", silent=True)


def compute_optical_depths(spec: TemplateSpec) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Run RFM for the ambient path and the plume (two jobs, in parallel).

    Returns
    -------
    wn, od_ambient, od_plume : np.ndarray
        ``od_plume`` is zero when the spec has no plume gas.

    Raises
    ------
    ReferenceComputationError
        If the two runs return different grids.
    """
    from rfmwrapper import runBatch
    jobs = [_rfm_od(dict(spec.ambient_vmr), spec, spec.pathlength,
                    spec.atm_temp, spec.atm_pres)]
    if spec.plume_scd:
        vmr_p = {g: float(scd_to_vmr(c, spec.plume_thickness, temp=spec.plume_temp,
                                     pres=spec.plume_pres))
                 for g, c in spec.plume_scd}
        jobs.append(_rfm_od(vmr_p, spec, spec.plume_thickness, spec.plume_temp,
                            spec.plume_pres))
    logger.info("Building the preprocess_report template: %d RFM run(s) over "
                "%.0f-%.0f cm⁻¹", len(jobs), spec.wn_start, spec.wn_stop)
    results = [r.getSpectra("opt") for r in runBatch(jobs, max_workers=len(jobs))]
    wn = np.asarray(results[0].wn, dtype=float)
    od_a = np.asarray(results[0].data[0], dtype=float)
    if len(results) == 1:
        return wn, od_a, np.zeros_like(od_a)
    wn_p = np.asarray(results[1].wn, dtype=float)
    if wn_p.size != wn.size or not np.allclose(wn_p, wn):
        raise ReferenceComputationError(
            "The ambient and plume RFM runs returned different grids "
            f"({wn.size} and {wn_p.size} points)."
        )
    return wn, od_a, np.asarray(results[1].data[0], dtype=float)


def convolve_blockwise(
    wn: np.ndarray,
    spectrum: np.ndarray,
    fov: float,
    max_opd: float,
    apod_type: str,
    nperwn: float,
    block: float = ILS_BLOCK,
) -> np.ndarray:
    """
    Convolve with the ILS, one kernel per ``block`` cm⁻¹ (centred on it).

    Each block is taken from a convolution of the whole spectrum, so block
    edges see their true neighbours.  Convention A: the FOV only broadens.

    Parameters
    ----------
    wn, spectrum : np.ndarray
        Model grid and monochromatic spectrum, shape ``(n,)``.
    fov, max_opd, apod_type : float, float, str
        ILS settings (resolved for the instrument).
    nperwn : float
        Model grid points per cm⁻¹.
    block : float
        Block width [cm⁻¹].

    Returns
    -------
    np.ndarray
        Convolved spectrum, shape ``(n,)``.
    """
    out = np.empty_like(spectrum, dtype=float)
    edges = np.arange(wn[0], wn[-1] + block, block)
    for a, b in zip(edges[:-1], edges[1:]):
        sel = (wn >= a) & (wn < b) if b < edges[-1] else (wn >= a)
        if not sel.any():
            continue
        k = make_ils(fov=fov, max_opd=max_opd, nper_wn=nperwn,
                     wn_center=0.5 * (a + b), apod_type=apod_type,
                     trim_opd=max_opd, trim_fov=fov)
        out[sel] = apply_ils(spectrum, k)[sel]
    return out


def build_template(
    spec: TemplateSpec,
    retrieval,
    cache_dir: Path,
    recompute: bool = False,
) -> Template:
    """
    Build (or load the optical depths of) the template and convolve it.

    Parameters
    ----------
    spec : TemplateSpec
    retrieval : Retrieval
        Resolved for the instrument (``fov`` a priori, ``max_opd``,
        ``apod_type``).
    cache_dir : Path
        Where ``template_<key>.npz`` is kept.
    recompute : bool
        Ignore the cache.

    Returns
    -------
    Template
    """
    cache_dir = Path(cache_dir)
    cache_dir.mkdir(parents=True, exist_ok=True)
    path = cache_dir / f"template_{spec.key()[:16]}.npz"
    loaded = None
    if path.exists() and not recompute:
        try:
            with np.load(path) as z:
                loaded = z["wn"], z["od_ambient"], z["od_plume"]
            logger.debug("Template optical depths from %s", path.name)
        except (OSError, KeyError, ValueError) as exc:
            logger.warning("Template cache %s unreadable (%s); recomputing",
                           path.name, exc)
    if loaded is None:
        loaded = compute_optical_depths(spec)
        tmp = path.with_suffix(".tmp.npz")
        np.savez(tmp, wn=loaded[0], od_ambient=loaded[1], od_plume=loaded[2])
        tmp.replace(path)
    wn, od_a, od_p = loaded
    ils = dict(fov=float(retrieval.fov), max_opd=float(retrieval.max_opd),
               apod_type=str(retrieval.apod_type))
    conv = lambda f: convolve_blockwise(wn, f, nperwn=spec.nperwn, **ils)  # noqa: E731
    return Template(spec=spec, wn=wn, od_ambient=od_a, od_plume=od_p,
                    ambient=conv(np.exp(-od_a)), total=conv(np.exp(-(od_a + od_p))),
                    ils=ils)
