"""
AtmLayerLUT — pre-computed radiance/transmittance for one atmospheric layer
in the emission geometry.

Each of the three emission layers (proximal, slice, distal) is computed by
zenith RFM through the sliced atmosphere at a set of H2O column-scaling
nodes.  The LUT stores, per layer:

    od_table(ν, H2O_sc)     — layer optical depth (from RFM 'opt')
    T_eff_table(ν, H2O_sc)  — effective emission (brightness) temperature

The forward applies a temperature offset analytically:

    L_layer(dT) = rad(ν, T_eff + dT) · (1 − T_layer),   T_layer = exp(−OD)

This keeps the LUT 1-D over H2O_sc (no temperature axis) while making the
layer's emission both adjustable and genuinely vertically-resolved (the real
RFM emission structure is encoded in T_eff).

**Interpolation over H2O_sc is QUADRATIC in optical depth** (3-node), not
linear in transmittance.  Because only H2O scales with H2O_sc (CO2/O3 fixed),
the layer OD is exactly ``OD_fixed + a·sc + b·sc²`` — quadratic — so 3-node
quadratic interpolation of OD is near-exact for the lines (a·sc) AND the
self-continuum (b·sc²).  Validated against direct RFM: vs linear-in-T, the
radiance line error drops 3.6%→0.15% and the continuum baseline error
0.9%→0.02% — the latter is what protects aerosol extinction/emission
retrieval, which sits on top of the smooth continuum.  T_eff stays linearly
interpolated (it varies slowly; the residual radiance error is ~0.02%).
Measured against direct RFM runs at the interpolation midpoints.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass

import numpy as np

logger = logging.getLogger(__name__)

# Below this (1 − T) the layer is effectively transparent; T_eff is
# indeterminate there but irrelevant since emission ∝ (1 − T) → 0.
_TRANSPARENT_EPS = 1e-6


def derive_t_eff(
    wn: np.ndarray,
    L_table: np.ndarray,
    T_table: np.ndarray,
    fill_temp: float = 250.0,
) -> np.ndarray:
    """
    Derive the effective emission temperature from layer radiance and
    transmittance: ``T_eff = bbt(ν, L / (1 − T))``.

    Where the layer is effectively transparent (``1 − T < eps``) the emission
    is ~0 regardless of temperature, so ``T_eff`` is set to ``fill_temp`` (a
    harmless placeholder — the forward multiplies it by ``1 − T ≈ 0``).

    Parameters
    ----------
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    L_table : np.ndarray
        Layer radiance [mW/(m²·sr·cm⁻¹)], shape ``(n_sc, n_wn)``.
    T_table : np.ndarray
        Layer transmittance, shape ``(n_sc, n_wn)``.
    fill_temp : float
        Placeholder temperature [K] for transparent regions.

    Returns
    -------
    np.ndarray
        Effective emission temperature [K], shape ``(n_sc, n_wn)``.
    """
    from plumeIR.physics import bbt

    one_minus_T = 1.0 - T_table
    opaque = one_minus_T > _TRANSPARENT_EPS

    L_eff = np.where(opaque, L_table / np.where(opaque, one_minus_T, 1.0), np.nan)
    t_eff = np.full_like(L_table, fill_temp)
    # bbt per scaling row (wn broadcasts)
    for j in range(L_table.shape[0]):
        row = opaque[j]
        if np.any(row):
            t_eff[j, row] = bbt(wn[row], L_eff[j, row])
    return t_eff


@dataclass
class AtmLayerLUT:
    """
    Optical-depth / emission-temperature LUT for one emission atmospheric layer.

    Parameters
    ----------
    name : str
        Layer identifier: ``'prox'``, ``'slice'``, or ``'dist'``.
    wn : np.ndarray
        Wavenumber grid [cm⁻¹], shape ``(n_wn,)``.
    h2o_sc_nodes : np.ndarray
        H2O column-scaling axis (ascending), shape ``(n_sc,)``.
    od_table : np.ndarray
        Layer optical depth (RFM 'opt'), shape ``(n_sc, n_wn)``.
    T_eff_table : np.ndarray
        Effective emission temperature [K], shape ``(n_sc, n_wn)``.
    """
    name:         str
    wn:           np.ndarray
    h2o_sc_nodes: np.ndarray
    od_table:     np.ndarray
    T_eff_table:  np.ndarray

    @property
    def n_sc(self) -> int:
        return len(self.h2o_sc_nodes)

    def query_od(self, h2o_sc: float) -> tuple[np.ndarray, np.ndarray]:
        """
        Interpolate ``(OD, T_eff)`` at the given H2O scaling.

        Returns optical depth rather than transmittance so callers can add
        further terms (a distal O3 column scaling, say) **in OD space**
        before exponentiating — which keeps saturated line cores correct.

        Optical depth is interpolated **quadratically** over H2O_sc (3-node,
        exact for ``OD_fixed + a·sc + b·sc²``), then ``T = exp(−OD)``.  T_eff
        is interpolated linearly.  H2O_sc is clamped to the node range.

        Returns
        -------
        (OD, T_eff)
            Each shape ``(n_wn,)``.
        """
        sc = float(np.clip(h2o_sc, self.h2o_sc_nodes[0], self.h2o_sc_nodes[-1]))
        nodes = self.h2o_sc_nodes

        i = int(np.clip(np.searchsorted(nodes, sc) - 1, 0, self.n_sc - 2))

        if self.n_sc >= 3:
            # Quadratic (3-node) interpolation of OD
            base = int(np.clip(i - 1, 0, self.n_sc - 3))   # valid 3-node window
            s0, s1, s2 = nodes[base], nodes[base + 1], nodes[base + 2]
            w0 = (sc - s1) * (sc - s2) / ((s0 - s1) * (s0 - s2))
            w1 = (sc - s0) * (sc - s2) / ((s1 - s0) * (s1 - s2))
            w2 = (sc - s0) * (sc - s1) / ((s2 - s0) * (s2 - s1))
            od = (w0 * self.od_table[base]
                  + w1 * self.od_table[base + 1]
                  + w2 * self.od_table[base + 2])
        else:
            # Fewer than 3 nodes: linear OD interpolation
            a = float(np.clip((sc - nodes[i]) / (nodes[i + 1] - nodes[i]), 0.0, 1.0))
            od = (1.0 - a) * self.od_table[i] + a * self.od_table[i + 1]
        # Linear interpolation of T_eff
        a = float(np.clip((sc - nodes[i]) / (nodes[i + 1] - nodes[i]), 0.0, 1.0))
        T_eff = (1.0 - a) * self.T_eff_table[i] + a * self.T_eff_table[i + 1]
        return od, T_eff

    def query(self, h2o_sc: float) -> tuple[np.ndarray, np.ndarray]:
        """
        Interpolate ``(T_layer, T_eff)`` at the given H2O scaling.

        Thin wrapper over :meth:`query_od`; see there for the interpolation
        scheme.

        Returns
        -------
        (T_layer, T_eff)
            Each shape ``(n_wn,)``.
        """
        od, T_eff = self.query_od(h2o_sc)
        return np.exp(-od), T_eff

    def emission(
        self,
        h2o_sc:   float,
        dT:       float = 0.0,
        extra_od: np.ndarray | None = None,
    ) -> tuple[np.ndarray, np.ndarray]:
        """
        Layer emission radiance and transmittance at the given conditions.

        ``L = rad(ν, T_eff + dT) · (1 − T_layer)``.

        Parameters
        ----------
        h2o_sc : float
            H2O column-scaling factor.
        dT : float
            Temperature offset [K] applied analytically to ``T_eff``.
        extra_od : np.ndarray or None
            Additional optical depth added **before** the exponential, shape
            ``(n_wn,)``.  Used for per-gas column scalings that are independent
            of ``h2o_sc`` — the emission geometry's distal O3 term.  Added in
            OD space so saturated cores stay saturated and the layer's
            emissivity ``1 − T`` rises consistently with its absorption.

        Returns
        -------
        (L_layer, T_layer)
            Each shape ``(n_wn,)``.
        """
        from plumeIR.physics import rad

        od, T_eff = self.query_od(h2o_sc)
        if extra_od is not None:
            od = od + extra_od
        T_layer = np.exp(-od)
        L_layer = rad(self.wn, T_eff + dT) * (1.0 - T_layer)
        return L_layer, T_layer
