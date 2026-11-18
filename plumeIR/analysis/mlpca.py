"""
Weighted low-rank fit of a value matrix: maximum-likelihood PCA (Wentzell
et al. 1997), the engine of the all-species composition
(:mod:`plumeIR.analysis.components`).

Each row (spectrum) ``x_i`` of ``p`` values (species) is modelled as

    x_ij = a_g(i),j + Σ_k t_ik · v_jk  + noise,   noise_i ~ N(0, Σ_i),

with ``v_k`` a component (a source's composition, a direction in species
space), ``t_ik`` how much of it row ``i`` holds, and ``a`` an offset per
species and group of rows: per window (``intercept='window'``), per run or
segment (``'group'``), or none (``'none'``).  :func:`fit_components`
minimises ``S = Σ_i r_iᵀ Σ_i⁻¹ r_i`` (``r_i`` the residual vector of row
``i``) over everything: total least squares weighted by every value's error.
``Σ_i`` is diagonal (``σ_ij²``) for independent errors, or carries a
correlation between species (``corr``; for species fitted in one spectral
window, from the fit covariance).  For two species and rank 1 it is York's
line (:func:`plumeIR.regression.york_fit` /
:func:`~plumeIR.regression.york_fit_grouped`), whose ratio is ``v₂/v₁``.
:func:`fit_offsets` is rank 0, the offsets alone.

Solved by alternating weighted least squares: scores given loadings (one
small system per row), then loadings and offsets given scores (one weighted
regression per species when ``Σ_i`` is diagonal; with correlated errors the
species are coupled and the loadings are one joint system, the offsets
eliminated exactly, :func:`_loadings_correlated`); each step lowers ``S``.
Nothing here knows about gases, runs or tables: it takes arrays.

Validated (2026-11-16/18, synthetic and validation data): two species equal York to 3e-10 (5e-10 with correlated errors); the
correlated path at negligible correlation equals the diagonal one to 2e-12.

References
----------
Wentzell, P. D., Andrews, D. T., Hamilton, D. C., Faber, K. and Kowalski,
B. R. (1997).  Maximum likelihood principal component analysis.
J. Chemometrics 11, 339-366.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np

from plumeIR.exceptions import PlumeIRError

INTERCEPTS: tuple[str, ...] = ("window", "group", "none")
#: Iteration limit and relative convergence tolerance on ``S``.
_MAX_ITER, _RTOL = 5000, 1e-13


@dataclass(frozen=True)
class ComponentFit:
    """
    A weighted rank-k fit of the gas columns.

    Attributes
    ----------
    gases : list of str
    rank : int
    loadings : numpy.ndarray
        Shape ``(n_gases, rank)``, in column units per unit score; rank 1
        is normalised to unit length in error-scaled units and to a positive
        sum, so its ratios are ``loadings[j] / loadings[ref]``.
    scores : numpy.ndarray
        Shape ``(n_spectra, rank)``; NaN for spectra left out.
    offsets : numpy.ndarray
        Shape ``(n_spectra, n_gases)``: each spectrum's offset ``a``.
    chi2 : float
        ``S`` at the minimum.
    dof : int
        Valid values minus fitted parameters.
    n_values : int
    used : numpy.ndarray of bool
        Spectra in the fit.
    intercept : str
    iterations : int
    """
    gases:      list
    rank:       int
    loadings:   np.ndarray
    scores:     np.ndarray = field(repr=False)
    offsets:    np.ndarray = field(repr=False)
    chi2:       float
    dof:        int
    n_values:   int
    used:       np.ndarray = field(repr=False)
    intercept:  str
    iterations: int

    @property
    def reduced_chi2(self) -> float:
        """``chi2 / dof``: 1 when the errors explain the residual scatter."""
        return self.chi2 / self.dof if self.dof > 0 else np.nan


def _group_means(values: np.ndarray, W: np.ndarray, groups: np.ndarray, n_groups: int) -> np.ndarray:
    """Weighted mean of ``values`` (n, m) per group, per column, broadcast back to (n, m)."""
    out = np.empty_like(values)
    for j in range(values.shape[1]):
        num = np.bincount(groups, W[:, j] * values[:, j], n_groups)
        den = np.bincount(groups, W[:, j], n_groups)
        with np.errstate(invalid="ignore", divide="ignore"):
            out[:, j] = (num / den)[groups]
    return np.nan_to_num(out)


#: A correlation matrix whose smallest eigenvalue is below this is shrunk
#: towards the identity until it reaches it (pairwise correlations clipped
#: separately need not make a positive definite matrix).
_MIN_EIGENVALUE = 1e-3


def _positive_definite(C: np.ndarray) -> np.ndarray:
    """
    Shrink each correlation matrix of ``C`` (n, p, p) towards the identity
    just enough that its smallest eigenvalue is :data:`_MIN_EIGENVALUE`.
    """
    lam = np.linalg.eigvalsh(C)[:, 0]
    # eigenvalues of (1 − α)C + αI are (1 − α)λ + α
    with np.errstate(invalid="ignore", divide="ignore"):
        alpha = np.where(lam < _MIN_EIGENVALUE, (_MIN_EIGENVALUE - lam) / (1.0 - lam), 0.0)
    alpha = np.clip(np.nan_to_num(alpha), 0.0, 1.0)
    eye = np.eye(C.shape[1])
    return (1.0 - alpha)[:, None, None] * C + alpha[:, None, None] * eye


def _weight_matrices(Es: np.ndarray, C: np.ndarray, valid: np.ndarray) -> np.ndarray:
    """
    Per-row inverse covariance (n, p, p) from scaled errors ``Es`` (n, p),
    correlations ``C`` (n, p, p) and the valid mask; zero rows and columns
    for missing values (the inverse of the valid block).
    """
    both = valid[:, :, None] & valid[:, None, :]
    cov = np.where(both, C * Es[:, :, None] * Es[:, None, :], 0.0)
    cov = cov + np.einsum("ij,jk->ijk", (~valid).astype(float), np.eye(Es.shape[1]))
    return np.where(both, np.linalg.inv(cov), 0.0)


def _loadings_correlated(Z: np.ndarray, T: np.ndarray, W: np.ndarray, g: np.ndarray,
                         n_groups: int, with_offsets: bool) -> tuple[np.ndarray, np.ndarray]:
    """
    Loadings and offsets given the scores, with a full weight matrix per row.

    Minimises ``Σ_i (z_i − a_g(i) − V t_i)ᵀ W_i (…)`` over ``V`` (p, k) and
    the offsets jointly: for given ``V`` the offset of group ``g`` is the
    weighted mean ``a_g = M_g⁻¹ Σ_{i∈g} W_i (z_i − V t_i)``,
    ``M_g = Σ_{i∈g} W_i``; substituted, the objective is quadratic in
    ``vec(V)`` with normal matrix
    ``Σ_i (t_i t_iᵀ) ⊗ W_i − Σ_g P_gᵀ M_g⁻¹ P_g``, ``P_g = Σ_{i∈g} t_iᵀ ⊗ W_i``.
    A gas missing from a whole group leaves ``M_g`` singular: its offset
    there is not determined, and the pseudo-inverse sets it to zero.

    Returns
    -------
    V : numpy.ndarray
        Shape ``(p, k)``.
    A : numpy.ndarray
        Shape ``(m, p)``: each row's offsets (zero without offsets).
    """
    m, p = Z.shape
    k = T.shape[1]
    # N[(a, j), (b, l)] = Σ_i t_ia t_ib W_i[j, l]; rhs[(a, j)] = Σ_i t_ia (W_i z_i)[j]
    N = np.einsum("ia,ib,ijl->ajbl", T, T, W).reshape(k * p, k * p)
    Wz = np.einsum("ijl,il->ij", W, Z)
    rhs = np.einsum("ia,ij->aj", T, Wz).reshape(k * p)
    if with_offsets:
        Mg = np.zeros((n_groups, p, p))
        np.add.at(Mg, g, W)
        Minv = np.linalg.pinv(Mg, hermitian=True)
        # P[g, j, l, a] = Σ_{i∈g} W_i[j, l] t_ia
        P = np.zeros((n_groups, p, p, k))
        np.add.at(P, g, np.einsum("ijl,ia->ijla", W, T))
        Wzg = np.zeros((n_groups, p))
        np.add.at(Wzg, g, Wz)
        N = N - np.einsum("gjla,gjq,gqmb->albm", P, Minv, P).reshape(k * p, k * p)
        rhs = rhs - np.einsum("gjla,gjq,gq->al", P, Minv, Wzg).reshape(k * p)
    try:
        v = np.linalg.solve(N, rhs)
    except np.linalg.LinAlgError:
        v = np.linalg.lstsq(N, rhs, rcond=None)[0]
    V = v.reshape(k, p).T
    if not with_offsets:
        return V, np.zeros_like(Z)
    a = np.einsum("gjq,gq->gj", Minv, Wzg - np.einsum("gjla,la->gj", P, V))
    return V, a[g]


def _offsets_only(Z: np.ndarray, W: np.ndarray, g: np.ndarray, n_groups: int) -> np.ndarray:
    """Each row's group offset with full weights (rank 0): ``M_g⁻¹ Σ W_i z_i``."""
    p = Z.shape[1]
    Mg = np.zeros((n_groups, p, p))
    np.add.at(Mg, g, W)
    Wzg = np.zeros((n_groups, p))
    np.add.at(Wzg, g, np.einsum("ijl,il->ij", W, Z))
    return np.einsum("gjq,gq->gj", np.linalg.pinv(Mg, hermitian=True), Wzg)[g]


def chi2_correlated(R: np.ndarray, W: np.ndarray) -> float:
    """``Σ_i r_iᵀ W_i r_i`` for residuals ``R`` (n, p) and weights ``W`` (n, p, p)."""
    return float(np.einsum("ij,ijl,il->", R, W, R))


def fit_components(
    X: np.ndarray,
    E: np.ndarray,
    rank: int,
    *,
    groups: np.ndarray | None = None,
    gases: list[str] | None = None,
    intercept: str = "window",
    init: np.ndarray | None = None,
    corr: np.ndarray | None = None,
) -> ComponentFit:
    """
    Weighted rank-``rank`` fit of a value matrix with per-value errors.

    Parameters
    ----------
    X : numpy.ndarray
        Values, shape ``(n, p)``; NaN where missing.
    E : numpy.ndarray
        1σ errors, shape ``(n, p)``; a missing value's error is ignored.
    rank : int
        Number of components, ``1 ≤ rank < p``.
    groups : numpy.ndarray or None
        Offset group of each row (``intercept`` 'window' or 'group');
        ignored for ``'none'``.
    gases : list of str or None
        Column names.
    intercept : {'window', 'group', 'none'}
        Offsets per group, or none.  'window' and 'group' differ only in what
        the groups are; both need ``groups``.
    init : numpy.ndarray or None
        Starting loadings ``(p, rank)``; from a weighted SVD when ``None``.
    corr : numpy.ndarray or None
        Correlation of the errors between gases, per row, shape
        ``(n, p, p)`` (unit diagonal); ``None`` or all off-diagonal zeros for
        independent errors.  A matrix that is not positive definite (e.g.
        from clipping) is shrunk towards the identity until it is.

    Returns
    -------
    ComponentFit

    Raises
    ------
    PlumeIRError
        For invalid shapes, rank or intercept, missing groups, too few valid
        values, or no convergence.
    """
    X = np.asarray(X, dtype=float)
    E = np.asarray(E, dtype=float)
    if X.ndim != 2 or X.shape != E.shape:
        raise PlumeIRError(f"X and E must be 2-D and of equal shape, got {X.shape}, {E.shape}")
    n, p = X.shape
    if corr is not None:
        corr = np.asarray(corr, dtype=float)
        if corr.shape != (n, p, p):
            raise PlumeIRError(f"corr must have shape {(n, p, p)}, got {corr.shape}")
        off = corr[:, ~np.eye(p, dtype=bool)]
        if not np.any(np.nan_to_num(off)):
            corr = None
    gases = list(gases) if gases is not None else [f"g{j}" for j in range(p)]
    if not 1 <= rank < p:
        raise PlumeIRError(f"rank must be in [1, {p - 1}] for {p} gases, got {rank}")
    if intercept not in INTERCEPTS:
        raise PlumeIRError(f"intercept must be one of {INTERCEPTS}, got {intercept!r}")
    valid = np.isfinite(X) & np.isfinite(E) & (E > 0)
    # A spectrum needs more valid gases than components to say anything.
    used = valid.sum(axis=1) > rank
    if intercept != "none":
        if groups is None:
            raise PlumeIRError(f"intercept={intercept!r} needs groups")
        _, g = np.unique(np.asarray(groups)[used], return_inverse=True)
    else:
        g = np.zeros(int(used.sum()), dtype=np.int64)
    Xu, Eu, Vu = X[used], E[used], valid[used]
    m = Xu.shape[0]
    n_groups = int(g.max()) + 1 if m else 0
    # Work in units of each gas's median error, so weights are O(1).
    scale = np.array([np.median(Eu[Vu[:, j], j]) if Vu[:, j].any() else 1.0 for j in range(p)])
    Z = np.where(Vu, Xu / scale, 0.0)
    W = np.where(Vu, (scale / np.where(Vu, Eu, 1.0)) ** 2, 0.0)
    Wfull = None
    if corr is not None:
        Cu = np.nan_to_num(corr[used], nan=0.0)
        Cu[:, np.arange(p), np.arange(p)] = 1.0
        Cu = _positive_definite(0.5 * (Cu + Cu.transpose(0, 2, 1)))
        Wfull = _weight_matrices(np.where(Vu, Eu / scale, 1.0), Cu, Vu)
    n_values = int(Vu.sum())
    # Parameters: scores m·k, loadings p·k less the k² of a rotation, and
    # offsets p per group less k (an offset can trade against the scores
    # along each component).  For p = 2, k = 1 this is York's n − 2 (one
    # group) or n − groups − 1 (york_fit_grouped).
    n_offsets = 0 if intercept == "none" else n_groups * (p - rank)
    dof = n_values - (m * rank + p * rank - rank * rank + n_offsets)
    if dof < 1:
        raise PlumeIRError(f"{n_values} values leave no degree of freedom for rank {rank} "
                           f"with {n_offsets} offsets")

    def centre(A: np.ndarray, Wt: np.ndarray) -> np.ndarray:
        return A - _group_means(A, Wt, g, n_groups) if intercept != "none" else A

    if init is None:
        Zc = centre(Z, W)
        _, _, vt = np.linalg.svd(Zc * np.sqrt(W), full_matrices=False)
        V = vt[:rank].T
    else:
        V = np.asarray(init, dtype=float) / scale[:, None]
    A = np.zeros_like(Z)
    S_old = np.inf
    for it in range(1, _MAX_ITER + 1):
        # scores: per spectrum, weighted LS of (z_i − a_i) on V
        R = Z - A
        if Wfull is None:
            M = np.einsum("jk,ij,jl->ikl", V, W, V)
            b = np.einsum("jk,ij,ij->ik", V, W, R)
        else:
            M = np.einsum("jk,ijl,lm->ikm", V, Wfull, V)
            b = np.einsum("jk,ijl,il->ik", V, Wfull, R)
        M += np.eye(rank) * 1e-12 * np.trace(M, axis1=1, axis2=2)[:, None, None] / rank
        T = np.linalg.solve(M, b[..., None])[..., 0]
        if Wfull is not None:
            # loadings and offsets jointly: the gases are coupled
            V, A = _loadings_correlated(Z, T, Wfull, g, n_groups, intercept != "none")
            S = chi2_correlated(Z - A - T @ V.T, Wfull)
            if abs(S_old - S) <= _RTOL * max(S, 1e-300) or S <= 1e-20 * n_values:
                break
            S_old = S
            continue
        # loadings and offsets: per gas, weighted LS of z on [offsets, T]
        for j in range(p):
            w = W[:, j:j + 1]
            if intercept != "none":
                Tc = T - _group_means(T, np.repeat(w, rank, axis=1), g, n_groups)
                zc = Z[:, j] - _group_means(Z[:, j:j + 1], w, g, n_groups)[:, 0]
            else:
                Tc, zc = T, Z[:, j]
            G = Tc.T @ (w * Tc)
            V[j] = np.linalg.solve(G, Tc.T @ (w[:, 0] * zc))
            if intercept != "none":
                A[:, j] = _group_means((Z[:, j] - T @ V[j])[:, None], w, g, n_groups)[:, 0]
        S = float(np.sum(W * (Z - A - T @ V.T) ** 2))
        # converged, or an exact fit (noise-free data: S at round-off)
        if abs(S_old - S) <= _RTOL * max(S, 1e-300) or S <= 1e-20 * n_values:
            break
        S_old = S
    else:
        raise PlumeIRError(f"component fit did not converge in {_MAX_ITER} iterations "
                           f"(rank {rank}, chi2 {S:.6g})")
    if rank == 1:
        norm = np.linalg.norm(V[:, 0])
        sign = 1.0 if V[:, 0].sum() >= 0 else -1.0
        V, T = V * sign / norm, T * sign * norm
    loadings = V * scale[:, None]
    scores = np.full((n, rank), np.nan)
    scores[used] = T
    offsets = np.full((n, p), np.nan)
    offsets[used] = A * scale
    return ComponentFit(gases=gases, rank=rank, loadings=loadings, scores=scores,
                        offsets=offsets, chi2=S, dof=int(dof), n_values=n_values, used=used,
                        intercept=intercept, iterations=it)


def fit_offsets(
    X: np.ndarray,
    E: np.ndarray,
    *,
    groups: np.ndarray | None = None,
    intercept: str = "window",
    corr: np.ndarray | None = None,
) -> tuple[float, int]:
    """
    Rank 0: ``S`` and degrees of freedom with the offsets alone (no
    component), the baseline of a rank table.

    Parameters
    ----------
    X, E, groups, intercept, corr
        As for :func:`fit_components`; rows with any value missing count
        only through their valid values.

    Returns
    -------
    chi2 : float
    dof : int
        Valid values minus ``p`` offsets per group (none for ``'none'``).

    Raises
    ------
    PlumeIRError
        For an unknown intercept, or offsets without groups.
    """
    X = np.asarray(X, dtype=float)
    E = np.asarray(E, dtype=float)
    if intercept not in INTERCEPTS:
        raise PlumeIRError(f"intercept must be one of {INTERCEPTS}, got {intercept!r}")
    p = X.shape[1]
    valid = np.isfinite(X) & np.isfinite(E) & (E > 0)
    rows = valid.any(axis=1)
    X, E, valid = X[rows], E[rows], valid[rows]
    # in units of each species' median error, as fit_components does
    scale = np.array([np.median(E[valid[:, j], j]) if valid[:, j].any() else 1.0
                      for j in range(p)])
    Z = np.where(valid, X / scale, 0.0)
    if corr is None:
        W = np.where(valid, (scale / np.where(valid, E, 1.0)) ** 2, 0.0)
    else:
        C = np.nan_to_num(np.asarray(corr, dtype=float)[rows], nan=0.0)
        C[:, np.arange(p), np.arange(p)] = 1.0
        C = _positive_definite(0.5 * (C + C.transpose(0, 2, 1)))
        W = _weight_matrices(np.where(valid, E / scale, 1.0), C, valid)
    n_values = int(valid.sum())
    if intercept == "none":
        R, dof = Z, n_values
    else:
        if groups is None:
            raise PlumeIRError(f"intercept={intercept!r} needs groups")
        _, g = np.unique(np.asarray(groups)[rows], return_inverse=True)
        n_groups = int(g.max()) + 1
        A = (_group_means(Z, W, g, n_groups) if corr is None
             else _offsets_only(Z, W, g, n_groups))
        R, dof = Z - A, n_values - n_groups * p
    S = float(np.sum(W * R ** 2)) if corr is None else chi2_correlated(R, W)
    return S, int(dof)
