"""
plumeIR.retrieval — retrieval engine (Phase 5).

Backends map a ``ForwardModel`` + ``ParameterSet`` to an immutable
``FitResult``.  ``NonLinearLeastSquares`` (NLLS) is the fast, prior-free path;
the optimal-estimation backend follows.  Both are single-fit and
parallelism-agnostic — concurrency is handled by the run-orchestration layer.
"""

from plumeIR.retrieval.results import FitResult, ResultsTable
from plumeIR.retrieval.nlls import NonLinearLeastSquares
from plumeIR.retrieval.oe import OptimalEstimation
from plumeIR.retrieval.run import (
    SpectrumFits, build_backend, collect_results, iter_retrieval, run_retrieval,
)

__all__ = [
    "FitResult", "ResultsTable", "NonLinearLeastSquares", "OptimalEstimation",
    "SpectrumFits", "iter_retrieval", "collect_results", "run_retrieval",
    "build_backend",
]

