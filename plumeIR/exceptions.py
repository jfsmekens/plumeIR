"""
Exception hierarchy for plumeIR v3.

All plumeIR exceptions derive from PlumeIRError so callers can catch
the entire family with a single except clause when appropriate.
"""


class PlumeIRError(Exception):
    """Base class for all plumeIR exceptions."""


class ConfigError(PlumeIRError):
    """
    Raised when a configuration file cannot be parsed or fails validation.

    The message should identify the offending field and the reason.
    Example: "RETRIEVAL.retrieval_method: expected 'nlls' or 'oe', got 'lm'"
    """


class ReferenceComputationError(PlumeIRError):
    """
    Raised when pre-computation of the Reference object fails.

    Covers RFM subprocess failures, mieEODG computation errors, and
    cache read/write problems.
    """


class SpectrumIOError(PlumeIRError):
    """
    Raised when a spectrum file cannot be read or parsed.

    Includes unsupported formats, corrupted files, and missing timestamps.
    """


class FitError(PlumeIRError):
    """
    Raised for unrecoverable failures in the retrieval engine.

    Routine convergence failures are not raised as exceptions — they are
    recorded in FitResult.success=False.  This exception is reserved for
    structural problems (e.g. mismatched parameter vector length, singular
    Jacobian that prevents any iteration).
    """


class RunFolderError(PlumeIRError):
    """
    Raised when a finished run's folder cannot be reopened.

    A file the folder should hold is missing (e.g. ``fits.npz`` in a run
    written before M3c), or its files disagree with each other.
    """
