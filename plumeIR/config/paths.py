"""
Resolution of file-system paths in a raw config dict.

A config must not have to hard-code a machine-specific absolute path.  Every
path-valued entry is resolved here, once, before validation, so that a config
and the data beside it can be moved or shared as a unit.

Resolution order for a value (first existing candidate wins):

  1. ``~`` and ``$VAR`` are expanded.
  2. An absolute path is used as written.
  3. A relative path is taken relative to the **config file's directory**
     first, then the working directory (so configs that predate this rule, with
     paths relative to the project root, keep working).
  4. RFM atmosphere profiles (``atm_path``) are additionally
     looked up in ``<rfm_dir>/atm`` — the directory rfmwrapper resolves from
     ``RFM_DIR``, its user config file, or ``~/RFM`` — so a bare file name such
     as ``mls.atm`` is enough.

If no candidate exists the first candidate is returned unchanged, so the
validator reports the missing file in the usual way.
"""

from __future__ import annotations

import logging
import os
from collections.abc import Sequence
from pathlib import Path

from plumeIR.config.keys import path_keys

logger = logging.getLogger(__name__)

#: ``(section, key)`` of every path-valued config entry, from the key
#: catalogue (keys of type ``path``).
PATH_KEYS: tuple[tuple[str, str], ...] = tuple(
    (k.section, k.name) for k in path_keys())

#: Entries that are RFM atmosphere profiles, also searched in ``<rfm_dir>/atm``
#: (keys declared with ``rfm_atm``).
RFM_ATM_KEYS: frozenset[tuple[str, str]] = frozenset(
    (k.section, k.name) for k in path_keys() if k.rfm_atm)


def rfm_atm_dir() -> Path | None:
    """
    Return the RFM atmosphere-profile directory, or None if rfmwrapper is absent.

    Returns
    -------
    Path or None
        ``<rfm_dir>/atm`` as resolved by :func:`rfmwrapper.config.get_rfm_dir`.
    """
    try:
        from rfmwrapper.config import get_rfm_dir
    except ImportError:
        return None
    return Path(get_rfm_dir()) / 'atm'


def candidate_paths(
    value: str,
    base_dir: Path,
    search_dirs: Sequence[Path] = (),
) -> list[Path]:
    """
    List the locations a config path value may refer to, in priority order.

    Parameters
    ----------
    value : str
        The path as written in the config.
    base_dir : Path
        Directory a relative path is taken relative to (the config file's).
    search_dirs : sequence of Path
        Further directories tried, in order, for a relative path.

    Returns
    -------
    list of Path
        One entry for an absolute path; otherwise ``base_dir / value``,
        ``Path.cwd() / value``, then ``d / value`` for each of ``search_dirs``.
    """
    expanded = Path(os.path.expandvars(value)).expanduser()
    if expanded.is_absolute():
        return [expanded]
    return [base_dir / expanded, Path.cwd() / expanded,
            *(d / expanded for d in search_dirs)]


def resolve_path(
    value: str,
    base_dir: Path,
    search_dirs: Sequence[Path] = (),
) -> Path:
    """
    Resolve one config path value (see the module docstring for the order).

    Parameters
    ----------
    value : str
        The path as written in the config.
    base_dir : Path
        Directory a relative path is taken relative to.
    search_dirs : sequence of Path
        Further directories tried for a relative path.

    Returns
    -------
    Path
        The first candidate that exists, else the first candidate.
    """
    candidates = candidate_paths(value, base_dir, search_dirs)
    return next((c for c in candidates if c.exists()), candidates[0])


def resolve_config_paths(raw: dict, base_dir: str | Path) -> dict:
    """
    Resolve every path-valued entry of a raw config dict.

    Parameters
    ----------
    raw : dict
        The dict from :func:`plumeIR.config.parser.read_config`.  Not modified.
    base_dir : str or Path
        Directory relative paths are taken relative to; normally the directory
        containing the config file.

    Returns
    -------
    dict
        A copy of ``raw`` in which each entry of :data:`PATH_KEYS` is an
        absolute-or-unresolved path string.  Resolving an already-resolved
        config is a no-op, so the result may be written out and re-read from
        another directory.
    """
    base = Path(base_dir)
    atm_dir = rfm_atm_dir()
    out = dict(raw)

    for section, key in PATH_KEYS:
        sec = raw.get(section)
        value = sec.get(key) if isinstance(sec, dict) else None
        if not isinstance(value, str):
            continue

        search = [atm_dir] if atm_dir and (section, key) in RFM_ATM_KEYS else []
        resolved = resolve_path(value, base, search)

        if (section, key) in RFM_ATM_KEYS and not resolved.exists():
            tried = ', '.join(str(c) for c in candidate_paths(value, base, search))
            logger.warning("%s.%s: %r not found; looked in: %s",
                           section, key, value, tried)

        if out[section] is sec:
            out[section] = dict(sec)
        out[section][key] = str(resolved)

    return out
