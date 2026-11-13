"""
Versions of plumeIR and its companion packages, for the run record.

plumeIR requires ``rfmwrapper`` and ``mieEODG`` at a minimum version only, so
fixes to either reach users without a plumeIR release (decided 2026-11-13).
The price is that two runs under the same plumeIR version may use different
companion code, so every run folder records what was installed: the version,
and the git commit where one can be found — from pip's record of a
``git+https`` install (``direct_url.json``), or by asking git in the source
folder of an editable or development install (the folder holding the
package), flagged ``+local changes`` when its working tree differs from the
commit.
"""

from __future__ import annotations

import importlib.util
import json
import subprocess
from importlib import metadata
from pathlib import Path
from urllib.parse import unquote, urlparse

#: Distributions named in the run record, in this order.
RECORDED = ("plumeIR", "rfmwrapper", "mieEODG")


def _git(folder: Path, *args: str) -> str | None:
    try:
        out = subprocess.run(["git", "-C", str(folder), *args], capture_output=True,
                             text=True, timeout=5, check=True)
    except (OSError, subprocess.SubprocessError):
        return None
    return out.stdout.strip()


def _git_state(folder: Path) -> str | None:
    """``'git abc1234'``, with ``' +local changes'`` for a modified tree, or None."""
    commit = _git(folder, "rev-parse", "--short", "HEAD")
    if not commit:
        return None
    dirty = _git(folder, "status", "--porcelain", "--untracked-files=no")
    return f"git {commit}{' +local changes' if dirty else ''}"


def describe(dist_name: str) -> str:
    """
    One line describing an installed distribution.

    Parameters
    ----------
    dist_name : str
        Distribution name, which is also the import name for every entry
        of :data:`RECORDED`.

    Returns
    -------
    str
        e.g. ``'0.6.0 (git 6c5d891)'``, ``'0.6.0 (editable, git 6c5d891 +local
        changes)'``, ``'not installed'``.
    """
    try:
        dist = metadata.distribution(dist_name)
    except metadata.PackageNotFoundError:
        return "not installed"
    text = dist.version
    info: dict = {}
    raw = dist.read_text("direct_url.json")
    if raw:
        try:
            info = json.loads(raw)
        except ValueError:
            info = {}
    if "vcs_info" in info:
        commit = info["vcs_info"].get("commit_id", "")[:7]
        return f"{text} (git {commit})" if commit else text
    if info.get("dir_info", {}).get("editable"):
        url = urlparse(info.get("url", ""))
        folder = Path(unquote(url.path)) if url.scheme == "file" else None
        state = _git_state(folder) if folder else None
        return f"{text} (editable, {state})" if state else f"{text} (editable)"
    # Older development installs (egg-link) record no URL: ask git in the
    # folder that holds the package.  A site-packages install is in no repo.
    spec = importlib.util.find_spec(dist_name)
    if spec is not None and spec.origin:
        state = _git_state(Path(spec.origin).resolve().parent.parent)
        if state:
            return f"{text} ({state})"
    return text


def package_versions() -> dict[str, str]:
    """:func:`describe` for every distribution in :data:`RECORDED`."""
    return {name: describe(name) for name in RECORDED}
