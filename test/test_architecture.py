"""
docs/ARCHITECTURE.md section 5 lists every module of the package.

The code map fell behind the code once (7 of 78 modules unmapped by
2026-11-18); this keeps the inventory complete in both directions: every
module is listed, and every listed module exists.  Package ``__init__.py``
files may be listed but need not be, unless they hold code a reader looks for.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PACKAGE = ROOT / "plumeIR"
MAP = ROOT / "docs" / "ARCHITECTURE.md"


def _inventory() -> str:
    """The text of section 5, "Every module"."""
    text = MAP.read_text(encoding="utf-8")
    start = text.index("## 5. Every module")
    return text[start:text.index("\n## ", start + 1)]


def _listed() -> set[str]:
    """Module paths written in section 5 (outside ``test/``, which it also names)."""
    return {m for m in re.findall(r"`([\w/]+\.py)`", _inventory())
            if not m.startswith("test/")}


def _modules() -> set[str]:
    """Every module path relative to ``plumeIR/``, without package ``__init__``."""
    return {p.relative_to(PACKAGE).as_posix() for p in PACKAGE.rglob("*.py")
            if "__pycache__" not in p.parts
            and not (p.name == "__init__.py" and p.parent != PACKAGE)}


def test_every_module_is_listed():
    listed = _listed()
    missing = sorted(_modules() - listed)
    assert not missing, f"add to docs/ARCHITECTURE.md section 5: {missing}"


def test_every_listed_module_exists():
    listed = _listed()
    stale = sorted(m for m in listed if not (PACKAGE / m).is_file())
    assert not stale, f"listed in docs/ARCHITECTURE.md section 5 but gone: {stale}"


def test_every_subpackage_has_a_heading():
    subpackages = {p.name for p in PACKAGE.iterdir()
                   if p.is_dir() and (p / "__init__.py").is_file()}
    text = _inventory()
    missing = sorted(s for s in subpackages if f"**`{s}/`**" not in text)
    assert not missing, f"no heading in docs/ARCHITECTURE.md section 5 for: {missing}"
