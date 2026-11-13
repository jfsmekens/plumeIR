"""
The worked examples in docs/CONFIG_GUIDE.md must be valid configs.

Each example is the fenced block under a "### 9.x" heading.  It is written to a
temporary folder with ``data_dir`` pointed there, then read and validated by
plumeIR's own parser and builder, so the guide cannot drift from the code.
The emission example names an RFM profile and is skipped where RFM's atm
folder is not configured.
"""

from __future__ import annotations

import re
from pathlib import Path

import pytest

from plumeIR.config.builder import build_from_raw
from plumeIR.config.parser import read_config

GUIDE = Path(__file__).resolve().parent.parent / "docs" / "CONFIG_GUIDE.md"


def _examples() -> dict[str, str]:
    """``{'9.1 Layer': text, ...}`` from the guide's section 9."""
    text = GUIDE.read_text(encoding="utf-8")
    found = re.findall(r"^### (9\.\d [^\n]+)\n.*?```\n(.*?)```", text,
                       flags=re.S | re.M)
    return {title.strip(): body for title, body in found}


def test_guide_has_both_examples():
    assert set(_examples()) == {"9.1 Layer", "9.2 Emission"}


@pytest.mark.parametrize("title", ["9.1 Layer", "9.2 Emission"])
def test_guide_example_builds(tmp_path, title):
    body = _examples()[title]
    body = re.sub(r"^data_dir\s*:.*$", f"data_dir : {tmp_path}", body, flags=re.M)
    if "atm_path" in body:
        from plumeIR.config.paths import rfm_atm_dir
        atm = rfm_atm_dir()
        if atm is None or not (atm / "mls.atm").is_file():
            pytest.skip("RFM atm folder with mls.atm not configured")
    path = tmp_path / "example.ini"
    path.write_text(body, encoding="utf-8")
    cfg = build_from_raw(read_config(path), base_dir=tmp_path)
    assert cfg["windows"], "no active windows"
    targets = {t for w in cfg["windows"] for t in w.targets}
    assert targets, "no targets"
