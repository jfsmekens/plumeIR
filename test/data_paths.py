"""
Locations of the real measurement datasets used by the tests and demo scripts.

Every dataset lives under a single root, resolved once here so that no test or
script hard-codes a machine-specific path.  The root defaults to the
repository's ``data/`` directory (git-ignored — the spectra are large binaries
and are never committed) and can be redirected with the ``PLUMEIR_TEST_DATA``
environment variable, e.g. to run against the full source datasets:

    PLUMEIR_TEST_DATA=~/data pytest test/test_spectra.py

Tests that need real data guard on the relevant constant with
``pytest.mark.skipif(not <path>.exists(), ...)`` and skip cleanly when the
dataset is absent, so the suite still runs on a machine without it.
"""

import os
from pathlib import Path

# Root holding every dataset.  Env override first, repo ./data as the default.
DATA_ROOT: Path = Path(
    os.environ.get("PLUMEIR_TEST_DATA", Path(__file__).parent.parent / "data")
)

# --- Datasets ---------------------------------------------------------------

#: Bruker OPUS, 2019-09-11 emission geometry.  Also the emission demo dataset.
BRUKER_DIR: Path = DATA_ROOT / "2019-09-11"

#: Bruker OPUS with numbered extensions (plume.0000, ...).  Subset: the reader
#: test needs one file; the rest are margin.
BRUKER_NUM_DIR: Path = DATA_ROOT / "190911_test11"

#: MIDAC .spc, 2021-08-11 IMO acquisition.  Spectra only — the fit-result
#: subdirectories of the source dataset are not mirrored here.
MIDAC_DIR: Path = DATA_ROOT / "2021-08-11-IMO"

#: MIDAC .spc, 2021-08-11 4-scan acquisition (better SNR).  The layer demo
#: dataset shipped with the public release; the spectra of the source '4scans'
#: directory, without its generated artifacts.
LAYER_DEMO_DIR: Path = DATA_ROOT / "2021-08-11"

# --- Individual files -------------------------------------------------------

BRUKER_PLUME: Path = BRUKER_DIR / "plume2019_09_11_1829_53_253000.0"
BRUKER_BBC: Path = BRUKER_DIR / "BBC.0"
BRUKER_BBH: Path = BRUKER_DIR / "BBH.0"
BRUKER_NUM: Path = BRUKER_NUM_DIR / "plume.0000"
MIDAC_FILE: Path = MIDAC_DIR / "2021_08_11_0036_01_544_sbm.spc"

#: v2 fit results for the 4-scan dataset, used to compare v3 against v2.
V2_RESULTS_CSV: Path = DATA_ROOT / "v2_reference" / "fitResults_230929_102809_ALL.csv"
