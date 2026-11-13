"""
Spectral file discovery, sorting, and renaming for plumeIR v3.

Provides the tools needed to go from a raw data directory to an ordered
list of spectrum paths, ready for sequential processing.
"""

from __future__ import annotations

import logging
import shutil
from pathlib import Path

from plumeIR.constants import (
    MIDAC_EXTENSIONS, INVALID_EXTENSIONS, REFERENCE_MARKERS,
)

logger = logging.getLogger(__name__)


def is_reference_file(path: str | Path) -> bool:
    """
    True if a file is a reference measurement rather than a scene spectrum.

    Matches :data:`~plumeIR.constants.REFERENCE_MARKERS` case-insensitively
    against the file stem.

    Parameters
    ----------
    path : str or Path
        File path to test.

    Returns
    -------
    bool
    """
    stem = Path(path).stem.lower()
    return any(marker in stem for marker in REFERENCE_MARKERS)


def find_reference_spectra(
    data_dir:   str | Path,
    markers:    list[str],
    filter_ext: str | None = None,
) -> list[Path]:
    """
    Find the preprocessing reference files matching any of ``markers``.

    The counterpart to :func:`discover_spectra`'s ``exclude_reference``: that
    drops reference files from the scene list, this picks them out of it.
    Matching is **case-insensitive** on the file stem, so ``BBC.0`` is found by
    the marker ``'bbc'`` — unlike ``discover_spectra``'s ``filter_string``,
    which is a case-sensitive substring test meant for user-supplied filters.

    Parameters
    ----------
    data_dir : str or Path
        Directory to search (non-recursive).
    markers : list of str
        Substrings to match against the lowercased file stem, e.g.
        ``['bbc']``.  A file matching any marker is returned.
    filter_ext : str or None
        Restrict to this extension (the run's ``filter_ext``).

    Returns
    -------
    list[Path]
        Matching files, unsorted.  Empty if none were found.
    """
    lowered = [m.lower() for m in markers]
    return [
        p for p in discover_spectra(
            data_dir, filter_ext=filter_ext, exclude_reference=False,
        )
        if any(m in p.stem.lower() for m in lowered)
    ]


def discover_spectra(
    data_dir: str | Path,
    filter_string: str | None = None,
    filter_ext: str | None = None,
    max_spec: int | None = None,
    exclude_reference: bool = True,
) -> list[Path]:
    """
    Find spectrum files in a directory.

    Parameters
    ----------
    data_dir : str or Path
        Directory to search (non-recursive).
    filter_string : str or None
        If given, only keep files whose stem contains this substring.
    filter_ext : str or None
        If given, only keep files with this extension (without leading dot).
        Overrides automatic format detection.
    max_spec : int or None
        Maximum number of files to return.  ``None`` = no limit.
    exclude_reference : bool
        Drop reference measurements — stems containing any of
        :data:`~plumeIR.constants.REFERENCE_MARKERS` — from the result.  These
        are preprocessing inputs, never scene spectra, and a user's
        ``filter_string`` does not reliably exclude them (``self_sbm.spc``
        matches a ``'sbm'`` filter).  Set ``False`` to find them.

    Returns
    -------
    list[Path]
        Unsorted list of matching file paths.
    """
    data_dir = Path(data_dir)
    if not data_dir.is_dir():
        raise NotADirectoryError(f"Data directory not found: {data_dir}")

    paths: list[Path] = []
    for p in data_dir.iterdir():
        if not p.is_file():
            continue
        ext = p.suffix.lstrip(".")
        # Skip known non-spectrum formats
        if ext.lower() in INVALID_EXTENSIONS:
            continue
        # Apply extension filter
        if filter_ext is not None and ext != filter_ext:
            continue
        # If no extension filter, skip unless Bruker or known MIDAC
        if filter_ext is None:
            if not (ext.isdigit() or ext.lower() in MIDAC_EXTENSIONS):
                continue
        # Apply name filter
        if filter_string is not None and filter_string not in p.stem:
            continue
        # Skip reference measurements (self-emission background, …)
        if exclude_reference and is_reference_file(p):
            continue
        paths.append(p)

    if max_spec is not None:
        paths = paths[:max_spec]

    logger.debug(
        "Discovered %d spectrum file(s) in '%s'", len(paths), data_dir.name
    )
    return paths


def load_spectra(data_config) -> list:
    """
    Discover, sort, limit, and read the spectra for a :class:`DataConfig`.

    Returns the spectra ordered by acquisition time, truncated to
    ``max_spec`` (the *earliest* that many, since the limit is applied after
    sorting).  Unreadable files are logged and skipped, not raised, so a single
    bad file does not abort a run.

    Parameters
    ----------
    data_config : DataConfig
        Data location and file-selection settings.

    Returns
    -------
    list[Spectrum]
    """
    from plumeIR.spectra.spectrum import read_spectrum

    filt = data_config.filter_string if data_config.filter_files else None
    paths = discover_spectra(
        data_config.data_dir, filter_string=filt,
        filter_ext=data_config.filter_ext, max_spec=None,
    )
    paths = sort_by_acquisition_time(paths)
    if data_config.max_spec is not None:
        paths = paths[: data_config.max_spec]

    spectra = []
    for p in paths:
        try:
            spectra.append(read_spectrum(p))
        except Exception as exc:
            logger.warning("Skipping unreadable spectrum '%s': %s", p, exc)
    logger.info("Loaded %d spectra from '%s'", len(spectra), data_config.data_dir)
    return spectra


def explain_no_spectra(data_config) -> str:
    """
    Say why :func:`load_spectra` found nothing, in terms of the config keys.

    Re-applies the discovery rules of :func:`discover_spectra` one at a time,
    extension, then ``filter_string``, then the reference-file exclusion, and
    names the first that removes every remaining file.  If files survive all
    three, they were found but none could be read.

    Parameters
    ----------
    data_config : DataConfig
        The run's data settings.

    Returns
    -------
    str
        A one-paragraph message naming the folder, what it holds, and the key
        to change.
    """
    data_dir = Path(data_config.data_dir)
    head = f"No spectra found in {data_dir}"
    if not data_dir.is_dir():
        return f"{head}: the folder does not exist (DATA.data_dir)."
    files = [p for p in data_dir.iterdir() if p.is_file()]
    if not files:
        return f"{head}: the folder holds no files (DATA.data_dir)."

    ext = data_config.filter_ext
    if ext is not None:
        candidates = [p for p in files if p.suffix.lstrip(".") == ext]
        rule = f"with extension '.{ext}' (DATA.filter_ext)"
    else:
        candidates = [p for p in files
                      if p.suffix.lstrip(".").isdigit()
                      or p.suffix.lstrip(".").lower() in MIDAC_EXTENSIONS]
        rule = "with a Bruker (numbered) or MIDAC extension"
    if not candidates:
        found: dict[str, int] = {}
        for p in files:
            key = p.suffix or "(none)"
            found[key] = found.get(key, 0) + 1
        listing = ", ".join(f"{k} ({n})" for k, n in sorted(found.items()))
        return (f"{head}: {len(files)} file(s), none {rule}. "
                f"Extensions present: {listing}.")

    filt = data_config.filter_string if data_config.filter_files else None
    if filt is not None:
        matched = [p for p in candidates if filt in p.stem]
        if not matched:
            return (f"{head}: {len(candidates)} file(s) {rule}, but none whose "
                    f"name contains '{filt}' (DATA.filter_string, "
                    f"case-sensitive). Change filter_string, or set "
                    f"filter_files: False to take them all.")
        candidates = matched

    scenes = [p for p in candidates if not is_reference_file(p)]
    if not scenes:
        markers = ", ".join(f"'{m}'" for m in REFERENCE_MARKERS)
        return (f"{head}: the {len(candidates)} matching file(s) are all "
                f"reference measurements (names containing {markers}), which "
                f"are never fitted as scene spectra.")
    return (f"{head}: {len(scenes)} file(s) matched, but none could be read; "
            f"see the warnings above for each file.")


def sort_by_acquisition_time(paths: list[Path]) -> list[Path]:
    """
    Sort spectrum files by acquisition timestamp.

    Timestamps are read from file metadata (Bruker) or filenames (MIDAC).
    Files whose timestamp cannot be determined are placed at the end,
    preserving their relative order.

    Parameters
    ----------
    paths : list[Path]
        Unsorted list of spectrum paths.

    Returns
    -------
    list[Path]
        Sorted list, earliest first.
    """
    from plumeIR.spectra.spectrum import read_spectrum
    from plumeIR.exceptions import SpectrumIOError

    timed:   list[tuple[Path, object]] = []
    untimed: list[Path]                = []

    for p in paths:
        try:
            spec = read_spectrum(p)
            if spec.timestamp is not None:
                timed.append((p, spec.timestamp))
            else:
                untimed.append(p)
        except SpectrumIOError as exc:
            logger.warning("Could not read '%s' for sorting: %s", p.name, exc)
            untimed.append(p)

    timed.sort(key=lambda t: t[1])

    if untimed:
        logger.warning(
            "%d file(s) have no timestamp and will be appended unsorted", len(untimed)
        )

    return [p for p, _ in timed] + untimed


def detect_corrupted(paths: list[Path]) -> tuple[list[Path], list[Path]]:
    """
    Separate readable spectrum files from corrupted ones.

    Attempts to read each file; files that raise a ``SpectrumIOError``
    are classified as corrupted.

    Parameters
    ----------
    paths : list[Path]
        List of spectrum paths to check.

    Returns
    -------
    tuple[list[Path], list[Path]]
        ``(valid, corrupted)`` — both in the same order as the input.
    """
    from plumeIR.spectra.spectrum import read_spectrum
    from plumeIR.exceptions import SpectrumIOError

    valid:     list[Path] = []
    corrupted: list[Path] = []

    for p in paths:
        try:
            read_spectrum(p)
            valid.append(p)
        except SpectrumIOError as exc:
            logger.warning("Corrupted file '%s': %s", p.name, exc)
            corrupted.append(p)

    if corrupted:
        logger.warning(
            "%d / %d file(s) could not be read", len(corrupted), len(paths)
        )

    return valid, corrupted


def move_corrupted(
    corrupted: list[Path],
    dest_dir: str | Path | None = None,
) -> Path:
    """
    Move corrupted files to a ``corrupted_files/`` subdirectory.

    Parameters
    ----------
    corrupted : list[Path]
        Files to move.
    dest_dir : str or Path or None
        Destination directory.  If ``None``, creates ``corrupted_files/``
        inside the parent of the first file.

    Returns
    -------
    Path
        The directory the files were moved into.
    """
    if not corrupted:
        return Path()

    if dest_dir is None:
        dest_dir = corrupted[0].parent / "corrupted_files"

    dest_dir = Path(dest_dir)
    dest_dir.mkdir(exist_ok=True)

    for p in corrupted:
        dest = dest_dir / p.name
        shutil.move(str(p), dest)
        logger.info("Moved corrupted file '%s' → '%s'", p.name, dest_dir.name)

    return dest_dir


def rename_with_timestamp(
    paths: list[Path],
    prefix: str = "",
    dry_run: bool = True,
) -> dict[Path, Path]:
    """
    Rename files to embed their acquisition timestamp in the filename.

    Produces names of the form ``{prefix}{YYYYMMDDHHMMSS_ffffff}{ext}``.
    Files that already contain a parseable timestamp are left unchanged.

    Parameters
    ----------
    paths : list[Path]
        Files to rename.
    prefix : str
        Optional prefix prepended to every new name.
    dry_run : bool
        If ``True`` (default), return the planned mapping without
        performing any renames.

    Returns
    -------
    dict[Path, Path]
        Mapping of ``old_path → new_path`` for files that would be
        (or were) renamed.  Files left unchanged are not included.
    """
    from plumeIR.spectra.spectrum import _timestamp_from_name, read_spectrum
    from plumeIR.exceptions import SpectrumIOError

    mapping: dict[Path, Path] = {}

    for p in paths:
        # Skip if timestamp already in name
        if _timestamp_from_name(p) is not None:
            continue

        try:
            spec = read_spectrum(p)
        except SpectrumIOError as exc:
            logger.warning(
                "Cannot determine timestamp for '%s': %s — skipped", p.name, exc
            )
            continue

        if spec.timestamp is None:
            logger.warning(
                "No timestamp found for '%s' — skipped", p.name
            )
            continue

        ts_str  = spec.timestamp.strftime("%Y%m%d%H%M%S_%f")
        new_name = f"{prefix}{ts_str}{p.suffix}"
        new_path = p.parent / new_name
        mapping[p] = new_path

        if not dry_run:
            p.rename(new_path)
            logger.info("Renamed '%s' → '%s'", p.name, new_name)

    if dry_run and mapping:
        logger.info(
            "dry_run=True: %d file(s) would be renamed (call with dry_run=False to apply)",
            len(mapping),
        )

    return mapping
