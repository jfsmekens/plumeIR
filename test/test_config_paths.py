"""Tests for plumeIR.config.paths — machine-independent config path resolution."""

from __future__ import annotations

import logging
from pathlib import Path

import pytest

from plumeIR.config.paths import (
    PATH_KEYS,
    candidate_paths,
    resolve_config_paths,
    resolve_path,
)


@pytest.fixture
def rfm_dir(tmp_path, monkeypatch) -> Path:
    """A fake RFM tree, selected through rfmwrapper's RFM_DIR variable."""
    root = tmp_path / 'rfm'
    (root / 'atm').mkdir(parents=True)
    monkeypatch.setenv('RFM_DIR', str(root))
    return root


@pytest.fixture
def cfg_dir(tmp_path, monkeypatch) -> Path:
    """A config directory, with the working directory somewhere else."""
    d = tmp_path / 'project' / 'configs'
    d.mkdir(parents=True)
    elsewhere = tmp_path / 'elsewhere'
    elsewhere.mkdir()
    monkeypatch.chdir(elsewhere)
    return d


class TestResolvePath:
    def test_absolute_path_used_as_written(self, tmp_path):
        f = tmp_path / 'a.atm'
        f.touch()
        assert resolve_path(str(f), Path('/ignored')) == f
        assert candidate_paths(str(f), Path('/ignored'), [Path('/x')]) == [f]

    def test_relative_to_config_dir_not_cwd(self, cfg_dir):
        (cfg_dir / 'data').mkdir()
        assert resolve_path('data', cfg_dir) == cfg_dir / 'data'

    def test_cwd_is_the_fallback_for_older_configs(self, cfg_dir, tmp_path):
        (Path.cwd() / 'data').mkdir()
        assert resolve_path('data', cfg_dir) == Path.cwd() / 'data'

    def test_config_dir_beats_cwd(self, cfg_dir):
        (cfg_dir / 'data').mkdir()
        (Path.cwd() / 'data').mkdir()
        assert resolve_path('data', cfg_dir) == cfg_dir / 'data'

    def test_search_dir_used_last(self, cfg_dir, tmp_path):
        extra = tmp_path / 'extra'
        extra.mkdir()
        (extra / 'mls.atm').touch()
        assert resolve_path('mls.atm', cfg_dir, [extra]) == extra / 'mls.atm'

    def test_tilde_and_env_var_expanded(self, cfg_dir, tmp_path, monkeypatch):
        monkeypatch.setenv('HOME', str(tmp_path))
        monkeypatch.setenv('PLUMEIR_X', str(tmp_path))
        (tmp_path / 'f.txt').touch()
        assert resolve_path('~/f.txt', cfg_dir) == tmp_path / 'f.txt'
        assert resolve_path('$PLUMEIR_X/f.txt', cfg_dir) == tmp_path / 'f.txt'

    def test_missing_returns_first_candidate(self, cfg_dir):
        assert resolve_path('nope.atm', cfg_dir) == cfg_dir / 'nope.atm'


class TestResolveConfigPaths:
    def test_bare_atm_name_found_in_rfm_dir(self, cfg_dir, rfm_dir):
        (rfm_dir / 'atm' / 'mls.atm').touch()
        out = resolve_config_paths({'GEOMETRY': {'atm_path': 'mls.atm'}}, cfg_dir)
        assert out['GEOMETRY']['atm_path'] == str(rfm_dir / 'atm' / 'mls.atm')

    def test_other_keys_do_not_search_the_rfm_dir(self, cfg_dir, rfm_dir):
        (rfm_dir / 'atm' / 'spec.txt').touch()
        out = resolve_config_paths({'GEOMETRY': {'self_file': 'spec.txt'}}, cfg_dir)
        assert out['GEOMETRY']['self_file'] == str(cfg_dir / 'spec.txt')

    def test_input_not_modified(self, cfg_dir, rfm_dir):
        (rfm_dir / 'atm' / 'mls.atm').touch()
        raw = {'GEOMETRY': {'atm_path': 'mls.atm'}}
        resolve_config_paths(raw, cfg_dir)
        assert raw == {'GEOMETRY': {'atm_path': 'mls.atm'}}

    def test_none_and_absent_values_left_alone(self, cfg_dir, rfm_dir):
        raw = {'GEOMETRY': {'atm_path': None}, 'DATA': {}}
        assert resolve_config_paths(raw, cfg_dir) == raw

    def test_idempotent_from_another_directory(self, cfg_dir, rfm_dir, tmp_path):
        (rfm_dir / 'atm' / 'mls.atm').touch()
        once = resolve_config_paths({'GEOMETRY': {'atm_path': 'mls.atm'}}, cfg_dir)
        assert resolve_config_paths(once, tmp_path / 'other') == once

    def test_unfound_atm_warns_with_every_location(self, cfg_dir, rfm_dir, caplog):
        with caplog.at_level(logging.WARNING, logger='plumeIR.config.paths'):
            resolve_config_paths({'GEOMETRY': {'atm_path': 'nope.atm'}}, cfg_dir)
        msg = caplog.text
        assert str(cfg_dir / 'nope.atm') in msg
        assert str(rfm_dir / 'atm' / 'nope.atm') in msg

    def test_every_path_key_is_resolved(self, cfg_dir, rfm_dir):
        raw: dict = {}
        for section, key in PATH_KEYS:
            raw.setdefault(section, {})[key] = f'{key}.dat'
        out = resolve_config_paths(raw, cfg_dir)
        for section, key in PATH_KEYS:
            assert Path(out[section][key]).is_absolute()
