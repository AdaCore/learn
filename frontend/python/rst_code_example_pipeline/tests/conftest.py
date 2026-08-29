"""
Fixtures shared by the whole rst_code_example_pipeline test suite.

The package keeps its settings in module-level globals, and its entry points
change the process working directory without changing it back.  Whatever a
test does to either one is therefore still in place when the next test runs,
and containing that is not specific to any one test module -- so it is done
here once instead of being re-implemented, differently, in each of them.

- ``restore_cwd`` puts the working directory back after every test.  Several
  entry points chdir and never chdir back: check_block() moves into the block
  directory it is checking, and get_projects() moves into the build directory
  it is scanning.  A test that reaches either one would otherwise leave the
  whole session pointing at a temporary directory that is deleted soon
  afterwards, and every later test that uses a relative path would fail for
  reasons that have nothing to do with what it is testing.
- ``reset_pipeline_globals`` puts the settings globals of the three entry-point
  modules back to the values their modules declare, around every test.  Those
  globals are what the command-line switches assign to, so a test that sets one
  is changing the setting for the rest of the session.
- ``restore_color_state`` puts ``Colors._enabled`` back after every test, so a
  test that turns colors on or off cannot change what a later test finds in its
  captured output.
- ``work_dir`` is opt-in rather than autouse: it enters a fresh temporary
  directory for the duration of the test and hands it back, for the many tests
  whose subject reads or writes relative to the working directory.
"""
import os

import pytest

from rst_code_example_pipeline import blocks
from rst_code_example_pipeline import check_code_block
from rst_code_example_pipeline import check_projects
from rst_code_example_pipeline import extract_projects
from rst_code_example_pipeline.colors import Colors


@pytest.fixture(autouse=True)
def restore_cwd():
    """Restore the working directory after each test."""
    original = os.getcwd()
    yield
    os.chdir(original)


def _reset_pipeline_globals() -> None:
    """Assign the settings globals the values their own modules declare."""
    check_code_block.verbose = False
    check_code_block.all_diagnostics = False
    check_code_block.max_columns = 0
    check_code_block.force_checks = False

    check_projects.verbose = False
    check_projects.all_diagnostics = False
    check_projects.max_columns = 0
    check_projects.force_checks = False

    extract_projects.verbose = False
    extract_projects.code_block_at = None
    extract_projects.current_config = blocks.ConfigBlock(
        run_button=False, prove_button=True, accumulate_code=False
    )


@pytest.fixture(autouse=True)
def reset_pipeline_globals():
    """Reset the entry-point modules' settings globals around each test."""
    _reset_pipeline_globals()
    yield
    _reset_pipeline_globals()


@pytest.fixture(autouse=True)
def restore_color_state():
    """Restore Colors._enabled after each test."""
    original = Colors._enabled
    yield
    Colors._enabled = original


@pytest.fixture()
def work_dir(tmp_path, monkeypatch):
    """Change to a fresh temporary directory and restore cwd on teardown."""
    monkeypatch.chdir(tmp_path)
    return tmp_path
