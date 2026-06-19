"""
Unit tests for rst_code_example_pipeline.check_projects.

Covers:
- get_blocks([]) → empty dict
- get_blocks() with a valid block_info.json present → dict with one project entry
- get_blocks() with a block_info.json missing the project field → skips, dict empty
- get_projects(build_dir, projects_list_file=None) with no JSON files → empty dict
- get_projects(build_dir, projects_list_file) with a valid projects-list JSON
- cwd side effect: get_projects calls os.chdir(build_dir) — fixture saves/restores cwd
"""
import json
import os

import pytest

import rst_code_example_pipeline.check_projects as cp
import rst_code_example_pipeline.extract_projects as ep
from rst_code_example_pipeline import blocks as _blocks_mod
import rst_code_example_pipeline.toolchain_info as info


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def restore_cwd():
    """Restore the working directory after each test (get_projects changes it)."""
    original = os.getcwd()
    yield
    os.chdir(original)


@pytest.fixture(autouse=True)
def reset_cp_globals():
    """Reset check_projects module-level globals before and after each test."""
    cp.verbose = False
    cp.all_diagnostics = False
    cp.max_columns = 0
    cp.force_checks = False
    yield
    cp.verbose = False
    cp.all_diagnostics = False
    cp.max_columns = 0
    cp.force_checks = False


def _make_minimal_block_info(project: str,
                             tmp_path,
                             subdir: str = "") -> str:
    """
    Write a minimal block_info.json for the given project into tmp_path (or a
    subdir of it) and return the absolute path to the JSON file.
    """
    # Ensure toolchain_info is initialised
    if not info.DEFAULT_VERSION:
        info.init_toolchain_info()

    block = _blocks_mod.CodeBlock(
        rst_file="test.rst",
        line_start=1,
        line_end=5,
        text="procedure Main is begin null; end Main;",
        language="ada",
        project=project,
        main_file=None,
        gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
        gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
        gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
        compiler_switches=["-gnata"],
        classes=["ada-nocheck"],
        manual_chop=False,
        buttons=["no"],
    )
    dest_dir = tmp_path / subdir if subdir else tmp_path
    dest_dir.mkdir(parents=True, exist_ok=True)
    json_file = str(dest_dir / "block_info.json")
    block.to_json_file(json_file)
    return json_file


# ---------------------------------------------------------------------------
# T-check_projects-01: get_blocks() with empty list
# ---------------------------------------------------------------------------

class TestGetBlocksEmpty:
    def test_empty_regex_list_returns_empty_dict(self):
        result = cp.get_blocks([])
        assert result == {}

    def test_return_type_is_dict(self):
        result = cp.get_blocks([])
        assert isinstance(result, dict)


# ---------------------------------------------------------------------------
# T-check_projects-02: get_blocks() with a valid block_info.json
# ---------------------------------------------------------------------------

class TestGetBlocksValid:
    def test_one_project_found(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        assert "MyProject" in result

    def test_project_entry_is_list(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        assert isinstance(result["MyProject"], list)

    def test_project_entry_has_one_tuple(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        assert len(result["MyProject"]) == 1

    def test_tuple_contains_codeblock_and_path(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        block, path = result["MyProject"][0]
        assert isinstance(block, _blocks_mod.CodeBlock)
        assert path == json_file

    def test_glob_pattern_finds_file(self, tmp_path):
        _make_minimal_block_info("GlobProject", tmp_path, subdir="subdir")
        pattern = str(tmp_path / "**" / "block_info.json")
        result = cp.get_blocks([pattern])
        assert "GlobProject" in result

    def test_two_projects_from_two_files(self, tmp_path):
        _make_minimal_block_info("Project1", tmp_path, subdir="p1")
        _make_minimal_block_info("Project2", tmp_path, subdir="p2")
        pattern = str(tmp_path / "**" / "block_info.json")
        result = cp.get_blocks([pattern])
        assert "Project1" in result
        assert "Project2" in result


# ---------------------------------------------------------------------------
# T-check_projects-03: get_blocks() with missing project field
# ---------------------------------------------------------------------------

class TestGetBlocksMissingProject:
    def test_missing_project_field_skipped(self, tmp_path, capsys):
        """A block_info.json whose block has project=None must be skipped."""
        # Ensure toolchain_info is initialised
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()

        block = _blocks_mod.CodeBlock(
            rst_file="test.rst",
            line_start=1,
            line_end=5,
            text="procedure Main is begin null; end Main;",
            language="ada",
            project=None,       # <-- no project
            main_file=None,
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=["ada-nocheck"],
            manual_chop=False,
            buttons=["no"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = cp.get_blocks([json_file])
        assert result == {}, "Block with project=None must be skipped"

    def test_missing_project_prints_error(self, tmp_path, capsys):
        """When project is None, an ERROR message must be printed."""
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()

        block = _blocks_mod.CodeBlock(
            rst_file="test.rst",
            line_start=1,
            line_end=5,
            text="stub",
            language="ada",
            project=None,
            main_file=None,
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=[],
            classes=[],
            manual_chop=False,
            buttons=["no"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        cp.get_blocks([json_file])
        captured = capsys.readouterr()
        assert "ERROR" in captured.out


# ---------------------------------------------------------------------------
# T-check_projects-04: get_projects() without projects_list_file
# ---------------------------------------------------------------------------

class TestGetProjectsNoPrjList:
    def test_empty_build_dir_returns_empty_dict(self, tmp_path):
        result = cp.get_projects(str(tmp_path), projects_list_file=None)
        assert result == {}

    def test_cwd_changed_to_build_dir(self, tmp_path):
        cp.get_projects(str(tmp_path), projects_list_file=None)
        # After the call, cwd should have been set to tmp_path by get_projects
        # (our restore_cwd fixture will reset it after the test, but within the
        # test we can verify it was changed)
        assert os.getcwd() == str(tmp_path)

    def test_block_info_in_build_dir_found(self, tmp_path):
        _make_minimal_block_info("AutoProject", tmp_path, subdir="projects/AutoProject/hash1")
        result = cp.get_projects(str(tmp_path), projects_list_file=None)
        assert "AutoProject" in result


# ---------------------------------------------------------------------------
# T-check_projects-05: get_projects() with projects_list_file
# ---------------------------------------------------------------------------

class TestGetProjectsWithPrjList:
    def test_with_valid_projects_list_returns_project(self, tmp_path):
        # Create a project directory and block_info.json
        project_name = "ListedProject"
        subdir = ep.get_project_dir(project_name) + "/hash123"
        _make_minimal_block_info(project_name, tmp_path, subdir=subdir)

        # Create a ProjectsList JSON
        pl = ep.ProjectsList()
        pl.add(project_name)
        prj_list_file = str(tmp_path / "projects.json")
        pl.to_json_file(prj_list_file)

        result = cp.get_projects(str(tmp_path), projects_list_file=prj_list_file)
        assert project_name in result

    def test_with_empty_projects_list_returns_empty(self, tmp_path):
        pl = ep.ProjectsList()
        prj_list_file = str(tmp_path / "empty_projects.json")
        pl.to_json_file(prj_list_file)

        result = cp.get_projects(str(tmp_path), projects_list_file=prj_list_file)
        assert result == {}

    def test_cwd_changed_to_build_dir_with_prj_list(self, tmp_path):
        prj_list_file = str(tmp_path / "projects.json")
        pl = ep.ProjectsList()
        pl.to_json_file(prj_list_file)

        cp.get_projects(str(tmp_path), projects_list_file=prj_list_file)
        assert os.getcwd() == str(tmp_path)

    def test_missing_prj_list_file_prints_warning(self, tmp_path, capsys):
        """When projects_list_file does not exist, from_json_file returns None
        and get_projects must print a WARNING."""
        missing_file = str(tmp_path / "no_such_projects.json")
        cp.get_projects(str(tmp_path), projects_list_file=missing_file)
        captured = capsys.readouterr()
        assert "WARNING" in captured.out


# ---------------------------------------------------------------------------
# T-check_projects-06: check_block() thin wrapper
# ---------------------------------------------------------------------------

class TestCheckBlockWrapper:
    def test_no_check_block_returns_false(self, tmp_path):
        """check_block() delegates to check_code_block.check_block(); a
        no-check block must return False (no error)."""
        json_file = _make_minimal_block_info("WrapProject", tmp_path)
        # Load the block from JSON (it has no_check=True from the ada-nocheck class)
        block = _blocks_mod.CodeBlock.from_json_file(json_file)
        assert block is not None
        os.chdir(str(tmp_path))
        result = cp.check_block(block, json_file)
        assert result is False


# ---------------------------------------------------------------------------
# T-check_projects-07: check_projects() integration
# ---------------------------------------------------------------------------

class TestCheckProjectsIntegration:
    def test_check_projects_with_nocheck_block_returns_false(self, tmp_path):
        """check_projects() iterates over all blocks in the build dir and calls
        check_block().  A build dir with only no-check blocks must return False."""
        subdir = "projects/MyProj/abc123"
        json_file = _make_minimal_block_info("MyProj", tmp_path, subdir=subdir)
        result = cp.check_projects(str(tmp_path), projects_list_file=None)
        assert result is False

    def test_check_projects_empty_build_dir_returns_false(self, tmp_path):
        """check_projects() on an empty build dir (no block_info.json files)
        must return False (no errors)."""
        result = cp.check_projects(str(tmp_path), projects_list_file=None)
        assert result is False


# ---------------------------------------------------------------------------
# T-check_projects-08: extended coverage — malformed JSON, verbose, inactive,
# duplicate project
# (covers check_projects.py lines 30-32, 38-40, 87-88, 93)
# ---------------------------------------------------------------------------

class TestCheckProjectsExtended:
    def test_get_blocks_from_json_file_returns_none(self, tmp_path, capsys, monkeypatch):
        """When from_json_file() returns None, get_blocks() prints ERROR and
        skips the entry (covers lines 30-32)."""
        # Write a valid block_info.json so iglob finds the file
        json_file = _make_minimal_block_info("NullProject", tmp_path)

        # Patch from_json_file to return None regardless of content
        monkeypatch.setattr(_blocks_mod.CodeBlock, "from_json_file",
                            staticmethod(lambda *args, **kwargs: None))

        result = cp.get_blocks([json_file])
        assert result == {}, "Expected empty dict when from_json_file returns None"
        out = capsys.readouterr().out
        assert "ERROR" in out, "Expected ERROR printed when block cannot be loaded"

    def test_get_blocks_duplicate_project(self, tmp_path):
        """Two block_info.json files with the same project name: the second hits
        the false branch of 'if not b.project in projects:' (lines 38-40)."""
        # Write two files for the same project in different subdirs
        _make_minimal_block_info("DupProject", tmp_path, subdir="a")
        _make_minimal_block_info("DupProject", tmp_path, subdir="b")
        pattern = str(tmp_path / "**" / "block_info.json")
        result = cp.get_blocks([pattern])
        # Both blocks are in the list under the same project key
        assert "DupProject" in result
        assert len(result["DupProject"]) == 2, \
            "Expected both blocks accumulated under the same project key"

    def test_get_projects_verbose(self, tmp_path, capsys):
        """check_projects() with verbose=True prints the project header
        (covers lines 87-88)."""
        subdir = "projects/VerbProj/abc123"
        _make_minimal_block_info("VerbProj", tmp_path, subdir=subdir)
        cp.verbose = True
        cp.check_projects(str(tmp_path), projects_list_file=None)
        out = capsys.readouterr().out
        assert "VerbProj" in out, \
            "Expected verbose project header to contain the project name"

    def test_check_projects_skips_inactive_block(self, tmp_path, monkeypatch):
        """A block with active=False is skipped by check_projects() without
        calling check_block() (covers line 93)."""
        # Build a block and serialise it with active=False
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()

        block = _blocks_mod.CodeBlock(
            rst_file="test.rst",
            line_start=1,
            line_end=5,
            text="procedure Main is begin null; end Main;",
            language="ada",
            project="InactiveProj",
            main_file=None,
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=["ada-nocheck"],
            manual_chop=False,
            buttons=["no"],
        )
        block.active = False  # mark inactive before serialising

        subdir = "projects/InactiveProj/hash000"
        dest_dir = tmp_path / subdir
        dest_dir.mkdir(parents=True, exist_ok=True)
        json_file = str(dest_dir / "block_info.json")
        block.to_json_file(json_file)

        # Track calls to check_block
        calls = []

        original_check_block = cp.check_block

        def tracking_check_block(blk, jf):
            calls.append(blk)
            return original_check_block(blk, jf)

        monkeypatch.setattr(cp, "check_block", tracking_check_block)

        result = cp.check_projects(str(tmp_path), projects_list_file=None)
        assert result is False, "Expected no error for inactive block"
        assert len(calls) == 0, \
            "check_block must NOT be called for an inactive block"
