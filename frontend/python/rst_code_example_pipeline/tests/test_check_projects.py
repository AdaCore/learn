"""
Unit tests for rst_code_example_pipeline.check_projects.

Covers:
- get_blocks([]) → empty dict
- get_blocks() with a valid block_info.json present → dict with one project entry
- get_blocks() with a block_info.json missing the project field → skips, dict empty
- get_projects(build_dir, projects_list_file=None) with no JSON files → empty dict
- get_projects(build_dir, projects_list_file) with a valid projects-list JSON
- cwd side effect: get_projects calls os.chdir(build_dir) — fixture saves/restores cwd
- check_projects() returns True when a block fails to compile (requires the Ada toolchain)
- a block dropped before it is checked -- its info file unreadable, or naming no
  project -- fails the run, each arm pinned separately, and neither costs the
  remaining blocks their check
"""
import os

import pytest

import rst_code_example_pipeline.check_projects as cp
import rst_code_example_pipeline.extract_projects as ep
from rst_code_example_pipeline import blocks as _blocks_mod
import rst_code_example_pipeline.toolchain_info as info


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

def _write_block_record(block, directory) -> str:
    """Write a block's record into ``directory`` under the name the package
    chooses for it, and hand back the path it landed at.

    The reader builds its search pattern from its own default name, so a test
    that spelled the name out here would be restating a choice the package is
    free to change -- and would keep passing if writer and reader ever drifted
    apart, since both sides of the seam would have been replaced by the test's
    own copy of the name.
    """
    directory.mkdir(parents=True, exist_ok=True)
    original_cwd = os.getcwd()
    os.chdir(str(directory))
    try:
        block.to_json_file()
    finally:
        os.chdir(original_cwd)
    written = sorted(directory.glob("*.json"))
    assert len(written) == 1, \
        "expected exactly one block record to be written, got {}".format(
            [path.name for path in written])
    return str(written[0])


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
    return _write_block_record(block, dest_dir)


# ---------------------------------------------------------------------------
# T-check_projects-01: get_blocks() with empty list
# ---------------------------------------------------------------------------

class TestGetBlocksEmpty:
    def test_empty_regex_list_returns_empty_dict(self):
        result = cp.get_blocks([])
        assert result == {}


# ---------------------------------------------------------------------------
# T-check_projects-02: get_blocks() with a valid block_info.json
# ---------------------------------------------------------------------------

class TestGetBlocksValid:
    def test_one_project_found(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        assert "MyProject" in result

    def test_project_entry_has_one_tuple(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        # A list, not just any sized container: callers append to it as further
        # block files for the same project are found.
        entry = result["MyProject"]
        assert isinstance(entry, list)
        assert len(entry) == 1

    def test_tuple_contains_codeblock_and_path(self, tmp_path):
        json_file = _make_minimal_block_info("MyProject", tmp_path)
        result = cp.get_blocks([json_file])
        block, path = result["MyProject"][0]
        assert isinstance(block, _blocks_mod.CodeBlock)
        assert path == json_file

    def test_glob_pattern_finds_file(self, tmp_path):
        written = _make_minimal_block_info("GlobProject", tmp_path, subdir="subdir")
        pattern = str(tmp_path / "**" / os.path.basename(written))
        result = cp.get_blocks([pattern])
        assert "GlobProject" in result

    def test_two_projects_from_two_files(self, tmp_path):
        written = _make_minimal_block_info("Project1", tmp_path, subdir="p1")
        _make_minimal_block_info("Project2", tmp_path, subdir="p2")
        pattern = str(tmp_path / "**" / os.path.basename(written))
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

@pytest.mark.toolchain
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
    @pytest.mark.toolchain
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
# ---------------------------------------------------------------------------

class TestCheckProjectsExtended:
    def test_get_blocks_from_json_file_returns_none(self, tmp_path, capsys, monkeypatch):
        """When from_json_file() returns None, get_blocks() prints ERROR and
        skips the entry (exercises the None-block error path in get_blocks)."""
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
        """Two block_info.json files with the same project name: the second block
        appends to the existing project entry rather than creating a new key."""
        # Write two files for the same project in different subdirs
        written = _make_minimal_block_info("DupProject", tmp_path, subdir="a")
        _make_minimal_block_info("DupProject", tmp_path, subdir="b")
        pattern = str(tmp_path / "**" / os.path.basename(written))
        result = cp.get_blocks([pattern])
        # Both blocks are in the list under the same project key
        assert "DupProject" in result
        assert len(result["DupProject"]) == 2, \
            "Expected both blocks accumulated under the same project key"

    @pytest.mark.toolchain
    def test_get_projects_verbose(self, tmp_path, capsys):
        """check_projects() with verbose=True prints the project header
        (exercises the verbose header output path)."""
        subdir = "projects/VerbProj/abc123"
        _make_minimal_block_info("VerbProj", tmp_path, subdir=subdir)
        cp.verbose = True
        cp.check_projects(str(tmp_path), projects_list_file=None)
        out = capsys.readouterr().out
        assert "VerbProj" in out, \
            "Expected verbose project header to contain the project name"

    def test_check_projects_skips_inactive_block(self, tmp_path, monkeypatch):
        """A block marked inactive must be skipped by check_projects()
        without being checked at all."""
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

        dest_dir = tmp_path / "projects" / "InactiveProj" / "hash000"
        json_file = _write_block_record(block, dest_dir)

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


# ---------------------------------------------------------------------------
# C5 — TestCheckProjectsReturnsTrue
# check_projects() must return True when check_block() returns True for a block.
# Requires the Ada toolchain (gprbuild invoked for a failing compile).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckProjectsReturnsTrue:
    """Tests that check_projects() propagates check_error=True."""

    BAD_ADA_SOURCE = "procedure Bad is\nbegin\n   SYNTAX ERROR HERE!!!\nend Bad;\n"

    def test_check_projects_returns_true_on_check_error(self, tmp_path):
        """Set up a block_info.json with Ada source that fails to compile.
        check_projects() must return True when check_block() reports an error."""
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()

        # Write a bad Ada source file so gprbuild will fail
        src = tmp_path / "bad.adb"
        src.write_text(self.BAD_ADA_SOURCE)

        # Change to tmp_path so write_project_file creates files there
        original_cwd = os.getcwd()
        os.chdir(str(tmp_path))

        project_filename = ep.write_project_file(
            main_file="bad.adb",
            compiler_switches=[],
            spark_mode=False,
        )

        # Build a CodeBlock that will trigger a compile attempt
        block = _blocks_mod.CodeBlock(
            rst_file="test.rst",
            line_start=1,
            line_end=5,
            text=self.BAD_ADA_SOURCE,
            language="ada",
            project="FailProject",
            main_file="bad.adb",
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=[],
            classes=[],
            manual_chop=False,
            buttons=["compile"],
            compile_it=True,
            run_it=False,
            syntax_only=False,
            no_check=False,
            source_files=["bad.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "bad.adb"

        # Place the block_info.json in a subdirectory matching check_projects expectations
        subdir = tmp_path / "projects" / "FailProject" / "hash001"
        subdir.mkdir(parents=True, exist_ok=True)

        # Copy the project files into the subdir (check_block os.chdir's into json_file's dir)
        import shutil
        shutil.copy(str(tmp_path / project_filename), str(subdir / project_filename))
        shutil.copy(str(tmp_path / "bad.adb"), str(subdir / "bad.adb"))
        # Take the configuration pragma files from what write_project_file
        # actually wrote, rather than naming one the package chose.
        for adc in tmp_path.glob("*.adc"):
            shutil.copy(str(adc), str(subdir / adc.name))

        json_file = _write_block_record(block, subdir)

        os.chdir(original_cwd)

        # Force checks to bypass any cached result
        cp.force_checks = True
        result = cp.check_projects(str(tmp_path), projects_list_file=None)
        assert result is True, \
            "check_projects() must return True when a block fails to compile"


# ---------------------------------------------------------------------------
# Blocks that are dropped before they are ever checked
# ---------------------------------------------------------------------------

class TestABlockThatWasNotCheckedFailsTheRun:
    """The two ways a block info file is dropped from the gathering loop.

    One is a file that cannot be turned into a block; the other is a block
    that names no project.  Each prints an ERROR line of its own and moves on
    to the next file, so neither reaches the checking loop and neither can
    contribute an error from there.

    What is asserted here is the outcome of the whole run, not the ERROR line
    -- the line was already printed while the run still came back clean, and
    a run that reports a problem and then says it went fine is the failure
    this suite exists to catch.  A build gates on the outcome and nothing
    else.

    The two arms are pinned separately.  They reach the run's outcome through
    the same list, so a single test would keep passing with either one of
    them disconnected.
    """

    C_SOURCE = "int main(void) { return 0; }\n"

    def _a_readable_block(self, tmp_path, project: str, subdir: str) -> str:
        """A block info file that reads back as a block, in its own
        directory below the build directory."""
        return _make_minimal_block_info(project, tmp_path, subdir=subdir)

    def _an_unreadable_block(self, tmp_path, subdir: str) -> str:
        """A block info file that cannot be turned into a block.

        Written by writing a real one first and then overwriting its text, so
        that the file ends up under the name the gathering loop looks for
        without that name being spelled out here.
        """
        written = _make_minimal_block_info("Unreadable", tmp_path,
                                           subdir=subdir)
        with open(written, "w") as f:
            f.write("{ this is not a block record")
        return written

    def _a_block_naming_no_project(self, tmp_path, subdir: str) -> str:
        """A block info file that reads back as a block naming no project.

        The extraction step refuses to write one, so this stands for a file
        that was edited or written by hand afterwards.
        """
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()

        block = _blocks_mod.CodeBlock(
            rst_file="test.rst",
            line_start=1,
            line_end=5,
            text="procedure Main is begin null; end Main;",
            language="ada",
            project=None,
            main_file=None,
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=["ada-nocheck"],
            manual_chop=False,
            buttons=["no"],
        )
        return _write_block_record(block, tmp_path / subdir)

    @staticmethod
    def _recording_checker(monkeypatch, fails=()):
        """Stand in for the per-block check and record what it was given.

        Which blocks survive the gathering loop and reach the check is what
        these tests are about; what a real check would then do to them is
        not, and running one would need the toolchain for no gain.  Blocks
        are recorded by their own file rather than by their project, so that
        two blocks of one project can be told apart.  ``fails`` names the
        block files whose check reports an error.
        """
        checked = []

        def recording_check_block(block, json_file):
            checked.append(json_file)
            return json_file in fails

        monkeypatch.setattr(cp, "check_block", recording_check_block)
        return checked

    def test_an_unreadable_block_info_file_fails_the_run(self, tmp_path):
        """A build directory whose one block info file cannot be read must
        fail the run.

        Nothing was checked, so a run that came back clean would be reporting
        success over an example no one looked at.
        """
        self._an_unreadable_block(tmp_path, "projects/Unreadable/hash1")

        assert cp.check_projects(str(tmp_path), projects_list_file=None) \
            is True, \
            "a block info file that could not be read must fail the run"

    def test_a_block_naming_no_project_fails_the_run(self, tmp_path):
        """A build directory whose one block info file names no project must
        fail the run, for the same reason: that block was never checked."""
        self._a_block_naming_no_project(tmp_path, "projects/NoProject/hash1")

        assert cp.check_projects(str(tmp_path), projects_list_file=None) \
            is True, \
            "a block that names no project must fail the run"

    def test_a_build_directory_of_readable_blocks_still_succeeds(
            self, tmp_path, monkeypatch):
        """Two blocks that read back and check out must come back clean.

        The control for the two tests above: without it they would still pass
        if the run had simply started failing for everything.
        """
        checked = self._recording_checker(monkeypatch)
        readable = [
            self._a_readable_block(tmp_path, "First", "projects/First/hash1"),
            self._a_readable_block(tmp_path, "Second", "projects/Second/hash2"),
        ]

        assert cp.check_projects(str(tmp_path), projects_list_file=None) \
            is False, \
            "a build directory whose blocks all read back and check out must " \
            "not fail the run"
        assert sorted(checked) == sorted(readable), \
            "both blocks must have been checked: {}".format(sorted(checked))

    def test_the_other_blocks_are_still_checked(self, tmp_path, monkeypatch):
        """A block info file that cannot be read must not cost the other
        blocks their check.

        This is the property that decides whether reporting the file instead
        of raising on it was an improvement at all.  The exception it replaced
        left the gathering loop before a single block had been handed to the
        checker, so a build directory like this one had none of its examples
        checked -- and the run still stopped, which is the only part that was
        ever visible.
        """
        checked = self._recording_checker(monkeypatch)
        self._an_unreadable_block(tmp_path, "projects/Unreadable/hash1")
        readable = [
            self._a_readable_block(tmp_path, "First", "projects/First/hash2"),
            self._a_readable_block(tmp_path, "Second", "projects/Second/hash3"),
        ]

        assert cp.check_projects(str(tmp_path), projects_list_file=None) \
            is True, \
            "the unreadable file must still fail the run"
        assert sorted(checked) == sorted(readable), \
            "every block that could be read must still have been checked: " \
            "{}".format(sorted(checked))

    def test_a_block_that_fails_does_not_stop_the_ones_after_it(
            self, tmp_path, monkeypatch):
        """A block whose check reports an error must not stop the blocks
        after it from being checked.

        The other half of the same property: three of the fixes on this
        branch turn an exception raised from inside the check of one block
        into a reported failure, and an exception there would have taken the
        remaining blocks with it just as surely as one raised while gathering
        them.

        Two blocks of one project report an error and a block of another
        project does not, so whichever of the two the check reaches first,
        both loops it runs -- the one over a project's blocks and the one
        over the projects -- still have a block left to visit after a
        failure.  The order the block files are found in is the file
        system's, so this cannot be arranged by putting the failing one
        first.
        """
        failing = [
            self._a_readable_block(tmp_path, "Shared", "projects/Shared/hash1"),
            self._a_readable_block(tmp_path, "Shared", "projects/Shared/hash2"),
        ]
        passing = self._a_readable_block(tmp_path, "Other",
                                         "projects/Other/hash3")
        checked = self._recording_checker(monkeypatch, fails=tuple(failing))

        assert cp.check_projects(str(tmp_path), projects_list_file=None) \
            is True, \
            "a block whose check reports an error must fail the run"
        assert sorted(checked) == sorted(failing + [passing]), \
            "every block must have been checked, whatever the ones before it " \
            "reported: {}".format(sorted(checked))
