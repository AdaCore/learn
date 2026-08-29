"""
Unit tests for rst_code_example_pipeline.extract_projects.

Covers:
- get_project_dir(): simple and dotted project names
- write_project_file(): all four combinations of spark_mode × main_file × compiler_switches
- ProjectsList: init, add(), to_json_file(), from_json_file() round-trip, missing file
- analyze_file(): minimal no-check / syntax-only Ada block
- analyze_file(): a block directory left over from a prior run whose info JSON file was
  deleted is detected as stale, logged, and removed rather than reused
- analyze_file() integration: compile_button / run_button / prove_button Ada blocks --
  the extracted source, the per-block directory name and the generated project files
  (requires the Ada toolchain — real gnatchop and write_project_file calls)
- analyze_file(): a block whose source text chops into zero source files is logged and
  skipped rather than crashing the whole analysis
- Global state (verbose, code_block_at, current_config) reset before each test

NOTE: a no-check block does not spare analyze_file() the toolchain.  The chop step runs
before the no-check test, and every block reaching it goes through the toolchain setup,
which writes into the toolchain installation tree.  Every analyze_file() test that
reaches the block loop therefore carries the `toolchain` marker; the only unmarked
analyze_file() tests are the two that return before that loop (a block without a
project, and a file whose blocks are all inactive).  The get_project_dir(),
write_project_file(), ProjectsList and Diag tests never call analyze_file() at all and
need no marker.
"""
import json
import os

import pytest

import rst_code_example_pipeline.extract_projects as ep


# ---------------------------------------------------------------------------
# T-extract_projects-01: get_project_dir()
# ---------------------------------------------------------------------------

class TestGetProjectDir:
    def test_simple_name(self):
        assert ep.get_project_dir("Simple") == "projects/Simple"

    def test_dotted_name_two_parts(self):
        assert ep.get_project_dir("Foo.Bar") == "projects/Foo/Bar"

    def test_dotted_name_three_parts(self):
        assert ep.get_project_dir("A.B.C") == "projects/A/B/C"

    def test_base_prefix_always_present(self):
        result = ep.get_project_dir("X")
        assert result.startswith("projects/")

    def test_no_trailing_slash(self):
        result = ep.get_project_dir("Foo")
        assert not result.endswith("/")


# ---------------------------------------------------------------------------
# T-extract_projects-02: write_project_file()
# ---------------------------------------------------------------------------

class TestWriteProjectFile:
    def test_no_main_no_switches_not_spark_creates_gpr(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=False)
        assert (work_dir / "main.gpr").exists()

    def test_no_main_no_switches_not_spark_creates_adc(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=False)
        assert (work_dir / "main.adc").exists()

    def test_returns_gpr_filename_not_spark(self, work_dir):
        result = ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=False)
        assert result == "main.gpr"

    def test_no_main_placeholder_absent_when_none(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=False)
        content = (work_dir / "main.gpr").read_text()
        assert "for Main use" not in content

    def test_with_main_file_gpr_contains_main_use(self, work_dir):
        ep.write_project_file(main_file="main.adb", compiler_switches=[], spark_mode=False)
        content = (work_dir / "main.gpr").read_text()
        assert 'for Main use ("main.adb")' in content

    def test_with_compiler_switch_gpr_contains_switch(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=["-gnatwa"], spark_mode=False)
        content = (work_dir / "main.gpr").read_text()
        assert '"-gnatwa"' in content

    def test_multiple_switches_all_present(self, work_dir):
        ep.write_project_file(
            main_file=None, compiler_switches=["-gnatwa", "-gnatwe"], spark_mode=False
        )
        content = (work_dir / "main.gpr").read_text()
        assert '"-gnatwa"' in content
        assert '"-gnatwe"' in content

    def test_spark_mode_creates_main_spark_gpr(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=True)
        assert (work_dir / "main_spark.gpr").exists()

    def test_spark_mode_creates_main_spark_adc(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=True)
        assert (work_dir / "main_spark.adc").exists()

    def test_spark_mode_returns_spark_gpr_filename(self, work_dir):
        result = ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=True)
        assert result == "main_spark.gpr"

    def test_spark_adc_contains_spark_mode_pragma(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=True)
        content = (work_dir / "main_spark.adc").read_text()
        assert "pragma SPARK_Mode (On);" in content

    def test_non_spark_adc_does_not_contain_spark_pragma(self, work_dir):
        ep.write_project_file(main_file=None, compiler_switches=[], spark_mode=False)
        content = (work_dir / "main.adc").read_text()
        assert "pragma SPARK_Mode" not in content

    def test_full_combo_main_switches_spark(self, work_dir):
        result = ep.write_project_file(
            main_file="main.adb", compiler_switches=["-gnatwa"], spark_mode=True
        )
        assert result == "main_spark.gpr"
        gpr = (work_dir / "main_spark.gpr").read_text()
        assert 'for Main use ("main.adb")' in gpr
        assert '"-gnatwa"' in gpr


# ---------------------------------------------------------------------------
# T-extract_projects-03: ProjectsList
# ---------------------------------------------------------------------------

class TestProjectsList:
    def test_init_no_args_empty_projects(self):
        pl = ep.ProjectsList()
        assert pl.projects == {}

    def test_init_with_projects_arg(self):
        pl = ep.ProjectsList(projects={"Foo": True})
        assert pl.projects == {"Foo": True}

    def test_add_project_appears_in_dict(self):
        pl = ep.ProjectsList()
        pl.add("MyProject")
        assert "MyProject" in pl.projects
        assert pl.projects["MyProject"] is True

    def test_add_multiple_projects(self):
        pl = ep.ProjectsList()
        pl.add("A")
        pl.add("B")
        assert set(pl.projects.keys()) == {"A", "B"}

    def test_to_json_file_creates_file(self, tmp_path):
        pl = ep.ProjectsList()
        pl.add("Foo")
        dest = str(tmp_path / "projects.json")
        pl.to_json_file(dest)
        assert os.path.isfile(dest)

    def test_to_json_file_content_is_valid_json(self, tmp_path):
        pl = ep.ProjectsList()
        pl.add("Bar")
        dest = str(tmp_path / "projects.json")
        pl.to_json_file(dest)
        with open(dest) as f:
            data = json.load(f)
        assert "projects" in data
        assert data["projects"]["Bar"] is True

    def test_round_trip_preserves_projects(self, tmp_path):
        pl = ep.ProjectsList()
        pl.add("Alpha")
        pl.add("Beta")
        dest = str(tmp_path / "roundtrip.json")
        pl.to_json_file(dest)
        pl2 = ep.ProjectsList.from_json_file(dest)
        assert pl2 is not None
        assert set(pl2.projects.keys()) == {"Alpha", "Beta"}

    def test_from_json_file_nonexistent_returns_none(self, tmp_path):
        result = ep.ProjectsList.from_json_file(str(tmp_path / "no_such.json"))
        assert result is None

    def test_to_json_file_overwrites_silently(self, tmp_path):
        pl1 = ep.ProjectsList()
        pl1.add("First")
        dest = str(tmp_path / "over.json")
        pl1.to_json_file(dest)

        pl2 = ep.ProjectsList()
        pl2.add("Second")
        pl2.to_json_file(dest)

        pl_loaded = ep.ProjectsList.from_json_file(dest)
        assert pl_loaded is not None
        assert "Second" in pl_loaded.projects
        assert "First" not in pl_loaded.projects


# ---------------------------------------------------------------------------
# T-extract_projects-04: analyze_file() — minimal no-check block
# ---------------------------------------------------------------------------

class TestAnalyzeFile:
    # A minimal RST file with a single Ada block marked as no-check.
    # The no-check class keeps analyze_file() from compiling or running the
    # block, but it is still chopped and still goes through the toolchain
    # setup, so these tests need the Ada toolchain all the same.
    # NOTE: analyze_file() requires every code block to have a project attribute;
    # blocks without one cause exit(1).  Always include project=... here.
    NOCHECK_RST = """\
.. code:: ada project=NoCheckProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""

    # A single Ada block whose chopping is made to yield nothing, so no source
    # file is ever written out for it.
    EMPTY_CHOP_RST = """\
.. code:: ada project=EmptyChopProject main=main.adb compile_button

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""

    def _write_rst(self, tmp_path, content: str) -> str:
        rst_path = tmp_path / "test_nocheck.rst"
        rst_path.write_text(content)
        return str(rst_path)

    @pytest.mark.toolchain
    def test_no_crash_on_nocheck_block(self, work_dir):
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        # analyze_file() must return without raising
        result = ep.analyze_file(rst_file)
        assert result is False

    @pytest.mark.toolchain
    def test_no_crash_on_nocheck_block_with_project(self, work_dir):
        rst_content = """\
.. code:: ada project=TestProj
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False

    @pytest.mark.toolchain
    def test_analyze_file_creates_project_dirs(self, work_dir):
        rst_content = """\
.. code:: ada project=MyProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        ep.analyze_file(rst_file)
        project_dir = work_dir / "projects" / "MyProject"
        assert project_dir.exists(), \
            f"Expected project directory {project_dir} to be created"

    @pytest.mark.toolchain
    def test_analyze_file_with_projects_list_file(self, work_dir):
        rst_content = """\
.. code:: ada project=ListedProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        prj_list_file = str(work_dir / "projects.json")
        ep.analyze_file(rst_file, prj_list_file)
        # The projects list JSON file must have been created
        assert os.path.isfile(prj_list_file), \
            "analyze_file() must write the projects list JSON file"
        with open(prj_list_file) as f:
            data = json.load(f)
        assert "projects" in data
        assert "ListedProject" in data["projects"]

    @pytest.mark.toolchain
    def test_analyze_file_verbose_existing_projects_list_file(self, work_dir, capsys):
        """verbose=True + extracted_projects_list_file pointing at a file that
        already exists prints the 'Extracted list of projects...' message."""
        prj_list = work_dir / "projects.json"
        prj_list.write_text('{"projects": {}}')
        ep.verbose = True
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        result = ep.analyze_file(rst_file, str(prj_list))
        assert result is False
        assert "Extracted list" in capsys.readouterr().out

    @pytest.mark.toolchain
    def test_analyze_file_verbose_missing_projects_list_file(self, work_dir, capsys):
        """verbose=True + extracted_projects_list_file pointing at a file that
        does not exist yet prints the 'will be created' message."""
        prj_list = work_dir / "new_projects.json"
        ep.verbose = True
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        result = ep.analyze_file(rst_file, str(prj_list))
        assert result is False
        assert "will be created" in capsys.readouterr().out

    @pytest.mark.toolchain
    def test_analyze_file_existing_projects_list_loaded(self, work_dir):
        # Pre-create a projects list JSON with an existing entry
        prj_list_file = str(work_dir / "projects.json")
        existing = ep.ProjectsList()
        existing.add("ExistingProject")
        existing.to_json_file(prj_list_file)

        rst_content = """\
.. code:: ada project=NewProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        ep.analyze_file(rst_file, prj_list_file)

        with open(prj_list_file) as f:
            data = json.load(f)
        # Both the pre-existing and the new project must be in the file
        assert "NewProject" in data["projects"], \
            "New project must be added to the existing projects list"

    @pytest.mark.toolchain
    def test_analyze_file_syntax_only_block(self, work_dir):
        rst_content = """\
.. code:: ada project=SyntaxProject
   :class: ada-syntax-only

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        # syntax_only blocks are still processed (no toolchain invocation needed
        # inside analyze_file for the project extraction phase)
        assert result is False

    def test_analyze_file_no_project_raises_system_exit(self, work_dir):
        """analyze_file() calls exit(1) when a block has no project attribute."""
        rst_content = """\
.. code:: ada
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        with pytest.raises(SystemExit):
            ep.analyze_file(rst_file)

    @pytest.mark.toolchain
    def test_analyze_file_no_button_block(self, work_dir):
        """A non-no-check, non-syntax-only block with buttons=["no"] reaches
        the project extraction path and writes block_info.json without error."""
        rst_content = """\
.. code:: ada project=NoBtnProject no_button

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False

    @pytest.mark.toolchain
    def test_analyze_file_config_block(self, work_dir):
        """A :code-config: line produces a ConfigBlock; analyze_file() must handle
        it (via isinstance check) without crashing."""
        rst_content = """\
:code-config:`run_button=False;prove_button=True;accumulate_code=False`

.. code:: ada project=CfgProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False

    @pytest.mark.toolchain
    def test_analyze_file_manual_chop_block(self, work_dir):
        """A C block uses manual_chop=True; analyze_file() must call manual_chop
        (not real_gnatchop) and succeed."""
        rst_content = """\
.. code:: c project=CProject no_button

   !main.c
   int main(void) { return 0; }

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False

    @pytest.mark.toolchain
    def test_code_block_at_matches_one_block(self, work_dir):
        """code_block_at set to a value inside a block's (line_start, line_end)
        range: that block stays active, the true branch of the code_block_at
        match."""
        ep.code_block_at = 4
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        result = ep.analyze_file(rst_file)
        assert result is False
        # The block stayed active, so its project directory must exist.
        assert (work_dir / "projects" / "NoCheckProject").exists()

    def test_code_block_at_sets_inactive(self, work_dir, capsys):
        """Set code_block_at to a value that matches no block — all blocks stay
        inactive and the inner loop skips all of them via the inactive-block continue path."""
        # code_block_at=9999 is far beyond any line in the small RST fixture
        ep.code_block_at = 9999
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        result = ep.analyze_file(rst_file)
        assert result is False
        # No project directory should have been created (all blocks inactive)
        assert not (work_dir / "projects" / "NoCheckProject").exists(), \
            "No project dir expected when all blocks are inactive"

    @pytest.mark.toolchain
    def test_verbose_prints_headers(self, work_dir, capsys):
        """Set verbose=True and confirm that project header lines are printed."""
        ep.verbose = True
        rst_content = """\
.. code:: ada project=VerboseProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        ep.analyze_file(rst_file)
        out = capsys.readouterr().out
        # The verbose header and block count line should appear
        assert "VerboseProject" in out, \
            "Expected project name in verbose output"

    @pytest.mark.toolchain
    def test_second_call_same_project_logs_exists(self, work_dir, capsys):
        """Call analyze_file() twice with the same project; the second call
        must print 'already exists' when verbose=True."""
        ep.verbose = True
        rst_content = """\
.. code:: ada project=RepeatedProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        ep.analyze_file(rst_file)  # first call: creates the project dir
        # reset verbose (it gets cleared by the autouse fixture between tests,
        # but we are in one test so set it again for the second call)
        ep.verbose = True
        capsys.readouterr()  # discard first-call output
        ep.analyze_file(rst_file)  # second call: dir already exists
        out = capsys.readouterr().out
        assert "already exists" in out, \
            "Expected 'already exists' in verbose output on second call"

    @pytest.mark.toolchain
    def test_stale_block_dir_missing_json_is_removed_and_recreated(self, work_dir, capsys):
        """If a code block's per-block directory already exists from a prior
        run but its info JSON file has since been deleted, the directory must
        be treated as stale: logged and removed rather than reused, and the
        analysis must complete without crashing."""
        rst_content = """\
.. code:: ada project=StaleProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""
        rst_file = self._write_rst(work_dir, rst_content)
        ep.analyze_file(rst_file)  # first call: creates the block's info JSON

        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) == 1, \
            f"Expected exactly 1 block_info.json after the first call; found {len(block_jsons)}"
        block_jsons[0].unlink()

        capsys.readouterr()  # discard first-call output
        result = ep.analyze_file(rst_file)  # second call: block dir is stale
        assert result is False

        out = capsys.readouterr().out
        assert "no JSON info file" in out, \
            "Expected the stale-directory message when the info JSON is missing"

    @pytest.mark.toolchain
    def test_no_check_verbose_skip(self, work_dir, capsys):
        """With verbose=True a no-check block must print a 'Skipping' message."""
        ep.verbose = True
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        ep.analyze_file(rst_file)
        out = capsys.readouterr().out
        assert "Skipping" in out, \
            "Expected 'Skipping' message for no-check block in verbose mode"

    @pytest.mark.toolchain
    def test_chopper_returning_no_source_files_is_reported(
            self, work_dir, monkeypatch, capsys):
        """A block whose source text chops to nothing must be reported.

        Two distinct messages are printed, one from the immediate failure site
        and one from the surrounding handler that moves on to the next block,
        and the block itself is still logged so the remaining blocks get their
        turn.

        The overall result the same run must report is covered by the
        companion ``xfail`` test below; the two are kept apart so that losing
        these messages fails the suite on its own."""
        monkeypatch.setattr(ep, "real_gnatchop", lambda *a, **kw: [])

        rst_file = self._write_rst(work_dir, self.EMPTY_CHOP_RST)
        ep.analyze_file(rst_file)

        out = capsys.readouterr().out
        assert "Failed to chop example" in out, \
            "Expected the immediate failure message when chopping yields nothing"
        assert "Error while updating code for the block, continuing with next one!" in out, \
            "Expected the surrounding handler to report that it moves on"
        assert list(work_dir.rglob("block_info.json")), \
            "Expected the failing block to still be logged before moving on"

    @pytest.mark.toolchain
    @pytest.mark.xfail(
        strict=True,
        reason="the error flag raised when a block cannot be chopped is set on "
               "a nested function's local, so analyze_file() still reports success",
    )
    def test_chopper_returning_no_source_files_fails_the_run(
            self, work_dir, monkeypatch):
        """A block whose source text chops to nothing must fail the analysis.

        Chopping producing no source files at all means the block's code was
        never written out, so the run cannot be called successful. The block
        itself is still logged and skipped so the remaining blocks get their
        turn, and the companion test above covers the diagnostics printed
        along the way; the overall result, though, must report an error.

        Tracking note — this currently fails. The failure site assigns the
        analysis-error flag inside a nested helper function, which makes it a
        fresh local of that helper instead of updating the flag
        ``analyze_file()`` eventually returns, so the run reports success and
        the caller's exit code stays zero. The same site also re-raises with no
        exception in flight, which turns the real diagnostic into Python's
        ``No active exception to reraise`` message. A fix would declare the
        flag ``nonlocal`` (and raise a real exception carrying the reason);
        this test then passes and the ``xfail`` marker must be removed."""
        monkeypatch.setattr(ep, "real_gnatchop", lambda *a, **kw: [])

        rst_file = self._write_rst(work_dir, self.EMPTY_CHOP_RST)
        assert ep.analyze_file(rst_file) is True, \
            "a per-block chopping failure must surface as an overall error"


# ---------------------------------------------------------------------------
# T-extract_projects-05: Diag class
# ---------------------------------------------------------------------------

class TestDiag:
    def test_fields_stored(self):
        d = ep.Diag("f.adb", 3, 7, "error message")
        assert d.file == "f.adb"
        assert d.line == 3
        assert d.col == 7
        assert d.msg == "error message"

    def test_repr_format(self):
        d = ep.Diag("f.adb", 3, 7, "error message")
        assert repr(d) == "f.adb:3:7: error message"

    def test_repr_edge_case_zero_and_empty(self):
        d = ep.Diag("", 0, 0, "")
        assert repr(d) == ":0:0: "


# ---------------------------------------------------------------------------
# T-extract_projects-06: same-project second block
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestAnalyzeFileSameProjectTwoBlocks:
    TWO_BLOCKS_RST = """\
.. code:: ada project=SameProject
   :class: ada-nocheck

   procedure Main is
   begin
      null;
   end Main;

First explanatory paragraph.

.. code:: ada project=SameProject
   :class: ada-nocheck

   procedure Helper is
   begin
      null;
   end Helper;

Second explanatory paragraph.
"""

    def _write_rst(self, tmp_path, content: str) -> str:
        rst_path = tmp_path / "two_blocks.rst"
        rst_path.write_text(content)
        return str(rst_path)

    def test_two_blocks_same_project(self, work_dir):
        """Two no-check Ada blocks with the same project= attribute: the second
        block hits the false branch of 'if not b.project in projects:'."""
        rst_file = self._write_rst(work_dir, self.TWO_BLOCKS_RST)
        result = ep.analyze_file(rst_file)
        assert result is False
        # The project directory must have been created
        assert (work_dir / "projects" / "SameProject").exists()
        # Two separate block_info.json files must exist (each block has its own
        # hash-named subdirectory)
        block_jsons = list((work_dir / "projects" / "SameProject").rglob("block_info.json"))
        assert len(block_jsons) == 2, \
            f"Expected 2 block_info.json files; found {len(block_jsons)}"


# ---------------------------------------------------------------------------
# C4 — TestAnalyzeFileIntegration
# analyze_file() with compile_button / run_button / prove_button Ada blocks.
# Requires the Ada toolchain (real gnatchop called for non-no-check blocks).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestAnalyzeFileIntegration:
    """Integration tests for analyze_file() with real Ada compilation paths.

    Each RST fixture uses a valid Ada ``procedure Main`` body so that
    real_gnatchop can parse it into exactly one source file.  The block
    attributes (compile_button / run_button / prove_button) set compile_it /
    run_it / prove_it on the parsed CodeBlock.
    """

    # A minimal but valid Ada procedure that gnatchop can chop into one file.
    _ADA_BODY = """\
procedure Main is
begin
   null;
end Main;"""

    # A C block asking for a prove button: proving is Ada-only, so this is a
    # malformed example.
    _C_PROVE_RST = (
        ".. code:: c project=TestCProve prove_button\n\n"
        "   !main.c\n"
        "   int main(void) { return 0; }\n\n"
        "Explanatory paragraph.\n"
    )

    # A compile/run-eligible Ada block declaring no button indicator at all,
    # not even no_button.
    _NO_BUTTONS_RST = """\
.. code:: ada project=TestNoBtns main=main.adb

   procedure Main is
   begin
      null;
   end Main;

Explanatory paragraph.
"""

    @staticmethod
    def _write_rst(work_dir, content: str, name: str = "test_integration.rst") -> str:
        rst_path = work_dir / name
        rst_path.write_text(content)
        return str(rst_path)

    @staticmethod
    def _block_dir(work_dir, project: str):
        """Return the single per-block directory written for ``project``.

        Every block gets its own directory below the project, named after the
        short hash of its text so that two blocks cannot collide; ``latest``
        is the staging copy and is not one of them."""
        project_dir = work_dir / "projects" / project
        block_dirs = sorted(d for d in project_dir.iterdir()
                            if d.is_dir() and d.name != "latest")
        assert len(block_dirs) == 1, \
            "expected exactly one per-block directory, got {}".format(
                [d.name for d in block_dirs])
        return block_dirs[0]

    @staticmethod
    def _block_info(block_dir) -> dict:
        return json.loads((block_dir / "block_info.json").read_text())

    def test_analyze_file_compile_button(self, work_dir):
        """RST with a compile_button Ada block: analyze_file() must call
        real_gnatchop, write the project file, write block_info.json, and
        return False (no error)."""
        rst_content = (
            ".. code:: ada project=TestCompile main=main.adb compile_button\n"
            "\n"
            + "\n".join("   " + line for line in self._ADA_BODY.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False, \
            "analyze_file() must return False for a valid compile_button block"

        block_dir = self._block_dir(work_dir, "TestCompile")
        info = self._block_info(block_dir)
        assert block_dir.name == info["text_hash_short"], \
            "the block directory must be named after the block's short hash"
        # The chopped source is what the compiler will see, so it must be the
        # author's code, unchanged and un-reindented.
        assert (block_dir / "main.adb").read_text() == self._ADA_BODY
        assert info["source_files"] == ["main.adb"]
        assert info["project_filename"] == "main.gpr"
        assert info["spark_project_filename"] is None, \
            "no SPARK project may be written for a block that is not proved"
        # A compile button alone is not runnable, so no main is selected and
        # the generated project must not name one.
        assert info["project_main_file"] is None
        assert "for Main use" not in (block_dir / "main.gpr").read_text()

    def test_analyze_file_run_button(self, work_dir):
        """RST with a run_button Ada block: analyze_file() must call
        real_gnatchop, write the project file, write block_info.json, and
        return False (no error)."""
        rst_content = (
            ".. code:: ada project=TestRun main=main.adb run_button\n"
            "\n"
            + "\n".join("   " + line for line in self._ADA_BODY.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False, \
            "analyze_file() must return False for a valid run_button block"

        block_dir = self._block_dir(work_dir, "TestRun")
        info = self._block_info(block_dir)
        assert (block_dir / "main.adb").read_text() == self._ADA_BODY
        assert info["source_files"] == ["main.adb"]
        assert info["project_filename"] == "main.gpr"
        assert info["spark_project_filename"] is None
        # A runnable block selects a main, and the project must name it or
        # there is nothing for the builder to link.
        assert info["project_main_file"] == "main.adb"
        assert 'for Main use ("main.adb");' in (block_dir / "main.gpr").read_text()

    def test_analyze_file_prove_button(self, work_dir):
        """RST with a prove_button SPARK Ada block: analyze_file() must call
        real_gnatchop, write the SPARK project file, write block_info.json, and
        return False (no error)."""
        spark_body = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;"""
        rst_content = (
            ".. code:: ada project=TestProve main=main.adb prove_button\n"
            "\n"
            + "\n".join("   " + line for line in spark_body.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False, \
            "analyze_file() must return False for a valid prove_button block"

        block_dir = self._block_dir(work_dir, "TestProve")
        info = self._block_info(block_dir)
        assert (block_dir / "main.adb").read_text() == spark_body
        assert info["source_files"] == ["main.adb"]
        # A prove button alone builds only the SPARK project.
        assert info["spark_project_filename"] == "main_spark.gpr"
        assert info["project_filename"] is None
        assert not (block_dir / "main.gpr").exists()
        # GNATprove only treats the unit as SPARK because of this pragma.
        assert "pragma SPARK_Mode (On);" in (block_dir / "main_spark.adc").read_text()

    def test_analyze_file_run_button_no_main(self, work_dir):
        """RST with run_button and no main= attribute: get_main_filename()
        falls back to using the chopped source file as the main file."""
        rst_content = (
            ".. code:: ada project=TestRunNoMain run_button\n"
            "\n"
            + "\n".join("   " + line for line in self._ADA_BODY.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False, \
            "analyze_file() must return False for a run_button block with no main="

        block_dir = self._block_dir(work_dir, "TestRunNoMain")
        info = self._block_info(block_dir)
        assert info["main_file"] is None, \
            "the fixture must not declare a main= attribute, or the fallback " \
            "this test exists for is never taken"
        # With nothing declared, the last chopped source becomes the main file.
        assert info["source_files"] == ["main.adb"]
        assert info["project_main_file"] == "main.adb"
        assert 'for Main use ("main.adb");' in (block_dir / "main.gpr").read_text()

    def test_analyze_file_prove_and_run_button(self, work_dir):
        """RST with both prove_button and run_button: the main file is
        resolved via get_main_filename() inside the prove_it handling as well
        as the compile_it handling, and both project files are written."""
        spark_body = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;"""
        rst_content = (
            ".. code:: ada project=TestProveRun prove_button run_button\n\n"
            + "\n".join("   " + line for line in spark_body.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False, \
            "analyze_file() must return False for a valid prove_button+run_button block"

        block_dir = self._block_dir(work_dir, "TestProveRun")
        info = self._block_info(block_dir)
        assert (block_dir / "main.adb").read_text() == spark_body
        # Both projects are written, and both must name the resolved main file.
        assert info["project_filename"] == "main.gpr"
        assert info["spark_project_filename"] == "main_spark.gpr"
        assert info["main_file"] is None
        assert info["project_main_file"] == "main.adb"
        for gpr in ("main.gpr", "main_spark.gpr"):
            assert 'for Main use ("main.adb");' in (block_dir / gpr).read_text(), \
                "{} must name the main file".format(gpr)
        assert "pragma SPARK_Mode (On);" in (block_dir / "main_spark.adc").read_text()

    def test_analyze_file_c_prove_button_reports_the_wrong_language(
            self, work_dir, capsys):
        """A prove button on a C block must be reported as a wrong language.

        Proving is Ada-only, so a C block asking for a prove button is a
        malformed example, and the run must name the problem.

        The overall result the same run must report is covered by the
        companion ``xfail`` test below; the two are kept apart so that losing
        this message fails the suite on its own."""
        rst_file = self._write_rst(work_dir, self._C_PROVE_RST)
        ep.analyze_file(rst_file)
        assert "Wrong language selected for prove button" in capsys.readouterr().out, \
            "Expected the wrong-language message for a prove button on a C block"

    @pytest.mark.xfail(
        strict=True,
        reason="the per-block error flag is never merged into analyze_file()'s "
               "return value, so a prove button on a non-Ada block reports success",
    )
    def test_analyze_file_c_prove_button_fails_the_run(self, work_dir):
        """A prove button on a C block must fail the analysis.

        Proving is Ada-only, so a C block asking for a prove button is a
        malformed example: the message is printed — the companion test above
        covers that — and the run must report an error so the caller's exit
        code reflects it.

        Tracking note — this currently fails, and so does the sibling
        ``xfail`` test covering a block that carries no button indicator at
        all: both paths set the same per-block error flag, which is written
        but never read. Nothing merges it into the value ``analyze_file()``
        returns, so the run reports success and a broken example passes
        unnoticed. One fix — folding the per-block flag into the overall
        analysis result — closes both; when it lands, both tests pass and
        both ``xfail`` markers must be removed."""
        rst_file = self._write_rst(work_dir, self._C_PROVE_RST)
        assert ep.analyze_file(rst_file) is True, \
            "a prove button on a non-Ada block must surface as an overall error"

    def test_analyze_file_no_buttons_block_is_reported(self, work_dir, capsys):
        """A compile/run-eligible block with no button indicator must be
        reported.

        Every such block is expected to declare at least a no_button
        indicator, so a block declaring none is a malformed example, and the
        run must name the problem.

        The overall result the same run must report is covered by the
        companion ``xfail`` test below; the two are kept apart so that losing
        this message fails the suite on its own."""
        rst_file = self._write_rst(work_dir, self._NO_BUTTONS_RST)
        ep.analyze_file(rst_file)
        assert "Expected at least" in capsys.readouterr().out, \
            "Expected the missing-indicator message for a block with no buttons"

    @pytest.mark.xfail(
        strict=True,
        reason="the per-block error flag is never merged into analyze_file()'s "
               "return value, so a block carrying no button indicator reports success",
    )
    def test_analyze_file_no_buttons_block_fails_the_run(self, work_dir):
        """A compile/run-eligible block with no button indicator must fail the
        analysis.

        Every such block is expected to declare at least a no_button
        indicator, so a block declaring none is a malformed example: the
        message is printed — the companion test above covers that — and the
        run must report an error so the caller's exit code reflects it.

        Tracking note — this currently fails, for the same reason as the
        sibling ``xfail`` test covering a prove button on a C block. Both
        paths set the same per-block error flag, which is written but never
        read: nothing merges it into the value ``analyze_file()`` returns, so
        the run reports success and a broken example passes unnoticed. One
        fix — folding the per-block flag into the overall analysis result —
        closes both; when it lands, both tests pass and both ``xfail``
        markers must be removed."""
        rst_file = self._write_rst(work_dir, self._NO_BUTTONS_RST)
        assert ep.analyze_file(rst_file) is True, \
            "a block with no button indicator must surface as an overall error"
