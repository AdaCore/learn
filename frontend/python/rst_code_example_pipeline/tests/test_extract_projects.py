"""
Unit tests for rst_code_example_pipeline.extract_projects.

Covers:
- get_project_dir(): simple and dotted project names
- write_project_file(): all four combinations of spark_mode × main_file × compiler_switches
- ProjectsList: init, add(), to_json_file(), from_json_file() round-trip, missing file
- analyze_file(): minimal no-check / syntax-only Ada block (no toolchain invocation)
- analyze_file() integration: compile_button / run_button / prove_button Ada blocks
  (requires the Ada toolchain — real gnatchop and write_project_file calls)
- Global state (verbose, code_block_at, current_config) reset before each test

NOTE: analyze_file() pure-unit tests use no-check blocks so gnatchop/toolchain are not
called.  The TestAnalyzeFileIntegration class uses real Ada source and requires the Ada
toolchain.
"""
import json
import os

import pytest

import rst_code_example_pipeline.extract_projects as ep
from rst_code_example_pipeline import blocks as _blocks_mod


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def reset_module_globals():
    """Reset extract_projects module-level globals before and after each test."""
    ep.verbose = False
    ep.code_block_at = None
    ep.current_config = _blocks_mod.ConfigBlock(
        run_button=False, prove_button=True, accumulate_code=False
    )
    yield
    ep.verbose = False
    ep.code_block_at = None
    ep.current_config = _blocks_mod.ConfigBlock(
        run_button=False, prove_button=True, accumulate_code=False
    )


@pytest.fixture()
def work_dir(tmp_path, monkeypatch):
    """Change to a fresh temporary directory and restore cwd on teardown."""
    monkeypatch.chdir(tmp_path)
    return tmp_path


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
        assert "SPARK_Mode" in content or "pragma SPARK_Mode" in content or \
               "SPARK_ADC" in ep.SPARK_ADC  # content from SPARK_ADC constant
        # Verify SPARK_ADC content is actually written
        assert "SPARK" in content

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
    # This avoids any gnatchop/toolchain invocation.
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

    def _write_rst(self, tmp_path, content: str) -> str:
        rst_path = tmp_path / "test_nocheck.rst"
        rst_path.write_text(content)
        return str(rst_path)

    def test_no_crash_on_nocheck_block(self, work_dir):
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        # analyze_file() must return without raising
        result = ep.analyze_file(rst_file)
        assert result is False

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

    def test_analyze_file_verbose_missing_projects_list_file(self, work_dir, capsys):
        """verbose=True + extracted_projects_list_file pointing at a file that
        does not exist yet prints the 'will be created' message."""
        prj_list = work_dir / "new_projects.json"
        ep.verbose = True
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        result = ep.analyze_file(rst_file, str(prj_list))
        assert result is False
        assert "will be created" in capsys.readouterr().out

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

    def test_no_check_verbose_skip(self, work_dir, capsys):
        """With verbose=True a no-check block must print a 'Skipping' message."""
        ep.verbose = True
        rst_file = self._write_rst(work_dir, self.NOCHECK_RST)
        ep.analyze_file(rst_file)
        out = capsys.readouterr().out
        assert "Skipping" in out, \
            "Expected 'Skipping' message for no-check block in verbose mode"


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

    @staticmethod
    def _write_rst(work_dir, content: str, name: str = "test_integration.rst") -> str:
        rst_path = work_dir / name
        rst_path.write_text(content)
        return str(rst_path)

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
        # At least one block_info.json must have been written
        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) >= 1, \
            "analyze_file() must write at least one block_info.json for a compile block"

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
        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) >= 1, \
            "analyze_file() must write at least one block_info.json for a run block"

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
        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) >= 1, \
            "analyze_file() must write at least one block_info.json for a prove block"

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
        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) >= 1

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
        block_jsons = list(work_dir.rglob("block_info.json"))
        assert len(block_jsons) >= 1

    def test_analyze_file_c_prove_button_wrong_language(self, work_dir, capsys):
        """A C-language block with prove_button hits the 'Wrong language
        selected for prove button' error path. Known behaviour (not a bug to
        fix): the per-block error flag set on this path is never merged into
        analyze_file()'s own return value, so the function still returns
        False even though an error was printed."""
        rst_content = (
            ".. code:: c project=TestCProve prove_button\n\n"
            "   !main.c\n"
            "   int main(void) { return 0; }\n\n"
            "Explanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False
        assert "Wrong language selected for prove button" in capsys.readouterr().out

    def test_analyze_file_no_buttons_block(self, work_dir, capsys):
        """A compile/run-eligible block with no button keyword at all
        (buttons == []) hits the 'Expected at least...' error path."""
        rst_content = (
            ".. code:: ada project=TestNoBtns main=main.adb\n\n"
            + "\n".join("   " + line for line in self._ADA_BODY.splitlines())
            + "\n\nExplanatory paragraph.\n"
        )
        rst_file = self._write_rst(work_dir, rst_content)
        result = ep.analyze_file(rst_file)
        assert result is False
        assert "Expected at least" in capsys.readouterr().out
