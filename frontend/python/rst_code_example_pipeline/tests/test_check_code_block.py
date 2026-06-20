"""
Unit tests for rst_code_example_pipeline.check_code_block.

Covers:
- Diag.__repr__: correct "file:line:col: msg" format
- check_block() with block.no_check=True → returns False immediately
- check_block() with prior BlockCheck.status_ok=True in cache + force_checks=False → cache hit
- check_block() with prior BlockCheck.status_ok=False in cache + force_checks=False → cached failure
- check_block() with force_checks=True → ignores cache, runs checks
- check_block() for a minimal Ada syntax-only block (gcc -gnats) → False
- check_block() for a block with empty buttons list → has_error=True (BUTTONS check fails)
- check_code_block_json() with nonexistent file → returns True (error)
- C compile path (gcc): valid C → False; invalid C → True (requires the Ada toolchain)
- ada-expect-compile-error class: Ada that fails to compile → False (expected failure)
- C run path: valid C that exits 0 → False (requires the Ada toolchain)
- gnatprove path: minimal SPARK Ada → False; C + prove_it → True (requires the Ada toolchain)
- verbose cache-skip path: status_ok=True in cache + verbose=True → "already checked" printed
- all_diagnostics flag: compiles a valid Ada block with all_diagnostics=True → no crash
- Global state: verbose, all_diagnostics, max_columns, force_checks reset before each test

NOTE: Tests that actually run gcc/gprbuild/gnatprove require the Ada toolchain.
"""
import json
import os

import pytest

import rst_code_example_pipeline.check_code_block as ccb
import rst_code_example_pipeline.extract_projects as ep
from rst_code_example_pipeline import blocks as _blocks_mod
from rst_code_example_pipeline import checks as _checks_mod
import rst_code_example_pipeline.toolchain_info as info


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def reset_module_globals():
    """Reset check_code_block module-level globals before and after each test."""
    ccb.verbose = False
    ccb.all_diagnostics = False
    ccb.max_columns = 0
    ccb.force_checks = False
    yield
    ccb.verbose = False
    ccb.all_diagnostics = False
    ccb.max_columns = 0
    ccb.force_checks = False


@pytest.fixture(autouse=True)
def restore_cwd():
    """Restore working directory after each test (check_block does os.chdir)."""
    original = os.getcwd()
    yield
    os.chdir(original)


def _make_block(project: str = "TestProject",
                language: str = "ada",
                classes: list[str] | None = None,
                buttons: list[str] | None = None,
                gnat_version: list[str] | None = None,
                gnatprove_version: list[str] | None = None,
                gprbuild_version: list[str] | None = None,
                no_check: bool | None = None,
                syntax_only: bool | None = None,
                compile_it: bool | None = None,
                run_it: bool | None = None,
                source_files: list[str] | None = None,
                text: str = "procedure Main is begin null; end Main;") -> _blocks_mod.CodeBlock:
    """Build a minimal CodeBlock for testing.

    NOTE: Pass ``buttons=[]`` explicitly (not ``None``) to produce a block
    with an empty buttons list.  ``None`` (the default) falls back to
    ``["no"]`` so that most tests get a valid button indicator without having
    to spell it out each time.
    """
    if not info.DEFAULT_VERSION:
        info.init_toolchain_info()
    classes = classes or []
    # Use explicit None-check so that buttons=[] is preserved as-is.
    buttons = ["no"] if buttons is None else buttons
    gnat_version = gnat_version or ["default", info.DEFAULT_VERSION["gnat"]]
    gnatprove_version = gnatprove_version or ["default", info.DEFAULT_VERSION["gnatprove"]]
    gprbuild_version = gprbuild_version or ["default", info.DEFAULT_VERSION["gprbuild"]]
    return _blocks_mod.CodeBlock(
        rst_file="test.rst",
        line_start=1,
        line_end=5,
        text=text,
        language=language,
        project=project,
        main_file=None,
        gnat_version=gnat_version,
        gnatprove_version=gnatprove_version,
        gprbuild_version=gprbuild_version,
        compiler_switches=["-gnata"],
        classes=classes,
        manual_chop=False,
        buttons=buttons,
        no_check=no_check,
        syntax_only=syntax_only,
        compile_it=compile_it,
        run_it=run_it,
        source_files=source_files or [],
    )


# ---------------------------------------------------------------------------
# T-check_code_block-01: Diag.__repr__
# ---------------------------------------------------------------------------

class TestDiagRepr:
    def test_format_is_correct(self):
        d = ccb.Diag("main.adb", 10, 3, "error: missing semicolon")
        assert repr(d) == "main.adb:10:3: error: missing semicolon"

    def test_different_values(self):
        d = ccb.Diag("foo.ads", 1, 1, "warning: unused")
        assert repr(d) == "foo.ads:1:1: warning: unused"

    def test_zero_line_col(self):
        d = ccb.Diag("x.adb", 0, 0, "note")
        assert repr(d) == "x.adb:0:0: note"


# ---------------------------------------------------------------------------
# T-check_code_block-02: check_block() with no_check=True
# ---------------------------------------------------------------------------

class TestCheckBlockNoCheck:
    def test_returns_false_when_no_check(self, tmp_path):
        block = _make_block(classes=["ada-nocheck"], no_check=True)
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        result = ccb.check_block(block, json_file)
        assert result is False

    def test_no_subprocess_called_when_no_check(self, tmp_path, monkeypatch):
        """Verify no subprocess is spawned when no_check=True."""
        import subprocess as S
        calls = []
        original_check_output = S.check_output

        def mock_check_output(*args, **kwargs):
            calls.append(args)
            return original_check_output(*args, **kwargs)

        monkeypatch.setattr(S, "check_output", mock_check_output)

        block = _make_block(classes=["ada-nocheck"], no_check=True)
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        ccb.check_block(block, json_file)
        # The only subprocess calls allowed are the toolchain setup calls (set_versions).
        # Those run gcc/gnat/gnatprove/gprbuild --version. But no_check returns before
        # set_versions is called, so there should be NO subprocess calls at all.
        assert calls == [], \
            "check_block() with no_check=True must not call any subprocess"


# ---------------------------------------------------------------------------
# T-check_code_block-03: check_block() cache hit (status_ok=True)
# ---------------------------------------------------------------------------

class TestCheckBlockCacheHitOk:
    def test_cache_hit_returns_false(self, tmp_path):
        """Prior check with status_ok=True and force_checks=False → return False."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        # Write a fake block_checks.json in the same directory
        os.chdir(str(tmp_path))
        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = True
        bc.to_json_file()  # writes block_checks.json in cwd

        result = ccb.check_block(block, json_file, force_checks=False)
        assert result is False

    def test_cache_hit_with_force_true_does_not_use_cache(self, tmp_path):
        """force_checks=True must bypass the cache and run actual checks."""
        block = _make_block(classes=["ada-nocheck"], no_check=True, buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        os.chdir(str(tmp_path))
        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = True
        bc.to_json_file()

        # With force_checks=True, even though cache says ok, execution continues.
        # But since no_check=True, the block is still skipped (no_check check comes
        # first in the code, before the cache lookup).
        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False


# ---------------------------------------------------------------------------
# T-check_code_block-04: check_block() cache hit (status_ok=False)
# ---------------------------------------------------------------------------

class TestCheckBlockCacheHitFail:
    def test_cached_failure_returns_true(self, tmp_path):
        """Prior check with status_ok=False and force_checks=False → return True."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        os.chdir(str(tmp_path))
        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = False
        bc.to_json_file()

        result = ccb.check_block(block, json_file, force_checks=False)
        assert result is True

    def test_cached_none_status_ok_reruns(self, tmp_path):
        """status_ok=None in the cache means previous run was incomplete.
        The code does `not ref_block_check.status_ok` which evaluates None as
        falsy — so has_error=True and we return True. Verify this edge case."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        os.chdir(str(tmp_path))
        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = None  # neither True nor False
        bc.to_json_file()

        # `not None` is True → has_error = True
        result = ccb.check_block(block, json_file, force_checks=False)
        assert result is True


# ---------------------------------------------------------------------------
# T-check_code_block-05: check_block() with no buttons (BUTTONS check failure)
# ---------------------------------------------------------------------------

class TestCheckBlockNoButtons:
    def test_empty_buttons_returns_true(self, tmp_path):
        """A block with empty buttons list must fail the BUTTONS check."""
        # Use syntax_only=True to short-circuit after the SYNTAX check so
        # we reach the BUTTONS validation. Actually syntax_only returns early.
        # Use an actual no-compile block but with empty buttons to hit BUTTONS.
        # We need to reach the BUTTONS check section (the "if True:" block always runs).
        # The BUTTONS check is always run (it's under `if True:`).
        # With syntax_only=True the function returns early before BUTTONS.
        # So we need a block that is NOT syntax-only and NOT no_check.
        # We need source_files to be empty so the SYNTAX loop doesn't subprocess-fail.
        # Easiest: use a block that IS marked syntax_only in the classes, so
        # gcc runs on zero source_files (loop doesn't execute), and then
        # the syntax_only branch returns early.
        # To actually hit the BUTTONS check, we need a non-syntax-only, non-no-check
        # block that has been pre-cached as passing syntax so it doesn't try subprocess.
        # The simplest approach: pre-write a block_checks.json with status_ok=True so
        # the cache is hit first. But we want to test BUTTONS.
        # Alternative: use force_checks=True and an empty source_files list so the
        # SYNTAX loop does nothing, then BUTTONS check runs and finds empty buttons.
        #
        # Actually: with force_checks=True, no cache is read. SYNTAX loop runs on
        # block.source_files (empty → loop body never executes → no subprocess).
        # block.syntax_only=False → we don't return early at the syntax_only branch.
        # block.compile_it=False → no compile.
        # block.prove_it=False → no prove.
        # BUTTONS check: buttons=[] → error.

        block = _make_block(buttons=[], syntax_only=False, no_check=False)
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "check_block() must return True (has_error) when buttons list is empty"

    def test_empty_buttons_prints_error(self, tmp_path, capsys):
        block = _make_block(buttons=[], syntax_only=False, no_check=False)
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        ccb.check_block(block, json_file, force_checks=True)
        captured = capsys.readouterr()
        assert "no_button" in captured.out or "Expected" in captured.out, \
            "An error message about missing buttons must be printed"


# ---------------------------------------------------------------------------
# T-check_code_block-06: check_block() real Ada syntax check
# ---------------------------------------------------------------------------

class TestCheckBlockRealSyntax:
    """Tests that actually invoke gcc -gnats."""

    ADA_SOURCE = """\
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line ("Hello, World!");
end Main;
"""

    def test_valid_ada_syntax_returns_false(self, tmp_path):
        """A syntactically correct Ada block must pass the syntax check."""
        # Write source file
        src = tmp_path / "main.adb"
        src.write_text(self.ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["main.adb"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A syntactically valid Ada block must not produce an error"

    def test_invalid_ada_syntax_returns_true(self, tmp_path):
        """A syntactically invalid Ada block must fail the syntax check."""
        bad_source = "this is not ada;\n"
        src = tmp_path / "bad.adb"
        src.write_text(bad_source)

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["bad.adb"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "A syntactically invalid Ada block must produce an error"


# ---------------------------------------------------------------------------
# T-check_code_block-07: check_code_block_json() with nonexistent file
# ---------------------------------------------------------------------------

class TestCheckCodeBlockJson:
    def test_nonexistent_file_returns_true(self, tmp_path):
        """check_code_block_json() on a missing file must return True (error)."""
        missing = str(tmp_path / "no_such_file.json")
        result = ccb.check_code_block_json(missing)
        assert result is True

    def test_nonexistent_file_prints_error(self, tmp_path, capsys):
        missing = str(tmp_path / "missing.json")
        ccb.check_code_block_json(missing)
        captured = capsys.readouterr()
        assert "ERROR" in captured.out

    def test_valid_nocheck_block_json_returns_false(self, tmp_path):
        """check_code_block_json() on a no-check block must return False."""
        block = _make_block(classes=["ada-nocheck"], no_check=True, buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))
        result = ccb.check_code_block_json(json_file)
        assert result is False


# ---------------------------------------------------------------------------
# T-check_code_block-08: selected toolchain + non-no button validation
# ---------------------------------------------------------------------------

class TestCheckBlockSelectedToolchainButtonValidation:
    def test_selected_gnat_with_compile_button_fails_buttons_check(self, tmp_path):
        """When a specific toolchain version is selected, only 'no' button is allowed.
        A block with gnat_version=selected and buttons=['compile'] must fail."""
        block = _make_block(
            gnat_version=["selected", "12.2.0-1"],
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            # Suppress compile_it so that we reach the BUTTONS check without
            # triggering gprclean/gprbuild (which need a real project file).
            compile_it=False,
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "A block with selected toolchain and non-'no' button must fail BUTTONS check"


# ---------------------------------------------------------------------------
# T-check_code_block-09: real compile check (gprbuild)
# ---------------------------------------------------------------------------

class TestCheckBlockRealCompile:
    """Tests that actually invoke gprbuild."""

    ADA_SOURCE = """\
procedure Main is
begin
   null;
end Main;
"""

    def _setup_project(self, tmp_path):
        """Write an Ada source file and a .gpr project file into tmp_path."""
        src = tmp_path / "main.adb"
        src.write_text(self.ADA_SOURCE)
        os.chdir(str(tmp_path))
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )
        return project_filename

    def test_valid_ada_compile_returns_false(self, tmp_path):
        """A compilable Ada block must pass the compile check."""
        project_filename = self._setup_project(tmp_path)

        block = _make_block(
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.adb"],
        )
        # Set the project fields that analyze_file normally sets
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A compilable Ada block must not produce a compile error"

    def test_compile_error_block_returns_true(self, tmp_path):
        """An Ada block that fails to compile must return True (error)."""
        bad_source = "procedure Bad is\nbegin\n   SYNTAX ERROR HERE!!!\nend Bad;\n"
        src = tmp_path / "bad.adb"
        src.write_text(bad_source)
        os.chdir(str(tmp_path))
        project_filename = ep.write_project_file(
            main_file="bad.adb",
            compiler_switches=[],
            spark_mode=False,
        )

        block = _make_block(
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["bad.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "bad.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "An Ada block that fails to compile must return True (has_error)"

    def test_valid_ada_run_returns_false(self, tmp_path):
        """A compilable and runnable Ada block must compile and run without error."""
        project_filename = self._setup_project(tmp_path)

        block = _make_block(
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A compilable and runnable Ada block must not produce an error"


# ---------------------------------------------------------------------------
# C1 — TestCheckBlockCCompile
# Covers check_code_block.py C language compile path (lines ~285-312)
# Requires gcc in PATH (part of the Ada toolchain).
# ---------------------------------------------------------------------------

class TestCheckBlockCCompile:
    """Tests that actually invoke gcc on C source files."""

    VALID_C_SOURCE = "int main(void) { return 0; }\n"
    INVALID_C_SOURCE = "this is not C at all !@#$\n"

    def test_c_compile_success(self, tmp_path):
        """A valid C file with compile_it=True and buttons=['compile'] must return False."""
        src = tmp_path / "main.c"
        src.write_text(self.VALID_C_SOURCE)
        os.chdir(str(tmp_path))

        block = _make_block(
            language="c",
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A valid C file must compile without error"

    def test_c_compile_failure(self, tmp_path):
        """An invalid C file with compile_it=True must return True (has_error)."""
        src = tmp_path / "main.c"
        src.write_text(self.INVALID_C_SOURCE)
        os.chdir(str(tmp_path))

        block = _make_block(
            language="c",
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "An invalid C file must produce a compile error"


# ---------------------------------------------------------------------------
# C2 — TestCheckBlockExpectCompileError + C run path
# Covers ada-expect-compile-error class handling and C run path.
# Requires the Ada toolchain.
# ---------------------------------------------------------------------------

class TestCheckBlockExpectCompileError:
    """Tests for ada-expect-compile-error class and C run path."""

    # This Ada source is syntactically valid (passes gcc -gnats) but fails
    # gprbuild compilation because it refers to a non-existent package.
    # The nosyntax-check class bypasses the SYNTAX phase so only the BUILD
    # phase runs; 'ada-expect-compile-error' suppresses the BUILD failure.
    BAD_BUILD_ADA_SOURCE = """\
with NonExistent_Package; use NonExistent_Package;
procedure Bad is
begin
   null;
end Bad;
"""
    VALID_C_SOURCE = "int main(void) { return 0; }\n"

    def test_ada_expect_compile_error(self, tmp_path):
        """A block with classes=['ada-expect-compile-error', 'nosyntax-check']
        and Ada source that fails to compile at the BUILD phase must return False
        (the expected compile failure is not treated as an error)."""
        src = tmp_path / "bad.adb"
        src.write_text(self.BAD_BUILD_ADA_SOURCE)
        os.chdir(str(tmp_path))
        project_filename = ep.write_project_file(
            main_file="bad.adb",
            compiler_switches=[],
            spark_mode=False,
        )

        block = _make_block(
            classes=["ada-expect-compile-error", "nosyntax-check"],
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["bad.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "bad.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "An expected compile error must not count as a test failure"

    def test_c_run(self, tmp_path):
        """A valid C file compiled and run (exits 0) must return False."""
        src = tmp_path / "main.c"
        src.write_text(self.VALID_C_SOURCE)
        os.chdir(str(tmp_path))

        block = _make_block(
            language="c",
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A valid C program that exits 0 must not produce a run error"


# ---------------------------------------------------------------------------
# C3 — TestCheckBlockGnatprove
# Covers gnatprove path (lines ~411-473)
# Requires gnatprove in PATH (part of the Ada toolchain).
# ---------------------------------------------------------------------------

class TestCheckBlockGnatprove:
    """Tests that actually invoke gnatprove."""

    SPARK_SOURCE = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;
"""

    def test_ada_gnatprove_success(self, tmp_path):
        """A minimal SPARK Ada block with prove_it=True must return False."""
        src = tmp_path / "main.adb"
        src.write_text(self.SPARK_SOURCE)
        os.chdir(str(tmp_path))

        spark_project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

        block = _make_block(
            buttons=["prove"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
        )
        block.project_filename = None
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"
        # prove_it is derived from buttons in CodeBlock but we can set it directly
        block.prove_it = True

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A provable SPARK block must not produce a prove error"

    def test_ada_gnatprove_language_c_else(self, tmp_path):
        """A block with language='c' and prove_it=True must return True
        (C + prove not supported — hits the else branch at line ~465)."""
        os.chdir(str(tmp_path))

        block = _make_block(
            language="c",
            buttons=["prove"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=[],
        )
        block.prove_it = True

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "C language with prove_it=True must return True (unsupported)"


# ---------------------------------------------------------------------------
# Verbose / all_diagnostics paths
# Covers the verbose cache-skip output and the all_diagnostics output path.
# ---------------------------------------------------------------------------

class TestCheckBlockVerbose:
    """Tests for verbose and all_diagnostics flag paths."""

    ADA_SOURCE = """\
procedure Main is
begin
   null;
end Main;
"""

    def test_verbose_cache_skip(self, tmp_path, capsys):
        """With verbose=True and a cached status_ok=True, check_block must print
        'already checked. Skipping...' (exercises the verbose cache-hit path)."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        os.chdir(str(tmp_path))
        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = True
        bc.to_json_file()

        ccb.verbose = True
        result = ccb.check_block(block, json_file, verbose=True, force_checks=False)
        assert result is False
        out = capsys.readouterr().out
        assert "already checked" in out or "Skipping" in out, \
            "Expected 'already checked. Skipping...' in verbose cache-hit output"

    def test_all_diagnostics_flag(self, tmp_path):
        """With all_diagnostics=True and verbose=True and a real Ada compile,
        check_block must not crash and must exercise the all_diagnostics output
        path as well as the verbose toolchain-version print path."""
        src = tmp_path / "main.adb"
        src.write_text(self.ADA_SOURCE)
        os.chdir(str(tmp_path))
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )

        block = _make_block(
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        ccb.all_diagnostics = True
        ccb.verbose = True
        result = ccb.check_block(
            block, json_file, all_diagnostics=True, verbose=True, force_checks=True
        )
        assert result is False, \
            "A valid Ada compile with all_diagnostics=True and verbose=True must not produce an error"
