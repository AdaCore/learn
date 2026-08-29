"""
Unit tests for rst_code_example_pipeline.check_code_block.

Covers:
- Diag.__repr__: correct "file:line:col: msg" format
- check_block() with block.no_check=True → returns False immediately
- check_block() with prior BlockCheck.status_ok=True in cache + force_checks=False → cache hit
- check_block() with prior BlockCheck.status_ok=False in cache + force_checks=False → cached failure
- check_block() with force_checks=True → a recorded failure is ignored, the block is
  checked again, and the record left behind carries this run's own result
- check_block() for a minimal Ada syntax-only block (gcc -gnats) → False
- check_block() for a block with empty buttons list → has_error=True (BUTTONS check fails)
- check_code_block_json() with nonexistent file → returns True (error)
- C compile path (gcc): valid C → False; invalid C → True (requires the Ada toolchain)
- ada-expect-compile-error class: Ada that fails to compile → False (expected failure)
- a failing Ada compile reports its diagnostics against the RST file, with the block's start line added
- C run path: valid C that exits 0 → False (requires the Ada toolchain)
- gnatprove path: C + prove_it → True (requires the Ada toolchain)
- gnatprove path: a pinned, genuinely installed legacy toolchain version still proves cleanly
- verbose cache-skip path: status_ok=True in cache + verbose=True → "already checked" printed
- all_diagnostics flag: a clean Ada compile announces the block, reports SUCCESS and prints no diagnostics
- a corrupt (unparseable) cache file on disk does not crash the check
- an unrecognized language value takes neither the Ada nor the C branch anywhere
- a toolchain binary missing from PATH falls back to an unknown-version marker instead of aborting the check
- gprclean and gnatprove --clean clean-up failures after a successful Ada compile and run are logged (or silently swallowed) without affecting the result
- an rm -f clean-up failure after a successful C compile and run is logged without affecting the result
- check_block() driven by the real extraction step rather than by a hand-built block:
  the compile, run and prove buttons an author writes in an RST directive, plus the
  C run path and the ada-expect-compile-error class, each carry through to the checks
  actually performed; an extracted block that does not build is reported as an error;
  and an extracted C block asking only for a compile is an xfail (requires the Ada
  toolchain).  These subsume the hand-built happy-path compile, run and prove tests
  that used to sit alongside them
- Global state: verbose, all_diagnostics, max_columns, force_checks reset before each test

NOTE: check_block() sets the toolchain up for every block before any early return, so a
test needs the Ada toolchain even when it stops at a no-check block or a cache hit and
never reaches a compiler.  Every test that calls check_block() therefore carries the
`toolchain` marker; only the Diag repr tests and the two check_code_block_json() tests
that bail out on a missing file are free of it.
"""
import ast
import json
import os
import re

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


# The smallest Ada program that compiles and runs, shared by every test that
# needs a source file but does not care what it contains.
MINIMAL_ADA_SOURCE = """\
procedure Main is
begin
   null;
end Main;
"""


def _installed_version(tool: str) -> str:
    """Return a version of ``tool`` declared as installed in the toolchain
    configuration, for tests that need to select a version explicitly rather
    than take the default one."""
    if not info.TOOLCHAINS:
        info.init_toolchain_info()
    return info.TOOLCHAINS[tool][0]


def _legacy_gnatprove_version() -> str:
    """Return the declared GNATprove version that gets the older command line.

    check_block() builds a pre-14 GNATprove command line for any version whose
    identifier starts with "12", so a test of that branch needs a declared
    version of that generation.  Fail with a message naming the branch if none
    is declared any more, rather than with an obscure lookup error.
    """
    if not info.TOOLCHAINS:
        info.init_toolchain_info()
    legacy = [v for v in info.TOOLCHAINS["gnatprove"] if v.startswith("12")]
    assert legacy, \
        "No GNATprove version of the 12 generation is declared as installed, " \
        "so the older-style command line it needs cannot be exercised"
    return legacy[0]


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
                line_start: int = 1,
                text: str = "procedure Main is begin null; end Main;") -> _blocks_mod.CodeBlock:
    """Build a minimal CodeBlock for testing.

    NOTE: Pass ``buttons=[]`` explicitly (not ``None``) to produce a block
    with an empty buttons list.  ``None`` (the default) falls back to
    ``["no"]`` so that most tests get a valid button indicator without having
    to spell it out each time.

    NOTE: ``line_start`` says where the block sits in its RST file.  A test
    that checks how a compiler diagnostic is mapped back onto the RST file
    should set it higher than any line the compiler could report on its own,
    so that an unmapped line cannot be mistaken for a mapped one.
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
        line_start=line_start,
        line_end=line_start + 4,
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

@pytest.mark.toolchain
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

@pytest.mark.toolchain
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


# ---------------------------------------------------------------------------
# T-check_code_block-04: check_block() cache hit (status_ok=False)
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
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


@pytest.mark.toolchain
class TestCheckBlockCorruptCache:
    def test_corrupt_cache_file_is_ignored(self, tmp_path):
        """A previous-check cache file that is not valid JSON must not crash
        check_block(): the read failure is caught, no cached result is used,
        and a full check runs and completes normally instead."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        (tmp_path / "block_checks.json").write_text("{not valid json")

        result = ccb.check_block(block, json_file)
        assert result is False, \
            "An unparseable cache file must be ignored rather than crash the check"


# ---------------------------------------------------------------------------
# check_block() with the checks forced against a populated cache
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockForceChecks:
    def test_forcing_the_checks_overrides_a_cached_failure(self, tmp_path):
        """Forcing the checks must ignore what a previous run recorded and
        check the block again.

        The block is checkable and clean, but a record of an earlier run
        sitting beside it says the block failed.  Left alone, that record is
        what the caller gets back -- TestCheckBlockCacheHitFail pins that.
        Forced, the stale record has to be ignored, the checks have to run for
        real, and the answer has to be the one the block earns rather than the
        one on disk.

        Both halves are asserted, because the outcome alone cannot tell a
        re-check apart from a cache lookup that happened to be dropped: the
        record left behind afterwards must carry this run's own result and the
        checks it performed.
        """
        src = tmp_path / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
        os.chdir(str(tmp_path))

        block = _make_block(
            buttons=["no"],
            no_check=False,
            syntax_only=False,
            source_files=["main.adb"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        stale = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        stale.status_ok = False
        stale.to_json_file()

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "a recorded failure must not be returned when the checks are forced"

        rewritten = json.loads((tmp_path / "block_checks.json").read_text())
        assert rewritten["status_ok"] is True, \
            "the forced run must replace the stale record with its own result"
        assert "SYNTAX" in rewritten["checks"], \
            "the forced run must have checked the block, not skipped it"


# ---------------------------------------------------------------------------
# T-check_code_block-05: check_block() with no buttons (BUTTONS check failure)
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
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
        """The diagnostic must name the offending block and say what was
        missing, since that text is all a course author gets to act on."""
        block = _make_block(buttons=[], syntax_only=False, no_check=False)
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        ccb.check_block(block, json_file, force_checks=True)
        captured = capsys.readouterr()
        # The "ERROR" prefix and its coloring belong to the message formatter
        # and are covered with it; what matters here is the location and the
        # wording that follows.
        expected = (
            "at {}:{} (code block hash: {}): "
            "Expected at least 'no_button' indicator, got none!".format(
                block.rst_file, block.line_start, block.text_hash_short))
        assert expected in captured.out


# ---------------------------------------------------------------------------
# T-check_code_block-06: check_block() real Ada syntax check
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
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

    @pytest.mark.toolchain
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

@pytest.mark.toolchain
class TestCheckBlockSelectedToolchainButtonValidation:
    def test_selected_gnat_with_compile_button_fails_buttons_check(self, tmp_path):
        """When a specific toolchain version is selected, only 'no' button is allowed.
        A block with gnat_version=selected and buttons=['compile'] must fail."""
        block = _make_block(
            gnat_version=["selected", _installed_version("gnat")],
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

@pytest.mark.toolchain
class TestCheckBlockRealCompile:
    """Tests that actually invoke gprbuild."""

    BAD_ADA_SOURCE = "procedure Bad is\nbegin\n   SYNTAX ERROR HERE!!!\nend Bad;\n"

    @staticmethod
    def _compile_failing_block_at(work_dir, capsys, line_start, bad_source):
        """Check a block that fails to compile, starting at ``line_start`` in
        its RST file.

        Returns the check result, the distinct line numbers the diagnostics
        were reported at against the RST file, and the distinct line numbers
        the compiler itself used for the extracted source -- the latter read
        back from the raw compiler output the check prints alongside them, so
        the test never has to know where the compiler places a diagnostic.

        Both are de-duplicated: a failing check reports the same diagnostic
        several times over, and how often it does is not what is under test
        here.
        """
        work_dir.mkdir(parents=True, exist_ok=True)
        (work_dir / "bad.adb").write_text(bad_source)
        os.chdir(str(work_dir))
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
            line_start=line_start,
        )
        block.project_filename = project_filename
        block.project_main_file = "bad.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(work_dir))

        capsys.readouterr()
        result = ccb.check_block(block, json_file, force_checks=True)
        out = capsys.readouterr().out

        reported = sorted({int(line) for line in re.findall(
            r"^{}:(\d+):\d+: ".format(re.escape(block.rst_file)), out, re.M)})
        raw = sorted({int(line)
                      for line in re.findall(r"bad\.adb:(\d+):\d+: ", out)})
        return result, reported, raw

    def test_compile_error_block_returns_true(self, tmp_path, capsys):
        """An Ada block that fails to compile must return True (error) and
        report the compiler diagnostics against the RST file, at the lines the
        block occupies there.

        The compiler numbers its diagnostics from the top of the extracted
        source; check_block has to re-point them at the RST file the reader is
        editing and shift them by where the block starts in it.  Neither the
        compiler's wording nor any particular line is pinned, so a compiler
        upgrade that moves or adds a diagnostic does not redden this:

        * against the compiler's own numbering, read back from the raw output
          printed alongside the remapped diagnostics, every reported line must
          be that number plus the block's start line -- which is what catches a
          shift that is missing, doubled, or off by one;
        * and compiling the same block a second time from a different start
          line must move every reported line by exactly that difference.
        """
        first_start, second_start = 100, 250

        first_result, first_lines, first_raw = self._compile_failing_block_at(
            tmp_path / "first", capsys, first_start, self.BAD_ADA_SOURCE)
        second_result, second_lines, second_raw = self._compile_failing_block_at(
            tmp_path / "second", capsys, second_start, self.BAD_ADA_SOURCE)

        assert first_result is True and second_result is True, \
            "An Ada block that fails to compile must return True (has_error)"
        assert first_lines, \
            "no compiler diagnostic was reported against the RST file"
        assert first_raw, \
            "the raw compiler output must be shown, or there is nothing to " \
            "compare the remapped line numbers against"

        assert first_lines == [line + first_start for line in first_raw], \
            "each diagnostic must be reported at its compiler line shifted by " \
            "the block's start line; compiler said {}, block starts at {}, " \
            "reported {}".format(first_raw, first_start, first_lines)
        assert second_lines == [line + second_start for line in second_raw], \
            "each diagnostic must be reported at its compiler line shifted by " \
            "the block's start line; compiler said {}, block starts at {}, " \
            "reported {}".format(second_raw, second_start, second_lines)

        assert second_lines == [
            line + (second_start - first_start) for line in first_lines], \
            "moving the block down the RST file must move its diagnostics with " \
            "it: {} at line {} became {} at line {}".format(
                first_lines, first_start, second_lines, second_start)


# ---------------------------------------------------------------------------
# C1 — TestCheckBlockCCompile
# Covers check_code_block.py C language compile path (lines ~285-312)
# Requires gcc in PATH (part of the Ada toolchain).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
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

@pytest.mark.toolchain
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

@pytest.mark.toolchain
class TestCheckBlockGnatprove:
    """Tests that actually invoke gnatprove."""

    SPARK_SOURCE = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;
"""

    def test_ada_gnatprove_language_c_else(self, tmp_path):
        """A block with language="c" and prove_it=True must return True:
        proving only supports Ada, so a non-Ada block takes the "wrong
        language selected for prove button" error branch instead of
        invoking gnatprove."""
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

    def test_ada_gnatprove_pinned_legacy_version(self, tmp_path):
        """A prove block pinned to a specific, genuinely installed legacy
        GNATprove version must build the older-style command line that
        version expects, and a real invocation with it must still prove the
        example cleanly."""
        src = tmp_path / "main.adb"
        src.write_text(self.SPARK_SOURCE)
        os.chdir(str(tmp_path))

        spark_project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

        block = _make_block(
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
            gnatprove_version=["selected", _legacy_gnatprove_version()],
        )
        block.project_filename = None
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"
        block.prove_it = True

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A provable SPARK block must prove cleanly under a pinned legacy GNATprove version"


# ---------------------------------------------------------------------------
# Unrecognized-language paths
# Covers cleanup/syntax-check/compile/run all falling through without taking
# either the Ada or the C branch, and without crashing.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockUnrecognizedLanguage:
    def test_unrecognized_language_takes_neither_branch(self, tmp_path):
        """A block whose language is neither 'ada' nor 'c' must fall through
        the cleanup, syntax-check, compile, and run steps without taking
        either language-specific branch, and must complete without raising."""
        block = _make_block(
            language="fortran",
            no_check=False,
            syntax_only=False,
            compile_it=True,
            run_it=True,
            source_files=["main.f90"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "An unrecognized language must not raise and must not report an error"


# ---------------------------------------------------------------------------
# Verbose / all_diagnostics paths
# Covers the verbose cache-skip output and the all_diagnostics output path.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockVerbose:
    """Tests for verbose and all_diagnostics flag paths."""

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
        expected = (
            "Code block at {}:{} (code block hash: {}) "
            "already checked. Skipping...".format(
                block.rst_file, block.line_start, block.text_hash_short))
        assert expected in out

    def test_all_diagnostics_flag(self, tmp_path, capsys):
        """With all_diagnostics=True and verbose=True, a clean Ada compile must
        announce the block it is checking, report success, and print no
        diagnostics at all."""
        src = tmp_path / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
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

        out = capsys.readouterr().out
        assert "Checking code block at {}:{} (code block hash: {})".format(
            block.rst_file, block.line_start, block.text_hash_short) in out
        assert "SUCCESS" in out
        assert not re.search(
            r"^{}:\d+:\d+: ".format(re.escape(block.rst_file)), out, re.M), \
            "a clean compile must not report any diagnostic against the RST file"


# ---------------------------------------------------------------------------
# TestCheckBlockMaxColumns
# Covers the max_columns setting being passed through to the Ada syntax
# check (it appends a -gnatyM<N> style-check switch).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockMaxColumns:
    def test_syntax_check_with_max_columns(self, tmp_path):
        """max_columns > 0 appends -gnatyMN to the syntax-check command and
        a normal-width Ada block still passes."""
        src = tmp_path / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["main.adb"],
        )
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, max_columns=80, force_checks=True)
        assert result is False


# ---------------------------------------------------------------------------
# TestCheckBlockRunExpectFailure
# Covers the ada-run-expect-failure class: an unexpectedly successful run,
# an expectedly failing run, and an unexpectedly failing run.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockRunExpectFailure:
    VALID_ADA_SOURCE = """\
procedure Main is
begin
   null;
end Main;
"""

    FAILING_ADA_SOURCE = """\
with Ada.Command_Line;
procedure Main is
begin
   Ada.Command_Line.Set_Exit_Status (1);
end Main;
"""

    def _setup_project(self, tmp_path, source):
        src = tmp_path / "main.adb"
        src.write_text(source)
        os.chdir(str(tmp_path))
        return ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )

    def _make_run_block(self, classes=None):
        return _make_block(
            classes=classes or [],
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.adb"],
        )

    def test_run_success_with_expect_failure_class(self, tmp_path):
        """A program that exits 0 while marked ada-run-expect-failure must
        return True: the run succeeded when a failure was expected."""
        project_filename = self._setup_project(tmp_path, self.VALID_ADA_SOURCE)
        block = self._make_run_block(classes=["ada-run-expect-failure"])
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True

    def test_ada_run_fail_with_expect_failure_class(self, tmp_path, capsys):
        """A program that exits non-zero while marked ada-run-expect-failure
        must return False: the failure was expected. With verbose enabled,
        the expected-failure message is printed."""
        project_filename = self._setup_project(tmp_path, self.FAILING_ADA_SOURCE)
        block = self._make_run_block(classes=["ada-run-expect-failure"])
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        ccb.verbose = True
        result = ccb.check_block(block, json_file, verbose=True, force_checks=True)
        assert result is False
        out = capsys.readouterr().out
        assert "Running of example expectedly failed" in out

    def test_ada_run_fail_without_expect_failure(self, tmp_path):
        """A program that exits non-zero without ada-run-expect-failure must
        return True: an unexpected run failure."""
        project_filename = self._setup_project(tmp_path, self.FAILING_ADA_SOURCE)
        block = self._make_run_block()
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True


# ---------------------------------------------------------------------------
# TestCheckBlockCRunExpectFailure
# Covers the c-run-expect-failure class, symmetric to the Ada case above.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockCRunExpectFailure:
    VALID_C_SOURCE = "int main(void) { return 0; }\n"
    FAILING_C_SOURCE = "int main(void) { return 1; }\n"

    def _make_c_run_block(self, classes=None):
        return _make_block(
            language="c",
            classes=classes or [],
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.c"],
        )

    def test_c_run_fail_with_expect_failure_class(self, tmp_path, capsys):
        """A C program that exits non-zero while marked c-run-expect-failure
        must return False: the failure was expected. With verbose enabled,
        the expected-failure message is printed."""
        src = tmp_path / "main.c"
        src.write_text(self.FAILING_C_SOURCE)
        os.chdir(str(tmp_path))

        block = self._make_c_run_block(classes=["c-run-expect-failure"])
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        ccb.verbose = True
        result = ccb.check_block(block, json_file, verbose=True, force_checks=True)
        assert result is False
        out = capsys.readouterr().out
        assert "Running of example expectedly failed" in out

    def test_c_run_success_with_expect_failure_class(self, tmp_path):
        """A C program that exits 0 while marked c-run-expect-failure must
        return True: the run succeeded when a failure was expected."""
        src = tmp_path / "main.c"
        src.write_text(self.VALID_C_SOURCE)
        os.chdir(str(tmp_path))

        block = self._make_c_run_block(classes=["c-run-expect-failure"])
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True

    def test_c_run_fail_without_expect_failure(self, tmp_path):
        """A C program that exits non-zero without c-run-expect-failure must
        return True: an unexpected run failure."""
        src = tmp_path / "main.c"
        src.write_text(self.FAILING_C_SOURCE)
        os.chdir(str(tmp_path))

        block = self._make_c_run_block()
        block.project_main_file = "main.c"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True


# ---------------------------------------------------------------------------
# TestCheckBlockCExpectCompileError
# Covers the c-expect-compile-error class in the C compile handler.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockCExpectCompileError:
    INVALID_C_SOURCE = "this is not C at all !@#$\n"

    def test_c_compile_error_expected(self, tmp_path):
        """A C file that fails to compile while marked c-expect-compile-error
        must return False: the compile failure was expected.

        nosyntax-check is also set: for C, the SYNTAX phase and the BUILD
        phase both invoke gcc on the same source, so a genuine syntax error
        would already fail (as an unexpected error) during SYNTAX before the
        BUILD phase's c-expect-compile-error handling is ever reached -- the
        same reason the analogous ada-expect-compile-error test bypasses the
        SYNTAX phase."""
        src = tmp_path / "main.c"
        src.write_text(self.INVALID_C_SOURCE)
        os.chdir(str(tmp_path))

        block = _make_block(
            language="c",
            classes=["c-expect-compile-error", "nosyntax-check"],
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
        assert result is False


# ---------------------------------------------------------------------------
# TestCheckBlockProveFailure
# Covers the gnatprove failure handler: the expected (ada-expect-prove-error)
# and unexpected branches.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockProveFailure:
    # X is read via Y := X before being initialized: a flow-analysis check
    # that reliably fails under --checks-as-errors (mirrors the pattern used
    # in the course's own "may not be initialized" SPARK examples).
    FAILING_SPARK_SOURCE = """\
procedure Main with SPARK_Mode is
   X, Y : Integer;
begin
   Y := X;
end Main;
"""

    def _setup_spark_project(self, tmp_path):
        src = tmp_path / "main.adb"
        src.write_text(self.FAILING_SPARK_SOURCE)
        os.chdir(str(tmp_path))
        return ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

    def _make_prove_block(self, classes=None):
        return _make_block(
            classes=classes or [],
            buttons=["prove"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
        )

    def test_prove_failure_expected(self, tmp_path):
        """SPARK code that fails to prove while marked ada-expect-prove-error
        must return False: the failure was expected."""
        spark_project_filename = self._setup_spark_project(tmp_path)
        block = self._make_prove_block(classes=["ada-expect-prove-error"])
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False

    def test_prove_failure_unexpected(self, tmp_path):
        """SPARK code that fails to prove without ada-expect-prove-error must
        return True: an unexpected prove failure."""
        spark_project_filename = self._setup_spark_project(tmp_path)
        block = self._make_prove_block()
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True


# ---------------------------------------------------------------------------
# TestCheckBlockProveExtraArgs
# Covers the gnatprove extra-arguments variants selected via the prove_flow /
# prove_flow_report_all / prove_report_all buttons.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockProveExtraArgs:
    SPARK_SOURCE = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;
"""

    def _setup_spark_project(self, tmp_path):
        src = tmp_path / "main.adb"
        src.write_text(self.SPARK_SOURCE)
        os.chdir(str(tmp_path))
        return ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

    def _make_prove_block(self, button):
        return _make_block(
            buttons=[button],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
        )

    def _run(self, tmp_path, button):
        spark_project_filename = self._setup_spark_project(tmp_path)
        block = self._make_prove_block(button)
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        return ccb.check_block(block, json_file, force_checks=True)

    def test_prove_flow_mode(self, tmp_path):
        """prove_flow button selects '--mode=flow'; a trivially valid SPARK
        block must still pass."""
        assert self._run(tmp_path, "prove_flow") is False

    def test_prove_flow_report_all(self, tmp_path):
        """prove_flow_report_all button selects '--mode=flow --report=all'."""
        assert self._run(tmp_path, "prove_flow_report_all") is False

    def test_prove_report_all(self, tmp_path):
        """prove_report_all button selects '--report=all'."""
        assert self._run(tmp_path, "prove_report_all") is False


# ---------------------------------------------------------------------------
# TestCheckCodeBlockJsonInactive
# Covers the inactive-block WARNING printed by check_code_block_json().
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckCodeBlockJsonInactive:
    def test_check_code_block_json_inactive_block(self, tmp_path, capsys):
        """check_code_block_json() on a block with active=False prints the
        deactivation WARNING and still checks it."""
        block = _make_block(classes=["ada-nocheck"], no_check=True, buttons=["no"])
        block.active = False
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)
        os.chdir(str(tmp_path))

        result = ccb.check_code_block_json(json_file)
        assert result is False
        assert "WARNING" in capsys.readouterr().out


# ---------------------------------------------------------------------------
# Missing-toolchain path
# Covers the version-lookup fallback when a toolchain binary is missing from
# PATH, in place of monkeypatching the subprocess call.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockMissingToolchain:
    def test_missing_toolchain_binary_falls_back_to_unknown_version(self, tmp_path, monkeypatch):
        """When none of the toolchain binaries can be found on PATH, the
        version lookup must not abort the check: it silently falls back to an
        unknown-version marker instead, and check_block() still completes and
        returns False. The recorded check result is read back from the raw
        written file (not through the round-trip API, which does not restore
        the nested per-check dict) to confirm the fallback value was actually
        recorded, rather than only asserting the absence of a crash."""
        block = _make_block(buttons=["no"])
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        monkeypatch.setenv("PATH", str(tmp_path))

        result = ccb.check_block(block, json_file)
        assert result is False, \
            "A missing toolchain must not crash the check, only skip real checks"

        written = json.loads((tmp_path / "block_checks.json").read_text())
        assert written["checks"]["SYNTAX"]["version"] == "<unknown>", \
            "The version lookup must have failed and recorded the fallback marker"


# ---------------------------------------------------------------------------
# Clean-up failure paths
# Covers the gprclean / gnatprove --clean clean-up failures after an Ada
# compile and run, and the rm -f clean-up failure after a C compile and run.
# The clean-up commands are selectively made to fail while every other
# command (the real compile and run) is left untouched.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockCleanupFailures:
    """A real Ada compile and run that both succeed, while every clean-up
    command invoked along the way is made to fail."""

    def _setup_project(self, tmp_path):
        """Write an Ada source file and a .gpr project file into tmp_path."""
        src = tmp_path / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
        os.chdir(str(tmp_path))
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )
        return project_filename

    def test_gprclean_and_gnatprove_clean_failures_do_not_affect_result(
            self, tmp_path, monkeypatch, capsys):
        """A gprclean failure before compiling, a gprclean failure during
        end-of-check clean-up, and a gnatprove --clean failure during
        end-of-check clean-up are all logged (the first two) or silently
        swallowed (the third) -- but none of them aborts the check or changes
        its result: a real compile and run that succeed still make the check
        pass."""
        import subprocess as S

        project_filename = self._setup_project(tmp_path)

        real_check_output = S.check_output
        failed_cleanups = []

        def fake_check_output(cmd, *args, **kwargs):
            if cmd[0] == "gprclean" or (cmd[0] == "gnatprove" and "--clean" in cmd):
                failed_cleanups.append(cmd[0])
                raise S.CalledProcessError(1, cmd, output=b"simulated cleanup failure")
            return real_check_output(cmd, *args, **kwargs)

        monkeypatch.setattr(S, "check_output", fake_check_output)

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
            "clean-up failures must not affect the outcome of a successful compile and run"

        # Both clean-up commands must have been reached and must have failed,
        # otherwise the test proves nothing about how their failure is handled.
        assert "gprclean" in failed_cleanups
        assert "gnatprove" in failed_cleanups

        out = capsys.readouterr().out
        # Both gprclean failures are logged and the gnatprove --clean one is
        # not, so at least two messages must appear.  The bound is a minimum
        # rather than an equality on purpose: adding a further clean-up step is
        # not a regression, whereas dropping the logging from either of the two
        # sites that have it is -- and the two messages are textually identical,
        # so counting them is the only way to tell one has gone.
        assert out.count("Failed to clean-up example") >= 2, \
            "a failing clean-up must be logged rather than passed over in silence"
        assert "simulated cleanup failure" in out, \
            "the failing clean-up command's own output must be shown with the message"


@pytest.mark.toolchain
class TestCheckBlockCCleanupFailure:
    """A real C compile and run that both succeed, while the rm -f clean-up
    command is made to fail."""

    VALID_C_SOURCE = "int main(void) { return 0; }\n"

    def test_rm_cleanup_failure_does_not_affect_result(self, tmp_path, monkeypatch, capsys):
        """An rm -f clean-up failure after a successful C compile and run is
        logged, but it does not abort the check or change its result."""
        import subprocess as S

        src = tmp_path / "main.c"
        src.write_text(self.VALID_C_SOURCE)
        os.chdir(str(tmp_path))

        real_check_output = S.check_output

        def fake_check_output(cmd, *args, **kwargs):
            if cmd[0] == "rm":
                raise S.CalledProcessError(1, cmd, output=b"simulated rm failure")
            return real_check_output(cmd, *args, **kwargs)

        monkeypatch.setattr(S, "check_output", fake_check_output)

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
            "an rm -f clean-up failure must not affect the outcome of a successful compile and run"

        assert "Failed to clean-up example" in capsys.readouterr().out


# ---------------------------------------------------------------------------
# check_block() driven by the real extraction step
# Requires the Ada toolchain (real gnatchop, gprbuild and gnatprove runs).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockDrivenByTheExtractor:
    """check_block() started from what the extraction step really wrote.

    Every other check_block() test in this file assembles a CodeBlock in
    memory and then pokes the fields the checker reads -- project_filename,
    spark_project_filename, project_main_file, source_files -- into the shape
    the path under test needs.  That verifies the checker against a state the
    extraction step may never produce, so a disagreement between the two
    halves about what is written, and about what it contains, stays invisible.

    These tests run the whole chain instead: the RST directive an author types
    is parsed, the extraction step chops the block and writes the project
    files and the block info beside it, and the check is then started from
    that block info exactly as the command line starts it.  Nothing is
    adjusted in between.

    The trade-off is deliberate: a hand-built block is independent of the
    extraction step, and these are not.  So the assertions are chosen to fail
    when the two halves disagree:

    * the set of phases the check recorded must be exactly the set the
      directive's button calls for -- this is the assertion with real
      detection power, and it is also the one that pins the phase labels
      ("SYNTAX", "BUILD", "RUN", "PROVE", "BUTTONS") as literals.  That pin is
      a deliberate trade: the labels are the checker's own choice of name, so
      renaming one reddens these tests and no others, but the recorded set is
      the only observable of *which* checks actually ran, and nothing else in
      the suite watches it;
    * and the project file the check really used -- read back out of the
      command line the check recorded -- must be configured the way that
      button requires, which is what catches the extraction step writing a
      project for the wrong mode.

    Known limit, so that the messages above are not read as promising more
    than they deliver: the extraction step always writes its project files
    under the same two names, so a checker that stopped reading the block
    info's filename fields and hard-coded those same names instead would
    behave identically and go undetected here.  What *is* detected is the two
    halves being cross-wired (a build driven from the SPARK project, or the
    reverse) and a project whose contents do not match the button.
    """

    _RUN_OUTPUT = "extracted example ran"
    _C_RUN_OUTPUT = "extracted C example ran"

    # The main file the directives below declare.  Kept as one value because
    # the tests assert that the generated project names this same file.
    _MAIN = "main.adb"
    _C_MAIN = "main.c"

    # A name nothing declares, so that a build has to fail on it and the
    # compiler has to say so.
    _MISSING_NAME = "No_Such_Procedure"

    # What tells a SPARK project apart from an ordinary one: GNATprove only
    # treats the unit as SPARK because this pragma is configured in.
    _SPARK_CONFIGURATION = "pragma SPARK_Mode (On);"

    # A minimal Ada program that announces itself, so that a test can tell a
    # run that really happened from one that was reported as having happened.
    _ADA_BODY = """\
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line ("{}");
end Main;""".format(_RUN_OUTPUT)

    # Syntactically valid -- so it chops and passes the syntax check -- but it
    # calls something that does not exist, so the build must fail.
    _BROKEN_ADA_BODY = """\
procedure Main is
begin
   {};
end Main;""".format(_MISSING_NAME)

    _SPARK_BODY = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;"""

    # A C block declares its file names inline; the chopper reads them off the
    # leading marker lines rather than calling gnatchop.
    _C_BODY = """\
!{}
#include <stdio.h>

int main(void)
{{
   printf("{}\\n");
   return 0;
}}""".format(_C_MAIN, _C_RUN_OUTPUT)

    @staticmethod
    def _rst(directive: str, body: str, classes: str | None = None) -> str:
        """An RST file holding exactly one code block.

        The body is indented the way an author writes it, and the explanatory
        paragraph that follows is what tells the parser the block has ended.
        """
        indented = "\n".join("   " + line for line in body.splitlines())
        head = directive if classes is None else \
            "{}\n   :class: {}".format(directive, classes)
        return "{}\n\n{}\n\nExplanatory paragraph.\n".format(head, indented)

    def _extract(self, work_dir, directive: str, body: str, project: str,
                 classes: str | None = None):
        """Run the real extraction step on a one-block RST file.

        Returns the per-block directory it wrote, the block info the checker
        will be handed, and the absolute path of that block info file.

        The per-block directory is found by asking the extraction step where
        it puts a project, and then by which directory below it holds a block
        info file -- the staging copy the extraction step keeps alongside does
        not have one.
        """
        rst_path = work_dir / "extracted.rst"
        rst_path.write_text(self._rst(directive, body, classes))
        os.chdir(str(work_dir))

        assert ep.analyze_file(str(rst_path)) is False, \
            "the fixture must extract cleanly, or the check that follows is " \
            "not being handed a well-formed block"

        project_dir = work_dir / ep.get_project_dir(project)
        block_dirs = sorted(d for d in project_dir.iterdir()
                            if (d / "block_info.json").is_file())
        assert len(block_dirs) == 1, \
            "expected exactly one per-block directory, got {}".format(
                [d.name for d in block_dirs])
        block_dir = block_dirs[0]
        json_file = block_dir / "block_info.json"
        return block_dir, json.loads(json_file.read_text()), str(json_file)

    @staticmethod
    def _buttons_asked_for(info) -> tuple[bool, bool, bool]:
        """The compile / run / prove decision the checker branches on."""
        return info["compile_it"], info["run_it"], info["prove_it"]

    @staticmethod
    def _recorded_checks(block_dir) -> dict:
        """The per-phase results the check wrote beside the block.

        Read straight from the file rather than through
        checks.BlockCheck.from_json_file(), which drops the per-phase entries
        on the way back in.
        """
        return json.loads((block_dir / "block_checks.json").read_text())["checks"]

    @staticmethod
    def _log_of(block_dir, recorded_check) -> str:
        """The log a recorded phase says it wrote."""
        return (block_dir / recorded_check["logfile"]).read_text()

    @staticmethod
    def _project_used(recorded_check) -> str:
        """The project file a recorded phase really ran against.

        The command line is recorded as the printed form of the argument list,
        so it can be read back as one and the project taken from behind the
        switch that names it -- rather than by matching a name the test would
        otherwise have to know in advance.
        """
        args = ast.literal_eval(recorded_check["cmdline"])
        return args[args.index("-P") + 1]

    @staticmethod
    def _configuration_pragmas(block_dir, project_filename: str) -> str:
        """The configuration pragmas a project file pulls in.

        Followed through the project's own reference to its pragma file, so
        that a project generated for the wrong mode is caught by what it
        configures rather than by what it happens to be called.
        """
        project_text = (block_dir / project_filename).read_text()
        named = re.search(r'for Global_Configuration_Pragmas use "([^"]+)"',
                          project_text)
        assert named is not None, \
            "the generated project must name a configuration pragma file"
        return (block_dir / named.group(1)).read_text()

    def test_compile_button_block_is_built_as_extracted(self, tmp_path):
        """A compile button carries from the RST directive through to a real
        build with nothing adjusted in between.

        The directive asks for a compile and nothing else, so the block must
        reach the checker asking for a compile and nothing else, the checker
        must record a build and neither a run nor a proof, and the project it
        built against must be an ordinary one naming no main -- a compile
        button selects no main to link.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: ada project=ExtractedCompile main={} compile_button".format(
                self._MAIN),
            self._ADA_BODY, "ExtractedCompile")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert self._buttons_asked_for(info) == (True, False, False), \
            "a compile button must reach the checker as a compile and nothing else"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted block as it stands"

        recorded = self._recorded_checks(block_dir)
        # Pins the checker's phase labels; see the class docstring for why
        # that trade is made deliberately.
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "a compile button must be syntax-checked and built, and neither " \
            "run nor proved"
        assert recorded["BUILD"]["status_ok"] is True

        built_against = self._project_used(recorded["BUILD"])
        assert "for Main use" not in (block_dir / built_against).read_text(), \
            "a compile button selects no main, so the project built against " \
            "must name none"
        assert self._SPARK_CONFIGURATION not in \
            self._configuration_pragmas(block_dir, built_against), \
            "a compile button must not be built against a SPARK-configured project"

    def test_run_button_block_is_built_and_run_as_extracted(self, tmp_path):
        """A run button carries from the RST directive through to the program
        actually running.

        A run implies a compile, so both must be asked for and both must be
        recorded.  The project built against must name the main the directive
        declared, or there is nothing for the builder to link.  And the output
        pinned below is what the author's code prints: it can only reach the
        run log if the block was chopped, built from the generated project,
        and executed.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: ada project=ExtractedRun main={} run_button".format(
                self._MAIN),
            self._ADA_BODY, "ExtractedRun")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert self._buttons_asked_for(info) == (True, True, False), \
            "a run button must reach the checker as a run, which implies a " \
            "compile, and not as a proof"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted block as it stands"

        recorded = self._recorded_checks(block_dir)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "a run button must be syntax-checked, built and run, and not proved"

        built_against = self._project_used(recorded["BUILD"])
        # A localizer, not a detector: the run above cannot happen at all
        # unless the project names a main, so this line says which link
        # broke rather than being the first to notice.
        assert 'for Main use ("{}");'.format(self._MAIN) in \
            (block_dir / built_against).read_text(), \
            "the project built against must name the main the directive declared"
        assert self._SPARK_CONFIGURATION not in \
            self._configuration_pragmas(block_dir, built_against), \
            "a run button must not be built against a SPARK-configured project"

        assert self._log_of(block_dir, recorded["RUN"]).strip() == self._RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"

    def test_prove_button_block_is_proved_as_extracted(self, tmp_path):
        """A prove button carries from the RST directive through to a real
        proof.

        Proving needs a project configured for SPARK, which the extraction
        step generates separately from the one a build would use.  So the
        proof must be recorded, the build must not be, and the project the
        proof really ran against must be one that turns SPARK mode on --
        asserted through what that project configures, since a project
        generated in the wrong mode would still be recorded under the right
        field name.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: ada project=ExtractedProve main={} prove_button".format(
                self._MAIN),
            self._SPARK_BODY, "ExtractedProve")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert self._buttons_asked_for(info) == (False, False, True), \
            "a prove button must reach the checker as a proof and nothing else"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted block as it stands"

        recorded = self._recorded_checks(block_dir)
        assert sorted(recorded) == ["BUTTONS", "PROVE", "SYNTAX"], \
            "a prove button must be syntax-checked and proved, and not built"
        assert recorded["PROVE"]["status_ok"] is True

        proved_against = self._project_used(recorded["PROVE"])
        assert self._SPARK_CONFIGURATION in \
            self._configuration_pragmas(block_dir, proved_against), \
            "the proof must have run against a project that turns SPARK mode on"

    def test_extracted_block_that_does_not_build_fails_the_check(self, tmp_path):
        """A block that does not compile must be reported as an error when the
        check is driven from the extraction step too.

        Without this the tests above could all pass on a seam that reports
        success whatever the compiler said.  The block is syntactically valid,
        so it chops and passes the syntax check and only the build can fail.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: ada project=ExtractedBadBuild main={} compile_button".format(
                self._MAIN),
            self._BROKEN_ADA_BODY, "ExtractedBadBuild")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert ccb.check_code_block_json(json_file) is True, \
            "an extracted block that does not compile must be reported as an error"

        recorded = self._recorded_checks(block_dir)
        assert recorded["SYNTAX"]["status_ok"] is True, \
            "the block must be syntactically valid, or the build is not what failed"
        assert recorded["BUILD"]["status_ok"] is False, \
            "the failure must be recorded against the build"
        assert self._MISSING_NAME in self._log_of(block_dir, recorded["BUILD"]), \
            "the build log must name what the compiler could not resolve"

    def test_extracted_block_expecting_a_compile_error_passes(self, tmp_path):
        """A block declared as expecting a compile error must pass the check
        even though the compiler rejects it.

        The class that declares the expectation is written in the RST source,
        so it has to survive extraction and reach the checker; if it did not,
        this block would be reported as a failure.  The build log is checked
        as well, because a class that suppressed the build entirely would give
        the same answer for the wrong reason.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: ada project=ExtractedExpectError main={} compile_button".format(
                self._MAIN),
            self._BROKEN_ADA_BODY, "ExtractedExpectError",
            classes="ada-expect-compile-error")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert "ada-expect-compile-error" in info["classes"], \
            "the class written in the RST source must reach the checker"

        assert ccb.check_code_block_json(json_file) is False, \
            "a compile error the block declared it expects must not fail the check"

        recorded = self._recorded_checks(block_dir)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "an expected compile error must still be syntax-checked and built"
        assert recorded["BUILD"]["status_ok"] is True, \
            "a compile error the block expects must not be recorded as a failure"
        assert self._MISSING_NAME in self._log_of(block_dir, recorded["BUILD"]), \
            "the compiler must really have rejected the block, or the " \
            "expectation was satisfied by nothing happening"

    def test_c_run_button_block_is_built_and_run_as_extracted(self, tmp_path):
        """A run button on a C block carries through to the program running.

        C blocks take a different route on both sides of the seam: the
        extraction step chops them from the file names written into the source
        rather than by calling gnatchop, and the checker compiles and links
        them with the C compiler instead of the project builder.  The output
        pinned below is what the author's code prints.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: c project=ExtractedCRun main={} run_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCRun")

        assert self._buttons_asked_for(info) == (True, True, False), \
            "a run button must reach the checker as a run, which implies a " \
            "compile, and not as a proof"
        assert info["source_files"] == [self._C_MAIN], \
            "the C source must have been chopped out under the name the block " \
            "declares for it"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted C block as it stands"

        recorded = self._recorded_checks(block_dir)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "a C run button must be syntax-checked, built and run, and not proved"
        assert self._log_of(block_dir, recorded["RUN"]).strip() == self._C_RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"

    @pytest.mark.xfail(
        strict=True,
        reason="a C block asking only for a compile is never given a main file "
               "by the extraction step, and the checker asserts it has one",
    )
    def test_c_compile_button_block_is_built_as_extracted(self, tmp_path):
        """A compile button on a C block must be compiled.

        Tracking note -- this currently fails.  The extraction step resolves a
        main file only for blocks that are also run, but the checker's C
        compile step names the executable after that main file and asserts it
        is set, so a C block asking only for a compile stops the check with an
        assertion instead of compiling.  An Ada block in the same position is
        fine, because the project builder takes the main from the generated
        project rather than from the field.  Resolving a main file for every
        compiled block, or naming the executable some other way, fixes it;
        when it lands this test passes and the marker must be removed.
        """
        block_dir, info, json_file = self._extract(
            tmp_path,
            ".. code:: c project=ExtractedCCompile main={} compile_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCCompile")

        assert self._buttons_asked_for(info) == (True, False, False), \
            "a compile button must reach the checker as a compile and nothing else"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted C block as it stands"

        recorded = self._recorded_checks(block_dir)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "a C compile button must be syntax-checked and built, and neither " \
            "run nor proved"
        assert recorded["BUILD"]["status_ok"] is True
