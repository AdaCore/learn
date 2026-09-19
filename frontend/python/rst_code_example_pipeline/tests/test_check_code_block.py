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
- each prove button, and each prove class an author writes, selects the gnatprove
  switches it names and no others -- read off the recorded command line, since the
  fixture block proves cleanly under any switches at all; the plain prove button and
  the plain prove class select neither switch, which is what says the others were
  selected rather than always present
- verbose cache-skip path: status_ok=True in cache + verbose=True → "already checked" printed
- all_diagnostics flag: a clean Ada compile announces the block, reports SUCCESS and prints no diagnostics
- a corrupt (unparseable) cache file on disk does not crash the check
- an unrecognized language value takes neither the Ada nor the C branch anywhere,
  and has neither a build nor a run recorded for it
- the maximum-columns setting reaches the Ada syntax check, and the limit applied
  is the one that was asked for
- a toolchain binary missing from PATH falls back to an unknown-version marker instead of aborting the check
- each of the three clean-up commands an Ada compile and run reaches is reported
  separately when it fails, the gnatprove --clean one naming the command it ran,
  and none of the failures affects the result
- an rm -f clean-up failure after a successful C compile and run is logged without affecting the result
- a run with no executable to run is reported as a failed run and recorded as one,
  in both languages, and the run-expect-failure classes do not absorb it
- check_block() driven by the real extraction step rather than by a hand-built block:
  the compile, run and prove buttons an author writes in an RST directive, plus the
  C run path and the ada-expect-compile-error class, each carry through to the checks
  actually performed; a C block asking to be run by class alone, with no button
  anywhere, is really built and run -- including one declaring it expects the run to
  fail, whose handling was reachable only through a run button before -- and c-norun
  takes a run away again; an extracted block that does not build is reported as an error;
  and an extracted C block asking only for a compile is compiled without being
  linked, while one that is also run is still linked into an executable named
  after its main (requires the Ada toolchain).  These subsume the hand-built
  happy-path compile, run and prove tests that used to sit alongside them
- the other direction of every expect-error declaration: a block that declared a
  compile error -- in either language -- or a prove error and then produced none
  is reported and fails the check, with the build or the proof recorded as having
  succeeded so that the report is known to come from the unmet expectation rather
  than from anything going wrong; and a block declaring one of those failures, or
  a suppressed run, while asking for no compile, no proof and no run is reported
  for that too.  The three core cases are covered twice over -- from a hand-built
  block and again driven through the real RST directive and the real extraction
  step
- a run class that names the language the block is not written in: each of the
  six spellings is reported, by name, and fails the check -- including on a
  block that a run button separately gets built and run, which is the case a
  report read off what the checker decided to do, rather than off what the
  block declared, would pass over.  The message and the returned value are
  asserted by separate tests, so a report that prints and leaves the run at
  success reddens the second alone.  The controls: the same six classes on the
  language they name, the classes that name a language and are deliberately not
  paired with one (ada-syntax-only, the two no-check spellings), the class that
  names none, and a proof asked for on a C block, which has its own report
  already and must not draw a second.  The three returns that come before the
  declaration checks are pinned as not reporting, since the report sits with
  those checks.  Driven through the real extraction step as well: a C block
  classed ada-run with no button is neither built nor run and fails the check,
  and an Ada block classed c-norun keeps the run its button asked for
- the arm of the previous-check lookup that does not read the record: with the
  lookup switched off, a recorded failure is neither returned nor announced, and
  the block is checked again although the checks were not forced
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


def _check_record(directory, block_record):
    """The record a check wrote, found as the JSON file beside the block that
    is not the one the check was handed.

    A check names that file itself, from the package's own default, so a test
    spelling the name out here would restate a choice the package is free to
    change -- and would go on passing if the check stopped writing a record
    at all, as long as a file of the expected name happened to be lying there
    from something else.
    """
    handed = os.path.realpath(str(block_record))
    written = sorted(path for path in directory.glob("*.json")
                     if os.path.realpath(str(path)) != handed)
    assert len(written) == 1, \
        "expected the check to write exactly one record beside the block, " \
        "got {}".format([path.name for path in written])
    return written[0]


def _reported(block, captured) -> list[str]:
    """The messages a check produced for this block, with the location prefix
    stripped off.

    Matched on the prefix the checker builds for the block under test, so a
    message about some other block could not be mistaken for one of these --
    and so the wording asserted against it is only the part a course author
    reads as the explanation.
    """
    prefix = "at {}:{} (code block hash: {}): ".format(
        block.rst_file, block.line_start, block.text_hash_short)
    return [line.split(prefix, 1)[1]
            for line in captured.out.splitlines() if prefix in line]


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

# The smallest Ada program that compiles and runs, shared by every test that
# needs a source file but does not care what it contains.
MINIMAL_ADA_SOURCE = """\
procedure Main is
begin
   null;
end Main;
"""


def _ada_source_with_a_line_of_width(width: int) -> str:
    """A syntactically valid Ada program whose declaration line is exactly
    ``width`` characters across.

    For tests that set a column limit to one side of that width and check
    what the syntax check makes of it.
    """
    head, tail = '   S : constant String := "', '";'
    line = head + "x" * (width - len(head) - len(tail)) + tail
    assert len(line) == width, \
        "the source line must be exactly the width the test asked for"
    return "procedure Main is\n{}\nbegin\n   null;\nend Main;\n".format(line)


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
    def test_cache_hit_returns_false(self, work_dir):
        """Prior check with status_ok=True and force_checks=False → return False."""
        block = _make_block(buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        # Write a fake block_checks.json in the same directory
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
    def test_cached_failure_returns_true(self, work_dir):
        """Prior check with status_ok=False and force_checks=False → return True."""
        block = _make_block(buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        bc = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        bc.status_ok = False
        bc.to_json_file()

        result = ccb.check_block(block, json_file, force_checks=False)
        assert result is True

    def test_cached_none_status_ok_reruns(self, work_dir):
        """status_ok=None in the cache means previous run was incomplete.
        The code does `not ref_block_check.status_ok` which evaluates None as
        falsy — so has_error=True and we return True. Verify this edge case."""
        block = _make_block(buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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
    def test_corrupt_cache_file_is_ignored(self, work_dir):
        """A previous-check cache file that is not valid JSON must not crash
        check_block(): the read failure is caught, no cached result is used,
        and a full check runs and completes normally instead."""
        block = _make_block(buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        # Let the package write the cache file, so that the corrupt one lands
        # under the name the read is going to look for.  Named here instead,
        # a rename would leave nothing to be read and this test would pass
        # over a check that never met a corrupt file at all.
        _checks_mod.BlockCheck(text_hash=block.text_hash,
                               text_hash_short=block.text_hash_short).to_json_file()
        _check_record(work_dir, json_file).write_text("{not valid json")

        result = ccb.check_block(block, json_file)
        assert result is False, \
            "An unparseable cache file must be ignored rather than crash the check"


# ---------------------------------------------------------------------------
# check_block() with the checks forced against a populated cache
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockForceChecks:
    def test_forcing_the_checks_overrides_a_cached_failure(self, work_dir):
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
        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            no_check=False,
            syntax_only=False,
            source_files=["main.adb"],
        )
        json_file = str(work_dir / "block_info.json")
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

        rewritten = json.loads(_check_record(work_dir, json_file).read_text())
        assert rewritten["status_ok"] is True, \
            "the forced run must replace the stale record with its own result"
        assert "SYNTAX" in rewritten["checks"], \
            "the forced run must have checked the block, not skipped it"


# ---------------------------------------------------------------------------
# check_block() with the previous-check lookup switched off
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockPreviousCheckLookupDisabled:
    def test_a_recorded_result_is_ignored_when_the_lookup_is_switched_off(
            self, work_dir, monkeypatch):
        """With the previous-check lookup switched off, a record beside the
        block must not be consulted at all.

        The module carries a switch that decides whether a block already
        carrying a record is skipped.  It ships on, so every other test in
        this file exercises only the arm that reads the record -- and the arm
        that does not was never entered by anything.

        The fixture is deliberately the same one TestCheckBlockCacheHitFail
        uses: a clean, checkable block with a record beside it saying the
        block failed, and the checks *not* forced.  That test pins the
        recorded failure being handed straight back.  Here the answer has to
        be the one the block earns instead, and the record left behind has to
        carry this run's own result and the checks it performed -- because the
        outcome alone cannot tell a re-check apart from a lookup that happened
        to find nothing.
        """
        monkeypatch.setattr(ccb, "LOOK_FOR_PREVIOUS_CHECKS", False)

        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            no_check=False,
            syntax_only=False,
            source_files=["main.adb"],
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        stale = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        stale.status_ok = False
        stale.to_json_file()

        result = ccb.check_block(block, json_file, force_checks=False)
        assert result is False, \
            "with the lookup switched off, a recorded failure must not be " \
            "returned even though the checks were not forced"

        rewritten = json.loads(_check_record(work_dir, json_file).read_text())
        assert rewritten["status_ok"] is True, \
            "the run must replace the stale record with its own result"
        assert "SYNTAX" in rewritten["checks"], \
            "the run must have checked the block, not skipped it"

    def test_the_block_is_not_announced_as_already_checked(
            self, work_dir, monkeypatch, capsys):
        """The message a skipped block gets must not be printed when the
        lookup is switched off.

        Asserted separately from the result above because the skip prints
        before it returns: a lookup that still ran and still reported the
        block as already checked, but whose result was then discarded, would
        satisfy the assertions above and be visible only here.  Verbose mode
        is asked for, since that is the setting under which the message is
        produced at all -- and it has to be asked for in the call, because the
        module global of that name is only the default the function was
        defined with and assigning to it afterwards changes nothing.
        """
        monkeypatch.setattr(ccb, "LOOK_FOR_PREVIOUS_CHECKS", False)

        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            no_check=False,
            syntax_only=False,
            source_files=["main.adb"],
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        recorded = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        recorded.status_ok = True
        recorded.to_json_file()

        ccb.check_block(block, json_file, verbose=True, force_checks=False)
        captured = capsys.readouterr()
        assert "already checked" not in captured.out, \
            "with the lookup switched off, no block may be announced as " \
            "already checked: {}".format(captured.out)


# ---------------------------------------------------------------------------
# T-check_code_block-05: check_block() with no buttons (BUTTONS check failure)
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockNoButtons:
    def test_empty_buttons_returns_true(self, work_dir):
        """A block with empty buttons list must fail the BUTTONS check."""
        # The block asks for nothing but the button validation: it is
        # neither no-check nor syntax-only, so the check runs to the end; it
        # declares no source files, so the syntax check has nothing to look
        # at; and it asks for no compile and no proof.  Forcing the checks
        # keeps a cached result from short-circuiting all of that.

        block = _make_block(buttons=[], syntax_only=False, no_check=False)
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "check_block() must return True (has_error) when buttons list is empty"

    def test_empty_buttons_prints_error(self, work_dir, capsys):
        """The diagnostic must name the offending block and say what was
        missing, since that text is all a course author gets to act on."""
        block = _make_block(buttons=[], syntax_only=False, no_check=False)
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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

    def test_valid_ada_syntax_returns_false(self, work_dir):
        """A syntactically correct Ada block must pass the syntax check."""
        # Write source file
        src = work_dir / "main.adb"
        src.write_text(self.ADA_SOURCE)

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["main.adb"],
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A syntactically valid Ada block must not produce an error"

    def test_invalid_ada_syntax_returns_true(self, work_dir):
        """A syntactically invalid Ada block must fail the syntax check."""
        bad_source = "this is not ada;\n"
        src = work_dir / "bad.adb"
        src.write_text(bad_source)

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["bad.adb"],
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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
    def test_valid_nocheck_block_json_returns_false(self, work_dir):
        """check_code_block_json() on a no-check block must return False."""
        block = _make_block(classes=["ada-nocheck"], no_check=True, buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)
        result = ccb.check_code_block_json(json_file)
        assert result is False


# ---------------------------------------------------------------------------
# T-check_code_block-08: selected toolchain + non-no button validation
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockSelectedToolchainButtonValidation:
    def test_selected_gnat_with_compile_button_fails_buttons_check(self, work_dir):
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
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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
# Covers the compile step for a C block.
# Requires gcc in PATH (part of the Ada toolchain).
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockCCompile:
    """Tests that actually invoke gcc on C source files."""

    VALID_C_SOURCE = "int main(void) { return 0; }\n"
    INVALID_C_SOURCE = "this is not C at all !@#$\n"

    def test_c_compile_success(self, work_dir):
        """A valid C file with compile_it=True and buttons=['compile'] must return False."""
        src = work_dir / "main.c"
        src.write_text(self.VALID_C_SOURCE)

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
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A valid C file must compile without error"

    def test_c_compile_failure(self, work_dir):
        """An invalid C file with compile_it=True must return True (has_error)."""
        src = work_dir / "main.c"
        src.write_text(self.INVALID_C_SOURCE)

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
        json_file = str(work_dir / "block_info.json")
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

    def test_ada_expect_compile_error(self, work_dir):
        """A block with classes=['ada-expect-compile-error', 'nosyntax-check']
        and Ada source that fails to compile at the BUILD phase must return False
        (the expected compile failure is not treated as an error)."""
        src = work_dir / "bad.adb"
        src.write_text(self.BAD_BUILD_ADA_SOURCE)
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

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "An expected compile error must not count as a test failure"

    def test_c_run(self, work_dir):
        """A valid C file compiled and run (exits 0) must return False."""
        src = work_dir / "main.c"
        src.write_text(self.VALID_C_SOURCE)

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
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A valid C program that exits 0 must not produce a run error"


# ---------------------------------------------------------------------------
# C3 — TestCheckBlockGnatprove
# Covers the proof step, which only Ada blocks reach.
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

    def test_ada_gnatprove_language_c_else(self, work_dir):
        """A block with language="c" and prove_it=True must return True:
        proving only supports Ada, so a non-Ada block takes the "wrong
        language selected for prove button" error branch instead of
        invoking gnatprove."""

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

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "C language with prove_it=True must return True (unsupported)"

    def test_ada_gnatprove_pinned_legacy_version(self, work_dir):
        """A prove block pinned to a specific, genuinely installed legacy
        GNATprove version must build the older-style command line that
        version expects, and a real invocation with it must still prove the
        example cleanly."""
        src = work_dir / "main.adb"
        src.write_text(self.SPARK_SOURCE)

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

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "A provable SPARK block must prove cleanly under a pinned legacy GNATprove version"

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        proved_with = ast.literal_eval(recorded["PROVE"]["cmdline"])
        assert "--no-axiom-guard" in proved_with, \
            "the older command line must ask for the switch only that " \
            "generation understands: {}".format(proved_with)
        assert "--checks-as-errors" in proved_with, \
            "the older command line must spell the checks-as-errors switch " \
            "the way that generation accepts it: {}".format(proved_with)
        assert "--function-sandboxing=off" not in proved_with, \
            "the older command line must not carry a switch introduced " \
            "after it: {}".format(proved_with)


# ---------------------------------------------------------------------------
# Unrecognized-language paths
# Covers cleanup/syntax-check/compile/run all falling through without taking
# either the Ada or the C branch, and without crashing.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockUnrecognizedLanguage:
    def test_unrecognized_language_takes_neither_branch(self, tmp_path,
                                                        monkeypatch):
        """A block whose language is neither 'ada' nor 'c' must fall through
        the syntax-check, compile and run steps without taking either
        language-specific branch, and must complete without raising.

        The block asks for a compile and a run, and names the main file a
        language branch would need, so that a branch wrongly taken would have
        enough to proceed rather than tripping over missing state: the check
        has to skip it on the language alone.  Three things then show it did.
        No command but the toolchain version probes is run -- a branch taken
        would invoke a compiler -- and the record left behind carries neither
        a BUILD nor a RUN phase.  A BUILD phase is only added from inside a
        language branch.  A RUN phase is only added when a run was really
        attempted, which is also only decided inside a language branch: a
        recorded RUN for a language the checker does not run would claim a
        successful run of a command that was never built, and would name a
        log file that was never written.
        """
        import subprocess as S

        commands = []
        real_check_output = S.check_output

        def recording_check_output(args, *rest, **kwargs):
            commands.append(list(args))
            return real_check_output(args, *rest, **kwargs)

        monkeypatch.setattr(S, "check_output", recording_check_output)

        block = _make_block(
            language="fortran",
            no_check=False,
            syntax_only=False,
            compile_it=True,
            run_it=True,
            source_files=["main.f90"],
        )
        block.project_main_file = "main.f90"
        json_file = str(tmp_path / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)

        assert all(command[1:2] == ["--version"] for command in commands), \
            "only the toolchain version probes may run for a language the " \
            "check does not know: {}".format(commands)

        recorded = json.loads(
            _check_record(tmp_path, json_file).read_text())["checks"]
        assert "BUILD" not in recorded, \
            "a compile was asked for, so a recorded BUILD phase means a " \
            "language branch was taken: {}".format(sorted(recorded))

        assert "RUN" not in recorded, \
            "a run was asked for, but no language branch could attempt one, " \
            "so a recorded RUN phase describes a run that never happened: " \
            "{}".format(sorted(recorded))

        assert result is False, \
            "An unrecognized language must not raise and must not report an error"


# ---------------------------------------------------------------------------
# Verbose / all_diagnostics paths
# Covers the verbose cache-skip output and the all_diagnostics output path.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockVerbose:
    """Tests for verbose and all_diagnostics flag paths."""

    def test_verbose_cache_skip(self, work_dir, capsys):
        """With verbose=True and a cached status_ok=True, check_block must print
        'already checked. Skipping...' (exercises the verbose cache-hit path)."""
        block = _make_block(buttons=["no"])
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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

    def test_all_diagnostics_flag(self, work_dir, capsys):
        """With all_diagnostics=True and verbose=True, a clean Ada compile must
        announce the block it is checking, report success, and print no
        diagnostics at all."""
        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
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

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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
# Covers the maximum-columns setting reaching the Ada syntax check, and the
# limit actually applied being the one that was asked for.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockMaxColumns:
    """The maximum-columns setting reaches the Ada syntax check.

    The syntax check already asks for the compiler's own style rules, and
    those carry a column limit of their own, narrower than the one either
    test below sets.  So a block that is wider than the limit it is given
    proves nothing on its own -- it would be reported either way -- and only
    the block that is *narrower* than the limit it is given can show that the
    setting was passed on at all.  The two tests together pin both halves:
    that the limit is applied, and that it is the one that was asked for.
    """

    #: How wide the one long line of the source below is.  Both tests set a
    #: limit to one side of it, and both limits are above the compiler's own.
    LINE_WIDTH = 90

    def _check_under_limit(self, work_dir, max_columns: int) -> bool:
        """Syntax-check a block holding one LINE_WIDTH-wide line under the
        given column limit, and return whether the check reported an error."""
        (work_dir / "main.adb").write_text(
            _ada_source_with_a_line_of_width(self.LINE_WIDTH))

        block = _make_block(
            buttons=["no"],
            syntax_only=True,
            no_check=False,
            source_files=["main.adb"],
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        return ccb.check_block(block, json_file, max_columns=max_columns,
                               force_checks=True)

    def test_line_within_the_column_limit_passes(self, work_dir):
        """A line narrower than the limit asked for must pass the syntax
        check, even though it is wider than the compiler's own limit.  Nothing
        but the setting having been passed on can make that happen."""
        assert self._check_under_limit(
            work_dir, self.LINE_WIDTH + 10) is False, \
            "a line of {} characters must pass a limit of {}".format(
                self.LINE_WIDTH, self.LINE_WIDTH + 10)

    def test_line_beyond_the_column_limit_fails(self, work_dir):
        """A line wider than the limit asked for must fail the syntax check,
        so that the limit applied is the one that was asked for rather than
        some other one that happens to be set."""
        assert self._check_under_limit(
            work_dir, self.LINE_WIDTH - 10) is True, \
            "a line of {} characters must not pass a limit of {}".format(
                self.LINE_WIDTH, self.LINE_WIDTH - 10)


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

    def _setup_project(self, work_dir, source):
        src = work_dir / "main.adb"
        src.write_text(source)
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

    def test_run_success_with_expect_failure_class(self, work_dir):
        """A program that exits 0 while marked ada-run-expect-failure must
        return True: the run succeeded when a failure was expected."""
        project_filename = self._setup_project(work_dir, self.VALID_ADA_SOURCE)
        block = self._make_run_block(classes=["ada-run-expect-failure"])
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True

    def test_ada_run_fail_with_expect_failure_class(self, work_dir, capsys):
        """A program that exits non-zero while marked ada-run-expect-failure
        must return False: the failure was expected. With verbose enabled,
        the expected-failure message is printed."""
        project_filename = self._setup_project(work_dir, self.FAILING_ADA_SOURCE)
        block = self._make_run_block(classes=["ada-run-expect-failure"])
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        ccb.verbose = True
        result = ccb.check_block(block, json_file, verbose=True, force_checks=True)
        assert result is False
        out = capsys.readouterr().out
        assert "Running of example expectedly failed" in out

    def test_ada_run_fail_with_expect_failure_class_says_nothing_quietly(
            self, work_dir, capsys):
        """The expected-failure message belongs to the verbose run only.

        The sibling above drives the same path with verbose enabled and
        asserts the message; without this one, nothing says the message is
        conditional at all, and the quiet run -- the one every real check
        makes -- would go unexercised.  Its C counterpart is reached by the
        extractor-driven expect-failure test further down, which runs quiet.
        """
        project_filename = self._setup_project(work_dir, self.FAILING_ADA_SOURCE)
        block = self._make_run_block(classes=["ada-run-expect-failure"])
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "a failure the block expects must not be reported as an error"
        out = capsys.readouterr().out
        assert "Running of example expectedly failed" not in out, \
            "the expected-failure message must be held back on a quiet " \
            "run: {}".format(out)
        assert "Running of example failed" not in out, \
            "an expected failure must not be reported as an unexpected one " \
            "either: {}".format(out)

    def test_ada_run_fail_without_expect_failure(self, work_dir):
        """A program that exits non-zero without ada-run-expect-failure must
        return True: an unexpected run failure."""
        project_filename = self._setup_project(work_dir, self.FAILING_ADA_SOURCE)
        block = self._make_run_block()
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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

    def test_c_run_fail_with_expect_failure_class(self, work_dir, capsys):
        """A C program that exits non-zero while marked c-run-expect-failure
        must return False: the failure was expected. With verbose enabled,
        the expected-failure message is printed."""
        src = work_dir / "main.c"
        src.write_text(self.FAILING_C_SOURCE)

        block = self._make_c_run_block(classes=["c-run-expect-failure"])
        block.project_main_file = "main.c"
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        ccb.verbose = True
        result = ccb.check_block(block, json_file, verbose=True, force_checks=True)
        assert result is False
        out = capsys.readouterr().out
        assert "Running of example expectedly failed" in out

    def test_c_run_success_with_expect_failure_class(self, work_dir):
        """A C program that exits 0 while marked c-run-expect-failure must
        return True: the run succeeded when a failure was expected."""
        src = work_dir / "main.c"
        src.write_text(self.VALID_C_SOURCE)

        block = self._make_c_run_block(classes=["c-run-expect-failure"])
        block.project_main_file = "main.c"
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True

    def test_c_run_fail_without_expect_failure(self, work_dir):
        """A C program that exits non-zero without c-run-expect-failure must
        return True: an unexpected run failure."""
        src = work_dir / "main.c"
        src.write_text(self.FAILING_C_SOURCE)

        block = self._make_c_run_block()
        block.project_main_file = "main.c"
        json_file = str(work_dir / "block_info.json")
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

    def test_c_compile_error_expected(self, work_dir):
        """A C file that fails to compile while marked c-expect-compile-error
        must return False: the compile failure was expected.

        nosyntax-check is also set: for C, the SYNTAX phase and the BUILD
        phase both invoke gcc on the same source, so a genuine syntax error
        would already fail (as an unexpected error) during SYNTAX before the
        BUILD phase's c-expect-compile-error handling is ever reached -- the
        same reason the analogous ada-expect-compile-error test bypasses the
        SYNTAX phase."""
        src = work_dir / "main.c"
        src.write_text(self.INVALID_C_SOURCE)

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
        json_file = str(work_dir / "block_info.json")
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

    def _setup_spark_project(self, work_dir):
        src = work_dir / "main.adb"
        src.write_text(self.FAILING_SPARK_SOURCE)
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

    def test_prove_failure_expected(self, work_dir):
        """SPARK code that fails to prove while marked ada-expect-prove-error
        must return False: the failure was expected."""
        spark_project_filename = self._setup_spark_project(work_dir)
        block = self._make_prove_block(classes=["ada-expect-prove-error"])
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False

    def test_prove_failure_unexpected(self, work_dir):
        """SPARK code that fails to prove without ada-expect-prove-error must
        return True: an unexpected prove failure."""
        spark_project_filename = self._setup_spark_project(work_dir)
        block = self._make_prove_block()
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True


# ---------------------------------------------------------------------------
# TestCheckBlockExpectedErrorThatNeverHappened
# Covers the other direction of every expect-error declaration: the checker
# has to report a block that declared a failure and then did not produce one.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockExpectedErrorThatNeverHappened:
    """A block declaring "this must fail" that succeeds instead.

    This is the direction the checker exists for.  A course example marked as
    expecting a compile error, or a proof failure, is not being checked for
    the failure it produces -- it is being watched in case it silently stops
    producing one, which is what happens when the example is repaired, the
    class is left on it, and nobody notices that the block is now asserting
    something untrue about the language.  The suite covered only the arm where
    the declared failure really occurs, so a checker that dropped these
    reports entirely would have stayed green.

    Each test asserts the outcome, the message a course author is given to act
    on, and -- where the block reaches a compiler or the prover -- that the
    phase itself was recorded as having succeeded.  That last one is what
    distinguishes the report under test from the block having failed for some
    other reason: the build or the proof went through, and it is the button
    validation that objected.
    """

    CLEAN_SPARK_SOURCE = """\
procedure Main with SPARK_Mode is
begin
   null;
end Main;
"""

    VALID_C_SOURCE = "int main(void) { return 0; }\n"

    def test_a_compile_error_that_did_not_happen_is_reported(
            self, work_dir, capsys):
        """Source that compiles cleanly under ada-expect-compile-error must
        fail the check.

        The block asks for a compile and gets one; the compiler is happy, so
        the declared error never arrives.  The build is recorded as having
        succeeded, which is what says the report comes from the expectation
        being unmet rather than from anything having gone wrong.
        """
        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=[],
            spark_mode=False,
        )

        block = _make_block(
            classes=["ada-expect-compile-error"],
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block declaring it expects a compile error must fail the " \
            "check when the source compiles"

        assert "Expected compile error, got none!" in \
            _reported(block, capsys.readouterr()), \
            "the check must say that the declared compile error never arrived"

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert recorded["BUILD"]["status_ok"] is True, \
            "the build must have succeeded, or the failure under test is not " \
            "the missing compile error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

    def test_a_c_compile_error_that_did_not_happen_is_reported(
            self, work_dir, capsys):
        """C source that compiles cleanly under c-expect-compile-error must
        fail the check.

        The C spelling of the test above, and for a long time the only one of
        the expect-error declarations that bought the author nothing: a C
        block marked "this must not compile" whose code the compiler accepted
        was reported as a success, so an example repaired without its class
        being taken off went on passing.  The compile step raises the same
        flag for either language, so the question asked here is the same
        question -- and the build is recorded as having succeeded, which is
        what says the report comes from the expectation being unmet rather
        than from anything having gone wrong.
        """
        src = work_dir / "main.c"
        src.write_text(self.VALID_C_SOURCE)

        block = _make_block(
            language="c",
            classes=["c-expect-compile-error"],
            buttons=["compile"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=False,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a C block declaring it expects a compile error must fail the " \
            "check when the source compiles"

        assert "Expected compile error, got none!" in \
            _reported(block, capsys.readouterr()), \
            "the check must say that the declared compile error never arrived"

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert recorded["BUILD"]["status_ok"] is True, \
            "the build must have succeeded, or the failure under test is not " \
            "the missing compile error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

    def test_a_prove_error_that_did_not_happen_is_reported(
            self, work_dir, capsys):
        """SPARK code that proves cleanly under ada-expect-prove-error must
        fail the check.

        The mirror of TestCheckBlockProveFailure.test_prove_failure_expected,
        which pins the arm where the proof really does fail.  Here the prover
        is satisfied, so the declared failure never arrives; the proof is
        recorded as having succeeded, which is what says the report comes from
        the expectation being unmet.
        """
        src = work_dir / "main.adb"
        src.write_text(self.CLEAN_SPARK_SOURCE)
        spark_project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

        block = _make_block(
            classes=["ada-expect-prove-error"],
            buttons=["prove"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
        )
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block declaring it expects a prove error must fail the check " \
            "when the proof succeeds"

        assert "Expected prove error, got none!" in \
            _reported(block, capsys.readouterr()), \
            "the check must say that the declared prove error never arrived"

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert recorded["PROVE"]["status_ok"] is True, \
            "the proof must have succeeded, or the failure under test is not " \
            "the missing prove error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

    def test_expecting_a_compile_error_with_nothing_that_compiles_is_reported(
            self, work_dir, capsys):
        """A block expecting a compile error while asking for no compile must
        be reported.

        Nothing in the block gives the checker a way to produce the error it
        declares: there is no compile and no run button, and neither of the
        classes that ask for one.  Both objections are asserted, because both
        are true of this block and each is a separate report -- the missing
        button or class, and, unavoidably, the compile error that no compile
        could have produced.
        """
        block = _make_block(
            classes=["ada-expect-compile-error"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block expecting a compile error with nothing to compile must " \
            "fail the check"

        reported = _reported(block, capsys.readouterr())
        assert "Expected compile or run button/class, got none!" in reported, \
            "the check must say the block asks for no compile: {}".format(
                reported)
        assert "Expected compile error, got none!" in reported, \
            "the check must also say the declared compile error never " \
            "arrived: {}".format(reported)

    def test_expecting_a_prove_error_without_a_proof_is_reported(
            self, work_dir, capsys):
        """A block expecting a prove error while asking for no proof must be
        reported.

        The class alone does not ask for a proof, so the block declares a
        failure the checker is never given the chance to observe.  Only the
        missing prove button is reported: the arm that reports the missing
        failure itself sits behind the proof having been asked for.
        """
        block = _make_block(
            classes=["ada-expect-prove-error"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
        )
        assert block.prove_it is False, \
            "the expect-prove-error class must not by itself ask for a " \
            "proof, or this test is not about a block that asks for none"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block expecting a prove error without a proof must fail the " \
            "check"

        reported = _reported(block, capsys.readouterr())
        assert "Expected prove button, got none!" in reported, \
            "the check must say the block asks for no proof: {}".format(
                reported)

    def test_declaring_no_run_without_a_run_button_is_reported(
            self, work_dir, capsys):
        """A block classed ada-norun with no run button must be reported.

        Taking a run away is only meaningful for a block that was going to be
        run, so the checker requires the run to have been asked for -- and
        says so when it was not.
        """
        block = _make_block(
            classes=["ada-norun"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        assert (block.run_it, block.compile_it) == (False, False), \
            "the class must have taken the run away, or this block is being " \
            "built and run rather than only validated"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block classed ada-norun with no run button must fail the check"

        reported = _reported(block, capsys.readouterr())
        assert "Expected run button, got none!" in reported, \
            "the check must say the block asks for no run: {}".format(reported)

    def test_expecting_a_run_failure_without_a_run_button_is_reported(
            self, work_dir, capsys):
        """The same report is due for a block expecting its run to fail.

        Written separately from the ada-norun block above rather than left to
        it: the two class names are read as one set, so a checker that stopped
        recognizing this one would still satisfy the other test.  The run and
        the compile are suppressed so that the block is only validated -- what
        is under test is the declaration, not a program.
        """
        block = _make_block(
            classes=["ada-run-expect-failure"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a block expecting its run to fail with no run button must fail " \
            "the check"

        reported = _reported(block, capsys.readouterr())
        assert "Expected run button, got none!" in reported, \
            "the check must say the block asks for no run: {}".format(reported)


# ---------------------------------------------------------------------------
# TestCheckBlockRunClassNamingTheOtherLanguage
# Covers the report for a run class that names a language the block is not
# written in -- the declaration whose consequence is that nothing happens.
# ---------------------------------------------------------------------------

# The six run classes, each paired with a block of the language it does not
# name.  Kept as one table so that the firing cases and the silent controls
# below are driven by the same list and cannot drift apart.
RUN_CLASSES_AND_THEIR_LANGUAGE = [
    ("ada-run", "ada"),
    ("ada-norun", "ada"),
    ("ada-run-expect-failure", "ada"),
    ("c-run", "c"),
    ("c-norun", "c"),
    ("c-run-expect-failure", "c"),
]

# The other language, for a table of two.
THE_OTHER_LANGUAGE = {"ada": "c", "c": "ada"}

WRONG_LANGUAGE_REPORT = "Wrong language selected for run class '{}'"


def _no_run_class_report(reported: list[str]) -> bool:
    """Whether none of the messages is the wrong-language run-class report.

    Matched on the part of the wording that is common to all six spellings,
    so that a report firing for a class this test did not name is caught as
    well as one firing for the class it did.
    """
    return not any("Wrong language selected for run class" in message
                   for message in reported)


@pytest.mark.toolchain
class TestCheckBlockRunClassNamingTheOtherLanguage:
    """A block carrying a run class that names the language it is not written
    in.

    The class does nothing for such a block -- that is the point of pairing
    each class with its language -- and "does nothing" is exactly the outcome
    this checker exists to prevent from passing quietly.  A C block tagged
    ada-run asks for no run, and therefore for no build, so without a report
    it is checked by nothing and recorded as a success.

    The messages and the exit status are asserted by separate tests here,
    rather than together, because they are separately losable: this package
    already carries a report that prints and leaves the status at zero, so a
    new one that did the same is a real possibility rather than a
    hypothetical, and it must redden the status tests on its own.
    """

    # A C program that announces itself, for the one test whose block is
    # really built and really run.
    C_RUN_OUTPUT = "the mis-classed example ran"

    C_SOURCE_THAT_ANNOUNCES_ITSELF = """\
#include <stdio.h>

int main(void)
{{
   printf("{}\\n");
   return 0;
}}
""".format(C_RUN_OUTPUT)

    @staticmethod
    def _checked(block, work_dir, json_file, **kwargs) -> bool:
        """Write the block out and check it, with the checks forced.

        Forced because a recorded result would otherwise decide the outcome
        on a second run in the same directory, which says nothing about the
        declaration under test.
        """
        block.to_json_file(json_file)
        return ccb.check_block(block, json_file, force_checks=True, **kwargs)

    @pytest.mark.parametrize("code_class,class_language",
                             RUN_CLASSES_AND_THEIR_LANGUAGE)
    def test_a_run_class_naming_the_other_language_is_reported(
            self, code_class, class_language, work_dir, capsys):
        """Each of the six run classes, on a block of the other language,
        must be reported by name.

        One case per class rather than one test over all six: a checker that
        recognized five of them would otherwise still pass.  The message
        names the offending class, so the author is told which word to fix
        rather than only that something is wrong with the block.

        Only the message is asserted.  That the report also fails the check
        is the separate claim held by the tests below, and keeping the two
        apart is what makes a report that prints and returns success redden
        those and not these.
        """
        block = _make_block(
            language=THE_OTHER_LANGUAGE[class_language],
            classes=[code_class],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")

        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert WRONG_LANGUAGE_REPORT.format(code_class) in reported, \
            "the report must name the offending class: {}".format(reported)

    @pytest.mark.parametrize("code_class,class_language",
                             RUN_CLASSES_AND_THEIR_LANGUAGE)
    def test_a_run_class_on_the_language_it_names_is_not_reported(
            self, code_class, class_language, work_dir, capsys):
        """The control for the six above.

        Without it, a report that fired on every run class whatsoever would
        satisfy all six and take every correctly tagged block in the material
        down with it.

        Only the absence of this report is asserted, and deliberately not the
        block's overall result: two of these six classes separately draw the
        pre-existing "Expected run button, got none!" objection, which is a
        behavior recorded as it stands rather than one this test should
        fasten itself to.

        The compile and the run are switched off rather than derived.  On the
        language it names, a run class really does ask for a run, and this
        block has no project and no source behind it -- what is under test is
        the declaration the checker reads, and the derivation it reads it
        through is covered where the derivation lives.
        """
        block = _make_block(
            language=class_language,
            classes=[code_class],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
        )
        json_file = str(work_dir / "block_info.json")

        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert _no_run_class_report(reported), \
            "a run class on the language it names must draw no report: " \
            "{}".format(reported)

    def test_a_c_block_declaring_ada_syntax_only_passes_and_stops_there(
            self, work_dir):
        """The class/language mismatch the material really carries.

        A C block declaring ada-syntax-only sits in the Ada course material
        today.  The syntax-only class names no language as far as the checker
        is concerned, so the block is syntax-checked and stops -- and it must
        go on doing that, or a content build fails on an example that is
        written the way it is on purpose.
        """
        source = work_dir / "main.c"
        source.write_text(self.C_SOURCE_THAT_ANNOUNCES_ITSELF)

        block = _make_block(
            language="c",
            classes=["ada-syntax-only"],
            buttons=["no"],
            no_check=False,
            source_files=["main.c"],
        )
        assert block.syntax_only is True, \
            "the class must still make the block syntax-only, or this is not " \
            "the block the material carries"

        json_file = str(work_dir / "block_info.json")
        assert self._checked(block, work_dir, json_file) is False, \
            "the C block the material carries must pass the check"

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert sorted(recorded) == ["SYNTAX"], \
            "a syntax-only block must be syntax-checked and nothing else"

    @pytest.mark.parametrize("code_class,language", [
        ("ada-syntax-only", "c"),
        ("ada-nocheck", "c"),
        ("c-nocheck", "ada"),
        ("nosyntax-check", "ada"),
    ])
    def test_a_class_that_is_not_a_run_class_is_never_reported(
            self, code_class, language, work_dir, capsys):
        """The classes that name a language in their spelling and are
        deliberately not paired with one, plus the one that names none.

        This is the control that a report written as a scan over class names
        beginning with "ada-" or "c-" fails.  Two of these are not idle
        worries: the syntax-only case is a block the material carries, and
        the two no-check spellings are documented as the Ada one and the C
        one while being read for either language -- a difference that was
        looked at and deliberately left alone, so a report firing here would
        quietly take the opposite decision.

        The block is built with the two early returns switched off, so that
        it reaches the declaration checks and the report is really consulted.
        A block declaring one of these classes would otherwise return before
        the report could fire, and this control would hold nothing.
        """
        block = _make_block(
            language=language,
            classes=[code_class],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")

        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert _no_run_class_report(reported), \
            "a class that is not a run class must draw no report: {}".format(
                reported)

    def test_a_prove_class_on_a_c_block_draws_only_the_prove_report(
            self, work_dir, capsys):
        """A proof asked for on a C block is already reported, and must not
        be reported twice.

        The prove classes name a language in their spelling too, and the
        checker has objected to a proof on a non-Ada block all along.  A
        second report saying the same thing in different words would leave an
        author looking for two mistakes where there is one.
        """
        block = _make_block(
            language="c",
            classes=["ada-prove"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
        )
        assert block.prove_it is True, \
            "the class must ask for a proof, or the existing report is not " \
            "the one being reached"

        json_file = str(work_dir / "block_info.json")
        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert "Wrong language selected for prove button" in reported, \
            "the existing report must still be made: {}".format(reported)
        assert _no_run_class_report(reported), \
            "the same mistake must not be reported a second time: {}".format(
                reported)

    def test_a_mis_classed_block_that_a_run_button_rescues_is_still_reported(
            self, work_dir, capsys):
        """A C block classed ada-run that also carries a run button.

        This is the case that separates a report read off the block's
        declarations from one read off what the checker decided to do with
        them.  The button asks for the run the class failed to ask for, so
        the block really is built and really is run, and nothing about the
        outcome is wrong -- yet the class still names a language the block is
        not written in, and the author still has a word to fix.

        A report derived from "a run class is present and the block is not
        being run" passes every other test in this file and fails this one.

        The build and the run are asserted as having happened, with the
        program's own output read back out of the run log, so that the report
        is known to come from a block the checker fully processed rather than
        from one it quietly skipped.

        Like its siblings above this asserts the message and not the returned
        value; the status side of this same case is held through the
        installed command, where a block with a run button is checked end to
        end.
        """
        source = work_dir / "main.c"
        source.write_text(self.C_SOURCE_THAT_ANNOUNCES_ITSELF)

        block = _make_block(
            language="c",
            classes=["ada-run"],
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"
        assert (block.run_it, block.compile_it) == (True, True), \
            "the button must have asked for the run the class did not, or " \
            "this is not the case under test"

        json_file = str(work_dir / "block_info.json")
        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert WRONG_LANGUAGE_REPORT.format("ada-run") in reported, \
            "the class must be reported although the block was run: " \
            "{}".format(reported)

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "the block must really have been built and run"
        assert recorded["RUN"]["status_ok"] is True, \
            "the run itself must have succeeded, or the report cannot be " \
            "attributed to the declaration"
        assert (work_dir / recorded["RUN"]["logfile"]).read_text().strip() == \
            self.C_RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"

    # The three returns that used to come before the report, each now a
    # decision of its own rather than one rule applied to all three.
    #
    # The report is read off the declaration, so it is made before the
    # recorded result is consulted, and only the block that asked for no
    # checking at all escapes it.  A block declaring a no-check class asked
    # for exactly that and is still skipped in silence.  A syntax-only block
    # asked for less checking, not for none, and is reported.  A block with a
    # recorded result asked for nothing less at all -- the record is a cache,
    # and it is keyed on a hash of the block's text, so editing only the
    # class leaves the key untouched and hands back the success recorded for
    # the declaration the block had before the edit.  That is the one outcome
    # this report exists to prevent, so it is made before the record is read.
    #
    # Each of the three is pinned below, so that moving the report past one
    # of them reddens a test naming the path it was moved past.

    def test_a_mis_classed_block_declaring_no_check_is_not_reported(
            self, work_dir, capsys):
        """A block declaring a no-check class is skipped before the report.

        Nothing is checked and nothing is recorded, so the mis-classed run
        class beside it goes unmentioned.
        """
        block = _make_block(
            language="c",
            classes=["ada-nocheck", "ada-run"],
            buttons=["no"],
        )
        assert block.no_check is True, \
            "the block must be the one the checker skips outright"

        json_file = str(work_dir / "block_info.json")
        assert self._checked(block, work_dir, json_file) is False, \
            "a block declaring no check must still pass"

        assert _no_run_class_report(_reported(block, capsys.readouterr())), \
            "a block the checker never looks at cannot be reported"

    def test_a_mis_classed_block_declaring_syntax_only_is_reported(
            self, work_dir, capsys):
        """A block declaring itself syntax-only is reported.

        Such a block asked for less checking, not for none: its syntax is
        checked and the check stops there.  The class it carries is still a
        word the author has to fix, and it is knowable from the declaration
        without anything being built, so the reduced checking the block asked
        for is no reason to withhold it.

        Only the message is asserted; that the report also fails the check is
        held separately, as it is for every other message test here.
        """
        source = work_dir / "main.c"
        source.write_text(self.C_SOURCE_THAT_ANNOUNCES_ITSELF)

        block = _make_block(
            language="c",
            classes=["ada-syntax-only", "ada-run"],
            buttons=["no"],
            no_check=False,
            source_files=["main.c"],
        )
        assert block.syntax_only is True, \
            "the block must be the one the checker stops after the syntax " \
            "check"

        json_file = str(work_dir / "block_info.json")
        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert WRONG_LANGUAGE_REPORT.format("ada-run") in reported, \
            "a block that stops after the syntax check must still be told " \
            "about the class it carries: {}".format(reported)

    def test_a_mis_classed_block_with_a_recorded_result_is_reported(
            self, work_dir, capsys):
        """A block whose result is already recorded is reported all the same.

        The record is a cache and its key is a hash of the block's text, so
        a class edited without the text being touched hands back the success
        recorded for the declaration the block had before the edit -- which
        is how a mis-classed block comes into existence in the first place.
        The report is read off the declaration and needs nothing the record
        holds, so it is made before the record is consulted.

        The recorded result here says the block passed, so nothing but the
        declaration can account for the report.  Only the message is
        asserted; the status side is held separately.
        """
        block = _make_block(
            language="c",
            classes=["ada-run"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        recorded = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        recorded.status_ok = True
        recorded.to_json_file()  # beside the block, under the package's name

        ccb.check_block(block, json_file)

        reported = _reported(block, capsys.readouterr())
        assert WRONG_LANGUAGE_REPORT.format("ada-run") in reported, \
            "a recorded success must not absorb the report: {}".format(
                reported)

    def test_a_mis_classed_block_is_reported_exactly_once(
            self, work_dir, capsys):
        """The report is made once, on the path that makes every report.

        The class is read at the top of the check and carried to the end,
        where it joins the other declaration objections in the record the
        block leaves behind.  Carrying it as a second print instead would
        tell an author of two mistakes where there is one, and the wording is
        the same both times, so nothing in the message would give the
        duplication away.
        """
        block = _make_block(
            language="c",
            classes=["ada-run"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")

        self._checked(block, work_dir, json_file)

        reported = _reported(block, capsys.readouterr())
        assert reported.count(WRONG_LANGUAGE_REPORT.format("ada-run")) == 1, \
            "one mistake must draw one report: {}".format(reported)


# ---------------------------------------------------------------------------
# TestCheckBlockRunClassNamingTheOtherLanguageFailsTheRun
# The exit status of the report above, held apart from its wording.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockRunClassNamingTheOtherLanguageFailsTheRun:
    """The report has to fail the check, not merely print.

    Kept apart from the wording tests above on purpose.  This package already
    holds a report that prints and leaves the run at success -- the
    wrong-language prove button reported by the extraction step, whose flag
    never reaches that function's return value, recorded in this suite as a
    known defect.  A new report that took the same shape would satisfy every
    message test written above while telling a build that the course checked
    out.  These tests assert the returned value and nothing else, so that
    exact defect reddens them alone.
    """

    def test_check_block_returns_an_error_for_a_mis_classed_block(
            self, work_dir):
        """check_block() itself must return the error, with no reference to
        what it printed."""
        block = _make_block(
            language="c",
            classes=["ada-run"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        assert ccb.check_block(block, json_file, force_checks=True) is True, \
            "a run class naming the other language must fail the check"

    @staticmethod
    def _recording_a_success(block) -> None:
        """Leave a record of a successful check beside the block.

        Written through the package's own writer, so that the record is right
        in every respect and lands under the name the check looks for without
        that name being restated here.
        """
        recorded = _checks_mod.BlockCheck(
            text_hash=block.text_hash,
            text_hash_short=block.text_hash_short,
        )
        recorded.status_ok = True
        recorded.to_json_file()

    def test_a_recorded_success_does_not_absorb_the_error(self, work_dir):
        """A recorded success must not decide the outcome for a block whose
        declaration has gone wrong since it was written.

        This is the case a course author actually meets.  The per-block
        directory is named after a hash of the block's text, the record
        beside it is never compared against the declaration, and nothing
        removes it -- so editing only the class of an example whose body was
        not touched hands back the result of the run before the edit.  The
        default local driver keeps that directory between runs by design, so
        a stale record is the ordinary state there rather than an unusual
        one.

        Asserted on the returned value alone, and with no --force: a report
        that printed and left the value at success would satisfy the message
        test of this same case while telling a build the course checked out.
        """
        block = _make_block(
            language="c",
            classes=["ada-run"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)
        self._recording_a_success(block)

        assert ccb.check_block(block, json_file) is True, \
            "a recorded success must not stand in for a check the block " \
            "can no longer pass"

    def test_a_recorded_success_is_still_handed_back_for_a_sound_block(
            self, work_dir):
        """The control for the test above.

        Reusing a recorded result is what the record is for, and the report
        must not cost every block that has one its reuse.  The same block
        with the class of its own language keeps the recorded success -- and
        keeps it without a compiler being reached, which is the whole point
        of the record.
        """
        block = _make_block(
            language="c",
            classes=["c-run"],
            buttons=["no"],
            syntax_only=False,
            no_check=False,
        )
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)
        self._recording_a_success(block)

        assert ccb.check_block(block, json_file) is False, \
            "a block whose declaration is sound must keep its recorded result"

    def test_a_syntax_only_block_fails_the_check_and_the_record(
            self, work_dir):
        """A syntax-only block carrying the class must fail, and be recorded
        as having failed.

        The block stops after the syntax check, so the report is the only
        objection it can draw, and the returned value is the only thing
        carrying it.  The record is asserted beside it because it is the
        record the next run reads: one saying the block passed would hand the
        failure straight back as a success the moment the check is run again.
        """
        source = work_dir / "main.c"
        source.write_text(
            TestCheckBlockRunClassNamingTheOtherLanguage
            .C_SOURCE_THAT_ANNOUNCES_ITSELF)

        block = _make_block(
            language="c",
            classes=["ada-syntax-only", "ada-run"],
            buttons=["no"],
            no_check=False,
            source_files=["main.c"],
        )
        assert block.syntax_only is True, \
            "the block must be the one the checker stops after the syntax " \
            "check"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        assert ccb.check_block(block, json_file, force_checks=True) is True, \
            "a syntax-only block carrying the class must fail the check"

        record = json.loads(_check_record(work_dir, json_file).read_text())
        assert record["status_ok"] is False, \
            "the record the next run reads must say the block failed"


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

    def _setup_spark_project(self, work_dir):
        src = work_dir / "main.adb"
        src.write_text(self.SPARK_SOURCE)
        return ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=True,
        )

    def _prove(self, work_dir, buttons=None, classes=None):
        """Prove a SPARK block asking for it the given way, and hand back the
        command line the proof phase recorded.

        The switches have to be read off that command line.  The fixture
        block is trivially valid, so it proves cleanly under any switches at
        all, and a passing result therefore says nothing whatever about which
        ones were selected.
        """
        spark_project_filename = self._setup_spark_project(work_dir)
        block = _make_block(
            buttons=buttons,
            classes=classes,
            syntax_only=False,
            no_check=False,
            compile_it=False,
            run_it=False,
            source_files=["main.adb"],
        )
        block.spark_project_filename = spark_project_filename
        block.project_main_file = "main.adb"

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        assert ccb.check_block(block, json_file, force_checks=True) is False, \
            "the fixture block must prove cleanly, or what the proof recorded " \
            "is not what this test is about"
        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert "PROVE" in recorded, \
            "the block must have asked for a proof, or there is no command " \
            "line to look at"
        return ast.literal_eval(recorded["PROVE"]["cmdline"])

    def test_prove_button_selects_neither_switch(self, work_dir):
        """A plain prove button asks for neither the flow mode nor the full
        report, so the proof runs on the default switches alone."""
        proved_with = self._prove(work_dir, buttons=["prove"])
        assert "--mode=flow" not in proved_with, \
            "a plain prove button must not restrict the proof to flow " \
            "analysis: {}".format(proved_with)
        assert "--report=all" not in proved_with, \
            "a plain prove button must not ask for the full report: " \
            "{}".format(proved_with)

    def test_prove_flow_mode(self, work_dir):
        """The prove_flow button selects the flow mode and nothing else."""
        proved_with = self._prove(work_dir, buttons=["prove_flow"])
        assert "--mode=flow" in proved_with, \
            "the flow button must restrict the proof to flow analysis: " \
            "{}".format(proved_with)
        assert "--report=all" not in proved_with, \
            "the flow button must not also ask for the full report: " \
            "{}".format(proved_with)

    def test_prove_flow_report_all(self, work_dir):
        """The prove_flow_report_all button selects both switches."""
        proved_with = self._prove(work_dir, buttons=["prove_flow_report_all"])
        assert "--mode=flow" in proved_with, \
            "the flow report-all button must restrict the proof to flow " \
            "analysis: {}".format(proved_with)
        assert "--report=all" in proved_with, \
            "the flow report-all button must ask for the full report: " \
            "{}".format(proved_with)

    def test_prove_report_all(self, work_dir):
        """The prove_report_all button selects the full report and nothing
        else."""
        proved_with = self._prove(work_dir, buttons=["prove_report_all"])
        assert "--report=all" in proved_with, \
            "the report-all button must ask for the full report: " \
            "{}".format(proved_with)
        assert "--mode=flow" not in proved_with, \
            "the report-all button must not also restrict the proof to flow " \
            "analysis: {}".format(proved_with)

    def test_ada_prove_flow_class_selects_the_flow_mode(self, work_dir):
        """The class an author writes selects what the matching button does."""
        proved_with = self._prove(work_dir, classes=["ada-prove-flow"])
        assert "--mode=flow" in proved_with, \
            "the flow class must restrict the proof to flow analysis: " \
            "{}".format(proved_with)
        assert "--report=all" not in proved_with, \
            "the flow class must not also ask for the full report: " \
            "{}".format(proved_with)

    def test_ada_prove_flow_report_all_class_selects_both(self, work_dir):
        """The class an author writes selects what the matching button does."""
        proved_with = self._prove(work_dir, classes=["ada-prove-flow-report-all"])
        assert "--mode=flow" in proved_with, \
            "the flow report-all class must restrict the proof to flow " \
            "analysis: {}".format(proved_with)
        assert "--report=all" in proved_with, \
            "the flow report-all class must ask for the full report: " \
            "{}".format(proved_with)

    def test_ada_prove_report_all_class_is_proved(self, work_dir):
        """The class alone asks for a proof, with no prove button present.

        Pins the fixture the test below depends on: that test can only
        report on the switches of a proof that really happened, so the proof
        itself is asserted here, on its own.
        """
        assert self._prove(work_dir, classes=["ada-prove-report-all"])

    def test_ada_prove_report_all_class_asks_for_the_full_report(self, work_dir):
        """A block classed ``ada-prove-report-all`` must be proved with
        ``--report=all``.

        Each prove button is paired with the class that carries the same
        name: prove_flow with ada-prove-flow, prove_flow_report_all with
        ada-prove-flow-report-all, and this one with ada-prove-report-all.
        That third arm used to test a differently-named class instead, so a
        block classed ada-prove-report-all was proved -- it is one of the
        classes that select a proof -- and then never reached the switch its
        own name asks for.

        This test can only report on the switches of a proof that really
        happened, so it depends on the unmarked sibling above, which drives
        the same fixture and reddens if the proof stops happening at all.
        """
        proved_with = self._prove(work_dir, classes=["ada-prove-report-all"])
        assert "--report=all" in proved_with, \
            "a class that names the full report must select it: {}".format(
                proved_with)
        assert "--mode=flow" not in proved_with, \
            "the report-all class must not also restrict the proof to flow " \
            "analysis: {}".format(proved_with)

    def test_ada_prove_class_selects_neither_switch(self, work_dir):
        """The plain prove class asks for neither the flow mode nor the full
        report, so the proof runs on the default switches alone.

        The control for the three class tests above: each of them names a
        switch and asserts it was selected, which a proof that always
        selected everything would satisfy.  This one fails on that.
        """
        proved_with = self._prove(work_dir, classes=["ada-prove"])
        assert "--mode=flow" not in proved_with, \
            "a plain prove class must not restrict the proof to flow " \
            "analysis: {}".format(proved_with)
        assert "--report=all" not in proved_with, \
            "a plain prove class must not ask for the full report: " \
            "{}".format(proved_with)


# ---------------------------------------------------------------------------
# TestCheckCodeBlockJsonInactive
# Covers the inactive-block WARNING printed by check_code_block_json().
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckCodeBlockJsonInactive:
    def test_check_code_block_json_inactive_block(self, work_dir, capsys):
        """check_code_block_json() on a block with active=False prints the
        deactivation WARNING and still checks it."""
        block = _make_block(classes=["ada-nocheck"], no_check=True, buttons=["no"])
        block.active = False
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

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

        written = json.loads(_check_record(tmp_path, json_file).read_text())
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

    def _setup_project(self, work_dir):
        """Write an Ada source file and a .gpr project file into work_dir."""
        src = work_dir / "main.adb"
        src.write_text(MINIMAL_ADA_SOURCE)
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )
        return project_filename

    def test_gprclean_and_gnatprove_clean_failures_do_not_affect_result(
            self, work_dir, monkeypatch, capsys):
        """Each of the three clean-up commands an Ada block reaches is
        reported when it fails, and none of the failures aborts the check or
        changes its result.

        The three are a gprclean before the build, and a gprclean and a
        gnatprove --clean during the end-of-check clean-up.  All three are
        made to fail here, so all three have to be reported: a real compile
        and run that succeed still make the check pass, but they do so
        loudly.

        The counts are exact rather than bounded from below, so that dropping
        any one of the three reddens this test.  The two gprclean sites print
        the same text, so only their number tells that both are still there;
        the gnatprove --clean site names the command it ran, so it is
        asserted by that name and by being the last of the three to report.
        """
        import subprocess as S

        project_filename = self._setup_project(work_dir)

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

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "clean-up failures must not affect the outcome of a successful compile and run"

        # Both clean-up commands must have been reached and must have failed,
        # otherwise the test proves nothing about how their failure is handled.
        assert "gprclean" in failed_cleanups
        assert "gnatprove" in failed_cleanups

        out = capsys.readouterr().out
        shared_message = "Failed to clean-up example"
        gnatprove_message = shared_message + " (gnatprove --clean)"

        assert out.count(gnatprove_message) == 1, \
            "the gnatprove --clean failure must be reported once, under a " \
            "message that names the command that failed -- three reports " \
            "spelled the same way would say that a clean-up failed and " \
            "never which one: {}".format(out)

        assert out.count(shared_message) - out.count(gnatprove_message) == 2, \
            "both gprclean failures -- the one before the build and the one " \
            "in the end-of-check clean-up -- must be reported, and the two " \
            "print the same text, so only their number tells that neither " \
            "has gone: {}".format(out)

        assert out.rindex(shared_message) == out.index(gnatprove_message), \
            "the gnatprove --clean report belongs to the end-of-check " \
            "clean-up and must therefore come after both gprclean reports: " \
            "{}".format(out)

        assert out.count("simulated cleanup failure") == 3, \
            "each report must carry the output of the command it is about, " \
            "which is the part that says why the clean-up failed: {}".format(out)


@pytest.mark.toolchain
class TestCheckBlockCCleanupFailure:
    """A real C compile and run that both succeed, while the rm -f clean-up
    command is made to fail."""

    VALID_C_SOURCE = "int main(void) { return 0; }\n"

    def test_rm_cleanup_failure_does_not_affect_result(self, work_dir, monkeypatch, capsys):
        """An rm -f clean-up failure after a successful C compile and run is
        logged, but it does not abort the check or change its result."""
        import subprocess as S

        src = work_dir / "main.c"
        src.write_text(self.VALID_C_SOURCE)

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
        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is False, \
            "an rm -f clean-up failure must not affect the outcome of a successful compile and run"

        assert "Failed to clean-up example" in capsys.readouterr().out


# ---------------------------------------------------------------------------
# A run with no executable to run
# Covers the run step finding nothing to execute, in both languages, with and
# without the class that declares a failing run to be expected.
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckBlockMissingExecutable:
    """A run whose executable is gone by the time the run starts.

    The build itself reports success and the executable is removed
    afterwards, which is the state the run step has to survive.  It used to
    escape as a bare FileNotFoundError, which is worse than a failing check
    in two separate ways: the run is never recorded at all, and the exception
    leaves the whole command, so every block queued behind this one goes
    unchecked as well.

    The two languages carry the same handling in two separate places, so both
    are exercised: dropping either one has to redden something.
    """

    VALID_C_SOURCE = """\
#include <stdio.h>

int main(void)
{
   printf("the C example ran\\n");
   return 0;
}
"""

    @staticmethod
    def _remove_after(monkeypatch, produced_by, executable):
        """Let the build run for real, then take its executable away.

        ``produced_by`` decides which command line is the one that links, so
        that neither the toolchain version probes nor the syntax check -- which
        invoke the same compiler -- is mistaken for it.
        """
        import subprocess as S

        real_check_output = S.check_output

        def fake_check_output(cmd, *args, **kwargs):
            output = real_check_output(cmd, *args, **kwargs)
            if produced_by(list(cmd)):
                assert os.path.isfile(executable), \
                    "the build must really have produced {}, or the run has " \
                    "nothing to lose".format(executable)
                os.remove(executable)
            return output

        monkeypatch.setattr(S, "check_output", fake_check_output)

    def _ada_block(self, work_dir, classes):
        (work_dir / "main.adb").write_text(MINIMAL_ADA_SOURCE)
        project_filename = ep.write_project_file(
            main_file="main.adb",
            compiler_switches=["-gnata"],
            spark_mode=False,
        )
        block = _make_block(
            classes=classes,
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.adb"],
        )
        block.project_filename = project_filename
        block.project_main_file = "main.adb"
        return block

    def _c_block(self, work_dir, classes):
        (work_dir / "main.c").write_text(self.VALID_C_SOURCE)
        block = _make_block(
            language="c",
            classes=classes,
            buttons=["run"],
            syntax_only=False,
            no_check=False,
            compile_it=True,
            run_it=True,
            source_files=["main.c"],
        )
        block.project_main_file = "main.c"
        return block

    @pytest.mark.parametrize("language", ["ada", "c"])
    def test_a_missing_executable_is_reported_and_recorded(
            self, language, work_dir, monkeypatch, capsys):
        """A run with no executable to run must be reported as a failed run,
        and must leave a failed run recorded behind it.

        Both halves matter.  Returning rather than raising is what lets the
        command go on to the blocks after this one.  Recording the run is
        what keeps the phase a check writes down honest: the run was
        attempted, it failed, and the record has to say so -- a run step that
        reported the failure but wrote no RUN phase would leave a block whose
        record cannot be told apart from one that was never asked to run.
        """
        if language == "ada":
            block = self._ada_block(work_dir, [])
            self._remove_after(monkeypatch,
                               lambda cmd: cmd[0] == "gprbuild", "main")
        else:
            block = self._c_block(work_dir, [])
            self._remove_after(
                monkeypatch,
                lambda cmd: cmd[0] == "gcc" and "-o" in cmd, "main")

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "a run with no executable must be reported as a failure rather " \
            "than leave the check as an exception"

        out = capsys.readouterr().out
        assert "no executable to run" in out, \
            "the report must say what was missing, or it is indistinguishable " \
            "from the example itself failing at run time: {}".format(out)

        recorded = json.loads(
            _check_record(work_dir, json_file).read_text())["checks"]
        assert recorded["RUN"]["status_ok"] is False, \
            "the run was attempted and failed, so it must be recorded as a " \
            "failed run: {}".format(sorted(recorded))

    @pytest.mark.parametrize("language,expect_failure_class",
                             [("ada", "ada-run-expect-failure"),
                              ("c", "c-run-expect-failure")])
    def test_an_expected_run_failure_does_not_absorb_a_missing_executable(
            self, language, expect_failure_class, work_dir, monkeypatch,
            capsys):
        """A block declaring that its run is expected to fail must still be
        reported when there is no executable to run.

        The class says the author expects the example to fail when it runs.
        Nothing ran here: the checker did not produce the program it was
        supposed to run, which is a defect on the checker's side of the line
        and not the failure the block declared.  Absorbing it would let a
        block carrying that class pass over an example that was never built.
        """
        if language == "ada":
            block = self._ada_block(work_dir, [expect_failure_class])
            self._remove_after(monkeypatch,
                               lambda cmd: cmd[0] == "gprbuild", "main")
        else:
            block = self._c_block(work_dir, [expect_failure_class])
            self._remove_after(
                monkeypatch,
                lambda cmd: cmd[0] == "gcc" and "-o" in cmd, "main")

        json_file = str(work_dir / "block_info.json")
        block.to_json_file(json_file)

        result = ccb.check_block(block, json_file, force_checks=True)
        assert result is True, \
            "the class expects the example to fail, not the executable to be " \
            "missing, so this must still be reported"

        assert "no executable to run" in capsys.readouterr().out, \
            "the report must name what was missing rather than read as the " \
            "expected run failure the block declared"


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

    # The same, for a program that announces itself and then fails.  It
    # prints before it fails so that a run which really happened can be told
    # from one that was reported as having happened: the exit status alone
    # would also be produced by no program running at all.
    _C_FAIL_OUTPUT = "extracted C example ran and then failed"

    _FAILING_C_BODY = """\
!{}
#include <stdio.h>

int main(void)
{{
   printf("{}\\n");
   return 1;
}}""".format(_C_MAIN, _C_FAIL_OUTPUT)

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
        record -- the staging copy the extraction step keeps alongside holds
        none.  The record is taken as the one JSON file in that directory
        rather than by a name written down here, so that what is read back is
        whatever the extraction step wrote.
        """
        rst_path = work_dir / "extracted.rst"
        rst_path.write_text(self._rst(directive, body, classes))

        assert ep.analyze_file(str(rst_path)) is False, \
            "the fixture must extract cleanly, or the check that follows is " \
            "not being handed a well-formed block"

        project_dir = work_dir / ep.get_project_dir(project)
        block_dirs = sorted(d for d in project_dir.iterdir()
                            if d.is_dir() and list(d.glob("*.json")))
        assert len(block_dirs) == 1, \
            "expected exactly one per-block directory, got {}".format(
                [d.name for d in block_dirs])
        block_dir = block_dirs[0]
        records = sorted(block_dir.glob("*.json"))
        assert len(records) == 1, \
            "expected exactly one block record, got {}".format(
                [record.name for record in records])
        json_file = records[0]
        return block_dir, json.loads(json_file.read_text()), str(json_file)

    @staticmethod
    def _buttons_asked_for(info) -> tuple[bool, bool, bool]:
        """The compile / run / prove decision the checker branches on."""
        return info["compile_it"], info["run_it"], info["prove_it"]

    @staticmethod
    def _recorded_checks(block_dir, block_record) -> dict:
        """The per-phase results the check wrote beside the block.

        Read straight from the file rather than through
        checks.BlockCheck.from_json_file(), which drops the per-phase entries
        on the way back in.
        """
        return json.loads(
            _check_record(block_dir, block_record).read_text())["checks"]

    @staticmethod
    def _log_of(block_dir, recorded_check) -> str:
        """The log a recorded phase says it wrote."""
        return (block_dir / recorded_check["logfile"]).read_text()

    @staticmethod
    def _command_line_of(recorded_check) -> list[str]:
        """The argument list a recorded phase really ran.

        Recorded as the printed form of the list, so it reads back as one --
        which is what lets a test assert on the switches the checker chose
        rather than on the fact that something was run.
        """
        return ast.literal_eval(recorded_check["cmdline"])

    @staticmethod
    def _project_used(recorded_check) -> str:
        """The project file a recorded phase really ran against.

        The command line is recorded as the printed form of the argument list,
        so it can be read back as one and the project taken from behind the
        switch that names it -- rather than by matching a name the test would
        otherwise have to know in advance.

        Only for phases that are driven by a project file: the Ada build and
        the proof.  A C build is a compiler command line with no project on
        it, and asking this for one raises rather than returning anything.
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

    def test_compile_button_block_is_built_as_extracted(self, work_dir):
        """A compile button carries from the RST directive through to a real
        build with nothing adjusted in between.

        The directive asks for a compile and nothing else, so the block must
        reach the checker asking for a compile and nothing else, the checker
        must record a build and neither a run nor a proof, and the project it
        built against must be an ordinary one naming no main -- a compile
        button selects no main to link.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
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

        recorded = self._recorded_checks(block_dir, json_file)
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

    def test_run_button_block_is_built_and_run_as_extracted(self, work_dir):
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
            work_dir,
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

        recorded = self._recorded_checks(block_dir, json_file)
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

    def test_prove_button_block_is_proved_as_extracted(self, work_dir):
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
            work_dir,
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

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUTTONS", "PROVE", "SYNTAX"], \
            "a prove button must be syntax-checked and proved, and not built"
        assert recorded["PROVE"]["status_ok"] is True

        proved_against = self._project_used(recorded["PROVE"])
        assert self._SPARK_CONFIGURATION in \
            self._configuration_pragmas(block_dir, proved_against), \
            "the proof must have run against a project that turns SPARK mode on"

    def test_extracted_block_that_does_not_build_fails_the_check(self, work_dir):
        """A block that does not compile must be reported as an error when the
        check is driven from the extraction step too.

        Without this the tests above could all pass on a seam that reports
        success whatever the compiler said.  The block is syntactically valid,
        so it chops and passes the syntax check and only the build can fail.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: ada project=ExtractedBadBuild main={} compile_button".format(
                self._MAIN),
            self._BROKEN_ADA_BODY, "ExtractedBadBuild")

        assert info["source_files"] == [self._MAIN], \
            "the chopped source must be recorded, or the syntax check runs " \
            "on nothing and passes vacuously"

        assert ccb.check_code_block_json(json_file) is True, \
            "an extracted block that does not compile must be reported as an error"

        recorded = self._recorded_checks(block_dir, json_file)
        assert recorded["SYNTAX"]["status_ok"] is True, \
            "the block must be syntactically valid, or the build is not what failed"
        assert recorded["BUILD"]["status_ok"] is False, \
            "the failure must be recorded against the build"
        assert self._MISSING_NAME in self._log_of(block_dir, recorded["BUILD"]), \
            "the build log must name what the compiler could not resolve"

    def test_extracted_block_expecting_a_compile_error_passes(self, work_dir):
        """A block declared as expecting a compile error must pass the check
        even though the compiler rejects it.

        The class that declares the expectation is written in the RST source,
        so it has to survive extraction and reach the checker; if it did not,
        this block would be reported as a failure.  The build log is checked
        as well, because a class that suppressed the build entirely would give
        the same answer for the wrong reason.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
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

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "an expected compile error must still be syntax-checked and built"
        assert recorded["BUILD"]["status_ok"] is True, \
            "a compile error the block expects must not be recorded as a failure"
        assert self._MISSING_NAME in self._log_of(block_dir, recorded["BUILD"]), \
            "the compiler must really have rejected the block, or the " \
            "expectation was satisfied by nothing happening"

    def test_extracted_block_expecting_a_compile_error_that_compiles_fails(
            self, work_dir):
        """A block declared as expecting a compile error must fail the check
        when the compiler accepts it.

        The mirror of the test above, and the one the checker exists for: an
        example marked "this must not compile" that quietly starts compiling
        is exactly what nobody notices by hand.  Driven through the real
        directive and the real extraction step, so the class has to survive
        both to reach the checker -- and the build is asserted to have
        succeeded, since a block that failed to build for some unrelated
        reason would also fail the check and would say nothing about the
        expectation.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: ada project=ExtractedExpectErrorThatCompiles "
            "main={} compile_button".format(self._MAIN),
            self._ADA_BODY, "ExtractedExpectErrorThatCompiles",
            classes="ada-expect-compile-error")

        assert "ada-expect-compile-error" in info["classes"], \
            "the class written in the RST source must reach the checker"

        assert ccb.check_code_block_json(json_file) is True, \
            "a block declaring a compile error it did not produce must be " \
            "reported as an error"

        recorded = self._recorded_checks(block_dir, json_file)
        assert recorded["BUILD"]["status_ok"] is True, \
            "the block must really have compiled, or the failure under test " \
            "is not the missing compile error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

    def test_extracted_c_block_expecting_a_compile_error_that_compiles_fails(
            self, work_dir):
        """A C block declared as expecting a compile error must fail the check
        when the compiler accepts it.

        The C half of the promise the test above pins for Ada, driven the same
        way.  Only the Ada half was ever enforced, so a C example marked "this
        must not compile" that quietly started compiling was reported as a
        success -- the one direction of the six expect-error declarations that
        nothing watched.  The class is written in the RST source, so it has to
        survive the directive and the extraction step to reach the checker,
        and the build is asserted to have succeeded, since a block that failed
        to build for some unrelated reason would also fail the check and would
        say nothing about the expectation.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCExpectErrorThatCompiles "
            "main={} compile_button".format(self._C_MAIN),
            self._C_BODY, "ExtractedCExpectErrorThatCompiles",
            classes="c-expect-compile-error")

        assert "c-expect-compile-error" in info["classes"], \
            "the class written in the RST source must reach the checker"

        assert ccb.check_code_block_json(json_file) is True, \
            "a C block declaring a compile error it did not produce must be " \
            "reported as an error"

        recorded = self._recorded_checks(block_dir, json_file)
        assert recorded["BUILD"]["status_ok"] is True, \
            "the block must really have compiled, or the failure under test " \
            "is not the missing compile error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

    def test_extracted_block_expecting_a_prove_error_that_proves_fails(
            self, work_dir):
        """A block declared as expecting a prove error must fail the check
        when the prover is satisfied.

        The proof half of the same promise, driven the same way.  The proof is
        asserted to have succeeded and to have run against a project that
        turns SPARK mode on, so a proof that was never really attempted -- or
        one attempted against a project the prover treats as ordinary Ada --
        cannot pass for a proof that found nothing to complain about.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: ada project=ExtractedExpectProveErrorThatProves "
            "main={} prove_button".format(self._MAIN),
            self._SPARK_BODY, "ExtractedExpectProveErrorThatProves",
            classes="ada-expect-prove-error")

        assert "ada-expect-prove-error" in info["classes"], \
            "the class written in the RST source must reach the checker"

        assert ccb.check_code_block_json(json_file) is True, \
            "a block declaring a prove error it did not produce must be " \
            "reported as an error"

        recorded = self._recorded_checks(block_dir, json_file)
        assert recorded["PROVE"]["status_ok"] is True, \
            "the proof must really have succeeded, or the failure under test " \
            "is not the missing prove error"
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the unmet expectation must be recorded against the block's " \
            "declarations"

        proved_against = self._project_used(recorded["PROVE"])
        assert self._SPARK_CONFIGURATION in \
            self._configuration_pragmas(block_dir, proved_against), \
            "the proof must have run against a project that turns SPARK mode on"

    def test_c_run_button_block_is_built_and_run_as_extracted(self, work_dir):
        """A run button on a C block carries through to the program running.

        C blocks take a different route on both sides of the seam: the
        extraction step chops them from the file names written into the source
        rather than by calling gnatchop, and the checker compiles and links
        them with the C compiler instead of the project builder.  The output
        pinned below is what the author's code prints.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
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

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "a C run button must be syntax-checked, built and run, and not proved"
        assert self._log_of(block_dir, recorded["RUN"]).strip() == self._C_RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"

        # A block that is run has a main file resolved for it, and that is the
        # arm of the C compile step which links an executable and names it.
        # The sibling compile-button test takes the other arm, so both are
        # pinned and neither can be made to serve the other's case unnoticed.
        built_with = self._command_line_of(recorded["BUILD"])
        assert built_with[:3] == ["gcc", "-o", os.path.splitext(self._C_MAIN)[0]], \
            "a C block with a resolved main must be linked into an executable " \
            "named after that main: {}".format(built_with)
        assert "-c" not in built_with, \
            "a C block with a resolved main must be linked, not merely " \
            "compiled: {}".format(built_with)
        assert self._C_MAIN in built_with, \
            "the chopped source must be on the command line, or nothing was " \
            "compiled: {}".format(built_with)

    def test_c_compile_button_block_is_built_as_extracted(self, work_dir):
        """A compile button on a C block must be compiled.

        Driven by the real extraction step, so the block arrives at the
        checker with exactly the fields extraction gives it.  The directive
        names a main, but extraction resolves a project main file only for
        blocks that are also run, so the checker gets none.  The C compile
        step therefore has to build such a block without an executable to
        name: it compiles without linking, which is what a compile button
        asks for and the only thing a block holding no main can do at all.
        The sibling Ada compile test pins the generated project as naming no
        main, so resolving a main for every compiled block is not an
        available alternative.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCCompile main={} compile_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCCompile")

        assert self._buttons_asked_for(info) == (True, False, False), \
            "a compile button must reach the checker as a compile and nothing else"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted C block as it stands"

        assert info["project_main_file"] is None, \
            "extraction must leave a compile-only block with no main file " \
            "resolved, or this is not the arm of the compile step under test"

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "a C compile button must be syntax-checked and built, and neither " \
            "run nor proved"
        assert recorded["BUILD"]["status_ok"] is True

        built_with = self._command_line_of(recorded["BUILD"])
        assert "-c" in built_with, \
            "a compile button asks for a compile and not a link: {}".format(
                built_with)
        assert "-o" not in built_with, \
            "nothing is being linked, so no executable may be named -- naming " \
            "one is what used to stop the check on an assertion: {}".format(
                built_with)
        assert self._C_MAIN in built_with, \
            "the chopped source must be on the command line, or nothing was " \
            "compiled: {}".format(built_with)

    # The C run classes an author writes, driven the same way.  These are the
    # only tests in the file that reach the run path of a C block without a
    # run button: every other one either writes the button into the directive
    # or hands check_block() a block with run_it already set, and a block that
    # arrives with the decision already made cannot show how it was reached.
    # That is why a green suite said nothing while a C block asking to be run
    # by class alone was never run and the check reported success over it.

    def test_c_run_class_block_is_built_and_run_as_extracted(self, work_dir):
        """A C block classed ``c-run`` and carrying no button must be run.

        The class is the whole of what asks for the run here -- the directive
        declares ``no_button`` -- so the phase set below is the assertion
        with the detection power, and the run log is what says the author's
        own program is what executed rather than the run being recorded over
        nothing.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCRunClass main={} no_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCRunClass", classes="c-run")

        assert info["buttons"] == ["no"], \
            "the block must carry no button, or the class is not what asked " \
            "for the run"
        assert self._buttons_asked_for(info) == (True, True, False), \
            "a c-run class must reach the checker as a run, which implies a " \
            "compile, and not as a proof"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted C block as it stands"

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "a c-run class must be syntax-checked, built and run, and not proved"
        assert recorded["RUN"]["status_ok"] is True
        assert self._log_of(block_dir, recorded["RUN"]).strip() == \
            self._C_RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"

    def test_c_run_expect_failure_class_block_is_run_without_a_button(
            self, work_dir):
        """A C block classed ``c-run-expect-failure`` and carrying no button
        must be run, and its failure must be the expected one.

        This is the case a green suite passed over.  The checker has long
        held a branch that absorbs a failing C run when the block declares it
        expects one, but nothing made such a block run from the class alone,
        so that branch was reachable only through a run button and the class
        on its own bought the block nothing.

        Three things are asserted together, because any two of them are
        satisfied by a block that was never run: the run must be recorded,
        the program's own output must be in the run log, and the check must
        pass even though the program exited non-zero.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCExpectFailure main={} no_button".format(
                self._C_MAIN),
            self._FAILING_C_BODY, "ExtractedCExpectFailure",
            classes="c-run-expect-failure")

        assert info["buttons"] == ["no"], \
            "the block must carry no button, or the class is not what asked " \
            "for the run"
        assert self._buttons_asked_for(info) == (True, True, False), \
            "a c-run-expect-failure class must reach the checker as a run, " \
            "which implies a compile, and not as a proof"

        assert ccb.check_code_block_json(json_file) is False, \
            "a run failure the block declared it expects must not fail the check"

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "the block must really have been run, not merely built and " \
            "reported as passing"
        assert recorded["RUN"]["status_ok"] is True, \
            "a failure the block expects must be recorded as a passing run"
        assert self._log_of(block_dir, recorded["RUN"]).strip() == \
            self._C_FAIL_OUTPUT, \
            "the program the author wrote must be the one that ran and failed"

    def test_c_norun_class_suppresses_the_run_of_an_extracted_block(
            self, work_dir):
        """A C block classed ``c-norun`` must not be run, whatever the
        directive asks for.

        The mirror of the two above: the class has to be able to take a run
        away as well as ask for one, or it is decoration on a block that was
        going to be run anyway.  The directive carries a compile button
        beside the run button, so that the compile survives the suppression
        and the block is still built -- which isolates what was suppressed to
        the run, and would catch a suppression that quietly stopped the whole
        check instead.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCNoRun main={} compile_button "
            "run_button".format(self._C_MAIN),
            self._C_BODY, "ExtractedCNoRun", classes="c-norun")

        assert "run" in info["buttons"], \
            "the block must carry the run button the class has to suppress"
        assert self._buttons_asked_for(info) == (True, False, False), \
            "c-norun must take the run away and leave the compile the " \
            "directive asked for separately"

        assert ccb.check_code_block_json(json_file) is False, \
            "the checker must accept the extracted C block as it stands"

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "SYNTAX"], \
            "a suppressed run must not be recorded as having happened"
        assert recorded["BUILD"]["status_ok"] is True, \
            "suppressing the run must not suppress the build as well"
        assert not (block_dir / "run.log").exists(), \
            "nothing may have been run, so no run log may have been written"

    def test_a_c_block_classed_ada_run_is_neither_built_nor_run(
            self, work_dir):
        """A C block classed ``ada-run``, with no button anywhere, must not be
        built, must not be run, and must fail the check.

        This is the whole shape of the problem, driven from the directive an
        author would really write.  The class names Ada, so it asks this
        block for nothing; the block carries no button to ask instead; and
        the source is never handed to a compiler.

        What the check then does about it is the subject of the test below,
        deliberately separated: this one would pass just as well if the block
        were quietly accepted, and saying so is the point -- it is about what
        was and was not done to the block, and nothing else.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCAdaRunClass main={} no_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCAdaRunClass", classes="ada-run")

        assert info["buttons"] == ["no"], \
            "the block must carry no button, or something other than the " \
            "class is deciding whether it is run"
        assert self._buttons_asked_for(info) == (False, False, False), \
            "a class naming the other language must ask for nothing"

        ccb.check_code_block_json(json_file)

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUTTONS", "SYNTAX"], \
            "the block must have been syntax-checked and nothing more"
        assert not (block_dir / "build.log").exists(), \
            "nothing was compiled, so no build log may have been written"
        assert not (block_dir / "run.log").exists(), \
            "nothing was run, so no run log may have been written"

    def test_a_c_block_classed_ada_run_fails_the_check_and_the_record(
            self, work_dir):
        """The same extracted block must fail the check, and must be recorded
        as having failed.

        The wrapper level, and the one place the on-disk record is read as
        evidence.  Neither is covered by asserting what was printed: a report
        that printed and handed back success is a defect this package has
        already shipped once, in the extraction step's own wrong-language
        report, so it is a live possibility rather than a hypothetical.

        The record matters on its own account.  It is what the next run reads
        to decide the block can be skipped, so a run that fails while
        recording success does not merely mislead once -- it tells every
        later run that the block was checked and passed.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: c project=ExtractedCAdaRunStatus main={} no_button".format(
                self._C_MAIN),
            self._C_BODY, "ExtractedCAdaRunStatus", classes="ada-run")

        assert ccb.check_code_block_json(json_file) is True, \
            "a block nothing was done to must not be reported as checked"

        recorded = self._recorded_checks(block_dir, json_file)
        assert recorded["BUTTONS"]["status_ok"] is False, \
            "the objection must be recorded against the block's declarations"
        record = json.loads(
            _check_record(block_dir, json_file).read_text())
        assert record["status_ok"] is False, \
            "the record left beside the block is read back by the next run " \
            "as a result to skip on, so it must not say the block passed"

    def test_an_ada_block_classed_c_norun_is_still_built_and_run(
            self, work_dir):
        """An Ada block classed ``c-norun`` and carrying a run button must
        still be run.

        The mirror direction, and the one where the unpaired reading used to
        take something away: a stray C norun canceled the run, and with it
        the build, leaving an Ada example that was never compiled.  The run
        log is what says the author's own program executed rather than a run
        being recorded over nothing.
        """
        block_dir, info, json_file = self._extract(
            work_dir,
            ".. code:: ada project=ExtractedAdaCNoRun main={} run_button".format(
                self._MAIN),
            self._ADA_BODY, "ExtractedAdaCNoRun", classes="c-norun")

        assert "run" in info["buttons"], \
            "the block must carry the run button the class must not suppress"
        assert self._buttons_asked_for(info) == (True, True, False), \
            "a norun class naming the other language must take nothing away"

        # The check is run for its effects, and its returned value is
        # deliberately not asserted: the stray class is separately reported,
        # so the value says something about the report rather than about the
        # run this test is here for.
        ccb.check_code_block_json(json_file)

        recorded = self._recorded_checks(block_dir, json_file)
        assert sorted(recorded) == ["BUILD", "BUTTONS", "RUN", "SYNTAX"], \
            "the block must have been built and run"
        assert recorded["RUN"]["status_ok"] is True
        assert self._log_of(block_dir, recorded["RUN"]).strip() == \
            self._RUN_OUTPUT, \
            "the program the author wrote must be the one that ran"
