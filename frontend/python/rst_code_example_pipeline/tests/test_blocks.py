"""
Unit tests for rst_code_example_pipeline.blocks.

Covers:
- Block.get_blocks_from_rst(): RST parser (all attributes, derived fields)
- CodeBlock constructor derived fields (no_check, syntax_only, run_it, compile_it,
  prove_it), including the C run classes, which ask for a run only on a C block
  and are suppressed by c-norun
- every run class is honored only for a block written in the language it names:
  the Ada run classes ask for no run, and therefore no build, on a C block, and
  neither norun class takes a run away from a block of the other language --
  with the controls that say so, since a derivation refusing every run, or
  suppressing nothing anywhere, satisfies those on its own.  The run button, the
  syntax-only class and the two no-check classes are deliberately not paired
  with any language, and are pinned as such
- text_hash / text_hash_short: deterministic, distinct per text, usable as a
  directory name
- CodeBlock.to_json_file() + from_json_file() round-trip
- CodeBlock.from_json_file() on a record that is present but cannot be turned
  into a block: read back as no block, and reported with the file name and the
  reason, rather than left as an exception for the caller to trip over
- ConfigBlock.__init__ and update(), for the strings a code-config directive
  produces and for the real booleans a caller may hand over instead
- Adversarial: empty RST, missing json file, exit(1) path

NOTE: get_blocks_from_rst() calls toolchain_info.get_toolchain_default_version()
at parse time; requires the Ada toolchain .ini
is present and toolchain_info initializes correctly.

NOTE: the version strings written inside the RST fixtures below, and the values
the parser is expected to produce from them, are deliberately spelled out.  They
stand for what a course author types in a real .rst file, and the parser never
validates them against the configured toolchains -- it round-trips the string
verbatim.  Driving both the input and the expected output from the toolchain
configuration would make the pair self-referential and hide a parsing error.
Version strings passed straight to the CodeBlock constructor are a different
matter: those are copies of configuration data and are read back from it.
"""
import json
import re
import subprocess
import sys
import textwrap

import pytest

from rst_code_example_pipeline.blocks import Block, CodeBlock, ConfigBlock
import rst_code_example_pipeline.toolchain_info as info


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

RST_FILE = "test.rst"


def minimal_rst(body: str) -> str:
    """Wrap body in a minimal RST file so there is a trailing explanatory
    paragraph to close the code block."""
    return body + "\n\nExplanatory paragraph.\n"


# ---------------------------------------------------------------------------
# T-blocks-01: minimal Ada block
# ---------------------------------------------------------------------------

class TestMinimalAdaBlock:
    RST = minimal_rst("""\
.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is
   begin
      Put_Line ("Hello");
   end Main;
""")

    def test_returns_one_block(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert len(blocks) == 1

    def test_rst_file_stored(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].rst_file == RST_FILE

    def test_language_is_ada(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].language == "ada"

    def test_project_is_none(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].project is None

    def test_main_file_is_none(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].main_file is None

    def test_manual_chop_false(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].manual_chop is False

    def test_default_compiler_switches_includes_gnata(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert "-gnata" in blocks[0].compiler_switches

    def test_gnat_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gnat_version[0] == "default"

    def test_gnatprove_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gnatprove_version[0] == "default"

    def test_gprbuild_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gprbuild_version[0] == "default"

    def test_line_span_and_text_are_exact(self):
        """The parser must report the block's span in the RST file and hand
        back its body with the directive indentation removed.

        Counting lines from zero, ``line_start`` is the first line after the
        ``.. code::`` directive -- which makes it equal to the directive's own
        1-based line number -- and ``line_end`` is the line that closed the
        block.  The body is everything between the two, so it keeps the blank
        lines separating the block from what follows it.

        The expected values are spelled out rather than derived from the
        parser: every consumer of a block reports diagnostics against these
        line numbers, so an off-by-one here misdirects a course author to the
        wrong line.  Recomputing them the way the parser does would make the
        test agree with whatever the parser produced.
        """
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].line_start == 1
        assert blocks[0].line_end == 9
        assert blocks[0].text == (
            'with Ada.Text_IO; use Ada.Text_IO;\n'
            'procedure Main is\n'
            'begin\n'
            '   Put_Line ("Hello");\n'
            'end Main;\n'
            '\n'
        )

    def test_active_defaults_to_true(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].active is True


# ---------------------------------------------------------------------------
# T-blocks-02: project and main_file attributes
# ---------------------------------------------------------------------------

class TestProjectAndMainFile:
    RST = minimal_rst("""\
.. code:: ada project=MyProject main=main.adb

   procedure Main is
   begin
      null;
   end Main;
""")

    def test_project(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].project == "MyProject"

    def test_main_file(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].main_file == "main.adb"


# ---------------------------------------------------------------------------
# T-blocks-03: compiler switches
# ---------------------------------------------------------------------------

class TestCompilerSwitches:
    RST = minimal_rst("""\
.. code:: ada switches=Compiler(-gnatwa,-gnatwe)

   procedure P is null;
""")

    def test_custom_switches_present(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        switches = blocks[0].compiler_switches
        assert "-gnatwa" in switches
        assert "-gnatwe" in switches

    def test_default_gnata_also_present(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert "-gnata" in blocks[0].compiler_switches


# ---------------------------------------------------------------------------
# T-blocks-04: gnat version selected
# ---------------------------------------------------------------------------

class TestGnatVersionSelected:
    RST = minimal_rst("""\
.. code:: ada gnat=12.2.0-1

   procedure P is null;
""")

    def test_gnat_version_is_selected(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gnat_version == ["selected", "12.2.0-1"]


# ---------------------------------------------------------------------------
# T-blocks-05: language=c sets manual_chop=True
# ---------------------------------------------------------------------------

class TestLanguageC:
    RST = minimal_rst("""\
.. code:: c

   #include <stdio.h>
   int main() { return 0; }
""")

    def test_manual_chop_true_for_c(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].manual_chop is True

    def test_language_is_c(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].language == "c"


# ---------------------------------------------------------------------------
# T-blocks-06: explicit manual_chop keyword
# ---------------------------------------------------------------------------

class TestManualChopKeyword:
    RST = minimal_rst("""\
.. code:: ada manual_chop

   procedure P is null;
""")

    def test_manual_chop_true(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].manual_chop is True


# ---------------------------------------------------------------------------
# T-blocks-07: buttons
# ---------------------------------------------------------------------------

class TestButtons:
    def test_run_button(self):
        rst = minimal_rst("""\
.. code:: ada run_button

   procedure P is null;
""")
        blocks = Block.get_blocks_from_rst(RST_FILE, rst)
        assert isinstance(blocks[0], CodeBlock)
        assert "run" in blocks[0].buttons

    def test_compile_button(self):
        rst = minimal_rst("""\
.. code:: ada compile_button

   procedure P is null;
""")
        blocks = Block.get_blocks_from_rst(RST_FILE, rst)
        assert isinstance(blocks[0], CodeBlock)
        assert "compile" in blocks[0].buttons


# ---------------------------------------------------------------------------
# T-blocks-08: :code-config: line produces ConfigBlock
# ---------------------------------------------------------------------------

class TestCodeConfig:
    RST = """\
:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Some paragraph.
"""

    def test_config_block_in_list(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        config_blocks = [b for b in blocks if isinstance(b, ConfigBlock)]
        assert len(config_blocks) == 1

    def test_config_attributes(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        cb = [b for b in blocks if isinstance(b, ConfigBlock)][0]
        # run_button/prove_button/accumulate_code are set dynamically via
        # setattr() in ConfigBlock.__init__, so they are looked up with
        # getattr() rather than direct attribute access.
        assert getattr(cb, "run_button") is False
        assert getattr(cb, "prove_button") is True
        assert getattr(cb, "accumulate_code") is False


# ---------------------------------------------------------------------------
# T-blocks-09: two consecutive code blocks
# ---------------------------------------------------------------------------

class TestTwoConsecutiveBlocks:
    RST = """\
.. code:: ada

   procedure A is null;

Some text.

.. code:: ada

   procedure B is null;

More text.
"""

    def test_two_code_blocks(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        code_blocks = [b for b in blocks if isinstance(b, CodeBlock)]
        assert len(code_blocks) == 2

    def test_line_spans_are_exact_and_ordered(self):
        """Each block must carry its own span, in file order and without
        overlapping the other one."""
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        code_blocks = [b for b in blocks if isinstance(b, CodeBlock)]
        assert [(b.line_start, b.line_end) for b in code_blocks] == [(1, 4), (7, 10)]
        assert [b.text for b in code_blocks] == [
            "procedure A is null;\n",
            "procedure B is null;\n",
        ]


# ---------------------------------------------------------------------------
# T-blocks-10: block at end of file
# ---------------------------------------------------------------------------

class TestBlockAtEndOfFile:
    RST_WITH_CONTENT = """\
.. code:: ada

   procedure P is null;
"""
    # Block with content but no trailing explanatory paragraph.
    # process_block() can still extract the block when called with "END" at
    # indent=0, so no exit(1) — just a WARNING printed.

    RST_EMPTY_BODY = ".. code:: ada\n"
    # Block with NO content at all — cb_indent stays -1, so process_block()
    # cannot set the indent and the block is not created.  exit(1) is called.

    def test_block_with_content_no_trailing_paragraph_succeeds(self):
        """A block at end-of-file that has content produces a WARNING but
        is successfully parsed (no SystemExit).

        With no explanatory paragraph to close the block, the end of the file
        closes it instead, so the span ends one line past the last line of the
        file -- pinned because this path computes it differently from the
        ordinary one, and because nothing follows the body here the text
        carries no trailing blank line.
        """
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST_WITH_CONTENT)
        assert len(blocks) == 1
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].line_start == 1
        assert blocks[0].line_end == 3
        assert blocks[0].text == "procedure P is null;"

    def test_empty_block_body_raises_system_exit(self):
        """A code-block directive with an empty body (no content lines at all)
        cannot be processed and triggers exit(1)."""
        with pytest.raises(SystemExit):
            Block.get_blocks_from_rst(RST_FILE, self.RST_EMPTY_BODY)


# ---------------------------------------------------------------------------
# T-blocks-11: empty RST returns empty list
# ---------------------------------------------------------------------------

class TestEmptyRst:
    def test_empty_string(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, "")
        assert blocks == []

    def test_only_text_no_code_blocks(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, "Just some text.\n\nNo code here.\n")
        assert blocks == []


# ---------------------------------------------------------------------------
# T-blocks-12: CodeBlock derived fields from classes
# ---------------------------------------------------------------------------

class TestCodeBlockDerivedFields:
    def _make_block(self, classes, buttons=None, language="ada",
                    text="procedure P is null;"):
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()
        return CodeBlock(
            rst_file="test.rst",
            line_start=0,
            line_end=5,
            text=text,
            language=language,
            project=None,
            main_file=None,
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=classes,
            manual_chop=False,
            buttons=buttons or [],
        )

    def test_no_check_from_ada_nocheck_class(self):
        b = self._make_block(["ada-nocheck"])
        assert b.no_check is True

    def test_no_check_from_c_nocheck_class(self):
        b = self._make_block(["c-nocheck"], language="c")
        assert b.no_check is True

    def test_no_check_false_default(self):
        b = self._make_block([])
        assert b.no_check is False

    def test_syntax_only_from_class(self):
        b = self._make_block(["ada-syntax-only"])
        assert b.syntax_only is True

    def test_syntax_only_false_default(self):
        b = self._make_block([])
        assert b.syntax_only is False

    def test_run_it_from_ada_run_class(self):
        b = self._make_block(["ada-run"])
        assert b.run_it is True

    def test_run_it_from_run_button(self):
        b = self._make_block([], buttons=["run"])
        assert b.run_it is True

    def test_run_it_false_when_ada_norun(self):
        # ada-norun overrides even when "run" is in buttons
        b = self._make_block(["ada-norun"], buttons=["run"])
        assert b.run_it is False

    # The C run classes, which a course author may write and CONTRIBUTING.md
    # documents.  They are asserted one class at a time and with no button
    # present, because a button would make every one of these pass on its own
    # and say nothing about the class.  Their Ada counterparts are covered
    # above; what is new here is that the C spellings are read at all, and
    # that they are read only on a C block.

    def test_run_it_from_c_run_class_on_a_c_block(self):
        """c-run alone must ask for a run, the way ada-run does."""
        b = self._make_block(["c-run"], language="c")
        assert b.run_it is True

    def test_run_it_from_c_run_expect_failure_class_on_a_c_block(self):
        """c-run-expect-failure alone must ask for a run.

        Nothing can expect a run to fail without a run happening, so a class
        that declares the expectation and does not cause the run leaves the
        handling of that expectation unreachable.
        """
        b = self._make_block(["c-run-expect-failure"], language="c")
        assert b.run_it is True

    def test_run_it_false_for_a_c_block_declaring_nothing(self):
        """A C block that asks for nothing must not be run.

        The control for the two above: without it they would pass equally
        well against a derivation that ran every C block.
        """
        b = self._make_block([], language="c")
        assert b.run_it is False

    def test_run_it_false_when_c_norun_suppresses_a_run_button(self):
        """c-norun must suppress a run the button asked for, as ada-norun
        does."""
        b = self._make_block(["c-norun"], buttons=["run"], language="c")
        assert b.run_it is False

    def test_run_it_false_when_c_norun_suppresses_the_c_run_class(self):
        """Asking for a run and suppressing it in the same breath must
        suppress: the two C classes are not read independently of each
        other."""
        b = self._make_block(["c-run", "c-norun"], language="c")
        assert b.run_it is False

    def test_run_it_false_for_c_run_class_on_an_ada_block(self):
        """A C run class on an Ada block must not cause a run.

        The class is paired with the language the way the compile classes
        already are, so writing the wrong language's spelling asks for
        nothing rather than for a run of a block it does not describe.
        """
        b = self._make_block(["c-run"], language="ada")
        assert b.run_it is False

    def test_run_it_false_for_c_run_expect_failure_class_on_an_ada_block(self):
        """Same pairing for the expect-failure spelling."""
        b = self._make_block(["c-run-expect-failure"], language="ada")
        assert b.run_it is False

    # The Ada run classes read on a block of the other language, and the two
    # norun classes read on a block they do not describe.  The C positives
    # above were already paired with their language; these are the remaining
    # four spellings, so that every class naming a language is honored only
    # for a block written in it.
    #
    # The compile is asserted beside the run wherever the run is taken away,
    # because it is the consequence that matters: compile_it is derived as
    # "run_it or ...", so a class that stops asking for a run also stops the
    # block from being built, and a block that is never built is checked by
    # nothing at all.

    def test_ada_run_class_on_a_c_block_asks_for_no_run_and_no_compile(self):
        """ada-run on a C block must ask for nothing.

        The class names Ada, so it does not describe this block.  Before the
        pairing it asked for a run, and the build dispatches on the block's
        own language, so the block really was built with gcc and run -- a
        visible mistake rather than a silent one.
        """
        b = self._make_block(["ada-run"], language="c")
        assert b.run_it is False
        assert b.compile_it is False

    def test_ada_run_expect_failure_class_on_a_c_block_asks_for_nothing(self):
        """Same pairing for the expect-failure spelling.

        Written separately from the plain spelling rather than left to it:
        the two class names are read as one set, so a derivation that stopped
        pairing this one would still satisfy the test above.
        """
        b = self._make_block(["ada-run-expect-failure"], language="c")
        assert b.run_it is False
        assert b.compile_it is False

    @pytest.mark.parametrize("code_class",
                             ["ada-run", "ada-run-expect-failure"])
    def test_the_ada_run_classes_still_ask_for_a_run_on_an_ada_block(
            self, code_class):
        """The control for the two tests above.

        Without it they are equally well satisfied by a derivation that
        refused every run, which would take the Ada classes away from the
        blocks they do describe.
        """
        b = self._make_block([code_class], language="ada")
        assert b.run_it is True

    def test_a_c_compile_class_on_an_ada_block_asks_for_no_compile(self):
        """The control for "no compile" above.

        The compile classes have been paired with the block's language all
        along, so this is the shape the run classes now follow.  It says that
        a compile_it of False is attributable to the class naming the other
        language, rather than to some other route through the derivation that
        would leave every block of this shape unbuilt.
        """
        b = self._make_block(["c-compile"], language="ada")
        assert b.compile_it is False

    def test_ada_norun_on_a_c_block_does_not_suppress_a_run_button(self):
        """ada-norun must not take a run away from a C block.

        Suppressing a run is the direction where the old, unpaired reading
        was itself the silent skip: a stray Ada norun on a C block took away
        a run the author had asked for, and nothing said so.
        """
        b = self._make_block(["ada-norun"], buttons=["run"], language="c")
        assert b.run_it is True

    def test_ada_norun_on_a_c_block_does_not_suppress_the_c_run_class(self):
        """The same, where the run was asked for by a class rather than by a
        button -- the two are separate terms of the derivation."""
        b = self._make_block(["ada-norun", "c-run"], language="c")
        assert b.run_it is True

    def test_c_norun_on_an_ada_block_does_not_suppress_a_run_button(self):
        """The mirror of the ada-norun case, on an Ada block."""
        b = self._make_block(["c-norun"], buttons=["run"], language="ada")
        assert b.run_it is True

    def test_c_norun_on_an_ada_block_does_not_suppress_ada_run(self):
        """c-norun must leave ada-run alone, and the block must still be
        built.

        This is the combination in which the unpaired reading did the most
        damage: the run was canceled, so the compile went with it, and an
        Ada example nobody built was recorded as having passed.
        """
        b = self._make_block(["ada-run", "c-norun"], language="ada")
        assert b.run_it is True
        assert b.compile_it is True

    def test_c_norun_on_an_ada_block_does_not_suppress_the_expect_failure_class(
            self):
        """The same for the expect-failure spelling of the Ada run class."""
        b = self._make_block(["ada-run-expect-failure", "c-norun"],
                             language="ada")
        assert b.run_it is True

    def test_the_norun_classes_still_suppress_on_their_own_language(self):
        """The control for the four tests above.

        Asserted as one test over both spellings so that a pairing widened
        until it never suppresses anything reddens something that names the
        property, rather than only the C case or only the Ada one.
        """
        ada = self._make_block(["ada-norun"], buttons=["run"], language="ada")
        c = self._make_block(["c-norun"], buttons=["run"], language="c")
        assert (ada.run_it, c.run_it) == (False, False)

    def test_a_run_button_asks_for_a_run_whatever_the_language_is(self):
        """A run button is not paired with any language, deliberately.

        Only the classes name a language; the button says "run this" about
        whatever the block happens to be written in.  Pinned here so that a
        later completion of the pairing, applied to the button as well,
        cannot silently stop running every block of a language the classes do
        not spell.
        """
        b = self._make_block([], buttons=["run"], language="cpp")
        assert b.run_it is True

    def test_ada_syntax_only_on_a_c_block_is_still_syntax_only(self):
        """The syntax-only class is not paired with a language either.

        It is the one class/language mismatch the material really carries: a
        C block declaring ada-syntax-only, which stops at the syntax check
        and is meant to.  Pinned so that a pairing widened to this class is
        caught here rather than by a content build.
        """
        b = self._make_block(["ada-syntax-only"], language="c")
        assert b.syntax_only is True

    def test_the_nocheck_classes_are_not_paired_with_a_language(self):
        """Either spelling of the no-check class suppresses the check on
        either language.

        Also deliberate, and asserted over both spellings at once for the
        same reason the norun control is.  The two names are documented as
        the Ada one and the C one, and that difference is recorded rather
        than acted on -- so a pairing applied here would quietly take the
        opposite decision.
        """
        ada_on_c = self._make_block(["ada-nocheck"], language="c")
        c_on_ada = self._make_block(["c-nocheck"], language="ada")
        assert (ada_on_c.no_check, c_on_ada.no_check) == (True, True)

    def test_compile_it_true_when_a_c_block_is_run_by_class(self):
        """A run implies a compile for the C classes too, so a C block asking
        to be run by class alone has something to run."""
        b = self._make_block(["c-run"], language="c")
        assert b.compile_it is True

    def test_compile_it_true_when_run_it_true(self):
        b = self._make_block(["ada-run"])
        assert b.compile_it is True

    def test_compile_it_from_ada_compile_class(self):
        b = self._make_block(["ada-compile"])
        assert b.compile_it is True

    def test_compile_it_false_default(self):
        b = self._make_block([])
        assert b.compile_it is False

    def test_prove_it_from_ada_prove_class(self):
        b = self._make_block(["ada-prove"])
        assert b.prove_it is True

    def test_prove_it_from_prove_button(self):
        b = self._make_block([], buttons=["prove"])
        assert b.prove_it is True

    def test_prove_it_false_default(self):
        b = self._make_block([])
        assert b.prove_it is False

    # The two hashes are tested for the properties the rest of the package
    # relies on, not against a fixed digest: the short hash names a block's
    # project directory and the long one keys its check cache, so nothing
    # outside this package requires any particular algorithm, and a pinned
    # digest would freeze one for no benefit.

    # Hash the given text in a fresh interpreter, in a block whose every other
    # field differs from the one the test builds in process.  Two things have
    # to be true at once and neither alone is enough: the hash must survive a
    # process boundary -- one that folds in a value drawn per process is
    # perfectly stable within a single run, and still moves the project
    # directory and orphans the cached check result on the next one -- and it
    # must be a function of the block text alone, or moving a block to another
    # file, or editing the line above it, has the same effect.
    _HASH_PROBE = textwrap.dedent(
        """
        import json, sys
        from rst_code_example_pipeline.blocks import CodeBlock

        block = CodeBlock(
            rst_file="other.rst",
            line_start=42,
            line_end=99,
            text=sys.argv[1],
            language="c",
            project="OtherProject",
            main_file="other.c",
            gnat_version=["selected", "1.2.3-4"],
            gnatprove_version=["selected", "1.2.3-4"],
            gprbuild_version=["selected", "1.2.3-4"],
            compiler_switches=["-gnatwa"],
            classes=["c-nocheck"],
            manual_chop=True,
            buttons=["run"],
        )
        print(json.dumps([block.text_hash, block.text_hash_short]))
        """
    )

    def test_text_hashes_are_deterministic_across_runs(self):
        """The same block text must hash the same way on every run and in every
        block that carries it, or a block's project directory moves and its
        cached check result is never found again."""
        b = self._make_block([])
        output = subprocess.check_output(
            [sys.executable, "-c", self._HASH_PROBE, b.text], text=True)
        fresh_hash, fresh_hash_short = json.loads(output)
        assert fresh_hash == b.text_hash
        assert fresh_hash_short == b.text_hash_short

    def test_text_hashes_distinguish_different_text(self):
        """Two blocks with different text must hash differently, or one
        block's extracted project overwrites the other's and one of the two
        is silently never checked."""
        b1 = self._make_block([], text="procedure P is null;")
        b2 = self._make_block([], text="procedure Q is null;")
        assert b1.text_hash != b2.text_hash
        assert b1.text_hash_short != b2.text_hash_short

    def test_text_hashes_are_usable_as_directory_names(self):
        """The short hash is used verbatim as a directory name, so both
        hashes must be non-empty lowercase hexadecimal with nothing in them
        that a path would have to escape."""
        b = self._make_block([])
        assert re.fullmatch(r"[0-9a-f]+", b.text_hash)
        assert re.fullmatch(r"[0-9a-f]+", b.text_hash_short)


# ---------------------------------------------------------------------------
# T-blocks-13: CodeBlock JSON round-trip
# ---------------------------------------------------------------------------

class TestCodeBlockJsonRoundTrip:
    def _make_block(self):
        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()
        return CodeBlock(
            rst_file="foo.rst",
            line_start=1,
            line_end=10,
            text="procedure P is null;",
            language="ada",
            project="MyProj",
            main_file="main.adb",
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=[],
            manual_chop=False,
            buttons=[],
        )

    def test_round_trip_basic_fields(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        b = self._make_block()
        b.to_json_file()
        b2 = CodeBlock.from_json_file()
        assert b2 is not None
        assert b2.rst_file == "foo.rst"
        assert b2.language == "ada"
        assert b2.project == "MyProj"
        assert b2.main_file == "main.adb"

    def test_round_trip_active_true(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        b = self._make_block()
        b.to_json_file()
        b2 = CodeBlock.from_json_file()
        assert b2 is not None
        assert b2.active is True

    def test_round_trip_explicit_filename(self, tmp_path):
        b = self._make_block()
        f = str(tmp_path / "info.json")
        b.to_json_file(f)
        b2 = CodeBlock.from_json_file(f)
        assert b2 is not None
        assert b2.text == "procedure P is null;"

    def test_from_json_file_nonexistent(self, tmp_path):
        f = str(tmp_path / "no_such.json")
        assert CodeBlock.from_json_file(f) is None


# ---------------------------------------------------------------------------
# A block record that is present but cannot be turned into a block
# ---------------------------------------------------------------------------

class TestCodeBlockRecordThatCannotBeRead:
    """A block record file that exists but does not describe a block.

    The reader used to check only that the file was there, so anything past
    that point left the reader as an exception -- and it is the reader both
    commands go through, so the traceback came out of whichever one was
    running.  Each case below is a different way for the file to be
    unusable, and each must come back as no block at all, with a message
    saying which file it was and why it could not be used.

    A record written by the extraction step is never in any of these states.
    These are the file after something else has been at it: a truncated
    write, a hand edit, a merge that went wrong.
    """

    # The text of a record that is present and unusable, one entry per way of
    # being unusable.  The first two never parse; the third parses into
    # something that is not a record; the fourth is a record with none of the
    # fields a block is made of.
    UNUSABLE_TEXTS = {
        "truncated": '{"rst_file": "test.rst", "line_start": 1',
        "not_json_at_all": "this file is not JSON",
        "json_but_not_an_object": "[1, 2, 3]",
        "an_object_with_none_of_the_fields": '{"something": "else"}',
    }

    # The fifth way, built from a real block at test time rather than written
    # out here: a complete, valid record of a real block, carrying one field
    # a block is not made of -- a record written by a later version of the
    # package than the one reading it.  It is valid JSON and an object of the
    # right shape, so it gets as far as being handed to the block, which is
    # where it is refused.  This is the case that shows the guard is not
    # merely a check that the text parses.
    A_RECORD_FROM_A_LATER_FORMAT = "a_record_from_a_later_format"

    ALL_CASES = sorted(UNUSABLE_TEXTS) + [A_RECORD_FROM_A_LATER_FORMAT]

    def _record_text(self, case: str) -> str:
        if case in self.UNUSABLE_TEXTS:
            return self.UNUSABLE_TEXTS[case]

        if not info.DEFAULT_VERSION:
            info.init_toolchain_info()
        block = CodeBlock(
            rst_file="foo.rst",
            line_start=1,
            line_end=10,
            text="procedure P is null;",
            language="ada",
            project="MyProj",
            main_file="main.adb",
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
            compiler_switches=["-gnata"],
            classes=[],
            manual_chop=False,
            buttons=[],
        )
        record = json.loads(json.dumps(block, default=lambda o: o.__dict__))
        record["a_field_this_version_does_not_know"] = "from a later format"
        return json.dumps(record)

    @pytest.mark.parametrize("case", ALL_CASES)
    def test_an_unusable_record_is_reported_as_no_block(self, case, tmp_path,
                                                        capsys):
        """Reading an unusable record must come back as no block, and must
        say which file could not be read and what was wrong with it.

        The reason is asserted separately from the file name, because the
        name alone is what the callers already print for themselves -- the
        reader is the only place that knows why.
        """
        json_file = str(tmp_path / "block_info.json")
        (tmp_path / "block_info.json").write_text(self._record_text(case))

        assert CodeBlock.from_json_file(json_file) is None, \
            "a record that cannot be turned into a block must read back as " \
            "no block rather than as an exception"

        out = capsys.readouterr().out
        assert "ERROR" in out, \
            "an unusable record must be reported: {}".format(out)
        assert json_file in out, \
            "the report must name the file it could not read: {}".format(out)
        assert out.split(json_file, 1)[1].strip(" :\n"), \
            "the report must say why the file could not be used, not only " \
            "which file it was: {}".format(out)

    def test_bytes_that_are_not_valid_utf8_are_reported_as_no_block(
            self, tmp_path, capsys):
        """A record file holding bytes that are not valid UTF-8 must also
        come back as no block, reported the same way, not as an exception.

        Every case above is written with ``Path.write_text()``, which is
        UTF-8 by construction and so cannot exercise this: the file it
        produces is always decodable. This case writes raw bytes instead --
        a lead byte with no valid meaning in UTF-8, the kind a hand edit in
        an editor defaulting to another encoding leaves behind. Decoding it
        raises ``UnicodeDecodeError``, which is a *sibling* of
        ``json.JSONDecodeError`` under ``ValueError`` rather than a subclass,
        so the reader has to name it separately or this case would escape as
        an uncaught exception instead of the reported failure the other
        unusable records get.
        """
        json_file = str(tmp_path / "block_info.json")
        (tmp_path / "block_info.json").write_bytes(b"\xff\xfe not valid utf-8")

        assert CodeBlock.from_json_file(json_file) is None, \
            "a record that is not valid UTF-8 must read back as no block " \
            "rather than as an exception"

        out = capsys.readouterr().out
        assert "ERROR" in out, \
            "an unreadable record must be reported: {}".format(out)
        assert json_file in out, \
            "the report must name the file it could not read: {}".format(out)
        assert out.split(json_file, 1)[1].strip(" :\n"), \
            "the report must say why the file could not be used, not only " \
            "which file it was: {}".format(out)


# ---------------------------------------------------------------------------
# T-blocks-14: ConfigBlock.__init__ and update()
# ---------------------------------------------------------------------------

class TestConfigBlock:
    def test_run_button_false(self):
        cb = ConfigBlock("test.rst", run_button="False")
        # run_button is set dynamically via setattr() in ConfigBlock.__init__,
        # so it is looked up with getattr() rather than direct attribute access.
        assert getattr(cb, "run_button") is False

    def test_prove_button_true(self):
        cb = ConfigBlock("test.rst", prove_button="True")
        assert getattr(cb, "prove_button") is True

    def test_accumulate_code_false(self):
        cb = ConfigBlock("test.rst", accumulate_code="False")
        assert getattr(cb, "accumulate_code") is False

    def test_rst_file_stored(self):
        cb = ConfigBlock("my.rst", run_button="True")
        assert cb.rst_file == "my.rst"

    def test_opts_stored(self):
        cb = ConfigBlock("my.rst", run_button="True", accumulate_code="False")
        assert "run_button" in cb._opts
        assert "accumulate_code" in cb._opts

    def test_update_replaces_opts(self):
        cb1 = ConfigBlock("my.rst", run_button="False", accumulate_code="True")
        cb2 = ConfigBlock("my.rst", run_button="True", accumulate_code="False")
        cb1.update(cb2)
        assert getattr(cb1, "run_button") is True
        assert getattr(cb1, "accumulate_code") is False

    def test_no_opts(self):
        cb = ConfigBlock("my.rst")
        assert cb._opts == {}

    # A configuration value normally arrives as a string, written in a
    # code-config directive, and only the string "False" means false.  The
    # tests above cover that.  The ones below cover a caller that hands over a
    # real boolean instead, which is what the package's own default
    # configuration does -- and which used to be compared against a string it
    # could never equal, so that every such value came out true whatever was
    # asked for.
    #
    # Nothing in the package reads these attributes back, so no other
    # behavior depends on them and no other test can go red for this.  These
    # assertions are the whole of what holds it.

    def test_a_real_false_is_kept_false(self):
        cb = ConfigBlock("test.rst", run_button=False)
        assert getattr(cb, "run_button") is False, \
            "a caller passing a real False means false, and must not be " \
            "given back the opposite of what it asked for"

    def test_a_real_true_is_kept_true(self):
        cb = ConfigBlock("test.rst", run_button=True)
        assert getattr(cb, "run_button") is True

    def test_real_booleans_that_differ_produce_configurations_that_differ(self):
        """Two configurations built from opposite real booleans must not agree.

        The per-value assertions above each name one attribute, so a
        coercion that answered true for everything would need all of them to
        catch it.  This one fails on the collapse itself: the two objects
        were once identical and all-true, whatever was asked for.
        """
        asked_for_false = ConfigBlock(
            "test.rst", run_button=False, prove_button=False,
            accumulate_code=False)
        asked_for_true = ConfigBlock(
            "test.rst", run_button=True, prove_button=True,
            accumulate_code=True)
        for name in ("run_button", "prove_button", "accumulate_code"):
            assert getattr(asked_for_false, name) != \
                getattr(asked_for_true, name), \
                "opposite requests must not produce the same value for " \
                "{}".format(name)

    def test_a_string_that_is_not_False_is_still_true(self):
        """The string reading is unchanged: only "False" is false.

        Written down because it is the reading every value coming out of a
        directive gets, and because a fix aimed at real booleans could
        plausibly have made a string like this one false as well.
        """
        cb = ConfigBlock("test.rst", run_button="no")
        assert getattr(cb, "run_button") is True


# ---------------------------------------------------------------------------
# T-blocks-15: gnatprove_version and gprbuild_version selected attributes
# (covers the "selected" branch of gnatprove= and gprbuild= version parsing)
# ---------------------------------------------------------------------------

class TestGnatproveVersionSelected:
    RST = minimal_rst("""\
.. code:: ada gnatprove=12.1.0-1

   procedure P is null;
""")

    def test_gnatprove_version_is_selected(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gnatprove_version == ["selected", "12.1.0-1"]


class TestGprbuildVersionSelected:
    RST = minimal_rst("""\
.. code:: ada gprbuild=22.0.0-1

   procedure P is null;
""")

    def test_gprbuild_version_is_selected(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].gprbuild_version == ["selected", "22.0.0-1"]


# ---------------------------------------------------------------------------
# T-blocks-16: default compiler switch not duplicated when already explicit
# ---------------------------------------------------------------------------

class TestDefaultSwitchNotDuplicated:
    RST = minimal_rst("""\
.. code:: ada switches=Compiler(-gnata)

   procedure P is null;
""")

    def test_gnata_not_duplicated(self):
        """-gnata is both the explicit switch and the default; it must only
        appear once in compiler_switches (the default-switches loop must skip
        adding it again)."""
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].compiler_switches.count("-gnata") == 1


# ---------------------------------------------------------------------------
# T-blocks-17: switches= value not shaped like Compiler(...)
# ---------------------------------------------------------------------------

class TestSwitchesValueNotCompilerShaped:
    RST = minimal_rst("""\
.. code:: ada switches=Foo(-gnata)

   procedure P is null;
""")

    def test_parses_without_error(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert len(blocks) == 1

    def test_no_explicit_switches_beyond_defaults(self):
        """switches=Foo(-gnata) is present but not shaped like Compiler(...),
        so the captured value is never used; only the default -gnata switch
        is present."""
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)
        assert blocks[0].compiler_switches == ["-gnata"]
