"""
Unit tests for rst_code_example_pipeline.blocks.

Covers:
- Block.get_blocks_from_rst(): RST parser (all attributes, derived fields)
- CodeBlock constructor derived fields (no_check, syntax_only, run_it, compile_it,
  prove_it, text_hash, text_hash_short)
- CodeBlock.to_json_file() + from_json_file() round-trip
- ConfigBlock.__init__ and update()
- Adversarial: empty RST, missing json file, exit(1) path

NOTE: get_blocks_from_rst() calls toolchain_info.get_toolchain_default_version()
at parse time; requires the Ada toolchain .ini
is present and toolchain_info initialises correctly.
"""
import hashlib
import os

import pytest

from rst_code_example_pipeline.blocks import Block, CodeBlock, ConfigBlock


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

    def test_type_is_codeblock(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert isinstance(blocks[0], CodeBlock)

    def test_rst_file_stored(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].rst_file == RST_FILE

    def test_language_is_ada(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].language == "ada"

    def test_project_is_none(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].project is None

    def test_main_file_is_none(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].main_file is None

    def test_manual_chop_false(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].manual_chop is False

    def test_default_compiler_switches_includes_gnata(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert "-gnata" in blocks[0].compiler_switches

    def test_gnat_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].gnat_version[0] == "default"

    def test_gnatprove_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].gnatprove_version[0] == "default"

    def test_gprbuild_version_default(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].gprbuild_version[0] == "default"

    def test_line_start_and_end_set(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].line_start >= 0
        assert blocks[0].line_end > blocks[0].line_start

    def test_text_not_empty(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].text.strip() != ""

    def test_active_defaults_to_true(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
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
        assert blocks[0].project == "MyProject"

    def test_main_file(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
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
        switches = blocks[0].compiler_switches
        assert "-gnatwa" in switches
        assert "-gnatwe" in switches

    def test_default_gnata_also_present(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
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
        assert blocks[0].manual_chop is True

    def test_language_is_c(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
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
        assert "run" in blocks[0].buttons

    def test_compile_button(self):
        rst = minimal_rst("""\
.. code:: ada compile_button

   procedure P is null;
""")
        blocks = Block.get_blocks_from_rst(RST_FILE, rst)
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
        assert cb.run_button is False
        assert cb.prove_button is True
        assert cb.accumulate_code is False


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

    def test_order_preserved(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        code_blocks = [b for b in blocks if isinstance(b, CodeBlock)]
        # First block comes before second
        assert code_blocks[0].line_start < code_blocks[1].line_start


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
        is successfully parsed (no SystemExit)."""
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST_WITH_CONTENT)
        assert len(blocks) == 1
        assert isinstance(blocks[0], CodeBlock)

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
    def _make_block(self, classes, buttons=None, language="ada"):
        return CodeBlock(
            rst_file="test.rst",
            line_start=0,
            line_end=5,
            text="procedure P is null;",
            language=language,
            project=None,
            main_file=None,
            gnat_version=["default", "15.1.0-2"],
            gnatprove_version=["default", "15.1.0-1"],
            gprbuild_version=["default", "25.0.0-1"],
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

    def test_text_hash_is_str(self):
        b = self._make_block([])
        assert isinstance(b.text_hash, str)

    def test_text_hash_short_is_str(self):
        b = self._make_block([])
        assert isinstance(b.text_hash_short, str)

    def test_text_hash_deterministic(self):
        text = "procedure P is null;"
        b1 = self._make_block([])
        b2 = self._make_block([])
        assert b1.text_hash == b2.text_hash

    def test_text_hash_sha512(self):
        text = "procedure P is null;"
        b = self._make_block([])
        expected = hashlib.sha512(text.encode("utf-8")).hexdigest()
        assert b.text_hash == expected

    def test_text_hash_short_md5(self):
        text = "procedure P is null;"
        b = self._make_block([])
        expected = hashlib.md5(text.encode("utf-8")).hexdigest()
        assert b.text_hash_short == expected


# ---------------------------------------------------------------------------
# T-blocks-13: CodeBlock JSON round-trip
# ---------------------------------------------------------------------------

class TestCodeBlockJsonRoundTrip:
    def _make_block(self):
        return CodeBlock(
            rst_file="foo.rst",
            line_start=1,
            line_end=10,
            text="procedure P is null;",
            language="ada",
            project="MyProj",
            main_file="main.adb",
            gnat_version=["default", "15.1.0-2"],
            gnatprove_version=["default", "15.1.0-1"],
            gprbuild_version=["default", "25.0.0-1"],
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
# T-blocks-14: ConfigBlock.__init__ and update()
# ---------------------------------------------------------------------------

class TestConfigBlock:
    def test_run_button_false(self):
        cb = ConfigBlock("test.rst", run_button="False")
        assert cb.run_button is False

    def test_prove_button_true(self):
        cb = ConfigBlock("test.rst", prove_button="True")
        assert cb.prove_button is True

    def test_accumulate_code_false(self):
        cb = ConfigBlock("test.rst", accumulate_code="False")
        assert cb.accumulate_code is False

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
        assert cb1.run_button is True
        assert cb1.accumulate_code is False

    def test_no_opts(self):
        cb = ConfigBlock("my.rst")
        assert cb._opts == {}


# ---------------------------------------------------------------------------
# T-blocks-15: gnatprove_version and gprbuild_version selected attributes
# (covers blocks.py lines 129 and 133)
# ---------------------------------------------------------------------------

class TestGnatproveVersionSelected:
    RST = minimal_rst("""\
.. code:: ada gnatprove=12.1.0-1

   procedure P is null;
""")

    def test_gnatprove_version_is_selected(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].gnatprove_version == ["selected", "12.1.0-1"]


class TestGprbuildVersionSelected:
    RST = minimal_rst("""\
.. code:: ada gprbuild=22.0.0-1

   procedure P is null;
""")

    def test_gprbuild_version_is_selected(self):
        blocks = Block.get_blocks_from_rst(RST_FILE, self.RST)
        assert blocks[0].gprbuild_version == ["selected", "22.0.0-1"]
