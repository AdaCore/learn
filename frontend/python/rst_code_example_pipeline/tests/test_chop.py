"""
Unit tests for rst_code_example_pipeline.chop — edge cases and real_gnatchop.

Covers:
- manual_chop with .ads and .adb extensions
- manual_chop with empty input
- manual_chop with no !filename lines at all (only garbage)
- manual_chop with garbage before first valid file
- cheapo_gnatchop with dotted package name
- cheapo_gnatchop with dotted procedure name
- cheapo_gnatchop with only a spec (package A)
- cheapo_gnatchop with empty input
- cheapo_gnatchop with only garbage (no recognized declaration)
- real_gnatchop: valid Ada, compiler_switches, error handler
  (requires the Ada toolchain)
"""
import pytest

from rst_code_example_pipeline.chop import manual_chop, cheapo_gnatchop, real_gnatchop
from rst_code_example_pipeline.resource import Resource


# ---------------------------------------------------------------------------
# T-chop-01: manual_chop — Ada extensions
# ---------------------------------------------------------------------------

class TestManualChopAdaExtensions:
    def test_adb_extension_recognized(self):
        lines = ["!main.adb", "procedure Main is", "begin null; end Main;"]
        result = manual_chop(lines)
        assert len(result) == 1
        assert result[0].basename == "main.adb"

    def test_ads_extension_recognized(self):
        lines = ["!pkg.ads", "package Pkg is", "end Pkg;"]
        result = manual_chop(lines)
        assert len(result) == 1
        assert result[0].basename == "pkg.ads"

    def test_adb_content_correct(self):
        lines = ["!main.adb", "procedure Main is", "begin null; end Main;"]
        result = manual_chop(lines)
        assert result[0].content == "procedure Main is\nbegin null; end Main;"

    def test_ads_content_correct(self):
        lines = ["!pkg.ads", "package Pkg is", "end Pkg;"]
        result = manual_chop(lines)
        assert result[0].content == "package Pkg is\nend Pkg;"

    def test_adb_and_ads_in_same_input(self):
        lines = [
            "!spec.ads",
            "package Spec is",
            "end Spec;",
            "!body.adb",
            "package body Spec is",
            "end Spec;",
        ]
        result = manual_chop(lines)
        assert len(result) == 2
        assert result[0].basename == "spec.ads"
        assert result[1].basename == "body.adb"


# ---------------------------------------------------------------------------
# T-chop-02: manual_chop — empty and garbage inputs
# ---------------------------------------------------------------------------

class TestManualChopEdgeCases:
    def test_empty_input_returns_empty_list(self):
        assert manual_chop([]) == []

    def test_only_garbage_no_filename_returns_empty_list(self):
        lines = ["no file here", "more garbage", "still nothing"]
        assert manual_chop(lines) == []

    def test_garbage_before_first_file_discarded(self):
        lines = [
            "garbage line 1",
            "garbage line 2",
            "!main.adb",
            "procedure Main is null;",
        ]
        result = manual_chop(lines)
        assert len(result) == 1
        assert result[0].basename == "main.adb"
        assert result[0].content == "procedure Main is null;"

    def test_fake_extension_not_matched(self):
        """A line like !fake.txt must not be treated as a valid file."""
        lines = ["!fake.txt", "some content", "!real.adb", "real content"]
        result = manual_chop(lines)
        assert len(result) == 1
        assert result[0].basename == "real.adb"

    def test_single_filename_no_content(self):
        lines = ["!empty.adb"]
        result = manual_chop(lines)
        assert len(result) == 1
        assert result[0].basename == "empty.adb"
        assert result[0].content == ""

    def test_multiple_files_content_correctly_split(self):
        lines = [
            "!a.ads",
            "package A is",
            "end A;",
            "!a.adb",
            "package body A is",
            "end A;",
            "!main.adb",
            "procedure Main is null;",
        ]
        result = manual_chop(lines)
        assert len(result) == 3
        assert result[0].content == "package A is\nend A;"
        assert result[1].content == "package body A is\nend A;"
        assert result[2].content == "procedure Main is null;"


# ---------------------------------------------------------------------------
# T-chop-03: cheapo_gnatchop — dotted names
# ---------------------------------------------------------------------------

class TestCheapoGnatchopDottedNames:
    def test_dotted_package_body(self):
        lines = ["package body Foo.Bar is", "end Foo.Bar;"]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "foo-bar.adb"

    def test_dotted_procedure(self):
        lines = ["procedure Foo.Bar is", "begin null; end Foo.Bar;"]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "foo-bar.adb"

    def test_triple_dotted_package_body(self):
        lines = ["package body A.B.C is", "end A.B.C;"]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "a-b-c.adb"

    def test_dotted_package_body_content(self):
        lines = ["package body Foo.Bar is", "end Foo.Bar;"]
        result = cheapo_gnatchop(lines)
        assert result[0].content == "package body Foo.Bar is\nend Foo.Bar;"


# ---------------------------------------------------------------------------
# T-chop-04: cheapo_gnatchop — spec only
# ---------------------------------------------------------------------------

class TestCheapoGnatchopSpecOnly:
    def test_spec_generates_ads(self):
        lines = ["package A is", "end A;"]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "a.ads"

    def test_spec_content_correct(self):
        lines = ["package A is", "end A;"]
        result = cheapo_gnatchop(lines)
        assert result[0].content == "package A is\nend A;"

    def test_dotted_spec(self):
        lines = ["package Foo.Bar is", "end Foo.Bar;"]
        result = cheapo_gnatchop(lines)
        assert result[0].basename == "foo-bar.ads"


# ---------------------------------------------------------------------------
# T-chop-05: cheapo_gnatchop — empty and garbage inputs
# ---------------------------------------------------------------------------

class TestCheapoGnatchopEdgeCases:
    def test_empty_input_returns_empty_list(self):
        assert cheapo_gnatchop([]) == []

    def test_only_garbage_returns_empty_list(self):
        lines = ["garbage line", "more garbage", "-- just a comment"]
        assert cheapo_gnatchop(lines) == []

    def test_garbage_before_first_declaration_discarded(self):
        lines = [
            "-- header comment",
            "with Ada.Text_IO;",
            "package body A is",
            "end A;",
        ]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "a.adb"
        assert "package body A is" in result[0].content

    def test_lowercase_names(self):
        lines = ["package body mypackage is", "end mypackage;"]
        result = cheapo_gnatchop(lines)
        assert result[0].basename == "mypackage.adb"

    def test_procedure_generates_adb(self):
        lines = ["procedure Main is", "begin null; end Main;"]
        result = cheapo_gnatchop(lines)
        assert len(result) == 1
        assert result[0].basename == "main.adb"

    def test_body_before_spec_both_captured(self):
        lines = [
            "package body A is",
            "end A;",
            "package A is",
            "end A;",
        ]
        result = cheapo_gnatchop(lines)
        assert len(result) == 2
        assert result[0].basename == "a.adb"
        assert result[1].basename == "a.ads"


# ---------------------------------------------------------------------------
# T-chop-06: real_gnatchop — Ada toolchain required
# (covers chop.py lines 96-149)
# ---------------------------------------------------------------------------

class TestRealGnatchop:
    """Tests for real_gnatchop; require gnatchop in PATH."""

    VALID_ADA = ["procedure Main is", "begin null; end Main;"]

    def test_valid_ada_no_switches_returns_resources(self):
        """real_gnatchop with compiler_switches=None returns a non-empty list
        of Resource objects (covers line 118 — compiler_switches=None branch)."""
        result = real_gnatchop(self.VALID_ADA, compiler_switches=None)
        assert len(result) >= 1
        assert all(isinstance(r, Resource) for r in result)

    def test_valid_ada_no_switches_basename(self):
        """gnatchop on a minimal procedure Main produces main.adb."""
        result = real_gnatchop(self.VALID_ADA, compiler_switches=None)
        basenames = [r.basename for r in result]
        assert "main.adb" in basenames

    def test_valid_ada_with_compiler_switches(self):
        """real_gnatchop with compiler_switches=["-gnata"] exercises the
        'cmd.extend' path (lines 120-125) and still succeeds."""
        result = real_gnatchop(self.VALID_ADA, compiler_switches=["-gnata"])
        assert len(result) >= 1
        basenames = [r.basename for r in result]
        assert "main.adb" in basenames

    def test_invalid_input_raises_exception(self):
        """Garbage input causes gnatchop to fail; the error handler at lines
        137-144 prints the numbered lines and raises Exception."""
        with pytest.raises(Exception, match="Could not chop files with gnatchop"):
            real_gnatchop(["this is not valid Ada at all !@#$"],
                          compiler_switches=None)

    def test_non_gnat_switch_is_skipped(self):
        """A compiler_switches entry that doesn't contain "gnat" (e.g. -Wall)
        is silently dropped before invoking gnatchop; gnatchop still succeeds
        since gnatchop itself never sees -Wall."""
        result = real_gnatchop(self.VALID_ADA, compiler_switches=["-Wall"])
        assert len(result) == 1
        assert result[0].basename == "main.adb"
