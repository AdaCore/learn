"""
Unit tests for rst_code_example_pipeline.fmt_utils.

Covers:
- header() returns string containing the input and the correct '*' underline
- error() prints to stdout; captured output contains "ERROR", loc, and msg
- simple_error() prints msg to stdout
- simple_success() prints msg to stdout
- Adversarial: empty string, Unicode string with non-ASCII characters
"""
import pytest

from rst_code_example_pipeline import fmt_utils
from rst_code_example_pipeline.colors import Colors, no_colors


@pytest.fixture(autouse=True)
def disable_colors_for_tests():
    """Disable ANSI codes so assertions on plain text are predictable."""
    original = Colors._enabled
    Colors._enabled = False
    yield
    Colors._enabled = original


# ---------------------------------------------------------------------------
# T-fmt_utils-01: header()
# ---------------------------------------------------------------------------

class TestHeader:
    def test_header_contains_string(self):
        result = fmt_utils.header("Hello")
        assert "Hello" in result

    def test_header_contains_stars_of_correct_length(self):
        s = "Hello"
        result = fmt_utils.header(s)
        assert '*' * len(s) in result

    def test_header_returns_str(self):
        assert isinstance(fmt_utils.header("x"), str)

    def test_header_empty_string(self):
        result = fmt_utils.header("")
        # "" has length 0 so the '*' block is also empty; just must not crash
        assert isinstance(result, str)

    def test_header_unicode(self):
        s = "Ünïcödé"
        result = fmt_utils.header(s)
        assert s in result
        assert '*' * len(s) in result

    def test_header_star_count_matches_message_length(self):
        for msg in ["a", "ab", "abc", "Hello, world!"]:
            result = fmt_utils.header(msg)
            assert '*' * len(msg) in result, f"star line missing for msg={msg!r}"

    def test_header_ends_with_newline(self):
        result = fmt_utils.header("Test")
        # col() wraps the whole string; with colors disabled it is the raw string
        # which ends with "\n"
        assert result.endswith("\n")


# ---------------------------------------------------------------------------
# T-fmt_utils-02: error()
# ---------------------------------------------------------------------------

class TestError:
    def test_error_contains_ERROR(self, capsys):
        fmt_utils.error("file.rst:10", "something went wrong")
        captured = capsys.readouterr()
        assert "ERROR" in captured.out

    def test_error_contains_loc(self, capsys):
        fmt_utils.error("src/foo.rst:42", "bad thing")
        captured = capsys.readouterr()
        assert "src/foo.rst:42" in captured.out

    def test_error_contains_msg(self, capsys):
        fmt_utils.error("x", "my error message")
        captured = capsys.readouterr()
        assert "my error message" in captured.out

    def test_error_writes_to_stdout(self, capsys):
        fmt_utils.error("loc", "msg")
        captured = capsys.readouterr()
        assert captured.out != ""
        assert captured.err == ""

    def test_error_empty_loc_and_msg(self, capsys):
        fmt_utils.error("", "")
        captured = capsys.readouterr()
        assert "ERROR" in captured.out

    def test_error_unicode(self, capsys):
        fmt_utils.error("über.rst:1", "Ünïcödé error")
        captured = capsys.readouterr()
        assert "über.rst:1" in captured.out
        assert "Ünïcödé error" in captured.out


# ---------------------------------------------------------------------------
# T-fmt_utils-03: simple_error()
# ---------------------------------------------------------------------------

class TestSimpleError:
    def test_simple_error_writes_msg(self, capsys):
        fmt_utils.simple_error("bad stuff")
        captured = capsys.readouterr()
        assert "bad stuff" in captured.out

    def test_simple_error_writes_to_stdout(self, capsys):
        fmt_utils.simple_error("err")
        captured = capsys.readouterr()
        assert captured.err == ""

    def test_simple_error_empty(self, capsys):
        fmt_utils.simple_error("")
        captured = capsys.readouterr()
        # print("") still emits a newline
        assert captured.out == "\n"

    def test_simple_error_unicode(self, capsys):
        fmt_utils.simple_error("erreur: Ünïcödé")
        captured = capsys.readouterr()
        assert "Ünïcödé" in captured.out


# ---------------------------------------------------------------------------
# T-fmt_utils-04: simple_success()
# ---------------------------------------------------------------------------

class TestSimpleSuccess:
    def test_simple_success_writes_msg(self, capsys):
        fmt_utils.simple_success("all good")
        captured = capsys.readouterr()
        assert "all good" in captured.out

    def test_simple_success_writes_to_stdout(self, capsys):
        fmt_utils.simple_success("ok")
        captured = capsys.readouterr()
        assert captured.err == ""

    def test_simple_success_empty(self, capsys):
        fmt_utils.simple_success("")
        captured = capsys.readouterr()
        assert captured.out == "\n"

    def test_simple_success_unicode(self, capsys):
        fmt_utils.simple_success("Ünïcödé success")
        captured = capsys.readouterr()
        assert "Ünïcödé" in captured.out
