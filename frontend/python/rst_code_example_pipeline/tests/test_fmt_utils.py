"""
Unit tests for rst_code_example_pipeline.fmt_utils.

Covers:
- header(): the message followed by a '*' underline of matching length
- error(): "ERROR <loc>: <msg>" written to stdout
- simple_error() and simple_success(): the message written to stdout
- Adversarial: empty string, Unicode string with non-ASCII characters

Each function gets one exact-output assertion rather than several partial
ones, plus the edge cases that exercise a different input shape.  The
underline lengths below are spelled out as literals on purpose: recomputing
them with the same '*' * len(...) expression the source uses would hide a
character-versus-byte length bug instead of catching it.
"""
import pytest

from rst_code_example_pipeline import fmt_utils
from rst_code_example_pipeline.colors import Colors


@pytest.fixture(autouse=True)
def disable_colors_for_tests():
    """Disable ANSI codes so assertions on plain text are predictable.

    The shared fixture in conftest.py puts the previous setting back, so this
    one only has to establish the setting these tests need.
    """
    Colors._enabled = False


# ---------------------------------------------------------------------------
# T-fmt_utils-01: header()
# ---------------------------------------------------------------------------

class TestHeader:
    def test_header_exact_output(self):
        assert fmt_utils.header("Hello") == "Hello\n*****\n"

    def test_header_empty_string(self):
        """An empty message underlines nothing, so both lines are empty."""
        assert fmt_utils.header("") == "\n\n"

    def test_header_unicode(self):
        """The underline is as long as the message in characters, not bytes:
        the seven letters below occupy more than seven bytes in UTF-8."""
        assert fmt_utils.header("Ünïcödé") == "Ünïcödé\n*******\n"


# ---------------------------------------------------------------------------
# T-fmt_utils-02: error()
# ---------------------------------------------------------------------------

class TestError:
    def test_error_exact_output(self, capsys):
        fmt_utils.error("src/foo.rst:42", "something went wrong")
        captured = capsys.readouterr()
        assert captured.out == "ERROR src/foo.rst:42: something went wrong\n"
        assert captured.err == ""

    def test_error_empty_loc_and_msg(self, capsys):
        fmt_utils.error("", "")
        captured = capsys.readouterr()
        assert captured.out == "ERROR : \n"

    def test_error_unicode(self, capsys):
        fmt_utils.error("über.rst:1", "Ünïcödé error")
        captured = capsys.readouterr()
        assert captured.out == "ERROR über.rst:1: Ünïcödé error\n"


# ---------------------------------------------------------------------------
# T-fmt_utils-03: simple_error()
# ---------------------------------------------------------------------------

class TestSimpleError:
    def test_simple_error_exact_output(self, capsys):
        fmt_utils.simple_error("bad stuff")
        captured = capsys.readouterr()
        assert captured.out == "bad stuff\n"
        assert captured.err == ""

    def test_simple_error_empty(self, capsys):
        fmt_utils.simple_error("")
        captured = capsys.readouterr()
        # print("") still emits a newline
        assert captured.out == "\n"

    def test_simple_error_unicode(self, capsys):
        fmt_utils.simple_error("erreur: Ünïcödé")
        captured = capsys.readouterr()
        assert captured.out == "erreur: Ünïcödé\n"


# ---------------------------------------------------------------------------
# T-fmt_utils-04: simple_success()
# ---------------------------------------------------------------------------

class TestSimpleSuccess:
    def test_simple_success_exact_output(self, capsys):
        fmt_utils.simple_success("all good")
        captured = capsys.readouterr()
        assert captured.out == "all good\n"
        assert captured.err == ""

    def test_simple_success_empty(self, capsys):
        fmt_utils.simple_success("")
        captured = capsys.readouterr()
        assert captured.out == "\n"

    def test_simple_success_unicode(self, capsys):
        fmt_utils.simple_success("Ünïcödé success")
        captured = capsys.readouterr()
        assert captured.out == "Ünïcödé success\n"
