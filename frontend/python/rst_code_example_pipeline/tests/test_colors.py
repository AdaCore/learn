"""
Unit tests for rst_code_example_pipeline.colors.

Covers:
- col() with colors enabled and disabled
- printcol() output captured via capsys
- no_colors() context manager (disable inside, restore outside)
- Colors.disable_colors() and state restore
- Adversarial: direct __enter__/__exit__ use on no_colors(), and restoring the
  previous setting when the guarded block raises
- both answers to the terminal test the module makes when it is imported:
  colors survive an import under a terminal and are switched off as soon as
  either of the two output streams is not one
"""
import importlib
import sys

import pytest

from rst_code_example_pipeline import colors as colors_module
from rst_code_example_pipeline.colors import Colors, col, no_colors, printcol


# ---------------------------------------------------------------------------
# T-colors-01: col() enabled
# ---------------------------------------------------------------------------

class TestColEnabled:
    def test_col_wraps_with_prefix_and_endc(self):
        Colors._enabled = True
        result = col("hello", Colors.RED)
        assert result == f"{Colors.RED}hello{Colors.ENDC}"

    def test_col_endc_does_not_double_wrap(self):
        """Passing Colors.ENDC as color should still wrap correctly."""
        Colors._enabled = True
        result = col("msg", Colors.ENDC)
        assert result == f"{Colors.ENDC}msg{Colors.ENDC}"


# ---------------------------------------------------------------------------
# T-colors-02: col() disabled
# ---------------------------------------------------------------------------

class TestColDisabled:
    def test_col_returns_bare_string_when_disabled(self):
        Colors._enabled = False
        assert col("hello", Colors.RED) == "hello"

    def test_col_empty_string_disabled(self):
        Colors._enabled = False
        assert col("", Colors.BLUE) == ""


# ---------------------------------------------------------------------------
# T-colors-03: printcol() output
# ---------------------------------------------------------------------------

class TestPrintcol:
    def test_printcol_prints_the_bare_message_when_disabled(self, capsys):
        Colors._enabled = False
        printcol("hello output", Colors.GREEN)
        captured = capsys.readouterr()
        assert captured.out == "hello output\n"
        assert captured.err == ""

    def test_printcol_prints_the_wrapped_message_when_enabled(self, capsys):
        Colors._enabled = True
        printcol("msg", Colors.RED)
        captured = capsys.readouterr()
        assert captured.out == f"{Colors.RED}msg{Colors.ENDC}\n"
        assert captured.err == ""


# ---------------------------------------------------------------------------
# T-colors-04: no_colors() context manager
# ---------------------------------------------------------------------------

class TestNoColors:
    def test_no_colors_disables_inside(self):
        Colors._enabled = True
        with no_colors():
            assert Colors._enabled is False

    def test_no_colors_restores_outside_when_was_true(self):
        Colors._enabled = True
        with no_colors():
            pass
        assert Colors._enabled is True

    def test_no_colors_restores_outside_when_was_false(self):
        Colors._enabled = False
        with no_colors():
            pass
        assert Colors._enabled is False

    def test_no_colors_col_returns_bare_inside(self):
        Colors._enabled = True
        with no_colors():
            result = col("bare", Colors.RED)
        assert result == "bare"

    def test_no_colors_col_colored_outside(self):
        Colors._enabled = True
        with no_colors():
            pass
        result = col("colored", Colors.RED)
        assert Colors.RED in result

    def test_no_colors_nested(self):
        """Nested no_colors() context managers must each restore correctly."""
        Colors._enabled = True
        with no_colors():
            assert Colors._enabled is False
            with no_colors():
                assert Colors._enabled is False
            assert Colors._enabled is False
        assert Colors._enabled is True


# ---------------------------------------------------------------------------
# T-colors-05: disable_colors()
# ---------------------------------------------------------------------------

class TestDisableColors:
    def test_disable_colors_sets_enabled_false(self):
        Colors._enabled = True
        Colors.disable_colors()
        assert Colors._enabled is False

    def test_col_after_disable_colors(self):
        Colors._enabled = True
        Colors.disable_colors()
        assert col("test", Colors.GREEN) == "test"


# ---------------------------------------------------------------------------
# T-colors-06: Adversarial — direct __enter__/__exit__ on no_colors()
# ---------------------------------------------------------------------------

class TestNoColorsAdversarial:
    def test_direct_enter_exit(self):
        """Using __enter__/__exit__ directly (without `with`) must still restore state."""
        Colors._enabled = True
        ctx = no_colors()
        ctx.__enter__()
        assert Colors._enabled is False
        ctx.__exit__(None, None, None)
        assert Colors._enabled is True

    def test_direct_enter_exit_when_was_false(self):
        Colors._enabled = False
        ctx = no_colors()
        ctx.__enter__()
        assert Colors._enabled is False
        ctx.__exit__(None, None, None)
        assert Colors._enabled is False

    def test_no_colors_restores_state_when_the_block_raises(self):
        """An exception escaping the 'with' block must still restore the
        previous color setting: no_colors() only narrows the scope it was
        given, so a caller that lets an exception through must not be left
        with colors silently disabled for the rest of the process."""
        Colors._enabled = True
        with pytest.raises(ValueError):
            with no_colors():
                assert Colors._enabled is False
                raise ValueError("oops")
        assert Colors._enabled is True


# ---------------------------------------------------------------------------
# T-colors-07: the terminal test the module makes when it is imported
# ---------------------------------------------------------------------------

class _StreamAnsweringIsatty:
    """An output stream that answers the terminal question a given way.

    Everything else is handed to the real stream, so a wrapped stream stays
    usable -- which matters because the one being wrapped is the one pytest
    has put in place to capture output.
    """

    def __init__(self, stream, is_a_tty: bool):
        self._stream = stream
        self._is_a_tty = is_a_tty

    def isatty(self) -> bool:
        return self._is_a_tty

    def __getattr__(self, name):
        return getattr(self._stream, name)


@pytest.fixture()
def imported_under_streams(monkeypatch):
    """Import the module again with the two output streams answering the
    terminal question a given way, and hand back what it decided.

    The decision is made once, at import, from ``sys.stdout`` and
    ``sys.stderr`` -- so the only way to drive it is to import the module
    again with those streams replaced.  Under pytest neither is a terminal,
    which is why the arm that keeps colors on was never entered by anything.

    Re-importing rebinds every name in the module, including the class the
    rest of this file and the shared color-state fixture hold directly.  The
    module's contents are therefore put back afterwards, so that the class
    those two are holding is the class the module goes on exposing -- and so
    that the two modules importing this one are not left looking at a
    different class from everybody else.
    """
    saved = dict(colors_module.__dict__)
    saved_enabled = Colors._enabled

    def reimport(stdout_is_a_tty: bool, stderr_is_a_tty: bool):
        monkeypatch.setattr(
            sys, "stdout",
            _StreamAnsweringIsatty(sys.stdout, stdout_is_a_tty))
        monkeypatch.setattr(
            sys, "stderr",
            _StreamAnsweringIsatty(sys.stderr, stderr_is_a_tty))
        importlib.reload(colors_module)
        return colors_module

    yield reimport

    colors_module.__dict__.clear()
    colors_module.__dict__.update(saved)
    Colors._enabled = saved_enabled


class TestTheTerminalTestMadeAtImport:
    def test_colors_are_kept_when_both_streams_are_terminals(
            self, imported_under_streams):
        """Under a terminal on both streams the module must leave colors on.

        This is the arm the whole class exists for: nothing had ever imported
        the module with a terminal on both streams, so a module that switched
        colors off unconditionally would have looked identical.  Asserted
        through what col() produces as well as through the setting, since the
        setting is only interesting for what it makes the output do.
        """
        reimported = imported_under_streams(True, True)
        assert reimported.Colors._enabled is True, \
            "an import under a terminal must leave colors enabled"
        assert reimported.col("msg", reimported.Colors.RED) == \
            "{}msg{}".format(reimported.Colors.RED, reimported.Colors.ENDC), \
            "colors left enabled must actually color the output"

    def test_colors_are_switched_off_when_stdout_is_not_a_terminal(
            self, imported_under_streams):
        """Standard output not being a terminal must switch colors off.

        The case that matters in practice: the command's output is being piped
        into a log or a build report, where escape sequences are noise.
        """
        reimported = imported_under_streams(False, True)
        assert reimported.Colors._enabled is False, \
            "colors must be switched off when standard output is not a terminal"
        assert reimported.col("msg", reimported.Colors.RED) == "msg", \
            "colors switched off must leave the output bare"

    def test_colors_are_switched_off_when_only_stderr_is_not_a_terminal(
            self, imported_under_streams):
        """Standard error alone not being a terminal must switch colors off
        too.

        Written separately rather than left to the test above: the module asks
        the question of both streams, and one that stopped asking it of
        standard error would still satisfy every other test here.
        """
        reimported = imported_under_streams(True, False)
        assert reimported.Colors._enabled is False, \
            "colors must be switched off when standard error is not a terminal"
        assert reimported.col("msg", reimported.Colors.RED) == "msg", \
            "colors switched off must leave the output bare"

    def test_reimporting_really_produces_a_new_class(
            self, imported_under_streams):
        """The re-import must really replace the class, not hand back the one
        already in place.

        Without this, the three tests above could all be reading the setting
        of the class this file imported at collection time -- which pytest
        leaves switched off -- and would go on passing whatever the module
        decided under the streams they set up.
        """
        assert imported_under_streams(True, True).Colors is not Colors, \
            "re-importing must produce a new class, or the tests above are " \
            "asserting against the class that was already there"

    def test_the_module_still_exposes_the_class_this_file_imported(self):
        """After the re-imports, the module must expose the same class again.

        Collected after them, so it sees what they left behind.  Everything
        else in this file, the shared color-state fixture, and the two modules
        that import this one all hold names bound before any re-import: if the
        module were left exposing the replacement class, they would be setting
        and restoring a flag nothing reads, and every one of those tests would
        pass over an output nobody colored.
        """
        assert colors_module.Colors is Colors, \
            "the module must expose the class this file imported"
        assert colors_module.col is col, \
            "the module must expose the function this file imported"
