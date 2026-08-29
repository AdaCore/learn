"""
Unit tests for rst_code_example_pipeline.colors.

Covers:
- col() with colors enabled and disabled
- printcol() output captured via capsys
- no_colors() context manager (disable inside, restore outside)
- Colors.disable_colors() and state restore
- Adversarial: direct __enter__/__exit__ use on no_colors(), and restoring the
  previous setting when the guarded block raises
"""
import pytest

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
