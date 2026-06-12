"""
Smoke tests for rst_code_example_pipeline.

Run with:
    python -m unittest discover -s tests/
or (from the project root):
    python -m unittest rst_code_example_pipeline.tests.test_smoke
"""
import unittest
from unittest.mock import patch

import rst_code_example_pipeline


class TestPackageMetadata(unittest.TestCase):
    def test_title(self) -> None:
        self.assertEqual(rst_code_example_pipeline.__title__,
                         'rst_code_example_pipeline')

    def test_version(self) -> None:
        self.assertRegex(rst_code_example_pipeline.__version__,
                         r'^\d+\.\d+\.\d+$')


class TestModuleImports(unittest.TestCase):
    """Each module must be importable without side-effects."""

    def test_import_colors(self) -> None:
        from rst_code_example_pipeline import colors  # noqa: F401

    def test_import_fmt_utils(self) -> None:
        from rst_code_example_pipeline import fmt_utils  # noqa: F401

    def test_import_checks(self) -> None:
        from rst_code_example_pipeline import checks  # noqa: F401

    def test_import_blocks(self) -> None:
        from rst_code_example_pipeline import blocks  # noqa: F401

    def test_import_toolchain_info(self) -> None:
        from rst_code_example_pipeline import toolchain_info  # noqa: F401

    def test_import_toolchain_setup(self) -> None:
        from rst_code_example_pipeline import toolchain_setup  # noqa: F401

    def test_import_check_code_block(self) -> None:
        from rst_code_example_pipeline import check_code_block  # noqa: F401

    def test_import_extract_projects(self) -> None:
        from rst_code_example_pipeline import extract_projects  # noqa: F401

    def test_import_check_projects(self) -> None:
        from rst_code_example_pipeline import check_projects  # noqa: F401


class TestEntryPoints(unittest.TestCase):
    """Entry-point main() functions must accept --help (exit 0)."""

    def _assert_help_exits_zero(self, entry: str) -> None:
        from importlib import import_module
        mod = import_module(f'rst_code_example_pipeline.cli.{entry}')
        with patch('sys.argv', [entry, '--help']):
            with self.assertRaises(SystemExit) as ctx:
                mod.main()
        self.assertEqual(ctx.exception.code, 0)

    def test_check_block_help(self) -> None:
        self._assert_help_exits_zero('check_block')

    def test_extract_help(self) -> None:
        self._assert_help_exits_zero('extract')

    def test_check_help(self) -> None:
        self._assert_help_exits_zero('check')


if __name__ == '__main__':
    unittest.main()
