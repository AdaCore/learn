"""
Smoke tests for rst_code_example_pipeline.

Covers:
- package metadata: the declared title and the shape of the declared version
- every module of the package is importable without side effects
- every command-line entry point accepts --help and exits successfully
"""
from importlib import import_module
import re
import sys

import pytest

import rst_code_example_pipeline


class TestPackageMetadata:
    def test_title(self):
        assert rst_code_example_pipeline.__title__ == \
            'rst_code_example_pipeline'

    def test_version(self):
        assert re.match(r'^\d+\.\d+\.\d+$',
                        rst_code_example_pipeline.__version__)


class TestModuleImports:
    """Each module must be importable without side-effects."""

    def test_import_colors(self):
        from rst_code_example_pipeline import colors  # noqa: F401

    def test_import_fmt_utils(self):
        from rst_code_example_pipeline import fmt_utils  # noqa: F401

    def test_import_checks(self):
        from rst_code_example_pipeline import checks  # noqa: F401

    def test_import_blocks(self):
        from rst_code_example_pipeline import blocks  # noqa: F401

    def test_import_toolchain_info(self):
        from rst_code_example_pipeline import toolchain_info  # noqa: F401

    def test_import_toolchain_setup(self):
        from rst_code_example_pipeline import toolchain_setup  # noqa: F401

    def test_import_check_code_block(self):
        from rst_code_example_pipeline import check_code_block  # noqa: F401

    def test_import_extract_projects(self):
        from rst_code_example_pipeline import extract_projects  # noqa: F401

    def test_import_check_projects(self):
        from rst_code_example_pipeline import check_projects  # noqa: F401


class TestEntryPoints:
    """Entry-point main() functions must accept --help (exit 0)."""

    @pytest.mark.parametrize("entry", ["check_block", "extract", "check"])
    def test_help_exits_zero(self, entry, monkeypatch):
        module = import_module("rst_code_example_pipeline.cli.{}".format(entry))
        monkeypatch.setattr(sys, "argv", [entry, "--help"])

        with pytest.raises(SystemExit) as raised:
            module.main()

        assert raised.value.code == 0, \
            "{} --help must exit successfully".format(entry)
