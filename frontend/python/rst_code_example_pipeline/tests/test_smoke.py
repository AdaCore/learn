"""
Smoke tests for rst_code_example_pipeline.

Covers:
- package metadata: the declared version is the one the distribution was
  installed under, and the declared title is the name the package is imported
  under
- every module of the package is importable without side effects
- every command-line entry point accepts --help and exits successfully
"""
from importlib import import_module, metadata
import sys

import pytest

import rst_code_example_pipeline


def _distribution_name() -> str:
    """The name the package is installed under.

    Read back from the installed metadata rather than written down here: the
    distribution is named with hyphens where the import package uses
    underscores, and only the metadata knows which distribution provides
    which import package.
    """
    provided_by = metadata.packages_distributions()[
        rst_code_example_pipeline.__name__]
    assert len(provided_by) == 1, \
        "expected exactly one distribution to provide the package, got " \
        "{}".format(provided_by)
    return provided_by[0]


class TestPackageMetadata:
    def test_version_matches_the_installed_distribution(self):
        """The version the package declares must be the one it was installed
        under.

        The version is written down twice -- in the package and in the
        packaging metadata -- and nothing ties the two together, so a release
        that bumps one and forgets the other would otherwise pass unnoticed
        and ship a package that misreports its own version.
        """
        installed = metadata.version(_distribution_name())
        assert rst_code_example_pipeline.__version__ == installed, \
            "the package declares version {} but was installed as {}".format(
                rst_code_example_pipeline.__version__, installed)

    def test_title_is_the_name_the_package_is_imported_under(self):
        """The declared title must be the name the package is imported under.

        It is not the distribution name, which is spelled with hyphens: the
        title has tracked the import package since before the package was
        distributed at all.  Checking it against the name the import machinery
        supplies catches a package that was renamed without the title
        following it.
        """
        assert rst_code_example_pipeline.__title__ == \
            rst_code_example_pipeline.__name__, \
            "the package declares the title {} but is imported as {}".format(
                rst_code_example_pipeline.__title__,
                rst_code_example_pipeline.__name__)


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
