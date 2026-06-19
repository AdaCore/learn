"""
Unit tests for rst_code_example_pipeline.toolchain_setup.

Covers:
- reset_toolchain() when no symlinks exist → no exception
- reset_toolchain() when symlinks exist → symlinks removed
- set_toolchain(block) with gnat_version=["default", …] → no symlink created
- set_toolchain(block) with gnat_version=["selected", "12.2.0-1"] → symlink created
- set_toolchain() followed by reset_toolchain() → symlinks removed
- Adversarial: set_toolchain() called twice without reset → must not fail
- State isolation: teardown_function resets toolchain after every test

NOTE: Requires the Ada toolchain installed at /opt/ada.
The tests redirect symlink creation into a tmp_path-based directory to avoid
mutating /opt/ada/selected in the real environment.
"""
import os

import pytest

import rst_code_example_pipeline.toolchain_info as info
import rst_code_example_pipeline.toolchain_setup as setup
from rst_code_example_pipeline.blocks import CodeBlock


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

def _make_block(gnat_version: list[str],
                gnatprove_version: list[str] | None = None,
                gprbuild_version: list[str] | None = None) -> CodeBlock:
    """Build a minimal CodeBlock with the given toolchain version selectors."""
    # Ensure toolchain_info is initialised so default version strings exist
    if not info.DEFAULT_VERSION:
        info.init_toolchain_info()
    gnatprove_version = gnatprove_version or ["default", info.DEFAULT_VERSION["gnatprove"]]
    gprbuild_version = gprbuild_version or ["default", info.DEFAULT_VERSION["gprbuild"]]
    return CodeBlock(
        rst_file="test.rst",
        line_start=1,
        line_end=5,
        text="procedure Main is begin null; end Main;",
        language="ada",
        project="TestProject",
        main_file=None,
        gnat_version=gnat_version,
        gnatprove_version=gnatprove_version,
        gprbuild_version=gprbuild_version,
        compiler_switches=["-gnata"],
        classes=[],
        manual_chop=False,
        buttons=["no"],
    )


@pytest.fixture()
def isolated_toolchain_path(tmp_path, monkeypatch):
    """
    Redirect TOOLCHAIN_PATH so symlinks are created in tmp_path instead of
    the real /opt/ada/selected directory.  Also creates stub target directories
    matching the installed toolchain versions so os.symlink targets exist.
    """
    # Ensure toolchain_info is initialised
    if not info.DEFAULT_VERSION:
        info.init_toolchain_info()

    root = tmp_path / "ada"
    selected = root / "selected"
    default_dir = root / "default"
    selected.mkdir(parents=True)
    default_dir.mkdir(parents=True)

    # Create stub version directories for the known installed versions
    for tool, versions in [
        ("gnat", ["12.2.0-1", "14.2.0-1", "15.1.0-2"]),
        ("gnatprove", ["12.1.0-1", "14.1.0-1", "15.1.0-1"]),
        ("gprbuild", ["22.0.0-1", "24.0.0-2", "25.0.0-1"]),
    ]:
        for ver in versions:
            tool_dir = root / tool / ver
            tool_dir.mkdir(parents=True, exist_ok=True)

    # Patch the module-level dict values
    monkeypatch.setitem(info.TOOLCHAIN_PATH, "root", str(root))
    monkeypatch.setitem(info.TOOLCHAIN_PATH, "selected", str(selected))
    monkeypatch.setitem(info.TOOLCHAIN_PATH, "default", str(default_dir))

    yield {
        "root": str(root),
        "selected": str(selected),
        "default": str(default_dir),
    }

    # Teardown: call reset_toolchain() so no symlinks survive across tests
    try:
        setup.reset_toolchain()
    except Exception:
        pass


# ---------------------------------------------------------------------------
# T-toolchain_setup-01: reset_toolchain() without prior symlinks
# ---------------------------------------------------------------------------

class TestResetToolchainNoSymlinks:
    def test_no_exception_when_symlinks_absent(self, isolated_toolchain_path):
        # No symlinks have been created; reset must silently succeed
        setup.reset_toolchain()  # must not raise

    def test_selected_dir_still_exists_after_reset(self, isolated_toolchain_path):
        setup.reset_toolchain()
        assert os.path.isdir(isolated_toolchain_path["selected"])


# ---------------------------------------------------------------------------
# T-toolchain_setup-02: reset_toolchain() removes existing symlinks
# ---------------------------------------------------------------------------

class TestResetToolchainRemovesSymlinks:
    def test_symlinks_removed_after_reset(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        root = isolated_toolchain_path["root"]

        # Manually create symlinks to simulate a prior set_toolchain call
        for tool in ("gnat", "gnatprove", "gprbuild"):
            link = os.path.join(selected, tool)
            target_ver = list(os.listdir(os.path.join(root, tool)))[0]
            target = os.path.join(root, tool, target_ver)
            os.symlink(target, link)

        # Verify they were created
        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert os.path.exists(os.path.join(selected, tool))

        setup.reset_toolchain()

        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert not os.path.exists(os.path.join(selected, tool)), \
                f"Symlink for {tool!r} was not removed by reset_toolchain()"

    def test_reset_idempotent_after_removal(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        root = isolated_toolchain_path["root"]

        for tool in ("gnat",):
            link = os.path.join(selected, tool)
            target_ver = list(os.listdir(os.path.join(root, tool)))[0]
            target = os.path.join(root, tool, target_ver)
            os.symlink(target, link)

        setup.reset_toolchain()
        # Second reset must not raise even though symlinks are already gone
        setup.reset_toolchain()


# ---------------------------------------------------------------------------
# T-toolchain_setup-03: set_toolchain() with all "default" versions
# ---------------------------------------------------------------------------

class TestSetToolchainDefaultVersion:
    def test_no_symlink_created_for_default_gnat(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(gnat_version=["default", info.DEFAULT_VERSION["gnat"]])
        setup.set_toolchain(block)
        assert not os.path.exists(os.path.join(selected, "gnat")), \
            "No symlink should be created when gnat_version is 'default'"

    def test_no_symlink_created_for_any_default(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(
            gnat_version=["default", info.DEFAULT_VERSION["gnat"]],
            gnatprove_version=["default", info.DEFAULT_VERSION["gnatprove"]],
            gprbuild_version=["default", info.DEFAULT_VERSION["gprbuild"]],
        )
        setup.set_toolchain(block)
        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert not os.path.exists(os.path.join(selected, tool)), \
                f"No symlink should be created for tool {tool!r} in default mode"


# ---------------------------------------------------------------------------
# T-toolchain_setup-04: set_toolchain() with "selected" gnat version
# ---------------------------------------------------------------------------

class TestSetToolchainSelectedVersion:
    def test_gnat_symlink_created(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(gnat_version=["selected", "12.2.0-1"])
        setup.set_toolchain(block)
        link_path = os.path.join(selected, "gnat")
        assert os.path.exists(link_path), \
            "Symlink selected/gnat must exist after set_toolchain() with 'selected'"

    def test_gnat_symlink_points_to_correct_version(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        root = isolated_toolchain_path["root"]
        block = _make_block(gnat_version=["selected", "14.2.0-1"])
        setup.set_toolchain(block)
        link_path = os.path.join(selected, "gnat")
        expected_target = os.path.join(root, "gnat", "14.2.0-1")
        assert os.readlink(link_path) == expected_target, \
            f"Symlink must point to {expected_target!r}"

    def test_no_gnatprove_symlink_when_only_gnat_selected(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(gnat_version=["selected", "12.2.0-1"])
        setup.set_toolchain(block)
        assert not os.path.exists(os.path.join(selected, "gnatprove")), \
            "gnatprove symlink must not be created when only gnat is 'selected'"

    def test_all_three_selected(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(
            gnat_version=["selected", "12.2.0-1"],
            gnatprove_version=["selected", "12.1.0-1"],
            gprbuild_version=["selected", "22.0.0-1"],
        )
        setup.set_toolchain(block)
        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert os.path.exists(os.path.join(selected, tool)), \
                f"Symlink for {tool!r} must be created when version is 'selected'"


# ---------------------------------------------------------------------------
# T-toolchain_setup-05: set_toolchain() followed by reset_toolchain()
# ---------------------------------------------------------------------------

class TestSetThenReset:
    def test_symlinks_removed_after_reset(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(gnat_version=["selected", "15.1.0-2"])
        setup.set_toolchain(block)
        assert os.path.exists(os.path.join(selected, "gnat"))
        setup.reset_toolchain()
        assert not os.path.exists(os.path.join(selected, "gnat")), \
            "Symlink must be gone after reset_toolchain()"

    def test_set_then_reset_is_idempotent(self, isolated_toolchain_path):
        block = _make_block(gnat_version=["selected", "14.2.0-1"])
        setup.set_toolchain(block)
        setup.reset_toolchain()
        # A second reset must not raise
        setup.reset_toolchain()


# ---------------------------------------------------------------------------
# T-toolchain_setup-06: adversarial — double set_toolchain() without reset
# ---------------------------------------------------------------------------

class TestAdversarialDoubleSet:
    def test_double_set_does_not_fail(self, isolated_toolchain_path):
        """set_toolchain() calls reset_toolchain() internally, so calling it
        twice without an explicit reset in between must not raise."""
        block = _make_block(gnat_version=["selected", "12.2.0-1"])
        setup.set_toolchain(block)
        # Second call must not raise (reset is called inside set_toolchain)
        setup.set_toolchain(block)

    def test_after_double_set_symlink_still_present(self, isolated_toolchain_path):
        selected = isolated_toolchain_path["selected"]
        block = _make_block(gnat_version=["selected", "12.2.0-1"])
        setup.set_toolchain(block)
        setup.set_toolchain(block)
        assert os.path.exists(os.path.join(selected, "gnat")), \
            "Symlink must still be present after two consecutive set_toolchain() calls"
