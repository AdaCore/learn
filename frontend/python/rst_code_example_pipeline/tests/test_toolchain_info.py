"""
Unit tests for rst_code_example_pipeline.toolchain_info.

Covers:
- init_toolchain_info() populates DEFAULT_VERSION, TOOLCHAINS, TOOLCHAIN_PATH
- every declared version has the release shape the provisioning script expects
- get_toolchain_default_version() for gnat, gnatprove, gprbuild
- the default version of each tool is one of the versions declared for it
- Re-initialization idempotency
- get_toolchain_default_version() for unknown tool raises KeyError
- State isolation: each test that mutates module-level dicts resets them

The assertions below are deliberately invariants rather than snapshots of the
versions currently configured: a toolchain upgrade must not redden this file.

NOTE: These tests require the Ada toolchain .ini file to be present
"""
import re

import pytest

import rst_code_example_pipeline.toolchain_info as info


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def reset_module_state():
    """Reset module-level dicts before and after every test."""
    info.DEFAULT_VERSION.clear()
    info.TOOLCHAINS.clear()
    info.TOOLCHAIN_PATH.clear()
    yield
    info.DEFAULT_VERSION.clear()
    info.TOOLCHAINS.clear()
    info.TOOLCHAIN_PATH.clear()


# ---------------------------------------------------------------------------
# T-toolchain_info-01: init_toolchain_info() populates the module dicts
# ---------------------------------------------------------------------------

class TestInitToolchainInfo:
    def test_default_version_keys_after_init(self):
        info.init_toolchain_info()
        assert set(info.DEFAULT_VERSION.keys()) == {"gnat", "gnatprove", "gprbuild"}

    def test_toolchains_keys_after_init(self):
        info.init_toolchain_info()
        assert set(info.TOOLCHAINS.keys()) == {"gnat", "gnatprove", "gprbuild"}

    def test_toolchain_path_keys_after_init(self):
        info.init_toolchain_info()
        assert set(info.TOOLCHAIN_PATH.keys()) == {"root", "selected", "default"}

    def test_default_version_values_nonempty(self):
        info.init_toolchain_info()
        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert info.DEFAULT_VERSION[tool], \
                f"DEFAULT_VERSION[{tool!r}] must be a non-empty string"

    def test_toolchains_entries_are_release_versions(self):
        """Every declared version must be a non-empty release identifier of the
        form <major>.<minor>.<patch>-<release>.

        That shape is not a matter of taste: the provisioning script builds the
        download URL of each toolchain by interpolating this exact token, so a
        malformed or missing entry produces a download failure far away from
        its cause.  It is also stronger than merely checking the value is a
        list: splitting an empty configuration entry on whitespace yields a
        one-element list holding an empty string, which no other test rejects.
        """
        info.init_toolchain_info()
        for tool in ("gnat", "gnatprove", "gprbuild"):
            versions = info.TOOLCHAINS[tool]
            assert versions, \
                f"TOOLCHAINS[{tool!r}] must declare at least one version"
            for ver in versions:
                assert re.fullmatch(r"\d+\.\d+\.\d+-\d+", ver), \
                    f"TOOLCHAINS[{tool!r}] entry {ver!r} is not a release version"

    def test_toolchain_path_values_nonempty_strings(self):
        info.init_toolchain_info()
        for key in ("root", "selected", "default"):
            val = info.TOOLCHAIN_PATH[key]
            assert isinstance(val, str) and val, \
                f"TOOLCHAIN_PATH[{key!r}] must be a non-empty string"


# ---------------------------------------------------------------------------
# T-toolchain_info-02: get_toolchain_default_version() auto-initializes
# ---------------------------------------------------------------------------

class TestGetToolchainDefaultVersion:
    def test_gnat_returns_string(self):
        # Dicts are empty; the function must initialize and return a value
        result = info.get_toolchain_default_version("gnat")
        assert isinstance(result, str) and result

    def test_gnatprove_returns_string(self):
        result = info.get_toolchain_default_version("gnatprove")
        assert isinstance(result, str) and result

    def test_gprbuild_returns_string(self):
        result = info.get_toolchain_default_version("gprbuild")
        assert isinstance(result, str) and result

    def test_default_version_is_one_of_the_declared_versions(self):
        """The default version of each tool must be one of the versions
        declared as installed for that tool.

        The provisioning script downloads exactly the declared versions and
        then points the default at one of them, so a default that is not in
        the list leaves a dangling symlink where the toolchain is expected.
        """
        for tool in ("gnat", "gnatprove", "gprbuild"):
            result = info.get_toolchain_default_version(tool)
            assert result in info.TOOLCHAINS[tool], \
                f"Default {tool} version {result!r} is not declared as installed: " \
                f"{info.TOOLCHAINS[tool]}"

    def test_auto_init_populates_default_version_dict(self):
        # Before the call the dict is empty (fixture cleared it)
        assert len(info.DEFAULT_VERSION) == 0
        info.get_toolchain_default_version("gnat")
        # After the call the dict must have been populated
        assert len(info.DEFAULT_VERSION) > 0

    def test_unknown_tool_raises_key_error(self):
        # init_toolchain_info() is called internally because dict is empty;
        # the key "unknown_tool" was never set so KeyError must propagate.
        with pytest.raises(KeyError):
            info.get_toolchain_default_version("unknown_tool")


# ---------------------------------------------------------------------------
# T-toolchain_info-03: re-initialization idempotency
# ---------------------------------------------------------------------------

class TestReInitIdempotency:
    def test_second_init_gnat_default_unchanged(self):
        info.init_toolchain_info()
        first = info.DEFAULT_VERSION["gnat"]
        info.init_toolchain_info()
        second = info.DEFAULT_VERSION["gnat"]
        assert first == second

    def test_second_init_toolchain_path_unchanged(self):
        info.init_toolchain_info()
        first = dict(info.TOOLCHAIN_PATH)
        info.init_toolchain_info()
        assert dict(info.TOOLCHAIN_PATH) == first

    def test_second_init_toolchains_unchanged(self):
        info.init_toolchain_info()
        first_gnat = list(info.TOOLCHAINS["gnat"])
        info.init_toolchain_info()
        assert list(info.TOOLCHAINS["gnat"]) == first_gnat

    def test_many_inits_stable(self):
        for _ in range(5):
            info.init_toolchain_info()
        # All keys must still be present
        assert "gnat" in info.DEFAULT_VERSION
        assert "root" in info.TOOLCHAIN_PATH
        assert "gprbuild" in info.TOOLCHAINS


# ---------------------------------------------------------------------------
# T-toolchain_info-04: state isolation — verify the fixture works correctly
# ---------------------------------------------------------------------------

class TestStateIsolation:
    def test_dicts_empty_at_test_start(self):
        # The autouse fixture clears dicts before every test; verify that here.
        assert len(info.DEFAULT_VERSION) == 0
        assert len(info.TOOLCHAINS) == 0
        assert len(info.TOOLCHAIN_PATH) == 0

    def test_manual_mutation_does_not_bleed_across(self):
        info.DEFAULT_VERSION["gnat"] = "fake-version"
        assert info.DEFAULT_VERSION["gnat"] == "fake-version"
        # The fixture teardown clears it; the next test will see an empty dict.
