"""
Unit tests for rst_code_example_pipeline.toolchain_info.

Covers:
- init_toolchain_info() populates DEFAULT_VERSION, TOOLCHAINS, TOOLCHAIN_PATH
- get_toolchain_default_version() for gnat, gnatprove, gprbuild
- Re-initialisation idempotency
- get_toolchain_default_version() for unknown tool raises KeyError
- State isolation: each test that mutates module-level dicts resets them

NOTE: These tests require the Ada toolchain .ini file to be present
"""
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

    def test_toolchains_values_are_lists(self):
        info.init_toolchain_info()
        for tool in ("gnat", "gnatprove", "gprbuild"):
            assert isinstance(info.TOOLCHAINS[tool], list), \
                f"TOOLCHAINS[{tool!r}] must be a list"

    def test_toolchains_gnat_contains_known_versions(self):
        info.init_toolchain_info()
        # At least the three installed versions must appear in the list
        for ver in ("12.2.0-1", "14.2.0-1", "15.1.0-2"):
            assert ver in info.TOOLCHAINS["gnat"], \
                f"Expected gnat version {ver!r} in TOOLCHAINS['gnat']"

    def test_toolchains_gnatprove_contains_known_versions(self):
        info.init_toolchain_info()
        for ver in ("12.1.0-1", "14.1.0-1", "15.1.0-1"):
            assert ver in info.TOOLCHAINS["gnatprove"], \
                f"Expected gnatprove version {ver!r} in TOOLCHAINS['gnatprove']"

    def test_toolchains_gprbuild_contains_known_versions(self):
        info.init_toolchain_info()
        for ver in ("22.0.0-1", "24.0.0-2", "25.0.0-1"):
            assert ver in info.TOOLCHAINS["gprbuild"], \
                f"Expected gprbuild version {ver!r} in TOOLCHAINS['gprbuild']"

    def test_toolchain_path_values_nonempty_strings(self):
        info.init_toolchain_info()
        for key in ("root", "selected", "default"):
            val = info.TOOLCHAIN_PATH[key]
            assert isinstance(val, str) and val, \
                f"TOOLCHAIN_PATH[{key!r}] must be a non-empty string"


# ---------------------------------------------------------------------------
# T-toolchain_info-02: get_toolchain_default_version() auto-initialises
# ---------------------------------------------------------------------------

class TestGetToolchainDefaultVersion:
    def test_gnat_returns_string(self):
        # Dicts are empty; the function must initialise and return a value
        result = info.get_toolchain_default_version("gnat")
        assert isinstance(result, str) and result

    def test_gnatprove_returns_string(self):
        result = info.get_toolchain_default_version("gnatprove")
        assert isinstance(result, str) and result

    def test_gprbuild_returns_string(self):
        result = info.get_toolchain_default_version("gprbuild")
        assert isinstance(result, str) and result

    def test_gnat_version_is_known_installed_version(self):
        result = info.get_toolchain_default_version("gnat")
        known = {"12.2.0-1", "14.2.0-1", "15.1.0-2"}
        assert result in known, \
            f"Default gnat version {result!r} not in known installed set {known}"

    def test_gnatprove_version_is_known_installed_version(self):
        result = info.get_toolchain_default_version("gnatprove")
        known = {"12.1.0-1", "14.1.0-1", "15.1.0-1"}
        assert result in known, \
            f"Default gnatprove version {result!r} not in known installed set {known}"

    def test_gprbuild_version_is_known_installed_version(self):
        result = info.get_toolchain_default_version("gprbuild")
        known = {"22.0.0-1", "24.0.0-2", "25.0.0-1"}
        assert result in known, \
            f"Default gprbuild version {result!r} not in known installed set {known}"

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
# T-toolchain_info-03: re-initialisation idempotency
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
