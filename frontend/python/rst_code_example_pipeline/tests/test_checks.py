"""
Unit tests for rst_code_example_pipeline.checks.

Covers:
- CodeCheck construction and defaults
- BlockCheck.__init__ stores fields; checks dict initially empty
- BlockCheck.add_check() accumulates CodeCheck entries
- BlockCheck.to_json_file() + from_json_file() round-trip
- BlockCheck.from_json_file() with nonexistent file → None
- BlockCheck.from_json_file() with explicit filename
- Adversarial: overwrite, empty JSON {}, TypeError on bad args
"""
import json
import os
import time

import pytest

from rst_code_example_pipeline.checks import BlockCheck, CodeCheck


# ---------------------------------------------------------------------------
# T-checks-01: CodeCheck defaults
# ---------------------------------------------------------------------------

class TestCodeCheckDefaults:
    def test_default_version_is_none(self):
        c = CodeCheck()
        assert c.version is None

    def test_default_status_ok_is_none(self):
        c = CodeCheck()
        assert c.status_ok is None

    def test_default_logfile_is_none(self):
        c = CodeCheck()
        assert c.logfile is None

    def test_default_cmdline_is_none(self):
        c = CodeCheck()
        assert c.cmdline is None

    def test_default_timestamp_is_recent_float(self):
        before = time.time()
        c = CodeCheck()
        after = time.time()
        assert isinstance(c.timestamp, float)
        assert before <= c.timestamp <= after

    def test_explicit_timestamp(self):
        c = CodeCheck(timestamp=1234567890.0)
        assert c.timestamp == 1234567890.0

    def test_all_fields_set(self):
        c = CodeCheck(timestamp=1.0, version="v1.2", status_ok=True,
                      logfile="out.log", cmdline="gcc main.c")
        assert c.timestamp == 1.0
        assert c.version == "v1.2"
        assert c.status_ok is True
        assert c.logfile == "out.log"
        assert c.cmdline == "gcc main.c"


# ---------------------------------------------------------------------------
# T-checks-02: BlockCheck construction
# ---------------------------------------------------------------------------

class TestBlockCheckInit:
    def test_stores_text_hash(self):
        bc = BlockCheck(text_hash="abc", text_hash_short="a")
        assert bc.text_hash == "abc"

    def test_stores_text_hash_short(self):
        bc = BlockCheck(text_hash="abc", text_hash_short="a")
        assert bc.text_hash_short == "a"

    def test_checks_initially_empty(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        assert bc.checks == {}

    def test_checks_empty_even_when_none_passed(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s", checks=None)
        assert bc.checks == {}

    def test_status_ok_default_none(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        assert bc.status_ok is None

    def test_timestamp_recent(self):
        before = time.time()
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        after = time.time()
        assert before <= bc.timestamp <= after

    def test_explicit_timestamp(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s", timestamp=999.0)
        assert bc.timestamp == 999.0


# ---------------------------------------------------------------------------
# T-checks-03: add_check()
# ---------------------------------------------------------------------------

class TestBlockCheckAddCheck:
    def test_add_single_check(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        cc = CodeCheck(status_ok=True)
        bc.add_check("syntax", cc)
        assert "syntax" in bc.checks
        assert bc.checks["syntax"] is cc

    def test_add_multiple_checks(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        bc.add_check("syntax", CodeCheck(status_ok=True))
        bc.add_check("compile", CodeCheck(status_ok=False))
        assert len(bc.checks) == 2
        assert "syntax" in bc.checks
        assert "compile" in bc.checks

    def test_overwrite_check(self):
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        cc1 = CodeCheck(status_ok=True)
        cc2 = CodeCheck(status_ok=False)
        bc.add_check("run", cc1)
        bc.add_check("run", cc2)
        assert bc.checks["run"] is cc2


# ---------------------------------------------------------------------------
# T-checks-04: to_json_file / from_json_file round-trip
# ---------------------------------------------------------------------------

class TestBlockCheckJsonRoundTrip:
    def test_round_trip_top_level_fields(self, tmp_path):
        bc = BlockCheck(
            text_hash="deadbeef",
            text_hash_short="dead",
            timestamp=1000.0,
            status_ok=True,
        )
        f = str(tmp_path / "block_checks.json")
        bc.to_json_file(f)
        bc2 = BlockCheck.from_json_file(f)
        assert bc2 is not None
        assert bc2.text_hash == "deadbeef"
        assert bc2.text_hash_short == "dead"
        assert bc2.timestamp == 1000.0
        assert bc2.status_ok is True

    def test_round_trip_checks_dict_not_persisted(self, tmp_path):
        """Known limitation: BlockCheck.__init__ always initialises self.checks
        to an empty dict (ignoring the 'checks' keyword argument).  Therefore
        from_json_file() — which calls BlockCheck(**json_data) — also loses any
        nested CodeCheck entries that were written to JSON.  This is a design
        limitation of the current implementation and is documented here rather
        than hidden."""
        bc = BlockCheck(text_hash="h", text_hash_short="s")
        cc = CodeCheck(timestamp=1.0, version="v1", status_ok=True,
                       logfile="x.log", cmdline="cmd")
        bc.add_check("syntax", cc)
        # Verify the check is present before saving
        assert "syntax" in bc.checks

        f = str(tmp_path / "bc.json")
        bc.to_json_file(f)

        # After reload, the checks dict is empty because __init__ ignores
        # the 'checks' kwarg and resets self.checks = dict().
        bc2 = BlockCheck.from_json_file(f)
        assert bc2 is not None
        assert bc2.checks == {}

    def test_explicit_filename(self, tmp_path):
        bc = BlockCheck(text_hash="abc", text_hash_short="a")
        f = str(tmp_path / "custom.json")
        bc.to_json_file(f)
        bc2 = BlockCheck.from_json_file(f)
        assert bc2 is not None
        assert bc2.text_hash == "abc"

    def test_default_filename(self, tmp_path, monkeypatch):
        """to_json_file() and from_json_file() with default filename work when
        cwd is set to tmp_path."""
        monkeypatch.chdir(tmp_path)
        bc = BlockCheck(text_hash="xyz", text_hash_short="x")
        bc.to_json_file()
        assert os.path.isfile("block_checks.json")
        bc2 = BlockCheck.from_json_file()
        assert bc2 is not None
        assert bc2.text_hash == "xyz"


# ---------------------------------------------------------------------------
# T-checks-05: from_json_file() with nonexistent file
# ---------------------------------------------------------------------------

class TestBlockCheckFromJsonMissing:
    def test_nonexistent_file_returns_none(self, tmp_path):
        f = str(tmp_path / "does_not_exist.json")
        assert BlockCheck.from_json_file(f) is None

    def test_nonexistent_default_returns_none(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        assert BlockCheck.from_json_file() is None


# ---------------------------------------------------------------------------
# T-checks-06: Adversarial
# ---------------------------------------------------------------------------

class TestBlockCheckAdversarial:
    def test_overwrite_existing_file(self, tmp_path):
        f = str(tmp_path / "bc.json")
        bc1 = BlockCheck(text_hash="first", text_hash_short="f")
        bc1.to_json_file(f)
        bc2 = BlockCheck(text_hash="second", text_hash_short="s")
        bc2.to_json_file(f)
        bc_loaded = BlockCheck.from_json_file(f)
        assert bc_loaded is not None
        assert bc_loaded.text_hash == "second"

    def test_empty_json_raises_type_error(self, tmp_path):
        """from_json_file() with '{}' should raise TypeError because __init__
        requires text_hash and text_hash_short."""
        f = tmp_path / "empty.json"
        f.write_text("{}")
        with pytest.raises(TypeError):
            BlockCheck.from_json_file(str(f))

    def test_from_json_file_none_argument_uses_default(self, tmp_path, monkeypatch):
        """Passing None explicitly is equivalent to omitting the argument."""
        monkeypatch.chdir(tmp_path)
        assert BlockCheck.from_json_file(None) is None
