"""
Unit tests for rst_code_example_pipeline.resource.

Covers:
- Resource constructor: basename stored, content=None → empty, content=[] → empty,
  single-element list, multi-element list joined with newline
- append() adds a line; empty resource then append
- content property always returns str
- Adversarial: append empty string; append line with embedded newline
"""
import pytest

from rst_code_example_pipeline.resource import Resource


# ---------------------------------------------------------------------------
# T-resource-01: constructor
# ---------------------------------------------------------------------------

class TestResourceConstructor:
    def test_basename_stored(self):
        r = Resource("foo.adb")
        assert r.basename == "foo.adb"

    def test_content_none_is_empty(self):
        r = Resource("f.adb", content=None)
        assert r.content == ""

    def test_content_default_is_empty(self):
        r = Resource("f.adb")
        assert r.content == ""

    def test_content_empty_list_is_empty(self):
        r = Resource("f.ads", content=[])
        assert r.content == ""

    def test_content_single_element(self):
        r = Resource("f.adb", content=["line one"])
        assert r.content == "line one"

    def test_content_two_elements_joined_with_newline(self):
        r = Resource("f.adb", content=["a", "b"])
        assert r.content == "a\nb"

    def test_content_multi_element(self):
        r = Resource("f.adb", content=["a", "b", "c"])
        assert r.content == "a\nb\nc"

    def test_content_property_is_str(self):
        r = Resource("f.adb", content=["hello"])
        assert isinstance(r.content, str)

    def test_content_none_property_is_str(self):
        r = Resource("f.adb", content=None)
        assert isinstance(r.content, str)


# ---------------------------------------------------------------------------
# T-resource-02: append()
# ---------------------------------------------------------------------------

class TestResourceAppend:
    def test_append_to_empty(self):
        r = Resource("f.adb")
        r.append("first line")
        assert r.content == "first line"

    def test_append_adds_line(self):
        r = Resource("f.adb", content=["existing"])
        r.append("new line")
        assert r.content == "existing\nnew line"

    def test_multiple_appends(self):
        r = Resource("f.adb")
        r.append("a")
        r.append("b")
        r.append("c")
        assert r.content == "a\nb\nc"

    def test_append_empty_string(self):
        r = Resource("f.adb", content=["line"])
        r.append("")
        # Join adds a newline between the two elements
        assert r.content == "line\n"

    def test_content_is_str_after_append(self):
        r = Resource("f.adb")
        r.append("x")
        assert isinstance(r.content, str)


# ---------------------------------------------------------------------------
# T-resource-03: Adversarial
# ---------------------------------------------------------------------------

class TestResourceAdversarial:
    def test_append_line_with_embedded_newline(self):
        """A line with an embedded newline is stored as a single element.
        The content join must use \\n between list elements, not within them,
        so the embedded newline is preserved literally."""
        r = Resource("f.adb", content=["a"])
        r.append("b\nc")
        # The list is ["a", "b\nc"]; joined by "\n" → "a\nb\nc"
        assert r.content == "a\nb\nc"

    def test_initial_content_with_embedded_newlines(self):
        """If content list elements themselves contain newlines, join still
        inserts exactly one \\n between each element."""
        r = Resource("f.adb", content=["x\ny", "z"])
        assert r.content == "x\ny\nz"

    def test_basename_with_path_separators(self):
        """basename is stored verbatim even if it contains slashes."""
        r = Resource("dir/file.adb")
        assert r.basename == "dir/file.adb"

    def test_large_content_list(self):
        lines = [str(i) for i in range(1000)]
        r = Resource("big.adb", content=lines)
        assert r.content == "\n".join(lines)

    def test_content_never_none(self):
        """content property must return a str, never None."""
        r = Resource("f.adb", content=None)
        assert r.content is not None
        assert isinstance(r.content, str)
