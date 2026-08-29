"""
End-to-end tests for the command-line entry points.

Every other test in this suite calls the package's functions directly.  These
run the installed commands -- extract-code, check-code and check-block -- as
real processes over a small course directory, and look at what a script
driving them can see: the exit status, and the message that explains it.  That
is the contract the package README sets out under "Exit status", and it is
what a build gates on; nothing else in the suite goes near it.

Covers:
- a course whose one example builds: extract-code and check-code both succeed
- the same course with the example broken: check-code fails, and says which
  name the compiler could not resolve
- check-block over a single extracted example: success for one that builds,
  failure for one that does not, and failure -- with a message rather than a
  crash -- for a block info file that cannot be read
- the command lines the README says are rejected: naming neither a build
  directory nor a project list fails, and an unknown switch is rejected
  outright with the distinct status argument parsing uses

NOTE: a command that gets as far as checking an example runs the Ada
toolchain over it, so those tests carry the `toolchain` marker.  The tests
that stop at argument handling, and the one that stops at an unreadable block
info file, never reach a compiler and carry no marker.

The commands under test are the console scripts the package installs, so they
must be on PATH -- which they are wherever the package is installed, the same
condition that lets the rest of the suite import it.
"""
import subprocess

import pytest


# A complete Ada example that announces itself, so that a course which is
# supposed to check out really does something rather than merely not failing.
WORKING_ADA_BODY = """\
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line ("the example ran");
end Main;"""

# A name nothing declares, so the build has to fail on it and the compiler has
# to say so -- which is how a failing run is told apart from one that failed
# for some unrelated reason.
MISSING_NAME = "No_Such_Procedure"

# Syntactically valid, so it chops and passes the syntax check, but it calls
# something that does not exist.
BROKEN_ADA_BODY = """\
procedure Main is
begin
   {};
end Main;""".format(MISSING_NAME)


def _write_course(directory, project: str, body: str):
    """Write a one-block RST file the way a course author would, and return
    its name relative to the directory holding it."""
    indented = "\n".join("   " + line for line in body.splitlines())
    (directory / "course.rst").write_text(
        ".. code:: ada project={} main=main.adb compile_button\n"
        "\n"
        "{}\n"
        "\n"
        "Explanatory paragraph.\n".format(project, indented))
    return "course.rst"


def _run(command: str, *arguments: str, cwd) -> subprocess.CompletedProcess:
    """Run one of the installed commands as a real process."""
    return subprocess.run([command, *arguments], cwd=str(cwd),
                          capture_output=True, text=True)


def _extract(cwd, project: str, body: str) -> subprocess.CompletedProcess:
    """Extract a one-block course into a build directory below ``cwd``."""
    rst_file = _write_course(cwd, project, body)
    return _run("extract-code", "--build-dir", "build", rst_file, cwd=cwd)


def _the_extracted_block(cwd) -> str:
    """The block info file the extraction step wrote, of which there is one.

    The extraction step keeps a staging copy of the sources alongside the
    per-block directory, and only the latter holds a block info file.
    """
    written = sorted((cwd / "build").rglob("block_info.json"))
    assert len(written) == 1, \
        "expected the extraction step to write exactly one block info " \
        "file, got {}".format([str(path) for path in written])
    return str(written[0])


# ---------------------------------------------------------------------------
# A course whose examples all build
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCourseThatChecksOut:
    def test_extract_and_check_both_succeed(self, tmp_path):
        """A course whose one example builds must be extracted and checked
        without either command reporting a failure."""
        extracted = _extract(tmp_path, "CliCourseGood", WORKING_ADA_BODY)
        assert extracted.returncode == 0, \
            "extracting a well-formed course must succeed: {}".format(
                extracted.stdout)

        checked = _run("check-code", "--build-dir", "build", cwd=tmp_path)
        assert checked.returncode == 0, \
            "checking a course whose example builds must succeed: {}".format(
                checked.stdout)


# ---------------------------------------------------------------------------
# A course with one example that does not build
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCourseWithABrokenExample:
    def test_check_code_fails_and_says_why(self, tmp_path):
        """A course with one example that does not build must be extracted
        without complaint -- the block is well-formed, it just does not
        compile -- and then fail the check.

        The message is asserted as well as the status, so that a run which
        fails because the course fixture itself is wrong cannot be mistaken
        for the failure the test is about.
        """
        extracted = _extract(tmp_path, "CliCourseBroken", BROKEN_ADA_BODY)
        assert extracted.returncode == 0, \
            "the course must extract cleanly, or the check that follows is " \
            "not failing on the example: {}".format(extracted.stdout)

        checked = _run("check-code", "--build-dir", "build", cwd=tmp_path)
        assert checked.returncode == 1, \
            "checking a course with an example that does not build must " \
            "fail: {}".format(checked.stdout)
        assert MISSING_NAME in checked.stdout, \
            "the failure must name what the compiler could not resolve: " \
            "{}".format(checked.stdout)


# ---------------------------------------------------------------------------
# A single extracted example
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCheckingASingleBlock:
    def test_a_block_that_builds_succeeds(self, tmp_path):
        """check-block on one previously extracted example that builds must
        succeed."""
        assert _extract(tmp_path, "CliBlockGood",
                        WORKING_ADA_BODY).returncode == 0
        checked = _run("check-block", "--force", _the_extracted_block(tmp_path),
                       cwd=tmp_path)
        assert checked.returncode == 0, \
            "checking one example that builds must succeed: {}".format(
                checked.stdout)

    def test_a_block_that_does_not_build_fails(self, tmp_path):
        """check-block on one previously extracted example that does not
        build must fail, and name what the compiler could not resolve."""
        assert _extract(tmp_path, "CliBlockBroken",
                        BROKEN_ADA_BODY).returncode == 0
        checked = _run("check-block", "--force", _the_extracted_block(tmp_path),
                       cwd=tmp_path)
        assert checked.returncode == 1, \
            "checking one example that does not build must fail: {}".format(
                checked.stdout)
        assert MISSING_NAME in checked.stdout, \
            "the failure must name what the compiler could not resolve: " \
            "{}".format(checked.stdout)


class TestBlockInfoThatCannotBeRead:
    def test_a_missing_block_info_file_fails_with_a_message(self, tmp_path):
        """A block info file that cannot be loaded counts as a failure, so a
        script gating on the status is not told the example checked out when
        nothing was checked at all.

        The command must say which file it could not read; a crash would also
        end in a failing status and would tell the reader nothing.
        """
        missing = str(tmp_path / "no_such_block.json")
        result = _run("check-block", missing, cwd=tmp_path)

        assert result.returncode == 1, \
            "a block info file that cannot be loaded must count as a failure"
        assert missing in result.stdout, \
            "the message must name the file that could not be read: " \
            "{}".format(result.stdout)
        assert "Traceback" not in result.stderr, \
            "the file must be reported, not crashed on: {}".format(
                result.stderr)


# ---------------------------------------------------------------------------
# Command lines that are rejected before any example is looked at
# ---------------------------------------------------------------------------

class TestRejectedCommandLines:
    def test_check_code_needs_somewhere_to_look(self, tmp_path):
        """check-code with neither a build directory nor a project list has
        nothing to check and must fail rather than report success over
        nothing."""
        result = _run("check-code", cwd=tmp_path)
        assert result.returncode == 1, \
            "check-code must fail when it is told nowhere to look: " \
            "{}".format(result.stdout)

    def test_extract_code_needs_somewhere_to_write(self, tmp_path):
        """extract-code with neither a build directory nor a project list has
        nowhere to put what it extracts and must fail."""
        rst_file = _write_course(tmp_path, "CliNoDestination", WORKING_ADA_BODY)
        result = _run("extract-code", rst_file, cwd=tmp_path)
        assert result.returncode == 1, \
            "extract-code must fail when it is told nowhere to write: " \
            "{}".format(result.stdout)

    @pytest.mark.parametrize("command",
                             ["extract-code", "check-code", "check-block"])
    def test_an_unknown_switch_is_rejected_outright(self, command, tmp_path):
        """A command line that cannot be parsed is rejected with a status of
        its own, so that a script can tell a mistyped invocation apart from an
        example that failed its check."""
        result = _run(command, "--no-such-switch", cwd=tmp_path)
        assert result.returncode == 2, \
            "{} must reject an unknown switch with the argument-parsing " \
            "status: {}".format(command, result.stderr)
