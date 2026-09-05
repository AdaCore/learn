"""
End-to-end tests for the command-line entry points.

Every other test in this suite calls the package's functions directly.  These
run the installed commands -- extract-code, check-code and check-block -- as
real processes over a small course directory, and look at what a script
driving them can see: the exit status, and the message that explains it.  That
is the contract the package README sets out under "Exit status", and it is
what a build gates on; nothing else in the suite goes near it.

Covers:
- a course whose one example builds and runs: extract-code and check-code both
  succeed, and what the example printed is there in the run log afterwards
- the same course with the example broken: check-code fails, and says which
  name the compiler could not resolve
- check-block over a single extracted example: success for one that builds,
  failure for one that does not, and failure -- with a message rather than a
  crash -- for a block info file that is missing, and for one that is present
  and unusable
- check-code over a build directory holding a block info file it has to drop:
  one that cannot be read, and one that names no project.  Each fails the run
  rather than reporting success over an example nothing looked at, and an
  unreadable one among several does not cost the others their check
- extract-code over a course whose block names no project: the run fails and
  no block info file is written at all, which is why a block naming no project
  is only reachable from a file written or edited by hand
- extract-code over a course whose block record was damaged since the last run:
  the record is rebuilt, a warning names it as rebuilt, the run still succeeds,
  and the example is still checked afterwards
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

from rst_code_example_pipeline import blocks
from rst_code_example_pipeline import constants
from rst_code_example_pipeline import toolchain_info


# A complete Ada example that announces itself when it runs.  The course
# below asks for a run, so a check that reports success has to have built the
# example, executed it, and recorded what it printed -- rather than merely not
# failing, which is what a command that checked nothing at all also does.
RUN_OUTPUT = "the example ran"

WORKING_ADA_BODY = """\
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line ("{}");
end Main;""".format(RUN_OUTPUT)

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
        ".. code:: ada project={} main=main.adb run_button\n"
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
    written = sorted((cwd / "build").rglob("*.json"))
    assert len(written) == 1, \
        "expected the extraction step to write exactly one block info " \
        "file, got {}".format([str(path) for path in written])
    return str(written[0])


def _write_course_of_several_blocks(directory, project: str,
                                    outputs: list[str]) -> str:
    """Write a course of several examples, each announcing itself with its
    own line, and return its name relative to the directory.

    Each example gets a project of its own, so that one of them being
    unreadable cannot be said to have taken its neighbors down with it merely
    by sharing a directory.  Distinct output then makes each block's run log
    identifiable, which is what lets a test say which examples were checked.
    """
    blocks_rst = []
    for number, output in enumerate(outputs, start=1):
        body = WORKING_ADA_BODY.replace(RUN_OUTPUT, output)
        indented = "\n".join("   " + line for line in body.splitlines())
        blocks_rst.append(
            ".. code:: ada project={}{} main=main.adb run_button\n"
            "\n"
            "{}\n"
            "\n"
            "Explanatory paragraph.\n".format(project, number, indented))
    (directory / "course.rst").write_text("\n".join(blocks_rst))
    return "course.rst"


def _the_extracted_blocks(cwd) -> list:
    """Every block info file the extraction step wrote, in a stable order."""
    return sorted((cwd / "build").rglob(constants.BLOCK_INFO_FILENAME))


def _block_info_files_written(cwd) -> list:
    """Every block info file below the build directory, or none if the
    extraction step did not get as far as making one."""
    build = cwd / "build"
    return _the_extracted_blocks(cwd) if build.is_dir() else []


def _a_block_record_naming_no_project(directory) -> str:
    """Write a well-formed block record that names no project.

    The extraction step refuses to write one -- it stops the whole run on a
    code block with no project name before writing anything -- so this state
    only exists in a file written or edited by hand.  It is produced through
    the package's own writer so that the record is right in every respect
    except the one under test, and lands under the name the check looks for
    without that name being restated here.
    """
    if not toolchain_info.DEFAULT_VERSION:
        toolchain_info.init_toolchain_info()

    versions = toolchain_info.DEFAULT_VERSION
    block = blocks.CodeBlock(
        rst_file="course.rst",
        line_start=1,
        line_end=5,
        text="procedure Main is begin null; end Main;",
        language="ada",
        project=None,
        main_file=None,
        gnat_version=["default", versions["gnat"]],
        gnatprove_version=["default", versions["gnatprove"]],
        gprbuild_version=["default", versions["gprbuild"]],
        compiler_switches=[],
        classes=["ada-nocheck"],
        manual_chop=False,
        buttons=["no"],
    )
    directory.mkdir(parents=True, exist_ok=True)
    written = str(directory / constants.BLOCK_INFO_FILENAME)
    block.to_json_file(written)
    return written


def _the_run_log(cwd) -> str:
    """What the example printed when it was run, of which there is one.

    A run writes its output beside the example rather than to the command's
    own output, so reading it back is the only way to tell a course that
    really ran something from one that reported success over nothing.
    """
    written = sorted((cwd / "build").rglob("run.log"))
    assert len(written) == 1, \
        "expected the check to write exactly one run log, got {}".format(
            [str(path) for path in written])
    return written[0].read_text()


# ---------------------------------------------------------------------------
# A course whose examples all build
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestCourseThatChecksOut:
    def test_extract_and_check_both_succeed(self, tmp_path):
        """A course whose one example builds and runs must be extracted and
        checked without either command reporting a failure.

        The status alone cannot tell success apart from having checked
        nothing, which the package README warns is possible, so the output the
        example printed is asserted as well: it can only be there if the block
        was extracted, built and executed.
        """
        extracted = _extract(tmp_path, "CliCourseGood", WORKING_ADA_BODY)
        assert extracted.returncode == 0, \
            "extracting a well-formed course must succeed: {}".format(
                extracted.stdout)

        checked = _run("check-code", "--build-dir", "build", cwd=tmp_path)
        assert checked.returncode == 0, \
            "checking a course whose example builds must succeed: {}".format(
                checked.stdout)

        assert RUN_OUTPUT in _the_run_log(tmp_path), \
            "a course reported as checked must have run its example, and the "\
            "run log is where what it printed ends up"


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

    def test_an_unusable_block_info_file_fails_with_a_message(self, tmp_path):
        """A block info file that is there but cannot be turned into a block
        must be reported the same way a missing one is.

        This is the case a file damaged after it was written falls into --
        truncated, edited, half-copied.  It used to leave the command as a
        traceback: the status was 1 all the same, but only because that is
        what Python gives an uncaught exception, and nothing in the output
        told the reader which file was at fault or why.
        """
        unusable = tmp_path / constants.BLOCK_INFO_FILENAME
        unusable.write_text("{ this is not a block record")

        result = _run("check-block", str(unusable), cwd=tmp_path)

        assert result.returncode == 1, \
            "a block info file that cannot be loaded must count as a failure"
        assert str(unusable) in result.stdout, \
            "the message must name the file that could not be read: " \
            "{}".format(result.stdout)
        assert "Traceback" not in result.stderr, \
            "the file must be reported, not crashed on: {}".format(
                result.stderr)


# ---------------------------------------------------------------------------
# A block the check dropped instead of checking
# ---------------------------------------------------------------------------

class TestABlockTheCheckNeverLookedAt:
    """check-code over a build directory holding a block info file it drops.

    Each of the two ways in prints an ERROR line and moves on to the next
    file, so neither block reaches the check and neither can report an error
    from there.  What is asserted here is the status of the command, because
    that is what a build gates on -- and both of these have printed their
    ERROR line while the command still exited 0, which is a run reporting
    success over an example nothing looked at.

    Both files are made here rather than extracted.  One stands for a record
    damaged after the extraction step wrote it; the other for a record
    written or edited by hand, since the extraction step refuses to write a
    block that names no project.
    """

    def test_an_unreadable_block_info_file_fails_the_run(self, tmp_path):
        """A build directory whose one block info file cannot be read must
        fail the run."""
        block_dir = tmp_path / "build" / "projects" / "Damaged" / "hash1"
        block_dir.mkdir(parents=True)
        unreadable = block_dir / constants.BLOCK_INFO_FILENAME
        unreadable.write_text("{ this is not a block record")

        result = _run("check-code", "--build-dir", "build", cwd=tmp_path)

        assert result.returncode == 1, \
            "a block info file that could not be read means an example was " \
            "never checked, and the run must say so: {}".format(result.stdout)
        assert str(unreadable) in result.stdout, \
            "the run must name the file it could not read: {}".format(
                result.stdout)
        assert "Traceback" not in result.stderr, \
            "the file must be reported, not crashed on: {}".format(
                result.stderr)

    def test_a_block_naming_no_project_fails_the_run(self, tmp_path):
        """A build directory whose one block info file names no project must
        fail the run, for the same reason: that block was never checked."""
        written = _a_block_record_naming_no_project(
            tmp_path / "build" / "projects" / "NoProject" / "hash1")

        result = _run("check-code", "--build-dir", "build", cwd=tmp_path)

        assert result.returncode == 1, \
            "a block that names no project is a block that was not checked, " \
            "and the run must say so: {}".format(result.stdout)
        assert written in result.stdout, \
            "the run must name the file whose block it dropped: {}".format(
                result.stdout)

    def test_an_empty_build_directory_still_succeeds(self, tmp_path):
        """A build directory with nothing in it must not fail the run.

        The control for the two tests above: without it they would go on
        passing if check-code had simply started failing for everything.
        """
        (tmp_path / "build").mkdir()

        result = _run("check-code", "--build-dir", "build", cwd=tmp_path)

        assert result.returncode == 0, \
            "a build directory holding no blocks has nothing to report: " \
            "{}".format(result.stdout)


@pytest.mark.toolchain
class TestOneBadBlockAmongSeveral:
    """A course whose block info files are not all readable.

    The one property that decides whether reporting an unreadable file
    instead of raising on it was an improvement: the exception it replaced
    left the command while the block info files were still being gathered, so
    not one example in the course was checked, whatever else was wrong with
    it.
    """

    OUTPUTS = ["the first example ran",
               "the second example ran",
               "the third example ran"]

    def test_the_other_examples_are_still_checked(self, tmp_path):
        """One unreadable block info file must fail the run and must not cost
        the other examples in the course their check.

        The run logs are what shows they were checked: an example's output
        can only reach one by being extracted, built and executed.
        """
        rst_file = _write_course_of_several_blocks(
            tmp_path, "CliCourseMixed", self.OUTPUTS)
        extracted = _run("extract-code", "--build-dir", "build", rst_file,
                         cwd=tmp_path)
        assert extracted.returncode == 0, \
            "the course must extract cleanly, or the check that follows is " \
            "not failing on the damaged file: {}".format(extracted.stdout)

        written = _the_extracted_blocks(tmp_path)
        assert len(written) == len(self.OUTPUTS), \
            "expected one block info file per example, got {}".format(
                [str(path) for path in written])

        damaged = written[0]
        damaged_output = [output for output in self.OUTPUTS
                          if output in damaged.read_text()]
        assert len(damaged_output) == 1, \
            "the file about to be damaged must belong to exactly one of the " \
            "examples, got {}".format(damaged_output)
        damaged.write_text("{ this is not a block record")

        checked = _run("check-code", "--build-dir", "build", cwd=tmp_path)

        assert checked.returncode == 1, \
            "the damaged file means an example was never checked, and the " \
            "run must say so: {}".format(checked.stdout)

        ran = "\n".join(path.read_text()
                        for path in (tmp_path / "build").rglob("run.log"))

        for output in self.OUTPUTS:
            if output == damaged_output[0]:
                assert output not in ran, \
                    "the example whose block info file was damaged cannot " \
                    "have been run: {}".format(ran)
            else:
                assert output in ran, \
                    "an example whose block info file was untouched must " \
                    "still have been checked and run: {} is missing from " \
                    "{}".format(output, ran)


# ---------------------------------------------------------------------------
# A course the extraction step refuses
# ---------------------------------------------------------------------------

class TestACourseWhoseBlockNamesNoProject:
    def test_nothing_is_extracted_and_the_run_fails(self, tmp_path):
        """A course with a code block that names no project must fail the
        extraction, and must leave no block info file behind.

        That is what makes the check's "block has no project" arm reachable
        only from a file written or edited by hand: the extraction step stops
        the whole run on such a block before it writes anything, so no file
        it produced can carry one.  The good block is written first so that a
        step which wrote as it went would be caught leaving the first one on
        disk.
        """
        indented = "\n".join("   " + line
                             for line in WORKING_ADA_BODY.splitlines())
        (tmp_path / "course.rst").write_text(
            ".. code:: ada project=CliCourseNamed main=main.adb run_button\n"
            "\n"
            "{}\n"
            "\n"
            "Explanatory paragraph.\n"
            "\n"
            ".. code:: ada main=main.adb run_button\n"
            "\n"
            "{}\n"
            "\n"
            "Another paragraph.\n".format(indented, indented))

        result = _run("extract-code", "--build-dir", "build", "course.rst",
                      cwd=tmp_path)

        assert result.returncode == 1, \
            "a code block with no project name must fail the extraction: " \
            "{}".format(result.stdout)
        assert _block_info_files_written(tmp_path) == [], \
            "the extraction step must write no block info file at all when " \
            "it refuses a course: {}".format(
                [str(path) for path in _block_info_files_written(tmp_path)])


# ---------------------------------------------------------------------------
# A course whose block record was damaged between runs
# ---------------------------------------------------------------------------

@pytest.mark.toolchain
class TestACourseWhoseBlockRecordWasDamaged:
    """extract-code finding a record it wrote earlier and cannot read now.

    A build directory is reused between runs, so a record damaged by an
    interrupted run survives into the next one.  Extraction rewrites it and
    carries on, which is the right outcome and used to be a traceback -- and
    because the outcome is a success, the message is the only thing that says
    the file was ever damaged.

    Asserted through the commands rather than in process, because what makes
    the repair honest is the pair of statuses: the extraction succeeds, and
    the example it repaired is then really checked.
    """

    DAMAGED_RECORD = "{ this is not a block record"

    def test_the_record_is_rebuilt_and_the_example_is_still_checked(
            self, tmp_path):
        """A damaged block record must be rebuilt with a warning naming it,
        the extraction must still succeed, and the example must still be
        checked afterwards.

        The last clause is the one the warning promises and the one most
        likely to rot: a repair that printed the line and left the record
        unusable would satisfy the status and the message and still leave the
        example unchecked.  The run log is what settles it -- the output below
        can only get there by the example being built and executed.
        """
        assert _extract(tmp_path, "CliCourseRebuilt",
                        WORKING_ADA_BODY).returncode == 0, \
            "the course must extract cleanly first, or there is no record to " \
            "damage"

        written = _the_extracted_blocks(tmp_path)
        assert len(written) == 1, \
            "expected one block record after the first extraction, got " \
            "{}".format([str(path) for path in written])
        record = written[0]
        record.write_text(self.DAMAGED_RECORD)

        again = _run("extract-code", "--build-dir", "build", "course.rst",
                     cwd=tmp_path)

        assert again.returncode == 0, \
            "rebuilding a damaged record is a recovery, so the extraction " \
            "must still succeed: {}".format(again.stdout)
        assert "WARNING" in again.stdout, \
            "a rebuilt record must be announced as a warning: {}".format(
                again.stdout)
        # The repair runs from inside the project directory, so the record is
        # named relative to it.  Derived from the real path rather than
        # written out here.
        named_as = "{}/{}".format(record.parent.name, record.name)
        assert named_as in again.stdout, \
            "the warning must name the record it rebuilt: {}".format(
                again.stdout)
        assert "course.rst" in again.stdout, \
            "the warning must say which block it is about, or the record it " \
            "names cannot be located from the message alone: {}".format(
                again.stdout)
        assert "extracted and the run was not cut short" in again.stdout, \
            "the warning must say the run was not cut short: {}".format(
                again.stdout)
        assert "Traceback" not in again.stderr, \
            "the record must be rebuilt, not crashed on: {}".format(
                again.stderr)

        assert record.read_text() != self.DAMAGED_RECORD, \
            "the damaged record must have been rewritten, not merely reported"

        checked = _run("check-code", "--build-dir", "build", cwd=tmp_path)
        assert checked.returncode == 0, \
            "the example the warning says is still checked must check out: " \
            "{}".format(checked.stdout)
        assert RUN_OUTPUT in _the_run_log(tmp_path), \
            "the example must really have been built and run after its " \
            "record was rebuilt"


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
