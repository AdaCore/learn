#! /usr/bin/env python3

"""
Check code blocks that were previously extracted from the ReST sources, one
block_info.json record at a time. What runs for a code block is decided by
what the code block itself declares. Every code block is syntax-checked
unless it declares 'nosyntax-check', and one declaring 'ada-syntax-only'
stops there. A code block that asks to be compiled or to be run is built
(gprbuild for Ada, gcc for C), and the resulting program is run, with its
exit status checked, only after a build that succeeded. A code block that
asks to be proved is proved with gnatprove independently of the build, so a
proof needs no build and does not trigger one. A code block may also declare
that its compilation, its run or its proof is expected to fail; the failure
is then the passing outcome, and its absence is reported. The outcome is
recorded next to the code block as block_checks.json, and a code block that
already carries such a record is skipped unless --force is given.
"""

# The text above is what argparse prints as this command's help
# description. It is deliberately free of ReST markup and of any layout
# worth preserving: the default help formatter re-wraps a description into a
# single filled paragraph, so a list would arrive as a run-on sentence and
# inline literals would arrive with their backquotes intact.

import argparse
import os
import subprocess as S
from os import path as P
import glob
import re

from . import blocks
from . import checks
from . import constants
from . import fmt_utils
from . import toolchain_setup

LOOK_FOR_PREVIOUS_CHECKS = True

verbose: bool = False
all_diagnostics: bool = False
max_columns: int = 0 # no check for max. columns
force_checks: bool = False


class Diag(object):
    def __init__(self,
                 file: str,
                 line: int,
                 col: int,
                 msg: str) -> None:
        self.file: str = file
        self.line: int = line
        self.col: int = col
        self.msg: str = msg

    def __repr__(self) -> str:
        return "{}:{}:{}: {}".format(self.file, self.line, self.col, self.msg)


def check_block(block: blocks.CodeBlock,
                json_file: str,
                verbose: bool = verbose,
                all_diagnostics: bool = all_diagnostics,
                max_columns: int = max_columns,
                force_checks: bool = force_checks) -> bool:
    """Runs the checks a single code block asks for

    A code block declares what is to be done with it, through its buttons
    and its ``:class:`` values, and this function turns that declaration
    into checks. The order below is part of the contract rather than an
    accident of the code, because the later checks depend on the earlier
    ones having run.

    A **syntax check** comes first, over every source file of the code
    block, and it runs for *every* code block -- including one that asks
    for nothing else at all -- unless the code block declares
    ``nosyntax-check``. So the weakest thing that can happen to a code
    block is still that its sources are parsed.

    A code block declared **syntax-only** returns right after that check,
    so it never reaches the build. It is still cleaned up and its result
    still recorded; what it skips is every check below.

    A **build** follows for a code block that asks to be compiled, which
    includes every code block that asks to be run, since asking for a run
    implies asking for a compile.

    The **run** is nested inside that build step, not placed beside it: a
    code block cannot be run without having been built, and a build that
    did not succeed suppresses the run. That holds for a build that failed
    and was reported, and equally for one that failed the way the code
    block said it would -- an expected compile error is still a program
    that was not produced.

    A **proof** is a sibling of the build rather than part of it. A code
    block that asks only to be proved is therefore never built, and one
    that asks for both gets both, independently of each other.

    A check of the code block's **own declarations** runs last, after
    everything that could satisfy them. It has to: what it reports is a
    compile error, a proof error or a run failure that the code block
    declared it expected and that then did not happen, and that is only
    knowable once the checks above have had their turn.

    Args:
        block (blocks.CodeBlock): The code block to check.
        json_file (str): The block info file the code block was read from.
            Only its directory is used, as the place the extracted sources
            and the generated project were written to.
        verbose (bool): Reports each command as it runs, plus toolchain
            versions and paths.
        all_diagnostics (bool): Reports the diagnostics collected over the
            whole check, in addition to those reported per failing check.
        max_columns (int): Maximum source line length the syntax check
            enforces for Ada; zero leaves the length unchecked.
        force_checks (bool): Re-runs the checks for a code block that
            already carries a result from an earlier run, which is
            otherwise reused.

    Returns:
        bool: True if any check failed. Note the polarity: this is an error
        flag, not a success flag, and the callers OR it across code blocks.

    Note:
        The outcome is written next to the code block as
        ``block_checks.json``, and a later run reuses it instead of
        checking again. Only the overall status survives that round trip:
        the per-check entries recorded here are written to the file but are
        dropped when it is read back, so nothing acts on them. They are a
        record for whoever reads the file, not an interface -- the ReST
        widget that renders an example's log files beside it locates them
        by globbing the code block's directory, not by reading their names
        from here.
    """

    def run(*run_args):
        if verbose:
            print("Running \"{}\"".format(" ".join(run_args)))
        try:
            output = S.check_output(run_args, stderr=S.STDOUT).decode("utf-8")
            all_output.extend(output.splitlines())
        except S.CalledProcessError as e:
            all_output.extend(e.output.decode("utf-8").splitlines())
            raise e

        return output

    def set_versions():
        gcc_version = None
        gnat_version = None
        gnat_prove_version = None
        gprbuild_version = None
        try:
            gcc_version = run("gcc", "--version").partition('\n')[0]
            gnat_version = run("gnat", "--version").partition('\n')[0]
            gnat_prove_version = run("gnatprove", "--version").partition('\n')[0]
            gprbuild_version = run("gprbuild", "--version").partition('\n')[0]
        except Exception:
            gcc_version = "<unknown>" if gcc_version is None else gcc_version
            gnat_version = "<unknown>" if gnat_version is None else gnat_version
            gnat_prove_version = "<unknown>" if gnat_prove_version is None else gnat_prove_version
            gprbuild_version = "<unknown>" if gprbuild_version is None else gprbuild_version

        return gcc_version, gnat_version, gnat_prove_version, gprbuild_version

    def extract_diagnostics(lines):
        diags = []
        r = re.compile(r"(.+?):(\d+):(\d+): (.+)")
        for l in lines:
            m = r.match(l)
            if m:
                f, l, c, t = m.groups()
                diags.append(Diag(f, int(l), int(c), t))
        return diags

    def remove_string(some_text, rem):
        return re.sub(r".*" + rem + r".*\n?","", some_text)

    has_error = False
    loc = "at {}:{} (code block hash: {})".format(
        block.rst_file, block.line_start, block.text_hash_short)

    all_output = []

    def print_diags():
        diags = extract_diagnostics(all_output)
        for diag in diags:
            diag.line = diag.line + block.line_start
            diag.file = block.rst_file
            print(diag)

    def print_error(*error_args):
        fmt_utils.error(*error_args)
        print_diags()

    def cleanup_project(language, project_filename, main_file):
        #
        # Clean-up source-code examples after compilation
        #
        if language == "ada":
            if project_filename is not None:

                try:
                    run("gprclean", "-P", project_filename)
                except S.CalledProcessError as e:
                    out = str(e.output.decode("utf-8"))
                    print_error(loc, "Failed to clean-up example")
                    print(out)

                try:
                    run("gnatprove", "-P", project_filename, "--clean")
                except S.CalledProcessError as e:
                    out = str(e.output.decode("utf-8"))
                    print_error(loc,
                                "Failed to clean-up example (gnatprove --clean)")
                    print(out)
        elif language == "c":
            try:
                cmd = ["rm", "-f"] + glob.glob('*.o') + glob.glob('*.gch')
                if main_file is not None:
                    cmd.append(P.splitext(main_file)[0])
                out = run(*cmd)
            except S.CalledProcessError as e:
                print_error(loc, "Failed to clean-up example")
                print(e.output)

    toolchain_setup.set_toolchain(block)

    project_block_dir = os.path.dirname(json_file)
    os.chdir(project_block_dir)

    if block.no_check:
        if verbose:
            print("Skipping code block {}".format(loc))
        return has_error

    if LOOK_FOR_PREVIOUS_CHECKS:
        ref_block_check = None

        try:
            ref_block_check = checks.BlockCheck.from_json_file()
        except Exception:
            pass

        if ref_block_check is not None and not force_checks:
            has_error = not ref_block_check.status_ok
            if verbose:
                print("Code block {} already checked. Skipping...".format(loc))
            if __name__ == '__main__':  # pragma: no cover
                print("WARNING: Code block {} already checked: use '--force' to re-run the check. Skipping...".format(loc))
            if has_error:
                print_error(
                    loc, "Previous check of example has failed"
                )
            return has_error

    if verbose:
        print(fmt_utils.header("Checking code block {}".format(loc)))

    gcc_version, gnat_version, gnat_prove_version, gprbuild_version = set_versions()

    if verbose:
        import shutil

        print("GCC version {}".format(gcc_version))
        print("GNAT version {}".format(gnat_version))
        print("GNATprove version {}".format(gnat_prove_version))
        print("GPRbuild version {}".format(gprbuild_version))

        print("GCC: {}".format(shutil.which("gcc")))
        print("GNAT: {}".format(shutil.which("gnat")))
        print("GNATprove: {}".format(shutil.which("gnatprove")))
        print("GPRbuild: {}".format(shutil.which("gprbuild")))

        print("Specified GNAT version {}".format(block.gnat_version))
        print("Specified GNATprove version {}".format(block.gnatprove_version))
        print("Specified GPRbuild version {}".format(block.gprbuild_version))

    block_check = checks.BlockCheck(text_hash=block.text_hash, text_hash_short=block.text_hash_short)
    block_check.status_ok = True

    # Syntax check
    if constants.CLASS_NOSYNTAX_CHECK not in block.classes:
        check_error = False

        for source_file in block.source_files:

            try:
                out: str = ""
                if block.language == "ada":
                    commands = ["gcc", "-c", "-gnats", "-gnatyg0-s"]
                    if max_columns > 0:
                        commands.append("-gnatyM" + str(max_columns))
                    out = run(*commands +
                                block.compiler_switches +
                                [source_file])
                elif block.language == "c":
                    out = run("gcc", "-c", source_file)

                if out:
                    print_error(loc, "Failed to syntax check example")
                    check_error = True
            except S.CalledProcessError:
                print_error(loc, "Failed to syntax check example")
                check_error = True

        code_check = checks.CodeCheck(version=gcc_version,
                                      status_ok=(not check_error))
        block_check.add_check("SYNTAX", code_check)

        if check_error:
            has_error = True

    if block.syntax_only:
        cleanup_project(block.language,
                        block.project_filename,
                        block.project_main_file)
        block_check.status_ok = not has_error
        block_check.to_json_file()
        return has_error

    compile_error = False
    prove_error = False
    is_prove_error_class = False

    if block.compile_it:
        check_error = False

        if block.language == "ada":
            cmdline = None

            try:
                cmdline = ["gprclean", "-P", block.project_filename]
                run(*cmdline)
            except S.CalledProcessError as e:
                out = str(e.output.decode("utf-8"))
                print_error(loc, "Failed to clean-up example")
                print(out)

            try:
                cmdline = ["gprbuild", "-q", "-P", block.project_filename]
                out = run(*cmdline)

            except S.CalledProcessError as e:
                if constants.CLASS_ADA_EXPECT_COMPILE_ERROR in block.classes:
                    compile_error = True
                else:
                    print_error(loc, "Failed to compile example")
                    print(e.output)
                    check_error = True
                out = str(e.output.decode("utf-8"))

            out = remove_string(out, "using project")
            with open("build.log", u"w") as logfile:
                logfile.write(out)

            code_check = checks.CodeCheck(version=gnat_version,
                                          status_ok=(not check_error),
                                          logfile="build.log",
                                          cmdline=str(cmdline))

            block_check.add_check("BUILD", code_check)

            if check_error:
                has_error = True

        elif block.language == "c":
            cmdline = None
            try:
                sources = glob.glob('*.c')
                if block.project_main_file is not None:
                    cmdline = ["gcc", "-o",
                               P.splitext(block.project_main_file)[0]] + sources
                else:
                    # A compile button asks for a compile and not a link, and
                    # a block that is not also run has no main file resolved
                    # for it -- it may hold no main at all.  Compiling without
                    # linking is what was asked for, and needs no name for an
                    # executable that is not being produced.
                    cmdline = ["gcc", "-c"] + sources
                out = run(*cmdline)
            except S.CalledProcessError as e:
                if constants.CLASS_C_EXPECT_COMPILE_ERROR in block.classes:
                    compile_error = True
                else:
                    print_error(loc, "Failed to compile example")
                    print(e.output)
                    check_error = True
                out = str(e.output.decode("utf-8"))
            with open("build.log", u"w") as logfile:
                logfile.write(out)

            code_check = checks.CodeCheck(version=gcc_version,
                                          status_ok=(not check_error),
                                          logfile="build.log",
                                          cmdline=str(cmdline))

            block_check.add_check("BUILD", code_check)

            if check_error:
                has_error = True

        if not compile_error and not has_error and block.run_it:
            check_error = False
            cmdline = None
            run_attempted = False

            if block.language == "ada":
                run_attempted = True
                try:
                    assert block.project_main_file is not None
                    cmdline = ["./{}".format(P.splitext(block.project_main_file)[0])]
                    out = run(*cmdline)

                    if constants.CLASS_ADA_RUN_EXPECT_FAILURE in block.classes:
                        print_error(
                            loc, "Running of example should have failed"
                        )
                        check_error = True

                except S.CalledProcessError as e:
                    if constants.CLASS_ADA_RUN_EXPECT_FAILURE in block.classes:
                        if verbose:
                            print("Running of example expectedly failed")
                    else:
                        print_error(loc, "Running of example failed")
                        check_error = True

                    out = str(e.output.decode("utf-8"))
                except FileNotFoundError as e:
                    print_error(loc, "Running of example failed: "
                                     "no executable to run")
                    check_error = True
                    out = str(e)

                with open("run.log", u"w") as logfile:
                    logfile.write(out)

            elif block.language == "c":
                run_attempted = True
                try:
                    assert block.project_main_file is not None
                    cmdline = ["./{}".format(P.splitext(block.project_main_file)[0])]
                    out = run(*cmdline)

                    if constants.CLASS_C_RUN_EXPECT_FAILURE in block.classes:
                        print_error(
                            loc, "Running of example should have failed"
                        )
                        check_error = True

                except S.CalledProcessError as e:
                    if constants.CLASS_C_RUN_EXPECT_FAILURE in block.classes:
                        if verbose:
                            print("Running of example expectedly failed")
                    else:
                        print_error(loc, "Running of example failed")
                        check_error = True
                    out = str(e.output.decode("utf-8"))
                except FileNotFoundError as e:
                    print_error(loc, "Running of example failed: "
                                     "no executable to run")
                    check_error = True
                    out = str(e)

                with open("run.log", u"w") as logfile:
                    logfile.write(out)

            # Only a language the checker actually runs gets a RUN phase.
            # Recording one for any other language claimed a successful run
            # of a command that was never built, naming a log file that was
            # never written.
            if run_attempted:
                code_check = checks.CodeCheck(status_ok=(not check_error),
                                              logfile="run.log",
                                              cmdline=str(cmdline))

                block_check.add_check("RUN", code_check)

            if check_error:
                has_error = True

    if block.prove_it:
        check_error = False

        if block.language == "ada":

            is_prove_error_class = any(c in [constants.CLASS_ADA_EXPECT_PROVE_ERROR,
                                constants.CLASS_ADA_EXPECT_COMPILE_ERROR,
                                constants.CLASS_ADA_RUN_EXPECT_FAILURE]
                        for c in block.classes)
            extra_args = []

            if 'prove_flow' in block.buttons \
                or constants.CLASS_ADA_PROVE_FLOW in block.classes:
                extra_args = ["--mode=flow"]
            elif 'prove_flow_report_all' in block.buttons \
                or constants.CLASS_ADA_PROVE_FLOW_REPORT_ALL in block.classes:
                extra_args = ["--mode=flow", "--report=all"]
            elif 'prove_report_all' in block.buttons \
                or constants.CLASS_ADA_PROVE_REPORT_ALL in block.classes:
                extra_args = ["--report=all"]

            # Default switches for GNATprove 14 and above
            line = ["gnatprove", "-P", block.spark_project_filename,
                    "--checks-as-errors=on", "--level=0",
                    "--function-sandboxing=off", "--output=oneline"]
            if block.gnatprove_version[1].startswith("12"):
                line = ["gnatprove", "-P", block.spark_project_filename,
                        "--checks-as-errors", "--level=0",
                        "--no-axiom-guard", "--output=oneline"]

            line.extend(extra_args)

            try:
                out = run(*line)
            except S.CalledProcessError as e:
                if is_prove_error_class:
                    prove_error = True
                else:
                    print_error(loc, "Failed to prove example")
                    print(e.output)
                    check_error = True
                out = str(e.output.decode("utf-8"))

            out = remove_string(out, "Summary logged in")
            with open("prove.log", u"w") as logfile:
                logfile.write(out)

            code_check = checks.CodeCheck(version=gnat_prove_version,
                                          status_ok=(not check_error),
                                          logfile="prove.log",
                                          cmdline=str(line))

            block_check.add_check("PROVE", code_check)

        else:
            print_error(loc, "Wrong language selected for prove button")
            check_error = True

            code_check = checks.CodeCheck(status_ok=(not check_error))
            block_check.add_check("PROVE", code_check)

        if check_error:
            has_error = True


    check_error = False

    if len(block.buttons) == 0:
        print_error(loc, "Expected at least 'no_button' indicator, got none!")
        check_error = True

    if ((block.gnat_version[0] == 'selected' or
         block.gnatprove_version[0] == 'selected' or
         block.gprbuild_version[0] == 'selected') and
        block.buttons != ['no']):
        print_error(loc, "Only 'no_button' is allowed when selecting a specific toolchain!")
        check_error = True

    if constants.CLASS_ADA_EXPECT_COMPILE_ERROR in block.classes:
        if (not (any(b in ['compile', 'run'] for b in block.buttons) or
                 any(c in [constants.CLASS_ADA_COMPILE,
                           constants.CLASS_ADA_RUN]
                     for c in block.classes))):
            print_error(loc, "Expected compile or run button/class, got none!")
            check_error = True
        if not compile_error:
            print_error(loc, "Expected compile error, got none!")
            check_error = True

    if constants.CLASS_ADA_EXPECT_PROVE_ERROR in block.classes:
        if not block.prove_it:
            print_error(loc, "Expected prove button, got none!")
            check_error = True

    if block.prove_it:
        if is_prove_error_class and not prove_error:
            print_error(loc, "Expected prove error, got none!")
            check_error = True

    if (any (c in [constants.CLASS_ADA_RUN_EXPECT_FAILURE,
                   constants.CLASS_ADA_NORUN]
                for c in block.classes)
        and not ('run' in block.buttons or
                 constants.CLASS_ADA_RUN in block.classes)):
        print_error(loc, "Expected run button, got none!")
        check_error = True

    code_check = checks.CodeCheck(status_ok=(not check_error))

    block_check.add_check("BUTTONS", code_check)

    if check_error:
        has_error = True

    if not has_error and verbose:
        fmt_utils.simple_success("SUCCESS")

    cleanup_project(block.language,
                    block.project_filename,
                    block.project_main_file)

    if all_diagnostics:
        print_diags()

    block_check.status_ok = not has_error
    block_check.to_json_file()

    toolchain_setup.reset_toolchain()

    return has_error


def check_code_block_json(json_file: str) -> bool:

    b = blocks.CodeBlock.from_json_file(json_file)

    if b is None:
        print("ERROR: Could not load block info from {}".format(json_file))
        return True

    if not b.active:
        print("WARNING: Block is deactivated. Checking it nevertheless...")

    has_error = check_block(b, json_file, verbose,
                            all_diagnostics, max_columns,
                            force_checks)

    return has_error


if __name__ == "__main__":  # pragma: no cover
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('json_files', type=str, nargs="+",
                        help="The JSON file for each code block")
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show more information')
    parser.add_argument('--all-diagnostics', '-A', action='store_true')
    parser.add_argument('--max-columns', type=int, default=0)
    parser.add_argument('--force', '-f', action='store_true',
                        help="Force checks even if previous check exists.")

    args = parser.parse_args()

    verbose = args.verbose
    all_diagnostics = args.all_diagnostics
    max_columns = args.max_columns
    force_checks = args.force

    has_error = False

    for f in args.json_files:
        check_error = check_code_block_json(f)
        if check_error:
            has_error = True

    exit(has_error)
