#! /usr/bin/env python3

"""
This program will try to compile and execute code blocks.
The default behavior is to:
- If the user indicated that the example should be ran (more on that later):
   a. Run gnatmake on the unit named 'main' if there are several, or on the
      first and only one if there is only one
   b. Run the resulting program and check the return code
- Else:
   a. Run gcc on every Ada file
"""

import argparse
import os
import subprocess as S
from os import path as P
import glob
import re

import blocks
import fmt_utils

verbose = False
all_diagnostics = False
max_columns = 0 # no check for max. columns


class Diag(object):
    def __init__(self, file, line, col, msg):
        self.file = file
        self.line = line
        self.col = col
        self.msg = msg

    def __repr__(self):
        return "{}:{}:{}: {}".format(self.file, self.line, self.col, self.msg)


def check_block(block : blocks.CodeBlock,
                json_file : str,
                verbose : bool = verbose,
                all_diagnostics : bool = all_diagnostics,
                max_columns : int = max_columns):

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

    def extract_diagnostics(lines):
        diags = []
        r = re.compile("(.+?):(\d+):(\d+): (.+)")
        for l in lines:
            m = r.match(l)
            if m:
                f, l, c, t = m.groups()
                diags.append(Diag(f, int(l), int(c), t))
        return diags

    def remove_string(some_text, rem):
        return re.sub(".*" + rem + ".*\n?","", some_text)

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
        if project_filename is not None:

            try:
                run("gprclean", "-P", project_filename)

                run("gnatprove", "-P", project_filename, "--clean")
            except S.CalledProcessError as e:
                out = str(e.output.decode("utf-8"))

        if language == "c":
            try:
                cmd = ["rm", "-f"] + glob.glob('*.o') + glob.glob('*.gch')
                if main_file is not None:
                    cmd.append(P.splitext(main_file)[0])
                out = run(*cmd)
            except S.CalledProcessError as e:
                print_error(loc, "Failed to clean-up example")
                print(e.output)
                has_error = True

    project_block_dir = os.path.dirname(json_file)
    os.chdir(project_block_dir)

    if block.no_check:
        if verbose:
            print("Skipping code block {}".format(loc))
        return has_error

    if verbose:
        print(fmt_utils.header("Checking code block {}".format(loc)))

    # Syntax check
    if 'nosyntax-check' not in block.classes:
        for source_file in block.source_files:

            try:
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
                    has_error = True
            except S.CalledProcessError:
                print_error(loc, "Failed to syntax check example")
                has_error = True

    if block.syntax_only:
        cleanup_project(block.language,
                        block.project_filename,
                        block.project_main_file)
        return has_error

    compile_error = False
    prove_error = False
    is_prove_error_class = False

    if block.compile_it:

        if block.language == "ada":

            try:
                run("gprclean", "-P", block.project_filename)
                out = run("gprbuild", "-q", "-P", block.project_filename)
            except S.CalledProcessError as e:
                if 'ada-expect-compile-error' in block.classes:
                    compile_error = True
                else:
                    print_error(loc, "Failed to compile example")
                    print(e.output)
                    has_error = True
                out = str(e.output.decode("utf-8"))

            out = remove_string(out, "using project")
            with open("build.log", u"w") as logfile:
                logfile.write(out)

        elif block.language == "c":
            try:
                cmd = ["gcc", "-o",
                        P.splitext(block.project_main_file)[0]] + glob.glob('*.c')
                out = run(*cmd)
            except S.CalledProcessError as e:
                if 'c-expect-compile-error' in block.classes:
                    compile_error = True
                else:
                    print_error(loc, "Failed to compile example")
                    print(e.output)
                    has_error = True
                out = str(e.output.decode("utf-8"))
            with open("build.log", u"w") as logfile:
                logfile.write(out)

        if not compile_error and not has_error and block.run_it:
            if block.language == "ada":
                try:
                    out = run("./{}".format(P.splitext(block.project_main_file)[0]))

                    if 'ada-run-expect-failure' in block.classes:
                        print_error(
                            loc, "Running of example should have failed"
                        )
                        has_error = True

                except S.CalledProcessError as e:
                    if 'ada-run-expect-failure' in block.classes:
                        if verbose:
                            print("Running of example expectedly failed")
                    else:
                        print_error(loc, "Running of example failed")
                        has_error = True

                    out = str(e.output.decode("utf-8"))

                with open("run.log", u"w") as logfile:
                    logfile.write(out)

            elif block.language == "c":
                try:
                    out = run("./{}".format(P.splitext(block.project_main_file)[0]))

                    if 'c-run-expect-failure' in block.classes:
                        print_error(
                            loc, "Running of example should have failed"
                        )
                        has_error = True

                except S.CalledProcessError as e:
                    if 'c-run-expect-failure' in block.classes:
                        if verbose:
                            print("Running of example expectedly failed")
                    else:
                        print_error(loc, "Running of example failed")
                        has_error = True
                    out = str(e.output.decode("utf-8"))

                with open("run.log", u"w") as logfile:
                    logfile.write(out)

    if False:

        for source_file in block.source_files:
            if block.language == "ada":
                try:
                    out = run("gcc", "-c", "-gnatc", "-gnatyg0-s",
                                source_file)
                except S.CalledProcessError as e:
                    if 'ada-expect-compile-error' in block.classes:
                        compile_error = True
                    else:
                        print_error(loc, "Failed to compile example")
                        has_error = True
                    out = str(e.output.decode("utf-8"))

                with open("compile.log", u"w+") as logfile:
                    logfile.write(out)

            elif block.language == "c":
                try:
                    out = run("gcc", "-c", source_file)
                except S.CalledProcessError as e:
                    if 'c-expect-compile-error' in block.classes:
                        compile_error = True
                    else:
                        print_error(loc, "Failed to compile example")
                        has_error = True
                    out = str(e.output.decode("utf-8"))

                with open("compile.log", u"w+") as logfile:
                    logfile.write(out)

    if block.prove_it:

        if block.language == "ada":

            is_prove_error_class = any(c in ['ada-expect-prove-error',
                                'ada-expect-compile-error',
                                'ada-run-expect-failure']
                        for c in block.classes)
            extra_args = []

            if 'prove_flow' in block.buttons:
                extra_args = ["--mode=flow"]
            elif 'prove_flow_report_all' in block.buttons:
                extra_args = ["--mode=flow", "--report=all"]
            elif 'prove_report_all' in block.buttons:
                extra_args = ["--report=all"]

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
                    has_error = True
                out = str(e.output.decode("utf-8"))

            out = remove_string(out, "Summary logged in")
            with open("prove.log", u"w") as logfile:
                logfile.write(out)
        else:
            print_error(loc, "Wrong language selected for prove button")
            print(e.output)
            has_error = True

    if len(block.buttons) == 0:
        print_error(loc, "Expected at least 'no_button' indicator, got none!")
        has_error = True

    if 'ada-expect-compile-error' in block.classes:
        if not any(b in ['compile', 'run'] for b in block.buttons):
            print_error(loc, "Expected compile or run button, got none!")
            has_error = True
        if not compile_error:
            print_error(loc, "Expected compile error, got none!")
            has_error = True

    if 'ada-expect-prove-error' in block.classes:
        if not block.prove_it:
            print_error(loc, "Expected prove button, got none!")
            has_error = True

    if block.prove_it:
        if is_prove_error_class and not prove_error:
            print_error(loc, "Expected prove error, got none!")
            has_error = True

    if (any (c in ['ada-run-expect-failure','ada-norun'] for
                c in block.classes)
        and not 'run' in block.buttons):
        print_error(loc, "Expected run button, got none!")
        has_error = True

    if not has_error and verbose:
        fmt_utils.simple_success("SUCCESS")

    cleanup_project(block.language,
                    block.project_filename,
                    block.project_main_file)

    if all_diagnostics:
        print_diags()

    return has_error


def check_code_block_json(json_file):

    b = blocks.CodeBlock.from_json_file(json_file)

    has_error = check_block(b, json_file, verbose,
                            all_diagnostics, max_columns)

    return has_error


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('json_files', type=str, nargs="+",
                        help="The JSON file for each code block")
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show more information')
    parser.add_argument('--all-diagnostics', '-A', action='store_true')
    parser.add_argument('--max-columns', type=int, default=0)

    args = parser.parse_args()

    verbose = args.verbose
    all_diagnostics = args.all_diagnostics
    max_columns = args.max_columns

    has_error = False

    for f in args.json_files:
        check_status = check_code_block_json(f)
        has_error |= check_status

    exit(has_error)
