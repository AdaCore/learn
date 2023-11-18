#! /usr/bin/env python3

"""
This program will extract every Ada code block in an Ada source file, and try
to compile and execute them.
The default behavior is to:
- Split the block with ``gnatchop``
- If the user indicated that the example should be ran (more on that later):
   a. Run gnatmake on the unit named 'main' if there are several, or on the
      first and only one if there is only one
   b. Run the resulting program and check the return code
- Else:
   a. Run gcc on every Ada file
Users can annotate their code blocks so that some behavior is adopted, using
the ``:class:`` option for code blocks. The interest is that this will be
usable in the generated HTML too.
Here are the available classes for annotation:
- ``ada-nocheck``: Specifies that the code block should not be checked at all
- ``nosyntax-check``: Specifies that the syntax of the code block should not
  be checked.
- ``ada-syntax-only``: Specifies that only the syntax of the code block should
  be checked, not the semantics.
- ``ada-expect-compile-error``: Specifies that a compilation error is expected.
- ``ada-run``: Specifies that the code should be ran after it is compiled.
- ``ada-run-expect-failure``: Specifies that the code should be ran, and that a
  runtime error is expected.
"""

import argparse
import os
import colors as C

import code_projects.fmt_utils as fmt_utils

EXTRACTED_PROJECTS_JSON = "extracted_projects.json"

if __name__ == "__main__":
    CALL_SCRIPTS = True
    RUN_PROJECT_EXTRACTION = True
    RUN_PROJECT_CHECK = True

    test_error = False

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rst_files', type=str, nargs="+",
                        help="The rst file from which to extract doc")
    parser.add_argument('--build-dir', '-B', type=str, default="build",
                        help='Dir in which to build code')

    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show more information')

    parser.add_argument('--keep_files', '-k', action='store_true',
                        help='Keep files generated in the test')
    parser.add_argument('--code-block', '-b', type=str, default=0)
    parser.add_argument('--all-diagnostics', '-A', action='store_true')
    parser.add_argument('--code-block-at', type=int, default=0)
    parser.add_argument('--max-columns', type=int, default=0)

    args = parser.parse_args()

    # Remove the build dir, but only if the user didn't ask for a specific
    # subset of code_blocks
    if (os.path.exists(args.build_dir) and not args.code_block
        and not args.keep_files):
        shutil.rmtree(args.build_dir)

    extracted_projects_json = None
    if args.build_dir:
        extracted_projects_json = \
            os.path.abspath(args.build_dir) + "/" + \
            EXTRACTED_PROJECTS_JSON

    if CALL_SCRIPTS:
        PATH_CODE_PROJECTS = \
            os.path.dirname(os.path.realpath(__file__)) + \
            "/../py_modules/code_projects"

        if RUN_PROJECT_EXTRACTION:
            cmd_extract_projects = PATH_CODE_PROJECTS + \
                "/extract_projects.py"

            if args.build_dir:
                cmd_extract_projects += " --build-dir " + \
                    os.path.abspath(args.build_dir)
            if args.verbose:
                cmd_extract_projects += " --verbose"
            if args.code_block:
                cmd_extract_projects += " --code-block " + \
                    str(args.code_block)
            if args.all_diagnostics:
                cmd_extract_projects += " --all-diagnostics"
            if args.code_block_at:
                cmd_extract_projects += " --code-block-at " + \
                    str(args.code_block_at)
            if args.max_columns:
                cmd_extract_projects += " --max-columns " + \
                    str(args.max_columns)

            if extracted_projects_json:
                cmd_extract_projects += " --extracted_projects " + \
                    extracted_projects_json

            if len(args.rst_files) > 0:
                cmd_extract_projects += " " + " ".join(args.rst_files)

            ret_value = os.system(cmd_extract_projects)
            test_error = ret_value != 0

        if RUN_PROJECT_CHECK:
            cmd_check_projects = PATH_CODE_PROJECTS + \
                "/check_projects.py"

            if args.build_dir:
                cmd_check_projects += " --build-dir " + \
                    os.path.abspath(args.build_dir)
            if args.verbose:
                cmd_check_projects += " --verbose"
            if args.all_diagnostics:
                cmd_check_projects += " --all-diagnostics"
            if args.max_columns:
                cmd_check_projects += " --max-columns " + \
                    str(args.max_columns)
            if extracted_projects_json:
                cmd_check_projects += " --extracted_projects " + \
                    extracted_projects_json

            ret_value = os.system(cmd_check_projects)
            test_error = test_error or ret_value != 0
    else:
        # NOT IMPLEMENTED!
        exit(1)

    # Remove the build dir, but only if the user didn't ask for a specific
    # subset of code_blocks
    if (os.path.exists(args.build_dir) and not args.keep_files):
        shutil.rmtree(args.build_dir)

    if test_error:
        fmt_utils.simple_error("TEST ERROR")
        exit(1)
    elif args.verbose:
        fmt_utils.simple_success("TEST SUCCESS")
