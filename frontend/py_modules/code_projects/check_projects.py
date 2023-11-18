#! /usr/bin/env python3

"""
This program will check all the code blocks from all projects.
"""

import argparse
import os
import glob

import blocks
import check_code_block
import fmt_utils

verbose = False
all_diagnostics = False
max_columns = 0 # no check for max. columns

def get_projects(build_dir):
    projects = dict()

    os.chdir(build_dir)
    base_project_dir = "projects"

    json_files_regex = base_project_dir + "/**/block_info.json"
    for json_file in glob.iglob(json_files_regex, recursive=True):
        json_file_path = os.path.abspath(json_file)
        b = blocks.CodeBlock.from_json_file(json_file_path)

        if not b.project in projects:
            projects[b.project] = list()
        projects[b.project].append((b, json_file_path))

    return projects


def check_block(block, json_file):
    has_error = check_code_block.check_block(
        block, json_file, verbose, all_diagnostics, max_columns)

    return has_error


def check_projects(build_dir):

    check_error = False

    work_dir = os.getcwd()

    projects = get_projects(build_dir)

    for project in projects:

        if verbose:
            print(fmt_utils.header("Checking project {}".format(project)))
            print("Number of code blocks: {}".format(len(projects[project])))

        for block, json_file in projects[project]:

            os.chdir(work_dir)  # change to work directory using absolute path

            has_error = check_block(block, json_file)

            if has_error:
                check_error = True

        os.chdir(work_dir)

    return check_error


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', '-B', type=str, default="build",
                        help='Dir in which to build code')
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show more information')
    parser.add_argument('--all-diagnostics', '-A', action='store_true')
    parser.add_argument('--max-columns', type=int, default=0)

    args = parser.parse_args()

    verbose = args.verbose
    all_diagnostics = args.all_diagnostics
    max_columns = args.max_columns

    test_error = check_projects(args.build_dir)

    if test_error:
        fmt_utils.simple_success("TEST ERROR")
        exit(1)
    elif verbose:
        fmt_utils.simple_success("TEST SUCCESS")
