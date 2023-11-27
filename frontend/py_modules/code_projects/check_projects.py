#! /usr/bin/env python3

"""
This program will check all the code blocks from all projects.
"""

import argparse
import os
import glob

import blocks
import check_code_block
import extract_projects
import fmt_utils

verbose = False
all_diagnostics = False
max_columns = 0 # no check for max. columns
force_checks = False


def get_blocks(json_files_regex_list):
    projects = dict()

    for json_regex in json_files_regex_list:
        for json_file in glob.iglob(json_regex, recursive=True):
            json_file_path = os.path.abspath(json_file)
            b = blocks.CodeBlock.from_json_file(json_file_path)

            if not b.project in projects:
                projects[b.project] = list()
            projects[b.project].append((b, json_file_path))

    return projects


def get_projects(build_dir, projects_list_file=None):
    json_files_regex_list = list()

    os.chdir(build_dir)

    if projects_list_file is not None:
        extracted_projects = \
            extract_projects.ProjectsList.from_json_file(projects_list_file)

        if extracted_projects:
            for prj in extracted_projects.projects:
                json_files_regex_list.append(extract_projects.get_project_dir(prj) +
                                            "/**/block_info.json")
        else:
            print("WARNING: no projects found in file: " + projects_list_file)
    else:
        json_files_regex_list.append("./**/block_info.json")

    projects = get_blocks(json_files_regex_list)

    return projects


def check_block(block, json_file):
    has_error = check_code_block.check_block(
        block, json_file, verbose, all_diagnostics, max_columns,
        force_checks)

    return has_error


def check_projects(build_dir, projects_list_file=None):

    check_error = False

    work_dir = os.getcwd()

    projects = get_projects(build_dir, projects_list_file)

    for project in projects:

        if verbose:
            print(fmt_utils.header("Checking project {}".format(project)))
            print("Number of code blocks: {}".format(len(projects[project])))

        for block, json_file in projects[project]:

            if not block.active:
                continue

            os.chdir(work_dir)  # change to work directory using absolute path

            has_error = check_block(block, json_file)

            if has_error:
                check_error = True

        os.chdir(work_dir)

    return check_error


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', '-B', type=str, default=None,
                        help='Dir in which to build code')
    parser.add_argument('--extracted_projects', type=str, default=None,
                        help='JSON file containing list of extracted projects')
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
    extracted_projects = args.extracted_projects

    if args.build_dir is None and \
        extracted_projects is None:
        print("ERROR: at least --extracted_projects or --build-dir should be specified (or both).")
        exit(1)

    if extracted_projects:
        extracted_projects = os.path.abspath(extracted_projects)

    if args.build_dir:
        build_dir = os.path.abspath(args.build_dir)
    else:
        build_dir = os.path.dirname(extracted_projects)
        if build_dir == '': ## Special case: no directory in path
            build_dir = os.getcwd()
        if verbose:
            print("Build directory is set to: " + build_dir)

    test_error = check_projects(build_dir, extracted_projects)

    if test_error:
        fmt_utils.simple_error("TEST ERROR")
        exit(1)
    elif verbose:
        fmt_utils.simple_success("TEST SUCCESS")

