#! /usr/bin/env python3

"""
This program will extract every Ada code block in an Ada source file
The default behavior is to:
- Split the block with ``gnatchop``
"""

from __future__ import annotations

import os
import shutil
import json

from .chop import manual_chop, real_gnatchop

from . import blocks
from . import constants
from . import fmt_utils
from . import toolchain_setup


current_config = blocks.ConfigBlock(
    run_button=False, prove_button=True, accumulate_code=False
)


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


verbose: bool = False
code_block_at: int | None = None

BASE_PROJECT_DIR = "projects"

def get_project_dir(project: str) -> str:
    return BASE_PROJECT_DIR + "/" + project.replace(".", "/")


COMMON_ADC = """
--  pragma Restrictions (No_Specification_of_Aspect => Import);
--  pragma Restrictions (No_Use_Of_Pragma => Import);
--  pragma Restrictions (No_Use_Of_Pragma => Interface);
--  pragma Restrictions (No_Use_Of_Pragma => Linker_Options);
--  pragma Restrictions (No_Dependence => System.Machine_Code);
--  pragma Restrictions (No_Dependence => Machine_Code);
"""

SPARK_ADC = """
pragma Profile(GNAT_Extended_Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);
pragma SPARK_Mode (On);
pragma Warnings (Off, "no Global contract available");
pragma Warnings (Off, "subprogram * has no effect");
pragma Warnings (Off, "file name does not match");
"""

MAIN_GPR="""
project Main is

   --MAIN_PLACEHOLDER--

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-O0");
      --COMPILER_SWITCHES_PLACEHOLDER--
   end Compiler;

   package Builder is
      for Default_Switches ("Ada") use ("-g");
      for Global_Configuration_Pragmas use "{}";
   end Builder;

end Main;
""".format(constants.PROJECT_PRAGMAS_FILENAME)

MAIN_SPARK_GPR="""
project Main_Spark is

   --MAIN_PLACEHOLDER--

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-O0");
      --COMPILER_SWITCHES_PLACEHOLDER--
   end Compiler;

   package Builder is
      for Default_Switches ("Ada") use ("-g");
      for Global_Configuration_Pragmas use "{}";
   end Builder;

end Main_Spark;
""".format(constants.SPARK_PROJECT_PRAGMAS_FILENAME)

def write_project_file(main_file: str | None,
                       compiler_switches: list[str],
                       spark_mode: bool) -> str:
    """Writes the project file for a code block, and its pragmas file

    Both files are written into the current working directory, which the
    caller has already changed to the block's own directory.

    Args:
        main_file (str, optional): The source file holding the main
            procedure, or None to generate a project that names no main.
        compiler_switches (list[str]): Switches added to the ``Compiler``
            package of the generated project.
        spark_mode (bool): Selects the SPARK variants of the project file
            and of the configuration pragmas file.

    Returns:
        str: The name of the project file that was written.

    Note:
        The project gets a ``for Main use`` attribute only when a main file
        is passed, and the caller passes one only for a code block that is
        meant to be run. That restriction is deliberate rather than
        incidental: a code block that is only compiled may legitimately have
        no main procedure at all -- a package spec and body on their own are
        a complete example -- and naming a main for such a block would send
        the builder looking for something to link that the block does not
        contain. The extraction tests pin both halves of the distinction:
        the attribute is present for a runnable code block and absent
        otherwise.
    """
    gpr_filename = constants.PROJECT_FILENAME
    adc_filename = constants.PROJECT_PRAGMAS_FILENAME
    main_gpr = MAIN_GPR

    if spark_mode:
        gpr_filename = constants.SPARK_PROJECT_FILENAME
        adc_filename = constants.SPARK_PROJECT_PRAGMAS_FILENAME
        main_gpr = MAIN_SPARK_GPR

    adc_content = COMMON_ADC
    if spark_mode:
        adc_content += '\n' + SPARK_ADC

    with open(gpr_filename, u"w") as gpr_file:

        filtered_switches = []
        for switch in compiler_switches:
            filtered_switches.append('"' + switch + '"')
        if filtered_switches:
            placeholder_str = "--COMPILER_SWITCHES_PLACEHOLDER--"
            switches_str = ', '.join(filtered_switches)
            line_str = f'for Switches ("Ada") use ({switches_str});'
            main_gpr = main_gpr.replace(placeholder_str, line_str)

        if main_file is not None:
            mains = [main_file]
            main_list = [f'"{x}"' for x in mains]
            to_insert = f"for Main use ({', '.join(main_list)});"
        else:
            to_insert = f""
        main_gpr = main_gpr.replace("--MAIN_PLACEHOLDER--", to_insert)

        gpr_file.write(main_gpr)

    with open(adc_filename, u"w") as adc_file:
        adc_file.write(adc_content)

    return gpr_filename

class ProjectsList(object):
    @staticmethod
    def from_json_file(json_filename: str) -> ProjectsList | None:

        if os.path.isfile(json_filename):
            with open(json_filename, u'r') as f:
                projects_info = json.load(f)
                return ProjectsList(**projects_info)

        return None

    def __init__(self, projects: dict[str, bool] | None = None) -> None:
        self.projects: dict[str, bool] = projects if projects is not None else \
            dict()

    def to_json_file(self, json_filename: str) -> None:
        projects_info = vars(self)

        with open(json_filename, u'w') as f:
            json.dump(projects_info, f, indent=4)

    def add(self, project: str) -> None:
        self.projects[project] = True


def analyze_file(rst_file: str, extracted_projects_list_file: str | None = None) -> bool:
    """Extracts the code blocks of a single ReST file

    Each active code block is written to its own project directory below the
    current working directory, together with the ``block_info.json`` file that
    describes it for the checking stage.

    Args:
        rst_file (str): The ReST file to extract the code blocks from
        extracted_projects_list_file (str, optional): JSON file the names of
            the extracted projects are added to. Defaults to None.

    Returns:
        bool: The error flag for this file. The extraction command turns a
            true value into a non-zero exit status.

    Note:
        That flag is effectively the constant ``False`` today, so the exit
        status derived from it never becomes non-zero:

        * The single assignment that would set it sits in the nested
          ``expand_source_files()``. Without a ``nonlocal`` declaration it
          binds a fresh local there rather than the flag defined in this
          function, so the chopping failure it records dies with the nested
          scope.
        * The remaining per-block errors printed here never touch the flag at
          all: a block whose button and language do not go together, and a
          block with no button indicator.
        * The one condition this function treats as fatal for the whole run,
          a code block with no project name, calls ``exit(1)`` directly and so
          bypasses the flag too.

        A caller that inspects only the returned value therefore always
        concludes the file was extracted cleanly. In the extraction command
        this leaves the failure branch unreachable; that branch also announces
        ``TEST ERROR`` through ``fmt_utils.simple_success()``, the formatter
        for success messages.

        Not every ``ERROR`` line printed here marks a failure either. Removing
        a per-block directory left over from an earlier run whose info JSON
        file has gone missing is reported the same way, and that is a recovery
        on the success path.

        Repairing this means declaring ``nonlocal analysis_error`` in the
        nested scope and setting the flag at the remaining per-block error
        sites. Both are behavior changes: ReST files that pass today would
        start failing.
    """

    analysis_error = False

    with open(rst_file) as f:
        content = f.read()

    all_blocks = list(enumerate(filter(
        lambda b: b.language in ["ada", "c"] if isinstance(b, blocks.CodeBlock) else True,
        blocks.Block.get_blocks_from_rst(rst_file, content)
    )))

    code_blocks = [(i, b) for i, b in all_blocks if isinstance(b, blocks.CodeBlock)]

    if code_block_at:
        for i, block in code_blocks:
            block.active = False
            if block.line_start < code_block_at < block.line_end:
                block.active = True

    projects = dict()

    extr_prjs = None
    if extracted_projects_list_file is not None:
        if os.path.exists(extracted_projects_list_file):
            extr_prjs = ProjectsList.from_json_file(extracted_projects_list_file)
            if verbose:
                print ("Extracted list of projects from existing JSON file.")
        else:
            extr_prjs = ProjectsList()
            if verbose:
                print ("JSON file with list of projects will be created.")

    for (i, b) in code_blocks:
        if not b.active:
            continue

        if b.project is None:
            print ("Error: project not set in {} at line {}".format(
                rst_file, str(b.line_start)))
            exit(1)

        if not b.project in projects:
            projects[b.project] = list()
        projects[b.project].append((i, b))

    work_dir = os.getcwd()

    for project in projects:
        if extr_prjs is not None:
            extr_prjs.add(project)

        latest_project_dir = "latest"

        def init_project_dir(project):
            project_dir = get_project_dir(project)

            if os.path.exists(project_dir):
                if verbose:
                    print("Project directory already exists: " + project_dir)
                    print("Removing 'latest'...")
                shutil.rmtree(project_dir + "/" + latest_project_dir,
                              ignore_errors=True)

            os.makedirs(project_dir, exist_ok=True)

            return project_dir

        project_dir = init_project_dir(project)

        if verbose:
            print(fmt_utils.header("Checking project {}".format(project)))
            print("Number of code blocks: {}".format(len(projects[project])))

        for i, block in projects[project]:
            if isinstance(block, blocks.ConfigBlock):  # pragma: no cover
                current_config.update(block)
                toolchain_setup.reset_toolchain()
                continue

            toolchain_setup.set_toolchain(block)

            os.chdir(work_dir)  # change to work directory using absolute path

            has_error = False
            loc = "at {}:{} (code block #{})".format(
                rst_file, block.line_start, i)

            def print_error(*error_args):
                fmt_utils.error(*error_args)

            def print_warning(*warning_args):
                fmt_utils.warning(*warning_args)

            def chdir_project():
                # combining path to work directory (absolute path)
                # and current project directory
                os.chdir(work_dir + "/" + project_dir)

            def update_latest():

                def expand_source_files():
                    split = block.text.splitlines()

                    source_files = list()
                    if block.manual_chop:
                        source_files = manual_chop(split)
                    else:
                        source_files = real_gnatchop(split, block.compiler_switches)

                    if len(source_files) == 0:
                        print_error(loc, "Failed to chop example, skipping\n")
                        analysis_error = True
                        raise

                    for source_file in source_files:
                        with open(source_file.basename, u"w") as code_file:
                            code_file.write(source_file.content)

                    return source_files

                chdir_project()

                os.makedirs(latest_project_dir, exist_ok=True)
                os.chdir(latest_project_dir)

                source_files = expand_source_files()
                chdir_project()

                return latest_project_dir, source_files


            def prepare_project_block_dir(latest_project_dir):

                project_block_dir = str(block.text_hash_short)
                ref_block = None
                copytree_latest = True

                if os.path.exists(project_block_dir):
                    json_filename = constants.BLOCK_INFO_FILENAME
                    json_file = project_block_dir + "/" + json_filename
                    # isfile, not exists, to match the guard the reader uses:
                    # anything else here would trip the warning below over a
                    # file the reader never attempted and could not report on.
                    if os.path.isfile(json_file):
                        copytree_latest = False
                        ref_block = blocks.CodeBlock.from_json_file(json_file)
                        if ref_block is None:
                            # The file is there, so it is present but
                            # unreadable.  Extraction rewrites the record, so
                            # nothing is dropped and the run still succeeds
                            # -- but something damaged this file earlier, and
                            # a kept build directory carries it between runs.
                            # Say so where it cannot be mistaken for the
                            # fatal case.
                            #
                            # The message does not promise the block is
                            # checked: a block carrying a no-check class is
                            # extracted and then deliberately skipped, so
                            # that would be false for it.
                            print_warning(
                                loc,
                                "Block info file could not be read and is "
                                "being rebuilt: {}. The example is still "
                                "extracted and the run was not cut short, "
                                "but something damaged this file "
                                "earlier".format(json_file))
                    else:
                        print_error(loc, "Directory exists, but no JSON info file: removing it...\n")
                        shutil.rmtree(project_block_dir,
                                      ignore_errors=True)

                if copytree_latest:
                    # os.makedirs(project_block_dir)
                    shutil.copytree(latest_project_dir, project_block_dir)

                return project_block_dir, ref_block

            try:
                latest_project_dir, source_files = update_latest()
                for source_file in source_files:
                    block.source_files.append(source_file.basename)
            except Exception as e:
                print(str(e))
                print("Error while updating code for the block, continuing with next one!")
                block.to_json_file()
                toolchain_setup.reset_toolchain()
                continue

            project_block_dir, ref_block = prepare_project_block_dir(latest_project_dir)
            os.chdir(project_block_dir)

            if block.no_check:
                if verbose:
                    print("Skipping code block {}".format(loc))
                block.to_json_file()
                toolchain_setup.reset_toolchain()
                continue

            if block.syntax_only:
                block.to_json_file()
                toolchain_setup.reset_toolchain()
                continue

            compile_error = False
            prove_error = False

            def get_main_filename(block):
                if block.main_file is not None:
                    main_file = block.main_file
                else:
                    main_file = block.source_files[-1]
                return main_file

            if block.compile_it:

                if block.run_it:
                    block.project_main_file = get_main_filename(block)
                block.project_filename = write_project_file(block.project_main_file,
                                                            block.compiler_switches,
                                                            spark_mode=False)

            if block.prove_it:

                if block.language == "ada":

                    if block.run_it:
                        block.project_main_file = get_main_filename(block)
                    block.spark_project_filename = write_project_file(block.project_main_file,
                                                                      block.compiler_switches,
                                                                      spark_mode=True)
                else:
                    print_error(loc, "Wrong language selected for prove button")
                    has_error = True

            if len(block.buttons) == 0:
                print_error(loc, "Expected at least 'no_button' indicator, got none!")
                has_error = True

            block.to_json_file()

            toolchain_setup.reset_toolchain()

        os.chdir(work_dir)

        if extr_prjs is not None and extracted_projects_list_file is not None:
            extr_prjs.to_json_file(extracted_projects_list_file)

    return analysis_error

if __name__ == "__main__":  # pragma: no cover
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rst_files', type=str, nargs="+",
                        help="The rst file from which to extract doc")
    parser.add_argument('--build-dir', '-B', type=str, default=None,
                        help='Dir in which to build code')
    parser.add_argument('--extracted_projects', type=str, default=None,
                        help='JSON file containing list of extracted projects')

    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show more information')

    parser.add_argument('--all-diagnostics', '-A', action='store_true')
    parser.add_argument('--code-block-at', type=int, default=0)
    parser.add_argument('--max-columns', type=int, default=0)

    args = parser.parse_args()

    args.rst_files = [os.path.abspath(f) for f in args.rst_files]

    verbose = args.verbose
    code_block_at = args.code_block_at
    extracted_projects = args.extracted_projects
    build_dir = args.build_dir

    if extracted_projects is None and \
       build_dir is None:
        print("ERROR: at least --extracted_projects or --build-dir should be specified (or both).")
        exit(1)

    if extracted_projects:
        extracted_projects = os.path.abspath(extracted_projects)

    if build_dir is None:
        assert extracted_projects is not None  # guaranteed by the exit(1) above
        build_dir = os.path.dirname(extracted_projects)
        if build_dir == '': ## Special case: no directory in path
            build_dir = os.getcwd()
        if verbose:
            print("Build directory is set to: " + build_dir)

    if not os.path.exists(build_dir):
        os.makedirs(build_dir)

    test_error = False

    os.chdir(build_dir)

    for f in args.rst_files:
        analysis_error = analyze_file(f, extracted_projects)
        if analysis_error:
            test_error = True

    if test_error:
        fmt_utils.simple_success("TEST ERROR")
        exit(1)
    elif verbose:
        fmt_utils.simple_success("TEST SUCCESS")
