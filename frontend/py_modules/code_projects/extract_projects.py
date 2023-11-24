#! /usr/bin/env python3

"""
This program will extract every Ada code block in an Ada source file
The default behavior is to:
- Split the block with ``gnatchop``
"""

import os
import shutil
import re
import json

from widget.chop import manual_chop, real_gnatchop

import blocks
import fmt_utils


current_config = blocks.ConfigBlock(
    run_button=False, prove_button=True, accumulate_code=False
)


class Diag(object):
    def __init__(self, file, line, col, msg):
        self.file = file
        self.line = line
        self.col = col
        self.msg = msg

    def __repr__(self):
        return "{}:{}:{}: {}".format(self.file, self.line, self.col, self.msg)


verbose = False
code_block_at = None

BASE_PROJECT_DIR = "projects"

def get_project_dir(project):
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
      for Global_Configuration_Pragmas use "main.adc";
   end Builder;

end Main;
"""

MAIN_SPARK_GPR="""
project Main_Spark is

   --MAIN_PLACEHOLDER--

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-O0");
      --COMPILER_SWITCHES_PLACEHOLDER--
   end Compiler;

   package Builder is
      for Default_Switches ("Ada") use ("-g");
      for Global_Configuration_Pragmas use "main_spark.adc";
   end Builder;

end Main_Spark;
"""

def write_project_file(main_file, compiler_switches, spark_mode):
    gpr_filename = "main.gpr"
    adc_filename = "main.adc"
    main_gpr = MAIN_GPR

    if spark_mode:
        gpr_filename = "main_spark.gpr"
        adc_filename = "main_spark.adc"
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
    def from_json_file(json_filename):

        if os.path.isfile(json_filename):
            with open(json_filename, u'r') as f:
                projects_info = json.load(f)
                return ProjectsList(**projects_info)

        return None

    def __init__(self, projects=None):
        self.projects = projects if projects is not None else \
            list()

    def to_json_file(self, json_filename):
        projects_info = vars(self)

        with open(json_filename, u'w') as f:
            json.dump(projects_info, f, indent=4)

    def append(self, project):
        self.projects.append(project)


def analyze_file(rst_file, extracted_projects_list_file=None):

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
    else:
        for i, block in code_blocks:
            block.active = True

    def remove_string(some_text, rem):
        return re.sub(".*" + rem + ".*\n?","", some_text)

    projects = dict()

    extr_prjs = None
    if extracted_projects_list_file is not None:
        if os.path.exists(extracted_projects_list_file):
            extr_prjs = ProjectsList.from_json_file(extracted_projects_list_file)
        else:
            print ("Error: output JSON file not found")
            exit(1)

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
            extr_prjs.append(project)

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
            if isinstance(block, blocks.ConfigBlock):
                current_config.update(block)
                continue

            os.chdir(work_dir)  # change to work directory using absolute path

            has_error = False
            loc = "at {}:{} (code block #{})".format(
                rst_file, block.line_start, i)

            def print_error(*error_args):
                fmt_utils.error(*error_args)

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
                        source_files = real_gnatchop(split)

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
                    json_filename = "block_info.json"
                    json_file = project_block_dir + "/" + json_filename
                    if os.path.exists(json_file):
                        copytree_latest = False
                        ref_block = blocks.CodeBlock.from_json_file(json_file)
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
                print(e.message)
                print("Error while updating code for the block, continuing with next one!")
                block.to_json_file()
                continue

            project_block_dir, ref_block = prepare_project_block_dir(latest_project_dir)
            os.chdir(project_block_dir)

            if block.no_check:
                if verbose:
                    print("Skipping code block {}".format(loc))
                block.to_json_file()
                continue

            if block.syntax_only:
                block.to_json_file()
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

                    block.project_main_file = get_main_filename(block)
                    block.spark_project_filename = write_project_file(block.project_main_file,
                                                                      block.compiler_switches,
                                                                      spark_mode=True)
                else:
                    print_error(loc, "Wrong language selected for prove button")
                    print(e.output)
                    has_error = True

            if len(block.buttons) == 0:
                print_error(loc, "Expected at least 'no_button' indicator, got none!")
                has_error = True

            block.to_json_file()

        if extr_prjs is not None:
            extr_prjs.to_json_file(extracted_projects_list_file)

        os.chdir(work_dir)

    return analysis_error

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rst_files', type=str, nargs="+",
                        help="The rst file from which to extract doc")
    parser.add_argument('--build-dir', '-B', type=str, default="build",
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

    if not os.path.exists(args.build_dir):
        os.makedirs(args.build_dir)

    test_error = False

    if args.extracted_projects is not None:
        # Init JSON file containing list of extracted projects
        extr_prjs = None
        extr_prjs = ProjectsList()
        extr_prjs.to_json_file(args.extracted_projects)

    os.chdir(args.build_dir)

    for f in args.rst_files:
        analysis_error = analyze_file(f, args.extracted_projects)
        if analysis_error:
            test_error = True

    if test_error:
        fmt_utils.simple_success("TEST ERROR")
        exit(1)
    elif verbose:
        fmt_utils.simple_success("TEST SUCCESS")
