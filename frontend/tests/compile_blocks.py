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
import subprocess as S
from os import path as P
import colors as C
import shutil
import glob
import re
import hashlib
import json
from widget.chop import manual_chop, cheapo_gnatchop, real_gnatchop


class Block(object):
    @staticmethod
    def get_blocks(rst_file, input_text):
        lang_re = re.compile("\s*.. code::\s*(\w+)?\s*")
        project_re = re.compile("\s*.. code::.*project=(\S+)?")
        main_re = re.compile("\s*.. code::.*main=(\S+)?")
        manual_chop_re = re.compile("\s*.. code::.*manual_chop?")
        button_re = re.compile("\s+(\S+)_button")
        code_config_re = re.compile(":code-config:`(.*)?`")
        classes_re = re.compile("\s*:class:\s*(.+)")
        switches_re = re.compile("\s*.. code::.*switches=(\S+)?")
        compiler_switches_re = re.compile("Compiler[(](\S+)?[)]")

        blocks = []
        lines = input_text.splitlines()

        def first_nonws(line):
            for i, c in enumerate(line):
                if not c.isspace():
                    return i
            return 0

        indents = map(first_nonws, lines)

        classes = []
        compiler_switches = []
        buttons = []
        cb_start = -1
        cb_indent = -1
        lang = ""
        project = None
        main_file = None
        manual_chop = None
        last_line_number = -1

        def is_empty(line):
            return (not line) or line.isspace()

        def process_block(i, line, indent):
            nonlocal classes, cb_start, cb_indent, lang

            if cb_indent == -1 and not is_empty(line):
                cb_indent = indent

            if indent < cb_indent and not is_empty(line):
                text = ("\n".join(l[cb_indent:] for l in lines[cb_start:i]))
                text = text[1:]     # Remove first newline

                blocks.append(CodeBlock(
                    rst_file,
                    cb_start,
                    i,
                    text,
                    lang,
                    project,
                    main_file,
                    compiler_switches,
                    classes,
                    manual_chop,
                    buttons
                ))

                classes, cb_start, cb_indent, lang = [], -1, -1, ""

            m = classes_re.match(line)

            if m:
                classes = [str.strip(l) for l in m.groups()[0].split(",")]
                cb_start = i + 1

        def start_code_block(i, line, indent):
            nonlocal cb_start, lang, project, main_file, manual_chop, \
                     buttons, compiler_switches

            cb_start, lang = (
                i + 1,
                lang_re.match(line).groups()[0]
            )
            project = project_re.match(line)
            if project is not None:
                project = project.groups()[0]

            main_file = main_re.match(line)
            if main_file is not None:
                # Retrieve actual main filename
                main_file = main_file.groups()[0]
            if lang == "c":
                manual_chop = True
            else:
                manual_chop = (manual_chop_re.match(line) is not None)
            buttons = button_re.findall(line)

            all_switches = switches_re.match(line)

            compiler_switches = []
            if all_switches is not None:
                all_switches = all_switches.groups()[0]
                compiler_switches = compiler_switches_re.match(all_switches)
                if compiler_switches is not None:
                    compiler_switches = [str.strip(l)
                        for l in compiler_switches.groups()[0].split(",")]

            # Add default switches
            default_switches = {
                "Builder": [],
                "Compiler": [
                    "-gnata",
                ],
            }

            for category in default_switches:
                for sw in default_switches[category]:
                    if sw not in compiler_switches:
                        compiler_switches.append(sw)


        def start_config_block(i, line, indent):
            blocks.append(ConfigBlock(rst_file,
                **dict(
                    kv.split('=')
                    for kv in code_config_re.findall(line)[0].split(";"))
            ))


        for i, (line, indent) in enumerate(zip(lines, indents)):
            last_line_number = i

            if cb_start != -1:
                process_block(i, line, indent)
            else:
                if line[indent:].startswith(".. code::"):
                    start_code_block(i, line, indent)
                elif line[indent:].startswith(":code-config:"):
                    start_config_block(i, line, indent)

        if cb_start != -1:
            print("{}: code block (start: {}, project: {}) doesn't have explanatory section!".format(
                    C.col("WARNING", C.Colors.YELLOW), cb_start, project))
            process_block(last_line_number + 1, "END", 0)

            # Error: unable to process last code block
            if cb_start != -1:
                print("{}: code block (start: {}, project: {}) hasn't been successfully processed!".format(
                    C.col("ERROR", C.Colors.RED), cb_start, project))
                exit(1)

        return blocks


class CodeBlock(Block):
    def __init__(self, rst_file, line_start, line_end, text, language, project,
                 main_file, compiler_switches, classes, manual_chop, buttons):
        self.rst_file = rst_file
        self.line_start = line_start
        self.line_end = line_end
        self.text = text
        self.language = language
        self.project = project
        self.main_file = main_file
        self.compiler_switches = compiler_switches
        self.classes = classes
        self.manual_chop = manual_chop
        self.buttons = buttons
        self.run = True

        # Hash of source-code
        str_text = str(self.text).encode("utf-8")
        self.text_hash: str = hashlib.sha512(str_text).hexdigest()
        self.text_hash_short: str = hashlib.md5(str_text).hexdigest()

class ConfigBlock(Block):
    def __init__(self, **opts):
        self._opts = opts
        for k, v in opts.items():
            setattr(self, k, False if v == "False" else True)

    def update(self, other_config):
        self.__init__(**other_config._opts)


current_config = ConfigBlock(
    run_button=False, prove_button=True, accumulate_code=False
)


def header(strn):
    return C.col("{}\n{}\n".format(strn, '*' * len(strn)), C.Colors.BLUE)


def error(loc, strn):
    print("{} {}: {}".format(C.col("ERROR", C.Colors.RED), loc, strn))


def get_line(block):
    """
    """
    line = block.line
    precise = True

    if not line:
        parent_line = block.parent.line
        if not parent_line:
            # tableflip
            return False, 0
        parent_text = block.parent.astext()
        block_text = block.astext()
        offset = parent_text.find(block_text)
        line = parent_line + len(re.findall("\n", parent_text[:offset]))
        precise = False

    return precise, line


class Diag(object):
    def __init__(self, file, line, col, msg):
        self.file = file
        self.line = line
        self.col = col
        self.msg = msg

    def __repr__(self):
        return "{}:{}:{}: {}".format(self.file, self.line, self.col, self.msg)


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

args.rst_files = [os.path.abspath(f) for f in args.rst_files]

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

def write_project_file(main_file, compiler_switches, spark_mode):
    gpr_filename = "main.gpr"
    adc_filename = "main.adc"

    adc_content = COMMON_ADC
    if spark_mode:
        adc_content += '\n' + SPARK_ADC

    with open(gpr_filename, u"w") as gpr_file:
        main_gpr = MAIN_GPR

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

def analyze_file(rst_file):

    analysis_error = False

    with open(rst_file) as f:
        content = f.read()

    blocks = list(enumerate(filter(
        lambda b: b.language in ["ada", "c"] if isinstance(b, CodeBlock) else True,
        Block.get_blocks(rst_file, content)
    )))

    code_blocks = [(i, b) for i, b in blocks if isinstance(b, CodeBlock)]

    def run(*run_args):
        if args.verbose:
            print("Running \"{}\"".format(" ".join(run_args)))
        try:
            output = S.check_output(run_args, stderr=S.STDOUT).decode("utf-8")
            all_output.extend(output.splitlines())
        except S.CalledProcessError as e:
            all_output.extend(e.output.decode("utf-8").splitlines())
            raise e

        return output

    def output_block_info(block):
        block_info = vars(block)

        with open('block_info.json', u'w') as f:
            json.dump(block_info, f, indent=4)

        block_info_json_file = "block_info.json"
        if os.path.isfile(block_info_json_file):
            with open(block_info_json_file, u'r') as f:
                block_info_json = json.load(f)

    if args.code_block_at:
        for i, block in code_blocks:
            block.run = False
            if block.line_start < args.code_block_at < block.line_end:
                block.run = True

    if args.code_block:
        expr = "code_blocks[{}]".format(args.code_block)
        subset = eval(expr, globals(), locals())
        if not isinstance(code_blocks, list):
            subset = [subset]

        for i, code_block in code_blocks:
            code_block.run = False

        for i, code_block in subset:
            code_block.run = True

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

    projects = dict()

    for (i, b) in code_blocks:
        if b.project is None:
            print ("Error: project not set in {} at line {}".format(
                rst_file, str(b.line_start)))
            exit(1)

        if not b.project in projects:
            projects[b.project] = list()
        projects[b.project].append((i, b))

    work_dir = os.getcwd()
    base_project_dir = "projects"

    for project in projects:

        def init_project_dir(project):
            project_dir = base_project_dir + "/" + project.replace(".", "/")

            if os.path.exists(project_dir):
                shutil.rmtree(project_dir)

            try:
                os.makedirs(project_dir)
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise

            return project_dir

        project_dir = init_project_dir(project)

        if args.verbose:
            print(header("Checking project {}".format(project)))
            print("Number of code blocks: {}".format(len(projects[project])))

        for i, block in projects[project]:
            if isinstance(block, ConfigBlock):
                current_config.update(block)
                continue

            os.chdir(work_dir)  # change to work directory using absolute path

            has_error = False
            loc = "at {}:{} (code block #{})".format(
                rst_file, block.line_start, i)

            all_output = []

            def print_diags():
                diags = extract_diagnostics(all_output)
                for diag in diags:
                    diag.line = diag.line + block.line_start
                    diag.file = rst_file
                    print(diag)

            def print_error(*error_args):
                error(*error_args)
                print_diags()

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

                latest_project_dir = "latest"
                os.makedirs(latest_project_dir, exist_ok=True)
                os.chdir(latest_project_dir)

                source_files = expand_source_files()
                chdir_project()

                return latest_project_dir, source_files


            def prepare_project_block_dir(latest_project_dir):

                project_block_dir = str(block.text_hash_short)
                if not os.path.exists(project_block_dir):
                    # os.makedirs(project_block_dir)
                    shutil.copytree(latest_project_dir, project_block_dir)

                return project_block_dir

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

            try:
                latest_project_dir, source_files = update_latest()
            except Exception as e:
                print(e.message)
                print("Error while updating code for the block, continuing with next one!")
                continue

            project_block_dir = prepare_project_block_dir(latest_project_dir)
            os.chdir(project_block_dir)

            project_filename = None
            main_file = None

            no_check = any(sphinx_class in ["ada-nocheck", "c-nocheck"]
                           for sphinx_class in block.classes)
            if no_check:
                if args.verbose:
                    print("Skipping code block {}".format(loc))
                continue

            if args.verbose:
                print(header("Checking code block {}".format(loc)))

            # Syntax check
            if 'nosyntax-check' not in block.classes:
                for source_file in source_files:

                    try:
                        if block.language == "ada":
                            commands = ["gcc", "-c", "-gnats", "-gnatyg0-s"]
                            if args.max_columns > 0:
                                commands.append("-gnatyM" + str(args.max_columns))
                            out = run(*commands +
                                      block.compiler_switches +
                                      [source_file.basename])
                        elif block.language == "c":
                            out = run("gcc", "-c", source_file.basename)

                        if out:
                            print_error(loc, "Failed to syntax check example")
                            has_error = True
                    except S.CalledProcessError:
                        print_error(loc, "Failed to syntax check example")
                        has_error = True

            if 'ada-syntax-only' in block.classes or not block.run:
                cleanup_project(block.language, project_filename, main_file)
                continue

            compile_error = False
            prove_error = False
            is_prove_error_class = False

            run_block = (('ada-run' in block.classes
                          or 'ada-run-expect-failure' in block.classes
                          or 'run' in block.buttons)
                         and not 'ada-norun' in block.classes)
            compile_block = run_block or ('compile' in block.buttons)

            prove_buttons = ["prove", "prove_flow", "prove_flow_report_all",
                             "prove_report_all"]

            prove_block = any(b in prove_buttons for b in block.buttons)

            def get_main_filename(block):
                if block.main_file is not None:
                    main_file = block.main_file
                else:
                    main_file = source_files[-1].basename
                return main_file

            if compile_block:

                if run_block:
                    main_file = get_main_filename(block)
                spark_mode = False
                project_filename = write_project_file(main_file,
                                                      block.compiler_switches,
                                                      spark_mode)

                if block.language == "ada":

                    try:
                        run("gprclean", "-P", project_filename)
                        out = run("gprbuild", "-q", "-P", project_filename)
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
                               P.splitext(main_file)[0]] + glob.glob('*.c')
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

                if not compile_error and not has_error and run_block:
                    if block.language == "ada":
                        try:
                            out = run("./{}".format(P.splitext(main_file)[0]))

                            if 'ada-run-expect-failure' in block.classes:
                                print_error(
                                    loc, "Running of example should have failed"
                                )
                                has_error = True

                        except S.CalledProcessError as e:
                            if 'ada-run-expect-failure' in block.classes:
                                if args.verbose:
                                    print("Running of example expectedly failed")
                            else:
                                print_error(loc, "Running of example failed")
                                has_error = True

                            out = str(e.output.decode("utf-8"))

                        with open("run.log", u"w") as logfile:
                            logfile.write(out)

                    elif block.language == "c":
                        try:
                            out = run("./{}".format(P.splitext(main_file)[0]))

                            if 'c-run-expect-failure' in block.classes:
                                print_error(
                                    loc, "Running of example should have failed"
                                )
                                has_error = True

                        except S.CalledProcessError as e:
                            if 'c-run-expect-failure' in block.classes:
                                if args.verbose:
                                    print("Running of example expectedly failed")
                            else:
                                print_error(loc, "Running of example failed")
                                has_error = True
                            out = str(e.output.decode("utf-8"))

                        with open("run.log", u"w") as logfile:
                            logfile.write(out)

            if False:

                for source_file in source_files:
                    if block.language == "ada":
                        try:
                            out = run("gcc", "-c", "-gnatc", "-gnatyg0-s",
                                      source_file.basename)
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
                            out = run("gcc", "-c", source_file.basename)
                        except S.CalledProcessError as e:
                            if 'c-expect-compile-error' in block.classes:
                                compile_error = True
                            else:
                                print_error(loc, "Failed to compile example")
                                has_error = True
                            out = str(e.output.decode("utf-8"))

                        with open("compile.log", u"w+") as logfile:
                            logfile.write(out)

            if prove_block:

                if block.language == "ada":

                    main_file = get_main_filename(block)
                    spark_mode = True
                    project_filename = write_project_file(main_file,
                                                          block.compiler_switches,
                                                          spark_mode)

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

                    line = ["gnatprove", "-P", project_filename,
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
                if not prove_block:
                    print_error(loc, "Expected prove button, got none!")
                    has_error = True

            if prove_block:
                if is_prove_error_class and not prove_error:
                    print_error(loc, "Expected prove error, got none!")
                    has_error = True

            if (any (c in ['ada-run-expect-failure','ada-norun'] for
                     c in block.classes)
                and not 'run' in block.buttons):
                print_error(loc, "Expected run button, got none!")
                has_error = True

            if has_error:
                analysis_error = True
            elif args.verbose:
                print(C.col("SUCCESS", C.Colors.GREEN))

            cleanup_project(block.language, project_filename, main_file)

            output_block_info(block)

            if args.all_diagnostics:
                print_diags()

        os.chdir(work_dir)

    if os.path.exists(base_project_dir) and not args.keep_files:
        shutil.rmtree(base_project_dir)

    return analysis_error

# Remove the build dir, but only if the user didn't ask for a specific
# subset of code_blocks
if (os.path.exists(args.build_dir) and not args.code_block
    and not args.keep_files):
    shutil.rmtree(args.build_dir)

if not os.path.exists(args.build_dir):
    os.makedirs(args.build_dir)

os.chdir(args.build_dir)

test_error = False

for f in args.rst_files:
    analysis_error = analyze_file(f)
    if analysis_error:
        test_error = True

if test_error:
    print(C.col("TEST ERROR", C.Colors.RED))
    exit(1)
elif args.verbose:
    print(C.col("TEST SUCCESS", C.Colors.GREEN))
