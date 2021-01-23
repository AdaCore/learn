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
import re
from widget.chop import manual_chop, cheapo_gnatchop, real_gnatchop


class Block(object):
    @staticmethod
    def get_blocks(input_text):
        lang_re = re.compile("\s*.. code::\s*(\w+)?\s*")
        project_re = re.compile("\s*.. code::.*project=(\S+)?")
        main_re = re.compile("\s*.. code::.*main=(\S+)?")
        manual_chop_re = re.compile("\s*.. code::.*manual_chop?")
        button_re = re.compile("\s+(\S+)_button")
        code_config_re = re.compile(":code-config:`(.*)?`")
        classes_re = re.compile("\s*:class:\s*(.+)")

        blocks = []
        lines = input_text.splitlines()

        def first_nonws(line):
            for i, c in enumerate(line):
                if not c.isspace():
                    return i
            return 0

        indents = map(first_nonws, lines)

        classes = []
        cb_start = -1
        cb_indent = -1
        lang = ""

        def is_empty(line):
            return (not line) or line.isspace()

        for i, (line, indent) in enumerate(zip(lines, indents)):
            if cb_start != -1:

                if cb_indent == -1 and not is_empty(line):
                    cb_indent = indent

                if indent < cb_indent and not is_empty(line):
                    blocks.append(CodeBlock(
                        cb_start,
                        i,
                        "\n".join(l[cb_indent:] for l in lines[cb_start:i]),
                        lang,
                        project,
                        main_file,
                        classes,
                        manual_chop,
                        buttons
                    ))

                    classes, cb_start, cb_indent, lang = [], -1, -1, ""

                m = classes_re.match(line)

                if m:
                    classes = [str.strip(l) for l in m.groups()[0].split(",")]
                    cb_start = i + 1
            else:
                if line[indent:].startswith(".. code::"):
                    cb_start, lang = (
                        i + 1,
                        lang_re.match(line).groups()[0]
                    )
                    project = project_re.match(line).groups()[0]
                    main_file = main_re.match(line)
                    if main_file is not None:
                        # Retrieve actual main filename
                        main_file = main_file.groups()[0]
                    if lang == "c":
                        manual_chop = True
                    else:
                        manual_chop = (manual_chop_re.match(line) is not None)
                    buttons = button_re.findall(line)

                elif line[indent:].startswith(":code-config:"):
                    blocks.append(ConfigBlock(**dict(
                        kv.split('=')
                        for kv
                        in code_config_re.findall(line)[0].split(";")
                    )))

        return blocks


class CodeBlock(Block):
    def __init__(self, line_start, line_end, text, language, project,
                 main_file, classes, manual_chop, buttons):
        self.line_start = line_start
        self.line_end = line_end
        self.text = text
        self.language = language
        self.project = project
        self.main_file = main_file
        self.classes = classes
        self.manual_chop = manual_chop
        self.buttons = buttons
        self.run = True


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

parser.add_argument('--verbose', '-v', type=bool, default=False,
                    help='Show more information')

parser.add_argument('--code-block', '-b', type=str, default=0)
parser.add_argument('--all-diagnostics', '-A', action='store_true')
parser.add_argument('--code-block-at', type=int, default=0)

args = parser.parse_args()

args.rst_files = [os.path.abspath(f) for f in args.rst_files]

def analyze_file(rst_file):

    analysis_error = False

    with open(rst_file) as f:
        content = f.read()

    blocks = list(enumerate(filter(
        lambda b: b.language in ["ada", "c"] if isinstance(b, CodeBlock) else True,
        Block.get_blocks(content)
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

    projects = dict()

    for (i, b) in code_blocks:
        if not b.project in projects:
            projects[b.project] = list()
        projects[b.project].append((i, b))

    work_dir = os.getcwd()
    base_project_dir = "projects"

    for project in projects:

        def init_project_dir(project):
            project_dir = base_project_dir + "/" + project.replace(".", "/")

            if os.path.exists(project_dir):
                shutil.rmtree(args.build_dir)

            try:
                os.makedirs(project_dir)
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise

            os.chdir(project_dir)

        init_project_dir(project)

        for i, block in projects[project]:
            if isinstance(block, ConfigBlock):
                current_config.update(block)
                continue

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

            no_check = any(sphinx_class in ["ada-nocheck", "c-nocheck"]
                           for sphinx_class in block.classes)
            if no_check:
                if args.verbose:
                    print("Skipping code block {}".format(loc))
                continue

            if args.verbose:
                print(header("Checking code block {}".format(loc)))

            split = block.text.splitlines()

            source_files = list()
            if block.manual_chop:
                source_files = manual_chop(split)
            else:
                source_files = real_gnatchop(split)

            if len(source_files) == 0:
                print_error(loc, "Failed to chop example, skipping\n")
                analysis_error = True
                continue

            for source_file in source_files:

                with open(source_file.basename, u"w") as code_file:
                    code_file.write(source_file.content)

                try:
                    if block.language == "ada":
                        out = run("gcc", "-c", "-gnats", "-gnatyg0-s",
                                  source_file.basename)
                    elif block.language == "c":
                        out = run("gcc", "-c", source_file.basename)

                    if out:
                        print_error(loc, "Failed to syntax check example")
                        has_error = True
                except S.CalledProcessError:
                    print_error(loc, "Failed to syntax check example")
                    has_error = True

            if 'ada-syntax-only' in block.classes or not block.run:
                continue

            compile_error = False

            if (('ada-run' in block.classes
                 or 'ada-run-expect-failure' in block.classes
                 or 'run' in block.buttons)
                and not 'ada-norun' in block.classes
            ):
                if block.main_file is not None:
                    main_file = block.main_file
                else:
                    main_file = source_files[-1].basename

                if block.language == "ada":
                    try:
                        out = run("gprbuild", "-gnata", "-gnatyg0-s", "-f",
                                  main_file)
                    except S.CalledProcessError as e:
                        if 'ada-expect-compile-error' in block.classes:
                            compile_error = True
                        else:
                            print_error(loc, "Failed to compile example")
                            print(e.output)
                            has_error = True
                elif block.language == "c":
                    try:
                        out = run("gcc", "-c", main_file)
                    except S.CalledProcessError as e:
                        if 'ada-expect-compile-error' in block.classes:
                            compile_error = True
                        else:
                            print_error(loc, "Failed to compile example")
                            print(e.output)
                            has_error = True

                if not compile_error and not has_error:
                    if block.language == "ada":
                        try:
                            run("./{}".format(P.splitext(main_file)[0]))

                            if 'ada-run-expect-failure' in block.classes:
                                print_error(
                                    loc, "Running of example should have failed"
                                )
                                has_error = True

                        except S.CalledProcessError:
                            if 'ada-run-expect-failure' in block.classes:
                                if args.verbose:
                                    print("Running of example expectedly failed")
                            else:
                                print_error(loc, "Running of example failed")
                                has_error = True

            elif 'compile' in block.buttons:

                for source_file in source_files:
                    if block.language == "ada":
                        try:
                            run("gcc", "-c", "-gnatc", "-gnatyg0-s",
                                source_file.basename)
                        except S.CalledProcessError:
                            if 'ada-expect-compile-error' in block.classes:
                                compile_error = True
                            else:
                                print_error(loc, "Failed to compile example")
                                has_error = True

            if 'ada-expect-compile-error' in block.classes and not compile_error:
                print_error(loc, "Expected compile error, got none!")
                has_error = True

            if has_error:
                analysis_error = True
            elif args.verbose:
                print(C.col("SUCCESS", C.Colors.GREEN))

            if args.all_diagnostics:
                print_diags()

        os.chdir(work_dir)

    if os.path.exists(base_project_dir):
        shutil.rmtree(base_project_dir)

    return analysis_error

# Remove the build dir, but only if the user didn't ask for a specific
# subset of code_blocks
if os.path.exists(args.build_dir) and not args.code_block:
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
