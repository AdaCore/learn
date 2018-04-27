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

from docutils.core import publish_doctree
import argparse
import os
import subprocess as S
from IPython import embed
from os import path as P
import colors as C
import shutil
import re


def header(strn):
    return C.col("{}\n{}\n".format(strn, '*' * len(strn)), C.Colors.BLUE)


def error(loc, strn):
    print "{} {}: {}".format(C.col("ERROR", C.Colors.RED), loc, strn)


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


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('rst_file', type=str,
                    help="The rst file from which to extract doc")
parser.add_argument('--build-dir', '-B', type=str, default="build",
                    help='Dir in which to build code')

parser.add_argument('--verbose', '-v', type=bool, default=False,
                    help='Show more information')

parser.add_argument('--code-block', '-b', type=str, default=0)

args = parser.parse_args()

with open(args.rst_file) as f:
    content = f.read()

doctree = publish_doctree(content)


def is_ada_code_block(node):
    return (node.tagname == 'literal_block'
            and 'code' in node.attributes['classes']
            and 'ada' in node.attributes['classes'])


code_blocks = list(enumerate(doctree.traverse(condition=is_ada_code_block)))

# Remove the build dir, but only if the user didn't ask for a specific subset
# of code_blocks
if os.path.exists(args.build_dir) and not args.code_block:
    shutil.rmtree(args.build_dir)

if not os.path.exists(args.build_dir):
    os.makedirs(args.build_dir)

os.chdir(args.build_dir)


def run(*run_args):
    if args.verbose:
        print "Running \"{}\"".format(" ".join(run_args))
    try:
        output = S.check_output(run_args, stderr=S.STDOUT)
        all_output.append(output)
    except S.CalledProcessError as e:
        all_output.append(e.output)
        raise e

    return output


if args.code_block:
    expr = "code_blocks[{}]".format(args.code_block)
    code_blocks = eval(expr, globals(), locals())
    if not isinstance(code_blocks, list):
        code_blocks = [code_blocks]

for i, b in code_blocks:
    has_error = False
    precise, b_line = get_line(b)
    qualifier = "at" if precise else "around"
    loc = "{} {}:{} (code block #{})".format(qualifier, args.rst_file, b_line, i)

    all_output = []

    def print_error(*args):
        print error(*args)
        print "\n".join(all_output)

    if 'ada-nocheck' in b.attributes['classes']:
        if args.verbose:
            print "Skipping code block {}".format(loc)
        continue

    if args.verbose:
        print header("Checking code block {}".format(loc))

    with open(u"code.ada", u"w") as code_file:
        code_file.write(b.astext().encode('utf-8'))

    try:
        out = run("gnatchop", "-w", "code.ada").splitlines()
    except S.CalledProcessError:
        print_error(loc, "Failed to chop example, skipping\n")
        continue

    if 'ada-syntax-only' in b.attributes['classes']:
        continue

    for i, line in enumerate(out):
        if line.endswith("into:"):
            idx = i + 1
            break

    source_files = [s.strip() for s in out[idx:]]

    compile_error = False

    if 'ada-run' in b.attributes['classes'] or 'ada-run-expect-failure' in b.attributes['classes']:
        if len(source_files) == 1:
            main_file = source_files[0]
        else:
            main_file = 'main.adb'

        try:
            run("gprbuild", "-f", main_file)
        except S.CalledProcessError:
            print_error(loc, "Compiling of example failed")
            has_error = True

        if not has_error:
            try:
                run("./{}".format(P.splitext(main_file)[0]))

                if 'ada-run-expect-failure' in b.attributes['classes']:
                    print_error(loc, "Running of example should have failed")
                    has_error = True

            except S.CalledProcessError:
                if 'ada-run-expect-failure' in b.attributes['classes']:
                    if args.verbose:
                        print "Running of example expectedly failed"
                else:
                    print_error(loc, "Running of example failed")
                    has_error = True

    else:
        for source_file in source_files:
            try:
                run("gcc", "-c", "-gnatc", "-gnaty", source_file)
            except S.CalledProcessError:
                if 'ada-expect-compile-error' in b.attributes['classes']:
                    compile_error = True
                else:
                    print_error(loc, "Failed to compile example")
                    has_error = True

    if 'ada-expect-compile-error' in b.attributes['classes'] and not compile_error:
        print_error(loc, "Expected compile error, got none!")
        has_error = True

    if not has_error and args.verbose:
        print C.col("SUCCESS", C.Colors.GREEN)
