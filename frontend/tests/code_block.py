import re
from typing import List, Optional

from colors import col, Colors

class CodeBlock:
    def __init__(self, rst_file, line_start, line_end, text, language, project,
                 main_file, compiler_switches, classes, manual_chop, buttons, src_file):
        self.rst_file: str = rst_file
        self.line_start: int = line_start
        self.line_end: int = line_end
        self.text: str = text
        self.language: str = language
        self.project: str = project
        self.main_file: Optional[str] = main_file
        self.compiler_switches: List[str] = compiler_switches
        self.classes: List[str] = classes
        self.manual_chop: bool = manual_chop
        self.buttons: List[str] = buttons
        self.run: bool = True
        self.loc: str = f"{src_file}:{line_start}"

def get_blocks(input_text, src_file):
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
            blocks.append(CodeBlock(
                cb_start,
                i,
                "\n".join(l[cb_indent:] for l in lines[cb_start:i]),
                lang,
                project,
                main_file,
                compiler_switches,
                classes,
                manual_chop,
                buttons,
                src_file
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


    for i, (line, indent) in enumerate(zip(lines, indents)):
        last_line_number = i

        if cb_start != -1:
            process_block(i, line, indent)
        else:
            if line[indent:].startswith(".. code::"):
                start_code_block(i, line, indent)

    if cb_start != -1:
        print("{}: code block (start: {}, project: {}) doesn't have explanatory section!".format(
                col("WARNING", Colors.YELLOW), cb_start, project))
        process_block(last_line_number + 1, "END", 0)

        # Error: unable to process last code block
        if cb_start != -1:
            print("{}: code block (start: {}, project: {}) hasn't been successfully processed!".format(
                col("ERROR", Colors.RED), cb_start, project))
            exit(1)

    return blocks
