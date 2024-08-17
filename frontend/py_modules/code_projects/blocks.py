import os
import re
import hashlib
import json

import colors as C
import toolchain_info

class Block(object):
    @staticmethod
    def get_blocks_from_rst(rst_file, input_text):
        lang_re = re.compile("\s*.. code::\s*(\w+)?\s*")
        project_re = re.compile("\s*.. code::.*project=(\S+)?")
        main_re = re.compile("\s*.. code::.*main=(\S+)?")
        manual_chop_re = re.compile("\s*.. code::.*manual_chop?")
        button_re = re.compile("\s+(\S+)_button")
        code_config_re = re.compile(":code-config:`(.*)?`")
        classes_re = re.compile("\s*:class:\s*(.+)")
        switches_re = re.compile("\s*.. code::.*switches=(\S+)?")
        compiler_switches_re = re.compile("Compiler[(](\S+)?[)]")
        gnat_version_re=re.compile("\s*.. code::.*gnat=(\S+)?")
        gnatprove_version_re=re.compile("\s*.. code::.*gnatprove=(\S+)?")
        gprbuild_version_re=re.compile("\s*.. code::.*gprbuild=(\S+)?")

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
        gnat_version = None
        gnatprove_version = None
        gprbuild_version = None
        last_line_number = -1

        def is_empty(line):
            return (not line) or line.isspace()

        def reset_block_info():
            # Reset information for next block
            nonlocal classes, cb_start, cb_indent, lang

            classes, cb_start, cb_indent, lang = [], -1, -1, ""

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
                    gnat_version,
                    gnatprove_version,
                    gprbuild_version,
                    compiler_switches,
                    classes,
                    manual_chop,
                    buttons
                ))

                reset_block_info()

            m = classes_re.match(line)

            if m:
                classes = [str.strip(l) for l in m.groups()[0].split(",")]
                cb_start = i + 1

        def start_code_block(i, line, indent):
            nonlocal cb_start, lang, project, main_file, manual_chop, \
                     buttons, compiler_switches, \
                     gnat_version, gnatprove_version, gprbuild_version

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

            project_gnat_version = gnat_version_re.match(line)
            project_gnatprove_version = gnatprove_version_re.match(line)
            project_gprbuild_version = gprbuild_version_re.match(line)

            if project_gnat_version is not None:
                gnat_version = ["selected", project_gnat_version.groups()[0]]
            else:
                gnat_version = ["default", toolchain_info.get_toolchain_default_version('gnat')]
            if project_gnatprove_version is not None:
                gnatprove_version = ["selected", project_gnatprove_version.groups()[0]]
            else:
                gnatprove_version = ["default", toolchain_info.get_toolchain_default_version('gnatprove')]
            if project_gprbuild_version is not None:
                gprbuild_version = ["selected", project_gprbuild_version.groups()[0]]
            else:
                gprbuild_version = ["default", toolchain_info.get_toolchain_default_version('gprbuild')]

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


        reset_block_info()

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

    def to_json_file(self, json_filename=None):
        block_info = vars(self)

        if json_filename is None:
            json_filename = "block_info.json"
        with open(json_filename, u'w') as f:
            json.dump(block_info, f, indent=4)

class CodeBlock(Block):
    @staticmethod
    def from_json_file(json_filename=None):

        if json_filename is None:
            json_filename = "block_info.json"

        if os.path.isfile(json_filename):
            with open(json_filename, u'r') as f:
                block_info_json = json.load(f)
                return CodeBlock(**block_info_json)

        return None

    def __init__(self, rst_file, line_start, line_end, text, language, project,
                 main_file,
                 gnat_version,gnatprove_version,gprbuild_version,
                 compiler_switches, classes, manual_chop, buttons,
                 active=None,no_check=None,syntax_only=None,run_it=None,
                 compile_it=None,prove_it=None,
                 source_files=None,
                 project_filename=None,spark_project_filename=None,
                 project_main_file=None,
                 text_hash=None,text_hash_short=None):
        self.rst_file = rst_file
        self.line_start = line_start
        self.line_end = line_end
        self.text = text
        self.language = language
        self.project = project
        self.main_file = main_file
        self.gnat_version = gnat_version
        self.gnatprove_version = gnatprove_version
        self.gprbuild_version = gprbuild_version
        self.compiler_switches = compiler_switches
        self.classes = classes
        self.manual_chop = manual_chop
        self.buttons = buttons
        self.active = active if active is not None else True

        self.no_check = no_check if no_check is not None else \
            any(sphinx_class in ["ada-nocheck", "c-nocheck"]
                for sphinx_class in self.classes)

        self.syntax_only = syntax_only if syntax_only is not None else \
            'ada-syntax-only' in self.classes

        self.run_it = run_it if run_it is not None else \
            (('ada-run' in self.classes
              or 'ada-run-expect-failure' in self.classes
              or 'run' in self.buttons)
              and not 'ada-norun' in self.classes)
        self.compile_it = compile_it if compile_it is not None else \
            self.run_it or \
            (('ada-compile' in self.classes and self.language == 'ada')
             or ('c-compile' in self.classes and self.language == 'c')
             or 'compile' in self.buttons)

        prove_buttons = ["prove", "prove_flow", "prove_flow_report_all",
                         "prove_report_all"]
        prove_classes = ["ada-prove", "ada-prove-flow", "ada-prove-flow-report-all",
                         "ada-prove-report-all"]

        self.prove_it = prove_it if prove_it is not None else \
            (any(b in prove_classes for b in self.classes)
             or any(b in prove_buttons for b in self.buttons))

        self.source_files = source_files if source_files is not None else \
            list()
        self.project_filename = project_filename
        self.spark_project_filename = spark_project_filename
        self.project_main_file = project_main_file

        # Hash of source-code
        str_text = str(self.text).encode("utf-8")
        self.text_hash: str = text_hash if text_hash is not None else \
            hashlib.sha512(str_text).hexdigest()
        self.text_hash_short: str = text_hash_short if text_hash_short is not None else \
            hashlib.md5(str_text).hexdigest()


class ConfigBlock(Block):
    def __init__(self, **opts):
        self._opts = opts
        for k, v in opts.items():
            setattr(self, k, False if v == "False" else True)

    def update(self, other_config):
        self.__init__(**other_config._opts)
