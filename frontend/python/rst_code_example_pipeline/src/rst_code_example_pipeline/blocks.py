from __future__ import annotations

import os
import re
import hashlib
import json
from typing import Any

from . import colors as C
from . import constants
from . import toolchain_info

class Block(object):
    @staticmethod
    def get_blocks_from_rst(rst_file: str, input_text: str) -> list[Block]:
        lang_re = re.compile(r"\s*.. code::\s*(\w+)?\s*")
        project_re = re.compile(r"\s*.. code::.*project=(\S+)?")
        main_re = re.compile(r"\s*.. code::.*main=(\S+)?")
        manual_chop_re = re.compile(r"\s*.. code::.*manual_chop?")
        button_re = re.compile(r"\s+(\S+)_button")
        code_config_re = re.compile(r":code-config:`(.*)?`")
        classes_re = re.compile(r"\s*:class:\s*(.+)")
        switches_re = re.compile(r"\s*.. code::.*switches=(\S+)?")
        compiler_switches_re = re.compile(r"Compiler[(](\S+)?[)]")
        gnat_version_re=re.compile(r"\s*.. code::.*gnat=(\S+)?")
        gnatprove_version_re=re.compile(r"\s*.. code::.*gnatprove=(\S+)?")
        gprbuild_version_re=re.compile(r"\s*.. code::.*gprbuild=(\S+)?")

        blocks: list[Block] = []
        lines = input_text.splitlines()

        def first_nonws(line):
            for i, c in enumerate(line):
                if not c.isspace():
                    return i
            return 0

        indents = map(first_nonws, lines)

        classes: list[str] = []
        compiler_switches: list[str] = []
        buttons: list[str] = []
        cb_start = -1
        cb_indent = -1
        lang = ""
        project: str | None = None
        main_file: str | None = None
        manual_chop: bool | None = None
        gnat_version: list[str] | None = None
        gnatprove_version: list[str] | None = None
        gprbuild_version: list[str] | None = None
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

                assert gnat_version is not None
                assert gnatprove_version is not None
                assert gprbuild_version is not None
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

            lang_match = lang_re.match(line)
            assert lang_match is not None
            cb_start, lang = (i + 1, lang_match.groups()[0])
            project_match = project_re.match(line)
            project = project_match.groups()[0] if project_match is not None else None

            main_file_match = main_re.match(line)
            # Retrieve actual main filename
            main_file = main_file_match.groups()[0] if main_file_match is not None else None
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
                all_switches_str = all_switches.groups()[0]
                compiler_switches_match = compiler_switches_re.match(all_switches_str)
                if compiler_switches_match is not None:
                    compiler_switches = [str.strip(l)
                        for l in compiler_switches_match.groups()[0].split(",")]

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
            blocks.append(ConfigBlock(
                rst_file,
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

    def to_json_file(self, json_filename: str | None = None) -> None:
        block_info = vars(self)

        if json_filename is None:
            json_filename = constants.BLOCK_INFO_FILENAME
        with open(json_filename, u'w') as f:
            json.dump(block_info, f, indent=4)

class CodeBlock(Block):
    """A single code block extracted from a ReST file

    Note:
        ``text_hash`` and ``text_hash_short`` are derived from the block's
        text whenever the constructor is not handed them. What this package
        asks of them is exactly three things:

        * **determinism** -- the same text hashes the same way in every run,
          or a block's directory moves and the result cached in it is never
          found again;
        * **distinctness** -- two different texts do not collide, or one
          block's extracted project overwrites another's and one of the two
          silently stops being checked;
        * **hexadecimal shape** -- the short hash is used verbatim as a
          directory name, so it must hold nothing a path would have to
          escape.

        What this package does **not** ask of them is any particular digest.
        Neither hash is compared against a value computed anywhere else in
        the package, so SHA-512 and MD5 are a choice made here, not a
        promise made to a caller. Tests belong on the three properties above
        and never on a literal digest: pinning one turns a correct change of
        algorithm into a test failure, which is the opposite of what such a
        test is for.

        One constraint does come from outside the package, and it is easy to
        miss because nothing fails loudly when it is broken:
        ``frontend/sphinx/widget_extension.py`` recomputes the same MD5 over
        the same block text and uses it to locate the per-block directory
        whose log files it renders beside the example. Change the algorithm
        on one side only and the boxes simply come out empty. The two sides
        have to move together.
    """

    @staticmethod
    def from_json_file(json_filename: str | None = None) -> CodeBlock | None:

        if json_filename is None:
            json_filename = constants.BLOCK_INFO_FILENAME

        if os.path.isfile(json_filename):
            with open(json_filename, u'r') as f:
                try:
                    block_info_json = json.load(f)
                    return CodeBlock(**block_info_json)
                except (json.JSONDecodeError, UnicodeDecodeError,
                        TypeError) as e:
                    # A file that is present but cannot be turned into a
                    # block is reported and treated as no block at all.  The
                    # callers already say what that means for them; only the
                    # reason is known here, and it is the part that would
                    # otherwise be lost.
                    #
                    # UnicodeDecodeError is listed separately on purpose: it
                    # is a *sibling* of JSONDecodeError under ValueError, not
                    # a subclass, so a record holding bytes that are not
                    # valid UTF-8 would otherwise escape -- and a hand edit
                    # in an editor defaulting to another encoding produces
                    # exactly that.
                    print("{}: cannot read block info from {}: {}".format(
                        C.col("ERROR", C.Colors.RED), json_filename, e))

        return None

    def __init__(self,
                 rst_file: str,
                 line_start: int,
                 line_end: int,
                 text: str,
                 language: str,
                 project: str | None,
                 main_file: str | None,
                 gnat_version: list[str],
                 gnatprove_version: list[str],
                 gprbuild_version: list[str],
                 compiler_switches: list[str],
                 classes: list[str],
                 manual_chop: bool | None,
                 buttons: list[str],
                 active: bool | None = None,
                 no_check: bool | None = None,
                 syntax_only: bool | None = None,
                 run_it: bool | None = None,
                 compile_it: bool | None = None,
                 prove_it: bool | None = None,
                 source_files: list[str] | None = None,
                 project_filename: str | None = None,
                 spark_project_filename: str | None = None,
                 project_main_file: str | None = None,
                 text_hash: str | None = None,
                 text_hash_short: str | None = None) -> None:
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
        self.active: bool = active if active is not None else True

        self.no_check: bool = no_check if no_check is not None else \
            any(sphinx_class in [constants.CLASS_ADA_NOCHECK, constants.CLASS_C_NOCHECK]
                for sphinx_class in self.classes)

        self.syntax_only: bool = syntax_only if syntax_only is not None else \
            constants.CLASS_ADA_SYNTAX_ONLY in self.classes

        # The C spellings are paired with the language the way compile_it
        # pairs its own, so that asking for a run by class alone works for C
        # as it already does for Ada.  Without them a c-run block was never
        # run and the check still reported success, and the branch handling
        # c-run-expect-failure could only be reached through a run button.
        self.run_it: bool = run_it if run_it is not None else \
            ((constants.CLASS_ADA_RUN in self.classes
              or constants.CLASS_ADA_RUN_EXPECT_FAILURE in self.classes
              or ((constants.CLASS_C_RUN in self.classes
                   or constants.CLASS_C_RUN_EXPECT_FAILURE in self.classes)
                  and self.language == 'c')
              or 'run' in self.buttons)
              and not constants.CLASS_ADA_NORUN in self.classes
              and not constants.CLASS_C_NORUN in self.classes)
        self.compile_it: bool = compile_it if compile_it is not None else \
            self.run_it or \
            ((constants.CLASS_ADA_COMPILE in self.classes and self.language == 'ada')
             or (constants.CLASS_C_COMPILE in self.classes and self.language == 'c')
             or 'compile' in self.buttons)

        prove_buttons: list[str] = ["prove", "prove_flow", "prove_flow_report_all",
                         "prove_report_all"]

        self.prove_it: bool = prove_it if prove_it is not None else \
            (any(b in constants.PROVE_CLASSES for b in self.classes)
             or any(b in prove_buttons for b in self.buttons))

        self.source_files: list[str] = source_files if source_files is not None else \
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
    def __init__(self,
                 rst_file: str | None = None,
                 **opts: Any) -> None:
        self.rst_file: str | None = rst_file
        self._opts: dict[str, Any] = opts
        for k, v in opts.items():
            # Values normally arrive as strings from a code-config directive,
            # where only "False" means false.  A caller passing a real
            # boolean means it literally, so pass it through instead of
            # comparing it against a string it can never equal.
            setattr(self, k, v if isinstance(v, bool) else v != "False")

    def update(self, other_config: ConfigBlock) -> None:
        self.__init__(**other_config._opts)
