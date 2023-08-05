from itertools import count
import re
from typing import List, Match, Dict

from .button import Button
from .chop import manual_chop, cheapo_gnatchop, real_gnatchop, ChopStrategy
from .resource import Resource

class ButtonException(Exception):
    pass


class NameException(Exception):
    pass


class ChopException(Exception):
    pass


class LabException(Exception):
    pass


class FileException(Exception):
    pass


class MainException(Exception):
    pass


class Widget:
    """The Widget class defines the layout of a widget

    This class will get passed to a jinja template. All instance variables
    and properties will be accessed via jinja template.

    The count class variable allows widgets to have a unique id on the rendered
    page.
    """
    __count = count(0)

    def __init__(self):
        """Widget constructor
        """
        self.shadow_files: List[Resource] = []
        self.is_lab: bool = False
        self.id = next(self.__count)

        self.switches: Dict[str, List[str]] = {}
        self.__files: List[Resource] = []
        self.__button_group: List[Button] = []
        self.__name: str = None
        self.__no_button: bool = False
        self.__chop_strategy: ChopStrategy = None
        self.__main = None
        self.allowed_switches: Dict[str, List[str]] = {
            "Builder": ["-g"],
            "Compiler": [
                "-g",
                "-O0",
                "-gnata",
                "-gnatW8",
                "-gnatwa",
                "-gnato",
                "-gnato0",
                "-gnato11",
                "-gnato21",
                "-gnato22",
                "-gnato23",
                "-gnateE",
                "-gnatX",
            ],
        }
        self.switch_description = {
            "-g" : "Optimize for debug",
            "-O0" : "No optimizations",
            "-gnata" : "Assertions enabled",
            "-gnatW8" : "UTF-8 encoding of source-code files",
            "-gnatwa" : "Activate most optional warnings",
            "-gnatyg0-s" : "Check default code style",
            "-gnatyM50" : "Check maximum line length of 50 characters",
            "-gnatyM80" : "Check maximum line length of 80 characters",
            "-gnato" : "Enable numeric overflow checking",
            "-gnato0" : "Suppresses overflow checking",
            "-gnato11" : "Handling of intermediate overflow: strict mode",
            "-gnato21" : "Handling of intermediate overflow: strict mode; minimized mode within assertions, pre/postconditions, and type invariants",
            "-gnato22" : "Handling of intermediate overflow: minimized mode",
            "-gnato23" : "Handling of intermediate overflow: minimized mode; eliminated mode within assertions, pre/postconditions, and type invariants",
            "-gnateE" : "Generate extra information in exception messages",
            "-gnatX" : "Enable GNAT implementation extensions and latest Ada version",
        }


    @property
    def button_group(self) -> List[Button]:
        """Property for button group access

        Raises:
            Exception: If no buttons are specified

        Returns:
            List[Button]: The list of buttons
        """
        if self.__no_button:
            return []
        else:
            if not self.__button_group:
                raise ButtonException('No buttons specified on widget')
            return self.__button_group

    @property
    def name(self) -> str:
        """Property for name access

        Raises:
            Exception: If no name is specified

        Returns:
            str: The name
        """
        if self.__name:
            return self.__name
        else:
            raise NameException('No name specified on widget')

    @property
    def files(self) -> List[Resource]:
        """Property for file access

        Raises:
            Exception: If no files are present

        Returns:
            List[Resource]: The list of files
        """
        if self.__files:
            return self.__files
        else:
            raise FileException('No files present on widget')

    @property
    def main(self) -> str:
        """Property for main access

        Raises:
            Exception: If specified main doesn't exist

        Returns:
            str: The main or empty string if not specified
        """
        if not self.__main:
            return ""

        if any(f.basename == self.__main for f in self.__files):
            return self.__main
        else:
            raise MainException('Specified main does not exist')


    def __parseButton(self, btn: str):
        """Parse a button specified in a Directive arg

        Args:
            btn (str): [description]
        """
        if btn == 'no_button':
            self.__no_button = True
        else:
            self.__button_group.append(Button(btn))

    def __parseName(self, match: Match):
        """Parse the matched name from Directive args

        Args:
            match (Match): The regex match from Directive args
        """
        if match.group(1) == 'lab':
            # this widget is a lab
            self.is_lab = True
            self.__parseButton('submit_button')
        else:
            # this widget is not a lab
            self.is_lab = False
        self.__name = match.group(2)

    def __parseSwitches(self, arg: str):
        """Parses switches from Directive args

        Args:
            arg (str): The switches to parse
        """
        sw_str = arg.split('=')[1]
        sw_types = sw_str.split(';')
        for t in sw_types:
            regex = re.compile(r"(.+)\((.+)\)")
            match = regex.search(t)
            if match:
                pkg = match.group(1)
                sw = match.group(2).split(',')
                if pkg in self.switches.keys():
                    self.switches[pkg].extend(sw)
                else:
                    self.switches[pkg] = sw

    def __parseLabIO(self, content: List[str]) -> List[str]:
        """Parses lab io data from Directive content

        Args:
            content (List[str]): The Directive content to parse

        Raises:
            Exception: If a duplicate start block is found
            Exception: If an end block is found before a start block
            Exception: If no end block is found

        Returns:
            List[str]: The content with the lab io removed
        """
        result: List[str] = []
        start_re = re.compile(r"--  START LAB IO BLOCK")
        end_re = re.compile(r"--  END LAB IO BLOCK")
        labio_filename = "lab_io.txt"

        lab_resource: Resource = None

        i = 0
        while i < len(content):
            j = content[i]
            start_match = start_re.match(j)
            end_match = end_re.match(j)

            if start_match:
                # we found a start block
                if lab_resource:
                    # this must be a duplicate start block
                    raise LabException('Duplicate start block found in Lab io')
                else:
                    lab_resource = Resource(labio_filename)
            elif end_match:
                # we found an end block
                if lab_resource:
                    self.shadow_files.append(lab_resource)
                    # add the rest of the string to the result and exit
                    result.extend(content[i + 1 :])
                    return result
                else:
                    # this must be an end before a start
                    raise LabException('End block found before start in Lab io')
            elif lab_resource:
                # this must be lab io data
                lab_resource.append(j)
            else:
                # this must be data before the start block
                result.append(j)
            i += 1

        # if we reach here, we never found an end block
        raise LabException('No end block found before start in Lab IO')

    def parseArgs(self, args: List[str]):
        """Parses Directive arguments

        Args:
            args (List[str]): The list of arguments

        Raises:
            ValueError: If an invalid argument is specified
        """
        name_re = re.compile(r'(lab|project)=(\S+)')

        for arg in args:
            nameMatch = name_re.match(arg)
            if arg.endswith('_button'):
                # this is a button argument
                self.__parseButton(arg)
            elif nameMatch:
                # this is the name argument
                self.__parseName(nameMatch)
            elif arg == 'manual_chop':
                self.__chop_strategy = ChopStrategy.MANUAL
            elif arg == 'c':
                self.__chop_strategy = ChopStrategy.MANUAL
            elif arg == 'ada':
                self.__chop_strategy = ChopStrategy.REAL
            elif arg.startswith('switches='):
                self.__parseSwitches(arg)
            elif arg.startswith('main='):
                self.__main = arg.split('=')[1]
            elif arg == 'cli_input':
                # TODO: implement this feature
                pass
            else:
                raise ValueError(f'Invalid argument: {arg}')

    def parseOpts(self, opts: List[str]):
        """Parses Directive options

        Args:
            opts (List[str]): The list of options

        Raises:
            ValueError: If an invalid option is specified
        """
        if 'class' in opts:
            for opt in opts['class']:
                if opt in ['ada-nocheck', 'c-nocheck']:
                    self.__no_button = True
                elif opt == 'nosyntax-check':
                    # this is for testing, nothing to do here
                    continue
                elif opt == 'ada-syntax-only':
                    self.__no_button = True
                elif opt in ['ada-expect-compile-error',
                             'c-expect-compile-error']:
                    # this is for testing, nothing to do here
                    continue
                elif opt == 'ada-expect-prove-error':
                    # this is for testing, nothing to do here
                    continue
                elif opt in ['ada-run', 'c-run']:
                    # this is for testing, nothing to do here
                    continue
                elif opt in ['ada-norun', 'c-norun']:
                    # this is for testing, nothing to do here
                    continue
                elif opt in ['ada-run-expect-failure',
                             'c-run-expect-failure']:
                    # this is for testing, nothing to do here
                    continue
                else:
                    raise ValueError(f'Invalid option: {opt}')

    def parseContent(self, content: List[str]):
        """Parse Directive content

        Args:
            content (List[str]): The list of string that make up the content
        """
        if self.is_lab:
            content = self.__parseLabIO(content)

        if self.__chop_strategy is ChopStrategy.MANUAL:
            self.__files = manual_chop(content)
        elif self.__chop_strategy is ChopStrategy.CHEAPO:
            self.__files = cheapo_gnatchop(content)
        elif self.__chop_strategy is ChopStrategy.REAL:
            self.__files = real_gnatchop(content)
        else:
            raise ChopException('No chop strategy defined')

    def __parse_code_block_info(self, code_block_info: Dict[str, str]) -> List[Resource]:
        results: List[Resource] = []

        for info_type in code_block_info:
            name = info_type + " info"
            contents = code_block_info[info_type].strip().splitlines()
            contents += "\n"
            results.append(Resource(name, contents))

        return results


    def parseCodeBlockInfo(self, content: Dict[str, str]):
        """Parse Directive code block info

        Args:
            content (Dict[str, str]): The dictionary with code block info
        """
        self.code_block_info = self.__parse_code_block_info(content)
