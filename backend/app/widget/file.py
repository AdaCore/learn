import os
import re

from celery.utils.log import get_task_logger

logger = get_task_logger(__name__)


RECEIVED_FILE_CHAR_LIMIT = 50 * 1000
# The limit in number of characters of files to accept


def new_file(basename, content):
    """
    The method creates a new File object or derived object from File based on the basename file extension. This should
    be used to create Files instead of using the File object constructors. This also ensures that the file is not longer
    than the file char limit.
    :param basename:
        The file name
    :param content:
        The file content
    :return:
        Returns a File or derived File object
    """
    if len(content) > RECEIVED_FILE_CHAR_LIMIT:
        raise Exception(f"File {basename} exceeds size limits")

    fn, ext = os.path.splitext(basename)
    if ext == ".adb" or ext == ".ads":
        return AdaFile(basename, content)
    elif ext == ".c" or ext == ".h":
        return CFile(basename, content)
    elif ext == ".cpp" or ext == ".hh":
        return CPPFile(basename, content)
    elif ext == ".gpr":
        return ProjectFile(basename, content)
    else:
        return File(basename, content)


def find_mains(filelist):
    """
    This checks a list of files to find files that can be considered mains. For Ada files, the criteria is that the
    adb file does not have a corresponding ads file. For C files, we use the CFile.is_main() method.
    :param filelist:
        The list of files to check for mains
    :return:
        The list of files that have mains
    """
    mains = []
    for f in filelist:
        logger.debug(f"Checking {f.get_name()} for main")
        if f.language() == "Ada":
            filename = f.get_name()
            base, ext = os.path.splitext(filename)
            if ext == ".adb":
                logger.debug(f"Looking for spec for {f.get_name()}")
                if not next((x for x in filelist if x.get_name() == (base + ".ads")), None):
                    logger.debug(f"Found main in {f.get_name()}")
                    mains.append(filename)
        else:
            if f.is_main():
                mains.append(f.get_name())
                logger.debug(f"Found main in {f.get_name()}")
    return mains


class File:
    """
    This is the base File class used to represent generic Files.

    Attributes
    ----------
    basename : str
        The file name
    content : str
        the name of the animal

    Methods
    -------
    get_name()
        Returns the name of the file
    get_content()
        Returns the content of the file
    language()
        Returns the coding language for the file is any
    is_main()
        Checks if the file is a main
    """
    def __init__(self, basename, content):
        """
        Constructor for File. THIS SHOULD NOT BE CALLED DIRECTLY!! Use new_file method instead.
        :param basename:
            File name
        :param content:
            File content
        """
        self.basename = basename
        self.content = content

    def get_name(self):
        """
        Returns the name of the file
        :return:
            The file name
        """
        return self.basename

    def get_content(self):
        """
        Returns the content of the file
        :return:
            The file content
        """
        return self.content

    def language(self):
        """
        Returns the language for the file
        :return:
            Returns the file language or None
        """
        return None

    def is_main(self):
        """
        Returns if the file is/has a main. Valid for C or CPP only now.
        :return:
            Returns True if the file has/is a main
        """
        return False


class AdaFile(File):
    """
    Class for an Ada file. Inherits from File.

    Attributes
    ----------
    basename : str
        The file name
    content : str
        the name of the animal

    Methods
    -------
    get_name()
        Returns the name of the file
    get_content()
        Returns the content of the file
    language()
        Returns the coding language for the file is any
    is_main()
        Checks if the file is a main
    """
    def is_main(self):
        """
        This should check if the Ada file is a main. This is unimplemented and shouldn't be used
        """
        # TODO: figure out how to do this
        raise NotImplementedError

    def language(self):
        """
        Returns "Ada"
        :return:
            The language string
        """
        return "Ada"


class CFile(File):
    """
    Class for a C file. Inherits from File.

    Attributes
    ----------
    basename : str
        The file name
    content : str
        the name of the animal

    Methods
    -------
    get_name()
        Returns the name of the file
    get_content()
        Returns the content of the file
    language()
        Returns the coding language for the file is any
    is_main()
        Checks if the file is a main
    """
    def is_main(self):
        """
        Uses a regex to compute if the C file has the right function layout/name for a main
        :return:
            True if the regex matches
        """
        main_re = re.compile("^(?:void|int) +main\(.*\)(?: |\n)*{", re.MULTILINE)
        return main_re.findall(self.content)

    def language(self):
        """
        Returns "c"
        :return:
            The language string
        """
        return "c"


class CPPFile(CFile):
    """
    Class for a CPP file. Inherits from CFile.

    Attributes
    ----------
    basename : str
        The file name
    content : str
        the name of the animal

    Methods
    -------
    get_name()
        Returns the name of the file
    get_content()
        Returns the content of the file
    language()
        Returns the coding language for the file is any
    is_main()
        Checks if the file is a main
    """
    def language(self):
        """
        Returns "c++"
        :return:
            The language string
        """
        return "c++"


class ProjectFile(File):
    """
    Class for a Project file. Inherits from File.

    Attributes
    ----------
    basename : str
        The file name
    content : str
        the name of the animal
    allowed_switches : dict
        the list of allowed switch to apply to gpr packages

    Methods
    -------
    get_name()
        Returns the name of the file
    get_content()
        Returns the content of the file
    language()
        Returns the coding language for the file is any
    is_main()
        Checks if the file is a main
    insert_language(languages)
        Inserts the languages for the project into the project file
    define_mains(mains)
        Inserts the mains for the project into the project file
    """

    allowed_switches = {
        'Builder': ['-g'],
        'Compiler': ['-g', '-O0', '-gnata', '-gnatwa', '-gnato', '-gnato0', '-gnato11', '-gnato23',
                     '-gnato21', '-gnato22']
    }

    def insert_languages(self, languages):
        """
        Inserts languages into the correct place in the project file
        :param languages:
            The list of languages to add to the project
        """
        lang_list = [f'"{x}"' for x in languages]
        to_insert = f"for Languages use ({', '.join(lang_list)});"
        self.content = self.content.replace("--LANGUAGE_PLACEHOLDER--", to_insert)

    def define_mains(self, mains):
        """
        Inserts the mains into the correct place in the project file
        :param mains:
            The list of mains to add to the project
        """
        main_list = [f'"{x}"' for x in mains]
        to_insert = f"for Main use ({', '.join(main_list)});"
        self.content = self.content.replace("--MAIN_PLACEHOLDER--", to_insert)

    def insert_switches(self, switch_list):
        sw_dict = {}

        regex = re.compile(r'(Builder|Compiler)\((.+)\)')
        for sec in switch_list:
            match = regex.search(sec)
            if match:
                pkg_name = match.group(1)
                switches = set(match.group(2).split(','))
                if pkg_name in sw_dict.keys():
                    sw_dict[pkg_name] = sw_dict[pkg_name] | switches
                else:
                    sw_dict[pkg_name] = switches

        for pkg, unfiltered_switches in sw_dict.items():
            filtered_switches = []
            for switch in unfiltered_switches:
                if switch in self.allowed_switches[pkg]:
                    filtered_switches.append('"' + switch + '"')
                else:
                    logger.error(f"Illegal switch requested in pkg {pkg}: {switch}")
            if filtered_switches:
                placeholder_str = "--" + pkg.upper() + "_SWITCHES_PLACEHOLDER--"
                switches_str = ', '.join(filtered_switches)
                line_str = f'for Switches ("Ada") use ({switches_str});'
                logger.debug(f"Adding line {line_str} to pkg {pkg}")
                self.content = self.content.replace(placeholder_str, line_str)



