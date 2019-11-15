import logging
import os
import re

logger = logging.getLogger(__name__)

RECEIVED_FILE_CHAR_LIMIT = 50 * 1000
# The limit in number of characters of files to accept

def new_file(basename, content):
    if len(content) > RECEIVED_FILE_CHAR_LIMIT:
        raise Exception("File {} exceeds size limits".format(basename))

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
    mains = []
    for f in filelist:
        logger.debug("Checking {} for main".format(f.get_name()))
        if f.language() == "Ada":
            filename = f.get_name()
            base, ext = os.path.splitext(filename)
            if ext == ".adb":
                logger.debug("Looking for spec for {}".format(f.get_name()))
                if not next((x for x in filelist if x.get_name() == (base + ".ads")), None):
                    logger.debug("Found main in {}".format(f.get_name()))
                    mains.append(filename)
        else:
            if f.is_main():
                mains.append(f.get_name())
                logger.debug("Found main in {}".format(f.get_name()))
    return mains


class File:

    def __init__(self, basename, content):
        self.basename = basename
        self.content = content

    def get_name(self):
        return self.basename

    def get_content(self):
        return self.content

    def language(self):
        return None

    def is_main(self):
        return False


class AdaFile(File):
    procedure_re = re.compile("^procedure +[A-Za-z][_a-zA-Z0-9]*[ |\n]+(is|with)", re.MULTILINE)

    def language(self):
        return "Ada"


class CFile(File):
    main_re = re.compile("^(?:void|int) +main\(.*\)(?: |\n)*{", re.MULTILINE)

    def is_main(self):
        return main_re.findall(self.contents)

    def language(self):
        return "c"


class CPPFile(CFile):

    def language(self):
        return "c++"


class ProjectFile(File):

    def insert_languages(self, languages):
        to_insert = "for Languages use ({});".format(", ".join(['"{}"'.format(x) for x in languages]))
        self.content = self.content.replace("--LANGUAGE_PLACEHOLDER--", to_insert)

    def define_mains(self, mains):
        to_insert = "for Main use ({});".format(", ".join(['"{}"'.format(x) for x in mains]))
        self.content = self.content.replace("--MAIN_PLACEHOLDER--", to_insert)
