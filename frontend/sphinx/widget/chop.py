import codecs
from enum import Enum, auto
import os
import re
import shutil
import subprocess
import tempfile
from typing import List, Optional

from .resource import Resource


class ChopStrategy(Enum):
    """Represents a chop strategy

    These correspond to the chop functions defined in this file. A Widget uses
    these based on the file type it is processing.
    """
    MANUAL = auto()
    CHEAPO = auto()
    REAL = auto()


def manual_chop(lines: List[str]) -> List[Resource]:
    """Manually chops the text of a C or Ada source-code file

    This function assumes that the filename is being provided as the first line
    in the given form:

    !<filename>.c|.h|.adb|.ads

    Args:
        lines (List[str]): The lines of the source file to process

    Returns:
        List[Resource]: Returns a list of Resources after chopping
    """
    results: List[Resource] = []
    re_fn = re.compile(r"\!(.+\.(?:c|h|adb|ads))")

    for j in lines:
        match = re_fn.match(j)
        if match:
            # we found a new file
            r = Resource(match.group(1))
            results.append(r)
        elif results:
            # we append this line to the last file we found
            results[-1].append(j)
        else:
            # we found content before finding a file. its garbage
            continue

    return results


def cheapo_gnatchop(lines: List[str]) -> List[Resource]:
    """Chops the text of files using regex instead of gnatchop

    Args:
        lines (List[str]): The lines of the source file to process

    Returns:
        List[Resource]: Returns a list of Resources after chopping
    """
    results = []
    body = re.compile("^(procedure|package body) ([^ ]+)")
    spec = re.compile("^(package) ([^ ]+)")

    def to_base_filename(g):
        return g.lower().replace('.', '-')

    for j in lines:
        bodyMatch = body.match(j)
        specMatch = spec.match(j)
        if bodyMatch:
            # we found a new body file
            newBasename = to_base_filename(bodyMatch.group(2)) + ".adb"
            results.append(Resource(newBasename))
            results[-1].append(j)
        elif specMatch:
            # we found a new spec file
            newBasename = to_base_filename(specMatch.group(2)) + ".ads"
            results.append(Resource(newBasename))
            results[-1].append(j)
        elif results:
            # we append this line to the last file we found
            results[-1].append(j)
        else:
            # we found content before finding a file. its garbage
            continue

    return results


def real_gnatchop(lines: List[str], compiler_switches: Optional[dict] = None) -> List[Resource]:
    """Uses gnatchop to chop the text into files

    Args:
        lines (List[str]): The lines of the source file to process

    Raises:
        Exception: If an error occurs during gnatchop

    Returns:
        List[Resource]: Returns a list of Resources after chopping
    """
    results: List[Resource] = []
    wd = tempfile.mkdtemp()
    try:
        # write string contents to file on temp fs
        gnatchop_file = os.path.join(wd, 'internal_gnatchop.txt')
        with codecs.open(gnatchop_file, 'wb', encoding='utf-8') as f:
            f.write('\n'.join(lines))

        # run gnatchop on temp file
        if compiler_switches is None:
            cmd = ['gnatchop', gnatchop_file]
        else:
            cmd = ['gnatchop']
            for sw in compiler_switches:
                # gnatchop only accepts `-gnatXXX` switches
                if "gnat" in sw:
                    cmd.append(sw)
            cmd.append(gnatchop_file)
        output = subprocess.check_output(cmd, cwd=wd)
        files = [os.path.join(wd, f.decode("utf-8").strip()) for f in output.splitlines()
                 if not f.startswith(b'splitting ')]
        os.remove(gnatchop_file)

        # loop over resulting files and create Resource for each
        for file in files:
            with codecs.open(file, 'rb', encoding='utf-8') as f:
                basename = os.path.basename(file)
                content = f.read().strip().splitlines()
                results.append(Resource(basename, content))
    except subprocess.CalledProcessError:
        print('======== gnatchop error ========\n')
        for num, line in enumerate(lines, 1):
            print("{line_num:4d} | {line}".format(
                  line_num=num,
                  line=line))
        print('\n================================')
        raise Exception('Could not chop files with gnatchop')
    finally:
        # remove temp fs
        shutil.rmtree(wd)

    return results
