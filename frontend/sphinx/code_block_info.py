"""Provide compiler/runtime information for code block.
"""

from typing import List, Dict
import glob

import os
import json

class CodeBlockInfo():
    """Code block info class
    """

    # Hard-coding directories:
    # - build directory
    # - base project directory
    base_project_dir = "projects"

    def __init__(self,
                 project_name : str,
                 filename : str,
                 line_number: int):
        """Widget constructor
        """
        self.__project_name: str = project_name
        self.__filename: str = filename
        self.__line_number: int = line_number
        self.__src_test_data_dir = ""
        self.__data_available = False

        if 'SRC_TEST_DIR' in os.environ:
            self.__src_test_data_dir = os.environ['SRC_TEST_DIR']
            self.__data_available = True

    def __get_project_dir(self) -> str:
        return self.__project_name.replace(".", "/")

    def __get_code_block_dir(self) -> str:
        return (self.__src_test_data_dir + "/" +
                self.base_project_dir + "/" +
                self.__get_project_dir() + "/" +
                str(self.__line_number))

    def get_info(self) -> Dict[str, str]:
        """
        """
        info : Dict[str, str] = {}

        if not self.__data_available:
            return info

        code_block_dir = self.__get_code_block_dir()

        for logfile in glob.glob(code_block_dir + "/*.log"):
            with open(logfile) as f:
                content = f.read()
                log_type = os.path.splitext(os.path.basename(logfile))[0]
                info[log_type] = content

        block_info_json_file = code_block_dir + "/block_info.json"
        if os.path.isfile(block_info_json_file):
            with open(block_info_json_file, u'r') as f:
                block_info_json = json.load(f)
                info['_metadata'] = block_info_json
        else:
            info['_metadata'] = None
        return info
