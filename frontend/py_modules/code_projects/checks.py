import os
import re
import hashlib
import json
import time

class CodeCheck(object):
    def __init__(self, timestamp=None,
                 version=None, status_ok=None,
                 logfile=None, cmdline=None):

        self.version = version
        self.status_ok = status_ok
        self.logfile = logfile
        self.cmdline = cmdline
        self.timestamp = timestamp if timestamp is not None else time.time()


class BlockCheck(object):
    @staticmethod
    def from_json_file(json_filename=None):

        if json_filename is None:
            json_filename = "block_checks.json"

        if os.path.isfile(json_filename):
            with open(json_filename, u'r') as f:
                block_checks_json = json.load(f)
                return BlockCheck(**block_checks_json)

        return None

    def __init__(self, text_hash, text_hash_short,
                 timestamp=None,
                 status_ok=None,
                 checks=None):

        self.text_hash: str = text_hash
        self.text_hash_short: str = text_hash_short
        self.timestamp = timestamp if timestamp is not None else time.time()
        self.status_ok = status_ok
        self.checks = dict()

    def to_json_file(self, json_filename=None):
        block_checks = self.__dict__

        if json_filename is None:
            json_filename = "block_checks.json"
        with open(json_filename, u'w') as f:
            json.dump(block_checks, f, indent=4, default=lambda __o: __o.__dict__)

    def add_check(self, check_type : str, code_check : CodeCheck):
        self.checks[check_type] = code_check
