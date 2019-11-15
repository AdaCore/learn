import logging
import re

logger = logging.getLogger(__name__)
lab_re = re.compile("(in|out) ?(\d+):(.*)")

class LabIO:

    def __init__(self, key):
        self.key = key
        self.input = None
        self.expected = None

    def get_key(self):
        return self.key

    def get_input(self):
        return self.input

    def set(self, io_str, val):
        if io_str == "in":
            self.set_input(val)
        elif io_str == "out":
            self.set_expected(val)
        else:
            raise Exception("Unknown string in lab io format")

    def set_input(self, val):
        self.input = val.split()

    def set_expected(self, val):
        self.expected = val

    def check_actual(self, actual, code):
        self.actual = actual.replace('\n', ' ').replace('\r', ' ').rstrip()
        self.code = code

        if self.expected == self.actual:
            self.status = "Success"
            return True
        else:
            self.status = "Failed"
            return False


    def get_result(self):
        return {
        "in": self.input,
        "out": self.expected,
        "actual": self.actual,
        "status": self.status
        }


class LabList:

    def __init__(self, file):
        self.test_cases = []

        logger.debug("LAB IO {}".format(file))
        for line in file.splitlines():
            match = lab_re.match(line)

            if match:
                logger.debug("LAB IO re match {}".format(match.groups()))
                io = match.group(1)
                key = match.group(2)
                seq = match.group(3)

                case = next((x for x in self.test_cases if x.get_key() == key), None)

                if case:
                    case.set(io, seq)
                else:
                    lab = LabIO(key)
                    lab.set(io, seq)
                    self.test_cases.append(lab)

    def loop(self):
        for I in self.test_cases:
            yield I

    def get_results(self):
        results = {}

        for t in self.test_cases:
            results[t.get_key()] = t.get_result()

        return results
