import logging
import re


logger = logging.getLogger(__name__)


class LabIO:
    """
    This class is used to represent the input data and the expected output data for a lab test case. The utility method
    check_actual is used to check the user code output with the expected output.

    Attributes
    ----------
    key
        The lab test case number
    input
        The input for the test case that will be provided to the lab code
    expected
        The expected output for the test case that will be used to check the correctness of the users code
    actual
        The actual output from the users code for the test case
    code
        Represents the return code for the test case. This attribute is currently not used
    status
        The success or fail state of the test case

    Methods
    -------
    get_key()
        Returns the lab test case key
    get_input()
        Returns the test case input
    set(io_str, val)
        Sets the correct attribute as described by io_str with the value val
    set_input(val)
        Sets the input attribute
    set_expected(val)
        Sets the expected attribute
    check_actual(actual, code)
        Checks the actual output from the test case with the expected output of the test case
    get_result()
        Returns the data for the lab as a dict
    """
    def __init__(self, key):
        """
        Constructs the LabIO
        :param key:
            The test case key
        """
        self.key = key
        self.input = None
        self.expected = None
        self.actual = None
        self.code = None
        self.status = None

    def get_key(self):
        """
        Returns the test case key
        :return:
            The key attribute
        """
        return self.key

    def get_input(self):
        """
        Returns the input for the test case
        :return:
            The input attribute
        """
        return self.input

    def set(self, io_str, val):
        """
        Sets the attribute corresponding to io_str with val
        :param io_str:
            "in" -> input attribute
            "out" -> expected attribute
        :param val:
            The value to store
        """
        if io_str == "in":
            self.set_input(val)
        elif io_str == "out":
            self.set_expected(val)
        else:
            raise Exception("Unknown string in lab io format")

    def set_input(self, val):
        """
        Sets the input attribute
        :param val:
            The value to store
        """
        self.input = val.split()

    def set_expected(self, val):
        """
        Sets the expected attribute
        :param val:
            The value to store
        """
        self.expected = val

    def check_actual(self, actual, code):
        """
        Checks the actual output from the test case against the expected attribute
        :param actual:
            The output from the users code with the test case input
        :param code:
            The exit code from the test case
        :return:
            Returns actual == expected
        """
        # We need to strip off newlines and replace them with spaces to match the formatting of the expected string
        self.actual = actual.replace('\n', ' ').replace('\r', ' ').rstrip()
        self.code = code

        if self.expected == self.actual:
            self.status = "Success"
            return True
        else:
            self.status = "Failed"
            return False

    def get_result(self):
        """
        Returns the results from the test case as a dict
        :return:
            The dict version of the test case
        """
        return {
            "in": self.input,
            "out": self.expected,
            "actual": self.actual,
            "status": self.status
        }


class LabList:
    """
    This class is used to represent the list of test cases for a lab.

    Attributes
    ----------
    test_cases
        The list of LabIO test cases

    Methods
    -------
    loop()
        This coroutine method loops over the list of test cases and yields each test case back to the caller
    get_results()
        Returns the results of the test cases for the lab
    """
    def __init__(self, file):
        """
        Constructor for the LabList class. Processes the lab description file and creates a list of test cases
        :param file:
            The lab description file to process
        """
        self.test_cases = []
        lab_re = re.compile("(in|out) ?(\d+):(.*)")

        logger.debug(f"LAB IO {file}")
        for line in file.splitlines():
            match = lab_re.match(line)

            if match:
                logger.debug(f"LAB IO re match {match.groups()}")
                io = match.group(1)
                key = match.group(2)
                seq = match.group(3)

                # This Python nonsense is a shortcut to check if the key from the regex already has a corresponding item
                # in the test_case attribute. If it exists, the item in the list if returned. If not, None.
                case = next((x for x in self.test_cases if x.get_key() == key), None)

                if case:
                    case.set(io, seq)
                else:
                    lab = LabIO(key)
                    lab.set(io, seq)
                    self.test_cases.append(lab)

    def loop(self):
        """
        Loops over the test cases and yield each one back to the caller
        """
        for I in self.test_cases:
            yield I

    def get_results(self):
        """
        Returns the results of all test cases
        :return:
            A list of test case results
        """
        results = {}

        for t in self.test_cases:
            results[t.get_key()] = t.get_result()

        return results
