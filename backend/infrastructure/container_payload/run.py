#!/usr/bin/env python

""" This is a standalone Python script that runs its argument
    safely in a container.

    At the moment it assumes that the container "safecontainer"
    exists and is running.
"""

import io
import re
import os
import codecs
import json
import glob
import logging
import os
import time
import sys
import subprocess
import traceback

CONT = 'safecontainer'
INTERRUPT_STRING = '<interrupted>'
INTERRUPT_RETURNCODE = 124

CLI_FILE = "cli.txt"

LAB_IO_FILE = "lab_io.txt"

LAB_IO_REGEX = re.compile("(in|out) ?(\d+):(.*)")


COMMON_ADC = """
pragma Restrictions (No_Specification_of_Aspect => Import);
pragma Restrictions (No_Use_Of_Pragma => Import);
pragma Restrictions (No_Use_Of_Pragma => Interface);
pragma Restrictions (No_Dependence => System.Machine_Code);
pragma Restrictions (No_Dependence => Machine_Code);
"""

SPARK_ADC = """
pragma Profile(GNAT_Extended_Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);
pragma SPARK_Mode (On);
pragma Warnings (Off, "no Global contract available");
pragma Warnings (Off, "subprogram * has no effect");
pragma Warnings (Off, "file name does not match");
"""


procedure_re = re.compile("^procedure +[A-Za-z][_a-zA-Z0-9]*[ |\n]+(is|with)", re.MULTILINE)


########################
# Some print functions #
########################

def clean_str(str):
    return str.decode(encoding='utf-8', errors='replace')


def json_print(pdict):
    print(json.dumps(pdict))


def print_generic(msg, tag, lab_ref):
    obj = {"msg": {
                    "type": tag,
                    "data": msg,
                  }
          }
    if lab_ref:
        obj["ref"] = lab_ref
    json_print(obj)


def print_stdout(msg, lab_ref=None):
    print_generic(clean_str(msg), "stdout", lab_ref)


def print_stderr(msg, lab_ref=None):
    print_generic(clean_str(msg), "stderr", lab_ref)


def print_lab(success, cases):
    print_generic({"success": success, "cases": cases}, "lab", None)


def print_console(cmd_list, lab_ref=None):
    print_generic(clean_str(" ".join(cmd_list).replace(workdir, '.')), "console", lab_ref)


def print_internal_error(msg, lab_ref=None):
    print_generic(msg, "internal_error", lab_ref)


def run(command):
    logger.debug(">{}".format(" ".join(command)))
    output = subprocess.check_output(["lxc", "exec", CONT, "--"] + command)
    if output:
        output = output.rstrip()
    logger.debug("<{}".format(output))
    return output


def extract_ada_main(workdir):
    """Return the main if it is found in workdir, empty string otherwise"""
    # Current heuristics for finding the main:
    # find the .adb that doesn't have a .ads.

    names = [os.path.basename(f)
             for f in glob.glob(os.path.join(workdir, "*.ad[sb]"))]
    bases = set([b[:-4] for b in names])
    mains = [b for b in bases if b + '.ads' not in names]
    if mains:
        main = mains[-1]

        # Verify that the main does indeed contain a main
        with open(os.path.join(workdir, main + '.adb'), 'rb') as fd:
            main_text = fd.read()

        if not procedure_re.findall(main_text):
            # This is not a main
            main = ''

        if len(mains) > 1:
            logger.debug("multiple mains found")
        return main
    else:
        logger.debug("No main found")
        return ''


def doctor_main_gpr(tempd, spark_mode=False):
    """Doctor the main.gpr to replace the placeholder with the name of the
       main, and the .adc configuration file for SPARK.

       See template "inline_code".

       Return the name of the main that was found.
    """
    # In the temporary directory, doctor the project file to know about the
    # main.

    to_insert = ''
    languages = []
    main = ""

    # Figure out which language(s) to use
    if glob.glob(os.path.join(tempd, '*.adb')):
        languages.append('Ada')
        # Figure out which main to use
        main = extract_ada_main(tempd)

    if glob.glob(os.path.join(tempd, '*.c')):
        languages.append('C')
        # If there is C
        main = 'main.c'

    if languages:
        to_insert += ("\nfor Languages use ({});".format(
            ", ".join(['"{}"'.format(x) for x in languages])))

    if main:
        to_insert += '\nfor Main use ("{}");'.format(main)

    # Read the project file

    project_file = os.path.join(tempd, "main.gpr")
    with codecs.open(project_file, "rb", encoding="utf-8") as f:
        project_str = f.read()

    project_str = project_str.replace("--MAIN_PLACEHOLDER--", to_insert)

    with codecs.open(project_file, "wb", encoding="utf-8") as f:
        f.write(project_str)

    # Create the main.adc file
    adc_file = os.path.join(tempd, "main.adc")
    contents = COMMON_ADC
    if spark_mode:
        contents += '\n' + SPARK_ADC
    with open(adc_file, "wb") as f:
        f.write(contents)

    return main


def safe_run(workdir, mode, lab):

    def c(cl=[], lab_ref=None):
        """Aux procedure, run the given command line and output to stdout.

        Parameters:
        cl (list): The command list to be sent to popen

        Returns:
        tuple: of (Boolean success, list stdout, int returncode).
        """

        stdout_list = []
        p = None
        try:
            logger.debug("running: {}".format(cl))

            p = subprocess.Popen(cl, cwd=workdir,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False)
            while True:
                stdout_line = p.stdout.readline().replace(workdir, '.')
                stderr_line = p.stderr.readline().replace(workdir, '.')

                if stderr_line:
                    print_stderr(stderr_line.rstrip(), lab_ref)
                    sys.stderr.flush()

                if stdout_line:
                    print_stdout(stdout_line.rstrip(), lab_ref)
                    stdout_list.append(stdout_line)
                    sys.stdout.flush()

                if not (stderr_line or stdout_line):
                    p.poll()
                    break

            sys.stdout.flush()
            sys.stderr.flush()

            if p.returncode == INTERRUPT_RETURNCODE:
                print_stderr(INTERRUPT_STRING, lab_ref)
            return True, stdout_list, p.returncode
        except Exception:
            print_stderr("ERROR when running {}".format(' '.join(cl)), lab_ref)
            print_stderr(traceback.format_exc(), lab_ref)
            return False, stdout_list, (p.returncode if p else 404)

    def build(extra_args):
        """Builds command string to build the application and passes that to c()

        Parameters:
        extra_args (list): The extra build arguments to be passed to the build

        Returns:
        tuple: of (Boolean success, list stdout, returncode).
        """

        line = ["gprbuild", "-q", "-P", "main", "-gnatwa"]
        line.extend(extra_args)
        print_console(line)
        return c(line)

    def run(main, workdir, arg_list, lab_ref=None):
        """Builds command string to run the application and passes that to c()

        Parameters:
        main (string): The name of the main
        workdir (string): The path of the working directory
        arg_list (list): The arguments to be passed to the main

        Returns:
        tuple: of (Boolean success, list stdout, returncode).
        """

        # We run:
        #  - as user 'unprivileged' that has no write access
        #  - under a timeout
        #  - with our ld preloader to prevent forks
        line = ['sudo', '-u', 'unprivileged', 'timeout', '10s',
                'bash', '-c',
                'LD_PRELOAD=/preloader.so {} {}'.format(
                   os.path.join(workdir, main.split('.')[0]), "`echo {}`".format(" ".join(arg_list)))]
        print_list = []
        print_console(["./{}".format(main)] + arg_list, lab_ref)
        return c(line, lab_ref)

    def prove(extra_args):
        """Builds command string to prove the application and passes that to c()

        Parameters:
        extra_args (list): The extra gnatprove arguments to be passed to the prover

        Returns:
        tuple: of (Boolean success, list stdout, returncode).
        """

        line = ["gnatprove", "-P", "main", "--checks-as-errors",
                "--level=0", "--no-axiom-guard"]
        line.extend(extra_args)
        print_console(line)
        return c(line)

    # This is necessary to get the first line from the container. Otherwise
    # the first line is lost.
    c(["echo"])
    try:
        if mode == "run" or mode == "submit":
            main = doctor_main_gpr(workdir, False)

            # In "run" or "submit" mode, build, and then launch the main
            if build([])[2] == 0 and main:
                if mode == "run":
                    # Check to see if cli.txt was sent from the front-end
                    cli_txt = os.path.join(workdir, CLI_FILE)
                    if os.path.isfile(cli_txt):
                        with open(cli_txt, 'r') as f:
                            cli = f.read().split()
                    else:
                        # otherwise pass no arguments to the main
                        cli = []

                    run(main, workdir, cli)
                else:
                    # mode == "submit"
                    # Check to see if lab has IO resources
                    labio_txt = os.path.join(workdir, LAB_IO_FILE)
                    if os.path.isfile(labio_txt):
                        # If it is found, read contents
                        with open(labio_txt, 'r') as f:
                            io_lines = f.readlines()

                        # organize test instances
                        test_cases = {}
                        for line in io_lines:
                            match = LAB_IO_REGEX.match(line)

                            if match:
                                # found match(es)
                                io = match.group(1)
                                key = match.group(2)
                                seq = match.group(3)

                                if key in test_cases.keys():
                                    if io in test_cases[key].keys():
                                        test_cases[key][io] += seq
                                    else:
                                        test_cases[key][io] = seq
                                else:
                                    test_cases[key] = {io: seq}

                        # Loop over IO resources and run all instances in sorted order by test case number
                        success = True
                        for index, test in sorted(test_cases.items()):
                            # check that this test case has defined ins and outs
                            if "in" in test.keys() and "out" in test.keys():

                                errno, stdout, retcode = run(main, workdir, test["in"].split(), index)
                                test["actual"] = " ".join(stdout).replace('\n', '').replace('\r', '')

                                if retcode is not None and retcode != 0:
                                    print_stderr("Process returned non-zero result: {}".format(retcode), index)
                                    test["status"] = "Failed"
                                    success = False
                                else:

                                    if test["actual"] == test["out"]:
                                        test["status"] = "Success"
                                    else:
                                        print_stderr("Program output ({}) does not match expected output ({}).".format(test["actual"], test["out"]), index)
                                        test["status"] = "Failed"
                                        success = False
                            else:
                                print_internal_error("Malformed test IO sequence in test case #{}.".format(index), index)
                                sys.exit(1)
                        print_lab(success, test_cases)
                    else:
                        # No lab IO resources defined. This is an error in the lab config
                        print_internal_error("No submission criteria found for this lab.")
            else:
                print_stderr("Build failed...")

        elif mode == "prove":
            doctor_main_gpr(workdir, spark_mode=True)
            prove([])
        elif mode == "prove_flow":
            doctor_main_gpr(workdir, spark_mode=True)
            prove(["--mode=flow"])
        elif mode == "prove_report_all":
            doctor_main_gpr(workdir, spark_mode=True)
            prove(["--report=all"])
        else:
            print_internal_error("Mode not implemented.")

    except Exception:
        traceback.print_exc()

    finally:
        if os.path.isdir(workdir):
            time.sleep(0.2)  # Time for the filesystem to sync
            c(["rm", "-rf", workdir])


if __name__ == '__main__':
    # perform some sanity checking on args - this is not meant to
    # be launched interactively
    logger = logging.getLogger("run.py")
    handler = logging.StreamHandler(sys.stdout)
    formatter = logging.Formatter("{} : %(asctime)s %(levelname)s (%(name)s) %(message)s".format(CONT))
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    env_level = os.getenv('LOGLEVEL')
    level = logging.getLevelName(env_level)
    logger.setLevel(level)

    logger.debug("loglevel {}".format(os.getenv('LOGLEVEL')))
    if len(sys.argv) >= 3:
        workdir = sys.argv[1]
        mode = sys.argv[2]

        if len(sys.argv) == 4:
            lab = sys.argv[3]
        else:
            lab = None
    else:
        print_internal_error("Error invoking run.")
        sys.exit(1)

    # This is where the compiler is installed
    os.environ["PATH"] = "/gnat/bin:{}".format(os.environ["PATH"])

    safe_run(workdir, mode, lab)
