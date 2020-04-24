import io
import os
import shutil
import tempfile
import zipfile

from .container import rw_user, ro_user
from .file import new_file, find_mains
from .lab import LabList
from .reporter import MQReporter

from celery.utils.log import get_task_logger

logger = get_task_logger(__name__)

CLI_FILE = "cli.txt"

LAB_IO_FILE = "lab_io.txt"

COMMON_ADC = """
pragma Restrictions (No_Specification_of_Aspect => Import);
pragma Restrictions (No_Use_Of_Pragma => Import);
pragma Restrictions (No_Use_Of_Pragma => Interface);
pragma Restrictions (No_Use_Of_Pragma => Linker_Options);
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

MAIN_GPR = """
project Main is

   --MAIN_PLACEHOLDER--

   --LANGUAGE_PLACEHOLDER--

   package Compiler is
      for Switches ("ada") use ("-g", "-O0", "-gnata", "-gnatwa");
   end Compiler;

   package Builder is
      for Switches ("ada") use ("-g");
      for Global_Configuration_Pragmas use "main.adc";
   end Builder;

end Main;
"""

class ProjectError(Exception):
    pass


class BuildError(ProjectError):
    pass


class RunError(ProjectError):
    pass


class ProveError(ProjectError):
    pass


class SubmitError(ProjectError):
    pass


class Project:
    """
    This class represents a project and is used as an interface to interact with code and artifacts of the build.

    Attributes
    ----------
    file_list
        The list of files associated with the project
    cli
        The command line arguments to pass to a run command on the project if any
    spark
        Whether or not the project is a spark project
    gpr
        The gpr file for the project
    lab_list
        The LabList object representing the lab test cases if any
    main
        The name of the main file for the project. Currently we only support a single main

    Methods
    -------
    zip()
        Zips up the files of the project and returns the byte stream
    """
    def __init__(self, files, spark_mode=False):
        """
        Construct the Project. The list of files passed in are processed and stored based on their types and contents.
        :param files:
            The files to construct the project with
        :param spark_mode:
            Whether this is a spark project
        """
        source_files = []
        self.file_list = []
        self.cli = None
        self.spark = spark_mode

        # Grab the default project file
        self.gpr = new_file("main.gpr", MAIN_GPR)

        # strip cli and labio files from files
        for file in files:
            if file['basename'] == CLI_FILE:
                self.cli = file['contents'].split()
            elif file['basename'] == LAB_IO_FILE:
                self.lab_list = LabList(file['contents'])
            else:
                source_files.append(file)

        # Add user submitted files to file list
        self.file_list = [new_file(f['basename'], f['contents']) for f in source_files]

        # Figure out what language(s) to use for the project
        languages = []
        if any([l.language() == "Ada" for l in self.file_list]):
            languages.append("Ada")

        if any([l.language() == "c" for l in self.file_list]):
            languages.append("c")

        if any([l.language() == "c++" for l in self.file_list]):
            languages.append("c++")

        self.gpr.insert_languages(languages)

        # Figure out which files are mains
        mains = find_mains(self.file_list)

        if len(mains) > 1:
            main_match = [i for i in mains if "main.adb" in i]
            if len(main_match) > 1:
                raise ProjectError("More than one main.adb found in project")
            elif len(main_match) < 1:
                self.main = None
            else:
                self.main = main_match[0]
        elif len(mains) == 1:
            self.main = mains[0]
        else:
            self.main = None

        if self.main:
            self.main = self.main.split('.')[0]
            self.gpr.define_mains([self.main])

        self.file_list.append(self.gpr)

        # Add ADC file to file list
        adc_content = COMMON_ADC
        if spark_mode:
            adc_content += '\n' + SPARK_ADC

        self.file_list.append(new_file("main.adc", adc_content))

    def zip(self):
        """
        Zips up the project files and returns the byte stream
        :return:
            Returns the byte stream for the zipfile
        """
        # Make the zipfile in memory to avoid file io
        memory_file = io.BytesIO()
        with zipfile.ZipFile(memory_file, 'w', zipfile.ZIP_DEFLATED) as zf:
            for f in self.file_list:
                logger.debug(f"Adding {f.get_name()} to zip")
                zf.writestr(f.get_name(), f.get_content())
        # Seek back to the beginning of the byte stream before sending
        memory_file.seek(0)
        return memory_file.getvalue()


class RemoteProject(Project):
    """
        This class represents a remote project and is used as an interface to interact with code that will be executed
        inside a container. This class derives from Project.

        Attributes
        ----------
        file_list
            The list of files associated with the project
        cli
            The command line arguments to pass to a run command on the project if any
        spark
            Whether or not the project is a spark project
        gpr
            The gpr file for the project
        lab_list
            The LabList object representing the lab test cases if any
        main
            The name of the main file for the project. Currently we only support a single main
        task_id
            The task id for the task that this class will live in. This is used to associate outputs back to the
            task runner.
        container
            The container that will be used to execute actions inside

        Methods
        -------
        zip()
            Zips up the files of the project and returns the byte stream
        build()
            Builds the project in the container
        run(lab_ref=None, cli=None)
            Runs the project inside the container. The run can be associated with a lab via lab_ref and can take in
            input via cli.
        prove(extra_args)
            Proves the project inside the container. Extra args can be added to gnatprove via extra_args
        submit()
            Submits the project against the lab_list data. This will run the project against each test case and
            aggregate the results
        """
    def __init__(self, app, container, task_id, files, spark_mode=False):
        """
        Constructs the RemoteProject. Calls the super constructor and creates local_tempd and remote_tempd directories
        and pushes files to the container.
        :param container:
            The container to use
        :param task_id:
            The task id for the task that created this class
        :param files:
            The files for the project
        :param spark_mode:
            Whether or not this is a spark project
        """
        self.task_id = task_id
        self.container = container
        self.app = app

        # Call the Project constructor
        super().__init__(files, spark_mode)

        self.container.push_files(self.file_list)

    def build(self):
        """
        Builds the project inside the container
        :return:
            Returns the status code returned from the build
        """
        rep = MQReporter(self.app, self.task_id)

        line = ["gprbuild", "-q", "-P", self.gpr.get_name()]
        rep.console(line)

        code, out, err = self.container.execute(line, rep, rw_user)
        if code != 0:
            rep.stderr(f"Build failed with error code: {code}")
            # We need to raise an exception here to disrupt a build/run/prove build chain from the main task
            raise BuildError(code)
        else:
            rep.stdout(f"Build completed successfully.")
        return code

    def run(self, lab_ref=None, cli=None):
        """
        Runs the project build artifact inside the container
        :param lab_ref:
            The lab_ref is any to associate with the run
        :param cli:
            The cli is any to pass to the run. If cli was passed into the constructor, then this cli overrides the
            member attribute cli.
        :return:
            Returns a tuple of exit status and stdout
        """
        if not self.main:
            raise RunError("Cannot run program without main")

        if not cli:
            if not self.cli:
                cli = []
            else:
                cli = self.cli

        rep = MQReporter(self.app, self.task_id, lab_ref=lab_ref)
        rep.console([f"./{self.main}", " ".join(cli)])

        echo_line = f"`echo {' '.join(cli)}`"
        line = ['timeout', '10s',
                'bash', '-c',
                f'LD_PRELOAD=/preloader.so ./{self.main} {echo_line}']

        code, out, err = self.container.execute(line, reporter=rep)
        return code, out

    def prove(self, extra_args):
        """
        Proves the project inside the container
        :param extra_args:
            The extra args to pass to the prover
        :return:
            Returns the exit code of gnatprove
        """
        if not self.spark:
            raise ProveError("Project not configured for spark mode")

        rep = MQReporter(self.app, self.task_id)
        line = ["gnatprove", "-P", self.gpr.get_name(), "--checks-as-errors", "--level=0", "--no-axiom-guard"]
        line.extend(extra_args)

        rep.console(line)

        code, out, err = self.container.execute(line, rep, rw_user)
        return code

    def submit(self):
        """
        Submits the project against the lab test cases. The lab test cases should have been passed into the constructor
        before this is called.
        """
        successes = []
        if not self.main:
            raise SubmitError("Cannot run program without main")

        if not self.lab_list:
            raise SubmitError("No lab io sent with project")

        # Loop over the labs in the lab_list and run each one against the test case input
        # Store the runs output in a list to check later
        for lab in self.lab_list.loop():
            code, out = self.run(lab_ref=lab.get_key(), cli=lab.get_input())
            successes.append(lab.check_actual(out, code))

        # Get the results from the test cases
        results = self.lab_list.get_results()
        rep = MQReporter(self.app, self.task_id)
        # Check to make sure all the test cases passed and send results to reporter
        rep.lab(all(successes), results)
