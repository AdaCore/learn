import io
import logging
import os
import shutil
import tempfile
import zipfile

logger = logging.getLogger(__name__)

from .file import new_file
from .lab import LabList
from .reporter import MQReporter

CLI_FILE = "cli.txt"

LAB_IO_FILE = "lab_io.txt"

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

TEMPLATE_PROJECT = os.path.join(os.getcwd(), "app", "widget", "static", "templates", "inline_code", "main.gpr")

class Project:

    def __init__(self, files, spark_mode=False):
        self.file_list = []
        self.spark = spark_mode

        # Grab project file
        gpr_name = TEMPLATE_PROJECT
        with open(gpr_name, 'r') as f:
            gpr_content = f.read()

        self.gpr = new_file(gpr_name, gpr_content)

        # Add user submitted files to file list
        self.file_list = [new_file(f['basename'], f['contents']) for f in files]

        # Figure out what language(s) to use for the project
        languages = []
        if any([l.language() == "Ada" for l in self.file_list]):
            languages += "Ada"

        if any([l.language() == "c" for l in self.file_list]):
            languages += "c"

        if any([l.language() == "c++" for l in self.file_list]):
            languages += "c++"

        self.gpr.insert_languages(languages)

        # Figure out which files are mains
        mains = [f.basename for f in self.file_list if f.is_main()]
        if len(mains) > 1:
            raise Exception("More than one main found in project")
        elif len(mains) == 1:
            self.main = mains[0]
            self.gpr.define_mains([self.main])
        else:
            self.main = None

        self.file_list.append(self.gpr)

    def zip(self):
        memory_file = io.BytesIO()
        with zipfile.ZipFile(memory_file, 'w', zipfile.ZIP_DEFLATED) as zf:
            for f in self.file_list:
                logger.debug("Adding {} to zip".format(f.get_name()))
                zf.writestr(f.get_name(), f.get_content())

        memory_file.seek(0)
        return memory_file.getvalue()


class RemoteProject(Project):

    def __init__(self, container, task_id, files, spark_mode=False):
        source_files = []
        self.task_id = task_id
        self.container = container

        # strip cli and labio files from files
        for file in files:
            if file['basename'] == CLI_FILE:
                self.cli = file[contents].split()
            elif file['basename'] == LAB_IO_FILE:
                self.lab_list = LabList(file['content'])
            else:
                source_files.append(file)

        super().__init__(source_files, spark_mode)

        # Add ADC file to file list
        adc_content = COMMON_ADC
        if spark_mode:
            contents += '\n' + SPARK_ADC

        self.file_list.append(new_file("main.adc", adc_content))

        self.local_tempd = tempfile.mkdtemp()
        tmp_name = os.path.basename(os.path.normpath(self.local_tempd))
        self.remote_tempd = os.path.join(os.path.sep, "workspace", "sessions", tmp_name)
        self.container.mkdir(self.remote_tempd)

        self.container.push_files(self.file_list, self.remote_tempd)

    def __del__(self):
        shutil.rmtree(self.local_tempd)
        self.container.rmdir(self.remote_tempd)

    def build(self):
        rep = MQReporter(self.task_id)
        line = ["gprbuild", "-q", "-P", self.gpr.get_name(), "-gnatwa"]

        code, out, err = self.container.execute_rw(line, rep)
        if code != 0:
            raise Exception("Build failed with error code: {}".format(code))
        return code

    def run(self, lab_ref=None, cli=None):
        if not self.main:
            raise Exception("Cannot run program without main")

        if not cli:
            cli = self.cli

        rep = MQReporter(task_id, lab_ref=lab_ref, quiet=True)

        line = ['LD_PRELOAD=/preloader.so {} {}'.format(os.path.join(self.tempd, self.main.split('.')[0]), "`echo {}`".format(" ".join(cli)))]
        rep.console(["./{}".format(self.main)] + cli, lab_ref)

        code, out, err = self.container.execute_ro(line, rep)
        return code, out

    def prove(self, extra_args):
        if not self.spark:
            raise Exception("Project not configured for spark mode")

        rep = MQReporter(task_id)

        line = ["gnatprove", "-P", self.gpr.get_name(), "--checks-as-errors", "--level=0", "--no-axiom-guard"]
        line.extend(extra_args)

        code, out, err = self.container.execute_rw(line, rep)
        return code

    def submit(self):
        successes = []
        if not self.main:
            raise Exception("Cannot run program without main")

        if not self.lab_list:
            raise Exception("No lab io sent with project")

        for lab in lab_list.loop():
            code, out = self.run(lab_ref=lab.get_key(), cli=lab.get_input())
            successes += lab.check_actual(out, code)

        results = lab_list.get_results()
        rep = MQReporter(task_id)
        rep.lab(all(successes), results)
