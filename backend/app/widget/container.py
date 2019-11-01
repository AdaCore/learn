import logging
import os
import pylxd

logger = logging.getLogger(__name__)


INTERRUPT_STRING = '<interrupted>'
INTERRUPT_RETURNCODE = 124

rw_user = "runner"
ro_user = "unprivileged"

class Container:

    def __init__(self, name):
        self.name = name

        # Create the pylxd client
        self.client = pylxd.Client()
        # Get the container from lxd
        self.container = self.client.containers.get(self.name)
        logger.debug("Attached to lxd {}".format(self.name))

    def push_files(self, files, dst):
        for f in files:
            dst_path = os.path.join(dst, f.get_name())
            logger.debug("Copying {} to {}".format(f.get_name(), dst_path))
            self.container.put(dst_path, f.get_content())

        # Change ownership and access permissions for files in container
        self.execute_rw(["chown", "-R", rw_user, dst])
        self.execute_rw(["chmod", "-R", "a+rx", dst])

    def __execute(self, cmds, reporter):
        logger.debug("Running {} in {}".format(cmds, self.name))
        if reporter:
            reporter.console(cmds)
            exit_code, stdout, stderr = self.container.execute(cmds, stdout_handler=reporter.stdout, stderr_handler=reporter.stderr)
            if exit_code == INTERRUPT_RETURNCODE:
                reporter.sterr(INTERRUPT_STRING)
            return exit_code, stdout, stderr
        else:
            return self.container.execute(cmds)

    def execute_rw(self, cmds, reporter=None):
        fix_cmds = ["su", "runner", "-c"]
        fix_cmds.extend(cmds)
        self.__execute(fix_cmds, reporter)

    def execute_ro(self, cmds, reporter=None):
        fix_cmds = ['sudo', '-u', ro_user, 'timeout', '10s', 'bash', '-c']
        fix_cmds.extend(cmds)
        self.__execute(fix_cmds, reporter)

    def mkdir(self, tdir):
        logger.debug("Making dir {}".format(tdir))
        self.execute_rw(["mkdir", tdir])

    def rmdir(self, tdir):
        logger.debug("Deleting dir {}".format(tdir))
        self.execute_rw(["rm", "-rf", tdir])




