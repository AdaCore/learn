import logging
import os
import pylxd

logger = logging.getLogger(__name__)


INTERRUPT_STRING = '<interrupted>'
INTERRUPT_RETURNCODE = 124

rw_user = "runner"
ro_user = "unprivileged"

GNAT_PATH = os.path.join(os.path.sep, "gnat", "bin")

class Container:

    def __init__(self, name):
        self.name = name

        # Create the pylxd client
        self.client = pylxd.Client()
        # Get the container from lxd
        self.container = self.client.containers.get(self.name)
        logger.debug("Attached to lxd {} with status {}".format(self.name, self.container.status))
        # TODO: check to see if the container is actually running

    def push_files(self, files, dst):

        for f in files:
            dst_path = os.path.normpath(os.path.join(dst, f.get_name()))
            logger.debug("Copying file {} with contents {} to {}".format(f.get_name(), f.get_content(), dst_path))
            self.container.files.put(dst_path, f.get_content())

        # Change ownership and access permissions for files in container
        self.__execute(["chown", "-R", rw_user, dst], {}, None)
        self.__execute(["chmod", "-R", "a+rx", dst], {}, None)

    def __execute(self, cmds, env, reporter):
        logger.debug("Running {} {} in {}".format(env, cmds, self.name))
        if reporter:
            exit_code, stdout, stderr = self.container.execute(cmds, env, stdout_handler=reporter.stdout, stderr_handler=reporter.stderr)
            if exit_code == INTERRUPT_RETURNCODE:
                reporter.sterr(INTERRUPT_STRING)
            return exit_code, stdout, stderr
        else:
            return self.container.execute(cmds, env)

    def execute_noenv(self, cmds, reporter=None):
        return self.__execute(cmds, {}, reporter)

    def execute(self, cmds, reporter=None):
        return self.__execute(cmds, {"PATH": GNAT_PATH}, reporter)

    def mkdir(self, tdir):
        logger.debug("Making dir {}".format(tdir))
        return self.__execute(["mkdir", tdir], {}, None)

    def rmdir(self, tdir):
        logger.debug("Deleting dir {}".format(tdir))
        return self.__execute(["rm", "-rf", tdir], {}, None)
