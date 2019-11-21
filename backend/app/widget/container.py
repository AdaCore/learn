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
    """
    This class represents the LXC container and is the interface for interactions with the container

    Attributes
    ----------
    name
        The name of the container
    client
        The pylxd client object used to interact with the container
    container
        The container object returned from pylxd

    Methods
    -------
    push_files(files, dst)
        Pushes files into the container at dst
    execute_noenv(cmds, reporter=None)
        Executes cmds in the container without modifying the container env and reports results back
        through reporter if any
    execute(cmds, reporter=None)
        Executes cmds in the container modifying the container env by setting PATH with GNAT installation bin path
        and reports results back through reporter if any
    mkdir(tdir)
        Create a folder in the container at tdir
    rmdir(tdir)
        Remove directory tdir in the container
    """
    def __init__(self, name):
        """
        Constructs the container object and grabs the container handles from pylxd.
        :param name:
            The name of the container we will use
        """
        self.name = name

        # Create the pylxd client
        self.client = pylxd.Client()

        # Get the container from lxd
        self.container = self.client.containers.get(self.name)
        logger.debug("Attached to lxd {} with status {}".format(self.name, self.container.status))
        # TODO: check to see if the container is actually running

    def push_files(self, files, dst):
        """
        Push files into the container
        :param files:
            The files to push
        :param dst:
            The destination to put the files
        """
        for f in files:
            dst_path = os.path.normpath(os.path.join(dst, f.get_name()))
            logger.debug("Copying file {} with contents {} to {}".format(f.get_name(), f.get_content(), dst_path))
            self.container.files.put(dst_path, f.get_content())

        # Change ownership and access permissions for files in container
        self.__execute(["chown", "-R", rw_user, dst], {}, None)
        self.__execute(["chmod", "-R", "a+rx", dst], {}, None)

    def __execute(self, cmds, env, reporter):
        """
        Execute commands inside the container and report back results through reporter if any or via return
        :param cmds:
            The cmds to run in the container
        :param env:
            The env to modify for the execution
        :param reporter:
            The reporter to send intermediate results to if any
        :return:
            Returns a tuple of exit status, stdout, and stderr
        """
        logger.debug("Running {} {} in {}".format(env, cmds, self.name))
        if reporter:
            exit_code, stdout, stderr = self.container.execute(cmds, env, stdout_handler=reporter.stdout, stderr_handler=reporter.stderr)
            if exit_code == INTERRUPT_RETURNCODE:
                reporter.sterr(INTERRUPT_STRING)
            return exit_code, stdout, stderr
        else:
            return self.container.execute(cmds, env)

    def execute_noenv(self, cmds, reporter=None):
        """
        Execute cmds in the container without modifying the execution env
        :param cmds:
            The cmds to execute
        :param reporter:
            The reporter to use if any
        :return:
            Returns a tuple of exit status, stdout, stderr
        """
        return self.__execute(cmds, {}, reporter)

    def execute(self, cmds, reporter=None):
        """
        Executes cmds in the container modifying the execution env PATH
        :param cmds:
            The cmds to execute
        :param reporter:
            The reporter to use if any
        :return:
            Returns a tuple of exit code, stdout, stderr
        """
        return self.__execute(cmds, {"PATH": GNAT_PATH}, reporter)

    def mkdir(self, tdir):
        """
        Make a directory in the container
        :param tdir:
            The directory path to create
        """
        logger.debug("Making dir {}".format(tdir))
        code, stdout, stderr = self.__execute(["mkdir", tdir], {}, None)
        if code != 0:
            raise Exception(stderr)

    def rmdir(self, tdir):
        """
        Remove a directory in the container
        :param tdir:
            The directory path to remove
        """
        logger.debug("Deleting dir {}".format(tdir))
        code, stdout, stderr = self.__execute(["rm", "-rf", tdir], {}, None)
        if code != 0:
            raise Exception(stderr)
