from abc import ABC, abstractmethod
import os

####### LXC #########
import pylxd
#####################

###### Docker #######
import docker
from io import BytesIO
import tarfile
#####################


from celery.utils.log import get_task_logger

logger = get_task_logger(__name__)


INTERRUPT_STRING = '<interrupted> : Maximum 10s process limit reached.'
INTERRUPT_RETURNCODE = 124

rw_user = "runner"
ro_user = "unprivileged"

GNAT_PATH = os.path.join(os.path.sep, "gnat", "bin")


def get_container(impl, name):
    if impl == "lxc":
        return LXCContainer(name)
    elif impl == "docker":
        return DockerContainer(name)
    else:
        raise Exception("Unknown container type")


class Container(ABC):
    """
    Abstract Base Class for container interface. See Derived classes for details

    Attributes
    ----------
    name
        The name of the container

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

    @abstractmethod
    def push_files(self, files, dst):
        """
        Push files into the container
        :param files:
            The files to push
        :param dst:
            The destination to put the files
        """
        pass

    @abstractmethod
    def _execute(self, cmds, env, reporter, user=ro_user):
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
        pass

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
        return self._execute(cmds, {}, reporter)

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
        return self._execute(cmds, {"PATH": GNAT_PATH}, reporter)

    def mkdir(self, tdir):
        """
        Make a directory in the container
        :param tdir:
            The directory path to create
        """
        logger.debug(f"Making dir {tdir}")
        code, stdout, stderr = self._execute(["mkdir", tdir], {}, None)
        if code != 0:
            raise Exception(stderr)

    def rmdir(self, tdir):
        """
        Remove a directory in the container
        :param tdir:
            The directory path to remove
        """
        logger.debug(f"Deleting dir {tdir}")
        code, stdout, stderr = self._execute(["rm", "-rf", tdir], {}, None)
        if code != 0:
            raise Exception(stderr)


class LXCContainer(Container):
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
    def __init__(self):
        super()
        # Create the pylxd client
        self.client = pylxd.Client()

        # Get the container from lxd
        self.container = self.client.containers.get(self.name)
        logger.debug(f"Attached to lxd {self.name} with status {self.container.status}")

        try:
            if self.container.status == "Stopped":
                self.container.start(timeout=30, force=True, wait=True)
        except pylxd.exceptions.LXDAPIException as ex:
            raise Exception(f"PYLXD error: {ex}")

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
            logger.debug(f"Copying file {f.get_name()} with contents {f.get_content()} to {dst_path}")
            self.container.files.put(dst_path, f.get_content())

        # Change ownership and access permissions for files in container
        self._execute(["chown", "-R", rw_user, dst], {}, None)
        self._execute(["chmod", "-R", "a+rx", dst], {}, None)

    def _execute(self, cmds, env, reporter, user=ro_user):
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
        logger.debug(f"Running {env} {cmds} in {self.name}")
        if reporter:
            exit_code, stdout, stderr = self.container.execute(cmds, environment=env, stdout_handler=reporter.stdout, stderr_handler=reporter.stderr)
            if exit_code == INTERRUPT_RETURNCODE:
                reporter.stderr(INTERRUPT_STRING)
            return exit_code, stdout, stderr
        else:
            return self.container.execute(cmds, env)

class DockerContainer(Container):
    """
    This class represents the Docker container and is the interface

    Attributes
    ----------
    name
        The name of the container
    client
        The Docker client object used to interact with the container
    container
        The container object returned from Docker

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
        super().__init__(name)
        # Create the pylxd client
        self.client = docker.from_env()

        # Get the container from lxd
        self.container = self.client.containers.create(f"{self.name}:Dockerfile", command= "/bin/sh", detach=True, pids_limit=300)
        self.container.start()
        logger.debug(f"Attached to docker {self.name} with status {self.container.status}")

    def __del__(self):
        self.container.remove()

    def push_files(self, files, dst):
        """
        Push files into the container
        :param files:
            The files to push
        :param dst:
            The destination to put the files
        """
        memfile = BytesIO()
        with tarfile.open('payload.tar', mode='w', fileobj=memfile) as tar:
            for f in files:
                data = f.get_content().encode('utf-8')
                file = BytesIO(data)
                info = tarfile.TarInfo(name=f.get_name())
                info.size = len(data)
                tar.addfile(tarinfo=info, fileobj=file)
        memfile.seek(0)

        success = self.container.put_archive(dst, memfile)

        if not success:
            raise Exception("Could not push files to docker container")

        # Change ownership and access permissions for files in container
        self._execute(["chown", "-R", rw_user, dst], {}, None, user=rw_user)
        self._execute(["chmod", "-R", "a+rx", dst], {}, None, user=rw_user)

    def _stream_exec(self, cmds, env, reporter, user):
        """
        Helper function to use docker low level API. Necessary to get return code
        :param cmds:
            The cmds to run in the container
        :param env:
            The env to modify for the execution
        :param reporter:
            The reporter to send intermediate results to if any
        :param user:
            The user to run the commands as
        :return:
            Returns the exit_code
        """
        exec_id = container.client.api.exec_create(self.container.id, cmds, user=user, environment=env)['Id']
        output = container.client.api.exec_start(exec_id, stream=True, demux=True)
        for item in output:
            stdout, stderr = output
            if stdout:
                reporter.stdout(stdout)
            if stderr:
                reporter.stderr(stderr)
        return self.client.api.exec_inspect(self.id)['ExitCode']

    def _execute(self, cmds, env, reporter, user=ro_user):
        """
        Execute commands inside the container and report back results through reporter if any or via return
        :param cmds:
            The cmds to run in the container
        :param env:
            The env to modify for the execution
        :param reporter:
            The reporter to send intermediate results to if any
        :param user:
            The user to run the commands as
        :return:
            Returns a tuple of exit status, stdout, and stderr
        """
        logger.debug(f"Running {env} {cmds} in {self.name}")
        if reporter:
            exit_code = self._stream_exec(cmds, env, reporter, user)

            if exit_code == INTERRUPT_RETURNCODE:
                reporter.stderr(INTERRUPT_STRING)
            return exit_code, None, None
        else:
            exit_code, output = self.container.exec_run(cmds, stdout=True, stderr=True, user=user, environment=env, demux=True)
            return (exit_code, *output)

    def mkdir(self, tdir):
        """
        Make a directory in the container
        :param tdir:
            The directory path to create
        """
        logger.debug(f"Making dir {tdir}")
        code, stdout, stderr = self._execute(["mkdir", tdir], {}, None, rw_user)
        if code != 0:
            raise Exception(f"mkdir failed with code: {code} - {stderr}")

    def rmdir(self, tdir):
        """
        Remove a directory in the container
        :param tdir:
            The directory path to remove
        """
        logger.debug(f"Deleting dir {tdir}")
        code, stdout, stderr = self._execute(["rm", "-rf", tdir], {}, None, rw_user)
        if code != 0:
            raise Exception(f"rmdir failed with code: {code} - {stderr}")