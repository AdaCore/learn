import docker
from io import BytesIO
import tarfile
import time

from celery.utils.log import get_task_logger

logger = get_task_logger(__name__)

INTERRUPT_STRING = '<interrupted> : Maximum 10s process limit reached.'
INTERRUPT_RETURNCODE = 124

rw_user = "runner"
ro_user = "unprivileged"

TMPFS = "/workspace/sessions"

CONTAINER_CONFIG = {
    'command': "/bin/sh",
    'environment': {
        'TMPDIR': TMPFS
    },
    'mem_limit': '32M',
    'network_disabled': True,
    'pids_limit': 100,
    'security_opt': [
        'no-new-privileges'
    ],
    'stdin_open': True,
    'tty': True,
    'working_dir': TMPFS
}


class DockerContainer:
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
    execute(cmds, reporter=None)
        Executes cmds in the container modifying the container env by setting PATH with GNAT installation bin path
        and reports results back through reporter if any
    """
    def __init__(self, image):
        self.client = docker.from_env()

        self.container = self.client.containers.create(image, **CONTAINER_CONFIG)
        self.name = self.container.name
        self.container.start()
        logger.debug(f"Attached to docker {self.name} with status {self.container.status}")

    def remove(self):
        logger.debug("Removing container")
        self.container.remove(force=True)
        logger.debug(f"Container status: {self.container.status}")

    def push_files(self, files):
        """
        Push files into the container
        :param files:
            The files to push
        """
        memfile = BytesIO()
        with tarfile.open('payload.tar', mode='w', fileobj=memfile) as tar:
            for f in files:
                data = f.get_content().encode('utf-8')
                file = BytesIO(data)
                info = tarfile.TarInfo(name=f.get_name())
                info.size = len(data)
                info.uname = rw_user
                info.mode = 0o755
                info.mtime = time.time()
                tar.addfile(tarinfo=info, fileobj=file)
        memfile.seek(0)

        logger.debug(f"Pushing files to {TMPFS} in {self.name}")
        success = self.container.put_archive(TMPFS, memfile)

        if not success:
            raise Exception("Could not push files to docker container")

    def _stream_exec(self, cmds, reporter, user):
        """
        Helper function to use docker low level API. Necessary to get return code
        :param cmds:
            The cmds to run in the container
        :param reporter:
            The reporter to send intermediate results to if any
        :param user:
            The user to run the commands as
        :return:
            Returns the exit_code, stdout, stderr
        """
        ret_stdout = ""
        ret_stderr = ""
        exec_id = self.container.client.api.exec_create(self.container.id,
                                                        cmds,
                                                        user=user,
                                                        workdir=TMPFS)['Id']
        output = self.container.client.api.exec_start(exec_id,
                                                      stream=True,
                                                      demux=True)
        for item in output:
            stdout, stderr = item
            if stdout:
                temp = stdout.decode("utf-8")
                ret_stdout += temp
                reporter.stdout(temp)

            if stderr:
                temp = stderr.decode("utf-8")
                ret_stderr += temp
                reporter.stderr(temp)
        return self.container.client.api.exec_inspect(exec_id)['ExitCode'], ret_stdout, ret_stderr

    def execute(self, cmds, reporter=None, user=ro_user):
        """
        Execute commands inside the container and report back results through reporter if any or via return
        :param cmds:
            The cmds to run in the container
        :param reporter:
            The reporter to send intermediate results to if any
        :param user:
            The user to run the commands as
        :return:
            Returns a tuple of exit status, stdout, and stderr
        """
        exit_code = -1
        stdout = None
        stderr = None

        logger.debug(f"Running {cmds} in {self.name}")

        if reporter:
            exit_code, stdout, stderr = self._stream_exec(cmds,
                                                          reporter,
                                                          user)

            if exit_code == INTERRUPT_RETURNCODE:
                reporter.stderr(INTERRUPT_STRING)
        else:
            exit_code, output = self.container.exec_run(cmds,
                                                        stdout=True,
                                                        stderr=True,
                                                        user=user,
                                                        workdir=TMPFS,
                                                        demux=True)
            if output[0]:
                stdout = output[0].decode("utf-8")
            if output[1]:
                stderr = output[1].decode("utf-8")
        return exit_code, stdout, stderr
