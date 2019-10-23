"""Summary

    The tasks module implements the tasks that will run in the celery worker.

Attributes:
    celery (celery.Celery): Instantiation of the Celery object
    logger (logging.Logger): Logger object
"""
from celery import Celery, states
from celery.exceptions import Ignore

import logging
import os
import pylxd
import traceback


celery = Celery(__name__, autofinalize=False)
logger = logging.getLogger(__name__)

container_name = "safecontainer"


def recursive_put(container, src, dst):
    norm_src = os.path.normpath(src)
    if not os.path.isdir(norm_src):
        raise NotADirectoryError(
            "'src' parameter must be a directory "
        )

    idx = len(norm_src)

    for path, dirname, files in os.walk(norm_src):
        dst_path = os.path.normpath(
            os.path.join(dst, path[idx:].lstrip(os.path.sep)))
        container.execute(["mkdir", dst_path])

        # copy files
        for f in files:
            filepath = os.path.join(dst_path, f)

            src_file = os.path.join(path, f)
            with open(src_file, 'r') as fp:
                data = fp.read()

            logger.debug("Copying {} to {}".format(src_file, filepath))
            container.files.put(filepath, data)


@celery.task(bind=True)
def run_program(self, tempd, run_cmd):

    def update_stdout(msg):
        logger.debug("raw msg {}".format(msg))
        for m in msg.splitlines():
            if m.startswith(container_name):
                print(m)
            else:
                logger.debug("Updating status with msg:{}".format(m))

                with open(os.path.join(tempd, "stdout.txt"), "a+") as f:
                    f.write("{}\n".format(m))

    try:
        # Create the pylxd client
        client = pylxd.Client()
        # Get the container from lxd
        container = client.containers.get(container_name)
        logger.debug("Celery worker attached to lxd {}".format(container_name))

        tmp_name = os.path.basename(os.path.normpath(tempd))

        # This is not supported yet, but is in the latest docs
        # container.files.recursive_put(tempd, os.path.join(container_name, "workspace", "sessions", tmp_name))

        # Temp workaround until recursive_put is released
        recursive_put(container, tempd, os.path.join(
            os.path.sep, "workspace", "sessions", tmp_name))
        logger.debug(
            "Celery worker transferred files to {}".format(container_name))

        # Change ownership and access permissions for files in container
        container.execute(["chown", "-R", "runner", os.path.join(os.path.sep,
                                                                 "workspace", "sessions", os.path.basename(tempd))])
        container.execute(["chmod", "-R", "a+rx", os.path.join(os.path.sep,
                                                               "workspace", "sessions", os.path.basename(tempd))])

    except Exception as e:
        logger.error("Celery worker failed to transfer program to {}: {}: {}".format(
            e, traceback.print_exc(), container_name))
        self.update_state(state=states.FAILURE,
                          meta={"error": "Error transferring the program to container"})
        raise Ignore()

    exc_env = {"LOGLEVEL": logging.getLevelName(logger.getEffectiveLevel())}
    logger.debug("Celery worker running {} in {} with env {}".format(
        run_cmd, container_name, exc_env))
    code, stdout, stderr = container.execute(
        ["su", "runner", "-c", run_cmd], environment=exc_env, stdout_handler=update_stdout)
    logger.debug("Celery worker completed with code {} stdout {} stderr {}".format(
        code, stdout, stderr))

    return {'status': code}
