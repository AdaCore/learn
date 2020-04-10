from celery import Celery, states
from celery.exceptions import Ignore
from celery.utils.log import get_task_logger

import traceback

from .container import get_container
from .project import RemoteProject, BuildError


celery = Celery(__name__, autofinalize=False)
logger = get_task_logger(__name__)


@celery.task(bind=True)
def run_program(self, data):
    """
    The task to use to build/run/prove/submit a project. This gets queued up by a flask route.
    :param self:
        This is the reference to the celery task
    :param data:
        The json data from the flask route that contains the files and run mode
    :return:
        Returns a dict containing the status code from the execution
    """
    try:
        container = get_container(celery.conf['CONTAINER_IMPL'], celery.conf['CONTAINER_NAME'])
        task_id = self.request.id
        mode = data['mode']
        app = self._app


        if mode == "run":
            project = RemoteProject(app, container, task_id, data['files'])
            code, out = project.run()
        elif mode == "submit":
            project = RemoteProject(app, container, task_id, data['files'])
            project.submit()
            code = 0
        elif mode == "compile":
            project = RemoteProject(app, container, task_id, data['files'])
            code = project.build()
        elif mode == "prove":
            project = RemoteProject(app, container, task_id, data['files'], True)
            code = project.prove([])
        elif mode == "prove_flow":
            project = RemoteProject(app, container, task_id, data['files'], True)
            code = project.prove(["--mode=flow"])
        elif mode == "prove_report_all":
            project = RemoteProject(app, container, task_id, data['files'], True)
            code = project.prove(["--report=all"])
        else:
            raise Exception("Mode not implemented")

    except BuildError as ex:
        logger.error(f"Build error code {ex}", exc_info=True)
        return {'status': int(f"{ex}")}
    except Exception as ex:
        logger.error("An error occured in run program", exc_info=True)
        self.update_state(state=states.FAILURE,
                          meta={
                            'exc_type': type(ex).__name__,
                            'exc_message': traceback.format_exc().split('\n')
                        })
        raise Ignore()
    finally:
        if container:
            del container

    return {'status': code}
