from celery import Celery, states
from celery.exceptions import Ignore

import logging
import traceback

from .container import Container
from .project import Project, RemoteProject


celery = Celery(__name__, autofinalize=False)
logger = logging.getLogger(__name__)

CONTAINER_NAME = "safecontainer"

@celery.task(bind=True)
def download_program(self, data):
    task_id = self.request.id
    try:
        project = Project(data['files'])
        zipfile = project.zip()
    except Exception as ex:
        self.update_state(state=states.FAILURE,
                          meta={
                            'exc_type': type(ex).__name__,
                            'exc_message': traceback.format_exc().split('\n')
                        })
        raise Ignore()

    return {'zipfile': zipfile}


@celery.task(bind=True)
def run_program(self, data):
    code = None
    container = Container(CONTAINER_NAME)
    task_id = self.request.id
    mode = data['mode']

    try:
        if mode == "run":
            project = RemoteProject(container, task_id, data['files'])
            project.build()
            code, out = project.run()
        elif mode == "submit":
            project = RemoteProject(container, task_id, data['files'])
            project.build()
            project.submit()
        elif mode == "compile":
            project = RemoteProject(container, task_id, data['files'])
            code = project.build()
        elif mode == "prove":
            project = RemoteProject(container, task_id, data['files'], True)
            code = project.prove()
        elif mode == "prove_flow":
            project = RemoteProject(container, task_id, data['files'], True)
            code = project.prove(["--mode=flow"])
        elif mode == "prove_report_all":
            project = RemoteProject(container, task_id, data['files'], True)
            code = project.prove(["--report=all"])
        else:
            raise Exception("Mode not implemented")

    except Exception as ex:
        self.update_state(state=states.FAILURE,
                          meta={
                            'exc_type': type(ex).__name__,
                            'exc_message': traceback.format_exc().split('\n')
                        })
        raise Ignore()

    return {'status': code}
