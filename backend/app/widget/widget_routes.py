from flask import Blueprint, make_response, request, send_file
from flask import current_app as app
from flask_cors import CORS
from flask_mail import Mail, Message

from queue import Empty
from kombu import Queue

import json
import sys
import time

from . import tasks, celery
from .project import Project

widget_bp = Blueprint('widget_bp', __name__)
CORS(widget_bp)


def compose_response(obj, code):
    """
    Utility method to create a response
    :param obj:
        The object to respond with
    :param code:
        The HTTP code to send
    :return:
        Returns the constructed response
    """
    response = make_response(obj, code)
    return response


@widget_bp.route('/download/', methods=['POST'])
def download_example():
    """
    The route for a download request. This constructs a Project, zips it up, and responds with the zipfile
    :return:
        The constructed response for the zipfile blob
    """
    data = request.get_json()
    app.logger.debug(data)

    project = Project(data['files'])
    zipfile = project.zip()

    archive = data['name'] + '.zip'

    app.logger.debug(f"Sending zipped file {archive} size={sys.getsizeof(zipfile)}")
    response = make_response(zipfile)

    # We need to mess with the header here because we are sending file attachments in CORS
    response.headers['Access-Control-Expose-Headers'] = 'Content-Disposition'
    response.headers['Access-Control-Allow-Headers'] = 'Content-Disposition'
    response.headers.set('Content-Type', 'application/zip')
    response.headers.set('Content-Disposition', 'attachment', filename=archive)
    return response


@widget_bp.route('/contact_form/', methods=['POST'])
def contact_form():
    """
    The route for a contact-form submission.The data is sent via email.
    :return:
        The success or fail of the email send
    """
    data = request.get_json()
    app.logger.debug(data)

    if not data['GDPRConsent']:
        success = False
        app.logger.error('Form submission with GDPR consent = false.')
    else:
        time.sleep(5)
        # mail = Mail(app)
        # msg = Message(subject=f"LEARN: Message from {data['Name']}",
        #               body=json.dumps(data, indent=4),
        #               sender=data["Email"],
        #               reply_to=data["Email"],
        #               recipients=["test@test.com"])
        # mail.send(msg)
        success = True

    return compose_response({'success': success}, 200)


@widget_bp.route('/run_program/', methods=['POST'])
def run_program():
    """
    The route for a run program request. This kicks off a celery task and responds back with the task id
    :return:
        The constructed response with the task id
    """
    data = request.get_json()
    app.logger.debug(data)

    # Push the code to the container in Celery task
    task = tasks.run_program.apply_async(kwargs={'data': data})
    app.logger.debug(f'Starting Celery task with id={task.id}')

    return compose_response({'identifier': task.id, 'message': "Pending"}, 200)


@widget_bp.route('/check_output/', methods=['POST'])
def check_run():
    """
    The route for a check program request. The user should have a task id supplied by the run request. We expect that
    as part of the incoming data and use it to query the message queues associated with that task id
    :return:
        The construct response with the output from the task, the task status, and completion status
    """
    error_code = 200
    data = request.get_json()
    app.logger.debug(data)
    identifier = data['identifier']
    task = tasks.run_program.AsyncResult(identifier)

    # Create a connection to the message queue associated with the task id
    # This is how we receive intermediate results from the task during run
    output = []
    with celery.connection_or_acquire() as conn:
        queue = conn.SimpleBuffer(data['identifier'])
        while True:
            try:
                msg = queue.get(block=False)
                app.logger.debug(f"Reading {msg.body} from mq")
                output.append(msg.body)
            #    msg.ack()
            except Empty:
                break
        queue.close()

    app.logger.debug(f"output {output}")

    response = {'output': [json.loads(l) for l in output],
                'status': 0,
                'completed': False,
                'message': task.state}

    app.logger.debug(f'Checking Celery task with id={identifier}')

    app.logger.debug(f"Task state {task.state}")
    if task.failed():
        result = task.get()
        app.logger.error(f'Task id={task.id} failed. Response={task.info}')
        error_code = 500

    if task.ready():
        app.logger.debug(f"Task info {task.info}")
        result = task.get()
        elapsed = result["elapsed"]
        app.logger.debug(f"Task took {elapsed}s.")
        response['completed'] = True
        response['status'] = result["status"]

    app.logger.debug(f'Responding with response={response} and code={error_code}')
    return compose_response(response, error_code)
