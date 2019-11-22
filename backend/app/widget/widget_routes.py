from flask import Blueprint, make_response, request, send_file
from flask import current_app as app
from flask_cors import CORS

from celery.messaging import establish_connection
from kombu.compat import Consumer

import json
import sys

from . import tasks
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
    connection = establish_connection()
    consumer = Consumer(connection=connection,
                        queue=data['identifier'],
                        exchange="learn",
                        routing_key=data['identifier'],
                        exchange_type="direct")

    # Read messages from the message queue
    output = []
    for msg in consumer.iterqueue():
        app.logger.debug(f"Reading {msg.body} from mq")
        output.append(msg.body)
        msg.ack()

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
        response['completed'] = True
        response['status'] = result["status"]

    app.logger.debug(f'Responding with response={response} and code={error_code}')
    return compose_response(response, error_code)
