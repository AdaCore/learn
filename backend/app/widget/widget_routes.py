from flask import Blueprint, make_response, request, send_file
from flask import current_app as app
from flask_cors import CORS

from celery.messaging import establish_connection
from celery.task.control import inspect
from kombu.compat import Consumer

import codecs
import glob
import io
import json
import os
import shutil
import sys
import tempfile
import zipfile

from . import tasks

widget_bp = Blueprint('widget_bp', __name__)
CORS(widget_bp)


def compose_response(obj, code):
    response = make_response(obj, code)
    return response


@widget_bp.route('/download/', methods=['POST'])
def download_example():

    data = request.get_json()
    e = get_example()
    if not e:
        return compose_response({'identifier': '', 'message': "example not found"}, 500)

    tempd, message = prep_example_directory(e, data)
    if message:
        return compose_response({'identifier': '', 'message': message}, 500)

    app.logger.debug("Zipping tmp dir {}".format(tempd))

    memory_file = io.BytesIO()
    cwd = os.getcwd()
    os.chdir(tempd)
    with zipfile.ZipFile(memory_file, 'w', zipfile.ZIP_DEFLATED) as zf:
        for root, dirs, files in os.walk('.'):
            for file in files:
                app.logger.debug("Adding {} to zip".format(os.path.join(root, file)))
                zf.write(os.path.join(root, file))
    os.chdir(cwd)
    memory_file.seek(0)

    archive = data['name'] + '.zip'

    app.logger.debug("Sending zipped file {} size={}".format(archive, sys.getsizeof(memory_file)))
    response = make_response(send_file(memory_file, attachment_filename=archive, as_attachment=True))
    response.headers['Access-Control-Expose-Headers'] = 'Content-Disposition'
    response.headers['Access-Control-Allow-Headers'] = 'Content-Disposition'
    return response


@widget_bp.route('/run_program/', methods=['POST'])
def run_program():
    data = request.get_json()
    app.logger.debug(data)

    # Push the code to the container in Celery task
    task = tasks.run_program.apply_async(kwargs={'data': data})
    app.logger.debug('Starting Celery task with id={}'.format(task.id))

    return compose_response({'identifier': task.id, 'message': "Pending"}, 200)


@widget_bp.route('/check_output/', methods=['POST'])
def check_output():
    error_code = 200
    data = request.get_json()
    app.logger.debug(data)
    identifier = data['identifier']
    task = tasks.run_program.AsyncResult(identifier)

    connection = establish_connection()
    consumer = Consumer(connection=connection,
                        queue=data['identifier'],
                        exchange="learn",
                        routing_key=data['identifier'],
                        exchange_type="direct")

    output = []
    for msg in consumer.iterqueue():
        app.logger.debug("Reading {} from mq".format(msg.body))
        output += msg.body
        msg.ack()

    app.logger.debug("output {}".format(output))

    response = {'output': [json.loads(l) for l in output],
                'status': 0,
                'completed': False,
                'message': task.state}

    app.logger.debug('Checking Celery task with id={}'.format(identifier))

    app.logger.debug("Task state {}".format(task.state))
    if task.failed():
        result = task.get()
        app.logger.error('Task id={} failed. Response={}'.format(task.id, task.info))
        error_code = 500

    if task.ready():
        app.logger.debug("Task info {}".format(task.info))
        result = task.get()
        response['completed'] = True
        response['status'] = result["status"]

    app.logger.debug('Responding with response={} and code={}'.format(response, error_code))
    return compose_response(response, error_code)
