from flask import Blueprint, make_response, request, send_file
from flask import current_app as app
from flask_cors import CORS

from celery.task.control import inspect

import glob
import io
import json
import os
import codecs
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


def get_example():
    """Return the default Inline code example directory path"""
    ex_dir = os.path.join(app.config['TEMPLATE_DIR'], "inline_code")
    app.logger.debug("Example directory is {}".format(ex_dir))
    return ex_dir


def prep_example_directory(example, request):
    """Prepare the directory in which the example can be run.
       Return a tuple with
          - the name of the directory created if it exists
          - the error message if not
    """
    # Create a temporary directory
    tempd = tempfile.mkdtemp()
    app.logger.debug("Creating tmp dir {}".format(tempd))

    # Copy the original resources in a sandbox directory
    for g in glob.glob(os.path.join(example, '*')):
        if not os.path.isdir(g):
            app.logger.debug("Copying {} to {}".format(g, tempd))
            shutil.copy(g, tempd)

    # Overwrite with the user-contributed files
    for file in request['files']:
        if len(file['contents']) > app.config['RECEIVED_FILE_CHAR_LIMIT']:
            app.logger.error("File {} exceeded char limit: size {}".format(
                file['basename'], len(file['contents'])))
            shutil.rmtree(tempd)
            return (None, "file contents exceeds size limits")
        app.logger.debug("Writing file {} to {}".format(
            file['basename'], tempd))
        with codecs.open(os.path.join(tempd, file['basename']),
                         'w', 'utf-8') as f:
            f.write(file['contents'])

    return (tempd, None)


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
    e = get_example()
    if not e:
        return compose_response({'identifier': '', 'message': "example not found"}, 500)

    tempd, message = prep_example_directory(e, data)
    if message:
        return compose_response({'identifier': '', 'message': message}, 500)

    app.logger.debug(data)
    mode = data['mode']

    # Check whether we have too many processes running
    # if not resources_available():
    #    return make_response({'identifier': '', 'message': "the machine is busy processing too many requests"},
    #                         500,
    #                         headers=get_headers())

    # Run the command(s)
    run_cmd = "python /workspace/run.py /workspace/sessions/{} {}".format(
        os.path.basename(tempd), mode)

    if data['lab']:
        lab = data['name']
        run_cmd += " {}".format(lab)

    # Push the code to the container in Celery task
    task = tasks.run_program.apply_async(
        kwargs={'tempd': tempd, 'run_cmd': run_cmd})
    app.logger.debug('Starting Celery task with id={}'.format(task.id))
    app.logger.debug('Running cmd in container: {}'.format(run_cmd))

    return compose_response({'identifier': task.id, 'tempd': tempd, 'message': "Pending"}, 200)


@widget_bp.route('/check_output/', methods=['POST'])
def check_output():
    error_code = 200
    data = request.get_json()
    app.logger.debug(data)

    identifier = data['identifier']
    app.logger.debug('Checking Celery task with id={}'.format(identifier))
    task = tasks.run_program.AsyncResult(identifier)

    if task.failed():
        result = task.get()
        app.logger.error(
            'Task id={} failed. Response={}'.format(task.id, result))
        error_code = 500
        response = task.info["error"]
    else:
        app.logger.debug("Task state {}".format(task.state))

        tempd = data['tempd']
        stdout_file = os.path.join(tempd, "stdout.txt")

        output = []
        if os.path.isfile(stdout_file):
            with open(stdout_file, 'r') as f:
                lines = f.readlines()
            output = lines[data['read']:]
        else:
            output = []

        app.logger.debug("output {}".format(output))

        response = {'output': [json.loads(l) for l in output],
                    'status': 0,
                    'completed': False,
                    'message': task.state}

        if task.ready():
            app.logger.debug("Task info {}".format(task.info))
            result = task.get()
            response['completed'] = True
            response['status'] = result["status"]
            shutil.rmtree(data['tempd'])

    app.logger.debug(
        'Responding with response={} and code={}'.format(response, error_code))
    return compose_response(response, error_code)
