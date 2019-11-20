"""
This file contains the functional tests for the widget blueprint.

These tests use POSTs to different URLs to check for the proper behavior
of the widget blueprint.
"""

import json
import os
import re

curd = os.path.dirname(__file__)

results_fn = "results.json"


def run_transaction(client, run_data):
    response = client.post('/run_program/', json=run_data)
    assert response.status_code == 200

    json_data = response.get_json()
    identifier = json_data['identifier']

    read = 0
    output = []
    while True:
        check_data = {
                      "identifier": identifier,
                      "read": read
                     }

        response = client.post('/check_output/', json=check_data)
        assert response.status_code == 200

        json_data = response.get_json()

        read += len(json_data["output"])
        output.extend(json_data["output"])
        if json_data["completed"]:
            return json_data, output


def prep_run_data(files):
    ret_files = []
    for file in files:
        with open(file, 'r') as f:
            ret_files.append({"basename": os.path.basename(file), "contents": f.read()})
    return ret_files


def get_results(dirname):
    test_dir = os.path.join(curd, "run_program", dirname)

    results_file = os.path.join(test_dir, results_fn)
    assert os.path.exists(results_file)
    with open(results_file, 'r') as f:
        results = json.loads(f.read())

    return results


def run_program(client, dirname):
    test_dir = os.path.join(curd, "run_program", dirname)

    src_files = []
    for x in os.listdir(test_dir):
        filepath = os.path.join(test_dir, x)
        if re.match(r'.*\.[adb|ads|c|h|cpp|hh]', filepath):
            src_files.append(filepath)

    run_data = {
            "files": prep_run_data(src_files),
            "mode" : "run",
            "name" : dirname,
            "lab" : False
    }

    return run_transaction(client, run_data)


def fix_results(received):
    combined = {}

    for msg in received:
        data = msg['msg']['data']
        mtype = msg['msg']['type']
        if mtype in combined.keys():
            combined[mtype].append(data)
        else:
            combined[mtype] = [data]

    return combined


def test_ada_single_pass(test_client):
    dirname = "test_run_singlefile_ada"
    expected = get_results(dirname)
    data, output = run_program(test_client, dirname)
    assert data["status"] == 0
    fixed_results = fix_results(output)
    assert expected == fixed_results


def test_ada_single_fail(test_client):
    dirname = "test_run_singlefile_error_ada"
    expected = get_results(dirname)
    data, output = run_program(test_client, dirname)
    assert data["status"] == 4
    fixed_results = fix_results(output)
    assert expected == fixed_results