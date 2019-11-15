"""
This file contains the functional tests for the widget blueprint.

These tests use POSTs to different URLs to check for the proper behavior
of the widget blueprint.
"""

import json


def test_run_singlefile_ada(test_client):
    """
    GIVEN a Flask application
    WHEN the '/run_program' page is posted to (POST)
    THEN check the response is valid
    """
    run_data = {
                "files": [
                        {"basename" : "learn.adb",
                         "contents" : "with Ada.Text_IO; use Ada.Text_IO;\n\nprocedure Learn is\n\n   subtype Alphabet is Character range 'A' .. 'Z';\n\nbegin\n\n   Put_Line (\"Learning Ada from \" & Alphabet'First & \" to \" & Alphabet'Last);\n\nend Learn;"
                        }
                     ],
                     "mode" : "run",
                     "name" : "Introduction",
                     "lab" : False
            }

    response = test_client.post('/run_program/', data=json.dumps(run_data))
    assert response.status_code == 200

    print(response.json)

    read = 0
    while True:
        check_data = {
                      "identifier": response.json["identifier"],
                      "read": read
                     }
        print(check_data)
        response = test_client.post('/check_output/', data=json.dumps(check_data))
        assert response.status_code == 200

        read += len(response.json["output"])
        if response.json["completed"]:
            assert response.json["status"] == 0
            break;

