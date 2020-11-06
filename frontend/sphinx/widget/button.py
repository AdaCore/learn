button_dictionary = {
    "prove_button": {
        "name": 'Prove',
        "title": 'Prove SPARK code',
        "mode": "prove"
    },
    "prove_flow_button": {
        "name": 'Examine',
        "title": 'Examine SPARK data and control flow',
        "mode": "prove_flow"
    },
    "prove_flow_report_all_button": {
        "name": 'Examine (report=all)',
        "title": 'Examine SPARK data and control flow and report all findings',
        "mode": "prove_flow_report_all"
    },
    "prove_report_all_button": {
        "name": 'Prove (report=all)',
        "title": 'Prove SPARK code and report all findings',
        "mode": "prove_report_all"
    },
    "run_button": {
        "name": 'Run',
        "title": 'Run code in editor',
        "mode": "run"
    },
    "submit_button": {
        "name": 'Submit',
        "title": 'Submit code for lab',
        "mode": "submit"
    },
    "compile_button": {
        "name": 'Compile',
        "title": 'Compile code to check syntax',
        "mode": "compile"
    },
}

class Button:
    def __init__(self, button: str):
        """Constructs a button

        The name supplied in the constructor will be used to lookup the
        button's information in the button_dictionary lookup table.

        Args:
            button (str): The name of a button to create
        """
        self.name = button_dictionary[button]["name"]
        self.title = button_dictionary[button]["title"]
        self.mode = button_dictionary[button]["mode"]
