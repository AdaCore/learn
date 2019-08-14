// List of known modes, and the corresponding button labels
interface buttonMode {
    buttonText : string;
    tooltip : string;
}

const modeDictionary : {[mode : string] : buttonMode } = {
  prove: {
    buttonText: 'Prove',
    tooltip: 'Run gnatprove to prove SPARK code',
  },
  prove_flow: {
    buttonText: 'Examine',
    tooltip: 'Run gnatprove to examine SPARK data and control flow',
  },
  prove_report_all: {
    buttonText: 'Prove (report=all)',
    tooltip: 'Run gnatprove to prove SPARK code and report all findings',
  },
  run: {
    buttonText: 'Run',
    tooltip: 'Run code in editor',
  },
  submit: {
    buttonText: 'Submit',
    tooltip: 'Submit code for lab',
  },
}

const TEST_CASE_LABEL = 'Test Case';

const RESET_TOOLTIP = 'Reset editor to default state';
// const SETTINGS_TOOLTIP = 'Modify settings for this editor';

const SETTINGS_TABBED_EDITOR_LABEL =
'Enable tabbed editor view for this editor';

const SETTINGS_THEME_EDITOR_LABEL =
'Use the dark theme';

const CUSTOM_INPUT_LABEL = 'Test against custom input';
const CUSTOM_INPUT_TOOLTIP =
'Use the Run button to test your code against a custom input sequence';

const INTERNAL_ERROR_MESSAGE =
'Please report this issue on https://github.com/AdaCore/learn/issues';

const LAB_TEST_INPUT_LABEL = 'Input';
const LAB_TEST_OUTPUT_LABEL = 'Expected Output';
const LAB_TEST_ACTUAL_LABEL = 'Received Output';
const LAB_TEST_STATUS_LABEL = 'Status';

const LAB_COMPLETE_LABEL = 'Lab completed successfully.';
const LAB_FAILED_LABEL = 'Lab failed.';

const EXIT_STATUS_LABEL = 'exit status';

const MACHINE_NOT_RESPONDING_LABEL =
'The machine running the examples is not responding, please try again later.';
const MACHINE_BUSY_LABEL =
'The machine running the examples may not be available or is busy, please try' +
' again now or come back later.';

const CONSOLE_OUTPUT_LABEL = 'Console Output';

const CLI_FILE = 'cli.txt';
