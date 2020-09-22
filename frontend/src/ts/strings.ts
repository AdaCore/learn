interface ButtonMode {
  buttonText: string;
  tooltip: string;
}

export const modeDictionary: {[mode: string]: ButtonMode } = {
  prove: {
    buttonText: 'Prove',
    tooltip: 'Prove SPARK code',
  },
  // eslint-disable-next-line @typescript-eslint/camelcase
  prove_flow: {
    buttonText: 'Examine',
    tooltip: 'Examine SPARK data and control flow',
  },
  // eslint-disable-next-line @typescript-eslint/camelcase
  prove_flow_report_all: {
    buttonText: 'Examine (report=all)',
    tooltip: 'Examine SPARK data and control flow and report all findings',
  },
  // eslint-disable-next-line @typescript-eslint/camelcase
  prove_report_all: {
    buttonText: 'Prove (report=all)',
    tooltip: 'Prove SPARK code and report all findings',
  },
  run: {
    buttonText: 'Run',
    tooltip: 'Run code in editor',
  },
  submit: {
    buttonText: 'Submit',
    tooltip: 'Submit code for lab',
  },
  compile: {
    buttonText: 'Compile',
    tooltip: 'Compile code to check syntax',
  },
};

export const TEST_CASE_LABEL = 'Test Case';

export const RESET_TOOLTIP = 'Reset editor to default state';
export const RESET_CONFIRM_MSG =
'Your changes will be lost after reset. ' +
'Are you sure you want to reset the editor?';

export const DOWNLOAD_TOOLTIP = 'Download source files';
// const SETTINGS_TOOLTIP = 'Modify settings for this editor';

export const SETTINGS_TABBED_EDITOR_LABEL =
'Enable tabbed editor view for this editor';

export const SETTINGS_THEME_EDITOR_LABEL =
'Use the dark theme';

export const CUSTOM_INPUT_LABEL = 'Test against custom input';
export const CUSTOM_INPUT_TOOLTIP =
'Use the Run button to test your code against a custom input sequence';

export const INTERNAL_ERROR_MESSAGE =
'Please report this issue on https://github.com/AdaCore/learn/issues';

export const LAB_TEST_INPUT_LABEL = 'Input';
export const LAB_TEST_OUTPUT_LABEL = 'Expected Output';
export const LAB_TEST_ACTUAL_LABEL = 'Received Output';
export const LAB_TEST_STATUS_LABEL = 'Status';

export const LAB_COMPLETE_LABEL = 'Lab completed successfully.';
export const LAB_FAILED_LABEL = 'Lab failed.';

export const EXIT_STATUS_LABEL = 'exit status';

export const MACHINE_NOT_RESPONDING_LABEL =
'The machine running the examples is not responding, please try again later.';
export const MACHINE_BUSY_LABEL =
'The machine running the examples may not be available or is busy, please try' +
' again now or come back later.';

export const CONSOLE_OUTPUT_LABEL = 'Console Output';

export const CLI_FILE = 'cli.txt';

export const FORM_SUCCESS =
'Thanks! Your message has been successfully sent to the ' +
'learn.adacore.com team.';

export const FORM_FAIL =
'An error occurred while trying to send your message. ' +
'Please try again, or submit an issue on our GitHub.';

export const FORM_GDPR_CONSENT = 'I agree to the processing of the above ' +
'information for the purpose of processing this feedback and with ' +
'AdaCore\'s privacy policy.';

export const FORM_PRIVACY_POLICY = 'The information above is collected by ' +
'AdaCore for the purpose of collecting feedback. Please read our ' +
'<a href=https://www.adacore.com/company/privacy>privacy policy</a> ' +
'if you want to know more about how we process your information and ' +
'your rights.';

export const FORM_NAME_ERROR_TEXT = 'Name field cannot be empty.';

export const FORM_EMAIL_ERROR_TEXT = 'Email address is invalid.';

export const FORM_MESSAGE_ERROR_TEXT = 'Message field cannot be empty.';
