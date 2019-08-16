export namespace Strings {
  // List of known modes, and the corresponding button labels
  export interface buttonMode {
      buttonText : string;
      tooltip : string;
  }

  export const modeDictionary : {[mode : string] : buttonMode } = {
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

  export const TEST_CASE_LABEL = 'Test Case';

  export const RESET_TOOLTIP = 'Reset editor to default state';
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
}
