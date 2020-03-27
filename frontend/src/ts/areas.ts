import $ from 'jquery';

import {CheckBox} from './components';
import * as Strings from './strings';
import {CheckOutput} from './types';
import * as util from './utilities';

/** Abstract class representing an Area **/
export abstract class Area {
  protected container: JQuery;
  public errorCount = 0;

  /**
   * Render the Area
   * @abstract
   * @return {JQuery} The object holding the Area
   */
  abstract render(): JQuery;

  /**
   * Add a line to the Area
   * @param {Array<string>} classes - The list of classes to add to the line
   * @param {string} text - The text to display on the line
   * @return {JQuery} The JQuery object of the newly created line
   */
  public add(classes: Array<string>, text: string): JQuery {
    const div = $('<div>');
    classes.map((c: string) => {
      div.addClass(c);
    });

    div.text(text);
    div.appendTo(this.container);
    return div;
  }

  /**
   * Empty the Area and reset to default state
   */
  public reset(): void {
    this.container.empty();
    this.errorCount = 0;
  }

  /**
   * Add a Console message to the Area
   * @param {string} text - The console message to add
   * @return {JQuery} The newly created line
   */
  public addConsole(text: string): JQuery {
    return this.add(['output_console'], '$ ' + text);
  }

  /**
   * Add an Info message to the Area
   * @param {string} text - The info message to add
   * @return {JQuery} The newly created line
   */
  public addInfo(text: string): JQuery {
    return this.add(['output_msg_info'], text);
  }

  /**
   * Add a Msg message to the Area
   * @param {string} text - The Msg message to add
   * @return {JQuery} The newly created line
   */
  public addMsg(text: string): JQuery {
    return this.add(['output_msg'], text);
  }

  /**
   * Add a Line message to the Area
   * @param {string} text - The Line message to add
   * @return {JQuery} The newly created line
   */
  public addLine(text: string): JQuery {
    return this.add(['output_line'], text);
  }
}

/**
 * Class representing an OutputArea
 * @extends Area
 */
export class OutputArea extends Area {
  private spinner: JQuery;

  /**
   * Construct an OutputArea
   */
  constructor() {
    super();
    this.container = $('<div>')
        .addClass('output_area');

    this.spinner = $('<div>')
        .addClass('spinner')
        .append(
            $('<div>').addClass('bounce1')
        )
        .append(
            $('<div>').addClass('bounce2')
        )
        .append(
            $('<div>').addClass('bounce3')
        );
  }

  /**
   * Render the OutputArea
   * @return {JQuery} The container of the OutputArea
   */
  public render(): JQuery {
    return this.container;
  }

  /**
   * Add an Error message to the OutputArea
   * @param {string} message - The error message to add
   */
  public addError(message: string): void {
    this.add(['output_error'], message);
  }

  /**
   * Add a LabStatus message to the Area
   * @param {boolean} status - The Pass/Fail status to add
   */
  public addLabStatus(status: boolean): void {
    if (status) {
      this.add(['lab_status'], Strings.LAB_COMPLETE_LABEL);
    } else {
      this.add(['lab_status'], Strings.LAB_FAILED_LABEL);
    }
  }

  /**
   * Show or hide the loading spinner
   * @param {boolean} show - Show or hide
   */
  public showSpinner(show: boolean): void {
    if (show) {
      this.spinner.appendTo(this.container);
    } else {
      this.spinner.remove();
    }
  }

  /**
   * Get the number of errors recorded in the OutputArea
   * @return {number} The number of errors counted
   */
  public getErrCount(): number {
    return this.errorCount;
  }
}

/**
 * Class representing a LabArea
 * @extends Area
 */
class LabArea extends Area {
  private readonly ref: number;
  private readonly wrapper: JQuery;
  private button: JQuery;

  /**
   * Constructs a LabArea
   * @param {number} ref - The ref number of the lab result
   */
  constructor(ref: number) {
    super();
    this.wrapper = $('<div>').addClass('acc_wrapper');
    this.ref = ref;

    this.button = $('<button>')
        .addClass('accordion')
        .appendTo(this.wrapper)
        .append(
            $('<span>').text(Strings.TEST_CASE_LABEL + ' #' + this.ref)
        ).on('click', () => {
          this.button.toggleClass('active');
          this.container.toggle();
        });
    this.container = $('<div>')
        .addClass('lab_test_case')
        .appendTo(this.wrapper);
  }

  /**
   * Render the LabArea
   * @return {JQuery} Returns the JQuery object holding the LabArea
   */
  public render(): JQuery {
    return this.wrapper;
  }

  /**
   * Adds a result to the LabArea
   * @param {CheckOutput.TestResult} result - the result to add
   */
  public addResults(result: CheckOutput.TestResult): void {
    if (result.status == 'Success') {
      this.button.addClass('lab_test_success');
      this.container.addClass('lab_test_success');
    } else {
      this.button.addClass('lab_test_failed');
      this.container.addClass('lab_test_failed');
    }

    const caseDiv: JQuery = $('<div>')
        .addClass('lab_results')
        .appendTo(this.container);

    $('<div>')
        .addClass('lab_test_msg')
        .addClass('lab_test_input')
        .append(
            $('<span>')
                .addClass('lab_test_msg_title')
                .text(Strings.LAB_TEST_INPUT_LABEL + ':')
        )
        .append(
            $('<code>').text(result.in)
        )
        .appendTo(caseDiv);

    $('<div>')
        .addClass('lab_test_msg')
        .addClass('lab_test_output')
        .append(
            $('<span>')
                .addClass('lab_test_msg_title')
                .text(Strings.LAB_TEST_OUTPUT_LABEL + ':')
        )
        .append(
            $('<code>').text(result.out)
        )
        .appendTo(caseDiv);

    $('<div>')
        .addClass('lab_test_msg')
        .addClass('lab_test_actual')
        .append(
            $('<span>')
                .addClass('lab_test_msg_title')
                .text(Strings.LAB_TEST_ACTUAL_LABEL + ':')
        )
        .append(
            $('<code>').text(result.actual)
        )
        .appendTo(caseDiv);

    $('<div>')
        .addClass('lab_test_msg')
        .addClass('lab_test_status')
        .append(
            $('<span>')
                .addClass('lab_test_msg_title')
                .text(Strings.LAB_TEST_STATUS_LABEL + ':')
        )
        .append(
            $('<code>').text(result.status)
        )
        .appendTo(caseDiv);
  }

  /**
   * Returns the ref number for the LabArea
   * @return {number} The ref number
   */
  public getRef(): number {
    return this.ref;
  }
}

/** Class representing the LabContainer */
export class LabContainer {
  private labList: Array<LabArea> = [];
  private readonly container: JQuery;

  /**
   * Constructs a LabContainer
   */
  constructor() {
    this.container = $('<div>').addClass('lab_area');
  }

  /**
   * Returns the LabArea corresponding to the ref number
   * @param {number} ref - The ref number to lookup
   * @return {LabArea} the LabArea with the ref number
   */
  public getLabArea(ref: number): LabArea {
    for (const l of this.labList) {
      if (l.getRef() == ref) {
        return l;
      }
    }
    const newLab: LabArea = new LabArea(ref);
    this.labList.push(newLab);
    return newLab;
  }

  /**
   * Renders the LabContainer
   * @return {JQuery} the JQuery object containing the LabContainer
   */
  public render(): JQuery {
    return this.container;
  }

  /**
   * Process the results from a lab submission
   * @param {CheckOutput.LabOutput} data - The lab submission data
   * @return {boolean} the success/fail of the lab
   */
  public processResults(data: CheckOutput.LabOutput): boolean {
    for (const index in data.cases) {
      if ({}.hasOwnProperty.call(data.cases, index)) {
        const test: CheckOutput.TestResult =
          (data.cases[index] as unknown) as CheckOutput.TestResult;
        const la = this.getLabArea(parseInt(index));
        la.addResults(test);
      }
    }

    return data.success;
  }

  /**
   * Empty and reset the LabContainer
   */
  public reset(): void {
    this.container.empty();
    this.labList = [];
  }

  /**
   * Sort the LabAreas in the LabContainer
   */
  public sort(): void {
    const sorted = this.labList.sort((lhs, rhs): number => {
      return lhs.getRef() - rhs.getRef();
    });
    sorted.map((l) => {
      return l.render().appendTo(this.container);
    });
  }
}

/** Class representing the CLIArea */
export class CLIArea {
  private textArea: JQuery;
  private checkBox: CheckBox;

  /**
   * Construct CLIArea
   */
  constructor() {
    this.textArea = $('<textarea>')
        .addClass('custom_input')
        .attr('name', 'custom_input')
        .attr('rows', '4')
        .attr('cols', '6')
        .hide();
    this.checkBox =
    new CheckBox(Strings.CUSTOM_INPUT_LABEL,
        undefined, ['custom_check_container'],
        Strings.CUSTOM_INPUT_TOOLTIP);

    this.checkBox.getCheckBox().on('change', () => {
      if (this.checkBox.checked()) {
        this.textArea.show();
      } else {
        this.textArea.hide();
      }
    });
  }

  /**
   * Renders the CLIArea
   * @param {JQuery} parent - the JQuery object to insert the CLIArea into
   */
  public render(parent: JQuery): void {
    this.textArea.appendTo(parent);
    this.checkBox.render().appendTo(parent);
  }

  /**
   * Checks if the CLI checkbox is checked
   * @return {boolean} true if the checkbox is checked
   */
  public enabled(): boolean {
    return this.checkBox.checked();
  }

  /**
   * Get the content of the textarea
   * @return {string} the textarea string
   */
  public getContent(): string {
    const ret: string | number | string[] = this.textArea.val();

    if (util.isString(ret)) {
      return ret as string;
    } else if (util.isNumber(ret)) {
      return ret.toString();
    } else {
      return (ret as string[]).join();
    }
  }
}
