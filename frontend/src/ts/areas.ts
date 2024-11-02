import * as Strings from './strings.ts';
import {CheckOutput} from './server-types.ts';

/** Class representing an Area */
export class Area {
  protected readonly container: HTMLDivElement;

  /**
   * Construct an Area
   * @param {HTMLDivElement} elem - A parent element.
   */
  constructor(elem: HTMLDivElement) {
    this.container = elem;
  }

  /**
   * The event callback signature for clickable divs
   * @callback eventCallback
   */

  /**
   * Add a line to the Area
   * @param {Array<string>} classes - The list of classes to add to the line
   * @param {string} text - The text to display on the line
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public add(classes: Array<string>, text: string, cb?: () => void): void {
    const div = document.createElement('div');
    classes.map((c: string) => {
      div.classList.add(c);
    });

    div.textContent = text;
    this.container.appendChild(div);

    if (cb) {
      div.addEventListener('click', cb);
    }
  }

  /**
   * Empty the Area and reset to default state
   */
  public reset(): void {
    while (this.container.firstChild) {
      this.container.removeChild(this.container.firstChild);
    }
  }

  /**
   * Add a Console message to the Area
   * @param {string} text - The console message to add
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public addConsole(text: string, cb?: () => void): void {
    this.add(['output_console'], '$ ' + text, cb);
  }

  /**
   * Add an Info message to the Area
   * @param {string} text - The info message to add
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public addInfo(text: string, cb?: () => void): void {
    this.add(['output_msg_info'], text, cb);
  }

  /**
   * Add a Msg message to the Area
   * @param {string} text - The Msg message to add
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public addMsg(text: string, cb?: () => void): void {
    this.add(['output_msg'], text, cb);
  }

  /**
   * Add a Line message to the Area
   * @param {string} text - The Line message to add
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public addLine(text: string, cb?: () => void): void {
    this.add(['output_line'], text, cb);
  }
}

/**
 * Class representing an OutputArea
 * @augments Area
 */
export class OutputArea extends Area {
  private spinner: HTMLDivElement;

  /**
   * Construct an OutputArea
   * @param {HTMLDivElement} elem (optional) - A parent element. If none is
   *  specified, one is created.
   */
  constructor(elem: HTMLDivElement) {
    super(elem);

    this.spinner = document.createElement('div');
    this.spinner.classList.add('spinner');
    for (let i = 1; i < 4; i++) {
      const b = document.createElement('div');
      b.classList.add('bounce' + i);
      this.spinner.appendChild(b);
    }
  }

  /**
   * Add an Error message to the OutputArea
   * @param {string} message - The error message to add
   * @param {eventCallback} cb (optional) - The on click function to register
   */
  public addError(message: string, cb?: () => void): void {
    this.add(['output_error'], message, cb);
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
      this.container.appendChild(this.spinner);
    } else {
      if (this.container.contains(this.spinner)) {
        this.container.removeChild(this.spinner);
      }
    }
  }
}

/**
 * Class representing a LabArea
 * @augments Area
 */
export class LabArea extends Area {
  private readonly ref: number;
  private readonly wrapper: HTMLDivElement;
  private button: HTMLButtonElement;

  /**
   * Constructs a LabArea
   * @param {number} ref - The ref number of the lab result
   * @param {HTMLDivElement} container - The container element
   */
  constructor(ref: number, container: HTMLDivElement) {
    super(container);
    this.wrapper = document.createElement('div');
    this.wrapper.classList.add('acc_wrapper');
    this.ref = ref;

    this.button = document.createElement('button');
    this.button.classList.add('accordion');
    this.wrapper.appendChild(this.button);

    const span = document.createElement('span');
    span.textContent = Strings.TEST_CASE_LABEL + ' #' + this.ref;
    this.button.appendChild(span);

    this.button.addEventListener('click', () => {
      this.button.classList.toggle('active');
      if (this.container.style.display == '' ||
          this.container.style.display == 'block') {
        this.container.style.display = 'none';
      } else {
        this.container.style.display = 'block';
      }
    });

    this.container.classList.add('lab_test_case');
    this.container.style.display = 'none';
    this.wrapper.appendChild(this.container);
  }

  /**
   * Render the LabArea
   * @returns {HTMLDivElement} Returns the obj holding the LabArea
   */
  public render(): HTMLDivElement {
    return this.wrapper;
  }

  /**
   * Create the DOM for the lab result
   * @param {string} divClass - the class to apply to the div
   * @param {string} spanText - the text to put in the span
   * @param {string} result - the result text to display
   * @returns {HTMLDivElement} Returns the obj for the lab result
   */
  private resultHelper(divClass: string, spanText: string,
      result: string): HTMLDivElement {
    const label = document.createElement('div');
    label.classList.add('lab_test_msg', divClass);

    const labelmsg = document.createElement('span');
    labelmsg.classList.add('lab_test_msg_title');
    labelmsg.textContent = spanText + ':';
    label.appendChild(labelmsg);

    const labelCode = document.createElement('code');
    labelCode.textContent = result;
    label.appendChild(labelCode);

    return label;
  }

  /**
   * Adds a result to the LabArea
   * @param {CheckOutput.TestResult} result - the result to add
   */
  public addResults(result: CheckOutput.TestResult): void {
    if (result.status == 'Success') {
      this.button.classList.add('lab_test_success');
      this.container.classList.add('lab_test_success');
    } else {
      this.button.classList.add('lab_test_failed');
      this.container.classList.add('lab_test_failed');
    }

    const caseDiv = document.createElement('div');
    caseDiv.classList.add('lab_results');
    this.container.appendChild(caseDiv);

    caseDiv.appendChild(this.resultHelper(
        'lab_test_input', Strings.LAB_TEST_INPUT_LABEL, result.in));
    caseDiv.appendChild(this.resultHelper(
        'lab_test_output', Strings.LAB_TEST_OUTPUT_LABEL, result.out));
    caseDiv.appendChild(this.resultHelper(
        'lab_test_actual', Strings.LAB_TEST_ACTUAL_LABEL, result.actual));
    caseDiv.appendChild(this.resultHelper(
        'lab_test_status', Strings.LAB_TEST_STATUS_LABEL, result.status));
  }

  /**
   * Returns the ref number for the LabArea
   * @returns {number} The ref number
   */
  public getRef(): number {
    return this.ref;
  }
}

/**
 * Makes a lab area and creates the container element
 * @param {number} ref - the lab reference
 * @returns {LabArea} - The created lab area
 */
export function makeLabArea(ref: number): LabArea {
  const container = document.createElement('div');
  return new LabArea(ref, container);
}

/** Class representing the LabContainer */
export class LabContainer {
  private labList: Array<LabArea> = [];
  private readonly container: HTMLDivElement;

  /**
   * Constructs a LabContainer
   * @param {HTMLDivElement} elem - The parents div to render in
   */
  constructor(elem: HTMLDivElement) {
    this.container = elem;
  }

  /**
   * Returns the LabArea corresponding to the ref number
   * @param {number} ref - The ref number to lookup
   * @returns {LabArea} the LabArea with the ref number
   */
  public getLabArea(ref: number): LabArea {
    for (const l of this.labList) {
      if (l.getRef() == ref) {
        return l;
      }
    }
    const newLab = makeLabArea(ref);
    this.labList.push(newLab);
    return newLab;
  }

  /**
   * Process the results from a lab submission
   * @param {CheckOutput.LabOutput} data - The lab submission data
   * @returns {boolean} the success/fail of the lab
   */
  public processResults(data: CheckOutput.LabOutput): boolean {
    for (const index in data.cases) {
      /* istanbul ignore next */
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
    while (this.container.firstChild) {
      this.container.removeChild(this.container.firstChild);
    }
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
      const render = l.render();
      this.container.appendChild(render);
      return render;
    });
  }
}
