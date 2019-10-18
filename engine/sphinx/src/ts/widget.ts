import $ from 'jquery';
import * as ace from 'brace';
import 'brace/mode/ada';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

import {CheckBox, Button, Tabs} from './components';
import * as Strings from './strings';
import * as Types from './types';
import * as util from './utilities';

/** Class representing an Editor **/
class Editor {
  private container: JQuery;
  private editor: ace.Editor;
  private initialContents: string;
  private basename: string;
  private tab: JQuery;

  /**
   * Create an Editor
   * @param {Types.Resource} resource - The resource to load into the editor
   */
  constructor(resource: Types.Resource) {
    this.container = $('<div>').addClass('editor-container');
    this.editor = ace.edit(this.container[0]);

    // Set the mode
    if (resource.basename.match(/.ad[sb]$/)) {
      this.editor.session.setMode('ace/mode/ada');
    } else {
      this.editor.session.setMode('ace/mode/c_cpp');
    }
    this.editor.$blockScrolling = Infinity;

    // ... and their contents
    this.editor.setValue(resource.contents);
    this.editor.setShowPrintMargin(false);
    this.editor.gotoLine(1);

    this.initialContents = resource.contents;
    this.basename = resource.basename;

    this.editor.setOptions({
      highlightActiveLine: false,
      fontSize: 13,
      tabSize: 3,
      useSoftTabs: true,
      theme: 'ace/theme/tomorrow',
      minLines: this.editor.session.doc.getLength(),
      maxLines: 50,
    });

    this.editor.resize();
    // place the cursor at 1,1
    this.editor.selection.moveCursorTo(0, 0);

    // clear undo stack to avoid undoing everything we just did
    this.editor.getSession().getUndoManager().reset();

    this.editor.renderer.setScrollMargin(5, 5, 0, 0);
  }

  /**
   * Set the theme of the editor
   * @param {string} theme - The ace theme to load
   */
  public setTheme(theme: string): void {
    this.editor.setTheme(theme);
  }

  /**
   * Reset the editor back to default state
   */
  public reset(): void {
    this.editor.setValue(this.initialContents);
    this.editor.gotoLine(1);
  }

  /**
   * Render the editor
   * @return {JQuery} The JQuery object holding the editor
   */
  public render(): JQuery {
    return this.container;
  }

  /**
   * Get the resource from the Editor
   * @return {Types.Resource} The Editor filename and contents
   */
  public getResource(): Types.Resource {
    return {basename: this.basename, contents: this.editor.getValue()};
  }

  /**
   * Store the tab holding this editor
   * @param {JQuery} tab - The tab holding this editor
   */
  public setTab(tab: JQuery): void {
    this.tab = tab;
  }

  /**
   * Return the tab holding this editor
   * @return {JQuery} The tab holding this editor
   */
  public getTab(): JQuery {
    return this.tab;
  }

  /**
   * Jumo the editor to row:col
   * @param {number} line - The line number to goto
   * @param {number} col - The col + 1 to goto
   */
  public gotoLine(line: number, col: number): void {
    this.editor.gotoLine(line, col - 1, true);
    this.editor.focus();
  }
}

/** Abstract class representing an Area **/
abstract class Area {
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
class OutputArea extends Area {
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
  private ref: number;
  private wrapper: JQuery;
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
        ).click(() => {
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
   * @param {Types.CheckOutput.TestResult} result - the result to add
   */
  public addResults(result: Types.CheckOutput.TestResult): void {
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
class LabContainer {
  private labList: Array<LabArea> = [];
  private container: JQuery;

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
   * @param {Types.CheckOutput.LabOutput} data - The lab submission data
   * @return {boolean} the success/fail of the lab
   */
  public processResults(data: Types.CheckOutput.LabOutput): boolean {
    for (const index in data.cases) {
      if ({}.hasOwnProperty.call(data.cases, index)) {
        const test: Types.CheckOutput.TestResult =
          (data.cases[index] as unknown) as Types.CheckOutput.TestResult;
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
class CLIArea {
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

    this.checkBox.getCheckBox().change(() => {
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

/** The Widget class */
export class Widget {
  private editors: Array<Editor> = [];
  private container: JQuery;
  private tabs: Tabs = new Tabs();
  private outputContainer: JQuery;
  private outputArea: OutputArea = new OutputArea();
  private labContainer: LabContainer;
  private cliArea: CLIArea;

  private buttons: Array<Button> = [];

  private linesRead = 0;
  private server: string;

  private shadowFiles: Array<Types.Resource> = [];

  /**
   * Constructs the Widget
   * @param {JQuery} container - the container for the widget
   * @param {string} server - the server address:port
   */
  constructor(container: JQuery, server: string) {
    const resources: Array<Types.Resource> = [];
    this.server = server;
    this.container = container;

    for (const file of this.container.children('.file')) {
      const a: Types.Resource = {basename: $(file).attr('basename'),
        contents: $(file).text()};
      $(file).text('');
      resources.push(a);
    }

    for (const file of this.container.children('.shadow_file')) {
      const a: Types.Resource = {basename: $(file).attr('basename'),
        contents: $(file).text()};
      $(file).text('');
      this.shadowFiles.push(a);
    }

    // Then fill the contents of the tabs
    resources.map((file) => {
      const ed = new Editor(file);
      this.editors.push(ed);

      const tab = this.tabs.addTab(file.basename, ed.render());
      ed.setTab(tab);
    });

    if (container.attr('lab')) {
      this.container.attr('prove_button', 'true');
      this.container.attr('run_button', 'true');
      this.container.attr('submit_button', 'true');
      this.container.attr('cli_input', 'true');

      this.labContainer = new LabContainer();
    } else {
      this.labContainer = null;
    }

    if (container.attr('cli_input')) {
      this.cliArea = new CLIArea();
    } else {
      this.cliArea = null;
    }

    for (const mode in Strings.modeDictionary) {
      if (this.container.attr(mode + '_button')) {
        const btn: Button = new Button([],
            Strings.modeDictionary[mode].tooltip,
            Strings.modeDictionary[mode].buttonText);
        btn.render().click((event: JQuery.ClickEvent) => {
          this.buttonCB(event, mode);
        });
        this.buttons.push(btn);
      }
    }
  }

  /* eslint-disable @typescript-eslint/ban-types,
  @typescript-eslint/no-explicit-any */
  /**
   * The communication interface with the server
   * @param {Object} payload - the payload to send
   * @param {string} url - the url to query
   * @return {JQuery.Promise} return the ajax promise for cb binding
   */
  private communicate(payload: Object, url: string): JQuery.Promise<any> {
    return $.ajax({
      url: this.server + '/' + url + '/',
      data: JSON.stringify(payload),
      type: 'POST',
      dataType: 'json',
      contentType: 'application/json; charset=UTF-8',
      timeout: 4000,
    }) as JQuery.Promise<any>;
  }
  /* eslint-enable @typescript-eslint/ban-types,
  @typescript-eslint/no-explicit-any */

  /**
   * The main callback for the widget buttons
   * @param {JQuery.ClickEvent} event - the event that triggered the CB
   * @param {string} mode - the mode of the button that triggered the event
   */
  private buttonCB(event: JQuery.ClickEvent, mode: string): void {
    if (event.target.disabled) {
      return;
    }

    this.buttons.map((b) => {
      b.disabled = true;
    });

    this.outputArea.reset();
    if (this.labContainer != null) {
      this.labContainer.reset();
    }

    this.outputArea.add(['output_info', 'console_output'],
        Strings.CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.showSpinner(true);

    const files: Array<Types.Resource> = [];
    this.editors.map((e) => {
      files.push(e.getResource());
    });

    // grab cli input and put in file CLI_FILE
    if (this.cliArea != null) {
      if (this.cliArea.enabled()) {
        files.push({
          basename: Strings.CLI_FILE,
          contents: this.cliArea.getContent(),
        });
      }
    }

    const labName = this.container.attr('lab_name');
    const serverData: Types.RunProgram.TS = {
      files: files.concat(this.shadowFiles),
      mode: mode,
      lab: labName,
    };

    this.linesRead = 0;
    // eslint-disable-next-line @typescript-eslint/ban-types
    this.communicate(serverData as Object, 'run_program')
        .done((json: Types.RunProgram.FS) => {
          if (json.identifier == '') {
            this.resetServerReq();
            this.outputArea.addError(json.message);
          } else {
            this.getOutputFromIdentifier(json);
          }
        })
        .fail((xhr, status, errorThrown) => {
          this.resetServerReq();
          this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
          console.log('Error: ' + errorThrown);
          console.log('Status: ' + status);
          console.dir(xhr);
        });
  }

  /**
   * Get the run output using the return identifier from the button CB
   * @param {Types.RunProgram.FS} json - the json data returned from button CB
   */
  private getOutputFromIdentifier(json: Types.RunProgram.FS): void {
    const data: Types.CheckOutput.TS = {
      identifier: json.identifier,
      tempd: json.tempd,
      read: this.linesRead,
    };

    // eslint-disable-next-line @typescript-eslint/ban-types
    this.communicate(data as Object, 'check_output')
        .done((data: Types.CheckOutput.FS) => {
          this.linesRead += this.processCheckOutput(data);

          if (!data.completed) {
            // We have not finished processing the output: call this again
            setTimeout(() => {
              this.getOutputFromIdentifier(json);
            }, 250);
          } else {
            if (this.labContainer != null) {
              this.labContainer.sort();
            }
          }
        })
        .fail((xhr, status, errorThrown) => {
          this.outputArea.addError(Strings.MACHINE_NOT_RESPONDING_LABEL);
          console.log('Error: ' + errorThrown);
          console.log('Status: ' + status);
          console.dir(xhr);
        })
        .fail((json) => {
          const message = ((json as unknown) as Types.CheckOutput.FS).message;
          this.resetServerReq();
          this.outputArea.addError(message);
        });
  }

  /**
   * Process the output from "check_output" ajax request
   * @param {Types.CheckOutput.FS} data - The data from check_output
   * @return {number} the number of lines read by this function
   */
  private processCheckOutput(data: Types.CheckOutput.FS): number {
    let readLines = 0;

    data.output.map((ol) => {
      let homeArea: Area;
      readLines++;

      if (ol.ref != null) {
        homeArea = this.labContainer.getLabArea(ol.ref);
      } else {
        homeArea = this.outputArea;
      }

      switch (ol.msg.type) {
        case 'console': {
          homeArea.addConsole(ol.msg.data);
          break;
        }
        case 'internal_error':
          ol.msg.data += ' ' + Strings.INTERNAL_ERROR_MESSAGE;
          // Intentional: fall through
        case 'stderr':
        case 'stdout': {
          const msg = ol.msg.data;
          const regex = /^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/;

          const matchFound: Array<string> = msg.match(regex);
          let div: JQuery;

          if (matchFound) {
            if (matchFound[4].indexOf(' info:') == 0) {
              div = homeArea.addInfo(msg);
            } else {
              div = homeArea.addMsg(msg);
              homeArea.errorCount++;
            }

            // Lines that contain a sloc are clickable:
            div.click(() => {
              const basename = matchFound[1];
              this.editors.map((e) => {
                if (basename == e.getResource().basename) {
                  // Switch to the tab that contains the editor
                  e.getTab().click();

                  // Jump to the corresponding line
                  e.gotoLine(parseInt(matchFound[2]), parseInt(matchFound[3]));
                }
              });
            });
          } else {
            homeArea.addLine(msg);
          }
          break;
        }
        case 'lab': {
          const result =
            this.labContainer.processResults(
                (ol.msg.data as unknown) as Types.CheckOutput.LabOutput);
          this.outputArea.addLabStatus(result);
          break;
        }
        default: {
          // TODO: this branch should probably throw an error
          homeArea.addLine(ol.msg.data);
          break;
        }
      }
    });

    if (data.completed) {
      this.linesRead = 0;
      this.buttons.map((b) => {
        b.disabled = false;
      });

      if (data.status != 0) {
        this.outputArea.addError(Strings.EXIT_STATUS_LABEL +
            ': ' + data.status);
      }

      this.outputArea.showSpinner(false);
    }

    return readLines;
  }

  /**
   * Reset the widget after a run_program request
   */
  private resetServerReq(): void {
    this.buttons.map((b) => {
      b.disabled = false;
    });

    this.linesRead = 0;
  }

  /**
   * Reset the editors, outputArea, and labContainer
   */
  private resetEditors(): void {
    this.linesRead = 0;
    this.outputArea.reset();
    if (this.labContainer != null) {
      this.labContainer.reset();
    }
    this.editors.map((e) => {
      e.reset();
    });
  }

  /**
   * Render the settings bar for the widget
   * @return {JQuery} the rendered settings bar
   */
  private renderSettingsBar(): JQuery {
    const settingsBar = $('<div>')
        .addClass('settings-bar');

    const dropdownContainer = $('<div>')
        .addClass('dropdown-container')
        .addClass('settingsbar-item')
        .appendTo(settingsBar);

    $('<button>')
        .addClass('dropdown-btn')
        .append(
            $('<i>').addClass('fas').addClass('fa-cog')
        )
        .appendTo(dropdownContainer);

    const dropdownContent = $('<div>')
        .addClass('dropdown-content')
        .appendTo(dropdownContainer);

    const tabSetting: CheckBox =
        new CheckBox(Strings.SETTINGS_TABBED_EDITOR_LABEL, dropdownContent);
    tabSetting.getCheckBox().prop('checked', true).change(() => {
      if (tabSetting.checked()) {
        this.tabs.show(true);
      } else {
        this.tabs.show(false);
      }
    });

    const themeSetting: CheckBox =
        new CheckBox(Strings.SETTINGS_THEME_EDITOR_LABEL, dropdownContent);
    themeSetting.getCheckBox().change(() => {
      let theme = 'ace/theme/tomorrow';
      if (themeSetting.checked()) {
        theme = 'ace/theme/tomorrow_night';
      }
      this.editors.map((e) => {
        e.setTheme(theme);
      });
    });

    $('<button>')
        .attr('type', 'button')
        .addClass('settingsbar-item')
        .addClass('reset-btn')
        .attr('title', Strings.RESET_TOOLTIP)
        .append(
            $('<i>').addClass('fas').addClass('fa-undo')
        )
        .appendTo(settingsBar)
        .click((event: JQuery.ClickEvent) => {
          if (event.target.disabled) {
            return;
          }
          if (window.confirm("Are you sure?")) {
            this.resetEditors();
          }
        });

    $('<button>')
        .attr('type', 'button')
        .addClass('settingsbar-item')
        .addClass('download-btn')
        .attr('title', Strings.DOWNLOAD_TOOLTIP)
        .append(
            $('<i>').addClass('fas').addClass('fa-file-download')
        )
        .appendTo(settingsBar)
        .click(() => {
          this.editors.map((e): void => {
            const resource: Types.Resource = e.getResource();
            const blob: Blob =
              new Blob([resource.contents], {type: 'text/plain'});
            const objURL: string = URL.createObjectURL(blob);

            const a = $('<a>')
                .attr('href', objURL)
                .attr('download', resource.basename)
                // .hide()
                .appendTo('body');
            a[0].click();
            a.remove();

            URL.revokeObjectURL(objURL);
          });
        });

    return settingsBar;
  }

  /**
   * Render the widget by putting it into this.container
   */
  public render(): void {
    this.tabs.render(this.container);
    this.renderSettingsBar().appendTo(this.container);
    const row = $('<div>')
        .addClass('row output_row')
        .appendTo(this.container);

    const butCol = $('<div>')
        .addClass('col-md-3')
        .appendTo(row);

    if (this.labContainer != null) {
      this.cliArea.render(butCol);
    }

    this.buttons.map((b) => {
      b.render().appendTo(butCol);
    });

    const outCol = $('<div>')
        .addClass('col-md-9')
        .appendTo(row)
        .append(this.outputArea.render());

    if (this.labContainer != null) {
      this.labContainer.render().appendTo(outCol);
    }
  }
}
