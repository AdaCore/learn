import $ from 'jquery';
import 'whatwg-fetch';

import {Button, CheckBox, Tabs} from './components';
import {Editor, EditorTheme} from './editor';
import * as Strings from './strings';
import * as Types from './types';
import * as util from './utilities';


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

/** The Widget class */
export class Widget {
  private editors: Array<Editor> = [];
  protected readonly container: JQuery;
  private readonly name: string;
  private tabs: Tabs = new Tabs();
  private outputContainer: JQuery;
  protected outputArea: OutputArea = new OutputArea();

  private buttons: Array<Button> = [];
  private buttonsDisabled = false;

  protected lab = false;

  private dlType: Types.DownloadType = Types.DownloadType.Client;

  private readonly server: string;

  private shadowFiles: Array<Types.Resource> = [];

  protected buttonGroup: JQuery;
  protected outputGroup: JQuery;

  /**
   * Constructs the Widget
   * @param {JQuery} container - the container for the widget
   * @param {string} server - the server address:port
   */
  constructor(container: JQuery, server: string) {
    const resources: Array<Types.Resource> = [];
    this.server = server;
    this.container = container;

    // Read attributes from container object to initialize members
    this.name = container.attr('name');

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

    // fill the contents of the tabs
    resources.map((file) => {
      const ed = new Editor(file);
      this.editors.push(ed);

      const tab = this.tabs.addTab(file.basename, ed.render());
      ed.setTab(tab);
    });

    // Check which buttons are enabled on container and populate
    for (const mode in Strings.modeDictionary) {
      if (this.container.attr(mode + '_button')) {
        this.addButton(mode);
      }
    }

    // if this widget doesn't have a name defined, don't allow for download
    if (util.isUndefined(this.name)) {
      this.dlType = Types.DownloadType.None;
    }
  }

  /**
   * Add a button to the button list
   * @param {string} mode - the mode of button to add
   */
  protected addButton(mode: string): void {
    // if there are any buttons, the dltype needs to be server
    this.dlType = Types.DownloadType.Server;

    const btn: Button = new Button([],
        Strings.modeDictionary[mode].tooltip,
        Strings.modeDictionary[mode].buttonText);
    btn.registerEvent('click', async (event: JQuery.ClickEvent) => {
      if (this.buttonsDisabled) {
        return;
      }
      this.buttonsDisabled = true;
      try {
        await this.buttonCB(event, mode);
      } catch (error) {
        this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
        console.error('Error:', error);
      } finally {
        this.buttonsDisabled = false;
      }
    });

    this.buttons.push(btn);
  }

  /**
   * Collect the resources loaded in the widget and return as list
   * @return {Array<Types.Resource>} return the widget resources
   */
  protected collectResources(): Array<Types.Resource> {
    const files: Array<Types.Resource> = [];
    this.editors.map((e) => {
      files.push(e.getResource());
    });
    return files.concat(this.shadowFiles);
  }

  /**
   * Perform a POST via the fetch method to retrieve json data
   * @param {string} data - the json to send
   * @param {string} url - the url suffix to send the fetch to
   * @typeparam R - This is the json object type to send
   * @typeparam T - This is the json object type to return
   * @return {T} returns a promise to a json object of type T
   */
  private async fetchJSON<R, T>(data: R, url: string): Promise<T> {
    const response = await fetch(this.server + '/' + url + '/', {
      method: 'POST',
      mode: 'cors',
      cache: 'no-cache',
      redirect: 'follow',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json',
      },
    });
    if (!response.ok) {
      throw new Error('Status: ' + response.status + ' Msg: ' +
        response.statusText);
    }
    return await response.json();
  }

  /**
   * Perform a POST via the fetch method to retrieve a file
   * @param {string} data - the json to send
   * @param {string} url - the url suffix to send the fetch to
   * @return {Promise<Types.Download.FS>} returns a promise to a dl file
   */
  private async fetchBlob(data: Types.Download.TS,
      url: string): Promise<Types.Download.FS> {
    const response = await fetch(this.server + '/' + url + '/', {
      method: 'POST',
      mode: 'cors',
      cache: 'no-cache',
      redirect: 'follow',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json',
      },
    });
    if (!response.ok) {
      throw new Error('Status: ' + response.status + ' Msg: ' +
        response.statusText);
    }
    const blob: Blob = await response.blob();
    const disposition = response.headers.get('content-disposition');
    const filename = disposition.match(
        /filename[^;=\n]*=((['"]).*?\2|[^;\n]*)/)[1];

    return {
      blob: blob,
      filename: filename,
    };
  }

  /**
   * The main callback for the widget buttons
   * @param {JQuery.ClickEvent} event - the event that triggered the CB
   * @param {string} mode - the mode of the button that triggered the event
   */
  protected async buttonCB(event: JQuery.ClickEvent,
      mode: string): Promise<void> {
    this.outputArea.reset();

    this.outputArea.add(['output_info', 'console_output'],
        Strings.CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.showSpinner(true);

    const files: Array<Types.Resource> = this.collectResources();

    const serverData: Types.RunProgram.TS = {
      files: files,
      mode: mode,
      name: this.name,
      lab: this.lab,
    };

    const json =
    await this.fetchJSON<Types.RunProgram.TS, Types.RunProgram.FS>(serverData,
        'run_program');

    if (json.identifier == '') {
      throw new Error(json.message);
    }

    return new Promise(async (resolve) => {
      await this.getOutputFromIdentifier(json);
      resolve();
    });
  }

  /**
   * The download example callback
   */
  private async downloadExample(): Promise<void> {
    const files: Array<Types.Resource> = this.collectResources();
    let blobs: Blob[];
    let filenames: string[];

    switch (this.dlType) {
      case Types.DownloadType.None:
        return;
      case Types.DownloadType.Server:
        const serverData: Types.Download.TS = {
          files: files,
          name: this.name,
        };

        const ret = await this.fetchBlob(serverData, 'download');
        blobs.push(ret.blob);
        filenames.push(ret.filename);

        break;
      case Types.DownloadType.Client:
        this.editors.map((e): void => {
          const resource: Types.Resource = e.getResource();
          blobs.push(new Blob([resource.contents], {type: 'text/plain'}));
          filenames.push(resource.basename);
        });
        break;
    }

    for (const [blob, name] of util.zip(blobs, filenames)) {
      const objURL: string = URL.createObjectURL(blob);

      const a = $('<a>')
          .attr('href', objURL)
          .attr('download', name)
          // .hide()
          .appendTo('body');
      a[0].click();
      a.remove();

      URL.revokeObjectURL(objURL);
    }
  }

  /**
   * Get the run output using the return identifier from the button CB
   * @param {Types.RunProgram.FS} json - the json data returned from button CB
   * @param {number} lRead - the number of lines already read from the stream
   */
  private async getOutputFromIdentifier(json: Types.RunProgram.FS, lRead = 0):
      Promise<void> {
    const data: Types.CheckOutput.TS = {
      identifier: json.identifier,
      read: lRead,
    };

    const rdata =
      await this.fetchJSON<Types.CheckOutput.TS, Types.CheckOutput.FS>(data,
          'check_output');

    lRead += this.processCheckOutput(rdata);

    if (!rdata.completed) {
      // We have not finished processing the output: call this again
      return new Promise((resolve) => {
        setTimeout(async () => {
          await this.getOutputFromIdentifier(json, lRead);
          resolve();
        }, 250);
      });
    }
  }

  /**
   * Returns the correct Area to place data in
   * @param {number} ref - should be null for Widget
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(ref: number): Area {
    if (ref != null) {
      throw new Error('Malformed data packet has ref in non-lab.');
    }
    return this.outputArea;
  }

  /**
   * Handle the msg data coming back from server
   * @param {Types.CheckOutput.RunMsg} msg - the returned msg
   * @param {Area} homeArea - the area to place the rendered msg
   */
  protected handleMsgType(msg: Types.CheckOutput.RunMsg, homeArea: Area): void {
    switch (msg.type) {
      case 'console': {
        homeArea.addConsole(msg.data);
        break;
      }
      case 'internal_error':
        msg.data += ' ' + Strings.INTERNAL_ERROR_MESSAGE;
        // Intentional: fall through
      case 'stderr':
      case 'stdout': {
        const outMsg = msg.data;
        const ctRegex = /^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/m;
        const rtRegex = /^raised .+ : ([a-zA-Z._0-9-]+):(\d+) (.+)$/m;

        const ctMatchFound: Array<string> = outMsg.match(ctRegex);
        const rtMatchFound: Array<string> = outMsg.match(rtRegex);
        let div: JQuery;

        if (ctMatchFound || rtMatchFound) {
          let basename: string;
          let row: number;
          let col: number;

          if (ctMatchFound) {
            basename = ctMatchFound[1];
            row = parseInt(ctMatchFound[2]);
            col = parseInt(ctMatchFound[3]);

            if (ctMatchFound[4].indexOf(' info:') == 0) {
              div = homeArea.addInfo(outMsg);
            } else {
              div = homeArea.addMsg(outMsg);
              homeArea.errorCount++;
            }
          } else {
            basename = rtMatchFound[1];
            row = parseInt(rtMatchFound[2]);
            col = 1;

            div = homeArea.addMsg(outMsg);
            homeArea.errorCount++;
          }

          // Lines that contain a sloc are clickable:
          div.on('click', () => {
            this.editors.map((e) => {
              if (basename == e.getResource().basename) {
                // Switch to the tab that contains the editor
                e.getTab().trigger('click');

                // Jump to the corresponding line
                e.gotoLine(row, col);
              }
            });
          });
        } else {
          homeArea.addLine(outMsg);
        }
        break;
      }
      default: {
        homeArea.addLine(msg.data);
        throw new Error('Unhandled msg type.');
        break;
      }
    }
  }

  /**
   * Process the output from "check_output" ajax request
   * @param {Types.CheckOutput.FS} data - The data from check_output
   * @return {number} the number of lines read by this function
   */
  private processCheckOutput(data: Types.CheckOutput.FS): number {
    let readLines = 0;

    data.output.map((ol) => {
      const homeArea = this.getHomeArea(ol.ref);
      readLines++;

      this.handleMsgType(ol.msg, homeArea);
    });

    if (data.completed) {
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
   * Reset the editors, outputArea
   */
  protected resetEditors(): void {
    this.outputArea.reset();

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
    tabSetting.getCheckBox().prop('checked', true).on('change', () => {
      if (tabSetting.checked()) {
        this.tabs.show(true);
      } else {
        this.tabs.show(false);
      }
    });

    const themeSetting: CheckBox =
        new CheckBox(Strings.SETTINGS_THEME_EDITOR_LABEL, dropdownContent);
    themeSetting.getCheckBox().on('change', () => {
      let theme = EditorTheme.Light;
      if (themeSetting.checked()) {
        theme = EditorTheme.Dark;
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
        .on('click', (event: JQuery.ClickEvent) => {
          if (event.target.disabled) {
            return;
          }
          if (window.confirm(Strings.RESET_CONFIRM_MSG)) {
            this.resetEditors();
          }
        });
    if (this.dlType != Types.DownloadType.None) {
      $('<button>')
          .attr('type', 'button')
          .addClass('settingsbar-item')
          .addClass('download-btn')
          .attr('title', Strings.DOWNLOAD_TOOLTIP)
          .append(
              $('<i>').addClass('fas').addClass('fa-file-download')
          )
          .appendTo(settingsBar)
          .on('click', () => {
            this.downloadExample()
                .catch((error: Error) => {
                  this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
                  console.error('Error:', error);
                });
          });
    }

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

    this.buttonGroup = $('<div>')
        .addClass('col-md-3')
        .appendTo(row);

    this.buttons.map((b) => {
      b.render().appendTo(this.buttonGroup);
    });

    this.outputGroup = $('<div>')
        .addClass('col-md-9')
        .appendTo(row)
        .append(this.outputArea.render());
  }
}

/** The LabWidget class */
export class LabWidget extends Widget {
  private readonly labContainer: LabContainer = new LabContainer;
  private readonly cliArea: CLIArea = new CLIArea();

  /**
   * Constructs the LabWidget
   * @param {JQuery} container - the container for the widget
   * @param {string} server - the server address:port
   */
  constructor(container: JQuery, server: string) {
    super(container, server);

    this.addButton('run');
    this.addButton('submit');

    this.lab = true;
  }

  /**
   * Collect the resources loaded in the widget and return as list
   *  then add cli data to file list
   * @return {Array<Types.Resource>} return the widget resources
   */
  protected collectResources(): Array<Types.Resource> {
    const files = super.collectResources();

    if (this.cliArea.enabled()) {
      files.push({
        basename: Strings.CLI_FILE,
        contents: this.cliArea.getContent(),
      });
    }

    return files;
  }

  /**
   * The main callback for the widget buttons
   * @param {JQuery.ClickEvent} event - the event that triggered the CB
   * @param {string} mode - the mode of the button that triggered the event
   */
  protected async buttonCB(event: JQuery.ClickEvent,
      mode: string): Promise<void> {
    this.labContainer.reset();

    await super.buttonCB(event, mode);

    this.labContainer.sort();
  }

  /**
   * Returns the correct Area to place data in
   * @param {number} ref - if not null, the lab ref
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(ref: number): Area {
    if (ref != null) {
      return this.labContainer.getLabArea(ref);
    }
    return this.outputArea;
  }

  /**
   * Handle the msg data coming back from server
   * @param {Types.CheckOutput.RunMsg} msg - the returned msg
   * @param {Area} homeArea - the area to place the rendered msg
   */
  protected handleMsgType(msg: Types.CheckOutput.RunMsg, homeArea: Area): void {
    switch (msg.type) {
      case 'lab': {
        const result =
          this.labContainer.processResults(
              (msg.data as unknown) as Types.CheckOutput.LabOutput);
        this.outputArea.addLabStatus(result);
        break;
      }
      default: {
        super.handleMsgType(msg, homeArea);
      }
    }
  }

  /**
   * Reset the editors, outputArea, and labContainer
   */
  protected resetEditors(): void {
    super.resetEditors();
    this.labContainer.reset();
  }

  /**
   * Render the widget by putting it into this.container
   */
  public render(): void {
    super.render();
    this.cliArea.render(this.buttonGroup);
    this.labContainer.render().appendTo(this.outputGroup);
  }
}
