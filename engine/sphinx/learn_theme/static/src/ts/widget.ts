import $ from 'jquery';
import * as ace from 'brace';
import 'brace/mode/ada';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

import {CheckBox, Button, Tabs} from './components';
import {Strings} from './strings';
import './types';

class Editor {
  private container : JQuery;
  private editor : ace.Editor;
  private initialContents : string;
  private basename : string;
  private tab : JQuery;

  constructor(resource : Resource) {
    this.container = $('<div>').addClass('editor-container');
    this.editor = ace.edit(null);

    // Set the mode
    if (resource.basename.match(/.ad[sb]$/)) {
      this.editor.session.setMode('ace/mode/ada');
    } else {
      this.editor.session.setMode('ace/mode/c_cpp');
    }

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
    this.container.append(this.editor.container);
  }

  public setTheme(theme) {
    this.editor.setTheme(theme);
  }

  public reset() {
    this.editor.setValue(this.initialContents);
    this.editor.gotoLine(1);
  }

  public render() : JQuery {
    return this.container;
  }

  public getBasename() : string {
    return this.basename;
  }

  public setTab(tab : JQuery) {
    this.tab = tab;
  }

  public getTab() : JQuery {
    return this.tab;
  }

  public gotoLine(line : number, col : number) {
    this.editor.gotoLine(line, col - 1, true);
    this.editor.focus()
  }
}

abstract class Area {
  protected container : JQuery;
  public errorCount : number = 0;

  constructor() { };
  abstract render() : JQuery;

  public add(classes : Array<string>, text : string) : JQuery {
    const div = $('<div>');
    for(const c in classes) {
      div.addClass(c);
    }
    div.text(text);
    div.appendTo(this.container);
    return div;
  }

  public reset() {
    this.container.empty();
    this.errorCount = 0;
  }

  public addConsole(text : string) : JQuery {
    return this.add(['output_console'], '$ ' + text);
  }

  public addInfo(text : string) : JQuery {
    return this.add(['output_msg_info'], text);
  }

  public addMsg(text : string) : JQuery {
    return this.add(['output_msg'], text);
  }

  public addLine(text : string) : JQuery {
    return this.add(['output_line'], text);
  }
}

class OutputArea extends Area {
  private spinner : JQuery;

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

  public render() : JQuery {
    return this.container;
  }

  public addError(message : string) {
    this.add(['output_error'], message);
  }

  public showSpinner(show : boolean) {
    if (show) {
      this.spinner.appendTo(this.container);
    } else {
      this.container.hide();
    }
  }

  public getErrCount() : number {
    return this.errorCount;
  }

  public getContainer() : JQuery {
    return this.container;
  }
}

class LabArea extends Area {
  private ref : number;
  private wrapper : JQuery;
  private button : JQuery;

  constructor(ref : number) {
    super();
    this.wrapper = $('<div>').addClass('acc_wrapper');
    this.ref = ref;

    this.button = $('<button>')
        .addClass('accordion')
        .appendTo(this.wrapper)
        .append(
          $('<span>').text(Strings.TEST_CASE_LABEL + ' #' + this.ref)
        ).click((event : JQuery.ClickEvent) => {
          event.target.toggleClass('active');
          this.container.toggle();
        });
    this.container = $('<div>')
        .addClass('lab_test_case')
        .appendTo(this.wrapper);
  }

  public render() : JQuery {
    return this.wrapper;
  }

  public addResults(success : string, result : CheckOutput.TestResult) {
    if(success == 'Success') {
      this.button.addClass('lab_test_success');
      this.container.addClass('lab_test_success');
    } else {
      this.button.addClass('lab_test_failed');
      this.container.addClass('lab_test_failed');
    }

    const caseDiv : JQuery = $('<div>')
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

  public getRef() : number {
    return this.ref;
  }
}

class LabContainer {
  private labList : Array<LabArea> = [];
  private container : JQuery;

  constructor() {
    this.container = $('<div>').addClass('lab_area');
  }

  public getLabArea(ref : number) : LabArea {
    for(const l of this.labList) {
      if(l.getRef() == ref) {
        return l;
      }
    }
    const newLab : LabArea = new LabArea(ref);
    return newLab;
  }

  public render() : JQuery {
    for(const l of this.labList) {
      l.render().appendTo(this.container);
    }
    return this.container;
  }

  public processResults(data : Object) {
    const labOutput : any = data['lab_output'];
    const success : string = labOutput['success'];
    const testCases : any = labOutput['test_cases'];

    for (const index in testCases) {
      const test : CheckOutput.TestResult = testCases[index];

      for(const l of this.labList) {
        if(l.getRef() == parseInt(index)) {
          l.addResults(success, test);
          return;
        }
      }
    }
  }

  public reset() {
    for(const l of this.labList) {
      l.reset();
    }
  }
}

class CLIArea {
  private textArea : JQuery;
  private checkBox : CheckBox;

  constructor() {
    this.textArea = $('<textarea>')
        .addClass('custom_input')
        .attr('name', 'custom_input')
        .attr('rows', '4')
        .attr('cols', '6');
    this.checkBox = new CheckBox(Strings.CUSTOM_INPUT_LABEL, undefined, ['custom_check_container'], Strings.CUSTOM_INPUT_TOOLTIP);
    this.checkBox.getCheckBox().change(() => {
      if(this.checkBox.checked()) {
        this.textArea.show();
      } else {
        this.textArea.hide();
      }
    });
  }

  public render(parent : JQuery) {
    this.textArea.appendTo(parent);
    this.checkBox.render().appendTo(parent);
  }
}

export class Widget {
  private editors : Array<Editor> = [];
  private container : JQuery;
  private tabs : Tabs = new Tabs();
  private outputContainer : JQuery;
  private outputArea : OutputArea = new OutputArea();
  private labContainer : LabContainer = new LabContainer();
  private cliArea : CLIArea = new CLIArea();

  private buttons : Array<Button> = [];

  private linesRead : number = 0;
  private server : string;

  private resources : Array<Resource> = [];
  private shadowFiles : Array<Resource> = [];

  constructor(container : JQuery, server : string) {
    this.server = server;
    this.container = container;

    for (const file of this.container.children('.file')) {
      const a : Resource = {basename: $(file).attr('basename'),
                            contents: $(file).text()};
      $(file).text('');
      this.resources.push(a);
    }

    for (const file of this.container.children('.shadow_file')) {
      const a : Resource = {basename: $(file).attr('basename'),
                            contents: $(file).text()};
      $(file).text('');
      this.shadowFiles.push(a);
    }

    // Then fill the contents of the tabs

    for (const file of this.resources) {
      const ed = new Editor(file);
      this.editors.push(ed);

      const tab = this.tabs.addTab(file.basename, ed.render());
      ed.setTab(tab);
    }

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

    for(const mode in Strings.modeDictionary) {
      if (this.container.attr(mode + '_button')) {
        const btn : Button = new Button([], Strings.modeDictionary[mode].tooltip,  Strings.modeDictionary[mode].buttonText);
        btn.render().click((event : JQuery.ClickEvent) => {
          this.buttonCB(event, mode);
        });
        this.buttons.push(btn);
      }
    }
  }

  private buttonCB(event : JQuery.ClickEvent, mode : string) {
    if (event.target.disabled)
      return;

    for(const b of this.buttons) {
      b.disabled = true;
    }

    this.outputArea.reset();
    if (this.labContainer != null) {
      this.labContainer.reset();
    }

    this.outputArea.add(['output_info', 'console_output'], Strings.CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.showSpinner(true);

    // TODO: grab cli input and put in shadow file CLI_FILE

    const labName = this.container.attr('lab_name');
    const serverData : RunProgram.TS = {
      files: this.resources.concat(this.shadowFiles),
      mode: mode,
      lab: labName
    }

    this.linesRead = 0;

    $.ajax({
      url: this.server + '/run_program/',
      data: JSON.stringify(serverData),
      type: 'POST',
      dataType: 'json',
      contentType: 'application/json; charset=UTF-8',
      timeout: 4000,
    }).done((json : RunProgram.FS) => {
      if (json.identifier == '') {
        this.resetServerReq();
        this.outputArea.addError(json.message);
      } else {
        this.getOutputFromIdentifier(json);
      }
    }).fail((xhr, status, errorThrown) => {
      this.resetServerReq();
      this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
      console.log('Error: ' + errorThrown);
      console.log('Status: ' + status);
      console.dir(xhr);
    });
  }

  private getOutputFromIdentifier(json : RunProgram.FS) {
    const data : CheckOutput.TS = {
      identifier: json.identifier,
      already_read: this.linesRead
    };

    $.ajax({
      url: this.server + '/check_output/',
      data: JSON.stringify(data),
      type: 'POST',
      dataType: 'json',
      contentType: 'application/json; charset=UTF-8',
      timeout: 4000,
    }).done((data : CheckOutput.FS) => {
      this.linesRead += this.processCheckOutput(data);

      if (!data.completed) {
        // We have not finished processing the output: call this again
        setTimeout(() => {
          this.getOutputFromIdentifier(json);
        }, 250);
      }
    }).fail((xhr, status, errorThrown) => {
      this.outputArea.addError(Strings.MACHINE_NOT_RESPONDING_LABEL);
      console.log('Error: ' + errorThrown);
      console.log('Status: ' + status);
      console.dir(xhr);
    }).fail((json : any) => {
      const message = (<CheckOutput.FS>json).message;
      this.resetServerReq();
      this.outputArea.addError(message);
    });
  }

  private processCheckOutput(data : CheckOutput.FS) : number {
    let readLines : number = 0;

    for(const str_ol in data.output_lines) {
      const ol : CheckOutput.OutputLine = data.output_lines[str_ol];
      let homeArea : Area;
      readLines++;

      if(ol.lab_ref != null) {
        homeArea = this.labContainer.getLabArea(ol.lab_ref);
      } else {
        homeArea = this.outputArea;
      }

      switch(ol.msg.type) {
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
          const regex : RegExp = /^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/;

          const matchFound : Array<string> = msg.match(regex);
          let div : JQuery;

          if(matchFound) {
            if (matchFound[4].indexOf(' info:') == 0) {
              div = homeArea.addInfo(msg);
            } else {
              div = homeArea.addMsg(msg);
              homeArea.errorCount++;
            }

            // Lines that contain a sloc are clickable:
            div.click((event : JQuery.Event) => {
              const basename = matchFound[1];
              for(const e of this.editors) {
                if(basename == e.getBasename()) {
                  // Switch to the tab that contains the editor
                  e.getTab().click();

                  // Jump to the corresponding line
                  e.gotoLine(parseInt(matchFound[2]),
                      // looks like column numbers are indexed from 0
                      parseInt(matchFound[3]) - 1);
                }
              }
            });
          } else {
            homeArea.addLine(msg);
          }
          break;
        }
        case 'lab_output': {
          this.labContainer.processResults(JSON.parse(ol.msg.data));
          break;
        }
        default: {
          // TODO: this branch should probably throw an error
          homeArea.addLine(ol.msg.data);
          break;
        }
      }
    }

    if(data.completed) {
      this.resetEditors();

      if(data.status != 0) {
        this.outputArea.addError(Strings.EXIT_STATUS_LABEL + ': ' + data.status);
      }

      this.outputArea.showSpinner(false);
    }

    return readLines;
  }

  private resetServerReq() {
    for(const b of this.buttons) {
      b.disabled = false;
    }
    this.linesRead = 0;
  }

  private resetEditors() {
    this.linesRead = 0;
    this.outputArea.reset();
    if(this.labContainer != null) {
      this.labContainer.reset();
    }
    for (const editor of this.editors) {
      editor.reset();
    }
  }

  private renderSettingsBar() : JQuery {
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

    const tabSetting : CheckBox = new CheckBox(Strings.SETTINGS_TABBED_EDITOR_LABEL, dropdownContent);
    tabSetting.getCheckBox().prop('checked', true).change(() => {
      if(tabSetting.checked()) {
        this.tabs.show(true);
      } else {
        this.tabs.show(false);
      }
    });

    const themeSetting : CheckBox = new CheckBox(Strings.SETTINGS_THEME_EDITOR_LABEL, dropdownContent);
    themeSetting.getCheckBox().change(() => {
      let theme = 'ace/theme/tomorrow';
      if (themeSetting.checked()) {
        theme = 'ace/theme/tomorrow_night';
      }
      for (let editor of this.editors) {
        editor.setTheme(theme);
      }
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
        if(event.target.disabled)
          return;
        this.resetEditors();
      });

    return settingsBar;
  }

  public render() {
    this.tabs.render(this.container);
    this.renderSettingsBar().appendTo(this.container);
    const row = $('<div>')
        .addClass('row output_row')
        .appendTo(this.container);

    const butCol = $('<div>')
        .addClass('col-md-3')
        .appendTo(row);

    if(this.labContainer != null) {
      this.cliArea.render(butCol);
    }

    for(const b of this.buttons) {
      b.render().appendTo(butCol);
    }

    const outCol = $('<div>')
        .addClass('col-md-9')
        .appendTo(row)
        .append(this.outputArea.render());

    if(this.labContainer != null) {
      this.labContainer.render().appendTo(outCol);
    }
  }
}
