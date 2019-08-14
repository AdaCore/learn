import $ from 'jquery';
import * as ace from 'brace';
import 'brace/mode/ada';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

import 'components';
import 'strings';
import 'types';

function generateUniqueId() : string {
  let dt = new Date().getTime();
  const uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
      .replace(/[xy]/g, function(c) {
        const r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
      });
  return uuid;
}

class Editor {
  private editor : ace.Editor;
  private initialContents : string;
  private basename : string;
  private tab : Tabs;

  constructor(resource : Resource) {
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
    this.editor.selection.moveTo(0, 0);

    // clear undo stack to avoid undoing everything we just did
    this.editor.getSession().getUndoManager().reset();

    this.editor.renderer.setScrollMargin(5, 5, 0, 0);
  }

  public setTheme(theme) {
    this.editor.setTheme(theme);
  }

  public reset() {
    this.editor.setValue(this.initialContents);
    this.editor.gotoLine(1);
  }

  public render() : JQuery {
    return $('<div>').addClass('editor-container').append(this.editor.container);
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

  constructor() { };
  abstract render();
  abstract addConsole(text : string);
  abstract addInfo(text : string);
  abstract addErr(text : string);
}

class OutputArea extends Area {
  private spinner : JQuery;

  public errorCount : number = 0;

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
        )
        .appendTo(this.container);
  }

  public reset() {
    this.container.empty();
  }

  public add(classes : string[], text : string) : JQuery {
    const div = $('<div>');
    for(const c in classes) {
      div.addClass(c);
    }
    div.text(text);
    div.appendTo(this.container);
    return div;
  }

  public addConsole(text : string) {
    this.add(['output_console'], '$ ' + text);
  }

  public addInfo(text : string) {

  }

  public addErr(text : string) {

  }

  public addError(message : string) {
    this.add(['output_error'], message);
  }

  public showSpinner(show : boolean) {
    if (show) {
      this.spinner.show();
    } else {
      this.spinner.hide();
    }
  }

  public getContainer() : JQuery {
    return this.container;
  }
}

class LabArea extends Area {
  private container : JQuery;

  constructor() {
    this.container = $('<div>').addClass('lab_area');
  }

  public reset() {
    this.container.empty();
  }

  public render() {

  }

  private refLab(ref : Number) : JQuery {

  }

  public processResults(data : Object) {
    const labOutput : any = JSON.parse(msg)['lab_output'];
    const success : string = labOutput['success'];
    const testCases : any = labOutput['test_cases'];

    for (const testIndex : string in testCases) {
      const test : CheckOutput.TestResult = testCases[index];
      homeDiv = this.refLab(parseInt(test));
      const caseDiv = $('<div>')
          .addClass('lab_results')
          .appendTo(homeDiv);

      let testClass = '';

      if (test.status == 'Success') {
        testClass = 'lab_test_success';
      } else {
        testClass = 'lab_test_failed';
      }

      homeDiv.addClass(testClass);

      const labref = homeDiv.data('labref');
      labArea.find('button[data-labref="' + labref + '"]')
          .addClass(testClass);

        $('<div>')
            .addClass('lab_test_msg')
            .addClass('lab_test_input')
            .append(
                $('<span>')
                    .addClass('lab_test_msg_title')
                    .text(LAB_TEST_INPUT_LABEL + ':')
            )
            .append(
                $('<code>').text(testCases[test]['in'])
            )
            .appendTo(caseDiv);

        $('<div>')
            .addClass('lab_test_msg')
            .addClass('lab_test_output')
            .append(
                $('<span>')
                    .addClass('lab_test_msg_title')
                    .text(LAB_TEST_OUTPUT_LABEL + ':')
            )
            .append(
                $('<code>').text(testCases[test]['out'])
            )
            .appendTo(caseDiv);

        $('<div>')
            .addClass('lab_test_msg')
            .addClass('lab_test_actual')
            .append(
                $('<span>')
                    .addClass('lab_test_msg_title')
                    .text(LAB_TEST_ACTUAL_LABEL + ':')
            )
            .append(
                $('<code>').text(testCases[test]['actual'])
            )
            .appendTo(caseDiv);

        $('<div>')
            .addClass('lab_test_msg')
            .addClass('lab_test_status')
            .append(
                $('<span>')
                    .addClass('lab_test_msg_title')
                    .text(LAB_TEST_STATUS_LABEL + ':')
            )
            .append(
                $('<code>').text(testCases[test]['status'])
            )
            .appendTo(caseDiv);
      }
    }

    div.addClass('lab_status');
    if (msgObj[msgType]['success']) {
      div.text(LAB_COMPLETE_LABEL);
    } else {
      div.text(LAB_FAILED_LABEL);
    }
  }
}

interface runProgramRequest {
  files : Resource[];
  mode : string;
  lab : string;
}

interface checkOutputRequest {
  identifier : string;
  already_read : number;
}

export class Widget {
  private editors : Editor[];
  private container : JQuery;
  private tabs : Tabs;
  private outputContainer : JQuery;
  private outputArea : OutputArea;
  private labArea : LabArea;
  private cliArea : JQuery;

  private buttons : JQuery[];

  private linesRead : number = 0;
  private server : string;

  private resources : Resource[];
  private shadowFiles : Resource[];

  constructor(container : JQuery, server : string) {
    container.attr('the_id', generateUniqueId());
    this.server = server;
    this.container = container;

    for (const file of this.container.children('.file')) {
      const a : Resource = {basename: file.attr('basename'),
                            contents: file.text()};
      file.text('');
      this.resources.push(a);
    }

    for (const file of this.container.children('.shadow_file')) {
      const a : Resource = {basename: file.attr('basename'),
                            contents: file.text()};
      file.text('');
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
      this.container.attr('prove_button', true);
      this.container.attr('run_button', true);
      this.container.attr('submit_button', true);
      this.container.attr('cli_input', true);

      this.labArea = new LabArea();
    } else {
      this.labArea = null;
    }

    if (container.attr('cli_input')) {
      this.cliArea = this.renderCLIInput();
    } else {
      this.cliArea = null;
    }

    for(const mode in modeDictionary) {
      if (this.container.attr(mode + '_button')) {
        const btn = Button([], modeDictionary[mode].tooltip,  modeDictionary[mode].buttonText);
        btn.click((event : JQuery.Event) => {
          this.buttonCB(event, mode);
        });
        this.buttons.push(btn);
      }
    }
  }

  private buttonCB(event : JQuery.Event, mode : string) {
    if (event.target.disabled)
      return;

    for(const b of this.buttons) {
      b.disabled = true;
    }

    this.outputArea.reset();
    if (this.labArea != null) {
      this.labArea.reset();
    }

    this.outputArea.add(['output_info', 'console_output'], CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.spinner(true);

    // TODO: grab cli input and put in shadow file CLI_FILE

    const labName = container.attr('lab_name');
    const serverData : runProgramRequest = {
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
    }).done((json) => {
      if (json.identifier == '') {
        resetServerReq();
        this.outputArea.addError(json.message);
      } else {
        processServerOutput(json.identifier);
      }
    }).fail((xhr, status, errorThrown) => {
      resetServerReq();
      this.outputArea.addError(MACHINE_BUSY_LABEL);
      console.log('Error: ' + errorThrown);
      console.log('Status: ' + status);
      console.dir(xhr);
    });
  }


  private processServerOutput(identifier : Object) {
    const data : checkOutputRequest = {
      identifier: identifier,
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
      const readLines = processCheckOutput(data);

      this.linesRead += readLines;

      if (!data.completed) {
        // We have not finished processing the output: call this again
        setTimeout(() => {
          this.processServerOutput(identifier);
        }, 250);
      }
    }).fail((xhr, status, errorThrown) => {
      this.outputArea.addError(MACHINE_NOT_RESPONDING_LABEL);
      console.log('Error: ' + errorThrown);
      console.log('Status: ' + status);
      console.dir(xhr);
    }).fail((json) => {
      this.resetServerReq();
      this.outputArea.addError(json.message);
    });
  }

  private processCheckOutput(data : CheckOutput.FS) {

    for(const i in data.output_lines) {
      const lab = (data.output_lines[i].lab_ref != null);


       switch(data.output_lines[i].msg.type) {
         case 'console': {
            if(lab) {
              this.labArea.
            } else {

            }
         }





      let homeDiv : JQuery = this.outputArea.getContainer();

      if(data.output_lines[i].lab_ref != null) {
        homeDiv = this.labArea.refLab(data.output_lines[i].lab_ref);
      }

      const div = $('<div>').appendTo(homeDiv);
      let msg = data.output_lines[i].msg.data;

      switch(data.output_lines[i].msg.type) {
        case 'console': {
          div.addClass('output_console');
          div.text('$ ' + msg);
          break;
        }
        case 'internal_error':
            msg += ' ' + INTERNAL_ERROR_MESSAGE;
          //  this fall through is intentional
        case 'stderr':
        case 'stdout': {
          // Look for lines that contain an error message
          const matchFound =
            msg.match(/^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/);

          if (matchFound) {
            if (matchFound[4].indexOf(' info:') == 0) {
              div.addClass('output_msg_info');
            } else {
              div.addClass('output_msg');
              this.outputArea.errorCount++;
            }

            // Lines that contain a sloc are clickable:
            div.click((event : JQuery.Event) => {
              // find the corresponding editor
              const basename : string = matchFound[1];
              for(const e of this.editors) {
                if(e.getBasename() == basename) {
                  // Switch to the tab that contains the editor
                  e.getTab().click();

                  // Jump to the corresponding line
                  e.gotoLine(parseInt(matchFound[2]),
                      // looks like column numbers are indexed from 0
                      parseInt(matchFound[3]));
                }
              }
            });
          } else {
            div.addClass('output_line');
          }

          div.text(msg);

          break;
        }
        case 'lab_output': {
          this.labArea.processResults(JSON.parse(msg));
          break;
        }
        default: {
          // TODO: this branch should probably throw an error
          const msg = msgObj[msgType]['msg'];
          div.addClass('output_line');
          div.text(msg);
          break;
        }
      }


    }
  }

  private resetServerReq() {
    for(const b in this.buttons) {
      b.disabled = false;
    }
    this.linesRead = 0;
  }

  private resetEditors() {
    this.linesRead = 0;
    this.outputArea.reset();
    this.labArea.reset();
    for (const editor of this.editors) {
      editor.reset();
    }
  }

  private renderCLIInput() : JQuery {
    // TODO
    return null;
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

    const tabsCheckboxId = generateUniqueId();
    $('<input>')
        .addClass('checkbox')
        .attr('id', tabsCheckboxId)
        .attr('type', 'checkbox')
        .prop('checked', true)
        .appendTo(dropdownContent)
        .change((event: JQuery.Event) => {
          if (event.target.is(':checked')) {
            this.tabs.show(true);
          } else {
            this.tabs.show(false);
          }
        });

    $('<label>')
        .attr('for', tabsCheckboxId)
        .text(SETTINGS_TABBED_EDITOR_LABEL)
        .appendTo(dropdownContent);

    const themeCheckboxId = generateUniqueId();
    $('<input>')
        .addClass('checkbox')
        .attr('id', themeCheckboxId)
        .attr('type', 'checkbox')
        .appendTo(dropdownContent)
        .change((event: JQuery.Event) => {
          let theme = 'ace/theme/tomorrow';
          if (event.target.is(':checked')) {
            theme = 'ace/theme/tomorrow_night';
          }
          for (let editor of this.editors) {
            editor.setTheme(theme);
          }
        });

    $('<label>')
        .attr('for', themeCheckboxId)
        .text(SETTINGS_THEME_EDITOR_LABEL)
        .appendTo(dropdownContent);

    $('<button>')
      .attr('type', 'button')
      .addClass('settingsbar-item')
      .addClass('reset-btn')
      .attr('title', RESET_TOOLTIP)
      .append(
          $('<i>').addClass('fas').addClass('fa-undo')
      )
      .appendTo(settingsBar)
      .click((event: JQuery.Event) => {
        if(event.target.disabled)
          return;
        this.resetEditors();
      });

    return settingsBar;
  }

  public render() {
    this.tabs.render().appendTo(this.container);
    const row = $('<div>')
        .addClass('row output_row')
        .appendTo(this.container);






  }
}
