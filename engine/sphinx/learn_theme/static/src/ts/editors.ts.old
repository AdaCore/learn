import $ from 'jquery';
import * as ace from 'brace';
import 'brace/mode/ada';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

// List of known modes, and the corresponding button labels
const MODES = {
  'prove': {
    'button_text': 'Prove',
    'tooltip': 'Run gnatprove to prove SPARK code',
  },
  'prove_flow': {
    'button_text': 'Examine',
    'tooltip': 'Run gnatprove to examine SPARK data and control flow',
  },
  'prove_report_all': {
    'button_text': 'Prove (report=all)',
    'tooltip': 'Run gnatprove to prove SPARK code and report all findings',
  },
  'run': {
    'button_text': 'Run',
    'tooltip': 'Run code in editor',
  },
  'submit': {
    'button_text': 'Submit',
    'tooltip': 'Submit code for lab',
  },
};

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

/**
 * Log an error message in the output area
 *
 * @param {jQuery} outputArea - The output area div where messages will be put
 * @param {string} message - The message to put in outputArea
 *
 */
function outputError(outputArea, message) {
  $('<div>')
      .addClass('output_error')
      .text(message)
      .appendTo(outputArea);
}

/**
 * Reset the buttons on the editors to the "enabled" state
 * Reset the count of lines already read to 0
 *
 * @param {jQuery} container - The container to reset
 * @param {Object} editors - The list of ace editors
 *
 */
function reset(container, editors) {
  editors.buttons.forEach(function(b) {
    b.disabled = false;
  });
  container.alreadyRead = 0;
}

/**
 * Process the result of a check
 *
 * @param {jQuery} container - The container with the editors
 * @param {Object} editors - The list of ace editors in the container
 * @param {jQuery} outputArea - The output area to put messages
 * @param {jQuery} labArea - The area in the output area where lab messages go
 * @param {string[]} output - The list of string outputs from the server
 * @param {Number} status - The exit status
 * @param {boolean} completed - Is the program finished running
 * @param {string} message - Any message coming back from the application
 * @return {Number} - The number of lines processed
 *
 * @todo Make use of message
 *
 */
function processCheckOutput(container, editors, outputArea, labArea, output,
    status, completed, message) {
  let readLines = 0;

  /**
   * Find and return the lab output div corresponding to the lab ref
   * Create a new div if one doesn't already exist
   *
   * @param {string} ref - The lab ref to search for
   * @return {jQuery} - The div to put the lab ref messages
   *
   */
  function findRefInLabRefList(ref) {
    const child = labArea.find( 'div.lab_test_case[data-labref=' + ref + ']');

    if (child.length > 0) {
      return child;
    }

    const tabId = generateUniqueId();


    const accWrapper = $('<div>')
        .addClass('acc_wrapper')
        .attr('data-labref', ref)
        .appendTo(labArea);

    $('<button>')
        .addClass('accordion')
        .attr('data-labref', ref)
        .append(
            $('<span>').text(TEST_CASE_LABEL + ' #' + ref)
        )
        .appendTo(accWrapper)
        .click(function() {
          $(this).toggleClass('active');
          $('#' + tabId).toggle();
        });

    return $('<div>')
        .attr('id', tabId)
        .addClass('lab_test_case')
        .attr('data-labref', ref)
        .appendTo(accWrapper);
  }

  output.forEach(function(l) {
    readLines++;

    const msgObj = JSON.parse(l);

    // look for classification of message
    let homeDiv = outputArea;
    for (const msgType in msgObj) {
      // Require Guarding for-in (guard-for-in)
      if ({}.hasOwnProperty.call(msgObj, msgType)) {
        if (Object.prototype.hasOwnProperty.call(msgObj[msgType], 'lab_ref')) {
          const labRef = msgObj[msgType]['lab_ref'];
          homeDiv = findRefInLabRefList(labRef);
        }

        const div = $('<div>').appendTo(homeDiv);

        switch (msgType) {
          case 'console':
            div.addClass('output_console');
            div.text('$ ' + msgObj[msgType]['msg']);
            break;
          case 'internal_error':
            msgObj[msgType]['msg'] += ' ' + INTERNAL_ERROR_MESSAGE;
          //  this fall through is intentional
          case 'stderr':
          case 'stdout': {
            const msg = msgObj[msgType]['msg'];
            // Look for lines that contain an error message
            const matchFound =
              msg.match(/^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/);

            if (matchFound) {
              if (matchFound[4].indexOf(' info:') == 0) {
                div.addClass('output_msg_info');
              } else {
                div.addClass('output_msg');
                outputArea.error_count++;
              }

              // Lines that contain a sloc are clickable:
              div.click(function(x) {
                // find the corresponding editor
                const basename = matchFound[1];
                editors.forEach(function(e) {
                  if (e.basename == basename) {
                    // Switch to the tab that contains the editor
                    $('#' + e.uniqueId + '_button').click();

                    // Jump to the corresponding line
                    e.gotoLine(parseInt(matchFound[2]),
                        // looks like column numbers are indexed from 0
                        parseInt((matchFound[3] - 1).toString()),
                        true);
                    e.focus();
                  }
                });
              });
            } else {
              div.addClass('output_line');
            }

            div.text(msg);

            break;
          }
          case 'lab_output': {
            const testCases = msgObj[msgType]['test_cases'];

            for (const test in testCases) {
              if ({}.hasOwnProperty.call(testCases, test)) {
                homeDiv = findRefInLabRefList(test);
                const caseDiv = $('<div>')
                    .addClass('lab_results')
                    .appendTo(homeDiv);

                let testClass = '';

                if (testCases[test]['status'] == 'Success') {
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
  });

  // Congratulations!
  if (completed) {
    reset(container, editors);

    if (status != 0) {
      outputError(outputArea, EXIT_STATUS_LABEL + ': ' + status);
    }

    outputArea.find( '.spinner' ).remove();
  }

  return readLines;
}

/**
 * Process the result of a check
 *
 * @param {jQuery} container - The container with the editors
 * @param {Object} editors - The list of ace editors in the container
 * @param {jQuery} outputArea - The output area to put messages
 * @param {jQuery} labArea - The area in the output area where lab messages go
 * @param {string} identifier - The location of the example directory
 *
 */
function getOutputFromIdentifier(container, editors, outputArea, labArea,
    identifier) {
  const data = {
    'identifier': identifier,
    'already_read': container.alreadyRead,
  };

  $.ajax({
    url: container.exampleServer + '/check_output/',
    data: JSON.stringify(data),
    type: 'POST',
    dataType: 'json',
    contentType: 'application/json; charset=UTF-8',
    timeout: 4000,
  }).done(function(json) {
    const readLines = processCheckOutput(
        container,
        editors, outputArea, labArea,
        json.output_lines, json.status, json.completed, json.message
    );
    container.alreadyRead = container.alreadyRead + readLines;
    if (!json.completed) {
      // We have not finished processing the output: call this again
      setTimeout(function() {
        getOutputFromIdentifier(container, editors, outputArea,
            labArea, identifier);
      }, 250);
    } else {
      if (container.parent().hasClass('test-descriptor')) {
        // we are in test mode. call test function callback
        testCallback(container);
      }

      // if there is a lab area, sort accordions
      if (labArea != null) {
        const labAccs = labArea.find('div.acc_wrapper');
        const sortedAccs = labAccs.sort(function(a, b) {
          return $(a).data('labref') > $(b).data('labref');
        });
        sortedAccs.appendTo(labArea);
      }
    }
  }).fail(function(xhr, status, errorThrown) {
    outputError(outputArea, MACHINE_NOT_RESPONDING_LABEL);
    console.log('Error: ' + errorThrown);
    console.log('Status: ' + status);
    console.dir(xhr);
  }).fail(function(json : any) {
    reset(container, editors);
    outputError(outputArea, json.message);
  });
}

/**
 * Launch a run on the given example editor
 *
 * @param {jQuery} container - The container with the editors
 * @param {Object} editors - The list of ace editors in the container
 * @param {jQuery} outputArea - The output area to put messages
 * @param {jQuery} labArea - The area in the output area where lab messages go
 * @param {string} mode - the button mode
 *
 */
function queryOperationResult(container, editors, outputArea, labArea, mode) {
  const files = [];

  // Grab the contents from actual editors
  editors.forEach(function(e) {
    files.push({
      'basename': e.basename,
      'contents': e.getValue(),
    });
  });

  // Grab the contents from shadow files
  if (container.shadow_files) {
    container.shadow_files.forEach(function(e) {
      files.push({
        'basename': e.basename,
        'contents': e.contents,
      });
    });
  }

  const inputSearch = container.find( 'textarea[name="custom_input"]' );
  const checkSearch = container.find( 'input.custom_check' );

  if (checkSearch.length == 1 && inputSearch.length == 1) {
    if (checkSearch.is(':checked')) {
      files.push({
        'basename': CLI_FILE,
        'contents': inputSearch.val(),
      });
    }
  }

  const data = {
    'files': files,
    'mode': mode,
  };

  const labName = container.attr('lab_name');
  if (labName) {
    data['lab'] = labName;
  }

  // reset the number of lines already read
  container.alreadyRead = 0;

  // request the examples
  $.ajax({
    url: container.exampleServer + '/run_program/',
    data: JSON.stringify(data),
    type: 'POST',
    dataType: 'json',
    contentType: 'application/json; charset=UTF-8',
    timeout: 4000,
  }).done(function(json) {
    if (json.identifier == '') {
      reset(container, editors);
      outputError(outputArea, json.message);
    } else {
      getOutputFromIdentifier(container, editors, outputArea, labArea,
          json.identifier);
    }
  }).fail(function(xhr, status, errorThrown) {
    reset(container, editors);
    outputError(outputArea, MACHINE_BUSY_LABEL);
    console.log('Error: ' + errorThrown);
    console.log('Status: ' + status);
    console.dir(xhr);
  });
}

/**
 * Generate a unique ID using a UUID generator
 *
 * @return {string} A unique ID based on a UUID
 *
 */
function generateUniqueId() {
  let dt = new Date().getTime();
  const uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
      .replace(/[xy]/g, function(c) {
        const r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
      });
  return uuid;
}

/**
 * Create an editor and add it to the list of tabs in the container
 *
 * @param {string} resource - The file to put in the editor
 * @param {jQuery} container - the container we are working in
 * @param {jQuery} tabs - The div where the tabs live
 * @param {Object} editors - The list of editors
 * @param {Number} counter - The count'nth editor in the widget
 * @return {Object} The created editor
 *
 */
function createEditor(resource, container, tabs, editors, counter) {
  const tabId = 'tab_' + container.attr('the_id');
  const theId = tabId + '-' + counter;

  $('<button>')
      .attr('id', theId + '_button')
      .addClass('tab-links')
      .addClass(tabId + (counter == 1 ? ' active' : ''))
      .text(resource.basename)
      .click(function() {
        // Get all elements with .tab-content in current editor and hide them
        container.find('.tab-content').hide();

        // Get all elements with class="tab-links" and remove the class "active"
        container.find('.tab-links').removeClass('active');
        container.find('.tab-content').removeClass('active');

        // Show the current tab, add "active" to button that opened the tab
        $('#' + theId).addClass('active');
        $('#' + theId).show();

        $(this).addClass('active');
      })
      .appendTo(tabs);

  const div = $('<div>')
      .addClass('tab-content')
      .addClass(tabId)
      .addClass(counter == 1 ? 'active' : '')
      .attr('id', theId)
      .appendTo(container);

  $('<div>')
      .addClass('editor_container')
      .attr('id', resource.basename + theId + '_editor')
      .appendTo(div);

  // ACE editors...
  const editor : any = ace.edit(resource.basename + theId + '_editor');

  // Set the mode
  if (resource.basename.match(/.ad[sb]$/)) {
    editor.session.setMode('ace/mode/ada');
  } else {
    editor.session.setMode('ace/mode/c_cpp');
  }

  // ... and their contents
  editor.setValue(resource.contents);
  editor.setShowPrintMargin(false);
  editor.gotoLine(1);
  editor.initial_contents = resource.contents;
  editor.basename = resource.basename;
  editor.uniqueId = theId;

  editor.setOptions({
    highlightActiveLine: false,
    fontSize: 13,
    tabSize: 3,
    useSoftTabs: true,
    theme: 'ace/theme/tomorrow',
  });

  $(container).children('.resource').each(function() {
    if ($(this).attr('region')) {
      const region = $(this).attr('region');

      // search editor content for region "region"
      const beginregion = editor.find('--  #region ' + region);
      const endregion = editor.find('--  #endregion ' + region);

      const newRange = beginregion.clone();
      newRange.end.row = endregion.end.row;
      newRange.end.column = endregion.end.column;

      const textReplace = $(this).text().replace(/^\s|\s+$/g, '');

      editor.getSession().getDocument().replace(newRange, textReplace);
      $(this).text('');
    } else {
      // No region: replace the whole editor
      editor.initial_contents = $(this).text();
      editor.setValue($(this).text());
      $(this).text('');
    }
  });

  // search for remaining region marks and remove
  editor.replaceAll('', {
    needle: '--  #region (.*)\n',
    regExp: true,
  });
  editor.replaceAll('', {
    needle: '--  #endregion (.*)\n',
    regExp: true,
  });

  // check if container is readonly
  if (container.attr('readonly')) {
    // remove all read only tags in the editor
    editor.replaceAll('', {
      needle: '--  (begin|end) readonly',
      regExp: true,
    });

    editor.setOption('readOnly', true);
  }

  // set the editor to use exactly the minimum vertical space it needs
  //  but set maxLines large enough so that it auto sizes when lines are added
  editor.setOptions({
    minLines: editor.session.doc.getLength(),
    // maxLines: editor.session.doc.getLength()
    maxLines: 50,
  });

  editor.resize();
  // place the cursor at 1,1
  editor.selection.moveTo(0, 0);

  // clear undo stack to avoid undoing everything we just did
  editor.getSession().getUndoManager().reset();

  editor.renderer.setScrollMargin(5, 5, 0, 0);

  return editor;
}

/**
 * Fills a <div> with an editable representation of an example.
 *
 * @param {jQuery} button - The button that was pressed
 * @param {Object} editors - The list of ace editors in the container
 * @param {jQuery} container - The container that holds the editors
 * @param {jQuery} outputArea - The output area to put messages
 * @param {jQuery} labArea -The area in the output area where lab messages go
 *
 */
function resetWorker(button, editors, container, outputArea, labArea) {
  if (button.disabled) {
    return;
  }
  editors.buttons.forEach(function(b) {
    b.disabled = false;
  });
  container.alreadyRead = 0;
  outputArea.empty();
  outputArea.error_count = 0;

  if (labArea != null) {
    labArea.empty();
  }

  button.editors.forEach(function(x) {
    x.setValue(x.initial_contents);
    x.gotoLine(1);
  });
}

/**
 * The callback for widget button presses
 *
 * @param {jQuery} button - The button that was pressed
 * @param {Object} editors - The list of ace editors in the container
 * @param {jQuery} container - The container that holds the editors
 * @param {jQuery} outputArea - The output area to put messages
 * @param {jQuery} labArea -The area in the output area where lab messages go
 *
 */
function checkWorker(button, editors, container, outputArea, labArea) {
  if (button.disabled) {
    return;
  }
  editors.buttons.forEach(function(b) {
    b.disabled = true;
  });
  outputArea.empty();
  outputArea.error_count = 0;

  if (labArea != null) {
    labArea.empty();
  }

  $('<div>')
      .addClass('output_info')
      .addClass('console_output')
      .text(CONSOLE_OUTPUT_LABEL + ':')
      .appendTo(outputArea);

  $('<div>')
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
      .appendTo(outputArea);
  queryOperationResult(container, editors, outputArea, labArea, button.mode);
}

/**
 * This is the callback used for the testing page
 *
 * @param {jQUery} container - The container being tested
 *
 */
function testCallback(container) {
  const parent = container.parent();
  const testName = parent.find('div.test_name').text();
  // const testInput = parent.find('div.test_input').text();
  const testExpects = parent.find('div.test_expects').find('div.output_area');

  const response = container.find('div.output_area');
  const resultsArea = $('div.test-results');

  const resultsDiv = $('<div>')
      .append('Test: ' + testName + '<br>');
  if (response.text() == testExpects.text()) {
    resultsDiv.append(
        $('<span>')
            .addClass('passed_test')
            .text('Test passed!')
    );
  } else {
    resultsDiv.append(
        $('<span>')
            .addClass('failed_test')
            .text('Test failed!')
    )
        .append('<p>Response: ' + response.html() + '</p>')
        .append('<p>Expects: ' + testExpects.html() + '</p>');
  }

  resultsDiv.append('<br><br></p>');
  resultsArea.append(resultsDiv);
}

/**
 * fill an editor with content from a file in the container
 *
 * @param {jQuery} container - The div to fill with editors
 * @param {string} exampleServer - the value of attr example_server of container
 * @param {Object[]} resources - The list of files
 *
 */
function fillEditorFromContents(container, exampleServer, resources) {
  // if container parent is test-descriptor then we are in test mode
  const testMode = container.parent().hasClass('test-descriptor');

  // Then fill the contents of the tabs
  const tabs = $('<div>')
      .addClass('tab')
      .appendTo(container);

  let counter = 0;

  const editors = [];

  resources.forEach(function(resource) {
    counter++;
    const editor = createEditor(resource, container, tabs, editors, counter);
    // Append the editor to the list of editors
    editors.push(editor);
  });

  const settingsBar = $('<div>')
      .addClass('settings-bar')
      .appendTo(container);

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
      .change(function() {
        if ($(this).is(':checked')) {
          container.find('.tab').show();
          container.find('.tab-content').hide();

          container.find('.tab-content.active').show();
        } else {
          container.find('.tab').hide();
          container.find('.tab-content').show();
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
      .change(function() {
        let theme = 'ace/theme/tomorrow';
        if ($(this).is(':checked')) {
          theme = 'ace/theme/tomorrow_night';
        }
        editors.forEach(function(ed) {
          ed.setTheme(theme);
        });
      });

  $('<label>')
      .attr('for', themeCheckboxId)
      .text(SETTINGS_THEME_EDITOR_LABEL)
      .appendTo(dropdownContent);

  editors.buttons = [];
  // create reset button
  const resetButton = $('<button>')
      .attr('type', 'button')
      .addClass('settingsbar-item')
      .addClass('reset-btn')
      .attr('title', RESET_TOOLTIP)
      .append(
          $('<i>').addClass('fas').addClass('fa-undo')
      )
      .appendTo(settingsBar)
      .click(function(x) {
        resetWorker(resetButton, editors, container, outputArea, labArea);
      });
  resetButton.editors = editors;
  editors.buttons.push(resetButton);

  // "click" all active tabs to show them
  $('button.tab-links.active').click();

  const row = $('<div>')
      .addClass('row')
      .addClass('output_row')
      .appendTo(container);

  // create the buttons

  const buttonsDiv = $('<div>')
      .addClass('col-md-3')
      .appendTo(row);

  const outputDiv = $('<div>')
      .addClass('col-md-9')
      .appendTo(row);

  const outputArea = $('<div>')
      .addClass('output_area')
      .appendTo(outputDiv);

  // if (container.attr("prove_button") || container.attr("run_button")) {}

  $('div.test-results').text('');

  let labArea = $('<div>').addClass('lab_area');
  if (container.attr('lab')) {
    container.attr('prove_button', true);
    container.attr('run_button', true);
    container.attr('submit_button', true);
    container.attr('cli_input', true);

    labArea.appendTo(outputDiv);
  } else {
    labArea = null;
  }

  if (container.attr('cli_input')) {
    $('<textarea>')
        .addClass('custom_input')
        .attr('name', 'custom_input')
        .attr('rows', '4')
        .attr('cols', '6')
        .appendTo(buttonsDiv);

    const uniqueId = generateUniqueId();
    const div = $('<div>')
        .addClass('custom_check_container')
        .appendTo(buttonsDiv);
    $('<input>')
        .attr('type', 'checkbox')
        .attr('id', uniqueId)
        .addClass('custom_check')
        .attr('title', CUSTOM_INPUT_TOOLTIP)
        .appendTo(div)
        .change( function() {
          const inputSearch = container.find('textarea[name="custom_input"]');
          if ($(this).is(':checked')) {
            if (inputSearch.length == 1) {
              inputSearch.show();
            }
          } else {
            if (inputSearch.length == 1) {
              inputSearch.hide();
            }
          }
        }).change();

    $('<label>')
        .addClass('custom_check')
        .attr('for', uniqueId)
        .text(CUSTOM_INPUT_LABEL)
        .appendTo(div);
  }

  for (const mode in MODES) {
    if (container.attr(mode + '_button')) {
      // Create button for each mode that has been specified in the attributes
      const theText = MODES[mode]['button_text'];
      const tooltipText = MODES[mode]['tooltip'];

      const checkButton = $('<button>')
          .attr('type', 'button')
          .addClass('btn')
          .addClass('btn-primary')
          .attr('title', tooltipText)
          .text(theText)
          .appendTo(buttonsDiv);

      checkButton.operation_label = theText;
      checkButton.mode = mode;
      editors.buttons.push(checkButton);
      checkButton.editors = editors;
      checkButton.click(checkButton, function(x) {
        checkWorker(x.data, editors, container, outputArea, labArea);
      });

      if (testMode) {
        const parent = container.parent();
        const testExercises = parent.find('div.test_exercises').text();

        if (theText == testExercises) {
          checkWorker(checkButton, editors, container, outputArea, labArea);
        }
      }
    }
  }
}

/**
 * create editors with files and shadow files and stick them in container
 *
 * @param {jQuery} container - The div to fill with editors
 * @param {string} exampleServer - the value of attr example_server of container
 *
 */
function fillEditor(container, exampleServer) {
  container.attr('the_id', generateUniqueId());
  container.alreadyRead = 0; // The number of lines already read
  container.exampleServer = exampleServer;

  // List the "file" divs, add these as resources
  const resources = [];
  $(container).children('.file').each(function() {
    // Create a fake resource for each 'file' div
    const a = {};
    a.basename = $(this).attr('basename');
    a.contents = $(this).text();
    $(this).text('');
    resources.push(a);
  });

  // List the contents of the ".shadow_file" divs
  container.shadow_files = [];
  $(container).children('.shadow_file').each(function() {
    // Create a fake resource for each 'file' div
    const a = {};
    a.basename = $(this).attr('basename');
    a.contents = $(this).text();
    $(this).text('');
    container.shadow_files.push(a);
  });

  fillEditorFromContents(container, exampleServer, resources);
}


// Called when the document is ready
$(document).ready(function() {
  // Iterate on all divs, finding those that have the "example_editor"
  // attribute
  $('div').each(function(index, element) {
    let exampleServer = $(this).attr('example_server');
    if (exampleServer) {
      fillEditor($(this), exampleServer);
    } else {
      exampleServer = '';
    }
  });
});
