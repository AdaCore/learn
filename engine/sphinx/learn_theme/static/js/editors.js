// List of known modes, and the corresponding button labels
MODES = {
    "prove": "Prove",
    "prove_flow": "Examine",
    "prove_report_all": "Prove (report=all)",
    "run": "Run",
    "submit": "Submit",
};

CLI_FILE = "cli.txt"

// Log an error message in the output area
function output_error(output_area, message) {
    var div = $('<div class="output_error">');
    div.text(message);
    div.appendTo(output_area);
}

// Reset the buttons on the editors to the "enabled" state
// Reset the count of lines already read to 0
function reset(container, editors) {
    editors.buttons.forEach(function(b) {
        b.disabled = false;
    });
    container.already_read = 0;
}

// Process the result of a check
//  editors: the editors to decorate
//  output_area: the div where the messages should go
//  status: the exit status
//  message: any message coming back from the application
//   TODO: make use of message
function process_check_output(container, editors, output_area, lab_area, output, status, completed, message) {
    // Process the lines

    var read_lines = 0;

    function find_ref_in_lab_ref_list(ref) {

        var child = lab_area.find( 'div.lab_test_case[data-labref=' + ref + ']');

        if(child.length > 0) {
            return child;
        }

        var acc_wrapper = $("<div class='acc_wrapper' data-labref=" + ref + ">");
        acc_wrapper.appendTo(lab_area);

        var acc_button = $("<button class='accordion' data-labref=" + ref + "><span>Test Case #" + ref + '</span></button>')
        acc_button.appendTo(acc_wrapper);

        var tab_id = generateUniqueId();

        acc_button.click(function() {
            $(this).toggleClass("active");
            $("#" + tab_id).toggle();
        });

        var div = $('<div id=' + tab_id + ' class="lab_test_case" data-labref=' + ref + '>');

        div.appendTo(acc_wrapper);

        return div;
    }

    output.forEach(function(l) {
        read_lines++;

        var msg_obj = JSON.parse(l);

        // look for classification of message
        for (var msg_type in msg_obj) {
            var home_div = output_area;

            if (msg_obj[msg_type].hasOwnProperty('lab_ref')) {
                lab_ref = msg_obj[msg_type]["lab_ref"];
                var found = false;
                var found_obj = null;

                home_div = find_ref_in_lab_ref_list(lab_ref);
            }

            var div = $('<div>');
            div.appendTo(home_div);

            switch(msg_type) {
                case "console":
                    var msg = msg_obj[msg_type]["msg"];
                    div.addClass("output_console");
                    div.text("$ " + msg);
                    break;
                case "internal_error":
                    msg_obj[msg_type]["msg"] += " Please report this issue on https://github.com/AdaCore/learn/issues";
                    //  this fall through is intentional
                case "stderr":
                    error_found = true;
                    //  this fall through is intentional
                case "stdout":
                    var msg = msg_obj[msg_type]["msg"];
                    // Look for lines that contain an error message
                    var match_found = msg.match(/^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/);
                    if (match_found) {
                        if (match_found[4].indexOf(" info:") == 0) {
                            div.addClass("output_msg_info");
                        } else {
                            div.addClass("output_msg");
                            output_area.error_count++;
                        }

                        // Lines that contain a sloc are clickable:
                        div.on('click', function(x) {
                            // find the corresponding editor
                            var basename = match_found[1];
                            editors.forEach(function(e) {
                                if (e.basename == basename) {
                                    // Switch to the tab that contains the editor

                                    // TODO: this is in the case of bootstrap only
                                    $("#" + e.unique_id + "_button").click();

                                    // Jump to the corresponding line
                                    e.gotoLine(parseInt(match_found[2]),
                                        // looks like column numbers are indexed from 0
                                        parseInt(match_found[3] - 1),
                                        true);
                                    e.focus();
                                }
                            });
                        });
                    }
                    else {
                        div.addClass("output_line");
                    }

                    div.text(msg);

                    break;
                case "lab_output":
                    var test_cases = msg_obj[msg_type]["test_cases"];

                    var lab_tab = $("<div class='lab_tab'>");

                    for (var test in test_cases) {

                        home_div = find_ref_in_lab_ref_list(test);
                        var case_div = $('<div class="lab_results">');
                        case_div.appendTo(home_div);

                        var test_class = ""

                        if (test_cases[test]["status"] == "Success") {
                            test_class = "lab_test_success";
                        }
                        else {
                            test_class = "lab_test_failed";
                        }

                        home_div.addClass(test_class);
                        var labref = home_div.data('labref');
                        lab_area.find("button[data-labref='" + labref + "']").addClass(test_class);

                        $('<div class="lab_test_msg lab_test_input"><span class="lab_test_msg_title">Input:</span>' + test_cases[test]["in"] + '</div>').appendTo(case_div);
                        $('<div class="lab_test_msg lab_test_output"><span class="lab_test_msg_title">Expected Output:</span>' + test_cases[test]["out"] + '</div>').appendTo(case_div);
                        $('<div class="lab_test_msg lab_test_actual"><span class="lab_test_msg_title">Actual Output:</span>' + test_cases[test]["actual"] + '</div>').appendTo(case_div);
                        $('<div class="lab_test_msg lab_test_status"><span class="lab_test_msg_title">Status:</span>' + test_cases[test]["status"] + '</div>').appendTo(case_div);
                    }

                    div.addClass("lab_status");
                    if (msg_obj[msg_type]["success"]) {
                        div.text("Lab completed successfully.");
                    }
                    else {
                        div.text("Lab failed.");
                    }
                    break;
                default:
                    // TODO: this branch should probably throw an error
                    div.addClass("output_line");
                    div.text(msg);
                    break;
            }
        }
    });

    // Congratulations!
    if (completed) {
        reset(container, editors);

        if (status != 0) {
            output_error(output_area, "exit status: " + status);
        }
    }

    return read_lines;
}

function get_output_from_identifier(container, editors, output_area, lab_area, identifier) {
    data = {
        "identifier": identifier,
        "already_read": container.already_read
    };
    $.ajax({
            url: container.example_server + "/check_output/",
            data: JSON.stringify(data),
            type: "POST",
            dataType: "json",
            contentType: 'application/json; charset=UTF-8',
            timeout: 4000
        })
        .done(function(json) {
            read_lines = process_check_output(
                container,
                editors, output_area, lab_area,
                json.output_lines, json.status, json.completed, json.message
            );
            container.already_read = container.already_read + read_lines;
            if (!json.completed) {
                // We have not finished processing the output: call this again
                setTimeout(function() {
                    get_output_from_identifier(container, editors, output_area, lab_area, identifier);
                }, 250);
            } else {

                if (container.parent().hasClass("test-descriptor")) {
                    // we are in test mode. call test function callback
                    test_callback(container);
                }

                // if there is a lab area, sort accordions
                var lab_accs = lab_area.find("div.acc_wrapper");
                var sorted_accs = lab_accs.sort(function(a, b) {
                    return $(a).data('labref') > $(b).data('labref');
                });
                sorted_accs.appendTo(lab_area);

            }
        })
        .fail(function(xhr, status, errorThrown) {
            output_error(output_area, "the machine running the examples is not responding, please try again later");
            console.log("Error: " + errorThrown);
            console.log("Status: " + status);
            console.dir(xhr);
        })
        .fail(function(json) {
            reset(container, editors);
            output_error(output_area, json.message);
        });
}


// Launch a run on the given example editor
function query_operation_result(container, editors, output_area, lab_area, mode) {

    files = [];

    // Grab the contents from actual editors
    editors.forEach(function(e) {
        files.push({
            'basename': e.basename,
            'contents': e.getValue()
        });
    });

    // Grab the contents from shadow files
    if (container.shadow_files) {
        container.shadow_files.forEach(function(e) {
            files.push({
                'basename': e.basename,
                'contents': e.contents
            });
        });
    }

    var input_search = container.find( 'textarea[name="custom_input"]' );
    var check_search = container.find( 'input.custom_check' );

    if(check_search.length == 1 && input_search.length == 1) {
        if(check_search.is(':checked')) {
            files.push({
                'basename': CLI_FILE,
                'contents': input_search.val(),
            });
        }
    }

    data = {
        "files": files,
        "mode": mode,
    };

    var lab_name = container.attr("lab_name");
    if(lab_name) {
        data["lab"] = lab_name;
    }

    // reset the number of lines already read
    container.already_read = 0;

    // request the examples
    $.ajax({
            url: container.example_server + "/run_program/",
            data: JSON.stringify(data),
            type: "POST",
            dataType: "json",
            contentType: 'application/json; charset=UTF-8',
            timeout: 4000,
        })
        .done(function(json) {
            if (json.identifier == "") {
                reset(container, editors);
                output_error(output_area, json.message);
            } else {
                get_output_from_identifier(container, editors, output_area, lab_area, json.identifier);
            }
        })
        .fail(function(xhr, status, errorThrown) {
            reset(container, editors);
            output_error(output_area, "The machine running the examples may not be available or is busy, please try again now or come back later.");
            console.log("Error: " + errorThrown);
            console.log("Status: " + status);
            console.dir(xhr);
        });
}

function generateUniqueId() {
    var dt = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
    });
    return uuid;
}


function create_editor(resource, container, tabs, editors, counter) {
    var tab_id = "tab_" + container.attr("the_id")
    var the_id = tab_id + "-" + counter;

    var tab_button = $('<button id="' + the_id + '_button" class="tab-links ' + tab_id + (counter == 1 ? ' active' : '') +'">' + resource.basename + '</button>');
    tab_button.click(function() {
        // Get all elements with class="tab-content" in current editor and hide them
        $("div.tab-content." + tab_id).hide();

        // Get all elements with class="tab-links" and remove the class "active"
        $("button.tab-links." + tab_id).removeClass("active");

        // Show the current tab, and add an "active" class to the button that opened the tab
        $("#" + the_id).addClass("active");
        $("#" + the_id).show();

        $(this).addClass("active");
    });
    tab_button.appendTo(tabs);

    var div = $('<div class="tab-content ' + tab_id +
        (counter == 1 ? ' active' : '') +
        '" id="' + the_id + '">');

    var editordiv = $('<div class="editor_container"' +
        ' id="' + resource.basename + the_id + '_editor">');

    // Display the file name for files that are not Ada or main.c
    if (!resource.basename.match(/.ad[sb]$|^main.c$/)) {
        var labeldiv = $('<div class="editor_label">' + resource.basename + '</div>');
        labeldiv.appendTo(div);
    }
    editordiv.appendTo(div);
    div.appendTo(container);

    // ACE editors...
    editor = ace.edit(resource.basename + the_id + '_editor');

    // Set the mode
    if (resource.basename.match(/.ad[sb]$/)) {
        editor.session.setMode("ace/mode/ada");
    } else {
        editor.session.setMode("ace/mode/c_cpp");
    }

    // ... and their contents
    editor.setValue(resource.contents);
    editor.setShowPrintMargin(false);
    editor.gotoLine(1);
    editor.initial_contents = resource.contents;
    editor.basename = resource.basename;
    editor.unique_id = the_id;

    editor.setOptions({
        highlightActiveLine: false,
        fontSize: 13,
        tabSize: 3,
        useSoftTabs: true,
        theme: "ace/theme/tomorrow"
    });

    $(container).children(".resource").each(function() {
        if ($(this).attr("region")) {
            region = $(this).attr("region");

            // search editor content for region "region"
            beginregion = editor.find("--  #region " + region);
            endregion = editor.find("--  #endregion " + region);

            newRange = beginregion.clone();
            newRange.end.row = endregion.end.row;
            newRange.end.column = endregion.end.column;

            textReplace = $(this).text().replace(/^\s|\s+$/g, '');

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
    editor.replaceAll("", {
        needle: "--  #region (.*)\n",
        regExp: true
    });
    editor.replaceAll("", {
        needle: "--  #endregion (.*)\n",
        regExp: true
    });

    // check if container is readonly
    if (container.attr("readonly")) {
        // remove all read only tags in the editor
        editor.replaceAll("", {
            needle: "--  (begin|end) readonly",
            regExp: true
        });

        editor.setOption("readOnly", true);
    }

    // set the editor to use exactly the minimum vertical space it needs
    //  but set maxLines large enough so that it auto sizes when lines are added
    editor.setOptions({
        minLines: editor.session.doc.getLength(),
    //    maxLines: editor.session.doc.getLength()
        maxLines: 50
    });

    editor.resize();
    // place the cursor at 1,1
    editor.selection.moveTo(0, 0);

    // clear undo stack to avoid undoing everything we just did
    editor.getSession().getUndoManager().reset();

    editor.renderer.setScrollMargin(5, 5, 0, 0);

    return editor;
}

// Fills a <div> with an editable representation of an example.
//    container: the <div> in question

var unique_id = 0;

function reset_worker(button, editors, container, output_area, lab_area) {
    if (button.disabled) {
        return;
    }
    editors.buttons.forEach(function(b) {
        b.disabled = false;
    });
    container.already_read = 0;
    output_area.empty();
    output_area.error_count = 0;

    if (lab_area != null)
        lab_area.empty();

    button.editors.forEach(function(x) {
        x.setValue(x.initial_contents);
        x.gotoLine(1);
    });
}

function check_worker(button, editors, container, output_area, lab_area) {
    if (button.disabled) {
        return;
    }
    editors.buttons.forEach(function(b) {
        b.disabled = true;
    });
    output_area.empty();
    output_area.error_count = 0;

    if (lab_area != null)
        lab_area.empty();

    var div = $('<div class="output_info">');
    div.html("Console Output:");
    div.appendTo(output_area);
    query_operation_result(container, editors, output_area, lab_area, button.mode);
}

function test_callback(container) {
    var parent = container.parent();
    var test_name = parent.find("div.test_name").text();
    var test_input = parent.find("div.test_input").text();
    var test_expects = parent.find("div.test_expects").find("div.output_area");

    var response = container.find("div.output_area");
    var results_area = $("div.test-results");

    var results_div = $("<div>");

    results_div.append("Test: " + test_name + "<br>");
    if (response.text() == test_expects.text()) {
        results_div.append("<span class='passed_test'>Test passed!</span>");
    } else {
        results_div.append("<span class='failed_test'>Test failed!</span>");
        results_div.append("<p>Response: " + response.html() + "</p>");
        results_div.append("<p>Expects: " + test_expects.html() + "</p>");
    }
    results_div.append("<br><br></p>");
    results_area.append(results_div);
}

function fill_editor_from_contents(container, example_server, resources) {

    // if container parent is test-descriptor then we are in test mode
    var test_mode = container.parent().hasClass("test-descriptor");

    // Then fill the contents of the tabs
    var tabs = $('<div class="tab">');

    tabs.appendTo(container);

    counter = 0;

    var editors = [];

    resources.forEach(function(resource) {
        counter++;
        var editor = create_editor(resource, container, tabs, editors, counter);
        // Append the editor to the list of editors
        editors.push(editor);
    });

    // "click" all active tabs to show them
    $("button.tab-links.active").click();

    var row = $('<div class="row output_row">');
    row.appendTo(container);

    // create the buttons

    var buttons_div = $('<div class="col-md-3">');
    buttons_div.appendTo(row);

    var output_div = $('<div class="col-md-9">');
    output_div.appendTo(row);

    var output_area = $('<div class="output_area">');
    output_area.appendTo(output_div);

    editors.buttons = [];

    if (container.attr("prove_button") || container.attr("run_button")) {}

    var reset_created = false;

    var results_div = $("div.test-results");

    results_div.text("");

    var lab_area = $('<div class="lab_area">');
    if(container.attr("lab")) {
        container.attr("prove_button", true);
        container.attr("run_button", true);
        container.attr("submit_button", true);
        container.attr("cli_input", true);

        lab_area.appendTo(output_div);
    }
    else {
        lab_area = null;
    }

    if(container.attr("cli_input")) {
        $( '<textarea class="custom_input" name="custom_input" rows="4" cols="6"></textarea>' ).appendTo(buttons_div);
        var unique_id = generateUniqueId();
        var div = $('<div class="custom_check_container">').appendTo(buttons_div);
        $('<input type="checkbox" id="' + unique_id + '" class="custom_check">').appendTo(div).change( function() {
            var input_search = container.find( 'textarea[name="custom_input"]' );
            if ($(this).is(':checked')) {
                if(input_search.length == 1) {
                    input_search.show();
                }
            }
            else {
                if(input_search.length == 1) {
                    input_search.hide();
                }
            }
        }).change();
        $('<label class="custom_check" for="' + unique_id + '">Test against custom input</label>').appendTo(div);
    }

    for (var mode in MODES) {
        if (container.attr(mode + "_button")) {

            // Create the reset button, but do this only once and if there are other buttons
            if (!reset_created) {
                reset_created = true;
                var reset_button = $('<button type="button" class="btn btn-secondary">').text("Reset").appendTo(buttons_div);
                reset_button.editors = editors;
                editors.buttons.push(reset_button);
                reset_button.on('click', function(x) {
                    reset_worker(reset_button, editors, container, output_area, lab_area);
                });

            }

            // Now create the button for each mode that has been specified in the attributes
            var the_text = MODES[mode];

            var check_button = $('<button type="button" class="btn btn-primary">').text(the_text).appendTo(buttons_div);
            check_button.operation_label = the_text;
            check_button.mode = mode;
            editors.buttons.push(check_button);
            check_button.editors = editors;
            check_button.click(check_button, function(x) {
                check_worker(x.data, editors, container, output_area, lab_area);
            });

            if (test_mode) {
                var parent = container.parent();
                var test_exercises = parent.find("div.test_exercises").text();

                if (the_text == test_exercises) {
                    check_worker(check_button, editors, container, output_area, lab_area);
                }
            }

        }
    }
}

function fill_editor(container, example_server) {
    unique_id++;
    container.attr("the_id", unique_id);
    container.already_read = 0; // The number of lines already read
    container.example_server = example_server;

    // List the "file" divs, add these as resources
    var resources = [];
    $(container).children(".file").each(function() {
        // Create a fake resource for each 'file' div
        a = {};
        a.basename = $(this).attr("basename");
        a.contents = $(this).text();
        $(this).text('');
        resources.push(a);
    });

    // List the contents of the ".shadow_file" divs
    container.shadow_files = [];
    $(container).children(".shadow_file").each(function() {
        // Create a fake resource for each 'file' div
        a = {};
        a.basename = $(this).attr("basename");
        a.contents = $(this).text();
        $(this).text('');
        container.shadow_files.push(a);
    });

    fill_editor_from_contents(container, example_server, resources);
}


// Called when the document is ready
$(document).ready(function() {

    // Iterate on all divs, finding those that have the "example_editor"
    // attribute
    $("div").each(function(index, element) {
        var example_server = $(this).attr("example_server");
        if (example_server) {
            fill_editor($(this), example_server);
        } else {
            example_server = '';
        }

    });
});
