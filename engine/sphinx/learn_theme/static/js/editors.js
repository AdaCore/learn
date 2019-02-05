// List of known modes, and the corresponding button labels

MODES = {
   "prove": "Prove",
   "run": "Run",
   "prove_flow": "Prove (flow)",
   "prove_report_all": "Prove (report=all)",
}

// Log an error message in the output area
function output_error(output_area, message) {
    var div = $('<div class="output_error">')
    div.text(message)
    div.appendTo(output_area)
}

// Reset the buttons on the editors to the "enabled" state
// Reset the count of lines already read to 0
function reset(container, editors){
   editors.buttons.forEach(function(b){b.disabled = false;})
   container.already_read = 0;
}

// Process the result of a check
//  editors: the editors to decorate
//  output_area: the div where the messages should go
//  status: the exit status
//  message: any message coming back from the application
//   TODO: make use of message
function process_check_output(container, editors, output_area, output, status, completed, message) {
    // Process the lines

    var read_lines = 0

    output.forEach(function (l) {
        read_lines++

        // Look for lines that contain an error message
        var error_found = false;
        var match_found = l.match(/^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/)
        if (match_found) {
           if (match_found[4].indexOf(" info:") == 0) {
              var klass = "output_msg_info";
           }
           else {
              var klass = "output_msg"
              error_found = true;
           }
        }
        else {
           var klass = "output_line";
        }

        // Print the line in the output area
        var div = $('<div class="' + klass + '">')
        div.text(l)
        div.appendTo(output_area)

        if (match_found != null) {
            if (error_found) {
               output_area.error_count++
            }

            // Lines that contain a sloc are clickable:
            div.on('click', function (x) {
               // find the corresponding editor
               var basename = match_found[1]
               editors.forEach(function (e) {
                  if (e.basename == basename) {
                     // Switch to the tab that contains the editor

                     // TODO: this is in the case of bootstrap only
                     // $("#" + e.unique_id + "-tab").tab('show')

                     // Jump to the corresponding line
                     e.gotoLine(parseInt(match_found[2]),
                         // looks like column numbers are indexed from 0
                         parseInt(match_found[3] - 1),
                         true)
                     e.focus()
                   }
                })
            })
        }
    })

    // Congratulations!
    if (completed) {
        reset(container, editors);

        if (status != 0) {
            output_error(output_area, "exit status: " + status)
        } else if (output_area.error_count == 0) {
            var div = $('<div class="output_success">')
            div.text("Success!")
            div.appendTo(output_area)
        } else if (output_area.error_count == 1) {
            var div = $('<div class="output_info">')
            div.text("One error.")
            div.appendTo(output_area)
        } else {
            var div = $('<div class="output_info">')
            div.text(output_area.error_count + " errors.")
            div.appendTo(output_area)
        }
    }

    return read_lines
}

function get_output_from_identifier(container, editors, output_area, identifier) {
    data = {
        "identifier": identifier,
        "already_read": container.already_read
    }
    $.ajax({
            url: container.example_server + "/check_output/",
            data: JSON.stringify(data),
            type: "POST",
            dataType: "json",
            contentType: 'application/json; charset=UTF-8',
            timeout: 4000
        })
        .done(function (json) {
            read_lines = process_check_output(
                container,
                editors, output_area,
                json.output_lines, json.status, json.completed, json.message
            )
            container.already_read = container.already_read + read_lines
            if (!json.completed) {
                // We have not finished processing the output: call this again
                setTimeout(function () {
                    get_output_from_identifier(container, editors, output_area, identifier)
                }, 250)
            }
        })
        .fail(function (xhr, status, errorThrown) {
            output_error(output_area, "the machine running the examples is not responding, please try again later")
            console.log("Error: " + errorThrown);
            console.log("Status: " + status);
            console.dir(xhr);
        })
        .fail(function (json) {
            reset(container,editors);
            output_error(output_area, json.message)
        })
}


// Launch a run on the given example editor
function query_operation_result(container, editors, output_area, mode) {

    files = []

    // Grab the contents from actual editors
    editors.forEach(function (e) {
        files.push({
            'basename': e.basename,
            'contents': e.getValue()
        })
    })

    // Grab the contents from shadow files
    if (container.shadow_files){
      container.shadow_files.forEach(function (e){
        files.push({
            'basename': e.basename,
            'contents': e.contents
            });
      });
    }

    data = {
        "files": files,
        "mode": mode,
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
        .done(function (json) {
            if (json.identifier == "") {
                reset(container, editors);
                output_error(output_area, json.message);
            } else {
                get_output_from_identifier(container, editors, output_area, json.identifier)
            }
        })
        .fail(function (xhr, status, errorThrown) {
            reset(container, editors);
            output_error(output_area, "the machine running the examples is not available, please try again later")
            console.log("Error: " + errorThrown);
            console.log("Status: " + status);
            console.dir(xhr);
        })
}


function create_editor(resource, container, content, editors, counter) {
   var the_id = "tab_" + container.attr("the_id") + "-" + counter
   var div = $('<div role="tabpanel" class="tab-pane' +
       (counter == 1 ? ' active' : '') +
       '" id="' + the_id + '">');
   var editordiv = $('<div class="editor_container"'
                     + ' id="' + resource.basename + the_id + '_editor">');
   editordiv.appendTo(div);
   div.appendTo(content);

   // ACE editors...
   editor = ace.edit(resource.basename + the_id + '_editor');
   editor.session.setMode("ace/mode/ada");

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

   $(container).children(".resource").each(function () {
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
       }
      else {
         // No region: replace the whole editor
         editor.initial_contents = $(this).text();
         editor.setValue($(this).text());
         $(this).text('');
      }
   })

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

   // set the editor to use exactly the vertical space it needs
   editor.setOptions({
        minLines: editor.session.doc.getLength(),
        maxLines: editor.session.doc.getLength()
   })

   editor.resize()
   // place the cursor at 1,1
   editor.selection.moveTo(0, 0);

   // clear undo stack to avoid undoing everything we just did
   editor.getSession().getUndoManager().reset();

   editor.renderer.setScrollMargin(5, 5, 0, 0);

   return editor;
}

// Fills a <div> with an editable representation of an example.
//    container: the <div> in question

var unique_id = 0

function fill_editor_from_contents(container, example_server, resources) {

   // Then fill the contents of the tabs

   var content = $('<div class="tab-content">')
   content.appendTo(container);

   counter = 0;

   var editors = []

   resources.forEach(function (resource) {
       counter++;
       var editor = create_editor(resource, container, content, editors, counter)
       // Append the editor to the list of editors
       editors.push(editor)
   })

   var row = $('<div class="row output_row">')
   row.appendTo(container)

   // create the buttons

   var buttons_div = $('<div class="col-md-3">')
   buttons_div.appendTo(row)

   var output_div = $('<div class="col-md-9">')
   output_div.appendTo(row)

   var output_area = $('<div class="output_area">')
   output_area.appendTo(output_div)

   editors.buttons = []

   if (container.attr("prove_button") || container.attr("run_button")){
   }

   var reset_created = false;

   for (var mode in MODES) {
      if (container.attr(mode + "_button")){

         // Create the reset button, but do this only once and if there are other buttons
         if (!reset_created) {
            reset_created = true;
            var reset_button = $('<button type="button" class="btn btn-secondary">').text("Reset").appendTo(buttons_div)
            reset_button.editors = editors;
            editors.buttons.push(reset_button)
            reset_button.on('click', function (x) {
                if (reset_button.disabled) {return;}
                editors.buttons.forEach(function(b){b.disabled = false;})
                container.already_read = 0;
                output_area.empty();
                output_area.error_count = 0;

                reset_button.editors.forEach(function (x) {
                    x.setValue(x.initial_contents);
                    x.gotoLine(1);
                })
            })

         }

         // Now create the button for each mode that has been specified in the attributes
         var the_text = MODES[mode];

         var check_button = $('<button type="button" class="btn btn-primary">').text(the_text).appendTo(buttons_div);
         check_button.operation_label = the_text;
         check_button.mode = mode;
         editors.buttons.push(check_button);
         check_button.editors = editors;
         check_button.click(check_button, function (x) {
             if (x.data.disabled) {return;}
             editors.buttons.forEach(function(b){b.disabled = true;})
             output_area.empty();
             output_area.error_count = 0;

             var div = $('<div class="output_info">');
             div.text(x.data.operation_label + "...");
             div.appendTo(output_area);
             query_operation_result(container, x.data.editors, output_area, x.data.mode);
          })
      }
   }
}

function fill_editor(container, example_server) {
    unique_id++;
    container.attr("the_id", unique_id);
    container.already_read = 0;  // The number of lines already read
    container.example_server = example_server;

    // List the "file" divs, add these as resources
    var resources = []
    $(container).children(".file").each(function () {
       // Create a fake resource for each 'file' div
       a = Object();
       a.basename = $(this).attr("basename");
       a.contents = $(this).text();
       $(this).text('');
       resources.push(a);
    })

    // List the contents of the ".shadow_file" divs
    container.shadow_files = []
    $(container).children(".shadow_file").each(function () {
       // Create a fake resource for each 'file' div
       a = Object();
       a.basename = $(this).attr("basename");
       a.contents = $(this).text();
       $(this).text('');
       container.shadow_files.push(a);
    })

    fill_editor_from_contents(container, example_server, resources);
}


// Called when the document is ready
$(document).ready(function () {

    // Iterate on all divs, finding those that have the "example_editor"
    // attribute
    $("div").each(function (index, element) {
        example_server = $(this).attr("example_server");
        if (!example_server) {
            example_server = '';
        }
        fill_editor($(this), example_server);
    })
});
