""" Provide a sphinx extension to generate widgets from code: ada.

A role :code-config: must be seen before parsing the first code: block,
for instance

   :code-config:`run_button=True;prove_button=False;accumulate_code=True`

See doc in the function codeconfig.

Code accumulation: cancel it with an empty :code-config: directive

This plugin interprets the folloging parameters to the code:: directive:

    * no_button   - removes all buttons
    * <X>_button  - forces the existence of a button for mode X.
                    Modes are defined in the MODES variable in editors.js.

these override the code-config setting.

The code inside code:: directives is extracted into a list of files.
The files are extracted the following way:

   - for valid Ada code, 'gnatchop' is run on the entirety of the
     snippet

   - for C code, the files should be named explicitely, with a marker of
     the form
             !<basename>
     placed at the beginning of each file in the snippet. This mechanism
     is also activated if the argument manual_chop is passed to the
     code:: directive. For instance:

              .. code:: prove_button manual_chop

                 !main.c
                 int main(void);

                 !t.ads
                 package T is
                 end T;

"""
import codecs
import os
import re
import shutil
import subprocess
import tempfile
from docutils import nodes
from docutils.parsers.rst import Directive, directives
from xml.sax.saxutils import escape

WIDGETS_SERVER_URL = os.environ.get(
    "CODE_SERVER_URL",
    "https://cloudchecker-staging.r53.adacore.com")

template = u"""
<div class="widget_editor"
     example_server="{server_url}"
     {extra_attribs}
     inline="true">
   {files_divs}
   {shadow_files_divs}
</div>
"""

NAME_REGEX = re.compile('(lab|project)=(\S+)')
LAB_IO_START_REGEX = re.compile("--  START LAB IO BLOCK")
LAB_IO_END_REGEX = re.compile("--  END LAB IO BLOCK")

LABIO_FILENAME = "lab_io.txt"

codeconfig_found = False
# A safeguard against documents not defining code-config. Is it needed?


# These are configured via the "code-config" role, see doc in the
# function codeconfig below.
class Config(object):
    buttons = set()
    # The list of active buttons. Strings of the form 'xxx_button'.
    accumulate_code = False
    reset_accumulator = False
    switches = set()


config = Config()

accumulated_files = {}
# The accumulated files. Key: basename, value: lastest content seen


def c_chop(lines):
    """Chops the text, counting on filenames being given in the form
          !<filename>
    as the first line of each file.
    Returns a list of tuples of the form (basename, contents)
    """
    results = []
    current_filename = None
    current_contents = []
    for j in lines:
        if j.startswith('!'):
            if current_filename:
                results.append((current_filename, '\n'.join(current_contents)))
                current_contents = []
            current_filename = j[1:]
        else:
            current_contents.append(j)
    if current_filename:
        results.append((current_filename, '\n'.join(current_contents)))
    return results


def cheapo_gnatchop(lines):
    """Performs a cheapo gnatchop on the given text.

    lines is a list of strings
    Returns a list of tuples of the form (basename, contents)
    """
    results = []
    current_basename = 'invalid.ads'
    current_contents = []
    body = re.compile("^(procedure|package body) ([^ ]+)")
    spec = re.compile("^(package) ([^ ]+)")
    end = re.compile("^end")
    text_found = False

    def to_base_filename(g):
        return g.lower().replace('.', '-')

    for j in lines:
        # Append the lines to the current contents except if it's a blank
        # line before anything is found
        if not j and not text_found:
            continue
        text_found = True
        current_contents.append(j)
        match = body.match(j)
        if match:
            current_basename = to_base_filename(match.group(2)) + ".adb"
        else:
            match = spec.match(j)
            if match:
                current_basename = to_base_filename(match.group(2)) + ".ads"
            else:
                if end.match(j):
                    results.append((current_basename,
                                    '\n'.join(current_contents)))
                    current_contents = []
                    text_found = False
    if current_contents:
        results.append((current_basename,
                        '\n'.join(current_contents)))
    return results


def real_gnatchop(lines):
    """Same API as cheapo_gnatchop, but launch a real gnatchop"""
    wd = tempfile.mkdtemp()
    try:
        gnatchop_file = os.path.join(wd, 'internal_gnatchop.txt')
        with codecs.open(gnatchop_file, 'wb', encoding='utf-8') as f:
            f.write('\n'.join(lines))
        cmd = ['gnatchop', gnatchop_file]
        output = subprocess.check_output(cmd, cwd=wd)
        files = [os.path.join(wd, f.decode("utf-8").strip()) for f in output.splitlines()
                 if not f.startswith(b'splitting ')]
        os.remove(gnatchop_file)
        results = []
        for file in files:
            with codecs.open(file, 'rb', encoding='utf-8') as f:
                results.append((os.path.basename(file), f.read().strip()))
        return results
    finally:
        shutil.rmtree(wd)


class WidgetCodeDirective(Directive):
    has_content = True
    required_arguments = 0
    optional_arguments = 1
    final_argument_whitespace = True
    option_spec = {
        'class': directives.class_option,
        'name': directives.unchanged,
    }

    def run(self):
        shadow_files_divs = ""
        extra_attribs = ""
        argument_list = []
        force_no_buttons = False
        is_lab = False

        def get_shadow_div(basename, content):
            return (u'<div class="shadow_file"'
                     'style="display:none" basename="{}">'
                     '{}</div>').format(basename, escape(content))

        if self.arguments:
            argument_list = self.arguments[0].split(' ')

        if 'no_button' in argument_list or (
            'class' in self.options and (
                'ada-nocheck' in self.options['class'] or
                'ada-syntax-only' in self.options['class'])):
            force_no_buttons = True

        # look for (lab|project)=my_name
        name_matches = [NAME_REGEX.match(line) for line in argument_list if NAME_REGEX.match(line)]
        if len(name_matches) == 1:
            if name_matches[0].group(1) == "lab":
                extra_attribs += ' lab="True"'
                is_lab = True
            extra_attribs += ' name={}'.format(name_matches[0].group(2))
        elif len(name_matches) > 1:
            raise self.error("malformed widget directive")

        # Make sure code-config exists in the document
        if not codeconfig_found:
            print (self.lineno, dir(self))
            raise self.error("you need to add a :code-config: role")

        if is_lab:
            # look for lab io start block
            io_start_matches = [i for i, line in enumerate(self.content) if LAB_IO_START_REGEX.match(line)]

            # look for lab io end block
            io_end_matches = [i for i, line in enumerate(self.content) if LAB_IO_END_REGEX.match(line)]

            # check for correct formation of lab io block
            if len(io_start_matches) == 1 and len(io_end_matches) == 1 and io_start_matches[0] < io_end_matches[0]:
                io_content = self.content[io_start_matches[0] + 1 : io_end_matches[0]]

                # create shadow file from io blocks
                new_file = "\n".join(io_content)
                shadow_files_divs += get_shadow_div(LABIO_FILENAME, new_file)

                # remove io block lines from self.content
                # The following does not work for some odd reason so we will have to copy the list
                # del self.content[io_start_matches[0] : (io_end_matches[0] + 1)]
                chop_contents = self.content[:io_start_matches[0]] + self.content[io_end_matches[0] + 1:]
            else:
                raise self.error("malformed lab io block: io_start={} io_end={}".format(io_start_matches, io_end_matches))
        else:
            chop_contents = self.content

        # chop contents into files
        try:
            # chop source files
            if 'manual_chop' in argument_list:
                files = c_chop(chop_contents)
            elif 'c' in argument_list:
                files = c_chop(chop_contents)
            else:
                files = real_gnatchop(chop_contents)

            if len(files) == 0:
                raise self.error('could not parse files from content')
        except subprocess.CalledProcessError:
            raise self.error("could not gnatchop example")

        # if config.accumulate_code:
        #     # We are accumulating code: store the new code in the
        #     # accumulated_files
        #     global accumulated_files
        #     for f in files:
        #         accumulated_files[f[0]] = f[1]

        try:
            # if config.accumulate_code:
            #     editor_files = set([f[0] for f in files])
            #     for k, v in accumulated_files.items():
            #         if k not in editor_files:
            #             shadow_files_divs += get_shadow_div(k, v)

            divs = "\n".join(
                [u'<div class="file" basename="{}">{}</div>'.format(
                    f[0], escape(f[1])) for f in files]
                )

            nodes_latex = []

            # Attemping to detect HTML or Latex output by checking for 'html' in tags
            if 'html' not in self.state.state_machine.document.settings.env.app.tags.tags:
                for f in files:
                    # Based on sphinx/directives/code.py

                    container_node = nodes.container(
                        '', literal_block=True,
                        classes=['literal-block-wrapper'])

                    literal = nodes.literal_block('',
                                                  f[1],
                                                  format='latex')
                    literal['language'] = self.arguments[0].split(' ')[0]
                    literal['linenos'] = 'linenos' in self.options or \
                        'lineno-start' in self.options
                    literal['source'] = f[0]

                    caption = nodes.caption('', f[0])
                    caption.source = literal.source
                    caption.line = literal.line

#                    container_node += caption
                    container_node += literal

                    nodes_latex.append(container_node)

        except Exception:
            # If we have an exception here, it's probably a codec error
            print (files)
            raise

        if not force_no_buttons:
            for x in (config.buttons |
                      set(filter(lambda y: y.endswith('_button'),
                                 argument_list))):
                extra_attribs += ' {}="True"'.format(x)

        unf_local_switches = list(filter(lambda y: y.startswith('switches='), argument_list))
        local_switches = [x.split('=')[1] for x in unf_local_switches]
        switch_list = (config.switches | set(local_switches))

        if switch_list:
            switch_str = ','.join(switch_list)
            extra_attribs += f' switches="{switch_str}"'

        return [
            nodes.raw('',
                      template.format(server_url=WIDGETS_SERVER_URL,
                                      files_divs=divs,
                                      shadow_files_divs=shadow_files_divs,
                                      extra_attribs=extra_attribs),
                      format='html')
        ] + nodes_latex

def codeconfig(typ, rawtext, text, lineno, inliner, options={}, content=[]):
    """Support the code-config role.

    This role contains a set of directives separated by ";". See below
    for the list of directives.
    """
    global codeconfig_found, config, accumulated_files
    codeconfig_found = True

    # When we encounter this directive, empty the accumulated_files

    directives = text.split(';')
    for d in directives:
        key, value = d.strip().split('=')
        if key.endswith('_button'):
            if value.lower() == "true":
                config.buttons.add(key)
            else:
                if key in config.buttons:
                    config.buttons.remove(key)
        elif key.startswith('switches'):
            config.switches.add(value)
        else:
            if not hasattr(config, key):
                raise inliner.error(
                    "wrong key for code-config: {}".format(key))
            setattr(config, key, value.lower() == "true")

    # if config.reset_accumulator:
    #     accumulated_files = {}
    #     config.reset_accumulator = False

    return [], []


def on_builder_inited(app):
    # Connect to the "code" directive
    app.add_directive('code', WidgetCodeDirective, override=True)


def setup(app):
    app.add_config_value('insert_widgets', True, 'html')
    app.add_role('code-config', codeconfig)

    app.connect('builder-inited', on_builder_inited)
    return {'version': '0.1'}
