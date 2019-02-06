""" Provide a sphinx extension to generate widgets from code: ada.

A role :code-config: must be seen before parsing the first code: block,
for instance

   :code-config:`run_button=True;prove_button=False;accumulate_code=True`

See doc in the function codeconfig.

Code accumulation: cancel it with an empty :code-config: directive

This plugin interprets the folloging parameters to the code:: directive:

    * run_button  - forces the existence of a run button
    * no_button   - removes all buttons

these override the code-config setting.

"""
from __future__ import print_function

import codecs
import glob
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

codeconfig_found = False
# A safeguard against documents not defining code-config. Is it needed?


# These are configured via the "code-config" role, see doc in the
# function codeconfig below.
class Config(object):
    buttons = set()
    # The list of active buttons. Strings of the form 'xxx_button'.
    accumulate_code = False
    reset_accumulator = False


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

        extra_attribs = ""
        argument_list = []
        force_no_buttons = False

        if self.arguments:
            argument_list = self.arguments[0].split(' ')

        if 'no_button' in argument_list or (
            'class' in self.options and (
                'ada-nocheck' in self.options['class'] or
                'ada-syntax-only' in self.options['class'])):
            force_no_buttons = True

        # Make sure code-config exists in the document
        if not codeconfig_found:
            print (self.lineno, dir(self))
            raise self.error("you need to add a :code-config: role")

        try:
            if 'c' in argument_list:
                files = c_chop(self.content)
            else:
                files = real_gnatchop(self.content)
        except subprocess.CalledProcessError:
            raise self.error("could not gnatchop example")

        if config.accumulate_code:
            # We are accumulating code: store the new code in the
            # accumulated_files
            global accumulated_files
            for f in files:
                accumulated_files[f[0]] = f[1]

        try:
            shadow_files_divs = ""
            if config.accumulate_code:
                editor_files = set([f[0] for f in files])
                for k, v in accumulated_files.items():
                    if k not in editor_files:
                        shadow_files_divs += (
                           u'<div class="shadow_file"'
                           'style="display:none" basename="{}">'
                           '{}</div>').format(k, escape(v))

            divs = "\n".join(
                [u'<div class="file" basename="{}">{}</div>'.format(
                    f[0], escape(f[1])) for f in files]
                )
        except Exception:
            # If we have an exception here, it's probably a codec error
            print (files)
            raise

        if not force_no_buttons:
            for x in (config.buttons |
                      set(filter(lambda y: y.endswith('_button'),
                                 argument_list))):
                extra_attribs += ' {}="True"'.format(x)

        return [
            nodes.raw('',
                      template.format(server_url=WIDGETS_SERVER_URL,
                                      files_divs=divs,
                                      shadow_files_divs=shadow_files_divs,
                                      extra_attribs=extra_attribs),
                      format='html')
        ]


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
        else:
            if not hasattr(config, key):
                raise inliner.error(
                    "wrong key for code-config: {}".format(key))
            setattr(config, key, value.lower() == "true")

    if config.reset_accumulator:
        accumulated_files = {}
        config.reset_accumulator = False

    return [], []


def on_builder_inited(app):
    # Connect to the "code" directive
    app.add_directive('code', WidgetCodeDirective)


def setup(app):
    app.add_config_value('insert_widgets', True, 'html')
    app.add_role('code-config', codeconfig)

    app.connect('builder-inited', on_builder_inited)
    return {'version': '0.1'}
