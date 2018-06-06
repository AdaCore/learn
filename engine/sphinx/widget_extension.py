""" Provide a sphinx extension to generate widgets from code: ada.

A role :code-config: must be seen before parsing the first code: block,
for instance

   :code-config:`run_button=True;prove_button=False;accumulate_code=True`

See doc in the function codeconfig.

Code accumulation: cancel it with an empty :code-config: directive
"""
import codecs
import glob
import os
import re
import shutil
import subprocess
import tempfile
from docutils import nodes
from docutils.parsers.rst import Directive, directives

WIDGETS_SERVER_URL = "https://cloudchecker-staging.r53.adacore.com"
# TODO: make this a configuration parameter

template = u"""
<div example_editor="{editor_base}"
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
    run_button = False
    prove_button = False
    accumulate_code = False
    reset_accumulator = False


config = Config()

accumulated_files = {}
# The accumulated files. Key: basename, value: lastest content seen


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
        try:
            subprocess.check_call(cmd, cwd=wd)
        except subprocess.CalledProcessError:
            print "could not gnatchop:\n" + '\n'.join(lines)
            raise
        os.remove(gnatchop_file)
        results = []
        for file in glob.glob(os.path.join(wd, '*')):
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

        # Make sure code-config exists in the document
        if not codeconfig_found:
            raise Exception("you need to add a :code-config: role")

        if 'class' in self.options and (
                'ada-nocheck' in self.options['class'] or
                'ada-syntax-only' in self.options['class']):
            # TODO: display it in an editor with no buttons
            return [nodes.raw('',
                              '<pre>{}</pre>'.format('\n'.join(self.content)),
                              format='html')]
        else:
            files = real_gnatchop(self.content)

        if config.accumulate_code:
            # We are accumulating code: store the new code in the
            # accumulated_files
            global accumulated_files
            for f in files:
                accumulated_files[f[0]] = f[1]

        if config.run_button:
            # We have a run button: try to find the main!
            # Current heuristics: find the .adb that doesn't have a .ads.
            names = [f[0] for f in files]
            bases = set([b[:-4] for b in names])
            main = [b + '.adb' for b in bases if b + '.ads' not in names]
            if main:
                extra_attribs += ' main="{}"'.format(main[0])

        try:
            shadow_files_divs = ""
            if config.accumulate_code:
                editor_files = set([f[0] for f in files])
                for k, v in accumulated_files.iteritems():
                    if k not in editor_files:
                        shadow_files_divs += (
                           u'<div class="shadow_file"'
                           'style="display:none" basename="{}">'
                           '{}</div>').format(k, v)

            divs = "\n".join(
                [u'<div class="file" basename="{}">{}</div>'.format(
                    f[0], f[1]) for f in files]
                )
        except Exception:
            # If we have an exception here, it's probably a codec error
            print files
            raise

        editor_base = "SPARK Main" if config.prove_button else "Ada Main"

        if config.run_button:
            extra_attribs += ' run_button="True"'
        if config.prove_button:
            extra_attribs += ' prove_button="True"'

        return [
            nodes.raw('',
                      template.format(server_url=WIDGETS_SERVER_URL,
                                      files_divs=divs,
                                      shadow_files_divs=shadow_files_divs,
                                      editor_base=editor_base,
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
        if not hasattr(config, key):
            raise Exception("wrong key for code-config: {}".format(key))
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
