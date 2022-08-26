"""Provide a sphinx extension to generate widgets from a .. code: directive.

This plugin interprets the following arguments to the code:: directive:
    * no_button   - removes all buttons
    * <X>_button  - forces the existence of a button for mode X.
                    Modes are defined in the MODES variable in editors.js.
    * switches    - supplies switches to be passed to the widget operations
    * cli_input   - tells the widget to accept command line args on run
    * manual_chop - tells the widget to chop files manually rather than gnatchop
    * c           - tells the widget that the code will be c
    * ada         - tells the widget that the code will be ada

Options can be specified with the :class: operation. These options are used to
tell the automatic test infrastructure how to process the widget. The following
options are available to tell the tests to:
    * ada-nocheck              - Ignore syntax errors
    * ada-syntax-only          - Check syntax only, not run
    * ada-expect-compile-error - Expect a compile error
    * ada-run                  - Run the example
    * ada-run-expect_failure   - Expect the run to fail

The code inside code:: directives is extracted into a list of files.
The files are extracted the following way:

   - for valid Ada code, 'gnatchop' is run on the entirety of the
     snippet

   - for C code, the files should be named explicitely, with a marker of
     the form
             !<basename>.c|.h
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
# System libs
import os
from typing import List

# HTML Template Libs
from jinja2 import Environment, PackageLoader, select_autoescape

# Sphinx libs
from docutils import nodes
from docutils.parsers.rst import Directive, directives

# Widget lib
from widget.widget import Widget
from code_block_info import CodeBlockInfo

# specifies the server address to set on the widgets
WIDGETS_SERVER_URL = os.environ.get(
    "CODE_SERVER_URL", "wss://api-staging.learn.r53.adacore.com/"
)


class WidgetCodeDirective(Directive):
    """Our custom Widget directive

    Derives from the base Directive object. The class variables define the
    behavior of the directive. See the Sphinx Directive API docs for details.
    """
    has_content = True
    required_arguments = 0
    optional_arguments = 1
    final_argument_whitespace = True
    option_spec = {
        'class': directives.class_option,
        'name': directives.unchanged,
    }

    def create_static_node(self, widget: Widget,
                           code_block_info : CodeBlockInfo,
                           node_format : str):
        """Performs Latex parsing on nodes

        Used to create the PDF builds of the site.

        Args:
            widget (Widget): The current working widget

        Returns:
            List[nodes]: Returns a list of Latex nodes
        """
        static_nodes = []

        for f in widget.files:
            # Based on sphinx/directives/code.py

            container_node = nodes.container(
                '', literal_block=True,
                classes=['literal-block-wrapper'])

            literal = nodes.literal_block('',
                                          f.content,
                                          format=node_format)
            literal['language'] = self.arguments[0].split(' ')[0]
            literal['linenos'] = True
            literal['source'] = f.basename

            caption = nodes.caption('', f.basename)
            caption.source = literal.source
            caption.line = literal.line

            container_node += caption
            container_node += literal

            static_nodes.append(container_node)

        def get_info_preamble(info_type : str) -> str:
            known_info_type : Dict[str, str]

            if node_format == 'latex':
                known_info_type = {
                    'build'   : '\\textbf{Build output}',
                    'run'     : '\\textbf{Runtime output}',
                    'compile' : '\\textbf{Compilation output}',
                    'prove'   : '\\textbf{Prover output}'
                }
            if node_format == 'html':
                known_info_type = {
                    'build'   : r"<div class='literal-block-preamble'>Build output</div>",
                    'run'     : r"<div class='literal-block-preamble'>Runtime output</div>",
                    'compile' : r"<div class='literal-block-preamble'>Compilation output</div>",
                    'prove'   : r"<div class='literal-block-preamble'>Prover output</div>"
                }

            if info_type in known_info_type:
                return known_info_type[info_type]
            else:
                return "Let's " + info_type + " the example:"

        block_info : Dict[str, str] = code_block_info.get_info()

        for info_type in sorted(block_info):

            if block_info[info_type] == "":
                # Do not show empty boxes
                continue

            preamble_node = nodes.container(
                '', literal_block=False,
                classes=[])

            preamble_raw = nodes.raw('',
                                     get_info_preamble(info_type),
                                     format=node_format)

            preamble_node += preamble_raw

            container_node = nodes.container(
                '', literal_block=True,
                classes=['literal-block-wrapper'])

            literal = nodes.literal_block('',
                                          block_info[info_type],
                                          format=node_format)
            literal['language'] = 'none'
            literal['source'] = info_type

            caption = nodes.caption('', info_type)
            caption.source = literal.source
            caption.line = literal.line

            # container_node += caption
            container_node += literal

            static_nodes.append(preamble_node)
            static_nodes.append(container_node)

        return static_nodes


    def run(self):
        """The main entrypoint for the WidgetDirective

        Raises:
            self.error: If an exception is thrown during directive parsing or
            creation of HTML from the template

        Returns:
            List[nodes]: Returns a list of nodes (HTML and Latex)
        """
        widget = Widget()
        nodes_html = []
        nodes_latex = []
        nodes_epub = []

        jinja_env = Environment(
            loader=PackageLoader('widget'),
            autoescape=select_autoescape(['html', 'xml'])
        )

        try:
            # parse directive arguments
            if self.arguments:
                widget.parseArgs(self.arguments[0].split(' '))

            # parse directive options
            if self.options:
                widget.parseOpts(self.options)

            # chop contents into files
            widget.parseContent(self.content)

            # Attemping to detect HTML or Latex output by checking for 'html' in tags
            if ('builder_html' in self.state.state_machine.document.settings.env.app.tags.tags
                and self.state.state_machine.document.settings.env.app.tags.tags['builder_html']):

                # insert widget into the template
                template = jinja_env.get_template('widget.html')
                html = template.render(url=WIDGETS_SERVER_URL, w=widget)

                nodes_html = [nodes.raw('', html, format='html')]
            else:
                code_block_info = CodeBlockInfo(project_name=widget.name,
                                                filename=self.content.items[0][0],
                                                line_number=self.content.items[0][1] - 1)
                if ('builder_latex' in self.state.state_machine.document.settings.env.app.tags.tags
                    and self.state.state_machine.document.settings.env.app.tags.tags['builder_latex']):
                    nodes_latex = self.create_static_node(widget, code_block_info, 'latex')
                if ('builder_epub' in self.state.state_machine.document.settings.env.app.tags.tags
                    and self.state.state_machine.document.settings.env.app.tags.tags['builder_epub']):
                    nodes_epub = self.create_static_node(widget, code_block_info, 'html')

        except Exception as err:
            raise self.error(err)

        return nodes_html + nodes_latex + nodes_epub


def on_builder_inited(app):
    # Connect to the "code" directive
    app.add_directive('code', WidgetCodeDirective, override=True)


def setup(app):
    app.add_config_value('insert_widgets', True, 'html')

    app.connect('builder-inited', on_builder_inited)
    return {'version': '0.1'}
