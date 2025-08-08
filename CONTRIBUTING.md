# Contributing to the learning material

## Writing guidelines

- Source-code examples should be complete, compilable, and free of errors.

    - Valid exceptions for incomplete code are, for example, when the
      writer wants to highlighting a specific source-code block from a
      previous example.

    - Erroneous source-code examples are acceptable when highlighting an
      important pitfall. They should, however, be avoided as much as
      possible.

      - In most cases, mentioning pitfalls in the text should be
        sufficient.

- References to other programming languages should be kept at a minimum
  level.

    - They should be used only when highlighting important differences for
      common use-cases in widely-known programming languages.

    - They should be part of admonitions, so that they don't disrupt the
      normal text.

    - They must be optional: readers should be able to understand the
      lessons without having to refer to other languages.

- For any topic that isn't covered in the course itself, a reference to
  external content may be used.

    - We encourage the use of references to computer science pages on Wikipedia
      whenever possible.

    - For example:

      ```
          The :wikipedia:`V-model_(software_development)` is...
      ```

### Editorial policies and guidelines

- The content must be written in ReST format.

    - Sphinx is used to generate HTML, PDF and EPUB content based on the ReST
      format.

- Text must contain up to 79 characters per line.

- A blank line between a title and the first paragraph must be used, as this
  improves readability for the website maintainers.

- The global definitions file (global.txt) for Sphinx should be used — unless
  there is a very good reason for using alternative definitions.

    - The global definitions are included with the following format:

      ```
          .. include:: ../../global.txt
      ```

    - The global definitions file may be extended if needed.

- Anchors must be prefixed with a name that clearly identifies the course.

    - There are two reasons for this requirement:

        - Because of the risk of clashing with anchors from other courses.

        - If a course A is referencing a section from course B, it's easier to
          identify that the referenced section is located in course B.

    - Also, anchors must use capital letters separated by underscores.

    - For example, for a course called "Ada for desktop applications", we
      could select a prefix such as ``Ada_Desktop_App``. For an anchor to a
      section called "Implementing a 'Hello World' application", we could
      use the following anchor:

      ```
          .. _Ada_Desktop_App_Hello_World_App:

          Hello World Application
          -----------------------
      ```

- The following format for section headers must be used:

    ```
        Course part
        ***********

        This is the beginning of a new part of a course.


        Chapter Title
        =============

        This is the beginning of a new chapter.


        Section Title
        -------------

        This is the beginning of a new section.


        Subsection Title
        ~~~~~~~~~~~~~~~~

        This is the beginning of a subsection.


        Sub-Subsection Title
        ^^^^^^^^^^^^^^^^^^^^

        This is the beginning of a sub-subsection.
    ```

    - Other formats for section headers must be avoided.

    - Note that not all courses have to use parts: only chapters are required.
      However, using parts can be useful for better structuring a course that
      has many chapters — for example, more than 30 chapters.

    - Note that Sphinx doesn't require a fixed format for section headers. That
      being said, we use the format described above for better maintainance.

    - Sections that do not use a section header should be avoided.

        - For example:

          ```
              **Section Title**

              This is the beginning of a new section.
          ```

        - These sections won't be part of the table of contents, and reader
          won't have a way to access them directly.

- For any words or expressions referring to Ada keywords or elements from
  source-code examples, the "ada role" (``:ada:<keyword|expression>``) must be
  used.

  - This rule applies even for elements that aren't explicitly shown in
    source-code example.

  - For example:

    ```
         If a compilation unit :ada:`Q` has a :ada:`with` dependence on package
         :ada:`P`, then :ada:`Q` ...
    ```

- For content in a directive, 4-character indentations must be used. For
  example:

  ```
      .. admonition:: For further reading...

          You may also consult...

      .. code:: ada run_button project=Courses.Ada_Desktop_App.Introduction.Hello_World_App.Hello_World_Terminal

          with Ada.Text_IO; use Ada.Text_IO;

          procedure Hello_World is
             H : String := "Hello";
             W : String := "World";
          begin
             Put_Line (H & " " & W & "!");
          end Hello_World;
  ```

  - For Ada source-code examples, note that after the 4-character indentation
    for the directive, the indentation follows the GNAT code style — which
    makes use of a 3-character indentation in most cases.

- For any images that are included in the content, the images in the generated
  HTML page must be visualized in a browser using both light and dark modes, as
  images might not be visible in one of those modes.

  - When an image was created with a light background in mind, but it actually
    makes use of a transparent background, we can use the
    ``dark-mode-invert-image`` to adapt the colors for dark mode. For example:

    ```
        .. image:: images/light_mode_figure.png
            :class: dark-mode-invert-image
    ```

- ``|mdash|`` or ``|ndash|`` must be used instead of ``---`` or ``--``.

  - Actual m-dashes and n-dashes (``­—`` or ``–``) should be avoided.

  - ``|mdash|`` or ``|ndash|``  are defined in the isopub.txt file included in
    the global definitions file.

- For any bibliographical references, the sphinxcontrib.bibtex plug-in for
  Sphinx should be used.

- Todo items must be written in this format:

  ```
  .. todo::

      This is a todo item...
  ```

  - When content is processed for publishing, those todo items are not
    included in the generated files.

## Interactive snippets

Interactive code-blocks (snippets) are generated for any code inside the
`code::` directive. For example:

```
    .. code:: ada run_button project=Courses.Ada_Desktop_App.Introduction.Hello_World_App.Hello_World_Terminal

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Hello_World is
        begin
           Put_Line ("Hello World!");
        end Hello_World;
```

### Code directives

By default, Sphinx supports the `code-block::` directive. When this directive
is used, the widget is not used and no post-processing takes place. Only the
features supported by Sphinx — such as syntax highlighting — are applied to
the code block.

Note that non-interactive code block using the `code-block::` directive
should be avoided, as there's no testing taking place to ensure that the code
compiles or runs as expected. In fact, not even a simple syntax check takes
place, so the contributor has to ensure manually that the code is written and
working as intended.

On the other hand, when the `code::` directive is used, an interactive widget
is created for the HTML output and the code block is verified. All the features
described below are available for this directive.

### Language-related parameters

The following language-related parameters are available for this directive:

  -  `ada`: indicates that the code is written in Ada or SPARK

  -  `c`: indicates that the code is written in C or C++

### Button-related parameters

The following button-related parameters are available for this directive:

  - `no_button`: removes all buttons.

  - `compile_button`: compile code to check syntax.

  - `run_button`: run code in editor.

  - `prove_button`: prove SPARK code.

  - `prove_flow_button`: examine SPARK data and control flow.

  - `prove_report_all_button`: prove SPARK code and report all findings.

  - `prove_flow_report_all_button`: examine SPARK data and control flow and
    report all findings.

  - `submit_button`: submit code for a lab.

### Code extraction / manual chop

The code inside `code::` directives is extracted into a list of files.
The files are extracted the following way:

   - for valid Ada code, `gnatchop` is run on the entirety of the
     snippet

   - for C code, the files should be named explicitly, with a marker of the
     form `!<basename>` placed at the beginning of each file in the snippet.
     This mechanism is also activated if the argument `manual_chop` is passed
     to the `code::` directive. For instance:

     ```
        .. code:: c run_button manual_chop

            !main.c
            #include <stdio.h>

            int main(int argc, const char * argv[])
            {
              printf("Hello World\n");
              return 0;
            }
     ```

   - for Ada code, the argument `manual_chop` can be used as well. This
     prevents `gnatchop` from being called for the code block. For example:

     ```
        .. code:: ada manual_chop compile_button project=Courses.Ada_Desktop_App.Introduction.Package.Simple_Package

            !p.ads
            package P is

               function Is_OK (T : Float)
                               return Boolean;

            end P;
     ```

### Code block classes

For special cases, a `class` can be added to the source code examples:

  - `nosyntax-check`: code must not be checked for syntax errors (and not be
    compiled).

  - `ada-nocheck`: code must not be compiled.

  - `ada-syntax-only`: compiler should only check for syntax errors (no
    run).

  - `ada-expect-compile-error`: a compilation error is expected.

  - `ada-run-expect-failure`: a run-time error is expected.

When the `no_button` parameter is used, the following classes are available to
compile or run the code examples:

  - `ada-compile` and `c-compile`: to compile Ada or C code, respectively.

  - `ada-run` and `c-run`: to build and run Ada or C code, respectively.

  - `ada-norun` and `c-norun`: to explicitly deactivate the build and run of
    Ada or C code, respectively.

  - `ada-prove`, `ada-prove-flow`, `ada-prove-flow-report-all`,
    `ada-prove-report-all`: to prove the code in various forms using gnatprove.

These classes are useful to generate static output (such as compiler warnings
and runtime output) that is included with a code example in the generated
output.

