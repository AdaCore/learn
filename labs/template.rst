A standard lab needs to be constituted of the following files

.. code-block:: sh

    {lab_name}.gpr         # The main project file for the lab
    input                  # Eventual input text for the lab

    lab.yaml               # Config file. For the moment contains eventual output
                           # file if output should be checked.

    common/*.ad(s|b)       # Base source files for the lab,
                           # shared between problem and solution. Read-only.
                           # Will typically contain the main.adb that is the
                           # driver for the lab.

    problem/*.ad(s|b)      # User's solution. Might have some code present from
                           # the start

    solution/*.ad(s|b)     # Source files for the solution. Read-only

    problem_statement.rst  # File containing the problem statement for the lab,
                           # + eventual hints, guidance, code snippets.

Lab.yaml content
----------------

Possible keys are

.. code-block:: yaml
    output-file: {output_file}
    tags: {tags for the test}

If the ``output-file`` key is present, then the lab infrastructure will match
the output of the program against output file.

If no output file is present, then the return code of the program will be
matched to see if it has succeeded or not.

``tags`` are simple text tags attached to the lab that will help categorize
them. Tags are alphanumeric works following the ``"\w[\w\d-]+"`` regex pattern.
For example:

.. code-block:: text
    embedded, access-types, privacy, ...
