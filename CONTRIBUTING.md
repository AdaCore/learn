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

## Interactive snippets

Interactive snippets get generated for code inside the `code::` directive.

The following parameters are available for this directive:

    * `ada`: indicates that the code is written in Ada or SPARK
    * `c`: indicates that the code is written in C or C++

    * `<X>_button`: forces the existence of a button for mode X.
                    Modes are defined in the MODES variable in editors.js.

    * `no_button`: removes all buttons

The role `:code-config:`can be used to globally activate or deactivate buttons,
like so:

```
   :code-config:`run_button=True;prove_button=False`
```

This is active until this is overridden by another `:code-config:` role.
Parameters to `code::` directives override the settings in `:code-config:`.

It's possible to accumulate the code in several snippets. To do this, use
the flag `accumulate_code` in the `:code-config` role:

```
   :code-config:`accumulate_code=True`

    (several code:: snippets)

   :code-config:`accumulate_code=False`
```

The code inside code:: directives is extracted into a list of files.
The files are extracted the following way:

   - for valid Ada code, `gnatchop` is run on the entirety of the
     snippet

   - for C code, the files should be named explicitely, with a marker of the
     form `!<basename>` placed at the beginning of each file in the snippet.
     This mechanism is also activated if the argument manual_chop is passed
     to the `code::` directive. For instance:
     ```
        .. code:: prove_button manual_chop

           !main.c
           int main(void);

           !t.ads
           package T is
           end T;
     ```

For special cases, a `class` can be added to the source code examples:

  - `ada-nocheck`: code must not be compiled.

  - `ada-syntax-only`: compiler should only check for syntax errors (no
    run).

  - `ada-expect-compile-error`: a compilation error is expected.

  - `ada-run-expect-failure`: a run-time error is expected.
