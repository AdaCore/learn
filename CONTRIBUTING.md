Contributing to the learning material
=====================================

Writing Guidelines
------------------

- Text must contain up to 79 characters per line.

- Source-code examples should be complete, compilable, and free of errors.

    - Valid exceptions for incomplete code are, for example, when the
      writer wants to highlighting a specific source-code block from a
      previous example.

    - Erroneous source-code examples are acceptable when highlighting an
      important pitfall. They should, however, be avoided as much as
      possible.

      - In most cases, mentioning pitfalls in the text should be
        sufficient.

- For special cases, a `class` must be added to the source-code examples:

    - `ada-nocheck`: code must not be compiled.

    - `ada-syntax-only`: compiler should only check for syntax errors (no
      run).

    - `ada-expect-compile-error`: a compilation error is expected.

    - `ada-run-expect-failure`: a run-time error is expected.

- Language-specific roles must be used for inline references to
  source-code (e.g. keywords).

    - Available roles: `ada`, `c`, `cpp`.

- References to other programming languages should be kept at a minimum
  level.

    - They should be used only when highlighting important differences for
      common use-cases in widely-known programming languages.

    - They should be part of admonitions, so that they don't disrupt the
      normal text.

    - They must be optional: readers should be able to understand the
      lessons without having to refer to other languages.
