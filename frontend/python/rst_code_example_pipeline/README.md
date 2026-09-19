# rst_code_example_pipeline

## Introduction

The `rst_code_example_pipeline` package contains scripts to extract, build and
run the code blocks from the ReST files. These are the main entry points:

- `extract-code` extracts all code blocks and stores into the specified
  build directory;

- `check-code` checks each code block from the specified build directory;

- `check-block` checks a single (previously extracted) code block.

Install the package from the repository, in editable mode:
```sh
pip install -e frontend/python/rst_code_example_pipeline
```

The entry points drive an Ada toolchain directly and expect it on `PATH`:
`extract-code` splits an Ada code block with `gnatchop`, and the two checking
commands syntax-check and compile with `gcc`, build with `gprbuild`, clean up
with `gprclean`, and prove with `gnatprove`.


## Simple usage

To build and run the source-code examples from a course, just run
`extract-code` followed by `check-code`. For example, to test
the source-code examples from the
[Introduction to Ada course](../../../content/courses/intro-to-ada), run:

```sh
extract-code                                                  \
  --build-dir test_output                                     \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

check-code                                                    \
  --build-dir test_output
```

When `extract-code` runs, it creates a JSON file called `block_info.json`
for each code block (source-code example) that is extracted from the ReST files.
`check-code` looks for all `block_info.json` files in the build directory
and checks the source-code example described in each of those JSON files.


## Exit status

All three entry points report the outcome of a run through their exit status:

- `check-code` and `check-block` exit `1` if any checked code block failed a
  check, and `0` otherwise. A code block that could not be read or checked
  also counts as a failure.

- `extract-code` exits `1` when it cannot run at all — for example, a code
  block has no project name, or neither `--build-dir` nor
  `--extracted_projects` was given — and `0` otherwise.

An invalid command line is rejected before any work is done, with exit
status `2`.

Some malformed code blocks are reported only through an `ERROR` line in the
output, while `extract-code` itself still exits `0`. Check the output, not
only the exit code, to catch these.


## Verbose mode

All the scripts have a `--verbose` / `-v` switch. For example:

```sh
extract-code                                                  \
  --verbose                                                   \
  --build-dir test_output                                     \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

check-code                                                    \
  --verbose                                                   \
  --build-dir test_output
```


## JSON file: list of extracted projects

It's possible to store the list of extracted projects into a JSON file and
use that file for checking the projects. For example, to build the source-code
examples from the
[Introduction to Ada course](../../../content/courses/intro-to-ada), run:

```sh
extract-code                                                  \
  --extracted_projects test_output/extracted_projects.json    \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

check-code                                                    \
  --extracted_projects test_output/extracted_projects.json
```

To build the source-code examples extracted from a single ReST file:

```sh
extract-code                                                  \
  --extracted_projects test_output/extracted_projects.json    \
  ../content/courses/intro-to-ada/chapters/imperative_language.rst

check-code                                                    \
  --extracted_projects test_output/extracted_projects.json
```


### JSON file and build directory

If only `--extracted_projects` is specified without `--build-dir`, then the
build directory corresponds to the path to the JSON file. For example, if
`--extracted_projects test_output/extracted_projects.json` is specified, then
the build directory is `test_output`.

To store the JSON file completely separate from the build directory, use
both `--extracted_projects` and `--build-dir` switches and specify different
paths. For example:

```sh
extract-code                                                  \
  --build-dir test_output                                     \
  --extracted_projects extracted_projects.json                \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

check-code                                                    \
  --build-dir test_output                                     \
  --extracted_projects extracted_projects.json
```


## Checking single code blocks

When extracting the source-code examples from the ReST files, a JSON file
called `block_info.json` is created for each code block. It's possible to check
a single code block by using `check-block` and indicating this JSON file.
For example:

```sh
check-block \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```


### Forced check

To force checking of a specific code block, use the `--force` switch:

```sh
check-block                                                   \
  --force                                                     \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```


## Testing single code block

To test just a single code block from an ReST file, use the `--code-block-at`
switch:

```sh
extract-code                                                  \
  --code-block-at=28                                          \
  --extracted_projects test_output/extracted_projects.json    \
  ../content/courses/intro-to-ada/chapters/imperative_language.rst

check-code                                                    \
  --extracted_projects test_output/extracted_projects.json
```


## Style check: maximum number of columns

To ensure that a maximum limit of 80 columns per line is respected in the code
examples, use the `--max-columns` switch.

For example, using `check-code`:

```sh
check-code                                                    \
  --max-columns 80                                            \
  --extracted_projects test_output/extracted_projects.json
```

For example, using `check-block`:

```sh
check-block                                                   \
  --max-columns 80                                            \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```


## Development

### Installing with test dependencies

The package declares an optional `test` extras group that installs
[pytest](https://docs.pytest.org/) and
[pytest-cov](https://pytest-cov.readthedocs.io/).
Install the package in editable mode together with those extras:

```sh
pip install -e ".[test]"
```

### Running the unit tests

Coverage options and test paths are configured in `pyproject.toml`, so a plain
`pytest` invocation from the package root is enough:

```sh
pytest
```

Some modules require an Ada toolchain (GNAT) to be on `PATH`; run the full
suite in an environment where GNAT is available.

To pass coverage options explicitly:

```sh
pytest --cov=rst_code_example_pipeline --cov-report=term-missing tests/
```
