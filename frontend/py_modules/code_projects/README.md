# Code_projects module

## Introduction

The [code_projects module](frontend/py_modules/code_projects) contains scripts
to extract, build and run the code blocks from the ReST files. These are the
main scripts:

- `extract_projects.py` extracts all code blocks and stores into the specified
  build directory;

- `check_projects.py` checks each code blocks from the specified build
  directory;

- `check_code_block.py` checks a single (previously extracted) code block.


## Simple usage

To build and run the source-code examples from a course, just run the
`extract_projects.py` script followed by `check_projects.py` script. For
example, to test the source-code examples from the
[Introduction to Ada course](content/courses/intro-to-ada), run:

```sh
python3 py_modules/code_projects/extract_projects.py     \
  --build-dir test_output                                \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

python3 py_modules/code_projects/check_projects.py      \
  --build-dir test_output
```

When the `extract_projects.py` runs, it creates a JSON file called
`block_info.json` for each code block (source-code example) that is extracted
from the ReST files. The `check_projects.py` looks for all `block_info.json`
files in the build directory and checks the source code example described in
each of those JSON files.


## Verbose mode

All the test script have a `--verbose` switch. For example:

```sh
python3 py_modules/code_projects/extract_projects.py     \
  --verbose                                              \
  --build-dir test_output                                \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

python3 py_modules/code_projects/check_projects.py       \
  --verbose                                              \
  --build-dir test_output
```


## JSON file: list of extracted projects

It's possible to store the list of extracted projects into a JSON file and
use that file for checking the projects. For example, to build the source-code
examples from the
[Introduction to Ada course](content/courses/intro-to-ada), run:

```sh
python3 py_modules/code_projects/extract_projects.py       \
  --extracted_projects test_output/extracted_projects.json \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

python3 py_modules/code_projects/check_projects.py         \
  --extracted_projects test_output/extracted_projects.json
```

To build the source-code examples extracted from a single ReST file:

```sh
python3 py_modules/code_projects/extract_projects.py       \
  --extracted_projects test_output/extracted_projects.json \
  ../content/courses/intro-to-ada/chapters/imperative_language.rst

python3 py_modules/code_projects/check_projects.py         \
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
python3 py_modules/code_projects/extract_projects.py       \
  --build-dir test_output                                  \
  --extracted_projects extracted_projects.json             \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

python3 py_modules/code_projects/check_projects.py         \
  --build-dir test_output                                  \
  --extracted_projects extracted_projects.json
```


## Checking single code blocks

When extracting the source-code examples from the ReST files, a JSON file
called `block_info.json` is created for each code block. It's possible to check a
single code block by using the `check_code_block.py` and indicating this
JSON file. For example:

```sh
python3 py_modules/code_projects/check_code_block.py \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```


### Forced check

To force checking of a specific code block, use the `--force` switch:

```sh
python3 py_modules/code_projects/check_code_block.py   \
  --force                                              \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```



## Testing single code block

To test just a single code block from an ReST file, use the `--code-block-at`
switch:


```sh
python3 py_modules/code_projects/extract_projects.py       \
  --code-block-at=28                                       \
  --extracted_projects test_output/extracted_projects.json \
  ../content/courses/intro-to-ada/chapters/imperative_language.rst

python3 py_modules/code_projects/check_projects.py         \
  --extracted_projects test_output/extracted_projects.json
```


## Style check: maximum number of columns

To ensure that a maximum limit of 80 columns per line is respected in the code
examples, use the `--max_columns` switch.

For example, using the `check_projects.py` script:

```sh
python3 py_modules/code_projects/check_projects.py   \
  --max-columns 80                                   \
  --extracted_projects test_output/extracted_projects.json
```

For example, using the `check_code_block.py` script:

```sh
python3 py_modules/code_projects/check_code_block.py \
  --max-columns 80                                   \
  test_output/projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/block_info.json
```
