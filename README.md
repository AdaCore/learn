# learn.adacore.com

Sources for AdaCore's learn.adacore.com website

---

![Typescript Test Suite](https://github.com/AdaCore/learn/workflows/Typescript%20Test%20Suite/badge.svg)
![Sphinx Plugin Tests](https://github.com/AdaCore/learn/workflows/Sphinx%20Plugin%20Tests/badge.svg)
![Sphinx Content Tests](https://github.com/AdaCore/learn/workflows/Sphinx%20Content%20Tests/badge.svg)

## Requirements

This project requires Vagrant and VirtualBox

## Getting started

To setup for development run:

```
$ vagrant up
```

This will spin up two VMs:

- web: Is the build system for the frontend web content. This includes the
webpack build system and sphinx build.

- epub: Is the publishing server. This includes all packages needed to
generate the learn website.

To build and start the development server for the frontend, run:

```
$ vagrant ssh web

# The following commands will be run inside the vm

$ source /vagrant/venv/bin/activate
$ cd /vagrant/frontend
$ yarn run dev
```

This will run webpack on the typescript and scss, then sphinx for the rst
using `make local` which will point the widgets at 127.0.0.1:8000

You can then point your browser on your host to 127.0.0.1:8080 to see the learn
website being served from vagrant.

## Generate content for publishing

To build and start the publishing server, run:

```
$ vagrant ssh epub

# The following commands will be run inside the VM

$ cd /vagrant
$ source venv/bin/activate
$ make site
```

This will build the content for the learn website. You can find it in the
`/vagrant/frontend/dist` directory.

Once you have generated the content for the website, you can test it by coping
the files (from your local clone of the repository) to a temporary folder and
starting a local server. For example:

```bash
LEARN_TEST=$(mktemp -d)

rsync -avz ./frontend/dist/html/       ${LEARN_TEST}/
rsync -avz ./frontend/dist/pdf_books/  ${LEARN_TEST}/pdf_books/
rsync -avz ./frontend/dist/epub_books/ ${LEARN_TEST}/epub_books/
rsync -avz ./frontend/dist/zip/        ${LEARN_TEST}/zip/

rm -r ${LEARN_TEST}/_sources
rm -r ${LEARN_TEST}/_plantuml

( cd ${LEARN_TEST} && python3 -m http.server 8001 )
```

You can then point your browser on your host to 127.0.0.1:8001 to see the learn
website being served from the local Python-based HTTP server.

## Testing the source-code examples

The ReST files found in the [content directory](content) have `.. code:: ada`
blocks that contain source-code examples. To test the expected behavior of
those examples, a test script called `compile_blocks.py` is available, as well
as the `code_projects` module.

To make use of this test infrastructure, the best approach is to start the
Vagrant publishing server (`epub`) and install the environment:

```sh
cd /vagrant/frontend
export PYTHONPATH="$PYTHONPATH:sphinx:py_modules"
```


### Using the `compile_blocks.py` script

After the environment is set, running `compile_blocks.py --help` displays the help
message:

```sh
python3 tests/compile_blocks.py --help
```

To test the code examples from a specific course, select the ReST files from
that course and specify them as arguments to the `compile_blocks.py` script.
For example, to test the
[Introduction to Ada course](content/courses/intro-to-ada), run:

```sh
python3 tests/compile_blocks.py \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')
```


#### Verbose mode

For more verbosity, just add the `--verbose` switch. For example:

```sh
python3 tests/compile_blocks.py \
  --verbose                     \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')
```


#### Keep the build directory

To store the build files in a specific directory, just use the `--build-dir`
switch and specify the directory. Also, use the `--keep_files` switch to
prevent that directory from being deleted after the test is complete. For
example:

```sh
python3 tests/compile_blocks.py \
  --keep_files                  \
  --build-dir test_output       \
  --verbose                     \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')
```


#### Single ReST file

To test a single ReST file, specify just that file instead of multiple ReST
files:

```sh
python3 tests/compile_blocks.py \
  ../content/index.rst
```

#### Single code block

To test just a single code block from the ReST file, use the `--code-block-at`
switch:

```sh
python3 tests/compile_blocks.py \
  --code-block-at=28            \
  ../content/courses/intro-to-ada/chapters/imperative_language.rst
```


#### Maximum number of columns

To check whether a maximum limit of 80 columns per line is respected in the
code examples, use the `--max_columns` switch:

```sh
python3 tests/compile_blocks.py \
  --max-columns 80              \
  ../content/index.rst
```


### Using the code_projects module

The [code_projects module](frontend/py_modules/code_projects) contains the
actual Python modules that are called by the `compile_blocks.py` script. It's
possible to use them directly:

- `extract_projects.py` extracts all code blocks and stores into the specified
  directory;

- `check_projects.py` checks each code blocks from the specified directory.

For example, to build the source-code examples from the
[Introduction to Ada course](content/courses/intro-to-ada), run:

```sh
python3 py_modules/code_projects/extract_projects.py     \
  --build-dir test_output                                \
  $(find ../content/courses/intro-to-ada/ -name '*.rst')

python3 py_modules/code_projects/check_projects.py       \
  --build-dir test_output
```

For more examples and alternative configurations, please refer to the
[README of the code_projects module](frontend/py_modules/code_projects/README.md)
