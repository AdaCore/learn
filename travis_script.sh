#! /usr/bin/env sh

export PATH=$GNAT_PATH/bin:$PATH

set -v
set -e

which gcc
which gnatchop
which gprbuild

cd frontend
ls
make SPHINXOPTS="-W" tests
# TODO: need to build the frontend to test build

# TODO: need to also test backend