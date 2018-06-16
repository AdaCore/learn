#! /usr/bin/env sh

export PATH=$GNAT_PATH/bin:$PATH

which gcc
which gnatchop
which gprbuild

make tests html
