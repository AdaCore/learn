#! /usr/bin/env sh

# Install requirements
pip install -q -r ../requirements.txt

set -v
set -e

# Get GNAT
if ! test -f $GNAT_TAR_PATH
then
    mkdir -p $TOOLS_DIR
    wget -O $GNAT_TAR_PATH \
        "http://mirrors.cdn.adacore.com/art/591c6d80c7a447af2deed1d7"
fi

# If needed, extract GNAT
if ! test -d $GNAT_PATH
then
    tar -xf $GNAT_TAR_PATH -C $TOOLS_DIR
fi

export PATH=$GNAT_PATH/bin:$PATH

# Log content
ls $HOME/build_tools
ls $GNAT_PATH
ls $GNAT_PATH/bin

which gcc
which gnatchop
which gprbuild
