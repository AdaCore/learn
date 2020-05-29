#! /usr/bin/env sh

# Install requirements
pip install -q -r frontend/requirements.txt

set -v
set -e

installer_hash=4d99b7b2f212c8efdab2ba8ede474bb9fa15888d
installer_name=gnat-2020-20200429-x86_64-linux-bin

# Create tools directory
if ! test -d $TOOLS_DIR
then
    mkdir -p $TOOLS_DIR
fi

# Get / update gnat_community_install_script repo
if ! test -d $TOOLS_DIR/gnat_community_install_script
then
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnat_community_install_script)
else
    (cd $TOOLS_DIR/gnat_community_install_script && git pull)
fi

# Get GNAT
if ! test -f $GNAT_INSTALLER_PATH
then
    wget -O $GNAT_INSTALLER_PATH \
        "https://community.download.adacore.com/v1/${installer_hash}?filename=${installer_name}"
fi

# If needed, extract GNAT
if ! test -d $GNAT_PATH
then
    (cd $TOOLS_DIR && sh gnat_community_install_script/install_package.sh $GNAT_INSTALLER_PATH $GNAT_PATH)
fi

export PATH=$GNAT_PATH/bin:$PATH

# Log content
ls $TOOLS_DIR
ls $GNAT_PATH
ls $GNAT_PATH/bin

which gcc
which gnatchop
which gprbuild
