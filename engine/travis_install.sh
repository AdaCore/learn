#! /usr/bin/env sh

# Install requirements
pip install -q -r ../requirements.txt

set -v
set -e

package_file=5cdffc5409dcd015aaf82626

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
        "http://mirrors.cdn.adacore.com/art/${package_file}"
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
