#! /usr/bin/env python3

import os
import re

import blocks
import toolchain_info as info

def set_toolchain(block : blocks.CodeBlock):

    # Init toolchain info if not initialized
    if not 'root' in info.TOOLCHAIN_PATH:
        info.init_toolchain_info()

    # Reset toolchain to ensure that no toolchain exists in the "select" folder
    reset_toolchain()

    # Create "selected" folder if it doesn't exist
    selected_path = info.TOOLCHAIN_PATH['selected']
    os.makedirs(selected_path,
                mode=0o777,
                exist_ok=True)

    if block.gnat_version[0] != "default":
        ver = block.gnat_version[1]

        os.symlink(info.TOOLCHAIN_PATH['root']     + '/gnat/' + ver,
                   info.TOOLCHAIN_PATH['selected'] + '/gnat')

    if block.gnatprove_version[0] != "default":
        ver = block.gnatprove_version[1]

        os.symlink(info.TOOLCHAIN_PATH['root']     + '/gnatprove/' + ver,
                   info.TOOLCHAIN_PATH['selected'] + '/gnatprove')

    if block.gprbuild_version[0] != "default":
        ver = block.gprbuild_version[1]

        os.symlink(info.TOOLCHAIN_PATH['root']     + '/gprbuild/' + ver,
                   info.TOOLCHAIN_PATH['selected'] + '/gprbuild')

def reset_toolchain():
    selected_folders = [info.TOOLCHAIN_PATH['selected'] + '/gnat',
                        info.TOOLCHAIN_PATH['selected'] + '/gnatprove',
                        info.TOOLCHAIN_PATH['selected'] + '/gprbuild']

    for f in selected_folders:
        if os.path.exists(f):
            os.unlink(f)
