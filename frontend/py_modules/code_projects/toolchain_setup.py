#! /usr/bin/env python3

import os

import blocks

DEFAULT_VERSION = {}
DEFAULT_VERSION['gnat'] = "12.2.0-1"
DEFAULT_VERSION['gnatprove'] = "12.1.0-1"
DEFAULT_VERSION['gprbuild'] = "22.0.0-1"
TOOLCHAIN_PATH = "/opt/ada"

def get_toolchain_default_version(tool):
    return DEFAULT_VERSION[tool]

def set_toolchain(block : blocks.CodeBlock):

    # Reset toolchain to ensure that no toolchain exists in the "select" folder
    reset_toolchain()

    # Create "selected" folder if it doesn't exist
    selected_path = TOOLCHAIN_PATH + '/selected'
    os.makedirs(selected_path,
                mode=0o777,
                exist_ok=True)

    gnat_version = DEFAULT_VERSION['gnat']
    os.symlink(TOOLCHAIN_PATH + '/gnat/' + gnat_version,
               TOOLCHAIN_PATH + '/selected/gnat')

    gnatprove_version = DEFAULT_VERSION['gnatprove']
    os.symlink(TOOLCHAIN_PATH + '/gnatprove/' + gnatprove_version,
               TOOLCHAIN_PATH + '/selected/gnatprove')

    gprbuild_version = DEFAULT_VERSION['gprbuild']
    os.symlink(TOOLCHAIN_PATH + '/gprbuild/' + gprbuild_version,
               TOOLCHAIN_PATH + '/selected/gprbuild')


def reset_toolchain():
    selected_folders = [TOOLCHAIN_PATH + '/selected/gnat',
                        TOOLCHAIN_PATH + '/selected/gnatprove',
                        TOOLCHAIN_PATH + '/selected/gprbuild']

    for f in selected_folders:
        if os.path.exists(f):
            os.unlink(f)
