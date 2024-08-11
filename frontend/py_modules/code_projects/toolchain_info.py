#! /usr/bin/env python3

import os
import re
import configparser

TOOLCHAIN_CONFIG = os.path.dirname(os.path.realpath(__file__))  + "/" + "toolchain.ini"

TOOLCHAIN_PATH = {}
TOOLCHAINS = {}
DEFAULT_VERSION = {}

def init_toolchain_info():
    config = configparser.ConfigParser()

    config.read(TOOLCHAIN_CONFIG)

    DEFAULT_VERSION['gnat']      = config['default_version']['gnat']
    DEFAULT_VERSION['gnatprove'] = config['default_version']['gnatprove']
    DEFAULT_VERSION['gprbuild']  = config['default_version']['gprbuild']

    TOOLCHAIN_PATH['root']       = config['toolchain_path']['root']
    TOOLCHAIN_PATH['selected']   = config['toolchain_path']['selected']
    TOOLCHAIN_PATH['default']    = config['toolchain_path']['default']

    TOOLCHAINS['gnat']      = str(config['toolchains']['gnat'])
    TOOLCHAINS['gnatprove'] = str(config['toolchains']['gnatprove'])
    TOOLCHAINS['gprbuild']  = str(config['toolchains']['gprbuild'])

    TOOLCHAINS['gnat']      = re.split(r'\s+',TOOLCHAINS['gnat'])
    TOOLCHAINS['gnatprove'] = re.split(r'\s+',TOOLCHAINS['gnatprove'])
    TOOLCHAINS['gprbuild']  = re.split(r'\s+',TOOLCHAINS['gprbuild'])

def get_toolchain_default_version(tool):
    # Force initialization if info isn't available
    if not tool in DEFAULT_VERSION:
        init_toolchain_info()

    return DEFAULT_VERSION[tool]
