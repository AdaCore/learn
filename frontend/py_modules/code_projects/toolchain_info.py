#! /usr/bin/env python3

import os
import re
import configparser

TOOLCHAIN_CONFIG = os.path.dirname(os.path.realpath(__file__))  + "/" + "toolchain.ini"

TOOLCHAIN_PATH: dict[str, str] = {}
TOOLCHAINS: dict[str, list[str]] = {}
DEFAULT_VERSION: dict[str, str] = {}

def init_toolchain_info() -> None:
    config = configparser.ConfigParser()

    config.read(TOOLCHAIN_CONFIG)

    DEFAULT_VERSION['gnat']      = config['default_version']['gnat']
    DEFAULT_VERSION['gnatprove'] = config['default_version']['gnatprove']
    DEFAULT_VERSION['gprbuild']  = config['default_version']['gprbuild']

    TOOLCHAIN_PATH['root']       = config['toolchain_path']['root']
    TOOLCHAIN_PATH['selected']   = config['toolchain_path']['selected']
    TOOLCHAIN_PATH['default']    = config['toolchain_path']['default']

    gnat_str      = str(config['toolchains']['gnat'])
    gnatprove_str = str(config['toolchains']['gnatprove'])
    gprbuild_str  = str(config['toolchains']['gprbuild'])

    TOOLCHAINS['gnat']      = re.split(r'\s+', gnat_str)
    TOOLCHAINS['gnatprove'] = re.split(r'\s+', gnatprove_str)
    TOOLCHAINS['gprbuild']  = re.split(r'\s+', gprbuild_str)

def get_toolchain_default_version(tool: str) -> str:
    # Force initialization if info isn't available
    if not tool in DEFAULT_VERSION:
        init_toolchain_info()

    return DEFAULT_VERSION[tool]
