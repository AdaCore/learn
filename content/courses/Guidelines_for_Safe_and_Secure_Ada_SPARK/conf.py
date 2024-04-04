# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Coding Standards'
copyright = '2024, AdaCore Technologies'
author = 'AdaCore Technologies'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.extlinks',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []

# sphinx.ext.extlinks: markup to shorten external links
extlinks = {
    'arm': ('http://www.ada-auth.org/standards/12rm/html/RM-%s.html',
            '[Ada Reference Manual; section %s]'),
    'aarm': ('http://www.ada-auth.org/standards/12aarm/html/AA-%s.html',
            '[Annotated Ada Reference Manual; section %s]'),
    'arm22': ('http://www.ada-auth.org/standards/22rm/html/RM-%s.html',
              '[Ada Reference Manual; section %s]'),
    'aarm22': ('http://www.ada-auth.org/standards/22aarm/html/AA-%s.html',
              '[Annotated Ada Reference Manual; section %s]'),
    'rat05': ('https://www.adaic.org/resources/add_content/standards/05rat/html/Rat-%s.html',
              '[Rationale for Ada 2005; section %s]'),
    'rat12': ('http://www.ada-auth.org/standards/12rat/html/Rat12-%s.html',
              '[Rationale for Ada 2012; section %s]'),
    'wikipedia': ('https://en.wikipedia.org/wiki/%s',
               '[Wikipedia page: %s]'),
    'spark_ug': ('https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/%s.html',
                 '[SPARK User\'s Guide: %s]'),
    'spark_ugs': ('https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/source/%s',
                 '[SPARK User\'s Guide: %s]'),
    'spark_rm': ('https://docs.adacore.com/spark2014-docs/html/lrm/%s.html',
                 '[SPARK Reference Manual: %s]'),
    'spark_rm_url': ('https://docs.adacore.com/spark2014-docs/html/lrm/%s',
                     '[SPARK Reference Manual: %s]'),
    'gnat_ugn': ('https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/%s.html',
                 '[GNAT User\'s Guide for Native Platforms: %s]'),
    'gnat_ugn_url': ('https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/%s',
                     '[GNAT User\'s Guide for Native Platforms: %s]'),
    'gnat_rm': ('https://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/%s.html',
                '[GNAT Reference Manual: %s]'),
    'gnat_rm_url': ('https://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/%s',
                    '[GNAT Reference Manual: %s]'),
    'gnat_check_rm_url': ('https://docs.adacore.com/live/wave/lkql/html/gnatcheck_rm/gnatcheck_rm/%s',
                          '[GNATcheck Reference Manual: %s]'),
    'gnat_stack_ug_url': ('http://docs.adacore.com/live/wave/gnatstack/html/gnatstack_ug/%s',
                          '[GNATstack User\'s Guide: %s]'),
    }

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'alabaster'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
