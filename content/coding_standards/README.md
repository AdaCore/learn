# Overview

This repository is intended to supply a baseline Ada coding standard.
It uses the Sphinx/docutils build structure to generate a document in
the user's desired format.

# Content

## Top-level directory

   * README.md

      This file

   * make.bat

      ``sphinx-build`` build script for Windows

   * Makefile

      Makefile to use with ``sphinx-build``

## ada_coding_standards

This folder contains the source files necessary to generate the coding standards.

Most of the files should be fairly boilerplate. The directory of most interest is
the ``guidelines`` folder, which contains all of the actual coding standards, grouped
by area of concern.

## Rules

This folder contains the sample gnatcheck rules file as well as the lkql subfolder
which contains all non-standard rules implemented specifically for this coding standard.
All lkql files should be copied into the $CODEPEER_INSTALL/share/lkql folder before
running gnatcheck. 

**Note: If you add a new file into the ``guidelines`` folder, make sure you update the
``guidelines.rst`` file accordingly**

