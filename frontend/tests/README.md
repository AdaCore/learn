# Script files

## generate_gnatcheck_rules.py

This script will take parse the RST files located in the
content/courses/Guidelines_for_Safe_and_Secure_Ada_SPARK/chapters/guidelines
folder to generate a rules file for use with GNATcheck tool.
For each standard it finds, it will print some information (depending
on script options) and then the GNATcheck rule necessary to enforce
the standard.
