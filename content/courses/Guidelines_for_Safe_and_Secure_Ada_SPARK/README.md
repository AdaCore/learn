# Guidelines for Safe and Secure Ada/SPARK

Customizable coding standards maintenance and generation.

## Introduction

This folder contains all the necessary content necessary to generate
a coding standard for use in any size software project. The default
content is a good starting point, but most users will want to customize
the standards to fit their own needs and coding styles.

In addition, many of the standards we want to enforce could be verified
automatically with a static analysis tool. This repository assumes the
`GNATcheck` tool will be used. Most of the standards contain the specific
rule that would be used to verify compliance.

This README file contains instructions on how to customize content
and build your own coding standards document.

## Prerequisites

### Sphinx

The only tool actually required for building your document is the
Sphinx utilities located [here](https://www.sphinx-doc.org/). Once
downloaded and installed, most of the output formats created by
Sphinx will be available. For formats that require additional tools,
please see the Sphinx documentation

### GNATcheck

`GNATcheck` is part of the Static Analysis Suite provided by **AdaCore**.
It is designed as a coding standards checker, and comes with many
pre-defined rules to use in validating standards conformance. In
addition, it uses the **Langkit Query Language** to allow users to
define their own rules.

### Python

You will notice that many of the individual coding standards specify
the verification method as **GNATcheck rule**. For these methods,
a particular `GNATcheck` rule is specified, using the RST role **rule**
to identify formatting for the rule. In the **tools** folder is a
python script **generate_rules_file.py** that can be used to build
a rules input file for `GNATcheck`. To use that script, you would
obviously need to install a version of Python (3.9 or better should work).

### make (Linux only)

If you are running this in a Linux environment, the build process
uses a **Makefile**, so you do need to have **make** on your path.

## Modifying the Content

The content files are all written in ReStructured Text (RST). For
those not familiar, a good "cheat sheet" can be found at the
Docutils site on SourceForge
[here](https://docutils.sourceforge.io/docs/ref/rst/directives.html).
The most likely candidate for modifications is the **chapters/guidelines**
folder.

### Adding a New Standard

For each guideline subsection, there is an RST file and a subfolder.
To add a new standard, first go into the subfolder and create a new file for
your standard. Feel free to use any of the other files as a template! Then,
edit the subsection RST file to include your new standard file in the
*.toctree* structure. That’s it – just rebuild the document and you’ve added
your standard!

If your new standard is detectable using a `GNATcheck` rule, make sure you
set the Verification Method field in your standard to indicate
`GNATcheck`, and which rule it needs to verify the standard. For example:

```
   GNATcheck rule: :rule:`OTHERS_In_CASE_Statements`
```

### Adding a New Programming Practice Category

If you want to add a whole new sub-collection of standards, the process
is similar. Now, in the **guidelines** folder you need to add an RST
file and subfolder, and populate them appropriately. (Again, copy-and-paste
is the best way to get started.) The extra step here is you need to modify
the *.toctree* structure in the **index.rst** file (located in the document's
main directory) and add your new subsection.

## Building Your Document

Once you have made your modifications, building the document is easy!
Go into the document's main directory and type `make <format>` where
`<format>` is one of the allowed formats. Typing `make` with no
format will give you a list of all available formats. Note that
this will work on Windows as well as Linux - there is a file **make.bat**
that works on Windows, and a **Makefile** that works on Linux.

Output will be placed in the **build** directory (which will be created
if it does not exist).

### Customizing your build process

The **make** files are designed to simplify the build process. If you are
more familiar with the Sphinx build process, you can either edit those files
to add your own options, or call `sphinx-build` directly with the
appropriate options.
