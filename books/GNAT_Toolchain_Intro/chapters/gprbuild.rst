GPRbuild
========

.. include:: <isopub.txt>

This chapter presents some useful features of GPRbuild, the project
manager of the GNAT toolchain. For a detailed presentation of the tool,
please refer to the
`GPRbuild User’s Guide <https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html>`_.

Project dependencies
--------------------

GPRbuild supports project dependencies, so that you can reuse information
from existing projects. The keyword :ada:`with` allows for including
another project into the current project.

Simple dependency
~~~~~~~~~~~~~~~~~

Let's look at a very simple example where we have a package called
:ada:`Test_Pkg` associated with the project ``test_pkg.gpr``. This is the
content of the project file:

.. code-block:: none

    project Test_Pkg is
        for Source_Dirs use ("src");
        for Object_Dir use "obj";
    end Test_Pkg;

This is the code for the :ada:`Test_Pkg` package:

.. code-block:: ada

    package Test_Pkg is

       type T is record
          X : Integer;
          Y : Integer;
       end record;

       function Init return T;

    end Test_Pkg;

.. code-block:: ada

    package body Test_Pkg is

       function Init return T is
       begin
          return V : T do
             V.X := 0;
             V.Y := 0;
          end return;
       end Init;

    end Test_Pkg;

For this example, we use a directory ``test_pkg``, which contains the
project file, and the subdirectory ``test_pkg/src``, which contains the
source-code files. This is an overview of the directory structure:

.. code-block:: none

    |- test_pkg
    |    | test_pkg.gpr
    |    |- src
    |    |    | test_pkg.adb
    |    |    | test_pkg.ads

Now, our goal is to use the :ada:`Test_Pkg` package in a separate
application. Instead of directly including the source-code files of the
:ada:`Test_Pkg` in the project file of our application, we can simply
reference the existing project file for the package by using
``with "test_pkg.gpr"``. This is the resulting project file:

.. code-block:: none

    with "../test_pkg/test_pkg.gpr";

    project Default is
        for Source_Dirs use ("src");
        for Object_Dir use "obj";
        for Main use ("main.adb");
    end Default;

This is the code for the main application:

.. code-block:: ada

    with Test_Pkg; use Test_Pkg;

    procedure Main is
       A : T;
    begin
       A := Init;
    end Main;

By building the main project file (``default.gpr``), we're automatically
building all dependent projects. To be more specific, the project file for
the main application automatically detects the relevant information from
the dependent projects, such as ``test_pkg.gpr``. Therefore, using a
``with`` in the main project file is really all we have to do.

Dependency to dynamic libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Structuring project files to make use of dynamic (shared) libraries has
a very similar approach as the one we've seen so far. In fact, it's pretty
straightforward to convert to project above, so that :ada:`Test_Pkg` is
now compiled into a dynamic library and linked to our main application.
All we need are a few additions to the project file for the
:ada:`Test_Pkg` package:

.. code-block:: none

    library project Test_Pkg is
        for Source_Dirs use ("src");
        for Object_Dir use "obj";
        for Library_Name use "test_pkg";
        for Library_Dir use "lib";
        for Library_Kind use "Dynamic";
    end Test_Pkg;

This is what we had to do:

    - We changed the ``project`` to ``library project``.
    - We added the specification for ``Library_Name``, ``Library_Dir``
      and ``Library_Kind``.

The project file for the main application doesn't need to be changed,
since it automatically detects the dependency information (e.g.: path to
dynamic library) from the project file for the :ada:`Test_Pkg` package.
With these small changes, we were able to compile the :ada:`Test_Pkg`
package to a dynamic library and link it to our main application.

