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

Configuration pragmas files
---------------------------

Configuration pragmas files contain a list of pragmas that drive
compilation of source-code files according to external requirements. For
example, depending on your environment, you may use pragmas for relaxed
requirements or more strict requirements.

In :program:`GPRbuild`, we can use ``Local_Configuration_Pragmas``
(from the ``Compiler`` package) to indicate the configuration pragmas file
to be used for the source-code files in our project.

The file ``gnat.adc`` listed below is an example of a configuration
pragmas file:

.. code-block:: none

    pragma Suppress (Overflow_Check);

We can use this file in our project by declaring a ``Compiler`` package.
This is the complete project file:

.. code-block:: none

    project Default is

       for Source_Dirs use ("src");
       for Object_Dir use "obj";
       for Main use ("main.adb");

       package Compiler is
          for Local_Configuration_Pragmas use "gnat.adc";
       end Compiler;

    end Default;

When compiling the code with this project, each pragma of ``gnat.adc`` is
taken into account.

Configuration packages
----------------------

It's possible to drive the compilation of the source-code by creating
packages for each use-case and selecting the appropriate package in the
compilation package. This is useful for example for conditional
compilation using Boolean constants, as in the code below:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    with Config;

    procedure Main is
    begin

       if Config.Debug then
          Put_Line ("Debug version");
       else
          Put_Line ("Release version");
       end if;
    end Main;

In this example, the Boolean constant is declared in the :ada:`Config`
package. By having multiple versions of the :ada:`Config` package, we may
create different specifications for each use-case. For this simple
example, there are only two possible use-cases: either :ada:`Debug` is
:ada:`True` or :ada:`False`. However, you can apply this strategy to
create more complex use-cases.

For this example, we store the packages in the subdirectories ``debug``
and ``release`` of the source-code directory. This is the content of
the ``src/debug/config.ads`` file:

.. code-block:: ada

    package Config is

       Debug : constant Boolean := True;

    end Config;

This is the content of the ``src/release/config.ads`` file:

.. code-block:: ada

    package Config is

       Debug : constant Boolean := False;

    end Config;

In this case, :program:`GPRbuild` selects the appropriate directory
containing the ``config.ads`` file according to information that we
provide for the compilation process. For example:

.. code-block:: sh

    gprbuild -P default.gpr -Xmode=release

We can do this by using a scenario type called ``Mode_Type`` in our
project file:

.. code-block:: none

    project Default is

       type Mode_Type is ("debug", "release");

       Mode : Mode_Type := external ("mode", "debug");

       for Source_Dirs use ("src", "src/" & Mode);
       for Object_Dir use "obj";
       for Main use ("main.adb");

    end Default;

We declare the scenario variable ``Mode`` and use it to add the path to
the subdirectory containing the ``config.ads`` file in the ``Source_Dirs``
specification. The expression ``"src/" & Mode`` takes the user-selected
mode to select the appropriate subdirectory. For more complex use-cases,
we could use a tree of subdirectories or multiple scenario variables for
each aspect that needs to be configurable.
