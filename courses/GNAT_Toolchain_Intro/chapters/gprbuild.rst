GPRbuild
========

.. include:: <isopub.txt>

.. include:: <isoamsa.txt>

.. role:: ada(code)
   :language: ada

This chapter presents some useful features of :program:`GPRbuild`, the
project manager component of the GNAT toolchain. For a detailed
presentation of the tool, please refer to the `GPRbuild User's Guide
<https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html>`_.

Project dependencies
--------------------

:program:`GPRbuild` supports project dependencies, so you can reuse
information from existing projects. The keyword :ada:`with` allows you
to include another project in the current project.

Simple dependency
~~~~~~~~~~~~~~~~~

Let's look at a very simple example. We have a package called
:ada:`Test_Pkg` associated with the project file :file:`test_pkg.gpr`. This
is the content of the project file:

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

For this example, we use a directory :file:`test_pkg` containing the
project file and the subdirectory :file:`test_pkg/src` containing the
source-code files.  The directory structure looks like this:

.. code-block:: none

    |- test_pkg
    |    | test_pkg.gpr
    |    |- src
    |    |    | test_pkg.adb
    |    |    | test_pkg.ads

Suppose we want to use the :ada:`Test_Pkg` package in a new
application. Instead of directly including the source-code files of the
:ada:`Test_Pkg` in the project file of our application, we can instead
reference the existing project file for the package by using ``with
"test_pkg.gpr"``. This is the resulting project file:

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

When we build the main project file (:file:`default.gpr`), we're
automatically building all dependent projects. More specifically, the
project file for the main application automatically detects the relevant
information from the dependent projects such as :file:`test_pkg.gpr`. Using
a ``with`` in the main project file is all we have to do for that to
happen.

Dependencies to dynamic libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can structuring project files to make use of dynamic (shared) libraries
using a very similar approach as above. It's straightforward to convert the
project above so that :ada:`Test_Pkg` is now compiled into a dynamic
library and linked to our main application.  All we need to do are make a
few additions to the project file for the :ada:`Test_Pkg` package:

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

The project file for the main application doesn't need to be changed: it
automatically detects the dependency information (e.g.: the path to the
dynamic library) from the project file for the :ada:`Test_Pkg` package.
With these small changes, we're able to compile the :ada:`Test_Pkg` package
to a dynamic library and link it to our main application.

Configuration pragmas files
---------------------------

Configuration pragmas files contain a list of pragmas that modify the
compilation of source-code files according to external requirements. For
example, you may use pragmas to either relax or strength requirements
depending on your environment.;

In :program:`GPRbuild`, we can use ``Local_Configuration_Pragmas`` (in the
``Compiler`` package) to indicate the configuration pragmas file to be used
with the source-code files in our project.

The file :file:`gnat.adc` shown here is an example of a configuration
pragmas file:

.. code-block:: none

    pragma Suppress (Overflow_Check);

We can use this in our project by declaring a ``Compiler`` package.  Here's
the complete project file:

.. code-block:: none

    project Default is

       for Source_Dirs use ("src");
       for Object_Dir use "obj";
       for Main use ("main.adb");

       package Compiler is
          for Local_Configuration_Pragmas use "gnat.adc";
       end Compiler;

    end Default;

When compiling the code with this project, each pragma contained in
:file:`gnat.adc` is applied to the compilation.

Configuration packages
----------------------

You can control the compilation of the source-code by creating packages for
each use-case and selecting the appropriate package in the compilation
package in the project file. One example where this is useful is
conditional compilation using Boolean constants, as in the code below:

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

In this example, we declared the Boolean constant in the :ada:`Config`
package. By having multiple versions of that package, we can create
different specifications for each use-case. For this simple example, there
are only two possible cases: either :ada:`Debug` is :ada:`True` or
:ada:`False`. However, we can apply this strategy to create more complex
cases.

In our next example, we store the packages in the subdirectories :file:`debug`
and :file:`release` of the source code directory.  Here's the content of the
:file:`src/debug/config.ads` file:

.. code-block:: ada

    package Config is

       Debug : constant Boolean := True;

    end Config;

Here's the :file:`src/release/config.ads` file:

.. code-block:: ada

    package Config is

       Debug : constant Boolean := False;

    end Config;

In this case, :program:`GPRbuild` selects the appropriate directory to look
for the :file:`config.ads` file according to information we provide for the
compilation process. For example:

.. code-block:: sh

    gprbuild -P default.gpr -Xmode=release

We do this by using a scenario type called ``Mode_Type`` in our project
file:

.. code-block:: none

    project Default is

       type Mode_Type is ("debug", "release");

       Mode : Mode_Type := external ("mode", "debug");

       for Source_Dirs use ("src", "src/" & Mode);
       for Object_Dir use "obj";
       for Main use ("main.adb");

    end Default;

We declare the scenario variable ``Mode`` and use it in the ``Source_Dirs``
specification to add the path to the subdirectory containing the
:file:`config.ads` file. The expression ``"src/" & Mode`` concatenates the
user-specified mode to select the appropriate subdirectory. For more
complex cases, we could use a tree of subdirectories or multiple scenario
variables for each aspect that we need to configure.
