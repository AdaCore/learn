GPRbuild
========

.. include:: ../../../global.txt

This chapter presents a brief overview of :program:`GPRbuild`, the project
manager of the GNAT toolchain.  It can be used to manage complex builds. In
terms of functionality, it's similar to :program:`make` and :program:`cmake`,
just to name two examples.

For a detailed presentation of the tool, please refer to the
`GPRbuild User’s Guide <https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html>`_.


Basic commands
--------------

As mentioned in the previous chapter, you can build a project using
:program:`gprbuild` from the command line:

.. code-block:: sh

    gprbuild -P project.gpr

In order to clean the project, you can use :program:`gprclean`:

.. code-block:: sh

    gprclean -P project.gpr


Project files
-------------

You can create project files using :program:`GNAT Studio`, which presents many
options on its graphical interface. However, you can also edit project
files manually as a normal text file in an editor, since its syntax is
human readable. In fact, project files use a syntax similar to the one
from the Ada language. Let's look at the basic structure of project files
and how to customize them.

Basic structure
~~~~~~~~~~~~~~~

The main element of a project file is a project declaration, which
contains definitions for the current project. A project file may also
include other project files in order to compose a complex build. One of
the simplest form of a project file is the following:

.. code-block:: none

    project Default is

       for Main use ("main");
       for Source_Dirs use ("src");

    end Default;

In this example, we declare a project named ``Default``. The
``for Main use`` expression indicates that the ``main.adb`` file is used
as the entry point (main source-code file) of the project. The main file
doesn't necessary need to be called ``main.adb``; we could use any source-code
implementing a main application, or even have a list of multiple main
files. The ``for Source_Dirs use`` expression indicates that the ``src``
directory contains the source-file for the application (including the main
file).

Customization
~~~~~~~~~~~~~

GPRbuild support scenario variables, which allow you to control the way
binaries are built. For example, you may want to distinguish between debug
and optimized versions of your binary. In principle, you could pass
command-line options to :program:`gprbuild` that turn debugging on and
off, for example. However, defining this information in the project file
is usually easier to handle and to maintain. Let's define a scenario
variable called ``ver`` in our project:

.. code-block:: none

    project Default is

       Ver := external ("ver", "debug");

       for Main use ("main");
       for Source_Dirs use ("src");

    end Default;

In this example, we're specifying that the scenario variable ``Ver`` is
initialized with the external variable ``ver``. Its default value is set
to ``debug``.

We can now set this variable in the call to :program:`gprbuild`:

.. code-block:: sh

    gprbuild -P project.gpr -Xver=debug

Alternatively, we can simply specify an environment variable. For example,
on Unix systems, we can say:

.. code-block:: sh

    export ver=debug

    # Value from environment variable "ver" used in the following call:

    gprbuild -P project.gpr

In the project file, we can use the scenario variable to customize the
build:

.. code-block:: none

    project Default is
       Ver := external ("ver", "debug");

       for Main use ("main.adb");
       for Source_Dirs use ("src");

       --  Using "ver" variable for obj directory
       for Object_Dir use "obj/" & Ver;

       package Compiler is
          case Ver is
             when "debug" =>
                for Switches ("Ada") use ("-g");
             when "opt" =>
                for Switches ("Ada") use ("-O2");
             when others =>
                null;
          end case;
       end Compiler;

    end Default;

We're now using ``Ver`` in the ``for Object_Dir`` clause to specify a
subdirectory of the ``obj`` directory that contains the object files.
Also, we're using ``Ver`` to select compiler options in the ``Compiler``
package declaration.

We could also specify all available options in the project file by
creating a typed variable. For example:

.. code-block:: none

    project Default is

       type Ver_Option is ("debug", "opt");
       Ver : Ver_Option := external ("ver", "debug");

       for Source_Dirs use ("src");
       for Main use ("main.adb");

       --  Using "ver" variable for obj directory
       for Object_Dir use "obj/" & Ver;

       package Compiler is
          case Ver is
             when "debug" =>
                for Switches ("Ada") use ("-g");
             when "opt" =>
                for Switches ("Ada") use ("-O2");
             when others =>
                null;
          end case;
       end Compiler;

    end Default;

The advantage of this approach is that :program:`gprbuild` can now check
whether the value that you provide for the ``ver`` variable is available
on the list of possible values and give you an error if you're entering
a wrong value.

Project dependencies
--------------------

:program:`GPRbuild` supports project dependencies.  This allows you to
reuse information from existing projects. Specifically, the keyword
:ada:`with` allows you to include another project within the current
project.

Simple dependency
~~~~~~~~~~~~~~~~~

Let's look at a very simple example. We have a package called
:ada:`Test_Pkg` associated with the project file :file:`test_pkg.gpr`, which
contains:

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
project file and a subdirectory :file:`test_pkg/src` containing the
source files.  The directory structure looks like this:

.. code-block:: none

    |- test_pkg
    |    | test_pkg.gpr
    |    |- src
    |    |    | test_pkg.adb
    |    |    | test_pkg.ads

Suppose we want to use the :ada:`Test_Pkg` package in a new
application. Instead of directly including the source files of
:ada:`Test_Pkg` in the project file of our application (either
directly or indirectly), we can instead reference the existing project
file for the package by using ``with "test_pkg.gpr"``. This is the
resulting project file:

.. code-block:: none

    with "../test_pkg/test_pkg.gpr";

    project Default is
        for Source_Dirs use ("src");
        for Object_Dir use "obj";
        for Main use ("main.adb");
    end Default;

And this is the code for the main application:

.. code-block:: ada

    with Test_Pkg; use Test_Pkg;

    procedure Main is
       A : T;
    begin
       A := Init;
    end Main;

When we build the main project file (:file:`default.gpr`), we're
automatically building all dependent projects. More specifically, the
project file for the main application automatically includes the
information from the dependent projects such as
:file:`test_pkg.gpr`. Using a ``with`` in the main project file is all
we have to do for that to happen.

Dependencies to dynamic libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can structure project files to make use of dynamic (shared)
libraries using a very similar approach. It's straightforward to
convert the project above so that :ada:`Test_Pkg` is now compiled into
a dynamic library and linked to our main application.  All we need to
do is to make a few additions to the project file for the
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

We don't need to change the project file for the main application because
:program:`GPRbuild` automatically detects the dependency information
(e.g., the path to the dynamic library) from the project file for the
:ada:`Test_Pkg` package.  With these small changes, we're able to
compile the :ada:`Test_Pkg` package to a dynamic library and link it
with our main application.

Configuration pragma files
--------------------------

Configuration pragma files contain a set of pragmas that modify the
compilation of source files according to external requirements. For
example, you may use pragmas to either relax or strengthen
requirements depending on your environment.

In :program:`GPRbuild`, we can use ``Local_Configuration_Pragmas`` (in
the ``Compiler`` package) to indicate the configuration pragmas file
we want :program:`GPRbuild` to use with the source files in our project.

The file :file:`gnat.adc` shown here is an example of a configuration
pragma file:

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

Each pragma contained in :file:`gnat.adc` is used in the compilation
of each file, as if that pragma was placed at the beginning of each
file.

Configuration packages
----------------------

You can control the compilation of your source code by creating
variants for various cases and selecting the appropriate variant in
the compilation package in the project file. One example where this is
useful is conditional compilation using Boolean constants, shown in
the code below:

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
different behavior for each usage. For this simple example, there are
only two possible cases: either :ada:`Debug` is :ada:`True` or
:ada:`False`. However, we can apply this strategy to create more
complex cases.

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

In this case, :program:`GPRbuild` selects the appropriate directory to
look for the :file:`config.ads` file according to information we
provide for the compilation process. We do this by using a scenario
type called ``Mode_Type`` in our project file:


.. code-block:: sh

    gprbuild -P default.gpr -Xmode=release

.. code-block:: none

    project Default is

       type Mode_Type is ("debug", "release");

       Mode : Mode_Type := external ("mode", "debug");

       for Source_Dirs use ("src", "src/" & Mode);
       for Object_Dir use "obj";
       for Main use ("main.adb");

    end Default;

We declare the scenario variable ``Mode`` and use it in the
``Source_Dirs`` declaration to add the desired path to the
subdirectory containing the :file:`config.ads` file. The expression
``"src/" & Mode`` concatenates the user-specified mode to select the
appropriate subdirectory. For more complex cases, we could use either
a tree of subdirectories or multiple scenario variables for each
aspect that we need to configure.
