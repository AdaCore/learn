GPRbuild
========

.. include:: <isopub.txt>

This chapter presents a brief overview of GPRbuild, the project manager of
the GNAT toolchain.  It can be used to manage complex builds. In terms of
functionality, it's similar to :program:`make` and :program:`cmake`, just
to name two examples.

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

You can create project files using :program:`GPS`, which presents many
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
doesn't necessary be called ``main.adb``; we could use any source-code
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
