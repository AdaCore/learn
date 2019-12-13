GNAT Community
==============

.. include:: <isopub.txt>

.. include:: <isoamsa.txt>

This chapter presents the steps needed to install the GNAT Community
toolchain and how to use basic commands from the toolchain.

Installation
------------

These are the basics steps to install GNAT Community on all platforms:

    - Go to the
      `AdaCore Community page <https://www.adacore.com/community>`_.

    - Download the GNAT installer.

    - Run the GNAT installer.

        - Leave all options checked on the "Select Components" page.

On Windows platforms, continue with the following steps:

    - Add ``C:\GNAT\2019\bin`` to your ``Path`` environment variable.

        - The environment variables can be found in the
          ``System Properties`` window of the ``Control Panel``.

    - You might need to restart your computer for the settings to take
      effect.

On Linux platforms, perform the following steps:

    - Make sure the GNAT installer has execution permissions before
      running it.

    - Select the directory where you want to install the toolchain.

        - For example: ``/home/me/GNAT/2019``

    - Add the path to the ``bin`` directory (within the toolchain
      directory) as the first directory in your ``PATH`` environment
      variable.

        - For example: ``/home/me/GNAT/2019/bin``.

Basic commands
--------------

Now that the toolchain is installed, you can start using it. From the
command line, you can compile a project using :program:`gprbuild`. For
example:

.. code-block:: sh

    gprbuild -P project.gpr

You can find the binary built with the command above in the *obj*
directory. You can the run it in the same way as you would do with any
other executable on your platform. For example:

.. code-block:: sh

    obj/main

A handy command-line option for :program:`gprbuild` you might want to use
is ``-p``, which automatically creates directories such as ``obj`` if they
aren't in the directory tree:

.. code-block:: sh

    gprbuild -p -P project.gpr

Ada source-code are stored in *.ads* and *.adb* files. To view the
content of these files, you can use the GNAT Programming Studio
(:program:`GPS`). To open :program:`GPS`, double-click on the *.gpr*
project file or invoke :program:`GPS` on the command line:

.. code-block:: sh

    gps -P project.gpr

To compile your project using :program:`GPS`, use the top-level menu to
invoke ``Build`` |srarr| ``Project`` |srarr| ``main.adb`` (or press the
keyboard shortcut ``F4``). To run the main program, click on
``Build`` |srarr| ``Run`` |srarr| ``main`` (or press the keyboard shortcut
``Shift + F2``).

Compiler warnings
-----------------

One of the strengths of the GNAT compiler is its ability to generate
many useful warnings. Some are displayed by default but others need to
be explicitly enabled. In this section, we discuss some of these
warnings, their purpose, and how you activate them.

``-gnatwa`` switch and warning suppression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. sectionauthor:: Bob Duff

We first need to understand the difference between a *warning* and an
*error*. Errors are generally violations of the Ada language rules as
specified in the Ada Reference Manual; warnings don't indicate
violations of those rules, but rather flag things in a program that
seem suspicious to the compiler.  Warnings are GNAT-specific, so other
Ada compilers might not warn about the same things GNAT does or warn
in a different way. Warnings are typically conservative; that is, some
warnings are false alarms. The programmer needs to study the code to
determine if the warning is describing a real problem.

Some warnings are produced by default while others are produced only if a
switch enables them. Use the ``-gnatwa`` switch to turn on (almost) all
warnings.

Warnings are useless if you don't do something about them. If you give your
team member some code that causes warnings, how are they supposed to know
whether they represent real problems?  If you don't do something about
them, people will soon starting ignoring warnings and there'll be many
warnings scattered all over your code.  To avoid this, you may want to use
the ``-gnatwae`` switch to both turn on (almost) all warnings and to treat
warnings as errors. This forces you to get a clean (no warnings or errors)
compilation.

However, some warnings are false alarms. Use :ada:`pragma Warnings (Off)` to
suppress false alarms. It's best to be as specific as possible and narrow
down to a single line of code and a single warning. Also, use a comment to
explain why the warning is a false alarm if it's not obvious.

Let's look at the following example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Warnings_Example is

       procedure Mumble (X : Integer) is
       begin
          Put_Line ("Mumble processing...");
       end Mumble;

    end Warnings_Example;

We compile the above code with ``-gnatwae``:

.. code-block:: none

    gnat compile -gnatwae ./src/warnings_example.adb

This causes GNAT to complain:

.. code-block:: none

    warnings_example.adb:5:22: warning: formal parameter "X" is not referenced

But the following compiles cleanly:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Warnings_Example is

       pragma Warnings (Off, "formal parameter ""X"" is not referenced");
       procedure Mumble (X : Integer) is
       pragma Warnings (On, "formal parameter ""X"" is not referenced");

       --  X is ignored here, because blah blah blah...
       begin
          Put_Line ("Mumble processing...");
       end Mumble;

    end Warnings_Example;

Here we've suppressed a specific warning message on a specific line.

If you get many warnings of a specific type and it's not feasible to fix
all of them, you can suppress that type of message so the good warnings
won't get buried beneath a pile of bogus ones. For example, you can use the
``-gnatwaeF`` switch to silence the warning on the first version of
:ada:`Mumble` above: the ``F`` suppresses warnings on unreferenced formal
parameters.  Using it would be a good idea if you have many of those.

As discussed above, ``-gnatwa`` activates almost all warnings, but not
all. Refer to the `section on warnings
<https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#warning-message-control>`_
in the GNAT User's Guide to get a list of the remaining warnings you could
also enable in your project. One example is ``-gnatw.o``, which displays
warnings when the compiler detects modified but unreferenced *Out*
parameters. Consider the following example:

.. code-block:: ada

    package Warnings_Example is

       procedure Process (X : in out Integer;
                          B :    out Boolean);

    end Warnings_Example;

.. code-block:: ada

    package body Warnings_Example is

       procedure Process (X : in out Integer;
                          B :    out Boolean) is
       begin
          if X = Integer'First or else X = Integer'Last then
             B := False;
          else
             X := X + 1;
             B := True;
          end if;
       end Process;

    end Warnings_Example;

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    with Warnings_Example; use Warnings_Example;

    procedure Main  is
       X : Integer := 0;
       Success : Boolean;
    begin
       Process (X, Success);
       Put_Line (Integer'Image (X));
    end Main;

If we build the main application using the ``-gnatw.o`` switch, the
compiler warns us that we didn't reference the :ada:`Success` variable,
which was modified in the call to :ada:`Process`:

.. code-block:: none

    main.adb:8:16: warning: "Success" modified by call, but value might not be referenced

In this case, this is actually a bug in our program, since :ada:`X` only
contains a valid value if :ada:`Success` is :ada:`True`. The corrected code
for :ada:`Main` is:

.. code-block:: ada

    -- ...
    begin
       Process (X, Success);

       if Success then
          Put_Line (Integer'Image (X));
       else
          Put_Line ("Couldn't process variable X.");
       end if;
    end Main;

To summarize, we suggest turning on as many warnings as makes sense for
your project. Then, when you see a warning message, look at the code and
decide if it's real. If it is, fix the code. If it's a false alarm,
suppress the warning. In either case, we strongly recommend you make the
warning disappear before you check your code into your configuration
management system.

Style checking
~~~~~~~~~~~~~~

GNAT provides many options to configure style checking of your code. The
main compiler switch for this is ``-gnatyy``, which sets almost all
standard style check options. As indicated by the `section on style
checking
<https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#style-checking>`_
in the GNAT User's Guide, using this switch "is equivalent to
``-gnaty3aAbcefhiklmnprst``, that is all checking options enabled with the
exception of ``-gnatyB``, ``-gnatyd``, ``-gnatyI``, ``-gnatyLnnn``,
``-gnatyo``, ``-gnatyO``, ``-gnatyS``, ``-gnatyu``, and ``-gnatyx``."

You may find that selecting the appropriate coding style is useful to
detect issues at early stages. For example, the ``-gnatyO`` switch checks
that overriding subprograms are explicitly marked as such. Using this
switch can avoid surprises when you didn't intentionally want to override
an operation for some data type. Therefore, we recommend studying the list
of coding style switches and selecting the ones that seem relevant for your
project. When in doubt, you can start by using all of them |mdash| using
``-gnatyy`` and ``-gnatyBdIL4oOSux``, for example |mdash| and deactivating
the ones that cause too much *noise* during compilation.
