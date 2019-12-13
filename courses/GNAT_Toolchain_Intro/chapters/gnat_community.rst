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

    - Add ``C:\GNAT\2018\bin`` to your ``Path`` environment variable.

        - The environment variables can be found in the
          ``System Properties`` window of the ``Control Panel``.

    - You might need to restart your computer for the settings to take
      effect.

On Linux platforms, perform the following steps:

    - Make sure the GNAT installer has execution permissions before
      running it.

    - Select the directory where you want to install the toolchain.

        - For example: ``/home/me/GNAT/2018``

    - Add the path to the ``bin`` directory (within the toolchain
      directory) as the first directory in your ``PATH`` environment
      variable.

        - For example: ``/home/me/GNAT/2018/bin``.

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

Expanded source code
--------------------

GNAT provides a command-line option to generate an expanded form of a
source file. You can use this to analyze what operations are used by
the source code.

Let's start with a simple example:

.. code-block:: ada

    procedure Main is
       F : Float := 0.0;
    begin
       F := F + 1.0;
    end Main;

To generate the expanded form, we run the compiler with the ``-gnatG``
switch and specify the source-code file containing the :ada:`Main`
procedure:

.. code-block:: sh

    gnat compile -gnatG main.adb > main.expanded

In this example, we redirected the output to the :file:`main.expanded`
file, which will contain the following expanded source-code:

.. code-block:: none

    Source recreated from tree for Main (body)
    ------------------------------------------


    procedure main is
       f : float := 0.0;
    begin
       f := f + [8388608.0*2**(-23)];
       return;
    end main;

The only substantive change from our input is that the original
floating-point value (:ada:`1.0`) is converted to a representation close to
the machine representation, which uses a 23-bit mantissa and an 8-bit
exponent: ``[8388608.0*2**(-23)]``

Let's look at an example using operator overriding:

.. code-block:: ada

    procedure Main is

       type Int is new Integer;

       overriding function "+" (A, B : Int) return Int;

       function "+" (A, B : Int) return Int is
          AI : Integer := Integer (A);
          BI : Integer := Integer (B);
       begin
          return Int (AI + BI);
       end;

       A : Int     := 1;
       B : Integer := 1;
    begin
       A := A + 2;
       B := B + 2;
    end Main;

Starting with the same procedure as before, we get the following
expanded code:

.. code-block:: none

    Source recreated from tree for Main (body)
    ------------------------------------------

    procedure main is
       [type main__TintB is new integer]
       freeze main__TintB []
       type main__int is new integer;
       function main__Oadd (a : main__int; b : main__int) return main__int;

       function main__Oadd (a : main__int; b : main__int) return main__int is
          ai : integer := integer(a);
          bi : integer := integer(b);
       begin
          return main__int(ai {+} bi);
       end main__Oadd;

       a : main__int := 1;
       b : integer := 1;
    begin
       a := main__Oadd (1, 2);
       b := b {+} 2;
       return;
    end main;

This shows more significant differences than the last example.  When
we analyze the expanded code, we notice that the compiler selects the
overridden addition for the operation on the :ada:`A` variable of
:ada:`Int` type. In this expansion, the :ada:`main__Oadd` function
represents the overridden addition. For the operation on the :ada:`B`
variable, the standard operator is used.  The brackets (``{+}``)
indicate that overflow checking is required when performing the
operation.

Analyzing the expanded code in this way is helpful to verify that the
operation selected by the compiler is the one we expected.  You may
also want to look at the expanded code to see how high-level
constructs such as tasking are implemented.


Target-dependent information
----------------------------

Target-dependent information refers to information about the computer
architecture to be used as the target of the compilation process. This
includes information such as size and alignment of base data types and
endianness.

You can generate a file containing target dependent information by
using the ``-gnatet=path`` switch. For example:

.. code-block:: sh

    gnat compile ./src/main.adb -gnatet=machine.tdi

On a typical 64-bit PC, the output file :file:`machine.tdi` will
contain the following information:

.. code-block:: none

    Bits_BE                       0
    Bits_Per_Unit                 8
    Bits_Per_Word                64
    Bytes_BE                      0
    Char_Size                     8
    Double_Float_Alignment        0
    Double_Scalar_Alignment       0
    Double_Size                  64
    Float_Size                   32
    Float_Words_BE                0
    Int_Size                     32
    Long_Double_Size            128
    Long_Long_Size               64
    Long_Size                    64
    Maximum_Alignment            16
    Max_Unaligned_Field          64
    Pointer_Size                 64
    Short_Enums                   0
    Short_Size                   16
    Strict_Alignment              0
    System_Allocator_Alignment   16
    Wchar_T_Size                 32
    Words_BE                      0

    float          6  I  32  32
    double        15  I  64  64
    long double   18  I  80 128
    TF            33  I 128 128

If you have a specific predefined architecture as a target that differs from
the target we're currently working on, it's possible to have the
compilation process use this specific architecture. You do this by using
the ``-gnateT=path`` option. For example:

.. code-block:: sh

    gnat compile ./src/main.adb -gnateT=machine.tdi

In this case, the compiler will use the information from
:file:`machine.tdi` as parameters for the target machine.  Note,
however, that it will still be generating code for the target
architecture, so changing these parameters too drastically can cause
unexpected results, including the compiler being unable to compile
your code at all.

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
