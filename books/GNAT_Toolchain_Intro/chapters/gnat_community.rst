GNAT Community
==============

.. include:: <isopub.txt>

.. include:: <isoamsa.txt>

.. role:: ada(code)
   :language: ada

This chapter presents useful compiler options from the GNAT compiler
including examples on how to use them.


Expanded source-code
--------------------

GNAT provides a command-line option for generating an expanded form of a
source-code file. This can be useful to analyze operations used in the
source-code.

Let's start with a simple example:

.. code-block:: ada

    procedure Main is
       F : Float := 0.0;
    begin
       F := F + 1.0;
    end Main;

For generating the expanded form, we run the compiler with the ``-gnatG``
option for the source-code file containing the :ada:`Main` procedure:

.. code-block:: sh

    gnat compile -gnatG main.adb > main.expanded

In this example, we redirect the output to the ``main.expanded`` file,
which contains the following expanded source-code:

.. code-block:: none

    Source recreated from tree for Main (body)
    ------------------------------------------


    procedure main is
       f : float := 0.0;
    begin
       f := f + [8388608.0*2**(-23)];
       return;
    end main;

Notice that the original floating-point value (:ada:`1.0`) is converted
to a representation close to the machine representation, which uses a
23-bit mantissa and an 8-bit exponent: ``[8388608.0*2**(-23)]``

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

When running the same command-line as before, we get the following
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

When we analyze the expanded code, we notice that the compiler selects the
overridden addition for the operation on the :ada:`A` variable of
:ada:`Int` type. In this code, the overridden addition is represented by
the  :ada:`main__Oadd` function. For operation on the :ada:`B` variable,
the standard operator (represented by ``{+}``) is used. This kind of
analysis is helpful to verify, for example, if the operation selected by
the compiler is the one we expected.


Target dependent information
----------------------------

Target dependent information refers to information about the computer
architecture that is used as the target of the compilation process. This
includes information such as size and alignment of base data types, and
endianness.

You may generate a file containing target dependent information with them
``-gnatet=path`` option. For example:

.. code-block:: sh

    gnat compile ./src/main.adb -gnatet=machine.tdi

On a typical 64-bit PC, the output file ``machine.tdi`` contains following
information:

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

In case we have a specific, predefined architecture as a target that
defers from the target we're currently working on, it's possible to drive
the compilation process to use this specific architecture. This is
achieved via the ``-gnateT=path`` option. For example:

.. code-block:: sh

    gnat compile ./src/main.adb -gnateT=machine.tdi

In this case, the information from ``machine.tdi`` is used as the target.


Compiler warnings
-----------------

One of the strengths of the GNAT compiler is the ability to generate lots
of useful warnings. Some of them are displayed by default, while some need
to be explicitly activated. In this section, we discuss some of these
warnings, their purpose and how to activate them.

``-gnatwa`` switch and warning suppression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start by recapitulating the difference between a *warning* and an
*error*. First, errors are generally violations of the Ada language rules
as specified in the Ada Reference Manual; warnings are GNAT specific.
Thus, other Ada compilers might not warn about the same things that GNAT
does. Second, warnings are typically conservative; that is, some warnings
will be false alarms, and the programmer needs to study the code to see if
the warning is a real problem.

Some warnings are given by default, whereas some are given only if a
switch enables them. Use the ``-gnatwa`` switch to turn on (almost) all
warnings.

Warnings are useless if you don't do something about them. If you give
your team-member some code that causes warnings, how are they supposed to
know if they represent real problems? Pretty soon people will ignore
warnings, and they will scatter themselves all about the code. Use the
``-gnatwae`` switch to turn on (almost) all warnings, and to treat
warnings as errors. This will force you to get a clean (no warnings or
errors) compilation.

But some warnings are false alarms. Use :ada:`pragma Warnings (Off)` to
suppress false alarms. It's best to be as specific as possible: narrow
down to a single line of code, and a single warning message. And use a
comment to explain why the warning is a false alarm, if it's not obvious.

Let's look at the following example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Warnings_Example is

       procedure Mumble (X : Integer) is
       begin
          Put_Line ("Mumble processing...");
       end Mumble;

    end Warnings_Example;

We compile the code above with ``-gnatwae``:

.. code-block:: none

    gnat compile -gnatwae ./src/warnings_example.adb

This will cause GNAT to complain:

.. code-block:: none

    warnings_example.adb:5:22: warning: formal parameter "X" is not referenced

But the following will compile cleanly:

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

Here we've suppressed the specific warning message on a specific line.

If you get many warnings of a specific type, and it's not feasible to fix
them all, then suppress that type of message, so the good warnings won't
get buried beneath a pile of bogus ones. The ``-gnatwaeF`` switch will
silence the warning on the first version of :ada:`Mumble` above: the ``F``
means suppress warnings on unreferenced formal parameters, and would be a
good idea if you have lots of those.

In summary, we suggest turning on as many warnings as makes sense for your
project. Then, whenever you see a warning message, look at the code and
decide if it's real. If so, fix the code. If it's a false alarm, suppress
the warning. Either way, make the warning disappear before checking your
code into your configuration management system.

