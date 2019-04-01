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

