Writing Ada on Embedded Systems
=================================

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Understanding the Ada Run-Time
------------------------------

.. todo::

    Complete section!

Low Level Programming
---------------------

Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~

We've seen in the previous chapters how Ada can be used to describe high level
semantics and architecture. The beauty of the language, however, is that it can
be used all the way down to the lowest levels of the development, including
embedded assembly code or bit-level data management.

One very interesting feature of the language is that, unlike C, for example,
there are no data representation constraints unless specified by the developer.
This means that the compiler is free to choose the best trade-off in terms of
representation vs. performance. Let's start with the following example:

[Ada]

.. code-block:: ada

    type R is record
       V  : Integer range 0 .. 255;
       B1 : Boolean;
       B2 : Boolean;
    end record
      with Pack;

[C]

.. code-block:: c

    struct R {
       unsigned int v:8;
       bool b1;
       bool b2;
    };


The Ada and the C++ code above both represent efforts to create an object
that's as small as possible. Controlling data size is not possible in Java, but
the language does specify the size of values for the primitive types.

Although the C++ and Ada code are equivalent in this particular example,
there's an interesting semantic difference. In C++, the number of bits required
by each field needs to be specified. Here, we're stating that :ada:`v` is only
8 bits, effectively representing values from 0 to 255. In Ada, it's the other
way around: the developer specifies the range of values required and the
compiler decides how to represent things, optimizing for speed or size. The
:ada:`Pack` aspect declared at the end of the record specifies that the
compiler should optimize for size even at the expense of decreased speed in
accessing record components.

Other representation clauses can be specified as well, along with compile-time
consistency checks between requirements in terms of available values and
specified sizes. This is particularly useful when a specific layout is
necessary; for example when interfacing with hardware, a driver, or a
communication protocol. Here's how to specify a specific data layout based on
the previous example:

.. code-block:: ada

    type R is record
       V  : Integer range 0 .. 255;
       B1 : Boolean;
       B2 : Boolean;
    end record;

    for R use record
       --  Occupy the first bit of the first byte.
       B1 at 0 range 0 .. 0;

       --  Occupy the last 7 bits of the first byte,
       --  as well as the first bit of the second byte.
       V  at 0 range 1 .. 8;

       --  Occupy the second bit of the second byte.
       B2 at 1 range 1 .. 1;
    end record;

We omit the :ada:`with Pack` directive and instead use a record representation
clause following the record declaration. The compiler is directed to spread
objects of type :ada:`R` across two bytes. The layout we're specifying here is
fairly inefficient to work with on any machine, but you can have the compiler
construct the most efficient methods for access, rather than coding your own
machine-dependent bit-level methods manually.

Embedded Assembly Code
~~~~~~~~~~~~~~~~~~~~~~

When performing low-level development, such as at the kernel or hardware driver
level, there can be times when it is necessary to implement functionality with
assembly code.

Every Ada compiler has its own conventions for embedding assembly code, based
on the hardware platform and the supported assembler(s). Our examples here will
work with GNAT and GCC on the x86 architecture.

All x86 processors since the Intel Pentium offer the ``rdtsc`` instruction,
which tells us the number of cycles since the last processor reset. It takes no
inputs and places an unsigned 64-bit value split between the ``edx`` and
``eax`` registers.

GNAT provides a subprogram called :ada:`System.Machine_Code.Asm` that can be
used for assembly code insertion. You can specify a string to pass to the
assembler as well as source-level variables to be used for input and output:

.. code-block:: ada

    with System.Machine_Code; use System.Machine_Code;
    with Interfaces;          use Interfaces;

    function Get_Processor_Cycles return Unsigned_64 is
       Low, High : Unsigned_32;
       Counter   : Unsigned_64;
    begin
       Asm ("rdtsc",
            Outputs =>
              (Unsigned_32'Asm_Output ("=a", High),
               Unsigned_32'Asm_Output ("=d", Low)),
            Volatile => True);

       Counter :=
         Unsigned_64 (High) * 2 ** 32 +
         Unsigned_64 (Low);

       return Counter;
    end Get_Processor_Cycles;

The :ada:`Unsigned_32'Asm_Output` clauses above provide associations between
machine registers and source-level variables to be updated. :ada:`=a` and
:ada:`=d` refer to the ``eax`` and ``edx`` machine registers, respectively. The
use of the :ada:`Unsigned_32` and :ada:`Unsigned_64` types from package
:ada:`Interfaces` ensures correct representation of the data. We assemble the
two 32-bit values to form a single 64-bit value.

We set the :ada:`Volatile` parameter to :ada:`True` to tell the compiler that
invoking this instruction multiple times with the same inputs can result in
different outputs. This eliminates the possibility that the compiler will
optimize multiple invocations into a single call.

With optimization turned on, the GNAT compiler is smart enough to use the
``eax`` and ``edx`` registers to implement the :ada:`High` and :ada:`Low`
variables, resulting in zero overhead for the assembly interface.

The machine code insertion interface provides many features beyond what was
shown here. More information can be found in the GNAT User's Guide, and the
GNAT Reference manual.

Interrupt Handling
------------------

.. todo::

    Complete section!

Interfacing with Devices
------------------------

.. todo::

    Complete section!

Understanding Bare-Metal Environment
------------------------------------

.. todo::

    Complete section!


Dealing with Absence of FPU with Fixed Point
--------------------------------------------

.. todo::

    Complete section!

Volatile and Atomic data
------------------------

.. todo::

    Complete section!

ARM and :program:`svd2ada`
--------------------------

.. todo::

    Complete section!
