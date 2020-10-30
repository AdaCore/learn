Writing Ada on Embedded Systems
=================================

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

:code-config:`reset_accumulator=True`

.. include:: ../../global.txt

Understanding the Ada Run-Time
------------------------------

Ada supports a high level of abstractness and expressiveness. In some cases,
the compiler translates those constructs directly into machine code. However,
there are many high-level constructs for which a direct compilation would be
difficult. In those cases, the compiler links to a library containing an
implementation of those high-level constructs: this is the so-called run-time
library.

One typical example of high-level constructs that can be cumbersome for direct
machine code generation is Ada source-code using tasking. In this case, linking
to a low-level implementation of multithreading support |mdash| for example, an
implementation using POSIX threads |mdash| is more straightforward than trying
to make the compiler generate all the machine code.

In the case of GNAT, the run-time library is implemented using both C and Ada
source-code. Also, depending on the operating system, the library will
interface with low-level functionality from the target operating system.

There are basically two types of run-time libraries:

- the **standard** run-time library: in many cases, this is the run-time
  library available on desktop operating systems or on some embedded
  platforms (such as ARM-Linux on a Raspberry-Pi).

- the **configurable** run-time library: this is a capability that is used to
  create custom run-time libraries for specific target devices.

*Configurable* run-time libraries are usually used for constrained target
devices where support for the full library would be difficult or even
impossible. In this case, configurable run-time libraries may support just a
subset of the full Ada language. There are many reasons that speak for this
approach:

- Some aspects of the Ada language may not translate well to limited operating
  systems.

- Memory constraints may require reducing the size of the run-time library, so
  that developers may need to replace or even remove parts of the library.

- When certification is required, those parts of the library that would require
  too much certification effort can be removed.

When using configurable run-time library, the compiler checks whether the
library supports certain features of the language. If a feature isn't
supported, the compiler will give an error message.

You can find further information about the run-time library on
`this chapter of the GNAT User's Guide Supplement for Cross Platforms <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/the_gnat_configurable_run_time_facility.html>`_

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

Fixed-point types
~~~~~~~~~~~~~~~~~

.. TODO: add link to advanced lesson that discusses 'Delta vs. 'Small

Ordinary fixed-point types are similar to decimal fixed-point types in that the
values are, in effect, scaled integers.  The difference between them is in the
scale factor: for a decimal fixed-point type, the scaling, given explicitly by
the type's :ada:`delta`, is always a power of ten.

In contrast, for an ordinary fixed-point type, the scaling is defined by the
type's :ada:`small`, which is derived from the specified :ada:`delta` and, by
default, is a power of two. Therefore, ordinary fixed-point types are sometimes
called binary fixed-point types.

.. note::
   Ordinary fixed-point types can be thought of being closer to the actual
   representation on the machine, since hardware support for decimal
   fixed-point arithmetic is not widespread (rescalings by a power of ten),
   while ordinary fixed-point types make use of the available integer shift
   instructions.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type-name> is delta <delta-value> range <lower-bound> .. <upper-bound>;

By default the compiler will choose a scale factor, or :ada:`small`, that is a
power of 2 no greater than <delta-value>.

For example, we may define a normalized range between -1.0 and 1.0 as
following:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Normalized_Fixed_Point_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Normalized_Fixed_Point_Type is
       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("TQ31 requires " & Integer'Image (TQ31'Size) & " bits");
       Put_Line ("The delta    value of TQ31 is " & TQ31'Image (TQ31'Delta));
       Put_Line ("The minimum  value of TQ31 is " & TQ31'Image (TQ31'First));
       Put_Line ("The maximum  value of TQ31 is " & TQ31'Image (TQ31'Last));
    end Normalized_Fixed_Point_Type;

In this example, we are defining a 32-bit fixed-point data type for our
normalized range. When running the application, we notice that the upper
bound is close to one, but not exact one. This is a typical effect of
fixed-point data types |mdash| you can find more details in this discussion
about the `Q format <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
We may also rewrite this code with an exact type definition:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Normalized_Adapted_Fixed_Point_Type

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is delta 2.0 ** (-15) * Pi range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("T_Inv_Trig requires " & Integer'Image (T_Inv_Trig'Size)
                 & " bits");
       Put_Line ("The delta    value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Delta));
       Put_Line ("The minimum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'First));
       Put_Line ("The maximum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called :ada:`T_Inv_Trig`,
which has a range from :math:`-\pi/2` to :math:`\pi/2`.

All standard operations are available for fixed-point types. For example:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Fixed_Point_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Point_Op is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);

       A, B, R : TQ31;
    begin
       A := 0.25;
       B := 0.50;
       R := A + B;
       Put_Line ("R is " & TQ31'Image (R));
    end Fixed_Point_Op;

As expected, :ada:`R` contains 0.75 after the addition of :ada:`A` and :ada:`B`.

In fact the language is more general than these examples imply, since in
practice it is typical to need to multiply or divide values from different
fixed-point types, and obtain a result that may be of a third fixed-point type.
The details are outside the scope of this introductory course.

It is also worth noting, although again the details are outside the scope of
this course, that you can explicitly specify a value for an ordinary
fixed-point type's :ada:`small`.  This allows non-binary scaling, for example:

.. code-block:: ada

    type Angle is delta 1.0/3600.0 range 0.0 .. 360.0 - 1.0/3600.0;
    for Angle'Small use Angle'Delta;

.. _VolatileAtomicData:

Volatile and Atomic data
------------------------

.. todo::

    Complete section!

ARM and :program:`svd2ada`
--------------------------

.. todo::

    Complete section!
