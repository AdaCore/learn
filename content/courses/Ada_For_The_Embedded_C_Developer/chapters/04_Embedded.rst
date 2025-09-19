Writing Ada on Embedded Systems
=================================

.. include:: ../../../global.txt

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

When using a configurable run-time library, the compiler checks whether the
library supports certain features of the language. If a feature isn't
supported, the compiler will give an error message.

You can find further information about the run-time library on
`this chapter of the GNAT User's Guide Supplement for Cross Platforms <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/the_gnat_configurable_run_time_facility.html>`_

Low Level Programming
---------------------

.. _Ada_For_Embedded_C_Dev_Representation_Clauses:

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


The Ada and the C code above both represent efforts to create an object
that's as small as possible. Controlling data size is not possible in Java, but
the language does specify the size of values for the primitive types.

Although the C and Ada code are equivalent in this particular example,
there's an interesting semantic difference. In C, the number of bits required
by each field needs to be specified. Here, we're stating that :ada:`v` is only
8 bits, effectively representing values from 0 to 255. In Ada, it's the other
way around: the developer specifies the range of values required and the
compiler decides how to represent things, optimizing for speed or size. The
:ada:`Pack` aspect declared at the end of the record specifies that the
compiler should optimize for size even at the expense of decreased speed in
accessing record components. We'll see more details about the :ada:`Pack`
aspect in the sections about :ref:`bitwise operations <Ada_For_Embedded_C_Dev_Bitwise_Operations>` and
:ref:`mapping structures to bit-fields <Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields>` in
chapter 6.

Other representation clauses can be specified as well, along with compile-time
consistency checks between requirements in terms of available values and
specified sizes. This is particularly useful when a specific layout is
necessary; for example when interfacing with hardware, a driver, or a
communication protocol. Here's how to specify a specific data layout based on
the previous example:

[Ada]

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

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Assembly_Code

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

Handling interrupts is an important aspect when programming embedded devices.
Interrupts are used, for example, to indicate that a hardware or software
event has happened. Therefore, by handling interrupts, an application can react
to external events.

Ada provides built-in support for handling interrupts. We can process
interrupts by attaching a handler |mdash| which must be a protected procedure
|mdash| to it. In the declaration of the protected procedure, we use the
:ada:`Attach_Handler` aspect and indicate which interrupt we want to handle.

Let's look into a code example that *traps* the quit interrupt (``SIGQUIT``)
on Linux:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Quit_Handler
    :class: ada-norun

    with System.OS_Interface;

    package Signal_Handlers is

       protected type Quit_Handler is
          function Requested return Boolean;
       private
          Quit_Request : Boolean := False;

          --
          --  Declaration of an interrupt handler for the "quit" interrupt:
          --
          procedure Handle_Quit_Signal
            with Attach_Handler => System.OS_Interface.SIGQUIT;
       end Quit_Handler;

    end Signal_Handlers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Signal_Handlers is

       protected body Quit_Handler is

          function Requested return Boolean is
            (Quit_Request);

          procedure Handle_Quit_Signal is
          begin
             Put_Line ("Quit request detected!");
             Quit_Request := True;
          end Handle_Quit_Signal;

       end Quit_Handler;

    end Signal_Handlers;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Signal_Handlers;

    procedure Test_Quit_Handler is
       Quit : Signal_Handlers.Quit_Handler;

    begin
       while True loop
          delay 1.0;
          exit when Quit.Requested;
       end loop;

       Put_Line ("Exiting application...");
    end Test_Quit_Handler;

The specification of the :ada:`Signal_Handlers` package from this example
contains the declaration of :ada:`Quit_Handler`, which is a protected type.
In the private part of this protected type, we declare the
:ada:`Handle_Quit_Signal` procedure. By using the :ada:`Attach_Handler`
aspect in the declaration of :ada:`Handle_Quit_Signal` and indicating the
quit interrupt (:ada:`System.OS_Interface.SIGQUIT`), we're instructing the
operating system to call this procedure for any quit request. So when the user
presses ``CTRL+\`` on their keyboard, for example, the application will behave
as follows:

- the operating system calls the :ada:`Handle_Quit_Signal` procedure , which
  displays a message to the user ("Quit request detected!") and sets a Boolean
  variable |mdash| :ada:`Quit_Request`, which is declared in the
  :ada:`Quit_Handler` type;

- the main application checks the status of the quit handler by calling the
  :ada:`Requested` function as part of the :ada:`while True` loop;

    - This call is in the :ada:`exit when Quit.Requested` line.

    - The :ada:`Requested` function returns :ada:`True` in this case because
      the :ada:`Quit_Request` flag was set by the :ada:`Handle_Quit_Signal`
      procedure.

- the main applications exits the loop, displays a message and finishes.

Note that the code example above isn't portable because it makes use of
interrupts from the Linux operating system. When programming embedded devices,
we would use instead the interrupts available on those specific devices.

Also note that, in the example above, we're declaring a static handler at
compilation time. If you need to make use of dynamic handlers, which can be
configured at runtime, you can use the subprograms from the
:ada:`Ada.Interrupts` package. This package includes not only a version of
:ada:`Attach_Handler` as a procedure, but also other procedures such as:

- :ada:`Exchange_Handler`, which lets us exchange, at runtime, the current
  handler associated with a specific interrupt by a different handler;

- :ada:`Detach_Handler`, which we can use to remove the handler currently
  associated with a given interrupt.

Details about the :ada:`Ada.Interrupts` package are out of scope for this
course. We'll discuss them in a separate, more advanced course in the future.
You can find some information about it in the
:aarm:`Interrupts appendix of the Ada Reference Manual <C-3-2>`.

.. todo::

    Once available, add link to section from a more advanced embedded course
    that explains the :ada:`Ada.Interrupts` package.

Dealing with Absence of FPU with Fixed Point
--------------------------------------------

Many numerical applications typically use floating-point types to compute
values. However, in some platforms, a floating-point unit may not be available.
Other platforms may have a floating-point unit, but using it in certain
numerical algorithms can be prohibitive in terms of performance. For those
cases, fixed-point arithmetic can be a good alternative.

The difference between fixed-point and floating-point types might not be so
obvious when looking at this code snippet:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Fixed_Point

    package Fixed_Definitions is

       D : constant := 2.0 ** (-31);

       type Fixed is delta D range -1.0 .. 1.0 - D;

    end Fixed_Definitions;

    with Ada.Text_IO;       use Ada.Text_IO;

    with Fixed_Definitions; use Fixed_Definitions;

    procedure Show_Float_And_Fixed_Point is
       Float_Value : Float := 0.25;
       Fixed_Value : Fixed := 0.25;
    begin

       Float_Value := Float_Value + 0.25;
       Fixed_Value := Fixed_Value + 0.25;

       Put_Line ("Float_Value = " & Float'Image (Float_Value));
       Put_Line ("Fixed_Value = " & Fixed'Image (Fixed_Value));
    end Show_Float_And_Fixed_Point;

In this example, the application will show the value 0.5 for both
:ada:`Float_Value` and :ada:`Fixed_Value`.

The major difference between floating-point and fixed-point types is in the way
the values are stored. Values of ordinary fixed-point types are, in effect,
scaled integers. The scaling used for ordinary fixed-point types is defined by
the type's :ada:`small`, which is derived from the specified :ada:`delta` and,
by default, is a power of two. Therefore, ordinary fixed-point types are
sometimes called binary fixed-point types. In that sense, ordinary fixed-point
types can be thought of being close to the actual representation on the
machine. In fact, ordinary fixed-point types make use of the available integer
shift instructions, for example.

Another difference between floating-point and fixed-point types is that Ada
doesn't provide standard fixed-point types |mdash| except for the
:ada:`Duration` type, which is used to represent an interval of time in
seconds. While the Ada standard specifies floating-point types such as
:ada:`Float` and :ada:`Long_Float`, we have to declare our own fixed-point
types. Note that, in the previous example, we have used a fixed-point type
named :ada:`Fixed`: this type isn't part of the standard, but must be declared
somewhere in the source-code of our application.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type_name> is delta <delta_value> range <lower_bound> .. <upper_bound>;

By default, the compiler will choose a scale factor, or :ada:`small`, that is a
power of 2 no greater than ``<delta_value>``.

For example, we may define a normalized range between -1.0 and 1.0 as
following:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Normalized_Fixed_Point_Type

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
bound is close to one, but not exactly one. This is a typical effect of
fixed-point data types |mdash| you can find more details in this discussion
about the :wikipedia:`Q format <Q_(number_format)>`.
We may also rewrite this code with an exact type definition:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Normalized_Adapted_Fixed_Point_Type

    package Normalized_Adapted_Fixed_Point_Type is

       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);

    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type Inv_Trig is delta 2.0 ** (-15) * Pi range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("Inv_Trig requires " & Integer'Image (Inv_Trig'Size)
                 & " bits");
       Put_Line ("The delta    value of Inv_Trig is "
                 & Inv_Trig'Image (Inv_Trig'Delta));
       Put_Line ("The minimum  value of Inv_Trig is "
                 & Inv_Trig'Image (Inv_Trig'First));
       Put_Line ("The maximum  value of Inv_Trig is "
                 & Inv_Trig'Image (Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called :ada:`Inv_Trig`,
which has a range from -π/2 to π/2.

All standard operations are available for fixed-point types. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Fixed_Point_Op

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

In the case of C, since the language doesn't support fixed-point arithmetic, we
need to emulate it using integer types and custom operations via functions.
Let's look at this very rudimentary example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Fixed_Point_C

    !main.c
    #include <stdio.h>
    #include <math.h>

    #define SHIFT_FACTOR  32

    #define TO_FIXED(x)     ((int)   ((x) * pow (2.0, SHIFT_FACTOR - 1)))
    #define TO_FLOAT(x)     ((float) ((double)(x) * (double)pow (2.0, -(SHIFT_FACTOR - 1))))

    typedef int fixed;

    fixed add (fixed a, fixed b)
    {
        return a + b;
    }

    fixed mult (fixed a, fixed b)
    {
        return (fixed)(((long)a * (long)b) >> (SHIFT_FACTOR - 1));
    }

    void display_fixed (fixed x)
    {
        printf("value (integer) = %d\n",    x);
        printf("value (float)   = %3.5f\n\n", TO_FLOAT (x));
    }

    int main(int argc, const char * argv[])
    {
        int fixed_value = TO_FIXED(0.25);

        printf("Original value\n");
        display_fixed(fixed_value);

        printf("... + 0.25\n");
        fixed_value = add(fixed_value, TO_FIXED(0.25));
        display_fixed(fixed_value);

        printf("... * 0.5\n");
        fixed_value = mult(fixed_value, TO_FIXED(0.5));
        display_fixed(fixed_value);

        return 0;
    }

Here, we declare the fixed-point type :c:`fixed` based on :c:`int` and two
operations for it: addition (via the :c:`add` function) and multiplication
(via the :c:`mult` function). Note that, while fixed-point addition is quite
straightforward, multiplication requires right-shifting to match the correct
internal representation. In Ada, since fixed-point operations are part of the
language specification, they don't need to be emulated. Therefore, no extra
effort is required from the programmer.

Also note that the example above is very rudimentary, so it doesn't take some
of the side-effects of fixed-point arithmetic into account. In C, you have to
manually take all side-effects deriving from fixed-point arithmetic into
account, while in Ada, the compiler takes care of selecting the right
operations for you.

.. _Ada_For_Embedded_C_Dev_Volatile_Atomic_Data:

Volatile and Atomic data
------------------------

Ada has built-in support for handling both volatile and atomic data. Let's
start by discussing volatile objects.

Volatile
~~~~~~~~

A :wikipedia:`volatile <Volatile_(computer_programming)>`
object can be described as an object in memory whose value may change between
two consecutive memory accesses of a process A |mdash| even if process A itself
hasn't changed the value. This situation may arise when an object in memory is
being shared by multiple threads. For example, a thread *B* may modify the
value of that object between two read accesses of a thread *A*. Another typical
example is the one of
:wikipedia:`memory-mapped I/O <Memory-mapped_I/O>`, where
the hardware might be constantly changing the value of an object in memory.

Because the value of a volatile object may be constantly changing, a compiler
cannot generate code that stores the value of that object into a register and
use the value from the register in subsequent operations. Storing into a
register is avoided because, if the value is stored there, it would be outdated
if another process had changed the volatile object in the meantime. Instead,
the compiler generates code in such a way that the process must read the value
of the volatile object from memory for each access.

Let's look at a simple example of a volatile variable in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Volatile_Object_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        volatile double val = 0.0;
        int i;

        for (i = 0; i < 1000; i++)
        {
            val += i * 2.0;
        }
        printf ("val: %5.3f\n", val);

        return 0;
    }

In this example, :c:`val` has the modifier :c:`volatile`, which indicates that
the compiler must handle :c:`val` as a volatile object. Therefore, each read
and write access in the loop is performed by accessing the value of :c:`val` in
then memory.

This is the corresponding implementation in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Volatile_Object_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Object is
       Val : Long_Float with Volatile;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Long_Float (I);
       end loop;

       Put_Line ("Val: " & Long_Float'Image (Val));
    end Show_Volatile_Object;

In this example, :ada:`Val` has the :ada:`Volatile` aspect, which makes the
object volatile. We can also use the :ada:`Volatile` aspect in type
declarations. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Volatile_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Type is
       type Volatile_Long_Float is new Long_Float with Volatile;

       Val : Volatile_Long_Float;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Volatile_Long_Float (I);
       end loop;

       Put_Line ("Val: " & Volatile_Long_Float'Image (Val));
    end Show_Volatile_Type;

Here, we're declaring a new type :ada:`Volatile_Long_Float` based on the
:ada:`Long_Float` type and using the :ada:`Volatile` aspect. Any object of this
type is automatically volatile.

In addition to that, we can declare components of an array to be volatile. In
this case, we can use the :ada:`Volatile_Components` aspect in the array
declaration. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Volatile_Array_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Array_Components is
       Arr : array (1 .. 2) of Long_Float with Volatile_Components;
    begin
       Arr := (others => 0.0);

       for I in 0 .. 999 loop
          Arr (1) := Arr (1) +  2.0 * Long_Float (I);
          Arr (2) := Arr (2) + 10.0 * Long_Float (I);
       end loop;

       Put_Line ("Arr (1): " & Long_Float'Image (Arr (1)));
       Put_Line ("Arr (2): " & Long_Float'Image (Arr (2)));
    end Show_Volatile_Array_Components;

Note that it's possible to use the :ada:`Volatile` aspect for the array
declaration as well:

[Ada]

.. code-block:: ada

    Arr : array (1 .. 2) of Long_Float with Volatile;

Atomic
~~~~~~

An atomic object is an object that only accepts atomic reads and updates. The
Ada standard specifies that "for an atomic object (including an atomic
component), all reads and updates of the object as a whole are indivisible."
In this case, the compiler must generate Assembly code in such a way that reads
and updates of an atomic object must be done in a single instruction, so that
no other instruction could execute on that same object before the read or
update completes.

.. admonition:: In other contexts

    Generally, we can say that operations are said to be atomic when they can
    be completed without interruptions. This is an important requirement when
    we're performing operations on objects in memory that are shared between
    multiple processes.

    This definition of atomicity above is used, for example, when implementing
    databases. However, for this section, we're using the term "atomic"
    differently. Here, it really means that reads and updates must be performed
    with a single Assembly instruction.

    For example, if we have a 32-bit object composed of four 8-bit bytes, the
    compiler cannot generate code to read or update the object using four 8-bit
    store / load instructions, or even two 16-bit store / load instructions.
    In this case, in order to maintain atomicity, the compiler must generate
    code using one 32-bit store / load instruction.

    Because of this strict definition, we might have objects for which the
    :ada:`Atomic` aspect cannot be specified. Lots of machines support integer
    types that are larger than the native word-sized integer. For example, a
    16-bit machine probably supports both 16-bit and 32-bit integers, but only
    16-bit integer objects can be marked as atomic |mdash| or, more generally,
    only objects that fit into at most 16 bits.

Atomicity may be important, for example, when dealing with shared hardware
registers. In fact, for certain architectures, the hardware may require that
memory-mapped registers are handled atomically. In Ada, we can use the
:ada:`Atomic` aspect to indicate that an object is atomic. This is how we can
use the aspect to declare a shared hardware register:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Atomic_Object

    with System;

    procedure Show_Shared_HW_Register is
       R   : Integer
         with Atomic, Address => System'To_Address (16#FFFF00A0#);
    begin
       null;
    end Show_Shared_HW_Register;

Note that the :ada:`Address` aspect allows for assigning a variable to a
specific location in the memory. In this example, we're using this aspect to
specify the address of the memory-mapped register. We'll discuss more about the
:ada:`Address` aspect later in the section about
:ref:`mapping structures to bit-fields <Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields>` (in
chapter 6).

In addition to atomic objects, we can declare atomic types and atomic array
components |mdash| similarly to what we've seen before for volatile objects.
For example:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Atomic_Types_Arrays

    with System;

    procedure Show_Shared_HW_Register is
       type Atomic_Integer is new Integer with Atomic;

       R : Atomic_Integer with Address => System'To_Address (16#FFFF00A0#);

       Arr : array (1 .. 2) of Integer with Atomic_Components;
    begin
       null;
    end Show_Shared_HW_Register;

In this example, we're declaring the :ada:`Atomic_Integer` type, which is an
atomic type. Objects of this type |mdash| such as :ada:`R` in this example
|mdash| are automatically atomic. This example also includes the declaration
of the :ada:`Arr` array, which has atomic components.

.. _Ada_For_Embedded_C_Dev_Interfacing_With_Devices:

Interfacing with Devices
------------------------

Previously, we've seen that we can use
:ref:`representation clauses <Ada_For_Embedded_C_Dev_Representation_Clauses>` to specify a particular
layout for a record type. As mentioned before, this is useful when interfacing
with hardware, drivers, or communication protocols. In this section, we'll
extend this concept for two specific use-cases: register overlays and data
streams. Before we discuss those use-cases, though, we'll first explain the
:ada:`Size` aspect and the :ada:`Size` attribute.

.. _Ada_For_Embedded_C_Dev_Size_Aspect_Attribute:

:ada:`Size` aspect and attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Size` aspect indicates the minimum number of bits required to
represent an object. When applied to a type, the :ada:`Size` aspect is telling
the compiler to not make record or array components of a type :ada:`T` any
smaller than :ada:`X` bits. Therefore, a common usage for this aspect is to
just confirm expectations: developers specify :ada:`'Size` to tell the compiler
that :ada:`T` should fit :ada:`X` bits, and the compiler will tell them if they
are right (or wrong).

When the specified size value is larger than necessary, it can cause objects to
be bigger in memory than they would be otherwise. For example, for some
enumeration types, we could say :ada:`for type Enum'Size use 32;` when the
number of literals would otherwise have required only a byte. That's useful for
unchecked conversions because the sizes of the two types need to be the same.
Likewise, it's useful for interfacing with C, where :c:`enum` types are just
mapped to the :ada:`int` type, and thus larger than Ada might otherwise
require. We'll discuss unchecked conversions
:ref:`later in the course <Ada_For_Embedded_C_Dev_Overlays_Vs_Unchecked_Conversions>`.

Let's look at an example from an earlier chapter:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Size_Aspect

    package My_Device_Types is

       type UInt10 is mod 2 ** 10
         with Size => 10;

    end My_Device_Types;

Here, we're saying that objects of type :ada:`UInt10` must have at least 10
bits. In this case, if the code compiles, it is a confirmation that such values
can be represented in 10 bits when packed into an enclosing record or array
type.

If the size specified was larger than what the compiler would use by default,
then it could affect the size of objects. For example, for :ada:`UInt10`,
anything up to and including 16 would make no difference on a typical machine.
However, anything over 16 would then push the compiler to use a larger object
representation. That would be important for unchecked conversions, for example.

The :ada:`Size` attribute indicates the number of bits required to represent a
type or an object. We can use the size attribute to retrieve the size of a type
or of an object:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Size_Aspect

    with Ada.Text_IO;     use Ada.Text_IO;

    with My_Device_Types; use My_Device_Types;

    procedure Show_Device_Types is
       UInt10_Obj : constant UInt10 := 0;
    begin
       Put_Line ("Size of UInt10 type:   " & Positive'Image (UInt10'Size));
       Put_Line ("Size of UInt10 object: " & Positive'Image (UInt10_Obj'Size));
    end Show_Device_Types;

Here, we're retrieving the actual sizes of the :ada:`UInt10` type and an
object of that type. Note that the sizes don't necessarily need to match. For
example, although the size of :ada:`UInt10` type is expected to be 10 bits, the
size of :ada:`UInt10_Obj` may be 16 bits, depending on the platform. Also,
components of this type within composite types (arrays, records) will probably
be 16 bits as well unless they are packed.

Register overlays
~~~~~~~~~~~~~~~~~

Register overlays make use of representation clauses to create a structure that
facilitates manipulating bits from registers. Let's look at a simplified
example of a power management controller containing registers such as a system
clock enable register. Note that this example is based on an actual
architecture:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.PMC_Peripheral

    with System;

    package Registers is

       type Bit    is mod 2 ** 1
         with Size => 1;
       type UInt5  is mod 2 ** 5
         with Size => 5;
       type UInt10 is mod 2 ** 10
         with Size => 10;

       subtype USB_Clock_Enable is Bit;

       --  System Clock Enable Register
       type PMC_SCER_Register is record
          --  Reserved bits
          Reserved_0_4   : UInt5            := 16#0#;
          --  Write-only. Enable USB FS Clock
          USBCLK         : USB_Clock_Enable := 16#0#;
          --  Reserved bits
          Reserved_6_15  : UInt10           := 16#0#;
       end record
         with
           Volatile,
           Size      => 16,
           Bit_Order => System.Low_Order_First;

       for PMC_SCER_Register use record
          Reserved_0_4   at 0 range 0 .. 4;
          USBCLK         at 0 range 5 .. 5;
          Reserved_6_15  at 0 range 6 .. 15;
       end record;

       --  Power Management Controller
       type PMC_Peripheral is record
          --  System Clock Enable Register
          PMC_SCER       : aliased PMC_SCER_Register;
          --  System Clock Disable Register
          PMC_SCDR       : aliased PMC_SCER_Register;
       end record
         with Volatile;

       for PMC_Peripheral use record
          --  16-bit register at byte 0
          PMC_SCER       at 16#0# range 0 .. 15;
          --  16-bit register at byte 2
          PMC_SCDR       at 16#2# range 0 .. 15;
       end record;

       --  Power Management Controller
       PMC_Periph : aliased PMC_Peripheral
         with Import, Address => System'To_Address (16#400E0600#);

    end Registers;

First, we declare the system clock enable register |mdash| this is
:ada:`PMC_SCER_Register` type in the code example. Most of the bits in that
register are reserved. However, we're interested in bit #5, which is used to
activate or deactivate the system clock. To achieve a correct representation of
this bit, we do the following:

- We declare the :ada:`USBCLK` component of this record using the
  :ada:`USB_Clock_Enable` type, which has a size of one bit; and

- we use a representation clause to indicate that the :ada:`USBCLK` component
  is specifically at bit #5 of byte #0.

After declaring the system clock enable register and specifying its individual
bits as components of a record type, we declare the power management controller
type |mdash| :ada:`PMC_Peripheral` record type in the code example. Here, we
declare two 16-bit registers as record components of :ada:`PMC_Peripheral`.
These registers are used to enable or disable the system clock. The strategy
we use in the declaration is similar to the one we've just seen above:

- We declare these registers as components of the :ada:`PMC_Peripheral` record
  type;

- we use a representation clause to specify that the :ada:`PMC_SCER` register
  is at byte #0 and the :ada:`PMC_SCDR` register is at byte #2.

  - Since these registers have 16 bits, we use a range of bits from 0 to 15.

The actual power management controller becomes accessible by the declaration of
the :ada:`PMC_Periph` object of :ada:`PMC_Peripheral` type. Here, we specify
the actual address of the memory-mapped registers (`400E0600` in hexadecimal)
using the :ada:`Address` aspect in the declaration. When we use the
:ada:`Address` aspect in an object declaration, we're indicating the address in
memory of that object.

Because we specify the address of the memory-mapped registers in the
declaration of :ada:`PMC_Periph`, this object is now an overlay for those
registers. This also means that any operation on this object corresponds to an
actual operation on the registers of the power management controller. We'll
discuss more details about overlays in the section about
:ref:`mapping structures to bit-fields <Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields>` (in
chapter 6).

Finally, in a test application, we can access any bit of any register of the
power management controller with simple record component selection. For
example, we can set the :ada:`USBCLK` bit of the :ada:`PMC_SCER` register by
using :ada:`PMC_Periph.PMC_SCER.USBCLK`:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.PMC_Peripheral

    with Registers;

    procedure Enable_USB_Clock is
    begin
       Registers.PMC_Periph.PMC_SCER.USBCLK := 1;
    end Enable_USB_Clock;

This code example makes use of many aspects and keywords of the Ada language.
One of them is the :ada:`Volatile` aspect, which we've discussed in the section
about :ref:`volatile and atomic objects <Ada_For_Embedded_C_Dev_Volatile_Atomic_Data>`. Using the
:ada:`Volatile` aspect for the :ada:`PMC_SCER_Register` type ensures that
objects of this type won't be stored in a register.

In the declaration of the :ada:`PMC_SCER_Register` record type of the example,
we use the :ada:`Bit_Order` aspect to specify the bit ordering of the
record type. Here, we can select one of these options:

- :ada:`High_Order_First`: first bit of the record is the most significant bit;

- :ada:`Low_Order_First`: first bit of the record is the least significant bit.

The declarations from the :ada:`Registers` package also makes use of the
:ada:`Import`, which is sometimes necessary when creating overlays. When used
in the context of object declarations, it avoids default initialization (for
data types that have it.). Aspect :ada:`Import` will be discussed in the
section that explains how to
:ref:`map structures to bit-fields <Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields>` in
chapter 6. Please refer to that chapter for more details.

.. admonition:: Details about :ada:`'Size`

    In the example above, we're using the :ada:`Size` aspect in the declaration
    of the :ada:`PMC_SCER_Register` type. In this case, the effect is that it
    has the compiler confirm that the record type will fit into the expected
    16 bits.

    That's what the aspect does for type :ada:`PMC_SCER_Register` in the
    example above, as well as for the types :ada:`Bit`, :ada:`UInt5` and
    :ada:`UInt10`. For example, we may declare a stand-alone object of type
    :ada:`Bit`:

    .. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Bit_Declaration

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Bit_Declaration is

           type Bit    is mod 2 ** 1
             with Size => 1;

           B : constant Bit := 0;
           --  ^ Although Bit'Size is 1, B'Size is almost certainly 8
        begin
           Put_Line ("Bit'Size = " & Positive'Image (Bit'Size));
           Put_Line ("B'Size   = " & Positive'Image (B'Size));
        end Show_Bit_Declaration;

    In this case, :ada:`B` is almost certainly going to be 8-bits wide on a
    typical machine, even though the language requires that :ada:`Bit'Size` is
    1 by default.

In the declaration of the components of the :ada:`PMC_Peripheral` record type,
we use the :ada:`aliased` keyword to specify that those record components are
accessible via other paths besides the component name. Therefore, the compiler
won't store them in registers. This makes sense because we want to ensure
that we're accessing specific memory-mapped registers, and not registers
assigned  by the compiler. Note that, for the same reason, we also use the
:ada:`aliased` keyword in the declaration of the :ada:`PMC_Periph` object.

Data streams
~~~~~~~~~~~~

Creating data streams |mdash| in the context of interfacing with devices
|mdash| means the serialization of arbitrary information and its transmission
over a communication channel. For example, we might want to transmit the
content of memory-mapped registers as byte streams using a serial port. To do
this, we first need to get a serialized representation of those registers as an
array of bytes, which we can then transmit over the serial port.

Serialization of arbitrary record types |mdash| including register overlays
|mdash| can be achieved by declaring an array of bytes as an overlay. By doing
this, we're basically interpreting the information from those record types as
bytes while ignoring their actual structure |mdash| i.e. their components and
representation clause. We'll discuss details about overlays in the section
about
:ref:`mapping structures to bit-fields <Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields>` (in
chapter 6).

Let's look at a simple example of serialization of an arbitrary record type:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Data_Stream_Declaration

    package Arbitrary_Types is

       type Arbitrary_Record is record
          A : Integer;
          B : Integer;
          C : Integer;
       end record;

    end Arbitrary_Types;

    with Arbitrary_Types;

    procedure Serialize_Data (Some_Object : Arbitrary_Types.Arbitrary_Record);

    with Arbitrary_Types;

    procedure Serialize_Data (Some_Object : Arbitrary_Types.Arbitrary_Record) is
       type UByte is new Natural range 0 .. 255
         with Size => 8;

       type UByte_Array is array (Positive range <>) of UByte;

       --
       --  We can access the serialized data in Raw_TX, which is our overlay
       --
       Raw_TX : UByte_Array (1 .. Some_Object'Size / 8)
         with Address => Some_Object'Address;
    begin
       null;
       --
       --  Now, we could stream the data from Some_Object.
       --
       --  For example, we could send the bytes (from Raw_TX) via the
       --  serial port.
       --
    end Serialize_Data;

    with Arbitrary_Types;
    with Serialize_Data;

    procedure Data_Stream_Declaration is
       Dummy_Object : Arbitrary_Types.Arbitrary_Record;

    begin
        Serialize_Data (Dummy_Object);
    end Data_Stream_Declaration;

The most important part of this example is the implementation of the
:ada:`Serialize_Data` procedure, where we declare :ada:`Raw_TX` as an overlay
for our arbitrary object (:ada:`Some_Object` of :ada:`Arbitrary_Record` type).
In simple terms, by writing :ada:`with Address => Some_Object'Address;` in the
declaration of :ada:`Raw_TX`, we're specifying that :ada:`Raw_TX` and
:ada:`Some_Object` have the same address in memory. Here, we are:

- taking the address of :ada:`Some_Object` |mdash| using the :ada:`Address`
  attribute |mdash|, and then

- using it as the address of :ada:`Raw_TX` |mdash| which is specified with
  the :ada:`Address` aspect.

By doing this, we're essentially saying that both :ada:`Raw_TX` and
:ada:`Some_Object` are different representations of the same object in memory.

Because the :ada:`Raw_TX` overlay is completely agnostic about the actual
structure of the  record type, the :ada:`Arbitrary_Record` type could really be
anything. By declaring :ada:`Raw_TX`, we create an array of bytes that we can
use to stream the information from :ada:`Some_Object`.

We can use this approach and create a data stream for the register overlay
example that we've seen before. This is the corresponding implementation:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Embedded.Data_Stream

    with System;

    package Registers is

       type Bit is mod 2 ** 1
         with Size => 1;
       type UInt5 is mod 2 ** 5
         with Size => 5;
       type UInt10 is mod 2 ** 10
         with Size => 10;

       subtype USB_Clock_Enable is Bit;

       --  System Clock Register
       type PMC_SCER_Register is record
          --  Reserved bits
          Reserved_0_4   : UInt5 := 16#0#;
          --  Write-only. Enable USB FS Clock
          USBCLK         : USB_Clock_Enable := 16#0#;
          --  Reserved bits
          Reserved_6_15  : UInt10 := 16#0#;
       end record
         with
           Volatile,
           Size      => 16,
           Bit_Order => System.Low_Order_First;

       for PMC_SCER_Register use record
          Reserved_0_4   at 0 range 0 .. 4;
          USBCLK         at 0 range 5 .. 5;
          Reserved_6_15  at 0 range 6 .. 15;
       end record;

       --  Power Management Controller
       type PMC_Peripheral is record
          --  System Clock Enable Register
          PMC_SCER       : aliased PMC_SCER_Register;
          --  System Clock Disable Register
          PMC_SCDR       : aliased PMC_SCER_Register;
       end record
         with Volatile;

       for PMC_Peripheral use record
          --  16-bit register at byte 0
          PMC_SCER       at 16#0# range 0 .. 15;
          --  16-bit register at byte 2
          PMC_SCDR       at 16#2# range 0 .. 15;
       end record;

       --  Power Management Controller
       PMC_Periph : aliased PMC_Peripheral;
    --     with Import, Address => System'To_Address (16#400E0600#);

    end Registers;

    package Serial_Ports is

       type UByte is new Natural range 0 .. 255
         with Size => 8;

       type UByte_Array is array (Positive range <>) of UByte;

       type Serial_Port is null record;

       procedure Read (Port : in out Serial_Port;
                       Data :    out UByte_Array);

       procedure Write (Port : in out Serial_Port;
                        Data :        UByte_Array);

    end Serial_Ports;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Serial_Ports is

       procedure Display (Data : UByte_Array) is
       begin
          Put_Line ("---- Data ----");
          for E of Data loop
             Put_Line (UByte'Image (E));
          end loop;
          Put_Line ("--------------");
       end Display;

       procedure Read (Port : in out Serial_Port;
                       Data :    out UByte_Array) is
          pragma Unreferenced (Port);
       begin
          Put_Line ("Reading data...");
          Data := (0, 0, 32, 0);
       end Read;

       procedure Write (Port : in out Serial_Port;
                        Data :        UByte_Array) is
          pragma Unreferenced (Port);
       begin
          Put_Line ("Writing data...");
          Display (Data);
       end Write;

    end Serial_Ports;

    with Serial_Ports; use Serial_Ports;
    with Registers;    use Registers;

    package Data_Stream is

       procedure Send (Port : in out Serial_Port;
                       PMC  :        PMC_Peripheral);

       procedure Receive (Port : in out Serial_Port;
                          PMC  :    out PMC_Peripheral);

    end Data_Stream;

    package body Data_Stream is

       procedure Send (Port : in out Serial_Port;
                       PMC  :        PMC_Peripheral)
       is
          Raw_TX : UByte_Array (1 .. PMC'Size / 8)
            with Address => PMC'Address;
       begin
          Write (Port => Port,
                 Data => Raw_TX);
       end Send;

       procedure Receive (Port : in out Serial_Port;
                          PMC  :    out PMC_Peripheral)
       is
          Raw_TX : UByte_Array (1 .. PMC'Size / 8)
            with Address => PMC'Address;
       begin
          Read (Port => Port,
                Data => Raw_TX);
       end Receive;

    end Data_Stream;

    with Ada.Text_IO;

    with Registers;
    with Data_Stream;
    with Serial_Ports;

    procedure Test_Data_Stream is

       procedure Display_Registers is
          use Ada.Text_IO;
       begin
          Put_Line ("---- Registers ----");
          Put_Line ("PMC_SCER.USBCLK: "
                    & Registers.PMC_Periph.PMC_SCER.USBCLK'Image);
          Put_Line ("PMC_SCDR.USBCLK: "
                    & Registers.PMC_Periph.PMC_SCDR.USBCLK'Image);
          Put_Line ("-------------- ----");
       end Display_Registers;

       Port : Serial_Ports.Serial_Port;
    begin
       Registers.PMC_Periph.PMC_SCER.USBCLK := 1;
       Registers.PMC_Periph.PMC_SCDR.USBCLK := 1;

       Display_Registers;

       Data_Stream.Send (Port => Port,
                         PMC  => Registers.PMC_Periph);

       Data_Stream.Receive (Port => Port,
                            PMC  => Registers.PMC_Periph);

       Display_Registers;
    end Test_Data_Stream;

In this example, we can find the overlay in the implementation of the
:ada:`Send` and :ada:`Receive` procedures from the :ada:`Data_Stream` package.
Because the overlay doesn't need to know the internals of the
:ada:`PMC_Peripheral` type, we're declaring it in the same way as in the
previous example (where we created an overlay for :ada:`Some_Object`). In this
case, we're creating an overlay for the :ada:`PMC` parameter.

Note that, for this section, we're not really interested in the details about
the serial port. Thus, package :ada:`Serial_Ports` in this example is just a
stub. However, because the :ada:`Serial_Port` type in that package only *sees*
arrays of bytes, after implementing an actual serial port interface for a
specific device, we could create data streams for any type.

ARM and :program:`svd2ada`
--------------------------

As we've seen in the previous section about
:ref:`interfacing with devices <Ada_For_Embedded_C_Dev_Interfacing_With_Devices>`, Ada offers powerful
features to describe low-level details about the hardware architecture without
giving up its strong typing capabilities. However, it can be cumbersome
to create a specification for all those low-level details when you have a
complex architecture. Fortunately, for ARM Cortex-M devices, the GNAT toolchain
offers an Ada binding generator called :program:`svd2ada`, which takes
CMSIS-SVD descriptions for those devices and creates Ada specifications that
match the architecture. CMSIS-SVD description files are based on the Cortex
Microcontroller Software Interface Standard (CMSIS), which is a hardware
abstraction layer for ARM Cortex microcontrollers.

Please refer to the
`svd2ada project page <https://github.com/AdaCore/svd2ada>`_ for details about
this tool.
