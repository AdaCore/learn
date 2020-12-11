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

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Quit_Handler

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
`Interrupts appendix of the Ada Reference Manual <https://www.adaic.org/resources/add_content/standards/12aarm/html/AA-C-3-2.html>`_.

.. todo::

    Once available, add link to section from a more advanced embedded course
    that explains the :ada:`Ada.Interrupts` package.

.. _Interfacing_With_Devices:

Interfacing with Devices
------------------------

.. todo::

    Complete section!

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

.. code:: ada run_button

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

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Normalized_Fixed_Point_Type

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
about the `Q format <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
We may also rewrite this code with an exact type definition:

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Normalized_Adapted_Fixed_Point_Type

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Custom_Fixed_Point_Range

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
which has a range from :math:`-\pi/2` to :math:`\pi/2`.

All standard operations are available for fixed-point types. For example:

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Fixed_Point_Op

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

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Embedded.Fixed_Point_C

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

.. _VolatileAtomicData:

Volatile and Atomic data
------------------------

Ada has built-in support for handling both volatile and atomic data. Let's
start by discussing volatile objects.

Volatile
~~~~~~~~

A `volatile <https://en.wikipedia.org/wiki/Volatile_(computer_programming)>`_
object can be described as an object in memory whose value may change between
two consecutive memory accesses of a process A |mdash| even if process A itself
hasn't changed the value. This situation may arise when an object in memory is
being shared by multiple threads. For example, a thread *B* may modify the
value of that object between two read accesses of a thread *A*. Another typical
example is the one of
`memory-mapped I/O <https://en.wikipedia.org/wiki/Memory-mapped_I/O>`_, where
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

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Embedded.Volatile_Object_C

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
    }

In this example, :c:`val` has the modifier :c:`volatile`, which indicates that
the compiler must handle :c:`val` as a volatile object. Therefore, each read
and write access in the loop is performed by accessing the value of :c:`val` in
then memory.

This is the corresponding implementation in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Embedded.Volatile_Object_Ada

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Embedded.Volatile_Type

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Embedded.Volatile_Array_Components

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

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Atomic_Object

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
:ada:`Address` aspect later in this course.

In addition to atomic objects, we can declare atomic types and atomic array
components |mdash| similarly to what we've seen before for volatile objects.
For example:

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Embedded.Atomic_Types_Arrays

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

ARM and :program:`svd2ada`
--------------------------

As we've seen in the previous section about
:ref:`interfacing with devices <Interfacing_With_Devices>`, Ada offers powerful
features to describe low-level details about the hardware architecture without
abdicating from its strong typing capabilities. However, it can be cumbersome
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
