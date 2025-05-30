Shared variable control
=======================

.. include:: ../../../global.txt

Ada has built-in support for handling both volatile and atomic data. Let's
start by discussing volatile objects.

.. admonition:: In the Ada Reference Manual

    - :arm22:`C.6 Shared Variable Control <C-6>`


.. _Adv_Ada_Shared_Variable_Control_Volatile:

Volatile
--------

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
cannot generate code to store the value of that object in a register and then
use the value from the register in subsequent operations. Storing into a
register is avoided because, if the value is stored there, it would be outdated
if another process had changed the volatile object in the meantime. Instead,
the compiler generates code in such a way that the process must read the value
of the volatile object from memory for each access.

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Volatile.Object_Ada

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Volatile.Type

    package Shared_Var_Types is

       type Volatile_Long_Float is new
         Long_Float with Volatile;

    end Shared_Var_Types;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Volatile_Type is
       Val : Volatile_Long_Float;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Volatile_Long_Float (I);
       end loop;

       Put_Line ("Val: "
                 & Volatile_Long_Float'Image (Val));
    end Show_Volatile_Type;

Here, we're declaring a new type :ada:`Volatile_Long_Float` in the
:ada:`Shared_Var_Types` package. This type is based on the :ada:`Long_Float`
type and uses the :ada:`Volatile` aspect. Any object of this type is
automatically volatile.

In addition to that, we can declare components of an array to be volatile. In
this case, we can use the :ada:`Volatile_Components` aspect in the array
declaration. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Volatile.Array_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Array_Components is
       Arr : array (1 .. 2) of Long_Float
               with Volatile_Components;
    begin
       Arr := (others => 0.0);

       for I in 0 .. 999 loop
          Arr (1) := Arr (1) +  2.0 * Long_Float (I);
          Arr (2) := Arr (2) + 10.0 * Long_Float (I);
       end loop;

       Put_Line ("Arr (1): "
                 & Long_Float'Image (Arr (1)));
       Put_Line ("Arr (2): "
                 & Long_Float'Image (Arr (2)));
    end Show_Volatile_Array_Components;

Note that it's possible to use the :ada:`Volatile` aspect for the array
declaration as well:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Volatile.Array

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Long_Float
               with Volatile;

    end Shared_Var_Types;

Note that, if the :ada:`Volatile` aspect is specified for an object, then the
:ada:`Volatile_Components` aspect is also specified automatically |mdash| if it
makes sense in the context, of course. In the example above, even though
:ada:`Volatile_Components` isn't specified in the declaration of the :ada:`Arr`
array , it's automatically set as well.


.. _Adv_Ada_Shared_Variable_Control_Independent:

Independent
-----------

When you write code to access a single object in memory, you might actually be
accessing multiple objects at once. For example, when you declare types that
make use of representation clauses |mdash| as we've seen in previous sections
|mdash|, you might be accessing multiple objects that are grouped together in
a single storage unit. For example, if you have components :ada:`A` and
:ada:`B` stored in the same storage unit, you cannot update :ada:`A` without
actually writing (the same value) to :ada:`B`. Those objects aren't
independently addressable because, in order to access one of them, we have to
actually address multiple objects at once.

When an object is independently addressable, we call it an independent object.
In this case, we make sure that, when accessing that object, we won't be
simultaneously accessing another object. As a consequence, this feature limits
the way objects can be represented in memory, as we'll see next.

To indicate that an object is independent, we use the :ada:`Independent`
aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Object

    package Shared_Var_Types is

       I : Integer with Independent;

    end Shared_Var_Types;

Similarly, we can use this aspect when declaring types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Type

    package Shared_Var_Types is

       type Independent_Boolean is new Boolean
         with Independent;

       type Flags is record
          F1 : Independent_Boolean;
          F2 : Independent_Boolean;
       end record;

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Independent_Boolean` type and using
it in the declaration of the :ada:`Flag` record type. Let's now derive the
:ada:`Flags` type and use a representation clause for the derived type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Type
    :class: ada-expect-compile-error

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. 0;
          F2 at 0 range 1 .. 1;
          --            ^  ERROR: start position of
          --                      F2 is wrong!
          --    ^          ERROR: F1 and F2 share the
          --                      same storage unit!
       end record;

    end Shared_Var_Types.Representation;

As you can see when trying to compile this example, the representation clause
that we used for :ada:`Rep_Flags` isn't following these limitations:

1. The size of each independent component must be a multiple of a storage unit.

2. The start position of each independent component must be a multiple of a
   storage unit.

For example, for architectures that have a storage unit of one byte |mdash|
such as standard desktop computers |mdash|, this means that the size and the
position of independent components must be a multiple of a byte. Let's correct
the issues in the code above by:

- setting the size of each independent component to correspond to
  :ada:`Storage_Unit` |mdash| using a range between 0 and
  :ada:`Storage_Unit - 1` |mdash|, and

- setting the start position to zero.

This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 1 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

Note that the representation that we're now using for :ada:`Rep_Flags` is most
likely the representation that the compiler would have chosen for this data
type. We could, however, have added an empty storage unit between :ada:`F1` and
:ada:`F2` |mdash| by simply writing :ada:`F2 at 2 ...`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 2 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

As long as we follow the rules for independent objects, we're still allowed to
use representation clauses that don't correspond to the one that the compiler
might select.

For arrays, we can use the :ada:`Independent_Components` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Components

    package Shared_Var_Types is

       Flags : array (1 .. 8) of Boolean
                 with Independent_Components;

    end Shared_Var_Types;

We've just seen in a previous example that some representation clauses might
not work with objects and types that have the :ada:`Independent` aspect. The
same restrictions apply when we use the :ada:`Independent_Components` aspect.
For example, this aspect prevents that array components are packed when the
:ada:`Pack` aspect is used. Let's discuss the following erroneous code example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Packed_Independent_Components
    :class: ada-expect-compile-error

    package Shared_Var_Types is

       type Flags is
         array (Positive range <>) of Boolean
           with Independent_Components, Pack;

       F : Flags (1 .. 8) with Size => 8;

    end Shared_Var_Types;

As expected, this code doesn't compile. Here, we can have either independent
components, or packed components. We cannot have both at the same time because
packed components aren't independently addressable. The compiler warns us that
the :ada:`Pack` aspect won't have any effect on independent components. When we
use the :ada:`Size` aspect in the declaration of :ada:`F`, we confirm this
limitation. If we remove the :ada:`Size` aspect, however, the code is compiled
successfully because the compiler ignores the :ada:`Pack` aspect and allocates
a larger size for :ada:`F`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Independent.Packed_Independent_Components

    package Shared_Var_Types is

       type Flags is
         array (Positive range <>) of Boolean
           with Independent_Components, Pack;

    end Shared_Var_Types;

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Flags_Size is
       F : Flags (1 .. 8);
    begin
       Put_Line ("Flags'Size:      "
                 & F'Size'Image & " bits");
       Put_Line ("Flags (1)'Size:  "
                 & F (1)'Size'Image & " bits");
       Put_Line ("# storage units: "
                 & Integer'Image
                     (F'Size /
                      System.Storage_Unit));
    end Show_Flags_Size;

As you can see in the output of the application, even though we specify the
:ada:`Pack` aspect for the :ada:`Flags` type, the compiler allocates eight
storage units, one per each component of the :ada:`F` array.


.. _Adv_Ada_Shared_Variable_Control_Atomic:

Atomic
------

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic.Object

    with System;

    package Shared_Var_Types is

    private
       R : Integer
             with Atomic,
                  Address =>
                    System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

Note that the :ada:`Address` aspect allows for assigning a variable to a
specific location in the memory. In this example, we're using this aspect to
specify the address of the memory-mapped register.

Later on, we talk again about the
:ref:`Address aspect <Adv_Ada_Address_Aspect>` and the GNAT-specific
:ref:`System'To_Address attribute <Adv_Ada_System_To_Address>`.

In addition to atomic objects, we can declare atomic types |mdash| similar to
what we've seen before for volatile types. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic.Types

    with System;

    package Shared_Var_Types is

       type Atomic_Integer is new Integer
         with Atomic;

    private
       R : Atomic_Integer
             with Address =>
                    System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Atomic_Integer` type, which is an
atomic type. Objects of this type |mdash| such as :ada:`R` in this example
|mdash| are automatically atomic.

We can also declare atomic array components:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic.Array_Components

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Integer
               with Atomic_Components;

    end Shared_Var_Types;

This example shows the declaration of the :ada:`Arr` array, which has atomic
components |mdash| the atomicity of its components is indicated by the
:ada:`Atomic_Components` aspect.

Note that if an object is atomic, it is also volatile and independent. In other
words, these type declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic.Volatile_Independent

    package Shared_Var_Types is

       type Atomic_Integer_1 is new Integer
         with Atomic;

       type Atomic_Integer_2 is new Integer
         with Atomic,
              Volatile,
              Independent;

    end Shared_Var_Types;

A simular rule applies to components of an array. When we use the
:ada:`Atomic_Components`, the following aspects are implied: :ada:`Volatile`,
:ada:`Volatile_Components` and :ada:`Independent_Components`. For example,
these array declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic.Volatile_Independent

    package Shared_Var_Types is

       Arr_1 : array (1 .. 2) of Integer
                 with Atomic_Components;

       Arr_2 : array (1 .. 2) of Integer
                 with Atomic_Components,
                      Volatile,
                      Volatile_Components,
                      Independent_Components;

    end Shared_Var_Types;


.. _Adv_Ada_Shared_Variable_Control_Full_Access_Only:

Full-access only
----------------

.. note::

   This feature was introduced in Ada 2022.

A full-access object is an object that requires that read or write operations
on this object are performed by reading or writing all bits of the object (i.e.
the *full object*) at once. Accordingly, a full-access type is a type whose
objects follow this requirement. Note that a full-access type must be
simultaneously a
:ref:`volatile type <Adv_Ada_Shared_Variable_Control_Volatile>` or an
:ref:`atomic type <Adv_Ada_Shared_Variable_Control_Atomic>`. (In other words,
if a type is neither volatile nor atomic, it cannot be a full-access type.)

.. admonition:: Important

    Just as a reminder, any atomic type is automatically also
    :ref:`volatile <Adv_Ada_Shared_Variable_Control_Volatile>` and
    :ref:`independent <Adv_Ada_Shared_Variable_Control_Independent>`.

Let's see some examples:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Full_Access_Only_Types switches=Compiler(-gnat2022);

    package Show_Full_Access_Only_Types is

       type Nonatomic_Full_Access_Type is
         new Long_Float
           with Volatile, Full_Access_Only;

       type Atomic_Full_Access_Type is
         new Long_Float
           with Atomic, Full_Access_Only;

    end Show_Full_Access_Only_Types;

Likewise, we can define nonatomic and atomic full-access objects:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Full_Access_Only_Objects switches=Compiler(-gnat2022);

    package Show_Full_Access_Only_Objects is

       Nonatomic_Full_Access_Obj : Long_Float
         with Volatile, Full_Access_Only;

       Atomic_Full_Access_Obj : Long_Float
         with Atomic, Full_Access_Only;

    end Show_Full_Access_Only_Objects;


.. admonition:: Relevant topics

    - :arm22:`9.10 Shared Variables <9-10>`
    - :arm22:`C.6 Shared Variable Control <C-6>`


.. _Adv_Ada_Shared_Variable_Control_Non_Atomic_Full_Access:

Nonatomic full-access
~~~~~~~~~~~~~~~~~~~~~

As we already know, the value of a volatile object may be constantly changing,
so the compiler generates code to read the
value of the volatile object from memory for each access. (In other words, the
value cannot be stored in a register for further processing.)

In the case of nonatomic full-access objects, the value of the object must not
only be read from memory or updated to memory every time, but those operations
must also be performed for the complete record object |mdash| not just parts of
it.

Consider the following example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Nonatomic_Full_Access_Register switches=Compiler(-gnat2022);

    with System;

    package Registers is

       type Boolean_Bit is new Boolean
         with Size => 1;

       type UInt1 is mod 2**1
         with Size => 1;

       type UInt2 is mod 2**2
         with Size => 2;

       type UInt14 is mod 2**14
         with Size => 14;

       type Window_Register is record
          --  horizontal line count
          Horizontal_Cnt : UInt14 := 16#0#;

          --  unspecified
          Reserved_14_15 : UInt2  := 16#0#;

          --  vertical line count
          Vertical_Cnt   : UInt14 := 16#0#;

          --  refresh signalling
          Refresh_Needed : Boolean_Bit := False;

          --  unspecified
          Reserved_30    : UInt1  := 16#0#;
       end record
         with Size      => 32,
              Bit_Order => System.Low_Order_First,
              Volatile,
              Full_Access_Only;

       for Window_Register use record
          Horizontal_Cnt at 0 range 0 .. 13;
          Reserved_14_15 at 0 range 14 .. 15;
          Vertical_Cnt   at 0 range 16 .. 29;
          Refresh_Needed at 0 range 30 .. 30;
          Reserved_30    at 0 range 31 .. 31;
       end record;

       procedure Show (WR : Window_Register);

    end Registers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Registers is

       procedure Show (WR : Window_Register) is
       begin
          Put_Line ("WR = (Horizontal_Cnt => "
                    & WR.Horizontal_Cnt'Image
                    & ",");
          Put_Line ("      Vertical_Cnt   => "
                    & WR.Vertical_Cnt'Image
                    & ",");
          Put_Line ("      Refresh_Needed => "
                    & WR.Refresh_Needed'Image
                    & ")");
       end Show;

    end Registers;

In this example, we have a 32-bit register (of :ada:`Window_Register` type)
that contains window information for a display:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | bits | component } |  { 0 | { { #0 .. 13 | Horizontal_Cnt } | { #14 .. #15 | Reserved_14_15 } | { #16 .. #29 | Vertical_Cnt } | { #30 .. #31 | Reserved_30_31 } } }"
            shape = "record"
        ];
   }

Let's use the :ada:`Window_Register` type from the :ada:`Registers` package in
a test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Nonatomic_Full_Access_Register switches=Compiler(-gnat2022);

    with Registers;   use Registers;

    procedure Show_Register is
       WR : Window_Register;
    begin
       --  Nonatomic full-access assignments
       WR.Horizontal_Cnt := 800;
       WR.Vertical_Cnt   := 600;
       WR.Refresh_Needed := True;

       Show (WR);
    end Show_Register;

The example contains assignments such as :ada:`WR.Horizontal_Cnt := 800` and
:ada:`WR.Vertical_Cnt:= 600`. Because :ada:`Window_Register` is a full-access
type, these assignments are performed for the complete 32-bit register, even
though we're updating just a single component of the record object.

Note that if :ada:`Window_Register` wasn't a *full-access* object, an
assignment such as :ada:`WR.Horizontal_Cnt := 800` could be performed with a
16-bit operation. In fact, this is what a compiler would most probably select
for this assignment, because that is more efficient than manipulating the
entire object. Therefore, using a *full-access* object prevents the compiler
from generating operations that could lead to unexpected results.

Whenever possible, this *full-access* assignment is performed in a single
machine operation. However, if it's not possible to generate a single machine
operation on the target machine, the compiler may generate multiple operations
for the update of the record components.

Note that we could combine these two assignments into a single one using an
aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Nonatomic_Full_Access_Register switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Registers;   use Registers;

    procedure Show_Register is
       WR : Window_Register;
    begin
       --  Nonatomic full-access assignment
       --  using an aggregate:
       WR := (Horizontal_Cnt => 800,
              Vertical_Cnt   => 600,
              Refresh_Needed => True,
              others         => <>);

       Show (WR);
    end Show_Register;

Again, this assignment is performed for the complete 32-bit register |mdash|
ideally, using a single 32-bit machine operation |mdash| by reading the value
from the memory.

Let's add another statement to the code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Nonatomic_Full_Access_Register switches=Compiler(-gnat2022);

    with Registers;   use Registers;

    procedure Show_Register is
       WR : Window_Register :=
              (Horizontal_Cnt => 800,
               Vertical_Cnt   => 600,
               Refresh_Needed => True,
               others         => <>);
    begin
       WR := (Horizontal_Cnt =>
                WR.Horizontal_Cnt * 2,
              Vertical_Cnt   =>
                Wr.Vertical_Cnt   * 2,
              others         => <>);

       Show (WR);
    end Show_Register;

In this example, we have an initialization using the same aggregate as in the
previous code example. We also have an assignment, in which we read the value
of :ada:`WR` and use it in the calculation.


.. _Adv_Ada_Shared_Variable_Control_Non_Atomic_Full_Access_Delta_Aggregates:

Delta aggregates
^^^^^^^^^^^^^^^^

If we want to just change two components, but leave the information of other
components untouched, we can use a
:ref:`delta aggregate <Adv_Ada_Delta_Aggregates>`.
(Note that we haven't discussed the topic of delta aggregates yet: we'll do
that :ref:`later on in this course <Adv_Ada_Delta_Aggregates>`. However, in
simple terms, we can use them to modify specific components of a record without
changing the remaining components of the record.)

For example, we might want to update just the vertical count and indicate that
update via the :ada:`Refresh_Needed` flag, but keep the same horizontal count:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Nonatomic_Full_Access_Register switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Registers;   use Registers;

    procedure Show_Registers is
       WR : Window_Register :=
              (Horizontal_Cnt => 800,
               Vertical_Cnt   => 600,
               others         => <>);
    begin
       --  Delta assignment
       WR := (WR with delta
                   Vertical_Cnt   => 800,
                   Refresh_Needed => True);

       Show (WR);
    end Show_Registers;

A delta assignment using an aggregate such as :ada:`(WR with delta ...)`
includes reading the value of the complete 32-bit :ada:`WR` object from memory,
changing the components specified after :ada:`with delta`, and writing the
complete 32-bit :ada:`WR` object back to memory. The reason is that we need to
retrieve the information that is supposed to remain intact |mdash| the
:ada:`Horizontal_Cnt` and the reserved components |mdash| in order to write
them back as a *full-access* operation.


Atomic full-access
~~~~~~~~~~~~~~~~~~

As we already know,
:ref:`atomic objects <Adv_Ada_Shared_Variable_Control_Atomic>` only accept
atomic reads and updates, which |mdash| as a whole |mdash| are indivisible,
i.e. they must be done in a single instruction, so that no other instruction
could execute on that same object before the read or update completes. (Again,
if an object is atomic, this implies it is also volatile.)

In the case of atomic full-access objects, the complete object must be read and
updated. Ideally, this operation corresponds to a single atomic
operation on the target machine, but it can also translate to multiple atomic
operations.

Let's adapt the previous example to illustrate this. First, we adapt the type
in the package:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Full_Access_Register switches=Compiler(-gnat2022);

    with System;

    package Registers is

       type Boolean_Bit is new Boolean
         with Size => 1;

       type UInt1 is mod 2**1
         with Size => 1;

       type UInt2 is mod 2**2
         with Size => 2;

       type UInt14 is mod 2**14
         with Size => 14;

       type Window_Register is record
          --  horizontal line count
          Horizontal_Cnt : UInt14 := 16#0#;

          --  unspecified
          Reserved_14_15 : UInt2  := 16#0#;

          --  vertical line count
          Vertical_Cnt   : UInt14 := 16#0#;

          --  refresh signalling
          Refresh_Needed : Boolean_Bit := False;

          --  unspecified
          Reserved_30    : UInt1  := 16#0#;
       end record
         with Size      => 32,
              Bit_Order => System.Low_Order_First,
              Atomic,
              Full_Access_Only;

       for Window_Register use record
          Horizontal_Cnt at 0 range 0 .. 13;
          Reserved_14_15 at 0 range 14 .. 15;
          Vertical_Cnt   at 0 range 16 .. 29;
          Refresh_Needed at 0 range 30 .. 30;
          Reserved_30    at 0 range 31 .. 31;
       end record;

       procedure Show (WR : Window_Register);

    end Registers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Registers is

       procedure Show (WR : Window_Register) is
       begin
          Put_Line ("WR = (Horizontal_Cnt => "
                    & WR.Horizontal_Cnt'Image
                    & ",");
          Put_Line ("      Vertical_Cnt   => "
                    & WR.Vertical_Cnt'Image
                    & ",");
          Put_Line ("      Refresh_Needed => "
                    & WR.Refresh_Needed'Image
                    & ")");
       end Show;

    end Registers;

We then use the package in our test application:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Full_Access_Register switches=Compiler(-gnat2022);

    with Registers;   use Registers;

    procedure Show_Register is
       WR : Window_Register :=
              (Horizontal_Cnt => 800,
               Vertical_Cnt   => 600,
               Refresh_Needed => True,
               others         => <>);
    begin
       WR := (Horizontal_Cnt =>
                WR.Horizontal_Cnt * 2,
              Vertical_Cnt   =>
                Wr.Vertical_Cnt   * 2,
              others         => <>);

       Show (WR);
    end Show_Register;

In this example, we first have an atomic initialization of :ada:`WR` using an
aggregate. Then, we have an atomic assignment to the atomic full-access object
:ada:`WR`. Because its type is an atomic full-access type, the operations are
atomic operations that always access the full object from and to memory.


Comparison: full-access and non-full-access types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An interesting exercise for the reader is to compare the Assembly code
generated for the code example above with a version of this code where the
:ada:`Window_Register` is not a full-access type.

.. admonition:: Relevant topics

    On a Linux platform, you can use `objdump` to retrieve the Assembly code
    and `diff` to see the difference between both versions of the type.
    For example:

    .. code-block:: bash

        objdump --target=elf64-x86-64 -d -S ./show_register > full_access.txt

        #  [...]

        diff --width=80 -t -y full_access.txt no_full_access.txt

By doing this kind of comparisons, you might gain more insights on the impact
of the :ada:`Full_Access_Only` aspect.

.. admonition:: For further reading...

    By running on a PC, we can compare the
    :wikipedia:`Intel Assembly <X86_instruction_listings>` code for various
    versions of the code. Let's start with the version using a nonatomic
    full-access version of :ada:`Window_Register` vs. the nonatomic
    (non-full-access) version of :ada:`Window_Register`:

    .. code-block:: ada

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Volatile,
                  Full_Access_Only;

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Volatile;

    These are the manually-adapted differences between both versions:

    .. code-block:: none

        --  Volatile, Full_Access_Only         |  --  Volatile

        procedure Show_Register is                procedure Show_Register is
            push   %rbp                               push   %rbp
            mov    %rsp,%rbp                          mov    %rsp,%rbp
            sub    $0x20,%rsp                  |      sub    $0x10,%rsp
           WR : Window_Register :=                   WR : Window_Register :=
                  (Horizontal_Cnt => 800,                   (Horizontal_Cnt => 800,
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xffffc000,%eax                   and    $0xffffc000,%eax
            or     $0x320,%eax                        or     $0x320,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xc000ffff,%eax                   and    $0xc000ffff,%eax
            or     $0x2580000,%eax                    or     $0x2580000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            or     $0x40000000,%eax                   or     $0x40000000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax             <
            mov    %eax,-0x14(%rbp)            <
            mov    -0x14(%rbp),%eax            <
            mov    %eax,-0x8(%rbp)             <
                   Vertical_Cnt   => 600,                    Vertical_Cnt   => 600,
                   Refresh_Needed => True,                   Refresh_Needed => True,
                   others         => <>);                    others         => <>);
        begin                                     begin
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
                    WR.Horizontal_Cnt * 2,                    WR.Horizontal_Cnt * 2,
            mov    -0x8(%rbp),%eax             |      mov    -0x4(%rbp),%eax
            mov    %eax,%ecx                   <
            and    $0x3fff,%cx                 |      and    $0x3fff,%ax
                                               >      add    %eax,%eax
           WR := (Horizontal_Cnt =>            <
            mov    -0xc(%rbp),%eax             <
            mov    %eax,%edx                   <
                    WR.Horizontal_Cnt * 2,     <
            lea    (%rcx,%rcx,1),%eax          <
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
            and    $0xffffc000,%edx            <
            or     %edx,%eax                   <
            mov    %eax,%edx                          mov    %eax,%edx
            mov    %edx,%eax                   |      mov    -0x8(%rbp),%eax
            mov    %eax,-0xc(%rbp)             |      and    $0xffffc000,%eax
            mov    -0xc(%rbp),%eax             |      or     %edx,%eax
                                               >      mov    %eax,-0x8(%rbp)
                                               >      mov    -0x8(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0xc(%rbp)             |      mov    %eax,-0x8(%rbp)
                  Vertical_Cnt   =>                         Vertical_Cnt   =>
                    Wr.Vertical_Cnt   * 2,                    Wr.Vertical_Cnt   * 2,
            mov    -0x8(%rbp),%eax             |      mov    -0x4(%rbp),%eax
            shr    $0x10,%eax                         shr    $0x10,%eax
            mov    %eax,%ecx                   |      and    $0x3fff,%ax
            and    $0x3fff,%cx                 |      add    %eax,%eax
           WR := (Horizontal_Cnt =>            <
            mov    -0xc(%rbp),%eax             <
            mov    %eax,%edx                   <
                    Wr.Vertical_Cnt   * 2,     <
            lea    (%rcx,%rcx,1),%eax          <
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
            shl    $0x10,%eax                         shl    $0x10,%eax
            and    $0xc000ffff,%edx            <
            or     %edx,%eax                   <
            mov    %eax,%edx                          mov    %eax,%edx
            mov    %edx,%eax                   |      mov    -0x8(%rbp),%eax
            mov    %eax,-0xc(%rbp)             |      and    $0xc000ffff,%eax
            mov    -0xc(%rbp),%eax             |      or     %edx,%eax
                                               >      mov    %eax,-0x8(%rbp)
                                               >      mov    -0x8(%rbp),%eax
            and    $0xbfffffff,%eax                   and    $0xbfffffff,%eax
            mov    %eax,-0xc(%rbp)             |      mov    %eax,-0x8(%rbp)
            mov    -0xc(%rbp),%eax             |      mov    -0x8(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0xc(%rbp)             <
            mov    -0xc(%rbp),%eax             <
            mov    %eax,-0x8(%rbp)                    mov    %eax,-0x8(%rbp)
                                               >      mov    -0x8(%rbp),%eax
                                               >      mov    %eax,-0x4(%rbp)
                  others         => <>);                    others         => <>);

    As we can see, although parts of the Assembly code are the same or look
    very similar, there are some differences between both versions. These
    differences are mostly related to the fact that we have to operate on the
    full object when reading it from memory.

    Likewise, we can compare the Assembly code for the atomic full-access
    version of :ada:`Window_Register` vs. the atomic (non-full-access) version
    of :ada:`Window_Register`:

    .. code-block:: ada

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Atomic,
                  Full_Access_Only;

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Atomic;

    These are the manually-adapted differences between both versions:

    .. code-block:: none

        --  Atomic, Full_Access_Only           |  --  Atomic

        procedure Show_Register is                procedure Show_Register is
            push   %rbp                               push   %rbp
            mov    %rsp,%rbp                          mov    %rsp,%rbp
            sub    $0x20,%rsp                  |      sub    $0x10,%rsp
           WR : Window_Register :=                   WR : Window_Register :=
                  (Horizontal_Cnt => 800,                   (Horizontal_Cnt => 800,
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xffffc000,%eax                   and    $0xffffc000,%eax
            or     $0x320,%eax                        or     $0x320,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xc000ffff,%eax                   and    $0xc000ffff,%eax
            or     $0x2580000,%eax                    or     $0x2580000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            or     $0x40000000,%eax                   or     $0x40000000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
           WR : Window_Register :=                   WR : Window_Register :=
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            mov    %eax,-0x14(%rbp)            <
            mov    -0x14(%rbp),%eax            <
            mov    %eax,-0x8(%rbp)                    mov    %eax,-0x8(%rbp)
                   Vertical_Cnt   => 600,                    Vertical_Cnt   => 600,
                   Refresh_Needed => True,                   Refresh_Needed => True,
                   others         => <>);                    others         => <>);
        begin                                     begin
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
                    WR.Horizontal_Cnt * 2,                    WR.Horizontal_Cnt * 2,
            mov    -0x8(%rbp),%eax                    mov    -0x8(%rbp),%eax
            mov    %eax,%ecx                   <
            and    $0x3fff,%cx                 |      and    $0x3fff,%ax
                                               |      add    %eax,%eax
           WR := (Horizontal_Cnt =>            <
            mov    -0xc(%rbp),%eax             <
            mov    %eax,%edx                   <
                    WR.Horizontal_Cnt * 2,     <
            lea    (%rcx,%rcx,1),%eax          <
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
                                               >      mov    %eax,%edx
                                               >      mov    -0xc(%rbp),%eax
            and    $0xffffc000,%edx            |      and    $0xffffc000,%eax
            or     %edx,%eax                          or     %edx,%eax
            mov    %eax,%edx                   <
            mov    %edx,%eax                   <
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
                  Vertical_Cnt   =>                         Vertical_Cnt   =>
                    Wr.Vertical_Cnt   * 2,                    Wr.Vertical_Cnt   * 2,
            mov    -0x8(%rbp),%eax                    mov    -0x8(%rbp),%eax
            shr    $0x10,%eax                         shr    $0x10,%eax
            mov    %eax,%ecx                   <
            and    $0x3fff,%cx                 |      and    $0x3fff,%ax
                                               >      add    %eax,%eax
           WR := (Horizontal_Cnt =>            <
            mov    -0xc(%rbp),%eax             <
            mov    %eax,%edx                   <
                    Wr.Vertical_Cnt   * 2,     <
            lea    (%rcx,%rcx,1),%eax          <
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
            shl    $0x10,%eax                         shl    $0x10,%eax
                                               >      mov    %eax,%edx
                                               >      mov    -0xc(%rbp),%eax
            and    $0xc000ffff,%edx            |      and    $0xc000ffff,%eax
            or     %edx,%eax                          or     %edx,%eax
            mov    %eax,%edx                   <
            mov    %edx,%eax                   <
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0xbfffffff,%eax                   and    $0xbfffffff,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            xchg   %eax,-0x8(%rbp)                    xchg   %eax,-0x8(%rbp)
                  others         => <>);                    others         => <>);

    Again, there are some differences between both versions, even though some
    parts of the Assembly code are the same or look very similar.

    Finally, we might want to compare the nonatomic full-access version
    vs. the atomic full-access version of the :ada:`Window_Register` type:

    .. code-block:: ada

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Volatile,
                  Full_Access_Only;

           type Window_Register is record
              --  [...]
           end record
             with Size      => 32,
                  Bit_Order => System.Low_Order_First,
                  Atomic,
                  Full_Access_Only;

    These are the differences between both versions:

    .. code-block:: none

        --  Volatile, Full_Access_Only         |  --  Atomic, Full_Access_Only

        procedure Show_Register is                procedure Show_Register is
            push   %rbp                               push   %rbp
            mov    %rsp,%rbp                          mov    %rsp,%rbp
            sub    $0x20,%rsp                         sub    $0x20,%rsp
           WR : Window_Register :=                   WR : Window_Register :=
                  (Horizontal_Cnt => 800,                   (Horizontal_Cnt => 800,
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xffffc000,%eax                   and    $0xffffc000,%eax
            or     $0x320,%eax                        or     $0x320,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0xc000ffff,%eax                   and    $0xc000ffff,%eax
            or     $0x2580000,%eax                    or     $0x2580000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            or     $0x40000000,%eax                   or     $0x40000000,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0x4(%rbp)                    mov    %eax,-0x4(%rbp)
           WR : Window_Register :=                   WR : Window_Register :=
            mov    -0x4(%rbp),%eax                    mov    -0x4(%rbp),%eax
            mov    %eax,-0x14(%rbp)                   mov    %eax,-0x14(%rbp)
            mov    -0x14(%rbp),%eax                   mov    -0x14(%rbp),%eax
            mov    %eax,-0x8(%rbp)                    mov    %eax,-0x8(%rbp)
                   Vertical_Cnt   => 600,                    Vertical_Cnt   => 600,
                   Refresh_Needed => True,                   Refresh_Needed => True,
                   others         => <>);                    others         => <>);
        begin                                     begin
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
                    WR.Horizontal_Cnt * 2,                    WR.Horizontal_Cnt * 2,
            mov    -0x8(%rbp),%eax                    mov    -0x8(%rbp),%eax
            mov    %eax,%ecx                          mov    %eax,%ecx
            and    $0x3fff,%cx                        and    $0x3fff,%cx
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            mov    %eax,%edx                          mov    %eax,%edx
                    WR.Horizontal_Cnt * 2,                    WR.Horizontal_Cnt * 2,
            lea    (%rcx,%rcx,1),%eax                 lea    (%rcx,%rcx,1),%eax
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
            and    $0xffffc000,%edx                   and    $0xffffc000,%edx
            or     %edx,%eax                          or     %edx,%eax
            mov    %eax,%edx                          mov    %eax,%edx
            mov    %edx,%eax                          mov    %edx,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0x3f,%ah                          and    $0x3f,%ah
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
                  Vertical_Cnt   =>                         Vertical_Cnt   =>
                    Wr.Vertical_Cnt   * 2,                    Wr.Vertical_Cnt   * 2,
            mov    -0x8(%rbp),%eax                    mov    -0x8(%rbp),%eax
            shr    $0x10,%eax                         shr    $0x10,%eax
            mov    %eax,%ecx                          mov    %eax,%ecx
            and    $0x3fff,%cx                        and    $0x3fff,%cx
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            mov    %eax,%edx                          mov    %eax,%edx
                    Wr.Vertical_Cnt   * 2,                    Wr.Vertical_Cnt   * 2,
            lea    (%rcx,%rcx,1),%eax                 lea    (%rcx,%rcx,1),%eax
            and    $0x3fff,%ax                        and    $0x3fff,%ax
           WR := (Horizontal_Cnt =>                  WR := (Horizontal_Cnt =>
            movzwl %ax,%eax                           movzwl %ax,%eax
            and    $0x3fff,%eax                       and    $0x3fff,%eax
            shl    $0x10,%eax                         shl    $0x10,%eax
            and    $0xc000ffff,%edx                   and    $0xc000ffff,%edx
            or     %edx,%eax                          or     %edx,%eax
            mov    %eax,%edx                          mov    %eax,%edx
            mov    %edx,%eax                          mov    %edx,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0xbfffffff,%eax                   and    $0xbfffffff,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            and    $0x7fffffff,%eax                   and    $0x7fffffff,%eax
            mov    %eax,-0xc(%rbp)                    mov    %eax,-0xc(%rbp)
            mov    -0xc(%rbp),%eax                    mov    -0xc(%rbp),%eax
            mov    %eax,-0x8(%rbp)             |      xchg   %eax,-0x8(%rbp)
                  others         => <>);                    others         => <>);

    As we can see, the code is basically the same |mdash| except for the last
    Assembly instruction, which is a `mov` instruction in the volatile version
    and an `xchg` instruction in the atomic version |mdash| which is an atomic
    instruction on this platform.



.. _Adv_Ada_Package_System_Atomic_Operations:

Atomic operations
-----------------

.. note::

    This feature was introduced in Ada 2022.

Ada offers four packages to handle atomic operations. Those packages are
child packages of the :ada:`System.Atomic_Operations` package. We will discuss
each of those package individually in this section.

.. admonition:: Relevant topics

    - :arm22:`C.6.1 The Package System.Atomic_Operations <C-6-1>`


Atomic Exchange
~~~~~~~~~~~~~~~

The generic :ada:`System.Atomic_Operations.Exchange` package provides
operations to compare and exchange objects atomically.

:ada:`Atomic_Exchange` function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One of those operations is the :ada:`Atomic_Exchange` function, which performs
the following operations atomically:

.. code-block:: ada

    function Atomic_Exchange
      (Item  : aliased in out Atomic_Type;
       Value :                Atomic_Type)
       return Atomic_Type
    is
       Old_Item : Atomic_Type := Item;
    begin
       Item := Value;
       return Old_Item;
    end Atomic_Exchange;

.. _Adv_Ada_Package_System_Atomic_Operations_Spinlocks:

As mentioned in the :arm22:`Ada Reference Manual <C-6-2>`, we can use this
function to implement a :wikipedia:`spinlock <Spinlock>`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Exchange switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Exchange;

    package Spinlocks is

       type Lock is new Boolean with Atomic;

       package Lock_Exchange is new
         System.Atomic_Operations.Exchange (Lock);

    end Spinlocks;

    with Spinlocks;
    use Spinlocks;
    use Spinlocks.Lock_Exchange;

    procedure Show_Locks is
       L : aliased Lock := False;
    begin
       --  Get the lock
       while Atomic_Exchange (Item  => L,
                              Value => True) loop
          null;
       end loop;

       --  At this point, we got the lock.
       --  Do some stuff here...

       --  Release the lock.
       L := False;
    end Show_Locks;

In this example, we call the :ada:`Atomic_Exchange` function for the :ada:`L`
lock until we get it. Then, we can use the resource that we protected via the
lock. After we finish our work, we can release the lock by setting :ada:`L` to
:ada:`False`.

Note that :ada:`System.Atomic_Operations.Exchange` is a generic package, so we
have to instantiate it for a specific atomic type |mdash| in this case, the
atomic Boolean :ada:`Lock` type.

.. _Adv_Ada_Package_System_Atomic_Operations_Spinlocks_Task_Number:

We can use multiple tasks to illustrate a situation where using a lock is
important to ensure that no :wikipedia:`race conditions <Race_condition>`
occur:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Exchange switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Exchange;

    package Spinlocks is

       type Lock is new Boolean with Atomic;

       package Lock_Exchange is new
         System.Atomic_Operations.Exchange (Lock);

    end Spinlocks;

    with Ada.Text_IO; use Ada.Text_IO;

    with Spinlocks;
    use Spinlocks;
    use Spinlocks.Lock_Exchange;

    procedure Show_Locks is
       L          : aliased Lock := False;
       Task_Count : Integer      := 0;

       task type A_Task;

       task body A_Task is
          Task_Number : Integer;
       begin
          --  Get the lock
          while Atomic_Exchange (Item  => L,
                                 Value => True) loop
             null;
          end loop;

          --  At this point, we got the lock.
          Task_Count  := Task_Count + 1;
          Task_Number := Task_Count;

          --  Release the lock.
          L := False;

          Put_Line ("Task_Number: "
                    & Task_Number'Image);

       end A_Task;

       A, B, C, D, E, F : A_Task;
    begin
       null;
    end Show_Locks;

In this example, we create multiple tasks (:ada:`A`, :ada:`B`, :ada:`C`,
:ada:`D`, :ada:`E`, :ada:`F`) and initialize the :ada:`Task_Number` of each task
based on the value of the :ada:`Task_Count` variable. To avoid multiple tasks accessing
the :ada:`Task_Count` variable at the same time, we use the :ada:`L` lock, which we get
before updating the :ada:`Task_Count`.

:ada:`Atomic_Compare_And_Exchange` function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another function from the :ada:`System.Atomic_Operations.Exchange` package is
:ada:`Atomic_Compare_And_Exchange`, which performs the following operations
atomically:

.. code-block:: ada

   function Atomic_Compare_And_Exchange
     (Item    : aliased in out Atomic_Type;
      Prior   : aliased in out Atomic_Type;
      Desired :                Atomic_Type)
      return Boolean is
    begin
       if Item = Prior then
          Item := Value;
          --  The item is only updated if its
          --  value and the prior value match

          return True;
       else
          Prior := Item;
          return False;
       end if;
    end Atomic_Exchange;

This function can be used for
:wikipedia:`lazy initialization <Lazy_initialization>` of variables. For
example, consider an application with multiple tasks that make use of a certain
value that isn't initialized at its declaration, but at a later point in time
by an arbitrary task. We can use :ada:`Atomic_Compare_And_Exchange` to ensure
that we only update that value if it wasn't already initialized.

Let's start with the package specification:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Compare_And_Exchange switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Exchange;
    with Ada.Numerics.Discrete_Random;

    package Lazy_Initialization is

       subtype Lazy_Value_Total_Range is
         Integer range 99 .. 1000;

       Lazy_Value_Default_Value : constant
          := Lazy_Value_Total_Range'First;

       subtype Lazy_Value_Range is Integer
         range Lazy_Value_Default_Value + 1 ..
               Lazy_Value_Total_Range'Last;

       type Lazy_Value is new Lazy_Value_Total_Range
         with Atomic,
              Default_Value =>
                Lazy_Value_Default_Value;

       package Value_Exchange is new
         System.Atomic_Operations.Exchange
           (Lazy_Value);

       package Lazy_Value_Random is new
         Ada.Numerics.Discrete_Random
           (Lazy_Value_Range);

    end Lazy_Initialization;

In this package, we declare the :ada:`Lazy_Value` type with a default value
(specified by the :ada:`Lazy_Value_Default_Value` constant). Note that we have
two ranges here: :ada:`Lazy_Value_Total_Range` and :ada:`Lazy_Value_Range`. We
use the :ada:`Lazy_Value_Total_Range` in the declaration of the
:ada:`Lazy_Value` type: it indicates the *total range* of the type. We use the
:ada:`Lazy_Value_Range` as a constraint for the total range. This range doesn't
contain the default value (:ada:`Lazy_Value_Default_Value`), and we use it to
indicate the valid values of the type. (We discuss the application of
:ada:`Lazy_Value_Range` later on.)

Also, in addition to instantiating the :ada:`System.Atomic_Operations.Exchange`
package, we instantiate the :ada:`Ada.Numerics.Discrete_Random` package, which
we'll use to generate random numbers in the expected range
(:ada:`Lazy_Value_Range`) for the :ada:`Lazy_Value` type. (We discussed the
:ada:`Ada.Numerics.Discrete_Random` package in the
:ref:`Introduction to Ada <Intro_Ada_Random_Number_Generation>` course.)

Let's use this package in the :ada:`Show_Lazy_Initialization` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Compare_And_Exchange switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Discrete_Random;

    with Lazy_Initialization;
    use Lazy_Initialization;
    use Lazy_Initialization.Value_Exchange;

    procedure Show_Lazy_Initialization is
      subtype A_Task_Number is Natural;

       Value             : aliased Lazy_Value;
       Value_Modified_By : A_Task_Number := 0;

       task type A_Task is
          entry Start (This : A_Task_Number);
          entry Stop;
       end A_Task;

       task body A_Task is
          Task_Number : A_Task_Number;
       begin
          accept Start (This : A_Task_Number) do
             Task_Number := This;
          end Start;

          Sleep_Some_Time : declare
             subtype Sleep_Range is
               Integer range 1 .. 3;

             package Random_Sleep is new
               Ada.Numerics.Discrete_Random
                 (Sleep_Range);
             use Random_Sleep;

             G : Generator;
          begin
             Reset (G);
             delay Duration (Random (G));
          end Sleep_Some_Time;

          Generate_Value : declare
             use Lazy_Value_Random;

             G             :         Generator;
             Initial_Value :         Lazy_Value_Range;
             Prior         : aliased Lazy_Value;
          begin
             Reset (G);
             Initial_Value := Random (G);

             if Atomic_Compare_And_Exchange
               (Item    => Value,
                Prior   => Prior,
                Desired => Lazy_Value (Initial_Value))
             then
                Value_Modified_By := Task_Number;
             end if;

          end Generate_Value;

          accept Stop do
             Put_Line ("Current task number:     "
                       & Task_Number'Image);
             Put_Line ("Value:               "
                       & Value'Image);
             Put_Line ("Modified by task number: "
                       & Value_Modified_By'Image);
             Put_Line ("---------------------");
          end Stop;
       end A_Task;

       Some_Tasks : array (1 .. 5) of A_Task;
    begin
       for I in Some_Tasks'Range loop
          Some_Tasks (I).Start (I);
       end loop;
       for I in Some_Tasks'Range loop
          Some_Tasks (I).Stop;
       end loop;
    end Show_Lazy_Initialization;

In the :ada:`Show_Lazy_Initialization` procedure, the most important variable
is :ada:`Value`, which is the variable we have to protect via a lock. In
addition, we have the auxiliary :ada:`Value_Modified_By` variable, which
indicates the number of the task that initialized the :ada:`Value` variable.

In this procedure, we also see two main
:ref:`block statements <Adv_Ada_Block_Statements>`:

- the block statement with the :ada:`Sleep_Some_Time` identifier, where we make
  the task *sleep* for a random amount of time (in the :ada:`Sleep_Range`
  range); and

- the block statement with the :ada:`Generate_Value` identified, where we
  generate a new value randomly and attempt to update the :ada:`Value` variable
  (of :ada:`Lazy_Value` type).

Let's discuss some details about the :ada:`Generate_Value` block statement. We
start by declaring some variables. Here, it's important to highlight that the
:ada:`Prior` variable is initialized with the default value
(:ada:`Lazy_Value_Default_Value`). We then call the
:ada:`Atomic_Compare_And_Exchange` function, and pass :ada:`Value` and
:ada:`Prior` as actual parameters. We can have two possible outcomes:

#. If :ada:`Value` hasn't been modified by a task yet, it will contain the
   default value |mdash| which means that the values of the :ada:`Prior` and
   :ada:`Value` variables match. In this case, the call to
   :ada:`Atomic_Compare_And_Exchange` will update the :ada:`Value` variable and
   return :ada:`True`. (Note that we also update the :ada:`Value_Modified_By`
   variable when :ada:`Atomic_Compare_And_Exchange` returns :ada:`True`.)

#. If :ada:`Value` has already been modified by a task, its value doesn't match
   the (default) value of :ada:`Prior` anymore, so the call to
   :ada:`Atomic_Compare_And_Exchange` doesn't modify the :ada:`Value` variable.

As mentioned before, we use a stricter range for the random number generator:
the :ada:`Lazy_Value_Range`. Because this range doesn't contain the default
value (:ada:`Lazy_Value_Default_Value`), we will never generate a random value
that matches the default value.

.. todo::

    Mention :ada:`Is_Lock_Free` function.

.. admonition:: Relevant topics

    - :arm22:`C.6.2 The Package System.Atomic_Operations.Exchange <C-6-2>`


Atomic Test and Set
^^^^^^^^^^^^^^^^^^^

The :ada:`System.Atomic_Operations.Test_And_Set` package provides atomic
operations to set and clear atomic flags. To declare flags, we use the
:ada:`Test_And_Set_Flag` type. The following operations are available:

#. the :ada:`Atomic_Test_And_Set` function, which we call to verify whether the
   flag can be set and, if positive, set it accordingly.

    - The function returns :ada:`True` if the flag has been set, and
      :ada:`False` otherwise.

#. the :ada:`Atomic_Clear` procedure, which we call to clear the flag.

We can use these functions to implement an application similar to the
:ref:`spinlocks <Adv_Ada_Package_System_Atomic_Operations_Spinlocks>` that
we've seen before:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Test_And_Set switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Test_And_Set;
    use  System.Atomic_Operations.Test_And_Set;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Test_And_Set is
       Lock       : aliased Test_And_Set_Flag;
       Task_Count : Integer := 0;

       task type A_Task;

       task body A_Task is
          Task_Number : Integer;
       begin
          --  Get the lock
          while Atomic_Test_And_Set (Lock) loop
             null;
          end loop;

          --  At this point, we got the lock.
          Task_Count  := Task_Count + 1;
          Task_Number := Task_Count;

          --  Release the lock.
          Atomic_Clear (Lock);

          Put_Line ("Task_Number: "
                    & Task_Number'Image);

       end A_Task;

       A, B, C, D, E, F : A_Task;
    begin
       null;
    end Show_Test_And_Set;

Here, we call :ada:`Atomic_Test_And_Set` in a loop until it returns
:ada:`True`. Then, we update the :ada:`Task_Count` and :ada:`Task_Number`. When we're
finished, we call the :ada:`Atomic_Clear` procedure to release the lock.

.. todo::

    Mention :ada:`Is_Lock_Free` function.

.. admonition:: Relevant topics

    - :arm22:`C.6.3 The Package System.Atomic_Operations.Test_and_Set <C-6-3>`


Atomic Operations using Integer Arithmetic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The generic :ada:`System.Atomic_Operations.Integer_Arithmetic` package is used
to perform atomic operations on atomic integer types. It provides the following
operations: the procedures :ada:`Atomic_Add` and :ada:`Atomic_Subtract`, and
the functions :ada:`Atomic_Fetch_And_Add` and :ada:`Atomic_Fetch_And_Subtract`.
The procedures and the corresponding :ada:`Atomic_Fetch_` functions do
basically the same thing, with the difference that :ada:`Atomic_Fetch`
functions return the previous (older) value of the input item.

The :ada:`Atomic_Add` procedure performs the following operations atomically:

.. code-block:: ada

    procedure Atomic_Add
      (Item  : aliased in out Atomic_Type;
       Value :                Atomic_Type) is
    begin
       Item := Item + Value;
    end Atomic_Add;

The corresponding :ada:`Atomic_Fetch_And_Add` function performs the following
operations atomically:

.. code-block:: ada

    function Atomic_Fetch_And_Add
      (Item  : aliased in out Atomic_Type;
       Value :                Atomic_Type)
       return Atomic_Type
    is
       Old_Item : Atomic_Type := Item;
    begin
       Item := Item + Value;
       return Old_Item;
    end Atomic_Fetch_And_Add;

The :ada:`Atomic_Subtract` procedure performs the following operations
atomically:

.. code-block:: ada

    procedure Atomic_Subtract
      (Item  : aliased in out Atomic_Type;
       Value :                Atomic_Type) is
    begin
       Item := Item - Value;
    end Atomic_Subtract;

The corresponding :ada:`Atomic_Fetch_And_Subtract` function performs the
following operations atomically:

.. code-block:: ada

    function Atomic_Fetch_And_Subtract
      (Item  : aliased in out Atomic_Type;
       Value :                Atomic_Type)
       return Atomic_Type
    is
       Old_Item : Atomic_Type := Item;
    begin
       Item := Item - Value;
       return Old_Item;
    end Atomic_Fetch_And_Subtract;

.. _Adv_Ada_Package_System_Integer_Arithmetic_Task_Number:

Let's reuse a
:ref:`previous code example <Adv_Ada_Package_System_Atomic_Operations_Spinlocks_Task_Number>`
that sets a unique number for each task. In this case, instead of using locks, we
use the atomic operations from the
:ada:`System.Atomic_Operations.Integer_Arithmetic` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Integer_Arithmetic switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Integer_Arithmetic;

    package Atomic_Integers is

       type Atomic_Integer is new Integer
         with Atomic;

       package Atomic_Integer_Arithmetic is new
         System.Atomic_Operations.Integer_Arithmetic
           (Atomic_Integer);

    end Atomic_Integers;

    with Ada.Text_IO; use Ada.Text_IO;

    with Atomic_Integers;
    use  Atomic_Integers;
    use  Atomic_Integers.Atomic_Integer_Arithmetic;

    procedure Show_Atomic_Integers is
       Task_Count : aliased Atomic_Integer := 0;

       task type A_Task;

       task body A_Task is
          Task_Number : Atomic_Integer;
       begin
          Task_Number :=
            Atomic_Fetch_And_Add (Task_Count, 1);

          Put_Line ("Task_Number: "
                    & Task_Number'Image);

       end A_Task;

       A, B, C, D, E, F : A_Task;
    begin
       null;
    end Show_Atomic_Integers;

In this example, we call the :ada:`Atomic_Fetch_And_Add` function to update the
:ada:`Task_Count` variable and, at the same time, initialize the :ada:`Task_Number`
variable of the current task.


.. admonition:: Relevant topics

    - :arm22:`C.6.4 The Package System.Atomic_Operations.Integer_Arithmetic <C-6-4>`


Atomic Operations using Modular Arithmetic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The generic :ada:`System.Atomic_Operations.Modular_Arithmetic` package is very
similar to the :ada:`System.Atomic_Operations.Integer_Arithmetic` package. In
fact, it provides the same operations: the procedures :ada:`Atomic_Add` and
:ada:`Atomic_Subtract`, and the functions :ada:`Atomic_Fetch_And_Add` and
:ada:`Atomic_Fetch_And_Subtract`. The only difference is that it is used for
modular types instead of integer types.

Let's reuse the
:ref:`previous code example <Adv_Ada_Package_System_Integer_Arithmetic_Task_Number>`,
but replace the atomic integer type by an atomic modular type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Shared_Variable_Control.Atomic_Operations.Modular_Arithmetic switches=Compiler(-gnat2022);

    with System.Atomic_Operations.Modular_Arithmetic;

    package Atomic_Modulars is

       type Atomic_Modular is mod 100
         with Atomic;

       package Atomic_Modular_Arithmetic is new
         System.Atomic_Operations.Modular_Arithmetic
           (Atomic_Modular);

    end Atomic_Modulars;

    with Ada.Text_IO; use Ada.Text_IO;

    with Atomic_Modulars;
    use  Atomic_Modulars;
    use  Atomic_Modulars.Atomic_Modular_Arithmetic;

    procedure Show_Atomic_Modulars is
       Task_Count : aliased Atomic_Modular := 0;

       task type A_Task;

       task body A_Task is
          Task_Number : Atomic_Modular;
       begin
          Task_Number :=
            Atomic_Fetch_And_Add (Task_Count, 1);

          Put_Line ("Task_Number: "
                    & Task_Number'Image);

       end A_Task;

       A, B, C, D, E, F : A_Task;
    begin
       null;
    end Show_Atomic_Modulars;

As we did in the previous example, we again call the
:ada:`Atomic_Fetch_And_Add` function to update the :ada:`Task_Count` variable and, at
the same time, initialize the :ada:`Task_Number` variable of the current task. The
only difference is that we use a modular type (:ada:`Atomic_Modular`).

.. admonition:: Relevant topics

    - :arm22:`C.6.5 The Package System.Atomic_Operations.Modular_Arithmetic <C-6-5>`
