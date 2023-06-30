Access Types
============

.. include:: ../../../global.txt

Access types: Terminology
-------------------------

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`
    - :arm22:`3.10.2 Operations of Access Types <3-10-2>`

.. todo::

    Complete section!

.. _Adv_Ada_Access_Types_Allocation:

Access types: Allocation
-------------------------

Ada makes the distinction between pool-specific and general access types, as
we'll discuss in this section. Before doing so, however, let's talk about
memory allocation.

In general terms, memory can be allocated dynamically on the
heap or statically on the stack. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.Simple_Allocation

    procedure Show_Simple_Allocation is

       --  Declaring access type:
       type Integer_Access is access Integer;

       --  Declaring access object:
       A1 : Integer_Access;

    begin
       --  Allocating an Integer object on the heap
       A1 := new Integer;

       declare
          --  Allocating an Integer object on the
          --  stack
          I : Integer;
       begin
          null;
       end;

    end Show_Simple_Allocation;

When we allocate an object on the heap via :ada:`new`, the allocation happens
in a memory pool that is associated with the access type. In our code example,
there's a memory pool associated with the :ada:`Integer_Access` type, and each
:ada:`new Integer` allocates a new integer object in that pool. Therefore,
access types of this kind are called pool-specific access types. (We discuss
:ref:`more about these types <Adv_Ada_Pool_Specific_Access_Types>` later.)

It is also possible to access objects that were allocated on the stack. To do
that, however, we cannot use pool-specific access types because |mdash| as the
name suggests |mdash| they're only allowed to access objects that were
allocated in the specific pool associated with the type. Instead, we have to
use general access types in this case:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.General_Access_Types

    procedure Show_General_Access_Type is

       --  Declaring general access type:
       type Integer_Access is access all Integer;

       --  Declaring access object:
       A1 : Integer_Access;

       --  Allocating an Integer object on the
       --  stack:
       I : aliased Integer;

    begin
       --  Getting access to an Integer object that
       --  was allocated on the stack
       A1 := I'Access;

    end Show_General_Access_Type;

In this example, we declare the general access type :ada:`Integer_Access` and
the access object :ada:`A1`. To initialize :ada:`A1`, we write :ada:`I'Access`
to get access to an integer object :ada:`I` that was allocated on the stack.
(For the moment, don't worry much about these details: we'll talk about general
access types again when we introduce the topic of
:ref:`aliased objects <Adv_Ada_Aliased_Objects>` later on.)

.. admonition:: For further reading...

    Note that it is possible to use general access types to allocate objects on
    the heap:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.General_Access_Types_Heap

        procedure Show_Simple_Allocation is

           --  Declaring general access type:
           type Integer_Access is access all Integer;

           --  Declaring access object:
           A1 : Integer_Access;

        begin
           --
           --  Allocating an Integer object on the heap
           --  and initializing an access object of
           --  the general access type Integer_Access.
           --
           A1 := new Integer;

        end Show_Simple_Allocation;

    Here, we're using a general access type :ada:`Integer_Access`, but
    allocating an integer object on the heap.

.. admonition:: Important

    In many code examples, we have used the :ada:`Integer` type as the
    designated subtype of the access types |mdash| by writing
    :ada:`access Integer`. Although we have used this specific scalar type,
    we aren't really limited to those types. In fact, we can use *any type* as
    the designated subtype, including user-defined types, composite types,
    task types and protected types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


.. _Adv_Ada_Pool_Specific_Access_Types:

Pool-specific access types
~~~~~~~~~~~~~~~~~~~~~~~~~~

We've already discussed many aspects about pool-specific access types. In this
section, we recapitulate some of those aspects, and discuss some new details
that haven't seen yet.

As we know, we cannot directly assign an object :ada:`Distance_Miles` of type
:ada:`Miles` to an object :ada:`Distance_Meters` of type :ada:`Meters`, even if
both share a common :ada:`Float` type ancestor. The assignment is only possible
if we perform a type conversion from :ada:`Miles` to :ada:`Meters`, or
vice-versa |mdash| e.g.:
:ada:`Distance_Meters := Meters (Distance_Miles) * Miles_To_Meters_Factor`.

Similarly, in the case of pool-specific access types, a direct assignment
between objects of different access types isn't possible. However, even if
both access types have the same designated subtype (let's say, they are both
declared using :ada:`is access Integer`), it's still not possible to perform
a type conversion between those access types. The only situation when an access
type conversion is allowed is when both types have a common ancestor.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.General_Access_Types

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Allocation is

       --  Declaring pool-specific access type:
       type Integer_Access_1 is access Integer;
       type Integer_Access_2 is access Integer;
       type Integer_Access_2B is new Integer_Access_2;

       --  Declaring access object:
       A1  : Integer_Access_1;
       A2  : Integer_Access_2;
       A2B : Integer_Access_2B;

    begin
       A1 := new Integer;
       Put_Line ("A1  : " & A1'Image);
       Put_Line ("Pool: " & A1'Storage_Pool'Image);

       A2 := new Integer;
       Put_Line ("A2:   " & A2'Image);
       Put_Line ("Pool: " & A2'Storage_Pool'Image);

       --  ERROR: Cannot directly assign access values
       --         for objects of unrelated access
       --         types; also, cannot convert between
       --         these types.
       --
       --  A1 := A2;
       --  A1 := Integer_Access_1 (A2);

       A2B := Integer_Access_2B (A2);
       Put_Line ("A2B:  " & A2B'Image);
       Put_Line ("Pool: " & A2B'Storage_Pool'Image);

    end Show_Simple_Allocation;

In this example, we declare three access types: :ada:`Integer_Access_1`,
:ada:`Integer_Access_2` and :ada:`Integer_Access_2B`. Also,
the :ada:`Integer_Access_2B` type is derived from the :ada:`Integer_Access_2`
type. Therefore, we can convert an object of :ada:`Integer_Access_2` type to
the :ada:`Integer_Access_2B` type |mdash| we do this in the
:ada:`A2B := Integer_Access_2B (A2)` assignment. However, we cannot directly
assign to or convert between unrelated types such as :ada:`Integer_Access_1`
and :ada:`Integer_Access_2`. (We would get a compilation error if we included
the :ada:`A1 := A2` or the :ada:`A1 := Integer_Access_1 (A2)` assignment.)

.. admonition:: Important

    Remember that:

    - As mentioned in the
      :ref:`Introduction to Ada course <Intro_Ada_Access_Type_Allocation_Constraints>`:

        - an access type can be unconstrained, but the actual object allocation
          must be constrained;

        - we can use a
          :ref:`qualified expression <Adv_Ada_Qualified_Expressions>` to
          allocate an object.

    - We can use the :ada:`Storage_Size` attribute to limit the size of the
      memory pool associated with an access type, as discussed previously in
      the :ref:`section about storage size <Adv_Ada_Storage_Size_Attribute>`.

    - When running out of memory while allocating via :ada:`new`, we get a
      :ada:`Storage_Error` exception because of the
      :ref:`storage check <Adv_Ada_Storage_Check>`.

    For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.Array_Allocation

        pragma Ada_2022;

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Array_Allocation is

           --  Unconstrained array type:
           type Integer_Array is
             array (Positive range <>) of Integer;

           --  Access type with unconstrained
           --  designated subtype and limited storage
           --  size.
           type Integer_Array_Access is
             access Integer_Array
               with Storage_Size => 128;

           --  An access object:
           A1 : Integer_Array_Access;

           procedure Show_Info
             (IAA : Integer_Array_Access) is
           begin
              Put_Line ("Allocated: " & IAA'Image);
              Put_Line ("Length:    "
                        & IAA.all'Length'Image);
              Put_Line ("Values:    "
                        & IAA.all'Image);
           end Show_Info;

        begin
           --  Allocating an integer array with
           --  constrained range on the heap:
           A1 := new Integer_Array (1 .. 3);
           A1.all := [others => 42];
           Show_Info (A1);

           --  Allocating an integer array on the
           --  heap using a qualified expression:
           A1 := new Integer_Array'(5, 10);
           Show_Info (A1);

           --  A third allocation fails at run time
           --  because of the constrained storage
           --  size:
           A1 := new Integer_Array (1 .. 100);
           Show_Info (A1);

        exception
            when Storage_Error =>
               Put_Line ("Out of memory!");

        end Show_Array_Allocation;


Multiple allocation
~~~~~~~~~~~~~~~~~~~

Up to now, we have seen examples of allocating a single object on the heap.
It's possible to allocate multiple objects *at once* as well |mdash| i.e.
syntactic sugar is available to simplify the code that performs this
allocation. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.Integer_Access_Array

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Access_Array_Allocation is

       type Integer_Access is access Integer;

       type Integer_Access_Array is
         array (Positive range <>) of Integer_Access;

       --  An array of access objects:
       Arr : Integer_Access_Array (1 .. 10);

    begin
       --
       --  Allocating 10 access objects and
       --  initializing the corresponding designated
       --  object with zero:
       --
       Arr := (others => new Integer'(0));

       --  Same as:
       for I in Arr'Range loop
          Arr (I) := new Integer'(0);
       end loop;

       Put_Line ("Arr: " & Arr'Image);

       Put_Line ("Arr (designated values): ");
       for E of Arr loop
          Put (E.all'Image);
       end loop;

    end Show_Access_Array_Allocation;

In this example, we have the access type :ada:`Integer_Access` and an array
type of this access type (:ada:`Integer_Access_Array`). We also declare an
array :ada:`Arr` of :ada:`Integer_Access_Array` type. This means that each
component of :ada:`Arr` is an access object. We allocate all ten components of
the :ada:`Arr` array by simply writing :ada:`Arr := (others => new Integer)`.
This :ref:`array aggregate <Adv_Ada_Array_Aggregates>` is syntactic sugar for a
loop over :ada:`Arr` that allocates each component. (Note that, by writing
:ada:`Arr := (others => new Integer'(0))`, we're also initializing the
designated objects with zero.)

Let's see another code example, this time with task types:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Types_Allocation.Workers

    package Workers is

       task type Worker is
          entry Start (Id : Positive);
          entry Stop;
       end Worker;

       type Worker_Access is access Worker;

       type Worker_Array is
         array (Positive range <>) of Worker_Access;

    end Workers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Workers is

       task body Worker is
          Id : Positive;
       begin
          accept Start (Id : Positive) do
             Worker.Id := Id;
          end Start;
          Put_Line ("Started Worker #"
                    & Id'Image);

          accept Stop;

          Put_Line ("Stopped Worker #"
                    & Id'Image);
       end Worker;

    end Workers;

    with Ada.Text_IO; use Ada.Text_IO;

    with Workers; use Workers;

    procedure Show_Workers is
       Worker_Arr : Worker_Array (1 .. 50);
    begin
       --
       --  Allocating 50 workers at once:
       --
       Worker_Arr := (others => new Worker);

       for I in Worker_Arr'Range loop
          Worker_Arr (I).Start (I);
       end loop;

       Put_Line ("Some processing...");
       delay 1.0;

       for W of Worker_Arr loop
          W.Stop;
       end loop;

    end Show_Workers;

In this example, we declare the task type :ada:`Worker`, the access type
:ada:`Worker_Access` and an array of access to tasks :ada:`Worker_Array`.
Using this approach, a task is only created when we allocate an individual
component of an array of :ada:`Worker_Array` type. Thus, when we declare
the :ada:`Worker_Arr` array in this example, we're only preparing a *container*
of 50 workers, but we don't have any actual tasks yet. We bring the 50 tasks
into existence by writing :ada:`Worker_Arr := (others => new Worker)`.


Discriminants as Access Values
------------------------------

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`
    - :arm22:`3.7.1 Discriminant Constraints <3-7-1>`

.. todo::

    Complete section!


Self-reference
--------------

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.1 Incomplete Type Declarations <3-10-1>`

.. todo::

    Complete section!


Dereferencing
-------------

In the :ref:`Introduction to Ada course <Intro_Ada_Access_Dereferencing>`, we
discussed the :ada:`.all` syntax to dereference access values:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Simple_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       --  Declaring access type:
       type Integer_Access is access Integer;

       --  Declaring access value:
       A1 : Integer_Access;

    begin
       A1 := new Integer;

       --  Dereferencing access value:
       A1.all := 22;

       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Dereferencing;

In this example, we declare :ada:`A1` as an access value, which allows us to
access objects of :ada:`Integer` type. We dereference :ada:`A1` by writing
:ada:`A1.all`.

Here's another example, this time with an array:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Array_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Array_Access is
         access Integer_Array;

       Arr : constant Integer_Array_Access :=
                        new Integer_Array (1 .. 6);
    begin
       Arr.all := (1, 2, 3, 5, 8, 13);

       for I in Arr'Range loop
          Put_Line ("Arr (: "
                    & Integer'Image (I) & "): "
                    & Integer'Image (Arr.all (I)));
       end loop;
    end Show_Dereferencing;

In this example, we dereference the access value by writing :ada:`Arr.all`. We
then assign an array aggregate to it |mdash| this becomes
:ada:`Arr.all := (..., ...);`. Similarly, in the loop, we write
:ada:`Arr.all (I)` to access the :ada:`I` component of the array.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.1 Names <4-1>`


.. _Adv_Ada_Implicit_Dereferencing:

Implicit Dereferencing
~~~~~~~~~~~~~~~~~~~~~~

Implicit dereferencing allows us to omit the :ada:`.all` suffix without getting
a compilation error. In this case, the compiler *knows* that the dereferenced
object is implied, not the access value.

Ada supports implicit dereferencing in these use cases:

- when accessing components of a record or an array |mdash| including array
  slices.

- when accessing subprograms that have at least one parameter (we
  discuss this topic later in this chapter);

- when accessing some attributes |mdash| such as some array and task
  attributes.

Arrays
^^^^^^

Let's start by looking into an example of implicit dereferencing of arrays. We
can take the previous code example and replace :ada:`Arr.all (I)` by
:ada:`Arr (I)`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Array_Implicit_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Array_Access is
         access Integer_Array;

       Arr : constant Integer_Array_Access :=
                        new Integer_Array (1 .. 6);
    begin
       Arr.all := (1, 2, 3, 5, 8, 13);

       Arr (1 .. 6) := (1, 2, 3, 5, 8, 13);

       for I in Arr'Range loop
          Put_Line
            ("Arr (: "
             & Integer'Image (I) & "): "
             & Integer'Image (Arr (I)));
          --                     ^ .all is implicit.
       end loop;
    end Show_Dereferencing;

Both forms |mdash| :ada:`Arr.all (I)` and :ada:`Arr (I)` |mdash| are
equivalent. Note, however, that there's no implicit dereferencing when we want
to access the whole array. (Therefore, we cannot write
:ada:`Arr := (1, 2, 3, 5, 8, 13);`.) However, as slices are implicitly
dereferenced, we can write :ada:`Arr (1 .. 6) := (1, 2, 3, 5, 8, 13);` instead
of :ada:`Arr.all (1 .. 6) := (1, 2, 3, 5, 8, 13);`. Alternatively, we can
assign to the array components individually and use implicit dereferencing for
each component:

.. code-block:: ada

   Arr (1) := 1;
   Arr (2) := 2;
   Arr (3) := 3;
   Arr (4) := 5;
   Arr (5) := 8;
   Arr (6) := 13;

Implicit dereferencing isn't available for the whole array because we have to
distinguish between assigning to access values and assigning to actual arrays.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Array_Assignments

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Array_Assignments is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Array_Access is
         access Integer_Array;

       procedure Show_Array
         (Name : String;
          Arr  : Integer_Array_Access) is
       begin
          Put (Name);
          for E of Arr.all loop
             Put (Integer'Image (E));
          end loop;
          New_Line;
       end Show_Array;

       Arr_1 : constant Integer_Array_Access :=
                          new Integer_Array (1 .. 6);
       Arr_2 :          Integer_Array_Access :=
                          new Integer_Array (1 .. 6);
    begin
       Arr_1.all := (1,   2,  3,  5,   8,  13);
       Arr_2.all := (21, 34, 55, 89, 144, 233);

       --  Array assignment
       Arr_2.all := Arr_1.all;

       Show_Array ("Arr_2", Arr_2);

       --  Access value assignment
       Arr_2 := Arr_1;

       Arr_1.all := (377, 610, 987, 1597, 2584, 4181);

       Show_Array ("Arr_2", Arr_2);
    end Show_Array_Assignments;

Here, :ada:`Arr_2.all := Arr_1.all` is an array assignment, while
:ada:`Arr_2 := Arr_1` is an access value assignment. By forcing the usage of
the :ada:`.all` suffix, the distinction is clear. Implicit dereferencing,
however, could be confusing here. (For example, the :ada:`.all` suffix in
:ada:`Arr_2 := Arr_1.all` is an oversight by the programmer when the intention
actually was to use access values on both sides.) Therefore, implicit
dereferencing is only supported in those cases where there's no risk of
ambiguities or oversights.

Records
^^^^^^^

Let's see an example of implicit dereferencing of a record:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Record_Implicit_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       type Rec is record
          I : Integer;
          F : Float;
       end record;

       type Rec_Access is access Rec;

       R : constant Rec_Access := new Rec;
    begin
       R.all := (I => 1, F => 5.0);

       Put_Line ("R.I: "
                 & Integer'Image (R.I));
       Put_Line ("R.F: "
                 & Float'Image (R.F));
    end Show_Dereferencing;

Again, we can replace :ada:`R.all.I` by :ada:`R.I`, as record components are
implicitly dereferenced. Also, we could use implicit dereference when assigning
to record components individually:

.. code-block:: ada

       R.I := 1;
       R.F := 5.0;

However, we have to write :ada:`R.all` when assigning to the whole record
:ada:`R`.

Attributes
^^^^^^^^^^

Finally, let's see an example of implicit dereference when using attributes:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Array_Implicit_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Array_Access is
         access Integer_Array;

       Arr : constant Integer_Array_Access :=
                        new Integer_Array (1 .. 6);
    begin
       Put_Line
         ("Arr'First: "
          & Integer'Image (Arr'First));
       Put_Line
         ("Arr'Last: "
          & Integer'Image (Arr'Last));

       Put_Line
         ("Arr'Component_Size: "
          & Integer'Image (Arr'Component_Size));
       Put_Line
         ("Arr.all'Component_Size: "
          & Integer'Image (Arr.all'Component_Size));

       Put_Line
         ("Arr'Size: "
          & Integer'Image (Arr'Size));
       Put_Line
         ("Arr.all'Size: "
          & Integer'Image (Arr.all'Size));
    end Show_Dereferencing;

Here, we can write :ada:`Arr'First` and :ada:`Arr'Last` instead of
:ada:`Arr.all'First` and :ada:`Arr.all'Last`, respectively, because :ada:`Arr`
is implicitly dereferenced. The same applies to :ada:`Arr'Component_Size`. Note
that we can write both :ada:`Arr'Size` and :ada:`Arr.all'Size`, but they have
different meanings:

- :ada:`Arr'Size` is the size of the access value; while

- :ada:`Arr.all'Size` indicates the size of the actual array :ada:`Arr`.

In other words, the :ada:`Size` attribute is *not* implicitly deferenced.
In fact, any attribute that could potentially be ambiguous is not implicitly
dereferenced. Therefore, in those cases, we must explicitly indicate (by using
:ada:`.all` or not) how we want to use the attribute.

Summary
^^^^^^^

The following table summarizes all instances where implicit dereferencing is
supported:

+------------------------+-------------------------+--------------------------+
| Entities               | Standard Usage          | Implicit Dereference     |
+========================+=========================+==========================+
| Array components       | Arr.all (I)             | Arr (I)                  |
+------------------------+-------------------------+--------------------------+
| Array slices           | Arr.all (F .. L)        | Arr (F .. L)             |
+------------------------+-------------------------+--------------------------+
| Record components      | Rec.all.C               | Rec.C                    |
+------------------------+-------------------------+--------------------------+
| Array attributes       | Arr.all’First           | Arr’First                |
|                        +-------------------------+--------------------------+
|                        | Arr.all’First (N)       | Arr’First (N)            |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Last            | Arr’Last                 |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Last (N)        | Arr’Last (N)             |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Range           | Arr’Range                |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Range (N)       | Arr’Range (N)            |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Length          | Arr’Length               |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Length (N)      | Arr’Length (N)           |
|                        +-------------------------+--------------------------+
|                        | Arr.all’Component_Size  | Arr’Component_Size       |
+------------------------+-------------------------+--------------------------+
| Task attributes        | T.all'Identity          | T'Identity               |
|                        +-------------------------+--------------------------+
|                        | T.all'Storage_Size      | T'Storage_Size           |
|                        +-------------------------+--------------------------+
|                        | T.all'Terminated        | T'Terminated             |
|                        +-------------------------+--------------------------+
|                        | T.all'Callable          | T'Callable               |
+------------------------+-------------------------+--------------------------+
| Tagged type attributes | X.all’Tag               | X’Tag                    |
+------------------------+-------------------------+--------------------------+
| Other attributes       | X.all'Valid             | X'Valid                  |
|                        +-------------------------+--------------------------+
|                        | X.all'Old               | X'Old                    |
|                        +-------------------------+--------------------------+
|                        | A.all’Constrained       | A’Constrained            |
+------------------------+-------------------------+--------------------------+

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.1 Names <4-1>`
    - :arm22:`4.1.1 Indexed Components <4-1-1>`
    - :arm22:`4.1.2 Slices <4-1-2>`
    - :arm22:`4.1.3 Selected Components <4-1-3>`
    - :arm22:`4.1.4 Attributes <4-1-4>`


.. _Adv_Ada_Ragged_Arrays:

Ragged arrays
-------------

Ragged arrays |mdash| also known as jagged arrays |mdash| are non-uniform,
multidimensional arrays. They can be useful to implement tables with varying
number of coefficients, as we discuss as an example in this section.

Uniform multidimensional arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider an algorithm that processes data based on coefficients that depends on
a selected quality level:

+------------------------+--------------+-----+-----+-----+-----+-----+
| Quality level          | Number of    |   #1|   #2|   #3|   #4|   #5|
|                        | coefficients |     |     |     |     |     |
+========================+==============+=====+=====+=====+=====+=====+
| Simplified             |            1 | 0.15|     |     |     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Better                 |            3 | 0.02| 0.16| 0.27|     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Best                   |            5 | 0.01| 0.08| 0.12| 0.20| 0.34|
+------------------------+--------------+-----+-----+-----+-----+-----+

(Note that this is just a bogus table with no real purpose, as we're not
trying to implement any actual algorithm.)

We can implement this table as a two-dimensional array (:ada:`Calc_Table`),
where each quality level has an associated array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

    private

       Calc_Table : constant array
         (Quality_Level, 1 .. 5) of Float :=
           (Simplified =>
                (0.15, 0.00, 0.00, 0.00, 0.00),
            Better     =>
                (0.02, 0.16, 0.27, 0.00, 0.00),
            Best       =>
                (0.01, 0.08, 0.12, 0.20, 0.34));

       Last : constant array
         (Quality_Level) of Positive :=
           (Simplified => 1,
            Better     => 3,
            Best       => 5);

    end Data_Processing;

Note that, in this implementation, we have a separate table :ada:`Last` that
indicates the actual number of coefficients of each quality level.

Alternatively, we could use a record (:ada:`Table_Coefficient`) that stores the
number of coefficients and the actual coefficients:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is
         array (Positive range <>) of Float;

    private

       type Table_Coefficient is record
          Last : Positive;
          Coef : Data (1 .. 5);
       end record;

       Calc_Table : constant array
         (Quality_Level) of Table_Coefficient :=
           (Simplified =>
                (1, (0.15, 0.00, 0.00, 0.00, 0.00)),
            Better     =>
                (3, (0.02, 0.16, 0.27, 0.00, 0.00)),
            Best       =>
                (5, (0.01, 0.08, 0.12, 0.20, 0.34)));

    end Data_Processing;

In this case, we have a unidimensional array where each component (of
:ada:`Table_Coefficient` type) contains an array (:ada:`Coef`) with the
coefficients.

This is an example of a :ada:`Process` procedure that references the
:ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Uniform_Table

    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in 1 .. Calc_Table (Q).Last loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Note that, to loop over the coefficients, we're using
:ada:`for J in 1 .. Calc_Table (Q).Last loop` instead of
:ada:`for J in Calc_Table (Q)'Range loop`. As we're trying to make a
non-uniform array fit in a uniform array, we cannot simply loop over all
elements using the :ada:`Range` attribute, but must be careful to use the
correct number of elements in the loop instead.

Also, note that :ada:`Calc_Table` has 15 coefficients in total. Out of those
coefficients, 6 coefficients (or 40 percent of the table) aren't being used.
Naturally, this is wasted memory space. We can improve this by using ragged
arrays.


Non-uniform multidimensional array
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ragged arrays are declared by using an access type to an array. By doing that,
each array can be declared with a different size, thereby creating a
non-uniform multidimensional array.

For example, we can declare a constant array :ada:`Table` as a ragged array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Simple_Ragged_Array

    package Data_Processing is

       type Integer_Array is
         array (Positive range <>) of Integer;

    private

       type Integer_Array_Access is
         access constant Integer_Array;

       Table : constant array (1 .. 3) of
                 Integer_Array_Access :=
         (1 => new Integer_Array'(1 => 15),
          2 => new Integer_Array'(1 => 12,
                                  2 => 15,
                                  3 => 20),
          3 => new Integer_Array'(1 => 12,
                                  2 => 15,
                                  3 => 20,
                                  4 => 20,
                                  5 => 25,
                                  6 => 30));

    end Data_Processing;

Here, each component of :ada:`Table` is an access to another array. As each
array is allocated via :ada:`new`, those arrays may have different sizes.

We can rewrite the example from the previous subsection using a ragged array
for the :ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Ragged_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is
         array (Positive range <>) of Float;

    private

       type Coefficients is access constant Data;

       Calc_Table : constant array (Quality_Level) of
                      Coefficients :=
         (Simplified =>
              new Data'(1 => 0.15),
          Better     =>
              new Data'(0.02, 0.16, 0.27),
          Best       =>
              new Data'(0.01, 0.08, 0.12,
                        0.20, 0.34));

    end Data_Processing;

Now, we aren't wasting memory space because each data component has the right
size that is required for each quality level. Also, we don't need to store the
number of coefficients, as this information is automatically available from the
array initialization (via the allocation of the :ada:`Data` array for the
:ada:`Coefficients` type).

This is the adapted :ada:`Process` procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Ragged_Arrays.Ragged_Table


    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in Calc_Table (Q)'Range loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Now, we can simply loop over the coefficients by writing
:ada:`for J in Calc_Table (Q)'Range loop`, as each element of :ada:`Calc_Table`
automatically has the correct range.


Aliasing
--------

The term :wikipedia:`aliasing <Aliasing_(computing)>`
refers to objects in memory that we can access using more than a single
reference. In Ada, if we allocate an object via :ada:`new`, we have a
potentially aliased object. We can then have multiple references to this
object:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Aliasing_Via_Access

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliasing is
       type Integer_Access is access Integer;

       A1, A2 : Integer_Access;
    begin
       A1 := new Integer;
       A2 := A1;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

       A2.all := 24;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));
    end Show_Aliasing;

In this example, we access the object allocated via :ada:`new` by using either
:ada:`A1` or :ada:`A2`, as both refer to the same *aliased* object. In other
words, :ada:`A1` or :ada:`A2` allow us to access the same object in memory.

.. admonition:: Important

    Note that aliasing is unrelated to renaming. For example, we could use
    renaming to write a program that looks similar to the one above:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Renaming

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Renaming is
           A1 : Integer;
           A2 : Integer renames A1;
        begin
           A1 := 22;
           Put_Line ("A1: " & Integer'Image (A1));
           Put_Line ("A2: " & Integer'Image (A2));

           A2 := 24;
           Put_Line ("A1: " & Integer'Image (A1));
           Put_Line ("A2: " & Integer'Image (A2));
        end Show_Renaming;

    Here, :ada:`A1` or :ada:`A2` are two different names for the same object.
    However, the object itself isn't aliased.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


.. _Adv_Ada_Aliased_Objects:

Aliased objects
~~~~~~~~~~~~~~~

As we discussed :ref:`previously <Adv_Ada_Access_Types_Allocation>`, we use
:ada:`new` to create aliased objects on the heap. We can also use general
access types to access objects that were created on the stack.

By default, objects created on the stack aren't aliased. Therefore, we have to
indicate that an object is aliased by using the :ada:`aliased` keyword in the
object's declaration: :ada:`Obj : aliased Integer;`.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Access_Aliased_Obj

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Obj is
       type Integer_Access is access all Integer;

       I_Var : aliased Integer;
       A1    : Integer_Access;
    begin
       A1 := I_Var'Access;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Aliased_Obj;

Here, we declare :ada:`I_Var` as an aliased integer variable and get a
reference to it, which we assign to :ada:`A1`. Naturally, we could also have
two accesses :ada:`A1` and :ada:`A2`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Access_Aliased_Obj

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Obj is
       type Integer_Access is access all Integer;

       I_Var  : aliased Integer;
       A1, A2 : Integer_Access;
    begin
       A1 := I_Var'Access;
       A2 := A1;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

       A2.all := 24;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

    end Show_Aliased_Obj;

In this example, both :ada:`A1` and :ada:`A2` refer to the :ada:`I_Var`
variable.

Note that these examples make use of these two features:

1. The declaration of a general access type (:ada:`Integer_Access`)
   using :ada:`access all`.

2. The retrieval of a reference to :ada:`I_Var` using the :ada:`Access`
   attribute.

In the next sections, we discuss these features in more details.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.3.1 Object Declarations <3-3-1>`
    - :arm22:`3.10 Access Types <3-10>`

General access modifiers
^^^^^^^^^^^^^^^^^^^^^^^^

Let's now discuss how to declare general access types. In addition to the
*standard* (pool-specific) access type declarations, Ada provides two access
modifiers:

+--------------------+----------------------------------------+
| Type               | Declaration                            |
+====================+========================================+
| Access-to-variable | :ada:`type T_Acc is access all T`      |
+--------------------+----------------------------------------+
| Access-to-constant | :ada:`type T_Acc is access constant T` |
+--------------------+----------------------------------------+

Let's look at an example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Show_Access_Modifiers

    package Integer_Access_Types is

       type Integer_Access is
         access Integer;

       type Integer_Access_All is
         access all Integer;

       type Integer_Access_Const is
         access constant Integer;

    end Integer_Access_Types;

As we've seen previously, we can use a type such as :ada:`Integer_Access` to
allocate objects dynamically. However, we cannot use this type to refer to
declared objects, for example. In this case, we have to use an
access-to-variable type such as :ada:`Integer_Access_All`. Also, if we want to
access constants |mdash| or access objects that we want to treat as constants
|mdash|, we use a type such as :ada:`Integer_Access_Const`.


Access attribute
^^^^^^^^^^^^^^^^

To get access to a variable or a constant, we make use of the :ada:`Access`
attribute. For example, :ada:`I_Var'Access` gives us access to the :ada:`I_Var`
object.

Let's look at an example of how to use the integer access types from the
previous code snippet:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Show_Access_Modifiers

    package Integer_Access_Types is

       type Integer_Access is
         access Integer;

       type Integer_Access_All is
         access all Integer;

       type Integer_Access_Const is
         access constant Integer;

       procedure Show;

    end Integer_Access_Types;

    with Ada.Text_IO;          use Ada.Text_IO;

    package body Integer_Access_Types is

       I_Var : aliased          Integer :=  0;
       Fact  : aliased constant Integer := 42;

       Dyn_Ptr     : constant Integer_Access
                       := new Integer'(30);
       I_Var_Ptr   : constant Integer_Access_All
                       := I_Var'Access;
       I_Var_C_Ptr : constant Integer_Access_Const
                       := I_Var'Access;
       Fact_Ptr    : constant Integer_Access_Const
                       := Fact'Access;

       procedure Show is
       begin
          Put_Line ("Dyn_Ptr:     "
                    & Integer'Image (Dyn_Ptr.all));
          Put_Line ("I_Var_Ptr:   "
                    & Integer'Image (I_Var_Ptr.all));
          Put_Line ("I_Var_C_Ptr: "
                    & Integer'Image
                        (I_Var_C_Ptr.all));
          Put_Line ("Fact_Ptr:    "
                    & Integer'Image (Fact_Ptr.all));
       end Show;

    end Integer_Access_Types;

    with Integer_Access_Types;

    procedure Show_Access_Modifiers is
    begin
       Integer_Access_Types.Show;
    end Show_Access_Modifiers;

In this example, :ada:`Dyn_Ptr` refers to a dynamically allocated object,
:ada:`I_Var_Ptr` refers to the :ada:`I_Var` variable, and :ada:`Fact_Ptr`
refers to the :ada:`Fact` constant. We get access to the variable and the
constant objects by using the :ada:`Access` attribute.

Also, we declare :ada:`I_Var_C_Ptr` as an access-to-constant, but we get
access to the :ada:`I_Var` variable. This simply means the object
:ada:`I_Var_C_Ptr` refers to is treated as a constant. Therefore, we can
write :ada:`I_Var := 22;`, but we cannot write :ada:`I_Var_C_Ptr.all := 22;`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.2 Operations of Access Types <3-10-2>`


Non-aliased objects
^^^^^^^^^^^^^^^^^^^

As mentioned earlier, by default, declared objects |mdash| which are allocated
on the stack |mdash| aren't aliased. Therefore, we cannot get a reference to
those objects. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Access_Non_Aliased_Obj
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Access_Error is
       type Integer_Access is access all Integer;
       I_Var : Integer;
       A1    : Integer_Access;
    begin
       A1 := I_Var'Access;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Access_Error;

In this example, the compiler complains that we cannot get a reference to
:ada:`I_Var` because :ada:`I_Var` is not aliased.


Ragged arrays using aliased objects
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can use aliased objects to declare
:ref:`ragged arrays <Adv_Ada_Ragged_Arrays>`. For example, we can rewrite a
previous program using aliased constant objects:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Ragged_Array_Aliased_Objs

    package Data_Processing is

       type Integer_Array is
         array (Positive range <>) of Integer;

    private

       type Integer_Array_Access is
         access constant Integer_Array;

       Tab_1 : aliased constant Integer_Array
                 := (1 => 15);
       Tab_2 : aliased constant Integer_Array
                 := (12, 15, 20);
       Tab_3 : aliased constant Integer_Array
                 := (12, 15, 20,
                     20, 25, 30);

       Table : constant array (1 .. 3) of
                 Integer_Array_Access :=
         (1 => Tab_1'Access,
          2 => Tab_2'Access,
          3 => Tab_3'Access);

    end Data_Processing;

Here, instead of allocating the constant arrays dynamically via :ada:`new`, we
declare three aliased arrays (:ada:`Tab_1`, :ada:`Tab_2` and :ada:`Tab_3`) and
get a reference to them in the declaration of :ada:`Table`.


Aliased access objects
^^^^^^^^^^^^^^^^^^^^^^

It's interesting to mention that access objects can be aliased themselves.
Consider this example where we declare the :ada:`Integer_Access_Access` type
to refer to an access object:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Aliased_Access

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Access_Obj is

       type Integer_Access        is
         access all Integer;
       type Integer_Access_Access is
         access all Integer_Access;

       I_Var : aliased Integer;
       A     : aliased Integer_Access;
       B     : Integer_Access_Access;
    begin
       A := I_Var'Access;
       B := A'Access;

       B.all.all := 22;
       Put_Line ("A: " & Integer'Image (A.all));
       Put_Line ("B: " & Integer'Image (B.all.all));
    end Show_Aliased_Access_Obj;

After the assignments in this example, :ada:`B` refers to :ada:`A`, which in
turn refers to :ada:`I_Var`. Note that this code only compiles because we
declare :ada:`A` as an aliased (access) object.


Aliased components
~~~~~~~~~~~~~~~~~~

Components of an array or a record can be aliased. This allows us to get access
to those components:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Aliased_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Components is

       type Integer_Access is access all Integer;

       type Rec is record
          I_Var_1 :         Integer;
          I_Var_2 : aliased Integer;
       end record;

       type Integer_Array is
         array (Positive range <>) of aliased Integer;

       R   : Rec := (22, 24);
       Arr : Integer_Array (1 .. 3) := (others => 42);
       A   : Integer_Access;
    begin
       --  A := R.I_Var_1'Access;
       --                 ^ ERROR: cannot access
       --                          non-aliased
       --                          component

       A := R.I_Var_2'Access;
       Put_Line ("A: " & Integer'Image (A.all));

       A := Arr (2)'Access;
       Put_Line ("A: " & Integer'Image (A.all));
    end Show_Aliased_Components;

In this example, we get access to the :ada:`I_Var_2` component of record
:ada:`R`. (Note that trying to access the :ada:`I_Var_1` component would gives us
a compilation error, as this component is not aliased.) Similarly, we get
access to the second component of array :ada:`Arr`.

Declaring components with the :ada:`aliased` keyword allows us to specify that
those are accessible via other paths besides the component name. Therefore, the
compiler won't store them in registers. This can be essential when doing
low-level programming |mdash| for example, when accessing memory-mapped
registers. In this case, we want to ensure that the compiler uses the memory
address we're specifying (instead of assigning registers for those components).

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.6 Array Types <3-6>`


Aliased parameters
~~~~~~~~~~~~~~~~~~

In addition to objects and components, we can declare aliased parameters. This
allows us to get access to those parameters in the body of a subprogram. We do
this by using the :ada:`aliased` keyword before the parameter mode.

The parameter mode indicates which type we must use for the access type, as
we'll discuss soon:

+------------------------+--------------------+
| Parameter mode         | Type               |
+========================+====================+
| :ada:`aliased in`      | Access-to-constant |
+------------------------+--------------------+
| :ada:`aliased out`     | Access-to-variable |
+------------------------+--------------------+
| :ada:`aliased in out`  | Access-to-variable |
+------------------------+--------------------+

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Aliasing.Aliased_Rec_Component

    package Data_Processing is

       procedure Proc (I : aliased in out Integer);

    end Data_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Data_Processing is

       procedure Show (I : aliased Integer) is
          --               ^ equivalent to
          --                 "aliased in Integer"

          type Integer_Constant_Access is
            access constant Integer;

          A : constant Integer_Constant_Access
                := I'Access;
       begin
          Put_Line ("Value : I "
                    & Integer'Image (A.all));
       end Show;

       procedure Set_One (I : aliased out Integer) is

          type Integer_Access is access all Integer;

          procedure Local_Set_One (A : Integer_Access)
          is
          begin
             A.all := 1;
          end Local_Set_One;

       begin
          Local_Set_One (I'Access);
       end Set_One;

       procedure Proc (I : aliased in out Integer) is

          type Integer_Access is access all Integer;

          procedure Add_One (A : Integer_Access) is
          begin
             A.all := A.all + 1;
          end Add_One;

       begin
          Show (I);
          Add_One (I'Access);
          Show (I);
       end Proc;

    end Data_Processing;

    with Data_Processing; use Data_Processing;

    procedure Show_Aliased_Param is
       I : aliased Integer := 22;
    begin
       Proc (I);
    end Show_Aliased_Param;

Here, :ada:`Proc` has an :ada:`aliased in out` parameter. In :ada:`Proc` \'s
body, we declare the :ada:`Integer_Access` type as an :ada:`access all` type.
We use the same approach in body of the :ada:`Set_One` procedure, which has an
:ada:`aliased out` parameter. Finally, the :ada:`Show` procedure has
an :ada:`aliased in` parameter. Therefore, we declare the
:ada:`Integer_Constant_Access` as an :ada:`access constant` type.

Note that parameter aliasing has an influence on how arguments are passed to a
subprogram when the parameter is of scalar type. When a scalar parameter is
declared as aliased, the corresponding argument is passed by reference.
For example, if we had declared :ada:`procedure Show (I : Integer)`, the
argument for :ada:`I` would be passed by value. However, since we're declaring
it as :ada:`aliased Integer`, it is passed by reference.

.. admonition:: In the Ada Reference Manual

    - :arm22:`6.1 Subprogram Declarations <6-1>`
    - :arm22:`6.2 Formal Parameter Modes <6-2>`
    - :arm22:`6.4.1 Parameter Associations <6-4-1>`


.. _Adv_Ada_Accessibility_Levels_Intro:

Accessibility Levels and Rules: An Introduction
-----------------------------------------------

This section provides an introduction to accessibility levels and accessibility
rules. This topic can be very complicated, and by no means do we intend to
cover all the details here. (In fact, discussing all the details about
accessibility levels and rules could be a long chapter on its own. If you're
interested in them, please refer to the Ada Reference Manual.) In any case, the
goal of this section is to present the intention behind the accessibility rules
and build intuition on how to best use access types in your code.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.2 Operations of Access Types <3-10-2>`

Lifetime of objects
~~~~~~~~~~~~~~~~~~~

First, let's talk a bit about
:wikipedia:`lifetime of objects <Variable_(computer_science)#Scope_and_extent>`.
We assume you understand the concept, so this section is very short.

In very simple terms, the lifetime of an object indicates when an object still
has relevant information. For example, if a variable :ada:`V` gets out of
scope, we say that its lifetime has ended. From this moment on, :ada:`V`
no longer exists.

For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Lifetime
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Lifetime is
       I_Var_1 : Integer := 22;
    begin

       Inner_Block : declare
          I_Var_2 : Integer := 42;
       begin
          Put_Line ("I_Var_1: "
                    & Integer'Image (I_Var_1));
          Put_Line ("I_Var_2: "
                    & Integer'Image (I_Var_2));

          --  I_Var_2 will get out of scope
          --  when the block finishes.
       end Inner_Block;

       --  I_Var_2 is now out of scope...

       Put_Line ("I_Var_1: "
                 & Integer'Image (I_Var_1));
       Put_Line ("I_Var_2: "
                 & Integer'Image (I_Var_2));
       --                         ^^^^^^^
       --  ERROR: lifetime of I_Var_2 has ended!
    end Show_Lifetime;

In this example, we declare :ada:`I_Var_1` in the :ada:`Show_Lifetime`
procedure, and :ada:`I_Var_2` in its :ada:`Inner_Block`.

This example doesn't compile because we're trying to use :ada:`I_Var_2` after
its lifetime has ended. However, if such a code could compile and run, the last
call to :ada:`Put_Line` would potentially display garbage to the user.
(In fact, the actual behavior would be undefined.)


.. _Adv_Ada_Accessibility_Levels:

Accessibility Levels
~~~~~~~~~~~~~~~~~~~~

In basic terms, accessibility levels are a mechanism to assess the lifetime
of objects (as we've just discussed). The starting point is the library level:
this is the base level, and no level can be deeper than that. We start "moving"
to deeper levels when we use a library in a subprogram or call other
subprograms for example.

Suppose we have a procedure :ada:`Proc` that makes use of a package :ada:`Pkg`,
and there's a block in the :ada:`Proc` procedure:

.. code-block:: ada

    package Pkg is

       --  Library level

    end Pkg;

    with Pkg; use Pkg;

    procedure Proc is

       --  One level deeper than
       --  library level

    begin

       declare
          --  Two levels deeper than
          --  library level
      begin
          null;
       end;

    end Proc;

For this code, we can say that:

- the specification of :ada:`Pkg` is at library level;

- the declarative part of :ada:`Proc` is one level deeper than the library
  level; and

- the block is two levels deeper than the library level.

(Note that this is still a very simplified overview of accessibility levels.
Things start getting more complicated when we use information from :ada:`Pkg`
in :ada:`Proc`. Those details will become more clear in the next sections.)

The levels themselves are not visible to the programmer. For example, there's
no :ada:`Access_Level` attribute that returns an integer value indicating the
level. Also, you cannot write a user message that displays the level at a
certain point. In this sense, accessibility levels are assessed relatively to
each other: we can only say that a specific operation is at the same or at a
deeper level than another one.


.. _Adv_Ada_Accessibility_Rules:

Accessibility Rules
~~~~~~~~~~~~~~~~~~~

The accessibility rules determine whether a specific use of access types or
objects is legal (or not). Actually, accessibility rules exist to prevent
:ref:`dangling references <Adv_Ada_Dangling_References>`, which we discuss
later. Also, they are based on the
:ref:`accessibility levels <Adv_Ada_Accessibility_Levels>` we discussed
earlier.


.. _Adv_Ada_Accessibility_Rules_Code_Example:

Code example
^^^^^^^^^^^^

As mentioned earlier, the accessibility level at a specific point isn't visible
to the programmer. However, to illustrate which level we have at each point in
the following code example, we use a prefix (:ada:`L0`, :ada:`L1`, and
:ada:`L2`)  to indicate whether we're at the library level (:ada:`L0`) or at a
deeper level.

Let's now look at the complete code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Accessibility_Library_Level
    :class: ada-expect-compile-error

    package Library_Level is

       type L0_Integer_Access is
         access all Integer;

       L0_IA  : L0_Integer_Access;

       L0_Var : aliased Integer;

    end Library_Level;

    with Library_Level; use Library_Level;

    procedure Show_Library_Level is
       type L1_Integer_Access is
         access all Integer;

       L0_IA_2 : L0_Integer_Access;
       L1_IA   : L1_Integer_Access;

       L1_Var : aliased Integer;

       procedure Test is
          type L2_Integer_Access is
            access all Integer;

          L2_IA  : L2_Integer_Access;

          L2_Var : aliased Integer;
       begin
          L1_IA := L2_Var'Access;
          --       ^^^^^^
          --       ILLEGAL: L2 object to
          --                L1 access object

          L2_IA := L2_Var'Access;
          --       ^^^^^^
          --       LEGAL: L2 object to
          --              L2 access object
       end Test;

    begin
       L0_IA := new Integer'(22);
       --       ^^^^^^^^^^^
       --       LEGAL: L0 object to
       --              L0 access object

       L0_IA_2 := new Integer'(22);
       --         ^^^^^^^^^^^
       --         LEGAL: L0 object to
       --                L0 access object

       L0_IA := L1_Var'Access;
       --       ^^^^^^
       --       ILLEGAL: L1 object to
       --                L0 access object

       L0_IA_2 := L1_Var'Access;
       --         ^^^^^^
       --         ILLEGAL: L1 object to
       --                  L0 access object

       L1_IA := L0_Var'Access;
       --       ^^^^^^
       --       LEGAL: L0 object to
       --              L1 access object

       L1_IA := L1_Var'Access;
       --       ^^^^^^
       --       LEGAL: L1 object to
       --              L1 access object

       L0_IA := L1_IA;
       --       ^^^^^
       --       ILLEGAL: type mismatch

       L0_IA := L0_Integer_Access (L1_IA);
       --       ^^^^^^^^^^^^^^^^^
       --       ILLEGAL: cannot convert
       --                L1 access object to
       --                L0 access object

       Test;
    end Show_Library_Level;

In this example, we declare

- in the :ada:`Library_Level` package: the :ada:`L0_Integer_Access` type, the
  :ada:`L0_IA` access object, and the :ada:`L0_Var` aliased variable;

- in the :ada:`Show_Library_Level` procedure: the :ada:`L1_Integer_Access`
  type, the :ada:`L0_IA_2` and :ada:`L1_IA` access objects, and the
  :ada:`L1_Var` aliased variable;

- in the nested :ada:`Test` procedure: the :ada:`L2_Integer_Access` type, the
  :ada:`L2_IA`, and the :ada:`L2_Var` aliased variable.

As mentioned earlier, the :ada:`Ln` prefix indicates the level of each type or
object. Here, the :ada:`n` value is zero at library level. We then increment
the :ada:`n` value each time we refer to a deeper level.

For instance:

- when we declare the :ada:`L1_Integer_Access` type in the
  :ada:`Show_Library_Level` procedure, that declaration is one level deeper
  than the level of the :ada:`Library_Level` package |mdash| so it has the
  :ada:`L1` prefix.

- when we declare the :ada:`L2_Integer_Access` type in the :ada:`Test`
  procedure, that declaration is one level deeper than the level of the
  :ada:`Show_Library_Level` procedure |mdash| so it has the :ada:`L2` prefix.

Types and Accessibility Levels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It's very important to highlight the fact that:

- types themselves also have an associated level, and

- objects have the same accessibility level as their types.

When we declare the :ada:`L0_IA_2` object in the code example, its
accessibility level is at library level because its type
(the :ada:`L0_Integer_Access` type) is at library level. Even though this
declaration is in the :ada:`Show_Library_Level` procedure |mdash| whose
declarative part is one level deeper than the library level |mdash|, the object
itself has the same accessibility level as its type.

Now that we've discussed the accessibility levels of this code example, let's
see how the accessibility rules use those levels.


Operations on Access Types
^^^^^^^^^^^^^^^^^^^^^^^^^^

In very simple terms, the accessibility rules say that:

- operations on access types at the same accessibility level are legal;

- assigning or converting to a deeper level is legal;

Otherwise, operations targeting objects at a *less-deep* level are illegal.

For example, :ada:`L0_IA := new Integer'(22)` and :ada:`L1_IA := L1_Var'Access`
are legal because we're operating at the same accessibility level. Also,
:ada:`L1_IA := L0_Var'Access` is legal because :ada:`L1_IA` is at a deeper
level than :ada:`L0_Var'Access`.

However, many operations in the code example are illegal. For instance,
:ada:`L0_IA := L1_Var'Access` and :ada:`L0_IA_2 := L1_Var'Access` are illegal
because the target objects in the assignment are *less* deep.

Note that the :ada:`L0_IA := L1_IA` assignment is mainly illegal because the
access types don't match. (Of course, in addition to that, assigning
:ada:`L1_Var'Access` to :ada:`L0_IA` is also illegal in terms of accessibility
rules.)


Conversion between Access Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The same rules apply to the conversion between access types. In the
code example, the :ada:`L0_Integer_Access (L1_IA)` conversion is illegal
because the resulting object is less deep. That being said, conversions on the
same level are fine:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Same_Level_Conversion

    procedure Show_Same_Level_Conversion is
       type L1_Integer_Access is
         access all Integer;

       type L1_B_Integer_Access is
         access all Integer;

       L1_IA   : L1_Integer_Access;
       L1_B_IA : L1_B_Integer_Access;

       L1_Var  : aliased Integer;
    begin
       L1_IA := L1_Var'Access;

       L1_B_IA := L1_B_Integer_Access (L1_IA);
       --         ^^^^^^^^^^^^^^^^^^^
       --         LEGAL: conversion from
       --                L1 access object to
       --                L1 access object
    end Show_Same_Level_Conversion;

Here, we're converting from the :ada:`L1_Integer_Access` type to the
:ada:`L1_B_Integer_Access`, which are both at the same level.


.. _Adv_Ada_Dangling_References:

Dangling References
~~~~~~~~~~~~~~~~~~~

An access value that points to a non-existent object is called a dangling
reference. Later on, we'll discuss how dangling references may occur using
:ref:`unchecked deallocation <Adv_Ada_Unchecked_Deallocation_Dangling_References>`.

Dangling references are created when we have an access value pointing to an
object whose lifetime has ended, so it becomes a  non-existent object. This
could occur, for example, when an access value still points to an object
:ada:`X` that has gone out of scope.

As mentioned in the previous section, the accessibility rules of the Ada
language ensure that such situations never happen! In fact, whenever possible,
the compiler applies those rules to detect potential dangling references at
compile time. When this detection isn't possible at compile time, the compiler
introduces an :ref:`accessibility check <Adv_Ada_Accessibility_Check>`. If this
check fails at runtime, it raises a :ada:`Program_Error` exception |mdash|
thereby preventing that a dangling reference gets used.

Let's see an example of how dangling references could occur:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Dangling_Reference_Rules
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dangling_Reference is

       type Integer_Access is
         access all Integer;

       I_Var_1 : aliased Integer := 22;

       A1    : Integer_Access;
    begin
       A1 := I_Var_1'Access;
       Put_Line ("A1.all: "
                 & Integer'Image (A1.all));

       Put_Line ("Inner_Block will start now!");

       Inner_Block : declare
          --
          --  I_Var_2 only exists in Inner_Block
          --
          I_Var_2 : aliased Integer := 42;

          --
          --  A2 only exists in Inner_Block
          --
          A2      : Integer_Access;
       begin
          A2 := I_Var_1'Access;
          Put_Line ("A2.all: "
                    & Integer'Image (A2.all));

          A1 := I_Var_2'Access;
          --   PROBLEM: A1 and Integer_Access type
          --            have longer lifetime than
          --            I_Var_2

          Put_Line ("A1.all: "
                    & Integer'Image (A1.all));

          A2 := I_Var_2'Access;
          --   PROBLEM: A2 has the same lifetime as
          --            I_Var_2, but Integer_Access
          --            type has a longer lifetime.

          Put_Line ("A2.all: "
                    & Integer'Image (A2.all));
       end Inner_Block;

       Put_Line ("Inner_Block has ended!");
       Put_Line ("A1.all: "
                 & Integer'Image (A1.all));

    end Show_Dangling_Reference;

Here, we declare the access objects :ada:`A1` and :ada:`A2` of
:ada:`Integer_Access` type, and the :ada:`I_Var_1` and :ada:`I_Var_2` objects.
Moreover, :ada:`A1` and :ada:`I_Var_1` are declared in the scope of the
:ada:`Show_Dangling_Reference` procedure, while :ada:`A2` and :ada:`I_Var_2`
are declared in the :ada:`Inner_Block`.

When we try to compile this code, we get two compilation errors due to
violation of accessibility rules. Let's now discuss these accessibility rules
in terms of lifetime, and see which problems they are preventing in each case.

1. In the :ada:`A1 := I_Var_2'Access` assignment, the main problem is that
   :ada:`A1` has a longer lifetime than :ada:`I_Var_2`. After the
   :ada:`Inner_Block` finishes |mdash| when :ada:`I_Var_2` gets out of scope
   and its lifetime has ended |mdash|, :ada:`A1` would still be pointing to an
   object that does not longer exist.

2. In the :ada:`A2 := I_Var_2'Access` assignment, however, both :ada:`A2` and
   :ada:`I_Var_2` have the same lifetime. In that sense, the assignment may
   actually look pretty much OK.

   - However, as mentioned in the previous section, Ada also cares about the
     lifetime of access types. In fact, since the :ada:`Integer_Access` type is
     declared outside of the :ada:`Inner_Block`, it has a longer lifetime than
     :ada:`A2` and :ada:`I_Var_2`.

   - To be more precise, the accessibility rules detect that :ada:`A2` is an
     access object of a type that has a longer lifetime than :ada:`I_Var_2`.

At first glance, this last accessibility rule may seem too strict, as both
:ada:`A2` and :ada:`I_Var_2` have the same lifetime |mdash| so nothing bad
could occur when dereferencing :ada:`A2`. However, consider the following
change to the code:

.. code-block:: ada

          A2 := I_Var_2'Access;

          A1 := A2;
          --    PROBLEM: A1 will still be referring
          --             to I_Var_2 after the
          --             Inner_Block, i.e. when the
          --             lifetime of I_Var_2 has
          --             ended!

Here, we're introducing the :ada:`A1 := A2` assignment. The problem with this
is that :ada:`I_Var_2`\ 's lifetime ends when the :ada:`Inner_Block` finishes,
but :ada:`A1` would continue to refer to an :ada:`I_Var_2` object that doesn't
exist anymore |mdash| thereby creating a dangling reference.

Even though we're actually not assigning :ada:`A2` to :ada:`A1` in the original
code, we could have done it. The accessibility rules ensure that such an error
is never introduced into the program.

.. admonition:: For further reading...

    In the original code, we can consider the :ada:`A2 := I_Var_2'Access`
    assignment to be safe, as we're not using the :ada:`A1 := A2` assignment
    there. Since we're confident that no error could ever occur in the
    :ada:`Inner_Block` due to the assignment to :ada:`A2`, we could replace it
    with :ada:`A2 := I_Var_2'Unchecked_Access`, so that the compiler accepts
    it. We discuss more about the unchecked access attribute
    :ref:`later in this chapter <Adv_Ada_Unchecked_Access>`.

    Alternatively, we could have solved the compilation issue that we see in
    the :ada:`A2 := I_Var_2'Access` assignment by declaring another access type
    locally in the :ada:`Inner_Block`:

    .. code-block:: ada

           Inner_Block : declare
              type Integer_Local_Access is
                access all Integer;

              I_Var_2 : aliased Integer := 42;

              A2      : Integer_Local_Access;
           begin
              A2 := I_Var_2'Access;
              --   This assignment is fine because
              --   the Integer_Local_Access type has
              --   the same lifetime as I_Var_2.
           end Inner_Block;

    With this change, :ada:`A2` becomes an access object of a type that has the
    same lifetime as :ada:`I_Var_2`, so that the assignment doesn't violate the
    rules anymore.

    (Note that in the :ada:`Inner_Block`, we could have simply named the local
    access type :ada:`Integer_Access` instead of :ada:`Integer_Local_Access`,
    thereby masking the :ada:`Integer_Access` type of the outer block.)

We discuss the effects of dereferencing dangling references
:ref:`later in this chapter <Adv_Ada_Dereferencing_Dangling_References>`.


.. _Adv_Ada_Unchecked_Access:

Unchecked Access
----------------

In this section, we discuss the :ada:`Unchecked_Access` attribute, which we
can use to circumvent accessibility issues for objects in specific cases. (Note
that this attribute only exists for objects, not for subprograms.)

We've seen :ref:`previously <Adv_Ada_Accessibility_Levels_Intro>` that the
accessibility levels verify the lifetime of access types. Let's see a
simplified version of a code example from that section:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Access.Dangling_Reference_Rules
    :class: ada-expect-compile-error

    package Integers is

       type Integer_Access is access all Integer;

    end Integers;

    with Ada.Text_IO; use Ada.Text_IO;

    with Integers; use Integers;

    procedure Show_Access_Issue is
       I_Var : aliased Integer := 42;

       A     : Integer_Access;
    begin
       A := I_Var'Access;
       --   PROBLEM: A has the same lifetime as I_Var,
       --            but Integer_Access type has a
       --            longer lifetime.

       Put_Line ("A.all: " & Integer'Image (A.all));
    end Show_Access_Issue;

Here, the compiler complains about the :ada:`A := I_Var'Access` assignment
because the :ada:`Integer_Access` type has a longer lifetime than :ada:`A`.
However, we know that this assignment to :ada:`A` |mdash| and further uses of
:ada:`A` in the code |mdash| won't cause dangling references to be created.
Therefore, we can assume that assigning the access to :ada:`I_Var` to :ada:`A`
is safe.

When we're sure that an access assignment cannot possibly generate dangling
references, we can the use :ada:`Unchecked_Access` attribute. For instance, we
can use this attribute to circumvent the compilation error in the previous code
example, since we know that the assignment is actually safe:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Access.Dangling_Reference_Rules

    package Integers is

       type Integer_Access is access all Integer;

    end Integers;

    with Ada.Text_IO; use Ada.Text_IO;

    with Integers; use Integers;

    procedure Show_Access_Issue is
       I_Var : aliased Integer := 42;

       A     : Integer_Access;
    begin
       A := I_Var'Unchecked_Access;
       --   OK: assignment is now accepted.

       Put_Line ("A.all: " & Integer'Image (A.all));
    end Show_Access_Issue;

When we use the :ada:`Unchecked_Access` attribute, most rules still apply.
The only difference to the standard :ada:`Access` attribute is that unchecked
access applies the rules as if the object we're getting access to was being
declared at library level. (For the code example we've just seen, the check
would be performed as if :ada:`I_Var` was declared in the :ada:`Integers`
package instead of being declared in the procedure.)

It is strongly recommended to avoid unchecked access in general. You should
only use it when you can safely assume that the access value will be discarded
before the object we had access to gets out of scope. Therefore, if this
situation isn't clear enough, it's best to avoid unchecked access. (Later in
this chapter, we'll see some of the nasty issues that arrive from creating
dangling references.) Instead, you should work on improving the software design
of your application by considering alternatives such as using containers or
encapsulating access types in well-designed abstract data types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Unchecked Access Value Creation <13-10>`


Unchecked Deallocation
----------------------

So far, we've seen multiple examples of using :ada:`new` to allocate objects.
In this section, we discuss how to manually deallocate objects.

Our starting point to manually deallocate an object is the generic
:ada:`Ada.Unchecked_Deallocation` procedure. We first instantiate this
procedure for an access type whose objects we want to be able to deallocate.
For example, let's instantiate it for the :ada:`Integer_Access` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Simple_Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       --
       --  Instantiation of Ada.Unchecked_Deallocation
       --  for the Integer_Access type:
       --
       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access);
    end Integer_Types;

Here, we declare the :ada:`Free` procedure, which we can then use to deallocate
objects that were allocated for the :ada:`Integer_Access` type.

:ada:`Ada.Unchecked_Deallocation` is a generic procedure that we can
instantiate for access types. When declaring an instance of
:ada:`Ada.Unchecked_Deallocation`, we have to specify arguments for:

- the formal :ada:`Object` parameter, which indicates the type of actual
  objects that we want to deallocate; and

- the formal :ada:`Name` parameter, which indicates the access type.

In a type declaration such as :ada:`type Integer_Access is access Integer`,
:ada:`Integer` denotes the :ada:`Object`, while :ada:`Integer_Access` denotes
the :ada:`Name`.

Because each instance of :ada:`Ada.Unchecked_Deallocation` is bound to a
specific access type, we cannot use it for another access type, even if the
type we use for the :ada:`Object` parameter is the same:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Simple_Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access);

       type Another_Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Another_Integer_Access);
    end Integer_Types;

Here, we're declaring two :ada:`Free` procedures: one for the
:ada:`Integer_Access` type, another for the :ada:`Another_Integer_Access`. We
cannot use the :ada:`Free` procedure for the :ada:`Integer_Access` type when
deallocating objects associated with the :ada:`Another_Integer_Access` type,
even though both types are declared as :ada:`access Integer`.

Note that we can use any name when instantiating the
:ada:`Ada.Unchecked_Deallocation` procedure. However, naming it :ada:`Free` is
very common.

Now, let's see a complete example that includes object allocation and
deallocation:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access);

       procedure Show_Is_Null (I : Integer_Access);

    end Integer_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Types is

       procedure Show_Is_Null (I : Integer_Access) is
       begin
          if I = null then
             Put_Line ("access value is null.");
          else
             Put_Line ("access value is NOT null.");
          end if;
       end Show_Is_Null;

    end Integer_Types;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I : Integer_Access;

    begin
       Put ("We haven't called new yet... ");
       Show_Is_Null (I);

       Put ("Calling new... ");
       I := new Integer;
       Show_Is_Null (I);

       Put ("Calling Free... ");
       Free (I);
       Show_Is_Null (I);
    end Show_Unchecked_Deallocation;

In the :ada:`Show_Unchecked_Deallocation` procedure, we first allocate an
object for :ada:`I` and then call :ada:`Free (I)` to deallocate it. Also, we
call the :ada:`Show_Is_Null` procedure at three different points: before any
allocation takes place, after allocating an object for :ada:`I`, and after
deallocating that object.

When we deallocate an object via a call to :ada:`Free`, the corresponding
access value |mdash| which was previously pointing to an existing object
|mdash| is set to :ada:`null`. Therefore, :ada:`I = null` after the call to
:ada:`Free`, which is exactly what we see when running this example code.

Note that it is OK to call :ada:`Free` multiple times for the same access
object:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I : Integer_Access;

    begin
       I := new Integer;

       Free (I);
       Free (I);
       Free (I);
    end Show_Unchecked_Deallocation;

The multiple calls to :ada:`Free` for the same access object don't cause any
issues. Because the access value is null after the first call to
:ada:`Free (I)`, we're actually just passing :ada:`null` as an argument in the
second and third calls to :ada:`Free`. However, any attempt to deallocate an
access value of null is ignored in the :ada:`Free` procedure, so the second and
third calls to :ada:`Free` don't have any effect.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.8 Allocators <4-8>`
    - :arm22:`13.11.2 Unchecked Storage Deallocation <13-11-2>`


.. _Adv_Ada_Unchecked_Deallocation_Dangling_References:

Unchecked Deallocation and Dangling References
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've discussed :ref:`dangling references <Adv_Ada_Dangling_References>`
before. In this section, we discuss how unchecked deallocation can create
dangling references and the issues of having them in an application.

Let's reuse the last example and introduce :ada:`I_2`, which will point to the
same object as :ada:`I`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation
    :class: ada-run-expect-failure

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I, I_2 : Integer_Access;

    begin
       I := new Integer;

       I_2 := I;

       --  NOTE: I_2 points to the same
       --        object as I.

       --
       --  Use I and I_2...
       --
       --  ... then deallocate memory...
       --

       Free (I);

       --  NOTE: at this point, I_2 is a
       --        dangling reference!

       --  Further calls to Free (I)
       --  are OK!

       Free (I);
       Free (I);

       --  A call to Free (I_2) is
       --  NOT OK:

       Free (I_2);
    end Show_Unchecked_Deallocation;

As we've seen before, we can have multiple calls to :ada:`Free (I)`.
However, the call to :ada:`Free (I_2)` is bad because :ada:`I_2` is not null.
In fact, it is a dangling reference |mdash| i.e. :ada:`I_2` points to an object
that doesn't exist anymore. Also, the first call to :ada:`Free (I)` will
reclaim the storage that was allocated for the object that :ada:`I`
originally referred to. The call to :ada:`Free (I_2)` will then try to reclaim
the previously-reclaimed object, but it'll fail in an undefined manner.

Because of these potential errors, you should be very careful when using
unchecked deallocation: it is the programmer's responsibility to avoid creating
dangling references!

For the example we've just seen, we could avoid creating a dangling reference
by explicitly assigning :ada:`null` to :ada:`I_2` to indicate that it doesn't
point to any specific object:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I, I_2 : Integer_Access;

    begin
       I := new Integer;

       I_2 := I;

       --  NOTE: I_2 points to the same
       --        object as I.

       --
       --  Use I and I_2...
       --
       --  ... then deallocate memory...
       --

       I_2 := null;

       --  NOTE: now, I_2 doesn't point to
       --        any object, so calling
       --        Free (I_2) is OK.

       Free (I);
       Free (I_2);
    end Show_Unchecked_Deallocation;

Now, calling :ada:`Free (I_2)` doesn't cause any issues because it doesn't
point to any object.

Note, however, that this code example is just meant to illustrate the issues of
dangling pointers and how we could circumvent them. We're not suggesting to use
this approach when designing an implementation. In fact, it's not practical for
the programmer to make every possible dangling reference become null if the
calls to :ada:`Free` are strewn throughout the code.

The suggested design is to not use :ada:`Free` in the client code, but
instead hide its use within bigger abstractions. In that way, all the
occurrences of the calls to :ada:`Free` are in one package, and the programmer
of that package can then prevent dangling references. We'll discuss these
:ref:`design strategies <Adv_Ada_Design_Strategies_Access_Types>` later on.


.. _Adv_Ada_Dereferencing_Dangling_References:

Dereferencing dangling references
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Of course, you shouldn't try to dereference a dangling reference because your
program becomes erroneous, as we discuss in this section. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation

    with Ada.Text_IO;   use Ada.Text_IO;
    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I_1, I_2 : Integer_Access;

    begin
       I_1 := new Integer'(42);
       I_2 := I_1;

       Put_Line ("I_1.all = "
                 & Integer'Image (I_1.all));
       Put_Line ("I_2.all = "
                 & Integer'Image (I_2.all));

       Put_Line ("Freeing I_1");
       Free (I_1);

       if I_1 /= null then
          Put_Line ("I_1.all = "
                    & Integer'Image (I_1.all));
       end if;

       if I_2 /= null then
          Put_Line ("I_2.all = "
                    & Integer'Image (I_2.all));
       end if;
    end Show_Unchecked_Deallocation;

In this example, we allocate an object for :ada:`I_1` and make :ada:`I_2` point
to the same object. Then, we call :ada:`Free (I)`, which has the following
consequences:

- The call to :ada:`Free (I_1)` will try to reclaim the storage for the
  original object (:ada:`I_1.all`), so it may be reused for other allocations.

- :ada:`I_1 = null` after the call to :ada:`Free (I_1)`.

- :ada:`I_2` becomes a dangling reference by the call to :ada:`Free (I_1)`.

   - In other words, :ada:`I_2` is still non-null, and what it points to is now
     undefined.

In principle, we could check for :ada:`null` before trying to dereference the
access value. (Remember that when deallocating an object via a call to
:ada:`Free`, the corresponding access value is set to :ada:`null`.) In fact,
this strategy works fine for :ada:`I_1`, but it doesn't work for :ada:`I_2`
because the access value is not :ada:`null`. As a consequence, the application
tries to dereference :ada:`I_2`.

Dereferencing a dangling reference is erroneous: the behavior is undefined in
this case. For the example we've just seen,

- :ada:`I_2.all` might make the application crash;

- :ada:`I_2.all` might give us a different value than before;

- :ada:`I_2.all` might even give us the same value as before (42) if the
  original object is still available.

Because the effect is unpredictable, it might be really difficult to debug the
application and identify the cause.

Having dangling pointers in an application should be avoided at all costs!
Again, it is the programmer's responsibility to be very careful when using
unchecked deallocation: avoid creating dangling references!

.. admonition:: In the Ada Reference Manual

   - :arm22:`13.9.1 Data Validity <13-9-1>`
   - :arm22:`13.11.2 Unchecked Storage Deallocation <13-11-2>`


Restrictions for :ada:`Ada.Unchecked_Deallocation`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two unsurprising restrictions for :ada:`Ada.Unchecked_Deallocation`:

1. It cannot be instantiated for access-to-constant types; and

2. It cannot be used when the :ada:`Storage_Size` aspect of a type is zero
   (i.e. when its storage pool is empty).

(Note that this last restriction also applies to the allocation via
:ada:`new`.)

Let's see an example of these restrictions:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Unchecked_Deallocation.Unchecked_Deallocation_Error
    :class: ada-expect-compile-error

    with Ada.Unchecked_Deallocation;

    procedure Show_Unchecked_Deallocation_Errors is

       type Integer_Access_Zero is access Integer
         with Storage_Size => 0;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access_Zero);

       type Constant_Integer_Access is
         access constant Integer;

       --  ERROR: Cannot use access-to-constant type
       --         for Name
       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Constant_Integer_Access);

       I : Integer_Access_Zero;

    begin
       --  ERROR: Cannot allocate objects from
       --         empty storage pool
       I := new Integer;

       --  ERROR: Cannot deallocate objects from
       --         empty storage pool
       Free (I);
    end Show_Unchecked_Deallocation_Errors;

Here, we see that trying to instantiate :ada:`Ada.Unchecked_Deallocation` for
the :ada:`Constant_Integer_Access` type is rejected by the compiler. Similarly,
we cannot allocate or deallocate an object for the :ada:`Integer_Access_Zero`
type because its storage pool is empty.


.. _Adv_Ada_Not_Null_Access:

Null & Not Null Access
----------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #23: Null Considered Harmful <https://www.adacore.com/gems/ada-gem-23>`__
    and `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`__.

Ada, like many languages, defines a special :ada:`null` value for access
types. All values of an access type designate some object of the
designated type, except for :ada:`null`, which does not designate any
object. The null value can be used as a special flag. For example, a
singly-linked list can be null-terminated. A :ada:`Lookup` function can
return :ada:`null` to mean "not found", presuming the result is of an
access type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Null_Return
    :class: ada-syntax-only

    package Show_Null_Return is

       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.
    end Show_Null_Return;

An alternative design for :ada:`Lookup` would be to raise an exception:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Not_Found_Exception
    :class: ada-syntax-only

    package Show_Not_Found_Exception is
       Not_Found : exception;

       function Lookup (T : Table) return Ref_Element;
       --  Raises Not_Found if not found.
       --  Never returns null.
    end Show_Not_Found_Exception;

Neither design is better in all situations; it depends in part on whether
we consider the "not found" situation to be exceptional.

Clearly, the client calling :ada:`Lookup` needs to know whether it can
return :ada:`null`, and if so, what that means. In general, it's a good
idea to document whether things can be null or not, especially for formal
parameters and function results. Prior to Ada 2005, we would do that with
comments. Since Ada 2005, we can use the :ada:`not null` syntax:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Not_Null_Return
    :class: ada-syntax-only

    package Show_Not_Null_Return is
       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table)
                        return not null Ref_Element;
       --  Possible since Ada 2005.
    end Show_Not_Null_Return;

This is a complete package for the code snippets above:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Complete_Null_Return

    package Example is

       type Element is limited private;
       type Ref_Element is access all Element;

       type Table is limited private;

       Not_Found : constant Ref_Element := null;
       function Lookup (T : Table)
                        return Ref_Element;
       --  Returns Not_Found if not found.

       Not_Found_2 : exception;
       function Lookup_2 (T : Table)
                          return not null Ref_Element;
       --  Raises Not_Found_2 if not found.

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    private
       type Element is limited
          record
             Component : Integer;
          end record;
       type Table is limited null record;
    end Example;

    package body Example is

       An_Element : aliased Element;

       function Lookup (T : Table)
                        return Ref_Element is
          pragma Unreferenced (T);
       begin
          --  ...
          return Not_Found;
       end Lookup;

       function Lookup_2 (T : Table)
                          return not null Ref_Element
       is
       begin
          --  ...
          raise Not_Found_2;

          return An_Element'Access;
          --  suppress error: 'missing "return"
          --  statement in function body'
       end Lookup_2;

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

       procedure R is
       begin
          Q (An_Element'Access);
       end R;

      pragma Unreferenced (R);

    end Example;

In general, it's better to use the language proper for documentation, when
possible, rather than comments, because compile-time and/or run-time
checks can help ensure that the "documentation" is actually true. With
comments, there's a greater danger that the comment will become false
during maintenance, and false documentation is obviously a menace.

In many, perhaps most cases, :ada:`null` is just a tripping hazard. It's
a good idea to put in :ada:`not null` when possible. In fact, a good
argument can be made that :ada:`not null` should be the default, with
extra syntax required when :ada:`null` is wanted. This is the way
:wikipedia:`Standard ML <Standard_ML>` works, for
example |mdash| you don't get any special null-like value unless you ask
for it. Of course, because Ada 2005 needs to be compatible with previous
versions of the language, :ada:`not null` cannot be the default for Ada.

One word of caution: access objects are default-initialized to
:ada:`null`, so if you have a :ada:`not null` object (or component) you
had better initialize it explicitly, or you will get
:ada:`Constraint_Error`. :ada:`not null` is more often useful on
parameters and function results, for this reason.

Another advantage of :ada:`not null` over comments is for efficiency.
Consider procedures :ada:`P` and :ada:`Q` in this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Complete_Null_Return

    package Example.Processing is

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    end Example.Processing;

    package body Example.Processing is

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

    end Example.Processing;

Without :ada:`not null`, the generated code for :ada:`P` will do a check
that :ada:`X /= null`, which may be costly on some systems. :ada:`P` is
called in a loop, so this check will likely occur many times. With
:ada:`not null`, the check is pushed to the call site. Pushing checks to
the call site is usually beneficial because

    1. the check might be hoisted out of a loop by the optimizer, or

    2. the check might be eliminated altogether, as in the example
       above, where the compiler knows that :ada:`An_Element'Access` cannot
       be :ada:`null`.

This is analogous to the situation with other run-time checks, such as
array bounds checks:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Null_And_Not_Null_Access.Process_Array

    package Show_Process_Array is

       type My_Index is range 1 .. 10;
       type My_Array is array (My_Index) of Integer;

       procedure Process_Array
         (X     : in out My_Array;
          Index :        My_Index);

    end Show_Process_Array;

    package body Show_Process_Array is

       procedure Process_Array
         (X     : in out My_Array;
          Index :        My_Index) is
       begin
          X (Index) := X (Index) + 1;
       end Process_Array;

    end Show_Process_Array;

If :ada:`X (Index)` occurs inside :ada:`Process_Array`, there is no need
to check that :ada:`Index` is in range, because the check is pushed to the
caller.


.. _Adv_Ada_Design_Strategies_Access_Types:

Design strategies for access types
----------------------------------

.. todo::

    Complete section!

    (See PR #752 for details.)

    It's not practical for the programmer to make every possible dangling
    reference become null if the calls to Free are strewn throughout the code.
    That means the suggested design is to not use Free in client code, but
    instead hide its use within the bigger abstraction. That way all the
    occurrences of the calls to Free are in one package, and the programmer of
    that package then can prevent dangling references. For example, if we have
    a bank account that involves dynamic allocation for some part of an
    account, the bank account is a private type (or limited), and the access
    values are hidden in the private part. Thus when an account is closed the
    allocated memory can be freed without the possibility of leaving dangling
    references. That will work as long as the reference cannot be copied when
    copying bank account objects, in which case we'd want the type to be
    limited as well as private. Or we'd make it a controlled type so that
    Finalize can call Free.


Access to subprograms
---------------------

Static vs. dynamic calls
~~~~~~~~~~~~~~~~~~~~~~~~

In a typical subprogram call, we indicate the subprogram we want to call
statically. For example, let's say we've implemented a procedure :ada:`Proc`
that calls a procedure :ada:`P`:

.. code:: ada compile_button main=proc.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Subprogram_Call

   procedure P (I : in out Integer);

   procedure P (I : in out Integer) is
   begin
      null;
   end P;

   with P;

   procedure Proc is
      I : Integer := 0;
   begin
      P (I);
   end Proc;

The call to :ada:`P` is statically dispatched: every time :ada:`Proc` runs and
calls :ada:`P`, that call is always to the same procedure. In other words, we
can determine at compilation time which procedure is called.

In contrast, an access to a subprogram allows us to dynamically indicate which
subprogram we want to call. For example, if we change :ada:`Proc` in the code
above to receive the access to a subprogram :ada:`P` as a parameter, the actual
procedure that would be called when running :ada:`Proc` would be determined at
run time, and it might be different for every call to :ada:`Proc`. In this
case, we wouldn't be able to determine at compilation time which
procedure would be called in every case. (In some cases, however, it could
still be possible to determine which procedure is called by analyzing the
argument that is passed to :ada:`Proc`.)


Access to subprogram declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We declare an access to a subprogram as a type by writing
:ada:`access procedure` or :ada:`access function` and the corresponding
prototype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

Note that, in the type declarations, we list all the parameters that we expect
in the subprogram.

We can use those types to declare access to subprograms |mdash| as subprogram
parameters, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    package Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure);

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure) is
          I : Integer := 0;
       begin
          P (I);
          --  P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

In the implementation of the :ada:`Proc` procedure of the code example, we call
the :ada:`P` procedure by simply passing :ada:`I` as a parameter. In this case,
:ada:`P` is automatically dereferenced. We may, however, explicitly dereference
:ada:`P` by writing :ada:`P.all (I)`.

Next, we can get access to a subprogram by using the :ada:`Access` attribute:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    procedure Add_Ten (I : in out Integer);

    procedure Add_Ten (I : in out Integer) is
    begin
       I := I + 10;
    end Add_Ten;

    with Access_To_Subprogram_Params;
    use  Access_To_Subprogram_Params;

    with Add_Ten;

    procedure Show_Access_To_Subprograms is
    begin
       Proc (Add_Ten'Access);
       --            ^ Getting access to Add_Ten
       --              procedure and passing it
       --              to Proc
    end Show_Access_To_Subprograms;

Here, we get access to the :ada:`Add_Ten` procedure and pass it to the
:ada:`Proc` procedure.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


Selecting subprograms
~~~~~~~~~~~~~~~~~~~~~

A practical application of access to subprograms is that it enables us to
dynamically select a subprogram and pass it to another subprogram, where it can
then be called.

For example, we may have a :ada:`Process` procedure that receives a logging
procedure as a parameter (:ada:`Log_Proc`). Also, this parameter may be
:ada:`null` by default |mdash| so that no procedure is called if the parameter
isn't specified:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Log_Procedure

    package Data_Processing is

       type Data_Container is
         array (Positive range <>) of Float;

       type Log_Procedure is
         access procedure (D : Data_Container);

       procedure Process
         (D        : in out Data_Container;
          Log_Proc :        Log_Procedure := null);

    end Data_Processing;

    package body Data_Processing is

       procedure Process
         (D        : in out Data_Container;
          Log_Proc :        Log_Procedure := null) is
       begin
          --  missing processing part...

          if Log_Proc /= null then
             Log_Proc (D);
          end if;
       end Process;

    end Data_Processing;

In the implementation of :ada:`Process`, we check whether :ada:`Log_Proc` is
null or not. (If it's not null, we call the procedure. Otherwise, we just skip
the call.)

Now, let's implement two logging procedures that match the expected form of
the :ada:`Log_Procedure` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line
      (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Csv (D : Data_Container) is
    begin
       for I in D'First .. D'Last - 1 loop
          Put (D (I)'Image & ", ");
       end loop;
       Put (D (D'Last)'Image);
       New_Line;
    end Log_Csv;

Finally, we implement a test application that selects each of the logging
procedures that we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Log_Element_Per_Line;
    with Log_Csv;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Log_Element_Per_Line ====");
       Process (D, Log_Element_Per_Line'Access);

       Put_Line ("==== Log_Csv ====");
       Process (D, Log_Csv'Access);

       Put_Line ("==== None ====");
       Process (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Log_Element_Per_Line` and :ada:`Log_Csv` procedures. Also, in the third
call, we don't pass any access as an argument, which is then :ada:`null` by
default.


Null exclusion
~~~~~~~~~~~~~~

We can use null exclusion when declaring an access to subprograms. By doing so,
we ensure that a subprogram must be specified |mdash| either as a parameter or
when initializing an access object. Otherwise, an exception is raised. Let's
adapt the previous example and introduce the :ada:`Init_Function` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_Init_Function

    package Data_Processing is

       type Data_Container is
         array (Positive range <>) of Float;

       type Init_Function is
         not null access function return Float;

       procedure Process
         (D         : in out Data_Container;
          Init_Func :        Init_Function);

    end Data_Processing;

    package body Data_Processing is

       procedure Process
         (D         : in out Data_Container;
          Init_Func :        Init_Function) is
       begin
          for I in D'Range loop
             D (I) := Init_Func.all;
          end loop;
       end Process;

    end Data_Processing;


In this case, we specify that :ada:`Init_Function` is :ada:`not null access`
because we want to always be able to call this function in the :ada:`Process`
procedure (i.e. without raising an exception).

When an access to a subprogram doesn't have parameters |mdash| which is the
case for the subprograms of :ada:`Init_Function` type |mdash| we need to
explicitly dereference it by writing :ada:`.all`. (In this case, :ada:`.all`
isn't optional.) Therefore, we have to write :ada:`Init_Func.all` in the
implementation of the :ada:`Process` procedure of the code example.

Now, let's declare two simple functions |mdash| :ada:`Init_Zero` and
:ada:`Init_One` |mdash| that return 0.0 and 1.0, respectively:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_Init_Function

    function Init_Zero return Float;
    function Init_One return Float;

    function Init_Zero return Float is
    begin
       return 0.0;
    end Init_Zero;

    function Init_One return Float is
    begin
       return 1.0;
    end Init_One;

Finally, let's see a test application where we select each of the init
functions we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_Init_Function

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line
      (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Init_Zero;
    with Init_One;

    with Log_Element_Per_Line;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Init_Zero ====");
       Process (D, Init_Zero'Access);
       Log_Element_Per_Line (D);

       Put_Line ("==== Init_One ====");
       Process (D, Init_One'Access);
       Log_Element_Per_Line (D);

       --  Put_Line ("==== None ====");
       --  Process (D, null);
       --  Log_Element_Per_Line (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Init_Zero` and :ada:`Init_One` functions. Also, if we uncomment the call
to :ada:`Process` with :ada:`null` as an argument for the init function, we see
that the :ada:`Constraint_Error` exception is raised at run time |mdash| as the
argument cannot be :ada:`null` due to the null exclusion.

.. admonition:: For further reading...

    .. note::

        This example was originally written by Robert A. Duff and was part of
        the `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`_.

    Here's another example, first with :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           type Iterate_Action is
             access procedure
               (X : not null Ref_Element);

           procedure Iterate
             (T      : Table;
              Action : Iterate_Action := null);
           --  If Action is null, do nothing.

        end Show_Null_Procedure;

    and without :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           procedure Do_Nothing
             (X : not null Ref_Element) is null;

           type Iterate_Action is
             access procedure
               (X : not null Ref_Element);

           procedure Iterate
             (T      : Table;
              Action : not null Iterate_Action
                         := Do_Nothing'Access);

        end Show_Null_Procedure;

    The style of the second :ada:`Iterate` is clearly better because it makes
    use of the syntax to indicate that a procedure is expected. This is a
    complete package that includes both versions of the :ada:`Iterate`
    procedure:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Complete_Not_Null_Procedure

        package Example is

           type Element is limited private;
           type Ref_Element is access all Element;

           type Table is limited private;

           type Iterate_Action is
             access procedure
               (X : not null Ref_Element);

           procedure Iterate
             (T : Table;
              Action : Iterate_Action := null);
           --  If Action is null, do nothing.

           procedure Do_Nothing
             (X : not null Ref_Element) is null;
           procedure Iterate_2
             (T : Table;
              Action : not null Iterate_Action
                         := Do_Nothing'Access);

        private
           type Element is limited
              record
                 Component : Integer;
              end record;
           type Table is limited null record;
        end Example;

        package body Example is

           An_Element : aliased Element;

           procedure Iterate
             (T : Table;
              Action : Iterate_Action := null)
           is
           begin
              if Action /= null then
                 Action (An_Element'Access);
                 --  In a real program, this would do
                 --  something more sensible.
              end if;
           end Iterate;

           procedure Iterate_2
             (T : Table;
              Action : not null Iterate_Action
                         := Do_Nothing'Access)
           is
           begin
              Action (An_Element'Access);
              --  In a real program, this would do
              --  something more sensible.
           end Iterate_2;

        end Example;

        with Example; use Example;

        procedure Show_Example is
           T : Table;
        begin
           Iterate_2 (T);
        end Show_Example;

    Writing :ada:`not null Iterate_Action` might look a bit more
    complicated, but it's worthwhile, and anyway, as mentioned earlier, the
    compatibility requirement requires that the :ada:`not null` be explicit,
    rather than the other way around.


Access to protected subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Up to this point, we've discussed access to *normal* Ada subprograms. In some
situations, however, we might want to have access to protected subprograms.
To do this, we can simply declare a type using :ada:`access protected`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Simple_Protected_Access

    package Simple_Protected_Access is

       type Access_Proc is
         access protected procedure;

       protected Obj is

          procedure Do_Something;

       end Obj;

       Acc : Access_Proc := Obj.Do_Something'Access;

    end Simple_Protected_Access;

    package body Simple_Protected_Access is

       protected body Obj is

          procedure Do_Something is
          begin
             --  Not doing anything
             --  for the moment...
             null;
          end Do_Something;

       end Obj;

    end Simple_Protected_Access;

Here, we declare the :ada:`Access_Proc` type as an access type to protected
procedures. Then, we declare the variable :ada:`Acc` and assign to it the
access to the :ada:`Do_Something` procedure (of the protected object
:ada:`Obj`).

Now, let's discuss a more useful example: a simple system that allows us to
register protected procedures and execute them. This is implemented in
:ada:`Work_Registry` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Protected_Access_Init_Function

    package Work_Registry is

       type Work_Id is tagged limited private;

       type Work_Handler is
         access protected procedure (T : Work_Id);

       subtype Valid_Work_Handler is
         not null Work_Handler;

       type Work_Handlers is
         array (Positive range <>) of Work_Handler;

       protected type Work_Handler_Registry
         (Last : Positive)
       is

          procedure Register (T : Valid_Work_Handler);

          procedure Reset;

          procedure Process_All;

       private

          D    : Work_Handlers (1 .. Last);
          Curr : Natural := 0;

       end Work_Handler_Registry;

    private

       type Work_Id is tagged limited null record;

    end Work_Registry;

    package body Work_Registry is

       protected body Work_Handler_Registry is

          procedure Register (T : Valid_Work_Handler)
          is
          begin
             if Curr < Last then
                Curr := Curr + 1;
                D (Curr) := T;
             end if;
          end Register;

          procedure Reset is
          begin
             Curr := 0;
          end Reset;

          procedure Process_All is
             Dummy_ID : Work_Id;
          begin
             for I in D'First .. Curr loop
                D (I).all (Dummy_ID);
             end loop;
          end Process_All;

       end Work_Handler_Registry;

    end Work_Registry;

Here, we declare the protected :ada:`Work_Handler_Registry` type with the
following subprograms:

- :ada:`Register`, which we can use to register a protected procedure;

- :ada:`Reset`, which we can use to reset the system; and

- :ada:`Process_All`, which we can use to call all procedures that were
  registered in the system.

:ada:`Work_Handler` is our access to protected subprogram type. Also, we
declare the :ada:`Valid_Work_Handler` subtype, which excludes :ada:`null`. By
doing so, we can ensure that only valid procedures are passed to the
:ada:`Register` procedure. In the protected :ada:`Work_Handler_Registry` type,
we store the procedures in an array (of :ada:`Work_Handlers` type).

.. admonition:: Important

    Note that, in the type declaration :ada:`Work_Handler`, we say that the
    protected procedure must have a parameter of :ada:`Work_Id` type. In this
    example, this parameter is just used to *bind* the procedure to the
    :ada:`Work_Handler_Registry` type. The :ada:`Work_Id` type itself is
    actually declared as a null record (in the private part of the package),
    and it isn't really useful on its own.

    If we had declared :ada:`type Work_Handler is access protected procedure;`
    instead, we would be able to register *any* protected procedure into the
    system, even the ones that might not be suitable for the system. By using
    a parameter of :ada:`Work_Id` type, however, we make use of strong
    typing to ensure that only procedures that were designed for the system
    can be registered.

In the next part of the code, we declare the :ada:`Integer_Storage` type,
which is a simple protected type that we use to store an integer value:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Protected_Access_Init_Function

    with Work_Registry;

    package Integer_Storage_System is

       protected type Integer_Storage is

          procedure Set (V : Integer);

          procedure Show (T : Work_Registry.Work_Id);

       private

          I : Integer := 0;

       end Integer_Storage;

       type Integer_Storage_Access is
         access Integer_Storage;

       type Integer_Storage_Array is
         array (Positive range <>) of
           Integer_Storage_Access;

    end Integer_Storage_System;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Storage_System is

       protected body Integer_Storage is

          procedure Set (V : Integer) is
          begin
             I := V;
          end Set;

          procedure Show (T : Work_Registry.Work_Id)
          is
             pragma Unreferenced (T);
          begin
             Put_Line ("Value: " & Integer'Image (I));
          end Show;

       end Integer_Storage;

    end Integer_Storage_System;

For the :ada:`Integer_Storage` type, we declare two procedures:

- :ada:`Set`, which we use to assign a value to the (protected) integer value;
  and

- :ada:`Show`, which we use to show the integer value that is stored in the
  protected object.

The :ada:`Show` procedure has a parameter of :ada:`Work_Id` type, which
indicates that this procedure was designed to be registered in the system of
:ada:`Work_Handler_Registry` type.

Finally, we have a test application in which we declare a registry (:ada:`WHR`)
and an array of "protected integer objects" (:ada:`Int_Stor`):

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Protected_Access_Init_Function

    with Work_Registry;
    use  Work_Registry;

    with Integer_Storage_System;
    use  Integer_Storage_System;

    procedure Show_Access_To_Protected_Subprograms is

       WHR      : Work_Handler_Registry (5);
       Int_Stor : Integer_Storage_Array (1 .. 3);

    begin
       --  Allocate and initialize integer storage
       --
       --  (For the initialization, we're just
       --  assigning the index here, but we could
       --  really have used any integer value.)

       for I in Int_Stor'Range loop
          Int_Stor (I) := new Integer_Storage;
          Int_Stor (I).Set (I);
       end loop;

       --  Register handlers

       for I in Int_Stor'Range loop
          WHR.Register (Int_Stor (I).all.Show'Access);
       end loop;

       --  Now, use Process_All to call the handlers
       --  (in this case, the Show procedure for
       --  each protected object from Int_Stor).

       WHR.Process_All;

    end Show_Access_To_Protected_Subprograms;

The work handler registry (:ada:`WHR`) has a maximum capacity of five
procedures, whereas the :ada:`Int_Stor` array has a capacity of three elements.
By calling :ada:`WHR.Register` and passing :ada:`Int_Stor (I).all.Show'Access`,
we register the :ada:`Show` procedure of each protected object from
:ada:`Int_Stor`.

.. admonition:: Important

    Note that the components of the :ada:`Int_Stor` array are of
    :ada:`Integer_Storage_Access` type, which is declared as an access to
    :ada:`Integer_Storage` objects. Therefore, we have to dereference the
    object (by writing :ada:`Int_Stor (I).all`) before getting access to the
    :ada:`Show` procedure (by writing :ada:`.Show'Access`).

    We have to use an access type here because we cannot pass the access (to
    the :ada:`Show` procedure) of a local object in the call to the
    :ada:`Register` procedure. Therefore, the protected objects (of
    :ada:`Integer_Storage` type) cannot be local.

    This issue becomes evident if we replace the declaration of
    :ada:`Int_Stor` with a local array (and then adapt the remaining code). If
    we do this, we get a compilation error in the call to :ada:`Register`:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Protected_Access_Init_Function
        :class: ada-expect-compile-error

        with Work_Registry;
        use  Work_Registry;

        with Integer_Storage_System;
        use  Integer_Storage_System;

        procedure Show_Access_To_Protected_Subprograms
        is
           WHR      : Work_Handler_Registry (5);

           Int_Stor : array (1 .. 3) of Integer_Storage;

        begin
           --  Allocate and initialize integer storage
           --
           --  (For the initialization, we're just
           --  assigning the index here, but we could
           --  really have used any integer value.)

           for I in Int_Stor'Range loop
              --  Int_Stor (I) := new Integer_Storage;
              Int_Stor (I).Set (I);
           end loop;

           --  Register handlers

           for I in Int_Stor'Range loop
              WHR.Register (Int_Stor (I).Show'Access);
              --            ^ ERROR!
           end loop;

           --  Now, call the handlers
           --  (i.e. the Show procedure of each
           --   protected object).

           WHR.Process_All;

        end Show_Access_To_Protected_Subprograms;

    As we've just discussed, this error is due to the fact that
    :ada:`Int_Stor` is now a "local" protected object, and the accessibility
    rules don't allow mixing it with non-local accesses in order to prevent the
    possibility of dangling references.

When we call :ada:`WHR.Process_All`, the registry system calls each procedure
that has been registered with the system. When looking at the values displayed
by the test application, we may notice that each call to :ada:`Show` is
referring to a different protected object. In fact, even though we're passing
just the access to a protected *procedure* in the call to :ada:`Register`, that
access is also associated to a specific protected object. (This is different
from access to non-protected subprograms we've discussed previously: in that
case, there's no object associated.) If we replace the argument to
:ada:`Register` by :ada:`Int_Stor (2).all.Show'Access`, for example, the three
:ada:`Show` procedures registered in the system will now refer to the same
protected object (stored at :ada:`Int_Stor (2)`).

Also, even though we have registered the same procedure (:ada:`Show`) of the
same type (:ada:`Integer_Storage`) in all calls to :ada:`Register`, we could
have used a different protected procedure |mdash| and of a different protected
type. As an exercise, we could, for example, create a new type called
:ada:`Float_Storage` (based on the code that we used for the
:ada:`Integer_Storage` type) and register some objects of :ada:`Float_Storage`
type into the system (with a couple of additional calls to :ada:`Register`). If
we then call :ada:`WHR.Process_All`, we'd see that the system is able to cope
with objects of both :ada:`Integer_Storage` and :ada:`Float_Storage` types. In
fact, the system implemented with the :ada:`Work_Handler_Registry` can be seen
as "type agnostic," as it doesn't care about which type the protected objects
have |mdash| as long as the subprograms we want to register are conformant to
the :ada:`Valid_Work_Handler` type.


.. _Adv_Ada_Accessibility_Rules_Access_To_Subprograms:

Accessibility Rules and Access-To-Subprograms
---------------------------------------------

.. todo::

    Complete section!


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Accessibility checks
    --------------------

    .. note::

        This section was originally written by Ramón Fernández and Robert A. Duff, and
        published as
        `Gem #33: Accessibility Checks <https://www.adacore.com/gems/gem-33>`__,
        `Gem #41 <https://www.adacore.com/gems/gem-41>`__ and
        `Gem #44: <https://www.adacore.com/gems/gem-44>`__.
