Access Types
============

.. include:: ../../../global.txt

We discussed access types back in the
:ref:`Introduction to Ada course <Intro_Ada_Access_Types_Overview>`. In
this chapter, we discuss further details about access types and techniques when
using them. Before we dig into details, however, we're going to make sure
we understand the terminology.

Access types: Terminology
-------------------------

In this section, we discuss some of the terminology associated with access
types. Usually, the terms used in Ada when discussing references and dynamic
memory allocation are different than the ones you might encounter in other
languages, so it's necessary you understand what each term means.

Access type, designated subtype and profile
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first term we encounter is (obviously) *access type*, which is a type that
provides us access to an object or a subprogram. We declare access types by
using the :ada:`access` keyword:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Terminology.Access_Type_Declaration

    package Show_Access_Type_Declaration is

       --
       --  Declaring access types:
       --

       --  Access-to-object type
       type Integer_Access is access Integer;

       --  Access-to-subprogram type
       type Init_Integer_Access is access
         function return Integer;

    end Show_Access_Type_Declaration;

Here, we're declaring two access types: the access-to-object type
:ada:`Integer_Access` and the access-to-subprogram type
:ada:`Init_Integer_Access`. (We discuss access-to-subprogram types
:ref:`later on <Adv_Ada_Access_To_Subprograms>`).

In the declaration of an access type, we always specify |mdash| after the
:ada:`access` keyword |mdash| the kind of thing we want to designate. In the
case of an access-to-object type declaration, we declare a subtype we want to
access, which is known as the *designated subtype* of an access type. In the
case of an access-to-subprogram type declaration, the subprogram prototype is
known as the *designated profile*.

In our previous code example, :ada:`Integer` is the designated subtype of the
:ada:`Integer_Access` type, and :ada:`function return Integer` is the
designated profile of the :ada:`Init_Integer_Access` type.

Access object and designated object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use an access-to-object type by first declaring a variable (or constant) of
an access type and then allocating an object. (This is actually just one way of
using access types; we discuss other methods later in this chapter.) The actual
variable or constant of an access type is called *access object*, while the
object we allocate (via :ada:`new`) is the *designated object*.

For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Object_Allocated_Object.Simple_Allocation

    procedure Show_Simple_Allocation is

       --  Access-to-object type
       type Integer_Access is access Integer;

       --  Access object
       I1 : Integer_Access;

    begin
       I1 := new Integer;
       --    ^^^^^^^^^^^ allocating an object,
       --                which becomes the designated
       --                object for I1

    end Show_Simple_Allocation;

In this example, :ada:`I1` is an access object and the object allocated via
:ada:`new Integer` is its designated object.

Access value and designated value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An access object and a designated (allocated) object, both store values. The
value of an access object is the *access value* and the value of a designated
object is the *designated value*. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_Value_Designated_Value.Values

    procedure Show_Values is

       --  Access-to-object type
       type Integer_Access is access Integer;

       I1, I2, I3 : Integer_Access;

    begin
       I1 := new Integer;
       I3 := new Integer;

       --  Copying the access value of I1 to I2
       I2 := I1;

       --  Copying the designated value of I1
       I3.all := I1.all;

    end Show_Values;

In this example, the assignment :ada:`I2 := I1` copies the access value of
:ada:`I1` to :ada:`I2`. The assignment :ada:`I3.all := I1.all` copies
:ada:`I1`\'s designated value to :ada:`I3`\'s designated object.
(As we already know, :ada:`.all` is used to dereference an access object. We
discuss this topic again :ref:`later in this chapter <Adv_Ada_Dereferencing>`.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`

.. _Adv_Ada_Access_Types_Allocation:

Access types: Allocation
-------------------------

Ada makes the distinction between pool-specific and general access types, as
we'll discuss in this section. Before doing so, however, let's talk about
memory allocation.

In general terms, memory can be allocated dynamically on the
heap or statically on the stack. (Strictly speaking, both are dynamic
allocations, in that they occur at run-time with amounts not previously
specified.) For example:

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


.. _Adv_Ada_Discriminants_As_Access_Values:

Discriminants as Access Values
------------------------------

We can use access types when declaring discriminants. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Discriminant_Access_Values

    package Custom_Recs is

       --  Declaring an access type:
       type Integer_Access is access Integer;

       --  Declaring a discriminant with this
       --  access type:
       type Rec (IA : Integer_Access) is record

          I : Integer := IA.all;
          --          ^^^^^^^^^
          --  Setting I's default to use the
          --  designated value of IA:
       end record;

       procedure Show (R : Rec);

    end Custom_Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Recs is

       procedure Show (R : Rec) is
       begin
          Put_Line ("R.IA = "
                    & Integer'Image (R.IA.all));
          Put_Line ("R.I  = "
                    & Integer'Image (R.I));
       end Show;

    end Custom_Recs;

    with Custom_Recs; use Custom_Recs;

    procedure Show_Discriminants_As_Access_Values is

       IA : constant Integer_Access :=
              new Integer'(10);
       R  : Rec (IA);

    begin
       Show (R);

       IA.all := 20;
       R.I    := 30;
       Show (R);

       --  As expected, we cannot change the
       --  discriminant. The following line is
       --  triggers a compilation error:
       --
       --  R.IA := new Integer;

    end Show_Discriminants_As_Access_Values;

In the :ada:`Custom_Recs` package from this example, we declare the access
type :ada:`Integer_Access`. We then use this type to declare the discriminant
(:ada:`IA`) of the :ada:`Rec` type. In the
:ada:`Show_Discriminants_As_Access_Values` procedure, we see that (as expected)
we cannot change the discriminant of an object of :ada:`Rec` type: an
assignment such as :ada:`R.IA := new Integer` would trigger a compilation
error.

Note that we can use a default for the discriminant:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Discriminant_Access_Values

    package Custom_Recs is

       type Integer_Access is access Integer;

       type Rec (IA : Integer_Access
                        := new Integer'(0)) is
          --               ^^^^^^^^^^^^^^^
          --                default value
       record
          I : Integer := IA.all;
       end record;

       procedure Show (R : Rec);

    end Custom_Recs;

    with Custom_Recs; use Custom_Recs;

    procedure Show_Discriminants_As_Access_Values is

       R1 : Rec;
       --   ^^^
       --   no discriminant: use default

       R2 : Rec (new Integer'(20));
       --        ^^^^^^^^^^^^^^^^
       --        allocating an unnamed integer object

    begin
       Show (R1);
       Show (R2);
    end Show_Discriminants_As_Access_Values;

Here, we've changed the declaration of the :ada:`Rec` type to allocate an
integer object if the type's discriminant isn't provided |mdash| we can see
this in the declaration of the :ada:`R1` object in the
:ada:`Show_Discriminants_As_Access_Values` procedure. Also, in this
procedure, we're allocating an unnamed integer object in the declaration
of :ada:`R2`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`
    - :arm22:`3.7.1 Discriminant Constraints <3-7-1>`


Unconstrained type as designated subtype
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice that we were using a scalar type as the designated subtype of the
:ada:`Integer_Access` type. We could have used an unconstrained type as well.
In fact, this is often used for the sake of having the effect of an
unconstrained discriminant type.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Persons

    package Persons is

       --  Declaring an access type whose
       --  designated subtype is unconstrained:
       type String_Access is access String;

       --  Declaring a discriminant with this
       --  access type:
       type Person (Name : String_Access) is record
          Age : Integer;
       end record;

       procedure Show (P : Person);

    end Persons;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Persons is

       procedure Show (P : Person) is
       begin
          Put_Line ("Name = "
                    & P.Name.all);
          Put_Line ("Age  = "
                    & Integer'Image (P.Age));
       end Show;

    end Persons;

    with Persons; use Persons;

    procedure Show_Person is
       P : Person (new String'("John"));
    begin
       P.Age := 30;
       Show (P);
    end Show_Person;

In this example, the discriminant of the :ada:`Person` type has an
unconstrained designated type. In the :ada:`Show_Person` procedure, we declare
the :ada:`P` object and specify the constraints of the allocated string object
|mdash| in this case, a four-character string initialized with the name "John".

.. admonition:: For further reading...

    In the previous code example, we used an array |mdash| actually, a string
    |mdash| to demonstrate the advantage of using discriminants as access
    values, for we can use an unconstrained type as the designated subtype. In
    fact, as we discussed
    :ref:`earlier in another chapter <Adv_Ada_Indefinite_Subtype_Discriminant>`,
    we can only use discrete types (or access types) as discriminants.
    Therefore, you wouldn't be able to use a string, for example, directly as a
    discriminant without using access types:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Persons_Error
        :class: ada-expect-compile-error

        package Persons is

           --  ERROR: Declaring a discriminant with an
           --         unconstrained type:
           type Person (Name : String) is record
              Age : Integer;
           end record;

        end Persons;

    As expected, compilation fails for this code because the discriminant of
    the :ada:`Person` type is indefinite.

    However, the advantage of discriminants as access values isn't restricted
    to being able to use unconstrained types such as arrays: we could really
    use any type as the designated subtype! In fact, we can generalized this
    to:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Generic_Access

        generic
           type T (<>);  --  any type
           type T_Access is access T;
        package Gen_Custom_Recs is
           --  Declare a type whose discriminant D can
           --  access any type:
           type T_Rec (D : T_Access) is null record;
        end Gen_Custom_Recs;

        with Gen_Custom_Recs;

        package Custom_Recs is

            type Incomp;
            --  Incomplete type declaration!

           type Incomp_Access is access Incomp;

           --  Instantiating package using
           --  incomplete type Incomp:
           package Inst is new
             Gen_Custom_Recs
               (T        => Incomp,
                T_Access => Incomp_Access);
           subtype Rec is Inst.T_Rec;

           --  At this point, Rec (Inst.T_Rec) uses
           --  an incomplete type as the designated
           --  subtype of its discriminant type

           procedure Show (R : Rec) is null;

           --  Now, we complete the Incomp type:
           type Incomp (B : Boolean := True) is private;

        private
           --  Finally, we have the full view of the
           --  Incomp type:
           type Incomp (B : Boolean := True) is
             null record;

        end Custom_Recs;

        with Custom_Recs; use Custom_Recs;

        procedure Show_Rec is
           R : Rec (new Incomp);
        begin
           Show (R);
        end Show_Rec;

    In the :ada:`Gen_Custom_Recs` package, we're using :ada:`type T (<>)`
    |mdash| which can be any type |mdash| for the designated subtype of the
    access type :ada:`T_Access`, which is the type of :ada:`T_Rec`\'s
    discriminant. In the :ada:`Custom_Recs` package, we use the incomplete type
    :ada:`Incomp` to instantiate the generic package. Only after the
    instantiation, we declare the complete type.

Later on, we'll discuss discriminants again when we look into
:ref:`anonymous access discriminants <Adv_Ada_Anonymous_Access_Discriminants>`,
which provide some advantages in terms of
:ref:`accessibility rules <Adv_Ada_Accessibility_Levels_Intro>`.


Whole object assignments
~~~~~~~~~~~~~~~~~~~~~~~~

As expected, we cannot change the discriminant value in whole object
assignments. If we do that, the :ada:`Constraint_Error` exception is raised
at runtime:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Discriminants_As_Access_Values.Persons
    :class: ada-run-expect-failure

    with Persons; use Persons;

    procedure Show_Person is
       S1 : String_Access := new String'("John");
       S2 : String_Access := new String'("Mark");
       P : Person := (Name => S1,
                      Age  => 30);
    begin
       P := (Name => S1, Age => 31);
       --            ^^ OK: we didn't change the
       --                   discriminant.
       Show (P);

       --  We can just repeat the discriminant:
       P := (Name => P.Name, Age => 32);
       --            ^^^^^^ OK: we didn't change the
       --                       discriminant.
       Show (P);

       --  Of course, we can change the string itself:
       S1.all := "Mark";
       Show (P);

       P := (Name => S2, Age => 40);
       --            ^^ ERROR: we changed the
       --                      discriminant!
       Show (P);
    end Show_Person;

The first and the second assignments to :ada:`P` are OK because we didn't
change the discriminant. However, the last assignment raises the
:ada:`Constraint_Error` exception at runtime because we're changing the
discriminant.


.. _Adv_Ada_Parameters_As_Access_Values:

Parameters as Access Values
---------------------------

In addition to
:ref:`using discriminants as access values <Adv_Ada_Discriminants_As_Access_Values>`,
we can use access types for subprogram formal parameters. For example, the
:ada:`N` parameter of the :ada:`Show` procedure below has an access type:

.. code:: ada no_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names

    package Names is

       type Name is access String;

       procedure Show (N : Name);

    end Names;

This is the complete code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names

    package Names is

       type Name is access String;

       procedure Show (N : Name);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Names is

       procedure Show (N : Name) is
       begin
          Put_Line ("Name: " & N.all);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       N : Name := new String'("John");
    begin
       Show (N);
    end Show_Names;

Note that in this example, the :ada:`Show` procedure is basically just
displaying the string. Since the procedure isn't doing anything that justifies
the need for an access type, we could have implemented it with a *simpler*
type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names_String

    package Names is

       type Name is access String;

       procedure Show (N : String);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Names is

       procedure Show (N : String) is
       begin
          Put_Line ("Name: " & N);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       N : Name := new String'("John");
    begin
       Show (N.all);
    end Show_Names;

It's important to highlight the difference between passing an access value to
a subprogram and passing an object by reference. In both versions of this code
example, the compiler will make use of a reference for the actual parameter of
the :ada:`N` parameter of the :ada:`Show` procedure. However, the difference
between these two cases is that:

- :ada:`N : Name` is a reference to an object (because it's an access value)
  that is passed by value, and

- :ada:`N : String` is an object passed by reference.

Changing the referenced object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the :ada:`Name` type gives us access to an object in the :ada:`Show`
procedure, we could actually change this object inside the procedure. To
illustrate this, let's change the :ada:`Show` procedure to lower each
character of the string before displaying it (and rename the procedure to
:ada:`Lower_And_Show`):

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Changed_Names

    package Names is

       type Name is access String;

       procedure Lower_And_Show (N : Name);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Characters.Handling;
    use  Ada.Characters.Handling;

    package body Names is

       procedure Lower_And_Show (N : Name) is
       begin
          for I in N'Range loop
             N (I) := To_Lower (N (I));
          end loop;
          Put_Line ("Name: " & N.all);
       end Lower_And_Show;

    end Names;

    with Names; use Names;

    procedure Show_Changed_Names is
       N : Name := new String'("John");
    begin
       Lower_And_Show (N);
    end Show_Changed_Names;

Notice that, again, we could have implemented the :ada:`Lower_And_Show`
procedure without using an access type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Changed_Names_String

    package Names is

       type Name is access String;

       procedure Lower_And_Show (N : in out String);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Characters.Handling;
    use  Ada.Characters.Handling;

    package body Names is

       procedure Lower_And_Show (N : in out String) is
       begin
          for I in N'Range loop
             N (I) := To_Lower (N (I));
          end loop;
          Put_Line ("Name: " & N);
       end Lower_And_Show;

    end Names;

    with Names; use Names;

    procedure Show_Changed_Names is
       N : Name := new String'("John");
    begin
       Lower_And_Show (N.all);
    end Show_Changed_Names;

Replace the access value
~~~~~~~~~~~~~~~~~~~~~~~~

Instead of changing the object in the :ada:`Lower_And_Show` procedure, we
could replace the access value by another one |mdash| for example, by
allocating a new string inside the procedure. In this case, we have to pass the
access value by reference using the :ada:`in out` parameter mode:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Replaced_Names

    package Names is

       type Name is access String;

       procedure Lower_And_Show (N : in out Name);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Characters.Handling;
    use  Ada.Characters.Handling;

    package body Names is

       procedure Lower_And_Show (N : in out Name) is
       begin
          N := new String'(To_Lower (N.all));
          Put_Line ("Name: " & N.all);
       end Lower_And_Show;

    end Names;

    with Names; use Names;

    procedure Show_Changed_Names is
       N : Name := new String'("John");
    begin
       Lower_And_Show (N);
    end Show_Changed_Names;

Now, instead of changing the object referenced by :ada:`N`, we're actually
replacing it with a new object that we allocate inside the
:ada:`Lower_And_Show` procedure.

As expected, contrary to the previous examples, we cannot implement this
code by relying on parameter modes to replace the object. In fact, we have to
use access types for this kind of operations.

Note that this implementation creates a memory leak. In a proper
implementation, we should make sure to
:ref:`deallocate the object <Adv_Ada_Unchecked_Deallocation>`, as explained
later on.


Side-effects on designated objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In previous code examples from this section, we've seen that passing a
parameter by reference using the :ada:`in` or :ada:`in out` parameter modes
is an alternative to using access values as parameters. Let's focus on the
subprogram declarations of those code examples and their parameter modes:

+-----------------------+-----------------+-----------------+
| Subprogram            | Parameter type  | Parameter mode  |
+=======================+=================+=================+
| :ada:`Show`           | :ada:`Name`     | :ada:`in`       |
+-----------------------+-----------------+-----------------+
| :ada:`Show`           | :ada:`String`   | :ada:`in`       |
+-----------------------+-----------------+-----------------+
| :ada:`Lower_And_Show` | :ada:`Name`     | :ada:`in`       |
+-----------------------+-----------------+-----------------+
| :ada:`Lower_And_Show` | :ada:`String`   | :ada:`in out`   |
+-----------------------+-----------------+-----------------+

When we analyze the information from this table, we see that in the case of
using strings with different parameter modes, we have a clear indication
whether the subprogram might change the object or not. For example,
we know that a call to :ada:`Show (N : String)` won't change the string object
that we're passing as the actual parameter.

In the case of passing an access value, we cannot know whether the
designated object is going to be altered by a call to the subprogram. In fact,
in both :ada:`Show` and :ada:`Lower_And_Show` procedures, the parameter is the
same: :ada:`N : Name` |mdash| in other words, the parameter mode is :ada:`in`
in both cases. Here, there's no clear indication about the effects of a
subprogram call on the designated object.

The simplest way to ensure that the object isn't changed in the subprogram is
by using
:ref:`access-to-constant types <Adv_Ada_General_Access_Modifiers>`, which we
discuss later on. In this case, we're basically saying that the object we're
accessing in :ada:`Show` is constant, so we cannot possibly change it:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names_Constant

    package Names is

       type Name is access String;

       type Constant_Name is access constant String;

       procedure Show (N : Constant_Name);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    --  with Ada.Characters.Handling;
    --  use  Ada.Characters.Handling;

    package body Names is

       procedure Show (N : Constant_Name) is
       begin
          --  for I in N'Range loop
          --     N (I) := To_Lower (N (I));
          --  end loop;
          Put_Line ("Name: " & N.all);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       N : Name := new String'("John");
    begin
       Show (Constant_Name (N));
    end Show_Names;

In this case, the :ada:`Constant_Name` type ensures that the :ada:`N`
parameter won't be changed in the :ada:`Show` procedure. Note that we need
to convert from :ada:`Name` to :ada:`Constant_Name` to be able to call the
:ada:`Show` procedure (in the :ada:`Show_Names` procedure). Although using
:ada:`in String` is still a simpler solution, this approach works fine.

(Feel free to uncomment the call to :ada:`To_Lower` in the :ada:`Show`
procedure and the corresponding with- and use-clauses to see that the
compilation fails when trying to change the constant object.)

We could also mitigate the problem by using contracts. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names_Postcondition

    package Names is

       type Name is access String;

       procedure Show (N : Name)
         with Post => N.all'Old = N.all;
       --             ^^^^^^^^^^^^^^^^^
       --     we promise that we won't change
       --     the object

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    --  with Ada.Characters.Handling;
    --  use  Ada.Characters.Handling;

    package body Names is

       procedure Show (N : Name) is
       begin
          --  for I in N'Range loop
          --     N (I) := To_Lower (N (I));
          --  end loop;
          Put_Line ("Name: " & N.all);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       N : Name := new String'("John");
    begin
       Show (N);
    end Show_Names;

Although a bit more verbose than a simple :ada:`in String`, the information in
the specification of :ada:`Show` at least gives us an indication that the
object won't be affected by the call to this subprogram. Note that this code
actually compiles if we try to modify :ada:`N.all` in the :ada:`Show`
procedure, but the post-condition fails at runtime when we do that.

(By uncommentating and building the code again, you'll see an exception being
raised at runtime when trying to change the object.)

In the postcondition above, we're using :ada:`'Old` to refer to the original
object before the subprogram call. Unfortunately, we cannot use this attribute
when dealing with
:ref:`limited private types <Adv_Ada_Limited_Private_Types>` |mdash| or limited
types in general. For example, let's change the declaration of :ada:`Name` and
have it as a limited private type instead:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names_Limited_Private
    :class: ada-expect-compile-error

    package Names is

       type Name is limited private;

       function Init (S : String) return Name;

       function Equal (N1, N2 : Name)
                       return Boolean;

       procedure Show (N : Name)
         with Post => Equal (N'Old = N);

    private

       type Name is access String;

       function Init (S : String) return Name is
         (new String'(S));

       function Equal (N1, N2 : Name)
                       return Boolean is
         (N1.all = N2.all);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    --  with Ada.Characters.Handling;
    --  use  Ada.Characters.Handling;

    package body Names is

       procedure Show (N : Name) is
       begin
          --  for I in N'Range loop
          --     N (I) := To_Lower (N (I));
          --  end loop;
          Put_Line ("Name: " & N.all);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       N : Name := Init ("John");
    begin
       Show (N);
    end Show_Names;

In this case, we have no means to indicate that a call to :ada:`Show` won't
change the internal state of the actual parameter.

.. admonition:: For further reading...

    As an alternative, we could declare a new :ada:`Constant_Name` type that
    is also limited private. If we use this type in :ada:`Show` procedure,
    we're at least indicating (in the type name) that the type is supposed to
    be constant |mdash| even though we're not directly providing means to
    actually ensure that no modifications occur in a call to the procedure.
    However, the fact that we declare this type as an access-to-constant (in
    the private part of the specification) makes it clear that a call to
    :ada:`Show` won't change the designated object.

    Let's look at the adapted code:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Parameters_As_Access_Values.Names_Constant_Limited_Private

        package Names is

           type Name is limited private;

           type Constant_Name is limited private;

           function Init (S : String) return Name;

           function To_Constant_Name
             (N : Name)
              return Constant_Name;

           procedure Show (N : Constant_Name);

        private

           type Name is
             access String;

           type Constant_Name is
             access constant String;

           function Init (S : String) return Name is
             (new String'(S));

           function To_Constant_Name
             (N : Name)
              return Constant_Name is
                (Constant_Name (N));

        end Names;

        with Ada.Text_IO; use Ada.Text_IO;

        --  with Ada.Characters.Handling;
        --  use  Ada.Characters.Handling;

        package body Names is

           procedure Show (N : Constant_Name) is
           begin
              --  for I in N'Range loop
              --     N (I) := To_Lower (N (I));
              --  end loop;
              Put_Line ("Name: " & N.all);
           end Show;

        end Names;

        with Names; use Names;

        procedure Show_Names is
           N : Name := Init ("John");
        begin
           Show (To_Constant_Name (N));
        end Show_Names;

    In this version of the source code, the :ada:`Show` procedure doesn't have
    any side-effects, as we cannot modify :ada:`N` inside the procedure.

Having the information about the effects of a subprogram call to an object is
very important: we can use this information to set expectations |mdash| and
avoid unexpected changes to an object. Also, this information can be used to
prove that a program works as expected. Therefore, whenever possible, we should
avoid access values as parameters. Instead, we can rely on appropriate
parameter modes and pass an object by reference.

There are cases, however, where the design of our application doesn't permit
replacing the access type with simple parameter modes. Whenever we have an
abstract data type encapsulated as a limited private type |mdash| such as in
the last code example |mdash|, we might have no means to avoid access values
as parameters. In this case, using the access type is of course justifiable.
We'll see such a case in the
:ref:`next section <Adv_Ada_Self_Reference_Access_Types>`.


.. _Adv_Ada_Self_Reference_Access_Types:

Self-reference
--------------

As we've discussed in the section about
:ada:`incomplete types <Adv_Ada_Incomplete_Types>`, we can use incomplete types
to create a recursive, self-referencing type. Let's revisit a code example from
that section:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Self_Reference.Linked_List_Example

    package Linked_List_Example is

       type Integer_List;

       type Next is access Integer_List;

       type Integer_List is record
          I : Integer;
          N : Next;
       end record;

    end Linked_List_Example;

Here, we're using the incomplete type :ada:`Integer_List` in the declaration of
the :ada:`Next` type, which we then use in the complete declaration of the
:ada:`Integer_List` type.

Self-references are useful, for example, to create unbounded containers |mdash|
such as the linked lists mentioned in the example above. Let's extend this code
example and partially implement a generic package for linked lists:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Self_Reference.Linked_List_Example

    generic
       type T is private;
    package Linked_Lists is

       type List is limited private;

       procedure Append_Front
          (L : in out List;
           E :        T);

       procedure Append_Rear
          (L : in out List;
           E :        T);

       procedure Show (L : List);

    private

       --  Incomplete type declaration:
       type Component;

       --  Using incomplete type:
       type List is access Component;

       type Component is record
          Value : T;
          Next  : List;
          --      ^^^^
          --   Self-reference via access type
       end record;

    end Linked_Lists;

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Linked_Lists is

       procedure Append_Front
          (L : in out List;
           E :        T)
       is
          New_First : constant List := new
            Component'(Value => E,
                       Next  => L);
       begin
          L := New_First;
       end Append_Front;

       procedure Append_Rear
          (L : in out List;
           E :        T)
       is
          New_Last : constant List := new
            Component'(Value => E,
                       Next  => null);
       begin
          if L = null then
             L := New_Last;
          else
             declare
                Last : List := L;
             begin
                while Last.Next /= null loop
                   Last := Last.Next;
                end loop;
                Last.Next := New_Last;
             end;
          end if;
       end Append_Rear;

       procedure Show (L : List) is
          Curr : List := L;
       begin
          if L = null then
             Put_Line ("[ ]");
          else
             Put ("[");
             loop
                Put (Curr.Value'Image);
                Put (" ");
                exit when Curr.Next = null;
                Curr := Curr.Next;
             end loop;
             Put_Line ("]");
          end if;
       end Show;

    end Linked_Lists;

    with Linked_Lists;

    procedure Test_Linked_List is
        package Integer_Lists is new
          Linked_Lists (T => Integer);
        use Integer_Lists;

        L : List;
    begin
        Append_Front (L, 3);
        Append_Rear (L, 4);
        Append_Rear (L, 5);
        Append_Front (L, 2);
        Append_Front (L, 1);
        Append_Rear (L, 6);
        Append_Rear (L, 7);

        Show (L);
    end Test_Linked_List;

In this example, we declare an incomplete type :ada:`Component` in the private
part of the generic :ada:`Linked_Lists` package. We use this incomplete type to
declare the access type :ada:`List`, which is then used as a self-reference in
the :ada:`Next` component of the :ada:`Component` type.

Note that we're using the :ada:`List` type
:ref:`as a parameter <Adv_Ada_Parameters_As_Access_Values>` for the
:ada:`Append_Front`, :ada:`Append_Rear` and :ada:`Show` procedures.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.1 Incomplete Type Declarations <3-10-1>`


.. _Adv_Ada_Dereferencing:

Dereferencing
-------------

In the :ref:`Introduction to Ada course <Intro_Ada_Access_Dereferencing>`, we
discussed the :ada:`.all` syntax to dereference access values:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Dereferencing.Simple_Dereferencing

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Dereferencing is

       --  Declaring access type:
       type Integer_Access is access Integer;

       --  Declaring access object:
       A1 : Integer_Access;

    begin
       A1 := new Integer;

       --  Dereferencing access value:
       A1.all := 22;

       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Dereferencing;

In this example, we declare :ada:`A1` as an access object, which allows us to
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
distinguish between assigning to access objects and assigning to actual arrays.
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

- :ada:`Arr'Size` is the size of the access object; while

- :ada:`Arr.all'Size` indicates the size of the actual array :ada:`Arr`.

In other words, the :ada:`Size` attribute is *not* implicitly dereferenced.
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
array initialization |mdash| via the allocation of the :ada:`Data` array for
the :ada:`Coefficients` type.

Note that the :ada:`Coefficients` type is defined as :ada:`access constant`.
We discuss :ref:`access-to-constant types <Adv_Ada_General_Access_Modifiers>`
in more details later on.

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

.. _Adv_Ada_Aliasing:

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


.. _Adv_Ada_General_Access_Modifiers:

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


.. _Adv_Ada_Access_Types_Aliased_Parameters:

Aliased parameters
~~~~~~~~~~~~~~~~~~

In addition to aliased objects and components, we can declare
:ref:`aliased parameters <Adv_Ada_Aliased_Parameters>`, as we already discussed
in an earlier chapter. As we mentioned there, aliased parameters are always
passed by reference, independently of the type we're using.

The parameter mode indicates which type we must use for the access type:

+------------------------+--------------------+
| Parameter mode         | Type               |
+========================+====================+
| :ada:`aliased in`      | Access-to-constant |
+------------------------+--------------------+
| :ada:`aliased out`     | Access-to-variable |
+------------------------+--------------------+
| :ada:`aliased in out`  | Access-to-variable |
+------------------------+--------------------+

Using aliased parameters in a subprogram allows us to get access to those
parameters in the body of that subprogram. Let's see an example:

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

Here, :ada:`Proc` has an :ada:`aliased in out` parameter. In :ada:`Proc`\'s
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


.. _Adv_Ada_Accessibility_Rules_Operations:

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


.. _Adv_Ada_Accessibility_Rules_Access_Values_As_Parameters:

Accessibility rules on parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the accessibility rules also apply to access values as subprogram
parameters. For example, compilation fails for this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Accessibility_Checks_Parameters
    :class: ada-expect-compile-error

    package Names is

       type Name is access all String;

       type Constant_Name is
         access constant String;

       procedure Show (N : Constant_Name);

    end Names;

    with Ada.Text_IO; use Ada.Text_IO;

    --  with Ada.Characters.Handling;
    --  use  Ada.Characters.Handling;

    package body Names is

       procedure Show (N : Constant_Name) is
       begin
          --  for I in N'Range loop
          --     N (I) := To_Lower (N (I));
          --  end loop;
          Put_Line ("Name: " & N.all);
       end Show;

    end Names;

    with Names; use Names;

    procedure Show_Names is
       S : aliased String := "John";
    begin
       Show (S'Access);
    end Show_Names;

In this case, the :ada:`S'Access` cannot be used as the actual parameter for
the :ada:`N` parameter of the :ada:`Show` procedure because it's in a deeper
level. If we allocate the string via :ada:`new`, however, the code compiles
as expected:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Levels_Rules_Introduction.Accessibility_Checks_Parameters

    with Names; use Names;

    procedure Show_Names is
       S : Name := new String'("John");
    begin
       Show (Constant_Name (S));
    end Show_Names;

This version of the code works because both object and access object have the same level.


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
only use it when you can safely assume that the access object will be discarded
before the object we had access to gets out of scope. Therefore, if this
situation isn't clear enough, it's best to avoid unchecked access. (Later in
this chapter, we'll see some of the nasty issues that arrive from creating
dangling references.) Instead, you should work on improving the software design
of your application by considering alternatives such as using containers or
encapsulating access types in well-designed abstract data types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Unchecked Access Value Creation <13-10>`


.. _Adv_Ada_Unchecked_Deallocation:

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

Previously, we learned about
:ref:`dangling references <Adv_Ada_Dangling_References>` and discussed the
effects of
:ref:`dereferencing them <Adv_Ada_Dereferencing_Dangling_References>`.
Also, we've seen the relationship between
:ref:`unchecked deallocation and dangling references <Adv_Ada_Unchecked_Deallocation_Dangling_References>`.
Ensuring that all calls to :ada:`Free` for a specific access type will never
cause dangling references can become an arduous task |mdash| if not impossible
|mdash| if those calls are located in different parts of the source code.

Although we used access types directly in the main application in many of the
previous code examples from this chapter, this approach was in fact selected
just for illustration purposes |mdash| i.e. to make the code look simpler. In
general, however, we should avoid this approach. Instead, our recommendation is
to encapsulate the access types in some form of abstraction. In this section,
we discuss design strategies for access types that take this recommendation
into account.

Abstract data type for access types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest form of abstraction is of course an abstract data type. For
example, we could declare a limited private type, which allows us to hide
the access type and to avoid copies of references that could potentially
become dangling references. (We discuss limited private types later
:ref:`in another chapter <Adv_Ada_Limited_Private_Types>`.)

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Design_Strategies.Access_Type_Abstraction

    package Access_Type_Abstraction is

       type Info is limited private;

       function To_Info (S : String) return Info;

       function To_String (Obj : Info)
                           return String;

       function Copy (Obj : Info) return Info;

       procedure Copy (To   : in out Info;
                       From :        Info);

       procedure Append (Obj : in out Info;
                         S   : String);

       procedure Reset (Obj : in out Info);

       procedure Destroy (Obj : in out Info);

    private

       type Info is access String;

    end Access_Type_Abstraction;

    with Ada.Unchecked_Deallocation;

    package body Access_Type_Abstraction is

       function To_Info (S : String) return Info is
         (new String'(S));

       function To_String (Obj : Info)
                           return String is
         (if Obj /= null then Obj.all else "");

       function Copy (Obj : Info) return Info is
         (To_Info (Obj.all));

       procedure Copy (To   : in out Info;
                       From :        Info) is
       begin
          Destroy (To);
          To := To_Info (From.all);
       end Copy;

       procedure Append (Obj : in out Info;
                         S   : String) is
          New_Info : constant Info :=
                       To_Info (To_String (Obj) & S);
       begin
          Destroy (Obj);
          Obj := New_Info;
       end Append;

       procedure Reset (Obj : in out Info) is
       begin
          Destroy (Obj);
       end Reset;

       procedure Destroy (Obj : in out Info) is
          procedure Free is
            new Ada.Unchecked_Deallocation
              (Object => String,
               Name   => Info);
       begin
          Free (Obj);
       end Destroy;

    end Access_Type_Abstraction;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_Type_Abstraction;
    use  Access_Type_Abstraction;

    procedure Main is
       Obj_1 : Info := To_Info ("hello");
       Obj_2 : Info := Copy (Obj_1);
    begin
       Put_Line ("TO_INFO / COPY");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Reset (Obj_1);
       Append (Obj_2, " world");

       Put_Line ("RESET / APPEND");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Copy (From => Obj_2,
             To   => Obj_1);

       Put_Line ("COPY");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Destroy (Obj_1);
       Destroy (Obj_2);

       Put_Line ("DESTROY");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Append (Obj_1, "hey");

       Put_Line ("APPEND");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("----------");

       Put_Line ("APPEND");
       Append (Obj_1, " there");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));

       Destroy (Obj_1);
       Destroy (Obj_2);
    end Main;

In this example, we hide an access type in the :ada:`Info` type |mdash| a
limited private type. We allocate an object of this type in the :ada:`To_Info`
function and deallocate it in the :ada:`Destroy` procedure. Also, we make
sure that the reference isn't copied in the :ada:`Copy` function |mdash|
we only copy the designated value in this function. This strategy eliminates
the possibility of dangling references, as each reference is encapsulated in
an object of :ada:`Info` type.


Controlled type for access types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous code example, the :ada:`Destroy` procedure had to be called
to deallocate the hidden access object. We could make sure that this
deallocation happens automatically by using a controlled (or limited
controlled) type. (We discuss controlled types in another chapter.)

.. todo::

    Add link to Adv_Ada_Finalization

Let's adapt the previous example and declare :ada:`Info` as a limited
controlled type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Design_Strategies.Access_Type_Limited_Controlled_Abstraction

    with Ada.Finalization;

    package Access_Type_Abstraction is

       type Info is limited private;

       function To_Info (S : String) return Info;

       function To_String (Obj : Info)
                           return String;

       function Copy (Obj : Info) return Info;

       procedure Copy (To   : in out Info;
                       From :        Info);

       procedure Append (Obj : in out Info;
                         S   :        String);

       procedure Reset (Obj : in out Info);

    private

       type String_Access is access String;

       type Info is new
         Ada.Finalization.Limited_Controlled with
          record
             Str_A : String_Access;
          end record;

       procedure Initialize (Obj : in out Info);
       procedure Finalize (Obj : in out Info);

    end Access_Type_Abstraction;

    with Ada.Unchecked_Deallocation;

    package body Access_Type_Abstraction is

       --
       --  STRING_ACCESS SUBPROGRAMS
       --

       function To_String_Access (S : String)
                                  return String_Access
       is
         (new String'(S));

       function To_String (S : String_Access)
                           return String is
         (if S /= null then S.all else "");

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => String,
            Name   => String_Access);

       procedure Copy (To   : in out String_Access;
                       From :        String_Access) is
       begin
          Free (To);
          To := To_String_Access (From.all);
       end Copy;

       procedure Append (Obj : in out String_Access;
                         S   :        String) is
          New_Str : constant String_Access :=
                      To_String_Access
                        (To_String (Obj) & S);
       begin
          Free (Obj);
          Obj := New_Str;
       end Append;

       --
       --  PRIVATE SUBPROGRAMS
       --

       procedure Initialize (Obj : in out Info) is
       begin
          Obj.Str_A := null;
       end Initialize;

       procedure Finalize (Obj : in out Info) is
       begin
          --  Put_Line ("Finalizing Info");
          Free (Obj.Str_A);
       end Finalize;

       --
       --  PUBLIC SUBPROGRAMS
       --

       function To_Info (S : String) return Info is
         (Ada.Finalization.Limited_Controlled
          with Str_A => To_String_Access (S));

       function To_String (Obj : Info)
                           return String is
         (To_String (Obj.Str_A));

       function Copy (Obj : Info) return Info is
         (To_Info (To_String (Obj.Str_A)));

       procedure Copy (To   : in out Info;
                       From :        Info) is
       begin
          Copy (To.Str_A, From.Str_A);
       end Copy;

       procedure Append (Obj : in out Info;
                         S   :        String) is
       begin
          Append (Obj.Str_A, S);
       end Append;

       procedure Reset (Obj : in out Info) is
       begin
          Free (Obj.Str_A);
       end Reset;

    end Access_Type_Abstraction;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_Type_Abstraction;
    use  Access_Type_Abstraction;

    procedure Main is
       Obj_1 : Info := To_Info ("hello");
       Obj_2 : Info := Copy (Obj_1);
    begin
       Put_Line ("TO_INFO / COPY");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Reset (Obj_1);
       Append (Obj_2, " world");

       Put_Line ("RESET / APPEND");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Copy (From => Obj_2,
             To   => Obj_1);

       Put_Line ("COPY");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Reset (Obj_1);
       Reset (Obj_2);

       Put_Line ("RESET");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("Obj_2 : "
                 & To_String (Obj_2));
       Put_Line ("----------");

       Append (Obj_1, "hey");

       Put_Line ("APPEND");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
       Put_Line ("----------");

       Put_Line ("APPEND");
       Append (Obj_1, " there");
       Put_Line ("Obj_1 : "
                 & To_String (Obj_1));
    end Main;

Of course, because we're using the
:ada:`Limited_Controlled` type from the :ada:`Ada.Finalization` package,
we had to adapt the prototype of the subprograms from the
:ada:`Access_Type_Abstraction`. In this version of the code, we only have
the allocation taking place in the :ada:`To_Info` procedure, but we don't have
a :ada:`Destroy` procedure for deallocation: this call was moved to the
:ada:`Finalize` procedure.

Since objects of the :ada:`Info` type |mdash| such as :ada:`Obj_1` in the
:ada:`Show_Access_Type_Abstraction` procedure |mdash| are now controlled, the
:ada:`Finalize` procedure is automatically called when they get out of scope.
In this procedure, which we override for the :ada:`Info` type, we perform the
deallocation of the internal access object :ada:`Str_A`. (You may uncomment the
calls to :ada:`Put_Line` in the body of the :ada:`Initialize` and
:ada:`Finalize` subprograms to confirm that these subprograms are called in the
background.)


Access types to access types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. _Adv_Ada_Access_To_Subprograms:

Access to subprograms
---------------------

So far in this chapter, we focused mainly on access-to-objects. However, we can
use access types to subprograms. This is the topic of this section.

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

In the designated profile of the access type declarations, we list all the
parameters that we expect in the subprogram.

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

Before we use this package, let's implement a simple procedure that we'll use
later on:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    procedure Add_Ten (I : in out Integer);

    procedure Add_Ten (I : in out Integer) is
    begin
       I := I + 10;
    end Add_Ten;

.. _Adv_Ada_Access_To_Subprogram_Params_Example:

Now, we can get access to a subprogram by using the :ada:`Access` attribute and
pass it as an actual parameter:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

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


Objects of access-to-subprogram type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, the :ada:`Proc` procedure had a parameter of
access-to-subprogram type. In addition to parameters, we can of course declare
*objects* of access-to-subprogram types as well. For example, we can extend
our previous test application and declare an object :ada:`P` of
access-to-subprogram type. Before we do so, however, let's implement another
small procedure that we'll use later on:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    procedure Add_Twenty (I : in out Integer);

    procedure Add_Twenty (I : in out Integer) is
    begin
       I := I + 20;
    end Add_Twenty;

In addition to :ada:`Add_Ten`, we've implemented the :ada:`Add_Twenty`
procedure, which we use in our extended test application:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    with Access_To_Subprogram_Params;
    use  Access_To_Subprogram_Params;

    with Add_Ten;
    with Add_Twenty;

    procedure Show_Access_To_Subprograms is
       P        : Access_To_Procedure;
       Some_Int : Integer := 0;
    begin
       P := Add_Ten'Access;
       --           ^ Getting access to Add_Ten
       --             procedure and assigning it
       --             to P

       Proc (P);
       --    ^ Passing access-to-subprogram as an
       --      actual parameter

       P (Some_Int);
       --  ^ Using access-to-subprogram object in a
       --    subprogram call

       P := Add_Twenty'Access;
       --              ^ Getting access to Add_Twenty
       --                procedure and assigning it
       --                to P

       Proc (P);
       P (Some_Int);
    end Show_Access_To_Subprograms;

In the :ada:`Show_Access_To_Subprograms` procedure,
we see the declaration of our access-to-subprogram object :ada:`P` (of
:ada:`Access_To_Procedure` type). We get access to the :ada:`Add_Ten` procedure
and assign it to :ada:`P`, and we then do the same for the :ada:`Add_Twenty`
procedure.

We can use an access-to-subprogram object either as the actual parameter of a
subprogram call, or in a subprogram call. In the code example, we're passing
:ada:`P` as the actual parameter of the :ada:`Proc` procedure in the
:ada:`Proc (P)` calls. Also, we're calling the subprogram assigned to
(designated by the current value of) :ada:`P` in the :ada:`P (Some_Int)` calls.


Components of access-to-subprogram type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to declaring subprogram parameters and objects of
access-to-subprogram types, we can declare components of these types. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

       type Access_To_Procedure_Array is
         array (Positive range <>) of
           Access_To_Procedure;

       type Access_To_Function_Array is
         array (Positive range <>) of
           Access_To_Function;

       type Rec_Access_To_Procedure is record
          AP : Access_To_Procedure;
       end record;

       type Rec_Access_To_Function is record
          AF : Access_To_Function;
       end record;

    end Access_To_Subprogram_Types;

Here, the access-to-procedure type :ada:`Access_To_Procedure` is used as a
component of the array type :ada:`Access_To_Procedure_Array`  and the record
type :ada:`Rec_Access_To_Procedure`. Similarly, the access-to-function type
:ada:`Access_To_Function` type is used as a component of the array type
:ada:`Access_To_Function_Array` and the record type
:ada:`Rec_Access_To_Function`.

Let's see two test applications using these types. First, let's use the
:ada:`Access_To_Procedure_Array` array type in a test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    with Add_Ten;
    with Add_Twenty;

    procedure Show_Access_To_Subprograms is
       PA : constant
              Access_To_Procedure_Array (1 .. 2) :=
                (Add_Ten'Access,
                 Add_Twenty'Access);

       Some_Int : Integer := 0;
    begin
       Put_Line ("Some_Int: " & Some_Int'Image);

       for I in PA'Range loop
          PA (I) (Some_Int);
          Put_Line ("Some_Int: " & Some_Int'Image);
       end loop;
    end Show_Access_To_Subprograms;

Here, we declare the :ada:`PA` array and use the access to the :ada:`Add_Ten`
and :ada:`Add_Twenty` procedures as its components. We can call any of these
procedures by simply specifying the index of the component, e.g.
:ada:`PA (2)`. Once we specify the procedure we want to use, we simply pass
the parameters, e.g.: :ada:`PA (2) (Some_Int)`.

Now, let's use the :ada:`Rec_Access_To_Procedure` record type in a test
application:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    with Add_Ten;
    with Add_Twenty;

    procedure Show_Access_To_Subprograms is
       RA       : Rec_Access_To_Procedure;
       Some_Int : Integer := 0;
    begin
       Put_Line ("Some_Int: " & Some_Int'Image);

       RA := (AP => Add_Ten'Access);
       RA.AP (Some_Int);
       Put_Line ("Some_Int: " & Some_Int'Image);

       RA := (AP => Add_Twenty'Access);
       RA.AP (Some_Int);
       Put_Line ("Some_Int: " & Some_Int'Image);
    end Show_Access_To_Subprograms;

Here, we declare two record aggregates where we specify the :ada:`AP`
component, e.g.: :ada:`(AP => Add_Ten'Access)`, which indicates the
access-to-subprogram we want to use. We can call the subprogram by simply
accessing the :ada:`AP` component, i.e.: :ada:`RA.AP`.


Access-to-subprogram as discriminant types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you might expect, we can use access-to-subprogram types when declaring
discriminants. In fact, when we were talking about
:ref:`discriminants as access values <Adv_Ada_Discriminants_As_Access_Values>`
earlier on, we used access-to-object types in our code examples, but we could
have used access-to-subprogram types as well. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    package Custom_Processing is

       --  Declaring an access type:
       type Integer_Processing is
         access procedure (I : in out Integer);

       --  Declaring a discriminant with this
       --  access type:
       type Rec (IP : Integer_Processing) is
         private;

       procedure Init (R     : in out Rec;
                       Value :        Integer);

       procedure Process (R : in out Rec);

       procedure Show (R : Rec);

    private

       type Rec (IP : Integer_Processing) is
       record
          I : Integer := 0;
       end record;

    end Custom_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Processing is

       procedure Init (R     : in out Rec;
                       Value :        Integer) is
       begin
          R.I := Value;
       end Init;

       procedure Process (R : in out Rec) is
       begin
          R.IP (R.I);
          --  ^^^^^^
          --  Calling procedure that we specified as
          --  the record's discriminant
       end Process;

       procedure Show (R : Rec) is
       begin
          Put_Line ("R.I = "
                    & Integer'Image (R.I));
       end Show;

    end Custom_Processing;

In this example, we declare the access-to-subprogram type
:ada:`Integer_Processing`, which we use as the :ada:`IP` discriminant of the
:ada:`Rec` type. In the :ada:`Process` procedure, we call the :ada:`IP`
procedure that we specified as the record's discriminant (:ada:`R.IP (R.I)`).

Before we look at a test application for this package, let's implement
another small procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    procedure Mult_Two (I : in out Integer);

    procedure Mult_Two (I : in out Integer) is
    begin
       I := I * 2;
    end Mult_Two;

Now, let's look at the test application:

.. code:: ada run_button main=show_access_to_subprogram_discriminants.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Ada.Text_IO;       use Ada.Text_IO;

    with Custom_Processing; use Custom_Processing;

    with Add_Ten;
    with Mult_Two;

    procedure Show_Access_To_Subprogram_Discriminants
    is

       R_Add_Ten  : Rec (IP => Add_Ten'Access);
       --                ^^^^^^^^^^^^^^^^^^^^
       --       Using access-to-subprogram as a
       --       discriminant

       R_Mult_Two : Rec (IP => Mult_Two'Access);
       --                ^^^^^^^^^^^^^^^^^^^^^
       --       Using access-to-subprogram as a
       --       discriminant

    begin
       Init (R_Add_Ten,  1);
       Init (R_Mult_Two, 2);

       Put_Line ("---- R_Add_Ten ----");
       Show (R_Add_Ten);

       Put_Line ("Calling Process procedure...");
       Process (R_Add_Ten);
       Show (R_Add_Ten);

       Put_Line ("---- R_Mult_Two ----");
       Show (R_Mult_Two);

       Put_Line ("Calling Process procedure...");
       Process (R_Mult_Two);
       Show (R_Mult_Two);
    end Show_Access_To_Subprogram_Discriminants;

In this procedure, we declare the :ada:`R_Add_Ten` and :ada:`R_Mult_Two` of
:ada:`Rec` type and specify the access to :ada:`Add_Ten` and :ada:`Mult_Two`,
respectively, as the :ada:`IP` discriminant. The procedure we specified here
is then called inside a call to the :ada:`Process` procedure.


Access-to-subprograms as formal parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use access-to-subprograms types when declaring formal parameters. For
example, let's revisit the :ada:`Custom_Processing` package from the previous
section and convert it into a generic package.

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    generic
       type T is private;

       --
       --  Declaring formal access-to-subprogram
       --  type:
       --
       type T_Processing is
         access procedure (Element : in out T);

       --
       --  Declaring formal access-to-subprogram
       --  parameter:
       --
       Proc : T_Processing;

       with function Image_T (Element : T)
                              return String;
    package Gen_Custom_Processing is

       type Rec is private;

       procedure Init (R     : in out Rec;
                       Value :        T);

       procedure Process (R : in out Rec);

       procedure Show (R : Rec);

    private

       type Rec is record
          Comp : T;
       end record;

    end Gen_Custom_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Gen_Custom_Processing is

       procedure Init (R     : in out Rec;
                       Value :        T) is
       begin
          R.Comp := Value;
       end Init;

       procedure Process (R : in out Rec) is
       begin
          Proc (R.Comp);
       end Process;

       procedure Show (R : Rec) is
       begin
          Put_Line ("R.Comp = "
                    & Image_T (R.Comp));
       end Show;

    end Gen_Custom_Processing;

In this version of the procedure, instead of declaring :ada:`Proc` as a
discriminant of the :ada:`Rec` record, we're declaring it as a formal parameter
of the :ada:`Gen_Custom_Processing` package. Also, we're declaring an
access-to-subprogram type (:ada:`T_Processing`) as a formal parameter. (Note
that, in contrast to these two parameters that we've just mentioned,
:ada:`Image_T` is not a formal access-to-subprogram parameter: it's actually
just a formal subprogram.)

We then instantiate the :ada:`Gen_Custom_Processing` package in our test
application:

.. code:: ada run_button main=show_access_to_subprogram_as_formal_parameter.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Access_To_Subprograms.Access_To_Subprogram_Types

    with Gen_Custom_Processing;

    with Add_Ten;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure
      Show_Access_To_Subprogram_As_Formal_Parameter
    is
       type Integer_Processing is
         access procedure (I : in out Integer);

       package Custom_Processing is new
         Gen_Custom_Processing
           (T            => Integer,
            T_Processing => Integer_Processing,
            --              ^^^^^^^^^^^^^^^^^^
            --              access-to-subprogram type
            Proc         => Add_Ten'Access,
            --              ^^^^^^^^^^^^^^^^^^
            --              access-to-subprogram
            Image_T      => Integer'Image);
       use Custom_Processing;

       R_Add_Ten  : Rec;

    begin
       Init (R_Add_Ten,  1);

       Put_Line ("---- R_Add_Ten ----");
       Show (R_Add_Ten);

       Put_Line ("Calling Process procedure...");
       Process (R_Add_Ten);
       Show (R_Add_Ten);
    end Show_Access_To_Subprogram_As_Formal_Parameter;

Here, we instantiate the :ada:`Gen_Custom_Processing` package as
:ada:`Custom_Processing` and specify the access-to-subprogram type and the
access-to-subprogram.


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

In general, the accessibility rules that we discussed
:ref:`previously for access-to-objects <Adv_Ada_Accessibility_Levels_Intro>`
also apply to access-to-subprograms. In this section, we discuss minor
differences when applying those rules to access-to-subprograms.

In our discussion about accessibility rules, we've looked into
:ref:`accessibility levels <Adv_Ada_Accessibility_Levels>` and
the :ref:`accessibility rules <Adv_Ada_Accessibility_Rules>` that are based on
those levels. The same accessibility rules apply to access-to-subprograms.
:ref:`As we said previously  <Adv_Ada_Accessibility_Rules_Operations>`,
operations targeting objects at a *less-deep* level are illegal, as it's the
case for subprograms as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Rules_Access_To_Subprograms.Access_To_Subprogram_Accessibility_Error_Less_Deep
    :class: ada-expect-compile-error

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    procedure Show_Access_To_Subprogram_Error is
       Func : Access_To_Function;

       Value : Integer := 0;
    begin
       declare
          function Add_One (I : Integer)
                            return Integer is
            (I + 1);
       begin
          Func := Add_One'Access;
          --  This assignment is illegal because the
          --  Access_To_Function type is less deep
          --  than Add_One.
       end;

       Put_Line ("Value: " & Value'Image);
       Value := Func (Value);
       Put_Line ("Value: " & Value'Image);
    end Show_Access_To_Subprogram_Error;

Obviously, we can correct this error by putting the :ada:`Add_One` function
at the same level as the :ada:`Access_To_Function` type, i.e. at library
level:

.. code:: ada run_button main=show_access_to_subprogram_error.adb project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Rules_Access_To_Subprograms.Access_To_Subprogram_Accessibility_Error_Less_Deep_Fix

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

    function Add_One (I : Integer) return Integer;

    function Add_One (I : Integer) return Integer is
    begin
       return I + 1;
    end Add_One;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    with Add_One;

    procedure Show_Access_To_Subprogram_Error is
       Func : Access_To_Function;

       Value : Integer := 0;
    begin
       Func := Add_One'Access;

       Put_Line ("Value: " & Value'Image);
       Value := Func (Value);
       Put_Line ("Value: " & Value'Image);
    end Show_Access_To_Subprogram_Error;

As a recommendation, resolving accessibility issues in the case of
access-to-subprograms is best done by refactoring the subprograms of your
source code |mdash| for example, moving subprograms to a different level.

Unchecked Access
~~~~~~~~~~~~~~~~

Previously, we discussed about the
:ref:`Unchecked_Access attribute <Adv_Ada_Unchecked_Access>`, which we can use
to circumvent accessibility issues in specific cases for access-to-objects. We
also said in that section that this attribute only exists for objects, not for
subprograms. We can use the previous example to illustrate this limitation:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Rules_Access_To_Subprograms.Access_To_Subprogram_Accessibility_Error_Same_Lifetime
    :class: ada-expect-compile-error

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    procedure Show_Access_To_Subprogram_Error is
       Func : Access_To_Function;

       function Add_One (I : Integer)
                return Integer is
         (I + 1);

       Value : Integer := 0;
    begin
       Func := Add_One'Access;

       Put_Line ("Value: " & Value'Image);
       Value := Func (Value);
       Put_Line ("Value: " & Value'Image);
    end Show_Access_To_Subprogram_Error;

When we analyze the :ada:`Show_Access_To_Subprogram_Error` procedure, we see
that the :ada:`Func` object and the :ada:`Add_One` function have the same
lifetime. Therefore, in this very specific case, we could safely assign
:ada:`Add_One'Access` to :ada:`Func` and call :ada:`Func` for :ada:`Value`.
Due to the accessibility rules, however, this assignment is illegal.
(Obviously, the accessibility issue here is that the
:ada:`Access_To_Function` type has a potentially longer lifetime.)

In the case of access-to-objects, we could use :ada:`Unchecked_Access` to
enforce assignments that we consider safe after careful analysis. However,
because this attribute isn't available for access-to-subprograms, the best
solution is to move the subprogram to a level that allows the assignment to
be legal, as we said before.

.. admonition:: In the GNAT toolchain

    GNAT offers an equivalent for :ada:`Unchecked_Access` that can be used for
    subprograms: the :ada:`Unrestricted_Access` attribute. Note, however, that
    this attribute is not portable.

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Access_Types.Accessibility_Rules_Access_To_Subprograms.Unrestricted_Access

        package Access_To_Subprogram_Types is

           type Access_To_Procedure is
             access procedure (I : in out Integer);

           type Access_To_Function is
             access function (I : Integer) return Integer;

        end Access_To_Subprogram_Types;

        with Ada.Text_IO; use Ada.Text_IO;

        with Access_To_Subprogram_Types;
        use  Access_To_Subprogram_Types;

        procedure Show_Access_To_Subprogram_Error is
           Func : Access_To_Function;

           function Add_One (I : Integer)
                    return Integer is
             (I + 1);

           Value : Integer := 0;
        begin
           Func := Add_One'Unrestricted_Access;
           --              ^^^^^^^^^^^^^^^^^^^
           --       Allowing access to local function

           Put_Line ("Value: " & Value'Image);
           Value := Func (Value);
           Put_Line ("Value: " & Value'Image);
        end Show_Access_To_Subprogram_Error;

    As we can see, the :ada:`Unrestricted_Access` attribute can be safely used
    in this specific case to circumvent the accessibility rule limitation.


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
