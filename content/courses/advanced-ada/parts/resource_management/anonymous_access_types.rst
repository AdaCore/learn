Anonymous Access Types
======================

.. include:: ../../../global.txt

.. _Adv_Ada_Anonymous_Access_Types:

Named and Anonymous Access Types
--------------------------------

The previous chapter dealt with access type declarations such as this one:

.. code-block:: ada

   type Integer_Access is access all Integer;

   procedure Add_One (A : Integer_Access);

In addition to named access type declarations such as the one in this example,
Ada also supports anonymous access types, which, as the name implies, don't
have an actual type declaration.

To declare an access object of anonymous type, we just specify the subtype of
the object or subprogram we want to have access to. For example:

.. code-block:: ada

   procedure Add_One (A : access Integer);

When we compare this example with the previous one, we see that the declaration
:ada:`A : Integer_Access` becomes :ada:`A : access Integer`. Here,
:ada:`access Integer` is the anonymous access type declaration, and :ada:`A` is
an access object of this anonymous type.

To be more precise, :ada:`A : access Integer` is an
:ref:`access parameter <Adv_Ada_Anonymous_Access_Parameter>` and it's
specifying an
:ref:`anonymous access-to-object type <Adv_Ada_Anonymous_Access_To_Object_Types>`.
Another flavor of anonymous access types are
:ref:`anonymous access-to-subprograms <Adv_Ada_Anonymous_Access_To_Subprograms>`.
We discuss all these topics in more details later.

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_Types.Simple_Anonymous_Access_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Anonymous_Access_Types is
       I_Var : aliased Integer;

       A     : access Integer;
       --      ^ Anonymous access type
    begin
       A := I_Var'Access;
       --   ^ Assignment to object of
       --     anonymous access type.

       A.all := 22;

       Put_Line ("A.all: " & Integer'Image (A.all));
    end Show_Anonymous_Access_Types;

Here, :ada:`A` is an access object whose value is initialized with the access
to :ada:`I_Var`. Because the declaration of :ada:`A` includes the declaration
of an anonymous access type, we don't declare an extra :ada:`Integer_Access`
type, as we did in previous code examples.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


Relation to named types
~~~~~~~~~~~~~~~~~~~~~~~

Anonymous access types were not part of the first version of the Ada standard,
which only had support for named access types. They were introduced later to
cover some use-cases that were difficult |mdash| or even impossible |mdash|
with access types.

In this sense, anonymous access types aren't just access types without names.
Certain accessibility rules for anonymous access types are a bit less strict.
In those cases, it might be interesting to consider using them instead of named
access types.

In general, however, we should only use anonymous access types in those
specific cases where using named access types becomes too cumbersome. As a
general recommendation, we should give preference to named access types
whenever possible. (Anonymous access-to-object types have
:ref:`drawbacks that we discuss later <Adv_Ada_Drawbacks_Anonymous_Access_To_Object_Types>`.)


Benefits of anonymous access types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the main benefits of anonymous access types is their flexibility:
since there isn't an explicit access type declaration associated with them,
we only have to worry about the subtype :ada:`S` we intend to access.

Also, as long as the subtype :ada:`S` in a declaration :ada:`access S` is
always the same, no conversion is needed between two access objects of that
anonymous type, and the :ada:`S'Access` attribute always works.

Let's see an example:

.. code:: ada run_button main=show_anonymous_access_types.adb project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_Types.Anonymous_Access_Object_Assignment

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show (Name : String;
                    V    : access Integer) is
    begin
       Put_Line (Name & ".all: "
                 & Integer'Image (V.all));
    end Show;

    with Show;

    procedure Show_Anonymous_Access_Types is
       I_Var : aliased Integer;
       A     : access Integer;
       B     : access Integer;
    begin
       A := I_Var'Access;
       B := A;

       A.all := 22;

       Show ("A", A);
       Show ("B", B);
    end Show_Anonymous_Access_Types;

In this example, we have two access objects :ada:`A` and :ada:`B`. Since
they're objects of anonymous access types that refer to the same subtype
:ada:`Integer`, we can assign :ada:`A` to :ada:`B` without a type conversion,
and pass those access objects as an argument to the :ada:`Show` procedure.

(Note that the use of an access parameter in the :ada:`Show` procedure is for
demonstration purpose only: a simply :ada:`Integer` as the type of this input
parameter would have been more than sufficient to implement the procedure.
Actually, in this case, avoiding the access parameter would be the recommended
approach in terms of clean Ada software design.)

In contrast, if we had used named type declarations, the code would be more
complicated and more limited:

.. code:: ada run_button main=show_anonymous_access_types.adb project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_Types.Anonymous_Access_Object_Assignment

    package Aux is

       type Integer_Access is access all Integer;

       procedure Show (Name : String;
                       V    : Integer_Access);

    end Aux;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Aux is

       procedure Show (Name : String;
                       V    : Integer_Access) is
       begin
          Put_Line (Name & ".all: "
                    & Integer'Image (V.all));
       end Show;

    end Aux;

    with Aux; use Aux;

    procedure Show_Anonymous_Access_Types is
       --  I_Var : aliased Integer;

       A : Integer_Access;
       B : Integer_Access;
    begin
       --  A := I_Var'Access;
       --       ^ ERROR: non-local pointer cannot
       --                point to local object.

       A := new Integer;
       B := A;

       A.all := 22;

       Show ("A", A);
       Show ("B", B);
    end Show_Anonymous_Access_Types;

Here, apart from the access type declaration (:ada:`Integer_Access`), we had to
make two adaptations to convert the previous code example:

1. We had to move the :ada:`Show` procedure to a package (which we simply
   called :ada:`Aux`) because of the access type declaration.

2. Also, we had to allocate an object for :ada:`A` instead of retrieving the
   access attribute of :ada:`I_Var` because we cannot use a pointer to a local
   object in the assignment to a non-local pointer, as indicate in the
   comments.

This restriction regarding non-local pointer assignments is an example of the
stricter accessibility rules that apply to named access types. As
mentioned earlier, the :ada:`S'Access` attribute always works when we use
anonymous access types |mdash| this is not always the case for named access
types.

.. admonition:: Important

    As mentioned earlier, if we want to use two access objects in an operation,
    the rule says that the subtype :ada:`S` of the anonymous type used in their
    corresponding declaration must match. In the following example, we can see
    how this rule works:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_Types.Anonymous_Access_Subtype_Error
        :class: ada-expect-compile-error

        procedure Show_Anonymous_Access_Subtype_Error is
           subtype Integer_1_10 is Integer range 1 .. 10;

           I_Var : aliased Integer;
           A     : access Integer := I_Var'Access;
           B     : access Integer_1_10;
        begin
           A := I_Var'Access;

           B := A;
           --  ^ ERROR: subtype doesn't match!

           B := I_Var'Access;
           --  ^ ERROR: subtype doesn't match!
        end Show_Anonymous_Access_Subtype_Error;

   Even though :ada:`Integer_1_10` is a subtype of :ada:`Integer`, we cannot
   assign :ada:`A` to :ada:`B` because the subtype that their access type
   declarations refer to |mdash| :ada:`Integer` and :ada:`Integer_1_10`,
   respectively |mdash| doesn't match. The same issue occurs when
   retrieving the access attribute of :ada:`I_Var` in the assignment to
   :ada:`B`.

The later sections on
:ref:`anonymous access-to-object type <Adv_Ada_Anonymous_Access_To_Object_Types>`
and
:ref:`anonymous access-to-subprograms <Adv_Ada_Anonymous_Access_To_Subprograms>`
cover more specific details on anonymous access types.


.. _Adv_Ada_Anonymous_Access_To_Object_Types:

Anonymous Access-To-Object Types
--------------------------------

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`

.. todo::

    Complete section!


.. _Adv_Ada_Drawbacks_Anonymous_Access_To_Object_Types:


Drawbacks of Anonymous Access-To-Object Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. _Adv_Ada_Anonymous_Access_Discriminants:

Access discriminants
--------------------

Previously, we've discussed
:ref:`discriminants as access values <Adv_Ada_Discriminants_As_Access_Values>`.
In that section, we only used named access types. Now, in this section, we see
how to use anonymous access types as discriminants. This feature is also known
as *access discriminants* and it provides some flexibility that can be
interesting in terms of software design, as we'll discuss later.

Let's start with an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Custom_Recs

    package Custom_Recs is

       --  Declaring a discriminant with an anonymous
       --  access type:
       type Rec (IA : access Integer) is record
          I : Integer := IA.all;
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

    procedure Show_Access_Discriminants is
       I  : aliased Integer := 10;
       R  : Rec (I'Access);
    begin
       Show (R);

       I   := 20;
       R.I := 30;
       Show (R);
    end Show_Access_Discriminants;

In this example, we use an anonymous access type for the discriminant in the
declaration of the :ada:`Rec` type of the :ada:`Custom_Recs` package. In the
:ada:`Show_Access_Discriminants` procedure, we declare :ada:`R` and provide
access to the local :ada:`I` integer.

Similarly, we can use unconstrained designated subtypes:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Persons

    package Persons is

       --  Declaring a discriminant with an anonymous
       --  access type whose designated subtype is
       --  unconstrained:
       type Person (Name : access String) is record
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
       S : aliased String := "John";
       P : Person (S'Access);
    begin
       P.Age := 30;
       Show (P);
    end Show_Person;

In this example, for the discriminant of the :ada:`Person` type, we use an
anonymous access type whose designated subtype is unconstrained. In the
:ada:`Show_Person` procedure, we declare the :ada:`P` object and provide
access to the :ada:`S` string.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.7 Discriminants <3-7>`
    - :arm22:`3.10.2 Operations of Access Types <3-10-2>`


Default Value of Access Discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In contrast to named access types, we cannot use a default value for the
access discriminant of a non-limited type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Custom_Recs
    :class: ada-expect-compile-error

    package Custom_Recs is

       --  Declaring a discriminant with an anonymous
       --  access type and a default value:
       type Rec (IA : access Integer :=
                        new Integer'(0)) is
       record
          I : Integer := IA.all;
       end record;

       procedure Show (R : Rec);

    end Custom_Recs;

However, if we change the type declaration to be a limited type, having a
default value for the access discriminant is OK:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Custom_Recs

    package Custom_Recs is

       --  Declaring a discriminant with an anonymous
       --  access type and a default value:
       type Rec (IA : access Integer :=
                        new Integer'(0)) is limited
       record
          I : Integer := IA.all;
       end record;

       procedure Show (R : Rec);

    end Custom_Recs;

Note that, if we don't provide a value for the access discriminant when
declaring an object :ada:`R`, the default value is allocated (via :ada:`new`)
during :ada:`R`\'s creation.

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Custom_Recs

    with Custom_Recs; use Custom_Recs;

    procedure Show_Access_Discriminants is
       R : Rec;
       --  ^^^
       --  This triggers "new Integer'(0)", so an
       --  integer object is allocated and stored in
       --  the R.IA discriminant.
    begin
       Show (R);

       --  R gets out of scope here, and the object
       --  allocated via new hasn't been deallocated.
    end Show_Access_Discriminants;

In this case, the allocated object won't be deallocated when :ada:`R` gets out
of scope!


Benefits of Access Discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Access discriminants have the same benefits that we've already seen
earlier while discussing
:ref:`discriminants as access values <Adv_Ada_Discriminants_As_Access_Values>`.
An additional benefit is its extended flexibility: access discriminants are
compatible with any access :ada:`T'Access`, as long as :ada:`T` is of the
designated subtype.

Consider the following example using the named access type
:ada:`Access_String`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Persons
    :class: ada-expect-compile-error

    package Persons is

       type Access_String is access all String;

       --  Declaring a discriminant with a named
       --  access type:
       type Person (Name : Access_String) is record
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
       S : aliased String := "John";
       P : Person (S'Access);
       --          ^^^^^^^^ ERROR: cannot use local
       --                          object
       --
       --  We can, however, allocate the string via
       --  new:
       --
       --  S : Access_String := new String'("John");
       --  P : Person (S);
    begin
       P.Age := 30;
       Show (P);
    end Show_Person;

This code doesn't compile because we cannot have a non-local pointer
(:ada:`Access_String`) pointing to the local object :ada:`S`. The only way
to make this work is by allocating the string via :ada:`new`
(i.e.: :ada:`S : Access_String := new String`).

However, if we use an access discriminant in the declaration of :ada:`Person`,
the code compiles fine:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Persons

    package Persons is

       --  Declaring a discriminant with an anonymous
       --  access type:
       type Person (Name : access String) is record
          Age : Integer;
       end record;

       procedure Show (P : Person);

    end Persons;

    with Persons; use Persons;

    procedure Show_Person is
       S : aliased String := "John";
       P : Person (S'Access);
       --          ^^^^^^^^ OK

       --  Allocating the string via new and using it
       --  in P's declaration is OK as well, but we
       --  should manually deallocate it before S
       --  gets out of scope:
       --
       --  S : access String := new String'("John");
       --  P : Person (S);
    begin
       P.Age := 30;
       Show (P);
    end Show_Person;

In this case, getting access to the local object :ada:`S` and using it for
:ada:`P`\'s discriminant is perfectly fine.


Preventing dangling pointers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the usual rules that prevent dangling pointers still apply here.
This ensures that we can safely use access discriminants. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_Discriminants.Persons
    :class: ada-expect-compile-error

    with Persons; use Persons;

    procedure Show_Person is

       function Local_Init return Person is
          S : aliased String := "John";
       begin
          return (Name => S'Access, Age => 30);
          --      ^^^^^^^^^^^^^^^^
          --      ERROR: dangling reference!
       end Local_Init;

       P : Person := Local_Init;
    begin
       Show (P);
    end Show_Person;

In this example, compilation fails in the :ada:`Local_Init` function when
trying to return an object of :ada:`Person` type because :ada:`S'Access`
would be a dangling reference.


.. _Adv_Ada_Self_Reference_Anonymous_Access_Types:

Self-reference
--------------

Previously, we've seen that we can declare
:ref:`self-references <Adv_Ada_Self_Reference_Access_Types>` using named access
types. We can do the same with anonymous access types. Let's revisit the code
example that implements linked lists:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Self_Reference.Linked_List_Example

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

       type Component is record
          Next  : access Component;
          --      ^^^^^^^^^^^^^^^^
          --       Self-reference
          --
          --       (Note that we haven't finished the
          --       declaration of the "Component" type
          --       yet, but we're already referring to
          --       it.)

          Value : T;
       end record;

       type List is access all Component;

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
                   Last := List (Last.Next);
                   --      ^^^^
                   --   type conversion:
                   --      "access Component" to
                   --      "List"
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

Here, in the declaration of the :ada:`Component` type (in the private part of
the generic :ada:`Linked_Lists` package), we declare :ada:`Next` as an
anonymous access type that refers to the :ada:`Component` type. (Note that
at this point, we haven't finished the declaration of the :ada:`Component`
type yet, but we're already using it as the designated subtype of an anonymous
access type.) Then, we declare :ada:`List` as a general access type (with
:ada:`Component` as the designated subtype).

It's worth mentioning that the :ada:`List` type and the anonymous
:ada:`access Component` type aren't the same type, although they share the same
designated subtype. Therefore, in the implementation of the :ada:`Append_Rear`
procedure, we have to use type conversion to convert from the anonymous
:ada:`access Component` type to the (named) :ada:`List` type.


.. _Adv_Ada_Mutually_Dependent_Types_Using_Anonymous_Access_Types:

Mutually dependent types using anonymous access types
-----------------------------------------------------

In the section on
:ref:`mutually dependent types <Adv_Ada_Mutually_Dependent_Types>`, we've seen
a code example that was using named access types. We could now rewrite it using
anonymous access types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_Types.Mutually_Dependent_Anonymous_Access_Types

    package Mutually_Dependent is

       type T2;

       type T1 is record
          B : access T2;
       end record;

       type T2 is record
          A : access T1;
       end record;

    end Mutually_Dependent;

In this example, :ada:`T1` and :ada:`T2` are mutually dependent types. We're
using anonymous access types in the declaration of the :ada:`B` and :ada:`A`
components.


.. _Adv_Ada_Anonymous_Access_Parameter:

Anonymous Access parameters
---------------------------


Interfacing To Other Languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


User-Defined References
-----------------------

:ref:`Implicit dereferencing <Adv_Ada_Implicit_Dereferencing>`
isn't limited to the contexts that Ada supports by
default: we can also add implicit dereferencing to our own types by using the
:ada:`Implicit_Dereference` aspect.

To do this, we have to declare:

- a reference type, where we use the :ada:`Implicit_Dereference` aspect to
  specify the reference discriminant, which is the record discriminant that
  will be dereferenced; and

- a reference object, which contains an access value that will be dereferenced.

Also, for the reference type, we have to:

- specify the reference discriminant as an
  :ref:`access discriminant <Adv_Ada_Anonymous_Access_Discriminants>`; and

- indicate the name of the reference discriminant when specifying the
  :ada:`Implicit_Dereference` aspect.

Let's see a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.Simple_User_Defined_References

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_User_Defined_Reference is

       type Id_Number is record
          Id : Positive;
       end record;

       --
       --  Reference type:
       --
       type Id_Ref (Ref : access Id_Number) is
       --           ^ reference discriminant
         null record
           with Implicit_Dereference => Ref;
         --                             ^^^
         --               name of the reference
         --               discriminant

       --
       --  Access value:
       --
       I : constant access Id_Number :=
             new Id_Number'(Id => 42);

       --
       --  Reference object:
       --
       R : Id_Ref (I);
    begin
       Put_Line ("ID: "
                 & Positive'Image (R.Id));
       --                          ^ Equivalent to:
       --                              R.Ref.Id
       --                            or:
       --                             R.Ref.all.Id
    end Show_User_Defined_Reference;

Here, we declare a simple record type (:ada:`Id_Number`) and a corresponding
reference type (:ada:`Id_Ref`). Note that:

- the reference discriminant :ada:`Ref` has an access to the :ada:`Id_Number`
  type; and

- we indicate this reference discriminant in the :ada:`Implicit_Dereference`
  aspect.

Then, we declare an access value (the :ada:`I` constant) and use it for the
:ada:`Ref` discriminant in the declaration of the reference object :ada:`R`.

Finally, we implicitly dereference :ada:`R` and access the :ada:`Id` component
by simply writing :ada:`R.Id` |mdash| instead of the extended forms
:ada:`R.Ref.Id` or :ada:`R.Ref.all.Id`.

.. admonition:: Important

    The extended form mentioned in the example that we just saw
    (:ada:`R.Ref.all.Id`) makes it clear that two steps happen when evaluating
    :ada:`R.Id`:

    - First, :ada:`R.Ref` is implied from :ada:`R` because of the
      :ada:`Implicit_Dereference` aspect.

    - Then, :ada:`R.Ref` is implicitly dereferenced to :ada:`R.Ref.all`.

    After these two steps, we can access the actual object. (In our case, we
    can access the :ada:`Id` component.)

Note that we cannot use access types directly for the reference discriminant.
For example, if we made the following change in the previous code example, it
wouldn't compile:

.. code-block:: ada

       type Id_Number_Access is access Id_Number;

       --  Reference type:
       type Id_Ref (Ref : Id_Number_Access) is
       --                 ^ ERROR: it must be
       --                          an access
       --                          discriminant!
         null record
           with Implicit_Dereference => Ref;

However, we could use other forms |mdash| such as :ada:`not null access`
|mdash| in the reference discriminant:

.. code-block:: ada

       --  Reference type:
       type Id_Ref (Ref : not null access Id_Number) is
         null record
           with Implicit_Dereference => Ref;

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.1.5 User-Defined References <4-1-5>`

Dereferencing of tagged types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Naturally, implicit dereferencing is also possible when calling primitives of a
tagged type. For example, let's change the declaration of the
:ada:`Id_Number` type from the previous code example and add a :ada:`Show`
primitive.

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.User_Defined_References

    package Info is
       type Id_Number (Id : Positive) is
         tagged private;

       procedure Show (R : Id_Number);
    private
       type Id_Number (Id : Positive) is
         tagged null record;
    end Info;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Info is

       procedure Show (R : Id_Number) is
       begin
          Put_Line ("ID: " & Positive'Image (R.Id));
       end Show;

    end Info;

Then, let's declare a reference type and a reference object in the test
application:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.User_Defined_References

    with Info; use Info;

    procedure Show_User_Defined_Reference is

       --  Reference type:
       type Id_Ref (Ref : access Id_Number) is
         null record
           with Implicit_Dereference => Ref;

       --  Access value:
       I : constant access Id_Number :=
             new Id_Number (42);

       --  Reference object:
       R : Id_Ref (I);
    begin

       R.Show;
       --  Equivalent to:
       --  R.Ref.all.Show;

    end Show_User_Defined_Reference;

Here, we can call the :ada:`Show` procedure by simply writing :ada:`R.Show`
instead of :ada:`R.Ref.all.Show`.


Simple container
~~~~~~~~~~~~~~~~

A typical application of user-defined references is to create cursors when
iterating over a container. As an example, let's implement the
:ada:`National_Date_Info` package to store the national day of a country:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.National_Dates

    package National_Date_Info is

       subtype Country_Code is String (1 .. 3);

       type Time is record
          Year  : Integer;
          Month : Positive range 1 .. 12;
          Day   : Positive range 1 .. 31;
       end record;

       type National_Date is tagged record
          Country : Country_Code;
          Date    : Time;
       end record;

       type National_Date_Access is
         access National_Date;

       procedure Show (Nat_Date : National_Date);

    end National_Date_Info;

    with Ada.Text_IO; use Ada.Text_IO;

    package body National_Date_Info is

       procedure Show (Nat_Date : National_Date) is
       begin
          Put_Line ("Country: "
                    & Nat_Date.Country);
          Put_Line ("Year:    "
                    & Integer'Image
                        (Nat_Date.Date.Year));
       end Show;

    end National_Date_Info;

Here, :ada:`National_Date` is a record type that we use to store the national
day information. We can call the :ada:`Show` procedure to display this
information.

Now, let's implement the :ada:`National_Date_Containers` with a container for
national days:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.National_Dates

    with National_Date_Info; use National_Date_Info;

    package National_Date_Containers is

       --  Reference type:
       type National_Date_Reference
         (Ref : access National_Date) is
           tagged limited null record
             with Implicit_Dereference => Ref;

       --  Container (as an array):
       type National_Dates is
         array (Positive range <>) of
           National_Date_Access;

       --  The Find function scans the container to
       --  find a specific country, which is returned
       --  as a reference object.
       function Find (Nat_Dates : National_Dates;
                      Country   : Country_Code)
                      return National_Date_Reference;

    end National_Date_Containers;

    package body National_Date_Containers is

       function Find (Nat_Dates : National_Dates;
                      Country   : Country_Code)
                      return National_Date_Reference
       is
       begin
          for I in Nat_Dates'Range loop
             if Nat_Dates (I).Country = Country then
                return National_Date_Reference'(
                         Ref => Nat_Dates (I));
                --     ^^^^^^^^^^^^^^^^^^^^^^^^^
                --   Returning reference object with a
                --   reference to the national day we
                --   found.
             end if;
          end loop;

          return
            National_Date_Reference'(Ref => null);
          --  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          --   Returning reference object with a null
          --   reference in case the country wasn't
          --   found. This will trigger an exception
          --   if we try to dereference it.
       end Find;

    end National_Date_Containers;

Package :ada:`National_Date_Containers` contains the :ada:`National_Dates`
type, which is an array type for declaring containers that we use to store
the national day information. We can also see the declaration of the
:ada:`National_Date_Reference` type, which is the reference type returned by
the :ada:`Find` function when looking for a specific country in the container.

.. admonition:: Important

    We're declaring the container type (:ada:`National_Dates`) as an array type
    just to simplify the code. In many cases, however, this approach isn't
    recommended! Instead, we should use a private type in order to encapsulate
    |mdash| and better protect |mdash| the information stored in the actual
    container.

Finally, let's see a test application that stores information for some
countries into the :ada:`Nat_Dates` container and displays the information for
a specific country:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.National_Dates

    with National_Date_Info;
    use  National_Date_Info;

    with National_Date_Containers;
    use  National_Date_Containers;

    procedure Show_National_Dates is

       Nat_Dates : constant National_Dates (1 .. 5) :=
         (new National_Date'("USA",
                             Time'(1776,  7,  4)),
          new National_Date'("FRA",
                             Time'(1789,  7, 14)),
          new National_Date'("DEU",
                             Time'(1990, 10,  3)),
          new National_Date'("SPA",
                             Time'(1492, 10, 12)),
          new National_Date'("BRA",
                             Time'(1822,  9,  7)));

    begin
       Find (Nat_Dates, "FRA").Show;
       --                     ^ implicit dereference
    end Show_National_Dates;

Here, we call the :ada:`Find` function to retrieve a reference object, whose
reference (access value) has the national day information of France. We then
implicitly dereference it to get the tagged object (of :ada:`National_Date`
type) and display its information by calling the :ada:`Show` procedure.

.. admonition:: Relevant topics

    The :ada:`National_Date_Containers` package was implemented specifically
    as an accompanying package for the :ada:`National_Date_Info` package.
    It is possible, however, to generalize it, so that we can reuse the
    container for other record types. In fact, this is actually very
    straightforward:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.National_Dates

        generic
           type T is private;
           type T_Access is access T;
           type T_Cmp is private;
           with function Matches (E    : T_Access;
                                  Elem : T_Cmp)
                                  return Boolean;
        package Generic_Containers is

           type Ref_Type (Ref : access T) is
             tagged limited null record
               with Implicit_Dereference => Ref;

           type Container is
             array (Positive range <>) of
               T_Access;

           function Find (Cont : Container;
                          Elem : T_Cmp)
                          return Ref_Type;

        end Generic_Containers;

        package body Generic_Containers is

           function Find (Cont : Container;
                          Elem : T_Cmp)
                          return Ref_Type is
           begin
              for I in Cont'Range loop
                 if Matches (Cont (I), Elem) then
                    return Ref_Type'(Ref => Cont (I));
                 end if;
              end loop;

              return Ref_Type'(Ref => null);
           end Find;

        end Generic_Containers;

    When comparing the :ada:`Generic_Containers` package to the
    :ada:`National_Date_Containers` package, we see that the main difference is
    the addition of the :ada:`Matches` function, which indicates whether the
    current element we're evaluating in the for-loop of the :ada:`Find`
    function is the one we're looking for.

    In the main application, we can implement the :ada:`Matches` function and
    declare the :ada:`National_Date_Containers` package as an instance of the
    :ada:`Generic_Containers` package:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.User_Defined_References.National_Dates

        with Generic_Containers;
        with National_Date_Info; use National_Date_Info;

        procedure Show_National_Dates is

           function Matches_Country
             (E    : National_Date_Access;
              Elem : Country_Code)
              return Boolean is
                (E.Country = Elem);

           package National_Date_Containers is new
             Generic_Containers
               (T        => National_Date,
                T_Access => National_Date_Access,
                T_Cmp    => Country_Code,
                Matches  => Matches_Country);

           use National_Date_Containers;

           subtype National_Dates is Container;

           Nat_Dates : constant
                         National_Dates (1 .. 5) :=
             (new National_Date'("USA",
                                 Time'(1776,  7,  4)),
              new National_Date'("FRA",
                                 Time'(1789,  7, 14)),
              new National_Date'("DEU",
                                 Time'(1990, 10,  3)),
              new National_Date'("SPA",
                                 Time'(1492, 10, 12)),
              new National_Date'("BRA",
                                 Time'(1822,  9,  7)));

        begin
           Find (Nat_Dates, "FRA").Show;
        end Show_National_Dates;

    Here, we instantiate the :ada:`Generic_Containers` package with the
    :ada:`Matches_Country` function, which is an expression function that
    compares the country component of the current :ada:`National_Date`
    reference with the name of the country we desire to learn about.

    This generalized approach is actually used for the standard containers
    from the :ada:`Ada.Containers` packages. For example,
    the :ada:`Ada.Containers.Vectors` is specified as follows:

    .. code-block:: ada

        with Ada.Iterator_Interfaces;

        generic
           type Index_Type is range <>;
           type Element_Type is private;
           with function "=" (Left, Right : Element_Type)
                              return Boolean is <>;
        package Ada.Containers.Vectors
          with Preelaborate, Remote_Types,
               Nonblocking,
               Global => in out synchronized is

           -- OMITTED

           type Reference_Type
             (Element : not null access Element_Type) is
               private
                 with Implicit_Dereference => Element,
                      Nonblocking,
                      Global => in out synchronized,
                      Default_Initial_Condition =>
                        (raise Program_Error);

           -- OMITTED

           function Reference
             (Container : aliased in out Vector;
              Index     : in Index_Type)
              return Reference_Type
                with Pre  => Index in
                               First_Index (Container) ..
                               Last_Index (Container)
                             or else raise
                                     Constraint_Error,
                   Post =>
                     Tampering_With_Cursors_Prohibited
                       (Container),
                   Nonblocking,
                   Global => null,
                   Use_Formal => null;

           -- OMITTED

           function Reference
             (Container : aliased in out Vector;
              Position  : in Cursor)
              return Reference_Type
                with Pre  => (Position /= No_Element
                              or else raise
                                      Constraint_Error)
                              and then
                                (Has_Element
                                  (Container, Position)
                                 or else raise
                                         Program_Error),
                   Post   =>
                     Tampering_With_Cursors_Prohibited
                       (Container),
                   Nonblocking,
                   Global => null,
                   Use_Formal => null;

           -- OMITTED

        end Ada.Containers.Vectors;

    (Note that most parts of the :ada:`Vectors` package were omitted for
    clarity. Please refer to the Ada Reference Manual for the complete package
    specification.)

    Here, we see that the :ada:`Implicit_Dereference` aspect is used in the
    declaration of :ada:`Reference_Type`, which is the reference type returned
    by the :ada:`Reference` functions for an index or a cursor.

    Also, note that the :ada:`Vectors` package has a formal equality function
    (:ada:`=`) instead of the :ada:`Matches` function we were using in our
    :ada:`Generic_Containers` package. The purpose of the formal function,
    however, is basically the same.

    .. admonition:: In the Ada Reference Manual

        - :arm22:`A.18.2 The Generic Package Containers.Vectors <A-18-2>`


.. _Adv_Ada_Anonymous_Access_Types_Accessibility_Rules:

Anonymous Access Types and Accessibility Rules
----------------------------------------------

In general, the :ref:`accessibility rules <Adv_Ada_Accessibility_Rules>` we've
seen earlier also apply to anonymous access types. However, there are some
subtle differences, which we discuss in this section.

Let's adapt the
:ref:`code example from that section <Adv_Ada_Accessibility_Rules_Code_Example>`
to make use of anonymous access types:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Levels_Rules_Introduction.Accessibility_Library_Level
    :class: ada-expect-compile-error

    package Library_Level is

       L0_AO  : access Integer;

       L0_Var : aliased Integer;

    end Library_Level;

    with Library_Level; use Library_Level;

    procedure Show_Library_Level is
       L1_Var : aliased Integer;

       L1_AO  : access Integer;

       procedure Test is
          L2_AO  : access Integer;

          L2_Var : aliased Integer;
       begin
          L1_AO := L2_Var'Access;
          --       ^^^^^^
          --       ILLEGAL: L2 object to
          --                L1 access object

          L2_AO := L2_Var'Access;
          --       ^^^^^^
          --       LEGAL: L2 object to
          --              L2 access object
       end Test;

    begin
       L0_AO := new Integer'(22);
       --       ^^^^^^^^^^^
       --       LEGAL: L0 object to
       --              L0 access object

       L0_AO := L1_Var'Access;
       --       ^^^^^^
       --       ILLEGAL: L1 object to
       --                L0 access object

       L1_AO := L0_Var'Access;
       --       ^^^^^^
       --       LEGAL: L0 object to
       --              L1 access object

       L1_AO := L1_Var'Access;
       --       ^^^^^^
       --       LEGAL: L1 object to
       --              L1 access object

       L0_AO := L1_AO;  -- legal!!
       --       ^^^^^
       --       LEGAL:   L1 access object to
       --                L0 access object
       --
       --       ILLEGAL: L1 object
       --                (L1_AO = L1_Var'Access)
       --                to
       --                L0 access object
       --
       --       This is actually OK at compile time,
       --       but the accessibility check fails at
       --       runtime.

       Test;
    end Show_Library_Level;

As we see in the code, in general, most accessibility rules are the same as the
ones we've discussed when using named access types. For example, an assignment
such as :ada:`L0_AO := L1_Var'Access` is illegal because we're trying to assign
to an access object of less deep level.

However, assignment such as :ada:`L0_AO := L1_AO` are possible now: we don't
get a type mismatch |mdash| as we did with named access types |mdash| because
both objects are of anonymous access types. Note that the accessibility level
cannot be determined at compile time: :ada:`L1_AO` can hold an access value at
library level (which would make the assignment legal) or at a deeper level.
Therefore, the compiler introduces an accessibility check here.

However, the accessibility check used in :ada:`L0_AO := L1_AO` fails at runtime
because the corresponding access value (:ada:`L1_Var'Access`) is of a deeper
level than :ada:`L0_AO`, which is illegal. (If you comment out the
:ada:`L1_AO := L1_Var'Access` assignment prior to the :ada:`L0_AO := L1_AO`
assignment, this accessibility check doesn't fail anymore.)


Conversions between Anonymous and Named Access Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous sections, we've discussed accessibility rules for named and
anonymous access types separately. In this section, we see that the same
accessibility rules apply when mixing both flavors together and converting
objects of anonymous to named access types.

Let's adapt parts of the previous
:ref:`code example <Adv_Ada_Accessibility_Rules_Code_Example>` and add
anonymous access types to it:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Levels_Rules_Introduction.Accessibility_Named_Anonymous_Access_Type_Conversions
    :class: ada-expect-compile-error

    package Library_Level is

       type L0_Integer_Access is
         access all Integer;

       L0_Var : aliased Integer;

       L0_IA  : L0_Integer_Access;
       L0_AO  : access Integer;

    end Library_Level;

    with Library_Level; use Library_Level;

    procedure Show_Library_Level is
       type L1_Integer_Access is
         access all Integer;

       L1_IA  : L1_Integer_Access;
       L1_AO  : access Integer;

       L1_Var : aliased Integer;

    begin
       ---------------------------------------
       --  From named type to anonymous type
       ---------------------------------------

       L0_IA := new Integer'(22);
       L1_IA := new Integer'(42);

       L0_AO := L0_IA;
       --       ^^^^^
       --       LEGAL: assignment from
       --              L0 access object (named type)
       --              to
       --              L0 access object
       --                (anonymous type)

       L0_AO := L1_IA;
       --       ^^^^^
       --       ILLEGAL: assignment from
       --                L1 access object (named type)
       --                to
       --                L0 access object
       --                  (anonymous type)

       L1_AO := L0_IA;
       --       ^^^^^
       --       LEGAL: assignment from
       --              L0 access object (named type)
       --              to
       --              L1 access object
       --                (anonymous type)

       L1_AO := L1_IA;
       --       ^^^^^
       --       LEGAL: assignment from
       --              L1 access object (named type)
       --              to
       --              L1 access object
       --                (anonymous type)

       ---------------------------------------
       --  From anonymous type to named type
       ---------------------------------------

       L0_AO := L0_Var'Access;
       L1_AO := L1_Var'Access;

       L0_IA := L0_Integer_Access (L0_AO);
       --       ^^^^^^^^^^^^^^^^^
       --       LEGAL: conversion / assignment from
       --              L0 access object
       --                (anonymous type)
       --              to
       --              L0 access object (named type)

       L0_IA := L0_Integer_Access (L1_AO);
       --       ^^^^^^^^^^^^^^^^^
       --       ILLEGAL: conversion / assignment from
       --                L1 access object
       --                  (anonymous type)
       --                to
       --                L0 access object (named type)
       --                (accessibility check fails)

       L1_IA := L1_Integer_Access (L0_AO);
       --       ^^^^^^^^^^^^^^^^^
       --       LEGAL: conversion / assignment from
       --              L0 access object
       --                (anonymous type)
       --              to
       --              L1 access object (named type)

       L1_IA := L1_Integer_Access (L1_AO);
       --       ^^^^^^^^^^^^^^^^^
       --       LEGAL: conversion / assignment from
       --              L1 access object
       --                (anonymous type)
       --              to
       --              L1 access object (named type)
    end Show_Library_Level;

As we can see in this code example, mixing access objects of named and
anonymous access types doesn't change the accessibility rules. Again, the rules
are only violated when the target object in the assignment is *less* deep. This
is the case in the :ada:`L0_AO := L1_IA` and the
:ada:`L0_IA := L0_Integer_Access (L1_AO)` assignments. Otherwise, mixing those
access objects doesn't impose additional hurdles.


.. _Adv_Ada_Anonymous_Access_To_Subprograms:

Anonymous Access-To-Subprograms
-------------------------------

In the previous chapter, we talked about
:ref:`named access-to-subprogram types <Adv_Ada_Access_To_Subprograms>`. Now,
we'll see that the anonymous version of those types isn't much different from
the named version.

Let's start our discussion by declaring a subprogram parameter using an
anonymous access-to-procedure type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Anonymous_Access_To_Subprogram_Example

    package Anonymous_Access_To_Subprogram is

       procedure Proc
         (P : access procedure (I : in out Integer));

    end Anonymous_Access_To_Subprogram;

    package body Anonymous_Access_To_Subprogram is

       procedure Proc
         (P : access procedure (I : in out Integer))
       is
          I : Integer := 0;
       begin
          P (I);
       end Proc;

    end Anonymous_Access_To_Subprogram;

In this example, we use the anonymous
:ada:`access procedure (I : in out Integer)` type as a parameter of the
:ada:`Proc` procedure. Note that we need an identifier in the declaration:
we cannot leave :ada:`I` out and write
:ada:`access procedure (in out Integer)`.

Before we look at a test application that makes use of the
:ada:`Anonymous_Access_To_Subprogram` package, let's implement two simple
procedures that we'll use later on:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Anonymous_Access_To_Subprogram_Example

    procedure Add_Ten (I : in out Integer);

    procedure Add_Ten (I : in out Integer) is
    begin
       I := I + 10;
    end Add_Ten;

    procedure Add_Twenty (I : in out Integer);

    procedure Add_Twenty (I : in out Integer) is
    begin
       I := I + 20;
    end Add_Twenty;

Finally, this is our test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Anonymous_Access_To_Subprogram_Example

    with Anonymous_Access_To_Subprogram;
    use  Anonymous_Access_To_Subprogram;

    with Add_Ten;

    procedure Show_Anonymous_Access_To_Subprograms is
    begin
       Proc (Add_Ten'Access);
       --            ^ Getting access to Add_Ten
       --              procedure and passing it
       --              to Proc
    end Show_Anonymous_Access_To_Subprograms;

Here, we get access to the :ada:`Add_Ten` procedure and pass it to the
:ada:`Proc` procedure. Note that this implementation is not different from the
:ref:`example for named access-to-subprogram types <Adv_Ada_Access_To_Subprogram_Params_Example>`.
In fact, in terms of usage, anonymous access-to-subprogram types are very
similar to named access-to-subprogram types. The major differences can be found
in the corresponding
:ref:`accessibility rules <Adv_Ada_Accessibility_Rules_Anonymous_Access_To_Subprograms>`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


Examples of anonymous access-to-subprogram usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the section about
:ref:`named access-to-subprogram types <Adv_Ada_Access_To_Subprograms>`, we've
seen a couple of different usages for those types. In all those examples
we discussed, we could instead have used anonymous access-to-subprogram types.
Let's see a code example that illustrates that:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Anonymous_Access_To_Subprogram_Example

    package All_Anonymous_Access_To_Subprogram is

       --
       --  Anonymous access-to-subprogram as
       --  subprogram parameter:
       --
       procedure Proc
         (P : access procedure (I : in out Integer));

       --
       --  Anonymous access-to-subprogram in
       --  array type declaration:
       --
       type Access_To_Procedure_Array is
         array (Positive range <>) of
           access procedure (I : in out Integer);

       protected type Protected_Integer is

         procedure Mult_Ten;

         procedure Mult_Twenty;

       private
         I : Integer := 1;
       end Protected_Integer;

       --
       --  Anonymous access-to-subprogram as
       --  component of a record type.
       --
       type Rec_Access_To_Procedure is record
          AP  : access procedure (I : in out Integer);
       end record;

       --
       --  Anonymous access-to-subprogram as
       --  discriminant:
       --
       type Rec_Access_To_Procedure_Discriminant
              (AP : access procedure
                      (I : in out Integer)) is
       record
          I : Integer := 0;
       end record;

       procedure Process
         (R : in out
                Rec_Access_To_Procedure_Discriminant);

       generic
          type T is private;

          --
          --  Anonymous access-to-subprogram as
          --  formal parameter:
          --
          Proc_T : access procedure
                     (Element : in out T);
       procedure Gen_Process (Element : in out T);

    end All_Anonymous_Access_To_Subprogram;

    with Ada.Text_IO; use Ada.Text_IO;

    package body All_Anonymous_Access_To_Subprogram is

       procedure Proc
         (P : access procedure (I : in out Integer))
       is
          I : Integer := 0;
       begin
          Put_Line
            ("Calling procedure for Proc...");
          P (I);
          Put_Line ("Finished.");
       end Proc;

       procedure Process
         (R : in out
                Rec_Access_To_Procedure_Discriminant)
       is
       begin
          Put_Line
            ("Calling procedure for"
             & " Rec_Access_To_Procedure_Discriminant"
             & " type...");
          R.AP (R.I);
          Put_Line ("Finished.");
       end Process;

       procedure Gen_Process (Element : in out T) is
       begin
          Put_Line
            ("Calling procedure for Gen_Process...");
          Proc_T (Element);
          Put_Line ("Finished.");
       end Gen_Process;

       protected body Protected_Integer is

          procedure Mult_Ten is
          begin
             I := I * 10;
          end Mult_Ten;

          procedure Mult_Twenty is
          begin
             I := I * 20;
          end Mult_Twenty;

       end Protected_Integer;

    end All_Anonymous_Access_To_Subprogram;

In the :ada:`All_Anonymous_Access_To_Subprogram` package, we see examples of
anonymous access-to-subprogram types:

- as a subprogram parameter;

- in an array type declaration;

- as a component of a record type;

- as a record type discriminant;

- as a formal parameter of a generic procedure.

Let's implement a test application that makes use of this package:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Anonymous_Access_To_Subprogram_Example

    with Ada.Text_IO; use Ada.Text_IO;

    with Add_Ten;
    with Add_Twenty;

    with All_Anonymous_Access_To_Subprogram;
    use  All_Anonymous_Access_To_Subprogram;

    procedure Show_Anonymous_Access_To_Subprograms is
       --
       --  Anonymous access-to-subprogram as
       --  an object:
       --
       P   : access procedure (I : in out Integer);

       --
       --  Array of anonymous access-to-subprogram
       --  components
       --
       PA  : constant
              Access_To_Procedure_Array (1 .. 2) :=
                (Add_Ten'Access,
                 Add_Twenty'Access);

       --
       --  Anonymous array of anonymous
       --  access-to-subprogram components:
       --
       PAA : constant
              array (1 .. 2) of access
                procedure (I : in out Integer) :=
                  (Add_Ten'Access,
                   Add_Twenty'Access);

       --
       --  Record with anonymous
       --  access-to-subprogram components:
       --
       RA : constant Rec_Access_To_Procedure :=
              (AP => Add_Ten'Access);

       --
       --  Record with anonymous
       --  access-to-subprogram discriminant:
       --
       RD : Rec_Access_To_Procedure_Discriminant
              (AP => Add_Twenty'Access) :=
                (AP => Add_Twenty'Access, I => 0);

       --
       --  Generic procedure with formal anonymous
       --  access-to-subprogram:
       --
       procedure Process_Integer is new
         Gen_Process (T      => Integer,
                      Proc_T => Add_Twenty'Access);

       --
       --  Object (APP) of anonymous
       --  access-to-protected-subprogram:
       --
       PI  : Protected_Integer;
       APP : constant access protected procedure :=
               PI.Mult_Ten'Access;

       Some_Int : Integer := 0;
    begin
       Put_Line ("Some_Int: " & Some_Int'Image);

       --
       --  Using object of
       --  anonymous access-to-subprogram type:
       --
       P := Add_Ten'Access;
       Proc (P);
       P (Some_Int);

       P := Add_Twenty'Access;
       Proc (P);
       P (Some_Int);

       Put_Line ("Some_Int: " & Some_Int'Image);

       --
       --  Using array with component of
       --  anonymous access-to-subprogram type:
       --
        Put_Line
          ("Calling procedure from PA array...");

       for I in PA'Range loop
          PA (I) (Some_Int);
          Put_Line ("Some_Int: " & Some_Int'Image);
       end loop;

       Put_Line ("Finished.");

       Put_Line
         ("Calling procedure from PAA array...");

       for I in PA'Range loop
          PAA (I) (Some_Int);
          Put_Line ("Some_Int: " & Some_Int'Image);
       end loop;

       Put_Line ("Finished.");

       Put_Line ("Some_Int: " & Some_Int'Image);

       --
       --  Using record with component of
       --  anonymous access-to-subprogram type:
       --
       RA.AP (Some_Int);
       Put_Line ("Some_Int: " & Some_Int'Image);

       --
       --  Using record with discriminant of
       --  anonymous access-to-subprogram type:
       --
       Process (RD);
       Put_Line ("RD.I: " & RD.I'Image);

       --
       --  Using procedure instantiated with
       --  formal anonymous access-to-subprogram:
       --
       Process_Integer (Some_Int);
       Put_Line ("Some_Int: " & Some_Int'Image);

       --
       --  Using object of anonymous
       --  access-to-protected-subprogram type:
       --
       APP.all;
    end Show_Anonymous_Access_To_Subprograms;

In the :ada:`Show_Anonymous_Access_To_Subprograms` procedure, we see examples
of anonymous access-to-subprogram types in:

- in objects (:ada:`P`) and (:ada:`APP`);

- in arrays (:ada:`PA` and :ada:`PAA`);

- in records (:ada:`RA` and :ada:`RD`);

- in the binding to a formal parameter (:ada:`Proc_T`) of an instantiated
  procedure (:ada:`Process_Integer`);

- as a parameter of a procedure (:ada:`Proc`).

Because we already discussed all these usages in the section about
:ref:`named access-to-subprogram types <Adv_Ada_Access_To_Subprograms>`, we
won't repeat this discussion here. If anything in this code example is still
unclear to you, make sure to revisit that section from the previous chapter.


Application of anonymous access-to-subprogram types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In general, there isn't much that speaks against using anonymous
access-to-subprogram types. We can say, for example, that they're much more
useful than
:ref:`anonymous access-to-objects types <Adv_Ada_Anonymous_Access_To_Object_Types>`,
which have
:ref:`many drawbacks <Adv_Ada_Drawbacks_Anonymous_Access_To_Object_Types>` |mdash|
as we discussed earlier.

There isn't much to be concerned when using anonymous access-to-subprogram
types. For example, we cannot allocate or deallocate a subprogram. As a
consequence, we won't have storage management issues affecting these types
because the access to those subprograms will always be available and no
memory leak can occur.

Also, anonymous access-to-subprogram types can be easier to use than named
access-to-subprogram types because of their less strict
:ref:`accessibility rules <Adv_Ada_Accessibility_Rules_Anonymous_Access_To_Subprograms>`.
Some of the accessibility issues we might encounter when using named
access-to-subprogram types can be solved by declaring them as anonymous types.
(We discuss the accessibility rules of anonymous access-to-subprogram types in
the next section.)


Readability
~~~~~~~~~~~

Note that readability suffers if you use a *cascade* of anonymous
access-to-subprograms. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Readability_Issue

    package Readability_Issue is

       function F
         return access
           function (A : Integer)
                     return access
                       function (B : Float)
                                 return Integer;

    end Readability_Issue;

    package Readability_Issue.Functions is

       function To_Integer (V : Float)
                            return Integer is
         (Integer (V));

       function Select_Conversion
         (A : Integer)
          return access
            function (B : Float)
                      return Integer is
         (To_Integer'Access);

    end Readability_Issue.Functions;

    with Readability_Issue.Functions;
    use  Readability_Issue.Functions;

    package body Readability_Issue is

       function F
         return access
           function (A : Integer)
                     return access
                       function (B : Float)
                           return Integer is
         (Select_Conversion'Access);

    end Readability_Issue;

In this example, the definition of :ada:`F` might compile fine, but it's simply
too long to be readable. Not only that: we need to carry this *chain* to other
functions as well |mdash| such as the :ada:`Select_Conversion` function above.
Also, using these functions in an application is not straightforward:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Anonymous_Access_To_Subprograms.Readability_Issue

    with Readability_Issue;
    use  Readability_Issue;

    procedure Show_Readability_Issue is
       F1 : access
              function (A : Integer)
                        return access
                          function (B : Float)
                                    return Integer
            := F;
       F2 : access function (B : Float)
                             return Integer
            := F1 (2);
       I  : Integer := F2 (0.1);
    begin
       I := F1 (2) (0.1);
    end Show_Readability_Issue;

Therefore, our recommendation is to avoid this kind of *access cascading* by
carefully designing your application. In general, you won't need that.


.. _Adv_Ada_Accessibility_Rules_Anonymous_Access_To_Subprograms:

Accessibility Rules and Anonymous Access-To-Subprograms
-------------------------------------------------------

In principle, the
:ref:`accessibility rules for anonymous access types <Adv_Ada_Anonymous_Access_Types_Accessibility_Rules>`
that we've seen before apply to anonymous access-to-subprograms as well. Also,
we had a discussion about
:ref:`accessibility rules and access-to-subprograms <Adv_Ada_Accessibility_Rules_Access_To_Subprograms>`
in the previous chapter. In this section, we review some of the rules that we
already know and discuss how they relate to anonymous access-to-subprograms.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`


Named vs. anonymous access-to-subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's see an example of a named access-to-subprogram type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Simple_Example_Named
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Access_To_Subprogram_Error is

       type PI is access
         procedure (I : in out Integer);

       P : PI;

       I : Integer := 0;
    begin
       declare
          procedure Add_One (I : in out Integer) is
          begin
             I := I + 1;
          end Add_One;
       begin
          P := Add_One'Access;
       end;
    end Show_Access_To_Subprogram_Error;

In this example, we get a compilation error because the lifetime of the
:ada:`Add_One` procedure is shorter than the access type :ada:`PI`.

In contrast, using an anonymous access-to-subprogram type eliminates the
compilation error, i.e. the assignment :ada:`P := Add_One'Access` becomes
legal:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Simple_Example_Anonymous
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Access_To_Subprogram_Error is
       P : access procedure (I : in out Integer);

       I : Integer := 0;
    begin
       declare
          procedure Add_One (I : in out Integer) is
          begin
             I := I + 1;
          end Add_One;
       begin
          P := Add_One'Access;
          --  ERROR: Add_One is out-of-scope after
          --         this line.
       end;
    end Show_Access_To_Subprogram_Error;

In this case, the compiler introduces an accessibility check, which fails at
runtime because the lifetime of :ada:`Add_One` is shorter than the lifetime of
the access object :ada:`P`.


Named vs. anonymous access-to-subprograms as parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using anonymous access-to-subprograms as parameters allows us to pass
subprograms at any level. For certain applications, the restrictions that are
applied to named access types might be too strict, so using anonymous
access-to-subprograms might be a good way to circumvent those restrictions.


Named access-to-subprograms as a parameter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's see an example using a named access-to-procedure type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Access_To_Subprogram_Parameter_Named
    :class: ada-expect-compile-error

    package Access_To_Subprogram_Types is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Process_Procedure is
         access
           procedure (Arr : in out Integer_Array);

       procedure Process
         (Arr : in out Integer_Array;
          P   :        Process_Procedure);

    end Access_To_Subprogram_Types;

    package body Access_To_Subprogram_Types is

       procedure Process
         (Arr : in out Integer_Array;
          P   :        Process_Procedure) is
       begin
          P (Arr);
       end Process;

    end Access_To_Subprogram_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    procedure Show_Access_To_Subprogram_Error is

       procedure Add_One
         (Arr : in out Integer_Array) is
       begin
          for E of Arr loop
             E := E + 1;
          end loop;
       end Add_One;

       procedure Display
         (Arr : in out Integer_Array) is
       begin
          for I in Arr'Range loop
             Put_Line ("Arr (" &
                       Integer'Image (I)
                       & "): "
                      & Integer'Image (Arr (I)));
          end loop;
       end Display;

       Arr : Integer_Array (1 .. 3) := (1, 2, 3);
    begin
       Process (Arr, Display'Access);

       Put_Line ("Add_One...");
       Process (Arr, Add_One'Access);

       Process (Arr, Display'Access);
    end Show_Access_To_Subprogram_Error;

In this example, we declare the :ada:`Process_Procedure` type in the
:ada:`Access_To_Subprogram_Types` package and use it in the :ada:`Process`
procedure, which we call in the :ada:`Show_Access_To_Subprogram_Error`
procedure. The accessibility rules trigger a compilation error because the
accesses (:ada:`Add_One'Access` and :ada:`Display'Access`) are at a
deeper level than the access-to-procedure type (:ada:`Process_Procedure`).

As we know already, there's no :ada:`Unchecked_Access` attribute that
we could use here. An easy way to make this code compile could be to move
:ada:`Add_One` and :ada:`Display` to the library level.


Anonymous access-to-subprograms as a parameter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To circumvent the compilation error, we could also use anonymous
access-to-subprograms instead:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Access_To_Subprogram_Parameter_Anonymous

    package Access_To_Subprogram_Types is

       type Integer_Array is
         array (Positive range <>) of Integer;

       procedure Process
         (Arr : in out Integer_Array;
          P   : access procedure
                  (Arr : in out Integer_Array));

    end Access_To_Subprogram_Types;

    package body Access_To_Subprogram_Types is

       procedure Process
         (Arr : in out Integer_Array;
          P   : access procedure
                  (Arr : in out Integer_Array)) is
       begin
          P (Arr);
       end Process;

    end Access_To_Subprogram_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Access_To_Subprogram_Types;
    use  Access_To_Subprogram_Types;

    procedure Show_Access_To_Subprogram_Error is

       procedure Add_One
         (Arr : in out Integer_Array) is
       begin
          for E of Arr loop
             E := E + 1;
          end loop;
       end Add_One;

       procedure Display
         (Arr : in out Integer_Array) is
       begin
          for I in Arr'Range loop
             Put_Line ("Arr (" &
                       Integer'Image (I)
                       & "): "
                      & Integer'Image (Arr (I)));
          end loop;
       end Display;

       Arr : Integer_Array (1 .. 3) := (1, 2, 3);
    begin
       Process (Arr, Display'Access);

       Put_Line ("Add_One...");
       Process (Arr, Add_One'Access);

       Process (Arr, Display'Access);
    end Show_Access_To_Subprogram_Error;

Now, the code is accepted by the compiler because anonymous
access-to-subprograms used as parameters allow passing of subprograms at any
level. Also, we don't see a run-time exception because the subprograms are
still *accessible* when we call :ada:`Process`.


Iterator
~~~~~~~~

A typical example that illustrates well the necessity of using anonymous
access-to-subprograms is that of a container iterator. In fact, many of the
standard Ada containers |mdash| the child packages of :ada:`Ada.Containers`
|mdash| make use of anonymous access-to-subprograms for their :ada:`Iterate`
subprograms.

.. admonition:: In the Ada Reference Manual

    - :arm22:`A.18.2 The Package Containers.Vectors <A-18-2>`
    - :arm22:`A.18.4 Maps <A-18-4>`
    - :arm22:`A.18.7 Sets <A-18-7>`


Using named access-to-subprograms
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with a simplified container type (:ada:`Data_Container`) using a
named access-to-subprogram type (:ada:`Process_Element`) for iteration:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Iterator_Named

    generic
       type Element is private;
    package Data_Processing is

       type Data_Container (Last : Positive) is
         private;

       Data_Container_Full : exception;

       procedure Append (D : in out Data_Container;
                         E :        Element);

       type Process_Element is
         not null access procedure (E : Element);

       procedure Iterate
         (D    : Data_Container;
          Proc : Process_Element);

    private

       type Data_Container_Storage is
         array (Positive range <>) of Element;

       type Data_Container (Last : Positive) is
       record
          S    : Data_Container_Storage (1 .. Last);
          Curr : Natural := 0;
       end record;

    end Data_Processing;

    package body Data_Processing is

       procedure Append (D : in out Data_Container;
                         E :        Element) is
       begin
          if D.Curr < D.S'Last then
             D.Curr := D.Curr + 1;
             D.S (D.Curr) := E;
          else
             raise Data_Container_Full;
             --  NOTE: This is just a dummy
             --        implementation. A better
             --        strategy is to add actual error
             --        handling when the container is
             --        full.
          end if;
       end Append;

       procedure Iterate
         (D    : Data_Container;
          Proc : Process_Element) is
       begin
          for I in D.S'First .. D.Curr loop
             Proc (D.S (I));
          end loop;
       end Iterate;

    end Data_Processing;

In this example, we declare the :ada:`Process_Element` type in the
generic :ada:`Data_Processing` package, and we use it in the :ada:`Iterate`
procedure. We then instantiate this package as :ada:`Float_Data_Processing`,
and we use it in the :ada:`Show_Access_To_Subprograms` procedure:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Iterator_Named
    :class: ada-expect-compile-error

    with Data_Processing;

    package Float_Data_Processing is
      new Data_Processing (Element => Float);

    with Ada.Text_IO; use Ada.Text_IO;

    with Float_Data_Processing;
    use  Float_Data_Processing;

    procedure Show_Access_To_Subprograms is

       procedure Display (F : Float) is
       begin
          Put_Line ("F :" & Float'Image (F));
       end Display;

       D : Data_Container (5);
    begin
        Append (D, 1.0);
        Append (D, 2.0);
        Append (D, 3.0);

        Iterate (D, Display'Access);
    end Show_Access_To_Subprograms;

Using :ada:`Display'Access` in the call to :ada:`Iterate` triggers a
compilation error because its lifetime is shorter than the lifetime of the
:ada:`Process_Element` type.


Using anonymous access-to-subprograms
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now, let's use an anonymous access-to-subprogram type in the :ada:`Iterate`
procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Iterator_Anonymous

    generic
       type Element is private;
    package Data_Processing is

       type Data_Container (Last : Positive) is
         private;

       Data_Container_Full : exception;

       procedure Append (D : in out Data_Container;
                         E :        Element);

       procedure Iterate
         (D    : Data_Container;
          Proc : not null access
                   procedure (E : Element));

    private

       type Data_Container_Storage is
         array (Positive range <>) of Element;

       type Data_Container (Last : Positive) is
       record
          S    : Data_Container_Storage (1 .. Last);
          Curr : Natural := 0;
       end record;

    end Data_Processing;

    package body Data_Processing is

       procedure Append (D : in out Data_Container;
                         E :        Element) is
       begin
          if D.Curr < D.S'Last then
             D.Curr := D.Curr + 1;
             D.S (D.Curr) := E;
          else
             raise Data_Container_Full;
             --  NOTE: This is just a dummy
             --        implementation. A better
             --        strategy is to add actual error
             --        handling when the container is
             --        full.
          end if;
       end Append;

       procedure Iterate
         (D    : Data_Container;
          Proc : not null access
                   procedure (E : Element)) is
       begin
          for I in D.S'First .. D.Curr loop
             Proc (D.S (I));
          end loop;
       end Iterate;

    end Data_Processing;

Note that the only changes we did to the package were to remove the
:ada:`Process_Element` type and replace the type of the :ada:`Proc` parameter
of the :ada:`Iterate` procedure from a named type (:ada:`Process_Element`) to
an anonymous type (:ada:`not null access procedure (E : Element)`).

Now, the same test application we used before
(:ada:`Show_Access_To_Subprograms`) compiles as expected:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Accessibility_Rules_Anonymous_Access_To_Subprograms.Iterator_Anonymous

    with Data_Processing;

    package Float_Data_Processing is
      new Data_Processing (Element => Float);

    with Ada.Text_IO; use Ada.Text_IO;

    with Float_Data_Processing;
    use  Float_Data_Processing;

    procedure Show_Access_To_Subprograms is

       procedure Display (F : Float) is
       begin
          Put_Line ("F :" & Float'Image (F));
       end Display;

       D : Data_Container (5);
    begin
        Append (D, 1.0);
        Append (D, 2.0);
        Append (D, 3.0);

        Iterate (D, Display'Access);
    end Show_Access_To_Subprograms;

Remember that the compiler introduces an accessibility check in the call to
:ada:`Iterate`, which is successful because the lifetime of
:ada:`Display'Access` is the same as the lifetime of the :ada:`Proc` parameter
of :ada:`Iterate`.
