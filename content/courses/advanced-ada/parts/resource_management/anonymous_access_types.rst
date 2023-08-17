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

We can declare subprogram parameters using anonymous types:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Anonymous_Access_Types.Access_To_Subprograms.Access_To_Subprogram_Params

    package Access_To_Subprogram_Params is

       procedure Proc
         (P : access procedure (I : in out Integer));

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc
         (P : access procedure (I : in out Integer))
       is
          I : Integer := 0;
       begin
          --  P (I);
          P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

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


.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10 Access Types <3-10>`

.. todo::

    Complete section!


.. _Adv_Ada_Accessibility_Rules_Anonymous_Access_To_Subprograms:

Accessibility Rules and Anonymous Access-To-Subprograms
-------------------------------------------------------

.. todo::

    Complete section!

