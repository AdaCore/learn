Records
=======

.. include:: ../../../global.txt

.. _Adv_Ada_Mutually_Dependent_Types:

Mutually dependent types
------------------------

In this section, we discuss how to use
:ref:`incomplete types <Adv_Ada_Incomplete_Types>` to declare mutually
dependent types. Let's start with this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Mutually_Dependent_Types.Mutually_Dependent
    :class: ada-expect-compile-error

    package Mutually_Dependent is

       type T1 is record
          B : T2;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

When you try to compile this example, you get a compilation error. The first
problem with this code is that, in the declaration of the :ada:`T1` record, the
compiler doesn't know anything about :ada:`T2`. We could solve this by
declaring an incomplete type (:ada:`type T2;`) before the declaration of
:ada:`T1`. This, however, doesn't solve all the problems in the code: the
compiler still doesn't know the size of :ada:`T2`, so we cannot create a
component of this type. We could, instead, declare an access type and use it
here. By doing this, even though the compiler doesn't know the size of
:ada:`T2`, it knows the size of an access type designating :ada:`T2`, so the
record component can be of such an access type.

To summarize, in order to solve the compilation error above, we need to:

- use at least one incomplete type;
- declare at least one component as an access to an object.

For example, we could declare an incomplete type :ada:`T2` and then declare
the component :ada:`B` of the :ada:`T1` record as an access to :ada:`T2`.
This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Mutually_Dependent_Types.Mutually_Dependent

    package Mutually_Dependent is

       type T2;
       type T2_Access is access T2;

       type T1 is record
          B : T2_Access;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

We could strive for consistency and declare two incomplete types and two
accesses, but this isn't strictly necessary in this case. Here's the adapted
code:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Mutually_Dependent_Types.Mutually_Dependent

    package Mutually_Dependent is

       type T1;
       type T1_Access is access T1;

       type T2;
       type T2_Access is access T2;

       type T1 is record
          B : T2_Access;
       end record;

       type T2 is record
          A : T1_Access;
       end record;

    end Mutually_Dependent;

Later on, we'll see that these code examples can be written using
:ref:`anonymous access types <Adv_Ada_Mutually_Dependent_Types_Using_Anonymous_Access_Types>`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.1 Incomplete Type Declarations <3-10-1>`


.. _Adv_Ada_Null_Records:

Null records
------------

A null record is a record that doesn't have any components. Consequently, it
cannot store any information. When declaring a null record, we simply
write :ada:`null` instead of declaring actual components, as we usually do for
records. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Null_Record

    package Null_Recs is

       type Null_Record is record
          null;
       end record;

    end Null_Recs;

Note that the syntax can be simplified to :ada:`is null record`, which is much
more common than the previous form:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Null_Record

    package Null_Recs is

       type Null_Record is null record;

    end Null_Recs;

Although a null record doesn't have components, we can still specify
subprograms for it. For example, we could specify an addition operation for it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Null_Record

    package Null_Recs is

       type Null_Record is null record;

       function "+" (A, B : Null_Record)
                     return Null_Record;

    end Null_Recs;

    package body Null_Recs is

       function "+" (A, B : Null_Record)
                     return Null_Record
       is
          pragma Unreferenced (A, B);
       begin
          return (null record);
       end "+";

    end Null_Recs;

    with Null_Recs; use Null_Recs;

    procedure Show_Null_Rec is
       A, B : Null_Record;
    begin
       B := A + A;
       A := A + B;
    end Show_Null_Rec;

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.3.1 Record Aggregates <4-3-1>`

Simple Prototyping
~~~~~~~~~~~~~~~~~~

A null record doesn't provide much functionality on itself, as we're not
storing any information in it. However, it's far from being useless. For
example, we can make use of null records to design an API, which we can then
use in an application without having to implement the actual functionality of
the API. This allows us to design a prototype without having to think about all
the implementation details of the API in the first stage.

Consider this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Device

    package Devices is

       type Device is private;

       function Create
         (Active : Boolean)
          return Device;

       procedure Reset
         (D : out Device) is null;

       procedure Process
         (D : in out Device) is null;

       procedure Activate
         (D : in out Device) is null;

       procedure Deactivate
         (D : in out Device) is null;

    private

       type Device is null record;

       function Create (Active : Boolean)
                        return Device is
         (null record);

    end Devices;

    with Ada.Text_IO; use Ada.Text_IO;
    with Devices;     use Devices;

    procedure Show_Device is
       A : Device;
    begin
       Put_Line ("Creating device...");
       A := Create (Active => True);

       Put_Line ("Processing on device...");
       Process (A);

       Put_Line ("Deactivating device...");
       Deactivate (A);

       Put_Line ("Activating device...");
       Activate (A);

       Put_Line ("Resetting device...");
       Reset (A);
    end Show_Device;

In the :ada:`Devices` package, we're declaring the :ada:`Device` type and its
primitive subprograms: :ada:`Create`, :ada:`Reset`, :ada:`Process`,
:ada:`Activate` and :ada:`Deactivate`. This is the API that we use in our
prototype. Note that, although the :ada:`Device` type is declared as a private
type, it's still defined as a null record in the full view.

In this example, the :ada:`Create` function, implemented as an expression
function in the private part, simply returns a null record. As expected, this
null record returned by :ada:`Create` matches the definition of the
:ada:`Device` type.

All procedures associated with the :ada:`Device` type are implemented as null
procedures, which means they don't actually have an implementation nor have any
effect. We'll discuss this topic
:ref:`later on in the course <Adv_Ada_Null_Procedures>`.

In the :ada:`Show_Device` procedure |mdash| which is an application
that implements our prototype |mdash|, we declare an object of :ada:`Device`
type and call all subprograms associated with that type.

Extending the prototype
~~~~~~~~~~~~~~~~~~~~~~~

Because we're either using expression functions or null procedures in the
specification of the :ada:`Devices` package, we don't have a package body for
it (as there's nothing to be implemented). We could, however, move those user
messages from the :ada:`Show_Devices` procedure to a dummy implementation of
the :ada:`Devices` package. This is the adapted code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Device

    package Devices is

       type Device is null record;

       function Create (Active : Boolean)
                        return Device;

       procedure Reset (D : out Device);

       procedure Process (D : in out Device);

       procedure Activate (D : in out Device);

       procedure Deactivate (D : in out Device);

    end Devices;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Devices is

       function Create (Active : Boolean)
                        return Device
       is
          pragma Unreferenced (Active);
       begin
          Put_Line ("Creating device...");
          return (null record);
       end Create;

       procedure Reset (D : out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Processing on device...");
       end Reset;

       procedure Process (D : in out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Deactivating device...");
       end Process;

       procedure Activate (D : in out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Activating device...");
       end Activate;

       procedure Deactivate (D : in out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Resetting device...");
       end Deactivate;

    end Devices;

    with Devices; use Devices;

    procedure Show_Device is
       A : Device;
    begin
       A := Create (Active => True);
       Process (A);
       Deactivate (A);
       Activate (A);
       Reset (A);
    end Show_Device;

As we changed the specification of the :ada:`Devices` package to not use null
procedures, we now need a corresponding package body for it. In this package
body, we  implement the operations on the :ada:`Device` type, which actually
just display a user message indicating which operation is being called.

Let's focus on this updated version of the :ada:`Show_Device` procedure. Now
that we've removed all those calls to :ada:`Put_Line` from this procedure and
just have the calls to operations associated with the :ada:`Device` type, it
becomes more apparent that, even though :ada:`Device` is just a null record, we
can design an application with a sequence of various commands operating on it.
Also, when we just read the source-code of the :ada:`Show_Device` procedure,
there's no clear indication that the :ada:`Device` type doesn't actually hold
any information.

More complex applications
~~~~~~~~~~~~~~~~~~~~~~~~~

As we've just seen, we can use null records like any other type and create
complex prototypes with them. We could, for instance, design an application
that makes use of many null records, or even have types that depend on or
derive from null records. Let's see a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Derived_Device

    package Many_Devices is

       type Device is null record;

       type Device_Config is null record;

       function Create (Config : Device_Config)
                        return Device is
         (null record);

       type Derived_Device is new Device;

       procedure Process (D : Derived_Device) is null;

    end Many_Devices;

    with Many_Devices; use Many_Devices;

    procedure Show_Derived_Device is
       A : Device;
       B : Derived_Device;
       C : Device_Config;
    begin
       A := Create (Config => C);
       B := Create (Config => C);

       Process (B);
    end Show_Derived_Device;

In this example, the :ada:`Create` function has a null record parameter
(of :ada:`Device_Config` type) and returns a null record (of :ada:`Device`
type). Also, we derive the :ada:`Derived_Device` type from the :ada:`Device`
type. Consequently, :ada:`Derived_Device` is also a null record (since it's
derived from a null record). In the :ada:`Show_Derived_Device` procedure, we
declare objects of those types (:ada:`A`, :ada:`B` and :ada:`C`) and call
primitive subprograms to operate on them.

This example shows that, even though the types we've declared are *just* null
records, they can still be used to represent dependencies in our application.

Implementing the API
~~~~~~~~~~~~~~~~~~~~

Let's focus again on the previous example. After we have an initial prototype,
we can start implementing some of the functionality needed for the
:ada:`Device` type. For example, we can store information about the current
activation state in the record:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Device

    package Devices is

       type Device is private;

       function Create (Active : Boolean)
                        return Device;

       procedure Reset (D : out Device);

       procedure Process (D : in out Device);

       procedure Activate (D : in out Device);

       procedure Deactivate (D : in out Device);

    private

       type Device is record
          Active : Boolean;
       end record;

    end Devices;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Devices is

       function Create (Active : Boolean)
                        return Device
       is
          pragma Unreferenced (Active);
       begin
          Put_Line ("Creating device...");
          return (Active => Active);
       end Create;

       procedure Reset (D : out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Processing on device...");
       end Reset;

       procedure Process (D : in out Device)
       is
          pragma Unreferenced (D);
       begin
          Put_Line ("Deactivating device...");
       end Process;

       procedure Activate (D : in out Device)
       is
       begin
          Put_Line ("Activating device...");
          D.Active := True;
       end Activate;

       procedure Deactivate (D : in out Device)
       is
       begin
          Put_Line ("Resetting device...");
          D.Active := False;
       end Deactivate;

    end Devices;

    with Ada.Text_IO; use Ada.Text_IO;
    with Devices;     use Devices;

    procedure Show_Device is
       A : Device;
    begin
       A := Create (Active => True);
       Process (A);
       Deactivate (A);
       Activate (A);
       Reset (A);
    end Show_Device;

Now, the :ada:`Device` record contains an :ada:`Active` component, which is
used in the updated versions of :ada:`Create`, :ada:`Activate` and
:ada:`Deactivate`.

Note that we haven't done any change to the implementation of the
:ada:`Show_Device` procedure: it's still the same application as before. As
we've been hinting in the beginning, using null records makes it easy for us to
first create a prototype |mdash| as we did in the :ada:`Show_Device` procedure
|mdash| and postpone the API implementation to a later phase of the project.

Tagged null records
~~~~~~~~~~~~~~~~~~~

A null record may be tagged, as we can see in this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Tagged_Null_Record

    package Null_Recs is

       type Tagged_Null_Record is
         tagged null record;

       type Abstract_Tagged_Null_Record is
         abstract tagged null record;

    end Null_Recs;

As we see in this example, a type can be :ada:`tagged`, or even
:ada:`abstract tagged`. We discuss abstract types
:ref:`later on in the course <Adv_Ada_Abstract_Types_And_Subprograms>`.

As expected, in addition to deriving from tagged types, we can also extend
them. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Null_Records.Extended_Device

    package Devices is

       type Device is private;

       function Create (Active : Boolean)
                        return Device;

       type Derived_Device is private;

    private

       type Device is tagged null record;

       function Create (Active : Boolean)
                        return Device is
         (null record);

       type Derived_Device is new Device with record
          Active : Boolean;
       end record;

       function Create (Active : Boolean)
                        return Derived_Device is
         (Active => Active);

    end Devices;

In this example, we derive :ada:`Derived_Device` from the :ada:`Device` type
and extend it with the :ada:`Active` component. (Because we have a type
extension, we also need to override the :ada:`Create` function.)

Since we're now introducing elements from object-oriented programming, we could
consider using interfaces instead of null records. We'll discuss this topic
:ref:`later on in the course <Adv_Ada_Null_Records_Vs_Interfaces>`.



.. _Adv_Ada_Per_Object_Expressions:

Per-Object Expressions
----------------------

In record type declarations, we might want to define a component that makes use
of a name that refers to a discriminant of the record type, or to the record
type itself. An expression where we use such a name is called a per-object
expression.

The term "per-object" comes from the fact that, in the component definition,
we're referring to a piece of information that will be known just when creating
an object of that type. For example, if the per-object expression refers to a
discriminant of a type :ada:`T`, the actual value of that discriminant will
only be specified when we declare an object of type :ada:`T`. Therefore, the
component definition is specific for that individual object |mdash| but not
necessarily for other objects of the same type, as we might use different
values for the discriminant.

The constraint that contains a per-object expression is called a per-object
constraint. The actual constraint of that component isn't completely known when
we declare the record type, but only later on when an object of that type is
created. (Note that the syntax of a constraint includes the parentheses or the
keyword :ada:`range`.)

In addition to referring to discriminants, per-object expressions can also
refer to the record type itself, as we'll see later.

Let's start with a simple record declaration:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression

    package Rec_Per_Object_Expressions is

       type Stack (S : Positive) is private;

    private

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Stack (S : Positive) is record
          Arr : Integer_Array (1 .. S);
          --                   ^^^^^^
          --
          --                        S
          --                        ^
          --    Per-object expression
          --
          --                  (1 .. S)
          --                  ^^^^^^^^
          --     Per-object constraint

          Top : Natural := 0;
       end record;

    end Rec_Per_Object_Expressions;

In this example, we see the :ada:`Stack` record type with a discriminant
:ada:`S`. In the declaration of the :ada:`Arr` component of the that type,
:ada:`S` is a per-object expression, as it refers to the :ada:`S` discriminant.
Also, :ada:`(1 .. S)` is a per-object constraint.

Let's look at another example using :ref:`anonymous access types <Adv_Ada_Anonymous_Access_Types>`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Access_Discriminant

    package Rec_Per_Object_Expressions is

       type T is private;

       type T_Processor (Selected_T : access T) is
         private;

    private

       type T is null record;

       type T_Container (Selected_T : access T) is
         null record;

       type T_Processor (Selected_T : access T) is
       record
          E : T_Container (Selected_T);
          --
          --               Selected_T
          --               ^^^^^^^^^^
          --    Per-object expression
          --
          --              (Selected_T)
          --              ^^^^^^^^^^^^
          --     Per-object constraint
       end record;

    end Rec_Per_Object_Expressions;

Let's focus on the :ada:`T_Processor` type from this example. The
:ada:`Selected_T` discriminant is being used in the definition of the :ada:`E`
component. The per-object constraint is :ada:`(Selected_T)`.

Finally, per-object expressions can also refer to the record type we're
declaring. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Access_Discriminant

    package Rec_Per_Object_Expressions is

       type T is limited private;

    private

       type T_Processor (Selected_T : access T) is
         null record;

       type T is limited record
          E : T_Processor (T'Access);
          --
          --               T'Access
          --               ^^^^^^^^
          --  Per-object expression
          --
          --              (T'Access)
          --              ^^^^^^^^^^
          --   Per-object constraint
       end record;

    end Rec_Per_Object_Expressions;

In this example, when we write :ada:`T'Access` within the declaration of the
:ada:`T` record type, the actual value for the :ada:`Access` attribute will be
known when an object of :ada:`T` type is created. In that sense,
:ada:`T'Access` is a per-object expression |mdash| :ada:`(T'Access)` is the
corresponding per-object constraint.

Note that :ada:`T'Access` is referring to the type within a type definition.
This is generally treated as a reference to the object being created, the
so-called *current instance*.

.. todo::

    - Add link to Adv_Ada_Reference_Current_Instance section ("Access Types:
      Reference to current instance") once it's available.

.. admonition:: Relevant topics

   - :arm22:`3.8 Record Types <3-8>`


.. _Adv_Ada_Per_Object_Expressions_Restrictions:

Restrictions
~~~~~~~~~~~~

There are some important restrictions on per-object constraints:

#. We can only use access attributes (:ada:`T'Access` and
   :ada:`T'Unchecked_Access`) in per-object range constraints.

    - Per-object range constraints such as :ada:`1 .. T'Size` are not allowed.

    - For example, the following code example doesn't compile:

        .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Range_Constraint
            :class: ada-expect-compile-error

            package Rec_Per_Object_Expressions is

               type Bit_Field is
                 array (Positive range <>) of Boolean
                   with Pack;

               type T is record
                  Arr : Bit_Field (1 .. T'Size);
                  --                    ^^^^^^
                  --  ERROR: per-object range constraint
                  --         using the Size attribute
                  --         is illegal.
               end record;

            end Rec_Per_Object_Expressions;

#. Within a per-object index constraint or discriminant constraint, each
   per-object expression must be the name of a discriminant directly, without
   any further computation.

    - Therefore, we're allowed to write :ada:`(1 .. S)` |mdash| as we've seen
      in a previous example |mdash|. However, writing :ada:`(1 .. S - 1)` would
      be illegal.

    - For example, the following adaptation to the previous code example
      doesn't compile:

        .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression
            :class: ada-expect-compile-error

            package Rec_Per_Object_Expressions is

               type Stack (S : Positive) is private;

            private

               type Integer_Array is
                 array (Natural range <>) of Integer;

               type Stack (S : Positive) is record
                  Arr : Integer_Array (0 .. S - 1);
                  --                        ^^^^^
                  --  ERROR: computation in per-object
                  --         expression is illegal.

                  Top : Integer := -1;
               end record;

            end Rec_Per_Object_Expressions;

    .. admonition:: Important

        On the other hand, the default expression for a component of a
        discriminated record can be an arbitrary per-object expression. This
        might include function calls or uses of any defined operator. For this
        reason, the following code example is accepted by the compiler:

        .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression

            package Rec_Per_Object_Expressions is

               type Stack (S : Positive) is private;

            private

               type Integer_Array is
                 array (Positive range <>) of Integer;

               type Stack (S : Positive) is record
                  Arr : Integer_Array (1 .. S);

                  Top : Natural := 0;

                  Overflow_Warning : Positive
                    := S * 9 / 10;
                  --   ^^^^^^^^^^
                  --   Per-object expression
                  --   using computation for
                  --   the default expression.
               end record
                 with
                   Dynamic_Predicate =>
                     Overflow_Warning in
                       (S + 1) / 2 .. S - 1;
                  --
                  --   (S + 1) / 2
                  --   ^^^^^^^^^^^
                  --   Per-object expression
                  --   using computation.
                  --
                  --                  S - 1
                  --                  ^^^^^
                  --   Per-object expression
                  --   using computation.

            end Rec_Per_Object_Expressions;
