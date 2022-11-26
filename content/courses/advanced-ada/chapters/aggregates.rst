Aggregates
==========

.. include:: ../../global.txt

.. _Adv_Ada_Null_Records:

Null records
------------

A null record is a record that doesn't have any components. Consequently, it
cannot store any information. When declaring a null record, we simply
write :ada:`null` instead of declaring actual components, as we usually do for
records. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Null_Record

    package Null_Recs is

       type Null_Record is record
          null;
       end record;

    end Null_Recs;

Note that the syntax can be simplified to :ada:`is null record`, which is much
more common than the previous form:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Null_Record

    package Null_Recs is

       type Null_Record is null record;

    end Null_Recs;

Although a null record doesn't have components, we can still specify
subprograms for it. For example, we could specify an addition operation for it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Null_Record

    package Null_Recs is

       type Null_Record is null record;

       function "+" (A, B : Null_Record) return Null_Record;

    end Null_Recs;

    package body Null_Recs is

       function "+" (A, B : Null_Record) return Null_Record is
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

    - :arm:`4.3.1 Record Aggregates <4-3-1>`

Simple Prototyping
~~~~~~~~~~~~~~~~~~

A null record doesn't provide much functionality on itself, as we're not
storing any information in it. However, it's far from being useless. For
example, we can make use of null records to design an API, which we can then
use in an application without having to implement the actual functionality of
the API. This allows us to design a prototype without having to think about all
the implementation details of the API in the first stage.

Consider this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Device

    package Devices is

       type Device is private;

       function Create (Active : Boolean) return Device;

       procedure Reset (D : out Device) is null;

       procedure Process (D : in out Device) is null;

       procedure Activate (D : in out Device) is null;

       procedure Deactivate (D : in out Device) is null;

    private

       type Device is null record;

       function Create (Active : Boolean) return Device
         is (null record);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Device

    package Devices is

       type Device is null record;

       function Create (Active : Boolean) return Device;

       procedure Reset (D : out Device);

       procedure Process (D : in out Device);

       procedure Activate (D : in out Device);

       procedure Deactivate (D : in out Device);

    end Devices;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Devices is

       function Create (Active : Boolean) return Device is
          pragma Unreferenced (Active);
       begin
          Put_Line ("Creating device...");
          return (null record);
       end Create;

       procedure Reset (D : out Device) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Processing on device...");
       end Reset;

       procedure Process (D : in out Device) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Deactivating device...");
       end Process;

       procedure Activate (D : in out Device) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Activating device...");
       end Activate;

       procedure Deactivate (D : in out Device) is
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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Derived_Device

    package Many_Devices is

       type Device is null record;

       type Device_Config is null record;

       function Create (Config : Device_Config) return Device
         is (null record);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Device

    package Devices is

       type Device is private;

       function Create (Active : Boolean) return Device;

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

       function Create (Active : Boolean) return Device is
          pragma Unreferenced (Active);
       begin
          Put_Line ("Creating device...");
          return (Active => Active);
       end Create;

       procedure Reset (D : out Device) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Processing on device...");
       end Reset;

       procedure Process (D : in out Device) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Deactivating device...");
       end Process;

       procedure Activate (D : in out Device) is
       begin
          Put_Line ("Activating device...");
          D.Active := True;
       end Activate;

       procedure Deactivate (D : in out Device) is
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Tagged_Null_Record

    package Null_Recs is

       type Tagged_Null_Record is tagged null record;

       type Abstract_Tagged_Null_Record is abstract tagged null record;

    end Null_Recs;

As we see in this example, a type can be :ada:`tagged`, or even
:ada:`abstract tagged`. We discuss abstract types
:ref:`later on in the course <Adv_Ada_Abstract_Types_And_Subprograms>`.

As expected, in addition to deriving from tagged types, we can also extend
them. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Extended_Device

    package Devices is

       type Device is private;

       function Create (Active : Boolean) return Device;

       type Derived_Device is private;

    private

       type Device is tagged null record;

       function Create (Active : Boolean) return Device
         is (null record);

       type Derived_Device is new Device with record
          Active : Boolean;
       end record;

       function Create (Active : Boolean) return Derived_Device
         is (Active => Active);

    end Devices;

In this example, we derive :ada:`Derived_Device` from the :ada:`Device` type
and extend it with the :ada:`Active` component. (Because we have a type
extension, we also need to override the :ada:`Create` function.)

Since we're now introducing elements from object-oriented programming, we could
consider using interfaces instead of null records. We'll discuss this topic
:ref:`later on in the course <Adv_Ada_Null_Records_Vs_Interfaces>`.


.. _Adv_Ada_Full_Coverage_Rules_Aggregates:

Full coverage rules for Aggregates
----------------------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #1: Limited Types in Ada 2005 <https://www.adacore.com/gems/gem-1>`_.

One interesting feature of Ada are the *full coverage rules* for
aggregates. For example, suppose we have a record type:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Persons is
       type Years is new Natural;

       type Person is record
          Name : Ada.Strings.Unbounded.Unbounded_String;
          Age  : Years;
       end record;
    end Persons;

We can create an object of the type using an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Init is

       X : constant Person :=
             (Name => To_Unbounded_String ("John Doe"),
              Age  => 25);
    begin
       null;
    end Show_Aggregate_Init;

The full coverage rules say that every component of :ada:`Person` must be
accounted for in the aggregate. If we later modify type :ada:`Person` by
adding a component:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Persons is
       type Years is new Natural;

       type Person is record
          Name      : Unbounded_String;
          Age       : Natural;
          Shoe_Size : Positive;
       end record;
    end Persons;

and we forget to modify :ada:`X` accordingly, the compiler will remind us.
Case statements also have full coverage rules, which serve a similar
purpose.

Of course, we can defeat the full coverage rules by using :ada:`others`
(usually for array aggregates and case statements, but occasionally useful
for record aggregates):

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Init_Others is

       X : constant Person :=
             (Name   => To_Unbounded_String ("John Doe"),
              others => 25);
    begin
       null;
    end Show_Aggregate_Init_Others;

According to the Ada RM, :ada:`others` here means precisely the same thing
as :ada:`Age | Shoe_Size`. But that's wrong: what :ada:`others` really
means is "all the other components, including the ones we might add next
week or next year". That means you shouldn't use :ada:`others` unless
you're pretty sure it should apply to all the cases that haven't been
invented yet.

Later on, we'll discuss
:ref:`full coverage rules for limited types <Adv_Ada_Full_Coverage_Rules_Limited_Types>`.


Extension Aggregates
--------------------

Extension aggregates provide a convenient way to express an aggregate for a
type that extends |mdash| adds components to |mdash| some existing type (the
"ancestor"). Although mainly a matter of convenience, an extension aggregate is
essential when we want to express an aggregate for an extension of a private
ancestor type, that is, when we don't have compile-time visibility to the
ancestor type's components.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.3.2 Extension Aggregates <4-3-2>`

Assignments to objects of derived types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before we discuss extension aggregates in more detail, though, let's start
with a simple use-case. Let's say we have:

- an object :ada:`A` of tagged type :ada:`T1`, and

- an object :ada:`B` of tagged type :ada:`T2`, which extends :ada:`T1`.

We can initialize object :ada:`B` by:

- copying the :ada:`T1` specific information from :ada:`A` to :ada:`B`, and

- initializing the :ada:`T2` specific components of :ada:`B`.

We can translate the description above to the following code:

.. code-block:: ada

       A : T1;
       B : T2;
    begin
       T1 (B) := A;

       B.Extended_Component_1 := Some_Value;
       --  [...]

Here, we use :ada:`T1 (B)` to select the ancestor view of object :ada:`B`, and
we copy all the information from :ada:`A` to this part of :ada:`B`. Then, we
initialize the remaining components of :ada:`B`. We'll elaborate on this kind
of assignments later on.

Example: :ada:`Points`
~~~~~~~~~~~~~~~~~~~~~~

To present a more concrete example, let's start with a package that defines
one, two and three-dimensional point types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    package Points is

       type Point_1D is tagged record
          X : Float;
       end record;

       procedure Display (P : Point_1D);

       type Point_2D is new Point_1D with record
          Y : Float;
       end record;

       procedure Display (P : Point_2D);

       type Point_3D is new Point_2D with record
          Z : Float;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_1D) is
       begin
          Put_Line ("(X => " & P.X'Image & ")");
       end Display;

       procedure Display (P : Point_2D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image & ")");
       end Display;

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image
                    & ", Z => " & P.Z'Image & ")");
       end Display;

    end Points;

Let's now focus on the :ada:`Show_Points` procedure below, where we initialize
a two-dimensional point using a one-dimensional point.

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    with Points; use Points;

    procedure Show_Points is
       P_1D : Point_1D;
       P_2D : Point_2D;
    begin
       P_1D := (X => 0.5);
       Display (P_1D);

       Point_1D (P_2D) := P_1D;
       --  Equivalent to: "P_2D.X := P_1D.X;"

       P_2D.Y := 0.7;

       Display (P_2D);
    end Show_Points;

In this example, we're initializing :ada:`P_2D` using the information stored in
:ada:`P_1D`. By writing :ada:`Point_1D (P_2D)` on the left side of the
assignment, we specify that we want to limit our focus on the :ada:`Point_1D`
view of the :ada:`P_2D` object. Then, we assign :ada:`P_1D` to the
:ada:`Point_1D` view of the :ada:`P_2D` object. This assignment initializes the
:ada:`X` component of the :ada:`P_2D` object. The :ada:`Point_2D` specific
components are not changed by this assignment. (In other words, this is
equivalent to just writing :ada:`P_2D.X := P_1D.X`, as the :ada:`Point_1D` type
only has the :ada:`X` component.) Finally, in the next line, we initialize the
:ada:`Y` component with 0.7.

Using extension aggregates
~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that, in the assignment to :ada:`P_1D`, we use a record aggregate.
Extension aggregates are similar to record aggregates, but they include the
:ada:`with` keyword |mdash| for example: :ada:`(Obj1 with Y => 0.5)`. This
allows us to assign to an object with information from another object
:ada:`Obj1` of a parent type and, in the same expression, set the value of the
:ada:`Y` component of the type extension.

Let's rewrite the previous :ada:`Show_Points` procedure using extension
aggregates:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    with Points; use Points;

    procedure Show_Points is
       P_1D : Point_1D;
       P_2D : Point_2D;
    begin
       P_1D := (X => 0.5);
       Display (P_1D);

       P_2D := (P_1D with Y => 0.7);
       Display (P_2D);
    end Show_Points;

When we write :ada:`P_2D := (P_1D with Y => 0.7)`, we're initializing
:ada:`P_2D` using:

- the information from the :ada:`P_1D` object |mdash| of :ada:`Point_1D` type,
  which is an ancestor of the :ada:`Point_2D` type |mdash|, and

- the information from the record component association list for the
  remaining components of the :ada:`Point_2D` type. (In this case, the only
  remaining component of the :ada:`Point_2D` type is :ada:`Y`.)

We could also specify the type of the extension aggregate. For example, in the
previous assignment to :ada:`P_2D`, we could write :ada:`Point_2D'(...)` to
indicate that we expect the :ada:`Point_2D` type for the extension aggregate.

.. code-block:: ada

    --  Explicitly state that the type of the
    --  extension aggregate is Point_2D:

    P_2D := Point_2D'(P_1D with Y => 0.7);

Also, we don't have to use named association in extension aggregates. We
could just use positional association instead. Therefore, we could simplify the
assignment to :ada:`P_2D` in the previous example by just writing:

.. code-block:: ada

    P_2D := (P_1D with 0.7);

More extension aggregates
~~~~~~~~~~~~~~~~~~~~~~~~~

We can use extension aggregates for descendents of the :ada:`Point_2D` type as
well. For example, let's extend our previous code example by declaring an
object of :ada:`Point_3D` type (called :ada:`P_3D`) and use extension
aggregates in assignments to this object:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    with Points; use Points;

    procedure Show_Points is
       P_1D : Point_1D;
       P_2D : Point_2D;
       P_3D : Point_3D;
    begin
       P_1D := (X => 0.5);
       Display (P_1D);

       P_2D := (P_1D with Y => 0.7);
       Display (P_2D);

       P_3D := (P_2D with Z => 0.3);
       Display (P_3D);

       P_3D := (P_1D with Y | Z => 0.1);
       Display (P_3D);
    end Show_Points;

In the first assignment to :ada:`P_3D` in the example above, we're
initializing this object with information from :ada:`P_2D` and specifying
the value of the :ada:`Z` component. Then, in the next assignment to the
:ada:`P_3D` object, we're using an aggregate with information from :ada:`P_1`
and specifying values for the :ada:`Y` and :ada:`Z` components. (Just as a
reminder, we can write :ada:`Y | Z => 0.1` to assign 0.1 to both :ada:`Y` and
:ada:`Z` components.)

:ada:`with others`
~~~~~~~~~~~~~~~~~~

Other versions of extension aggregates are possible as well. For example, we
can combine keywords and write :ada:`with others` to focus on all remaining
components of an extension aggregate.

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    with Points; use Points;

    procedure Show_Points is
       P_1D : Point_1D;
       P_2D : Point_2D;
       P_3D : Point_3D;
    begin
       P_1D := (X => 0.5);
       P_2D := (P_1D with Y => 0.7);

       --  Initialize P_3D with P_1D and set other
       --  components to 0.6.
       --
       P_3D := (P_1D with others => 0.6);
       Display (P_3D);

       --  Initialize P_3D with P_2D, and other
       --  components with their default value.
       --
       P_3D := (P_2D with others => <>);
       Display (P_3D);
    end Show_Points;

In this example, the first assignment to :ada:`P_3D` has an aggregate with
information from :ada:`P_1D`, while the remaining components |mdash| in this
case, :ada:`Y` and :ada:`Z` |mdash| are just set to 0.6.

Continuing with this example, in the next assignment to :ada:`P_3D`, we're
using information from :ada:`P_2` in the extension aggregate. This covers the
:ada:`Point_2D` part of the :ada:`P_3D` object |mdash| components :ada:`X` and
:ada:`Y`, to be more specific. The :ada:`Point_3D` specific components of
:ada:`P_3D` |mdash| component :ada:`Z` in this case |mdash| receive their
corresponding default value. In this specific case, however, we haven't
specified a default value for component :ada:`Z` in the declaration of the
:ada:`Point_3D` type, so we cannot rely on any specific value being assigned to
that component when using :ada:`others => <>`.

:ada:`with null record`
~~~~~~~~~~~~~~~~~~~~~~~

We can also use extension aggregates with null records. Let's focus on the
:ada:`P_3D_Ext` object of :ada:`Point_3D_Ext` type. This object is declared in
the :ada:`Show_Points` procedure of the next code example.

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    package Points.Extensions is

       type Point_3D_Ext is new Point_3D with null record;

    end Points.Extensions;

    with Points;            use Points;
    with Points.Extensions; use Points.Extensions;

    procedure Show_Points is
       P_3D     : Point_3D;
       P_3D_Ext : Point_3D_Ext;
    begin
       P_3D := (X => 0.0, Y => 0.5, Z => 0.4);

       P_3D_Ext := (P_3D with null record);
       Display (P_3D_Ext);
    end Show_Points;

The :ada:`P_3D_Ext` object is of :ada:`Point_3D_Ext` type, which is declared in
the :ada:`Points.Extensions` package and derived from the :ada:`Point_3D` type.
Note that we're not extending :ada:`Point_3D_Ext` with new components, but
using a null record instead in the declaration. Therefore, as the
:ada:`Point_3D_Ext` type doesn't own any new components, we just write
:ada:`(P_3D with null record)` to initialize the :ada:`P_3D_Ext` object.

Extension aggregates and descendent types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the examples above, we've been initializing objects of descendent types by
using objects of ascending types in extension aggregates. We could, however, do
the opposite and initialize objects of ascending types using objects of
descendent type in extension aggregates. Consider this code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Extension_Aggregate_Points

    with Points; use Points;

    procedure Show_Points is
       P_2D : Point_2D;
       P_3D : Point_3D;
    begin
       P_3D := (X => 0.5, Y => 0.7, Z => 0.3);
       Display (P_3D);

       P_2D := (Point_1D (P_3D) with Y => 0.3);
       Display (P_2D);
    end Show_Points;

Here, we're using :ada:`Point_1D (P_3D)` to select the :ada:`Point_1D` view of
an object of :ada:`Point_3D` type. At this point, we have specified the
:ada:`Point_1D` part of the aggregate, so we still have to specify the
remaining components of the :ada:`Point_2D` type |mdash| the :ada:`Y`
component, to be more specific. When we do that, we get the appropriate
aggregate for the :ada:`Point_2D` type. In summary, by carefully selecting the
appropriate view, we're able to initialize an object of ascending type
(:ada:`Point_2D`), which contains less components, using an object of a
descendent type (:ada:`Point_3D`), which contains more components.


Delta Aggregates
----------------

.. admonition:: Relevant topics

   - :arm22:`Delta Aggregates <4-3-4>`

.. todo::

   Complete section!


Container Aggregates
--------------------

.. admonition:: Relevant topics

   - :arm22:`Container Aggregates <4-3-5>`

.. todo::

   Complete section!
