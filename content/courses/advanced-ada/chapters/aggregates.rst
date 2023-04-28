Aggregates
==========

.. include:: ../../global.txt

.. _Adv_Ada_Container_Aggregates:

Container Aggregates
--------------------

.. note::

   This feature was introduced in Ada 2022.

A container aggregate is a list of elements |mdash| such as :ada:`[1, 2, 3]`
|mdash| that we use to initialize or assign to a container. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Simple_Container_Aggregate

    pragma Ada_2022;

    with Ada.Containers.Vectors;

    procedure Show_Container_Aggregate is

       package Float_Vec is new
         Ada.Containers.Vectors (Positive, Float);

       V : constant Float_Vec.Vector :=
             [1.0, 2.0, 3.0];

       pragma Unreferenced (V);
    begin
       null;
    end Show_Container_Aggregate;

In this example, :ada:`[1.0, 2.0, 3.0]` is a container aggregate that we use
to initialize a vector :ada:`V`.

We can specify container aggregates in three forms:

    - as a null container aggregate, which indicates a container without any
      elements and is represented by the :ada:`[]` syntax;

    - as a positional container aggregate, where the elements are simply
      listed in a sequence (such as :ada:`[1, 2]`);

    - as a named container aggregate, where a key is indicated for each element
      of the list (such as :ada:`[1 => 10, 2 => 15]`).

Let's look at a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Simple_Container_Aggregate

    pragma Ada_2022;

    with Ada.Containers.Vectors;

    procedure Show_Container_Aggregate is

       package Float_Vec is new
         Ada.Containers.Vectors (Positive, Float);

       --  Null container aggregate
       Null_V  : constant Float_Vec.Vector :=
                   [];

       --  Positional container aggregate
       Pos_V   : constant Float_Vec.Vector :=
                   [1.0, 2.0, 3.0];

       --  Named container aggregate
       Named_V : constant Float_Vec.Vector :=
                   [1 => 1.0,
                    2 => 2.0,
                    3 => 3.0];

       pragma Unreferenced (Null_V, Pos_V, Named_V);
    begin
       null;
    end Show_Container_Aggregate;

In this example, we see the three forms of container aggregates. The difference
between positional and named container aggregates is that:

    - for positional container aggregates, the vector index is implied by
      its position;

while

    - for named container aggregates, the index (or key) of each element is
      explicitly indicated.

Also, the named container aggregate in this example (:ada:`Named_V`) is using
an index as the name (i.e. it's an indexed aggregate). Another option is to use
non-indexed aggregates, where we use actual keys |mdash| as we do in maps.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Named_Container_Aggregate

    pragma Ada_2022;

    with Ada.Containers.Vectors;
    with Ada.Containers.Indefinite_Hashed_Maps;
    with Ada.Strings.Hash;

    procedure Show_Named_Container_Aggregate is

       package Float_Vec is new
         Ada.Containers.Vectors (Positive, Float);

       package Float_Hashed_Maps is new
         Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => String,
            Element_Type    => Float,
            Hash            => Ada.Strings.Hash,
            Equivalent_Keys => "=");

       --  Named container aggregate
       --  using an index
       Indexed_Named_V : constant Float_Vec.Vector :=
                           [1 => 1.0,
                            2 => 2.0,
                            3 => 3.0];

       --  Named container aggregate
       --  using a key
       Keyed_Named_V : constant
         Float_Hashed_Maps.Map :=
           ["Key_1" => 1.0,
            "Key_2" => 2.0,
            "Key_3" => 3.0];

       pragma Unreferenced (Indexed_Named_V,
                            Keyed_Named_V);
    begin
       null;
    end Show_Named_Container_Aggregate;

In this example, :ada:`Indexed_Named_V` and :ada:`Keyed_Named_V` are both
initialized with a named container aggregate. However:

- the container aggregate for :ada:`Indexed_Named_V` is an indexed aggregate,
  so we use an index for each element;

while

- the container aggregate for :ada:`Keyed_Named_V` has a key for each element.

Later on, we'll talk about the
:ref:`'Aggregate aspect <Adv_Ada_Aggregate_Aspect>`, which allows for
defining custom container aggregates for any record type.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.3.5 Container Aggregates <4-3-5>`


.. _Adv_Ada_Record_Aggregates:

Record aggregates
-----------------

We've already seen record aggregates in the
:ref:`Introduction to Ada <Intro_Ada_Record_Aggregates>` course, so this is just
a brief overview on the topic.

As we already know, record aggregates can have positional and named component
associations. For example, consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    package Points is

       type Point_3D is record
          X, Y, Z : Integer;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => "
                    & Integer'Image (P.X)
                    & ",");
          Put_Line (" Y => "
                    & Integer'Image (P.Y)
                    & ",");
          Put_Line (" Z => "
                    & Integer'Image (P.Z)
                    & ")");
       end Display;

    end Points;

We can use positional or named record aggregates when assigning to an object
:ada:`P` of :ada:`Point_3D` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D;
    begin
       --  Positional component association
       P := (0, 1, 2);

       Display (P);

       --  Named component association
       P := (X => 3,
             Y => 4,
             Z => 5);

       Display (P);
    end Show_Record_Aggregates;

Also, we can have a mixture of both:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D;
    begin
       --  Positional and named component associations
       P := (3, 4,
             Z => 5);

       Display (P);
    end Show_Record_Aggregates;

In this case, only the :ada:`Z` component has a named association, while the
other components have a positional association.

Note that a positional association cannot follow a named association, so we
cannot write :ada:`P := (3, Y => 4, 5);`, for example. Once we start using a
named association for a component, we have to continue using it for the
remaining components.

In addition, we can choose multiple components at once and assign the same value
to them. For that, we use the :ada:`|` syntax:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D;
    begin
       --  Multiple component selection
       P := (X | Y => 5,
             Z     => 6);

       Display (P);
    end Show_Record_Aggregates;

Here, we assign 5 to both :ada:`X` and :ada:`Y`.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.3.1 Record Aggregates <4-3-1>`


:ada:`<>`
~~~~~~~~~

We can use the :ada:`<>` syntax to tell the compiler to use the default value
for specific components. However, if there's no default value for specific
components, that component isn't initialized to a known value. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D;
    begin
       P := (0, 1, 2);
       Display (P);

       --  Specifying X component.
       P := (X => 42,
             Y => <>,
             Z => <>);
       Display (P);

       --  Specifying Y and Z components.
       P := (X => <>,
             Y => 10,
             Z => 20);
       Display (P);
    end Show_Record_Aggregates;

Here, as the components of :ada:`Point_3D` don't have a default value, those
components that have :ada:`<>` are not initialized:

- when we write :ada:`(X => 42, Y => <>, Z => <>)`, only :ada:`X` is
  initialized;

- when we write :ada:`(X => <>, Y => 10, Z => 20)` instead, only :ada:`X` is
  uninitialized.

.. admonition:: For further reading...

    As we've just seen, all components that get a :ada:`<>` are uninitialized
    because the components of :ada:`Point_3D` don't have a default value.
    As no initialization is taking place for those components of the aggregate,
    the actual value that is assigned to the record is undefined. In other
    words, the resulting behavior might dependent on the compiler's
    implementation.

    When using GNAT, writing :ada:`(X => 42, Y => <>, Z => <>)` keeps the value
    of :ada:`Y` and :ada:`Z` intact, while :ada:`(X => <>, Y => 10, Z => 20)`
    keeps the value of :ada:`X` intact.

If the components of :ada:`Point_3D` had default values, those would have been
used. For example, we may change the type declaration of :ada:`Point_3D` and use
default values for each component:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    package Points is

       type Point_3D is record
          X : Integer := 10;
          Y : Integer := 20;
          Z : Integer := 30;
       end record;

       procedure Display (P : Point_3D);

    end Points;

Then, writing :ada:`<>` makes use of those default values we've just specified:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D := (0, 0, 0);
    begin
       --  Using default value for
       --  all components
       P := (X => <>,
             Y => <>,
             Z => <>);
       Display (P);
    end Show_Record_Aggregates;

Now, as expected, the default values of each component (10, 20 and 30) are used
when we write :ada:`<>`.

Similarly, we can specify a default value for the type of each component. For
example, let's declare a :ada:`Point_Value` type with a default value |mdash|
using the :ada:`Default_Value` aspect |mdash| and use it in the :ada:`Point_3D`
record type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregate_Default_Value

    package Points is

       type Point_Value is new Float
         with Default_Value => 99.9;

       type Point_3D is record
          X : Point_Value;
          Y : Point_Value;
          Z : Point_Value;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => "
                    & Point_Value'Image (P.X)
                    & ",");
          Put_Line (" Y => "
                    & Point_Value'Image (P.Y)
                    & ",");
          Put_Line (" Z => "
                    & Point_Value'Image (P.Z)
                    & ")");
       end Display;

    end Points;

Then, writing :ada:`<>` makes use of the default value of the :ada:`Point_Value`
type:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregate_Default_Value

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D := (0.0, 0.0, 0.0);
    begin
       --  Using default value of Point_Value
       --  for all components
       P := (X => <>,
             Y => <>,
             Z => <>);
       Display (P);
    end Show_Record_Aggregates;

In this case, the default value of the :ada:`Point_Value` type (99.9) is used
for all components when we write :ada:`<>`.


:ada:`others`
~~~~~~~~~~~~~

Also, we can use the :ada:`others` selector to assign a value to all components
that aren't explicitly mentioned in the aggregate. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Pos_Named_Rec_Aggregates

    with Points; use Points;

    procedure Show_Record_Aggregates is
       P : Point_3D;
    begin
       --  Specifying X component;
       --  using 42 for all
       --  other components.
       P := (X      => 42,
             others => 100);
       Display (P);

       --  Specifying all components
       P := (others => 256);
       Display (P);
    end Show_Record_Aggregates;

When we write :ada:`P := (X => 42, others => 100)`, we're assigning 42 to
:ada:`X` and 100 to all other components (:ada:`Y` and :ada:`Z` in this case).
Also, when we write :ada:`P := (others => 256)`, all components have the
same value (256).

Note that writing a specific value in :ada:`others` |mdash| such as
:ada:`(others => 256)`  |mdash| only works when all components have the same
type. In this example, all components of :ada:`Point_3D` have the same type:
:ada:`Integer`. If we had components with different types in the components
selected by :ada:`others`, say :ada:`Integer` and :ada:`Float`, then
:ada:`(others => 256)` would trigger a compilation error. For example, consider
this package:

.. code:: ada no_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregates_Others

    package Custom_Records is

       type Integer_Float is record
         A, B : Integer := 0;
         Y, Z : Float   := 0.0;
       end record;

    end Custom_Records;

If we had written an aggregate such as :ada:`(others => 256)` for an object of
type :ada:`Integer_Float`, the value (256) would be OK for components :ada:`A`
and :ada:`B`, but not for components :ada:`Y` and :ada:`Z`:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregates_Others
    :class: ada-expect-compile-error

    with Custom_Records; use Custom_Records;

    procedure Show_Record_Aggregates_Others is
       Dummy : Integer_Float;
    begin
       --  ERROR: components selected by
       --         others must be of same
       --         type.
       Dummy := (others => 256);
    end Show_Record_Aggregates_Others;

We can fix this compilation error by making sure that :ada:`others` only refers
to components of the same type:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregates_Others

    with Custom_Records; use Custom_Records;

    procedure Show_Record_Aggregates_Others is
       Dummy : Integer_Float;
    begin
       --  OK: components selected by
       --      others have Integer type.
       Dummy := (Y | Z  => 256.0,
                 others => 256);
    end Show_Record_Aggregates_Others;

In any case, writing :ada:`(others => <>)` is always accepted by the compiler
because it simply selects the default value of each component, so the type of
those values is unambiguous:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregates_Others

    with Custom_Records; use Custom_Records;

    procedure Show_Record_Aggregates_Others is
       Dummy : Integer_Float;
    begin
       Dummy := (others => <>);
    end Show_Record_Aggregates_Others;

This code compiles because :ada:`<>` uses the appropriate default value of each
component.


Record discriminants
~~~~~~~~~~~~~~~~~~~~

When a record type has discriminants, they must appear as components of an
aggregate of that type. For example, consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregate_Discriminant

    package Points is

       type Point_Dimension is (Dim_1, Dim_2, Dim_3);

       type Point (D : Point_Dimension) is record
          case D is
          when Dim_1 =>
             X1         : Integer;
          when Dim_2 =>
             X2, Y2     : Integer;
          when Dim_3 =>
             X3, Y3, Z3 : Integer;
          end case;
       end record;

       procedure Display (P : Point);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point) is
       begin
          Put_Line (Point_Dimension'Image (P.D));

          case P.D is
          when Dim_1 =>
             Put_Line ("  (X => "
                       & Integer'Image (P.X1)
                       & ")");
          when Dim_2 =>
             Put_Line ("  (X => "
                       & Integer'Image (P.X2)
                       & ",");
             Put_Line ("   Y => "
                       & Integer'Image (P.Y2)
                      & ")");
          when Dim_3 =>
             Put_Line ("  (X => "
                       & Integer'Image (P.X3)
                       & ",");
             Put_Line ("   Y => "
                       & Integer'Image (P.Y3)
                       & ",");
             Put_Line ("   Z => "
                       & Integer'Image (P.Z3)
                       & ")");
          end case;
       end Display;

    end Points;

To write aggregates of the :ada:`Point` type, we have to specify the :ada:`D`
discriminant as a component of the aggregate. The discriminant must be included
in the aggregate |mdash| and must be static |mdash| because the compiler must
be able to examine the aggregate to determine if it is both complete and
consistent. All components must be accounted for one way or another, as usual
|mdash| but, in addition, references to those components whose existence
depends on the discriminant's values must be consistent with the actual
discriminant value used in the aggregate. For example, for type :ada:`Point`,
an aggregate can only reference the :ada:`X3`, :ada:`Y3`, and :ada:`Z3`
components when :ada:`Dim_3` is specified for the discriminant :ada:`D`;
otherwise, those three components don't exist in that aggregate. Also, the
discriminant :ada:`D` must be the first one if we use positional component
association. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Aggregate_Discriminant

    with Points; use Points;

    procedure Show_Rec_Aggregate_Discriminant is
       --  Positional component association
       P1 : constant Point := (Dim_1, 0);

       --  Named component association
       P2 : constant Point := (D  => Dim_2,
                               X2 => 3,
                               Y2 => 4);

       --  Positional / named component association
       P3 : constant Point := (Dim_3,
                               X3 => 3,
                               Y3 => 4,
                               Z3 => 5);
    begin
       Display (P1);
       Display (P2);
       Display (P3);
    end Show_Rec_Aggregate_Discriminant;

As we see in this example, we can use any component association in the
aggregate, as long as we make sure that the discriminants of the type appear as
components |mdash| and are the first components in the case of positional
component association.


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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Device

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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Derived_Device

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

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Device

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Tagged_Null_Record

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Extended_Device

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


.. _Adv_Ada_Full_Coverage_Rules_Aggregates:

Full coverage rules for Aggregates
----------------------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #1: Limited Types in Ada 2005 <https://www.adacore.com/gems/gem-1>`_.

One interesting feature of Ada are the *full coverage rules* for
aggregates. For example, suppose we have a record type:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package Persons is
       type Years is new Natural;

       type Person is record
          Name : Unbounded_String;
          Age  : Years;
       end record;
    end Persons;

We can create an object of the type using an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    with Persons; use Persons;

    procedure Show_Aggregate_Init is

       X : constant Person :=
             (Name =>
                To_Unbounded_String ("John Doe"),
              Age  => 25);
    begin
       null;
    end Show_Aggregate_Init;

The full coverage rules say that every component of :ada:`Person` must be
accounted for in the aggregate. If we later modify type :ada:`Person` by
adding a component:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

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
(usually for :ref:`array aggregates <Adv_Ada_Array_Aggregates>` and case
statements, but occasionally useful for
:ref:`record aggregates <Adv_Ada_Record_Aggregates>`):

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    with Persons; use Persons;

    procedure Show_Aggregate_Init_Others is

       X : constant Person :=
             (Name   =>
                To_Unbounded_String ("John Doe"),
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


.. _Adv_Ada_Array_Aggregates:

Array aggregates
----------------

We've already discussed array aggregates in the
:ref:`Introduction to Ada <Intro_Ada_Array_Type_Declaration>` course. Therefore,
this section just presents some details about this topic.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.3.3 Array Aggregates <4-3-3>`

Positional and named array aggregates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

   The array aggregate syntax using brackets (e.g.: :ada:`[1, 2, 3]`), which we
   mention in this section, was introduced in Ada 2022.

Similar to :ref:`record aggregates <Adv_Ada_Record_Aggregates>`, array
aggregates can be positional or named. Consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    package Points is

       type Point_3D is array (1 .. 3) of Integer;

       procedure Display (P : Point_3D);

    end Points;

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => "
                    & Integer'Image (P (1))
                    & ",");
          Put_Line (" Y => "
                    & Integer'Image (P (2))
                    & ",");
          Put_Line (" Z => "
                    & Integer'Image (P (3))
                    & ")");
       end Display;

    end Points;

We can write positional or named aggregates when assigning to an object :ada:`P`
of :ada:`Point_3D` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       --  Positional component association
       P := [0, 1, 2];

       Display (P);

       --  Named component association
       P := [1 => 3,
             2 => 4,
             3 => 5];

       Display (P);
    end Show_Array_Aggregates;

In this example, we assign a positional array aggregate (:ada:`[1, 2, 3]`) to
:ada:`P`. Then, we assign a named array aggregate
(:ada:`[1 => 3, 2 => 4, 3 => 5]`) to :ada:`P`. In this case, the *name* is the
index of  the component we're assigning to.

We can also assign array aggregates to slices:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D := [others => 0];
    begin
       --  Positional component association
       P (2 .. 3) := [1, 2];

       Display (P);

       --  Named component association
       P (2 .. 3) := [1 => 3,
                      2 => 4];

       Display (P);
    end Show_Array_Aggregates;

Note that, when using a named array aggregate, the index (*name*) that we use
in the aggregate doesn't have to match the slice. In this example, we're
assigning the component from index 1 of the aggregate to the component of index
2 of the array :ada:`P` (and so on).

.. admonition:: Historically

   In the first versions of Ada, we could only write array aggregates using
   parentheses.

    .. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

        pragma Ada_2012;

        with Points; use Points;

        procedure Show_Array_Aggregates is
           P : Point_3D;
        begin
           --  Positional component association
           P := (0, 1, 2);

           Display (P);

           --  Named component association
           P := (1 => 3,
                 2 => 4,
                 3 => 5);

           Display (P);
        end Show_Array_Aggregates;

    This syntax is considered obsolescent since Ada 2022: brackets
    (:ada:`[1, 2, 3]`) should be used instead.


Null array aggregate
~~~~~~~~~~~~~~~~~~~~

.. note::

   This feature was introduced in Ada 2022.

We can also write null array aggregates: :ada:`[]`. As the name implies, this
kind of array aggregate doesn't have any components.

Consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2

    package Integer_Arrays is

       type Integer_Array is
         array (Positive range <>) of Integer;

       procedure Display (A : Integer_Array);

    end Integer_Arrays;

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Arrays is

       procedure Display (A : Integer_Array) is
       begin
          Put_Line ("Length = "
                    & A'Length'Image);

          Put_Line ("(");
          for I in A'Range loop
             Put ("  "
                  & I'Image
                  & " => "
                  & A (I)'Image);
             if I /= A'Last then
                Put_Line (",");
             else
                New_Line;
             end if;
          end loop;
          Put_Line (")");
       end Display;

    end Integer_Arrays;

We can initialize an object :ada:`N` of :ada:`Integer_Array` type with a null
array aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2

    pragma Ada_2022;

    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Aggregates is
       N : constant Integer_Array := [];
    begin
       Display (N);
    end Show_Array_Aggregates;

In this example, when we call the :ada:`Display` procedure, we confirm that
:ada:`N` doesn't have any components.


:ada:`|`, :ada:`<>`, :ada:`others`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've seen the following syntactic elements when we were discussing
:ref:`record aggregates <Adv_Ada_Record_Aggregates>`: :ada:`|`, :ada:`<>` and
:ada:`others`. We can apply them to array aggregates as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       --  All components have a value of zero.
       P := [others => 0];

       Display (P);

       --  Both first and second components have
       --  a value of three.
       P := [1 | 2 => 3,
             3     => 4];

       Display (P);

       --  The default value is used for the first
       --  component, and all other components
       --  have a value of five.
       P := [1      => <>,
             others => 5];

       Display (P);
    end Show_Array_Aggregates;

In this example, we use the :ada:`|`, :ada:`<>` and :ada:`others` elements in a
very similar way as we did with record aggregates. (See the comments in the code
example for more details.)

Note that, as for record aggregates, the :ada:`<>` makes use of the default
value (if it is available). We discuss this topic in more details
:ref:`later on <Adv_Ada_Array_Aggregate_Box_Default_Value>`.

:ada:`..`
~~~~~~~~~

We can also use the range syntax (:ada:`..`) with array aggregates:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       --  All components have a value of zero.
       P := [1 .. 3 => 0];

       Display (P);

       --  Both first and second components have
       --  a value of three.
       P := [1 .. 2 => 3,
             3      => 4];

       Display (P);

       --  The default value is used for the first
       --  component, and all other components
       --  have a value of five.
       P := [1      => <>,
             2 .. 3 => 5];

       Display (P);
    end Show_Array_Aggregates;

This example is a variation of the previous one. However, in this case, we're
using ranges instead of the :ada:`|` and :ada:`others` syntax.


Missing components
~~~~~~~~~~~~~~~~~~

All components must have an associated value. If we don't specify a value for a
certain component, an exception is raised:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates
    :class: ada-run-expect-failure

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       P := [1 => 4];
       --  ERROR: value of components at indices
       --         2 and 3 are missing

       Display (P);
    end Show_Array_Aggregates;

We can use :ada:`others` to specify a value to all components that
haven't been explicitly mentioned in the aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       P := [1 => 4, others => 0];
       --  OK: unspecified components have a
       --      value of zero

       Display (P);
    end Show_Array_Aggregates;

However, :ada:`others` can only be used when the range is known |mdash|
compilation fails otherwise:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2
    :class: ada-expect-compile-error

    pragma Ada_2022;

    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Aggregates is
       N1 : Integer_Array := [others => 0];
       --  ERROR: range is unknown

       N2 : Integer_Array (1 .. 3) := [others => 0];
       --  OK: range is known
    begin
       Display (N1);
       Display (N2);
    end Show_Array_Aggregates;

Of course, we could fix the declaration of :ada:`N1` by specifying a range
|mdash| e.g. :ada:`N1 : Integer_Array (1 .. 10) := [others => 0];`.


Iterated component association
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

   This feature was introduced in Ada 2022.

We can use iterated component association to specify an aggregate. This is the
general syntax:

.. code-block:: ada

    --  All components have a value of zero
    P := [for I in 1 .. 3 => 0];

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D;
    begin
       --  All components have a value of zero
       P := [for I in 1 .. 3 => 0];

       Display (P);

       --  Both first and second components have
       --  a value of three
       P := [for I in 1 .. 3 =>
               (if I = 1 or I = 2
                then 3
                else 4)];

       Display (P);

       --  The first component has a value of 99
       --  and all other components have a value
       --  that corresponds to its index
       P := [1 => 99,
             for I in 2 .. 3 => I];

       Display (P);
    end Show_Array_Aggregates;

In this example, we use iterated component associations in different ways:

1. We write a simple iteration (:ada:`[for I in 1 .. 3 => 0]`).

2. We use a conditional expression in the iteration:
   :ada:`[for I in 1 .. 3 => (if I = 1 or I = 2 then 3 else 4)]`.

3. We use a named association for the first element, and then iterated component
   association for the remaining components:
   :ada:`[1 => 99, for I in 2 .. 3 => I]`.

So far, we've used a discrete choice list (in the :ada:`for I in Range` form) in
the iterated component association. We could use an iterator (in the
:ada:`for E of` form) instead. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Array_Aggregates is
       P : Point_3D := [for I in Point_3D'Range => I];
    begin
       --  Each component is doubled
       P := [for E of P => E * 2];

       Display (P);

       --  Each component is increased
       --  by one
       P := [for E of P => E + 1];

       Display (P);
    end Show_Array_Aggregates;

In this example, we use iterators in different ways:

1. We write :ada:`[for E of P => E * 2]` to double the value of each component.

2. We write :ada:`[for E of P => E + 1]` to increase the value of each component
   by one.

Of course, we could write more complex operations on :ada:`E` in the iterators.


Multidimensional array aggregates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So far, we've discussed one-dimensional array aggregates. We can also use the
same concepts when dealing with multidimensional arrays. Consider, for example,
this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Matrix_Aggregates

    package Matrices is

       type Matrix is array (Positive range <>,
                             Positive range <>)
                             of Integer;

       procedure Display (M : Matrix);

    end Matrices;

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Matrices is

       procedure Display (M : Matrix) is

          procedure Display_Row (M : Matrix;
                                 I : Integer) is
          begin
             Put_Line ("  (");
             for J in M'Range (2) loop
                Put ("    "
                     & J'Image
                     & " => "
                     & M (I, J)'Image);
                if J /= M'Last (2) then
                   Put_Line (",");
                else
                   New_Line;
                end if;
             end loop;
             Put ("  )");
          end Display_Row;

       begin
          Put_Line ("Length (1) = "
                    & M'Length (1)'Image);
          Put_Line ("Length (2) = "
                    & M'Length (2)'Image);

          Put_Line ("(");
          for I in M'Range (1) loop
             Display_Row (M, I);
             if I /= M'Last (1) then
                Put_Line (",");
             else
                New_Line;
             end if;
          end loop;
          Put_Line (")");

       end Display;

    end Matrices;

We can assign multidimensional aggregates to a matrix :ada:`M` using
positional or named component association:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Matrix_Aggregates

    pragma Ada_2022;

    with Matrices; use Matrices;

    procedure Show_Array_Aggregates is
       M : Matrix (1 .. 2, 1 .. 3);
    begin
       --  Positional component association
       M := [[0, 1, 2],
             [3, 4, 5]];

       Display (M);

       --  Named component association
       M := [[1 => 3,
              2 => 4,
              3 => 5],
             [1 => 6,
              2 => 7,
              3 => 8]];

       Display (M);

    end Show_Array_Aggregates;

The first aggregate we use in this example is :ada:`[[0, 1, 2], [3, 4, 5]]`.
Here, :ada:`[0, 1, 2]` and :ada:`[3, 4, 5]` are subaggregates of the
multidimensional aggregate. Subaggregates don't have a type themselves, but are
rather just considered part of a multidimensional aggregate (which, of course,
has an array type). In this sense, a subaggregate such as :ada:`[0, 1, 2]` is
different from a one-dimensional aggregate (such as :ada:`[0, 1, 2]`), even
though they are written in the same way.

Strings in subaggregates
^^^^^^^^^^^^^^^^^^^^^^^^

In the case of matrices using characters, we can use strings in the
corresponding array aggregates. Consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.String_Aggregates

    package String_Lists is

       type String_List is array (Positive range <>,
                                  Positive range <>)
                                  of Character;

       procedure Display (SL : String_List);

    end String_Lists;

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    package body String_Lists is

       procedure Display (SL : String_List) is

          procedure Display_Row (SL : String_List;
                                 I  : Integer) is
          begin
             Put ("  (");
             for J in SL'Range (2) loop
                Put (SL (I, J));
             end loop;
             Put (")");
          end Display_Row;

       begin
          Put_Line ("Length (1) = "
                    & SL'Length (1)'Image);
          Put_Line ("Length (2) = "
                    & SL'Length (2)'Image);

          Put_Line ("(");
          for I in SL'Range (1) loop
             Display_Row (SL, I);
             if I /= SL'Last (1) then
                Put_Line (",");
             else
                New_Line;
             end if;
          end loop;
          Put_Line (")");
       end Display;

    end String_Lists;

Then, when assigning to an object :ada:`SL` of :ada:`String_List` type, we can
use strings in the aggregates:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.String_Aggregates

    pragma Ada_2022;

    with String_Lists; use String_Lists;

    procedure Show_Array_Aggregates is
       SL : String_List (1 .. 2, 1 .. 3);
    begin
       --  Positional component association
       SL := ["ABC",
              "DEF"];

       Display (SL);

       --  Named component association
       SL := [[1 => 'X',
               2 => 'Y',
               3 => 'Z'],
              [others => ' ']];

       Display (SL);
    end Show_Array_Aggregates;

In the first assignment to :ada:`SL`, we have the aggregate
:ada:`["ABC", "DEF"]`, which uses strings as subaggregates. (Of course, we can
use a named aggregate and assign characters to the individual components.)


.. _Adv_Ada_Array_Aggregate_Box_Default_Value:

:ada:`<>` and default values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we indicated earlier, the :ada:`<>` syntax sets a component to its default
value |mdash| if such a default value is available. If a default value isn't
defined, however, the component will remain uninitialized, so that the behavior
is undefined. Let's look at more complex example to illustrate this situation.
Consider this package, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Rec_Array_Aggregates

    package Points is

       subtype Point_Value is Integer;

       type Point_3D is record
          X, Y, Z : Point_Value;
       end record;

       procedure Display (P : Point_3D);

       type Point_3D_Array is
         array (Positive range <>) of Point_3D;

       procedure Display (PA : Point_3D_Array);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put ("      (X => "
               & Point_Value'Image (P.X)
               & ",");
          New_Line;
          Put ("       Y => "
               & Point_Value'Image (P.Y)
               & ",");
          New_Line;
          Put ("       Z => "
               & Point_Value'Image (P.Z)
               & ")");
       end Display;

       procedure Display (PA : Point_3D_Array) is
       begin
          Put_Line ("(");
          for I in PA'Range (1) loop
             Put_Line ("  "
                       & Integer'Image (I)
                       & " =>");
             Display (PA (I));
             if I /= PA'Last (1) then
                Put_Line (",");
             else
                New_Line;
             end if;
          end loop;
          Put_Line (")");
       end Display;

    end Points;

Then, let's use :ada:`<>` for the array components:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Rec_Array_Aggregates

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Record_Aggregates is
       PA : Point_3D_Array (1 .. 2);
    begin
       PA := [ (X => 3,
                Y => 4,
                Z => 5),
               (X => 6,
                Y => 7,
                Z => 8) ];
       Display (PA);

       --  Array components are
       --  uninitialized.
       PA := [1 => <>,
              2 => <>];
       Display (PA);
    end Show_Record_Aggregates;

Because the record components (of the :ada:`Point_3D` type) don't have default
values, they remain uninitialized when we write :ada:`[1 => <>, 2 => <>]`.
(In fact, you may see *garbage* in the values displayed by the :ada:`Display`
procedure.)

When a default value is specified, it is used whenever :ada:`<>` is
specified. For example, we could use a type that has the :ada:`Default_Value`
aspect in its specification:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2

    package Integer_Arrays is

       type Value is new Integer
         with Default_Value => 99;

       type Integer_Array is
         array (Positive range <>) of Value;

       procedure Display (A : Integer_Array);

    end Integer_Arrays;

    pragma Ada_2022;

    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Aggregates is
       N : Integer_Array (1 .. 4);
    begin
       N := [for I in N'Range => Value (I)];
       Display (N);

       N := [others => <>];
       Display (N);
    end Show_Array_Aggregates;

When writing an aggregate for the :ada:`Point_3D` type, any component that has
:ada:`<>` gets the default value of the :ada:`Point` type (99):

.. admonition:: For further reading...

    Similarly, we could specify the :ada:`Default_Component_Value` aspect in the
    declaration of the array type:

    .. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2

        package Integer_Arrays is

           type Value is new Integer;

           type Integer_Array is
             array (Positive range <>) of Value
               with Default_Component_Value => 9999;

           procedure Display (A : Integer_Array);

        end Integer_Arrays;

        pragma Ada_2022;

        with Integer_Arrays; use Integer_Arrays;

        procedure Show_Array_Aggregates is
           N : Integer_Array (1 .. 4);
        begin
           N := [for I in N'Range => Value (I)];
           Display (N);

           N := [others => <>];
           Display (N);
        end Show_Array_Aggregates;

    In this case, when writing :ada:`<>` for a component, the value specified in
    the :ada:`Default_Component_Value` aspect is used.

    Finally, we might want to use both :ada:`Default_Value` and
    :ada:`Default_Component_Value` aspects at the same time. In this case, the
    value specified in the :ada:`Default_Component_Value` aspect has higher
    priority:

    .. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Array_Aggregates_2

        package Integer_Arrays is

           type Value is new Integer
             with Default_Value => 99;

           type Integer_Array is
             array (Positive range <>) of Value
               with Default_Component_Value => 9999;

           procedure Display (A : Integer_Array);

        end Integer_Arrays;

        pragma Ada_2022;

        with Integer_Arrays; use Integer_Arrays;

        procedure Show_Array_Aggregates is
           N : Integer_Array (1 .. 4);
        begin
           N := [for I in N'Range => Value (I)];
           Display (N);

           N := [others => <>];
           Display (N);
        end Show_Array_Aggregates;

    Here, 9999 is used when we specify :ada:`<>` for a component.


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

.. _Adv_Ada_Extension_Aggregates:

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

We can use extension aggregates for descendants of the :ada:`Point_2D` type as
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

       type Point_3D_Ext is new
         Point_3D with null record;

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

.. note::

   This feature was introduced in Ada 2022.

Previously, we've discussed
:ref:`extension aggregates <Adv_Ada_Extension_Aggregates>`, which are used to
assign an object :ada:`Obj_From` of a tagged type to an object :ada:`Obj_To` of
a descendent type.

We may want also to assign an object :ada:`Obj_From` of to an object
:ada:`Obj_To` of the same type, but change some of the components in this
assignment. To do this, we use delta aggregates.

Delta Aggregates for Tagged Records
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's reuse the :ada:`Points` package from a previous example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Tagged

    package Points is

       type Point_1D is tagged record
          X : Float;
       end record;

       type Point_2D is new Point_1D with record
          Y : Float;
       end record;

       type Point_3D is new Point_2D with record
          Z : Float;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image
                    & ", Z => " & P.Z'Image & ")");
       end Display;

    end Points;

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Points is
       P1, P2, P3 : Point_3D;
    begin
       P1 := (X => 0.5, Y => 0.7, Z => 0.3);
       Display (P1);

       P2 := (P1 with delta X => 1.0);
       Display (P2);

       P3 := (P1 with delta X => 0.2, Y => 0.3);
       Display (P3);
    end Show_Points;

Here, we assign :ada:`P1` to :ada:`P2`, but change the :ada:`X` component.
Also, we assign  :ada:`P1` to :ada:`P3`, but change the :ada:`X` and :ada:`Y`
components.

We can use class-wide types with delta aggregates. Consider this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Tagged

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Points is

       P_3D : Point_3D;

       function Reset (P_2D : Point_2D'Class)
                       return Point_2D'Class is
         ((P_2D with delta X | Y => 0.0));

    begin
       P_3D := [X => 0.1, Y => 0.2, Z => 0.3];
       Display (P_3D);

       P_3D := Point_3D (Reset (P_3D));
       Display (P_3D);

    end Show_Points;

In this example, the :ada:`Reset` function returns an object of
:ada:`Point_2D'Class` where all components of :ada:`Point_2D'Class` type are
zero. We call the :ada:`Reset` function for the :ada:`P_3D` object of
:ada:`Point_3D` type, so that only the :ada:`Z` component remains untouched.

Note that we use the syntax :ada:`X | Y` in the body of the :ada:`Reset`
function and assign the same value to both components.

.. admonition:: For further reading...

    We could have implemented :ada:`Reset` as a procedure |mdash| in this case,
    without using delta aggregates:

    .. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Tagged

        with Points; use Points;

        procedure Show_Points is

           P_3D : Point_3D;

           procedure Reset
             (P_2D : in out Point_2D'Class) is
           begin
              Point_2D (P_2D) := (others => 0.0);
           end Reset;

        begin
           P_3D := (X => 0.1, Y => 0.2, Z => 0.3);
           Display (P_3D);

           Reset (P_3D);
           Display (P_3D);

        end Show_Points;



Delta Aggregates for Non-Tagged Records
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The examples above use tagged types. We can also use delta aggregates with
non-tagged types. Let's rewrite the :ada:`Points` package and convert
:ada:`Point_3D` to a non-tagged record type.

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Non_Tagged

    package Points is

       type Point_3D is record
          X : Float;
          Y : Float;
          Z : Float;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image
                    & ", Z => " & P.Z'Image & ")");
       end Display;

    end Points;

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Points is
       P1, P2, P3 : Point_3D;
    begin
       P1 := (X => 0.5, Y => 0.7, Z => 0.3);
       Display (P1);

       P2 := (P1 with delta X => 1.0);
       Display (P2);

       P3 := (P1 with delta X => 0.2, Y => 0.3);
       Display (P3);
    end Show_Points;

In this example, :ada:`Point_3D` is a non-tagged type. Note that we haven't
changed anything in the :ada:`Show_Points` procedure: it still works as it did
with tagged types.

Delta Aggregates for Arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use delta aggregates for arrays. Let's change the declaration of
:ada:`Point_3D` and use an array to represent a 3-dimensional point:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Array

    package Points is

       type Float_Array is
         array (Positive range <>) of Float;

       type Point_3D is new Float_Array (1 .. 3);

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_3D) is
       begin
          Put ("(");
          for I in P'Range loop
             Put (I'Image
                  & " => "
                  & P (I)'Image);
          end loop;
          Put_Line (")");
       end Display;

    end Points;

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Points is
       P1, P2, P3 : Point_3D;
    begin
       P1 := [0.5, 0.7, 0.3];
       Display (P1);

       P2 := [P1 with delta 1 => 1.0];
       Display (P2);

       P3 := [P1 with delta 1 => 0.2, 2 => 0.3];
       --  Alternatively:
       --  P3 := [P1 with delta 1 .. 2 => 0.2, 0.3];

       Display (P3);
    end Show_Points;

The implementation of :ada:`Show_Points` in this example is very similar to the
version where use a record type. In this case, we:

- assign :ada:`P1` to :ada:`P2`, but change the first component, and

- we assign  :ada:`P1` to :ada:`P3`, but change the first and second
  components.

Using slices
^^^^^^^^^^^^

In the assignment to :ada:`P3`, we can either specify each component of the
delta individually or use a slice: both forms are equivalent. Also, we can use
slices to assign the same number to multiple components:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Array

    pragma Ada_2022;

    with Points; use Points;

    procedure Show_Points is
       P1, P3 : Point_3D;
    begin
       P1 := [0.5, 0.7, 0.3];
       Display (P1);

       P3 := [P1 with delta
                P3'First + 1 .. P3'Last => 0.0];
       Display (P3);
    end Show_Points;

In this example, we're assigning :ada:`P1` to :ada:`P3`, but resetting all
components of the array starting by the second one.

Multiple components
^^^^^^^^^^^^^^^^^^^

We can also assign multiple components or slices:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Delta_Aggregates_Array

    package Float_Arrays is

       type Float_Array is
         array (Positive range <>) of Float;

       procedure Display (P : Float_Array);

    end Float_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Float_Arrays is

       procedure Display (P : Float_Array) is
       begin

          Put ("(");
          for I in P'Range loop
             Put (I'Image
                  & " => "
                  & P (I)'Image);
          end loop;
          Put_Line (")");

       end Display;

    end Float_Arrays;

    pragma Ada_2022;

    with Float_Arrays; use Float_Arrays;

    procedure Show_Multiple_Delta_Slices is

       P1, P2 : Float_Array (1 .. 5);

    begin
       P1 := [1.0, 2.0, 3.0, 4.0, 5.0];
       Display (P1);

       P2 := [P1 with delta
                P2'First + 1 .. P2'Last - 2 => 0.0,
                P2'Last - 1  .. P2'Last => 0.2];
       Display (P2);
    end Show_Multiple_Delta_Slices;

In this example, we have two arrays :ada:`P1` and :ada:`P2` of
:ada:`Float_Array` type. We assign :ada:`P1` to :ada:`P2`, but change:

- the second to the last-but-two components to 0.0, and

- the last-but-one and last components to 0.2.

.. admonition:: In the Ada Reference Manual

   - :arm22:`Delta Aggregates <4-3-4>`
