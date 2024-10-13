Records
=======

.. include:: ../../../global.txt

.. _Adv_Ada_Record_Component_Default_Initialization:

Default Initialization
----------------------

As mentioned in the
:ref:`Introduction to Ada <Intro_Ada_Record_Default_Values>` course, record
components can have default initial values. Also, we've seen that other kinds
of types can have :ref:`default values <Adv_Ada_Default_Initial_Values>`.

In the Ada Reference Manual, we refer to these default initial values as
"default expressions of record components." The term *default expression*
indicates that we can use any kind of expression for the default initialization
of record components |mdash| which includes subprogram calls for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Simple_Example

    package Show_Default_Initialization is

       function Init return Integer is
         (42);

       type Rec is record
          A : Integer := Init;
       end record;

    end Show_Default_Initialization;

In this example, the :ada:`A` component is initialized by default by a call to
the :ada:`Init` procedure.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.8 Record Types <3-8>`


Dependencies
~~~~~~~~~~~~

Default expressions cannot depend on other components. For example, if we have
two components :ada:`A` and :ada:`B`, we cannot initialize :ada:`B` based on
the value that :ada:`A` has:

.. code:: ada compile_button manual_chop project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.No_Dependency
    :class: ada-expect-compile-error

    !show_default_initialization_dependency.ads
    package Show_Default_Initialization_Dependency is

       function Init return Integer is
         (42);

       type Rec is record
          A : Integer := Init;
          B : Integer := Rec.A;  --  Illegal!
       end record;

    end Show_Default_Initialization_Dependency;

In this example, we cannot initialize the :ada:`B` component based on the value
of the :ada:`A` component. (In fact, the syntax :ada:`Rec.A` as a way to refer
to the :ada:`A` component is only allowed in predicates, not in the record
component declaration.)

.. todo::

    Add link to section on predicates once it's available.


Initialization Order
~~~~~~~~~~~~~~~~~~~~

The default initialization of record components is performed in arbitrary
order. In fact, the order is decided by the compiler, so we don't have control
over it.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Initialization_Order

    package Simple_Recs is

       function Init (S : String;
                      I : Integer)
                      return Integer;

       type Rec is record
          A : Integer := Init ("A", 1);
          B : Integer := Init ("B", 2);
       end record;

    end Simple_Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Recs is

       function Init (S : String;
                      I : Integer)
                      return Integer is
       begin
          Put_Line (S & ": " & I'Image);
          return I;
       end Init;

    end Simple_Recs;

    with Simple_Recs; use Simple_Recs;

    procedure Show_Initialization_Order is
       R : Rec;
    begin
       null;
    end Show_Initialization_Order;

When running this code example, you might see this:

.. code-block:: none

    A: 1
    B: 2

However, the compiler is allowed to rearrange the operations, so this output is
possible as well:

.. code-block:: none

    B: 2
    A: 1

Therefore, we must write the default expression of each individual record
components in such a way that the resulting initialization value is always
correct, independently of the order that those expressions are evaluated.


Evaluation
~~~~~~~~~~

According to the Annotated Ada Reference Manual, the "default expression of a
record component is only evaluated upon the creation of a default-initialized
object of the record type." This means that the default expression is by itself
not evaluated when we declare the record type, but when we create an object of
this type. It follows from this rule that the default is only evaluated when
necessary, i.e,, when an explicit initial value is not specified in the object
declaration.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Initialization_Order

    with Ada.Text_IO; use Ada.Text_IO;
    with Simple_Recs; use Simple_Recs;

    procedure Show_Initialization_Order is
    begin
       Put_Line ("Some processing first...");
       Put_Line
         ("Now, let's declare an object "
          & "of the record type Rec...");

       declare
          R : Rec;
       begin
          Put_Line
            ("An object of Rec type has "
             & "just been created.");
       end;

    end Show_Initialization_Order;

Here, we only see the information displayed by the :ada:`Init` function
|mdash| which is called to initialize the :ada:`A` and :ada:`B` components of
the :ada:`R` record |mdash| during the object creation. In other words,
the default expressions :ada:`Init ("A", 1)` and :ada:`Init ("B", 2)` are *not*
evaluated when we declare the :ada:`R` type, but when we create an object of
this type.

.. admonition:: In the Ada Reference Manual

    - :aarm22:`3.8 Record Types <3-8>`


Defaults and object declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

    This subsection was originally written by Robert A. Duff and published as
    `Gem #12: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-12>`_.

Consider the following type declaration:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Default_Init
    :class: ada-syntax-only

    package Type_Defaults is
       type Color_Enum is (Red, Blue, Green);

       type T is private;
    private
       type T is
          record
             Color     : Color_Enum := Red;
             Is_Gnarly : Boolean := False;
             Count     : Natural;
          end record;

       procedure Do_Something;
    end Type_Defaults;

If we want to say, "make :ada:`Count` equal :ada:`100`, but initialize
:ada:`Color` and :ada:`Is_Gnarly` to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Default_Init

    package body Type_Defaults is

       Object_100 : constant T :=
                      (Color     => <>,
                       Is_Gnarly => <>,
                       Count     => 100);

       procedure Do_Something is null;

    end Type_Defaults;

.. admonition:: Historically

    Prior to Ada 2005, the following style was common:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Default_Init

        package body Type_Defaults is

           Object_100 : constant T :=
                          (Color     => Red,
                           Is_Gnarly => False,
                           Count     => 100);

           procedure Do_Something is null;

        end Type_Defaults;

    Here, we only wanted :ada:`Object_100` to be a default-initialized
    :ada:`T`, with :ada:`Count` equal to :ada:`100`. It's a little bit annoying
    that we had to write the default values :ada:`Red` and :ada:`False` twice.
    What if we change our mind about :ada:`Red`, and forget to change it in all
    the relevant places? Since Ada 2005, the :ada:`<>` notation comes to the
    rescue, as we've just seen.

On the other hand, if we want to say, "make :ada:`Count` equal :ada:`100`,
but initialize all other components, including the ones we might add next
week, to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Default_Init

    package body Type_Defaults is

       Object_100 : constant T := (Count  => 100,
                                   others => <>);

       procedure Do_Something is null;

    end Type_Defaults;

Note that if we add a component :ada:`Glorp : Integer;` to type :ada:`T`,
then the :ada:`others` case leaves :ada:`Glorp` undefined just as this
code would do:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Default_Init

    package body Type_Defaults is

       procedure Do_Something is
          Object_100 : T;
       begin
          Object_100.Count := 100;
       end Do_Something;

    end Type_Defaults;

Therefore, you should be careful and think twice before using
:ada:`others`.


Advanced Usages
~~~~~~~~~~~~~~~

In addition to expressions such as subprogram calls, we can use
:ref:`per-object expressions <Adv_Ada_Per_Object_Expressions_Default_Value>`
for the default value of a record component. (We discuss this topic later on
in more details.)

For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Default_Initialization.Per_Object_Expressions

    package Rec_Per_Object_Expressions is

       type T (D : Positive) is private;

    private

       type T (D : Positive) is record
          V : Natural := D - 1;
          --             ^^^^^
          --    Per-object expression
       end record;

    end Rec_Per_Object_Expressions;

In this example, component :ada:`V` is initialized by default with the
per-object expression :ada:`D - 1`, where :ada:`D` refers to the discriminant
:ada:`D`.


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


.. _Adv_Ada_Record_Discriminants:

Record discriminants
--------------------

We introduced the topic of record discriminants in the
:ref:`Introduction to Ada course <Intro_Ada_Record_Discriminants>`. Also,
in a previous chapter, we mentioned that record types with unconstrained
discriminants without defaults are
:ref:`indefinite types <Adv_Ada_Definite_Indefinite_Subtypes>`.

In this section, we discuss a couple of details about record discriminants that
we haven't covered yet. Although the discussion will be restricted to
record discriminants, keep in mind that tasks and protected types can also have
discriminants. We'll focus on discriminants for tasks and protected types in
separate chapters.

.. todo::

    Add link to section on task discriminants once it's available.

In addition, discriminants can be used to write
:ref:`per-object expressions <Adv_Ada_Per_Object_Expressions>`. We discuss this
topic later in this chapter.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.7 Discriminants <3-7>`


.. _Adv_Ada_Known_Unknown_Discriminant_Parts:

Known and unknown discriminant parts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When it comes to discriminants, a type declaration falls into one of the
following three categories: it has either no discriminants at all, known
discriminants or unknown discriminants.

In order to have no discriminants, a type simply doesn't have a discriminant
part in its declaration. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.No_Discriminant_Part

    package Show_Discriminants is

       type T_No_Discr is private;
       --            ^^^
       --   no discriminant part

    private

       type T_No_Discr is null record;

    end Show_Discriminants;

By using parentheses after the type name, we're defining a discriminant part.
In this case, the type can either have unknown or known discriminants. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Discriminant_Parts

    package Show_Discriminants is

       type T_Unknown_Discr (<>) is
       --                    ^^
       --   Unknown discriminant
         private;

       type T_Known_Discr (D : Integer) is
       --                  ^^^^^^^^^^^
       --   Known discriminant
         private;

    private

       type T_Unknown_Discr is
         null record;

       type T_Known_Discr (D : Integer) is
         null record;

    end Show_Discriminants;

An unknown discriminant part is represented by :ada:`(<>)` in the partial view
|mdash| this is basically the so-called *box notation* :ada:`<>` (also known as
*box compound delimiter*) in parentheses. We discuss unknown discriminant parts
and their peculiarities
:ref:`later on in this chapter <Adv_Ada_Unknown_Discriminants>`. In this
section, we mainly focus on known discriminants.

We've already seen examples of known discriminants in previous chapters. In
simple terms, known discriminants are composed by one or more discriminant
specifications, which are similar to subprogram parameters, but without
parameter modes. In fact, we can think of discriminants as parameters for a
type :ada:`T`, but with the goal of defining specific characteristics or
constraints when declaring objects of type :ada:`T`.


Discriminant as constant property
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can think of discriminants as constant properties of a type. In fact, if you
want to specify a record component :ada:`C` that shouldn't change, declaring it
constant isn't allowed in Ada:

.. code:: ada compile_button manual_chop project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Constant_Properties
    :class: ada-expect-compile-error, nosyntax-check

    !constant_properties.ads
    package Constant_Properties is

       type Rec is record
          C : constant Integer;
          --  ^^^^^^^^
          --  ERROR: record components
          --         cannot be constant.
          V :          Integer;
       end record;

    end Constant_Properties;

A simple solution is to use a record discriminant:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Constant_Properties

    package Constant_Properties is

       type Rec (C : Integer) is
       record
          V :          Integer;
       end record;

    end Constant_Properties;

A record discriminant can be accessed as a normal component, but it is
read-only, so we cannot change it:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Constant_Properties
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    with Constant_Properties;
    use  Constant_Properties;

    procedure Show_Constant_Property is
       R : Rec (10);
    begin
       Put_Line ("R.C = "
                 & R.C'Image);

       R.C := R.C + 1;
       --  ERROR: cannot change
       --         record discriminant
    end Show_Constant_Property;

In this code example, the compilation fails because we cannot change the
:ada:`C` discriminant. In this sense, :ada:`C` is a basically a constant
component of the :ada:`R` object.


.. _Adv_Ada_Record_Discriminants_Private_Types:

Private types
~~~~~~~~~~~~~

As we've seen in previous chapters, private types can have discriminants. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Private_With_Discriminants

    package Private_With_Discriminants is

       type T (L : Positive) is private;

    private

       type Integer_Array is
         array (Positive range <>) of Integer;

       type T (L : Positive) is
       record
          Arr : Integer_Array (1 .. L);
       end record;

    end Private_With_Discriminants;

Here, discriminant :ada:`L` is used to specify the constraints of the array
component :ada:`Arr`. Note that the same discriminant part must appear in both
:ref:`the partial and the full view <Adv_Ada_Type_View>` of type :ada:`T`.


.. _Adv_Ada_Record_Discriminants_Object_Declaration:

Object declaration
~~~~~~~~~~~~~~~~~~

As we've already seen, we declare objects of a type :ada:`T` with a
discriminant :ada:`D` by specifying the actual value of discriminant :ada:`D`.
This is called a
:ref:`discriminant constraint <Adv_Ada_Record_Discriminant_Constraints>`.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Objects_Discriminants

    package Recs is

       type T (L : Positive;
               M : Positive) is
         null record;

    end Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    with Recs;        use Recs;

    procedure Show_Object_Declaration is
       A : T (L => 5, M => 6);
       B : T (7, 8);
    begin
       Put_Line ("A.L = "
                 & A.L'Image);
       Put_Line ("A.M = "
                 & A.M'Image);
       Put_Line ("B.L = "
                 & B.L'Image);
       Put_Line ("B.M = "
                 & B.M'Image);
    end Show_Object_Declaration;

As we can see in the declaration of objects :ada:`A` and :ada:`B`, for the
discriminant values, we can use a positional (:ada:`(7, 8)`) or named
association (:ada:`(L => 5, M => 6)`).


Object size
^^^^^^^^^^^

Discriminants can have an impact on the object size because we can set the
discriminant to constraint a component of an
:ref:`indefinite subtype <Adv_Ada_Definite_Indefinite_Subtypes>`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Objects_Discriminants_Size

    package Recs is

       type Null_Rec (L : Positive;
                      M : Positive) is
         private;

       type Rec_Array (L : Positive) is
         private;

    private

       type Null_Rec (L : Positive;
                      M : Positive) is
         null record;

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Rec_Array (L : Positive) is
       record
          Arr : Integer_Array (1 .. L);
       end record;

    end Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    with Recs;        use Recs;

    procedure Show_Object_Sizes is
       Null_Rec_A  : Null_Rec (1, 2);
       Null_Rec_B  : Null_Rec (5, 6);
       Rec_Array_A : Rec_Array (10);
       Rec_Array_B : Rec_Array (20);
    begin
       Put_Line ("Null_Rec_A'Size = "
                 & Null_Rec_A'Size'Image);
       Put_Line ("Null_Rec_B'Size = "
                 & Null_Rec_B'Size'Image);
       Put_Line ("Rec_Array_A'Size = "
                 & Rec_Array_A'Size'Image);
       Put_Line ("Rec_Array_B'Size = "
                 & Rec_Array_B'Size'Image);
    end Show_Object_Sizes;

In this example, :ada:`Null_Rec_A` and :ada:`Null_Rec_B` have the same size
because the type is a null record. However, :ada:`Rec_Array_A` and
:ada:`Rec_Array_B` have different sizes because we're setting the :ada:`L`
discriminant |mdash| which we use to constraint the :ada:`Arr` array component
of the :ada:`Rec_Array` type |mdash| to 10 and 20, respectively.


.. _Adv_Ada_Record_Discriminants_Object_Assignments:

Object assignments
~~~~~~~~~~~~~~~~~~

As we've just seen, when we set the values for the discriminants of a type in
the object declaration, we're constraining the objects. Those constraints are
checked at runtime by the
:ref:`discriminant check <Adv_Ada_Discriminant_Check>`. If the discriminants
don't match, the :ada:`Constraint_Error` exception is raised.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Object_Assignments
    :class: ada-run-expect-failure

    package Recs is

       type T (L : Positive;
               M : Positive) is
         null record;

    end Recs;

    with Recs;        use Recs;

    procedure Show_Object_Assignments is
       A1, A2 : T (5, 6);
       B      : T (7, 8);
    begin
       A1 := A2;    --  OK
       B  := A1;    --  ERROR!
    end Show_Object_Assignments;

In this example, the :ada:`A1 := A2` assignment is accepted because both
:ada:`A1` and :ada:`A2` have the same constraints (:ada:`(5, 6)`). However, the
:ada:`B := A1` assignment is not accepted because the discriminant check fails
at runtime.

Note that the discriminant check is not performed when we use
:ref:`mutable subtypes <Adv_Ada_Mutable_Subtypes>` |mdash| we discuss this
specific kind of subtypes later on.


Discriminant type
~~~~~~~~~~~~~~~~~

In a discriminant specification, the type of the discriminant can only be a
discrete subtype or an
:ref:`access type <Adv_Ada_Discriminants_As_Access_Values>`. Other kinds of
types |mdash| e.g. composite types such as record types |mdash| are illegal for
discriminants. However, we can always use them indirectly by using access
types. (We'll see an example later.)

In addition to that, we can also use a different kind of access types, namely
:ref:`anonymous access-to-object subtypes <Adv_Ada_Anonymous_Access_To_Object_Types>`.
This specific kind of discriminant is called
:ref:`access discriminant <Adv_Ada_Anonymous_Access_Discriminants>`. We discuss
this topic in more details in another chapter.

Let's see a code example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Discriminants_Subtype

    package Recs is

       type Usage_Mode is (Off,
                           Simple_Usage,
                           Advanced_Usage);

       type Priv_Info is private;

       type Priv_Info_Access is access Priv_Info;

       type Proc_Access is
         access procedure (P : in out Priv_Info);

       type Priv_Rec (Last  : Positive;
                      Usage : Usage_Mode;
                      Info  : Priv_Info_Access;
                      Proc  : Proc_Access) is
         private;

    private

       type Priv_Info is record
         A : Positive;
         B : Positive;
       end record;

       type Priv_Rec (Last  : Positive;
                      Usage : Usage_Mode;
                      Info  : Priv_Info_Access;
                      Proc  : Proc_Access) is
         null record;

    end Recs;

In this example, we're declaring the :ada:`Priv_Rec` type with the following
discriminants:

- The :ada:`Last` discriminant of the scalar (i.e. discrete) type
  :ada:`Positive`;

- The :ada:`Usage` discriminant of the enumeration (i.e. discrete) type
  :ada:`Usage_Mode`;

- The :ada:`Info` discriminant of the
  :ref:`access-to-object type <Adv_Ada_Access_Types_Terminology>`
  :ada:`Priv_Info_Access`;

    - We discuss
      :ref:`access-to-object types as discriminant type <Adv_Ada_Discriminants_As_Access_Values>`
      in another chapter.

- The :ada:`Proc` discriminant of the
  :ref:`access-to-subprogram type <Adv_Ada_Access_To_Subprograms>`
  :ada:`Proc_Access`;

    - We discuss
      :ref:`access-to-subprogram types as discriminant type <Adv_Ada_Access_To_Subprograms_As_Discriminant_Type>`
      in another chapter.

As indicated previously, it's illegal to use a private type or a record type as
the type of a discriminant. For example:


.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Discriminants_Subtype_Error
    :class: ada-expect-compile-error

    package Recs is

       type Priv_Info is private;

       type Priv_Rec (Info : Priv_Info) is
         private;
       --             ^^^^^^^^^^^^^^^^
       --  ERROR: cannot use private type
       --         in discriminant.

    private

       type Priv_Info is record
         A : Positive;
         B : Positive;
       end record;

       type Priv_Rec (Info  : Priv_Info) is
         null record;

    end Recs;

We cannot use the :ada:`Priv_Info` directly as a discriminant type because it's
a private type. However, as we've just seen in the previous code example, we
use it indirectly by using an access type to this private type (see
:ada:`Priv_Info_Access` in the code example).


.. _Adv_Ada_Indefinite_Subtype_Discriminant:

Indefinite subtypes as discriminants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As we already implied, we cannot use indefinite subtypes as discriminants. For
example, the following code won't compile:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types_Error
    :class: ada-expect-compile-error

    package Unconstrained_Types is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Simple_Record (Arr : Integer_Array) is
       --                  ^^^^^^^^^^^^^^^^^^^
       --  ERROR: cannot use indefinite type
       --         in discriminant.
       record
          L : Natural := Arr'Length;
       end record;

    end Unconstrained_Types;

:ada:`Integer_Array` is a correct type declaration |mdash| although
the type itself is indefinite after the declaration. However, we cannot
use it as the discriminant in the declaration of :ada:`Simple_Record`.
We could, however, have a correct declaration by using discriminants as
access values:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types_Error

    package Unconstrained_Types is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Array_Access is
         access Integer_Array;

       type Simple_Record
         (Arr : Integer_Array_Access) is
       record
          L : Natural := Arr'Length;
       end record;

    end Unconstrained_Types;

By adding the :ada:`Integer_Array_Access` type and using it in
:ada:`Simple_Record`\'s type declaration, we can indirectly use an
indefinite type in the declaration of another indefinite type. We discuss
this topic later
:ref:`in another chapter <Adv_Ada_Discriminants_As_Access_Values>`.


.. _Adv_Ada_Record_Discriminants_Default_Values:

Default values
~~~~~~~~~~~~~~

We can specify default values for discriminants. Note, however, that we must
either specify default values for **all** discriminants of the discriminant
part or for none of them. This contrasts with default values for subprogram
parameters, where we can
:ref:`specify default values for just a subset of all parameters of a specific subprogram <Adv_Ada_Parameter_Order_And_Association>`.

As expected, we can override the default values by specifying the values of
each discriminant when declaring an object. Let's see a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Discriminant_Default_Value

    package Recs is

       type T (L : Positive := 1;
               M : Positive := 2) is
         private;

    private

       type T (L : Positive := 1;
               M : Positive := 2) is
         null record;

    end Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    with Recs;        use Recs;

    procedure Show_Object_Declaration is
       A : T;
       B : T (7, 8);
    begin
       Put_Line ("A.L = "
                 & A.L'Image);
       Put_Line ("A.M = "
                 & A.M'Image);
       Put_Line ("B.L = "
                 & B.L'Image);
       Put_Line ("B.M = "
                 & B.M'Image);
    end Show_Object_Declaration;

In this example, object :ada:`A` makes use of the default values for the
discriminants of type :ada:`T`, so it has the discriminants
:ada:`(L => 1, M => 2)`. In the case of object :ada:`B`, we're specifying the
values :ada:`(L => 7, M => 8)`, which are used instead of the default values.

Note that we cannot set default values for nonlimited tagged types. The same
applies to generic formal types. For example:

.. todo::

    Add link to section on generic formal types once it's available.

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Discriminant_Default_Value_Tagged_TYpe
    :class: ada-expect-compile-error

    package Recs is

       type TT (L : Positive := 1;
                M : Positive := 2) is
       --       ^^^^^^^^^^^^^^^^^
       --  ERROR: cannot assign default
       --         in discriminant of
       --         nonlimited tagged type.
         tagged private;

       type LTT (L : Positive := 1;
                 M : Positive := 2) is
         tagged limited private;

    private

       type TT (L : Positive := 1;
                M : Positive := 2) is
         tagged null record;

       type LTT (L : Positive := 1;
                 M : Positive := 2) is
         tagged limited null record;

    end Recs;

As we can see, compilation fails because of the default values for the
discriminants of the nonlimited tagged type :ada:`TT`. In the case of the
limited tagged type :ada:`LTT`, the default values for the discriminants are
legal.


.. _Adv_Ada_Mutable_Subtypes:

Mutable subtypes
^^^^^^^^^^^^^^^^

An unconstrained discriminated subtype with defaults is called a mutable
subtype, and a variable of such a subtype is called a mutable variable because
the discriminants of such a variable can be changed. An important feature of
mutable subtypes is that it allows for changing the discriminants of an object
via assignments |mdash| in this case, no
:ref:`discriminant check <Adv_Ada_Discriminant_Check>` is performed.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Mutable_Subtype
    :class: ada-run-expect-failure

    package Mutability is

       type T_Non_Mutable
         (L : Positive;
          M : Positive) is
         null record;

       type T_Mutable
         (L : Positive := 1;
          M : Positive := 2) is
         null record;

    end Mutability;

    with Mutability; use Mutability;

    procedure Show_Mutable_Subtype_Assignment is
       NM_1 : T_Non_Mutable (5, 6);
       NM_2 : T_Non_Mutable (7, 8);

       M_1  : T_Mutable (7, 8);
       M_2  : T_Mutable;
    begin
       NM_2 := NM_1;  --  ERROR!
       M_2  := M_1;   --  OK
    end Show_Mutable_Subtype_Assignment;

In this example, the :ada:`NM_2 := NM_1` assignment fails because both objects
are of a non-mutable subtype with different discriminants, so that the
discriminant check fails at runtime. However, the :ada:`M_2 := M_1` assignment
is OK because both objects are mutable variables. In this case, this assignment
changes the discriminants of :ada:`M_2` from :ada:`(L => 1, M => 2)` to
:ada:`(L => 7, M => 8)`.

Note that assignments of mutable variables may not always work at runtime. For
example, if a discriminant of a mutable subtype is used to constraint a
component of indefinite subtype, we might see the corresponding checks fail at
runtime. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Mutable_Subtype_Error
    :class: ada-run-expect-failure

    package Mutability is

       type T_Mutable_Array (L : Positive := 10) is
         private;

    private

       type Integer_Array is
         array (Positive range <>) of Integer;

       type T_Mutable_Array (L : Positive := 10) is
       record
          Arr : Integer_Array (1 .. L);
       end record;

    end Mutability;

    with Ada.Text_IO; use Ada.Text_IO;

    with Mutability;  use Mutability;

    procedure Show_Mutable_Subtype_Error is
       A : T_Mutable_Array (10);
       B : T_Mutable_Array (20);
    begin
       Put_Line ("A'Size = "
                 & A'Size'Image);
       Put_Line ("B'Size = "
                 & B'Size'Image);

       A := B;  --  ERROR!
    end Show_Mutable_Subtype_Error;

In this case, the assignment :ada:`A := B` raises the :ada:`Constraint_Error`
exception at runtime. Here, the :ada:`Arr` component of each object has a
different range: :ada:`1 .. 10` for object :ada:`A` and :ada:`1 .. 20` for
object :ada:`B`.
To prevent this situation, we should declare :ada:`T_Mutable_Array` as a
limited type, so that assignments are not permitted.


Derived types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~

As expected, we may derive types with discriminants or declare subtypes of it.
However, there are a couple of details associated with this, which we discuss
now.

Subtypes
^^^^^^^^

When declaring a subtype of a type with discriminants, we have the choice to
specify the value of the discriminants for the parent type, or specify no
discriminants at all:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Subtypes

    package Subtypes_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       subtype Sub_T is T;
       --  Discriminants are not specified:
       --  taking the ones from T.

       subtype Sub_T_2 is T
        (L => 3, M => 4);
       --  Discriminants are specified:
       --  taking the ones from Sub_T_2

    end Subtypes_With_Discriminants;

For the :ada:`Sub_T` subtype declaration in this example, we don't specify
values for the parent type's discriminants. For :ada:`Sub_T_2`, in contrast, we
set the discriminants to :ada:`(L => 3, M => 4)`.

When declaring objects of these subtypes, we need to take the constraints into
account:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Subtypes
    :class: ada-run-expect-failure

    package Subtypes_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       subtype Sub_T is T;
       --  Discriminants are not specified:
       --  taking the ones from T.

       subtype Sub_T_2 is T
        (L => 3, M => 4);
       --  Discriminants are specified:
       --  taking the ones from Sub_T_2

    end Subtypes_With_Discriminants;

    with Subtypes_With_Discriminants;
    use  Subtypes_With_Discriminants;

    procedure Show_Subtypes_With_Discriminants is
       A1 : T (1, 2);
       A2 : T (3, 4);
       B1 : Sub_T (1, 2);
       B2 : Sub_T (3, 4);
       C2 : Sub_T_2;

       --  C1 : Sub_T_2 (1, 2);
       --                ^^^^
       --  ERROR: discriminants already
       --         constrained
    begin
       B1 := A1;
       --  OK: discriminants match

       B2 := A1;
       --  CONSTRAINT_ERROR!

       B2 := A2;
       --  OK: discriminants match

       C2 := A1;
       --  CONSTRAINT_ERROR!

       C2 := A2;
       --  OK: discriminants match
    end Show_Subtypes_With_Discriminants;

For objects of :ada:`Sub_T` subtype, we *have to* specify the value of each
discriminant. On the other hand, for objects of :ada:`Sub_T_2` type, we
*cannot* specify the constraints because they have already been defined in the
subtype's declaration |mdash| in this case, they're always set to
:ada:`(3, 4)`.

When assigning objects of different subtypes, the discriminant check will be
performed |mdash| as we
:ref:`mentioned before <Adv_Ada_Record_Discriminants_Object_Assignments>`. In
this example, the assignments :ada:`B2 := A1` and :ada:`C2 := A1` fail because
the objects have different constraints.


Derived types
^^^^^^^^^^^^^

The behavior for derived types is very similar to the one we've just described
for subtypes. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       type T_Derived is new T;
       --  Discriminants are not specified:
       --  taking the ones from T.

       type T_Derived_2 is new T
         (L => 3, M => 4);
       --  Discriminants are specified:
       --  taking the ones from T_Derived_2

    end Derived_With_Discriminants;

For the :ada:`T_Derived` type, we reuse the discriminants of the parent type
:ada:`T`. For the :ada:`T_Derived_2` type, we specify a value for each
discriminant of :ada:`T`.

As you probably notice, this code looks very similar to the code using
subtypes. The main difference between using subtypes and derived types is that,
as expected, we have to perform a
:ref:`type conversion <Adv_Ada_Type_Conversion>` in the assignments:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types
    :class: ada-run-expect-failure

    with Derived_With_Discriminants;
    use  Derived_With_Discriminants;

    procedure Show_Derived_With_Discriminants is
       A1 : T (1, 2);
       A2 : T (3, 4);
       B1 : T_Derived (1, 2);
       B2 : T_Derived (3, 4);
       C2 : T_Derived_2;

       --  C1 : Sub_T_2 (1, 2);
       --                ^^^^
       --  ERROR: discriminants already
       --         constrained
    begin
       B1 := T_Derived (A1);
       --  OK: discriminants match

       B2 := T_Derived (A1);
       --  ERROR!

       C2 := T_Derived_2 (A1);
       --  CONSTRAINT_ERROR!

       C2 := T_Derived_2 (A2);
       --  OK: discriminants match
    end Show_Derived_With_Discriminants;

Once again, a discriminant check is performed when assigning objects to ensure
that the type discriminants match. In this code example, the assignments
:ada:`B2 := A1` and :ada:`C2 := A1` fail because the objects have different
constraints.


Derived types with renamed discriminants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We could rewrite a type declaration such as :ada:`type T_Derived is new T` by
explicitly declaring the discriminants. We can do that for the previous code
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Same_Discriminants

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       --  The declaration:
       --
       --     type T_Derived is new T;
       --
       --  is the same as:
       --
       type T_Derived
         (L : Positive;
          M : Positive) is
         new T (L => L, M => M);

    end Derived_With_Discriminants;

We may, however, rename the discriminants instead. For example, we could rename
:ada:`L` and :ada:`M` to :ada:`X` and :ada:`Y`. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Renamed_Discriminants

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       type T_Derived
         (X : Positive;
          Y : Positive) is
         new T (L => X, M => Y);

    end Derived_With_Discriminants;

Of course, if we use named association when declaring objects, we have to use
the correct discriminant names:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Renamed_Discriminants

    with Ada.Text_IO; use Ada.Text_IO;

    with Derived_With_Discriminants;
    use  Derived_With_Discriminants;

    procedure Show_Derived_With_Discriminants is
       A : T (L => 1, M => 2);
       B : T_Derived (X => 3, Y => 4);
       --             ^^^^^^^^^^^^^^
       --  Using correct discriminant names
    begin
       Put_Line ("A.L = "
                 & A.L'Image);
       Put_Line ("A.M = "
                 & A.M'Image);
       Put_Line ("B.X = "
                 & B.X'Image);
       Put_Line ("B.Y = "
                 & B.Y'Image);
    end Show_Derived_With_Discriminants;

In essence, the discriminants of both parent and derived types are the same:
the only difference is that they are accessed by different names. This allows
us to convert from a parent type to a derived type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Renamed_Discriminants

    with Derived_With_Discriminants;
    use  Derived_With_Discriminants;

    procedure Show_Derived_With_Discriminants is
       A : T (L => 1, M => 2);
       B : T_Derived (X => 1, Y => 2);
    begin
       B := T_Derived (A);  --  OK
    end Show_Derived_With_Discriminants;

Here, even though objects :ada:`A` and :ada:`B` have discriminants with
different names, the assignment :ada:`B := T_Derived (A)` is valid.


Derived types with more constrained discriminants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When deriving types with discriminants, we may use a more constrained type for
the discriminants of derived type. For example, if the discriminant :ada:`D` of
the parent type is of :ada:`Integer` type, the corresponding discriminant of
the derived type may use a constrained subtype such as :ada:`Natural` or
:ada:`Positive` |mdash| because both :ada:`Natural` and :ada:`Positive` are
subtypes of type :ada:`Integer`. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_More_Constrained_Discriminants

    package Derived_With_Discriminants is

       type T
         (L : Integer;
          M : Integer) is
         null record;

       type T_Derived_2
         (X : Natural;
          Y : Positive) is
         new T (L => X, M => Y);

    end Derived_With_Discriminants;

As expected, the constraints of each discriminant's type are taken into account
when evaluating the value that is specified for each discriminant:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_More_Constrained_Discriminants

    with Ada.Text_IO; use Ada.Text_IO;

    with Derived_With_Discriminants;
    use  Derived_With_Discriminants;

    procedure Show_Derived_With_Discriminants is
       A : T (L => -1, M => -2);
       B : T_Derived_2 (X => 0, Y => 1);
    begin
       Put_Line ("A.L = "
                 & A.L'Image);
       Put_Line ("A.M = "
                 & A.M'Image);
       Put_Line ("B.X = "
                 & B.X'Image);
       Put_Line ("B.Y = "
                 & B.Y'Image);
    end Show_Derived_With_Discriminants;

Here, we can use :ada:`(L => -1, M => -2)` in the declaration of object
:ada:`A` because both discriminants are of :ada:`Integer` type. However, in the
declaration of object :ada:`B`, we can only use values for the discriminants
that are in the range of the :ada:`Natural` and :ada:`Positive` subtypes,
respectively. (If you change the code to use negative values instead, a
:ada:`Constraint_Error` exception is raised at runtime.)


Extending the discriminant part
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As we've seen, we can rename discriminants or use more constrained subtypes for
discriminants in derived types. We might also want to add a new discriminant to
the derived type |mdash| in addition to the discriminants of the parent's type.
However, this is considered a type extension, as the new discriminant is part
of the type definition.

As an example, we may want to add the :ada:`A` discriminant of :ada:`Boolean`
type to a derived type. For non-tagged types, such a declaration will trigger a
compilation error as expected:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Extension_Error
    :class: ada-expect-compile-error

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       type T_Derived
         (X : Positive;
          Y : Positive;
          A : Boolean) is
       --  ^^^^^^^^^^^
       --  ERROR: cannot extend type with new
       --         Boolean discriminant A
         new T (L => X, M => Y);

    end Derived_With_Discriminants;

To circumvent this issue, we could, of course, declare a component of :ada:`T`
type instead of deriving from it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Extension_Error

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         null record;

       type T_2
         (X : Positive;
          Y : Positive;
          A : Boolean) is
       record
          A_Comp : T (L => X, M => Y);
       end record;

    end Derived_With_Discriminants;

In this case, :ada:`A_Comp` is a component of type :ada:`T`, and we're using
the discriminant :ada:`X` and :ada:`Y` as the constraints of this component.

Naturally, using tagged types is another alternative:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Tagged_Types

    package Derived_With_Discriminants is

       type T
         (L : Positive;
          M : Positive) is
         tagged null record;

       type T_Derived_Extended
         (X : Positive;
          Y : Positive;
          A : Boolean) is  --  New discriminant
         new T (L => X, M => Y)
           with null record;

       type T_Derived_Extended_2
         (A : Boolean;     --  New discriminant
          X : Positive;
          Y : Positive) is
         new T (L => X, M => Y)
           with null record;

       type T_Derived_Extended_3
         (A : Boolean) is  --  New discriminant
         new T (L => 1, M => 2)
           with null record;

       type T_Derived_Extended_4
         (A : Boolean;     --  New discriminant
          X : Positive) is
         new T (L => X, M => X)
           with null record;

    end Derived_With_Discriminants;

In this code example, we're adding the :ada:`A` discriminant when declaring
:ada:`T_Derived_Extended`. Because :ada:`T` is a tagged type, such a new
discriminant is fine.

Note that the order of the discriminants can be rearranged: when deriving a new
type, we don't need to specify the discriminants of the parent type before any
new discriminants. In fact, in the declaration of :ada:`T_Derived_Extended_2`,
the additional discriminant :ada:`A` is declared before the discriminants that
match the parent type's discriminants.

In addition, we may even use literals to specify the constraints for the parent
type |mdash| as we're doing in the declaration of :ada:`T_Derived_Extended_3`.
Also, we can use the same discriminant from the derived type for the
constraints of the parent type |mdash| in the declaration of
:ada:`T_Derived_Extended_4`, we use the :ada:`X` discriminant for both :ada:`L`
and :ada:`M` discriminants of type :ada:`T`.


Deriving with defaults
^^^^^^^^^^^^^^^^^^^^^^

If the discriminants of the parent type have default values, those default
values are inherited by the derived type. Alternatively, we can set different
default values.

Let's see a code example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Defaults

    package Derived_With_Discriminants is

       type T
         (L : Positive := 1;
          M : Positive := 2) is
         null record;

       type T_Derived is new T;

       type T_Derived_2
         (L : Positive := 1;
          M : Positive := 3) is
         new T (L => L, M => M);

    end Derived_With_Discriminants;

In this example, the derived type :ada:`T_Derived` has the same default values
as the parent type :ada:`T`, namely :ada:`(L => 1, M => 2)`. For the derived
type :ada:`T_Derived_2`, we're changing the value of :ada:`M` to 3 and keeping
the same value for :ada:`L`.

As we've seen before, instead of setting default values, we can set the
constraints of the parent type in the declaration of the derived type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Defaults_Constraints

    package Derived_With_Discriminants is

       type T
         (L : Positive := 1;
          M : Positive := 2) is
         null record;

       type T_Derived_Constrainted is new T
         (L => 1, M => 3);

    end Derived_With_Discriminants;

In this case, we're constraining the discriminants of the parent type to
:ada:`(L => 1, M => 3)`. Note that :ada:`L` has the same value as the default
value set for the parent type :ada:`T`.

.. admonition:: For further reading...

    In other contexts (such as
    :ref:`record aggregates <Adv_Ada_Record_Aggregates>`, which we discuss in
    another chapter), we could use the so-called
    :ref:`box notation <Adv_Ada_Aggregates_Box_Notation>` to specify that we
    want to use the default value. This, however, isn't possible with type
    discriminants:

    .. code:: ada compile_button manual_chop project=Courses.Advanced_Ada.Data_Types.Records.Discriminants.Derived_Types_Defaults_Constraints_Box_Notation
        :class: ada-expect-compile-error, nosyntax-check

        !derived_with_discriminants.ads
        package Derived_With_Discriminants is

           type T
             (L : Positive := 1;
              M : Positive := 2) is
             null record;

           type T_Derived_Constraint is new T
             (L => <>, M => 3);
           --   ^^^^^^^
           --  ERROR: cannot use default values
           --         via box notation
        end Derived_With_Discriminants;

    Instead of using :ada:`<>`, we have to repeat the value explicitly.


.. _Adv_Ada_Record_Discriminant_Constraints_Operations:

Discriminant constraints and operations
---------------------------------------

In this section, we discuss some details about discriminant constraints and
operations related to discriminants |mdash| more specifically, the
:ada:`Constrained` attribute.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.7.1 Discriminant Constraints <3-7-1>`


.. _Adv_Ada_Record_Discriminant_Constraints:

Discriminant constraints
~~~~~~~~~~~~~~~~~~~~~~~~

As we discussed before, when
:ref:`declaring an object with a discriminant <Adv_Ada_Record_Discriminants_Object_Declaration>`,
we have to specify the values of the all discriminants |mdash| unless, of
course, those discriminants have a
:ref:`default value <Adv_Ada_Record_Discriminants_Default_Values>`. The values
we specify for the discriminants are called discriminant constraints.

Let's revisit the code example we've seen earlier on:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Discriminant_Constraint

    package Recs is

       type T (L : Positive;
               M : Positive) is
         null record;

    end Recs;

    with Recs;        use Recs;

    procedure Show_Object_Declaration is
       A : T (L => 5, M => 6);
       B : T (7, 8);
       C : T (7, M => 8);
    begin
       null;
    end Show_Object_Declaration;

Here, :ada:`L => 5, M => 6` (for object :ada:`A`) are named constraints, while
:ada:`7, 8` (for object :ada:`B`) are positional constraints.

It's possible to use both positional and named constraints, as we do for object
:ada:`C`: :ada:`7, M => 8`. In this case, the positional associations must
precede the named associations.

In the case of named constraints, we can use multiple selector names:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Discriminant_Constraint

    with Recs;        use Recs;

    procedure Show_Object_Declaration is
       A : T (L | M => 5);
       --     ^^^^^
       --  multiple selector names
    begin
       null;
    end Show_Object_Declaration;

This is only possible, however, if those named discriminants are all of the
same type. (In this case, :ada:`L` and :ada:`M` are both of :ada:`Positive`
subtype.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.7.1 Discriminant Constraints <3-7-1>`


Discriminant constraint in subtypes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can use discriminant constraints in the declaration of subtypes. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Discriminant_Constraint

    with Recs;        use Recs;

    procedure Show_Object_Declaration is
       subtype T_5_6 is T (L => 5, M => 6);
       --                  ^^^^^^^^^^^^^^
       --  discriminant constraints for subtype

       A : T_5_6;
    begin
       null;
    end Show_Object_Declaration;

In this example, we use the named discriminant constraints
:ada:`L => 5, M => 6` in the declaration of the subtype :ada:`T_5_6`.


.. _Adv_Ada_Constrained_Attribute:

Constrained Attribute
~~~~~~~~~~~~~~~~~~~~~

We can use the :ada:`Constrained` attribute to verify whether an object of
discriminated type is constrained or not. Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Simple_Constrained_Attribute

    package Recs is

       type T (L : Positive := 1) is
         null record;

    end Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    with Recs;        use Recs;

    procedure Show_Constrained_Attribute is
       Constr   : T (L => 5);
       --            ^^^^^^ constrained.
       Unconstr : T;
       --         ^ unconstrained;
       --           using defaults.
    begin
       Put_Line ("Constr'Constrained:   "
                 & Constr'Constrained'Image);
       Put_Line ("Unconstr'Constrained: "
                 & Unconstr'Constrained'Image);
    end Show_Constrained_Attribute;

As the :ada:`Constrained` attribute indicates, the :ada:`Constr` object is
constrained (by the :ada:`L => 5` discriminant constraint), while the
:ada:`Unconstr` object is unconstrained. Note that, even though :ada:`Unconstr`
is using the default value for :ada:`L` |mdash| which would correspond to the
discriminant constraint :ada:`L => 1` |mdash| the object itself hasn't been
constraint at its declaration.

Let's continue our discussion with a more complex example by reusing
the :ada:`Unconstrained_Types` package that we declared in a
:ref:`previous section <Adv_Ada_Definite_Indefinite_Subtypes>`. In this
version of the package, we're adding a :ada:`Reset` procedure for the
discriminated record type :ada:`Simple_Record`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Constrained_Attribute

    package Unconstrained_Types is

       type Simple_Record
         (Extended : Boolean := False) is
       record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

       procedure Reset (R : in out Simple_Record);

    end Unconstrained_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Unconstrained_Types is

       procedure Reset (R : in out Simple_Record) is
          Zero_Not_Extended : constant
            Simple_Record := (Extended => False,
                              V        => 0);

          Zero_Extended : constant
            Simple_Record := (Extended => True,
                              V        => 0,
                              V_Float  => 0.0);
       begin
          Put_Line ("---- Reset: R'Constrained => "
                    & R'Constrained'Image);

          if not R'Constrained then
             R := Zero_Extended;
          else
             if R.Extended then
                R := Zero_Extended;
             else
                R := Zero_Not_Extended;
             end if;
          end if;
       end Reset;

    end Unconstrained_Types;

As the name indicates, the :ada:`Reset` procedure initializes all record
components with zero. Note that we use the :ada:`Constrained` attribute to
verify whether objects are constrained before assigning to them. For objects
that are not constrained, we can simply assign another object to it |mdash| as
we do with the :ada:`R := Zero_Extended` statement. When an object is
constrained, however, the discriminants must match. If we assign an object to
:ada:`R`, the discriminant of that object must match the discriminant of
:ada:`R`. This is the kind of verification that we do in the :ada:`else` part
of that procedure: we check the state of the :ada:`Extended` discriminant
before assigning an object to the :ada:`R` parameter.

The :ada:`Using_Constrained_Attribute` procedure below declares two objects of
:ada:`Simple_Record` type: :ada:`R1` and :ada:`R2`. Because the
:ada:`Simple_Record` type has a default value for its discriminant, we can
declare objects of this type without specifying a value for the discriminant.
This is exactly what we do in the declaration of :ada:`R1`. Here, we don't
specify any constraints, so that it takes the default value
(:ada:`Extended => False`).  In the declaration of :ada:`R2`, however, we
explicitly set :ada:`Extended` to :ada:`False`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Records.Discriminants_Constraints_Operations.Constrained_Attribute

    with Ada.Text_IO;         use Ada.Text_IO;

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Constrained_Attribute is
       R1 : Simple_Record;
       R2 : Simple_Record (Extended => False);

       procedure Show_Rs is
       begin
          Put_Line ("R1'Constrained => "
                    & R1'Constrained'Image);
          Put_Line ("R1.Extended => "
                    & R1.Extended'Image);
          Put_Line ("--");
          Put_Line ("R2'Constrained => "
                    & R2'Constrained'Image);
          Put_Line ("R2.Extended => "
                    & R2.Extended'Image);
          Put_Line ("----------------");
       end Show_Rs;
    begin
       Show_Rs;

       Reset (R1);
       Reset (R2);
       Put_Line ("----------------");

       Show_Rs;
    end Using_Constrained_Attribute;

When we run this code, the user messages from :ada:`Show_Rs` indicate to us
that :ada:`R1` is not constrained, while :ada:`R2` is constrained.
Because we declare :ada:`R1` without specifying a value for the :ada:`Extended`
discriminant, :ada:`R1` is not constrained. In the declaration of
:ada:`R2`, on the other hand, the explicit value for the :ada:`Extended`
discriminant makes this object constrained. Note that, for both :ada:`R1` and
:ada:`R2`, the value of :ada:`Extended` is :ada:`False` in the declarations.

As we were just discussing, the :ada:`Reset` procedure includes checks to avoid
mismatches in discriminants. When we don't have those checks, we might get
exceptions at runtime. We can force this situation by replacing the
implementation of the :ada:`Reset` procedure with the following lines:

.. code-block:: ada

    --  [...]
    begin
       Put_Line ("---- Reset: R'Constrained => "
                 & R'Constrained'Image);
       R := Zero_Extended;
    end Reset;

Running the code now generates a runtime exception:

::

    raised CONSTRAINT_ERROR : unconstrained_types.adb:12 discriminant check failed

This exception is raised during the call to :ada:`Reset (R2)`. As we see in the
code, :ada:`R2` is constrained. Also, its :ada:`Extended` discriminant is set
to :ada:`False`, which means that it doesn't have the :ada:`V_Float`
component. Therefore, :ada:`R2` is not compatible with the constant
:ada:`Zero_Extended` object, so we cannot assign :ada:`Zero_Extended` to
:ada:`R2`. Also, because :ada:`R2` is constrained, its :ada:`Extended`
discriminant cannot be modified.

The behavior is different for the call to :ada:`Reset (R1)`, which works fine.
Here, when we pass :ada:`R1` as an argument to the :ada:`Reset` procedure, its
:ada:`Extended` discriminant is :ada:`False` by default. Thus, :ada:`R1` is
also not compatible with the :ada:`Zero_Extended` object. However, because
:ada:`R1` is not constrained, the assignment modifies :ada:`R1` (by changing
the value of the :ada:`Extended` discriminant). Therefore, with the call to
:ada:`Reset`, the :ada:`Extended` discriminant of :ada:`R1` changes from
:ada:`False` to :ada:`True`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.7.2 Operations of Discriminated Types <3-7-2>`



.. _Adv_Ada_Unknown_Discriminants:

Unknown discriminants
---------------------

As we've seen :ref:`previously <Adv_Ada_Known_Unknown_Discriminant_Parts>`, a
type with discriminants can have known discriminants or unknown discriminants.
In this section, we focus on unknown discriminants. Because the discriminants
are unknown, this is an
:ref:`indefinite type <Adv_Ada_Definite_Indefinite_Subtypes>`.
Let's start with a simple example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Simple_Example

    package Unknown_Discriminants is

       type T_Unknown_Discr (<>) is
       --                   ^^^^
       --   Unknown discriminant part
         private;

    private

       type T_Unknown_Discr is
         null record;

    end Unknown_Discriminants;

Note that we can only use an unknown discriminant part in the
:ref:`partial view <Adv_Ada_Type_View>`; we cannot use it in the full view of a
type:

.. code:: ada manual_chop compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Wrong_Full_View
    :class: nosyntax-check, ada-expect-compile-error

    !unknown_discriminants.ads
    package Unknown_Discriminants is

       type T_Unknown_Discr (<>) is
         null record;

    end Unknown_Discriminants;

To be more precise, an unknown discriminant part can only be used in the
declaration of a private type, a private extension or an
:ref:`incomplete type <Adv_Ada_Incomplete_Types>`. In addition, as we'll see in
another chapter, it can also be used in the generic equivalents: generic
private types, generic private extensions, generic incomplete types, and formal
derived types.

.. todo::

    Add link to section on unknown discriminant parts for generics once it's
    available.

For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Simple_Example

    package Unknown_Discriminants is

       --   Private type
       type Rec (<>) is
         private;

       --   Tagged private type
       type Tagged_Rec (<>) is
         tagged private;

       --   Incomplete type
       type T_Incomplete (<>);

       type T_Incomplete (<>) is
         private;

    private

       type Rec is
         null record;

       type Tagged_Rec is
         tagged null record;

       type T_Incomplete is
         null record;

    end Unknown_Discriminants;

In this example, we have three forms of private types using an unknown
discriminant part: an untagged private type (:ada:`Rec`), a tagged type
(:ada:`Tagged_Rec`) and an incomplete type (:ada:`T_Incomplete`) that
becomes an untagged private type.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.7 Discriminants <3-7>`


.. _Adv_Ada_Unknown_Discriminants_Object_Declaration:

Object declaration
~~~~~~~~~~~~~~~~~~

Now, let's talk about objects of types with unknown discriminants. Consider
the :ada:`Rec` type below:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Object_Declaration

    package Unknown_Discriminants is

       type Rec (<>) is private;

    private

       type Rec is
       record
          I : Integer;
       end record;

    end Unknown_Discriminants;

We cannot declare objects of type :ada:`Rec` *directly*, as this type is
:ref:`indefinite <Adv_Ada_Definite_Indefinite_Subtypes>`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Object_Declaration
    :class: ada-expect-compile-error

    with Unknown_Discriminants;
    use  Unknown_Discriminants;

    procedure Show_Object_Declaration is
       A : Rec;
    begin
       null;
    end Show_Object_Declaration;

Because the type is indefinite, it requires explicit initialization |mdash| we
can do this by introducing a subprogram that initializes the type. In our
code example, we can implement a simple :ada:`Init` function for this type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Discriminants.Object_Declaration

    package Unknown_Discriminants is

       type Rec (<>) is private;

       function Init return Rec;

    private

       type Rec is
       record
          I : Integer;
       end record;

       function Init return Rec is
         ((I => 0));

    end Unknown_Discriminants;

    with Unknown_Discriminants;
    use  Unknown_Discriminants;

    procedure Show_Constructor_Function is
       R : Rec := Init;
    begin
       null;
    end Show_Constructor_Function;

In the :ada:`Show_Constructor_Function` procedure from this
example, we call the :ada:`Init` function to initialize the :ada:`R` object in
its declaration (of :ada:`Rec` type). Note that for this specific type, this is
the only possible way to declare the :ada:`R` object. In fact, compilation
fails if we write :ada:`R : Rec;`.

Using a private type with unknown discriminants is an important Ada idiom, as
we gain extra control over its initialization. For example, if we have to
ensure that certain components of the private record are initialized when an
object is being declared, we can perform this initialization in the :ada:`Init`
function |mdash| instead of just hoping that an initialization function is
called for this object at some point. Also, if further information is needed to
initialize an object, we can add parameters to the :ada:`Init` function,
thereby forcing the user to provide this information.

For even more control over objects, we can use
:ref:`limited types with unknown discriminants <Adv_Ada_Limited_Types_Unknown_Discriminants>`.


.. _Adv_Ada_Unknown_Discriminants_Partial_Full_View:

Partial and full view
~~~~~~~~~~~~~~~~~~~~~

As we've just seen, if we declare a type with an unknown discriminant part, we
can only use it in the partial view. In the full view. we cannot use an unknown
discriminant part, but have to use either no discriminants or known
discriminants. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Partial_Full_View

    package Unknown_Discriminants is

       type Rec_No_Discr (<>) is private;

       type Rec_Known_Discr (<>) is private;

    private

       type Rec_No_Discr is null record;

       type Rec_Known_Discr
         (L : Positive) is null record;

    end Unknown_Discriminants;

In this example, :ada:`Rec_No_Discr` has no discriminants in its full
view, while :ada:`Rec_Known_Discr` has the discriminant :ada:`L`.

In addition, the full view can be an (unconstrained) array type as well:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Partial_Full_View

    package Unknown_Discriminants is

       type Arr (<>) is private;

    private

       type Arr is
         array (Positive range <>)
           of Integer;

    end Unknown_Discriminants;

Here, the full view of :ada:`Arr` is an array type.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.7 Discriminants <3-7>`


.. _Adv_Ada_Unknown_Discriminants_Derived_Types:

Derived types
~~~~~~~~~~~~~

As expected, we can derive from types with unknown discriminants. Consider the
following package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Type

    package Unknown_Discriminants is

       type Rec (<>) is private;

    private

       type Rec is null record;

    end Unknown_Discriminants;

We can then declare the :ada:`Derived_Rec` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Type

    package Unknown_Discriminants.Children is

       type Derived_Rec is
         new Rec;

    end Unknown_Discriminants.Children;

Note that :ada:`Derived_Rec` has unknown discriminants, even though we're not
explicitly using an unknown discriminant part (:ada:`(<>)`) in its declaration.
(In fact, we're not allowed to use an unknown discriminant part in this case.)
Therefore, declaring objects of this type directly isn't possible, just like
the parent type :ada:`Rec`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Type
    :class: ada-expect-compile-error

    with Unknown_Discriminants.Children;
    use  Unknown_Discriminants.Children;

    procedure Show_Object_Declaration is
       A : Derived_Rec;
    begin
       null;
    end Show_Object_Declaration;

Deriving from tagged types
^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also derive from tagged types with unknown discriminants. Consider the
following package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Tagged_Type

    package Unknown_Discriminants is

       type Rec (<>) is tagged private;

    private

       type Rec is tagged null record;

    end Unknown_Discriminants;

We can derive from the :ada:`Rec` type. In this case, however, we can use
an unknown discriminant part, a known discriminant part, or no discriminants:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Tagged_Type

    package Unknown_Discriminants.Children is

       type Derived_Rec_Unknown_Discr (<>) is
         new Rec with private;

       type Derived_Rec_Known_Discr (L : Positive) is
         new Rec with private;

       type Derived_Rec_No_Discr is
         new Rec with private;

    private

       type Derived_Rec_Unknown_Discr is
         new Rec with null record;

       type Derived_Rec_Known_Discr (L : Positive) is
         new Rec with null record;

       type Derived_Rec_No_Discr is
         new Rec with null record;

    end Unknown_Discriminants.Children;

In this example, we declare :ada:`Derived_Rec_Unknown_Discr` with an unknown
discriminant part, :ada:`Derived_Rec_Known_Discr` with a known discriminant
part, and :ada:`Derived_Rec_No_Discr` with no discriminants.

As expected, :ada:`Derived_Rec_Unknown_Discr` has unknown discriminants because
it has an unknown discriminant part. In the case of
:ada:`Derived_Rec_No_Discr`, which has no discriminants, we're deriving the
unknown discriminants of :ada:`Rec`, so it also has unknown discriminants.
In contrast, because :ada:`Derived_Rec_Known_Discr` has a known discriminant
part, those discriminants are overriding the unknown discriminants of the
parent type :ada:`Rec`. Therefore, we can declare objects of
:ada:`Derived_Rec_Known_Discr` type without explicit initialization:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Tagged_Type
    :class: ada-expect-compile-error

    with Unknown_Discriminants.Children;
    use  Unknown_Discriminants.Children;

    procedure Show_Object_Declaration is
       A : Derived_Rec_Unknown_Discr;
       --  ERROR: unknown discriminants
       --         because of the type's
       --         unknown discriminant part

       B : Derived_Rec_Known_Discr (1);
       --  OK: known discriminants

       C : Derived_Rec_No_Discr;
       --  ERROR: unknown discriminants
       --         because of parent type's
       --         unknown discriminant part
    begin
       null;
    end Show_Object_Declaration;

As we can see, we can only directly declare objects of type
:ada:`Derived_Rec_Known_Discr` because it has known discriminants, while the
other two derived types have unknown discriminants |mdash| which are explicitly
specified (:ada:`Derived_Rec_Unknown_Discr`) or implicitly derived from the
parent (:ada:`Derived_Rec_No_Discr`).

Note that the parent type :ada:`Rec` had a requirement for explicit
initialization. By using known discriminants in the declaration of
:ada:`Derived_Rec_Known_Discr`, we're removing this requirement for the derived
type.

The contrary is also true: we can derive a type with known discriminants and
use an unknown discriminant part:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Tagged_Type
    :class: ada-expect-compile-error

    package Unknown_Discriminants.Children.Grand is

       type Grand_Rec_Unknown_Discr (<>) is
         new Derived_Rec_Known_Discr (1)
           with private;

    private

       type Grand_Rec_Unknown_Discr is
         new Derived_Rec_Known_Discr (1)
           with null record;

    end Unknown_Discriminants.Children.Grand;

    with Unknown_Discriminants.Children.Grand;
    use  Unknown_Discriminants.Children.Grand;

    procedure Show_Object_Declaration is
       A : Grand_Rec_Unknown_Discr;
       --  ERROR: unknown discriminants
       --         because of the type's
       --         unknown discriminant part
    begin
       null;
    end Show_Object_Declaration;

In this example, :ada:`Grand_Rec_Unknown_Discr` has unknown discriminants and
requires explicit initialization, even though its parent type
:ada:`Derived_Rec_Known_Discr` has known discriminants.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.7 Discriminants <3-7>`


.. _Adv_Ada_Unconstrained_Subtypes:

Unconstrained subtypes
----------------------

A subtype is called an unconstrained subtype if its type has unknown
discriminants. Consider a simple :ada:`Rec` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Unconstrained_Subtype

    package Unknown_Discriminants is

       type Rec (<>) is private;

    private

       type Rec is null record;

    end Unknown_Discriminants;

A subtype of :ada:`Rec` type is unconstrained:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Unconstrained_Subtype

    package Unknown_Discriminants.Children is

       subtype Rec_Unconstrained is Rec;

    end Unknown_Discriminants.Children;

In this example, :ada:`Rec_Unconstrained` is an unconstrained subtype because
it's derived from the :ada:`Rec` type. We can verify this by triggering a
compilation error:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Derived_Type
    :class: ada-expect-compile-error

    with Unknown_Discriminants.Children;
    use  Unknown_Discriminants.Children;

    procedure Show_Object_Declaration is
       A : Rec_Unconstrained;
    begin
       null;
    end Show_Object_Declaration;

In addition, if we declare a subtype based on a type that allows range, index,
or discriminant constraints, but we don't constraint the subtype, this subtype
is also considered an unconstrained subtype. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Unknown_Discriminants.Other_Unconstrained_Subtypes

    package Unconstrained_Subtypes is

       type Arr is
         array (Positive range <>) of
           Integer;

       type Rec (L : Positive) is
         null record;

       subtype Arr_Sub is Arr;
       --                 ^^^
       --  no constraints

       subtype Rec_Sub is Rec;
       --                 ^^^
       --  no constraints

    end Unconstrained_Subtypes;

In this example, :ada:`Arr_Sub` and :ada:`Rec_Sub` are unconstrained subtypes.

.. admonition:: In the Ada Reference Manual

   - :arm:`3.2 Types and Subtypes <3-2>`


Variant parts
-------------

.. admonition:: In the Ada Reference Manual

    - :arm:`3.8.1 Variant Parts and Discrete Choices <3-8-1>`






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

.. admonition:: In the Ada Reference Manual

   - :arm22:`3.8 Record Types <3-8>`


.. _Adv_Ada_Per_Object_Expressions_Default_Value:

Default value
~~~~~~~~~~~~~

We can also use per-object expressions to calculate the default value of a
record component:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Default_Value

    package Rec_Per_Object_Expressions is

       type T (D : Positive) is private;

    private

       type T (D : Positive) is record
          V : Natural := D - 1;
          --             ^^^^^
          --    Per-object expression

          S : Natural := D'Size;
          --             ^^^^^^
          --    Per-object expression
       end record;

    end Rec_Per_Object_Expressions;

Here, we calculate the default value of :ada:`V` using the per-object
expression :ada:`D - 1`, and the default of value of :ada:`S` using the
per-object :ada:`D'Size`.

The default expression for a component of a discriminated record can be
an arbitrary per-object expression. (This contrasts with
:ref:`important restrictions <Adv_Ada_Per_Object_Expressions_Restrictions>`
that exist for per-object constraints, as we discuss later on.) Such
expressions might include function calls or uses of any defined operator. For
this reason, the following code example is accepted by the compiler:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Computation

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

In this example, we can identify multiple per-object expressions that use
a computation: :ada:`S * 9 / 10`, :ada:`(S + 1) / 2`, and :ada:`S - 1`.


.. _Adv_Ada_Per_Object_Expressions_Restrictions:

Restrictions
~~~~~~~~~~~~

There are some important restrictions on per-object constraints:

#. Per-object range constraints such as :ada:`1 .. T'Size` are not allowed.

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

        .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Records.Per_Object_Expressions.Per_Object_Expression_Range_Computation
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

      In this example, using the computation :ada:`S - 1` to specify the
      range of :ada:`Arr` isn't permitted. (Note that,
      :ref:`as we've seen before <Adv_Ada_Per_Object_Expressions_Default_Value>`,
      this restriction doesn't apply when the computation is used in a
      per-object expression that calculates the default value of a component.)

#. We can only use access attributes (:ada:`T'Access` and
   :ada:`T'Unchecked_Access`) in per-object constraints.
