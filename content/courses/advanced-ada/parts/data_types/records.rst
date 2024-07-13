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


Record discriminants
--------------------



.. _Adv_Ada_Indefinite_Subtype_Discriminant:

Indefinite subtypes as discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we cannot use indefinite subtypes as discriminants. For example,
the following code won't compile:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types_Error
    :class: ada-expect-compile-error

    package Unconstrained_Types is

        type Integer_Array is
            array (Positive range <>) of Integer;

        type Simple_Record (Arr : Integer_Array) is
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


.. admonition:: In the Ada Reference Manual

   - :arm:`3.7 Discriminants <3-7>`

.. todo::

   - (MENTION: Per-object expressions)
   - Known and unknown discriminant parts
   - (MENTION: Object declaration and assignments)
   - Private types
   - Subtypes and access definitions
   - (MOVE: Indefinite subtypes as discriminants)
   - Default values
      - (MENTION: Definite and Indefinite Subtypes)
   - Derived types / subtypes


..
    TO BE DONE:

    Discriminant constraints and operations
    ---------------------------------------

    .. admonition:: In the Ada Reference Manual

        - :arm:`3.7.1 Discriminant Constraints <3-7-1>`

    .. todo::

        - Discriminant constraint
        - (MOVE: Constrained Attribute)


..
    TO BE DONE:

    Unknown discriminants
    ---------------------

    .. admonition:: In the Ada Reference Manual

        - :arm:`3.7 Discriminants <3-7>`

    .. todo::

        - Complete section!


..
    TO BE DONE:

    Variant parts
    -------------

    .. admonition:: In the Ada Reference Manual

        - :arm:`3.8.1 Variant Parts and Discrete Choices <3-8-1>`

    .. todo::

        - Complete section!



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
