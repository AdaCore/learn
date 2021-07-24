Aggregates
==========

.. include:: ../../global.txt

Null records
------------

A null record is a record that doesn't have any component. Consequently, it
cannot store any information on it. When declaring a null record, we simply
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

Simple Prototyping
~~~~~~~~~~~~~~~~~~

A null record doesn't provide much functionality on itself, as we're not
storing any information on it. However, it's far from being useless. For
example, we can make use of null records to design an API, which we can then
use in a application without having to implement the actual functionality of
the API. This allows us to design a prototype without having to think about all
the implementation details of the API on a first stage.

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
:ref:`later on in the course <Null_Procedures>`.

In the :ada:`Show_Device` procedure |mdash| which is an application
that implements our prototype |mdash|, we declare an object of :ada:`Device`
type and call all subprograms associated with that type.

Extending prototype
~~~~~~~~~~~~~~~~~~~

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

Implementing prototype
~~~~~~~~~~~~~~~~~~~~~~

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
:ref:`later on in the course <Abstract_Types_And_Subprograms>`.

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
later on in the course.

Extension Aggregates
--------------------

.. admonition:: Relevant topics

    - `Extension Aggregates <http://www.ada-auth.org/standards/2xrm/html/RM-4-3-2.html>`_

.. todo::

    Complete section!


Delta Aggregates
----------------

.. admonition:: Relevant topics

    - `Delta Aggregates <http://www.ada-auth.org/standards/2xrm/html/RM-4-3-4.html>`_

.. todo::

    Complete section!


Container Aggregates
--------------------

.. admonition:: Relevant topics

    - `Container Aggregates <http://www.ada-auth.org/standards/2xrm/html/RM-4-3-5.html>`_

.. todo::

    Complete section!
