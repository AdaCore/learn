Handling Variability and Re-usability
=======================================

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

:code-config:`reset_accumulator=True`

.. include:: ../../global.txt

Understanding static and dynamic variability
--------------------------------------------

It is common to see embedded software being used in a variety of configurations
that require small changes to the code for each instance. For example, the same
application may need to be portable between two different architectures (ARM and
x86), or two different platforms with different set of devices available. Maybe
the same application is used for two different generations of the product, so it
needs to account for absence or presence of new features, or it's used for
different projects which may select different components or configurations. All
these cases, and many others, require variability in the software in order to
ensure its reusability.

In C, variability is usually achieved through macros and function pointers, the
former being tied to static variability (variability in different
builds) the latter to dynamic variability (variability within the same build
decided at run-time).

Ada offers many alternatives for both techniques, which aim at structuring
possible variations of the software. When Ada isn't enough, the GNAT
compilation system also provides a layer of capabilities, in particular
selection of alternate bodies.

If you're familiar with object-oriented programming (OOP) |mdash| supported in
languages such as C++ and Java |mdash|, you might also be interested in knowing
that OOP is supported by Ada and can be used to implement variability. This
should, however, be used with care, as OOP brings its own set of problems, such
as loss of efficiency |mdash| dispatching calls can't be inlined and require one
level  of indirection |mdash| or loss of analyzability |mdash| the target of a
dispatching call isn't known at run time. As a rule of thumb, OOP should be
considered only for cases of dynamic variability, where several versions of the
same object need to exist concurrently in the same application.

Handling variability & reusability statically
---------------------------------------------

Genericity
~~~~~~~~~~

One usage of C macros involves the creation of functions that works regardless
of the type they're being called upon. For example, a swap macro may look like:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Swap_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    #define SWAP(t, a, b) ({\
                               t tmp = a; \
                               a = b; \
                               b = tmp; \
                            })

    int main()
    {
        int a = 10;
        int b = 42;

        printf("a = %d, b = %d\n", a, b);

        SWAP (int, a, b);

        printf("a = %d, b = %d\n", a, b);
    }

Ada offers a way to declare this kind of functions as a generic, that is, a
function that is written after static arguments, such as a parameter:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Swap_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       generic
          type A_Type is private;
       procedure Swap (Left, Right : in out A_Type);

       procedure Swap (Left, Right : in out A_Type) is
          Temp : constant A_Type := Left;
       begin
          Left  := Right;
          Right := Temp;
       end Swap;

       procedure Swap_I is new Swap (Integer);

       A : Integer := 10;
       B : Integer := 42;

    begin
       Put_Line ("A = "
                 & Integer'Image (A)
                 & ", B = "
                 & Integer'Image (B));

       Swap_I (A, B);

       Put_Line ("A = "
                 & Integer'Image (A)
                 & ", B = "
                 & Integer'Image (B));
    end Main;

There are a few key differences between the C and the Ada version here. In C,
the macro can be used directly and essentially get expanded by the preprocessor
without any kind of checks. In Ada, the generic will first be checked for
internal consistency. It then needs to be explicitly instantiated for a
concrete type. From there, it's exactly as if there was an actual version of
this :ada:`Swap` function, which is going to be called as any other function.
All rules for parameter modes and control will apply to this instance.

In many respects, an Ada generic is a way to provide a safe specification and
implementation of such macros, through both the validation of the generic
itself and its usage.

Subprograms aren't the only entities that can me made generic. As a matter of
fact, it's much more common to render an entire package generic. In this case
the instantiation creates a new version of all the entities present in the
generic, including global variables. For example:

:code-config:`accumulate_code=True`

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Gen_Pkg_1

    generic
       type T is private;
    package Gen is
       type C is tagged record
          V : T;
       end record;

       G : Integer;
    end Gen;

The above can be instantiated and used the following way:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Gen_Pkg_1

    with Gen;

    procedure Main is
       package I1 is new Gen (Integer);
       package I2 is new Gen (Integer);
       subtype Str10 is String (1..10);
       package I3 is new Gen (Str10);
    begin
       I1.G := 0;
       I2.G := 1;
       I3.G := 2;
    end Main;

:code-config:`accumulate_code=False`

Here, :ada:`I1.G`, :ada:`I2.G` and :ada:`I3.G` are three distinct variables.

So far, we've only looked at generics with one kind of parameter: a so-called
private type. There's actually much more that can be described in this section,
such as variables, subprograms or package instantiations with certain
properties. For example, the following provides a sort algorithm for any kind
of array:

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Gen_Pkg_2

    generic
       type Component is private;
       type Index is (<>);
       with function "<" (Left, Right : Component) return Boolean;
       type Array_Type is array (Index range <>) of Component;
    procedure Sort (A : in out Array_Type);

The declaration above states that we need a type (:ada:`Component`), a discrete
type (:ada:`Index`), a comparison subprogram (:ada:`"<"`), and an array
definition (:ada:`Array_Type`). Given these, it's possible to write an
algorithm that can sort any :ada:`Array_Type`. Note the usage of the with
reserved word in front of the function name: it exists to differentiate between
the generic parameter and the beginning of the generic subprogram.

Here is a non-exhaustive overview of the kind of constraints that can be put on
types:

.. code-block:: ada

    type T is private; -- T is a constrained type, such as Integer
    type T (<>) is private; -- T can be an unconstrained type e.g. String
    type T is tagged private; -- T is a tagged type
    type T is new T2 with private; -- T is an extension of T2
    type T is (<>); -- T is a discrete type
    type T is range <>; -- T is an integer type
    type T is digits <>; -- T is a floating point type
    type T is access T2; -- T is an access type to T2

Simple derivation
~~~~~~~~~~~~~~~~~

Let's take a case where a codebase needs to handle small variations of a given
device, or maybe different generations of a device, depending on the platform
it's running on. In this example, we're assuming that each platform will lead
to a different binary, so the code can statically resolve which set of services
are available. However, we want an easy way to implement a new device based on
a previous one, saying "this new device is the same as this previous device,
with these new services and these changes in existing services".

We can implement such patterns using Ada's simple derivation |mdash| as opposed
to tagged derivation, which is OOP-related and discussed in a later section.

Let's start from the following example:

:code-config:`accumulate_code=True`

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    package Drivers_1 is

       type Device_1 is null record;
       procedure Startup (Device : Device_1);
       procedure Send (Device : Device_1; Data : Integer);
       procedure Send_Fast (Device : Device_1; Data : Integer);
       procedure Receive (Device : Device_1; Data : out Integer);

    end Drivers_1 ;

    package body Drivers_1 is

       --  NOTE: unimplemented procedures: Startup, Send, Send_Fast
       --        mock-up implementation: Receive

       procedure Startup (Device : Device_1) is null;

       procedure Send (Device : Device_1; Data : Integer) is null;

       procedure Send_Fast (Device : Device_1; Data : Integer) is null;

       procedure Receive (Device : Device_1; Data : out Integer) is
       begin
          Data := 42;
       end Receive;

    end Drivers_1 ;

In the above example, :ada:`Device_1` is an empty record type. It may also have
some fields if required, or be a different type such as a scalar. Then the four
procedures :ada:`Startup`, :ada:`Send`, :ada:`Send_Fast` and :ada:`Receive` are
primitives of this type. A primitive is essentially a subprogram that has a
parameter or return type directly referencing this type and declared in the
same scope. At this stage, there's nothing special with this type: we're using
it as we would use any other type. For example:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    with Ada.Text_IO; use Ada.Text_IO;
    with Drivers_1;   use Drivers_1;

    procedure Main is
       D : Device_1;
       I : Integer;
    begin
       Startup (D);
       Send_Fast (D, 999);
       Receive (D, I);
       Put_Line (Integer'Image (I));
    end Main;

Let's now assume that we need to implement a new generation of device,
:ada:`Device_2`. This new device works exactly like the first one, except for
the startup code that has to be done differently. We can create a new type that
operates exactly like the previous one, but modifies only the behavior of
:ada:`Startup`:

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    with Drivers_1; use Drivers_1;

    package Drivers_2 is

       type Device_2 is new Device_1;

       overriding
       procedure Startup (Device : Device_2);

    end Drivers_2;

    package body Drivers_2 is

       overriding
       procedure Startup (Device : Device_2) is null;

    end Drivers_2;


Here, :ada:`Device_2` is derived from :ada:`Device_1`. It contains all the
exact same properties and primitives, in particular, :ada:`Startup`,
:ada:`Send`, :ada:`Send_Fast` and :ada:`Receive`. However, here, we decided to
change the :ada:`Startup` function and to provide a different implementation.
We override this function. The main subprogram doesn't change much, except for
the fact that it now relies on a different type:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    with Ada.Text_IO; use Ada.Text_IO;
    with Drivers_2;   use Drivers_2;

    procedure Main is
       D : Device_2;
       I : Integer;
    begin
       Startup (D);
       Send_Fast (D, 999);
       Receive (D, I);
       Put_Line (Integer'Image (I));
    end Main;

We can continue with approach and introduce a new generation of devices. This
new device doesn't implement the :ada:`Send_Fast` service so we want
to remove it from the list of available services. Furthermore, for the purpose
of our example, let's assume that the hardware team went back to the
:ada:`Device_1` way of implementing :ada:`Startup`. We can write this new
device the following way:

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    with Drivers_1; use Drivers_1;

    package Drivers_3 is

       type Device_3 is new Device_1;

       overriding
       procedure Startup (Device : Device_3);

       procedure Send_Fast (Device : Device_3; Data : Integer)
       is abstract;

    end Drivers_3;

    package body Drivers_3 is

       overriding
       procedure Startup (Device : Device_3) is null;

    end Drivers_3;

The :ada:`is abstract` definition makes illegal any call to a function, so
calls to :ada:`Send_Fast` on :ada:`Device_3` will be flagged as being illegal.
To then implement :ada:`Startup` of :ada:`Device_3` as being the same as the
:ada:`Startup` of :ada:`Device_1`, we can convert the type in the
implementation:

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    package body Drivers_3 is

       overriding
       procedure Startup (Device : Device_3) is
       begin
          Drivers_1.Startup (Device_1 (Device));
       end Startup;

    end Drivers_3;

Our :ada:`Main` now looks like:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    with Ada.Text_IO; use Ada.Text_IO;
    with Drivers_3;   use Drivers_3;

    procedure Main is
       D : Device_3;
       I : Integer;
    begin
       Startup (D);
       Send_Fast (D, 999);
       Receive (D, I);
       Put_Line (Integer'Image (I));
    end Main;

:code-config:`accumulate_code=False`

Here, the call to :ada:`Send_Fast` will get flagged by the compiler.

Note that the fact that the code of :ada:`Main` has to be changed for every
implementation isn't necessary satisfactory. We may want to go one step
further, and isolate the selection of the device kind to be used for the whole
application in one unique file. One way to do this is to use the same name for
all types, and use a renaming to select which package to use. Here's a
simplified example to illustrate that:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Derived_Drivers

    package Drivers_1 is

       type Device_Type is null record;
       procedure Send (Device : Device_Type; Data : Integer);
       procedure Receive (Device : Device_Type; Data : out Integer);

    end Drivers_1;

    package body Drivers_1 is

       procedure Send (Device : Device_Type; Data : Integer) is null;

       procedure Receive (Device : Device_Type; Data : out Integer) is
          pragma Unreferenced (Device);
       begin
          Data := 42;
       end Receive;

    end Drivers_1;

    with Drivers_1;

    package Drivers_2 is

       type Device_Type is new Drivers_1.Device_Type;
       procedure Send (Device : Device_Type; Data : Integer);
       procedure Receive (Device : Device_Type; Data : out Integer);

    end Drivers_2;

    package body Drivers_2 is

       procedure Send (Device : Device_Type; Data : Integer) is null;

       procedure Receive (Device : Device_Type; Data : out Integer) is
          pragma Unreferenced (Device);
       begin
          Data := 42;
       end Receive;

    end Drivers_2;

    with Drivers_1;

    package Drivers renames Drivers_1;

    with Ada.Text_IO; use Ada.Text_IO;
    with Drivers;     use Drivers;

    procedure Main is
       D : Device_Type;
       I : Integer;
    begin
       Send (D, 999);
       Receive (D, I);
       Put_Line (Integer'Image (I));
    end Main;

In the above example, the whole code can rely on :file:`drivers.ads`, instead
of relying on the specific driver. Here, :ada:`Driver`  is an alias to
:ada:`Driver_1`. In order to switch to :ada:`Driver_2`, the project only has to
replace that one :file:`drivers.ads` file.

In the following section, we'll go one step further and demonstrate that this
selection can be done through a configuration switch selected at build time
instead of a manual code modification.

Configuration specific files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Handling variability & reusability dynamically
----------------------------------------------

Records with discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Object orientation
~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Pointer to subprograms
~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Design by components using dynamic libraries
--------------------------------------------

.. todo::

    Complete section!

