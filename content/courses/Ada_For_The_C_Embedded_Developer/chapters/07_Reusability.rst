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

.. _SimpleDerivation:

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

       type Transceiver is null record;
       procedure Send (Device : Transceiver; Data : Integer);
       procedure Receive (Device : Transceiver; Data : out Integer);

    end Drivers_1;

    package body Drivers_1 is

       procedure Send (Device : Transceiver; Data : Integer) is null;

       procedure Receive (Device : Transceiver; Data : out Integer) is
          pragma Unreferenced (Device);
       begin
          Data := 42;
       end Receive;

    end Drivers_1;

    with Drivers_1;

    package Drivers_2 is

       type Transceiver is new Drivers_1.Transceiver;
       procedure Send (Device : Transceiver; Data : Integer);
       procedure Receive (Device : Transceiver; Data : out Integer);

    end Drivers_2;

    package body Drivers_2 is

       procedure Send (Device : Transceiver; Data : Integer) is null;

       procedure Receive (Device : Transceiver; Data : out Integer) is
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
       D : Transceiver;
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

.. _RecordsWithDiscriminants:

Records with discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Object orientation
~~~~~~~~~~~~~~~~~~

In the :ref:`previous section <RecordsWithDiscriminants>`, we've seen that we
can add variability to records by using discriminants. Another approach is to
use *tagged* records, which are the base for object-oriented programming in
Ada.

Type extension
^^^^^^^^^^^^^^

A tagged record type is declared by adding the :ada:`tagged` keyword. For
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Tagged_Type_Decl

    procedure Main is

       type Rec is record
          V : Integer;
       end record;

       type Tagged_Rec is tagged record
          V : Integer;
       end record;

       R1 : Rec;
       R2 : Tagged_Rec;

       pragma Unreferenced (R1, R2);
    begin
       R1 := (V => 0);
       R2 := (V => 0);
    end Main;

In this simple example, there isn't much difference between the :ada:`Rec` and
:ada:`Tagged_Rec` type. However, tagged types can be derived and extended. For
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Tagged_Type_Extension_Decl

    procedure Main is

       type Rec is record
          V : Integer;
       end record;

       --  We cannot declare this:
       --
       --  type Ext_Rec is new Rec with record
       --     V : Integer;
       --  end record;

       type Tagged_Rec is tagged record
          V : Integer;
       end record;

       --  But we can declare this:
       --
       type Ext_Tagged_Rec is new Tagged_Rec with record
          V2 : Integer;
       end record;

       R1 : Rec;
       R2 : Tagged_Rec;
       R3 : Ext_Tagged_Rec;

       pragma Unreferenced (R1, R2, R3);
    begin
       R1 := (V => 0);
       R2 := (V => 0);
       R3 := (V => 0, V2 => 0);
    end Main;

As indicated in the example, a type derived from an untagged type cannot have
an extension. The compiler indicates this error if you uncomment the
declaration of the :ada:`Ext_Rec` type above. In contrast, we can extend a
tagged type, as we did in the declaration of :ada:`Ext_Tagged_Rec`. In this
case, :ada:`Ext_Tagged_Rec` has all the components of the :ada:`Tagged_Rec`
type (:ada:`V`, in this case) plus the additional components from its own type
declaration (:ada:`V2`, in this case).

Overriding subprograms
^^^^^^^^^^^^^^^^^^^^^^

Previously, we've seen that subprograms can be overriden. For example, if we
had implemented a :ada:`Reset` and a :ada:`Display` procedure for the
:ada:`Rec` type that we declared above, these procedures would be available for
an :ada:`Ext_Rec` type derived from :ada:`Rec`. Also, we could override these
procedures for the :ada:`Ext_Rec` type. In Ada, we don't need object-oriented
programming features to do that: simple (untagged) records can be used to
derive types, inherit operations and override them. However, in applications
where the actual subprogram to be called is determined dynamically at run-time,
we need dispatching calls. In this case, we must use tagged types to implement
this.

Comparing untagged and tagged types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's discuss the similarities and differences between untagged and tagged
types based on this example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Tagged_Type_Extension_Decl

    package P is

       type Rec is record
          V : Integer;
       end record;

       procedure Display (R : in Rec);
       procedure Reset (R : out Rec);

       type New_Rec is new Rec;

       overriding procedure Display (R : in New_Rec);
       not overriding procedure New_Op (R : in out New_Rec);

       type Tagged_Rec is tagged record
          V : Integer;
       end record;

       procedure Display (R : in Tagged_Rec);
       procedure Reset (R : out Tagged_Rec);

       type Ext_Tagged_Rec is new Tagged_Rec with record
          V2 : Integer;
       end record;

       overriding procedure Display (R : in Ext_Tagged_Rec);
       overriding procedure Reset (R : out Ext_Tagged_Rec);
       not overriding procedure New_Op (R : in out Ext_Tagged_Rec);

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Display (R : in Rec) is
       begin
          Put_Line ("TYPE: REC");
          Put_Line ("Rec.V = " & Integer'Image (R.V));
          New_Line;
       end Display;

       procedure Reset (R : out Rec) is
       begin
          R.V := 0;
       end Reset;

       procedure Display (R : in New_Rec) is
       begin
          Put_Line ("TYPE: NEW_REC");
          Put_Line ("New_Rec.V = " & Integer'Image (R.V));
          New_Line;
       end Display;

       procedure New_Op (R : in out New_Rec) is
       begin
          R.V := R.V + 1;
       end New_Op;

       procedure Display (R : in Tagged_Rec) is
       begin
          --  Using External_Tag attribute to retrieve the tag as a string
          Put_Line ("TYPE: " & Tagged_Rec'External_Tag);
          Put_Line ("Tagged_Rec.V = " & Integer'Image (R.V));
          New_Line;
       end Display;

       procedure Reset (R : out Tagged_Rec) is
       begin
          R.V := 0;
       end Reset;

       procedure Display (R : in Ext_Tagged_Rec) is
       begin
          --  Using External_Tag attribute to retrieve the tag as a string
          Put_Line ("TYPE: " & Ext_Tagged_Rec'External_Tag);
          Put_Line ("Ext_Tagged_Rec.V  = " & Integer'Image (R.V));
          Put_Line ("Ext_Tagged_Rec.V2 = " & Integer'Image (R.V2));
          New_Line;
       end Display;

       procedure Reset (R : out Ext_Tagged_Rec) is
       begin
          Tagged_Rec (R).Reset;
          R.V2 := 0;
       end Reset;

       procedure New_Op (R : in out Ext_Tagged_Rec) is
       begin
          R.V := R.V + 1;
       end New_Op;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with P;           use P;

    procedure Main is
       X_Rec              : Rec;
       X_New_Rec          : New_Rec;

       X_Tagged_Rec       : aliased Tagged_Rec;
       X_Ext_Tagged_Rec   : aliased Ext_Tagged_Rec;

       X_Tagged_Rec_Array : constant array (1 .. 2) of access Tagged_Rec'Class
                             := (X_Tagged_Rec'Access, X_Ext_Tagged_Rec'Access);
    begin
       --
       --  Reset all objects
       --
       Reset (X_Rec);
       Reset (X_New_Rec);
       X_Tagged_Rec.Reset;    --  we could write "Reset (X_Tagged_Rec)" as well
       X_Ext_Tagged_Rec.Reset;

       --
       --  Use new operations when available
       --
       New_Op (X_New_Rec);
       X_Ext_Tagged_Rec.New_Op;

       --
       --  Display all objects
       --
       Display (X_Rec);
       Display (X_New_Rec);
       X_Tagged_Rec.Display;  --  we could write "Display (X_Tagged_Rec)" as well
       X_Ext_Tagged_Rec.Display;

       --
       --  Resetting and display objects of Tagged_Rec'Class
       --
       Put_Line ("Operations on Tagged_Rec'Class");
       Put_Line ("------------------------------");
       for E of X_Tagged_Rec_Array loop
          E.Reset;
          E.Display;
       end loop;
    end Main;

These are the similarities between untagged and tagged types:

- We can derive types and inherit operations in both cases.

    - Both :ada:`X_New_Rec` and :ada:`X_Ext_Tagged_Rec` inherit the
      :ada:`Display` and :ada:`Reset` procedures from their respective
      ancestors.

- We can override operations in both cases.

- We can implement new operations in both cases.

    - Both :ada:`X_New_Rec` and :ada:`X_Ext_Tagged_Rec` implement a procedure
      called :ada:`New_Op`, which is not available for their respective
      ancestors.

Now, let's look at the differences between untagged and tagged types:

- We can dispatch calls for a given type class.

    - This is what we do when we iterate over objects of the
      :ada:`Tagged_Rec` class |mdash| in the loop over
      :ada:`X_Tagged_Rec_Array` at the last part of the :ada:`Main` procedure.

- We can use the dot notation.

    - We can write both :ada:`E.Reset` or :ada:`Reset (E)` forms: they're
      equivalent.

Dispatching calls
^^^^^^^^^^^^^^^^^

Let's look more closely at the dispatching calls implemented above. First, we
declare the :ada:`X_Tagged_Rec_Array` array and initialize it with the access
to objects of both parent and derived tagged types:

.. code-block:: ada

       X_Tagged_Rec       : aliased Tagged_Rec;
       X_Ext_Tagged_Rec   : aliased Ext_Tagged_Rec;

       X_Tagged_Rec_Array : constant array (1 .. 2) of access Tagged_Rec'Class
                             := (X_Tagged_Rec'Access, X_Ext_Tagged_Rec'Access);

Here, we use the :ada:`aliased` keyword to be able to get access to the objects
(via the :ada:`'Access` attribute).

Then, we loop over this array and call the :ada:`Reset` and :ada:`Display`
procedures:

.. code-block:: ada

       for E of X_Tagged_Rec_Array loop
          E.Reset;
          E.Display;
       end loop;

Since we're using dispatching calls, the actual procedure that is selected
depends on the type of the object. For the first element
(:ada:`X_Tagged_Rec_Array (1)`), this is :ada:`Tagged_Rec`, while for the
second element (:ada:`X_Tagged_Rec_Array (2)`), this is :ada:`Ext_Tagged_Rec`.

Dispatching calls are only possible for a type class |mdash| for example, the
:ada:`Tagged_Rec'Class`. When the type of an object is known at compile time,
the calls won't dispatch at runtime. For example, the call to the :ada:`Reset`
procedure of the  :ada:`X_Ext_Tagged_Rec` object
(:ada:`X_Ext_Tagged_Rec.Reset`) will always take the overriden
:ada:`Reset` procedure of the :ada:`Ext_Tagged_Rec` type. Similarly, if we
perform a view conversion by writing
:ada:`Tagged_Rec (A_Ext_Tagged_Rec).Display`, we're instructing the compiler to
interpret :ada:`A_Ext_Tagged_Rec` as an object of type :ada:`Tagged_Rec`, so
that the compiler selects the :ada:`Display` procedure of the :ada:`Tagged_Rec`
type.

Interfaces
^^^^^^^^^^

Another useful feature of object-oriented programming is the use of interfaces.
In this case, we can define abstract operations, and implement them in the
derived tagged types. We declare an interface by simply writing
:ada:`type T is interface`. For example:

.. code-block:: ada

    type My_Interface is interface;

    procedure Op (Obj : My_Interface) is abstract;

    --  We cannot declare actual objects of an interface:
    --
    --  Obj : My_Interface;  --   ERROR!

All operations on an interface type are abstract, so we need to write
:ada:`is abstract` in the signature |mdash| as we did in the declaration of
:ada:`Op` above. Also, since interfaces are abstract types and don't have an
actual implementation, we cannot declare objects for it.

We can derive tagged types from an interface and implement the actual
operations of that interface:

.. code-block:: ada

    type My_Derived is new My_Interface with null record;

    procedure Op (Obj : My_Derived);

Note that we're not using the :ada:`tagged` keyword in the declaration because
any type derived from an interface is automatically tagged.

Let's look at an example with an interface and two derived tagged types:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Interfaces_1

    package P is

       type Display_Interface is interface;
       procedure Display (D : Display_Interface) is abstract;

       type Small_Display_Type is new Display_Interface with null record;
       procedure Display (D : Small_Display_Type);

       type Big_Display_Type is new Display_Interface with null record;
       procedure Display (D : Big_Display_Type);

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Display (D : Small_Display_Type) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Using Small_Display_Type");
       end Display;

       procedure Display (D : Big_Display_Type) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Using Big_Display_Type");
       end Display;

    end P;

    with P; use P;

    procedure Main is
       D_Small : Small_Display_Type;
       D_Big   : Big_Display_Type;

       procedure Dispatching_Display (D : Display_Interface'Class) is
       begin
          D.Display;
       end;

    begin
       Dispatching_Display (D_Small);
       Dispatching_Display (D_Big);
    end Main;

In this example, we have an interface type :ada:`Display_Interface` and two
tagged types that are derived from :ada:`Display_Interface`:
:ada:`Small_Display_Type` and :ada:`Big_Display_Type`.

Both types (:ada:`Small_Display_Type` and :ada:`Big_Display_Type`) implement
the interface by overriding the :ada:`Display` procedure. Then, in the inner
procedure :ada:`Dispatching_Display` of the :ada:`Main` procedure, we perform
a dispatching call depending on the actual type of :ada:`D`.

Deriving from multiple interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code-config:`accumulate_code=True`

We may derive a type from multiple interfaces by simply writing
:ada:`type Derived_T is new T1 and T2 with null record`. For example:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Multiple_Interfaces

    package Transceivers is

       type Send_Interface is interface;

       procedure Send (Obj : in out Send_Interface) is abstract;

       type Receive_Interface is interface;

       procedure Receive (Obj : in out Receive_Interface) is abstract;

       type Transceiver is new Send_Interface and Receive_Interface with null record;

       procedure Send (D : in out Transceiver);
       procedure Receive (D : in out Transceiver);

    end Transceivers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Transceivers is

       procedure Send (D : in out Transceiver) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Sending data...");
       end Send;

       procedure Receive (D : in out Transceiver) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Receiving data...");
       end Receive;

    end Transceivers;

    with Transceivers; use Transceivers;

    procedure Main is
       D : Transceiver;
    begin
       D.Send;
       D.Receive;
    end Main;

In this example, we're declaring two interfaces (:ada:`Send_Interface` and
:ada:`Receive_Interface`) and the tagged type :ada:`Transceiver` that derives
from both interfaces. Since we need to implement the interfaces, we implement
both :ada:`Send` and :ada:`Receive` for :ada:`Transceiver`.

Abstract tagged types
^^^^^^^^^^^^^^^^^^^^^

We may also declare abstract tagged types. Note that, because the type is
abstract, we cannot use it to declare objects for it |mdash| this is the same
as for interfaces. We can only use it to derive other types. Let's look at the
abstract tagged type declared in the :ada:`Abstract_Transceivers` package:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Multiple_Interfaces
   :class: ada-expect-compile-error

    with Transceivers; use Transceivers;

    package Abstract_Transceivers is

       type Abstract_Transceiver is abstract new Send_Interface and
         Receive_Interface with null record;

       procedure Send (D : in out Abstract_Transceiver);
       --  We don't implement Receive for Abstract_Transceiver!

    end Abstract_Transceivers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Abstract_Transceivers is

       procedure Send (D : in out Abstract_Transceiver) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Sending data...");
       end Send;

    end Abstract_Transceivers;

    with Abstract_Transceivers; use Abstract_Transceivers;

    procedure Main is
       D : Abstract_Transceiver;
    begin
       D.Send;
       D.Receive;
    end Main;

In this example, we declare the abstract tagged type
:ada:`Abstract_Transceiver`. Here, we're only partially implementing the
interfaces from which this type is derived: we're implementing :ada:`Send`, but
we're skipping the implementation of :ada:`Receive`. Therefore, :ada:`Receive`
is an abstract operation of :ada:`Abstract_Transceiver`. Since any type that
has abstract operations is abstract, we must indicate this by adding the
:ada:`abstract` keyword in type declaration.

Also, when compiling this example, we get an error because we're trying to
declare an object of :ada:`Abstract_Transceiver` (in the :ada:`Main`
procedure), which is not possible. Naturally, if we derive another type from
:ada:`Abstract_Transceiver` and implement :ada:`Receive` as well, then we can
declare objects of this derived type. This is what we do in the
:ada:`Full_Transceivers` below:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Multiple_Interfaces

    with Abstract_Transceivers; use Abstract_Transceivers;

    package Full_Transceivers is

       type Full_Transceiver is new Abstract_Transceiver with null record;
       procedure Receive (D : in out Full_Transceiver);

    end Full_Transceivers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Full_Transceivers is

       procedure Receive (D : in out Full_Transceiver) is
          pragma Unreferenced (D);
       begin
          Put_Line ("Receiving data...");
       end Receive;

    end Full_Transceivers;

    with Full_Transceivers; use Full_Transceivers;

    procedure Main is
       D : Full_Transceiver;
    begin
       D.Send;
       D.Receive;
    end Main;

Here, we implement the :ada:`Receive` procedure for the
:ada:`Full_Transceiver`. Therefore, the type doesn't have any abstract
operation, so we can use it to declare objects.

:code-config:`reset_accumulator=True`

:code-config:`accumulate_code=False`

From simple derivation to OOP
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the :ref:`section about simple derivation <SimpleDerivation>`, we've seen an
example where the actual selection was done at *implementation* time by
renaming one of the packages:

.. code-block:: ada

    with Drivers_1;

    package Drivers renames Drivers_1;

Although this approach is useful in many cases, there might be situations where
we need to select the actual driver dynamically at runtime. Let's look at how
we could rewrite that example using interfaces, tagged types and dispatching
calls:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Tagged_Drivers

    package Drivers_Base is

       type Transceiver is interface;

       procedure Send (Device : Transceiver; Data : Integer) is abstract;
       procedure Receive (Device : Transceiver; Data : out Integer) is abstract;
       procedure Display (Device : Transceiver) is abstract;

    end Drivers_Base;

    with Drivers_Base;

    package Drivers_1 is

       type Transceiver is new Drivers_Base.Transceiver with null record;

       procedure Send (Device : Transceiver; Data : Integer);
       procedure Receive (Device : Transceiver; Data : out Integer);
       procedure Display (Device : Transceiver);

    end Drivers_1;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Drivers_1 is

       procedure Send (Device : Transceiver; Data : Integer) is null;

       procedure Receive (Device : Transceiver; Data : out Integer) is
          pragma Unreferenced (Device);
       begin
          Data := 42;
       end Receive;

       procedure Display (Device : Transceiver) is
          pragma Unreferenced (Device);
       begin
          Put_Line ("Using Drivers_1");
       end Display;

    end Drivers_1;

    with Drivers_Base;

    package Drivers_2 is

       type Transceiver is new Drivers_Base.Transceiver with null record;

       procedure Send (Device : Transceiver; Data : Integer);
       procedure Receive (Device : Transceiver; Data : out Integer);
       procedure Display (Device : Transceiver);

    end Drivers_2;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Drivers_2 is

       procedure Send (Device : Transceiver; Data : Integer) is null;

       procedure Receive (Device : Transceiver; Data : out Integer) is
          pragma Unreferenced (Device);
       begin
          Data := 42;
       end Receive;

       procedure Display (Device : Transceiver) is
          pragma Unreferenced (Device);
       begin
          Put_Line ("Using Drivers_2");
       end Display;

    end Drivers_2;

    with Ada.Text_IO; use Ada.Text_IO;

    with Drivers_Base;
    with Drivers_1;
    with Drivers_2;

    procedure Main is
       D1 : aliased Drivers_1.Transceiver;
       D2 : aliased Drivers_2.Transceiver;
       D  : access Drivers_Base.Transceiver'Class;

       I  : Integer;

       type Driver_Number is range 1 .. 2;

       procedure Select_Driver (N : Driver_Number) is
       begin
          if N = 1 then
             D := D1'Access;
          else
             D := D2'Access;
          end if;
          D.Display;
       end Select_Driver;

    begin
       Select_Driver (1);
       D.Send (999);
       D.Receive (I);
       Put_Line (Integer'Image (I));

       Select_Driver (2);
       D.Send (999);
       D.Receive (I);
       Put_Line (Integer'Image (I));
    end Main;

In this example, we declare the :ada:`Transceiver` interface in the
:ada:`Drivers_Base` package. This interface is then used to derive the tagged
types :ada:`Transceiver` from both :ada:`Drivers_1` and :ada:`Drivers_2`
packages.

In the :ada:`Main` procedure, we use the access to :ada:`Transceiver'Class`
|mdash| from the interface declared in the :ada:`Drivers_Base` package |mdash|
to declare :ada:`D`. This object :ada:`D` contains the access to the actual
driver loaded at any specific time. We select the driver at runtime in the
inner :ada:`Select_Driver` procedure, which initializes :ada:`D` (with the
access to the selected driver). Then, any operation on :ada:`D` triggers a
dispatching call to the selected driver.

Further resources
^^^^^^^^^^^^^^^^^

In the appendices, we have a step-by-step
:ref:`hands-on overview of object-oriented programming <HandsOnOOP>` that
discusses how to translate a simple system written in C to an equivalent
system in Ada using object-oriented programming.

Pointer to subprograms
~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Design by components using dynamic libraries
--------------------------------------------

.. todo::

    Complete section!

