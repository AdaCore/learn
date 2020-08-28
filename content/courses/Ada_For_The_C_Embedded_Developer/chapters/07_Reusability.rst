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
       begin
          Data := 42;
       end Receive;

    end Drivers_1;

    with Drivers_1;

    package Drivers_2 is

       type Device_Type is new Device_1.Device_Type;
       procedure Send (Device : Device_Type; Data : Integer);
       procedure Receive (Device : Device_Type; Data : out Integer);

    end Drivers_2;

    package body Drivers_2 is

       procedure Send (Device : Device_Type; Data : Integer) is null;

       procedure Receive (Device : Device_Type; Data : out Integer) is
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

In basic terms, records with discriminants are records that include parameters
in its type definition. This allows for adding more flexibility to the type
definition. In the section about :ref:`pointers <Pointers>`, we've seen this
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Rec_Disc_Ada

    procedure Main is
       type Arr is array (Integer range <>) of Integer;

       type S (Last : Positive) is record
          A : Arr (0 .. Last);
       end record;

       V : S (9);
    begin
       null;
    end Main;

Here, :ada:`Last` is the discriminant for type :ada:`S`. When declaring the
variable :ada:`V` as :ada:`S (9)`, we specify the actual index of the last
position of the array component :ada:`A` by setting the :ada:`Last`
discriminant to 9.

We can create an equivalent implementation in C by declaring a :c:`struct`
with a pointer to an array:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Rec_Disc_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    typedef struct {
        int       * a;
        const int   last;
    } S;

    S init_s (int last)
    {
        S v = { malloc (sizeof(int) * last + 1), last };
        return v;
    }

    int main(int argc, const char * argv[])
    {
        S v = init_s (9);
    }

Here, we need to explicitly allocate the :ada:`a` array of the :ada:`S` struct
via a call to :ada:`malloc()`, which allocates memory space on the heap. In the
Ada version, in contrast, the array (:ada:`V.A`) is allocated on the stack and
we don't need to explicitly allocate it.

Note that the information that we provide as the discriminant to the record
type (in the Ada code) is constant, so we cannot assign a value to it. For
example, we cannot write:

.. code-block:: ada

    V.Last := 10;       --  COMPILATION ERROR!

In the C version, we declare the :c:`last` field constant to get the same
behavior.

.. code-block:: c

    v.last = 10;        //  COMPILATION ERROR!

Note that the information provided as discriminants is visible. In the example
above, we could display :ada:`Last` by writing:

.. code-block:: ada

    Put_Line ("Last : " & Integer'Image (V.Last));

Also note that, even if a type is private, we can still access the information
of the discriminants if they are visible in the *public* part of the type
declaration. Let's rewrite the example above:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Rec_Disc_Ada_Private

    package P is
       type Arr is array (Integer range <>) of Integer;

       type S (Last : Integer) is private;

    private
       type S (Last : Integer) is record
          A : Arr (0 .. Last);
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with P;           use P;

    procedure Main is
       V : S (9);
    begin
       Put_Line ("Last : " & Integer'Image (V.Last));
    end Main;

Even though the :ada:`S` type is now private, we can still display :ada:`Last`
because this discriminant is visible in the *non-private* part of package
:ada:`P`.


Variant records
~~~~~~~~~~~~~~~

In simple terms, a variant record is a record with discriminants that allows
for changing its structure. Basically, it's a record containing a :ada:`case`.
This is the general structure:

.. code-block:: ada

       type Var_Rec (V : F) is record

          case V is
            when Opt_1 => F1 : Type_1;
            when Opt_2 => F2 : Type_2;
          end case;

       end record;

Let's look at this example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Var_Rec_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Float_Int (Use_Float : Boolean) is record
          case Use_Float is
            when True  => F : Float;
            when False => I : Integer;
          end case;
       end record;

       procedure Display (V : Float_Int) is
       begin
          if V.Use_Float then
             Put_Line ("Float value:   " & Float'Image (V.F));
          else
             Put_Line ("Integer value: " & Integer'Image (V.I));
          end if;
       end Display;

       F : constant Float_Int := (Use_Float => True,  F => 10.0);
       I : constant Float_Int := (Use_Float => False, I => 9);

    begin
       Display (F);
       Display (I);
    end Main;

Here, we declare :ada:`F` containing a floating-point value, and :ada:`I`
containing an integer value. In the :ada:`Display` procedure, we present the
correct information to the user according to the :ada:`Use_Float` discriminant
of the :ada:`Float_Int` type.

We can implement this example in C by using unions:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Var_Rec_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    typedef struct {
        int use_float;
        union {
            float f;
            int   i;
        };
    } float_int;

    float_int init_float (float f)
    {
        float_int v;

        v.use_float = 1;
        v.f         = f;
        return v;
    }

    float_int init_int (int i)
    {
        float_int v;

        v.use_float = 0;
        v.i         = i;
        return v;
    }

    void display (float_int v)
    {
        if (v.use_float) {
            printf("Float value   : %f\n", v.f);
        }
        else {
            printf("Integer value : %d\n", v.i);
        }
    }

    int main(int argc, const char * argv[])
    {
        float_int f = init_float (10.0);
        float_int i = init_int (9);

        display (f);
        display (i);
    }

Similar to the Ada code, we declare :ada:`f` containing a floating-point value,
and :ada:`i` containing an integer value. One difference is that we use the
:c:`init_float()` and :c:`init_int()` functions to initialize the
:c:`float_int` struct. These functions initialize the correct field of the
union and set the :c:`use_float` field accordingly.

Variant records and unions
^^^^^^^^^^^^^^^^^^^^^^^^^^

There is, however, a difference in accessibility between variant records in Ada
and unions in C. In C, we're allowed to access any field of the union
regardless of the initialization:

[C]

.. code-block:: c

        float_int v = init_float (10.0);

        printf("Integer value : %d\n", v.i);

This feature is useful to create overlays. In this specific example, however,
the information displayed to the user doesn't make sense, since the union was
initialized with a floating-point value (:c:`v.f`) and, by accessing the
integer field (:c:`v.i`), we're displaying it as if it was an integer value.

In Ada, accessing the wrong component would raise an exception at run-time
("discriminant check failed"), since the component is checked before being
accessed:

[Ada]

.. code-block:: ada

       V : constant Float_Int := (Use_Float => True,  F => 10.0);
    begin
       Put_Line ("Integer value: " & Integer'Image (V.I));
       --                                             ^ Constraint_Error is raised!

Using this method, Ada prevents that wrong information is used in other parts
of the program.

To get the same behavior in Ada as we do in C, we need to explicitly use the
:ada:`Unchecked_Union` aspect in the type declaration. This is the modified
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Unchecked_Union_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Float_Int_Union (Use_Float : Boolean) is record
          case Use_Float is
            when True  => F : Float;
            when False => I : Integer;
          end case;
       end record
         with Unchecked_Union;

       V : constant Float_Int_Union := (Use_Float => True,  F => 10.0);

    begin
       Put_Line ("Integer value: " & Integer'Image (V.I));
    end Main;

Now, we can display the integer component (:ada:`V.I`) even though we
initialized the floating-point component (:ada:`V.F`). As expected, the
information displayed by the test application in this case doesn't make sense.

Note that, when using :ada:`Unchecked_Union` aspect in the declaration of a
variant record, the reference discriminant is not available anymore, since it
isn't stored as part of the record. Therefore, we cannot access the
:ada:`Use_Float` discriminant as in the following code:

[Ada]

.. code-block:: ada

       V : constant Float_Int_Union := (Use_Float => True,  F => 10.0);
    begin
       if V.Use_Float then       --  COMPILATION ERROR!
          --  Do something...
       end if;

We could, however, declare another record with discriminants and use the
:ada:`Float_Int_Union` type for one of its components. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Unchecked_Union_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Float_Int_Union (Use_Float : Boolean) is record
          case Use_Float is
            when True  => F : Float;
            when False => I : Integer;
          end case;
       end record
         with Unchecked_Union;

       type Float_Int (Use_Float : Boolean) is record
          U : Float_Int_Union (Use_Float);
       end record;

       V : constant Float_Int := (Use_Float => True,
                                  U         => (Use_Float => True,  F => 10.0));

    begin
       Put_Line ("Using float:   " & Boolean'Image (V.Use_Float));
       Put_Line ("Integer value: " & Integer'Image (V.U.I));
    end Main;

Unchecked unions are particularly useful in Ada when creating bindings for C
code.

Optional components
^^^^^^^^^^^^^^^^^^^

We can also use variant records to specify optional components of a record.
For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Var_Rec_Null_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr is array (Integer range <>) of Integer;

       type Extra_Info is (No, Yes);

       type S_Var (Last : Integer; Has_Extra_Info : Extra_Info) is record
          A : Arr (0 .. Last);

          case Has_Extra_Info is
            when No   => null;
            when Yes  => B : Arr (0 .. Last);
          end case;
       end record;

       V1 : S_Var (Last => 9, Has_Extra_Info => Yes);
       V2 : S_Var (Last => 9, Has_Extra_Info => No);
    begin
       Put_Line ("Size of V1 is: " & Integer'Image (V1'Size));
       Put_Line ("Size of V2 is: " & Integer'Image (V2'Size));
    end Main;

Here, in the declaration of :ada:`S_Var`, we don't have any component in case
:ada:`Has_Extra_Info` is false. The component is simply set to :ada:`null` in
this case.

When running the example above, we see that the size of :ada:`V1` is greater
than the size of :ada:`V2` due to the extra :ada:`B` component |mdash| which is
only included when :ada:`Has_Extra_Info` is true.

Optional output information
^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can use optional components to prevent that subprograms generate invalid
information that could be misused by the caller. Consider the following
example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Non_Opt_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    float calculate (float  f1,
                     float  f2,
                     int   *success)
    {
        if (f1 < f2) {
            *success = 1;
            return f2 - f1;
        }
        else {
            *success = 0;
            return 0.0;
        }
    }

    void display (float v,
                  int   success)
    {
        if (success) {
            printf("Value = %f\n", v);
        }
        else {
            printf("Calculation error!\n");
        }
    }

    int main(int argc, const char * argv[])
    {
        float f;
        int success;

        f = calculate (1.0, 0.5, &success);
        display (f, success);

        f = calculate (0.5, 1.0, &success);
        display (f, success);
    }

In this code, we're using the output parameter :ada:`success` of the
:ada:`calculate()` function to indicate whether the calculation was successful
or not. This approach has a major problem: there's no way to prevent that the
invalid value returned by :ada:`calculate()` in case of an error is misused in
another computation. For example:

[C]

.. code-block:: c

    int main(int argc, const char * argv[])
    {
        float f;
        int success;

        f = calculate (1.0, 0.5, &success);

        f = f * 0.25;   // Using f in another computation even though
                        // calculate() returned a dummy value due to error!
                        // We should have evaluated "success", but we didn't.
    }

We cannot prevent access to the returned value or, at least, force the caller
to evaluate :ada:`success` before using the returned value.

This is the corresponding code in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Non_Opt_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       function Calculate (F1, F2  : Float;
                           Success : out Boolean) return Float is
       begin
          if F1 < F2 then
             Success := True;
             return F2 - F1;
          else
            Success := False;
            return 0.0;
          end if;
       end Calculate;

       procedure Display (V : Float; Success : Boolean) is
       begin
          if Success then
             Put_Line ("Value = " & Float'Image (V));
          else
             Put_Line ("Calculation error!");
          end if;
       end Display;

       F       : Float;
       Success : Boolean;
    begin
       F := Calculate (1.0, 0.5, Success);
       Display (F, Success);

       F := Calculate (0.5, 1.0, Success);
       Display (F, Success);
    end Main;

The Ada code above suffers from the same drawbacks as the C code. Again,
there's no way to prevent misuse of the invalid value returned by
:ada:`Calculate` in case of errors.

However, in Ada, we can use variant records to make the component unavailable
and therefore prevent misuse of this information. Let's rewrite the original
example and *wrap* the returned value in a variant record:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Opt_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Opt_Float (Success : Boolean) is record
          case Success is
            when False => null;
            when True  => F : Float;
          end case;
       end record;

       function Calculate (F1, F2 : Float) return Opt_Float is
       begin
          if F1 < F2 then
             return (Success => True, F => F2 - F1);
          else
             return (Success => False);
          end if;
       end Calculate;

       procedure Display (V : Opt_Float) is
       begin
          if V.Success then
             Put_Line ("Value = " & Float'Image (V.F));
          else
             Put_Line ("Calculation error!");
          end if;
       end Display;

    begin
       Display (Calculate (1.0, 0.5));
       Display (Calculate (0.5, 1.0));
    end Main;

In this example, we can determine whether the calculation was successful or not
by evaluating the :ada:`Success` component of the :ada:`Opt_Float`. If the
calculation wasn't successful, we won't be able to access the :ada:`F`
component of the :ada:`Opt_Float`. As mentioned before, trying to access the
component in this case would raise an exception. Therefore, in case of errors,
we can ensure that no information is misused after the call to
:ada:`Calculate`.


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

