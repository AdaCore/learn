Handling Variability and Re-usability
=======================================

.. include:: ../../../global.txt

Understanding static and dynamic variability
--------------------------------------------

It is common to see embedded software being used in a variety of configurations
that require small changes to the code for each instance. For example, the same
application may need to be portable between two different architectures (ARM
and x86), or two different platforms with different set of devices available.
Maybe the same application is used for two different generations of the
product, so it needs to account for absence or presence of new features, or
it's used for different projects which may select different components or
configurations. All these cases, and many others, require variability in the
software in order to ensure its reusability.

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
as loss of efficiency |mdash| dispatching calls can't be inlined and require
one level  of indirection |mdash| or loss of analyzability |mdash| the target
of a dispatching call isn't known at run time. As a rule of thumb, OOP should
be considered only for cases of dynamic variability, where several versions of
the same object need to exist concurrently in the same application.

Handling variability & reusability statically
---------------------------------------------

.. _Ada_For_Embedded_C_Dev_Genericity:

Genericity
~~~~~~~~~~

One usage of C macros involves the creation of functions that works regardless
of the type they're being called upon. For example, a swap macro may look like:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Swap_C

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

        return 0;
    }

Ada offers a way to declare this kind of functions as a generic, that is, a
function that is written after static arguments, such as a parameter:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Swap_Ada

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

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Gen_Pkg_1

    generic
       type T is private;
    package Gen is
       type C is tagged record
          V : T;
       end record;

       G : Integer;
    end Gen;

The above can be instantiated and used the following way:

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Gen_Pkg_1

    with Gen;

    procedure Main is
       package I1 is new Gen (Integer);
       package I2 is new Gen (Integer);
       subtype Str10 is String (1 .. 10);
       package I3 is new Gen (Str10);
    begin
       I1.G := 0;
       I2.G := 1;
       I3.G := 2;
    end Main;

Here, :ada:`I1.G`, :ada:`I2.G` and :ada:`I3.G` are three distinct variables.

So far, we've only looked at generics with one kind of parameter: a so-called
private type. There's actually much more that can be described in this section,
such as variables, subprograms or package instantiations with certain
properties. For example, the following provides a sort algorithm for any kind
of structurally compatible array type:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Gen_Pkg_2

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

For a more complete list please reference the Generic Formal Types in the
:doc:`Appendix of the Introduction to Ada course </courses/intro-to-ada/chapters/appendices>`.

.. _Ada_For_Embedded_C_Dev_Simple_Derivation:

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

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

    package Drivers_1 is

       type Device_1 is null record;
       procedure Startup (Device : Device_1);
       procedure Send (Device : Device_1; Data : Integer);
       procedure Send_Fast (Device : Device_1; Data : Integer);
       procedure Receive (Device : Device_1; Data : out Integer);

    end Drivers_1;

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

    end Drivers_1;

In the above example, :ada:`Device_1` is an empty record type. It may also have
some fields if required, or be a different type such as a scalar. Then the four
procedures :ada:`Startup`, :ada:`Send`, :ada:`Send_Fast` and :ada:`Receive` are
primitives of this type. A primitive is essentially a subprogram that has a
parameter or return type directly referencing this type and declared in the
same scope. At this stage, there's nothing special with this type: we're using
it as we would use any other type. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

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

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

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

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

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

We can continue with this approach and introduce a new generation of devices.
This new device doesn't implement the :ada:`Send_Fast` service so we want
to remove it from the list of available services. Furthermore, for the purpose
of our example, let's assume that the hardware team went back to the
:ada:`Device_1` way of implementing :ada:`Startup`. We can write this new
device the following way:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

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

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

    package body Drivers_3 is

       overriding
       procedure Startup (Device : Device_3) is
       begin
          Drivers_1.Startup (Device_1 (Device));
       end Startup;

    end Drivers_3;

Our :ada:`Main` now looks like:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers
    :class: ada-expect-compile-error

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

Here, the call to :ada:`Send_Fast` will get flagged by the compiler.

Note that the fact that the code of :ada:`Main` has to be changed for every
implementation isn't necessarily satisfactory. We may want to go one step
further, and isolate the selection of the device kind to be used for the whole
application in one unique file. One way to do this is to use the same name for
all types, and use a renaming to select which package to use. Here's a
simplified example to illustrate that:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Derived_Drivers

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
of relying on the specific driver. Here, :ada:`Drivers` is another name for
:ada:`Driver_1`. In order to switch to :ada:`Driver_2`, the project only has to
replace that one :file:`drivers.ads` file.

In the following section, we'll go one step further and demonstrate that this
selection can be done through a configuration switch selected at build time
instead of a manual code modification.

Configuration pragma files
~~~~~~~~~~~~~~~~~~~~~~~~~~

Configuration pragmas are a set of pragmas that modify the compilation of
source-code files. You may use them to either relax or strengthen requirements.
For example:

.. code-block:: none

    pragma Suppress (Overflow_Check);

In this example, we're suppressing the overflow check, thereby relaxing a
requirement. Normally, the following program would raise a constraint error due
to a failed overflow check:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Constraint_Error_Detection
    :class: ada-run-expect-failure

    package P is
       function Add_Max (A : Integer) return Integer;
    end P;

    package body P is
       function Add_Max (A : Integer) return Integer is
       begin
          return A + Integer'Last;
       end Add_Max;
    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with P;           use P;

    procedure Main is
       I : Integer := Integer'Last;
    begin
       I := Add_Max (I);
       Put_Line ("I = " & Integer'Image (I));
    end Main;

When suppressing the overflow check, however, the program doesn't raise an
exception, and the value that :ada:`Add_Max` returns is ``-2``, which is a
wraparound of the sum of the maximum integer values
(:ada:`Integer'Last + Integer'Last`).

We could also strengthen requirements, as in this example:

.. code-block:: none

    pragma Restrictions (No_Floating_Point);

Here, the restriction forbids the use of floating-point types and objects. The
following program would violate this restriction, so the compiler isn't able to
compile the program when the restriction is used:

.. code-block:: ada

    procedure Main is
       F : Float := 0.0;
       --  Declaration is not possible with No_Floating_Point restriction.
    begin
       null;
    end Main;

Restrictions are especially useful for high-integrity applications. In fact,
the Ada Reference Manual has :arm:`a separate section for them <H-4>`.

When creating a project, it is practical to list all configuration pragmas in a
separate file. This is called a configuration pragma file, and it usually has
an `.adc` file extension. If you use :program:`GPRbuild` for building Ada
applications, you can specify the configuration pragma file in the
corresponding project file. For example, here we indicate that :file:`gnat.adc`
is the configuration pragma file for our project:

.. code-block:: none

    project Default is

       for Source_Dirs use ("src");
       for Object_Dir use "obj";
       for Main use ("main.adb");

       package Compiler is
          for Local_Configuration_Pragmas use "gnat.adc";
       end Compiler;

    end Default;

Configuration packages
~~~~~~~~~~~~~~~~~~~~~~

In C, preprocessing flags are used to create blocks of code that are only
compiled under certain circumstances. For example, we could have a block that
is only used for debugging:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Debug_Code_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    int func(int x)
    {
        return x % 4;
    }

    int main()
    {
        int a, b;

        a = 10;
        b = func(a);

    #ifdef DEBUG
        printf("func(%d) => %d\n", a, b);
    #endif

        return 0;
    }

Here, the block indicated by the :c:`DEBUG` flag is only included in the build
if we define this preprocessing flag, which is what we expect for a debug
version of the build. In the release version, however, we want to keep debug
information out of the build, so we don't use this flag during the build
process.

Ada doesn't define a preprocessor as part of the language. Some Ada toolchains
|mdash| like the GNAT toolchain |mdash| do have a preprocessor that could
create code similar to the one we've just seen. When programming in Ada,
however, the recommendation is to use configuration packages to select code
blocks that are meant to be included in the application.

When using a configuration package, the example above can be written as:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Debug_Code_Ada

    package Config is

       Debug : constant Boolean := False;

    end Config;

    function Func (X : Integer) return Integer;

    function Func (X : Integer) return Integer is
    begin
        return X mod 4;
    end Func;

    with Ada.Text_IO; use Ada.Text_IO;
    with Config;
    with Func;

    procedure Main is
       A, B : Integer;
    begin
       A := 10;
       B := Func (A);

       if Config.Debug then
          Put_Line ("Func(" & Integer'Image (A) & ") => "
                    & Integer'Image (B));
       end if;
    end Main;

In this example, :ada:`Config` is a configuration package. The version of
:ada:`Config` we're seeing here is the release version. The debug version of
the :ada:`Config` package looks like this:

.. code-block:: ada

    package Config is

       Debug : constant Boolean := True;

    end Config;

The compiler makes sure to remove dead code. In the case of the release
version, since :ada:`Config.Debug` is constant and set to :ada:`False`, the
compiler is smart enough to remove the call to :ada:`Put_Line` from the build.

As you can see, both versions of :ada:`Config` are very similar to each other.
The general idea is to create packages that declare the same constants, but
using different values.

In C, we differentiate between the debug and release versions by selecting
the appropriate preprocessing flags, but in Ada, we select the appropriate
configuration package during the build process. Since the file name is usually
the same (:file:`config.ads` for the example above), we may want to store them
in distinct directories. For the example above, we could have:

- :file:`src/debug/config.ads` for the debug version, and

- :file:`src/release/config.ads` for the release version.

Then, we simply select the appropriate configuration package for each version
of the build by indicating the correct path to it. When using
:program:`GPRbuild`, we can select the appropriate directory where the
:file:`config.ads` file is located. We can use scenario variables in our
project, which allow for creating different versions of a build. For example:

.. code-block:: none

    project Default is

       type Mode_Type is ("debug", "release");

       Mode : Mode_Type := external ("mode", "debug");

       for Source_Dirs use ("src", "src/" & Mode);
       for Object_Dir use "obj";
       for Main use ("main.adb");

    end Default;

In this example, we're defining a scenario type called ``Mode_Type``. Then,
we're declaring the scenario variable ``Mode`` and using it in the
``Source_Dirs`` declaration to complete the path to the subdirectory
containing the :file:`config.ads` file. The expression ``"src/" & Mode``
concatenates the user-specified mode to select the appropriate subdirectory.

We can then set the mode on the command-line. For example:

.. code-block:: sh

    gprbuild -P default.gpr -Xmode=release

In addition to selecting code blocks for the build, we could also specify
values that depend on the target build. For our example above, we may want to
create two versions of the application, each one having a different version of
a :c:`MOD_VALUE` that is used in the implementation of :c:`func()`. In C, we
can achieve this by using preprocessing flags and defining the corresponding
version in :c:`APP_VERSION`. Then, depending on the value of :c:`APP_VERSION`,
we define the corresponding value of :c:`MOD_VALUE`.

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.App_Version_C

    !defs.h
    #ifndef APP_VERSION
    #define APP_VERSION     1
    #endif

    #if APP_VERSION == 1
    #define MOD_VALUE       4
    #endif

    #if APP_VERSION == 2
    #define MOD_VALUE       5
    #endif

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    #include "defs.h"

    int func(int x)
    {
        return x % MOD_VALUE;
    }

    int main()
    {
        int a, b;

        a = 10;
        b = func(a);

        return 0;
    }

If not defined outside, the code above will compile version #1 of the
application. We can change this by specifying a value for :c:`APP_VERSION`
during the build (e.g. as a Makefile switch).

For the Ada version of this code, we can create two configuration packages for
each version of the application. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.App_Version_Ada

    --  ./src/app_1/app_defs.ads

    package App_Defs is

       Mod_Value : constant Integer := 4;

    end App_Defs;

    function Func (X : Integer) return Integer;

    with App_Defs;

    function Func (X : Integer) return Integer is
    begin
        return X mod App_Defs.Mod_Value;
    end Func;

    with Func;

    procedure Main is
       A, B : Integer;
    begin
       A := 10;
       B := Func (A);
    end Main;

The code above shows the version #1 of the configuration package. The
corresponding implementation for version #2 looks like this:

.. code-block:: ada

    --  ./src/app_2/app_defs.ads

    package App_Defs is

       Mod_Value : constant Integer := 5;

    end App_Defs;

Again, we just need to select the appropriate configuration package for each
version of the build, which we can easily do when using :program:`GPRbuild`.

Handling variability & reusability dynamically
----------------------------------------------

.. _Ada_For_Embedded_C_Dev_Records_With_Discriminants:

Records with discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~

In basic terms, records with discriminants are records that include
"parameters" in their type definitions. This allows for adding more flexibility
to the type definition. In the section about :ref:`pointers <Ada_For_Embedded_C_Dev_Pointers>`, we've
seen this example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Rec_Disc_Ada

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

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Rec_Disc_C

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

        return 0;
    }

Here, we need to explicitly allocate the :c:`a` array of the :c:`S` struct
via a call to :c:`malloc()`, which allocates memory space on the heap. In the
Ada version, in contrast, the array (:c:`V.A`) is allocated on the stack and
we don't need to explicitly allocate it.

Note that the information that we provide as the discriminant to the record
type (in the Ada code) is constant, so we cannot assign a value to it. For
example, we cannot write:

[Ada]

.. code-block:: ada

    V.Last := 10;       --  COMPILATION ERROR!

In the C version, we declare the :c:`last` field constant to get the same
behavior.

[C]

.. code-block:: c

    v.last = 10;        //  COMPILATION ERROR!

Note that the information provided as discriminants is visible. In the example
above, we could display :ada:`Last` by writing:

[Ada]

.. code-block:: ada

    Put_Line ("Last : " & Integer'Image (V.Last));

Also note that, even if a type is private, we can still access the information
of the discriminants if they are visible in the *public* part of the type
declaration. Let's rewrite the example above:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Rec_Disc_Ada_Private

    package Array_Definition is
       type Arr is array (Integer range <>) of Integer;

       type S (Last : Integer) is private;

    private
       type S (Last : Integer) is record
          A : Arr (0 .. Last);
       end record;

    end Array_Definition;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Array_Definition; use Array_Definition;

    procedure Main is
       V : S (9);
    begin
       Put_Line ("Last : " & Integer'Image (V.Last));
    end Main;

Even though the :ada:`S` type is now private, we can still display :ada:`Last`
because this discriminant is visible in the *non-private* part of package
:ada:`Array_Definition`.


Variant records
~~~~~~~~~~~~~~~

In simple terms, a variant record is a record with discriminants that allows
for changing its structure. Basically, it's a record containing a :ada:`case`.
This is the general structure:

[Ada]

.. code-block:: ada

       type Var_Rec (V : F) is record

          case V is
            when Opt_1 => F1 : Type_1;
            when Opt_2 => F2 : Type_2;
          end case;

       end record;

Let's look at this example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Var_Rec_Ada

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

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Var_Rec_C

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

        return 0;
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

Using this method prevents wrong information being used in other parts
of the program.

To get the same behavior in Ada as we do in C, we need to explicitly use the
:ada:`Unchecked_Union` aspect in the type declaration. This is the modified
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Unchecked_Union_Ada

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

Note that, when using the :ada:`Unchecked_Union` aspect in the declaration of a
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

Unchecked unions are particularly useful in Ada when creating bindings for C
code.

Optional components
^^^^^^^^^^^^^^^^^^^

We can also use variant records to specify optional components of a record.
For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Var_Rec_Null_Ada

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

We can use optional components to prevent subprograms from generating invalid
information that could be misused by the caller. Consider the following
example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Non_Opt_C

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

        return 0;
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

        return 0;
    }

We cannot prevent access to the returned value or, at least, force the caller
to evaluate :ada:`success` before using the returned value.

This is the corresponding code in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Non_Opt_Ada

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

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Opt_Ada

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

In the :ref:`previous section <Ada_For_Embedded_C_Dev_Records_With_Discriminants>`, we've seen that we
can add variability to records by using discriminants. Another approach is to
use *tagged* records, which are the base for object-oriented programming in
Ada.

Type extension
^^^^^^^^^^^^^^

A tagged record type is declared by adding the :ada:`tagged` keyword. For
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Tagged_Type_Decl

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

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Tagged_Type_Extension_Decl

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

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Tagged_Type_Extension_Decl

    package P is

       type Rec is record
          V : Integer;
       end record;

       procedure Display (R : Rec);
       procedure Reset (R : out Rec);

       type New_Rec is new Rec;

       overriding procedure Display (R : New_Rec);
       not overriding procedure New_Op (R : in out New_Rec);

       type Tagged_Rec is tagged record
          V : Integer;
       end record;

       procedure Display (R : Tagged_Rec);
       procedure Reset (R : out Tagged_Rec);

       type Ext_Tagged_Rec is new Tagged_Rec with record
          V2 : Integer;
       end record;

       overriding procedure Display (R : Ext_Tagged_Rec);
       overriding procedure Reset (R : out Ext_Tagged_Rec);
       not overriding procedure New_Op (R : in out Ext_Tagged_Rec);

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Display (R : Rec) is
       begin
          Put_Line ("TYPE: REC");
          Put_Line ("Rec.V = " & Integer'Image (R.V));
          New_Line;
       end Display;

       procedure Reset (R : out Rec) is
       begin
          R.V := 0;
       end Reset;

       procedure Display (R : New_Rec) is
       begin
          Put_Line ("TYPE: NEW_REC");
          Put_Line ("New_Rec.V = " & Integer'Image (R.V));
          New_Line;
       end Display;

       procedure New_Op (R : in out New_Rec) is
       begin
          R.V := R.V + 1;
       end New_Op;

       procedure Display (R : Tagged_Rec) is
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

       procedure Display (R : Ext_Tagged_Rec) is
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

[Ada]

.. code-block:: ada

       X_Tagged_Rec       : aliased Tagged_Rec;
       X_Ext_Tagged_Rec   : aliased Ext_Tagged_Rec;

       X_Tagged_Rec_Array : constant array (1 .. 2) of access Tagged_Rec'Class
                             := (X_Tagged_Rec'Access, X_Ext_Tagged_Rec'Access);

Here, we use the :ada:`aliased` keyword to be able to get access to the objects
(via the :ada:`'Access` attribute).

Then, we loop over this array and call the :ada:`Reset` and :ada:`Display`
procedures:

[Ada]

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

[Ada]

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

[Ada]

.. code-block:: ada

    type My_Derived is new My_Interface with null record;

    procedure Op (Obj : My_Derived);

Note that we're not using the :ada:`tagged` keyword in the declaration because
any type derived from an interface is automatically tagged.

Let's look at an example with an interface and two derived tagged types:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Interfaces_1

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
       end Dispatching_Display;

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

We may derive a type from multiple interfaces by simply writing
:ada:`type Derived_T is new T1 and T2 with null record`. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Multiple_Interfaces

    package Transceivers is

       type Send_Interface is interface;

       procedure Send (Obj : in out Send_Interface) is abstract;

       type Receive_Interface is interface;

       procedure Receive (Obj : in out Receive_Interface) is abstract;

       type Transceiver is new Send_Interface and Receive_Interface
         with null record;

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

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Multiple_Interfaces
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
is an abstract operation of :ada:`Abstract_Transceiver`. Since any tagged type
that has abstract operations is abstract, we must indicate this by adding the
:ada:`abstract` keyword in type declaration.

Also, when compiling this example, we get an error because we're trying to
declare an object of :ada:`Abstract_Transceiver` (in the :ada:`Main`
procedure), which is not possible. Naturally, if we derive another type from
:ada:`Abstract_Transceiver` and implement :ada:`Receive` as well, then we can
declare objects of this derived type. This is what we do in the
:ada:`Full_Transceivers` below:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Multiple_Interfaces

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

From simple derivation to OOP
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the :ref:`section about simple derivation <Ada_For_Embedded_C_Dev_Simple_Derivation>`, we've seen an
example where the actual selection was done at *implementation* time by
renaming one of the packages:

[Ada]

.. code-block:: ada

    with Drivers_1;

    package Drivers renames Drivers_1;

Although this approach is useful in many cases, there might be situations where
we need to select the actual driver dynamically at runtime. Let's look at how
we could rewrite that example using interfaces, tagged types and dispatching
calls:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Tagged_Drivers

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
          Data := 7;
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
:ref:`hands-on overview of object-oriented programming <Ada_For_Embedded_C_Dev_Hands_On_OOP>` that
discusses how to translate a simple system written in C to an equivalent
system in Ada using object-oriented programming.

Pointer to subprograms
~~~~~~~~~~~~~~~~~~~~~~

Pointers to subprograms allow us to dynamically select an appropriate
subprogram at runtime. This selection might be triggered by an external
event, or simply by the user. This can be useful when multiple versions of a
routine exist, and the decision about which one to use cannot be made at
compilation time.

This is an example on how to declare and use pointers to functions in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Selecting_Subprogram_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    void show_msg_v1 (char *msg)
    {
        printf("Using version #1: %s\n", msg);
    }

    void show_msg_v2 (char *msg)
    {
        printf("Using version #2:\n %s\n", msg);
    }

    int main()
    {
        int selection = 1;
        void (*current_show_msg) (char *);

        switch (selection)
        {
            case 1:  current_show_msg = &show_msg_v1;    break;
            case 2:  current_show_msg = &show_msg_v2;    break;
            default: current_show_msg = NULL;            break;
        }

        if (current_show_msg != NULL)
        {
            current_show_msg ("Hello there!");
        }
        else
        {
            printf("ERROR: no version of show_msg() selected!\n");
        }

        return 0;
    }

The example above contains two versions of the :c:`show_msg()` function:
:c:`show_msg_v1()` and :c:`show_msg_v2()`. The function is selected depending
on the value of :c:`selection`, which initializes the function pointer
:c:`current_show_msg`. If there's no corresponding value, :c:`current_show_msg`
is set to :c:`null` |mdash| alternatively, we could have selected a default
version of :c:`show_msg()` function. By calling
:c:`current_show_msg ("Hello there!")`, we're calling the function that
:c:`current_show_msg` is pointing to.

This is the corresponding implementation in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Selecting_Subprogram_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Subprogram_Selection is

       procedure Show_Msg_V1 (Msg : String) is
       begin
          Put_Line ("Using version #1: " & Msg);
       end Show_Msg_V1;

       procedure Show_Msg_V2 (Msg : String) is
       begin
          Put_Line ("Using version #2: ");
          Put_Line (Msg);
       end Show_Msg_V2;

       type Show_Msg_Proc is access procedure (Msg : String);

       Current_Show_Msg : Show_Msg_Proc;
       Selection        : Natural;

    begin
       Selection := 1;

       case Selection is
          when 1      => Current_Show_Msg := Show_Msg_V1'Access;
          when 2      => Current_Show_Msg := Show_Msg_V2'Access;
          when others => Current_Show_Msg := null;
       end case;

       if Current_Show_Msg /= null then
          Current_Show_Msg ("Hello there!");
       else
          Put_Line ("ERROR: no version of Show_Msg selected!");
       end if;

    end Show_Subprogram_Selection;

The structure of the code above is very similar to the one used in the C code.
Again, we have two version of :ada:`Show_Msg`: :ada:`Show_Msg_V1` and
:ada:`Show_Msg_V2`. We set :ada:`Current_Show_Msg` according to the value of
:ada:`Selection`. Here, we use :ada:`'Access` to get access to the
corresponding procedure. If no version of :ada:`Show_Msg` is available, we set
:ada:`Current_Show_Msg` to :ada:`null`.

Pointers to subprograms are also typically used as callback functions. This
approach is extensively used in systems that process events, for example. Here,
we could have a two-layered system:

- A layer of the system (an event manager) triggers events depending on
  information from sensors.

    - For each event, callback functions can be registered.

    - The event manager calls registered callback functions when an event is
      triggered.

- Another layer of the system registers callback functions for specific events
  and decides what to do when those events are triggered.

This approach promotes information hiding and component decoupling because:

- the layer of the system responsible for managing events doesn't need to know
  what the callback function actually does, while

- the layer of the system that implements callback functions remains
  agnostic to implementation details of the event manager |mdash| for example,
  how events are implemented in the event manager.

Let's see an example in C where we have a :c:`process_values()` function that
calls a callback function (:c:`process_one`) to process a list of values:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Callback_C

    !process_values.h
    typedef int (*process_one_callback) (int);

    void process_values (int                  *values,
                         int                   len,
                         process_one_callback  process_one);

    !process_values.c
    #include "process_values.h"

    #include <assert.h>
    #include <stdio.h>

    void process_values (int                  *values,
                         int                   len,
                         process_one_callback  process_one)
    {
        int i;

        assert (process_one != NULL);

        for (i = 0; i < len; i++)
        {
            values[i] = process_one (values[i]);
        }
    }

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    #include "process_values.h"

    int proc_10 (int val)
    {
        return val + 10;
    }

    # define LEN_VALUES     5

    int main()
    {

        int values[LEN_VALUES] = { 1, 2, 3, 4, 5 };
        int i;

        process_values (values, LEN_VALUES, &proc_10);

        for (i = 0; i < LEN_VALUES; i++)
        {
            printf("Value [%d] = %d\n", i, values[i]);
        }

        return 0;
    }

As mentioned previously, :c:`process_values()` doesn't have any knowledge about
what :c:`process_one()` does with the integer value it receives as a parameter.
Also, we could replace :c:`proc_10()` by another function without having to
change the implementation of :c:`process_values()`.

Note that :c:`process_values()` calls an :c:`assert()` for the function
pointer to compare it against :c:`null`. Here, instead of checking the validity
of the function pointer, we're expecting the caller of :c:`process_values()`
to provide a valid pointer.

This is the corresponding implementation in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.Callback_Ada

    package Values_Processing is

       type Integer_Array is array (Positive range <>) of Integer;

       type Process_One_Callback is not null access
         function (Value : Integer) return Integer;

       procedure Process_Values (Values      : in out Integer_Array;
                                 Process_One :        Process_One_Callback);

    end Values_Processing;

    package body Values_Processing is

       procedure Process_Values (Values      : in out Integer_Array;
                                 Process_One :        Process_One_Callback) is
       begin
          for I in Values'Range loop
             Values (I) := Process_One (Values (I));
          end loop;
       end Process_Values;

    end Values_Processing;

    function Proc_10 (Value : Integer) return Integer;

    function Proc_10 (Value : Integer) return Integer is
    begin
       return Value + 10;
    end Proc_10;

    with Ada.Text_IO; use Ada.Text_IO;

    with Values_Processing; use Values_Processing;
    with Proc_10;

    procedure Show_Callback is
       Values : Integer_Array := (1, 2, 3, 4, 5);
    begin
       Process_Values (Values, Proc_10'Access);

       for I in Values'Range loop
          Put_Line ("Value ["
                    & Positive'Image (I)
                    & "] = "
                    & Integer'Image (Values (I)));
       end loop;
    end Show_Callback;

Similar to the implementation in C, the :ada:`Process_Values` procedure
receives the access to a callback routine, which is then called for each value
of the :ada:`Values` array.

Note that the declaration of :ada:`Process_One_Callback` makes use of the
:ada:`not null access` declaration. By using this approach, we ensure that
any parameter of this type has a valid value, so we can always call the
callback routine.

Design by components using dynamic libraries
--------------------------------------------

In the previous sections, we have shown how to use packages to create separate
components of a system. As we know, when designing a complex system, it is
advisable to separate concerns into distinct units, so we can use Ada packages
to represent each unit of a system. In this section, we go one step further and
create separate dynamic libraries for each component, which we'll then link to
the main application.

Let's suppose we have a main system (:ada:`Main_System`) and a
component *A* (:ada:`Component_A`) that we want to use in the main system. For
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Reusability.System_For_Dyn_Lib

    --
    --  File: component_a.ads
    --
    package Component_A is

       type Float_Array is array (Positive range <>) of Float;

       function Average (Data : Float_Array) return Float;

    end Component_A;

    --
    --  File: component_a.adb
    --
    package body Component_A is

       function Average (Data : Float_Array) return Float is
          Total : Float := 0.0;
       begin
          for Value of Data loop
             Total := Total + Value;
          end loop;
          return Total / Float (Data'Length);
       end Average;

    end Component_A;

    --
    --  File: main_system.adb
    --
    with Ada.Text_IO; use Ada.Text_IO;

    with Component_A; use Component_A;

    procedure Main_System is
       Values        : constant Float_Array := (10.0, 11.0, 12.0, 13.0);
       Average_Value : Float;
    begin
       Average_Value := Average (Values);
       Put_Line ("Average = " & Float'Image (Average_Value));
    end Main_System;

Note that, in the source-code example above, we're indicating the name of each
file. We'll now see how to organize those files in a structure that is suitable
for the GNAT build system (:program:`GPRbuild`).

In order to discuss how to create dynamic libraries, we need to dig into some
details about the build system. With GNAT, we can use project files for
:program:`GPRbuild` to easily design dynamic libraries. Let's say we use the
following directory structure for the code above:

.. code-block:: none

    |- component_a
    |    | component_a.gpr
    |    |- src
    |    |    | component_a.adb
    |    |    | component_a.ads
    |- main_system
    |    | main_system.gpr
    |    |- src
    |    |    | main_system.adb

Here, we have two directories: `component_a` and `main_system`. Each directory
contains a project file (with the `.gpr` file extension) and a source-code
directory (`src`).

In the source-code example above, we've seen the content of files
:file:`component_a.ads`, :file:`component_a.adb` and :file:`main_system.adb`.
Now, let's discuss how to write the project file for :ada:`Component_A`
(:file:`component_a.gpr`), which will build the dynamic library for this
component:

.. code-block:: none

    library project Component_A is

       for Source_Dirs use ("src");
       for Object_Dir use "obj";
       for Create_Missing_Dirs use "True";
       for Library_Name use "component_a";
       for Library_Kind use "dynamic";
       for Library_Dir use "lib";

    end Component_A;

The project is defined as a `library project` instead of `project`. This tells
:program:`GPRbuild` to build a library instead of an executable binary. We then
specify the library name using the `Library_Name` attribute, which is required,
so it must appear in a library project. The next two library-related attributes
are optional, but important for our use-case. We use:

- `Library_Kind` to specify that we want to create a dynamic library |mdash|
  by default, this attribute is set to `static`;

- `Library_Dir` to specify the directory where the library is stored.

In the project file of our main system (:file:`main_system.gpr`), we just need
to reference the project of :ada:`Component_A` using a `with` clause and
indicating the correct path to that project file:

.. code-block:: none

    with "../component_a/component_a.gpr";

    project Main_System is
        for Source_Dirs use ("src");
        for Object_Dir use "obj";
        for Create_Missing_Dirs use "True";
        for Main use ("main_system.adb");
    end Main_System;

:program:`GPRbuild` takes care of selecting the correct settings to link the
dynamic library created for :ada:`Component_A` with the main application
(:ada:`Main_System`) and build an executable.

We can use the same strategy to create a :ada:`Component_B` and dynamically
link to it in the :ada:`Main_System`. We just need to create the separate
structure for this component |mdash| with the appropriate Ada packages and
project file |mdash| and include it in the project file of the main system
using a `with` clause:

.. code-block:: none

    with "../component_a/component_a.gpr";
    with "../component_b/component_b.gpr";

    ...

Again, :program:`GPRbuild` takes care of selecting the correct settings to link
both dynamic libraries together with the main application.

You can find more details and special setting for library projects in the
`GPRbuild documentation <https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html#library-projects>`_.

.. admonition:: In the GNAT toolchain

    The GNAT toolchain includes a more advanced example focusing on how to load
    dynamic libraries at runtime. You can find it in the
    :file:`share/examples/gnat/plugins` directory of the GNAT toolchain
    installation. As described in the README file from that directory, this
    example "comprises a main program which probes regularly for the existence
    of shared libraries in a known location. If such libraries are present, it
    uses them to implement features initially not present in the main program."
