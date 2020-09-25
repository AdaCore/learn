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

.. todo::

    Complete section!


Object orientation
~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Pointer to subprograms
~~~~~~~~~~~~~~~~~~~~~~

Pointers to subprograms allow us to dynamically select an appropriate
subprogram at runtime. This selection might be triggered by an external
event, or simply by the user. This can be useful when multiple versions of a
routine exist, and the decision about which one to use cannot be made at
compilation time.

This is an example on how to declare and use pointers to functions in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Selecting_Subprogram_C

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Selecting_Subprogram_Ada

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

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Callback_C

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Reusability.Callback_Ada

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

.. todo::

    Complete section!

