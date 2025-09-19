C to Ada Translation Patterns
=================================

.. include:: ../../../global.txt

Naming conventions and casing considerations
--------------------------------------------

One question that may arise relatively soon when converting from C to
Ada is the style of source code presentation. The Ada language doesn't
impose any particular style and for many reasons, it may seem attractive
to keep a C-like style |mdash| for example, camel casing |mdash| to the
Ada program.

However, the code in the Ada language standard, most third-party code,
and the libraries provided by GNAT follow a specific style for
identifiers and reserved words. Using a different style for the rest of
the program leads to inconsistencies, thereby decreasing readability and
confusing automatic style checkers. For those reasons, it's usually
advisable to adopt the Ada style |mdash| in which each identifier starts
with an upper case letter, followed by lower case letters (or digits),
with an underscore separating two "distinct" words within the
identifier. Acronyms within identifiers are in upper case. For example,
there is a language-defined package named :ada:`Ada.Text_IO`. Reserved words
are all lower case.

Following this scheme doesn't preclude adding additional,
project-specific rules.


Manually interfacing C and Ada
------------------------------

Before even considering translating code from C to Ada, it's worthwhile to
evaluate the possibility of keeping a portion of the C code intact, and only
translating selected modules to Ada. This is a necessary evil when introducing
Ada to an existing large C codebase, where re-writing the entire code upfront
is not practical nor cost-effective.

Fortunately, Ada has a dedicated set of features for interfacing with other
languages. The :ada:`Interfaces` package hierarchy and the pragmas
:ada:`Convention`, :ada:`Import`, and :ada:`Export` allow you to make
inter-language calls while observing proper data representation for each
language.

Let's start with the following C code:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.My_Struct_C

    !call.c
    #include <stdio.h>

    struct my_struct {
        int A, B;
    };

    void call (struct my_struct *p) {
        printf ("%d", p->A);
    }

To call that function from Ada, the Ada compiler requires a description of the
data structure to pass as well as a description of the function itself. To
capture how the C :c:`struct my_struct` is represented, we can use the
following record along with a :ada:`pragma Convention`. The pragma directs the
compiler to lay out the data in memory the way a C compiler would.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.My_Struct_Ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Interfaces.C;

    procedure Use_My_Struct is

       type my_struct is record
         A : Interfaces.C.int;
         B : Interfaces.C.int;
       end record;
       pragma Convention (C, my_struct);

       V : my_struct := (A => 1, B => 2);
    begin
       Put_Line ("V = ("
                 & Interfaces.C.int'Image (V.A)
                 & Interfaces.C.int'Image (V.B)
                 & ")");
    end Use_My_Struct;

Describing a foreign subprogram call to Ada code is called *binding* and it is
performed in two stages. First, an Ada subprogram specification equivalent to
the C function is coded. A C function returning a value maps to an Ada
function, and a void function maps to an Ada procedure. Then, rather than
implementing the subprogram using Ada code, we use a :ada:`pragma Import`:

.. code-block:: ada

    procedure Call (V : my_struct);
    pragma Import (C, Call, "call"); -- Third argument optional

The :ada:`Import` pragma specifies that whenever :ada:`Call` is invoked by Ada
code, it should invoke the :ada:`Call` function with the C calling convention.

And that's all that's necessary. Here's an example of a call to :ada:`Call`:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.My_Struct

    with Interfaces.C;

    procedure Use_My_Struct is

       type my_struct is record
         A : Interfaces.C.int;
         B : Interfaces.C.int;
       end record;
       pragma Convention (C, my_struct);

       procedure Call (V : my_struct);
       pragma Import (C, Call, "call"); -- Third argument optional

       V : my_struct := (A => 1, B => 2);
    begin
       Call (V);
    end Use_My_Struct;

Building and Debugging mixed language code
------------------------------------------

The easiest way to build an application using mixed C / Ada code is to create
a simple project file for :program:`gprbuild` and specify C as an additional
language. By default, when using :program:`gprbuild` we only compile Ada source
files. To compile C code files as well, we use the ``Languages`` attribute and
specify ``c`` as an option, as in the following example of a project file named
`default.gpr`:

.. code-block:: ada

    project Default is

       for Languages use ("ada", "c");
       for Main use ("main.adb");

    end Default;

Then, we use this project file to build the application by simply calling
``gprbuild``. Alternatively, we can specify the project file on the
command-line with the ``-P`` option |mdash| for example,
``gprbuild -P default.gpr``. In both cases, ``gprbuild`` compiles all C
source-code file found in the directory and links the corresponding object
files to build the executable.

In order to include debug information, you can use ``gprbuild -cargs -g``. This
option adds debug information based on both C and Ada code to the executable.
Alternatively, you can specify a ``Builder`` package in the project file and
include global compilation switches for each language using the
``Global_Compilation_Switches`` attribute. For example:

.. code-block:: ada

    project Default is

       for Languages use ("ada", "c");
       for Main use ("main.adb");

       package Builder is
          for Global_Compilation_Switches ("Ada") use ("-g");
          for Global_Compilation_Switches ("C") use ("-g");
       end Builder;

    end Default;

In this case, you can simply run ``gprbuild -P default.gpr`` to build the
executable.

To debug the executable, you can use programs such as :program:`gdb` or
:program:`ddd`, which are suitable for debugging both C and Ada source-code. If
you prefer a complete IDE, you may want to look into :program:`GNAT Studio`,
which supports building and debugging an application within a single
environment, and remotely running applications loaded to various embedded
devices. You can find more information about :program:`gprbuild` and
:program:`GNAT Studio` in the
:doc:`Introduction to GNAT Toolchain </courses/GNAT_Toolchain_Intro/index>`
course.

Automatic interfacing
---------------------

It may be useful to start interfacing Ada and C by using automatic binding
generators. These can be done either by invoking :program:`gcc`
``-fdump-ada-spec`` option (to generate an Ada binding to a C header file) or
``-gnatceg`` option (to generate a C binding to an Ada specification file). For
example:

::

    gcc -c -fdump-ada-spec my_header.h
    gcc -c -gnatceg spec.ads

The level of interfacing is very low level and typically requires either
massaging (changing the generated files) or wrapping (calling the generated
files from a higher level interface). For example, numbers bound from C to Ada
are only standard numbers where user-defined types may be desirable. C uses a
lot of by-pointer parameters which may be better replaced by other parameter
modes, etc.

However, the automatic binding generator helps having a starting point which
ensures compatibility of the Ada and the C code.

Using Arrays in C interfaces
----------------------------

It is relatively straightforward to pass an array from Ada to C. In particular,
with the GNAT compiler, passing an array is equivalent to passing a pointer to
its first element. Of course, as there's no notion of boundaries in C, the
length of the array needs to be passed explicitly. For example:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_1

    !p.h
    void p (int * a, int length);

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_1

    procedure Main is
       type Arr is array (Integer range <>) of Integer;

       procedure P (V : Arr; Length : Integer);
       pragma Import (C, P);

       X : Arr (5 .. 15);
    begin
       P (X, X'Length);
    end Main;

The other way around |mdash| that is, retrieving an array that has been
creating on the C side |mdash| is more difficult. Because C doesn't explicitly
carry boundaries, they need to be recreated in some way.

The first option is to actually create an Ada array without boundaries. This is
the most flexible, but also the least safe option. It involves creating an
array with indices over the full range of :ada:`Integer` without ever creating
it from Ada, but instead retrieving it as an access from C. For example:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_2

    !f.h
    int * f ();

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_2

    procedure Main is
       type Arr is array (Integer) of Integer;
       type Arr_A is access all Arr;

       function F return Arr_A;
       pragma Import (C, F);
    begin
       null;
    end Main;

Note that :ada:`Arr` is a constrained type (it doesn't have the :ada:`range <>`
notation for indices). For that reason, as it would be for C, it's possible to
iterate over the whole range of integer, beyond the memory actually allocated
for the array.

A somewhat safer way is to overlay an Ada array over the C one. This requires
having access to the length of the array. This time, let's consider two cases,
one with an array and its size accessible through functions, another one on
global variables. This time, as we're using an overlay, the function will be
directly mapped to an Ada function returning an address:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_3

    !fg.h
    int * f_arr (void);
    int f_size (void);

    int * g_arr;
    int g_size;

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Arr_3

    with System;

    package Fg is

       type Arr is array (Integer range <>) of Integer;

       function F_Arr return System.Address;
       pragma Import (C, F_Arr, "f_arr");

       function F_Size return Integer;
       pragma Import (C, F_Size, "f_size");

       F : Arr (0 .. F_Size - 1) with Address => F_Arr;

       G_Size : Integer;
       pragma Import (C, G_Size, "g_size");

       G_Arr : Arr (0 .. G_Size - 1);
       pragma Import (C, G_Arr, "g_arr");

    end Fg;

    with Fg;

    procedure Main is
    begin
       null;
    end Main;

With all solutions though, importing an array from C is a relatively unsafe
pattern, as there's only so much information on the array as there would be on
the C side in the first place. These are good places for careful peer reviews.

.. _Ada_For_Embedded_C_Dev_By_Value_Vs_By_Reference:

By-value vs. by-reference types
-------------------------------

When interfacing Ada and C, the rules of parameter passing are a bit different
with regards to what's a reference and what's a copy. Scalar types and pointers
are passed by value, whereas record and arrays are (almost) always passed by
reference. However, there may be cases where the C interface also passes values
and not pointers to objects. Here's a slightly modified version of a previous
example to illustrate this point:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Param_By_Value_C

    !call.c
    #include <stdio.h>

    struct my_struct {
        int A, B;
    };

    void call (struct my_struct p) {
        printf ("%d", p.A);
    }

In Ada, a type can be modified so that parameters of this type can always be
passed by copy.

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Param_By_Value_Ada

    with Interfaces.C;

    procedure Main is
       type my_struct is record
          A : Interfaces.C.int;
          B : Interfaces.C.int;
       end record
         with Convention => C_Pass_By_Copy;

       procedure Call (V : my_struct);
       pragma Import (C, Call, "call");
    begin
       null;
    end Main;

Note that this cannot be done at the subprogram declaration level, so if there
is a mix of by-copy and by-reference calls, two different types need to be
used on the Ada side.

Naming and prefixes
-------------------

Because of the absence of namespaces, any global name in C tends to be very
long. And because of the absence of overloading, they can even encode type
names in their type.

In Ada, the package is a namespace |mdash| two entities declared in two
different packages are clearly identified and can always be specifically
designated. The C names are usually a good indication of the names of the
future packages and should be stripped |mdash| it is possible to use the
full name if useful. For example, here's how the following declaration and
call could be translated:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Namespaces

    !reg_interface.h
    void registerInterface_Initialize (int size);

    !reg_interface_test.c
    #include "reg_interface.h"

    int main(int argc, const char * argv[])
    {
        registerInterface_Initialize(15);

        return 0;
    }

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Namespaces

    package Register_Interface is
       procedure Initialize (Size : Integer)
         with Import     => True,
              Convention => C,
              External_Name => "registerInterface_Initialize";

    end Register_Interface;

    with Register_Interface;

    procedure Main is
    begin
       Register_Interface.Initialize (15);
    end Main;

Note that in the above example, a :ada:`use` clause on
:ada:`Register_Interface` could allow us to omit the prefix.


.. _Ada_For_Embedded_C_Dev_Pointers:

Pointers
--------

The first thing to ask when translating pointers from C to Ada is: are they
needed in the first place? In Ada, pointers (or access types) should only be
used with complex structures that cannot be allocated at run-time |mdash| think
of a linked list or a graph for example. There are many other situations that
would need a pointer in C, but do not in Ada, in particular:

- Arrays, even when dynamically allocated

- Results of functions

- Passing large structures as parameters

- Access to registers

- ... others

This is not to say that pointers aren't used in these cases but, more often
than not, the pointer is hidden from the user and automatically handled by the
code generated by the compiler; thus avoiding possible mistakes from being
made. Generally speaking, when looking at C code, it's good practice to start
by analyzing how many pointers are used and to translate as many as possible
into *pointerless* Ada structures.

Here are a few examples of such patterns |mdash| additional examples can be
found throughout this document.

Dynamically allocated arrays can be directly allocated on the stack:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Array_Stack_Alloc_C

    !array_decl.c
    #include <stdlib.h>

    int main() {
        int *a = malloc(sizeof(int) * 10);

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Array_Stack_Alloc_Ada

    procedure Main is
       type Arr is array (Integer range <>) of Integer;
       A : Arr (0 .. 9);
    begin
       null;
    end Main;

It's even possible to create a such an array within a structure, provided that
the size of the array is known when instantiating this object, using a type
discriminant:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Struct_Array_Stack_Alloc_C

    !array_decl.c
    #include <stdlib.h>

    typedef struct {
        int * a;
    } S;

    int main(int argc, const char * argv[])
    {
        S v;

        v.a = malloc(sizeof(int) * 10);

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Struct_Array_Stack_Alloc_Ada

    procedure Main is
       type Arr is array (Integer range <>) of Integer;

       type S (Last : Integer) is record
          A : Arr (0 .. Last);
       end record;

       V : S (9);
    begin
       null;
    end Main;

With regards to parameter passing, usage mode (input / output) should be
preferred to implementation mode (by copy or by reference). The Ada compiler
will automatically pass a reference when needed. This works also for smaller
objects, so that the compiler will copy in an out when needed. One of the
advantages of this approach is that it clarifies the nature of the object: in
particular, it differentiates between arrays and scalars. For example:

[C]

.. code:: c manual_chop no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Array_In_Out_C

    !p.h
    void p (int * a, int * b);

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Array_In_Out_Ada

    package Array_Types is
       type Arr is array (Integer range <>) of Integer;

       procedure P (A : in out Integer; B : in out Arr);
    end Array_Types;

Most of the time, access to registers end up in some specific structures
being mapped onto a specific location in memory. In Ada, this can be achieved
through an :ada:`Address` clause associated to a variable, for example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Address_C

    !test_c.c
    int main(int argc, const char * argv[])
    {
        int * r = (int *)0xFFFF00A0;

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Address_Ada

    with System;

    procedure Test is
       R : Integer with Address => System'To_Address (16#FFFF00A0#);
    begin
       null;
    end Test;

These are some of the most common misuse of pointers in Ada. Previous sections
of the document deal with specifically using access types if absolutely
necessary.

.. _Ada_For_Embedded_C_Dev_Bitwise_Operations:

Bitwise Operations
------------------

Bitwise operations such as masks and shifts in Ada should be relatively rarely
needed, and, when translating C code, it's good practice to consider
alternatives. In a lot of cases, these operations are used to insert several
pieces of data into a larger structure. In Ada, this can be done by describing
the structure layout at the type level through representation clauses, and then
accessing this structure as any other.

Consider the case of using a C primitive type as a container for single bit
boolean flags. In C, this would be done through masks, e.g.:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Flags_C

    !flags.c
    #define FLAG_1 0b0001
    #define FLAG_2 0b0010
    #define FLAG_3 0b0100
    #define FLAG_4 0b1000

    int main(int argc, const char * argv[])
    {
        int value = 0;

        value |= FLAG_2 | FLAG_4;

        return 0;
    }

In Ada, the above can be represented through a Boolean array of enumerate
values:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Flags_Ada

    procedure Main is
       type Values is (Flag_1, Flag_2, Flag_3, Flag_4);
       type Value_Array is array (Values) of Boolean
         with Pack;

       Value : Value_Array :=
          (Flag_2 => True,
           Flag_4 => True,
           others => False);
    begin
       null;
    end Main;

Note the :ada:`Pack` directive for the array, which requests that the array
takes as little space as possible.

It is also possible to map records on memory when additional control over the
representation is needed or more complex data are used:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Rec_Map_C

    !struct_map.c
    int main(int argc, const char * argv[])
    {
        int value = 0;

        value = (2 << 1) | 1;

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Rec_Map_Ada

    procedure Main is
       type Value_Rec is record
         V1 : Boolean;
         V2 : Integer range 0 .. 3;
       end record;

       for Value_Rec use record
          V1 at 0 range 0 .. 0;
          V2 at 0 range 1 .. 2;
       end record;

       Value : Value_Rec := (V1 => True, V2 => 2);
    begin
       null;
    end Main;

The benefit of using Ada structure instead of bitwise operations is threefold:

- The code is simpler to read / write and less error-prone

- Individual fields are named

- The compiler can run consistency checks (for example, check that the value
  indeed fit in the expected size).

Note that, in cases where bitwise operators are needed, Ada provides modular
types with :ada:`and`, :ada:`or` and :ada:`xor` operators. Further shift
operators can also be provided upon request through a :ada:`pragma`. So the
above could also be literally translated to:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitwise_Ops_Ada
   :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Value_Type is mod 2 ** 32;
       pragma Provide_Shift_Operators (Value_Type);

       Value : Value_Type;
    begin
       Value := Shift_Left (2, 1) or 1;
       Put_Line ("Value = " & Value_Type'Image (Value));
    end Main;

.. _Ada_For_Embedded_C_Dev_Mapping_Structures_To_Bit_Fields:

Mapping Structures to Bit-Fields
--------------------------------

In the previous section, we've seen how to perform bitwise operations. In this
section, we look at how to interpret a data type as a bit-field and perform
low-level operations on it.

In general, you can create a bit-field from any arbitrary data type. First, we
declare a bit-field type like this:

[Ada]

.. code-block:: ada

    type Bit_Field is array (Natural range <>) of Boolean with Pack;

As we've seen previously, the :ada:`Pack` aspect declared at the end of the
type declaration indicates that the compiler should optimize for size. We must
use this aspect to be able to interpret data types as a bit-field.

Then, we can use the :ada:`Size` and the :ada:`Address` attributes of an
object of any type to declare a bit-field for this object. We've discussed the
:ada:`Size` attribute :ref:`earlier in this course <Ada_For_Embedded_C_Dev_Size_Aspect_Attribute>`.

The :ada:`Address` attribute indicates the address in memory of that object.
For example, assuming we've declare a variable :ada:`V`, we can declare an
actual bit-field object by referring to the :ada:`Address` attribute of
:ada:`V` and using it in the declaration of the bit-field, as shown here:

[Ada]

.. code-block:: ada

    B : Bit_Field (0 .. V'Size - 1) with Address => V'Address;

Note that, in this declaration, we're using the :ada:`Address` attribute of
:ada:`V` for the :ada:`Address` aspect of :ada:`B`.

This technique is called overlays for serialization. Now, any operation that we
perform on :ada:`B` will have a direct impact on :ada:`V`, since both are using
the same memory location.

The approach that we use in this section relies on the :ada:`Address` aspect.
Another approach would be to use unchecked conversions, which we'll
discuss in the :ref:`next section <Ada_For_Embedded_C_Dev_Overlays_Vs_Unchecked_Conversions>`.

We should add the :ada:`Volatile` aspect to the declaration to cover the case
when both objects can still be changed independently |mdash| they need to be
volatile, otherwise one change might be missed. This is the updated
declaration:

[Ada]

.. code-block:: ada

    B : Bit_Field (0 .. V'Size - 1) with Address => V'Address, Volatile;

Using the :ada:`Volatile` aspect is important at high level of optimizations.
You can find further details about this aspect in the section about the
:ref:`Volatile and Atomic aspects <Ada_For_Embedded_C_Dev_Volatile_Atomic_Data>`.

Another important aspect that should be added is :ada:`Import`. When used in
the context of object declarations, it'll avoid default initialization which
could overwrite the existing content while creating the overlay |mdash| see an
example in the admonition below. The declaration now becomes:

.. code-block:: ada

    B : Bit_Field (0 .. V'Size - 1)
      with
        Address => V'Address, Import, Volatile;

Let's look at a simple example:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Bitfield is
       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       V : Integer := 0;
       B : Bit_Field (0 .. V'Size - 1)
         with Address => V'Address, Import, Volatile;
    begin
       B (2) := True;
       Put_Line ("V = " & Integer'Image (V));
    end Simple_Bitfield;

In this example, we first initialize :ada:`V` with zero. Then, we use the
bit-field :ada:`B` and set the third element (:ada:`B (2)`) to :ada:`True`.
This automatically sets bit #3 of :ada:`V` to 1. Therefore, as expected,
the application displays the message :ada:`V = 4`, which corresponds to
2\ :sup:`2` = 4.

Note that, in the declaration of the bit-field type above, we could also have
used a positive range. For example:

.. code-block:: ada

    type Bit_Field is array (Positive range <>) of Boolean with Pack;

    B : Bit_Field (1 .. V'Size)
      with Address => V'Address, Import, Volatile;

The only difference in this case is that the first bit is :ada:`B (1)` instead
of :ada:`B (0)`.

In C, we would rely on bit-shifting and masking to set that specific bit:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_C

    !bitfield.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int v = 0;

        v = v | (1 << 2);

        printf("v = %d\n", v);

        return 0;
    }

.. admonition:: Important

    Ada has the concept of default initialization. For example, you may set the
    default value of record components:

    [Ada]

    .. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Default_Record_Type

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Main is

           type Rec is record
              X : Integer := 10;
              Y : Integer := 11;
           end record;

           R : Rec;
        begin
           Put_Line ("R.X = " & Integer'Image (R.X));
           Put_Line ("R.Y = " & Integer'Image (R.Y));
        end Main;

    In the code above, we don't explicitly initialize the components of
    :ada:`R`, so they still have the default values 10 and 11, which are
    displayed by the application.

    Likewise, the :ada:`Default_Value` aspect can be used to specify the
    default value in other kinds of type declarations. For example:

    [Ada]

    .. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Default_Value_Type

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Main is

           type Percentage is range 0 .. 100
             with Default_Value => 10;

           P : Percentage;
        begin
           Put_Line ("P = " & Percentage'Image (P));
        end Main;

    When declaring an object whose type has a default value, the object will
    automatically be initialized with the default value. In the example above,
    :ada:`P` is automatically initialized with 10, which is the default value
    of the :ada:`Percentage` type.

    Some types have an implicit default value. For example, access types have a
    default value of :ada:`null`.

    As we've just seen, when declaring objects for types with associated
    default values, automatic initialization will happen. This can also happens
    when creating an overlay with the :ada:`Address` aspect. The default value
    is then used to overwrite the content at the memory location indicated by
    the address. However, in most situations, this isn't the behavior we
    expect, since overlays are usually created to analyze and manipulate
    existing values. Let's look at an example where this happens:

    [Ada]

    .. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Overlay_Default_Init_Overwrite
        :class: ada-run

        package P is

           type Unsigned_8 is mod 2 ** 8 with Default_Value => 0;

           type Byte_Field is array (Natural range <>) of Unsigned_8;

           procedure Display_Bytes_Increment (V : in out Integer);
        end P;

        with Ada.Text_IO; use Ada.Text_IO;

        package body P is

           procedure Display_Bytes_Increment (V : in out Integer) is
              BF  : Byte_Field (1 .. V'Size / 8)
                with Address => V'Address, Volatile;
           begin
              for B of BF loop
                 Put_Line ("Byte = " & Unsigned_8'Image (B));
              end loop;
              Put_Line ("Now incrementing...");
              V := V + 1;
           end Display_Bytes_Increment;

        end P;

        with Ada.Text_IO; use Ada.Text_IO;

        with P; use P;

        procedure Main is
           V : Integer := 10;
        begin
           Put_Line ("V = " & Integer'Image (V));
           Display_Bytes_Increment (V);
           Put_Line ("V = " & Integer'Image (V));
        end Main;

    In this example, we expect :ada:`Display_Bytes_Increment` to display each
    byte of the :ada:`V` parameter and then increment it by one. Initially,
    :ada:`V` is set to 10, and the call to :ada:`Display_Bytes_Increment`
    should change it to 11. However, due to the default value associated to the
    :ada:`Unsigned_8` type |mdash| which is set to 0 |mdash| the value of
    :ada:`V` is overwritten in the declaration of :ada:`BF` (in
    :ada:`Display_Bytes_Increment`). Therefore, the value of :ada:`V` is 1
    after the call to :ada:`Display_Bytes_Increment`. Of course, this is not
    the behavior that we originally intended.

    Using the :ada:`Import` aspect solves this problem. This aspect tells the
    compiler to not apply default initialization in the declaration because the
    object is imported. Let's look at the corrected example:

    [Ada]

    .. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Overlay_Default_Init_Import
        :class: ada-run

        package P is

           type Unsigned_8 is mod 2 ** 8 with Default_Value => 0;

           type Byte_Field is array (Natural range <>) of Unsigned_8;

           procedure Display_Bytes_Increment (V : in out Integer);
        end P;

        with Ada.Text_IO; use Ada.Text_IO;

        package body P is

           procedure Display_Bytes_Increment (V : in out Integer) is
              BF  : Byte_Field (1 .. V'Size / 8)
                with Address => V'Address, Import, Volatile;
           begin
              for B of BF loop
                 Put_Line ("Byte = " & Unsigned_8'Image (B));
              end loop;
              Put_Line ("Now incrementing...");
              V := V + 1;
           end Display_Bytes_Increment;

        end P;

        with Ada.Text_IO; use Ada.Text_IO;

        with P; use P;

        procedure Main is
           V : Integer := 10;
        begin
           Put_Line ("V = " & Integer'Image (V));
           Display_Bytes_Increment (V);
           Put_Line ("V = " & Integer'Image (V));
        end Main;

    This unwanted side-effect of the initialization by the :ada:`Default_Value`
    aspect that we've just seen can also happen in these cases:

    - when we set a default value for components of a record type declaration,

    - when we use the :ada:`Default_Component_Value` aspect for array types, or

    - when we set use the :ada:`Initialize_Scalars` pragma for a package.

    Again, using the :ada:`Import` aspect when declaring the overlay eliminates
    this side-effect.

We can use this pattern for objects of more complex data types like arrays or
records. For example:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Int_Array_Ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Int_Array_Bitfield is
       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       A : array (1 .. 2) of Integer := (others => 0);
       B : Bit_Field (0 .. A'Size - 1)
         with Address => A'Address, Import, Volatile;
    begin
       B (2) := True;
       for I in A'Range loop
          Put_Line ("A (" & Integer'Image (I)
                    & ")= " & Integer'Image (A (I)));
       end loop;
    end Int_Array_Bitfield;

In the Ada example above, we're using the bit-field to set bit #3 of the first
element of the array (:ada:`A (1)`). We could set bit #4 of the second element
by using the size of the data type (in this case, :ada:`Integer'Size`):

[Ada]

.. code-block:: ada

    B (Integer'Size + 3) := True;

In C, we would select the specific array position and, again, rely on
bit-shifting and masking to set that specific bit:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Int_Array_C

    !bitfield_int_array.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int i;
        int a[2] = {0, 0};

        a[0] = a[0] | (1 << 2);

        for (i = 0; i < 2; i++)
        {
            printf("a[%d] = %d\n", i, a[i]);
        }

        return 0;
    }

Since we can use this pattern for any arbitrary data type, this allows us to
easily create a subprogram to serialize data types and, for example, transmit
complex data structures as a bitstream. For example:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Serialization_ada
    :class: ada-run

    package Serializer is

       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       procedure Transmit (B : Bit_Field);

    end Serializer;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Serializer is

       procedure Transmit (B : Bit_Field) is

          procedure Show_Bit (V : Boolean) is
          begin
             case V is
                when False => Put ("0");
                when True  => Put ("1");
             end case;
          end Show_Bit;

       begin
          Put ("Bits: ");
          for E of B loop
             Show_Bit (E);
          end loop;
          New_Line;
       end Transmit;

    end Serializer;

    package My_Recs is

       type Rec is record
          V : Integer;
          S : String (1 .. 3);
       end record;

    end My_Recs;

    with Serializer;  use Serializer;
    with My_Recs;     use My_Recs;

    procedure Main is
       R : Rec := (5, "abc");
       B : Bit_Field (0 .. R'Size - 1)
         with Address => R'Address, Import, Volatile;
    begin
       Transmit (B);
    end Main;

In this example, the :ada:`Transmit` procedure from :ada:`Serializer` package
displays the individual bits of a bit-field. We could have used this strategy
to actually transmit the information as a bitstream. In the main application,
we call :ada:`Transmit` for the object :ada:`R` of record type :ada:`Rec`.
Since :ada:`Transmit` has the bit-field type as a parameter, we can use it
for any type, as long as we have a corresponding bit-field representation.

In C, we interpret the input pointer as an array of bytes, and then use
shifting and masking to access the bits of that byte. Here, we use the
:c:`char` type because it has a size of one byte in most platforms.

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Serialization_C

    !my_recs.h
    typedef struct {
        int v;
        char s[4];
    } rec;

    !serializer.h
    void transmit (void *bits, int len);

    !serializer.c
    #include "serializer.h"

    #include <stdio.h>
    #include <assert.h>

    void transmit (void *bits, int len)
    {
        int i, j;
        char *c = (char *)bits;

        assert(sizeof(char) == 1);

        printf("Bits: ");
        for (i = 0; i < len / (sizeof(char) * 8); i++)
        {
            for (j = 0; j < sizeof(char) * 8; j++)
            {
                printf("%d", c[i] >> j & 1);
            }
        }
        printf("\n");
    }

    !bitfield_serialization.c
    #include <stdio.h>

    #include "my_recs.h"
    #include "serializer.h"

    int main(int argc, const char * argv[])
    {
        rec r = {5, "abc"};

        transmit(&r, sizeof(r) * 8);

        return 0;
    }

Similarly, we can write a subprogram that converts a bit-field |mdash| which
may have been received as a bitstream |mdash| to a specific type. We can add a
:ada:`To_Rec` subprogram to the :ada:`My_Recs` package to convert a bit-field
to the :ada:`Rec` type. This can be used to convert a bitstream that we
received into the actual data type representation.

As you know, we may write the :ada:`To_Rec` subprogram as a procedure or as a
function. Since we need to use slightly different strategies for the
implementation, the following example has both versions of :ada:`To_Rec`.

This is the updated code for the :ada:`My_Recs` package and the :ada:`Main`
procedure:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Deserialization_Ada
    :class: ada-run

    package Serializer is

       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       procedure Transmit (B : Bit_Field);

    end Serializer;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Serializer is

       procedure Transmit (B : Bit_Field) is

          procedure Show_Bit (V : Boolean) is
          begin
             case V is
                when False => Put ("0");
                when True  => Put ("1");
             end case;
          end Show_Bit;

       begin
          Put ("Bits: ");
          for E of B loop
             Show_Bit (E);
          end loop;
          New_Line;
       end Transmit;

    end Serializer;

    with Serializer;  use Serializer;

    package My_Recs is

       type Rec is record
          V : Integer;
          S : String (1 .. 3);
       end record;

       procedure To_Rec (B :     Bit_Field;
                         R : out Rec);

       function To_Rec (B : Bit_Field) return Rec;

       procedure Display (R : Rec);

    end My_Recs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body My_Recs is

       procedure To_Rec (B :     Bit_Field;
                         R : out Rec) is
          B_R : Rec
            with Address => B'Address, Import, Volatile;
       begin
          --  Assigning data from overlayed record B_R to output parameter R.
          R := B_R;
       end To_Rec;

       function To_Rec (B : Bit_Field) return Rec is
          R   : Rec;
          B_R : Rec
            with Address => B'Address, Import, Volatile;
       begin
          --  Assigning data from overlayed record B_R to local record R.
          R := B_R;

          return R;
       end To_Rec;

       procedure Display (R : Rec) is
       begin
          Put ("("  & Integer'Image (R.V) & ", "
               & (R.S) & ")");
       end Display;

    end My_Recs;

    with Ada.Text_IO; use Ada.Text_IO;
    with Serializer;  use Serializer;
    with My_Recs;     use My_Recs;

    procedure Main is
       R1 : Rec := (5, "abc");
       R2 : Rec := (0, "zzz");

       B1 : Bit_Field (0 .. R1'Size - 1)
         with Address => R1'Address, Import, Volatile;
    begin
       Put ("R2 = ");
       Display (R2);
       New_Line;

       --  Getting Rec type using data from B1, which is a bit-field
       --  representation of R1.
       To_Rec (B1, R2);

       --  We could use the function version of To_Rec:
       --  R2 := To_Rec (B1);

       Put_Line ("New bitstream received!");
       Put ("R2 = ");
       Display (R2);
       New_Line;
    end Main;

In both versions of :ada:`To_Rec`, we declare the record object :ada:`B_R` as
an overlay of the input bit-field. In the procedure version of :ada:`To_Rec`,
we then simply copy the data from :ada:`B_R` to the output parameter :ada:`R`.
In the function version of :ada:`To_Rec`, however, we need to declare a local
record object :ada:`R`, which we return after the assignment.

In C, we can interpret the input pointer as an array of bytes, and copy the
individual bytes. For example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Deserialization_C

    !my_recs.h
    typedef struct {
        int v;
        char s[3];
    } rec;

    void to_r (void *bits, int len, rec *r);

    void display_r (rec *r);

    !my_recs.c
    #include "my_recs.h"

    #include <stdio.h>
    #include <assert.h>

    void to_r (void *bits, int len, rec *r)
    {
        int i;
        char *c1 = (char *)bits;
        char *c2 = (char *)r;

        assert(len == sizeof(rec) * 8);

        for (i = 0; i < len / (sizeof(char) * 8); i++)
        {
            c2[i] = c1[i];
        }
    }

    void display_r (rec *r)
    {
        printf("{%d, %c%c%c}", r->v, r->s[0], r->s[1], r->s[2]);
    }

    !bitfield_serialization.c
    #include <stdio.h>
    #include "my_recs.h"

    int main(int argc, const char * argv[])
    {
        rec r1 = {5, "abc"};
        rec r2 = {0, "zzz"};

        printf("r2 = ");
        display_r (&r2);
        printf("\n");

        to_r(&r1, sizeof(r1) * 8, &r2);

        printf("New bitstream received!\n");
        printf("r2 = ");
        display_r (&r2);
        printf("\n");

        return 0;
    }

Here, :c:`to_r` casts both pointer parameters to pointers to :c:`char` to get
a byte-aligned pointer. Then, it simply copies the data byte-by-byte.

.. _Ada_For_Embedded_C_Dev_Overlays_Vs_Unchecked_Conversions:

Overlays vs. Unchecked Conversions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unchecked conversions are another way of converting between unrelated data
types. This conversion is done by instantiating the generic
:ada:`Unchecked_Conversions` function for the types you want to convert. Let's
look at a simple example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Simple_Unchecked_Conversion

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Unchecked_Conversion;

    procedure Simple_Unchecked_Conversion is
       type State is (Off, State_1, State_2)
         with Size => Integer'Size;

       for State use (Off => 0, State_1 => 32, State_2 => 64);

       function As_Integer is new Ada.Unchecked_Conversion (Source => State,
                                                            Target => Integer);

       I : Integer;
    begin
       I := As_Integer (State_2);
       Put_Line ("I = " & Integer'Image (I));
    end Simple_Unchecked_Conversion;

In this example, :ada:`As_Integer` is an instantiation of
:ada:`Unchecked_Conversion` to convert between the :ada:`State` enumeration and
the :ada:`Integer` type. Note that, in order to ensure safe conversion, we're
declaring :ada:`State` to have the same size as the :ada:`Integer` type we
want to convert to.

This is the corresponding implementation using overlays:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Simple_Overlay
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Overlay is
       type State is (Off, State_1, State_2)
         with Size => Integer'Size;

       for State use (Off => 0, State_1 => 32, State_2 => 64);

       S : State;
       I : Integer
         with Address => S'Address, Import, Volatile;
    begin
       S := State_2;
       Put_Line ("I = " & Integer'Image (I));
    end Simple_Overlay;

Let's look at another example of converting between different numeric formats.
In this case, we want to convert between a 16-bit fixed-point and a 16-bit
integer data type. This is how we can do it using :ada:`Unchecked_Conversion`:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Fixed_Int_Unchecked_Conversion

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Unchecked_Conversion;

    procedure Fixed_Int_Unchecked_Conversion is
       Delta_16 : constant := 1.0 / 2.0 ** (16 - 1);
       Max_16   : constant := 2 ** 15;

       type Fixed_16 is delta Delta_16 range -1.0 .. 1.0 - Delta_16
         with Size => 16;
       type Int_16   is range -Max_16 .. Max_16 - 1
         with Size => 16;

       function As_Int_16 is new Ada.Unchecked_Conversion (Source => Fixed_16,
                                                           Target => Int_16);
       function As_Fixed_16 is new Ada.Unchecked_Conversion (Source => Int_16,
                                                             Target => Fixed_16);

       I : Int_16   := 0;
       F : Fixed_16 := 0.0;
    begin
       F := Fixed_16'Last;
       I := As_Int_16 (F);

       Put_Line ("F = " & Fixed_16'Image (F));
       Put_Line ("I = " & Int_16'Image (I));
    end Fixed_Int_Unchecked_Conversion;

Here, we instantiate :ada:`Unchecked_Conversion` for the :ada:`Int_16` and
:ada:`Fixed_16` types, and we call the instantiated functions explicitly. In
this case, we call :ada:`As_Int_16` to get the integer value corresponding to
:ada:`Fixed_16'Last`.

This is how we can rewrite the implementation above using overlays:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Fixed_Int_Overlay
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Int_Overlay is
       Delta_16 : constant := 1.0 / 2.0 ** (16 - 1);
       Max_16   : constant := 2 ** 15;

       type Fixed_16 is delta Delta_16 range -1.0 .. 1.0 - Delta_16
         with Size => 16;
       type Int_16   is range -Max_16 .. Max_16 - 1
         with Size => 16;

       I : Int_16   := 0;
       F : Fixed_16
         with Address => I'Address, Import, Volatile;
    begin
       F := Fixed_16'Last;

       Put_Line ("F = " & Fixed_16'Image (F));
       Put_Line ("I = " & Int_16'Image (I));
    end Fixed_Int_Overlay;

Here, the conversion to the integer value is implicit, so we don't need to call
a conversion function.

Using :ada:`Unchecked_Conversion` has the advantage of making it clear that a
conversion is happening, since the conversion is written explicitly in the
code. With overlays, that conversion is automatic and therefore implicit. In
that sense, using an unchecked conversion is a cleaner and safer approach.
On the other hand, an unchecked conversion requires a copy, so it's less
efficient than overlays, where no copy is performed |mdash| because one change
in the source object is automatically reflected in the target object (and
vice-versa). In the end, the choice between unchecked conversions and overlays
depends on the level of performance that you want to achieve.

Also note that an unchecked conversion only has defined behavior
when instantiated for constrained types. For example, we shouldn't use this
kind of conversion:

.. code-block:: ada

   Ada.Unchecked_Conversion (Source => String,
                             Target => Integer);

Although this compiles, the behavior will only be well-defined in those cases
when :ada:`Source'Size = Target'Size`. Therefore, instead of using an
unconstrained type for :ada:`Source`, we should use a subtype that matches this
expectation:

.. code-block:: ada

   subtype Integer_String is String (1 .. Integer'Size / Character'Size);

   function As_Integer is new
     Ada.Unchecked_Conversion (Source => Integer_String,
                               Target => Integer);

Similarly, in order to rewrite the examples using bit-fields that we've
seen in the previous section, we cannot simply instantiate
:ada:`Unchecked_Conversion` with the :ada:`Target` indicating the
*unconstrained* bit-field, such as:

.. code-block:: ada

    Ada.Unchecked_Conversion (Source => Integer,
                              Target => Bit_Field);

Instead, we have to declare a subtype for the specific range we're interested
in. This is how we can rewrite one of the previous examples:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Translation.Bitfield_Conversion

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Unchecked_Conversion;

    procedure Simple_Bitfield_Conversion is
       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       V : Integer := 4;

       --  Declaring subtype that takes the size of V into account.
       --
       subtype Integer_Bit_Field is Bit_Field (0 .. V'Size - 1);

       --  NOTE: we could also use the Integer type in the declaration:
       --
       --    subtype Integer_Bit_Field is Bit_Field (0 .. Integer'Size - 1);
       --

       --  Using the Integer_Bit_Field subtype as the target
       function As_Bit_Field is new
         Ada.Unchecked_Conversion (Source => Integer,
                                   Target => Integer_Bit_Field);

       B : Integer_Bit_Field;
    begin
       B := As_Bit_Field (V);

       Put_Line ("V = " & Integer'Image (V));
    end Simple_Bitfield_Conversion;

In this example, we first declare the subtype :ada:`Integer_Bit_Field` as a
bit-field with a length that fits the :ada:`V` variable we want to convert to.
Then, we can use that subtype in the instantiation of
:ada:`Unchecked_Conversion`.
