C to Ada Translation Patterns
=================================

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

:code-config:`reset_accumulator=True`

.. include:: ../../global.txt

Naming conventions and casing considerations
--------------------------------------------

One question that may arise relatively soon when converting from C to Ada is
the style of identifiers. The Ada language doesn't impose any particular style
and for many reasons, it may seem attractive to keep a C-like style |mdash| for
example, camel casing |mdash| to the Ada program.

However, the Ada run-time library |mdash| in particular the one provided by
GNAT |mdash| is following a specific style, so that using a different style for
the rest of the program leads to inconsistencies, thereby decreasing readability
and confusing automatic style checkers. For those reasons, it's usually
advisable to adopt the Ada style |mdash| which each word is an upper case letter
followed by lower cases (several upper case letter at start are OK), with an
underscore separating two words.

Following this scheme doesn't prevent to have an extra layer of compatible
rules. For example, Ada being a strongly typed language, it's not rare to create
a type for only one instance and not to have a different word to identify the
variable and its type. Therefore, some styles add systematically a leading ``T_``
or trailing ``_T`` to all type names.

Interfacing C and Ada
---------------------

Manual Interfacing
~~~~~~~~~~~~~~~~~~

Before even considering translating code from C to Ada, it's worthwhile
evaluating the possibility of keeping portion of the C code intact, and only
translating selected modules to Ada. This is a necessary evil when introducing
Ada to an existing large C codebase, where re-writing the entire code upfront is
not practical or not cost-effective.

Fortunately, Ada has a dedicated set of features for interfacing with other
languages. The :ada:`Interfaces` package hierarchy and the pragmas
:ada:`Convention`, :ada:`Import`, and :ada:`Export` allow you to make
inter-language calls while observing proper data representation for each
language.

Let's start with the following C code:

:code-config:`accumulate_code=True`

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.My_Struct

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.My_Struct

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

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.My_Struct

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

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

Building and Debugging mixed language code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
`Introduction to GNAT Toolchain <https://learn.adacore.com/courses/GNAT_Toolchain_Intro/index.html>`_
course.

Automatic interfacing
~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is relatively straightforward to pass an array from Ada to C. In particular,
with the GNAT compiler, passing an array is equivalent to passing a pointer to
its first element. Of course, as there's no notion of boundaries in C, the
length of the array needs to be passed explicitly. For example:

:code-config:`accumulate_code=True`

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_1

    !p.h
    void p (int * a, int length);

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_1

    procedure Main is
       type Arr is array (Integer range <>) of Integer;

       procedure P (V : Arr; Length : Integer);
       pragma Import (C, P);

       X : Arr (5 .. 15);
    begin
       P (X, X'Length);
    end Main;

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

The other way around |mdash| that is, retrieving an array that has been
creating on the C side |mdash| is more difficult. Because C doesn't explicitly
carry boundaries, they need to be recreated in some way.

The first option is to actually create an Ada array without boundaries. This is
the most flexible, but also the least safe option. It involves creating an
array with indices over the full range of :ada:`Integer` without ever creating
it from Ada, but instead retrieving it as an access from C. For example:

:code-config:`accumulate_code=True`

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_2

    !f.h
    int * f ();

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_2

    procedure Main is
       type Arr is array (Integer) of Integer;
       type Arr_A is access all Arr;

       function F return Arr_A;
       pragma Import (C, F);
    begin
       null;
    end Main;

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

Note that :ada:`Arr` is a constrained type (it doesn't have the :ada:`range <>`
notation for indices). For that reason, as it would be for C, it's possible to
iterate over the whole range of integer, beyond the memory actually allocated
for the array.

A somewhat safer way is to overlay an Ada array over the C one. This requires
having access to the length of the array. This time, let's consider two cases,
one with an array and its size accessible through functions, another one on
global variables. This time, as we're using an overlay, the function will be
directly mapped to an Ada function returning an address:

:code-config:`accumulate_code=True`

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_3

    !fg.h
    int * f_arr (void);
    int f_size (void);

    int * g_arr;
    int g_size;

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Arr_3

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

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

With all solutions though, importing an array from C is a relatively unsafe
pattern, as there's only so much information on the array as there would be on
the C side in the first place. These are good places for careful peer reviews.

By-value v.s. by-reference types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When interfacing Ada and C, the rules of parameter passing are a bit different
with regards to what's a reference and what's a copy. Scalar types and pointers
are passed by value, record and arrays are always passed by reference (recall
that, in pure Ada, this may vary from the size of the object). However, there
may be cases where the C interface also passes values and not pointers to
objects. Here's a slightly modified version of a previous example to illustrate
this point:

:code-config:`accumulate_code=True`

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Param_By_Value

    !call.c
    typedef struct my_struct {
        int A, B;
    };

    void call (struct my_struct p) {
        printf ("%d", p.A);
    }

In Ada, a type can be modified so that parameters of this type can always be
passed by copy.

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Param_By_Value

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

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

Note that this cannot be done at the subprogram declaration level, so if there
is a mix of by-copy and by-reference calls, two different types need to be
used on the Ada side.

Naming and prefixes
~~~~~~~~~~~~~~~~~~~

Because of the absence of namespaces, any global name in C tends to be very
long. And because of the absence of overloading, they can even encode type
names in their type.

In Ada, the package is a namespace |mdash| two entities declared in two
different packages are clearly identified and can always be specifically
designated. The C names are usually a good indication of the names of the
futures packages and should be stripped |mdash| it will be possible to force
full name if useful. For example, here's how the following declaration and call
could be translated:

:code-config:`accumulate_code=True`

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Namespaces

    !reg_interface.h
    void registerInterface_Initialize (int size);

    !reg_interface_test.c
    #include "reg_interface.h"

    int main(int argc, const char * argv[])
    {
        registerInterface_Initialize(15);
    }

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Namespaces

    package Register_Interface is
       procedure Initialize(Size : Integer)
         with Import     => True,
              Convention => C,
              External_Name => "registerInterface_Initialize";

    end Register_Interface;

    with Register_Interface;

    procedure Main is
    begin
       Register_Interface.Initialize (15);
    end Main;

:code-config:`accumulate_code=False`

:code-config:`reset_accumulator=True`

Note that in the above example, a :ada:`use` clause on
:ada:`Register_Interface` could allow to omit the prefix.

Pointers
~~~~~~~~

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

This is not to say that no pointers is used in these cases, but, more often than
not, the pointer is hidden from the user and automatically handled by the code
generated by the compiler, thus avoiding possible mistakes to be made. Generally
speaking, when looking at C code, it's good practice to start by analyzing how
many pointers are used and to translate as many as possible into *pointerless*
Ada structures.

Here are a few examples of such patterns |mdash| additional examples can be
found throughout this document.

Dynamically allocated arrays can be directly allocated on the stack:

[C]

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Array_Stack_Alloc_C

    !array_decl.c
    #include <stdlib.h>

    int main() {
        int *a = malloc(sizeof(int) * 10);
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Array_Stack_Alloc_Ada

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

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Struct_Array_Stack_Alloc_C

    !array_decl.c
    #include <stdlib.h>

    typedef struct {
        int * a;
    } S;

    int main(int argc, const char * argv[])
    {
        S v;

        v.a = malloc(sizeof(int) * 10);
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Struct_Array_Stack_Alloc_Ada

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

.. code:: c manual_chop project=Courses.Ada_For_C_Embedded_Dev.Translation.Array_In_Out_C

    !p.h
    void p (int * a, int * b);

[Ada]

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Translation.Array_In_Out_Ada

    package Array_Types is
       type Arr is array (Integer range <>) of Integer;

       procedure P (A : in out Integer; B : in out Arr);
    end Array_Types;

Most of the time, access to registers end up in some specific structures
being mapped onto a specific location in memory. In Ada, this can be achieved
through an :ada:`Address` clause associated to a variable, for example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Address_C

    !test_c.c
    int main(int argc, const char * argv[])
    {
        int * r = (int *)0xFFFF00A0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Address_Ada

    with System;

    procedure Test is
       R : Integer with Address => System'To_Address (16#FFFF00A0#);
    begin
       null;
    end Test;

These are some of the most common misuse of pointers in Ada. Previous sections
of the document deal with specifically using access types if absolutely
necessary.

Bitwise Operations
~~~~~~~~~~~~~~~~~~

Bitwise operations such as masks and shifts in Ada should be relatively rarely
needed, and, when translating C code, it's good practice to consider
alternatives. In a lot of cases, these operations are used to insert several
pieces of data into a larger structure. In Ada, this can be done by describing
the structure layout at the type level through representation clauses, and then
accessing this structure as any other.

This is the case of an array of bytes representing various flags. In C, this
would be done through masks, e.g.:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Flags_C

    !flags.c
    #define FLAG_1 0b0001
    #define FLAG_2 0b0010
    #define FLAG_3 0b0100
    #define FLAG_4 0b1000

    int main(int argc, const char * argv[])
    {
        int value = 0;

        value |= FLAG_2 | FLAG_4;
    }

In Ada, the above can be represented through a Boolean array of enumerate
values:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Flags_Ada

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

Note the :ada:`Pack` directive for the array, which guarantees that the array
takes as little space as possible.

It is also possible to map records on memory when additional control over the
representation is needed or more complex data are used:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Rec_Map_C

    !struct_map.c
    int main(int argc, const char * argv[])
    {
        int value = 0;

        value = (2 << 1) | 1;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Rec_Map_Ada

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

Note that, in case where bitwise operators are needed, Ada provides modular
types with :ada:`and`, :ada:`or` and :ada:`xor` operators. Further shifts
operators can also be provided upon request through a :ada:`pragma`. So the
above could also be literally translated to:

[C]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Bitwise_Ops_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Value_Type is mod 2 ** 32;
       pragma Provide_Shift_Operators (Value_Type);

       Value : Value_Type;
    begin
       Value := Shift_Left (2, 1) or 1;
       Put_Line ("Value = " & Value_Type'Image (Value));
    end Main;

Serialization of data types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous section, we've seen how to perform bitwise operations. In this
section, we look at how to interpret a data type as a bit-field and perform
low-level operations on it.

In general, you can create a bit-field from any arbitrary data type. First, we
declare a bit-field type like this:

.. code-block:: ada

    type Bit_Field is array (Natural range <>) of Boolean with Pack;

As we've seen previously, the :ada:`Pack` aspect declared at the end of the
type declaration indicates that the compiler should optimize for size. We must
use this attribute to be able to interpret data types as a bit-field.

Then, we can use the :ada:`Size` and the :ada:`Address` attributes of an
object of any type to declare a bit-field for this object. The :ada:`Size`
attributes indicates the number of bits required to represent the object, while
the :ada:`Address` attribute indicates the address in memory of that object.
For example, assuming we've declare a variable :ada:`V`, we can declare an
actual bit-field object using this pattern:

.. code-block:: ada

    B : Bit_Field (0 .. V'Size - 1) with Address => V'Address;

This technique is called overlays for serialization. Now, any operation that we
perform on :ada:`B` will have a direct impact on :ada:`V`, since both are using
the same memory location.

Let's look at a simple example:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Bitfield

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Bitfield is
       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       V : Integer := 0;
       B : Bit_Field (0 .. V'Size - 1) with Address => V'Address;
    begin
       B (2) := True;
       Put_Line ("V = " & Integer'Image (V));
    end Simple_Bitfield;

In this example, we first initialize :ada:`V` with zero. Then, we use the
bit-field :ada:`B` and set the third element (:ada:`B (2)`) to :ada:`True`.
This automatically sets bit #3 of :ada:`V` to 1. Therefore, as expected,
the application displays the message :ada:`V = 4`, which corresponds to
:math:`2^2 = 4`.

Note that, in the declaration of the bit-field type above, we could also have
used a positive range. For example:

.. code-block:: ada

    type Bit_Field is array (Positive range <>) of Boolean with Pack;

    B : Bit_Field (1 .. V'Size) with Address => V'Address;

The only difference in this case is that the first bit is :ada:`B (1)` instead
of :ada:`B (0)`.

We can use this pattern for objects of more complex data types like arrays or
records. For example:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Bitfield_Int_Array

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Int_Array_Bitfield is
       type Bit_Field is array (Natural range <>) of Boolean with Pack;

       A : array (1 .. 2) of Integer := (others => 0);
       B : Bit_Field (0 .. A'Size - 1) with Address => A'Address;
    begin
       B (2) := True;
       for I in A'Range loop
          Put_Line ("Value (" & Integer'Image (I)
                    & ")= " & Integer'Image (A (I)));
       end loop;
    end Int_Array_Bitfield;

In this example, we're using the bit-field to set bit #3 of the first element
of the array (:ada:`A (1)`). We could set bit #4 of the second element by
using the size of the data type (in this case, :ada:`Integer'Size`):

.. code-block:: ada

    B (Integer'Size + 3) := True;

Since we can use this pattern for any arbitrary data type, this allows us to
easily create a subprogram to serialize data types and, for example, transmit
complex data structures as a bitstream. For example:


.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Bitfield_Serialization

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
          for I in B'Range loop
             Show_Bit (B (I));
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
       B : Bit_Field (0 .. R'Size - 1) with Address => R'Address;
    begin
       Transmit (B);
    end Main;

In this example, the :ada:`Transmit` procedure from :ada:`Serializer` package
displays the individual bits of a bit-field. We could have used this strategy
to actually transmit the information as a bitstream. In the main application,
we call :ada:`Transmit` for the object :ada:`R` of record type :ada:`Rec`.
Since :ada:`Transmit` has the bit-field type as a parameter, we can use it
for any type, as long as we have a corresponding bit-field representation.

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Translation.Bitfield_Serialization

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
          B_R : Bit_Field (0 .. R'Size - 1) with Address => R'Address;
       begin
          --  Assigning data from bit-field parameter B to bit-field
          --  representation of output parameter.
          B_R := B;
       end To_Rec;

       function To_Rec (B : Bit_Field) return Rec is
          R   : Rec;
          B_R : Bit_Field (0 .. R'Size - 1) with Address => R'Address;
       begin
          --  Assigning data from bit-field parameter B to local bit-field B_R.
          --  R is automatically updated.
          B_R := B;

          return R;
       end To_Rec;

       procedure Display (R : Rec) is
       begin
          Put_Line ("("  & Integer'Image (R.V) & ", "
                    & (R.S) & ")");
       end;

    end My_Recs;

    with Ada.Text_IO; use Ada.Text_IO;
    with Serializer;  use Serializer;
    with My_Recs;     use My_Recs;

    procedure Main is
       R1 : Rec := (5, "abc");
       R2 : Rec := (0, "zzz");

       B1 : Bit_Field (0 .. R1'Size - 1) with Address => R1'Address;
    begin
       Put ("R2 = ");
       Display(R2);
       New_Line;

       --  Getting Rec type using data from B1, which is a bit-field
       --  representation of R1.
       To_Rec (B1, R2);

       --  We could use the function version of To_Rec:
       --  R2 := To_Rec (B1);

       Put_Line ("New bitstream received!");
       Put ("R2 = ");
       Display(R2);
       New_Line;
    end Main;

In the procedure version of :ada:`To_Rec`, we create a bit-field representation
of the output parameter (:ada:`B_R`) and simply copy the data from the input
bit-field to :ada:`B_R`, so that the output parameter is updated. In the
function version of :ada:`To_Rec`, however, we need to use a local object and a
corresponding bit-field representation and return this object after updating
the bit-field.
