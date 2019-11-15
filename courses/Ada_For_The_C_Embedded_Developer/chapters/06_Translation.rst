C to Ada Translation Patterns
=================================

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Naming conventions and casing considerations
--------------------------------------------

One question that may arise relatively soon when converting from C to Ada is
the style of identifiers. The Ada language doesn't impose any particular style
and for many reasons, it may seem attractive to keep a C-like style, for
example camel casing, to the Ada program.

However, the Ada run-time library |mdash| in particular the one provided by
GNAT |mdash| is following a specific style and using a different style for the
rest of the program leads to inconsistencies, decreasing readability and
confusing automatic style checkers. For those reasons, it's usually advisable
to adopt the Ada style |mdash| which each word is an upper case letter followed
by lower cases (several upper case letter at start are OK), with an underscore
separating two words.

Following this scheme doesn't prevent to have an extra layer of compatible
rules. For example, Ada being strongly typed language, it's not rare to create
a type for only one instance and not to have a different word to identify the
variable and is type |mdash| so some styles add systematically a leading ``T_``
or trailing ``_T`` to all type names.

Interfacing C and Ada
---------------------

Manual Interfacing
~~~~~~~~~~~~~~~~~~

Before even considering translating code from C to Ada, it's worthwhile
evaluating the possibility of keeping portion of the C code intact, and only
translating selected modules in Ada. This is a necessary evil when introducing
Ada to a large existing C codebase, were re-writing the entire code upfront is
not practical or not cost effective.

Fortunately, Ada has a dedicated set of features for interfacing with other
languages. The :ada:`Interfaces` package hierarchy and the pragmas
:ada:`Convention`, :ada:`Import`, and :ada:`Export` allow you to make
inter-language calls while observing proper data representation for each
language.

Let's start with the following C code:

.. code-block:: c

    typedef struct my_struct {
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

.. code-block:: ada

    type my_struct is record
       A : Interfaces.C.int;
       B : Interfaces.C.int;
    end record;
    pragma Convention (C, my_struct);

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

.. code-block:: ada

    declare
       V : my_struct := (A => 1, B => 2);
    begin
       Call (V);
    end;

Building and Debugging mixed language code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Automatic interfacing
~~~~~~~~~~~~~~~~~~~~~

It may be useful to start interfacing Ada to C by using automatic binding
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
lot of by-pointer parameters which may be better to be replaced by other
parameter modes, etc.

However, the automatic binding generator helps having a starting point which
ensures compatibility of the Ada and the C code.

Using Arrays in C interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is relatively straightforward to pass an array from Ada to C. In particular,
with the GNAT compiler, passing an array is equivalent to passing a pointer to
its first element. Of course, as there's no notion of boundaries in C, the
length of the array needs to be passed explicitly. For example:

[C]

.. code-block:: c

    void p (int * a, int length);

[Ada]

.. code-block:: ada

    type Arr is array (Integer range <>) of Integer;

    procedure P (V : Arr; Length : Integer);
    pragma Import (C, P);

    X : Arr (5 .. 15);

    P (X, X'Length);

The other way around |mdash| that is, retrieving an array that has been
creating on the C side |mdash| is more difficult. Because C doesn't explicitly
carry boundaries, they need to be recreated in some way.

The first option is to actually create an Ada array without boundaries. This is
the most flexible, but also the least safe option. It involves creating an
array with indices over the full range of Integer without ever creating it from
Ada, but instead retrieving it as an access from C. For example:

[C]

.. code-block:: c

    int * f ();

[Ada]

.. code-block:: ada

    type Arr is array (Integer) of Integer;
    type Arr_A is access all Arr;

    function F return Arr_A;
    pragma Import (C, F);

Note that :ada:`Arr` is a constrained type (it doesn't have the :ada:`range <>`
notation for indices). For that reason, as it would be for C, it's possible to
iterate over the whole range of integer, beyond the memory actually allocated
for the array.

A somewhat safer way is to overlay an Ada array over the C one. This requires
to have access to the length of the array. Let's consider this time two cases,
one with an array and its size accessible through functions, another one on
global variables. This time, as we're using an overlay, the function will be
directly mapped to an Ada function returning an address:

[C]

.. code-block:: c

    int * f_arr (void);
    int f_size (void);

    int * g_arr;
    int g_size;

[Ada]

.. code-block:: ada

    type Arr is array (Integer range <>) of Integer;

    function F_Arr return System.Address;
    function F_Size return Integer;

    F : Arr (0 .. F_Size - 1) with Address => F_Size;

    G_Size : Integer;
    pragma Import (C, G_Size, "g_size");

    G_Arr : Arr (0 .. G_Size - 1);
    pragma Import (C, G_Arr, "g_arr");

With all solutions though, importing an array from C is a relatively unsafe
pattern, as there's only so much information on the array as there would be on
the C side in the first place. These are good places for careful peer reviews.

By-value v.s. by-reference types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When interfacing Ada and C, the rules of parameter passing are a bit different
with regards to what's a reference and what's a copy. Scalar types and pointers
are passed by value, record and arrays are always passed by reference (recall
that in pure Ada, this may vary from the size of the object). However, there
may be cases where the C interface also pass values and not pointers to
objects. Here's a slightly modified version of a previous example to illustrate
this point:

[C]

.. code-block:: c

    typedef struct my_struct {
       int A, B;
    };

    void call (struct my_struct p) {
       printf ("%d", p.A);
    }

In Ada, a type can be modified so that parameter of this type can always be
passed by copy.

.. code-block:: ada

    type my_struct is record
       A : Interfaces.C.int;
       B : Interfaces.C.int;
    end record
      with Convention => C,
                         C_Pass_By_Copy;

    procedure Call (V : my_struct);
    pragma Import (C, Call, "call");

Note that this cannot be done at the subprogram declaration level, so if there
is a mix of a by-copy and by-reference calls, two different types need to be
used on the Ada side.

Naming and prefixes
~~~~~~~~~~~~~~~~~~~

Because of the absence of namespaces, any global name in C tend to be very
long. And because of the absence of overloading, they can even encode type
names in their type.

In Ada, the package is a namespace |mdash| two entities declared in two
different packages are clearly identified and can always be specifically
designated. The C names are usually a good indication of the names of the
futures packages and should be stripped |mdash| it will be possible to force
full name if useful. For example, here's how the following declaration and call
could be translated:

[C]

.. code-block:: c

    void registerInterface_Initialize (int size);

    registerInterface_Initialize(15);

[Ada]

.. code-block:: ada

    package Register_Interface is
       Procedure Initialize(Size : Integer)
    end Register_Interface;

    Register_Interface.Initialize (15);

Note that in the above example, a :ada:`use` clause on
:ada:`Register_Interface` could allow to omit the prefix.

Pointers
~~~~~~~~

The first thing to ask when translating pointers from C to Ada is: are they
needed in the first place? In Ada, pointers (or access types) should only be
used with complex structures that cannot be allocated at run-time |mdash| think
of a linked list or a graph for example. There are many other situations that
would need a pointer in C but doesn't in Ada, in particular:

- Arrays, even when dynamically allocated

- Results of functions

- Passing large structures as parameters

- Access to registers

- ... others

This is not to say that no pointers is used in these cases, but more often than
not the pointer is hidden from the user and automatically handled by the code
generated by the compiler, avoiding possible mistakes to be made. Generally
speaking, it's good practise when looking at C code to start by analyzing how
many pointers are used and to translate as many as possible into *pointerless*
Ada structures.

Here are a few examples of such patterns |mdash| additional examples can be
found through this document.

Dynamically allocated array can be directly allocated on the stack:

[C]

.. code-block:: c

    int [] a = new int [10];

[Ada]

.. code-block:: ada

    type Arr is new Array (Integer range <>) of Integer;
    A : Arr (0 .. 9);

It's even possible to create a such an array within a structure, provided that
the size of the array is known when instantiating this object, using a type
discriminant:

[C]

.. code-block:: c

    typedef struct S {
       int * a;
    };

    S v;

    V.a = new int [10];

[Ada]

.. code-block:: ada

    type Arr is new Array (Integer range <>) of Integer;

    type S (Last : Integer) is record
       A : Arr (0 .. Last);
    end record;

    V : S (9);

With regards to parameter passing, usage mode (input / output) should be
preferred to implementation mode (by copy or by reference). The Ada compiler
will automatically pass a reference when needed. This works also for smaller
objects that the compiler will copy in an out when needed. One of the advantage
is that it clarifies the nature of the object, in particular differentiates
arrays from scalars. For example:

[C]

.. code-block:: c

    void p (int * a, int * b);

[Ada]

.. code-block:: ada

    type Arr is new Array (Integer range <>) of Integer;

    procedure P (A : in out Integer; B : in out Arr);

Most of the time, access to registers end up into some specific structures
being mapped on to a specific location in memory. In Ada, this can be achieved
through an :ada:`Address` clause associated to a variable, for example:

[C]

.. code-block:: c

    int * r = 0xFFFF00A0;

[Ada]

.. code-block:: ada

    with System;

    procedure Test is
       R : Integer
         with Address => System.To_Address (16#FFFF00A0#);
    begin
       null;
    end;

These are some of the most common misuse of pointers in Ada. Previous sections
of the document deal with specifically using access types if absolutely
necessary.

Bitwise Operations
~~~~~~~~~~~~~~~~~~

Bitwise operations such as masks and shifts in Ada should be relatively rarely
needed, and it's good practise when translating C code to consider alternative.
In a lot of cases, these operations are used to insert several pieces of data
into a larger structure. In Ada, this can be done by describing the structure
layout at the type level through representation clauses, and then accessing
this structure as any other.

The simpler example of the above is the case of an array of bytes representing
various flags. In C, this would be done through masks, e.g.:

[C]

.. code-block:: c

    #define flag1 = 0b0001;
    #define flag2 = 0b0010;
    #define flag3 = 0b0100;
    #define flag4 = 0b1000;

    int value = 0;

    value = value | flag2 | flag4;

In Ada, the above can be represented through a Boolean array of enumerate
values:

[Ada]

.. code-block:: ada

    type Values is (Flag_1, Flag_2, Flag_3, Flag_4);
    type Value_Array is array (Values) of Boolean
       with Pack;

    Value : Value_Array :=
       (Flat_2 => True,
        Flag_4 => True,
        others => False)

Note the :ada:`Pack` directive for the array which guarantees that the array
takes as little space as possible.

It is also possible to map record on memory when additional control over the
representation is needed or more complex data are used:

[C]

.. code-block:: c

    int value = 0;

    value = (2 << 1) | 1;

[Ada]

.. code-block:: ada

    type Value_Rec is record
      V1 : Boolean;
      V2 : Integer range 0 .. 3;
    end record;

    for Value_Rec use record
       V1 at 0 range 0 .. 0;
       V2 at 0 range 1 .. 2;
    end record;

    Value : Value_Rec := (V1 => True, V2 => 2);

The benefit of using Ada structure instead of bitwise operations is threefold:

- The code is simpler to read / write and less error-prone

- Data are named

- The compiler can run consistency checks (for example, check that the value
  indeed fit in the expected size).

Note that in case where bitwise operators are needed, Ada provides modular
types with :ada:`and`, :ada:`or` and :ada:`xor` operators. Further shifts
operators can also be provided upon request through a :ada:`pragma`. So the
above could also be literally translated to:

[C]

.. code-block:: ada

    type Value_Type is mod 2 ** 32
       with Provide_Shift_Operators;

    Value : Value_Type := Shift_Left (2, 1) or 1;

Converting structures to Integer or Addresses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!
