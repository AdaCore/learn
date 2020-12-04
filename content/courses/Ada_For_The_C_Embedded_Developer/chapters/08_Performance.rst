Performance considerations
==========================

.. include:: ../../global.txt

Overall expectations
--------------------

All in all, there should not be significant performance differences between
code written in Ada and code written in C, provided that they are semantically
equivalent. Taking the current GNAT implementation and its GCC C counterpart
for example, most of the code generation and optimization phases are shared
between C and Ada |mdash| so there's not one compiler more efficient than the
other. Furthermore, the two languages are fairly similar in the way they
implement imperative semantics, in particular with regards to memory management
or control flow. They should be equivalent on average.

However, it's not uncommon to have projects developing similar pieces of code
in Ada and C to confirm relative performances, and to observe differences from
20 |ndash| 30% to sometimes 5 or 20 times slower. This usually comes from the
fact that, while the two piece appear semantically equivalent, they happen to
be actually quite different. This section will list some of the most common
suspects and their resolution.

Switches and optimizations
--------------------------

Clever use of compilation switches might optimize the performance of an
application significantly. In this section, we'll briefly look into some of
the switches available in the GNAT toolchain.

Optimizations levels
~~~~~~~~~~~~~~~~~~~~

Optimization levels can be found in many compilers for multiple languages. On
the lowest level, the compiler doesn't optimize the code at all, while at the
highest level, the compiler analyses the code and optimizes it by removing
unnecessary operations and making most use of the target processor's
capabilities.

By being part of GCC, GNAT offers the same ``-O_`` switches as GCC:

+-------------+--------------------------------------------------------------+
| Switch      | Description                                                  |
+=============+==============================================================+
| ``-O0``     | No optimization: the generated code is completely            |
|             | unoptimized. This is the default optimization level.         |
+-------------+--------------------------------------------------------------+
| ``-O1``     | Moderate optimization.                                       |
+-------------+--------------------------------------------------------------+
| ``-O2``     | Full optimization.                                           |
+-------------+--------------------------------------------------------------+
| ``-O3``     | Same optimization level as for ``-O2``. In addition, further |
|             | optimization strategies, such as aggressive automatic        |
|             | inlining and vectorization.                                  |
+-------------+--------------------------------------------------------------+

Note that the highest the level, the slowest will be the compilation time. For
fast compilation during development phase, unless you're working on
benchmarking algorithms, using ``-O0`` is probably a good idea.

In addition to the levels presented above, GNAT also has the ``-Os`` switch,
which allows for optimizing code and data usage.

Inlining
~~~~~~~~

As we've seen in the previous section, automatic inlining depends on the
optimization level. The highest optimization level (``-O3``), for example,
performs aggressive automatic inlining. This could mean that this level inlines
too much rather than not enough. As a result, the cache may become an issue and
the overall performance may be worse than the one we would achieve by compiling
the same code with optimization level 2 (``-O2``). Therefore, the general
recommendation is to not *just* select ``-O3`` for the optimized version of an
application, but instead compare it the optimized version built with ``-O2``.

In some cases, it's better to reduce the optimization level and perform manual
inlining instead of automatic inlining. We do that by using the :ada:`Inline`
aspect. Let's reuse an example from a previous chapter and inline the
:ada:`Average` function:

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Inlining

    package Float_Arrays is

       type Float_Array is array (Positive range <>) of Float;

       function Average (Data : Float_Array) return Float
         with Inline;

    end Float_Arrays;

    package body Float_Arrays is

       function Average (Data : Float_Array) return Float is
          Total : Float := 0.0;
       begin
          for Value of Data loop
             Total := Total + Value;
          end loop;
          return Total / Float (Data'Length);
       end Average;

    end Float_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Float_Arrays; use Float_Arrays;

    procedure Compute_Average is
       Values        : constant Float_Array := (10.0, 11.0, 12.0, 13.0);
       Average_Value : Float;
    begin
       Average_Value := Average (Values);
       Put_Line ("Average = " & Float'Image (Average_Value));
    end Compute_Average;

When compiling this example, GNAT will inline :ada:`Average` in the
:ada:`Compute_Average` procedure.

In order to effectively use this aspect, however, we need to set the
optimization level to at least ``-O1`` and use the ``-gnatn`` switch, which
instructs the compiler to take the :ada:`Inline` aspect into account.

Note, however, that the :ada:`Inline` aspect is just a *recommendation* to the
compiler. Sometimes, the compiler might not be able to follow this
recommendation, so it won't inline the subprogram. In this case, we get a
compilation warning from GNAT.

These are some examples of situations where the compiler might not be able to
inline a subprogram:

- when the code is too large,

- when it's too complicated |mdash| for example, when it involves exception
  handling |mdash|, or

- when it contains tasks, etc.

In addition to the :ada:`Inline` aspect, we also have the :ada:`Inline_Always`
aspect. In contrast to the former aspect, however, the :ada:`Inline_Always`
aspect isn't primarily related to performance. Instead, it should be used when
the functionality would be incorrect if inlining was not performed by the
compiler. Examples of this are procedures that insert Assembly instructions
that only make sense when the procedure is inlined, such as memory barriers.

Similar to the :ada:`Inline` aspect, there might be situations where a
subprogram has the :ada:`Inline_Always` aspect, but the compiler is unable to
inline it. In this case, we get a compilation error from GNAT.

Checks and assertions
---------------------

.. todo::

    Complete section!


Dynamic v.s static structures
-----------------------------

Ada generally speaking provides more ways than C or C++ to write simple dynamic
structures, that is to say structures that have constraints computed after
variables. For example, it's quite typical to have initial values in record
types:

.. code-block:: ada

    type R is record
       F : Some_Field := Call_To_Some_Function;
    end record;

However, the consequences of the above is that any declaration of a instance of
this type without an explicit value for :ada:`V` will issue a call to
:ada:`Call_To_Some_Function`. More subtle issue may arise with elaboration. For
example, it's possible to write:

.. code:: ada compile_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Dynamic_Array

    package Some_Functions is

       function Some_Function_Call return Integer is (2);

       function Some_Other_Function_Call return Integer is (10);

    end Some_Functions;

    with Some_Functions; use Some_Functions;

    package Values is
       A_Start : Integer := Some_Function_Call;
       A_End   : Integer := Some_Other_Function_Call;
    end Values;

    with Values; use Values;

    package P is
       type Arr is array (Integer range A_Start .. A_End) of Integer;
    end P;

It may indeed be appealing to be able to change the values of :ada:`A_Start`
and :ada:`A_End` at startup as to align a series of arrays dynamically. The
consequence, however, is that these values will not be know statically, so any
code that needs to access to boundaries of the array will need to read data
from memory. While it's perfectly fine most of the time, there may be
situations where performances are so critical that static values for array
boundaries must be enforced.

Here's a last case which may also be surprising:

.. code:: ada compile_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Record_With_Arrays

    package P is
       type Arr is array (Integer range <>) of Integer;

       type R (D1, D2 : Integer) is record
          F1 : Arr (1 .. D1);
          F2 : Arr (1 .. D2);
       end record;
    end P;

In the code above, :ada:`R` contains two arrays, :ada:`F1` and :ada:`F2`,
respectively constrained by the discriminant :ada:`D1` and :ada:`D2`. The
consequence is, however, that to access :ada:`F2`, the run-time needs to know
how large :ada:`F1` is, which is dynamically constrained when creating an
instance. Therefore, accessing to :ada:`F2` requires a computation involving
:ada:`D1` which is slower than, let's say, two pointers in an C array that would
point to two different arrays.

Generally speaking, when values are used in data structures, it's useful to
always consider where they're coming from, and if their value is static
(computed by the compiler) or dynamic (only known at run-time). There's nothing
fundamentally wrong with dynamically constrained types, unless they appear is
performance-critical pieces of the application.

Pointers v.s. data copies
-------------------------

In the section about :ref:`pointers <Pointers>`, we mentioned that the Ada
compiler will automatically pass parameters by reference when needed. Also, in
the section about :ref:`by value vs. by reference <By_Value_Vs_By_Reference>`,
we mentioned that scalar types and pointers are passed by value, while record
and array types are always passed by reference. Therefore, unlike C, you don't
have to use access types in Ada to get better performance when passing arrays
or records to subprograms. Using :ada:`out` or :ada:`in out` gives you
equivalent performance and, at the same time, improves the readability of your
code.

Let's look at this example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Passing_Rec_By_Reference_C

    !main.c
    #include <stdio.h>

    struct Data {
        int prev, curr;
    };

    void update(struct Data *d,
                int          v)
    {
        d->prev = d->curr;
        d->curr = v;
    }

    void display(const struct Data *d)
    {
        printf("Prev : %d\n", d->prev);
        printf("Curr : %d\n", d->curr);
    }

    int main(int argc, const char * argv[])
    {
        struct Data D1 = { 0, 1 };

        update (&D1, 3);
        display (&D1);
    }

In this C code example, we're using pointers to pass :c:`D1` as a reference to
:c:`update` and :c:`display`. In contrast, the equivalent code in Ada simply
uses the parameter modes to specify the data flow directions. The mechanisms
used to pass the values do not appear in the source code.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Passing_Rec_By_Reference_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Update_Record is

       type Data is record
          Prev : Integer;
          Curr : Integer;
       end record;

       procedure Update (D : in out Data;
                         V :        Integer) is
       begin
          D.Prev := D.Curr;
          D.Curr := V;
       end Update;

       procedure Display (D : Data) is
       begin
          Put_Line ("Prev: " & Integer'Image (D.Prev));
          Put_Line ("Curr: " & Integer'Image (D.Curr));
       end Display;

       D1 : Data := (0, 1);

    begin
       Update (D1, 3);
       Display (D1);
    end Update_Record;

In the calls to :ada:`Update` and :ada:`Display`, :ada:`D1` is always be passed
by reference. Because no extra copy takes place, we get a performance that is
equivalent to the C version. If we had used arrays in the example above,
:ada:`D1` would have been passed by reference as well:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Passing_Array_By_Reference_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Update_Array is

       type Data_State is (Prev, Curr);
       type Data is array (Data_State) of Integer;

       procedure Update (D : in out Data;
                         V :        Integer) is
       begin
          D (Prev) := D (Curr);
          D (Curr) := V;
       end Update;

       procedure Display (D : Data) is
       begin
          Put_Line ("Prev: " & Integer'Image (D (Prev)));
          Put_Line ("Curr: " & Integer'Image (D (Curr)));
       end Display;

       D1 : Data := (0, 1);

    begin
       Update (D1, 3);
       Display (D1);
    end Update_Array;

Again, no extra copy is performed in the calls to :ada:`Update` and
:ada:`Display`, which gives us optimal performance when dealing with arrays and
avoids the need to use access types to optimize the code.
