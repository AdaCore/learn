Performance considerations
==========================

.. include:: ../../../global.txt

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

When comparing the performance of C and Ada code, differences might be
observed. This usually comes from the fact that, while the two piece *appear*
semantically equivalent, they happen to be actually quite different; C code
semantics do not implicitly apply the same run-time checks that Ada does.
This section will present common ways for improving Ada code performance.

Switches and optimizations
--------------------------

Clever use of compilation switches might optimize the performance of an
application significantly. In this section, we'll briefly look into some of
the switches available in the GNAT toolchain.

Optimizations levels
~~~~~~~~~~~~~~~~~~~~

Optimization levels can be found in many compilers for multiple languages. On
the lowest level, the GNAT compiler doesn't optimize the code at all, while at
the higher levels, the compiler analyses the code and optimizes it by removing
unnecessary operations and making the most use of the target processor's
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

Note that the higher the level, the longer the compilation time. For
fast compilation during development phase, unless you're working on
benchmarking algorithms, using ``-O0`` is probably a good idea.

In addition to the levels presented above, GNAT also has the ``-Os`` switch,
which allows for optimizing code and data usage.

.. _Ada_For_Embedded_C_Dev_Inlining:

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

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Inlining

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

Checks
~~~~~~

Ada provides many runtime checks to ensure that the implementation is working
as expected. For example, when accessing an array, we would like to make sure
that we're not accessing a memory position that is not allocated for that
array. This is achieved by an index check.

Another example of runtime check is the verification of valid ranges. For
example, when adding two integer numbers, we would like to ensure that the
result is still in the valid range |mdash| that the value is neither too large
nor too small. This is achieved by an range check. Likewise, arithmetic operations
shouldn't overflow or underflow. This is achieved by an overflow check.

Although runtime checks are very useful and should be used as much as possible,
they can also increase the overhead of implementations at certain hot-spots.
For example, checking the index of an array in a sorting algorithm may
significantly decrease its performance. In those cases, suppressing the check
may be an option. We can achieve this suppression by using
:ada:`pragma Suppress (Index_Check)`. For example:

[Ada]

.. code-block:: ada

    procedure Sort (A : in out Integer_Array) is
       pragma Suppress (Index_Check);
    begin
       --  (implementation removed...)
       null;
    end Sort;

In case of overflow checks, we can use :ada:`pragma Suppress (Overflow_Check)`
to suppress them:

.. code-block:: ada

    function Some_Computation (A, B : Int32) return Int32 is
       pragma Suppress (Overflow_Check);
    begin
       --  (implementation removed...)
       null;
    end Sort;

We can also deactivate overflow checks for integer types using the ``-gnato``
switch when compiling a source-code file with GNAT. In this case, overflow
checks in the whole file are deactivated.

It is also possible to suppress all checks at once using
:ada:`pragma Suppress (All_Checks)`. In addition, GNAT offers a compilation
switch called ``-gnatp``, which has the same effect on the whole file.

Note, however, that this kind of suppression is just a recommendation to the
compiler. There's no guarantee that the compiler will actually suppress any of
the checks because the compiler may not be able to do so |mdash| typically
because the hardware happens to do it. For example, if the machine traps on any
access via address zero, requesting the removal of null access value checks in
the generated code won't prevent the checks from happening.

It is important to differentiate between required and redundant checks. Let's
consider the following example in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Division_By_Zero_C
    :class: c-run-expect-failure

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int a = 8, b = 0, res;

        res = a / b;

        // printing the result
        printf("res = %d\n", res);

        return 0;
    }

Because C doesn't have language-defined checks, as soon as the application
tries to divide a value by zero in :c:`res = a / b`, it'll break |mdash| on
Linux, for example, you may get the following error message by the operating
system: ``Floating point exception (core dumped)``. Therefore, we need to
manually introduce a check for zero before this operation. For example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Division_By_Zero_Check_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int a = 8, b = 0, res;

        if (b != 0) {
            res = a / b;

            // printing the result
            printf("res = %d\n", res);
        }
        else
        {
            // printing error message
            printf("Error: cannot calculate value (division by zero)\n");
        }

        return 0;
    }

This is the corresponding code in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Division_By_Zero_Ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Division_By_Zero is
       A   : Integer := 8;
       B   : Integer := 0;
       Res : Integer;
    begin
       Res := A / B;

       Put_Line ("Res = " & Integer'Image (Res));
    end Show_Division_By_Zero;

Similar to the first version of the C code, we're not explicitly checking for a
potential division by zero here. In Ada, however, this check is *automatically
inserted* by the language itself. When running the application above, an
exception is raised when the application tries to divide the value in :ada:`A`
by zero. We could introduce exception handling in our example, so that we get
the same message as we did in the second version of the C code:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Division_By_Zero_Check_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Division_By_Zero is
       A   : Integer := 8;
       B   : Integer := 0;
       Res : Integer;
    begin
       Res := A / B;

       Put_Line ("Res = " & Integer'Image (Res));
    exception
       when Constraint_Error =>
          Put_Line ("Error: cannot calculate value (division by zero)");
       when others =>
          null;
    end Show_Division_By_Zero;

This example demonstrates that the division check for :ada:`Res := A / B` is
required and shouldn't be suppressed. In contrast, a check is redundant |mdash|
and therefore not required |mdash| when we know that the condition that leads
to a failure can never happen. In many cases, the compiler itself detects
redundant checks and eliminates them (for higher optimization levels).
Therefore, when improving the performance of your application, you should:

#. keep all checks active for most parts of the application;

#. identify the hot-spots of your application;

#. identify which checks haven't been eliminated by the optimizer on these
   hot-spots;

#. identify which of those checks are redundant;

#. only suppress those checks that are redundant, and keep the required ones.

Assertions
~~~~~~~~~~

We've already discussed assertions in
:ref:`this section of the SPARK chapter <Ada_For_Embedded_C_Dev_Dynamic_Checks_Vs_Formal_Proof>`.
Assertions are user-defined checks that you can add to your code using the
:ada:`pragma Assert`. For example:

[Ada]

.. code-block:: ada

    function Some_Computation (A, B : Int32) return Int32 is
       Res : Int32;
    begin
       --  (implementation removed...)

       pragma Assert (Res >= 0);

       return Res;
    end Sort;

Assertions that are specified with :ada:`pragma Assert` are not enabled by
default. You can enable them by setting the assertion policy to *check* |mdash|
using :ada:`pragma Assertion_Policy (Check)` |mdash| or by using the ``-gnata``
switch when compiling with GNAT.

Similar to the checks discussed previously, assertions can generate significant
overhead when used at hot-spots. Restricting those assertions to development
(e.g. debug version) and turning them off on the release version may be an
option. In this case, formal proof |mdash| as discussed in the
:doc:`SPARK chapter <05_SPARK>` |mdash| can help you. By formally proving that
assertions will never fail at run-time, you can safely deactivate them.

Dynamic vs. static structures
-----------------------------

Ada generally speaking provides more ways than C or C++ to write simple dynamic
structures, that is to say structures that have constraints computed after
variables. For example, it's quite typical to have initial values in record
types:

[Ada]

.. code-block:: ada

    type R is record
       F : Some_Field := Call_To_Some_Function;
    end record;

However, the consequences of the above is that any declaration of a instance of
this type without an explicit value for :ada:`F` will issue a call to
:ada:`Call_To_Some_Function`. More subtle issue may arise with elaboration. For
example, it's possible to write:

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Dynamic_Array

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

    package Arr_Def is
       type Arr is array (Integer range A_Start .. A_End) of Integer;
    end Arr_Def;

It may indeed be appealing to be able to change the values of :ada:`A_Start`
and :ada:`A_End` at startup so as to align a series of arrays dynamically. The
consequence, however, is that these values will not be known statically, so any
code that needs to access to boundaries of the array will need to read data
from memory. While it's perfectly fine most of the time, there may be
situations where performances are so critical that static values for array
boundaries must be enforced.

Here's a last case which may also be surprising:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Record_With_Arrays

    package Arr_Def is
       type Arr is array (Integer range <>) of Integer;

       type R (D1, D2 : Integer) is record
          F1 : Arr (1 .. D1);
          F2 : Arr (1 .. D2);
       end record;
    end Arr_Def;

In the code above, :ada:`R` contains two arrays, :ada:`F1` and :ada:`F2`,
respectively constrained by the discriminant :ada:`D1` and :ada:`D2`. The
consequence is, however, that to access :ada:`F2`, the run-time needs to know
how large :ada:`F1` is, which is dynamically constrained when creating an
instance. Therefore, accessing to :ada:`F2` requires a computation involving
:ada:`D1` which is slower than, let's say, two pointers in an C array that
would point to two different arrays.

Generally speaking, when values are used in data structures, it's useful to
always consider where they're coming from, and if their value is static
(computed by the compiler) or dynamic (only known at run-time). There's nothing
fundamentally wrong with dynamically constrained types, unless they appear in
performance-critical pieces of the application.

Pointers vs. data copies
------------------------

In the section about :ref:`pointers <Ada_For_Embedded_C_Dev_Pointers>`, we mentioned that the
Ada compiler will automatically pass parameters by reference when
needed. Let's look into what "when needed" means. The fundamental point
to understand is that the parameter types determine how the parameters
are passed in and/or out. The parameter modes do not control how parameters
are passed.

Specifically, the language standards specifies that scalar types are
always passed by value, and that some other types are always passed by
reference. It would not make sense to make a copy of a task when passing
it as a parameter, for example. So parameters that can be passed
reasonably by value will be, and those that must be passed by reference
will be. That's the safest approach.

But the language also specifies that when the parameter is an array type
or a record type, and the record/array components are all by-value
types, then the compiler decides: it can pass the parameter using either
mechanism. The critical case is when such a parameter is large, e.g., a
large matrix. We don't want the compiler to pass it by value because
that would entail a large copy, and indeed the compiler will not do so.
But if the array or record parameter is small, say the same size as an
address, then it doesn't matter how it is passed and by copy is just as
fast as by reference. That's why the language gives the choice to the
compiler. Although the language does not mandate that large parameters
be passed by reference, any reasonable compiler will do the right thing.

The modes do have an effect, but not in determining how the parameters
are passed. Their effect, for parameters passed by value, is to
determine how many times the value is copied. For :ada:`mode in` and
:ada:`mode out` there is just one copy. For :ada:`mode in out` there
will be two copies, one in each direction.

Therefore, unlike C, you don't have to use access types in Ada to get
better performance when passing arrays or records to subprograms. The
compiler will almost certainly do the right thing for you.

Let's look at this example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Passing_Rec_By_Reference_C

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

        return 0;
    }

In this C code example, we're using pointers to pass :c:`D1` as a reference to
:c:`update` and :c:`display`. In contrast, the equivalent code in Ada simply
uses the parameter modes to specify the data flow directions. The mechanisms
used to pass the values do not appear in the source code.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Passing_Rec_By_Reference_Ada

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

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Passing_Array_By_Reference_Ada

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

Function returns
~~~~~~~~~~~~~~~~

Previously, we've discussed the cost of passing complex records as
arguments to subprograms. We've seen that we don't have to use explicit
access type parameters to get better performance in Ada. In this
section, we'll briefly discuss the cost of function returns.

In general, we can use either procedures or functions to initialize a
data structure. Let's look at this example in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Init_Rec_Proc_And_Func_C

    !main.c
    #include <stdio.h>

    struct Data {
        int prev, curr;
    };

    void init_data(struct Data *d)
    {
        d->prev = 0;
        d->curr = 1;
    }

    struct Data get_init_data()
    {
        struct Data d  = { 0, 1 };

        return d;
    }

    int main(int argc, const char * argv[])
    {
        struct Data D1;

        D1 = get_init_data();

        init_data(&D1);

        return 0;
    }

This code example contains two subprograms that initialize the :c:`Data`
structure:

- :c:`init_data()`, which receives the data structure as a reference (using
  a pointer) and initializes it, and

- :c:`get_init_data()`, which returns the initialized structure.

In C, we generally avoid implementing functions such as :c:`get_init_data()`
because of the extra copy that is needed for the function return.

This is the corresponding implementation in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Init_Rec_Proc_And_Func_Ada

    procedure Init_Record is

       type Data is record
          Prev : Integer;
          Curr : Integer;
       end record;

       procedure Init (D : out Data) is
       begin
          D := (Prev => 0, Curr => 1);
       end Init;

       function Init return Data is
          D : constant Data := (Prev => 0, Curr => 1);
       begin
          return D;
       end Init;

       D1 : Data;

       pragma Unreferenced (D1);
    begin
       D1 := Init;

       Init (D1);
    end Init_Record;

In this example, we have two versions of :ada:`Init`: one using a
procedural form, and the other one using a functional form. Note that,
because of Ada's support for subprogram overloading, we can use the same
name for both subprograms.

The issue is that assignment of a function result entails a copy, just
as if we assigned one variable to another. For example, when assigning a
function result to a constant, the function result is copied into the
memory for the constant. That's what is happening in the above examples
for the initialized variables.

Therefore, in terms of performance, the same recommendations apply: for
large types we should avoid writing functions like the :ada:`Init`
function above. Instead, we should use the procedural form of
:ada:`Init`. The reason is that the compiler necessarily generates a
copy for the :ada:`Init` function, while the :ada:`Init` procedure uses
a reference for the output parameter, so that the actual record
initialization is performed in place in the caller's argument.

An exception to this is when we use functions returning values of
limited types, which by definition do not allow assignment. Here, to
avoid allowing something that would otherwise look suspiciously like an
assignment, the compiler generates the function body so that it builds
the result directly into the object being assigned. No copy takes place.

We could, for example, rewrite the example above using limited types:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Performance.Init_Lim_Rec_Proc_And_Func_Ada

    procedure Init_Limited_Record is

       type Data is limited record
          Prev : Integer;
          Curr : Integer;
       end record;

       function Init return Data is
       begin
          return D : Data do
             D.Prev := 0;
             D.Curr := 1;
          end return;
       end Init;

       D1 : Data := Init;

       pragma Unreferenced (D1);
    begin
       null;
    end Init_Limited_Record;

In this example, :ada:`D1 : Data := Init;` has the same cost as the call to the
procedural form |mdash| :ada:`Init (D1);` |mdash| that we've seen in the
previous example. This is because the assignment is done in place.

Note that limited types require the use of the extended return statements
(:ada:`return ... do ... end return`) in function implementations. Also note
that, because the :ada:`Data` type is limited, we can only use the :ada:`Init`
function in the declaration of :ada:`D1`; a statement in the code such as
:ada:`D1 := Init;` is therefore forbidden.
