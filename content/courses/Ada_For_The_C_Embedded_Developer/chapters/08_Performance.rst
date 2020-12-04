Performance considerations
==========================

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

:code-config:`reset_accumulator=True`

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

.. todo::

    Complete section!


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
result is still in the valid range |mdash| e.g., it shouldn't overflow or
underflow. This is achieved by an overflow check.

Although runtime checks are very useful and should be used as much as possible,
they can also increase the overhead of implementations at certain hot-spots.
For example, checking the index of an array in a sorting algorithm may
significantly decrease its performance. In those cases, suppressing the check
may be an option. We can achieve this suppression by using
:ada:`pragma Suppress (Index_Check)`. For example:

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
access via address zero, requesting the removal of null-access value checks in
the generated code won't prevent them from happening.

It is important to differentiate between required and redundant checks. Let's
consider the following example in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Division_By_Zero

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

.. code:: c manual_chop run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Division_By_Zero_Check

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Division_By_Zero

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Division_By_Zero is
       A   : Integer := 8;
       B   : Integer := 0;
       Res : Integer;
    begin
       Res := A / B;

       Put_Line ("Res = " & Integer'Image (Res));
    end Show_Division_By_Zero;

Similar to the first version of the C code, we're not checking for a potential
division by zero here. In Ada, however, this check is *automatically
introduced* by the language itself. When running the application above, an
exception is raised when the application tries to divide the value in :ada:`A`
by zero. We could introduce exception handling in our example, so that we get
the same message as we did in the second version of the C code:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Division_By_Zero

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
:ref:`this section of the SPARK chapter <Dynamic_Checks_Vs_Formal_Proof>`.
Assertions are user-defined checks that you can add to your code using the
:ada:`pragma Assert`. For example:

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

.. code:: ada project=Courses.Ada_For_C_Embedded_Dev.Performance.Dynamic_Array

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

.. code:: ada run_button project=Courses.Ada_For_C_Embedded_Dev.Performance.Record_With_Arrays

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

.. todo::

    Complete section!
