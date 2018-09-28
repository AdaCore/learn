:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Recovering Basic Syntactic Guarantees
-------------------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

C is well known for its concise syntax. The flip side of this coint is that it
is also very permissive in terms of syntax, which makes it possible to
unintentionnally abuse the syntax to say something that was not
intended. MISRA-C contains guidelines to:

* distinguish clearly code from comments
* handle specially function parameters and result
* ensure that control structures are not abused

Distinguishing Code and Comments
********************************

The problem arises from block comments in C, starting with ``/*`` and ending
with ``*/``. These comments do not nest with other block comments or with line
comments. For example, consider a block comment surrounding three lines that
each increase variable ``a`` by one:

.. code-block:: c

   /*
   ++a;
   ++a;
   ++a; */

Now consider what happens if the first line gets commented out using a block comment
and the third line gets commented out using a line comment:

.. code-block:: c

   /*
   /* ++a; */
   ++a;
   // ++a; */

The result of commenting out code that was already commented out is that the
second line of code becomes live! Of course, the above example is simplified,
but similar situations do arise in practice, which is the reason for MISRA-C
Directive 4.1 `"Sections of code should not be 'commented out'"`.  This is
reinforced with Rules 3.1 and 3.2 from the section on "Comments" that forbid in
particular the use of ``/*`` inside a comment like we did above.

These situations cannot arise in SPARK, as only line comments are possible,
using ``--``:

.. code-block:: ada

   --  A := A + 1;
   --  A := A + 1;
   --  A := A + 1;

So commenting again the first and third lines does not change the code:

.. code-block:: ada

   --  --  A := A + 1;
   --  A := A + 1;
   --  --  A := A + 1;


Handling Specially Function Parameters and Result
*************************************************

Handling the Result of Function Calls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible in C to ignore the result of a function call, either implicitly
or explicitly by converting the result to ``void``:

.. code-block:: c

   f();
   (void)f();

This is particularly inadapted when the function returns an error status, as
the caller is then ignoring the possibility of errors in the callee. This is
why MISRA-C Directive 4.7 forbids that case: `"If a function returns error
information, then that error information shall be tested"`. In the general case
of a function returning a result which is not an error status, MISRA-C Rule
17.7 states that `"The value returned by a function having non-void return type
shall be used"`, where an explicit conversion to ``void`` counts as a use.

In SPARK, the result of a function call must be assigned to a variable,
contrary to procedures which are the equivalent to void-returning functions
in C. SPARK analysis also checks that the result of the function is `really`
used to influence an output of the calling subprogram. For example, the first
two calls to ``F`` in the following are detected as unused, even if the result
of the function call is always assigned to a variable, which is itself used in
the second case:

.. code:: ada spark-flow

    package Fun is
       function F return Integer is (1);
    end Fun;

    with Fun; use Fun;

    procedure Use_F (Z : out Integer) is
       X, Y : Integer;
    begin
       X := F;

       Y := F;
       X := Y;

       Z := F;
    end Use_F;

Only the result of the third call is used to influence the value of an output
of ``Use_F``, here the output parameter ``Z`` of the procedure.

Handling Function Parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In C, function parameters are treated as local variables of the function. They
can be modified, but these modifications won't be visible outside the
function. This is an opportunity for mistakes. For example, the following code
which appears to swap the values of its parameters has in reality no effect:

.. code-block:: c

   void swap (int x, int y) {
      int tmp = x;
      x = y;
      y = tmp;
   }

MISRA-C Rule 17.8 prevents such mistakes by stating that `"A function parameter
should not be modified"`.

No such rule is needed in SPARK, as function parameters are only inputs so
cannot be modified, and procedure parameters have a `mode` defining whether
they can be modified or not. Only parameters of mode `out` or `in out` can be
modified, and their modification is visible at the calling site. For example,
assigning to parameter of mode `in` (the default parameter mode which can also
be ommitted) results in compilation errors:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Swap (X, Y : Integer) is
       Tmp : Integer := X;
    begin
       X := Y;  --  ERROR
       Y := Tmp;  --  ERROR
    end Swap;

The correct version of ``Swap`` in SPARK takes parameters of mode `in out`:

.. code:: ada

    procedure Swap (X, Y : in out Integer) is
       Tmp : Integer := X;
    begin
       X := Y;
       Y := Tmp;
    end Swap;

Ensuring Control Structures Are Not Abused
******************************************

The previous issue with the resulf of function calls being ignored is an
example of a control structure being abused, due to the permissive syntax
of C. There are many such examples, and MISRA-C contains a number of guidelines
to prevent such abuse.

.. _Preventing the Semicolon Mistake:

Preventing the Semicolon Mistake
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Because a semicolon can act as a statement, and because if-statement and loops
accept a simple statement (among them the semicolon) as body, then insertion of
a single semicolon can completely change the behavior of the code below:

.. code-block:: c

   int main() {
      if (1)
         while (1)
            return 0;
      return 1;
   }

As written, the code above returns with status 0. If a semicolon is added after
the first line (``if (1);``), then the code returns with status 1. If a
semicolon is added instead after the second line (``while (1);``), then the
code does not return. To prevent this, MISRA-C Rule 15.6 states that `"The body
of an iteration-statement or a selection-statement shall be a compound
statement"` so that the code above must be written:

.. code-block:: c

   int main() {
      if (1) {
         while (1) {
            return 0;
         }
      }
      return 1;
   }

Then, no addition of a single semicolon can change the behavior of the code.

In SPARK, the semicolon is not a statement by itself, only a marker of end of
statement. The null statement is an explicit ``null;`` and all blocks of
statements have explicit begin and end markers, which defeats mistakes like the
ones that are possible in C. The above C code is written as follows in SPARK:

.. code:: ada

    function Main return Integer is
    begin
       if True then
          while True loop
             return 0;
          end loop;
       end if;
       return 1;
    end Main;

Avoiding Complex Switch
^^^^^^^^^^^^^^^^^^^^^^^

Switch statements are well-known for being easily misused. Control can jump
anywhere there is a case in the body of the switch, which in C can be before
any statement contained in the body of the switch. At the end of the treatment
associated to a case, execution continues with the code that follows unless a
break is uncountered. This is a recipe for mistakes, and MISRA-C enforces a
simpler `well-formed` syntax for switch statements defined in Rule 16.1: `"All
switch statements shall be well-formed"`.

The other rules of the section on "Switch statements" go on detailing
individual consequences of Rule 16.1. For example Rule 16.3 forbids the
fall-through from one case to the next: `"An unconditional break statement
shall terminate every switch-clause"`. As another example Rule 16.4 mandates
the presence of a default case to handle cases not taken into account
explicitly: `"Every switch statement shall have a default label"`.

Switch statements in SPARK have already a simpler and more robust structure,
with execution automatically exiting the switch after a case is handled, and
the compiler checking that the cases to handle are disjoint (like in C) and
complete (unlike in C). So the following code is rejected by the compiler:

.. code:: ada
    :class: ada-expect-compile-error

    package Sign_Domain is

       type Sign is (Negative, Zero, Positive);

       function Opposite (A : Sign) return Sign is
         (case A is  --  ERROR
             when Negative => Positive,
             when Positive => Negative);

       function Multiply (A, B : Sign) return Sign is
         (case A is
             when Negative        => Opposite (B),
             when Zero | Positive => Zero,
             when Positive        => B);  --  ERROR

       procedure Get_Sign (X : Integer; S : out Sign);

    end Sign_Domain;

    package body Sign_Domain is

       procedure Get_Sign (X : Integer; S : out Sign) is
       begin
          case X is
             when 0 => S := Zero;
             when others => S := Negative;  --  ERROR
             when 1 .. Integer'Last => S := Positive;
          end case;
       end Get_Sign;

    end Sign_Domain;

The error in function ``Opposite`` is that the cases do not cover all values of
the expression being switched over. Here, ``A`` is of enumeration type
``Sign``, so all three values of the enumeration must be covered.

The error in function ``Multiply`` is that the case for ``Positive`` is covered
twice, in the second and the third cases. This is not allowed.

The error in procedure ``Get_Sign`` is that the ``others`` case (the equivalent
of C ``default`` case) should come last. Note that an ``others`` case would be
useless in ``Opposite`` and ``Multiply``, as the compiler already checks that
all cases are covered.

Similar rules applied above to both case-expressions as in functions
``Opposite`` and ``Multiply`` and in case-statements as in procedure
``Get_Sign``. Here is a correct version of the same code:

.. code:: ada

    package Sign_Domain is

       type Sign is (Negative, Zero, Positive);

       function Opposite (A : Sign) return Sign is
         (case A is
             when Negative => Positive,
             when Zero     => Zero,
             when Positive => Negative);

       function Multiply (A, B : Sign) return Sign is
         (case A is
             when Negative => Opposite (B),
             when Zero     => Zero,
             when Positive => B);

       procedure Get_Sign (X : Integer; S : out Sign);

    end Sign_Domain;

    package body Sign_Domain is

       procedure Get_Sign (X : Integer; S : out Sign) is
       begin
          case X is
             when 0 => S := Zero;
             when 1 .. Integer'Last => S := Positive;
             when others => S := Negative;
          end case;
       end Get_Sign;

    end Sign_Domain;

Avoiding Complex Loops
^^^^^^^^^^^^^^^^^^^^^^

Similarly to C switches, for-loops in C can become unreadable. MISRA-C thus
enforces similarly a simpler `well-formed` syntax for for-loops defined in Rule
14.2: `"A for loop shall be well-formed"`. The main effect of this
simplification is that for-loops in C look like for-loops in SPARK, with a
scalar `loop counter` being incremented or decremented. Section 8.14 defined
precisely what a loop counter is:

#. It has a scalar type;
#. Its value varies monotonically on each iteration of a given instance of a loop; and
#. It is involved in a decision to exit the loop.

In particular, Rule 14.2 forbids any modification of the loop counter inside
the loop body. Let's look at the example used in MISRA-C:2012 to illustrate
this rule:

.. code-block:: c

   bool_t flag = false;

   for ( int16_t i = 0; ( i < 5 ) && !flag; i++ )
   {
     if ( C )
     {
       flag = true; /* Compliant - allows early termination of loop */
     }

     i = i + 3;     /* Non-compliant - altering the loop counter */
   }

The equivalent code in SPARK does not compile due to the attempt at modifying
the value of the loop counter:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Well_Formed_Loop (C : Boolean) is
       Flag : Boolean := False;
    begin
       for I in 0 .. 4 loop
          exit when not Flag;

          if C then
             Flag := True;
          end if;

          I := I + 3;  --  ERROR
       end loop;
    end Well_Formed_Loop;

Removing the problematic line leads to a valid SPARK program. Note that the
additional condition being tested in the C for-loop has been moved to a
separate exit statement at the start of the loop body in SPARK.

SPARK loops can be increasing as above, or decreasing:

.. code-block:: ada

      for I in reverse 0 .. 4 loop

SPARK loops can iterate over integers as above, or over other scalar types like
enumerations:

.. code-block:: ada

      type Sign is (Negative, Zero, Positive);

      for S in Sign loop

Avoiding the Dangling Else Issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In its effort to minimize the number of keystrokes, C does not provide a
closing symbol for an if-statement. This makes it possible to write the
following code which appears to try to return the absolute value of its
argument, while it actually returns its opposite:

.. code-block:: c

   #include <stdio.h>

   int absval (int x) {
      int result = x;
      if (x >= 0)
         if (x == 0)
            result = 0;
      else
         result = -x;
      return result;
   }

   int main() {
      printf("absval(5) = %d\n", absval(5));
      printf("absval(0) = %d\n", absval(0));
      printf("absval(-10) = %d\n", absval(-10));
   }

The warning issued by GCC or LLVM with option ``-Wdangling-else`` (implied by
``-Wall``) gives a clue about the problem: although the ``else`` branch is
written above as completing the outter if-statement, it completes in fact the
inner if-statement. This is a common parsing conflict, which is resolved in C
by binding the ``else`` with the innermost if-statement (in parsing theory,
preferring shift to reduce to solve the shift-reduce conflict).

MISRA-C avoids that problem by requiring in Rule 15.6 that `"The body of an
iteration-statement or a selection-statement shall be a compound
statement"`. Yes, that's the same rule as the one we saw before for
:ref:`Preventing the Semicolon Mistake`. So the code for ``absval`` must be
written:

.. code-block:: c

   #include <stdio.h>

   int absval (int x) {
      int result = x;
      if (x >= 0) {
         if (x == 0) {
            result = 0;
         }
      } else {
         result = -x;
      }
      return result;
   }

   int main() {
      printf("absval(5) = %d\n", absval(5));
      printf("absval(0) = %d\n", absval(0));
      printf("absval(-10) = %d\n", absval(-10));
   }

which has the expected behavior.

In SPARK, if-statements have an end marker ``end if;`` so the dangling-else
problem cannot arise. The above C code is written as follows in SPARK:

.. code:: ada

    function Absval (X : Integer) return Integer is
       Result : Integer := X;
    begin
       if X >= 0 then
          if X = 0 then
             Result := 0;
          end if;
       else
          Result := -X;
       end if;
       return Result;
    end Absval;
