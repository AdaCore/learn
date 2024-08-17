Enforcing Basic Syntactic Guarantees
------------------------------------

.. include:: ../../global.txt

C's syntax is concise but also very permissive, which makes it easy
to write programs whose effect is not what was intended.
MISRA C contains guidelines to:

* clearly distinguish code from comments
* specially handle function parameters and result
* ensure that control structures are not abused

Distinguishing Code and Comments
********************************

The problem arises from block comments in C, starting with ``/*`` and ending
with ``*/``. These comments do not nest with other block comments or with line
comments. For example, consider a block comment surrounding three lines that
each increase variable :c:`a` by one:

.. code-block:: c

   /*
   ++a;
   ++a;
   ++a; */

Now consider what happens if the first line is commented out using a block
comment and the third line is commented out using a line comment (also known
as a C++ style comment, allowed in C since C99):

.. code-block:: c

   /*
   /* ++a; */
   ++a;
   // ++a; */

The result of commenting out code that was already commented out is that the
second line of code becomes live! Of course, the above example is simplified,
but similar situations do arise in practice, which is the reason for MISRA C
Directive 4.1 `"Sections of code should not be 'commented out'"`.  This is
reinforced with Rules 3.1 and 3.2 from the section on "Comments" that forbid in
particular the use of ``/*`` inside a comment like we did above.

These situations cannot arise in SPARK (or in Ada), as only line comments are
permitted, using :ada:`--`:

.. code-block:: ada

   --  A := A + 1;
   --  A := A + 1;
   --  A := A + 1;

So commenting again the first and third lines does not change the effect:

.. code-block:: ada

   --  --  A := A + 1;
   --  A := A + 1;
   --  --  A := A + 1;


Specially Handling Function Parameters and Result
*************************************************

Handling the Result of Function Calls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible in C to ignore the result of a function call, either implicitly
or else explicitly by converting the result to :c:`void`:

.. code-block:: c

   f();
   (void)f();

This is particularly dangerous when the function returns an error status, as
the caller is then ignoring the possibility of errors in the callee. Thus the
MISRA C Directive 4.7: `"If a function returns error
information, then that error information shall be tested"`. In the general case
of a function returning a result which is not an error status, MISRA C Rule
17.7 states that `"The value returned by a function having non-void return type
shall be used"`, where an explicit conversion to :c:`void` counts as a use.

In SPARK, as in Ada, the result of a function call must be used, for example by assigning
it to a variable or by passing it as a parameter, in
contrast with procedures (which are equivalent to void-returning functions
in C). SPARK analysis also checks that the result of the function is actually
used to influence an output of the calling subprogram. For example, the first
two calls to :ada:`F` in the following are detected as unused, even though the result
of the function call is assigned to a variable, which is itself used in
the second case:

.. code:: ada prove_flow_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Func_Return

    package Fun is
       function F return Integer is (1);
    end Fun;

    procedure Use_F (Z : out Integer);

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
of :ada:`Use_F`, here the output parameter :ada:`Z` of the procedure.

Handling Function Parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In C, function parameters are treated as local variables of the function. They
can be modified, but these modifications won't be visible outside the
function. This is an opportunity for mistakes. For example, the following code,
which appears to swap the values of its parameters, has in reality no effect:

.. code-block:: c

   void swap (int x, int y) {
      int tmp = x;
      x = y;
      y = tmp;
   }

MISRA C Rule 17.8 prevents such mistakes by stating that `"A function parameter
should not be modified"`.

No such rule is needed in SPARK, since function parameters are only inputs so
cannot be modified, and procedure parameters have a *mode* defining whether
they can be modified or not. Only parameters of mode :ada:`out` or ada:`in out`
can be modified |mdash| and these are prohibited from functions in SPARK
|mdash| and their modification is visible at the calling site. For example,
assigning to a parameter of mode :ada:`in` (the default parameter mode if
omitted) results in compilation errors:

.. code:: ada compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Swap
    :class: ada-expect-compile-error

    procedure Swap (X, Y : Integer);

    procedure Swap (X, Y : Integer) is
       Tmp : Integer := X;
    begin
       X := Y;  --  ERROR
       Y := Tmp;  --  ERROR
    end Swap;

Here is the output of AdaCore's GNAT compiler:

::

        1.     procedure Swap (X, Y : Integer) is
        2.        Tmp : Integer := X;
        3.     begin
        4.        X := Y;  --  ERROR
                  |
           >>> assignment to "in" mode parameter not allowed

        5.        Y := Tmp;  --  ERROR
                  |
           >>> assignment to "in" mode parameter not allowed

        6.     end Swap;

The correct version of :ada:`Swap` in SPARK takes parameters of mode
:ada:`in out`:

.. code:: ada prove_flow_report_all_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Swap

    procedure Swap (X, Y : in out Integer);

    procedure Swap (X, Y : in out Integer) is
       Tmp : constant Integer := X;
    begin
       X := Y;
       Y := Tmp;
    end Swap;

Ensuring Control Structures Are Not Abused
******************************************

The previous issue (ignoring the result of a function call) is an
example of a control structure being abused, due to the permissive syntax
of C. There are many such examples, and MISRA C contains a number of guidelines
to prevent such abuse.

.. _SPARK_For_MISRA_C_Dev_Preventing_The_Semicolon_Mistake:

Preventing the Semicolon Mistake
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Because a semicolon can act as a statement, and because an if-statement and a loop
accept a simple statement (possibly only a semicolon) as body, inserting
a single semicolon can completely change the behavior of the code:

.. code-block:: c

   int func() {
      if (0)
         return 1;
      while (1)
         return 0;
      return 0;
   }

As written, the code above returns with status 0. If a semicolon is added after
the first line (:c:`if (0);`), then the code returns with status 1. If a
semicolon is added instead after the third line (:c:`while (1);`), then the
code does not return. To prevent such surprises, MISRA C Rule 15.6 states that
`"The body of an iteration-statement or a selection-statement shall be a compound
statement"` so that the code above must be written:

.. code-block:: c

   int func() {
      if (0) {
         return 1;
      }
      while (1) {
         return 0;
      }
      return 0;
   }

Note that adding a semicolon after the test of the :c:`if` or :c:`while`
statement has the same effect as before! But doing so would violate MISRA C
Rule 15.6.

In SPARK, the semicolon is not a statement by itself, but rather a marker that
terminates a statement. The null statement is an explicit :ada:`null;`, and all
blocks of statements have explicit :ada:`begin` and :ada:`end` markers, which
prevents mistakes that are possible in C. The SPARK (also Ada) version of the
above C code is as follows:

.. code:: ada no_button gnat=12.2.0-1 gnatprove=12.1.0-1 project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Semicolon
    :class: ada-compile, ada-prove-flow-report-all

    function Func return Integer;

    function Func return Integer is
    begin
       if False then
          return 1;
       end if;
       while True loop
          return 0;
       end loop;
       return 0;
    end Func;

Avoiding Complex Switch Statements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Switch statements are well-known for being easily misused. Control can jump
to any case section in the body of the switch, which in C can be before
any statement contained in the body of the switch. At the end of the sequence
of statements associated with a case, execution continues with the code that
follows unless a break is encountered. This is a recipe for mistakes, and
MISRA C enforces a simpler `well-formed` syntax for switch statements defined
in Rule 16.1: `"All switch statements shall be well-formed"`.

The other rules in the section on "Switch statements" go on detailing
individual consequences of Rule 16.1. For example Rule 16.3 forbids the
fall-through from one case to the next: `"An unconditional break statement
shall terminate every switch-clause"`. As another example, Rule 16.4 mandates
the presence of a default case to handle cases not taken into account
explicitly: `"Every switch statement shall have a default label"`.

The analog of the C switch statements in SPARK (and in Ada) is the case statement. This statement
has a simpler and more robust structure than the C switch,
with control automatically exiting after one of the case alternatives is executed, and
the compiler checking that the alternatives are disjoint (like in C) and
complete (unlike in C). So the following code is rejected by the compiler:

.. code:: ada run_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Case_Statement
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

The error in function :ada:`Opposite` is that the :ada:`when` choices do not cover
all values of the target expression. Here, :ada:`A` is of the enumeration type
:ada:`Sign`, so all three values of the enumeration must be covered.

The error in function :ada:`Multiply` is that :ada:`Positive` is covered
twice, in the second and the third alternatives. This is not allowed.

The error in procedure :ada:`Get_Sign` is that the :ada:`others` choice (the equivalent
of C :c:`default` case) must come last. Note that an :ada:`others` choice would be
useless in :ada:`Opposite` and :ada:`Multiply`, as all :ada:`Sign` values are covered.

Here is a correct version of the same code:

.. code:: ada prove_flow_report_all_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Case_Statement

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

Similarly to C switches, for-loops in C can become unreadable. MISRA C thus
enforces a simpler `well-formed` syntax for for-loops, defined in Rule
14.2: `"A for loop shall be well-formed"`. The main effect of this
simplification is that for-loops in C look like for-loops in SPARK (and in Ada), with a
`loop counter` that is incremented or decremented at each iteration. Section 8.14 defines
precisely what a loop counter is:

#. It has a scalar type;
#. Its value varies monotonically on each loop iteration; and
#. It is used in a decision to exit the loop.

In particular, Rule 14.2 forbids any modification of the loop counter inside
the loop body. Here's the example used in MISRA C:2012 to illustrate
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

The equivalent SPARK (and Ada) code does not compile, because of the attempt
to modify the value of the loop counter:

.. code:: ada run_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Well_Formed_Loop
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

Removing the problematic line leads to a valid program. Note that the
additional condition being tested in the C for-loop has been moved to a
separate exit statement at the start of the loop body.

SPARK (and Ada) loops can increase (or, with explicit syntax, decrease) the
loop counter by 1 at each iteration.

.. code-block:: ada

      for I in reverse 0 .. 4 loop
         ... -- Successive values of I are 4, 3, 2, 1, 0
      end loop;

SPARK loops can iterate over any discrete type; i.e., integers as above or enumerations:

.. code-block:: ada

      type Sign is (Negative, Zero, Positive);

      for S in Sign loop
        ...
      end loop;

Avoiding the Dangling Else Issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

C does not provide a
closing symbol for an if-statement. This makes it possible to write the
following code, which appears to try to return the absolute value of its
argument, while it actually does the opposite:

.. code:: c run_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Dangling_Else_C

   !main.c
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
``-Wall``) gives a clue about the problem: although the :c:`else` branch is
written as though it completes the outer if-statement, in fact it completes the
inner if-statement.

MISRA C Rule 15.6 avoids the problem: `"The body of an
iteration-statement or a selection-statement shall be a compound
statement"`. That's the same rule as the one shown earlier for
:ref:`SPARK_For_MISRA_C_Dev_Preventing_The_Semicolon_Mistake`. So the code for :c:`absval` must be
written:

.. code:: c run_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Dangling_Else_MISRA_C

   !main.c
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

In SPARK (as in Ada), each if-statement has a matching end marker :c:`end if;`
so the dangling-else problem cannot arise. The above C code is written as follows:

.. code:: ada prove_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Syntactic_Guarantees.Dangling_Else_Ada
    :class: ada-expect-prove-error

    function Absval (X : Integer) return Integer;

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

Interestingly, SPARK analysis detects here that the negation operation on line
9 might overflow. That's an example of runtime error detection which will be
covered in the chapter on :ref:`SPARK_For_MISRA_C_Dev_Detecting_Undefined_Behavior`.
