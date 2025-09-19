Controlling Side Effects
------------------------

.. include:: ../../../global.txt

As with most programming languages, C allows side effects in expressions. This
leads to subtle issues about conflicting side effects, when subexpressions of
the same expression read/write the same variable.

Preventing Undefined Behavior
*****************************

Conflicting side effects are a kind of undefined behavior; the C Standard (section
6.5) defines the concept as follows:

   `"Between two sequence points, an object is modified more than once, or is
   modified and the prior value is read other than to determine the value to
   be stored"`

This legalistic wording is somewhat opaque, but the notion of sequence points
is summarized in Annex C of the C90 and C99 standards. MISRA C repeats these
conditions in the Amplification of Rule 13.2, including the read of a volatile
variable as a side effect similar to writing a variable.

This rule is undecidable, so MISRA C completes it with two rules that provide
simpler restrictions preventing some side effects in expressions, thus reducing
the potential for undefined behavior:

- Rule 13.3: `"A full expression containing an increment
  (++) or decrement (--) operator should have no other potential side effects
  other than that caused by the increment or decrement operator"`.

- Rule 13.4: `"The result of an assignment operator should
  not be used"`.

In practice, conflicting side effects usually manifest themselves as
portability issues, since the result of the evaluation of an expression depends on
the order in which a compiler decides to evaluate its subexpressions. So
changing the compiler version or the target platform might lead to a different
behavior of the application.

To reduce the dependency on evaluation order, MISRA C
Rule 13.1 states: `"Initializer lists shall not contain persistent
side effects"`. This case is theoretically different from the previously
mentioned conflicting side effects, because initializers that comprise an
initializer list are separated by sequence points, so there is no risk of
undefined behavior if two initializers have conflicting side effects. But
given that initializers are executed in an unspecified order, the result
of a conflict is potentially as damaging for the application.

Reducing Programmer Confusion
*****************************

Even in cases with no undefined or unspecified behavior, expressions with
multiple side effects can be confusing to programmers reading or maintaining
the code. This problem arises in particular with C's increment and decrement
operators that can be applied prior to or after the expression evaluation,
and with the assignment operator :c:`=` in C since it can easily be mistaken
for equality. Thus MISRA C forbids the use of the
increment / decrement (Rule 13.3) and assignment (Rule 13.4) operators in
expressions that have other potential side effects.

In other cases, the presence of expressions with side effects might be
confusing, if the programmer wrongly thinks that the side effects are
guaranteed to occur. Consider the function :c:`decrease_until_one_is_null`
below, which decreases both arguments until one is null:

.. code:: c run_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.Side_Effect_C

   !main.c
   #include <stdio.h>

   void decrease_until_one_is_null (int *x, int *y) {
      if (x == 0 || y == 0) {
         return;
      }
      while (--*x != 0 && --*y != 0) {
         // nothing
      }
   }

   int main() {
      int x = 42, y = 42;
      decrease_until_one_is_null (&x, &y);
      printf("x = %d, y = %d\n", x, y);
      return 0;
   }

The program produces the following output:

::

      x = 0, y = 1


I.e., starting from the same value 42 for both :c:`x` and :c:`y`, only
:c:`x` has reached the value zero after :c:`decrease_until_one_is_null`
returns. The reason is that the side effect on :c:`y` is performed only
conditionally. To avoid such surprises, MISRA C Rule 13.5 states:
`"The right hand operand of a logical && or || operator shall not contain
persistent side effects"`; this rule forbids the code above.

MISRA C Rule 13.6 similarly states: `"The operand of the sizeof operator
shall not contain any expression which has potential side effects"`. Indeed,
the operand of :c:`sizeof` is evaluated only in rare situations, and only
according to C99 rules, which makes any side effect in such an operand a
likely mistake.

Side Effects and SPARK
**********************

In SPARK, expressions cannot have side effects; only statements can. In
particular, there are no increment/decrement operators, and no assignment
operator. There is instead an assignment statement, whose syntax using :ada:`:=`
clearly distinguishes it from equality (using :ada:`=`). And in any event an
expression is not allowed as a statement and this a construct such as
:ada:`X = Y;` would be illegal. Here is how a variable :ada:`X` can be assigned,
incremented and decremented:

.. code-block:: ada

   X := 1;
   X := X + 1;
   X := X - 1;

There are two possible side effects when evaluating an expression:

- a read of a volatile variable

- a side effect occurring inside a function that the expression calls

Reads of volatile variables in SPARK are restricted to appear immediately at
statement level, so the following is not allowed:

.. code:: ada prove_flow_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.Volatile_Read_1
    :class: ada-expect-prove-error

    package Volatile_Read is
       X : Integer with Volatile;
       procedure P (Y : out Integer);
    end Volatile_Read;

    package body Volatile_Read is
       procedure P (Y : out Integer) is
       begin
          Y := X - X;  --  ERROR
       end P;
    end Volatile_Read;

Instead, every read of a volatile variable must occur immediately before being
assigned to another variable, as follows:

.. code:: ada prove_flow_report_all_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.Volatile_Read_2

    package Volatile_Read is
       X : Integer with Volatile;
       procedure P (Y : out Integer);
    end Volatile_Read;

    package body Volatile_Read is
       procedure P (Y : out Integer) is
          X1 : constant Integer := X;
          X2 : constant Integer := X;
       begin
          Y := X1 - X2;
       end P;
    end Volatile_Read;

Note here that the order of capture of the volatile value of :ada:`X` might be
significant. For example, :ada:`X` might denote a quantity which only increases,
like clock time, so that the above expression :ada:`X1 - X2` would always be
negative or zero.

Even more significantly, functions in SPARK cannot have side effects; only
procedures can. The only effect of a SPARK function is the computation of a
result from its inputs, which may be passed as parameters or as global
variables. In particular, SPARK functions cannot have :ada:`out` or :ada:`in out`
parameters:

.. code:: ada prove_flow_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.Function_With_Out_Param
    :class: ada-expect-prove-error

    function Bad_Function (X, Y : Integer; Sum, Max : out Integer) return Boolean;
    --  ERROR, since "out" parameters are not allowed

More generally, SPARK does not allow functions that have a side effect
in addition to returning their result, as is typical of many idioms in other
languages, for example when setting a new value and returning the previous one:

.. code:: ada prove_flow_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.Side_Effect_Ada
    :class: ada-expect-prove-error

    package Bad_Functions is
       function Set (V : Integer) return Integer;
       function Get return Integer;
    end Bad_Functions;

    package body Bad_Functions is

       Value : Integer := 0;

       function Set (V : Integer) return Integer is
          Previous : constant Integer := Value;
       begin
          Value := V;  --  ERROR
          return Previous;
       end Set;

       function Get return Integer is (Value);

    end Bad_Functions;

GNATprove detects that function :ada:`Set` has a side effect on global variable
:ada:`Value` and issues an error. The correct idiom in SPARK for such a case is to
use a procedure with an :ada:`out` parameter to return the desired result:

.. code:: ada prove_flow_report_all_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Side_Effect.No_Side_Effect_Ada

    package Ok_Subprograms is
       procedure Set (V : Integer; Prev : out Integer);
       function Get return Integer;
    end Ok_Subprograms;

    package body Ok_Subprograms is

       Value : Integer := 0;

       procedure Set (V : Integer; Prev : out Integer) is
       begin
          Prev := Value;
          Value := V;
       end Set;

       function Get return Integer is (Value);

    end Ok_Subprograms;

With the above restrictions in SPARK, none of the conflicts of side effects
that can occur in C can occur in SPARK, and this is guaranteed by flow analysis.
