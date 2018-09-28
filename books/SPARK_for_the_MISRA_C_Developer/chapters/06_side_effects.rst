:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Controlling Side-Effects
------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

As most programming languages do, C allows side-effects in expressions. This
leads to subtle issues about conflicting side-effects, when sub-expressions of
the same expression read/write the same variable.

Preventing Undefined Behavior
*****************************

It is a case of undefined behavior in C to have conflicting side-effects, which
is defined in C Standard section 6.5 as follows:

   `"Between two sequence points, an object is modified more than once, or is
   modified and the prior value is read other than to determine the value to
   be stored"`

If you understood the above sentence, you're probably a member of the C
Standard committee. Otherwise, the notion of sequence points is summarized in
Annex C of the C90 and C99 standards. MISRA-C repeats these conditions in the
Amplification of Rule 13.2, including the read of volatile variables as
side-effects similar to writing a variable.

This rule is undecidable, so MISRA-C completes it with two rules that provide
simpler restrictions preventing some side-effects in expressions, thus reducing
the potential for undefined behavior:

- MISRA-C Rule 13.3 states that `"A full expression containing an increment
  (++) or decrement (--) operator should have no other potential side effects
  other than that caused by the increment or decrement operator"`.

- MISRA-C Rule 13.4 states that `"The result of an assignment operator should
  not be used"`.

In practice, conflicting side-effects usually manifest themselves as
portability issues, as the result of the evaluation of an expression depends on
the order in which a compiler decides to evaluate its sub-expressions. So
changing the compiler version or the target platform might lead to a different
behavior of the application.

This is precisely to reduce the dependency on evaluation order that MISRA-C
Rule 13.1 states that `"Initializer lists shall not contain persistent
side-effects"`. This case is theoretically different from the previously
mentioned conflicting side-effects, because initializers that compose an
initializer list are separated by sequence points, so there is no risk of
undefined behavior if two initializers have conflicting side-effects. But given
that initializers are executed in an unspecified order, the result of a
conflict is potentially as damaging for the application.

Reducing Programmer Confusion
*****************************

Even in cases where there are no undefined or unspecified behavior, it may be
confusing for programmers to have expressions with multiple side-effects, in
particular with increment/decrement operators in C that can be applied prior to
or after the evaluation, or with assignment operator in C that can easily be
confused for the equality operator. Thus MISRA-C forbids the use of
increment/decrement (Rule 13.3) or assignment (Rule 13.4) operators in
expressions that have other potential side-effects.

In other cases, the presence of expressions with side-effects might be
deceiving, if the programmer wrongly thinks that the side-effects are
guaranteed to happen. Consider the function ``decrease_until_one_is_null``
below, which decreases both arguments until one is null:

.. code-block:: c

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

Starting from the same value 42 for both ``x`` and ``y``, we see that only
``x`` has reached the value zero after ``decrease_until_one_is_null``
returns. The reason is that the side effect on ``y`` is performed only
conditionally. To avoid such surprises, MISRA-C Rule 13.5 states that `"The
right hand operand of a logical && or || operator shall not contain persistent
side-effects"`, which forbids the code above.

MISRA-C Rule 13.6 similarly states that `"The operand of the sizeof operator
shall not contain any expression which has potential side-effects"`. Indeed,
the operand of ``sizeof`` is evaluated in very rare occasions, and only
according to C99 rules, which makes any side-effect in such an operand a likely
mistake.

Control of Side-Effects in SPARK
********************************

In SPARK, expressions cannot have side-effects. Only statements can. In
particular, there are no increment/decrement operators, and no assignment
operator. There is instead an assignment statement using token ``:=`` to
clearly distinguish it from an equality using token ``=`` which itself cannot
appear as a statement anyway. Here is how ``X`` is assigned, incremented and
decremented:

.. code-block:: ada

   X := 1;
   X := X + 1;
   X := X - 1;

There are two possible side-effects when evaluating an expression:

- a read of a volatile variable

- a side-effect occurring inside a function called

Reads of volatile variables in SPARK are restricted to appear immediately at
statement level, so its's not allowed to write:

.. code:: ada spark-flow

    package Volatile_Read is
       X : Integer with Volatile;
       procedure P (Y : out Integer);
    end Volatile_Read;

    package body Volatile_Read is
       procedure P (Y : out Integer) is
       begin
          Y := X - X;
       end P;
    end Volatile_Read;

Instead, every read of a volatile variable must occur immediately before being
assigned to another variable, as follows:

.. code:: ada spark-flow

    package Volatile_Read is
       X : Integer with Volatile;
       procedure P (Y : out Integer);
    end Volatile_Read;

    package body Volatile_Read is
       procedure P (Y : out Integer) is
          X1 : Integer := X;
          X2 : Integer := X;
       begin
          Y := X1 - X2;
       end P;
    end Volatile_Read;

Note here that the order of capture of the volatile value of ``X`` might be
significant. For example, ``X`` might denote a quantity which only increases,
like clock time, so that the above expression ``X1 - X2`` would always be
negative or null.

Even more significantly, functions in SPARK cannot have side-effects. Only
procedures can. The only effect of SPARK functions is the computation of a
result from their inputs, passed both as parameters or as global variables. In
particular, SPARK functions cannot have output parameters:

.. code-block:: ada

   function Bad_Function (X, Y : Integer; Sum, Max : out Integer) return Boolean;

More generally, it is not possible to write functions that have a side-effect
in addition to returning their result, as is typical of many idioms in other
languages, for example when setting a new value and returning the previous one:

.. code:: ada spark-flow

    package Bad_Functions is
       function Set (V : Integer) return Integer;
       function Get return Integer;
    end Bad_Functions;

    package body Bad_Functions is

       Value : Integer := 0;

       function Set (V : Integer) return Integer is
          Previous : constant Integer := Value;
       begin
          Value := V;
          return Previous;
       end Set;

       function Get return Integer is (Value);

    end Bad_Functions;

GNATprove computes that function ``Set`` has a side-effect on global variable
``Value`` and issues an error. The correct idiom in SPARK for such a case is to
use a procedure with an output parameter to return the desired result:

.. code:: ada spark-flow

    package Ok_Functions is
       procedure Set (V : Integer; Prev : out Integer);
       function Get return Integer;
    end Ok_Functions;

    package body Ok_Functions is

       Value : Integer := 0;

       procedure Set (V : Integer; Prev : out Integer) is
       begin
          Prev := Value;
          Value := V;
       end Set;

       function Get return Integer is (Value);

    end Ok_Functions;

With the above restrictions in SPARK, none of the conflicts of side-effects
that can occur in C can occur in SPARK, as guaranteed by flow analysis.
