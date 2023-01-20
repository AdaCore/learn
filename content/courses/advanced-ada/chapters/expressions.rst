Expressions
===========

.. include:: ../../global.txt

Conditional Expressions
-----------------------

As we've seen before, we can write simple expressions such as :ada:`I = 0` or
:ada:`D.Valid`. A conditional expression, as the name implies, is an
expression that contains a condition. This might be an "if-expression" (in the
:ada:`if ... then ... else` form) or a "case-expression" (in the
:ada:`case ... is when =>` form).

The :ada:`Max` function in the following code example is an expression function
implemented with a conditional expression |mdash| an if-expression, to be more
precise:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Simple_Conditional_If_Expressions

    package Expr_Func is

       function Max (A, B : Integer) return Integer is
         (if A >= B then A else B);

    end Expr_Func;


Let's say we have a system with four states :ada:`Off`, :ada:`On`,
:ada:`Waiting`, and :ada:`Invalid`. For this system, we want to implement a
function named :ada:`Toggled` that returns the *toggled* value of a state
:ada:`S`. If the current value of :ada:`S` is either :ada:`Off` or :ada:`On`,
the function toggles from :ada:`Off` to :ada:`On` (or from :ada:`On`
to :ada:`Off`). For other values, the state remains unchanged |mdash| i.e. the
returned value is the same as the input value. This is the implementation using
a conditional expression:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_If_Expressions_1

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State is
         (if S = Off then On elsif S = On then Off else S);

    end Expr_Func;

As you can see, if-expressions may contain an :ada:`elsif` branch (and
therefore be more complicated).

The code above corresponds to this more verbose version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_If_Expressions_2

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State;

    end Expr_Func;

    package body Expr_Func is

       function Toggled (S : State) return State is
       begin
          if S = Off then
             return On;
          elsif S = On then
             return Off;
          else
             return S;
          end if;
       end Toggled;

    end Expr_Func;

If we compare the if-block of this code example to the if-expression of the
previous example, we notice that the if-expression is just a simplified version
without the :ada:`return` keyword and the :ada:`end if;`. In fact, converting
an if-block to an if-expression is quite straightforward.

We could also replace the if-expression used in the :ada:`Toggled` function
above with a case-expression. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_Case_Expressions_1

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State is
         (case S is
           when Off    => On,
           when On     => Off,
           when others => S);

    end Expr_Func;

Note that we use commas in case-expressions to separate the alternatives (the
:ada:`when` expressions). The code above corresponds to this more verbose
version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_Case_Expressions_2

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State;

    end Expr_Func;

    package body Expr_Func is

       function Toggled (S : State) return State is
       begin
          case S is
             when Off    => return On;
             when On     => return Off;
             when others => return S;
          end case;
       end Toggled;

    end Expr_Func;

If we compare the case block of this code example to the case-expression of the
previous example, we notice that the case-expression is just a simplified
version of the case block without the :ada:`return` keyword and the
:ada:`end case;`, and with alternatives separated by commas instead of
semicolons.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.5.7 Conditional Expressions <4-5-7>`

.. _Adv_Ada_Quantified_Expressions:

Quantified Expressions
----------------------

Quantified expressions are :ada:`for` expressions using a quantifier |mdash|
which can be either :ada:`all` or :ada:`some` |mdash| and a predicate. This
kind of expressions let us formalize statements such as:

- "all values of array :ada:`A` must be zero" into
  :ada:`for all I in A'Range => A (I) = 0`, and

- "at least one value of array :ada:`A` must be zero" into
  :ada:`for some I in A'Range => A (I) = 0`.

In the quantified expression :ada:`for all I in A'Range => A (I) = 0`, the
quantifier is :ada:`all` and the predicate is :ada:`A (I) = 0`. In the second
expression, the quantifier is :ada:`some`. The result of a quantified
expression is always a Boolean value.

For example, we could use the quantified expressions above and implement these
two functions:

- :ada:`Is_Zero`, which checks whether all components of an array :ada:`A` are
  zero, and

- :ada:`Has_Zero`, which checks whether array :ada:`A` has at least one
  component of the array :ada:`A` is zero.

This is the complete code:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_1

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean is
          (for all I in A'Range => A (I) = 0);

       function Has_Zero (A : Integer_Arr) return Boolean is
          (for some I in A'Range => A (I) = 0);

       procedure Display_Array (A : Integer_Arr; Name : String);

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Arrays is

       procedure Display_Array (A    : Integer_Arr;
                                Name : String) is
       begin
          Put (Name & ": ");
          for E of A loop
             Put (E'Image & " ");
          end loop;
          New_Line;
       end Display_Array;

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Int_Arrays;  use Int_Arrays;

    procedure Test_Int_Arrays is
       A : Integer_Arr := (0, 0, 1);
    begin
       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

As you might have expected, we can rewrite a quantified expression as a loop
in the :ada:`for I in A'Range loop if ... return ...` form. In the code below,
we're implementing :ada:`Is_Zero` and :ada:`Has_Zero` using loops and
conditions instead of quantified expressions:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_2

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean;

       function Has_Zero (A : Integer_Arr) return Boolean;

       procedure Display_Array (A : Integer_Arr; Name : String);

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Arrays is

       function Is_Zero (A : Integer_Arr) return Boolean is
       begin
          for I in A'Range loop
             if A (I) /= 0 then
                return False;
             end if;
          end loop;

          return True;
       end Is_Zero;

       function Has_Zero (A : Integer_Arr) return Boolean is
       begin
          for I in A'Range loop
            if A (I) = 0 then
               return True;
            end if;
          end loop;

          return False;
       end Has_Zero;

       procedure Display_Array (A    : Integer_Arr;
                                Name : String) is
       begin
          Put (Name & ": ");
          for E of A loop
             Put (E'Image & " ");
          end loop;
          New_Line;
       end Display_Array;

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Int_Arrays;  use Int_Arrays;

    procedure Test_Int_Arrays is
       A : Integer_Arr := (0, 0, 1);
    begin
       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

So far, we've seen quantified expressions using indices |mdash| e.g.
:ada:`for all I in A'Range => ...`. We could avoid indices in quantified
expressions by simply using the :ada:`E of A` form. In this case, we can just
write :ada:`for all E of A => ...`. Let's adapt the implementation of
:ada:`Is_Zero` and :ada:`Has_Zero` using this form:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_3

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean is
          (for all E of A => E = 0);

       function Has_Zero (A : Integer_Arr) return Boolean is
          (for some E of A => E = 0);

    end Int_Arrays;

Here, we're checking the components :ada:`E` of the array :ada:`A` and
comparing them against zero.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.5.8 Quantified Expressions <4-5-8>`


Declare Expressions
-------------------

So far, we've seen expressions that make use of existing objects. Sometimes, we
might want to declare constant objects that we can use in an expression.
Similarly, we might want to rename an object and use it in an expression. In
those cases, we can use a declare expression.

A declare expression allows for declaring or renaming objects within an
expression:

.. code:: ada compile_button project=Courses.Advanced_Ada.Expressions.Simple_Declare_Expression

    pragma Ada_2022;

    package P is

       function Max (A, B : Integer) return Integer is
         (declare
             Bigger_A : constant Boolean := (A >= B);
          begin
             (if Bigger_A then A else B));

    end P;

The declare expression starts with the :ada:`declare` keyword and the usual
object declarations, and it's followed by the :ada:`begin` keyword and the
body. In this example, the body of the declare expression is a conditional
expression.

Of course, the code above isn't really useful, so let's look at a more complete
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Expressions.Integer_Arrays

    pragma Ada_2022;

    package Integer_Arrays is

       type Integer_Array is
         array (Positive range <>) of Integer;

       function Sum (Arr : Integer_Array)
                     return Integer;

       --
       --  Expression function using
       --  declare expression:
       --
       function Avg (Arr : Integer_Array)
                     return Float is
         (declare
             A :          Integer_Array renames Arr;
             S : constant Float := Float (Sum (A));
             L : constant Float := Float (A'Length);
          begin
             S / L);

    end Integer_Arrays;

    package body Integer_Arrays is

       function Sum (Arr : Integer_Array)
                     return Integer is
       begin
          return Acc : Integer := 0 do
             for V of Arr loop
                Acc := Acc + V;
             end loop;
          end return;
       end Sum;

    end Integer_Arrays;

    pragma Ada_2022;

    with Ada.Text_IO;    use Ada.Text_IO;

    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Integer_Arrays is
       Arr : constant Integer_Array := [1, 2, 3];
    begin
       Put_Line ("Sum: "
                 & Sum (Arr)'Image);
       Put_Line ("Avg: "
                 & Avg (Arr)'Image);
    end Show_Integer_Arrays;

In this example, the :ada:`Avg` function is implemented using a declare
expression. In this expression, :ada:`A` renames the :ada:`Arr` array, and
:ada:`S` is a constant initialized with the value returned by the :ada:`Sum`
function.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.5.9 Declare Expressions <4-5-9>`


Restrictions in the declarative part
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The declarative part of a declare expression is more restricted than the
declarative part of a subprogram or declare block. In fact, we cannot:

- declare variables;

- declare constants of limited types;

- rename an object of limited type that is constructed within the declarative
  part;

- declare aliased constants;

- declare constants that make use of the :ada:`Access` or
  :ada:`Unchecked_Access` attributes in the initialization;

- declare constants of anonymous access type.

Let's see some examples of erroneous declarations:

.. code:: ada compile_button project=Courses.Advanced_Ada.Expressions.Integer_Arrays_Error
    :class: ada-expect-compile-error

    pragma Ada_2022;

    package Integer_Arrays is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Integer_Sum is limited private;

       type Const_Integer_Access is
         access constant Integer;

       function Sum (Arr : Integer_Array)
                     return Integer;

       function Sum (Arr : Integer_Array)
                     return Integer_Sum;

       --
       --  Expression function using
       --  declare expression:
       --
       function Avg (Arr : Integer_Array)
                     return Float is
         (declare
             A  : Integer_Array renames Arr;

             S1 : aliased constant Integer := Sum (A);
             --  ERROR: aliased constant

             S : Float := Float (S1);
             L : Float := Float (A'Length);
             --  ERROR: declaring variables

             S2 : constant Integer_Sum := Sum (A);
             --  ERROR: declaring constant of
             --         limited type

             A1 : Const_Integer_Access :=
                    S1'Unchecked_Access;
             --  ERROR: using 'Unchecked_Access
             --         attribute

             A2 : access Integer := null;
             --  ERROR: declaring object of
             --         anonymous access type
          begin
             S / L);

    private

       type Integer_Sum is new Integer;

    end Integer_Arrays;

    package body Integer_Arrays is

       function Sum (Arr : Integer_Array)
                     return Integer is
       begin
          return Acc : Integer := 0 do
             for V of Arr loop
                Acc := Acc + V;
             end loop;
          end return;
       end Sum;

       function Sum (Arr : Integer_Array)
                     return Integer_Sum is
         (Integer_Sum (Integer'(Sum (Arr))));

    end Integer_Arrays;

In this version of the :ada:`Avg` function, we see many errors in the
declarative part of the declare expression. If we convert the declare
expression into an actual function implementation, however, those declarations
won't trigger compilation errors. (Feel free to try this out!)


Reduction Expressions
---------------------

.. admonition:: Relevant topics

    - :arm22:`Reduction Expressions <4-5-10>`

.. todo::

    Complete section!
