Expressions
===========

.. include:: ../../global.txt

Expressions: Definition
-----------------------

According to the Ada Reference Manual, an expression "is a formula that defines
the computation or retrieval of a value." Also, when an expression is evaluated,
the computed or retrieved value always has an associated type.

Even though the definition above is very simple, Ada expressions are actually
very flexible |mdash| and they can also be very complex. In fact, if you read
the :arm:`corresponding section <4-4>` of the Ada Reference Manual, you'll
quickly discover that they include elements such as relations, membership
choices, terms and primaries. In this section, we present examples of just some
of these elements. For a complete overview, please refer to the Reference
Manual.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.4 Expressions <4-4>`


Relations and simple expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Expressions usually consist of relations, which in turn consist of simple
expressions. (There are more details to this, but we'll keep it simple for the
moment.) Let's see a code example with a few expressions, which we dissect into
the corresponding grammatical elements (we're going to discuss them later):

.. code:: ada run_button project=Courses.Advanced_Ada.Expressions.Expression_Elements

    procedure Show_Expression_Elements is
       type Mode is (Off, A, B, C, D);

       pragma Unreferenced (B, C, D);

       subtype Active_Mode is Mode
         range Mode'Succ (Off) .. Mode'Last;

       M1, M2 : Mode;
       Dummy     : Boolean;
    begin
       M1 := A;

       Dummy :=
           M1 in Active_Mode
                    and then M2 in Off | A;
       --
       --   ^^^^^^^^^^^^^^^^^ relation
       --
       --                     ^^^^^^^^^^^^^^ relation
       --   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expression

       Dummy :=
           M1 in Active_Mode;
       --  ^^ name
       --  ^^ primary
       --  ^^ factor
       --  ^^ term
       --  ^^ simple expression
       --
       --        ^^^^^^^^^^^ membership choice
       --        ^^^^^^^^^^^ membership choice list
       --
       --  ^^^^^^^^^^^^^^^^^ relation
       --  ^^^^^^^^^^^^^^^^^ expression

       Dummy :=
           M2 in Off | A;
       --  ^^ name
       --  ^^ primary
       --  ^^ factor
       --  ^^ term
       --  ^^ simple expression
       --
       --        ^^^ membership choice
       --              ^ membership choice
       --        ^^^^^^^ membership choice list
       --
       --  ^^^^^^^^^^^^^ relation
       --  ^^^^^^^^^^^^^ expression

    end Show_Expression_Elements;

In this code example, we see three expressions. As we mentioned earlier, every
expression has a type; here, the type of each expression is :ada:`Boolean`.

The first expression (:ada:`M1 in Active_Mode and then M2 in Off | A`) consists
of two relations: :ada:`M1 in Active_Mode` and :ada:`M2 in Off | A`. Let's
discuss some of the details.

The :ada:`M1 in Active_Mode` relation consists of the simple expression
:ada:`M1` and the membership choice list :ada:`Active_Mode`. (Here, the
:ada:`in` keyword is part of the relation definition.) Also, as we see in the
comments of the source code, the simple expression :ada:`M1` is, at the same
time, a term, a factor, a primary and a name.

Let's briefly talk about this chain of syntactic elements for simple
expressions. Very roughly said, this is how we can break up simple expressions:

- a simple expression consists of terms;

- a term consists of factors;

- a factor consists of primaries;

- a primary can be one of those:

    - a numeric literal;

    - :ada:`null`;

    - a string literal;

    - :doc:`an aggregate <aggregates>`;

    - a name;

    - an allocator (like :ada:`new Integer`);

    - :ref:`a parenthesized expression <Adv_Ada_Parenthesized_Expressions>`;

    - :ref:`a conditional expression <Adv_Ada_Conditional_Expressions>`;

    - :ref:`a quantified expression <Adv_Ada_Quantified_Expressions>`;

    - :ref:`a declare expression <Adv_Ada_Declare_Expressions>`.

.. admonition:: For further reading...

    The definition of simple expressions we've just seen is very simplified. In
    actuality, these are the grammatical elements specified in the Ada Reference
    Manual:

    .. code-block:: none

        simple_expression ::=
          [unary_adding_operator] term {binary_adding_operator term}

        term ::= factor {multiplying_operator factor}

        factor ::= primary [** primary] | abs primary | not primary

        primary ::=
          numeric_literal | null | string_literal | aggregate
        | name | allocator | (expression)
        | (conditional_expression) | (quantified_expression)
        | (declare_expression)

Later on in this chapter, we discuss
:ref:`conditional expressions <Adv_Ada_Conditional_Expressions>`,
:ref:`quantified expressions <Adv_Ada_Quantified_Expressions>` and
:ref:`declare expressions <Adv_Ada_Declare_Expressions>` in more details.

In the relation :ada:`M2 in Off | A` from the code example, :ada:`Off | A` is
a membership choice list, and :ada:`Off` and :ada:`A` are membership choices.

.. admonition:: For further reading...

    Relations can actually be much more complicated than the one we just
    seen. In fact, this is the definition from the Ada Reference Manual:

    .. code-block:: none

        expression ::=
             relation {and relation} | relation {and then relation}
           | relation {or relation}  | relation {or else relation}
           | relation {xor relation}

        relation ::=
             simple_expression [relational_operator simple_expression]
           | simple_expression [not] in membership_choice_list
           | raise_expression

    Again, for more details, please refer to the
    :arm:`section on expressions <4-4>` of the Ada Reference Manual.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.4 Expressions <4-4>`
    - :arm:`4.5.2 Relational Operators and Membership Tests <4-5-2>`


Numeric expressions
~~~~~~~~~~~~~~~~~~~

The expressions we've seen so far had the :ada:`Boolean` type. Although much
of the grammar described in the Manual exists exclusively for Boolean
operations, we can also write numeric expressions such as the following one:

.. code:: ada run_button project=Courses.Advanced_Ada.Expressions.Numeric_Expressions

    procedure Show_Numeric_Expressions is
       C1    : constant Integer := 5;
       Dummy :          Integer;
    begin
       Dummy :=
           -2 ** 4 + 3 * C1 ** 8;
       --                      ^ numeric literal
       --                      ^ primary
       --                ^^      name
       --                ^^      primary
       --                ^^^^^^^ factor
       --              ^ multiplying operator
       --            ^           numeric literal
       --            ^           primary
       --            ^           factor
       --            ^^^^^^^^^^^ term
       --
       --        ^ numeric literal
       --        ^ primary
       --   ^ numeric literal
       --   ^ primary
       --   ^^^^^^               factor
       --   ^^^^^^               term
       --          ^ binary adding operator
       --  ^ unary adding operator
       --
       --  ^^^^^^^^^^^^^^^^^^^^^^ simple expression
       --
       --  ^^^^^^^^^^^^^^^^^^^^^^ expression
    end Show_Numeric_Expressions;

In this code example, the expression :ada:`- 2 ** 4 + 3 * C1 ** 8` consists of
just a single simple expression. (Note that simple expressions do not have to
be "simple".) This simple expression consists of two terms: :ada:`2 ** 4` and
:ada:`3 * C1 ** 8`. While the :ada:`2 ** 4` term is also a single factor, the
:ada:`3 * C1 ** 8` term consists of two factors: :ada:`3` and :ada:`C1 ** 8`.
Both the :ada:`2 ** 4` and the :ada:`C1 ** 8` factors consists of two primaries
each:

- the :ada:`2 ** 4` factor has the primaries :ada:`2` and :ada:`4`,

- the :ada:`C1 ** 8` factor has the primaries :ada:`C1` and :ada:`8`.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.4 Expressions <4-4>`


Other expressions
~~~~~~~~~~~~~~~~~

Expressions aren't limited to the :ada:`Boolean` type or to numeric types.
Indeed, expressions can be of any type, and the definition of primaries we've
seen earlier on already hints in this direction |mdash| as it includes elements
such as allocators. Because expressions are very flexible, covering all possible
variations and combinations in this section is out of scope. Again, please refer
to the :arm:`section on expressions <4-4>` of the Ada Reference Manual for
further details.


.. _Adv_Ada_Parenthesized_Expressions:

Parenthesized expression
~~~~~~~~~~~~~~~~~~~~~~~~

An interesting aspect of primaries is that, by using parentheses, we can
embed an expression inside another expression. As an example, let's discuss the
following expression and its elements:

.. code:: ada run_button project=Courses.Advanced_Ada.Expressions.Parenthesized_Expressions

    procedure Show_Parenthesized_Expressions is
       C1 : constant Integer := 4;
       C2 : constant Integer := 5;

       Dummy : Integer;
    begin
       Dummy :=
           (2 + C1) * C2;
       --       ^^       name
       --       ^^       primary
       --       ^^       factor
       --       ^^       term
       --
       --   ^            numeric literal
       --   ^            primary
       --   ^            factor
       --   ^            term
       --
       --     ^          binary adding operator
       --  ^^^^^^^^      simple expression
       --
       --  ^^^^^^^^      expression
       --  ^^^^^^^^      primary
       --  ^^^^^^^^      factor
       --
       --             ^^ factor
       --  ^^^^^^^^^^^^^ term
       --
       --  ^^^^^^^^^^^^^ simple expression
       --
       --  ^^^^^^^^^^^^^ expression
    end Show_Parenthesized_Expressions;

In this example, we first start with the single expression :ada:`(2 + C1) * C2`,
which is also a simple expression consisting of just one term, which consists of
two factors: :ada:`(2 + C1)` and :ada:`C2`. The :ada:`(2 + C1)` factor is also a
primary. Now, because of the parentheses, we identify that the primary
:ada:`(2 + C1)` is an expression that is embedded in another expression.

.. admonition:: Important

    To be fair, the existence of parentheses in a primary could also indicate
    other kinds of expressions, such as conditional or quantified expressions.
    However, differentiating between them is straightforward, as we'll see later
    on in this chapter.

We then proceed to parse the :ada:`(2 + C1)` expression, which consists of the
terms :ada:`2` and :ada:`C1`. As we've seen in the comments of the code example,
each of these terms consists of one factor, which consists of one primary. In
the end, after parsing the primaries, we identify that :ada:`2` is a numeric
literal and :ada:`C1` is a name.

Note that the usage of parentheses might lead to situations where we have
expressions in potentially unsuspected places. For example, consider the
following code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Expressions.Name_In_Expression

    procedure Show_Name_In_Expression is
       type Mode is (Off, A, B, C, D);

       M1 : Mode;
    begin
       M1 := A;

       case M1 is
         when Off | D   =>
           null;
         when A | B | C =>
           M1 := D;
       end case;

    end Show_Name_In_Expression;

Here, the case statement expects a selecting expression. In this case, :ada:`M1`
is identified as a name |mdash| after being identified as a relation, a simple
expression, a term, a factor and a primary.

However, if we replace :ada:`case M1 is` by :ada:`case (M1) is`, :ada:`(M1)`
is identified as a parenthesized expression, not as a name! This parenthesized
expression is first parsed and evaluated, which might have implications in case
statements, as we'll see in another chapter.

.. todo::

    Add link to subsection on case statements and expressions.


.. _Adv_Ada_Conditional_Expressions:

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
         (if S = Off
           then On
           elsif S = On
             then Off
             else S);

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

       type Integer_Arr is
         array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr)
                         return Boolean is
          (for all I in A'Range => A (I) = 0);

       function Has_Zero (A : Integer_Arr)
                          return Boolean is
          (for some I in A'Range => A (I) = 0);

       procedure Display_Array (A    : Integer_Arr;
                                Name : String);

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
       Put_Line ("Is_Zero: "
                 & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: "
                 & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "
                 & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: "
                 & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

As you might have expected, we can rewrite a quantified expression as a loop
in the :ada:`for I in A'Range loop if ... return ...` form. In the code below,
we're implementing :ada:`Is_Zero` and :ada:`Has_Zero` using loops and
conditions instead of quantified expressions:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_2

    package Int_Arrays is

       type Integer_Arr is
         array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr)
                         return Boolean;

       function Has_Zero (A : Integer_Arr)
                          return Boolean;

       procedure Display_Array (A    : Integer_Arr;
                                Name : String);

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Arrays is

       function Is_Zero (A : Integer_Arr)
                         return Boolean is
       begin
          for I in A'Range loop
             if A (I) /= 0 then
                return False;
             end if;
          end loop;

          return True;
       end Is_Zero;

       function Has_Zero (A : Integer_Arr)
                          return Boolean is
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
       Put_Line ("Is_Zero: "
                 & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: "
                 & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "
                 & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: "
                 & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

So far, we've seen quantified expressions using indices |mdash| e.g.
:ada:`for all I in A'Range => ...`. We could avoid indices in quantified
expressions by simply using the :ada:`E of A` form. In this case, we can just
write :ada:`for all E of A => ...`. Let's adapt the implementation of
:ada:`Is_Zero` and :ada:`Has_Zero` using this form:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_3

    package Int_Arrays is

       type Integer_Arr is
         array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr)
                         return Boolean is
          (for all E of A => E = 0);

       function Has_Zero (A : Integer_Arr)
                          return Boolean is
          (for some E of A => E = 0);

    end Int_Arrays;

Here, we're checking the components :ada:`E` of the array :ada:`A` and
comparing them against zero.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.5.8 Quantified Expressions <4-5-8>`


.. _Adv_Ada_Declare_Expressions:

Declare Expressions
-------------------

So far, we've seen expressions that make use of existing objects declared
outside of the expression. Sometimes, we might want to declare constant objects
inside the expression, so we can use them locally in the expression. Similarly,
we might want to rename an object and use the renamed object in an expression.
In those cases, we can use a declare expression.

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

.. note::

   This feature was introduced in Ada 2022.

A reduction expression reduces a list of values into a single value. For
example, we can reduce the list :ada:`[2, 3, 4]` to a single value:

- by adding the values of the list: :ada:`2 + 3 + 4 = 9`, or

- by multiplying the values of the list: :ada:`2 * 3 * 4 = 24`.

We write a reduction expression by using the :ada:`Reduce` attribute and
providing the reducer and its initial value:

- the reducer is the operator (e.g.: :ada:`+` or :ada:`*`) that we use to
  *combine* the values of the list;

- the initial value is the value that we use before all other values of the
  list.

For example, if we use :ada:`+` as the operator and :ada:`0` an the initial
value, we get the reduction expression: :ada:`0 + 2 + 3 + 4 = 9`. This can be
implemented using an array:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Simple_Reduction_Expression switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       A : array (1 .. 3) of Integer;
       I : Integer;
    begin
       A := [2, 3, 4];
       I := A'Reduce ("+", 0);

       Put_Line ("A = "
                 & A'Image);
       Put_Line ("I = "
                 & I'Image);
    end Show_Reduction_Expression;

Here, we have the array :ada:`A` with a list of values. The
:ada:`A'Reduce ("+", 0)` expression reduces the list of values of :ada:`A` into
a single value |mdash| in this case, an integer value that is stored in
:ada:`I`. This statement is equivalent to:

.. code-block:: ada

       I := 0;
       for E of A loop
          I := I + E;
       end loop;

Naturally, we can reduce the array using the :ada:`*` operator:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Simple_Reduction_Expression switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       A : array (1 .. 3) of Integer;
       I : Integer;
    begin
       A := [2, 3, 4];
       I := A'Reduce ("*", 1);

       Put_Line ("A = "
                 & A'Image);
       Put_Line ("I = "
                 & I'Image);
    end Show_Reduction_Expression;

In this example, we call :ada:`A'Reduce ("*", 1)` to reduce the list. (Note
that we use an initial value of one because it is the
:wikipedia:`identity element <Identity_element>` of a multiplication, so the
complete operation is: :ada:`1 * 2 * 3 * 4 = 24`.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`Reduction Expressions <4-5-10>`


Value sequences
~~~~~~~~~~~~~~~

In addition to arrays, we can apply reduction expression to value sequences,
which consist of an iterated element association |mdash| for example,
:ada:`[for I in 1 .. 3 => I + 1]`. We can simply *append* the reduction
expression to a value sequence:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Reduction_Expression_Value_Sequences switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       I : Integer;
    begin
       I := [for I in 1 .. 3 =>
               I + 1]'Reduce ("+", 0);
       Put_Line ("I = "
                 & I'Image);

       I := [for I in 1 .. 3 =>
               I + 1]'Reduce ("*", 1);
       Put_Line ("I = "
                 & I'Image);
    end Show_Reduction_Expression;

In this example, we create the value sequence :ada:`[for I in 1 .. 3 => I + 1]`
and reduce it using the :ada:`+` and :ada:`*` operators. (Note that the
operations in this example have the same results as in the previous examples
using arrays.)


Custom reducers
~~~~~~~~~~~~~~~

In the previous examples, we've used standard operators such as :ada:`+` and
:ada:`*` as the reducer. We can, however, write our own reducers and pass
them to the :ada:`Reduce` attribute. For example:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Custom_Reducer_Procedure switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       type Integer_Array is
         array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);
       I : Long_Integer;

       procedure Accumulate
         (Accumulator : in out Long_Integer;
          Value       : Integer) is
       begin
          Accumulator := Accumulator
                         + Long_Integer (Value);
       end Accumulate;

    begin
       A := [2, 3, 4];
       I := A'Reduce (Accumulate, 0);

       Put_Line ("A = "
                 & A'Image);
       Put_Line ("I = "
                 & I'Image);
    end Show_Reduction_Expression;

In this example, we implement the :ada:`Accumulate` procedure as our reducer,
which is called to accumulate the individual elements (integer values) of the
list. We pass this procedure to the :ada:`Reduce` attribute in the
:ada:`I := A'Reduce (Accumulate, 0)` statement, which is equivalent to:

.. code-block:: ada

       I := 0;
       for E of A loop
          Accumulate (I, E);
       end loop;

A custom reducer must have the following parameters:

1. The accumulator parameter, which stores the interim result |mdash| and the
   final result as well, once all elements of the list have been processed.

2. The value parameter, which is a single element from the list.

Note that the accumulator type doesn't need to match the type of the individual
components. In this example, we're using :ada:`Integer` as the component type,
while the accumulator type is :ada:`Long_Integer`. (For this kind of reducers,
using :ada:`Long_Integer` instead of :ada:`Integer` for the accumulator type
makes lots of sense due to the risk of triggering overflows while the reducer
is accumulating values |mdash| e.g. when accumulating a long list with larger
numbers.)

In the example above, we've implemented the reducer as a procedure. However, we
can also implement it as a function. In this case, the accumulated value is
returned by the function:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Custom_Reducer_Function switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       type Integer_Array is
         array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);
       I : Long_Integer;

       function Accumulate
         (Accumulator : Long_Integer;
          Value       : Integer)
          return Long_Integer is
       begin
          return Accumulator + Long_Integer (Value);
       end Accumulate;

    begin
       A := [2, 3, 4];
       I := A'Reduce (Accumulate, 0);

       Put_Line ("A = "
                 & A'Image);
       Put_Line ("I = "
                 & I'Image);
    end Show_Reduction_Expression;

In this example, we converted the :ada:`Accumulate` procedure into a function
(while the core implementation is essentially the same).

Note that the reduction expression remains the same, independently of whether
we're using a procedure or a function as the reducer. Therefore, the statement
with the reduction expression in this example is the same as in the previous
example: :ada:`I := A'Reduce (Accumulate, 0);`. Now that we're using a
function, this statement is equivalent to:

.. code-block:: ada

       I := 0;
       for E of A loop
          I := Accumulate (I, E);
       end loop;


Other accumulator types
~~~~~~~~~~~~~~~~~~~~~~~

The accumulator type isn't restricted to scalars: in fact, we could use record
types as well. For example:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Reducer_Integer_Accumulator switches=Compiler(-gnatX)


    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Reduction_Expression is
       type Integer_Array is
         array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);

       type Integer_Accumulator is record
          Value : Long_Integer;
          Count : Integer;
       end record;

       function Accumulate
         (Accumulator : Integer_Accumulator;
          Value       : Integer)
          return Integer_Accumulator is
       begin
          return (Value => Accumulator.Value
                           + Long_Integer (Value),
                  Count => Accumulator.Count + 1);
       end Accumulate;

       function Zero return Integer_Accumulator is
         (Value => 0, Count => 0);

       function Average (Acc : Integer_Accumulator)
                         return Float is
         (Float (Acc.Value) / Float (Acc.Count));

       Acc : Integer_Accumulator;

    begin
       A := [2, 3, 4];

       Acc := A'Reduce (Accumulate, Zero);
       Put_Line ("Acc = "
                 & Acc'Image);
       Put_Line ("Avg = "
                 & Average (Acc)'Image);
    end Show_Reduction_Expression;

In this example, we're using the :ada:`Integer_Accumulator` record type in our
reducer |mdash| the :ada:`Accumulate` function. In this case, we're not only
accumulating the values, but also counting the number of elements in the
list. (Of course, we could have used :ada:`A'Length` for that as well.)

Also, we're not limited to numeric types: we can also create a reducer using
strings as the accumulator type. In fact, we can display the initial value and
the elements of the list by using unbounded strings:

.. code:: ada run_button manual_chop project=Courses.Advanced_Ada.Expressions.Reducer_String_Accumulator switches=Compiler(-gnatX)

    !show_reduction_expression.adb
    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    procedure Show_Reduction_Expression is
       type Integer_Array is
         array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);

       function Unbounded_String_List
         (Accumulator : Unbounded_String;
          Value       : Integer)
              return Unbounded_String is
       begin
          return Accumulator
                 & ", " & Value'Image;
       end Unbounded_String_List;

    begin
       A := [2, 3, 4];

       Put_Line ("A = "
                 & A'Image);
       Put_Line ("L = "
                 & To_String (A'Reduce
                   (Unbounded_String_List,
                      To_Unbounded_String ("0"))));
    end Show_Reduction_Expression;

In this case, the "accumulator" is concatenating the initial value and
individual values of the list into a string.
