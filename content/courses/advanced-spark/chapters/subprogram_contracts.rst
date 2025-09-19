Subprogram Contracts
=====================================================================

.. include:: ../../../global.txt

Subprogram Contracts in Ada 2012 and SPARK 2014
---------------------------------------------------------------------

- Originate in Floyd-Hoare logic (1967-1969)

    - a Hoare triple {P}C{Q}

    - P is the precondition before executing command C

    - Q is the postcondition after executing command C

- Executable version by Meyer in Eiffel (1988)

    - Called Design by Contract ™

    - Precondition is checked dynamically before a routine starts

    - Postcondition is checked dynamically when a routine returns

- SPARK 2014 combines both views

    - SPARK 2005 version was only logic, Ada version is only executable


Dynamic Execution of Subprogram Contracts
---------------------------------------------------------------------

- Contract on subprogram declaration

    - Different from subprogram body in general (but not always)

- Ada Reference Manual allows implementations choice

    - Contract can be checked in the caller or in the callee

    - GNAT's choice is to execute in the callee

- GNAT introduces wrappers in some cases for contracts

    - For an imported subprogram (e.g. from C) with a contract

    - For cases where contracts on static call/dispatching are different

- Contracts are not enabled by default

    - Switch ``-gnata`` enables dynamic checking of contracts in GNAT


Dynamic Behavior when Subprogram Contracts Fail
---------------------------------------------------------------------

- Violation of contract raises an exception

    - Standard exception :ada:`Assertion_Error` is raised (same as for
      pragma :ada:`Assert` and all other assertions)

    - Exception cannot be caught by subprogram's own exception handler
      implementation choice caller/callee has no effect

    - Idiom allows to select another exception

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Dynamic_Behavior

    with Ada.Numerics; use Ada.Numerics;

    package Show_Dynamic_Behavior is

       function Sqrt (X : Float) return Float with
          Pre => X >= 0.0 or else raise Argument_Error;

    end Show_Dynamic_Behavior;

- Control over sequencing of checks

    - Typical pre/post is a conjunction of Boolean conditions

    - Use and when no possible RTE, and then otherwise (recommended for
      SPARK)


Precondition
---------------------------------------------------------------------

- Better alternative to defensive programming, compare

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Precondition

    with Ada.Numerics; use Ada.Numerics;

    package Show_Precondition is

       function Sqrt (X : Float) return Float with
          Pre => X >= 0.0 or else raise Argument_Error;

    end Show_Precondition;

and

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Precondition

    with Ada.Numerics; use Ada.Numerics;

    package Show_Precondition is

       --  X should be non-negative or Argument_Error is raised
       function Sqrt (X : Float) return Float;

    end Show_Precondition;

    package body Show_Precondition is

       function Sqrt (X : Float) return Float is
          Res : Float := 0.0;
       begin
          if X >= 0.0 then
             raise Argument_Error;
          end if;

          --  [...]

          return Res;
       end Sqrt;

    end Show_Precondition;

- Preconditions can be activated alone

.. code-block:: ada

    pragma Assertion_Policy (Pre => Check);


Postcondition
---------------------------------------------------------------------

- Single place to check all return paths from the subprogram

    - Avoids duplication of checks before each return statement

    - Much more robust during maintenance

    - Only applies to normal returns (not in exception, not on abort)

- Can relate input and output values

    - Special attribute :ada:`X'Old` for referring to input value of
      variable :ada:`X`

    - Special attribute :ada:`Func'Result` for referring to result of
      function :ada:`Func`

    - Special attribute :ada:`Rec'Update` or :ada:`Arr'Update` for
      referring to modified value of record :ada:`Rec` or array :ada:`Arr`

        - replaced by delta aggregate syntax in Ada 202X: (
          :ada:`Rec with delta Comp => Value`)


Contract Cases
---------------------------------------------------------------------

- Convenient syntax to express a contract by cases

    - Cases must be disjoint and complete (forming a partition)

    - Introduced in SPARK, planned for inclusion in Ada 202X

    - Case is (guard => consequence) with :ada:`'Old` / :ada:`'Result` in
      consequence

    - Can be used in combination with precondition/postcondition

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Contract_Cases

    package Show_Contract_Cases is

       function Sqrt (X : Float) return Float with
         Contract_Cases =>
           (X > 1.0             => Sqrt'Result <= X,
            X = 1.0             => Sqrt'Result = 1.0,
            X < 1.0 and X > 0.0 => Sqrt'Result >= X,
            X = 0.0             => Sqrt'Result = 0.0);

    end Show_Contract_Cases;

- Both a precondition and a postcondition

    - On subprogram entry, exactly one guard must hold

    - On subprogram exit, the corresponding consequence must hold


Attribute ``'Old``
---------------------------------------------------------------------

- :ada:`X'Old` expresses the input value of :ada:`X` in postconditions

    - Same as :ada:`X` when variable not modified in the subprogram

    - Compiler inserts a copy of :ada:`X` on subprogram entry if :ada:`X`
      is large, copy can be expensive in memory footprint!

    - :ada:`X` can be a variable, a function call, a qualification (but
      not limited!)

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Attribute_Old

    package Show_Attribute_Old is

       type Value is new Integer;

       type My_Range is range 1 .. 10;

       type My_Array is array (My_Range) of Value;

       procedure Extract (A : in out My_Array;
                          J :        My_Range;
                          V :    out Value)
         with
           Post => (if J in A'Range then V = A (J)'Old and A (J) = 0);

    end Show_Attribute_Old;

- :ada:`Expr'Old` is rejected in potentially unevaluated context

    - :ada:`pragma Unevaluated_Use_Of_Old (Allow)` allows it

    - In Ada, user is responsible – in SPARK, user can rely on proof


Implication and Equivalence
---------------------------------------------------------------------

- If-expression can be used to express an implication

    - :ada:`(if A then B)` expresses the logical implication

        - ``A → B``

    - :ada:`(if A then B else C)` expresses the formula

        - ``(A → B)  (¬A → C)``

    - :ada:`(if A then B else C)` can also be used with B, C not of
      Boolean type

    - :ada:`(A <= B)` should not be used for expressing implication (same
      dynamic semantics, but less readable, and harmful in SPARK)

- Equality can be used to express an equivalence

    - :ada:`(A = B)` expresses the logical equivalence

        - ``(A ↔ B)``

    - A double implication should not be used for expressing equivalence
      (same semantics, but less readable and maintainable)


Reasoning by Cases
---------------------------------------------------------------------

- Case-expression can be used to reason by cases

    - Case test only on values of expressions of discrete type

    - Can sometimes be an alternative to contract cases

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Case_Expression

    with Ada.Text_IO;

    package Show_Case_Expression is

       type File_Mode is (Open, Active, Closed);

       type File is record
          F_Type : Ada.Text_IO.File_Type;
          Mode   : File_Mode;
       end record;

       procedure Open (F : in out File; Success : out Boolean) with
         Post =>
           (case F.Mode'Old is
              when Open   => Success,
              when Active => not Success,
              when Closed => Success = (F.Mode = Open));

    end Show_Case_Expression;

- Can sometimes be used at different levels in the expression

.. code-block:: ada

       procedure Open (F : in out File; Success : out Boolean) with
         Post =>
           Success = (case F.Mode'Old is
                        when Open   => True,
                        when Active => False,
                        when Closed => F.Mode = Open);


Universal and Existential Quantification
---------------------------------------------------------------------

- Quantified expressions can be used to express a property over a
  collection of values

    - :ada:`(for all X in A .. B => C)` expresses the universally
      quantified property

        - ``(∀ X . X ≥ A ⋀ X ≤ B → C)``

    - :ada:`(for some X in A .. B => C)` expresses the universally
      quantified property

        - ``(∃ X . X ≥ A ⋀ X ≤ B ⋀ C)``

- Quantified expressions translated as loops at run time

    - Control exits the loop as soon as the condition becomes false (resp.
      true) for a universally (resp. existentially) quantified expression

- Quantification forms over array and collection content

    - Syntax uses :ada:`(for all/some V of ... => C)`


Expression Functions
---------------------------------------------------------------------

- Without abstraction, contracts can become unreadable

    - Also, use of quantifications can make them unprovable

- Expression functions provide the means to abstract contracts

    - Expression function is a function consisting in an expression

    - Definition can complete a previous declaration

    - Definition is allowed in a package spec! (crucial for proof with
      SPARK)

.. code-block:: ada

    function Valid_Configuration return Boolean is
       (case Cur_State is
          when Piece_Falling | Piece_Blocked =>
            No_Overlap (Cur_Board, Cur_Piece),
          when Board_Before_Clean => True,
          when Board_After_Clean =>
            No_Complete_Lines (Cur_Board));


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_01

    with Ada.Assertions; use Ada.Assertions;

    procedure Example_01 is

       --  Fail systematically fails a precondition and catches the
       --  resulting exception.

       procedure Fail (Condition : Boolean) with
         Pre => Condition
       is
          Bad_Condition : Boolean := False;
       begin
          Fail (Bad_Condition);
       exception
          when Assertion_Error => return;
       end Fail;
    begin
       null;
    end Example_01;

This code is not correct. The exception from the recursive call is always
caught in the handler, but not the exception raised if caller of ``Fail``
passes :ada:`False` as value for ``Condition``.


Example #2
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_02

    with Interfaces.C; use Interfaces.C;

    package Example_02 is

       procedure Memset
         (B  : in out char_array;
          Ch :        char;
          N  :        size_t)
         with
           Import,
           Pre  => N <= B'Length,
           Post => (for all Idx in B'Range =>
                      (if Idx < B'First + N then
                             B (Idx) = Ch
                           else
                             B (Idx) = B'Old (Idx)));

    end Example_02;

This code is correct. GNAT will create a wrapper for checking the
precondition and postcondition of ``Memset``, calling the imported
``memset`` from ``libc``.


Example #3
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_03

    procedure Example_03 is

       pragma Assertion_Policy (Pre => Ignore);
       function Sqrt (X : Float) return Float with
         Pre => X >= 0.0;

       pragma Assertion_Policy (Pre => Check);
       function Sqrt (X : Float) return Float is
          Ret : Float := 0.0;
       begin
          --  missing implementation...
          return Ret;
       end Sqrt;

    begin
       null;
    end Example_03;

This code is not correct. Although GNAT inserts precondition checks in the
subprogram body instead of its caller, it is the value of :ada:`Pre`
assertion policy at the declaration of the subprogram that decides if
preconditions are activated.


Example #4
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_04

    procedure Example_04 is

       function Sqrt (X : Float) return Float with
         Pre => X >= 0.0;

       function Sqrt (X : Float) return Float with
         Pre => X >= 0.0
       is
          Ret : Float := 0.0;
       begin
          --  missing implementation...
          return Ret;
       end Sqrt;

    begin
       null;
    end Example_04;

This code is not correct. Contract is allowed only on the spec of a
subprogram. Hence it is not allowed on the body when a separate spec is
available.


Example #5
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_05

    procedure Example_05 is

       procedure Add (X, Y : Natural; Z : out Integer) with
         Contract_Cases =>
           (X <= Integer'Last - Y => Z = X + Y,
            others                => Z = 0)
       is
       begin
          Z := 0;
          Z := X + Y;
       end Add;

    begin
       null;
    end Example_05;

This code is not correct. Postcondition is only relevant for normal
returns.


Example #6
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_06

    procedure Example_06 is

       procedure Add (X, Y : Natural; Z : out Integer) with
         Post => Z = X + Y
       is
       begin
          Z := 0;
          Z := X + Y;
       end Add;
    begin
       null;
    end Example_06;

This code is correct. Procedure may raise an exception, but postcondition
correctly describes normal returns.


Example #7
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_07

    procedure Example_07 is

       procedure Add (X, Y : Natural; Z : out Integer) with
         Pre  => X <= Integer'Last - Y,
         Post => Z = X + Y
       is
       begin
          Z := X + Y;
       end Add;
    begin
       null;
    end Example_07;

This code is correct. Precondition prevents exception inside ``Add``.
Postcondition is always satisfied.


Example #8
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_08

    package Example_08 is

       procedure Memset
         (B  : in out String;
          Ch :        Character;
          N  :        Natural)
         with
           Pre  => N <= B'Length,
           Post => (for all Idx in B'Range =>
                      (if Idx < B'First + N then
                          B (Idx) = Ch
                       else
                          B (Idx) = B (Idx)'Old));
    end Example_08;

This code is not correct. :ada:`'Old` on expression including a quantified
variable is not allowed.


Example #9
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_09

    package Example_09 is

       procedure Memset
         (B  : in out String;
          Ch :        Character;
          N  :        Natural)
         with
           Pre  => N <= B'Length - 1,
           Post => (for all Idx in 1 .. N => B (B'First + Idx - 1) = Ch)
                    and then B (B'First + N) = B (B'First + N)'Old;

    end Example_09;

This code is not correct. :ada:`Expr'Old` on potentially unevaluated
expression is allowed only when :ada:`Expr` is a variable.


Example #10
~~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Subprogram_Contracts.Example_10

    package Example_10 is

       procedure Memset
         (B  : in out String;
          Ch :        Character;
          N  :        Natural)
         with
           Pre  => N <= B'Length - 1,
           Post => (for all Idx in 1 .. N => B (B'First + Idx - 1) = Ch)
                    and B (B'First + N) = B (B'First + N)'Old;

    end Example_10;

This code is correct. :ada:`Expr'Old` does not appear anymore in a
potentially unevaluated expression. Another solution would have been to
apply :ada:`'Old` on ``B`` or to use
:ada:`pragma Unevaluated_Use_Of_Old (Allow)`.
