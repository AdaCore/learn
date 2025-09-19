Proof of Program Integrity
=====================================================================

.. include:: ../../../global.txt

This section presents the proof capability of GNATprove, a major tool for
the SPARK language. We focus here on the simpler proofs that you'll need to
write to verify your program's integrity.  The primary objective of
performing proof of your program's integrity is to ensure the absence of
runtime errors during its execution.

The analysis steps discussed here are only sound if you've previously
performed :ref:`Intro_SPARK_Flow_Analysis`.  You shouldn't proceed further if you
still have unjustified flow analysis messages for your program.


Runtime Errors
---------------------------------------------------------------------

There's always the potential for errors that aren't detected during
compilation to occur during a program's execution. These errors, called
runtime errors, are those targeted by GNATprove.

There are various kinds of runtime errors, the most common being references
that are out of the range of an array (
:wikipedia:`buffer overflow <Buffer_overflow>` in Ada), subtype range
violations, overflows in computations, and divisions by zero. The code
below illustrates many examples of possible runtime errors, all within a
single statement.  Look at the assignment statement setting the
:ada:`I + J`'th cell of an array :ada:`A` to the value :ada:`P /Q`.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Runtime_Errors
    :class: ada-expect-prove-error

    package Show_Runtime_Errors is

       type Nat_Array is array (Integer range <>) of Natural;

       procedure Update (A : in out Nat_Array; I, J, P, Q : Integer);

    end Show_Runtime_Errors;

    package body Show_Runtime_Errors is

       procedure Update (A : in out Nat_Array; I, J, P, Q : Integer) is
       begin
          A (I + J) := P / Q;
       end Update;

    end Show_Runtime_Errors;

There are quite a number of errors that may occur when executing this code.
If we don't know anything about the values of :ada:`I`, :ada:`J`, :ada:`P`, and
:ada:`Q`, we can't rule out any of those errors.

First, the computation of :ada:`I` + :ada:`J` can overflow, for example if :ada:`I`
is :ada:`Integer'Last` and :ada:`J` is positive.

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;

Next, the sum, which is used as an array index, may not be in the range of
the index of the array.

.. code-block:: ada

    A (A'Last + 1) := P / Q;

On the other side of the assignment, the division may also overflow, though
only in the very special case where :ada:`P` is :ada:`Integer'First` and :ada:`Q`
is -1 because of the asymmetric range of signed integer types.

.. code-block:: ada

    A (I + J) := Integer'First / -1;

The division is also not allowed if :ada:`Q` is 0.

.. code-block:: ada

    A (I + J) := P / 0;

Finally, since the array contains natural numbers, it's also an error to
store a negative value in it.

.. code-block:: ada

    A (I + J) := 1 / -1;

The compiler generates checks in the executable code corresponding to each
of those runtime errors.  Each check raises an exception if it fails.  For
the above assignment statement, we can see examples of exceptions raised
due to failed checks for each of the different cases above.

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;
    -- raised CONSTRAINT_ERROR : overflow check failed

    A (A'Last + 1) := P / Q;
    -- raised CONSTRAINT_ERROR : index check failed

    A (I + J) := Integer'First / (-1);
    -- raised CONSTRAINT_ERROR : overflow check failed

    A (I + J) := 1 / (-1);
    -- raised CONSTRAINT_ERROR : range check failed

    A (I + J) := P / 0;
    -- raised CONSTRAINT_ERROR : divide by zero

These runtime checks are costly, both in terms of program size and
execution time. It may be appropriate to remove them if we can statically
ensure they aren't needed at runtime, in other words if we can prove that
the condition tested for can never occur.

This is where the analysis done by GNATprove comes in.  It can be used to
demonstrate statically that none of these errors can ever occur at
runtime. Specifically, GNATprove logically interprets the meaning of every
instruction in the program. Using this interpretation, GNATprove generates
a logical formula called a *verification condition* for each check that
would otherwise be required by the Ada (and hence SPARK) language.


.. code-block:: ada

    A (Integer'Last + 1) := P / Q;
    -- medium: overflow check might fail

    A (A'Last + 1) := P / Q;
    -- medium: array index check might fail

    A (I + J) := Integer'First / (-1);
    -- medium: overflow check might fail

    A (I + J) := 1 / (-1);
    -- medium: range check might fail

    A (I + J) := P / 0;
    -- medium: divide by zero might fail

GNATprove then passes these verification conditions to an automatic prover,
stated as conditions that must be true to avoid the error. If every such
condition can be validated by a prover (meaning that it can be
mathematically shown to always be true), we've been able to prove that no
error can ever be raised at runtime when executing that program.


Modularity
---------------------------------------------------------------------

To scale to large programs, GNATprove performs proofs on a per-subprogram
basis by relying on preconditions and postconditions to properly summarize
the input and output state of each subprogram. More precisely, when
verifying the body of a subprogram, GNATprove assumes it knows nothing
about the possible initial values of its parameters and of the global
variables it accesses except what you state in the subprogram's
precondition. If you don't specify a precondition, it can't make any
assumptions.

For example, the following code shows that the body of :ada:`Increment` can be
successfully verified: its precondition constrains the value of its
parameter :ada:`X` to be less than :ada:`Integer'Last` so we know the overflow
check is always false.

In the same way, when a subprogram is called, GNATprove assumes its
:ada:`out` and :ada:`in out` parameters and the global variables it writes
can be modified in any way compatible with their postconditions. For
example, since :ada:`Increment` has no postcondition, GNATprove doesn't know
that the value of :ada:`X` after the call is always less than
:ada:`Integer'Last`. Therefore, it can't prove that the addition following
the call to :ada:`Increment` can't overflow.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Modularity_1
    :class: ada-expect-prove-error

    procedure Show_Modularity is

       procedure Increment (X : in out Integer) with
         Pre => X < Integer'Last is
       begin
          X := X + 1;
          --  info: overflow check proved
       end Increment;

       X : Integer;
    begin
       X := Integer'Last - 2;
       Increment (X);
       --  After the call, GNATprove no longer knows the value of X

       X := X + 1;
       --  medium: overflow check might fail
    end Show_Modularity;

Exceptions
~~~~~~~~~~

There are two cases where GNATprove doesn't require modularity and hence
doesn't make the above assumptions. First, local subprograms without
contracts can be inlined if they're simple enough and are neither recursive
nor have multiple return points. If we remove the contract from
:ada:`Increment`, it fits the criteria for inlining.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Modularity_2

    procedure Show_Modularity is

       procedure Increment (X : in out Integer) is
       begin
          X := X + 1;
          --  info: overflow check proved, in call inlined at...
       end Increment;

       X : Integer;
    begin
       X := Integer'Last - 2;
       Increment (X);
       X := X + 1;
       --  info: overflow check proved
    end Show_Modularity;

GNATprove now sees the call to :ada:`Increment` exactly as if the increment on
:ada:`X` was done outside that call, so it can successfully verify that
neither addition can overflow.

.. note::

   For more details on contextual analysis of subprograms, see the
   :spark_ugs:`SPARK User's Guide <how_to_write_subprogram_contracts.html#contextual-analysis-of-subprograms-without-contracts>`.

The other case involves functions. If we define a function as an expression
function, with or without contracts, GNATprove uses the expression itself
as the postcondition on the result of the function.

In our example, replacing :ada:`Increment` with an expression function allows
GNATprove to successfully verify the overflow check in the addition.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Modularity_3

    procedure Show_Modularity is

       function Increment (X : Integer) return Integer is
         (X + 1)
         --  info: overflow check proved
         with Pre => X < Integer'Last;

       X : Integer;
    begin
       X := Integer'Last - 2;
       X := Increment (X);
       X := X + 1;
       --  info: overflow check proved
    end Show_Modularity;

.. note::

   For more details on expression functions, see the
   :spark_ugs:`SPARK User's Guide <specification_features.html#expression-functions>`.

Contracts
---------------------------------------------------------------------

Ada contracts are perfectly suited for formal verification, but are
primarily designed to be checked at runtime.  When you specify the
``-gnata`` switch, the compiler generates code that verifies the contracts
at runtime. If an Ada contract isn't satisfied for a given subprogram call,
the program raises the :ada:`Assert_Failure` exception. This switch is
particularly useful during development and testing, but you may also retain
run-time execution of assertions, and specifically preconditions, during
the program's deployment to avoid an inconsistent state.

Consider the incorrect call to :ada:`Increment` below, which violates its
precondition. One way to detect this error is by compiling the function
with assertions enabled and testing it with inputs that trigger the
violation. Another way, one that doesn't require guessing the needed
inputs, is to run GNATprove.

.. code:: ada run_button prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Precondition_Violation
    :class: ada-run-expect-failure, ada-expect-prove-error

    procedure Show_Precondition_Violation is

       procedure Increment (X : in out Integer) with
         Pre => X < Integer'Last  is
       begin
          X := X + 1;
       end Increment;

       X : Integer;

    begin
       X := Integer'Last;
       Increment (X);
    end Show_Precondition_Violation;

Similarly, consider the incorrect implementation of function :ada:`Absolute`
below, which violates its postcondition. Likewise, one way to detect this
error is by compiling the function with assertions enabled and testing with
inputs that trigger the violation. Another way, one which again doesn't
require finding the inputs needed to demonstrate the error, is to run
GNATprove.

.. code:: ada run_button prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Postcondition_Violation
    :class: ada-run-expect-failure, ada-expect-prove-error

    procedure Show_Postcondition_Violation is

       procedure Absolute (X : in out Integer) with
         Post => X >= 0 is
       begin
          if X > 0 then
             X := -X;
          end if;
       end Absolute;

       X : Integer;

    begin
       X := 1;
       Absolute (X);
    end Show_Postcondition_Violation;

The benefits of dynamically checking contracts extends beyond making
testing easier.  Early failure detection also allows an easier recovery and
facilitates debugging, so you may want to enable these checks at runtime to
terminate execution before some damaging or hard-to-debug action occurs.

GNATprove statically analyses preconditions and postconditions. It verifies
preconditions every time a subprogram is called, which is the runtime
semantics of contracts.  Postconditions, on the other hand, are verified
once as part of the verification of the subprogram's body. For example,
GNATprove must wait until :ada:`Increment` is improperly called to detect the
precondition violation, since a precondition is really a contract for the
caller. On the other hand, it doesn't need :ada:`Absolute` to be called to
detect that its postcondition doesn't hold for all its possible inputs.

.. note::

   For more details on pre and postconditions, see the
   :spark_ugs:`SPARK User's Guide <subprogram_contracts.html#preconditions>`.

Executable Semantics
~~~~~~~~~~~~~~~~~~~~

Expressions in Ada contracts have the same semantics as Boolean expressions
elsewhere, so runtime errors can occur during their computation. To
simplify both debugging of assertions and combining testing and static
verification, the same semantics are used by GNATprove.

While proving programs, GNATprove verifies that no error can ever be raised
during the execution of the contracts. However, you may sometimes find
those semantics too heavy, in particular with respect to overflow checks,
because they can make it harder to specify an appropriate precondition.  We
see this in the function :ada:`Add` below.

.. code:: ada run_button prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Executable_Semantics
    :class: ada-run-expect-failure, ada-expect-prove-error

    procedure Show_Executable_Semantics
      with SPARK_Mode => On
    is
       function Add (X, Y : Integer) return Integer is (X + Y)
         with Pre => X + Y in Integer;

       X : Integer;
    begin
       X := Add (Integer'Last, 1);
    end Show_Executable_Semantics;

GNATprove issues a message on this code warning about a possible overflow
when computing the sum of :ada:`X` and :ada:`Y` in the precondition. Indeed,
since expressions in assertions have normal Ada semantics, this addition
can overflow, as you can easily see by compiling and running the code that
calls :ada:`Add` with arguments :ada:`Integer'Last` and 1.

On the other hand, you sometimes may prefer GNATprove to use the
mathematical semantics of addition in contracts while the generated code
still properly verifies that no error is ever raised at runtime in the body
of the program. You can get this behavior by using the compiler switch
``-gnato??`` (for example ``-gnato13``), which allows you to independently
set the overflow mode in code (the first digit) and assertions (the second
digit).  For both, you can either reduce the number of overflow checks (the
value 2), completely eliminate them (the value 3), or preserve the default
Ada semantics (the value 1).

.. note::

   For more details on overflow modes, see the
   :spark_ugs:`SPARK User's Guide <overflow_modes.html>`.


Additional Assertions and Contracts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've seen, a key feature of SPARK is that it allows us to state
properties to check using assertions and contracts. SPARK supports
preconditions and postconditions as well as assertions introduced by the
:ada:`Assert` pragma.

The SPARK language also includes new contract types used to assist formal
verification. The new pragma :ada:`Assume` is treated as an assertion
during execution but introduces an assumption when proving programs.  Its
value is a Boolean expression which GNATprove assumes to be true without
any attempt to verify that it's true. You'll find this feature useful, but
you must use it with great care.  Here's an example of using it.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Pragma_Assume

    procedure Incr (X : in out Integer) is
    begin
       pragma Assume (X < Integer'Last);
       X := X + 1;
    end Incr;

.. note::

   For more details on pragma :ada:`Assume`, see the
   :spark_ugs:`SPARK User's Guide <assertion_pragmas.html#pragma-assume>`.

The :ada:`Contract_Cases` aspect is another construct introduced for
GNATprove, but which also acts as an assertion during execution. It allows
you to specify the behavior of a subprogram using a disjunction of
cases. Each element of a :ada:`Contract_Cases` aspect is a *guard*, which
is evaluated before the call and may only reference the subprogram's
inputs, and a *consequence*. At each call of the subprogram, one and only
one guard is permitted to evaluate to :ada:`True`. The consequence of that
case is a contract that's required to be satisfied when the subprogram
returns.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Absolute

    procedure Absolute (X : in out Integer) with
      Pre            =>  X > Integer'First,
      Contract_Cases => (X <  0 => X = -X'Old,
                         X >= 0 => X =  X'Old)
    is
    begin
       if X < 0 then
          X := -X;
       end if;
    end Absolute;

Similarly to how it analyzes a subprogram's precondition, GNATprove
verifies the :ada:`Contract_Cases` only once.  It verifies the validity of
each consequence (given the truth of its guard) and the disjointness and
completeness of the guard conditions (meaning that exactly one guard must
be true for each possible set of input values).

.. note::

   For more details on :ada:`Contract_Cases`, see the
   :spark_ugs:`SPARK User's Guide <subprogram_contracts.html#contract-cases>`.


.. _Intro_SPARK_Debugging_Failed_Proof_Attempts:

Debugging Failed Proof Attempts
---------------------------------------------------------------------

GNATprove may report an error while verifying a program for any of the
following reasons:

- there might be an error in the program; or

- the property may not be provable as written because more information is
  required; or

- the prover used by GNATprove may be unable to prove a perfectly valid
  property.

We spend the remainder of this section discussing the sometimes tricky task
of debugging failed proof attempts.

Debugging Errors in Code or Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, let's discuss the case where there's indeed an error in the program.
There are two possibilities: the code may be incorrect or, equally likely,
the specification may be incorrect. As an example, there's an error in our
procedure :ada:`Incr_Until` below which makes its :ada:`Contract_Cases`
unprovable.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Failed_Proof_Attempt_1
    :class: ada-expect-prove-error

    package Show_Failed_Proof_Attempt is

       Incremented : Boolean := False;

       procedure Incr_Until (X : in out Natural) with
         Contract_Cases =>
           (Incremented => X > X'Old,
            others      => X = X'Old);

    end Show_Failed_Proof_Attempt;

    package body Show_Failed_Proof_Attempt is

       procedure Incr_Until (X : in out Natural) is
       begin
          if X < 1000 then
             X := X + 1;
             Incremented := True;
          else
             Incremented := False;
          end if;
       end Incr_Until;

    end Show_Failed_Proof_Attempt;

Since this is an assertion that can be executed, it may help you find the
problem if you run the program with assertions enabled on representative
sets of inputs. This allows you to find bugs in both the code and its
contracts. In this case, testing :ada:`Incr_Until` with an input greater than
1000 raises an exception at runtime.

.. code:: ada run_button prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Failed_Proof_Attempt_2
    :class: ada-run-expect-failure, ada-expect-prove-error

    package Show_Failed_Proof_Attempt is

       Incremented : Boolean := False;

       procedure Incr_Until (X : in out Natural) with
         Contract_Cases =>
           (Incremented => X > X'Old,
            others      => X = X'Old);

    end Show_Failed_Proof_Attempt;

    package body Show_Failed_Proof_Attempt is

       procedure Incr_Until (X : in out Natural) is
       begin
          if X < 1000 then
             X := X + 1;
             Incremented := True;
          else
             Incremented := False;
          end if;
       end Incr_Until;

    end Show_Failed_Proof_Attempt;

    with Show_Failed_Proof_Attempt; use Show_Failed_Proof_Attempt;

    procedure Main is
       Dummy : Integer;
    begin
       Dummy := 0;
       Incr_Until (Dummy);

       Dummy := 1000;
       Incr_Until (Dummy);
    end Main;

The error message shows that the first contract case is failing, which
means that :ada:`Incremented` is :ada:`True`. However, if we print the value
of :ada:`Incremented` before returning, we see that it's :ada:`False`, as
expected for the input we provided. The error here is that guards of
contract cases are evaluated before the call, so our specification is
wrong! To correct this, we should either write :ada:`X < 1000` as the guard of
the first case or use a standard postcondition with an if-expression.

Debugging Cases where more Information is Required
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Even if both the code and the assertions are correct, GNATprove may still
report that it can't prove a verification condition for a property. This
can happen for two reasons:

- The property may be unprovable because the code is missing some
  assertion. One category of these cases is due to the modularity of the
  analysis which, as we discussed above, means that GNATprove only knows
  about the properties of your subprograms that you have explicitly
  written.

- There may be some information missing in the logical model of the program
  used by GNATprove.

Let's look at the case where the code and the specification are correct but
there's some information missing. As an example, GNATprove finds the
postcondition of :ada:`Increase` to be unprovable.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Failed_Proof_Attempt_3
    :class: ada-expect-prove-error

    package Show_Failed_Proof_Attempt is

       C : Natural := 100;

       procedure Increase (X : in out Natural) with
          Post => (if X'Old < C then X > X'Old else X = C);

    end Show_Failed_Proof_Attempt;

    package body Show_Failed_Proof_Attempt is

       procedure Increase (X : in out Natural) is
       begin
          if X < 90 then
             X := X + 10;
          elsif X >= C then
             X := C;
          else
             X := X + 1;
          end if;
       end Increase;

    end Show_Failed_Proof_Attempt;

This postcondition is a conditional.  It says that if the parameter (:ada:`X`)
is less than a certain value (:ada:`C`), its value will be increased by the
procedure while if it's greater, its value will be set to :ada:`C`
(saturated). When :ada:`C` has the value 100, the code of :ada:`Increases` adds
10 to the value of :ada:`X` if it was initially less than 90, increments :ada:`X`
by 1 if it was between 90 and 99, and sets :ada:`X` to 100 if it was greater
or equal to 100.  This behavior does satisfy the postcondition, so why is
the postcondition not provable?

The values in the counterexample returned by GNATprove in its message gives
us a clue: :ada:`C = 0 and X = 10 and X'Old = 0`. Indeed, if :ada:`C` is not
equal to 100, our reasoning above is incorrect: the values of 0 for :ada:`C`
and :ada:`X` on entry indeed result in :ada:`X` being 10 on exit, which violates
the postcondition!

We probably didn't expect the value of :ada:`C` to change, or at least not to
go below 90.  But, in that case, we should have stated so by either
declaring :ada:`C` to be constant or by adding a precondition to the
:ada:`Increase` subprogram. If we do either of those, GNATprove is able to
prove the postcondition.

Debugging Prover Limitations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, there are cases where GNATprove provides a perfectly valid
verification condition for a property, but it's nevertheless not proved by
the automatic prover that runs in the later stages of the tool's
execution. This is quite common. Indeed, GNATprove produces its
verification conditions in first-order logic, which is not decidable,
especially in combination with the rules of arithmetic. Sometimes, the
automatic prover just needs more time.  Other times, the prover will
abandon the search almost immediately or loop forever without reaching a
conclusive answer (either a proof or a counterexample).

For example, the postcondition of our :ada:`GCD` function below |mdash| which
calculates the value of the :ada:`GCD` of two positive numbers using Euclide's
algorithm |mdash| can't be verified with GNATprove's default settings.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Failed_Proof_Attempt_4
    :class: ada-expect-prove-error

    package Show_Failed_Proof_Attempt is

       function GCD (A, B : Positive) return Positive with
         Post =>
           A mod GCD'Result = 0
           and B mod GCD'Result = 0;

    end Show_Failed_Proof_Attempt;

    package body Show_Failed_Proof_Attempt is

       function GCD (A, B : Positive) return Positive is
       begin
          if A > B then
             return GCD (A - B, B);
          elsif B > A then
             return GCD (A, B - A);
          else
             return A;
          end if;
       end GCD;

    end Show_Failed_Proof_Attempt;

The first thing we try is increasing the amount of time the prover is
allowed to spend on each verification condition using the ``--timeout``
option of GNATprove (e.g., by using the dialog box in GNAT Studio). In this
example, increasing it to one minute, which is relatively high, doesn't
help. We can also specify an alternative automatic prover |mdash| if we have
one |mdash| using the option ``--prover`` of GNATprove (or the dialog box). For
our postcondition, we tried Alt-Ergo, cvc5, and Z3 without any luck.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Failed_Proof_Attempt_5
    :class: ada-expect-prove-error

    package Show_Failed_Proof_Attempt is

       function GCD (A, B : Positive) return Positive with
         Post =>
           A mod GCD'Result = 0
           and B mod GCD'Result = 0;

    end Show_Failed_Proof_Attempt;

    package body Show_Failed_Proof_Attempt is

       function GCD (A, B : Positive) return Positive
       is
          Result : Positive;
       begin
          if A > B then
             Result := GCD (A - B, B);
             pragma Assert ((A - B) mod Result = 0);
             --  info: assertion proved
             pragma Assert (B mod Result = 0);
             --  info: assertion proved
             pragma Assert (A mod Result = 0);
             --  medium: assertion might fail
          elsif B > A then
             Result := GCD (A, B - A);
             pragma Assert ((B - A) mod Result = 0);
             --  info: assertion proved
          else
             Result := A;
          end if;
          return Result;
       end GCD;

    end Show_Failed_Proof_Attempt;

To better understand the reason for the failure, we added intermediate
assertions to simplify the proof and pin down the part that's causing the
problem.  Adding such assertions is often a good idea when trying to
understand why a property is not proved. Here, provers can't verify that if
both :ada:`A` - :ada:`B` and :ada:`B` can be divided by :ada:`Result` so can :ada:`A`. This
may seem surprising, but non-linear arithmetic, involving, for example,
multiplication, modulo, or exponentiation, is a difficult topic for provers
and is not handled very well in practice by any of the general-purpose ones
like Alt-Ergo, cvc5, or Z3.

.. note::

   For more details on how to investigate unproved checks, see the
   :spark_ugs:`SPARK User's Guide <how_to_investigate_unproved_checks.html>`.


Code Examples / Pitfalls
---------------------------------------------------------------------

We end with some code examples and pitfalls.

Example #1
~~~~~~~~~~

The package :ada:`Lists` defines a linked-list data structure.  We call
:ada:`Link(I,J)` to make a link from index :ada:`I` to index :ada:`J` and call
:ada:`Goes_To(I,J)` to determine if we've created a link from index :ada:`I` to
index :ada:`J`. The postcondition of :ada:`Link` uses :ada:`Goes_To` to state that
there must be a link between its arguments once :ada:`Link` completes.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_01
    :class: ada-expect-prove-error

    package Lists with SPARK_Mode is

       type Index is new Integer;

       function Goes_To (I, J : Index) return Boolean;

       procedure Link (I, J : Index) with Post => Goes_To (I, J);

    private

       type Cell (Is_Set : Boolean := True) is record
          case Is_Set is
             when True =>
                Next : Index;
             when False =>
                null;
          end case;
       end record;

       type Cell_Array is array (Index) of Cell;

       Memory : Cell_Array;

    end Lists;

    package body Lists with SPARK_Mode is

       function Goes_To (I, J : Index) return Boolean is
       begin
          if Memory (I).Is_Set then
             return Memory (I).Next = J;
          end if;
          return False;
       end Goes_To;

       procedure Link (I, J : Index) is
       begin
          Memory (I) := (Is_Set => True, Next => J);
       end Link;

    end Lists;

This example is correct, but can't be verified by GNATprove.  This is
because :ada:`Goes_To` itself has no postcondition, so nothing is known about
its result.


Example #2
~~~~~~~~~~

We now redefine :ada:`Goes_To` as an expression function.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_02

    package Lists with SPARK_Mode is

       type Index is new Integer;

       function Goes_To (I, J : Index) return Boolean;

       procedure Link (I, J : Index) with Post => Goes_To (I, J);

    private

       type Cell (Is_Set : Boolean := True) is record
          case Is_Set is
          when True =>
             Next : Index;
          when False =>
             null;
          end case;
       end record;

       type Cell_Array is array (Index) of Cell;

       Memory : Cell_Array;

       function Goes_To (I, J : Index) return Boolean is
         (Memory (I).Is_Set and then Memory (I).Next = J);

    end Lists;

    package body Lists with SPARK_Mode is

       procedure Link (I, J : Index) is
       begin
          Memory (I) := (Is_Set => True, Next => J);
       end Link;

    end Lists;

GNATprove can fully prove this version: :ada:`Goes_To` is an expression
function, so its body is available for proof (specifically, for creating
the postcondition needed for the proof).


Example #3
~~~~~~~~~~

The package :ada:`Stacks` defines an abstract stack type with a :ada:`Push`
procedure that adds an element at the top of the stack and a function
:ada:`Peek` that returns the content of the element at the top of the stack
(without removing it).

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_03
    :class: ada-expect-prove-error

    package Stacks with SPARK_Mode is

       type Stack is private;

       function  Peek (S : Stack) return Natural;
       procedure Push (S : in out Stack; E : Natural) with
         Post => Peek (S) = E;

    private

       Max : constant := 10;

       type Stack_Array is array (1 .. Max) of Natural;

       type Stack is record
          Top     : Positive;
          Content : Stack_Array;
       end record;

       function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);

    end Stacks;

    package body Stacks with SPARK_Mode is

       procedure Push (S : in out Stack; E : Natural) is
       begin
          if S.Top >= Max then
             return;
          end if;

          S.Top := S.Top + 1;
          S.Content (S.Top) := E;
       end Push;

    end Stacks;

This example isn't correct. The postcondition of :ada:`Push` is only satisfied
if the stack isn't full when we call :ada:`Push`.


Example #4
~~~~~~~~~~

We now change the behavior of :ada:`Push` so it raises an exception when the
stack is full instead of returning.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_04
    :class: ada-expect-prove-error

    package Stacks with SPARK_Mode is

       type Stack is private;

       Is_Full_E : exception;

       function  Peek (S : Stack) return Natural;
       procedure Push (S : in out Stack; E : Natural) with
         Post => Peek (S) = E;

    private

       Max : constant := 10;

       type Stack_Array is array (1 .. Max) of Natural;

       type Stack is record
          Top     : Positive;
          Content : Stack_Array;
       end record;

       function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);

    end Stacks;

    package body Stacks with SPARK_Mode is

       procedure Push (S : in out Stack; E : Natural) is
       begin
          if S.Top >= Max then
             raise Is_Full_E;
          end if;

          S.Top := S.Top + 1;
          S.Content (S.Top) := E;
       end Push;

    end Stacks;

The postcondition of :ada:`Push` is now proved because GNATprove only
considers execution paths leading to normal termination. But it issues a
message warning that exception :ada:`Is_Full_E` may be raised at runtime.


Example #5
~~~~~~~~~~

Let's add a precondition to :ada:`Push` stating that the stack shouldn't be
full.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_05

    package Stacks with SPARK_Mode is

       type Stack is private;

       Is_Full_E : exception;

       function  Peek (S : Stack) return Natural;
       function  Is_Full (S : Stack) return Boolean;
       procedure Push (S : in out Stack; E : Natural) with
         Pre  => not Is_Full (S),
         Post => Peek (S) = E;

    private

       Max : constant := 10;

       type Stack_Array is array (1 .. Max) of Natural;

       type Stack is record
          Top     : Positive;
          Content : Stack_Array;
       end record;

       function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);
       function Is_Full (S : Stack) return Boolean is (S.Top >= Max);

    end Stacks;

    package body Stacks with SPARK_Mode is

       procedure Push (S : in out Stack; E : Natural) is
       begin
          if S.Top >= Max then
             raise Is_Full_E;
          end if;
          S.Top := S.Top + 1;
          S.Content (S.Top) := E;
       end Push;

    end Stacks;

This example is correct. With the addition of the precondition, GNATprove
can now verify that :ada:`Is_Full_E` can never be raised at runtime.


Example #6
~~~~~~~~~~

The package :ada:`Memories` defines a type :ada:`Chunk` that models chunks of
memory.  Each element of the array, represented by its index, corresponds
to one data element.  The procedure :ada:`Read_Record` reads two pieces of
data starting at index :ada:`From` out of the chunk represented by the value
of :ada:`Memory`.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_06
    :class: ada-expect-prove-error

    package Memories is

       type Chunk is array (Integer range <>) of Integer
         with Predicate => Chunk'Length >= 10;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On,
           Pre => From in Memory'First .. Memory'Last - 2
    is
       function Read_One (First : Integer; Offset : Integer) return Integer
         with Pre => First + Offset in Memory'Range
       is
          Value : Integer := Memory (First + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Data1, Data2 : Integer;

    begin
       Data1 := Read_One (From, 1);
       Data2 := Read_One (From, 2);
    end Read_Record;

This example is correct, but it can't be verified by GNATprove, which
analyses :ada:`Read_One` on its own and notices that an overflow may occur in
its precondition in certain contexts.


Example #7
~~~~~~~~~~

Let's rewrite the precondition of :ada:`Read_One` to avoid any possible overflow.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_07
    :class: ada-expect-prove-error

    package Memories is

       type Chunk is array (Integer range <>) of Integer
         with Predicate => Chunk'Length >= 10;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On,
           Pre => From in Memory'First .. Memory'Last - 2
    is
       function Read_One (First : Integer; Offset : Integer) return Integer
         with Pre => First >= Memory'First
                and then Offset in 0 .. Memory'Last - First
       is
          Value : Integer := Memory (First + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Data1, Data2 : Integer;

    begin
       Data1 := Read_One (From, 1);
       Data2 := Read_One (From, 2);
    end Read_Record;

This example is also not correct: unfortunately, our attempt to correct
:ada:`Read_One`'s precondition failed. For example, an overflow will occur at
runtime if :ada:`First` is :ada:`Integer'Last` and :ada:`Memory'Last` is
negative. This is possible here because type :ada:`Chunk` uses :ada:`Integer` as
base index type instead of :ada:`Natural` or :ada:`Positive`.


Example #8
~~~~~~~~~~

Let's completely remove the precondition of :ada:`Read_One`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_08

    package Memories is

       type Chunk is array (Integer range <>) of Integer
         with Predicate => Chunk'Length >= 10;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On,
           Pre => From in Memory'First .. Memory'Last - 2
    is
       function Read_One (First : Integer; Offset : Integer) return Integer is
          Value : Integer := Memory (First + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Data1, Data2 : Integer;

    begin
       Data1 := Read_One (From, 1);
       Data2 := Read_One (From, 2);
    end Read_Record;

This example is correct and fully proved. We could have fixed the contract
of :ada:`Read_One` to correctly handle both positive and negative values of
:ada:`Memory'Last`, but we found it simpler to let the function be inlined for
proof by removing its precondition.


Example #9
~~~~~~~~~~

The procedure :ada:`Compute` performs various computations on its argument.
The computation performed depends on its input range and is reflected in
its contract, which we express using a :ada:`Contract_Cases` aspect.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_09
    :class: ada-expect-prove-error

    procedure Compute (X : in out Integer) with
      Contract_Cases => ((X in -100 .. 100) => X = X'Old * 2,
                         (X in    0 .. 199) => X = X'Old + 1,
                         (X in -199 .. 0)   => X = X'Old - 1,
                          X >=  200          => X =  200,
                          others             => X = -200)
    is
    begin
       if X in -100 .. 100 then
          X := X * 2;
       elsif X in 0 .. 199 then
          X := X + 1;
       elsif X in -199 .. 0 then
          X := X - 1;
       elsif X >= 200 then
          X := 200;
       else
          X := -200;
       end if;
    end Compute;

This example isn't correct. We duplicated the content of :ada:`Compute`'s body
in its contract. This is incorrect because the semantics of
:ada:`Contract_Cases` require disjoint cases, just like a case
statement. The counterexample returned by GNATprove shows that :ada:`X = 0` is
covered by two different case-guards (the first and the second).


Example #10
~~~~~~~~~~~

Let's rewrite the contract of :ada:`Compute` to avoid overlapping cases.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Program_Integrity.Example_10
    :class: ada-expect-prove-error

    procedure Compute (X : in out Integer) with
      Contract_Cases => ((X in    0 ..  199) => X >= X'Old,
                         (X in -199 ..   -1) => X <= X'Old,
                          X >=  200           => X =  200,
                          X <  -200           => X = -200)
    is
    begin
       if X in -100 .. 100 then
          X := X * 2;
       elsif X in 0 .. 199 then
          X := X + 1;
       elsif X in -199 .. 0 then
          X := X - 1;
       elsif X >= 200 then
          X := 200;
       else
          X := -200;
       end if;
    end Compute;

This example is still not correct.  GNATprove can successfully prove the
different cases are disjoint and also successfully verify each case
individually. This isn't enough, though: a :ada:`Contract_Cases` must cover
all cases. Here, we forgot the value -200, which is what GNATprove reports in
its counterexample.
