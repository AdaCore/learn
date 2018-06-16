Proof of Program Integrity
=====================================================================

.. role:: ada(code)
   :language: ada

This section presents the proof capability provided by the GNATprove tool
supporting SPARK. In this section, we focus on the simpler proofs that are
needed for program integrity.

For its soundness, this analysis relies on the prerequisite of having performed
:ref:`Flow Analysis` and a user should not proceed further if there are still
unjustified flow analysis messages for her program. The primary objective of
performing proof of program integrity is to ensure the absence of any runtime
error during the program's execution.


Runtime Errors
---------------------------------------------------------------------

There is always the potential for errors which can occur at program's execution
but will not be detected during compilation. These errors, called runtime
errors, are those targeted by proof in GNATprove. There are various kinds of
runtime errors, the most common being array out of range access (`buffer
overflow <https://en.wikipedia.org/wiki/Buffer_overflow>`_ in Ada), subtype
range violation, overflows in computations, and division by zero. Taking a look
at the code below to give an example, let us look at a simple assignment
statement setting the value of the ``I`` + ``J`` th cell of an array of
naturals ``A`` to ``P`` / ``Q``.

.. code:: ada

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

If we don't know the values of ``I``, ``J``, ``P``, and ``Q``, then there
is a question of what are the errors which may occur when executing this
code. In fact, there are quite an important number of them.

First, the computation of ``I`` + ``J`` may overflow, for example if ``I``
is :ada:`Integer'Last` and ``J`` is positive.

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;

Then, its result may not be in the range of the array ``A``.

.. code-block:: ada

    A (A'Last + 1) := P / Q;

On the other side of the assignment, the division may also overflow, but
only in the very special case where ``P`` is :ada:`Integer'First` and
``Q`` is -1 because of the asymmetric range of signed integer types.

.. code-block:: ada

    A (I + J) := Integer'First / -1;

As the array contains natural numbers, it is also an error to store a
negative value in it.

.. code-block:: ada

    A (I + J) := 1 / -1;

Finally, the division is not allowed if ``Q`` is 0.

.. code-block:: ada

    A (I + J) := P / 0;

For all those runtime errors, the compiler will generate checks in the
executable code to make sure that no inconsistent state can be reached,
raising an exception if those checks fail. You can see the type of
exceptions raised due to failed checks for each of the different
assignment statements below:

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

Note that these runtime checks are costly, both in terms of program size
and execution time. They do not come at zero cost and therefore, depending
on the context, it may be appropriate to remove them if we can statically
ensure that they can never be needed at runtime.

This is where analysis using GNATprove can be used to demonstrate
statically that none of these errors will ever occur at runtime. More
precisely, GNATprove logically interprets the meaning of every instruction
in the program. Using this interpretation, GNATprove generates a logical
formula called verification condition for each possible check required
for the validity of the code.

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

The verification conditions are then given to an automatic prover. If
every verification condition generated for a program can be validated by a
prover, it means that no error will ever be raised at runtime when
executing this program.


Modularity
---------------------------------------------------------------------

For scalability reasons, GNATprove performs proof of program modularly on a per
subprogram basis. To do this, it relies on preconditions and postconditions to
properly summarize the input and output state of each subprogram. More
precisely, when verifying the body of a subprogram, GNATprove assumes it knows
nothing about the possible initial values of its parameters and of the global
variables it accesses except what is stated in the subprogram's
precondition. If no precondition is given, then no assumptions can be made.

For example, the following code shows the body of ``Increment`` can be
successfully verified as its precondition constrains the value of its
parameter ``X`` to be less than :ada:`Integer'Last`.

In the same way, when a subprogram is called, GNATprove assumes its :ada:`out`
and :ada:`in out` parameters and the global variables it writes can be modified
in any way compatible with its postcondition. For example, since ``Increment``
has no postcondition, GNATprove does not know that ``X`` is smaller than
:ada:`Integer'Last` after the call. Therefore, it cannot prove that the
addition following the call to ``Increment`` cannot overflow.

.. code:: ada spark-report-all

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
       --  Here GNATprove does not know the value of X

       X := X + 1;
       --  medium: overflow check might fail
    end Show_Modularity;

Exceptions
~~~~~~~~~~

There are two cases where modularity is not enforced by GNATprove. First,
local subprograms without contracts can be inlined if they are simple
enough. In particular, they should not be recursive or have multiple return
points. If we remove the contract from ``Increment`` then it fits the
criteria for inlining.

.. code:: ada spark-report-all

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

As GNATprove sees the call to ``Increment`` exactly as if the increment on
``X`` was done directly, it can verify successfully that no overflow may
occur on either of the subsequent additions. The other case concerns
expression functions. If a function is defined as an expression function,
with or without contracts, then it is handled as if it had a postcondition
stating the value of its result.

In our example, replacing ``Increment`` with an expression function allows
GNATprove to verify successfully the overflow check in the following
addition.

.. code:: ada spark-report-all

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

Contracts
---------------------------------------------------------------------

Though they are perfectly suited for formal verification, Ada
contracts are primarily designed to be checked at runtime. Code that
verifies the contracts at runtime can be generated by the compiler using
the switch ``-gnata``. If an Ada contract does
not hold at a given subprogram call, the exception
:ada:`Assert_Failure` will be raised. This is particularly convenient
during development and testing, but execution of assertions, and in
particular of preconditions, may also be retained during the program's
deployment to avoid reaching an inconsistent state.

Consider the incorrect call to ``Increment`` below, which violates its
precondition. One way to detect this error is by compiling the function with
assertions enabled and testing is on suitable inputs that trigger the
violation. Another way which does not require guessing suitable inputs is to
run GNATprove.

.. code:: ada run_button
   :class: ada-run-expect-failure

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

Similarly, consider the incorrect implementation for function ``Absolute``
below, which violates its postcondition. One way to detect this error is by
compiling the function with assertions enabled and testing is on suitable
inputs that trigger the violation. Another way which does not require guessing
suitable inputs is to run GNATprove.

.. code:: ada run_button
   :class: ada-run-expect-failure

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

The benefits of dynamically checking contracts extends beyond testing. It can
be profitable to enable these checks at runtime to stop execution before some
damaging event. This early failure detection allows an easier recovery and
facilitates debugging.

GNATprove analyses preconditions and postcondition statically. Like in the
runtime semantics of contracts, preconditions are verified every time a
subprogram is called.  Postconditions, on the other hand, are verified
modularly once and for all as part of the verification of the subprogram's
body. For example, it has to wait until ``Increment`` is improperly called to
detect the precondition violation, as a precondition is really a contract for
the caller. On the other hand, it does not need ``Absolute`` to be called to
detect that its postcondition does not hold on all its possible inputs.


Executable Semantics
~~~~~~~~~~~~~~~~~~~~

In Ada, expressions in contracts have the regular semantics of
Boolean expressions. In particular, runtime errors may occur during their
computation. To facilitate both debugging of assertions and combining
testing and static verification, the same semantics is used by GNATprove.

During proof of programs, it makes sure that no error will ever be raised
during the execution of the contracts. This semantic may sometimes be
considered too heavy, in particular regarding overflow checks. For example, it
makes it harder to specify an appropriate precondition for the function ``Add``
below:

.. code:: ada run_button
   :class: ada-run-expect-failure

    procedure Show_Executable_Semantics
      with SPARK_Mode => On
    is
       function Add (X, Y : Integer) return Integer is (X + Y)
         with Pre => X + Y in Integer;

       X : Integer;
    begin
       X := Add (Integer'Last, 1);
    end Show_Executable_Semantics;

GNATprove issues a message on this code about a possible overflow when
computing the addition of ``X`` and ``Y`` in the precondition. Indeed, as
expressions in assertions have the regular Ada semantics, this addition may
overflow, as one can see immediately by compiling and running the code that
calls ``Add`` with arguments :ada:`Integer'Last` and 1.

On the other hand, depending on the context, we may have preferred to have
GNATprove use the mathematical semantics of addition and properly verify that
no error will ever be raised at runtime in the body of ``Add``. This behavior
may be obtained by using the compiler switch ``-gnato??`` (for example
``--gnato13``) which allows to independently set the overflow mode in code and
assertions to either reduce the number of overflow checks or to completely
eliminate them. Note that this switch will also make the compiler avoid
overflows at runtime.


Additional Assertions and Contracts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we have seen, the ability to state properties to check in assertions and
contracts is a key feature of SPARK. It supports preconditions and
postconditions, as well as assertions introduced by the pragma :ada:`Assert`.

New contracts have also been introduced for the process of formal
verification. For example, the new pragma :ada:`Assume` is handled as an
assertion at execution but introduces an assumption for proof of program,
that is, a Boolean expression which is assumed to be true by the tool
without any verification. This feature is useful but must be used with
great care.

.. code:: ada spark-report-all

    procedure Incr (X : in out Integer) is
    begin
       pragma Assume (X < Integer'Last);
       X := X + 1;
    end Incr;

Another construct introduced for GNATprove is the :ada:`Contract_Cases`
aspect. It allows to specify the behavior of a subprogram with a disjunction of
cases. Each element of a contract-cases is in fact a small contract made of a
guard, which may only reference subprogram's inputs and is evaluated before the
call, and of a consequence. At each call of the subprogram, there must be one
and only one case for which the guard evaluates to :ada:`True`. The consequence
of this case is the one that should hold on exit.

.. code:: ada spark-report-all

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

In GNATprove, validity --- as well as disjointness and completeness of the
:ada:`Contract_Cases` --- is verified only once in the context of the
subprogram's precondition.


.. _Debugging Failed Proof Attempts:

Debugging Failed Proof Attempts
---------------------------------------------------------------------

If GNATprove reports an error while verifying a program, it may be for
different reasons:

- there might be an error in the program, or

- the property may not be provable because of some missing information, or

- the prover used by GNATprove may be unable to prove a perfectly valid
  property.

The remainder of this section is dedicated to the sometimes tricky task of
debugging failed proof attempts.

Debugging Errors in Code or Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, let us look at the case where there is indeed an error in the
program. There are two possibilities: the code may be incorrect, or, and
it is equally likely, the specification may be incorrect. As an example,
there is an error in our procedure ``Incr_Until`` which makes its
:ada:`Contract_Cases` unprovable.

.. code:: ada

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

As assertions can be executed, it may help to test the program on a
representative set of inputs with assertions enabled. This allows bugs to
be found both in the code and in its contracts. For example, testing
``Incr_Until`` on an input bigger than 1000 will raise an exception at
runtime.

.. code:: ada run_button
   :class: ada-run-expect-failure

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
       X : Integer;
    begin
       X := 0;
       Incr_Until (X);

       X := 1000;
       Incr_Until (X);
    end Main;

It shows a case where the first contract case is failing, which means that
``Incremented`` is :ada:`True`. Still, if we print the value of ``Incremented``
before returning, we will see that it is :ada:`False`, as expected for such an
input. What occurs here is that guards of contract cases are evaluated before
the call, so our specification is wrong! To correct this, we should either put
``X < 1000`` as a guard of the first case or use a standard postcondition with
an if-expression instead.

Debugging Missing Information Causes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Even if both the code and the assertions are correct, GNATprove may still
generate an unprovable verification condition for a property. This may
happen for two reasons:

- First, the property may be unprovable because some assertion is missing in
  the code. In particular, this can be induced by the modularity of the
  analysis which causes the tool to only know explicitly written properties
  about some data.

- Second, there may also be some missing information in the logical model of
  the program used by GNATprove. This is the case for example for the content
  of string literals.

Let's look at the case where the code and the specification are correct, but
there is some missing information. As an example, the verification generated by
GNATprove for the postcondition of ``Increase`` is unprovable.

.. code:: ada

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

This postcondition states that, if the parameter ``X`` is smaller than a
certain value ``C``, then its value will be increased by the procedure, whereas
if it is bigger, its value will be saturated to ``C``. Indeed, using 100 for
value of ``C``, the code of ``Increases`` will bump the value ``X`` by 10 for
``X`` less than 90, it will bump its value by 1 for ``X`` between 90 and 99 and
it will set it to 100 for ``X`` greater or equal to 100. It does respect the
postcondition, so why is the postcondition not provable?

The values in the counterexample returned by GNATprove in its message give a
clue: :ada:`C = 0 and X = 10 and X'Old = 0`. Indeed, if ``C`` is not equal to 100,
out reasoning above breaks! And the values 0 for ``C`` and ``X`` on entry
indeed result in ``X`` being 10 on exit, which violates the postcondition!

Maybe we did not expect the value of ``C`` to change, or at least not to go
below 90. In this case, we should simply state so by either declaring ``C`` to
be constant or by adding a precondition to the ``Increase`` subprogram. In both
cases, GNATprove is able to prove the postcondition.

Debugging Prover Limitations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, there are cases where GNATprove provides a perfectly valid
verification condition for a property, but it is not proved by the
automatic prover in latter stages of the tool execution. This is quite a
common occurrence. Indeed, GNATprove produces its verification conditions
in first order logic, which is not decidable, especially in combination
with arithmetic. Sometimes, the automatic prover just needs more time. But
also sometimes, the prover will abandon the search almost immediately or
loop forever without reaching a conclusive answer.

For example, the postcondition of our ``GCD`` function --- which
calculates the value of the ``GCD`` of two positive numbers using
Euclide's algorithm --- cannot be verified with GNATprove's default
settings.

.. code:: ada

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

The first thing to try is to increase the maximal amount of time that the
prover is allowed to spend on each verification condition using the option
``--timeout`` of GNATprove or the dialog box inside GPS. In our example,
bumping it to one minute, which is relatively high, does not help. We can
also specify an alternative automatic prover --- if we have one --- using
the option ``--prover`` of GNATprove or the dialog box. For our
postcondition, we have tried both Alt-Ergo, CVC4 and Z3 without any luck.

.. code:: ada spark-report-all

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

To better understand the problem, we have added intermediate assertions to
simplify the proof and pin down the part that was causing the problem.
This is often a good idea when trying to understand by review why a
property is not proved. Here, provers cannot verify that, if ``A`` - ``B``
and ``B`` can be divided by ``Result``, then so does ``A``. This may seem
surprising, but non-linear arithmetic, involving multiplication, modulo,
or exponentiation for example, is a difficult topic for provers and is not
handled very well in practice by any of the general-purpose ones like
Alt-Ergo, CVC4 or Z3.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

The package ``Lists`` defines a linked-list data structure that can be updated
by calling ``Link(I,J)`` to insert a link from index ``I`` to index ``J``, and
queried by calling ``Goes_To(I,J)`` to know if there is a link from index ``I``
to index ``J``. The postcondition of ``Link`` states that there should be a
link between its arguments using ``Goes_To``.

.. code:: ada

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

This example is correct, but it cannot be verified with GNATprove. As
``Goes_To`` itself has no postcondition, nothing is known about its result.


Example #2
~~~~~~~~~~

We now redefine ``Goes_To`` as an expression function.

.. code:: ada spark-report-all

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

GNATprove can fully prove this version. As ``Goes_To`` is an expression
function, its body is available for proof.


Example #3
~~~~~~~~~~

The package ``Stacks`` defines an abstract stack type with a ``Push`` procedure
to add an element at the top of the stack, and a function ``Peek`` to peek at
the element at the top of the stack.

.. code:: ada

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

This example is not correct. The postcondition of ``Push`` is only true if
the stack is not full when ``Push`` is called.


Example #4
~~~~~~~~~~

We now change the behavior of ``Push`` to raise an exception instead of
returning when the stack is full.

.. code:: ada

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

The postcondition of ``Push`` is now proved, as it only concerns execution
paths leading to normal termination. But GNATprove issues a message warning
that exception ``Is_Full_E`` may be raised at runtime.


Example #5
~~~~~~~~~~

Let's add a precondition to ``Push`` stating that the stack should not be full.

.. code:: ada spark-report-all

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

This example is correct. In the context of the precondition, GNATprove can
now verify that ``Is_Full_E`` can never be raised at runtime.


Example #6
~~~~~~~~~~

The package ``Memories`` defines a type ``Chunk`` representing chunks of
memory, where some distinguished data elements can be interpreted as indexes
into the array. The procedure ``Read_Record`` reads two pieces of data from its
``Memory`` chunk passed in parameter starting at index ``From``.

.. code:: ada

    package Memories is

       type Chunk is array (Integer range <>) of Integer;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On
    is
       function Read_One (First : Integer; Offset : Integer) return Integer
         with Pre => Memory (First) + Offset in Memory'Range
       is
          Value : Integer := Memory (Memory (First) + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Size, Data1, Data2, Addr : Integer;

    begin
       Size := Read_One (From, 0);
       pragma Assume (Size in 1 .. 10
                      and then Memory (From) < Integer'Last - 2 * Size);

       Data1 := Read_One (From, 1);

       Addr  := Read_One (From, Size + 1);
       pragma Assume (Memory (Addr) > Memory (From) + Size);

       Data2 := Read_One (Addr, -Size);
    end Read_Record;

It is correct, but it cannot be verified with GNATprove. GNATprove
analyses ``Read_One`` on its own and notices that an overflow may occur in
its precondition in certain contexts.


Example #7
~~~~~~~~~~

Let's rewrite the precondition of ``Read_One`` to avoid any possible overflow.

.. code:: ada

    package Memories is

       type Chunk is array (Integer range <>) of Integer;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On
    is
       function Read_One (First : Integer; Offset : Integer) return Integer
         with Pre => Memory (First) <= Memory'Last - Offset
       is
          Value : Integer := Memory (Memory (First) + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Size, Data1, Data2, Addr : Integer;

    begin
       Size := Read_One (From, 0);
       pragma Assume (Size in 1 .. 10
                      and then Memory (From) < Integer'Last - 2 * Size);

       Data1 := Read_One (From, 1);

       Addr  := Read_One (From, Size + 1);
       pragma Assume (Memory (Addr) > Memory (From) + Size);

       Data2 := Read_One (Addr, -Size);
    end Read_Record;

This example is not correct. Unfortunately, our attempt to correct
``Read_One``'s precondition failed. For example, an overflow will occur at
runtime when ``Memory (First)`` is :ada:`Integer'Last` and ``Offset`` is
negative.


Example #8
~~~~~~~~~~

Let's remove completely the precondition of ``Read_One``.

.. code:: ada spark-report-all

    package Memories is

       type Chunk is array (Integer range <>) of Integer;

       function Is_Too_Coarse (V : Integer) return Boolean;

       procedure Treat_Value (V : out Integer);

    end Memories;

    with Memories; use Memories;

    procedure Read_Record (Memory : Chunk; From : Integer)
      with SPARK_Mode => On
    is
       function Read_One (First : Integer; Offset : Integer) return Integer is
          Value : Integer := Memory (Memory (First) + Offset);
       begin
          if Is_Too_Coarse (Value) then
             Treat_Value (Value);
          end if;
          return Value;
       end Read_One;

       Size, Data1, Data2, Addr : Integer;

    begin
       Size := Read_One (From, 0);
       pragma Assume (Size in 1 .. 10
                      and then Memory (From) < Integer'Last - 2 * Size);

       Data1 := Read_One (From, 1);

       Addr  := Read_One (From, Size + 1);
       pragma Assume (Memory (Addr) > Memory (From) + Size);

       Data2 := Read_One (Addr, -Size);
    end Read_Record;

This example is correct and fully proved. We could have fixed the contract on
``Read_One`` to handle correctly positive and negative values of
``Offset``. However, we found it simpler to let the function be inlined for
proof by removing its precondition.


Example #9
~~~~~~~~~~

The procedure ``Compute`` does various computations on its argument depending
on its input range, that are reflected in its contract expressed using a
``Contract_Cases`` aspect.

.. code:: ada

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

This example is not correct. We duplicated in ``Compute``'s contract the
content of its body. This is not correct with respect to the semantics of
:ada:`Contract_Cases` which expects disjoint cases, like a case statement. The
counterexample returned by GNATprove points out the case of ``X = 0`` which is
covered by two different case-guards (the first and the second).


Example #10
~~~~~~~~~~~

Let's rewrite the contract of ``Compute`` to avoid overlapping cases.

.. code:: ada

    procedure Compute (X : in out Integer) with
      Contract_Cases => ((X in    1 ..  199) => X >= X'Old,
                         (X in -199 ..   -1) => X <= X'Old,
                          X >=  200           => X =  200,
                          X <= -200           => X = -200)
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

This example is still not correct. Here, GNATprove can successfully check that
the different cases are disjoint. It can also successfully verify each case on
its own. This is not enough though, as a :ada:`Contract_Cases` must also be
total. Here, we forgot the value 0, which is reported by GNATprove in its
counterexample.
