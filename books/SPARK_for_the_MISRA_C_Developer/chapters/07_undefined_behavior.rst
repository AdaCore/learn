:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Detecting Undefined Behavior
----------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Undefined behavior (and critical unspecified behavior, that we'll include in
undefined behavior for this discussion) are the plague of C programs. Many
rules in MISRA-C actually contribute to avoid undefined behavior, as visible in
the 20 occurrences of "undefined" in the MISRA-C:2012 document.

MISRA-C Rule 1.3 is the rule that rules them all, stating very simply:

   `"There shall be no occurrence of undefined or critical unspecified
   behaviour."`

The deceiving simplicity of this rule rests on the definition of `undefined or
critical unspecified behaviour`. Appendix H of MISRA:C 2012 lists over 10 pages
the hundreds of cases of undefined and critical unspecified behavior in the C
programming language standard, a majority of which are not individually
decidable.

It is therefore not surprising that a majority of MISRA-C checkers stay away
from any serious attempt at verifying MISRA-C Rule 1.3.

Undefined Behavior in SPARK
***************************

As SPARK is a subset of the Ada programming language, SPARK programs may
exhibit two types of undefined behaviors:

- `bounded errors` occur when the program enters a state not defined in the
  programming language semantics, but the consequences are bounded in various
  ways. For example, reading uninitialized data can lead to a bounded error,
  when the value read does not correspond to a valid value for the type of the
  object. In this specific case, the Ada Reference Manual states that either a
  predefined exception is raised or execution continues using the invalid
  representation.

- `erroneous executions` occur when when the program enters a state not defined
  in the programming language semantics, but the consequences are not bounded
  by the Ada Reference Manual. This is the closest to an undefined behavior
  in C. For exemple, concurrently writing through different tasks to the same
  unprotected variable is a case of erroneous behavior.

Many cases of undefined behavior in C are in fact raising exceptions in
SPARK. For example, accessing an array beyond its bounds raises the exception
``Constraint_Error`` while reaching the end of a function without returning a
value raises the exception ``Program_Error``.

SPARK Reference Manual defines the SPARK subset through a combination of
legality rules (checked by the compiler, or the compile-like phase preceding
analysis) and verification rules (checked by the formal analysis tool
GNATprove). Bounded errors and erroneous execution are prevented by a
combination of legality rules and the part of GNATprove called `flow analysis`
which in particular detects potential reads of uninitialized data, as seen in
:ref:`Detecting Read of Uninitialized Data`. In the following, we concentrate
on the verification that no exceptions can be raised.

Proof of Absence of Runtime Errors in SPARK
*******************************************

The most common runtime errors are related to misuse of arithmetic (division by
zero, overflows, exceeding the range of allowed values), arrays (accessing
beyond an array bounds, assigning between arrays of different lengths),
structures (accessing components that are not defined in a given dynamic
variant).

The arithmetic runtime errors can occur with any numeric type: signed integers,
unsigned integers, floating-point, fixed-point. These can occur when applying
arithmetic operations or when converting between numeric types.

Operations on enumerations too can lead to runtime errors when trying to obtain
the predecessor of the first element in an enumeration ``Enum'First'Pred`` or
the successor of the last element in an enumeration ``Enum'Last'Succ``.

Let's look at a simple assignment statement setting the value of the ``I`` +
``J`` th cell of an array of naturals ``A`` to ``P`` / ``Q``.

.. code:: ada spark-prove

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
is ``Integer'Last`` and ``J`` is positive.

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;

Then, its result may not be in the range of the array ``A``.

.. code-block:: ada

    A (A'Last + 1) := P / Q;

On the other side of the assignment, the division may also overflow, but
only in the very special case where ``P`` is ``Integer'First`` and
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

The way to program in SPARK in order to prove the absence of runtime errors is
a combination of:

- more precise types for variables, that give precise ranges to numeric values

- using preconditions and postconditions on subprograms to specify respectively
  the constraints that callers should respect and the guarantees that the
  subprogram should provide on exit

For example, here is a possible way to rewrite the previous program so that we
can guarantee through proof that no possible runtime error can be raised:

.. code:: ada spark-prove

    package No_Runtime_Errors is

       subtype Index is Integer range 0 .. 100;

       type Nat_Array is array (Index range <>) of Natural;

       procedure Update (A : in out Nat_Array; I, J : Index; P, Q : Positive) with
         Pre => I + J in A'Range;

    end No_Runtime_Errors;

    package body No_Runtime_Errors is

       procedure Update (A : in out Nat_Array; I, J : Index; P, Q : Positive) is
       begin
          A (I + J) := P / Q;
       end Update;

    end No_Runtime_Errors;
