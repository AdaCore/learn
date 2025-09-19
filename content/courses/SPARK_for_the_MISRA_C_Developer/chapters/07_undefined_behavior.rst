.. _SPARK_For_MISRA_C_Dev_Detecting_Undefined_Behavior:

Detecting Undefined Behavior
----------------------------

.. include:: ../../../global.txt

Undefined behavior (and critical unspecified behavior, which we'll treat as
undefined behavior) are the plague of C programs. Many
rules in MISRA C are designed to avoid undefined behavior, as evidenced by
the twenty occurrences of "undefined" in the MISRA C:2012 document.

MISRA C Rule 1.3 is the overarching rule, stating very simply:

   `"There shall be no occurrence of undefined or critical unspecified
   behaviour."`

The deceptive simplicity of this rule rests on the definition of `undefined or
critical unspecified behaviour`. Appendix H of MISRA:C 2012 lists
hundreds of cases of undefined and critical unspecified behavior in the C
programming language standard, a majority of which are not individually
decidable.

It is therefore not surprising that a majority of MISRA C checkers do
not make a serious attempt to verify compliance with MISRA C Rule 1.3.

Preventing Undefined Behavior in SPARK
**************************************

Since SPARK is a subset of the Ada programming language, SPARK programs may
exhibit two types of undefined behaviors that can occur in Ada:

- `bounded error`: when the program enters a state not defined by the
  language semantics, but the consequences are bounded in various
  ways. For example, reading uninitialized data can lead to a bounded error,
  when the value read does not correspond to a valid value for the type of the
  object. In this specific case, the Ada Reference Manual states that either a
  predefined exception is raised or execution continues using the invalid
  representation.

- `erroneous execution`: when when the program enters a state not defined
  by the language semantics, but the consequences are not bounded
  by the Ada Reference Manual. This is the closest to an undefined behavior
  in C. For example, concurrently writing through different tasks to the same
  unprotected variable is a case of erroneous execution.

Many cases of undefined behavior in C would in fact raise exceptions in
SPARK. For example, accessing an array beyond its bounds raises the exception
:ada:`Constraint_Error` while reaching the end of a function without returning a
value raises the exception :ada:`Program_Error`.

The SPARK Reference Manual defines the SPARK subset through a combination of
*legality rules* (checked by the compiler, or the compiler-like phase preceding
analysis) and *verification rules* (checked by the formal analysis tool
GNATprove). Bounded errors and erroneous execution are prevented by a
combination of legality rules and the `flow analysis` part of GNATprove,
which in particular detects potential reads of uninitialized data, as described in
:ref:`SPARK_For_MISRA_C_Dev_Detecting_Read_Of_Uninitialized_Data`. The following discussion focuses
on how SPARK can verify that no exceptions can be raised.

Proof of Absence of Run-Time Errors in SPARK
********************************************

The most common run-time errors are related to misuse of arithmetic (division by
zero, overflows, exceeding the range of allowed values), arrays (accessing
beyond an array bounds, assigning between arrays of different lengths), and
structures (accessing components that are not defined for a given variant).

Arithmetic run-time errors can occur with signed integers,
unsigned integers, fixed-point and floating-point (although with
IEEE 754 floating-point arithmetic, errors are manifest as special
run-time values such as NaN and infinities rather than as exceptions
that are raised). These errors can occur when applying
arithmetic operations or when converting between numeric types (if the
value of the expression being converted is outside the range of the
type to which it is being converted).

Operations on enumeration values can also lead to run-time errors; e.g.,
:ada:`T'Pred(T'First)` or :ada:`T'Succ(T'Last)` for an enumeration type :ada:`T`,
or :ada:`T'Val(N)` where :ada:`N` is an integer value that
is outside the range :ada:`0 .. T'Pos(T'Last)`.

The :ada:`Update` procedure below contains what appears to be a simple assignment
statement, which sets the value of array element :ada:`A(I+J)`  to :ada:`P/Q`.

.. code:: ada prove_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Undefined_Behavior.Runtime_Errors
    :class: ada-expect-prove-error

    package Show_Runtime_Errors is

       type Nat_Array is array (Integer range <>) of Natural;
       --  The values in subtype Natural are 0 , 1, ... Integer'Last

       procedure Update (A : in out Nat_Array; I, J, P, Q : Integer);

    end Show_Runtime_Errors;

    package body Show_Runtime_Errors is

       procedure Update (A : in out Nat_Array; I, J, P, Q : Integer) is
       begin
          A (I + J) := P / Q;
       end Update;

    end Show_Runtime_Errors;

However, for an arbitrary invocation of this procedure, say
:ada:`Update(A, I, J, P, Q)`, an exception can be raised in a variety of
circumstances:

* The computation :ada:`I+J` may overflow, for example if :ada:`I`
  is :ada:`Integer'Last` and :ada:`J` is positive.

  .. code-block:: ada

     A (Integer'Last + 1) := P / Q;

* The value of :ada:`I+J` may be outside the range of the array :ada:`A`.

  .. code-block:: ada

     A (A'Last + 1) := P / Q;

* The division :ada:`P / Q` may overflow in the special case where :ada:`P`
  is :ada:`Integer'First` and :ada:`Q` is :ada:`-1`, because of the asymmetric
  range of signed integer types.

  .. code-block:: ada

     A (I + J) := Integer'First / -1;

* Since the array can only contain non-negative numbers (the element subtype
  is :ada:`Natural`), it is also an error to store a negative value in it.

  .. code-block:: ada

    A (I + J) := 1 / -1;

* Finally, if :ada:`Q` is 0 then a divide by zero error will occur.

  .. code-block:: ada

    A (I + J) := P / 0;

For each of these potential run-time errors, the compiler will generate checks in the
executable code, raising an exception if any of the checks fail:

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;
    --  raised CONSTRAINT_ERROR : overflow check failed

    A (A'Last + 1) := P / Q;
    --  raised CONSTRAINT_ERROR : index check failed

    A (I + J) := Integer'First / (-1);
    --  raised CONSTRAINT_ERROR : overflow check failed

    A (I + J) := 1 / (-1);
    --  raised CONSTRAINT_ERROR : range check failed

    A (I + J) := P / 0;
    --  raised CONSTRAINT_ERROR : divide by zero

These run-time checks incur an overhead in program size
and execution time. Therefore it may be appropriate to remove them
if we are confident that they are not needed.

The traditional way to obtain the needed confidence is through testing,
but it is well known that this can never be complete, at least for
non-trivial programs. Much better is to guarantee the absence of
run-time errors through sound static analysis, and that's where
SPARK and GNATprove can help.

More precisely, GNATprove logically interprets the meaning of every instruction
in the program, taking into account both control flow and data/information
dependencies. It uses this analysis to generate a logical
formula called a *verification condition* for each possible check.

.. code-block:: ada

    A (Integer'Last + 1) := P / Q;
    --  medium: overflow check might fail

    A (A'Last + 1) := P / Q;
    --  medium: array index check might fail

    A (I + J) := Integer'First / (-1);
    --  medium: overflow check might fail

    A (I + J) := 1 / (-1);
    --  medium: range check might fail

    A (I + J) := P / 0;
    --  medium: divide by zero might fail

The verification conditions are then given to an automatic prover. If
every verification condition can be proved, then no run-time errors will
occur.

GNATprove's analysis is sound |mdash| it will detect all possible instances of
run-time exceptions being raised |mdash| while also having high precision
(i.e., not producing a cascade of "false alarms").

The way to program in SPARK so that GNATprove can guarantee the absence of run-time
errors entails:

- declaring variables with precise constraints, and in particular to specify
  precise ranges for scalars; and

- defining preconditions and postconditions on subprograms, to specify respectively
  the constraints that callers should respect and the guarantees that the
  subprogram should provide on exit.

For example, here is a revised version of the previous example, which
can guarantee through proof that no possible run-time error can be raised:

.. code:: ada prove_report_all_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Undefined_Behavior.No_Runtime_Errors

    package No_Runtime_Errors is

       subtype Index_Range is Integer range 0 .. 100;

       type Nat_Array is array (Index_Range range <>) of Natural;

       procedure Update (A    : in out Nat_Array;
                         I, J : Index_Range;
                         P, Q : Positive)
       with
         Pre => I + J in A'Range;

    end No_Runtime_Errors;

    package body No_Runtime_Errors is

       procedure Update (A    : in out Nat_Array;
                         I, J : Index_Range;
                         P, Q : Positive) is
       begin
          A (I + J) := P / Q;
       end Update;

    end No_Runtime_Errors;
