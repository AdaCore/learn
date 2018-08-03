:next_state: False

Proof of Functional Correctness
=====================================================================

.. role:: ada(code)
   :language: ada

This section is dedicated to functional correctness of programs. It
considers and presents advanced proof features that may be required to
specify or verify complex program properties.

Beyond Program Integrity
---------------------------------------------------------------------

The correctness of a program is the fact that it complies with its
specification. In functional correctness, we are specifically concerned
with relations between the subprogram's inputs and outputs, as opposed to,
for example, time or memory consumption. The properties specified for a
program are usually stronger than what is required for program integrity.
In certification processes, the properties should be derived from the
requirements of the system. They can also come from more informal sources,
like the program's documentation, comments in the code, or test oracles.

For example, to ensure no runtime error is raised when using the result of
function ``Find``, it may be enough to know that, whenever it is not 0, then it
is in ``A``'s range. This can be expressed as a postcondition of ``Find``, and
proved automatically by GNATprove:

.. code:: ada spark-report-all

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post => Find'Result in 0 | A'Range;

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;

    end Show_Find;

However, for the program to be meaningful, we may want ``Find`` to verify more
complex properties. For example that it only returns 0 if ``E`` is not in ``A``
and that, otherwise, it returns an index of ``A`` where ``E`` is stored. This
can also be expressed as a postcondition of ``Find``, but GNATprove does not
prove it automatically:

.. code:: ada

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post =>
           (if (for all I in A'Range => A (I) /= E)
              then Find'Result = 0
            else Find'Result in A'Range and then A (Find'Result) = E);

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;

    end Show_Find;

We will see later that we should help GNATprove here by providing a loop
invariant, which is both checked by GNATprove and allows it to prove
automatically the above postcondition for ``Find``.

Writing at least part of the program's specification in the form of
contracts has certain advantages. First, these contracts can be executed
during test campaigns. This improves the maintainability of the code by
detecting earlier discrepancies between the program and its specification.
If they are precise enough, these contracts may be used as oracles to
decide whether a given test passed or failed. As such, they may serve to
verify the outputs of specific units while running the complete code. This
may, in certain context, replace the need for unit testing by running
integration tests with assertions enabled. Subsequently, if the code is in
SPARK, these contracts can also be formally proven using GNATprove.

Formal proof has the advantage of verifying all possible executions,
something which is not always possible using dynamic verification. For
example, during a test campaign, the postcondition of the subprogram
``Find`` shown below will be checked dynamically on every set of inputs on
which ``Find`` is called:

.. code:: ada run_button

    with Ada.Text_IO; use Ada.Text_IO;

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post =>
           (if (for all I in A'Range => A (I) /= E)
              then Find'Result = 0
            else Find'Result in A'Range and then A (Find'Result) = E);

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;

    end Show_Find;

    with Ada.Text_IO; use Ada.Text_IO;
    with Show_Find; use Show_Find;

    procedure Use_Find with
      SPARK_Mode => Off
    is
       Seq : constant Nat_Array (1 .. 3) := (1, 5, 3);
       Res : Natural;
    begin
       Res := Find (Seq, 3);
       Put_Line ("Found 3 in index #" & Natural'Image (Res) & " of array");
    end Use_Find;

However, if ``Find`` is formally verified, then its postcondition is
checked for all possible inputs as well. Static verification can also be
attempted earlier than testing in the development as it works modularly on
a per subprogram basis. For example, in the code shown above, ``Use_Find``
can be formally verified even before the subprogram ``Find`` has a body.


Advanced Contracts
---------------------------------------------------------------------

As contracts for functional correctness are usually more involved than
contracts for program integrity, they more often require the use of the
new forms of expressions introduced by the Ada 2012 standard. In
particular, quantified expressions, which allow to specify properties that
should hold for all or for at least one element of a range, come in handy
when specifying properties for arrays.

As contracts become more complex, it can be useful to introduce new
abstractions in order to improve their readability. Expression functions
are a good means to this end as their body can stay in the package's
specification.

Finally, some properties that are more invariants over data than
properties of subprograms, may be cumbersome to express as subprogram's
contracts. Type predicates, which should hold for every object of a given
type, are usually a better match for this purpose. As an example:

.. code:: ada spark-report-all

    package Show_Sort is

       type Nat_Array is array (Positive range <>) of Natural;

       function Is_Sorted (A : Nat_Array) return Boolean is
         (for all I in A'Range =>
            (if I < A'Last then A (I) <= A (I + 1)));
       --  Returns True if A is sorted in increasing order.

       subtype Sorted_Nat_Array is Nat_Array with
         Dynamic_Predicate => Is_Sorted (Sorted_Nat_Array);
       --  Elements of type Sorted_Nat_Array are all sorted.

       Good_Array : Sorted_Nat_Array := (1, 2, 4, 8, 42);
    end Show_Sort;

Here, the subtype ``Sorted_Nat_Array`` can be used to type an array
variable that should remain sorted throughout the program. As specifying
that an array is sorted requires a rather complex expression involving
quantifiers, this property is abstracted away as an expression function in
our example to improve readability. The fact that ``Is_Sorted``'s body
remains in the package's specification allows users of the package to
retain a precise knowledge of its exact meaning when necessary.


Ghost Code
~~~~~~~~~~

As the properties we want to specify grow more complex, the need can arise
for entities that are only used for the purpose of specification.
It may be important to make sure that these new
entities cannot affect the behavior of the program, or even to ensure they
are removed from production code. This concept, usually called ghost code,
is supported in SPARK by the new :ada:`Ghost` aspect.

The :ada:`Ghost` aspect can be used to annotate any normal entity, such as
variables, types, subprograms, or packages. If an entity is marked as
:ada:`Ghost`, GNATprove will make sure that it cannot affect the program's
behavior. To be able to dynamically test the contracts using it, ghost
code will be executed like normal code when the program is compiled with
assertions enabled. The compiler can also be instructed not to generate
code for ghost entities.

Consider the procedure ``Do_Something`` which calls a first complex treatment
on its input ``X`` and wants to check afterwards that the input and modified
values of ``X`` are related in some way:

.. code:: ada spark-report-all

    package Show_Ghost is

       type T is record
          A, B, C, D, E : Boolean;
       end record;

       function Formula (X : T) return Boolean is
         ((X.A and X.B) or (X.C and (X.D or X.E)));

       function Is_Correct (X, Y : T) return Boolean is
         (Formula (X) = Formula (Y));

       procedure Do_Something (X : in out T);

    end Show_Ghost;

    package body Show_Ghost is

       procedure Do_Some_Complex_Stuff (X : in out T) is
       begin
          X := T'(X.B, X.A, X.C, X.E, X.D);
       end Do_Some_Complex_Stuff;

       procedure Do_Something (X : in out T) is
          X_Init : constant T := X with Ghost;
       begin
          Do_Some_Complex_Stuff (X);
          pragma Assert (Is_Correct (X_Init, X));
          --  It is OK to use X_Init inside an assertion.
       end Do_Something;

    end Show_Ghost;

Here, the ``Do_Something`` subprogram stores the initial value of ``X`` in
a ghost constant called ``X_Init``. We can then reference this variable
from assertions to check that the computation performed by the call to the
``Do_Some_Complex_Stuff`` subprogram modified the value of ``X`` in the
expected manner.

However ``X_Init`` should not be used in normal code, for example to
restore the initial value of ``X``:

.. code:: ada run_button
    :class: ada-expect-compile-error

    package Show_Ghost is

       type T is record
          A, B, C, D, E : Boolean;
       end record;

       function Formula (X : T) return Boolean is
         ((X.A and X.B) or (X.C and (X.D or X.E)));

       function Is_Correct (X, Y : T) return Boolean is
         (Formula (X) = Formula (Y));

       procedure Do_Something (X : in out T);

    end Show_Ghost;

    package body Show_Ghost is

       procedure Do_Some_Complex_Stuff (X : in out T) is
       begin
          X := T'(X.B, X.A, X.C, X.E, X.D);
       end Do_Some_Complex_Stuff;

       procedure Do_Something (X : in out T) is
          X_Init : constant T := X with Ghost;
       begin
          Do_Some_Complex_Stuff (X);
          pragma Assert (Is_Correct (X_Init, X));

          X := X_Init; -- ERROR

       end Do_Something;

    end Show_Ghost;

    with Show_Ghost; use Show_Ghost;

    procedure Use_Ghost is
       X : T := (True, True, False, False, True);
    begin
       Do_Something (X);
    end Use_Ghost;

When compiling this example, the use of ``X_Init`` is
flagged as illegal by the compiler. Note that more complex cases of
interference between ghost and normal code may only be detected by running
GNATprove.


Ghost Functions
~~~~~~~~~~~~~~~

Functions only used in specifications are a rather common occurrence when
writing contracts for functional correctness. For example, expression
functions used to simplify or factor out common patterns in contracts can
usually be marked as ghost.

But ghost functions can do more than improve readability. In real world
programs, it is often the case that, because of abstraction, some
information necessary for functional specification is not accessible in
the package's specification.

Making this information available to users of the packages is generally
out of the question as it would break the abstraction principle. Ghost
functions come in handy here as they provide a way to give access to
information that will not be available to normal client code.

Let's look at the following example:

.. code:: ada

    package Stacks is

       pragma Unevaluated_Use_Of_Old (Allow);

       type Stack is private;

       type Element is new Natural;
       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;

       function Get_Model (S : Stack) return Element_Array with Ghost;
       --  Returns an array as a model of a stack.

       procedure Push (S : in out Stack; E : Element) with
         Pre  => Get_Model (S)'Length < Max,
         Post => Get_Model (S) = Get_Model (S)'Old & E;

    private

       subtype Length_Type is Natural range 0 .. Max;

       type Stack is record
          Top     : Length_Type := 0;
          Content : Element_Array (1 .. Max) := (others => 0);
       end record;

    end Stacks;

In our example, the type ``Stack`` is private. To be able to specify the
expected behavior of the procedure ``Push``, we need to disclose this
abstraction and access the values of the elements stored in ``S``. For
this, we introduce a function ``Get_Model`` that returns an array as a
model of the stack.

Still, we don't want user code of the ``Stack`` package to use
``Get_Model`` to break our stack's abstraction from normal code, as could
be done in the subprogram ``Peek``:

.. code:: ada
    :class: ada-expect-compile-error

    package Stacks is

       pragma Unevaluated_Use_Of_Old (Allow);

       type Stack is private;

       type Element is new Natural;
       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;

       function Get_Model (S : Stack) return Element_Array with Ghost;
       --  Returns an array as a model of a stack.

       procedure Push (S : in out Stack; E : Element) with
         Pre  => Get_Model (S)'Length < Max,
         Post => Get_Model (S) = Get_Model (S)'Old & E;

       function Peek (S : Stack; I : Positive) return Element is
         (Get_Model (S) (I)); -- ERROR

    private

       subtype Length_Type is Natural range 0 .. Max;

       type Stack is record
          Top     : Length_Type := 0;
          Content : Element_Array (1 .. Max) := (others => 0);
       end record;

    end Stacks;

Marking the function as :ada:`Ghost` achieves this goal. What is more,
it ensures that the subprogram ``Get_Model`` is never used in
production code.

Global Ghost Variables
~~~~~~~~~~~~~~~~~~~~~~

Though it happens less often, specifications may require storing additional
information into global variables. As this information is not needed in
normal code, these global variables should be marked as ghost, so that
they can be erased by the compiler. These variables can be used for
various reasons, and a rather common case is to specify programs modifying
a complex or private global data structure by providing a model for it,
that is updated by the program along with the data structure.

Global variables can also store information about previous runs of subprograms
in order to specify simple temporal properties. In the following example, we
have two procedures, one to access a state ``A`` and the other to access a
state ``B``. The global variable ``Last_Accessed_Is_A`` is used to specify that
``B`` cannot be accessed twice without accessing ``A`` in between.

.. code:: ada run_button
   :class: ada-run-expect-failure

    package Call_Sequence is

       type T is new Integer;

       Last_Accessed_Is_A : Boolean := False with Ghost;

       procedure Access_A with
         Post => Last_Accessed_Is_A;

       procedure Access_B with
         Pre  => Last_Accessed_Is_A,
         Post => not Last_Accessed_Is_A;
       --  B can only be accessed after A

    end Call_Sequence;

    package body Call_Sequence is

       procedure Access_A is
       begin
          --  ...
          Last_Accessed_Is_A := True;
       end Access_A;

       procedure Access_B is
       begin
          --  ...
          Last_Accessed_Is_A := False;
       end Access_B;

    end Call_Sequence;

    with Call_Sequence; use Call_Sequence;

    procedure Main is
    begin
       Access_A;
       Access_B;
       Access_B; -- ERROR
    end Main;

As another example, it can be the case that the requirement of a subprogram
expresses its expected behavior as a sequence of actions to be performed. To
write this kind of specification more easily, global ghost variables may be
used to store intermediate values of variables in the program.

For example, we specify below the subprogram ``Do_Two_Things`` in two steps
using the global variable ``V_Interm`` to store the intermediate value of ``V``
between the two things to be done. Note that this usage could be expressed
using an existential quantification on the variable ``V_Interm``, although this
would be very inefficient at runtime to iterate over all integers. Besides,
this cannot always be done in SPARK as quantification in Ada is restricted to
:ada:`for ... loop` patterns. What is more, supplying the value of the variable
may help the prover to effectively verify the contracts.

.. code:: ada

    package Action_Sequence is

       type T is new Integer;

       V_Interm : T with Ghost;

       function First_Thing_Done (X, Y : T) return Boolean with Ghost;
       function Second_Thing_Done (X, Y : T) return Boolean with Ghost;

       procedure Do_Two_Things (V : in out T) with
         Post => First_Thing_Done (V'Old, V_Interm)
           and then Second_Thing_Done (V_Interm, V);

    end Action_Sequence;

Guide Proof
---------------------------------------------------------------------

As properties of interest for functional correctness are more complex than
those involved in proof of program integrity, it is expected that GNATprove may
not be able to verify them right away even though they are valid. Techniques
for :ref:`Debugging Failed Proof Attempts` will come in handy here. Here, we
focus on improving results on the remaining cases where the property is valid
but is not proved by GNATprove in a reasonable amount of time.

In these cases, users may want to try and guide GNATprove in order either
to complete the proof or strip it down to a small number of easily
reviewable assumptions. For this purpose, assertions can be added to break
complex proofs into smaller steps:

.. code-block:: ada

    pragma Assert (Assertion_Checked_By_The_Tool);
    --  info: assertion proved

    pragma Assert (Assumption_Validated_By_Other_Means);
    --  medium: assertion might fail

    pragma Assume (Assumption_Validated_By_Other_Means);
    --  The tool does not attempt to check this expression.
    --  It is recorded as an assumption.

In particular, it may be a good idea, as an intermediate step, to try and
prove a theoretically equivalent version of the desired property where
things have been simplified for the prover, for example by splitting
different cases up or by inlining the definitions of functions.

It can be the case that some intermediate assertions are not discharged by
GNATprove, either because it is missing some information or because it
gets lost in the amount of information available. Those remaining
assertions can then be verified by other means like testing, since they
are executable, or by review. Users can choose to instruct GNATprove to
ignore them, either by turning them into assumptions, like in our example,
or by justifying the check using a :ada:`pragma Annotate`. In both cases,
the assumption will still be checked at runtime when assertions are
enabled.


Local Ghost Variables
~~~~~~~~~~~~~~~~~~~~~

Just like for specifications, ghost code can be used to enhance what can
be expressed inside intermediate assertions. In particular, local
variables or constants whose only purpose is to serve in assertions are a
common occurrence. Most of the time, these variables are used to store
previous values of variables or expressions to which we want to refer in
assertions. They are especially useful to refer to initial values of
parameters and expressions which cannot be accessed using the :ada:`'Old`
attribute outside of the subprogram's postcondition.

In the example shown below, to help GNATprove discharge the postcondition of
``P``, we want to assert that it holds separately in every branch of an
:ada:`if` statement. Since in these assertions we cannot use the :ada:`'Old`
attribute to access the initial value of the parameter ``X`` (unlike in ``P``'s
postcondition), we must resort to introducing a local ghost constant ``X_Init``
for this value.

.. code:: ada spark-report-all

    package Show_Local_Ghost is

       type T is new Natural;

       function F (X, Y : T) return Boolean is (X > Y) with Ghost;

       function Condition (X : T) return Boolean is (X mod 2 = 0);

       procedure P (X : in out T) with
         Pre  => X < 1_000_000,
         Post => F (X, X'Old);

    end Show_Local_Ghost;

    package body Show_Local_Ghost is

       procedure P (X : in out T) is
          X_Init : constant T := X with Ghost;
       begin
          if Condition (X) then
             X := X + 1;
             pragma Assert (F (X, X_Init));
          else
             X := X * 2;
             pragma Assert (F (X, X_Init));
          end if;
       end P;

    end Show_Local_Ghost;

Local ghost variables can also be used for more complex things such as
building a data-structure that serves as witness for a complex property of
the subprogram. In our example, we want to prove that the ``Sort``
procedure do not create new elements, that is, all the elements that are
in ``A`` after the sort were already in ``A`` before the sort. Note that
this property is not enough to ensure that, after a call to ``Sort``,
``A`` is a permutation of its value before the call. Still, it is already
complex for a prover to verify as it involves an alternation of
quantifiers. To help GNATprove, it may be interesting to store, for each
index ``I``, an index ``J`` that has the expected property.

.. code-block:: ada

    procedure Sort (A : in out Nat_Array) with
      Post => (for all I in A'Range =>
                 (for some J in A'Range => A (I) = A'Old (J)))
    is
       Permutation : Index_Array := (1 => 1, 2 => 2, ...) with Ghost;
    begin
       ...
    end Sort;

Ghost Procedures
~~~~~~~~~~~~~~~~

Ghost procedures cannot affect the value of normal variables. Therefore,
they are mostly used to perform treatments on ghost variables or to group
together a set of intermediate assertions.

Abstracting away the treatment of ghost variables or assertions inside a ghost
procedure has several advantages. First, it allows to use any code inside the
ghost procedure. This is not the case outside ghost procedures, where the only
ghost statements allowed are assignments to ghost variables and calls to ghost
procedures.

As an example, the :ada:`for` loop contained in ``Increase_A`` could not
appear by itself in normal code:

.. code:: ada spark-report-all

    package Show_Ghost_Proc is

       type Nat_Array is array (Integer range <>) of Natural;

       A : Nat_Array (1 .. 100) with Ghost;

       procedure Increase_A with
         Ghost,
         Pre => (for all I in A'Range => A (I) < Natural'Last);

    end Show_Ghost_Proc;

    package body Show_Ghost_Proc is

       procedure Increase_A is
       begin
          for I in A'Range loop
             A (I) := A (I) + 1;
          end loop;
       end Increase_A;

    end Show_Ghost_Proc;

Then, it improves readability by hiding away complex code that is of no
use for the functional behavior of the subprogram. Finally, it can help
GNATprove by abstracting away assertions that would otherwise pollute its
context.

For the example below, calling ``Prove_P`` on ``X`` will only add ``P
(X)`` to the proof context instead of the possible important set of
assertions that are required to verify it. What is more, the proof of
``P`` will only be done once and may be made easier by the fact that no
unnecessary information is present in the context while verifying it.
Also, if ``Prove_P`` happens to not be fully verified, the remaining
assumptions will be reviewed more easily if they are in a small context.

.. code-block:: ada

    procedure Prove_P (X : T) with Ghost,
      Global => null,
      Post   => P (X);


Handling of Loops
~~~~~~~~~~~~~~~~~

A case in which user annotations are almost always required for GNATprove
to complete a proof is when the program involves a loop. Indeed, the
verification techniques used by GNATprove do not handle cycles in a
subprogram's control flow. As a consequence, loops are flattened by
dividing them into several acyclic parts.

As an example, let us look at a simple loop statement with an exit
condition:

.. code:: ada
    :class: ada-nocheck

    Stmt1;
    loop
      Stmt2;
      exit when Cond;
      Stmt3;
    end loop;
    Stmt4;

As shown on the schema, the control flow will be divided into three parts:

.. image:: 05_loop.png
   :align: center

The first one, in yellow, starts from the beginning of the subprogram to
the loop statement. Then, the loop itself is divided into two parts. The
red one stands for a complete execution of the loop's body, that is, an
execution in which the exit condition is not satisfied. The blue one
stands for the last execution of the loop. The exit condition is assumed
to hold and the rest of the subprogram can be accessed. The red and blue
parts obviously always happen after the yellow one.

Still, as there is no way to know how the loop may have modified the
variables it updates, GNATprove simply forgets everything it knows about
them when entering these parts. Values of constants and variables that are
not modified in the loop are of course preserved.

The consequence of this particular handling is that GNATprove suffers from
imprecision when verifying a subprogram involving a loop. More precisely,
it won't be able to verify a property relying on values of variables
modified inside the loop. Also, though it will not forget any information
it had on the value of constants or unmodified variables, it still won't
be able to deduce new information about them using the loop.

For example, consider function ``Find`` which iterates over the array ``A``
searching for an index where ``E`` is stored in ``A``:

.. code:: ada spark-report-all

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural;

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Assert (for all J in A'First .. I - 1 => A (J) /= E);
             --  assertion is not proved
             if A (I) = E then
                return I;
             end if;
             pragma Assert (A (I) /= E);
             --  assertion is proved
          end loop;
          return 0;
       end Find;

    end Show_Find;

Though, at each loop iteration,
GNATprove knows that, for the loop to continue, the value stored in ``A`` at
index ``I`` must not be ``E`` (the second assertion which is proved), it will
not be able to accumulate this information to deduce that it is true for all
the indexes smaller than ``I`` (the first assertion which is not proved).


.. _Loop Invariants:

Loop Invariants
~~~~~~~~~~~~~~~

To overcome these limitations, users can provide additional information to
GNATprove in the form of a loop invariant. In SPARK, a loop invariant
is a Boolean expression which should hold at every iteration of the loop.
Like every other assertion, it can be checked at runtime by compiling the
program with assertions enabled.

The specificity of a loop invariant in comparison to other assertions lies
in the way it is handled for proof. The proof of a loop invariant is done
in two steps: first GNATprove checks that it holds in the first
iteration of the loop, and then, it checks that it holds in an arbitrary
iteration assuming it held in the previous iteration.

As an example, let us add a loop invariant to the ``Find`` function
stating that the first element of ``A`` is not ``E``:

.. code:: ada spark-report-all

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural;

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Loop_Invariant (A (A'First) /= E);
             --  loop invariant not proved in first iteration
             --  but preservation of loop invariant is proved
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;

    end Show_Find;

To verify this invariant, GNATprove generates two checks. The first
one, that checks whether the assertion holds in the first iteration of the
loop, is not verified by GNATprove. Indeed, there is no reason for the
first element of ``A`` to be different from ``E`` in this iteration.
However, the second check succeeds. Indeed, it is easy to deduce that,
if the first element of ``A`` was not ``E`` in a given iteration, then it
is still not ``E`` in the next one. Note that, if we move the invariant to
the end of the loop, then it is successfully verified by GNATprove.

Not only do loop invariants allow to verify complex properties over loops,
they are also used by GNATprove to verify other properties, such as the
absence of runtime errors over the loop's body and the statements
following the loop. More precisely, when verifying runtime checks or other
assertions from the loop's body or from statements following the loop, the
last occurrence of the loop invariant preceding this check is assumed to
hold.

Let's look at a version of ``Find`` where we use a loop invariant instead of an
assertion to state that all array elements seen so far are not equal to ``E``:

.. code:: ada spark-report-all

    package Show_Find is

       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural;

    end Show_Find;

    package body Show_Find is

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Loop_Invariant
               (for all J in A'First .. I - 1 => A (J) /= E);
             if A (I) = E then
                return I;
             end if;
          end loop;
          pragma Assert (for all I in A'Range => A (I) /= E);
          return 0;
       end Find;

    end Show_Find;

This version is fully proved by GNATprove! This time, GNATprove proves that the
loop invariant holds in every iteration of the loop (separately proving this
property for the first iteration and for the following iterations). It also
proves that, after the loop, all the elements of ``A`` are different from ``E``
by assuming that the loop invariant holds in the last iteration of the loop.

Coming up with a good loop invariant can turn out to be quite a challenge.
To make this task easier, let us review the four good properties of a good
loop invariant:

+-------------+---------------------------------------------------------+
| Property    | Description                                             |
+=============+=========================================================+
| INIT        | It should be provable in the first iteration of the     |
|             | loop                                                    |
+-------------+---------------------------------------------------------+
| INSIDE      | It should allow proving absence of run-time errors and  |
|             | local assertions inside the loop                        |
+-------------+---------------------------------------------------------+
| AFTER       | It should allow proving absence of run-time errors,     |
|             | local assertions and the subprogram postcondition after |
|             | the loop                                                |
+-------------+---------------------------------------------------------+
| PRESERVE    | It should be provable after the first iteration of the  |
|             | loop                                                    |
+-------------+---------------------------------------------------------+

First, the loop invariant should be provable in the first iteration of the loop
(INIT). To achieve this property, the loop invariant's initialization can be
debugged like any failing proof attempt using strategies for :ref:`Debugging
Failed Proof Attempts`.

Next, the loop invariant should be precise enough to allow proving absence of
runtime errors both in statements from the loop's body (INSIDE) and in
statements following the loop (AFTER). To achieve this, users should remember
that every information concerning a variable modified in the loop that is not
stated in the invariant will be forgotten by GNATprove. In particular, users
should take care to include in their invariant what is usually called the
loop's frame condition. It consists in stating the preservation of parts of
composite variables that have not been modified by the loop.

Finally, the loop invariant should be precise enough to prove that it is
preserved through successive iterations of the loop (PRESERVE). This is
generally the trickiest part. To understand why the preservation of a loop
invariant is not proved by GNATprove, it is often useful to repeat it into
local assertions throughout the loop's body to determine at which point the
proof is lost.

As an example, let us look at a loop that iterates through an array ``A``
and applies a function ``F`` to each of its elements:

.. code:: ada spark-report-all

    package Show_Map is

       type Nat_Array is array (Positive range <>) of Natural;

       function F (V : Natural) return Natural is
         (if V /= Natural'Last then V + 1 else V);

       procedure Map (A : in out Nat_Array);

    end Show_Map;

    package body Show_Map is

       procedure Map (A : in out Nat_Array) is
          A_I : constant Nat_Array := A with Ghost;
       begin
          for K in A'Range loop
             A (K) := F (A (K));
             pragma Loop_Invariant
               (for all J in A'First .. K => A (J) = F (A'Loop_Entry (J)));
          end loop;
          pragma Assert (for all K in A'Range => A (K) = F (A_I (K)));
       end Map;

    end Show_Map;

We want to prove that, after the loop, each element of the array is the result
of applying ``F`` to the value that was stored in ``A`` at the same index
before the loop. To specify this property, we copy the value of ``A`` before
the loop in a ghost variable ``A_I``. As a loop invariant, we state that, for
every index smaller than ``K``, the array has been modified in the expected
way. Note that we choose here to use the :ada:`Loop_Entry` attribute to refer
to the value of ``A`` on entry of the loop, instead of using ``A_I``.

Does our loop invariant has the four good properties of a good
loop-invariant? When launching GNATprove on it, we see that ``INIT`` is
fulfilled, the invariant's initialization is proved. So are ``INSIDE`` and
``AFTER``, no potential runtime errors are reported and the assertion
following the loop is successfully verified.

The situation is slightly more complex for the ``PRESERVE`` property. GNATprove
manages to prove that the invariant holds after the first iteration, thanks to
the automatic generation of frame conditions. What happens is that GNATprove
completes the provided loop invariant with the following property called `frame
condition` stating what part of the array has not been modified so far:

.. code-block:: ada

             pragma Loop_Invariant
               (for all J in K .. A'Last => A (J) = (if J > K then A'Loop_Entry (J)));

The user-provided and the internally-generated loop invariants are then used to
prove ``PRESERVE``. In more complex cases, the heuristics used by GNATprove to
generate the frame condition are not sufficient, and a user must provide one as
loop invariant. For example, consider a version of ``Map`` where the result of
applying ``F`` to an element at index ``K`` is stored at index ``K-1``:

.. code:: ada spark-report-all

    package Show_Map is

       type Nat_Array is array (Positive range <>) of Natural;

       function F (V : Natural) return Natural is
         (if V /= Natural'Last then V + 1 else V);

       procedure Map (A : in out Nat_Array);

    end Show_Map;

    package body Show_Map is

       procedure Map (A : in out Nat_Array) is
          A_I : constant Nat_Array := A with Ghost;
       begin
          for K in A'Range loop
             if K /= A'First then
                A (K-1) := F (A (K));
             end if;
             pragma Loop_Invariant
               (for all J in A'First .. K =>
                 (if J /= A'First then A (J-1) = F (A'Loop_Entry (J))));
             -- pragma Loop_Invariant
             --  (for all J in K .. A'Last => A (J) = A'Loop_Entry (J));
          end loop;
          pragma Assert (for all K in A'Range =>
                          (if K /= A'First then A (K-1) = F (A_I (K))));
       end Map;

    end Show_Map;

You need to uncomment the second loop invariant containing the frame condition
in order to prove the assertion after the loop.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

We are implementing a ring buffer inside an array ``Content``, where the
content of a ring buffer of length ``Length`` is obtained by starting at index
``First`` and possibly wrapping around the end of the buffer. We use a ghost
function ``Get_Model`` to return the content of the ring buffer, for use in
contracts.

.. code:: ada spark-report-all

    package Ring_Buffer is

       Max_Size : constant := 100;

       type Nat_Array is array (Positive range <>) of Natural;

       function Get_Model return Nat_Array with Ghost;

       procedure Push_Last (E : Natural) with
         Pre  => Get_Model'Length < Max_Size,
         Post => Get_Model'Length = Get_Model'Old'Length + 1;

    end Ring_Buffer;

    package body Ring_Buffer is

       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       function Get_Model return Nat_Array with
         Refined_Post => Get_Model'Result'Length = Length
       is
          Size   : constant Length_Range := Length;
          Result : Nat_Array (1 .. Size) := (others => 0);
       begin
          if First + Length - 1 <= Max_Size then
             Result := Content (First .. First + Length - 1);
          else
             declare
                Len : constant Length_Range := Max_Size - First + 1;
             begin
                Result (1 .. Len) := Content (First .. Max_Size);
                Result (Len + 1 .. Length) := Content (1 .. Length - Len);
             end;
          end if;
          return Result;
       end Get_Model;

       procedure Push_Last (E : Natural) is
       begin
          if First + Length <= Max_Size then
             Content (First + Length) := E;
          else
             Content (Length - Max_Size + First) := E;
          end if;
          Length := Length + 1;
       end Push_Last;

    end Ring_Buffer;

This is correct as ``Get_Model`` is used only in contracts. Note that calls to
``Get_Model`` cause copies of the buffer's content, which is not
efficient. This is fine because ``Get_Model`` is only used for verification,
not in the final production code. This is enforced by making it a ghost
function and producing the final production code with appropriate compiler
switches (not using ``-gnata``) that ensure that assertions are ignored.


Example #2
~~~~~~~~~~

Instead of using a ghost function ``Get_Model`` to retrieve the content of the
ring buffer, we're now using a global ghost variable ``Model``.

.. code:: ada
    :class: ada-expect-compile-error

    package Ring_Buffer is

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       type Nat_Array is array (Positive range <>) of Natural;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean;

       procedure Push_Last (E : Natural) with
         Pre  => Valid_Model
           and then Model.Length < Max_Size,
         Post => Model.Length = Model.Length'Old + 1;

    end Ring_Buffer;

    package body Ring_Buffer is

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       function Valid_Model return Boolean is
          (Model.Content'Length = Length);

       procedure Push_Last (E : Natural) is
       begin
          if First + Length <= Max_Size then
             Content (First + Length) := E;
          else
             Content (Length - Max_Size + First) := E;
          end if;
          Length := Length + 1;
       end Push_Last;

    end Ring_Buffer;

This example is not correct. ``Model``, which is a ghost variable, cannot
influence the return value of the normal function ``Valid_Model``. As
``Valid_Model`` is only used in specifications, it should be marked as
:ada:`Ghost`. Another problem is that ``Model`` variable needs to be updated
inside ``Push_Last`` to reflect the changes to the ring buffer.


Example #3
~~~~~~~~~~

Let's mark ``Valid_Model`` as :ada:`Ghost` and update ``Model`` inside
``Push_Last``.

.. code:: ada spark-report-all

    package Ring_Buffer is

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       type Nat_Array is array (Positive range <>) of Natural;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean with Ghost;

       procedure Push_Last (E : Natural) with
         Pre  => Valid_Model
           and then Model.Length < Max_Size,
         Post => Model.Length = Model.Length'Old + 1;

    end Ring_Buffer;

    package body Ring_Buffer is

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       function Valid_Model return Boolean is
          (Model.Content'Length = Length);

       procedure Push_Last (E : Natural) is
       begin
          if First + Length <= Max_Size then
             Content (First + Length) := E;
          else
             Content (Length - Max_Size + First) := E;
          end if;
          Length := Length + 1;
          Model := (Length  => Model.Length + 1,
                    Content => Model.Content & E);
       end Push_Last;

    end Ring_Buffer;

This example is correct. The ghost variable ``Model`` can be referenced both
from the body of the ghost function ``Valid_Model`` and from the body of the
non-ghost procedure ``Push_Last`` as long as it is only used in ghost
statements.


Example #4
~~~~~~~~~~

We're now modifying ``Push_Last`` to share the computation of the new length
between the operational code and the ghost code.

.. code:: ada
    :class: ada-expect-compile-error

    package Ring_Buffer is

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       type Nat_Array is array (Positive range <>) of Natural;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean with Ghost;

       procedure Push_Last (E : Natural) with
         Pre  => Valid_Model
           and then Model.Length < Max_Size,
         Post => Model.Length = Model.Length'Old + 1;

    end Ring_Buffer;

    package body Ring_Buffer is

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       function Valid_Model return Boolean is
          (Model.Content'Length = Length);

       procedure Push_Last (E : Natural) is
          New_Length : constant Length_Range := Model.Length + 1;
       begin
          if First + Length <= Max_Size then
             Content (First + Length) := E;
          else
             Content (Length - Max_Size + First) := E;
          end if;
          Length := New_Length;
          Model := (Length  => New_Length,
                    Content => Model.Content & E);
       end Push_Last;

    end Ring_Buffer;

This example is not correct. Local constant ``New_Length`` is not marked as
:ada:`Ghost`, hence it cannot be computed from the value of ghost variable
``Model``. Note that making ``New_Length`` a ghost constant would only report
the problem on the assignment from ``New_Length`` to ``Length``. The correct
solution here is to compute ``New_Length`` from the value of non-ghost variable
``Length``.


Example #5
~~~~~~~~~~

Let's move the code updating ``Model`` inside a local ghost procedure
``Update_Model``, still using a local variable ``New_Length`` to compute the
new length.

.. code:: ada spark-report-all

    package Ring_Buffer is

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       type Nat_Array is array (Positive range <>) of Natural;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean with Ghost;

       procedure Push_Last (E : Natural) with
         Pre  => Valid_Model
           and then Model.Length < Max_Size,
         Post => Model.Length = Model.Length'Old + 1;

    end Ring_Buffer;

    package body Ring_Buffer is

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       function Valid_Model return Boolean is
          (Model.Content'Length = Length);

       procedure Push_Last (E : Natural) is

          procedure Update_Model with Ghost is
             New_Length : constant Length_Range := Model.Length + 1;
          begin
             Model := (Length  => New_Length,
                       Content => Model.Content & E);
          end Update_Model;

       begin
          if First + Length <= Max_Size then
             Content (First + Length) := E;
          else
             Content (Length - Max_Size + First) := E;
          end if;
          Length := Length + 1;
          Update_Model;
       end Push_Last;

    end Ring_Buffer;

Everything is fine here. ``Model`` is only accessed inside ``Update_Model``
which is itself a ghost procedure, so it's fine to declare local variable
``New_Length`` without the :ada:`Ghost` aspect as everything inside a ghost
procedure body is ghost. Moreover, we don't need to add any contract to
``Update_Model``. Indeed, as it is a local procedure without contract, it is
inlined by GNATprove.

Example #6
~~~~~~~~~~

Function ``Max_Array`` takes as arguments two arrays of the same length (but
not necessarily starting and ending at the same indexes) and returns an array
of the maximum values between its arguments at each index.

.. code:: ada

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

    end Array_Util;

    package body Array_Util is

       function Max_Array (A, B : Nat_Array) return Nat_Array is
          R : Nat_Array (A'Range) := (others => 0);
          J : Integer := B'First;
       begin
          for I in A'Range loop
             if A (I) > B (J) then
                R (I) := A (I);
             else
                R (I) := B (J);
             end if;
             J := J + 1;
          end loop;
          return R;
       end Max_Array;

    end Array_Util;

This program is correct, but GNATprove cannot prove that ``J`` stays in the
index range of ``B`` (the unproved index check) or even that it stays in the
bounds of its type (the unproved overflow check). Indeed, when checking the
body of the loop, GNATprove forgets everything about the current value of ``J``
as it has been modified by previous iterations of the loop. To get more precise
results, we need to provide a loop invariant.


Example #7
~~~~~~~~~~

Let's add a loop invariant that states that ``J`` stays in the index range of
``B``, and let's protect the increment to ``J`` by checking that it is not
already the maximal integer value.

.. code:: ada

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

    end Array_Util;

    package body Array_Util is

       function Max_Array (A, B : Nat_Array) return Nat_Array is
          R : Nat_Array (A'Range) := (others => 0);
          J : Integer := B'First;
       begin
          for I in A'Range loop
             pragma Loop_Invariant (J in B'Range);
             if A (I) > B (J) then
                R (I) := A (I);
             else
                R (I) := B (J);
             end if;
             if J < Integer'Last then
                J := J + 1;
             end if;
          end loop;
          return R;
       end Max_Array;

    end Array_Util;

The loop invariant now allows verifying that no runtime error can occur in the
loop's body (property INSIDE seen in section :ref:`Loop
Invariants`). Unfortunately, GNATprove will fail to verify that the invariant
stays valid after the first iteration of the loop (property PRESERVE). Indeed,
knowing that ``J`` is in ``B'Range`` in a given iteration is not enough to show
that it will remain so in the next iteration. We need a more precise invariant,
linking ``J`` to the value of the loop index ``I``, like :ada:`J = I -
A'First + B'First`.


Example #8
~~~~~~~~~~

We now consider a version of ``Max_Array`` which takes arguments starting and
ending at the same indexes. We want to prove that ``Max_Array`` returns an
array of the maximum values between its arguments at each index.

.. code:: ada run_button
   :class: ada-run-expect-failure

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post => (for all K in A'Range =>
                   Max_Array'Result (K) = Natural'Max (A (K), B (K)));

    end Array_Util;

    package body Array_Util is

       function Max_Array (A, B : Nat_Array) return Nat_Array is
          R : Nat_Array (A'Range) := (others => 0);
       begin
          for I in A'Range loop
             pragma Loop_Invariant (for all K in A'First .. I =>
                                      R (K) = Natural'Max (A (K), B (K)));
             if A (I) > B (I) then
                R (I) := A (I);
             else
                R (I) := B (I);
             end if;
          end loop;
          return R;
       end Max_Array;

    end Array_Util;

    with Array_Util; use Array_Util;

    procedure Main is
       A : Nat_Array := (1, 1, 2);
       B : Nat_Array := (2, 1, 0);
       R : Nat_Array (1 .. 3);
    begin
       R := Max_Array (A, B);
    end Main;

Here, GNATprove does not manage to prove the loop invariant even in the first
loop iteration (property INIT seen in section :ref:`Loop Invariants`). In fact,
the loop invariant is incorrect, as can be checked by executing the function
``Max_Array`` with assertions enabled. Indeed, at each loop iteration, ``R``
contains the maximum of ``A`` and ``B`` only until ``I - 1`` as the ``I`` th
index was not handled yet.


Example #9
~~~~~~~~~~

We now consider a procedural version of ``Max_Array`` which updates its first
argument instead of returning a new array. We want to prove that ``Max_Array``
returns in its first argument the maximum values between its arguments at each
index.

.. code:: ada spark-report-all

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       procedure Max_Array (A : in out Nat_Array; B : Nat_Array) with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post => (for all K in A'Range =>
                    A (K) = Natural'Max (A'Old (K), B (K)));

    end Array_Util;

    package body Array_Util is

       procedure Max_Array (A : in out Nat_Array; B : Nat_Array) is
       begin
          for I in A'Range loop
             pragma Loop_Invariant
               (for all K in A'First .. I - 1 =>
                  A (K) = Natural'Max (A'Loop_Entry (K), B (K)));
             pragma Loop_Invariant
               (for all K in I .. A'Last => A (K) = A'Loop_Entry (K));
             if A (I) <= B (I) then
                A (I) := B (I);
             end if;
          end loop;
       end Max_Array;

    end Array_Util;

Everything is proved. The first loop invariant states that ``A`` before the
loop index contains the maximum values between the arguments of ``Max_Array``
(referring to the input value of ``A`` with ``A'Loop_Entry``). The second loop
invariant states that ``A`` after and including the loop index is the same as
on entry, also known as the `frame condition` of the loop.


Example #10
~~~~~~~~~~~

Let's remove the frame condition from the previous example.

.. code:: ada spark-report-all

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       procedure Max_Array (A : in out Nat_Array; B : Nat_Array) with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post => (for all K in A'Range =>
                    A (K) = Natural'Max (A'Old (K), B (K)));

    end Array_Util;

    package body Array_Util is

       procedure Max_Array (A : in out Nat_Array; B : Nat_Array) is
       begin
          for I in A'Range loop
             pragma Loop_Invariant
               (for all K in A'First .. I - 1 =>
                  A (K) = Natural'Max (A'Loop_Entry (K), B (K)));
             if A (I) <= B (I) then
                A (I) := B (I);
             end if;
          end loop;
       end Max_Array;

    end Array_Util;

Everything is still proved. In fact, GNATprove internally generates the frame
condition for the loop, so it's sufficient here to state that ``A`` before the
loop index contains the maximum values between the arguments of ``Max_Array``.
