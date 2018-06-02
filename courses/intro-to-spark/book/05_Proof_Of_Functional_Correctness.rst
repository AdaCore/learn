Lesson 5: Proof of Functional Correctness
=====================================================================

.. role:: ada(code)
   :language: ada

This course is dedicated to functional correctness of programs. It
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

For example:

.. code:: ada

    procedure Show_Find_1 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post => Find'Result in 0 | A'Range;

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;
    begin
       null;
    end Show_Find_1;

To ensure no runtime error is raised when using the result of function
``Find``, it may be enough to know that, whenever it is not 0, then it is
in ``A``'s range. However, for the program to be meaningful, we may want
``Find`` to verify more complex properties. For example that it only
returns 0 if ``E`` is not in ``A`` and that, otherwise, it returns an
index of ``A`` where ``E`` is stored. If, like we did for ``Find``, these
specifications are expressed as contracts in subprograms or packages, then
GNATprove can be used to statically verify them.

.. code:: ada

    procedure Show_Find_2 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post =>
           (if (for all I in A'Range => A (I) /= E)
              then Find'Result = 0
                else Find'Result in A'Range and then A (Find'Result) = E);

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;
    begin
       null;
    end Show_Find_2;

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

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_3 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural with
         Post =>
           (if (for all I in A'Range => A (I) /= E)
              then Find'Result = 0
                else Find'Result in A'Range and then A (Find'Result) = E);

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;

       Seq : constant Nat_Array (1 .. 3) := (1, 5, 3);
       Res : Natural;
    begin
       Res := Find (Seq, 3);
       Put_Line ("Found 3 in index #"
                 & Natural'Image (Res) & " of array");
    end Show_Find_3;

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

.. code:: ada

    procedure Show_Sort is
       type Nat_Array is array (Positive range <>) of Natural;

       function Is_Sorted (A : Nat_Array) return Boolean is
         (for all I in A'Range =>
            (if I < A'Last then A (I) <= A (I + 1)));
       --  Returns True if A is sorted in increasing order.

       subtype Sorted_Nat_Array is Nat_Array with
         Dynamic_Predicate => Is_Sorted (Sorted_Nat_Array);
       --  Elements of type Sorted_Nat_Array are all sorted.
    begin
       null;
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
for entities that are only used for the purpose of specification. In a
qualification process, it may be important to make sure that these new
entities cannot affect the behavior of the program, or even to ensure they
are removed from production code. This concept, usually called ghost code,
is supported in SPARK 2014 by the new :ada:`Ghost` aspect.

The :ada:`Ghost` aspect can be used to annotate any normal entity, such as
variables, types, subprograms, or packages. If an entity is marked as
:ada:`Ghost`, GNATprove will make sure that it cannot affect the program's
behavior. To be able to dynamically test the contracts using it, ghost
code will be executed like normal code when the program is compiled with
assertions enabled. The compiler can also be instructed not to generate
code for ghost entities.

As an example:

.. code:: ada

    procedure Show_Ghost is
       type T is new Integer;

       function Is_Correct (X, Y : T) return Boolean is (X = Y);

       procedure Do_Some_Complex_Stuff (X : in out T) is null;

       procedure Do_Something (X : in out T) is
          X_Init : constant T := X with Ghost;
       begin
          Do_Some_Complex_Stuff (X);
          pragma Assert (Is_Correct (X_Init, X));
          --  It is OK to use X_Init inside an assertion.
       end Do_Something;

       pragma Unreferenced (Do_Something);
    begin
       null;
    end Show_Ghost;

Here, the ``Do_Something`` subprogram stores the initial value of ``X`` in
a ghost constant called ``X_Init``. We can then reference this variable
from assertions to check that the computation performed by the call to the
``Do_Some_Complex_Stuff`` subprogram modified the value of ``X`` in the
expected manner.

However ``X_Init`` should not be used in normal code, for example to
restore the initial value of ``X``:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Show_Ghost is
       type T is new Integer;

       function Is_Correct (X, Y : T) return Boolean is (X = Y);

       procedure Do_Some_Complex_Stuff (X : in out T) is null;

       procedure Do_Something (X : in out T) is
          X_Init : constant T := X with Ghost;
       begin
          Do_Some_Complex_Stuff (X);
          pragma Assert (Is_Correct (X_Init, X));

          X := X_Init;
          --  Compilation error:
          --     Ghost entity cannot appear in this context.
       end Do_Something;
    begin
       null;
    end Show_Ghost;

When compiling this example, you'll see that the use of ``X_Init`` will be
flagged as illegal by the compiler. Note that more complex cases of
interference between ghost and normal code may only be detected by running
GNATprove.


Ghost Functions
~~~~~~~~~~~~~~~

Functions only used in specifications is a rather common occurrence when
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
    :class: ada-nocheck

    package Stacks is

       --  [...]

       function Peek (S : Stack; I : Positive) return Natural is
         (Get_Model (S) (I));
       --  Get_Model cannot be used in this context.

Marking the function as :ada:`Ghost` will achieve this goal. What is more,
it will ensure that the subprogram ``Get_Model`` is never used in
production code.


Global Ghost Variables
~~~~~~~~~~~~~~~~~~~~~~

Though it happens less often, specification may require storing additional
information into global variables. As this information is not needed in
normal code, these global variables should be marked as ghost, so that
they can be erased by the compiler. These variables can be used for
various reasons, and a rather common case is to specify programs modifying
a complex or private global data-structure by providing a model for it,
that is updated by the program along with the data-structure.

Global variables can also store information about previous runs of
subprograms in order to specify simple temporal properties. Let's look at
an example:

.. code:: ada

    package Show_Global_Ghost_Vars is
       type T is new Integer;

       Last_Accessed_Is_A : Boolean := False with Ghost;

       function First_Thing_Done
         (V_Old, V_Interm : T)
          return Boolean with Ghost;

       function Second_Thing_Done
         (V_Old, V_Interm : T)
          return Boolean with Ghost;

       procedure Access_A with
         Post => Last_Accessed_Is_A;

       procedure Access_B with
         Pre  => Last_Accessed_Is_A,
         Post => not Last_Accessed_Is_A;
       --  B can only be accessed after A

       V_Interm           : T with Ghost;

       procedure Do_Two_Things (V : in out T) with
         Post => (First_Thing_Done (V'Old, V_Interm)
                  and Second_Thing_Done (V_Interm, V));
    end Show_Global_Ghost_Vars;

In our example, we have two procedures, one to access a state ``A`` and
the other to access a state ``B``. The global variable
``Last_Accessed_Is_A`` is used to specify that ``B`` cannot be accessed
twice without accessing ``A`` in between.

It can be the case that the requirements of a subprogram expresses its
expected behavior as a sequence of actions to be performed. To write this
kind of specification more easily, global ghost variables may be used to
store intermediate values of variables in the program.

For example, we specify here the subprogram ``Do_Two_Things`` in two steps
using the global variable ``V_Interm`` to store the intermediate value of
``V`` between the two things to be done. Note that, conceptually, this
usage could be expressed using an existential quantification on the
variable ``V_Interm``. This cannot always be done in SPARK as
quantification in Ada is restricted to :ada:`for ... loop` patterns. What
is more, supplying the value of the variable may help the prover to
effectively verify the contracts.


Guide Proof
---------------------------------------------------------------------

As properties of interest for functional correctness are more complex than
those involved in proof of program integrity, it is expected that
GNATprove may not be able to verify them right away even though they are
valid. Techniques for debugging failed proof attempts explained in the
proof of program integrity course will come in handy here (see
:doc:`03_Proof_Of_Program_Integrity`). We don't go over them again in this
course, but rather focus on improving results on the remaining cases where
the property is valid but is not proved by GNATprove in a reasonable
amount of time.

In these cases, users may want to try and guide GNATprove in order either
to complete the proof or strip it down to a small number of easily
reviewable assumptions. For this purpose, assertions can be added to break
complex proofs into smaller steps:

.. code:: ada
    :class: ada-nocheck

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
our assumptions. They are especially useful to refer to initial values of
parameters and expressions which cannot be accessed using the :ada:`‘Old`
attribute outside of the subprogram's postcondition.

For example:

.. code:: ada

    procedure Show_Local_Ghost_Vars is

       type T is new Integer;
       type Index_Array is array (1 .. 3) of T;
       type Nat_Array   is array (1 .. 3) of Natural;

       function F (X, X_Old : T) return Boolean
         is (X = X_Old) with Ghost;

       function Condition (X : T) return Boolean
           is (X > 2);

       procedure P (X : in out T) with
         Post => F (X, X'Old)
       is
          X_Init : constant T := X with Ghost;
          --  Declaring ghost variable X_Init
       begin
          if Condition (X) then
             pragma Assert (F (X, X_Init));

             --  [...]
             null;
          end if;
       end P;

       procedure Sort (A : in out Nat_Array) with
         Post => (for all I in A'Range =>
                    (for some J in A'Range => A (I) = A'Old (J)))
       is
          Permutation : Index_Array := (1 => 1, 2 => 2, 3 => 3) with
            Ghost;
       begin
          null;
       end Sort;
    begin
       null;
    end Show_Local_Ghost_Vars;

In the example shown here, to help GNATprove discharge the postcondition
of ``P``, we want to assert that it holds separately in every branch of an
:ada:`if` statement. Since in these assertions, unlike in ``P``'s
postconditions, we cannot use the :ada:`‘Old` attribute to access the
initial value of the parameter ``X``, we must resort to introducing a
local ghost constant ``X_Init`` for this value.

Local ghost variables can also be used for more complex things such as
building a data-structure that serves as witness of a complex property of
the subprogram. In our example, we want to prove that the ``Sort``
procedure do not create new elements, that is, all the elements that are
in ``A`` after the sort were already in ``A`` before the sort. Note that
this property is not enough to ensure that, after a call to ``Sort``,
``A`` is a permutation of its value before the call. Still, it is already
complex for a prover to verify as it involves an alternation of
quantifiers. To help GNATprove, it may be interesting to store, for each
index ``I``, an index ``J`` that has the expected property.


Ghost Procedures
~~~~~~~~~~~~~~~~

Ghost procedures cannot affect the value of normal variables. Therefore,
they are mostly used to perform treatments on ghost variables or to group
together a set of intermediate assertions.

Abstracting away treatment of ghost variables or assertions inside a ghost
procedure has several advantages. First, it enhances expressivity as, to
simplify the removal of ghost code by the compiler: the only ghost
statements that are allowed to appear in normal code are assignments to
ghost variables and ghost procedure calls.

As an example, the :ada:`for` loop contained in ``Increase_A`` could not
appear by itself in normal code:

.. code:: ada

    procedure Show_Ghost_Proc is

       type Nat_Array   is array (1 .. 3) of Natural;

       A : Nat_Array := (1, 2, 3) with Ghost;

       procedure Increase_A (A : in out Nat_Array)
         with Ghost is
       begin
          for I in A'Range loop
             A (I) := A (I) + 1;
          end loop;
       end Increase_A;

    begin
       Increase_A (A);
    end Show_Ghost_Proc;

Then, it improves readability by hiding away complex code that is of no
use for the functional behavior of the subprogram. Finally, it can help
GNATprove by abstracting away assertions that would otherwise pollute its
context.

For the example above, calling ``Prove_P`` on ``X`` will only add ``P
(X)`` to the proof context instead of the possible important set of
assertions that are required to verify it. What is more, the proof of
``P`` will only be done once and may be made easier by the fact that no
unnecessary information is present in the context while verifying it.
Also, if ``Prove_P`` happens to not be fully verified, the remaining
assumptions will be reviewed more easily if they are in a small context.


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

The first one, in yellow, starts from the beginning of the subprogram to
the loop statement. Then, the loop itself is divided into two parts. The
red one stands for a complete execution of the loop's body, that is, an
execution in which the exit condition is not satisfied. The blue one
stands for the last execution of the loop. The exit condition is assumed
to hold and the rest of the subprogram can be accessed. The red and blue
parts obviously always happen after the yellow one.

Still, as there is no way to know how the loop may have modified the
variables it accesses, GNATprove simply forgets everything it knows about
them when entering these parts. Values of constants and variables that are
not modified in the loop are of course preserved.

The consequence of this particular handling is that GNATprove suffers from
imprecision when verifying a subprogram involving a loop. More precisely,
it won't be able to verify a property relying on values of variables
modified inside the loop. Also, though it will not forget any information
it had on the value of constants or unmodified variables, it still won't
be able to deduce new information about them using the loop.

For example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_4 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Assert
               (for all J in A'First .. I - 1 => A (J) /= E);
             --  medium: assertion might fail
             if A (I) = E then
                return I;
             end if;
             pragma Assert (A (I) /= E);
             --  info: assertion proved
          end loop;
          return 0;
       end Find;
    begin
       null;
    end Show_Find_4;

Here, in our function ``Find``, we iterate over the array ``A`` searching
for an index where ``E`` is stored in ``A``. Though, at each loop
iteration, GNATprove knows that, for the loop to continue, the value
stored in ``A`` at index ``I`` must not be ``E``, it will not be able to
accumulate this information to deduce that it is true for all the indexes
smaller than ``I``.


Loop Invariants
~~~~~~~~~~~~~~~

To overcome these limitations, users can provide additional information to
the tool in the form of a loop invariant. In SPARK 2014, a loop invariant
is a Boolean expression which should hold at every iteration of the loop.
Like every other assertion, it can be checked at runtime by compiling the
program with assertions enabled.

The specificity of a loop invariant in comparison to other assertions lies
in the way it is handled for proof. The proof of a loop invariant is done
in two steps: first the GNATprove checks that it holds in the first
iteration of the loop, and then, it checks that it holds in an arbitrary
iteration assuming it held in the previous one.

As an example, let us add a loop invariant to our ``Find`` function
stating that the first element of ``A`` is not ``E``:

.. code:: ada

    procedure Show_Find_5 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Loop_Invariant (A (A'First) /= E);
             --  medium: loop invariant might fail in first iteration
             --  info: loop invariant preservation proved
             if A (I) = E then
                return I;
             end if;
          end loop;
          return 0;
       end Find;
    begin
       null;
    end Show_Find_5;

To verify this invariant, GNATprove will generate two checks. The first
one, that checks whether the assertion holds in the first iteration of the
loop, will not be verified by the tool. Indeed, there is no reason for the
first element of ``A`` to be different from ``E`` in this iteration.
However, the second check will succeed. Indeed, it is easy to deduce that,
if the first element of ``A`` was not ``E`` in a given iteration, then it
is still not ``E`` in the next one. Note that, if we move the invariant to
the end of the loop, then it will be successfully verified by GNATprove.

Not only do loop invariants allow to verify complex properties over loops,
they are also used by GNATprove to verify other properties, such as the
absence of runtime errors over the loop's body and the statements
following the loop. More precisely, when verifying runtime checks or other
assertions from the loop's body or from statements following the loop, the
last occurrence of the loop invariant preceding this check is assumed to
hold.

Let's look again at this version of the ``Find`` example:

.. code:: ada

    procedure Show_Find_6 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Find (A : Nat_Array; E : Natural) return Natural is
       begin
          for I in A'Range loop
             pragma Loop_Invariant
               (for all J in A'First .. I - 1 => A (J) /= E);
             --  info: loop invariant initialization proved
             --  info: loop invariant preservation proved
             if A (I) = E then
                return I;
             end if;
          end loop;
          pragma Assert (for all I in A'Range => A (I) /= E);
          --  info: assertion proved
          return 0;
       end Find;
    begin
       null;
    end Show_Find_6;

In our ``Find`` function, GNATprove can verify that, after the loop, all
the elements of ``A`` are different from ``E`` by assuming that the loop
invariant holds in the last iteration of the loop.

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

First, the loop invariant should be provable in the first iteration of the
loop. To achieve this property, the loop invariant's initialization can be
debugged like any failing proof attempt using strategies from the Proof of
Program Integrity course (see :doc:`03_Proof_Of_Program_Integrity`).

Next, the loop invariant should be precise enough to allow proving absence
of runtime errors both in statements from the loop's body and in
statements following the loop. To achieve this, users should remember that
every information concerning a variable modified in the loop that is not
stated in the invariant will be forgotten by the tool. In particular,
users should take care to include in their invariant what is usually
called the loop's frame condition. It consists in stating the preservation
of parts of composite variables that have not been modified by the loop.

Finally, the loop invariant should be precise enough to prove that it is
preserved through successive iterations of the loop. This is generally the
trickiest part. To understand why the preservation of a loop invariant is
not proved by GNATprove, it is often useful to repeat it into local
assertions throughout the loop's body to determine at which point the
proof is lost.

As an example, let us look at a loop that iterates through an array ``A``
and applies a function ``F`` to each of its elements:

.. code:: ada

    procedure Show_Loop_1 is
       type Nat_Array is array (Positive range <>) of Natural;

       function F (V : Natural) return Natural is
         (if V /= Natural'Last then V + 1 else V);

       A   : Nat_Array := (1, 2, 3);
       A_I : constant Nat_Array := A with Ghost;

    begin
       for K in A'Range loop
          A (K) := F (A (K));
          pragma Loop_Invariant
            (for all J in A'First .. K => A (J) = F (A_I (J)));
          --  info: loop invariant initialization proved
          --  medium: loop invariant might fail after first iteration
       end loop;
       pragma Assert (for all K in A'Range => A (K) = F (A_I (K)));
       --  info: assertion proved
    end Show_Loop_1;

We want to prove that, after the loop, each element of the array is the
result of applying ``F`` to the value that was stored in ``A`` at the same
index before the loop. To specify this property, we copy the value of
``A`` before the loop in a ghost variable ``A_I``. As a loop invariant, we
state that, for every index smaller than ``K``, the array has been
modified in the expected way.

Does our loop invariant has the four good properties of a good
loop-invariant? When launching GNATprove on it, we see that ``INIT`` is
fulfilled, the invariant's initialization is proved. So are ``INSIDE`` and
``AFTER``, no potential runtime errors are reported and the assertion
following the loop is successfully verified.
However, we are missing the ``PRESERVE`` property, as GNATprove reports
that it was unable to prove that the invariant holds after the first
iteration. Investigating this failed proof attempt, we'll see that the
problem lies in the fact that GNATprove is unable to verify that at the
beginning of the loop iteration: :ada:`A (K) = A_I (K)`. Indeed, ``A`` is
modified in the loop. All that is known after the first iteration is what
is stated in the invariant. Unfortunately, nothing is stated in the
invariant about values of ``A`` after the current index. Here, we are
missing the loop's frame condition. We should add to the invariant that,
for every index ``J`` bigger than ``K``, ``A (J)`` is still equal to ``A_I
(J)``.

Alternatively, we could use the :ada:`Loop_Entry` attribute, which is used
to refer to values on entry of the loop:

.. code:: ada

    procedure Show_Loop_2 is
       type Nat_Array is array (Positive range <>) of Natural;

       function F (V : Natural) return Natural is
         (if V /= Natural'Last then V + 1 else V);

       A   : Nat_Array := (1, 2, 3);
       A_I : constant Nat_Array := A with Ghost;

    begin
       for K in A'Range loop
          A (K) := F (A (K));
          pragma Loop_Invariant
            (for all J in A'First .. K => A (J) = F (A'Loop_Entry (J)));
          --  info: loop invariant initialization proved
          --  info: loop invariant preservation proved
       end loop;
       pragma Assert (for all K in A'Range => A (K) = F (A_I (K)));
       --  info: assertion proved
    end Show_Loop_2;

In this case, thanks to the automatic generation of frame conditions in
GNATprove, we also get ``PRESERVE`` property.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

Let's review this code:

.. code:: ada

    package Ring_Buffer with SPARK_Mode is
       type Nat_Array is array (Positive range <>) of Natural;

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       function Valid_Model (M : Nat_Array) return Boolean;

       function Get_Model return Nat_Array with Ghost,
         Post => Valid_Model (Get_Model'Result)
         and Get_Model'Result'First = 1
         and Get_Model'Result'Length in Length_Range;

       procedure Push_Last (E : Natural) with
         Pre  => Get_Model'Length < Max_Size,
         Post => Get_Model = Get_Model'Old & E;

    end Ring_Buffer;

    package body Ring_Buffer with SPARK_Mode is
       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;


       function Get_Model return Nat_Array is
          Result : Nat_Array (1 .. Length);
       begin
          Result := Content (1 .. Length);
          return Result;
       end Get_Model;

       function Valid_Model (M : Nat_Array) return Boolean is
          pragma Unreferenced (M);
       begin
          return True;
          --  missing implementation
       end Valid_Model;

       procedure Push_Last (E : Natural) is
       begin
          null;
          --  missing implementation
       end Push_Last;

    end Ring_Buffer;

This is correct as ``Get_Model`` is used for specification only. Note that
calls to ``Get_Model`` cause copies of the buffer's content. They can be
automatically removed from production code by the compiler.


Example #2
~~~~~~~~~~

Let's review this code:

.. code:: ada
    :class: ada-expect-compile-error

    package Ring_Buffer with SPARK_Mode is
       type Nat_Array is array (Positive range <>) of Natural;

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean;

       procedure Push_Last (E : Natural) with
         Pre  => Valid_Model and Model.Length < Max_Size,
         Post => Valid_Model and Model.Content = Model.Content'Old & E;

    end Ring_Buffer;

    package body Ring_Buffer with SPARK_Mode is
       Content : Nat_Array (1 .. Max_Size);
       First   : Index_Range;
       Length  : Length_Range;

       function Get_Model return Nat_Array is
          Result : Nat_Array (1 .. Length);
       begin
          Result := Content (1 .. Length);
          return Result;
       end Get_Model;

       function Valid_Model return Boolean is
         (Length = Model.Length
          and then True -- not yet complete ...
         );

       procedure Push_Last (E : Natural) is
       begin
          null;
          --  missing implementation
       end Push_Last;

    end Ring_Buffer;

This example is not correct. ``Model``, which is a ghost variable, cannot
influence the return value of the normal function ``Valid_Model``. As
``Valid_Model`` is only used in specifications, it could be marked as
ghost.


Example #3
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Stack_Example_1 is
       type Nat_Array is array (Positive range <>) of Natural;

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       Content : Nat_Array (1 .. Max_Size);
       First   : Index_Range;
       Length  : Length_Range;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean;

       procedure Pop_When_Available (E : in out Natural) with
         Pre            => Valid_Model,
         Post           => Valid_Model;
    --     Contract_Cases =>
    --       (Model.Length > 0 => E & Model.Content = Model.Content'Old,
    --        others           => Model = Model'Old and E = E'Old);

       function Valid_Model return Boolean is
         (True -- not yet complete ...
         );

       procedure Pop_When_Available (E : in out Natural) is
       begin
          if Length > 0 then
             Model := (Length  => Model.Length - 1,
                       Content => Model.Content (2 .. Model.Length));
             E := Content (First);
             Length := Length - 1;
             First := (if First < Max_Size then First + 1 else 1);
          end if;
       end Pop_When_Available;

    begin
       null;
    end Stack_Example_1;

This example is correct. ``Model``, though it is marked as :ada:`Ghost`,
can be referenced from the body of the non-ghost procedure
``Pop_When_Available`` as long as it is only used in ghost statements.


Example #4
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Stack_Example_2 is
       type Nat_Array is array (Positive range <>) of Natural;

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean;

       procedure Pop_When_Available (E : in out Natural) with
         Pre            => Valid_Model,
         Post           => Valid_Model;

       function Valid_Model return Boolean is
         (True -- not yet complete ...
         );

       procedure Pop_When_Available (E : in out Natural) is
       begin
          if Model.Length > 0 then
             Model := (Length  => Model.Length - 1,
                       Content => Model.Content (2 .. Model.Length));
          end if;

          if Length > 0 then
             E := Content (First);
             Length := Length - 1;
             First := (if First < Max_Size then First + 1 else 1);
          end if;
       end Pop_When_Available;
    begin
       null;
    end Stack_Example_2;

This example is not correct. The test on ``Model`` is not allowed even
though it is only used to update its own value. Indeed, to simplify
removal of ghost code by the compiler, the only statements considered as
ghost in normal code are assignments to ghost variables and ghost
procedure calls.


Example #5
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Stack_Example_3 is
       type Nat_Array is array (Positive range <>) of Natural;

       Max_Size : constant := 100;
       subtype Length_Range is Natural range 0 .. Max_Size;
       subtype Index_Range  is Natural range 1 .. Max_Size;

       Content : Nat_Array (1 .. Max_Size) := (others => 0);
       First   : Index_Range               := 1;
       Length  : Length_Range              := 0;

       type Model_Type (Length : Length_Range := 0) is record
          Content : Nat_Array (1 .. Length);
       end record
         with Ghost;

       Model : Model_Type with Ghost;

       function Valid_Model return Boolean;

       procedure Pop_When_Available (E : in out Natural) with
         Pre            => Valid_Model,
         Post           => Valid_Model;

       function Valid_Model return Boolean is
         (True -- not yet complete ...
         );

       procedure Pop_When_Available (E : in out Natural) is
          procedure Update_Model with Ghost is
          begin
             if Model.Length > 0 then
                Model := (Length  => Model.Length - 1,
                          Content => Model.Content (2 .. Model.Length));
             end if;
          end Update_Model;

       begin
          Update_Model;

          if Length > 0 then
             E := Content (First);
             Length := Length - 1;
             First := (if First < Max_Size then First + 1 else 1);
          end if;
       end Pop_When_Available;
    begin
       null;
    end Stack_Example_3;

Everything is fine here. ``Model`` is only accessed inside
``Update_Model`` which is itself a ghost procedure. Moreover, we don't
need to add any contract to ``Update_Model``. Indeed, as it is a local
procedure, it will be inlined by GNATprove.


Example #6
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Max_Array_1 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

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
    begin
       null;
    end Max_Array_1;

This program is correct. Unfortunately, GNATprove will fail to verify that
``J`` stays in the index range of ``B``. Indeed, when checking the body of
the loop, GNATprove forgets everything about the current value of ``J`` as
it will have been modified by previous iterations of the loop. To get more
precise results, we need to provide a loop invariant.


Example #7
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Max_Array_2 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

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
             J := J + 1;
          end loop;
          return R;
       end Max_Array;
    begin
       null;
    end Max_Array_2;

This example is correct. The loop invariant now allows verifying that no
runtime error can occur in the loop's body. Unfortunately, GNATprove will
fail to verify that the invariant stays valid after the first iteration of
the loop. Indeed, knowing that ``J`` is in ``B``'Range in a given
iteration is not enough to show that it will remain so in the next
iteration. We need a more precise invariant, linking ``J`` to the value of
the loop index ``I``, like :ada:`J = I – A'First + B'First`.


Example #8
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Max_Array_3 is
       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post =>
           (for all K in A'Range =>
              Max_Array'Result (K) = Natural'Max (A (K), B (K)));

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

    begin
       null;
    end Max_Array_3;

This example is not correct. The program itself is correct but the
invariant is not, as can be checked by executing the function
``Max_Array`` with assertions enabled. Indeed, at each loop iteration,
``R`` contains the maximum of ``A`` and ``B`` only until :ada:`I – 1` as
the ``I``th index was not handled yet.


Example #9
~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Max_Array_4 is
       type Nat_Array is array (Positive range <>) of Natural;

       procedure Max_Array (A : in out Nat_Array;
                            B : Nat_Array) with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post => (for all K in A'Range =>
                    A (K) = Natural'Max (A'Old (K), B (K)));

       procedure Max_Array (A : in out Nat_Array;
                            B : Nat_Array) is
       begin
          for I in A'Range loop
             pragma Loop_Invariant
               (for all K in A'First .. I - 1 =>
                  A (K) = Natural'Max (A'Loop_Entry (K), B (K)));
             pragma Loop_Invariant
               (for all K in I .. A'Last =>
                  A (K) = A'Loop_Entry (K));
             if A (I) <= B (I) then
                A (I) := B (I);
             end if;
          end loop;
       end Max_Array;

    begin
       null;
    end Max_Array_4;

The program is correct. GNATprove can verify that the loop invariant stays
valid after the first iteration thanks to the provided frame condition: it
knows that the values stored in ``A`` after ``I`` were preserved in the
previous iterations.


Example #10
~~~~~~~~~~~

Let's review this code:

.. code:: ada

    procedure Max_Array_5 is
       type Nat_Array is array (Positive range <>) of Natural;

       procedure Max_Array (A : in out Nat_Array;
                            B : Nat_Array) with
         Pre  => A'First = B'First and A'Last = B'Last,
         Post => (for all K in A'Range =>
                    A (K) = Natural'Max (A'Old (K), B (K)));

       procedure Max_Array (A : in out Nat_Array;
                            B : Nat_Array) is
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

    begin
       null;
    end Max_Array_5;

The program is correct. GNATprove can verify that the loop invariant stays
valid after the first iteration thanks to its generation of the frame
condition: it knows that the values stored in ``A`` after ``I`` were
preserved in the previous iterations.
