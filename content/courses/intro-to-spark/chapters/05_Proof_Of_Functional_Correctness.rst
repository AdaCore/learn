:next_state: False

Proof of Functional Correctness
=====================================================================

.. include:: ../../../global.txt

This section is dedicated to the functional correctness of programs. It
presents advanced proof features that you may need to use for the
specification and verification of your program's complex properties.

Beyond Program Integrity
---------------------------------------------------------------------

When we speak about the *correctness* of a program or subprogram, we mean
the extent to which it complies with its specification. Functional
correctness is specifically concerned with properties that involve the
relations between the subprogram's inputs and outputs, as opposed to other
properties such as running time or memory consumption.

For functional correctness, we usually specify stronger properties than
those required to just prove program integrity. When we're involved in a
certification processes, we should derive these properties from the
requirements of the system, but, especially in non-certification contexts,
they can also come from more informal sources, such as the program's
documentation, comments in its code, or test oracles.

For example, if one of our goals is to ensure that no runtime error is
raised when using the result of the function :ada:`Find` below, it may be
enough to know that the result is either 0 or in the range of :ada:`A`. We can
express this as a postcondition of :ada:`Find`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Find_1

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

In this case, it's automatically proved by GNATprove.

However, to be sure that :ada:`Find` performs the task we expect, we may want
to verify more complex properties of that function. For example, we want to
ensure it returns an index of :ada:`A` where :ada:`E` is stored and returns 0
only if :ada:`E` is nowhere in :ada:`A`. Again, we can express this as a
postcondition of :ada:`Find`.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Find_2
    :class: ada-expect-prove-error

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

This time, GNATprove can't prove this postcondition automatically, but
we'll see later that we can help GNATprove by providing a loop invariant,
which is checked by GNATprove and allows it to automatically prove the
postcondition for :ada:`Find`.

Writing at least part of your program's specification in the form of
contracts has many advantages.  You can execute those contracts during
testing, which improves the maintainability of the code by detecting
discrepancies between the program and its specification in earlier stages
of development.  If the contracts are precise enough, you can use them as
oracles to decide whether a given test passed or failed. In that case, they
can allow you to verify the outputs of specific subprograms while running a
larger block of code. This may, in certain contexts, replace the need for
you to perform unit testing, instead allowing you to run integration tests
with assertions enabled. Finally, if the code is in SPARK, you can also use
GNATprove to formally prove these contracts.

The advantage of a formal proof is that it verifies all possible execution
paths, something which isn't always possible by running test cases. For
example, during testing, the postcondition of the subprogram :ada:`Find` shown
below is checked dynamically for the set of inputs for which :ada:`Find` is
called in that test, but just for that set.

.. code:: ada prove_button run_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Find_3
    :class: ada-expect-prove-error

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

However, if :ada:`Find` is formally verified, that verification checks its
postcondition for all possible inputs.  During development, you can attempt
such verification earlier than testing since it's performed modularly on a
per-subprogram basis. For example, in the code shown above, you can
formally verify :ada:`Use_Find` even before you write the body for subprogram
:ada:`Find`.


Advanced Contracts
---------------------------------------------------------------------

Contracts for functional correctness are usually more complex than
contracts for program integrity, so they more often require you to use the
new forms of expressions introduced by the Ada 2012 standard. In
particular, quantified expressions, which allow you to specify properties
that must hold for all or for at least one element of a range, come in
handy when specifying properties of arrays.

As contracts become more complex, you may find it useful to introduce new
abstractions to improve the readability of your contracts. Expression
functions are a good means to this end because you can retain their bodies
in your package's specification.

Finally, some properties, especially those better described as invariants
over data than as properties of subprograms, may be cumbersome to express
as subprogram contracts. Type predicates, which must hold for every object
of a given type, are usually a better match for this purpose. Here's an
example.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Sort

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

We can use the subtype :ada:`Sorted_Nat_Array` as the type of a variable that
must remain sorted throughout the program's execution. Specifying that an
array is sorted requires a rather complex expression involving quantifiers,
so we abstract away this property as an expression function to improve
readability.  :ada:`Is_Sorted`'s body remains in the package's specification
and allows users of the package to retain a precise knowledge of its
meaning when necessary.  (You must use :ada:`Nat_Array` as the type of the
operand of :ada:`Is_Sorted`.  If you use :ada:`Sorted_Nat_Array`, you'll get
infinite recursion at runtime when assertion checks are enabled since that
function is called to check all operands of type :ada:`Sorted_Nat_Array`.)


Ghost Code
~~~~~~~~~~

As the properties you need to specify grow more complex, you may have
entities that are only needed because they are used in specifications
(contracts).  You may find it important to ensure that these entities can't
affect the behavior of the program or that they're completely removed from
production code. This concept, having entities that are only used for
specifications, is usually called having *ghost* code and is supported in
SPARK by the :ada:`Ghost` aspect.

You can use :ada:`Ghost` aspects to annotate any entity including
variables, types, subprograms, and packages. If you mark an entity as
:ada:`Ghost`, GNATprove ensures it can't affect the program's
behavior. When the program is compiled with assertions enabled, ghost code
is executed like normal code so it can execute the contracts using it. You
can also instruct the compiler to not generate code for ghost entities.

Consider the procedure :ada:`Do_Something` below, which calls a complex
function on its input, :ada:`X`, and wants to check that the initial and
modified values of :ada:`X` are related in that complex way.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Ghost_1

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

:ada:`Do_Something` stores the initial value of :ada:`X` in a ghost constant,
:ada:`X_Init`. We reference it in an assertion to check that the computation
performed by the call to :ada:`Do_Some_Complex_Stuff` modified the value of
:ada:`X` in the expected manner.

However, :ada:`X_Init` can't be used in normal code, for example to restore
the initial value of :ada:`X`.

.. code:: ada prove_button run_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Ghost_2
    :class: ada-expect-prove-error, ada-expect-compile-error

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

When compiling this example, the compiler flags the use of :ada:`X_Init`
as illegal, but more complex cases of interference between ghost and
normal code may sometimes only be detected when you run GNATprove.


Ghost Functions
~~~~~~~~~~~~~~~

Functions used only in specifications are a common occurrence when writing
contracts for functional correctness. For example, expression functions
used to simplify or factor out common patterns in contracts can usually be
marked as ghost.

But ghost functions can do more than improve readability. In real-world
programs, it's often the case that some information necessary for
functional specification isn't accessible in the package's specification
because of abstraction.

Making this information available to users of the packages is generally out
of the question because that breaks the abstraction. Ghost functions come
in handy in that case since they provide a way to give access to that
information without making it available to normal client code.

Let's look at the following example.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Ghost_Functions

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

Here, the type :ada:`Stack` is private.  To specify the expected behavior of
the :ada:`Push` procedure, we need to go inside this abstraction and access
the values of the elements stored in :ada:`S`. For this, we introduce a
function :ada:`Get_Model` that returns an array as a representation of the
stack.  However, we don't want code that uses the :ada:`Stack` package to use
:ada:`Get_Model` in normal code since this breaks our stack's abstraction.

Here's an example of trying to break that abstraction in the subprogram
:ada:`Peek` below.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Ghost_Model
    :class: ada-expect-prove-error

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

We see that marking the function as :ada:`Ghost` achieves this goal: it
ensures that the subprogram :ada:`Get_Model` is never used in production code.

Global Ghost Variables
~~~~~~~~~~~~~~~~~~~~~~

Though it happens less frequently, you may have specifications requiring
you to store additional information in global variables that isn't needed
in normal code.  You should mark these global variables as ghost, allowing
the compiler to remove them when assertions aren't enabled. You can use
these variables for any purpose within the contracts that make up your
specifications.  A common scenario is writing specifications for
subprograms that modify a complex or private global data structure: you can
use these variables to provide a model for that structure that's updated by
the ghost code as the program modifies the data structure itself.

You can also use ghost variables to store information about previous runs
of subprograms to specify temporal properties. In the following example, we
have two procedures, one that accesses a state :ada:`A` and the other that
accesses a state :ada:`B`. We use the ghost variable :ada:`Last_Accessed_Is_A` to
specify that :ada:`B` can't be accessed twice in a row without accessing :ada:`A`
in between.

.. code:: ada prove_button run_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Global_Ghost_Vars
    :class: ada-expect-prove-error, ada-run-expect-failure

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

Let's look at another example. The specification of a subprogram's expected
behavior is sometimes best expressed as a sequence of actions it must
perform.  You can use global ghost variables that store intermediate values
of normal variables to write this sort of specification more easily.

For example, we specify the subprogram :ada:`Do_Two_Things` below in two
steps, using the ghost variable :ada:`V_Interm` to store the intermediate
value of :ada:`V` between those steps. We could also express this using an
existential quantification on the variable :ada:`V_Interm`, but it would be
impractical to iterate over all integers at runtime and this can't always
be written in SPARK because quantification is restricted to
:ada:`for ... loop` patterns.

Finally, supplying the value of the variable may help the prover verify the
contracts.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Intermediate_Values

    package Action_Sequence is

       type T is new Integer;

       V_Interm : T with Ghost;

       function First_Thing_Done (X, Y : T) return Boolean with Ghost;
       function Second_Thing_Done (X, Y : T) return Boolean with Ghost;

       procedure Do_Two_Things (V : in out T) with
         Post => First_Thing_Done (V'Old, V_Interm)
           and then Second_Thing_Done (V_Interm, V);

    end Action_Sequence;

.. note::

   For more details on ghost code, see the
   :spark_ugs:`SPARK User's Guide <specification_features.html#ghost-code>`.

Guide Proof
---------------------------------------------------------------------

Since properties of interest for functional correctness are more complex
than those involved in proofs of program integrity, we expect GNATprove to
initially be unable to verify them even though they're valid. You'll find
the techniques we discussed in :ref:`Intro_SPARK_Debugging_Failed_Proof_Attempts` to
come in handy here. We now go beyond those techniques and focus on more
ways of improving results in the cases where the property is valid but
GNATprove can't prove it in a reasonable amount of time.

In those cases, you may want to try and guide GNATprove to either complete
the proof or strip it down to a small number of easily-reviewable
assumptions. For this purpose, you can add assertions to break complex
proofs into smaller steps.

.. todo::

    This might confuse people, what is the difference between the
    first two assertions provided here?

.. code-block:: ada

    pragma Assert (Assertion_Checked_By_The_Tool);
    --  info: assertion proved

    pragma Assert (Assumption_Validated_By_Other_Means);
    --  medium: assertion might fail

    pragma Assume (Assumption_Validated_By_Other_Means);
    --  The tool does not attempt to check this expression.
    --  It is recorded as an assumption.

One such intermediate step you may find useful is to try to prove a
theoretically-equivalent version of the desired property, but one where
you've simplified things for the prover, such as by splitting up different
cases or inlining the definitions of functions.

Some intermediate assertions may not be proved by GNATprove either because
it's missing some information or because the amount of information
available is confusing. You can verify these remaining assertions by other
means such as testing (since they're executable) or by review. You can then
choose to instruct GNATprove to ignore them, either by turning them into
assumptions, as in our example, or by using a :ada:`pragma Annotate`. In
both cases, the compiler generates code to check these assumptions at
runtime when you enable assertions.


Local Ghost Variables
~~~~~~~~~~~~~~~~~~~~~

You can use ghost code to enhance what you can express inside intermediate
assertions in the same way we did above to enhance our contracts in
specifications. In particular, you'll commonly have local variables or
constants whose only purpose is to be used in assertions.  You'll mostly
use these ghost variables to store previous values of variables or
expressions you want to refer to in assertions. They're especially useful
to refer to initial values of parameters and expressions since the
:ada:`'Old` attribute is only allowed in postconditions.

In the example below, we want to help GNATprove verify the postcondition of
:ada:`P`.  We do this by introducing a local ghost constant, :ada:`X_Init`, to
represent this value and writing an assertion in both branches of an
:ada:`if` statement that repeats the postcondition, but using :ada:`X_Init`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Local_Ghost

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

You can also use local ghost variables for more complex purposes such as
building a data structure that serves as witness for a complex property of
a subprogram. In our example, we want to prove that the :ada:`Sort` procedure
doesn't create new elements, that is, that all the elements present in
:ada:`A` after the sort were in :ada:`A` before the sort.  This property isn't
enough to ensure that a call to :ada:`Sort` produces a value for :ada:`A` that's
a permutation of its value before the call (or that the values are indeed
sorted).  However, it's already complex for a prover to verify because it
involves a nesting of quantifiers. To help GNATprove, you may find it
useful to store, for each index :ada:`I`, an index :ada:`J` that has the expected
property.

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

.. todo::

    This section still seems very abstract. Can a more concrete example be developed?

Ghost procedures can't affect the value of normal variables, so they're
mostly used to perform operations on ghost variables or to group together a
set of intermediate assertions.

.. todo::

    Hard to understand what this first sentence is trying to express;
    specifically the word "treatment".

Abstracting away the treatment of assertions and ghost variables inside a
ghost procedure has several advantages. First, you're allowed to use these
variables in any way you choose in code inside ghost procedures.  This
isn't the case outside ghost procedures, where the only ghost statements
allowed are assignments to ghost variables and calls to ghost procedures.

As an example, the :ada:`for` loop contained in :ada:`Increase_A` couldn't
appear by itself in normal code.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Ghost_Proc

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

Using the abstraction also improves readability by hiding complex code that
isn't part of the functional behavior of the subprogram. Finally, it can
help GNATprove by abstracting away assertions that would otherwise make its
job more complex.

.. todo::

    What is a Proof Context? I don't think this is defined anywhere.

In the example below, calling :ada:`Prove_P` with :ada:`X` as an operand only
adds :ada:`P (X)` to the proof context instead of the larger set of assertions
required to verify it. In addition, the proof of :ada:`P` need only be done
once and may be made easier not having any unnecessary information present
in its context while verifying it.  Also, if GNATprove can't fully verify
:ada:`Prove_P`, you can review the remaining assumptions more easily since
they're in a smaller context.

.. code-block:: ada

    procedure Prove_P (X : T) with Ghost,
      Global => null,
      Post   => P (X);


Handling of Loops
~~~~~~~~~~~~~~~~~

When the program involves a loop, you're almost always required to provide
additional annotations to allow GNATprove to complete a proof because the
verification techniques used by GNATprove don't handle cycles in a
subprogram's control flow. Instead, loops are flattened by dividing them
into several acyclic parts.

As an example, let's look at a simple loop with an exit condition.

.. code-block:: ada

    Stmt1;
    loop
      Stmt2;
      exit when Cond;
      Stmt3;
    end loop;
    Stmt4;

As shown below, the control flow is divided into three parts.

.. image:: 05_loop.png
   :align: center
   :class: dark-mode-light-background

The first, shown in yellow, starts earlier in the subprogram and enters the
loop statement. The loop itself is divided into two parts.  Red represents
a complete execution of the loop's body: an execution where the exit
condition isn't satisfied.  Blue represents the last execution of the loop,
which includes some of the subprogram following it. For that path, the exit
condition is assumed to hold. The red and blue parts are always executed
after the yellow one.

GNATprove analyzes these parts independently since it doesn't have a way to
track how variables may have been updated by an iteration of the loop.  It
forgets everything it knows about those variables from one part when
entering another part. However, values of constants and variables that
aren't modified in the loop are not an issue.

In other words, handling loops in that way makes GNATprove imprecise when
verifying a subprogram involving a loop: it can't verify a property that
relies on values of variables modified inside the loop. It won't forget any
information it had on the value of constants or unmodified variables, but
it nevertheless won't be able to deduce new information about them from the
loop.

For example, consider the function :ada:`Find` which iterates over the array
:ada:`A` and searches for an element where :ada:`E` is stored in :ada:`A`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Loop
    :class: ada-expect-prove-error

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

At the end of each loop iteration, GNATprove knows that the value stored at
index :ada:`I` in :ada:`A` must not be :ada:`E`. (If it were, the loop wouldn't
have reached the end of the interation.) This proves the second assertion.  But
it's unable to aggregate this information over multiple loop iterations to
deduce that it's true for all the indexes smaller than :ada:`I`, so it can't
prove the first assertion.


.. _Intro_SPARK_Loop_Invariants:

Loop Invariants
~~~~~~~~~~~~~~~

To overcome these limitations, you can provide additional information to
GNATprove in the form of a *loop invariant*. In SPARK, a loop invariant is
a Boolean expression which holds true at every iteration of the loop.  Like
other assertions, you can have it checked at runtime by compiling the
program with assertions enabled.

The major difference between loop invariants and other assertions is the
way it's treated for proofs. GNATprove performs the proof of a loop
invariant in two steps: first, it checks that it holds for the first
iteration of the loop and then it checks that it holds in an arbitrary
iteration assuming it held in the previous iteration.  This is called
:wikipedia:`proof by induction <Mathematical_induction>`.

As an example, let's add a loop invariant to the :ada:`Find` function stating
that the first element of :ada:`A` is not :ada:`E`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Loop_Invariant_1
    :class: ada-expect-prove-error

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

To verify this invariant, GNATprove generates two checks. The first checks
that the assertion holds in the first iteration of the loop.  This isn't
verified by GNATprove. And indeed there's no reason to expect the first
element of :ada:`A` to always be different from :ada:`E` in this iteration.
However, the second check is proved: it's easy to deduce that if the first
element of :ada:`A` was not :ada:`E` in a given iteration it's still not :ada:`E` in
the next. However, if we move the invariant to the end of the loop, then it
is successfully verified by GNATprove.

Not only do loop invariants allow you to verify complex properties of
loops, but GNATprove also uses them to verify other properties, such as the
absence of runtime errors over both the loop's body and the statements
following the loop. More precisely, when verifying a runtime check or other
assertion there, GNATprove assumes that the last occurrence of the loop
invariant preceding the check or assertion is true.

Let's look at a version of :ada:`Find` where we use a loop invariant instead
of an assertion to state that none of the array elements seen so far are
equal to :ada:`E`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Loop_Invariant_2

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

This version is fully verified by GNATprove! This time, it proves that the
loop invariant holds in every iteration of the loop (separately proving
this property for the first iteration and then for the following
iterations). It also proves that none of the elements of :ada:`A` are equal to
:ada:`E` after the loop exits by assuming that the loop invariant holds in the
last iteration of the loop.

.. note::

   For more details on loop invariants, see the
   :spark_ugs:`SPARK User's Guide <assertion_pragmas.html#loop-invariants>`.

Finding a good loop invariant can turn out to be quite a challenge.  To
make this task easier, let's review the four good properties of a good loop
invariant:

+-------------+---------------------------------------------------------+
| Property    | Description                                             |
+=============+=========================================================+
| INIT        | It should be provable in the first iteration of the     |
|             | loop.                                                   |
+-------------+---------------------------------------------------------+
| INSIDE      | It should allow proving the absence of run-time errors  |
|             | and local assertions inside the loop.                   |
+-------------+---------------------------------------------------------+
| AFTER       | It should allow proving absence of run-time errors,     |
|             | local assertions, and the subprogram postcondition      |
|             | after the loop.                                         |
+-------------+---------------------------------------------------------+
| PRESERVE    | It should be provable after the first iteration of the  |
|             | loop.                                                   |
+-------------+---------------------------------------------------------+

Let's look at each of these in turn.  First, the loop invariant should be
provable in the first iteration of the loop (INIT). If your invariant fails
to achieve this property, you can debug the loop invariant's initialization
like any failing proof attempt using strategies for
:ref:`Intro_SPARK_Debugging_Failed_Proof_Attempts`.

Second, the loop invariant should be precise enough to allow GNATprove to
prove absence of runtime errors in both statements from the loop's body
(INSIDE) and those following the loop (AFTER). To do this, you should
remember that all information concerning a variable modified in the loop
that's not included in the invariant is forgotten by GNATprove. In
particular, you should take care to include in your invariant what's
usually called the loop's *frame condition*, which lists properties of
variables that are true throughout the execution of the loop even though
those variables are modified by the loop.

Finally, the loop invariant should be precise enough to prove that it's
preserved through successive iterations of the loop (PRESERVE). This is
generally the trickiest part. To understand why GNATprove hasn't been able
to verify the preservation of a loop invariant you provided, you may find
it useful to repeat it as local assertions throughout the loop's body to
determine at which point it can no longer be proved.

As an example, let's look at a loop that iterates through an array :ada:`A`
and applies a function :ada:`F` to each of its elements.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Loop_Invariant_3

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

After the loop, each element of :ada:`A` should be the result of applying
:ada:`F` to its previous value. We want to prove this.  To specify this
property, we copy the value of :ada:`A` before the loop into a ghost variable,
:ada:`A_I`. Our loop invariant states that the element at each index less than
:ada:`K` has been modified in the expected way. We use the :ada:`Loop_Entry`
attribute to refer to the value of :ada:`A` on entry of the loop instead of
using :ada:`A_I`.

Does our loop invariant have the four properties of a good loop-invariant?
When launching GNATprove, we see that ``INIT`` is fulfilled: the
invariant's initialization is proved. So are ``INSIDE`` and ``AFTER``: no
potential runtime errors are reported and the assertion following the loop
is successfully verified.

The situation is slightly more complex for the ``PRESERVE``
property. GNATprove manages to prove that the invariant holds after the
first iteration thanks to the automatic generation of frame conditions. It
was able to do this because it completes the provided loop invariant with
the following frame condition stating what part of the array hasn't been
modified so far:

.. code-block:: ada

             pragma Loop_Invariant
               (for all J in K .. A'Last => A (J) = (if J > K then A'Loop_Entry (J)));

GNATprove then uses both our and the internally-generated loop invariants
to prove ``PRESERVE``. However, in more complex cases, the heuristics used
by GNATprove to generate the frame condition may not be sufficient and
you'll have to provide one as a loop invariant. For example, consider a
version of :ada:`Map` where the result of applying :ada:`F` to an element at
index :ada:`K` is stored at index :ada:`K-1`:

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Loop_Invariant_4
    :class: ada-expect-prove-error

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
                A (K - 1) := F (A (K));
             end if;
             pragma Loop_Invariant
               (for all J in A'First .. K =>
                 (if J /= A'First then A (J - 1) = F (A'Loop_Entry (J))));
             --  pragma Loop_Invariant
             --  (for all J in K .. A'Last => A (J) = A'Loop_Entry (J));
          end loop;
          pragma Assert (for all K in A'Range =>
                          (if K /= A'First then A (K - 1) = F (A_I (K))));
       end Map;

    end Show_Map;

.. todo::

    Uncommenting the code did not result in successful analysis.

You need to uncomment the second loop invariant containing the frame condition
in order to prove the assertion after the loop.

.. note::

   For more details on how to write a loop invariant, see the
   :spark_ugs:`SPARK User's Guide <how_to_write_loop_invariants.html>`.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

We implement a ring buffer inside an array :ada:`Content`, where the contents
of a ring buffer of length :ada:`Length` are obtained by starting at index
:ada:`First` and possibly wrapping around the end of the buffer. We use a
ghost function :ada:`Get_Model` to return the contents of the ring buffer for
use in contracts.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_01

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

This is correct: :ada:`Get_Model` is used only in contracts.  Calls to
:ada:`Get_Model` make copies of the buffer's contents, which isn't efficient,
but is fine because :ada:`Get_Model` is only used for verification, not in
production code. We enforce this by making it a ghost function.  We'll
produce the final production code with appropriate compiler switches (i.e.,
not using ``-gnata``) that ensure assertions are ignored.


Example #2
~~~~~~~~~~

Instead of using a ghost function, :ada:`Get_Model`, to retrieve the contents
of the ring buffer, we're now using a global ghost variable, :ada:`Model`.

.. code:: ada compile_button prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_02
    :class: ada-expect-compile-error, ada-expect-prove-error

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

This example isn't correct. :ada:`Model`, which is a ghost variable, must not
influence the return value of the normal function :ada:`Valid_Model`. Since
:ada:`Valid_Model` is only used in specifications, we should have marked it as
:ada:`Ghost`. Another problem is that :ada:`Model` needs to be updated inside
:ada:`Push_Last` to reflect the changes to the ring buffer.


Example #3
~~~~~~~~~~

Let's mark :ada:`Valid_Model` as :ada:`Ghost` and update :ada:`Model` inside
:ada:`Push_Last`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_03

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

This example is correct. The ghost variable :ada:`Model` can be referenced
both from the body of the ghost function :ada:`Valid_Model` and the non-ghost
procedure :ada:`Push_Last` as long as it's only used in ghost statements.


Example #4
~~~~~~~~~~

We're now modifying :ada:`Push_Last` to share the computation of the new
length between the operational and ghost code.

.. code:: ada compile_button prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_04
    :class: ada-expect-prove-error

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

This example isn't correct. We didn't mark local constant :ada:`New_Length` as
:ada:`Ghost`, so it can't be computed from the value of ghost variable
:ada:`Model`. If we made :ada:`New_Length` a ghost constant, the compiler would
report the problem on the assignment from :ada:`New_Length` to :ada:`Length`. The
correct solution here is to compute :ada:`New_Length` from the value of the
non-ghost variable :ada:`Length`.


Example #5
~~~~~~~~~~

Let's move the code updating :ada:`Model` inside a local ghost procedure,
:ada:`Update_Model`, but still using a local variable, :ada:`New_Length`, to
compute the length.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_05

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

Everything's fine here. :ada:`Model` is only accessed inside :ada:`Update_Model`,
itself a ghost procedure, so it's fine to declare local variable
:ada:`New_Length` without the :ada:`Ghost` aspect: everything inside a ghost
procedure body is ghost. Moreover, we don't need to add any contract to
:ada:`Update_Model`: it's inlined by GNATprove because it's a local procedure
without a contract.

Example #6
~~~~~~~~~~

The function :ada:`Max_Array` takes two arrays of the same length (but not
necessarily with the same bounds) as arguments and returns an array with
each entry being the maximum values of both arguments at that index.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_06
    :class: ada-expect-prove-error

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

    end Array_Util;

    package body Array_Util is

       function Max_Array (A, B : Nat_Array) return Nat_Array is
          R : Nat_Array (A'Range);
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

This program is correct, but GNATprove can't prove that :ada:`J` is always in
the index range of :ada:`B` (the unproved index check) or even that it's
always within the bounds of its type (the unproved overflow check). Indeed,
when checking the body of the loop, GNATprove forgets everything about the
current value of :ada:`J` because it's been modified by previous loop
iterations. To get more precise results, we need to provide a loop
invariant.


Example #7
~~~~~~~~~~

Let's add a loop invariant that states that :ada:`J` stays in the index range
of :ada:`B` and let's protect the increment to :ada:`J` by checking that it's not
already the maximal integer value.

.. code:: ada prove_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_07
    :class: ada-expect-prove-error

    package Array_Util is

       type Nat_Array is array (Positive range <>) of Natural;

       function Max_Array (A, B : Nat_Array) return Nat_Array with
         Pre => A'Length = B'Length;

    end Array_Util;

    package body Array_Util is

       function Max_Array (A, B : Nat_Array) return Nat_Array is
          R : Nat_Array (A'Range);
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

The loop invariant now allows verifying that no runtime error can occur in
the loop's body (property INSIDE seen in section :ref:`Intro_SPARK_Loop_Invariants`).
Unfortunately, GNATprove fails to verify that the invariant
stays valid after the first iteration of the loop (property
PRESERVE). Indeed, knowing that :ada:`J` is in :ada:`B'Range` in a given
iteration isn't enough to prove it'll remain so in the next iteration. We
need a more precise invariant, linking :ada:`J` to the value of the loop index
:ada:`I`, like :ada:`J = I - A'First + B'First`.


Example #8
~~~~~~~~~~

We now consider a version of :ada:`Max_Array` which takes arguments that have
the same bounds. We want to prove that :ada:`Max_Array` returns an array of
the maximum values of both its arguments at each index.

.. code:: ada prove_button run_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_08
    :class: ada-expect-prove-error, ada-run-expect-failure

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

Here, GNATprove doesn't manage to prove the loop invariant even for the
first loop iteration (property INIT seen in section :ref:`Intro_SPARK_Loop_Invariants`).
In fact, the loop invariant is incorrect, as you can see by
executing the function :ada:`Max_Array` with assertions enabled: at each loop
iteration, :ada:`R` contains the maximum of :ada:`A` and :ada:`B` only until
:ada:`I - 1` because the :ada:`I`'th index wasn't yet handled.


Example #9
~~~~~~~~~~

We now consider a procedural version of :ada:`Max_Array` which updates its
first argument instead of returning a new array. We want to prove that
:ada:`Max_Array` sets the maximum values of both its arguments into each index
in its first argument.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_09

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

Everything is proved. The first loop invariant states that the values of
:ada:`A` before the loop index contains the maximum values of the arguments of
:ada:`Max_Array` (referring to the input value of :ada:`A` with
:ada:`A'Loop_Entry`). The second loop invariant states that the values of
:ada:`A` beyond and including the loop index are the same as they were on
entry.  This is the frame condition of the loop.


Example #10
~~~~~~~~~~~

Let's remove the frame condition from the previous example.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Proof_of_Functional_Correctness.Example_10

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

Everything is still proved.  GNATprove internally generates the frame
condition for the loop, so it's sufficient here to state that :ada:`A` before
the loop index contains the maximum values of the arguments of
:ada:`Max_Array`.
