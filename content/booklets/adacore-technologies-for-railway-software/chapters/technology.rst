.. include:: ../../../global.txt

Technology Usage Guide
======================

This chapter explains how AdaCore's tools and technologies support a variety
of techniques from Annex D.

.. _Railway_SW_Analyzable_Programs:

.. index:: single: Annex D; Analyzable Programs (D.2)
.. index:: single: Ada language; Support for Analyzable Programs (D.2)

Analyzable Programs (D.2)
-------------------------

The Ada language has been designed to increase program specification
expressiveness and verification. Explicit constraints at the code level
can be used as the basis of both manual analysis (inspection), such as
code reviews, and automatic analysis, ranging from the compiler's
semantic consistency checks to the SPARK tools' formal proof of
program properties.

Examples of Ada language features supporting analysis include:

* type and subtype ranges and predicates
* parameter modes and subprogram contracts
* packages and private types (encapsulation)
* the Ravenscar concurrency profile
* minimal set of implicit or undefined behaviors

.. index:: single: GNATmetric; Support for Analyzable Programs (D.2)
.. index:: single: GNATcheck; Support for Analyzable Programs (D.2)

Tools such as GNATmetric and GNATcheck allow monitoring the complexity
and quality of the code and identifying potentially problematic constructs.
This is accomplished through techniques such as basic code size metrics,
cyclomatic complexity computation, and coupling analysis.

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Analyzable Programs (D.2)

GNAT SAS identifies potential run-time errors in the code. The number of
false positive results depends on the code complexity. A high number of
false positives is often a symptom of overly-complicated code. Using GNAT SAS
during development allows finding locations in the code that are
overly complex and provides information on what needs to be improved.

.. index:: single: SPARK technology; Support for Analyzable Programs (D.2)

The SPARK language is much more ambitious in analyzing programs, at the
extreme supporting full correctness proofs against formally specified
requirements.
It structurally forbids features such as exceptions, which complicate or
prevent formal analysis.
Code that is hard to analyze is often hard to understand and maintain,
and conversely.
Using SPARK as part of the development phase thus results in code that is
not only maximally analyzable but also clear and readable.

.. index:: single: GNAT Studio IDE; Support for Analyzable Programs (D.2)

During code review phases, GNAT Studio offers a variety of features that can
be used for program analysis, in particular call graphs, reference searches,
and other code organization viewers.

.. index:: single: Annex D; Boundary Value Analysis (D.4)

Boundary Value Analysis (D.4)
-----------------------------

The objective of this technique is to verify and test the behavior of a
subprogram at the limits and boundaries values of its parameters.
AdaCore's technologies can provide complementary assurance on the quality
of this analysis and potentially decrease the number of tests that need
to be performed.

.. index:: single: Ada language; Support for Boundary Value Analysis (D.4)

Ada's strong typing allows refining types and variables boundaries.
For example:

.. code-block:: ada

    type Temperature is new Float range -273.15 .. 1_000;
    V : Temperature;

Additionally, it's possible to define the specific behavior of values at
various locations in the code. For example, it's possible to define
relationships between the input and output of a subprogram, in the form
of a partitioning of the input domain:

.. code-block:: ada

    function Compute (J : Integer) return Integer
       with Contract_Cases => (J = Integer'First => Compute'Result = -1,
                               J = Integer'Last  => Compute'Result = 1,
                               others            => J - 1);

The above shows an input partition of one parameter (but it can also be a
combination of several parameters). The behavior on the boundaries of :ada:`J`
is specified and can then either be tested (for example, with enabled
assertions) or formally proven with SPARK. Further discussion of input
partitioning can be found in the context of
:ref:`Railway_SW_Equivalence_Classes_and_Input_Partition_Testing`.

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Boundary Value Analysis (D.4)

Another possibility is to use GNAT SAS to identify possible values for
variables, and propagate those values from call to call, constructing
lists and/or ranges of potential values for each variable at each point
of the program. These are used as the input to run-time error analysis.
When used in full-soundness mode, GNAT SAS provides guarantees that the
locations it reports on the code are the only ones that may have run-time
errors, thus allowing a reduction of the scope of testing and review to
only these places.

However, it's important to stress that GNAT SAS is only performing this
boundary value analysis with respect to potential exceptions and robustness.
No information is provided regarding the correctness of the values
produced by subprograms.

GNAT SAS also has the capacity to display the possible values of variables
and parameters. This can be used as a mechanism to increase confidence
that testing has taken into account all possible boundaries for values.

.. index:: single: SPARK technology; Support for Boundary Value Analysis (D.4)
.. index:: Absence of Run-Time Errors (AORTE)
.. index:: single: SPARK technology; Absence of Run-Time Errors (AORTE)

SPARK has the ability to perform similar absence of run-time errors (AORTE)
analysis, thus reaching the same objectives. In addition to the above,
when requirements can be described in the form of boolean contracts,
SPARK can demonstrate correctness of the relation between input and output
on the entire range of values.

.. index:: single: Annex D; Control Flow Analysis (D.8)
.. index:: single: GNAT Studio IDE; Support for Control Flow Analysis (D.8)

Control Flow Analysis (D.8)
---------------------------

Control flow analysis requires identifying poor and incorrect data structures,
including unreachable code and useless tests in the code (such as conditions
that are always true).

GNAT Studio can display call graphs between subprograms, allowing
visualization and analysis of control flow in the application.

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Control Flow Analysis (D.8)

GNAT SAS contributes to control flow analysis by identifying unreachable code,
as well as conditions being always true or always false. This analysis is
partial and needs to be completed with other techniques such as code review or
code coverage analysis, which together will allow reaching higher levels of
confidence.

.. index:: single: GNATmetric; Support for Control Flow Analysis (D.8)

GNATmetric can compute coupling metrics between units, helping to identify
loosely or tightly coupled units.

.. index:: single: GNATstack; Support for Control Flow Analysis (D.8)

GNATstack computes worst-case stack consumption based on the application's
call graph. This can help identify poorly structured code which consumes
too much memory on some sequences of calls.

.. index:: single: Annex D; Data Flow Analysis (D.10)

Data Flow Analysis (D.10)
-------------------------

.. index:: single: GNAT Pro Assurance; Support for Data Flow Analysis (D.10)

The GNAT Pro toolchain can be configured to detect uninitialized variables
at run-time through the use of the pragma :ada:`Initialize_Scalars`.
With this pragma, all scalars are automatically initialized to either an
out-of-range value (if there is one) or to an "unusual" value (either
the largest or smallest).
This significantly improves detection at test time.

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Data Flow Analysis (D.10)

GNAT SAS can detect suspicious and potentially incorrect data
flows, such as variables that are read before they are written
(uninitialized variables), variables written more than once without being
read (redundant assignments), and variables that are written but never read.
This analysis is partial and needs to be completed with other techniques
such as formal proof, code review or code coverage analysis, which together
allow reaching higher levels of confidence.

.. index:: single: SPARK technology; Support for Data Flow Analysis (D.10)

SPARK performs this analysis and much more, allowing the specification
and verification of data flow. This is used in the following activities:

* verification that all inputs and outputs have been specified,
  including possible side effects
* verification that all dependencies between inputs and outputs are specified
* verification that the implemented dataflow corresponds to the one specified

Here's an example:

.. code-block:: ada

    procedure Compute (A, B, C : Integer; R1, R2 : out Integer)
       with Depends => (R1 => (A, B),
                        R2 => (B, C));

    procedure Compute (A, B, C : Integer; R1, R2 : out Integer) is
    begin
       R1 := A + B;
       R2 := A + B - C;
    end Compute;

:ada:`R1` is required to be computed from :ada:`A` and :ada:`B`, and :ada:`R2`
from :ada:`B` and :ada:`C`. However, in the procedure body, :ada:`R2` also
depends on :ada:`A`. SPARK's formal proof detects this error.

The error is likewise detected in the presence of branches:

.. code-block:: ada

    procedure Compute (A, B, C : Integer; R1, R2 : out Integer) is
    begin
       R1 := A + B;
       if A = 0 then
          R2 := B + C;
       else
          R2 := B - C;
       end if;
    end Compute;

Here :ada:`R2` depends on the result of the expression :ada:`A = 0`,
so its value is actually computed from
:ada:`A`, :ada:`B` and :ada:`C`, and not just :ada:`B` and :ada:`C`.
As in the previous case, SPARK's formal analysis detects the error.

A similar result occurs when the dependence is indirect, through a subprogram
call.
Here's an example based on a logging procedure that has a global state,
:ada:`Screen`, which is written to by the procedure:

.. code-block:: ada

     procedure Log (V : String)
        with Global  => (Output => Screen),
             Depends => (Screen => V)

     procedure Compute (A, B, C : Integer; R1, R2 : out Integer)
       with Depends => (R1 => (A, B),
                        R2 => (B, C));

     procedure Compute (A, B, C : Integer; R1, R2 : out Integer) is
     begin
        R1 := A + B;
        R2 := B + C;

        if A = 0 then
           Log ("A is 0");
        end if;
     end Compute;

The data flow does not correspond to the specification: :ada:`Compute` should
specify that it modifies :ada:`Screen`. So the incorrect code is detected.
The error is detected whether or not a branch is present, serving as a useful
complement to structural code coverage in many cases.

.. index:: single: Annex D; Defensive Programming (D.14)

Defensive Programming (D.14)
----------------------------

As stated in sub-clause D.14, the goal of defensive programming is to
"detect anomalous control flow, data flow, or data values ... \
and react to them in a predetermined and acceptable manner".

.. index:: single: Ada language; Support for Defensive Programming (D.14)

Ada's strong typing will avoid the need for many situations where constraints
would be expressed in the form of defensive code. However, in some situations
strong typing is not enough. This can be the case, for example, when accessing
an element of an array. In this case, Ada allows expressing constraints in the
specification, through preconditions, postconditions or predicates.

Beyond this, Ada provides specific support for a subset of what's specified
in the D.14 annex. GNAT SAS and SPARK will allow the development of defensive
programming in places where it makes the most sense.

.. index:: single: GNATcheck; Support for Defensive Programming (D.14)

Specific defensive code rules can also be defined in the coding standard and
their verification can then be automated through code analysis using,
for example, GNATcheck.

.. index:: Range checking

Data should be range checked
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada offers types and subtypes that are naturally associated with ranges, e.g.:

.. code-block:: ada

    subtype Percent is Integer range 0 .. 100;
    -- Percent is the same type as Integer but with a run-time constraint on its range

    X, Y : Integer;
    V    : Percent;
    ...
    V := X + Y; -- Raises exception Constraint_Error if X + Y is not in 0 .. 100
    ...

It's then the task of the developer to react to potential exceptions.
Alternatively, it's possible to write explicit verification in the code to
ensure that the expression is within its boundary:

.. code-block:: ada

    V_Int : Integer;
    V_Pct : Percent;
    ...
    V_Int := X+Y;
    if V_Int in Percent then
       V_Pct := V_Int;
    else
      ... -- Respond to out-of-range result
    end if;

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Defensive Programming (D.14)
.. index:: single: SPARK technology; Support for Defensive Programming (D.14)

Another way to proactively ensure the absence of range check failure is
to use tools such as GNAT SAS or SPARK, which statically identify the
only possible locations in the code where such failures can happen.

Note that run-time checks can be deactivated if needed for performance
reasons, for example once thorough testing or formal proof has been
performed.

Data should be dimension-checked
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: single: GNAT Pro Assurance; Support for Defensive Programming (D.14)
.. index:: Dimension consistency analysis

The GNAT Pro compiler provides a language extension for dimensional
consistency analysis, which ensures that variables are properly typed
according to their dimension. The system is implemented based on the
seven base dimensions (meter, kilogram, second, ampere, kelvin, mole,
candela), and will check that operations between these types are
consistent.
For example, a type :ada:`Speed` can be defined to represent time per
distance. Consistency between these types is checked at compile time
so that dimension errors will be reported as errors. For example:

.. code-block:: ada

    D       : Distance := 10;
    T       : Time     := 1;
    S       : Speed    := D / T; -- OK
    My_Time : Time     := 100;
    ...
    Distance_Traveled := S / My_Time;
    -- Error, resulting dimension is Distance / Time**2
    -- The expression should be S * My_Time

.. index:: single: Ada language; Support for Defensive Programming (D.14)
.. index:: single: Ada language; Parameter checking

Read-only and read-write parameters should be separated and their access checked
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Ada, the parameter mode is specified in parameter specifications and
is checked by the compiler. For example, a read-only parameter is passed as
mode :ada:`in` and may not be modified. A read-write parameter is passed
either as mode :ada:`in out` or as mode :ada:`out` and is modifiable.
(The :ada:`out` mode is appropriate if the parameter is written before being
read). The compiler will produce an error for
an attempted modification of :ada:`in` parameters and detect when an
:ada:`in out` or :ada:`out` parameter is not modified and so could have been
passed as :ada:`in`. For example:

.. code-block:: ada

    procedure P (V : in Integer) is
    begin
       V := 5; -- ERROR, V is mode "in"
    end P;

Functions should treat all parameters as read-only
--------------------------------------------------

The original version of Ada required that functions could only have :ada:`in`
parameters.
This restriction was relaxed in a later version of the standard,
but the original behavior can be reverted through a GNATcheck rule.
The SPARK Ada subset forbids functions with writable (i.e., :ada:`out` or
:ada:`in out` parameters).

Literals and constants should not be write-accessible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada provides many kinds of literals (e.g. numeric, character, enumeration,
string) and allows declaring constants of any type
but ensures that their values can not be updated.

.. code-block:: ada

    type Color is (Red, Blue, Green);
    Answer    : constant Integer := 42;
    One_Third : constant         := 1.0 / 3.0;
    Greeting  : String           := "Hello";

The literals and constants are read-only as per language definition.
For example, trying to pass :ada:`Red` or :ada:`Answer` to a subprogram
as an :ada:`out` or :ada:`in out` parameter would be illegal.
Note that :ada:`Greeting` is a variable and can be assigned to, but
the literal :ada:`"Hello"` is immutable.

.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Defensive Programming (D.14)
.. index:: single: SPARK technology; Support for Defensive Programming (D.14)

Using GNAT SAS and SPARK to drive defensive programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GNAT SAS and SPARK identify locations where there are potential run-time
errors |mdash| in other words, places where code is either wrong or where
defensive programming should be deployed. This helps guide the writing
of defensive code. For example:

.. code-block:: ada

    procedure P (S : String; V : Integer) is
       C : Character;
    begin
       ...
       C := S (V);
       ...
    end P;

In the above code, there's a use of :ada:`V` as an index into the
String :ada:`S`.
GNAT SAS and SPARK will detect the potential for a run-time error. Protection
of the code to prevent the error can take several forms:

.. rubric:: Explicit test

The application code checks that :ada:`V` is in range before using its
value as an index into the String. If the check fails, the appropriate
recovery action can be taken (here the procedure simply returns).

.. code-block:: ada

    procedure P (S : String; V : Integer) is
       C : Character;
    begin
       ...
       if V not in S'Range then
          return;
       end if;
       C := S (V)
       ...
    end P;

.. rubric:: Precondition

Here the error is detected at call time. If assertion checking is
enabled and the check fails, the :ada:`Assertion_Check` exception
is raised.

.. code-block:: ada

   procedure P (S : String; V : Integer)
      with Pre => V in S'Range
   is
      C : Character;
   begin
      ...
      C := S (V);
      ...
   end P;

The main difference between GNAT SAS and SPARK is that GNAT SAS may miss some
potential run-time errors (except when run only on small pieces of code if
configured in "sound" mode), while SPARK requires the use of the appropriate
Ada subset but is a sound technology (i.e., it will detect all potential
run-time errors).

In general, the recommended Ada style is to use contracts instead of defensive
code.

.. index:: single: Annex D; Coding Standards and Style Guide (D.15)
.. index:: single: GNATcheck; Support for Coding Standards and Style Guide (D.15)
.. index:: single: pragma Restrictions; Support for Coding Standards and Style Guide (D.15)

Coding Standards and Style Guide (D.15)
---------------------------------------

A coding standard can be defined using a combination of predefined rules
(using GNAT options and GNATcheck rules) and appropriate arguments to
pragma :ada:`Restrictions`.

.. _Railway_SW_Equivalence_Classes_and_Input_Partition_Testing:

.. index:: single: GNAT Pro Assurance; Support for Coding Standards and Style Guide (D.15)
.. index:: single: SPARK technology; Support for Coding Standards and Style Guide (D.15)

Equivalence Classes and Input Partition Testing (D.18)
------------------------------------------------------

This technique involves partitioning the various potential inputs to
subprograms and creating a testing and verification strategy based on
this partitioning.

Ada extensions included in GNAT Pro for Ada can support partitioning at the
source code level. The partition is a list of conditions for inputs together
with their associated expected output, verifying the following criteria:

* The full set of all potential values is covered
* There is no overlap between partitions

These criteria can be verified either dynamically, by verifying at
test time that all inputs exercised fall into one and only one partition,
or formally by SPARK, proving that the partition are indeed complete
and disjoint.

.. index:: Contract_Cases aspect

Here's a simple example of such partitioning with two input variables:

.. code-block:: ada

    function ArcTan (X, Y : Float) return Float with
       Contract_Cases =>
          (X >= 0 and Y >= 0 => ArcTan'Result >= 0        and ArcTan'Result <= PI/2,
           X <  0 and Y >= 0 => ArcTan'Result >= PI/2     and ArcTan'Result <= PI,
           X <  0 and Y < 0  => ArcTan'Result >= PI       and ArcTan'Result <= 3 * PI/2,
           X >= 0 and Y < 0  => ArcTan'Result >= 3 * PI/2 and ArcTan'Result <= 2 * PI);

The presence of these contracts enable further verification. At run time,
they act as assertions and allow verification that the form of the output
indeed corresponds to the expected input. If SPARK is used, it's possible
to formally verify the correctness of the relation between the input and
properties.

.. index:: single: Annex D; Error Guessing (D.20)
.. index:: single: GNATfuzz; Support for Error Guessing (D.20)

Error Guessing (D.20)
---------------------

The GNATfuzz tool for fuzz testing (part of the GNAT Dynamic Analysis Suite)
supports the Error Guessing technique and can provide evidence for a system's
robustness.
GNATfuzz exercises a program with a large number of automatically generated
test values, often random or malformed, and checks for crashes, hangs,
and other anomalous behavior.

.. index:: single: Annex D; Failure Assertion Programming (D.24)
.. index:: single: pragma Assert; Support for Failure Assertion Programming (D.24)
.. index:: single: Contract-based programming; Support for Failure Assertion Programming (D.24)

Failure Assertion Programming (D.24)
------------------------------------

Ada offers a large variety of assertions (contracts) that can be defined
in the code, either through pragmas or aspects.

* Pragma :ada:`Assert`

  This pragma allows verification within a sequence of statements:

  .. code-block:: ada

      A := B + C;
      pragma Assert (A /= 0);
      D := X / A;

* Pre- and postcondition contracts

  Pre- and postconditions can be defined as subprogram aspects:

  .. code-block:: ada

      procedure Double (X : in out Integer)
         with Pre  => X < 100,
              Post => X = X'Old * 2;

* Predicates and invariants

  Predicate and invariant contracts can be defined on types:

  .. code-block:: ada

      type Even is new Integer
      with Dynamic_Predicate => Even mod 2 = 0;

These contracts can be checked dynamically, for example, during testing.
The developer has fine control over which contracts can be removed
(e.g. for improved performance) and which should remain in the deployed
software.

.. index:: single: SPARK technology; Support for Failure Assertion Programming (D.24)

The contracts can be used by the static analysis and formal proof tools.
GNAT SAS uses contracts to refine its analysis and exploits them as
assertions, even if it may not be able to demonstrate that they are correct.
In this manner, contracts provide the tool with additional information
on the code behavior. SPARK can go further and either prove their
correctness, or else report its inability to do so.  (In the latter case,
the issue is either that the contract or the code is incorrect, or that
the proof engine is not powerful enough to construct a proof.)

.. index:: single: Annex D; Formal Methods (D.28)
.. index:: single: SPARK technology; Support for Formal Methods (D.28)

Formal Methods (D.28)
---------------------

With SPARK, formal methods are used to define and check certain architectural
properties, in particular for data coupling specification and verification.
For example:

.. code-block:: ada

    G : Integer;

    procedure P (X, Y : Integer)
    with Global => (Output => G),
         Depends => (G => (X, Y));

In the above example, the side effect of the subprogram is fully defined:
:ada:`P` is modifying :ada:`G`. SPARK will check that this side effect,
and no other, is present. :ada:`G` is specified as depending on the values
of :ada:`X` and :ada:`Y`. Again, SPARK will analyze the code to check that
the variable relationships specified are correct.

In this example, an actual variable is used to define data flow.
It's also possible to create an "abstract" state, implemented by a set of
variables. Generally speaking, although these notations and verifications
are quite useful on the lower levels of the architecture, they may not be
that pertinent at higher levels. SPARK is flexible with regard to where
this should be checked or and where it should not.

At the lower level of the design phases, some properties and requirements
can be refined or specified in the form of boolean expressions.
SPARK will allow expressing these properties, including the formalism of
first-order logic (quantifiers). These properties can be expressed in the
form of subprogram preconditions, postconditions, type invariants and type
predicates. For example:

.. code-block:: ada

    -- P must have an input greater or equal to 10, and then has to modify V.
    procedure P (V : in out Integer)
    with Pre  => V     >= 10,
         Post => V'Old /= V;

    -- Variables of type Even must be even
    type Even is new Integer
    with Dynamic_Predicate => Even mod 2 = 0;

    -- Arrays of this type are always sorted in ascending order
    type Sorted_Array is array (Integer range <>) of Integer
    with Dynamic_Predicate =>
       Sorted_Array'Size <= 1 or else
       (for all I in Sorted_Array'First .. Sorted_Array'Last - 1 =>
           Sorted_Array (I) <= Sorted_Array (I + 1));

These properties can be formally verified through the SPARK toolset, using
state of the art theorem proving methodologies. Testing aimed at verifying
the correctness of these properties can then be simplified, if not entirely
removed.

.. index:: single: Annex D; Impact Analysis (D.32)
.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Support for Impact Analysis (D.32)


Impact Analysis (D.32)
----------------------

Identifying the effect of a change on entire software component requires
the combination of various techniques, including reviews, testing and static
analysis. GNAT SAS has specific features to identify the impact of a change
from the perspective of potential run-time errors. It can establish a baseline
with regard to potential failure analysis and filter only the potential
defects that have been introduced or repaired following a change in the code.

.. index:: single: GNAT Studio IDE; Support for Impact Analysis (D.32)

GNAT Studio can provide call graphs and call trees, allowing the developer
to see how a function is called in the software. This can be directly used
in impact analysis.

.. index:: single: Annex D; Information Encapsulation (D.33)

.. index:: single: Ada language; Support for Information Encapsulation (D.33)

Information Encapsulation (D.33)
--------------------------------

Information encapsulation is good software engineering practice,
enforcing access to data on a "need to know" basis and preventing
hard-to-detect bugs from erroneous updates to global variables.
Encapsulation has been intrinsic to the Ada design since the earliest
version of the language and is embodied in the syntax and semantics
of a variety of language features.

.. index:: single: Ada language; Object-Oriented Programming (OOP)

Ada's approach to encapsulation achieves similar methodological benefits to
Object-Oriented Programming, but with a different syntax.
In most OO languages, a class is both a type (which can be
instantiated to produce objects) and a module (which can
be separately compiled). Ada separates these concepts,
modeling a class by a type (typically a private type, as will
be shown below) defined with a package (the main unit of
modularization in Ada).

.. rubric:: Separation of specification and body

The various program units in Ada |mdash| packages, tasks, subprograms,
generic templates |mdash| have a structure that supports the separation of
the unit's specification (its interface to other units) and its
implementation (inaccessible externally).
This physical separation not only supports encapsulation but also
facilitates independent development of the two parts. For example,
a package specification can be produced during the detailed design
phase, with the body fleshed out later, perhaps by a different
developer, during the implementation phase.

.. rubric:: Package structure

.. index:: single: Ada language; Package feature

A package comprises at least a specification and, if necessary,
a body that implements the subprograms and other entities whose
specifications are in the package specification.
The package specification in general consists of a visible part
and a private part. In a typical scenario, the visible part declares
a type as :ada:`private`, along with subprogram specifications
for the operations that are relevant to that type.
The type and operations form the interface for that type.
The private part of the package specification then provides
the full declaration of the type, and the package body supplies
the bodies for the subprograms defined for the type.

External to the package, the type name and the operations
defined for the type are accessible, but the representational details
for the full type declaration are hidden.
This allows the designer of the package to modify the representation
of the type during development or maintenance, without requiring
source code changes to client code.
This principle is sometimes referred to as *data abstraction*.
Here is an example:

.. code-block:: ada

    package Counters is
       type Counter is private;
       --  We don't want to give access to the representation of the counter here

       procedure Increment (C : in out Counter);
       procedure Print (C : in out Counter);

    private

       type Counter is new Integer;
        -- Here, Counter is an Integer, but it could change to something
        -- else if needed without disturbing the interface.
    end Counters;

    with Ada.Text_IO;
    package body Counters is
       procedure Increment (C : in out Counter) is
       begin
          C := C + 1;
       end Increment;

       procedure Print (C : in out Counter) is
       begin
          Ada.Text_IO.Put_Line (C'Img);
       end Print;
    end Counters;

As a variation on this example, Ada supports encapsulation through
"getter" and "setter" subprograms.
Rather than directly manipulating a global variable declared in a
package specification, the program can be structured to enforce
accesses through a procedural interface:

.. code-block:: ada

    package Data is
       function Value return Integer;       -- "Getter" function
       procedure Set (New_Value : Integer); -- "Setter" procedure
    end Data;

    package body Data is
       Global : Integer := 0;

       function Value return Integer is
       begin
          return Global;
       end Value;

       procedure Set (New_Value : Integer) is
       begin
          Value := New_Value;
       end Set;
    end Data;

.. index:: single: Ada language; Concurrent programming
.. index:: single: Ada language; Task object / task type
.. index:: single: Ada language; Protected object / Protected type

.. rubric:: Concurrency

Both of Ada's tasking constructs |mdash| the task and the protected object
|mdash| enforce encapsulation.

* A task object or task type specification defines its interface
  (its entries, which are used for synchronization and communication), and its
  body defines the implementation.

* A protected object or protected type specification defines its interface
  (entries and procedures, which are executed with mutual exclusion),
  and its body defines the implementation.
  The private part of a protected object/type specification
  encapsulates the data that is being protected: it can only be accessed
  externally through the entries and procedures that it defines.

.. index:: single: Ada language; Representation clause

.. rubric:: Representation clauses

As another form of encapsulation,
Ada's representation clause facility
separates an entity's logical properties |mdash|
its interface to client code |mdash| and its representation.
For example:

.. code-block:: ada

    type Alert is (Low, Medium, High);

    type Packet is record
      Flag :   Boolean;
      Danger : Alert;
      Data :   Interfaces.Unsigned_8;
    end record;

    Byte : constant := 8;

    for Alert use (Low => 0, Medium => 5, High => 10);
    for Alert'Size use 4;

    for Packet use record
       Flag   at 0*Byte range 3 .. 3;  -- Bits 1..2 are unused
       Danger at 0*Byre range 4 .. 7;
       Data   at 1*Byte range 0 .. 7;
    end Packet;
    for Packet'Size use 2*Byte;


.. index:: single: Annex D; Interface Testing (D.34)
.. index:: single: Ada language; Support for Interface Testing (D.34)

Interface Testing (D.34)
------------------------

Ada allows extending the expressiveness of an interface specification
at the code level, allowing constraints such as:

* parameter passing modes
* pre- and postconditions
* input partitioning
* typing

These are each described in other sections of this document.
These specifications can help the development of tests around the interface,
formalize constraints on how the interface is supposed to be used,
and activate additional dynamic checking or formal proofs (through SPARK),
all ensuring that users are indeed respecting the expectations of the
interface designer.

.. index:: single: GNATtest; Support for Interface Testing (D.34)

In addition, GNATtest can generate a testing framework to implement
interface testing, and GNATfuzz can help by probing the robustness
of the system when interface requirements are violated.

.. _Railway_SW_Language_Subset:

.. index:: single: Annex D; Language Subset (D.35)
.. index:: single: pragma Restrictions; Support for Language Subset (D.35)

Language Subset (D.35)
----------------------

The Ada language has been designed to facilitate subsetting, since its
targeted domain |mdash| long-lived safety-critical embedded systems |mdash|
often involves small-footprint applications that need to be certified
under demanding software standards. The full language would be inappropriate
with such constraints, and Ada provides a general feature |mdash| pragma
:ada:`Restrictions` |mdash| to allow subsetting on a user-selectable basis.
For example, with :ada:`pragma Restrictions (No_Abort_Statements)`
the program will be rejected by the compiler if it contains an :ada:`abort`
statement.

.. index:: single: Ravenscar Profile; Support for Language Subset (D.35)

Going one step further, the language standard has bundled a set of
restrictions into a so-called profile |mdash| the Ravenscar Profile |mdash|
that supports common concurrency idioms (e.g. periodic and sporadic tasks)
and can make a tasking program deterministic and statically analyzable.

.. index:: SPARK language; Support for Language Subset (D.35)

SPARK is a natural Ada language subset, constraining the language so that
programs can be subject to formal analysis (e.g., safe pointers,
no aliasing, and no exceptions).

.. index:: single: Certifiable profile; Support for Language Subset (D.35)
.. index:: single: GNATcheck; Support for Language Subset (D.35)

Other language subsets can be supplied by the implementation, such as
the features implemented by the GNAT Pro Certifiable Profiles.
And with GNATcheck the user can in effect define a subset in an *à la carte*
fashion, to specify prohibited constructs and verify that they are
not present in the code.

.. index:: single: Annex D; Metrics (D.37)
.. index:: single: GNATmetric; Support for Metrics (D.37)

Metrics (D.37)
--------------

The GNATmetric tool reports various metrics on the code, from simple
structural metrics such as lines of code or number of entities to more
complex computations such as cyclomatic complexity or coupling.

Custom metrics can be computed based on these first-level metrics.
In particular, the GNATdashboard environment allows gathering all metrics
into a database that can then accessed through Python or SQL.

These metrics can be viewed through various interfaces.

.. index:: single: Annex D; Modular Approach (D.38)

Modular Approach (D.38)
-----------------------

Connections between modules shall be limited and defined, coherence shall be strong
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: Ada language; Support for Modular Approach (D.38)
.. index:: Project (GNAT Pro); Support for Modular Approach (D.38)
.. index:: Child package; Support for Modular Approach (D.38)
.. index:: Private package; Support for Modular Approach (D.38)

Ada allows the developer to define group of packages that have different
levels of coupling, through the notions of *child packages* and
*private packages* as described below.
In addition, the GNAT Pro technology provides the notion of a *project*,
which defines a group of packages, possibly with a defined interface.
These constructs can be used to define a tool-supported notion of "component"
or "module" at the software level.

.. index:: Ada language; Coupling between modules

There are three main types of dependence between compilation units:

* Loose coupling through :ada:`with` clauses. If unit :ada:`Q`
  :ada:`with`\ s unit :ada:`P`, then :ada:`Q` can only access
  the entities in the visible part of :ada:`P`.

* Medium coupling through public child units.
  If :ada:`P` is a package,
  then child :ada:`P.Q` has visibility privileges that would
  not be available to a unit that only :ada:`with`\ s :ada:`P`.
  More specifically, :ada:`P.Q`, which is said to be a *public
  child*, can access the entities in the visible part of :ada:`P`.
  However, only the private part and body of :ada:`P.Q` can
  access the entities in the private part of :ada:`P`.
  And the entities declared in the body of :ada:`P` are
  only accessible in the package body itself.

* Tight coupling through private child units.
  As a generalization of public child units,
  if :ada:`P.Q` is declared as a *private* child, then
  the visible part of :ada:`P.Q` can also access the entities
  in the private part of :ada:`P`.
  This does not compromise encapsulation; the only units that
  can :ada:`with` a private child are units that otherwise would
  have access to the entities that the private child can see.

Ada's expressiveness makes it easier to develop large software systems, with
precise control over the coupling between modules, and guaranteeing
that data are only accessed by the intended units.

A typical example is the implementation of a complex system that needs to be
spread across several packages.
For example, suppose that packages :ada:`Communication` and :ada:`Interfaces`,
contribute to the implementation of a signaling protocol.
In Ada, this design can be implemented in three (or more) distinct files:

.. code-block:: ada

    package Signaling is ...
    private package Signaling.Communication is .
    private package Signaling.Interfaces is ...

The two private packages are defined in separate files. They are private
children of :ada:`Signaling`, which means they can only be used by the
implementation of :ada:`Signaling`, and not by any module outside of the
hierarchy.

.. index:: GNATmetric; Metrics on inter-package coupling

In addition, tools can provide metrics on coupling between packages.
GNATmetric has built-in support for retrieving these numbers.

.. index:: Project (GNAT Pro); GPR files

At a coarser granularity, packages can be grouped together into a
GNAT Project file ("GPR"), with a clear interface.
An application architecture can be defined as a combination of project files.

Collections of subprograms shall be built providing several level of modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Following the above example, it's possible to create public sub-modules
as well, creating a hierarchy of services.
Public child units are accessible to client code.

Subprograms shall have a single entry and single exit only
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: GNATcheck; Support for Modular Approach (D.38)

The GNATcheck tool has specific rules to verify this property
on any Ada code.

Modules shall communicate with other modules via their interface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: Ada language; Support for Modular Approach (D.38)

This is built-in to the Ada language. It's not possible to circumvent
a package's interface.
If a module is implemented using a coarser granularity, e.g. as a group of
packages or at project level, then the project file description allows
identifying those packages that are part of the interface and those packages
that are not.

Module interfaces shall be fully documented
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Although this is mostly the responsibility of the developer,
Ada contracts can be used to formalize part of the documentation
associated with a package interface, using a formal notation that can be
checked for consistency by the compiler.
This addresses the part of the documentation that can be
expressed through boolean properties based on the software-visible entities.

Interfaces shall contain the minimum number of parameters necessary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The GNAT Pro compiler will warn about parameters not used by a subprogram
implementation.

.. index:: GNATcheck; Support for Modular Approach (D.38)

A suitable restriction of parameter number shall be specified, typically 5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GNATcheck allows specifying a maximum number of parameters per subprogram.

Unit Proof and Unit Test
~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: GNATtest; Support for Modular Approach (D.38)

GNATtest can be used to generate a unit testing framework for Ada
applications.

.. index:: SPARK technology; Support for Modular Approach (D.38)

SPARK performs a modular formal verification: it proves the postcondition
of a subprogram according to its own precondition and the precondition
and postconditions of its callees, whether or not these callees are
themselves proven.

For a complete proof, all the subprograms of an application need
to be formally proven.
Where this is not possible, one subset can be proven
and the other can be assumed to be true.
These assumptions can then be verified using traditional testing methodology,
allowing for a hybrid test / proof verification system.

.. index:: Ada language; Support for Strongly Typed Programming Languages (D.49)

Strongly Typed Programming Languages (D.49)
-------------------------------------------

Ada is, from its inception, a strongly typed language, which supports
both static and dynamic verification.

From a static verification point of view, each type is associated with a
representation and a semantic interpretation. Two types with similar
representations but different semantics will still be considered different
by the compiler. For example:

.. code-block:: ada

    type Kilometers is new Float;
    type Miles is new Float:

These are distinct types.
the compiler will not allow mixed operations, for example assigning a
:ada:`Kilometers` value to a :ada:`Miles` variable, or adding
a :ada:`Kilometers` value and a :ada:`Miles` value, unless explicit
conversions are used.
Mixing floating point and integer values is similar: the developer is
responsible for deciding where and how conversion should be made.

From a dynamic verification point of view, types can be associated with
constraints, such as value ranges or arbitrary boolean predicates.
These type ranges and predicates will be verified at specific points in
the application, allowing the early detection of inconsistencies.

.. index:: single: Annex D; Structure Based Testing (D.50)

.. index:: GNATtest; Support for Structure Based Testing (D.50)
.. index:: GNATemulator; Support for Structure Based Testing (D.50)
.. index:: GNATcoverage; Support for Structure Based Testing (D.50)

Structure Based Testing (D.50)
------------------------------

AdaCore provides three tools to support structure based testing:

* GNATtest is a unit testing framework generator. It will run on Ada
  specifications, and generate a skeleton for each subprogram.
  The actual test can then be manually written into that skeleton.

* GNATemulator allows emulating code for a given target (e.g. PowerPC
  and Leon) on a host platform such as Windows or Linux.
  It's particularly well suited for running unit tests.

* GNATcoverage performs structural coverage analysis from an instrumented
  platform (GNATemulator or Valgrind on Linux or directly on a board through
  a Nexus probe).
  It supports statement coverage and decision coverage as well as MC/DC.
  Note that although |en-50128| requires compound condition coverage,
  Modified Condition/Decision Coverage (MC/DC) is usually accepted as
  a means of compliance.

.. index:: single: Annex D; Structured Programming (D.53)

.. index:: Ada language; Support for Structured Programming (D.53)

Structured Programming (D.53)
-----------------------------

The Ada language supports all the usual paradigms of structured programming.
Complexity can be controlled with various tools, see
:ref:`Railway_SW_Analyzable_Programs` for more details.

.. index:: single: Annex D; Suitable Programming Languages (D.54)
.. index:: Ada language; Support for Suitable Programming Languages (D.54)

Suitable Programming Languages (D.54)
-------------------------------------

Ada is noted as "Highly Recommended" in the list of programming
languages.
Some features may, however, not be suitable for the highest SIL.
To enforce the detection and rejection of specific features, the
developer can specify a language subset; see :ref:`Railway_SW_Language_Subset`.

One of the advantage of the Ada language is that it is precisely defined
in a international document, ISO/IEC 8652.
This document specifies the required effect as well as any
implementation-defined behavior for the core language, the standard Ada
libraries (known as the "predefined environment"), and the specialized needs
annexes.

.. index:: single: Annex D; Object Oriented Programming (D.57)

.. index:: Ada language; Support for Object Oriented Programming Languages (D.57)

Object Oriented Programming (D.57)
----------------------------------

Ada supports the usual constructs for object-oriented programming, but, for
reasons of simplicity and reliability, with multiple inheritance limited to
:ada:`interface` types.
In addition, the :index:`Liskov Substitution Principle` can be verified
through class-wide contracts and SPARK formal verification, allowing the
verification of class hierarchy consistency and the safety of dispatching
operations.

Ada's OOP model is particularly well suited to safety-critical applications,
as it allows instantiating objects on the stack. For example:

.. code-block:: ada

    type Base_Class is tagged ...; -- Base_Class is the root of a class hierarchy
    procedure P (X : Base_Class);
    ...
    type Subclass is new Base_Class with ...;
    overriding procedure P (X : Subclass);

    B : Base_Class := ... -- on the stack
    S : Subclass   := ... -- on the stack

    X : Some_Type'Class := (if .. then B else S):
    P (X);  -- Dispatches to appropriate version of P

In the above code, :ada:`X` is a polymorphic object that can be initialized
with a value from any class in the hierarchy rooted at :ada:`Base_Class`;
here it will be a value from either :ada:`Base_Class` or :ada:`Subclass`.
Storage for :ada:`X` is reserved on the stack, and the invocation :ada:`P (X)`
will dispatch to the appropriate version of :ada:`P`.

The booklet :cite:`Railway_SW_AdaCore_2016` provides additional information on how to use
object-oriented features in a certified context.

.. index:: single: Annex D; Procedural Programming (D.60)

.. index:: Ada language; Support for Procedural Programming (D.60)

Procedural Programming (D.60)
-----------------------------

Ada implements all the usual features of procedural programming languages,
with a general-purpose data type facility and a comprehensive set of control
constructs.
