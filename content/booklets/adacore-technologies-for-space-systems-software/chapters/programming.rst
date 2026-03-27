.. include:: ../../../global.txt

.. _Space_Systems_SW_Programming_Languages_for_Space_Software:

Programming Languages for Space Software
========================================

This chapter explains how space software developers can benefit from
the Ada language and its formally analyzable SPARK subset
(:footcite:p:`Space_SW_AdaCore_Capgemini_Web`,
:footcite:p:`Space_SW_McCormick_Chapin_2015`).  Unless explicitly
stated otherwise, the Ada discussion applies to the |ada-2012| version
of the language standard with Technical Corrigendum\ |nbsp|\ 1
(:footcite:p:`Space_SW_ISO_IEC_2016`,
:footcite:p:`Space_SW_Barnes_2014`,
:footcite:p:`Space_SW_Barnes_Brosgol_2015`), and the SPARK language
discussion applies to the 2014 version of the language as amended by
:footcite:p:`Space_SW_AdaCore_Capgemini_Web`.  Both Ada and SPARK
support the development and verification of software at the highest
criticality class levels.

.. index:: single: Ada language; Summary

Ada
---

The choice of programming language(s) is one of the fundamental decisions
during software design. The source code is the artifact that is developed,
verified, and maintained, and it is also the subject of much of the analysis /
inspection required for certification / qualification against domain-specific
standards.  Although in principle almost any programming language can be used
for software development, in practice the life-cycle costs for the
high-assurance real-time software found in space systems are reduced when the
chosen language has been explicitly designed for reliability, safety,
security, and ease of maintenance of large, long-lived systems.

.. index:: Memory safety

Ada helps meet high-assurance requirements through its support for sound
software engineering principles, compile-time checks that guarantee type
safety, and run-time checks for constraints such as array index bounds and
scalar ranges. As will be explained below, the SPARK subset of Ada shares
these benefits and adds an important advantage: the dynamic constraints are
enforced through mathematics-based static analysis. This avoids run-time
overhead for checks in production code while eliminating the risk that such
a check could fail. With their standard features for preventing buffer
overruns, pointer misuses and other vulnerabilities, Ada and SPARK both
serve as memory-safe languages.


Ada language overview
~~~~~~~~~~~~~~~~~~~~~

Ada is multi-faceted. From one perspective it is a classical stack-based
general-purpose language (unlike languages like Java, it does not require
garbage collection), and it is not tied to any specific development
methodology. It offers:

    * a simple syntax designed for human readability;
    * structured control statements;
    * flexible data composition facilities;
    * strong type checking;
    * traditional features for code modularization (“sub­programs”);
    * standard support for "programming in the large" and module reuse,
      including packages, Object-Oriented Programming, hierarchical
      package namespace ("child libraries"), and generic templates;
    * a mechanism for detecting and responding to exceptional run-time
      conditions ("exception handling"); and
    * high-level concurrency support ("tasking") along with a deterministic
      subset (the Ravenscar profile) appropriate in applications that need
      to meet high-assurance certification / qualification requirements
      and/or small footprint constraints.

The language standard also includes:

    * an extensive predefined environment with support for I/O, string
      handling, math functions, containers, and more;
    * a standard mechanism for interfacing with other programming languages
      (such as C and C++); and
    * specialized needs annexes for functionality in several domains (Systems
      Programming, Real-Time Systems, Distributed Systems, Numerics,
      Information Systems, and High-Integrity Systems).

Source code portability was a key goal for Ada. The challenge for a
programming language is to define the semantics in a platform-independent
manner but not sacrifice run-time efficiency. Ada achieves this in several
ways.

    * Ada provides a high-level model for concurrency (tasking), memory
      management, and exception handling, with standard semantics across
      all platforms. The language's dynamic semantics can be mapped to the
      most efficient services provided by the target environment.
    * The developer can express the logical properties of a type (such as
      integer range, floating-point precision, and record fields/types)
      in a machine-independent fashion, which the compiler can then map
      to an efficient underlying representation.
    * The physical representation of data structures (layout, alignment,
      and addresses) is sometimes specified by system requirements. Ada
      allows this to be defined in the program logic but separated from
      target-independent properties for ease of maintenance.
    * Platform-specific characteristics such as machine word size are
      encapsulated in an API, so that references to these values are through
      a standard syntax. Likewise, Ada defines a standard type :ada:`Address`
      and associated operations, again facilitating the portability of
      low-level code.

.. index:: single: Ada language; Background

.. Ada_language_background:

Ada language background
~~~~~~~~~~~~~~~~~~~~~~~

.. index:: Ichbiah (Jean), CII-Honeywell-Bull

Ada was designed for large, long-lived applications |mdash| and embedded
systems in particular |mdash| where reliability, maintainability, and
efficiency are essential. Under sponsorship of the U.S. Department of Defense,
the language was originally developed in the early 1980s (this version is
generally known as |ada-83|) by a team led by Jean Ichbiah at
CII-Honeywell-Bull in France.

.. index:: Taft (Tucker), Intermetrics

Ada was revised and enhanced in an upward-compatible fashion in the early
1990s, under the leadership of Tucker Taft from Intermetrics in the U.S.
The resulting language, |ada-95|, was the first internationally standardized
object-oriented language.

Under the auspices of the International Organization for Standardization
(ISO), a further (minor) revision was completed as an amendment to the
standard; this version of the language is known as |ada-2005|.

Additional features (including support for contract-based programming
in the form of subprogram pre- and postconditions and type invariants)
were added in |ada-2012|, and other enhancements to increase the
language's expressiveness were introduced in |ada-2022|
:footcite:p:`Space_SW_ISO_IEC_2022`.

.. index:: Lovelace (Augusta Ada), Babbage (Charles), Byron (Lord George)

The name "Ada" is not an acronym; it was chosen in honor of Augusta Ada
Lovelace (1815-1852), a mathematician who is sometimes regarded as the
world's first programmer because of her work with Charles Babbage.
She was also the daughter of the poet Lord Byron.

The Ada language has long been a language of choice
in high-assurance / safety-critical / high-security domains, where software
failures can have catastrophic consequences.
Ada has a proven track record in space applications,
military and commercial aircraft avionics, air traffic control, and railroad
software, and it has also seen successful usage in other domains (such as
automotive systems and medical devices).

.. index:: Ravenscar profile

With its embodiment of modern software engineering principles, Ada is
especially appropriate for teaching in both introductory and advanced
computer science courses, and it has also been the subject of
significant research, especially in the area of real-time
technologies.  Throughout much of Ada's history, a worldwide group of
experts from industry, academia, and government convened a biannual
meeting |mdash| the *International Real-Time Ada Workshop* ("IRTAW")
|mdash| to focus on Ada's support for real-time software.  The 1997
IRTAW in Ravenscar, UK, led directly to the definition of a subset of
Ada's concurrency features with deterministic semantics |mdash| the
so-called *Ravenscar Profile* :footcite:p:`Space_SW_Burns_et_al_2003`.
This work broke new ground in supporting the use of concurrent
programming in high-assurance software.

AdaCore has a long history and close connection with the Ada programming
language. Company members worked on the original Ada 83 design and review
and played key roles in the Ada 95 project as well as the subsequent
revisions. The initial GNAT compiler was delivered at the time of the |ada-95|
language's standardization, thus guaranteeing that users would have a quality
implementation for transitioning to |ada-95| from |ada-83| or other languages.

The following subsections provide additional detail on Ada language features.

.. index:: single: Ada language; Scalar ranges

Scalar ranges
~~~~~~~~~~~~~

Unlike languages based on C (such as C++, Java, and C#), Ada allows the
programmer to simply and explicitly specify the range of values that are
permitted for variables of scalar types (integer, floating-point, fixed-point,
and enumeration types). The attempted assignment of an out-of-range value
raises an exception, reflecting a run-time error.

The ability to specify range constraints makes the programmer's intent
explicit and makes it easier to detect a major source of coding and user
input errors. It also provides useful information to static analysis tools
and facilitates automated proofs of program properties.

Here's an example of an integer scalar range and an associated run-time check:

.. code-block:: ada

    declare
       My_Score : Integer range 1..100;
       N        : Integer;
    begin
       ...  -- Code that assigns an integer value to N
       My_Score := N;
       -- A run-time check verifies that N is within the range 1 through 100, inclusive
       -- If this check fails, the Constraint_Error exception is raised
    end;

The run-time check can be optimized out if the compiler can guarantee that
the value of :ada:`N` is within the range 1 through 100 when control reaches
the assignment to :ada:`My_Score`.


.. index:: single: Ada language; Contract-based programming
.. index:: single: Ada language; Preconditions
.. index:: single: Ada language; Postconditions


Contract-based programming
~~~~~~~~~~~~~~~~~~~~~~~~~~

A feature introduced in Ada 2012 allows extending a subprogram specification
or a type/subtype declaration with a contract (a Boolean assertion).
Subprogram contracts take the form of preconditions and postconditions;
type contracts are used for invariants; and subtype contracts provide
generalized constraints (predicates). Through contracts the developer can
formalize the intended behavior of the application, and can
verify this behavior by testing, static analysis or formal proof.

Here's a skeletal example that illustrates contact-based programming; a
:ada:`Table` object is a fixed-length container for distinct :ada:`Float`
values.

.. code-block:: ada

    package Table_Pkg is
       type Table is private;  -- Encapsulated type

       function Is_Full  (T    : in Table) return Boolean;

       function Contains (T    : in Table;
                          Item : in Float) return Boolean;

       procedure Insert (T : in out Table; Item: in Float)
         with Pre  => not Is_Full(T) and
                      not Contains(T, Item),
              Post => Contains(T, Item);

       procedure Remove (T : in out Table; Item: in Float);
         with Pre  => Contains(T, Item),
              Post => not Contains(T, Item);
       ...
    private
       ... -- Full declaration of Table
    end Table_Pkg;

    package body Table_Pkg is
       ... -- Implementation of Is_Full, Contains, Insert, Remove
    end Table_Pkg;

A compiler option controls whether the pre- and postconditions are checked
at run time. If checks are enabled, any pre- or postcondition failure |mdash|
i.e., the contact's Boolean expression evaluating to :ada:`False` |mdash|
raises the :ada:`Assertion_Error` exception.

.. index:: single: Ada language; Type invariants
.. index:: single: Ada language; Type/subtype predicates

Ada's type invariants and type / subtype predicates specify
precisely what is and is not valid for any particular (sub)type, including
composite types such as records and arrays. For example, the code fragment
below specifies that field :ada:`Max_Angle` in the
:ada:`Launching_Pad` structure below is the maximal angle allowed, given the
distance :ada:`D` to the center of the launching pad and the height :ada:`H`
of the rocket. The compiler will insert the necessary run-time checks when a
:ada:`Launching_Pad` object is created, to verify this predicate as well as
the constraints on the individual fields:

.. code-block:: ada

    subtype Meter  is Float range 0.0 .. 200.0;
    subtype Radian is Float range 0.0 .. 2.0 * Pi;

    type Launching_Pad is
       record
          D, H      : Meter;
          Max_Angle : Radian;
       end record
    with
       Predicate => Arctan (H, D) <= Max_Angle;

Further information about type invariants and type / subtype predicates
may be found in the *Design by contracts* chapter of the *Introduction to Ada*
course of :footcite:p:`Space_SW_AdaCore_AdaLearn`.

.. index:: single: Ada language; Programming in the large

Programming in the large
~~~~~~~~~~~~~~~~~~~~~~~~

The original |ada-83| design introduced the package construct, a feature
that supports encapsulation ("information hiding") and modularization, and
which allows the developer to control the namespace that is accessible
within a given compilation unit. |ada-95| introduced the concept of
"child units", which provides a hierarchical and extensible namespace
for library units and thus eases the design and maintenance of very large
systems.

|ada-2005| extended the language's modularization facilities by allowing
mutual references between package specifications, thus making it easier
to interface with languages such as Java.

.. index:: single: Ada language; Generic templates

Generic templates
~~~~~~~~~~~~~~~~~

A key to reusable components is a mechanism for
parameterizing modules with respect to data types and other program entities.
A typical example is a data structure (e.g., a stack, FIFO queue, or
varying-length array) for an arbitrary element type :ada:`T`, where each element
of the data structure is of type :ada:`T`.
For safety and efficiency, compile-time checks should enforce type safety
both within the parameterized module, and at each use ("instantiation").
Conceptually an
instantiation can be regarded as an expansion of the parameterized module,
with actual parameters replacing the formal parameters. However, the expansion
is not at the lexical/syntactic level (source text) but rather at the semantic
level (scope-resolved names).
In addition, an implementation may be able to share a single copy of the code
across multiple instantiations and thereby save code space.

.. index:: Stroustrup (Bjarne)

Ada supplies this functionality through a facility known as
*generics*, with type consistency enforced by the compiler both within
the generic unit and at each instantiation. Ada generics are analogous
to C++ templates but with more extensive compile-time checking: a data
object within a generic unit can only be processed using operations
that are known to be available for the object's type. (As a historical
note, at the ACM SIGPLAN Conference on History of Programming
Languages in 1993, the C++ designer Bjarne Stroustrup acknowledged the
Ada design for generics as one of the inspirations for templates in
C++ :footcite:p:`Space_SW_ACM_1993`.)

.. index:: single: Ada language; Object-Oriented Programming (OOP)

Object-Oriented Programming (OOP)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

|ada-83| was object-based, allowing the partitioning of a system into modules
(packages) corresponding to abstract data types or abstract objects.
Full OOP support was not provided since, first, it seemed not to be required
in the real-time domain that was Ada's primary target, and second, the
apparent need for automatic garbage collection in an Object Oriented language
would have interfered with predictable and efficient performance.

However, large real-time systems often have components such as graphical user
interfaces (GUIs) that do not have hard real-time constraints and that can be
most effectively developed using OOP features. In part for this reason,
|ada-95| added comprehensive support for OOP, through its "tagged type"
facility: classes, polymorphism, inheritance, and dynamic binding.
These features do not require automatic garbage collection; instead,
definitional features introduced by |ada-95| allow the developer to supply
type-specific storage reclamation operations ("finalization").

|ada-2005| brought additional OOP features, including Java-like interfaces
and traditional :ada:`X.P(...)` notation for invoking operation :ada:`P(...)`
on object :ada:`X`.

Ada is methodologically neutral and does not impose a distributed overhead
for OOP. If an application does not need OOP, then the OOP features do not
have to be used, and there is no run-time penalty.

See :footcite:p:`Space_SW_Barnes_2014` or
:footcite:p:`Space_SW_AdaCore_2016` for more details.

.. index:: single: Ada language; Concurrent programming (tasking)

Concurrent programming
~~~~~~~~~~~~~~~~~~~~~~

Ada supplies a structured, high-level facility for concurrency. The unit of
concurrency is a program entity known as a "task". Tasks can communicate
implicitly via shared data or explicitly via a synchronous control
mechanism known as the rendezvous. A shared data item can be defined
abstractly as a "protected object" (a feature introduced in |ada-95|),
with operations executed under mutual exclusion when invoked from multiple
tasks. Protected objects provide the functionality of semaphores and
condition variables but more clearly and reliably (e.g., avoiding subtle
race conditions).

Ada supports asynchronous task interactions for timeouts, software event
notifications, and task termination. Such asynchronous behavior is deferred
during certain operations, to prevent the possibility of leaving shared data
in an inconsistent state. Mechanisms designed to help take advantage
of multicore architectures were introduced in |ada-2012|.

.. index:: single: Ada language; Systems programming

Systems programming
~~~~~~~~~~~~~~~~~~~

Both in the core language and the Systems Programming Annex, Ada supplies
the necessary features for low-level / hardware-specific processing.
For example, the programmer can specify the bit layout for fields in a
record, define alignment and size properties, place data at specific machine
addresses, and express specialized code sequences in assembly language.
Interrupt handlers can also be written in Ada, using the protected object
facility.

.. index:: single: Ada language; Real-time programming

Real-time programming
~~~~~~~~~~~~~~~~~~~~~

Ada's tasking facility and the Real-Time Systems Annex support common idioms
such as periodic or event-driven tasks, with features that can help avoid
unbounded priority inversions. A protected object locking policy is defined
that uses priority ceilings; this has an especially efficient implementation
in Ada (mutexes are not required) since protected operations are not allowed
to block. |ada-95| defined a standard task dispatching policy in which a task
runs until blocked or preempted, and |ada-2005| introduced several others
including Earliest Deadline First.

.. index:: single: Ada language; Time and space analysis

Time and Space Analysis
~~~~~~~~~~~~~~~~~~~~~~~

.. rubric:: Timing verification

Suitably subsetted, Ada (and SPARK) are amenable to the static analysis of
timing behavior. This kind of analysis is relevant for real-time systems,
where worst-case execution time (WCET) must be known in order to guarantee
that timing deadlines will always be met. Timing analysis is also of interest
for secure systems, where the issue might be to show that programs do not leak
information via so-called side-channels based on the observation of
differences in execution time.

AdaCore does not produce its own WCET tool, but there are several such tools
on the market from partner companies, such as RapiTime from
Rapita Systems Ltd.

.. rubric:: Memory usage verification

Ada and SPARK can support the static analysis of worst-case memory
consumption, so that a developer can show that a program will never
run out of memory at execution time.

In both SPARK and Ada, users can specify pragma :ada:`Restrictions` with the
standard arguments :ada:`No_Allocators` and
:ada:`No_Implicit_Heap_Allocations`. This will completely prevent heap usage,
thus reducing memory usage analysis to a worst-case computation of stack usage
for each task in a system. Stack size analysis is implemented directly in
AdaCore's GNATstack tool, as described in :ref:`Space_Systems_SW_GNATstack`.

.. index:: single: Ada language; High-integrity systems
.. index:: single: Ada language; Memory safety
.. index:: Memory safety

High-integrity systems
~~~~~~~~~~~~~~~~~~~~~~

With its emphasis on sound software engineering principles, Ada
supports the development of safety-critical and other high-integrity
applications, including those that need to be certified/qualified
against software standards such as |E-ST-40C| and |Q-ST-80C| for space
systems, |do-178c| for avionics
:footcite:p:`Space_SW_RTCA_EUROCAE_2011a`, and CENELEC |en-50128| for
rail systems :footcite:p:`Space_SW_CENELEC_2020b`. Similarly, Ada (and
its SPARK subset) can help developers produce high Evaluation
Assurance Level (EAL) code that meets security standards such as the
Common Criteria :footcite:p:`Space_SW_CCDB_2022`.  Key to Ada's
support for high-assurance software is the language's memory safety;
this is illustrated by a number of features, including:

* Strong typing

  Data intended for one purpose will only be accessed via operations that are
  legal for that data item's type, so errors such as treating pointers as
  integers (or vice versa) are prevented. (Low-level code sometimes needs to
  defeat the language's type checking; e.g., by treating "raw bits" as a
  value of a specific type. As a language that supports systems programming,
  Ada allows such usages but requires explicit syntax that makes the intent
  clear.)

* Array bounds checking

  A run-time check guarantees that an array index is within the bounds of
  the array. This prevents buffer overrun vulnerabilities that
  are common in C and C++. In many cases a compiler optimization can
  detect statically that the index is within bounds and thus eliminate
  any run-time code for the check.

* Prevention of null pointer dereferences

  As with array bounds, pointer dereferences are checked to make sure that
  the pointer is not null. Again, such checks can often be optimized out.

* Prevention of dangling references

  A scope accessibility check ensures that a pointer (known in Ada as an
  *access value*) cannot reference an object on the stack after exit/return
  from the scope (block or subprogram) in which the object is declared.
  Such checks prevent dangling references to stack objects (i.e., subprogram
  parameters and local variables) and are generally static, with no run-time
  overhead.
  (Since Ada does not require garbage collection
  of inaccessible dynamically allocated objects, it provides an
  :ada:`Unchecked_Deallocation` facility for programmer-supplied
  deallocation. Uses of this feature require explicit syntax |mdash|
  a :ada:`with` clause at the beginning of the compilation unit.
  This makes clear that special attention needs to be paid to verification,
  since erroneous deallocations can lead to dangling references.)

The evolution of Ada has seen a continued increase in support for
safety-critical and high-security applications. |ada-2005|
standardized the Ravenscar Profile
:footcite:p:`Space_SW_Burns_et_al_2003`, a collection of concurrency
features that are powerful enough for real-time programming but simple
enough to make safety certification practical. |ada-2012| introduced
contract-based programming facilities, allowing the programmer to
specify preconditions and/or postconditions for subprograms, and
invariants for encapsulated (private) types. These can serve both for
run-time checking and as input to static analysis tools.

The most recent version of the standard, Ada 2022, has added several
contract-based programming constructs inspired by SPARK
(:ada:`Contract_Cases`, :ada:`Global`, and :ada:`Depends` aspects)
and, more generally, has enhanced the language's expressiveness.
For example, Ada 2022 has introduced some new syntax in its
concurrency support and has defined the Jorvik tasking profile,
which is more inclusive than Ravenscar.

.. index:: Coding standard enforcement

Enforcing a coding standard
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada is a large language, suitable for general-purpose programming, but
the full language may be inappropriate in a safety- or security-critical
application. The generality and flexibility of some features |mdash|
especially those with complex run-time semantics |mdash| complicate
analysis and could interfere with traceability / certification
requirements or impose too large a memory footprint. A project will
then need to define and enforce a coding standard that prohibits
problematic features. Several techniques are available:

* pragma :ada:`Restrictions`

  This standard Ada pragma allows the user to specify constructs
  that the compiler will reject. Sample restrictions include dependence
  on particular packages or language facilities, or usage
  of features requiring unwanted implementation techniques (e.g. run-time
  support that is overly complex and difficult to certify).

* pragma :ada:`Profile`

  This standard Ada pragma provides a common name for a collection
  of related :ada:`Restrictions` pragmas. The predefined pragma
  :ada:`Profile(Ravenscar)` is a shorthand for the various restrictions
  that comprise the Ravenscar tasking subset.

.. index:: GNATcheck
.. index: GNAT Static Analysis Suite; GNATcheck

* Static analysis tool (coding standard enforcer)

  Other restrictions on Ada features can be detected by AdaCore's
  automated GNATcheck tool (see :ref:`Space_Systems_SW_GNATcheck`)
  that is included with the GNAT Pro Ada Static Analysis Suite. The
  developer can configure this rule-based and tailorable tool to flag
  violations of the project's coding standard, such as usage of
  specific prohibited types or subprograms defined in
  otherwise-permitted packages.

.. index:: single: Ada language; ECSS standards support

.. _Space_Systems_SW_Ada_and_the_ECSS_Standards:

Ada and the ECSS Standards
~~~~~~~~~~~~~~~~~~~~~~~~~~

|E-ST-40C| covers software engineering practice, and |Q-ST-80C| covers
software product assurance, and both of these areas are "sweet spots"
that match Ada's strengths. Please see
:ref:`Space_Systems_SW_Compliance_with_ECSS-E-ST-40C` and
:ref:`Space_Systems_SW_Compliance_with_ECSS-Q-ST-80C` to learn how
specific clauses in these standards relate to individual Ada features.
In summary, the use of Ada can help the software supplier meet the
requirements in the following sections:

* |E-ST-40C|

  * |sect|\ 5.4 Software requirements and architecture engineering process

    * |sect|\ 5.4.3 Software architecture design

  * |sect|\ 5.5 Software design and implementation engineering process

    * |sect|\ 5.5.2 Design of software items

  * |sect|\ 5.8 Software verification process

    * §5.8.3 Verification activities

  * |sect|\ 5.9 Software operation process

    * §5.9.4 Software operation support

  * |sect|\ 5.10 Software maintenance process

    * |sect|\ 5.10.4 Modification implementation

  * |sect|\ 5.11 Software security process

    * |sect| 5.11.2 Process implementation

    * |sect| 5.11.3 Software security analysis

    * |sect| 5.11.5 Security activities in the software life cycle

  * Annex U - Software code verification

* |Q-ST-80C|

  * |sect|\ 6.2 Requirements applicable to all software engineering processes

    * |sect|\ 6.2.3 Handling of critical software

    * |sect|\ 6.2.9 Software security

    * |sect|\ 6.2.10 Handling of security sensitive software

  * |sect|\ 6.3 Requirements applicable to individual software engineering
    processes or activities

    * |sect|\ 6.3.4 Coding

  * |sect|\ 7.2 Product quality requirements

    * |sect|\ 7.2.2 Design and related documentation

In brief, Ada is an internationally standardized language combining
object-oriented programming features, well-engineered concurrency facilities,
real-time predictability and performance, and built-in reliability through
both compile-time and run-time checks. With its support for producing
software that is correct, maintainable, and efficient, the language is
especially well suited for writing space applications.

SPARK
-----

.. index:: single: SPARK language; Summary

SPARK Basics
~~~~~~~~~~~~

SPARK (:footcite:p:`Space_SW_AdaCore_Capgemini_Web`,
:footcite:p:`Space_SW_McCormick_Chapin_2015`) is a software
development technology (programming language and verification toolset)
specifically oriented around applications demanding an ultra-low
defect level, for example where safety and/or security are key
requirements. SPARK Pro is the commercial-grade offering of the SPARK
technology developed by AdaCore, Capgemini Engineering (formerly
Altran), and Inria. As will be described in
:ref:`Space_Systems_SW_Static_Verification_SPARK_Pro`, the main
component in the toolset is GNATprove, which performs formal
verification on SPARK code.

SPARK has an extensive industrial track record. Since its inception in the
late 1980s it has been used worldwide in a wide variety of applications,
including civil and military avionics, space satellite control, air traffic
management / control, railway signaling, cryptographic software, medical
devices, automotive systems, and cross-domain solutions. SPARK 2014 is the
most recent version of the technology.

The SPARK language has been stable over the years, with periodic enhancements.
The 2014 version of SPARK represented a major revision, incorporating
contract-based programming syntax from Ada 2012, and subsequent upgrades
included support for pointers (access types) based on the Rust ownership
model.

.. index:: single: SPARK language; Memory safety
.. index:: Memory safety

The SPARK language is a large subset of Ada 2012 and is memory safe.
It includes as much of the Ada language as is possible / practical to
analyze formally, while eliminating sources of undefined and
implementation-dependent behavior.
SPARK includes Ada's program structure support (packages, generics, child
libraries), most data types, safe pointers, contract-based programming
(subprogram pre- and postconditions, scalar ranges, type/subtype predicates),
Object-Oriented Programming, and the Ravenscar subset of the tasking features.

Principal exclusions are side effects in functions and expressions,
problematic aliasing of names, exception handling, pointer functionality
that may be unsafe, and most tasking features.

.. index:: single: SPARK language; Soundness

A legal SPARK program has unambiguous semantics: its effect is precisely
defined and does not depend on the implementation. This property helps
ensure the soundness of static verification; i.e., the
absence of "false negatives". If the SPARK tools report that a program does
not have a specific vulnerability, such as a reference to an uninitialized
variable, then that conclusion can be trusted with mathematical certainty.
Soundness builds confidence in the tools, provides evidence-based assurance,
completely removes many classes of dangerous defects, and significantly
simplifies subsequent verification effort (e.g., testing), owing to less
rework. Moreover, the SPARK tools achieve soundness while keeping
"false positives" manageable.

SPARK offers the flexibility of configuring the language on a per-project
basis. Restrictions can be fine-tuned based on the relevant coding standards
or run-time environments.

SPARK code can easily be combined with full Ada code or with C, so that new
systems can be built on and reuse legacy codebases. Moreover, the same code
base can have some sections in SPARK and others excluded from SPARK analysis
(SPARK and non-SPARK code can also be mixed in the same package or
subprogram).

.. index:: single: SPARK language; Formal methods

Software verification typically involves extensive testing, including unit
tests and integration tests. Traditional testing methodologies are a major
contributor to the high delivery costs for safety-critical software.
Furthermore, testing can never be complete and thus may fail to detect errors.
SPARK addresses this issue by using automated proof (embodying
mathematics-based formal methods) to demonstrate program
integrity properties up to functional correctness at the subprogram level,
either in combination with or as a replacement for unit testing. In the
high proportion of cases where proofs can be discharged automatically,
the cost of writing unit tests may be completely avoided. Moreover,
verification by proofs covers all execution conditions and not just a sample.

Here is an example of SPARK code:

.. code-block:: ada

    package Example is
       N : Positive := 100; -- N constrained to 1 .. Integer'Last

       procedure Decrement (X in out Integer)
          with Global => (Input => N),
               Depends => (X => (X, N)),
               Pre     => X >= Integer'First + N,
               Post    => X = X'Old - N;
    end Example;

    package body Example is
       procedure Decrement (X in out Integer) is
       begin
          X := X - N;
       end Decrement;
    end Example;

The :ada:`with` constructs, known as "aspects", here define the
:ada:`Decrement` procedure's contracts:

* :ada:`Global`: the only access to non-local data is to read the value of
  :ada:`N`

* :ada:`Depends`: the value of :ada:`X` on return depends only on :ada:`N`
  and the value of :ada:`X` on entry

* :ada:`Pre`: a Boolean condition that the procedure assumes on entry

* :ada:`Post`: a Boolean condition that the subprogram guarantees on return

In this example the SPARK tool can verify the :ada:`Global` and :ada:`Depends`
contracts and can also prove several dynamic properties: no run-time errors
will occur during execution of the :ada:`Decrement` procedure, and, if the
:ada:`Pre` contract is met when the procedure is invoked then the :ada:`Post`
contract will be satisfied on return.

SPARK (and the SPARK proof tools) work with Ada 2012 syntax, but a SPARK
program can also be expressed in Ada 95, with contracts captured as pragmas.

Ease of Adoption
~~~~~~~~~~~~~~~~

User experience has shown that the language and the SPARK Pro toolset
do not require a steep learning curve. Training material such as
AdaCore's online AdaLearn course for SPARK
:footcite:p:`Space_SW_AdaCore_AdaLearn` can quickly bring developers
up to speed; users are assumed to be experts in their own application
domain such as space technology and do not need to be familiar with
formal methods or the proof approaches implemented by the toolset.  In
effect, SPARK Pro is an advanced static analysis tool that will detect
many logic errors and security vulnerabilities very early in the
software life cycle. It can be smoothly integrated into an
organization's existing development and verification methodology and
infrastructure.

SPARK uses the standard Ada 2012 contract syntax, which both simplifies the
learning process and also allows new paradigms of software verification.
Programmers familiar with writing executable contracts for run-time assertion
checking can use the same approach but with additional flexibility: the
contracts can be verified either dynamically through classical run-time
testing methods or statically (i.e., pre-compilation and pre-test) using
automated tools.

.. index:: single: SPARK language; Levels of adoption

.. _Space_Systems_SW_Levels_of_Adoption_of_Formal_Methods:

Levels of Adoption of Formal Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Formal methods are not an "all or nothing" technique. It is possible and
in fact advisable for an organization to introduce the methodology in a
stepwise manner, with the ultimate level depending on the assurance
requirements for the software. This approach is documented in
:footcite:p:`Space_SW_AdaCore_Thales_2020`,
which details the levels of adoption, including the benefits and costs at
each level, based on the practical experience of a major aerospace company
in adopting formal methods incrementally; the development team did not have
previous knowledge of formal methods. The levels are additive; all the
checks at one level are also performed at the next higher level.

Stone level: Valid SPARK
^^^^^^^^^^^^^^^^^^^^^^^^

As the first step, a project can implement as much of the code as is possible
in the SPARK subset, run the SPARK analyzer on the codebase (or new code),
and look at violations. For each violation, the developer can decide whether
to convert the code to valid SPARK or exclude it from analysis. The benefits
include easier maintenance for the SPARK modules (no aliasing, no side effects
in functions) and project experience with the basic usage of formal methods.
The costs include the effort that may be required to convert the code to SPARK
(especially if there is heavy use of pointers).

Bronze level: Initialization and correct data flow
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This level entails performing flow analysis on the SPARK code to verify
intended data usage. The benefits include assurance of no reads of
uninitialized variables, no interference between parameters and global
objects, no unintended access to global variables, and no race conditions
on accesses to shared data. The costs include a conservative analysis of
arrays (since indices may be computed at run time) and potential
"false alarms" that need to be inspected.

.. index:: single: SPARK language; Absence of run-time errors (AORTE)

Silver level: Absence of run-time errors (AORTE)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At the Silver level, the SPARK proof tool performs flow analysis, locates all
potential run-time checks (e.g., array indexing), and then attempts to prove
that none will fail. If the proof succeeds, this brings all the benefits of
the Bronze level plus the ability to safely compile the final executable
without exception checks. Critical software should aim for at least this
level. The cost is the additional effort needed to obtain provability.
In some cases (if the programmer knows that an unprovable check will always
succeed, for example because of hardware properties) it may be necessary to
augment the code with pragmas to help the prover.

Gold level: Proof of key integrity properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At the Gold level, the proof tool will verify properties such as correctness
of critical data invariants or safe transitions between program states.
Subprogram pre- and postconditions and subtype predicates are especially
useful here, as is "ghost" code that serves only for verification and is not
part of the executable. A benefit is that the proofs can be used for safety
case rationale, to replace certain kinds of testing. The cost is increased
time for tool execution, and the possibility that some properties may be
beyond the abilities of current provers.

Platinum level: Full functional correctness
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At the Platinum level, the algorithmic code is proved to satisfy its formally
specified functional requirements. This is still a challenge in practice for
realistic programs but may be appropriate for small critical modules,
especially for security-critical systems at high Evaluation Assurance Levels
where formal methods can provide the needed confidence.

.. index:: single: SPARK language; Hybrid verification

.. _Space_Systems_SW_Hybrid_Verification:

Hybrid Verification
~~~~~~~~~~~~~~~~~~~

Hybrid verification combines formal methods with traditional testing.
A typical scenario is an in-progress project that is based on testing but
which has high-assurance requirements that can best be demonstrated
through formal methods. The new code will be in SPARK; and the adoption
level depends on the experience of the project team (typically Stone at
the start, then progressing to Bronze or Silver). The existing codebase
may be in Ada or other languages. To maximize the precision of the SPARK
analysis, the subprograms that the SPARK code will be invoking should
have relevant pre- and postconditions expressing the subprograms' low-level
requirements. If the non-SPARK code is not in Ada, then the pre- and
postconditions should be included on the Ada subprogram specification
corresponding to the imported function; here is an example.

.. code-block:: ada

    function getascii return Interfaces.C.unsigned_char
       with Post => getascii'Result in 0..127;

    pragma Import (C, getascii);
    -- Interfaces.C.unsigned_char is a modular (unsigned)
    -- integer type, typically ranging from 0 through 255

    procedure Example is
       N : Interfaces.C.unsigned_char range 0 .. 127;
    begin
       N := getascii;
       -- SPARK can prove that no range check is needed
    end Example;

The verification activity depends on whether the formally verified code
invokes the tested code or vice versa.

* The SPARK code calls a tested subprogram

  If the tested subprogram has a precondition, the SPARK code is checked
  at each call site to see if the precondition is met. Any call that the
  proof tool cannot verify for compliance with the precondition needs to be
  inspected to see why the precondition cannot be proved. It could be a
  problem with the precondition, a problem at the call site, or a limitation
  of the prover.

  The postcondition of the called subprogram can be assumed to be valid at
  the point following the return, although the validity needs to be
  established by testing. In the example above, testing would
  need to establish that the :ada:`getascii` function only returns a result
  in the range 0 through 127.

* The SPARK code is invoked from tested code

  Testing would need to establish that, at each call, the precondition of the
  SPARK subprogram is met. Since the SPARK subprogram has been formally
  verified, at the point of return the subprogram’s postcondition is known
  to be satisfied. Testing of the non-SPARK code can take advantage of this
  fact, thereby reducing the testing effort.

Hybrid verification can be performed within a single module; e.g., a package
can specify different sections where SPARK analysis is or is not to be
performed.

.. index:: single: SPARK language; ECSS standards support

.. _Space_Systems_SW_SPARK_and_the_ECSS_Standards:

SPARK and the ECSS Standards
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In summary, the qualities that make Ada an appropriate choice for
space software also apply to SPARK (see
:ref:`Space_Systems_SW_Ada_and_the_ECSS_Standards`).  And specific to
SPARK, the static determination that the code is free from run-time
errors ("Silver" level of SPARK adoption) can significantly reduce the
effort in showing that the application is sufficiently safe and
secure.

The use of SPARK can help the software supplier meet the requirements in the
following sections of |E-ST-40C| and |Q-ST-80C|:

* |E-ST-40C|

  * |sect|\ 5.4 Software requirements and architecture engineering process

    * |sect|\ 5.4.3 Software architecture design

  * |sect|\ 5.5 Software design and implementation engineering process

    * |sect|\ 5.5.2 Design of software items

  * |sect|\ 5.8 Software verification process

    * §5.8.3 Verification activities

  * |sect|\ 5.10 Software maintenance process

    * |sect|\ 5.10.4 Modification implementation

  * |sect|\ 5.11 Software security process

    * |sect| 5.11.2 Process implementation

    * |sect| 5.11.3 Software security analysis

    * |sect| 5.11.5 Security activities in the software life cycle

  * Annex U Software code verification

* |Q-ST-80C|

  * |sect|\ 6.2 Requirements applicable to all software engineering processes

    * |sect|\ 6.2.3 Handling of critical software

    * |sect|\ 6.2.9 Software security

    * |sect|\ 6.2.10 Handling of security sensitive software

  * |sect|\ 6.3 Requirements applicable to individual software engineering
    processes or activities

    * |sect|\ 6.3.4 Coding

  * |sect|\ 7.2 Product quality requirements

    * |sect|\ 7.2.2 Design and related documentation

Note in particular that the SPARK language directly addresses several criteria
in |Q-ST-80C| requirement 6.2.3.2a:

* "use of a 'safe subset' of programming language"
* "use of formal design language for formal proof"

Further details on how SPARK and SPARK Pro can contribute to
compliance with the ECSS standards are presented in
:ref:`Space_Systems_SW_Compliance_with_ECSS-E-ST-40C` and
:ref:`Space_Systems_SW_Compliance_with_ECSS-Q-ST-80C`.


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
