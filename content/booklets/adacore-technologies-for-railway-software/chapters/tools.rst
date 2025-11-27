.. include:: ../../../global.txt

AdaCore Tools and Technologies Overview
=======================================

.. index:: single: Ada language; History

.. _Railway_SW_Ada:

Ada
---

.. _Railway_SW_Background:

Background
~~~~~~~~~~

.. index:: Ichbiah (Jean), Taft (Tucker)

Ada is a modern programming language designed for large, long-lived
applications |mdash| and embedded systems in particular |mdash| where
reliability, maintainability, and efficiency are essential.
It was originally developed in the early 1980s (this version is
generally known as Ada 83) by a team led by Jean Ichbiah at
CII-Honeywell-Bull in France. The language was revised and
enhanced in an upward compatible fashion in the early 1990s,
under the leadership of Tucker Taft from Intermetrics in the U.S.

The resulting language, Ada 95, was the first internationally
standardized (ISO) object-oriented language. Under the auspices
of ISO, a further (minor) revision was completed as an amendment
to the standard; this version of the language is known as Ada 2005.
Additional features (including support for contract-based programming
in the form of subprogram pre- and postconditions and type invariants)
were added in the Ada 2012 version of the language standard, and a
number of features to increase the language's expressiveness were
introduced in Ada 2022 (see :cite:`Railway_SW_ISO_IEC_2016`,
:cite:`Railway_SW_Barnes_Brosgol_2015`,
:cite:`Railway_SW_Barnes_2014`,
:cite:`Railway_SW_ISO_IEC_2022` for information about Ada).

.. index:: Lovelace (Augusta Ada), Babbage (Charles), Byron (Lord George)

The name "Ada" is not an acronym; it was chosen in honor of
Augusta Ada Lovelace (1815-1852), a mathematician who is
regarded as the world's first programmer because of her work with
Charles Babbage. She was also the daughter of the poet Lord Byron.

The Ada language is seeing significant usage worldwide in high-integrity /
safety-critical / high-security domains including railway systems,
commercial and military aircraft avionics, air traffic control, and
medical devices.

With its embodiment of modern software engineering principles, Ada
is an excellent teaching language for both introductory and advanced
computer science courses, and it has been the subject of significant
university research especially in the area of real-time technologies.
The so-called :index:`Ravenscar Profile` |mdash| a subset of the language's
concurrency features with deterministic semantics |mdash| broke new ground
in supporting the use of concurrent programming in high assurance software.

AdaCore has a long history and close connection with the Ada programming
language. Company members worked on the original Ada 83 design and review
and played key roles in the Ada 95 project as well as the subsequent
revisions. AdaCore's initial GNAT compiler was essential to the growth of
Ada 95; it was delivered at the time of the language's standardization,
thus guaranteeing that users would have a quality implementation for
transitioning to Ada 95 from Ada 83 or other languages.

.. _Railway_SW_Language_Overview:

Language Overview
~~~~~~~~~~~~~~~~~

Ada is multi-faceted. From one perspective it is a classical stack-based
general-purpose language, not tied to any specific development methodology.
It has a simple syntax, structured control statements, flexible data
composition facilities, strong type checking, traditional features for code
modularization (*subprograms*), and a mechanism for detecting and responding
to exceptional run-time conditions (*exception handling*).
But it also includes much more:

.. index:: single: Ada language; Scalar ranges

Scalar Ranges
^^^^^^^^^^^^^

Unlike languages based on C syntax (such as C++, Java, and C#), Ada allows the
programmer to simply and explicitly specify the range of values that are
permitted for variables of scalar types (integer, floating-point, fixed-point,
and enumeration types). The attempted assignment of an out-of-range value
causes a run-time error. The ability to specify range constraints makes
programmer intent explicit and makes it easier to detect a major source of
coding and user input errors. It also provides useful information to
static analysis tools and facilitates automated proofs of program properties.

Here's an example of an integer scalar range:

.. code-block:: ada

    declare
       Score : Integer range 1..100;
       N     : Integer;
    begin
       ... -- Code that assigns a value to N
       Score := N;
       -- A run-time check verifies that N is within the range 1..100
       -- If this check fails, the Constraint_Error exception is raised
    end;

.. index:: single: Ada language; Contract-based programming
.. index:: single: Ada language; Preconditions
.. index:: single: Ada language; Postconditions

Contract-Based Programming
^^^^^^^^^^^^^^^^^^^^^^^^^^

A feature introduced in Ada 2012 allows extending a subprogram specification
or a type/subtype declaration with a contract (a Boolean assertion).
Subprogram contracts take the form of preconditions and postconditions,
type contracts are used for invariants, and subtype contracts provide
generalized constraints (predicates). Through contracts the developer can
formalize the intended behavior of the application, and can
verify this behavior by testing, static analysis or formal proof.

Here's a skeletal example that illustrates contact-based programming; a
:ada:`Table` object is a fixed-length container for distinct :ada:`Float`
values.

.. code-block:: ada

    package Table_Pkg is
       type Table is private;  -- Encapsulated type

       procedure Insert (T : in out Table; Item: in Float)
         with Pre  => not Is_Full(T) and not Contains(T, Item),
              Post => Contains(T, Item);

       procedure Remove (T : in out Table; Item: out Float);
         with Pre  => Contains(T, Item),
              Post => not Contains(T, Item);

       function Is_Full  (T : in Table) return Boolean;
       function Contains (T : in Table; Item: in Float) return Boolean;
       ...
    private
       ... -- Full declaration of type Table
    end Table_Pkg;

    package body Table_Pkg is
       ... -- Implementation of Insert, Remove, Is_Full, and Contains
    end Table_Pkg;

A compiler option controls whether the pre- and postconditions
are checked at run time. If checks are enabled,
any pre- or postcondition failure |mdash| i.e., the contract's
Boolean expression evaluating to :ada:`False` |mdash| raises the
:ada:`Assertion_Error` exception.

.. index:: single: Ada language; Programming in the large

Programming in the large
^^^^^^^^^^^^^^^^^^^^^^^^

The original Ada 83 design introduced the package construct,
a feature that supports encapsulation (*information hiding*) and
modularization, and which allows the developer to control the
namespace that is accessible within a given compilation unit. Ada 95
introduced the concept of *child units*, adding considerable flexibility
and easing the design of very large systems.
Ada 2005 extended the language's modularization facilities by allowing
certain kinds of mutual references between package specifications,
thus making it easier to interface with languages such as Java.

.. index:: single: Ada language; Generic templates

Generic Templates
^^^^^^^^^^^^^^^^^

A key to reusable components is a mechanism for parameterizing modules
with respect to data types and other program entities, for example a
stack package for an arbitrary element type. Ada meets this requirement
through a facility known as *generics*; since the parameterization is done
at compile time, run-time performance is not penalized.

.. index:: single: Ada language; Object-Oriented Programming (OOP)

Object-Oriented Programming (OOP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Ada 83 was object-based, allowing the partitioning of a system into modules
corresponding to abstract data types or abstract objects.
Full OOP support was not provided since, first, it seemed not to be required
in the real-time domain that was Ada's primary target,
and, second, the apparent need for automatic garbage collection in an
Object-Oriented language would have interfered with predictable and efficient
performance.

However, large real-time systems often have components such as GUIs that do
not have real-time constraints and that could be most effectively developed
using OOP features. In part for this reason, Ada 95 supplies comprehensive
support for OOP, through its *tagged type* facility: classes, polymorphism,
inheritance, and dynamic binding. Ada 95 does not require automatic garbage
collection but rather supplies definitional features allowing the developer
to supply type-specific storage reclamation operations (*finalization*).
Ada 2005 brought additional OOP features including Java-like interfaces and
traditional :ada:`obj.op(...)` operation invocation notation.

Ada is methodologically neutral and does not impose a distributed overhead for
OOP. If an application does not need OOP, then the OOP features do not have
to be used, and there is no run-time penalty.
See :cite:`Railway_SW_Barnes_2014` or :cite:`Railway_SW_AdaCore_2016` for more details..

.. index:: single: Ada language; Concurrent programming

Concurrent Programming
^^^^^^^^^^^^^^^^^^^^^^

Ada supplies a structured, high-level facility for concurrency. The unit of
concurrency is a program entity known as a *task*. Tasks can communicate
implicitly via shared data or explicitly via a synchronous control mechanism
known as the rendezvous. A shared data item can be defined abstractly as a
*protected object* (a feature introduced in Ada 95), with operations executed
under mutual exclusion when invoked from multiple tasks.
Protected objects provide the functionality of semaphores and condition
variables but more clearly and reliably (e.g., avoiding subtle race
conditions).

Ada supports asynchronous task interactions for timeouts, software event
notifications, and task termination. Such asynchronous behavior is deferred
during certain operations, to prevent the possibility of leaving shared data
in an inconsistent state. Mechanisms designed to help take advantage of
multi-core architectures were introduced in Ada 2012.

.. index:: single: Ada language; Systems programming

Systems Programming
^^^^^^^^^^^^^^^^^^^

Both in the *core* language and the Systems Programming Annex, Ada supplies
the necessary features for hardware-specific processing.
For example, the programmer can specify the bit layout for fields in a record,
define alignment and size properties, place data at specific machine
addresses, and express specialized code sequences in assembly language.
Interrupt handlers can be written in Ada, using the protected type facility.

.. index:: single: Ada language; Real-time programming

Real-Time Programming
^^^^^^^^^^^^^^^^^^^^^

Ada's tasking facility and the Real-Time Systems Annex support common idioms
such as periodic or event-driven tasks, with features that can help avoid
unbounded priority inversions. A protected object locking policy is defined
that uses priority ceilings; this has an especially efficient implementation
in Ada (mutexes are not required) since protected operations are not allowed
to block. Ada 95 defined a task dispatching policy that basically requires
tasks to run until blocked or preempted. Subsequent versions of the language
standard introduced several other policies, such as Earliest Deadline First.

.. index:: single: Ada language; High-integrity systems
.. index:: single: Ada language; Prevention of vulnerabilities
.. index:: single: Ada language; Prevention of buffer overflow
.. index:: single: Ada language; Memory safety
.. index:: single: Ada language; Strong typing
.. index:: single: Ada language; Prevention of null pointer dereferencing
.. index:: single: Ada language; Prevention of dangling references
.. index:: Buffer overflow
.. index:: Memory safety

High-Integrity Systems
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Common Criteria security standard

With its emphasis on sound software engineering principles, Ada supports the
development of high-integrity applications, including those that need to be
certified against safety standards such |en-50128| for rail systems,
|do-178c| :cite:`Railway_SW_RTCA_EUROCAE_2011a` for avionics, and security standards
such as the Common Criteria :cite:`Railway_SW_CCDB_2022`.
Key to Ada's support for high-assurance software is the language's
memory safety; this is illustrated by a number of features, including:

* *Strong typing*

  Data intended for one purpose will not be accessed via inappropriate
  operations; errors such as treating pointers as integers (or vice versa)
  are prevented.

* *Array bounds checking*

  A run-time check guarantees that an array index is within the bounds of
  the array.
  This prevents buffer overflow vulnerabilities that are common in C and C++.
  In many cases a a compiler optimization can detect statically that the
  index is within bounds and thus eliminate any run-time code for the check.

* *Prevention of null pointer dereferences*

  As with array bounds, pointer dereferences are checked to make sure that
  the pointer is not null.
  Again, such checks can often be optimized out.

* *Prevention of dangling references*

  A scope accessibility checks ensures that a pointer cannot reference an
  object on the stack after exit/return from the scope (block or subprogram)
  in which the object is declared. Such checks are generally static,
  with no run-time overhead.

.. index:: single: Ada language; pragma Restrictions

However, the full language may be inappropriate in a safety-critical
application, since the generality and flexibility could interfere
with traceability / certification requirements. Ada addresses this issue by
supplying a compiler directive, :ada:`pragma Restrictions`, that
allows constraining the language features to a well-defined subset
(for example, excluding dynamic OOP facilities).

The evolution of Ada has seen the continued increase in support for
safety-critical and high-security applications. Ada 2005
standardized the :index:`Ravenscar Profile`, a collection of concurrency
features that are powerful enough for real-time programming but
simple enough to make certification and formal analysis practical.

.. index:: Contract-based programming
.. index:: single: Ada language; Contract-based programming

Ada 2012 introduced contract-based programming facilities, allowing the
programmer to specify preconditions and/or postconditions for subprograms,
and invariants for encapsulated (private) types. These can serve both
for run-time checking and as input to static analysis tools.

The most recent version of the standard, Ada 2022, has added several
contract-based programming constructs inspired by SPARK
(:ada:`Contract_Cases`, :ada:`Global`, and :ada:`Depends` aspects) and,
more generally, has enhanced the language's expressiveness. For example,
Ada 2022 has introduced some new syntax in its concurrency support and has
defined the :index:`Jorvik tasking profile <Jorvik profile>`, which is more
inclusive than :index:`Ravenscar <Ravenscar Profile>`.

Summary
^^^^^^^

In brief, Ada is an internationally standardized language combining
object-oriented programming features, well-engineered concurrency
facilities, real-time support, and built-in reliability through both
compile-time and run-time checks. As such it is an appropriate
language for addressing the real issues facing software developers
today. Ada has a long and successful history and is used throughout
a number of major industries to design software that protects
life and property.

.. index:: SPARK language
.. index:: SPARK Pro toolsuite
.. index:: GNATprove
.. index:: single: SPARK language; Usage
.. index:: single: SPARK Pro toolsuite; GNATprove


.. _Railway_SW_SPARK:

SPARK
-----

SPARK is a software development technology (programming language and
verification toolset) specifically designed for engineering
ultra-low defect level applications, for example where safety
and/or security are key requirements. SPARK Pro is AdaCore's
commercial-grade offering of the SPARK technology. The main component
in the toolset is GNATprove, which performs formal verification on
SPARK code.

SPARK has an extensive industrial track record. Since its inception
in the late 1980s it has been used worldwide in a range of
industrial applications such as railway signaling, civil and military
avionics, air traffic management / control, cryptographic
software, and cross-domain solutions.

The SPARK language has been stable over the years, with periodic
enhancements. The 2014 version of SPARK represented a major revision
:cite:`Railway_SW_McCormick_Chapin_2015`, :cite:`Railway_SW_AdaCore_Altran_2020`),
incorporating contract-based programming
syntax from Ada 2012, and subsequent upgrades included support for pointers
(access types) based on the Rust ownership model.

Flexibility
~~~~~~~~~~~

SPARK offers the flexibility of configuring the language on a per-project
basis. Restrictions can be fine-tuned based on the relevant coding
standards or run-time environments.
SPARK code can easily be combined with full Ada code or with C, so that
new systems can be built on and reuse legacy codebases.

.. index:: single: SPARK language; Static verification
.. index:: single: SPARK language; Formal verification
.. index:: single: SPARK technology; Absence of Run-Time Errors (AORTE)

Powerful Static Verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SPARK language supports a wide range of static verification techniques.
At one end of the spectrum is basic data and control flow analysis, i.e.,
exhaustive detection of errors such as attempted reads of uninitialized
variables, and ineffective assignments (where a variable is assigned a
value that is never read). For more critical applications, dependency
contracts can constrain the information flow allowed in an application.
Violations of these contracts |mdash| potentially representing violations of
safety or security policies |mdash| can then be detected even before the code
is compiled.

In addition, the language supports mathematical proof and can thus provide
high confidence that the software meets a range of assurance requirements:
from the absence of run-time errors (*AORTE*), to the enforcement of safety
or security properties, to compliance with a formal specification of the
program's required behavior.

Ease of Adoption
~~~~~~~~~~~~~~~~

User experience has shown that the language and the SPARK Pro toolset do not
require a steep learning curve. Training material such as AdaCore's online
AdaLearn course for SPARK :cite:`Railway_SW_AdaCore_AdaLearn` can quickly bring
developers up to speed; users are assumed to be experts in their own
application domain such as railway software and do not need to be familiar
with formal methods or the proof technology implemented by the toolset.
In effect, SPARK Pro is an advanced static analysis tool that will detect
many logic errors very early in the software life cycle. It can be smoothly
integrated into an organization's existing development and verification
methodology and infrastructure.

SPARK uses the standard Ada 2012 contract syntax, which both simplifies the
learning process and also allows new paradigms of software verification.
Programmers familiar with writing executable contracts for run-time assertion
checking can use the same approach but with additional flexibility: the
contracts can be verified either dynamically through classical run-time
testing methods or statically (i.e., pre-compilation and pre-test) using
automated tools.

.. index:: single: SPARK language; Hybrid verification
.. index:: Hybrid verification

.. _Railway_SW_Hybrid_Verification:

Hybrid Verification
~~~~~~~~~~~~~~~~~~~

SPARK supports *hybrid verification*, which combines testing and
formal proofs.
As an example, an existing project in Ada and C can adopt SPARK to implement
new functionality for critical components. The SPARK units can be analyzed
statically to achieve the desired level of verification, with testing
performed at the interfaces between the SPARK units and the modules in the
other languages.

.. index:: single: SPARK language; Reduced cost of verification

Reduced Cost and Improved Efficiency of Executable Object Code Verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Software verification typically involves extensive testing, including unit
tests and integration tests. Traditional testing methodologies are a major
contributor to the high delivery costs for safety-critical software.
Furthermore, they may fail to detect errors. SPARK addresses this issue by
allowing automated proof to be used to demonstrate functional correctness
at the subprogram level, either in combination with or as a replacement
for unit testing.
In the high proportion of cases where proofs can be discharged automatically,
the cost of writing unit tests is completely avoided. Moreover, verification
by proofs covers all execution conditions and not just a sample.

.. index:: GNAT Pro Assurance

.. _Railway_SW_GNAT_Pro_Assurance:

GNAT Pro Assurance
------------------

.. index:: single: GNAT Pro Assurance; Sustained branch
.. index:: Sustained branch
.. index:: single: GNAT Pro Assurance; GNAT Pro for Ada
.. index:: single: GNAT Pro Assurance; GNAT Pro for C
.. index:: single: GNAT Pro Assurance; GNAT Pro for C++
.. index:: single: GNAT Pro Assurance; GNAT Pro for Rust
.. index:: C language support
.. index:: C++ language support
.. index:: Rust language support

.. _Railway_SW_Sustained_Branches:

Sustained Branches
~~~~~~~~~~~~~~~~~~

GNAT Pro Assurance is a specialized version of the GNAT Pro development
environment, available for any of the products in the GNAT Pro family:
GNAT Pro for Ada, GNAT Pro for C, GNAT Pro for C++, and GNAT Pro for Rust.
It is targeted to projects
requiring customized support, including bug fixes and *known problems*
analyses, on a specific version of the toolchain. This service is
especially suitable for applications with long maintenance cycles or
certification requirements, since critical updates to the compiler or
other product components may become necessary years after the initial
release. Such customized maintenance of a specific version of the product
is known as a *sustained branch*.

A project on a sustained branch can monitor relevant known
problems, analyze their impact and, if needed, update to a newer version
of the product on the same development branch (i.e., not incorporating
changes introduced in later versions of the product).

Sustained branches are a practical solution to the problem of ensuring
toolchain stability while allowing flexibility in case an upgrade is
needed to correct a critical problem.

Language and Tool Support
~~~~~~~~~~~~~~~~~~~~~~~~~

GNAT Pro Assurance for Ada supports all versions of the Ada
language standard as well as multiple versions of C (C89, C99, and C11).
It provides an Integrated Development Environment
(see :ref:`Railway_SW_Integrated_Development_Environments`), a comprehensive toolsuite
including a visual debugger, and an extensive set of libraries and bindings.
Details on the GNAT Pro for Ada toolchain may be found in
:cite:`Railway_SW_AdaCore_Web_UG_Native`.
AdaCore's GNAT project facility, based on a multi-language builder for
systems organized into subsystems and libraries, is documented in
:cite:`Railway_SW_AdaCore_Web_GPR`.

.. index:: single: GNAT Pro Assurance; Configurable Run-Time Libraries

.. _Railway_SW_Configurable_Run-Time_Libraries:

Configurable Run-Time Libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: Light Profile
.. index:: Light-Tasking Profile
.. index:: Certifiable profile

Two specific GNAT-defined run-time libraries have been designed with
certification in mind and are known as the Certifiable Profiles
(see :cite:`Railway_SW_AdaCore_Web_UG_Cross`):

* *Light Profile*

* *Light-Tasking Profile*

The Light Profile provides a flexible Ada subset that is supported by a
certifiable Ada run-time library. Depending on application requirements,
this profile can be further restricted through the :ada:`Restrictions` pragma,
with the application only including run-time code that is used by the
application.

These run-time libraries can also be customized directly to suit
certification requirements: unneeded packages can be removed to allow for
self-certification of the runtime, while the "|nbhyphen| nostdlib" linker
switch can be used to prevent the use of the runtime. Even when the run-time
library is suppressed, some run-time sources are still required to provide
compile-time definitions. While this code produces no object code, the
certification protocol may still require tests to ensure correct access
to these definitions.

.. index:: Ravenscar Profile

The Light-Tasking Profile expands the Light Profile to include Ravenscar
tasking support, allowing developers to use concurrency in their certification
applications.

Although limited in terms of dynamic Ada semantics, all Certifiable Profiles
fully support static Ada constructs such as private types, generic templates,
and child units. Some dynamic semantics are also supported. For example,
these profiles allow the use of tagged types (at library level) and other
Object-Oriented Programming features, including dynamic dispatching.
The general use of dynamic dispatching at the application level can be
prevented through pragma :ada:`Restrictions`.

A traditional problem with predefined profiles is their inflexibility:
if a feature outside a given profile is needed, then it is the developer's
responsibility to address the certification issues deriving from its use.
GNAT Pro Assurance accommodates this need by allowing the developer to define
a profile for the specific set of features that are used. Typically this will
be for features with run-time libraries that require associated certification
materials. Thus the program will have a tailored run-time library supporting
only those features that have been specified.

More generally, the configurable run-time capability allows specifying support
for Ada's dynamic features in an |a-grave| la carte fashion ranging from none
at all to full Ada.
The units included in the executable may be either a subset of
the standard libraries provided with GNAT Pro, or specially tailored to the
application. This latter capability is useful, for example, if one of the
predefined profiles implements almost all the dynamic functionality needed
in an existing system that has to meet new safety-critical requirements,
and where the costs of adapting the application without the additional
run-time support are considered prohibitive.


Full Implementation of Ada Standards
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GNAT Pro provides a complete implementation of the Ada language from Ada 83
to Ada 2012, and support for selected features from Ada 2022.
Developers of safety-critical and high-security systems can thus take
advantage of features such as contract-based programming, which effectively
embed requirements in the source program text and simplify verification.

.. index:: single: GNAT Pro Assurance; Source-to-object traceability

Source to Object Traceability
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A compiler option can limit the use of language constructs that generate
object code that is not directly traceable to the source code.
As an add-on service, AdaCore can perform an analysis that demonstrates
this traceability and justifies any remaining cases of non-traceable code.

.. index:: single: AdaCore; Support services
.. index:: single: AdaCore; Training and consulting services

Safety-Critical Support and Expertise
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the heart of every AdaCore subscription are the support services that
AdaCore provides to its customers. AdaCore staff are recognized experts on
the Ada language, software certification standards in several domains,
compilation technologies, and static and dynamic verification.
They have extensive experience in supporting customers in railway, avionics,
space, energy, air traffic management/control, automotive, and military
projects. Every AdaCore product comes with front-line support provided
directly by these experts, who are also the developers of the technology.
This ensures that customers' questions (requests for guidance on feature
usage, suggestions for technology enhancements, or defect reports) are
handled efficiently and effectively.

Beyond this bundled support, AdaCore also provides Ada language and tool
training as well as on-site consulting on topics such as how to best deploy
the technology, and assistance on start-up issues. On-demand tool development
and ports to new platforms are also available.

.. index:: Libadalang
.. index:: single: GNAT Pro Assurance; Libadalang

Libadalang
~~~~~~~~~~

Libadalang is a library included with GNAT Pro that gives applications access
to the complete syntactic and semantic structure of an Ada compilation unit.
This library is typically used by tools that need to perform some sort of
static analysis on an Ada program.

AdaCore can assist customers in developing libadalang-based tools to meet
their specific needs, as well as develop such tools upon request.

Typical libadalang applications include:

* Static analysis (property verification)
* Code instrumentation
* Design and document generation tools
* Metric testing or timing tools
* Dependency tree analysis tools
* Type dictionary generators
* Coding standard enforcement tools
* Language translators (e.g., to CORBA IDL)
* Quality assessment tools
* Source browsers and formatters
* Syntax directed editors

.. index:: GNATstack
.. index:: single: GNAT Pro Assurance; GNATstack

.. _Railway_SW_GNATstack:

GNATstack
~~~~~~~~~

Included with GNAT Pro is GNATstack, a static analysis tool that enables an
Ada/C software developer to accurately predict the maximum size of the memory
stack required for program execution.

GNATstack statically predicts the maximum stack space required by each task
in an application. The computed bounds can be used to ensure that sufficient
space is reserved, thus guaranteeing safe execution with respect to stack
usage. The tool uses a conservative analysis to deal with complexities such
as subprogram recursion, while avoiding unnecessarily pessimistic estimates.

This static stack analysis tool exploits data generated by the compiler to
compute worst-case stack requirements. It performs per-subprogram stack usage
computation combined with control flow analysis.

GNATstack can analyze object-oriented applications, automatically determining
maximum stack usage on code that uses dynamic dispatching in Ada.
A dispatching call challenges static analysis because the identity of the
subprogram being invoked is not known until run time. GNATstack solves this
problem by statically determining the subset of potential targets (primitive
operations) for every dispatching call. This significantly reduces the
analysis effort and yields precise stack usage bounds on complex Ada code.

GNATstack's analysis is based on information known at compile time. When the
tool indicates that the result is accurate, the computed bound can never be
exceeded.

On the other hand, there may be cases in which the results will not be
accurate (the tool will report such situations) because of some missing
information (such as the maximum depth of subprogram recursion, indirect
calls, etc.). The user can assist the tool by specifying missing call graph
and stack usage information.

GNATstack's main output is the worst-case stack usage for every entry point,
together with the paths that result in these stack sizes.
The list of entry points can be automatically computed (all the tasks,
including the environment task) or can be specified by the user (a list of
entry points or all the subprograms matching a given regular expression).

GNATstack can also detect and display a list of potential problems when
computing stack requirements:

* Indirect (including dispatching) calls. The tool will indicate the number
  of indirect calls made from any subprogram.
* External calls. The tool displays all the subprograms that are reachable
  from any entry point for which there is no stack or call graph information.
* Unbounded frames. The tool displays all the subprograms that are reachable
  from any entry point with an unbounded stack requirement.
  The required stack size depends on the arguments passed to the subprogram.
  For example:

  .. code-block:: ada

      procedure P(N : Integer) is
         S : String (1..N);
      begin
         ...
      end P;

* Cycles. The tool can detect all the cycles (i.e., potential recursion) in
  the call graph.

GNATstack allows the user to supply a text file with the missing information,
such as the potential targets for indirect calls, the stack requirements for
external calls, and the maximal size for unbounded frames.


.. index:: GNAT Static Analysis Suite (GNAT SAS)

.. _Railway_SW_GNAT_Static_Analysis_Suite:

GNAT Static Analysis Suite (GNAT SAS)
-------------------------------------

.. index:: Defects and vulnerability analysis (in GNAT SAS)
.. index:: single: GNAT Static Analysis Suite (GNAT SAS); Defects and vulnerability analysis

Defects and Vulnerability Analyzer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GNAT SAS features an Ada source code analyzer that detects run-time and logic
errors. It assesses potential bugs and vulnerabilities before program
execution, serving as an automated peer reviewer, helping to find errors
easily at any stage of the development life-cycle. It helps improve code
quality and makes it easier to perform safety and/or security
analysis.

The defects and vulnerability analyzer can detect several of the
"Top 25 Most Dangerous Software Errors" in the Common Weakness Enumeration.
It is a stand-alone tool that runs on Windows and Linux platforms and
may be used with any standard Ada compiler or fully integrated into the
GNAT Pro development environment.


.. index:: GNATmetric
.. index:: single: GNAT Static Analysis Suite (GNAT SAS); GNATmetric

GNATmetric
~~~~~~~~~~

The GNATmetric tool analyzes source code to calculate a set of commonly used
industry metrics, thus allowing developers to estimate the size and better
understand the structure of the source code. This information also
facilitates satisfying the requirements of certain software development
frameworks.

.. index:: GNATcheck
.. index:: single: GNAT Static Analysis Suite (GNAT SAS); GNATcheck

.. _Railway_SW_GNATcheck:

GNATcheck
~~~~~~~~~

GNATcheck is a coding standard verification tool that is extensible and
rule-based. It allows developers to completely define a project-specific
coding standard as a set of rules, for example a subset of permitted
language features and/or code formatting and style conventions.
It verifies a program's conformance with the resulting rules and thereby
facilitates demonstration of a system's compliance with a certification
standard's requirements on language subsetting.

GNATcheck provides:

* An integrated "Ada Restrictions" mechanism for banning specific features
  from an application. This can be used to restrict features
  such as tasking, exceptions, dynamic allocation, fixed- or floating point,
  input/output, and unchecked conversions.

* Restrictions specific to GNAT Pro, such as banning features that result
  in the generation of implicit loops or conditionals
  in the object code, or in the generation of elaboration code.

* Additional Ada semantic rules resulting from customer input, such as
  ordering of parameters, normalized naming of entities, and
  subprograms with multiple returns.

* An easy-to-use interface for creating and using a complete coding standard.

* Generation of project-wide reports, including evidence of the level of
  compliance with a given coding standard.

* Over 30 compile-time warnings from GNAT Pro that detect typical error
  situations, such as local variables being used before being
  initialized, incorrect assumptions about array lower bounds, infinite
  recursion, incorrect data alignment, and accidental hiding of names.

* Style checks that allow developers to control indentation, casing,
  comment style, and nesting level.

AdaCore's :index:`GNATformat` tool :cite:`Railway_SW_AdaCore_Web_GNATformat`, which
formats Ada source code according to the GNAT coding style
:cite:`Railway_SW_AdaCore_Coding_Style`, can help avoid having code that violates
GNATcheck rules. GNATformat is included in the GNAT Pro for Ada toolchain.

GNATcheck comes with a query language (LKQL, for Language Kit Query Language)
that lets developers define their own checks for any in-house rules that need
to be followed.
GNATcheck can thus be customized to meet an organization's specific
requirements, processes and procedures.


.. index:: GNAT Dynamic Analysis Suite (GNAT DAS)

.. _Railway_SW_GNAT_Dynamic_Analysis_Suite:

GNAT Dynamic Analysis Suite (GNAT DAS)
--------------------------------------

.. index:: GNATtest
.. index:: single: GNAT Dynamic Analysis Suite (GNAT DAS); GNATtest

.. _Railway_SW_GNATtest:

GNATtest
~~~~~~~~

The GNATtest tool helps create and maintain a complete unit testing
infrastructure for complex projects. It captures
the simple idea that each public subprogram (these are known as
*visible* subprograms in Ada) should have at least one corresponding
unit test. GNATtest takes a project file as input, and produces two outputs:

* The complete harnessing code for executing all the unit tests under
  consideration. This code is generated completely automatically.

* A set of separate test stubs for each subprogram to be tested.
  These test stubs are to be completed by the user.

GNATtest handles Ada's Object-Oriented Programming features and can be used
to help verify tagged type substitutability (the Liskov Substitution
Principle) that can be used to demonstrate consistency of class hierarchies.

Testing a private subprogram is outside the scope of GNATtest but can be
implemented by defining the relevant testing code in a private child of the
package that declares the private subprogram.
Additionally, hybrid verification can help (see :ref:`Railway_SW_Hybrid_Verification`):
augmenting testing with the use of SPARK to formally prove relevant properties
of the private subprogram.

.. index:: GNATemulator
.. index:: single: GNAT Dynamic Analysis Suite (GNAT DAS); GNATemulator

GNATemulator
~~~~~~~~~~~~

GNATemulator is an efficient and flexible tool that provides integrated,
lightweight target emulation.

Based on the QEMU technology, a generic and open-source machine emulator
and virtualizer, GNATemulator allows software developers to
compile code directly for their target architecture and run it on their
host platform, through an approach that translates from the
target object code to native instructions on the host. This avoids the
inconvenience and cost of managing an actual board, while offering an
efficient testing environment compatible with the final hardware.

There are two basic types of emulators. The first can serve as a surrogate
for the final hardware during development for a wide range of verification
activities, particularly those that require time accuracy. However, they
tend to be extremely costly, and are often very slow. The second, which
includes GNATemulator, does not attempt to be a complete time-accurate
target board simulator, and thus it cannot be used for all aspects of
testing. But it does provide a very efficient and cost-effective way to
execute the target code very early in the development and verification
processes. GNATemulator thus offers a practical compromise between a native
environment that lacks target emulation capability, and a cross configuration
where the final target hardware might not be available soon enough or in
sufficient quantity.

.. index:: GNATcoverage
.. index:: single: GNAT Dynamic Analysis Suite (GNAT DAS); GNATcoverage

GNATcoverage
~~~~~~~~~~~~

GNATcoverage is a code coverage analysis tool. Its results are computed from
trace files that show which program constructs have been exercised by a given
test campaign. With source code instrumentation, the tool produces these files
by executing an alternative version of the program, built from source code
instrumented to populate coverage-related data structures.
Through an option to GNATcoverage, the user can specify the granularity of
the analysis: statement coverage, decision coverage, or Modified Condition /
Decision Coverage (MC/DC).

Source-based instrumentation brings several major benefits: efficiency of tool
execution (much faster than alternative coverage strategies using binary
traces and target emulation, especially on native platforms), compact-size
source trace files independent of execution duration, and support for coverage
of shared libraries.

.. index:: single: GNAT Dynamic Analysis Suite (GNAT DAS); GNATfuzz
.. index:: GNATfuzz

.. _Railway_SW_GNATfuzz:

GNATfuzz
~~~~~~~~

GNATfuzz is a fuzzing tool; i.e., a tool that automatically
and repeatedly executes tests and generates new test cases at a very high
frequency to detect faulty behavior of the system under test. Such anomalous
behavior is captured by monitoring the system for triggered exceptions,
failing built-in assertions, and signals such as SIGSEGV.

Fuzz testing has proven to be an effective mechanism for finding corner-case
vulnerabilities that traditional human-driven verification mechanisms,
such as unit and integration testing, can miss.
Since such vulnerabilities can often lead to malicious exploitations,
fuzzing technology can help meet security verification
requirements.

However, fuzz-testing campaigns are complex and time-consuming to construct,
execute and monitor. GNATfuzz simplifies the process by analyzing a code base
and identifying subprograms that can act as fuzz-test entry points. GNATfuzz
then automates the creation of test harnesses suitable for fuzzing.
In addition, GNATfuzz will automate the building, executing and analyzing
of fuzz-testing campaigns.

GNATfuzz can serve a useful role as part of the software development
and verification life cycle processes. For example, by detecting
anomalous behavior such as data corruption due to
task or interrupt conflicts, GNATfuzz can help prevent defects from being
introduced into the source code.

.. index:: TGen
.. index:: single: GNAT Dynamic Analysis Suite (GNAT DAS); TGen

TGen
~~~~

TGen is an experimental run-time library / marshalling technology that can be
used by :index:`GNATtest` and/or :index:`GNATfuzz` to automate the production
of test cases for Ada code. It performs type-specific low-level processing to
generate test vectors for subprogram parameters, such as uniform value
distribution for scalar types and analogous strategies for unconstrained
arrays and record discriminants. A command-line argument specifies the number
of test values to be generated, and these can then be used as input to test
cases created by GNATtest.

TGen can also be used with GNATfuzz, to help start a fuzz-testing campaign
when the user supplies an initial set of test cases where some may contain
invalid data. GNATfuzz will utilize coverage-driven fuzzer mutations coupled
with TGen to convert invalid test cases into valid ones. TGen represents test
data values compactly, removing a large amount of memory padding that would
otherwise be present for alignment of data components. With its
space-efficient representation, TGen significantly increases the probability
of a successful mutation that results in a new valid test case.

.. index:: GNAT Pro for Rust
.. index:: Rust language support

GNAT Pro for Rust
-----------------

The Rust language was designed for software that needs to meet stringent
requirements for both assurance and performance: Rust is a memory-safe
systems-programming language with software integrity guarantees (in both
concurrent and sequential code) enforced by compile-time checks. The language
is seeing growing use in domains such as automotive systems and is a viable
choice for railway software.

AdaCore's GNAT Pro for Rust is a complete development environment for the
Rust programming language, supporting both native builds and cross compilation
to embedded targets. The product is not a fork of the Rust programming
language or the Rust tools. Instead, GNAT Pro for Rust is a professionally
supported build of a selected version of rustc and other core Rust development
tools that offers stability for professional and high-integrity Rust projects.
Critical fixes to GNAT Pro for Rust are upstreamed to the Rust community,
and critical fixes made by the community to upstream Rust tools are backported
as needed to the GNAT Pro for Rust code base.
Additionally, the Assurance edition of GNAT Pro for Rust includes the
"sustained branch" service (see :ref:`Railway_SW_Sustained_Branches`) that strikes the
balance between tool stability and project flexibility.

.. index:: Integrated Development Environments (IDEs)

.. _Railway_SW_Integrated_Development_Environments:

Integrated Development Environments (IDEs)
------------------------------------------

.. index:: single: Integrated Development Environments (IDEs); GNAT Studio
.. index:: GNAT Studio IDE

GNAT Studio
~~~~~~~~~~~

GNAT Studio is a powerful and simple-to-use IDE that streamlines software
development from the initial coding stage through testing, debugging, system
integration, and maintenance. It is designed to allow programmers to get the
most out of GNAT Pro technology.

**Tools**

GNAT Studio's extensive navigation and analysis tools can generate a variety
of useful information including call graphs, source dependencies,
project organization, and complexity metrics, giving a thorough understanding
of a program at multiple levels. It allows interfacing with third-party
version control systems, easing both development and maintenance.

**Robust, Flexible and Extensible**

Especially suited for large, complex systems, GNAT Studio can import existing
projects from other Ada implementations while adhering to their
file naming conventions and retaining the existing directory organization.
Through the multi-language capabilities of GNAT Studio, components
written in C and C++ can also be handled. The IDE is highly extensible;
additional tools can be plugged in through a simple scripting
approach. It is also tailorable, allowing various aspects of the program's
appearance to be customized in the editor.

**Easy to learn, easy to use**

GNAT Studio is intuitive to new users thanks to its menu-driven interface
with extensive online help (including documentation on all the
menu selections) and *tool tips*. The Project Wizard makes it simple to get
started, supplying default values for almost all of the project properties.
For experienced users, it offers the necessary level of control for
advanced purposes; e.g., the ability to run command scripts. Anything that
can be done on the command line is achievable through the menu interface.

**Remote Programming**

Integrated into GNAT Studio, Remote Programming provides a secure and
efficient way for programmers to access any number of remote servers
on a wide variety of platforms while taking advantage of the power and
familiarity of their local PC workstations.

.. index:: single: Integrated Development Environments (IDEs); VS Code support
.. index:: VS Code support

VS Code Extensions for Ada and SPARK
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AdaCore's extensions to Visual Studio Code (VS Code) enable Ada and SPARK
development with a lightweight editor, as an alternative to the full
GNAT Studio IDE. Functionality includes:

* Syntax highlighting for Ada and SPARK files
* Code navigation
* Error diagnostics (errors reported in the Problems pane)
* Build integration (execution of GNAT-based toolchains from within VS Code)
* Display of SPARK proof results (green/red annotations from GNATprove)
* Basic IntelliSense (completion and hover information for known symbols)

.. index:: single: Integrated Development Environments (IDEs); GNATbench
.. index:: single: Integrated Development Environments (IDEs); Workbench
.. index:: single: Integrated Development Environments (IDEs); Eclipse
.. index:: Workbench IDE (Wind River)
.. index:: Eclipse IDE
.. index:: GNATbench IDE

Eclipse Support |ndash| GNATbench
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GNATbench is an Ada development plug-in for Eclipse and Wind River's Workbench
environment. The Workbench integration supports Ada development on a variety
of VxWorks real-time operating systems. The Eclipse version is primarily
for native applications, with some support for cross development. In both
cases the Ada tools are tightly integrated.

.. index:: single: Integrated Development Environments (IDEs); GNATdashboard
.. index:: GNATdashboard IDE

GNATdashboard
~~~~~~~~~~~~~

GNATdashboard serves as a one-stop control panel for monitoring and improving
the quality of Ada software. It integrates and aggregates the results of
AdaCore's various static and dynamic analysis tools (GNATmetric, GNATcheck,
GNATcoverage, SPARK Pro, among others) within a common interface, helping
quality assurance managers and project leaders understand or reduce
their software's technical debt, and eliminating the need for manual input.

GNATdashboard fits naturally into a continuous integration environment,
providing users with metrics on code complexity, code coverage,
conformance to coding standards, and more.
