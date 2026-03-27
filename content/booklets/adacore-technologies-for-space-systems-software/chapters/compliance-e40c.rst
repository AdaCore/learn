.. include:: ../../../global.txt

.. _Space_Systems_SW_Compliance_with_ECSS-E-ST-40C:

Compliance with ECSS-E-ST-40C
=============================

The |E-ST-40C| standard is concerned with software engineering |mdash|
the principles and techniques underlying the production of code that is
reliable, safe, secure, readable, maintainable, portable and efficient.
These are the goals that drove the design of the Ada language (and its
SPARK subset), whose features assist in designing a modular and robust
system architecture and in preventing or detecting errors such as type
mismatches or buffer overruns that can arise in other languages.

This chapter explains how Ada and SPARK, together with the relevant
AdaCore development and verification tools, can help a space software
supplier meet many of the requirements presented in |E-ST-40C|. The
section numbers in braces refer to the associated content in |E-ST-40C|.

.. index:: single: ECSS-E-ST-40C compliance; §5.4 Software requirements and architecture engineering process

Software requirements and architecture engineering process {§5.4}
-----------------------------------------------------------------

Software architecture design {§5.4.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Transformation of software requirements into a software architecture {§5.4.3.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall transform the requirements for the software into an
  architecture that describes the top-level structure; identifies the software
  components, ensuring that all the requirements for the software item are
  allocated to the software components and later refined to facilitate
  detailed design; covers as a minimum hierarchy, dependency, interfaces and
  operational usage for the software components; documents the process, data
  and control aspects of the product; describes the architecture static
  decomposition into software elements such as packages, classes or units;
  describes the dynamic architecture, which involves the identification of
  active objects such as threads, tasks and processes; describes the software
  behavior." {§5.4.3.1a}

  * The Ada and SPARK languages (and thus the GNAT Pro Ada and SPARK Pro
    toolsuites directly support this requirement. Relevant
    features include packages, child libraries, subunits, private types,
    tasking, and object-oriented programming (tagged types). The
    :index:`GNATstub` utility (included with GNAT Pro Ada) is useful here;
    it generates empty package bodies ("stubs") from a software design's
    top-level API (package specs).

Software design method {§5.4.3.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall use a method (e.g., object oriented or functional) to
  produce the static and dynamic architecture including: software elements,
  their interfaces and; software elements relationships." {§5.4.3.2a}

  * Ada and SPARK are methodology agnostic and fully support both
    object-oriented and functional styles.

Selection of a computational model for real-time software {§5.4.3.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The dynamic architecture design shall be described according to an
  analytical computational model." {§5.4.3.3a}

  * The Ada and SPARK tasking facility supports a stylistic idiom
    that is amenable to Rate Monotonic Analysis, allowing static
    verification that real-time deadlines will be met.

Description of software behavior {§5.4.3.4}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The software design shall also describe the behaviour of the software,
  by means of description techniques using automata and scenarios."
  {§5.4.3.4a}

  * Ada and SPARK are appropriate target languages for tools that support
    such techniques.

Development and documentation of the software interfaces {§5.4.3.5}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document a software preliminary design for
  the interfaces external to the software item and between the software
  components of the software item." {§5.4.3.5a}

  * The supplier can use the Ada / SPARK package facility to specify the
    interfaces, both external and internal. The contract-based programming
    features provide additional expressive power, allowing the specification
    of pre- and postconditions for the subprograms comprising an interface.

Definition of methods and tools for software intended for reuse {§5.4.3.6}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall define procedures, methods and tools for reuse, and
  apply these to the software engineering processes to comply with the
  reusability requirements for the software development." {§5.4.3.6a}

  * Ada and SPARK facilitate reuse via the separate compilation semantics
    (which allows "bottom-up" development by reusing existing libraries)
    and the generic facility (which, for example, allows a module to be
    defined in a general and type-independent fashion and then instantiated
    with specific types as needed). The semantics for these features
    enforces safe reuse:

    * All checks that are performed within a single compilation unit are
      also enforced across separate compilation boundaries.

    * A post-compilation pre-link check detects and prevents "version skew"
      (building an executable where some compilation unit depends on an
      obsolescent version of another unit).

    * Unlike the situation with C++ templates, a type mismatch in an Ada
      generic instantiation is detected and prevented at compile time,
      ensuring consistency between the instantiation and the generic unit.

.. index:: single: ECSS-E-ST-40C compliance; §5.5 Software design and implementation engineering process

Software design and implementation engineering process {§5.5}
-------------------------------------------------------------

Design of software items {§5.5.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Detailed design of each software component {§5.5.2.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop a detailed design for each component of the
  software and document it." {§5.5.2.1a}

  * Ada / SPARK features, including packages and child units,
    and thus GNAT Pro for Ada and SPARK Pro, help meet
    this requirement. The contract-based programming feature (e.g.,
    pre- and postconditions) allows the supplier to express low-level
    requirements as part of the software architecture, facilitating
    the low-level design of algorithms.

* "Each software component shall be refined into lower levels containing
  software units that can be coded, compiled, and tested." {§5.5.2.1b}

  * Relevant Ada / SPARK features include packages, child units, and
    subunits.

Development and documentation of the software interfaces detailed design {§5.5.2.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document a detailed design for the interfaces
  external to the software items, between the software components, and between
  the software units, in order to allow coding without requiring further
  information." {§5.5.2.2a}

  * Ada / SPARK features, including packages and child units,
    and thus SPARK Pro and GNAT Pro for Ada, help meet this
    requirement. The contract-based programming feature (e.g., pre- and
    postconditions) allows the supplier to express low-level requirements as
    part of the interfaces, facilitating the implementation of algorithms.

Production of the detailed design model {§5.5.2.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall produce the detailed design model of the software
  components defined during the software architectural design, including their
  static, dynamic and behavioural aspects." {§5.5.2.3a}

  * Ada / SPARK features such as packages, child units, and contract-based
    programming, and thus SPARK Pro and GNAT Pro for Ada, help meet this
    requirement.

Software detail design method {§5.5.2.4}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall use a design method (e.g. object oriented or functional
  method) to produce the detailed design including: software units, their
  interfaces, and; (*sic*) software units relationships." {§5.5.2.4a}

  * Ada and SPARK are methodology agnostic and fully support both
    object-oriented and functional styles.

Detailed design of real-time software {§5.5.2.5}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The dynamic design model shall be compatible with the computational model
  selected during the software architectural design model" {§5.5.2.5a}

  * The Ada / SPARK tasking model allows a straightforward mapping from the
    architectural design (where the system comprises a collection of tasks
    that interact via protected shared resources) to the detailed design.

* "The supplier shall document and justify all timing and synchronization
  mechanisms" {§5.5.2.5b}

  * The Ada / SPARK tasking model supplies the necessary timing and
    synchronization support.

* "The supplier shall document and justify all the design mutual exclusion
  mechanisms to manage access to the shared resources." {§5.5.2.5c}

  * The Ada / SPARK tasking model supplies the necessary mutual exclusion
    mechanisms (protected types/objects, pragma Atomic).
    The protected type/object facility prevents certain kinds of race
    conditions: in state-based mutual exclusion, the state of an object
    cannot change between the time that a task evaluates the state
    condition and when it executes the code based on that state.
    Other race conditions can be detected by the Defects and Vulnerability
    Analyzer in the GNAT Static Analysis Suite.

* "The supplier shall document and justify the use of dynamic allocation
  of resources." {§5.5.2.5d}

  * Ada has a general and flexible mechanism for dynamic memory management,
    including the ability of the programmer to specify the semantics of
    allocation and deallocation within a storage pool. This can be used,
    for example, to define a fragmentation-free strategy for memory
    management with constant time for allocation and deallocation. The
    latest version of SPARK includes a facility for safe pointers.

* "The supplier shall ensure protection against problems that can be induced
  by the use of dynamic allocation of resources, e.g. memory leaks."
  {§5.5.2.5e}

  * Ada includes a variety of mechanisms that assist in preventing dynamic
    memory management issues.
    The :ada:`No_Standard_Allocators_After_Elaboration` argument to
    pragma :ada:`Restrictions` produces a run-time check that detects
    attempts to perform allocations from a standard storage pool after
    elaboration (initialization). Depending on the program structure,
    static analysis by the GNAT Static Analysis Suite's Defect and
    vulnerability Analyzer may be able to determine that this check
    will never fail.

Utilization of description techniques for the software behaviour {§5.5.2.6}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The behavioural design of the software units shall be described by means
  of techniques using automata and scenarios." {§5.5.2.6a}

  * Ada and SPARK are appropriate target languages for tools that support
    such techniques.

Coding and testing {§5.5.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Development and documentation of the software units {§5.5.3.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document the following: the coding of each
  software unit; the build procedures to compile and link software units"
  {§5.5.3.1a}

  * The GNAT Pro project and gprbuild facility automate the build process
    and prevent "version skew".

Software unit testing {§5.5.3.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document the test procedures and data
  for testing each software unit" {§5.5.3.2a}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

* "The supplier shall test each software unit ensuring that it satisfies its
  requirements and document the test results." {§5.5.3.2b}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

* "The unit test shall exercise: code using boundaries at *n-1*, *n*, *n+1*
  including looping instructions *while*, *for* and tests that use
  comparisons; all the messages and error cases defined in the design
  document; the access of all global variables as specified in the design
  document; out of range values for input data, including values that can
  cause erroneous results in mathematical functions; the software at the
  limits of its requirements (stress testing)." {§5.5.3.2c}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

Integration {§5.5.4}
~~~~~~~~~~~~~~~~~~~~

Software units and software component integration and testing {§5.5.4.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall integrate the software units and software components,
  and test them, as the aggregates are developed, in accordance with the
  integration plan, ensuring each aggregate satisfies the requirements of
  the software item and that the software item is integrated at the conclusion
  of the integration activity." {§5.5.4.2a}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process, supplementing the GNAT Pro for Ada
    compilation facilities.

.. index:: single: ECSS-E-ST-40C compliance; §5.6 Software validation process


Validation activities with respect to the technical specification {§5.6.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Development and documentation of a software validation specification with respect to the technical specification {§5.6.3.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document, for each requirement of the
  software item in TS [Technical Specification] (including ICD [Interface
  Control Document]), a set of tests, test cases (inputs, outputs, test
  criteria) and test procedures ...." {§5.6.3.1a}

  * AdaCore's GNAT Pro Ada environment and GNAT Dynamic Analysis
    Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    support this process.

* "Validation shall be performed by test." {§5.6.3.1b}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

* "If it can be justified that validation by test cannot be performed,
  validation shall be performed by either analysis, inspection or review
  of design" {§5.6.3.1c}

  * The Defects and Vulnerability Analyzer (see
    :ref:`Space_Systems_SW_Defects_and_Vulnerability_Analyzer`) in
    the GNAT Static Analysis Suite and/or SPARK Pro may be able to
    show that a run-time check will always succeed and that no test
    case will trigger a failure.

Validation activities with respect to the requirements baseline {§5.6.4}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Development and documentation of a software validation specification with respect to the requirements baseline {§5.6.4.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall develop and document, for each requirement of the
  software item in RB [Requirements Baseline] (including IRD [Interface
  Requirements Document]), a set of tests, test cases (inputs, outputs,
  test criteria) and test procedures ...." {§5.6.4.1a}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

* "Validation shall be performed by test." {§5.6.4.1b}

  * AdaCore's GNAT Dynamic Analysis Suite (see
    :ref:`Space_Systems_SW_GNAT_Dynamic_Analysis_Suite_GNAT_DAS`) can
    assist in this process.

* "If it can be justified that validation by test cannot be performed,
  validation shall be performed by either analysis, inspection or review
  of design” {§5.6.4.1c}

  * The Defects and Vulnerability Analyzer (see
    :ref:`Space_Systems_SW_Defects_and_Vulnerability_Analyzer`) in
    the GNAT Static Analysis Suite and/or SPARK Pro may be able to
    show that a run-time check will always succeed and that no test
    case will trigger a failure.

.. index:: single: ECSS-E-ST-40C compliance; §5.7 Software delivery and acceptance process

Software delivery and acceptance process {§5.7}
-----------------------------------------------

Software acceptance {§5.7.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Executable code generation and installation {§5.7.3.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The acceptance shall include generation of the executable code from
  configuration managed source code components and its installation on
  the target environment." {§5.7.3.3a}

  * The GNAT Pro project and gprbuild facility can assist in the build
    and installation process.

.. index:: single: ECSS-E-ST-40C compliance; §5.8 Software verification process

Software verification process {§5.8}
------------------------------------

Verification activities {§5.8.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Verification of the software detailed design {§5.8.3.4}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall verify the software detailed design ensuring that:
  ... 5. testing is feasible, by assessing that: (a) controllability
  and observability features are identified and included in the detailed
  design in order to prepare the effective testing of the performance
  requirements; (b) computationally invariant properties and temporal
  properties are added within the design; (c) fault injection is
  possible. ... 7. the design is correct with respect to requirements
  and interfaces, including safety, security, and other critical
  requirements; 8. the design implements proper sequence of events,
  inputs, outputs, interfaces, logic flow, allocation of timing and
  sizing budgets, and error handling; ..." {§5.8.3.4a}

  * SPARK (and GNATprove) help meet criteria 5, 7, and 8.

Verification of code {§5.8.3.5}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall verify the software code ensuring at least that:
  1. the code is externally consistent with the requirements and design
  of the software item; 2. there is internal consistency between
  software units; 3. the code is traceable to design and requirements,
  testable, correct, and in conformity to software requirements and coding
  standards; 4. the code that is not traced to the units is justified;
  5. the code implements proper events sequences, consistent interfaces,
  correct data and control flow, completeness, appropriate allocation
  of timing and sizing budgets; 6. the code implements
  safety, security, and other critical requirements correctly as shown by
  appropriate methods; 7. the code is implemented in a way that it cannot
  result in runtime errors; 8. the effects of any residual runtime errors
  are controlled through error handling." {§5.8.3.5a}

  * SPARK (and GNATprove) help meet criterion 1.
  * Ada's strong typing and interface checks (and thus GNAT Pro for Ada)
    help meet criterion 2.
  * For criterion 3, the Defects and Vulnerability Analyzer (see
    :ref:`Space_Systems_SW_Defects_and_Vulnerability_Analyzer`) in
    the GNAT Static Analysis Suite and/or SPARK Pro can help verify
    correctness, and the GNATcheck utility included in the GNAT
    Static Analysis Suite (see :ref:`Space_Systems_SW_GNATcheck`)
    can enforce conformance with a coding standard.
  * For criterion 5, Ada's strong typing and interface checks, as well as
    SPARK and GNATprove, can help show consistent interfaces and correct
    data flow.
  * The Defects and Vulnerability Analyzer (see
    :ref:`Space_Systems_SW_Defects_and_Vulnerability_Analyzer`) in
    the GNAT Static Analysis Suite, SPARK Pro, and the standard
    semantic checks performed by the GNAT Pro compiler can help meet
    criterion 6.
  * The GNAT Static Analysis Suite and SPARK / GNATprove can statically
    detect potential run-time errors and thereby help meet criterion 7.
  * Ada's exception handling facility can help meet criterion 8.

* "The supplier shall verify that the following code coverage is achieved:

  .. csv-table::
      :file: table-source-coverage-vs-criticality.csv
      :header-rows: 1
      :widths: 50, 10, 10, 10, 10

  Note: 'TBA' means that the value is to be agreed with the customer and
  measured as per |Q-ST-80C| clause 6.3.5.2." {§5.8.3.5b}

  * The GNATcoverage tool (see :ref:`Space_Systems_SW_GNATcoverage`)
    in the GNAT Dynamic Analysis Suite can help meet this
    requirement.

* "The supplier shall measure code coverage by analysis of the results of
  the execution of tests." {§5.8.3.5c}

  * The GNATcoverage tool (see :ref:`Space_Systems_SW_GNATcoverage`)
    in the GNAT Dynamic Analysis Suite can help meet this
    requirement.

* "If it can be justified that the required percentage cannot be achieved
  by test execution, then analysis, inspection or review of design shall be
  applied to the non-covered code." {§5.8.3.5d}

  * The GNATcoverage tool (see :ref:`Space_Systems_SW_GNATcoverage`)
    in the GNAT Dynamic Analysis Suite can help meet this
    requirement.

* "In case the traceability between source code and object code cannot be
  verified, the supplier shall perform
  additional code coverage analysis on object code level as follows:

  .. csv-table::
      :file: table-object-coverage-vs-criticality.csv
      :header-rows: 1
      :widths: 50, 10, 10, 10, 10

  Note: 'N/A' means not applicable.

  Note: The use of some compiler optimization options can make the
  traceability between source code and object code not possible." {§5.8.3.5e}

  * The GNATcoverage tool (see :ref:`Space_Systems_SW_GNATcoverage`)
    in the GNAT Dynamic Analysis Suite can help meet this
    requirement.

  * AdaCore can prepare an analysis of traceability between source and object
    code; the company has provided this to customers in connection with
    certification under the DO-178C/ED-12C standard for airborne software for
    the commercial aviation industry.

* "The supplier shall verify source code robustness. AIM: use static analysis
  for the errors that are difficult to detect at run-time." {§5.8.3.5f}

  * Errors such as division by zero, null pointer dereferencing, array
    indices out of bounds, and many others are flagged at run-time by
    raising an exception. Effective practice is to keep these checks
    enabled during development and then, after verifying either statically
    or through sufficient testing that the run-time checks are not needed,
    disable the checks in the final code for maximal efficiency.

  * The Defects and Vulnerability Analyzer (see
    :ref:`Space_Systems_SW_Defects_and_Vulnerability_Analyzer`) in
    the GNAT Static Analysis Suite will detect such errors as well as
    many others, including suspicious constructs that, although
    legitimate Ada, are likely logic errors.

  * SPARK Pro will enforce the SPARK subset and can be used to
    demonstrate absence of run-time errors.

  * The GNATstack tool in GNAT Pro computes the potential maximum stack usage for
    each task in a program. Combining the result with a separate analysis
    showing the maximal depth of recursion, the developer can allocate
    sufficient stack space for program execution and prevent stack overflow.

Schedulability analysis for real-time software {§5.8.3.11}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Ravenscar profile

* "As part of the verification of the software requirements and architectural
  design, the supplier shall use an analytical model to
  perform a schedulability analysis and prove that the design is feasible."
  {§5.8.3.11a}

  * The Ada Ravenscar profile restricts the tasking model to enable precise
    schedulability analysis, including Rate-Monotonic Analysis (RMA).

.. index:: single: ECSS-E-ST-40C compliance; §5.9 Software operation process

Software operation process {§5.9}
---------------------------------

Process implementation {§5.9.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Problem handling procedures definition {§5.9.2.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The SOS [Software Operation Support] entity shall establish procedures for
  receiving, recording, resolving, tracking problems, and providing feedback."
  {§5.9.2.3a}

  * In the event that a product problem is due to a defect in an AdaCore tool
    (e.g., a code generation bug), AdaCore has a rigorous QA process for
    responding to and resolving such issues. The "sustained branch" service,
    which is included with a GNAT Pro Assurance subscription, helps by
    ensuring that a specific version of the toolchain is maintained over
    the lifetime of the supplier's project.

* "The SOS entity shall ensure that information regarding problems that can
  have an impact on security is protected." {§5.9.2.3b}

  * AdaCore's internal processes for maintaining sensitive data help to meet
    this criterion.

Software operation support {§5.9.4}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Problem handling {§5.9.4.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "Encountered problems shall be recorded and handled in accordance with the
  applicable procedures." {§5.9.4.2a}

  * As described above in connection with clause 5.9.2.3, AdaCore's QA
    process, and more specifically the GNAT Pro Assurance sustained branch
    service with its management of Known Problems, can help meet this
    requirement when an issue arises that is due to an AdaCore tool.

Vulnerabilities in operations {§5.9.4.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "During operations, security vulnerabilities, threats and exploits shall be:
  1. continuously monitored; 2. subject to further security analysis when
  evaluated relevant to the security of the system; 3. maintained for auditing
  purposes even when evaluated not relevant to the security of the system."
  {§5.9.4.3a}

  * The ability to express security-related requirements as contracts in the
    Ada source code, with run-time checks when needed, helps to meet
    criterion 1.

User support §5.9.5
~~~~~~~~~~~~~~~~~~~

Provisions of work-around solutions {§5.9.5.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "If a reported problem has a temporary work-around solution before a
  permanent solution can be released, the SOS entity shall give to the
  originator of the problem report the option to use it." {§5.9.5.3a}

  * As part of the GNAT Pro Assurance sustained branch service, AdaCore
    can supply work-arounds to critical problems prior to releasing
    a permanent solution.

.. index:: single: ECSS-E-ST-40C compliance; §5.10 Software maintenance process

Software maintenance process {§5.10}
------------------------------------

Process implementation {§5.10.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Long term maintenance for flight software {§5.10.2.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The maintainer shall propose
  solutions to be able to implement and upload modifications to the spacecraft
  up to its end of life." {§5.10.2.2a}

  * AdaCore's "sustained branch" service, which is included with a GNAT Pro
    Assurance subscription, in effect means that the compilation environment
    will receive support and not become obsolescent.

Modification implementation {§5.10.4}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Invoking of software engineering processes for modification implementation {§5.10.4.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The maintainer shall apply the software engineering processes specified
  in clauses 5.3 to 5.8 and 5.11 that are relevant to the scope of the
  modifications, using the tailoring applied during the development of the
  software." {§5.10.4.3a}

  * The Ada and SPARK languages have specific features that support the design
    of modular, maintainable software with high cohesion and low coupling.
    These include encapsulation (private types, separation of specification
    from implementation), hierarchical child libraries, and object-oriented
    programming (tagged types). By exploiting these features and utilizing
    GNAT Pro for Ada and SPARK Pro, the developer
    can localize the impact of maintenance changes.

  * The GNAT Static and Dynamic Analysis Suites can ensure that any modifications
    meet the verification criteria applicable to the original software.

.. index:: single: ECSS-E-ST-40C compliance; §5.11 Software security process

Software security process {|sect| 5.11}
---------------------------------------

Process implementation {|sect| 5.11.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* "A software security management plan shall be produced documenting: ...
  7. the tools, methods and procedures to be used...." {|sect| 5.11.2a}

  * The Ada and SPARK languages, GNAT Pro for Ada, the GNAT Static and
    Dynamic Analysis Suites, and the SPARK Pro toolset support the
    software security management plan.

.. _Space_Systems_SW_Software_security_analysis:

Software security analysis {|sect| 5.11.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* "The methods to be used for the security analysis shall be identified
  as part of the planning of the project." {|sect| 5.11.3b}

  * The Ada and SPARK languages, GNAT Pro for Ada, the GNAT Static and
    Dynamic Analysis Suites, and the SPARK Pro toolset help meet this
    requirement. For example:

    * Ada's compile-time checks prevent unsafe practices such as
      treating an integer value as a pointer.

    * The Defects and Vulnerability Analyzer in the GNAT Static Analysis
      Suite can detect a number of dangerous software errors in the
      MITRE Corporation's Common Weakness Enumeration.

    * The SPARK language and SPARK Pro can enforce security-related
      properties such as correct information flows and absence of
      run-time errors.

    * GNATfuzz can be used to stress test the software with malformed
      input values.

Security activities in the software life cycle {|sect| 5.11.5}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Security in the requirements baseline {|sect| 5.11.5.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The security assurance requirements shall determine the type and extent
  of security verification and validation activities, including testing,
  to be conducted.... Security verification and validation activities can
  include, for example, fuzzing tests...." {|sect| 5.11.5.1c}

  * The Ada and SPARK languages, GNAT Pro for Ada, the GNAT Static and
    Dynamic Analysis Suites, and the SPARK Pro toolset help meet this
    requirement.

Security in the detailed design and implementation engineering {|sect| 5.11.5.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The software security analysis shall be used during verification and
  validation activities to evaluate iteratively residual vulnerabilities and
  to reassess security risks." {|sect| 5.11.5.3b}

  * The Ada and SPARK languages, the GNAT Static and Dynamic Analysis
    Suites, and the SPARK Pro toolset help meet this requirement. For
    examples, see :ref:`Space_Systems_SW_Software_security_analysis`.

.. index:: single: ECSS-E-ST-40C compliance; Annex U: Software code verification

Software code verification {Annex U (informative)}
--------------------------------------------------

* "The following checks are expected to be performed on the software code:

  1. the code implements numerical protection mechanisms (e.g. against
     overflow and underflow, division by zero);

  2. the code does not perform out of bounds accesses - e.g. underrun or
     overrun of buffers, arrays or strings;

  3. the code does not include any infinite loop other than the main loop of
     the software and the main loops of cyclic tasks;

  4. the code appropriately uses arithmetical and logical operators (e.g.
     arithmetical OR vs. logical OR);

  5. implicit type conversions do not lead to arithmetical errors;

  6. the lifetime of variables is consistent with their use;

  7. the code makes proper use of static/global functions/variables to enforce
     the correct level of visibility;

  8. the code makes proper use of volatile variables for all variables that can
     be modified asynchronously (e.g. hardware access, memory-mapped
     I/O);

  9. the code does not perform invalid memory accesses (e.g. NULL
     dereferences);

  10. the code does not access uninitialized variables;

  11. the code does not perform unused assignments, unless this is done to
      trigger HW side-effects;

  12. there are no memory leaks;

  13. pointer arithmetic is justified and types of operands are consistent;

  14. the code does not lead to race conditions." {|sect| U.2}

:numref:`Space_Systems_SW_Verification-Support` shows how AdaCore's
technologies help meet these requirements:

.. _Space_Systems_SW_Verification-Support:

.. table:: Verification Support
    :align: center
    :widths: 17 65 18

    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | Verification check #    | Technology                                                    | Explanation                            |
    +=========================+===============================================================+========================================+
    | 1                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Run-time check [#rtcheck]_             |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | SPARK language, SPARK Pro                                     | Static check                           |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 2                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Run-time check [#rtcheck]_             |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | SPARK language, SPARK Pro                                     | Static check                           |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 3                       | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | GNATcheck (in GNAT Static Analysis Suite)                     | User-defined rule                      |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | SPARK language, SPARK Pro                                     | Static check                           |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 4                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Static check                           |
    |                         | SPARK language, SPARK Pro                                     |                                        |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | GNATcheck (in GNAT Static Analysis Suite)                     | User-defined rule                      |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 5                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Static check                           |
    |                         | SPARK language, SPARK Pro                                     |                                        |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 6                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Static check                           |
    |                         | SPARK language, SPARK Pro                                     |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 7                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Static check                           |
    |                         | SPARK language, SPARK Pro                                     |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 8                       | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 9                       | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Run-time check [#rtcheck]_             |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | SPARK language, SPARK Pro                                     | Static check                           |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 10                      | SPARK language, SPARK Pro                                     | Static check                           |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 11                      | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 12                      | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | Ada Language, GNAT Pro for Ada, GNAT Pro Assurance            | Run-time check [#memleak]_             |
    |                         +---------------------------------------------------------------+----------------------------------------+
    |                         | GNATstack (for preventing stack overflow)                     | Static check                           |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 13                      | Ada language, GNAT Pro for Ada, GNAT Pro Assurance            | Static check                           |
    |                         | SPARK language, SPARK Pro                                     |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+
    | 14                      | Defects and Vulnerability Analyzer                            | Static check [#stcheck]_               |
    |                         | (in GNAT Static Analysis Suite)                               |                                        |
    +-------------------------+---------------------------------------------------------------+----------------------------------------+

.. [#rtcheck] Depending on the source code, the GNAT Pro compiler may be
   able to guarantee that the check will always succeed and can thus avoid
   generating run-time code to detect violations.

.. [#stcheck] The static checks can result in false alarms
   ("false positives"), but the user can calibrate the tool to control the
   tradeoff between soundness (no false negatives) and precision (minimization
   of false positives).

.. [#memleak] Exhausting dynamic memory raises the :ada:`Storage_Error`
   exception at run time. Ada does not require a general garbage collector for
   storage reclamation, but several techniques can be used to prevent storage
   leaks:

   * Ensure, through analysis or testing, that dynamic allocations occur only
     during startup ("elaboration time") and not thereafter.
   * Reclaim storage explicitly through Ada's :ada:`Unchecked_Deallocation`
     and ensure, through analysis or testing, that this does not create
     dangling references.
   * Define a memory pool for a pointer type ("access type") so that allocations
     of objects for that type only use that storage area, and through analysis
     or testing demonstrate that the pool is never exhausted. Ada's memory pool
     facility can be used to implement a reference counting strategy (for
     non-cyclic data structures) with automatic reclamation.

Compliance Summary
------------------

:numref:`Space_Systems_SW_ECSS-E-ST-40C-Compliance-Support` shows how
AdaCore's technologies support the requirements in ECSS-E-ST-40C:

.. _Space_Systems_SW_ECSS-E-ST-40C-Compliance-Support:

.. table:: Technology Support for ECSS-E-ST-40C Compliance
    :align: left

    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | Technology                  | |sect|\ 5.4 | |sect|\5.5 | |sect|\ 5.6 | |sect|\ 5.7 | |sect|\ 5.8 | |sect|\ 5.9 | |sect|\ 5.10 | |sect|\ 5.11 | Annex U |
    +=============================+=============+============+=============+=============+=============+=============+==============+==============+=========+
    | Ada language                | |check|     | |check|    |             |             | |check|     | |check|     | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | SPARK language              | |check|     | |check|    |             |             | |check|     |             | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | GNAT Pro for Ada            | |check|     | |check|    | |check|     | |check|     | |check|     |             | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | SPARK Pro                   | |check|     | |check|    | |check|     |             | |check|     |             | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | GNAT Static Analysis Suite  |             | |check|    | |check|     |             | |check|     |             | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | GNAT Dynamic Analysis Suite |             | |check|    | |check|     |             | |check|     |             | |check|      | |check|      |         |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
    | GNAT Pro Assurance          | |check|     | |check|    | |check|     | |check|     | |check|     | |check|     | |check|      | |check|      | |check| |
    +-----------------------------+-------------+------------+-------------+-------------+-------------+-------------+--------------+--------------+---------+
