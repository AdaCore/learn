.. include:: ../../../global.txt

.. _Space_Systems_SW_Compliance_with_ECSS-Q-ST-80C:

Compliance with ECSS-Q-ST-80C
==============================

The |Q-ST-80C| standard defines software product assurance requirements
for the development and maintenance of space software systems. This
chapter explains how AdaCore's products can help a supplier meet many
of these requirements. The section numbers in braces refer to the
relevant content in |Q-ST-80C|.

.. index:: single: ECSS-Q-ST-80C compliance; §5 Software product assurance programme implementation

Software product assurance programme implementation {§5}
--------------------------------------------------------

Software product assurance programme management {§5.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Quality requirements and quality models {§5.2.7}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "Quality models shall be used to specify the software quality
  requirements" {§5.2.7.1a}

  * The GNATmetric tool (see :ref:`Space_Systems_SW_GNATmetric`) in
    the GNAT Static Analysis Suite can show quality metrics related
    to the source code structure.

  * The GNATdashboard IDE tool (see
    :ref:`Space_Systems_SW_GNATdashboard`) can display software
    quality data.

Tools and supporting environment {§5.6}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Methods and tools {§5.6.1}
^^^^^^^^^^^^^^^^^^^^^^^^^^

* "Methods and tools to be used for all activities of the development cycle
   ... shall be identified by the supplier and agreed by the customer"
   {§5.6.1.1a}

  * The GNAT Pro for Ada environment and any supplemental tools that are
    selected (e.g., GNAT Static and Dynamic Analysis Suites, SPARK Pro)
    should be listed.

* "The choice of development methods and tools shall be justified by
  demonstrating through testing or documented assessment that ... the tools
  and methods are appropriate for the functional and operational
  characteristics of the product, and ... the tools are available (in an
  appropriate hardware environment) throughout the development and
  maintenance lifetime of the product,
  ... the tools and methods are appropriate to the security sensitivity
  of the product as determined by the security analysis and as defined
  in the software security management plan" {§5.6.1.2a}

  * AdaCore can make available a variety of documentation showing
    that the selected tools are "appropriate for the functional and
    operational characteristics of the product", ranging from user
    manuals to qualification material relative to other
    high-assurance software standards such as |do-178c|
    :footcite:p:`Space_SW_RTCA_EUROCAE_2011a` and |en-50128|
    :footcite:p:`Space_SW_CENELEC_2020b`.

  * AdaCore's "sustained branch" service for its GNAT Pro for Ada
    Assurance product (see
    :ref:`Space_Systems_SW_Sustained_Branches`) can guarantee that
    the toolchain is maintained throughout the product lifetime.

  * AdaCore's SPARK Pro environment (see
    :ref:`Space_Systems_SW_Static_Verification_SPARK_Pro`) can be
    used to demonstrate security properties such as correct
    information flows and, for software at the highest security
    levels, compliance with formally specified requirements.

Development environment selection {§5.6.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The software development environment shall be selected according to the
  following criteria: 1. availability; 2. compatibility; 3. performance;
  4. maintenance; 5. durability and technical consistency with the operational
  environment; 6. the assessment of the product with respect to requirements,
  including the criticality category; 7. the available support documentation;
  8. the acceptance and warranty conditions; 9. the conditions of
  installation, preparation, training and use; 10. the maintenance conditions,
  including the possibilities of evolutions; 11. copyright and intellectual
  property rights constraints; 12. dependence on one specific supplier;
  13. the assessment of the product with respect to the security sensitivity
  level of the products; 14. compliance with appropriate security requirements
  due to organizational or national security regulations, policies or
  directives."
  {§5.6.2.1a}

  * AdaCore tools directly satisfy these criteria. The availability of
    qualification material for specific tools (GNATcheck,
    GNATprove, GNATstack) contributes to criterion 6, and the
    "sustained branch" service for GNAT Pro for Ada Assurance supports
    criteria 4, 7 and 10. AdaCore tools come with source code and
    flexible licensing, mitigating the issues noted in criteria 11 and 12.
    The GNAT Static Analysis Suite, SPARK, and the GNATfuzz tool in
    the GNAT Dynamic Analysis Suite facilitate
    demonstrating the relevant security properties (criteria 13 and 14).

.. index:: single: ECSS-Q-ST-80C compliance; §6 Software process assurance

Software process assurance {§6}
-------------------------------

Requirements applicable to all software engineering processes {§6.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Handling of critical software {§6.2.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall define, justify and apply measures to assure the
  dependability and safety of critical software.... These measures can
  include: ... use of a 'safe subset' of programming language; use of
  formal design language for formal proof; 100% code branch coverage
  at unit testing level; ... removing deactivated code or showing through
  a combination of analysis and testing that the means by which such code
  can be inadvertently executed are prevented, isolated, or eliminated;
  use of dynamic code verification techniques."
  {§6.2.3.2a}

  * Ada's pragma :ada:`Restrictions` and pragma :ada:`Profile`,
    together with the GNAT Static Analysis Suite tool GNATcheck, can
    enforce a coding standard for Ada (in effect a 'safe
    subset'). See :ref:`Space_Systems_SW_GNAT_Pro_Enterprise` and
    :ref:`Space_Systems_SW_GNATcheck`.

  * The SPARK language serves as a safe subset of full Ada, and a formal
    design language for formal proof.

  * SPARK Pro uses proof technology that can demonstrate a program's
    conformance with formally specified requirements.

  * GNATcoverage can report code coverage up to MC/DC at the source level.

  * The Defects and Vulnerability Analyzer in the GNAT Static Analysis Suite
    can detect unreachable code, including deactivated code.

  * The Ada language and the GNAT Pro Ada development environment allow
    contracts to be verified either statically or with run-time checks and
    thus facilitate dynamic code verification.

Verification {§6.2.6}
^^^^^^^^^^^^^^^^^^^^^

* "The completion of actions related to software problem reports generated
  during verification shall be verified and recorded." {§6.2.6.4a}

  * GNAT Pro for Ada Assurance, and its Sustained Branch service, help
    support compliance.

* "Software containing deactivated code shall be verified specifically to
  ensure that the deactivated code cannot be activated or that its accidental
  activation cannot harm the operation of the system." {§6.2.6.5a}

  * The Defects and Vulnerability Analyzer in the GNAT Static Analysis Suite
    can detect unreachable code, including deactivated code.

Software security {§6.2.9}
^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall identify the methods and techniques for the software
  security analysis." {|sect| 6.2.9.3a}

  * The Ada and SPARK languages, GNAT Pro for Ada, the GNAT Static and Dynamic
    Analysis Suites, and the SPARK Pro toolset help meet this requirement.

* "Based on the results of the software security analysis, the supplier shall
  apply engineering measures to reduce the number of security sensitive
  software components and mitigate the risks associated with security
  sensitive software." {|sect| 6.2.9.4a}

  * Code modularization in Ada and SPARK can be used to minimize the number
    of security sensitive components, and to localize and encapsulate them
    in modules (packages) with compile-time enforcement of data consistency.
    Checks performed by the GNAT Pro and SPARK Pro tools help mitigate
    security risks, while hybrid verification can combine formal analysis
    (for high security components in SPARK) with traditional testing methods.


Handling of security sensitive software {|sect| 6.2.10}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall define and implement measures to avoid propagation
  of failures, including the ones caused by deliberate action, between
  software components." {|sect| 6.2.10.1a}

  * The Ada language provides features that support this requirement
    for components in the same address space.
    Failures can be modeled by exceptions, and software
    components that need to interact synchronously or asynchronously
    can be modeled by tasks. (A failure in one component may be due to a
    device malfunction and is not necessarily security related, but it
    needs to be handled in a way that does not compromise security or
    other mission requirements.) If the failure (exception) occurs in an Ada
    task, then an appropriate style is for the task to take corrective
    measures by handling that exception and/or to report the failure so that
    a separate component can take further actions. If the task terminates as
    a result of the exception, the effect is localized; the failure that
    triggered the termination does not propagate to other components.

  * The SPARK language and SPARK Pro can help by ensuring that failures
    do not occur (proving the Absence of Run-Time Errors).


Requirements applicable to individual software engineering processes or activities {§6.3}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Coding {§6.3.4}
^^^^^^^^^^^^^^^

* "Coding standards (including security, consistent naming conventions and
  adequate commentary rules) shall be specified and observed." {§6.3.4.1a}

  * Ada's :ada:`Restrictions` and :ada:`Profile` pragmas, together
    with AdaCore’s GNATcheck tool (see section
    :ref:`Space_Systems_SW_GNATcheck`), can define and enforce a
    coding standard.

  * The SPARK language can serve as a coding standard.

* "The tools to be used in implementing and checking conformance with coding
  standards shall be identified in the product assurance plan before coding
  activities start." {§6.3.4.3a}

  * GNATcheck and SPARK Pro are relevant tools for this activity.

* "The supplier shall define measurements, criteria and tools to ensure that
  the software code meets the quality and security requirements." {§6.3.4.6a}

  * The GNATmetric tool (see :ref:`Space_Systems_SW_GNATmetric`) can
    be used to report quality data related to the source code
    structure.

* "Synthesis of the code analysis results and corrective actions implemented
  shall be described in the software product assurance reports." {§6.3.4.7a}

  * GNATdashboard (see :ref:`Space_Systems_SW_GNATdashboard`) can be
    used to synthesize and summarize code quality metrics.

Testing and validation {§6.3.5}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "Testing shall be performed in accordance with a strategy for each testing
  level (i.e. unit, integration, validation against the technical
  specification, validation against the requirements baseline, acceptance),
  ...." {§6.3.5.1a}

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement for unit testing.

* "Based on the dependability and safety criticality, and the security
  sensitivity of the software, test coverage goals for each
  testing level shall be agreed between the customer and the supplier and
  their achievement monitored by metrics ...." {§6.3.5.2a)

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement.

* "Test coverage shall be checked with respect to the stated goals."
  {§6.3.5.5a}

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement.

* "The test coverage of configurable code shall be checked to ensure that the
  stated requirements are met in each tested configuration." {§6.3.5.7a}

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement.

* "Test tool development or acquisition ... shall be planned for in the
  overall project plan." {§6.3.5.24a}

  * The tools in AdaCore's GNAT Dynamic Analysis Suite (GNATtest,
    GNATcoverage, GNATfuzz) are candidates for consideration in test tool
    acquisition.

* "Software containing deactivated code shall be validated specifically to
  ensure that the deactivated code cannot be activated or that its accidental
  activation cannot harm the operation of the system." {§6.3.5.30a}

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement by detecting non-covered code that is
    intended (and can be categorized) as deactivated.

* "Software containing configurable code shall be validated specifically to
  ensure that unintended configuration cannot be activated at run time or
  included during code generation". {§6.3.5.31a}

  * AdaCore's GNAT Dynamic Analysis Suite (GNATtest, GNATcoverage, GNATfuzz)
    can help meet this requirement by detecting configurable code that,
    because of incorrect configuration settings, was unintentionally included
    during code generation.

Maintenance {§6.3.9}
^^^^^^^^^^^^^^^^^^^^

* "The maintenance plans and procedures shall include the following as a
  minimum: 1. scope of maintenance; 2. identification of the first version of the
  software product for which maintenance is to be done; 3. support organization;
  4. maintenance life cycle; 5. maintenance activities; 6. quality measures to be
  applied during the maintenance; 7. security measures to be applied during the
  maintenance. 8. maintenance records and reports."
  {§6.3.9.4a}

  * The "sustained branch" service of AdaCore's GNAT Pro Assurance product
    can help meet this requirement.

* "Maintenance records shall be established for each software product ...."
  {§6.3.9.7a}

  * AdaCore's ticket system, which is part of the standard support in all
    product subscriptions, provides an audit trail for problem reports /
    resolution.
  * The "sustained branch" service includes special maintenance
    accommodation for dealing with problems that relate to software safety.

.. index:: single: ECSS-Q-ST-80C compliance; §7 Software product quality assurance

Software product quality assurance {§7}
---------------------------------------

Product quality objectives and metrication {§7.1}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assurance activities for product quality requirements {§7.1.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The supplier shall define assurance activities to ensure that the product
  meets the quality requirements as specified in the technical
  specification" {§7.1.3a}

  * Any of AdaCore's tools could potentially contribute to meeting this
    requirement, depending on the nature of the metrics that have been
    defined, and the GNATdashboard tool can integrate the metrics
    in a meaningful way.

  * Use of the SPARK language, and formal demonstration of absence of
    runtime errors, support this requirement.

Basic metrics {§7.1.5}
^^^^^^^^^^^^^^^^^^^^^^

* "The following basic products metrics shall be used: size (code); complexity
  (design, code); fault density and failure intensity; test coverage; number
  of failures." {§7.1.5a}

  * The GNATmetric, GNATtest, and GNATcoverage tools in the GNAT Dynamic Analysis
    Suite directly help to meet this requirement.

Product quality requirements {§7.2}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Design and related documentation {§7.2.2}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "The software shall be designed to facilitate testing." {§7.2.2.2a}

  * The Ada language encourages and supports the use of sound software
    engineering principles such as modular design and structured programming,
    which makes the code easier to test.

  * The contract facility in Ada and SPARK supports verification by both
    static analysis and testing.

* "Software with a long planned lifetime shall be designed with minimum
  dependency on the operating system and the hardware in order to aid
  portability." {§7.2.2.3a}

  * The Ada language has abstracted away the specifics of the underlying
    operating system and hardware through standard syntax and semantics for
    features such as concurrency, memory management, exception handling,
    and I/O. As a result, Ada programs can often be ported across different
    processor architectures and operating systems by simply recompiling,
    with minimal or no source code changes needed.

Test and validation documentation {§7.2.3}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* "Test procedures, data and expected results shall be specified."
  {|sect| 7.2.3.4a}

  * The GNATtest and GNATfuzz tools in the GNAT Dynamic Analysis Suite
    help meet this requirement.

* "For any requirements not covered by testing a verification report shall be
  drawn up documenting or referring to the verification activities performed."
  {§7.2.3.6a}

  * In many cases where verification cannot be achieved by testing,
    SPARK Pro may be able to provide convincing alternative verification
    evidence (for example, a robustness demonstration by proof that an
    out-of-range or otherwise non-valid input will never be passed to
    the unit being verified).


Compliance Summary
------------------

:numref:`Space_Systems_SW_ECSS-Q-ST-80C-Compliance-Support` shows how
AdaCore's technologies support the requirements in ECSS-Q-ST-80C:

.. _Space_Systems_SW_ECSS-Q-ST-80C-Compliance-Support:

.. table:: Technology Support for ECSS-Q-ST-80C Compliance
    :align: left

    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | Technology                  | |sect|\ 5.2 | |sect|\5.6  | |sect|\ 6.2 | |sect|\ 6.3 | |sect|\ 7.1 | |sect|\ 7.2 |
    +=============================+=============+=============+=============+=============+=============+=============+
    | Ada language                | |blankcell| | |blankcell| | |check|     | |check|     | |blankcell| | |check|     |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | SPARK language              | |blankcell| | |check|     | |check|     | |blankcell| | |check|     | |blankcell| |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | GNAT Pro for Ada            | |blankcell| | |check|     | |check|     | |blankcell| | |check|     | |blankcell| |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | SPARK Pro                   | |blankcell| | |check|     | |check|     | |blankcell| | |check|     | |check|     |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | GNAT Static Analysis Suite  | |blankcell| | |check|     | |check|     | |check|     | |check|     | |blankcell| |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | GNAT Dynamic Analysis Suite | |blankcell| | |check|     | |check|     | |check|     | |check|     | |check|     |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | GNAT Pro Assurance          | |blankcell| | |check|     | |check|     | |check|     | |check|     | |blankcell| |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+
    | GNAT Pro IDEs               | |check|     | |blankcell| | |blankcell| | |check|     | |check|     | |blankcell| |
    +-----------------------------+-------------+-------------+-------------+-------------+-------------+-------------+


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
