.. include:: ../../../global.txt

Technology Annex
================

This annex summarizes how AdaCore's tools and technologies support the various
techniques and measures defined in  Annex\ |nbsp|\ D of |en-50128|.
The qualification status for tools, and certifiability for run-time libraries,
are also noted.

.. index:: single: Ada language; Support for Annex D techniques (summary)

Ada Programming Language
------------------------

See :ref:`Railway_SW_Ada`.

Qualification
~~~~~~~~~~~~~

Although there is no qualification of a language *per se*, the Ada language
is standardized through an official process managed by an ISO committee,
IEC/ISO 8652.
AdaCore's Ada compilers and tools have reference and user documentation
that precisely describes the expected behavior, including the effects
of implementation-defined features.

Annex D References
~~~~~~~~~~~~~~~~~~

* D.2 Analyzable Programs
* D.4 Boundary Value Analysis
* D.14 Defensive Programming
* D.18 Equivalence Classes and Input Partition Testing
* D.24 Failure Assertion Programming
* D.33 Information Hiding / Encapsulation
* D.34 Interface Testing
* D.35 Language Subset
* D.38 Modular Approach
* D.49 Strongly Typed Programming Languages
* D.53 Structured Programming
* D.54 Suitable Programming Languages
* D.57 Object Oriented Programming
* D.60 Procedural Programming


GNAT Pro Assurance Toolsuite
----------------------------

.. index:: single: GNAT Pro Assurance; Qualification
.. index:: single: Tool qualification; GNAT Pro Assurance

Qualification
~~~~~~~~~~~~~

.. rubric:: GNAT Pro compiler family

The GNAT Pro compilers for Ada and for C are qualified at class T3.
AdaCore can provide documentation attesting to various aspects such as
service history, development standard, and testing results.
This documentation has been submitted and accepted in past certification
activities.
T3 qualification material can also be developed for the GNAT Pro for C++ and
GNAT Pro for Rust compilers.

Since compilers are large and complex pieces of software, bugs can be
detected (and subsequently corrected) after a particular version has been
chosen. Following the requirements stated in 6.7.4.11, however, a corrected
version of the compiler cannot be deployed without specific justification.
AdaCore offers a dedicated service |ndash| GNAT Pro Assurance |ndash| on a
specified version of the technology, which provides critical problem fixes
(or workaround suggestions) as well as detailed descriptions of the changes.
Using GNAT Pro Assurance, a customer can integrate a corrected version of
a specific compiler release into their development infrastructure
without the risk of regressions from unwanted updates.

See :ref:`Railway_SW_GNAT_Pro_Assurance`.

.. index:: single: GNATstack; Tool qualification
.. index:: single: Tool qualification; GNATstack

.. rubric:: GNATstack

GNATstack can be qualified as a class T2 tool.

.. index:: single: Light Profile; Certification material
.. index:: single: Light-Tasking Profile; Certification material

Run-Time Certification
~~~~~~~~~~~~~~~~~~~~~~

Certification material up to SIL 4 can be developed for the Light and
Light-Tasking run-time libraries.

See :ref:`Railway_SW_Configurable_Run-Time_Libraries`.

.. index:: single: GNAT Pro Assurance; Support for Annex D techniques (summary)

Annex D References
~~~~~~~~~~~~~~~~~~

* D.10 Data Flow Analysis
* D.15 Coding Standards and Style Guide
* D.18 Equivalence Classes and Input Partition Testing
* D.35 Language Subset


SPARK Language and Toolsuite
----------------------------

See :ref:`Railway_SW_SPARK`.

.. index:: single: SPARK Pro toolsuite; Qualification
.. index:: single: Tool qualification; SPARK Pro toolsuite

Qualification
~~~~~~~~~~~~~

The SPARK Pro toolsuite can be qualified at class T2.

.. index:: single: SPARK technology; Support for Annex D techniques (summary)

Annex D References
~~~~~~~~~~~~~~~~~~

The SPARK language and toolset can contribute to the deployment or
implementation of the following techniques:

* D.2 Analyzable Programs
* D.4 Boundary Value Analysis
* D.10 Data Flow Analysis
* D.14 Defensive Programming
* D.18 Equivalence Classes and Input Partition Testing
* D.24 Failure Assertion Programming
* D.28 Formal Methods
* D.29 Formal Proof
* D.34 Interface Testing
* D.35 Language Subset
* D.38 Modular Approach
* D.57 Object Oriented Programming

GNAT Static Analysis Suite
--------------------------

**This section will be completed after review of the content for the
various GNAT SAS tools. CodePeer is no longer a branded tool, and
in any event there is an issue with its qualification status
since it is not sound.**

See :ref:`Railway_SW_GNAT_Static_Analysis_Suite`.

Defects and Vulnerability Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**This section will be deleted (and re-titled), or else adapted as needed**

.. index:: single: Defects and Vulnerability Analysis; Qualification
.. index:: single: Tool qualification; Defects and Vulnerability Analysis


Qualification
^^^^^^^^^^^^^

GNAT SAS's defects and vulnerability analysis tool can be qualified at
class T2.
It has a long cross-industry track record and has been qualified under other
standards in the past, such as DO-178B/C as a verification tool/TQL5.

.. index:: single: Defects and Vulnerability Analysis; Support for Annex D techniques (summary)

Annex D References
^^^^^^^^^^^^^^^^^^

GNAT SAS's defects and vulnerability analysis tool can contribute to the
deployment or implementation of the following techniques:

* D.2 Analyzable Programs
* D.4 Boundary Value Analysis
* D.8 Control Flow Analysis
* D.10 Data Flow Analysis
* D.14 Defensive Programming
* D.18 Equivalence Classes and Input Partition Testing
* D.24 Failure Assertion Programming
* D.32 Impact Analysis

Basic Static Analysis tools
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic tools are GNATcheck and GNATmetric.

.. index:: single: GNATcheck; Qualification
.. index:: single: Tool qualification; GNATcheck
.. index:: single: GNATmetric; Qualification
.. index:: single: Tool qualification; GNATmetric

Qualification
^^^^^^^^^^^^^

These tools can be qualified at class T2.
GNATcheck has been qualified under other standards as well,
such as DO-178B/C as a verification tool/TQL5.

.. index:: single: GNATcheck; Support for Annex D techniques (summary)
.. index:: single: GNATmetric; Support for Annex D techniques (summary)

Annex D References
^^^^^^^^^^^^^^^^^^

* D.2 Analyzable Programs
* D.14 Defensive Programming
* D.15 Coding Standard and Style Guide
* D.35 Language Subset
* D.37 Metrics


GNAT Dynamic Analysis Suite
---------------------------

This suite comprises GNATtest, GNATemulator, GNATcoverage, GNATfuzz, and TGEN.

See :ref:`Railway_SW_GNAT_Dynamic_Analysis_Suite`.

.. index:: single: GNATtest; Qualification
.. index:: single: Tool qualification; GNATtest
.. index:: single: GNATemulator; Qualification
.. index:: single: Tool qualification; GNATemulator
.. index:: single: GNATcoverage; Qualification
.. index:: single: Tool qualification; GNATcoverage

Qualification
~~~~~~~~~~~~~

GNATtest, GNATemulator and GNATcoverage can be qualified at class T2.
GNATcoverage has been qualified under other standards as well,
such as DO-178B/C as a verification tool/TQL5.

.. index:: single: GNATtest; Support for Annex D techniques (summary)
.. index:: single: GNATemulator; Support for Annex D techniques (summary)
.. index:: single: GNATcoverage; Support for Annex D techniques (summary)

Annex D References
~~~~~~~~~~~~~~~~~~

* D.50 Structure Based Testing


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
