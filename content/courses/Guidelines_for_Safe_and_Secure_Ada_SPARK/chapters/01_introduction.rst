**************
Introduction
**************

.. include:: ../../global.txt

Ada is a general purpose, high-level programming language designed to support
the construction of long-lived, highly-reliable applications.  Like all
general-purpose languages, only a subset of the full language is appropriate
for safety-critical applications because the full language includes facilities
that are difficult to analyze and verify to the degree required.  This document
facilitates identification of subsets appropriate for the highest levels of
integrity, including safety-critical applications.

SPARK is a statically verifiable subset of Ada designed specifically for the
most critical applications. Ada constructs not amenable to verification are
precluded, such as arbitrary use of access types and full tasking. SPARK is
also a superset of Ada, with additional contracts for specifying and verifying
programs. Many of the guidelines (and more) are implicit in the design of
SPARK.

Therefore, this document defines guidelines for the development of
high-integrity, safety-critical applications in either the Ada or SPARK
programming languages, or both (because the two can be mixed).

=======
Scope
=======

This document provides guidelines for development decisions, both at the system
level and at the unit level, regarding the use of the programming languages Ada
and SPARK, as well as related tools, such as static analyzers and unit test
generators. It is not concerned with presentation issues such as naming, use of
whitespace, or the like.

===========
Structure
===========

Rather than defining a specific set of rules defining a single subset, this
document defines a set of criteria, in the form of guidelines, used by system
architects to identify project-specific subsets appropriate to a given project.

The guidelines are separated into related categories, such as storage
management, object-oriented programming, concurrency management, and so on.
Each guideline is in a separate table, specifying the rule name, a unique
identifier, and additional attributes common to each table.

=============
Enforcement
=============

Detection and enforcement mechanisms are indicated for each guideline. These
mechanisms typically consist of the application of a language standard pragma
named :ada:`Restrictions`, with policy-specific restriction identifiers given as
parameters to the pragma [AdaRM2016]_. Violations of the given restrictions are
then detected and enforced by the Ada compiler.

Alternatively, the AdaCore GNATcheck utility program has rules precisely
corresponding to those restriction identifiers, with the same degree of
detection and enforcement. For example, the language restriction identifier
No_Unchecked_Deallocation corresponds to the GNATcheck
**+RRestrictions:No_Unchecked_Deallocation** rule.

The advantage of GNATcheck over the compiler is that all generated messages
will be collected in the GNATcheck report that can be used as evidence of the
level of adherence to the coding standard. In addition, GNATcheck provides a
mechanism to deal with accepted exemptions.

In some cases the enforcement mechanism is the SPARK language and analyzer.
Many of the guidelines (and more) are implicit in the design of SPARK and are,
therefore, automatically enforced.

In some (very) rare cases the enforcement mechanism is manual program
inspection, although alternatives (e.g., SPARK) are usually available and
recommended. These guidelines are included because they are considered
invaluable in this domain.
