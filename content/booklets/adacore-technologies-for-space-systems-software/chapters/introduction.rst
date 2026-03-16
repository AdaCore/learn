.. include:: ../../../global.txt

Introduction
===============
Software for space applications must meet unique and formidable requirements.
Hard real-time deadlines, a constrained target execution environment with
limited storage capacity, and distributed functionality between ground and
on-board systems are some of the challenges, with little margin for error.
The software needs to work correctly from the outset, without safety or
security defects, and the source code needs to be amenable to maintenance
over the system’s lifetime (which may extend over decades) as requirements
evolve.

.. index:: European Cooperation for Space Standardization (ECSS)
.. index:: European Space Agency (ESA)

To provide a common approach to addressing these challenges, the European
Cooperation for Space Standardization (ECSS) was formed in the mid-1990s
in a joint effort conducted by the European Space Agency (ESA), individual
national space organizations, and industrial partners. As stated in
:footcite:p:`Space_SW_Kriedke_ElGammal_1995`:

..

  *The European Cooperation for Space Standardization (ECSS) is an initiative
  established to develop a coherent, single set of user-friendly standards
  for use in all European space activities.*

The resulting set of standards, available from the ECSS web portal
:footcite:p:`Space_SW_ECSS_HomePage`, addresses space activities as a
whole and complement the relevant country-specific standards.

The ECSS standards specify requirements that must be satisfied (although
project-specific tailoring is allowed) and fall into three categories:

* *Space engineering* (the "|ndash|\ E" series),

* *Space product assurance* (the "|ndash|\ Q" series), and

* *Space project management* (the "|ndash|\ M" series).

This document focuses on two specific standards:

* |E-ST-40C| Rev.\ |nbsp|\ 1 (Space engineering / Software)
  :footcite:p:`Space_SW_ECSS_2025a`, and

* |Q-ST-80C|\ |nbsp|\ Rev.\ |nbsp|\ 2 (Space product assurance /
  Software product assurance) :footcite:p:`Space_SW_ECSS_2025b`

and shows how the Ada and SPARK languages, together with AdaCore's
product and services offerings,
can help space software suppliers comply with these standards.
Unless noted otherwise, all references to |E-ST-40C| and |Q-ST-80C| in this
document relate to these cited editions of the standards.

AdaCore has a long and successful history supporting developers of space
software, and the company has proven experience and expertise in
qualification under |E-ST-40C| and |Q-ST-80C|. Examples include:

.. index:: Zero Footprint (ZFP) run-time library

* The ZFP (Zero Footprint) minimal run-time library for Ada on LEON2 ELF,
  qualified at criticality category B, for the aerospace company AVIO
  :footcite:p:`Space_SW_AdaCore_Web_2019a`.

.. index:: Ravenscar Small Footprint (SFP) run-time library

* The Ravenscar SFP (Small Footprint) QUAL run-time library for Ada on
  LEON2 and LEON3 boards, pre­qualified at criticality category B, for
  ESA :footcite:p:`Space_SW_AdaCore_Web_2019b`.

.. index:: Memory safety

An important update in the 2025 versions of |E-ST-40C| and |Q-ST-80C|
is the explicit attention paid to security issues. Memory-safe languages
like Ada and SPARK, and formal analysis tools such as SPARK Pro, help
reduce the effort in demonstrating security properties in space software.

The remainder of this chapter summarizes the |E-ST-40C| and |Q-ST-80C|
standards, and the subsequent chapters have the following content:

* :ref:`Programming_Languages_for_Space_Software`
  describes the Ada and SPARK programming languages and relates their software
  engineering support to the relevant sections / requirements in the two
  standards.

* Analogously, :ref:`Tools_for_Space_Software_Development` presents AdaCore's
  various software development and verification toolsuites and relates their
  functionality to the relevant sections / requirements in the two standards.

* In the other direction, :ref:`Compliance_with_ECSS-E-ST-40C`
  surveys the individual requirements in |E-ST-40C| and shows how a large
  number of them can be met by a software supplier through Ada, SPARK,
  and/or specific AdaCore products.

* :ref:`Compliance_with_ECSS-Q-ST-80C` does likewise for the requirements in
  |Q-ST-80C|.

* For ease of reference, the :ref:`Abbreviations` chapter contains a table of
  acronyms and initialisms used in this document, and bibliography lists
  the various resources cited.

Although this document is focused on specific ECSS standards, the
:ref:`Programming_Languages_for_Space_Software` and
:ref:`Tools_for_Space_Software_Development` chapters
explain how the Ada and SPARK languages / technologies and AdaCore's products
benefit software development in general for large-scale safety-critical
systems. These chapters may thus be applicable to software that has to comply
with regulatory standards in other domains.

.. index:: single: ECSS-E-ST-40C; Summary

ECSS-E-ST-40C: Space engineering / Software
-------------------------------------------

As stated in |E-ST-40C| (:footcite:p:`Space_SW_ECSS_2025a`, p. 11):

  "This Standard covers all aspects of space software engineering including
  requirements definition, design, production, verification and validation,
  transfer, operations and maintenance."

  "It defines the scope of the space software engineering processes and its
  interfaces with management and product assurance, which are addressed in the
  Management (|ndash|\ M) and Product assurance (|ndash|\ Q) branches of the
  ECSS System, and explains how they apply in the software engineering
  processes."

|E-ST-40C| defines the following space system software engineering processes:

* Software-related systems requirements process (|sect| 4.2.2)

  This process links the system and software levels and "establishes the
  functional and the performance requirements baseline (including the interface
  requirement specification) (RB) of the software development"
  (:footcite:p:`Space_SW_ECSS_2025a`, p. 27).

* Software management process (|sect| 4.2.3)

  This process "tailors the M standards for software-specific issues"
  and produces "a software development plan including the life cycle
  description, activities description, milestones and outputs, the
  techniques to be used, and the risks identification"
  (:footcite:p:`Space_SW_ECSS_2025a`, pp. 27, 28). It covers the joint
  review process, interface management, and technical budget and
  margin management.

* Software requirements and architecture engineering process (|sect| 4.2.4)

  This process comprises software requirements analysis (based on system
  requirements) and a resulting software architecture design. Activities
  associated with the latter include selection of a design method, selection
  of a computational model for real-time software, description of software
  behavior, development and documentation of the software interfaces, and
  definition of methods and tools for software intended for reuse.

* Software design and implementation engineering process (|sect| 4.2.5)

  This process covers the detailed design of the software items (including an
  analysis of the dynamic model showing how issues such as storage leakage and
  corrupted shared data are avoided), coding, testing, and integration.

* Software validation process (|sect| 4.2.6)

  Software validation entails "software product testing against both the
  technical specification and the requirements baseline" and "confirm[ing]
  that the technical specification and the requirements baseline functions
  and performances are correctly and completely implemented in the final
  product" (:footcite:p:`Space_SW_ECSS_2025a`, p. 29).

* Software delivery and acceptance process (|sect| 4.2.7)

  This process "prepares the software product for delivery and testing in its
  operational environment" (:footcite:p:`Space_SW_ECSS_2025a`, p. 29).

* Software verification process (|sect| 4.2.8)

  Software verification "confirm[s] that adequate specifications and inputs
  exist for every activity and that the outputs of the activities are
  correct and consistent with the specifications and inputs. This process
  is concurrent with all the previous processes."
  (:footcite:p:`Space_SW_ECSS_2025a`, p. 30)

* Software operation process (|sect| 4.2.9)

  This process involves the activities needed to ensure that the software
  remains operational for its users; these include "mainly the helpdesk and
  the link between the users, the developers or maintainers, and the
  customer." (:footcite:p:`Space_SW_ECSS_2025a`, p. 30)

* Software maintenance process (|sect| 4.2.10)

  This process "covers software product modification to code or associated
  documentation for correcting an error, a problem or implementing an
  improvement or adaptation." (:footcite:p:`Space_SW_ECSS_2025a`, p. 31)

* Software security process (|sect| 4.2.11)

  This process "is supported by a software security analysis that is
  systematically maintained at different points in the lifecycle of the
  software.... The software security analysis is used to ensure that
  security risks are properly addressed.... It is also used to assess
  and drive the design, implementation and operation of secure software."
  (:footcite:p:`Space_SW_ECSS_2025a`, p. 32)

The standard specifies the requirements associated with each of these
processes and defines the expected output for each requirement. The expected
output identifies three entities:

* the relevant destination file,
* the DRL (Document Requirements List) item(s) within that file where the
  requirement is addressed, and
* the review that will assess whether the requirement is met.

The files in question are the RB (Requirements Baseline), TS (Technical
Specification), DDF (Design Definition File), DJF (Design Justification File),
MGT (Management File), MF (Maintenance File), OP (Operational Plan), and PAF
(Product Assurance File).

The reviews are the SRR (System Requirements Review), PDR (Preliminary Design
Review), CDR (Critical Design Review), QR (Qualification Review),
AR (Acceptance Review), and ORR (Operational Readiness Review).

The tables below, derived from Table A-1 in Annex A of |E-ST-40C|, show the
association between files, DRL items, and reviews. Cells with "E" indicate
requirements from |E-ST-40C|, and cells with "Q" are the contributions from
|Q-ST-80C|.

.. To refer to a table by its number, use the :numref:`table-name` command,
.. and to refer to a table by its name, use the :ref:`table-name` command,
.. where table-name is as specified in the :name: option of the csv-table
.. For example: See :numref:`RB table`

.. csv-table:: **Relationship between RB (Requirements Baseline), DRL items, and reviews**
   :name:   RB table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software System Specification","E",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Interface requirements document (IRD)","E",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Safety and dependability analysis results for lower level suppliers", "E Q",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|

.. csv-table:: **Relationship between TS (Technical Specification), DRL items, and reviews**
   :name:   TS table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software requirements specification (SRS)",|blankcell|,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Software interface control document (ICD)",|blankcell|,E,E,|blankcell|,|blankcell|,|blankcell|


.. csv-table:: **Relationship between DDF (Design Definition File), DRL items, and reviews**
   :name:   DDF table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software design document (SDD)",|blankcell|,E,E,|blankcell|,|blankcell|,|blankcell|
   "Software configuration file (SCF)",|blankcell|,E,E,"E Q",E,"E Q"
   "Software release document (SRelD)",|blankcell|,|blankcell|,|blankcell|,E,E,|blankcell|
   "Software user manual (SUM)",|blankcell|,|blankcell|,E,E,E,|blankcell|
   "Software source code and media labels",|blankcell|,|blankcell|,E,|blankcell|,|blankcell|,|blankcell|
   "Software product and media labels",|blankcell|,|blankcell|,|blankcell|,E,E,E
   "Training material",|blankcell|,|blankcell|,|blankcell|,E,|blankcell|,|blankcell|

.. csv-table:: **Relationship between DJF (Design Justification File), DRL items, and reviews**
   :name:   DJF table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software verification plan (SVerP)",|blankcell|,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Software validation plan (SValP)",|blankcell|,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Independent software verification and validation plan","E Q",E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Software integration test plan (SITP)",|blankcell|,E,E,|blankcell|,|blankcell|,|blankcell|
   "Software unit test plan (SUTP)",|blankcell|,|blankcell|,E,|blankcell|,|blankcell|,|blankcell|
   "Software validation specification (SVS) with respect to TS",|blankcell|,|blankcell|,E,|blankcell|,|blankcell|,|blankcell|
   "Software validation specification (SVS) with respect to RB",|blankcell|,|blankcell|,|blankcell|,E,E,|blankcell|
   "Acceptance test plan",|blankcell|,|blankcell|,|blankcell|,E,E,|blankcell|
   "Acceptance test report",|blankcell|,|blankcell|,E,|blankcell|,|blankcell|,|blankcell|
   "Installation report",|blankcell|,|blankcell|,E,|blankcell|,|blankcell|,|blankcell|
   "Software verification report (SVR)",E,E,E,E,E,"E Q"
   "Independent software verification and validation report",|blankcell|,"E Q","E Q","E Q","E Q",E
   "Software reuse file (SRF)","E Q",E,E,|blankcell|,|blankcell|,|blankcell|
   "Software problems reports and nonconformance reports","E Q","E Q","E Q","E Q","E Q","E Q"
   "Joint review reports",E,E,E,E,E,|blankcell|
   "Justification of selection of operational ground equipment and support services","E Q","E Q",|blankcell|,|blankcell|,|blankcell|,|blankcell|

.. csv-table:: **Relationship between MGT (Management File), DRL items, and reviews**
   :name:   MGT table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software development plan (SDP)",E,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Software review plan (SRP)",E,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Software configuration management plan",E,E,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Training plan","E Q",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Interface management procedures",E,|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Identification of NRB SW and members","E Q",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Procurement data","E Q","E Q",|blankcell|,|blankcell|,|blankcell|,|blankcell|

.. csv-table:: **Relationship between MF (Maintenance File), DRL items, and reviews**
   :name:   MF table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Maintenance plan",|blankcell|,|blankcell|,|blankcell|,E,E,E
   "Maintenance records",Q,Q,Q,"E Q","E Q","E Q"
   "SPR and NCR- Modification analysis report- Problem analysis report- Modification documentation- Baseline for change - Joint review reports",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Migration plan and notification",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|
   "Retirement plan and notification",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|

.. csv-table:: **Relationship between OP (Operational Plan), DRL items, and reviews**
   :name:   OP table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software operation support plan",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,E
   "Operational testing results",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,E
   "SPR and NCR- User's request record- Post operation review report",|blankcell|,|blankcell|,|blankcell|,|blankcell|,|blankcell|,E

.. csv-table:: **Relationship between PAF (Product Assurance File), DRL items, and reviews**
   :name:   PAF table
   :header: "DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 58, 7, 7, 7, 7, 7, 7

   "Software product assurance plan (SPAP)","E Q","E Q","E Q","E Q","E Q","E Q"
   "Software product assurance requirements for suppliers","E Q",Q,Q,Q,Q,Q
   "Audit plan and schedule","E Q",Q,Q,Q,Q,Q
   "Review and inspection plans or procedures",Q,Q,Q,Q,Q,Q
   "Procedures and standards",Q,"E Q",Q,Q,Q,Q
   "Modelling and design standards","E Q","E Q",Q,Q,Q,Q
   "Coding standards and description of tools",Q,"E Q",Q,Q,Q,Q
   "Software problem reporting procedure",Q,"E Q",Q,Q,Q,Q
   "Software dependability and safety analysis report- Criticality classification of software components",Q,"E Q","E Q","E Q","E Q",Q
   "Software product assurance report",Q,Q,Q,Q,Q,Q
   "Software product assurance milestone report (SPAMR)","E Q","E Q","E Q","E Q","E Q","E Q"
   "Statement of compliance with test plans and procedures",Q,Q,"E Q","E Q","E Q","E Q"
   "Records of training and experience",Q,Q,Q,Q,Q,Q
   "(Preliminary) alert information",Q,Q,Q,Q,Q,Q
   "Results of preaward audits and assessments, and of procurement sources",Q,Q,Q,Q,Q,Q
   "Software process assessment plan",Q,Q,Q,Q,Q,Q
   "Software process assessment records",Q,Q,Q,Q,Q,Q
   "Review and inspection reports",Q,Q,Q,Q,Q,Q
   "Receiving inspection report","E Q","E Q","E Q","E Q",Q,Q
   "Input to product assurance plan for systems operation",Q,Q,Q,Q,Q,"E Q"

.. csv-table:: **ECSS-E-ST-40 and ECSS-Q-ST-80 Document requirements list (DRL)**
   :header: File,"DRL Item",SRR,PDR,CDR,QR,AR,ORR
   :widths: 10, 48, 7, 7, 7, 7, 7, 7

   RB,  "Software system specification (SSS)",                                                                                                                  E, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   RB,  "Interface requirements document (IRD)",                                                                                                                E, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   RB,  "Safety and dependability analysis results for lower level suppliers",                                                                              "E Q", |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   TS,  "Software requirements specification (SRS)",                                                                                                  |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   TS,  "Software interface control document (ICD)",                                                                                                  |blankcell|,           E,           E, |blankcell|, |blankcell|, |blankcell|
   DDF, "Software design document (SDD)",                                                                                                             |blankcell|,           E,           E, |blankcell|, |blankcell|, |blankcell|
   DDF, "Software configuration file (SCF)",                                                                                                          |blankcell|,           E,           E,       "E Q",           E, "E Q"
   DDF, "Software release document (SRelD)",                                                                                                          |blankcell|, |blankcell|, |blankcell|,           E,           E, |blankcell|
   DDF, "Software user manual (SUM)",                                                                                                                 |blankcell|, |blankcell|,           E,           E,           E, |blankcell|
   DDF, "Software source code and media labels",                                                                                                      |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DDF, "Software product and media labels",                                                                                                          |blankcell|, |blankcell|, |blankcell|,           E,            E, E
   DDF, "Training material",                                                                                                                          |blankcell|, |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|
   DJF, "Software verification plan (SVerP)",                                                                                                         |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software validation plan (SValP)",                                                                                                           |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   DJF, "Independent software verification and validation plan",                                                                                            "E Q",           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software integration test plan (SUITP)",                                                                                                     |blankcell|,           E,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software unit test plan (SUITP)",                                                                                                            |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software validation specification (SVS) with respect to TS",                                                                                 |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software validation specification (SVS) with respect to RB",                                                                                 |blankcell|, |blankcell|, |blankcell|,           E,           E, |blankcell|
   DJF, "Acceptance test plan",                                                                                                                       |blankcell|, |blankcell|, |blankcell|,           E,           E, |blankcell|
   DJF, "Software unit test report",                                                                                                                  |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software integration test report",                                                                                                           |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software validation report with respect to TS",                                                                                              |blankcell|, |blankcell|,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software validation report with respect to RB",                                                                                              |blankcell|, |blankcell|, |blankcell|,           E,           E, |blankcell|
   DJF, "Acceptance test report",                                                                                                                     |blankcell|, |blankcell|, |blankcell|, |blankcell|,           E, |blankcell|
   DJF, "Installation report",                                                                                                                        |blankcell|, |blankcell|, |blankcell|, |blankcell|,           E, |blankcell|
   DJF, "Software verification report (SVR)",                                                                                                                   E,           E,           E,           E,           E, "E Q"
   DJF, "Independent software verification and validation report",                                                                                    |blankcell|,       "E Q",       "E Q",       "E Q",       "E Q", E
   DJF, "Software reuse file (SRF)",                                                                                                                        "E Q",           E,           E, |blankcell|, |blankcell|, |blankcell|
   DJF, "Software problems reports and nonconformance reports",                                                                                             "E Q",       "E Q",       "E Q",       "E Q",       "E Q", "E Q"
   DJF, "Joint review reports",                                                                                                                                 E,           E,           E,         E  ,         E  , |blankcell|
   DJF, "Justification of selection of operational ground equipment and support services",                                                                  "E Q",       "E Q", |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Software development plan (SDP)",                                                                                                                      E,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Software review plan (SRevP)",                                                                                                                         E,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Software configuration management plan",                                                                                                               E,           E, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Training plan",                                                                                                                                    "E Q", |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Interface management procedures",                                                                                                                      E, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Identification of NRB SW and members",                                                                                                             "E Q", |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MGT, "Procurement data",                                                                                                                                 "E Q",       "E Q", |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MF,  "Maintenance plan",                                                                                                                           |blankcell|,            , |blankcell|,           E,           E, E
   MF,  "Maintenance records",                                                                                                                                  Q,           Q,           Q,       "E Q",       "E Q", "E Q"
   MF,  "SPR and NCR- Modification analysis report- Problem analysis report- Modification documentation- Baseline for change - Joint review reports", |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MF,  "Migration plan and notification",                                                                                                            |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   MF,  "Retirement plan and notification",                                                                                                           |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|
   OP,  "Software operation support plan",                                                                                                            |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, E
   OP,  "Operational testing results",                                                                                                                |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, E
   OP,  "SPR and NCR- User’s request record- Post operation review report",                                                                           |blankcell|, |blankcell|, |blankcell|, |blankcell|, |blankcell|, E
   PAF, "Software product assurance plan (SPAP)",                                                                                                           "E Q",       "E Q",       "E Q",       "E Q",       "E Q", "E Q"
   PAF, "Software product assurance requirements for suppliers",                                                                                            "E Q",           Q,           Q,           Q,           Q,    Q
   PAF, "Audit plan and schedule",                                                                                                                          "E Q",           Q,           Q,           Q,           Q,    Q
   PAF, "Review and inspection plans or procedures",                                                                                                            Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Procedures and standards",                                                                                                                             Q,       "E Q",           Q,           Q,           Q,    Q
   PAF, "Modelling and design standards",                                                                                                                   "E Q",       "E Q",           Q,           Q,           Q,    Q
   PAF, "Coding standards and description of tools",                                                                                                            Q,       "E Q",           Q,           Q,           Q,    Q
   PAF, "Software problem reporting procedure",                                                                                                                 Q,       "E Q",           Q,           Q,           Q,    Q
   PAF, "Software dependability and safety analysis report- Criticality classification of software components",                                                 Q,       "E Q",       "E Q",       "E Q",       "E Q",    Q
   PAF, "Software product assurance report",                                                                                                                    Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Software product assurance milestone report (SPAMR)",                                                                                              "E Q",       "E Q",       "E Q",       "E Q",       "E Q", "E Q"
   PAF, "Statement of compliance with test plans and procedures",                                                                                               Q,           Q,       "E Q",       "E Q",       "E Q", "E Q"
   PAF, "Records of training and experience",                                                                                                                   Q,           Q,           Q,           Q,           Q,    Q
   PAF, "(Preliminary) alert information",                                                                                                                      Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Results of preaward audits and assessments, and of procurement sources",                                                                               Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Software process assessment plan",                                                                                                                     Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Software process assessment records",                                                                                                                  Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Review and inspection reports",                                                                                                                        Q,           Q,           Q,           Q,           Q,    Q
   PAF, "Receiving inspection report",                                                                                                                      "E Q",       "E Q",       "E Q",       "E Q",           Q,    Q
   PAF, "Input to product assurance plan for systems operation",                                                                                                Q,           Q,           Q,           Q,           Q, "E Q"

.. index:: single: ECSS-Q-ST-80C; Summary

ECSS-Q-ST-80C: Space product assurance / Software product assurance
-------------------------------------------------------------------

The |Q-ST-80C| standard defines software product assurance requirements for
the development and maintenance of space software systems, including
non-deliverable software that affects the quality of the deliverable
product. As stated in :footcite:p:`Space_SW_ECSS_2025b`, p. 20:

  *The objectives of software product assurance are to provide adequate
  confidence to the customer and to the supplier that the developed or
  procured/reused software satisfies its requirements throughout the system's
  lifetime. In particular, that the software is developed to perform properly,
  securely, and safely in its operational environment, meeting the project's
  agreed quality objectives.*

The requirements apply throughout the software lifecycle and cover a range of
activities, including organizational responsibilities, process assessment,
development environment selection, and product verification. The specific set
of requirements that need to be met can be tailored based on several factors:

* Dependability and safety aspects, as determined by the software criticality
  category,
* Software development constraints, for example the type of development
  (database vs. real-time), or
* Product quality / business objectives as specified by the customer

|Q-ST-80C| defines requirements in the following areas:

* Software product assurance programme implementation

  This set of activities includes organizational aspects, product assurance
  management, risk management and critical item control, supplier selection
  and control, procurement, tools and supporting environment selection, and
  assessment and improvement process.

* Software process assurance

  These activities comprise software life cycle management; requirements
  applicable to all software engineering processes (e.g., documentation,
  safety analysis, handling of critical software, configuration management,
  metrics, verification, reuse, and automatic code generation); and
  requirements applicable to individual software engineering processes or
  activities (e.g., requirements analysis, architecture and design, coding,
  testing and validation, delivery and acceptance, operations, and
  maintenance).

* Software product quality assurance

  These activities comprise product quality objectives and metrication;
  product quality requirements; software intended for reuse; standard ground
  hardware and services for operational system; and firmware.

As with |E-ST-40C|, the expected output for each requirement identifies the
destination file, the DRL items within that file, and the review(s) that
assess compliance with the requirement. The table above includes this
information for the requirements in |Q-ST-80C|.

.. index:: Criticality categories

.. index:: DO-178C

The ECSS standards recognize that software systems (and different components
of the same software system) may vary in their effects on system safety.
The standards accordingly define several criticality categories,
denoted A (most critical) to D, which correspond closely to the software
levels in the airborne standard |do-178c|.

.. index:: ECSS Handbooks

ECSS Handbooks
--------------

Supplementing the normative standards in the |ndash|\ E, |ndash|\ Q, and
|ndash|\ M series, ECSS has published a set of handbooks offering additional
support, guidance and practical discussion about the standards and their
requirements. They indicate how a customer (the organization acquiring
the space software or system) will likely interpret the standards and thus
how they will expect the supplier to comply.

Several handbooks complement |E-ST-40C|, including:

* |E-HB-40A| (Software engineering handbook)
  :footcite:p:`Space_SW_ECSS_2013`,

  This document provides guidance, explanations, and examples on how to
  satisfy the |E-ST-40C| requirements in practice.

* |E-HB-40-01A| (Agile software development handbook)
  :footcite:p:`Space_SW_ECSS_2020`.

  This handbook shows how to reconcile agile development practices
  with the formal ECSS space software engineering processes.

* |E-HB-40-02A| (Machine learning handbook)
  :footcite:p:`Space_SW_ECSS_2024`.

  This handbook provides guidelines on how to create reliable machine
  learning functions and perform the verification and validation considering
  the specifics of machine learning development practices.

Several handbooks complement ECSS-Q-ST-80C, including:

* |Q-HB-80-01A| (Reuse of existing software)
  :footcite:p:`Space_SW_ECSS_2011b`

  This handbook offers guidance on software reuse (including software
  tools) and also presents a Tool Qualification Level (TQL) concept
  based on |do-178c| :footcite:p:`Space_SW_RTCA_EUROCAE_2011a`,
  |do-330| :footcite:p:`Space_SW_RTCA_EUROCAE_2011b`, and |ISO-26262|
  :footcite:p:`Space_SW_ISO_2018`.

* |Q-HB-80-03A-1| (Software dependability and safety)
  :footcite:p:`Space_SW_ECSS_2017`

  This handbook focuses on analysis techniques such as Failure,
  Mode and Effects Analysis (FMEA) and their application to software;
  i.e., how to analyze what happens in case of failure due to software.
  It covers topics such as defensive programming and prevention of failure
  propagation.

* |Q-HB-80-04A| (Software metrication program definition and implementation)
  :footcite:p:`Space_SW_ECSS_2011a`

  This handbook offers recommendations on organizing and
  implementing a metrication program for space software projects.


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
