.. include:: ../../../global.txt

Summary of contributions to DO-178C/ED-12C objectives
=====================================================

Overall summary: which objectives are met
-----------------------------------------

The following tables summarize how the Ada and SPARK languages and
AdaCore's tools help meet the objectives in |do-178c| and the
technology supplements. The numbers refer to the specific objectives
in the core document or the relevant supplement.

Table A-3 and Tables A-8 through A-10 are not included, since they are
independent of AdaCore's technologies.

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Summary
           mapping to DO-178C/ED-12C Objectives

.. only:: latex

   .. raw:: latex

      \begin{landscape}

Mapping of AdaCore's Technologies to DO-178C/ED-12C Objectives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. csv-table:: Overall Summary, Part 1 - Which |do-178c| objectives are met by AdaCore's Technologies
   :widths: 20, 20, 20, 20, 20

   |blankcell|, |blankcell|, Objectives, Objectives, Objectives
   Technology, Component, "Table A-1 Software Planning Process", "Table A-2 Software Development Process", "Table A-4 Verification of Outputs of Software Design Process"
   Programming Language, Ada, "3, 5", "3, 4, 5, 6, 7", "3, 7, 8, 10"
   Programming Language, "SPARK (GNATprove)", "3, 5", "3, 4, 5, 6, 7", "FM 14, 15, 16, 17"
   GNAT Pro Toolchain, GNAT Pro Assurance, 3, 7
   GNAT Pro Toolchain, GNATstack, "3, 4", |blankcell|, |blankcell|
   |gnatsas|, Defects & Vulnerability Analysis, "3, 4", |blankcell|, |blankcell|
   |gnatsas|, GNATmetric, 3, |blankcell|, |blankcell|
   |gnatsas|, GNATcheck, "3, 4, 5", |blankcell|, |blankcell|
   |gnatdas|, GNATtest, "3, 4", |blankcell|, |blankcell|
   |gnatdas|, GNATemulator, 3, |blankcell|, |blankcell|
   |gnatdas|, GNATcoverage, "3, 4", |blankcell|, |blankcell|
   IDE, GPS, 3, |blankcell|, |blankcell|
   IDE, GNATbench, 3, |blankcell|, |blankcell|
   IDE, GNATdashboard, 3, |blankcell|, |blankcell|

.. csv-table:: Overall Summary, Part 2 - Which |do-178c| objectives are met by AdaCore's Technologies
   :widths: 20, 20, 20, 20, 20

   |blankcell|, |blankcell|, Objectives, Objectives, Objectives
   Technology, Component, "Table A-5 Verification of Outputs of Software Coding and Verification Processes", "Table A-6 Verification of Outputs of Integration Processes", "Table A-7 Verification of Outputs of Verification Process Results"
   Programming Language, Ada, "2, 3, 5, 6, 8, 9", |blankcell|, "OO 10, 11"
   Programming Language, "SPARK (GNATprove)", "FM 10, 11, 12, 13", "3, 4", "FM 1-10"
   GNAT Pro Toolchain, GNAT Pro Assurance, 7, |blankcell|, |blankcell|
   GNAT Pro Toolchain, GNATstack, 6, |blankcell|, |blankcell|
   |gnatsas|, Defects & Vulnerability Analysis, "3, 4, 6", |blankcell|, |blankcell|
   |gnatsas|, GNATmetric, 4, |blankcell|, |blankcell|
   |gnatsas|, GNATcheck, 4, |blankcell|, |blankcell|
   |gnatdas|, GNATtest, |blankcell|, "3, 4", "1, 2"
   |gnatdas|, GNATemulator, "3, 4", |blankcell|, |blankcell|
   |gnatdas|, GNATcoverage, |blankcell|, |blankcell|, "5, 6, 7, 8"
   IDE, GPS, |blankcell|, |blankcell|, |blankcell|
   IDE, GNATbench, |blankcell|, |blankcell|, |blankcell|
   IDE, GNATdashboard, |blankcell|, |blankcell|, |blankcell|

Detailed summary: which activities are supported
-------------------------------------------------

In the tables below, the references in the Activities column are to
sections in |do-178c| or to one of the technology supplements. The
references in the Use case columns are to sections in this document.

Since AdaCore's tools mostly contribute to the bottom stages of the
"V" cycle (design, coding, integration and related verification
activities), verification of High-Level Requirements (and thus Table
A-3) are outside the scope of AdaCore solutions.

Likewise, the objectives in Table A-8 (Configuration Management), A-9
(Quality Assurance) and A-10 (Certification Liaison Process) are
independent of AdaCore's technologies; they are the responsibility of
the user.

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-1:
           Software Planning Process

Table A-1: Software Planning Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The objectives of the Software Planning process are satisfied by
developing software plans and standards. These activities are the
responsibility of the software project. However, using AdaCore
solutions can reduce the effort in meeting some of these objectives.

.. csv-table::
   :widths: 10, 27, 8, 25, 15, 15
   :class: longtable

   Objective, Summary, Activities, "Use case #1a", "Use case #1b (OOT)", "Use case #2"
   1, "The activities of the software life cycle processes are defined", All, "This document describes possible methods and tools that may be used. When an AdaCore solution is adopted, it should be documented in the  plans.", Same as #1a, Same as #1a
   2, "The software life cycle(s), including the inter-relationships between the processes, their sequencing, and transition criteria, is defined.", All, "A variety of software life cycles may be defined (such as V cycle, Incremental, Iterative, and Agile). AdaCore solutions do not require any specific software life cycle.", Same as #1a, Same as #1a
   3, "Software life cycle environment is selected and defined", "4.4.1.a, 4.4.1.f, 4.4.2.b, 4.4.3.a, 4.4.3.b", "When an AdaCore solution is used, the plans should identify and escribe the associated tools. In particular, see :ref:`Airborn_SW_Sustained_Branches` and :ref:`Airborn_SW_Compiling_with_the_GNAT_Pro_compiler`", Same as #1a, Same as #1a
   4, "Additional considerations are addressed", "4.2.j, 4.2.k", "The need for tool qualification is addressed throughout this document.", "Same as #1a", "Same as #1a"
   5, "Software development standards are defined.", "4.2.b, 4.5.b, 4.5.c, 4.5.d", "This document describes possible languages, methods and tools that may be used during the design and coding processes. When any of them are used, design and code standards must be developed accordingly. A Code Standard can be defined through :ref:`Airborn_SW_GNATcheck`", "Same as #1a", "Same as #1a"
   6, "Software plans comply with this document.", All, "This objective is satisfied through the review and analysis of the plans and standards.", "Same as #1a", "Same as #1a"
   7, "Development and revision of software plans are coordinated.", All, "This objective is satisfied through the review and analysis of the plans and standards.", "Same as #1a", "Same as #1a"

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-2: Software Development Processes
.. index:: V software life cycle

Table A-2: Software Development Processes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AdaCore tools mostly contribute to the bottom stages of the
traditional "V" cycle (design, coding, integration, and the related
verification activities).

.. csv-table::
   :widths: 6, 21, 7, 21, 23, 22
   :class: longtable

   Objective, Description, Activities, Use case #1a, Use case #1b (OOT), Use case #2
   1, "High-level requirements are developed", 5.1.2.j, "Outside the scope of AdaCore solutions, except for :ref:`Airborn_SW_Parameter_Data_Items`", Same as #1a, Same as #1a
   2, "Derived high-level requirements are defined and provided to the system processes, including the system safety assessment process.", |blankcell|, "Outside the scope of AdaCore solutions", Same as #1a, Same as #1a
   3, "Software architecture is developed.", 5.2.2.a, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", "See :ref:`Airborn_SW_Object_orientation_for_the_architecture`, :ref:`Airborn_SW_Memory_management_issues`, :ref:`Airborn_SW_Exception_handling`", "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`"
   4, "Low-level requirements are developed.", 5.2.2.a, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", "See :ref:`Airborn_SW_Dealing_with_dynamic_dispatching_and_substitutability`", "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`, :ref:`Airborn_SW_Robustness_and_SPARK`"
   5, "Derived low-level requirements are defined and provided to the system processes, including the system safety assessment process", 5.2.2.b, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", "See :ref:`Airborn_SW_Dealing_with_dynamic_dispatching_and_substitutability`", "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`, :ref:`Airborn_SW_Robustness_and_SPARK`"
   6, "Source code is developed", All, "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`, :ref:`Airborn_SW_Integration_of_C_components_with_Ada`, :ref:`Airborn_SW_Robustness_defensive_programming`", "Same as #1a",   "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`"
   7, "Executable Object Code and Parameter Data Files, if any produced and loaded in the target computer.", "5.4.2.a, 5.4.2.b, 5.4.2.d", "See :ref:`Airborn_SW_Compiling_with_the_GNAT_Pro_compiler`, :ref:`Airborn_SW_Integration_of_C_components_with_Ada`, :ref:`Airborn_SW_Parameter_Data_Items`", "Same as #1a", "Same as #1a"

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-4:
           Verification of Outputs of Software Design Process

Table A-4: Verification of Outputs of Software Design Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AdaCore solutions may contribute to the verification of the
architecture and Low-Level Requirements when Ada/SPARK is used during
design process. However, compliance with High-Level Requirements is
not addressed by AdaCore solutions.

.. csv-table::
   :widths: 8, 15, 10, 25, 20, 22
   :class: longtable

   Objective, Description, Activities, Use case #1a, Use case #1b (OOT), Use case #2
   1, "Low-level requirements comply with high-level requirements.", 6.3.2, "Outside the scope of AdaCore solutions, except for :ref:`Airborn_SW_Parameter_Data_Items`", "Same as #1a", "Same as #1a"
   2, "Low-level requirements are accurate and consistent", 6.3.2, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   3, "Low-level requirements are compatible with target computer.", 6.3.2, "See :ref:`Airborn_SW_Implementation_of_Hardware_Software_Interfaces`", |blankcell|, |blankcell|
   4, "Low-level requirements are verifiable.", 6.3.2, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   5, "Low-level requirements conform to standards.", 6.3.2, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   6, "Low-level requirements are traceable to high-level requirements.", |blankcell|, "Outside the scope of AdaCore solutions", "Same as #1a", "Same as #1a"
   7, "Algorithms are accurate.", 6.3.2, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", "Same as #1a", "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   8, "Software architecture is compatible with high-level requirements.", 6.3.3, |blankcell|, "See :ref:`Airborn_SW_Memory_management_issues`, :ref:`Airborn_SW_Exception_handling`", |blankcell|
   9, "Software architecture is consistent.", 6.3.3, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_architecture_reviews`"
   10, "Software architecture is compatible with target computer.", 6.3.3, "See :ref:`Airborn_SW_Implementation_of_Hardware_Software_Interfaces`", "Same as #1a", "Same as #1a"
   11, "Software architecture is verifiable.", 6.3.3, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_architecture_reviews`"
   12, "Software architecture conforms to standards.", 6.3.3, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_architecture_reviews`"
   13, "Software partitioning integrity is confirmed.", |blankcell|, "Outside the scope of AdaCore solutions", "Same as #1a", "Same as #1a"
   FM14, "Formal analysis cases and procedures are correct.", FM 6.3.6, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM15, "Formal analysis results are correct and discrepancies explained.", FM 6.3.6, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM16, "Requirements formalization is correct.", FM 6.3.6, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM17, "Formal method is appropriately defined, justified, and appropriate.", FM 6.3.6, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-5: Verification of Outputs of Software Requirement Process

Table A-5 Verification of Outputs of Software Requirement Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. csv-table::
   :widths: 5, 15, 6, 26, 24, 23
   :class: longtable

   Objective, Description, Activities, Use case #1a, Use case #1b (OOT), Use case #2
   1, "Source Code complies with low-level requirements.", 6.3.4, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_source_code_reviews`"
   2, "Source Code complies with software architecture.", 6.3.4, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", |blankcell|, "See :ref:`Airborn_SW_Contributions_to_source_code_reviews`"
   3, "Source Code is verifiable.", 6.3.4, "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`", "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`",  "See :ref:`Airborn_SW_Contributions_to_source_code_reviews`"
   4, "Source Code conforms to standards.", 6.3.4, "See :ref:`Airborn_SW_Defining_and_Verifying_a_Code_Standard_with_GNATcheck`", |blankcell|
   5, "Source Code is traceable to low-level requirements.", 6.3.4, "See :ref:`Airborn_SW_Using_Ada_during_the_design_process`", |blankcell|, "See :ref:`Airborn_SW_Contributions_to_source_code_reviews`"
   6, "Source Code is accurate and consistent.", 6.3.4, "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`, :ref:`Airborn_SW_Robustness_defensive_programming`, :ref:`Airborn_SW_Checking_worst-case_stack_consumption_with_GNATstack`, :ref:`Airborn_SW_Checking_source_code_accuracy_and_consistency_with_GNAT_SAS`", "See :ref:`Airborn_SW_Benefits_of_the_Ada_language`, :ref:`Airborn_SW_Robustness_defensive_programming`, :ref:`Airborn_SW_Checking_worst-case_stack_consumption_with_GNATstack`, :ref:`Airborn_SW_Checking_source_code_accuracy_and_consistency_with_GNAT_SAS`, :ref:`Airborn_SW_Overloading_and_type_conversion_vulnerabilities`, :ref:`Airborn_SW_Accounting_for_dispatching_in_performing_resource_ analysis`", "See :ref:`Airborn_SW_Checking_worst-case_stack_consumption_with_GNATstack`"
   7, "Output of software integration process is complete and correct.", 6.3.5, "See :ref:`Airborn_SW_Compiling_with_the_GNAT_Pro_compiler`", "Same as #1a", "Same as #1a"
   8, "Parameter Data Item File is complete and correct.", 6.6, "See :ref:`Airborn_SW_Parameter_Data_Items`", "Same as #1a", "Same as #1a"
   9, "Verification of Parameter Data Item File is achieved.", 6.6, "See :ref:`Airborn_SW_Parameter_Data_Items`", "Same as #1a", "Same as #1a"
   FM 10, "Formal analysis cases and procedures are correct.", "FM.6.3.6.a, FM.6.3.6.b", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM 11, "Formal analysis results are correct and discrepancies explained.", "FM.6.3.6.c", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM 12, "Requirement formalization is correct.", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`"
   FM 13, "Formal method is correctly defined, justified and appropriate.", "FM.6.2.1.a, FM.6.2.1.b, FM.6.2.1.c", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`"

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-6:
           Testing of Outputs of Integration Process

Table A-6 Testing of Outputs of Integration Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. csv-table::
   :widths: 5, 16, 7, 26, 23, 22

   Objective, Description, Activities, Use case #1a, Use case #1b (OOT), Use case #2
   1, "Executable Object Code complies with high-level requirements.", |blankcell|, "This objective is outside the scope of AdaCore solutions", "Same as #1a", "Same as #1a"
   2, "Executable Object Code is robust with high-level requirements.", |blankcell|, "This objective is outside the scope of AdaCore solutions", "Same as #1a", "Same as #1a"
   3, "Executable Object Code complies with low-level requirements.", "6.4.2, 6.4.2.1, 6.4.3, 6.5", "See :ref:`Airborn_SW_Using_GNATtest_for_low-level_testing`, :ref:`Airborn_SW_Using_GNATemulator_for_low-level_and_software/software_integration_tests`", "See :ref:`Airborn_SW_Formal_analysis_as_an_alternative_to_low-level_testing`, :ref:`Airborn_SW_Low-level_verification_by_mixing_test_and_proof`"
   4, "Executable Object Code is robust with low-level requirements.", "6.4.2, 6.4.2.2, 6.4.3, 6.5", "See :ref:`Airborn_SW_Using_GNATtest_for_low-level_testing`, :ref:`Airborn_SW_Using_GNATemulator_for_low-level_and_software/software_integration_tests`, :ref:`Airborn_SW_Robustness_defensive_programming`", "Same as #1a", "See :ref:`Airborn_SW_Formal_analysis_as_an_alternative_to_low-level_testing`, :ref:`Airborn_SW_Low-level_verification_by_mixing_test_and_proof`"
   5, "Executable Object Code is compatible with target computer.", |blankcell|, "This objective is based on High-Level Requirements and is thus outside the scope of AdaCore solutions", "Same as #1a", "Same as #1a"

.. index:: single: DO-178C/ED-12C and AdaCore technologies; Table A-7:
           Verification of Verification Process Results

Table A-7 Verification of Verification Process Results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use case #2 applied formal analysis to verify compliance with
Low-Level Requirements.  In applying |do-333|, objectives 4 to 7 from
|do-178c| are replaced with objectives FM 1 to FM 10.

.. csv-table::
   :widths: 4, 17, 7, 26, 23, 22
   :class: longtable

   Objective, Description, Activities, Use case #1a, Use case #1b (OOT), Use case #2
   1, "Test procedures are correct.", 6.4.5, "See :ref:`Airborn_SW_Using_GNATtest_for_low-level_testing`", "Same as #1a", "Limited to verification not performed by formal analysis"
   2, "Test results are correct and discrepancies explained.", 6.4.5, "See :ref:`Airborn_SW_Using_GNATtest_for_low-level_testing`", "Same as #1a", "Limited to verification not performed by formal analysis"
   3, "Test coverage of high-level requirements is achieved.", |blankcell|, "This objective concerns the verification of High-Level Requirements and thus is outside the scope of Adacore solutions", "Same as #1a", "Same as #1a"
   4, "Test coverage of low-level requirements is achieved.", 6.4.4.1, |blankcell|, "See :ref:`Airborn_SW_Coverage_in_the_case_of_generics`", |blankcell|
   5, "Test coverage of software structure (modified condition / decision coverage) is achieved.", "6.4.4.2.a, 6.4.4.2.b", "See :ref:`Airborn_SW_Structural_code_coverage_with_GNATcoverage`, :ref:`Airborn_SW_Coverage_in_the_case_of_generics`", |blankcell|
   6, "Test coverage of software structure (design coverage) is achieved.", "6.4.4.2.a, 6.4.4.2.b", "See :ref:`Airborn_SW_Structural_code_coverage_with_GNATcoverage`, :ref:`Airborn_SW_Coverage_in_the_case_of_generics`", |blankcell|
   7, "Test coverage of software structure (statement coverage) is achieved.", "6.4.4.2.a, 6.4.4.2.b", "See :ref:`Airborn_SW_Structural_code_coverage_with_GNATcoverage`, :ref:`Airborn_SW_Coverage_in_the_case_of_generics`", |blankcell|
   8, "Test coverage of software structure (data coupling and control coupling) is achieved.", "See :ref:`Airborn_SW_Data_and_control_coupling_coverage_with_GNATcoverage`", "See :ref:`Airborn_SW_Data_and_control_coupling_coverage_with_GNATcoverage`, :ref:`Airborn_SW_Dispatching_as_a_new_module_coupling_mechanism`", "See :ref:`Airborn_SW_Data_and_control_coupling_coverage_with_GNATcoverage`"
   9, "Verification of additional code, that cannot be traced to Source Code, is achieved.", 6.4.4.2.b, "See :ref:`Airborn_SW_Demonstrating_traceability_of_source_to_object_code`", "Same as #1a", "Same as #1a"
   OO 10, "Verify local type consistency.", "OO.6.7.2", |blankcell|, "See :ref:`Airborn_SW_Dealing_with_dynamic_dispatching_and_substitutability`",
   OO 11, "Verify the use of dynamic memory management is robust.", "OO.6.8.2", |blankcell|, "See :ref:`Airborn_SW_Memory_management_issues`", |blankcell|
   FM 1, "Formal analysis cases and procedures are correct.", |blankcell|, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM 2, "Formal analysis results are correct and discrepancies explained.", |blankcell|, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Contributions_to_Low-Level_Requirement_reviews`"
   FM 3, "Coverage of high-level requirements is achieved.", |blankcell|, |blankcell|, |blankcell|, "In this use case, only LLR are used for formal analysis"
   FM 4, "Coverage of low-level requirements is achieved.", |blankcell|, |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Alternatives_to_code_coverage_when_using_proofs`"
   FM 5-8, "Verification coverage of software structure is achieved.", "FM.6.7.1.2, FM.6.7.1.3, FM.6.7.1.4, FM.6.7.1.5", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Alternatives_to_code_coverage_when_using_proofs`"
   FM 9, "Verification of additional code, that cannot be traced to Source Code, is achieved.", "FM.6.7", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Property_preservation_between_source_code_and_object_code`"
   FM 10, "Formal method is appropriately defined, justified and appropriate.", "FM.6.2.1.a, FM.6.2.1.b, FM.6.2.1.c", |blankcell|, |blankcell|, "See :ref:`Airborn_SW_Using_SPARK_for_design_data_development`"

.. index:: DO-330/ED-215: Software Tool Qualification Considerations
.. index:: single: GNATstack; TQL-5 qualification material
.. index:: single: GNATcheck; TQL-5 qualification material
.. index:: single: GNATcoverage; TQL-5 qualification material
.. index:: single: Light Profile; Level A certification material
.. index:: single: Light-Tasking Profile; Level A certification material

AdaCore Tool Qualification and Library Certification
----------------------------------------------------

Qualification material can be developed for GNATstack and is available
for GNATcheck and GNATcoverage:

.. csv-table::
   :widths: 14, 13, 38, 35

   Tool, TQL, |do-178c| Objectives / Activities, |do-330| Objectives / Activities
   GNATstack, "TQL-5", "A-5[6]: 6.3.4.f", |blankcell|
   GNATcheck, "TQL-5", "A-5[4]: 6.3.4.d", "T-5[1..6y]: 6.1.3.4.d"
   GNATcoverage, "TQL-5", "A-7[5..9]: 6.4.4.2", "T-7[5..9]: 6.1.4.3.2.a"

Certification material up to Software Level A can be developed for the
Light and Light-Tasking run-time libraries.

.. only:: latex

   .. raw:: latex

      \end{landscape}
