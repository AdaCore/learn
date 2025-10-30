.. include:: ../../../global.txt

.. index:: Software Quality Assurance Plan

AdaCore Contributions to the Software Quality Assurance Plan
============================================================

This chapter identifies AdaCore's tools and technologies that support the
techniques and measures defined in the |en-50128| Annex A tables
and that can be cited accordingly in the Software Quality Assurance Plan.
The information is presented in the form of annotations on the
relevant tables in Annex A. These annotations indicate whether a technique
or measure is covered by an AdaCore tool or technology and, if so, a
comment on how the tool or technology contributes is provided.

Summary of abbreviations:

* **M**   |rt-arrow| Mandatory
* **HR**  |rt-arrow| Highly Recommended
* **R**   |rt-arrow| Recommended
* **---** |rt-arrow| Optional (neither Recommended nor Not Recommended)
* **NR**  |rt-arrow| Not Recommended

Table A.3 |ndash| Software Architecture (7.3)
---------------------------------------------

The Ada language and AdaCore technology do not provide support for software
architecture *per se*, but rather are more targeted towards software component
design. However, the presence of some capabilities at the lower level may
enable certain design decisions at a higher level. This table offers some
guidance on how that can be done.

.. only:: latex

   .. raw:: latex

      \begin{landscape}

.. csv-table:: 
   :file: table4-1.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.4 |ndash| Software Design and Implementation (7.4)
----------------------------------------------------------

.. csv-table:: 
   :file: table4-2.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.5 |ndash| Verification and Testing (6.2 and 7.3)
--------------------------------------------------------

.. csv-table:: 
   :file: table4-3.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.6 |ndash| Integration (7.6)
-----------------------------------

.. csv-table:: 
   :file: table4-4.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.7 |ndash| Overall Software Testing (6.2 and 7.7)
--------------------------------------------------------

.. csv-table:: 
   :file: table4-5.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.8 |ndash| Software Analysis Techniques (6.3)
----------------------------------------------------

.. csv-table:: 
   :file: table4-6.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.9 |ndash| Software Quality Assurance (6.5)
--------------------------------------------------

Although AdaCore doesn't directly provide services for ISO 9001
or configuration management, it follows standards to enable 
tool qualification and/or certification. The following table 
only lists items that can be useful to third parties. 

.. csv-table:: 
   :file: table4-7.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.10 |ndash| Software Maintenance (9.2)
---------------------------------------------

.. csv-table:: 
   :file: table4-8.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.11 |ndash| Data Preparation Techniques (8.4)
----------------------------------------------------

.. csv-table:: 
   :file: table4-9.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.12 |ndash| Coding Standards
-----------------------------------

There are available references for coding standards. Their verification
can be automated through different ways: the GNAT compiler can define
base coding standard rules to be checked at compile time, and
GNATcheck implements a wider range of rules and is tailorable to support
project-specific coding standards.  

.. csv-table:: 
   :file: table4-10.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.13 |ndash| Dynamic Analysis and Testing
-----------------------------------------------

.. csv-table:: 
   :file: table4-11.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.14 |ndash| Functional/Black Box Test
--------------------------------------------

GNATtest can generate and execute a testing framework,
with the actual tests being written by developers from requirements.

.. csv-table:: 
   :file: table4-12.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.15 |ndash| Textual Programming Language
-----------------------------------------------

.. csv-table:: 
   :file: table4-13.csv
   :widths: 30 20 20 20 70
   :header-rows: 1
   :class: longtable


Table A.17 |ndash| Modeling
----------------------------

.. csv-table:: 
   :file: table4-14.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.18 |ndash| Performance Testing
--------------------------------------

.. csv-table:: 
   :file: table4-15.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.19 |ndash| Static Analysis
----------------------------------

.. csv-table:: 
   :file: table4-16.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.20 |ndash| Components
-----------------------------

.. csv-table:: 
   :file: table4-17.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.21 |ndash| Test Coverage for Code
-----------------------------------------

.. csv-table:: 
   :file: table4-18.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.22 |ndash| Object Oriented Software Architecture
--------------------------------------------------------

.. csv-table:: 
   :file: table4-19.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable


Table A.23 |ndash| Object Oriented Detailed Design
--------------------------------------------------

.. csv-table:: 
   :file: table4-20.csv
   :widths: 28 13 17 22 10 70
   :header-rows: 1
   :class: longtable

.. only:: latex

   .. raw:: latex

      \end{landscape}
