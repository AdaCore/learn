-------------------------------
Use SPARK Extensively (SWE01)
-------------------------------

.. include:: ../../../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` High, as retrofit can be extensive

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

:spark_ug:`SPARK User's Guide, section 8: "Applying SPARK in Practice" <usage_scenarios>`

+++++++++++++
Description
+++++++++++++

SPARK has proven itself highly effective, both in terms of low defects, low
development costs, and high productivity. The guideline advises extensive use of
SPARK, especially for the sake of formally proving the most critical parts of
the source code. The rest of the code can be in SPARK as well, even if formal
proof is not intended, with some parts in Ada when features outside the SPARK
subset are essential.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-670 - Always-Incorrect Control Flow Implementation <670>`
* :cwe:`CWE-754 - Improper Check for Unusual or Exceptional Conditions <754>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

Any code outside the (very large) SPARK subset is flagged by the compiler.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

N/A

+++++++
Notes
+++++++

Violations are detected by the SPARK toolset.
