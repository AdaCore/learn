-------------------------------
Use SPARK Extensively (SWE01)
-------------------------------

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

[SPARK2014]_ Section 8 "Applying SPARK in Practice"

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
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

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
