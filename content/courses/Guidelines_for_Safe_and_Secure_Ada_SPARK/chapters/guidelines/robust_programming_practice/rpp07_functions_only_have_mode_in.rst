---------------------------------------
Functions Only Have Mode "in" (RPP07)
---------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Required

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security:

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`function_out_parameters`

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Functions must have only mode :ada:`in`.

As of Ada 2012, functions are allowed to have the same modes as procedures.
However, this can lead to side effects and aliasing.

This rule disallows all modes except mode :ada:`in` for functions.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.24 Side-effects and order of evaluation [SAM]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp07.adb
  :language: Ada
  :lines: 4-11
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp07.adb
  :language: Ada
  :lines: 12-16
  :dedent: 3

OR

.. literalinclude:: examples/rpp07.adb
  :language: Ada
  :lines: 17-23
  :dedent: 3

+++++++
Notes
+++++++

Violations are detected by SPARK.
