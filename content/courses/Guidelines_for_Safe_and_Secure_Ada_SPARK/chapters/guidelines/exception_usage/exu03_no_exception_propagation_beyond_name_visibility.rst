---------------------------------------------------------
No Exception Propagation Beyond Name Visibility (EXU03)
---------------------------------------------------------

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
:rule:`Non_Visible_Exceptions`

+++++++++++
Reference
+++++++++++

RPP05

+++++++++++++
Description
+++++++++++++

An active exception can be propagated dynamically past the point where the name
of the exception is visible (the scope of the declaration). The exception can
only be handled via :ada:`others` past that point. That situation prevents
handling the exception specifically, and violates RPP05.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-248 - Uncaught Exception <248>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/exu03.adb
  :language: Ada
  :lines: 4-12
  :dedent: 3

As a result the exception name cannot be referenced outside the body:

.. literalinclude:: examples/exu03.adb
  :language: Ada
  :lines: 13-20
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/exu03.adb
  :language: Ada
  :lines: 21-37
  :dedent: 3


+++++++
Notes
+++++++

N/A
