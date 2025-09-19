--------------------------------------------------
No Enumeration Ranges in Case Constructs (RPP02)
--------------------------------------------------

.. include:: ../../../../../global.txt

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
:rule:`Enumeration_Ranges_In_CASE_Statements` (builtin rule)

+++++++++++
Reference
+++++++++++

Similar to RPP01

+++++++++++++
Description
+++++++++++++

A range of enumeration literals must not be used as a choice in a
:ada:`case` statement or a :ada:`case`
expression. This includes explicit ranges :ada:`(A .. B)`,
subtypes, and the
:ada:`'Range` attribute. Much like the use of :ada:`others` in
:ada:`case`
statement alternatives, the use of ranges makes it possible for a new
enumeration value to be added but not handled with a specific alternative, when
a specific alternative was intended.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.5 Enumerator issues [CCB]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp02.adb
  :language: Ada
  :lines: 6-11
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp02.adb
  :language: Ada
  :lines: 16-21
  :dedent: 3

+++++++
Notes
+++++++

N/A
