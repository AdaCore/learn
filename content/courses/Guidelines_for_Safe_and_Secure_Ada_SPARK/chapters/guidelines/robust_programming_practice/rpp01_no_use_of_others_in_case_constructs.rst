-----------------------------------------------
No Use of "others" in Case Constructs (RPP01)
-----------------------------------------------

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
:rule:`OTHERS_In_CASE_Statements` (builtin rule)

+++++++++++
Reference
+++++++++++

[SEI-C]_ MSC01-C

+++++++++++++
Description
+++++++++++++

Case statement alternatives and case-expressions must not include use of the
:ada:`others` discrete choice option. This rule prevents accidental coverage
of a choice added after the initial case statement is written, when an explicit
handler was intended for the addition.

Note that this is opposite to typical C guidelines such as [SEI-C]_ MSC01-C.
The reason is that in C, the :c:`default` alternative plays the role of
defensive
code to mitigate the switch statement's non-exhaustivity. In Ada, the
:ada:`case` construct is exhaustive:
the compiler statically verifies that for every possible value of the
:ada:`case` expression there is a branch alternative, and there is also
a dynamic check against invalid values which serves as implicit defensive code.
As a result, Ada's :ada:`others` alternative doesn't play C's defensive code
role and therefore a stronger guideline can be adopted.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.27 Switch statements and static analysis [CLL]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-478 - Missing Default Case in Multiple Condition Expression <478>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp01.adb
  :language: Ada
  :lines: 6-11
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp01.adb
  :language: Ada
  :lines: 16-21
  :dedent: 3

+++++++
Notes
+++++++

N/A
