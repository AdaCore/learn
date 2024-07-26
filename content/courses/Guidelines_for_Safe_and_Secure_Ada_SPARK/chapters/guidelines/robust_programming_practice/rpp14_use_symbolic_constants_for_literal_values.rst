---------------------------------------------------
Use Symbolic Constants for Literal Values (RPP14)
---------------------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Advisory

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
:rule:`Numeric_Literals`

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Extensive use of literals in a program can lead to two problems. First,
the meaning of the literal is often obscured or unclear from the context.
Second, changing a frequently used literal requires searching the entire
program source for that literal and distinguishing the uses that must be
modified from those that should remain unmodified.

Avoid these problems by declaring objects with meaningfully named constants,
setting their values to the desired literals, and referencing the constants
instead of the literals throughout the program. This approach clearly
indicates the meaning or intended use of each literal. Furthermore, should
the constant require modification, the change is limited to the declaration;
searching the code is unnecessary.

Some literals can be replaced with attribute values. For example, when
iterating over an array, it is better to use
:ada:`Array_Object'First .. Array_Object'Last`
than using :ada:`1 .. Array_Object'Length`.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-547 - Use of Hard-coded, Security-relevant Constants <547>`
* :cwe:`CWE-1106 - Insufficient Use of Symbolic Constants <1106>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp14.ads
  :language: Ada
  :lines: 4-6
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp14.ads
  :language: Ada
  :lines: 10-13
  :dedent: 3

+++++++
Notes
+++++++

N/A
