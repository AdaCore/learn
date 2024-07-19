----------------------
No Recursion (RPP12)
----------------------

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
:rule:`Recursive_Subprograms`

+++++++++++
Reference
+++++++++++

MISRA C Rule 17.2: "Functions shall not call themselves, either directly or
indirectly."

+++++++++++++
Description
+++++++++++++

No subprogram shall be invoked, directly or indirectly, as part of its own
execution.

In addition to making static analysis more complex, recursive calls make static
stack usage analysis extremely difficult, requiring, for example, manual supply of call
limits.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.35 Recursion [GDL]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* CWE-674 - Uncontrolled Recursion

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp12.adb
  :language: Ada
  :lines: 7-15
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp12.adb
  :language: Ada
  :lines: 16-24
  :dedent: 3

+++++++
Notes
+++++++

The compiler will detect violations with the restriction :ada:`No_Recursion` in place.
Note this is a dynamic check.

The GNATcheck rule specified above is a static check, subject to the
limitations
described in
:gnat_check_rm_url:`GNATcheck Reference Manual: Recursive Subprograms <predefined_rules.html#recursive-subprograms>`.
