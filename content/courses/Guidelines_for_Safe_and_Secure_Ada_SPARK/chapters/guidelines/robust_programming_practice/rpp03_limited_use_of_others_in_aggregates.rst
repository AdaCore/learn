-----------------------------------------------
Limited Use of "others" in Aggregates (RPP03)
-----------------------------------------------

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
:rule:`OTHERS_In_Aggregates`

+++++++++++
Reference
+++++++++++

Similar to RPP01

+++++++++++++
Description
+++++++++++++

Do not use an :ada:`others` choice in an extension aggregate. In :ada:`record`
and :ada:`array` aggregates,
do not use an :ada:`others` choice unless it is used either
to refer to all components, or to all but one component.

This guideline prevents accidental provision of a general value for a
:ada:`record` component or :ada:`array`
component, when a specific value was intended. This
possibility includes the case in which new components are added to an existing
composite type.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.5 Enumerator issues [CCB]
* 6.27 Switch statements and static analysis [CLL]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp03.ads
  :language: Ada
  :lines: 4-14
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp03.ads
  :language: Ada
  :lines: 18-26
  :dedent: 3

+++++++
Notes
+++++++

N/A
