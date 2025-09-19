-------------------------------------------
Use Class-wide Pre/Post Contracts (OOP06)
-------------------------------------------

.. include:: ../../../../../global.txt

**Level** :math:`\rightarrow` Required

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Specific_Pre_Post` (builtin rule)

+++++++++++
Reference
+++++++++++

[AdaOOP2016]_ section 6.1.4

:spark_ugs:`SPARK User's Guide, section 7.5.2 <how_to_write_object_oriented_contracts.html#writing-contracts-on-dispatching-subprograms>`

+++++++++++++
Description
+++++++++++++

For primitive operations of tagged types, use only class-wide pre/post
contracts, if any.

The class-wide form of precondition and postcondition expresses conditions that
are intended to apply to any version of the subprogram. Therefore, when a
subprogram is derived as part of inheritance, only the class-wide form of those
contracts is inherited from the parent subprogram, if any are defined. As a
result, it only makes sense to use the class-wide form in this situation.

(The same semantics and recommendation applies to type invariants.)

Note: this approach will be required for OOP07 (Ensure Local Type Consistency).

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.42 Violations of the Liskov substitution principle or the contract model
  [BLP]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/oop06.adb
  :language: Ada
  :lines: 4-9
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/oop06.adb
  :language: Ada
  :lines: 13-18
  :dedent: 3

+++++++
Notes
+++++++

N/A
