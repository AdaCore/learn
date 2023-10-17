-------------------------------------------------------------
Avoid Shared Variables for Inter-task Communication (CON03)
-------------------------------------------------------------

.. include:: ../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security:

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Volatile_Objects_Without_Address_Clauses`

+++++++++++
Reference
+++++++++++

:arm:`Ada RM D.13 \\- The Ravenscar Profile`

+++++++++++++
Description
+++++++++++++

Although the Ravenscar and Jorvik profiles allow the use of shared variables
for inter-task communication, such use is less robust and less reliable than
encapsulating shared variables within protected objects.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.56 Undefined behaviour [EWF]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/con03.ads
  :language: Ada
  :lines: 5-7
  :dedent: 3

Note that variables marked as :ada:`Atomic` are also :ada:`Volatile`, per the
:arm:`Ada RM C.6\\/8\\(3\\) \\- Shared Variable Control`

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

When assigned to a memory address, a :ada:`Volatile` variable can be used to
interact with a memory-mapped device, among other similar usages.

.. literalinclude:: examples/con03.ads
  :language: Ada
  :lines: 11-14
  :dedent: 3

+++++++
Notes
+++++++

In additon to GNATcheck, SPARK and CodePeer can also detect conflicting access
to unprotected variables.
