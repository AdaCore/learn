----------------------------------------
Static Dispatching Only Policy (OOP02)
----------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` N/A

**Verification Method** :math:`\rightarrow` Compiler restrictions

**Mutually Exclusive** :math:`\rightarrow` OOP01

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

In this approach, class-wide constructs are allowed, as well as tagged types
and type extension (inheritance), but dynamic dispatching remains disallowed
(i.e., as in OOP01).

This rule ensures there are no class-wide values passed as the parameter to a
primitive operation of a tagged type, hence there are no dynamically dispatched
calls.

Note that this rule should not be applied without due consideration.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.43 Redispatching [PPH]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   Some_Primitive (Object'Class (X));

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   Some_Primitive (X);

+++++++
Notes
+++++++

N/A
