-----------------------------------------
No Class-wide Constructs Policy (OOP01)
-----------------------------------------

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

**Mutually Exclusive** :math:`\rightarrow` OOP02

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

In this approach, tagged types are allowed and type extension (inheritance) is
allowed, but there are no class-wide constructs.

This restriction ensures there are no class-wide objects or formal parameters,
nor access types designating class-wide types.

In this approach there are no possible dynamic dispatching calls because such
calls can only occur when a class-wide value is passed as the parameter to a
primitive operation of a tagged type.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.43 Redispatching [PPH]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   X : Object'Class := Some_Object;

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   X : Object := Some_Object;

+++++++
Notes
+++++++

The compiler will detect violations with the standard Ada restriction
No_Dispatch applied.
