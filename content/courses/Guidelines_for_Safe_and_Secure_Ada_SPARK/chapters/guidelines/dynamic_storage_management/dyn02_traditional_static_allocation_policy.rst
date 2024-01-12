----------------------------------------------
Traditional Static Allocation Policy (DYN02)
----------------------------------------------

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

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

MISRA C Dir 4.12 "Dynamic memory allocation shall not be used"

+++++++++++++
Description
+++++++++++++

The following restrictions must be in effect:

   * No_Allocators
   * No_Task_Allocators

Under the traditional approach, no dynamic allocations and no deallocations
occur.  Only declared objects are used and no access types of any kind appear
in the code.

Without allocations there is no issue with deallocation as there would be
nothing to deallocate. *Heap* storage exhaustion and fragmentation are clearly
prevented although storage may still be exhausted due to insufficient stack
size allotments.

In this approach the following constructs are not allowed:

   * Allocators
   * Access-to-constant access types
   * Access-to-variable access types
   * User-defined storage pools
   * Unchecked Deallocations

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 4.10 Storage Pool

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

Any code using the constructs listed above.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

N/A

+++++++
Notes
+++++++

The compiler, and/or GNATcheck, will detect violations of the restrictions.
