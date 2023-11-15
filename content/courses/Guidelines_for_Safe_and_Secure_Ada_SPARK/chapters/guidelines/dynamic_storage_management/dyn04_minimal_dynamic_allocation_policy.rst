-------------------------------------------
Minimal Dynamic Allocation Policy (DYN04)
-------------------------------------------

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security:

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

Power of Ten rule 3 "Do not use dynamic memory allocation after initialization"

+++++++++++++
Description
+++++++++++++

The following restrictions must be in effect:

   * No_Local_Allocators
   * No_Dependence => Ada.Unchecked_Deallocation

In this approach dynamic allocation is only allowed during "start-up" and no
later.  Deallocations never occur.  As a result, storage exhaustion should
never occur assuming the initial allotment is sufficient.  This assumption is
as strong as when using only declared objects on the "stack" because in that
case a sufficient initial storage allotment for the stack must be made.

In this approach the following constructs are not allowed:

   * Unchecked Deallocations

Note that some operating systems intended for this domain directly support this
policy.

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

Code performing dynamic allocations any time prior to an arbitrary point
designated as the end of the "startup" interval.

+++++++
Notes
+++++++

The compiler, and/or GNATcheck, will detect violations of the restrictions.
