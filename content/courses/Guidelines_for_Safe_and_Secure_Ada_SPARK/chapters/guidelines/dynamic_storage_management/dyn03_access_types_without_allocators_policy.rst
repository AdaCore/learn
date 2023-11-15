------------------------------------------------
Access Types Without Allocators Policy (DYN03)
------------------------------------------------

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

MISRA C Rule 21.3 "The memory allocation and deallocation functions of <stdlib.h>
shall not be used"

+++++++++++++
Description
+++++++++++++

The following restrictions must be in effect:

   * No_Allocators
   * No_Dependence => Ada.Unchecked_Deallocation

In this approach dynamic access values are only created via the attribute
:ada:`'Access` applied to aliased objects. Allocation and deallocation never
occur. As a result, storage exhaustion cannot occur because no *dynamic*
allocations occur. Fragmentation cannot occur because there are no
deallocations.

In this approach the following constructs are not allowed:

   * Allocators
   * User-defined storage pools
   * Unchecked Deallocations

Aspects should be applied to all access types in this approach, specifying a
value of zero for the storage size.  Although the restriction No_Allocators is
present, such clauses may be necessary to prevent any default storage pools
from being allocated for the access types, even though the pools would never be
used. A direct way to accomplish this is to use pragma Default_Storage_Pool
with a parameter of :ada:`null` like so:

   :ada:`pragma Default_Storage_Pool (null);`

The above would also ensure no allocations can occur with access types that
have the default pool as their associated storage pool (per
:arm:`13.11.3\\(6.1\\/3\\) Default Storage Pools <13-11>`

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.14 Dangling reference to heap [XYK]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

Any code using the constructs listed above.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   type Descriptor is ...;
   type Descriptor_Ref is access all Descriptor;
   ...
   Device : aliased Descriptor;
   ...
   P : Descriptor_Ref := Device'Access;
   ...

+++++++
Notes
+++++++

The compiler, and/or GNATcheck, will detect violations of the restrictions.
