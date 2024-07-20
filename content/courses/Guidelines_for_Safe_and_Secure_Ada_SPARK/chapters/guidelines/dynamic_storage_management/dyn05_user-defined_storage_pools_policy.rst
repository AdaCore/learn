-------------------------------------------
User-Defined Storage Pools Policy (DYN05)
-------------------------------------------

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

**Verification Method** :math:`\rightarrow` Code inspection

+++++++++++
Reference
+++++++++++

MISRA C Rule 21.3: "The memory allocation and deallocation functions of
<stdlib.h> shall not be used."

+++++++++++++
Description
+++++++++++++

There are two issues that make storage utilization analysis difficult:

1. the predictability of the allocation and deallocation implementation, and

2. how access values are used by the application.

The behavior of the underlying
implementation is largely undefined and may, for example, consist of calls to
the operating system (if present). Application code can manipulate access
values beyond the scope of analysis.

Under this policy, the full expressive power of access-to-object types is
provided but one of the two areas of analysis difficulty is removed.
Specifically, predictability of the allocation and deallocation implementation
is achieved via user-defined storage pools.  With these  storage pools, the
implementation of allocation (:ada:`new`) and deallocation (instances of
:ada:`Ada.Unchecked_Deallocation`) is defined by the pool type.

If the pool type is implemented with fixed-size blocks on the stack, allocation
and deallocation timing behavior are predictable.

Such an implementation would also be free from fragmentation.

Given an analysis providing the worst-case allocations and deallocations, it
would be possible to verify that pool exhaustion does not occur.  However, as
mentioned such analysis can be quite difficult. A mitigation would be the use
of the "owning" access-to-object types provided by SPARK.

In this approach no storage-related constructs are disallowed unless the SPARK
subset is applied.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 4.10 Storage Pool

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-401 - Missing Release of Memory after Effective Lifetime <401>`
* :cwe:`CWE-415 - Double Free <415>`
* :cwe:`CWE-416 - Use After Free <416>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

Allocation via an access type not tied to a user-defined storage pool.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   Heap : Sequential_Fixed_Blocks.Storage_Pool
            (Storage_Size => Required_Storage_Size,
             Element_Size => Representable_Obj_Size,
             Alignment    => Representation_Alignment);
   type Pointer is access all Unsigned_Longword with
      Storage_Pool => Heap;
   Ptr : Pointer;
   ...
   Ptr := new Unsigned_Longword; -- from Heap

+++++++
Notes
+++++++

Enforcement of this approach can only be provided by manual code review unless
SPARK is used.

However, the User-Defined Storage Pools Policy can be enforced statically by
specifying :ada:`Default_Storage_Pool (null)`. This essentially requires all
access types to have a specified storage pool if any allocators are used with
the access type.
