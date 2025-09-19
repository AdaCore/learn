==================================
Dynamic Storage Management (DYN)
==================================

.. include:: ../../../../global.txt

*Goal*
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance: :math:`\checkmark`
   :Security: :math:`\checkmark`

Description
   Have a plan for managing dynamic memory allocation and deallocation.

Rules
   DYN01, DYN02, DYN03, DYN04, DYN05, DYN06

In Ada, objects are created by being either *declared* or *allocated*.
Declared objects may be informally thought of as being created "on the stack"
although such details are not specified by the language.  *Allocated* objects
may be thought of as being allocated "from the heap", which is, again, an
informal term. Allocated objects are created by the evaluation of allocators
represented by the reserved word :ada:`new` and, unlike declared objects, have
lifetimes independent of scope.

The terms *static* and *dynamic* tend to be used in place of *declared* and
*allocated*, although in traditional storage management terminology all storage
allocation in Ada is dynamic. In the following discussion, the term *dynamic
allocation* refers to storage that is allocated by allocators. *Static* object
allocation refers to objects that are declared. *Deallocation* refers to the
reclamation of allocated storage.

Unmanaged dynamic storage allocation and deallocation can lead to storage
exhaustion; the required analysis is difficult under those circumstances.
Furthermore, access values can establish aliases that complicate verification,
and explicit deallocation of dynamic storage can lead to specific errors (e.g.,
"double free", "use after free") having unpredictable results. As a result, the
prevalent approach to storage management in high-integrity systems is to
disallow dynamic management techniques completely. [SEI-C]_ [MISRA2013]_
[Holzmann2006]_ [ISO2000]_

However, restricted forms of storage management and associated feature usage
can support the necessary reliability and analyzability characteristics while
retaining sufficient expressive power to justify the analysis expense. The
following sections present possible approaches, including the traditional
approach in which no dynamic behavior is allowed. Individual projects may then
choose which storage management approach best fits their requirements and apply
appropriate tailoring, if necessary, to the specific guidelines.

Realization
   There is a spectrum of management schemes possible, trading ease of analysis
   against increasing expressive power. At one end there is no dynamic memory
   allocation (and hence, deallocation) allowed, making analysis trivial. At the
   other end, nearly the full expressive power of the Ada facility is available,
   but with analyzability partially retained. In the latter, however, the user
   must create the allocators in such a manner as to ensure proper behavior.

Rule DYN01 is Required, as it avoids problematic features whatever the strategy
chosen. Rules DYN02-05 are marked as Advisory, because one of them should be
chosen and enforced throughout a given software project.

.. toctree::
   :maxdepth: 1

   Common High Integrity Restrictions (DYN01) <dynamic_storage_management/dyn01_common_high_integrity_restrictions.rst>
   Traditional Static Allocation Policy (DYN02) <dynamic_storage_management/dyn02_traditional_static_allocation_policy.rst>
   Access Types Without Allocators Policy (DYN03) <dynamic_storage_management/dyn03_access_types_without_allocators_policy.rst>
   Minimal Dynamic Allocation Policy (DYN04) <dynamic_storage_management/dyn04_minimal_dynamic_allocation_policy.rst>
   User-Defined Storage Pools Policy (DYN05) <dynamic_storage_management/dyn05_user-defined_storage_pools_policy.rst>
   Statically Determine Maximum Stack Requirements (DYN06) <dynamic_storage_management/dyn06_statically_determine_maximum_stack_requirements.rst>
