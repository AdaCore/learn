---------------------------------------------------------
Statically Determine Maximum Stack Requirements (DYN06)
---------------------------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Required

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

**Verification Method** :math:`\rightarrow` Static analysis tools

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Each Ada application task has a stack, as does the "environment task" that
elaborates library packages and calls the main subprogram. A tool to statically
determine the maximum storage required for these stacks must be used, per task.

This guideline concerns another kind of dynamic memory utilization. The
previous guidelines concerned the management of storage commonly referred to as
the "heap." This guideline concerns the storage commonly referred to as the
"stack."  (Neither term is defined by the language, but both are commonly
recognized and are artifacts of the underlying run-time library or operating
system implementation.)

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 4.10 Storage Pool

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

N/A

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

N/A

+++++++
Notes
+++++++

The :gnat_stack_ug_url:`GNATstack <index.html>` tool can statically determine
the maximum requirements per task.
