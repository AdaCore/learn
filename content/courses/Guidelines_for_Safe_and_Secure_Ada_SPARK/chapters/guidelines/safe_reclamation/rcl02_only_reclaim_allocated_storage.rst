----------------------------------------
Only Reclaim Allocated Storage (RCL02)
----------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Mandatory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` Code inspection

+++++++++++
Reference
+++++++++++

[SEI-C]_ MEM34-C: Only Free Memory Allocated Dynamically

+++++++++++++
Description
+++++++++++++

Only deallocate storage that was dynamically allocated by the evaluation of an
allocator (i.e., :ada:`new`).

This is possible because Ada allows creation of access values designating
declared (aliased) objects.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.39 Memory leak and heap fragmentation [XYL]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

      type String_Reference is access all String;
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => String, Name => String_Reference);
      S : aliased String := "Hello";
      Y : String_Reference := S'Access;
   begin
      Free (Y);

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

Remove the call to :ada:`Free (Y)`.

+++++++
Notes
+++++++

Enforcement of this rule can only be provided by manual code review, unless
deallocation is forbidden via No_Unchecked_Deallocation.
