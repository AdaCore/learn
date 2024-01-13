---------------------------------------
Only Reclaim to the Same Pool (RCL03)
---------------------------------------

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

N/A

+++++++++++++
Description
+++++++++++++

When deallocating, ensure that the pool to which the storage will be returned
is the same pool from which it was allocated. Execution is erroneous
otherwise, meaning anything can happen
(:arm:`Ada Reference Manual: 13.11.2 (16) Unchecked Storage Deallocation <13-11-2>`).

Each access type has an associated storage pool, either implicitly by default,
or explicitly with a storage pool specified by the programmer. The implicit
default pool might not be the same pool used for another access type, even an
access type designating the same subtype.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.39 Memory leak and heap fragmentation [XYL]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

      type Pointer1 is access all Integer;
      type Pointer2 is access all Integer;
      P1 : Pointer1;
      P2 : Pointer2;
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Integer, Name => Pointer2);
   begin
      P1 := new Integer;
      P2 := Pointer2 (P1);
      Call_Something ( P2.all );
      ...
      Free (P2);

In the above, :ada:`P1.all` was allocated from :ada:`Pointer1'Storage_Pool`,
but, via the type conversion, the code above is attempting to return it to
:ada:`Pointer2'Storage_Pool`, which may be a different pool.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

      type Pointer1 is access all Integer;
      type Pointer2 is access all Integer;
      P1 : Pointer1;
      P2 : Pointer2;
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Integer, Name => Pointer1);
   begin
      P1 := new Integer;
      P2 := Pointer2 (P1);
      Call_Something ( P2.all );
      ...
      Free (P1);

+++++++
Notes
+++++++

Enforcement of this rule can only be provided by manual code review, unless
deallocation is forbidden via :ada:`No_Unchecked_Deallocation`.
