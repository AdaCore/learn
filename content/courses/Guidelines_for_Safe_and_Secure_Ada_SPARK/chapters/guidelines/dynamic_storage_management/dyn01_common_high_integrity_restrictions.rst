--------------------------------------------
Common High Integrity Restrictions (DYN01)
--------------------------------------------

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
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

:arm:`Ada Reference Manual: H.4 High Integrity Restrictions <H-4>`

+++++++++++++
Description
+++++++++++++

The following restrictions must be in effect:

   * :ada:`No_Anonymous_Allocators`
   * :ada:`No_Coextensions`
   * :ada:`No_Access_Parameter_Allocators`
   * :ada:`Immediate_Reclamation`

The first three restrictions prevent problematic usage that, for example, may
cause un-reclaimed (and unreclaimable) storage. The last restriction ensures
any storage allocated by the compiler at run-time for representing objects is
reclaimed at once. (That restriction does not apply to objects created by
allocators in the application.)

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 4.10 Storage Pool

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* CWE-401 - Missing Release of Memory after Effective Lifetime
* CWE-415 - Double Free
* CWE-416 - Use After Free
*  - 

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

For :ada:`No_Anonymous_Allocators`:

   .. code-block:: Ada

      X : access String := new String'("Hello");
      ...
      X := new String'("Hello");

For :ada:`No_Coextensions`:

   .. code-block:: Ada

      type Object (Msg : access String) is ...
      Obj : Object (Msg => new String'("Hello"));

For :ada:`No_Access_Parameter_Allocators`:

   .. code-block:: Ada

      procedure P (Formal : access String);
      ...
      P (Formal => new String'("Hello"));

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

For :ada:`No_Anonymous_Allocators`, use a named access type:

   .. code-block:: Ada

      type String_Reference is access all String;
      S : constant String_Reference := new String'("Hello");
      X : access String := S;
      ...
      X := S;

For :ada:`No_Coextensions`, use a variable of a named access type:

   .. code-block:: Ada

      type Object (Msg : access String) is ...
      type String_Reference is access all String;
      S : String_Reference := new String'("Hello");
      Obj : Object (Msg => S);

For :ada:`No_Access_Parameter_Allocators`, use a variable of a named access type:

   .. code-block:: Ada

      procedure P (Formal : access String);
      type String_Reference is access all String;
      S : String_Reference := new String'("Hello");
      ...
      P (Formal => S);

+++++++
Notes
+++++++

The compiler will detect violations of the first three restrictions. Note that
GNATcheck can detect violations in addition to the compiler.

The fourth restriction is a directive for implementation behavior, not subject
to source-based violation detection.
