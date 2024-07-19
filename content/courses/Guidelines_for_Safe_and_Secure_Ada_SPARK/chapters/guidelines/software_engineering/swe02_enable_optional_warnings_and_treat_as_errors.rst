-------------------------------------------------------
Enable Optional Warnings and Treat As Errors  (SWE02)
-------------------------------------------------------

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

Power of 10 rule #10: "All code must be compiled, from the first day of
development, with all compiler warnings enabled at the most
pedantic setting available. All code must compile without warnings."

+++++++++++++
Description
+++++++++++++

The Ada compiler does a degree of static analysis itself, and generates many
warnings when they are enabled. These warnings likely indicate very real
problems so they should be examined and addressed, either by changing the code
or disabling the warning for the specific occurrence flagged in the source
code.

To ensure that warnings are examined and addressed one way or the other, the
compiler must be configured to treat warnings as errors, i.e.,  preventing
object code generation.

Note that warnings will occasionally be given for code usage that is
intentional. In those cases the warnings should be disabled by using
:ada:`pragma Warnings` with the parameter :ada:`Off`, and a string indicating
the error message to
be disabled. In other cases, a different mechanism might be appropriate, such
as aspect (or pragma) :ada:`Unreferenced`.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.18 Dead Store [WXQ]
* 6.19 Unused variable [YZS]
* 6.20 Identifier name reuse [YOW]
* 6.22 Initialization of variables [LAV]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* CWE-1127 - Compilation with Insufficient Warnings or Errors

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   procedure P (This : Obj) is
   begin
      ... code not referencing This
   end P;

The formal parameter controls dispatching for the sake of selecting the
subprogram to be called but does not participate in the implementation of the
body.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   procedure P (This : Obj) is
      pragma Unreferenced (This);
   begin
      ... code not referencing This
   end P;

The compiler will no longer issue a warning that the formal parameter
:ada:`This` is not referenced. Of course, if that changes and :ada:`This`
becomes referenced, the compiler will flag the :ada:`pragma`.

+++++++
Notes
+++++++

This rule can be applied via the GNAT :switch:`-gnatwae` compiler switch,
which both enables warnings and treats them as errors. Note that the switch
enables almost all optional warnings, but not all. Some optional warnings
correspond to very specific circumstances, and would otherwise generate too
much noise for their value.
