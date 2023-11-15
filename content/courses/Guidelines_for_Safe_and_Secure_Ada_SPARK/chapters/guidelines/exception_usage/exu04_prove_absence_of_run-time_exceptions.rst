----------------------------------------------
Prove Absence of Run-time Exceptions (EXU04)
----------------------------------------------

**Level** :math:`\rightarrow` Required

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security:

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

MISRA C Rule 1.3 "There shall be no occurrence of undefined or critical
unspecified behaviour"

+++++++++++++
Description
+++++++++++++

In many high-integrity systems the possible responses to an exception are
limited or nonexistent.  In these cases the only approach is to prove
exceptions cannot occur in the first place.  Additionally, the cost of proving
exceptions cannot happen may be less than the cost of analyzing code in which
they are allowed to be raised.

The restriction :ada:`No_Exceptions` can be used with :ada:`pragma Restrictions`
to enforce
this approach.  Specifically, the restriction ensures that :ada:`raise`
statements and exception handlers do not appear in the source code and that
language-defined checks are not emitted by the compiler.  However, a run-time
check performed automatically by the hardware is permitted because it typically
cannot be prevented.  An example of such a check would be traps on invalid
addresses.  If a hardware check fails, or if an omitted language-defined check
would have failed, execution is unpredictable. As a result, enforcement with
the restriction is not ideal. However, proof of the absence of run-time errors
is possible using the SPARK subset of Ada.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

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

This restriction is detected by SPARK, in which any statements explicitly
raising an exception must be proven unreachable (or proof fails and the failure
is indicated), and any possibility of run-time exception should be proved not
to happen.
