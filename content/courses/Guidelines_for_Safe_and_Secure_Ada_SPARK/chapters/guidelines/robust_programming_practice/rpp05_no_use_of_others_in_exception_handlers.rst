--------------------------------------------------
No Use of "others" in Exception Handlers (RPP05)
--------------------------------------------------

.. include:: ../../../../global.txt

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

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`OTHERS_In_Exception_Handlers`

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Much like the situation with :ada:`others` in
:ada:`case` statements and :ada:`case`
expressions, the use of :ada:`others` in exception handlers makes it
possible to omit an intended specific handler for an exception, especially
a new exception added to an existing set of handlers. As a result, a
subprogram could return normally without having applied any recovery for
the specific exception occurrence, which is likely a coding error.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* CWE-396 - Declaration of Catch for Generic Exception

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp05.adb
  :language: Ada
  :lines: 3-10
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp05.adb
  :language: Ada
  :lines: 11-18
  :dedent: 3

+++++++
Notes
+++++++

ISO TR 24772-2: 6.50.2 slightly contradicts this when applying exception
handlers around calls to library routines:

   * Put appropriate exception handlers in all routines that call library
     routines, including the catch-all exception handler :ada:`when others =>`

   * Put appropriate exception handlers in all routines that are called by
     library routines, including the catch-all exception handler
     :ada:`when others =>`

ISO TR 24772-2 also recommends "All tasks should contain an exception handler
at the outer
level to prevent silent termination due to unhandled exceptions."
