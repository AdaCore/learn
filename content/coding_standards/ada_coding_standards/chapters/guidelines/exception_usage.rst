=======================
Exception Usage (EXU)
=======================

.. include:: ../../../global.txt

*Goal*
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security: :math:`\checkmark`

Description
   Have a plan for managing the use of Ada exceptions at the application level.

Rules
   EXU01, EXU02, EXU03, EXU04

Exceptions in modern languages present the software architect with a dilemma.
On one hand, exceptions can increase integrity by allowing components to signal
specific errors in a manner that cannot be ignored, and, in general, allow
residual errors to be caught. (Although there should be no unexpected errors in
high integrity code, there may be some such errors due, for example, to
unforeseeable events such as radiation-induced single-event upsets.)  On the
other hand, unmanaged use of exceptions increases verification expense and
difficulty, especially flow analysis, perhaps to an untenable degree. In that
case overall integrity is reduced or unwarranted.

In addition, programming languages may define some system-level errors in terms
of language-defined exceptions. Such exceptions may be unavoidable, at least at
the system level. For example, in Ada, stack overflow is signalled with the
language-defined :ada:`Storage_Error` exception. Other system events, such as
bus error, may also be mapped to language-defined or vendor-defined exceptions.

Complicating the issue further is the fact that, if exceptions are completely
disallowed, there will be no exception handling code in the underlying run-time
library. The effects are unpredictable if any exception actually does occur.

Therefore, for the application software the system software architect must
decide whether to allow exceptions at all, and if they are to be used, decide
the degree and manner of their usage. At the system level, the architect must
identify the exceptions that are possible and how they will be addressed.

.. toctree::
   :maxdepth: 1

   Don't Raise Language-Defined Exceptions (EXU01) <exception_usage/exu01_dont_raise_language-defined_exceptions.rst>
   No Unhandled Application-Defined Exceptions (EXU02) <exception_usage/exu02_no_unhandled_application-defined_exceptions.rst>
   No Exception Propagation Beyond Name Visibility (EXU03) <exception_usage/exu03_no_exception_propagation_beyond_name_visibility.rst>
   Prove Absence of Run-time Exceptions (EXU04) <exception_usage/exu04_prove_absence_of_run-time_exceptions.rst>
