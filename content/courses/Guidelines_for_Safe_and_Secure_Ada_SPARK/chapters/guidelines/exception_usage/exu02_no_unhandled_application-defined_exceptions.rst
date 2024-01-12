-----------------------------------------------------
No Unhandled Application-Defined Exceptions (EXU02)
-----------------------------------------------------

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
:rule:`Unhandled_Exceptions`

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

All application-defined exceptions must have at least one corresponding handler
that is applicable. Otherwise, if an exception is raised, undesirable behavior
is possible. The term *applicable* means that there is no dynamic call chain
that can reach the active exception which does not also include a handler that
will be invoked for that exception, somewhere in that chain.

When an unhandled exception occurs in the sequence of statements
of an application task and propagates to task's body, the task terminates
abnormally. No *notification* of some sort is required or defined by the
language, although some vendors' implementations may print out a log message or
provide some other non-standard response. (Note that such a notification
implies an external persistent environment, such as an operating system, that
may not be present in all platforms.) The task failure does not affect any
other tasks unless those other tasks attempt to communicate with it. In short,
failure is silent.

Although the language-defined package :ada:`Ada.Task_Termination` can be used to
provide a response using standard facilities, not all run-time libraries
provide that package. For example, under the  Ravenscar profile, application
tasks are not intended to terminate, neither normally nor abnormally, and the
language does not define what happens if they do. A run-time library for a
memory-constrained target, especially a bare-metal target without an operating
system, might  not include any support for task termination when the tasking
model is Ravenscar. The effects of task termination in that case are not
defined by the language.

When an unhandled exception occurrence reaches the main subprogram and is not
handled there, the exception occurrence is propagated to the environment task,
which then completes abnormally.  Even if the main subprogram does handle the
exception, the environment task still completes (normally in that case).

When the environment task completes (normally or abnormally) it waits for the
completion of dependent application tasks, if any. Those dependent tasks
continue executing normally, i.e., they do not complete as a result of the
environment task completion. Alternatively, however, instead of waiting for
them, the implementation has permission to abort the dependent application
tasks, per
:arm:`10.2\\(30\\) Program Execution <10-2>`
The resulting application-specific effect is undefined.

Finally, whether the environment task waited for the dependent tasks or aborted
them, the semantics of further execution beyond that point are undefined. There
is no concept of a calling environment beyond the environment task
(:arm:`10.2\\(30\\) Program Execution <10-2>`).
In some systems there is no calling environment, such as bare-metal platforms
with only an Ada run-time library and no operating system.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.36 Ignored error status and unhandled exceptions [OYB]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/exu02.adb
  :language: Ada
  :lines: 19-31
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/exu02.adb
  :language: Ada
  :lines: 35-50
  :dedent: 3

+++++++
Notes
+++++++

SPARK can prove that no exception will be raised (or fail to prove it and
indicate the failure).
