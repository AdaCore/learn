-------------------------------------------------
Don't Raise Language-Defined Exceptions (EXU01)
-------------------------------------------------

.. include:: ../../../global.txt

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
:rule:`Raising_Predefined_Exceptions`

+++++++++++
Reference
+++++++++++

[SEI-Java]_ ERR07-J

+++++++++++++
Description
+++++++++++++

In no case should the application explicitly raise a language-defined
exception.

The Ada language-defined exceptions are raised implicitly in specific
circumstances defined by the language standard. Explicitly raising these
exceptions would be confusing to application developers. The potential for
confusion increases as the exception is propagated up the dynamic call chain,
away from the point of the
:ada:`raise` statement, because this increases the number
of paths and thus corresponding language-defined checks that could have been
the cause.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/exu01.adb
  :language: Ada
  :lines: 4-13
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/exu01.adb
  :language: Ada
  :lines: 14-23
  :dedent: 3

+++++++
Notes
+++++++

N/A
