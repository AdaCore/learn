----------------------------------------
Hide Implementation Artifacts  (SWE04)
----------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` High, as retrofit can be extensive

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Visible_Components`

+++++++++++
Reference
+++++++++++

MISRA C Rule 8.7: "Functions and objects should not be defined with external
linkage if they are referenced in only one translation unit."

+++++++++++++
Description
+++++++++++++

Do not make implementation artifacts compile-time visible to clients. Only make
available those declarations that define the abstraction presented to clients
by the component. In other words, define Abstract Data Types and use the
language to enforce the abstraction. This is a fundamental Object-Oriented
Design principle.

This guideline minimizes client dependencies and thus allows the maximum
flexibility for changes in the underlying implementation. It minimizes the
editing changes required for client code when implementation changes are made.

This guideline also limits the region of code required to find any bugs to the
package and child packages, if any, defining the abstraction.

This guideline is to be followed extensively as the design default for
components. Once the application code size becomes non-trivial, the cost of
retrofit is extremely high.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/swe04.ads
  :language: Ada
  :lines: 5-18
  :dedent: 3

Note that both type :ada:`Content_T`, as well as the record type components of
type :ada:`Stack_T`, are visible to clients. Client code may declare variables
of type :ada:`Content_T` and may directly access and modify the record
components.
Bugs introduced via this access could be anywhere in the entire client
codebase.

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/swe04.ads
  :language: Ada
  :lines: 19-34
  :dedent: 3


Type :ada:`Content_T`, as well as the record type components of type
:ada:`Stack_T`,
are no longer visible to clients. Any bugs in the stack processing code must be
in
this package, or its child packages, if any.

+++++++
Notes
+++++++

The GNATcheck rule specified above is not exhaustive.
