-------------------------------------
Avoid Function Side-Effects (RPP06)
-------------------------------------

.. include:: ../../../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security:

**Remediation** :math:`\rightarrow` Medium

**Verification Method** :math:`\rightarrow` Code inspection

+++++++++++
Reference
+++++++++++

MISRA C Rule 13.2: "The value of an expression and its persistent side effects
shall be the same under all permitted evaluation orders."

+++++++++++++
Description
+++++++++++++

Functions cannot update an actual parameter or global variable.

A side effect occurs when evaluation of an expression updates an object. This
rule applies to function calls, a specific form of expression.

Side effects enable one form of parameter aliasing (see below) and evaluation
order dependencies.  In general they are a potential point of confusion because
the reader expects only a computation of a value.

There are useful idioms based on functions with side effects. Indeed, a random
number generator expressed as a function must use side effects to update the
seed value.  So-called "memo" functions are another example, in which the
function tracks the number of times it is called. Therefore, exceptions to this
rule are anticipated but should only be allowed on a per-instance basis after
careful analysis.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.24 Side-effects and order of evaluation [SAM]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   Call_Count : Integer := 0;
   function F return Boolean is
      Result : Boolean;
   begin
      ...
      Call_Count := Call_Count + 1;
      return Result;
   end F;

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

Remove the update to :ada:`Call_Count`, or change the function into a procedure
with a parameter for :ada:`Call_Count`.

+++++++
Notes
+++++++

Violations are detected by SPARK as part of a rule disallowing side effects on
expression evaluation.
