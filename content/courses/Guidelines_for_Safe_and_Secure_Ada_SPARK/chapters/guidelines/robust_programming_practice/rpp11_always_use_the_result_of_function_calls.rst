-------------------------------------------------
Always Use the Result of Function Calls (RPP11)
-------------------------------------------------

.. include:: ../../../../global.txt

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

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Compiler restrictions

+++++++++++
Reference
+++++++++++

MISRA C Rule 17.7: "The value returned by a function having
non-void return type shall be used," and

Directive 4.7: "If a function returns error information, that error
information shall be tested."

+++++++++++++
Description
+++++++++++++

In Ada and SPARK, it is not possible to ignore the object returned by a
function call. The call must be treated as a value, otherwise the compiler will
reject the call. For example, the value must be assigned to a variable, or
passed as the actual parameter to a formal parameter of another call, and so
on.

However, that does not mean that the value is actually used to compute some
further results. Although almost certainly a programming error, one could call
a function, assign the result to a variable (or constant), and then not use
that variable further.

Note that functions will not have side-effects (due to RPP06) so it is only the
returned value that is of interest here.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.47 Inter-language calling [DJS]

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

The GNAT compiler warning switch :switch:`-gnatwu` (or the more general
:switch:`-gnatwa` warnings switch) will cause the compiler to detect variables
assigned but not read. CodePeer will detect these unused variables as well.
SPARK goes further by checking that all computations contribute all the way
to subprogram outputs.
