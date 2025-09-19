-----------------------------------------------------
No Unassigned Mode-Out Procedure Parameters (RPP04)
-----------------------------------------------------

.. include:: ../../../../../global.txt

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

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Unassigned_OUT_Parameters` (builtin rule)

+++++++++++
Reference
+++++++++++

MISRA C Rule 9.1: "The value of an object with automatic storage duration shall
not be read before it has been set."

+++++++++++++
Description
+++++++++++++

For any procedure, all formal parameters of mode :ada:`out` must be assigned a
value if the procedure exits normally. This rule ensures that, upon a normal
return, the corresponding actual parameter has a defined value. Ensuring a
defined value is especially important for scalar parameters because they are
passed by value, such that some value is copied out to the actual. These
undefined values can be especially difficult to locate because evaluation
of the actual parameter's value might not occur immediately after the call
returns.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.32 Passing parameters and return values [CSJ]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-457 - Use of Uninitialized Variable <457>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp04.adb
  :language: Ada
  :lines: 7-22
  :dedent: 3

In the above example, some value is copied back for an output parameter as
specified by :ada:`Register`. The other parameter is not assigned, and
on return the value copied to the actual parameter may not be a valid
representation for a value of the type.
(We give the enumeration values a non-standard representation for the sake
of illustration, i.e., to make it more likely that the undefined value
is not valid.)

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp04.adb
  :language: Ada
  :lines: 23-33
  :dedent: 3

+++++++
Notes
+++++++

The GNATcheck rule specified above only detects a trivial case of an
unassigned variable and doesn't provide a guarantee that there is no
uninitialized access. It is not a replacement for a rigorous check
for uninitialized access provided by advanced static analysis tools
such as SPARK and CodePeer.

Note that the GNATcheck rule does not check function parameters (as
of Ada 2012 functions can have :ada:`out` parameters). As a result, the
better choice is either SPARK or CodePeer.
