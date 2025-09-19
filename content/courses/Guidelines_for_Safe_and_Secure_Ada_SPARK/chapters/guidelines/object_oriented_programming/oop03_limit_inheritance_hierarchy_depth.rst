-------------------------------------------
Limit Inheritance Hierarchy Depth (OOP03)
-------------------------------------------

.. include:: ../../../../../global.txt

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

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Deep_Inheritance_Hierarchies:2` (builtin rule)

+++++++++++
Reference
+++++++++++

[AdaOOP2016]_ section 5.1

+++++++++++++
Description
+++++++++++++

A class inheritance hierarchy consists of a set of types related by
inheritance. Each class, other than the root class, is a subclass of other
classes, and each, except for "leaf" nodes, is a base class for those that are
derived from it.

Improperly designed inheritance hierarchies complicate system maintenance and
increase the effort in safety certification, in any programming language.

A common characteristic of problematic hierarchies is "excessive" depth, in
which a given class is a subclass of many other classes. Depth can be a problem
because a change to a class likely requires inspection, modification,
recompilation, and retesting/reverification of all classes below it in the
hierarchy. The extent of that effect increases as we approach the root class.
This rippling effect is known as the *fragile base class* problem. Clearly, the
greater the depth the more subclasses there are to be potentially affected. In
addition, note that a change to one class may cause a cascade of other
secondary changes to subclasses, so the effect is often not limited to a single
change made to all the subclasses in question.

Deep inheritance hierarchies also contribute to complexity, rather than
lessening it, by requiring the reader to understand multiple superclasses in
order to understand the behavior of a given subclass.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.41 Inheritance [RIP]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-1074 - Class with Excessively Deep Inheritance <1074>`
* :cwe:`CWE-1086 - Class with Excessive Number of Child Classes <1086>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

The threshold for "too deep" is inexact, but beyond around 4 or 5 levels the
complexity accelerates rapidly.

.. literalinclude:: examples/oop03.ads
  :language: Ada
  :lines: 4-13
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/oop03.ads
  :language: Ada
  :lines: 28-35
  :dedent: 3


+++++++
Notes
+++++++

N/A
