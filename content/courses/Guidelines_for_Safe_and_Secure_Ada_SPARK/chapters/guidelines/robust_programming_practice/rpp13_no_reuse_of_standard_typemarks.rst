----------------------------------------
No Reuse of Standard Typemarks (RPP13)
----------------------------------------

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

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`overrides_standard_name`

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Do not reuse the names of standard Ada typemarks
(e.g. :ada:`type Integer is range -1_000 .. 1_000;`)

When a developer uses an identifier that has the same name as a standard
typemark, such as :ada:`Integer`, a subsequent maintainer might be unaware that
this identifier does not actually refer to :ada:`Standard.Integer` and might
unintentionally use the locally-scoped :ada:`Integer` rather than the original
:ada:`Standard.Integer`. The locally-scoped :ada:`Integer` can have different
attributes (and may not even be of the same base type).

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/rpp13.ads
  :language: Ada
  :lines: 4-5
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/rpp13.ads
  :language: Ada
  :lines: 9-10
  :dedent: 3

+++++++
Notes
+++++++

N/A
