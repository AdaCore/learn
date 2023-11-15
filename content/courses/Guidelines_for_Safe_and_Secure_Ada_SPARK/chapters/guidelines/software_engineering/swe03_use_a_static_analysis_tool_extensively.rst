------------------------------------------------
Use a Static Analysis Tool Extensively (SWE03)
------------------------------------------------

**Level** :math:`\rightarrow` Mandatory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` Static analysis tools

+++++++++++
Reference
+++++++++++

Power of 10 rule #10: All code must also be checked daily with at least one,
but preferably more than one, strong static source code analyzer and should
pass all analyses with zero warnings.

+++++++++++++
Description
+++++++++++++

If not using SPARK for regular development, use a static analyzer, such as
CodePeer, extensively. No warnings or errors should remain unresolved at the
given level adopted for analysis (which can be selected to adjust the false
positive ratio).

Specifically, any code checked into the configuration management system must be
checked by the analyzer and be error-free prior to check-in. Similarly, each
nightly build should produce a CodePeer baseline for the project.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.6 Conversion errors [FLC]
* 6.18 Dead store [WXQ]
* 6.19 Unused variable [YZS]
* 6.20 Identifier name reuse [YOW]
* 6.24 Side-effects and order of evaluation [SAM]
* 6.25 Likely incorrect expression [KOA]

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

CodePeer is the recommended static analyzer. Note that CodePeer can detect
GNATcheck rule violations (via the :switch:`--gnatcheck` CodePeer switch and a
rules file).
