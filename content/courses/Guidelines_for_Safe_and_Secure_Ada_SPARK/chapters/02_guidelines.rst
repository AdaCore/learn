.. Coding Standards documentation master file, created by
   sphinx-quickstart on Fri Jun 18 09:39:07 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root 'toctree' directive.

Guidelines
============

Although we refer to them as **rules** in the tables for the sake of brevity,
these entries should be considered **guidance** because they require both
thought and consideration of project-specific characteristics.  For example, in
some cases the guidance is to make a selection from among a set of distinct
enumerated policies. In other cases a single guideline should be followed but
not without some exceptional situations allowing it to be violated. The project
lead should consider which guidelines to apply and how best to apply
each guideline selected.

Many of these rules can also be considered *good* programming practices. As
such, many of them can be directly correlated to the *ISO/IEC Guidance to
Avoiding Vulnerabilities in Programming Languages* [TR24772]_. When a rule
addresses one of these vulnerabilities, it is listed in the appropriate
subsection.

.. toctree::
   :maxdepth: 2

   Definitions <guidelines/definitions>
   Dynamic Storage Management <guidelines/dynamic_storage_management>
   Safe Reclamation <guidelines/safe_reclamation>
   Concurrency <guidelines/concurrency>
   Robust Programming Practice <guidelines/robust_programming_practice>
   Exception Usage <guidelines/exception_usage>
   Object-Oriented Programming <guidelines/object_oriented_programming>
   Software Engineering <guidelines/software_engineering>

