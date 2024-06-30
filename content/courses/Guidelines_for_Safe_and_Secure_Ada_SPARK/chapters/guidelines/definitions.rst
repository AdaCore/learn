
=============
Definitions
=============

.. include:: ../../../global.txt

This section contains terms and values used in the definitions of the rules set
forth in this chapter.

-------
Level
-------

**Level** is the compliance level for the rule. Possible values are:

   Mandatory
      Non-compliance with a *Mandatory* recommendation level corresponds
      to a **high risk** of a software bug. There would need to be a good
      reason for non-conformity to a mandatory rule and, although it is
      accepted that exceptional cases may exist, any non-conformance
      should be accompanied by a clear technical explanation of
      the exceptional circumstance.

   Required
      Non-compliance with a *Required* recommendation level corresponds
      to a **medium to high risk** of a software bug. Much like a
      *Mandatory* recommendation, there would need to be a good reason
      for non-conformity to a required rule. Although it is accepted that
      more exceptional cases may exist, non-conformance
      should be accompanied by a clear technical explanation of the
      exceptional circumstance.

   Advisory
      Failure to follow an *Advisory* recommendation does not necessarily
      result in a software bug; the risk of a direct correlation between
      non-conformance of an advisory rule and a software bug is low.
      Non-compliance with an advisory recommendation level does not
      require a supporting technical explanation, however, as the quality
      of the code may be impacted, the reason for the non-conformance
      should be understood.

-------------
Remediation
-------------

**Remediation** indicates the the level of difficulty to modify/update
code that does not follow this particular rule.

   High
      Failure to follow this rule will likely cause an unreasonable
      amount of modifications/updates to bring the code base into compliance.

   Medium
      Failure to follow this rule will likely cause a large amount of
      modifications/updates to bring the code base into compliance, but
      those changes may still be cost-effective.

   Low
      Failure to follow this rule may cause a small amount of
      modifications/updates to bring the code base into compliance, but
      those changes will be minor compared to the benefit.

   N/A
      This rule is more of a design decision (as opposed to a coding
      flaw) and therefore, if the rule is violated, it is done so
      with a specific purpose.
