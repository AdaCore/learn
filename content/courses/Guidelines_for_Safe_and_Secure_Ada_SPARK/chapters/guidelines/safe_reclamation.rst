
========================
Safe Reclamation (RCL)
========================

.. include:: ../../../global.txt

*Goal*
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance: :math:`\checkmark`
   :Security: :math:`\checkmark`

Description
   Related to managing dynamic storage at the system (policy) level, these
   statement-level rules concern the safe reclamation of access (*pointer*)
   values.

Rules
   RCL01, RCL02, RCL03

.. toctree::
   :maxdepth: 1

   No Multiple Reclamations (RCL01) <safe_reclamation/rcl01_no_multiple_reclamations.rst>
   Only Reclaim Allocated Storage (RCL02) <safe_reclamation/rcl02_only_reclaim_allocated_storage.rst>
   Only Reclaim To The Same Pool (RCL03) <safe_reclamation/rcl03_only_reclaim_to_the_same_pool.rst>
