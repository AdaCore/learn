-----------------------------------
Use the Ravenscar Profile (CON01)
-----------------------------------

.. include:: ../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security:

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`uses_profile:ravenscar`

**Mutually Exclusive** :math:`\rightarrow` CON02

+++++++++++
Reference
+++++++++++

:arm:`Ada RM D.13 \\- The Ravenscar and Jorvik Profiles`

+++++++++++++
Description
+++++++++++++

The following profile must be in effect:

.. code-block:: Ada

   pragma Profile (Ravenscar);

The profile is equivalent to the following set of pragmas:

.. code-block:: Ada

   pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
   pragma Locking_Policy (Ceiling_Locking);
   pragma Detect_Blocking;
   pragma Restrictions (
           No_Abort_Statements,
           No_Dynamic_Attachment,
           No_Dynamic_CPU_Assignment,
           No_Dynamic_Priorities,
           No_Implicit_Heap_Allocations,
           No_Local_Protected_Objects,
           No_Local_Timing_Events,
           No_Protected_Type_Allocators,
           No_Relative_Delay,
           No_Requeue_Statements,
           No_Select_Statements,
           No_Specific_Termination_Handlers,
           No_Task_Allocators,
           No_Task_Hierarchy,
           No_Task_Termination,
           Simple_Barriers,
           Max_Entry_Queue_Length => 1,
           Max_Protected_Entries => 1,
           Max_Task_Entries => 0,
           No_Dependence => Ada.Asynchronous_Task_Control,
           No_Dependence => Ada.Calendar,
           No_Dependence => Ada.Execution_Time.Group_Budgets,
           No_Dependence => Ada.Execution_Time.Timers,
           No_Dependence => Ada.Synchronous_Barriers,
           No_Dependence => Ada.Task_Attributes,
           No_Dependence => System.Multiprocessors.Dispatching_Domains);

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.59 Concurrency - Activation [GGA]
* 6.60 Concurrency - Directed termination [CGT]
* 6.61 Concurrent data access [CGX]
* 6.62 Concurrency - Premature termination [CGS]
* 6.63 Lock protocol errors [CGM]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

Any code disallowed by the profile. Remediation is **high** because use of the
facilities outside the subset can be difficult to retrofit into compliance.

.. literalinclude:: examples/con01.adb
  :language: Ada
  :lines: 12-19
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/con01.adb
  :language: Ada
  :lines: 28-37
  :dedent: 3

+++++++
Notes
+++++++

The Ada builder will detect violations if the programmer specifies this profile
or corresponding pragmas. GNATcheck also can detect violations of profile
restrictions.
