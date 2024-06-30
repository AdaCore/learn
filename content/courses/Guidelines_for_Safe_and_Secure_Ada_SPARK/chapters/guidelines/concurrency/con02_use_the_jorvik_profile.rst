--------------------------------
Use the Jorvik Profile (CON02)
--------------------------------

.. include:: ../../../../global.txt

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
:rule:`uses_profile:jorvik`

**Mutually Exclusive** :math:`\rightarrow` CON01

+++++++++++
Reference
+++++++++++

:arm:`Ada Reference Manual: D.13 The Ravenscar and Jorvik Profiles <D-13>`

+++++++++++++
Description
+++++++++++++

The following profile must be in effect:

.. code-block:: Ada

   pragma Profile (Jorvik);

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
              No_Local_Protected_Objects,
              No_Local_Timing_Events,
              No_Protected_Type_Allocators,
              No_Requeue_Statements,
              No_Select_Statements,
              No_Specific_Termination_Handlers,
              No_Task_Allocators,
              No_Task_Hierarchy,
              No_Task_Termination,
              Pure_Barriers,
              Max_Task_Entries => 0,
              No_Dependence => Ada.Asynchronous_Task_Control,
              No_Dependence => Ada.Execution_Time.Group_Budgets,
              No_Dependence => Ada.Execution_Time.Timers,
              No_Dependence => Ada.Task_Attributes,
              No_Dependence => System.Multiprocessors.Dispatching_Domains);

The following restrictions are part of the Ravenscar profile but **not** part
of the Jorvik profile.

.. code-block:: Ada

    No_Implicit_Heap_Allocations
    No_Relative_Delay
    Max_Entry_Queue_Length => 1
    Max_Protected_Entries => 1
    No_Dependence => Ada.Calendar
    No_Dependence => Ada.Synchronous_Barriers

Jorvik also replaces restriction :ada:`Simple_Barriers` with
:ada:`Pure_Barriers` (a weaker requirement than the restriction
:ada:`Simple_Barriers`).

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
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

.. literalinclude:: examples/con02.adb
  :language: Ada
  :lines: 13-23
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/con02.adb
  :language: Ada
  :lines: 32-40
  :dedent: 3


+++++++
Notes
+++++++

The Ada builder will detect violations. GNATcheck can also detect violations.
