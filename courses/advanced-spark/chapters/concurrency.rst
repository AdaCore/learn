:code-config:'run_button=False;prove_button=True;accumulate_code=False'

Concurrency
=====================================================================

.. role:: ada(code)
   :language: ada


Concurrency ≠ Parallelism
---------------------------------------------------------------------

- Concurrency allows to create a well structured program

- Parallelism allows to create a high performance program

- Multiple cores/processors are...

    - possible for concurrent programs

    - essential to parallelism

- What about Ada and SPARK?

    - GNAT runtimes for concurrency available on single core & multicore (for SMP platforms)

    - parallel features scheduled for inclusion in Ada and SPARK 202x


Concurrent Program Structure in Ada
---------------------------------------------------------------------

.. image:: concurrent_program_struct_ada.png
   :align: center


The problems with concurrency
---------------------------------------------------------------------

- Control and data flow become much more complex

    - possibly nondeterministic even

    - actual behavior is one of many possible interleavings of tasks

- Data may be corrupted by concurrent accesses

    - so called data races or race conditions

- Control may block indefinitely, or loop indefinitely

    - so called deadlocks and livelocks

- Scheduling and memory usage are harder to compute


Ravenscar – the Ada solution to concurrency problems
---------------------------------------------------------------------

- Ravenscar profile restricts concurrency in Ada

    - ensures deterministic behavior at every point in time

    - recommends use of protected objects to avoid data races

    - prevents deadlocks with Priority Ceiling Protocol

    - allows use of scheduling analysis techniques (RMA, RTA)

    - facilitates computation of memory usage with static tasks

- GNAT Extended Ravenscar profile lifts some restrictions

    - still same benefits as Ravenscar profile

    - removes painful restrictions for some applications


Concurrent Program Structure in Ravenscar
---------------------------------------------------------------------

.. image:: concurrent_program_struct_ravenscar.png
   :align: center


Ravenscar – the SPARK solution to concurrency problems
---------------------------------------------------------------------

- Ravenscar and Extended_Ravenscar profiles supported in SPARK

- Data races prevented by flow analysis

    - ensures no problematic concurrent access to unprotected data

    - flow analysis also ensures non-termination of tasks

- Run-time errors prevented by proof

    - includes violations of the Priority Ceiling Protocol


Concurrency – A trivial example
---------------------------------------------------------------------

.. code:: ada

    task type T;

    Id : Task_Id;

    task body T is
    begin
       loop
          Id := Current_Task;
       end loop;
    end T;

    T1, T2 : T;

- Id can be written by ``T1`` and ``T2`` at the same time


Setup for using concurrency in SPARK
---------------------------------------------------------------------

- Any unit using concurrency features (tasks, protected objects, etc.) must set the profile

.. code:: ada

    pragma Profile (Ravenscar);
    --  or
    pragma Profile (GNAT_Extended_Ravenscar);

- ... plus an additional pragma

    - that ensures tasks start after the end of elaboration

.. code:: ada

    pragma Partition_Elaboration_Policy (Sequential);

- ... which are checked by GNAT partition-wide

    - pragmas needed for verification even it not for compilation


Tasks in Ravenscar
---------------------------------------------------------------------

- A task can be either a singleton object or a type

    - no declarations of entries for rendez-vous

.. code:: ada

    task T;
    task type TT;

- ... completed by a body

    - infinite loop to prevent termination

.. code:: ada

    task body T is
    begin
       loop
          ...
       end loop;
    end T;

- Tasks are declared at library-level

- ... as standalone objects or inside records/arrays

.. code:: ada

    type TA is array (1 .. 3) of TT;
    type TR is record
       A, B : TT;
    end record;


Communication Between Tasks in Ravenscar
---------------------------------------------------------------------

- Tasks can communicate through protected objects

- A protected object is either a singleton object or a type

    - all PO private data initialized by default in SPARK

.. code:: ada

    protected (type) P is
       procedure Set (V : Natural);
       function Get return Natural;
    private
       The_Data : Natural := 0;
    end P;

- ... completed by a body

.. code:: ada

    protected body P is
       procedure Set (V : Natural) is
       begin
          The_Data := V;
       end Set;
       function Get return Natural is
         (The_Data);
    end P;


Protected Objects in Ravenscar
---------------------------------------------------------------------

- Protected objects are  declared at library-level

- ... as standalone objects or inside records/arrays

    - The record type needs to be volatile, as a non-volatile type cannot contain a volatile component. The array type is implicitly volatile when its component type is volatile.

.. code:: ada

    P : PT;

    type PAT is array (1 .. 3) of PT;
    PA : PAT;

    type PRT is record
       A, B : PT;
    end record with Volatile;
    PR : PRT;


Protected Communication with Procedures & Functions
---------------------------------------------------------------------

- CREW enforced (Concurrent-Read-Exclusive-Write)

    - procedures have exclusive read-write access to PO

    - functions have shared read-only access to PO

- Actual mechanism depends on target platform

    - scheduler enforces policy on single core

    - locks used on multicore (using CAS instructions)

    - lock-free transactions used for simple PO (again using CAS)

- Mechanism is transparent to user

    - user code simply calls procedures/functions

    - task may be queued until PO is released by another task


Blocking Communication with Entries
---------------------------------------------------------------------

- Only protected objects have entries in Ravenscar

- Entry = procedure with :ada:`entry` guard condition

    - second level of queues, one for each entry, on a given PO

    - task may be queued until guard is True and PO is released

    - at most one entry in Ravenscar

    - guard is a :ada:`Boolean` component of PO in Ravenscar

.. code:: ada

    protected (type) P is
       entry Reset;
    private
       Is_Not_Null : Boolean := False;
       ...

    protected body P is
       entry Reset when Is_Not_Null is
       begin
          The_Data := 0;
       end Reset;
    end P;


Relaxed Constraints on Entries with Extended Ravenscar
---------------------------------------------------------------------

- Proof limitations with Ravenscar

    - not possible to relate guard to other components with invariant

- GNAT Extended Ravenscar profile lifts these constraints

    - and allows multiple tasks to call the same entry

.. code:: ada

    protected type Mailbox is
       entry Publish;
       entry Retrieve;
    private
       Num_Messages : Natural := 0;
       ...

    protected body Mailbox is
       entry Publish when Num_Messages < Max is ...
       entry Retrieve when Num_Messages > 0 is ...
    end P;


Interrupt Handlers in Ravenscar
---------------------------------------------------------------------

- Interrupt handlers are parameterless procedures of PO

    - with aspect :ada:`Attach_Handler` specifying the corresponding signal

    - with aspect :ada:`Interrupt_Priority` on the PO specifying the priority

.. code:: ada

    protected P with
      Interrupt_Priority =>
        System.Interrupt_Priority'First
    is
       procedure Signal with
         Attach_Handler => SIGHUP;
       ...

- Priority of the PO should be in :ada:`System.Interrupt_Priority`

    - default is OK – in the range of :ada:`System.Interrupt_Priority`

    - checked by proof (default or value of :ada:`Priority` or :ada:`Interrupt_Priority`)


Other Communications Between Tasks in SPARK
---------------------------------------------------------------------

- Tasks must communicate through synchronized objects

    - atomic objects

    - protected objects

    - suspension objects (standard :ada:`Boolean` protected objects)

- Constants are considered as synchronized

    - this includes variables constant after elaboration (specified with aspect :ada:`Constant_After_Elaboration`)

- Single task or PO can access an unsynchronized object

    - exclusive relation between object and task/PO must be specified with aspect :ada:`Part_Of`


Data and Flow Dependencies of Tasks
---------------------------------------------------------------------

- Input/output relation can be specified for a task

    - as task never terminates, output is understood while task runs

    - task itself is both an input and an output

    - implicit :ada:`In_Out => T`

    - explicit dependency

.. code:: ada

    task T with
       Global => (Input => X,
                  Output => Y,
                  In_Out => Z),
       Depends => (T => T,
                   Z => Y,
                   Y => X)
    is
       ...


State Abstraction over Synchronized Variables
---------------------------------------------------------------------

- Synchronized objects can be abstracted in synchronized abstract state with aspect :ada:`Synchronous`

.. code:: ada

    package P with
       Abstract_State => (State with Synchronous)
    is ...

    package body P with
       Refined_State => (State => (A, P, T))
    is
       A : Integer with Atomic, Async_Readers, Async_Writers;
       P : Protected_Type;
       T : Task_Type;
    end P;

- Synchronized state is a form of external state

    - :ada:`Synchronous` same as :ada:`External => (Async_Readers, Async_Writers)`

    - tasks are not volatile and can be part of regular abstract state


Synchronized Abstract State in the Standard Library
---------------------------------------------------------------------

- Standard library maintains synchronized state

    - the tasking runtime maintains state about running tasks

    - the real-time runtime maintains state about current time

.. code:: ada

    package Ada.Task_Identification with
      SPARK_Mode,
      Abstract_State =>
        (Tasking_State with Synchronous,
           External => (Async_Readers, Async_Writers)),
      Initializes    => Tasking_State

    package Ada.Real_Time with
      SPARK_Mode,
      Abstract_State =>
        (Clock_Time with Synchronous,
           External => (Async_Readers, Async_Writers)),
      Initializes    => Clock_Time

- API of these units refer to :ada:`Tasking_State` and :ada:`Clock_Time`


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada

    procedure Rendezvous is
       task T1 is
          entry Start;
       end T1;

       task body T1 is
       begin
          accept Start;
       end T1;

    begin
       T1.Start;
    end Rendezvous;

This code is not correct. Task rendezvous is not allowed; violation of restriction :ada:`Max_Task_Entries = 0`. A local task is not allowed; violation of restriction :ada:`No_Task_Hierarchy`


Example #2
~~~~~~~~~~

.. code:: ada

    protected P is
       entry Reset;
    end P;

    Data : Boolean := False;

    protected body P is
       entry Reset when Data is
       begin
          null;
       end Reset;
    end P;

This code is not correct. Global data in entry guard is not allowed. Violation of restriction :ada:`Simple_Barriers` (for Ravenscar) or :ada:`Pure_Barriers` (for Extended Ravenscar)


Example #3
~~~~~~~~~~

.. code:: ada

    protected P is
       procedure Set (Value : Integer);
    end P;
    task type TT;
    T1, T2 : TT;


    Data : Integer := 0;
    protected body P is
       procedure Set (Value : Integer) is
       begin
          Data := Value;
       end Set;
    end P;
    task body TT is
       Local : Integer := 0;
    begin
       loop
          Local := (Local + 1) mod 100;
          P.Set (Local);
       end loop;
    end TT;

This code is not correct. Global unprotected data accessed in protected object shared between tasks


Example #4
~~~~~~~~~~

.. code:: ada

    protected P is
       procedure Set (Value : Integer);
    end P;
    Data : Integer := 0 with Part_Of => P;
    task type TT;
    T1, T2 : TT;

    protected body P is
       procedure Set (Value : Integer) is
       begin
          Data := Value;
       end Set;
    end P;
    task body TT is
       Local : Integer := 0;
    begin
       loop
          Local := (Local + 1) mod 100;
          P.Set (Local);
       end loop;
    end TT;

This code is correct. ``Data`` is part of the protected object state. The only accesses to ``Data`` are through ``P``.


Example #5
~~~~~~~~~~

.. code:: ada

    protected P1 with Priority => 3 is
       procedure Set (Value : Integer);
    end P1;

    protected P2 with Priority => 2 is
       procedure Set (Value : Integer);
    end P2;

    task type TT with Priority => 1;
    T1, T2 : TT;

    protected body P2 is
       procedure Set (Value : Integer) is
       begin
          P1.Set (Value);
       end Set;
    end P2;

    task body TT is
    begin
       loop
          P2.Set (Local);
       end loop;
    end TT;


This code is correct. :ada:`Ceiling_Priority` policy is respected. Task never accesses a protected object with lower priority than its active priority. Note that PO can call procedure or function from another PO, but not an entry (possibly blocking).


Example #6
~~~~~~~~~~

.. code:: ada

    protected type Mailbox is
       entry Publish;
       entry Retrieve;
    private
       Not_Empty    : Boolean := True;
       Not_Full     : Boolean := False;
       Num_Messages : Natural := 0;
    end Mailbox;

    Max : constant := 100;
    protected body Mailbox is
       entry Publish when Not_Full is
       begin
          Num_Messages := Num_Messages + 1;
          Not_Empty := True;
          if Num_Messages = Max then
             Not_Full := False;
          end if;
       end Publish;
       entry Retrieve when Not_Empty is ...
    end Mailbox;

This code is not correct. Integer range cannot be proved correct.


Example #7
~~~~~~~~~~

.. code:: ada

    protected type Mailbox is
       entry Publish;
       entry Retrieve;
    private
       Num_Messages : Natural := 0;
    end Mailbox;

    Max : constant := 100;
    protected body Mailbox is
       entry Publish when Num_Messages < Max is
       begin
          Num_Messages := Num_Messages + 1;
       end Publish;

       entry Retrieve when Num_Messages > 0 is
       begin
          Num_Messages := Num_Messages - 1;
       end Retrieve;
    end Mailbox;

This code is correct. Precise range obtained from entry guards allows to prove checks.


Example #8
~~~~~~~~~~

.. code:: ada

    type Content is record
       Not_Empty ... Not_Full ... Num_Messages ...
    end record with Predicate =>
       Num_Messages in 0 .. Max
       and Not_Empty = (Num_Messages > 0)
       and Not_Full = (Num_Messages < Max);
    protected type Mailbox is
       ... C : Content;
    end Mailbox;

    protected body Mailbox is
       entry Publish when C.Not_Full is
          Not_Full     : Boolean := C.Not_Full;
          Num_Messages : Natural := C.Num_Messages;
       begin
          Num_Messages := Num_Messages + 1;
          if Num_Messages = Max then
             Not_Full := False;
          end if;
          C := (True, Not_Full, Num_Messages);
       end Publish;

This code is correct. Precise range obtained from predicate allows to prove checks. Predicate is preserved.


Example #9
~~~~~~~~~~

.. code:: ada

    package Service with
      Abstract_State => (State with External)
    is
       procedure Extract (Data : out Integer) with
         Global => (In_Out => State);

    task type T;
    T1, T2 : T;

    task body T is
       X : Integer;
    begin
       loop
          Extract (X);
       end loop;
    end T;

This code is not correct. Unsynchronized state cannot be accessed from multiple tasks or protected objects.


Example #10
~~~~~~~~~~~

.. code:: ada

    package Service with
      Abstract_State => (State with Synchronous, External)
    is
       procedure Extract (Data : out Integer) with
         Global => (In_Out => State);

    task type T;
    T1, T2 : T;

    task body T is
       X : Integer;
    begin
       loop
          Extract (X);
       end loop;
    end T;

This code is correct. Abstract state is synchronized, hence can be accessed from multiple tasks and protected objects.
