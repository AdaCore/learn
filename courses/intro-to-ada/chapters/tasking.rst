Tasking
=======

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Tasks and protected objects allow the implementation of concurrency in
Ada. The following sections explain these concepts in more details.

Tasks
-----

A task can be thought as an application that runs *concurrently* with the
main application. In other programming languages, a task can be called a
`thread <https://en.wikipedia.org/wiki/Thread_(computing)>`_, and tasking
can be called `multithreading
<https://en.wikipedia.org/wiki/Thread_(computing)#Multithreading>`_.

Tasks may synchronize with the main application but may also process
information completely independent from the main application. Here we show
how this is accomplished.

Simple task
~~~~~~~~~~~

Tasks are declared using the keyword :ada:`task`. The task implementation
is specified in a :ada:`task body` block. For example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

Here, we're declaring and implementing the task ``T``. As soon as the main
application starts, task ``T`` starts automatically --- it's not necessary
to manually start this task. By running the application above, we can see
that both calls to :ada:`Put_Line` are performed.

Note that:

- The main application is itself a task (the main task).

  - In this example, the subprogram ``Show_Simple_Task`` is the main task of
    the application.

- Task ``T`` is a subtask.

  - Each subtask has a master task.

  - Therefore the main task is also the master task of task ``T``.

- The number of tasks is not limited to one: we could include a
  task ``T2`` in the example above.

  - This task also starts automatically and runs *concurrently* with
    both task ``T`` and the main task. For example:

    .. code:: ada

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Simple_Tasks is
           task T;
           task T2;

           task body T is
           begin
              Put_Line ("In task T");
           end T;

           task body T2 is
           begin
              Put_Line ("In task T2");
           end T2;

        begin
           Put_Line ("In main");
        end Show_Simple_Tasks;

Simple synchronization
~~~~~~~~~~~~~~~~~~~~~~

As we've just seen, as soon as the main task starts, its subtasks also
start automatically. The main task continues its processing until it has
nothing more to do. At that point, however, it will not terminate. Instead,
the task waits until its subtasks have finished before it allows itself to
terminate. In other words, this waiting process provides synchronization
between the main task and its subtasks.  After this synchronization, the
main task will terminate. For example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Sync is
       task T;
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    begin
       null;
       --  Will wait here until all tasks have terminated
    end Show_Simple_Sync;

The same mechanism is used for other subprograms that contain subtasks: the
subprogram's master task will wait for its subtasks to finish.  So this
mechanism is not limited to the main application and also applies to any
subprogram called by the main application or its subprograms.

Synchronization also occurs if we move the task to a separate package. In
the example below, we declare a task ``T`` in the package
``Simple_Sync_Pkg``.

.. code:: ada

    package Simple_Sync_Pkg is
       task T;
    end Simple_Sync_Pkg;

This is the corresponding package body:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Sync_Pkg is
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    end Simple_Sync_Pkg;

Because the package is :ada:`with`'ed by the main procedure, the task ``T``
defined in the package is part of the main task. For example:

.. code:: ada

    with Simple_Sync_Pkg;

    procedure Test_Simple_Sync_Pkg is
    begin
       null;
       --  Will wait here until all tasks have terminated
    end Test_Simple_Sync_Pkg;

Again, as soon as the main task reaches its end, it synchronizes with task
``T`` from ``Simple_Sync_Pkg`` before terminating.

Delay
~~~~~

We can introduce a delay by using the keyword :ada:`delay`. This puts the
task to sleep for the length of time (in seconds) specified in the delay
statement. For example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Delay is

       task T;

       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello from task T");
             delay 1.0;
             --    ^ Wait 1.0 seconds
          end loop;
       end T;
    begin
       delay 1.5;
       Put_Line ("hello from main");
    end Show_Delay;

In this example, we're making the task ``T`` wait one second after each
time it displays the "hello" message. In addition, the main task is waiting
1.5 seconds before displaying its own "hello" message

Synchronization: rendez-vous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only type of synchronization we've seen so far is the one that happens
automatically at the end of the main task. You can also define custom
synchronization points using the keyword :ada:`entry`. An *entry* can be
viewed as a special kind of subprogram, which is called by the master task
using a similar syntax, as we will see later.

In the task definition, you define which part of the task will accept the
entries by using the keyword :ada:`accept`. A task proceeds until it
reaches an :ada:`accept` statement and then waits for the master task to
synchronize with the it. Specifically,

- The subtask waits at that point (in the :ada:`accept` statement),
  ready to accept a call to the corresponding entry from the master task.

- The master task calls the task entry, in a manner similar to a procedure
  call, to synchronize with the subtask.

This synchronization between tasks is called *rendez-vous*. Let's see an
example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous is

       task T is
          entry Start;
       end T;

       task body T is
       begin
          accept Start; -- Waiting for somebody to call the entry
          Put_Line ("In T");
       end T;

    begin
       Put_Line ("In Main");
       T.Start; --  Calling T's entry
    end Show_Rendezvous;

In this example, we declare an entry ``Start`` for task ``T``.  In the task
body, we implement this entry using :ada:`accept Start`. When task ``T``
reaches this point, it waits for the master task. This synchronization
occurs in the ``T.Start`` statement. After the synchronization completes,
the main task and task ``T`` again run concurrently until they synchronize
one final time when the main task finishes.

An entry may be used to perform more than a simple task synchronization: it
also may perform multiple statements during the time both tasks are
synchronized. We do this with a :ada:`do ... end` block. For the previous
example, we would simply write :ada:`accept Start do <statements>;
end;`. We use this kind of block in the next example.

Select loop
~~~~~~~~~~~

There's no limit to the number of times an entry can be accepted. We could
even create an infinite loop in the task and accept calls to the same entry
over and over again. An infinite loop, however, prevents the subtask from
finishing, so it blocks the master task when it reaches the end of its
processing. Therefore, a loop containing :ada:`accept` statements in a task
body is normally used in conjunction with a :ada:`select ... or terminate`
statement. In simple terms, this statement allows the master task to
automatically terminate the subtask when the master task finishes.  For
example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous_Loop is

       task T is
          entry Start;
       end T;

       task body T is
          Cnt : Integer := 0;
       begin
          loop
             select
                accept Start do
                   Cnt := Cnt + 1;
                end Start;
                Put_Line ("In T's loop (" & Integer'Image (Cnt) & ")");
             or
                terminate;
             end select;
          end loop;
       end T;

    begin
       Put_Line ("In Main");

       for I in 1 .. 4 loop
          T.Start; --  Calling T's entry multiple times
       end loop;

    end Show_Rendezvous_Loop;

In this example, the task body implements an infinite loop that accepts
calls to the ``Start`` entry. We make the following observations:

- The :ada:`accept E do ... end` block is used to increment a counter.

    - As long as task ``T`` is performing the :ada:`do ... end` block, the
      main task waits for the block to complete.

- The main task is calling the ``Start`` entry multiple times in the loop
  from ``1 .. 4``.

    - Because task ``T`` contains an infinite loop, it always accepts calls
      to the ``Start`` entry.

    - When the main task finishes, it checks the status of the ``T``
      task. Even though task ``T`` could accept new calls to the ``Start``
      entry, the master task is allowed to terminate task ``T`` due to the
      :ada:`or terminate` part of the :ada:`select` statement.

Cycling tasks
~~~~~~~~~~~~~

In a previous example, we saw how to delay a task a specified time by using
the :ada:`delay` keyword. However, using delay statements in a loop is not
enough to guarantee regular intervals between those delay statements. For
example, we may have a call to a computationally intensive procedure
between executions of successive delay statements:

.. code-block:: ada

          while True loop
             delay 1.0;
             --    ^ Wait 1.0 seconds
             Computational_Intensive_App;
          end loop;

In this case, we can't guarantee that exactly 10 seconds have elapsed after
10 calls to the delay statement because a time drift may be introduced by
the ``Computational_Intensive_App`` procedure. In many cases, this time
drift is not relevant, so using the :ada:`delay` keyword is good enough.

However, there are situations where a time drift isn't acceptable. In those
cases, we need to use the :ada:`delay until` statement, which accepts a
precise time for the end of the delay, allowing us to define a regular
interval. This is useful, for example, in real-time applications.

We will soon see an example of how this time drift may be introduced and
how the :ada:`delay until` statement circumvents the problem. But before we
do that, we look at a package containing a procedure allowing us to measure
the elapsed time (``Show_Elapsed_Time``) and a dummy
``Computational_Intensive_App`` procedure which is simulated by using a
simple delay. This is the package specification:

.. code:: ada

    with Ada.Real_Time; use Ada.Real_Time;

    package Delay_Aux_Pkg is

       function Get_Start_Time return Time
         with Inline;

       procedure Show_Elapsed_Time
         with Inline;

       procedure Computational_Intensive_App;
    private
       Start_Time   : Time := Clock;

       function Get_Start_Time return Time is (Start_Time);

    end Delay_Aux_Pkg;

And this is the package body:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Delay_Aux_Pkg is

       procedure Show_Elapsed_Time is
          Now_Time     : Time;
          Elapsed_Time : Time_Span;
       begin
          Now_Time     := Clock;
          Elapsed_Time := Now_Time - Start_Time;
          Put_Line ("Elapsed time "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Show_Elapsed_Time;

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

    end Delay_Aux_Pkg;

Using this auxiliary package, we're now ready to write our time-drifting
application:

.. code:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Drifting_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay 1.0;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             Put_Line ("Cycle # " & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished time-drifting loop");
       end T;

    begin
       null;
    end Show_Time_Drifting_Task;

We can see by running the application that we already have a time
difference of about four seconds after three iterations of the loop due to
the drift introduced by ``Computational_Intensive_App``. Using the
:ada:`delay until` statement, however, we're able to avoid this time drift
and have a regular interval of exactly one second:

.. code:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Cycling_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cycle : constant Time_Span := Milliseconds (1000);
          Next  : Time := Aux.Get_Start_Time + Cycle;

          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay until Next;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             --  Calculate next execution time using a
             --  cycle of one seconds
             Next := Next + Cycle;

             Put_Line ("Cycle # " & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished cycling");
       end T;

    begin
       null;
    end Show_Cycling_Task;

Now, as we can see by running the application, the :ada:`delay until`
statement ensures that the ``Computational_Intensive_App`` doesn't disturb
the regular interval of one second between iterations.

Protected objects
-----------------

When multiple tasks are accessing shared data, corruption of that data may
occur. For example, data may be inconsistent if one task overwrites parts
of the information that's being read by another task at the same time. In
order to avoid these kinds of problems and ensure information is accessed
in a coordinated way, we use *protected objects*.

Protected objects encapsulate data and provide access to that data by means
of *protected operations*, which may be subprograms or protected
entries. Using protected objects ensures that data is not corrupted by race
conditions or other simultaneous access.

.. admonition:: Important

    Protected objects can be implemented using Ada tasks. In fact, this was
    the *only* possible way of implementing them in Ada 83 (the first
    version of the Ada language). However, the use of protected objects is
    much simpler than using similar mechanisms implemented using only
    tasks. Therefore, you should use protected objects when your main goal
    is only to protect data.

Simple object
~~~~~~~~~~~~~

You declare a protected object with the :ada:`protected` keyword. The
syntax is similar to that used for packages: you can declare operations
(e.g., procedures and functions) in the public part and data in the private
part. The corresponding implementation of the operations is included in the
:ada:`protected body` of the object. For example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects is

       protected Obj is
          --  Operations go here (only subprograms)
          procedure Set (V : Integer);
          function Get return Integer;
       private
          --  Data goes here
          Local : Integer := 0;
       end Obj;

       protected body Obj is
          --  procedures can modify the data
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          --  functions cannot modify the data
          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj;

    begin
       Obj.Set (5);
       Put_Line ("Number is: " & Integer'Image (Obj.Get));
    end Show_Protected_Objects;

In this example, we define two operations for ``Obj``: ``Set`` and
``Get``. The implementation of these operations is in the ``Obj`` body. The
syntax used for writing these operations is the same as that for normal
procedures and functions. The implementation of protected objects is
straightforward --- we simply access and update ``Local`` in these
subprograms.  To call these operations in the main application, we use
prefixed notation, e.g., ``Obj.Get``.

Entries
~~~~~~~

In addition to protected procedures and functions, you can also define
protected entry points. Do this using the :ada:`entry` keyword. Protected
entry points allow you to define barriers using the :ada:`when`
keyword. Barriers are conditions that must be fulfilled before the entry
can start performing its actual processing --- we speak of *releasing* the
barrier when the condition is fulfilled.

The previous example used procedures and functions to define operations on
the protected objects. However, doing so permits reading protected
information (via ``Obj.Get``) before it's set (via ``Obj.Set``). To allow
that to be a defined operation, we specified a default value (0). Instead,
by rewriting ``Obj.Get`` using an *entry* instead of a function, we
implement a barrier, ensuring no task can read the information before it's
been set.

The following example implements the barrier for the ``Obj.Get``
operation. It also contains two concurrent subprograms (main task and task
``T``) that try to access the protected object.

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects_Entries is

       protected Obj is
          procedure Set (V : Integer);
          entry Get (V : out Integer);
       private
          Local  : Integer;
          Is_Set : Boolean := False;
       end Obj;

       protected body Obj is
          procedure Set (V : Integer) is
          begin
             Local := V;
             Is_Set := True;
          end Set;

          entry Get (V : out Integer)
            when Is_Set is
             --  Entry is blocked until the condition is true.
             --  The barrier is evaluated at call of entries and at exits of
             --  procedures and entries.
             --  The calling task sleeps until the barrier is released.
          begin
             V := Local;
             Is_Set := False;
          end Get;
       end Obj;

       N : Integer := 0;

       task T;

       task body T is
       begin
          Put_Line ("Task T will delay for 4 seconds...");
          delay 4.0;
          Put_Line ("Task T will set Obj...");
          Obj.Set (5);
          Put_Line ("Task T has just set Obj...");
       end T;
    begin
       Put_Line ("Main application will get Obj...");
       Obj.Get (N);
       Put_Line ("Main application has just retrieved Obj...");
       Put_Line ("Number is: " & Integer'Image (N));

    end Show_Protected_Objects_Entries;

As we see by running it, the main application waits until the protected
object is set (by the call to ``Obj.Set`` in task ``T``) before it reads
the information (via ``Obj.Get``). Because a 4-second delay has been added
in task ``T``, the main application is also delayed by 4 seconds. Only
after this delay does task ``T`` set the object and release the barrier in
``Obj.Get`` so that the main application can then resume processing (after
the information is retrieved from the protected object).

Task and protected types
------------------------

In the previous examples, we defined single tasks and protected objects. We
can, however, generalize tasks and protected objects using type
definitions. This allows us, for example, to create multiple tasks based on
just a single task type.

Task types
~~~~~~~~~~

A task type is a generalization of a task. The declaration is similar to
simple tasks: you replace :ada:`task` with :ada:`task type`. The
difference between simple tasks and task types is that task types don't
create actual tasks that automatically start. Instead, a task declaration
is needed. This is exactly the way normal variables and types work:
objects are only created by variable definitions, not type definitions.

To illustrate this, we repeat our first example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

We now rewrite it by replacing ``task T`` with ``task type TT``.  We
declare a task (``A_Task``) based on the task type ``TT`` after its
definition:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task_Type is
       task type TT;

       task body TT is
       begin
          Put_Line ("In task type TT");
       end TT;

       A_Task : TT;
    begin
       Put_Line ("In main");
    end Show_Simple_Task_Type;

We can extend this example and create an array of tasks. Since we're using
the same syntax as for variable declarations, we use a similar syntax for
task types: :ada:`array (<>) of Task_Type`. Also, we can pass information
to the individual tasks by defining a ``Start`` entry. Here's the updated
example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Task_Type_Array is
       task type TT is
          entry Start (N : Integer);
       end TT;

       task body TT is
          Task_N : Integer;
       begin
          accept Start (N : Integer) do
             Task_N := N;
          end Start;
          Put_Line ("In task T: " & Integer'Image (Task_N));
       end TT;

       My_Tasks : array (1 .. 5) of TT;
    begin
       Put_Line ("In main");

       for I in My_Tasks'Range loop
          My_Tasks (I).Start (I);
       end loop;
    end Show_Task_Type_Array;

In this example, we're declaring five tasks in the array ``My_Tasks``. We
pass the array index to the individual tasks in the entry point
(``Start``). After the synchronization between the individual subtasks and
the main task, each subtask calls ``Put_Line`` concurrently.

Protected types
~~~~~~~~~~~~~~~

A protected type is a generalization of a protected object. The
declaration is similar to that for protected objects: you replace
:ada:`protected` with :ada:`protected type`. Like task types,
protected types require an object declaration to create actual
objects. Again, this is similar to variable declarations and allows
for creating arrays (or other composite objects) of protected objects.

We can reuse a previous example and rewrite it to use a protected type:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Object_Type is

       protected type Obj_Type is
          procedure Set (V : Integer);
          function Get return Integer;
       private
          Local : Integer := 0;
       end Obj_Type;

       protected body Obj_Type is
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj_Type;

       Obj : Obj_Type;
    begin
       Obj.Set (5);
       Put_Line ("Number is: " & Integer'Image (Obj.Get));
    end Show_Protected_Object_Type;

In this example, instead of directly defining the protected object
``Obj``, we first define a protected type ``Obj_Type`` and then
declare ``Obj`` as an object of that protected type. Note that the
main application hasn't changed: we still use ``Obj.Set`` and
``Obj.Get`` to access the protected object, just like in the original
example.
