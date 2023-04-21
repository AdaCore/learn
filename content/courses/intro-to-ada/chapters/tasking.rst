Tasking
=======

.. include:: ../../global.txt

Tasks and protected objects allow the implementation of concurrency in
Ada. The following sections explain these concepts in more detail.

Tasks
-----

A task can be thought as an application that runs *concurrently* with the
main application. In other programming languages, a task might be called a
:wikipedia:`thread <Thread_(computing)>`, and tasking
might be called
:wikipedia:`multithreading <Thread_(computing)#Multithreading>`.

Tasks may synchronize with the main application but may also process
information completely independently from the main application. Here we show
how this is accomplished.

Simple task
~~~~~~~~~~~

Tasks are declared using the keyword :ada:`task`. The task implementation
is specified in a :ada:`task body` block. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task

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

Here, we're declaring and implementing the task :ada:`T`. As soon as the main
application starts, task :ada:`T` starts automatically |mdash| it's not necessary
to manually start this task. By running the application above, we can see
that both calls to :ada:`Put_Line` are performed.

Note that:

- The main application is itself a task (the main or “environment” task).

  - In this example, the subprogram :ada:`Show_Simple_Task` is the main task of
    the application.

- Task :ada:`T` is a subtask.

  - Each subtask has a master, which represents the program construct in which
    the subtask is declared. In this case, the main subprogram
    :ada:`Show_Simple_Task` is :ada:`T` \'s master.

  - The master construct is executed by some enclosing task, which we will
    refer to as the "master task" of the subtask.

- The number of tasks is not limited to one: we could include a
  task :ada:`T2` in the example above.

  - This task also starts automatically and runs *concurrently* with
    both task :ada:`T` and the main task. For example:

    .. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Multiple_Simple_Task

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

As we've just seen, as soon as the master construct reaches its “begin”,
its subtasks also
start automatically. The master continues its processing until it has
nothing more to do. At that point, however, it will not terminate. Instead,
the master waits until its subtasks have finished before it allows itself to
complete. In other words, this waiting process provides synchronization
between the master task and its subtasks.  After this synchronization, the
master construct  will complete. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Sync

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
       --  Will wait here until all tasks
       --  have terminated
    end Show_Simple_Sync;

The same mechanism is used for other subprograms that contain subtasks: the
subprogram execution will wait for its subtasks to finish.  So this
mechanism is not limited to the main subprogram and also applies to any
subprogram called by the main subprogram, directly or indirectly.

Synchronization also occurs if we move the task to a separate package. In
the example below, we declare a task :ada:`T` in the package
:ada:`Simple_Sync_Pkg`.

.. code:: ada no_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg
    :class: ada-syntax-only

    package Simple_Sync_Pkg is
       task T;
    end Simple_Sync_Pkg;

This is the corresponding package body:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Sync_Pkg is
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    end Simple_Sync_Pkg;

Because the package is :ada:`with`'ed by the main procedure, the task :ada:`T`
defined in the package will become a subtask of the main task. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg

    with Simple_Sync_Pkg;

    procedure Test_Simple_Sync_Pkg is
    begin
       null;
       --  Will wait here until all tasks
       --  have terminated
    end Test_Simple_Sync_Pkg;

As soon as the main subprogram returns, the main task synchronizes with any
subtasks spawned by packages
:ada:`T` from :ada:`Simple_Sync_Pkg` before finally terminating.

Delay
~~~~~

We can introduce a delay by using the keyword :ada:`delay`. This puts the
current task to sleep for the length of time (in seconds) specified in the delay
statement. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Delay

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Delay is

       task T;

       task body T is
       begin
          for I in 1 .. 5 loop
             Put_Line ("hello from task T");
             delay 1.0;
             --    ^ Wait 1.0 seconds
          end loop;
       end T;
    begin
       delay 1.5;
       Put_Line ("hello from main");
    end Show_Delay;

In this example, we're making the task :ada:`T` wait one second after each
time it displays the "hello" message. In addition, the main task is waiting
1.5 seconds before displaying its own "hello" message

Synchronization: rendezvous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only type of synchronization we've seen so far is the one that happens
automatically at the end of a master construct with a subtask.
You can also define custom
synchronization points using the keyword :ada:`entry`. An *entry* can be
viewed as a special kind of subprogram, which is called by another task
using a similar syntax, as we will see later.

In the task body definition, you define which part of the task will accept the
entries by using the keyword :ada:`accept`. A task proceeds until it
reaches an :ada:`accept` statement and then waits for some other task to
synchronize with it. Specifically,

- The task with the entry waits at that point (in the :ada:`accept` statement),
  ready to accept a call to the corresponding entry from the master task.

- The other task calls the task entry, in a manner similar to a procedure
  call, to synchronize with the entry.

This synchronization between tasks is called a *rendezvous*. Let's see an
example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Rendezvous

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous is

       task T is
          entry Start;
       end T;

       task body T is
       begin
          accept Start;
          --     ^ Waiting for somebody
          --       to call the entry

          Put_Line ("In T");
       end T;

    begin
       Put_Line ("In Main");

       --  Calling T's entry:
       T.Start;
    end Show_Rendezvous;

In this example, we declare an entry :ada:`Start` for task :ada:`T`.  In the task
body, we implement this entry using :ada:`accept Start`. When task :ada:`T`
reaches this point, it waits for some other task to call its entry.
This synchronization
occurs in the :ada:`T.Start` statement. After the rendezvous completes,
the main task and task :ada:`T` again run concurrently until they synchronize
one final time when the main subprogram :ada:`Show_Rendezvous` finishes.

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
finishing, so it blocks its master task when it reaches the end of its
processing. Therefore, a loop containing :ada:`accept` statements in a task
body can be used in conjunction with a :ada:`select ... or terminate`
statement. In simple terms, this statement allows its master task to
automatically terminate the subtask when the master construct reaches its end.  For
example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Rendezvous_Loop

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous_Loop is

       task T is
          entry Reset;
          entry Increment;
       end T;

       task body T is
          Cnt : Integer := 0;
       begin
          loop
             select
                accept Reset do
                   Cnt := 0;
                end Reset;
                Put_Line ("Reset");
             or
                accept Increment do
                   Cnt := Cnt + 1;
                end Increment;
                Put_Line ("In T's loop ("
                          & Integer'Image (Cnt)
                          & ")");
             or
                terminate;
             end select;
          end loop;
       end T;

    begin
       Put_Line ("In Main");

       for I in 1 .. 4 loop
          --  Calling T's entry multiple times
          T.Increment;
       end loop;

       T.Reset;
       for I in 1 .. 4 loop
          --  Calling T's entry multiple times
          T.Increment;
       end loop;

    end Show_Rendezvous_Loop;

In this example, the task body implements an infinite loop that accepts
calls to the :ada:`Reset` and :ada:`Increment` entry. We make the following
observations:

- The :ada:`accept E do ... end` block is used to increment a counter.

    - As long as task :ada:`T` is performing the :ada:`do ... end` block, the
      main task waits for the block to complete.

- The main task is calling the :ada:`Increment` entry multiple times in the
  loop from :ada:`1 .. 4`. It is also calling the :ada:`Reset` entry before
  the second loop.

    - Because task :ada:`T` contains an infinite loop, it always accepts calls
      to the :ada:`Reset` and :ada:`Increment` entries.

    - When the master construct of the subtask (the :ada:`Show_Rendezvous_Loop`
      subprogram) completes, it checks the status of the :ada:`T`
      task. Even though task :ada:`T` could accept new calls to the
      :ada:`Reset` or :ada:`Increment` entries, the master construct is
      allowed to
      terminate task :ada:`T` due to the :ada:`or terminate` part of the
      :ada:`select` statement.

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
the :ada:`Computational_Intensive_App` procedure. In many cases, this time
drift is not relevant, so using the :ada:`delay` keyword is good enough.

However, there are situations where a time drift isn't acceptable. In those
cases, we need to use the :ada:`delay until` statement, which accepts a
precise time for the end of the delay, allowing us to define a regular
interval. This is useful, for example, in real-time applications.

We will soon see an example of how this time drift may be introduced and
how the :ada:`delay until` statement circumvents the problem. But before we
do that, we look at a package containing a procedure allowing us to measure
the elapsed time (:ada:`Show_Elapsed_Time`) and a dummy
:ada:`Computational_Intensive_App` procedure which is simulated by using a
simple delay. This is the complete package:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Real_Time; use Ada.Real_Time;

    package Delay_Aux_Pkg is

       function Get_Start_Time return Time
         with Inline;

       procedure Show_Elapsed_Time
         with Inline;

       procedure Computational_Intensive_App;
    private
       Start_Time   : Time := Clock;

       function Get_Start_Time return Time is
         (Start_Time);

    end Delay_Aux_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Delay_Aux_Pkg is

       procedure Show_Elapsed_Time is
          Now_Time     : Time;
          Elapsed_Time : Time_Span;
       begin
          Now_Time     := Clock;
          Elapsed_Time := Now_Time - Start_Time;
          Put_Line ("Elapsed time "
                    & Duration'Image
                        (To_Duration (Elapsed_Time))
                    & " seconds");
       end Show_Elapsed_Time;

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

    end Delay_Aux_Pkg;

Using this auxiliary package, we're now ready to write our time-drifting
application:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay 1.0;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             Put_Line ("Cycle # "
                       & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished time-drifting loop");
       end T;

    begin
       null;
    end Show_Time_Task;

We can see by running the application that we already have a time
difference of about four seconds after three iterations of the loop due to
the drift introduced by :ada:`Computational_Intensive_App`. Using the
:ada:`delay until` statement, however, we're able to avoid this time drift
and have a regular interval of exactly one second:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cycle : constant Time_Span :=
            Milliseconds (1000);
          Next  : Time := Aux.Get_Start_Time
                          + Cycle;

          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay until Next;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             --  Calculate next execution time
             --  using a cycle of one second
             Next := Next + Cycle;

             Put_Line ("Cycle # "
                       & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished cycling");
       end T;

    begin
       null;
    end Show_Time_Task;

Now, as we can see by running the application, the :ada:`delay until`
statement ensures that the :ada:`Computational_Intensive_App` doesn't disturb
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
conditions or other concurrent access.

.. admonition:: Important

    Objects can be protected from concurrent access using Ada tasks.
    In fact, this was the *only* way of protecting objects from concurrent
    access in Ada 83 (the first
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

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Objects

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
       Put_Line ("Number is: "
                 & Integer'Image (Obj.Get));
    end Show_Protected_Objects;

In this example, we define two operations for :ada:`Obj`: :ada:`Set` and
:ada:`Get`. The implementation of these operations is in the :ada:`Obj` body. The
syntax used for writing these operations is the same as that for normal
procedures and functions. The implementation of protected objects is
straightforward |mdash| we simply access and update :ada:`Local` in these
subprograms.  To call these operations in the main application, we use
prefixed notation, e.g., :ada:`Obj.Get`.

Entries
~~~~~~~

In addition to protected procedures and functions, you can also define
protected entry points. Do this using the :ada:`entry` keyword. Protected
entry points allow you to define barriers using the :ada:`when`
keyword. Barriers are conditions that must be fulfilled before the entry
can start performing its actual processing |mdash| we speak of *releasing* the
barrier when the condition is fulfilled.

The previous example used procedures and functions to define operations on
the protected objects. However, doing so permits reading protected
information (via :ada:`Obj.Get`) before it's set (via :ada:`Obj.Set`). To allow
that to be a defined operation, we specified a default value (0). Instead,
by rewriting :ada:`Obj.Get` using an *entry* instead of a function, we
implement a barrier, ensuring no task can read the information before it's
been set.

The following example implements the barrier for the :ada:`Obj.Get`
operation. It also contains two concurrent subprograms (main task and task
:ada:`T`) that try to access the protected object.

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Objects_Entries

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
             --  Entry is blocked until the
             --  condition is true. The barrier
             --  is evaluated at call of entries
             --  and at exits of procedures and
             --  entries. The calling task sleeps
             --  until the barrier is released.
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
object is set (by the call to :ada:`Obj.Set` in task :ada:`T`) before it reads
the information (via :ada:`Obj.Get`). Because a 4-second delay has been added
in task :ada:`T`, the main application is also delayed by 4 seconds. Only
after this delay does task :ada:`T` set the object and release the barrier in
:ada:`Obj.Get` so that the main application can then resume processing (after
the information is retrieved from the protected object).

Task and protected types
------------------------

In the previous examples, we defined single tasks and protected objects. We
can, however, generalize tasks and protected objects using type
definitions. This allows us, for example, to create multiple tasks based on
just a single task type.

.. _Intro_Ada_Task_Types:

Task types
~~~~~~~~~~

A task type is a generalization of a task. The declaration is similar to
simple tasks: you replace :ada:`task` with :ada:`task type`. The
difference between simple tasks and task types is that task types don't
create actual tasks that automatically start. Instead, a task object declaration
is needed. This is exactly the way normal variables and types work:
objects are only created by variable definitions, not type definitions.

To illustrate this, we repeat our first example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task

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

We now rewrite it by replacing :ada:`task T` with :ada:`task type TT`.  We
declare a task (:ada:`A_Task`) based on the task type :ada:`TT` after its
definition:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task_Type

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
to the individual tasks by defining a :ada:`Start` entry. Here's the updated
example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Task_Type_Array

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
          Put_Line ("In task T: "
                    & Integer'Image (Task_N));
       end TT;

       My_Tasks : array (1 .. 5) of TT;
    begin
       Put_Line ("In main");

       for I in My_Tasks'Range loop
          My_Tasks (I).Start (I);
       end loop;
    end Show_Task_Type_Array;

In this example, we're declaring five tasks in the array :ada:`My_Tasks`. We
pass the array index to the individual tasks in the entry point
(:ada:`Start`). After the synchronization between the individual subtasks and
the main task, each subtask calls :ada:`Put_Line` concurrently.

Protected types
~~~~~~~~~~~~~~~

A protected type is a generalization of a protected object. The
declaration is similar to that for protected objects: you replace
:ada:`protected` with :ada:`protected type`. Like task types,
protected types require an object declaration to create actual
objects. Again, this is similar to variable declarations and allows
for creating arrays (or other composite objects) of protected objects.

We can reuse a previous example and rewrite it to use a protected type:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Object_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Object_Type is

       protected type P_Obj_Type is
          procedure Set (V : Integer);
          function Get return Integer;
       private
          Local : Integer := 0;
       end P_Obj_Type;

       protected body P_Obj_Type is
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          function Get return Integer is
          begin
             return Local;
          end Get;
       end P_Obj_Type;

       Obj : P_Obj_Type;
    begin
       Obj.Set (5);
       Put_Line ("Number is: "
                 & Integer'Image (Obj.Get));
    end Show_Protected_Object_Type;

In this example, instead of directly defining the protected object
:ada:`Obj`, we first define a protected type :ada:`P_Obj_Type` and then
declare :ada:`Obj` as an object of that protected type. Note that the
main application hasn't changed: we still use :ada:`Obj.Set` and
:ada:`Obj.Get` to access the protected object, just like in the original
example.
