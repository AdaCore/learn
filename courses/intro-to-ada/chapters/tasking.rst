Tasking
=======
:code-config:`reset_accumulator=True`

Tasks and protected objects allow for implementing concurrency in Ada. The
following sections explain those concepts in more details.

Tasks
-----

A task can be thought as an application that runs *concurrently* with
the main application. In other programming languages, a task may also be
called a `thread <https://en.wikipedia.org/wiki/Thread_(computing)>`_,
and tasking may be called
`multithreading  <https://en.wikipedia.org/wiki/Thread_(computing)#Multithreading>`_.

Tasks may synchronize with the main application, but they also may process
information completely independent from the main application. This section
will show how this is accomplished.

Simple task
~~~~~~~~~~~

Tasks can be declared by using the keyword :ada:`task`. The task
implementation is defined in a :ada:`task body` block. For example:

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

Here, we're declaring and implementing the task ``T``. As soon as the
main application starts, task ``T`` will also start automatically --- it's
not necessary to manually start this task. By running the application
above, we can see that both calls to :ada:`Put_Line` are performed.

Note that:

- The main application is a task itself.

  - In this case, the subprogram ``Show_Simple_Task`` is the main task of
    the application.

- Task ``T`` is a subtask.

  - Each subtask has a master task.

  - Therefore, the main task is also the master task of task ``T``.

- The number of tasks is not limited to one: we could include a
  task ``T2`` in the example above.

  - This task would also start automatically and run *concurrently* with
    task ``T`` and the main task. For example:

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

As we've just seen, as soon as the main task starts, its subtasks
also start automatically. The main task will continue its
processing until it reaches the end of its implementation. At this point,
however, it will not finish. Instead, the main task will wait until its
subtasks have finished before it finishes itself. In other words, after
this waiting process, a synchronization between the main task and its
subtasks occurs. After this final synchronization, the main task may
finish. For example:

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

Note that the same mechanism is used for other subprograms that contain
subtasks: the subprogram's master task will wait for its subtasks to
finish. In other words, this mechanism is not limited to the main
application, but is also applied to any subprogram called by the main
application or its subprograms.

A synchronization is also achieved if we move the task to a separate
package. In the example below, we declare a task ``T`` in the package
``Simple_Sync_Pkg``.

.. code:: ada

    package Simple_Sync_Pkg is
       task T;
    end Simple_Sync_Pkg;

This is the corresponding package implementation:

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

As soon as the package is :ada:`with`'ed for the main procedure, the task
``T`` defined in the package will be part of the main task. For example:

.. code:: ada

    with Simple_Sync_Pkg;

    procedure Test_Simple_Sync_Pkg is
    begin
       null;
       --  Will wait here until all tasks have terminated
    end Test_Simple_Sync_Pkg;

Again, as soon as the main task reaches its end, it will synchronize with
task ``T`` from ``Simple_Sync_Pkg`` before finishing.

Delay
~~~~~

A delay may be introduced by using the keyword :ada:`delay`. This will
put the task to sleep for the amount of seconds specified in the delay
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
time it displays the "hello" message. Also, the main task is waiting 1.5
seconds and displaying another "hello" message

Synchronization: rendez-vous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous examples, the only kind of synchronization we've seen was
the one that happens automatically at the end of the main task. In
addition to that, it is possible to define custom synchronization points
using the keyword :ada:`entry`. An entry can be viewed as a special kind
of subprogram, and it be called by the master task using a similar syntax,
as we will see later.

In the task implementation, we shall define in which part of the task the
entries will be accepted by using the keyword :ada:`accept`. A task will
process its statements until it reaches an :ada:`accept` statement. At
this point, it will wait for the master task to synchronize with the it.
In other words:

- The subtask will wait at this point (in the :ada:`accept` statement),
  ready to accept a call from the master task to the corresponding entry.

- The master task will call the task entry in order to synchronize with
  the subtask --- similar to a procedure call.

The synchronization between tasks is called rendez-vous. Let's see an
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

In this example, we declare an entry ``Start`` for the task ``T``.
In the task body, we implement this entry by using :ada:`accept Start`.
When the task ``T`` reaches this point, it will wait for the master task
to synchronize. This synchronization happens in the ``T.Start`` statement.
After the synchronization is finished, the main task and task ``T``
will run concurrently until they synchronize again when the main
task reaches its end.

Note that an entry may be used to perform more than just a simple
task synchronization: we may also perform multiple statements during
the time both tasks are synchronized. This is achieved by using a
:ada:`do ... end` block. For the previous example, we would simply write
:ada:`accept Start do <statements>; end;`. We will use this kind of block
in the next example.

Select loop
~~~~~~~~~~~

There is no limit for the amount of times an entry might be accepted in
the task implementation. In fact, we could create an infinite loop in the
task implementation and accept calls to the same entry over and over. An
infinite loop, however, would prevent the subtask from finishing, so it
would also block the master task when it reaches the end of its
processing. Therefore, a loop containing :ada:`accept` statements in a
task body is normally implemented together with a
:ada:`select ... or terminate` statement. In simple terms, this statement
allows the master task to terminate the subtask when the end of the master
task is reached. For example:

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
calls to the ``Start`` entry. We can make the following observations:

- In this case, an :ada:`accept E do ... end` block is used, where a
  counter is incremented.

    - As long as task ``T`` is performing the :ada:`do ... end` block, the
      main task will wait for the block to finish.

- In addition, the main task is calling the ``Start`` entry  multiple
  times in the loop from ``1 .. 4``.

    - Because task ``T`` uses an infinite loop, it will always accept
      calls to the ``Start`` entry.

    - When the main task reaches the end, it will check the status of the
      ``T`` task. Even though task ``T`` could accept new calls to the
      ``Start`` entry, the master task is allowed to terminate task ``T``
      due to the :ada:`or terminate` part of the :ada:`select` statement.

Cycling tasks
~~~~~~~~~~~~~

In a previous example, we've seen that we can delay a task by a given
amount of seconds using :ada:`delay` keyword. When using delay statements
in a loop, however, we cannot expect to have regular interval between the
delay statements. For example, we may have a call to a computationally
intensive procedure between the delay statements:

.. code-block:: ada

          while True loop
             delay 1.0;
             --    ^ Wait 1.0 seconds
             Computational_Intensive_App;
          end loop;

In this case, we cannot guarantee that, after 10 calls to the delay
statement, the time span is just 10 seconds. In fact, a time drift may be
introduced by the ``Computational_Intensive_App`` procedure. In many
cases, this time drift is not relevant, so that using the :ada:`delay`
keyword is good enough.

There are situations, however, where a time drift is not acceptable. In
this case, we need to use the :ada:`delay until` statement, which accepts
a precise time for the next execution, so that a regular interval can be
defined. This is useful, for example, in real-time applications.

We will see an example of how this time drift may be introduced, and how
the :ada:`delay until` statement circumvents the problem. Before we do
that, we will look into an auxiliary package containing a procedure that
allows for measuring the elapsed time (``Show_Elapsed_Time``) and a dummy
``Computational_Intensive_App`` procedure using a simple delay. This is
the package specification:

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

This is the package definition:

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

As we can see by running the application, due to the drift introduced by
``Computational_Intensive_App``, after three iterations of the loop, we
already have a time span of about four seconds. Using the
:ada:`delay until` statement, however, we'll be able to avoid this time
drift and have a regular interval of one second:

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

As we can see by running the application, the :ada:`delay until` statement
makes sure that the ``Computational_Intensive_App`` does not affect the
regular interval of one second between the iterations.

Protected objects
-----------------

In situations where multiple tasks are accessing shared data, data
corruption may happen. For example, data will be inconsistent when one
task overwrites parts of the information that is being read by another
task. In order to avoid this kind of problems and ensure that the
information is accessed in a coordinated way, we can use protected
objects.

Protected objects encapsulate data and provide access to this data by
means of protected operations. These protected operations may be
subprograms or protected entries. Using protected objects ensures that the
data will not be corrupted by race conditions.

.. admonition:: Important

    It is possible to implement protected objects using Ada tasks. In
    fact, this was the only possible way of implementing them in Ada 83
    (the first version of the Ada language). However, the use of protected
    objects greatly simplify the implementation when compared to similar
    mechanisms implemented strictly with tasks. Therefore, the
    recommendation is to use protected objects when the main goal is to
    just protect data.

Simple object
~~~~~~~~~~~~~

A protected object is declared by using the :ada:`protected` keyword. The
syntax is similar to the one used for packages: we can declare operations
(e.g.: procedures and functions) in the public part, and data in the
private part. The corresponding implementation of the operations is
included in the :ada:`protected body` of the object. For example:

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

In this example, we are defining two operations for ``Obj``: ``Set`` and
``Get``. The implementation of these operations can be found in the
``Obj`` body. The syntax used for implementing these operations is the
same as the one used for common procedures and functions. Therefore  the
implementation of protected objects is straightforward --- we simply
access and update ``Local`` in these subprograms. In order to call these
operations in the main application, we use the prefixed notation, e.g.:
``Obj.Get``.

Entries
~~~~~~~

In addition to protected procedures and functions, we may also define
protected entry points. This is achieved by using the :ada:`entry`
keyword. Protected entry points allow for defining barriers using the
:ada:`when` keyword. Barriers are conditions that must be fulfilled before
the actual processing defined in the entry can start.

In the previous example, we've used procedures and functions to define
our operations on the protected objects. However, this implementation
allows for reading the protected information (via ``Obj.Get``) before the
information is set (via ``Obj.Set``). In that case, we have defined a
default value (0). By rewriting ``Obj.Get`` and using an entry instead of
a function, we may implement a barrier: this ensures that no task will
read the information before it has been first set.

The following example implements the barrier for the ``Obj.Get``
operation. Also, it contains two concurrent subprograms (main task
and task ``T``) that try to access the protected object.

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
             --  Entry will be blocked until the condition is true.
             --  Barrier is evaluated at call of entry, and at exit of
             --  procedures and entries.
             --  Calling task will sleep until the barrier is relieved.
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

As we can see by running the application, the main application waits until
the protected object is set (by the call to ``Obj.Set`` in task ``T``)
before it reads the information (via ``Obj.Get``). Because a 4-second
delay has been added in task ``T``, the main application will also be
delayed by 4 seconds. Only after this delay, task ``T`` will set the
object and release the barrier set by ``Obj.Get``, so that the main
application can then restore processing (after the information from the
protected object is retrieved).

Task and protected types
------------------------

In the previous examples, we have defined single tasks and protected
objects. It is possible, however, to generalize tasks and protected
objects using type definitions. This allows, for example, for creating
multiple tasks based on just a single task type.

Task types
~~~~~~~~~~

A task type is a generalization of a task. The declaration is similar to
simple tasks: you just have to replace :ada:`task` by :ada:`task type`.
The main difference between simple tasks and task types is that task types
don't create actual tasks that automatically start. Instead, a task
declaration is needed for that --- similar to variable declarations.

For example, we may reuse our first example:

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

We can rewrite the example and replace ``task T`` by ``task type TT``.
After the type definition is complete, we declare a task (``A_Task``)
based on the task type ``TT``:

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

We may extend this example and create an array of tasks. Since we're using
the same syntax as for variable declarations, we can simply use a similar
syntax for task types: :ada:`array (<>) of Task_Type`. Also, we may pass
information to the individual tasks by defining a ``Start`` entry. This
is the updated example:

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

In the example above, we're declaring five tasks in the array
``My_Tasks``. We pass the array index to the individual tasks in the
start entry point (``Start``). After the synchronization with the
individual subtasks in the main task, each subtask will proceed to call
``Put_Line`` concurrently.

Protected types
~~~~~~~~~~~~~~~

A protected type is a generalization of a protected object. The
declaration is similar to protected objects: you just have to replace
:ada:`protected` by :ada:`protected type`. However, similar to task types,
protected types require an object declaration in order to create actual
objects. Again, this is similar to variable declarations and allows for
creating arrays of protected objects, for example.

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
``Obj``, we first define a protected type ``Obj_Type`` and then declare
``Obj`` based on that protected type. Note that the main application
hasn't change: we still use ``Obj.Set`` and ``Obj.Get`` to access the
protected object as in the original example.
