Concurrency and Real-Time
============================

.. include:: ../../../global.txt

Understanding the various options
---------------------------------

Concurrent and real-time programming are standard parts of the Ada
language. As such, they have the same semantics, whether executing on a
native target with an OS such as Linux, on a real-time operating system
(RTOS) such as VxWorks, or on a bare metal target with no OS or RTOS at
all.

For resource-constrained systems, two subsets of the Ada concurrency
facilities are defined, known as the Ravenscar and Jorvik profiles.
Though restricted, these subsets have highly desirable properties,
including: efficiency, predictability, analyzability, absence of
deadlock, bounded blocking, absence of priority inversion, a real-time
scheduler, and a small memory footprint. On bare metal systems, this
means in effect that Ada comes with its own real-time kernel.

.. admonition:: For further information

    We'll discuss the Ravenscar profile
    :ref:`later in this chapter <Ada_For_Embedded_C_Dev_Ravenscar>`. Details about the Jorvik profile
    can be found elsewhere [Jorvik]_.

    .. [Jorvik] A New Ravenscar-Based Profile by P. Rogers, J. Ruiz, T. Gingold
                and P. Bernardi, in Reliable Software Technologies |mdash| Ada
                Europe 2017, Springer-Verlag Lecture Notes in Computer Science,
                Number 10300.

Enhanced portability and expressive power are the primary advantages of
using the standard concurrency facilities, potentially resulting in
considerable cost savings. For example, with little effort, it is
possible to migrate from Windows to Linux to a bare machine without
requiring any changes to the code. Thread management and synchronization
is all done by the implementation, transparently. However, in some
situations, it’s critical to be able to access directly the services
provided by the platform. In this case, it’s always possible to make
direct system calls from Ada code. Several targets of the GNAT compiler
provide this sort of API by default, for example win32ada for Windows
and Florist for POSIX systems.

On native and RTOS-based platforms GNAT typically provides the full
concurrency facilities. In contrast, on bare metal platforms GNAT typically
provides the two standard subsets: Ravenscar and Jorvik.


Tasks
-----

Ada offers a high level construct called a *task* which is an
independent thread of execution. In GNAT, tasks are either mapped to the
underlying OS threads, or use a dedicated kernel when not available.

The following example will display the 26 letters of the alphabet twice, using
two concurrent tasks. Since there is no synchronization between the two threads
of control in any of the examples, the output may be interspersed.

[Ada]

.. code:: ada run_button ada project=Courses.Ada_For_Embedded_C_Dev.Concurrency.My_Task

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is -- implicitly called by the environment task
       subtype A_To_Z is Character range 'A' .. 'Z';

       task My_Task;

       task body My_Task is
       begin
          for I in A_To_Z'Range loop
             Put (I);
          end loop;
          New_Line;
       end My_Task;
    begin
       for I in A_To_Z'Range loop
          Put (I);
       end loop;
       New_Line;
    end Main;

Any number of Ada tasks may be declared in any declarative region. A task
declaration is very similar to a procedure or package declaration. They all
start automatically when control reaches the begin. A block will not exit until
all sequences of statements defined within that scope, including those in
tasks, have been completed.

A task type is a generalization of a task object; each object of a task type
has the same behavior. A declared object of a task type is started within the
scope where it is declared, and control does not leave that scope until the
task has terminated.

Task types can be parameterized; the parameter serves the same purpose as an
argument to a constructor in Java. The following example creates 10 tasks, each
of which displays a subset of the alphabet contained between the parameter and
the :ada:`'Z'` Character.  As with the earlier example, since there is no
synchronization among the tasks, the output may be interspersed depending on
the underlying implementation of the task scheduling algorithm.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.My_Task_Type

    package My_Tasks is

       task type My_Task (First : Character);

    end My_Tasks;

    with Ada.Text_IO; use Ada.Text_IO;

    package body My_Tasks is

       task body My_Task is
       begin
          for I in First .. 'Z' loop
             Put (I);
          end loop;
          New_Line;
       end My_Task;

    end My_Tasks;

    with My_Tasks; use My_Tasks;

    procedure Main is
       Dummy_Tab : array (0 .. 3) of My_Task ('W');
    begin
       null;
    end Main;

In Ada, a task may be dynamically allocated rather than declared
statically. The task will then start as soon as it has been allocated,
and terminates when its work is completed.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.My_Task_Type

    with My_Tasks; use My_Tasks;

    procedure Main is
       type Ptr_Task is access My_Task;

       T : Ptr_Task;
    begin
       T := new My_Task ('W');
    end Main;

Rendezvous
----------

A rendezvous is a synchronization between two tasks, allowing them to exchange
data and coordinate execution. Let's consider the following example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Rendezvous

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       task After is
          entry Go;
       end After;

       task body After is
       begin
          accept Go;
          Put_Line ("After");
       end After;

    begin
       Put_Line ("Before");
       After.Go;
    end Main;

The :ada:`Go` entry declared in :ada:`After` is the client interface to the
task. In the task body, the :ada:`accept` statement causes the task to wait for
a call on the entry. This particular :ada:`entry` and :ada:`accept` pair
simply causes the task to wait until :ada:`Main` calls
:ada:`After.Go`. So, even though the two tasks start simultaneously and execute
independently, they can coordinate via :ada:`Go`. Then, they both continue
execution independently after the rendezvous.

The :ada:`entry`/:ada:`accept` pair can take/pass parameters, and the
:ada:`accept` statement can contain a sequence of statements; while these
statements are executed, the caller is blocked.

Let's look at a more ambitious example. The rendezvous below accepts parameters
and executes some code:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Rendezvous_Params

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       task After is
          entry Go (Text : String);
       end After;

       task body After is
       begin
          accept Go (Text : String) do
             Put_Line ("After: " & Text);
          end Go;
       end After;

    begin
       Put_Line ("Before");
       After.Go ("Main");
    end Main;

In the above example, the :ada:`Put_Line` is placed in the accept statement.
Here's a possible execution trace, assuming a uniprocessor:

#. At the begin of :ada:`Main`, task :ada:`After` is started and the main
   procedure is suspended.

#. :ada:`After` reaches the :ada:`accept` statement and is suspended, since
   there is no pending call on the :ada:`Go` entry.

#. The main procedure is awakened and executes the :ada:`Put_Line` invocation,
   displaying the string :ada:`"Before"`.

#. The main procedure calls the :ada:`Go` entry.  Since :ada:`After` is
   suspended on its :ada:`accept` statement for this entry, the call succeeds.

#. The main procedure is suspended, and the task :ada:`After` is awakened to
   execute the body of the :ada:`accept` statement. The actual parameter
   :ada:`"Main"` is passed to the :ada:`accept` statement, and the
   :ada:`Put_Line` invocation is executed. As a result, the string
   :ada:`"After: Main"` is displayed.

#. When the :ada:`accept` statement is completed, both the :ada:`After` task
   and the main procedure are ready to run.  Suppose that the :ada:`Main`
   procedure is given the processor. It reaches its end, but the local task
   :ada:`After` has not yet terminated.  The main procedure is suspended.

#. The :ada:`After` task continues, and terminates since it is at its end.  The
   main procedure is resumed, and it too can terminate since its dependent task
   has terminated.

The above description is a conceptual model; in practice the implementation can
perform various optimizations to avoid unnecessary context switches.

Selective Rendezvous
---------------------

The :ada:`accept` statement by itself can only wait for a single event (call)
at a time. The :ada:`select` statement allows a task to listen for multiple
events simultaneously, and then to deal with the first event to occur. This
feature is illustrated by the task below, which maintains an integer value that
is modified by other tasks that call :ada:`Increment`, :ada:`Decrement`, and
:ada:`Get`:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Selective_Rendezvous

    package Counters is

       task Counter is
          entry Get (Result : out Integer);
          entry Increment;
          entry Decrement;
       end Counter;

    end Counters;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Counters is

       task body Counter is
          Value : Integer := 0;
       begin
          loop
             select
                accept Increment do
                   Value := Value + 1;
                end Increment;
             or
                accept Decrement do
                   Value := Value - 1;
                end Decrement;
             or
                accept Get (Result : out Integer) do
                   Result := Value;
                end Get;
             or
                delay 5.0;
                Put_Line ("Exiting Counter task...");
                exit;
             end select;
          end loop;
       end Counter;

    end Counters;

    with Ada.Text_IO; use Ada.Text_IO;
    with Counters;    use Counters;

    procedure Main is
       V : Integer;
    begin
       Put_Line ("Main started.");

       Counter.Get (V);
       Put_Line ("Got value. Value = " & Integer'Image (V));

       Counter.Increment;
       Put_Line ("Incremented value.");

       Counter.Increment;
       Put_Line ("Incremented value.");

       Counter.Get (V);
       Put_Line ("Got value. Value = " & Integer'Image (V));

       Counter.Decrement;
       Put_Line ("Decremented value.");

       Counter.Get (V);
       Put_Line ("Got value. Value = " & Integer'Image (V));

       Put_Line ("Main finished.");
    end Main;

When the task's statement flow reaches the select, it will wait for all four
events |mdash| three entries and a delay |mdash| in parallel. If the delay of
five seconds is exceeded, the task will execute the statements following the
:ada:`delay` statement (and in this case will exit the loop, in effect
terminating the task). The :ada:`accept` bodies for the :ada:`Increment`,
:ada:`Decrement`, or :ada:`Get` entries will be otherwise executed as they're
called. These four sections of the select statement are mutually exclusive: at
each iteration of the loop, only one will be invoked. This is a critical point;
if the task had been written as a package, with procedures for the various
operations, then a *race condition* could occur where multiple tasks
simultaneously calling, say, :ada:`Increment`, cause the value to only get
incremented once. In the tasking version, if multiple tasks simultaneously call
:ada:`Increment` then only one at a time will be accepted, and the value will
be incremented by each of the tasks when it is accepted.

More specifically, each entry has an associated queue of pending callers.  If a
task calls one of the entries and Counter is not ready to accept the call
(i.e., if :ada:`Counter` is not suspended at the :ada:`select` statement) then
the calling task is suspended, and placed in the queue of the entry that it is
calling.  From the perspective of the :ada:`Counter` task, at any iteration of
the loop there are several possibilities:

* There is no call pending on any of the entries.  In this case :ada:`Counter`
  is suspended.  It will be awakened by the first of two events: a call on one
  of its entries (which will then be immediately accepted), or the expiration
  of the five second delay (whose effect was noted above).

* There is a call pending on exactly one of the entries.  In this case control
  passes to the :ada:`select` branch with an :ada:`accept` statement for that
  entry.

* There are calls pending on more than one entry.  In this case one of the
  entries with pending callers is chosen, and then one of the callers is chosen
  to be de-queued. The choice of which caller to accept depends on
  the queuing policy, which can be specified via a :ada:`pragma` defined in the
  Real-Time Systems Annex of the Ada standard; the default is
  *First-In First-Out*.

Protected Objects
-----------------

Although the rendezvous may be used to implement mutually exclusive access to a
shared data object, an alternative (and generally preferable) style is through
a protected object, an efficiently implementable mechanism that makes the
effect more explicit. A protected object has a public interface (its protected
operations) for accessing and manipulating the object's components (its private
part). Mutual exclusion is enforced through a conceptual lock on the object,
and encapsulation ensures that the only external access to the components are
through the protected operations.

Two kinds of operations can be performed on such objects: read-write operations
by procedures or entries, and read-only operations by functions. The lock
mechanism is implemented so that it's possible to perform concurrent read
operations but not concurrent write or read/write operations.

Let's reimplement our earlier tasking example with a protected object called
:ada:`Counter`:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Protected_Counter

    package Counters is

       protected Counter is
          function Get return Integer;
          procedure Increment;
          procedure Decrement;
       private
          Value : Integer := 0;
       end Counter;

    end Counters;

    package body Counters is

       protected body Counter is
          function Get return Integer is
          begin
             return Value;
          end Get;

          procedure Increment is
          begin
             Value := Value + 1;
          end Increment;

          procedure Decrement is
          begin
             Value := Value - 1;
          end Decrement;
       end Counter;

    end Counters;

Having two completely different ways to implement the same paradigm might seem
complicated. However, in practice the actual problem to solve usually drives
the choice between an active structure (a task) or a passive structure (a
protected object).

A protected object can be accessed through prefix notation:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Protected_Counter

    with Ada.Text_IO; use Ada.Text_IO;
    with Counters;    use Counters;

    procedure Main is
    begin
       Counter.Increment;
       Counter.Decrement;
       Put_Line (Integer'Image (Counter.Get));
    end Main;

A protected object may look like a package syntactically, since it contains
declarations that can be accessed externally using prefix notation. However,
the declaration of a protected object is extremely restricted; for example, no
public data is allowed, no types can be declared inside, etc. And besides the
syntactic differences, there is a critical semantic distinction: a protected
object has a conceptual lock that guarantees mutual exclusion; there is no such
lock for a package.

Like tasks, it's possible to declare protected types that can be instantiated
several times:

.. code-block:: ada

    declare
       protected type Counter is
          -- as above
       end Counter;

       protected body Counter is
          -- as above
       end Counter;

       C1 : Counter;
       C2 : Counter;
    begin
       C1.Increment;
       C2.Decrement;
       .. .
    end;

Protected objects and types can declare a procedure-like operation known as an
*entry*. An entry is somewhat similar to a procedure but includes a so-called
barrier condition that must be true in order for the entry invocation to
succeed. Calling a protected entry is thus a two step process: first, acquire
the lock on the object, and then evaluate the barrier condition.  If the
condition is true then the caller will execute the entry body.  If the
condition is false, then the caller is placed in the queue for the entry, and
relinquishes the lock.  Barrier conditions (for entries with non-empty queues)
are reevaluated upon completion of protected procedures and protected entries.

Here's an example illustrating protected entries: a protected type that models
a binary semaphore / persistent signal.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Protected_Binary_Semaphore

    package Binary_Semaphores is

       protected type Binary_Semaphore is
          entry Wait;
          procedure Signal;
       private
          Signaled : Boolean := False;
       end Binary_Semaphore;

    end Binary_Semaphores;

    package body Binary_Semaphores is

       protected body Binary_Semaphore is
          entry Wait when Signaled is
          begin
             Signaled := False;
          end Wait;

          procedure Signal is
          begin
             Signaled := True;
          end Signal;
       end Binary_Semaphore;

    end Binary_Semaphores;

    with Ada.Text_IO;       use Ada.Text_IO;
    with Binary_Semaphores; use Binary_Semaphores;

    procedure Main is
       B : Binary_Semaphore;

       task T1;
       task T2;

       task body T1 is
       begin
          Put_Line ("Task T1 waiting...");
          B.Wait;

          Put_Line ("Task T1.");
          delay 1.0;

          Put_Line ("Task T1 will signal...");
          B.Signal;

          Put_Line ("Task T1 finished.");
       end T1;

       task body T2 is
       begin
          Put_Line ("Task T2 waiting...");
          B.Wait;

          Put_Line ("Task T2");
          delay 1.0;

          Put_Line ("Task T2 will signal...");
          B.Signal;

          Put_Line ("Task T2 finished.");
       end T2;

    begin
       Put_Line ("Main started.");
       B.Signal;
       Put_Line ("Main finished.");
    end Main;

Ada concurrency features provide much further generality than what's been
presented here. For additional information please consult one of the works
cited in the *References* section.


.. _Ada_For_Embedded_C_Dev_Ravenscar:

Ravenscar
---------

The Ravenscar profile is a subset of the Ada concurrency facilities that
supports determinism, schedulability analysis, constrained memory utilization,
and certification to the highest integrity levels. Four distinct application
domains are intended:

- hard real-time applications requiring predictability,

- safety-critical systems requiring formal, stringent certification,

- high-integrity applications requiring formal static analysis and
  verification,

- embedded applications requiring both a small memory footprint and low
  execution overhead.

Tasking constructs that preclude analysis, either technically or economically,
are disallowed. You can use the :ada:`pragma Profile (Ravenscar)` to indicate
that the Ravenscar restrictions must be observed in your program.

Some of the examples we've seen above will be rejected by the compiler when
using the Ravenscar profile. For example:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Ravenscar
    :class: ada-expect-compile-error

    package My_Tasks is

       task type My_Task (First : Character);

    end My_Tasks;

    with Ada.Text_IO; use Ada.Text_IO;

    package body My_Tasks is

       task body My_Task is
       begin
          for C in First .. 'Z' loop
             Put (C);
          end loop;
          New_Line;
       end My_Task;

    end My_Tasks;

    pragma Profile (Ravenscar);

    with My_Tasks; use My_Tasks;

    procedure Main is
       Tab : array (0 .. 3) of My_Task ('W');
    begin
       null;
    end Main;

This code violates the `No_Task_Hierarchy` restriction of the Ravenscar
profile. This is due to the declaration of :ada:`Tab` in the :ada:`Main`
procedure. Ravenscar requires task declarations to be done at the library level.
Therefore, a simple solution is to create a separate package and reference it
in the main application:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Concurrency.Ravenscar

    with My_Tasks; use My_Tasks;

    package My_Task_Inst is

       Tab : array (0 .. 3) of My_Task ('W');

    end My_Task_Inst;

    pragma Profile (Ravenscar);

    with My_Task_Inst;

    procedure Main is
    begin
       null;
    end Main;

Also, Ravenscar prohibits entries for tasks. For example, we're not allowed to
write this declaration:

.. code-block:: ada

    task type My_Task (First : Character) is
       entry Start;
    end My_Task;

You can use, however, one entry per protected object. As an example, the
declaration of the :ada:`Binary_Semaphore` type that we've discussed before
compiles fine with Ravenscar:

.. code-block:: ada

    protected type Binary_Semaphore is
       entry Wait;
       procedure Signal;
    private
       Signaled : Boolean := False;
    end Binary_Semaphore;

We could add more procedures and functions to the declaration of
:ada:`Binary_Semaphore`, but we wouldn't be able to add another entry when
using Ravenscar.

Similar to the previous example with the task array declaration, objects of
:ada:`Binary_Semaphore` cannot be declared in the main application:

.. code-block:: ada

    procedure Main is
       B : Binary_Semaphore;
    begin
       null;
    end Main;

This violates the `No_Local_Protected_Objects` restriction. Again, Ravenscar
expects this declaration to be done on a library level, so a solution to make
this code compile is to have this declaration in a separate package and
reference it in the :ada:`Main` procedure.

Ravenscar offers many additional restrictions. Covering those would exceed
the scope of this chapter. You can find more examples using the Ravenscar
profile on
`this blog post <https://blog.adacore.com/theres-a-mini-rtos-in-my-language>`_.
