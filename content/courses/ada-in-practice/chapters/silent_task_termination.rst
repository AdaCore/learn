.. _Ada_In_Practice_Silent_Task_Termination:

Dealing with Silent Task Termination
====================================


.. include:: ../../global.txt


Motivation
----------

A task completes abnormally when an exception is raised in its sequence of
statements and is not handled. Even if the task body has a matching exception
handler and it executes, the task still completes after the handler executes,
although this time it completes normally. Similarly, if a task is aborted the
task completes, again abnormally.

Whatever the cause, once completed a task will (eventually) terminate, and it
does this silently |mdash| there is no notification or logging of the
termination to the external environment. A vendor could support notification by
their run-time library [#f1]_, but the language standard does not require it
and most vendors |mdash| if not all |mdash| do not.

Nevertheless, applications may require some sort of notification of the event
that caused the termination. Assuming the developer is responsible for
implementing it, how can the requirement best be met?


Solution
--------

For unhandled exceptions, the simplest solution to silent termination is to
define the announcement or logging response as an exception handler located in
the task body exception handler part:

.. code-block:: ada

    with Ada.Exceptions; use Ada.Exceptions;
    with Ada.Text_IO;    use Ada.Text_IO;
    -- ...
       task body Worker is
       begin
          --  ...
       exception
          when Error : others => -- last wishes
             Put_Line ("Task T terminated due to " & Exception_Name (Error));
       end Worker;

A handler at this level expresses the task's *last wishes* prior to completion,
in this case printing the names of the task and the active exception to
:ada:`Standard_Output`. The :ada:`others` choice covers all exceptions not
previously covered, so in the above it covers all exceptions. Specific
exceptions also could be covered, but the :ada:`others` choice should be
included (at the end) to ensure no exception occurrence can be missed.

You'll probably want this sort of handler for every application task if you
want it for any of them. That's somewhat inconvenient if there are many tasks
in the application, but feasible. Possible mitigation includes the use of a
task type. In that case you need only define the handler once, in the task
type's body. You could even declare such a task type inside a generic package,
with generic formal subprograms for the normal processing and the exception
handler's processing. That would make the task type reusable.  But that's a bit
heavy, and it can be awkward. See the
:ref:`Notes <Ada_In_Practice_Silent_Task_Termination_Notes>` section below for
details.

Alternatively, we could prevent unhandled exceptions from causing termination
in the first place. We can do that by preventing task completion, via some
additional constructs that prevent reaching the end of the task's sequence of
statements. We will show these constructs incrementally for the sake of
clarity.

Before we do, note that many tasks are intended to run until power is removed,
so they have an infinite loop at the outer level as illustrated in the code
below. For the sake of clarity and realism, we name the loop :ada:`Normal` and
call some procedures to show the typical structure, along with the last wishes
handler:

.. code-block:: ada

    task body Worker is
    begin
       Initialize_State;
       Normal : loop
          Do_Actual_Work;
       end loop Normal;
    exception
       when Error : others => -- last wishes
          Put_Line ("Task T terminated due to " &
                    Exception_Name (Error));
    end Worker;

In the above, the procedures' names indicate what is done at that point in the
code. The steps performed may or may not be done by actual procedure calls.

Strictly speaking, the optional exception handler part of the task body is the
very end of the task's sequence of statements (the *handled sequence of
statements*). We want to prevent the thread of control reaching that end
|mdash| which would happen if any handler there ever executed |mdash| because
the task would then complete.

Therefore, for the first additional construct, we first wrap the existing code
inside a block statement. The task body's exception handler section becomes
part of the block statement rather than at the top level of the task:

.. code-block:: ada

    task body Worker is
    begin
       begin
          Initialize_State;
          Normal : loop
             Do_Actual_Work;
          end loop Normal;
       exception
          when Error : others =>  -- last wishes
             Put_Line ("Task T terminated due to " &
                       Exception_Name (Error));
       end;
    end Worker;

Now any exception raised within :ada:`Initialize_State` and
:ada:`Do_Actual_Work` will be caught in the block statement's handler, not the
final part of the task's sequence of statements. Nothing else changes,
semantically. The task will still complete because the block statement exits
after the handler executes, and so far there's nothing after that block
statement. We need to make one more addition.

The second (final) addition prevents reaching the end of the sequence of
statements after a handler executes, and hence the task from completing. This
is accomplished by wrapping the new block statement inside a new loop
statement. We name this outermost loop :ada:`Recovery`:

.. code-block:: ada

    task body Worker is
    begin
       Recovery : loop
          begin
             Initialize_State;
             Normal : loop
                Do_Actual_Work;
             end loop Normal;
          exception
             when Error : others =>
                Put_Line ("Task T terminated due to " &
                          Exception_Name (Error));
          end;
       end loop Recovery;
    end Worker;

When the block exits after the exception handler prints the announcement,
normal execution resumes and the end of the :ada:`Recovery` loop is reached.
The thread of control then continues at the top of the loop. Of course, absent
an unhandled exception reaching this level, the :ada:`Normal` loop is never
exited in the first place.

These two additions ensure that :ada:`Worker` never terminates due to an
unhandled exception raised during execution of the task's sequence of
statements. Note that an exception raised during elaboration of the task
body's declarative part is not handled by the approach, or any other approach
at this level, because the exception is propagated immediately to the master
of this task. Such a task never reaches the handled sequence of statements in
the first place.

That works, but the state initialization requires some thought. As shown above,
full initialization is performed again when the :ada:`Recovery` loop circles
back around to the top of the loop. As a result, the *normal* processing in
:ada:`Do_Normal_Work` must be prepared for suddenly encountering completely
different state, i.e., a restart to the initial state. If that is not feasible
the call to :ada:`Initialize_State` could be moved outside, prior to the start
of the :ada:`Recovery` loop, so that it only executes once. Perhaps a different
initialization procedure could be called after the exception handler to do
partial initialization. Whether or not that will suffice depends on the
application.

However, these solutions do not address task termination due to task abort
statements.

Aborting tasks is both messy and expensive at run-time. If a task is updating
some object and is aborted before it finishes the update, that object is
potentially corrupted. That's the messiness. If an aborted task has dependent
tasks, all the dependents are aborted too, transitively. A task in a rendezvous
with the aborted task is affected, as are those queued waiting to rendezvous
with it, and so on. That's part of the expensiveness when aborts are actually
used. Worse, even if never used, abort statements impose an expense at
run-time. The language semantics requires checks for an aborted task at certain
places within the run-time library. Those checks are executed even if no task
abort statement is ever used in the application. To avoid that distributed
cost, you would need to apply a tasking profile disallowing abort statements
and build the executable with a correspondingly reduced run-time library
implementation.

As a consequence, aborting a task should be very rarely done. Regardless, the
task abort statement exists. How can we express a *last wishes* response for
that cause too?

Fortunately, Ada provides a facility that addresses all possible causes: normal
termination, termination due to task abort, and termination due to unhandled
exceptions.

With this facility developers specify procedures that are invoked automatically
by the run-time library during task finalization. These procedures express the
last wishes for the task, but do not require any source code within the task,
unlike the exception handler in each task body described earlier.  These
response procedures are known as *handlers.*

During execution, handlers can be applied to an individual task or to groups of
related tasks. Handlers can also be removed from those tasks or replaced with
other handlers. Because procedures are not first-class entities in Ada,
handlers are assigned and removed by passing access values designating them.

The facility is defined by package :ada:`Ada.Task_Termination`. The package
declaration for this language-defined facility follows, with slight changes for
easier comprehension.

.. code-block:: ada

    with Ada.Task_Identification;  use Ada.Task_Identification;
    with Ada.Exceptions;           use Ada.Exceptions;

    package Ada.Task_Termination
       --  ...
    is
       type Cause_Of_Termination is (Normal, Abnormal, Unhandled_Exception);

       type Termination_Handler is access protected procedure
         (Cause : in Cause_Of_Termination;
          T     : in Task_Id;
          X     : in Exception_Occurrence);

       procedure Set_Dependents_Fallback_Handler
         (Handler : in Termination_Handler);

       function Current_Task_Fallback_Handler
         return Termination_Handler;

       procedure Set_Specific_Handler
         (T       : in Task_Id;
          Handler : in Termination_Handler);

       function Specific_Handler (T : Task_Id) return Termination_Handler;
    end Ada.Task_Termination;

As shown, termination handlers are actually protected procedures, with a
specific parameter profile. Therefore, the type :ada:`Termination_Handler` is
an access-to-protected-procedure with that signature. The compiler ensures that
any designated protected procedure matches the parameter profile.

Termination handlers apply either to a specific task or to a group of related
tasks, including potentially all tasks in the partition.  Each task has one,
both, or neither kind of handler. By default none apply.

Clients call procedure :ada:`Set_Specific_Handler` to apply the protected
procedure designated by :ada:`Handler` to the task with the specific
:ada:`Task_Id` value :ada:`T`. These are known as *specific* handlers. The use
of a :ada:`Task_Id` to specify the task, rather than the task name, means that
we can set or remove a handler without having direct visibility to the task in
question.

Clients call procedure :ada:`Set_Dependents_Fallback_Handler` to apply the
protected procedure designated by :ada:`Handler` to the task making the call,
i.e., the current task, and to all tasks that are dependents of that task.
These handlers are known as *fall-back* handlers.

Handlers are invoked automatically, with the following semantics:

#. If a specific handler is set for the terminating task, it is called and then
   the response finishes.

#. If no specific handler is set for the terminating task, the run-time library
   searches for a fall-back handler. The search is recursive, up the hierarchy
   of task masters, including, ultimately, the environment task. If no
   fall-back handler is found no handler calls are made whatsoever. If a
   fall-back handler is found it is called and then the response finishes; no
   further searching or handler calls occur.

As a result, at most one handler is called in response to any given task
termination.

The following client package illustrates the approach. Package
:ada:`Obituary` declares protected object :ada:`Obituary.Writer`, which
declares two protected procedures. Both match the profile specified by type
:ada:`Termination_Handler`.  One such procedure would suffice, we just provide
two for the sake of illustrating the flexibility of the dynamic approach.

.. code:: ada no_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary
    :class: ada-syntax-only

    with Ada.Exceptions;          use Ada.Exceptions;
    with Ada.Task_Termination;    use Ada.Task_Termination;
    with Ada.Task_Identification; use Ada.Task_Identification;

    package Obituary is

       protected Writer is

          procedure Note_Passing
             (Cause    : Cause_Of_Termination;
              Departed : Task_Id;
              Event    : Exception_Occurrence);
          --  Written by someone who's read too much English lit

          procedure Dissemble
             (Cause    : Cause_Of_Termination;
              Departed : Task_Id;
              Event    : Exception_Occurrence);
          --  Written by someone who may know more than they're saying

       end Writer;

    end Obituary;

Clients can choose among these protected procedures to set a handler for one or
more tasks.

The two protected procedures display messages corresponding to the cause of the
termination. One procedure prints respectful messages, in the style of someone
who's read too much Old English literature. The other prints rather dissembling
messages, as if written by someone who knows more than they are willing to say.
The point of the difference is that more than one handler can be available to
clients, and their choice is made dynamically at run-time.

The package body is structured as follows:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    package body Obituary is

       protected body Writer is
          procedure Note_Passing () is ...
          procedure Dissemble () is ...
       end Writer;

    begin -- optional package executable part
       Set_Dependents_Fallback_Handler (Writer.Note_Passing'Access);
    end Obituary;

In addition to defining the bodies of the protected procedures, the package
body has an executable part. That part is optional, but in this case it is
convenient. This executable part calls procedure
:ada:`Set_Dependents_Fallback_Handler` to apply one of the two handlers.
Because this call happens during library unit elaboration, it sets the
fall-back handler for all the tasks in the partition (the program). The effect
is global to the partition because library unit elaboration is invoked by the
*environment task,* and the environment task is the master of all application
tasks in the partition. Therefore, the fall-back handler is applied to the top
of the task dependents hierarchy, and thus to all tasks. The application tasks
need not do anything in their source code for the handler to apply to them.

The call to :ada:`Set_Dependents_Fallback_Handler` need not occur in this
particular package body, or even in a package body at all. But because we want
it to apply to all tasks in this specific example, including library tasks,
placement in a library package's elaboration achieves that effect.

The observant reader will note the with-clause for :ada:`Ada.Text_IO`,
included for the sake of references to :ada:`Put_Line`. We'll address the
ramifications momentarily. Here are the bodies for the two handlers:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary

    with Ada.Text_IO;  use Ada.Text_IO;

    package body Obituary is

       protected body Writer is

          procedure Note_Passing
            (Cause    : Cause_Of_Termination;
             Departed : Task_Id;
             Event    : Exception_Occurrence)
          is
          begin
             case Cause is
                when Normal =>
                   Put_Line (Image (Departed) &
                             " went gently into that good night");
                when Abnormal =>
                   Put_Line (Image (Departed) & " was most fouly murdered!");
                when Unhandled_Exception =>
                   Put_Line (Image (Departed) &
                             " was unknit by the much unexpected " &
                             Exception_Name (Event));
             end case;
          end Note_Passing;

          procedure Dissemble
            (Cause    : Cause_Of_Termination;
             Departed : Task_Id;
             Event    : Exception_Occurrence)
          is
          begin
             case Cause is
                when Normal =>
                   Put_Line (Image (Departed) & " died, naturally.");
                   Put_Line ("We had nothing to do with it.");
                when Abnormal =>
                   Put_Line (Image (Departed) & " had a tragic accident.");
                   Put_Line ("We're sorry it had to come to that.");
                when Unhandled_Exception =>
                   Put_Line (Image (Departed) &
                             " was apparently fatally allergic to " &
                             Exception_Name (Event));
             end case;
          end Dissemble;

       end Writer;

    begin -- optional package executable part
       Set_Dependents_Fallback_Handler (Writer.Note_Passing'Access);
    end Obituary;


Now, about those calls to :ada:`Ada.Text_IO.Put_Line`. Because of those calls,
the bodies of procedures :ada:`Note_Passing` and :ada:`Dissemble` are not
portable. The :ada:`Put_Line` calls are useful for illustration and will likely
work as expected on a native OS. However, their execution is a bounded error
and may do something else on other targets, including raising
:ada:`Program_Error` if detected.

For a portable approach, we move these two blocking calls to a new dedicated
task and revise the protected object accordingly. That's a portable approach
because a task can make blocking calls.

First, we change :ada:`Obituary.Writer` to have a single protected procedure
and a new entry. The protected procedure will be used as a termination handler,
as before, but does not print the messages. Instead, when invoked by task
finalization, the handler enters the parameter values into an internal data
structure and then enables the entry barrier on the protected entry. The
dedicated task waits on that entry barrier and, when enabled, retrieves the
stored values describing a termination. The task can then call :ada:`Put_Line`
to print the announcement with those values.

Here's the updated :ada:`Obituary` package declaration:

.. code:: ada no_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary_Updated
    :class: ada-syntax-only

    with Ada.Exceptions;           use Ada.Exceptions;
    with Ada.Task_Termination;     use Ada.Task_Termination;
    with Ada.Task_Identification;  use Ada.Task_Identification;
    with Ada.Containers.Vectors;

    package Obituary is

       pragma Elaborate_Body;

       Comment_On_Normal_Passing : Boolean := True;
       --  Do we say anything if the task completed normally?

       type Termination_Event is record
          Cause    : Cause_Of_Termination;
          Departed : Task_Id;
          Event    : Exception_Id;
       end record;

       package Termination_Events is new Ada.Containers.Vectors
         (Positive, Termination_Event);

       protected Writer is

          procedure Note_Passing
             (Cause    : Cause_Of_Termination;
              Departed : Task_Id;
              Event    : Exception_Occurrence);

          entry Get_Event (Next : out Termination_Event);

       private
          Stored_Events : Termination_Events.Vector;
       end Writer;

    end Obituary;

As a minor refinement we add the option to not print announcements for normal
completions, for those applications that allow task completion.

We must declare the generic container instantiation outside the protected
object, an unfortunate limitation of protected objects. We would prefer that
clients have no compile-time visibility to it, since it is an implementation
artifact.

The updated package body is straightforward:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary_Updated

    package body Obituary is

       protected body Writer is

          ------------------
          -- Note_Passing --
          ------------------

          procedure Note_Passing
             (Cause    : Cause_Of_Termination;
              Departed : Task_Id;
              Event    : Exception_Occurrence)
          is
          begin
             if Cause = Normal and then
                not Comment_On_Normal_Passing
             then
                return;
             else -- store all three causes and their info
                Stored_Events.Append
                  (Termination_Event'(Cause,
                                      Departed,
                                      Exception_Identity (Event)));
             end if;
          end Note_Passing;

          ---------------
          -- Get_Event --
          ---------------

          entry Get_Event (Next : out Termination_Event)
          when
             not Stored_Events.Is_Empty
          is
          begin
             Next := Stored_Events.First_Element;
             Stored_Events.Delete_First;
          end Get_Event;

       end Writer;

    begin -- optional package executable part
       Set_Dependents_Fallback_Handler (Writer.Note_Passing'Access);
    end Obituary;

A new child package declares the task that prints the termination information:

.. code:: ada no_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary_Updated
    :class: ada-syntax-only

    package Obituary.Output is
       pragma Elaborate_Body;

       task Printer;

    end Obituary.Output;

In the package body, the task body iteratively suspends on the call to
:ada:`Writer.Get_Event`, waiting for a termination handler to make the
termination data available. Once it returns from the call, if ever, it simply
prints the information and awaits further events:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary_Updated

    with Ada.Text_IO;  use Ada.Text_IO;

    package body Obituary.Output is

       -------------
       -- Printer --
       -------------

       task body Printer is
          Next : Termination_Event;
       begin
          loop
             Writer. Get_Event (Next);
             case Next.Cause is
                when Normal =>
                    Put_Line (Image (Next.Departed) & " died, naturally.");
                    --  What a difference that comma makes!
                    Put_Line ("We had nothing to do with it.");
                when Abnormal =>
                   Put_Line (Image (Next.Departed) &
                             " had a terrible accident.");
                   Put_Line ("We're sorry it had to come to that.");
                when Unhandled_Exception =>
                   Put_Line (Image (Next.Departed) &
                             " reacted badly to " &
                             Exception_Name (Next.Event));
                   Put_Line ("Really, really badly.");

             end case;
          end loop;
       end Printer;

    end Obituary.Output;

We declared this task in a child package because one can view the
:ada:`Printer` and the :ada:`Writer` as parts of a single subsystem, but that
structure isn't necessary. An unrelated application task could just as easily
retrieve the information stored by the protected :ada:`Writer` object.

Here is a sample demonstration main procedure, a simple test to ensure that
termination due to task abort is captured and displayed:

.. code:: ada run_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Obituary_Updated
    :class: ada-norun

    with Obituary.Output; pragma Unreferenced (Obituary.Output);
    --  otherwise neither package is in the executable

    procedure Demo_Fallback_Handler_Abort is

       task Worker;
       task body Worker is
       begin
          loop  --  ensure not already terminated when aborted
             delay 0.0;  --  yield the processor
          end loop;
       end Worker;

    begin
       abort Worker;
    end Demo_Fallback_Handler_Abort;

Note that the nested task would not be accepted under the Ravenscar or Jorvik
profiles because those profiles require tasks to be declared at the library
level, but that can easily be addressed.

When this demo main is run, the output looks like this:

.. code-block:: none

    worker_00000174BC68A570 had a terrible accident.
    We're sorry it had to come to that.

The actual string representing the task identifier will vary with the
implementation.

You'll have to use control-c (or whatever is required on your host) to end the
program because the :ada:`Printer` task in :ada:`Obituary.Output` runs forever.
Many applications run *forever* so that isn't necessarily a problem. That could
be addressed if need be.


Pros
----

The facility provided by package :ada:`Ada.Task_Termination` allows developers
to respond in any way required to task termination. The three causes, normal
completion, unhandled exceptions, and task abort are all supported.
Significantly, no source code in application tasks is required for the
termination support to be applied, other than the isolated calls to set the
handlers.


Cons
----

On a bare metal target there may be restrictions that limit the usefulness of
the facility. For example, on targets that apply the Ravenscar or Jorvik
profiles, task abort is not included in the profile and tasks are never
supposed to terminate for any reason, including normally. Independent of the
profiles, some run-time libraries may not support exception propagation, or
even any exception semantics at all.


Relationship With Other Idioms
------------------------------

None.


.. _Ada_In_Practice_Silent_Task_Termination_Notes:

Notes
-----

If you did want to use a generic package to define a task type that is
resilient to unhandled exceptions, you could do it like this:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Resilient_Workers

    with System;
    with Ada.Exceptions; use Ada.Exceptions;

    generic
       type Task_Local_State is limited private;
       with procedure Initialize (This : out Task_Local_State);
       with procedure Update (This : in out Task_Local_State);
       with procedure Respond_To_Exception
              (Current_State : in out Task_Local_State;
               Error         : Exception_Occurrence);
    package Resilient_Workers is

       task type Worker
         (Urgency : System.Priority := System.Default_Priority)
       with
          Priority => Urgency;

    end Resilient_Workers;

    package body Resilient_Workers is

       task body Worker is
          State : Task_Local_State;
       begin
          Recovery : loop
             begin
                Initialize (State);
                Normal : loop
                   Update (State);
                   --  The call above is expected to return, ie
                   --  this loop is meant to iterate
                end loop Normal;
             exception
                when Error : others =>
                   Respond_To_Exception (State, Error);
             end;
          end loop Recovery;
       end Worker;

    end Resilient_Workers;

Although this code looks useful, in practice it has issues.

First, in procedure :ada:`Initialize`, the formal parameter mode may be a
problem. You might need to change the parameter mode from mode out to mode
in-out instead, because recovery from unhandled exceptions will result in
another call to :ada:`Initialize`. Mode out makes sense for the first time
:ada:`Initialize` is called, but does it make sense for all calls after that?
It depends on the application's procedures. The behavior of :ada:`Update` may
be such that local state should only partially be reset in subsequent calls to
:ada:`Initialize`.

Furthermore, if :ada:`Initialize` must only perform a partial initialization on
subsequent calls, the procedure must keep track of the number of calls. That
requires a variable declared external to the body of :ada:`Initialize`. The
additional complexity is unfortunate. We could perhaps mitigate this problem by
having two initialization routines passed to the instantiation: one for full
initialization, called only once with mode out for the state, and one for
partial initialization, called on each iteration of the :ada:`Recovery` loop
with mode in-out for the state:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Silent_Task_Termination.Resilient_Workers

    package body Resilient_Workers is

       task body Worker is
          State : Task_Local_State;
       begin
          Fully_Initialize (State);

          Recovery : loop
             begin
                Normal : loop
                   Update (State);
                   --  The call above is expected to return, i.e.
                   --  this loop is meant to iterate
                end loop Normal;
             exception
                when Error : others =>
                   Respond_To_Exception (State, Error);
             end;

             Partially_Initialize (State);
          end loop Recovery;
       end Worker;

    end Resilient_Workers;

If both application initialization routines happen to do the same thing, we'd
like the developer to be able to pass the same application procedure to both
generic formal procedures :ada:`Fully_Initialize` and
:ada:`Partially_Initialize` in the instantiation. But that wouldn't compile
because the parameter modes don't match.

Then there's the question of the nature of the task. Is it periodic, or
sporadic, or free running? If it is periodic, we need a delay statement in the
:ada:`Normal` loop to suspend the task for the required period. The generic's
task body doesn't do that. The actual procedure passed to :ada:`Update` could
do the delay, but now, like a single version of :ada:`Initialize` required to
do both partial and full initialization, it needs additional state declared
external to the procedure body (for the :ada:`Time` variable used by the
absolute delay statement).

Finally, the single generic formal type used to represent the task's local
state can be awkward. Having one type for a task's total state is unusual, and
aggregating otherwise unrelated types into one isn't good software engineering
and doesn't reflect the application domain. Furthermore, that awkwardness
extends to the procedures that use that single object, in that every procedure
except for :ada:`Initialize` will likely ignore parts of it.

In summary, the problems are likely more problematic than this generic is
worth.


.. rubric:: Footnotes

.. [#f1] The Verdix Ada Development System did so.
