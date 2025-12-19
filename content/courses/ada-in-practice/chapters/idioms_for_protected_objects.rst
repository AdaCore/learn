.. _Ada_In_Practice_Idioms_For_Protected_Objects:

Idioms for Protected Objects
============================

.. include:: ../../../global.txt

.. note::

    Parts of this chapter were originally published as the
    `Blog Post: On the Benefits of Families ... (Entry Families) <https://www.adacore.com/blog/on-the-benefits-of-families>`_.


First, a bit of background regarding synchronization in concurrent programming,
protected objects, and the requeue statement. If you already know this
material, feel free to skip ahead to the
:ref:`Motivation <Ada_In_Practice_Idioms_For_Protected_Objects_Motivation>`
section for this chapter.

Concurrent programming is more complex (and more fun) than purely sequential
programming. The cause of this complexity is two-fold: 1) the executing
threads' statements are likely interleaved at the assembly language level, and
2) the order of that interleaving is unpredictable. As a result, developers
cannot know, in general, where in its sequence of statements any given thread is
executing. Developers can only assume that the threads are making finite
progress when they execute.

A consequence of unpredictable interleaving is that the bugs specific to this
type of programming are timing-dependent. Such bugs are said to be
*Heisenbugs* because they "go away when you look at them," i.e., changing the
code |mdash| adding debugging statements or inserting debugger breakpoints
|mdash| changes the timing. The bug might then disappear entirely, or simply
appear elsewhere in time. We developers must reason about the possible effects
of interleaving and design our code to prevent the resulting bugs. (That's why
this is fun.) Such bugs are really design errors.

One of these errors is known as a *race condition*. A race condition is
possible when multiple threads access some shared resource that requires
mutually exclusive access. If we accidentally forget the finite progress
assumption we may incorrectly assume that the threads sharing that resource
will access it serially. Unpredictable execution interleaving cannot support
that assumption.

Race conditions on memory locations are the most common, but the issue is
general in nature, including for example hardware devices and OS files. Hence
the term "resource."

For example, suppose multiple threads concurrently access an output display
device. This device can be ordered to move its cursor to arbitrary points on
the display by writing a specific sequence of bytes to it, including the
numeric values for X and Y coordinates. A common use is to send the "move
cursor to X, Y" sequence and then send the text intended to appear at coordinates
X and Y.

Clearly, this device requires each client thread to have mutually exclusive
access to the device while sending those two byte sequences. Otherwise,
uncoordinated interleaving could result in one thread preempting another
thread in the middle of sending those two sequences. The result would be an
intermingled sequence of bytes sent to the device. (On a graphics display the
chaotic output can be entertaining to observe.)

Memory races on variables are less obvious. Imagine two threads, Thread1 and
Thread2, that both increment a variable visible to both (an integer, let's
say).

Suppose that the shared integer variable has a value of zero. Both threads
increment the variable, so after they do so the new value should be two. The
compiler will use a hardware register to hold and manipulate the variable's
value because of the increased performance over memory accesses. Each thread
has an independent copy of the registers, and will perform the same assembly
instructions:

#. load a register's value from the memory location containing the variable's value
#. increment the register's value
#. store the register's new value back to the variable's memory location.

The two threads' executions may be interleaved in these three steps. It is
therefore possible that Thread1 will execute step 1 and step 2, and then be
preempted by the execution of Thread2. Thread2 also executes those two steps.
As a result, both threads' registers have the new value of 1. Finally, Thread1
and Thread2 perform the third step, both storing a value of 1 to the
variable's memory location. The resulting value of the shared variable will be
1, rather than 2.

Another common design bug is assuming that some required program state has been
achieved. For example, for a thread to retrieve some data from a shared
buffer, the buffer must not be empty. Some other thread must already have
inserted data. Likewise, for a thread to insert some data, the buffer must not
be full. Again, the finite progress assumption means that we cannot know
whether either of those two states are achieved.

Therefore, interacting threads require two forms of synchronization: *mutual
exclusion* and *condition synchronization*. These two kinds of synchronization
enable developers to reason rigorously about the execution of their code in the
context of the finite progress assumption.

Mutual exclusion synchronization prevents threads that access some shared
resource from doing so at the same time, i.e., it provides mutually exclusive
access to that resource. The effect is achieved by ensuring that, while any
given thread is accessing the resource, that execution will not be interleaved
with the execution of any other thread's access to that shared resource.

Condition synchronization suspends a thread until the condition |mdash| an
arbitrary Boolean expression |mdash| is :ada:`True`. Only when the expression
is (or becomes) :ada:`True` can the caller thread be allowed to continue.

A thread-safe bounded buffer is a good example for these two kinds of
synchronization. Some threads, the producers, will insert items into the
buffer. Other threads, the consumers, will concurrently remove items. The array
object representing the buffer contents, as well as the indexes into the array,
require mutually exclusive access for both producers and consumers.
Furthermore, producers must be blocked (suspended) as long as the given buffer
is full, and consumers must be blocked as long as the given buffer is empty.

Concurrent programming languages support mechanisms providing the two forms
of synchronization. In some languages these are explicit constructs; other
languages take different approaches. In any case, developers can apply these
mechanisms to enforce assumptions more specific than simple finite progress.

Ada uses the term :ada:`task` rather than thread so we will use that term from
here on.

The protected procedures and protected entries declared by a protected object
(PO) automatically execute with mutually exclusive access to the entire
protected object. No other caller task can be executing these operations at the
same time, so execution of the procedure or entry body statements will not be
interleaved. (Functions are special because they have read-only access to the
data in the PO.) Therefore, there can be no race conditions on the data
encapsulated within it. Even if the protected object has no encapsulated data, these operations
always execute with mutually exclusive access. During such execution we can say
that the PO is locked, speaking informally, because all other caller tasks are
held pending.

Protected entries are much like protected procedures, except that entry bodies
include a barrier condition that is used to express condition synchronization.
The condition is an arbitrary Boolean expression, although there are some
restrictions on the content for implementation reasons. Only when the barrier
condition is :ada:`True` will a caller to the entry be allowed to execute the
entry body. Once the body completes, the caller exits and can continue
execution outside of the PO. (We'll say more about that later.) For example,
entry barriers can express whether a bounded buffer is full or empty, thereby
enabling and disabling buffer insertion and removal.

Under some circumstances, an entry may execute a requeue statement to reroute
the caller to some other entry, for reasons that will be explained shortly, but
from the caller task's point of view there is only one call being made.

The requeue statement may not be familiar to many readers. To explain
its semantics we first need to provide its rationale.

Ada synchronization constructs are based on *avoidance synchronization*, meaning
that:

#. the user-written controls that enable/disable the execution of protected
   entry bodies and task entry accept statements enable them only when they can
   actually provide the requested service, and

#. that determination is based on information known prior to the execution of
   the entry body or accept statement.

For example, at runtime, if a bounded buffer is full, that fact can be
determined from the buffer's state: is the count of contained items equal to
the capacity of the backing array? If so, the user-defined controls disable the
operation to insert another value. Likewise, if the buffer is empty, the
removal operation is disabled. When we write the buffer implementation we know
beforehand what the operations will try to do, so we can write the controls to
disallow them at runtime until they can succeed. Most of the time that's
sufficient, but not always. If we can't know precisely what the operations will
do when we write the code, avoidance synchronization won't suffice.

The :ada:`requeue` statement is employed when avoidance synchronization is
not sufficient. A task calling an entry that executes a requeue statement is much
like a person calling a large company on the telephone. Calling the main number
connects you to a receptionist (if you're lucky and don't get an annoying
menu). If the receptionist can answer your question, they do so and then you
both hang up. Otherwise, the receptionist forwards the call to the person they
determine that you need to speak with. After doing so, the receptionist hangs
up, because from their point of view the call is complete. The call is not
complete from your point of view, though, until you finish your conversation
with the new person. And of course you may have to wait to speak to that
person.

Like the receptionist, the first entry called must take (execute) the call
without knowing what the request will be, because the entry barrier cannot
reference the entry parameters. The parameter values are only known once the
entry body executes. Therefore, the first entry may or may not be able to
provide the requested service and allow the caller to return from the call. If
not, it requeues the call and finishes, leaving the call still pending on the
requeue target, i.e., the second entry.

A requeue statement is not required in all cases but, as you will see,
sometimes it is essential. Note that protected procedures cannot execute
requeue statements, only protected entries can do so. Protected procedures
are appropriate when only mutual exclusion is required (to update encapsulated data).

.. _Ada_In_Practice_Idioms_For_Protected_Objects_Motivation:

Motivation
----------

Of the several highly significant features added to the Ada language over the
years, protected objects are one of the most important.

One of the reasons for this prominence is that protected objects make
efficient asynchronous task interactions possible. Many, if not most, task
interactions are asynchronous, but early Ada had only a synchronous mechanism
for communication and synchronization, known informally as the rendezvous. The
rendezvous is a high-level, very robust mechanism providing communication and
synchronization for two tasks at a time. This mechanism isn't a problem in
itself. If the application requires what amounts to an atomic action with two
task participants, then the rendezvous meets this requirement nicely.

But, as a synchronous mechanism, the rendezvous is far too expensive when only
an asynchronous mechanism (involving only one task) is required. Older mechanisms used for asynchronous
interactions, such as semaphores, mutexes, and condition variables, are nowhere
near as simple and robust for clients, but are much faster.

In addition, the rendezvous is only available between tasks, meaning that
abstractions requiring mutual exclusion and condition synchronization had to be
implemented as tasks too. Inserting and removing from a thread-safe buffer, for
example, involved expensive task switching between the buffer task, the
producer task, and the consumer task. This was the primary source of
comparative inefficiency.

There was a non-standard notion of a *passive task* that wasn't actually a
thread of control, and therefore did not require task switching, but it was not
widely adopted. In that same vein, Ada 80 had a built-in :ada:`Semaphore` task
type, intended to be implemented efficiently and used as the name suggests, but
mixing the higher-level rendezvous with the much lower-level semaphore
abstraction was considered poor language design. It did not survive to the ANSI
and first ISO standards. Ultimately, the designers of the first version of Ada
thought that processors would become so much faster in the future that the
relative inefficiency and semantic mismatch wouldn't matter. Processors did get
faster, but the problems still mattered.

Another reason that protected objects are so important is that they are
applicable to a wide range of programming domains. Protected objects are
critical to concurrent programming, real-time programming, and embedded systems
programming with Ada. We've already highlighted their high level, robust
support for asynchronous interactions in concurrent programming. For real-time
programming, systems of any significant complexity will map deadlines to tasks.
Consequently, in such systems the programming model is concurrent programming
with the addition of predictability. In these systems protected objects have
additional semantics (e.g., priorities) but supply the same benefits as in
concurrent programming. For embedded systems programming, protected objects are
used to express interrupt handlers, again with added semantics.

Their most important contribution, however, goes beyond direct client use of
their automatic mutual exclusion and condition synchronization semantics.
Developers can use protected objects to create just about any synchronization
and communication protocol imaginable. These include application independent
abstractions such as atomic actions, readers-writers locks, mutexes, and so on,
but also schemes based on application-specific protocols and data structures.
When combined with other language features, such as requeue, task identifiers,
and the overall composition capabilities of the language, the result is a
flexible, powerfully expressive facility.


Implementation
--------------

Protected objects are primarily utilized in two ways. We will refer to these
two ways as idioms, for the sake of consistency with the rest of this course,
although the other idioms in this course are much more narrow in scope. In the
first, protected objects encapsulate and manipulate application-specific data.
In the second, protected objects are used to create developer-defined
synchronization and communication abstractions.


First Idiom Category: Application-Specific Protected Objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the first idiom, a given protected object implements all the
application-specific functionality for the shared resources it encapsulates.
We declare the shared data in the PO private part and declare the protected
entries and procedures that manipulate that data in the visible part, as well
as functions for reading that data if needed. The compiler won't allow any
direct references to the hidden data from outside of the PO; the visible
operations must be called by client tasks to manipulate the data. Readers
familiar with the classic *monitor* construct will recognize it as the
conceptual foundation for protected objects used this way.

For example, let's say we want to protect a product serial number variable
from concurrent manipulation by multiple caller tasks. These tasks need to get
the next sequential serial number, which entails incrementing the current value
each time a task requests the next number. We must prevent the increment from
occurring concurrently, otherwise the resulting race condition could
occasionally provide incorrect values to the callers. Therefore, the increment
will be done inside a protected procedure that provides the current value via
parameter and also increments the value before returning. We declare the
protected object like so:

.. code-block:: ada

    protected Serial_Number is
       procedure Get_Next (Number : out Positive);
    private
       Value : Positive := 1;
    end Serial_Number;


    protected body Serial_Number is

       procedure Get_Next (Number : out Positive) is
       begin
          Number := Value;
          Value  := Value + 1;
       end Get_Next;

    end Serial_Number;

Whenever any task calls :ada:`Serial_Number.Get_Next`, the task will block
until it has mutually exclusive access to the PO, and consequently to the
:ada:`Serial_Number.Value` component. At that point, :ada:`Value` is assigned
to the formal parameter and then incremented. Once the procedure returns, the
caller task can continue execution with their unique serial number copy. No
race conditions are possible and the shared serial number value increments
safely each time :ada:`Get_Next` is called.

Note the robust nature of a protected object's procedural interface: clients
simply call the protected procedures, entries, or functions. The called
procedure or entry body, when it executes, will always do so with mutually
exclusive access. (Functions can have some additional semantics that we can
ignore here.) There is no explicit lower level synchronization mechanism for
the client to acquire and release. The semantics of protected objects are
implemented by the underlying Ada run-time library, hence all error cases are
also covered. This procedural interface, with automatic implementation for
mutual exclusion, is one of the significant benefits of the monitor construct
on which protected objects are based.


Second Idiom Category: Developer-Defined Concurrency Abstractions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the second idiom, data may be declared in the protected object private part,
but they are not application data. Likewise, the protected operations do
nothing application specific.

Instead, the PO provides some synchronization (and perhaps communication)
protocol that we want to make available to client tasks. These tasks call the
protected object's operations in order to get the protocol's semantics applied
to their execution. The data declared in the private part, if any, exist purely
for the sake of implementing the intended protocol.

In particular, the protected operations block and release the caller tasks per
the new abstraction's semantics. We are using the term *block* loosely here,
meaning the caller task is not allowed to return from the call until some
condition holds.

These abstractions are frequently declared as protected types rather than
anonymously typed protected objects like the :ada:`Serial_Number` PO.
Protected types are especially preferred when the protocol is application
independent and hence reusable. Declaration as a type also provides all the
flexibility of types, including the ability to declare as many objects of the
type as required and the ability to compose other types using them. Types also
allow parameterization via discriminants, if necessary.

The synchronization abstractions may be classic mechanisms long known to the
concurrent programming community, for example semaphores, or they may be wholly
novel, perhaps based on application-specific contexts and data structures. Very
sophisticated abstractions can be expressed, such as atomic actions involving
an arbitrary number of tasks. The possibilities are endless.

For example, we could have a protected type that implements the Readers-Writers
synchronization protocol. In this protocol only one task at a time can write
(update) the state of the shared objects, and writers must wait until there are
no readers, but multiple simultaneous readers are allowed as long as there is
no writer active. Such a protected object would have multiple protected
operations, some to block callers until appropriate for the given read or write
action requested, and some to signal the end of the read or write operation so
that a pending request (if any) can be granted.

This second idiomatic application of protected objects is extremely useful and
therefore common. However, there is also a situation in which we are forced to
use it, for the sake of portability. That happens when statements that would
otherwise be within a protected operation include a potentially blocking
operation. This is a term defined by the language for those constructs that may
cause a currently executing caller task to yield the processor to another task.
As such, they are not allowed within protected operations, neither directly nor
indirectly. To understand why, you need to understand the underlying system
approaches available for implementing the mutually exclusive access that
protected operations provide automatically.


.. _Ada_In_Practice_Idioms_For_Protected_Objects_System_Implementation_PO_Mutual_Exclusion:

System Implementation of PO Mutual Exclusion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The underlying run-time library implements the mutual exclusion and
thread management semantics for protected objects. Two approaches are known.

One implementation approach, typical when executing on an operating system,
uses an explicit locking mechanism provided by the OS. The run-time library
code implementing the protected operations first acquires a dedicated OS lock
and then later releases it when exiting.

But another approach is available that does not use explicit locks. Instead,
mutual exclusion is implemented via priorities, both task priorities and PO
priorities. Note that this implementation requires priorities to be defined,
execution on a uniprocessor, and the :ada:`Ceiling_Locking` policy to have been
specified via the :ada:`Locking_Policy` pragma.

Specifically, developers assign a priority to each protected object. Each PO
priority must be the highest (the *ceiling*) of all the priorities of those
tasks that actually call the operations provided by the PO. Consequently, for
any given PO, no task that calls that PO will have a higher priority than the
PO priority. Because caller tasks inherit the PO priority (immediately), their
calls execute with the highest priority of any caller task for that specific
PO. Therefore, no other caller task can preempt any current caller executing
within the PO. The current caller may be preempted, but not by a task that
would also call that same PO. Thus, mutually exclusive access is provided
automatically, and very efficiently. This approach has other benefits as well
that are not pertinent here.

However, the priority-based implementation cannot work reliably if blocking is
allowed within a protected operation. If the current caller could yield the
processor inside a protected operation, some other task could then be allowed
to continue execution, including possibly a task making a call to that same PO.
In that case mutual exclusion would not be provided for that PO.

As a result, the language defines a number of potentially blocking operations
and disallows them within protected operations. Any I/O operation is
potentially blocking, for example, as are delay statements, but there are
others as well. See the Ada RM, section 9.5{34} for the full list.

For example, in the
:ref:`Dealing with Silent Task Termination idiom <Ada_In_Practice_Silent_Task_Termination>`
idiom we had an initial implementation of a protected procedure body that
called :ada:`Ada.Text_IO.Put_Line`:

.. code-block:: ada

    procedure Dissemble
      (Cause    : in Cause_Of_Termination;
       Departed : in Task_Id;
       Event    : in Exception_Occurrence)
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

As described in that idiom entry, the above might work, but it is not
portable.

As a consequence, we may find ourselves with some statement (e.g., the call to
:ada:`Put_Line`) that would have been within a protected operation for the
sake of mutually exclusive access, but that cannot be included there if the
code is to be portable. The statement must be written outside of a PO, not
within a protected object's operations, and not in anything called by those
protected operations.


Examples for Second Idiom Category
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We will use the approach to potentially blocking operations as the first
example.

Suppose we are implementing a message logging facility. Any given task
executing in the application can write a log message by calling a procedure
named :ada:`Enter`, defined in package :ada:`Log`. The actual messages are
values of type :ada:`String`:

.. code-block:: ada

    package Log is
       procedure Enter (Log_Entry : String);
    end Log;

Messages are written to an external file so that they will persist. That file
is declared in the package body. Therefore, the package design is an
:ref:`Abstract Data Machine <Ada_In_Practice_Abstract_Data_Machines>`:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    package body Log is

       Log_File : File_Type;

       procedure Enter (Log_Entry : String) is
       begin
          Put_Line (Log_File, Msg);
          Flush (Log_File);
       end Enter;

    begin
       Create (Log_File, Out_File, "log.txt");
    end Log;

Unfortunately, this won't work reliably in a concurrent program. Multiple
tasks may call procedure :ada:`Enter` simultaneously, indirectly making the
:ada:`Log_File` object a shared resource. Race conditions are therefore
possible when updating the :ada:`Log_File` object via :ada:`Put_Line`. We could
employ a protected object to prevent the race condition, but as we saw with
protected procedure :ada:`Dissemble` above, placing the call to :ada:`Put_Line`
within a protected operation is not portable. We need some other way to ensure
mutually exclusive access to the shared file object.

In some programming languages, a *mutex* is used to provide mutually exclusive
access (hence the name) to some set of objects that are shared among multiple
competing threads. All these threads must follow the same usage pattern:

#. before accessing the shared resource, a thread calls a routine on the mutex
   in order to block until it is appropriate to continue,

#. upon return from the call that thread executes an arbitrary sequence of
   statements accessing the resource,

#. after that sequence, the thread calls another operation on the same mutex to
   signal that some other thread can now be allowed to return from their call
   in step one.

A mutex must be implemented so that, for any given mutex object, only one
caller at a time is allowed to return from the call in step one. Therefore,
step one is said to acquire or seize the mutex object, and step three releases
it. The result is that only one thread at a time will execute the statements in
step two, hence with mutually exclusive access to the manipulated resources.

We can create a protected type providing a basic mutex abstraction. The
protected operations will consist of two routines: one to acquire the mutex
(step one) and one to release it (step three). Calls to these two PO operations
can then bracket an application-specific sequence of statements that manipulate
objects requiring mutually exclusive access (step two). But now this bracketed
code can include some potentially blocking operations.

.. code-block:: ada

    protected type Mutex is
       entry Acquire;
       procedure Release;
    private
       Available : Boolean := True;
    end Mutex;


    protected body Mutex is

       entry Acquire when Available is
       begin
          Available := False;
       end Acquire;

       procedure Release is
       begin
          Available := True;
       end Release;

    end Mutex;

Tasks that want to exclusively acquire an object of the :ada:`Mutex` type will
call entry :ada:`Acquire` on that PO. Similarly, tasks call protected procedure
:ada:`Release` to signal that the logical hold on the PO is no longer required.
The component :ada:`Available` is declared within the protected private part,
and exists only to implement the mutex semantics.

The gist of the implementation is that acquiring the :ada:`Mutex` object
amounts to allowing a caller task to exit their call to the entry
:ada:`Acquire`, with any other caller tasks held pending. The entry barrier
condition expresses the logic of whether the caller is allowed to continue, via
the internal Boolean component :ada:`Available`. There is no actual lock in
view here, just the effect of a lock. That effect is achieved via condition
synchronization that ensures only one task at a time can return from the
:ada:`Acquire` call. All other callers to :ada:`Acquire` are held, suspended,
in the entry's queue. When :ada:`Release` is called that :ada:`Mutex` protected
object becomes available for locking again.

The following code fragment illustrates using the :ada:`Mutex` type for the
sake of controlling access to a shared variable, in this case the file object
in the message logging package. Here is the pertinent part of the logging
facility's package body:

.. code-block:: ada

    Log_Lock : Mutex;
    Log_File : File_Type;

    procedure Enter (Log_Entry : String) is
    begin
       Log_Lock.Acquire;
       Put_Line (Log_File, Log_Entry);
       Flush (Log_File);
       Log_Lock.Release;
    end Enter;

The body of procedure :ada:`Enter` first calls :ada:`Log_Lock.Acquire`. The
call is not allowed to return until the caller task exclusively holds the
logical lock associated with the :ada:`Log_Lock` object. Therefore, every
subsequent statement executes with mutual exclusion relative to the
:ada:`Log_Lock` object. In this case, there are two such statements, the one
that writes the string to the single shared output file and one that flushes
any internal buffers associated with the file. They are both potentially
blocking operations, but we're not in a protected operation so that's not a
problem. Finally, procedure :ada:`Enter` calls :ada:`Log_Lock.Release` to
relinquish the current caller task's hold on the :ada:`Log_Lock` mutex. If some
other task was waiting to hold the :ada:`Log_Lock` object, that task can now
return from its call to :ada:`Acquire` and can execute its update to the log
file.

There are issues unaddressed in the three-step client protocol illustrated by
the code above, especially error cases. For example, even if an exception is
raised in step two, we need to ensure that :ada:`Release` is called with
exactly-once semantics. There are other abstractions that address these client
usage issues, namely scope-based locking, but we'll ignore them here. See the
:ref:`Resource Acquisition Is Initialization <Ada_In_Practice_RAII>` idiom for
the :ada:`Scope_Lock` type.

The implementation of type :ada:`Mutex` above doesn't have quite the full
canonical semantics. So far it is really just that of a binary semaphore. In
particular, a mutex should only be released by the same task that previously
acquired it, i.e., the current owner. We can implement that consistency check in a fuller
illustration of this example, one that raises an exception if the caller to
:ada:`Release` is not the current owner of the :ada:`Mutex` object.

The new version of type :ada:`Mutex` is declared as follows. The difference is
the additional component of type :ada:`Ada.Task_Identification.Task_Id` named
:ada:`Current_Owner`. (Assume a use-clause for that package.)

.. code-block:: ada

    protected type Mutex is
       entry Acquire;
       procedure Release;
    private
       Available     : Boolean := True;
       Current_Owner : Task_Id := Null_Task_Id;
    end Mutex;

The updated protected body is as follows:

.. code-block:: ada

    protected body Mutex is

       entry Acquire when Available is
       begin
          Available := False;
          Current_Owner := Acquire'Caller;
       end Acquire;

       procedure Release is
       begin
          if Current_Owner = Current_Task then
             Available := True;
             Current_Owner := Null_Task_Id;
          else
             raise Protocol_Error;
          end if;
       end Release;

    end Mutex;

Note how entry :ada:`Acquire`, when granting the logical lock and releasing the
caller, also captures the identity of that caller. Procedure :ada:`Release` can
verify that identity when it is eventually called, using function
:ada:`Current_Task` declared in package :ada:`Ada.Task_Identification`.

We can build on that version of the type :ada:`Mutex` to make a variation named
:ada:`Reentrant_Mutex`. This type allows a given task to re-acquire a
:ada:`Reentrant_Mutex` object if that same task is the current owner, i.e., has
returned from a previous call to :ada:`Acquire` and has not yet called
:ada:`Release` a matching number of times:

.. code-block:: ada

    protected type Reentrant_Mutex is
       entry Acquire;
       procedure Release;
    private

       entry Retry;
       --  Internal target of requeue when the mutex is already owned.

       Depth : Natural := 0;
       --  Number of calls to Seize for a given holder. A value of zero
       --  corresponds no task currently holding the mutex.

       Current_Owner : Task_Id := Null_Task_Id;
       --  The current holder of the mutex, initially none.

    end Reentrant_Mutex;

We still have the :ada:`Current_Owner` component, but we've added a new
component to keep track of the *depth* of the current owner's calls. The depth
test replaces the simple Boolean test of being available, so the
:ada:`Available` component is gone. Instead, when the depth is zero the
corresponding protected object is available, but it is also available if the
current caller of :ada:`Acquire` is the current owner from a previous call.

.. code-block:: ada

    protected body Reentrant_Mutex is

       entry Acquire when True is
       begin
          if Current_Owner = Null_Task_Id then
             Current_Owner := Acquire'Caller;
             Depth := 1;
          elsif Current_Owner = Acquire'Caller then
             Depth := Depth + 1;
          else -- held already, but not by current caller
             requeue Retry with abort;
          end if;
       end Acquire;

       procedure Release is
       begin
          if Current_Owner = Current_Task then
             Depth := Integer'Max (0, Depth - 1);
             if Depth = 0 then
                Current_Owner := Null_Task_Id;
             end if;
          else
             raise Protocol_Error;
          end if;
       end Release;

       entry Retry when Depth = 0 is
       begin
          Depth := 1;
          Current_Owner := Retry'Caller;
       end Retry;

    end Reentrant_Mutex;

The barrier for entry :ada:`Acquire` is always set to :ada:`True` because the
test for availability is not possible until the body begins to execute. If the
PO is not available, the caller task is requeued onto the :ada:`Retry` entry.
(A barrier set to :ada:`True` like this is a strong indicator of a requeue
operation.) The :ada:`Retry` entry will allow a caller to return |mdash| from the
caller's point of view, a return from the call to :ada:`Acquire` |mdash| only when no
other caller currently owns the PO.

The examples so far exist primarily for providing mutual exclusion to code that
includes potentially blocking operations. By no means, however, are these the
only examples. Much more sophisticated abstractions are possible.

For example, let's say we want to have a notion of *events* that application
tasks can await, suspending until the specified event is *signaled*. At some
point, other tasks will signal that these events are ready to be handled by the
waiting tasks. Understand that events don't have any state of their own, they
either have happened or not, and may happen more than once.

For the sake of discussion let's declare an enumeration type representing four
possible events:

.. code-block:: ada

    type Event is (A, B, C, D);

These event names are not very meaningful, but they are just placeholders for
those that applications would actually define. Perhaps a submersible's code
would have events named :ada:`Hatch_Open`, :ada:`Hatch_Closed`,
:ada:`Umbilical_Detached`, and so on.

Client tasks can suspend, waiting for an arbitrary event to be signaled, and
other tasks can signal the occurrence of events, using a *event manager* that
the two sets of tasks reference.

Here's the declaration of the manager type:

.. code-block:: ada

    type Manager is limited private;

Type :ada:`Manager` will be fully implemented in the package private part as a
protected type.

The type is limited because it doesn't make sense for clients to assign one
:ada:`Manager` object to another, nor to compare them via predefined equality.
There's another reason you'll see shortly. The type is private because that's
the default for good software engineering, and there's no reason to override
that default to make the implementation visible to clients. Our API will
provide everything clients require, when combined with the capabilities
provided by any limited type (e.g., declaring objects, and passing them as
parameters).

Tasks can wait for a single event to be signaled, or they can wait for one of
several. Similarly, tasks can signal one or more events at a time. Such groups
of events are easily represented by an unconstrained array type:

.. code-block:: ada

    type Event_List is array (Positive range <>) of Event;

We chose :ada:`Positive` as the index subtype because it allows a very large
number of components, far more than is likely ever required, and has an
intuitive default lower bound of 1. An aggregate value of the array type can
then represent multiple events, for example:

.. code-block:: ada

    Event_List'(A, C, D)

Given these three types we can define a useful API:

.. code-block:: ada

    procedure Wait
      (This         : in out Manager;
       Any_Of_These :        Event_List;
       Enabler      :    out Event);

    procedure Wait
      (This     : in out Manager;
       This_One : Event);

    procedure Signal
      (This         : in out Manager;
       All_Of_These : Event_List);

    procedure Signal
      (This     : in out Manager;
       This_One : Event);

Here's a task that waits for either event :ada:`A` or :ada:`B`, using a global
:ada:`Controller` variable of the :ada:`Manager` type:

.. code-block:: ada

    task body A_or_B_Processor is
       Active : Event;
    begin
       loop
          Wait (Controller,
                Any_Of_These => Event_List'(A, B),
                Enabler => Active);
          Put_Line ("A_or_B_Processor responding to event " &
                    Active'Image);
       end loop;
    end A_or_B_Processor;

When the call to :ada:`Wait` returns, at least one of either :ada:`A` or
:ada:`B` has been signaled. One of those signaled events was selected and
returned in the :ada:`Enabler` parameter. That selected event is no longer
signaled when the call returns and will stay that way until another call to
procedure :ada:`Signal` changes it. The other event in the list is not
affected, whether or not it was also signaled.

A signaling task could use the API to signal one event:

.. code-block:: ada

    Signal (Controller, This_One => B);

or just:

.. code-block:: ada

    Signal (Controller, B);

To signal multiple events:

.. code-block:: ada

    Signal (Controller, All_Of_These => Event_List'(A, C, D));

Now let's consider the :ada:`Manager` implementation. As this is a concurrent
program, we need it to be thread-safe. We've declared the :ada:`Manager` type
as limited, so either a task type or a protected type would be allowed as the
type's completion. (That's the other reason the type is limited.) There's no
need for this manager to do anything active, it just suspends some tasks and
resumes others when invoked. Therefore, a protected type will suffice, rather
than a task's active thread of control.

Clearly, tasks that await events must block until a requested event has been
signaled, assuming it was not already signaled when the call occurred, so a
protected procedure won't suffice. Protected procedures only provide mutual
exclusion, whereas protected entries can suspend a caller on a condition.
Therefore, we'll use a protected entry for the waiters to call. As you will see
later, there is another reason to use protected entries here.

Inside the :ada:`Manager` protected type we need a way to represent whether
events have been signaled. We can use an array of Boolean components for this
purpose, with the events as the indexes. For any given event index value, if
the corresponding array component is :ada:`True` that event has been signaled,
otherwise it has not.

.. code-block:: ada

    type Event_States is array (Event) of Boolean;

    Signaled : Event_States := (others => False);

Thus, for example, if :ada:`Signaled (B)` is :ada:`True`, a task that calls
:ada:`Wait` for :ada:`B` will be able to return. Otherwise, that task will be
blocked and cannot return from the call. Later another task will set
:ada:`Signaled (B)` to :ada:`True`, and then the waiting task can be unblocked.

Since an aggregate can also contain only one component if desired, we can use a
single set of protected routines for waiting and signaling in the
:ada:`Manager` protected type. We don't need one set of routines for waiting
and signaling a single event, and another set of routines for waiting and
signaling multiple events. Here then is the visible part:

.. code-block:: ada

    protected type Manager is

       entry Wait
         (Any_Of_These : Event_List;
          Enabler      : out Event);

       procedure Signal (All_Of_These : Event_List);

    private
       ...
    end Manager;

Both the entry and the procedure take an argument of the array type, indicating
one or more client events. The entry, called by waiting tasks, also has an
output argument, :ada:`Enabler`, indicating which specific event enabled the
task to resume, i.e., which event was found signaled and was selected to
unblock the task. We need that parameter because the task may have specified
that any one of several events would suffice, and more than one could have been
signaled.

The bodies of our API routines are then just calls into the protected
:ada:`Manager` that is passed as the first argument. For example, here are two
of the four:

.. code-block:: ada

    procedure Wait
      (This         : in out Manager;
       Any_Of_These :        Event_List;
       Enabler      :    out Event)
    is
    begin
       This.Wait (Any_Of_These, Enabler);
    end Wait;

    procedure Signal
      (This     : in out Manager;
       This_One : Event)
    is
    begin
       This.Signal (Event_List'(1 => This_One));
    end Signal;

Now let's examine the implementation of the protected type. The visible part is
repeated here:

.. code-block:: ada

    protected type Manager is

       entry Wait
         (Any_Of_These : Event_List;
          Enabler      : out Event);

       procedure Signal (All_Of_These : Event_List);

    private
       ...
    end Manager;

The entry :ada:`Wait` suspends callers until one of the requested events is
signaled, as specified by the argument :ada:`Any_Of_These`. Normally we'd
expect to use the entry barrier to express this behavior by querying the
events' state array. If one of the requested events is :ada:`True` the barrier
would allow the call to execute and complete. However, barriers do not have
compile-time visibility to the entry parameters, so the parameters cannot be
referenced in the barriers. This situation calls for a requeue statement.

Because :ada:`Wait` always takes a call, the entry barrier is just hard-coded
to :ada:`True`. (As mentioned earlier, that's always a strong indication that
requeue is involved.)
Even though this barrier always allows a call, much like a protected procedure,
we must use an entry because only protected entries can requeue callers.

Inside the entry body the specified events' states are checked, looking for one
that is :ada:`True`. If one is found, the entry body completes and the caller
returns to continue further, responding to the found event. If no requested
event is :ada:`True`, though, we cannot let the caller continue. We block it by
requeueing the caller on to another entry. Eventually that other entry will
allow the caller to return, when an awaited event finally becomes :ada:`True`
via :ada:`Signal`.

Here then is the full declaration for the protected type :ada:`Manager`,
including the array type declaration that cannot be internal to the protected
type:

.. code-block:: ada

    type Event_States is array (Event) of Boolean;

    protected type Manager is

       entry Wait
         (Any_Of_These : Event_List;
          Enabler      : out Event);

       procedure Signal (All_Of_These : Event_List);

    private

       Signaled          : Event_States := (others => False);
       Prior_Retry_Calls : Natural := 0;

       entry Retry
         (Any_Of_These : Event_List;
          Enabler      : out Event);

    end Manager;

The private part contains the event states component, a management component,
and the other entry, :ada:`Retry`, onto which we will requeue when necessary.
Note that this other entry is only meant to be called by a requeue from the
visible entry :ada:`Wait`, so we declare it in the private part to ensure there
are no other calls to it. That informs the reader, but also the maintainer, who
in the future might be tempted to call it in some other context.

The routine that checks for an existing signaled event is internal to the
protected type and is declared as follows:

.. code-block:: ada

    procedure Check_Signaled
      (These   : Event_List;
       Enabler : out Event;
       Found   : out Boolean);

The procedure examines the events specified in the formal parameter
:ada:`These` to see if any of them are currently signaled, i.e., have a value
of :ada:`True`. If it finds one, :ada:`Enabler` is set to that event value and
:ada:`Found` is set to :ada:`True`. Otherwise, Found is set to :ada:`False` and
:ada:`Enabler` is set to the value :ada:`Event'First`. The value assigned to
:ada:`Enabler` in that case is arbitrary, but the assignment itself is
important. Assigning a value prevents the actual parameter from becoming
undefined upon return. :ada:`Enabler` will only be evaluated when :ada:`Found`
returns :ada:`True` so the arbitrary value will be ignored.

Here's the body of the entry :ada:`Wait`, containing a call to
:ada:`Check_Signaled` and the requeue statement. Note that the formal parameter
:ada:`Wait.Enabler` is passed as the actual parameter to
:ada:`Check_Signaled.Enabler`.

.. code-block:: ada

    entry Wait
      (Any_Of_These : Event_List;
       Enabler      : out Event)
    when
       True
    is
       Found_Awaited_Event : Boolean;
    begin
       Check_Signaled (Any_Of_These, Enabler, Found_Awaited_Event);
       if not Found_Awaited_Event then
          requeue Retry;
       end if;
    end Wait;

The hard-coded entry barrier (:ada:`when True`) always allows a caller to
execute, subject to the mutual exclusion requirement. If :ada:`Check_Signaled`
doesn't find one of the specified events signaled, we requeue the caller to the
:ada:`Retry` entry. (The :ada:`Wait` entry parameters go to the :ada:`Retry`
entry, transparently.) On the other hand, if :ada:`Check_Signaled` did find a
specified event signaled, we just exit the entry, the formal parameter
:ada:`Enabler` having been set already by the call to the internal procedure.

Eventually, presumably, an awaited :ada:`False` event will become :ada:`True`.
That happens when :ada:`Signal` is called:

.. code-block:: ada

    procedure Signal (All_Of_These : Event_List) is
    begin
       for C of All_Of_These loop
          Signaled (C) := True;
       end loop;
       Prior_Retry_Calls := Retry'Count;
    end Signal;

After setting the specified events' states to :ada:`True`, :ada:`Signal`
captures the number of queued callers waiting on :ada:`Retry`. (The component
:ada:`Prior_Retry_Calls` is an internal component declared in the protected
type. The value is never presented to callers, but is, instead, used only to
manage callers.)

At long last, here's the body of :ada:`Retry`:

.. code-block:: ada

    entry Retry
      (Any_Of_These : Event_List;
       Enabler      : out Event)
    when
       Prior_Retry_Calls > 0
    is
       Found_Signaled_Event : Boolean;
    begin
       Prior_Retry_Calls := Prior_Retry_Calls - 1;
       Check_Signaled (Any_Of_These, Enabler, Found_Signaled_Event);
       if not Found_Signaled_Event then
          requeue Retry;
       end if;
    end Retry;

When a protected procedure or entry completes their sequence of statements, the
run-time system re-evaluates all the object's entry barriers, looking for an
open (:ada:`True`) barrier with a caller queued, waiting. If one is found, that
entry body is allowed to execute on behalf of that caller. On exit, the
evaluation / execution process repeats. This process is known as a protected
action and is one reason protected objects are so expressive and powerful. The
protected action continues iterating, executing enabled entry bodies on behalf
of queued callers, until either no barriers are open or no open barriers have
callers waiting. Note that one of these entries may enable the barrier
condition of some other entry in that same PO.

Therefore, when procedure :ada:`Signal` sets :ada:`Prior_Retry_Calls` to a
value greater than zero and then completes, the protected action allows
:ada:`Retry` to execute. Furthermore, :ada:`Retry` continues to execute,
attempting to service all the prior callers in the protected action, because
its barrier becomes :ada:`False` only when all those prior callers have been
serviced.

For each caller, :ada:`Retry` attempts the same thing :ada:`Wait` did: if a
requested event is :ada:`True` the caller is allowed to return from the call.
Otherwise, the caller is requeued onto :ada:`Retry`. So yes, :ada:`Retry`
requeues the caller onto itself! Doing so is not inherently a problem, but in
this particular case a caller would continue to be requeued indefinitely when
the requested event is :ada:`False`, unless something prevents that from
happening. That's the purpose of the count of prior callers. Only that number
of callers are executed by the body of :ada:`Retry` in the protected action.
After that the barrier is closed by :ada:`Prior_Retry_Calls` becoming zero, the
protected action ceases when the entry body exits, and any unsatisfied callers
remain queued.

All well and good, but have you noticed the underlying assumption? The code
assumes that unsatisfied callers are placed onto the entry queue at the end of
the queue, i.e., in FIFO order. Consequently, they are not included in the
value of the :ada:`Prior_Retry_Calls` count and so do not get executed again
until :ada:`Signal` is called again. But suppose we have requested elsewhere
that entry queues (among other things) are ordered by caller priority? We'd
want that for a real-time system. But then a requeued caller would not go to
the back of the entry queue and could, instead, execute all over again,
repeatedly, until the prior caller count closed the entry.

If priority queuing might be used, we must change the internal implementation
so that the queuing policy is irrelevant. We'll still have :ada:`Wait` do a
requeue when necessary, but no requeue will ever go to the same entry executing
the requeue statement. Therefore, the entry queuing order won't make a
difference. An entry family facilitates that change, and rather elegantly, too.

An entry family is much like an array of entries, each one identical to the
others. To work with any one of the entries we specify an index, as with an
array. For example, here's a requeue to :ada:`Retry` as a member of an entry
family, with :ada:`Active_Retry` as the index:

.. code-block:: ada

    requeue Retry (Active_Retry)

In the above, the caller uses the value of :ada:`Active_Retry` as an index to
select a specific entry in the family.

The resulting changes to the :ada:`Manager` type are as follows:

.. code-block:: ada

    type Retry_Entry_Id is mod 2; --  hence 0 .. 1
    type Retry_Barriers is array (Retry_Entry_Id) of Boolean;

    protected type Manager is
       ... as before
    private

       Signaled      : Event_States := (others => False);
       Retry_Enabled : Retry_Barriers := (others => False);
       Active_Retry  : Retry_Entry_Id := Retry_Entry_Id'First;

       entry Retry (Retry_Entry_Id)
         (Any_Of_These : Event_List;
          Enabler      : out Event);

    end Manager;

Our entry family index type is :ada:`Retry_Entry_Id`. We happen to need two
entries in this implementation, so a modular type with two values will suffice.
Modular arithmetic will also express the logic nicely, as you'll see. The
component :ada:`Active_Retry` is of this type, initialized to zero.

The entry :ada:`Retry` is now a family, as indicated by the entry declaration
syntax specifying the index type :ada:`Retry_Entry_Id` within parentheses. Each
entry has the same parameters as any others in the family, in this case the
same parameters as in the previous implementation.

We thus have two :ada:`Retry` entries so that, at any given time, one of the
entries can requeue onto the other one, instead of onto itself. An entry family
makes that simple to express.

At runtime, one of the :ada:`Retry` entries will field requeue calls from
:ada:`Wait` and will be the entry enabled by :ada:`Signal`. That entry is
designated the *active* retry target, via the index held in the component
:ada:`Active_Retry`.

Here's the updated body of :ada:`Wait`:

.. code-block:: ada

    entry Wait
      (Any_Of_These : Event_List;
       Enabler      : out Event)
    when
       True
    is
       Found_Signaled_Event : Boolean;
    begin
       Check_Signaled (Any_Of_These, Enabler, Found_Signaled_Event);
       if not Found_Signaled_Event then
          requeue Retry (Active_Retry) with abort;
       end if;
    end Wait;

The body is as before, except that the requeue target depends on the value of
:ada:`Active_Retry`. (We'll discuss :ada:`with abort` later.)

When :ada:`Signal` executes, it now enables the *active retry* entry barrier:

.. code-block:: ada

    procedure Signal (All_Of_These : Event_List) is
    begin
       for C of All_Of_These loop
          Signaled (C) := True;
       end loop;
       Retry_Enabled (Active_Retry) := True;
    end Signal;

The barrier component :ada:`Retry_Enabled` is now an array, using the same
index type as the entry family.

The really interesting part of the implementation is the body of :ada:`Retry`,
showing the expressive power of the language. The entry family member enabled
by :ada:`Signal` goes through all its pending callers, attempting to satisfy
them and requeuing those that it cannot. But instead of requeuing onto itself,
it requeues them onto the other entry in the family. As a result, the ordering
of the queues is immaterial. Again, the entry family makes this easy to
express:

.. code-block:: ada

    entry Retry (for K in Retry_Entry_Id)
      (Any_Of_These : Event_List;
       Enabler      : out Event)
    when
       Retry_Enabled (K)
    is
       Found_Signaled_Event : Boolean;
    begin
       Check_Signaled (Any_Of_These, Enabler, Found_Signaled_Event);
       if Found_Signaled_Event then
          return;
       end if;
       --  otherwise...
       if Retry (K)'Count = 0 then  --  current caller is last one present
          --  switch to the other Retry family member for
          --  subsequent retries
          Retry_Enabled (K) := False;
          Active_Retry := Active_Retry + 1;
       end if;
       --  NB: K + 1 wraps around to the other family member
       requeue Retry (K + 1) with abort;
    end Retry;

Note the first line:

.. code-block:: ada

    entry Retry (for K in Retry_Entry_Id)

as well as the entry barrier (before the reserved word :ada:`is`):

.. code-block:: ada

    when Retry_Enabled (K)

:ada:`K` is the entry family index, in this case iterating over all the values
of :ada:`Retry_Entry_Id` (i.e., 0 .. 1).

We don't have to write a loop checking each family member's barrier; that
happens automatically, via :ada:`K`. When a barrier at index :ada:`K` is found
to be :ada:`True`, that corresponding entry can execute a prior caller.

Note the last statement, the one performing the requeue:

.. code-block:: ada

    requeue Retry (K + 1) with abort;

Like the :ada:`Active_Retry` component, the index :ada:`K` is of the modular
type with two possible values, so :ada:`K + 1` is always the *other* entry of
the two. The addition wraps around, conveniently. As a result, the requeue is
always onto the other entry, never itself, so the entry queue ordering makes no
difference.

The :ada:`with abort` syntax can be read as "with abort enabled for the
requeued caller task." Ordinarily, an aborted task that is suspended on an
entry queue is removed from that queue. That removal is allowable in this
version of protected type :ada:`Manager`, unlike the earlier FIFO version,
because we are not using the count of prior callers to control the number of
iterations in the protected action involving :ada:`Retry`. In the FIFO
implementation we could not allow requeued callers to be removed from the
:ada:`Retry` queue because the count of prior callers would no longer match the
number of queued callers actually present. The protected action would await a
caller that would never execute. In this more robust implementation that cannot
happen, so it is safe to allow aborted tasks to be removed from the
:ada:`Retry` queue.

Note that we do still check the count of pending queued callers, we just don't
capture it and use it to control the number of iterations in the protected
action. If we've processed the last caller for member :ada:`K`, we close member
:ada:`K`\'s barrier immediately and then set the active member index to the
other entry member. Consequently, a subsequent call to :ada:`Wait` will requeue
to the other entry family member and :ada:`Signal` will, eventually, enable it.

Because we did not make the implementation visible to the package's clients,
our internal changes will not require users to change any of their code.

Note that both the Ravenscar and Jorvik profiles allow entry families, but
Ravenscar allows only one member per family because only one entry is allowed
per protected object. Such an entry family doesn't provide any benefit over a
single entry declaration. Jorvik allows multiple entry family members because
it allows multiple entries per protected object. However, neither profile
allows requeue statements, for the sake of simplifying the underlying run-time
library implementation.

The full version using the entry-family approach is provided at the end of this
text. Note that we have used a generic package so that we can factor out the
specific kind of events involved, via the generic formal type. As long as the
generic actual type is a discrete type the compiler will be happy. That
correspondence is essential because we use the event type as an index for the
array type :ada:`Event_States`.

.. code-block:: ada

    generic
       type Event is (<>);
    package Event_Management is

       type Manager is limited private;

       ...

    private

       type Event_States is array (Event) of Boolean;
       ...
    end Event_Management;

Here is a small demonstration program. As before, we just have some simple
event names to await and signal. We instantiate the generic package
:ada:`Event_Management` with that :ada:`Event` type, and also the generic
package :ada:`Ada.Numerics.Discrete_Random` so that we can randomly generate
events to test the :ada:`Event_Management` instance.

.. code-block:: ada

    --  Make the protected entry queues not be FIFO ordered, to
    --  demonstrate that the type Manager handles this case too.
    pragma Queuing_Policy (Priority_Queuing);

    with Ada.Text_IO;   use Ada.Text_IO;
    with Event_Management;

    with Ada.Numerics.Discrete_Random;

    procedure Demo_Events is

       type Event is (A, B, C, D);

       package Events is new Event_Management (Event);
       use Events;

       package Arbitrary_Event is
         new Ada.Numerics.Discrete_Random (Event);
       use Arbitrary_Event;

       G : Arbitrary_Event.Generator;

       Controller : Events.Manager;

       --  Tasks to await the events being signaled.
       --
       --  We give them priorities to exercise the priority-based
       --  implementation, but the values are arbitrary.

       task A_or_B_Processor with Priority => 5;
       task C_Processor      with Priority => 6;
       task D_Processor      with Priority => 7;

       ----------------------
       -- A_or_B_Processor --
       ----------------------

       task body A_or_B_Processor is
          Active : Event;
       begin
          loop
             Wait (Controller,
                   Any_Of_These => Event_List'(A, B),
                   Enabler => Active);
             Put_Line ("A_or_B_Processor responding to event " &
                       Active'Image);
          end loop;
       end A_or_B_Processor;

       -----------------
       -- C_Processor --
       -----------------

       task body C_Processor is
       begin
          loop
             Wait (Controller, C);
             Put_Line ("C_Processor responding to event C");
          end loop;
       end C_Processor;

       -----------------
       -- D_Processor --
       -----------------

       task body D_Processor is
       begin
          loop
             Wait (Controller, D);
             Put_Line ("D_Processor responding to event D");
          end loop;
       end D_Processor;

    begin
       loop
          Signal (Controller, Random (G));
          --  The tasks have priority for the sake of realism
          --  since the queues are now ordered by priority.
          --  However, we don't want any one task to
          --  monopolize the output, so for the sake of the
          --  demonstration we give the other tasks time to
          --  suspend on their calls to Wait too. The delay
          --  also makes the output easier to read.
          delay 0.5;
       end loop;
    end Demo_Events;

When executed, each task iteratively prints a message indicated that it is
responding to one of the awaited events. One of the tasks waits for one of two
specified events, and the other two tasks wait for a single event each. The
main procedure signals events at random. The demo runs forever so you'll have
to kill it manually.

Each task writes to :ada:`Standard_Output`. Strictly speaking, this tasking
structure allows race conditions on that shared (logical) file, but this is
just a simple demo of the event facility so it is not worth bothering to
prevent them. For the same reason, we didn't declare a task type parameterized
with a discriminant for those tasks that await a single event.


Concurrent Programming
~~~~~~~~~~~~~~~~~~~~~~

Concurrent programming applications will likely use both idioms. Thread-safe
buffers are extremely common, for example. But in addition, potentially
blocking operations are sometimes necessary within regions of code that require
mutually exclusive access.


Real-Time Programming
~~~~~~~~~~~~~~~~~~~~~

As we mentioned in the introduction, protected objects provide the same
benefits for real-time programming as they provide for concurrent programming,
albeit with additional semantics. Those additions include execution with a
priority, in particular. Clients will assign a ceiling priority to each
protected object, as described in the
:ref:`System Implementation of PO Mutual Exclusion <Ada_In_Practice_Idioms_For_Protected_Objects_System_Implementation_PO_Mutual_Exclusion>`
section above. The purpose is to limit the blocking experienced by tasks, along
with other task interaction benefits on a uniprocessor.

The GNAT package hierarchy includes a thread-safe bounded buffer abstraction
that can be used in real-time applications. The protected type is declared
within a generic package, like so:

.. code-block:: ada

    protected type Bounded_Buffer
      (Capacity : Positive;
       Ceiling  : System.Priority)
    with
       Priority => Ceiling
    is
       ...
    private
        ...
    end Bounded_Buffer;

The two discriminants allow the type to be parameterized when clients declare
objects of the type. In this case, the :ada:`Capacity` discriminant will be
given a value specifying the maximum number of :ada:`Element` values that the
object should be able to contain. More to the point here, the :ada:`Ceiling`
discriminant specifies the priority to be given to the protected object itself.


Embedded Systems Programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the canonical model for handling interrupts in Ada, the handlers are
protected procedures. The enclosing PO is again given a priority, as for
real-time programming, but now the priority level is that of the
:ada:`Interrupt_Priority` subtype. The handlers are invoked by the hardware via
the run-time library and execute at the priority specified. This is essentially
use of the first idiom for protected objects, even though the encapsulated
application data is hardware oriented.

That's the canonical model, and hence the portable approach, but other
approaches are possible. For example, if the target and OS allow it, a
developer can set up the system to directly vector to a non-protected
procedure. However, doing so is not portable, loses the benefits of the
integration with the priority scheme, and almost certainly includes limitations
on the operations allowed within the procedure body.

For further discussion, see the
:ref:`Interrupt Handling <Ada_In_Practice_Interrupt_Handling>` idiom entry.


Pros
----

Protected objects extend prior research in concurrent programming language
constructs, specifically the monitor construct that replaces semaphores,
mutexes, and condition variables. Condition synchronization is simply stated as
a Boolean expression on an entry, with caller suspension and resumption handled
automatically. The monitor's explicit mechanism for local caller suspension and
resumption is no longer required. Furthermore, protected action semantics make
the code simpler than the combination of condition variables with mutexes,
including the need for the looping idiom in the PThreads model, because when
the condition expressed by the barrier becomes :ada:`True` the awakened caller
is guaranteed to hold the mutual exclusion lock.

Protected objects add asynchronous communication and synchronization to the
existing synchronous mechanism of Ada 83, addressing a serious deficiency for
both concurrent and real-time programming in Ada. They are also critical to
embedded systems programming with Ada.

Most importantly, developers can use protected objects (types) to create just
about any synchronization and communication protocol imaginable. Especially
when combined with other language features, the result is a flexible, extremely
expressive facility.


Cons
----

The private part of a protected definition can contain only declarations for
protected operations and component declarations. This limitation leads to
declarations, such as the array type :ada:`Event_States`, that are purely
implementation artifacts but cannot be hidden inside the private part. These
artifacts will usually be declared immediately before the protected object or
type, thus making them compile-time visible to clients whenever the protected
type or object is visible to clients. Note that anonymously-typed array objects
are not allowed in the private part. You will understand why these limitations
exist when you consider that protected objects, when first conceived, were
known as *protected records*. They have only slightly more declarative options
than those of record types.

Protected objects have more capabilities than semaphores, mutexes, and
condition variables. As a consequence, they may have more run-time overhead,
but not much. For the automatic mutual exclusion implementation, the expense
can be literally zero when priorities are used instead of actual locks for the
implementation.


Relationship With Other Idioms
------------------------------

None


Notes
-----

#. Thanks to Andrei Gritsenko (Андрей Гриценко@disqus_VErl9jPNvR) for
   suggesting a nice simplification of the FIFO version of the event waiting
   facility.

#. For more on tasking and topics like this, including examples of the second
   idiom, see the book by Burns and Wellings, Concurrent and Real-Time
   Programming In Ada, Cambridge University Press, 2007. Yes, 2007, but it is
   excellent and remains directly applicable today. The implementation of the
   event manager is based on their :ada:`Resource_Controller` example, for
   example.


Full Code for :ada:`Event_Management` Generic Package
-----------------------------------------------------

The full implementation of the approach that works regardless of whether the
queues are FIFO ordered is provided below. Note that it includes some defensive
code that we did not mention above, for the sake of simplicity. See in
particular procedures :ada:`Signal` and :ada:`Wait` that take an :ada:`Event_List` as inputs.

When compiling this generic package, you may get warnings indicating that the
use of parentheses for aggregates is an obsolete feature and that square
brackets should be used instead. Feel free to ignore them. Parentheses are not
obsolete, neither in a practical sense nor in the language standard's sense of
being obsolescent. There are indeed cases where square brackets are better, or
even required, but those situations don't appear here.

.. code:: ada no_button project=Courses.Ada_In_Practice.Idioms_For_Protected_Objects.Event_Management

    --  This package provides a means for blocking a calling task
    --  until/unless any one of an arbitrary set of "events" is
    --  "signaled."

    --  NOTE: this implementation allows either priority-ordered or
    --  FIFO-ordered queuing.

    generic
       type Event is (<>);
    package Event_Management is

       type Manager is limited private;

       type Event_List is array (Positive range <>) of Event;

       procedure Wait
         (This         : in out Manager;
          Any_Of_These :        Event_List;
          Enabler      :    out Event)
       with
         Pre => Any_Of_These'Length > 0;
       --  Block until/unless any one of the events in Any_Of_These has
       --  been signaled. The one enabling event will be returned in the
       --  Enabler parameter, and is cleared internally as Wait exits.
       --  Any other signaled events remain signaled. Note that,
       --  when Signal is called, the events within the aggregate
       --  Any_of_These are checked (for whether they are signaled)
       --  in the order they appear in the aggregate. We use a precondition
       --  on Wait because the formal parameter Enabler is mode out, and type
       --  Event is a discrete type. As such, if there was nothing in the list
       --  to await, the call would return immediately, leaving Enabler's value
       --  undefined.

       procedure Wait
         (This     : in out Manager;
          This_One : Event);
       --  Block until/unless the specified event has been signaled.
       --  This procedure is a convenience routine that can be used
       --  instead of an aggregate with only one event component.

       procedure Signal
         (This         : in out Manager;
          All_Of_These : Event_List);
       --  Indicate that all of the events in All_Of_These are now
       --  signaled. The events remain signaled until cleared by Wait.
       --  We don't use a similar precondition like that of procedure
       --  Wait because, for Signal, doing nothing is what the empty
       --  list requests.

       procedure Signal
         (This     : in out Manager;
          This_One : Event);
       --  Indicate that event This_One is now signaled. The event
       --  remains signaled until cleared by Wait. This procedure is a
       --  convenience routine that can be used instead of an aggregate
       --  with only one event component.

    private

       type Event_States is array (Event) of Boolean;

       type Retry_Entry_Id is mod 2;

       type Retry_Barriers is array (Retry_Entry_Id) of Boolean;

       protected type Manager is
          entry Wait
            (Any_Of_These : Event_List;
             Enabler      : out Event);
          procedure Signal (All_Of_These : Event_List);
       private
          Signaled      : Event_States := (others => False);
          Retry_Enabled : Retry_Barriers := (others => False);
          Active_Retry  : Retry_Entry_Id := Retry_Entry_Id'First;
          entry Retry (Retry_Entry_Id)
            (Any_Of_These : Event_List;
             Enabler      : out Event);
       end Manager;

    end Event_Management;

And the generic package body:

.. code:: ada compile_button project=Courses.Ada_In_Practice.Idioms_For_Protected_Objects.Event_Management

    package body Event_Management is

       ----------
       -- Wait --
       ----------

       procedure Wait
         (This         : in out Manager;
          Any_Of_These :        Event_List;
          Enabler      :    out Event)
       is
       begin
          This.Wait (Any_Of_These, Enabler);
       end Wait;

       ----------
       -- Wait --
       ----------

       procedure Wait
         (This     : in out Manager;
          This_One : Event)
       is
          Unused : Event;
       begin
          This.Wait (Event_List'(1 => This_One), Unused);
       end Wait;

       ------------
       -- Signal --
       ------------

       procedure Signal
         (This         : in out Manager;
          All_Of_These : Event_List)
       is
       begin
           --  Calling Manager.Signal has an effect even when the list
           --  is empty, albeit minor, so we don't call it in that case
          if All_Of_These'Length > 0 then
             This.Signal (All_Of_These);
          end if;
       end Signal;

       ------------
       -- Signal --
       ------------

       procedure Signal
         (This     : in out Manager;
          This_One : Event)
       is
       begin
          This.Signal (Event_List'(1 => This_One));
       end Signal;

       -------------
       -- Manager --
       -------------

       protected body Manager is

          procedure Check_Signaled
            (These   : Event_List;
             Enabler : out Event;
             Found   : out Boolean);

          ----------
          -- Wait --
          ----------

          entry Wait
            (Any_Of_These : Event_List;
             Enabler      : out Event)
          when
             True
          is
             Found_Signaled_Event : Boolean;
          begin
             Check_Signaled (Any_Of_These, Enabler, Found_Signaled_Event);
             if not Found_Signaled_Event then
                requeue Retry (Active_Retry) with abort;
             end if;
          end Wait;

          ------------
          -- Signal --
          ------------

          procedure Signal (All_Of_These : Event_List) is
          begin
             for C of All_Of_These loop
                Signaled (C) := True;
             end loop;
             Retry_Enabled (Active_Retry) := True;
          end Signal;

          -----------
          -- Retry --
          -----------

          entry Retry (for K in Retry_Entry_Id)
            (Any_Of_These : Event_List;
             Enabler      : out Event)
          when
             Retry_Enabled (K)
          is
             Found_Signaled_Event : Boolean;
          begin
             Check_Signaled (Any_Of_These, Enabler, Found_Signaled_Event);
             if Found_Signaled_Event then
                return;
             end if;
             --  otherwise...
             if Retry (K)'Count = 0 then -- current caller is last one
                --  switch to the other Retry family member for
                --  subsequent retries
                Retry_Enabled (K) := False;
                Active_Retry := Active_Retry + 1;
             end if;
             --  NB: K + 1 wraps around to the other family member
             requeue Retry (K + 1) with abort;
          end Retry;

          --------------------
          -- Check_Signaled --
          --------------------

          procedure Check_Signaled
            (These   : Event_List;
             Enabler : out Event;
             Found   : out Boolean)
          is
          begin
             for C of These loop
                if Signaled (C) then
                   Signaled (C) := False;
                   Enabler := C;
                   Found := True;
                   return;
                end if;
             end loop;
             Enabler := Event'First; -- arbitrary, avoids undefined value
             Found := False;
          end Check_Signaled;

       end Manager;

    end Event_Management;

