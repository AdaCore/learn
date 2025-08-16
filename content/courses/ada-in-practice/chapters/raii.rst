.. _Ada_In_Practice_RAII:

Resource Acquisition Is Initialization (RAII)
=============================================

.. include:: ../../global.txt

Motivation
----------

In order for the expected semantics to be obtained, some types require clients
to follow a specific protocol when calling the type's operations. Furthermore,
failing to follow the protocol can cause system-wide ill effects.

For example, in the context of competing concurrent threads accessing shared
resources, concurrency abstractions such as mutexes provide the mutually
exclusive access necessary to prevent race conditions. These mutex objects must
be 1) both acquired and released, 2) by every thread accessing that shared
resource, 3) at the right places in the source code, and 4) in the proper
order. Failure to acquire the mutex prior to accessing the shared resource
leads to race conditions, and failure to release it can lead to deadlocks.
Ensuring the mutex is released is complicated by the possibility of exceptions
raised after the lock is acquired.

Although concurrency is a prime example, the issue is general in nature. We
will continue with the concurrency context for the sake of discussion.

Like the classic *monitor* concept (:footcite:p:`1977:hansen`,
:footcite:p:`1973:hansen`, :footcite:p:`1974:hoare`) on which they are based.
(In the works cited, Hoare's contribution was equally important, but Hansen's
contributions were reified in
:wikipedia:`Concurrent Pascal <Concurrent_Pascal>`, a concrete programming
language.), Ada defines a protected object (PO) as a concurrency construct that
is higher-level and more robust than mutexes and semaphores. Those advantages
accrue because the bodies of the protected operations implement only the
functional requirements, with guaranteed mutually exclusive access to the
protected data. The underlying implementation, i.e., the run-time library,
provides the necessary exclusive access, and also thread management. As a
result, the lock is guaranteed to be acquired, and likewise, guaranteed to be
relinquished, even in the face of exceptions.

However, a protected object is not always appropriate. Consider an existing
sequential program that makes calls to visible procedures provided by a
package:

.. code-block:: ada

    package P is

       procedure Operation_1;

       procedure Operation_2;

       --  ...

    end P;

Inside the package body are one or more state variables that are manipulated by
the procedures (i.e., as in an
:ref:`Abstract Data Machine <Ada_In_Practice_Abstract_Data_Machines>`):

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    package body P is

       State : Integer := 0;

       procedure Operation_1 is
       begin
          State := State + 1;  -- for example...
          Put_Line ("State is now " & State'Image);
       end Operation_1;

       procedure Operation_2 is
       begin
          State := State - 1;  -- for example...
          Put_Line ("State is now " & State'Image);
       end Operation_2;

       --  ...

    end P;

This design is reasonable in a strictly sequential caller context. But if new
application requirements are such that multiple tasks will be calling these
procedures asynchronously, there is a problem. The package-level variable
:ada:`State` will be subject to race conditions because it is (indirectly)
shared among the calling tasks. Race conditions tend to be Heisenbugs because
they are timing-dependent, so they can be exceedingly difficult to identify and
expensive to debug.

In response to the new requirements, we could declare a protected object within
the package body and move the declaration of :ada:`State` into that PO. In
addition, we would declare two protected procedures corresponding to
:ada:`Operation_1` and :ada:`Operations_2`. The two new protected procedure
bodies would do what the original procedures did, including accessing and
updating :ada:`State`. The original procedures |mdash| still presented to
clients |mdash| would now call these new protected procedures:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    package body P is

       protected Threadsafe is
          procedure Operation_1;
          procedure Operation_2;
       private
          State : Integer := 0;
       end Threadsafe;

       protected body Threadsafe is

          procedure Operation_1 is
          begin
             State := State + 1;  -- for example...
             Put_Line ("State is now " & State'Image);
          end Operation_1;

          procedure Operation_2 is
          begin
             State := State - 1;  -- for example...
             Put_Line ("State is now " & State'Image);
          end Operation_2;

       end Threadsafe;

       procedure Operation_1 is
       begin
          Threadsafe.Operation_1;
       end Operation_1;

       procedure Operation_2 is
       begin
          Threadsafe.Operation_2;
       end Operation_2;

       --  ...

    end P;

As a result, there can be no race conditions on :ada:`State`, and the source
code is both simple and robust.

We could put the PO in the package spec and then clients could call the
protected object's operations directly, but changing all the clients could be
expensive.

However, this new design is not portable because the two :ada:`Threadsafe`
protected procedure bodies both call a potentially blocking operation, in this
case :ada:`Ada.Text_IO.Put_Line`. Erroneous execution is the result. It might
work as intended when executed, or it might do something else, or, if detected,
:ada:`Program_Error` will be raised. On a run-time library built on top of an
operating system, it may work because the OS may provide thread locking
mechanisms that the run-time library can use. In that case a blocking operation
just suspends the caller thread's execution temporarily without releasing the
PO lock. Although the blocking operation would allow some other caller task to
be dispatched, no other caller could acquire that same PO lock, so race
conditions are prevented within that PO. When the blocking operation returns,
the protected procedure body can continue executing, still holding the lock.
However, on a run-time library that does not use locks for mutual exclusion
|mdash| it can use priorities, in particular |mdash| another caller to that
same PO could access the enclosed variables while the first caller is blocked,
thus breaking the mutually exclusive access guarantee.

Calling an I/O operation is not all that strange here, and those are not the
only potentially blocking operation defined by the language.

Note that moving the calls to :ada:`Put_Line` out of the PO procedure bodies,
back to the regular procedure bodies that call those PO procedures, would solve
the portability problem but would not work functionally. There would be no
guarantee that a call to :ada:`Put_Line` would immediately follow the execution
of the protected procedure called immediately before it in the source code.
Hence the printed text might not reflect the current value of the :ada:`State`
variable.

As a consequence, we must fall back to manually acquiring and releasing an
explicit lock. For example, we could declare a lock object at the package
level, as shown below, and have each operation acquire and release it:

.. code-block:: ada

    with GNAT.Semaphores;  use GNAT.Semaphores;
    with Ada.Text_IO;      use Ada.Text_IO;

    package body P is

       subtype Mutual_Exclusion is Binary_Semaphore
        (Initially_Available => True,
         Ceiling             => Default_Ceiling);

       Lock : Mutual_Exclusion;

       State : Integer := 0;

       procedure Operation_1 is
       begin
          Lock.Seize;
          State := State + 1;  -- for example...
          Put_Line ("State is now" & State'Img);
          Lock.Release;
       exception
          when others =>
             Lock.Release;
             raise;
       end Operation_1;

       procedure Operation_2 is
       begin
          Lock.Seize;
          State := State - 1;  -- for example...
          Put_Line ("State is now" & State'Img);
          Lock.Release;
       exception
          when others =>
             Lock.Release;
             raise;
       end Operation_2;

    end P;

The subtype :ada:`Mutual_Exclusion` is just a binary semaphore with the
discriminant set so that any object of the subtype is initially available. You
can assume it is a protected type with classic binary semaphore semantics. See
package :ada:`GNAT.Semaphores` for the details. The ceiling discriminant isn't
important here, but we must set them all if we set any of them.

This design works, but the resulting code is clearly more complex and less
robust than the PO approach.

Solution
--------

Our solution uses an explicit global lock (a mutex), as above, but reintroduces
automatic lock acquisition and release.

To achieve that automation, we leverage the language-defined object *lifetime*
rules. These rules specify that an object is initialized when it is created and
finalized when it is about to be destroyed. Initialization and finalization may
be null operations, and thus absent from the object code, but application
developers can define explicit initialization and finalization operations. When
defined, these operations are called automatically by the underlying
implementation, during the object's lifetime.

We will use the object initialization operation to seize the global lock and
the object finalization operation to release it. The object lifetime rules will
ensure that the lock's operations are called at the necessary times, thereby
providing the required mutually exclusive access. In addition, the rules will
ensure that the lock will be released even if an exception is raised in the
bracketed application code.

Developers may be familiar with this approach under the name
:wikipedia:`Resource Acquisition Is Initialization <Resource_acquisition_is_initialization>`.
Another name for this technique is *Scope-Bound Resource Management* because of
the initialization and finalization steps invoked upon scope entry and exit.

Therefore, we will create a new type with user-defined initialization and
finalization operations. We name this new type :ada:`Lock_Manager` because the
type provides a wrapper for locks, rather than being a lock directly. Object
creation and destruction will invoke the initialization and finalization
routines, automatically.

Because they are wrappers for locks, each object of this type will reference a
distinct lock object so that the initialization and finalization operations can
manipulate that lock object. We use an access discriminant to designate that
lock. By doing so, we decouple the new type from the specific lock, and thus
from the application code. Otherwise, the new facility would not be reusable.

The resulting relationship between the global shared lock and the local object
will be as follows:

.. code-block:: ada

       Lock : Mutual_Exclusion;

       procedure Op is
          LM : Lock_Manager (<pointer to Lock>)
          --  <initialization automatically called for LM>
       begin
          --  ... sequence of statements for Op
          --  <finalization called for LM>
       end Op;

The language rules specify that a subprogram's local declarative part is
elaborated prior to the execution of that subprogram's sequence of statements.
During that elaboration, objects are created and initialized. The object
creation for :ada:`LM` precedes the sequence of statements in the procedure
body for :ada:`Op`, so the designated lock will be acquired prior to the shared
resource use within that body.

Similarly, the rules specify that finalization occurs when an object is about
to cease to exist, in this case because the local object :ada:`LM` goes out of
scope. That won't happen until the end of the sequence of statements is reached
for :ada:`Op`, in the normal case, so finalization will ensure that the lock is
released after any possible reference in :ada:`Op`\'s statement sequence. The
run-time will also invoke finalization in the face of exceptions because
exceptions also cause the scope to be exited.

To define the :ada:`Lock_Manager` type, we declare it in a separate package as
a tagged limited private type with a discriminant designating a
:ada:`Mutual_Exclusion` object:

.. code-block:: ada

      type Lock_Manager (Lock : not null access Mutual_Exclusion) is
         tagged limited private;

We make it a limited type because copying doesn't make sense semantically for
:ada:`Lock_Manager` objects.

Only *controlled* types support user-defined initialization and finalization
operations (as of Ada 2022). Therefore, in the package private part the type is
fully declared as a controlled type derived from
:ada:`Ada.Finalization.Limited_Controlled`, as shown below. We hide the fact
that the type will be controlled because we don't intend :ada:`Initialize` and
:ada:`Finalize` to be called manually by clients.

.. code-block:: ada

       type Lock_Manager (Lock : not null access Mutual_Exclusion) is
          new Ada.Finalization.Limited_Controlled with null record;

No additional record components are required, beyond the access discriminant.

Immediately following the type declaration, we declare overridden versions of
the inherited procedures :ada:`Initialize` and :ada:`Finalize`:

.. code-block:: ada

       overriding procedure Initialize (This : in out Lock_Manager);
       overriding procedure Finalize   (This : in out Lock_Manager);

These are the operations called automatically by the implementation.

The full package spec is as follows:

.. code-block:: ada

    with Ada.Finalization;
    with GNAT.Semaphores; use GNAT.Semaphores;

    package Lock_Managers is

      subtype Mutual_Exclusion is Binary_Semaphore
        (Initially_Available => True,
         Ceiling             => Default_Ceiling);

       type Lock_Manager (Lock : not null access Mutual_Exclusion) is
          tagged limited private;

    private

       type Lock_Manager (Lock : not null access Mutual_Exclusion) is
          new Ada.Finalization.Limited_Controlled with null record;

       overriding procedure Initialize (This : in out Lock_Manager);
       overriding procedure Finalize (This : in out Lock_Manager);

    end Lock_Managers;

The fact that there are no visible primitive operations tells the reader that
this is a somewhat different :ref:`ADT <Ada_In_Practice_Abstract_Data_Types>`.
The most useful thing a client can do with such a type is to declare objects,
but that's exactly what we want.

Each overridden procedure simply references the lock designated by the formal
parameter's :ada:`Lock` discriminant:

.. code-block:: ada

    package body Lock_Managers is

       ----------------
       -- Initialize --
       ----------------

       overriding procedure Initialize (This : in out Lock_Manager) is
       begin
          This.Lock.Seize;
       end Initialize;

       --------------
       -- Finalize --
       --------------

       overriding procedure Finalize (This : in out Lock_Manager) is
       begin
          This.Lock.Release;
       end Finalize;

    end Lock_Managers;

The resulting user code is almost unchanged from the original sequential code:

.. code-block:: ada

    with Ada.Text_IO;    use Ada.Text_IO;
    with Lock_Managers;  use Lock_Managers;

    package body P is

       State : Integer := 0;

       Lock : aliased Mutual_Exclusion;

       procedure Operation_1 is
          LM : Lock_Manager (Lock'Access) with Unreferenced;
       begin
          State := State + 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_1;

       procedure Operation_2 is
          LM : Lock_Manager (Lock'Access) with Unreferenced;
       begin
          State := State - 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_2;

    end P;

The aspect :ada:`Unreferenced` tells the compiler that no references in the
source code are expected. That has two effects during compilation. First,
warnings about the lack of references in the source code are disabled.
Ordinarily we'd want those warnings because an unreferenced object usually
indicates a coding error. That warning would be *noise* for objects of this
type. But by the same token, the compiler will issue a warning if some explicit
reference is present, perhaps added much later in the project lifetime.

Pros
----

Race conditions are precluded, the client code is simpler than direct manual
calls, and the code is robust, especially concerning exceptions. These
advantages are significant, given the cost in engineering time to debug the
errors this design prevents.


Cons
----

The lock is global, so all calls go through it. Hence all calls are sequential,
even if some could run concurrently. In the above example that's exactly as
required, but in other situations it might be too limiting.

Compared to the manual call approach, the run-time cost for keeping track of
objects to be finalized could be non-trivial. That's likely true in any
language.


Relationship With Other Idioms
------------------------------

None.


Notes
-----

- The name for a similar type in the C++ Boost library is :cpp:`Scoped_Lock`,
  as is the Ada type in the GNAT library package :ada:`GNATColl.Locks`. I used
  :ada:`Scope_Lock` in AdaCore's Gem #70 :footcite:p:`2009:rogers`.

- I didn't invent the name :ada:`Scope_Lock` or the Ada implementation, but I
  don't recall where I first saw it many years ago. My apologies to that
  author.

- I consider the name :ada:`Lock_Manager` or something similar to be better,
  since objects of the type are wrappers for locks, not locks themselves.
  Indeed, in C++ 2011 the name is :cpp:`lock_guard`.


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
