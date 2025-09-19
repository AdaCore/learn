Concurrency
-------------

.. include:: ../../../global.txt

.. todo::

  *Update with comparison to new C++11 concurrency features*

Tasks
~~~~~~

Java and Ada both provide support for concurrency in the language. The C++ language has added a concurrency facility in its most recent revision, C++11, but we are assuming that most C++ programmers are not (yet) familiar with these new features. We thus provide the following mock API for C++ which is similar to the Java :java:`Thread` class:

.. code-block:: cpp

   class Thread {
      public:
         virtual void run (); // code to execute
         void start (); // starts a thread and then call run ()
         void join (); // waits until the thread is finished
   };

Each of the following examples will display the 26 letters of the alphabet twice, using two concurrent threads/tasks. Since there is no synchronization between the two threads of control in any of the examples, the output may be interspersed.

[Ada]

.. code-block:: ada

   procedure Main is -- implicitly called by the environment task
      task My_Task;

      task body My_Task is
      begin
         for I in 'A' .. 'Z' loop
            Put_Line (I);
         end loop;
      end My_Task;
   begin
      for I in 'A' .. 'Z' loop
         Put_Line (I);
      end loop;
   end Main;

[C++]

.. code-block:: cpp

   class MyThread : public Thread {
      public:

      void run () {
         for (char i = 'A'; i <= 'Z'; ++i) {
            cout << i << endl;
         }
      }
   };

   int main (int argc, char ** argv) {
      MyThread myTask;
      myTask.start ();

      for (char i = 'A'; i <= 'Z'; ++i) {
         cout << i << endl;
      }

      myTask.join ();

      return 0;
   }

[Java]

.. code-block:: java

   public class Main {
      static class MyThread extends Thread {
         public void run () {
            for (char i = 'A'; i <= 'Z'; ++i) {
               System.out.println (i);
            }
         }
      }

      public static void main (String args) {
         MyThread myTask = new MyThread ();
         myTask.start ();

         for (char i = 'A'; i <= 'Z'; ++i) {
            System.out.println (i);
         }
         myTask.join ();
      }
   }

Any number of Ada tasks may be declared in any declarative region. A task declaration is very similar to a procedure or package declaration. They all start automatically when control reaches the :ada:`begin`. A block will not exit until all sequences of statements defined within that scope, including those in tasks, have been completed.

A task type is a generalization of a task object; each object of a task type has the same behavior. A declared object of a task type is started within the scope where it is declared, and control does not leave that scope until the task has terminated.

An Ada task type is somewhat analogous to a Java :java:`Thread` subclass, but in Java the instances of such a subclass are always dynamically allocated.  In Ada an instance of a task type may either be declared or dynamically allocated.

Task types can be parametrized; the parameter serves the same purpose as an argument to a constructor in Java. The following example creates 10 tasks, each of which displays a subset of the alphabet contained between the parameter and the :ada:`'Z'` Character.  As with the earlier example, since there is no synchronization among the tasks, the output may be interspersed depending on the implementation's task scheduling algorithm.


[Ada]

.. code-block:: ada

   task type My_Task (First : Character);

   task body My_Task is
   begin
      for I in First .. 'Z' loop
         Put_Line (I);
      end loop;
   end My_Task;

   procedure Main is
      Tab : array (0 .. 9) of My_Task ('G');
   begin
      null;
   end Main;

[C++]

.. code-block:: cpp

   class MyThread : public Thread {
      public:

      char first;

      void run () {
         for (char i = first; i <= 'Z'; ++i) {
            cout << i << endl;
         }
      }
   };

   int main (int argc, char ** argv) {
      MyThread tab [10];

      for (int i = 0; i < 9; ++i) {
         tab [i].first = 'G';
         tab [i].start ();
      }

      for (int i = 0; i < 9; ++i) {
         tab [i].join ();
      }

      return 0;
   }

[Java]

.. code-block:: java

   public class MyThread extends Thread {
      public char first;

      public MyThread (char first){
         this.first = first;
      }

      public void run () {
         for (char i = first; i <= 'Z'; ++i) {
            cout << i << endl;
         }
      }
   }

   public class Main {
      public static void main (String args) {
         MyThread [] tab = new MyThread [10];

         for (int i = 0; i < 9; ++i) {
            tab [i] = new MyThread ('G');
            tab [i].start ();
         }

         for (int i = 0; i < 9; ++i) {
            tab [i].join ();
         }
      }
   }

In Ada a task may be allocated on the heap as opposed to the stack. The task will then start as soon as it has been allocated, and terminates when its work is completed. This model is probably the one that's the most similar to Java:


[Ada]

.. code-block:: ada

   type Ptr_Task is access My_Task;

   procedure Main is
      T : Ptr_Task;
   begin
      T := new My_Task ('G');
   end Main;

[C++]

.. code-block:: cpp

   int main (int argc, char ** argv) {
      MyThread * t = new MyThread ();
      t->first = 'G';
      t->start ();
      return 0;
   }

[Java]

.. code-block:: java

   public class Main {
      public static void main (String args) {
         MyThread t = new MyThread ('G');

         t.start ();
      }
   }

Rendezvous
~~~~~~~~~~~~

.. todo::

   *Check if rendezvous is supported in Java or C++ through an API*

A rendezvous is a synchronization between two tasks, allowing them to exchange data and coordinate execution. Ada's rendezvous facility cannot be modeled with C++ or Java without complex machinery. Therefore, this section will just show examples written in Ada.

Let's consider the following example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is

      task After is
         entry Go;
      end After ;

      task body After is
      begin
         accept Go;
         Put_Line ("After");
      end After;

   begin
      Put_Line ("Before");
      After.Go;
   end;

The :ada:`Go` :ada:`entry` declared in :ada:`After` is the external interface to the task. In the task body, the :ada:`accept` statement causes the task to wait for a call on the entry. This particular :ada:`entry` and :ada:`accept` pair doesn't do much more than cause the task to wait until :ada:`Main` calls :ada:`After.Go`. So, even though the two tasks start simultaneously and execute independently, they can coordinate via :ada:`Go`. Then, they both continue execution independently after the rendezvous.

The :ada:`entry`\/:ada:`accept` pair can take/pass parameters, and the :ada:`accept` statement can contain a sequence of statements; while these statements are executed, the caller is blocked.

Let's look at a more ambitious example. The rendezvous below accepts parameters and executes some code:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is

      task After is
         entry Go (Text : String);
      end After ;

      task body After is
      begin
         accept Go (Text : String) do
            Put_Line ("After: " & Text);
         end Go;
      end After;

   begin
      Put_Line ("Before");
      After.Go ("Main");
   end;

In the above example, the :ada:`Put_Line` is placed in the :ada:`accept` statement. Here's a possible execution trace, assuming a uniprocessor:

1. At the :ada:`begin` of :ada:`Main`, task :ada:`After` is started and the main procedure is suspended.

2. :ada:`After` reaches the :ada:`accept` statement and is suspended, since there is no pending call on the :ada:`Go` entry.

3. The main procedure is awakened and executes the :ada:`Put_Line` invocation, displaying the string "Before".

4. The main procedure calls the :ada:`Go` entry.  Since :ada:`After` is suspended on its :ada:`accept` statement for this entry, the call succeeds.

5. The main procedure is suspended, and the task :ada:`After` is awakened to execute the body of the :ada:`accept` statement. The actual parameter :ada:`"Main"` is passed to the :ada:`accept` statement, and the :ada:`Put_Line` invocation is executed. As a result, the string :ada:`"After: Main"` is displayed.

6. When the :ada:`accept` statement is completed, both the :ada:`After` task and the main procedure are ready to run.  Suppose that the :ada:`Main` procedure is given the processor. It reaches its :ada:`end`, but the local task :ada:`After` has not yet terminated.  The main procedure is suspended.

7. The :ada:`After` task continues, and terminates since it is at its :ada:`end`.  The main procedure is resumed, and it too can terminate since its dependent task has terminated.

The above description is a conceptual model; in practice the implementation can perform various optimizations to avoid unnecessary context switches.


Selective Rendezvous
~~~~~~~~~~~~~~~~~~~~~

The accept statement by itself can only wait for a single event (call) at a time. The :ada:`select` statement allows a task to listen for multiple events simultaneously, and then to deal with the first event to occur. This feature is illustrated by the task below, which maintains an integer value that is modified by other tasks that call :ada:`Increment`, :ada:`Decrement`, and :ada:`Get`:

.. code-block:: ada

   task Counter is
      entry Get (Result : out Integer);
      entry Increment;
      entry Decrement;
   end Counter;

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
            delay 60.0;  --  delay 1 minute
            exit;
         end select;
      end loop;
   end Counter;

When the task's statement flow reaches the :ada:`select`, it will wait for all four events |mdash| three entries and a delay |mdash| in parallel. If the delay of one minute is exceeded, the task will execute the statements following the :ada:`delay` statement (and in this case will exit the loop, in effect terminating the task). The accept bodies for the :ada:`Increment`, :ada:`Decrement`, or :ada:`Get` entries will be otherwise executed as they're called. These four sections of the :ada:`select` statement are mutually exclusive: at each iteration of the loop, only one will be invoked. This is a critical point; if the task had been written as a package, with procedures for the various operations, then a "race condition" could occur where multiple tasks simultaneously calling, say, :ada:`Increment`, cause the value to only get incremented once. In the tasking version, if multiple tasks simultaneously call :ada:`Increment` then only one at a time will be accepted, and the value will be incremented by each of the tasks when it is accepted.

More specifically, each entry has an associated queue of pending callers.  If a task calls one of the entries and :ada:`Counter` is not ready to accept the call (i.e., if :ada:`Counter` is not suspended at the :ada:`select` statement) then the calling task is suspended, and placed in the queue of the entry that it is calling.  From the perspective of the :ada:`Counter` task, at any iteration of the loop there are several possibilities:

* There is no call pending on any of the entries.  In this case :ada:`Counter` is suspended.  It will be awakened by the first of two events: a call on one of its entries (which will then be immediately accepted), or the expiration of the one minute delay (whose effect was noted above).

* There is a call pending on exactly one of the entries.  In this case control passes to the :ada:`select` branch with an :ada:`accept` statement for that entry.  The choice of which caller to accept, if more than one, depends on the queuing policy, which can be specified via a pragma defined in the Real-Time Systems Annex of the Ada standard; the default is First-In First-Out.

* There are calls pending on more than one entry.  In this case one of the entries with pending callers is chosen, and then one of the callers is chosen to be de-queued (the choices depend on the queueing policy).


Protected Objects
~~~~~~~~~~~~~~~~~~

Although the rendezvous may be used to implement mutually exclusive access to a shared data object, an alternative (and generally preferable) style is through a *protected object*, an efficiently implementable mechanism that makes the effect more explicit. A protected object has a public interface (its *protected operations*) for accessing and manipulating the object's components (its private part). Mutual exclusion is enforced through a conceptual lock on the object, and encapsulation ensures that the only external access to the components are through the protected operations.

Two kinds of operations can be performed on such objects: read-write operations by procedures or entries, and read-only operations by functions. The lock mechanism is implemented so that it's possible to perform concurrent read operations but not concurrent write or read/write operations.

Let's reimplement our earlier tasking example with a protected object called :ada:`Counter`:

.. code-block:: ada

   protected Counter is
      function Get return Integer;
      procedure Increment;
      procedure Decrement;
   private
      Value : Integer := 0;
   end Counter;

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

Having two completely different ways to implement the same paradigm might seem complicated. However, in practice the actual problem to solve usually drives the choice between an active structure (a task) or a passive structure (a protected object).

A protected object can be accessed through prefix notation:

.. code-block:: ada

   Counter.Increment;
   Counter.Decrement;
   Put_Line (Integer'Image (Counter.Get));

A protected object may look like a package syntactically, since it contains declarations that can be accessed externally using prefix notation. However, the declaration of a protected object is extremely restricted; for example, no public data is allowed, no types can be declared inside, etc. And besides the syntactic differences, there is a critical semantic distinction: a protected object has a conceptual lock that guarantees mutual exclusion; there is no such lock for a package.

Like tasks, it's possible to declare protected types that can be instantiated several times:

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
      ...
   end;

Protected objects and types can declare a procedure-like operation known as an "entry". An entry is somewhat similar to a procedure but includes a so-called *barrier condition* that must be true in order for the entry invocation to succeed. Calling a protected entry is thus a two step process: first, acquire the lock on the object, and then evaluate the barrier condition.  If the condition is true then the caller will execute the entry body.  If the condition is false, then the caller is placed in the queue for the entry, and relinquishes the lock.  Barrier conditions (for entries with non-empty queues) are reevaluated upon completion of protected procedures and protected entries.

Here's an example illustrating protected entries: a protected type that models a binary semaphore / persistent signal.

.. code-block:: ada

  protected type Binary_Semaphore is
    entry Wait;
    procedure Signal;
  private
    Signaled : Boolean := False;
  end Binary_Semaphore;

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

Ada concurrency features provide much further generality than what's been presented here. For additional information please consult one of the works cited in the *References* section.
