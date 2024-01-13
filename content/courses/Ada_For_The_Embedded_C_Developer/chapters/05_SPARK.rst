Enhancing Verification with SPARK and Ada
============================================

.. include:: ../../global.txt

Understanding Exceptions and Dynamic Checks
-------------------------------------------

In Ada, several common programming errors that are not already detected
at compile-time are detected instead at run-time, triggering
"exceptions" that interrupt the normal flow of execution. For example,
an exception is raised by an attempt to access an array component via an
index that is out of bounds. This simple check precludes exploits based
on buffer overflow. Several other cases also raise language-defined
exceptions, such as scalar range constraint violations and null pointer
dereferences. Developers may declare and raise their own
application-specific exceptions too. (Exceptions are software artifacts,
although an implementation may map hardware events to exceptions.)

Exceptions are raised during execution of what we will loosely define as
a "frame." A frame is a language construct that has a call stack entry
when called, for example a procedure or function body. There are a few
other constructs that are also pertinent but this definition will
suffice for now.

Frames have a sequence of statements implementing their functionality.
They can also have optional "exception handlers" that specify the
response when exceptions are "raised" by those statements. These
exceptions could be raised directly within the statements, or indirectly
via calls to other procedures and functions.

For example, the frame below is a procedure including three exceptions
handlers:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exceptions
    :class: ada-nocheck

    procedure P is
    begin
       Statements_That_Might_Raise_Exceptions;
    exception
       when A =>
          Handle_A;
       when B =>
          Handle_B;
       when C =>
          Handle_C;
    end P;

The three exception handlers each start with the word :ada:`when` (lines
5, 7, and 9). Next comes one or more exception identifiers, followed by
the so-called "arrow." In Ada, the arrow always associates something on
the left side with something on the right side. In this case, the left
side is the exception name and the right side is the handler's code for
that exception.

Each handler's code consists of an arbitrary sequence of statements, in
this case specific procedures called in response to those specific
exceptions. If exception :ada:`A` is raised we call procedure
:ada:`Handle_A` (line 6), dedicated to doing the actual work of handling
that exception. The other two exceptions are dealt with similarly, on
lines 8 and 10.

Structurally, the exception handlers are grouped together and textually
separated from the rest of the code in a frame. As a result, the
sequence of statements representing the normal flow of execution is
distinct from the section representing the error handling. The
reserved word :ada:`exception` separates these two sections (line 4
above). This separation helps simplify the overall flow, increasing
understandability. In particular, status result codes are not required
so there is no mixture of error checking and normal processing. If no
exception is raised the exception handler section is automatically
skipped when the frame exits.

Note how the syntactic structure of the exception handling section
resembles that of an Ada case statement. The resemblance is intentional,
to suggest similar behavior. When something in the statements of the
normal execution raises an exception, the corresponding exception
handler for that specific exception is executed. After that, the routine
completes. The handlers do not "fall through" to the handlers below. For
example, if exception :ada:`B` is raised, procedure :ada:`Handle_B` is
called but :ada:`Handle_C` is not called. There's no need for a
:c:`break` statement, just as there is no need for it in a case
statement. (There's no break statement in Ada anyway.)

So far, we've seen a frame with three specific exceptions handled. What
happens if a frame has no handler for the actual exception raised? In
that case the run-time library code goes "looking" for one.

Specifically, the active exception is propagated up the dynamic call
chain. At each point in the chain, normal execution in that caller is
abandoned and the handlers are examined. If that caller has a handler
for the exception, the handler is executed. That caller then returns
normally to its caller and execution continues from there. Otherwise,
propagation goes up one level in the call chain and the process repeats.
The search continues until a matching handler is found or no callers
remain. If a handler is never found the application terminates
abnormally. If the search reaches the main procedure and it has a
matching handler it will execute the handler, but, as always, the
routine completes so once again the application terminates.

For a concrete example, consider the following:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exceptions

   package Arrays is

      type List is array (Natural range <>) of Integer;

      function Value (A : List; X, Y : Integer) return Integer;

   end Arrays;

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exceptions

   package body Arrays is

      function Value (A : List; X, Y : Integer) return Integer is
      begin
         return A (X + Y * 10);
      end Value;

   end Arrays;

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exceptions

   with Ada.Text_IO; use Ada.Text_IO;
   with Arrays;      use Arrays;

   procedure Some_Process is
      L : constant List (1 .. 100) := (others => 42);
   begin
      Put_Line (Integer'Image (Value (L, 1, 10)));
   exception
      when Constraint_Error =>
         Put_Line ("Constraint_Error caught in Some_Process");
         Put_Line ("Some_Process completes normally");
   end Some_Process;

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exceptions

   with Some_Process;
   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is
   begin
      Some_Process;
      Put_Line ("Main completes normally");
   end Main;

Procedure :ada:`Main` calls :ada:`Some_Process`, which in turn calls
function :ada:`Value` (line 7). :ada:`Some_Process` declares the array
object :ada:`L` of type :ada:`List` on line 5, with bounds 1 through
100. The call to :ada:`Value` has arguments, including variable
:ada:`L`, leading to an attempt to access an array component via an
out-of-bounds index (:ada:`1 + 10 * 10 = 101`, beyond the last index of
:ada:`L`). This attempt will trigger an exception in :ada:`Value` prior
to actually accessing the array object's memory. Function :ada:`Value`
doesn't have any exception handlers so the exception is propagated up to
the caller :ada:`Some_Process`. Procedure :ada:`Some_Process` has an
exception handler for :ada:`Constraint_Error` and it so happens that
:ada:`Constraint_Error` is the exception raised in this case. As a
result, the code for that handler will be executed, printing some
messages on the screen. Then procedure :ada:`Some_Process` will return
to :ada:`Main` normally. :ada:`Main` then continues to execute normally
after the call to :ada:`Some_Process` and prints its completion message.

If procedure :ada:`Some_Process` had also not had a handler for
:ada:`Constraint_Error`, that procedure call would also have returned
abnormally and the exception would have been propagated further up the
call chain to procedure :ada:`Main`. Normal execution in :ada:`Main`
would likewise be abandoned in search of a handler. But :ada:`Main` does
not have any handlers so :ada:`Main` would have completed abnormally,
immediately, without printing its closing message.

This semantic model is the same as with many other programming languages,
in which the execution of a frame's sequence of statements is
unavoidably abandoned when an exception becomes active. The model is a
direct reaction to the use of status codes returned from functions as in
C, where it is all too easy to forget (intentionally or otherwise) to
check the status values returned. With the exception model errors cannot
be ignored.

However, full exception propagation as described above is not the norm
for embedded applications when the highest levels of integrity are
required. The run-time library code implementing exception propagation
can be rather complex and expensive to certify. Those problems apply to
the application code too, because exception propagation is a form of
control flow without any explicit construct in the source. Instead of
the full exception model, designers of high-integrity applications often
take alternative approaches.

One alternative consists of deactivating exceptions altogether, or more
precisely, deactivating language-defined checks, which means that the
compiler will not generate code checking for conditions giving rise to
exceptions. Of course, this makes the code vulnerable to attacks, such
as buffer overflow, unless otherwise verified (e.g. through static
analysis). Deactivation can be applied at the unit level, through the
``-gnatp`` compiler switch, or locally within a unit via the
pragma :ada:`Suppress`. (Refer to the
:gnat_ugn:`GNAT User’s Guide for Native Platforms <building_executable_programs_with_gnat>`
for more details about the switch.)

For example, we can write the following. Note the pragma on line 4 of
:file:`arrays.adb` within function :ada:`Value`:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Suppress

   package Arrays is

      type List is array (Natural range <>) of Integer;

      function Value (A : List; X, Y : Integer) return Integer;

   end Arrays;

   package body Arrays is

      function Value (A : List; X, Y : Integer) return Integer is
         pragma Suppress (All_Checks);
      begin
         return A (X + Y * 10);
      end Value;

   end Arrays;

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Suppress

   with Ada.Text_IO; use Ada.Text_IO;
   with Arrays;      use Arrays;

   procedure Some_Process is
      L : constant List (1 .. 100) := (others => 42);
   begin
      Put_Line (Integer'Image (Value (L, 1, 10)));
   exception
      when Constraint_Error =>
         Put_Line ("FAILURE");
   end Some_Process;


This placement of the pragma will only suppress checks in the function
body. However, that is where the exception would otherwise have been
raised, leading to incorrect and unpredictable execution. (Run the
program more than once. If it prints the right answer (42), or even the
same value each time, it's just a coincidence.) As you can see,
suppressing checks negates the guarantee of errors being detected and
addressed at run-time.

Another alternative is to leave checks enabled but not retain the
dynamic call-chain propagation. There are a couple of approaches
available in this alternative.

The first approach is for the run-time library to invoke a global "last
chance handler" (LCH) when any exception is raised. Instead of the
sequence of statements of an ordinary exception handler, the LCH is
actually a procedure intended to perform "last-wishes" before the
program terminates. No exception handlers are allowed. In this scheme
"propagation" is simply a direct call to the LCH procedure. The default LCH
implementation provided by GNAT does nothing other than loop infinitely.
Users may define their own replacement implementation.

The availability of this approach depends on the run-time library.
Typically, *Zero Footprint* and *Ravenscar SFP* run-times will provide
this mechanism because they are intended for certification.

A user-defined LCH handler can be provided either in C or in Ada, with
the following profiles:

[Ada]

.. code-block:: ada

    procedure Last_Chance_Handler (Source_Location : System.Address; Line : Integer);
    pragma Export (C,
                   Last_Chance_Handler,
                   "__gnat_last_chance_handler");

[C]

.. code-block:: ada

    void __gnat_last_chance_handler (char *source_location,
                                     int line);

We'll go into the details of the pragma :ada:`Export` in a further
section on language interfacing. For now, just know that the symbol
:c:`__gnat_last_chance_handler` is what the run-time uses to branch
immediately to the last-chance handler. Pragma :ada:`Export` associates
that symbol with this replacement procedure so it will be invoked
instead of the default routine. As a consequence, the actual procedure
name in Ada is immaterial.

Here is an example implementation that simply blinks an LED
forever on the target:

.. code-block:: ada

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      Next_Release : Time := Clock;
      Period       : constant Time_Span := Milliseconds (500);
   begin
      Initialize_LEDs;
      All_LEDs_Off;

      loop
         Toggle (LCH_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Last_Chance_Handler;

The :ada:`LCH_LED` is a constant referencing the LED used by the
last-chance handler, declared elsewhere. The infinite loop is necessary
because a last-chance handler must never return to the caller (hence the
term "last-chance"). The LED changes state every half-second.

Unlike the approach in which there is only the last-chance handler
routine, the other approach allows exception handlers, but in a
specific, restricted manner. Whenever an exception is raised, the only
handler that can apply is a matching handler located in the same frame
in which the exception is raised. Propagation in this context is simply
an immediate branch instruction issued by the compiler, going directly
to the matching handler's sequence of statements. If there is no
matching local handler the last chance handler is invoked. For example
consider the body of function :ada:`Value` in the body of package :ada:`Arrays`:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Return

   package Arrays is

      type List is array (Natural range <>) of Integer;

      function Value (A : List; X, Y : Integer) return Integer;

   end Arrays;

   package body Arrays is

      function Value (A : List; X, Y : Integer) return Integer is
      begin
         return A (X + Y * 10);
      exception
         when Constraint_Error =>
            return 0;
      end Value;

   end Arrays;

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Return

   with Ada.Text_IO; use Ada.Text_IO;
   with Arrays;      use Arrays;

   procedure Some_Process is
      L : constant List (1 .. 100) := (others => 42);
   begin
      Put_Line (Integer'Image (Value (L, 1, 10)));
   exception
      when Constraint_Error =>
         Put_Line ("FAILURE");
   end Some_Process;

In both procedure :ada:`Some_Process` and function :ada:`Value` we have
an exception handler for :ada:`Constraint_Error`. In this example the
exception is raised in :ada:`Value` because the index check fails there.
A local handler for that exception is present so the handler applies and
the function returns zero, normally. Because the call to the function
returns normally, the execution of :ada:`Some_Process` prints zero and
then completes normally.

Let's imagine, however, that function :ada:`Value` did *not* have a
handler for :ada:`Constraint_Error`. In the context of full exception
propagation, the function call would return to the caller, i.e.,
:ada:`Some_Process`, and would be handled in that procedure's handler.
But only local handlers are allowed under the second alternative so the
lack of a local handler in :ada:`Value` would result in the last-chance
handler being invoked. The handler for :ada:`Constraint_Error` in
:ada:`Some_Process` under this alternative approach.

So far we've only illustrated handling the :ada:`Constraint_Error`
exception. It's possible to handle other language-defined and
user-defined exceptions as well, of course. It is even possible to
define a single handler for all other exceptions that might be
encountered in the handled sequence of statements, beyond those
explicitly named. The "name" for this otherwise anonymous exception is
the Ada reserved word :ada:`others`. As in case statements, it covers
all other choices not explicitly mentioned, and so must come last. For
example:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Return_Others

    package Arrays is

       type List is array (Natural range <>) of Integer;

       function Value (A : List; X, Y : Integer) return Integer;

    end Arrays;

    package body Arrays is

       function Value (A : List; X, Y : Integer) return Integer is
       begin
          return A (X + Y * 10);
       exception
          when Constraint_Error =>
             return 0;
          when others =>
             return -1;
       end Value;

    end Arrays;

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Exception_Return_Others

    with Ada.Text_IO; use Ada.Text_IO;
    with Arrays;      use Arrays;

    procedure Some_Process is
       L : constant List (1 .. 100) := (others => 42);
    begin
       Put_Line (Integer'Image (Value (L, 1, 10)));
    exception
       when Constraint_Error =>
          Put_Line ("FAILURE");
    end Some_Process;

In the code above, the :ada:`Value` function has a handler specifically
for :ada:`Constraint_Error` as before, but also now has a handler for
all other exceptions. For any exception other than :ada:`Constraint_Error`,
function :ada:`Value` returns -1. If you remove the function's handler
for :ada:`Constraint_Error` (lines 7 and 8) then the other "anonymous"
handler will catch the exception and -1 will be returned instead of zero.

There are additional capabilities for exceptions, but for now you have a good
basic understanding of how exceptions work, especially their dynamic
nature at run-time.

.. _Ada_For_Embedded_C_Dev_Dynamic_Checks_Vs_Formal_Proof:

Understanding Dynamic Checks versus Formal Proof
------------------------------------------------

So far, we have discussed language-defined checks inserted by the
compiler for verification at run-time, leading to exceptions being
raised. We saw that these dynamic checks verified semantic conditions
ensuring proper execution, such as preventing writing past the end of a
buffer, or exceeding an application-specific integer range constraint,
and so on. These checks are defined by the language because they apply
generally and can be expressed in language-defined terms.

Developers can also define dynamic checks. These checks specify
component-specific or application-specific conditions, expressed in
terms defined by the component or application. We will refer to these
checks as "user-defined" for convenience. (Be sure you understand that
we are not talking about user-defined *exceptions* here.)

Like the language-defined checks, user-defined checks must be
true at run-time. All checks consist of Boolean conditions, which is why
we can refer to them as assertions: their conditions are asserted to be
true by the compiler or developer.

Assertions come in several forms, some relatively low-level,
such as a simple pragma :ada:`Assert`, and some high-level, such as type
invariants and contracts. These forms will be presented in detail in a
later section, but we will illustrate some of them here.

User-defined checks can be enabled at run-time in GNAT with the ``-gnata``
switch, as well as with pragma :ada:`Assertion_Policy`. The switch
enables all forms of these assertions, whereas the pragma can be used to
control specific forms. The switch is typically used but there are
reasonable use-cases in which some user-defined checks are enabled,
and others, although defined, are disabled.

By default in GNAT, language-defined checks are enabled but user-defined
checks are disabled. Here's an example of a simple program employing a
low-level assertion. We can use it to show the effects of the switches,
including the defaults:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Low_Level_Assertion

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is
      X : Positive := 10;
   begin
      X := X * 5;
      pragma Assert (X > 99);
      X := X - 99;
      Put_Line (Integer'Image (X));
   end Main;

If we compiled this code we would get a warning about the assignment on
line 8 after the pragma :ada:`Assert`, but not one about the
:ada:`Assert` itself on line 7.

::

   gprbuild -q -P main.gpr
   main.adb:8:11: warning: value not in range of type "Standard.Positive"
   main.adb:8:11: warning: "Constraint_Error" will be raised at run time

No code is generated for the user-defined check expressed via pragma
:ada:`Assert` but the language-defined check is emitted. In this case the range
constraint on :ada:`X` excludes zero and negative numbers, but :ada:`X *
5 = 50`, :ada:`X - 99 = -49`. As a result, the check for the last
assignment would fail, raising :ada:`Constraint_Error` when the program runs.
These results are the expected behavior for the default switch settings.

But now let's enable user-defined checks and build it. Different
compiler output will appear.

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Assert
   :class: ada-run-expect-failure

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is
      X : Positive := 10;
   begin
      X := X * 5;
      pragma Assert (X > 99);
      X := X - 99;
      Put_Line (Integer'Image (X));
   end Main;

Now we also get the compiler warning about the pragma :ada:`Assert` condition.
When run, the failure of pragma :ada:`Assert` on line 7 raises the exception
:ada:`Ada.Assertions.Assertion_Error`. According to the expression in the
assertion, :ada:`X` is expected (incorrectly) to be above 99 after the
multiplication. (The exception name in the error message,
SYSTEM.ASSERTIONS.ASSERT_FAILURE, is a GNAT-specific alias for
:ada:`Ada.Assertions.Assertion_Error`.)

It's interesting to see in the output that the compiler can detect some
violations at compile-time:

::

   main.adb:7:19: warning: assertion will fail at run time
   main.adb:7:21: warning: condition can only be True if invalid values present
   main.adb:8:11: warning: value not in range of type "Standard.Positive"

Generally speaking, a complete analysis is beyond the scope of compilers
and they may not find all errors prior to execution, even those we
might detect ourselves by inspection. More errors can be found by tools
dedicated to that purpose, known as static analyzers. But even an
automated static analysis tool cannot guarantee it will find all
potential problems.

A much more powerful alternative is formal proof, a form of static analysis
that can (when possible) give strong guarantees about the checks, for
all possible conditions and all possible inputs. Proof can be
applied to both language-defined and user-defined checks.

Be sure you understand that formal proof, as a form of static analysis,
verifies conditions prior to execution, even prior to compilation. That
earliness provides significant cost benefits. Removing bugs earlier is
far less expensive than doing so later because the cost to fix bugs
increases exponentially over the phases of the project life cycle,
especially after deployment. Preventing bug introduction into the
deployed system is the least expensive approach of all. Furthermore,
cost savings during the initial development will be possible as well,
for reasons specific to proof. We will revisit this topic later in
this section.

Formal analysis for proof can be achieved through the SPARK subset of
the Ada language combined with the :program:`gnatprove` verification
tool. SPARK is a subset encompassing most of the Ada language, except
for features that preclude proof. As a disclaimer, this course is not
aimed at providing a full introduction to proof and the SPARK language,
but rather to present in a few examples what it is about and what it can
do for us.

As it turns out, our procedure :ada:`Main` is already SPARK compliant so
we can start verifying it.

.. code:: ada prove_button run_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Assert
   :class: ada-run-expect-failure, ada-expect-prove-error

   with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       X : Positive := 10;
    begin
       X := X * 5;
       pragma Assert (X > 99);
       X := X - 99;
       Put_Line (Integer'Image (X));
    end Main;

The "Prove" button invokes :program:`gnatprove` on :file:`main.adb`. You
can ignore the parameters to the invocation. For the purpose of this
demonstration, the interesting output is this message:

::

    main.adb:7:19: medium: assertion might fail, cannot prove X > 99 (e.g. when X = 50)

:program:`gnatprove` can tell that the assertion :ada:`X > 99` may have
a problem. There's indeed a bug here, and :program:`gnatprove` even
gives us the counterexample (when :ada:`X` is 50). As a result the code
is not proven and we know we have an error to correct.

Notice that the message says the assertion "might fail" even though
clearly :program:`gnatprove` has an example for when failure is certain.
That wording is a reflection of the fact that SPARK gives strong
guarantees when the assertions are proven to hold, but does not
guarantee that flagged problems are indeed problems. In other words,
:program:`gnatprove` does not give false positives but false negatives
are possible. The result is that if :program:`gnatprove` does not
indicate a problem for the code under analysis we can be sure there is
no problem, but if :program:`gnatprove` does indicate a problem the tool
may be wrong.

Initialization and Correct Data Flow
------------------------------------

An immediate benefit from having our code compatible with the SPARK
subset is that we can ask :program:`gnatprove` to verify initialization
and correct data flow, as indicated by the absence of messages during
SPARK "flow analysis." Flow analysis detects programming errors such as
reading uninitialized data, problematic aliasing between formal
parameters, and data races between concurrent tasks.

In addition, :program:`gnatprove` checks unit specifications for the
actual data read or written, and the flow of information from inputs to
outputs. As you can imagine, this verification provides significant
benefits, and it can be reached with comparatively low cost.

For example, the following illustrates an initialization failure:

.. code:: ada prove_flow_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_0 main=main.adb
   :class: ada-expect-prove-error

   with Increment;
   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is
      B : Integer;
   begin
      Increment (B);
      Put_Line (B'Image);
   end Main;

   procedure Increment (Value : in out Integer) is
   begin
       Value := Value + 1;
   end Increment;

Granted, :ada:`Increment` is a silly procedure as-is, but imagine it did
useful things, and, as part of that, incremented the argument.
:program:`gnatprove` tells us that the caller has not assigned a value
to the argument passed to :ada:`Increment`.

Consider this next routine, which contains a serious coding error. Flow
analysis will find it for us.

.. code:: ada prove_flow_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_1
   :class: ada-expect-prove-error

   with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

   procedure Compute_Offset (K : Float; Z : out Integer; Flag : out Boolean) is
      X : constant Float := Sin (K);
   begin
      if X < 0.0 then
         Z := 0;
         Flag := True;
      elsif X > 0.0 then
         Z := 1;
         Flag := True;
      else
         Flag := False;
      end if;
   end Compute_Offset;

:program:`gnatprove` tells us that :ada:`Z` might not be initialized
(assigned a value) in :ada:`Compute_Offset`, and indeed that is correct.
:ada:`Z` is a mode :ada:`out` parameter so the routine should assign a
value to it: :ada:`Z` is an output, after all. The fact that
:ada:`Compute_Offset` does not do so is a significant and nasty bug. Why is it
so nasty? In this case, formal parameter :ada:`Z` is of the scalar type
:ada:`Integer`, and scalar parameters are always passed by copy in Ada
and SPARK. That means that, when returning to the caller, an integer
value is copied to the caller's argument passed to :ada:`Z`. But this
procedure doesn't always assign the value to be copied back, and in that
case an arbitrary value |mdash| whatever is on the stack |mdash| is
copied to the caller's argument. The poor programmer must debug the code
to find the problem, yet the effect could appear well downstream from
the call to :ada:`Compute_Offset`. That's not only painful, it is expensive.
Better to find the problem before we even compile the code.


Contract-Based Programming
--------------------------

So far, we've seen assertions in a routine's sequence of statements,
either through implicit language-defined checks (is the index in the
right range?) or explicit user-defined checks. These checks are already
useful by themselves but they have an important limitation: the assertions
are in the implementation, hidden from the callers of the routine. For
example, a call's success or failure may depend upon certain input
values but the caller doesn't have that information.

Generally speaking, Ada and SPARK put a lot of emphasis on strong,
complete specifications for the sake of abstraction and analysis.
Callers need not examine the implementations to determine
whether the arguments passed to it are changed, for example. It is
possible to go beyond that, however, to specify implementation
constraints and functional requirements. We use contracts to do so.

At the language level, contracts are higher-level forms of assertions
associated with specifications and declarations rather than sequences
of statements. Like other assertions they can be activated or
deactivated at run-time, and can be statically proven. We'll concentrate
here on two kinds of contracts, both associated especially (but not
exclusively) with procedures and functions:

- *Preconditions*, those Boolean conditions required to be true *prior* to a
  call of the corresponding subprogram

- *Postconditions*, those Boolean conditions required to be true *after* a
  call, as a result of the corresponding subprogram's execution

In particular, preconditions specify the initial conditions, if any,
required for the called routine to correctly execute. Postconditions, on
the other hand, specify what the called routine's execution must have
done, at least, on normal completion. Therefore, preconditions are obligations
on callers (referred to as "clients") and postconditions are obligations
on implementers. By the same token, preconditions are guarantees to the
implementers, and postconditions are guarantees to clients.

Contract-based programming, then, is the specification and rigorous
enforcement of these obligations and guarantees. Enforcement is rigorous
because it is not manual, but tool-based: dynamically at run-time with
exceptions, or, with SPARK, statically, prior to build.

Preconditions are specified via the "Pre" aspect. Postconditions are
specified via the "Post" aspect. Usually subprograms have separate
declarations and these aspects appear with those declarations, even
though they are *about* the bodies. Placement on the declarations allows
the obligations and guarantees to be visible to all parties. For
example:

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_2

    function Mid (X, Y : Integer) return Integer with
       Pre  => X + Y /= 0,
       Post => Mid'Result > X;

The precondition on line 2 specifies that, for any given call, the sum of the
values passed to parameters :ada:`X` and :ada:`Y` must not be zero.
(Perhaps we're dividing by :ada:`X + Y` in the body.) The declaration
also provides a guarantee about the function call's result, via the
postcondition on line 3: for any given call, the value returned will be greater
than the value passed to :ada:`X`.

Consider a client calling this function:

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_2 switches=Compiler(-gnato23);
    :class: ada-expect-prove-error

    with Mid;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Demo is
       A, B, C : Integer;
    begin
       A := Mid (1, 2);
       B := Mid (1, -1);
       C := Mid (A, B);
       Put_Line (C'Image);
    end Demo;

:program:`gnatprove` indicates that the assignment to :ada:`B` (line 8) might
fail because of the precondition, i.e., the sum of the inputs shouldn't
be 0, yet :ada:`-1 + 1 = 0`. (We will address the other output message
elsewhere.)

Let's change the argument passed to :ada:`Y` in the second call (line 8).
Instead of -1 we will pass -2:

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_3 switches=Compiler(-gnato23);

    with Mid;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Demo is
       A, B, C : Integer;
    begin
       A := Mid (1, 2);
       B := Mid (1, -2);
       C := Mid (A, B);
       Put_Line (C'Image);
    end Demo;

    function Mid (X, Y : Integer) return Integer with
       Pre  => X + Y /= 0,
       Post => Mid'Result > X;

The second call will no longer be flagged for the precondition. In
addition, :program:`gnatprove` will know from the postcondition that
:ada:`A` has to be greater than 1, as does :ada:`B`, because in both
calls 1 was passed to :ada:`X`. Therefore, :program:`gnatprove` can
deduce that the precondition will hold for the third call :ada:`C :=
Mid (A, B);` because the sum of two numbers greater than 1 will
never be zero.

Postconditions can also compare the state prior to a call with the state
after a call, using the :ada:`'Old` attribute. For example:

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_4

   procedure Increment (Value : in out Integer) with
     Pre  => Value < Integer'Last,
     Post => Value = Value'Old + 1;

   procedure Increment (Value : in out Integer) is
   begin
      Value := Value + 1;
   end Increment;

The postcondition specifies that, on return, the argument passed to the
parameter :ada:`Value` will be one greater than it was immediately prior
to the call (:ada:`Value'Old`).

Replacing Defensive Code
------------------------

One typical benefit of contract-based programming is the removal of
defensive code in subprogram implementations. For example, the `Push`
operation for a stack type would need to ensure that the given stack is
not already full. The body of the routine would first check that,
explicitly, and perhaps raise an exception or set a status code. With
preconditions we can make the requirement explicit and
:program:`gnatprove` will verify that the requirement holds at all call
sites.

This reduction has a number of advantages:

- The implementation is simpler, removing validation code that is often
  difficult to test, makes the code more complex and leads to behaviors that
  are difficult to define.

- The precondition documents the conditions under which it's correct to
  call the subprogram, moving from an implementer responsibility to mitigate
  invalid input to a user responsibility to fulfill the expected interface.

- Provides the means to verify that this interface is properly respected,
  through code review, dynamic checking at run-time, or formal static proof.

As an example, consider a procedure :ada:`Read` that returns a component
value from an array. Both the :ada:`Data` and :ada:`Index` are objects visible
to the procedure so they are not formal parameters.

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Defensive

   package P is

      type List is array (Integer range <>) of Character;

      Data  : List (1 .. 100);
      Index : Integer := Data'First;

      procedure Read (V : out Character);

   end P;

   package body P is

      procedure Read (V : out Character) is
      begin
         if Index not in Data'Range then
            V := Character'First;
            return;
         end if;

         V := Data (Index);
         Index := Index + 1;
      end Read;
   end P;

In addition to procedure :ada:`Read` we would also have a way to load
the array components in the first place, but we can ignore that for
the purpose of this discussion.

Procedure :ada:`Read` is responsible for reading an element of the array
and then incrementing the index. What should it do in case of an
invalid index? In this implementation there is defensive code that returns a
value arbitrarily chosen. We could also redesign the code to return a
status in this case, or |mdash| better |mdash| raise an exception.

An even more robust approach would be instead to ensure that this
subprogram is only called when :ada:`Index` is within the indexing
boundaries of :ada:`Data`. We can express that requirement with a
precondition (line 9).

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Defensive

   package P is

      type List is array (Integer range <>) of Character;

      Data : List (1 .. 100);
      Index : Integer := 1;

      procedure Read (V : out Character)
        with Pre => Index in Data'Range;

   end P;

   package body P is

      procedure Read (V : out Character) is
      begin
         V := Data (Index);
         Index := Index + 1;
      end Read;

   end P;

Now we don't need the defensive code in the procedure body. That's safe
because SPARK will attempt to prove statically that the check will not
fail at the point of each call.

Assuming that procedure :ada:`Read` is intended to be the only way to
get values from the array, in a real application (where the principles
of software engineering apply) we would take advantage of the
compile-time visibility controls that packages offer. Specifically, we
would move all the variables' declarations to the private part of the
package, or even the package body, so that client code could not
possibly access the array directly. Only procedure :ada:`Read` would
remain visible to clients, thus remaining the only means of accessing
the array. However, that change would entail others, and in this chapter
we are only concerned with introducing the capabilities of SPARK.
Therefore, we keep the examples as simple as possible.

Proving Absence of Run-Time Errors
----------------------------------

Earlier we said that :program:`gnatprove` will verify both
language-defined and user-defined checks. Proving that the
language-defined checks will not raise exceptions at run-time is known
as proving "Absence of Run-Time Errors" or AoRTE for short. Successful
proof of these checks is highly significant in itself.

One of the major resulting benefits is that we can deploy the final
executable with checks disabled. That has obvious performance benefits,
but it is also a safety issue. If we disable the checks we also disable
the run-time library support for them, but in that case the language
does not define what happens if indeed an exception is raised. Formally
speaking, anything could happen. We must have good reason for thinking
that exceptions cannot be raised.

This is such an important issue that proof of AoRTE can be used to comply
with the objectives of certification standards in various high-integrity
domains (for example, DO-178B/C in avionics, EN 50128 in railway, IEC
61508 in many safety-related industries, ECSS-Q-ST-80C in space, IEC
60880 in nuclear, IEC 62304 in medical, and ISO 26262 in automotive).

As a result, the quality of the program can be guaranteed to
achieve higher levels of integrity than would be possible in other
programming languages.

However, successful proof of AoRTE may require additional assertions,
especially preconditions. We can see that with procedure :ada:`Increment`, the
procedure that takes an Integer argument and increments it by one. But
of course, if the incoming value of the argument is the largest possible
positive value, the attempt to increment it would overflow, raising
:ada:`Constraint_Error`. (As you have likely already concluded,
:ada:`Constraint_Error` is the most common exception you will have to
deal with.) We added a precondition to allow only the integer values up to,
but not including, the largest positive value:

.. code:: ada prove_button project=Courses.Ada_For_Embedded_C_Dev.SPARK.Contracts_5

   procedure Increment (Value : in out Integer) with
     Pre  => Value < Integer'Last,
     Post => Value = Value'Old + 1;

   procedure Increment (Value : in out Integer) is
   begin
      Value := Value + 1;
   end Increment;

Prove it, then comment-out the precondition and try proving it again.
Not only will :program:`gnatprove` tell us what is wrong, it will
suggest a solution as well.

Without the precondition the check it provides would have to be
implemented as defensive code in the body. One or the other is critical
here, but note that we should never need both.

Proving Abstract Properties
---------------------------

The postcondition on :ada:`Increment` expresses what is, in fact, a unit-level
requirement. Successfully proving such requirements is another
significant robustness and cost benefit. Together with the proofs for
initialization and AoRTE, these proofs ensure program integrity, that
is, the program executes within safe boundaries: the control flow of the
program is correctly programmed and cannot be circumvented through
run-time errors, and data cannot be corrupted.

We can go even further. We can use contracts to express arbitrary
abstract properties when such exist. Safety and security properties, for
instance, could be expressed as postconditions and then proven by
:program:`gnatprove`.

For example, imagine we have a procedure to move a train to a new
position on the track, and we want to do so safely, without leading to a
collision with another train. Procedure :ada:`Move`, therefore, takes
two inputs: a train identifier specifying which train to move, and the
intended new position. The procedure's output is a value indicating a
motion command to be given to the train in order to go to that new
position. If the train cannot go to that new position safely the output
command is to stop the train. Otherwise the command is for the train to
continue at an indicated speed:

.. code-block:: ada

   type Move_Result is (Full_Speed, Slow_Down, Keep_Going, Stop);

   procedure Move
      (Train        : in  Train_Id;
       New_Position : in  Train_Position;
       Result       : out Move_Result)
   with
      Pre  => Valid_Id (Train) and
              Valid_Move (Trains (Train), New_Position) and
              At_Most_One_Train_Per_Track and
              Safe_Signaling,
      Post => At_Most_One_Train_Per_Track and
              Safe_Signaling;

   function At_Most_One_Train_Per_Track return Boolean;

   function Safe_Signaling return Boolean;

The preconditions specify that, given a safe initial state and a valid
move, the result of the call will also be a safe state: there will be at
most one train per track section and the track signaling system will not
allow any unsafe movements.


Final Comments
--------------

Make sure you understand that :program:`gnatprove` does not attempt to
prove the program correct as a whole. It attempts to prove
language-defined and user-defined assertions about parts of the program,
especially individual routines and calls to those routines. Furthermore,
:program:`gnatprove` proves the routines correct only to the extent that
the user-defined assertions correctly and sufficiently describe and
constrain the implementation of the corresponding routines.

Although we are not proving whole program correctness, as you will have
seen |mdash| and done |mdash| we can prove properties than make our
software far more robust and bug-free than is possible otherwise. But in
addition, consider what proving the unit-level requirements for your
procedures and functions would do for the cost of unit testing and
system integration. The tests would pass the first time.

However, within the scope of what SPARK can do, not everything can be
proven. In some cases that is because the software behavior is not
amenable to expression as boolean conditions (for example, a mouse
driver). In other cases the source code is beyond the capabilities of
the analyzers that actually do the mathematical proof. In these cases
the combination of proof and actual test is appropriate, and still less
expensive that testing alone.

There is, of course, much more to be said about what can be done with
SPARK and :program:`gnatprove`. Those topics are reserved for the
:doc:`Introduction to SPARK </courses/intro-to-spark/index>` course.
