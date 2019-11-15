Enhancing Verification with SPARK and Ada
============================================

Dealing or not Dealing with Exceptions
--------------------------------------

In Ada, incorrect execution of code isare often translated into what's called an exception, which interrupts the normal flow of execution. For example, accessing to an index out of the bound of an array will raise such an event. The general model for Ada exceptions is the same as of many other programming languages, where an, that is the exception is being propagated from call frame to call frame up until it either reaches the main suprogram (terminating the application) or is caught by an so called exception handler in one of the call frames. For example:

.. code-block:: ada

    function Read (A : Arr; X, Y : Integer) return Integer is
    begin
       return A (X + Y * 10);
    end Read;

.. code-block:: ada

    with Read;

    procedure Some_Process is
       A : Arr (1 .. 100);
    begin
       Put_Line (Integer'Image (Read (A, 1, 10)));
    exception
       when Constraint_Error =>
          Put_Line ("FAILURE");
    end;

.. code-block:: ada

    with Some_Process;

    procedure Main is
    begin
       Some_Process;
    end Main;

On the above code, Some_Process calls Read with values leading to an out-of-bound access (1 + 10 * 10 = 101, beyond A last index). This will trigger an exception in Read which will happen prior to reading the value in memory. Read doesn't have any code handling exceptions, so the exception is propagated to the caller Some_Process. Some process has an exception handled, introduced by the construction exception, which looks in particular for the exception of kind Constraint_Error. It happens so that Constraint_Error is the exception raise in case of array out of bound access, so the code of the handler will be executed (printing an error message on the screen) and the function Some_Process will then return normally.

Full exception propagation as described above are not the norm for embedded applications, in particular when high level of integrity are required. The code needed for exception propagation can be rather complex and difficult to certify, and having control flow jumping through the call stack creates situations with inconsistent data (for example, if it happens in the middle of an update). Not to mention of course the fact that checking for the failing conditions is taking processor time in the first place, which hinders application performances. Instead of the full exception model, embedded application usually resolve to alternative options. The following options are mostly GNAT-specific, as the Ada standard doesn't provide much guidance there.

The first one consists in deactivating exceptions altogether - or more precisely, deactivating checks, which will mean that the compiler will not generate verifications leading to exceptions in cases such as buffer overflow. Of course, this makes then the code vulnerable to attacks, unless otherwise verified (e.g. through static analysis). This can be done at the file level, through the -gnatp compiler switch, or locally to an area within the pragma Suppress. For example, we can write:

.. code-block:: ada

    function Read (A : Arr, X, Y : Integer) return Integer is
    begin
       pragma Suppress (All_Checks);
       return A (X + Y * 10);
    end Read;

This will only suppress checks on this function (leading to wrong code execution in our case).

If exceptions are kept, there are still alternate models to exception propagation. The first one is called the "last chance handler", which is a function called right before terminating an application in response of an exception. The presence of this mechanism depends on the run-time profile that the toolchain is configured with. Typically, Zero Footprint and Ravenscar SFP run-times will use this mechanism.

This handler can be provided either in C or in Ada, with the following profiles:

[Ada]

.. code-block:: ada

    procedure Last_Chance_Handler
      (Source_Location : System.Address; Line : Integer);
    pragma Export (C, Last_Chance_Handler,
                   "__gnat_last_chance_handler");

[C]

.. code-block:: ada

    void __gnat_last_chance_handler (char *source_location,
                                     int line);

We'll go into the details of the prama Export in a further section on language interfacing.

Last, even without exception propagation, exceptions can still be handled locally, that is in the same scope as the one raising the exception. This allows the compiler to translate the exception handling through a simple goto statement. For example:

.. code-block:: ada

    function Read (A : Arr, X, Y : Integer) return Integer is
    begin
       return A (X + Y * 10);
    exception
       when Contraint_Error =>
          return 0;
    end Read;

Note that so far, we've only reacted to Constraint_Error exception. It's possible to react to different exceptions, and even to have one handler for all those that would not have been caught before:

.. code-block:: ada

    function Read (A : Arr, X, Y : Integer) return Integer is
    begin
       return A (X + Y * 10);
    exception
       when Contraint_Error =>
          return 0;
       when others =>
          Return -1;
    end Read;

Understanding Dynamic Checks v.s. Formal Proof
----------------------------------------------

So far, we discussed dynamic checks automatically inserted by the compiler, and we saw that these can be deactivated using either then -gnatp compiler switch or pragma Supress. It's also possible to add user checks through the means of assertions, assumptions and contracts. These will be presented in a later section. These can be enabled at run-time with -gnata. Here's an example of a simple program and the result of the compilation on under various set of switches:

::

    with GNAT.IO; use GNAT.IO;

    procedure Main is
       X : Positive;
    begin
       X := X * 5;
       pragma Assert (X > 99);
       X := X - 99;
       Put_Line (Integer'Image (X));
    end Main;

::

    gprbuild main.adb
    main
    raised CONSTRAINT_ERROR : main.adb:8 range check failed

The above call is the default behavior. The assertion isn't checked, but the type constraints are. X * 5 = 50, X - 99  = -49, which is outside of positive boundaries. As a result, the check on the last assignment fails.

::

    gprbuild -gnata hello_world.adb
    main
    raised SYSTEM.ASSERTIONS.ASSERT_FAILURE : main.adb:7

On the above example with the -gnata switch, I'm asking to enable manual checks, here the assertion. As a result, the code fails one line above, on the check requesting that X is supposed to be above 99.

::

    gprbuild -gnatp hello_world.adb
    main
    -49

On this last example, we removed automatic check with -gnatp switch. The program will run without verification, and here provide a result that is inconsistent, -49, on data supposed to be positive.

It's interesting to see in the above example that code inspection could have pointed out that there would always be a problem. An alternative to code inspection is to use formal proof, which will find all places in the code that are not 100% safe. This can be done through the SPARK subset of the Ada language and the gnatprove tool.

SPARK is a subset encompassing most of the Ada language, but for features that prevent the proof. As a disclaimer, this booklet is not aimed at providing a full introduction to the SPARK language and proof, but to present in a few examples what it is about.

As it turns out, our code is already SPARK compliant. We just need to enable SPARK checks with the SPARK mode aspect:

.. code-block:: ada

    with GNAT.IO; use GNAT.IO;

    procedure Main
      with SPARK_Mode => ON
    is
       X : Positive;
    begin
       X := X * 5;
       pragma Assert (X > 99);
       X := X - 99;
       Put_Line (Integer'Image (X));
    end Main;

The gnatprove tool will need a GNAT project file to run. We haven't gone through project files yet, but for the purpose of the example, you can create a simple prj.gpr file with this content:

::

    project Prj
    end project;

Then run gnatprove on main.adb:

::

    gnatprove -Pprj hello_world.adb
    main.adb:9:19: medium: assertion might fail, cannot prove X > 99 (e.g. when X = 50)

For the purpose of this demonstration, you can disregard other warnings that gnatprove may raise, which relate to data flow analysis. The interesting bit is the above message: gnatprove can tell that the assertion pragma Assert (X > 99);. This tells us two things. First, this assertion may fail, so the code is not proven. There's indeed a bug here. Second, perhaps even more interesting, there is no warning on the assignment on the following line anymore. This means that under this assertion, the next line will never fail. You can try removing the assertion, and see that now the assignment is not protected anymore and will fail.

In terms of performances, interestingly, checks that prove don't need to be ran at run-time anymore, as they are guaranteed to always succeed.

Programming by Contracts
------------------------

Up until now, we discussed check happening in the control flow of a program, either through implicit checks (is the index in the rights range?) or explicit assertions. This is already useful by itself, but has an important limitation: these checks are hidden from the user of the function. In particular, if their success or failure is linked to certain input values, the user of a subprogram doesn't have information on how to use the right values.

Generally speaking, Ada and SPARK put a lot of emphasis on extending the specification, and providing means to verify that the implementation matches the specification. Beyond structural information, it's possible to specify constraints and behavior at specification through the means of contracts, which are essentially boolean expressions to be checked at certain point of the program. As for regular assertions, those can be activated or deactivated at run-time, as well as formally proven. We'll concentrate here on three kind of contracts:
Preconditions, which are boolean expressions that have to be true prior to a call
Postconditions, which are boolean expressions that have to be true after a call
Predicates, which are boolean expressions that have to be true for values of a given type

These contracts and be build after the values of function, parameters, global data or references to the current instance. For example:

.. code-block:: ada

    function Compute (X, Y : Integer) return Integer
      with Pre => X + Y /= 0,
           Post => Compute'Result > X;

This function has a precondition, establishing a relationship between X and Y. Their sum has to be different than 0, maybe because we're dividing by X + Y in Compute. It also provides some guarantees on the result, the fact that that it has to be different than 0. If we look at a piece of code using these:

.. code-block:: ada

    A := Compute (1, 2);
    B := Compute (1, -1);
    C := Compute (A, B);

From a dynamic analysis perspective, the second statement will fail with an assertion failure, as -1 + 1 = 0. This will happen prior to calling any of the code within Compute. If we fix the code, for example:

.. code-block:: ada

    A := Compute (1, 2);
    B := Compute (1, -2);
    C := Compute (A, B);

Gnatprove will know from compute postcondition that A has to be above 1, and so does B. It therefore can deduce that the call C := Compute (A, B); is compliant with regards to its precondition, as the addition of two numbers above 1 will be different than 0.

Postconditions can also compare the state prior to a call with the state after a call, using the ‘Old attribute. For example:

.. code-block:: ada

    Counter : Integer := 0;

    procedure Increment_Counter
       with Post => Counter = Counter'Old + 1;

This will check that the counter has indeed been incremented after the call.

Contract can also be associated with types through predicates, with will add verifications on its values. For example:

.. code-block:: ada

    type R is record
       Initialized : Boolean := False;
       A, B : Integer;
    end record
       with Predicate => (if R.Initialized then R.A /= R.B);

Will verify that if the value of Initialize is true, then A and and B have to be different. Note that verifications are inserted at certain points in the program, such as parameter passing,  assignments, conversion or qualification. But not on expression computation. Also, the predicate of a type will not be checked when modifying its subcomponents (which allows to do an update in steps). This can lead to some delayed detection that can be surprising at first glance. For example:

.. code-block:: ada

    declare
       V : R;
    begin
       V.Initialized := True;
       V.A := 0;
       V.B := 0; -- The predicate is not correct but not verified.
       Some_Call (V) -- The predicate will be checked and raise an error

Absence of Run-Time Errors v.s.Functional Correctness
-----------------------------------------------------

A proof is a way to verify an implementation against a specification. Often, the main challenge resides not in doing the proof itself, but in the ability to formally express a requirements. This may require skills and experience beyond what's provided through typical software engineering curriculum. In this regards, although possible replacing entirely functional verification (e.g. functional test) by proof is a hard problem that shouldn't be considered prior to having more experience with the technology.

There's however a specification implicit in an Ada problem which is much easier to work with - the specification that an Ada exception should not raise any exception. This is also known as proof of absence of run-time error. In short, every situation that may raise an exception in Ada will generate a proof obligation, and they are many: index range check in an array, range check, division by zero, integer overflows…

Proving absence of run-time error may lead to code adaptations, in particular addition of pre and postconditions specifying domain of variables, addition of assertion, reduction of ranges to variables, etc. But these contracts and constraints should remain relatively simple. This can also be done bottom-up. The contracts are designed following constraints of the implemented code.

In contrast, functional properties are typically a top down approach, where requirements are first specified, translated into contract, then refined together with the code to reach verification.

To be clear, contracts written to help proof of absence of run-time errors and contracts that correspond to requirements are not different in nature. They're both represented as boolean expressions specified around subprograms. Their difference lies in their origin (the code for run-time errors, the requirements for functional contracts) and often in their complexity, functional contracts being usually more difficult to write.

Even when targeting functional correctness, it's a perfectly reasonable approach to only define and prove a subset of the requirements in SPARK, and demonstrate other through other means, in particular testing. This is a pragmatic approach that provides good results in terms of return on investment and can be more easily adopted by traditional software development team.

Replacing Defensive Code
------------------------

One typical usage of contract-base programming is to lower the amount of defensive code in a subprogram implementation to upgrade it to its specification. This has a number of advantages:
The implementation is simpler, removing validation code that is often difficult to test, makes the code more complex and leads to behaviors that are difficult to define.
Document conditions under which it's legal to call a subprogram, moving from an implementer responsibility to implement mitigation of invalid input to a user responsibility to fulfil the expected interface
Provides means to verify that this interface is properly respected, through code review, dynamic checking at run-time or formal proof.

To take an example, let's consider a subprogram that manipulates an array, expecting an index of this array as input:

.. code-block:: ada

    type Arr is array (Integer range <>) of Character;

    Data : Arr (1 .. 100);
    Index : Integer := 1;

    procedure Read (V : out Value);

    procedure Read (V : out Value) is
    begin
       if Index not in Data'Range then
          V := Character'First;
          return;
       end if;

       V := Data (Index);
       Index := Index + l;
    end Read ;

This procedure is responsible for reading an element on an array and then incrementing the read index. What should it do in case of an invalid index? In this case, there is some defensive code which return an arbitrary value (which could also be a valid value in the array). We could also complexify the code and return a status in this case, or raise an exception.

A more efficient way of working would be instead to make sure that this subprogram cannot be called if Index is out of the boundaries of data:

.. code-block:: ada

    procedure Read (V : out Value)
       with Pre => Index in Data'Range;

    procedure Read (V : out Value) is
    begin
       V := Data (Index);
       Index := Index + l;
    end Read;

At the point of call, the compiler can insert a dynamic check (with assertion enabled) or SPARK can prove that such check will never fail.

Ghost Code
----------

Adding contracts and assertions in a piece of code can be viewed as embedded the implementation and verification on the same program. It may be useful to designate some entities are being solely written for the purpose of the verification, and to be stripped from the program if said verification has to be removed, for example for final deployment.

Most Ada entities can be declared with ghost mode, for example types, subprograms and variables. A ghost entity cannot be used to produce non-ghost results for the program. However, ghost code can be interlaced with non ghost code as long as the ghost computation doesn't influence the non ghost one.

As an example, let's consider a re-entrant lock that we want to make sure will be properly freed. You can write something like:

.. code-block:: ada

    Lock_Depth : Integer := 0 with Ghost;

    procedure P (X : out Integer)
    with Post => Lock_Depth = Lock_Depth'Old;

    procedure Lock
    with Post => Lock_Depth = Lock_Depth'Old + 1;

    procedure Unlock
    with Post => Lock_Depth = Lock_Depth'Old - 1;

On the above, Lock and Unlock expect increment and decrement of the depth of the lock. We expect P to return the Lock in the initial state, so to balance Lock and Unlock calls. The implementation may look like:

.. code-block:: ada

    procedure P (X : out Integer) is
    begin
      Lock;
      X := Some_Call;
      Unlock;
    end P;

    procedure Lock is
    begin
       Lock_Depth:= Lock_Depth+ 1;
       Take_The_Lock;
    end Lock;

    procedure Unlock is
    Begin
       Lock_Depth:= Lock_Depth- 1;
       Release_The_Lock;
    end Unlock;

Note that it's fine to compute new values for the variable Lock_Depth in Lock and Unlock. These will be stripped automatically from the program when assertion are off. However, the following would be illegal:

.. code-block:: ada

    procedure P (X : out Integer) is
    begin
      Lock;
      X := Lock_Depth;
      Unlock;
    end P;

X is a normal variable of the application and thus can't be influenced by Lock_Depth.

Understanding proof locality
----------------------------

One of the difficulty of using prover lies in the fact that in order to prove code, one need to have some understanding on how the prover underneath operates. In particular, in the case of SPARK, the key property to understand about the proof is that this proof is local. Proving a subprogram means proving that a postcondition is true given that the precondition is true, verifying that all the preconditions of the called subprograms are verified and assuming that all the postconditions of such subprograms are also true. Let's take the following example:

.. code-block:: ada

    function Add_Abs (V1, V2 : Integer) return Integer
       with Post => Add'Result >= 0;

    function My_Abs (I : Integer);

    function Add_Abs (V1, V2 : Integer) return Integer is
    begin
       Return My_Abs (V1) + My_Abs (V2);
    end Add_Abs;

    function My_Abs (I : Integer) is
    begin
       if I < 0 then
          return -I;
       else
          return I;
       end if;
    end My_Abs;

From the above code, it's quite clear that the result of Add_Abs will indeed return a positive result, as the function is calling My_Abs on both values which returns a positive number. However, SPARK doesn't look at the body of My_Abs when analyzing Add_Abs. As a matter of fact, this body could be written in SPARK, Ada, C, or even not yet provided and here just as a placeholder. SPARK is only considering the specification of My_Abs, which is not providing much information at this stage - so the code will not prove. To be able to reach proof, you may complete the specification of My_Abs by stating that the result is always above zero:

.. code-block:: ada

    function My_Abs (I : Integer)
      with My_Abs'Result > 0;

The proof locality has two very interesting properties: first of all, it makes the proof scalable. You can concentrate on proving a subprogram without knowing the whole program, which allows separate developers to work separately, and ensure that the proof effort for the whole programs increases linearly with size. The second property is that it is possible to prove a subprogram depending on non-proven code. Let's for example assume that the above My_Abs is implemented by some super efficient assembly function. SPARK will have no way of proving it, and some other way need to be used to demonstrate that the implementation of My_Abs fulfills its specification - such as test or review. However, even in that case, Add_Abs can be proven correct, under the assumption that My_Abs operates as specified.

Understanding proofs on loops and containers
--------------------------------------------

Proving properties on containers and ensembles is one of the most powerful feature of SPARK, but may be daunting at start as it requires the understanding of two constructions mostly exclusive to proofs: quantifiers and loop invariants.

A quantifier is a way to express that a property has to be true for all element of a container - or for at least an element of a container. It can  be written according to the indices/cursors or actual values. Here are a few examples based on an array, checking whether all values are positive - or if at least one value is positive:

.. code-block:: ada

    type Arr is array (Integer range <>) of Integer;

    function All_Is_Positive_1 (V : Arr) return Boolean is
       (for all I in V'Range => V (I) > 0);

    function All_Is_Positive_1 (V : Arr) return Boolean is
       (for all E of V => E > 0);

    function At_Least_One_Is_Positive_1 (V : Arr) return Boolean is
       (for some I in V'Range => V (I) > 0);

    function At_Least_One_Is_Positive_2 (V : Arr) return Boolean is
       (for some E of V => E > 0);

Note the qualifier all to verify that a property is true for all element, some to check that it's true for at least one, and the difference between in when iterating over indices of the container and of to iterate over the actual values.

Using the above, we could write more complex properties. Here's for example a function that places the minimal value in the front of an array:

.. code-block:: ada

    type Arr is array (Integer range <>) of Integer;

    procedure Put_Min_In_Front (V : in out Arr)
       with Post =>
          (for all I in V'First .. V'Last => V (V'First) <= V (I));

The post condition could read "either the array size is 1 or less, or the element at position V'First is is lower or equal to the next. For sake of simplicity, we're not proving that all of the elements in the initial array are still in the final array.

The code that implements such procedure is quite trivial:

.. code-block:: ada

    procedure Put_Min_In_Front (V : in out Arr)
    is
       Tmp : Integer;
    begin
       for I in V'First .. V'Last loop
          if V(V'First) > V (I) then
             Tmp := V (I);
             V (I) := V (V'First);
             V(V'First) := Tmp;
          end if;
       end loop;
    end Put_Min_In_Front;

As you might imagine, the purpose of this chapter is because this can't be proven as-is. This is because it's extremely hard for provers to understand what happening inside loops. To be able to process things, they need to cut the loop as only to consider pieces of sequential code. There are three sequences to consider - from initial conditions to first iteration, from iteration n to iteration n + 1 and from final iteration to final conditions. This specific point where things are cut is called a loop invariant. On the second and third path, the prover doesn't know anything beyond what's in the invariant - which is an assertion to be true at every iteration, helping to prove the postcondition. In the example above, this would look like:

From start to first iteration:

.. code-block:: ada

    I := V'First

    if V(V'First) > V (I) then
       Tmp := V (I);
       V (I) := V (V'First);
       V(V'First) := Tmp;
    end if;

    pragma Loop_Invariant (<some invariant>);

From one loop to the next

.. code-block:: ada

    pragma Loop_Invariant (<some invariant>);

    I := I + 1;

    pragma Assume (I in V'First .. V'Last);

    if V(V'First) > V (I) then
       Tmp := V (I);
       V (I) := V (V'First);
       V(V'First) := Tmp;
    end if;

    pragma Loop_Invariant (<some invariant>);

And at the last loop:

.. code-block:: ada

    pragma Loop_Invariant (<some invariant>);
    pragma Assume (I = V'Last);

    Pragma Assert ((for all I in V'First .. V'Last => V (V'First) <= V (I));

The trick is now to define the invariant that will allow to prove all three pieces and be verified from one iteration to the next. Here, we want to say that at each iteration, all the elements until the current iterated index I are greater to the first element. The overall code then looks like:

.. code-block:: ada

    procedure Put_Min_In_Front (V : in out Arr)
    is
       Tmp : Integer;
    begin
       for I in V'First .. V'Last loop
          if V(V'First) > V (I) then
             Tmp := V (I);
             V (I) := V (V'First);
             V(V'First) := Tmp;
          end if;

          pragma Loop_Invariant
             ((for all K in V'First .. I => V (V'First) <= V (K)));
       end loop;
    end Put_Min_In_Front;

You can replace this invariant in all three path above to get convinced that this is indeed true.

Specifying what changes and what doesn't
----------------------------------------

When a subprogram is manipulating data structures, it's often as important to specify what changes than what doesn't change. Let's take the example of a simple procedure manipulating a record representing registers:

.. code-block:: ada

    type Arr is array (Integer range <>) of Integer;

    type Stack is record
       Data : Arr;
       Top : Integer;
    end Stack;

    procedure Push_Sum (S : in out Stack)
       with Post => S.Top := S.Top'Old + 1 and
                    S.Data(S.Top) = S.Data(S.Top - 1)
                                    + S.Data(S.Top - 2);

    procedure Push_Sum (S : in out Stack)
    is
    begin
      S.Data (S.Top + 1) := S.Data (S.Top) + S.Data (S.Top - 1);
      S.Top := S.Top + 1;
    end Push_Sum;

Now for the purpose of this example, we're trying to ensure that the numbers of this stack are always positive. We're doing some process on the stack - some sums - and want to make sure that these numbers are still positive:

.. code-block:: ada

    procedure Process (S : Stack)
       with Pre => (for all E of S.Data (S.Data'First .. S.Top) => E > 0)
            Post => (for all E of S.Data (S.Data'First .. S.Top) => E > 0);

    procedure Process (S : Stack) is
    begin
       Push_Sum (S);
       Push_Sum (S);
    end Process;

The expectation is that the post condition should be proven (not taking into account overflows). We have a list of positive numbers and push another positive number on top of it. However, it doesn't - the reason being that the postcondition of Top doesn't say anything about the other values in the array. They may have been modified, they may be the same.

To fix this, one possibility is to use the Update attribute, which specifies that only specific components of a record or an array are changed, all others remaining the same:

.. code-block:: ada

    procedure Push_Sum (S : in out Stack)
       with Post => S.Top := S.Top'Old + 1 and
                    S.Data'Update (Top => S.Data(S.Top - 1) + S.Data(S.Top - 2));

With the above changes, the code should prove fine.

Data Flow and Control Flow
--------------------------

.. todo::

    Complete section!
    (TODO + problems with value following)


Ownership and access types
--------------------------

.. todo::

    Complete section!

Still using exceptions in SPARK
-------------------------------

At the time of writing, exception handlers are not supported in SPARK. They introduce difficulties in terms of control flow that make it difficult to prove a piece of code in the case of an jump between an arbitrary location in the code and a handler. However, there's a way to work around this issue by wrapping a non-proven subprograms with the handler within a proven function. This can be quite handy in particular if SPARK is calling Ada function which may be raising exceptions, or to use exceptions as a mean to manage rare events. Of course, extra care must be given on these as no formal analysis is done.

Both the proven and the unproven subprograms have SPARK interface, but the unproven subprograms deactivates SPARK in the body.

Here's how it may look like:

.. code-block:: ada

    package Processes
       with SPARK_Mode => On
    is
       procedure Process
          with Pre => <some precondition here>,
               Post => <some postcondition here>;
    end Processes;

    package body Processes is

       procedure Process_Proven
          with Pre => <same precondition as process>,
               Post => <same postcondition as process>;

       procedure Process_Proven is
       begin
          <proven code>
       end Process_Proven;

       procedure Process
          with SPARK_Mode => Off
       is
       begin
         Process_Proven;
       exception
          when others =>
             <handler code>
       end Process;
    end Processes;

In the non-SPARK body, it's very important to make sure that the postcondition still holds. This is something that needs to be verified manually. In many cases, it's not possible, in which case the best solution is to re-raise an exception at the end of the handler in order to avoid getting the control flow back to regular SPARK code (which would otherwise render the prove void).

Understanding proof consistency
-------------------------------

One of the first difficulties in developing proven code is to ensure that the contract is correct in the first place - in other words, that it correctly represents the intent of the developer. As a result, while allowing to remove a lot of effort traditionally allocated to code review, contract-based programming and SPARK emphasis contract review.

One element that is critical to understand is that it the premise of a proof is false, then the conclusion is always true - or doesn't matter. For example in the following code seems to be proving that 0 = 1:

.. code-block:: ada

    procedure Read (V : out Positive)
       with Post => (if V < 0 then 1 = 0);

The postcondition of this code will always be proven, although it doesn't make much sense. The logic reason is that since V is positive, V cannot be below zero, thus anything under the V < 0 premise doesn't matter. One way to get further convinced of the above is to actually execute the code with assertion enabled. There's no way to make this postcondition fail.

Beyond this simple example, the consequence of these situation may be that the developer may think he manage to prove a piece of code while there's a fundamental contradiction in what had to be proven in the first place. The above is one of a short list of inconsistencies that can fortunately be detected automatically. Because the detection takes computing time, it's not enabled by default. The gnatprove switch --proof-warnings will enable them, and is highly advised when developing the contracts or during final validation.

Why isn't my code proving?
--------------------------

There are various reasons why a piece of code does not prove. The obvious one may be that the code is incorrect with regards to the specification, or that the specification itself is incorrect. This can also come from the prover inability to prove a rule too complex to demonstrate. This section provides some hints as to how to diagnose problems and work towards fixes.

First, unproven assertion sometimes comes with counter-examples. This means that not only the assertion was not proven, but the prover can demonstrate a specific case that will lead to an error. Looking at the specific values will help identifying which conditions lead to problematic paths. Clicking on the icon next to the warning message also highlights a path where these values are computed and lead to failures.

If no counterexample is provided, this may mean that the proof is too complex. To complete, the proof may need additional time or to employ additional provers. This can be controlled by the --level switch which can be set from the GPS interface.

Another way to investigate proof failures is to actually test and debug the code with assertions enabled (with the -gnata switch during compilation). This only works if the proofs can be executed in a reasonable time. Looking for ways to make the pre and post conditions fail is a good way to identify manually counter example.

Finally, adding assertions in the code through pragma Assert is a good way to do "static debugging". The basic idea is to introduce conditions in the implementation that you believe are true prior to the assertion or contract to be proven. This can help identifying from which point in the program things turn not to be correct, a bit like when stepping in a program. Let's illustrate this last technique through an example. Consider the following piece of code:

.. code-block:: ada

    type Direction is (North, South, West, East, Center);

    procedure Move
       (X, Y : in out Integer; Max, Min : Integer; D : Direction)
     with Pre => Max > Min and X in Min .. Max and Y in Min .. Max,
     Post => (if D in North | South then X = X'Old else Y = Y'Old);

    procedure Move
       (X, Y : in out Integer; Max, Min : Integer; D : Direction)
    is
    begin
       XInit := X;
       YInit := Y;

       if D = North and then Y < Max then
          Y := Y + 1;
       elsif D = South and then Y > Min  then
          Y := Y - 1;
       elsif D = West and then X > Min then
          X := X - 1;
       elsif X < Max then
          X := X + 1;
       end if;
    end Move;

This function Move moves X and Y according to a direction, and ensure that North and South don't move X, and that West and East don't move Y. Granted, this is a simplistic example with a very partial contract, and SPARK should be able to exhibit a counterexample showing values that are problematic. But let's assume it doesn't. One way to investigate postcondition failures could be to replicate the post condition as assumption to each of the branches and see which one is problematic. In this very case, as the postcondition is written according to the initial state of the call, we also need to store the initial values as local variables, which we're going to mark as Ghost since this is only going to be used for the purpose of the verification. This would then look like:

.. code-block:: ada

    procedure Move
       (X, Y : in out Integer; Max, Min : Integer; D : Direction)
    is
       XInit, YInit : Integer with Ghost;
    begin
       XInit := X;
       YInit := Y;

       if D = North and then Y < Max then
          Y := Y + 1;
          pragma Assert
            ((if D in North | South then X = XInit else Y = YInit));
       elsif D = South and then Y > Min  then
          Y := Y - 1;
          pragma Assert
            ((if D in North | South then X = XInit else Y = YInit));
       elsif D = West and then X > Min then
          X := X - 1;
          pragma Assert
            ((if D in North | South then X = XInit else Y = YInit));
       elsif X < Max then
          X := X + 1;
          pragma Assert
            ((if D in North | South then X = XInit else Y = YInit));
       end if;
    end Move;

Running this to the prover, you'll notice that only the last assertion doesn't prove, which indicates that this is the only incorrect branch. Indeed, a case where D = South and Y <= Min would end up in this branch. The code can be fixed with:

.. code-block:: ada

    procedure Move
       (X, Y : in out Integer; Max, Min : Integer; D : Direction)
    is
    begin
       XInit := X;
       YInit := Y;

       if D = North and then Y < Max then
          Y := Y + 1;
       elsif D = South and then Y > Min  then
          Y := Y - 1;
       elsif D = West and then X > Min then
          X := X - 1;
       elsif D = East and then X < Max then
          X := X + 1;
       end if;
    end Move;

RavenSPARK
----------

.. todo::

    Complete section!


Physical Dimension Checking
---------------------------

.. todo::

    Complete section!
