Lesson 3: Proof of Program Integrity
=====================================================================

.. role:: ada(code)
   :language: ada

Welcome to the second lecture on using GNATprove to perform program analysis. This lecture presents the second type of analysis which is use to perform proof of program integrity. For its soundness, this analysis relies on the prerequisite of having performed flow analysis and a user should not proceed further if there are still unanswered flow analysis messages for their program. The primary objective of performing proof of program integrity is to ensure the absence of any runtime error during the program’s execution.


Run-Time Errors
---------------------------------------------------------------------

There is always the potential for errors which can occur at program’s execution but will not be detected during compilation. These errors, called runtime errors, are those targeted by GNATprove’s analysis capabilities. There are various kinds of runtime errors, including array out of range access, subtype range violation, overflows in computations, and division by zero being the most common. Taking a look at the code on this slide to give an example, let us look at a simple assignment statement setting the value of the *I* + *J* th cell of an array of naturals *A* to *P* / *Q*.

.. code:: ada

   type Nat_Array is array (Integer range <>) of Natural;

   A : Nat_Array (1 .. 10);
   I, J, P, Q : Integer;

   A (I + J) := P / Q;

If we don’t know the values of *I*, *J*, *P*, and *Q*, then there is a question of what are the errors which may occur when executing this code. In fact, there are quite an important number of them.

First, the computation of *I* + *J* may overflow, for example if *I* is :ada:`Integer’Last` and *J* is positive.

.. code:: ada

   A (I + J) := P / Q;

.. code:: ada

   A (Integer’Last + 1) := P / Q;

Then, its result may not be in the range of the array *A*.

.. code:: ada

   A (A’Last + 1) := P / Q;

On the other side of the assignment, the division may also overflow, but only in the very special case where *P* is :ada:`Integer’First` and *Q* is -1 because of the asymmetric range of signed integer types.

.. code:: ada

   A (I + J) := Integer’First / -1;

As the array contains natural numbers, it is also an error to store a negative value in it.

.. code:: ada

   A (I + J) := 1 / -1;

Finally, the division is not allowed if *Q* is 0.

.. code:: ada

   A (I + J) := P / 0;

For all those runtime errors, the compiler will generate checks in the executable code to make sure that no inconsistent state can be reached, raising an exception if those checks fail. You can see the type of exceptions raised due to failed checks for each of the different assignment statements below:

.. code:: ada

   A (Integer’Last + 1) := P / Q;
   --  raised CONSTRAINT_ERROR : overflow check failed

   A (A’Last + 1) := P / Q;
   --  raised CONSTRAINT_ERROR : index check failed

   A (I + J) := Integer’First / (-1);
   --  raised CONSTRAINT_ERROR : overflow check failed

   A (I + J) := 1 / (-1);
   --  raised CONSTRAINT_ERROR : range check failed

   A (I + J) := P / 0;
   --  raised CONSTRAINT_ERROR : divide by zero

Note that these runtime checks are costly, both in terms of program size and execution time. They do not come at zero cost and therefore, depending on the context, it may be appropriate to remove them if we can statically ensure that they can never be needed at runtime.

This is where analysis using GNATprove can be used to demonstrate statically that none of these errors will ever occur at runtime. More precisely, GNATprove logically interprets the meaning of every instruction in the program. Using this interpretation, GNATprove generates a logical formula and named verification condition for each possible check that implies the validity of the code.

.. code:: ada

   A (Integer’Last + 1) := P / Q;
   medium: overflow check might fail

   A (A’Last + 1) := P / Q;
   medium: array index check might fail

   A (I + J) := Integer’First / (-1);
   medium: overflow check might fail

   A (I + J) := 1 / (-1);
   medium: range check might fail

   A (I + J) := P / 0;
   medium: divide by zero might fail

The verification conditions will then be given to an automatic prover. If every verification condition generated for a program can be validated by a prover, it means that no error will ever be raised at runtime when executing this program.


Modularity
---------------------------------------------------------------------

For scalability reasons, GNATprove performs proof of program modularly on a per subprogram basis. To do this, it relies on pre and postconditions to properly summarize the input and output state of each subprogram. More precisely, when verifying the body of a subprogram, GNATprove assumes it knows nothing about the possible initial values of its parameters and of the global variables it accesses except what is stated in the subprogram’s precondition. If no precondition is given, then no assumptions can be made.

For example, the following code shows the body of *Increment* can be successfully verified as its precondition constrains the value of its parameter *X* to be less than :ada:`Integer’Last`.

.. code:: ada

   procedure Increment (X : in out Integer) with
      Pre => X < Integer’Last is
   begin
      X := X + 1;
      -- info: overflow check proved
   end;

   X := Integer’Last - 2;
   Increment (X);
   -- Here GNATprove does not know the value of X

   X := X + 1;
   -- medium: overflow check might fail

In the same way, when a subprogram is called, GNATprove assumes its :ada:`out` and :ada:`in out` parameters and the global variables it writes can be modified in any way compatible with its postcondition. For example, since *Increment* has no postcondition, GNATprove does not know that *X* is smaller than :ada:`Integer’Last` after the call. Therefore, it cannot prove that the following addition cannot overflow.


Exceptions
~~~~~~~~~~

There are two cases where modularity is not enforced by GNATprove. First, local subprograms without contracts can be inlined if they are simple enough, however they should not be recursive or have multiple return points. If we remove the contract from *Increment* then it fits the criteria for in-lining.

.. code:: ada

   procedure Increment (X : in out Integer) is
   begin
      X := X + 1;
      -- info: overflow check proved, in call inlined at line 7
   end Increment;

   X := Integer’Last - 2;
   Increment (X);
   X := X + 1;
   -- info: overflow check proved

As GNATprove sees the call to *Increment* exactly as if the increment on *X* was done directly, it can verify successfully that no overflow may occur on either of the subsequent additions. The other case concerns expression functions. If a function is defined as an expression function, with or without contracts, then it is handled as if it had a postcondition stating the value of its result.

In our example, replacing *Increment* with an expression function allows GNATprove to verify successfully the overflow check in the following addition.

.. code:: ada

   function Increment (X : Integer) return Integer is
      (X + 1)
      -- info: overflow check proved
      with Pre => X < Integer’Last;

   X := Integer’Last - 2;
   X := Increment (X);
   X := X + 1;
   -- info: overflow check proved


Contracts
---------------------------------------------------------------------

Though they are perfectly suited for formal verification, Ada 2012 contracts are primarily designed to be checked at runtime. Code that verifies the contracts at runtime can be generated by the compiler using the appropriate switch, which is ``-gnata``. If an Ada 2012 contract does not hold at a given subprogram call, an exception, named :ada:`assert_failure`, will be raised. This is particularly convenient during development and testing, but execution of assertions, and in particular of preconditions, may also be retained during the program’s deployment to avoid reaching an inconsistent state.

For example, given the following code:

.. code:: ada

   procedure Increment (X : in out Integer) with
      Pre => X < Integer’Last;

   X := Integer’Last;
   Increment (X);
   -- raised ASSERT_FAILURE : failed precondition

   procedure Absolute (X : in out Integer) with
      Post => X >= 0 is
   begin
      if X > 0 then
         X := - X;
      end if;
   end Absolute;

   X := 1;
   Absolute (X);
   -- raised ASSERT_FAILURE : failed postcondition

If called on :ada:`Integer’Last`, *Increment* will fail before its body is even started, possibly avoiding an inconsistent modification of the global state of the program. In the same way, any call to the badly implemented *Absolute* function on anything else than 0 will fail before the caller can be badly impacted by receiving a negative value. This early failure detection allows an easier recovery and facilitates debugging.

To ensure the soundness of its analysis, GNATprove needs to statically verify pre and postconditions contracts. Like in the runtime semantics of contracts, preconditions are verified every time a subprogram is called. Postconditions, on the other hand, are verified modularly once and for all as part of the verification of the subprogram’s body.

In the following example, GNATprove will detect both the identified errors as soon as they are visible.

.. code:: ada

   procedure Increment (X : in out Integer) with
      Pre => X < Integer’Last;

   X := Integer’Last;
   Increment (X);
   -- medium: precondition might fail

   procedure Absolute (X : in out Integer) with
      Post => X >= 0 is
      -- medium: postcondition might fail, requires X >= 0
   begin
      if X > 0 then
         X := - X;
      end if;
   end Absolute;

   X := 1;
   Absolute (X);

For the precondition, it has to wait until *Increment* is improperly called, as a precondition is really a contract for the caller. On the other hand, it does not need *Absolute* to be called to detect that its postcondition does not hold on all its possible inputs.


Executable Semantics
~~~~~~~~~~~~~~~~~~~~

In Ada 2012,  expressions in contracts have the regular semantics of Boolean expressions. In particular, runtime errors may occur during their computation. To facilitate both debugging of assertions and combination of testing and static verification, the same semantics is used by GNATprove.

During proof of programs, it makes sure that no error will ever be raised during the execution of the contracts. This semantic may sometimes be considered too heavy, in particular regarding overflow checks. For example, we tried specifying an appropriate precondition for the function *Add* that would avoid overflows in its body when computing the addition of *X* and *Y*.

.. code:: ada

   function Add (X, Y : Integer) return Integer with
      Pre => X + Y in Integer;
      -- medium: overflow check might fail

   X := Add (Integer’Last, 1);
   -- raised CONSTRAINT_ERROR : overflow check failed

Unfortunately, as expressions in assertions have the regular Ada semantics, GNATprove complains that an errors may be raised while checking *Add*’s precondition. This is legitimate, as we may see by calling *Add* on :ada:`Integer’Last` and 1.

On the other hand, depending on the context, we may have preferred to have GNATprove use the mathematical semantics of addition and properly verify that no error will ever be raised at runtime in the body of *Add*. This behavior may be obtained by using a compiler switch named ``-gnato`` which allows to independently set the overflow mode in code and assertions to either reduce the number of overflow checks or to completely eliminate them. Note that this switch will also make the compiler avoid overflows at runtime.


Additional Contracts
~~~~~~~~~~~~~~~~~~~~

As we have seen, contracts are a key feature for GNATprove. It supports pre and postconditions, as well as assertions, introduced by the pragma :ada:`Assert`, and type predicates.

New contracts have also been introduced for the process of formal verification. For example, the new pragma :ada:`Assume` is handled as an assertion at execution but introduces an assumption for proof of program, that is, a Boolean expression which is assumed to be true by the tool without any verification. This feature is useful but must be used with great care.

Another construct introduced for GNATprove is the :ada:`Contract_Cases` aspect. It allows to specify the behavior of a subprogram by a disjunction of cases. Each element of a contract-cases is in fact a small contract made of a guard, which may only reference subprogram’s inputs and is evaluated before the call, and of a consequence. At each call of the subprogram, there must be one and only one case for which the guard evaluates to :ada:`True`. The consequence of this case is the only one that should hold on exit.

.. code:: ada

   procedure Absolute (X : in out Integer) with
      Pre            =>  X > Integer’First,
      Contract_Cases => (X <  0 => X = - X’Old,
                         X >= 0 => X =   X’Old);
      -- info: disjoint contract cases proved
      -- info: complete contract cases proved
      -- info: contract case proved

   pragma Assume (X < Integer’Last);

   X := X + 1;

In GNATprove, validity --- as well as disjointness and completeness of the :ada:`Contract_Cases` --- are verified only once in the context of the subprogram’s precondition.


Debug Failed Proof Attempts
---------------------------------------------------------------------

If GNATprove reports an error while verifying a program, it may be for different reasons:

- There might be an error in the program,

- the property may not be provable because of some missing information, or

- the prover used by GNATprove may be unable to discharge a perfectly valid verification condition.

The remainder of this lecture is dedicated to the sometimes tricky task of debugging failed proof attempts.

First, let us look at the case where there is indeed an error in the program. There are two possibilities: the code may be incorrect, or, and it is equally likely, the specification may be incorrect. As an example, there is an error in our procedure *Incr_Until* which makes its :ada:`Contract_Cases` unprovable.

.. code:: ada

   procedure Incr_Until (X : in out Natural) with
      Contract_Cases =>
         (Incremented => X > X’Old,
         -- medium: contract case might fail
          others      => X = X’Old) is
         -- medium: contract case might fail
   begin
      if X < 1000 then
         X := X + 1;
         Incremented := True;
      else
         Incremented := False;
      end if;
   end Incr_Until;

As assertions can be executed, it may help to test the program on a representative set of inputs with assertions enabled. This allows bugs to be found both in the code and in its contracts. For example, testing *Incr_Until* on an input bigger than 1000 will raise an exception at runtime.

.. code:: ada

   procedure Incr_Until (X : in out Natural) with
      Contract_Cases =>
         (Incremented => X > X’Old,
          others      => X = X’Old) is
   begin
      -- ...
   end Incr_Until;

   X := 0;
   Incr_Until (X);

   X := 1000;
   Incr_Until (X);
   -- raised ASSERT_FAILURE : failed contract case at line 3

   -- Incremented is True when evaluating the
   -- Contract_Cases’ guards?
   -- That is because they are evaluated before the call!

It specifies that the first contract case is failing, which means that *Incremented* is :ada:`True`. Still, if we print the value of *Incremented* after the call, we will see that it is :ada:`False`, as expected for such an input. Indeed, guards of contract cases are evaluated before the call, and our specification is erroneous. To correct this, we should either put *X* < 1000 as a guard of the first contract case or use a standard postcondition with an if expression instead.

Even if both the code and the assertions are correct, GNATprove may still generate an unprovable verification condition for a property. This may happen for two reasons:

- First, the property may be unprovable because some assertion is missing in the code.

   - In particular, this can be induced by the modularity of the analysis which causes the tool to only retain explicitly annotated properties.

- Second, there may also be some missing information in the logical model of the program used by GNATprove.

This is especially likely for difficult to support features such as floating-point arithmetic or string literals. As an example, the verification generated by GNATprove for the postcondition of *Increase* is unprovable.

.. code:: ada

   C : Natural := 100;

   procedure Increase (X : in out Natural) with
      Post => (if X < C then X > X’Old else X = C) is
      -- medium: postcondition might fail
   begin
      if X < 90 then
         X := X + 10;
      elsif X >= C then
         X := C;
      else
         X := X + 1;
      end if;
   end Increase;

It states that, if its parameter *X* is smaller than a certain value *C*, then its value will be increased by the procedure, whereas if it is bigger, its value will be saturated to *C*.

When used with the appropriate options, GNATprove can provide additional information on a failed verification condition. In particular, if the condition is complex, it can locate precisely the part of the condition which is failing. For the example shown here, GNATprove states that it cannot prove that *X* = *C*, which means that we are in a case where *X* is greater than *C*.

.. code:: ada

   C : Natural := 100; -- Requires C >= 90

   procedure Increase (X : in out Natural) with
      Post => (if X < C then X > X’Old else X = C) is
      -- medium: postcondition might fail, requires X = C
   begin
      if X < 90 then
         X := X + 10;
      elsif X >= C then
         X := C;

Another additional information may help the code review. If it is used inside GNATbench or GPS, GNATprove can highlight the path in the program leading to a fail condition. Here, it is the first branch of the if statement. As a consequence, we know that GNATprove cannot prove the postcondition of *Increase* when both *X* is greater than *C* and *X* is smaller than 90. Indeed, in this case, our postcondition does not hold. But maybe we did not expect the value of *C* to change, or at least not to go below 90. In this case, we should simply state so by either declaring *C* to be constant or adding a precondition to the *Increase* subprogram.

Finally, there are cases where GNATprove provides a perfectly valid verification condition for a property, but it is not proved by the automatic prover in latter stages of the tool execution. This is quite a common occurrence. Indeed, GNATprove produces its verification conditions in first order logic, which is not decidable, especially in combination with arithmetic. Sometimes, the automatic prover just needs more time. But also sometimes, the prover will abandon the search almost immediately or loop forever without reaching a conclusive answer.

For example, the postcondition of our *GCD* function --- which calculates the value of the *GCD* of two positive numbers using Euclide’s algorithm --- cannot be verified with GNATprove’s default settings.

.. code:: ada

   function GCD (A, B : Positive) return Positive with
   Post => A mod GCD’Result = 0
       and B mod GCD’Result = 0 is
   -- medium: postcondition might fail
   begin
      if A > B then
         return GCD (A - B, B);
      elsif B > A then
         return GCD (A, B - A);
      else
         return A;
      end if;
   end GCD;

The first thing to try is to increase the maximal amount of time that the prover is allowed to spend on each verification condition using the option ``--timeout`` of GNATprove or the dialog box inside GPS. In our example, bumping it to one minute, which is relatively high, does not help. We can also specify an alternative automatic prover --- if we have one --- using the option ``--prover`` of GNATprove or the dialog box. For our postcondition, we have tried both z3, Alt-ergo, and CVC4 without any luck.

.. code:: ada

   function GCD (A, B : Positive) return Positive with
   Post => A mod GCD’Result = 0
       and B mod GCD’Result = 0 is
   begin
      if A > B then
         Result := GCD (A - B, B);
         pragma Assert ((A – B) mod Result = 0);
      -- info: assertion proved
         pragma Assert (B mod Result = 0);
      -- info: assertion proved
         pragma Assert (A mod Result = 0);
      -- medium: assertion might fail

To better understand the problem, we have added intermediate assertions to simplify the proof and pin down the part that was causing the problem. This is often a good idea when trying to understand by review why a property is not proved. Here, provers cannot verify that, if *A* - *B* and *B* can be divided by *Result*, then so does *A*. This may seem surprising, but non-linear arithmetic, involving multiplication, modulo, or exponentiation for example, is a difficult topic for provers and is not handled very well in practice by any of the general-purpose ones like Alt-Ergo, Z3, or CVC4.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

Let's review this code:

.. code:: ada

   package Lists with SPARK_Mode is
      function Goes_To (I, J : Index) return Boolean;

      procedure Link (I, J : Index) with Post => Goes_To (I, J);
   private
      type Cell (Is_Set : Boolean := True) is record …
      type Cell_Array is array (Index) of Cell;

      Memory : Cell_Array;
   end Lists;

   package body Lists with SPARK_Mode is
      function Goes_To (I, J : Index) return Boolean is
      begin
         if Memory (I).Is_Set then
            return Memory (I).Next = J;
         end if;
         return False;
      end Goes_To;

      procedure Link (I, J : Index) is
      begin
         Memory (I) := (Is_Set => True, Next => J);
      end Link;
   end Lists;

This example is correct, but it cannot be verified with GNATprove. As *Goes_To* has no postcondition, nothing is known about its result.


Example #2
~~~~~~~~~~

Let's review this code:

.. code:: ada

   package Lists with SPARK_Mode is
      function Goes_To (I, J : Index) return Boolean;

      procedure Link (I, J : Index) with Post => Goes_To (I, J);
   private
      type Cell (Is_Set : Boolean := True) is record …
      type Cell_Array is array (Index) of Cell;

      Memory : Cell_Array;

      function Goes_To (I, J : Index) return Boolean is
            (Memory (I).Is_Set and then Memory (I).Next = J);
   end Lists;

   package body Lists with SPARK_Mode is
      procedure Link (I, J : Index) is
      begin
         Memory (I) := (Is_Set => True, Next => J);
      end Link;
   end Lists;

This example is correct. *Goes_To* is an expression function. As a consequence, its body is available for proof.


Example #3
~~~~~~~~~~

Let's review this code:

.. code:: ada

   package Stacks with SPARK_Mode is
      type Stack is private;

      function  Peek (S : Stack) return Natural;
      procedure Push (S : in out Stack; E : Natural) with
         Post => Peek (S) = E;
   private
      type Stack is record ...
      function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);
   end Stacks;

   package body Stacks with SPARK_Mode is
      procedure Push (S : in out Stack; E : Natural) is
      begin
         if S.Top >= Max then
            return;
         end if;

         S.Top := S.Top + 1;
         S.Content (S.Top) := E;
      end Push;
   end Stacks;

This example is not correct. The postcondition of *Push* is only true if the stack is not full when *Push* is called.


Example #4
~~~~~~~~~~

Let's review this code:

.. code:: ada

   package Stacks with SPARK_Mode is
      type Stack is private;

      function  Peek (S : Stack) return Natural;
      procedure Push (S : in out Stack; E : Natural) with
         Post => Peek (S) = E;
   private
      type Stack is record ...
      function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);
   end Stacks;

   package body Stacks with SPARK_Mode is
      procedure Push (S : in out Stack; E : Natural) is
      begin
         if S.Top >= Max then
            raise Is_Full_E;
         end if;

         S.Top := S.Top + 1;
         S.Content (S.Top) := E;
      end Push;
   end Stacks;

This example is not correct. GNATprove can now verify *Push*’s postcondition as it only considers paths leading to normal termination. It will warn that *Is_Full_E* may be raised at runtime though, leading to an error.


Example #5
~~~~~~~~~~

Let's review this code:

.. code:: ada

   package Stacks with SPARK_Mode is
      type Stack is private;

      function  Peek (S : Stack) return Natural;
      function  Is_Full (S : Stack) return Natural;
      procedure Push (S : in out Stack; E : Natural) with
         Pre  => not Is_Full (S),
         Post => Peek (S) = E;
   private
      type Stack is record ...
      function Peek (S : Stack) return Natural is
         (if S.Top in S.Content'Range then S.Content (S.Top) else 0);
      function Is_Full (S : Stack) return Natural is (S.Top >= Max);
   end Stacks;

   package body Stacks with SPARK_Mode is
      procedure Push (S : in out Stack; E : Natural) is
      begin
         if S.Top >= Max then
            raise Is_Full_E;
         end if;
         S.Top := S.Top + 1;
         S.Content (S.Top) := E;
      end Push;
   end Stacks;

This example is correct. In the context of the precondition, GNATprove can now verify that *Is_Full_E* can never be raised at runtime.


Example #6
~~~~~~~~~~

Let's review this code:

.. code:: ada

   procedure Read_Record (From : Integer) is
      function Read_One (First : Integer; Offset : Integer)
         return Integer
      with
         Pre => Memory (First) + Offset in Memory'Range
      is
         Value : Integer := Memory (Memory (First) + Offset);
      begin
         if Is_Too_Coarse (Value) then
            Treat_Value (Value);
         end if;
         return Value;
      end Read_One;
   begin
      Size := Read_One (From, 0);
      pragma Assume (Size in 1 .. 10
                     and then Memory (From) < Integer'Last - 2 * Size);
      Data1 := Read_One (From, 1);
      Addr  := Read_One (From, Size + 1);
      pragma Assume (Memory (Addr) > Memory (From) + Size);
      Data2 := Read_One (Addr, -Size);
   end Read_Record;

It is correct, but it cannot be verified with GNATprove. GNATprove analyses *Read_One* on its own and notices that an overflow may occur in its precondition in certain contexts.


Example #7
~~~~~~~~~~

Let's review this code:

.. code:: ada

   procedure Read_Record (From : Integer) is
      function Read_One (First : Integer; Offset : Integer)
         return Integer
      with
         Pre => Memory (First) <= Memory’Last – Offset
      is
         Value : Integer := Memory (Memory (First) + Offset);
      begin
         if Is_Too_Coarse (Value) then
            Treat_Value (Value);
         end if;
         return Value;
      end Read_One;
   begin
      Size := Read_One (From, 0);
      pragma Assume (Size in 1 .. 10
                     and then Memory (From) < Integer'Last - 2 * Size);
      Data1 := Read_One (From, 1);
      Addr  := Read_One (From, Size + 1);
      pragma Assume (Memory (Addr) > Memory (From) + Size);
      Data2 := Read_One (Addr, -Size);
   end Read_Record;

This example is not correct. Unfortunately, our attempt to correct *Read_One*’s precondition failed. For example, an overflow will occur at runtime when *Memory (First)* is :ada:`Integer'Last` and *Offset* is negative.


Example #8
~~~~~~~~~~

Let's review this code:

.. code:: ada

   procedure Read_Record (From : Integer) is
      function Read_One (First : Integer; Offset : Integer)
         return Integer
      is
         Value : Integer := Memory (Memory (First) + Offset);
      begin
         if Is_Too_Coarse (Value) then
            Treat_Value (Value);
         end if;
         return Value;
      end Read_One;
   begin
      Size := Read_One (From, 0);
      pragma Assume (Size in 1 .. 10
                     and then Memory (From) < Integer'Last - 2 * Size);
      Data1 := Read_One (From, 1);
      Addr  := Read_One (From, Size + 1);
      pragma Assume (Memory (Addr) > Memory (From) + Size);
      Data2 := Read_One (Addr, -Size);
   end Read_Record;

This example is correct. We could have fixed the contract on *Read_One* to handle correctly positive and negative values of *Offset*. However, we found it simpler to let the function be inlined for proof by removing its precondition.


Example #9
~~~~~~~~~~

Let's review this code:

.. code:: ada

   procedure Compute (X : in out Integer) with
      Contract_Cases => ((X in -100 .. 100) => X = X'Old * 2,
                         (X in    0 .. 199) => X = X'Old + 1,
                         (X in -199 .. 0)   => X = X'Old - 1,
                          X >=  200         => X =  200,
                          others            => X = -200)
   is
   begin
      if X in -100 .. 100 then
         X := X * 2;
      elsif X in 0 .. 199 then
         X := X + 1;
      elsif X in -199 .. 0 then
         X := X - 1;
      elsif X >= 200 then
         X := 200;
      else
         X := -200;
      end if;
   end Compute;

This example is not correct. We duplicated in *Compute*’s contract the content of its body. This is not correct with respect to the semantics of :ada:`Contract_Cases` which expects disjoint cases, like a case statement.


Example #10
~~~~~~~~~~~

Let's review this code:

.. code:: ada

   procedure Compute (X : in out Integer) with
      Contract_Cases => ((X in    1 ..  199) => X >= X'Old,
                         (X in -199 ..   -1) => X <= X'Old,
                          X >=  200          => X =  200,
                          X <= -200          => X = -200)
   is
   begin
      if X in -100 .. 100 then
         X := X * 2;
      elsif X in 0 .. 199 then
         X := X + 1;
      elsif X in -199 .. 0 then
         X := X - 1;
      elsif X >= 200 then
         X := 200;
      else
         X := -200;
      end if;
   end Compute;

This example is not correct. Here, GNATprove can successfully check that the different cases are disjoint. It can also successfully verify each case on its own. This is not enough though, as a :ada:`Contract_Cases` must also be total. Here, we forgot the value 0.
