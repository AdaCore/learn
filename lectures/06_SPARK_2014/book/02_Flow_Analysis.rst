Lesson 2: Flow Analysis
=====================================================================

.. role:: ada(code)
   :language: ada


This lecture presents the static flow analysis capability provided by the GNATprove tool supporting SPARK 2014.


What does flow analysis do?
---------------------------------------------------------------------

Flow analysis concentrates primarily on variables. It models how information flows through them during a subprogram’s execution, linking the final values of variables to their initial values. It also takes into account global variables declared at library level, as well as local variables defined inside subprograms and formal parameters.

For a nested subprogram, we call *scope variables* variables that are declared locally to an enclosing unit. Scope variables really are global variables from the nested subprogram’s point of view.

Flow analysis is usually fast, it takes approximatively the same time as compilation. It permits the detection of kinds of errors, as well as violations of some SPARK requirements, including the absence of aliasing and side-effect freedom in expressions. Note that flow analysis is sound: if it does not detect any errors of a supported kind, then it is guaranteed that there are no errors.


Errors Detected
---------------------------------------------------------------------

Uninitialized Variables
~~~~~~~~~~~~~~~~~~~~~~~

We will now present each class of error which can be detected by flow analysis, starting with reading an uninitialized variable which is nearly always an error. It introduces non-determinism and breaks the type system as the value of an uninitialized variable may be outside that of its subtype’s range. For these reasons, GNATprove has resorted to requiring every read variable to be initialized.

Flow analysis is responsible for ensuring that this requirement is always fulfilled by SPARK code. For example, in the function *Max_Array* shown on this example, we have forgotten to initialize the value of *Max* prior to entering the loop. As a consequence, the value read in the condition of the if statement may be uninitialized. Flow analysis will detect and report this error.

.. code:: ada

   function Max_Array (A : Array_Of_Naturals) return Natural is
      Max : Natural;
   begin
      for I in A’Range loop
         if A (I) > Max then    -- Here Max may not be initialized
            Max := A (I);
         end if;
      end loop;
      return Max;
   end Max_Array;


Ineffective Statements
~~~~~~~~~~~~~~~~~~~~~~

Ineffective statements are different from dead code as they are executed, even usually modifying the value of variables. Only, they have no effect on any of the subprogram’s outputs, be they parameters, global variables or function result. Ineffective statements, like unused variables, should be avoided, as they make the code less readable and more difficult to maintain.

What is more, they are often caused by errors in the program, which may be difficult to detect. It is the case in the subprograms *Swap1* and *Swap2* shown below, which do not properly swap their two parameters *X* and *Y*. Though they are not considered as errors in themselves, flow analysis warns both on ineffective statements and unused variables.

.. code:: ada

   procedure Swap1 (X, Y : in out T) is
      Tmp : T;
   begin
      Tmp := X;             -- This statement is ineffective
      X   := Y;
      Y   := X;
   end Swap1;

   Tmp : T;

   procedure Swap2 (X, Y : in out T) is
      Temp : T := X;        -- This variable is unused
   begin
      X := Y;
      Y := Tmp;
   end Swap2;


Incorrect Parameter Mode
~~~~~~~~~~~~~~~~~~~~~~~~

Parameter modes influence the behavior of the compiler and are a key point for documenting the usage of a subprogram. Flow analysis will check that specified parameter modes always correspond to their usage in the subprogram’s body. More precisely, it will check that an :ada:`in` parameter is never updated, either directly or through a subprogram call. It will also check that the initial value of an :ada:`out` parameter will never be read in the subprogram, as it may not be copied on subprogram entry. Finally, flow analysis will also warn when an :ada:`in out` parameter is not updated or when its initial value is not used in the subprogram, as it may be the sign of an error. An example is shown below in the subprogram called *Swap*.

.. code:: ada

   procedure Swap (X, Y : in out T) is
      Tmp : T := X;
   begin
      Y := X;    -- The initial value of Y is not used
      X := Tmp;  -- Y is computed to be out
   end Swap;

Note that, in SPARK, a parameter which is not read but not updated on every path should be declared as :ada:`in out` as its final value may depend on its initial value.

+--------------------+----------------------+-----------------------+----------------+
| Initial value read | Updated on some path | Updated on every path | Parameter mode |
+====================+======================+=======================+================+
| X                  |                      |                       | in             |
+--------------------+----------------------+-----------------------+----------------+
| X                  | (X)                  | (X)                   | in out         |
+--------------------+----------------------+-----------------------+----------------+
|                    | X                    |                       | in out         |
+--------------------+----------------------+-----------------------+----------------+
|                    |                      | X                     | out            |
+--------------------+----------------------+-----------------------+----------------+


Additional Verifications
---------------------------------------------------------------------

Global Contracts
~~~~~~~~~~~~~~~~

Until now, we have seen verifications which do not require any additional annotations from the developer. Flow analysis will also check user-written flow annotations when supplied. In SPARK, it is possible to specify the global and scoped variables accessed or modified by a subprogram. This is done using an Ada 2012 like contract named :ada:`Global`.

When a :ada:`Global` contract is supplied by the user for a subprogram, flow analysis will check that it is correct and complete, that is, no other variable than those stated in the contract are accessed or modified, either directly or through a subprogram call. For example, we may want to specify that the function *Get_Value_Of_X* reads the value of the global variable *X* and does not access any other global variable.

.. code:: ada

   X : Natural := 0;

   function Get_Value_Of_X return Natural;
   -- Get_Value_Of_X reads the value of the global variable X


Global contracts are provided as part of the subprogram specification. Indeed, they provide useful information to users of a subprogram. The value specified for the :ada:`Global` aspect is an aggregate-like list of global variables’ names, grouped together depending on their mode.

In the example shown below, the procedure *Set_X_To_Y_Plus_Z* reads both *Y* and *Z*, listed as :ada:`Input`, and updates *X*, listed as :ada:`Output`. As *Set_X_To_X_Plus_Y* both updates *X* and reads its initial value, *X*’s mode is :ada:`In_Out`. Like for parameters, if no mode is specified, then the default is :ada:`Input`. That is the case in the declaration of *Get_Value_Of_X*. Finally, if a subprogram, like *Incr_Parameter_X*, does not reference any global variable, the value of the global contract should be set to :ada:`null`.

.. code:: ada

  procedure Set_X_To_Y_Plus_Z with
     Global => (Input  => (Y, Z), -- reads values of Y and Z
                Output => X);     -- modifies value of X

   procedure Set_X_To_X_Plus_Y with
     Global => (Input  => Y,  -- reads value of Y
                In_Out => X); -- modifies value of X
   -- also reads its initial value

   function Get_Value_Of_X return Natural with
     Global => X;  -- reads the value of the global variable X

   procedure Incr_Parameter_X (X : in out Natural) with
     Global => null; -- do not reference any global variable


Depends Contracts
~~~~~~~~~~~~~~~~~

A user may also supply a :ada:`Depends` contract for a subprogram to specify dependencies between its outputs and its inputs. Here, not only global variables are considered but also parameters and function results. When a :ada:`Depends` contract is supplied for a subprogram, flow analysis checks that it is correct and complete, that is, that each subprogram output is related to all of its inputs.

For example, a user may want to check that, on return of *Swap* shown below, each parameter only depends on the initial value of the other parameter or that the value of *X* on return of *Set_X_To_Zero* does not depend on any global variable.

.. code:: ada

  procedure Swap (X, Y : in out T);
   -- The value of X (resp. Y) after the call depends only
   -- on the value of Y (resp. X) before the call

   X : Natural;
   procedure Set_X_To_Zero;
   -- The value of X after the call depends on no input


Like :ada:`Global` contracts, a :ada:`Depends` contract is specified on subprogram declarations using an aspect. Its value is a list of one or more dependency relations between outputs and inputs of the program. Each such relation is represented as two lists of variable names separated by an arrow. At the left of the arrow are the variables whose final value depends on the initial value of the variables on the right.

For example, the final value of each parameter of *Swap* only depends on the initial value of the other parameter. If the subprogram is a function, its result must be listed as an output, as we did for *Get_Value_Of_X* using the :ada:`Result` attribute.

.. code:: ada

   procedure Swap (X, Y : in out T) with
     Depends => (X => Y,            -- X depends on the initial value of Y
                 Y => X);           -- Y depends on the initial value of X

   function Get_Value_Of_X return Natural with
     Depends => (Get_Value_Of_X’Result => X);    -- result depends on X

   procedure Set_X_To_Y_Plus_Z with
     Depends => (X => (Y, Z));      -- X depends on Y and Z

   procedure Set_X_To_X_Plus_Y with
     Depends => (X => + Y);          -- X depends on Y and X’s initial value

   procedure Do_Nothing (X : T) with
     Depends => (null => X);        -- No output is affected by X

   procedure Set_X_To_Zero with
     Depends => (X => null);        -- X depends on no input


It is often the case that the final value of a variable depends on its own initial value. This can be specified in a concise way using the :ada:`+` character, like in the specification of *Set_X_To_X_Plus_Y*. Note that, if there are more than one variable on the left of the arrow, a :ada:`+` means that each variables depends on itself, and not that they all depend on each other.

It can also be the case that an input is not used to compute the final value of any output. This can be expressed by putting :ada:`null` at the left of the dependency relation, like we have for the *Do_Nothing* subprogram shown here. Note that there can only be one such dependency relation, listing all the unused inputs of the subprogram, and that it must be declared last. Also note that such an annotation will silence flow analysis’ warning about unused parameters. Finally, :ada:`null` can also be used at the right of a dependency relation to state that an output depends on no input. It is the case for the procedure *Set_X_To_Zero*.


Shortcomings
---------------------------------------------------------------------

Modularity
~~~~~~~~~~

Flow analysis is a sound analysis, which means that, if it does not output any message on some analyzed SPARK code, then none of the supported errors may occur in this code. On the other hand, there are cases where flow analysis will issue a message when there are in fact no errors. The first ---and maybe most common reason for this--- has to do with modularity.

To improve efficiency on large projects, verifications are in general done on a per subprogram basis. It is in particular the case for detection of uninitialized variables. For this detection to be done modularly, flow analysis needs to assume initialization of inputs on subprogram entry and initialization of outputs after subprogram execution. Therefore, every time a subprogram is called, flow analysis will check that global and parameter inputs are initialized, and every time a subprogram returns, it will check that global and parameter outputs are also initialized.

This may lead to messages being issued on perfectly correct subprograms like *Set_X_To_Y_Plus_Z* which only sets its :ada:`out` parameter *X* when *Overflow* is :ada:`False`.

.. code:: ada

   procedure Set_X_To_Y_Plus_Z (Y, Z     :     Natural;
                                X        : out Natural;
                                Overflow : out Boolean) is
   begin
      if Natural’Last – Z < Y then
         Overflow := True; -- X should be initialized on every path
      else
         Overflow := False;
         X := Y + Z;
      end if;
   end Set_X_To_Y_Plus_Z;


This simply means that, in that case, flow analysis was not able to verify that no uninitialized variable could be read. To solve this problem, *X* can either be set to a dummy value when there is an overflow or the user can verify by her own means that *X* is never used after a call to *Set_X_To_Y_Plus_Z* if *Overflow* is :ada:`True`.


Composite Types
~~~~~~~~~~~~~~~

Another common cause for false alarms is the way composite types are handled in flow analysis. Let us first look at arrays in particular.

In flow analysis, array objects are treated as single, entire objects. This means that an update to an element of the array is handled as an update of the entire array object. Obviously, this makes reasoning about global variables accessed and dependencies less precise. But it also affects detection of reads of uninitialized variables.

Indeed, it is often impossible for flow analysis to decide if the entire object has been initialized, and so, even in really simple cases. For example, after initializing every element of an unconstrained array *A* with zero in a loop, we may still have a flow message stating that the array is not initialized. To solve this issue, a user can either use an aggregate assignment, or, if it is not possible, verify initialization of the object by other means.

.. code:: ada

  for I in A’Range loop
      A (I) := 0;
   end loop;
   -- flow analysis does not know that A is initialized

   A := (others => 0);
   -- flow analysis knows that A is initialized


Flow analysis is more precise on record objects, in the sense that it tracks separately the value of each component inside a single subprogram. As a consequence, when a record object is initialized by successive assignments of its components, flow analysis can make sure that the whole object is initialized. Note that record objects are still treated as entire objects when taken as input or output of subprograms.

.. code:: ada

   type Rec is record
      F1 : Natural;
      F2 : Natural;
   end record;

   R : Rec;

   R.F1 := 0;
   R.F2 := 0;
   --  R is initialized


For example, using a procedure call to initialize only some components of a record object will result in flow analysis complaining about non-initialization of to-be initialized components in entry of the subprogram, like for *Init_F2*.

.. code:: ada

   procedure Init_F2
     (R : in out Rec) is
   begin
      R.F2 := 0;
   end Init_F2;

   R.F1 := 0;
   Init_F2 (R);
   -- R should be initialized
   -- before this call


Value Dependency
~~~~~~~~~~~~~~~~

It is also worth noting that flow analysis is not value dependent, in the sense that it never reasons about values of expressions. As a consequence, if some path of execution in the subprogram is impossible due to values of expressions, it will still consider them feasible and therefore may emit unnecessary messages concerning them.

On the first version of *Absolute_Value*, for example, flow analysis computes that, on a path entering none of the two conditional statements, *R* is uninitialized. As it does not consider values of expressions, it cannot know that such a case can never happen.

.. code:: ada

  procedure Absolute_Value
     (X :     Integer;
      R : out Natural)
   is
   begin
      if X < 0 then
         R := -X;
      end if;
      if X >= 0 then
         R := X;
      end if;
   end Absolute_Value;

   -- Flow analysis does not
   -- know that R is initialized


To avoid this problem, it is better to make the control flow explicit, as in the second version of *Absolute_Value*:

.. code:: ada

   procedure Absolute_Value
     (X :     Integer;
      R : out Natural)
   is
   begin
      if X < 0 then
         R := -X;
      else
         R := X;
      end if;
   end Absolute_Value;

   -- Flow analysis knows that R
   -- is initialized


Contract Computation
~~~~~~~~~~~~~~~~~~~~

Finally, unexpected flow messages may come from inaccuracy in flow contract computations. Why does flow analysis compute contracts? As we have explained earlier, both :ada:`Global` and :ada:`Depends` contracts are optional. But GNATprove still needs them for some of its analysis.

For example, knowing the set of global variables accessed by a subprogram is necessary for detecting the use of uninitialized variables. As for :ada:`Depends` contracts on a subprogram, they are necessary to be able to check user-supplied dependency contracts on callers of this subprogram. As each flow contract on a subprogram depends on the flow contracts of all the subprograms called inside its body, this computation can easily be quite time-consuming. Therefore, flow analysis sometimes trades-off precision of this computation for efficiency.

That is in particular the case for :ada:`Depends` contracts, for which flow analysis simply assumes the worst: it assumes that each subprogram output depends on all of the subprogram’s inputs. To solve this issue, it is enough to manually supply contracts when computed ones are not precise enough. Note that supplying :ada:`Global` contracts may also be a good idea to speed up flow analysis on larger projects in general.


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

The procedure *Search_Array* searches for a particular element *E* in an array *A*. If the element is found, then it is stored in *Result*. Otherwise, *Found* is set to :ada:`False`.

.. code:: ada

  procedure Search_Array ( A      :     Array_Of_Positives;
                           E      :     Positive;
                           Result : out Integer;
                           Found  : out Boolean
                          ) is
   begin
      for I in A’Range loop
         if A (I) = E then
            Result := I;
            Found  := True;
            return;
         end if;
      end loop;
      Found := False;
   end Search_Array;

This example is not correct. Though there clearly are legal uses of the function *Search_Array*, flow analysis will complain that *Result* is not initialized on the path that does not exit inside the loop. Note that, even if this program is not incorrect, the flow message cannot necessarily be discarded. Indeed, it means that flow analysis cannot guaranty that *Result* will never be read when uninitialized, which is an assumption to further analysis performed by GNATprove. Therefore, the user should either initialize *Result* when *Found* is false, which will silence flow analysis, or verify this assumption by other means.


Example #2
~~~~~~~~~~

Here, to avoid the flow message from previous slide, *Search_Array* raises an exception when *E* is not found in *A*.

.. code:: ada

   Not_Found : exception;

   procedure Search_Array (A      :     Array_Of_Positives;
                           E      :     Positive;
                           Result : out Integer) is
   begin
      for I in A‘Range loop
         if A (I) = E then
            Result := I;
            return;
         end if;
      end loop;
      raise Not_Found;
   end Search_Array;

This example is correct. Flow analysis won’t emit any message here, which means that it can make sure that *Result* cannot be read uninitialized in SPARK code. Why is it, since *Result* is still not initialized when *E* is not in *A*? In fact, it comes from the fact that the exception *Not_Found* can never be caught inside SPARK code. Therefore, the burden of insuring that *Result* is never read when uninitialized is still on the user. However, it is no longer stated explicitly by the tool, as it now falls into a general category of assumptions documented in the user guide. Also note that the GNATprove tool as a whole will try to make sure that *Not_Found* is never raised in this program as part of ensuring absence of runtime errors in SPARK code.


Example #3
~~~~~~~~~~

Instead of raising an exception, we have chosen to use a discriminant record for that result of *Search_Array*. In this way, the index at which *E* was found in *A* can be set only when *E* was indeed found.

.. code:: ada

  type Search_Result (Found : Boolean := False) is record
      case Found is
      when True =>
         Content : Integer;
      when False => null;
      end case;
   end record;

   procedure Search_Array (A      :     Array_Of_Positives;
                           E      :     Positive;
                           Result : out Search_Result) is
   begin
      for I in A’Range loop
         if A (I) = E then
            Result := (Found   => True,
                       Content => I);
            return;
         end if;
      end loop;
      Result := (Found => False);
   end Search_Array;


This example is correct. No flow message will be emitted here, as flow analysis indeed can make sure both that no uninitialized variable will be read in *Search_Array*’s body, and that all its outputs are initialized on return.


Example #4
~~~~~~~~~~

The function *Size_Of_Biggest_Increasing_Sequence* goes over all the sequences of a global array *A* which contain increasing elements to compute the length of the biggest one. For this, a nested procedure *Test_Index* is called iteratively on all the elements of *A*. *Test_Index* checks if the sequence is still increasing. If it is the case, it updates the current maximal value read so far. Otherwise, it has found the end of an increasing sequence. It therefore computes the size of this sequence and stores it in *Size_Of_Seq*.

.. code:: ada

  function Size_Of_Biggest_Increasing_Sequence return Natural is
      Max         : Natural;
      End_Of_Seq  : Boolean;
      Size_Of_Seq : Natural;
      Beginning   : Integer;
      procedure Test_Index (Current_Index : Integer) is
      begin
         if A (Current_Index) >= Max then
            Max := A (Current_Index);
            End_Of_Seq := False;
         else
            Max         := 0;
            End_Of_Seq  := True;
            Size_Of_Seq := Current_Index - Beginning;
            Beginning   := Current_Index;
         end if;
      end Test_Index;
   begin
      for I in A’Range loop
         Test_Index (I);
         --  ...


This example is not correct. Flow analysis will emit a message on the call to *Test_Index* stating that *Max*, *Beginning*, and *Size_Of_Seq* should be initialized before the call. Indeed, both *Max* and *Beginning* need an initial value as they are read in *Test_Index*. As for *Size_Of_Seq*, if we only read its value when *End_Of_Seq* is true, which is probably meant so by design, then there can be no problem. Flow analysis can simply not verify its initialization modularly.


Example #5
~~~~~~~~~~

Permutations are modeled as arrays where the element at index *I* is the position of the *I* th element in the permutation. The procedure *Init* initializes a permutation to be the identity, the *I* th elements is at the *I* th position. *Cyclic_Permuation* calls *Init* and then swaps the elements until it has constructed a cyclic permutation.

.. code:: ada

   type Permutation is array (Positive range <>) of Positive;

   procedure Init (A : out Permutation) is
   begin
      for I in A'Range loop
         A (I) := I;
      end loop;
   end Init;

   function Cyclic_Permutation (N : Natural) return Permutation is
      A : Permutation (1 .. N);
   begin
      Init (A);
      for I in A'First .. A'Last - 1 loop
         Swap (A, I, I + 1);
      end loop;
      return A;
   end Cyclic_Permutation;


This program is correct. Flow analysis will still emit a message though, because it cannot make sure that every element of *A* is initialized during the loop. This message is a false alarm and can be discarded safely.


Example #6
~~~~~~~~~~

This program is the same as the previous one except that, to avoid the flow warning at the array assignment, the mode of *A* in the specification of *Init* has been changed to :ada:`in out`.

.. code:: ada

  type Permutation is array (Positive range <>) of Positive;

   procedure Init (A : in out Permutation) is
   begin
      for I in A'Range loop
         A (I) := I;
      end loop;
   end Init;

   function Cyclic_Permutation (N : Natural) return Permutation is
      A : Permutation (1 .. N);
   begin
      Init (A);
      for I in A'First .. A'Last - 1 loop
         Swap (A, I, I + 1);
      end loop;
      return A;
   end Cyclic_Permutation;


This program is not correct. Changing the mode of a parameter that should really be :ada:`out` to :ada:`in out` to silence a false alarm is not a good idea. Other than this obfuscates the specification of *Init*, now a message will be emitted on every call to the procedure for which *A* is not initialized.


Example #7
~~~~~~~~~~

*Incr_Step_Function* takes an array *A* as an argument. It then iterates through *A* to increment every element by the value of *Increment*. Only, for each index, it calculate a threshold which must not be exceeded after the increment. A global contract has been specified for *Incr_Until_Threshold*.

.. code:: ada

  Increment : constant Natural := 10;

   procedure Incr_Step_Function (A : in out Array_Of_Positives) is
      Threshold : Positive := Positive’Last;
      procedure Incr_Until_Threshold (I : Integer) with
        Global => (Input  => Threshold,
                   In_Out => A);

      procedure Incr_Until_Threshold (I : Integer) is
      begin
         if Threshold – Increment <= A (I) then
            A (I) := Threshold;
         else
            A (I) := A (I) + Increment;
         end if;
      end Incr_Until_Threshold;

   begin
      for I in A’Range loop
         .. .
           Incr_Until_Threshold (I);
      end loop;
   end Incr_Step_Function;


Everything is fine here. The *Global* contract, in particular, is correct. It mentions both *Threshold*, which is read but not updated in the procedure, and *A*, which is both read and updated. The fact that *A* is a parameter of an enclosing unit does not prevent its usage inside the :ada:`Global` contract as it really is global to *Incr_Until_Threshold*. Remark that we did not mention *Increment* as it is a static constant.


Example #8
~~~~~~~~~~

We are back to the procedure *Test_Index* from example #4. We have corrected the missing initializations and are now interested into the :ada:`Global` contract of *Test_Index*. Is it correct?

.. code:: ada

   Max         : Natural := 0;
   End_Of_Seq  : Boolean;
   Size_Of_Seq : Natural := 0;
   Beginning   : Integer := A’First - 1;
   procedure Test_Index (Current_Index : Integer) with
     Global => (In_Out => (Beginning, Max, Size_Of_Seq),
                Output => End_Of_Seq,
                Input  => Current_Index);

   procedure Test_Index (Current_Index : Integer) is
   begin
      if A (Current_Index) >= Max then
         Max := A (Current_Index);
         End_Of_Seq := False;
      else
         Max         := 0;
         End_Of_Seq  := True;
         Size_Of_Seq := Current_Index - Beginning;
         Beginning   := Current_Index;
      end if;
   end Test_Index;


This example is not correct. *Current_Index* is a parameter of *Test_Index*, it should not be referenced as a global variable. Also, if *A* is not a constant, it should be mentioned as an :ada:`Input` in the :ada:`Global` contract.


Example #9
~~~~~~~~~~

We have changed the :ada:`Global` contract of *Test_Index* to a :ada:`Depends` contract. Note that we do not in general need both as global variables accessed can be deduced from the :ada:`Depends` contract.

.. code:: ada

   Max         : Natural := 0;
   End_Of_Seq  : Boolean;
   Size_Of_Seq : Natural := 0;
   Beginning   : Integer := A’First - 1;
   procedure Test_Index (Current_Index : Integer) with
     Depends => ((Max, End_Of_Seq)        => (A, Current_Index, Max),
                 (Size_Of_Seq, Beginning) =>
                     +(A, Current_Index, Max, Beginning))

   procedure Test_Index (Current_Index : Integer) is
   begin
      if A (Current_Index) >= Max then
         Max := A (Current_Index);
         End_Of_Seq := False;
      else
         Max         := 0;
         End_Of_Seq  := True;
         Size_Of_Seq := Current_Index - Beginning;
         Beginning   := Current_Index;
      end if;
   end Test_Index;


This example is correct. Some of the dependencies, such as *Size_Of_Seq* depending on *Beginning*, come directly from the assignments in the subprogram. As the control flow influences the final value of all of the outputs, variables read in the condition, that is, *A*, *Current_Index*, and *Max*, are present in every dependency relation. Finally, the dependencies of *Size_Of_Eq* and *Beginning* on themselves come from the fact that they may not be modified by the subprogram execution.


Example #10
~~~~~~~~~~~

The subprogram *Identity* swaps the value of its parameter twice. Its :ada:`Depends` contract states that *X* the final value of *X* only depends on its initial value and the same for *Y*.

.. code:: ada

   procedure Swap (X, Y : in out Positive);

   procedure Swap (X, Y : in out Positive) is
      Tmp : constant Positive := X;
   begin
      X := Y;
      Y := Tmp;
   end Swap;

   procedure Identity (X, Y : in out Positive) with
     Depends => (X => X,
                 Y => Y);

   procedure Identity (X, Y : in out Positive) is
   begin
      Swap (X, Y);
      Swap (Y, X);
   end Identity;


This code is correct, but flow analysis cannot verify the :ada:`Depends` contract of *Identity*. Indeed, *Swap* has no user-specified :ada:`Depends` contract. As a consequence, flow analysis assumes that all outputs of *Swap*, that is *X* and *Y*, depend on all its inputs, that is both *X* and *Y*’s initial values. To solve this problem, it is enough to manually specify a more precise :ada:`Depends` contract on *Swap*.

