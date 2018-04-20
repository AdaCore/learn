Lesson 1: SPARK 2014 Overview
=====================================================================

.. role:: ada(code)
   :language: ada

Welcome to the first lesson of the AdaCore University on SPARK 2014! During this lecture, you'll be provided with an overview of the SPARK 2014 technology and toolset.


What is it?
---------------------------------------------------------------------

SPARK 2014 refers to two different things:

- a programming language targeted at functional specification and static verification, plus
- a set of development and verification tools.

The SPARK 2014 language is based on a subset of the Ada language, which is particularly suited to formal verification, as it is designed for critical software development.

.. image:: 01_spark_ada.png

Ada 2012 introduced the use of aspects that can be used for subprogram contracts, and SPARK 2014 added its own aspects in order to aid static analysis.


What do the tools do?
---------------------------------------------------------------------

We shall start by reviewing static verification of your programs, meaning the verification of the source code directly without compiling or executing it. This verification involves tools that perform static analysis and this, in general, can take various forms. It includes, for example, type checking and visibility rules enforcement, as done by the compiler, as well as more complex reasoning, such as abstract interpretation, as done by a tool like CodePeer from AdaCore. The tools perform two different forms of static analysis:

- The first one is called flow analysis and, in general, it is the fastest form of analysis and it checks in particular initialization of variables and data dependencies between inputs and outputs of subprograms.

   - It can also find unused assignments and unmodified variables.

- The second one is called formal proof and it checks in particular absence of runtime error in program execution as well as conformance with Ada 2012 contracts.

Key Tools
---------------------------------------------------------------------

The tool for formal verification of the SPARK 2014 language is called GNATprove. It checks for conformance with the SPARK subset and performs flow analysis and functional verification of the source code. The SPARK 2014 language is also supported by several other tools. In particular, it is fully supported by the GNAT compiler and by the GPS integrated development environment.


A trivial example
---------------------------------------------------------------------

We will now look at a simple example of Ada 2012 subprogram that has also used SPARK 2014 aspects to specify a verifiable subprogram contract. The subprogram called ``Increment`` adds 1 to the value of its parameter ``X``:

.. code:: ada

   procedure Increment
            (X : in out Integer)

   with Global  => null,

         Depends => (X => X),

         Pre     => X < Integer'Last,
         Post    => X = X'Old + 1;

   procedure Increment
            (X : in out Integer)
   is
   begin
      X := X + 1;
   end Increment;


Several properties can be specified on this subprogram using the shown contracts:

- The SPARK 2014 Global aspect specifies that ``Increment`` does not read and does not write any global variable.

- The SPARK 2014 Depend aspect is especially interesting for the security of this subprogram, as it specifies that the value of the parameter ``X`` after the call only depends on the value of ``X`` before the call.

- Functional properties of Increment are specified using the :ada:`Pre` and :ada:`Post` aspects of Ada 2012.

   - Increment can only be called if the value of ``X`` before the call is smaller that :ada:`Integer’Last`.

      - It is necessary to ensure that the addition operation performed in the subprogram body will also not overflow.

   - Finally, we specify that ``Increment`` does indeed perform an increment of ``X``, that is, the value of ``X`` after a call is one more than its value before the call.

The SPARK 2014 verification tools can verify all of these contracts. It additionally makes sure that no error may be raised at runtime when executing ``Increment``’s body.


The Programming Language
---------------------------------------------------------------------

At this point it helps to understand the rationale behind the differences between the SPARK and Ada languages. The aim while designing the SPARK subset of Ada was to create the biggest possible subset still amenable to easy specification and sound verification.

The most notable exclusions include access type and allocators, as well as handling of exceptions, which are both known to increase considerably the amount of required user-written annotations. Goto statements and controlled types are also not supported as they introduce non-trivial control flow. The two remaining restrictions are side-effects in expressions and aliasing of names, which we will now look at in more detail.


Limitations
---------------------------------------------------------------------

No side-effects in expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SPARK language does not support side-effects in expressions, that is, evaluating a SPARK expression cannot update any object. This limitation is necessary to avoid unpredictable behavior depending on order of evaluation, parameter passing mechanism, or compiler optimizations. The expression below for ``G`` is non-deterministic due to the order in which the two calls to F are evaluated, and is therefore not legal SPARK 2014.

.. code:: ada

   G : Integer;

   function F (X : in out Integer) return Integer;

   G := F (G) + F (G); --  ??


To aid the static verification of expressions and because function calls are themselves expressions, they must also be free of side effects. Potential side effects of a function include updates of parameters and global variables. As a consequence, SPARK 2014 forbids subprograms that are functions with :ada:`out` or :ada:`in out` parameters, like the function ``F``, as well as functions updating a global variable.

.. code:: ada

   function F (X : in out Integer) return Integer; -- Illegal

   function Incr (X : Integer) return Integer;  -- OK?

   function Incr_And_Log (X : Integer) return Integer;  -- OK?

In most cases, these functions can easily be replaced by procedures.


No side-effects in expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When it has access to their body, the SPARK tool verifies which are functions are indeed free from side effects. Here for example, the two functions ``Incr`` and ``Incr_And_Log`` have the same signature.

.. code:: ada

   function Incr (X : Integer) return Integer;  -- OK?

   function Incr_And_Log (X : Integer) return Integer;  -- OK?

However, ``Incr`` is valid SPARK while ``Incr_And_Log`` is not as it attempts to update the global variable ``Call_Count``.

.. code:: ada

   function Incr (X : in Integer) return Integer
      is (X + 1); -- OK

   Call_Count : Natural := 0;

   function Incr_And_Log (X : in Integer) return Integer is
   begin
      Call_Count := Call_Count + 1; -- Illegal
      return X + 1;
   end Incr_And_Log;


No aliasing of names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another restriction imposed in the SPARK subset concerns aliasing. We say that two names are aliased if they refer to the same object. Since access types are not allowed in SPARK, aliasing can only occur as part of the parameter passing in a procedure call. As a consequence, when a procedure is called, SPARK makes sure that no :ada:`out` or :ada:`in out` parameter is aliased with either an other parameter of the procedure or a global variable updated in the procedure’s body.

There are two reasons to forbid aliasing in SPARK:

- First, it makes verification more difficult

   - as it requires taking into account the fact that updates to two variables with different names may in fact update the same object.

- Then, results may seem unexpected from a user point of view.

   - Indeed, when its parameters are aliased, the results of a subprogram call may depend on compiler specific treatment, like parameter passing mechanisms.

What is more, most of the time, possibility of aliasing was not even taken into account by the programmer. For example:

.. code:: ada

   Total : Natural := 0;

   procedure Move_To_Total (Source : in out Natural) is
   begin
      Total  := Total + Source;
      Source := 0;
   end Move_To_Total;

The example subprogram ``Move_To_Total`` shown here increases the global variable ``Total`` of the value of its input parameter ``Source``. It then resets ``Source`` to 0. Here obviously, the programmer has not taken into account the possibility of an aliasing between ``Total`` and ``Source``. This is common practice. This subprogram is valid SPARK, and, for its verification, the SPARK 2014 tools assume, like the programmer, non-aliasing between ``Total`` and ``Source``. To ensure that this assumption is correct, the tool will then check for non-aliasing on every call to ``Move_To_Total``.

.. code:: ada

   X : Natural := 3;

   Move_To_Total (X); -- OK
   Move_To_Total (Total); -- Error


Identifying SPARK Code
---------------------------------------------------------------------

The SPARK language has been restricted to only allow easily specifiable and verifiable constructs. However, sometimes, a user cannot or does not want to abide by these limitations on all her code base. Therefore, the SPARK 2014 tools only check conformance to the SPARK subset on code which identified as being in SPARK.

This can be done using an aspect named :ada:`SPARK_Mode`. If not explicitly specified, :ada:`SPARK_Mode` is `Off`, which means, the code is in full Ada. This default can be changed using a configuration pragma also. To allow easy reuse of existing Ada library, entities declared in withed units with no explicit :ada:`SPARK_Mode` can still be used from SPARK code. The tool will only check for SPARK conformance on the declaration of those which are effectively used within the SPARK code.

Here is a common case of use of the :ada:`SPARK_Mode` aspect.

.. code:: ada

   package P
      with SPARK_Mode => On
   is
      -- package spec is SPARK, so can be used
      -- by SPARK clients
   end P;


   package body P
      with SPARK_Mode => Off
   is
      -- body is NOT SPARK, so assumed to
      -- be full Ada
   end P;

The package ``P`` only defines entities whose specifications are in the SPARK subset. However, it uses full Ada features in its body which, therefore, should not be analyzed and have the  :ada:`SPARK_Mode` aspect set to `Off`.

:ada:`SPARK_Mode` can be specified in a fine-grained manner on a per-unit basis. More precisely, a package has four different parts: the visible and private parts of its specification, as well as the declarative and statement part of its body. On each of these parts, :ada:`SPARK_Mode` can be specified to be either `On` or `Off`. In the same way, a subprogram has two parts: its specification and its body.

A general rule in SPARK is that when :ada:`SPARK_Mode` has been set to `Off`, it can never be switched to `On` again. This prevents both setting :ada:`SPARK_Mode` to `On` on subunits of a unit with :ada:`SPARK_Mode` `Off` and switching back to :ada:`SPARK_Mode` `On` on a part of a given unit when a previous part had been set to `Off`.


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

Here is a package defining a private ``Stack`` type containing elements of type ``Element`` and along with some subprograms providing the usual functionalities over stacks. It is marked to be in the SPARK subset.

.. code:: ada

   package Stack_Package
      with SPARK_Mode => On
   is
      type Element is new Natural;
      type Stack is private;

      function Empty return Stack;
      procedure Push (S : in out Stack; E : Element);
      function Pop (S : in out Stack) return Element;

   private
      --  ...
   end Stack_Package;

Side effects in expressions are not allowed in SPARK. Therefore, ``Pop`` is not allowed to modify its parameter ``S``.


Example #2
~~~~~~~~~~

Here we are interested in a package body providing a single instance stack. ``Content`` and ``Top`` are the global variables used to register the stack’s state. Once again, this package is identified to be in the SPARK subset.

.. code:: ada

   package body Global_Stack
      with SPARK_Mode => On
   is
      Max : constant Natural := 100;
      type Element_Array is array (1 .. Max) of Element;

      Content : Element_Array;
      Top     : Natural;

      function Pop return Element is
         E : constant Element := Content (Top);
      begin
         Top := Top – 1;
         return E;
      end Pop;

   end Global_Stack;

Like previously, functions should be free from side effects. Here, ``Pop`` updates the global variable ``Top``, which is not allowed in SPARK.


Example #3
~~~~~~~~~~

We now consider two procedures ``Permute`` and ``Swap``. ``Permute`` applies a circular permutation to the value of its three parameters. ``Swap`` then uses ``Permute`` to swap the value of ``X`` and ``Y``.

.. code:: ada

   package body P
      with SPARK_Mode => On
   is
      procedure Permute (X, Y, Z : in out Positive) is
         Tmp : constant Positive := X;
      begin
         X := Y;
         Y := Z;
         Z := Tmp;
      end Permute;

      procedure Swap (X, Y : in out Positive) is
      begin
         Permute (X, Y, Y);
      end Swap;
   end P;

Here, in the call to ``Permute``, actual values for parameters ``Y`` and ``Z`` are aliased, which is not allowed in SPARK. On this example, we see the reason why aliasing is not allowed in SPARK. Indeed, since ``Y`` and ``Z`` are :ada:`Positive`, they are passed by copy, and the result of the call to ``Permute`` therefore depends on the order in which they are copied back after the call.


Example #4
~~~~~~~~~~

Here, the ``Swap`` procedure is used to swap the value of the two record components of ``R``.

.. code:: ada

   package body P
      with SPARK_Mode => On
   is
      procedure Swap (X, Y : in out Positive);

      type Rec is record
         F1 : Positive;
         F2 : Positive;
      end record;

      procedure Swap_Fields (R : in out Rec) is
      begin
         Swap (R.F1, R.F2);
      end Swap_Fields;

      --  ...
   end P;

This code is correct. The call to ``Swap`` is safe, as two different components of the same record object cannot refer to the same object.


Example #5
~~~~~~~~~~

Here is a slight modification of the previous example using an array instead of a record. ``Swap_Indexes`` uses ``Swap`` on values stored in the array ``A``.

.. code:: ada

   package body P
      with SPARK_Mode => On
   is
      procedure Swap (X, Y : in out Positive);

      type P_Array is array (Natural range <>) of Positive;

      procedure Swap_Indexes (A : in out P_Array, I, J : Natural) is
      begin
         Swap (P (I), P (J));
      end Swap_Indexes;

      --  ...
   end P;

This code is not valid. Unlike the previous example, we have no way here to know that the two elements ``A (I)`` and ``A (J)`` really are distinct when we call ``Swap``.


Example #6
~~~~~~~~~~

Here is a package declaring a type ``Dictionary``, which is an array containing a word per letter. The procedure ``Store`` allows to insert a word at the correct index in a dictionary.

.. code:: ada

   package P
      with SPARK_Mode => On
   is
      subtype Letter is Character range ‘a’ .. ‘z’;
      type String_Access is access String;
      type Dictionary is array (Letter) of String_Access;

      procedure Store (D : in out Dictionary; W : String);
   end P;

   package body P
      with SPARK_Mode => On
   is
      procedure Store (D : in out Dictionary; W : String) is
         First_Letter : constant Letter := W (W’First);
      begin
         D (First_Letter) := new String’(W);
      end Store;
   end P;

This code is not correct, as access types are not part of the SPARK subset. In this case, they are really useful though, as, without them, we cannot store arbitrarily long strings into an array. The solution here is to use :ada:`SPARK_Mode` to separate parts of the access type from the rest of the code in a fine grained manner.


Example #7
~~~~~~~~~~

Here is a modified version of the previous example. It has been adapted to hide the access type inside the private part of ``P``.

.. code:: ada

   package P
      with SPARK_Mode => On
   is
      subtype Letter is Character range ‘a’ .. ‘z’;
      type String_Access is private;
      type Dictionary is array (Letter) of String_Access;

      function New_String_Access (W : String) return String_Access;

      procedure Store (D : in out Dictionary; W : String);

   private
      pragma SPARK_Mode (Off);

      type String_Access is access String;

      function New_String_Access (W : String) return String_Access is
         (new String’(W));
   end P;

As the access type is defined and used inside of a part in full Ada, this code is correct.


Example #8
~~~~~~~~~~

Now let us consider P’s body, with the definition of Store, again.

.. code:: ada

   package P with SPARK_Mode => On is
      subtype Letter is Character range ‘a’ .. ‘z’;
      type String_Access is private;
      type Dictionary is array (Letter) of String_Access;
      function New_String_Access (W : String) return String_Access;
      procedure Store (D : in out Dictionary; W : String);

   private
      pragma SPARK_Mode (Off);
      --  ...
   end P;

   package body P with SPARK_Mode => On is
      procedure Store (D : in out Dictionary; W : String) is
         First_Letter : constant Letter := W (W’First);
      begin
         D (First_Letter) := New_String_Access (W);
      end Store;
   end P;

Though the body of ``Store`` really uses no construct that are out of the SPARK subset, it is not possible to set :ada:`SPARK_Mode` to ``On`` on ``P``’s body. Indeed, even if we don’t use it, we have the visibility here on ``P``’s private part which is in full Ada.


Example #9
~~~~~~~~~~

Here, we have moved the declaration and the body of the procedure ``Store`` to another package named ``Q``.

.. code:: ada

   package P with SPARK_Mode => On is
      subtype Letter is Character range ‘a’ .. ‘z’;
      type String_Access is private;
      type Dictionary is array (Letter) of String_Access;
      function New_String_Access (W : String) return String_Access;
   private
      pragma SPARK_Mode (Off);
      --  ...
   end P;

   with P; use P;
   package Q with SPARK_Mode => On is
      procedure Store (D : in out Dictionary; W : String);
   end Q;

   package body Q with SPARK_Mode => On is
      procedure Store (D : in out Dictionary; W : String)  is
         First_Letter : constant Letter := W (W’First);
      begin
         D (First_Letter) := New_String_Access (W);
      end Store;
   end Q;

Here everything is fine. We have managed to retain the use of the access type while having most of our code in the SPARK subset, so that GNATprove will be able to analyze it.


Example #10
~~~~~~~~~~~

Here, we have two functions which are searching for 0 inside an array ``A``. The first one raises an exception if 0 is not found in ``A`` while the other simply returns 0 in that case.

.. code:: ada

   package body P with SPARK_Mode => On is
      type N_Array is array (Positive range <>) of Natural;
      Not_Found : exception;

      function Search_Zero_P (A : N_Array) return Positive is
      begin
         for I in A'Range loop
            if A (I) = 0 then
               return I;
            end if;
         end loop;
         raise Not_Found;
      end Search_Zero_P;

      function Search_Zero_N (A : N_Array) return Natural
         with SPARK_Mode => Off is
      begin
         return Search_Zero_P (A);
      exception
         when Not_Found => return 0;
      end Search_Zero_N;
   end P;

This code is perfectly correct. Remark that GNATprove will try to demonstrate that ``Not_Found`` will never be raised in ``Search_Zero_P``. Looking at ``Search_Zero_N``, it is likely that such a property is not true, which means that the user will need to verify that ``Not_Found`` will only be raised when appropriate by her own means.
