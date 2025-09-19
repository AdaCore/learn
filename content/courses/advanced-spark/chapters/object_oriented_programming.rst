Object-oriented Programming
=====================================================================

.. include:: ../../../global.txt

What is Object Oriented Programming?
---------------------------------------------------------------------

    Object-oriented software construction is
    the building of software systems as structured collections
    of [...] abstract data type implementations.

    Bertrand Meyer, “Object Oriented Software Construction”

- Object Oriented Programming is about:

    - isolating clients from implementation details (abstraction)

    - isolating clients from the choice of data types (dynamic
      dispatching)

- Object Oriented Programming is not:

    - the same as prototype programming (class and objects)

    - the same as scoping (class as the scope for methods)

    - the same as code reuse (use a component in a record in SPARK)


Prototypes and Scopes in SPARK
---------------------------------------------------------------------

- Types in SPARK come with methods aka primitive operations

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Type_Primitives

    package Show_Type_Primitives is

       type Int is range 1 .. 10;
       function Equal (Arg1, Arg2 : Int) return Boolean;
       procedure Bump (Arg : in out Int);

       type Approx_Int is new Int;
       --  implicit definition of Equal and Bump for Approx_Int

    end Show_Type_Primitives;

- Scope for the prototype is current declarative region

    - ... or up to the first freezing point – point at which the type must
      be fully defined, e.g. when defining an object of the type

- OOP without dynamic dispatching = Abstract Data Types


Classes in SPARK
---------------------------------------------------------------------

- Classes in SPARK are tagged records

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Classes

    package Show_Classes is

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       function Equal (Arg1, Arg2 : Int) return Boolean;
       procedure Bump (Arg : in out Int);

       type Approx_Int is new Int with record
          Precision : Natural;
       end record;
       --  implicit definition of Equal and Bump for Approx_Int

    end Show_Classes;

- Derived types are specializations of the root type

    - typically with more components

    - inheriting the methods on the parent type

    - can add their own methods


Methods in SPARK
---------------------------------------------------------------------

- Derived methods can be overriding or not

.. code:: ada compile_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Derived_Methods

    package Show_Derived_Methods is

       pragma Elaborate_Body;

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       function Equal (Arg1, Arg2 : Int) return Boolean;
       procedure Bump (Arg : in out Int);

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding function Equal (Arg1, Arg2 : Approx_Int)
                                  return Boolean;
       overriding procedure Bump (Arg : in out Approx_Int);

       not overriding procedure Blur (Arg : in out Approx_Int);

    end Show_Derived_Methods;

    package body Show_Derived_Methods is

       function Equal (Arg1, Arg2 : Int) return Boolean is
         (Arg1 = Arg2);

       procedure Bump (Arg : in out Int) is
          Next : constant Integer := (if Arg.Value < Integer'Last
                                      then Arg.Value + 1
                                      else Integer'Last);
       begin
          if Next <= Arg.Max then
             Arg.Value := Next;
          end if;
       end Bump;

       overriding function Equal (Arg1, Arg2 : Approx_Int)
                                  return Boolean is
         (Arg1 = Arg2);

       overriding procedure Bump (Arg : in out Approx_Int) is
       begin
          Bump (Int (Arg));
       end Bump;

       not overriding procedure Blur (Arg : in out Approx_Int) is
          Prev : constant Integer := (if Arg.Value > Integer'First
                                      then Arg.Value - 1
                                      else Integer'First);
       begin
          if Arg.Value >= Prev then
             Arg.Value := Prev;
          end if;
       end Blur;

    end Show_Derived_Methods;

- Method called depends on static type

.. code:: ada run_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Derived_Methods

    with Show_Derived_Methods; use Show_Derived_Methods;

    procedure Use_Derived_Methods is
       I  : Int;
       AI : Approx_Int;
    begin
       Bump (I); -- call to Int.Bump
       I.Bump; -- call to Int.Bump (object.method notation)

       Bump (AI); -- call to Approx_Int.Bump
       Bump (Int (AI)); -- call to Int.Bump
    end Use_Derived_Methods;

Dynamic dispatching in SPARK
---------------------------------------------------------------------

- Class-wide types

    - type of object that triggers dispatching

    - method called depends on dynamic type

.. code:: ada run_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Derived_Methods

    with Show_Derived_Methods; use Show_Derived_Methods;

    procedure Use_Dynamic_Dispatching is

       I  : Int;
       AI : Approx_Int;
    begin
       declare
          IC : Int'Class := Int'Class (I);
       begin
          IC.Bump; -- call to Int.Bump
       end;

       declare
          IC : Int'Class := Int'Class (AI);
       begin
          IC.Bump; -- call to Approx_Int.Bump
       end;
    end Use_Dynamic_Dispatching;

- Class-wide views of objects

    - in Ada, usually manipulated through pointers

    - in SPARK, manipulated through parameter passing

.. code:: ada run_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Derived_Methods

    with Show_Derived_Methods; use Show_Derived_Methods;

    procedure Use_Classwide_Dispatching is

       procedure Call_Bump (Arg : in out Int'Class) is
       begin
          Arg.Bump;
       end Call_Bump;

       I  : Int;
       AI : Approx_Int;

    begin
       Call_Bump (Int'Class (I));  -- calls Int.Bump(I)
       Call_Bump (Int'Class (AI)); -- calls Approx_Int.Bump(AI)
    end Use_Classwide_Dispatching;

A trivial example
~~~~~~~~~~~~~~~~~

- what is called here?

.. code:: ada run_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Trivial_Example

    procedure Show_Trivial_Example is

       package Pkg_Trivial is
          type Int is tagged record
             Min, Max, Value : Integer;
          end record;

          procedure Bump (Arg : in out Int) is null;
       end Pkg_Trivial;

       use Pkg_Trivial;

       procedure Call_Bump
         (Arg : in out Int'Class) is
       begin
          Arg.Bump;
       end Call_Bump;

    begin
       null;
    end Show_Trivial_Example;

The problems with dynamic dispatching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Control and data flow are not known statically

    - control flow – which subprogram is called when dispatching

    - data flow – what data this subprogram is accessing

    - similar to callbacks through subprogram pointers

- Avionics standard DO-178C lists 3 verification options

    - run all tests on parent type where derived type is used instead

    - cover all possible methods at dispatching calls

    - prove type substitutability (Liskov Substitution Principle aka LSP)


LSP – the SPARK solution to dynamic dispatching problems
---------------------------------------------------------------------

- Class-wide contracts on methods

    - :ada:`Pre'Class` specifies strongest precondition for the hierarchy

    - :ada:`Post'Class` specifies weakest postcondition for the hierarchy

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.LSP

    package Show_LSP is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding procedure Bump (Arg : in out Approx_Int) with
         Pre'Class  => Arg.Value > 100,
         Post'Class => Arg.Value = Arg.Value'Old;

    end Show_LSP;

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.LSP

    package Show_LSP is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding procedure Bump (Arg : in out Approx_Int) with
         Pre'Class  => True,
         Post'Class => Arg.Value = Arg.Value'Old + 10;

    end Show_LSP;

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.LSP

    package Show_LSP is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding procedure Bump (Arg : in out Approx_Int);
       --  inherited Pre'Class from Int.Bump
       --  inherited Post'Class from Int.Bump

    end Show_LSP;

Verification of dynamic dispatching calls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Class-wide contracts used for dynamic dispatching calls

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.LSP

    with Show_LSP; use Show_LSP;

    procedure Show_Dynamic_Dispatching_Verification is

       procedure Call_Bump (Arg : in out Int'Class) with
         Pre  => Arg.Value < Arg.Max - 10,
         Post => Arg.Value > Arg.Value'Old
       is
       begin
          Arg.Bump;
       end Call_Bump;

    begin
       null;
    end Show_Dynamic_Dispatching_Verification;

- LSP applies to data dependencies too

    - overriding method cannot read more global variables

    - overriding method cannot write more global variables

    - overriding method cannot have new input-output flows

    - SPARK RM defines :ada:`Global'Class` and :ada:`Depends'Class` (not
      yet implemented ⟶ use :ada:`Global` and :ada:`Depends` instead)


Class-wide contracts and data abstraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Abstraction can be used in class-wide contracts

- Typically use expression functions for abstraction

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Classwide_Contracts

    package Show_Classwide_Contracts is

       type Int is tagged private;

       function Get_Value (Arg : Int) return Integer;

       function Small (Arg : Int) return Boolean with Ghost;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Small,
         Post'Class => Arg.Get_Value > Arg.Get_Value'Old;

    private

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       function Get_Value (Arg : Int) return Integer is
         (Arg.Value);
       function Small (Arg : Int) return Boolean is
         (Arg.Value < Arg.Max - 10);

    end Show_Classwide_Contracts;

Class-wide contracts, data abstraction and overriding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Abstraction functions can be overridden freely

    - overriding needs not be weaker or stronger than overridden

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Contract_Override

    package Show_Contract_Override is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       function Small (Arg : Int) return Boolean is
         (Arg.Value < Arg.Max - 10);

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding function Small (Arg : Approx_Int) return Boolean is
         (True);

    end Show_Contract_Override;

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Contract_Override

    package Show_Contract_Override is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       function Small (Arg : Int) return Boolean is
         (Arg.Value < Arg.Max - 10);

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       function Small (Arg : Approx_Int) return Boolean is
         (Arg.Value in 1 .. 100);

    end Show_Contract_Override;

- Inherited contract reinterpreted for derived class

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Contract_Override

    package Show_Contract_Override is

       type Int is tagged record
          Min, Max, Value : Integer := 0;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding procedure Bump (Arg : in out Approx_Int);
       --  inherited Pre'Class uses Approx_Int.Small
       --  inherited Post'Class uses Approx_Int.Get_Value

    end Show_Contract_Override;

Dynamic semantics of class-wide contracts
---------------------------------------------------------------------

- Class-wide precondition is the disjunction (or) of

    - own class-wide precondition, and

    - class-wide preconditions of all overridden methods

- Class-wide postcondition is the conjunction (and) of

    - own class-wide postcondition, and

    - class-wide postconditions of all overridden methods

- Plain :ada:`Post` + class-wide :ada:`Pre` / :ada:`Post` can be used
  together

- Proof guarantees no violation of contracts at runtime

    - LSP guarantees stronger than dynamic semantics


Redispatching and Extensions_Visible aspect
---------------------------------------------------------------------

- Redispatching is dispatching after class-wide conversion

    - formal parameter cannot be converted to class-wide type when
      :ada:`Extensions_Visible` is :ada:`False`

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Contract_Override

    with Show_Contract_Override; use Show_Contract_Override;

    procedure Show_Redispatching is

       procedure Re_Call_Bump (Arg : in out Int) is
       begin
          Int'Class (Arg).Bump;
       end Re_Call_Bump;
    begin
       null;

    end Show_Redispatching;

- Aspect :ada:`Extensions_Visible` allows class-wide conversion

    - parameter mode used also for hidden components

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Contract_Override

    with Show_Contract_Override; use Show_Contract_Override;

    procedure Show_Redispatching is

       procedure Re_Call_Bump (Arg : in out Int)
         with Extensions_Visible is
       begin
          Int'Class (Arg).Bump;
       end Re_Call_Bump;
    begin
       null;

    end Show_Redispatching;

Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_01

    package OO_Example_01 is

       type Int is record
          Min, Max, Value : Integer;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

    end OO_Example_01;

This code is not correct. Class-wide contracts are only allowed on tagged
records.

Example #2
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_02

    package OO_Example_02 is

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre  => Arg.Value < Arg.Max - 10,
         Post => Arg.Value > Arg.Value'Old;

    end OO_Example_02;

This code is not correct. Plain precondition on dispatching subprogram is
not allowed in SPARK. Otherwise it would have to be both weaker and
stronger than the class-wide precondition (because they are both checked
dynamically on both plain calls and dispatching calls).

Plain postcondition is allowed, and should be stronger than class-wide
postcondition (plain postcondition used for plain calls).


Example #3
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_03

    package OO_Example_03 is

       pragma Elaborate_Body;

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       procedure Bump (Arg : in out Int) with
         Pre'Class  => Arg.Value < Arg.Max - 10,
         Post'Class => Arg.Value > Arg.Value'Old;

       type Approx_Int is new Int with record
          Precision : Natural := 0;
       end record;

       overriding procedure Bump (Arg : in out Approx_Int) with
         Post'Class => Arg.Value = Arg.Value'Old + 10;

    end OO_Example_03;

    package body OO_Example_03 is

       procedure Bump (Arg : in out Int) is
       begin
          Arg.Value := Arg.Value + 10;
       end Bump;

       overriding procedure Bump (Arg : in out Approx_Int) is
       begin
          Arg.Value := Arg.Value + 10;
       end Bump;

    end OO_Example_03;

This code is correct. Class-wide precondition of ``Int.Bump`` is inherited
by ``Approx_Int.Bump``. Class-wide postcondition of ``Approx_Int.Bump`` is
stronger than the one of ``Int.Bump``.


Example #4
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_04

    package OO_Example_04 is

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       function "+" (Arg1, Arg2 : Int) return Int with
         Pre'Class => Arg1.Min = Arg2.Min
                      and Arg1.Max = Arg2.Max;

       type Approx_Int is new Int with record
          Precision : Natural;
       end record;

       --  inherited function “+”

    end OO_Example_04;

This code is not correct. A type must be declared abstract or :ada:`"+"`
overridden.


Example #5
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_05

    package OO_Example_05 is

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       procedure Reset (Arg : out Int);

       type Approx_Int is new Int with record
          Precision : Natural;
       end record;

       --  inherited procedure Reset

    end OO_Example_05;

This code is not correct. A type must be declared abstract or ``Reset``
overridden ``Reset`` is subject to :ada:`Extensions_Visible`
:ada:`False`.


Example #6
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_06

    package OO_Example_06 is

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       procedure Reset (Arg : out Int) with Extensions_Visible;

       type Approx_Int is new Int with record
          Precision : Natural;
       end record;

       --  inherited procedure Reset

    end OO_Example_06;

    package body OO_Example_06 is

       procedure Reset (Arg : out Int) is
       begin
          Arg := Int'(Min   => -100,
                      Max   => 100,
                      Value => 0);
       end Reset;

    end OO_Example_06;

This code is not correct. High: extension of ``Arg`` is not initialized in
``Reset``.


Example #7
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_07
    :class: ada-syntax-only

    package OO_Example_07 is

       pragma Elaborate_Body;

       type Int is tagged record
          Min, Max, Value : Integer;
       end record;

       function Zero return Int;

       procedure Reset (Arg : out Int) with Extensions_Visible;

       type Approx_Int is new Int with record
          Precision : Natural;
       end record;

       overriding function Zero return Approx_Int;

       --  inherited procedure Reset

    end OO_Example_07;

    package body OO_Example_07 is

       function Zero return Int is
          ((0, 0, 0));

       procedure Reset (Arg : out Int) is
       begin
          Int'Class (Arg) := Zero;
       end Reset;

       function Zero return Approx_Int is
           ((0, 0, 0, 0));

    end OO_Example_07;

This code is correct. Redispatching ensures that ``Arg`` is fully
initialized on return.


Example #8
~~~~~~~~~~

.. code:: ada run_button main=oo_example_08.adb project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_08

    package File_System is

       type File is tagged private;

       function Closed (F : File) return Boolean;
       function Is_Open (F : File) return Boolean;

       procedure Create (F : out File) with
         Post'Class => F.Closed;

       procedure Open_Read (F : in out File) with
         Pre'Class  => F.Closed,
         Post'Class => F.Is_Open;

       procedure Close (F : in out File) with
         Pre'Class  => F.Is_Open,
         Post'Class => F.Closed;

    private
       type File is tagged record
          Closed  : Boolean := True;
          Is_Open : Boolean := False;
       end record;

       function Closed (F : File) return Boolean is
         (F.Closed);

       function Is_Open (F : File) return Boolean is
         (F.Is_Open);

    end File_System;

    package body File_System is

       procedure Create (F : out File) is
       begin
          F.Closed  := True;
          F.Is_Open := False;
       end Create;

       procedure Open_Read (F : in out File) is
       begin
          F.Is_Open := True;
       end Open_Read;

       procedure Close (F : in out File) is
       begin
          F.Closed := True;
       end Close;

    end File_System;

    with File_System; use File_System;

    procedure OO_Example_08 is

       procedure Use_File_System (F : out File'Class) is
       begin
          F.Create;
          F.Open_Read;
          F.Close;
       end Use_File_System;

    begin
       null;
    end OO_Example_08;

This code is correct. State automaton encoded in class-wide contracts is
respected.


Example #9
~~~~~~~~~~

.. code:: ada run_button main=oo_example_09.adb project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_08

    package File_System.Sync is

       type File is new File_System.File with private;

       function Is_Synchronized (F : File) return Boolean;

       procedure Create (F : out File) with
         Post'Class => F.Closed;

       procedure Open_Read (F : in out File) with
         Pre'Class  => F.Closed,
         Post'Class => F.Is_Open and F.Is_Synchronized;

       procedure Close (F : in out File) with
         Pre'Class  => F.Is_Open and F.Is_Synchronized,
         Post'Class => F.Closed;

    private
       type File is new File_System.File with record
          In_Synch : Boolean := True;
       end record;

       function Is_Synchronized (F : File) return Boolean is
         (F.In_Synch);

    end File_System.Sync;

    package body File_System.Sync is

       procedure Create (F : out File) is
       begin
          File_System.File (F).Create;
          F.In_Synch := True;
       end Create;

       procedure Open_Read (F : in out File) is
       begin
          File_System.File (F).Open_Read;
          F.In_Synch := True;
       end Open_Read;

       procedure Close (F : in out File) is
       begin
          File_System.File (F).Close;
          F.Closed := True;
       end Close;

    end File_System.Sync;

    with File_System.Sync; use File_System.Sync;

    procedure OO_Example_09 is

       procedure Use_File_System (F : out File'Class) is
       begin
          F.Create;
          F.Open_Read;
          F.Close;
       end Use_File_System;

    begin
       null;
    end OO_Example_09;

This code is not correct. Medium: class-wide precondition might be
stronger than overridden one


Example #10
~~~~~~~~~~~

.. code:: ada run_button main=oo_example_10.adb project=Courses.Advanced_SPARK.Object_Oriented_Programming.Example_08

    package File_System.Sync is

       type File is new File_System.File with private;

       function Is_Synchronized (F : File) return Boolean;

       procedure Create (F : out File) with
         Post'Class => F.Closed;

       procedure Open_Read (F : in out File) with
         Pre'Class  => F.Closed,
         Post'Class => F.Is_Open;

       procedure Close (F : in out File) with
         Pre'Class  => F.Is_Open,
         Post'Class => F.Closed;

    private
       type File is new File_System.File with record
          In_Synch : Boolean;
       end record with
         Predicate => File_System.File (File).Closed
                      or In_Synch;

       function Is_Synchronized (F : File) return Boolean is
         (F.In_Synch);

    end File_System.Sync;

    package body File_System.Sync is

       procedure Create (F : out File) is
       begin
          File_System.File (F).Create;
          F.In_Synch := True;
       end Create;

       procedure Open_Read (F : in out File) is
       begin
          File_System.File (F).Open_Read;
          F.In_Synch := True;
       end Open_Read;

       procedure Close (F : in out File) is
       begin
          File_System.File (F).Close;
          F.Closed := True;
       end Close;

    end File_System.Sync;

    with File_System.Sync; use File_System.Sync;

    procedure OO_Example_10 is

       procedure Use_File_System (F : out File'Class) is
       begin
          F.Create;
          F.Open_Read;
          F.Close;
       end Use_File_System;

    begin
       null;
    end OO_Example_10;

This code is correct. Predicate encodes the additional constraint on
opened files. Type invariants are not yet supported on tagged types in
SPARK.
