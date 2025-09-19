Systems Programming
=====================================================================

.. include:: ../../../global.txt

Type Contracts in Ada 2012 and SPARK 2014
---------------------------------------------------------------------

Systems Programming – What is it?
---------------------------------------------------------------------

- Bare metal programming

    - bare board applications (no Operating System)

    - Operating Systems (ex: Muen separation kernel)

    - device drivers (ex: Ada Drivers Library)

    - communication stacks (ex: AdaCore TCP/IP stack)

- Specifics of Systems Programming

    - direct access to hardware: registers, memory, etc.

    - side-effects (yes!)

    - efficiency is paramount (sometimes real-time even)

    - hard/impossible to debug


Systems Programming – How can SPARK help?
---------------------------------------------------------------------

- SPARK is a Systems Programming language

    - same features as Ada for accessing hardware (representation clauses,
      address clauses)

    - as efficient as Ada or C

- Side-effects can be modeled in SPARK

    - reads and writes to memory-mapped devices are modeled

    - concurrent interactions with environment are modeled

- SPARK can help catch problems by static analysis

    - correct flows, initialization, concurrent accesses

    - absence of run-time errors and preservation of invariants


Systems Programming – A trivial example
---------------------------------------------------------------------

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Trivial_Sys_Prog

    package Show_Trivial_Sys_Prog is

       Y : Integer;

       --  Y'Address could be replaced by any
       --  external address
       X : Integer with Volatile,
         Address => Y'Address;

       procedure Get (Val : out Integer)
         with Global  => (In_Out => X),
         Depends => (Val => X,
                     X   => X);

    end Show_Trivial_Sys_Prog;

    package body Show_Trivial_Sys_Prog is

       procedure Get (Val : out Integer) is
       begin
          Val := X;
       end Get;

    end Show_Trivial_Sys_Prog;

- Comments:

    - ``X`` is volatile

    - ``X`` is also an output; output ``X`` depends on input ``X``

    - ``X`` is only read


Volatile Variables and Volatile Types
---------------------------------------------------------------------

- Variables whose reads/writes cannot be optimized away

- Identified through multiple aspects (or pragmas)

    - aspect :ada:`Volatile`

    - but also aspect :ada:`Atomic`

    - and GNAT aspect :ada:`Volatile_Full_Access`

    - all the above aspects can be set on type or object

- Other aspects are useful on volatile variables

    - aspect :ada:`Address` to specify location in memory

    - aspect :ada:`Import` to skip definition/initialization

.. code-block:: ada

    type T is new Integer with Volatile;

    X : Integer with Atomic, Import, Address => ... ;


Flavors of Volatile Variables
---------------------------------------------------------------------

Using ``Async_Readers`` / ``Async_Writers``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Boolean aspects describing asynchronous behavior

    - :ada:`Async_Readers` if variable may be read asynchronously

    - :ada:`Async_Writers` if variable may be written asynchronously

- Effect of :ada:`Async_Readers` on flow analysis

- Effect of :ada:`Async_Writers` on flow analysis & proof

    - always initialized,  always has an unknown value

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    package Volatile_Vars is

       pragma Elaborate_Body;

       Ext : array (1 .. 2) of Integer;

       X : Integer with Volatile,
         Address => Ext (1)'Address,
         Async_Readers;

       Y : Integer with Volatile,
         Address => Ext (2)'Address,
         Async_Writers;

       procedure Set;
    end Volatile_Vars;

    package body Volatile_Vars is

       procedure Set is
          U, V : constant Integer := Y;
       begin
          pragma Assert (U = V);
          X := 0;
          X := 1;
       end Set;
    begin
       Ext := (others => 0);
    end Volatile_Vars;

.. code:: ada run_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    with Volatile_Vars;

    procedure Show_Volatile_Vars is
    begin
       Volatile_Vars.Set;
    end Show_Volatile_Vars;


Using ``Effective_Reads`` / ``Effective_Writes``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Boolean aspects distinguishing values & sequences

    - :ada:`Effective_Reads` if reading the variable has an effect on its
      value

    - :ada:`Effective_Writes` if writing the variable has an effect on its
      value

- Effect of both on proof and flow dependencies

    - Final value of variable is seen as a sequence of values it took

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    package Volatile_Vars is

       pragma Elaborate_Body;

       Ext : array (1 .. 2) of Integer;

       X : Integer with Volatile,
         Address => Ext (1)'Address,
         Async_Readers,
         Effective_Writes;

       Y : Integer with Volatile,
         Address => Ext (2)'Address,
         Async_Writers,
         Effective_Reads;

       procedure Set with
          Depends => (X => Y,
                      Y => Y);
    end Volatile_Vars;

    package body Volatile_Vars is

       procedure Set is
       begin
          X := Y;
          X := 0;
       end Set;

    begin
       Ext := (others => 0);
    end Volatile_Vars;

.. code:: ada run_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    with Volatile_Vars;

    procedure Show_Volatile_Vars is
    begin
       Volatile_Vars.Set;
    end Show_Volatile_Vars;


Combinations of Flavors of Volatile Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- All four flavors can be set independently

    - Default for Volatile/Atomic is all four :ada:`True`

    - When some aspects set, all others default to :ada:`False`

- Only half the possible combinations are legal

    - :ada:`Async_Readers` and/or :ada:`Async_Writers` is set

    - :ada:`Effective_Reads = True` forces :ada:`Async_Writers = True`

    - :ada:`Effective_Writes = True` forces :ada:`Async_Readers = True`

    - sensor: :ada:`AW=True`

    - actuator: :ada:`AR=True`

    - input port: :ada:`AW=True`, :ada:`ER=True`

    - output port: :ada:`AR=True`, :ada:`EW=True`


Constraints on Volatile Variables
---------------------------------------------------------------------

- Volatile variables must be defined at library level

- Expressions (and functions) cannot have side-effects

    - read of variable with :ada:`AW=True` must appear alone on *rhs* of
      assign

    - a function cannot read a variable with :ada:`ER=True`

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    package Volatile_Vars is

       pragma Elaborate_Body;

       Ext : array (1 .. 4) of Integer;

       AR : Integer with Volatile,
         Address => Ext (1)'Address,
         Async_Readers;

       AW : Integer with Volatile,
         Address => Ext (2)'Address,
         Async_Writers;

       ER : Integer with Volatile,
         Address => Ext (3)'Address,
         Async_Writers,
         Effective_Reads;

       EW : Integer with Volatile,
         Address => Ext (4)'Address,
         Async_Readers,
         Effective_Writes;

       procedure Read_All;

       function Read_ER return Integer;

       procedure Set (V : Integer);

    end Volatile_Vars;

    package body Volatile_Vars is

       procedure Read_All is
          Tmp : Integer := 0;
       begin
          Tmp := Tmp + AR;
          Tmp := Tmp + AW;
          EW := Tmp;
          Set (ER);
       end Read_All;

       function Read_ER return Integer is
          Tmp : Integer := ER;
       begin
          return Tmp;
       end Read_ER;

       procedure Set (V : Integer) is
       begin
          AW := V;
       end Set;

    begin
       Ext := (others => 0);
    end Volatile_Vars;

.. code:: ada run_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    with Volatile_Vars;

    procedure Show_Volatile_Vars is
       V : Integer;
    begin
       Volatile_Vars.Read_All;
       V := Volatile_Vars.Read_ER;
    end Show_Volatile_Vars;

- Comments:

    - AW not alone on rhs

    - ER not alone on rhs

    - ER output of Read_ER


Constraints on Volatile Functions
---------------------------------------------------------------------

- Functions should have mathematical interpretation

    - a function reading a variable with :ada:`AW=True` is marked as
      volatile with aspect :ada:`Volatile_Function`

    - calls to volatile functions are restricted like reads of
      :ada:`Async_Writers`

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    package Volatile_Vars is

       pragma Elaborate_Body;

       Ext : array (1 .. 4) of Integer;

       AR : Integer with Volatile,
         Address => Ext (1)'Address,
         Async_Readers;

       AW : Integer with Volatile,
         Address => Ext (2)'Address,
         Async_Writers;

       ER : Integer with Volatile,
         Address => Ext (3)'Address,
         Async_Writers,
         Effective_Reads;

       EW : Integer with Volatile,
         Address => Ext (4)'Address,
         Async_Readers,
         Effective_Writes;

       function Read_Non_Volatile
         return Integer;

       function Read_Volatile
         return Integer
         with Volatile_Function;

       function Read_ER
         return Integer
         with Volatile_Function;

    end Volatile_Vars;

    package body Volatile_Vars is

       function Read_Non_Volatile
         return Integer is
          Tmp : Integer := 0;
       begin
          --  reads AR, AW, EW
          --  ERROR: not a volatile function
          Tmp := Tmp + AR;
          Tmp := Tmp + AW;
          Tmp := Tmp + EW;

          return Tmp;
       end Read_Non_Volatile;

       function Read_Volatile
         return Integer is
          Tmp : Integer := 0;
       begin
          --  reads AR, AW, EW
          --  OK for volatile function
          Tmp := Tmp + AR;
          Tmp := Tmp + AW;
          Tmp := Tmp + EW;

          return Tmp;
       end Read_Volatile;

       function Read_ER
         return Integer is
          Tmp : Integer := ER;
       begin
          --  reads ER
          --  ERROR: ER output of Read_ER
          return Tmp;
       end Read_ER;

    begin
       Ext := (others => 0);
    end Volatile_Vars;

.. code:: ada run_button project=Courses.Advanced_SPARK.Systems_Programming.Volatile_Vars

    with Volatile_Vars;

    procedure Show_Volatile_Vars is
       V : Integer;
    begin
       V := Volatile_Vars.Read_Non_Volatile;
       V := Volatile_Vars.Read_Volatile;
       V := Volatile_Vars.Read_ER;
    end Show_Volatile_Vars;

State Abstraction on Volatile Variables
---------------------------------------------------------------------

- Abstract state needs to be identified as :ada:`External`

- Flavors of volatility can be specified

    - Default if none specified is all True

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.State_Abstraction_1

    package P1 with
      Abstract_State => (S with External)
    is
       procedure Process (Data : out Integer) with
         Global => (In_Out => S);

    end P1;

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.State_Abstraction_2

    package P2 with
      Abstract_State => (S with External =>
                           (Async_Writers,
                            --  OK if refined into AW, ER
                            Effective_Reads)
                            --  not OK if refined into AR, EW
                        )
    is
       procedure Process (Data : out Integer) with
         Global => (In_Out => S);

    end P2;


Constraints on Address Attribute
---------------------------------------------------------------------

- Address of volatile variable can be specified

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Address_Attribute

    package Show_Address_Attribute is

       Ext : array (1 .. 2) of Integer;

       X : Integer with Volatile,
         Address => Ext (1)'Address;

       Y : Integer with Volatile;
       for Y'Address use Ext (2)'Address;

    end Show_Address_Attribute;

- Address attribute not allowed in expressions

- Overlays are allowed

    - GNATprove does not check absence of overlays

    - GNATprove does not model the resulting aliasing

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Address_Overlay

    procedure Show_Address_Overlay is

       X : Integer := 1;
       Y : Integer := 0
         with Address => X'Address;

       pragma Assert (X = 1);
       --  assertion wrongly proved
    begin
       null;

    end Show_Address_Overlay;

Can something be known of volatile variables?
---------------------------------------------------------------------

- Variables with :ada:`Async_Writers` have no known value

- ... but they have a known type!

    - type range, ex: :ada:`0 .. 360`

    - type predicate, ex: :ada:`0 .. 15 | 17 .. 42 | 43 .. 360`

- Variables without :ada:`Async_Writers` have a known value

- GNATprove also assumes all values are valid (:ada:`X'Valid`)

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Provable_Volatile_Var

    package Show_Provable_Volatile_Var is

       X : Integer with Volatile, Async_Readers;

       procedure Read_Value;

    end Show_Provable_Volatile_Var;

    package body Show_Provable_Volatile_Var is

       procedure Read_Value is
       begin
          X := 42;
          pragma Assert (X = 42);
          --  proved!
       end Read_Value;

    end Show_Provable_Volatile_Var;

Other Concerns in Systems Programming
---------------------------------------------------------------------

- Software startup state ⟶ elaboration rules

    - SPARK follows Ada static elaboration model

    - ... with additional constraints for ensuring correct initialization

    - ... but GNATprove follows the relaxed GNAT static elaboration

- Handling of faults ⟶ exception handling

    - raising exceptions is allowed in SPARK

    - ... but exception handlers are :ada:`SPARK_Mode => Off`

    - ... typically the last-chance-handler is used instead

- Concurrency inside the application ⟶ tasking support

    - Ravenscar and Extended_Ravenscar profiles supported in SPARK


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_01
    :class: ada-expect-compile-error

    package Example_01 is

       Ext : Integer;

       X   : Integer with Volatile,
         Address => Ext'Address;

       procedure Get (Val : out Integer)
         with Global  => (Input => X),
         Depends => (Val => X);

    end Example_01;

    package body Example_01 is

       procedure Get (Val : out Integer) is
       begin
          Val := X;
       end Get;

    end Example_01;

This code is not correct. ``X`` has :ada:`Effective_Reads` set by default,
hence it is also an output.


Example #2
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_02
    :class: ada-expect-compile-error

    package Example_02 is

       Ext : Integer;

       X : Integer with Volatile, Address => Ext'Address,
         Async_Readers, Async_Writers, Effective_Writes;

       procedure Get (Val : out Integer)
         with Global  => (Input => X),
         Depends => (Val => X);

    end Example_02;

    package body Example_02 is

       procedure Get (Val : out Integer) is
       begin
          Val := X;
       end Get;

    end Example_02;

This code is correct. ``X`` has :ada:`Effective_Reads = False`, hence it
is only an input.


Example #3
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_03

    package Example_03 is

       Speed : Float with Volatile, Async_Writers;
       Motor : Float with Volatile, Async_Readers;

       procedure Adjust with
         Depends => (Motor =>+ Speed);

    end Example_03;

    package body Example_03 is

       procedure Adjust is
          Cur_Speed : constant Float := Speed;
       begin
          if abs Cur_Speed > 100.0 then
             Motor := Motor - 1.0;
          end if;
       end Adjust;

    end Example_03;

This code is correct. ``Speed`` is an input only, ``Motor`` is both an
input and output. Note how the current value of ``Speed`` is first copied
to be tested in a larger expression.


Example #4
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_04

    package Example_04 is

       Raw_Data : Float with Volatile,
         Async_Writers, Effective_Reads;
       Data     : Float with Volatile,
         Async_Readers, Effective_Writes;

       procedure Smooth with
         Depends => (Data => Raw_Data);

    end Example_04;

    package body Example_04 is

       procedure Smooth is
          Data1 : constant Float := Raw_Data;
          Data2 : constant Float := Raw_Data;
       begin
          Data := Data1;
          Data := (Data1 + Data2) / 2.0;
          Data := Data2;
       end Smooth;

    end Example_04;

This code is not correct. ``Raw_Data`` has :ada:`Effective_Reads` set,
hence it is also an output.

Example #5
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Systems_Programming.Example_05

    package Example_05 is

       type Regval is new Integer with Volatile;
       type Regnum is range 1 .. 32;
       type Registers is array (Regnum) of Regval;

       Regs : Registers with Async_Writers, Async_Readers;

       function Reg (R : Regnum) return Integer is
         (Integer (Regs (R))) with Volatile_Function;

    end Example_05;

This code is not correct. ``Regs`` has :ada:`Async_Writers` set, hence it
cannot appear as the expression in an expression function.


Example #6
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_06

    package Example_06 is

       type Regval is new Integer with Volatile;
       type Regnum is range 1 .. 32;
       type Registers is array (Regnum) of Regval;

       Regs : Registers with Async_Writers, Async_Readers;

       function Reg (R : Regnum) return Integer
         with Volatile_Function;

    end Example_06;

    package body Example_06 is

       function Reg (R : Regnum) return Integer is
          V : Regval := Regs (R);
       begin
          return Integer (V);
       end Reg;

    end Example_06;

This code is not correct. ``Regval`` is a volatile type, hence variable
``V`` is volatile and cannot be declared locally.


Example #7
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_07

    package Example_07 is

       type Regval is new Integer with Volatile;
       type Regnum is range 1 .. 32;
       type Registers is array (Regnum) of Regval;

       Regs : Registers with Async_Writers, Async_Readers;

       function Reg (R : Regnum) return Integer
         with Volatile_Function;

    end Example_07;

    package body Example_07 is

       function Reg (R : Regnum) return Integer is
       begin
          return Integer (Regs (R));
       end Reg;

    end Example_07;

This code is correct. ``Regs`` has :ada:`Effective_Reads = False` hence
can be read in a function. Function ``Reg`` is marked as volatile with
aspect :ada:`Volatile_Function`. No volatile variable is declared locally.


Example #8
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_08

    package Example_08 with
      Abstract_State => (State with External),
        Initializes => State
    is
       procedure Dummy;
    end Example_08;

    package body Example_08 with
      Refined_State => (State => (X, Y, Z))
    is
       X : Integer with Volatile, Async_Readers;
       Y : Integer with Volatile, Async_Writers;
       Z : Integer := 0;

       procedure Dummy is
       begin
          null;
       end Dummy;

    end Example_08;

This code is not correct. ``X`` has :ada:`Async_Writers = False`, hence is
not considered as always initialized. As aspect :ada:`Initializes`
specifies that ``State`` should be initialized after elaboration, this is
an error. Note that is allowed to bundle volatile and non-volatile
variables in an external abstract state.


Example #9
~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_09

    package Example_09 is

       type Pair is record
          U, V : Natural;
       end record
         with Predicate => U /= V;

       X : Pair with Atomic, Async_Readers, Async_Writers;

       function Max return Integer with
         Volatile_Function,
         Post => Max'Result /= 0;

    end Example_09;

    package body Example_09 is

       function Max return Integer is
          Val1 : constant Natural := X.U;
          Val2 : constant Natural := X.V;
       begin
          return Natural'Max (Val1, Val2);
       end Max;

    end Example_09;

This code is not correct. ``X`` has :ada:`Async_Writers` set, hence it may
have been written between the successive reads of ``X.U`` and ``X.V``.


Example #10
~~~~~~~~~~~

.. code:: ada compile_button project=Courses.Advanced_SPARK.Systems_Programming.Example_10

    package Example_10 is

       type Pair is record
          U, V : Natural;
       end record
         with Predicate => U /= V;

       X : Pair with Atomic, Async_Readers, Async_Writers;

       function Max return Integer with
         Volatile_Function,
         Post => Max'Result /= 0;

    end Example_10;

    package body Example_10 is

       function Max return Integer is
          P    : constant Pair := X;
          Val1 : constant Natural := P.U;
          Val2 : constant Natural := P.V;
       begin
          return Natural'Max (Val1, Val2);
       end Max;

    end Example_10;

This code is correct. Values of ``P.U`` and ``P.V`` are provably
different, and the postcondition is proved.
