Type Contracts
=====================================================================

.. include:: ../../../global.txt

Type Contracts in Ada 2012 and SPARK 2014
---------------------------------------------------------------------

- Natural evolution in Ada from previous type constraints

    - Scalar range specifies lower and upper bounds

    - Record discriminant specifies variants of the same type

- Executable type invariants by Meyer in Eiffel (1988)

    - Part of Design by Contract ™

    - Type invariant is checked dynamically when an object is created, and
      when an exported routine of the class returns

- Ada 2012 / SPARK 2014 support strong and weak invariants

    - A strong invariant must hold all the time

    - A weak invariant must hold outside of the scope of the type


Static and Dynamic Predicates
---------------------------------------------------------------------

Static Predicate
~~~~~~~~~~~~~~~~

- Original use case for type predicates in Ada 2012

    - Supporting non-contiguous subtypes of enumerations

    - Removes the constraint to define enumeration values in an order that
      allows defining interesting subtypes

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Static_Predicate

    package Show_Static_Predicate is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;
       subtype Day_Off is Day with
         Static_Predicate => Day_Off in Wednesday | Weekend;

    end Show_Static_Predicate;

- Typical use case on scalar types for holes in range

    - e.g. floats without :ada:`0.0`

- Types with static predicate are restricted

    - Cannot be used for the index of a loop or for array index (but OK
      for value tested in case statement)


Dynamic Predicate
~~~~~~~~~~~~~~~~~

- Extension of static predicate for any property

    - Property for static predicate must compare value to static
      expressions

    - Property for dynamic predicate can be anything

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Dynamic_Predicate

    package Show_Dynamic_Predicate is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       function Check_Is_Off_In_Calendar (D : Day) return Boolean;

       subtype Day_Off is Day with
         Dynamic_Predicate => Check_Is_Off_In_Calendar (Day_Off);

    end Show_Dynamic_Predicate;

- Various typical use cases on scalar and composite types

    - Strings that start at index 1 (:ada:`My_String'First = 1`)

    - Upper bound on record component that depends on the discriminant
      value (:ada:`Length <= Capacity`)

    - Ordering property on array values (:ada:`Is_Sorted (My_Array)`)


Restrictions on Types With Dynamic Predicate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Types with dynamic predicate are restricted

    - Cannot be used for the index of a loop (same as static predicate)

    - Cannot be used as array index (same as static predicate)

    - Cannot be used for the value tested in a case statement

- No restriction on the property in Ada

    - Property can read the value of global variable (e.g.
      ``Check_Is_Off_In_Calendar``)

        - what if global variable is updated?

    - Property can even have side-effects!

- Stronger restrictions on the property in SPARK

    - Property cannot read global variables or have side-effects

    - These restrictions make it possible to prove predicates


Dynamic Checking of Predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Partly similar to other type constraints

    - Checked everywhere a range/discriminant check would be issued:
      assignment, parameter passing, type conversion, type qualification

    - ...but exception :ada:`Assertion_Error` is raised in case of
      violation

    - ...but predicates not checked by default, activated with ``-gnata``

- Static predicate does not mean verification at compile time!

.. code:: ada compile_button project=Courses.Advanced_SPARK.Type_Contracts.Static_Predicate_Verified_At_Runtime

    package Show_Static_Predicate_Verified_At_Runtime is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;
       subtype Day_Off is Day with
         Static_Predicate => Day_Off in Wednesday | Weekend;

       procedure Process_Day (This_Day : Day);

    end Show_Static_Predicate_Verified_At_Runtime;

    package body Show_Static_Predicate_Verified_At_Runtime is

       procedure Process_Day (This_Day : Day) is
          --  Predicate cannot be verified at compile time
          My_Day_Off : Day_Off := This_Day;
       begin
          --  missing implementation
          null;
       end Process_Day;

    end Show_Static_Predicate_Verified_At_Runtime;

- Property should not contain calls to functions of the type

    - These functions will check the predicate on entry, leading to an
      infinite loop

    - GNAT compiler warns about such cases


Temporary Violations of the Dynamic Predicate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Sometimes convenient to locally violate the property

    - Inside subprogram, to assign components of a record without an
      aggregate assignment

    - Violation even if no run-time check on component assignment

- Idiom is to define two types

    - First type does not have a predicate

    - Second type is a subtype of the first with the predicate

    - Conversions between these types at subprogram boundary

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Temp_Violation_Dyn_Predicate

    package Show_Temp_Violation_Dyn_Predicate is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Raw_Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record;

       subtype Week_Schedule is Raw_Week_Schedule with
         Dynamic_Predicate =>
           Week_Schedule.Day_Off /= Week_Schedule.Day_On_Duty;

    end Show_Temp_Violation_Dyn_Predicate;

Type Invariant
---------------------------------------------------------------------

- Corresponds to the weak version of invariants

    - Predicates should hold always (only enforced with SPARK proof)

    - Type invariants should only hold outside of their defining package

- Type invariant can only be used on private types

    - Either on the private declaration

    - Or on the completion of the type in the private part of the package
      (makes more sense in general, only option in SPARK)

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Type_Invariant

    package Show_Type_Invariant is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private;
    private

       type Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record with
         Type_Invariant => Day_Off /= Day_On_Duty;

       procedure Internal_Adjust (WS : in out Week_Schedule);

    end Show_Type_Invariant;

Dynamic Checking of Type Invariants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Checked on outputs of public subprograms of the package

    - Checked on results of public functions

    - Checked on (:ada:`in`) :ada:`out` parameters of public subprograms

    - Checked on variables of the type, or having a part of the type

    - Exception :ada:`Assertion_Error` is raised in case of violation

    - Not checked by default, activated with ``-gnata``

- No checking on internal subprograms!

    - Choice between predicate and type invariants depends on the need for
      such internal subprograms without checking

.. code:: ada compile_button project=Courses.Advanced_SPARK.Type_Contracts.Type_Invariant

    package Show_Type_Invariant is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private;
    private

       type Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record with
         Type_Invariant => Day_Off /= Day_On_Duty;

       procedure Internal_Adjust (WS : in out Week_Schedule);

    end Show_Type_Invariant;

    package body Show_Type_Invariant is

       procedure Internal_Adjust (WS : in out Week_Schedule) is
       begin
          WS.Day_Off := WS.Day_On_Duty;
       end Internal_Adjust;

    end Show_Type_Invariant;

Inheritance of Predicates and Type Invariants
---------------------------------------------------------------------

- Derived types inherit the predicates of their parent type

    - Similar to other type constraints like bounds

    - Allows to structure a hierarchy of subtypes, from least to most
      constrained

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Predicate_Inheritance

    package Show_Predicate_Inheritance is

       subtype String_Start_At_1 is String with
         Dynamic_Predicate => String_Start_At_1'First = 1;

       subtype String_Normalized is String_Start_At_1 with
         Dynamic_Predicate => String_Normalized'Last >= 0;

       subtype String_Not_Empty is String_Normalized with
         Dynamic_Predicate => String_Not_Empty'Length >= 1;

    end Show_Predicate_Inheritance;

- Type invariants are typically not inherited

    - A private type cannot be derived unless it is tagged

    - Special aspect :ada:`Type_Invariant'Class` preferred for tagged
      types


Other Useful Gotchas on Predicates and Type Invariants
---------------------------------------------------------------------

- GNAT defines its own aspects :ada:`Predicate` and :ada:`Invariant`

    - Predicate is the same as :ada:`Static_Predicate` if property allows
      it

    - Otherwise :ada:`Predicate` is the same as :ada:`Dynamic_Predicate`

    - :ada:`Invariant` is the same as :ada:`Type_Invariant`

- Referring to the *current object* in the property

    - The name of the type acts as the *current object* of that type

    - Components of records can be mentioned directly

- Type invariants on protected objects

    - Ada/SPARK do not define type invariants on protected objects

    - Idiom is to use a record type as unique component of the PO, and use
      a predicate for that record type


Default Initial Condition
---------------------------------------------------------------------

- Aspect defined in GNAT to state a property on default initial values of
  a private type

    - Introduced for proof in SPARK

    - GNAT introduces a dynamic check when ``-gnata`` is used

    - Used in the formal containers library to state that containers are
      initially empty

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Default_Init_Cond

    with Ada.Containers;

    package Show_Default_Init_Cond is

       type Count_Type is new Ada.Containers.Count_Type;

       type List (Capacity : Count_Type) is private with
          Default_Initial_Condition => Is_Empty (List);

       function Is_Empty (L : List) return Boolean;

    private

       type List (Capacity : Count_Type) is null record;
       --  missing implementation...

    end Show_Default_Init_Cond;

- Can also be used without a property for SPARK analysis

    - No argument specifies that the value is fully default initialized

    - Argument null specifies that there is no default initialization


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada manual_chop no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_01
    :class: ada-nocheck

    !example_01.ads
    package Example_01 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;

       subtype Day_Off is Day range Wednesday | Weekend;

    end Example_01;

This code is not correct. The syntax of range constraints does not allow
sets of values. A predicate should be used instead.


Example #2
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_02

    package Example_02 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;

       subtype Day_Off is Weekend with
         Static_Predicate => Day_Off in Wednesday | Weekend;

    end Example_02;

This code is not correct. This is accepted by GNAT, but result is not the
one expected by the user. ``Day_Off`` has the same constraint as
``Weekend``.


Example #3
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_03

    package Example_03 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;

       subtype Day_Off is Day with
         Dynamic_Predicate => Day_Off in Wednesday | Weekend;

    end Example_03;

This code is correct. It is valid to use a :ada:`Dynamic_Predicate` where
a :ada:`Static_Predicate` would be allowed.


Example #4
~~~~~~~~~~

.. code:: ada run_button project=Courses.Advanced_SPARK.Type_Contracts.Example_04

    package Week is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       subtype Weekend is Day range Saturday .. Sunday;

       subtype Day_Off is Day with
         Static_Predicate => Day_Off in Wednesday | Weekend;

    end Week;

    with Week; use Week;

    procedure Example_04 is

       function Next_Day_Off (D : Day_Off) return Day_Off is
       begin
          case D is
             when Wednesday => return Saturday;
             when Saturday  => return Sunday;
             when Sunday    => return Wednesday;
          end case;
       end Next_Day_Off;

    begin
       null;
    end Example_04;

This code is correct. It is valid to use a type with
:ada:`Static_Predicate` for the value tested in a case statement. This is
not true for :ada:`Dynamic_Predicate`.


Example #5
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_05

    package Example_05 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private with
         Type_Invariant => Valid (Week_Schedule);

       function Valid (WS : Week_Schedule) return Boolean;

    private
       type Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record;

       function Valid (WS : Week_Schedule) return Boolean is
         (WS.Day_Off /= WS.Day_On_Duty);

    end Example_05;

This code is correct. It is valid in Ada because the type invariant is not
checked on entry or return from ``Valid``. Also, function ``Valid`` is
visible from the type invariant (special visibility in contracts). But it
is invalid in SPARK, where private declaration cannot hold a type
invariant. The reason is that the type invariant is assumed in the
precondition of public functions for proof. That would lead to circular
reasoning if ``Valid`` could be public.


Example #6
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_06

    package Example_06 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private;

    private

       type Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record with
         Type_Invariant => Valid (Week_Schedule);

       function Valid (WS : Week_Schedule) return Boolean is
         (WS.Day_Off /= WS.Day_On_Duty);

    end Example_06;

This code is correct. This version is valid in both Ada and SPARK.


Example #7
~~~~~~~~~~

.. code:: ada manual_chop no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_07
    :class: ada-nocheck

    !example_07.ads
    package Example_07 is

       subtype Sorted_String is String with
         Dynamic_Predicate =>
           (for all Pos in Sorted_String'Range =>
              Sorted_String (Pos) <= Sorted_String (Pos + 1));

       subtype Unique_String is String with
         Dynamic_Predicate =>
           (for all Pos1, Pos2 in Unique_String'Range =>
              Unique_String (Pos1) /= Unique_String (Pos2));

       subtype Unique_Sorted_String is String with
         Dynamic_Predicate =>
           Unique_Sorted_String in Sorted_String and then
           Unique_Sorted_String in Unique_String;

    end Example_07;

This code is not correct. There are 3 problems in this code:

- there is a run-time error on the array access in ``Sorted_String``;

- quantified expression defines only one variable;

- the property in ``Unique_String`` is true only for the empty string.


Example #8
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_08

    package Example_08 is

       subtype Sorted_String is String with
         Dynamic_Predicate =>
           (for all Pos in Sorted_String'First ..
              Sorted_String'Last - 1 =>
                Sorted_String (Pos) <= Sorted_String (Pos + 1));

       subtype Unique_String is String with
         Dynamic_Predicate =>
           (for all Pos1 in Unique_String'Range =>
              (for all Pos2 in Unique_String'Range =>
                 (if Pos1 /= Pos2 then
                      Unique_String (Pos1) /= Unique_String (Pos2))));

       subtype Unique_Sorted_String is String with
         Dynamic_Predicate =>
           Unique_Sorted_String in Sorted_String and then
           Unique_Sorted_String in Unique_String;

    end Example_08;

This code is correct. This is a correct version in Ada. For proving AoRTE
in SPARK, one will need to change slightly the property of
``Sorted_String``.


Example #9
~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_09

    package Example_09 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private with
         Default_Initial_Condition => Valid (Week_Schedule);

       function Valid (WS : Week_Schedule) return Boolean;

    private

       type Week_Schedule is record
          Day_Off, Day_On_Duty : Day;
       end record;

       function Valid (WS : Week_Schedule) return Boolean is
         (WS.Day_Off /= WS.Day_On_Duty);

    end Example_09;

This code is not correct. The default initial condition is not satisfied.


Example #10
~~~~~~~~~~~

.. code:: ada no_button project=Courses.Advanced_SPARK.Type_Contracts.Example_10

    package Example_10 is

       type Day is (Monday,   Tuesday, Wednesday,
                    Thursday, Friday,  Saturday,
                    Sunday);

       type Week_Schedule is private with
         Default_Initial_Condition => Valid (Week_Schedule);

       function Valid (WS : Week_Schedule) return Boolean;

    private

       type Week_Schedule is record
          Day_Off     : Day := Wednesday;
          Day_On_Duty : Day := Friday;
       end record;

       function Valid (WS : Week_Schedule) return Boolean is
         (WS.Day_Off /= WS.Day_On_Duty);

    end Example_10;

This code is correct. This is a correct version, which can be proved with
SPARK.
