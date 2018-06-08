Design by contracts
===================

.. role:: ada(code)
   :language: ada

Contracts are used in programming to specify expectations. For example,
parameter modes of a subprogram can be viewed as a form of contract.
When the specification of subprogram ``Op`` declares a parameter using
:ada:`in` mode, the caller of ``Op`` knows that the :ada:`in` argument
won't be changed by ``Op``. In other words, the caller expects that ``Op``
doesn't modify the argument it's providing, but rather just read the
information stored in the argument. Constraints and subtypes are other
examples of contracts. In summary, these elements allow for improve the
consistency of the application.

In general, design-by-contract programming refers to techniques that
include pre- and postconditions, type invariants, and subtype predicates.
We will look into those topics in this section.

Pre and postconditions
----------------------

Pre and postconditions refer to expectations regarding input and output
parameters of subprograms, and return value of functions. If we say that
certain requirements should be met before calling a subprogram ``Op``,
we're talking about preconditions. Similarly, if certain requirements
should be met after a call to the subprogram ``Op``, we're talking about
postconditions.

Pre and postconditions are specified by using a :ada:`with` clause in the
subprogram declaration. A :ada:`with Pre => <condition>` clause
specifies a precondition, whereas a :ada:`with Post => <condition>` clause
specifies a postcondition.

The following code shows an example of preconditions:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Elementary_Functions;

    procedure Show_Simple_Precondition is
       pragma Assertion_Policy (Pre  => Check);

       procedure DB_Entry (Name : String;
                           Age  : Natural)
         with Pre => Name'Length > 0
       is
       begin
          --  Missing implementation
          null;
       end DB_Entry;
    begin
       DB_Entry ("John", 30);
       DB_Entry ("",     21);  --  Precondition will fail!
    end Show_Simple_Precondition;

In this example, we want to ensure that the name field in database doesn't
contain an empty string. We can implement that by using a precondition
that ensures that the length of the string used for the :ada:`Name`
parameter in the :ada:`DB_Entry` procedure is greater than zero. If
another subprogram attempts to call the :ada:`DB_Entry` procedure with an
empty string for the :ada:`Name` parameter, the call will fail because the
precondition is not met.

The following code shows an example of a postcondition:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Elementary_Functions;

    procedure Show_Simple_Postcondition is
       pragma Assertion_Policy (Post => Check);

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1
         with Size => 8;

       type Int_8_Array is array (Integer range <>) of Int_8;

       function Double (A : Int_8) return Int_8 is
         (A * A)
         with Post => Double'Result > A;

       procedure Double (A : in out Int_8_Array)
         with Post => (for all I in A'Range =>
                         A (I) = A'Old (I) * A'Old (I))
       is
       begin
          for V of A loop
             V := Double (V);
          end loop;
       end Double;

       V : Int_8_Array := (9, 10, 11);
    begin
       for E of V loop
          Put_Line ("Original: " & Int_8'Image (E));
       end loop;
       New_Line;

       Double (V);
       for E of V loop
          Put_Line ("Double:   " & Int_8'Image (E));
       end loop;
    end Show_Simple_Postcondition;

In this example, we declare a signed 8-bit type :ada:`Int_8` and an array
of that type (:ada:`Int_8_Array`). We want to ensure that, when calling
the procedure :ada:`Double` for a :ada:`Int_8_Array`, each element of the
array will be doubled. This is implemented by a postcondition that uses
a :ada:`for all` expression. The postcondition also makes use of the
original value of the parameter before the call. The :ada:`'Old` attribute
is used in this case to retrieve the original value.

Also, we want to ensure that, in calls to the
:ada:`Double` function for the :ada:`Int_8` type, the result will be
greater than the input value. This is implemented by a postcondition that
uses the :ada:`'Result` attribute of the function and compares to the
input value.

We can use pre and postconditions at the same time in the declaration of
a subprogram. For example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Elementary_Functions;

    procedure Show_Simple_Contract is
       pragma Assertion_Policy (Pre  => Check);
       pragma Assertion_Policy (Post => Check);

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1
         with Size => 8;

       function Double (A : Int_8) return Int_8 is
         (A * A)
         with
              Pre  => (Integer'Size >= Int_8'Size * 2 and
                       Integer (A) * Integer (A) < Integer (Int_8'Last)),
              Post => Double'Result > A;

       V : Int_8;
    begin
       V := Double (11);
       Put_Line ("Double of 11 is " & Int_8'Image (V));

       V := Double (12);   --  Precondition will fail...
       Put_Line ("Double of 12 is " & Int_8'Image (V));
    end Show_Simple_Contract;

In this example, we want to ensure  that, in calls to the
:ada:`Double` function for the :ada:`Int_8` type, the input value will not
overflow when calling the function. This is implemented by converting
the input value to the :ada:`Integer` type, which is used to store the
temporary calculation, and check if the result is still in the appropriate
range. The postcondition is still the same as in the previous example.

Type invariants
---------------

Type invariants are used to define expectations regarding private types
declared in a package. For example, if a type ``T`` from a package ``P``
has a type invariant, this ensures that operations on objects of type
``T`` will always be consistent. They can be viewed as a kind of
post-condition for types.

Type invariants are specified by using a
:ada:`with Type_Invariant => <property>` clause. Similarly to pre and
postconditions, the *property* defines a condition that allows us to check
if an element of type ``T`` is conformant to the requirements.

Let's look at an example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO;           use Ada.Text_IO;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Calendar;          use Ada.Calendar;
    with Ada.Containers.Vectors;

    procedure Show_Type_Invariant is
       pragma Assertion_Policy (Type_Invariant => Check);

       package Courses is
          type Course is private
            with Type_Invariant => Check (Course);

          type Course_Container is private;

          procedure Add (CC         : in out Course_Container;
                         C          : Course);

          function Init (Name       : String;
                         Start_Date : Time;
                         End_Date   : Time)   return Course;

          function Check (C         : Course) return Boolean;

       private
          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record;

          function Check (C         : Course) return Boolean is
            (C.Start_Date <= C.End_Date);

          package Course_Vectors is new Ada.Containers.Vectors
            (Index_Type   => Natural,
             Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container;
                         C  : Course) is
          begin
             CC.V.Append (C);
          end Add;

          function Init (Name       : String;
                         Start_Date : Time;
                         End_Date   : Time) return Course is
          begin
             return Course'(Name       => To_Unbounded_String (Name),
                            Start_Date => Start_Date,
                            End_Date   => End_Date);
          end Init;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Init (Name       => "Intro to Photography",
                  Start_Date => Time_Of (2018, 5, 1),
                  End_Date   => Time_Of (2018, 5, 10)));

       --  This should trigger an error in the type-invariant check
       Add (CC,
            Init (Name       => "Intro to Video Recording",
                  Start_Date => Time_Of (2019, 5, 1),
                  End_Date   => Time_Of (2018, 5, 10)));
    end Show_Type_Invariant;

In this example, the package :ada:`Courses` defines a type :ada:`Course`
for individual courses, and a type :ada:`Course_Container` that contains
all courses. We want to ensure that the start date for every course is not
set to a date after the end date. This is implemented in the function
:ada:`Check`. In order to enforce this rule, we declare a type invariant
for the :ada:`Course` that calls the :ada:`Check` for every object. For
example, in the call to the :ada:`Init` function, :ada:`Check` will be
called during the object creation to ensure that they meet our
requirements.
