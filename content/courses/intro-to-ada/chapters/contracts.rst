Design by contracts
===================

.. include:: ../../../global.txt

Contracts are used in programming to codify expectations. Parameter modes
of a subprogram can be viewed as a simple form of contracts. When the
specification of subprogram :ada:`Op` declares a parameter using :ada:`in`
mode, the caller of :ada:`Op` knows that the :ada:`in` argument won't be
changed by :ada:`Op`. In other words, the caller expects that :ada:`Op` doesn't
modify the argument it's providing, but just reads the information stored
in the argument. Constraints and subtypes are other examples of
contracts. In general, these specifications improve the consistency of the
application.

*Design-by-contract* programming refers to techniques that include pre- and
postconditions, subtype predicates, and type invariants. We study those
topics in this chapter.

Pre- and postconditions
-----------------------

Pre- and postconditions provide expectations regarding input and output
parameters of subprograms and return value of functions. If we say that
certain requirements must be met before calling a subprogram :ada:`Op`, those
are preconditions. Similarly, if certain requirements must be met after a
call to the subprogram :ada:`Op`, those are postconditions. We can think of
preconditions and postconditions as promises between the subprogram caller
and the callee: a precondition is a promise from the caller to the callee,
and a postcondition is a promise in the other direction.

Pre- and postconditions are specified using an aspect clause in the
subprogram declaration. A :ada:`with Pre => <condition>` clause specifies a
precondition and a :ada:`with Post => <condition>` clause specifies a
postcondition.

The following code shows an example of preconditions:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Simple_Precondition
    :class: ada-run-expect-failure

    procedure Show_Simple_Precondition is

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

       --  Precondition will fail!
       DB_Entry ("",     21);
    end Show_Simple_Precondition;

In this example, we want to prevent the name field in our database from
containing an empty string. We implement this requirement by using a
precondition requiring that the length of the string used for the
:ada:`Name` parameter of the :ada:`DB_Entry` procedure is greater than
zero. If the :ada:`DB_Entry` procedure is called with an empty string for
the :ada:`Name` parameter, the call will fail because the precondition is
not met.

.. admonition:: In the GNAT toolchain

    GNAT handles pre- and postconditions by generating runtime assertions for
    them. By default, however, assertions aren't enabled. Therefore, in order
    to check pre- and postconditions at runtime, you need to enable assertions
    by using the `-gnata` switch.

Before we get to our next example, let's briefly discuss quantified
expressions, which are quite useful in concisely writing pre- and
postconditions. Quantified expressions return a Boolean value indicating
whether elements of an array or container match the expected
condition. They have the form: :ada:`(for all I in A'Range => <condition on
A(I)>`, where :ada:`A` is an array and :ada:`I` is an index.  Quantified
expressions using :ada:`for all` check whether the condition is true for
every element. For example:

.. code-block:: ada
    :class: ada-nocheck

    (for all I in A'Range => A (I) = 0)

This quantified expression is only true when all elements of the array
:ada:`A` have a value of zero.

Another kind of quantified expressions uses :ada:`for some`. The form
looks similar: :ada:`(for some I in A'Range => <condition on
A(I)>`. However, in this case the qualified expression tests whether the
condition is true only on *some* elements (hence the name) instead of all
elements.

We illustrate postconditions using the following example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Simple_Postcondition

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Postcondition is

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

       type Int_8_Array is
         array (Integer range <>) of Int_8;

       function Square (A : Int_8) return Int_8 is
         (A * A)
           with Post => (if abs A in 0 | 1
                         then Square'Result = abs A
                         else Square'Result > A);

       procedure Square (A : in out Int_8_Array)
         with Post => (for all I in A'Range =>
                         A (I) = A'Old (I) *
                                 A'Old (I))
       is
       begin
          for V of A loop
             V := Square (V);
          end loop;
       end Square;

       V : Int_8_Array := (-2, -1, 0, 1, 10, 11);
    begin
       for E of V loop
          Put_Line ("Original: "
                    & Int_8'Image (E));
       end loop;
       New_Line;

       Square (V);
       for E of V loop
          Put_Line ("Square:   "
                    & Int_8'Image (E));
       end loop;
    end Show_Simple_Postcondition;

We declare a signed 8-bit type :ada:`Int_8` and an array of that type
(:ada:`Int_8_Array`). We want to ensure each element of the array is
squared after calling the procedure :ada:`Square` for an object of the
:ada:`Int_8_Array` type. We do this with a postcondition using a :ada:`for
all` expression. This postcondition also uses the :ada:`'Old` attribute to
refer to the original value of the parameter (before the call).

We also want to ensure that the result of calls to the :ada:`Square`
function for the :ada:`Int_8` type are greater than the input to that call.
To do that, we write a postcondition using the :ada:`'Result` attribute of
the function and comparing it to the input value.

We can use both pre- and postconditions in the declaration of a single
subprogram. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Simple_Contract
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Contract is

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

       function Square (A : Int_8) return Int_8 is
         (A * A)
           with
             Pre  => (Integer'Size >= Int_8'Size * 2
                      and Integer (A) *
                            Integer (A) <=
                          Integer (Int_8'Last)),
             Post => (if abs A in 0 | 1
                      then Square'Result = abs A
                      else Square'Result > A);

       V : Int_8;
    begin
       V := Square (11);
       Put_Line ("Square of 11 is "
                 & Int_8'Image (V));

       --  Precondition will fail...
       V := Square (12);
       Put_Line ("Square of 12 is "
                 & Int_8'Image (V));
    end Show_Simple_Contract;

In this example, we want to ensure that the input value of calls to the
:ada:`Square` function for the :ada:`Int_8` type won't cause overflow in
that function. We do this by converting the input value to the
:ada:`Integer` type, which is used for the temporary calculation, and check
if the result is in the appropriate range for the :ada:`Int_8` type. We
have the same postcondition in this example as in the previous one.

Predicates
----------

Predicates specify expectations regarding types. They're similar to pre-
and postconditions, but apply to types instead of subprograms. Their
conditions are checked for each object of a given type, which allows
verifying that an object of type :ada:`T` is conformant to the requirements of
its type.

There are two kinds of predicates: static and dynamic. In simple terms,
static predicates are used to check objects at compile-time, while dynamic
predicates are used for checks at run time. Normally, static predicates are
used for scalar types and dynamic predicates for the more complex types.

Static and dynamic predicates are specified using the following clauses,
respectively:

- :ada:`with Static_Predicate => <property>`

- :ada:`with Dynamic_Predicate => <property>`

Let's use the following example to illustrate dynamic predicates:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Dynamic_Predicate_Courses
    :class: ada-run-expect-failure

    with Ada.Calendar; use Ada.Calendar;

    with Ada.Containers.Vectors;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    procedure Show_Dynamic_Predicate_Courses is

       package Courses is
          type Course_Container is private;

          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record
            with Dynamic_Predicate =>
              Course.Start_Date <= Course.End_Date;

          procedure Add (CC : in out Course_Container;
                         C  :        Course);
       private
          package Course_Vectors is new
            Ada.Containers.Vectors
              (Index_Type   => Natural,
               Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container;
                         C  :        Course) is
          begin
             CC.V.Append (C);
          end Add;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Course'(
              Name       =>
                To_Unbounded_String
                  ("Intro to Photography"),
              Start_Date =>
                Time_Of (2018, 5, 1),
              End_Date   =>
                Time_Of (2018, 5, 10)));

       --  This should trigger an error in the
       --  dynamic predicate check
       Add (CC,
            Course'(
              Name       =>
                To_Unbounded_String
                  ("Intro to Video Recording"),
              Start_Date =>
                Time_Of (2019, 5, 1),
              End_Date   =>
                Time_Of (2018, 5, 10)));

    end Show_Dynamic_Predicate_Courses;

In this example, the package :ada:`Courses` defines a type :ada:`Course`
and a type :ada:`Course_Container`, an object of which contains all
courses. We want to ensure that the dates of each course are consistent,
specifically that the start date is no later than the end date. To enforce
this rule, we declare a dynamic predicate for the :ada:`Course` type that
performs the check for each object. The predicate uses the type name where
a variable of that type would normally be used: this is a reference to the
instance of the object being tested.

Note that the example above makes use of unbounded strings and dates. Both types
are available in Ada's standard library. Please refer to the following sections
for more information about:

- the unbounded string type (:ada:`Unbounded_String`):
  :ref:`Unbounded Strings <Intro_Ada_Unbounded_Strings>` section;

- dates and times:
  :ref:`Dates & Times <Intro_Ada_Dates_Times>` section.

Static predicates, as mentioned above, are mostly used for scalar types and
checked during compilation. They're particularly useful for representing
non-contiguous elements of an enumeration. A classic example is a list of
week days:

.. code-block:: ada

    type Week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

We can easily create a sub-list of work days in the week by specifying a
:ada:`subtype` with a range based on :ada:`Week`. For example:

.. code-block:: ada

    subtype Work_Week is Week range Mon .. Fri;

Ranges in Ada can only be specified as contiguous lists: they don't allow
us to pick specific days. However, we may want to create a list containing
just the first, middle and last day of the work week. To do that, we use a
static predicate:

.. code-block:: ada

   subtype Check_Days is Work_Week
     with Static_Predicate =>
            Check_Days in Mon | Wed | Fri;

Let's look at a complete example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Predicates
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Predicates is

       type Week is (Mon, Tue, Wed, Thu,
                     Fri, Sat, Sun);

       subtype Work_Week is Week range Mon .. Fri;

       subtype Test_Days is Work_Week
         with Static_Predicate =>
           Test_Days in Mon | Wed | Fri;

       type Tests_Week is array (Week) of Natural
         with Dynamic_Predicate =>
           (for all I in Tests_Week'Range =>
              (case I is
                   when Test_Days =>
                      Tests_Week (I) > 0,
                   when others    =>
                      Tests_Week (I) = 0));

       Num_Tests : Tests_Week :=
                     (Mon => 3, Tue => 0,
                      Wed => 4, Thu => 0,
                      Fri => 2, Sat => 0,
                      Sun => 0);

       procedure Display_Tests (N : Tests_Week) is
       begin
          for I in Test_Days loop
             Put_Line ("# tests on "
                       & Test_Days'Image (I)
                       & " => "
                       & Integer'Image (N (I)));
          end loop;
       end Display_Tests;

    begin
       Display_Tests (Num_Tests);

       --  Assigning non-conformant values to
       --  individual elements of the Tests_Week
       --  type does not trigger a predicate
       --  check:
       Num_Tests (Tue) := 2;

       --  However, assignments with the "complete"
       --  Tests_Week type trigger a predicate
       --  check. For example:
       --
       --  Num_Tests := (others => 0);

       --  Also, calling any subprogram with
       --  parameters of Tests_Week type
       --  triggers a predicate check. Therefore,
       --  the following line will fail:
       Display_Tests (Num_Tests);
    end Show_Predicates;

Here we have an application that wants to perform tests only on three days
of the work week. These days are specified in the :ada:`Test_Days`
subtype. We want to track the number of tests that occur each day. We
declare the type :ada:`Tests_Week` as an array, an object of which will
contain the number of tests done each day. According to our requirements,
these tests should happen only in the aforementioned three days; on other
days, no tests should be performed. This requirement is implemented with a
dynamic predicate of the type :ada:`Tests_Week`. Finally, the actual
information about these tests is stored in the array :ada:`Num_Tests`,
which is an instance of the :ada:`Tests_Week` type.

The dynamic predicate of the :ada:`Tests_Week` type is verified during the
initialization of :ada:`Num_Tests`. If we have a non-conformant value
there, the check will fail. However, as we can see in our example,
individual assignments to elements of the array do not trigger a check. We
can't check for consistency at this point because the initialization of the
a complex data structure (such as arrays or records) may not be performed
with a single assignment. However, as soon as the object is passed as an
argument to a subprogram, the dynamic predicate is checked because the
subprogram requires the object to be consistent. This happens in the last
call to :ada:`Display_Tests` in our example. Here, the predicate check
fails because the previous assignment has a non-conformant value.

Type invariants
---------------

Type invariants are another way of specifying expectations regarding types.
While predicates are used for *non-private* types, type invariants are used
exclusively to define expectations about private types. If a type :ada:`T`
from a package :ada:`P` has a type invariant, the results of operations on
objects of type :ada:`T` are always consistent with that invariant.

Type invariants are specified with a
:ada:`with Type_Invariant => <property>`
clause. Like predicates, the *property* defines a condition
that allows us to check if an object of type :ada:`T` is conformant to its
requirements. In this sense, type invariants can be viewed as a sort of
predicate for private types.  However, there are some differences in terms
of checks. The following table summarizes the differences:

+------------+-----------------------------+-----------------------------+
| Element    | Subprogram parameter checks | Assignment checks           |
+============+=============================+=============================+
| Predicates | On all :ada:`in` and        | On assignments and explicit |
|            | :ada:`out` parameters       | initializations             |
+------------+-----------------------------+-----------------------------+
| Type       | On :ada:`out` parameters    | On all initializations      |
| invariants | returned from subprograms   |                             |
|            | declared in the same public |                             |
|            | scope                       |                             |
+------------+-----------------------------+-----------------------------+

We could rewrite our previous example and replace dynamic predicates by
type invariants. It would look like this:

.. code:: ada run_button project=Courses.Intro_To_Ada.Contracts.Show_Type_Invariant
    :class: ada-run-expect-failure

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Calendar; use Ada.Calendar;

    with Ada.Containers.Vectors;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    procedure Show_Type_Invariant is

       package Courses is
          type Course is private
            with Type_Invariant => Check (Course);

          type Course_Container is private;

          procedure Add (CC : in out Course_Container;
                         C  :        Course);

          function Init
            (Name                 : String;
             Start_Date, End_Date : Time)
             return Course;

          function Check (C : Course)
                          return Boolean;

       private
          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record;

          function Check (C : Course)
                          return Boolean is
            (C.Start_Date <= C.End_Date);

          package Course_Vectors is new
            Ada.Containers.Vectors
              (Index_Type   => Natural,
               Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container;
                         C  :        Course) is
          begin
             CC.V.Append (C);
          end Add;

          function Init
            (Name                 : String;
             Start_Date, End_Date : Time)
             return Course is
          begin
             return
               Course'(Name       =>
                         To_Unbounded_String (Name),
                       Start_Date => Start_Date,
                       End_Date   => End_Date);
          end Init;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Init (Name       =>
                    "Intro to Photography",
                  Start_Date =>
                    Time_Of (2018, 5, 1),
                  End_Date   =>
                    Time_Of (2018, 5, 10)));

       --  This should trigger an error in the
       --  type-invariant check
       Add (CC,
            Init (Name       =>
                    "Intro to Video Recording",
                  Start_Date =>
                    Time_Of (2019, 5, 1),
                  End_Date   =>
                    Time_Of (2018, 5, 10)));
    end Show_Type_Invariant;

The major difference is that the :ada:`Course` type was a visible (public)
type of the :ada:`Courses` package in the previous example, but in this
example is a private type.
