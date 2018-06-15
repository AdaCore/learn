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
examples of contracts. In summary, these elements allow for improving the
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
postconditions. We can think of precondition and postconditions as
promisses between the subprogram caller and the callee: a precondition is
a promise from the caller to the callee, and a postcondition is a promise
from the callee to the caller.

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

       procedure DB_Entry (Name : String; Age  : Natural)
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

In this example, we want to ensure that the name field in our database
doesn't contain an empty string. We can implement this requirement by
using a precondition that ensures that the length of the string used for
the :ada:`Name` parameter of the :ada:`DB_Entry` procedure is greater than
zero. If another subprogram attempts to call the :ada:`DB_Entry` procedure
with an empty string for the :ada:`Name` parameter, the call will fail
because the precondition is not met.

Note that the :ada:`pragma Assertion_Policy` statement is used to force
the compiler to generate a check for the precondition. The same
:ada:`pragma` will be used for other kinds of contracts in the remaining
examples from this section. When using GNAT, it's possible to trigger that
behavior globally via a configuration pragma or via a command-line switch
--- please consult the GNAT documentation on
`configuration pragmas <http://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#configuration-pragmas>`_
for details.

Before we look into our next example, let's discuss briefly quantified
expressions, which are quite useful to specify pre and postconditions in a
concise way. Quantified expressions return a Boolean value indicating
whether an array or container matches the expected condition. They have
the following form:
:ada:`(for all I in A'Range => <condition on A(I)>`, where :ada:`A` is an
array and :ada:`I` is the current index. In other words, quantified
expressions using :ada:`for all` check each element of the array or
container in order to assess whether the condition is true. For example:

.. code:: ada
    :class: ada-nocheck

    (for all I in A'Range => A (I) = 0)

This quantified expression will verify whether all elements of the array
:ada:`A` have a value of zero.

Another kind of quantified expressions makes use of :ada:`for some`. The
form is basically the same:
:ada:`(for some I in A'Range => <condition on A(I)>`. However, in this
case, finding a single element that matches the condition is sufficient to
determine that the expression is valid for the whole array :ada:`A`.

Let's now discuss postconditions using the following example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Elementary_Functions;

    procedure Show_Simple_Postcondition is
       pragma Assertion_Policy (Post => Check);

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

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
the procedure :ada:`Double` for an object of :ada:`Int_8_Array` type, each
element of the array will be doubled. This is implemented by a
postcondition that uses a :ada:`for all` expression. The postcondition
also makes use of the original value of the parameter before the call.
The :ada:`'Old` attribute is used in this case to retrieve the original
value.

Also, we want to ensure that, in calls to the
:ada:`Double` function for the :ada:`Int_8` type, the result will be
greater than the input value. This is implemented by a postcondition that
uses the :ada:`'Result` attribute of the function and compares it to the
input value.

We can use pre and postconditions at the same time in the declaration of
a subprogram. For example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Numerics.Elementary_Functions;

    procedure Show_Simple_Contract is
       pragma Assertion_Policy (Pre  => Check,
                                Post => Check);

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

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
overflow in the call to the function. This is implemented by converting
the input value to the :ada:`Integer` type, which is used to store the
temporary calculation, and check if the result is still in the appropriate
range for the :ada:`Int_8` type. The postcondition in this example is the
same as in the previous example.

Predicates
----------

Predicates are used to define expectations regarding types. They are
similar to pre and postconditions, and can be viewed as conditions that
are verified for a given type. This allows for checking if an element of
type ``T`` is conformant to the requirements.

There are two kinds of predicates: static and dynamic predicates. In
simple terms, static predicates are used to check types at compile-time,
whereas dynamic predicates are used for checks at run-time. We can also
say that static predicates are used for scalar types, whereas dynamic
predicates are used for all remaining (more complex) types.

Static and dynamic predicates are specified by using the following
clauses, respectively:

- :ada:`with Static_Predicate => <property>`

- :ada:`with Dynamic_Predicate => <property>`

Let's discuss dynamic predicates with the following example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO;           use Ada.Text_IO;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Calendar;          use Ada.Calendar;
    with Ada.Containers.Vectors;

    procedure Show_Dynamic_Predicate_Courses is

       pragma Assertion_Policy (Dynamic_Predicate => Check);

       package Courses is
          type Course_Container is private;

          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record
            with Dynamic_Predicate => Course.Start_Date <= Course.End_Date;

          procedure Add (CC : in out Course_Container; C : Course);
       private
          package Course_Vectors is new Ada.Containers.Vectors
            (Index_Type   => Natural,
             Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container; C : Course) is
          begin
             CC.V.Append (C);
          end Add;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Course'(
              Name       => To_Unbounded_String ("Intro to Photography"),
              Start_Date => Time_Of (2018, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

       --  This should trigger an error in the dynamic predicate check
       Add (CC,
            Course'(
              Name       => To_Unbounded_String ("Intro to Video Recording"),
              Start_Date => Time_Of (2019, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

    end Show_Dynamic_Predicate_Courses;

In this example, the package :ada:`Courses` defines a type :ada:`Course`
for individual courses, and a type :ada:`Course_Container` that contains
all courses. We want to ensure that the start date of every course is not
set to a date after the end date of the same course. In other words, we
want to check that the start and end dates are consistent to each other.
This is implemented by the function :ada:`Check`. In order to enforce this
rule, we declare a dynamic predicate for the :ada:`Course` type that calls
the :ada:`Check` function for every object. For example, when we enter our
course using the procedure :ada:`Add`, :ada:`Check` will be called during
the object creation to ensure that the object that is being created
matches our expectations.

Static predicates, as mentioned above, are used for scalar types and
checked during compilation time. They are particularly useful for
representing non-contiguous elements of an enumeration. A classic example
is a list of week days:

.. code:: ada
    :class: ada-nocheck

    type Week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

We can easily create a sub-list of working days in the week by specifying
a :ada:`subtype` with a range based on :ada:`Week`. For example:

.. code:: ada
    :class: ada-nocheck

    subtype Work_Week is Week range Mon .. Fri;

However, ranges in Ada can only be specified for contiguous lists: they
won't allow us to pick specific days. For example, we may want to create a
list containing the first, middle and last day of the working week to
make some checks in our application. In that case, we can use a static
predicate to specify this list:

.. code:: ada
    :class: ada-nocheck

   subtype Check_Days is Work_Week
     with Static_Predicate => Check_Days in Mon | Wed | Fri;

Let's look now at a complete example:

.. code:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Predicates is

       pragma Assertion_Policy (Static_Predicate  => Check,
                                Dynamic_Predicate => Check);

       type Week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       subtype Work_Week is Week range Mon .. Fri;

       subtype Test_Days is Work_Week
         with Static_Predicate => Test_Days in Mon | Wed | Fri;

       type Tests_Week is array (Week) of Natural
         with Dynamic_Predicate =>
           (for all I in Tests_Week'Range =>
              (case I is
                   when Test_Days => Tests_Week (I) > 0,
                   when others    => Tests_Week (I) = 0));

       Num_Tests : Tests_Week :=
                     (Mon => 3, Tue => 0,
                      Wed => 4, Thu => 0,
                      Fri => 2, Sat => 0, Sun => 0);

       procedure Display_Tests (N : Tests_Week) is
       begin
          for I in Test_Days loop
             Put_Line ("# tests on " & Test_Days'Image (I)
                       & " => "      & Integer'Image (N (I)));
          end loop;
       end Display_Tests;

    begin
       Display_Tests (Num_Tests);

       --  Assigning non-conformant values to individual elements of
       --  the Tests_Week type does not trigger a predicate check:
       Num_Tests (Tue) := 2;

       --  However, assignments with the "complete" Tests_Week type
       --  trigger a predicate check. For example:
       --
       --  Num_Tests := (others => 0);

       --  Also, calling any subprogram with parameters of Tests_Week
       --  type triggers a predicate check.
       --  Therefore, the following line will fail:
       Display_Tests (Num_Tests);
    end Show_Predicates;

In this example, we want to have tests in our application that happen
three days in the working week. These days are specified in
:ada:`Test_Days` subtype. Also, we want to track the number of tests
that happen each day. Therefore, we declare the type :ada:`Tests_Week` as
an array containing the number of tests. According to our requirements,
these tests should happen only in the aforementioned three days; in other
days, no test should be performed. This requirement is implemented as a
dynamic predicate of the type :ada:`Tests_Week`. Finally, in our
application, the actual information about these tests is stored in
the array :ada:`Num_Tests` based on the :ada:`Tests_Week` type.

In the initialization of :ada:`Num_Tests`, the dynamic predicate of the
:ada:`Tests_Week` type is verified. If we have a non-conformant value
there, the predicate check will fail. However, as we can see in our
example, individual assignments to elements of the array do not trigger a
check. The reason is that, in the case of complex data structures such as
arrays or records, the initialization of the complete structure may not be
performed with a single assignment. Therefore, we cannot check for
consistency at this point. However, as soon as this data structure is
passed as an argument to a subprogram, the dynamic predicate will be
checked because the subprogram expects the data structure to be
consistent. This is what happens in the last call to :ada:`Display_Tests`
in our example. Here, the predicate check fails because of the previous
assignment with a non-conformant value.

Type invariants
---------------

Type invariants are also used to define expectations regarding types.
However, while predicates are used for all *non-private* types,
type invariants are used exclusively to define expectations regarding
private types declared in a package. If a type ``T`` from a
package ``P`` has a type invariant, this ensures that operations on
objects of type ``T`` will always be consistent.

Type invariants are specified by using a
:ada:`with Type_Invariant => <property>` clause. Similarly to
postconditions, the *property* defines a condition that allows us to check
if an element of type ``T`` is conformant to the requirements. In this
sense, type invariants can be viewed as a sort of postcondition for
private types.

Type invariants are similar to predicates. However, there are some
differences in terms of checks. The following table summarizes the
differences:

+------------+-----------------------------+-----------------------------+
| Element    | Subprogram parameter checks | Assignment checks           |
+==========================================+=============================+
| Predicates | On all :ada:`in` and        | On assignments and explicit |
|            | :ada:`out` parameters       | initializations             |
+------------+-----------------------------+-----------------------------+
| Type       | On :ada:`out` parameters    | On all initializations      |
| invariants | returned from subprograms   |                             |
|            | declared in the same public |                             |
|            | scope                       |                             |
+------------+-----------------------------+-----------------------------+

We could rewrite our previous example and replace dynamic predicates by
type invariants. This would be the outcome:

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

          procedure Add (CC : in out Course_Container; C : Course);

          function Init
            (Name : String; Start_Date, End_Date : Time) return Course;

          function Check (C : Course) return Boolean;

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
          procedure Add (CC : in out Course_Container; C : Course) is
          begin
             CC.V.Append (C);
          end Add;

          function Init
            (Name : String; Start_Date, End_Date : Time) return Course is
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

Note that, in the previous example, the :ada:`Course` type was a visible
(public) type of the :ada:`Courses` package, whereas, in this example, it
is a private type.
