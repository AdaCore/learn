Strongly typed language
=======================

.. include:: ../../global.txt

Ada is a strongly typed language. It is interestingly modern in that
respect: strong static typing has been increasing in popularity in programming
language design, owing to factors such as the growth of statically typed
functional programming, a big push from the research community in the typing
domain, and many practical languages with strong type systems.

.. _Intro_Ada_What_Is_A_Type:

What is a type?
---------------

In statically typed languages, a type is mainly (but not only) a *compile time*
construct. It is a construct to enforce invariants about the behavior of a
program.  Invariants are unchangeable properties that hold for all variables of
a given type. Enforcing them ensures, for example, that variables of a data
type never have invalid values.

A type is used to reason about the *objects* a program manipulates (an object
is a variable or a constant). The aim is to classify objects by what you can
accomplish with them (i.e., the operations that are permitted), and this way
you can reason about the correctness of the objects' values.

.. todo::

    Expand / clarify (on section: "what is a type?")

.. _Intro_Ada_Integers:

Integers
--------

A nice feature of Ada is that you can define your own integer types, based on
the requirements of your program (i.e., the range of values that makes sense).
In fact, the definitional mechanism that Ada provides forms the semantic basis
for the predefined integer types.  There is no "magical" built-in type in that
regard, which is unlike most languages, and arguably very elegant.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Integer_Type_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Integer_Type_Example is
       --  Declare a signed integer type,
       --  and give the bounds
       type My_Int is range -1 .. 20;
       --                         ^ High bound
       --                   ^ Low bound

       --  Like variables, type declarations can
       --  only appear in declarative regions.
    begin
       for I in My_Int loop
          Put_Line (My_Int'Image (I));
          --              ^ 'Image attribute
          --                converts a value
          --                to a String.
       end loop;
    end Integer_Type_Example;

This example illustrates the declaration of a signed integer type, and
several things we can do with them.

Every type declaration in Ada starts with the :ada:`type` keyword (except for
:ref:`task types <Intro_Ada_Task_Types>`). After the type, we can see a range that looks
a lot like
the ranges that we use in for loops, that defines the low and high bound of the
type. Every integer in the inclusive range of the bounds is a valid value for
the type.

.. admonition:: Ada integer types

   In Ada, an integer type is not specified in terms of its
   machine representation, but rather by its range. The
   compiler will then choose the most appropriate representation.

Another point to note in the above example is the :ada:`My_Int'Image (I)`
expression. The :ada:`Name'Attribute (optional params)` notation is used for
what is called an attribute in Ada. An attribute is a
built-in operation on a type, a value, or some other program entity.  It is
accessed by using a :ada:`'` symbol (the ASCII apostrophe).

Ada has several types available as "built-ins"; :ada:`Integer` is one of
them. Here is how :ada:`Integer` might be defined for a typical processor:

.. code-block:: ada

    type Integer is
      range -(2 ** 31) .. +(2 ** 31 - 1);

:ada:`**` is the exponent operator, which means that the first valid
value for :ada:`Integer` is -2\ :sup:`31`, and the last valid value is
2\ :sup:`31` - 1.

Ada does not mandate the range of the built-in type Integer. An implementation
for a 16-bit target would likely  choose the range -2\ :sup:`15` through
2\ :sup:`15` - 1.


Operational semantics
~~~~~~~~~~~~~~~~~~~~~~

Unlike some other languages, Ada requires that operations on integers should be
checked for overflow.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check
    :class: ada-run-expect-failure

    procedure Main is
       A : Integer := Integer'Last;
       B : Integer;
    begin
       B := A + 5;
       --  This operation will overflow, eg. it
       --  will raise an exception at run time.
    end Main;

There are two types of overflow checks:

* Machine-level overflow, when the result of an operation exceeds the maximum
  value (or is less than the minimum value) that can be represented in the
  storage reserved for an object of the type, and

* Type-level overflow, when the result of an operation is outside the range
  defined for the type.

Mainly for efficiency reasons, while machine level overflow always results in
an exception, type level overflows will only be checked at specific boundaries,
like assignment:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type My_Int is range 1 .. 20;
       A : My_Int := 12;
       B : My_Int := 15;
       M : My_Int := (A + B) / 2;
       --  No overflow here, overflow checks
       --  are done at specific boundaries.
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
       --  Loop body executed 13 times
    end Main;

Type level overflow will only be checked at specific points in the execution.
The result, as we see above, is that you might have an operation that overflows
in an intermediate computation, but no exception will be raised because the
final result does not overflow.

.. _Intro_Ada_Unsigned_Types:

Unsigned types
--------------

Ada also features unsigned Integer types. They're called *modular* types in Ada
parlance. The reason for this designation is due to their behavior in case of
overflow: They simply "wrap around", as if a modulo operation was applied.

For machine sized modular types, for example a modulus of 2\ :sup:`32`, this
mimics the most common implementation behavior of unsigned types. However, an
advantage of Ada is that the modulus is more general:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Unsigned_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Mod_Int is mod 2 ** 5;
       --              ^ Range is 0 .. 31

       A : constant Mod_Int := 20;
       B : constant Mod_Int := 15;

       M : constant Mod_Int := A + B;
       --  No overflow here,
       --  M = (20 + 15) mod 32 = 3
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
    end Main;

Unlike in C/C++, since this wraparound behavior is guaranteed by the Ada
specification, you can rely on it to implement portable code. Also, being able
to leverage the wrapping on arbitrary bounds is very useful |mdash| the modulus
does not need to be a power of 2 |mdash| to implement certain algorithms and
data structures, such as
:wikipedia:`ring buffers <Circular_buffer>`.

.. _Intro_Ada_Enum_Types:

Enumerations
------------

Enumeration types are another nicety of Ada's type system. Unlike C's enums,
they are *not* integers, and each new enumeration type is incompatible with
other enumeration types. Enumeration types are part of the bigger family of
discrete types, which makes them usable in certain situations that we will
describe later but one context that we have already seen is a case statement.

.. TODO: add link to section on discrete features

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Enumeration_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Enumeration_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);
       --  An enumeration type
    begin
       for I in Days loop
          case I is
             when Saturday .. Sunday =>
                Put_Line ("Week end!");

             when Monday .. Friday =>
                Put_Line ("Hello on "
                          & Days'Image (I));
                --  'Image attribute, works on
                --  enums too
          end case;
       end loop;
    end Enumeration_Example;

Enumeration types are powerful enough that, unlike in most languages, they're
used to define the standard Boolean type:

.. code-block:: ada

    type Boolean is (False, True);

As mentioned previously, every "built-in" type in Ada is defined with facilities
generally available to the user.

Floating-point types
--------------------

Basic properties
~~~~~~~~~~~~~~~~

Like most languages, Ada supports floating-point types. The most commonly used
floating-point type is :ada:`Float`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Demo

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Demo is
       A : constant Float := 2.5;
    begin
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Demo;

The application will display :ada:`2.5` as the value of :ada:`A`.

The Ada language does not specify the precision (number of decimal digits in
the mantissa) for Float; on a typical 32-bit machine the precision will be 6.

All common operations that could be expected for floating-point types are
available, including absolute value and exponentiation.  For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Operations

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Operations is
       A : Float := 2.5;
    begin
       A := abs (A - 4.5);
       Put_Line ("The value of A is "
                 & Float'Image (A));

       A := A ** 2 + 1.0;
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Operations;

The value of :ada:`A` is :ada:`2.0` after the first operation and :ada:`5.0`
after the second operation.

In addition to :ada:`Float`, an Ada implementation may offer data types with
higher precision such as :ada:`Long_Float` and :ada:`Long_Long_Float`. Like
Float, the standard does not indicate the exact precision of these types: it
only guarantees that the type :ada:`Long_Float`, for example, has at least the
precision of :ada:`Float`. In order to guarantee that a certain precision
requirement is met, we can define custom floating-point types, as we will see
in the next section.

Precision of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada allows the user to specify the precision for a floating-point type,
expressed in terms of decimal digits. Operations on these custom types will
then have at least the specified precision. The syntax for a simple
floating-point type declaration is:

.. code-block:: ada

    type T is digits <number_of_decimal_digits>;

The compiler will choose a floating-point representation that supports the
required precision. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Floating_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Custom_Floating_Types is
       type T3  is digits 3;
       type T15 is digits 15;
       type T18 is digits 18;
    begin
       Put_Line ("T3  requires "
                 & Integer'Image (T3'Size)
                 & " bits");
       Put_Line ("T15 requires "
                 & Integer'Image (T15'Size)
                 & " bits");
       Put_Line ("T18 requires "
                 & Integer'Image (T18'Size)
                 & " bits");
    end Custom_Floating_Types;

In this example, the attribute :ada:`'Size` is used to retrieve the number of
bits used for the specified data type. As we can see by running this example,
the compiler allocates 32 bits for :ada:`T3`, 64 bits for :ada:`T15` and 128
bits for :ada:`T18`.  This includes both the mantissa and the exponent.

The number of digits specified in the data type is also used in the format
when displaying floating-point variables. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Display_Custom_Floating_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Custom_Floating_Types is
       type T3  is digits 3;
       type T18 is digits 18;

       C1 : constant := 1.0e-4;

       A : constant T3  := 1.0 + C1;
       B : constant T18 := 1.0 + C1;
    begin
       Put_Line ("The value of A is "
                 & T3'Image (A));
       Put_Line ("The value of B is "
                 & T18'Image (B));
    end Display_Custom_Floating_Types;

As expected, the application will display the variables according to
specified precision (1.00E+00 and 1.00010000000000000E+00).

Range of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to the precision, a range can also be specified for a
floating-point type. The syntax is similar to the one used for integer data
types |mdash| using the :ada:`range` keyword.  This simple example creates a new
floating-point type based on the type :ada:`Float`, for a normalized range
between :ada:`-1.0` and :ada:`1.0`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 1.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range;

The application is responsible for ensuring that variables of this type stay
within this range; otherwise an exception is raised. In this example, the
exception :ada:`Constraint_Error` is raised when assigning :ada:`2.0` to the
variable :ada:`A`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range_Exception
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range_Exception is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 2.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range_Exception;

Ranges can also be specified for custom floating-point types. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Range_Types

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Range_Types is
       type T6_Inv_Trig  is
         digits 6 range -Pi / 2.0 .. Pi / 2.0;
    begin
       null;
    end Custom_Range_Types;

In this example, we are defining a type called :ada:`T6_Inv_Trig`, which has a
range from -π / 2 to π / 2 with a minimum precision of 6
digits. (:ada:`Pi` is defined in the predefined package :ada:`Ada.Numerics`.)

Strong typing
-------------

As noted earlier, Ada is strongly typed. As a result, different types of the
same family are incompatible with each other; a value of one type cannot be
assigned to a variable from the other type. For example:


.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Error
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Illegal_Example is
       --  Declare two different floating point types
       type Meters is new Float;
       type Miles  is new Float;

       Dist_Imperial : Miles;

       --  Declare a constant
       Dist_Metric : constant Meters := 1000.0;
    begin
       --  Not correct: types mismatch
       Dist_Imperial := Dist_Metric * 621.371e-6;
       Put_Line (Miles'Image (Dist_Imperial));
    end Illegal_Example;

A consequence of these rules is that, in the general case, a "mixed mode"
expression like :ada:`2 * 3.0` will trigger a compilation error. In a language
like C or Python, such expressions are made valid by implicit conversions. In
Ada, such conversions must be made explicit:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Conv is
       type Meters is new Float;
       type Miles is new Float;
       Dist_Imperial : Miles;
       Dist_Metric : constant Meters := 1000.0;
    begin
       Dist_Imperial :=
         Miles (Dist_Metric) * 621.371e-6;
       --  ^^^^^^^^^^^^^^^^^
       --    Type conversion, from Meters to Miles
       --    Now the code is correct

       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

Of course, we probably do not want to write the conversion code every time we
convert from meters to miles. The idiomatic Ada way in that case would be to
introduce conversion functions along with the types.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Func

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Conv is
       type Meters is new Float;
       type Miles  is new Float;

       --  Function declaration, like procedure
       --  but returns a value.
       function To_Miles (M : Meters) return Miles is
       --                             ^ Return type
       begin
          return Miles (M) * 621.371e-6;
       end To_Miles;

       Dist_Imperial : Miles;
       Dist_Metric   : constant Meters := 1000.0;
    begin
       Dist_Imperial := To_Miles (Dist_Metric);
       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

If you write a lot of numeric code, having to explicitly provide such
conversions might seem painful at first. However, this approach brings some
advantages. Notably, you can rely on the absence of implicit conversions, which
will in turn prevent some subtle errors.

.. admonition:: In other languages

    In C, for example, the rules for implicit conversions may not
    always be completely obvious. In Ada, however, the code will always do
    exactly what it seems to do. For example:

    .. code-block:: c

        int a = 3, b = 2;
        float f = a / b;

    This code will compile fine, but the result of :c:`f` will be 1.0 instead
    of 1.5, because the compiler will generate an integer division (three
    divided by two) that results in one. The software developer must be
    aware of data conversion issues and use an appropriate casting:

    .. code-block:: c

        int a = 3, b = 2;
        float f = (float)a / b;

    In the corrected example, the compiler will convert both variables to
    their corresponding floating-point representation before performing the
    division. This will produce the expected result.

    This example is very simple, and experienced C developers will probably
    notice and correct it before it creates bigger
    problems. However, in more complex applications where the type
    declaration is not always visible |mdash| e.g. when referring to elements of
    a :c:`struct` |mdash| this situation might not always be evident and quickly
    lead to software defects that can be harder to find.

    The Ada compiler, in contrast, will always reject code that
    mixes floating-point and integer variables without explicit conversion.
    The following Ada code, based on the erroneous example in C, will not
    compile:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Implicit_Cast
        :class: ada-expect-compile-error

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    The offending line must be changed to :ada:`F := Float (A) / Float (B);`
    in order to be accepted by the compiler.

- You can use Ada's strong typing to help
  enforce invariants in your code, as in the example
  above: Since Miles and Meters are two different types, you cannot mistakenly
  convert an instance of one to an instance of the other.

.. TODO: Add link to invariants

Derived types
-------------

In Ada you can create new types based on existing ones. This is very useful:
you get a type that has the same properties as some existing type but is
treated as a distinct type in the interest of strong typing.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Derived_Types
    :class: ada-expect-compile-error

    procedure Main is
       --  ID card number type,
       --  incompatible with Integer.
       type Social_Security_Number is new Integer
         range 0 .. 999_99_9999;
       --      ^ Since a SSN has 9 digits
       --        max., and cannot be
       --        negative, we enforce
       --        a validity constraint.

       SSN : Social_Security_Number :=
         555_55_5555;
       --   ^ You can put underscores as
       --     formatting in any number.

       I   : Integer;

       --  The value -1 below will cause a
       --  runtime error and a compile time
       --  warning with GNAT.
       Invalid : Social_Security_Number := -1;
    begin
       --  Illegal, they have different types:
       I := SSN;

       --  Likewise illegal:
       SSN := I;

       --  OK with explicit conversion:
       I := Integer (SSN);

       --  Likewise OK:
       SSN := Social_Security_Number (I);
    end Main;

The type :ada:`Social_Security` is said to be a *derived type*;
its *parent type* is Integer.

As illustrated in this example, you can refine the valid range when defining a
derived scalar type (such as integer, floating-point and enumeration).

.. ?? The enumeration example looks rather artificial and would not be likely
.. ?? in real code.  I suggest deleting it

The syntax for enumerations uses the :ada:`range <range>` syntax:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Weekend_Days is new
         Days range Saturday .. Sunday;
       --  New type, where only Saturday and Sunday
       --  are valid literals.
    begin
       null;
    end Greet;

Subtypes
--------

As we are starting to see, types may be used in Ada to enforce constraints on
the valid range of values. However, we sometimes want to enforce constraints on
some values while staying within a single type.  This is where subtypes come
into play.  A subtype does not introduce a new type.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       --  Declaration of a subtype
       subtype Weekend_Days is
         Days range Saturday .. Sunday;
       --     ^ Constraint of the subtype

       M : Days := Sunday;

       S : Weekend_Days := M;
       --  No error here, Days and Weekend_Days
       --  are of the same type.
    begin
       for I in Days loop
          case I is
             --  Just like a type, a subtype can
             --  be used as a range
             when Weekend_Days =>
                Put_Line ("Week end!");
             when others =>
                Put_Line ("Hello on "
                          & Days'Image (I));
          end case;
       end loop;
    end Greet;

Several subtypes are predefined in the standard package in Ada, and are
automatically available to you:

.. code-block:: ada

    subtype Natural  is Integer range 0 .. Integer'Last;
    subtype Positive is Integer range 1 .. Integer'Last;

While subtypes of a type are statically compatible with each other,
constraints are enforced at run time: if you violate a subtype constraint,
an exception will be raised.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype_Error
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Weekend_Days is
         Days range Saturday .. Sunday;

       Day     : Days := Saturday;
       Weekend : Weekend_Days;
    begin
       Weekend := Day;
       --         ^ Correct: Same type, subtype
       --           constraints are respected
       Weekend := Monday;
       --         ^ Wrong value for the subtype
       --           Compiles, but exception at runtime
    end Greet;

.. _Intro_Ada_Subtype_Aliases:

Subtypes as type aliases
~~~~~~~~~~~~~~~~~~~~~~~~

Previously, we've seen that we can create new types by declaring
:ada:`type Miles is new Float`. We could also create type aliases, which
generate alternative names |mdash| *aliases* |mdash| for known types. Note that
type aliases are sometimes called *type synonyms*.

We achieve this in Ada by using subtypes without new constraints. In this case,
however, we don't get all of the benefits of Ada's strong type checking. Let's
rewrite an example using type aliases:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Undetected_Imperial_Metric_Error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Undetected_Imperial_Metric_Error is
       --  Declare two type aliases
       subtype Meters is Float;
       subtype Miles is Float;

       Dist_Imperial : Miles;

       --  Declare a constant
       Dist_Metric : constant Meters := 100.0;
    begin
       --  No conversion to Miles type required:
       Dist_Imperial := (Dist_Metric * 1609.0)
                          / 1000.0;

       --  Not correct, but undetected:
       Dist_Imperial := Dist_Metric;

       Put_Line (Miles'Image (Dist_Imperial));
    end Undetected_Imperial_Metric_Error;

In the example above, the fact that both :ada:`Meters` and :ada:`Miles` are
subtypes of :ada:`Float` allows us to mix variables of both types without
type conversion. This, however, can lead to all sorts of programming mistakes
that we'd like to avoid, as we can see in the undetected error highlighted in
the code above. In that example, the error in the assignment of a value in
meters to a variable meant to store values in miles remains undetected because
both :ada:`Meters` and :ada:`Miles` are subtypes of :ada:`Float`. Therefore,
the recommendation is to use strong typing |mdash| via :ada:`type X is new Y`
|mdash| for cases such as the one above.

There are, however, many situations where type aliases are useful. For example,
in an application that uses floating-point types in multiple contexts, we could
use type aliases to indicate additional meaning to the types or to avoid long
variable names. For example, instead of writing:

.. code-block:: ada

    Paid_Amount, Due_Amount : Float;

We could write:

.. code-block:: ada

    subtype Amount is Float;

    Paid, Due : Amount;

.. admonition:: In other languages

    In C, for example, we can use a :c:`typedef` declaration to create a type
    alias. For example:

    .. code-block:: c

        typedef float meters;

    This corresponds to the declaration that we've seen above using subtypes.
    Other programming languages include this concept in similar ways. For
    example:

        - C++: ``using meters = float;``
        - Swift: ``typealias Meters = Double``
        - Kotlin: ``typealias Meters = Double``
        - Haskell: ``type Meters = Float``

Note, however, that subtypes in Ada correspond to type aliases if, and only
if, they don't have new constraints. Thus, if we add a new constraint to a
subtype declaration, we don't have a type alias anymore. For example, the
following declaration *can't* be consider a type alias of :ada:`Float`:

.. code-block:: ada

    subtype Meters is Float range 0.0 .. 1_000_000.0;

Let's look at another example:

.. code-block:: ada

    subtype Degree_Celsius is Float;

    subtype Liquid_Water_Temperature is
      Degree_Celsius range 0.0 .. 100.0;

    subtype Running_Water_Temperature is
      Liquid_Water_Temperature;

In this example, :ada:`Liquid_Water_Temperature` isn't an alias of
:ada:`Degree_Celsius`, since it adds a new constraint that wasn't part of the
declaration of the :ada:`Degree_Celsius`. However, we do have two type aliases
here:

- :ada:`Degree_Celsius` is an alias of :ada:`Float`;
- :ada:`Running_Water_Temperature` is an alias of
  :ada:`Liquid_Water_Temperature`, even if :ada:`Liquid_Water_Temperature`
  itself has a constrained range.
