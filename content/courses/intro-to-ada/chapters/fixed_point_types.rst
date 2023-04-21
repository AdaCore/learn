Fixed-point types
=================

.. include:: ../../global.txt

Decimal fixed-point types
-------------------------

We have already seen how to specify floating-point types.  However, in some
applications floating-point is not appropriate since, for example, the roundoff
error from binary arithmetic may be unacceptable or perhaps the hardware does
not support floating-point instructions.  Ada provides a category of types, the
decimal fixed-point types, that allows the programmer to specify the required
decimal precision (number of digits) as well as the scaling factor (a power of
ten) and, optionally, a range.  In effect the values will be represented as
integers implicitly scaled by the specified power of 10. This is useful, for
example, for financial applications.

The syntax for a simple decimal fixed-point type is

.. code-block:: ada

    type <type-name> is delta <delta-value> digits <digits-value>;

In this case, the :ada:`delta` and the :ada:`digits` will be used by the
compiler to derive a range.

Several attributes are useful for dealing with decimal types:

+------------------------+----------------------------------------------+
| Attribute Name         | Meaning                                      |
+========================+==============================================+
| First                  | The first value of the type                  |
+------------------------+----------------------------------------------+
| Last                   | The last value of the type                   |
+------------------------+----------------------------------------------+
| Delta                  | The delta value of the type                  |
+------------------------+----------------------------------------------+

In the example below, we declare two data types: :ada:`T3_D3` and :ada:`T6_D3`.
For both types, the delta value is the same: 0.001.

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D3 is delta 10.0 ** (-3) digits 6;
    begin
       Put_Line ("The delta    value of T3_D3 is "
                 & T3_D3'Image (T3_D3'Delta));
       Put_Line ("The minimum  value of T3_D3 is "
                 & T3_D3'Image (T3_D3'First));
       Put_Line ("The maximum  value of T3_D3 is "
                 & T3_D3'Image (T3_D3'Last));
       New_Line;

       Put_Line ("The delta    value of T6_D3 is "
                 & T6_D3'Image (T6_D3'Delta));
       Put_Line ("The minimum  value of T6_D3 is "
                 & T6_D3'Image (T6_D3'First));
       Put_Line ("The maximum  value of T6_D3 is "
                 & T6_D3'Image (T6_D3'Last));
    end Decimal_Fixed_Point_Types;

When running the application, we see that the delta value of both
types is indeed the same: 0.001. However, because :ada:`T3_D3` is restricted
to 3 digits, its range is -0.999 to 0.999. For the :ada:`T6_D3`, we have
defined a precision of 6 digits, so the range is -999.999 to 999.999.

Similar to the type definition using the :ada:`range` syntax, because we
have an implicit range, the compiled code will check that the variables
contain values that are not out-of-range. Also, if the result of a
multiplication or division on decimal fixed-point types is smaller than
the delta value required for the context, the actual result will be
zero. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Smaller

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Smaller is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D6 is delta 10.0 ** (-6) digits 6;
       A : T3_D3 := T3_D3'Delta;
       B : T3_D3 := 0.5;
       C : T6_D6;
    begin
       Put_Line ("The value of A     is "
                 & T3_D3'Image (A));

       A := A * B;
       Put_Line ("The value of A * B is "
                 & T3_D3'Image (A));

       A := T3_D3'Delta;
       C := A * B;
       Put_Line ("The value of A * B is "
                 & T6_D6'Image (C));
    end Decimal_Fixed_Point_Smaller;

In this example, the result of the operation 0.001 * 0.5 is
0.0005. Since this value is not representable for the :ada:`T3_D3` type
because the delta value is 0.001, the actual value stored in variable
:ada:`A` is zero. However, accuracy is preserved during the arithmetic
operations if the target has sufficient precision, and the value
displayed for C is 0.000500.

Ordinary fixed-point types
--------------------------

.. TODO: add link to advanced lesson that discusses 'Delta vs. 'Small

Ordinary fixed-point types are similar to decimal fixed-point types in that the
values are, in effect, scaled integers.  The difference between them is in the
scale factor: for a decimal fixed-point type, the scaling, given explicitly by
the type's :ada:`delta`, is always a power of ten.

In contrast, for an ordinary fixed-point type, the scaling is defined by the
type's :ada:`small`, which is derived from the specified :ada:`delta` and, by
default, is a power of two. Therefore, ordinary fixed-point types are sometimes
called binary fixed-point types.

.. note::
   Ordinary fixed-point types can be thought of being closer to the actual
   representation on the machine, since hardware support for decimal
   fixed-point arithmetic is not widespread (rescalings by a power of ten),
   while ordinary fixed-point types make use of the available integer shift
   instructions.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

By default the compiler will choose a scale factor, or :ada:`small`, that is a
power of 2 no greater than <delta-value>.

For example, we may define a normalized range between -1.0 and 1.0 as
following:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Normalized_Fixed_Point_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Normalized_Fixed_Point_Type is
       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("TQ31 requires "
                 & Integer'Image (TQ31'Size)
                 & " bits");
       Put_Line ("The delta    value of TQ31 is "
                 & TQ31'Image (TQ31'Delta));
       Put_Line ("The minimum  value of TQ31 is "
                 & TQ31'Image (TQ31'First));
       Put_Line ("The maximum  value of TQ31 is "
                 & TQ31'Image (TQ31'Last));
    end Normalized_Fixed_Point_Type;

In this example, we are defining a 32-bit fixed-point data type for our
normalized range. When running the application, we notice that the upper
bound is close to one, but not exact one. This is a typical effect of
fixed-point data types |mdash| you can find more details in this discussion
about the :wikipedia:`Q format <Q_(number_format)>`.

We may also rewrite this code with an exact type definition:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Normalized_Adapted_Fixed_Point_Type

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is
         delta 2.0 ** (-15) * Pi
         range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("T_Inv_Trig requires "
                 & Integer'Image (T_Inv_Trig'Size)
                 & " bits");
       Put_Line ("Delta    value of T_Inv_Trig: "
                 & T_Inv_Trig'Image
                     (T_Inv_Trig'Delta));
       Put_Line ("Minimum  value of T_Inv_Trig: "
                 & T_Inv_Trig'Image
                     (T_Inv_Trig'First));
       Put_Line ("Maximum  value of T_Inv_Trig: "
                 & T_Inv_Trig'Image
                     (T_Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called :ada:`T_Inv_Trig`,
which has a range from -π/2 to π/2.

All standard operations are available for fixed-point types. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Fixed_Point_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Point_Op is
       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);

       A, B, R : TQ31;
    begin
       A := 0.25;
       B := 0.50;
       R := A + B;
       Put_Line ("R is " & TQ31'Image (R));
    end Fixed_Point_Op;

As expected, :ada:`R` contains 0.75 after the addition of :ada:`A` and :ada:`B`.

In fact the language is more general than these examples imply, since in
practice it is typical to need to multiply or divide values from different
fixed-point types, and obtain a result that may be of a third fixed-point type.
The details are outside the scope of this introductory course.

It is also worth noting, although again the details are outside the scope of
this course, that you can explicitly specify a value for an ordinary
fixed-point type's :ada:`small`.  This allows non-binary scaling, for example:

.. code-block:: ada

    type Angle is
      delta 1.0/3600.0
      range 0.0 .. 360.0 - 1.0 / 3600.0;
    for Angle'Small use Angle'Delta;
