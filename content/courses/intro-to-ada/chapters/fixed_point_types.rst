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


Ordinary fixed-point types
--------------------------

Ordinary fixed-point types are similar to decimal fixed-point types in that the
values are, in effect, scaled integers.  The difference between them is in the
scale factor: for a decimal fixed-point type, the scaling, given explicitly by
the type's :ada:`delta`, is always a power of ten.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

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
