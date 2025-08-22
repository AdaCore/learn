Fixed-point types
=================

.. include:: ../../global.txt

.. _Intro_Ada_Decimal_Fixed_Point_Types:

Decimal fixed-point types
-------------------------

We have already seen how to specify
:ref:`floating-point types <Intro_Ada_Floating_Point_Types>`.  However, in some
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

    type <type-name> is
      delta <delta-value> digits <digits-value>;

In this case, the :ada:`delta` and the :ada:`digits` will be used by the
compiler to derive a range.


.. _Intro_Ada_Ordinary_Fixed_Point_Types:

Ordinary fixed-point types
--------------------------

Ordinary fixed-point types are similar to decimal fixed-point types in that the
values are, in effect, scaled integers.  The difference between them is in the
scale factor: for a decimal fixed-point type, the scaling, given explicitly by
the type's :ada:`delta`, is always a power of ten.
In contrast, for an ordinary fixed-point type, the :ada:`delta` isn't limited
to power of 10 values, but it can have any arbitrary base.

.. admonition:: For further reading...

    When representing ordinary fixed-point types on the machine, the compiler
    selects a scaling factor derived from the value of :ada:`delta` specified
    in the type declaration. This compiler-selected scaling factor is, by
    default, a power of two |mdash| even if the value provided for the
    :ada:`delta` isn't a  power of two. Therefore, ordinary fixed-point types
    are sometimes called binary fixed-point types.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

For example, we can define an ordinary fixed-point type :ada:`T_Inv_Trig` for
inverse trigonometric calculations:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is
         delta 0.0005
         range -Pi / 2.0 .. Pi / 2.0;
    begin
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

In this example, we are defining the :ada:`T_Inv_Trig` type with a range from
-π/2 to π/2, and a delta of 0.0005. Note that, in this case, the delta is
neither a power of ten nor a power of two. (In fact, this value corresponds to
:ada:`2000.0 ** (-1)`.)
