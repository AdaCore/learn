Fixed-point types
=================

.. include:: ../../../global.txt

In this chapter, we discuss fixed-point types, which can be classified in two
categories:
:ref:`decimal fixed-point types <Intro_Ada_Decimal_Fixed_Point_Types>` and
:ref:`ordinary (binary) fixed-point types <Intro_Ada_Ordinary_Fixed_Point_Types>`.
Afterward a brief overview of each category, we discuss some
:ref:`differences between fixed-point and floating-point types <Intro_Ada_Fixed_Point_Vs_Floating_Point_Types>`.


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


Decimal delta
^^^^^^^^^^^^^

The delta determines the required decimal precision for the type. For example,
if we want to be able to use two digits after the decimal point, we would write
:ada:`delta 10.0 ** (-2)` |mdash| which is equivalent to :ada:`delta 0.01`.
(You can use any of those definitions: both :ada:`delta 10.0 ** (-2)` and
:ada:`delta 0.01` are correct.)

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type Decimal is
         delta 10.0 ** (-1) digits 3;

       --  Alternatively:
       --  type Decimal is
       --    delta 0.1 digits 3;
    begin
       Put_Line
         ("The decimal precision of Decimal is "
          & Decimal'Delta'Image);
    end Decimal_Fixed_Point_Types;

In this example, we declare the :ada:`Decimal` type, which has a decimal
precision of 0.1. We use the :ada:`'Delta` attribute to show the decimal
precision of the type.


Decimal digits
^^^^^^^^^^^^^^

Unsurprisingly, the :ada:`digits` part of the type declaration determines
the number of digits that a type is able to represent. For example, by writing
:ada:`digits 3`, we're able to represent values with three digits ranging from
-999 to 999. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type Decimal is
         delta 10.0 ** (0) digits 3;
    begin
       Put_Line ("The minimum value of Decimal is "
                 & Decimal'First'Image);
       Put_Line ("The maximum value of Decimal is "
                 & Decimal'Last'Image);
    end Decimal_Fixed_Point_Types;

In this example, we declare the :ada:`Decimal` type, which has a range from
-999 to 999. We use the :ada:`'First` and :ada:`'Last` attributes to show
the first and last value of the range, respectively.

.. admonition:: For further reading...

    When running the application above, we see that the first and last values
    are -999.0 and 999.0, respectively |mdash| i.e. values with the decimal
    point. Strictly speaking, however, the actual first and last values are
    -999 and 999 because we selected a delta of 1.0. The decimal point
    (``.0``) we see in the application output (in the values ``-999.0`` and
    ``999.0``) is only there to indicate that this is not an integer value
    |mdash| however, it doesn't indicate an extended decimal precision at all.

    .. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Type_Precision_Error
        :class: ada-expect-compile-error

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Decimal_Fixed_Point_Types is
           type Decimal is
             delta 10.0 ** (0) digits 3;

           D : Decimal := 0.1;
           --             ^^^
           --  ERROR: value cannot be represented
           --         by Decimal type.
        begin
           Put_Line (D'Image);
        end Decimal_Fixed_Point_Types;

    Assigning the value 0.1 to :ada:`D` is wrong because the :ada:`Decimal`
    type cannot represent this value.

.. admonition:: For further reading...

    The :ada:`Decimal` type above is similar to |mdash| but far from being
    equivalent to |mdash| the following floating-point type declaration:

    .. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Floating_Point_Range_Equivalent

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Decimal_Fixed_Point_Types is
           type Float_999 is
             digits 3
             range -999.0 .. 999.0;
        begin
           Put_Line ("The minimum value of Float_999 is "
                     & Float_999'First'Image);
           Put_Line ("The maximum value of Float_999 is "
                     & Float_999'Last'Image);
        end Decimal_Fixed_Point_Types;

    The :ada:`Float_999` type from this example has (roughly) the same range as
    the :ada:`Decimal` type that we declared in the previous example: -999 to
    999. However, there are substantial
    :ref:`differences between fixed-point and floating-point types <Intro_Ada_Fixed_Point_Vs_Floating_Point_Types>`,
    so we cannot say that these type declarations are equivalent.


Decimal delta and digits
^^^^^^^^^^^^^^^^^^^^^^^^

By combining those three digits (i.e. :ada:`digits 3`) with a decimal precision
of two digits after the decimal point (:ada:`delta 10.0 ** (-2)`), we get a
range from -9.99 to 9.99. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type Decimal is
         delta 10.0 ** (-2) digits 3;
    begin
       Put_Line ("The minimum value of Decimal is "
                 & Decimal'First'Image);
       Put_Line ("The maximum value of Decimal is "
                 & Decimal'Last'Image);
    end Decimal_Fixed_Point_Types;

In this example, we declare the :ada:`Decimal` type, which has a range from
-9.99 to 9.99 (as expected).


Requirements for the delta
^^^^^^^^^^^^^^^^^^^^^^^^^^

Note that the delta expression for decimal fixed-point types must be a power
of 10. Using a different value for the power leads to compilation errors. For
example:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Fixed_Point_Type_Error
    :class: ada-expect-compile-error

    package Decimal_Fixed_Point_Type_Error is

       type Decimal_Error_1 is
         delta 2.0 ** (-1) digits 3;
       --      ^^^^^^^^^^^
       --  ERROR: not power of ten

       type Decimal_Error_2 is
         delta 0.125 digits 3;
       --      ^^^^^
       --  ERROR: not power of ten

    end Decimal_Fixed_Point_Type_Error;

In this example, the type declarations (of :ada:`Decimal_Error_1` and
:ada:`Decimal_Error_2`) are wrong because the delta expression is not a power
of 10.


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


.. _Intro_Ada_Fixed_Point_Vs_Floating_Point_Types:

Fixed-point vs. floating-point types
------------------------------------

The main difference between fixed-point and floating-point types is that
fixed-point types don't have an exponent. This has an impact on calculations
using small values: while they might still be representable with floating-point
types, those small values might simply *disappear* (i.e. become zero) in the
fixed-point representation. Let's see an example where we compare the decimal
type :ada:`Decimal` to the floating-point type :ada:`Float_32`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Fixed_Point_Types.Decimal_Vs_Floating_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Vs_Floating_Point_Types is
       type Decimal is
         delta 10.0 ** (-2) digits 9;

       type Float_32 is
         digits 6
         range -9999999.99 .. 9999999.99;

       D : Decimal  := 0.01;
       F : Float_32 := 0.01;
    begin
       Put_Line ("D = " &
                 D'Image);
       Put_Line ("F = " &
                 F'Image);

       D := D / 2.0;
       --   ^^^^^^^
       --  Value becomes zero.

       F := F / 2.0;
       --   ^^^^^^^
       --  Exponent is used to
       --  represent smaller
       --  value.

       Put_Line ("D = " &
                 D'Image);
       Put_Line ("F = " &
                 F'Image);
    end Decimal_Vs_Floating_Point_Types;

Both types in this example have roughly the same size and range. However, the
result of the divide-by-two operation isn't the same: because of the exponent,
:ada:`F` has the expected value (0.005) after the operation. while the value of
:ada:`D` is zero. The reason is that the resulting value 0.005 cannot be
represented by the decimal precision of the :ada:`Decimal` type. In the case of
:ada:`F`, however, the value can be represented due to a simple change in the
exponent.

This lack of precision we just described might seem like a drawback for
fixed-point types. However, depending on the algorithm and its field of
application, this is the exact behavior that we might be looking for. As
mentioned in the beginning of this chapter, financial applications
benefit from decimal types, while using floating-point type for these
applications can lead to unpredictable (or undesirable) behavior.

Another major difference concerns the way fixed-point operations translate into
machine operations. In most cases, operations on fixed-point types are modeled
in a processor by using integer registers and instructions. Essentially,
the compiler maps fixed-point types to integer types, but it uses slightly
different numeric rules. This fact can be an advantage for specific embedded
applications where a floating-point unit might be either non-existent or its
usage might have a higher associated cost in terms of CPU cycles or power
consumption. Therefore, for these specific applications, using fixed-point
types could be considered as an alternative.
