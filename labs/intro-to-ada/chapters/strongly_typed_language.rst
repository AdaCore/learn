:orphan:

Strongly typed language
=======================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Exercise #1
-----------

In this exercise, you'll work with enumerations. These are your goals:

#. Declare an enumeration named :ada:`HTML_Color` for the following
   colors:

   +-------------+---------------+
   | Color       | Value         |
   +=============+===============+
   | Salmon      | ```#FA8072``` |
   +-------------+---------------+
   | Firebrick   | ```#B22222``` |
   +-------------+---------------+
   | Red         | ```#FF0000``` |
   +-------------+---------------+
   | Darkred     | ```#8B0000``` |
   +-------------+---------------+
   | Lime        | ```#00FF00``` |
   +-------------+---------------+
   | Forestgreen | ```#228B22``` |
   +-------------+---------------+
   | Green       | ```#008000``` |
   +-------------+---------------+
   | Darkgreen   | ```#006400``` |
   +-------------+---------------+
   | Blue        | ```#0000FF``` |
   +-------------+---------------+
   | Mediumblue  | ```#0000CD``` |
   +-------------+---------------+
   | Darkblue    | ```#00008B``` |
   +-------------+---------------+

#. Implement a function :ada:`To_Integer` to convert from the
   :ada:`HTML_Color` type to the integer values listed on table above.

   - Hints: You may use a :ada:`case` for this. Also, in order to express
            the hexadecimal values above in Ada, use the following syntax:
            ```16#<hex_value>#``` (e.g.: :ada:`16#FFFFFF#`).

#. Declare another color type (:ada:`Basic_HTML_Color`) with the following
   colors: Red, Green, Blue. Also, implement the function
   :ada:`To_HTML_Color` that converts from the :ada:`Basic_HTML_Color` to
   :ada:`HTML_Color`.

Don't worry about the details of the :ada:`Main` procedure. You should
just focus on declaring the types (in the :ada:`Color_Types` package) and
implementing the functions as indicated below.

.. code:: ada lab=Strongly_Typed_Enumerations

    --  START LAB IO BLOCK
    in 0: HTML_Color_Range
    out 0:
    in 1: HTML_Color_To_Integer
    out 1:
    in 1: Basic_HTML_Color_To_HTML_Color
    out 1: RED GREEN BLUE
    --  END LAB IO BLOCK

    package Color_Types is
    end Color_Types;

Exercise #2
-----------

In this exercise, you'll create integer types. These are your goals:

#. Declare two custom integer types with values between 0 and 100: one
   integer type (:ada:`I_100`) and a modular type (:ada:`U_100`).

#. Implement the functions :ada:`To_I_100` and :ada:`To_U_100` to convert
   between the :ada:`I_100` and :ada:`U_100` types.

#. Declare integer types with values between 10 and 50 using :ada:`I_100`
   as a base type. You must create one derived type (:ada:`D_50`) and a
   subtype (:ada:`S_50`).

#. Implement the function :ada:`To_D_50` and :ada:`To_S_50` that convert
   from :ada:`I_100` to these types and saturate the value if they are out
   of range. For example, if the input for :ada:`To_D_50` is 100, the
   output of the function is 50.

   - Hint: you may use the type attributes :ada:`D_50'First` and
           :ada:`D_50'Last`, which indicate, respectively, the minimum and
           maximum value of the :ada:`D_50` type. The same attributes are
           available for the :ada:`S_50` type.

#. Implement the function :ada:`To_I_100` that convert from :ada:`D_100`
   back to the base type :ada:`I_100`.

   - Remark: we could write a function :ada:`To_I_100` to convert from
             :ada:`S_100` as well. However, we skip this here because
             explicit conversions are not needed for subtypes.

Don't worry about the details of the :ada:`Main` procedure. You should
just focus on declaring the types (in the :ada:`Int_Types` package) and
implementing the functions as indicated below.

.. code:: ada lab=Strongly_Typed_Integers

    --  START LAB IO BLOCK
    in 0: I_100_Range
    out 0: 0 100
    in 1: U_100_Range
    out 1: 0 100
    in 1: U_100_Wraparound
    out 1: 100 0
    --  END LAB IO BLOCK

    package Int_Types is
    end Int_Types;

Exercise #3
-----------

In this exercise, you'll work with custom floating-point and integer types
to implement a small system with temperatures in Celsius and Kelvin. These
are your goals:

#. Declare a floating-point and an integer type for temperatures in
   Celsius: :ada:`Celsius` and :ada:`Int_Celsius`, respectively. You must
   use a range between -273.15 and 5504.85 for the floating-point type and
   the rounded interval between -273 and 5505 for the integer type.

#. Implement the functions :ada:`To_Celsius` and :ada:`To_Int_Celsius` to
   convert between these types. Because of the slightly different ranges,
   you'll need to check for the minimum and maximum values of the input
   values in the implementation of the :ada:`To_Celsius` function.

   - Hint: use variables of floating-point type (:ada:`Float`) for
           intermediate values in the implementation of :ada:`To_Celsius`.

#. Declare a floating-point type named :ada:`Kelvin` for temperatures in
   Kelvin using a range between 0.0 and 5778.0.

#. Implement the functions :ada:`To_Celsius` and :ada:`To_Kelvin` to
   convert between temperatures in Kelvin and Celsius.

   - Hint: use variable of floating-point type (:ada:`Float`) for
           intermediate values.

For the floating-point types above, use a precision of six digits.

.. code:: ada lab=Strongly_Typed_Floating_Point

    --  START LAB IO BLOCK
    in 0: Celsius_Range
    out 0:
    in 1: Celsius_To_Int_Celsius
    out 1:
    in 1: Int_Celsius_To_Celsius
    out 1:
    in 1: Kelvin_To_Celsius
    out 1:
    in 1: Kelvin_To_Celsius
    out 1:
    --  END LAB IO BLOCK

    package Temperature_Types is
    end Temperature_Types;
