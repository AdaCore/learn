Numerics
========

.. include:: ../../../global.txt

.. _Adv_Ada_Numeric_Literals:

Numeric Literals
----------------

Classification
~~~~~~~~~~~~~~

We've already discussed basic characteristics of numeric literals in the
Introduction to Ada course |mdash| although we haven't used this terminology
there. There are two kinds of numeric literals in Ada: integer literals and
real literals. They are distinguished by the absence or presence of a radix
point. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Real_Integer_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Real_Integer_Literals is
       Integer_Literal : constant := 365;
       Real_Literal    : constant := 365.2564;
    begin
       Put_Line ("Integer Literal: "
                 & Integer_Literal'Image);
       Put_Line ("Real Literal:    "
                 & Real_Literal'Image);
    end Real_Integer_Literals;

Another classification takes the use of a base indicator into account.
(Remember that, when writing a literal such as :ada:`2#1011#`, the base is the
element before the first ``#`` sign.) So here we distinguish between decimal
literals and based literals. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Decimal_Based_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Based_Literals is

       package F_IO is new
         Ada.Text_IO.Float_IO (Float);

       --
       --  DECIMAL LITERALS
       --

       Dec_Integer  : constant := 365;

       Dec_Real     : constant := 365.2564;
       Dec_Real_Exp : constant := 0.365_256_4e3;

       --
       --  BASED LITERALS
       --

       Based_Integer     : constant := 16#16D#;
       Based_Integer_Exp : constant := 5#243#e1;

       Based_Real        : constant :=
         2#1_0110_1101.0100_0001_1010_0011_0111#;
       Based_Real_Exp    : constant :=
         7#1.031_153_643#e3;
    begin
       F_IO.Default_Fore := 3;
       F_IO.Default_Aft  := 4;
       F_IO.Default_Exp  := 0;

       Put_Line ("Dec_Integer:       "
                 & Dec_Integer'Image);

       Put ("Dec_Real:           ");
       F_IO.Put (Item => Dec_Real);
       New_Line;

       Put ("Dec_Real_Exp:       ");
       F_IO.Put (Item => Dec_Real_Exp);
       New_Line;

       Put_Line ("Based_Integer:     "
                 & Based_Integer'Image);
       Put_Line ("Based_Integer_Exp: "
                 & Based_Integer_Exp'Image);

       Put ("Based_Real:         ");
       F_IO.Put (Item => Based_Real);
       New_Line;

       Put ("Based_Real_Exp:     ");
       F_IO.Put (Item => Based_Real_Exp);
       New_Line;
    end Decimal_Based_Literals;

Based literals use the ``base#number#`` format. Also, they aren't limited to
simple integer literals such as :ada:`16#16D#`. In fact, we can use a radix
point or an exponent in based literals, as well as underscores. In addition, we
can use any base from 2 up to 16. We discuss these aspects further in the next
section.


Features and Flexibility
~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Franco Gasperoni and published as
    `Gem #7: The Beauty of Numeric Literals in Ada <https://www.adacore.com/gems/ada-gem-7>`_.

Ada provides a simple and elegant way of expressing numeric literals. One of
those simple, yet powerful aspects is the ability to use underscores to
separate groups of digits. For example,
:ada:`3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37510` is more
readable and less error prone to type than
:ada:`3.14159265358979323846264338327950288419716939937510`. Here's the
complete code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Pi_Literals
   :class: nosyntax-check

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       Pi   : constant :=
         3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37510;

       Pi2  : constant :=
         3.14159265358979323846264338327950288419716939937510;

       Z    : constant := Pi - Pi2;
       pragma Assert (Z = 0.0);

       use Ada.Text_IO;
    begin
       Put_Line ("Z = " & Float'Image (Z));
    end Ada_Numeric_Literals;

Also, when using based literals, Ada allows any base from 2 to 16. Thus, we can
write the decimal number 136 in any one of the following notations:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Based_Literals

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       Bin_136 : constant := 2#1000_1000#;
       Oct_136 : constant := 8#210#;
       Dec_136 : constant := 10#136#;
       Hex_136 : constant := 16#88#;
       pragma Assert (Bin_136 = 136);
       pragma Assert (Oct_136 = 136);
       pragma Assert (Dec_136 = 136);
       pragma Assert (Hex_136 = 136);

       use Ada.Text_IO;

    begin
       Put_Line ("Bin_136 = "
                 & Integer'Image (Bin_136));
       Put_Line ("Oct_136 = "
                 & Integer'Image (Oct_136));
       Put_Line ("Dec_136 = "
                 & Integer'Image (Dec_136));
       Put_Line ("Hex_136 = "
                 & Integer'Image (Hex_136));
    end Ada_Numeric_Literals;

.. admonition:: In other languages

    The rationale behind the method to specify based literals in the C
    programming language is strange and unintuitive. Here, you have only three
    possible bases: 8, 10, and 16 (why no base 2?). Furthermore, requiring
    that numbers in base 8 be preceded by a zero feels like a bad joke on us
    programmers. For example, what values do :c:`0210` and :c:`210` represent
    in C?

When dealing with microcontrollers, we might encounter I/O devices that are
memory mapped. Here, we have the ability to write:

.. code-block:: ada

        Lights_On  : constant := 2#1000_1000#;
        Lights_Off : constant := 2#0111_0111#;

and have the ability to turn on/off the lights as follows:

.. code-block:: ada

    Output_Devices := Output_Devices or  Lights_On;
    Output_Devices := Output_Devices and Lights_Off;

Here's the complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Literal_Lights

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       Lights_On  : constant := 2#1000_1000#;
       Lights_Off : constant := 2#0111_0111#;

       type Byte is mod 256;
       Output_Devices : Byte := 0;

       --  for Output_Devices'Address
       --    use 16#DEAD_BEEF#;
       --  ^^^^^^^^^^^^^^^^^^^^^^^^^^
       --  Memory mapped Output

       use Ada.Text_IO;
    begin
       Output_Devices := Output_Devices or
                           Lights_On;

       Put_Line ("Output_Devices (lights on ) = "
                 & Byte'Image (Output_Devices));

       Output_Devices := Output_Devices and
                           Lights_Off;

       Put_Line ("Output_Devices (lights off) = "
                 & Byte'Image (Output_Devices));
    end Ada_Numeric_Literals;

Of course, we can also use
:ref:`records with representation clauses <Adv_Ada_Record_Representation_Storage_Clauses>`
to do the above, which is even more elegant.

The notion of base in Ada allows for exponents, which is particularly pleasant.
For instance, we can write:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Literal_Binary

    package Literal_Binaries is
       Kilobyte  : constant := 2#1#e+10;
       Megabyte  : constant := 2#1#e+20;
       Gigabyte  : constant := 2#1#e+30;
       Terabyte  : constant := 2#1#e+40;
       Petabyte  : constant := 2#1#e+50;
       Exabyte   : constant := 2#1#e+60;
       Zettabyte : constant := 2#1#e+70;
       Yottabyte : constant := 2#1#e+80;
    end Literal_Binaries;

In based literals, the exponent |mdash| like the base |mdash| uses the regular
decimal notation and specifies the power of the base that the based literal
should be multiplied with to obtain the final value. For instance
:ada:`2#1#e+10` = 1 x 2\ :sup:`10` = :ada:`1_024` (in base 10), whereas
:ada:`16#F#e+2` = 15 x 16\ :sup:`2` = 15 x 256 = :ada:`3_840` (in
base 10).

Based numbers apply equally well to real literals. We can, for instance, write:

.. code-block:: ada

    One_Third : constant := 3#0.1#;
    --                      ^^^^^^
    --                  same as 1.0/3

Whether we write :ada:`3#0.1#` or :ada:`1.0 / 3`, or even :ada:`3#1.0#e-1`, Ada
allows us to specify exactly rational numbers for which decimal literals cannot
be written.

The last nice feature is that Ada has an open-ended set of integer and real
types. As a result, numeric literals in Ada do not carry with them their type
as, for example, in C. The actual type of the literal is determined from the
context. This is particularly helpful in avoiding overflows, underflows, and
loss of precision.

.. admonition:: In other languages

    In C, a source of confusion can be the distinction between :c:`32l` and
    :c:`321`. Although both look similar, they're actually very different from
    each other.

And this is not all: all constant computations done at compile time are done in
infinite precision, be they integer or real. This allows us to write constants
with whatever size and precision without having to worry about overflow or
underflow. We can for instance write:

.. code-block:: ada

           Zero : constant := 1.0 - 3.0 * One_Third;

and be guaranteed that constant :ada:`Zero` has indeed value zero. This is very
different from writing:

.. code-block:: ada

    One_Third_Approx : constant :=
      0.33333333333333333333333333333;
    Zero_Approx      : constant :=
      1.0 - 3.0 * One_Third_Approx;

where :ada:`Zero_Approx` is really :ada:`1.0e-29` |mdash| and that will show up
in your numerical computations. The above is quite handy when we want to write
fractions without any loss of precision. Here's the complete code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Literals

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       One_Third : constant := 3#1.0#e-1;
       --  same as 1.0/3.0

       Zero      : constant := 1.0 - 3.0 * One_Third;
       pragma Assert (Zero = 0.0);

       One_Third_Approx : constant :=
         0.33333333333333333333333333333;
       Zero_Approx      : constant :=
         1.0 - 3.0 * One_Third_Approx;

       use Ada.Text_IO;

    begin
       Put_Line ("Zero        = "
                 & Float'Image (Zero));
       Put_Line ("Zero_Approx = "
                 & Float'Image (Zero_Approx));
    end Ada_Numeric_Literals;

Along these same lines, we can write:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Literal_Binary

    with Ada.Text_IO;

    with Literal_Binaries; use Literal_Binaries;

    procedure Ada_Numeric_Literals is

       Big_Sum : constant := 1         +
                             Kilobyte  +
                             Megabyte  +
                             Gigabyte  +
                             Terabyte  +
                             Petabyte  +
                             Exabyte   +
                             Zettabyte;

       Result : constant := (Yottabyte - 1) /
                            (Kilobyte - 1);

       Nil    : constant := Result - Big_Sum;
       pragma Assert (Nil = 0);

       use Ada.Text_IO;

    begin
       Put_Line ("Nil         = "
                 & Integer'Image (Nil));
    end Ada_Numeric_Literals;

and be guaranteed that :ada:`Nil` is equal to zero.


.. _Adv_Ada_Universal_Numeric_Types:

Universal Numeric Types
-----------------------

Previously, we introduced the concept of
:ref:`universal types <Adv_Ada_Universal_Types>`. Three of them are numeric
types: universal real, universal integer and universal fixed types. In this
section, we discuss these universal numeric types in more detail.


.. _Adv_Ada_Universal_Real_Integer:

Universal Real and Integer
~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal real and integer types are mainly used in the declaration of
:ref:`named numbers <Adv_Ada_Named_Numbers>`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Universal_Real_Integer

    package Show_Universal_Real_Integer is

       Pi : constant := 3.1415926535;
       --               ^^^^^^^^^^^^
       --            universal real type

       N  : constant := 10;
       --               ^^
       --        universal integer type

    end Show_Universal_Real_Integer;

The type of a named number is implied by the type of the
:ref:`numeric literal <Adv_Ada_Numeric_Literals>` and the type of any named
numbers that we use in the
:ref:`static expression <Adv_Ada_Static_Expressions>`. (We discuss static
expressions next.) In this specific example, we declare :ada:`Pi` using a real
literal, which implies that it's a named number of universal real type.
Likewise, :ada:`N` is of universal integer type because we use an integer
literal in its declaration.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.3.2 Number Declarations <3-3-2>`


.. _Adv_Ada_Static_Expressions:

Static expressions
^^^^^^^^^^^^^^^^^^

As we've just seen, we can use an expression in the declaration of a named
number. This expression is static, as it's always evaluated at compile time.
Therefore, we must use the keyword :ada:`constant` in the declaration of named
numbers.

If all components of the static expression are of universal integer type, then
the named number is of universal integer type. Otherwise, the static expression
is of universal real type. For example, if the first element of a static
expression is of universal integer type, but we have a constant of universal
real type in the same expression, then the type of the whole static expression
is universal real:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Static_Expressions

    package Static_Expressions is

       Two_Pi : constant := 2 * 3.1415926535;
       --                   ^
       --              universal integer type
       --
       --                       ^^^^^^^^^^^^
       --                 universal real type
       --
       --      => result: universal real type

    end Static_Expressions;

In this example, the static expression is of universal real type because of the
real literal (:ada:`3.1415926535`) |mdash| even though we have the universal
integer :ada:`2` in the expression.

Likewise, if we use a constant of universal real type in the static expression,
the result is of universal real type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Static_Expressions

    package Static_Expressions is

       Pi     : constant := 3.1415926535;
       --                   ^^^^^^^^^^^^
       --               universal real type

       Two_Pi : constant := 2 * Pi;
       --                   ^
       --              universal integer type
       --
       --                       ^^
       --                 universal real type
       --
       --      => result: universal real type

    end Static_Expressions;

In this example, the result of the static expression is of universal real type
because of we're using the named number :ada:`Pi`, which is of universal real
type.

Complexity of static expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The operations that we use in static expressions may be arbitrarily complex.
For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Static_Expressions

    package Static_Expressions is

       C1 : constant := 300_453.5;
       C2 : constant := 455_233.5 * C1;
       C3 : constant := 872_922.5 * C2;
       C4 : constant := 155_277.5 * C1 + C2 / C3;
       C5 : constant := 2.0 * C1 +
                        3.0 * (C2 / (C4 * C3)) +
                        4.0 * (C1 / (C2 * C2)) +
                        5.0 * (C3 / (C1 * C1));

    end Static_Expressions;

As we can see in this example, we may create a chain of dependencies, where the
result of a static expression depends on the result of previously evaluated
static expressions. For instance, :ada:`C5` depends on the evaluation of
:ada:`C1`, :ada:`C2`, :ada:`C3`, :ada:`C4`.

Accuracy of static expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The accuracy and range of numeric literals used in static expressions may be
arbitrarily high as well:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Static_Expressions

    package Static_Expressions is

       Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288;

       Seed : constant :=
          143_574_786_272_784_656_928_283_872_972_764;

       Super_Seed : constant :=
          Seed * Seed * Seed * Seed * Seed * Seed;

    end Static_Expressions;

In this example, :ada:`Super_Seed` has a value that is above the typical range
of integer constants. This might become challenging when using such named
numbers in actual computations, as we
:ref:`discuss soon <Adv_Ada_Conversion_Of_Universal_Real_Integer>`.

Another example is when the result of the expression is a
:wikipedia:`repeating decimal <Repeating_decimal>`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Repeating_Decimal

    package Repeating_Decimals is

       One_Over_Three : constant :=
          1.0 / 3.0;

    end Repeating_Decimals;

    with Ada.Text_IO; use Ada.Text_IO;

    with Repeating_Decimals;
    use  Repeating_Decimals;

    procedure Show_Repeating_Decimals is
       F_1_3    : constant Float           :=
                    One_Over_Three;
       LF_1_3   : constant Long_Float      :=
                    One_Over_Three;
       LLF_1_3  : constant Long_Long_Float :=
                    One_Over_Three;
    begin
       Put_Line (F_1_3'Image);
       Put_Line (LF_1_3'Image);
       Put_Line (LLF_1_3'Image);
    end Show_Repeating_Decimals;

In this example, as expected, we see that the accuracy of the value we display
increases if we use a type with higher precision. This wouldn't be possible if
we had used a floating-point type with limited precision for the
:ada:`One_Over_Three` constant:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Repeating_Decimal

    package Repeating_Decimals is

       One_Over_Three : constant Long_Float :=
          1.0 / 3.0;
       --                        ^^^^^^^^^^
       --          using Long_Float instead of
       --              universal real type

    end Repeating_Decimals;

    with Ada.Text_IO; use Ada.Text_IO;

    with Repeating_Decimals;
    use  Repeating_Decimals;

    procedure Show_Repeating_Decimals is
       F_1_3    : constant Float           :=
                    Float (One_Over_Three);
       LF_1_3   : constant Long_Float      :=
                    Long_Float (One_Over_Three);
       LLF_1_3  : constant Long_Long_Float :=
                    Long_Long_Float (One_Over_Three);
    begin
       Put_Line (F_1_3'Image);
       Put_Line (LF_1_3'Image);
       Put_Line (LLF_1_3'Image);
    end Show_Repeating_Decimals;

Because we're using the :ada:`Long_Float` type for the :ada:`One_Over_Three`
constant instead of the universal real type, the accuracy doesn't increase when
we use the :ada:`Long_Long_Float` type |mdash| as we see in the value of the
:ada:`LLF_1_3` constant |mdash| even though this type has a higher precision.

.. admonition:: For further reading...

    When using :ref:`big numbers <Adv_Ada_Big_Numbers>`, you could simply
    assign the named number :ada:`One_Over_Three` to a big real:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Repeating_Decimal switches=Compiler(-gnat2022);

        package Repeating_Decimals is

           One_Over_Three : constant :=
              1.0 / 3.0;

        end Repeating_Decimals;

        with Ada.Text_IO; use Ada.Text_IO;

        with Ada.Numerics.Big_Numbers.Big_Reals;
        use  Ada.Numerics.Big_Numbers.Big_Reals;

        with Repeating_Decimals;
        use  Repeating_Decimals;

        procedure Show_Repeating_Decimals is
           BR_1_3 : constant Big_Real := One_Over_Three;
        begin
           Put_Line ("BR: "
                     & To_String (Arg   => BR_1_3,
                                  Fore  => 2,
                                  Aft   => 31,
                                  Exp   => 0));
        end Show_Repeating_Decimals;

    Another approach is to use the division operation directly:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Repeating_Decimal switches=Compiler(-gnat2022);

        with Ada.Text_IO; use Ada.Text_IO;

        with Ada.Numerics.Big_Numbers.Big_Reals;
        use  Ada.Numerics.Big_Numbers.Big_Reals;

        with Repeating_Decimals;
        use  Repeating_Decimals;

        procedure Show_Repeating_Decimals is
           BR_1_3   : constant Big_Real := 1 / 3;
        begin
           Put_Line ("BR: "
                     & To_String (Arg   => BR_1_3,
                                  Fore  => 2,
                                  Aft   => 31,
                                  Exp   => 0));
        end Show_Repeating_Decimals;

    We talk more about
    :ref:`big real and quotients <Adv_Ada_Big_Real_Quotients>` later on.

.. _Adv_Ada_Conversion_Of_Universal_Real_Integer:

Conversion of universal real and integer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Although a named number exists as an numeric representation form in Ada, the
value it represents cannot be used directly at runtime |mdash| even if we
*just* display the value of the constant at runtime, for example. In fact, a
conversion to a non-universal type is required in order to use the named number
anywhere else other than a static expression:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Conversion_To_Non_Universal_Types

    package Static_Expressions is

       Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288;

       Seed : constant :=
          143_574_786_272_784_656_928_283_872_972_764;

       Super_Seed : constant :=
          Seed * Seed * Seed * Seed * Seed * Seed;

    end Static_Expressions;

    with Ada.Text_IO; use Ada.Text_IO;

    with Static_Expressions;
    use  Static_Expressions;

    procedure Show_Static_Expressions is
    begin
       Put_Line (Pi'Image);
       --  Same as:
       --  Put_Line (Float (Pi)'Image);

       Put_Line (Seed'Image);
       --  Same as:
       --  Put_Line (
       --    Long_Long_Long_Integer (Seed)'Image);
    end Show_Static_Expressions;

As we see in this example, the named number :ada:`Pi` is converted to
:ada:`Float` before being used as an actual parameter in the call to
:ada:`Put_Line`. Similarly, :ada:`Seed` is converted to
:ada:`Long_Long_Long_Integer`.

When we use the :ada:`Image` attribute, the compiler automatically selects a
numeric type which has a suitable range for the named number. In the example
above, we wouldn't be able to represent the value of :ada:`Seed` with
:ada:`Integer`, so the compiler selected :ada:`Long_Long_Long_Integer`. Of
course, we could have also specified the type by using explicit
:ref:`type conversions <Adv_Ada_Type_Conversion>` or a
:ref:`qualified expressions <Adv_Ada_Qualified_Expressions>`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Conversion_To_Non_Universal_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Static_Expressions;
    use  Static_Expressions;

    procedure Show_Static_Expressions is
    begin
       Put_Line (Long_Long_Float (Pi)'Image);
       Put_Line (Long_Long_Float'(Pi)'Image);
    end Show_Static_Expressions;

Now, we're explicitly converting to :ada:`Long_Long_Float` in the first call
to :ada:`Put_Line` and using a qualified expression in the second call to
:ada:`Put_Line`.

A conversion is also performed when we use a named number in an object
declaration:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Conversion_To_Non_Universal_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Static_Expressions;
    use  Static_Expressions;

    procedure Show_Static_Expressions is
       Two_Pi : constant Float := 2.0 * Pi;
       --  Same as:
       --  Two_Pi: constant Float :=
       --            2.0 * Float (Pi);

       Two_Pi_More_Precise :
         constant Long_Long_Float := 2.0 * Pi;
       --  Same as:
       --  Two_Pi_More_Precise :
       --    constant Long_Long_Float :=
       --      2.0 * Long_Long_Float (Pi);
    begin
       Put_Line (Two_Pi'Image);
       Put_Line (Two_Pi_More_Precise'Image);
    end Show_Static_Expressions;

In this example, :ada:`Pi` is converted to :ada:`Float` in the declaration of
:ada:`Two_Pi` because we use the :ada:`Float` type in its declaration.
Likewise, :ada:`Pi` is converted to :ada:`Long_Long_Float` in the declaration
of :ada:`Two_Pi_More_Precise` because we use the :ada:`Long_Long_Float` type in
its declaration. (Actually, the same conversion is performed for each instance
of the real literal :ada:`2.0` in this example.)

Note that the range of the type we select might not be suitable for the named
number we want to use. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Conversion_To_Non_Universal_Types
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    with Static_Expressions;
    use  Static_Expressions;

    procedure Show_Static_Expressions is
       Initial_Seed : constant
         Long_Long_Long_Integer :=
           Super_Seed;
    begin
       Put_Line (Initial_Seed'Image);
    end Show_Static_Expressions;

In this example, we get a compilation error because the range of the
:ada:`Long_Long_Long_Integer` type isn't enough to store the value of the
:ada:`Super_Seed`.

.. admonition:: For further reading...

    To circumvent the compilation error in the code example we've just seen,
    the best alternative to use :ref:`big numbers <Adv_Ada_Big_Numbers>`
    |mdash| we discuss this topic later on in this chapter:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Numeric_Types.Conversion_To_Non_Universal_Types switches=Compiler(-gnat2022);

        with Ada.Text_IO; use Ada.Text_IO;

        with Ada.Numerics.Big_Numbers.Big_Integers;
        use  Ada.Numerics.Big_Numbers.Big_Integers;

        with Static_Expressions;
        use  Static_Expressions;

        procedure Show_Static_Expressions is
           Initial_Seed : constant
             Big_Integer :=
               Super_Seed;
        begin
           Put_Line (Initial_Seed'Image);
        end Show_Static_Expressions;

    By changing the type from :ada:`Long_Long_Long_Integer` to
    :ada:`Big_Integer`, we get rid of the compilation error. (The value of
    :ada:`Super_Seed` |mdash| stored in :ada:`Initial_Seed` |mdash| is
    displayed at runtime.)


.. _Adv_Ada_Universal_Fixed:

Universal Fixed
~~~~~~~~~~~~~~~

For fixed-point types, we also have a corresponding universal type. However, in
contrast to the universal real and integer types, universal fixed types aren't
an abstraction used in static expressions, but rather a concept that permeates
actual fixed-point types. In fact, for
:ref:`fixed-point types <Adv_Ada_Fixed_Point_Type_Attributes>`, some operations
are accomplished via universal fixed types |mdash| for example, the conversion
between fixed-point types and the multiplication and division operations.

Let's start by analyzing how floating-point and integer types associate their
operations to the specific type of an object. For example, if we have an object
:ada:`A` of type :ada:`Float` in a multiplication, we cannot just write
:ada:`A * B` if we want to multiply :ada:`A` by an object :ada:`B` of another
floating-point type |mdash| if :ada:`B` is of type :ada:`Long_Float`, for
example, writing :ada:`A * B` triggers a compilation error. (Otherwise, which
precision should be used for the result?) Therefore, we have
to convert one of the objects to have matching types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Types.Float_Multiplication
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Float_Multiplication_Mismatch is
       F  : Float      := 0.25;
       LF : Long_Float := 0.50;
    begin
       F := F * LF;
       Put_Line ("F = " & F'Image);
    end Show_Float_Multiplication_Mismatch;

This code example fails to compile because of the :ada:`F * LF` operation.
(We could correct the code by writing :ada:`F * Float (LF)`, for example.)

In contrast, for fixed-point types, we can mix objects of different types in a
multiplication or division. (In this case, mixing is allowed for the
convenience of the programmer.) For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Types.Fixed_Point_Multiplication

    package Normalized_Fixed_Point_Types is

       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);

       type TQ15 is
         delta 2.0 ** (-15)
         range -1.0 .. 1.0 - 2.0 ** (-15);

    end Normalized_Fixed_Point_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Normalized_Fixed_Point_Types;
    use  Normalized_Fixed_Point_Types;

    procedure Show_Fixed_Multiplication is
       A : TQ15 := 0.25;
       B : TQ31 := 0.50;
    begin
       A := A * B;
       Put_Line ("A = " & A'Image);
    end Show_Fixed_Multiplication;

In this example, the :ada:`A * B` is accepted by the compiler, even though
:ada:`A` and :ada:`B` have different types. This is only possible because the
multiplication operation of fixed-point types makes use of the universal fixed
type. In other words, the multiplication operation in this code example doesn't
operate directly on the fixed-point type :ada:`TQ31`. Instead, it converts
:ada:`A` and :ada:`B` to the universal fixed type, performs the operation using
this type, and converts back to the original type |mdash| :ada:`TQ15` in this
case.

In addition to the multiplication operation, other operations such as the
conversion between fixed-point types and the division operations make use of
universal fixed types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Types.Universal_Fixed

    package Custom_Decimal_Types is

      type T3_D3 is delta 10.0 ** (-3) digits 3;
      type T3_D6 is delta 10.0 ** (-3) digits 6;
      type T6_D6 is delta 10.0 ** (-6) digits 6;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Universal_Fixed is
      Val_T3_D3 : T3_D3;
      Val_T3_D6 : T3_D6;
      Val_T6_D6 : T6_D6;
    begin
       Val_T3_D3 := 0.65;

       Val_T3_D6 := T3_D6 (Val_T3_D3);
       --           ^^^^^^^^^^^^^^^^^
       --      type conversion using
       --       universal fixed type

       Val_T6_D6 := T6_D6 (Val_T3_D6);
       --           ^^^^^^^^^^^^^^^^^
       --      type conversion using
       --       universal fixed type

       Put_Line ("Val_T3_D3 = "
                 & Val_T3_D3'Image);
       Put_Line ("Val_T3_D6 = "
                 & Val_T3_D6'Image);
       Put_Line ("Val_T6_D6 = "
                 & Val_T3_D6'Image);
       Put_Line ("-----------------");

       Val_T3_D6 := Val_T6_D6 * 2.0;
       --           ^^^^^^^^^^^^^^^^
       --    using universal fixed type for
       --      the multiplication operation
       Put_Line ("Val_T3_D6 = "
                 & Val_T3_D6'Image);

       Val_T3_D6 := Val_T6_D6 / Val_T3_D3;
       --           ^^^^^^^^^^^^^^^^^^^^^
       --      different fixed-point types:
       --    using universal fixed type for
       --           the division operation
       Put_Line ("Val_T3_D6 = "
                 & Val_T3_D6'Image);

    end Show_Universal_Fixed;

In this example, the conversion from the fixed-point type :ada:`T3_D3` to the
:ada:`T3_D6` and :ada:`T6_D6` types is performed via universal fixed types.

Similarly, the multiplication operation :ada:`Val_T6_D6 * 2.0` uses universal
fixed types. Here, we're actually multiplying a variable of type :ada:`T6_D6`
by two and assigning it to a variable of type :ada:`Val_T3_D6`. Although these
variable have different fixed-point types, no explicit conversion (e.g.:
:ada:`Val_T3_D6 := T3_D6 (Val_T6_D6 * 2.0);`) is required in this case because
the result of the operation is of universal fixed type, so that it can be
assigned to a variable of any fixed-point type.

Finally, in the :ada:`Val_T3_D6 := Val_T6_D6 / Val_T3_D3` statement, we're
using three fixed-point types: we're dividing a variable of type :ada:`T6_D6`
by a variable of type :ada:`T3_D3`, and assigning it to a variable of type
:ada:`T3_D6`. All these operations are only possible without explicit type
conversions because the underlying types for the fixed-point division operation
are universal fixed types.

.. admonition:: For further reading...

    It's possible to implement custom :ada:`*` and :ada:`/` operators for
    fixed-point types. However, those operators do **not** override the
    corresponding operators for universal fixed-point types. For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Universal_Types.Fixed_Point_Custom_Multiplication

        package Normalized_Fixed_Point_Types is

           type TQ63 is
             delta 2.0 ** (-63)
             range -1.0 .. 1.0 - 2.0 ** (-63);

           type TQ31 is
             delta 2.0 ** (-31)
             range -1.0 .. 1.0 - 2.0 ** (-31);

           overriding
           --  ^^^^^^
           --  "+" operator is overriding!
           function "+" (L, R : TQ31)
                         return TQ31;

           not overriding
           --  ^^^^^^^^^^
           --  "*" operator is NOT overriding!
           function "*" (L, R : TQ31)
                         return TQ31;

           type TQ15 is
             delta 2.0 ** (-15)
             range -1.0 .. 1.0 - 2.0 ** (-15);

        end Normalized_Fixed_Point_Types;

        with Ada.Text_IO; use Ada.Text_IO;

        package body Normalized_Fixed_Point_Types is

           function "+" (L, R : TQ31)
                         return TQ31 is
           begin
              Put_Line
                ("=> Overriding '+'");
              return TQ31 (TQ63 (L) + TQ63 (R));
           end "+";

           function "*" (L, R : TQ31)
                         return TQ31 is
           begin
              Put_Line
                ("=> Custom "
                 & "non-overriding '*'");
              return TQ31 (TQ63 (L) * TQ63 (R));
           end "*";

        end Normalized_Fixed_Point_Types;

        with Ada.Text_IO; use Ada.Text_IO;

        with Normalized_Fixed_Point_Types;
        use  Normalized_Fixed_Point_Types;

        procedure Show_Fixed_Multiplication is
           Q31_A : TQ31 := 0.25;
           Q31_B : TQ31 := 0.50;
           Q15_A : TQ15 := 0.25;
           Q15_B : TQ15 := 0.50;
        begin
           Q31_A := Q31_A * Q31_B;
           Put_Line ("Q31_A = " & Q31_A'Image);

           Q15_A := Q15_A * Q15_B;
           Put_Line ("Q15_A = " & Q31_A'Image);

           Q15_A := TQ15 (Q31_A) * Q15_B;
           --       ^^^^^^^^^^^^
           --  A conversion is required because of
           --  the multiplication operator of
           --  TQ15.
           Put_Line ("Q31_A = " & Q31_A'Image);
        end Show_Fixed_Multiplication;

    In this example, we're declaring a custom multiplication operator for the
    :ada:`TQ31` type. As we can see in the declaration, we specify that it's
    :ada:`not overriding` the :ada:`*` operator. (Removing the :ada:`not`
    keyword triggers a compilation error.) In contrast, for the :ada:`+`
    operator, we're indeed overriding the default :ada:`+` operator of the
    :ada:`TQ31` type in the :ada:`Normalized_Fixed_Point_Types` because the
    addition operator is associated with its corresponding fixed-point type,
    not with the universal fixed-point type. In the
    :ada:`Q31_A := Q31_A * Q31_B` statement, we see at runtime (through the
    "=> Custom non-overriding '*'" message) that the custom
    multiplication is being used.

    However, because of this custom :ada:`*` operator, we cannot mix objects of
    this type with objects of other fixed-point types in multiplication or
    division operations. Therefore, for a statement such as
    :ada:`Q15_A := Q31_A * Q15_B`, we have to convert :ada:`Q31_A` to the
    :ada:`TQ15` type before multiplying it by :ada:`Q15_B`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.5.5 Multiplying Operators <4-5-5>`


.. _Adv_Ada_Base_Types:

Base types
----------

You might remember our discussion on :ref:`root types <Adv_Ada_Root_Types>`
and the corresponding numeric root types.

Ada also has the concept of base types, which *sounds* similar to the
concept of the root type. However, the focus of each one is different:
while the the root type refers to the derivation tree of a type, the base
type refers to the constraints of a type.

In fact, the base type denotes the unconstrained underlying hardware
representation selected for a given numeric type. For example, if we were
making use of a constrained type :ada:`T`, the compiler would select a type
based on the hardware characteristics that has sufficient precision to
represent :ada:`T` on the target platform. Of course, that type |mdash| the
base type |mdash| would necessarily be unconstrained.

Let's discuss the :ada:`Integer` type as an example.
The Ada standard specifies that the minimum range of the :ada:`Integer` type
is :ada:`-2**15 + 1 .. 2**15 - 1`. In modern 64-bit systems |mdash|
where wider types such as :ada:`Long_Integer` are defined |mdash| the range
is at least :ada:`-2**31 + 1 .. 2**31 - 1`. Therefore, we could think of
the :ada:`Integer` type as having the following declaration:

.. code-block:: ada

    type Integer is
      range -2 ** 31 .. 2 ** 31 - 1;

However, even though :ada:`Integer` is a predefined Ada type, it's actually
a subtype of an anonymous type. That anonymous "type" is the hardware's
representation for the numeric type as chosen by the compiler based on the
requested range (for the signed integer types) or digits of precision (for
floating-point types). In other words, these types are actually subtypes of
something that does not have a specific name in Ada, and that is not
constrained.

In effect,

.. code-block:: ada

    type Integer is
      range -2 ** 31 .. 2 ** 31 - 1;

is really as if we said this:

.. code-block:: ada

    subtype Integer is
      Some_Hardware_Type_With_Sufficient_Range
      range -2 ** 31 .. 2 ** 31 - 1;

Since the :ada:`Some_Hardware_Type_With_Sufficient_Range` type is anonymous
and we therefore cannot refer to it in the code, we just say that
:ada:`Integer` is a type rather than a subtype.

Let's focus on signed integers |mdash| as the other numerics work the same
way. When we declare a signed integer type, we have to specify the required
range, statically. If the compiler cannot find a hardware-defined or
supported signed integer type with at least the range requested, the
compilation is rejected. For example, in current architectures, the code
below most likely won't compile:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Base_Type.Very_Big_Range
    :class: ada-expect-compile-error

    package Int_Def is

        type Too_Big_To_Fail is
          range -2 ** 255 .. 2 ** 255 - 1;

    end Int_Def;

Otherwise, the compiler maps the named Ada type to the hardware "type",
presumably choosing the smallest one that supports the requested range.
(That's why the range has to be static in the source code, unlike for
explicit subtypes.)


.. _Adv_Ada_Base_Attribute:

Base
~~~~

The :ada:`Base` attribute gives us the unconstrained underlying hardware
representation selected for a given numeric type. As an example, let's say we
declared a subtype of the :ada:`Integer` type named :ada:`One_To_Ten`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Base_Type.Base_Attr

    package My_Integers is

       subtype One_To_Ten is Integer
         range 1 .. 10;

    end My_Integers;

If we then use the :ada:`Base` attribute |mdash| by writing
:ada:`One_To_Ten'Base` |mdash|, we're actually referring to the unconstrained
underlying hardware representation selected for :ada:`One_To_Ten`. As
:ada:`One_To_Ten` is a subtype of the :ada:`Integer` type, this also means that
:ada:`One_To_Ten'Base` is equivalent to :ada:`Integer'Base`, i.e. they refer to
the same base type. (This base type is the underlying hardware type
representing the :ada:`Integer` type |mdash| but is not the :ada:`Integer` type
itself.)

The following example shows how the :ada:`Base` attribute affects the bounds of
a variable:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Base_Type.Base_Attr

    with Ada.Text_IO; use Ada.Text_IO;
    with My_Integers; use My_Integers;

    procedure Show_Base is
       C : constant One_To_Ten := One_To_Ten'Last;
    begin
       Using_Constrained_Subtype : declare
          V : One_To_Ten := C;
       begin
          Put_Line
            ("Increasing value for One_To_Ten...");

          V := One_To_Ten'Succ (V);
       exception
          when others =>
             Put_Line ("Exception raised!");
       end Using_Constrained_Subtype;

       Using_Base : declare
          V : One_To_Ten'Base := C;
       begin
          Put_Line
          ("Increasing value for One_To_Ten'Base...");

          V := One_To_Ten'Succ (V);
       exception
          when others =>
             Put_Line ("Exception raised!");
       end Using_Base;

       Put_Line ("One_To_Ten'Last: "
                 & One_To_Ten'Last'Image);
       Put_Line ("One_To_Ten'Base'Last: "
                 & One_To_Ten'Base'Last'Image);
    end Show_Base;

In the first block of the example (:ada:`Using_Constrained_Subtype`), we're
asking for the next value after the last value of a range |mdash| in this case,
:ada:`One_To_Ten'Succ (One_To_Ten'Last)`. As expected, since the last value of
the range doesn't have a successor, a constraint exception is raised.

In the :ada:`Using_Base` block, we're declaring a variable :ada:`V` of
:ada:`One_To_Ten'Base` subtype. In this case, the next value exists |mdash|
because the condition :ada:`One_To_Ten'Last + 1 <= One_To_Ten'Base'Last` is
true |mdash|, so we can use the :ada:`Succ` attribute without having an
exception being raised.

In the following example, we adjust the result of additions and subtractions
to avoid constraint errors:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Base_Type.Base_Attr_Sat

    package My_Integers is

       subtype One_To_Ten is Integer range 1 .. 10;

       function Sat_Add (V1, V2 : One_To_Ten'Base)
                         return One_To_Ten;

       function Sat_Sub (V1, V2 : One_To_Ten'Base)
                         return One_To_Ten;

    end My_Integers;

    --  with Ada.Text_IO; use Ada.Text_IO;

    package body My_Integers is

       function Saturate (V : One_To_Ten'Base)
                          return One_To_Ten is
       begin
          --  Put_Line ("SATURATE " & V'Image);

          if V < One_To_Ten'First then
             return One_To_Ten'First;
          elsif V > One_To_Ten'Last then
             return One_To_Ten'Last;
          else
             return V;
          end if;
       end Saturate;

       function Sat_Add (V1, V2 : One_To_Ten'Base)
                         return One_To_Ten is
       begin
          return Saturate (V1 + V2);
       end Sat_Add;

       function Sat_Sub (V1, V2 : One_To_Ten'Base)
                         return One_To_Ten is
       begin
          return Saturate (V1 - V2);
       end Sat_Sub;

    end My_Integers;

    with Ada.Text_IO; use Ada.Text_IO;
    with My_Integers; use My_Integers;

    procedure Show_Base is

       type Display_Saturate_Op is (Add, Sub);

       procedure Display_Saturate
         (V1, V2 : One_To_Ten;
          Op     : Display_Saturate_Op)
       is
          Res : One_To_Ten;
       begin
          case Op is
          when Add =>
             Res := Sat_Add (V1, V2);
          when Sub =>
             Res := Sat_Sub (V1, V2);
          end case;
          Put_Line ("SATURATE " & Op'Image
                    & " (" & V1'Image
                    & ", " & V2'Image
                    & ") = " & Res'Image);
       end Display_Saturate;

    begin
       Display_Saturate (1,  1, Add);
       Display_Saturate (10, 8, Add);
       Display_Saturate (1,  8, Sub);
    end Show_Base;

In this example, we're using the :ada:`Base` attribute to declare the
parameters of the :ada:`Sat_Add`, :ada:`Sat_Sub` and :ada:`Saturate` functions.
Note that the parameters of the :ada:`Display_Saturate` procedure are of
:ada:`One_To_Ten` type, while the parameters of the :ada:`Sat_Add`,
:ada:`Sat_Sub` and :ada:`Saturate` functions are of the (unconstrained) base
subtype (:ada:`One_To_Ten'Base`). In those functions, we perform operations
using the parameters of unconstrained subtype and adjust the result |mdash| in
the :ada:`Saturate` function |mdash| before returning it as a constrained value
of :ada:`One_To_Ten` subtype.

The code in the body of the :ada:`My_Integers` package contains lines that were
commented out |mdash| to be more precise, a call to :ada:`Put_Line` call in the
:ada:`Saturate` function. If you uncomment them, you'll see the value of the
input parameter :ada:`V` (of :ada:`One_To_Ten'Base` type) in the runtime output
of the program before it's adapted to fit the constraints of the
:ada:`One_To_Ten` subtype.


.. _Adv_Ada_Discrete_Numeric_Types:

Discrete Numeric Types
----------------------





.. _Adv_Ada_Real_Numeric_Types:

Real Numeric Types
------------------

In the Introduction to Ada course, we talked about
:ref:`floating-point <Intro_Ada_Floating_Point_Types>` and
:doc:`fixed-point </courses/intro-to-ada/chapters/fixed_point_types>` types.
In Ada, these two categories of numeric types belong to the so-called *real
types*. In very simple terms, we could say that real types are the ones whose
objects we could assign
:ref:`real numeric literals <Adv_Ada_Numeric_Literals>` to. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Real_Numeric_Types.Universal_And_Real_Numeric_Types

    procedure Show_Real_Numeric_Object is
        V : Float;
    begin
       V := 2.3333333333;
       --   ^^^^^^^^^^^^
       --  real numeric literal
    end Show_Real_Numeric_Object;

Note that we shouldn't confuse real numeric types with
:ref:`universal real types <Adv_Ada_Universal_Real_Integer>`. Even though we
can assign a named number of universal real type to an object of a real type,
these terms refer to very distinct concepts. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Real_Numeric_Types.Universal_And_Real_Numeric_Types

    package Universal_And_Real_Numeric_Types is

       Pi : constant := 3.1415926535;
       --               ^^^^^^^^^^^^
       --            universal real type

       V : Float     := Pi;
       --  ^^^^^
       --  real type
       --  (floating-point type)
       --

    end Universal_And_Real_Numeric_Types;

In this example, :ada:`Pi` is a named number of universal real type, while
:ada:`V` is an object of real type |mdash| and of floating-point type, to be
more precise.

Note that both real types and universal real types are implicitly derived from
the :ref:`root real type <Adv_Ada_Root_Types>`, which we already discussed in
another chapter.

In the next two sections, we discuss further details about
floating-point and
fixed-point types.

.. todo::

    Add link to section on floating-point types <Adv_Ada_Floating_Point_Types>
    when it has become available!

.. todo::

    Add link to section on fixed-point types <Adv_Ada_Fixed_Point_Types>
    when it has become available!

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.6 Real Types <3-5-6>`


.. ::

    .. _Adv_Ada_Floating_Point_Types:

    Floating-point types
    --------------------

    .. todo::

        Complete section!


.. ::

    .. _Adv_Ada_Fixed_Point_Types:

    Fixed-point types
    -----------------

    .. todo::

        Complete section!


.. _Adv_Ada_Big_Numbers:

Big Numbers
-----------

As we've seen before, we can define numeric types in Ada with a high degree of
precision. However, these normal numeric types in Ada are limited to what
the underlying hardware actually supports. For example, any signed integer
type |mdash| whether defined by the language or the user |mdash| cannot have a
range greater than that of :ada:`System.Min_Int .. System.Max_Int` because
those constants reflect the actual hardware's signed integer types. In certain
applications, that precision might not be enough, so we have to rely on
:wikipedia:`arbitrary-precision arithmetic <arbitrary-precision_arithmetic>`.
These so-called "big numbers" are limited conceptually only by available
memory, in contrast to the underlying hardware-defined numeric types.

Ada supports two categories of big numbers: big integers and big reals |mdash|
both are specified in child packages of the :ada:`Ada.Numerics.Big_Numbers`
package:

+--------------+----------------------------------------------+
| Category     | Package                                      |
+==============+==============================================+
| Big Integers | :ada:`Ada.Numerics.Big_Numbers.Big_Integers` |
+--------------+----------------------------------------------+
| Big Reals    | :ada:`Ada.Numerics.Big_Numbers.Big_Real`     |
+--------------+----------------------------------------------+

.. admonition:: In the Ada Reference Manual

    - :arm22:`Big Numbers <A-5-5>`
    - :arm22:`Big Integers <A-5-6>`
    - :arm22:`Big Reals <A-5-7>`

Overview
~~~~~~~~

Let's start with a simple declaration of big numbers:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Numbers switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Simple_Big_Numbers is
       BI : Big_Integer;
       BR : Big_Real;
    begin
       BI := 12345678901234567890;
       BR := 2.0 ** 1234;

       Put_Line ("BI: " & BI'Image);
       Put_Line ("BR: " & BR'Image);

       BI := BI + 1;
       BR := BR + 1.0;

       Put_Line ("BI: " & BI'Image);
       Put_Line ("BR: " & BR'Image);
    end Show_Simple_Big_Numbers;

In this example, we're declaring the big integer :ada:`BI` and the big real
:ada:`BR`, and we're incrementing them by one.

Naturally, we're not limited to using the :ada:`+` operator (such as in this
example). We can use the same operators on big numbers that we can use with
normal numeric types. In fact, the common unary operators
(:ada:`+`, :ada:`-`, :ada:`abs`) and binary operators (:ada:`+`, :ada:`-`,
:ada:`*`, :ada:`/`, :ada:`**`, :ada:`Min` and :ada:`Max`) are available to us.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Numbers_Operators switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Simple_Big_Numbers_Operators is
       BI : Big_Integer;
    begin
       BI := 12345678901234567890;

       Put_Line ("BI: " & BI'Image);

       BI := -BI + BI / 2;
       BI :=  BI - BI * 2;

       Put_Line ("BI: " & BI'Image);
    end Show_Simple_Big_Numbers_Operators;

In this example, we're applying the four basic operators (:ada:`+`, :ada:`-`,
:ada:`*`, :ada:`/`) on big integers.


Factorial
~~~~~~~~~

A typical example is the :wikipedia:`factorial <Factorial>`: a sequence of the
factorial of consecutive small numbers can quickly lead to big numbers. Let's
take this implementation as an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Factorial_Integer switches=Compiler(-gnat2022);
    :class: ada-run-expect-failure

    function Factorial (N : Integer)
                        return Long_Long_Integer;

    function Factorial (N : Integer)
                        return Long_Long_Integer is
       Fact : Long_Long_Integer := 1;
    begin
       for I in 2 .. N loop
          Fact := Fact * Long_Long_Integer (I);
       end loop;

       return Fact;
    end Factorial;

    with Ada.Text_IO; use Ada.Text_IO;

    with Factorial;

    procedure Show_Factorial is
    begin
       for I in 1 .. 50 loop
          Put_Line (I'Image & "! = "
                    & Factorial (I)'Image);
       end loop;
    end Show_Factorial;

Here, we're using :ada:`Long_Long_Integer` for the computation and return type
of the :ada:`Factorial` function. (We're using :ada:`Long_Long_Integer` because
its range is probably the biggest possible on the machine, although that is not
necessarily so.) The last number we're able to calculate
before getting an exception is `20!`, which basically shows the limitation of
standard integers for this kind of algorithm. If we use big integers instead,
we can easily display all numbers up to `50!` (and more!):

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Factorial_Big_Numbers switches=Compiler(-gnat2022);

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    function Factorial (N : Integer)
                        return Big_Integer;

    function Factorial (N : Integer)
                        return Big_Integer is
       Fact : Big_Integer := 1;
    begin
       for I in 2 .. N loop
          Fact := Fact * To_Big_Integer (I);
       end loop;

       return Fact;
    end Factorial;

    with Ada.Text_IO; use Ada.Text_IO;

    with Factorial;

    procedure Show_Big_Number_Factorial is
    begin
       for I in 1 .. 50 loop
          Put_Line (I'Image & "! = "
                    & Factorial (I)'Image);
       end loop;
    end Show_Big_Number_Factorial;

As we can see in this example, replacing the :ada:`Long_Long_Integer` type by
the :ada:`Big_Integer` type fixes the problem (the runtime exception) that we
had in the previous version.
(Note that we're using the :ada:`To_Big_Integer` function to convert from
:ada:`Integer` to :ada:`Big_Integer`: we discuss these conversions next.)

Note that there is a limit to the upper bounds for big integers. However, this
limit isn't dependent on the hardware types |mdash| as it's the case for normal
numeric types |mdash|, but rather compiler specific. In other words, the
compiler can decide how much memory it wants to use to represent big integers.


Conversions
~~~~~~~~~~~

Most probably, we want to mix big numbers and *standard* numbers (i.e. integer
and real numbers) in our application. In this section, we talk about the
conversion between big numbers and standard types.

Validity
^^^^^^^^

The package specifications of big numbers include subtypes that *ensure*
that a actual value of a big number is valid:

+------------------------------+---------------------------------------------+
| Type                         | Subtype for valid values                    |
+==============================+=============================================+
| Big Integers                 | :ada:`Valid_Big_Integer`                    |
+------------------------------+---------------------------------------------+
| Big Reals                    | :ada:`Valid_Big_Real`                       |
+------------------------------+---------------------------------------------+

These subtypes include a contract for this check. For example, this is the
definition of the :ada:`Valid_Big_Integer` subtype:

.. code-block:: ada

    subtype Valid_Big_Integer is Big_Integer
      with Dynamic_Predicate =>
               Is_Valid (Valid_Big_Integer),
           Predicate_Failure =>
               (raise Program_Error);

Any operation on big numbers is actually performing this validity check (via a
call to the :ada:`Is_Valid` function). For example, this is the addition
operator for big integers:

.. code-block:: ada

    function "+" (L, R : Valid_Big_Integer)
                  return Valid_Big_Integer;

As we can see, both the input values to the operator as well as the return
value are expected to be valid |mdash| the :ada:`Valid_Big_Integer` subtype
triggers this check, so to say. This approach ensures that an algorithm
operating on big numbers won't be using invalid values.


Conversion functions
^^^^^^^^^^^^^^^^^^^^

These are the most important functions to convert between big number and
*standard* types:

+--------------+-------------------------------------------------+---------------------------------------+
| Category     | To big number                                   | From big number                       |
+==============+=================================================+=======================================+
| Big Integers | * :ada:`To_Big_Integer`                         | * :ada:`To_Integer` (:ada:`Integer`)  |
|              |                                                 |                                       |
|              |                                                 | * :ada:`From_Big_Integer`             |
|              |                                                 |   (other integer types)               |
+--------------+-------------------------------------------------+---------------------------------------+
| Big Reals    | * :ada:`To_Big_Real` (floating-point types or   | * :ada:`From_Big_Real`                |
|              |   fixed-point types)                            |                                       |
|              +-------------------------------------------------+---------------------------------------+
|              | * :ada:`To_Big_Real` (:ada:`Valid_Big_Integer`) | * :ada:`Numerator`,                   |
|              |                                                 |   :ada:`Denominator` (:ada:`Integer`) |
|              | * :ada:`To_Real` (:ada:`Integer`)               |                                       |
+--------------+-------------------------------------------------+---------------------------------------+

In the following sections, we discuss these functions in more detail.


Big integer to integer
^^^^^^^^^^^^^^^^^^^^^^

We use the :ada:`To_Big_Integer` and :ada:`To_Integer` functions to convert
back and forth between :ada:`Big_Integer` and :ada:`Integer` types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Integer_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Simple_Big_Integer_Conversion is
       BI : Big_Integer;
       I  : Integer := 10000;
    begin
       BI := To_Big_Integer (I);
       Put_Line ("BI: " & BI'Image);

       I := To_Integer (BI + 1);
       Put_Line ("I:  " & I'Image);
    end Show_Simple_Big_Integer_Conversion;

In addition, we can use the generic :ada:`Signed_Conversions` and
:ada:`Unsigned_Conversions` packages to convert between :ada:`Big_Integer` and
any signed or unsigned integer types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Arbitrary_Big_Integer_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Arbitrary_Big_Integer_Conversion is

       type Mod_32_Bit is mod 2 ** 32;

       package Long_Long_Integer_Conversions is new
         Signed_Conversions (Long_Long_Integer);
       use Long_Long_Integer_Conversions;

       package Mod_32_Bit_Conversions is new
         Unsigned_Conversions (Mod_32_Bit);
       use Mod_32_Bit_Conversions;

       BI   : Big_Integer;
       LLI  : Long_Long_Integer := 10000;
       U_32 : Mod_32_Bit        := 2 ** 32 + 1;

    begin
       BI := To_Big_Integer (LLI);
       Put_Line ("BI:   " & BI'Image);

       LLI := From_Big_Integer (BI + 1);
       Put_Line ("LLI:  " & LLI'Image);

       BI := To_Big_Integer (U_32);
       Put_Line ("BI:   " & BI'Image);

       U_32 := From_Big_Integer (BI + 1);
       Put_Line ("U_32: " & U_32'Image);

    end Show_Arbitrary_Big_Integer_Conversion;

In this examples, we declare the :ada:`Long_Long_Integer_Conversions` and the
:ada:`Mod_32_Bit_Conversions` to be able to convert between big integers and
the :ada:`Long_Long_Integer` and the :ada:`Mod_32_Bit` types, respectively.

Note that, when converting from big integer to integer, we used the
:ada:`To_Integer` function, while, when using the instances of the generic
packages, the function is named :ada:`From_Big_Integer`.


Big real to floating-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When converting between big real and floating-point types, we have to
instantiate the generic :ada:`Float_Conversions` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Floating_Point_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Real_Floating_Point_Conversion
    is
       type D10 is digits 10;

       package D10_Conversions is new
         Float_Conversions (D10);
       use D10_Conversions;

       package Long_Float_Conversions is new
         Float_Conversions (Long_Float);
       use Long_Float_Conversions;

       BR  : Big_Real;
       LF  : Long_Float := 2.0;
       F10 : D10        := 1.999;

    begin
       BR := To_Big_Real (LF);
       Put_Line ("BR:   " & BR'Image);

       LF := From_Big_Real (BR + 1.0);
       Put_Line ("LF:   " & LF'Image);

       BR := To_Big_Real (F10);
       Put_Line ("BR:   " & BR'Image);

       F10 := From_Big_Real (BR + 0.1);
       Put_Line ("F10:  " & F10'Image);

    end Show_Big_Real_Floating_Point_Conversion;

In this example, we declare the :ada:`D10_Conversions` and the
:ada:`Long_Float_Conversions` to be able to convert between big reals and
the custom floating-point type :ada:`D10` and the :ada:`Long_Float` type,
respectively. To do that, we use the :ada:`To_Big_Real` and the
:ada:`From_Big_Real` functions.


Big real to fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When converting between big real and ordinary fixed-point types, we have to
instantiate the generic :ada:`Fixed_Conversions` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Fixed_Point_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Real_Fixed_Point_Conversion
    is
       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;

       package TQ31_Conversions is new
         Fixed_Conversions (TQ31);
       use TQ31_Conversions;

       BR   : Big_Real;
       FQ31 : TQ31 := 0.25;

    begin
       BR := To_Big_Real (FQ31);
       Put_Line ("BR:   " & BR'Image);

       FQ31 := From_Big_Real (BR * 2.0);
       Put_Line ("FQ31: " & FQ31'Image);

    end Show_Big_Real_Fixed_Point_Conversion;

In this example, we declare the :ada:`TQ31_Conversions` to be able to convert
between big reals and the custom fixed-point type :ada:`TQ31` type.
Again, we use the :ada:`To_Big_Real` and the :ada:`From_Big_Real` functions for
the conversions.

Note that there's no direct way to convert between decimal fixed-point types
and big real types. (Of course, you could perform this conversion indirectly
by using a floating-point or an ordinary fixed-point type in between.)


Big reals to (big) integers
^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also convert between big reals and big integers (or standard integers):

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Big_Integer_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Real_Big_Integer_Conversion
    is
       I  : Integer;
       BI : Big_Integer;
       BR : Big_Real;

    begin
       I  := 12345;
       BR := To_Real (I);
       Put_Line ("BR (from I):  " & BR'Image);

       BI := 123456;
       BR := To_Big_Real (BI);
       Put_Line ("BR (from BI): " & BR'Image);

    end Show_Big_Real_Big_Integer_Conversion;

Here, we use the :ada:`To_Real` and the :ada:`To_Big_Real` and functions for
the conversions.


String conversions
^^^^^^^^^^^^^^^^^^

In addition to that, we can use string conversions:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Number_String_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Number_String_Conversion
    is
       BI : Big_Integer;
       BR : Big_Real;
    begin
       BI := From_String ("12345678901234567890");
       BR := From_String ("12345678901234567890.0");

       Put_Line ("BI: "
                 & To_String (Arg   => BI,
                              Width => 5,
                              Base => 2));
       Put_Line ("BR: "
                 & To_String (Arg   => BR,
                              Fore  => 2,
                              Aft   => 6,
                              Exp   => 18));
    end Show_Big_Number_String_Conversion;

In this example, we use the :ada:`From_String` to convert a string to a big
number. Note that the :ada:`From_String` function is actually called when
converting a literal |mdash| because of the corresponding aspect for
user-defined literals in the definitions of the :ada:`Big_Integer` and the
:ada:`Big_Real` types.

.. admonition:: For further reading...

    Big numbers are implemented using
    :ref:`user-defined literals <Adv_Ada_User_Defined_Literals>`, which we
    discussed previously. In fact, these are the corresponding type
    declarations:

    .. code-block:: ada

        --  Declaration from
        --  Ada.Numerics.Big_Numbers.Big_Integers;

        type Big_Integer is private
          with Integer_Literal => From_Universal_Image,
               Put_Image       => Put_Image;

        function From_Universal_Image
          (Arg : String)
          return Valid_Big_Integer
            renames From_String;

        --  Declaration from
        --  Ada.Numerics.Big_Numbers.Big_Reals;

        type Big_Real is private
          with Real_Literal => From_Universal_Image,
               Put_Image    => Put_Image;

        function From_Universal_Image
          (Arg : String)
           return Valid_Big_Real
             renames From_String;

    As we can see in these declarations, the :ada:`From_String` function
    renames the :ada:`From_Universal_Image` function, which is being used for
    the user-defined literals.

Also, we call the :ada:`To_String` function to get a string for the big
numbers. Naturally, using the :ada:`To_String` function instead of the
:ada:`Image` attribute |mdash| as we did in previous examples |mdash| allows
us to customize the format of the string that we display in the user message.


Other features of big integers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, let's look at two additional features of big integers:

- the natural and positive subtypes, and

- other available operators and functions.

Big positive and natural subtypes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to integer types, big integers have the :ada:`Big_Natural` and
:ada:`Big_Positive` subtypes to indicate natural and positive numbers. However,
in contrast to the :ada:`Natural` and :ada:`Positive` subtypes, the
:ada:`Big_Natural` and :ada:`Big_Positive` subtypes are defined via predicates
rather than the simple ranges of normal (ordinary) numeric types:

.. code-block:: ada

    subtype Natural  is
      Integer range 0 .. Integer'Last;

    subtype Positive is
      Integer range 1 .. Integer'Last;

    subtype Big_Natural is Big_Integer
      with Dynamic_Predicate =>
             (if Is_Valid (Big_Natural)
                then Big_Natural >= 0),
           Predicate_Failure =>
             (raise Constraint_Error);

    subtype Big_Positive is Big_Integer
      with Dynamic_Predicate =>
             (if Is_Valid (Big_Positive)
                then Big_Positive > 0),
           Predicate_Failure =>
             (raise Constraint_Error);

Therefore, we cannot simply use attributes such as :ada:`Big_Natural'First`.
However, we can use the subtypes to ensure that a big integer is in the
expected (natural or positive) range:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Positive_Natural switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Big_Positive_Natural is
       BI, D, N : Big_Integer;
    begin
       D  := 3;
       N  := 2;
       BI := Big_Natural (D / Big_Positive (N));

       Put_Line ("BI: " & BI'Image);
    end Show_Big_Positive_Natural;

By using the :ada:`Big_Natural` and :ada:`Big_Positive` subtypes in the
calculation above (in the assignment to :ada:`BI`), we ensure that we don't
perform a division by zero, and that the result of the calculation is a natural
number.


Other operators for big integers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the :ada:`mod` and :ada:`rem` operators with big integers:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Integer_Rem_Mod switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Big_Integer_Rem_Mod is
       BI : Big_Integer;
    begin
       BI := 145 mod (-4);
       Put_Line ("BI (mod): " & BI'Image);

       BI := 145 rem (-4);
       Put_Line ("BI (rem): " & BI'Image);
    end Show_Big_Integer_Rem_Mod;

In this example, we use the :ada:`mod` and :ada:`rem` operators in the
assignments to :ada:`BI`.

Moreover, there's a :ada:`Greatest_Common_Divisor` function for big
integers which, as the name suggests, calculates the greatest common divisor of
two big integer values:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Integer_Greatest_Common_Divisor switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Show_Big_Integer_Greatest_Common_Divisor
    is
       BI : Big_Integer;
    begin
       BI := Greatest_Common_Divisor (145, 25);
       Put_Line ("BI: " & BI'Image);

    end Show_Big_Integer_Greatest_Common_Divisor;

In this example, we retrieve the greatest common divisor of 145 and 25
(i.e.: 5).


.. _Adv_Ada_Big_Real_Quotients:

Big real and quotients
~~~~~~~~~~~~~~~~~~~~~~

An interesting feature of big reals is that they support quotients. In fact,
we can simply assign `2/3` to a big real variable. (Note that we're able to
omit the decimal points, as we write :ada:`2/3` instead of :ada:`2.0 / 3.0`.)
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Quotient_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Real_Quotient_Conversion
    is
       BR   : Big_Real;
    begin
       BR := 2 / 3;
       --  Same as:
       --  BR := From_Quotient_String ("2 / 3");

       Put_Line ("BR:   " & BR'Image);

       Put_Line ("Q:    "
                 & To_Quotient_String (BR));

       Put_Line ("Q numerator:    "
                 & Numerator (BR)'Image);
       Put_Line ("Q denominator:  "
                 & Denominator (BR)'Image);
    end Show_Big_Real_Quotient_Conversion;

In this example, we assign :ada:`2 / 3` to :ada:`BR` |mdash| we could have used
the :ada:`From_Quotient_String` function as well. Also, we use the
:ada:`To_Quotient_String` to get a string that represents the quotient.
Finally, we use the :ada:`Numerator` and :ada:`Denominator` functions to
retrieve the values, respectively, of the numerator and denominator of the
quotient (as big integers) of the big real variable.


Range checks
~~~~~~~~~~~~

Previously, we've talked about the :ada:`Big_Natural` and :ada:`Big_Positive`
subtypes. In addition to those subtypes, we have the :ada:`In_Range` function
for big numbers:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Numbers_In_Range switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use  Ada.Numerics.Big_Numbers.Big_Integers;

    with Ada.Numerics.Big_Numbers.Big_Reals;
    use  Ada.Numerics.Big_Numbers.Big_Reals;

    procedure Show_Big_Numbers_In_Range is

       BI : Big_Integer;
       BR : Big_Real;

       BI_From : constant Big_Integer := 0;
       BI_To   : constant Big_Integer := 1024;

       BR_From : constant Big_Real := 0.0;
       BR_To   : constant Big_Real := 1024.0;

    begin
       BI := 1023;
       BR := 1023.9;

       if In_Range (BI, BI_From, BI_To) then
          Put_Line ("BI ("
                    & BI'Image
                    & ") is in the "
                    & BI_From'Image
                    & " .. "
                    & BI_To'Image
                    & " range");
       end if;

       if In_Range (BR, BR_From, BR_To) then
          Put_Line ("BR ("
                    & BR'Image
                    & ") is in the "
                    & BR_From'Image
                    & " .. "
                    & BR_To'Image
                    & " range");
       end if;

    end Show_Big_Numbers_In_Range;

In this example, we call the :ada:`In_Range` function to check whether the big
integer number (:ada:`BI`) and the big real number (:ada:`BR`) are in the range
between 0 and 1024.
