Numerics
========

.. include:: ../../../../global.txt

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

In this example, :ada:`365` is an integer literal and :ada:`365.2564` is a
real literal.

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
:ada:`3.14159_26535_89793_23846_26433_83279_50288` is more
readable and less error prone to type than
:ada:`3.14159265358979323846264338327950288`. Here's the
complete code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Numeric_Literals.Pi_Literals
   :class: nosyntax-check

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       Pi   : constant :=
         3.14159_26535_89793_23846_26433_83279_50288;

       Pi2  : constant :=
         3.14159265358979323846264338327950288;

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
       --                       3.1415926535
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
       --                       Pi
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
    the best alternative is to use :ref:`big numbers <Adv_Ada_Big_Numbers>`
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
    corresponding operators for universal fixed types. For example:

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
    not with the universal fixed type. In the
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


.. _Adv_Ada_Discrete_And_Real_Numeric_Types:

Discrete and Real Numeric Types
-------------------------------

.. _Adv_Ada_Discrete_Numeric_Types:

Discrete Numeric Types
~~~~~~~~~~~~~~~~~~~~~~

In the Introduction to Ada course, we've seen that Ada has two kinds of
discrete numeric types: :ref:`signed integer <Intro_Ada_Integers>` and
:ref:`modular <Intro_Ada_Unsigned_Types>` types. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Discrete_And_Real_Numeric_Types.Signed_Modular_Types

    package Num_Types is

       type Signed_Integer is range 1 .. 1_000_000;
       type Modular is mod 2**32;

    end Num_Types;

Remember that modular types are similar to *unsigned* integer types in other
programming languages.

In this chapter, we review these types and look into a couple of details that
haven't been covered yet. We start the discussion with
:ref:`signed integer types <Adv_Ada_Integer_Types>`, and then move on to
:ref:`modular types <Adv_Ada_Modular_Types>`.

.. _Adv_Ada_Real_Numeric_Types:

Real Numeric Types
~~~~~~~~~~~~~~~~~~

In the Introduction to Ada course, we talked about
:ref:`floating-point <Intro_Ada_Floating_Point_Types>` and
:doc:`fixed-point </courses/intro-to-ada/chapters/fixed_point_types>` types.
In Ada, these two categories of numeric types belong to the so-called *real
types*. In very simple terms, we could say that real types are the ones whose
objects we could assign
:ref:`real numeric literals <Adv_Ada_Numeric_Literals>` to. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Discrete_And_Real_Numeric_Types.Universal_And_Real_Numeric_Types

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Discrete_And_Real_Numeric_Types.Universal_And_Real_Numeric_Types

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

.. todo::

    In this chapter, we discuss further details about
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


.. _Adv_Ada_Integer_Types:

Integer types
-------------

In the :ref:`Introduction to Ada <Intro_Ada_Integers>` course, we mentioned
that you can define your own integer types in Ada. In fact, typically you're
expected to do so, as Ada only guarantees the existence of a single integer
type |mdash| and the names of a few optional integer types.
Even though a specific compiler
might offer multiple predefined integer types, there's no guarantee that it
does that. Therefore, you should carefully evaluate the expected range of each
integer type in your implementation and specify that information in the
corresponding type definition.


.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.4 Integer Types <3-5-4>`

Predefined integer types
~~~~~~~~~~~~~~~~~~~~~~~~

Ada only has a single predefined integer type (:ada:`Integer`) and two subtypes
(:ada:`Natural` and :ada:`Positive`). Although the actual range of
:ada:`Integer` depends on the compiler and the target architecture, it must at
least support a 16-bit range |mdash| we can say that the following
specification is the minimum requirement for the :ada:`Integer` type:

.. code-block:: ada

    package Standard is

       --  [...]

       type Integer is
         range -2**15 + 1 .. +2**15 - 1;

       subtype Natural  is Integer
         range 0 .. Integer'Last;

       subtype Positive is Integer
         range 1 .. Integer'Last;

       --  [...]

    end Standard;

Note that the range of :ada:`Integer` doesn't start at :math:`-2^{15}`, but
rather at :math:`-2^{15} + 1`, which might seem a bit unusual. Thus, if your
algorithm requires the existence of :math:`-2^{15}`, you have a good reason to
define a custom range instead of relying on the :ada:`Integer` type.

As we've just said, the Ada standard only guarantees that :ada:`Integer` is at
least a 16-bit type, but it doesn't define its actual range for a specific
compiler or target architecture. For example, :ada:`Integer` could be defined
as a 32-bit type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Check_Integer_Type_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Check_Integer_Type_Range is
    begin
       Put_Line ("Integer'Size  :"
                 & Integer'Size'Image
                 & " bits");
       Put_Line ("Integer'First :"
                 & Integer'First'Image);
       Put_Line ("Integer'Last  :"
                 & Integer'Last'Image);
    end Check_Integer_Type_Range;

When running the example above on a typical PC, we might indeed confirm that
:ada:`Integer` is a 32-bit type |mdash| ranging from -2147483648 up to
2147483647. Of course, this doesn't go against the Ada standard, as it doesn't
specify the maximum range of the :ada:`Integer` type, only the minimum range.

The Ada standard also recommends that the :ada:`Long_Integer` type should be
available if the target architecture supports at least 32-bit operations.
However, the standard only guarantees that, if the :ada:`Long_Integer` is
available, it must support at least a 32-bit range |mdash| again, starting at
:math:`-2^{31} + 1` instead of :math:`-2^{31}`:

.. code-block:: ada

    package Standard is

       --  [...]

       type Long_Integer is
         range -2**31 + 1 .. +2**31 - 1;

       --  [...]

    end Standard;

Since this is a minimum requirement, it is possible that different types have
the same range |mdash| e.g. :ada:`Integer` and :ada:`Long_Integer` could have
the same range on a specific target architecture.

In addition, the Ada standard suggests that compilers may offer integer types
with names such as :ada:`Long_Long_Integer` and :ada:`Long_Long_Long_Integer`
|mdash| or :ada:`Short_Integer` and :ada:`Short_Short_Integer`. However, all
these types are considered non-portable, as there's no requirement concerning
their availability or expected range.

.. admonition:: In other languages

    In C, you have a longer list of standard integer types:

    .. code:: c run_button manual_chop project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Integer_Types_C

        !main.c
        #include <stdio.h>

        int main(int argc, const char * argv[])
        {
            printf("signed char:   %zu bytes\n",
                   sizeof(signed char) * 8);
            printf("short int:     %zu bytes\n",
                   sizeof(short int) * 8);
            printf("int:           %zu bytes\n",
                   sizeof(int) * 8);
            printf("long int:      %zu bytes\n",
                   sizeof(long int) * 8);
            printf("long long int: %zu bytes\n",
                   sizeof(long long int) * 8);
            return 0;
        }

    (Note that some of the types above aren't available in all versions of the
    C standard.)

    For the types above, there are no equivalent types in the Ada standard.
    (However, a compiler may implement this equivalence for practical reasons.)
    Therefore, if you're porting code from C to Ada, for example, you should
    check the expected range of your algorithm and specify the corresponding
    types in the Ada implementation.


.. admonition:: In the GNAT toolchain

    The GNAT compiler provides a couple of integer types in addition to the
    standard :ada:`Integer` type:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.GNAT_Integer_Types

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_GNAT_Integer_Types is
        begin

           Put_Line ("Short_Short_Integer'Size:    "
                     & Short_Short_Integer'Size'Image
                     & " bits");

           Put_Line ("Short_Integer'Size:          "
                     & Short_Integer'Size'Image
                     & " bits");

           Put_Line ("Integer'Size:                "
                     & Integer'Size'Image
                     & " bits");

           Put_Line ("Long_Integer'Size:           "
                     & Long_Integer'Size'Image
                     & " bits");

           Put_Line ("Long_Long_Integer'Size:      "
                     & Long_Long_Integer'Size'Image
                     & " bits");

           Put_Line ("Long_Long_Long_Integer'Size: "
                     & Long_Long_Long_Integer'Size'Image
                     & " bits");

        end Show_GNAT_Integer_Types;

    The actual range of each of these integer types depends on the target
    architecture. (Note that you may have different types with the same range.)

    Also, when interfacing with C code, GNAT guarantees the following type
    equivalence:

    +------------------------------+------------------------------------------+
    | C type                       | Ada type                                 |
    +==============================+==========================================+
    | :c:`signed char`             | :ada:`Short_Short_Integer`               |
    +------------------------------+------------------------------------------+
    | :c:`short int`               | :ada:`Short_Integer`                     |
    +------------------------------+------------------------------------------+
    | :c:`int`                     | :ada:`Integer`                           |
    +------------------------------+------------------------------------------+
    | :c:`long`                    | :ada:`Long_Integer`                      |
    +------------------------------+------------------------------------------+
    | :c:`long long`               | :ada:`Long_Long_Integer`                 |
    +------------------------------+------------------------------------------+


.. _Adv_Ada_Custom_Integer_Types:

Custom integer types
~~~~~~~~~~~~~~~~~~~~

As we've mentioned before, for the language-defined numeric data types such as
:ada:`Integer` or :ada:`Long_Integer`, the range selected by the compiler may
not correspond to the required range of the numeric algorithm we're
implementing. Therefore, it is best to simply declare custom types with the
necessary ranges specified. To do that, you should evaluate the algorithm and
reach a clear understanding about the adequate range of each integer type
|mdash| this should be based on the requirements of the algorithm.

For example, if some coefficients in your algorithm expected at least 32-bit
precision, you may consider defining this type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Custom_Integer_Type

    package Custom_Integer_Types is

       type Coefficient is
         range -2**31 .. +2**31 - 1;

       --  [...]

    end Custom_Integer_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Integer_Types;
    use Custom_Integer_Types;

    procedure Show_Custom_Integer_Types is
    begin
       Put_Line ("Coefficient'Size  :"
                 & Coefficient'Size'Image
                 & " bits");
       Put_Line ("Coefficient'First :"
                 & Coefficient'First'Image);
       Put_Line ("Coefficient'Last  :"
                 & Coefficient'Last'Image);
    end Show_Custom_Integer_Types;

In this example, we declare the 32-bit :ada:`Coefficient` type. We ensure that
it's a 32-bit type by explicitly writing :ada:`range -2**31 .. +2**31 - 1`.

Note that a custom type definition is always derived from the
:ref:`root integer type <Adv_Ada_Root_Types>`, which we discussed in another
chapter.


Illegal integer definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the specified range cannot be supported by the target machine, the Ada
compiler will reject the source code containing the type declaration (and all
clients of that code). For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Illegal_Custom_Integer_Type
    :class: ada-expect-compile-error

    package Custom_Integer_Types is

       type Int_1024_Bits is
         range -2**1023 .. +2**1023 - 1;

       --  [...]

    end Custom_Integer_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Integer_Types;
    use Custom_Integer_Types;

    procedure Show_Custom_Integer_Types is
    begin
       Put_Line ("Int_1024_Bits'Size  :"
                 & Int_1024_Bits'Size'Image
                 & " bits");
       Put_Line ("Int_1024_Bits'First :"
                 & Int_1024_Bits'First'Image);
       Put_Line ("Int_1024_Bits'Last  :"
                 & Int_1024_Bits'Last'Image);
    end Show_Custom_Integer_Types;

In this example, we're trying to define a 1024-bit integer type. Unless you're
compiling this code example many decades in the future, the compiler will (most
likely) reject this definition because current hardware architectures don't
support this range in any way. In order to handle integer values in such
ranges, you might consider using :ref:`big numbers <Adv_Ada_Big_Numbers>`.

You can query the maximum supported range by using the :ada:`System.Min_Int`
and :ada:`System.Max_Int` values. We discuss this topic
:ref:`next <Adv_Ada_System_Min_Max_Int>`.

.. admonition:: In the GNAT toolchain

    As of 2025, GNAT supports 128-bit integers:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Max_Custom_Integer_Type

        package Custom_Integer_Types is

           type Int_128_Bits is
             range -2**127 .. +2**127 - 1;

           --  [...]

        end Custom_Integer_Types;

        with Ada.Text_IO; use Ada.Text_IO;

        with Custom_Integer_Types;
        use Custom_Integer_Types;

        procedure Show_Custom_Integer_Types is
        begin
           Put_Line ("Int_128_Bits'Size  :"
                     & Int_128_Bits'Size'Image
                     & " bits");
           Put_Line ("Int_128_Bits'First :"
                     & Int_128_Bits'First'Image);
           Put_Line ("Int_128_Bits'Last  :"
                     & Int_128_Bits'Last'Image);
        end Show_Custom_Integer_Types;


.. admonition:: For further reading...

    This is a different approach to portability than that of, say, C, where for
    example type :c:`int` is always defined and hence the client code always
    compiles, but won't necessarily work at run-time. In that case, at best you
    find the problem during testing, which is comparatively expensive. Worse,
    if you don't find out until after deployment, the cost to fix it is much,
    much higher.

    In contrast, with a user-specified integer type, if the specified range
    cannot be supported by the (perhaps new) target machine, you find out at
    compile-time, which is far less expensive and more robust too.


.. _Adv_Ada_System_Min_Max_Int:

System max. and min. values
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've just mentioned, a custom type definition is derived from the
:ref:`root integer type <Adv_Ada_Root_Types>`. The base range of the root
integer type is :ada:`System.Min_Int .. System.Max_Int`.

The value of :ada:`System.Min_Int` and :ada:`System.Max_Int` depends on the
target system. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.System_Int_Range

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    procedure Show_System_Int_Range is
    begin
       Put_Line ("System.Min_Int :"
                 & System.Min_Int'Image);
       Put_Line ("System.Max_Int :"
                 & System.Max_Int'Image);
    end Show_System_Int_Range;

On a typical desktop PC, you might get the following values:

- :ada:`System.Min_Int`: -170141183460469231731687303715884105728
- :ada:`System.Max_Int`:  170141183460469231731687303715884105727

Because :ref:`custom integer types <Adv_Ada_Custom_Integer_Types>` are
implicitly derived from the root integer type, we cannot declare a custom
integer type outside of the :ada:`System.Min_Int .. System.Max_Int` range:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Custom_Int_Out_Of_Range
    :class: ada-expect-compile-error

    with System;

    package Custom_Int_Out_Of_Range is

       type Custom_Int is
         range System.Min_Int - 1 ..
               System.Max_Int + 1;

    end Custom_Int_Out_Of_Range;

The compilation of this package fails because the :ada:`Custom_Int'First` is
below :ada:`System.Min_Int` and :ada:`Custom_Int'Last` is above
:ada:`System.Max_Int`.


Range of base type
~~~~~~~~~~~~~~~~~~

As we've said before, a custom type definition is derived from the root
integer type. The range of its :ref:`base type <Adv_Ada_Base_Types>`, however,
is *not* derived from the root integer type, but rather determined by the range
of the type specification. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Integer_Types.Custom_Int_Base_Range

    with System;

    package Custom_Integer_Types is

       type Custom_Int is
         range 1 .. 10;

    end Custom_Integer_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Integer_Types;
    use  Custom_Integer_Types;

    procedure Show_Custom_Integer_Types is
    begin
       Put_Line ("Custom_Int'Size  :"
                 & Custom_Int'Size'Image
                 & " bits");
       Put_Line ("Custom_Int'First :"
                 & Custom_Int'First'Image);
       Put_Line ("Custom_Int'Last  :"
                 & Custom_Int'Last'Image);

       Put_Line ("Custom_Int'Base'Size  :"
                 & Custom_Int'Base'Size'Image);
       Put_Line ("Custom_Int'Base'First :"
                 & Custom_Int'Base'First'Image);
       Put_Line ("Custom_Int'Base'Last  :"
                 & Custom_Int'Base'Last'Image);
    end Show_Custom_Integer_Types;

On a typical desktop PC, you might see that the range of :ada:`Custom_Int'Base`
is :ada:`-128 .. 127`, while the
:ref:`system max. and min. values <Adv_Ada_System_Min_Max_Int>` we've seen
before had a much wider range.

As a reminder, the range of the base type might be wider than the range of the
custom integer type we're defining. (We mentioned this earlier on when
discussing :ref:`base types <Adv_Ada_Base_Types>`.)


.. _Adv_Ada_Modular_Types:

Modular Types
-------------

As we've mentioned in the :ref:`Introduction to Ada <Intro_Ada_Unsigned_Types>`
course, modular types are the Ada version of *unsigned* integer types. We
declare a modular type by specifying its modulo |mdash| by using the
:ada:`mod` keyword:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_Types

    package Modular_Types is

       type Modular is mod 2**32;

    end Modular_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Modular_Types;
    use  Modular_Types;

    procedure Show_Modular_Types is
    begin
       Put_Line ("Modular'Size  :"
                 & Modular'Size'Image
                 & " bits");
       Put_Line ("Modular'First :"
                 & Modular'First'Image);
       Put_Line ("Modular'Last  :"
                 & Modular'Last'Image);
    end Show_Modular_Types;

This example declares the 32-bit modular type :ada:`Modular`.

Note that, different from other languages such as C, the modulus need not be a
power of two. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_Types_Not_Power_Of_Two

    package Modular_Types is

       type Modular_10 is mod 10;

    end Modular_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Modular_Types;
    use  Modular_Types;

    procedure Show_Not_Power_Of_Two_Modular is
    begin
       Put_Line ("Modular_10'Size  :"
                 & Modular_10'Size'Image
                 & " bits");
       Put_Line ("Modular_10'First :"
                 & Modular_10'First'Image);
       Put_Line ("Modular_10'Last  :"
                 & Modular_10'Last'Image);
    end Show_Not_Power_Of_Two_Modular;

In this example, the modulus of type :ada:`Modular_10` is 10 (which obviously
is not a power-of-two number).

There are many attributes on modular types. We talk about them
:ref:`in another chapter <Adv_Ada_Modular_Type_Attributes>`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.4 Integer Types <3-5-4>`


.. _Adv_Ada_System_Max_Modulus:

System max. values for modulus
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we use a power-of-two number as the modulus, the maximum value that we
could use in the type declaration is indicated by the
:ada:`System.Max_Binary_Modulus` constant. In contrast, for non-power-of-two
numbers, the maximum value for the modulus is indicated by the
:ada:`System.Max_Nonbinary_Modulus` constant:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Max_Binary_Nonbinary_Modulus_Values

    with System;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Max_Binary_Nonbinary_Modulus is
       type Modular_Max is
         mod System.Max_Binary_Modulus;
    begin
       Put_Line
         ("System.Max_Binary_Modulus - 1 :"
          & Modular_Max'Last'Image);
       Put_Line
         ("System.Max_Nonbinary_Modulus  :"
          & System.Max_Nonbinary_Modulus'Image);
    end Show_Max_Binary_Nonbinary_Modulus;

On a typical desktop PC, you might get the following values:

- :ada:`System.Max_Binary_Modulus`: 2\ :sup:`128` =
  340,282,366,920,938,463,463,374,607,431,768,211,456

- :ada:`System.Max_Nonbinary_Modulus`:  2\ :sup:`32` - 1 = 4,294,967,295

As expected, we can simply use these constants in modular type declarations:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Max_Binary_Nonbinary_Modulus_Types

    with System;

    package Show_Max_Binary_Nonbinary_Modulus is

       type Modular_Max is
         mod System.Max_Binary_Modulus;

       type Modular_Max_Non_Power_Two is
         mod System.Max_Nonbinary_Modulus;

    end Show_Max_Binary_Nonbinary_Modulus;

In this example, we use :ada:`Max_Binary_Modulus` as the modulus of the
:ada:`Modular_Max` type, and :ada:`Max_Nonbinary_Modulus` as the modulus of the
:ada:`Modular_Max_Non_Power_Two` type.


.. _Adv_Ada_Floating_Point_Types:

Floating-point types
--------------------

In the :ref:`Introduction to Ada <Intro_Ada_Floating_Point_Types>` course, we
already covered a couple of details about floating-point types. In this
section, we will revise and expand on those topics.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.7 Floating Point Types <3-5-7>`


.. _Adv_Ada_Floating_Point_Types_Decimal_Precision:

Decimal precision
~~~~~~~~~~~~~~~~~

The main defining characteristic of a floating-point types is its decimal
precision |mdash| and not its range, as for integer types. (You may, however,
define
:ref:`ranges for floating-point types <Adv_Ada_Floating_Point_Type_Ranges>`,
as we'll discuss later on.) This means in simple terms that, when the value of
a floating-point object of type :ada:`T` is represented as a string, its
accuracy is guaranteed for the number of significant decimal digits defined for
type :ada:`T`.

For example, consider a number such as 0.123456123, which has 9 significant
digits. If we want to store this number in an object with a decimal precision
of 6 digits, the number will be *simplified* (actually, truncated) to 0.123456
|mdash| which has 6 significant digits:

::

    0.123456123     9 significant digits
    0.123456        6 significant digits

.. _Adv_Ada_Floating_Point_Types_Float_6_Digits_Example:

Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Decimal_Precision

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Digits is

       type Float_6_Digits is
         digits 6;

       type Float_9_Digits is
         digits 9;

       F6 : Float_6_Digits;
    begin
       F6 := 0.123456123;

       Put_Line ("F6 = "
                 & F6'Image);

       Put_Line ("F6 = "
                 & Float_9_Digits (F6)'Image
                 & " (9 digits)");

       Put_Line ("Float_6_Digits'Size  :"
                 & Float_6_Digits'Size'Image
                 & " bits");
    end Show_Decimal_Digits;

In this example, we define the custom floating-point type
:ada:`Float_6_Digits`, which has a decimal precision of 6 digits. This ensures
that, if we assign a number such as 0.123456123 to a variable :ada:`F` of this
type, the 6 first significant digits of this number (123456) will be correctly
represented. Because these are the only number of digits that the language
guarantees, no further digits are used when converting the number to a string
|mdash| therefore, we see ``F6 =  1.23456E+00`` in the user message.

However, the digits that we specify in the decimal precision of the type
definition are the required *minimum* number of significant decimal digits.
This means that the compiler is allowed to make use of a higher precision when
storing floating-point values into registers and memory. In fact, the
compiler might select a data type that allows for a much higher precision than
the one that would be theoretically needed for the decimal precision we
requested.

In the code snippet above, we use the :ada:`Float_9_Digits (F6)` conversion to
display the value stored in :ada:`F6` with a decimal precision of 9 digits
|mdash| the requested precision for the :ada:`Float_9_Digits` type. When we
display this converted value, we might see (at least, on a desktop PC) that the
actual value stored in :ada:`F6` isn't 1.23456, but rather a value closer to
the one we used in the :ada:`F6 := 0.123456123` assignment. This indicates that
the underlying hardware precision for the :ada:`Float_6_Digits` type is higher
than the 6 decimal digits we requested.


Predefined floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we know, Ada offers the predefined floating-point type :ada:`Float`. If the
compiler supports floating-point types with 6 or more digits of decimal
precision, then the decimal precision of :ada:`Float` must be at least 6
digits:

.. code-block:: ada

    package Standard is

       --  [...]

       type Float is digits 6;

       --  [...]

    end Standard;

The Ada standard also recommends that, if the :ada:`Long_Float` type is made
available, its decimal precision must be at least 11 digits:

.. code-block:: ada

    package Standard is

       --  [...]

       type Long_Float is digits 11;

       --  [...]

    end Standard;

In addition, similar to integer types, the Ada standard suggests that
compilers may offer floating-point types with names such as
:ada:`Long_Long_Float` |mdash| or :ada:`Short_Float` and
:ada:`Short_Short_Float`. However, all these types are considered non-portable,
as there's no requirement concerning their availability or expected decimal
precision.

.. admonition:: In other languages

    In C, we have a longer list of standard floating-point types:

    .. code:: c run_button manual_chop project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Types_C

        !main.c
        #include <stdio.h>

        int main(int argc, const char * argv[])
        {
            printf("float:        %zu bytes\n",
                   sizeof(float) * 8);
            printf("double:       %zu bytes\n",
                   sizeof(double) * 8);
            printf("long double:  %zu bytes\n",
                   sizeof(long double) * 8);
            return 0;
        }

    (Note that some of the types above aren't available in all versions of the
    C standard.)

    For the types above, there are no equivalent types in the Ada standard.
    (However, a compiler may implement this equivalence for practical reasons.)
    Therefore, if you're porting code from C to Ada, for example, you should
    rather check the expected range of your algorithm and specify custom
    floating-point types in the Ada implementation.

.. admonition:: In the GNAT toolchain

    The GNAT compiler provides a couple of floating-point types in addition to
    the standard :ada:`Float` type:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.GNAT_Floating_Point_Types

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_GNAT_Float_Types is
        begin

           Put_Line ("Short_Float'Size:          "
                     & Short_Float'Size'Image
                     & " bits");

           Put_Line ("Float'Size:                "
                     & Float'Size'Image
                     & " bits");

           Put_Line ("Long_Float'Size:           "
                     & Long_Float'Size'Image
                     & " bits");

           Put_Line ("Long_Long_Float'Size:      "
                     & Long_Long_Float'Size'Image
                     & " bits");

        end Show_GNAT_Float_Types;

    The actual precision of each of these floating-point types depends on the
    target architecture. (Note that you may have different types with the same
    precision.)

    Also, when interfacing with C code, GNAT guarantees the following type
    equivalence:

    +------------------------------+------------------------------------------+
    | C type                       | Ada type                                 |
    +==============================+==========================================+
    | :c:`float`                   | :ada:`Float`                             |
    +------------------------------+------------------------------------------+
    | :c:`double`                  | :ada:`Long_Float`                        |
    +------------------------------+------------------------------------------+
    | :c:`long double`             | :ada:`Long_Long_Float`                   |
    +------------------------------+------------------------------------------+


.. _Adv_Ada_Custom_Floating_Point_Types:

Custom floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similarly to what we discussed for
custom integer types, language-defined
numeric data types such as :ada:`Float` or :ada:`Long_Float` may not be
sufficient for the requirements of the numeric algorithm we're implementing. So
again, it's best to simply declare custom types with sufficient precision.
For that, we have to evaluate the algorithm and assess the minimum required
precision of each floating-point type |mdash| this should be based on the
requirements of the algorithm.

.. todo::

    Add link to section on custom integer types <Adv_Ada_Custom_Integer_Types>
    once it has become available!

For example, if some coefficients from your algorithm expect a decimal
precision of at least 12 digits, you may consider defining this type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Custom_Floating_Point_Types

    package Custom_Floating_Point_Types is

       type Coefficient is
         digits 12;

       --  [...]

    end Custom_Floating_Point_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Floating_Point_Types;
    use Custom_Floating_Point_Types;

    procedure Show_Custom_Floating_Point_Types is
    begin
       Put_Line ("Coefficient'Digits :"
                 & Coefficient'Digits'Image
                 & " digits");
       Put_Line ("Coefficient'Size   :"
                 & Coefficient'Size'Image
                 & " bits");
    end Show_Custom_Floating_Point_Types;

In this example, we declare the :ada:`Coefficient` type with a decimal
precision of at least 12 digits. We ensure that this precision is maintained
for the type by explicitly writing :ada:`digits 12`.
(Here, we're using the :ref:`Digits attribute <Adv_Ada_Digits_Attribute>`,
which we discuss in another chapter.)

Note that a custom type definition is always derived from the
:ref:`root real type <Adv_Ada_Root_Types>`, which we discussed in another
chapter.


.. _Adv_Ada_Floating_Point_Derived_Types_Subtypes:

Derived floating-point types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we have a brief discussion about types derived from
floating-point types, as well as subtypes of floating-point types.

Derived floating-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As expected, we can derive from any floating-point type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Derived_Floating_Point_Types

    package Custom_Floating_Point_Types is

       type Coefficient is
         digits 12;

       type Filter_Coefficient is new
         Coefficient;

    end Custom_Floating_Point_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Floating_Point_Types;
    use  Custom_Floating_Point_Types;

    procedure Show_Derived_Floating_Point_Types is
       C  : Coefficient;
       FC : Filter_Coefficient;
    begin
       C  := 0.532344123;
       Put_Line ("C  = "
                 & C'Image);

       FC := Filter_Coefficient (C);
       Put_Line ("FC = "
                 & FC'Image);
    end Show_Derived_Floating_Point_Types;

In this example, we derive the :ada:`Filter_Coefficient` type from the :ada:`Coefficient` type.

.. admonition:: For further reading...

    We can also constrain the decimal precision of the derived type. However,
    this feature is considered obsolescent, so it should be avoided. (Note that
    this applies to subtypes as well.) For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Derived_Floating_Point_Types

        package Custom_Floating_Point_Types is

           type Coefficient is
             digits 12;

           type Filter_Coefficient is new
             Coefficient
               digits 6;

        end Custom_Floating_Point_Types;

        with Ada.Text_IO; use Ada.Text_IO;

        with Custom_Floating_Point_Types;
        use Custom_Floating_Point_Types;

        procedure Show_Derived_Floating_Point_Types is
           C  : Coefficient;
           FC : Filter_Coefficient;
        begin
           C  := 0.532344123;
           Put_Line ("C  = "
                     & C'Image);

           FC := Filter_Coefficient (C);
           Put_Line ("FC = "
                     & FC'Image);
        end Show_Derived_Floating_Point_Types;

    In this example, we derive the :ada:`Filter_Coefficient` type from the
    :ada:`Coefficient` type and decrease the decimal precision from 12 to 6
    digits.

    .. admonition:: In the Ada Reference Manual

        - :arm22:`J.3 Floating Point Types <J-3>`


Floating-point subtypes
^^^^^^^^^^^^^^^^^^^^^^^

We can also declare subtypes of floating-point types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Subtypes

    package Custom_Floating_Point_Types is

       type Coefficient is
         digits 12;

       subtype Filter_Coefficient is
         Coefficient;

    end Custom_Floating_Point_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Floating_Point_Types;
    use Custom_Floating_Point_Types;

    procedure Show_Floating_Point_Subtypes is
       C  : Coefficient;
       FC : Filter_Coefficient;
    begin
       C  := 0.532344123;
       Put_Line ("C  = "
                 & C'Image);

       FC := C;
       Put_Line ("FC = "
                 & FC'Image);
    end Show_Floating_Point_Subtypes;

In this example, we declare :ada:`Filter_Coefficient` as a subtype of the
:ada:`Coefficient` type.


.. _Adv_Ada_Floating_Point_Base_Type_Decimal_Precision:

Decimal precision of base type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We discussed :ref:`base types <Adv_Ada_Base_Types>` earlier on. For
floating-point types, the decimal precision of the base type of a :ada:`T` type
might be higher than the decimal precision we've specified for type :ada:`T`.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Base_Type_Precision

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Base_Type_Precision is

       type Float_3_Digits is
         digits 3;

    begin
       Put_Line
         ("Float_3_Digits'Digits       :"
          & Float_3_Digits'Digits'Image
          & " digits");

       Put_Line
         ("Float_3_Digits'Base'Digits  :"
          & Float_3_Digits'Base'Digits'Image
          & " digits");
    end Show_Base_Type_Precision;

On a typical desktop PC, you may see that the base type of
:ada:`Float_3_Digits` has 6 digits, while the :ada:`Float_3_Digits` type itself
has only 3 digits |mdash| as requested in its type declaration.

.. _Adv_Ada_Floating_Point_Size:

Size of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice that the size of the :ada:`Float_6_Digits` type from the
:ref:`first code example <Adv_Ada_Floating_Point_Types_Float_6_Digits_Example>`
was 32 bits. Reducing the number of digits might not have a direct impact on
the type's size. In fact, on a typical desktop PC, if we reduce the decimal
precision of a type to, say, 3 or 2 digits, the compiler will most probably
still select a 32-bit floating-point type for the target platform. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Decimal_Precision

    package Custom_Floating_Point_Types is

       type Float_1_Digits is
         digits 1;
       type Float_2_Digits is
         digits 2;
       type Float_3_Digits is
         digits 3;
       type Float_4_Digits is
         digits 4;
       type Float_5_Digits is
         digits 5;
       type Float_6_Digits is
         digits 6;
       type Float_7_Digits is
         digits 7;
       type Float_8_Digits is
         digits 8;
       type Float_9_Digits is
         digits 9;
       type Float_10_Digits is
         digits 10;
       type Float_11_Digits is
         digits 11;
       type Float_12_Digits is
         digits 12;
       type Float_13_Digits is
         digits 13;
       type Float_14_Digits is
         digits 14;
       type Float_15_Digits is
         digits 15;
       type Float_16_Digits is
         digits 16;
       type Float_17_Digits is
         digits 17;
       type Float_18_Digits is
         digits 18;

    end Custom_Floating_Point_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Floating_Point_Types;
    use  Custom_Floating_Point_Types;

    procedure Show_Decimal_Digits is
    begin
       Put_Line ("Float_1_Digits'Size   :"
                 & Float_1_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_2_Digits'Size   :"
                 & Float_2_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_3_Digits'Size   :"
                 & Float_3_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_4_Digits'Size   :"
                 & Float_4_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_5_Digits'Size   :"
                 & Float_5_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_6_Digits'Size   :"
                 & Float_6_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_7_Digits'Size   :"
                 & Float_7_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_8_Digits'Size   :"
                 & Float_8_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_9_Digits'Size   :"
                 & Float_9_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_10_Digits'Size  :"
                 & Float_10_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_11_Digits'Size  :"
                 & Float_11_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_12_Digits'Size  :"
                 & Float_12_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_13_Digits'Size  :"
                 & Float_13_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_14_Digits'Size  :"
                 & Float_14_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_15_Digits'Size  :"
                 & Float_15_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_16_Digits'Size  :"
                 & Float_16_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_17_Digits'Size  :"
                 & Float_17_Digits'Size'Image
                 & " bits");
       Put_Line ("Float_18_Digits'Size  :"
                 & Float_18_Digits'Size'Image
                 & " bits");
    end Show_Decimal_Digits;

On a typical desktop PC, we may see the following results:

+-------------+-------------+-------------+
| Min. digits | Max. digits | Size (bits) |
+=============+=============+=============+
|           1 |           6 |          32 |
+-------------+-------------+-------------+
|           7 |          15 |          64 |
+-------------+-------------+-------------+
|          16 |          18 |         128 |
+-------------+-------------+-------------+

Ada doesn't actually give us any guarantees about specific sizes of
floating-point data types on the target hardware. However, as you might recall
from an earlier chapter, we can request specific sizes for custom types. We
discuss this topic next.

Note that, for the example above, the size of the type is equal to the size of
its base type, i.e. :ada:`Float_1_Digits'Size = Float_1_Digits'Base'Size`,
:ada:`Float_2_Digits'Size = Float_2_Digits'Base'Size`, and so on.


.. _Adv_Ada_Floating_Point_Type_Size:

Custom size of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As discussed earlier on, the Ada standard requires that the precision defined
after the :ada:`digits` keyword of a type is maintained for all objects of that
floating-point type. It doesn't require, however, that custom floating-point
types |mdash| or even predefined floating-point types |mdash| have a certain
size. Therefore, if we really have to use a specific size for a
floating-point data type, we can add the
:ref:`Size aspect <Adv_Ada_Size_Aspect>` to the type declaration. For example:


.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Decimal_Precision

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Digits is

       type Float_6_Digits is
         digits 6
           with Size => 128;

    begin
       Put_Line ("Float_6_Digits'Size  :"
                 & Float_6_Digits'Size'Image
                 & " bits");
    end Show_Decimal_Digits;

In this example, we specify that :ada:`Float_6_Digits` requires a size of 128
bits to be represented |mdash| instead of the 32 bits that we would typically
see on a desktop PC. (Also, remember that this code example won't compile if
your target architecture doesn't support 128-bit floating-point data types.)

.. todo::

    Discuss :ada:`Float_6_Digits'Size` vs. :ada:`Float_6_Digits'Base'Size`.


Range of custom floating-point types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to specifying the decimal precision of a floating-point type, we
can also specify its range:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Range_Def

    package Show_Range_Definition is

       type Float_6_Digits_Normalized is
         digits 6
           range -1.0 .. 1.0;

    end Show_Range_Definition;

You probably recall that, for integer types, we were able to declare a type by
specifying its range. For floating-point types, however, we cannot specify the
floating-point range without a decimal precision, as the compiler wouldn't be
able to infer the intended precision based on the range alone:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Range_Def
    :class: ada-expect-compile-error

    package Show_Range_Definition is

       type Float_Normalized is
         range -1.0 .. 1.0;
       --  ERROR: 'digits' specification
       --         is missing!

    end Show_Range_Definition;

Compilation of this code example fails because the decimal precision was not
specified.

Assigning to objects of different floating-point types works as expected. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Range is

       type Float_6_Digits_Normalized is
         digits 6
           range -1.0 .. 1.0;

       type Float_9_Digits_Normalized is
         digits 9
           range -1.0 .. 1.0;

       F6_N : Float_6_Digits_Normalized;
       F9_N : Float_9_Digits_Normalized;
    begin
       F6_N := 0.123456123;

       Put_Line ("F6_N = "
                 & F6_N'Image);

       F9_N := Float_9_Digits_Normalized (F6_N);
       --  Converting from
       --    Float_6_Digits_Normalized
       --  to
       --    Float_9_Digits_Normalized

       Put_Line ("F9_N = "
                 & F9_N'Image);

       Put_Line
         ("Float_6_Digits_Normalized'Size  :"
          & Float_6_Digits_Normalized'Size'Image
          & " bits");

       Put_Line
         ("Float_9_Digits_Normalized'Size  :"
          & Float_9_Digits_Normalized'Size'Image
          & " bits");
    end Show_Range;

In this example, we assign the :ada:`F6_N` object of
:ada:`Float_6_Digits_Normalized` type to the :ada:`F9_N` object of
:ada:`Float_9_Digits_Normalized` type. Of course, if a range is specified, the
value of an object cannot be outside of the type's range:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Range
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Range is

       type Float_6_Digits_Normalized is
         digits 6
           range -1.0 .. 1.0;

       F6_N : Float_6_Digits_Normalized;
    begin
       F6_N := 0.123456123;

       Put_Line ("F6_N = "
                 & F6_N'Image);

       F6_N := F6_N * 10.0 - 0.5;

       Put_Line ("F6_N = "
                 & F6_N'Image);

       F6_N := F6_N + 1.0;
       --  ERROR:  result of this operation
       --          is outside of the interval
       --          [-1.0, 1.0].

       Put_Line ("F6_N = "
                 & F6_N'Image);
    end Show_Range;

In this example, the assignment :ada:`F6_N := F6_N + 1.0` overflows because the
resulting value is outside of the range of the :ada:`Float_6_Digits_Normalized`
type. In contrast, the assignment :ada:`F6_N := F6_N * 10.0 - 0.5` doesn't
raise an exception because the resulting value is inside the range |mdash| even
though the intermediate value (1.23456) resulting from the :ada:`F6_N * 10.0`
operation is outside the type's range.


Range of derived floating-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can specify a range when deriving from floating-point types. In fact, it's
possible to specify a range when the parent type doesn't have any range
constraints, or specify a subrange when the parent type already has a range
constraint. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Derived_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Range is

       type Float_6_Digits is
         digits 6;

       type Float_6_Digits_Normalized is new
         Float_6_Digits
           range -1.0 .. 1.0;

       type Float_6_Digits_Normalized_Positive is new
         Float_6_Digits_Normalized
           range 0.0 .. 1.0;

       F6_N  : Float_6_Digits_Normalized;
       F6_NP : Float_6_Digits_Normalized_Positive;
    begin
       F6_N := 0.123456123;

       Put_Line ("F6_N  = "
                 & F6_N'Image);

       F6_NP :=
         Float_6_Digits_Normalized_Positive (F6_N);

       Put_Line ("F6_NP = "
                 & F6_NP'Image);

       Put_Line
         ("Float_6_Digits_Normalized'Size  :"
          & Float_6_Digits_Normalized'Size'Image
          & " bits");
    end Show_Range;

In this example, we derive the type :ada:`Float_6_Digits_Normalized` from
:ada:`Float_6_Digits` and specify the normalized range -1.0 .. 1.0. Similarly,
we derive :ada:`Float_6_Digits_Normalized_Positive` from
:ada:`Float_6_Digits_Normalized` and constrain its range to positive numbers
(0.0 .. 1.0).

As we know, extending the range when deriving from a type isn't possible for
any scalar type, be it discrete or real. Therefore, as expected, it's not
possible to increase the range in this case:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Derived_Range_Increase

    package Show_Extended_Range is

       type Float_6_Digits is
         digits 6;

       type Float_6_Digits_Normalized is new
         Float_6_Digits
           range -1.0 .. 1.0;

       type Float_6_Digits_Normalized_Ext is new
         Float_6_Digits_Normalized
           range -2.0 .. 2.0;

    end Show_Extended_Range;

Compilation fails for this example because we're trying to extend the range
from -1.0 .. 1.0 to -2.0 .. 2.0 when deriving from the
:ada:`Float_6_Digits_Normalized` type.


.. _Adv_Ada_Floating_Point_Type_Ranges:

Range of floating-point subtypes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also specify a range when declaring a subtype of a floating-point type.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Subtype_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Floating_Point_Subtype_Ranges is

       type Float_6_Digits is
         digits 6;

       subtype Float_6_Digits_Subtype is
         Float_6_Digits;
       --  Same range as Float_6_Digits

       subtype Float_6_Digits_Normalized is
         Float_6_Digits
           range -1.0 .. 1.0;

       F6_N : Float_6_Digits_Normalized;
    begin
       F6_N := 0.123456123;

       Put_Line ("F6_N = "
                 & F6_N'Image);
    end Show_Floating_Point_Subtype_Ranges;

In this example, we declare the :ada:`Float_6_Digits_Normalized` type as a
subtype of :ada:`Float_6_Digits` and specify the normalized range -1.0 .. 1.0.

In the case of the subtype :ada:`Float_6_Digits_Subtype`, however, we haven't
specified any range. Therefore, as expected, the range of the
:ada:`Float_6_Digits` type is used.


Range of base type
~~~~~~~~~~~~~~~~~~

Because the base type of a floating-point type is only constrained by the range
of the root floating-point type, its range doesn't necessarily match the range
of a floating-point type :ada:`T` |mdash| this is especially the case when
we're specifying a custom range. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floating_Point_Base_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Floating_Point_Base_Range is

       type Float_6D is
         digits 6;

       type Float_6D_Norm is
         digits 6
           range -1.0 .. 1.0;

    begin

       Put_Line
         ("Float_6D'First           = "
          & Float_6D'First'Image);
       Put_Line
         ("Float_6D'Last            = "
          & Float_6D'Last'Image);

       Put_Line
         ("--------------------------");

       Put_Line
         ("Float_6D_Norm'First      = "
          & Float_6D_Norm'First'Image);
       Put_Line
         ("Float_6D_Norm'Last       = "
          & Float_6D_Norm'Last'Image);

       Put_Line
         ("Float_6D_Norm'Base'First = "
          & Float_6D_Norm'Base'First'Image);
       Put_Line
         ("Float_6D_Norm'Base'Last  = "
          & Float_6D_Norm'Base'Last'Image);

    end Show_Floating_Point_Base_Range;

In this example, we see that the range of the range-constrained type
:ada:`Float_6D_Norm` is restricted to -1.0 .. 1.0. On a desktop PC, the range
of its base type |mdash| as well as the range of the :ada:`Float_6D` type
|mdash| is typically -3.40282E+38 .. 3.40282E+38.


.. _Adv_Ada_System_Max_Base_Digits_And_Max_Digits:

System max. base digits and max. digits values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two values associated with the maximum decimal precision of
floating-point types: :ada:`System.Max_Digits` and
:ada:`System.Max_Base_Digits`. They are dependent on the compiler capabilities,
as well as hardware limitations:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.System_Max_Digits

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    procedure Show_System_Max_Digits is
    begin
       Put_Line ("System.Max_Digits      :"
                 & System.Max_Digits'Image
                 & " digits");
       Put_Line ("System.Max_Base_Digits :"
                 & System.Max_Base_Digits'Image
                 & " digits");
    end Show_System_Max_Digits;

On a typical desktop PC, we might see that the maximum decimal precision is the
same in both cases:

- :ada:`System.Max_Digits`:       18 digits
- :ada:`System.Max_Base_Digits`:  18 digits

Note that this might not be the case for certain embedded devices.

For floating-point type declarations without a range constraint, the maximum
decimal precision must not be greater than :ada:`System.Max_Digits`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Max_Float

    with System;

    package Show_Max_Floating_Point is

       type Max_Float is
         digits System.Max_Digits;

    end Show_Max_Floating_Point;

Here, we're declaring the :ada:`Max_Float` using the maximum precision possible
on the target platform.

When a range constraint is included in floating-point type declarations, the
maximum decimal precision must not be greater than
:ada:`System.Max_Base_Digits`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Max_Float_Range

    with System;

    package Show_Max_Floating_Point is

       type Max_Float_Normalized is
         digits System.Max_Base_Digits
           range -1.0 .. 1.0;

    end Show_Max_Floating_Point;

Here, we're declaring the range-constrained :ada:`Max_Float_Normalized` using
the maximum precision possible on the target platform.


Illegal floating-point type declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a floating-point type declaration isn't supported by the Ada compiler or the
target platform, it is considered illegal and, therefore, compilation will fail
for that declaration. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Max_Float_Illegal
    :class: ada-expect-compile-error

    with System;

    package Show_Max_Floating_Point is

       type Max_Float is
         digits System.Max_Digits + 1;

    end Show_Max_Floating_Point;

In this example, we're trying to declare the :ada:`Max_Float` type with a
decimal precision greater than the maximum supported precision. Therefore,
compilation fails for this example.


.. _Adv_Ada_Fixed_Point_Types:

Fixed-point types
-----------------

We already discussed :ref:`fixed-point types <Intro_Ada_Fixed_Point_Types>` in
the Introduction to Ada course. Roughly speaking, fixed-point types can be
thought as a way to mimic operations that look like floating-point types, but
use discrete numeric types *in the background*. This has a big advantage for
the implementation of certain numeric algorithms, as developers can use
operations that look familiar because they resemble the ones they use with
floating-point types.

.. admonition:: In other languages

    In many programming languages such as C, there's no built-in support for
    fixed-point types. This forces developers that need fixed-point types to
    circumvent this absence with sometimes cumbersome alternative. They could,
    for example, use integer types and introduce additional operations to match
    fixed-point operations. Alternatively, frameworks or non-portable,
    compiler-specific extensions might be used in some cases. In contrast, the
    fact that Ada has built-in support for fixed-point types means that using
    these types is both portable and doesn't require extra efforts to
    circumvent limitations |mdash| such as the ones that originate from using
    integer types to emulate fixed-point operations.

As mentioned in the Introduction to Ada course course, fixed-point types
are classified as either
:ref:`decimal fixed-point types <Adv_Ada_Decimal_Fixed_Point_Types>` or
ordinary (binary) types.

.. todo::

    Add link to sections above once available.

Decimal fixed-point types are based on powers of ten and have the following
syntax:

.. code-block:: ada

    type <type-name> is
      delta <delta-value> digits <digits-value>;

Decimal fixed-point types are useful, for example, in many financial
applications, where round-off errors from arithmetic operations are considered
unacceptable.

Ordinary fixed-point types are based on powers of two (in their hardware
implementation) and have the following syntax:

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

Ordinary fixed-point types can be found in some implementations for digital
signal processing, for example.

In the next sections, we discuss further details about these specific types.
Next in this section, we introduce the concept of *small* and *delta* of
fixed-point types, which are common for both kinds of fixed-point types.


.. _Adv_Ada_Fixed_Point_Types_Small_Delta:

Small and delta
~~~~~~~~~~~~~~~

The *small* and the *delta* of a fixed-point type indicate the numeric
precision of that type. Let's discuss these concepts and how they differ
from each other.

The *delta* corresponds to the value used for the :ada:`delta` in the type
definition. For example, if we declare
:ada:`type T3_D3 is delta 10.0 ** (-3) digits D`, then the *delta* is equal to
the 10.0\ :sup:`-3` that we used in the type definition.

The *small* of a type :ada:`T` is the smallest positive value used in the
machine representation  of the type. In other words, while the *delta* is
primarily a user-selected value that (ideally) fits the requirements of the
implementation, the *small* indicates how that *delta* is represented on the
target machine.

The *small* must be at least equal to or smaller than the *delta*. In many
cases, however, the *small* of a type :ada:`T` is equal to the *delta* of that
type. In addition, for decimal fixed-point types specifically, the *small* is
**always** equal to its *delta*.

Note that *small* of a type isn't necessarily a small number |mdash| in fact,
it could be quite large. We'll see examples of that later on in this chapter.

We can use the :ada:`T'Small` and :ada:`T'Delta` attributes to retrieve the
actual values of the *small* and *delta* of a fixed-point type :ada:`T`. (We
discuss more details about these attributes
:ref:`in another chapter <Adv_Ada_Fixed_Point_Type_Small_Delta_Attributes>`.)
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Small_Delta

    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Fixed_Small_Delta is

       type Ordinary_Fixed_Point is
         delta 0.25
         range -2.0 .. 2.0;

    begin
       Put_Line ("Ordinary_Fixed_Point'Small: "
                 & Ordinary_Fixed_Point'Small'Image);
       Put_Line ("Ordinary_Fixed_Point'Delta: "
                 & Ordinary_Fixed_Point'Delta'Image);
       Put_Line ("Ordinary_Fixed_Point'Size: "
                 & Ordinary_Fixed_Point'Size'Image);
    end Show_Fixed_Small_Delta;

In this example, we see the values for the compiler-selected *small* and the
*delta* of type :ada:`Ordinary_Fixed_Point`. (Both are 0.25.)

When we declare a fixed-point data type, we must specify the *delta*. In
contrast, providing a *small* in the type declaration is optional for
ordinary fixed-point data types, but forbidden for decimal fixed-point types.

By default, the compiler automatically selects the *small*: this value is a
power of ten for decimal fixed-point types and a power of two for ordinary
fixed-point types. Also, for ordinary fixed-point types, we can specify the
*small* by using the :ada:`Small` aspect.

.. todo::

    Add link to subsection on :ada:`Small` aspect once available.

As we mentioned before, the selected value for the *small* always follows the
rule that it must be smaller or equal to the *delta*. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Small_Delta

    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Fixed_Small_Delta is

       type Ordinary_Fixed_Point is
         delta 0.2
         range -2.0 .. 2.0;

    begin
       Put_Line ("Ordinary_Fixed_Point'Small: "
                 & Ordinary_Fixed_Point'Small'Image);
       Put_Line ("Ordinary_Fixed_Point'Delta: "
                 & Ordinary_Fixed_Point'Delta'Image);
       Put_Line ("Ordinary_Fixed_Point'Size: "
                 & Ordinary_Fixed_Point'Size'Image);
    end Show_Fixed_Small_Delta;

In this example, the *delta* that we specifed for :ada:`Ordinary_Fixed_Point`
is 0.2, while the compiler-selected *small* is 2.0\ :sup:`-3`.

.. admonition:: For further reading...

    As we've mentioned, the small and the delta need not actually be small
    numbers.
    They can be arbitrarily large. For instance, they could be 1.0, or 1000.0.
    Consider the following example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Large_Small_Attribute

        package Fixed_Point_Defs is
           S     : constant := 32;
           Exp   : constant := 128;
           D     : constant := 2.0 ** (-S + Exp + 1);

           type Fixed is delta D
             range -1.0 * 2.0 ** Exp ..
                    1.0 * 2.0 ** Exp - D;

           pragma Assert (Fixed'Size = S);
        end Fixed_Point_Defs;

        with Fixed_Point_Defs; use Fixed_Point_Defs;
        with Ada.Text_IO;      use Ada.Text_IO;

        procedure Show_Fixed_Type_Info is
        begin
           Put_Line ("Size : "
                     & Fixed'Size'Image);
           Put_Line ("Small : "
                     & Fixed'Small'Image);
           Put_Line ("Delta : "
                     & Fixed'Delta'Image);
           Put_Line ("First : "
                     & Fixed'First'Image);
           Put_Line ("Last : "
                     & Fixed'Last'Image);
        end Show_Fixed_Type_Info;

    In this example, the *small* of the :ada:`Fixed` type is actually quite
    large: 1.58456325028528675\ :sup:`29`. (Also, the first and the last values
    are large: -340,282,366,920,938,463,463,374,607,431,768,211,456.0 and
    340,282,366,762,482,138,434,845,932,244,680,310,784.0, or approximately
    -3.4028\ :sup:`38` and 3.4028\ :sup:`38`.)

    In this case, if we assign 1 or 1,000 to a variable :ada:`F` of this type,
    the actual value stored in :ada:`F` is zero. Feel free to try this out!


.. _Adv_Ada_Fixed_Point_Derived_Types_Subtypes:

Derived fixed-point types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we present a brief discussion about types derived from
fixed-point types, as well as subtypes of fixed-point types.

Derived fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^

We can of course derive from any fixed-point types. Let's see an example for
decimal fixed-point types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Derived_Decimal_Fixed_Point_Types

    package Custom_Decimal_Types is

       type Decimal is
         delta 10.0 ** (-2) digits 6;

       type Small_Money is new
         Decimal;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Derived_Decimal_Types is
       D  : Decimal;
       SM : Small_Money;
    begin
       D  := 231.53;
       Put_Line ("D  = "
                 & D'Image);

       SM := Small_Money (D);
       Put_Line ("SM = "
                 & SM'Image);
    end Show_Derived_Decimal_Types;

In this example, we derive the :ada:`Small_Money` type from the :ada:`Decimal`
type. Also, :ada:`Small_Money (D)` performs a conversion between decimal
fixed-point types (from the :ada:`Decimal` type to the :ada:`Small_Money`
type).


Fixed-point subtypes
^^^^^^^^^^^^^^^^^^^^

We can also declare subtypes of fixed-point types. Let's see an example using
decimal fixed-point types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Fixed_Point_Subtypes

    package Custom_Decimal_Types is

       type Decimal is
         delta 10.0 ** (-2) digits 6;

       subtype Small_Money is Decimal;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use Custom_Decimal_Types;

    procedure Show_Decimal_Subtypes is
       C  : Decimal;
       FC : Small_Money;
    begin
       C  := 231.53;
       Put_Line ("C  = "
                 & C'Image);

       FC := C;
       Put_Line ("FC = "
                 & FC'Image);
    end Show_Decimal_Subtypes;

In this example, we declare :ada:`Small_Money` as a subtype of the
:ada:`Decimal` type.


.. _Adv_Ada_Fixed_Point_Type_Size:

Custom size of fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can explicitly require a certain size for a fixed-point type |mdash|
similar to what we can do with other types such as
:ref:`floating-point types <Adv_Ada_Floating_Point_Type_Size>`. In order to do
that, we add the :ref:`Size aspect <Adv_Ada_Size_Aspect>` to the type
declaration.

Let's see an example using a decimal fixed-point type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Custom_Size_Decimal_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Custom_Size_Decimal is

       type Decimal_128_Bits is
         delta 10.0 ** (-2) digits 6
           with Size => 128;

    begin
       Put_Line ("Decimal_128_Bits'Size      :"
                 & Decimal_128_Bits'Size'Image
                 & " bits");
    end Show_Custom_Size_Decimal;

In this example, we require that :ada:`Decimal_128_Bits` has a size of 128
bits on the target platform |mdash| instead of the 32 bits that we would
typically see for that type on a desktop PC. (As a reminder, this code example
won't compile if your target architecture doesn't support 128-bit data types.)


.. _Adv_Ada_Fixed_Point_Machine_Representation:

Machine representation of fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we discuss how fixed-point types are represented in actual
hardware. Typically, the machine representation of objects of fixed-point type
consists of integer values scaled by the *small* of the type. To retrieve
the actual integer representation, we can use
:ref:`overlays <Adv_Ada_Address_Aspect_Overlay>`.


.. _Adv_Ada_Decimal_Fixed_Point_Machine_Representation:

Machine representation of decimal types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with decimal fixed-ppint types. Consider the following types from
the :ada:`Custom_Decimal_Types` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Machine_Implementation_Decimal_Types

    package Custom_Decimal_Types is

       type T0_D4 is
         delta 10.0 ** (-0) digits 4;
       type T2_D6 is
         delta 10.0 ** (-2) digits 6;
       type T2_D12 is
         delta 10.0 ** (-2) digits 12;

       type Int_T0_D4 is
         range -2 ** (T0_D4'Size - 1) ..
                2 ** (T0_D4'Size - 1) - 1;
       type Int_T2_D6 is
         range -2 ** (T2_D6'Size - 1) ..
                2 ** (T2_D6'Size - 1) - 1;

    end Custom_Decimal_Types;

We can use an overlay to uncover the actual integer values stored on the
machine when assigning values to objects of decimal type. For example:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Machine_Implementation_Decimal_Types
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Machine_Implementation is
       V_T0_D4     : T0_D4;
       V_Int_T0_D4 : Int_T0_D4
         with Address => V_T0_D4'Address,
              Import, Volatile;

       V_T2_D6     : T2_D6;
       V_Int_T2_D6 : Int_T2_D6
         with Address => V_T2_D6'Address,
              Import, Volatile;
    begin
       V_T0_D4 := 1.0;
       Put_Line ("1.0 (T0_D4)        : "
                 & V_T0_D4'Image);
       Put_Line ("1.0 (Int_T0_D4)    : "
                 & V_Int_T0_D4'Image);

       V_T2_D6 := 1.55;
       V_T0_D4 := T0_D4 (V_T2_D6);
       Put_Line ("1.55 (T0_D4)       : "
                 & V_T0_D4'Image);
       Put_Line ("1.55 (Int_T0_D4)   : "
                 & V_Int_T0_D4'Image);

       V_T0_D4 := 2.0;
       Put_Line ("2.0 (T0_D4)        : "
                 & V_T0_D4'Image);
       Put_Line ("2.0 (Int_T0_D4)    : "
                 & V_Int_T0_D4'Image);

       Put_Line ("-----------------------------");

       V_T2_D6 := 1.0;
       Put_Line ("1.00 (T2_D6)        : "
                 & V_T2_D6'Image);
       Put_Line ("1.00 (Int_T2_D6)    : "
                 & V_Int_T2_D6'Image);

       V_T2_D6 := 1.55;
       Put_Line ("1.55 (T2_D6)        : "
                 & V_T2_D6'Image);
       Put_Line ("1.55 (Int_T2_D6)    : "
                 & V_Int_T2_D6'Image);

       V_T2_D6 := 2.0;
       Put_Line ("2.00 (T2_D6)        : "
                 & V_T2_D6'Image);
       Put_Line ("2.00 (Int_T2_D6)    : "
                 & V_Int_T2_D6'Image);

       Put_Line ("-----------------------------");
    end Show_Machine_Implementation;

In this example, we use the overlays :ada:`V_Int_T0_D4` and :ada:`V_Int_T2_D6`
to retrieve the integer representation of the decimal variables :ada:`V_T0_D4`
and :ada:`V_T2_D6`. By doing this, we retrieve the machine representation of
the real values for the :ada:`T0_D4` and :ada:`T2_D6` types. The table shows
the values that we get by running the test application:

+-------------+-----------------------------+
| Real value  | Integer representation      |
|             +--------------+--------------+
|             | :ada:`T0_D4` | :ada:`T2_D6` |
|             | type         | type         |
+=============+==============+==============+
|        1.00 |            1 |          100 |
+-------------+--------------+--------------+
|        1.55 |            1 |          155 |
+-------------+--------------+--------------+
|        2.00 |            2 |          200 |
+-------------+--------------+--------------+

In other words, integer values are being used |mdash| with an associated
scalefactor based on powers of ten |mdash| to represent decimal fixed-point
types on the target machine.

The scalefactor is 1 (or 10\ :sup:`0`) for the :ada:`T0_D4` type and 0.01
(or 10\ :sup:`-2`) for the :ada:`T2_D6` type. As you have might have noticed,
this scalefactor is equal to the *delta* we've used in the type declaration.
In actuality, however, the scalefactor is the *small* of the type |mdash|
which, as we've seen before, is equal to the *delta* for decimal fixed-point
types. (Later on, we see that this *detail* makes a difference for ordinary
fixed-point types.)

For example, if we multiple the integer representation of the real value by the
*small*, we get the real value:

+-------------+-------------------------------+
| Real value  | :ada:`T2_D6` type             |
|             +-------------------------------+
|             | Integer representation        |
|             | multiplied by the *small*     |
+=============+===============================+
|        1.00 |                  = 100 * 0.01 |
+-------------+-------------------------------+
|        1.55 |                  = 155 * 0.01 |
+-------------+-------------------------------+
|        2.00 |                  = 200 * 0.01 |
+-------------+-------------------------------+


.. _Adv_Ada_Fixed_Point_Types_Conversions:

Type conversion using fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we briefly discuss
:ref:`type conversion <Adv_Ada_Type_Conversion>` using fixed-point types: this
includes the conversion between
:ref:`fixed-point types <Adv_Ada_Fixed_Point_Type_Conversion>` and the
:ref:`conversion to other types <Adv_Ada_Fixed_Point_Type_Conversion_Other_Types>`
such as floating-point types.


.. _Adv_Ada_Fixed_Point_Type_Conversion:

Type conversion between fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with an example of type conversion between decimal fixed-point
types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Type_Conversions

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Type_Conversions is

       type Decimal is
         delta 10.0 ** (-2) digits 9;

       type Long_Long_Decimal is
         delta 10.0 ** (-2) digits 38;

       D   : Decimal;
       Acc : Long_Long_Decimal;
    begin
       D   := 2.0;
       Acc := Long_Long_Decimal (D);

       Put_Line ("D   = "
                 & D'Image);
       Put_Line ("Acc = "
                 & Acc'Image);
       Put_Line ("--------------");

       Acc := 10.0;
       D   := Decimal (Acc);

       Put_Line ("D   = "
                 & D'Image);
       Put_Line ("Acc = "
                 & Acc'Image);
    end Show_Decimal_Type_Conversions;

In this example, we convert the value of :ada:`D` |mdash| from the
:ada:`Decimal` to the :ada:`Long_Long_Decimal` type |mdash| by writing
:ada:`Long_Long_Decimal (D)`. Similarly, we convert the value of
:ada:`Acc` by writing :ada:`Decimal (Acc)`, which converts it from the
:ada:`Long_Long_Decimal` to the :ada:`Decimal` type.


.. _Adv_Ada_Fixed_Point_Type_Conversion_Other_Types:

Conversion to other types
^^^^^^^^^^^^^^^^^^^^^^^^^

Similarly, we can convert from and to fixed-point types when using
other numeric types such as integer and floating-point types.

Let's see an example for decimal fixed-point types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Type_Conversions_Other_Types

    package Custom_Types is

       type Decimal is
         delta 10.0 ** (-2) digits 6;
       --  Decimal type

       type TD18 is
         digits 18;
       --  Floating-point type

       type TD18_1000 is
         digits 18
           range -1_000.0 .. 1_000.0;
       --  Range-constrained
       --  floating-point type

    end Custom_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Types;
    use  Custom_Types;

    procedure Show_Decimal_Type_Conversions is
       D6       : Decimal;
       D18      : TD18;
       D18_1000 : TD18_1000;
    begin
       D6  := Decimal'Last;
       D18 := TD18 (D6);
       --     ^^^^^^^^^
       --  Conversion from
       --  decimal fixed-point

       Put_Line ("D6       = "
                 & D6'Image);
       Put_Line ("D18      = "
                 & D18'Image);

       D18 := TD18 (Decimal'Last);
       D6  := Decimal (D18);
       --     ^^^^^^^^^^
       --  Conversion to
       --  decimal fixed-point

       Put_Line ("D6       = "
                 & D6'Image);
       Put_Line ("D18      = "
                 & D18'Image);

       D6       := 800.0;
       D18_1000 := TD18_1000 (D6);
       --          ^^^^^^^^^^^^^^
       --  Conversion from
       --  decimal fixed-point

       Put_Line ("D6       = "
                 & D6'Image);
       Put_Line ("D18_1000 = "
                 & D18_1000'Image);

    end Show_Decimal_Type_Conversions;

In this example, we declare the decimal fixed-point type :ada:`Decimal` and the
floating-point type :ada:`TD18`. Conversion between these two types works as
expected: we use :ada:`TD18 (D6)` to convert from a decimal fixed-point type
and :ada:`Decimal (D18)` to convert to a decimal fixed-point type.

Of course, when converting to a fixed-point type, we have to ensure
that the floating-point value is in the range that is suitable for the target
type. Likewise, the same applies when converting from a fixed-point
type to a floating-point type |mdash| if we had assigned 2000.0 to :ada:`D6`
instead of 800.0, for example, the conversion :ada:`TD18_1000 (D6)` would have
raised a :ada:`Constraint_Error` because of the failed range check.


Operations using universal fixed types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's look at how fixed-point types behave in the case of operations
that make use of :ref:`universal fixed types <Adv_Ada_Universal_Fixed>`.

When mixing objects of different fixed-point types, as usual, we can use
:ref:`type conversions <Adv_Ada_Fixed_Point_Type_Conversion>`, e.g.
when  assigning the result to an object of a different type. As we've mentioned
before, type conversions between fixed-point types make use of universal
fixed-point types.

.. ::

    Consider the following package from a previous section:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_2

        package Custom_Decimal_Types is

           type Short_Decimal is
             delta 10.0 ** (-0) digits 4;
            --  range -9_999.0 .. 9_999.0;

           type Decimal is
             delta 10.0 ** (-2) digits 6;
            --  range -9_999.99 .. 9_999.99;

        end Custom_Decimal_Types;

    Let's look at a simple example of type conversions between these types:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_2

        with Ada.Text_IO; use Ada.Text_IO;

        with Custom_Decimal_Types;
        use  Custom_Decimal_Types;

        procedure Show_Mixing_Decimal_Types is
           A : Short_Decimal;
           B : Decimal;
        begin
           A := Short_Decimal'Last;     --  9_999.0
           B := Decimal (A);
           Put_Line ("A =     " &
                     A'Image);
           Put_Line ("B = A = " &
                     B'Image);

           Put_Line ("--------------");
           B := 9_999.0;
           A := Short_Decimal (B);
           Put_Line ("B =     " &
                     B'Image);
           Put_Line ("A = B = " &
                     A'Image);

        end Show_Mixing_Decimal_Types;

    Here, we use :ada:`Decimal (A)` and :ada:`Short_Decimal (B)` to convert to :ada:`Decimal`
    and :ada:`Short_Decimal`, respectively.

    Note that, if we had assigned :ada:`9_999.99` (or :ada:`Decimal`) to :ada:`B` in
    the code above, the :ada:`Short_Decimal (B)` would raise a :ada:`Constraint_Error`
    exception due the small difference in the range that we mentioned previously.

In addition, the multiplication and division operations also make use of
universal fixed types. Consider the following package with decimal fixed-point
types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_Decimal

    package Custom_Decimal_Types is

       type Short_Decimal is
         delta 10.0 ** (-0) digits 4;
        --  range -9_999.0 .. 9_999.0;

       type Decimal is
         delta 10.0 ** (-2) digits 6;
        --  range -9_999.99 .. 9_999.99;

    end Custom_Decimal_Types;

Let's look at a code example using the multiplication operation applied to two
objects of different decimal types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_Decimal

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Mixing_Decimal_Types is
       A : Short_Decimal;
       B : Decimal;
    begin
       A := 1000.0;
       B := 0.19;
       Put_Line ("A = " &
                 A'Image);
       Put_Line ("B = " &
                 B'Image);
       Put_Line ("----------");

       A := A * B;
       Put_Line ("A := A * B");
       Put_Line ("A = " &
                 A'Image);
    end Show_Mixing_Decimal_Types;

In this example, the :ada:`A * B` expression makes use of universal fixed
types. If this wasn't the case, :ada:`B` would have to be first convert to the
:ada:`Short_Decimal` type, and the result of the operation would be zero:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_Decimal

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Mixing_Decimal_Types is
       A : Short_Decimal;
       B : Decimal;
    begin
       A := 1000.0;
       B := 0.19;
       Put_Line ("A = " &
                 A'Image);
       Put_Line ("B = " &
                 B'Image);
       Put_Line ("----------");

       A := A * Short_Decimal (B);
       Put_Line ("A := A * B");
       Put_Line ("A = " &
                 A'Image);
    end Show_Mixing_Decimal_Types;

Because universal fixed types are used for the :ada:`A * B` operation, we
don't have to perform type conversion before the multiplication, and the result
of the operation has a meaningful value.

Note that, after the :ada:`A * B` operation, the result of the operation is
converted from universal fixed to the actual type we're using in the assignment
|mdash| :ada:`Short_Decimal` in this case.

For the division operation, universal fixed types are used as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_Decimal

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Mixing_Decimal_Types is
       A : Short_Decimal;
       B : Decimal;
    begin
       A := 1000.0;
       B := 0.19;
       Put_Line ("A = " &
                 A'Image);
       Put_Line ("B = " &
                 B'Image);
       Put_Line ("----------");

       A := A / B;
       Put_Line ("A := A / B");
       Put_Line ("A = " &
                 A'Image);
    end Show_Mixing_Decimal_Types;

Similar to the previous example, objects :ada:`A` and :ada:`B` have different
types, and the :ada:`A / B` expression makes use of universal fixed types.

.. admonition:: For further reading...

    Note that we can use *explicit* type conversions, and the results is still
    the same:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Universal_Fixed_Decimal

        with Ada.Text_IO; use Ada.Text_IO;

        with Custom_Decimal_Types;
        use  Custom_Decimal_Types;

        procedure Show_Mixing_Decimal_Types is
           A : Short_Decimal;
           B : Decimal;
        begin
           A := 1000.0;
           B := 0.19;
           Put_Line ("A = " &
                     A'Image);
           Put_Line ("B = " &
                     B'Image);
           Put_Line ("----------");

           A := Short_Decimal (Decimal (A) / B);
           Put_Line ("A := A / B");
           Put_Line ("A = " &
                     A'Image);
        end Show_Mixing_Decimal_Types;

    Here, we convert :ada:`A` from the :ada:`Short_Decimal` to the :ada:`Decimal`
    type before performing the division operation. After the division operation
    is finished, we convert the resulting value back to the :ada:`Short_Decimal` type,
    and then assign the converted value to :ada:`A`. Note, however, that the
    division operation itself is still performed using universal fixed types.
    (Also, keep in mind that the type conversion is also performed using
    universal fixed types, too.)


.. _Adv_Ada_Decimal_Fixed_Point_Types:

Decimal fixed-point types
-------------------------

We already introduced decimal fixed-point types in the
:ref:`Introduction to Ada <Intro_Ada_Decimal_Fixed_Point_Types>` course.
These types are useful, for example, for financial applications.

This is the syntax of a simple decimal fixed-point type declaration:

.. code-block:: ada

    type <type-name> is delta <delta-value> digits <digits-value>;

In this case, the :ada:`delta` and the :ada:`digits` specifications are used by
the compiler to derive a range.

Note that, unlike floating-point types, there are no predefined decimal
fixed-point types such as :ada:`Decimal`, :ada:`Long_Decimal`, and
:ada:`Long_Long_Decimal`. In fact, all decimal types are always custom types.

In terms of syntax, the main difference between the declaration of a custom
floating-point type and a decimal fixed-point type is the delta specification:

.. code:: ada compile_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Decimal_Vs_Floating_Point_Type_Declarations

    package Decimal_Vs_Float_Type_Decl is

        --
        --  Decimal type declaration
        --
        type Decimal_D3 is
          delta 0.1 digits 3;

        --
        --  Floating-point type declaration
        --
        type Float_D3 is
          digits 3;

    end Decimal_Vs_Float_Type_Decl;

In this example, we declare the decimal type :ada:`Decimal_D3` and the
floating-point type :ada:`Float_D3`. In terms of syntax, the :ada:`delta`
indicates that the type is fixed-point, while the :ada:`digits` specification
is used in both floating-point and decimal fixed-point type declarations.
Again, when both :ada:`delta` and :ada:`digits` keywords are combined in a
type declaration, we have a decimal fixed-point type declaration.

The *delta* is a scaling factor (a power of ten) that allows developers to
specify the required decimal precision. On the target machine, decimal
fixed-point types are represented as integers, which are implicitly scaled by
the specified power of 10. (We discuss
:ref:`machine representation of decimal fixed-point types <Adv_Ada_Decimal_Fixed_Point_Machine_Representation>`
later on.)
Also, as mentioned :ref:`earlier on <Adv_Ada_Fixed_Point_Types_Small_Delta>`,
for decimal fixed-point types, the *small* is automatically selected by the
compiler, and it's always equal to the *delta*.

Let's look at a small, practical example showing the conversion between two
currencies |mdash| in this case, between euros and yen:

.. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Currency_Conversion

    package Currencies is

       type EUR is
         delta 0.01 digits 12;

       type Yen is
         delta 1.0 digits 12;

       --  Exchange rates as of
       --  2025-12-26:
       EUR_Per_Yen : constant := 184.365_5;
       Yen_Per_EUR : constant := 0.005_42;

       function To_EUR (Y : Yen)
                       return EUR is
         (Y * Yen_Per_EUR);

       function To_Yen (E : EUR)
                       return Yen is
         (E * EUR_Per_Yen);

    end Currencies;

    with Ada.Text_IO; use Ada.Text_IO;
    with Currencies;  use Currencies;

    procedure Show_Currency_Conversion is
       E : EUR;
       Y : Yen;
    begin
       Y := 1000.0;
       Put_Line (Y'Image
                 & " JPY = "
                 & To_EUR (Y)'Image
                 & " EUR");

       E := 10.0;
       Put_Line (E'Image
                 & " EUR = "
                 & To_Yen (E)'Image
                 & " JPY");
    end Show_Currency_Conversion;

In this example, we see the conversion from 1000 yen to euros, as well as 10
euros to yen. We have two decimal fixed-point data types for the currencies:
:ada:`EUR` and :ada:`Yen`. As the function names imply, we use the
:ada:`To_EUR` function to convert to the :ada:`EUR` type and the :ada:`To_Yen`
function to convert to the :ada:`Yen` type.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.9 Fixed Point Types <3-5-9>`


.. _Adv_Ada_Decimal_Fixed_Point_Types_Decimal_Precision:

Decimal precision
~~~~~~~~~~~~~~~~~

Previously, we talked about the
:ref:`decimal precision of floating-point types <Adv_Ada_Floating_Point_Types_Decimal_Precision>`.
Now, let's focus on decimal precision in the context of decimal fixed-point
types.

As expected, we can adjust the number of significant decimal digits of a
decimal type via the :ada:`digits` specification, which should be based
on the numeric requirements of our implementation. Also, we
can obviously declare types that have the same delta, but different decimal
precision.

In the example below, we declare two data types: :ada:`T3_D3` and :ada:`T6_D3`.
For both types, the *delta* is the same: 0.001.

.. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Decimal_Precision

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Precision is
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
    end Show_Decimal_Precision;

When running the application, we confirm that the delta value of both
types is indeed the same: 0.001. However, because :ada:`T3_D3` is restricted
to 3 digits, its range goes from -0.999 to 0.999. For the :ada:`T6_D3`, we've
specified a precision of 6 digits, so the range goes from -999.999 to 999.999.
As usual, runtime checks are used to ensure that objects of decimal
fixed-point types do not have values that are out of range.

(Note that, in this code example, we use the
:ref:`First and Last attributes <Adv_Ada_Scalar_Type_Attributes>`, and the
:ref:`Delta attribute <Adv_Ada_Fixed_Point_Type_Small_Delta_Attributes>`.)

Also, if the
result of a multiplication or division using decimal fixed-point types is
smaller than the delta value required for the context, the actual result will
be zero. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Smaller

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Smaller is
       type T3_D3 is
         delta 10.0 ** (-3) digits 3;
       type T6_D6 is
         delta 10.0 ** (-6) digits 6;

       A, B : T3_D3;
       C    : T6_D6;
    begin
       A := T3_D3'Delta;
       B := 0.5;

       Put_Line ("The value of A     is "
                 & T3_D3'Image (A));
       Put_Line ("The value of B     is "
                 & T3_D3'Image (B));

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
because the *delta* is 0.001, the actual value stored in variable
:ada:`A` is zero. However, if the target object has sufficient precision, which
is the case for the :ada:`C` variable of :ada:`T6_D6` type, it can store the
0.0005 value.


Scale and delta
~~~~~~~~~~~~~~~

The previous example purposefully used the form :ada:`10.0 ** (-3)` to declare
the delta of decimal fixed-point types. Here, the variable :ada:`N` in the
expression 10\ :sup:`-N` is the scale. In Ada terms, this corresponds to
:ada:`Delta_Value : constant := 10.0 ** (-Scale_Value);`. (Note that the scale
:ada:`N` has a minus sign. We talk more about that later on.)

This terminology is important because, as we see later on, the
:ref:`min. and max. values for the scale <Adv_Ada_Decimal_Fixed_Point_Min_Max_Scale_Delta>`
depend on the compiler and target platform. In fact, the values of min. and
max. delta are simply derived from the values of :ada:`Min_Scale` and
:ada:`Max_Scale`, which are compiler-defined values that can vary according to
the specific target platform.

Although we might commonly see positive values or zero for the scale |mdash| in
some cases, the :ada:`N` scale might even be negative. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Positive_Scale

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Positive_Scale is

       type TP3_D3 is
         delta 10.0 ** 3 digits 2;
         --           ^^^
         --  Scale N is -(-3), i.e.:
         --  TP3_D3'Scale = -3

    begin
       Put_Line ("TP3_D3'Range : "
                 & TP3_D3'First'Image
                 & " .. "
                 & TP3_D3'Last'Image);
       Put_Line ("TP3_D3'Delta : "
                 & TP3_D3'Delta'Image);
    end Show_Positive_Scale;

In this example, we have a scale of -3, so the corresponding delta of type
:ada:`TP3_D3` is 10\ :sup:`-(-3)` (i.e. 10\ :sup:`3`, or 1000). This means that
even a value such as 999.0 is too small to be represented by an object
of this type. Accordingly, we see that this type has a range between -99,000
and 99,000. (We discuss
:ref:`ranges of decimal fixed-point types <Adv_Ada_Decimal_Fixed_Point_Type_Ranges>`
later on.)


.. _Adv_Ada_Decimal_Fixed_Point_Derived_Types_Subtypes:

Derived decimal fixed-point types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we present a brief discussion about types derived from
decimal fixed-point types, as well as subtypes of decimal fixed-point types.

Constraining decimal precision
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also constrain the decimal precision of the derived type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Derived_Decimal_Fixed_Point_Types
    :class: ada-run-expect-failure

    package Custom_Decimal_Types is

       type T2_D6 is
         delta 10.0 ** (-2) digits 6;

       type Small_Money is new
         T2_D6;

       type Smaller_Money is new
         T2_D6 digits 2;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Derived_Decimal_Types is
       D  : T2_D6;
       SM : Smaller_Money;
    begin
       Put_Line ("T2_D6'Range : "
                 & T2_D6'First'Image
                 & " .. "
                 & T2_D6'Last'Image);
       Put_Line ("T2_D6'Delta : "
                 & T2_D6'Delta'Image);
       Put_Line ("--------------------");

       Put_Line ("Smaller_Money'Range : "
                 & Smaller_Money'First'Image
                 & " .. "
                 & Smaller_Money'Last'Image);
       Put_Line ("Smaller_Money'Delta : "
                 & Smaller_Money'Delta'Image);
       Put_Line ("--------------------");

       D  := 231.53;
       Put_Line ("D  = "
                 & D'Image);

       SM := Smaller_Money (D);
       Put_Line ("SM = "
                 & SM'Image);
    end Show_Derived_Decimal_Types;

In this example, we derive the :ada:`Smaller_Money` type from the
:ada:`T2_D6` type and decrease the decimal precision from 6 to 2 digits.
Because the *delta* of both types is the same, we see that the range of the
:ada:`Smaller_Money` type (from -0.99 to 0.99) is smaller than the range of the
:ada:`T2_D6` type (from 9999.99 to 9999.99).

As expected, the type conversion :ada:`Smaller_Money (D)` in this example
|mdash| from :ada:`T2_D6` to the :ada:`Smaller_Money` type |mdash| raises a
:ada:`Constraint_Error` exception because the value of :ada:`D` (231.53) is
beyond the range of the :ada:`Smaller_Money` type.




Decimal precision of the base type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We discussed :ref:`base types <Adv_Ada_Base_Types>` earlier on. Also, we
discussed the
:ref:`decimal precision of the base type of floating-point types <Adv_Ada_Floating_Point_Base_Type_Decimal_Precision>`.

We learned that the decimal precision of the base type of a floating-point
type :ada:`FPT` might be higher than the decimal precision we've specified
for type :ada:`FPT`. For decimal fixed-point types, however, the decimal
precision of the base type of a decimal fixed-point type :ada:`DT` always
matches the decimal precision of the :ada:`DT` type itself.

.. todo::

    Find specific ARM reference for matching decimal precision of DT and
    DT'Base.

For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Base_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Base_Type_Precision is

       type DT_6 is
         delta 10.0 ** (-2) digits 6;

       type DT_12 is
         delta 10.0 ** (-2) digits 12;

    begin
       Put_Line
         ("DT_6'Digits        :"
          & DT_6'Digits'Image
          & " digits");
       Put_Line
         ("DT_6'Base'Digits   :"
          & DT_6'Base'Digits'Image
          & " digits");
       Put_Line
         ("DT_12'Digits       :"
          & DT_12'Digits'Image
          & " digits");
       Put_Line
         ("DT_12'Base'Digits  :"
          & DT_12'Base'Digits'Image
          & " digits");
    end Show_Base_Type_Precision;

In this example, we see that the decimal precision of :ada:`DT_6` and
:ada:`DT_6'Base` is 6, while the decimal precision of :ada:`DT_12` and
:ada:`DT_12'Base` is 12.


Size of decimal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, we talked about the
:ref:`size of floating-point types <Adv_Ada_Floating_Point_Size>` and how the
number of digits might not have a direct impact on the type's size. In
contrast, for decimal fixed-point types, each digit increases the type's size.
Note, however, that the *delta* of the decimal type doesn't have an influence
on the type's size. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Size

    package Decimal_Types is

       type Decimal_1_Digits is
         delta 10.0 ** (-2) digits 1;
       type Decimal_2_Digits is
         delta 10.0 ** (-2) digits 2;
       type Decimal_3_Digits is
         delta 10.0 ** (-2) digits 3;
       type Decimal_4_Digits is
         delta 10.0 ** (-2) digits 4;
       type Decimal_5_Digits is
         delta 10.0 ** (-2) digits 5;
       type Decimal_6_Digits is
         delta 10.0 ** (-2) digits 6;
       type Decimal_7_Digits is
         delta 10.0 ** (-2) digits 7;
       type Decimal_8_Digits is
         delta 10.0 ** (-2) digits 8;
       type Decimal_9_Digits is
         delta 10.0 ** (-2) digits 9;
       type Decimal_10_Digits is
         delta 10.0 ** (-2) digits 10;
       type Decimal_11_Digits is
         delta 10.0 ** (-2) digits 11;
       type Decimal_12_Digits is
         delta 10.0 ** (-2) digits 12;
       type Decimal_13_Digits is
         delta 10.0 ** (-2) digits 13;
       type Decimal_14_Digits is
         delta 10.0 ** (-2) digits 14;
       type Decimal_15_Digits is
         delta 10.0 ** (-2) digits 15;
       type Decimal_16_Digits is
         delta 10.0 ** (-2) digits 16;
       type Decimal_17_Digits is
         delta 10.0 ** (-2) digits 17;
       type Decimal_18_Digits is
         delta 10.0 ** (-2) digits 18;
       type Decimal_19_Digits is
         delta 10.0 ** (-2) digits 19;
       type Decimal_20_Digits is
         delta 10.0 ** (-2) digits 20;
       type Decimal_21_Digits is
         delta 10.0 ** (-2) digits 21;
       type Decimal_22_Digits is
         delta 10.0 ** (-2) digits 22;
       type Decimal_23_Digits is
         delta 10.0 ** (-2) digits 23;
       type Decimal_24_Digits is
         delta 10.0 ** (-2) digits 24;
       type Decimal_25_Digits is
         delta 10.0 ** (-2) digits 25;
       type Decimal_26_Digits is
         delta 10.0 ** (-2) digits 26;
       type Decimal_27_Digits is
         delta 10.0 ** (-2) digits 27;
       type Decimal_28_Digits is
         delta 10.0 ** (-2) digits 28;
       type Decimal_29_Digits is
         delta 10.0 ** (-2) digits 29;
       type Decimal_30_Digits is
         delta 10.0 ** (-2) digits 30;
       type Decimal_31_Digits is
         delta 10.0 ** (-2) digits 31;
       type Decimal_32_Digits is
         delta 10.0 ** (-2) digits 32;
       type Decimal_33_Digits is
         delta 10.0 ** (-2) digits 33;
       type Decimal_34_Digits is
         delta 10.0 ** (-2) digits 34;
       type Decimal_35_Digits is
         delta 10.0 ** (-2) digits 35;
       type Decimal_36_Digits is
         delta 10.0 ** (-2) digits 36;
       type Decimal_37_Digits is
         delta 10.0 ** (-2) digits 37;
       type Decimal_38_Digits is
         delta 10.0 ** (-2) digits 38;

    end Decimal_Types;

    with Ada.Text_IO;   use Ada.Text_IO;

    with Decimal_Types; use Decimal_Types;

    procedure Show_Decimal_Digits_Size is
    begin
       Put_Line ("Decimal_1_Digits'Size   :"
                 & Decimal_1_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_2_Digits'Size   :"
                 & Decimal_2_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_3_Digits'Size   :"
                 & Decimal_3_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_4_Digits'Size   :"
                 & Decimal_4_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_5_Digits'Size   :"
                 & Decimal_5_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_6_Digits'Size   :"
                 & Decimal_6_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_7_Digits'Size   :"
                 & Decimal_7_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_8_Digits'Size   :"
                 & Decimal_8_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_9_Digits'Size   :"
                 & Decimal_9_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_10_Digits'Size  :"
                 & Decimal_10_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_11_Digits'Size  :"
                 & Decimal_11_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_12_Digits'Size  :"
                 & Decimal_12_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_13_Digits'Size  :"
                 & Decimal_13_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_14_Digits'Size  :"
                 & Decimal_14_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_15_Digits'Size  :"
                 & Decimal_15_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_16_Digits'Size  :"
                 & Decimal_16_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_17_Digits'Size  :"
                 & Decimal_17_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_18_Digits'Size  :"
                 & Decimal_18_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_19_Digits'Size  :"
                 & Decimal_19_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_20_Digits'Size  :"
                 & Decimal_20_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_21_Digits'Size  :"
                 & Decimal_21_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_22_Digits'Size  :"
                 & Decimal_22_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_23_Digits'Size  :"
                 & Decimal_23_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_24_Digits'Size  :"
                 & Decimal_24_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_25_Digits'Size  :"
                 & Decimal_25_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_26_Digits'Size  :"
                 & Decimal_26_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_27_Digits'Size  :"
                 & Decimal_27_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_28_Digits'Size  :"
                 & Decimal_28_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_29_Digits'Size  :"
                 & Decimal_29_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_30_Digits'Size  :"
                 & Decimal_30_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_31_Digits'Size  :"
                 & Decimal_31_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_32_Digits'Size  :"
                 & Decimal_32_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_33_Digits'Size  :"
                 & Decimal_33_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_34_Digits'Size  :"
                 & Decimal_34_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_35_Digits'Size  :"
                 & Decimal_35_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_36_Digits'Size  :"
                 & Decimal_36_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_37_Digits'Size  :"
                 & Decimal_37_Digits'Size'Image
                 & " bits");
       Put_Line ("Decimal_38_Digits'Size  :"
                 & Decimal_38_Digits'Size'Image
                 & " bits");
    end Show_Decimal_Digits_Size;

When running the application above, we see that the number of bits increases
for each digit that we *add* to our decimal type declaration. On a typical
desktop PC, we may see the following results:

+-------------+-------------+
| Digits      | Size (bits) |
+=============+=============+
|           1 |           5 |
+-------------+-------------+
|           2 |           8 |
+-------------+-------------+
|           3 |          11 |
+-------------+-------------+
|           4 |          15 |
+-------------+-------------+
|           5 |          18 |
+-------------+-------------+
|       [...] |       [...] |
+-------------+-------------+
|          10 |          35 |
+-------------+-------------+
|       [...] |       [...] |
+-------------+-------------+
|          18 |          61 |
+-------------+-------------+
|          19 |          65 |
+-------------+-------------+
|       [...] |       [...] |
+-------------+-------------+
|          38 |         128 |
+-------------+-------------+

When we look at the base type of these decimal fixed-point types, we see that
the actual size on hardware is usually bigger. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Size

    with Ada.Text_IO;   use Ada.Text_IO;

    with Decimal_Types; use Decimal_Types;

    procedure Show_Decimal_Digits_Size is
    begin
       Put_Line ("Decimal_1_Digits'Base'Size   :"
                 & Decimal_1_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_2_Digits'Base'Size   :"
                 & Decimal_2_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_3_Digits'Base'Size   :"
                 & Decimal_3_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_4_Digits'Base'Size   :"
                 & Decimal_4_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_5_Digits'Base'Size   :"
                 & Decimal_5_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_6_Digits'Base'Size   :"
                 & Decimal_6_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_7_Digits'Base'Size   :"
                 & Decimal_7_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_8_Digits'Base'Size   :"
                 & Decimal_8_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_9_Digits'Base'Size   :"
                 & Decimal_9_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_10_Digits'Base'Size  :"
                 & Decimal_10_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_11_Digits'Base'Size  :"
                 & Decimal_11_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_12_Digits'Base'Size  :"
                 & Decimal_12_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_13_Digits'Base'Size  :"
                 & Decimal_13_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_14_Digits'Base'Size  :"
                 & Decimal_14_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_15_Digits'Base'Size  :"
                 & Decimal_15_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_16_Digits'Base'Size  :"
                 & Decimal_16_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_17_Digits'Base'Size  :"
                 & Decimal_17_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_18_Digits'Base'Size  :"
                 & Decimal_18_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_19_Digits'Base'Size  :"
                 & Decimal_19_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_20_Digits'Base'Size  :"
                 & Decimal_20_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_21_Digits'Base'Size  :"
                 & Decimal_21_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_22_Digits'Base'Size  :"
                 & Decimal_22_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_23_Digits'Base'Size  :"
                 & Decimal_23_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_24_Digits'Base'Size  :"
                 & Decimal_24_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_25_Digits'Base'Size  :"
                 & Decimal_25_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_26_Digits'Base'Size  :"
                 & Decimal_26_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_27_Digits'Base'Size  :"
                 & Decimal_27_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_28_Digits'Base'Size  :"
                 & Decimal_28_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_29_Digits'Base'Size  :"
                 & Decimal_29_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_30_Digits'Base'Size  :"
                 & Decimal_30_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_31_Digits'Base'Size  :"
                 & Decimal_31_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_32_Digits'Base'Size  :"
                 & Decimal_32_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_33_Digits'Base'Size  :"
                 & Decimal_33_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_34_Digits'Base'Size  :"
                 & Decimal_34_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_35_Digits'Base'Size  :"
                 & Decimal_35_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_36_Digits'Base'Size  :"
                 & Decimal_36_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_37_Digits'Base'Size  :"
                 & Decimal_37_Digits'Base'Size'Image
                 & " bits");
       Put_Line ("Decimal_38_Digits'Base'Size  :"
                 & Decimal_38_Digits'Base'Size'Image
                 & " bits");
    end Show_Decimal_Digits_Size;

On a typical desktop PC, we may see the following results:

+---------------------------------------------------+-----------+
| Decimal Type                                      | Base Type |
+-------------+-------------+-----------+-----------+-----------+
| Min. digits | Max. digits | Min. Size | Max. Size | Size      |
|             |             | (bits)    | (Bits)    | (bits)    |
+=============+=============+===========+===========+===========+
|           1 |           2 |         5 |         8 |         8 |
+-------------+-------------+-----------+-----------+-----------+
|           3 |           4 |        11 |        15 |        16 |
+-------------+-------------+-----------+-----------+-----------+
|           5 |           9 |        18 |        31 |        32 |
+-------------+-------------+-----------+-----------+-----------+
|          10 |          18 |        35 |        61 |        64 |
+-------------+-------------+-----------+-----------+-----------+
|          19 |          38 |        65 |       128 |       128 |
+-------------+-------------+-----------+-----------+-----------+

In other words, while the size of a decimal fixed-point type varies according
to the number of digits, the size of the base type (on a typical desktop PC)
corresponds to common power-of-two sizes such as 8, 16, 32, 64, and 128 bits.


.. todo:

    .. _Adv_Ada_Decimal_Fixed_Point_Type_Size:

    Custom size of decimal fixed-point types
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Discuss :ada:`Decimal_6_Digits'Size` vs. :ada:`Decimal_6_Digits'Base'Size`.


.. _Adv_Ada_Decimal_Fixed_Point_Type_Ranges:

Range of decimal fixed-point types and subtypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we discuss how to retrieve the range information of decimal
fixed-point types and subtypes. Also, we look at how we can use the
:ada:`range` specification to restrict the range of derived types.


Range of decimal fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As we've seen in the
:ref:`Introduction to Ada course <Intro_Ada_Decimal_Fixed_Point_Types_Digits>`,
the :ada:`digits` part of the type declaration determines the number of digits
that the decimal fixed-point type is able to represent. For example, by writing
:ada:`digits 3` and specifying a delta of 10\ :sup:`0` (1.0), we're able to
represent values with three digits ranging from -999 to 999 |mdash| this
corresponds to a range from -10\ :sup:`3` + 1 to 10\ :sup:`3` - 1. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Range is

       type D1 is
         delta 1.0 digits 1;
       type D2 is
         delta 1.0 digits 2;
       type D3 is
         delta 1.0 digits 3;
       type D6 is
         delta 1.0 digits 6;
       type D38 is
         delta 1.0 digits 38;

    begin
       Put_Line ("D1'Range : "
                 & D1'First'Image
                 & " .. "
                 & D1'Last'Image);
       Put_Line ("D2'Range : "
                 & D2'First'Image
                 & " .. "
                 & D2'Last'Image);
       Put_Line ("D3'Range : "
                 & D3'First'Image
                 & " .. "
                 & D3'Last'Image);
       Put_Line ("D6'Range : "
                 & D6'First'Image
                 & " .. "
                 & D6'Last'Image);
       Put_Line ("D38'Range : "
                 & D38'First'Image
                 & " .. "
                 & D38'Last'Image);
    end Show_Decimal_Range;

In this example, we declare multiple decimal types. This is the range of each
one of them:

+------------+---------------------------------------------+------------------------------------------+
| Type       | Min. value                                  | Max. value                               |
+============+=============================================+==========================================+
| :ada:`D1`  |                                        -9.0 |                                      9.0 |
+------------+---------------------------------------------+------------------------------------------+
| :ada:`D2`  |                                       -99.0 |                                     99.0 |
+------------+---------------------------------------------+------------------------------------------+
| :ada:`D3`  |                                      -999.0 |                                    999.0 |
+------------+---------------------------------------------+------------------------------------------+
| :ada:`D6`  |                                   -999999.0 |                                 999999.0 |
+------------+---------------------------------------------+------------------------------------------+
| :ada:`D38` |   -99999999999999999999999999999999999999.0 | 99999999999999999999999999999999999999.0 |
+------------+---------------------------------------------+------------------------------------------+

As mentioned earlier on, the range is derived from the :ada:`digits`:

+------------+------------------+--------------------+--------------------+
| Type       | Type             | Min. value         | Max. value         |
+============+==================+====================+====================+
| :ada:`D1`  | :ada:`digits 1`  |  -10\ :sup:`1` + 1 |   10\ :sup:`1` - 1 |
+------------+------------------+--------------------+--------------------+
| :ada:`D2`  | :ada:`digits 2`  |  -10\ :sup:`2` + 1 |   10\ :sup:`2` - 1 |
+------------+------------------+--------------------+--------------------+
| :ada:`D3`  | :ada:`digits 3`  |  -10\ :sup:`3` + 1 |   10\ :sup:`3` - 1 |
+------------+------------------+--------------------+--------------------+
| :ada:`D6`  | :ada:`digits 6`  |  -10\ :sup:`6` + 1 |   10\ :sup:`6` - 1 |
+------------+------------------+--------------------+--------------------+
| :ada:`D38` | :ada:`digits 38` | -10\ :sup:`38` + 1 |  10\ :sup:`38` - 1 |
+------------+------------------+--------------------+--------------------+


Custom range of decimal fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to floating-point types, we can define custom ranges for decimal
fixed-point types by using the :ada:`range` keyword. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Custom_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Custom_Range is

       type D6 is
         delta 1.0 digits 6;
       type D6_R100 is
         delta 1.0 digits 6
           range -100_000.0 .. 100_000.0;

    begin
       Put_Line ("D6'Range      : "
                 & D6'First'Image
                 & " .. "
                 & D6'Last'Image);
       Put_Line ("D6_R100'Range : "
                 & D6_R100'First'Image
                 & " .. "
                 & D6_R100'Last'Image);
    end Show_Decimal_Custom_Range;

In this example, we declare the :ada:`D6` type with :ada:`digits 6`, which
has a range between -999,999.0 and 999,999.0. In addition, we declare the
:ada:`D6_R100` type, which has the same number of significant digits, but is
constrained to the range between -100,000.0 and 100,000.0.


Range of derived decimal fixed-point types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also derive from decimal fixed-point types and limit the range at the
same time |mdash| as we can do with floating-point types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Derived_Decimal_Fixed_Point_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Derived_Decimal_Range is

       type D6 is
         delta 1.0 digits 6;

       type D6_RD3 is new D6
         range -999.0 .. 999.0;

       type D6_R5 is new D6
         range -5.0 .. 5.0;

    begin
       Put_Line ("D6'Range     : "
                 & D6'First'Image
                 & " .. "
                 & D6'Last'Image);
       Put_Line ("D6_RD3'Range : "
                 & D6_RD3'First'Image
                 & " .. "
                 & D6_RD3'Last'Image);
       Put_Line ("D6_R5'Range  : "
                 & D6_R5'First'Image
                 & " .. "
                 & D6_R5'Last'Image);
    end Show_Derived_Decimal_Range;

Here, :ada:`D6_RD3` and :ada:`D6_R5` types are both derived from the :ada:`D6`
type, which ranges from -999,999.0 to 999,999.0. For the derived type
:ada:`D6_RD3`, we constrain the original range to an interval between -999.0
and 999.0. For :ada:`D6_R5`, we constrain the type's range to an interval
between -5.0 and 5.0.


Range of decimal fixed-point subtypes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similarly, we can declare subtypes of decimal fixed-point types and limit the
range at the same time. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Subtype_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Subtype_Range is

       type D6 is
         delta 1.0 digits 6;

       subtype D6_RD3 is D6
         range -999.0 .. 999.0;

       subtype D6_R5 is D6
         range -5.0 .. 5.0;

    begin
       Put_Line ("D6'Range     : "
                 & D6'First'Image
                 & " .. "
                 & D6'Last'Image);
       Put_Line ("D6_RD3'Range : "
                 & D6_RD3'First'Image
                 & " .. "
                 & D6_RD3'Last'Image);
       Put_Line ("D6_R5'Range  : "
                 & D6_R5'First'Image
                 & " .. "
                 & D6_R5'Last'Image);
    end Show_Decimal_Subtype_Range;

Now, :ada:`D6_RD3` and :ada:`D6_R5` are subtypes of the :ada:`D6` type, which
has a range between -999,999.0 to 999,999.0. For these subtypes, we use the
same ranges as in the previous code example |mdash| i.e. the range of the
:ada:`D6_RD3` type goes from -999.0 to 999.0, while the range of the
:ada:`D6_R5` type goes from -5.0 to 5.0.


Range of the base type
^^^^^^^^^^^^^^^^^^^^^^

Note that the range of a decimal fixed-point type might be smaller than the
range of its :ref:`base type <Adv_Ada_Base_Types>`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Base_Range

    with Ada.Text_IO;   use Ada.Text_IO;

    procedure Show_Decimal_Fixed_Point_Base_Range is

       type D6 is
         delta 1.0 digits 6;

    begin
       Put_Line ("D6'Range      : "
                 & D6'First'Image
                 & " .. "
                 & D6'Last'Image);
       Put_Line ("D6'Base'Range : "
                 & D6'Base'First'Image
                 & " .. "
                 & D6'Base'Last'Image);
    end Show_Decimal_Fixed_Point_Base_Range;

In this example, we see that the range of the :ada:`D6` goes from -999,999 to
999,999. The range of the base type, however, can be wider. On a desktop PC, it
might go from -2,147,483,648 to 2,147,483,647 |mdash| which corresponds to
-2\ :sup:`31` to 2\ :sup:`31` - 1. (The actual hardware representation has a
range based on powers of two in this case, while the range of decimal
fixed-point types is based on powers of ten.)


.. _Adv_Ada_Decimal_Fixed_Point_Type_Conversion:

Type conversion using decimal types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've already seen a couple of examples of
:ref:`type conversion <Adv_Ada_Fixed_Point_Types_Conversions>` between
fixed-point types. Let's continue the discussion with the following code
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Type_Conversions

    package Custom_Decimal_Types is

       type T2_D6 is
         delta 10.0 ** (-2) digits 6;

       type T2_D38 is
         delta 10.0 ** (-2) digits 38;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Decimal_Type_Conversions is
       D6  : T2_D6;
       D38 : T2_D38;
    begin
       D6  := T2_D6'Last;
       D38 := T2_D38 (D6);

       Put_Line ("D6  = "
                 & D6'Image);
       Put_Line ("D38 = "
                 & D38'Image);
    end Show_Decimal_Type_Conversions;

In this example, we convert the value of :ada:`D6` |mdash| from the
:ada:`T2_D6` to the :ada:`T2_D38` type |mdash| by writing :ada:`T2_D38 (D6)`.
This conversion is cannot is safe |mdash| i.e. it cannot raise an exception
|mdash| because the range of the target type is wider.

Of course, type conversions may fail when the ranges of two types don't
match |mdash| more specifically, when the value of an object is out of the
range of the type we're converting to. However, as expected, we can safely
convert to a decimal fixed-point type with a wider range.

We can also safely convert between decimal fixed-point types that have roughly
the same range |mdash| if we disconsider, of course, the truncation that
happens during the conversion. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Decimal_Type_Conversions

    package Custom_Decimal_Types is

       type T4_D8 is
         delta 10.0 ** (-4) digits 8;

       type T2_D6 is
         delta 10.0 ** (-2) digits 6;

       type T0_D4 is
         delta 10.0 ** (0) digits 4;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Decimal_Type_Conversions is
       D8 : T4_D8;
       D6 : T2_D6;
       D4 : T0_D4;
    begin
       D8 := T4_D8'Last;
       D6 := T2_D6 (D8);
       D4 := T0_D4 (D6);

       Put_Line ("D8  = "
                 & D8'Image);
       Put_Line ("D6  = "
                 & D6'Image);
       Put_Line ("D4  = "
                 & D4'Image);
    end Show_Decimal_Type_Conversions;

In this example, the value of :ada:`D8` is 9999.9999. When assigning the value
of :ada:`D8` to :ada:`D6`, the conversion from :ada:`T4_D8` to :ada:`T2_D6`
simply *removes* the last two digits (i.e. it truncates the value as expected),
so that the value becomes 9999.99. Similarly, the value becomes 9999.0 in the
conversion to the :ada:`T0_D4` type.


.. _Adv_Ada_Package_Decimal:

Package Decimal
~~~~~~~~~~~~~~~

The standard :ada:`Decimal` package contains information about the
:ref:`min. and max. values for the scale and delta <Adv_Ada_Decimal_Fixed_Point_Min_Max_Scale_Delta>`
of decimal fixed-point types. In addition, it contains the declaration of the
:ref:`generic Divide procedure <Adv_Ada_Decimal_Fixed_Point_Generic_Divide_Proc>`.


.. admonition:: In the Ada Reference Manual

    - :arm22:`F.2 The Package Decimal <F-2>`


.. _Adv_Ada_Decimal_Fixed_Point_Min_Max_Scale_Delta:

Min. and max. scale and delta
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Min_Scale` and :ada:`Max_Scale` values are the smallest and largest
values we can use for a scale :ada:`N` in the formula
:ada:`delta 10.0 ** (-N)`. Because the formula uses a negative exponent
(:ada:`-N`), this means that the minimum delta :ada:`Min_Delta` is calculated
with the :ada:`Max_Scale`, while the :ada:`Max_Delta` is calculated with the
:ada:`Min_Scale`. In fact, this is declaration of those constants in the
:ada:`Decimal` package:

.. code-block:: ada

    package Ada.Decimal is

       -- [...]

       Min_Delta : constant := 10.0 ** (-Max_Scale);
       Max_Delta : constant := 10.0 ** (-Min_Scale);

       --  [...]

    end Ada.Decimal.

Let's inspect the value of all these constants:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Min_Max_Scale

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Decimal; use Ada.Decimal;

    procedure Show_Min_Max_Scale is
    begin
       Put_Line ("Min_Scale : "
                 & Min_Scale'Image);
       Put_Line ("Max_Scale : "
                 & Max_Scale'Image);
       Put_Line ("--------------------");
       Put_Line ("Min_Delta : "
                 & Min_Delta'Image);
       Put_Line ("Max_Delta : "
                 & Max_Delta'Image);
    end Show_Min_Max_Scale;

On a typical desktop PC, you may see that the :ada:`Min_Scale` is -38, while
the :ada:`Max_Scale` is 38. Therefore, the :ada:`Min_Delta` is 10\ :sup:`-38`
and the :ada:`Max_Delta` is  10\ :sup:`38`.

The values of these constants depend on the compiler implementation and the
target platform. However, the standard requires that :ada:`Min_Scale` shall be
at most 0, while :ada:`Max_Scale` shall be at least 18. This means that the
smallest delta supported by an Ada compiler (:ada:`Min_Delta`) is at most
10\ :sup:`-18` (or smaller than that), while the largest delta supported by an
Ada compiler (:ada:`Max_Delta`) is at least 1.0 or more.

.. admonition:: For further reading...

    The :ada:`Scale` attribute gives us the scale :ada:`N` of a decimal
    fixed-point type. (We discuss the
    :ref:`Scale attribute <Adv_Ada_Decimal_Fixed_Point_Type_Scale_Attribute>`
    in the next chapter.) For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Scale_Attribute

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Scale_Attribute is
           type T4_D8 is
             delta 10.0 ** (-4) digits 8;
        begin
           Put_Line ("T4_D8'Scale : "
                     & T4_D8'Scale'Image);
        end Show_Scale_Attribute;

    By using the :ada:`Scale` attribute with the :ada:`T4_D8` type, we retrieve
    its scale, which is 4.


Max. decimal digits
^^^^^^^^^^^^^^^^^^^

The :ada:`Max_Decimal_Digits` defines the maximum value for the number of
significant decimal digits:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Max_Decimal_Digits

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Decimal; use Ada.Decimal;

    procedure Show_Max_Decimal_Digits is
    begin
       Put_Line ("Max_Decimal_Digits : "
                 & Max_Decimal_Digits'Image);
    end Show_Max_Decimal_Digits;

On a typical desktop PC, we may see that the value of :ada:`Max_Decimal_Digits`
is 38. The Ada standard requires that :ada:`Max_Decimal_Digits` must be at
least 18.

Note that there's no corresponding :ada:`Min_Decimal_Digits`. The minimum value
for the number of significant decimal digits is one.

.. admonition:: For further reading...

    The :ada:`Digits` attribute gives us the number of significant decimal
    digits of a decimal fixed-point type. (We discuss the
    :ref:`Digits attribute <Adv_Ada_Decimal_Fixed_Point_Type_Digits_Attribute>`
    in the next chapter.) For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Digits_Attribute

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Digits_Attribute is
           type T4_D8 is
             delta 10.0 ** (-4) digits 8;
        begin
           Put_Line ("T4_D8'Digits : "
                     & T4_D8'Digits'Image);
        end Show_Digits_Attribute;

    By using the :ada:`Digits` attribute of the :ada:`T4_D8` type, we retrieve
    its scale, which is 8.

If we consider a delta of 0.01, which we might typically encounter in financial
applications, we can calculate the corresponding largest range:

.. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Max_Decimal_Digits_Financial

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Decimal; use Ada.Decimal;

    procedure Max_Decimal_Digits_Financial is

       type Max_Fin_Decimal is
         delta 0.01
         digits Max_Decimal_Digits;

    begin
       Put_Line ("Max_Fin_Decimal'Range : "
                 & Max_Fin_Decimal'First'Image
                 & " .. "
                 & Max_Fin_Decimal'Last'Image);
       Put_Line ("Max_Fin_Decimal'Delta : "
                 & Max_Fin_Decimal'Delta'Image);
       Put_Line ("Max_Fin_Decimal'Size  : "
                 & Max_Fin_Decimal'Size'Image);
    end Max_Decimal_Digits_Financial;

In this example, the :ada:`Max_Fin_Decimal` type uses a delta of 0.01 and the
number of significant decimal digits based on the value of
:ada:`Max_Decimal_Digits`. On a typical desktop PC, this gives us (almost) a
range between -10\ :sup:`36` and 10\ :sup:`36` |mdash|  actually, it's a
range between -999,999,999,999,999,999,999,999,999,999,999,999.99 and
999,999,999,999,999,999,999,999,999,999,999,999.99, to be more precise. (Note
that, in this case, :ada:`Max_Fin_Decimal` is a 128-bit data type.)

.. admonition:: For further reading...

    By combining the values of :ada:`Min_Scale` and :ada:`Max_Decimal_Digits`,
    we get the largest possible numbers we can represent with decimal
    fixed-point types:

    .. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Max_Decimal_Digits_Min_Scale

        with Ada.Text_IO; use Ada.Text_IO;
        with Ada.Decimal; use Ada.Decimal;

        procedure Show_Max_Decimal_Digits_Min_Scale is

           type Max_Decimal is
             delta 10.0 ** (-Min_Scale)
             digits Max_Decimal_Digits;

        begin
           Put_Line ("Max_Decimal'Range : "
                     & Max_Decimal'First'Image
                     & " .. "
                     & Max_Decimal'Last'Image);
           Put_Line ("Max_Decimal'Delta : "
                     & Max_Decimal'Delta'Image);
           Put_Line ("Max_Decimal'Size  : "
                     & Max_Decimal'Size'Image);
        end Show_Max_Decimal_Digits_Min_Scale;

    In this example, we declare the :ada:`Max_Decimal` type, which allows for
    representing the largest possible numbers for decimal fixed-point types. In
    fact, the range of :ada:`Max_Decimal` goes from -10\ :sup:`76` to
    10\ :sup:`76`.
    Note, however, that the delta is quite large as well: 10\ :sup:`38` is the
    smallest value we can represent.

    By combining the values of :ada:`Max_Scale` and :ada:`Max_Decimal_Digits`,
    we get the smallest possible number we can represent with decimal
    fixed-point types:

    .. code:: ada run_button project=Courses.Advanced_Ada.Decimal_Fixed_Point_Types.Max_Decimal_Digits_Max_Scale

        with Ada.Text_IO; use Ada.Text_IO;
        with Ada.Decimal; use Ada.Decimal;

        procedure Show_Max_Decimal_Digits_Max_Scale is

           type Smallest_Decimal is
             delta 10.0 ** (-Max_Scale)
             digits Max_Decimal_Digits;

        begin
           Put_Line ("Smallest_Decimal'Range : "
                     & Smallest_Decimal'First'Image
                     & " .. "
                     & Smallest_Decimal'Last'Image);
           Put_Line ("Smallest_Decimal'Delta : "
                     & Smallest_Decimal'Delta'Image);
           Put_Line ("Smallest_Decimal'Size  : "
                     & Smallest_Decimal'Size'Image);
        end Show_Max_Decimal_Digits_Max_Scale;

    In this example, we declare the :ada:`Smallest_Decimal` type, which allows
    for representing the smallest possible number for decimal fixed-point
    types |mdash| in this case, it's -10\ :sup:`-38`. The range of this type
    is the normalized interval (-1.0, 1.0).


.. _Adv_Ada_Decimal_Fixed_Point_Generic_Divide_Proc:

Generic Divide procedure
^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we look into the generic :ada:`Divide` procedure. Before we do
so, however, let's look at an example of the division operator (:ada:`/`)
applied to objects of decimal fixed-point types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Divide_Procedure

    package Custom_Decimal_Types is

       type T0_D4 is
         delta 10.0 ** (-0) digits 4;

    end Custom_Decimal_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Divide_Procedure is

       Dividend  : T0_D4;
       Divisor   : T0_D4;
       Result    : T0_D4;
    begin
       Dividend := 501.0;
       Divisor  := 2.0;

       Result   := Dividend / Divisor;

       Put_Line ("Dividend           : "
                 & Dividend'Image);
       Put_Line ("Divisor            : "
                 & Divisor'Image);
       Put_Line ("Dividend / Divisor : "
                 & Result'Image);
    end Show_Divide_Procedure;

In this example, we calculate the result of the operation :ada:`501.0 / 2.0`
using objects of :ada:`T0_D4` type. As expected, due to the delta of this type
(1.0), the result is not 250.5, but instead 250.0. (In other words, we *lose*
0.5 in this operation because of the delta.)

However, we might want to get the quotient and remainder of the division
operation |mdash| so that we can keep track of errors, for example. For that,
we have to instantiate the generic :ada:`Divide` procedure for this type. Let's
look at a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Divide_Procedure

    with Ada.Decimal;
    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Divide_Procedure is

       procedure Div is new
         Ada.Decimal.Divide
           (Dividend_Type  => T0_D4,
            Divisor_Type   => T0_D4,
            Quotient_Type  => T0_D4,
            Remainder_Type => T0_D4);

       Dividend  : T0_D4;
       Divisor   : T0_D4;
       Quotient  : T0_D4;
       Remainder : T0_D4;
    begin
       Dividend := 501.0;
       Divisor  := 2.0;

       Div (Dividend, Divisor, Quotient, Remainder);

       Put_Line ("Dividend  : "
                 & Dividend'Image);
       Put_Line ("Divisor   : "
                 & Divisor'Image);
       Put_Line ("Quotient  : "
                 & Quotient'Image);
       Put_Line ("Remainder : "
                 & Remainder'Image);
    end Show_Divide_Procedure;

In this example, we declare the :ada:`Div` procedure as an instance of the
:ada:`Divide` procedure. Now, the result of the operation :ada:`501.0 / 2.0` is
a quotient of 250.0 (as we had before) with a remainder of 1.00.

Note that, in this particular case, we're using the :ada:`T0_D4` type for all
parameters (:ada:`Dividend_Type`, :ada:`Divisor_Type` :ada:`Quotient_Type` and
:ada:`Remainder_Type`) in the instantiation of the :ada:`Divide` procedure. We
could, however, have used different decimal fixed-point types as well.


Illegal decimal fixed-point type declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've seen before, we can declare
:ref:`custom ranges for decimal fixed-point types <Adv_Ada_Decimal_Fixed_Point_Type_Ranges>`.
However, as expected, if the range we're specifying is outside the maximum
range possible for that type, it is considered illegal:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Illegal_Decimal_Types
    :class: ada-expect-compile-error

    package Illegal_Decimal_Types is

       type T0_D4 is
         delta 10.0 ** (-0) digits 4
           range -10_000.0 .. 10_000.0;
           --    ^^^^^^^^^^^^^^^^^^^^^
           --  ERROR: outside the maximum range
           --         9_999.0 .. 9_999.0

    end Illegal_Decimal_Types;

In this example, the range we declare for the :ada:`T0_D4` type
(from -10,000 to 10,000) is outside the maximum range that the type allows
(from 9,999 to 9,999).


Operations on decimal types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we discuss some aspects of operations using objects of decimal
fixed-point types.

Mixing decimal types
^^^^^^^^^^^^^^^^^^^^

First, let's look at how we can mix decimal fixed-point types in operation such
as additions and subtractions.

Consider the following package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Mixing_Decimal_Types

    package Custom_Decimal_Types is

       type T0_D4 is
         delta 10.0 ** (-0) digits 4;
        --  range -9_999.0 .. 9_999.0;

       type T2_D6 is
         delta 10.0 ** (-2) digits 6;
        --  range -9_999.99 .. 9_999.99;

    end Custom_Decimal_Types;

The range of the :ada:`T0_D4` and :ada:`T2_D6` types from this example is quite
close: the range of :ada:`T0_D4` goes from -9,999.0 to 9,999.0, while the range
of :ada:`T2_D6` goes from -9,999.99 to 9,999.99. In other words, when comparing
the ranges, we see a small difference of 0.99 in the first and last values of
the ranges.

Let's look at simple operations such as :ada:`1000 + 500.25` and
:ada:`1000 - 500.25` when mixing these two decimal types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Mixing_Decimal_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Show_Mixing_Decimal_Types is
       A : T0_D4;
       B : T2_D6;
    begin
       A := 1000.0;
       B := 500.25;
       Put_Line ("A = " &
                 A'Image);
       Put_Line ("B = " &
                 B'Image);

       Put_Line ("--------------");
       Put_Line ("A := A + B");
       A := A + T0_D4 (B);
       Put_Line ("A = " &
                 A'Image);

       Put_Line ("--------------");
       A := 1000.0;
       B := 500.25;
       Put_Line ("A := A - B");
       A := A - T0_D4 (B);
       Put_Line ("A = " &
                 A'Image);
    end Show_Mixing_Decimal_Types;

In this example, during to the :ada:`T0_D4 (B)` conversion, we get the value
500.0 instead of 500.25 to the delta of the :ada:`T0_D4` type. (This is of
course the expected behavior for this type.) Therefore, the result of the
operation is 500.0.


.. ::

    Using universal fixed types
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Consider the following package from a previous section:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Universal_Fixed_2

        package Custom_Decimal_Types is

           type T0_D4 is
             delta 10.0 ** (-0) digits 4;
            --  range -9_999.0 .. 9_999.0;

           type T2_D6 is
             delta 10.0 ** (-2) digits 6;
            --  range -9_999.99 .. 9_999.99;

        end Custom_Decimal_Types;

    Let's look at a simple example of type conversions between these types:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Universal_Fixed_2

        with Ada.Text_IO; use Ada.Text_IO;

        with Custom_Decimal_Types;
        use  Custom_Decimal_Types;

        procedure Show_Mixing_Decimal_Types is
           A : T0_D4;
           B : T2_D6;
        begin
           A := T0_D4'Last;     --  9_999.0
           B := T2_D6 (A);
           Put_Line ("A =     " &
                     A'Image);
           Put_Line ("B = A = " &
                     B'Image);

           Put_Line ("--------------");
           B := 9_999.0;
           A := T0_D4 (B);
           Put_Line ("B =     " &
                     B'Image);
           Put_Line ("A = B = " &
                     A'Image);

        end Show_Mixing_Decimal_Types;

    Here, we use :ada:`T2_D6 (A)` and :ada:`T0_D4 (B)` to convert to :ada:`T2_D6`
    and :ada:`T0_D4`, respectively.

    Note that, if we had assigned :ada:`9_999.99` (or :ada:`T2_D6`) to :ada:`B` in
    the code above, the :ada:`T0_D4 (B)` would raise a :ada:`Constraint_Error`
    exception due the small difference in the range that we mentioned previously.



Integer multiplication and division
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



Decimal vs. floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we present two simplified, yet practical examples that benefit
from using decimal fixed-point types instead of floating-point types.

Prices after tax
^^^^^^^^^^^^^^^^

Let's look at a simplified example of an application that calculates the price
of products including sales tax. First, let's start with the definition of the
:ada:`Price` and :ada:`Rate` types that we're going to use in the application:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Price_After_Tax

    package Custom_Decimal_Types is

       type Price is
         delta 0.01 digits 16;

       type Price_Array is
         array (Positive range <>) of
           Price;

       type Rate is
         delta 0.0001 digits 18;

    end Custom_Decimal_Types;

This is the simple test application that calculates the gross price (i.e. after
tax) for items whose net price is stored in an array (see :ada:`Prices` in
the code):

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Price_After_Tax

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Price_After_Tax is

       Prices        : Price_Array :=
                         (8.40, 5.03, 1.67);
       P_After_Tax : Price;
       Tax_Rate      : Rate;

       procedure Show_Prices (Before,
                              After : Price) is
       begin
          Put_Line (Before'Image
                    & " => "
                    & After'Image);
       end Show_Prices;
    begin
       Tax_Rate := 1.19;

       Put_Line ("Price BEFORE => AFTER Tax");
       for P of Prices loop
          P_After_Tax := P * Tax_Rate;
          Show_Prices (P, P_After_Tax);
       end loop;

    end Price_After_Tax;

In this example, we apply a tax rate of 19% to the original net prices, so that
we get the following gross prices:

+--------------+---------+--------------+
| Price before | Tax (%) | Price after  |
| tax          |         | tax          |
+==============+=========+==============+
|         8.40 |    19.0 |         9.99 |
+--------------+---------+--------------+
|         5.03 |    19.0 |         5.98 |
+--------------+---------+--------------+
|         1.67 |    19.0 |         1.98 |
+--------------+---------+--------------+

Now, let's replace the definition of the :ada:`Price` and :ada:`Rate` types
with floating-point types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Price_After_Tax_Float

    package Custom_Float_Types is

       type Price is
         digits 16;

       type Price_Array is
         array (Positive range <>) of
           Price;

       type Rate is
         digits 18;

    end Custom_Float_Types;

We can reuse the previous code with small adaptations:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Price_After_Tax_Float

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Float_Types;
    use  Custom_Float_Types;

    procedure Price_After_Tax is

       Prices        : Price_Array :=
                         (8.40, 5.03, 1.67);
       P_After_Tax : Price;
       Tax_Rate      : Rate;

       procedure Show_Prices (Before,
                              After : Price) is
       begin
          Put_Line (Before'Image
                    & " => "
                    & After'Image);
       end Show_Prices;
    begin
       Tax_Rate := 1.19;

       Put_Line ("Price BEFORE => AFTER Tax");
       for P of Prices loop
          P_After_Tax :=
            Price (Rate (P) * Tax_Rate);
          Show_Prices (P, P_After_Tax);
       end loop;
    end Price_After_Tax;

In this example, we again apply a tax rate of 19% to the net prices to get the
following net prices |mdash| this time, however, using floating-point types.
This is the result:

+--------------+---------+--------------+
| Price before | Tax (%) | Price after  |
| tax          |         | tax          |
+==============+=========+==============+
|         8.40 |    19.0 |       9.996  |
+--------------+---------+--------------+
|         5.03 |    19.0 |       5.9857 |
+--------------+---------+--------------+
|         1.67 |    19.0 |       1.9873 |
+--------------+---------+--------------+

As we can see, some of the prices that we get have four digits after the dot,
which cannot be used for the total price |mdash| as we typically don't use
values smaller than one cent in prices. We could, of course, apply rounding
after these operations and calculate the value with two digits after the dot.
However, this would require additional operations for each price we're
calculating, thereby delivering worse performance than the previous example
with decimal fixed-point types.


Total price calculation
^^^^^^^^^^^^^^^^^^^^^^^

Let's now focus on a second simplified example. This time, we look at an
application that calculates the total price (e.g. of an invoice) when buying
multiple products.

Again, let's start with the definition of the decimal data types that we're
going to use in the application:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price

    package Custom_Decimal_Types is

       type Price is
         delta 0.01 digits 16;

       type Price_Array is
         array (Positive range <>) of
           Price;

       type Price_Accum is
          delta 0.0001 digits 18;

       type Rate is
         delta 0.0001 digits 18;

    end Custom_Decimal_Types;

There are basically two methods for the calculation of the total price. We can
either use the net price of each item and apply the sales tax rate once we have
the subtotal, or we can use the gross price |mdash| which already includes
sales tax  |mdash| of each item to calculate the total price.

The test application calculates the total price of each item considering the
prices stored in the :ada:`Prices` array, the quantities stored in the
:ada:`Quantities` array, and a sales tax rate of 19.0%.

In the first version of the test application, we use the net price of each item
to calculate the subtotal, and apply the sales tax to that value:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Total_Price is

       type Quantities_Array is
         array (Positive range <>) of
           Natural;

       Prices        : constant Price_Array :=
                         (8.40, 5.04, 1.68);
       Quantities    : constant Quantities_Array :=
                         (1, 8, 9);
       Total_Item    : Price_Accum;
       Total_Sum     : Price_Accum;
       Tax_Rate      : constant Rate := 1.19;
    begin
       Total_Sum := 0.0;

       Put_Line ("Sum Per Item");
       Put_Line ("Item #    Price Quant     Total");
       for I in Prices'Range loop
          Total_Item := Price_Accum (Prices (I) *
                                     Quantities (I));
          Total_Sum  := Total_Sum + Total_Item;
          Put_Line ("    " & I'Image
                    & "    " & Prices (I)'Image
                    & "    " & Quantities (I)'Image
                    & "    "
                    & Price (Total_Item)'Image);
       end loop;

       Put_Line ("SUBTOTAL:                "
                 & Price (Total_Sum)'Image);
       Put_Line ("TAX RATE (%):            "
                 & Rate'Image (
                     (Tax_Rate - 1.0) * 100.0));

       Total_Sum := Total_Sum * Tax_Rate;
       Put_Line ("TOTAL WITH TAX:        "
                 & Price (Total_Sum)'Image);
    end Total_Price;

In this example, we calculate the total price for each item (:ada:`Total_Item`)
and accumulate it in :ada:`Total_Sum`. After the loop, we calculate the total
price by multiplying the subtotal stored in :ada:`Total_Sum` by the value of
:ada:`Tax_Rate`.

For the specific invoice calculated in this test application, we get a subtotal
|mdash| i.e. total price without sales tax |mdash| of 63.84 and a total price
(with sales tax) of 75.96.

In the second version of the test application, we use the gross price of each
item and, after calculating the total price, we derive the total net price
(without sales tax) from that:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Decimal_Types;
    use  Custom_Decimal_Types;

    procedure Total_Price is
       type Quantities_Array is
         array (Positive range <>) of
           Natural;

       Prices         : constant Price_Array :=
                          (8.40, 5.04, 1.68);
       Quantities     : constant Quantities_Array :=
                          (1, 8, 9);
       Adjusted_Price : Price_Accum;
       Total_Item     : Price_Accum;
       Total_Sum      : Price_Accum;
       Tax_Rate       : constant Rate := 1.19;
    begin
       Total_Sum := 0.0;

       Put_Line ("Sum Per Item");
       Put_Line ("Item #    Price Quant     Total");
       for I in Prices'Range loop
          Adjusted_Price := Price_Accum (Prices (I) *
                                         Tax_Rate);
          Total_Item := Adjusted_Price *
                        Quantities (I);
          Total_Sum  := Total_Sum + Total_Item;
          Put_Line ("    " & I'Image
                    & "    "
                    & Price (Adjusted_Price)'Image
                    & "    " & Quantities (I)'Image
                    & "    "
                    & Price (Total_Item)'Image);
       end loop;

       Put_Line ("TOTAL WITH TAX:        "
                 & Price (Total_Sum)'Image);
       Put_Line ("TAX RATE (%):            "
                 & Rate'Image (
                     (Tax_Rate - 1.0) * 100.0));

       Total_Sum := Total_Sum / Tax_Rate;
       Put_Line ("VALUE BEFORE TAX       "
                 & Price (Total_Sum)'Image);
    end Total_Price;

In this example, we calculate the gross price of each item
(:ada:`Adjusted_Price`), and then the total price of each item
(:ada:`Total_Item`), which we accumulate in :ada:`Total_Sum`. After the loop,
we calculate the net price by dividing the subtotal stored in :ada:`Total_Sum`
by the value of :ada:`Tax_Rate`.

For the specific invoice calculated in this test application, we get a total
price of 75.96 and a net price of 63.84. (This information matches the prices
we calculated in the previous version of the test application.)

Now, let's replace the definition of the :ada:`Price`, :ada:`Price_Accum` and
:ada:`Rate` types with floating-point types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price_Float

    package Custom_Float_Types  is

       type Price is
         digits 16;

       type Price_Array is
         array (Positive range <>) of
           Price;

       type Price_Accum is
         digits 18;

       type Rate is
         digits 18;

    end Custom_Float_Types;

This is the first version of the test application after a couple of small
adaptations:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price_Float

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Float_Types;
    use  Custom_Float_Types;

    procedure Total_Price is

       type Quantities_Array is
         array (Positive range <>) of
           Natural;

       Prices        : constant Price_Array :=
                         (8.40, 5.04, 1.68);
       Quantities    : constant Quantities_Array :=
                         (1, 8, 9);
       Total_Item    : Price_Accum;
       Total_Sum     : Price_Accum;
       Tax_Rate      : constant Rate := 1.19;
    begin
       Total_Sum := 0.0;

       Put_Line ("Sum Per Item");
       Put_Line ("Item #    Price                  "
                 & "Quant     Total");
       for I in Prices'Range loop
          Total_Item := Price_Accum (Prices (I)) *
                        Price_Accum (Quantities (I));
          Total_Sum  := Total_Sum + Total_Item;
          Put_Line ("    " & I'Image
                    & "    " & Prices (I)'Image
                    & "    " & Quantities (I)'Image
                    & "    "
                    & Price (Total_Item)'Image);
       end loop;

       Put_Line ("SUBTOTAL:                "
                 & Price (Total_Sum)'Image);
       Put_Line ("TAX RATE (%):            "
                 & Rate'Image (
                     (Tax_Rate - 1.0) * 100.0));

       Total_Sum := Total_Sum *
                    Price_Accum (Tax_Rate);
       Put_Line ("TOTAL WITH TAX:        "
                 & Price (Total_Sum)'Image);
    end Total_Price;

In this case, the subtotal is 63.84 and the total price is 75.9696. As we can
see, the total price has four digits after the dot. If we applied rounding to
those extra digits, we would get a total price of 75.97 |mdash| instead of the
value of 75.96 that we calculated using decimal fixed-point types.

Let's adapt the second version of the test application to floating-point
types, too:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Decimal_Fixed_Point_Types.Total_Price_Float

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Float_Types;
    use  Custom_Float_Types;

    procedure Total_Price is
       type Quantities_Array is
         array (Positive range <>) of
           Natural;

       Prices         : constant Price_Array :=
                          (8.40, 5.04, 1.68);
       Quantities     : constant Quantities_Array :=
                          (1, 8, 9);
       Adjusted_Price : Price_Accum;
       Total_Item     : Price_Accum;
       Total_Sum      : Price_Accum;
       Tax_Rate       : constant Rate := 1.19;
    begin
       Total_Sum := 0.0;

       Put_Line ("Sum Per Item");
       Put_Line ("Item #    Price Quant     Total");
       for I in Prices'Range loop
          Adjusted_Price := Price_Accum (Prices (I)) *
                            Price_Accum (Tax_Rate);
          Total_Item := Adjusted_Price *
                        Price_Accum (Quantities (I));
          Total_Sum  := Total_Sum + Total_Item;
          Put_Line ("    " & I'Image
                    & "    "
                    & Price (Adjusted_Price)'Image
                    & "    " & Quantities (I)'Image
                    & "    "
                    & Price (Total_Item)'Image);
       end loop;

       Put_Line ("TOTAL WITH TAX:        "
                 & Price (Total_Sum)'Image);
       Put_Line ("TAX RATE (%):            "
                 & Rate'Image (
                     (Tax_Rate - 1.0) * 100.0));

       Total_Sum := Total_Sum /
                    Price_Accum (Tax_Rate);
       Put_Line ("VALUE BEFORE TAX       "
                 & Price (Total_Sum)'Image);
    end Total_Price;

In this case, the total price is 75.9696 and the price without sales tax is
63.84. Again, if we round the total price to get two digits after the dot, we
get 75.97 instead 75.96.

A 0.01 error might be consider small, but the accumulation of such errors in
a complex financial application can be significant and, therefore, it might be
considered undesirable. As we've seen in this example, we can use decimal
fixed-point types to avoid such unwanted side effects.

.. ::

    .. _Intro_Ada_Ordinary_Fixed_Point_Types:

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

    .. code:: ada run_button project=Courses.Advanced_Ada.Fixed_Point_Types.Normalized_Fixed_Point_Type

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

    .. code:: ada compile_button project=Courses.Advanced_Ada.Fixed_Point_Types.Normalized_Adapted_Fixed_Point_Type

        procedure Normalized_Adapted_Fixed_Point_Type is
           type TQ31 is
             delta 2.0 ** (-31)
             range -1.0 .. 1.0 - 2.0 ** (-31);
        begin
           null;
        end Normalized_Adapted_Fixed_Point_Type;

    We may also use any other range. For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Fixed_Point_Types.Custom_Fixed_Point_Range

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

    .. code:: ada run_button project=Courses.Advanced_Ada.Fixed_Point_Types.Fixed_Point_Op

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
