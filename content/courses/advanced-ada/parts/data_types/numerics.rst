Numerics
========

.. include:: ../../../global.txt

Modular Types
-------------

In the Introduction to Ada course, we've seen that Ada has two kinds of integer
type: :ref:`signed <Intro_Ada_Integers>` and
:ref:`modular <Intro_Ada_Unsigned_Types>` types. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_1

    package Num_Types is

       type Signed_Integer is range 1 .. 1_000_000;
       type Modular is mod 2**32;

    end Num_Types;

In this section, we discuss two attributes of modular types: :ada:`Modulus`
and :ada:`Mod`. We also discuss operations on modular types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.4 Integer Types <3-5-4>`


:ada:`Modulus` Attribute
~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Modulus` attribute returns the modulus of the modular type as a
universal integer value. Let's get the modulus of the 32-bit :ada:`Modular`
type that we've declared in the :ada:`Num_Types` package of the previous
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_1

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       Modulus_Value : constant := Modular'Modulus;
    begin
       Put_Line (Modulus_Value'Image);
    end Show_Modular;

When we run this example, we get 4294967296, which is equal to :ada:`2**32`.

:ada:`Mod` Attribute
~~~~~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #26: The Mod Attribute <https://www.adacore.com/gems/gem-26>`_.

Operations on signed integers can overflow: if the result is outside the base
range, :ada:`Constraint_Error` will be raised. In our previous example, we
declared the :ada:`Signed_Integer` type:

.. code-block:: ada

    type Signed_Integer is range 1 .. 1_000_000;

The base range of :ada:`Signed_Integer` is the range of
:ada:`Signed_Integer'Base`, which is chosen by the compiler, but is likely to
be something like :ada:`-2**31 .. 2**31 - 1`. (Note: we discussed the
:ada:`Base` attribute :ref:`in this section <Adv_Ada_Base_Attribute>`.)

Operations on modular integers use modular (wraparound) arithmetic. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_1

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       X : Modular;
    begin
       X := 1;
       Put_Line (X'Image);

       X := -X;
       Put_Line (X'Image);
    end Show_Modular;

Negating X gives -1, which wraps around to :ada:`2**32 - 1`, i.e.
all-one-bits.

But what about a type conversion from signed to modular? Is that a signed
operation (so it should overflow) or is it a modular operation (so it should
wrap around)? The answer in Ada is the former |mdash| that is, if you try to
convert, say, :ada:`Integer'(-1)` to :ada:`Modular`, you will get
:ada:`Constraint_Error`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_1
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       I : Integer := -1;
       X : Modular := 1;
    begin
       X := Modular (I);  --  raises Constraint_Error
       Put_Line (X'Image);
    end Show_Modular;

To solve this problem, we can use the :ada:`Mod` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Modular_1

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       I : constant Integer := -1;
       X : Modular := 1;
    begin
       X := Modular'Mod (I);
       Put_Line (X'Image);
    end Show_Modular;

The :ada:`Mod` attribute will correctly convert from any integer type to a
given modular type, using wraparound semantics.

.. admonition:: Historically

    In older versions of Ada |mdash| such as Ada 95 |mdash|, the only way to do
    this conversion is to use :ada:`Unchecked_Conversion`, which is somewhat
    uncomfortable. Furthermore, if you're trying to convert to a generic formal
    modular type, how do you know what size of signed integer type to use? Note
    that :ada:`Unchecked_Conversion` might malfunction if the source and target
    types are of different sizes.

    The :ada:`Mod` attribute was added to Ada 2005 to solve this problem.
    Also, we can now safely use this attribute in generics. For example:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Mod_Attribute

        generic
           type Formal_Modular is mod <>;
        package Mod_Attribute is
           function F return Formal_Modular;
        end Mod_Attribute;

        package body Mod_Attribute is

           A_Signed_Integer : Integer := -1;

           function F return Formal_Modular is
           begin
              return Formal_Modular'Mod
                       (A_Signed_Integer);
           end F;

        end Mod_Attribute;

    In this example, :ada:`F` will return the all-ones bit pattern, for
    whatever modular type is passed to :ada:`Formal_Modular`.

Operations on modular types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Modular types are particularly useful for bit manipulation. For example, we
can use the :ada:`and`, :ada:`or`, :ada:`xor` and :ada:`not` operators for
modular types.

Also, we can perform bit-shifting by multiplying or dividing a modular object
with a power of two. For example, if :ada:`M` is a variable of modular type,
then :ada:`M := M * 2 ** 3;` shifts the bits to the left by three bits.
Likewise, :ada:`M := M / 2 ** 3` shifts the bits to the right. Note that the
compiler selects the appropriate shifting operator when translating these
operations to machine code |mdash| no actual multiplication or division will be
performed.

Let's see a simple implementation of the CRC-CCITT (0x1D0F) algorithm:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Modular_Types.Mod_Crc_CCITT_Ada

    package Crc_Defs is

        type Byte is mod 2 ** 8;
        type Crc  is mod 2 ** 16;

        type Byte_Array is
          array (Positive range <>) of Byte;

        function Crc_CCITT (A : Byte_Array)
                            return Crc;

        procedure Display (Crc_A : Crc);

        procedure Display (A : Byte_Array);

    end Crc_Defs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Crc_Defs is

        package Byte_IO is new Modular_IO (Byte);
        package Crc_IO  is new Modular_IO (Crc);

        function Crc_CCITT (A : Byte_Array)
                            return Crc
        is
           X     : Byte;
           Crc_A : Crc := 16#1d0f#;
        begin
           for I in A'Range loop
              X := Byte (Crc_A / 2 ** 8) xor A (I);
              X := X xor (X / 2 ** 4);
              declare
                 Crc_X : constant Crc := Crc (X);
              begin
                 Crc_A := Crc_A * 2 ** 8  xor
                          Crc_X * 2 ** 12 xor
                          Crc_X * 2 ** 5  xor
                          Crc_X;
              end;
           end loop;

           return Crc_A;
        end Crc_CCITT;

        procedure Display (Crc_A : Crc) is
        begin
           Crc_IO.Put (Crc_A);
           New_Line;
        end Display;

        procedure Display (A : Byte_Array) is
        begin
           for E of A loop
              Byte_IO.Put (E);
              Put (", ");
           end loop;
           New_Line;
        end Display;

    begin
       Byte_IO.Default_Width := 1;
       Byte_IO.Default_Base  := 16;
       Crc_IO.Default_Width  := 1;
       Crc_IO.Default_Base   := 16;
    end Crc_Defs;

    with Ada.Text_IO; use Ada.Text_IO;
    with Crc_Defs;    use Crc_Defs;

    procedure Show_Crc is
       AA    : constant Byte_Array :=
                 (16#0#, 16#20#, 16#30#);
       Crc_A : Crc;
    begin
       Crc_A := Crc_CCITT (AA);

       Put ("Input array: ");
       Display (AA);

       Put ("CRC-CCITT: ");
       Display (Crc_A);
    end Show_Crc;

In this example, the core of the algorithm is implemented in the
:ada:`Crc_CCITT` function. There, we use bit shifting |mdash| for instance,
:ada:`* 2 ** 8` and :ada:`/ 2 ** 8`, which shift left and right, respectively,
by eight bits. We also use the :ada:`xor` operator.


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


Floating-Point Types
--------------------

In this section, we discuss various attributes related to floating-point types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.8 Operations of Floating Point Types <3-5-8>`
    - :arm22:`A.5.3 Attributes of Floating Point Types <A-5-3>`

Representation-oriented attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we discuss attributes related to the representation of
floating-point types.

Attribute: :ada:`Machine_Radix`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Machine_Radix` is an attribute that returns the radix of the hardware
representation of a type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Machine_Radix

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Machine_Radix is
    begin
       Put_Line
         ("Float'Machine_Radix:           "
          & Float'Machine_Radix'Image);
       Put_Line
         ("Long_Float'Machine_Radix:      "
          & Long_Float'Machine_Radix'Image);
       Put_Line
         ("Long_Long_Float'Machine_Radix: "
          & Long_Long_Float'Machine_Radix'Image);
    end Show_Machine_Radix;

Usually, this value is two, as the radix is based on a binary system.


Attributes: :ada:`Machine_Mantissa`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Machine_Mantissa` is an attribute that returns the number of bits
reserved for the mantissa of the floating-point type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Machine_Mantissa

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Machine_Mantissa is
    begin
       Put_Line
         ("Float'Machine_Mantissa:           "
          & Float'Machine_Mantissa'Image);
       Put_Line
         ("Long_Float'Machine_Mantissa:      "
          & Long_Float'Machine_Mantissa'Image);
       Put_Line
         ("Long_Long_Float'Machine_Mantissa: "
          & Long_Long_Float'Machine_Mantissa'Image);
    end Show_Machine_Mantissa;

On a typical desktop PC, as indicated by :ada:`Machine_Mantissa`, we have 24
bits for the floating-point mantissa of the :ada:`Float` type.

:ada:`Machine_Emin` and :ada:`Machine_Emax`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Machine_Emin` and :ada:`Machine_Emax` attributes return the minimum
and maximum value, respectively, of the machine exponent the floating-point
type. Note that, in all cases, the returned value is a universal integer. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Machine_Emin_Emax

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Machine_Emin_Emax is
    begin
       Put_Line
         ("Float'Machine_Emin:               "
          & Float'Machine_Emin'Image);
       Put_Line
         ("Float'Machine_Emax:               "
          & Float'Machine_Emax'Image);
       Put_Line
         ("Long_Float'Machine_Emin:          "
          & Long_Float'Machine_Emin'Image);
       Put_Line
         ("Long_Float'Machine_Emax:          "
          & Long_Float'Machine_Emax'Image);
       Put_Line
         ("Long_Long_Float'Machine_Emin:     "
          & Long_Long_Float'Machine_Emin'Image);
       Put_Line
         ("Long_Long_Float'Machine_Emax:     "
          & Long_Long_Float'Machine_Emax'Image);
    end Show_Machine_Emin_Emax;

On a typical desktop PC, the value of :ada:`Float'Machine_Emin` and
:ada:`Float'Machine_Emax` is -125 and 128, respectively.

To get the actual minimum and maximum value of the exponent for a specific
type, we need to use the :ada:`Machine_Radix` attribute that we've seen
previously. Let's calculate the minimum and maximum value of the exponent for
the :ada:`Float` type on a typical PC:

- Value of minimum exponent: :ada:`Float'Machine_Radix ** Float'Machine_Emin`.

    - In our target platform, this is
      2\ :sup:`-125` = 2.35098870164457501594 x 10\ :sup:`-38`.

- Value of maximum exponent: :ada:`Float'Machine_Radix ** Float'Machine_Emax`.

    - In our target platform, this is
      2\ :sup:`128`  = 3.40282366920938463463 x 10\ :sup:`38`.

Attribute: :ada:`Digits`
^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Digits` is an attribute that returns the requested decimal precision of
a floating-point subtype. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Digits

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Digits is
    begin
       Put_Line ("Float'Digits:           "
                 & Float'Digits'Image);
       Put_Line ("Long_Float'Digits:      "
                 & Long_Float'Digits'Image);
       Put_Line ("Long_Long_Float'Digits: "
                 & Long_Long_Float'Digits'Image);
    end Show_Digits;

Here, the requested decimal precision of the :ada:`Float` type is six digits.

Note that we said that :ada:`Digits` is the *requested* level of precision,
which is specified as part of declaring a floating point type. We can retrieve
the actual decimal precision with :ada:`Base'Digits`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Base_Digits

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Base_Digits is
       type Float_D3 is new Float digits 3;
    begin
       Put_Line ("Float_D3'Digits:           "
                 & Float_D3'Digits'Image);
       Put_Line ("Float_D3'Base'Digits:      "
                 & Float_D3'Base'Digits'Image);
    end Show_Base_Digits;

The requested decimal precision of the :ada:`Float_D3` type is three digits,
while the actual decimal precision is six digits (on a typical desktop PC).


Attributes: :ada:`Denorm`, :ada:`Signed_Zeros`, :ada:`Machine_Rounds`, :ada:`Machine_Overflows`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we discuss attributes that return :ada:`Boolean` values
indicating whether a feature is available or not in the target architecture:

- :ada:`Denorm` is an attribute that indicates whether the target architecture
  uses :wikipedia:`denormalized numbers <Subnormal_number>`.

- :ada:`Signed_Zeros` is an attribute that indicates whether the type uses a
  sign for zero values, so it can represent both -0.0 and 0.0.

- :ada:`Machine_Rounds` is an attribute that indicates whether
  rounding-to-nearest is used, rather than some other choice (such as
  rounding-toward-zero).

- :ada:`Machine_Overflows` is an attribute that indicates whether a
  :ada:`Constraint_Error` exception is (or is not) guaranteed to be raised
  when an operation with that type produces an overflow or divide-by-zero.

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Machine_Rounds_Overflows

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Boolean_Attributes is
    begin
       Put_Line
         ("Float'Denorm:           "
          & Float'Denorm'Image);
       Put_Line
         ("Long_Float'Denorm:      "
          & Long_Float'Denorm'Image);
       Put_Line
         ("Long_Long_Float'Denorm: "
          & Long_Long_Float'Denorm'Image);
       Put_Line
         ("Float'Signed_Zeros:           "
          & Float'Signed_Zeros'Image);
       Put_Line
         ("Long_Float'Signed_Zeros:      "
          & Long_Float'Signed_Zeros'Image);
       Put_Line
         ("Long_Long_Float'Signed_Zeros: "
          & Long_Long_Float'Signed_Zeros'Image);
       Put_Line
         ("Float'Machine_Rounds:           "
          & Float'Machine_Rounds'Image);
       Put_Line
         ("Long_Float'Machine_Rounds:      "
          & Long_Float'Machine_Rounds'Image);
       Put_Line
         ("Long_Long_Float'Machine_Rounds: "
          & Long_Long_Float'Machine_Rounds'Image);
       Put_Line
         ("Float'Machine_Overflows:           "
          & Float'Machine_Overflows'Image);
       Put_Line
         ("Long_Float'Machine_Overflows:      "
          & Long_Float'Machine_Overflows'Image);
       Put_Line
         ("Long_Long_Float'Machine_Overflows: "
          & Long_Long_Float'Machine_Overflows'Image);
    end Show_Boolean_Attributes;

On a typical PC, we have the following information:

- :ada:`Denorm` is true (i.e. the architecture uses denormalized numbers);

- :ada:`Signed_Zeros` is true (i.e. the standard floating-point types use a
  sign for zero values);

- :ada:`Machine_Rounds` is true (i.e. rounding-to-nearest is used for
  floating-point types);

- :ada:`Machine_Overflows` is false (i.e. there's no guarantee that a
  :ada:`Constraint_Error` exception is raised when an operation with a
  floating-point type produces an overflow or divide-by-zero).

Primitive function attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we discuss attributes that we can use to manipulate
floating-point values.

Attributes: :ada:`Fraction`, :ada:`Exponent` and :ada:`Compose`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Exponent` and :ada:`Fraction` attributes return "parts" of a
floating-point value:

- :ada:`Exponent` returns the machine exponent, and

- :ada:`Fraction` returns the mantissa part.

:ada:`Compose` is used to return a floating-point value based on a fraction
(the mantissa part) and the machine exponent.

Let's see some examples:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Exponent_Fraction

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Exponent_Fraction_Compose is
    begin
       Put_Line
         ("Float'Fraction (1.0):     "
          & Float'Fraction (1.0)'Image);
       Put_Line
         ("Float'Fraction (0.25):    "
          & Float'Fraction (0.25)'Image);
       Put_Line
         ("Float'Fraction (1.0e-25): "
          & Float'Fraction (1.0e-25)'Image);
       Put_Line
         ("Float'Exponent (1.0):     "
          & Float'Exponent (1.0)'Image);
       Put_Line
         ("Float'Exponent (0.25):    "
          & Float'Exponent (0.25)'Image);
       Put_Line
         ("Float'Exponent (1.0e-25): "
          & Float'Exponent (1.0e-25)'Image);
       Put_Line
         ("Float'Compose (5.00000e-01, 1):   "
          & Float'Compose (5.00000e-01, 1)'Image);
       Put_Line
         ("Float'Compose (5.00000e-01, -1):  "
          & Float'Compose (5.00000e-01, -1)'Image);
       Put_Line
         ("Float'Compose (9.67141E-01, -83): "
          & Float'Compose (9.67141E-01, -83)'Image);
    end Show_Exponent_Fraction_Compose;

To understand this code example, we have to take this formula into account:

   Value = Fraction x Machine_Radix\ :sup:`Exponent`

Considering that the value of :ada:`Float'Machine_Radix` on a typical PC is
two, we see that the value 1.0 is composed by a fraction of 0.5 and a machine
exponent of one. In other words:

   0.5 x 2\ :sup:`1` = 1.0

For the value 0.25, we get a fraction of 0.5 and a machine exponent of -1,
which is the result of 0.5 x 2\ :sup:`-1` = 0.25.
We can use the :ada:`Compose` attribute to perform this calculation. For
example, :ada:`Float'Compose (0.5, -1) = 0.25`.

Note that :ada:`Fraction` is always between 0.5 and 0.999999 (i.e < 1.0),
except for denormalized numbers, where it can be < 0.5.

Attribute: :ada:`Scaling`
^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Scaling` is an attribute that scales a floating-point value based on the
machine radix and a machine exponent passed to the function. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Scaling

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Scaling is
    begin
       Put_Line ("Float'Scaling (0.25, 1): "
                 & Float'Scaling (0.25, 1)'Image);
       Put_Line ("Float'Scaling (0.25, 2): "
                 & Float'Scaling (0.25, 2)'Image);
       Put_Line ("Float'Scaling (0.25, 3): "
                 & Float'Scaling (0.25, 3)'Image);
    end Show_Scaling;

The scaling is calculated with this formula:

   scaling = value x Machine_Radix\ :sup:`machine exponent`

For example, on a typical PC with a machine radix of two,
:ada:`Float'Scaling (0.25, 3) = 2.0` corresponds to

   0.25 x 2\ :sup:`3` = 2.0

Round-up and round-down attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Floor` and :ada:`Ceiling` are attributes that returned the rounded-down
or rounded-up value, respectively, of a floating-point value. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Floor_Ceiling

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Floor_Ceiling is
    begin
       Put_Line ("Float'Floor (0.25):   "
                 & Float'Floor (0.25)'Image);
       Put_Line ("Float'Ceiling (0.25): "
                 & Float'Ceiling (0.25)'Image);
    end Show_Floor_Ceiling;

As we can see in this example, the rounded-down value (floor) of 0.25 is 0.0,
while the rounded-up value (ceiling) of 0.25 is 1.0.

Round-to-nearest attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we discuss three attributes used for rounding:
:ada:`Rounding`, :ada:`Unbiased_Rounding`, :ada:`Machine_Rounding`
In all cases, the rounding attributes return the nearest integer value (as a
floating-point value). For example, the rounded value for 4.8 is 5.0 because 5
is the closest integer value.

Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Rounding

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Roundings is
    begin
       Put_Line
         ("Float'Rounding (0.5):  "
          & Float'Rounding (0.5)'Image);
       Put_Line
         ("Float'Rounding (1.5):  "
          & Float'Rounding (1.5)'Image);
       Put_Line
         ("Float'Rounding (4.5):  "
          & Float'Rounding (4.5)'Image);
       Put_Line
         ("Float'Rounding (-4.5): "
          & Float'Rounding (-4.5)'Image);
       Put_Line
         ("Float'Unbiased_Rounding (0.5): "
          & Float'Unbiased_Rounding (0.5)'Image);
       Put_Line
         ("Float'Unbiased_Rounding (1.5): "
          & Float'Unbiased_Rounding (1.5)'Image);
       Put_Line
         ("Float'Machine_Rounding (0.5): "
          & Float'Machine_Rounding (0.5)'Image);
       Put_Line
         ("Float'Machine_Rounding (1.5): "
          & Float'Machine_Rounding (1.5)'Image);
    end Show_Roundings;

The difference between these attributes is the way they handle the case when a
value is exactly in between two integer values. For example, 4.5 could be
rounded up to 5.0 or rounded down to 4.0. This is the way each rounding
attribute works in this case:

- :ada:`Rounding` rounds away from zero. Positive floating-point values are
  rounded up, while negative floating-point values are rounded down when the
  value is between two integer values. For example:

  - 4.5 is rounded-up to 5.0, i.e.
    :ada:`Float'Rounding (4.5) = Float'Ceiling (4.5) = 5.0`.

  - -4.5 is rounded-down to -5.0, i.e.
    :ada:`Float'Rounding (-4.5) = Float'Floor (-4.5) = -5.0`.

- :ada:`Unbiased_Rounding` rounds toward the even integer. For example,

  - :ada:`Float'Unbiased_Rounding (0.5) = 0.0` because zero is the closest even
    integer, while

  - :ada:`Float'Unbiased_Rounding (1.5) = 2.0` because two is the closest even
    integer.

- :ada:`Machine_Rounding` uses the most appropriate rounding instruction
  available on the target platform. While this rounding attribute can
  potentially have the best performance, its result may be non-portable. For
  example, whether the rounding of 4.5 becomes 4.0 or 5.0 depends on the target
  platform.

  - If an algorithm depends on a specific rounding behavior, it's best to avoid
    the :ada:`Machine_Rounding` attribute. On the other hand, if the rounding
    behavior won't have a significant impact on the results, we can safely use
    this attribute.

Attributes: :ada:`Truncation`, :ada:`Remainder`, :ada:`Adjacent`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Truncation` attribute returns the *truncated* value of a
floating-point value, i.e. the value corresponding to the integer part of a
number rounded toward zero. This corresponds to the number before the radix
point. For example, the truncation of 1.55 is 1.0 because the integer part of
1.55 is 1.

The :ada:`Remainder` attribute returns the remainder part of a division. For
example, :ada:`Float'Remainder (1.25, 0.5) = 0.25`. Let's briefly discuss the
details of this operations. The result of the division 1.25 / 0.5 is 2.5. Here,
1.25 is the dividend and 0.5 is the divisor. The quotient and remainder of this
division are 2 and 0.25, respectively. (Here, the quotient is an integer number,
and the remainder is the floating-point part that remains.)

Note that the relation between quotient and remainder is defined in such a way
that we get the original dividend back when we use the formula: "quotient x
divisor + remainder = dividend". For the previous example, this means
2 x 0.5 + 0.25 = 1.25.

The :ada:`Adjacent` attribute is the next machine value towards another value.
For example, on a typical PC, the adjacent value of a small value |mdash|
say, 1.0 x 10\ :sup:`-83` |mdash| towards zero is +0.0, while the adjacent
value of this small value towards 1.0 is another small, but greater value
|mdash| in fact, it's 1.40130 x 10\ :sup:`-45`. Note that the first parameter
of the :ada:`Adjacent` attribute is the value we want to analyze and the
second parameter is the :ada:`Towards` value.

Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Truncation_Remainder

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Truncation_Remainder_Adjacent is
    begin
       Put_Line
         ("Float'Truncation (1.55):  "
          & Float'Truncation (1.55)'Image);
       Put_Line
         ("Float'Truncation (-1.55): "
          & Float'Truncation (-1.55)'Image);
       Put_Line
         ("Float'Remainder (1.25, 0.25): "
          & Float'Remainder (1.25, 0.25)'Image);
       Put_Line
         ("Float'Remainder (1.25, 0.5):  "
          & Float'Remainder (1.25, 0.5)'Image);
       Put_Line
         ("Float'Remainder (1.25, 1.0):  "
          & Float'Remainder (1.25, 1.0)'Image);
       Put_Line
         ("Float'Remainder (1.25, 2.0):  "
          & Float'Remainder (1.25, 2.0)'Image);
       Put_Line
         ("Float'Adjacent (1.0e-83, 0.0): "
          & Float'Adjacent (1.0e-83, 0.0)'Image);
       Put_Line
         ("Float'Adjacent (1.0e-83, 1.0): "
          & Float'Adjacent (1.0e-83, 1.0)'Image);
    end Show_Truncation_Remainder_Adjacent;


Attributes: :ada:`Copy_Sign` and :ada:`Leading_Part`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Copy_Sign` is an attribute that returns a value where the sign of the
second floating-point argument is multiplied by the magnitude of the first
floating-point argument. For example, :ada:`Float'Copy_Sign (1.0, -10.0)` is
-1.0. Here, the sign of the second argument (-10.0) is multiplied by the
magnitude of the first argument (1.0), so the result is -1.0.

:ada:`Leading_Part` is an attribute that returns the *approximated* version of
the mantissa of a value based on the specified number of leading bits for the
mantissa. Let's see some examples:

- :ada:`Float'Leading_Part (3.1416, 1)` is 2.0 because that's the value we can
  represent with one leading bit.

  - Note that :ada:`Float'Fraction (2.0) = 0.5` |mdash| which can be
    represented with one leading bit in the mantissa |mdash| and
    :ada:`Float'Exponent (2.0) = 2`.)

- If we increase the number of leading bits of the mantissa to two |mdash| by
  writing :ada:`Float'Leading_Part (3.1416, 2)` |mdash|, we get 3.0 because
  that's the value we can represent with two leading bits.

- If we increase again the number of leading bits to five |mdash|
  :ada:`Float'Leading_Part (3.1416, 5)` |mdash|, we get 3.125.

  - Note that, in this case :ada:`Float'Fraction (3.125) = 0.78125`
    and :ada:`Float'Exponent (3.125) = 2`.

  - The binary mantissa is actually :ada:`2#110_0100_0000_0000_0000_0000#`,
    which can be represented with five leading bits as expected:
    :ada:`2#110_01#`.

     - We can get the binary mantissa by calculating
       :ada:`Float'Fraction (3.125) * Float (Float'Machine_Radix) ** (Float'Machine_Mantissa - 1)`
       and converting the result to binary format. The -1 value in the formula
       corresponds to the sign bit.

.. admonition:: Attention

    In this explanation about the :ada:`Leading_Part` attribute, we're
    talking about leading bits. Strictly speaking, however, this is actually a
    simplification, and it's only correct if :ada:`Machine_Radix` is equal to
    two |mdash| which is the case for most machines. Therefore, in most cases,
    the explanation above is perfectly acceptable.

    However, if :ada:`Machine_Radix` is *not* equal to two, we cannot use the
    term "bits" anymore, but rather digits of the :ada:`Machine_Radix`.

Let's see some examples:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Sign_Leading

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Copy_Sign_Leading_Part_Machine is
    begin
       Put_Line
         ("Float'Copy_Sign (1.0, -10.0): "
          & Float'Copy_Sign (1.0, -10.0)'Image);
       Put_Line
         ("Float'Copy_Sign (-1.0, -10.0): "
          & Float'Copy_Sign (-1.0, -10.0)'Image);
       Put_Line
         ("Float'Copy_Sign (1.0,  10.0): "
          & Float'Copy_Sign (1.0,  10.0)'Image);
       Put_Line
         ("Float'Copy_Sign (1.0, -0.0):  "
          & Float'Copy_Sign (1.0, -0.0)'Image);
       Put_Line
         ("Float'Copy_Sign (1.0,  0.0):  "
          & Float'Copy_Sign (1.0,  0.0)'Image);
       Put_Line
         ("Float'Leading_Part (1.75, 1): "
          & Float'Leading_Part (1.75, 1)'Image);
       Put_Line
         ("Float'Leading_Part (1.75, 2): "
          & Float'Leading_Part (1.75, 2)'Image);
       Put_Line
         ("Float'Leading_Part (1.75, 3): "
          & Float'Leading_Part (1.75, 3)'Image);
    end Show_Copy_Sign_Leading_Part_Machine;


.. _Adv_Ada_Machine_Attribute:

Attribute: :ada:`Machine`
^^^^^^^^^^^^^^^^^^^^^^^^^^

Not every real number is directly representable as a floating-point value on a
specific machine. For example, let's take a value such as 1.0 x 10\ :sup:`15`
(or 1,000,000,000,000,000):

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Float_Value

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Float_Value is
       package F_IO is new
         Ada.Text_IO.Float_IO (Float);

       V : Float;
    begin
       F_IO.Default_Fore := 3;
       F_IO.Default_Aft  := 1;
       F_IO.Default_Exp  := 0;

       V := 1.0E+15;
       Put ("1.0E+15 = ");
       F_IO.Put (Item => V);
       New_Line;

    end Show_Float_Value;

If we run this example on a typical PC, we see that the expected value
:ada:`1_000_000_000_000_000.0` was displayed as :ada:`999_999_986_991_000.0`.
This is because 1.0 x 10\ :sup:`15` isn't
directly representable on this machine, so it has to be modified to a value that
is actually representable (on the machine).

This *automatic* modification we've just described is actually hidden, so to
say, in the assignment. However, we can make it more visible by using the
:ada:`Machine (X)` attribute, which returns a version of :ada:`X` that is
representable on the target machine. The :ada:`Machine (X)` attribute rounds
(or truncates) :ada:`X` to either one of the adjacent machine numbers for the
specific floating-point type of :ada:`X`. (Of course, if the real value of
:ada:`X` is directly representable on the target machine, no modification is
performed.)

In fact, we could rewrite the :ada:`V := 1.0E+15` assignment of the code example
as :ada:`V := Float'Machine (1.0E+15)`, as we're never assigning a real value
directly to a floating-pointing variable |mdash| instead, we're first
converting it to a version of the real value that is representable on the
target machine. In this case, 999999986991000.0 is a representable version of
the real value 1.0 x 10\ :sup:`15`. Of course, writing :ada:`V := 1.0E+15` or
:ada:`V := Float'Machine (1.0E+15)` doesn't make any difference to the actual
value that is assigned to :ada:`V` (in the case of this specific target
architecture), as the conversion to a representable value happens automatically
during the assignment to :ada:`V`.

There are, however, instances where using the :ada:`Machine` attribute does
make a difference in the result. For example, let's say we want to calculate
the difference between the original real value in our example
(1.0 x 10\ :sup:`15`) and the actual value that is assigned to :ada:`V`. We can
do this by using the :ada:`Machine` attribute in the calculation:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Machine_Attribute

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Machine_Attribute is
       package F_IO is new
         Ada.Text_IO.Float_IO (Float);

       V : Float;
    begin
       F_IO.Default_Fore := 3;
       F_IO.Default_Aft  := 1;
       F_IO.Default_Exp  := 0;

       Put_Line
         ("Original value: 1_000_000_000_000_000.0");

       V := 1.0E+15;
       Put ("Machine value:  ");
       F_IO.Put (Item => V);
       New_Line;

       V := 1.0E+15 - Float'Machine (1.0E+15);
       Put ("Difference:     ");
       F_IO.Put (Item => V);
       New_Line;

    end Show_Machine_Attribute;

When we run this example on a typical PC, we see that the difference is
roughly 1.3009 x 10\ :sup:`7`. (Actually, the value that we might see is
1.3008896 x 10\ :sup:`7`, which is a version of 1.3009 x 10\ :sup:`7` that is
representable on the target machine.)

When we write :ada:`1.0E+15 - Float'Machine (1.0E+15)`:

- the first value in the operation is the universal real value
  1.0 x 10\ :sup:`15`, while

- the second value in the operation is a version of the universal real value
  1.0 x 10\ :sup:`15` that is representable on the target machine.

This also means that, in the assignment to :ada:`V`, we're actually writing
:ada:`V := Float'Machine (1.0E+15 - Float'Machine (1.0E+15))`, so that:

1. we first get the intermediate real value that represents the difference
   between these values; and then

2. we get a version of this intermediate real value that is representable on the
   target machine.

This is the reason why we see 1.3008896 x 10\ :sup:`7` instead of
1.3009 x 10\ :sup:`7` when we run this application.

..
    TO BE IMPROVED:

    Model-oriented attributes
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    In this section, we discuss model-oriented attributes. Depending on the
    programming languages you're accustomed to, the notion of a "model" of
    arithmetic might sound unfamiliar. This is how the Ada Reference Manual
    defines it:

    | Associated with each floating point type is an infinite set of model numbers.
    | The model numbers of a type are used to define the accuracy requirements that
    | have to be satisfied by certain predefined operations of the type; through
    | certain attributes of the model numbers, they are also used to explain the
    | meaning of a user-declared floating point type declaration.

    .. admonition:: In the Ada Reference Manual

        - :arm22:`G.2.1 Model of Floating Point Arithmetic <G-2-1>`

    Attributes: :ada:`Model_Mantissa`, :ada:`Model_Emin`
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The :ada:`Model_Mantissa` attribute is similar to the :ada:`Machine_Mantissa`
    attribute, but it returns the number of bits for the mantissa based on the
    underlying numeric model for floating-point operations.

    .. admonition:: Attention

        We can only say that :ada:`Model_Mantissa` returns the "number of bits" of
        the mantissa if :ada:`Machine_Radix` is equal to two. As this is typically
        the case for most machines, this simplification is acceptable. However,
        if :ada:`Machine_Radix` is *not* equal to two, we're talking about "number
        of digits" in the :ada:`Machine_Radix`.

    The :ada:`Model_Emin` attribute is similar to the :ada:`Machine_Emin`
    attribute, but it returns the minimum machine exponent based on the underlying
    numeric model for floating-point operations.

    Let's see an example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Model_Mantissa

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Model_Mantissa_Emin is
        begin
           Put_Line
             ("Float'Model_Mantissa:           "
              & Float'Model_Mantissa'Image);
           Put_Line
             ("Long_Float'Model_Mantissa:      "
              & Long_Float'Model_Mantissa'Image);
           Put_Line
             ("Long_Long_Float'Model_Mantissa: "
              & Long_Long_Float'Model_Mantissa'Image);
           Put_Line
             ("Float'Model_Emin:               "
              & Float'Model_Emin'Image);
           Put_Line
             ("Long_Float'Model_Emin:          "
              & Long_Float'Model_Emin'Image);
           Put_Line
             ("Long_Long_Float'Model_Emin:     "
              & Long_Long_Float'Model_Emin'Image);
        end Show_Model_Mantissa_Emin;


    Attributes: :ada:`Model_Epsilon` and :ada:`Model_Small`
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    :ada:`Model_Epsilon` is an attribute that returns the
    :wikipedia:`epsilon <Machine_epsilon>` of the underlying
    numeric model. For example, for the :ada:`Float` type, the :ada:`Model_Epsilon`
    corresponds to 2\ :sup:`-23` on a typical desktop PC. (Here, 23 comes from the
    mantissa, 24 bits, minus the sign bit.)

    :ada:`Model_Small` is an attribute that returns the smallest value
    representable with the underlying numeric model. It corresponds to
    :ada:`Machine_Radix ** (-Model_Emin - 1)`. For example, for the :ada:`Float`
    type, this roughly corresponds to
    :ada:`Float (Float'Machine_Radix) ** (Float'Model_Emin - 1)`, or
    2\ :sup:`(-125 - 1)`. Note that the result of this calculation is of
    :ada:`Float` type, while the result of :ada:`Float'Model_Small` is a universal
    real.

    Let's see some examples:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Model_Epsilon_Small

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Model_Epsilon_Small is
        begin
           Put_Line
             ("Float'Model_Epsilon:           "
              & Float'Model_Epsilon'Image);
           Put_Line
             ("Long_Float'Model_Epsilon:      "
              & Long_Float'Model_Epsilon'Image);
           Put_Line
             ("Long_Long_Float'Model_Epsilon: "
              & Long_Long_Float'Model_Epsilon'Image);
           Put_Line
             ("Float'Model_Small:           "
              & Float'Model_Small'Image);
           Put_Line
             ("Long_Float'Model_Small:      "
              & Long_Float'Model_Small'Image);
           Put_Line
             ("Long_Long_Float'Model_Small: "
              & Long_Long_Float'Model_Small'Image);
        end Show_Model_Epsilon_Small;

    Attribute: :ada:`Model`
    ^^^^^^^^^^^^^^^^^^^^^^^^

    The :ada:`Model` attribute is similar to the :ada:`Machine` attribute that
    :ref:`we discussed earlier on <Adv_Ada_Machine_Attribute>`. The difference is
    that, instead of returning a version of :ada:`X` that is representable on the
    target machine (as the :ada:`Machine (X)` attribute does), :ada:`Model (X)`
    returns a version of :ada:`X` that is representable on the model that is being
    used. This is performed by rounding or truncating :ada:`X` to either one of the
    adjacent model numbers for the specific floating-point type of :ada:`X`.
    (As expected, if the real value of :ada:`X` is representable in the model, no
    modification is performed.)

    For example, let's say we want to calculate the difference between the real
    value in 1.0 x 10\ :sup:`15` and the actual model value, we can use the
    :ada:`Model` attribute:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Model_Attribute

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Model_Attribute is
           package F_IO is new
             Ada.Text_IO.Float_IO (Float);

           V : Float;
        begin
           F_IO.Default_Fore := 3;
           F_IO.Default_Aft  := 1;
           F_IO.Default_Exp  := 0;

           Put_Line
             ("Original value: 1_000_000_000_000_000.0");

           V := 1.0E+15;
           Put ("Model value:    ");
           F_IO.Put (Item => V);
           New_Line;

           V := 1.0E+15 - Float'Model (1.0E+15);
           Put ("Difference:     ");
           F_IO.Put (Item => V);
           New_Line;

        end Show_Model_Attribute;

    When running this example on a typical PC, we see that the difference is
    roughly 1.3009 x 10\ :sup:`7`. (Actually, the value we see is
    1.3008896 x 10\ :sup:`7`.)

    Depending on the model that is being used, the subtraction
    :ada:`1.0E+15 - Float'Model (1.0E+15)` might gives us the same value as
    :ada:`1.0E+15 - Float'Machine (1.0E+15)` or not. For example, the result is the
    same if the 32-bit IEEE floating-point model from the ISO/IEC 60559:2020
    standard is being used.


    Attributes: :ada:`Safe_First` and :ada:`Safe_Last`
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The :ada:`Safe_First` and :ada:`Safe_Last` attributes return the safe range of
    a type based on the underlying numeric model. As indicated by the Ada Reference
    Manual, this is the range "for which the accuracy corresponding to the base
    decimal precision is preserved by all predefined operations."

    Let's see a code example with these attributes and compare them to the
    :ada:`First` and :ada:`Last` attributes:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Floating_Point_Types.Safe_First_Last

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Safe_First_Last is
        begin
           Put_Line ("Float'First:                "
                     & Float'First'Image);
           Put_Line ("Float'Last:                 "
                     & Float'Last'Image);
           Put_Line ("Float'Safe_First:           "
                     & Float'Safe_First'Image);
           Put_Line ("Float'Safe_Last:            "
                     & Float'Safe_Last'Image);
           Put_Line ("Long_Float'First:           "
                     & Long_Float'First'Image);
           Put_Line ("Long_Float'Last:            "
                     & Long_Float'Last'Image);
           Put_Line ("Long_Float'Safe_First:      "
                     & Long_Float'Safe_First'Image);
           Put_Line ("Long_Float'Safe_Last:       "
                     & Long_Float'Safe_Last'Image);
           Put_Line ("Long_Long_Float'First:      "
                     & Long_Long_Float'First'Image);
           Put_Line ("Long_Long_Float'Last:       "
                     & Long_Long_Float'Last'Image);
           Put_Line ("Long_Long_Float'Safe_First: "
                     & Long_Long_Float'Safe_First'Image);
           Put_Line ("Long_Long_Float'Safe_Last:  "
                     & Long_Long_Float'Safe_Last'Image);
        end Show_Safe_First_Last;

    When comparing :ada:`Float'First` to :ada:`Float'Safe_First`, we see that the
    values are similar. However, :ada:`Float'Safe_First` has the precision of a
    universal real, while :ada:`Float'First` is limited to the precision of the
    :ada:`Float` type.


Fixed-Point Types
-----------------

In this section, we discuss various attributes and operations related to
fixed-point types.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.10 Operations of Fixed Point Types <3-5-10>`
    - :arm22:`A.5.4 Attributes of Fixed Point Types <A-5-4>`

Attributes of fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Attribute: :ada:`Machine_Radix`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Machine_Radix` is an attribute that returns the radix of the hardware
representation of a type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Machine_Radix

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Fixed_Machine_Radix is
       type T3_D3 is delta 10.0 ** (-3) digits 3;

       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("T3_D3'Machine_Radix: "
                 & T3_D3'Machine_Radix'Image);
       Put_Line ("TQ31'Machine_Radix:  "
                 & TQ31'Machine_Radix'Image);
    end Show_Fixed_Machine_Radix;

Usually, this value is two, as the radix is based on a binary system.


Attribute: :ada:`Machine_Rounds` and :ada:`Machine_Overflows`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we discuss attributes that return :ada:`Boolean` values
indicating whether a feature is available or not in the target architecture:

- :ada:`Machine_Rounds` is an attribute that indicates what happens when the
  result of a fixed-point operation is inexact:

  - :ada:`T'Machine_Rounds = True`: inexact result is rounded;

  - :ada:`T'Machine_Rounds = False`: inexact result is truncated.

- :ada:`Machine_Overflows` is an attribute that indicates whether a
  :ada:`Constraint_Error` is guaranteed to be raised when a fixed-point
  operation with that type produces an overflow or divide-by-zero.

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Machine_Rounds_Overflows

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Boolean_Attributes is
       type T3_D3 is delta 10.0 ** (-3) digits 3;

       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("T3_D3'Machine_Rounds:    "
                 & T3_D3'Machine_Rounds'Image);
       Put_Line ("TQ31'Machine_Rounds:     "
                 & TQ31'Machine_Rounds'Image);
       Put_Line ("T3_D3'Machine_Overflows: "
                 & T3_D3'Machine_Overflows'Image);
       Put_Line ("TQ31'Machine_Overflows:  "
                 & TQ31'Machine_Overflows'Image);
    end Show_Boolean_Attributes;


Attribute: :ada:`Small` and :ada:`Delta`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Small` and :ada:`Delta` attributes return numbers that indicate the
numeric precision of a fixed-point type. In many cases, the :ada:`Small` of a
type :ada:`T` is equal to the :ada:`Delta` of that type |mdash| i.e.
:ada:`T'Small = T'Delta`. Let's discuss each attribute and how they distinguish
from each other.

The :ada:`Delta` attribute returns the value of the :ada:`delta` that was
used in the type definition. For example, if we declare
:ada:`type T3_D3 is delta 10.0 ** (-3) digits D`, then the value of
:ada:`T3_D3'Delta` is the :ada:`10.0 ** (-3)` that we used in the type
definition.

The :ada:`Small` attribute returns the "small" of a type, i.e. the smallest
value used in the machine representation of the type. The *small* must be at
least equal to or smaller than the *delta* |mdash| in other words, it must
conform to the :ada:`T'Small <= T'Delta` rule.

.. admonition:: For further reading...

    The :ada:`Small` and the :ada:`Delta` need not actually be small numbers.
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

When we declare an ordinary fixed-point data type, we must specify the *delta*.
Specifying the *small*, however, is optional:

- If the *small* isn't specified, it is automatically selected by the compiler.
  In this case, the actual value of the *small* is an implementation-defined
  power of two |mdash| always following the rule that says:
  :ada:`T'Small <= T'Delta`.

- If we want, however, to specify the *small*, we can do that by using the
  :ada:`Small` aspect. In this case, it doesn't need to be a power of two.

For decimal fixed-point types, we cannot specify the *small*. In this case,
it's automatically selected by the compiler, and it's always equal to the
*delta*.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Small_Delta

    package Fixed_Small_Delta is
       D3 : constant := 10.0 ** (-3);

       type T3_D3 is delta D3 digits 3;

       type TD3   is delta D3 range -1.0 .. 1.0 - D3;

       D31 : constant := 2.0 ** (-31);
       D15 : constant := 2.0 ** (-15);

       type TQ31 is delta D31 range -1.0 .. 1.0 - D31;

       type TQ15 is delta D15 range -1.0 .. 1.0 - D15
         with Small => D31;
    end Fixed_Small_Delta;

    with Ada.Text_IO;       use Ada.Text_IO;

    with Fixed_Small_Delta; use Fixed_Small_Delta;

    procedure Show_Fixed_Small_Delta is
    begin
       Put_Line ("T3_D3'Small: "
                 & T3_D3'Small'Image);
       Put_Line ("T3_D3'Delta: "
                 & T3_D3'Delta'Image);
       Put_Line ("T3_D3'Size: "
                 & T3_D3'Size'Image);
       Put_Line ("--------------------");

       Put_Line ("TD3'Small: "
                 & TD3'Small'Image);
       Put_Line ("TD3'Delta: "
                 & TD3'Delta'Image);
       Put_Line ("TD3'Size: "
                 & TD3'Size'Image);
       Put_Line ("--------------------");

       Put_Line ("TQ31'Small: "
                 & TQ31'Small'Image);
       Put_Line ("TQ31'Delta: "
                 & TQ31'Delta'Image);
       Put_Line ("TQ32'Size: "
                 & TQ31'Size'Image);
       Put_Line ("--------------------");

       Put_Line ("TQ15'Small: "
                 & TQ15'Small'Image);
       Put_Line ("TQ15'Delta: "
                 & TQ15'Delta'Image);
       Put_Line ("TQ15'Size: "
                 & TQ15'Size'Image);
    end Show_Fixed_Small_Delta;

As we can see in the output of the code example, the :ada:`Delta` attribute
returns the value we used for :ada:`delta` in the type definition of the
:ada:`T3_D3`, :ada:`TD3`, :ada:`TQ31` and :ada:`TQ15` types.

The :ada:`TD3` type is an ordinary fixed-point type with the the same delta as
the decimal :ada:`T3_D3` type. In this case, however, :ada:`TD3'Small` is not
the same as the :ada:`TD3'Delta`. On a typical desktop PC, :ada:`TD3'Small` is
2\ :sup:`-10`, while the delta is 10\ :sup:`-3`. (Remember that, for ordinary
fixed-point types, if we don't specify the *small*, it's automatically selected
by the compiler as a power of two smaller than or equal to the *delta*.)

In the case of the :ada:`TQ15` type, we're specifying the *small* by using the
:ada:`Small` aspect. In this case, the underlying size of the :ada:`TQ15`
type is 32 bits, while the precision we get when operating with this type is
16 bits. Let's see a specific example for this type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Small_Delta

    with Ada.Text_IO;       use Ada.Text_IO;

    with Fixed_Small_Delta; use Fixed_Small_Delta;

    procedure Show_Fixed_Small_Delta is
       V : TQ15;
    begin
       Put_Line ("V'Size: " & V'Size'Image);

       V := TQ15'Small;
       Put_Line ("V: " & V'Image);

       V := TQ15'Delta;
       Put_Line ("V: " & V'Image);
    end Show_Fixed_Small_Delta;

In the first assignment, we assign :ada:`TQ15'Small` (2\ :sup:`-31`) to
:ada:`V`. This value is smaller than the type's *delta* (2\ :sup:`-15`). Even
though :ada:`V'Size` is 32 bits, :ada:`V'Delta` indicates 16-bit precision, and
:ada:`TQ15'Small` requires 32-bit precision to be represented correctly.
As a result, :ada:`V` has a value of zero after this assignment.

In contrast, after the second assignment |mdash| where we assign
:ada:`TQ15'Delta` (2\ :sup:`-15`) to :ada:`V` |mdash| we see, as expected, that
:ada:`V` has the same value as the *delta*.


Attributes: :ada:`Fore` and :ada:`Aft`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Fore` and :ada:`Aft` attributes indicate the number of characters
or digits needed for displaying a value in decimal representation. To be more
precise:

- The :ada:`Fore` attribute refers to the digits before the decimal point and
  it returns the number of digits plus one for the sign indicator (which is
  either :ada:`-` or space), and it's always at least two.

- The :ada:`Aft` attribute returns the number of decimal digits that is needed
  to represent the delta after the decimal point.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Fixed_Fore_Aft

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Fixed_Fore_Aft is
       type T3_D3 is delta 10.0 ** (-3) digits 3;

       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;

       Dec : constant T3_D3 := -0.123;
       Fix : constant TQ31  := -TQ31'Delta;
    begin
       Put_Line ("T3_D3'Fore: "
                 & T3_D3'Fore'Image);
       Put_Line ("T3_D3'Aft:  "
                 & T3_D3'Aft'Image);

       Put_Line ("TQ31'Fore: "
                 & TQ31'Fore'Image);
       Put_Line ("TQ31'Aft:  "
                 & TQ31'Aft'Image);
       Put_Line ("----");
       Put_Line ("Dec: "
                 & Dec'Image);
       Put_Line ("Fix: "
                 & Fix'Image);
    end Show_Fixed_Fore_Aft;

As we can see in the output of the :ada:`Dec` and :ada:`Fix` variables at the
bottom, the value of :ada:`Fore` is two for both :ada:`T3_D3` and :ada:`TQ31`.
This value corresponds to the length of the string "-0" displayed in the output
for these variables (the first two characters of "-0.123" and "-0.0000000005").

The value of :ada:`Dec'Aft` is three, which matches the number of digits after
the decimal point in "-0.123". Similarly, the value of :ada:`Fix'Aft` is 10,
which matches the number of digits after the decimal point in "-0.0000000005".


Attributes of decimal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The attributes presented in this subsection are only available for decimal
fixed-point types.

Attribute: :ada:`Digits`
^^^^^^^^^^^^^^^^^^^^^^^^^

:ada:`Digits` is an attribute that returns the number of significant decimal
digits of a decimal fixed-point subtype. This corresponds to the value that we
use for the :ada:`digits` in the definition of a decimal fixed-point type.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Digits

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Digits is
       type T3_D6 is delta 10.0 ** (-3) digits 6;
       subtype T3_D2 is T3_D6 digits 2;
    begin
       Put_Line ("T3_D6'Digits: "
                 & T3_D6'Digits'Image);
       Put_Line ("T3_D2'Digits: "
                 & T3_D2'Digits'Image);
    end Show_Decimal_Digits;

In this example, :ada:`T3_D6'Digits` is six, which matches the value that we
used for :ada:`digits` in the type definition of :ada:`T3_D6`. The same logic
applies for subtypes, as we can see in the value of :ada:`T3_D2'Digits`. Here,
the value is two, which was used in the declaration of the :ada:`T3_D2`
subtype.


Attribute: :ada:`Scale`
^^^^^^^^^^^^^^^^^^^^^^^^

According to the Ada Reference Manual, the :ada:`Scale` attribute "indicates
the position of the point relative to the rightmost significant digits of
values" of a decimal type. For example:

- If the value of :ada:`Scale` is two, then there are two decimal digits after
  the decimal point.

- If the value of :ada:`Scale` is negative, that implies that the
  :ada:`Delta` is a power of 10 greater than 1, and it would be the number of
  zero digits that every value would end in.

The :ada:`Scale` corresponds to the N used in the :ada:`delta 10.0 ** (-N)`
expression of the type declaration. For example, if we write
:ada:`delta 10.0 ** (-3)` in the declaration of a type :ada:`T`, then the value
of :ada:`T'Scale` is three.

Let's look at this complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Scale

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Scale is
       type TM3_D6 is delta 10.0 **   3  digits 6;
       type T3_D6  is delta 10.0 ** (-3) digits 6;
       type T9_D12 is delta 10.0 ** (-9) digits 12;
    begin
       Put_Line ("TM3_D6'Scale: "
                 & TM3_D6'Scale'Image);
       Put_Line ("T3_D6'Scale: "
                 & T3_D6'Scale'Image);
       Put_Line ("T9_D12'Scale: "
                 & T9_D12'Scale'Image);
    end Show_Decimal_Scale;

In this example, we get the following values for the scales:

- :ada:`TM3_D6'Scale = -3`,
- :ada:`T3_D6'Scale = 3`,
- :ada:`T9_D12 = 9`.

As you can see, the value of :ada:`Scale` is directly related to the *delta*
of the corresponding type declaration.


Attribute: :ada:`Round`
^^^^^^^^^^^^^^^^^^^^^^^^

The :ada:`Round` attribute rounds a value of any real type to the nearest
value that is a multiple of the *delta* of the decimal fixed-point type,
rounding away from zero if exactly between two such multiples.

For example, if we have a type :ada:`T` with three digits, and we use a value
with 10 digits after the decimal point in a call to :ada:`T'Round`, the
resulting value will have three digits after the decimal point.

Note that the :ada:`X` input of an :ada:`S'Round (X)` call is a universal real
value, while the returned value is of :ada:`S'Base` type.

Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Fixed_Point_Types.Decimal_Round

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_Round is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
    begin
       Put_Line ("T3_D3'Round (0.2774): "
                 & T3_D3'Round (0.2774)'Image);
       Put_Line ("T3_D3'Round (0.2777): "
                 & T3_D3'Round (0.2777)'Image);
    end Show_Decimal_Round;

Here, the :ada:`T3_D3` has a precision of three digits. Therefore, to fit this
precision, 0.2774 is rounded to 0.277, and 0.2777 is rounded to 0.278.


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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Numbers

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Numbers_Operators

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Factorial_Integer
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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Factorial_Big_Numbers

    pragma Ada_2022;

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

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Simple_Big_Integer_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Arbitrary_Big_Integer_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Floating_Point_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Fixed_Point_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Big_Integer_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Number_String_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Positive_Natural

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Integer_Rem_Mod

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Integer_Greatest_Common_Divisor

    pragma Ada_2022;

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


Big real and quotients
~~~~~~~~~~~~~~~~~~~~~~

An interesting feature of big reals is that they support quotients. In fact,
we can simply assign `2/3` to a big real variable. (Note that we're able to
omit the decimal points, as we write :ada:`2/3` instead of :ada:`2.0 / 3.0`.)
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Real_Quotient_Conversion

    pragma Ada_2022;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Numerics.Big_Numbers.Big_Numbers_In_Range

    pragma Ada_2022;

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
