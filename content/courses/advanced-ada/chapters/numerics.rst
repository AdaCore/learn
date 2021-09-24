Numerics
========

.. include:: ../../global.txt

Modular Types
-------------

In the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/strongly_typed_language>`,
we've seen that Ada has two kinds of integer type: signed and modular. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Modular_1

    package Num_Types is

       type Signed_Integer is range 1 .. 1_000_000;
       type Modular is mod 2**32;

    end Num_Types;

In this section, we discuss two attributes of modular types: :ada:`Modulus`
and :ada:`Mod`. We also discuss operations on modular types.


:ada:`Modulus` Attribute
~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Modulus` attribute returns the modulus of the modular type as an
universal integer value. Let's get the modulus of the 32-bit :ada:`Modular`
type that we've declared in the :ada:`Num_Types` package of the previous
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1

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
:ada:`Base` attribute :ref:`in this section <Base_Attribute>`.)

Operations on modular integers use modular (wraparound) arithmetic. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1

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

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1
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

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1

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

    .. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Mod_Attribute

        generic
           type Formal_Modular is mod <>;
        package Mod_Attribute is
           function F return Formal_Modular;
        end Mod_Attribute;

        package body Mod_Attribute is

           A_Signed_Integer : Integer := -1;

           function F return Formal_Modular is
           begin
              return Formal_Modular'Mod (A_Signed_Integer);
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

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Mod_Crc_CCITT_Ada

    package Crc_Defs is

        type Byte is mod 2 ** 8;
        type Crc  is mod 2 ** 16;

        type Byte_Array is array (Positive range <>) of Byte;

        function Crc_CCITT (A : Byte_Array) return Crc;

        procedure Display (Crc_A : Crc);

        procedure Display (A : Byte_Array);

    end Crc_Defs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Crc_Defs is

        package Byte_IO is new Modular_IO (Byte);
        package Crc_IO  is new Modular_IO (Crc);

        function Crc_CCITT (A : Byte_Array) return Crc is
           X : Byte;
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
       AA    : constant Byte_Array := (16#0#, 16#20#, 16#30#);
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
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/strongly_typed_language>`.
We've seen that there are two kinds of numeric literals in Ada: integer
literals and real literals. They are distinguished by the absence or presence
of a point. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Real_Integer_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Real_Integer_Literals is
       Integer_Literal : constant := 365;
       Real_Literal    : constant := 365.2564;
    begin
       Put_Line ("Integer Literal: " & Integer_Literal'Image);
       Put_Line ("Real Literal:    " & Real_Literal'Image);
    end Real_Integer_Literals;

Another classification takes the use of a base indicator into account.
(Remember that, when writing a literal such as :ada:`2#1011#`, the base is the
element before the first ``#`` sign.) So here we distinguish between decimal
literals and based literals. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Decimal_Based_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Based_Literals is

       package F_IO is new Ada.Text_IO.Float_IO (Float);

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
       Based_Real_Exp    : constant := 7#1.031_153_643#e3;
    begin
       F_IO.Default_Fore := 3;
       F_IO.Default_Aft  := 4;
       F_IO.Default_Exp  := 0;

       Put_Line ("Dec_Integer:       " & Dec_Integer'Image);

       Put ("Dec_Real:           ");
       F_IO.Put (Item => Dec_Real);
       New_Line;

       Put ("Dec_Real_Exp:       ");
       F_IO.Put (Item => Dec_Real_Exp);
       New_Line;

       Put_Line ("Based_Integer:     " & Based_Integer'Image);
       Put_Line ("Based_Integer_Exp: " & Based_Integer_Exp'Image);

       Put ("Based_Real:         ");
       F_IO.Put (Item => Based_Real);
       New_Line;

       Put ("Based_Real_Exp:     ");
       F_IO.Put (Item => Based_Real_Exp);
       New_Line;
    end Decimal_Based_Literals;

Based literals use the ``base#number#`` format. Also, they aren't limited to
simple integer literals such as :ada:`16#16D#`. In fact, we can use a point or
an exponent in based literals, as well as underscores. In addition, we can use
any base from 2 up to 16. We discuss these aspects further in the next section.


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
:ada:`3.14159265358979323846264338327950288419716939937510`.

Also, when using based literals, Ada allows any base from 2 to 16. Thus, we can
write the decimal number 136 in any one of the following notations:

.. code-block:: ada

    2#1000_1000#
    8#210#
    10#136#
    16#88#

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

    Output_Devices := Output_Devices  or   Lights_On;
    Output_Devices := Output_Devices  and  Lights_Off;

Of course, we can also use
:ref:`records with representation clauses <Record_Representation_Storage_Clauses>`
to do the above, which is even more elegant.

The notion of base in Ada allows for exponents, which is particularly pleasant.
For instance, we can write:

.. code-block:: ada

     Kilobinary  : constant := 2#1#e+10;
     Megabinary  : constant := 2#1#e+20;
     Gigabinary  : constant := 2#1#e+30;
     Terabinary  : constant := 2#1#e+40;
     Petabinary  : constant := 2#1#e+50;
     Exabinary   : constant := 2#1#e+60;
     Zettabinary : constant := 2#1#e+70;
     Yottabinary : constant := 2#1#e+80;

In based literals, the exponent |mdash| like the base |mdash| uses the regular
decimal notation and specifies the power of the base that the based literal
should be multiplied with to obtain the final value. For instance
:ada:`2#1#e+10` = :math:`1 \times 2^{10}` = :ada:`1_024` (in base 10), whereas
:ada:`16#F#e+2` = :math:`15 \times 16^2 = 15 \times 256` = :ada:`3_840` (in
base 10).

Based numbers apply equally well to real literals. We can, for instance, write:

.. code-block:: ada

    One_Third : constant := 3#0.1#;  --  same as 1.0/3

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

    One_Third_Approx : constant := 0.33333333333333333333333333333;
    Zero_Approx      : constant := 1.0 - 3.0 * One_Third_Approx;

where :ada:`Zero_Approx` is really :ada:`1.0e-29` |mdash| and that will show up
in your numerical computations. The above is quite handy when we want to write
fractions without any loss of precision.

Along these same lines, we can write:

.. code-block:: ada

    Big_Sum : constant := 1           +
                          Kilobinary  +
                          Megabinary  +
                          Gigabinary  +
                          Terabinary  +
                          Petabinary  +
                          Exabinary   +
                          Zettabinary;

    Result : constant := (Yottabinary - 1) / (Kilobinary - 1);
    Nil : constant := Result - Big_Sum;

and be guaranteed that :ada:`Nil` is equal to zero.

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Literals

    with Ada.Text_IO;

    procedure Ada_Numeric_Literals is
       Pi   : constant := 3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37510;
       Pi2  : constant := 3.14159265358979323846264338327950288419716939937510;
       Z    : constant := Pi - Pi2;
       pragma Assert (Z = 0.0);

       Bin_136 : constant := 2#1000_1000#;
       Oct_136 : constant := 8#210#;
       Dec_136 : constant := 10#136#;
       Hex_136 : constant := 16#88#;
       pragma Assert (Bin_136 = 136);
       pragma Assert (Oct_136 = 136);
       pragma Assert (Dec_136 = 136);
       pragma Assert (Hex_136 = 136);

       Lights_On  : constant := 2#1000_1000#;
       Lights_Off : constant := 2#0111_0111#;

       type Byte is mod 256;
       Output_Devices : Byte := 0;
       --  for Output_Devices'Address use 16#DEAD_BEEF#;  --  Memory mapped Output

       Kilobyte  : constant := 2#1#e+10;
       Megabyte  : constant := 2#1#e+20;
       Gigabyte  : constant := 2#1#e+30;
       Terabyte  : constant := 2#1#e+40;
       Petabyte  : constant := 2#1#e+50;
       Exabyte   : constant := 2#1#e+60;
       Zettabyte : constant := 2#1#e+70;
       Yottabyte : constant := 2#1#e+80;

       One_Third : constant := 3#1.0#e-1;  --  same as 1.0/3.0
       Zero      : constant := 1.0 - 3.0 * One_Third;
       pragma Assert (Zero = 0.0);

       One_Third_Approx : constant := 0.33333333333333333333333333333;
       Zero_Approx      : constant := 1.0 - 3.0 * One_Third_Approx;

       Big_Sum : constant := 1         +
                             Kilobyte  +
                             Megabyte  +
                             Gigabyte  +
                             Terabyte  +
                             Petabyte  +
                             Exabyte   +
                             Zettabyte;

       Result : constant := (Yottabyte - 1) / (Kilobyte - 1);

       Nil : constant := Result - Big_Sum;
       pragma Assert (Nil = 0);

       use Ada.Text_IO;

    begin
       Put_Line ("Z = " & Float'Image (Z));
       New_Line;
       Put_Line ("Bin_136 = " & Integer'Image (Bin_136));
       Put_Line ("Oct_136 = " & Integer'Image (Oct_136));
       Put_Line ("Dec_136 = " & Integer'Image (Dec_136));
       Put_Line ("Hex_136 = " & Integer'Image (Hex_136));
       New_Line;
       Output_Devices := Output_Devices  or   Lights_On;
       Put_Line ("Output_Devices (lights on ) = " & Byte'Image (Output_Devices));
       Output_Devices := Output_Devices  and  Lights_Off;
       Put_Line ("Output_Devices (lights off) = " & Byte'Image (Output_Devices));
       New_Line;
       Put_Line ("Zero        = " & Float'Image (Zero));
       Put_Line ("Zero_Approx = " & Float'Image (Zero_Approx));
       Put_Line ("Nil         = " & Integer'Image (Nil));
    end Ada_Numeric_Literals;


Operations
----------

.. admonition:: Relevant topics

    - operations on floating-point types: :ada:`S'Digits`

    - operations on fixed-point types: :ada:`S'Small`, :ada:`S'Delta`,
      :ada:`S'Fore`, :ada:`S'Aft`, :ada:`S'Digits`, ...

.. todo::

    Complete section!


Big Numbers
-----------

.. admonition:: Relevant topics

    - `Big Numbers <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-5.html>`_
    - `Big Integers <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-6.html>`_
    - `Big Reals <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-7.html>`_

.. todo::

    Complete section!


Floating-Point Types
--------------------

.. admonition:: Relevant topics

    - Brief mentioning relevant parts of
      `Model of Floating Point Arithmetic <http://www.ada-auth.org/standards/2xrm/html/RM-G-2-1.html>`_

.. todo::

    Complete section!


Fixed-Point Types
-----------------

.. admonition:: Relevant topics

    - Brief mentioning relevant parts of
      `Model of Fixed Point Arithmetic <http://www.ada-auth.org/standards/2xrm/html/RM-G-2-3.html>`_

.. todo::

    Complete section!
