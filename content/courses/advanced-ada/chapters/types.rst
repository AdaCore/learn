Types
=====

.. include:: ../../global.txt

Scalar Types
------------

In general terms, scalar types are the most basic types that we can get. As
we know, we can classify them as follows:

+-------------+----------+----------+
| Category    | Discrete | Numeric  |
+=============+==========+==========+
| Enumeration | Yes      | No       |
+-------------+----------+----------+
| Integer     | Yes      | Yes      |
+-------------+----------+----------+
| Real        | No       | Yes      |
+-------------+----------+----------+

Many attributes exist for scalar types. For example, we can use the
:ada:`Image` and :ada:`Value` attributes to convert between a given type and a
string type. The following table presents the main attributes for scalar types:

+------------+-----------------+----------------------------------------------+
| Category   | Attribute       | Returned value                               |
+============+=================+==============================================+
| Ranges     | :ada:`First`    | First value of the discrete subtype's range. |
+            +-----------------+----------------------------------------------+
|            | :ada:`Last`     | Last value of the discrete subtype's range.  |
+            +-----------------+----------------------------------------------+
|            | :ada:`Range`    | Range of the discrete subtype (corresponds   |
|            |                 | to :ada:`Subtype'First .. Subtype'Last`).    |
+------------+-----------------+----------------------------------------------+
| Iterators  | :ada:`Pred`     | Predecessor of the input value.              |
+            +-----------------+----------------------------------------------+
|            | :ada:`Succ`     | Successor of the input value.                |
+------------+-----------------+----------------------------------------------+
| Comparison | :ada:`Min`      | Minimum of two values.                       |
+            +-----------------+----------------------------------------------+
|            | :ada:`Max`      | Maximum of two values.                       |
+------------+-----------------+----------------------------------------------+
| String     | :ada:`Image`    | String representation of the input value.    |
+ conversion +-----------------+----------------------------------------------+
|            | :ada:`Value`    | Value of a subtype based on input string.    |
+------------+-----------------+----------------------------------------------+

We already discussed some of these attributes in the
Introduction to Ada course (in the sections about
:ref:`range and related attributes <Intro_Ada_Range_Attribute>` and
:ref:`image attribute <Intro_Ada_Image_Attribute>`). In this
section, we'll discuss some aspects that have been left out of the previous
course.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.5 Scalar types <3-5>`

Ranges
~~~~~~

We've seen that the :ada:`First` and :ada:`Last` attributes can be used with
discrete types. Those attributes are also available for real types. Here's an
example using the :ada:`Float` type and a subtype of it:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Ranges_Real_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_First_Last_Real is
       subtype Norm is Float range 0.0 .. 1.0;
    begin
       Put_Line ("Float'First: " & Float'First'Image);
       Put_Line ("Float'Last:  " & Float'Last'Image);
       Put_Line ("Norm'First:  " & Norm'First'Image);
       Put_Line ("Norm'Last:   " & Norm'Last'Image);
    end Show_First_Last_Real;

This program displays the first and last values of both the :ada:`Float` type
and the :ada:`Norm` subtype. In the case of the :ada:`Float` type, we see the
full range, while for the :ada:`Norm` subtype, we get the values we used in the
declaration of the subtype (i.e. 0.0 and 1.0).

Predecessor and Successor
~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the :ada:`Pred` and :ada:`Succ` attributes to get the predecessor
and successor of a specific value. For discrete types, this is simply the next
discrete value. For example, :ada:`Pred (2)` is 1 and :ada:`Succ (2)` is 3.
Let's look at a complete source-code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Show_Succ_Pred_Discrete

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Succ_Pred_Discrete is
       type State is (Idle, Started, Processing, Stopped);

       Machine_State : constant State := Started;

       I : constant Integer := 2;
    begin
       Put_Line ("State                     : "
                 & Machine_State'Image);
       Put_Line ("State'Pred (Machine_State): "
                 & State'Pred (Machine_State)'Image);
       Put_Line ("State'Succ (Machine_State): "
                 & State'Succ (Machine_State)'Image);
       Put_Line ("----------");

       Put_Line ("I               : "
                 & I'Image);
       Put_Line ("Integer'Pred (I): "
                 & Integer'Pred (I)'Image);
       Put_Line ("Integer'Succ (I): "
                 & Integer'Succ (I)'Image);
    end Show_Succ_Pred_Discrete;

In this example, we use the :ada:`Pred` and :ada:`Succ` attributes for a
variable of enumeration type (:ada:`State`) and a variable of :ada:`Integer`
type.

We can also use the :ada:`Pred` and :ada:`Succ` attributes with real types. In
this case, however, the value we get depends on the actual type we're using:

- for fixed-point types, the value is calculated using the smallest value
  (:ada:`Small`), which is derived from the declaration of the fixed-point
  type;

- for floating-point types, the value used in the calculation depends on
  representation constraints of the actual target machine.

Let's look at this example with a decimal type (:ada:`Decimal`) and a
floating-point type (:ada:`My_Float`):

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Show_Succ_Pred_Real

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Succ_Pred_Real is
       subtype My_Float is Float range 0.0 .. 0.5;

       type Decimal is delta 0.1 digits 2 range 0.0 .. 0.5;

       D : Decimal;
       N : My_Float;
    begin
       Put_Line ("---- DECIMAL -----");
       Put_Line ("Small: " & Decimal'Small'Image);
       Put_Line ("----- Succ -------");
       D := Decimal'First;
       loop
          Put_Line (D'Image);
          D := Decimal'Succ (D);

          exit when D = Decimal'Last;
       end loop;
       Put_Line ("----- Pred -------");

       D := Decimal'Last;
       loop
          Put_Line (D'Image);
          D := Decimal'Pred (D);

          exit when D = Decimal'First;
       end loop;
       Put_Line ("==================");

       Put_Line ("---- MY_FLOAT ----");
       Put_Line ("----- Succ -------");
       N := My_Float'First;
       for I in 1 .. 5 loop
          Put_Line (N'Image);
          N := My_Float'Succ (N);
       end loop;
       Put_Line ("----- Pred -------");

       for I in 1 .. 5 loop
          Put_Line (N'Image);
          N := My_Float'Pred (N);
       end loop;
    end Show_Succ_Pred_Real;

As the output of the program indicates, the smallest value (see
:ada:`Decimal'Small` in the example) is used to calculate the previous and next
values of :ada:`Decimal` type.

In the case of the :ada:`My_Float` type, the difference between the current
and the previous or next values is 1.40130E-45 (or 2\ :sup:`-149`) on a
standard PC.

Scalar To String Conversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've seen that we can use the :ada:`Image` and :ada:`Value` attributes to
perform conversions between values of a given subtype and a string:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Image_Value_Attr

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Image_Value_Attr is
       I : constant Integer := Integer'Value ("42");
    begin
       Put_Line (I'Image);
    end Show_Image_Value_Attr;

The :ada:`Image` and :ada:`Value` attributes are used for the :ada:`String`
type specifically. In addition to them, there are also attributes for different
string types |mdash| namely :ada:`Wide_String` and :ada:`Wide_Wide_String`.
This is the complete list of available attributes:

+-----------------------+-------------------------+---------------------------+
| Conversion type       | Attribute               | String type               |
+=======================+=========================+===========================+
| Conversion to string  | :ada:`Image`            | :ada:`String`             |
+                       +-------------------------+---------------------------+
|                       | :ada:`Wide_Image`       | :ada:`Wide_String`        |
+                       +-------------------------+---------------------------+
|                       | :ada:`Wide_Wide_Image`  | :ada:`Wide_Wide_String`   |
+-----------------------+-------------------------+---------------------------+
| Conversion to subtype | :ada:`Value`            | :ada:`String`             |
+                       +-------------------------+---------------------------+
|                       | :ada:`Wide_Value`       | :ada:`Wide_String`        |
+                       +-------------------------+---------------------------+
|                       | :ada:`Wide_Wide_Value`  | :ada:`Wide_Wide_String`   |
+-----------------------+-------------------------+---------------------------+

We discuss more about :ada:`Wide_String` and :ada:`Wide_Wide_String` in
:ref:`another section <Adv_Ada_Wide_Wide_Strings>`.

Width attribute
~~~~~~~~~~~~~~~

When converting a value to a string by using the :ada:`Image` attribute, we get
a string with variable width. We can assess the maximum width of that string
for a specific subtype by using the :ada:`Width` attribute. For example,
:ada:`Integer'Width` gives us the maximum width returned by the :ada:`Image`
attribute when converting a value of :ada:`Integer` type to a string of
:ada:`String` type.

This attribute is useful when we're using bounded strings in our code to store
the string returned by the :ada:`Image` attribute. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Width_Attr

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;

    procedure Show_Width_Attr is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => Integer'Width);
       use B_Str;

       Str_I : Bounded_String;

       I : constant Integer := 42;
       J : constant Integer := 103;
    begin
       Str_I := To_Bounded_String (I'Image);
       Put_Line ("Value:         " & To_String (Str_I));
       Put_Line ("String Length: " & Length (Str_I)'Image);
       Put_Line ("----");

       Str_I := To_Bounded_String (J'Image);
       Put_Line ("Value:         " & To_String (Str_I));
       Put_Line ("String Length: " & Length (Str_I)'Image);
    end Show_Width_Attr;

In this example, we're storing the string returned by :ada:`Image` in the
:ada:`Str_I` variable of :ada:`Bounded_String` type.

Similar to the :ada:`Image` and :ada:`Value` attributes, the :ada:`Width`
attribute is also available for string types other than :ada:`String`. In fact,
we can use:

- the :ada:`Wide_Width` attribute for strings returned by :ada:`Wide_Image`;
  and

- the :ada:`Wide_Wide_Width` attribute for strings returned by
  :ada:`Wide_Wide_Image`.

.. _Adv_Ada_Base_Attribute:

Base
~~~~

The :ada:`Base` attribute gives us the unconstrained underlying hardware
representation selected for a given numeric type. As an example, let's say we
declared a subtype of the :ada:`Integer` type named :ada:`One_To_Ten`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Base_Attr

    package My_Integers is

       subtype One_To_Ten is Integer range 1 .. 10;

    end My_Integers;

If we then use the :ada:`Base` attribute |mdash| by writing
:ada:`One_To_Ten'Base` |mdash|, we're actually referring to the unconstrained
underlying hardware representation selected for :ada:`One_To_Ten`. As
:ada:`One_To_Ten` is a subtype of the :ada:`Integer` type, this also means that
:ada:`One_To_Ten'Base` is equivalent to :ada:`Integer'Base`, i.e. they refer to
the same base type. (This base type is the underlying hardware type
representing the :ada:`Integer` type |mdash| but is not the :ada:`Integer` type
itself.)

.. admonition:: For further reading...

    The Ada standard defines that the minimum range of the :ada:`Integer` type
    is :ada:`-2**15 + 1 .. 2**15 - 1`. In modern 64-bit systems |mdash|
    where wider types such as :ada:`Long_Integer` are defined |mdash| the range
    is at least :ada:`-2**31 + 1 .. 2**31 - 1`. Therefore, we could think of
    the :ada:`Integer` type as having the following declaration:

    .. code-block:: ada

        type Integer is range -2 ** 31 .. 2 ** 31 - 1;

    However, even though :ada:`Integer` is a predefined Ada type, it's actually
    a subtype of an anonymous type. That anonymous "type" is the hardware's
    representation for the numeric type as chosen by the compiler based on the
    requested range (for the signed integer types) or digits of precision (for
    floating-point types). In other words, these types are actually subtypes of
    something that does not have a specific name in Ada, and that is not
    constrained.

    In effect,

    .. code-block:: ada

        type Integer is range -2 ** 31 .. 2 ** 31 - 1;

    is really as if we said this:

    .. code-block:: ada

        subtype Integer is Some_Hardware_Type_With_Sufficient_Range
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

    .. code:: ada compile_button project=Courses.Advanced_Ada.Types.Very_Big_Range
        :class: ada-expect-compile-error

        package Int_Def is

           type Too_Big_To_Fail is range -2 ** 255 .. 2 ** 255 - 1;

        end Int_Def;

    Otherwise, the compiler maps the named Ada type to the hardware "type",
    presumably choosing the smallest one that supports the requested range.
    (That's why the range has to be static in the source code, unlike for
    explicit subtypes.)

The following example shows how the :ada:`Base` attribute affects the bounds of
a variable:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Base_Attr

    with Ada.Text_IO; use Ada.Text_IO;
    with My_Integers; use My_Integers;

    procedure Show_Base is
       C : constant One_To_Ten := One_To_Ten'Last;
    begin
       Using_Constrained_Subtype : declare
          V : One_To_Ten := C;
       begin
          Put_Line ("Increasing value for One_To_Ten...");
          V := One_To_Ten'Succ (V);
       exception
          when others =>
             Put_Line ("Exception raised!");
       end Using_Constrained_Subtype;

       Using_Base : declare
          V : One_To_Ten'Base := C;
       begin
          Put_Line ("Increasing value for One_To_Ten'Base...");
          V := One_To_Ten'Succ (V);
       exception
          when others =>
             Put_Line ("Exception raised!");
       end Using_Base;

       Put_Line ("One_To_Ten'Last: "      & One_To_Ten'Last'Image);
       Put_Line ("One_To_Ten'Base'Last: " & One_To_Ten'Base'Last'Image);
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

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Base_Attr_Sat

    package My_Integers is

       subtype One_To_Ten is Integer range 1 .. 10;

       function Sat_Add (V1, V2 : One_To_Ten'Base) return One_To_Ten;

       function Sat_Sub (V1, V2 : One_To_Ten'Base) return One_To_Ten;

    end My_Integers;

    --  with Ada.Text_IO; use Ada.Text_IO;

    package body My_Integers is

       function Saturate (V : One_To_Ten'Base) return One_To_Ten is
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

       function Sat_Add (V1, V2 : One_To_Ten'Base) return One_To_Ten is
       begin
          return Saturate (V1 + V2);
       end Sat_Add;

       function Sat_Sub (V1, V2 : One_To_Ten'Base) return One_To_Ten is
       begin
          return Saturate (V1 - V2);
       end Sat_Sub;

    end My_Integers;

    with Ada.Text_IO; use Ada.Text_IO;
    with My_Integers; use My_Integers;

    procedure Show_Base is

       type Display_Saturate_Op is (Add, Sub);

       procedure Display_Saturate (V1, V2 : One_To_Ten;
                                   Op     : Display_Saturate_Op) is
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


Enumerations
------------

We've introduced enumerations back in the
:ref:`Introduction to Ada course <Intro_Ada_Enum_Types>`.
In this section, we'll discuss a few useful features of enumerations, such as
enumeration renaming, enumeration overloading and representation clauses.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.5.1 Enumeration Types <3-5-1>`

Enumerations as functions
~~~~~~~~~~~~~~~~~~~~~~~~~

If you have used programming language such as C in the past, you're familiar
with the concept of enumerations being constants with integer values. In Ada,
however, enumerations are not integers. In fact, they're actually parameterless
functions! Let's consider this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_As_Function

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       --  Essentially, we're declaring these functions:
       --
       --  function Mon return Day;
       --  function Tue return Day;
       --  function Wed return Day;
       --  function Thu return Day;
       --  function Fri return Day;
       --  function Sat return Day;
       --  function Sun return Day;

    end Days;

In the package :ada:`Days`, we're declaring the enumeration type :ada:`Day`.
When we do this, we're essentially declaring seven parameterless functions, one
for each enumeration. For example, the :ada:`Mon` enumeration corresponds to
:ada:`function Mon return Day`. You can see all seven function declarations in
the comments of the example above.

Note that this has no direct relation to how an Ada compiler generates machine
code for enumeration. Even though enumerations are parameterless functions, a
typical Ada compiler doesn't generate function calls for code that deals with
enumerations.

Enumeration renaming
^^^^^^^^^^^^^^^^^^^^

The idea that enumerations are parameterless functions can be used when we want
to rename enumerations. For example, we could rename the enumerations of the
:ada:`Day` type like this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

    package Enumeration_Example is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       function Monday    return Day renames Mon;
       function Tuesday   return Day renames Tue;
       function Wednesday return Day renames Wed;
       function Thursday  return Day renames Thu;
       function Friday    return Day renames Fri;
       function Saturday  return Day renames Sat;
       function Sunday    return Day renames Sun;

    end Enumeration_Example;

Now, we can use both :ada:`Monday` or :ada:`Mon` to refer to Monday of the
:ada:`Day` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

    with Ada.Text_IO;         use Ada.Text_IO;
    with Enumeration_Example; use Enumeration_Example;

    procedure Show_Renaming is
       D1 : constant Day := Mon;
       D2 : constant Day := Monday;
    begin
       if D1 = D2 then
          Put_Line ("D1 = D2");
          Put_Line (Day'Image (D1) & " =  " & Day'Image (D2));
       end if;
    end Show_Renaming;

When running this application, we can confirm that :ada:`D1` is equal to
:ada:`D2`. Also, even though we've assigned :ada:`Monday` to :ada:`D2` (instead
of :ada:`Mon`), the application displays ``Mon = Mon``, since :ada:`Monday`
is just another name to refer to the actual enumeration (:ada:`Mon`).

.. admonition:: Hint

    If you just want to have a single (renamed) enumeration visible in your
    application |mdash| and make the original enumeration invisible |mdash|,
    you can use a separate package. For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

        package Enumeration_Example is

           type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

        end Enumeration_Example;

        with Enumeration_Example;

        package Enumeration_Renaming is

           subtype Day is Enumeration_Example.Day;

           function Monday    return Day renames Enumeration_Example.Mon;
           function Tuesday   return Day renames Enumeration_Example.Tue;
           function Wednesday return Day renames Enumeration_Example.Wed;
           function Thursday  return Day renames Enumeration_Example.Thu;
           function Friday    return Day renames Enumeration_Example.Fri;
           function Saturday  return Day renames Enumeration_Example.Sat;
           function Sunday    return Day renames Enumeration_Example.Sun;

        end Enumeration_Renaming;

        with Ada.Text_IO;          use Ada.Text_IO;
        with Enumeration_Renaming; use Enumeration_Renaming;

        procedure Show_Renaming is
           D1 : constant Day := Monday;
        begin
           Put_Line (Day'Image (D1));
        end Show_Renaming;

    Note that the call to :ada:`Put_Line` still display ``Mon`` instead of
    ``Monday``.

Enumeration overloading
~~~~~~~~~~~~~~~~~~~~~~~

Enumerations can be overloaded. In simple terms, this means that the same name
can be used to declare an enumeration of different types. A typical example is
the declaration of colors:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    package Colors is

       type Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       type Primary_Color is
         (Red,
          Green,
          Blue);

    end Colors;

Note that we have :ada:`Red` as an enumeration of type :ada:`Color` and of type
:ada:`Primary_Color`. The same applies to :ada:`Green` and :ada:`Blue`. Because
Ada is a strongly-typed language, in most cases, the enumeration that we're
referring to is clear from the context. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Red_Colors is
       C1 : constant Color         := Red;   --  Using Red from Color
       C2 : constant Primary_Color := Red;   --  Using Red from Primary_Color
    begin
       if C1 = Red then
          Put_Line ("C1 = Red");
       end if;
       if C2 = Red then
          Put_Line ("C2 = Red");
       end if;
    end Red_Colors;

When assigning :ada:`Red` to :ada:`C1` and :ada:`C2`, it is clear that, in the
first case, we're referring to :ada:`Red` of :ada:`Color` type, while in the
second case, we're referring to :ada:`Red` of the :ada:`Primary_Color` type.
The same logic applies to comparisons such as the one in
:ada:`if C1 = Red`: because the type of :ada:`C1` is defined
(:ada:`Color`), it's clear that the :ada:`Red` enumeration is the one of
:ada:`Color` type.

Enumeration subtypes
^^^^^^^^^^^^^^^^^^^^

Note that enumeration overloading is not the same as enumeration subtypes. For
example, we could define the following subtype:

.. code:: ada no_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    package Colors.Shades is

       subtype Blue_Shades is Colors range Blue .. Darkblue;

    end Colors.Shades;

In this case, :ada:`Blue` of :ada:`Blue_Shades` and :ada:`Blue` of
:ada:`Colors` are the same enumeration.

Enumeration ambiguities
^^^^^^^^^^^^^^^^^^^^^^^

A situation where enumeration overloading might lead to ambiguities is when we
use them in ranges. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities
    :class: ada-expect-compile-error

    package Colors is

       type Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       type Primary_Color is
         (Red,
          Green,
          Blue);

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Red .. Blue loop       --  ERROR: range is ambiguous!
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Here, it's not clear whether the range in the loop is of :ada:`Color` type or
of :ada:`Primary_Color` type. Therefore, we get a compilation error for this
code example. The next line in the code example |mdash| the one with the call
to :ada:`Put_Line` |mdash| gives us a hint about the developer's intention to
refer to the :ada:`Color` type. In this case, we can use qualification |mdash|
for example, :ada:`Color'(Red)` |mdash| to resolve the ambiguity:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Color'(Red) .. Color'(Blue) loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Note that, in the case of ranges, we can also rewrite the loop by using a range
declaration:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Color range Red .. Blue loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Alternatively, :ada:`Color range Red .. Blue` could be used in a subtype
declaration, so we could rewrite the example above using a subtype (such as
:ada:`Red_To_Blue`) in the loop:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
       subtype Red_To_Blue is Color range Red .. Blue;
    begin
       for C in Red_To_Blue loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Enumeration representation clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've said above, a typical Ada compiler doesn't generate function calls for
code that deals with enumerations. On the contrary, each enumeration has values
associated with it, and the compiler uses those values instead.

Each enumeration has:

- a position value, which is a natural value indicating the position of the
  enumeration in the enumeration type; and

- an internal code, which, by default, in most cases, is the same as the
  position value.

Also, by default, the value of the first position is zero, the value of the
second position is one, and so on. We can see this by listing each enumeration
of the :ada:`Day` type and displaying the value of the corresponding position:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D) & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D) & " internal code = "
                    & Integer'Image (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Note that this application also displays the internal code, which, in this
case, is equivalent to the position value for all enumerations.

We may, however, change the internal code of an enumeration using a
representation clause, which has the following format:

.. code-block:: ada

    for Primary_Color is (Red   =>    1,
                          Green =>    5,
                          Blue  => 1000);

The value of each code in a representation clause must be distinct. However, as
you can see above, we don't need to use sequential values |mdash| the values
must, however, increase for each enumeration.

We can rewrite the previous example using a representation clause:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       for Day use (Mon => 2#00000001#, Tue => 2#00000010#,
                    Wed => 2#00000100#, Thu => 2#00001000#,
                    Fri => 2#00010000#, Sat => 2#00100000#,
                    Sun => 2#01000000#);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D) & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D) & " internal code = "
                    & Integer'Image (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Now, the value of the internal code is the one that we've specified in the
representation clause instead of being equivalent to the value of the
enumeration position.

In the example above, we're using binary values for each enumeration |mdash|
basically viewing the integer value as a bit-field and assigning one bit for
each enumeration. As long as we maintain an increasing order, we can use
totally arbitrary values as well. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       for Day use (Mon =>  5, Tue =>  9,
                    Wed => 42, Thu => 49,
                    Fri => 50, Sat => 66,
                    Sun => 99);

    end Days;

.. _Adv_Ada_Definite_Indefinite_Subtypes:

Definite and Indefinite Subtypes
--------------------------------

Indefinite types were mentioned back in the
:ref:`Introduction to Ada course <Intro_Ada_Indefinite_Subtype>`.
In this section, we'll recapitulate and extend on both definite and indefinite
types.

Definite types are the basic kind of types we commonly use when programming
applications. For example, we can only declare variables of definite types;
otherwise, we get a compilation error. Interestingly, however, to be able to
explain what definite types are, we need to first discuss indefinite types.

Indefinite types include:

- unconstrained arrays;

- record types with unconstrained discriminants without defaults.

Let's see some examples of indefinite types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    package Unconstrained_Types is

       type Integer_Array is array (Positive range <>) of Integer;

       type Simple_Record (Extended : Boolean) is record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

    end Unconstrained_Types;

As we've just mentioned, we cannot declare variable of indefinite types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types
    :class: ada-expect-compile-error

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Unconstrained_Type is

       A : Integer_Array;

       R : Simple_Record;

    begin
       null;
    end Using_Unconstrained_Type;

As we can see when we try to build this example, the compiler complains about
the declaration of :ada:`A` and :ada:`R` because we're trying to use indefinite
types to declare variables. The main reason we cannot use indefinite types here
is that the compiler needs to know at this point how much memory it should
allocate. Therefore, we need to provide the information that is missing. In
other words, we need to change the declaration so the type becomes definite. We
can do this by either declaring a definite type or providing constraints in the
variable declaration. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Unconstrained_Type is

       subtype Integer_Array_5 is Integer_Array (1 .. 5);

       A1 : Integer_Array_5;
       A2 : Integer_Array (1 .. 5);

       subtype Simple_Record_Ext is Simple_Record (Extended => True);

       R1 : Simple_Record_Ext;
       R2 : Simple_Record (Extended => True);

    begin
       null;
    end Using_Unconstrained_Type;

In this example, we declare the :ada:`Integer_Array_5` subtype, which is
definite because we're constraining it to a range from 1 to 5, thereby
defining the information that was missing in the indefinite type
:ada:`Integer_Array`. Because we now have a definite type, we can use it to
declare the :ada:`A1` variable. Similarly, we can use the indefinite type
:ada:`Integer_Array` directly in the declaration of :ada:`A2` by specifying the
previously unknown range.

Similarly, in this example, we declare the :ada:`Simple_Record_Ext` subtype,
which is definite because we're initializing the record discriminant
:ada:`Extended`. We can therefore use it in the declaration of the :ada:`R1`
variable. Alternatively, we can simply use the indefinite type
:ada:`Simple_Record` and specify the information required for the
discriminants. This is what we do in the declaration of the :ada:`R2` variable.

Although we cannot use indefinite types directly in variable declarations,
they're very useful to generalize algorithms. For example, we can use them as
parameters of a subprogram:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array (A : Integer_Array);

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Integer_Array (A : Integer_Array) is
    begin
       for I in A'Range loop
          Put_Line (Positive'Image (I) & ": " & Integer'Image (A (I)));
       end loop;
       Put_Line ("--------");
    end Show_Integer_Array;

    with Unconstrained_Types; use Unconstrained_Types;
    with Show_Integer_Array;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  := (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) := (1, 2, 3, 4, 5, others => 99);
    begin
       Show_Integer_Array (A_5);
       Show_Integer_Array (A_10);
    end Using_Unconstrained_Type;

In this particular example, the compiler doesn't know a priori which range is
used for the :ada:`A` parameter of :ada:`Show_Integer_Array`. It could be a
range from 1 to 5 as used for variable :ada:`A_5` of the
:ada:`Using_Unconstrained_Type` procedure, or it could be a range from 1 to 10
as used for variable :ada:`A_10`, or it could be anything else. Although the
parameter :ada:`A` of :ada:`Show_Integer_Array` is unconstrained, both calls to
:ada:`Show_Integer_Array` |mdash| in :ada:`Using_Unconstrained_Type` procedure
|mdash| use constrained objects.

Note that we could call the :ada:`Show_Integer_Array` procedure above with
another unconstrained parameter. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_With_Header (AA : Integer_Array;
                                              HH : String);

    with Ada.Text_IO;         use Ada.Text_IO;
    with Show_Integer_Array;

    procedure Show_Integer_Array_With_Header (AA : Integer_Array;
                                              HH : String) is
    begin
       Put_Line (HH);
       Show_Integer_Array (AA);
    end Show_Integer_Array_With_Header;

    with Unconstrained_Types;            use Unconstrained_Types;
    with Show_Integer_Array_With_Header;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  := (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) := (1, 2, 3, 4, 5, others => 99);
    begin
       Show_Integer_Array_With_Header (A_5, "First example");
       Show_Integer_Array_With_Header (A_10, "Second example");
    end Using_Unconstrained_Type;

In this case, we're calling the :ada:`Show_Integer_Array` procedure with
another unconstrained parameter (the :ada:`AA` parameter). However, although we
could have a long *chain* of procedure calls using indefinite types in their
parameters, we still use a (definite) object at the beginning of this chain.
For example, for the :ada:`A_5` object, we have this chain:

::

    A_5

        ==> Show_Integer_Array_With_Header (AA => A_5, ...);

            ==> Show_Integer_Array (A => AA);

Therefore, at this specific call to :ada:`Show_Integer_Array`, even though
:ada:`A` is declared as a parameter of indefinite type, the actual argument
is of definite type because :ada:`A_5` is constrained |mdash| and, thus, of
definite type.

Note that we can declare variables based on parameters of indefinite type. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_Plus (A : Integer_Array;
                                       V : Integer);

    with Show_Integer_Array;

    procedure Show_Integer_Array_Plus (A : Integer_Array;
                                       V : Integer) is
       A_Plus : Integer_Array (A'Range);
    begin
       for I in A_Plus'Range loop
          A_Plus (I) := A (I) + V;
       end loop;
       Show_Integer_Array (A_Plus);
    end Show_Integer_Array_Plus;

    with Unconstrained_Types; use Unconstrained_Types;
    with Show_Integer_Array_Plus;

    procedure Using_Unconstrained_Type is
       A_5 : constant Integer_Array (1 .. 5) := (1, 2, 3, 4, 5);
    begin
       Show_Integer_Array_Plus (A_5, 5);
    end Using_Unconstrained_Type;

In the :ada:`Show_Integer_Array_Plus` procedure, we're declaring :ada:`A_Plus`
based on the range of :ada:`A`, which is itself of indefinite type. However,
since the object passed as an argument to :ada:`Show_Integer_Array_Plus` must
have a constraint, :ada:`A_Plus` will also be constrained. For example, in the
call to :ada:`Show_Integer_Array_Plus` using :ada:`A_5` as an argument, the
declaration of :ada:`A_Plus` becomes :ada:`A_Plus : Integer_Array (1 .. 5);`.
Therefore, it becomes clear that the compiler needs to allocate five elements
for :ada:`A_Plus`.

We'll see later how definite and indefinite types apply to
:ref:`formal parameters <Adv_Ada_Formal_Definite_Indefinite_Subtypes>`.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.3 Objects and Named Numbers <3-3>`

Constrained Attribute
~~~~~~~~~~~~~~~~~~~~~

We can use the :ada:`Constrained` attribute to verify whether an object of
discriminated type is constrained or not. Let's start our discussion by reusing
the :ada:`Simple_Record` type from previous examples. In this version of the
:ada:`Unconstrained_Types` package, we're adding a :ada:`Reset` procedure for
the discriminated record type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Constrained_Attribute

    package Unconstrained_Types is

       type Simple_Record (Extended : Boolean := False) is record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

       procedure Reset (R : in out Simple_Record);

    end Unconstrained_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Unconstrained_Types is

       procedure Reset (R : in out Simple_Record) is
          Zero_Not_Extended : constant Simple_Record
            := (Extended => False, V => 0);
          Zero_Extended : constant Simple_Record
            := (Extended => True, V => 0, V_Float => 0.0);
       begin
          Put_Line ("---- Reset: R'Constrained => " & R'Constrained'Image);

          if not R'Constrained then
             R := Zero_Extended;
          else
             if R.Extended then
                R := Zero_Extended;
             else
                R := Zero_Not_Extended;
             end if;
          end if;
       end Reset;

    end Unconstrained_Types;

As the name indicates, the :ada:`Reset` procedure initializes all record
components with zero. Note that we use the :ada:`Constrained` attribute to
verify whether objects are constrained before assigning to them. For objects
that are not constrained, we can simply assign another object to it |mdash| as
we do with the :ada:`R := Zero_Extended` statement. When an object is
constrained, however, the discriminants must match. If we assign an object to
:ada:`R`, the discriminant of that object must match the discriminant of
:ada:`R`. This is the kind of verification that we do in the :ada:`else` part
of that procedure: we check the state of the :ada:`Extended` discriminant
before assigning an object to the :ada:`R` parameter.

The :ada:`Using_Constrained_Attribute` procedure below declares two objects of
:ada:`Simple_Record` type: :ada:`R1` and :ada:`R2`. Because the
:ada:`Simple_Record` type has a default value for its discriminant, we can
declare objects of this type without specifying a value for the discriminant.
This is exactly what we do in the declaration of :ada:`R1`. Here, we don't
specify any constraints, so that it takes the default value
(:ada:`Extended => False`).  In the declaration of :ada:`R2`, however, we
explicitly set :ada:`Extended` to :ada:`False`:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Constrained_Attribute

    with Ada.Text_IO;         use Ada.Text_IO;

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Constrained_Attribute is
       R1 : Simple_Record;
       R2 : Simple_Record (Extended => False);

       procedure Show_Rs is
       begin
          Put_Line ("R1'Constrained => "
                    & R1'Constrained'Image);
          Put_Line ("R1.Extended => "
                    & R1.Extended'Image);
          Put_Line ("--");
          Put_Line ("R2'Constrained => "
                    & R2'Constrained'Image);
          Put_Line ("R2.Extended => "
                    & R2.Extended'Image);
          Put_Line ("----------------");
       end Show_Rs;
    begin
       Show_Rs;

       Reset (R1);
       Reset (R2);
       Put_Line ("----------------");

       Show_Rs;
    end Using_Constrained_Attribute;

When we run this code, the user messages from :ada:`Show_Rs` indicate to us
that :ada:`R1` is not constrained, while :ada:`R2` is constrained.
Because we declare :ada:`R1` without specifying a value for the :ada:`Extended`
discriminant, :ada:`R1` is not constrained. In the declaration of
:ada:`R2`, on the other hand, the explicit value for the :ada:`Extended`
discriminant makes this object constrained. Note that, for both :ada:`R1` and
:ada:`R2`, the value of :ada:`Extended` is :ada:`False` in the declarations.

As we were just discussing, the :ada:`Reset` procedure includes checks to avoid
mismatches in discriminants. When we don't have those checks, we might get
exceptions at runtime. We can force this situation by replacing the
implementation of the :ada:`Reset` procedure with the following lines:

.. code-block:: ada

    --  [...]
    begin
       Put_Line ("---- Reset: R'Constrained => " & R'Constrained'Image);
       R := Zero_Extended;
    end Reset;

Running the code now generates a runtime exception:

::

    raised CONSTRAINT_ERROR : unconstrained_types.adb:12 discriminant check failed

This exception is raised during the call to :ada:`Reset (R2)`. As see in the
code, :ada:`R2` is constrained. Also, its :ada:`Extended` discriminant is set
to :ada:`False`, which means that it doesn't have the :ada:`V_Float`
component. Therefore, :ada:`R2` is not compatible with the constant
:ada:`Zero_Extended` object, so we cannot assign :ada:`Zero_Extended` to
:ada:`R2`. Also, because :ada:`R2` is constrained, its :ada:`Extended`
discriminant cannot be modified.

The behavior is different for the call to :ada:`Reset (R1)`, which works fine.
Here, when we pass :ada:`R1` as an argument to the :ada:`Reset` procedure, its
:ada:`Extended` discriminant is :ada:`False` by default. Thus, :ada:`R1` is
also not compatible with the :ada:`Zero_Extended` object. However, because
:ada:`R1` is not constrained, the assignment modifies :ada:`R1` (by changing
the value of the :ada:`Extended` discriminant). Therefore, with the call to
:ada:`Reset`, the :ada:`Extended` discriminant of :ada:`R1` changes from
:ada:`False` to :ada:`True`.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.7.2 Operations of Discriminated Types <3-7-2>`


.. _Adv_Ada_Incomplete_Types:

Incomplete types
----------------

Incomplete types |mdash| as the name suggests |mdash| are types that have
missing information in their declaration. This is a simple example:

.. code-block:: ada

    type Incomplete;

Because this type declaration is incomplete, we need to provide the missing
information at some later point. Consider the incomplete type :ada:`R` in the
following example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Incomplete_Types

    package Incomplete_Type_Example is

       type R;
       --  Incomplete type declaration!

       type R is record
          I : Integer;
       end record;
       --  type R is now complete!

    end Incomplete_Type_Example;

The first declaration of type :ada:`R` is incomplete. However, in the second
declaration of :ada:`R`, we specify that :ada:`R` is a record. By providing
this missing information, we're completing the type declaration of :ada:`R`.

It's also possible to declare an incomplete type in the private part of a
package specification and its complete form in the package body. Let's rewrite
the example above accordingly:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Incomplete_Types_2

    package Incomplete_Type_Example is

    private

       type R;
       --  Incomplete type declaration!

    end Incomplete_Type_Example;

    package body Incomplete_Type_Example is

       type R is record
          I : Integer;
       end record;
       --  type R is now complete!

    end Incomplete_Type_Example;

A typical application of incomplete types is to create linked lists using
access types based on those incomplete types. This kind of type is called
a recursive type. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Linked_List_Example

    package Linked_List_Example is

       type Integer_List;

       type Next is access Integer_List;

       type Integer_List is record
          I : Integer;
          N : Next;
       end record;

    end Linked_List_Example;

Here, the :ada:`N` component of :ada:`Integer_List` is essentially giving us
access to the next element of :ada:`Integer_List` type. Because the :ada:`Next`
type is both referring to the :ada:`Integer_List` type and being used in the
declaration of the :ada:`Integer_List` type, we need to start with an
incomplete declaration of the :ada:`Integer_List` type and then complete it
after the declaration of :ada:`Next`.

Incomplete types are useful to declare mutually dependent types, as we'll
see in the next section. Also, we can also have formal incomplete types, as
we'll discuss :ref:`later <Adv_Ada_Formal_Incomplete_Types>`.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.10.1 Incomplete Type Declarations <3-10-1>`


.. _Adv_Ada_Mutually_Dependent_Types:

Mutually dependent types
------------------------

In this section, we discuss how to use incomplete types to declare mutually
dependent types. Let's start with this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent
    :class: ada-expect-compile-error

    package Mutually_Dependent is

       type T1 is record
          B : T2;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

When you try to compile this example, you get a compilation error. The first
problem with this code is that, in the declaration of the :ada:`T1` record, the
compiler doesn't know anything about :ada:`T2`. We could solve this by
declaring an incomplete type (:ada:`type T2;`) before the declaration of
:ada:`T1`. This, however, doesn't solve all the problems in the code: the
compiler still doesn't know the size of :ada:`T2`, so we cannot create a
component of this type. We could, instead, declare an access type and use it
here. By doing this, even though the compiler doesn't know the size of
:ada:`T2`, it knows the size of an access type designating :ada:`T2`, so the
record component can be of such an access type.

To summarize, in order to solve the compilation error above, we need to:

- use at least one incomplete type;
- declare at least one component as an access to an object.

For example, we could declare an incomplete type :ada:`T2` and then declare
the component :ada:`B` of the :ada:`T1` record as an access to :ada:`T2`.
This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent

    package Mutually_Dependent is

       type T2;
       type T2_Access is access T2;

       type T1 is record
          B : T2_Access;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

We could strive for consistency and declare two incomplete types and two
accesses, but this isn't strictly necessary in this case. Here's the adapted
code:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent

    package Mutually_Dependent is

       type T1;
       type T1_Access is access T1;

       type T2;
       type T2_Access is access T2;

       type T1 is record
          B : T2_Access;
       end record;

       type T2 is record
          A : T1_Access;
       end record;

    end Mutually_Dependent;

Later on, we'll see that these code examples can be written using
:ref:`anonymous access types <Adv_Ada_Mutually_Dependent_Types_Using_Anonymous_Access_Types>`.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.10.1 Incomplete Type Declarations <3-10-1>`


.. _Adv_Ada_Type_View:

Type view
---------

Ada distinguishes between the partial and the full view of a type. The full
view is a type declaration that contains all the information needed by the
compiler. For example, the following declaration of type :ada:`R` represents
the full view of this type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Full_View

    package Full_View is

       --  Full view of the R type:
       type R is record
          I : Integer;
       end record;

    end Full_View;

As soon as we start applying encapsulation and information hiding |mdash| via
the :ada:`private` keyword |mdash| to a specific type, we are introducing a
partial view and making only that view compile-time visible to clients. Doing
so requires us to introduce the private part of the package (unless already
present). For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Partial_Full_View

    package Partial_Full_Views is

       --  Partial view of the R type:
       type R is private;

    private

       --  Full view of the R type:
       type R is record
          I : Integer;
       end record;

    end Partial_Full_Views;

As indicated in the example, the :ada:`type R is private` declaration is the
partial view of the :ada:`R` type, while the :ada:`type R is record [...]`
declaration in the private part of the package is the full view.

Although the partial view doesn't contain the full type declaration, it
contains very important information for the users of the package where it's
declared. In fact, the partial view of a private type is all that users
actually need to know to effectively use this type, while the full view is only
needed by the compiler.

In the previous example, the partial view indicates that :ada:`R` is a private
type, which means that, even though users cannot directly access any
information stored in this type |mdash| for example, read the value of the
:ada:`I` component of :ada:`R` |mdash|, they can use the :ada:`R` type to
declare objects. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Partial_Full_View

    with Partial_Full_Views; use Partial_Full_Views;

    procedure Main is
       --  Partial view of R indicates that R exists as a private type,
       --  so we can declare objects of this type:
       C : R;
    begin
       --  But we cannot directly access any information declared in the full
       --  view of R:
       --
       --  C.I := 42;
       --
       null;
    end Main;

In many cases, the restrictions applied to the partial and full views must
match. For example, if we declare a limited type in the full view of a private
type, its partial view must also be limited:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Limited_Private

    package Limited_Private_Example is

       --  Partial view must be limited, since the
       --  full view is limited.
       type R is limited private;

    private

       type R is limited record
          I : Integer;
       end record;

    end Limited_Private_Example;

There are, however, situations where the full view may contain additional
requirements that aren't mentioned in the partial view. For example, a type may
be declared as non-tagged in the partial view, but, at the same time, be tagged
in the full view:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Tagged_Full_View

    package Tagged_Full_View_Example is

       --  Partial view using non-tagged type:
       type R is private;

    private

       --  Full view using tagged type:
       type R is tagged record
          I : Integer;
       end record;

    end Tagged_Full_View_Example;

In this case, from a user's perspective, the :ada:`R` type is non-tagged, so
that users cannot use any object-oriented programming features for this type.
In the package body of :ada:`Tagged_Full_View_Example`, however, this type is
tagged, so that all object-oriented programming features are available for
subprograms of the package body that make use of this type. Again, the partial
view of the private type contains the most important information for users that
want to declare objects of this type.

.. admonition:: Important

    Although it's very common to declare private types as record types, this is
    not the only option. In fact, we could declare any type in the full view
    |mdash| scalars, for example |mdash|, so we could declare a "private
    integer" type:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Types.Private_Integer

        package Private_Integers is

           --  Partial view of private Integer type:
           type Private_Integer is private;

        private

           --  Full view of private Integer type:
           type Private_Integer is new Integer;

        end Private_Integers;

    This code compiles as expected, but isn't very useful. We can improve it by
    adding operators to it, for example:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Types.Private_Integer

        package Private_Integers is

           --  Partial view of private Integer type:
           type Private_Integer is private;

           function "+" (Left, Right : Private_Integer)
                         return Private_Integer;

        private

           --  Full view of private Integer type:
           type Private_Integer is new Integer;

        end Private_Integers;

        package body Private_Integers is

           function "+" (Left, Right : Private_Integer)
                         return Private_Integer is
              Res : constant Integer := Integer (Left)
                                        + Integer (Right);
              --  Note that we're converting Left and Right to
              --  Integer, which calls the "+" operator of the
              --  Integer type. Writing "Left + Right" would
              --  have called the "+" operator of Private_Integer,
              --  which leads to recursive calls, as this is the
              --  operator we're currently in.
           begin
              return Private_Integer (Res);
           end "+";

        end Private_Integers;

    Now, we can use the :ada:`+` operator as a common integer variable:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Types.Private_Integer

        with Private_Integers; use Private_Integers;

        procedure Show_Private_Integers is
           A, B : Private_Integer;
        begin
           A := A + B;
        end Show_Private_Integers;

.. admonition:: In the Ada Reference Manual

    - :arm:`7.3 Private Types and Private Extensions <7-3>`


Type conversion
---------------

.. admonition:: In the Ada Reference Manual

    - :arm:`4.6 Type Conversions <4-6>`

.. todo::

    Complete section!

    Bried discussion on:

    - type conversion
    - view conversion
    - value conversion


Qualified Expressions
---------------------

We already saw qualified expressions in the
:ref:`Introduction to Ada <Intro_Ada_Qualified_Expressions>` course. As
mentioned there, a qualified expression specifies the exact type that the
target expression will be resolved to, and it can be either any expression
in parentheses, or an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Qualified_Expressions

    package Simple_Integers is

       type Int is new Integer;

       subtype Int_Not_Zero is Int
         with Dynamic_Predicate => Int_Not_Zero /= 0;

    end Simple_Integers;

    with Simple_Integers; use Simple_Integers;

    procedure Show_Qualified_Expressions is
       I : Int;
    begin
       I := Int'(0);
    end Show_Qualified_Expressions;

Here, :ada:`Int'(0)` indicates that the value zero is of :ada:`Int` type.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.7 Qualified Expressions <4-7>`


Verifying subtypes
~~~~~~~~~~~~~~~~~~

.. note::

   This feature was introduced in Ada 2022.

We can use qualified expressions to verify a subtype's predicate:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Qualified_Expressions
    :class: ada-run-expect-failure

    with Simple_Integers; use Simple_Integers;

    procedure Show_Qualified_Expressions is
       I : Int;
    begin
       I := Int_Not_Zero'(0);
    end Show_Qualified_Expressions;

Here, the qualified expression checks the dynamic predicate of
:ada:`Int_Not_Zero`, which fails at runtime.


Default initial values
----------------------

In the
:ref:`Introduction to Ada course <Intro_Ada_Record_Default_Values>`,
we've seen that record components can have default values. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_1

    package Defaults is

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

    end Defaults;

In this section, we'll extend the concept of default values to other kinds of
type declarations, such as scalar types and arrays.

To assign a default value for a scalar type declaration |mdash| such as an
enumeration and a new integer |mdash|, we use the :ada:`Default_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_2

    package Defaults is

       type E is (E1, E2, E3) with Default_Value => E1;

       type T is new Integer with Default_Value => -1;

    end Defaults;

Note that we cannot specify a default value for a subtype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_3
    :class: ada-expect-compile-error

    package Defaults is

       subtype T is Integer with Default_Value => -1;
       --  ERROR!!

    end Defaults;

For array types, we use the :ada:`Default_Component_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_4

    package Defaults is

       type Arr is array (Positive range <>) of Integer
         with Default_Component_Value => -1;

    end Defaults;

This is a package containing the declarations we've just seen:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults

    package Defaults is

       type E is (E1, E2, E3) with Default_Value => E1;

       type T is new Integer with Default_Value => -1;

       --  We cannot specify default values for subtypes:
       --
       --  subtype T is Integer with Default_Value => -1;

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

       type Arr is array (Positive range <>) of Integer
         with Default_Component_Value => -1;

    end Defaults;

In the example below, we declare variables of the types from the
:ada:`Defaults` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Defaults

    with Ada.Text_IO; use Ada.Text_IO;
    with Defaults; use Defaults;

    procedure Use_Defaults is
       E1 : E;
       T1 : T;
       R1 : R;
       A1 : Arr (1 .. 5);
    begin
       Put_Line ("Enumeration:  " & E'Image (E1));
       Put_Line ("Integer type: " & T'Image (T1));
       Put_Line ("Record type:  " & Positive'Image (R1.X)
                 & ", " & Positive'Image (R1.Y));

       Put ("Array type:   ");
       for V of A1 loop
          Put (Integer'Image (V) & " ");
       end loop;
       New_Line;
    end Use_Defaults;

As we see in the :ada:`Use_Defaults` procedure, all variables still have their
default values, since we haven't assigned any value to them.

.. admonition:: In the Ada Reference Manual

    - :arm:`3.5 Scalar Types <3-5>`
    - :arm:`3.6 Array Types <3-6>`


Deferred Constants
------------------

Deferred constants are declarations where the value of the constant is not
specified immediately, but rather *deferred* to a later point. In that sense,
if a constant declaration is deferred, it is actually declared twice:

1. in the deferred constant declaration, and
2. in the full constant declaration.

The simplest form of deferred constant is the one that has a full constant
declaration in the private part of the package specification. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Private

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       Light : constant Speed := 299_792_458.0;
       --      ^ full constant declaration

    end Deferred_Constants;

Another form of deferred constant is the one that imports a constant from an
external implementation |mdash| using the :ada:`Import` keyword. We can use
this to import a constant declaration from an implementation in C. For example,
we can declare the :c:`light` constant in a C file:

.. code:: c no_button manual_chop project=Courses.Advanced_Ada.Types.Deferred_Constant_C
    :class: ada-syntax-only

    !constants.c
    double light = 299792458.0;

Then, we can import this constant in the :ada:`Deferred_Constants` package:

.. code:: ada no_button project=Courses.Advanced_Ada.Types.Deferred_Constant_C
    :class: ada-syntax-only

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed with
         Import, Convention => C;
       --      ^ deferred constant declaration; imported from C file

    end Deferred_Constants;

In this case, we don't have a full declaration in the :ada:`Deferred_Constants`
package, as the :ada:`Light` constant is imported from the :file:`constants.c`
file.

As a rule, the deferred and the full declarations should match |mdash| except,
of course, for the actual value that is missing in the deferred declaration.
For instance, we're not allowed to use different types in both declarations.
However, we may use a subtype in the full declaration |mdash| as long as it's
compatible with the type that was used in the deferred declaration. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Subtype

    package Deferred_Constants is

       type Speed is new Long_Float;

       subtype Positive_Speed is Speed range 0.0 .. Speed'Last;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       Light : constant Positive_Speed := 299_792_458.0;
       --      ^ full constant declaration using a subtype

    end Deferred_Constants;

Here, we're using the :ada:`Speed` type in the deferred declaration of the
:ada:`Light` constant, but we're using the :ada:`Positive_Speed` subtype in
the full declaration.

A useful application of deferred constants is when the value of the constant is
calculated using entities not meant to be compile-time visible to clients.
As such, these other entities are only visible in the private part of the
package, so that's where the value of the deferred constant must be computed.
For example, the full constant declaration may be computed by a call to an
expression function:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Function

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       function Calculate_Light return Speed is (299_792_458.0);

       Light : constant Speed := Calculate_Light;
       --      ^ full constant declaration calling a private function

    end Deferred_Constants;

Here, we call the :ada:`Calculate_Light` function |mdash| declared in the
private part of the :ada:`Deferred_Constants` package |mdash| for the full
declaration of the :ada:`Light` constant.

.. admonition:: In the Ada Reference Manual

    - :arm:`7.4 Deferred Constants <7-4>`


.. _Adv_Ada_User_Defined_Literals:

User-defined literals
---------------------

Any type definition has a kind of literal associated with it. For example,
integer types are associated with integer literals. Therefore, we can
initialize an object of integer type with an integer literal:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Simple_Integer_Literal

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Integer_Literal is
       V : Integer;
    begin
       V := 10;

       Put_Line (Integer'Image (V));
    end Simple_Integer_Literal;

Here, :ada:`10` is the integer literal that we use to initialize the integer
variable :ada:`V`. Other examples of literals are real literals and string
literals, as we'll see later.

When we declare an enumeration type, we limit the set of literals that we can
use to initialize objects of that type:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Simple_Enumeration

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Enumeration is
       type Activation_State is (Unknown, Off, On);

       S : Activation_State;
    begin
       S := On;
       Put_Line (Activation_State'Image (S));
    end Simple_Enumeration;

For objects of :ada:`Activation_State` type, such as :ada:`S`, the only
possible literals that we can use are :ada:`Unknown`, :ada:`Off` and :ada:`On`.
In this sense, types have a constrained set of literals that can be used for
objects of that type.

User-defined literals allow us to extend this set of literals. We could, for
example, extend the type declaration of :ada:`Activation_State` and allow the
use of integer literals for objects of that type. In this case, we need to use
the :ada:`Integer_Literal` aspect and specify a function that implements the
conversion from literals to the type we're declaring. For this conversion from
integer literals to the :ada:`Activation_State` type, we could specify that 0
corresponds to :ada:`Off`, 1 corresponds to :ada:`On` and other values
correspond to :ada:`Unknown`. We'll see the corresponding implementation later.

.. note:: This feature was first introduced in Ada 2020 and might not be
          available in older compilers.

These are the three kinds of literals and their corresponding aspect:

+----------+----------+-------------------------+
| Literal  | Example  | Aspect                  |
+==========+==========+=========================+
| Integer  |        1 | :ada:`Integer_Literal`  |
+----------+----------+-------------------------+
| Real     |      1.0 | :ada:`Real_Literal`     |
+----------+----------+-------------------------+
| String   |     "On" | :ada:`String_Literal`   |
+----------+----------+-------------------------+

For our previous :ada:`Activation_States` type, we could declare a function
:ada:`Integer_To_Activation_State` that converts integer literals to one of the
enumeration literals that we've specified for the :ada:`Activation_States`
type:

.. code:: ada manual_chop no_button project=Courses.Advanced_Ada.Types.User_Defined_Literals

    !activation_states.ads
    package Activation_States is

       type Activation_State is (Unknown, Off, On)
         with Integer_Literal => Integer_To_Activation_State;

       function Integer_To_Activation_State (S : String)
                                             return Activation_State;

    end Activation_States;

Based on this specification, we can now use an integer literal to initialize an
object :ada:`S` of :ada:`Activation_State` type:

.. code-block:: ada

    S : Activation_State := 1;

Note that we have a string parameter in the declaration of the
:ada:`Integer_To_Activation_State` function, even though the function itself is
only used to convert integer literals (but not string literals) to the
:ada:`Activation_State` type. It's our job to process that string parameter in
the implementation of the :ada:`Integer_To_Activation_State` function and
convert it to an integer value |mdash| using :ada:`Integer'Value`, for example:

.. code:: ada manual_chop compile_button project=Courses.Advanced_Ada.Types.User_Defined_Literals

    !activation_states.adb
    package body Activation_States is

       function Integer_To_Activation_State (S : String)
                                             return Activation_State is
       begin
          case Integer'Value (S) is
             when 0      => return Off;
             when 1      => return On;
             when others => return Unknown;
          end case;
       end Integer_To_Activation_State;

    end Activation_States;

Let's look at a complete example that makes use of all three kinds of literals:

.. code:: ada manual_chop run_button project=Courses.Advanced_Ada.Types.Activation_States

    !activation_states.ads
    package Activation_States is

       type Activation_State is (Unknown, Off, On)
         with String_Literal  => To_Activation_State,
              Integer_Literal => Integer_To_Activation_State,
              Real_Literal    => Real_To_Activation_State;

       function To_Activation_State (S : Wide_Wide_String)
                                     return Activation_State;

       function Integer_To_Activation_State (S : String)
                                             return Activation_State;

       function Real_To_Activation_State (S : String)
                                          return Activation_State;

    end Activation_States;

    !activation_states.adb
    package body Activation_States is

       function To_Activation_State (S : Wide_Wide_String)
                                     return Activation_State is
       begin
          if S = "Off" then
             return Off;
          elsif S = "On" then
             return On;
          else
             return Unknown;
          end if;
       end To_Activation_State;

       function Integer_To_Activation_State (S : String)
                                             return Activation_State is
       begin
          case Integer'Value (S) is
             when 0      => return Off;
             when 1      => return On;
             when others => return Unknown;
          end case;
       end Integer_To_Activation_State;

       function Real_To_Activation_State (S : String)
                                          return Activation_State is
          V : constant Float := Float'Value (S);
       begin
          if V < 0.0 then
             return Unknown;
          elsif V < 1.0 then
             return Off;
          else
             return On;
          end if;
       end Real_To_Activation_State;

    end Activation_States;

    !activation_examples.adb
    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Activation_Examples is
       S : Activation_State;
    begin
       S := "Off";
       Put_Line ("String: Off  => " & Activation_State'Image (S));

       S := 1;
       Put_Line ("Integer: 1   => " & Activation_State'Image (S));

       S := 1.5;
       Put_Line ("Real:    1.5 => " & Activation_State'Image (S));
    end Activation_Examples;

In this example, we're extending the declaration of the :ada:`Activation_State`
type to include string and real literals. For string literals, we use the
:ada:`To_Activation_State` function, which converts:

    - the :ada:`"Off"` string to :ada:`Off`,

    - the :ada:`"On"` string to :ada:`On`, and

    - any other string to :ada:`Unknown`.

For real literals, we use the :ada:`Real_To_Activation_State` function, which
converts:

    - any negative number to :ada:`Unknown`,

    - a value in the interval [0, 1) to :ada:`Off`, and

    - a value equal or above 1.0 to :ada:`On`.

Note that the string parameter of :ada:`To_Activation_State` function |mdash|
which converts string literals |mdash| is of :ada:`Wide_Wide_String` type, and
not of :ada:`String` type, as it's the case for the other conversion functions.

In the :ada:`Activation_Examples` procedure, we show how we can initialize an
object of :ada:`Activation_State` type with all kinds of literals (string,
integer and real literals).

With the definition of the :ada:`Activation_State` type that we've seen in the
complete example, we can initialize an object of this type with an enumeration
literal or a string, as both forms are defined in the type specification:

.. code:: ada manual_chop run_button main=using_string_literal.adb project=Courses.Advanced_Ada.Types.Activation_States

    !using_string_literal.adb
    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Using_String_Literal is
       S1 : constant Activation_State := On;
       S2 : constant Activation_State := "On";
    begin
       Put_Line (Activation_State'Image (S1));
       Put_Line (Activation_State'Image (S2));
    end Using_String_Literal;

Note we need to be very careful when designing conversion functions. For
example, the use of string literals may limit the kind of checks that we can
do. Consider the following misspelling of the :ada:`Off` literal:

.. code:: ada manual_chop run_button main=misspelling_example.adb project=Courses.Advanced_Ada.Types.Activation_States
    :class: ada-expect-compile-error

    !misspelling_example.adb
    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Misspelling_Example is
       S : constant Activation_State := Offf;
       --                               ^ Error: Off is misspelled.
    begin
       Put_Line (Activation_State'Image (S));
    end Misspelling_Example;

As expected, the compiler detects this error. However, this error is accepted
when using the corresponding string literal:

.. code:: ada manual_chop run_button main=misspelling_example.adb project=Courses.Advanced_Ada.Types.Activation_States

    !misspelling_example.adb
    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Misspelling_Example is
       S : constant Activation_State := "Offf";
       --                               ^ Error: Off is misspelled.
    begin
       Put_Line (Activation_State'Image (S));
    end Misspelling_Example;

Here, our implementation of :ada:`To_Activation_State` simply returns
:ada:`Unknown`. In some cases, this might be exactly the behavior that we want.
However, let's assume that we'd prefer better error handling instead. In this
case, we could change the implementation of :ada:`To_Activation_State` to check
all literals that we want to allow, and indicate an error otherwise |mdash| by
raising an exception, for example. Alternatively, we could specify this in the
preconditions of the conversion function:

.. code-block:: ada

       function To_Activation_State (S : Wide_Wide_String)
                                     return Activation_State
         with Pre => S = "Off" or S = "On" or S = "Unknown";

In this case, the precondition explicitly indicates which string literals are
allowed for the :ada:`To_Activation_State` type.

User-defined literals can also be used for more complex types, such as records.
For example:

.. code:: ada manual_chop run_button project=Courses.Advanced_Ada.Types.Record_Literals

    !silly_records.ads
    package Silly_Records is

       type Silly is record
          X : Integer;
          Y : Float;
       end record
         with String_Literal => To_Silly;

       function To_Silly (S : Wide_Wide_String) return Silly;
    end Silly_Records;

    !silly_records.adb
    package body Silly_Records is

       function To_Silly (S : Wide_Wide_String) return Silly is
       begin
          if S = "Magic" then
             return (X => 42, Y => 42.0);
          else
             return (X => 0, Y => 0.0);
          end if;
       end To_Silly;

    end Silly_Records;

    !silly_magic.adb
    with Ada.Text_IO;   use Ada.Text_IO;
    with Silly_Records; use Silly_Records;

    procedure Silly_Magic is
       R1 : Silly;
    begin
       R1 := "Magic";
       Put_Line (R1.X'Image & ", " & R1.Y'Image);
    end Silly_Magic;

In this example, when we initialize an object of :ada:`Silly` type with a
string, its components are:

- set to 42 when using the "Magic" string; or

- simply set to zero when using any other string.

Obviously, this example isn't particularly useful. However, the goal is to
show that this approach is useful for more complex types where a string literal
(or a numeric literal) might simplify handling those types. Used-defined
literals let you design types in ways that, otherwise, would only be possible
when using a preprocessor or a domain-specific language.

.. admonition:: In the Ada Reference Manual

    - :arm:`4.2.1 User-Defined Literals <4-2-1>`

