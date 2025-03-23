Types
=====

.. include:: ../../../global.txt

.. _Adv_Ada_Names:

Names
-----



..
    TO BE DONE:

    Objects
    -------

    .. todo::

        - Definition of an object in Ada
        - Difference to object-oriented programming


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

    - :arm22:`3.5 Scalar types <3-5>`

Ranges
~~~~~~

We've seen that the :ada:`First` and :ada:`Last` attributes can be used with
discrete types. Those attributes are also available for real types. Here's an
example using the :ada:`Float` type and a subtype of it:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Ranges_Real_Types

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Show_Succ_Pred_Discrete

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Succ_Pred_Discrete is
       type State is (Idle, Started,
                      Processing, Stopped);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Show_Succ_Pred_Real

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Succ_Pred_Real is
       subtype My_Float is
         Float range 0.0 .. 0.5;

       type Decimal is
         delta 0.1 digits 2
         range 0.0 .. 0.5;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Image_Value_Attr

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Width_Attr

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;

    procedure Show_Width_Attr is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length
           (Max => Integer'Width);
       use B_Str;

       Str_I : Bounded_String;

       I : constant Integer := 42;
       J : constant Integer := 103;
    begin
       Str_I := To_Bounded_String (I'Image);
       Put_Line ("Value:         "
                 & To_String (Str_I));
       Put_Line ("String Length: "
                 & Length (Str_I)'Image);
       Put_Line ("----");

       Str_I := To_Bounded_String (J'Image);
       Put_Line ("Value:         "
                 & To_String (Str_I));
       Put_Line ("String Length: "
                 & Length (Str_I)'Image);
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Base_Attr

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

.. admonition:: For further reading...

    The Ada standard defines that the minimum range of the :ada:`Integer` type
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

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Very_Big_Range
        :class: ada-expect-compile-error

        package Int_Def is

           type Too_Big_To_Fail is
             range -2 ** 255 .. 2 ** 255 - 1;

        end Int_Def;

    Otherwise, the compiler maps the named Ada type to the hardware "type",
    presumably choosing the smallest one that supports the requested range.
    (That's why the range has to be static in the source code, unlike for
    explicit subtypes.)

The following example shows how the :ada:`Base` attribute affects the bounds of
a variable:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Base_Attr

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Scalar_Types.Base_Attr_Sat

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


Enumerations
------------

We've introduced enumerations back in the
:ref:`Introduction to Ada course <Intro_Ada_Enum_Types>`.
In this section, we'll discuss a few useful features of enumerations, such as
enumeration renaming, enumeration overloading and representation clauses.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5.1 Enumeration Types <3-5-1>`

Enumerations as functions
~~~~~~~~~~~~~~~~~~~~~~~~~

If you have used programming language such as C in the past, you're familiar
with the concept of enumerations being constants with integer values. In Ada,
however, enumerations are not integers. In fact, they're actually parameterless
functions! Let's consider this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_As_Function

    package Days is

       type Day is (Mon, Tue, Wed,
                    Thu, Fri,
                    Sat, Sun);

       --  Essentially, we're declaring
       --  these functions:
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Renaming

    package Enumeration_Example is

       type Day is (Mon, Tue, Wed,
                    Thu, Fri,
                    Sat, Sun);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Renaming

    with Ada.Text_IO;         use Ada.Text_IO;
    with Enumeration_Example; use Enumeration_Example;

    procedure Show_Renaming is
       D1 : constant Day := Mon;
       D2 : constant Day := Monday;
    begin
       if D1 = D2 then
          Put_Line ("D1 = D2");
          Put_Line (Day'Image (D1)
                    & " =  "
                    & Day'Image (D2));
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

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Renaming

        package Enumeration_Example is

           type Day is (Mon, Tue, Wed,
                        Thu, Fri,
                        Sat, Sun);

        end Enumeration_Example;

        with Enumeration_Example;

        package Enumeration_Renaming is

           subtype Day is Enumeration_Example.Day;

           function Monday    return Day renames
             Enumeration_Example.Mon;
           function Tuesday   return Day renames
             Enumeration_Example.Tue;
           function Wednesday return Day renames
             Enumeration_Example.Wed;
           function Thursday  return Day renames
             Enumeration_Example.Thu;
           function Friday    return Day renames
             Enumeration_Example.Fri;
           function Saturday  return Day renames
             Enumeration_Example.Sat;
           function Sunday    return Day renames
             Enumeration_Example.Sun;

        end Enumeration_Renaming;

        with Ada.Text_IO; use Ada.Text_IO;

        with Enumeration_Renaming;
        use  Enumeration_Renaming;

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Overloading

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Overloading

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Red_Colors is
       C1 : constant Color         := Red;
       --  Using Red from Color

       C2 : constant Primary_Color := Red;
       --  Using Red from Primary_Color
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

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Overloading

    package Colors.Shades is

       subtype Blue_Shades is
         Colors range Blue .. Darkblue;

    end Colors.Shades;

In this case, :ada:`Blue` of :ada:`Blue_Shades` and :ada:`Blue` of
:ada:`Colors` are the same enumeration.

Enumeration ambiguities
^^^^^^^^^^^^^^^^^^^^^^^

A situation where enumeration overloading might lead to ambiguities is when we
use them in ranges. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Ambiguities
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
       for C in Red .. Blue loop
       --       ^^^^^^^^^^^
       --  ERROR: range is ambiguous!
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Here, it's not clear whether the range in the loop is of :ada:`Color` type or
of :ada:`Primary_Color` type. Therefore, we get a compilation error for this
code example. The next line in the code example |mdash| the one with the call
to :ada:`Put_Line` |mdash| gives us a hint about the developer's intention to
refer to the :ada:`Color` type. In this case, we can use qualification |mdash|
for example, :ada:`Color'(Red)` |mdash| to resolve the ambiguity:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Ambiguities

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Ambiguities

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
       subtype Red_To_Blue is Color range Red .. Blue;
    begin
       for C in Red_To_Blue loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;


.. _Adv_Ada_Enumeration_Position_Internal_Code:

Position and Internal Code
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Enumerations.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed,
                    Thu, Fri,
                    Sat, Sun);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D)
                    & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D)
                    & " internal code = "
                    & Integer'Image
                        (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Note that this application also displays the internal code, which, in this
case, is equivalent to the position value for all enumerations.

We may, however, change the internal code of an enumeration using a
representation clause. We discuss this topic
:ref:`in another section <Adv_Ada_Enumeration_Representation_Clauses>`.


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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types

    package Unconstrained_Types is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Simple_Record (Extended : Boolean) is
       record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

    end Unconstrained_Types;

In this example, both :ada:`Integer_Array` and :ada:`Simple_Record` are
indefinite types.

As we've just mentioned, we cannot declare variable of indefinite types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Unconstrained_Type is

       subtype Integer_Array_5 is
         Integer_Array (1 .. 5);

       A1 : Integer_Array_5;
       A2 : Integer_Array (1 .. 5);

       subtype Simple_Record_Ext is
         Simple_Record (Extended => True);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array (A : Integer_Array);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Integer_Array (A : Integer_Array)
    is
    begin
       for I in A'Range loop
          Put_Line (Positive'Image (I)
                    & ": "
                    & Integer'Image (A (I)));
       end loop;
       Put_Line ("--------");
    end Show_Integer_Array;

    with Unconstrained_Types; use Unconstrained_Types;
    with Show_Integer_Array;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  :=
                (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) :=
                (1, 2, 3, 4, 5, others => 99);
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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_Header
      (AA : Integer_Array;
       HH : String);

    with Ada.Text_IO;         use Ada.Text_IO;
    with Show_Integer_Array;

    procedure Show_Integer_Array_Header
      (AA : Integer_Array;
       HH : String)
    is
    begin
       Put_Line (HH);
       Show_Integer_Array (AA);
    end Show_Integer_Array_Header;

    with Unconstrained_Types; use Unconstrained_Types;

    with Show_Integer_Array_Header;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  :=
                (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) :=
                (1, 2, 3, 4, 5, others => 99);
    begin
       Show_Integer_Array_Header (A_5,
                                  "First example");
       Show_Integer_Array_Header (A_10,
                                  "Second example");
    end Using_Unconstrained_Type;

In this case, we're calling the :ada:`Show_Integer_Array` procedure with
another unconstrained parameter (the :ada:`AA` parameter). However, although we
could have a long *chain* of procedure calls using indefinite types in their
parameters, we still use a (definite) object at the beginning of this chain.
For example, for the :ada:`A_5` object, we have this chain:

::

    A_5

        ==> Show_Integer_Array_Header (AA => A_5,
                                       ...);

            ==> Show_Integer_Array (A => AA);

Therefore, at this specific call to :ada:`Show_Integer_Array`, even though
:ada:`A` is declared as a parameter of indefinite type, the actual argument
is of definite type because :ada:`A_5` is constrained |mdash| and, thus, of
definite type.

Note that we can declare variables based on parameters of indefinite type. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Definite_Indefinite_Subtypes.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_Plus
      (A : Integer_Array;
       V : Integer);

    with Show_Integer_Array;

    procedure Show_Integer_Array_Plus
      (A : Integer_Array;
       V : Integer)
    is
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
       A_5 : constant Integer_Array (1 .. 5) :=
               (1, 2, 3, 4, 5);
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

    - :arm22:`3.3 Objects and Named Numbers <3-3>`


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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Incomplete_Types.Incomplete_Types

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Incomplete_Types.Incomplete_Types_2

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Incomplete_Types.Linked_List_Example

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

Incomplete types are useful to declare
:ref:`mutually dependent types <Adv_Ada_Mutually_Dependent_Types>`, as we'll
see later on. Also, we can also have formal incomplete types, as
we'll discuss :ref:`later <Adv_Ada_Formal_Incomplete_Types>`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.10.1 Incomplete Type Declarations <3-10-1>`


.. _Adv_Ada_Type_View:

Type view
---------

Ada distinguishes between the partial and the full view of a type. The full
view is a type declaration that contains all the information needed by the
compiler. For example, the following declaration of type :ada:`R` represents
the full view of this type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Full_View

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Partial_Full_View

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Partial_Full_View

    with Partial_Full_Views; use Partial_Full_Views;

    procedure Main is
       --  Partial view of R indicates that
       --  R exists as a private type, so we
       --  can declare objects of this type:
       C : R;
    begin
       --  But we cannot directly access any
       --  information declared in the full
       --  view of R:
       --
       --  C.I := 42;
       --
       null;
    end Main;

In many cases, the restrictions applied to the partial and full views must
match. For example, if we declare a limited type in the full view of a private
type, its partial view must also be limited:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Limited_Private

    package Limited_Private_Example is

       --  Partial view must be limited,
       --  since the full view is limited.
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Tagged_Full_View

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

.. admonition:: In the Ada Reference Manual

    - :arm22:`7.3 Private Types and Private Extensions <7-3>`

.. _Adv_Ada_Non_Record_Private_Types:

Non-Record Private Types
~~~~~~~~~~~~~~~~~~~~~~~~

Although it's very common to declare private types as record types, this is
not the only option. In fact, we could declare any type in the full view
|mdash| scalars, for example |mdash|, so we could declare a "private
integer" type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Integer

    package Private_Integers is

       --  Partial view of private Integer type:
       type Private_Integer is private;

    private

       --  Full view of private Integer type:
       type Private_Integer is new Integer;

    end Private_Integers;

This code compiles as expected, but isn't very useful. We can improve it by
adding operators to it, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Integer

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
                     return Private_Integer
       is
          Res : constant Integer :=
                  Integer (Left) + Integer (Right);
          --  Note that we're converting Left
          --  and Right to Integer, which calls
          --  the "+" operator of the Integer
          --  type. Writing "Left + Right" would
          --  have called the "+" operator of
          --  Private_Integer, which leads to
          --  recursive calls, as this is the
          --  operator we're currently in.
       begin
          return Private_Integer (Res);
       end "+";

    end Private_Integers;

Now, let's use the new operator in a test application:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Integer

    with Private_Integers; use Private_Integers;

    procedure Show_Private_Integers is
       A, B : Private_Integer;
    begin
       A := A + B;
    end Show_Private_Integers;

In this example, we use the :ada:`+` operator as if we were adding two common
integer variables of :ada:`Integer` type.

Unconstrained Types
^^^^^^^^^^^^^^^^^^^

There are, however, some limitations: we cannot use unconstrained types such as
arrays or even discriminants for arrays in the same way as we did for scalars.
For example, the following declarations won't work:

.. code:: ada compile_button manual_chop project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Array
    :class: ada-expect-compile-error

    !private_arrays.ads
    package Private_Arrays is

       type Private_Unconstrained_Array is private;

       type Private_Constrained_Array
         (L : Positive) is private;

    private

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Private_Unconstrained_Array is
         array (Positive range <>) of Integer;

       type Private_Constrained_Array
         (L : Positive) is
           array (1 .. 2) of Integer;

       --  NOTE: using an array type fails as well:
       --
       --  type Private_Constrained_Array
       --    (L : Positive) is
       --      Integer_Array (1 .. L);

    end Private_Arrays;

Completing the private type with an unconstrained array type in the full view
is not allowed because clients could expect, according to their view, to
declare objects of the type. But doing so would not be allowed according to the
full view. So this is another case of the partial view having to present
clients with a sufficiently *true* view of the type's capabilities.

One solution is to rewrite the declaration of :ada:`Private_Constrained_Array`
using a record type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Array

    package Private_Arrays is

       type Private_Constrained_Array
         (L : Positive) is private;

    private

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Private_Constrained_Array
         (L : Positive) is
       record
          Arr : Integer_Array  (1 .. 2);
       end record;

    end Private_Arrays;

    with Private_Arrays; use Private_Arrays;

    procedure Declare_Private_Array is
      Arr : Private_Constrained_Array (5);
    begin
      null;
    end Declare_Private_Array;

Now, the code compiles fine |mdash| but we had to use a record type in the
full view to make it work.

Another solution is to make the private type indefinite. In this case, the
client's partial view would be consistent with a completion as an indefinite
type in the private part:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_View.Private_Array

    package Private_Arrays is

       type Private_Constrained_Array (<>) is
         private;

       function Init
         (L : Positive)
          return Private_Constrained_Array;

    private

       type Private_Constrained_Array is
         array (Positive range <>) of Integer;

    end Private_Arrays;

    package body Private_Arrays is

       function Init
         (L : Positive)
          return Private_Constrained_Array
       is
          PCA : Private_Constrained_Array (1 .. L);
       begin
          return PCA;
       end Init;

    end Private_Arrays;

    with Private_Arrays; use Private_Arrays;

    procedure Declare_Private_Array is
      Arr : Private_Constrained_Array := Init (5);
    begin
      null;
    end Declare_Private_Array;

The bounds for the object's declaration come from the required initial value
when an object is declared. In this case, we initialize the object with a call
to the :ada:`Init` function.


.. _Adv_Ada_Type_Conversion:

Type conversion
---------------

An important operation when dealing with objects of different types is type
conversion, which we already discussed in the
:ref:`Introduction to Ada course <Intro_Ada_Type_Conversion>`. In fact, we can
convert an object :ada:`Obj_X` of an *operand* type :ada:`X` to a similar,
closely related *target* type :ada:`Y` by simply indicating the target type:
:ada:`Y (Obj_X)`. In this section, we discuss type conversions for different
kinds of types.

Ada distinguishes between two kinds of conversion: value conversion and view
conversion. The main difference is the way how the operand (argument) of the
conversion is evaluated:

- in a value conversion, the operand is evaluated as an
  :ref:`expression <Adv_Ada_Expressions>`;

- in a view conversion, the operand is evaluated as a name.

In other words, we cannot use expressions such as :ada:`2 * A` in a view
conversion, but only :ada:`A`. In a value conversion, we could use both forms.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.6 Type Conversions <4-6>`


Value conversion
~~~~~~~~~~~~~~~~

Value conversions are possible for various types. In this section, we see some
examples, starting with types derived from scalar types up to array
conversions.

Root and derived types
^^^^^^^^^^^^^^^^^^^^^^

Let's start with the conversion between a scalar type and its derived types.
For example, we can convert back-and-forth between the :ada:`Integer` type and
the derived :ada:`Int` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Root_Derived_Type_Conversion

    package Custom_Integers is

       type Int is new Integer
         with Dynamic_Predicate => Int /= 0;

       function Double (I : Integer)
                        return Integer is
         (I * 2);

    end Custom_Integers;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Custom_Integers; use Custom_Integers;

    procedure Show_Conversion is
       Int_Var     : Int     := 1;
       Integer_Var : Integer := 2;
    begin
       --  Int to Integer conversion
       Integer_Var := Integer (Int_Var);

       Put_Line ("Integer_Var : "
                 & Integer_Var'Image);

       --  Int to Integer conversion
       --  as an actual parameter
       Integer_Var := Double (Integer (Int_Var));

       Put_Line ("Integer_Var : "
                 & Integer_Var'Image);

       --  Integer to Int conversion
       --  using an expression
       Int_Var     := Int (Integer_Var * 2);

       Put_Line ("Int_Var :     "
                 & Int_Var'Image);
    end Show_Conversion;

In the :ada:`Show_Conversion` procedure from this example, we first convert
from :ada:`Int` to :ada:`Integer`. Then, we do the same conversion while
providing the resulting value as an actual parameter for the :ada:`Double`
function. Finally, we convert the :ada:`Integer_Var * 2` expression from
:ada:`Integer` to :ada:`Int`.

Note that the converted value must conform to any constraints that the target
type might have. In the example above, :ada:`Int` has a predicate that dictates
that its value cannot be zero. This (dynamic) predicate is checked at runtime,
so an exception is raised if it fails:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Root_Derived_Type_Conversion
    :class: ada-run-expect-failure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Custom_Integers; use Custom_Integers;

    procedure Show_Conversion is
       Int_Var     : Int;
       Integer_Var : Integer;
    begin
       Integer_Var := 0;
       Int_Var     := Int (Integer_Var);

       Put_Line ("Int_Var : "
                 & Int_Var'Image);
    end Show_Conversion;

In this case, the conversion from :ada:`Integer` to :ada:`Int` fails because,
while zero is a valid integer value, it doesn't obey :ada:`Int`\'s predicate.


Numeric type conversion
^^^^^^^^^^^^^^^^^^^^^^^

A typical conversion is the one between integer and floating-point values. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Numeric_Type_Conversion

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Conversion is
       F : Float   := 1.0;
       I : Integer := 2;
    begin
       I := Integer (F);

       Put_Line ("I : "
                 & I'Image);

       I := 4;
       F := Float (I);

       Put_Line ("F :   "
                 & F'Image);
    end Show_Conversion;

Also, we can convert between fixed-point types and floating-point or integer
types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Numeric_Type_Conversion

    package Fixed_Point_Defs is
       S     : constant := 32;
       Exp   : constant := 15;
       D     : constant := 2.0 ** (-S + Exp + 1);

       type TQ15_31 is delta D
         range -1.0 * 2.0 ** Exp ..
                1.0 * 2.0 ** Exp - D;

       pragma Assert (TQ15_31'Size = S);
    end Fixed_Point_Defs;

    with Fixed_Point_Defs; use Fixed_Point_Defs;
    with Ada.Text_IO;      use Ada.Text_IO;

    procedure Show_Conversion is
       F  : Float;
       FP : TQ15_31;
       I  : Integer;
    begin
       FP := TQ15_31 (10.25);
       I  := Integer (FP);

       Put_Line ("FP : "
                 & FP'Image);
       Put_Line ("I : "
                 & I'Image);

       I  := 128;
       FP := TQ15_31 (I);
       F  := Float (FP);

       Put_Line ("FP : "
                 & FP'Image);
       Put_Line ("F :   "
                 & F'Image);
    end Show_Conversion;

As we can see in the examples above, converting between different numeric types
works in all directions. (Of course, rounding is applied when converting from
floating-point to integer types, but this is expected.)


Enumeration conversion
^^^^^^^^^^^^^^^^^^^^^^

We can also convert between an enumeration type and a type derived from it:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Enumeration_Type_Conversion

    package Custom_Enumerations is

       type Priority is (Low, Mid, High);

       type Important_Priority is new
         Priority range Mid .. High;

    end Custom_Enumerations;

    with Ada.Text_IO;         use Ada.Text_IO;
    with Custom_Enumerations; use Custom_Enumerations;

    procedure Show_Conversion is
       P  : Priority           := Low;
       IP : Important_Priority := High;
    begin
       P := Priority (IP);

       Put_Line ("P:  "
                 & P'Image);

       P  := Mid;
       IP := Important_Priority (P);

       Put_Line ("IP: "
                 & IP'Image);
    end Show_Conversion;

In this example, we have the :ada:`Priority` type and the derived type
:ada:`Important_Priority`. As expected, the conversion works fine when the
converted value is in the range of the target type. If not, an exception is
raised:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Enumeration_Type_Conversion
    :class: ada-run-expect-failure

    with Ada.Text_IO;         use Ada.Text_IO;
    with Custom_Enumerations; use Custom_Enumerations;

    procedure Show_Conversion is
       P  : Priority;
       IP : Important_Priority;
    begin
       P  := Low;
       IP := Important_Priority (P);

       Put_Line ("IP: "
                 & IP'Image);
    end Show_Conversion;

In this example, an exception is raised because :ada:`Low` is not in the :ada:`Important_Priority` type's range.


Array conversion
^^^^^^^^^^^^^^^^

Similarly, we can convert between array types. For example, if we have the
array type :ada:`Integer_Array` and its derived type
:ada:`Derived_Integer_Array`, we can convert between those array types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Array_Type_Conversion switches=Compiler(-gnat2022);

    package Custom_Arrays is

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Derived_Integer_Array is new
         Integer_Array;

    end Custom_Arrays;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Custom_Arrays; use Custom_Arrays;

    procedure Show_Conversion is
       subtype Common_Range is Positive range 1 .. 3;

       AI : Integer_Array (Common_Range);
       AI_D : Derived_Integer_Array (Common_Range);
    begin
       AI_D := [1, 2, 3];
       AI := Integer_Array (AI_D);

       Put_Line ("AI: "
                 & AI'Image);

       AI   := [4, 5, 6];
       AI_D := Derived_Integer_Array (AI);

       Put_Line ("AI_D: "
                 & AI_D'Image);
    end Show_Conversion;

Note that both arrays must have the same number of components in order for the
conversion to be successful. (Sliding is fine, though.) In this example, both
arrays have the same range: :ada:`Common_Range`.

We can also convert between array types that aren't derived one from the
other. As long as the components and the index subtypes are of the same type,
the conversion between those types is possible. To be more precise, these are
the requirements for the array conversion to be accepted:

- The component types must be the same type.

- The index types (or subtypes) must be the same or, at least, convertible.

- The dimensionality of the arrays must be the same.

- The bounds must be compatible (but not necessarily equal).

Converting between different array types can be very handy, especially when
we're dealing with array types that were not declared in the same package. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Array_Type_Conversion switches=Compiler(-gnat2022);

    package Custom_Arrays_1 is

       type Integer_Array_1 is
         array (Positive range <>) of Integer;

       type Float_Array_1 is
         array (Positive range <>) of Float;

    end Custom_Arrays_1;

    package Custom_Arrays_2 is

       type Integer_Array_2 is
         array (Positive range <>) of Integer;

       type Float_Array_2 is
         array (Positive range <>) of Float;

    end Custom_Arrays_2;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Custom_Arrays_1; use Custom_Arrays_1;
    with Custom_Arrays_2; use Custom_Arrays_2;

    procedure Show_Conversion is
       subtype Common_Range is Positive range 1 .. 3;

       AI_1 : Integer_Array_1 (Common_Range);
       AI_2 : Integer_Array_2 (Common_Range);
       AF_1 : Float_Array_1 (Common_Range);
       AF_2 : Float_Array_2 (Common_Range);
    begin
       AI_2 := [1, 2, 3];
       AI_1 := Integer_Array_1 (AI_2);

       Put_Line ("AI_1: "
                 & AI_1'Image);

       AI_1 := [4, 5, 6];
       AI_2 := Integer_Array_2 (AI_1);

       Put_Line ("AI_2: "
                 & AI_2'Image);

       --  ERROR: Cannot convert arrays whose
       --         components have different types:
       --
       --  AF_1 := Float_Array_1 (AI_1);
       --
       --  Instead, use array aggregate where each
       --  component is converted from integer to
       --  float:
       --
       AF_1 := [for I in AF_1'Range =>
                  Float (AI_1 (I))];

       Put_Line ("AF_1: "
                 & AF_1'Image);

       AF_2 := Float_Array_2 (AF_1);

       Put_Line ("AF_2: "
                 & AF_2'Image);
    end Show_Conversion;

As we can see in this example, the fact that :ada:`Integer_Array_1` and
:ada:`Integer_Array_2` have the same component type (:ada:`Integer`) allows us
to convert between them. The same applies to the :ada:`Float_Array_1` and
:ada:`Float_Array_2` types.

A conversion is not possible when the component types don't match. Even though
we can convert between integer and floating-point types, we cannot convert an
array of integers to an array of floating-point directly. Therefore, we cannot
write a statement such as :ada:`AF_1 := Float_Array_1 (AI_1);`.

However, when the components don't match, we can of course implement the array
conversion by converting the individual components. For the example above, we
used an iterated component association in an array aggregate:
:ada:`[for I in AF_1'Range => Float (AI_1 (I))];`. (We discuss this topic later
:ref:`in another chapter <Adv_Ada_Array_Aggregates>`.)

We may also encounter array types originating from the instantiation of generic
packages. In this case as well, we can use array conversions. Consider the
following generic package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Generic_Array_Type_Conversion switches=Compiler(-gnat2022);

    generic
       type T is private;
    package Custom_Arrays is
       type T_Array is
         array (Positive range <>) of T;
    end Custom_Arrays;

We could instantiate this generic package and reuse parts of the previous code
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Generic_Array_Type_Conversion switches=Compiler(-gnat2022);

    with Ada.Text_IO;   use Ada.Text_IO;
    with Custom_Arrays;

    procedure Show_Conversion is
       package CA_Int_1 is
         new Custom_Arrays (T => Integer);
       package CA_Int_2 is
         new Custom_Arrays (T => Integer);

       subtype Common_Range is Positive range 1 .. 3;

       AI_1 : CA_Int_1.T_Array (Common_Range);
       AI_2 : CA_Int_2.T_Array (Common_Range);
    begin
       AI_2 := [1, 2, 3];
       AI_1 := CA_Int_1.T_Array (AI_2);

       Put_Line ("AI_1: "
                 & AI_1'Image);

       AI_1 := [4, 5, 6];
       AI_2 := CA_Int_2.T_Array (AI_1);

       Put_Line ("AI_2: "
                 & AI_2'Image);
    end Show_Conversion;

As we can see in this example, each of the instantiated :ada:`CA_Int_1` and
:ada:`CA_Int_2` packages has a :ada:`T_Array` type. Even though these
:ada:`T_Array` types have the same name, they're actually completely unrelated
types. However, we can still convert between them in the same way as we did in
the previous code examples.


.. _Adv_Ada_View_Conversion:

View conversion
~~~~~~~~~~~~~~~

As mentioned before, view conversions just allow names to be converted. Thus,
we cannot use expressions in this case.

Note that a view conversion never changes the value during the conversion. We
could say that a view conversion is simply making us *view* an object from a
different angle. The object itself is still the same for both the original and
the target types.

For example, consider this package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Tagged_Types_View

    package Some_Tagged_Types is

       type T is tagged record
          A : Integer;
       end record;

       type T_Derived is new T with record
          B : Float;
       end record;

       Obj : T_Derived;

    end Some_Tagged_Types;

Here, :ada:`Obj` is an object of type :ada:`T_Derived`. When we *view* this
object, we notice that it has two components: :ada:`A` and :ada:`B`. However,
we could *view* this object as being of type :ada:`T`. From that perspective,
this object only has one component: :ada:`A`. (Note that changing the
perspective doesn't change the object itself.) Therefore, a view conversion
from :ada:`T_Derived` to :ada:`T` just makes us *view* the object :ada:`Obj`
from a different angle.

In this sense, a view conversion changes the view of a given object to the
target type's view, both in terms of components that exist and operations that
are available. It doesn't really change anything at all in the value itself.

There are basically two kinds of view conversions: the ones using tagged types
and the ones using untagged types. We discuss these kinds of conversion in this
section.

View conversion of tagged types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A conversion between tagged types is a view conversion. Let's consider a
typical code example that declares one, two and three-dimensional points:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Tagged_Type_Conversion

    package Points is

       type Point_1D is tagged record
          X : Float;
       end record;

       procedure Display (P : Point_1D);

       type Point_2D is new Point_1D with record
          Y : Float;
       end record;

       procedure Display (P : Point_2D);

       type Point_3D is new Point_2D with record
          Z : Float;
       end record;

       procedure Display (P : Point_3D);

    end Points;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Points is

       procedure Display (P : Point_1D) is
       begin
          Put_Line ("(X => " & P.X'Image & ")");
       end Display;

       procedure Display (P : Point_2D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image & ")");
       end Display;

       procedure Display (P : Point_3D) is
       begin
          Put_Line ("(X => " & P.X'Image
                    & ", Y => " & P.Y'Image
                    & ", Z => " & P.Z'Image & ")");
       end Display;

    end Points;

We can use the types from the :ada:`Points` package and convert between each
other:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Tagged_Type_Conversion

    with Ada.Text_IO; use Ada.Text_IO;
    with Points;      use Points;

    procedure Show_Conversion is
       P_1D : Point_1D;
       P_3D : Point_3D;
    begin
       P_3D := (X => 0.1, Y => 0.5, Z => 0.3);
       P_1D := Point_1D (P_3D);

       Put ("P_3D : ");
       Display (P_3D);

       Put ("P_1D : ");
       Display (P_1D);
    end Show_Conversion;

In this example, as expected, we're able to convert from the :ada:`Point_3D`
type (which has three components) to the :ada:`Point_1D` type, which has only
one component.


View conversion of untagged types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For untagged types, a view conversion is the one that happens when we have an
object of an untagged type as an actual parameter for a formal :ada:`in out`
or :ada:`out` parameter.

Let's see a code example. Consider the following simple procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Untagged_Type_View_Conversion

    procedure Double (X : in out Float);

    procedure Double (X : in out Float) is
    begin
       X := X * 2.0;
    end Double;

The :ada:`Double` procedure has an :ada:`in out` parameter of :ada:`Float`
type. We can call this procedure using an integer variable :ada:`I` as the
actual parameter. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Untagged_Type_View_Conversion

    with Ada.Text_IO; use Ada.Text_IO;
    with Double;

    procedure Show_Conversion is
       I : Integer;
    begin
       I := 2;
       Put_Line ("I : "
                 & I'Image);

       --  Calling Double with
       --  Integer parameter:
       Double (Float (I));
       Put_Line ("I : "
                 & I'Image);
    end Show_Conversion;

In this case, the :ada:`Float (I)` conversion in the call to :ada:`Double`
creates a temporary floating-point variable. This is the same as if we had
written the following code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Untagged_Type_View_Conversion

    with Ada.Text_IO; use Ada.Text_IO;
    with Double;

    procedure Show_Conversion is
       I : Integer;
    begin
       I := 2;
       Put_Line ("I : "
                 & I'Image);

       declare
          F : Float := Float (I);
       begin
          Double (F);
          I := Integer (F);
       end;
       Put_Line ("I : "
                 & I'Image);
    end Show_Conversion;

In this sense, the view conversion that happens in :ada:`Double (Float (I))`
can be considered syntactic sugar, as it allows us to elegantly write two
conversions in a single statement.


Implicit conversions
~~~~~~~~~~~~~~~~~~~~

Implicit conversions are only possible when we have a type :ada:`T` and a
subtype :ada:`S` related to the :ada:`T` type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Implicit_Subtype_Conversion

    package Custom_Integers is

       type Int is new Integer
         with Dynamic_Predicate => Int /= 0;

       subtype Sub_Int_1 is Integer
         with Dynamic_Predicate => Sub_Int_1 /= 0;

       subtype Sub_Int_2 is Sub_Int_1
         with Dynamic_Predicate => Sub_Int_2 /= 1;

    end Custom_Integers;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Custom_Integers; use Custom_Integers;

    procedure Show_Conversion is
       Int_Var       : Int;
       Sub_Int_1_Var : Sub_Int_1;
       Sub_Int_2_Var : Sub_Int_2;
       Integer_Var   : Integer;
    begin
       Integer_Var := 5;
       Int_Var     := Int (Integer_Var);

       Put_Line ("Int_Var :       "
                 & Int_Var'Image);

       --  Implicit conversions:
       --  no explicit conversion required!
       Sub_Int_1_Var := Integer_Var;
       Sub_Int_2_Var := Integer_Var;

       Put_Line ("Sub_Int_1_Var : "
                 & Sub_Int_1_Var'Image);
       Put_Line ("Sub_Int_2_Var : "
                 & Sub_Int_2_Var'Image);
    end Show_Conversion;

In this example, we declare the :ada:`Int` type and the :ada:`Sub_Int_1` and
:ada:`Sub_Int_2` subtypes:

- the :ada:`Int` type is derived from the :ada:`Integer` type,

- :ada:`Sub_Int_1` is a subtype of the :ada:`Integer` type, and

- :ada:`Sub_Int_2` is a subtype of the :ada:`Sub_Int_1` subtype.

We need an explicit conversion when converting between the :ada:`Integer` and
:ada:`Int` types. However, as the conversion is implicit for subtypes, we can
simply write :ada:`Sub_Int_1_Var := Integer_Var;`. (Of course, writing the
explicit conversion :ada:`Sub_Int_1 (Integer_Var)` in the assignment is
possible as well.) Also, the same applies to the :ada:`Sub_Int_2` subtype: we
can write an implicit conversion in the :ada:`Sub_Int_2_Var := Integer_Var;`
statement.


Conversion of other types
~~~~~~~~~~~~~~~~~~~~~~~~~

For other kinds of types, such as records, a direct conversion as we've seen so
far isn't possible. In this case, we have to write a conversion function
ourselves. A common convention in Ada is to name this function
:ada:`To_Typename`. For example, if we want to convert from any type to
:ada:`Integer` or :ada:`Float`, we implement the :ada:`To_Integer` and
:ada:`To_Float` functions, respectively. (Obviously, because Ada supports
subprogram overloading, we can have multiple :ada:`To_Typename` functions for
different operand types.)

Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Other_Type_Conversions

    package Custom_Rec is

       type Rec is record
          X : Integer;
       end record;

       function To_Integer (R : Rec)
                            return Integer is
         (R.X);

    end Custom_Rec;

    with Ada.Text_IO; use Ada.Text_IO;
    with Custom_Rec;  use Custom_Rec;

    procedure Show_Conversion is
       R : Rec;
       I : Integer;
    begin
       R := (X => 2);
       I := To_Integer (R);

       Put_Line ("I : " & I'Image);
    end Show_Conversion;

In this example, we have the :ada:`To_Integer` function that converts from the
:ada:`Rec` type to the :ada:`Integer` type.

.. admonition:: In other languages

    In C++, you can define conversion operators to cast between objects of
    different classes. Also, you can overload the :cpp:`=` operator.
    Consider this example:

    .. code-block:: cpp

        #include <iostream>

        class T1 {
        public:
            T1 (float x) :
              x(x) {}

            // If class T3 is declared before class
            // T1, we can overload the "=" operator.
            //
            // void operator=(T3 v) {
            //     x = static_cast<float>(v);
            // }

            void display();
        private:
           float x;
        };

        class T3 {
        public:
            T3 (float x, float y, float z) :
              x(x), y(y), z(z) {}

            // implicit conversion
            operator float() const {
                return (x + y + z) / 3.0;
            }

            // implicit conversion
            //
            // operator T1() const {
            //     return T1((x + y + z) / 3.0);
            // }

            // explicit conversion (C++11)
            explicit operator T1() const {
                return T1(float(*this));
            }

            void display();

        private:
            float x, y, z;
        };

        void T1::display()
        {
            std::cout << "(x => " << x
                      << ")" << std::endl;
        }

        void T3::display()
        {
            std::cout << "(x => " << x
                      << "y => "  << y
                      << "z => "  << z
                      << ")" << std::endl;
        }

        int main ()
        {
            const T3 t_3 (0.5, 0.4, 0.6);
            T1 t_1 (0.0);
            float f;

            // Implicit conversion
            f = t_3;

            std::cout << "f : " << f
                      << std::endl;

            // Explicit conversion
            f = static_cast<float>(t_3);

            // f = (float)t_3;

            std::cout << "f : " << f
                      << std::endl;

            // Explicit conversion
            t_1 = static_cast<T1>(t_3);

            // t_1 = (T1)t_3;

            std::cout << "t_1 : ";
            t_1.display();
            std::cout << std::endl;
        }

    Here, we're using :cpp:`operator float()` and :cpp:`operator T1()` to
    cast from an object of class :cpp:`T3` to a floating-point value and an
    object of class :cpp:`T1`, respectively. (If we switch the order and
    declare the :cpp:`T3` class before the :cpp:`T1` class, we could overload
    the :cpp:`=` operator, as you can see in the commented-out lines.)

    In Ada, this kind of conversions isn't available. Instead, we have to
    implement conversion functions such as the :ada:`To_Integer` function from
    the previous code example. This is the corresponding implementation:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Type_Conversion.Explicit_Rec_Conversion

        package Custom_Defs is

           type T1 is private;

           function Init (X : Float)
                          return T1;

           procedure Display (Obj : T1);

           type T3 is private;

           function Init (X, Y, Z : Float)
                          return T3;

           function To_Float (Obj : T3)
                              return Float;

           function To_T1 (Obj : T3)
                           return T1;

           procedure Display (Obj : T3);

        private
           type T1 is record
              X : Float;
           end record;

           function Init (X : Float)
                          return T1 is
             (X => X);

           type T3 is record
              X, Y, Z : Float;
           end record;

           function Init (X, Y, Z : Float)
                          return T3 is
             (X => X, Y => Y, Z => Z);

        end Custom_Defs;

        with Ada.Text_IO; use Ada.Text_IO;

        package body Custom_Defs is

           procedure Display (Obj : T1) is
           begin
              Put_Line ("(X => "
                        & Obj.X'Image & ")");
           end Display;

           function To_Float (Obj : T3)
                              return Float is
             ((Obj.X + Obj.Y + Obj.Z) / 3.0);

           function To_T1 (Obj : T3)
                           return T1 is
             (Init (To_Float (Obj)));

           procedure Display (Obj : T3) is
           begin
              Put_Line ("(X => "    & Obj.X'Image
                        & ", Y => " & Obj.Y'Image
                        & ", Z => " & Obj.Z'Image
                        & ")");
           end Display;

        end Custom_Defs;

        with Ada.Text_IO; use Ada.Text_IO;
        with Custom_Defs; use Custom_Defs;

        procedure Show_Conversion is
           T_3 : constant T3 := Init (0.5, 0.4, 0.6);
           T_1 :          T1 := Init (0.0);
           F   : Float;
        begin
           --  Explicit conversion from
           --  T3 to Float type
           F := To_Float (T_3);

           Put_Line ("F : " & F'Image);

           --  Explicit conversion from
           --  T3 to T1 type
           T_1 := To_T1 (T_3);

           Put ("T_1 : ");
           Display (T_1);
        end Show_Conversion;

    In this example, we *translate* the casting operators from the C++ version
    by implementing the :ada:`To_Float` and :ada:`To_T1` functions.
    (In addition to that, we replace the C++ constructors by :ada:`Init`
    functions.)


.. _Adv_Ada_Qualified_Expressions:

Qualified Expressions
---------------------

We already saw qualified expressions in the
:ref:`Introduction to Ada <Intro_Ada_Qualified_Expressions>` course. As
mentioned there, a qualified expression specifies the exact type or subtype
that the target expression will be resolved to, and it can be either any
expression in parentheses, or an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Qualified_Expressions.Example

    package Simple_Integers is

       type Int is new Integer;

       subtype Int_Not_Zero is Int
         with Dynamic_Predicate => Int_Not_Zero /= 0;

    end Simple_Integers;

    with Simple_Integers; use Simple_Integers;

    procedure Show_Qualified_Expressions is
       I : Int;
    begin
       --  Using qualified expression Int'(N)
       I := Int'(0);
    end Show_Qualified_Expressions;

Here, the qualified expression :ada:`Int'(0)` indicates that the value zero is
of :ada:`Int` type.

.. admonition:: In the Ada Reference Manual

    - :arm22:`4.7 Qualified Expressions <4-7>`


Verifying subtypes
~~~~~~~~~~~~~~~~~~

.. note::

   This feature was introduced in Ada 2022.

We can use qualified expressions to verify a subtype's predicate:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Qualified_Expressions.Example
    :class: ada-run-expect-failure

    with Simple_Integers; use Simple_Integers;

    procedure Show_Qualified_Expressions is
       I : Int;
    begin
       I := Int_Not_Zero'(0);
    end Show_Qualified_Expressions;

Here, the qualified expression :ada:`Int_Not_Zero'(0)` checks the dynamic
predicate of the subtype. (This predicate check fails at runtime.)


.. _Adv_Ada_Default_Initial_Values:

Default initial values
----------------------

In the
:ref:`Introduction to Ada course <Intro_Ada_Record_Default_Values>`,
we've seen that record components can have default values. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults_1

    package Defaults is

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

    end Defaults;

In this section, we'll extend the concept of default values to other kinds of
type declarations, such as scalar types and arrays.

.. _Adv_Ada_Default_Value:

To assign a default value for a scalar type declaration |mdash| such as an
enumeration and a new integer |mdash|, we use the :ada:`Default_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults_2

    package Defaults is

       type E is (E1, E2, E3)
         with Default_Value => E1;

       type T is new Integer
         with Default_Value => -1;

    end Defaults;

Note that we cannot specify a default value for a subtype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults_3
    :class: ada-expect-compile-error

    package Defaults is

       subtype T is Integer
         with Default_Value => -1;
       --  ERROR!!

    end Defaults;

.. _Adv_Ada_Default_Component_Value:

For array types, we use the :ada:`Default_Component_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults_4

    package Defaults is

       type Arr is
         array (Positive range <>) of Integer
           with Default_Component_Value => -1;

    end Defaults;

This is a package containing the declarations we've just seen:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults

    package Defaults is

       type E is (E1, E2, E3)
         with Default_Value => E1;

       type T is new Integer
         with Default_Value => -1;

       --  We cannot specify default
       --  values for subtypes:
       --
       --  subtype T is Integer
       --    with Default_Value => -1;

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

       type Arr is
         array (Positive range <>) of Integer
           with Default_Component_Value => -1;

    end Defaults;

In the example below, we declare variables of the types from the
:ada:`Defaults` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.Default_Initial_Values.Defaults

    with Ada.Text_IO; use Ada.Text_IO;
    with Defaults; use Defaults;

    procedure Use_Defaults is
       E1 : E;
       T1 : T;
       R1 : R;
       A1 : Arr (1 .. 5);
    begin
       Put_Line ("Enumeration:  "
                 & E'Image (E1));
       Put_Line ("Integer type: "
                 & T'Image (T1));
       Put_Line ("Record type:  "
                 & Positive'Image (R1.X)
                 & ", "
                 & Positive'Image (R1.Y));

       Put ("Array type:   ");
       for V of A1 loop
          Put (Integer'Image (V) & " ");
       end loop;
       New_Line;
    end Use_Defaults;

As we see in the :ada:`Use_Defaults` procedure, all variables still have their
default values, since we haven't assigned any value to them.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.5 Scalar Types <3-5>`
    - :arm22:`3.6 Array Types <3-6>`


Deferred Constants
------------------

Deferred constants are declarations where the value of the constant is not
specified immediately, but rather *deferred* to a later point. In that sense,
if a constant declaration is deferred, it is actually declared twice:

1. in the deferred constant declaration, and
2. in the full constant declaration.

The simplest form of deferred constant is the one that has a full constant
declaration in the private part of the package specification. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Deferred_Constants.Deferred_Constant_Private

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

.. code:: c no_button manual_chop project=Courses.Advanced_Ada.Data_Types.Types.Deferred_Constants.Deferred_Constant_C
    :class: ada-syntax-only

    !constants.c
    double light = 299792458.0;

Then, we can import this constant in the :ada:`Deferred_Constants` package:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Types.Deferred_Constants.Deferred_Constant_C
    :class: ada-syntax-only

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed with
         Import, Convention => C;
       --  ^^^^ deferred constant
       --       declaration; imported
       --       from C file

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Deferred_Constants.Deferred_Constant_Subtype

    package Deferred_Constants is

       type Speed is new Long_Float;

       subtype Positive_Speed is
         Speed range 0.0 .. Speed'Last;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       Light : constant Positive_Speed :=
                 299_792_458.0;
       --      ^ full constant declaration
       --        using a subtype

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.Deferred_Constants.Deferred_Constant_Function

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       function Calculate_Light return Speed is
         (299_792_458.0);

       Light : constant Speed := Calculate_Light;
       --      ^ full constant declaration
       --        calling a private function

    end Deferred_Constants;

Here, we call the :ada:`Calculate_Light` function |mdash| declared in the
private part of the :ada:`Deferred_Constants` package |mdash| for the full
declaration of the :ada:`Light` constant.

.. admonition:: In the Ada Reference Manual

    - :arm22:`7.4 Deferred Constants <7-4>`


.. _Adv_Ada_User_Defined_Literals:

User-defined literals
---------------------

.. note::

   This feature was introduced in Ada 2022.

Any type definition has a kind of literal associated with it. For example,
integer types are associated with integer literals. Therefore, we can
initialize an object of integer type with an integer literal:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Simple_Integer_Literal

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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Simple_Enumeration

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

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.User_Defined_Literals switches=Compiler(-gnat2022);

    package Activation_States is

       type Activation_State is (Unknown, Off, On)
         with Integer_Literal =>
                Integer_To_Activation_State;

       function Integer_To_Activation_State
         (S : String)
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.User_Defined_Literals switches=Compiler(-gnat2022);

    package body Activation_States is

       function Integer_To_Activation_State
         (S : String)
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

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Activation_States switches=Compiler(-gnat2022);

    package Activation_States is

       type Activation_State is (Unknown, Off, On)
         with String_Literal  =>
                To_Activation_State,
              Integer_Literal =>
                Integer_To_Activation_State,
              Real_Literal    =>
                Real_To_Activation_State;

       function To_Activation_State
         (S : Wide_Wide_String)
          return Activation_State;

       function Integer_To_Activation_State
         (S : String)
          return Activation_State;

       function Real_To_Activation_State
         (S : String)
          return Activation_State;

    end Activation_States;

    package body Activation_States is

       function To_Activation_State
         (S : Wide_Wide_String)
          return Activation_State
       is
       begin
          if S = "Off" then
             return Off;
          elsif S = "On" then
             return On;
          else
             return Unknown;
          end if;
       end To_Activation_State;

       function Integer_To_Activation_State
         (S : String)
          return Activation_State
       is
       begin
          case Integer'Value (S) is
             when 0      => return Off;
             when 1      => return On;
             when others => return Unknown;
          end case;
       end Integer_To_Activation_State;

       function Real_To_Activation_State
         (S : String)
          return Activation_State
       is
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

    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Activation_Examples is
       S : Activation_State;
    begin
       S := "Off";
       Put_Line ("String: Off  => "
                 & Activation_State'Image (S));

       S := 1;
       Put_Line ("Integer: 1   => "
                 & Activation_State'Image (S));

       S := 1.5;
       Put_Line ("Real:    1.5 => "
                 & Activation_State'Image (S));
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

.. code:: ada run_button main=using_string_literal.adb project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Activation_States switches=Compiler(-gnat2022);

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

.. code:: ada run_button main=misspelling_example.adb project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Activation_States switches=Compiler(-gnat2022);
    :class: ada-expect-compile-error

    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Misspelling_Example is
       S : constant Activation_State :=
             Offf;
       --    ^ Error: Off is misspelled.
    begin
       Put_Line (Activation_State'Image (S));
    end Misspelling_Example;

As expected, the compiler detects this error. However, this error is accepted
when using the corresponding string literal:

.. code:: ada run_button main=misspelling_example.adb project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Activation_States switches=Compiler(-gnat2022);

    with Ada.Text_IO;       use Ada.Text_IO;
    with Activation_States; use Activation_States;

    procedure Misspelling_Example is
       S : constant Activation_State :=
             "Offf";
       --     ^ Error: Off is misspelled.
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

       function To_Activation_State
         (S : Wide_Wide_String)
          return Activation_State
            with Pre => S = "Off"  or
                        S = "On"   or
                        S = "Unknown";

In this case, the precondition explicitly indicates which string literals are
allowed for the :ada:`To_Activation_State` type.

User-defined literals can also be used for more complex types, such as records.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Types.User_Defined_Literals.Record_Literals switches=Compiler(-gnat2022);

    package Silly_Records is

       type Silly is record
          X : Integer;
          Y : Float;
       end record
         with String_Literal => To_Silly;

       function To_Silly (S : Wide_Wide_String)
                          return Silly;
    end Silly_Records;

    package body Silly_Records is

       function To_Silly (S : Wide_Wide_String)
                          return Silly
       is
       begin
          if S = "Magic" then
             return (X => 42, Y => 42.0);
          else
             return (X => 0, Y => 0.0);
          end if;
       end To_Silly;

    end Silly_Records;

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

    - :arm22:`4.2.1 User-Defined Literals <4-2-1>`

