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

We already discussed most of these attributes in the
:doc:`Introduction to Ada course <courses/intro-to-ada/index>`. In this
section, we'll discuss some aspects that have been left out of the previous
course.

Ranges
~~~~~~

We've seen that the :ada:`First` and :ada:`Last` attributes can be used with
discrete types. Those attributes are also available for real types. Here's an
example using the :ada:`Float` type and a subtype of it:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Ranges_Real_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_First_Last_Real is
       subtype Norm is Float range 0.0 .. 1.0;

       N : Norm;
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
and the previous or next values is 1.40130E-45 (or :math:`2^{-149}`) on a
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
:ref:`another section <Wide_Wide_Strings>`.

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

Base
~~~~

The :ada:`Base` attribute gives us the unconstrained version of a subtype. As
an example, let's say we declared a subtype of the :ada:`Integer` type named
:ada:`One_To_Ten`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Base_Attr

    package My_Integers is

       subtype One_To_Ten is Integer range 1 .. 10;

    end My_Integers;

If we then use the :ada:`Base` attribute |mdash| by writing
:ada:`One_To_Ten'Base` |mdash|, we're actually referring to the
:ada:`Integer` type (because the :ada:`Integer` type was used as the base
subtype of the :ada:`One_To_Ten` subtype).

The following example shows how the attribute affects the bounds of a variable:

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
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/strongly_typed_language>`.
In this section, we'll discuss a few useful features of enumerations, such as
enumeration renaming, enumeration overloading and representation clauses.

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

.. _Definite_Indefinite_Subtypes:

Definite and Indefinite Subtypes
--------------------------------

Indefinite types were mentioned back in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/more_about_types>`.
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
:ref:`formal parameters <Formal_Definite_Indefinite_Subtypes>`.

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
we'll discuss :ref:`later <Formal_Incomplete_Types>`.

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
here, or simply use an anonymous access to :ada:`T2`. By doing this, even
though the compiler doesn't know the size of :ada:`T2`, it knows the
size of an access type designating :ada:`T2`, so the record component
can be of such an access type (anonymous or not)."

To summarize, in order to solve the compilation error above, we need to:

- use at least one incomplete type;
- declare at least one component as an access to an object.

For example, we could declare an incomplete type :ada:`T2` and then declare
the component :ada:`B` of the :ada:`T1` record as an access to :ada:`T2`.
This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent

    package Mutually_Dependent is

       type T2;

       type T1 is record
          B : access T2;
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
       type T2;

       type T1 is record
          B : access T2;
       end record;

       type T2 is record
          A : access T1;
       end record;

    end Mutually_Dependent;

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

Default initial values
----------------------

In the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/records>`,
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


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Stable Properties of a Type
    ---------------------------

    .. admonition:: Relevant topics

        - `Stable Properties of a Type <http://www.ada-auth.org/standards/2xrm/html/RM-7-3-4.html>`_


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

    - a value in the interval :math:`[0, 1)` to :ada:`Off`, and

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


Data Representation
-------------------

This section provides a glimpse on attributes and aspects used for data
representation. They are usually used for embedded applications because of
strict requirements that are often found there. Therefore, unless you have
very specific requirements for your application, in most cases, you won't need
them. However, you should at least have a rudimentary understanding of them.
To read a thorough overview on this topic, please refer to the
*Introduction to Embedded Systems Programming* course.

.. todo::

    Add link once available:

    ``Introduction to Embedded Systems Programming <courses/intro-to-embedded-sys-prog/low_level_programming>``

Sizes
~~~~~

Ada offers multiple attributes to retrieve the size of a type or an object:

+-----------------------+-----------------------------------------------------+
| Attribute             | Description                                         |
+=======================+=====================================================+
| :ada:`Size`           | Size of the representation of a subtype or an       |
|                       | object.                                             |
+-----------------------+-----------------------------------------------------+
| :ada:`Object_Size`    | Size of a component or an aliased object.           |
|                       |                                                     |
+-----------------------+-----------------------------------------------------+
| :ada:`Component_Size` | Size of a component of an array.                    |
+-----------------------+-----------------------------------------------------+
| :ada:`Storage_Size`   | Number of storage elements reserved for an access   |
|                       | type or a task object.                              |
+-----------------------+-----------------------------------------------------+

For the first three attributes, the size is measured in bits. In the case of
:ada:`Storage_Size`, the size is measured in storage elements. Note that the
size information depends your target architecture. We'll discuss some examples
to better understand the differences among those attributes.

Size attribute and aspect
^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with a code example using the :ada:`Size` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_S32 is range 0 .. 127
         with Size => 32;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       V1 : UInt_7;
       V2 : UInt_7_S32;
    begin
       Put_Line ("UInt_7'Size:            " & UInt_7'Size'Image);
       Put_Line ("UInt_7'Object_Size:     " & UInt_7'Object_Size'Image);
       Put_Line ("V1'Size:                " & V1'Size'Image);
       New_Line;

       Put_Line ("UInt_7_S32'Size:        " & UInt_7_S32'Size'Image);
       Put_Line ("UInt_7_S32'Object_Size: " & UInt_7_S32'Object_Size'Image);
       Put_Line ("V2'Size:                " & V2'Size'Image);
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7'Size:             7
    UInt_7'Object_Size:      8
    V1'Size:                 8

    UInt_7_S32'Size:         32
    UInt_7_S32'Object_Size:  32
    V2'Size:                 32

When we use the :ada:`Size` attribute for a type :ada:`T`, we're retrieving the
minimum number of bits necessary to represent objects of that type. Note that
this is not the same as the actual size of an object of type :ada:`T` because
the compiler will select an object size that is appropriate for the target
architecture.

In the example above, the size of the :ada:`UInt_7` is 7 bits, while the most
appropriate size to store objects of this type in the memory of our target
architecture is 8 bits. To be more specific, the range of :ada:`UInt_7`
(0 .. 127) can be perfectly represented in 7 bits. However, most target
architectures don't offer 7-bit registers or 7-bit memory storage, so 8 bits is
the most appropriate size in this case.

We can retrieve the size of an object of type :ada:`T` by using the
:ada:`Object_Size`. Alternatively, we can use the :ada:`Size` attribute
directly on objects of type :ada:`T` to retrieve their actual size |mdash| in
our example, we write :ada:`V1'Size` to retrieve the size of :ada:`V1`.

In the example above, we've used both the :ada:`Size` attribute (for example,
:ada:`UInt_7'Size`) and the :ada:`Size` aspect (:ada:`with Size => 32`).
While the size attribute is a function that returns the size, the size aspect
is a request to the compiler to verify that the expected size can be used on
the target platform. You can think of this attribute as a dialog between the
developer and the compiler:

    (Developer) "I think that :ada:`UInt_7_S32` should be stored using at
    least 32 bits. Do you agree?"

    (Ada compiler) "For the target platform that you selected, I can confirm
    that this is indeed the case."

Depending on the target platform, however, the conversation might play out like
this:

    (Developer) "I think that :ada:`UInt_7_S32` should be stored using at
    least 32 bits. Do you agree?"

    (Ada compiler) "For the target platform that you selected, I cannot
    possibly do it! COMPILATION ERROR!"

Component size
^^^^^^^^^^^^^^

Let's continue our discussion on sizes with an example that makes use of the
:ada:`Component_Size` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Array is array (Positive range <>) of UInt_7;

       type UInt_7_Array_Comp_32 is array (Positive range <>) of UInt_7
         with Component_Size => 32;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       Arr_1 : UInt_7_Array (1 .. 20);
       Arr_2 : UInt_7_Array_Comp_32 (1 .. 20);
    begin
       Put_Line ("UInt_7_Array'Size:                   "
                 & UInt_7_Array'Size'Image);
       Put_Line ("UInt_7_Array'Object_Size:            "
                 & UInt_7_Array'Object_Size'Image);
       Put_Line ("UInt_7_Array'Component_Size:         "
                 & UInt_7_Array'Component_Size'Image);
       Put_Line ("Arr_1'Component_Size:                "
                 & Arr_1'Component_Size'Image);
       Put_Line ("Arr_1'Size:                          "
                 & Arr_1'Size'Image);
       New_Line;

       Put_Line ("UInt_7_Array_Comp_32'Object_Size:    "
                 & UInt_7_Array_Comp_32'Size'Image);
       Put_Line ("UInt_7_Array_Comp_32'Object_Size:    "
                 & UInt_7_Array_Comp_32'Object_Size'Image);
       Put_Line ("UInt_7_Array_Comp_32'Component_Size: "
                 & UInt_7_Array_Comp_32'Component_Size'Image);
       Put_Line ("Arr_2'Component_Size:                "
                 & Arr_2'Component_Size'Image);
       Put_Line ("Arr_2'Size:                          "
                 & Arr_2'Size'Image);
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Array'Size:                    17179869176
    UInt_7_Array'Object_Size:             17179869176
    UInt_7_Array'Component_Size:          8
    Arr_1'Component_Size:                 8
    Arr_1'Size:                           160

    UInt_7_Array_Comp_32'Size:            68719476704
    UInt_7_Array_Comp_32'Object_Size:     68719476704
    UInt_7_Array_Comp_32'Component_Size:  32
    Arr_2'Component_Size:                 32
    Arr_2'Size:                           640

Here, the value we get for :ada:`Component_Size` of the :ada:`UInt_7_Array`
type is 8 bits, which matches the :ada:`UInt_7'Object_Size` |mdash| as we've
seen in the previous subsection. In general, we expect the component size to
match the object size of the underlying type.

However, we might have component sizes that aren't equal to the object size of
the component's type. For example, in the declaration of the
:ada:`UInt_7_Array_Comp_32` type, we're using the :ada:`Component_Size` aspect
to query whether the size of each component can be 32 bits:

.. code-block:: ada

    type UInt_7_Array_Comp_32 is array (Positive range <>) of UInt_7
      with Component_Size => 32;

If the code compiles, we see this value when we use the :ada:`Component_Size`
attribute. In this case, even though :ada:`UInt_7'Object_Size` is 8 bits, the
component size of the array type (:ada:`UInt_7_Array_Comp_32'Component_Size`)
is 32 bits.

Note that we can use the :ada:`Component_Size` attribute with data types, as
well as with actual objects of that data type. Therefore, we can write
:ada:`UInt_7_Array'Component_Size` and :ada:`Arr_1'Component_Size`, for
example.

This big number (17179869176 bits) for :ada:`UInt_7_Array'Size` and
:ada:`UInt_7_Array'Object_Size` might be surprising for you. This is due to the
fact that Ada is reporting the size of the :ada:`UInt_7_Array` type for the
case when the complete range is used. Considering that we specified a positive
range in the declaration of the :ada:`UInt_7_Array` type, the maximum length
on this machine is :math:`2^{31} -1`. The object size of an array type is
calculated by multiplying the maximum length by the component size. Therefore,
the object size of the :ada:`UInt_7_Array` type corresponds to the
multiplication of :math:`2^{31} -1` components (maximum length) by 8 bits
(component size).

Storage size
^^^^^^^^^^^^

To complete our discussion on sizes, let's look at this example of storage
sizes:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Access is access UInt_7;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       AV1, AV2 : UInt_7_Access;
    begin
       Put_Line ("UInt_7_Access'Storage_Size:          "
                 & UInt_7_Access'Storage_Size'Image);
       Put_Line ("UInt_7_Access'Storage_Size (bits):   "
                 & Integer'Image (UInt_7_Access'Storage_Size
                   * System.Storage_Unit));

       Put_Line ("UInt_7'Size:               " & UInt_7'Size'Image);
       Put_Line ("UInt_7_Access'Size:        " & UInt_7_Access'Size'Image);
       Put_Line ("UInt_7_Access'Object_Size: " & UInt_7_Access'Object_Size'Image);
       Put_Line ("AV1'Size:                  " & AV1'Size'Image);
       New_Line;

       Put_Line ("Allocating AV1...");
       AV1 := new UInt_7;
       Put_Line ("Allocating AV2...");
       AV2 := new UInt_7;
       New_Line;

       Put_Line ("AV1.all'Size:              " & AV1.all'Size'Image);
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Access'Storage_Size:           0
    UInt_7_Access'Storage_Size (bits):    0

    UInt_7'Size:                7
    UInt_7_Access'Size:         64
    UInt_7_Access'Object_Size:  64
    AV1'Size:                   64

    Allocating AV1...
    Allocating AV2...

    AV1.all'Size:               8

As we've mentioned earlier on, :ada:`Storage_Size` corresponds to the number of
storage elements reserved for an access type or a task object. In this case,
we see that the storage size of the :ada:`UInt_7_Access` type is zero. This is
because we haven't indicated that memory should be reserved for this data type.
Thus, the compiler doesn't reserve memory and simply sets the size to zero.

Because :ada:`Storage_Size` gives us the number of storage elements, we have
to multiply this value by :ada:`System.Storage_Unit` |mdash| which gives
us the size (in bits) of a single storage element |mdash| to get the total
storage size in bits. (In this particular example, however, the multiplication
doesn't make any difference, as the number of storage elements is zero.)

Note that the size of our original data type :ada:`UInt_7` is 7 bits, while the
size of its corresponding access type :ada:`UInt_7_Access` (and the access
object :ada:`AV1`) is 64 bits. This is due to the fact that the access type
doesn't contain an object, but rather memory information about an object. You
can retrieve the size of an object allocated via :ada:`new` by first
dereferencing it |mdash| in our example, we do this by writing
:ada:`AV1.all'Size`.

Now, let's use the :ada:`Storage_Size` aspect to actually reserve memory for
this data type:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Sizes
    :class: ada-run-expect-failure

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Reserved_Access is access UInt_7
         with Storage_Size => 8;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       RAV1, RAV2 : UInt_7_Reserved_Access;
    begin
       Put_Line ("UInt_7_Reserved_Access'Storage_Size:        "
                 & UInt_7_Reserved_Access'Storage_Size'Image);
       Put_Line ("UInt_7_Reserved_Access'Storage_Size (bits): "
                 & Integer'Image (UInt_7_Reserved_Access'Storage_Size
                   * System.Storage_Unit));

       Put_Line ("UInt_7_Reserved_Access'Size:        "
                 & UInt_7_Reserved_Access'Size'Image);
       Put_Line ("UInt_7_Reserved_Access'Object_Size: "
                 & UInt_7_Reserved_Access'Object_Size'Image);
       Put_Line ("RAV1'Size:                          " & RAV1'Size'Image);
       New_Line;

       Put_Line ("Allocating RAV1...");
       RAV1 := new UInt_7;
       Put_Line ("Allocating RAV2...");
       RAV2 := new UInt_7;
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Reserved_Access'Storage_Size:         8
    UInt_7_Reserved_Access'Storage_Size (bits):  64

    UInt_7_Reserved_Access'Size:         64
    UInt_7_Reserved_Access'Object_Size:  64
    RAV1'Size:                           64

    Allocating RAV1...
    Allocating RAV2...

    raised STORAGE_ERROR : s-poosiz.adb:108 explicit raise

In this case, we're reserving 8 storage elements in the declaration of
:ada:`UInt_7_Reserved_Access`.

.. code-block:: ada

    type UInt_7_Reserved_Access is access UInt_7
      with Storage_Size => 8;

Since each storage unit corresponds to one byte (8 bits) in this architecture,
we're reserving a maximum of 64 bits for the :ada:`UInt_7_Reserved_Access`
type.

This example raises an exception at runtime |mdash| a storage error, to be more
specific. This is because the maximum reserved size is 64 bits, and the size of
a single access object is 64 bits as well. Therefore, after the first
allocation, the reserved storage space is already consumed, so we cannot
allocate a second access object.

This behavior might be quite limiting in many cases. However, for certain
applications where memory is very constrained, this might be exactly what we
want to see. For example, having an exception being raised when the allocated
memory for this data type has reached its limit might allow the application to
have enough memory to at least handle the exception gracefully.

Alignment
~~~~~~~~~

For many algorithms, it's important to ensure that we're using the appropriate
alignment. This can be done by using the :ada:`Alignment` attribute and the
:ada:`Alignment` aspect. Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Alignment

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type Aligned_UInt_7 is new UInt_7
         with Alignment => 4;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Alignment is
       V         : constant UInt_7         := 0;
       Aligned_V : constant Aligned_UInt_7 := 0;
    begin
       Put_Line ("UInt_7'Alignment:           " & UInt_7'Alignment'Image);
       Put_Line ("UInt_7'Size:                " & UInt_7'Size'Image);
       Put_Line ("UInt_7'Object_Size:         " & UInt_7'Object_Size'Image);
       Put_Line ("V'Alignment:                " & V'Alignment'Image);
       Put_Line ("V'Size:                     " & V'Size'Image);
       New_Line;

       Put_Line ("Aligned_UInt_7'Alignment:   " & Aligned_UInt_7'Alignment'Image);
       Put_Line ("Aligned_UInt_7'Size:        " & Aligned_UInt_7'Size'Image);
       Put_Line ("Aligned_UInt_7'Object_Size: "
                 & Aligned_UInt_7'Object_Size'Image);
       Put_Line ("Aligned_V'Alignment:        " & Aligned_V'Alignment'Image);
       Put_Line ("Aligned_V'Size:             " & Aligned_V'Size'Image);
       New_Line;
    end Show_Alignment;

Depending on your target architecture, you may see this output:

::

    UInt_7'Alignment:            1
    UInt_7'Size:                 7
    UInt_7'Object_Size:          8
    V'Alignment:                 1
    V'Size:                      8

    Aligned_UInt_7'Alignment:    4
    Aligned_UInt_7'Size:         7
    Aligned_UInt_7'Object_Size:  32
    Aligned_V'Alignment:         4
    Aligned_V'Size:              32

In this example, we're reusing the :ada:`UInt_7` type that we've already been
using in previous examples. Because we haven't specified any alignment for the
:ada:`UInt_7` type, it has an alignment of 1 storage unit (or 8 bits). However,
in the declaration of the :ada:`Aligned_UInt_7` type, we're using the
:ada:`Alignment` aspect to request an alignment of 4 storage units (or 32
bits):

.. code-block:: ada

    type Aligned_UInt_7 is new UInt_7
      with Alignment => 4;

When using the :ada:`Alignment` attribute for the :ada:`Aligned_UInt_7` type,
we can confirm that its alignment is indeed 4 storage units (bytes).

Note that we can use the :ada:`Alignment` attribute for both data types and
objects |mdash| in the code above, we're using :ada:`UInt_7'Alignment` and
:ada:`V'Alignment`, for example.

Because of the alignment we're specifying for the :ada:`Aligned_UInt_7` type,
its size |mdash| indicated by the :ada:`Object_Size` attribute |mdash| is 32
bits instead of 8 bits as for the :ada:`UInt_7` type.

Note that you can also retrieve the alignment associated with a class using
:ada:`S'Class'Alignment`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Class_Alignment

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Class_Alignment is

       type Point_1D is tagged record
          X : Integer;
       end record;

       type Point_2D is new Point_1D with record
          Y : Integer;
       end record
         with Alignment => 16;

       type Point_3D is new Point_2D with record
          Z : Integer;
       end record;

    begin
       Put_Line ("1D_Point'Alignment:       " & Point_1D'Alignment'Image);
       Put_Line ("1D_Point'Class'Alignment: " & Point_1D'Class'Alignment'Image);
       Put_Line ("2D_Point'Alignment:       " & Point_2D'Alignment'Image);
       Put_Line ("2D_Point'Class'Alignment: " & Point_2D'Class'Alignment'Image);
       Put_Line ("3D_Point'Alignment:       " & Point_3D'Alignment'Image);
       Put_Line ("3D_Point'Class'Alignment: " & Point_3D'Class'Alignment'Image);
    end Show_Class_Alignment;

Overlapping Storage
~~~~~~~~~~~~~~~~~~~

Algorithms can be designed to perform in-place or out-of-place processing. In
other words, they can take advantage of the fact that input and output arrays
share the same storage space or not.

We can use the :ada:`Has_Same_Storage` and the :ada:`Overlaps_Storage`
attributes to retrieve more information about how the storage space of two
objects related to each other:

- the :ada:`Has_Same_Storage` attribute indicates whether two objects have the
  exact same storage.

  - A typical example is when both objects are exactly the same, so they
    obviously share the same storage. For example, for array :ada:`A`,
    :ada:`A'Has_Same_Storage (A)` is always :ada:`True`.

- the :ada:`Overlaps_Storage` attribute indicates whether two objects have at
  least one bit in common.

  - Note that, if two objects have the same storage, this implies that their
    storage also overlaps. In other words, :ada:`A'Has_Same_Storage (B) = True`
    implies that :ada:`A'Overlaps_Storage (B) = True`.


Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Overlapping_Storage

    package Int_Array_Processing is

       type Int_Array is array (Positive range <>) of Integer;

       procedure Show_Storage (X : Int_Array;
                               Y : Int_Array);

       procedure Process (X :     Int_Array;
                          Y : out Int_Array);

    end Int_Array_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Array_Processing is

       procedure Show_Storage (X : Int_Array;
                               Y : Int_Array) is
       begin
          if X'Has_Same_Storage (Y) then
             Put_Line ("Info: X and Y have the same storage.");
          else
             Put_Line ("Info: X and Y don't the have same storage.");
          end if;
          if X'Overlaps_Storage (Y) then
             Put_Line ("Info: X and Y overlap.");
          else
             Put_Line ("Info: X and Y don't overlap.");
          end if;
       end Show_Storage;

       procedure Process (X :     Int_Array;
                          Y : out Int_Array) is
       begin
          Put_Line ("==== PROCESS ====");
          Show_Storage (X, Y);

          if X'Has_Same_Storage (Y) then
             Put_Line ("In-place processing...");
          else
             if not X'Overlaps_Storage (Y) then
                Put_Line ("Out-of-place processing...");
             else
                Put_Line ("Cannot process overlapping arrays...");
             end if;
          end if;
          New_Line;
       end Process;

    end Int_Array_Processing;

    with Int_Array_Processing; use Int_Array_Processing;

    procedure Main is
       A : Int_Array (1 .. 20) := (others => 3);
       B : Int_Array (1 .. 20) := (others => 4);
    begin
       Process (A, A);
       --  In-place processing: sharing the exact same storage

       Process (A (1 .. 10), A (10 .. 20));
       --  Overlapping one component: A (10)

       Process (A (1 .. 10), A (11 .. 20));
       --  Out-of-place processing: same array, but not sharing any storage

       Process (A, B);
       --  Out-of-place processing: two different arrays
    end Main;

In this code example, we implement two procedures:

- :ada:`Show_Storage`, which shows storage information about two arrays by
  using the :ada:`Has_Same_Storage` and :ada:`Overlaps_Storage` attributes.

- :ada:`Process`, which are supposed to process an input array :ada:`X` and
  store the processed data in the output array :ada:`Y`.

    - Note that the implementation of this procedure is actually just a
      mock-up, so that no processing is actually taking place.

We have four different instances of how we can call the :ada:`Process`
procedure:

- in the :ada:`Process (A, A)` call, we're using the same array for the input
  and output arrays. This is a perfect example of in-place processing. Because
  the input and the output arrays arguments are actually the same object, they
  obviously share the exact same storage.

- in the :ada:`Process (A (1 .. 10), A (10 .. 20))` call, we're using two
  slices of the :ada:`A` array as input and output arguments. In this case, a
  single component of the :ada:`A` array is shared: :ada:`A (10)`. Because the
  storage space is overlapping, but not exactly the same, neither in-place nor
  out-of-place processing can usually be used in this case.

- in the :ada:`Process (A (1 .. 10), A (11 .. 20))` call, even though we're
  using the same array :ada:`A` for the input and output arguments, we're using
  slices that are completely independent from each other, so that the input and
  output arrays are not sharing any storage in this case. Therefore, we can use
  out-of-place processing.

- in the :ada:`Process (A, B)` call, we have two different arrays |mdash| which
  obviously don't share any storage space |mdash|, so we can use out-of-place
  processing.

Packed Representation
~~~~~~~~~~~~~~~~~~~~~

As we've seen previously, the minimum number of bits required to represent a
data type might be less than the actual number of bits used to store an object
of that same type. We've seen an example where :ada:`UInt_7'Size` was 7 bits,
while :ada:`UInt_7'Object_Size` was 8 bits. The most extreme case is the one
for the :ada:`Boolean` type: in this case, :ada:`Boolean'Size` is 1 bit, while
:ada:`Boolean'Object_Size` might be 8 bits (or even more on certain
architectures). In such cases, we have 7 (or more) unused bits in memory for
each object of :ada:`Boolean` type. In other words, we're wasting memory. On
the other hand, we're gaining speed of access because we can directly access
each element without having to first change its internal representation back
and forth. We'll come back to this point later.

The situation is even worse when implementing bit-fields, which can be
declared as an array of :ada:`Boolean` components. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Non_Packed_Flags

    package Flag_Definitions is

       type Flags is array (Positive range <>) of Boolean;

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Flags is
       Flags_1 : Flags (1 .. 8);
    begin
       Put_Line ("Boolean'Size:           " & Boolean'Size'Image);
       Put_Line ("Boolean'Object_Size:    " & Boolean'Object_Size'Image);
       Put_Line ("Flags_1'Size:           " & Flags_1'Size'Image);
       Put_Line ("Flags_1'Component_Size: " & Flags_1'Component_Size'Image);
    end Show_Flags;

Depending on your target architecture, you may see this output:

::

    Boolean'Size:            1
    Boolean'Object_Size:     8
    Flags_1'Size:            64
    Flags_1'Component_Size:  8

In this example, we're declaring the :ada:`Flags` type as an array of
:ada:`Boolean` components. As we can see in this case, although the size of the
:ada:`Boolean` type is just 1 bit, an object of this type has a size of 8 bits.
Consequently, each component of the :ada:`Flags` type has a size of 8 bits.
Moreover, an array with 8 components of :ada:`Boolean` type |mdash| such as
the :ada:`Flags_1` array |mdash| has a size of 64 bits.

Therefore, having a way to compact the representation |mdash| so that we can
store multiple objects without wasting storage space |mdash| may help us
improving memory usage. This is actually possible by using the :ada:`Pack`
aspect. For example, we could extend the previous example and declare a
:ada:`Packed_Flags` type that makes use of this aspect:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Packed_Flags

    package Flag_Definitions is

       type Flags is array (Positive range <>) of Boolean;

       type Packed_Flags is array (Positive range <>) of Boolean
         with Pack;

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Packed_Flags is
       Flags_1 : Flags (1 .. 8);
       Flags_2 : Packed_Flags (1 .. 8);
    begin
       Put_Line ("Boolean'Size:           " & Boolean'Size'Image);
       Put_Line ("Boolean'Object_Size:    " & Boolean'Object_Size'Image);
       Put_Line ("Flags_1'Size:           " & Flags_1'Size'Image);
       Put_Line ("Flags_1'Component_Size: " & Flags_1'Component_Size'Image);
       Put_Line ("Flags_2'Size:           " & Flags_2'Size'Image);
       Put_Line ("Flags_2'Component_Size: " & Flags_2'Component_Size'Image);
    end Show_Packed_Flags;

Depending on your target architecture, you may see this output:

::

    Boolean'Size:            1
    Boolean'Object_Size:     8
    Flags_1'Size:            64
    Flags_1'Component_Size:  8
    Flags_2'Size:            8
    Flags_2'Component_Size:  1

In this example, we're declaring the :ada:`Flags_2` array of
:ada:`Packed_Flags` type. Its size is 8 bits |mdash| instead of the 64 bits
required for the :ada:`Flags_1` array. Because the array type
:ada:`Packed_Flags` is packed, we can now effectively use this type to store an
object of :ada:`Boolean` type using just 1 bit of the memory, as indicated by
the :ada:`Flags_2'Component_Size` attribute.

In many cases, we need to convert between a *normal* representation (such as
the one used for the :ada:`Flags_1` array above) to a packed representation
(such as the one for the :ada:`Flags_2` array). In many programming languages,
this conversion may require writing custom code with manual bit-shifting and
bit-masking to get the proper target representation. In Ada, however, we just
need to indicate the actual type conversion, and the compiler takes care of
generating code containing bit-shifting and bit-masking to performs the type
conversion.

Let's modify the previous example and introduce this type conversion:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Flag_Conversion

    package Flag_Definitions is

       type Flags is array (Positive range <>) of Boolean;

       type Packed_Flags is array (Positive range <>) of Boolean
         with Pack;

       Default_Flags : constant Flags := (True, True, False, True,
                                          False, False, True, True);

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Flag_Conversion is
       Flags_1 : Flags (1 .. 8);
       Flags_2 : Packed_Flags (1 .. 8);
    begin
       Flags_1 := Default_Flags;
       Flags_2 := Packed_Flags (Flags_1);

       for I in Flags_2'Range loop
          Put_Line (I'Image & ": "
                    & Flags_1 (I)'Image & ", "
                    & Flags_2 (I)'Image);
       end loop;
    end Show_Flag_Conversion;

In this extended example, we're now declaring :ada:`Default_Flags` as an array
of constant flags, which we use to initialize :ada:`Flags_1`.

The actual conversion happens with :ada:`Flags_2 := Packed_Flags (Flags_1)`.
Here, the type conversion :ada:`Packed_Flags()` indicates that we're converting
from the normal representation (used for the :ada:`Flags` type) to the packed
representation (used for :ada:`Packed_Flags` type). We don't need to write more
code than that to perform the correct type conversion.

Also, by using the same strategy, we could read information from a packed
representation. For example:

.. code-block:: ada

    Flags_1 := Flags (Flags_2);

In this case, we use :ada:`Flags()` to convert from a packed representation to
the normal representation.

We elaborate on the topic of converting between data representations in the
section on :ref:`changing data representation <Changing_Data_Representation>`.

Trade-offs
^^^^^^^^^^

As indicated previously, when we're using a packed representation (vs. using a
standard *unpacked* representation), we're trading off speed of access for less
memory consumption. The following table summarizes this:

+----------------+----------------------+-------------------------+
| Representation | More speed of access | Less memory consumption |
+================+======================+=========================+
| Unpacked       | X                    |                         |
+----------------+----------------------+-------------------------+
| Packed         |                      | X                       |
+----------------+----------------------+-------------------------+

On one hand, we have better memory usage when we apply packed representations
because we may save many bits for each object. On the other hand, there's a
cost associated with accessing those packed objects because they need to be
unpacked before we can actually access them. In fact, the compiler generates
code |mdash| using bit-shifting and bit-masking |mdash| that converts a packed
representation into an unpacked representation, which we can then access. Also,
when storing a packed object, the compiler generates code that converts the
unpacked representation of the object into the packed representation.

This packing and unpacking mechanism has a performance cost associated with it,
which results in less speed of access for packed objects. As usual in those
circumstances, before using packed representation, we should assess whether
memory constraints are more important than speed in our target architecture.

Record Representation and storage clauses
-----------------------------------------

In this section, we discuss how to use record representation clauses to specify
how a record is represented in memory. Our goal is to provide a brief
introduction into the topic. If you're interested in more details, you can find
a thorough discussion about record representation clauses in the
*Introduction to Embedded Systems Programming* course.

Let's start with the simple approach of declaring a record type without
providing further information. In this case, we're basically asking the
compiler to select a reasonable representation for that record in the memory of
our target architecture.

Let's see a simple example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Rep_Clauses_1

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

Considering a typical 64-bit PC architecture with 8-bit storage units, and
:ada:`Integer` defined as a 32-bit type, we get this memory representation:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | component } | { { 0 | 1 | 2 | 3 } | A } | { { 4 | 5 | 6 | 7 } | B }"
            shape = "record"
        ];
   }

Each storage unit is a position in memory. In the graph above, the numbers on
the top (0, 1, 2, ...) represent those positions for record :ada:`R`.

In addition, we can show the bits that are used for components :ada:`A` and
:ada:`B`:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | bits | component } |  { { { 0 | #0 .. 7 } | { 1 | #8 .. #15 } | { 2 | #16 .. #23 } | { 3 | #24 .. #31 } } | A } | { { { 4 | #0 .. 7 } | { 5 | #8 .. #15 } | { 6 | #16 .. #23 } | { 7 | #24 .. #31 } } | B }"
            shape = "record"
        ];
   }

The memory representation we see in the graph above can be described in Ada
using representation clauses, as you can see in the code starting at the
:ada:`for R use record` line in the code example below |mdash| we'll discuss
the syntax and further details right after this example.

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Rep_Clauses_2

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

       --  Representation clause for record R:
       for R use record
          A at 0 range 0 .. 31;
          --   ^ starting memory position
          B at 4 range 0 .. 31;
          --           ^ first bit .. last bit
       end record;

    end P;

Here, we're specifying that the :ada:`A` component is stored in the bits #0 up
to #31 starting at position #0. Note that the position itself doesn't represent
an absolute address in the device's memory; instead, it's relative to the
memory space reserved for that record. The :ada:`B` component has the same
32-bit range, but starts at position #4.

This is a generalized view of the syntax:

.. code-block:: ada

    for Record_Type use record
       Component_Name at Start_Position range First_Bit .. Last_Bit;
    end record;

These are the elements we see above:

- :ada:`Component_Name`: name of the component (from the record type
  declaration);

- :ada:`Start_Position`: start position |mdash| in storage units |mdash| of the
  memory space reserved for that component;

- :ada:`First_Bit`: first bit (in the start position) of the component;

- :ada:`Last_Bit`: last bit of the component.

Note that the last bit of a component might be in a different storage unit.
Since the :ada:`Integer` type has a larger width (32 bits) than the storage
unit (8 bits), components of that type span over multiple storage units.
Therefore, in our example, the first bit of component :ada:`A` is at position
#0, while the last bit is at position #3.

Also note that the last eight bits of component :ada:`A` are bits #24 .. #31.
If we think in terms of storage units, this corresponds to bits #0 .. #7 of
position #3. However, when specifying the last bit in Ada, we always use the
:ada:`First_Bit` value as a reference, not the position where those bits might
end up. Therefore, we write :ada:`range 0 .. 31`, well knowing that those 32
bits span over four storage units (positions #0 .. #3).

Storage Place Attributes
~~~~~~~~~~~~~~~~~~~~~~~~

We can retrieve information about the start position, and the first and last
bits of a component by using the storage place attributes:

- :ada:`Position`, which retrieves the start position of a component;

- :ada:`First_Bit`, which retrieves the first bit of a component;

- :ada:`Last_Bit`, which retrieves the last bit of a component.

Note, however, that these attributes can only be used with actual records, and
not with record types.

We can revisit the previous example and verify how the compiler represents the
:ada:`R` type in memory:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Storage_Place_Attributes

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    with P;           use P;

    procedure Show_Storage is
       R1 : R;
    begin
       Put_Line ("R'Size:              " & R'Size'Image);
       Put_Line ("R'Object_Size:       " & R'Object_Size'Image);
       New_Line;

       Put_Line ("System.Storage_Unit: " & System.Storage_Unit'Image);
       New_Line;

       Put_Line ("R1.A'Position  : " & R1.A'Position'Image);
       Put_Line ("R1.A'First_Bit : " & R1.A'First_Bit'Image);
       Put_Line ("R1.A'Last_Bit  : " & R1.A'Last_Bit'Image);
       New_Line;

       Put_Line ("R1.B'Position  : " & R1.B'Position'Image);
       Put_Line ("R1.B'First_Bit : " & R1.B'First_Bit'Image);
       Put_Line ("R1.B'Last_Bit  : " & R1.B'Last_Bit'Image);
    end Show_Storage;

.. only:: builder_html

    On a typical 64-bit PC architecture, you probably see this output:

    ::

        R'Size:               64
        R'Object_Size:        64
        System.Storage_Unit:  8

        R1.A'Position  :  0
        R1.A'First_Bit :  0
        R1.A'Last_Bit  :  31

        R1.B'Position  :  4
        R1.B'First_Bit :  0
        R1.B'Last_Bit  :  31

First of all, we see that the size of the :ada:`R` type is 64 bits, which can
be explained by those two 32-bit integer components. Then, we see that
components :ada:`A` and :ada:`B` start at positions #0 and #4, and each one
makes use of bits in the range from #0 to #31. This matches the graph we've
seen above.

Using Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use representation clauses to change the way the compiler handles
memory for a record type. For example, let's say we want to have an empty
storage unit between components :ada:`A` and :ada:`B`. We can use a
representation clause where we specify that component :ada:`B` starts at
position #5 instead of #4, leaving an empty byte after component :ada:`A` and
before component :ada:`B`:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | bits | component } |  { { { 0 | #0 .. 7 } | { 1 | #8 .. #15 } | { 2 | #16 .. #23 } | { 3 | #24 .. #31 } } | A } | { 4 |  |  } | { { { 5 | #0 .. 7 } | { 6 | #8 .. #15 } | { 7 | #16 .. #23 } | { 8 | #24 .. #31 } } | B }"
            shape = "record"
        ];
   }

This is the code that implements that:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

       for R use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        " & R'Size'Image);
       Put_Line ("R'Object_Size: " & R'Object_Size'Image);
    end Show_Empty_Byte;

When running the application above, we see that, due to the extra byte in the
record representation, the sizes increase. On a typical 64-bit PC,
:ada:`R'Size` is now 76 bits, which reflects the additional eight bits that we
introduced between components :ada:`A` and :ada:`B`. Depending on the target
architecture, you may also see that :ada:`R'Object_Size` is now 96 bits, which
is the size the compiler selects as the most appropriate for this record type.
As we've mentioned in the previous section, we can use aspects to request a
specific size to the compiler. In this case, we could use the
:ada:`Object_Size` aspect:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record
         with Object_Size => 72;

       for R use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        " & R'Size'Image);
       Put_Line ("R'Object_Size: " & R'Object_Size'Image);
    end Show_Empty_Byte;

If the code compiles, :ada:`R'Size` and :ada:`R'Object_Size` should now have
the same value.

Derived Types And Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In some cases, you might want to modify the memory representation of a record
without impacting existing code. For example, you might want to use a record
type that was declared in a package that you're not allowed to change. Also,
you would like to modify its memory representation in your application. A nice
strategy is to derive a type and use a representation clause for the derived
type.

We can apply this strategy on our previous example. Let's say we would like to
use record type :ada:`R` from package :ada:`P` in our application, but we're
not allowed to modify package :ada:`P` |mdash| or the record type, for that
matter. In this case, we could simply derive :ada:`R` as :ada:`R_New` and use a
representation clause for :ada:`R_New`. This is exactly what we do in the
specification of the child package :ada:`P.Rep`:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Derived_Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

    package P.Rep is

       type R_New is new R
         with Object_Size => 72;

       for R_New use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P.Rep;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;
    with P.Rep;       use P.Rep;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        " & R'Size'Image);
       Put_Line ("R'Object_Size: " & R'Object_Size'Image);

       Put_Line ("R_New'Size:        " & R_New'Size'Image);
       Put_Line ("R_New'Object_Size: " & R_New'Object_Size'Image);
    end Show_Empty_Byte;

When running this example, we see that the :ada:`R` type retains the memory
representation selected by the compiler for the target architecture, while the
:ada:`R_New` has the memory representation that we specified.

Representation on Bit Level
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A very common application of representation clauses is to specify individual
bits of a record. This is particularly useful, for example, when mapping
registers or implementing protocols.

Let's consider the following fictitious register as an example:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ bit | component } | { { 0 | 1 }  | S } | { { 2 | 3 } | (reserved) } | { 4 | Error } | { { 5 | 6 | 7 } | V1 }"
            shape = "record"
        ];
   }

Here, :ada:`S` is the current status, :ada:`Error` is a flag, and :ada:`V1`
contains a value. Due to the fact that we can use representation clauses to
describe individual bits of a register as records, the implementation becomes
as simple as this:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Rep_Clauses_Simple_Reg

    package P is

      type Status is (Ready, Waiting, Processing, Done);
      type UInt_3 is range 0 .. 2 ** 3 - 1;

       type Simple_Reg is record
          S     : Status;
          Error : Boolean;
          V1    : UInt_3;
       end record;

       for Simple_Reg use record
          S     at 0 range 0 .. 1;
          --  Bit #2 and 3: reserved!
          Error at 0 range 4 .. 4;
          V1    at 0 range 5 .. 7;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Simple_Reg is
    begin
       Put_Line ("Simple_Reg'Size:        " & Simple_Reg'Size'Image);
       Put_Line ("Simple_Reg'Object_Size: " & Simple_Reg'Object_Size'Image);
    end Show_Simple_Reg;

As we can see in the declaration of the :ada:`Simple_Reg` type, each component
represents a field from our register, and it has a fixed location (which
matches the register representation we see in the graph above). Any operation
on the register is as simple as accessing the record component. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Rep_Clauses_Simple_Reg

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Simple_Reg is
       Default : constant Simple_Reg := (S => Ready, Error => False, V1 => 0);

       R : Simple_Reg := Default;
    begin
       Put_Line ("R.S:  " & R.S'Image);

       R.V1 := 4;

       Put_Line ("R.V1: " & R.V1'Image);
    end Show_Simple_Reg;

As we can see in the example, to retrieve the current status of the register,
we just have to write :ada:`R.S`. To update the *V1* field of the register with
the value 4, we just have to write :ada:`R.V1 := 4`. No extra code |mdash|
such as bit-masking or bit-shifting |mdash| is needed here.

.. admonition:: In other languages

    Some programming languages require that developers use complicated,
    error-prone approaches |mdash| which may include manually bit-shifting and
    bit-masking variables |mdash| to retrieve information from or store
    information to individual bits or registers. In Ada, however, this is
    efficiently handled by the compiler, so that developers only need to
    correctly describe the register mapping using representation clauses.


.. _Changing_Data_Representation:

Changing Data Representation
----------------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #27: Changing Data Representation <https://www.adacore.com/gems/gem-27>`_
    and `Gem #28 <https://www.adacore.com/gems/gem-28>`_.

A powerful feature of Ada is the ability to specify the exact data layout. This
is particularly important when you have an external device or program that
requires a very specific format. Some examples are:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Com_Packet

    package Communication is

       type Com_Packet is record
          Key : Boolean;
          Id  : Character;
          Val : Integer range 100 .. 227;
       end record;

       for Com_Packet use record
          Key at 0 range 0 .. 0;
          Id  at 0 range 1 .. 8;
          Val at 0 range 9 .. 15;
       end record;

    end Communication;

which lays out the fields of a record, and in the case of :ada:`Val`, forces a
biased representation in which all zero bits represents 100. Another example
is:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Array_Rep

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);

       type Arr is array (1 .. 16) of Val
         with Component_Size => 3;

    end Array_Representation;

which forces the components to take only 3 bits, crossing byte boundaries as
needed. A final example is:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enum_Rep

    package Enumeration_Representation is

       type Status is (Off, On, Unknown);
       for Status use (Off => 2#001#, On => 2#010#, Unknown => 2#100#);

    end Enumeration_Representation;

which allows specified values for an enumeration type, instead of the efficient
default values of 0, 1, 2.

In all these cases, we might use these representation clauses to match external
specifications, which can be very useful. The disadvantage of such layouts is
that they are inefficient, and accessing individual components, or, in the case
of the enumeration type, looping through the values can increase space and
time requirements for the program code.

One approach that is often effective is to read or write the data in question
in this specified form, but internally in the program represent the data in the
normal default layout, allowing efficient access, and do all internal
computations with this more efficient form.

To follow this approach, you will need to convert between the efficient format
and the specified format. Ada provides a very convenient method for doing this,
as described in RM 13.6 "Change of Representation".

The idea is to use type derivation, where one type has the specified format and
the other has the normal default format. For instance for the array case above,
we would write:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Array_Rep

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);
       type Arr is array (1 .. 16) of Val;

       type External_Arr is new Arr
         with Component_Size => 3;

    end Array_Representation;

Now we read and write the data using the :ada:`External_Arr` type. When we want
to convert to the efficient form, :ada:`Arr`, we simply use a type conversion.

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Array_Rep

    with Array_Representation; use Array_Representation;

    procedure Using_Array_For_IO is
       Input_Data  : External_Arr;
       Work_Data   : Arr;
       Output_Data : External_Arr;
    begin
       --  (read data into Input_Data)

       --  Now convert to internal form
        Work_Data := Arr (Input_Data);

       --  (computations using efficient Work_Data form)

       --  Convert back to external form
       Output_Data := External_Arr (Work_Data);

    end Using_Array_For_IO;

Using this approach, the quite complex task of copying all the data of the
array from one form to another, with all the necessary masking and shift
operations, is completely automatic.

Similar code can be used in the record and enumeration type cases. It is even
possible to specify two different representations for the two types, and
convert from one form to the other, as in:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enum_Rep

    package Enumeration_Representation is

       type Status_In is (Off, On, Unknown);
       type Status_Out is new Status_In;

       for Status_In use (Off => 2#001#, On => 2#010#, Unknown => 2#100#);
       for Status_Out use (Off => 103, On => 1045, Unknown => 7700);

    end Enumeration_Representation;

There are two restrictions that must be kept in mind when using this feature.
First, you have to use a derived type. You can't put representation clauses on
subtypes, which means that the conversion must always be explicit. Second,
there is a rule RM 13.1(10) that restricts the placement of interesting
representation clauses:

    10 For an untagged derived type, no type-related representation items are
    allowed if the parent type is a by-reference type, or has any user-defined
    primitive subprograms.

All the representation clauses that are interesting from the point of view of
change of representation are "type related", so for example, the following
sequence would be illegal:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Array_Rep_2
    :class: ada-expect-compile-error

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);
       type Arr is array (1 .. 16) of Val;

       procedure Rearrange (Arg : in out Arr);

       type External_Arr is new Arr
         with Component_Size => 3;

    end Array_Representation;

Why these restrictions? Well, the answer is a little complex, and has to do
with efficiency considerations, which we will address below.

Restrictions
~~~~~~~~~~~~

In the previous subsection, we discussed the use of derived types and
representation clauses to achieve automatic change of representation. More
accurately, this feature is not completely automatic, since it requires you to
write an explicit conversion. In fact there is a principle behind the design
here which says that a change of representation should never occur implicitly
behind the back of the programmer without such an explicit request by means of
a type conversion.

The reason for that is that the change of representation operation can be very
expensive, since in general it can require component by component copying,
changing the representation on each component.

Let's have a look at the ``-gnatG`` expanded code to see what is hidden under
the covers here. For example, the conversion :ada:`Arr (Input_Data)` from the
previous example generates the following expanded code:

.. code-block::

       B26b : declare
          [subtype p__TarrD1 is integer range 1 .. 16]
          R25b : p__TarrD1 := 1;
       begin
          for L24b in 1 .. 16 loop
             [subtype p__arr___XP3 is
               system__unsigned_types__long_long_unsigned range 0 ..
               16#FFFF_FFFF_FFFF#]
             work_data := p__arr___XP3!((work_data and not shift_left!(
               16#7#, 3 * (integer(L24b - 1)))) or shift_left!(p__arr___XP3!
               (input_data (R25b)), 3 * (integer(L24b - 1))));
             R25b := p__TarrD1'succ(R25b);
          end loop;
       end B26b;

That's pretty horrible! In fact, we could have simplified it for this section,
but we have left it in its original form, so that you can see why it is nice to
let the compiler generate all this stuff so you don't have to worry about it
yourself.

Given that the conversion can be pretty inefficient, you don't want to convert
backwards and forwards more than you have to, and the whole approach is only
worthwhile if we'll be doing extensive computations involving the value.

The expense of the conversion explains two aspects of this feature that are not
obvious. First, why do we require derived types instead of just allowing
subtypes to have different representations, avoiding the need for an explicit
conversion?

The answer is precisely that the conversions are expensive, and you don't want
them happening behind your back. So if you write the explicit conversion, you
get all the gobbledygook listed above, but you can be sure that this never
happens unless you explicitly ask for it.

This also explains the restriction we mentioned in previous subsection from
RM 13.1(10):

    10 For an untagged derived type, no type-related representation items are
    allowed if the parent type is a by-reference type, or has any user-defined
    primitive subprograms.

It turns out this restriction is all about avoiding implicit changes of
representation. Let's have a look at how type derivation works when there are
primitive subprograms defined at the point of derivation. Consider this
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.My_Int

    package My_Ints is

       type My_Int_1 is range 1 .. 10;

       function Odd (Arg : My_Int_1) return Boolean;

       type My_Int_2 is new My_Int_1;

    end My_Ints;

    package body My_Ints is

       function Odd (Arg : My_Int_1) return Boolean is (True);
       --  Dummy implementation!

    end My_Ints;

Now when we do the type derivation, we inherit the function :ada:`Odd` for
:ada:`My_Int_2`. But where does this function come from? We haven't
written it explicitly, so the compiler somehow materializes this new implicit
function. How does it do that?

We might think that a complete new function is created including a body in
which :ada:`My_Int_2` replaces :ada:`My_Int_1`, but that would be impractical
and expensive. The actual mechanism avoids the need to do this by use of
implicit type conversions. Suppose after the above declarations, we write:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.My_Int

    with My_Ints; use My_Ints;

    procedure Using_My_Int is
       Var : My_Int_2;
    begin

       if Odd (Var) then
          --   ^ Calling Odd function for My_Int_2 type.
          null;
       end if;

    end Using_My_Int;

The compiler translates this as:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.My_Int

    with My_Ints; use My_Ints;

    procedure Using_My_Int is
       Var : My_Int_2;
    begin

       if Odd (My_Int_1 (Var)) then
          --   ^ Converting My_Int_2 to My_Int_1 type before
          --     calling Odd function.
          null;
       end if;

    end Using_My_Int;

This implicit conversion is a nice trick, it means that we can get the effect
of inheriting a new operation without actually having to create it.
Furthermore, in a case like this, the type conversion generates no code,
since :ada:`My_Int_1` and :ada:`My_Int_2` have the same representation.

But the whole point is that they might not have the same representation if one
of them had a representation clause that made the representations different,
and in this case the implicit conversion inserted by the compiler could be
expensive, perhaps generating the junk we quoted above for the :ada:`Arr` case.
Since we never want that to happen implicitly, there is a rule to prevent it.

The business of forbidding by-reference types (which includes all tagged
types) is also driven by this consideration. If the representations are the
same, it is fine to pass by reference, even in the presence of the conversion,
but if there was a change of representation, it would force a copy, which would
violate the by-reference requirement.

So to summarize this section, on the one hand Ada gives you a very convenient
way to trigger these complex conversions between different representations. On
the other hand, Ada guarantees that you never get these potentially expensive
conversions happening unless you explicitly ask for them.

Valid
-----

When receiving data from external sources, we're subjected to problems such as
transmission errors. If not handled properly, erroneous data can lead to major
issues in an application.

One of those issues originates from the fact that transmission errors might
lead to invalid information stored in memory. When proper checks are active,
using invalid information is detected at runtime and an exception is raised at
this point, which might then be handled by the application.

Instead of relying on exception handling, however, we could instead ensure that
the information we're about to use is valid. We can do this by using the
:ada:`Valid` attribute. For example, if we have a variable :ada:`Var`, we can
verify that the value stored in :ada:`Var` is valid by writing
:ada:`Var'Valid`, which returns a :ada:`Boolean` value. Therefore, if the value
of :ada:`Var` isn't valid, :ada:`Var'Valid` returns :ada:`False`, so we can
have code that handles this situation before we actually make use of
:ada:`Var`. In other words, instead of handling a potential exception in other
parts of the application, we can proactively verify that input information is
correct and avoid that an exception is raised.

In the next example, we show an application that

- generates a file containing mock-up data, and then

- reads information from this file as state values.

The mock-up data includes valid and invalid states.

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Valid_States
    :class: ada-run

    procedure Create_Test_File (File_Name : String);

    with Ada.Sequential_IO;

    procedure Create_Test_File (File_Name : String) is
       package Integer_Sequential_IO is new Ada.Sequential_IO (Integer);
       use Integer_Sequential_IO;

       F : File_Type;
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1);
       Write (F,  2);
       Write (F,  4);
       Write (F,  3);
       Write (F,  2);
       Write (F,  10);
       Close (F);
    end Create_Test_File;

    with Ada.Sequential_IO;

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off => 1, On => 2, Waiting => 4);

       package State_Sequential_IO is new Ada.Sequential_IO (State);

       procedure Read_Display_States (File_Name : String);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Read_Display_States (File_Name : String) is
          use State_Sequential_IO;

          F : State_Sequential_IO.File_Type;
          S : State;

          procedure Display_State (S : State) is
          begin
             --  Before displaying the value, check whether it's valid or not.
             if S'Valid then
                Put_Line (S'Image);
             else
                Put_Line ("Invalid value detected!");
             end if;
          end Display_State;

       begin
          Open (F, In_File, File_Name);

          while not End_Of_File (F) loop
             Read (F, S);
             Display_State (S);
          end loop;

          Close (F);
       end Read_Display_States;

    end States;

    with States;           use States;
    with Create_Test_File;

    procedure Show_States_From_File is
       File_Name : constant String := "data.bin";
    begin
       Create_Test_File (File_Name);
       Read_Display_States (File_Name);
    end Show_States_From_File;

.. only:: builder_html

    When running the application, you'd see this output:

    ::

        OFF
        ON
        WAITING
        Invalid value detected!
        ON
        Invalid value detected!

Let's start our discussion on this example with the :ada:`States` package,
which contains the declaration of the :ada:`State` type. This type is a simple
enumeration containing three states: :ada:`Off`, :ada:`On` and :ada:`Waiting`.
We're assigning specific integer values for this type by declaring an
enumeration representation clause. Note that we're using the :ada:`Size` aspect
to request that objects of this type have the same size as the :ada:`Integer`
type. This becomes important later on when parsing data from the file.

In the :ada:`Create_Test_File` procedure, we create a file containing integer
values, which is parsed later by the :ada:`Read_Display_States` procedure. The
:ada:`Create_Test_File` procedure doesn't contain any reference to the
:ada:`State` type, so we're not constrained to just writing information that is
valid for this type. On the contrary, this procedure makes use of the
:ada:`Integer` type, so we can write any integer value to the file. We use this
strategy to write both valid and invalid values of :ada:`State` to the file.
This allows us to simulate an environment where transmission errors occur.

We call the :ada:`Read_Display_States` procedure to read information from the
file and display each state stored in the file. In the main loop of this
procedure, we call :ada:`Read` to read a state from the file and store it in
the :ada:`S` variable. We then call the nested :ada:`Display_State` procedure
to display the actual state stored in :ada:`S`. The most important line of code
in the :ada:`Display_State` procedure is the one that uses the :ada:`Valid`
attribute:

.. code-block:: ada

    if S'Valid then

In this line, we're verifying that the :ada:`S` variable contains a valid state
before displaying the actual information from :ada:`S`. If the value stored in
:ada:`S` isn't valid, we can handle the issue accordingly. In this case, we're
simply displaying a message indicating that an invalid value was detected. If
we didn't have this check, the :ada:`Constraint_Error` exception would be
raised when trying to use invalid data stored in :ada:`S` |mdash| this would
happen, for example, after reading the integer value 3 from the input file.

In summary, using the :ada:`Valid` attribute is a good strategy we can employ
when we know that information stored in memory might be corrupted.

Unchecked Union
---------------

We've introduced variant records back in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/more_about_types>`.
In simple terms, a variant record is a record with discriminants that allows
for changing its structure. Basically, it's a record containing a :ada:`case`.

The :ada:`State_Or_Integer` declaration in the :ada:`States` package below is
an example of a variant record:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.State_Or_Integer

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off => 1, On => 2, Waiting => 4);

       type State_Or_Integer (Use_Enum : Boolean) is record
          case Use_Enum is
             when False => I : Integer;
             when True  => S : State;
          end case;
       end record;

       procedure Display_State_Value (V : State_Or_Integer);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value (V : State_Or_Integer) is
       begin
          Put_Line ("State: " & V.S'Image);
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

As mentioned in the previous course, if you try to access a component that is
not valid for your record, a :ada:`Constraint_Error` exception is raised. For
example, in the implementation of the :ada:`Display_State_Value` procedure,
we're trying to retrieve the value of the integer component (:ada:`I`) of the
:ada:`V` record. When calling this procedure, the :ada:`Constraint_Error`
exception is raised as expected because :ada:`Use_Enum` is set to :ada:`True`,
so that the :ada:`I` component is invalid |mdash| only the :ada:`S` component
is valid in this case.

.. code:: ada run_button project=Courses.Advanced_Ada.Types.State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Variant_Rec_Error is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value (V);
    end Show_Variant_Rec_Error;

In addition to not being able to read the value of a component that isn't
valid, assigning a value to a component that isn't valid also raises an
exception at runtime. In this example, we cannot assign to :ada:`V.I`:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Variant_Rec_Error is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.I := 4;
       --  Error: V.I cannot be accessed because Use_Enum is set to True.
    end Show_Variant_Rec_Error;

We may circumvent this limitation by using the :ada:`Unchecked_Union` aspect.
For example, we can derive a new type from :ada:`State_Or_Integer` and use
this aspect in its declaration. We do this in the declaration of the
:ada:`Unchecked_State_Or_Integer` type below.

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off => 1, On => 2, Waiting => 4);

       type State_Or_Integer (Use_Enum : Boolean) is record
          case Use_Enum is
             when False => I : Integer;
             when True  => S : State;
          end case;
       end record;

       type Unchecked_State_Or_Integer (Use_Enum : Boolean) is
         new State_Or_Integer (Use_Enum) with Unchecked_Union;

       procedure Display_State_Value (V : Unchecked_State_Or_Integer);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value (V : Unchecked_State_Or_Integer) is
       begin
          Put_Line ("State: " & V.S'Image);
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

Because we now use the :ada:`Unchecked_State_Or_Integer` type for the input
parameter of the :ada:`Display_State_Value` procedure, no exception is raised
at runtime, as both components are now accessible. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer

    with States; use States;

    procedure Show_Unchecked_Union is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value (Unchecked_State_Or_Integer (V));
    end Show_Unchecked_Union;

Note that, in the call to the :ada:`Display_State_Value` procedure, we first
need to convert the :ada:`V` argument from the :ada:`State_Or_Integer` to the
:ada:`Unchecked_State_Or_Integer` type.

Also, we can assign to any of the components of a record that has the
:ada:`Unchecked_Union` aspect. In our example, we can now assign to both the
:ada:`S` and the :ada:`I` components of the :ada:`V` record:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer (Use_Enum => True);
    begin
       V := (Use_Enum => True, S => On);
       Display_State_Value (V);

       V := (Use_Enum => False, I => 4);
       Display_State_Value (V);
    end Show_Unchecked_Union;

In the example above, we're use an aggregate in the assignments to :ada:`V`. By
doing so, we avoid that :ada:`Use_Enum` is set to the *wrong* component. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value (V);

       V.I := 4;  --  Error: cannot directly assign to V.I, as Use_Enum is
                  --         set to True.
       Display_State_Value (V);
    end Show_Unchecked_Union;

Here, even though the record has the :ada:`Unchecked_Union` attribute, we
cannot directly assign to the :ada:`I` component because :ada:`Use_Enum` is set
to :ada:`True`, so only the :ada:`S` is accessible. We can, however, read its
value, as we do in the :ada:`Display_State_Value` procedure.

Be aware that, due to the fact the union is not checked, we might write invalid
data to the record. In the example below, we initialize the :ada:`I` component
with 3, which is a valid integer value, but results in an invalid value for
the :ada:`S` component, as the value 3 cannot be mapped to the representation
of the :ada:`State` type.

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer (Use_Enum => True);
    begin
       V := (Use_Enum => False, I => 3);
       Display_State_Value (V);
    end Show_Unchecked_Union;

To mitigate this problem, we could use the :ada:`Valid` attribute |mdash|
discussed in the previous section |mdash| for the :ada:`S` component before
trying to use its value in the implementation of the :ada:`Display_State_Value`
procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Unchecked_State_Or_Integer

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value (V : Unchecked_State_Or_Integer) is
       begin
          if V.S'Valid then
             Put_Line ("State: " & V.S'Image);
          else
             Put_Line ("State: <invalid>");
          end if;
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer (Use_Enum => True);
    begin
       V := (Use_Enum => False, I => 3);
       Display_State_Value (V);
    end Show_Unchecked_Union;


However, in general, you should avoid using the :ada:`Unchecked_Union` aspect
due to the potential issues you might introduce into your application. In the
majority of the cases, you don't need it at all |mdash| except for special
cases such as when interfacing with C code that makes use of union types or
solving very specific problems when doing low-level programming.

Shared variable control
-----------------------

Ada has built-in support for handling both volatile and atomic data. Let's
start by discussing volatile objects.

Volatile
~~~~~~~~

A `volatile <https://en.wikipedia.org/wiki/Volatile_(computer_programming)>`_
object can be described as an object in memory whose value may change between
two consecutive memory accesses of a process A |mdash| even if process A itself
hasn't changed the value. This situation may arise when an object in memory is
being shared by multiple threads. For example, a thread *B* may modify the
value of that object between two read accesses of a thread *A*. Another typical
example is the one of
`memory-mapped I/O <https://en.wikipedia.org/wiki/Memory-mapped_I/O>`_, where
the hardware might be constantly changing the value of an object in memory.

Because the value of a volatile object may be constantly changing, a compiler
cannot generate code to store the value of that object in a register and then
use the value from the register in subsequent operations. Storing into a
register is avoided because, if the value is stored there, it would be outdated
if another process had changed the volatile object in the meantime. Instead,
the compiler generates code in such a way that the process must read the value
of the volatile object from memory for each access.

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Volatile_Object_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Object is
       Val : Long_Float with Volatile;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Long_Float (I);
       end loop;

       Put_Line ("Val: " & Long_Float'Image (Val));
    end Show_Volatile_Object;

In this example, :ada:`Val` has the :ada:`Volatile` aspect, which makes the
object volatile. We can also use the :ada:`Volatile` aspect in type
declarations. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Volatile_Type

    package Shared_Var_Types is

       type Volatile_Long_Float is new Long_Float with Volatile;

    end Shared_Var_Types;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Volatile_Type is
       Val : Volatile_Long_Float;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Volatile_Long_Float (I);
       end loop;

       Put_Line ("Val: " & Volatile_Long_Float'Image (Val));
    end Show_Volatile_Type;

Here, we're declaring a new type :ada:`Volatile_Long_Float` in the
:ada:`Shared_Var_Types` package. This type is based on the :ada:`Long_Float`
type and uses the :ada:`Volatile` aspect. Any object of this type is
automatically volatile.

In addition to that, we can declare components of an array to be volatile. In
this case, we can use the :ada:`Volatile_Components` aspect in the array
declaration. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Volatile_Array_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Array_Components is
       Arr : array (1 .. 2) of Long_Float with Volatile_Components;
    begin
       Arr := (others => 0.0);

       for I in 0 .. 999 loop
          Arr (1) := Arr (1) +  2.0 * Long_Float (I);
          Arr (2) := Arr (2) + 10.0 * Long_Float (I);
       end loop;

       Put_Line ("Arr (1): " & Long_Float'Image (Arr (1)));
       Put_Line ("Arr (2): " & Long_Float'Image (Arr (2)));
    end Show_Volatile_Array_Components;

Note that it's possible to use the :ada:`Volatile` aspect for the array
declaration as well:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Volatile_Array

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Long_Float with Volatile;

    end Shared_Var_Types;

Note that, if the :ada:`Volatile` aspect is specified for an object, then the
:ada:`Volatile_Components` aspect is also specified automatically |mdash| if it
makes sense in the context, of course. In the example above, even though
:ada:`Volatile_Components` isn't specified in the declaration of the :ada:`Arr`
array , it's automatically set as well.

Independent
~~~~~~~~~~~

When you write code to access a single object in memory, you might actually be
accessing multiple objects at once. For example, when you declare types that
make use of representation clauses |mdash| as we've seen in previous sections
|mdash|, you might be accessing multiple objects that are grouped together in
a single storage unit. For example, if you have components :ada:`A` and
:ada:`B` stored in the same storage unit, you cannot update :ada:`A` without
actually writing (the same value) to :ada:`B`. Those objects aren't
independently addressable because, in order to access one of them, we have to
actually address multiple objects at once.

When an object is independently addressable, we call it an independent object.
In this case, we make sure that, when accessing that object, we won't be
simultaneously accessing another object. As a consequence, this feature limits
the way objects can be represented in memory, as we'll see next.

To indicate that an object is independent, we use the :ada:`Independent`
aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Object

    package Shared_Var_Types is

       I : Integer with Independent;

    end Shared_Var_Types;

Similarly, we can use this aspect when declaring types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Type

    package Shared_Var_Types is

       type Independent_Boolean is new Boolean with Independent;

       type Flags is record
          F1 : Independent_Boolean;
          F2 : Independent_Boolean;
       end record;

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Independent_Boolean` type and using
it in the declaration of the :ada:`Flag` record type. Let's now derive the
:ada:`Flags` type and use a representation clause for the derived type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Type
    :class: ada-expect-compile-error

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. 0;
          F2 at 0 range 1 .. 1;
          --            ^  ERROR: start position of F2
          --                      is wrong!
          --    ^          ERROR: F1 and F2 share the
          --                      same storage unit!
       end record;

    end Shared_Var_Types.Representation;

As you can see when trying to compile this example, the representation clause
that we used for :ada:`Rep_Flags` isn't following these limitations:

1. The size of each independent component must be a multiple of a storage unit.

2. The start position of each independent component must be a multiple of a
   storage unit.

For example, for architectures that have a storage unit of one byte |mdash|
such as standard desktop computers |mdash|, this means that the size and the
position of independent components must be a multiple of a byte. Let's correct
the issues in the code above by:

- setting the size of each independent component to correspond to
  :ada:`Storage_Unit` |mdash| using a range between 0 and
  :ada:`Storage_Unit - 1` |mdash|, and

- setting the start position to zero.

This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 1 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

Note that the representation that we're now using for :ada:`Rep_Flags` is most
likely the representation that the compiler would have chosen for this data
type. We could, however, have added an empty storage unit between :ada:`F1` and
:ada:`F2` |mdash| by simply writing :ada:`F2 at 2 ...`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 2 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

As long as we follow the rules for independent objects, we're still allowed to
use representation clauses that don't correspond to the one that the compiler
might select.

For arrays, we can use the :ada:`Independent_Components` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Independent_Components

    package Shared_Var_Types is

       Flags : array (1 .. 8) of Boolean with Independent_Components;

    end Shared_Var_Types;

We've just seen in a previous example that some representation clauses might
not work with objects and types that have the :ada:`Independent` aspect. The
same restrictions apply when we use the :ada:`Independent_Components` aspect.
For example, this aspect prevents that array components are packed when the
:ada:`Pack` aspect is used. Let's discuss the following erroneous code example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Packed_Independent_Components
    :class: ada-expect-compile-error

    package Shared_Var_Types is

       type Flags is array (Positive range <>) of Boolean
         with Independent_Components, Pack;

       F : Flags (1 .. 8) with Size => 8;

    end Shared_Var_Types;

As expected, this code doesn't compile. Here, we can have either independent
components, or packed components. We cannot have both at the same time because
packed components aren't independently addressable. The compiler warns us that
the :ada:`Pack` aspect won't have any effect on independent components. When we
use the :ada:`Size` aspect in the declaration of :ada:`F`, we confirm this
limitation. If we remove the :ada:`Size` aspect, however, the code is compiled
successfully because the compiler ignores the :ada:`Pack` aspect and allocates
a larger size for :ada:`F`:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Packed_Independent_Components

    package Shared_Var_Types is

       type Flags is array (Positive range <>) of Boolean
         with Independent_Components, Pack;

    end Shared_Var_Types;

    with Ada.Text_IO;      use Ada.Text_IO;
    with System;

    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Flags_Size is
       F : Flags (1 .. 8);
    begin
       Put_Line ("Flags'Size:      "
                 & F'Size'Image & " bits");
       Put_Line ("Flags (1)'Size:  "
                 & F (1)'Size'Image & " bits");
       Put_Line ("# storage units: "
                 & Integer'Image (F'Size / System.Storage_Unit));
    end Show_Flags_Size;

As you can see in the output of the application, even though we specify the
:ada:`Pack` aspect for the :ada:`Flags` type, the compiler allocates eight
storage units, one per each component of the :ada:`F` array.

Atomic
~~~~~~

An atomic object is an object that only accepts atomic reads and updates. The
Ada standard specifies that "for an atomic object (including an atomic
component), all reads and updates of the object as a whole are indivisible."
In this case, the compiler must generate Assembly code in such a way that reads
and updates of an atomic object must be done in a single instruction, so that
no other instruction could execute on that same object before the read or
update completes.

.. admonition:: In other contexts

    Generally, we can say that operations are said to be atomic when they can
    be completed without interruptions. This is an important requirement when
    we're performing operations on objects in memory that are shared between
    multiple processes.

    This definition of atomicity above is used, for example, when implementing
    databases. However, for this section, we're using the term "atomic"
    differently. Here, it really means that reads and updates must be performed
    with a single Assembly instruction.

    For example, if we have a 32-bit object composed of four 8-bit bytes, the
    compiler cannot generate code to read or update the object using four 8-bit
    store / load instructions, or even two 16-bit store / load instructions.
    In this case, in order to maintain atomicity, the compiler must generate
    code using one 32-bit store / load instruction.

    Because of this strict definition, we might have objects for which the
    :ada:`Atomic` aspect cannot be specified. Lots of machines support integer
    types that are larger than the native word-sized integer. For example, a
    16-bit machine probably supports both 16-bit and 32-bit integers, but only
    16-bit integer objects can be marked as atomic |mdash| or, more generally,
    only objects that fit into at most 16 bits.

Atomicity may be important, for example, when dealing with shared hardware
registers. In fact, for certain architectures, the hardware may require that
memory-mapped registers are handled atomically. In Ada, we can use the
:ada:`Atomic` aspect to indicate that an object is atomic. This is how we can
use the aspect to declare a shared hardware register:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Atomic_Object

    with System;

    package Shared_Var_Types is

    private
       R   : Integer
         with Atomic, Address => System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

Note that the :ada:`Address` aspect allows for assigning a variable to a
specific location in the memory. In this example, we're using this aspect to
specify the address of the memory-mapped register.

In addition to atomic objects, we can declare atomic types |mdash| similar to
what we've seen before for volatile objects. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Atomic_Types

    with System;

    package Shared_Var_Types is

       type Atomic_Integer is new Integer with Atomic;

    private
       R : Atomic_Integer with Address => System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Atomic_Integer` type, which is an
atomic type. Objects of this type |mdash| such as :ada:`R` in this example
|mdash| are automatically atomic.

We can also declare atomic array components:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Atomic_Array_Components

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Integer with Atomic_Components;

    end Shared_Var_Types;

This example shows the declaration of the :ada:`Arr` array, which has atomic
components |mdash| the atomicity of its components is indicated by the
:ada:`Atomic_Components` aspect.

Note that if an object is atomic, it is also volatile and independent. In other
words, these type declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Atomic_Volatile_Independent

    package Shared_Var_Types is

       type Atomic_Integer_1 is new Integer with Atomic;

       type Atomic_Integer_2 is new Integer
         with Atomic,
              Volatile,
              Independent;

    end Shared_Var_Types;

A simular rule applies to components of an array. When we use the
:ada:`Atomic_Components`, the following aspects are implied: :ada:`Volatile`,
:ada:`Volatile_Components` and :ada:`Independent_Components`. For example,
these array declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Atomic_Volatile_Independent

    package Shared_Var_Types is

       Arr_1 : array (1 .. 2) of Integer with Atomic_Components;

       Arr_2 : array (1 .. 2) of Integer
         with Atomic_Components,
              Volatile,
              Volatile_Components,
              Independent_Components;

    end Shared_Var_Types;

..
    REMOVED FROM THIS SECTION, TO BE RE-EVALUATED:

    .. admonition:: Relevant topics

        - **Briefly** discuss :ada:`Full_Access_Only`
        - `The Package System.Atomic_Operations <http://www.ada-auth.org/standards/2xrm/html/RM-C-6-1.html>`_

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Discarding names
    ----------------

    .. admonition:: Relevant topics

        - **Briefly** discuss discarding name mentioned in
        `Aspect Discard_Names <http://www.ada-auth.org/standards/2xrm/html/RM-C-5.html>`_
