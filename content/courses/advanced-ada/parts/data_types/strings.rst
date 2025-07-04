Strings
=======

.. include:: ../../../global.txt

.. _Adv_Ada_Character_String_Literals:

Character and String Literals
-----------------------------

So far, we're already seen many examples of string literals |mdash| both in
the :ref:`Introduction to Ada <Intro_Ada_Course_Index>` course and in the
present course. In this section, we define them once more and discuss a couple
of details about them.


Character Literals
~~~~~~~~~~~~~~~~~~

A character literal is simply a character between apostrophes (or
*single quotation marks*). For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Character_String_Literals.Character_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Character_Literals is
       C   : Character := 'a';
       --                 ^^^
       --           Character literal
    begin
       Put_Line ("Character : " & C);
    end Show_Character_Literals;

In this example, we initialize the character variable :ada:`C` with the
character literal :ada:`'a'`.


String Literals
~~~~~~~~~~~~~~~

A string literal is simply a collection of characters between quotation marks.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Character_String_Literals.Simple_String_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_String_Literals is
       S1 : String := "Hello";
       --             ^^^^^^^
       --         String literal

       S2 : String := "World";
       --             ^^^^^^^
       --           String literal
    begin
       Put_Line (S1 & " " & S2);
    end Show_Simple_String_Literals;

In this example, :ada:`"Hello"` and :ada:`"World"` are string literals.


String literals with quotation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to include a quotation mark in a string literal, you have to write
:ada:`""` (inside that string literal):

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Character_String_Literals.String_Literals_With_Quotes

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_String_Literals_With_Quotes is
       S1 : String := "Hello";
       S2 : String := "World";
    begin
       Put_Line ("  "" " & S1
       --           ^^
       --       Quotation marks
                 & " " & S2 & " ""  ");
       --                       ^^
       --       Quotation marks

       Put_Line ("""Hello World!""");
       --         ^^            ^^
       --          Quotation marks

       Put_Line ("""""");
       --         ^^^^
       --    Quotation marks
    end Show_String_Literals_With_Quotes;

In this example, we display ``" Hello World "`` to the user by adding
quotation marks to the concatenated strings in the call to :ada:`Put_Line`.

Note that the three quotation marks at the beginning of
:ada:`"""Hello World!"""` consist of the quotation mark that indicate the
beginning of the string literal and the two quotation marks that represent a
single quotation mark inside the string literal. (The same thing happens at the
end of this string literal, but in reverse.) This string literal is displayed
as ``"Hello World!"`` to the user.

Finally, the string literal :ada:`""""""` is displayed as ``""`` to the
user.


Empty string literals
^^^^^^^^^^^^^^^^^^^^^

An empty string is represented by quotation marks without characters in
between: :ada:`""`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Character_String_Literals.Empty_String_Literals

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Empty_String_Literals is
       S1 : String          := "";
       S2 : String (1 .. 0) := "";
    begin
       Put_Line (S1);
       Put_Line (S2);
       Put_Line ("");
    end Show_Empty_String_Literals;

Note that an empty string is an array of characters without any components.
This is made explicit by the declaration of :ada:`S2`. Here, by using the range
:ada:`1 .. 0`, we're declaring an empty array.

.. todo::

    Add link to subsection on empty arrays.

.. admonition:: In other languages

    In C, an empty string still contains a single character: the null character
    (``\0``). In Ada, however, an empty string doesn't have any characters.

.. admonition:: In the Ada Reference Manual

    - :arm22:`2.5 Character Literals <2-5>`
    - :arm22:`2.6 String Literals <2-6>`


.. _Adv_Ada_Wide_Wide_Strings:

Wide and Wide-Wide Strings
--------------------------

We've seen many source-code examples so far that includes strings. In most of
them, we were using the standard string type: :ada:`String`. This type is
useful for the common use-case of displaying messages or dealing with
information in plain English. Here, we define "plain English" as the use of the
language that avoids French accents or German umlaut, for example, and doesn't
make use of any characters in non-Latin alphabets.

There are two additional string types in Ada: :ada:`Wide_String`, and
:ada:`Wide_Wide_String`. These types are particularly important when dealing
with textual information in non-standard English, or in various other
languages, non-Latin alphabets and special symbols.

These string types use different bit widths for their characters. This becomes
more apparent when looking at the type definitions:

.. code-block:: ada

    type String is
      array (Positive range <>) of Character;

    type Wide_String is
      array (Positive range <>) of Wide_Character;

    type Wide_Wide_String is
      array (Positive range <>) of
        Wide_Wide_Character;

The following table shows the typical bit-width of each character of the
string types:

+----------------------------+---------+
+ Character Type             | Width   |
+============================+=========+
| :ada:`Character`           |  8 bits |
+----------------------------+---------+
| :ada:`Wide_Character`      | 16 bits |
+----------------------------+---------+
| :ada:`Wide_Wide_Character` | 32 bits |
+----------------------------+---------+

We can see that when running this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Wide_Wide-Wide_Strings.Wide_Char_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Wide_Char_Types is
    begin
       Put_Line ("Character'Size:           "
                 & Integer'Image
                     (Character'Size));
       Put_Line ("Wide_Character'Size:      "
                 & Integer'Image
                     (Wide_Character'Size));
       Put_Line ("Wide_Wide_Character'Size: "
                 & Integer'Image
                     (Wide_Wide_Character'Size));
    end Show_Wide_Char_Types;

Let's look at another example, this time using wide strings:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Wide_Wide-Wide_Strings.Wide_String_Types

    with Ada.Text_IO;
    with Ada.Wide_Text_IO;
    with Ada.Wide_Wide_Text_IO;

    procedure Show_Wide_String_Types is
       package TI   renames Ada.Text_IO;
       package WTI  renames Ada.Wide_Text_IO;
       package WWTI renames Ada.Wide_Wide_Text_IO;

       S   : constant String           := "hello";
       WS  : constant Wide_String      := "hello";
       WWS : constant Wide_Wide_String := "hello";
    begin
       TI.Put_Line ("String:           " & S);
       TI.Put_Line ("Length:           "
                    & Integer'Image (S'Length));
       TI.Put_Line ("Size:             "
                    & Integer'Image (S'Size));
       TI.Put_Line ("Component_Size:   "
                    & Integer'Image
                        (S'Component_Size));
       TI.Put_Line ("------------------------");

       WTI.Put_Line ("Wide string:      " & WS);
       TI.Put_Line ("Length:           "
                    & Integer'Image (WS'Length));
       TI.Put_Line ("Size:             "
                    & Integer'Image (WS'Size));
       TI.Put_Line ("Component_Size:   "
                    & Integer'Image
                        (WS'Component_Size));
       TI.Put_Line ("------------------------");

       WWTI.Put_Line ("Wide-wide string: " & WWS);
       TI.Put_Line ("Length:           "
                    & Integer'Image (WWS'Length));
       TI.Put_Line ("Size:             "
                    & Integer'Image (WWS'Size));
       TI.Put_Line ("Component_Size:   "
                    & Integer'Image
                        (WWS'Component_Size));
       TI.Put_Line ("------------------------");
    end Show_Wide_String_Types;

Here, all strings (:ada:`S`, :ada:`WS` and :ada:`WWS`) have the same length of
5 characters. However, the size of each character is different |mdash| thus,
each string has a different overall size.

The recommendation is to use the :ada:`String` type when the textual
information you're processing is in standard English. In case any kind of
internationalization is needed, using :ada:`Wide_Wide_String` is probably the
best choice, as it covers all possible use-cases.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.6.3 String Types <3-6-3>`

Text I/O
~~~~~~~~

Note that, in the previous example, we were using different versions of the
:ada:`Ada.Text_IO` package depending on the string type we were using:

- :ada:`Ada.Text_IO` for objects of :ada:`String` type,
- :ada:`Ada.Wide_Text_IO` for objects of :ada:`Wide_String` type,
- :ada:`Ada.Wide_Wide_Text_IO` for objects of :ada:`Wide_Wide_String` type.

In that example, we were also using package renaming to differentiate among
those packages.

Similarly, there are different versions of text I/O packages for individual
types. For example, if we want to display the value of a :ada:`Long_Integer`
variable based on the :ada:`Wide_Wide_String` type, we can select the
:ada:`Ada.Long_Integer_Wide_Wide_Text_IO` package. In fact, the list of
packages resulting from the combination of those types is quite long:

+--------------------------+--------------------------------------------------+
+ Scalar Type              | Text I/O Packages                                |
+==========================+==================================================+
| :ada:`Integer`           | - :ada:`Ada.Integer_Text_IO`                     |
|                          | - :ada:`Ada.Integer_Wide_Text_IO`                |
|                          | - :ada:`Ada.Integer_Wide_Wide_Text_IO`           |
+--------------------------+--------------------------------------------------+
| :ada:`Long_Integer`      | - :ada:`Ada.Long_Integer_Text_IO`                |
|                          | - :ada:`Ada.Long_Integer_Wide_Text_IO`           |
|                          | - :ada:`Ada.Long_Integer_Wide_Wide_Text_IO`      |
+--------------------------+--------------------------------------------------+
| :ada:`Long_Long_Integer` | - :ada:`Ada.Long_Long_Integer_Text_IO`           |
|                          | - :ada:`Ada.Long_Long_Integer_Wide_Text_IO`      |
|                          | - :ada:`Ada.Long_Long_Integer_Wide_Wide_Text_IO` |
+--------------------------+--------------------------------------------------+
| :ada:`Float`             | - :ada:`Ada.Float_Text_IO`                       |
|                          | - :ada:`Ada.Float_Wide_Text_IO`                  |
|                          | - :ada:`Ada.Float_Wide_Wide_Text_IO`             |
+--------------------------+--------------------------------------------------+
| :ada:`Long_Float`        | - :ada:`Ada.Long_Float_Text_IO`                  |
|                          | - :ada:`Ada.Long_Float_Wide_Text_IO`             |
|                          | - :ada:`Ada.Long_Float_Wide_Wide_Text_IO`        |
+--------------------------+--------------------------------------------------+
| :ada:`Long_Long_Float`   | - :ada:`Ada.Long_Long_Float_Text_IO`             |
|                          | - :ada:`Ada.Long_Long_Float_Wide_Text_IO`        |
|                          | - :ada:`Ada.Long_Long_Float_Wide_Wide_Text_IO`   |
+--------------------------+--------------------------------------------------+

Also, there are different versions of the generic packages :ada:`Integer_IO`
and :ada:`Float_IO`:

+--------------------------+--------------------------------------------------+
+ Scalar Type              | Text I/O Packages                                |
+==========================+==================================================+
| Integer types            | - :ada:`Ada.Text_IO.Integer_IO`                  |
|                          | - :ada:`Ada.Wide_Text_IO.Integer_IO`             |
|                          | - :ada:`Ada.Wide_Wide_Text_IO.Integer_IO`        |
+--------------------------+--------------------------------------------------+
| Real types               | - :ada:`Ada.Text_IO.Float_IO`                    |
|                          | - :ada:`Ada.Wide_Text_IO.Float_IO`               |
|                          | - :ada:`Ada.Wide_Wide_Text_IO.Float_IO`          |
+--------------------------+--------------------------------------------------+

.. admonition:: In the Ada Reference Manual

    - :arm22:`A.10 Text Input-Output <A-10>`
    - :arm22:`A.10.1 The Package Text_IO <A-10-1>`
    - :arm22:`A.10.8 Input-Output for Integer Types <A-10-8>`
    - :arm22:`A.10.9 Input-Output for Real Types <A-10-9>`
    - :arm22:`A.11 Wide Text Input-Output and Wide Wide Text Input-Output <A-11>`


Wide and Wide-Wide String Handling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've just seen, we have different versions of the :ada:`Ada.Text_IO`
package. The same applies to string handling packages. As we've seen in the
:ref:`Introduction to Ada course <Intro_Ada_String_Operations>`,
we can use the :ada:`Ada.Strings.Fixed` and :ada:`Ada.Strings.Maps` packages
for string handling. For other formats, we have these packages:

- :ada:`Ada.Strings.Wide_Fixed`,
- :ada:`Ada.Strings.Wide_Wide_Fixed`,
- :ada:`Ada.Strings.Wide_Maps`,
- :ada:`Ada.Strings.Wide_Wide_Maps`.

Let's look at
:ref:`this example <Intro_Ada_String_Operations_Show_Find_Words>` from the
Introduction to Ada course, which we adapted for wide-wide strings:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Wide_Wide-Wide_Strings.Wide_Wide_String_Handling

    with Ada.Strings; use Ada.Strings;

    with Ada.Strings.Wide_Wide_Fixed;
    use  Ada.Strings.Wide_Wide_Fixed;

    with Ada.Strings.Wide_Wide_Maps;
    use  Ada.Strings.Wide_Wide_Maps;

    with Ada.Wide_Wide_Text_IO;
    use  Ada.Wide_Wide_Text_IO;

    procedure Show_Find_Words is

       S   : constant Wide_Wide_String :=
               "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant
         Wide_Wide_Character_Set :=
           To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: "
                 & Integer'Wide_Wide_Image
                     (S'Length));

       while I in S'Range loop
          Find_Token
            (Source  => S,
             Set     => Whitespace,
             From    => I,
             Test    => Outside,
             First   => F,
             Last    => L);

          exit when L = 0;

          Put_Line ("Found word instance at position "
                    & F'Wide_Wide_Image
                    & ": '" & S (F .. L) & "'");

          I := L + 1;
       end loop;

    end Show_Find_Words;

In this example, we're using the :ada:`Find_Token` procedure to find the words
from the phrase stored in the :ada:`S` constant. All the operations we're using
here are similar to the ones for :ada:`String` type, but making use of the
:ada:`Wide_Wide_String` type instead. (We talk about the :ada:`Wide_Wide_Image`
attribute :ref:`later on <Adv_Ada_Wider_Versions_Image_Attribute>`.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`A.4.6 String-Handling Sets and Mappings <A-4-6>`
    - :arm22:`A.4.7 Wide_String Handling <A-4-7>`
    - :arm22:`A.4.8 Wide_Wide_String Handling <A-4-8>`


Bounded and Unbounded Wide and Wide-Wide Strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've seen in the Introduction to Ada course
that other kinds of :ada:`String` types are available. For example, we can
use :ref:`bounded <Intro_Ada_Bounded_Strings>` and
:ref:`unbounded strings <Intro_Ada_Unbounded_Strings>` |mdash| those correspond
to the :ada:`Bounded_String` and :ada:`Unbounded_String` types.

Those kinds of string types are available for :ada:`Wide_String`, and
:ada:`Wide_Wide_String`. The following table shows the available types and
corresponding packages:

+-----------------------------------+----------------------------------------+
+ Type                              | Package                                |
+===================================+========================================+
| :ada:`Bounded_Wide_String`        | :ada:`Ada.Strings.Wide_Bounded`        |
+-----------------------------------+----------------------------------------+
| :ada:`Bounded_Wide_Wide_String`   | :ada:`Ada.Strings.Wide_Wide_Bounded`   |
+-----------------------------------+----------------------------------------+
| :ada:`Unbounded_Wide_String`      | :ada:`Ada.Strings.Wide_Unbounded`      |
+-----------------------------------+----------------------------------------+
| :ada:`Unbounded_Wide_Wide_String` | :ada:`Ada.Strings.Wide_Wide_Unbounded` |
+-----------------------------------+----------------------------------------+

The same applies to text I/O for those strings. For the standard case, we have
:ada:`Ada.Text_IO.Bounded_IO` for the :ada:`Bounded_String` type and
:ada:`Ada.Text_IO.Unbounded_IO` for the :ada:`Unbounded_String` type.

For wider string types, we have:

+-----------------------------------+-----------------------------------------------------+
+ Type                              | Text I/O Package                                    |
+===================================+=====================================================+
| :ada:`Bounded_Wide_String`        | :ada:`Ada.Wide_Text_IO.Wide_Bounded_IO`             |
+-----------------------------------+-----------------------------------------------------+
| :ada:`Bounded_Wide_Wide_String`   | :ada:`Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO`   |
+-----------------------------------+-----------------------------------------------------+
| :ada:`Unbounded_Wide_String`      | :ada:`Ada.Wide_Text_IO.Wide_Unbounded_IO`           |
+-----------------------------------+-----------------------------------------------------+
| :ada:`Unbounded_Wide_Wide_String` | :ada:`Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO` |
+-----------------------------------+-----------------------------------------------------+

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Wide_Wide-Wide_Strings.Unbounded_Wide_Wide_String

    with Ada.Strings.Wide_Wide_Unbounded;
    use  Ada.Strings.Wide_Wide_Unbounded;

    with Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO;
    use  Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO;

    procedure Show_Unbounded_Wide_Wide_String is
       S : Unbounded_Wide_Wide_String
         := To_Unbounded_Wide_Wide_String ("Hello");
    begin
       S := S & Wide_Wide_String'(" hello");
       Put_Line ("Unbounded wide-wide string: " & S);
    end Show_Unbounded_Wide_Wide_String;

In this example, we're declaring a variable :ada:`S` and initializing it with
the word "Hello." Then, we're concatenating it with " hello" and displaying it.
All the operations we're using here are similar to the ones for
:ada:`Unbounded_String` type, but they've been adapted for the
:ada:`Unbounded_Wide_Wide_String` type.

.. admonition:: In the Ada Reference Manual

    - :arm22:`A.4.7 Wide_String Handling <A-4-7>`
    - :arm22:`A.4.8 Wide_Wide_String Handling <A-4-8>`
    - :arm22:`A.11 Wide Text Input-Output and Wide Wide Text Input-Output <A-11>`


String Encoding
---------------

Unicode is one of the most widespread standards for encoding writing
systems other than the Latin alphabet. It defines a format called
`Unicode Transformation Format (UTF) <https://unicode.org/faq/utf_bom.html#gen2>`_
in various versions, which vary
according to the underlying precision, support for backwards-compatibility
and other requirements.

.. admonition:: In the Ada Reference Manual

    - :arm22:`A.4.11 String Encoding <A-4-11>`

UTF-8 encoding and decoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A common UTF format is UTF-8, which encodes strings using up to four
(8-bit) bytes and is backwards-compatible with the ASCII format. While
encoding of ASCII characters requires only one byte, Chinese characters
require three bytes, for example.

In Ada applications, UTF-8 strings are indicated by using the
:ada:`UTF_8_String` from the :ada:`Ada.Strings.UTF_Encoding` package.
In order to encode from and to UTF-8 strings, we can use the :ada:`Encode`
and :ada:`Decode` functions. Those functions are specified in the child
packages of the `Ada.Strings.UTF_Encoding` package. We select the appropriate
child package depending on the string type we're using, as you can see in the
following table:

+---------------------------------+------------------------------+
| Child Package of                | Convert from / to            |
| :ada:`Ada.Strings.UTF_Encoding` |                              |
+=================================+==============================+
| :ada:`.Strings`                 | :ada:`String` type           |
+---------------------------------+------------------------------+
| :ada:`.Wide_Strings`            | :ada:`Wide_String` type      |
+---------------------------------+------------------------------+
| :ada:`.Wide_Wide_Strings`       | :ada:`Wide_Wide_String` type |
+---------------------------------+------------------------------+

Let's look at an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.WW_UTF_String

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Strings.Wide_Wide_Unbounded;
    use  Ada.Strings.Wide_Wide_Unbounded;

    procedure Show_WW_UTF_String is

       function To_UWWS
         (Source : Wide_Wide_String)
          return Unbounded_Wide_Wide_String
            renames To_Unbounded_Wide_Wide_String;

       function To_WWS
         (Source : Unbounded_Wide_Wide_String)
          return Wide_Wide_String
            renames To_Wide_Wide_String;

       Hello_World_Arabic : constant
         UTF_8_String := "مرحبا يا عالم";
       WWS_Hello_World_Arabic : constant
         Wide_Wide_String :=
           Decode (Hello_World_Arabic);

       UWWS : Unbounded_Wide_Wide_String;
    begin
       UWWS := "Hello World: "
               & To_UWWS (WWS_Hello_World_Arabic);

       Show_WW_String : declare
          WWS : constant Wide_Wide_String :=
                  To_WWS (UWWS);
       begin
          Put_Line ("Wide_Wide_String Length: "
                    & WWS'Length'Image);
          Put_Line ("Wide_Wide_String Size:   "
                    & WWS'Size'Image);
       end Show_WW_String;

       Put_Line
         ("---------------------------------------");
       Put_Line
         ("Converting Wide_Wide_String to UTF-8...");

       Show_UTF_8_String : declare
          S_UTF_8 : constant UTF_8_String :=
                      Encode (To_WWS (UWWS));
       begin
          Put_Line ("UTF-8 String:        "
                    & S_UTF_8);
          Put_Line ("UTF-8 String Length: "
                    & S_UTF_8'Length'Image);
          Put_Line ("UTF-8 String Size:   "
                    & S_UTF_8'Size'Image);
       end Show_UTF_8_String;

    end Show_WW_UTF_String;

In this application, we start by storing a string in Arabic in the
:ada:`Hello_World_Arabic` constant. We then use the :ada:`Decode` function to
convert that string from :ada:`UTF_8_String` type to :ada:`Wide_Wide_String`
type |mdash| we store it in the :ada:`WWS_Hello_World_Arabic` constant.

We use a variable of type :ada:`Unbounded_Wide_Wide_String` (:ada:`UWWS`) to
manipulate strings: we append the string in Arabic to the "Hello World: "
string and store it in :ada:`UWWS`.

In the :ada:`Show_WW_String` block, we convert the string |mdash| stored in
:ada:`UWWS` |mdash| from the :ada:`Unbounded_Wide_Wide_String` type to the
:ada:`Wide_Wide_String` type and display the length and size of the string. We
do something similar in the :ada:`Show_UTF_8_String` block, but there, we
convert to the :ada:`UTF_8_String` type.

Also, in the :ada:`Show_UTF_8_String` block, we use the :ada:`Encode` function
to convert that string from :ada:`Wide_Wide_String` type to then
:ada:`UTF_8_String` type |mdash| we store it in the :ada:`S_UTF_8` constant.

UTF-8 size and length
~~~~~~~~~~~~~~~~~~~~~

As you can see when running the last code example from the previous subsection,
we have different sizes and lengths depending on the string type:

+--------------------------+-------+--------+
| String type              | Size  | Length |
+==========================+=======+========+
| :ada:`Wide_Wide_String`  |   832 |     26 |
+--------------------------+-------+--------+
| :ada:`UTF_8_String`      |   296 |     37 |
+--------------------------+-------+--------+

The size needed for storing the string when using the :ada:`Wide_Wide_String`
type is bigger than the one when using the :ada:`UTF_8_String` type. This is
expected, as the :ada:`Wide_Wide_String` uses 32-bit characters, while the
:ada:`UTF_8_String` type uses 8-bit codes to store the string in a more
efficient way (memory-wise).

The length of the string using the :ada:`Wide_Wide_String` type is equivalent
to the number of symbols we have in the original string: 26 characters /
symbols. When using UTF-8, however, we may need more 8-bit codes to
represent one symbol from the original string, so we may end up with a length
value that is bigger than the actual number of symbols from the original string
|mdash| as it is the case in this source-code example.

This difference in sizes might not always be the case. In fact, the sizes
match when encoding a symbol in UTF-8 that requires four 8-bit codes. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    procedure Show_UTF_8 is

       Symbol_UTF_8 : constant UTF_8_String := "𝚡";
       Symbol_WWS   : constant Wide_Wide_String :=
                        Decode (Symbol_UTF_8);

    begin
       Put_Line ("Wide_Wide_String Length: "
                 & Symbol_WWS'Length'Image);
       Put_Line ("Wide_Wide_String Size:   "
                 & Symbol_WWS'Size'Image);
       Put_Line ("UTF-8 String Length:     "
                 & Symbol_UTF_8'Length'Image);
       Put_Line ("UTF-8 String Size:       "
                 & Symbol_UTF_8'Size'Image);
       New_Line;
       Put_Line ("UTF-8 String:            "
                 & Symbol_UTF_8);
    end Show_UTF_8;

In this case, both strings |mdash| using the :ada:`Wide_Wide_String` type or
the :ada:`UTF_8_String` type |mdash| have the same size: 32 bits. (Here, we're
using the :ada:`𝚡` symbol from the
:wikipedia:`Mathematical Alphanumeric Symbols block <Mathematical_Alphanumeric_Symbols>`,
not the standard "x" from the
:wikipedia:`Basic Latin block <Basic_Latin_(Unicode_block)>`.)


UTF-16 encoding and decoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So far, we've discussed the UTF-8 encoding scheme. However, other encoding
schemes exist and are supported as well. In fact, the
:ada:`Ada.Strings.UTF_Encoding` package defines three encoding schemes:

.. code-block:: ada

    type Encoding_Scheme is (UTF_8,
                             UTF_16BE,
                             UTF_16LE);

For example, instead of using UTF-8 encoding, we can use UTF-16 encoding
|mdash| either in the big-endian or in the little-endian version.
To convert between UTF-8 and UTF-16 encoding schemes, we can make use of the
conversion functions from the :ada:`Ada.Strings.UTF_Encoding.Conversions`
package.

To declare a UTF-16 encoded string, we can use one of the following data types:

- the 8-bit-character based :ada:`UTF_String` type, or

- the 16-bit-character based :ada:`UTF_16_Wide_String` type.

When using the 8-bit version, though, we have to specify the input and output
schemes when converting between UTF-8 and UTF-16 encoding schemes.

Let's see a code example that makes use of both :ada:`UTF_String` and
:ada:`UTF_16_Wide_String` types:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_16_Types

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_UTF16_Types is
       Symbols_UTF_8  : constant
         UTF_8_String := "♥♫";

       Symbols_UTF_16 : constant
         UTF_16_Wide_String :=
           Convert (Symbols_UTF_8);
       --  ^ Calling Convert for UTF_8_String
       --    to UTF_16_Wide_String conversion.

       Symbols_UTF_16BE : constant
         UTF_String :=
           Convert (Item          => Symbols_UTF_8,
                    Input_Scheme  => UTF_8,
                    Output_Scheme => UTF_16BE);
       --  ^ Calling Convert for UTF_8_String
       --    to UTF_String conversion in UTF-16BE
       --    encoding.
    begin
       Put_Line ("UTF_8_String:          "
                 & Symbols_UTF_8);

       Put_Line ("UTF_16_Wide_String:    "
                 & Convert (Symbols_UTF_16));
       --          ^ Calling Convert for
       --            the UTF_16_Wide_String to
       --            UTF_8_String conversion.

       Put_Line
         ("UTF_String / UTF_16BE: "
          & Convert
              (Item          => Symbols_UTF_16BE,
               Input_Scheme  => UTF_16BE,
               Output_Scheme => UTF_8));
    end Show_UTF16_Types;

In this example, we're declaring a UTF-8 encoded string and storing it in the
:ada:`Symbols_UTF_8` constant. Then, we're calling the :ada:`Convert`
functions to convert between UTF-8 and UTF-16 encoding schemes. We're using two
versions of this function:

- the :ada:`Convert` function that returns an object of
  :ada:`UTF_16_Wide_String` type for an input of :ada:`UTF_8_String` type, and

- the :ada:`Convert` function that returns an object of :ada:`UTF_String`
  type for an input of :ada:`UTF_8_String` type.

  - In this case, we need to specify the input and output schemes (see
    :ada:`Input_Scheme` and :ada:`Output_Scheme` parameters in the code
    example).

Previously, we've seen that the
:ada:`Ada.Strings.UTF_Encoding.Wide_Wide_Strings` package offers functions to
convert between UTF-8 and the :ada:`Wide_Wide_String` type. The same kind of
conversion functions exist for UTF-16 strings as well. Let's look at this code
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.WW_UTF_16_String

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_WW_UTF16_String is
       Symbols_UTF_16 : constant
         UTF_16_Wide_String :=
           Wide_Character'Val (16#2665#) &
           Wide_Character'Val (16#266B#);
       --  ^ Calling Wide_Character'Val
       --    to specify the UTF-16 BE code
       --    for "♥" and "♫".

       Symbols_WWS : constant
         Wide_Wide_String :=
           Decode (Symbols_UTF_16);
       --  ^ Calling Decode for UTF_16_Wide_String
       --    to Wide_Wide_String conversion.
    begin
       Put_Line ("UTF_16_Wide_String: "
                 & Convert (Symbols_UTF_16));
       --          ^ Calling Convert for the
       --            UTF_16_Wide_String to
       --            UTF_8_String conversion.

       Put_Line ("Wide_Wide_String:   "
                 & Encode (Symbols_WWS));
       --          ^ Calling Encode for the
       --            Wide_Wide_String to
       --            UTF_8_String conversion.
    end Show_WW_UTF16_String;

In this example, we're calling the :ada:`Wide_Character'Val` function to
specify the UTF-16 BE code of the "♥" and "♫" symbols. We're then using
the :ada:`Decode` function to convert between the :ada:`UTF_16_Wide_String` and
the :ada:`Wide_Wide_String` types.


UTF-8 applications
------------------

In this section, we take a further look into UTF-8 encoding and some real-world
applications. First, we discuss the use of UTF-8 encoding in source-code files.
Then, we talk about parsing UTF-8 files using *wide-wide* strings.

UTF-8 encoding in source-code files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the past, it was common to use different character sets in text files when
writing in different (human) languages. By default, Ada source-code files are
expected to use the Latin-1 coding, which is a 8-bit character set.

Nowadays, however, using UTF-8 coding for text files |mdash| including
source-code files |mdash| is very common. If your Ada code only uses standard
ASCII characters, but you're saving it in a UTF-8 coded file, there's no need
to worry about character sets, as UTF-8 is backwards compatible with ASCII.

However, you might want to use Unicode symbols in your Ada source code to
declare constants |mdash| as we did in the previous sections |mdash| and store
the source code in a UTF-8 coded file. In this case, you need be careful about
how this file is parsed by the compiler.

Let's look at this source-code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_Strings

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    procedure Show_UTF_8_Strings is

        Symbols_UTF_8 : constant
          UTF_8_String := "♥♫";

    begin
        Put_Line ("UTF_8_String: "
                  & Symbols_UTF_8);

        Put_Line ("Length:       "
                  & Symbols_UTF_8'Length'Image);

    end Show_UTF_8_Strings;

Here, we're using Unicode symbols to initialize the :ada:`Symbols_UTF_8`
constant of :ada:`UTF_8_String` type.

Now, let's assume this source-code example is stored in a UTF-8 coded file.
Because the :ada:`"♥♫"` string makes use of non-ASCII Unicode symbols,
representing this string in UTF-8 format will require more than 2 bytes.
In fact, each one of those Unicode symbols requires 2 bytes to be encoded in
UTF-8. (Keep in mind that Unicode symbols may require
:wikipedia:`between 1 to 4 bytes <UTF-8>` to be encoded in UTF-8 format.) Also,
in this case, the UTF-8 encoding process is using two additional bytes.
Therefore, the total length of the string is six, which matches what we see
when running the :ada:`Show_UTF_8_Strings` procedure. In other words, the
length of the :ada:`Symbols_UTF_8` string doesn't refer to those two characters
(:ada:`"♥♫"`) that we were using in the constant declaration, but the length of
the encoded bytes in its UTF-8 representation.

The UTF-8 format is very useful for storing and transmitting texts. However, if
we want to process Unicode symbols, it's probably better to use string types
with 32-bit characters |mdash| such as :ada:`Wide_Wide_String`. For example,
let's say we want to use the :ada:`"♥♫"` string again to initialize a constant
of :ada:`Wide_Wide_String` type:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.WWS_Strings_W8 switches=Compiler(-gnatW8);

    with Ada.Text_IO;
    with Ada.Wide_Wide_Text_IO;

    procedure Show_WWS_Strings is

       package TIO   renames Ada.Text_IO;
       package WWTIO renames Ada.Wide_Wide_Text_IO;

       Symbols_WWS : constant
         Wide_Wide_String := "♥♫";

    begin
       WWTIO.Put_Line ("Wide_Wide_String: "
                       & Symbols_WWS);

       TIO.Put_Line ("Length:           "
                     & Symbols_WWS'Length'Image);

    end Show_WWS_Strings;

In this case, as mentioned above, if we store this source code in a text file
using UTF-8 format, we need to ensure that the UTF-8 coded symbols are
correctly interpreted by the compiler when it parses the text file.
Otherwise, we might get unexpected behavior. (Interpreting the characters in
UTF-8 format as Latin-1 format is certainly an example of what we want to avoid
here.)

.. _Adv_Ada_GNAT_W8_Switch:

.. admonition:: In the GNAT toolchain

    You can use UTF-8 coding in your source-code file and initialize strings of
    32-bit characters. However, as we just mentioned, you need to make sure
    that the UTF-8 coded symbols are correctly interpreted by the compiler when
    dealing with types such as :ada:`Wide_Wide_String`. For this case, GNAT
    offers the ``-gnatW8`` switch. Let's run the previous example using this
    switch:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.WWS_Strings_W8 switches=Compiler(-gnatW8);

        with Ada.Text_IO;
        with Ada.Wide_Wide_Text_IO;

        procedure Show_WWS_Strings is

           package TIO   renames Ada.Text_IO;
           package WWTIO renames Ada.Wide_Wide_Text_IO;

           Symbols_WWS : constant
             Wide_Wide_String := "♥♫";

        begin
           WWTIO.Put_Line ("Wide_Wide_String: "
                           & Symbols_WWS);

           TIO.Put_Line ("Length:           "
                         & Symbols_WWS'Length'Image);

        end Show_WWS_Strings;

    Because the :ada:`Wide_Wide_String` type has 32-bit characters. we expect
    the length of the string to match the number of symbols that we're using.
    Indeed, when running the :ada:`Show_WWS_Strings` procedure, we see that
    the :ada:`Symbols_WWS` string has a length of two characters, which matches
    the number of characters of the :ada:`"♥♫"` string.

    When we use the ``-gnatW8`` switch, GNAT converts the UTF-8-coded string
    (:ada:`"♥♫"`) to UTF-32 format, so we get two 32-bit characters. It then
    uses the UTF-32-coded string to initialize the :ada:`Symbols_WWS` string.

    If we don't use the ``-gnatW8`` switch, however, we get wrong results.
    Let's look at the same example again without the switch:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.WWS_Strings_No_W8

        with Ada.Text_IO;
        with Ada.Wide_Wide_Text_IO;

        procedure Show_WWS_Strings is

           package TIO   renames Ada.Text_IO;
           package WWTIO renames Ada.Wide_Wide_Text_IO;

           Symbols_WWS : constant
             Wide_Wide_String := "♥♫";

        begin
           WWTIO.Put_Line ("Wide_Wide_String: "
                           & Symbols_WWS);

           TIO.Put_Line ("Length:           "
                         & Symbols_WWS'Length'Image);

        end Show_WWS_Strings;

    Now, the :ada:`"♥♫"` string is being interpreted as a string of six 8-bit
    characters. (In other words, the UTF-8-coded string isn't converted to
    the UTF-32 format.) Each of those 8-bit characters is then stored in a
    32-bit character of the :ada:`Wide_Wide_String` type. This explains why
    the :ada:`Show_WWS_Strings` procedure reports a length of 6 components for
    the :ada:`Symbols_WWS` string.

Portability of UTF-8 in source-code files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In a previous code example, we were assuming that the format that we use for
the source-code file is UTF-8. This allows us to simply use Unicode symbols
directly in strings:

.. code-block:: ada

    Symbol_UTF_8 : constant UTF_8_String := "★";

This approach, however, might not be portable. For example, if the compiler
uses a different string encoding for source-code files, it might interpret that
Unicode character as something else |mdash| or just throw a compilation error.

If you're afraid that format mismatches might happen in your compilation
environment, you may want to write strings in your code in a completely
portable fashion, which consists in entering the exact sequence of codes in
bytes |mdash| using the :ada:`Character'Val` function |mdash| for the symbols
you want to use.

We can reuse parts of the previous example and replace the UTF-8 character with
the corresponding UTF-8 code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    procedure Show_UTF_8 is

       Symbol_UTF_8 : constant
         UTF_8_String :=
           Character'Val (16#e2#)
           & Character'Val (16#98#)
           & Character'Val (16#85#);

    begin
       Put_Line ("UTF-8 String: "
                 & Symbol_UTF_8);
    end Show_UTF_8;

Here, we use a sequence of three calls to the :ada:`Character'Val(code)`
function for the UTF-8 code that corresponds to the "★" symbol.


.. _Adv_Ada_UTF_8_Files_Wide_Wide_Strings:

Parsing UTF-8 files for Wide-Wide-String processing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A typical use-case is to parse a text file in UTF-8 format and use *wide-wide*
strings to process the lines of that file. Before we look at the implementation
that does that, let's first write a procedure that generate a text file in
UTF-8 format:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_File_Processing

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    procedure Generate_UTF_8_File
      (Output_File_Name : String)
    is
       F : File_Type;
    begin
       Create (F, Out_File, Output_File_Name);
       Put_Line (F, UTF_8_String'("♥♫"));
       Put_Line
         (F,
          UTF_8_String'("مرحبا يا عالم"));
       Close (F);
    end Generate_UTF_8_File;

Procedure :ada:`Generate_UTF_8_File` writes two strings with non-Latin
characters into the UTF-8 file indicated by the :ada:`Output_File_Name`
parameter.

In addition, let's implement an auxiliary procedure to display the individual
characters of a *wide-wide* string:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_File_Processing

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    procedure Put_Line_UTF_8_Characters
      (WSS : Wide_Wide_String)
    is
       procedure Put_Complete_UTF_8_String
         (WSS : Wide_Wide_String)
       is
          S_UTF_8 : constant UTF_8_String :=
                      Encode (WSS);
       begin
          Put_Line ("STRING: " & S_UTF_8);
          Put_Line ("Length: "
                    & WSS'Length'Image
                    & " characters");
          New_Line;
       end Put_Complete_UTF_8_String;

       --  This is a wrapper function of the
       --  Encode function for the
       --  Wide_Wide_Character type:
       function Encode (Item : Wide_Wide_Character)
                        return UTF_8_String
        is
           SC : constant Wide_Wide_String (1 .. 1)
                  := (1 => Item);
           --  We need a 1-character string
           --  for the call to Encode.
       begin
           return Encode (SC);
       end Encode;

       procedure Put_UTF_8_Characters
         (WSS : Wide_Wide_String) is
       begin
          for I in WSS'Range loop
             Put (I'Image & ": ");
             Put (Encode (WSS (I)));
             New_Line;
          end loop;
       end Put_UTF_8_Characters;

    begin
        Put_Complete_UTF_8_String (WSS);
        Put_UTF_8_Characters (WSS);
        Put_Line ("--------------------");
    end Put_Line_UTF_8_Characters;

Finally, let's look at a code example that parses an UTF-8 file:

.. code:: ada run_button main=show_utf_8.adb project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_File_Processing

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Generate_UTF_8_File;
    with Put_Line_UTF_8_Characters;

    procedure Show_UTF_8 is

       File_Name : constant String :=
                     "utf-8_test.txt";

       procedure Read_UTF_8_File
         (Input_File_Name : String)
       is
          F : File_Type;
       begin
          Open (F, In_File, Input_File_Name);

          while not End_Of_File (F) loop
             declare
                S_UTF8 : constant UTF_8_String
                           := Get_Line (F);
                S      : constant Wide_Wide_String
                           := Decode (S_UTF8);
             begin
                Put_Line_UTF_8_Characters (S);
             end;
          end loop;
          Close (F);
       end Read_UTF_8_File;

    begin
       Generate_UTF_8_File (File_Name);
       Read_UTF_8_File (File_Name);
    end Show_UTF_8;

The :ada:`Show_UTF_8` procedure first calls the :ada:`Generate_UTF_8_File`
procedure to generate a text file in UTF-8 format, and then calls the nested
:ada:`Read_UTF_8_File` procedure to read from that file |mdash| this is done by
reading the 8-bit UTF-8 encoded string and decoding it into a string of
:ada:`Wide_Wide_String` type.

(Note that we call the auxiliary :ada:`Put_Line_UTF_8_Characters` procedure to
display the characters of each line we read from the UTF-8 file.)

For completeness, we include the nested :ada:`Read_Write_UTF_8_File` procedure,
which not only reads each line from a UTF-8 file, but also writes it into
another UTF-8 file:

.. code:: ada run_button main=show_utf_8.adb project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_File_Processing

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.UTF_Encoding;
    use  Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Generate_UTF_8_File;
    with Put_Line_UTF_8_Characters;

    procedure Show_UTF_8 is

       File_Name_In  : constant String :=
                         "utf-8_test.txt";
       File_Name_Out : constant String :=
                         "utf-8_copy.txt";

       procedure Read_Write_UTF_8_File
         (Input_File_Name,
         Output_File_Name : String)
       is
          F_In, F_Out : File_Type;
       begin
          Open (F_In, In_File, Input_File_Name);
          Create (F_Out, Out_File, Output_File_Name);

          while not End_Of_File (F_In) loop
             declare
                S : constant Wide_Wide_String :=
                      Decode (Get_Line (F_In));
             begin
                Put_Line_UTF_8_Characters (S);
                Put_Line (F_Out, Encode (S));
             end;
          end loop;

          Close (F_In);
          Close (F_Out);
       end Read_Write_UTF_8_File;

    begin
       Generate_UTF_8_File (File_Name_In);

       Read_Write_UTF_8_File
         (Input_File_Name  => File_Name_In,
          Output_File_Name => File_Name_Out);
    end Show_UTF_8;

In the nested :ada:`Read_Write_UTF_8_File` procedure, we see both :ada:`Decode`
and :ada:`Encode` functions being called to convert from and to the
:ada:`UTF_8_String` type, respectively.

.. admonition:: In the GNAT toolchain

    If we use the ``-gnatW8`` switch, which we mentioned
    :ref:`in a previous section <Adv_Ada_GNAT_W8_Switch>`, the implementation
    of :ada:`Generate_UTF_8_File` and :ada:`Put_Line_UTF_8_Characters` must be
    adapted. In addition, we can simplify the implementation of the
    :ada:`Show_UTF_8` procedure, too. (Note, however, that the previous
    implementation, which makes use of the :ada:`Decode` and :ada:`Encode`
    functions, would work fine as well.)

    .. code:: ada run_button main=show_utf_8.adb project=Courses.Advanced_Ada.Data_Types.Strings.String_Encoding.UTF_8_File_Processing switches=Compiler(-gnatW8);

        with Ada.Wide_Wide_Text_IO;
        use  Ada.Wide_Wide_Text_IO;

        procedure Put_Line_UTF_8_Characters
          (WSS : Wide_Wide_String)
        is
           procedure Put_Complete_UTF_8_String
             (WSS : Wide_Wide_String)
           is
           begin
              Put_Line ("STRING: " & WSS);
              Put_Line ("Length: "
                        & WSS'Length'Wide_Wide_Image
                        & " characters");
              New_Line;
           end Put_Complete_UTF_8_String;

           procedure Put_UTF_8_Characters
             (WSS : Wide_Wide_String)
           is
           begin
              for I in WSS'Range loop
                 Put (I'Wide_Wide_Image & ": ");
                 Put (WSS (I));
                 New_Line;
              end loop;
           end Put_UTF_8_Characters;

        begin
            Put_Complete_UTF_8_String (WSS);
            Put_UTF_8_Characters (WSS);
            Put_Line ("--------------------");
        end Put_Line_UTF_8_Characters;

        with Ada.Wide_Wide_Text_IO;
        use  Ada.Wide_Wide_Text_IO;

        procedure Generate_UTF_8_File
          (Output_File_Name : String)
        is
           F : File_Type;
        begin
           Create (F, Out_File, Output_File_Name);
           Put_Line (F, "♥♫");
           Put_Line (F, "مرحبا يا عالم");
           Close (F);
        end Generate_UTF_8_File;

        with Ada.Wide_Wide_Text_IO;
        use  Ada.Wide_Wide_Text_IO;

        with Generate_UTF_8_File;
        with Put_Line_UTF_8_Characters;

        procedure Show_UTF_8 is

           File_Name_In  : constant String :=
                             "utf-8_test.txt";
           File_Name_Out : constant String :=
                             "utf-8_copy.txt";

           procedure Read_Write_UTF_8_File
             (Input_File_Name,
              Output_File_Name : String)
           is
              F_In, F_Out : File_Type;
           begin
              Open (F_In, In_File, Input_File_Name);
              Create (F_Out, Out_File, Output_File_Name);

              while not End_Of_File (F_In) loop
                 declare
                    S : constant Wide_Wide_String :=
                          Get_Line (F_In);
                 begin
                    Put_Line_UTF_8_Characters (S);
                    Put_Line (F_Out, S);
                 end;
              end loop;

              Close (F_In);
              Close (F_Out);
           end Read_Write_UTF_8_File;

        begin
           Generate_UTF_8_File (File_Name_In);

           Read_Write_UTF_8_File
             (Input_File_Name  => File_Name_In,
              Output_File_Name => File_Name_Out);
        end Show_UTF_8;

    In this version of the code, we've removed all references to the
    :ada:`UTF_8_String` type |mdash| as well as the :ada:`Decode` and
    :ada:`Encode` functions that we were using to convert from and to this
    type. In this case, all UTF-8 processing happens directly using strings of
    :ada:`Wide_Wide_Strings` type.


.. _Adv_Ada_Image_Attribute:

Image attribute
---------------

Overview
~~~~~~~~

In the :ref:`Introduction to Ada <Intro_Ada_Image_Attribute>` course, we've
seen that the :ada:`Image` attribute returns a string that contains a textual
representation of an object. For example, we write :ada:`Integer'Image (V)` to
get a string for the integer variable :ada:`V`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Simple_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Image is
       V : Integer;
    begin
       V := 10;
       Put_Line ("V: " & Integer'Image (V));
    end Show_Simple_Image;

Naturally, we can use the :ada:`Image` attribute with other scalar types. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Simple_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Image is
       type Status is (Unknown, Off, On);

       V : Float;
       S : Status;
    begin
       V := 10.0;
       S := Unknown;

       Put_Line ("V: " & Float'Image (V));
       Put_Line ("S: " & Status'Image (S));
    end Show_Simple_Image;

In this example, we retrieve a string representing the floating-point
variable :ada:`V`. Also, we use :ada:`Status'Image (V)` to retrieve a string representing the textual version of the :ada:`Status`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Image Attributes <4-10>`


:ada:`Type'Image` and :ada:`Obj'Image`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also apply the :ada:`Image` attribute to an object directly:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Simple_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Image is
       V : Integer;
    begin
       V := 10;
       Put_Line ("V: " & V'Image);

       --  Equivalent to:
       --  Put_Line ("V: " & Integer'Image (V));
    end Show_Simple_Image;

In this example, the :ada:`Integer'Image (V)` and :ada:`V'Image` forms are
equivalent.


.. _Adv_Ada_Wider_Versions_Image_Attribute:

Wider versions of :ada:`Image`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Although we've been talking only about the :ada:`Image` attribute, it's
important to mention that each of the wider versions of the string types also
has a corresponding :ada:`Image` attribute. In fact, this is the attribute for
each string type:

+------------------------+----------------------------+
| Attribute              | Type of Returned String    |
+========================+============================+
| :ada:`Image`           | :ada:`String`              |
+------------------------+----------------------------+
| :ada:`Wide_Image`      | :ada:`Wide_String`         |
+------------------------+----------------------------+
| :ada:`Wide_Wide_Image` | :ada:`Wide_Wide_String`    |
+------------------------+----------------------------+

Let's see a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Wide_Wide_Image

    with Ada.Wide_Wide_Text_IO;
    use  Ada.Wide_Wide_Text_IO;

    procedure Show_Wide_Wide_Image is
       F : Float;
    begin
       F := 100.0;
       Put_Line ("F = "
                 & F'Wide_Wide_Image);
    end Show_Wide_Wide_Image;

In this example, we use the :ada:`Wide_Wide_Image` attribute to retrieve a
string of :ada:`Wide_Wide_String` type for the floating-point variable
:ada:`F`.


:ada:`Image` attribute for non-scalar types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

   This feature was introduced in Ada 2022.

In the previous code examples, we were using the :ada:`Image` attribute with
scalar types, but it isn't restricted to those types. In fact, we can also use
this attribute when dealing with non-scalar types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Non_Scalar_Image switches=Compiler(-gnat2022);

    package Simple_Records is

       type Rec is limited private;

       type Rec_Access is access Rec;

       function Init return Rec;

       type Null_Rec is null record;

    private

       type Rec is limited record
          F : Float;
          I : Integer;
       end record;

       function Init return Rec is
          ((F => 10.0, I => 4));

    end Simple_Records;

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Unchecked_Deallocation;

    with Simple_Records;
    use  Simple_Records;

    procedure Show_Non_Scalar_Image is

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Rec,
            Name   => Rec_Access);

       R_A : Rec_Access :=
         new Rec'(Init);

       N_R : Null_Rec :=
         (null record);
    begin
       R_A := new Rec'(Init);
       N_R := (null record);

       Put_Line ("R_A:     " & R_A'Image);
       Put_Line ("R_A.all: " & R_A.all'Image);
       Put_Line ("N_R:     " & N_R'Image);

       Free (R_A);
       Put_Line ("R_A:     " & R_A'Image);
    end Show_Non_Scalar_Image;

In the :ada:`Show_Non_Scalar_Image` procedure from this example, we display the
access value of :ada:`R_A` and the contents of the dereferenced access object
(:ada:`R_A.all`). Also, we see the indication that :ada:`N_R` is a null record
and :ada:`R_A` is null after the call to :ada:`Free`.

.. admonition:: Historically

    Since Ada 2022, the :ada:`Image` attribute is available for all types.
    Prior to this version of the language, it was only available for scalar
    types. (For other kind of types, programmers had to use the :ada:`Image`
    attribute for each component of a record, for example.)

    In fact, prior to Ada 2022, the :ada:`Image` attribute was described in
    the :arm22:`3.5 Scalar Types <3-5>` section of the Ada Reference Manual, as
    it was only applied to those types. Now, it is part of the new
    :arm22:`Image Attributes <4-10>` section.

Let's see another example, this time with arrays:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Array_Image switches=Compiler(-gnat2022);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Array_Image is

       type Float_Array is
         array (Positive range <>) of Float;

       FA_3C   : Float_Array (1 .. 3);
       FA_Null : Float_Array (1 .. 0);

    begin
       FA_3C   := [1.0, 3.0, 2.0];
       FA_Null := [];

       Put_Line ("FA_3C:   " & FA_3C'Image);
       Put_Line ("FA_Null: " & FA_Null'Image);
    end Show_Array_Image;

In this example, we display the values of the three components of the
:ada:`FA_3C` array. Also, we display the null array :ada:`FA_Null`.


:ada:`Image` attribute for tagged types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to untagged types, we can also use the :ada:`Image` attribute with
tagged types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Tagged_Image switches=Compiler(-gnat2022);

    package Simple_Records is

       type Rec is tagged limited private;

       function Init return Rec;

       type Rec_Child is new Rec with private;

       overriding function Init return Rec_Child;

    private

       type Status is (Unknown, Off, On);

       type Rec is tagged limited record
          F : Float;
          I : Integer;
       end record;

       function Init return Rec is
          ((F => 10.0, I => 4));

       type Rec_Child is new Rec with record
          Z : Status;
       end record;

       function Init return Rec_Child is
          (Rec'(Init) with Z => Off);

    end Simple_Records;

    with Ada.Text_IO;    use Ada.Text_IO;

    with Simple_Records; use Simple_Records;

    procedure Show_Tagged_Image is
       R       : constant Rec       := Init;
       R_Class : constant Rec'Class := Rec'(Init);
       R_C     : constant Rec_Child := Init;
    begin
       Put_Line ("R:       " & R'Image);
       Put_Line ("R_Class: " & R_Class'Image);
       Put_Line ("R_A:     " & R_C'Image);
    end Show_Tagged_Image;

In the :ada:`Show_Tagged_Image` procedure from this example, we display the
contents of the :ada:`R` object of :ada:`Rec` type and the :ada:`R_Class`
object of :ada:`Rec'Class` type. Also, we display the contents of the
:ada:`R_C` object of the :ada:`Rec_Child` type, which is derived from the
:ada:`Rec` type.


:ada:`Image` attribute for task and protected types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also apply the :ada:`Image` attribute to protected objects and tasks:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Image_Attribute.Protected_Task_Image switches=Compiler(-gnat2022);

    package Simple_Tasking is

       protected type Protected_Float (I : Integer) is

       private
          V : Float := Float (I);
       end Protected_Float;

       protected type Protected_Null is
       private
       end Protected_Null;

       task type T is
          entry Start;
       end T;

    end Simple_Tasking;

    package body Simple_Tasking is

       protected body Protected_Float is

       end Protected_Float;

       protected body Protected_Null is

       end Protected_Null;

       task body T is
       begin
          accept Start;
       end T;

    end Simple_Tasking;

    with Ada.Text_IO;    use Ada.Text_IO;

    with Simple_Tasking; use Simple_Tasking;

    procedure Show_Protected_Task_Image is

       PF : Protected_Float (0);
       PN : Protected_Null;
       T1 : T;

    begin
       Put_Line ("PF: " & PF'Image);
       Put_Line ("PN: " & PN'Image);
       Put_Line ("T1: " & T1'Image);

       T1.Start;
    end Show_Protected_Task_Image;

In this example, we display information about the protected object :ada:`PF`,
the componentless protected object :ada:`PN` and the task :ada:`T1`.


.. _Adv_Ada_Put_Image_Aspect:

:ada:`Put_Image` aspect
-----------------------

.. note::

   This feature was introduced in Ada 2022.

Overview
~~~~~~~~

In the previous section, we discussed many details about the :ada:`Image`
attribute. In the code examples from that section, we've seen the default
behavior of this attribute: the string returned by the calls to :ada:`Image`
was always in the format defined by the Ada standard.

In some situations, however, we might want to customize the string that is
returned by the :ada:`Image` attribute of a type :ada:`T`. Ada allows us to do
that via the :ada:`Put_Image` aspect. This is what we have to do:

1. Specify the :ada:`Put_Image` aspect for the type :ada:`T` and indicate a
   procedure with a specific parameter profile |mdash| let's say, for example,
   a procedure named :ada:`P`.

2. Implement the procedure :ada:`P` and write the information we want to use
   into a buffer (by calling the routines defined for :ada:`Root_Buffer_Type`,
   such as the :ada:`Put` procedure).

We can see these steps performed in the code example below:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Strings.Put_Image.Simple_Put_Image switches=Compiler(-gnat2022);

    with Ada.Strings.Text_Buffers;

    package Show_Put_Image is

       type T is null record
         with Put_Image => Put_Image_T;
       --     ^ Custom version of Put_Image

       use Ada.Strings.Text_Buffers;

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T);

    end Show_Put_Image;

    package body Show_Put_Image is

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T) is
          pragma Unreferenced (Arg);
       begin
          --  Call Put with customized
          --  information
          Buffer.Put ("<custom info>");
       end Put_Image_T;

    end Show_Put_Image;

In the :ada:`Show_Put_Image` package, we use the :ada:`Put_Image` aspect in
the declaration of the :ada:`T` type. There, we indicate that the
:ada:`Image` attribute shall use the :ada:`Put_Image_T` procedure instead
of the default version.

In the body of the :ada:`Put_Image_T` procedure, we implement our custom
version of the :ada:`Image` attribute. We do that by calling the
:ada:`Put` procedure with the information we want to provide in the
:ada:`Image` attribute. Here, we access a buffer of :ada:`Root_Buffer_Type`
type, which is defined in the :ada:`Ada.Strings.Text_Buffers` package. (We
discuss more about this package
:ref:`later on <Adv_Ada_Universal_Text_Buffer>`.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`Image Attributes <4-10>`


Complete Example of :ada:`Put_Image`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's see a complete example in which we use the :ada:`Put_Image` aspect and
write useful information to the buffer:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Put_Image.Put_Image_Custom_Numerics switches=Compiler(-gnat2022);

    with Ada.Strings.Text_Buffers;

    package Custom_Numerics is

       type Float_Integer is record
         F : Float   := 0.0;
         I : Integer := 0;
       end record
         with Dynamic_Predicate =>
                Integer (Float_Integer.F) =
                  Float_Integer.I,
              Put_Image         => Put_Float_Integer;
       --     ^ Custom version of Put_Image

       use Ada.Strings.Text_Buffers;

       procedure Put_Float_Integer
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        Float_Integer);

    end Custom_Numerics;

    package body Custom_Numerics is

       procedure Put_Float_Integer
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        Float_Integer) is
       begin
          --  Call Wide_Wide_Put with customized
          --  information
          Buffer.Wide_Wide_Put
            ("(F : "  & Arg.F'Wide_Wide_Image & ", "
             & "I : " & Arg.I'Wide_Wide_Image & ")");
       end Put_Float_Integer;

    end Custom_Numerics;

    with Ada.Text_IO;     use Ada.Text_IO;

    with Custom_Numerics; use Custom_Numerics;

    procedure Show_Put_Image is
       V : Float_Integer;
    begin
       V := (F => 100.2,
             I => 100);
       Put_Line ("V = "
                 & V'Image);
    end Show_Put_Image;

In the :ada:`Custom_Numerics` package of this example, we specify the
:ada:`Put_Image` aspect and indicate the :ada:`Put_Float_Integer` procedure.
In that procedure, we display the information of components :ada:`F` and
:ada:`I`. Then, in the :ada:`Show_Put_Image` procedure, we use the :ada:`Image`
attribute for the :ada:`V` variable and see the information in the exact format
we specified. (If you like to see the default version of the
:ada:`Put_Image` instead, you may comment out the :ada:`Put_Image` aspect part
in the declaration of :ada:`Float_Integer`.)


Relation to the :ada:`Image` attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we cannot override the :ada:`Image` attribute directly |mdash|
there's no :ada:`Image` *aspect* that we could specify. However, as we've just
seen, we can do this indirectly by using our own version of the
:ada:`Put_Image` procedure for a type :ada:`T`.

The :ada:`Image` attribute of a type :ada:`T` makes use of the procedure
indicated in the :ada:`Put_Image` aspect. Let's say we have the following
declaration:

.. code-block:: ada

    type T is null record
      with Put_Image => Put_Image_T;

When we then use the :ada:`T'Image` attribute in our code, the custom
:ada:`Put_Image_T` procedure is automatically called. This is a simplified
example of how the :ada:`Image` function is implemented:

.. code-block:: ada

   function Image (V : T)
                   return String is
      Buffer : Custom_Buffer;
      --       ^ of Root_Buffer_Type'Class
   begin
      --  Calling Put_Image procedure
      --  for type T
      Put_Image_T (Buffer, V);

      --  Retrieving the text from the
      --  buffer as a string
      return Buffer.Get;
   end Image;

In other words, the :ada:`Image` attribute basically:

- calls the :ada:`Put_Image` procedure specified in the :ada:`Put_Image`
  aspect of type :ada:`T`\ 's declaration and passes a buffer;

and

- retrieves the contents of the buffer as a string and returns it.

If the :ada:`Put_Image` aspect of type :ada:`T` isn't specified, the default
version is used. (We've seen the default version of various types
:ref:`in the previous section <Adv_Ada_Image_Attribute>` about the :ada:`Image`
attribute.)


:ada:`Put_Image` and derived types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Types that were derived from untagged types (or null extensions) make use of
the :ada:`Put_Image` procedure that was specified for
their parent type |mdash| either a custom procedure indicated in the
:ada:`Put_Image` aspect or the default one. Naturally, if a derived type
has the :ada:`Put_Image` aspect, the procedure indicated in the aspect is used
instead. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Put_Image.Untagged_Put_Image switches=Compiler(-gnat2022);

    with Ada.Strings.Text_Buffers;

    package Untagged_Put_Image is

       use Ada.Strings.Text_Buffers;

       type T is null record
         with Put_Image => Put_Image_T;

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T);

       type T_Derived_1 is new T;

       type T_Derived_2 is new T
         with Put_Image => Put_Image_T_Derived_2;

       procedure Put_Image_T_Derived_2
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T_Derived_2);

    end Untagged_Put_Image;

    package body Untagged_Put_Image is

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T) is
          pragma Unreferenced (Arg);
       begin
          Buffer.Wide_Wide_Put ("Put_Image_T");
       end Put_Image_T;

       procedure Put_Image_T_Derived_2
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T_Derived_2) is
          pragma Unreferenced (Arg);
       begin
          Buffer.Wide_Wide_Put
            ("Put_Image_T_Derived_2");
       end Put_Image_T_Derived_2;

    end Untagged_Put_Image;

    with Ada.Text_IO;        use Ada.Text_IO;

    with Untagged_Put_Image; use Untagged_Put_Image;

    procedure Show_Untagged_Put_Image is
       Obj_T           : T;
       Obj_T_Derived_1 : T_Derived_1;
       Obj_T_Derived_2 : T_Derived_2;
    begin
       Put_Line ("T'Image :           "
                 & Obj_T'Image);
       Put_Line ("T_Derived_1'Image : "
                 & Obj_T_Derived_1'Image);
       Put_Line ("T_Derived_2'Image : "
                 & Obj_T_Derived_2'Image);
    end Show_Untagged_Put_Image;

In this example, we declare the type :ada:`T` and its derived types
:ada:`T_Derived_1` and :ada:`T_Derived_2`. When running this code, we see that:

- :ada:`T_Derived_1` makes use of the :ada:`Put_Image_T` procedure from its
  parent.

    - Note that, if we remove the :ada:`Put_Image` aspect from the declaration
      of :ada:`T`, the default version of the :ada:`Put_Image` procedure is
      used for both :ada:`T` and :ada:`T_Derived_1` types.

- :ada:`T_Derived_2` makes use of the :ada:`Put_Image_T_Derived_2` procedure,
  which was indicated in the :ada:`Put_Image` aspect of that type, instead of
  its parent's procedure.


:ada:`Put_Image` and tagged types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Types that are derived from a tagged type may also inherit the :ada:`Put_Image`
aspect. However, there are a couple of small differences in comparison to
untagged types, as we can see in the following example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Put_Image.Tagged_Put_Image switches=Compiler(-gnat2022);

    with Ada.Strings.Text_Buffers;

    package Tagged_Put_Image is

       use Ada.Strings.Text_Buffers;

       type T is tagged record
          I : Integer := 0;
       end record
         with Put_Image => Put_Image_T;

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T);

       type T_Child_1 is new T with record
          I1 : Integer;
       end record;

       type T_Child_2 is new T with null record;

       type T_Child_3 is new T with record
          I3 : Integer := 0;
       end record
         with Put_Image => Put_Image_T_Child_3;

       procedure Put_Image_T_Child_3
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T_Child_3);

    end Tagged_Put_Image;

    package body Tagged_Put_Image is

       procedure Put_Image_T
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T) is
          pragma Unreferenced (Arg);
       begin
          Buffer.Wide_Wide_Put ("Put_Image_T");
       end Put_Image_T;

       procedure Put_Image_T_Child_3
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        T_Child_3) is
          pragma Unreferenced (Arg);
       begin
          Buffer.Wide_Wide_Put
            ("Put_Image_T_Child_3");
       end Put_Image_T_Child_3;

    end Tagged_Put_Image;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Tagged_Put_Image; use Tagged_Put_Image;

    procedure Show_Tagged_Put_Image is
       Obj_T         : T;
       Obj_T_Child_1 : T_Child_1;
       Obj_T_Child_2 : T_Child_2;
       Obj_T_Child_3 : T_Child_3;
    begin
       Put_Line ("T'Image :         "
                 & Obj_T'Image);
       Put_Line ("--------------------");
       Put_Line ("T_Child_1'Image : "
                 & Obj_T_Child_1'Image);
       Put_Line ("--------------------");
       Put_Line ("T_Child_2'Image : "
                 & Obj_T_Child_2'Image);
       Put_Line ("--------------------");
       Put_Line ("T_Child_3'Image : "
                 & Obj_T_Child_3'Image);
       Put_Line ("--------------------");
       Put_Line ("T'Class'Image :   "
                 & T'Class (Obj_T_Child_1)'Image);
    end Show_Tagged_Put_Image;

In this example, we declare the type :ada:`T` and its derived types
:ada:`T_Child_1`, :ada:`T_Child_2` and :ada:`T_Child_3`. When running this
code, we see that:

- for both :ada:`T_Child_1` and :ada:`T_Child_2`, the parent's
  :ada:`Put_Image` aspect (the :ada:`Put_Image_T` procedure) is called and its
  information is combined with the information from the type extension;

    - For the :ada:`T_Child_1` type, the :ada:`I1` component of the type
      extension is displayed by calling a default version of the
      :ada:`Put_Image` procedure for that component |mdash|
      ``(Put_Image_T with I1 =>  0)`` is displayed.

    - For the :ada:`T_Child_2` type, no additional information is displayed
      because this type has a null extension.

- for the :ada:`T_Child_3` type, the :ada:`Put_Image_T_Child_3` procedure,
  which was indicated in the :ada:`Put_Image` aspect of the type, is used.

Finally, class-wide types (such as :ada:`T'Class`) include additional
information. Here, the tag of the specific derived type is displayed first
|mdash| in this case, the tag of the :ada:`T_Child_1` type |mdash| and
then the actual information for the derived type is displayed.


.. _Adv_Ada_Universal_Text_Buffer:

Universal text buffer
---------------------

In the :ref:`previous section <Adv_Ada_Put_Image_Aspect>`, we've seen that the
first parameter of the procedure indicated in the :ada:`Put_Image` aspect has
the :ada:`Root_Buffer_Type'Class` type, which is defined in the
:ada:`Ada.Strings.Text_Buffers` package. In this section, we talk more about
this type and additional procedures associated with this type.

.. note::

   This feature was introduced in Ada 2022.

Overview
~~~~~~~~

We use the :ada:`Root_Buffer_Type'Class` type to implement a universal text
buffer that is used to store and retrieve information about data types. Because
this text buffer isn't associated with specific data types, it is universal
|mdash| in the sense that we can really use it for any data type, regardless of
the characteristics of this type.

In theory, we could use Ada's universal text buffer to implement applications
that actually process text in some form |mdash| for example, when implementing
a text editor. However, in general, Ada programmers are only expected to make
use of the :ada:`Root_Buffer_Type'Class` type when implementing a procedure for
the :ada:`Put_Image` aspect. For this reason, we won't discuss any kind of
type derivation |mdash| or any other kind of usages of this type |mdash| in
this section. Instead, we'll just focus on additional subprograms from the
:ada:`Ada.Strings.Text_Buffers` package.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Universal Text Buffers <A-4-12>`


Additional procedures
~~~~~~~~~~~~~~~~~~~~~

In the previous section, we used the :ada:`Put` procedure |mdash| and the
related :ada:`Wide_Put` and :ada:`Wide_Wide_Put` procedures |mdash| from the
:ada:`Ada.Strings.Text_Buffers` package. In addition to these procedures, the
package also includes:

- the :ada:`New_Line` procedure, which writes a new line marker to the text
  buffer;

- the :ada:`Increase_Indent` procedure, which increases the indentation in the
  text buffer; and

- the :ada:`Decrease_Indent` procedure, which decreases the indentation in the
  text buffer.

The :ada:`Ada.Strings.Text_Buffers` package also includes the
:ada:`Current_Indent` function, which retrieves the current indentation
counter.

Let's revisit an example from the previous section and use the procedures
mentioned above:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Strings.Universal_Text_Buffer.Put_Image_Custom_Numerics switches=Compiler(-gnat2022);

    with Ada.Strings.Text_Buffers;

    package Custom_Numerics is

       type Float_Integer is record
         F : Float;
         I : Integer;
       end record
         with Dynamic_Predicate =>
                Integer (Float_Integer.F) =
                  Float_Integer.I,
              Put_Image         => Put_Float_Integer;
       --     ^ Custom version of Put_Image

       use Ada.Strings.Text_Buffers;

       procedure Put_Float_Integer
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        Float_Integer);

    end Custom_Numerics;

    package body Custom_Numerics is

       procedure Put_Float_Integer
         (Buffer : in out Root_Buffer_Type'Class;
          Arg    :        Float_Integer) is
       begin
          Buffer.Wide_Wide_Put ("(");
          Buffer.New_Line;

          Buffer.Increase_Indent;

          Buffer.Wide_Wide_Put
            ("F : "
             & Arg.F'Wide_Wide_Image);
          Buffer.New_Line;

          Buffer.Wide_Wide_Put
            ("I : "
            & Arg.I'Wide_Wide_Image);

          Buffer.Decrease_Indent;
          Buffer.New_Line;

          Buffer.Wide_Wide_Put (")");
       end Put_Float_Integer;

    end Custom_Numerics;

    with Ada.Text_IO;     use Ada.Text_IO;

    with Custom_Numerics; use Custom_Numerics;

    procedure Show_Put_Image is
       V : Float_Integer;
    begin
       V := (F => 100.2,
             I => 100);
       Put_Line ("V = "
                 & V'Image);
    end Show_Put_Image;

In the body of the :ada:`Put_Float_Integer` procedure, we're using the
:ada:`New_Line`, :ada:`Increase_Indent` and :ada:`Decrease_Indent` procedures
to improve the format of the string returned by the :ada:`Float_Integer'Image`
attribute. Using these procedures, you can create any kind of output format
for your custom type.
