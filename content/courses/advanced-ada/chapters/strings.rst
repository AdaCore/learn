Strings
=======

.. include:: ../../global.txt

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

    type String is array(Positive range <>) of Character;
    type Wide_String is array(Positive range <>) of Wide_Character;
    type Wide_Wide_String is array(Positive range <>) of Wide_Wide_Character;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Wide_Char_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Wide_Char_Types is
    begin
       Put_Line ("Character'Size:           "
                 & Integer'Image (Character'Size));
       Put_Line ("Wide_Character'Size:      "
                 & Integer'Image (Wide_Character'Size));
       Put_Line ("Wide_Wide_Character'Size: "
                 & Integer'Image (Wide_Wide_Character'Size));
    end Show_Wide_Char_Types;

Let's look at another example, this time using wide strings:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Wide_String_Types

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
       TI.Put_Line ("Length:           " & Integer'Image (S'Length));
       TI.Put_Line ("Size:             " & Integer'Image (S'Size));
       TI.Put_Line ("Component_Size:   " & Integer'Image (S'Component_Size));
       TI.Put_Line ("------------------------");

       WTI.Put_Line ("Wide string:      " & WS);
       TI.Put_Line ("Length:           " & Integer'Image (WS'Length));
       TI.Put_Line ("Size:             " & Integer'Image (WS'Size));
       TI.Put_Line ("Component_Size:   " & Integer'Image (WS'Component_Size));
       TI.Put_Line ("------------------------");

       WWTI.Put_Line ("Wide-wide string: " & WWS);
       TI.Put_Line ("Length:           " & Integer'Image (WWS'Length));
       TI.Put_Line ("Size:             " & Integer'Image (WWS'Size));
       TI.Put_Line ("Component_Size:   " & Integer'Image (WWS'Component_Size));
       TI.Put_Line ("------------------------");
    end Show_Wide_String_Types;

Here, all strings (:ada:`S`, :ada:`WS` and :ada:`WWS`) have the same length of
5 characters. However, the size of each character is different |mdash| thus,
each string has a different overall size.

The recommendation is to use the :ada:`String` type when the textual
information you're processing is in standard English. In case any kind of
internationalization is needed, using :ada:`Wide_Wide_String` is probably the
best choice, as it covers all possible use-cases.

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

Wide and Wide-Wide String Handling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've just seen, we have different versions of the :ada:`Ada.Text_IO`
package. The same applies to string handling packages. As we've seen in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/standard_library_strings>`,
we can use the :ada:`Ada.Strings.Fixed` and :ada:`Ada.Strings.Maps` packages
for string handling. For other formats, we have these packages:

- :ada:`Ada.Strings.Wide_Fixed`,
- :ada:`Ada.Strings.Wide_Wide_Fixed`,
- :ada:`Ada.Strings.Wide_Maps`,
- :ada:`Ada.Strings.Wide_Wide_Maps`.

Let's look at this example from the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/standard_library_strings>`,
which we adapted for wide-wide strings:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Wide_Wide_String_Handling

    with Ada.Strings;                 use Ada.Strings;
    with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
    with Ada.Strings.Wide_Wide_Maps;  use Ada.Strings.Wide_Wide_Maps;
    with Ada.Wide_Wide_Text_IO;       use Ada.Wide_Wide_Text_IO;

    procedure Show_Find_Words is

       S   : constant Wide_Wide_String := "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant Wide_Wide_Character_Set :=
         To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: " & Integer'Wide_Wide_Image (S'Length));

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
:ada:`Wide_Wide_String` type instead.

Bounded and Unbounded Wide and Wide-Wide Strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've seen in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/standard_library_strings>`
that other kinds of :ada:`String` types are available. For example, we can
use bounded and unbounded strings |mdash| those correspond to the
:ada:`Bounded_String` and :ada:`Unbounded_String` types.

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Unbounded_Wide_Wide_String

    with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

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


String Encoding
---------------

Unicode is one of the most widespread standards for encoding writing
systems other than the Latin alphabet. It defines a format called
`Unicode Transformation Format (UTF) <https://unicode.org/faq/utf_bom.html#gen2>`_
in various versions, which vary
according to the underlying precision, support for backwards-compatibility
and other requirements.

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.WW_UTF_String

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Strings.Wide_Wide_Unbounded;
    use  Ada.Strings.Wide_Wide_Unbounded;

    procedure Show_WW_UTF_String is

       function To_UWWS (Source : Wide_Wide_String)
                         return Unbounded_Wide_Wide_String
                         renames To_Unbounded_Wide_Wide_String;

       function To_WWS (Source : Unbounded_Wide_Wide_String)
                         return Wide_Wide_String
                         renames To_Wide_Wide_String;

       Hello_World_Arabic     : constant
         UTF_8_String     := "مرحبا يا عالم";
       WWS_Hello_World_Arabic : constant
         Wide_Wide_String := Decode (Hello_World_Arabic);

       UWWS : Unbounded_Wide_Wide_String;
    begin
       UWWS := "Hello World: " & To_UWWS (WWS_Hello_World_Arabic);

       Show_WW_String : declare
          WWS : constant Wide_Wide_String := To_WWS (UWWS);
       begin
          Put_Line ("Wide_Wide_String Length: " & WWS'Length'Image);
          Put_Line ("Wide_Wide_String Size:   " & WWS'Size'Image);
       end Show_WW_String;

       Put_Line ("---------------------------------------");
       Put_Line ("Converting Wide_Wide_String to UTF-8...");

       Show_UTF_8_String : declare
          S_UTF_8 : constant UTF_8_String := Encode (To_WWS (UWWS));
       begin
          Put_Line ("UTF-8 String:        " & S_UTF_8);
          Put_Line ("UTF-8 String Length: " & S_UTF_8'Length'Image);
          Put_Line ("UTF-8 String Size:   " & S_UTF_8'Size'Image);
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
:ada:`UTF_8_String` type uses 8-bit characters to store the string in a more
efficient way (memory-wise).

The length of the string using the :ada:`Wide_Wide_String` type is equivalent
to the number of symbols we have in the original string: 26 characters /
symbols. When using UTF-8, however, we may need more 8-bit characters to
represent one symbol from the original string, so we may end up with a length
value that is bigger than the actual number of symbols from the original string
|mdash| as it is the case in this source-code example.

This difference in sizes might not always be the case. In fact, the sizes
match when encoding a symbol in UTF-8 that requires four 8-bit characters. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Emoji

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    procedure Show_Emoji is

       Emoji_Symbol     : constant UTF_8_String := "😀";
       WWS_Emoji_Symbol : constant Wide_Wide_String :=
                            Decode (Emoji_Symbol);

    begin
       Put_Line ("Wide_Wide_String Length: "
                 & WWS_Emoji_Symbol'Length'Image);
       Put_Line ("Wide_Wide_String Size:   "
                 & WWS_Emoji_Symbol'Size'Image);
       Put_Line ("UTF-8 String Length:     "
                 & Emoji_Symbol'Length'Image);
       Put_Line ("UTF-8 String Size:       "
                 & Emoji_Symbol'Size'Image);
       New_Line;
       Put_Line ("UTF-8 String:            "
                 & Emoji_Symbol);
    end Show_Emoji;

In this case, both strings |mdash| using the :ada:`Wide_Wide_String` type or
the :ada:`UTF_8_String` type |mdash| have the same size: 32 bits.

Portability of UTF-8 in source-code files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous code example, we were assuming that the format that we use for
the source-code file itself is UTF-8. This allows us to simply use emojis
|mdash| and other Unicode symbols |mdash| directly in strings:

.. code-block:: ada

    Emoji_Symbol : constant UTF_8_String := "😀";

This approach, however, might not be portable. For example, if the compiler
uses a different string encoding for source-code files, it might interpret that
Unicode symbol as something else |mdash| or just throw a compilation error.

If you're afraid that format mismatches might happen in your compilation
environment, you may want to write strings in your code in a completely
portable fashion, which consists in entering the exact sequence of codes in
bytes |mdash| using the :ada:`Character'Val` function |mdash| for the symbols
you want to use.

We can reuse parts of the previous example and replace the UTF-8 symbol with
the corresponding UTF-8 code:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Emoji

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

    procedure Show_Emoji is

       Emoji_Symbol     : constant UTF_8_String
         := Character'Val (16#f0#) & Character'Val (16#9f#) &
            Character'Val (16#98#) & Character'Val (16#80#);

    begin
       Put_Line ("UTF-8 String:            "
                 & Emoji_Symbol);
    end Show_Emoji;

Here, we use a sequence of four calls to the :ada:`Character'Val(code)`
function for the UTF-8 code that corresponds to the "😀" symbol.

UTF-16 encoding and decoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So far, we've discussed the UTF-8 encoding scheme. However, other encoding
schemes exist and are supported as well. In fact, the
:ada:`Ada.Strings.UTF_Encoding` package defines three encoding schemes:

.. code-block:: ada

    type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE);

For example, instead of using UTF-8 encoding, we can use UTF-16 encoding.
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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.UTF_16_Types

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_UTF16_Types is
       World_Emoji_UTF_8    : constant UTF_8_String := "🌐";

       World_Emoji_UTF_16   : constant UTF_16_Wide_String
         := Convert (World_Emoji_UTF_8);
       --   ^ Calling Convert for UTF_8_String
       --     to UTF_16_Wide_String conversion.

       World_Emoji_UTF_16BE : constant UTF_String
         := Convert (Item          => World_Emoji_UTF_8,
                     Input_Scheme  => UTF_8,
                     Output_Scheme => UTF_16BE);
       --   ^ Calling Convert for UTF_8_String
       --     to UTF_String conversion in UTF-16BE
       --     encoding.
    begin
       Put_Line ("UTF_8_String:          "
                 & World_Emoji_UTF_8);

       Put_Line ("UTF_16_Wide_String:    "
                 & Convert (World_Emoji_UTF_16));
       --          ^ Calling Convert for UTF_16_Wide_String
       --            to UTF_8_String conversion.

       Put_Line ("UTF_String / UTF_16BE: "
                 & Convert (Item          => World_Emoji_UTF_16BE,
                            Input_Scheme  => UTF_16BE,
                            Output_Scheme => UTF_8));
    end Show_UTF16_Types;

In this example, we're declaring a UTF-8 encoded string and storing it in the
:ada:`World_Emoji_UTF_8` constant. Then, we're calling the :ada:`Convert`
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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.WW_UTF_16_String

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_WW_UTF16_String is
       World_Emoji_UTF_16   : constant UTF_16_Wide_String
         := Wide_Character'Val (16#D83C#) &
            Wide_Character'Val (16#DF10#);
       --   ^ Calling Wide_Character'Val
       --     to specify the UTF-16 code for "🌐"

       WWS_World_Emoji      : constant Wide_Wide_String
         := Decode (World_Emoji_UTF_16);
       --   ^ Calling Decode for UTF_16_Wide_String
       --     to Wide_Wide_String conversion.
    begin
       Put_Line ("UTF_16_Wide_String: "
                 & Convert (World_Emoji_UTF_16));
       --          ^ Calling Convert for UTF_16_Wide_String
       --            to UTF_8_String conversion.

       Put_Line ("Wide_Wide_String:   "
                 & Encode (WWS_World_Emoji));
       --          ^ Calling Encode for Wide_Wide_String
       --            to UTF_8_String conversion.
    end Show_WW_UTF16_String;

In this example, we're calling the :ada:`Wide_Character'Val` function to
specify the UTF-16 code for an emoji |mdash| the "🌐" symbol. We're then using
the :ada:`Decode` function to convert between the :ada:`UTF_16_Wide_String` and
the :ada:`Wide_Wide_String` types.

.. admonition:: Relevant topics

    - `String Encoding <http://www.ada-auth.org/standards/2xrm/html/RM-A-4-11.html>`_

.. todo::

    Complete section!


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Universal text buffer
    ---------------------

    .. admonition:: Relevant topics

        - `Universal Text Buffers <http://www.ada-auth.org/standards/2xrm/html/RM-A-4-12.html>`_


Image attribute
---------------

.. admonition:: Relevant topics

    - Image attribute mentioned in
      `Image Attributes <http://www.ada-auth.org/standards/2xrm/html/RM-4-10.html>`_

.. todo::

    Complete section!


:ada:`Put_Image` aspect
-----------------------

.. admonition:: Relevant topics

    - :ada:`Put_Image` aspect mentioned in
      `Image Attributes <http://www.ada-auth.org/standards/2xrm/html/RM-4-10.html>`_

.. todo::

    Complete section!
