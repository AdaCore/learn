Strings
=======

.. include:: ../../global.txt

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

.. admonition:: In the Ada Reference Manual

    - :arm:`3.6.3 String Types <3-6-3>`

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

    - :arm:`A.10 Text Input-Output <A-10>`
    - :arm:`A.10.1 The Package Text_IO <A-10-1>`
    - :arm:`A.10.8 Input-Output for Integer Types <A-10-8>`
    - :arm:`A.10.9 Input-Output for Real Types <A-10-9>`
    - :arm:`A.11 Wide Text Input-Output and Wide Wide Text Input-Output <A-11>`


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
:ada:`Wide_Wide_String` type instead. (We talk about the :ada:`Wide_Wide_Image`
attribute :ref:`later on <Adv_Ada_Wider_Versions_Image_Attribute>`.)

.. admonition:: In the Ada Reference Manual

    - :arm:`A.4.6 String-Handling Sets and Mappings <A-4-6>`
    - :arm:`A.4.7 Wide_String Handling <A-4-7>`
    - :arm:`A.4.8 Wide_Wide_String Handling <A-4-8>`


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

.. admonition:: In the Ada Reference Manual

    - :arm:`A.4.7 Wide_String Handling <A-4-7>`
    - :arm:`A.4.8 Wide_Wide_String Handling <A-4-8>`
    - :arm:`A.11 Wide Text Input-Output and Wide Wide Text Input-Output <A-11>`


String Encoding
---------------

Unicode is one of the most widespread standards for encoding writing
systems other than the Latin alphabet. It defines a format called
`Unicode Transformation Format (UTF) <https://unicode.org/faq/utf_bom.html#gen2>`_
in various versions, which vary
according to the underlying precision, support for backwards-compatibility
and other requirements.

.. admonition:: In the Ada Reference Manual

    - :arm:`A.4.11 String Encoding <A-4-11>`

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.UTF_8

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.UTF_8_Strings

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

    procedure Show_UTF_8_Strings is

        Symbols_UTF_8 : constant UTF_8_String := "♥♫";

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

.. code:: ada no_button project=Courses.Advanced_Ada.Strings.WWS_Strings_W8

    with Ada.Text_IO;
    with Ada.Wide_Wide_Text_IO;

    procedure Show_WWS_Strings is

       package TIO   renames Ada.Text_IO;
       package WWTIO renames Ada.Wide_Wide_Text_IO;

       Symbols_WWS : constant Wide_Wide_String := "♥♫";

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

.. admonition:: In the GNAT toolchain

    You can use UTF-8 coding in your source-code file and initialize strings of
    32-bit characters. However, as we just mentioned, you need to make sure
    that the UTF-8 coded symbols are correctly interpreted by the compiler when
    dealing with types such as :ada:`Wide_Wide_String`. For this case, GNAT
    offers the ``-gnatW8`` switch. Let's run the previous example using this
    switch:

    .. code:: ada run_button project=Courses.Advanced_Ada.Strings.WWS_Strings_W8 switches=Compiler(-gnatW8);

        with Ada.Text_IO;
        with Ada.Wide_Wide_Text_IO;

        procedure Show_WWS_Strings is

           package TIO   renames Ada.Text_IO;
           package WWTIO renames Ada.Wide_Wide_Text_IO;

           Symbols_WWS : constant Wide_Wide_String := "♥♫";

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

    .. code:: ada run_button project=Courses.Advanced_Ada.Strings.WWS_Strings_No_W8

        with Ada.Text_IO;
        with Ada.Wide_Wide_Text_IO;

        procedure Show_WWS_Strings is

           package TIO   renames Ada.Text_IO;
           package WWTIO renames Ada.Wide_Wide_Text_IO;

           Symbols_WWS : constant Wide_Wide_String := "♥♫";

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.UTF_8

    with Ada.Text_IO;              use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

    procedure Show_UTF_8 is

       Symbol_UTF_8 : constant UTF_8_String
         := Character'Val (16#e2#) & Character'Val (16#98#) &
            Character'Val (16#85#);

    begin
       Put_Line ("UTF-8 String: "
                 & Symbol_UTF_8);
    end Show_UTF_8;

Here, we use a sequence of three calls to the :ada:`Character'Val(code)`
function for the UTF-8 code that corresponds to the "★" symbol.


UTF-16 encoding and decoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So far, we've discussed the UTF-8 encoding scheme. However, other encoding
schemes exist and are supported as well. In fact, the
:ada:`Ada.Strings.UTF_Encoding` package defines three encoding schemes:

.. code-block:: ada

    type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.UTF_16_Types

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_UTF16_Types is
       Symbols_UTF_8  : constant UTF_8_String := "♥♫";

       Symbols_UTF_16 : constant UTF_16_Wide_String
         := Convert (Symbols_UTF_8);
       --   ^ Calling Convert for UTF_8_String
       --     to UTF_16_Wide_String conversion.

       Symbols_UTF_16BE : constant UTF_String
         := Convert (Item          => Symbols_UTF_8,
                     Input_Scheme  => UTF_8,
                     Output_Scheme => UTF_16BE);
       --   ^ Calling Convert for UTF_8_String
       --     to UTF_String conversion in UTF-16BE
       --     encoding.
    begin
       Put_Line ("UTF_8_String:          "
                 & Symbols_UTF_8);

       Put_Line ("UTF_16_Wide_String:    "
                 & Convert (Symbols_UTF_16));
       --          ^ Calling Convert for UTF_16_Wide_String
       --            to UTF_8_String conversion.

       Put_Line ("UTF_String / UTF_16BE: "
                 & Convert (Item          => Symbols_UTF_16BE,
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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.WW_UTF_16_String

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Strings.UTF_Encoding.Conversions;
    use  Ada.Strings.UTF_Encoding.Conversions;

    procedure Show_WW_UTF16_String is
       Symbols_UTF_16 : constant UTF_16_Wide_String
         := Wide_Character'Val (16#2665#) &
            Wide_Character'Val (16#266B#);
       --   ^ Calling Wide_Character'Val
       --     to specify the UTF-16 BE code
       --     for "♥" and "♫".

       Symbols_WWS : constant Wide_Wide_String
         := Decode (Symbols_UTF_16);
       --   ^ Calling Decode for UTF_16_Wide_String
       --     to Wide_Wide_String conversion.
    begin
       Put_Line ("UTF_16_Wide_String: "
                 & Convert (Symbols_UTF_16));
       --          ^ Calling Convert for UTF_16_Wide_String
       --            to UTF_8_String conversion.

       Put_Line ("Wide_Wide_String:   "
                 & Encode (Symbols_WWS));
       --          ^ Calling Encode for Wide_Wide_String
       --            to UTF_8_String conversion.
    end Show_WW_UTF16_String;

In this example, we're calling the :ada:`Wide_Character'Val` function to
specify the UTF-16 BE code of the "♥" and "♫" symbols. We're then using
the :ada:`Decode` function to convert between the :ada:`UTF_16_Wide_String` and
the :ada:`Wide_Wide_String` types.


.. _Adv_Ada_Image_Attribute:

Image attribute
---------------

Overview
~~~~~~~~

In the :ref:`Introduction to Ada <Intro_Ada_Image_Attribute>` course, we've
seen that the :ada:`'Image` attribute returns a string that contains a textual
representation of an object. For example, we write :ada:`Integer'Image (V)` to
get a string for the integer variable :ada:`V`:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Simple_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Image is
       V : Integer;
    begin
       V := 10;
       Put_Line ("V: " & Integer'Image (V));
    end Show_Simple_Image;

Naturally, we can use the :ada:`Image` attribute with other scalar types. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Simple_Image

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

We can also apply the :ada:`'Image` attribute to an object directly:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Simple_Image

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Wide_Wide_Image

    with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

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

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Non_Scalar_Image

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

    pragma Ada_2022;

    with Ada.Text_IO;                use Ada.Text_IO;
    with Ada.Unchecked_Deallocation;

    with Simple_Records;             use Simple_Records;

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

    Since Ada 2022, the :ada:`'Image` attribute is available for all types.
    Prior to this version of the language, it was only available for scalar
    types. (For other kind of types, programmers had to use the :ada:`'Image`
    attribute for each component of a record, for example.)

    In fact, prior to Ada 2022, the :ada:`'Image` attribute was described in
    the :arm22:`3.5 Scalar Types <3-5>` section of the Ada Reference Manual, as
    it was only applied to those types. Now, it is part of the new
    :arm22:`Image Attributes <4-10>` section.

Let's see another example, this time with arrays:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Array_Image

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Array_Image is

       type Float_Array is array (Positive range <>) of Float;

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


:ada:`'Image` attribute for tagged types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to untagged types, we can also use the :ada:`'Image` attribute with
tagged types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Tagged_Image

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

    pragma Ada_2022;

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


:ada:`'Image` attribute for task and protected types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also apply the :ada:`'Image` attribute to protected objects and tasks:

.. code:: ada run_button project=Courses.Advanced_Ada.Strings.Protected_Task_Image

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

    pragma Ada_2022;

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

.. admonition:: Relevant topics

    - :ada:`Put_Image` aspect mentioned in
      :arm22:`Image Attributes <4-10>`

.. todo::

    Complete section!


.. _Adv_Ada_Universal_Text_Buffer:

Universal text buffer
---------------------

.. admonition:: Relevant topics

    - :arm22:`Universal Text Buffers <A-4-12>`

.. todo::

    Complete section!

