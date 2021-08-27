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

UTF encoding
^^^^^^^^^^^^

Unicode is one of the most widespread standards for encoding writing
systems other than the Latin alphabet. It defines a format called
`Unicode Transformation Format (UTF) <https://unicode.org/faq/utf_bom.html#gen2>`_
in various versions, which vary
according to the underlying precision, support for backwards-compatibility
and other requirements.

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

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;

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

       Hello_World_Arabic     : constant UTF_8_String := "مرحبا يا عالم";
       WWS_Hello_World_Arabic : constant Wide_Wide_String :=
                                  Decode (Hello_World_Arabic);

       UWWS : Unbounded_Wide_Wide_String;
    begin
       UWWS := "Hello World: " & To_UWWS (WWS_Hello_World_Arabic);

       Show_WW_String : declare
          WWS : constant Wide_Wide_String := To_WWS (UWWS);
       begin
          Put_Line ("Wide_Wide_String Length: " & Integer'Image (WWS'Length));
          Put_Line ("Wide_Wide_String Size:   " & Integer'Image (WWS'Size));
       end Show_WW_String;

       Put_Line ("---------------------------------------");
       Put_Line ("Converting Wide_Wide_String to UTF-8...");

       Show_UTF_8_String : declare
          S_UTF_8 : constant UTF_8_String := Encode (To_WWS (UWWS));
       begin
          Put_Line ("UTF-8 String:        " & S_UTF_8);
          Put_Line ("UTF-8 String Length: " & Integer'Image (S_UTF_8'Length));
          Put_Line ("UTF-8 String Size:   " & Integer'Image (S_UTF_8'Size));
       end Show_UTF_8_String;

    end Show_WW_UTF_String;

By running this application, we notice that, although our input string
has 14 characters in total, the corresponding UTF-8 string encoding
requires 21 bytes. The 8-bit character representation used by the
:ada:`UTF_8_String` type is good enough for storing information. However,
it is not suitable for direct string manipulation. In this case, we need
to use the :ada:`Wide_String` and :ada:`Wide_Wide_String` types. In our
example, when converting ``S8`` to ``WWS`` with a call to ``Decode``, we
create a new string that correctly stores the individual characters. Also,
we can convert back-and-forth between :ada:`UTF_8_String` and
:ada:`Wide_Wide_String` types. In the last block of our example, we
display the individual characters from the string by encoding each
character into UTF-8 format and displaying it.


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
