Standard library: Strings
=========================

.. include:: ../../global.txt

In previous chapters, we've seen source-code examples using the :ada:`String`
type, which is a fixed-length string type |mdash| essentialy, it's an array
of characters. In many cases, this data type is good enough to deal with
textual information. However, there are situations that require more advanced
text processing. Ada offers alternative approaches for these cases:

- *Bounded strings*: similar to fixed-length strings, bounded strings have a
  maximum length, which is set at its instantiation. However, bounded strings
  are not arrays of characters. At any time, they can contain a string of
  varied length |mdash| provided this length is below or equal to the maximum
  length.

- *Unbounded strings*: similar to bounded strings, unbounded strings can
  contain strings of varied length. However, in addition to that, they don't
  require a maximum length to be specified at the declaration of a string. In
  this sense, they are very flexible.

.. admonition:: For further reading...

    Although we don't specify a maximum length for unbounded string, the
    limit is :arm:`defined by the Reference Manual <A-4-5>`:

        An object of type :ada:`Unbounded_String` represents a :ada:`String`
        whose low bound is 1 and whose length can vary conceptually between 0
        and :ada:`Natural'Last`.

    Therefore, the implicit maximum length is :ada:`Natural'Last`. In
    contrast, bounded strings have an explicit maximum length that is specified
    when the :ada:`Generic_Bounded_Length` package is instantiated (as we'll
    see :ref:`later on <Intro_Ada_Bounded_Strings>`).

    Another difference between bounded and unbounded strings is the strategy
    that is used by the compiler to allocate memory for those strings. When
    using GNAT, bounded strings are allocated on the stack, while unbounded
    strings are allocated on the heap.

The following sections present an overview of the different string types and
common operations for string types.

.. _Intro_Ada_String_Operations:

String operations
-----------------

Operations on standard (fixed-length) strings are available in the
:ada:`Ada.Strings.Fixed` package. As mentioned previously, standard strings
are arrays of elements of :ada:`Character` type with *a
fixed-length*. That's why this child package is called :ada:`Fixed`.

One of the simplest operations provided is counting the number of
substrings available in a string (:ada:`Count`) and finding their
corresponding indices (:ada:`Index`). Let's look at an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Find_Substring

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Substring is

       S   : String := "Hello" & 3 * " World";
       P   : constant String := "World";
       Idx : Natural;
       Cnt : Natural;
    begin
       Cnt := Ada.Strings.Fixed.Count
         (Source  => S,
          Pattern => P);

       Put_Line ("String: " & S);
       Put_Line ("Count for '" & P & "': "
                 & Natural'Image (Cnt));

       Idx := 0;
       for I in 1 .. Cnt loop
          Idx := Index
            (Source  => S,
             Pattern => P,
             From    => Idx + 1);

          Put_Line ("Found instance of '"
                    & P & "' at position: "
                    & Natural'Image (Idx));
       end loop;

    end Show_Find_Substring;

We initialize the string :ada:`S` using a multiplication. Writing
:ada:`"Hello" & 3 * " World"` creates the string ``Hello World World World``.
We then call the function ``Count`` to get the number of instances
of the word :ada:`World` in :ada:`S`.  Next we call the function :ada:`Index` in a
loop to find the index of each instance of :ada:`World` in :ada:`S`.

.. _Intro_Ada_String_Operations_Show_Find_Words:

That example looked for instances of a specific substring.  In the next
example, we retrieve all the words in the string. We do this using
:ada:`Find_Token` and specifying whitespaces as separators. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Find_Words

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Words is

       S   : String := "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant Character_Set :=
         To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: "
                 & Integer'Image (S'Length));

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
                    & Natural'Image (F)
                    & ": '" & S (F .. L) & "'");
          --   & "-" & F'Img & "-" & L'Img

          I := L + 1;
       end loop;
    end Show_Find_Words;

We pass a set of characters to be used as delimitators to the procedure
:ada:`Find_Token`. This set is a member of the :ada:`Character_Set` type from the
:ada:`Ada.Strings.Maps` package. We call the :ada:`To_Set` function (from the
same package) to initialize the set to :ada:`Whitespace` and then call
:ada:`Find_Token` to loop over each valid index and find the starting index of
each word. We pass :ada:`Outside` to the :ada:`Test` parameter of the
:ada:`Find_Token` procedure to indicate that we're looking for indices that
are outside the :ada:`Whitespace` set, i.e. actual words. The :ada:`First` and
:ada:`Last` parameters of :ada:`Find_Token` are output parameters that indicate
the valid range of the substring. We use this information to display the
string (:ada:`S (F .. L)`).

The operations we've looked at so far read strings, but don't modify
them. We next discuss operations that change the content of strings:

+-----------------------+-----------------------------------------+
| Operation             | Description                             |
+=======================+=========================================+
| Insert                | Insert substring in a string            |
+-----------------------+-----------------------------------------+
| Overwrite             | Overwrite a string with a substring     |
+-----------------------+-----------------------------------------+
| Delete                | Delete a substring                      |
+-----------------------+-----------------------------------------+
| Trim                  | Remove whitespaces from a string        |
+-----------------------+-----------------------------------------+

All these operations are available both as functions or procedures.
Functions create a new string but procedures perform the operations in
place. The procedure will raise an exception if the constraints of the
string are not satisfied. For example, if we have a string :ada:`S` containing
10 characters, inserting a string with two characters (e.g. :ada:`"!!"`) into
it produces a string containing 12 characters. Since it has a fixed length,
we can't increase its size. One possible solution in this case is to
specify that truncation should be applied while inserting the substring.
This keeps the length of :ada:`S` fixed. Let's see an example that makes use
of both function and procedure versions of :ada:`Insert`, :ada:`Overwrite`, and
:ada:`Delete`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Adapted_Strings

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Adapted_Strings is

       S   : String := "Hello World";
       P   : constant String := "World";
       N   : constant String := "Beautiful";

       procedure Display_Adapted_String
         (Source   : String;
          Before   : Positive;
          New_Item : String;
          Pattern  : String)
       is
          S_Ins_In : String := Source;
          S_Ovr_In : String := Source;
          S_Del_In : String := Source;

          S_Ins : String :=
            Insert (Source,
                    Before,
                    New_Item & " ");
          S_Ovr : String :=
            Overwrite (Source,
                       Before,
                       New_Item);
          S_Del : String :=
            Trim (Delete (Source,
                          Before,
                          Before +
                            Pattern'Length - 1),
                  Ada.Strings.Right);
       begin
          Insert (S_Ins_In,
                  Before,
                  New_Item,
                  Right);

          Overwrite (S_Ovr_In,
                     Before,
                     New_Item,
                     Right);

          Delete (S_Del_In,
                  Before,
                  Before + Pattern'Length - 1);

          Put_Line ("Original:  '"
                    & Source & "'");

          Put_Line ("Insert:    '"
                    & S_Ins  & "'");
          Put_Line ("Overwrite: '"
                    & S_Ovr  & "'");
          Put_Line ("Delete:    '"
                    & S_Del  & "'");

          Put_Line ("Insert    (in-place): '"
                    & S_Ins_In & "'");
          Put_Line ("Overwrite (in-place): '"
                    & S_Ovr_In & "'");
          Put_Line ("Delete    (in-place): '"
                    & S_Del_In & "'");
       end Display_Adapted_String;

       Idx : Natural;
    begin
       Idx := Index
         (Source  => S,
          Pattern => P);

       if Idx > 0 then
          Display_Adapted_String (S, Idx, N, P);
       end if;
    end Show_Adapted_Strings;

In this example, we look for the index of the substring :ada:`World` and
perform operations on this substring within the outer string. The procedure
:ada:`Display_Adapted_String` uses both versions of the operations.  For the
procedural version of :ada:`Insert` and :ada:`Overwrite`, we apply truncation to
the right side of the string (:ada:`Right`). For the :ada:`Delete` procedure, we
specify the range of the substring, which is replaced by whitespaces. For
the function version of :ada:`Delete`, we also call :ada:`Trim` which trims the
trailing whitespace.

Limitation of fixed-length strings
----------------------------------

Using fixed-length strings is usually good enough for strings that are
initialized when they are declared. However, as seen in the previous
section, procedural operations on strings cause difficulties when done on
fixed-length strings because fixed-length strings are arrays of
characters. The following example shows how cumbersome the initialization
of fixed-length strings can be when it's not performed in the declaration:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Char_Array

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Char_Array is
       S : String (1 .. 15);
       --  Strings are arrays of Character
    begin
       S := "Hello          ";
       --  Alternatively:
       --
       --  #1:
       --      S (1 .. 5)      := "Hello";
       --      S (6 .. S'Last) := (others => ' ');
       --
       --  #2:
       --      S := ('H', 'e', 'l', 'l', 'o',
       --            others => ' ');

       Put_Line ("String: " & S);
       Put_Line ("String Length: "
                 & Integer'Image (S'Length));
    end Show_Char_Array;

In this case, we can't simply write :ada:`S := "Hello"` because the
resulting array of characters for the :ada:`Hello` constant has a different
length than the :ada:`S` string. Therefore, we need to include trailing
whitespaces to match the length of :ada:`S`. As shown in the example, we could
use an exact range for the initialization ( :ada:`S (1 .. 5)`) or use an
explicit array of individual characters.

When strings are initialized or manipulated at run-time, it's usually
better to use bounded or unbounded strings. An important feature of these
types is that they aren't arrays, so the difficulties presented above don't
apply. Let's start with bounded strings.

.. _Intro_Ada_Bounded_Strings:

Bounded strings
---------------

Bounded strings are defined in the
:ada:`Ada.Strings.Bounded.Generic_Bounded_Length` package. Because
this is a generic package, you need to instantiate it and set the
maximum length of the bounded string. You can then declare bounded
strings of the :ada:`Bounded_String` type.

Both bounded and fixed-length strings have a maximum length that they
can hold. However, bounded strings are not arrays, so initializing
them at run-time is much easier. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Bounded_String

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length
           (Max => 15);
       use B_Str;

       S1, S2 : Bounded_String;

       procedure Display_String_Info
         (S : Bounded_String)
       is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: "
                    & Integer'Image (Length (S)));
          --  String:
          --          S'Length => ok
          --  Bounded_String:
          --          S'Length => compilation error:
          --                      bounded strings are
          --                      not arrays!

          Put_Line ("Max.   Length: "
                    & Integer'Image (Max_Length));
       end Display_String_Info;

    begin
       S1 := To_Bounded_String ("Hello");
       Display_String_Info (S1);

       S2 := To_Bounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Bounded_String
         ("Something longer to say here...",
          Right);
       Display_String_Info (S1);
    end Show_Bounded_String;

By using bounded strings, we can easily assign to :ada:`S1` and :ada:`S2`
multiple times during execution. We use the :ada:`To_Bounded_String` and
:ada:`To_String` functions to convert, in the respective direction, between
fixed-length and bounded strings. A call to :ada:`To_Bounded_String` raises an
exception if the length of the input string is greater than the maximum
capacity of the bounded string. To avoid this, we can use the truncation
parameter (:ada:`Right` in our example).

Bounded strings are not arrays, so we can't use the :ada:`'Length`
attribute as we did for fixed-length strings. Instead, we call the
:ada:`Length` function, which returns the length of the bounded string. The
:ada:`Max_Length` constant represents the maximum length of the bounded string
that we set when we instantiated the package.

After initializing a bounded string, we can manipulate it. For example, we
can append a string to a bounded string using :ada:`Append` or concatenate
bounded strings using the :ada:`&` operator.  Like so:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Bounded_String_Op

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String_Op is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length
           (Max => 30);
       use B_Str;

       S1, S2 : Bounded_String;
    begin
       S1 := To_Bounded_String ("Hello");
       --  Alternatively:
       --
       --  A := Null_Bounded_String & "Hello";

       Append (S1, " World");
       --  Alternatively:
       --    Append (A, " World", Right);

       Put_Line ("String: " & To_String (S1));

       S2 := To_Bounded_String ("Hello!");
       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Bounded_String_Op;

We can initialize a bounded string with an empty string using the
:ada:`Null_Bounded_String` constant. Also, we can use the :ada:`Append` procedure
and specify the truncation mode like we do with the :ada:`To_Bounded_String`
function.

.. _Intro_Ada_Unbounded_Strings:

Unbounded strings
-----------------

Unbounded strings are defined in the :ada:`Ada.Strings.Unbounded` package.
This is *not* a generic package, so we don't need to instantiate it before
using the :ada:`Unbounded_String` type. As you may recall from the previous
section, bounded strings require a package instantiation.

Unbounded strings are similar to bounded strings. The main difference is
that they can hold strings of any size and adjust according to the input
string: if we assign, e.g., a 10-character string to an unbounded string
and later assign a 50-character string, internal operations in the
container ensure that memory is allocated to store the new string. In most
cases, developers don't need to worry about these operations. Also, no
truncation is necessary.

Initialization of unbounded strings is very similar to bounded strings.
Let's look at an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Unbounded_String

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Strings; use Ada.Strings;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    procedure Show_Unbounded_String is
       S1, S2 : Unbounded_String;

       procedure Display_String_Info
         (S : Unbounded_String)
       is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: "
                    & Integer'Image (Length (S)));
       end Display_String_Info;
    begin
       S1 := To_Unbounded_String ("Hello");
       --  Alternatively:
       --
       --  A := Null_Unbounded_String & "Hello";

       Display_String_Info (S1);

       S2 := To_Unbounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Unbounded_String
               ("Something longer to say here...");
       Display_String_Info (S1);
    end Show_Unbounded_String;

Like bounded strings, we can assign to :ada:`S1` and :ada:`S2` multiple times
during execution and use the :ada:`To_Unbounded_String` and :ada:`To_String`
functions to convert back-and-forth between fixed-length strings and
unbounded strings. However, in this case, truncation is not needed.

And, just like for bounded strings, you can use the :ada:`Append` procedure and
the :ada:`&` operator for unbounded strings. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Unbounded_String_Op

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    procedure Show_Unbounded_String_Op is
       S1, S2 : Unbounded_String :=
                  Null_Unbounded_String;
    begin
       S1 := S1 & "Hello";
       S2 := S2 & "Hello!";

       Append (S1, " World");
       Put_Line ("String: " & To_String (S1));

       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Unbounded_String_Op;

In this example, we're concatenating the unbounded :ada:`S1` and :ada:`S2`
strings with the :ada:`"Hello"` and :ada:`"Hello!"` strings, respectively.
Also, we're using the :ada:`Append` procedure, just like we did with bounded
strings.
