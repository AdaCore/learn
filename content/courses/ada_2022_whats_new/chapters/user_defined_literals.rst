.. include:: ../../global.txt

User-Defined Literals
=====================

.. note::

    User-defined literals are supported by

    * GNAT Community Edition 2020
    * GCC 11

In Ada 2022 you can use string, integer or real literals with your type.
The compiler will convert such literals to your type at run time using
a provided function.
To do so just specify one or more new aspects:

 * :ada:`Integer_Literal`
 * :ada:`Real_Literal`
 * :ada:`String_Literal`

For the demo let's define all three of them for some simple type and see
how it works. For simplicity we will use a :ada:`Wide_Wide_String`
component for internal representation:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.User_Defined_Literals

   !main.adb
   pragma Ada_2022;

   with Ada.Wide_Wide_Text_IO;
   with Ada.Characters.Conversions;

   procedure Main is

      type My_Type (Length : Natural) is record
         Value : Wide_Wide_String (1 .. Length);
      end record
        with String_Literal => From_String,
          Real_Literal      => From_Real,
          Integer_Literal   => From_Integer;

      function From_String (Value : Wide_Wide_String) return My_Type is
         ((Length => Value'Length, Value => Value));

      function From_Real (Value : String) return My_Type is
         ((Length => Value'Length,
           Value  => Ada.Characters.Conversions.To_Wide_Wide_String (Value)));

      function From_Integer (Value : String) return My_Type renames From_Real;

      procedure Print (Self : My_Type) is
      begin
         Ada.Wide_Wide_Text_IO.Put_Line (Self.Value);
      end Print;

   begin
      Print ("Test ""string""");
      Print (123);
      Print (16#DEAD_BEAF#);
      Print (2.99_792_458e+8);
   end Main;

As you see, real and integer literals are turned into strings preserving the
formatting from the source code, while string literals are decoded:
:ada:`From_String` is passed the denoted string value.
In any case, the compiler translates these literals into function calls.

Turn Ada into JavaScript
------------------------

Do you know that `'5'+3` in JavaScript is `53`?

.. code-block:: JavaScript

   > '5'+3
   '53'

Now we can get the same success with Ada! But before we need to define a
custom :ada:`+` operator:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.User_Defined_Literals_JS

   !main.adb
   pragma Ada_2022;

   with Ada.Wide_Wide_Text_IO;
   with Ada.Characters.Conversions;

   procedure Main is

      type My_Type (Length : Natural) is record
         Value : Wide_Wide_String (1 .. Length);
      end record
        with String_Literal => From_String,
          Real_Literal      => From_Real,
          Integer_Literal   => From_Integer;

      function "+" (Left, Right : My_Type) return My_Type is
        (Left.Length + Right.Length, Left.Value & Right.Value);

      function From_String (Value : Wide_Wide_String) return My_Type is
         ((Length => Value'Length, Value => Value));

      function From_Real (Value : String) return My_Type is
         ((Length => Value'Length,
           Value  => Ada.Characters.Conversions.To_Wide_Wide_String (Value)));

      function From_Integer (Value : String) return My_Type renames From_Real;

      procedure Print (Self : My_Type) is
      begin
         Ada.Wide_Wide_Text_IO.Put_Line (Self.Value);
      end Print;

   begin
      Print ("5" + 3);
   end Main;

Jokes aside, this feature is very useful, for example it allows
a "native-looking API" for :ref:`big integers<big_integers>`.

References
----------

* `ARM 4.2.1 User-Defined Literals`_
* AI12-0249-1_
* AI12-0342-1_

 .. _`ARM 4.2.1 User-Defined Literals`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-2-1.html
 .. _AI12-0249-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0249-1.TXT
 .. _AI12-0342-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0342-1.TXT
