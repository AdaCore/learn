.. include:: ../../global.txt

Advanced Array Aggregates
=========================

.. note::

    Such array aggregates are supported by

    * GNAT Community Edition 2020
    * GCC 11

Square brackets
---------------

In Ada 2022 you can use square brackets in array aggregates. Square
brackets usage simplifies writing of an empty aggregate and a single
element aggregate. Consider this:

.. code:: ada compile_button manual_chop project=Courses.Ada_2022_Whats_New.Show_Square_Brackets

   !show_square_brackets.ads
   pragma Ada_2022;
   pragma Extensions_Allowed (On);

   package Show_Square_Brackets is

      type Integer_Array is array (Positive range <>) of Integer;

      Old_Style_Empty : Integer_Array := (1 .. 0 => <>);
      New_Style_Empty : Integer_Array := [];

      Old_Style_One_Item : Integer_Array := (1 => 5);
      New_Style_One_Item : Integer_Array := [5];

   end Show_Square_Brackets;

Apart from zero and one element aggregates, there is no other difference
between brackets and parentheses in array aggregates.

Iterated Component Association
------------------------------

There is a new kind of component association:

.. code-block:: ada

   Vector : Integer_Array := (for J in 1 .. 5 => J * 2);

The association starts with :ada:`for` keyword, just like a quantified
expression. It declares an index parameter that can be used in a
component computation.

Of cource, iterated component association could nest and be nested in
another associtation (iterated or not). But you can't mix iterated and
not iterated association in one list. Here is a square matrix definition:

.. code-block:: ada

   Matrix : array (1 .. 3, 1 .. 3) of Positive :=
    (for J in 1 .. 3 =>
      (for K in 1 .. 3 => J * 10 + K));

Intresting that such aggregates were originally proposed more than 25
years ago!

Complete code snippet:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Array_Aggregates

   !main.adb
   pragma Ada_2022;
   pragma Extensions_Allowed (On);  --  for square brackets

   with Ada.Text_IO;

   procedure Main is

      type Integer_Array is array (Positive range <>) of Integer;

      Old_Style_Empty : Integer_Array := (1 .. 0 => <>);
      New_Style_Empty : Integer_Array := [];

      Old_Style_One_Item : Integer_Array := (1 => 5);
      New_Style_One_Item : Integer_Array := [5];

      Vector : constant Integer_Array := [for J in 1 .. 5 => J * 2];

      Matrix : constant array (1 .. 3, 1 .. 3) of Positive :=
        (for J in 1 .. 3 =>
          (for K in 1 .. 3 => J * 10 + K));
   begin
      Ada.Text_IO.Put_Line (Vector'Image);
      Ada.Text_IO.Put_Line (Matrix'Image);
   end Main;

References
----------

* `ARM 4.3.3 Array Aggregates`_
* AI12-0212-1_
* AI12-0306-1_

 .. _`ARM 4.3.3 Array Aggregates`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-3-3.html
 .. _AI12-0212-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0212-1.TXT
 .. _AI12-0306-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0306-1.TXT
