.. include:: ../../global.txt

Target Name Symbol (@)
======================

.. note::

    Target name symbol is supported by

    * GNAT Community Edition 2019
    * GCC 9

Ada 2022 introduces a new symbol :ada:`@`. It could appears only at the
right side of an assignment statement. This symbol works as a name of
the left side of the assignment statement. It was introduced to avoid
code duplication. Instead of retyping (potentially long) name you can
just use :ada:`@` wherever you need it.

The target name symbol denotes a constant, so you can't pass it into
[:ada:`in`] :ada:`out` argument of a function.

As an example, let's calculate some statistic for My_Data array:

.. code:: ada compile_button manual_chop project=Courses.Ada_2022_Whats_New.Assignment_Tagged_Intro

   !statistics.ads
   pragma Ada_2022;

   package Statistics is

      type Statistic is record
         Count : Natural := 0;
         Total : Float := 0.0;
      end record;
   
      My_Data : array (1 .. 5) of Float := (for J in 1 .. 5 => Float (J));
   
      Statistic_For_My_Data : Statistic;
   
   end Statistics;

To do this we just loop over :ada:`My_Data` elements:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Assignment_Tagged_Loop

   !main.adb
   pragma Ada_2022;
   with Ada.Text_IO;

   procedure Main is

      type Statistic is record
         Count : Natural := 0;
         Total : Float := 0.0;
      end record;
   
      My_Data : constant array (1 .. 5) of Float :=
        (for J in 1 .. 5 => Float (J));
   
      Statistic_For_My_Data : Statistic;

   begin
      for Data of My_Data loop
         Statistic_For_My_Data.Count := @ + 1;
         Statistic_For_My_Data.Total := @ + Data;
      end loop;
   
      Ada.Text_IO.Put_Line (Statistic_For_My_Data'Image);
   end Main;

The left hand side evaluated just once, no matter how many :ada:`@` it
has. Let's check this by introducing a function call. This function
prints a line each time it's called:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Assignment_Tagged_To_Index

   !main.adb
   pragma Ada_2022;
   with Ada.Text_IO;

   procedure Main is

      My_Data : array (1 .. 5) of Float := (for J in 1 .. 5 => Float (J));
   
      function To_Index (Value : Positive) return Positive is
      begin
         Ada.Text_IO.Put_Line ("To_Index is called.");
         return Value;
      end To_Index;

   begin
      My_Data (To_Index (1)) := @ ** 2 - 3.0 * @;
      Ada.Text_IO.Put_Line (My_Data'Image);
   end Main;

Perhaps, it looks a bit cryptic, but no better solution was found.
Comparing with other languages (like :c:`sum += x;`) this approach
let you mention :ada:`@` several times on the right side of an
assigment statement, so it's more flexible.

Alternatives
------------

In C++, the previous statement could be written with a reference
type (one line longer!):

.. code-block:: c

   auto& a = my_data[to_index(1)];
   a = a * a - 3.0 * a;

In Ada 2022 you can use a corresponding renaming:

.. code-block:: ada

   declare
      A renames My_Data (To_Index (1));
   begin
      A := A ** 2 - 3.0 * A;
   end;

Here we use a new shorten form of the rename declaration, but anyway
this looks too heavy. But even worse, this can't be used for
discriminant dependent-components.

References
----------

* `ARM 5.2.1 Target Name Symbols`_
* AI12-0125-3_

 .. _`ARM 5.2.1 Target Name Symbols`: http://www.ada-auth.org/standards/2xaarm/html/AA-5-2-1.html
 .. _AI12-0125-3: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0125-3.TXT
