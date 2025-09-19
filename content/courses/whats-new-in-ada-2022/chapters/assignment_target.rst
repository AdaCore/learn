.. include:: ../../../global.txt

Target Name Symbol (@)
======================

.. note::

    Target name symbol is supported by

    * GNAT Community Edition 2019
    * GCC 9

Ada 2022 introduces a new symbol, :ada:`@`, which can only appear on the
right hand side of an assignment statement. This symbol acts as the
equivalent of the name on the left hand side of that assignment statement.
It was introduced to avoid code duplication: instead of retyping a
(potentially long) name, you can use :ada:`@`. This symbol denotes a
constant, so you can't pass it into [:ada:`in`] :ada:`out` arguments of a
subprogram.

As an example, let's calculate some statistics for :ada:`My_Data` array:

.. code:: ada compile_button project=Courses.Ada_2022_Whats_New.Assignment_Tagged_Intro switches=Compiler(-gnat2022);

   package Statistics is

      type Statistic is record
         Count : Natural := 0;
         Total : Float := 0.0;
      end record;

      My_Data : array (1 .. 5) of Float := [for J in 1 .. 5 => Float (J)];

      Statistic_For_My_Data : Statistic;

   end Statistics;

To do this, we loop over :ada:`My_Data` elements:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Assignment_Tagged_2 switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is

      type Statistic is record
         Count : Natural := 0;
         Total : Float := 0.0;
      end record;

      My_Data : constant array (1 .. 5) of Float :=
        [for J in 1 .. 5 => Float (J)];

      Statistic_For_My_Data : Statistic;

   begin
      for Data of My_Data loop
         Statistic_For_My_Data.Count := @ + 1;
         Statistic_For_My_Data.Total := @ + Data;
      end loop;

      Ada.Text_IO.Put_Line (Statistic_For_My_Data'Image);
   end Main;

Each right hand side is evaluated only once, no matter how many :ada:`@`
symbols it contains. Let's verify this by introducing a function call that
prints a line each time it's called:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Assignment_Tagged_3 switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is

      My_Data : array (1 .. 5) of Float := [for J in 1 .. 5 => Float (J)];

      function To_Index (Value : Positive) return Positive is
      begin
         Ada.Text_IO.Put_Line ("To_Index is called.");
         return Value;
      end To_Index;

   begin
      My_Data (To_Index (1)) := @ ** 2 - 3.0 * @;
      Ada.Text_IO.Put_Line (My_Data'Image);
   end Main;

This use of :ada:`@` may look a bit cryptic, but it's the best solution
that was found.  Unlike other languages (e.g., :c:`sum += x;` in C), this
approach lets you use :ada:`@` an arbitrary number of times within the
right hand side of an assignment statement.

Alternatives
------------

In C++, the previous statement could be written with a reference
type (one line longer!):

.. code-block:: c

   auto& a = my_data[to_index(1)];
   a = a * a - 3.0 * a;

In Ada 2022, you can use a similar renaming:

.. code-block:: ada

   declare
      A renames My_Data (To_Index (1));
   begin
      A := A ** 2 - 3.0 * A;
   end;

Here we use a new short form of the rename declaration, but this still looks
too heavy, and even worse, it can't be used for discriminant-dependent
components.

References
----------

* :aarm22:`ARM 5.2.1 Target Name Symbols <5-2-1>`
* AI12-0125-3_

 .. _AI12-0125-3: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0125-3.TXT
