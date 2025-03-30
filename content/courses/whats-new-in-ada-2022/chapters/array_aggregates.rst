.. include:: ../../global.txt

Advanced Array Aggregates
=========================

.. note::

    These array aggregates are supported by

    * GNAT Community Edition 2020
    * GCC 11

.. _Whats_New_Ada_2022_Array_Aggregates:

Square brackets
---------------

In Ada 2022, you can use square brackets in array aggregates. Using
square brackets simplifies writing both empty aggregates and
single-element aggregates. Consider this:

.. code:: ada compile_button project=Courses.Ada_2022_Whats_New.Square_Brackets switches=Compiler(-gnat2022,-gnatX);

   package Show_Square_Brackets is

      type Integer_Array is array (Positive range <>) of Integer;

      Old_Style_Empty : Integer_Array := (1 .. 0 => <>);
      New_Style_Empty : Integer_Array := [];

      Old_Style_One_Item : Integer_Array := (1 => 5);
      New_Style_One_Item : Integer_Array := [5];

   end Show_Square_Brackets;

.. admonition:: Short summary for parentheses and brackets

   * Record aggregates use parentheses
   * :ref:`Container aggregates <container_aggregates>` use square brackets
   * Array aggregates can use both square brackets and parentheses, but
     parentheses usage is obsolescent

Iterated Component Association
------------------------------

There is a new kind of component association:

.. code-block:: ada

   Vector : Integer_Array := [for J in 1 .. 5 => J * 2];

This association starts with :ada:`for` keyword, just like a quantified
expression. It declares an index parameter that you can use in the
computation of a component.

Iterated component associations can nest and can be nested in another
association (iterated or not). Here we use this to define a square matrix:

.. code-block:: ada

   Matrix : array (1 .. 3, 1 .. 3) of Positive :=
    [for J in 1 .. 3 =>
      [for K in 1 .. 3 => J * 10 + K]];


Iterated component associations in this form provide both element indices
and values, just like named component associations:

.. code-block:: ada

   Data : Integer_Array (1 .. 5) :=
     [for J in 2 .. 3 => J, 5 => 5, others => 0];

Here :ada:`Data` contains :ada:`(0, 2, 3, 0, 5)`, not :ada:`(2, 3, 5, 0, 0)`.

Another form of iterated component association corresponds to a positional
component association and provides just values, but no element indices:

.. code-block:: ada

   Vector_2 : Integer_Array := [for X of Vector => X / 2];

You cannot mix these forms in a single aggregate.

It's interesting that such aggregates were originally proposed more than 25
years ago!

Complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Iterated_Component_Association switches=Compiler(-gnat2022,-gnatX);

   with Ada.Text_IO;

   procedure Show_Iterated_Component_Association is

      type Integer_Array is array (Positive range <>) of Integer;

      Old_Style_Empty : Integer_Array := (1 .. 0 => <>);
      New_Style_Empty : Integer_Array := [];

      Old_Style_One_Item : Integer_Array := (1 => 5);
      New_Style_One_Item : Integer_Array := [5];

      Vector : constant Integer_Array := [for J in 1 .. 5 => J * 2];

      Matrix : constant array (1 .. 3, 1 .. 3) of Positive :=
        [for J in 1 .. 3 =>
          [for K in 1 .. 3 => J * 10 + K]];

      Data : constant Integer_Array (1 .. 5) :=
        [for J in 2 .. 3 => J, 5 => 5, others => 0];

      Vector_2 : constant Integer_Array := [for X of Vector => X / 2];
   begin
      Ada.Text_IO.Put_Line (Vector'Image);
      Ada.Text_IO.Put_Line (Matrix'Image);
      Ada.Text_IO.Put_Line (Data'Image);
      Ada.Text_IO.Put_Line (Vector_2'Image);
   end Show_Iterated_Component_Association;

References
----------

* :aarm22:`ARM 4.3.3 Array Aggregates <4-3-3>`
* AI12-0212-1_
* AI12-0306-1_

 .. _AI12-0212-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0212-1.TXT
 .. _AI12-0306-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0306-1.TXT
