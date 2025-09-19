.. include:: ../../../global.txt

Delta Aggregates
================

.. note::

    Delta aggregates are supported by

    * GNAT Community Edition 2019
    * GCC 9

Sometimes you need to create a copy of an object, but with a few
modifications. Before Ada 2022, doing this involves a dummy object
declaration or an aggregate with associations for each property.  The dummy
object approach doesn't work in contract aspects or when there are limited
components. On the other hand, re-listing properties in an large aggregate
can be very tedious and error-prone. So, in Ada 2022, you can use a `delta
aggregate` instead.

Delta aggregate for records
---------------------------

The delta aggregate for a record type looks like this:

.. code-block:: ada

   type Vector is record
      X, Y, Z : Float;
   end record;

   Point_1 : constant Vector := (X => 1.0, Y => 2.0, Z => 3.0);

   Projection_1 : constant Vector := (Point_1 with delta Z => 0.0);

The more components you have, the more you will like the delta
aggregate.

Delta aggregate for arrays
--------------------------

You can also use delta aggregates for arrays to change elements, but not
bounds. Moreover, it only works for one-dimensional arrays of non-limited
components.

.. code-block:: ada

   type Vector_3D is array (1 .. 3) of Float;

   Point_2 : constant Vector_3D := [1.0, 2.0, 3.0];
   Projection_2 : constant Vector_3D := [Point_2 with delta 3 => 0.0];

You can use parentheses for array aggregates, but you can't use square
brackets for record aggregates.

Here is the complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Delta_Aggregates switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is

      type Vector is record
         X, Y, Z : Float;
      end record;

      Point_1 : constant Vector := (X => 1.0, Y => 2.0, Z => 3.0);
      Projection_1 : constant Vector := (Point_1 with delta Z => 0.0);

      type Vector_3D is array (1 .. 3) of Float;

      Point_2 : constant Vector_3D := [1.0, 2.0, 3.0];
      Projection_2 : constant Vector_3D := [Point_2 with delta 3 => 0.0];
   begin
      Ada.Text_IO.Put (Float'Image (Projection_1.X));
      Ada.Text_IO.Put (Float'Image (Projection_1.Y));
      Ada.Text_IO.Put (Float'Image (Projection_1.Z));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Float'Image (Projection_2 (1)));
      Ada.Text_IO.Put (Float'Image (Projection_2 (2)));
      Ada.Text_IO.Put (Float'Image (Projection_2 (3)));
      Ada.Text_IO.New_Line;
   end Main;


References
----------

* :aarm22:`ARM 4.3.4 Delta Aggregates <4-3-4>`
* AI12-0127-1_

 .. _AI12-0127-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0127-1.TXT

