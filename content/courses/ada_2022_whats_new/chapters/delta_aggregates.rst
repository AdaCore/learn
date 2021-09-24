.. include:: ../../global.txt

Delta Aggregates
================

.. note::

    Delta aggregates are supported by

    * GNAT Community Edition 2019
    * GCC 9

Sometimes you need to create a copy of an object with a few
modifications. Before Ada 2022 it would involve a dummy object
declaration or an aggregate with associations for each property.
The dummy object approach doesn't work in contract aspects.
A limited component won't work with dummy object approach neither.
On the other side re-listing properties in an aggregate could be
too hard. So, in Ada 2022, you can use a `delta aggregates` instead.

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

There is also the delta aggregate for arrays. You can change array
elements with it, but you can't change array bounds. Also, it works
only for one-dimention arrays of non-limited components.

.. code-block:: ada

   type Vector_3D is array (1 .. 3) of Float;

   Point_2 : constant Vector_3D := (1.0, 2.0, 3.0);
   Projection_2 : constant Vector_3D := (Point_2 with delta 3 => 0.0);

You can use parentheses for array aggregates, but you can't use square
brackets for record aggregates.

Complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Delta_Aggregetes

   pragma Ada_2022;

   with Ada.Text_IO;

   procedure Main is

      type Vector is record
         X, Y, Z : Float;
      end record;

      Point_1 : constant Vector := (X => 1.0, Y => 2.0, Z => 3.0);
      Projection_1 : constant Vector := (Point_1 with delta Z => 0.0);

      type Vector_3D is array (1 .. 3) of Float;

      Point_2 : constant Vector_3D := (1.0, 2.0, 3.0);
      Projection_2 : constant Vector_3D := (Point_2 with delta 3 => 0.0);
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


References:
-----------

* `ARM 4.3.4 Delta Aggregates`_
* AI12-0127-1_

 .. _`ARM 4.3.4 Delta Aggregates`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-3-4.html
 .. _AI12-0127-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0127-1.TXT

