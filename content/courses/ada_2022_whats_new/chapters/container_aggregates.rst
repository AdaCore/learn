.. include:: ../../global.txt

Container Aggregates
====================

.. note::

    Container aggregates are supported by

    * GNAT Community Edition 2021
    * GCC 11

Ada 2022 introduces container aggregates. Consider this:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Container_Aggregates

   pragma Ada_2022;

   with Ada.Text_IO;
   with Ada.Containers.Vectors;
   with Ada.Containers.Ordered_Maps;

   procedure Main is

      package Int_Vectors is new Ada.Containers.Vectors
        (Positive, Integer);

      X : constant Int_Vectors.Vector := (1, 2, 3);

      package Float_Maps is new Ada.Containers.Ordered_Maps
        (Integer, Float);

      Y : constant Float_Maps.Map := (-10 => 1.0, 0 => 2.5, 10 => 5.51);
   begin
      Ada.Text_IO.Put_Line (X'Image);
      Ada.Text_IO.Put_Line (Y'Image);
   end Main;


References:
-----------

* `ARM 4.3.5 Container Aggregates`_
* AI12-0212-1_

 .. _`ARM 4.3.5 Container Aggregates`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-3-5.html
 .. _AI12-0212-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0212-1.TXT
