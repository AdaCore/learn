.. include:: ../../../global.txt

:ada:`'Image` attribute for any type
====================================

.. note::

   Attribute :ada:`'Image` for any type is supported by

    * GNAT Community Edition 2020 and latter
    * GCC 11

:ada:`'Image` attribute for a value
-----------------------------------

Since the publication of the `Technical Corrigendum 1`_ in February
2016, the :ada:`'Image` attribute can now be applied to a value. So
instead of :ada:`My_Type'Image (Value)`, you can just write
:ada:`Value'Image`, as long as the :ada:`Value` is a name_. These two
statements are equivalent:

.. code-block:: ada

   Ada.Text_IO.Put_Line (Ada.Text_IO.Page_Length'Image);

   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Count'Image (Ada.Text_IO.Page_Length));

.. _`Technical Corrigendum 1`: https://reznikmm.github.io/ada-auth/rm-4-NC/RM-0-1.html
.. _name: https://reznikmm.github.io/ada-auth/rm-4-NC/RM-4-1.html#S0091

:ada:`'Image` attribute for any type
------------------------------------

In Ada 2022, you can apply the :ada:`'Image` attribute to any type,
including records, arrays, access types, and private types. Let's see how
this works. We'll define array, record, and access types and corresponding
objects and then convert these objects to strings and print them:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Image_Attribute switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is
      type Vector is array (Positive range <>) of Integer;

      V1 : aliased Vector := [1, 2, 3];

      type Text_Position is record
         Line, Column : Positive;
      end record;

      Pos : constant Text_Position := (Line => 10, Column => 3);

      type Vector_Access is access all Vector;

      V1_Ptr : constant Vector_Access := V1'Access;

   begin
      Ada.Text_IO.Put_Line (V1'Image);
      Ada.Text_IO.Put_Line (Pos'Image);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (V1_Ptr'Image);
   end Main;

.. code-block:: bash

   $ gprbuild -q -P main.gpr
     Build completed successfully.
   $ ./main
     [ 1,  2,  3]
     (LINE =>  10,
      COLUMN =>  3)
     (access 7fff64b23988)

Note the square brackets in the array image output. In Ada 2022, array
aggregates could be written :ref:`this way <Whats_New_Ada_2022_Array_Aggregates>`!

References
----------

* :aarm22:`ARM 4.10 Image Attributes <4-10>`
* AI12-0020-1_

 .. _AI12-0020-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0020-1.txt
