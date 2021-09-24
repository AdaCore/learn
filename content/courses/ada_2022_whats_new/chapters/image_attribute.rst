.. include:: ../../global.txt

:ada:`'Image` attribute for any type
====================================

.. note::

   Attribute :ada:`'Image` for any type is supported by

    * GNAT Community Edition 2020 and latter
    * GCC 11

:ada:`'Image` attribute for a value
-----------------------------------

If you missed `Technical Corrigendum 1`_ changes (they were published
in February 2016) then you probably don't know that the :ada:`'Image`
attribute can be applied now to a value. So, instead of
:ada:`My_Type'Image (Value)` you can just write :ada:`Value'Image`,
but only if the :ada:`Value` is a name_. So, these two statements are equal:

.. code-block:: ada

   Ada.Text_IO.Put_Line (Ada.Text_IO.Page_Length'Image);

   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Count'Image (Ada.Text_IO.Page_Length));

.. _`Technical Corrigendum 1`: https://reznikmm.github.io/ada-auth/rm-4-NC/RM-0-1.html
.. _name: https://reznikmm.github.io/ada-auth/rm-4-NC/RM-4-1.html#S0091

:ada:`'Image` attribute for any type
------------------------------------

Now you can apply :ada:`'Image` attribute for any type, including records,
array, access and private types. Let's see how this works. Define an array,
record, access types and corresponding objects:

.. code-block:: ada

   type Vector is array (Positive range <>) of Integer;

   V1 : aliased Vector := (1, 2, 3);

   type Text_Position is record
      Line, Column : Positive;
   end record;

   Pos : constant Text_Position := (Line => 10, Column => 3);

   type Vector_Access is access all Vector;

   V1_Ptr : constant Vector_Access := V1'Access;

Now you can convert these objects to string and print:

.. code-block:: ada

   Ada.Text_IO.Put_Line (V1'Image);

   Ada.Text_IO.Put_Line (Pos'Image);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (V1_Ptr'Image);

Output is:

.. code-block:: ada

   [ 1,  2,  3]

   (LINE =>  10,
    COLUMN =>  3)

   (access 7ff5c5717138)

Note square brackets in array image. In Ada 2022 array aggregates could
be written this way!

Complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Image_Attribute

   pragma Ada_2022;

   with Ada.Text_IO;

   procedure Main is
      type Vector is array (Positive range <>) of Integer;

      V1 : aliased Vector := (1, 2, 3);

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

References:
-----------

* `ARM 4.10 Image Attributes`_
* AI12-0020-1_

 .. _`ARM 4.10 Image Attributes`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-10.html
 .. _AI12-0020-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0020-1.txt