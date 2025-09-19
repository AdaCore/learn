.. include:: ../../../global.txt

Redefining the :ada:`'Image` attribute
======================================

In Ada 2022, you can redefine :ada:`'Image` attribute for your type,
though the syntax to do this has been changed several times. Let's see
how it works in GNAT Community 2021.

.. note::

    Redefining attribute :ada:`'Image` is supported by

    * GNAT Community Edition 2021 (using :ada:`Text_Buffers`)
    * GNAT Community Edition 2020 (using :ada:`Text_Output.Utils`)
    * GCC 11 (using :ada:`Text_Output.Utils`)

In our example, let's redefine the :ada:`'Image` attribute for a
location in source code. To do this, we provide a new :ada:`Put_Image`
aspect for the type:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Image_Redefine switches=Compiler(-gnat2022);

   with Ada.Text_IO;
   with Ada.Strings.Text_Buffers;

   procedure Main is

      type Source_Location is record
         Line   : Positive;
         Column : Positive;
      end record
        with Put_Image => My_Put_Image;

      procedure My_Put_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : Source_Location);

      procedure My_Put_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : Source_Location)
      is
         Line   : constant String := Value.Line'Image;
         Column : constant String := Value.Column'Image;
         Result : constant String :=
           Line (2 .. Line'Last) & ':' & Column (2 .. Column'Last);
      begin
          Output.Put (Result);
      end My_Put_Image;

      Line_10 : constant Source_Location := (Line => 10, Column => 1);

   begin
      Ada.Text_IO.Put_Line (Line_10'Image);
   end Main;

What's the Root_Buffer_Type?
----------------------------

Let's see how it's defined in the :ada:`Ada.Strings.Text_Buffers` package.

.. code-block:: ada

   type Root_Buffer_Type is abstract tagged limited private;

   procedure Put
     (Buffer : in out Root_Buffer_Type;
      Item   : in     String) is abstract;

In addition to :ada:`Put`, there are also :ada:`Wide_Put`,
:ada:`Wide_Wide_Put`, :ada:`Put_UTF_8`, :ada:`Wide_Put_UTF_16`. And
also :ada:`New_Line`, :ada:`Increase_Indent`, :ada:`Decrease_Indent`.

Outdated draft implementation
-----------------------------

GNAT Community Edition 2020 and GCC 11 both provide a draft
implementation that's incompatible with the Ada 2022 specification.
For those versions, :ada:`My_Put_Image` looks like:

.. code-block:: ada

   procedure My_Put_Image
     (Sink  : in out Ada.Strings.Text_Output.Sink'Class;
      Value : Source_Location)
   is
      Line   : constant String := Value.Line'Image;
      Column : constant String := Value.Column'Image;
      Result : constant String :=
        Line (2 .. Line'Last) & ':' & Column (2 .. Column'Last);
   begin
       Ada.Strings.Text_Output.Utils.Put_UTF_8 (Sink, Result);
   end My_Put_Image;


References
----------

* :aarm22:`ARM 4.10 Image Attributes <4-10>`
* AI12-0020-1_
* AI12-0384-2_

 .. _AI12-0020-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0020-1.TXT
 .. _AI12-0384-2: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/AI12-0384-2.TXT
