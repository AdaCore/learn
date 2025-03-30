.. include:: ../../global.txt

Enumeration representation
==========================

.. note::

    Enumeration representation attributes are supported by

    * GNAT Community Edition 2019
    * GCC 9

Enumeration types in Ada are represented as integers at the machine
level. But there are actually two mappings from enumeration to
integer: a literal position and a representation value.

Literal positions
-----------------

Each enumeration literal has a corresponding position in the type
declaration. We can easily obtain it from the :ada:`Type'Pos (Enum)`
attribute.

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Enum_Val.Pos switches=Compiler(-gnat2022);

   with Ada.Text_IO;
   with Ada.Integer_Text_IO;

   procedure Main is
   begin
      Ada.Text_IO.Put ("Pos(False) =");
      Ada.Integer_Text_IO.Put (Boolean'Pos (False));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Pos(True)  =");
      Ada.Integer_Text_IO.Put (Boolean'Pos (True));
   end Main;

For the reverse mapping, we use :ada:`Type'Val (Int)`:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Enum_Val.Val switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is
   begin
      Ada.Text_IO.Put_Line (Boolean'Val (0)'Image);
      Ada.Text_IO.Put_Line (Boolean'Val (1)'Image);
   end Main;

Representation values
---------------------

The representation value defines the `internal` code, used to store
enumeration values in memory or CPU registers. By default, enumeration
representation values are the same as the corresponding literal
positions, but you can redefine them.  Here, we created a copy of
:ada:`Boolean` type and assigned it a custom representation.

In Ada 2022, we can get an integer value of the representation with
:ada:`Type'Enum_Rep(Enum)` attribute:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Enum_Val.Enum_Rep switches=Compiler(-gnat2022);

   with Ada.Text_IO;
   with Ada.Integer_Text_IO;

   procedure Main is
      type My_Boolean is new Boolean;
      for My_Boolean use (False => 3, True => 6);
   begin
      Ada.Text_IO.Put ("Enum_Rep(False) =");
      Ada.Integer_Text_IO.Put (My_Boolean'Enum_Rep (False));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Enum_Rep(True)  =");
      Ada.Integer_Text_IO.Put (My_Boolean'Enum_Rep (True));
   end Main;

And, for the reverse mapping, we can use :ada:`Type'Enum_Val (Int)`:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Enum_Val.Enum_Val switches=Compiler(-gnat2022);

   with Ada.Text_IO;
   with Ada.Integer_Text_IO;

   procedure Main is
      type My_Boolean is new Boolean;
      for My_Boolean use (False => 3, True => 6);
   begin
      Ada.Text_IO.Put_Line (My_Boolean'Enum_Val (3)'Image);
      Ada.Text_IO.Put_Line (My_Boolean'Enum_Val (6)'Image);

      Ada.Text_IO.Put ("Pos(False) =");
      Ada.Integer_Text_IO.Put (My_Boolean'Pos (False));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Pos(True)  =");
      Ada.Integer_Text_IO.Put (My_Boolean'Pos (True));
   end Main;

Note that the :ada:`'Val(X)/'Pos(X)` behaviour still is the same.

Custom representations can be useful for integration with a low level
protocol or hardware.

Before Ada 2022
---------------

This doesn't initially look like an important feature, but let's see
how we'd do the equivalent with Ada 2012 and earlier versions. First,
we need an integer type of matching size, then we instantiate
:ada:`Ada.Unchecked_Conversion`.  Next, we call :ada:`To_Int/From_Int`
to work with representation values.  And finally an extra type
conversion is needed:


.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Enum_Val.Conv

   with Ada.Text_IO;
   with Ada.Integer_Text_IO;
   with Ada.Unchecked_Conversion;

   procedure Main is

      type My_Boolean is new Boolean;
      for My_Boolean use (False => 3, True => 6);
      type My_Boolean_Int is range 3 .. 6;
      for My_Boolean_Int'Size use My_Boolean'Size;

      function To_Int is new Ada.Unchecked_Conversion
        (My_Boolean, My_Boolean_Int);

      function From_Int is new Ada.Unchecked_Conversion
        (My_Boolean_Int, My_Boolean);

   begin
      Ada.Text_IO.Put ("To_Int(False) =");
      Ada.Integer_Text_IO.Put (Integer (To_Int (False)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("To_Int(True)  =");
      Ada.Integer_Text_IO.Put (Integer (To_Int (True)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("From_Int (3)  =");
      Ada.Text_IO.Put_Line (From_Int (3)'Image);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("From_Int (6)  =");
      Ada.Text_IO.Put_Line (From_Int (6)'Image);
   end Main;

Even with all that, this solution doesn't work for generic formal type
(because :ada:`T'Size` must be a static value)!

We should note that these new attributes may already be familiar to GNAT
users because they've been in the GNAT compiler for many years.

References
----------

* :aarm22:`ARM 13.4 Enumeration Representation Clauses <13-4>`
* AI12-0237-1_

 .. _AI12-0237-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0237-1.TXT
