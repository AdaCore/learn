.. include:: ../../global.txt

.. _container_aggregates:

Container Aggregates
====================

.. note::

    Container aggregates are supported by

    * GNAT Community Edition 2021
    * GCC 11

Ada 2022 introduces container aggregates, which can be used to easily create
values for vectors, lists, maps, and other aggregates.  For containers such
as maps, the aggregate must use named assоciations to provide keys and values.
For other containers it uses positional assоciations.  Only square brackets
are allowed.  Here's an example:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Container_Aggregates_1 switches=Compiler(-gnat2022);

   with Ada.Text_IO;
   with Ada.Containers.Vectors;
   with Ada.Containers.Ordered_Maps;

   procedure Main is

      package Int_Vectors is new Ada.Containers.Vectors
        (Positive, Integer);

      X : constant Int_Vectors.Vector := [1, 2, 3];

      package Float_Maps is new Ada.Containers.Ordered_Maps
        (Integer, Float);

      Y : constant Float_Maps.Map := [-10 => 1.0, 0 => 2.5, 10 => 5.51];
   begin
      Ada.Text_IO.Put_Line (X'Image);
      Ada.Text_IO.Put_Line (Y'Image);
   end Main;

At run time, the compiler creates an empty container and populates it with
elements one by one. If you define a new container type, you can specify a
new :ada:`Aggregate` aspect to enable container aggregates for your
container and let the compiler know what subprograms to use to construct the
aggregate:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Container_Aggregates_2 switches=Compiler(-gnat2022);

   procedure Main is

      package JSON is
         type JSON_Value is private
           with Integer_Literal => To_JSON_Value;

         function To_JSON_Value (Text : String) return JSON_Value;

         type JSON_Array is private
           with Aggregate => (Empty       => New_JSON_Array,
                              Add_Unnamed => Append);

         function New_JSON_Array return JSON_Array;

         procedure Append
           (Self  : in out JSON_Array;
            Value : JSON_Value) is null;

      private
         type JSON_Value is null record;
         type JSON_Array is null record;

         function To_JSON_Value (Text : String) return JSON_Value
           is (null record);

         function New_JSON_Array return JSON_Array is (null record);
      end JSON;

      List : JSON.JSON_Array := [1, 2, 3];
      ------------------------------------
   begin
      --  Equivalent old initialization code
      List := JSON.New_JSON_Array;
      JSON.Append (List, 1);
      JSON.Append (List, 2);
      JSON.Append (List, 3);
   end Main;

The equivalent for maps is:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Container_Aggregates_3 switches=Compiler(-gnat2022);

   procedure Main is

      package JSON is
         type JSON_Value is private
           with Integer_Literal => To_JSON_Value;

         function To_JSON_Value (Text : String) return JSON_Value;

         type JSON_Object is private
           with Aggregate => (Empty     => New_JSON_Object,
                              Add_Named => Insert);

         function New_JSON_Object return JSON_Object;

         procedure Insert
           (Self  : in out JSON_Object;
            Key   : Wide_Wide_String;
            Value : JSON_Value) is null;

      private
         type JSON_Value is null record;
         type JSON_Object is null record;

         function To_JSON_Value (Text : String) return JSON_Value
           is (null record);

         function New_JSON_Object return JSON_Object is (null record);
      end JSON;

      Object : JSON.JSON_Object := ["a" => 1, "b" => 2, "c" => 3];
      ------------------------------------------------------------
   begin
      --  Equivalent old initialization code
      Object := JSON.New_JSON_Object;
      JSON.Insert (Object, "a", 1);
      JSON.Insert (Object, "b", 2);
      JSON.Insert (Object, "c", 3);
   end Main;

You can't specify both :ada:`Add_Named` and :ada:`Add_Unnamed` subprograms
for the same type. This prevents you from defining :ada:`JSON_Value` with
both array and object aggregates present. But we can define conversion
functions for array and object and get code almost as dense as the same
code in native JSON.  For example:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Container_Aggregates_4 switches=Compiler(-gnat2022);

   procedure Main is

      package JSON is
         type JSON_Value is private
           with Integer_Literal => To_Value, String_Literal => To_Value;

         function To_Value (Text : String) return JSON_Value;
         function To_Value (Text : Wide_Wide_String) return JSON_Value;

         type JSON_Object is private
           with Aggregate => (Empty     => New_JSON_Object,
                              Add_Named => Insert);

         function New_JSON_Object return JSON_Object;

         procedure Insert
           (Self  : in out JSON_Object;
            Key   : Wide_Wide_String;
            Value : JSON_Value) is null;

         function From_Object (Self : JSON_Object) return JSON_Value;

         type JSON_Array is private
           with Aggregate => (Empty       => New_JSON_Array,
                              Add_Unnamed => Append);

         function New_JSON_Array return JSON_Array;

         procedure Append
           (Self  : in out JSON_Array;
            Value : JSON_Value) is null;

         function From_Array (Self : JSON_Array) return JSON_Value;

      private
         type JSON_Value is null record;
         type JSON_Object is null record;
         type JSON_Array is null record;

         function To_Value (Text : String) return JSON_Value is
           (null record);
         function To_Value (Text : Wide_Wide_String) return JSON_Value is
           (null record);
         function New_JSON_Object return JSON_Object is
           (null record);
         function New_JSON_Array return JSON_Array is
           (null record);
         function From_Object (Self : JSON_Object) return JSON_Value is
           (null record);
         function From_Array (Self : JSON_Array) return JSON_Value is
           (null record);
      end JSON;

      function "+" (X : JSON.JSON_Object) return JSON.JSON_Value
        renames JSON.From_Object;
      function "-" (X : JSON.JSON_Array) return JSON.JSON_Value
        renames JSON.From_Array;

      Offices : JSON.JSON_Array :=
        [+["name"   => "North American Office",
           "phones" => -[1_877_787_4628,
                         1_866_787_4232,
                         1_212_620_7300],
           "email"  => "info@adacore.com"],
         +["name"   => "European Office",
           "phones" => -[33_1_49_70_67_16,
                         33_1_49_70_05_52],
           "email"  => "info@adacore.com"]];
      -----------------------------------------------------------------
   begin
      --  Equivalent old initialization code is too long to print it here
      null;
   end Main;

The :ada:`Offices` variable is supposed to contain this value:

.. code-block:: json

     [{"name"  : "North American Office",
       "phones": [18777874628,
                  18667874232,
                  12126207300],
       "email" : "info@adacore.com"},
      {"name"  : "European Office",
       "phones": [33149706716,
                  33149700552],
       "email" : "info@adacore.com"}]


References
----------

* :aarm22:`ARM 4.3.5 Container Aggregates <4-3-5>`
* AI12-0212-1_

 .. _AI12-0212-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0212-1.TXT
