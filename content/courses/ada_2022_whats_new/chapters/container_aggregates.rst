.. include:: ../../global.txt

Container Aggregates
====================

.. note::

    Container aggregates are supported by

    * GNAT Community Edition 2021
    * GCC 11

Ada 2022 introduces container aggregates. 
They can be used to easily create values for vectors, lists, maps, etc.
Container aggregates leverage array aggregate syntax when the container
doesn't have keys. On the other side hashed and ordered maps use record
aggregate syntax.
Consider this:

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

At run time compiler creates an empty container and then populates it
with elements one by one. So, if you define a new container type, then
you can specify a new :ada:`Aggregate` aspect to let the compiler know
two construction subprograms and enable container aggregates for your
container:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Container_Aggregates.Array

   !main.adb
   pragma Ada_2022;
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
   
      List : JSON.JSON_Array := (1, 2, 3);
      ------------------------------------
   begin
      --  Equivalent old initialization code
      List := JSON.New_JSON_Array;
      JSON.Append (List, 1);
      JSON.Append (List, 2);
      JSON.Append (List, 3);
   end Main;

For maps this could be:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Container_Aggregates.Map

   !main.adb
   pragma Ada_2022;
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
      
      Object : JSON.JSON_Object := ("a" => 1, "b" => 2, "c" => 3);
      ------------------------------------------------------------
   begin
      --  Equivalent old initialization code
      Object := JSON.New_JSON_Object;
      JSON.Insert (Object, "a", 1);
      JSON.Insert (Object, "b", 2);
      JSON.Insert (Object, "c", 3);
   end Main;

We can't specify both :ada:`Add_Named` and :ada:`Add_Unnamed` subprograms to
the same type. This prevents us from define :ada:`JSON_Value` with both
array and object aggregates defined. But we can define convention functions
for array and object and get almost the same densed code as in plain JSON.
Take a look:

.. code:: ada run_button manual_chop project=Courses.Ada_2022_Whats_New.Container_Aggregates.Both

   !main.adb
   pragma Ada_2022;
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
      
         function To_Value (Text : String) return JSON_Value is (null record);
         function To_Value (Text : Wide_Wide_String) return JSON_Value is (null record);
         function New_JSON_Object return JSON_Object is (null record);
         function New_JSON_Array return JSON_Array is (null record);
         function From_Object (Self : JSON_Object) return JSON_Value is (null record);
         function From_Array (Self : JSON_Array) return JSON_Value is (null record);
      end JSON;   
      
      function "+" (X : JSON.JSON_Object) return JSON.JSON_Value
        renames JSON.From_Object;
      function "-" (X : JSON.JSON_Array) return JSON.JSON_Value
        renames JSON.From_Array;
      
      Offices : JSON.JSON_Array :=
        (+("name"   => "North American Office",
           "phones" => -(1_877_787_4628, 1_866_787_4232, 1_212_620_7300),
           "email"  => "info@adacore.com"),
         +("name"   => "European Office",
           "phones" => -(33_1_49_70_67_16, 33_1_49_70_05_52),
           "email"  => "info@adacore.com"));
      -----------------------------------------------------------------
   begin
      --  Equivalent old initialization code is too long to print it here
      null;
   end Main;

The :ada:`Offices` variable is supposed to contain this value:

.. code-block::

     [{"name"  : "North American Office",
       "phones": [18777874628, 18667874232, 12126207300],
       "email" : "info@adacore.com"},
      {"name"  : "European Office",
       "phones": [33149706716, 33149700552],
       "email" : "info@adacore.com"}];


References
----------

* `ARM 4.3.5 Container Aggregates`_
* AI12-0212-1_

 .. _`ARM 4.3.5 Container Aggregates`: http://www.ada-auth.org/standards/2xaarm/html/AA-4-3-5.html
 .. _AI12-0212-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0212-1.TXT
