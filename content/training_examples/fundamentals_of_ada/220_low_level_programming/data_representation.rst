.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Low_Level_Programming.data_representation
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Test_Data_Representation is
   
      type Enum is (E1, E2, E3);
   
      procedure Attributes is
         type Integer_T is range 1 .. 10;
         type Similar_Integer_T is range 1 .. 10 with
            Size        => 13,
            Object_Size => 16,
            Alignment   => 1;
      begin
         Put_Line
           ("Integer_T 'Size / 'Object_size / 'Alignment " &
            Integer'Image (Integer_T'Size) &
            Integer'Image (Integer_T'Object_Size) &
            Integer'Image (Integer_T'Alignment));
         Put_Line
           ("Similar_Integer_T 'Size / 'Object_size / 'Alignment " &
            Integer'Image (Similar_Integer_T'Size) &
            Integer'Image (Similar_Integer_T'Object_Size) &
            Integer'Image (Similar_Integer_T'Alignment));
      end Attributes;
   
      procedure Representation_Clauses is
   
         type Normal_Record_T is record
            A : Integer range 0 .. 4;
            B : Boolean;
            C : Integer;
            D : Enum := E1;
         end record;
         type Normal_Array_T is array (1 .. 1_000) of Boolean;
   
         type Packed_Record_T is record
            A : Integer range 0 .. 4;
            B : Boolean;
            C : Integer;
            D : Enum := E2;
         end record with
            Pack;
         type Packed_Array_T is array (1 .. 1_000) of Boolean with
            Pack;
   
         type Repped_Record_T is record
            A : Integer range 0 .. 4;
            B : Boolean;
            C : Integer;
            D : Enum := E3;
         end record;
         for Repped_Record_T use record
            A at 0 range 0 ..  2;
            B at 0 range 3 ..  3;
            C at 0 range 5 .. 36;
            D at 5 range 0 ..  2;
         end record;
         type Repped_Array_T is array (1 .. 1_000) of Boolean;
         for Repped_Array_T'Component_Size use 2;
   
      begin
         Put_Line
           ("Size of normal record / array: " &
            Integer'Image (Normal_Record_T'Size) &
            Integer'Image (Normal_Array_T'Size));
         Put_Line
           ("Size of packed record / array: " &
            Integer'Image (Packed_Record_T'Size) &
            Integer'Image (Packed_Array_T'Size));
         Put_Line
           ("Size of repped record / array: " &
            Integer'Image (Repped_Record_T'Size) &
            Integer'Image (Repped_Array_T'Size));
      end Representation_Clauses;
   
   begin
      Attributes;
      Representation_Clauses;
   
   end Test_Data_Representation;
