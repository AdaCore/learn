.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Basic_Types.discrete_numeric_types
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Discrete_Numeric_Types is
   
      type Signed_Integer_Type is range -128 .. 127;
      Signed_Integer : Signed_Integer_Type := 100;
   
      type Unsigned_Integer_Type is mod 256;
      Unsigned_Integer : Unsigned_Integer_Type := 100;
   
   begin
   
      Signed_Integer := Signed_Integer_Type'Last;
      Signed_Integer := Signed_Integer_Type'Succ (Signed_Integer);
      Put_Line (Signed_Integer'Image);
   
      Unsigned_Integer := Unsigned_Integer_Type'First;
      Unsigned_Integer := Unsigned_Integer_Type'Pred (Unsigned_Integer);
      Put_Line (Unsigned_Integer'Image);
   
      Unsigned_Integer := Unsigned_Integer_Type (Signed_Integer);
      Put_Line (Unsigned_Integer'Image);
   
      Unsigned_Integer := Unsigned_Integer_Type'Mod (Signed_Integer);
      Put_Line (Unsigned_Integer'Image);
   
      declare
         Some_String : constant String :=
           Unsigned_Integer_Type'Image (Unsigned_Integer);
      begin
         Signed_Integer := Signed_Integer_Type'Value (Some_String);
         Put_Line (Signed_Integer'Image);
   
         Put_Line (Some_String);
      end;
   
   end Discrete_Numeric_Types;
