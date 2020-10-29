.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Basic_Types.real_types
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Real_Types is
   
      Predefined_Floating_Point : Float := 0.0;
   
      type Floating_Point_Type is digits 8 range -1.0e10 .. 1.0e10;
      Floating_Point : Floating_Point_Type := 1.234e2;
   
   begin
   
      Put_Line (Integer'Image (Floating_Point_Type'Digits));
      Put_Line (Integer'Image (Floating_Point_Type'Base'Digits));
      Floating_Point := Floating_Point_Type'Succ (Floating_Point);
      Put_Line (Floating_Point_Type'Image (Floating_Point));
   
   end Real_Types;
