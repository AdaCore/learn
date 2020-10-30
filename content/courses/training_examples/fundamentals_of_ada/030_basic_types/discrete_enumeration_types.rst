.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Basic_Types.discrete_enumeration_types
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Discrete_Enumeration_Types is
   
      type Colors_Type is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
      Color : Colors_Type := Red;
   
      type Traffic_Light_Type is (Red, Yellow, Green);
      for Traffic_Light_Type use (1, 2, 4);
      Stoplight : Traffic_Light_Type := Red;
   
      type Roman_Numeral_Digit_Type is ('I', 'V', 'X', 'L', 'C', 'M');
      Digit : Roman_Numeral_Digit_Type := 'I';
   
      Flag : Boolean;
   
      Position : Integer;
   
   begin
   
      Position  := Traffic_Light_Type'Pos (Green);
      Color     := Colors_Type'Val (Position);
      Stoplight := Traffic_Light_Type'(Red);
      Digit     := Roman_Numeral_Digit_Type'Succ (Digit);
      Flag      := End_Of_Line;
   
      Put_Line (Position'Image);
      Put_Line (Color'Image);
      Put_Line (Flag'Image);
      Put_Line (Digit'Image);
      Put_Line (Stoplight'Image);
   
   end Discrete_Enumeration_Types;
