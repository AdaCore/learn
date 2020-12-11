.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Subprograms.potential_pitfalls
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Potential_Pitfalls is
      Global_I : Integer  := 0;
      Global_P : Positive := 1;
      Global_S : String   := "Hello";
   
      procedure Unassigned_Out (A : in Integer; B : out Positive) is
      begin
         if A > 0 then
            B := A;
         end if;
      end Unassigned_Out;
   
      function Cause_Side_Effect return Integer is
      begin
         Global_I := Global_I + 1;
         return Global_I;
      end Cause_Side_Effect;
   
      procedure Order_Dependent_Code (X, Y : Integer) is
      begin
         Put_Line (Integer'Image (X) & " / " & Integer'Image (Y));
      end Order_Dependent_Code;
   
      procedure Aliasing
        (Param : in     String;
         I1    : in out Integer;
         I2    : in out Integer) is
      begin
         Global_S := "World";
         I1       := I1 * 2;
         I2       := I2 * 3;
         Put_Line ("Aliasing string: " & Param);
      end Aliasing;
   
   begin
   
      Unassigned_Out (-1, Global_P);
      Put_Line ("Global_P = " & Positive'Image (Global_P));
   
      Order_Dependent_Code (Global_I, Cause_Side_Effect);
   
      Global_P := Positive'First;
   
      -- compile error Aliasing (Global_S, Global_I, Global_I);
      Aliasing (Global_S, Global_I, Global_P);
      Put_Line ("Global_S: " & Global_S);
      Put_Line ("Global_P: " & Global_P'Image);
   
   end Potential_Pitfalls;
