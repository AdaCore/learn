.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Statements.assignment_statements
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Assignment_Statements is
   
      Max_Miles : constant Integer := 20;
   
      type Feet_T is range 0 .. Max_Miles * 5_280;
      type Miles_T is range 0 .. Max_Miles;
   
      Feet  : constant Feet_T := Feet_T (Line) * 1_000;
      Miles : Miles_T         := 0;
   
      Index1, Index2 : Miles_T range 1 .. 20;
   
   begin
   
      -- Miles := Feet / 5_280; -- compile error
   
      -- Max_Miles := Max_Miles + 1; -- compile error
   
      Index1 := Miles_T (Max_Miles); -- constraint checking added
      Index2 := Index1;    -- no constraint checking needed
   
      Put_Line ("Index1 = " & Index1'Image);
      Put_Line ("Index2 = " & Index2'Image);
   
      Index1 := 0; -- run-time error
      Put_Line ("Index1 = " & Index1'Image);
   
   end Assignment_Statements;
