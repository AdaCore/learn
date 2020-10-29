.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Statements.assignment_statements
    :class: ada-run

   procedure Assignment_Statements is
   
      Max_Miles : constant Integer := 20;
   
      type Miles_Type is range 0 .. Max_Miles;
      type Feet_Type is range 0 .. Miles_Type'Last * 5_280;
   
      Feet  : Feet_Type  := 0;
      Miles : Miles_Type := 0;
   
      Index1, Index2 : Miles_Type range 1 .. 20;
   
   begin
   
      Feet  := 10_560;
      -- Miles := Feet / 5_280; -- compile error
   
      -- Max_Miles := Max_Miles + 1; -- compile error
   
      Index1 := 0; -- run-time error
   
      Index1 := Miles; -- constraint checking added
      Index2 := Index1; -- no constraint checking needed
   
   end Assignment_Statements;
