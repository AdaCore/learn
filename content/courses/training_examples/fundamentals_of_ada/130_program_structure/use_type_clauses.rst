.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.use_type_clauses
   :class: ada-run

   package P is
      type Int1 is range 0 .. 1_000;
      type Int2 is range 0 .. 2_000;
      type Int3 is range 0 .. 3_000;
      function "+"
        (Left  : Int1;
         Right : Int3)
         return Int3;
      function "+"
        (Left  : Int2;
         Right : Int3)
         return Int3;
   end P;

   with Ada.Text_IO; use Ada.Text_IO;
   with P;
   procedure Test is
      A, B, C : P.Int1 := 123;
      use type P.Int1;
      -- D : Int2; -- "Int2" is not visible
      D : P.Int2 := 234;
      E : P.Int3 := 345;
   begin
      B := A;
      C := A + B; -- implicit operator is visible
      Put_Line (C'Image);
      A := B;
      E := A + E; -- "used" operator visible
      Put_Line (E'Image);
      -- E := D + E; -- illegal: operator not "used" E := E + A; -- illegal: no
      -- matching operator
   end Test;

   package body P is
      function "+"
        (Left  : Int1;
         Right : Int3)
         return Int3 is (Int3'Last);
      function "+"
        (Left  : Int2;
         Right : Int3)
         return Int3 is (Int3'Last);
   end P;
