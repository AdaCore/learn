.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.use_clauses
   :class: ada-run

   package Pkg_A is
      Constant_A  : constant := 1;
      Constant_Aa : constant := 11;
      Initialized : Boolean  := False;
   end Pkg_A;

   package Pkg_B is
      Constant_B  : constant := 20;
      Constant_Bb : constant := 220;
      Initialized : Boolean  := False;
   end Pkg_B;

   package Pkg_B.Child is
      Constant_Bbb : constant := 222;
   end Pkg_B.Child;

   with Pkg_A; use Pkg_A;
   with Pkg_B;
   with Pkg_B.Child;
   package P is
      type Type_1 is range
          Constant_A .. -- visible without dot-notation
   
            Pkg_B.Constant_B; -- not visible without dot-notation
   
      use Pkg_B;
      -- Constant_B is now visible without dot-notation
      type Type_2 is range Constant_Aa .. Constant_Bb;
   
      Constant_Bb : Integer := 33;
      -- with or without "use", Constant_Bb will always be the local version
      function Bb return Integer is (Constant_Bb);
   
      function Is_Initialized return Boolean is
      -- Need dot-notation to resolve ambiguity
   
        (Pkg_A.Initialized and Pkg_B.Initialized);
   
      -- we "use" Pkg_B, so Child is directly visible
      Object : Integer := Child.Constant_Bbb;
   
   end P;

   with Ada.Text_IO; use Ada.Text_IO;
   with P;
   procedure Test is
      A, B, C : P.Type_2 := P.Type_2'First;
   begin
      -- C := A + B; -- illegal
      C := P."+" (A, B); -- legal but not pretty
      Put_Line (C'Image);
      declare
         use P; -- make everything visible (including operators)
      begin
         A := A + 1;
         B := B + 1;
         C := A + B; -- now legal
         Put_Line (C'Image);
      end;
   end Test;
