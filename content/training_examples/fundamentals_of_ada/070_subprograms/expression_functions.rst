.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Subprograms.expression_functions
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Expression_Functions is
   
      function Square1 (X : Integer) return Integer is (X * 2);
      function Square2 (X : Integer) return Integer is
      begin
         return X * 2;
      end Square2;
   
      function Square3 (X : Integer) return Integer;
      function Square3 (X : Integer) return Integer is (X * 2);
   
      function Square4 (X : Integer) return Integer is (X * 2);
      -- illegal: Square4 already complete function Square4 (X : Integer) return
      -- Integer is begin
      --    return X * 2;
      -- end Square4;
   
   begin
      Put_Line (Integer'Image (Square1 (2)));
      Put_Line (Integer'Image (Square2 (3)));
      Put_Line (Integer'Image (Square3 (4)));
      Put_Line (Integer'Image (Square4 (5)));
   end Expression_Functions;
