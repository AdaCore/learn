.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Subprograms.expression_functions
    :class: ada-run

   procedure Expression_Functions is
   
      function Square1 (X : Integer) return Integer is (X * 2);
      function Square2 (X : Integer) return Integer is
      begin
         return X * 2;
      end Square2;
   
      function Square3 (X : Integer) return Integer;
      function Square3 (X : Integer) return Integer is (X * 2);
   
      function Square4 (X : Integer) return Integer is (X * 2);
      -- illegal: Square4 already complete
      function Square4 (X : Integer) return Integer is
      begin
         return X * 2;
      end Square4;
   
   begin
      null;
   end Expression_Functions;
