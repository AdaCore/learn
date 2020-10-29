.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Declarations.aspect_clauses
    :class: ada-run

   procedure Aspect_Clauses is
      Eight_Bits : Integer range 0 .. 255 with
         Size => 8;
      Object : Integer with
         Atomic;
   begin
      null;
   end Aspect_Clauses;

