.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Declarations.aspect_clauses
    :class: ada-run

   package Aspect_Clauses is
      Eight_Bits : Integer range 0 .. 255 with
         Size => 8;
      Object : Integer with
         Atomic;
   end Aspect_Clauses;
