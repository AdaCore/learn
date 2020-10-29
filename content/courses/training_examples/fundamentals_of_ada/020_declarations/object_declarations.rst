.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Declarations.object_declarations
    :class: ada-run

   with Ada.Calendar; use Ada.Calendar;
   procedure Object_Declarations is
      A    : Integer := 0;
      B, C : Time    := Clock;
      D    : Integer := A + 1;
   begin
      null;
   end Object_Declarations;
