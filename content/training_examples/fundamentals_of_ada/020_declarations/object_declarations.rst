.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Declarations.object_declarations
    :class: ada-run

   with Ada.Calendar; use Ada.Calendar;
   package Object_Declarations is
      A    : Integer := 0;
      B, C : Time    := Clock;
      D    : Integer := A + 1;
   end Object_Declarations;
