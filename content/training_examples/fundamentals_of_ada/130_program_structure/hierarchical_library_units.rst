.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.hierarchical_library_units
   :class: ada-run

   package Complex is
      type Number is private;
      function "*" (Left, Right : Number) return Number;
      function "/" (Left, Right : Number) return Number;
      function "+" (Left, Right : Number) return Number;
      function "-" (Left, Right : Number) return Number;
   private
      type Number is record
         Real_Part, Imaginary_Part : Float;
      end record;
   end Complex;

   package Complex.Utils is
      function To_String (C : Number) return String;
   end Complex.Utils;

   package body Complex.Utils is
      -- construction of "number" is visible in the child body
      function To_String (C : Number) return String is
        (C.Real_Part'Image & " + i" & C.Imaginary_Part'Image);
   end Complex.Utils;

   package Complex.Debug is
      -- "with Complex;" not needed for visibility to number
      procedure Print (C : Number);
   end Complex.Debug;

   with Ada.Text_IO;
   -- "with Complex.Utils" needed for visibility to "To_String"
   with Complex.Utils;
   package body Complex.Debug is
      procedure Print (C : Number) is
      begin
         -- because of parent visibility, don't need to use "Complex.Utils"
         Ada.Text_IO.Put_Line (Utils.To_String (C));
      end Print;
   end Complex.Debug;

   package body Complex is
      function "*" (Left, Right : Number) return Number is (Left);
      function "/" (Left, Right : Number) return Number is (Left);
      function "+" (Left, Right : Number) return Number is (Left);
      function "-" (Left, Right : Number) return Number is (Left);
   end Complex;
