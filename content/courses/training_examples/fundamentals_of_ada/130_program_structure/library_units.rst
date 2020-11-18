.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.library_units
   :class: ada-run

   package Named_Common is
      X : Integer; -- valid object for life of application
      Y : Float;    -- valid object for life of application
   end Named_Common;

   procedure Library_Procedure (Parameter : in out Integer);

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Library_Procedure (Parameter : in out Integer) is
      -- X is visible to Library_Procedure and Nested_Procedure
      X : constant Integer := Parameter;
      procedure Nested_Procedure is
         -- Y is only visible to Nested_Procedure
         Y : constant Integer := X * 2;
      begin
         Parameter := X * Y;
      end Nested_Procedure;
   begin
      Nested_Procedure;
      Put_Line ("parameter = " & Parameter'Image);
   end Library_Procedure;

   with Library_Procedure;
   with Named_Common;
   procedure Main is
   begin
      Named_Common.X := 123;
      Library_Procedure (Named_Common.X);
   end Main;
