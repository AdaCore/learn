.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Declarations.scope_and_visibility
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Scope_And_Visibility is
      Name : Integer;
   begin
      Name := 1;
      declare
         Name : Float := 2.0;
      begin
         Name := Name + Float (Scope_And_Visibility.Name);
         Put_Line (Name'Image);
      end;
      Put_Line (Name'Image);
   end Scope_And_Visibility;
