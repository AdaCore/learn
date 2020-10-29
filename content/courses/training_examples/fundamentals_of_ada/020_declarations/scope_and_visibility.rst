.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Declarations.scope_and_visibility
    :class: ada-run

   procedure Scope_And_Visibility is
      Name : Integer;
   begin
      Name := 1;
      declare
         Name : Float := 2.0;
      begin
         Name := Name + Float (Scope_And_Visibility.Name);
      end;
   end Scope_And_Visibility;
