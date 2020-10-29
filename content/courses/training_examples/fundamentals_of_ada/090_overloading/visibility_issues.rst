.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Overloading.visibility_issues
   :class: ada-run

   procedure Visibility_Issues is
      procedure Foo (I : Integer) is
         procedure Foo (N : Natural) is null;
      begin
         null;
      end Foo;
      procedure Foo (N : Natural) is
      begin
         null;
      end Foo;
   begin
      null;
   end Visibility_Issues;
