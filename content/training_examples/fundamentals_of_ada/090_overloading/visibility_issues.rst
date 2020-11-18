.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Overloading.visibility_issues
   :class: ada-run

   procedure Visibility_Issues is
      procedure Foo (I : Integer) is
         procedure Foo (N : Natural) is null;
      begin
         Foo (I);
      end Foo;
      -- procedure Foo (N : Natural) is null; -- compile error
   begin
      Foo (1);
   end Visibility_Issues;
