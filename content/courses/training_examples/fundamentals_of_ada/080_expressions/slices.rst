.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Expressions.slices
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Slices is
      procedure Explicit_Indices is
         Full_Name : String (1 .. 20);
      begin
         Put_Line (Full_Name);
         Put_Line (Full_Name (1 .. 10)); -- first half of name
         Put_Line (Full_Name (11 .. 20)); -- second half of name
      end Explicit_Indices;
   
      procedure Subtype_Indices is
         subtype First_Name is Positive range 1 .. 10;
         subtype Last_Name is Positive range 11 .. 20;
         Full_Name : String (First_Name'First .. Last_Name'Last);
      begin
         Put_Line (Full_Name);
         Put_Line (Full_Name (First_Name)); -- first half of name
         Put_Line (Full_Name (Last_Name)); -- second half of name
      end Subtype_Indices;
   begin
      Explicit_Indices;
      Subtype_Indices;
   end Slices;
