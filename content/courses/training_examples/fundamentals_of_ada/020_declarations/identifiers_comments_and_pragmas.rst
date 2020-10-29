.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Declarations.identifiers_comments_and_pragmas
    :class: ada-run

   procedure Identifiers_Comments_And_Pragmas is
   
      Spaceperson : Integer;
      --SPACEPERSON : integer; -- identifier is a duplicate
      Space_Person : Integer;
      --Null : integer := 0; -- identifier is a reserved word
      pragma Optimize (Off);
   
   begin
      null;
   end Identifiers_Comments_And_Pragmas;
