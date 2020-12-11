.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Contracts.preconditions_and_postconditions
   :class: ada-run

   package Stack_Pkg is
   
      procedure Push (Item : in Integer) with
         Pre  => not Full,
         Post => not Empty and then Top = Item;
   
      procedure Pop (Item : out Integer) with
         Pre  => not Empty,
         Post => not Full and Item = Top'Old;
   
      function Pop return Integer with
         Pre  => not Empty,
         Post => not Full and Pop'Result = Top'Old;
   
      function Top return Integer with
         Pre => not Empty;
   
      function Empty return Boolean;
   
      function Full return Boolean;
   
   end Stack_Pkg;

   package body Stack_Pkg is
   
      Values  : array (1 .. 100) of Integer;
      Current : Natural := 0;
   
      procedure Push (Item : in Integer) is
      begin
         -- precondition enforces validity
         Current          := Current + 1;
         Values (Current) := Item;
      end Push;
   
      procedure Pop (Item : out Integer) is
      begin
         -- precondition enforces validity
         Item    := Values (Current);
         Current := Current - 1;
      end Pop;
   
      function Pop return Integer is
         Item : constant Integer := Values (Current);
      begin
         Current := Current - 1;
         return Item;
      end Pop;
   
      function Top return Integer is (Values (Current));
   
      function Empty return Boolean is (Current not in Values'Range);
   
      function Full return Boolean is (Current >= Values'Length);
   
   end Stack_Pkg;
