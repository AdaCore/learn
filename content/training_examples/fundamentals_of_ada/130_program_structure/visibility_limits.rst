.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.visibility_limits

   package Stack is
      procedure Push (Item : in Integer);
      procedure Pop (Item : out Integer);
   private
      Object : array (1 .. 100) of integer;
      Top    : Natural := 0;
   end Stack;

   package Stack.Utils is
      function Top return Integer;
   private
      -- Legal here, but not above "private"
      function Top return Integer is (Object (Stack.Top));
   end Stack.Utils;

   package Stack.Child is
      procedure Misbehave;
      procedure Reset;
      function Peek (Index : Natural) return Integer;
   end Stack.Child;
   
   package body Stack.Child is
      procedure Misbehave is
      begin
         Top := 0;
      end Misbehave;

      procedure Reset is
      begin
         Top := 0;
      end Reset;

      function Peek (Index : Natural) return Integer is (Object (Index));
   end Stack.Child;

   package body Stack is
      procedure Push (Item : in Integer) is null;
      procedure Pop (Item : out Integer) is null;
   end Stack;
