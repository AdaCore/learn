.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.visibility_limits
   :class: ada-run

   package Stack is
      Capacity : constant := 100;
      procedure Push (Item : in Integer);
      procedure Pop (Item : out Integer);
   private
      type List_T is array (Natural range <>) of Integer;
      Object : List_T (1 .. Capacity);
      Top    : Natural := 0;
   end Stack;

   package Stack.Utils is
      function Top return Integer;
   private
      -- Legal here, but not above "private"
      function Top return Integer is (Object (Stack.Top));
   end Stack.Utils;

   package Stack.Bad_Child is
      procedure Misbehave;
      function Peek (Index : Natural) return Integer;
   end Stack.Bad_Child;

   package body Stack.Bad_Child is
      procedure Misbehave is
      begin
         Top := 0;
      end Misbehave;
      function Peek (Index : Natural) return Integer is (Object (Index));
   end Stack.Bad_Child;

   package Stack.Good_Child is
      procedure Reset;
   end Stack.Good_Child;

   package body Stack.Good_Child is
      procedure Reset is
      begin
         Top := 0;
      end Reset;
   end Stack.Good_Child;

   package body Stack is
      procedure Push (Item : in Integer) is
      begin
         Top          := Top + 1;
         Object (Top) := Item;
      end Push;
      procedure Pop (Item : out Integer) is
      begin
         Item := Object (Top);
         Top  := Top - 1;
      end Pop;
   end Stack;
