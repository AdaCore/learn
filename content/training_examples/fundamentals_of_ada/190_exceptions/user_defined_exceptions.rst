.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Exceptions.user_defined_exceptions
   :class: ada-run

   package Stack is
      Underflow, Overflow : exception;
      procedure Push (Item : in Integer);
      procedure Pop (Item : out Integer);
   end Stack;

   package body Stack is
      Values : array (1 .. 100) of Integer;
      Top    : Integer := 0;
   
      procedure Push (Item : in Integer) is
      begin
         if Top = Values'Last then
            raise Overflow;
         end if;
         Top          := Top + 1;
         Values (Top) := Item;
      end Push;
   
      procedure Pop (Item : out Integer) is
      begin
         if Top < Values'First then
            raise Underflow;
         end if;
         Item := Values (Top);
         Top  := Top - 1;
      end Pop;
   
   end Stack;

   with Ada.Text_IO; use Ada.Text_IO;
   with Stack;
   procedure Test_Stack is
      Global : Integer := 123;
   
      procedure Push (X : Integer) is
      begin
         Stack.Push (X);
      exception
         when Stack.Overflow =>
            Put_Line ("No room on the stack");
      end Push;
   
      procedure Pop is
      begin
         Stack.Pop (Global);
      exception
         when Stack.Underflow =>
            Put_Line ("Nothing on the stack");
      end Pop;
   
   begin
   
      Push (1);
      Pop;
      Pop;
      for I in 1 .. 100 loop
         Push (I);
      end loop;
      Push (2);
   
   end Test_Stack;
