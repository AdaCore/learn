.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Private_Types.implementing_abstract_data_types_view_views
   :class: ada-run

   package Bounded_Stack is
      Max_Capacity : constant := 100;
      type Stack_T is private;
      procedure Push (This : in out Stack_T; Item : Integer);
      procedure Pop (This : in out Stack_T; Item : out Integer);
      function Is_Empty (This : Stack_T) return Boolean;
   private
      type List_T is array (1 .. Max_Capacity) of Integer;
      type Stack_T is record
         List : List_T;
         Top  : Integer range 0 .. Max_Capacity := 0;
      end record;
   end Bounded_Stack;

   package body Bounded_Stack is
      procedure Push (This : in out Stack_T; Item : Integer) is
      begin
         if This.Top < Max_Capacity then
            This.Top             := This.Top + 1;
            This.List (This.Top) := Item;
         end if;
      end Push;
      procedure Pop (This : in out Stack_T; Item : out Integer) is
      begin
         if This.Top > 0 then
            Item     := This.List (This.Top);
            This.Top := This.Top - 1;
         end if;
      end Pop;
      function Is_Empty (This : Stack_T) return Boolean is (This.Top = 0);
   
   end Bounded_Stack;

   with Ada.Text_IO;   use Ada.Text_IO;
   with Bounded_Stack; use Bounded_Stack;
   procedure Main is
      Stack : Stack_T;
      Item  : Integer;
   begin
      Push (Stack, 42);
      Put_Line (Boolean'Image (Is_Empty (Stack)));
      Pop (Stack, Item);
      --Put_Line (Integer'Image (Stack.Top)); -- compile error
      Put_Line (Boolean'Image (Is_Empty (Stack)));
      Put_Line (Item'Image);
   end Main;
