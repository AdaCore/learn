.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Packages.declarations
   :class: ada-run

   package Global_Data is
      Object : Integer;
   end Global_Data;

   package Float_Stack is
      Max : constant := 100;
      procedure Push (X : in Float);
      procedure Pop (X : out Float);
   end Float_Stack;

   package body Float_Stack is
      procedure Not_Exported is null;
      procedure Push (X : in Float) is null;
      procedure Pop (X : out Float) is null;
   end Float_Stack;

   with Float_Stack;
   procedure Float_Stack_Reference is
      Count : Integer := 0;
   begin
      if Count < Float_Stack.Max then
         Float_Stack.Push (123.4);
      end if;
   end Float_Stack_Reference;
