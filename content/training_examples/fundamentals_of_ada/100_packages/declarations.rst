.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Packages.declarations
   :class: ada-run

   package Global_Data is
      Object : Integer := 100;
      type Float_T is digits 6;
   end Global_Data;

   with Global_Data;
   package Float_Stack is
      Max : constant Integer := Global_Data.Object;
      procedure Push (X : in Global_Data.Float_T);
      function Pop return Global_Data.Float_T;
   end Float_Stack;

   package body Float_Stack is
      Local_Object : Global_Data.Float_T;
      procedure Not_Exported is null;
      procedure Push (X : in Global_Data.Float_T) is
      begin
         Not_Exported;
         Local_Object := X;
      end Push;
      function Pop return Global_Data.Float_T is (Local_Object);
   end Float_Stack;
