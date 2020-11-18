.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Access_Types.access_types
   :class: ada-run

   package Access_Types is
   
      type R is record
         F1, F2 : Integer;
      end record;
      type A_Int is access Integer;
      type A_String is access all String;
      type A_R is access R;
   
      V_Int    : A_Int    := new Integer;
      V_String : A_String := new String'("abc");
      V_R      : A_R      := new R;
   
      procedure Do_Something;
   
   end Access_Types;

   package body Access_Types is
   
      function Local_Access_Example return Integer is
         type String_Access is access String; -- only visible here
         X : String_Access;
      begin
         X := new String'("Hello, World");
         return X.all'Length;
      end Local_Access_Example;
   
      procedure Do_Something is
      begin
         V_Int.all    := Local_Access_Example;
         V_String.all := "cde";
         V_String (1) := 'z'; -- similar to V_String.all (1) := 'z';
         V_R.all      := (0, 0);
         V_R.F1       := 1; -- similar to V_R.all.F1 := 1;
         V_Int        := null;
         V_R          := null;
      end Do_Something;
   
   end Access_Types;
