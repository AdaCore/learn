.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Access_Types.pool_specific_access_types
   :class: ada-run

   package Pool_Specific is
      type Pointed_To_T is new Integer;
      type Access_T is access Pointed_To_T;
      Object : Access_T := new Pointed_To_T;
   
      type Other_Access_T is access Pointed_To_T;
      -- Other_Object : Other_Access_T := Other_Access_T ( Object ); -- illegal
   
      type String_Access_T is access String;
   end Pool_Specific;

   with Ada.Unchecked_Deallocation;
   with Ada.Text_IO;   use Ada.Text_IO;
   with Pool_Specific; use Pool_Specific;
   procedure Use_Pool_Specific is
      X : Access_T        := new Pointed_To_T'(123);
      Y : String_Access_T := new String (1 .. 10);
   
      procedure Free is new Ada.Unchecked_Deallocation (Pointed_To_T, Access_T);
   
   begin
      Put_Line (Y.all);
      Y := new String'("String will be long enough to hold this");
      Put_Line (Y.all);
      Put_Line (Pointed_To_T'Image (X.all));
      Free (X);
      Put_Line (Pointed_To_T'Image (X.all)); -- run-time error
   end Use_Pool_Specific;
