.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Access_Types.general_access_types
   :class: ada-run

   package General is
      type Pointed_To_T is new Integer;
      type Access_T is access all Pointed_To_T;
      Object : Access_T := new Pointed_To_T;
   
      type Other_Access_T is access all Pointed_To_T;
      Other_Object : Other_Access_T := Other_Access_T (Object);
   
      Pointed_To : aliased Pointed_To_T := 1_234;
   
   end General;

   with Ada.Text_IO; use Ada.Text_IO;
   with General;     use General;
   procedure Use_General is
   begin
      Object := Pointed_To'Access;
      Put_Line (Pointed_To'Image & Pointed_To_T'Image (Object.all));
      Pointed_To := Pointed_To + 1;
      Put_Line (Pointed_To'Image & Pointed_To_T'Image (Object.all));
      Object.all := Object.all * 2;
      Put_Line (Pointed_To'Image & Pointed_To_T'Image (Object.all));
   end Use_General;
