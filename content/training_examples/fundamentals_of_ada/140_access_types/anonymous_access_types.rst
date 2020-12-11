.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Anonymous_Access_Types.access_types
   :class: ada-run

   package Anonymous_Access_Types is
   
      type Access_T is access all Integer;
      Global : Access_T := new Integer'(123);
   
      function F1 (Param : access Integer) return Boolean is (Param = null);
      function F2 (Param : access Integer) return Boolean is (F1 (Param));
   
      function F3
        (Param : access Integer)
         return Boolean is
        (F1
           (Param) -- Param ia an anonymous access type
      or F2 (Global)); -- Global is a named access type
   end Anonymous_Access_Types;

   package Primitives_And_Access_Type is
   
      type Root_T is tagged null record;
      type Access_Root_T is access all Root_T;
      function Primitive_Of_Root (V : access Root_T) return Boolean is (V = null);
      function Action_On_Access (V : Access_Root_T) return Boolean is (V = null);
      type Child_T is new Root_T with null record;
      type Access_Child_T is access all Child_T;
      overriding function Primitive_Of_Root (V : access Child_T) return Boolean is
        (False);
      -- overriding function Action_On_Access (V : access_child_t_t) return
      -- boolean is ( false ); -- illegal
   
   end Primitives_And_Access_Type;

   with Ada.Text_IO; use Ada.Text_IO;
   with Anonymous_Access_Types;
   with Primitives_And_Access_Type;
   procedure Anonymous_Access_Modifiers is
   
      Global : aliased Primitives_And_Access_Type.Root_T;
   
      type Constant_Access_T is access constant Integer;
      type Not_Null_Access_T is not null access Integer;
   
      Constant_Access_Object : Constant_Access_T := new Integer'(123);
      Not_Null_Access_Object : Not_Null_Access_T := new Integer'(345);
   
   begin
   
      Put_Line
        (Boolean'Image (Anonymous_Access_Types.F3 (Not_Null_Access_Object)));
      Put_Line
        (Boolean'Image
           (Primitives_And_Access_Type.Primitive_Of_Root (Global'Access)));
   
      Put_Line (Integer'Image (Not_Null_Access_Object.all));
      Not_Null_Access_Object := new Integer'(Constant_Access_Object.all);
      Put_Line (Integer'Image (Not_Null_Access_Object.all));
   
      -- Constant_Access_Object.all := Not_Null_Access_Object.all; -- illegal
      Constant_Access_Object := null; -- legal
      Put_Line (Boolean'Image (Constant_Access_Object = null));
   
   end Anonymous_Access_Modifiers;
