.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Polymorphism.dispatching_and_redispatching
   :class: ada-run

   package Types is
   
      type Root_T is tagged null record;
      function Primitive (V : Root_T) return String is ("Root_T");
   
      type Child_T is new Root_T with null record;
      function Primitive (V : Child_T) return String is ("Child_T");
   
   end Types;

   with Ada.Text_IO; use Ada.Text_IO;
   with Types;       use Types;
   procedure Test_Dispatching_And_Redispatching is
   
      Root_Object  : Root_T;
      Child_Object : Child_T;
   
      V1 : constant Root_T'Class  := Root_Object;
      V2 : constant Root_T'Class  := Child_Object;
      V3 : constant Child_T'Class := Child_Object;
   
   begin
   
      Put_Line (Primitive (V1));
      Put_Line (Primitive (V2));
      Put_Line (Primitive (V3));
   
   end Test_Dispatching_And_Redispatching;
