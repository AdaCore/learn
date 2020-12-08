.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Polymorphism.classes_of_types
   :class: ada-run

   package Class_Types is
   
      type Root_T is tagged null record;
      type Child1_T is new Root_T with null record;
      type Child2_T is new Root_T with null record;
      type Grandchild1_T is new Child1_T with null record;
   
      -- Root'Class = {Root_T, Child1_T, Child2_T, Grandchild1_T} Child1'Class =
      -- {Child1_T, Grandchild1_T} Child2'Class = {Child2_T} Granchild1'Class =
      -- {Grandchild1_T}
   
      procedure Test;
   
   end Class_Types;

   with Ada.Tags;    use Ada.Tags;
   with Ada.Text_IO; use Ada.Text_IO;
   with Class_Types; use Class_Types;
   package body Class_Types is
   
      Root_Object  : Root_T;
      Child_Object : Child1_T;
   
      Class_Object1 : Child1_T'Class := Child_Object;
      Class_Object2 : Root_T'Class   := Class_Object1;
      Class_Object3 : Root_T'Class   := Child_Object;
      -- Class_Object4 : Root_T'class; -- illegal
   
      procedure Do_Something (Object : in out Root_T'Class) is
      begin
         Put_Line
           ("Do_Something: " & Boolean'Image (Object in Root_T'Class) & " / " &
            Boolean'Image (Object in Child1_T'Class));
      end Do_Something;
   
      procedure Test is
      begin
   
         Put_Line (Boolean'Image (Class_Object1'Tag = Class_Object2'Tag));
         Put_Line (Boolean'Image (Class_Object2'Tag = Class_Object3'Tag));
   
         Do_Something (Root_Object);
         Do_Something (Child_Object);
         Do_Something (Class_Object1);
         Do_Something (Class_Object2);
         Do_Something (Class_Object3);
   
      end Test;
   
   end Class_Types;

   package Abstract_Types is
   
      type Root_T is abstract tagged record
         Field : Integer;
      end record;
      function Primitive1 (V : Root_T) return String is abstract;
      function Primitive2
        (Prompt : String;
         V      : Root_T)
         return String is (Prompt & "> " & Integer'Image (V.Field));
   
      type Child_T is abstract new Root_T with null record;
      -- Child_T does not need to redefine any primitives
   
      type Grandchild_T is new Child_T with null record;
      -- Grandchild_T is required to create a concrete version of Primitive2
      function Primitive1 (V : Grandchild_T) return String is
        (Integer'Image (V.Field));
   
      procedure Test;
   
   end Abstract_Types;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Abstract_Types is
   
      Object1 : constant Grandchild_T := (Field => 123);
      Object2 : constant Root_T'Class := Object1;
   
      procedure Test is
   
      begin
   
         Put_Line (Object1.Primitive1);
         Put_Line (Primitive2 ("Object1", Object2));
   
         Put_Line (Object2.Primitive1);
         Put_Line (Primitive2 ("Object2", Object2));
   
      end Test;
   
   end Abstract_Types;

   with Abstract_Types;
   with Class_Types;
   procedure Test is
   begin
      Class_Types.Test;
      Abstract_Types.Test;
   end Test;
