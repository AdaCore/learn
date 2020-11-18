.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Inheritance.primitives
   :class: ada-run

   package Primitives_Example is
   
      type Record_T is record
         Field : Integer;
      end record;
      type Access_To_Record_T is access Record_T;
      type Array_T is array (1 .. 10) of Integer;
   
      procedure Primitive_Of_Record_T (P : in out Record_T) is null;
      function Primitive_Of_Record_T (P : Integer) return Record_T is
        ((Field => P));
      procedure Primitive_Of_Record_T (I : Integer; P : access Record_T) is null;
      procedure Not_A_Primitive_Of_Record_T
        (I : Integer; P : Access_To_Record_T) is null;
   
      procedure Primitive_Of_Record_T_And_Array_T
        (P1 : in out Record_T; P2 : in out Array_T) is null;
   end Primitives_Example;
