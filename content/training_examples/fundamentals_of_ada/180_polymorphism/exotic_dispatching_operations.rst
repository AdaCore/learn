.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Polymorphism.exotic_dispatching_operations
   :class: ada-run

   package Types is
   
      type Root_T is tagged record
         Field : Integer;
      end record;
      function Primitive
        (Left  : Root_T;
         Right : Root_T)
         return Integer is (Left.Field + Right.Field);
      function "="
        (Left  : Root_T;
         Right : Root_T)
         return Boolean is (Left.Field in Right.Field - 1 .. Right.Field + 1);
      function Constructor (I : Integer := 0) return Root_T is ((Field => I));
   
      type Child_T is new Root_T with null record;
      overriding function Primitive
        (Left  : Child_T;
         Right : Child_T)
         return Integer is (Left.Field * Right.Field);
      overriding function "="
        (Left  : Child_T;
         Right : Child_T)
         return Boolean is (Right.Field in Left.Field - 1 .. Left.Field + 1);
         -- function Constructor ( I : Integer := 0 ) return child_T; -- inherited
         -- from Root_t
   
      type Child2_T is new Root_T with record
         Field2 : Integer;
      end record;
      overriding function Primitive
        (Left  : Child2_T;
         Right : Child2_T)
         return Integer is (Left.Field * Right.Field);
      overriding function "="
        (Left  : Child2_T;
         Right : Child2_T)
         return Boolean is (Left.Field = Right.Field);
         -- must create a constructor because new fields added
      function Constructor (I : Integer := 0) return Child2_T is ((I, I));
   
   end Types;

   with Ada.Text_IO; use Ada.Text_IO;
   with Types;       use Types;
   procedure Test_Exotic_Dispatching_Operations is
   
      R1  : constant Root_T       := (Field => 10);
      R2  : constant Root_T       := (Field => 20);
      C1  : constant Child_T      := (Field => 10);
      Cl1 : constant Root_T'Class := R1;
      Cl2 : constant Root_T'Class := R2;
      Cl3 : constant Root_T'Class := C1;
   
      procedure Test_Primitive is
      begin
         Put_Line ("Primitive");
         Put_Line (Integer'Image (Primitive (R1, R2)));     -- static:  ok
         -- Put_Line (Integer'Image (Primitive (R1, C1))); -- static: error
         Put_Line (Integer'Image (Primitive (Cl1, Cl2)));   -- dynamic: ok
         -- Put_Line (Integer'Image (Primitive (R1, Cl1))); -- static: error
         Put_Line
           (Integer'Image (Primitive (Root_T'Class (R1), Cl1))); -- dynamic: ok
         Put_Line (Integer'Image (Primitive (Cl1, Cl3)));   -- dynamic: error
      end Test_Primitive;
   
      procedure Test_Equality is
      begin
         Put_Line ("Equality");
         Put_Line ("Cl1 = Cl2 " & Boolean'Image (Cl1 = Cl2));
         Put_Line ("Cl2 = Cl3 " & Boolean'Image (Cl2 = Cl3));
         Put_Line ("Cl3 = Cl1 " & Boolean'Image (Cl3 = Cl1));
      end Test_Equality;
   
      procedure Test_Constructor is
         -- Static call to Root_T primitive
         V1 : Root_T'Class := Root_T'(Constructor);
         V2 : Root_T'Class := V1;
         -- Static call to Child2_T primitive
         V3 : Root_T'Class := Child2_T'(Constructor);
         -- V4 : Root_T'Class := Constructor; -- What is the tag of V4?
      begin
         -- No
         -- V1 := Constructor;
         -- Yes
         V1 := Root_T'(Constructor);
      end Test_Constructor;
   
   begin
      Test_Equality;
      Test_Constructor;
      Test_Primitive;
   end Test_Exotic_Dispatching_Operations;
