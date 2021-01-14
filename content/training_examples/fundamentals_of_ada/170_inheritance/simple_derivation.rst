.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Inheritance.simple_derivation
   :class: ada-run

   package Simple_Derivation is
   
      type Parent_T is range 1 .. 10;
      function Primitive1 (V : Parent_T) return String is
        ("Primitive1 of Parent_T" & V'Image);
      function Primitive2 (V : Parent_T) return String is
        ("Primitive2 of Parent_T" & V'Image);
      function Primitive3 (V : Parent_T) return String is
        ("Primitive3 of Parent_T" & V'Image);
   
      type Child_T is new Parent_T;
      -- implicitly gets access to Primitive1
   
      -- new behavior for Primitive2
      overriding function Primitive2 (V : Child_T) return String is
        ("Primitive2 of Child_T" & V'Image);
   
      -- remove behavior for Primitive3 from Child_T
      overriding function Primitive3 (V : Child_T) return String is abstract;
   
      -- add primitive only for Child_T
      not overriding function Primitive4 (V : Child_T) return String is
        ("Primitive4 of Child_T" & V'Image);
   
   end Simple_Derivation;

   with Ada.Text_IO;       use Ada.Text_IO;
   with Simple_Derivation; use Simple_Derivation;
   procedure Test_Simple_Derivation is
      function Not_A_Primitive (V : Parent_T) return String is
        ("Not_A_Primitive" & V'Image);
      Parent_V : Parent_T := 1;
      Child_V  : Child_T  := 2;
   begin
   
      Put_Line ("Parent_V - " & Primitive1 (Parent_V));
      Put_Line ("Parent_V - " & Primitive2 (Parent_V));
      Put_Line ("Parent_V - " & Primitive3 (Parent_V));
      -- Put_Line ("Parent_V - " & Primitive4 (Parent_V)); -- illegal
   
      Put_Line ("Child_V - " & Primitive1 (Child_V));
      Put_Line ("Child_V - " & Primitive2 (Child_V));
      -- Put_Line ("Child_V - " & Primitive3 (Child_V)); -- illegal
      Put_Line ("Child_V - " & Primitive4 (Child_V));
   
      Put_Line (Not_A_Primitive (Parent_V));
      Put_Line (Not_A_Primitive (Parent_T (Child_V)));
   
   end Test_Simple_Derivation;
