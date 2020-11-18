.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Inheritance.Tagged_derivation
   :class: ada-run

   package Tagged_Derivation is
   
      type Root_T is tagged record
         Root_Field : Integer;
      end record;
      function Primitive_1 (This : Root_T) return Integer is (This.Root_Field);
      function Primitive_2 (This : Root_T) return String is
        (Integer'Image (This.Root_Field));
   
      type Child_T is new Root_T with record
         Child_Field : Integer;
      end record;
      overriding function Primitive_2 (This : Child_T) return String is
        (Integer'Image (This.Root_Field) & " " &
         Integer'Image (This.Child_Field));
      function Primitive_3 (This : Child_T) return Integer is
        (This.Root_Field + This.Child_Field);
   
      -- type Simple_Deriviation_T is new Child_T; -- illegal
   
      type Root2_T is tagged record
         Root_Field : Integer;
      end record;
      -- procedure Primitive_4 (X : Root_T; Y : Root2_T); -- illegal
   
   end Tagged_Derivation;

   with Ada.Text_IO;       use Ada.Text_IO;
   with Tagged_Derivation; use Tagged_Derivation;
   procedure Test_Tagged_Derivation is
      Root  : Root_T  := (Root_Field => 1);
      Child : Child_T := (Root_Field => 11, Child_Field => 22);
   begin
      Put_Line ("Root: " & Primitive_2 (Root));
      Put_Line ("Child: " & Primitive_2 (Child));
      Root := Root_T (Child);
      Put_Line ("Root from Child: " & Primitive_2 (Root));
      -- Child := Child_T ( Root ); -- illegal put_line ( "Child from Root: " &
      -- primitive_2 ( Child ) );
      Child := (Root with Child_Field => 999);
      Put_Line ("Child from Root via aggregate: " & Primitive_2 (Child));
   end Test_Tagged_Derivation;
