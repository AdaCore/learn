.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Record_Types.variant_records
   
   with Ada.Text_IO; use Ada.Text_IO;
   procedure Variant_Records is
      type Person_Tag is (Student, Faculty);
      type Person (Tag : Person_Tag) is -- Tag is the discriminant
      record
         Name : String (1 .. 10); -- Always present
         case Tag is
            when Student => -- 1st variant
               Gpa  : Float range 0.0 .. 4.0;
               Year : Integer range 1 .. 4;
            when Faculty => -- 2nd variant
               Pubs : Integer;
         end case;
      end record;
      S : Person (Student) :=
        (Tag => Student, Name => (others => 'S'), Gpa => 4.0, Year => 4);
      F : Person (Faculty) :=
        (Tag => Faculty, Name => (others => 'F'), Pubs => 10);
   begin
      Put_Line (S.Name & " " & F.Name);
      Put_Line (S.Gpa'Image);
      Put_Line (S.Pubs'Image); -- run-time error
      Put_Line (F.Pubs'Image);
      Put_Line (F.Year'Image); -- run-time error
   end Variant_Records;
