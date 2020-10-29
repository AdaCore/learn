.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Array_Types.operations_added_for_ada2012
    :class: ada-run
   
   with Ada.Text_IO; use Ada.Text_IO;
   procedure Operations_Added_For_Ada2012 is
   
      type Integer_Array_T is array (1 .. 10) of Integer with
         Default_Component_Value => -1;
      Int_Array : Integer_Array_T;
   
      type Matrix_T is array (1 .. 3, 1 .. 3) of Integer with
         Default_Component_Value => -1;
      Matrix : Matrix_T;
   
   begin
   
      for Index in Int_Array'First + 1 .. Int_Array'Last - 1 loop
         Int_Array (Index) := Index * 10;
      end loop;
      for Item of Int_Array loop
         Put_Line (Integer'Image (Item));
      end loop;
   
      for Index1 in Matrix_T'First (1) + 1 .. Matrix'Last (1) loop
         for Index2 in Matrix_T'First (2) + 1 .. Matrix'Last (2) loop
            Matrix (Index1, Index2) := Index1 * 100 + Index2;
         end loop;
      end loop;
      for Item of reverse Matrix loop
         Put_Line (Integer'Image (Item));
      end loop;
   
   end Operations_Added_For_Ada2012;
