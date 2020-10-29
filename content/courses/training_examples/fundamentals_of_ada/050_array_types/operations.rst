.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Array_Types.operations
    :class: ada-run
   
   procedure Operations is
   
      type Boolean_Array_T is array (0 .. 15) of Boolean;
      Bool1, Bool2, Bool3 : Boolean_Array_T;
   
      type Integer_Array_T is array (1 .. 100) of Integer;
      Int1, Int2 : Integer_Array_T;
   
      Str1 : String (1 .. 10);
      Str2 : String (2 .. 9);
   
      Flag : Boolean;
   
   begin
   
      Bool3 := Bool1 or Bool2;
      Flag  := Int1 > Int2;
   
      declare
         Str3 : String := Str1 & Str2;
      begin
         Str3 (Str3'First .. Str3'First + 1) := "**";
         Str3 (1 .. 4) := Str1 (1 .. 2) & Str2 (8 .. 9);
      end;
   
      if Int1 (1) in Bool1'Range then
         Bool2 (Int1 (1)) := Int1 (1) > Int2 (1);
      end if;
   
   end Operations;
