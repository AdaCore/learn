.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Low_Level_Programming.address_clauses_and_overlays
   :class: ada-run

   with Ada.Text_IO;             use Ada.Text_IO;
   with System;
   with System.Storage_Elements; use System.Storage_Elements;
   procedure Test_Address_Clauses_And_Overlays is
   
      type Bitfield_T is array (Integer range <>) of Boolean with
         Pack;
   
      V : aliased Integer; -- object can be referenced elsewhere
      pragma Volatile (V);  -- may be updated at any time
   
      V2 : aliased Integer;
      pragma Volatile (V2);
   
      V_A : constant System.Address  := V'Address;
      V_I : constant Integer_Address := To_Integer (V_A);
   
      --  This maps directly on to the bits of V
      V3 : aliased Bitfield_T (1 .. V'Size);
      for V3'Address use V_A; -- overlays V
   
      V4 : aliased Integer;
      --  Trust me, I know what I'm doing, this is V2
      for V4'Address use To_Address (V_I - 4);
   
      function Str (Bitfield : Bitfield_T) return String is
         Retval : String (Bitfield'First .. Bitfield'Last);
      begin
         for I in Bitfield'Range loop
            Retval (I) := (if Bitfield (I) then '1' else '0');
         end loop;
         return Retval;
      end Str;
   
   begin
   
      V := 123;
      Put (Integer'Image (V) & " => " & Str (V3));
      New_Line;
   
      V3 (V3'First + 2) := not V3 (V3'First + 2);
      Put (Str (V3) & " => " & Integer'Image (V));
      New_Line;
   
      V2 := 456;
      Put_Line ("V4 = " & Integer'Image (V4));
      V4 := 789;
      Put_Line ("V2 = " & Integer'Image (V2));
   
   end Test_Address_Clauses_And_Overlays;
