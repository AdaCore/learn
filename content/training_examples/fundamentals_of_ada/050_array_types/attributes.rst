.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Array_Types.attributes
    :class: ada-run

   procedure Attributes is
   
      type Array_Of_Bits_T is array (Natural range <>) of Boolean;
      Bits8 : Array_Of_Bits_T (0 .. 7);
   
      type Array_Of_Bitstrings_T is
        array (Natural range <>, Natural range <>) of Boolean;
      Bitstrings : Array_Of_Bitstrings_T (1 .. 10, 0 .. 16);
   
      Value : Natural;
   
   begin
   
      Value := 0;
      for Index in Bits8'First .. Bits8'Last loop
         if Bits8 (Index) then
            Value := Value + 2**(Index - Bits8'First);
         end if;
      end loop;
   
      for String_Index in Bitstrings'Range (1) loop
         Value := 0;
         for Bit_Index in Bitstrings'Range (2) loop
            if Bitstrings (String_Index, Bit_Index) then
               Value := Value + 2**(Bit_Index - Bitstrings'First (2));
            end if;
         end loop;
      end loop;
   
   end Attributes;
