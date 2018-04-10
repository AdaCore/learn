function Int_To_String (Int : Integer) return String is
   Buffer      : String (1 .. 10);
   Temp        : Integer := abs Int;
   Start_Index : Positive;
begin
   for I in reverse Buffer'Range loop
      Buffer (I) := Character'Val(Character'Pos ('0') + Temp mod 10);
      Temp := Temp / 10;
      if Temp = 0 then
         Start_Index := I;
         exit;
      end if;
   end loop;

   return (if Int < 0 then "-" else "") & Buffer (Start_Index .. Buffer'Last);
end Int_To_String;
