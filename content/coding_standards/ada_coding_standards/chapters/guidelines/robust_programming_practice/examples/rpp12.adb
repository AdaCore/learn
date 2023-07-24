function Rpp12
  (Number : Positive)
   return Positive is

   A, B : Positive;

   function Noncompliant (N : Positive) return Positive is
   begin
      if N = 1 then
         return 1;
      else
         return N * Noncompliant (N - 1);  -- could overflow
      end if;
   end Noncompliant;

   function Compliant (N : Positive) return Positive is
      Result : Positive := 1;
   begin
      for K in 2 .. N loop
         Result := Result * K;  -- could overflow
      end loop;
      return Result;
   end Compliant;

begin

   A := Noncompliant (Number);
   B := Compliant (Number);
   return A + B;

end Rpp12;
