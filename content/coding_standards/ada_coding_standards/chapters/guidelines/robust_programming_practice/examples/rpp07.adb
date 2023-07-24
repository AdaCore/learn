procedure Rpp07 (X : in out Integer) is
   A, B : Integer;

   function Noncompliant (Value : in out Integer) return Integer is
   begin
      if Value < Integer'last then
         Value := Value + 1;
      end if;
      return Value;
   end Noncompliant;

   function Compliant (Value : Integer) return Integer is
   begin
      return Value + 1;
   end Compliant;

   procedure Compliant (Value : in out Integer) is
   begin
      if Value < Integer'last then
         Value := Value + 1;
      end if;
   end Compliant;

begin
   A := X;
   B := Noncompliant (A);
   X := Compliant (B);
end Rpp07;
