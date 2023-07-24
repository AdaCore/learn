procedure Rpp05 (Value : in out Integer) is

   procedure Noncompliant (X : in out Integer) is
   begin
      X := X * X;
   exception
      when others =>
         X := -1;
   end Noncompliant;

   procedure Compliant (X : in out Integer) is
   begin
      X := X * X;
   exception
      when Constraint_Error =>
         X := -1;
   end Compliant;

begin
   Noncompliant (Value);
   Compliant (Value);
end Rpp05;
