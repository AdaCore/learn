procedure Exu01 (X : in out Integer) is
   Math_Overflow : exception;

   procedure Noncompliant (X : in out Integer) is
   begin
      if X < Integer'Last / 2
      then
         X := X * 2;
      else
         raise Constraint_Error;
      end if;
   end Noncompliant;

   procedure Compliant (X : in out Integer) is
   begin
      if X < Integer'Last / 2
      then
         X := X * 2;
      else
         raise Math_Overflow;
      end if;
   end Compliant;

begin

   null;

end Exu01;
