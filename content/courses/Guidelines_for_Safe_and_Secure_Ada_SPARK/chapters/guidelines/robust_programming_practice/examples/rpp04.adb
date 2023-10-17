procedure Rpp04
  (Register  :     Character;
   Registera : out Integer;
   Registerb : out Integer) is

   type Value_T is (Alpha, Baker, Charlie, Dog, Invalid);
   for Value_T use
     (Alpha   => 2#1#,
      Baker   => 2#10#,
      Charlie => 2#100#,
      Dog     => 2#1000#,
      Invalid => 2#1111#);

   procedure Noncompliant (Register  :     Character;
                           Registera : out Value_T;
                           Registerb : out Value_T) is
   begin
      if Register = 'A' then
         Registera := Alpha;
      end if;
   end Noncompliant;

   procedure Compliant (Register  :     Character;
                        Registera : out Value_T;
                        Registerb : out Value_T) is
   begin
      Registera := Invalid;
      Registerb := Invalid;
      if Register = 'A' then
         Registera := Alpha;
      end if;
   end Compliant;

   A, B : Value_T;

begin
   Noncompliant (Register, A, B);
   Compliant (Register, A, B);
   Registera := Value_T'enum_rep (A);
   Registerb := Value_T'enum_rep (B);

end Rpp04;
