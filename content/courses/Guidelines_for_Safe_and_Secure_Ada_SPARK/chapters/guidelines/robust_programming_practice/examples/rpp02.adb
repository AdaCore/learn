procedure Rpp02 (X : in out Character) is
   subtype Digit_T is Character range '0' .. '9';

   procedure Noncompliant (C : in out Character) is
   begin
      case Digit_T (C) is
         when '0' | '9' =>
            C := Character'Succ (C);
         when '1' .. '8' =>
            C := Character'Pred (C);
      end case;
   end Noncompliant;

   procedure Compliant (C : in out Character) is
   begin
      case Digit_T (C) is
         when '0' | '9' =>
            C := Character'Succ (C);
         when '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' =>
            C := Character'Pred (C);
      end case;
   end Compliant;

begin
   Noncompliant (X);
   Compliant (X);
end Rpp02;
