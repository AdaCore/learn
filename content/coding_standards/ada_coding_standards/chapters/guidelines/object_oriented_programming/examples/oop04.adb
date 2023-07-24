procedure Oop04 is

   package Root is
      type Root_T is tagged null record;
      procedure Noncompliant (X : in out Root_T) is null;
      procedure Compliant (X : in out Root_T) is null;
      procedure Other_Prim (X : in out Root_T) is null;
   end Root;

   package Child is
      use Root;
      type Child_T is new Root_T with null record;
      procedure Noncompliant (X : in out Child_T);
      procedure Compliant (X : in out Child_T);
      procedure Other_Prim (X : in out Child_T);
   end Child;

   procedure Not_A_Primitive (X : in out Child.Child_T) is null;

   package body Child is

      procedure Noncompliant (X : in out Child_T) is
      begin
         Other_Prim (Root_T (X));
         Other_Prim (X);
      end Noncompliant;

      procedure Compliant (X : in out Child_T) is
      begin
         Compliant (Root_T (X)); -- constructor style is OK
         Not_A_Primitive (X);
      end Compliant;

      procedure Other_Prim (X : in out Child_T) is null;
   end Child;

begin

   null;

end Oop04;
