package Oop05 is

   type Root_T is tagged null record;
   procedure Primitive (X : in out Root_T) is null;

   type Noncompliant_Child_T is new Root_T with null record;
   procedure Primitive (X : in out Noncompliant_Child_T) is null;

   type Compliant_Child_T is new Root_T with null record;
   overriding procedure Primitive (X : in out Compliant_Child_T) is null;

end Oop05;
