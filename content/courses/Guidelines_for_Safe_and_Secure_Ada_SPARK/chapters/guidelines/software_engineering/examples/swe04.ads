package Swe04 is

   type Capacity_T is range 0 .. 100;

   package Noncompliant is
      type Content_T is array (Capacity_T range <>) of Integer;
      type Stack_T (Capacity : Capacity_T) is tagged record
         Content : Content_T (1 .. Capacity);
         Top     : Capacity_T := 0;
      end record;
      procedure Push
        (Stack : in out Stack_T;
         Item  :        Integer);
      procedure Pop
        (Stack : in out Stack_T;
         Item  :    out Integer);
   end Noncompliant;

   package Compliant is
      type Stack_T (Capacity : Capacity_T) is tagged private;
      procedure Push
        (Stack : in out Stack_T;
         Item  :        Integer);
      procedure Pop
        (Stack : in out Stack_T;
         Item  :    out Integer);
   private
      type Content_T is array (Capacity_T range <>) of Integer;
      type Stack_T (Capacity : Capacity_T) is tagged record
         Content : Content_T (1 .. Capacity);
         Top     : Capacity_T := 0;
      end record;
   end Compliant;

end Swe04;
