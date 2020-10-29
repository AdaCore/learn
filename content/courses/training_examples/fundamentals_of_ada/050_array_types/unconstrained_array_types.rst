.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Array_Types.unconstrained_array_types
    :class: ada-run
   
   procedure Unconstrained_Array_Types is
   
      type Index_T is range 1 .. 100;
      type List_T is array (Index_T range <>) of Character;
      Wrong : List_T (0 .. 10); -- runtime error
      Right : List_T (11 .. 20);
   
      type Array_Of_Bits_T is array (Natural range <>) of Boolean;
      Bits8  : Array_Of_Bits_T (0 .. 7);
      Bits16 : Array_Of_Bits_T (1 .. 16);
   
      type Days_T is (Sun, Mon, Tues, Wed, Thu, Fri, Sat);
      type Schedule_T is array (Days_T range <>) of Float;
      Schedule : Schedule_T (Mon .. Fri);
   
      Name : String (1 .. 10);
   
      type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
      type Roman_Number is array (Natural range <>) of Roman_Digit;
      Orwellian : constant Roman_Number := "MCMLXXXIV";
   
   begin
      null;
   end Unconstrained_Array_Types;
