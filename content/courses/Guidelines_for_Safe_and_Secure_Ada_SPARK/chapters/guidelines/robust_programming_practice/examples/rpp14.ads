package Rpp14 is

   package Non_Compliant is
      type Array_T is array (0 .. 31) of Boolean;
      function Any_Set (X : Array_T) return Boolean is
         (for some Flag in 0 .. 31 => X (Flag));
   end Non_Compliant;

   package Compliant is
      Number_Of_Bits : constant := 32;
      type Array_T is array (0 .. Number_Of_Bits - 1) of Boolean;
      function Any_Set (X : Array_T) return Boolean is
         (for some Flag in X'Range => X (Flag));
   end Compliant;

end Rpp14;
