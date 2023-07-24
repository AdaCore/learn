package Rpp13 is

   package Non_Compliant is
      type Boolean is range 0 .. 1 with Size => 1;
      type Character is ('A', 'E', 'I', 'O', 'U');
   end Non_Compliant;

   package Compliant is
      type Boolean_T is range 0 .. 1 with Size => 1;
      type Character_T is ('A', 'E', 'I', 'O', 'U');
   end Compliant;

end Rpp13;
