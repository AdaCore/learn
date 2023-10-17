package Rpp03 is

   package Noncompliant is
      type Record_T is record
         Field1 : Integer   := 1;
         Field2 : Boolean   := False;
         Field3 : Character := ' ';
      end record;
      type Array_T is array (Character) of Boolean;
      Rec : Record_T := (Field1 => 1,
                         Field3 => '2',
                         others => <>);
      Arr : Array_T := ('0' .. '9' => True,
                        others     => False);
   end Noncompliant;

   package Compliant is
      type Record_T is record
         Field1 : Integer   := 1;
         Field2 : Boolean   := False;
         Field3 : Character := ' ';
      end record;
      type Array_T is array (Character) of Boolean;
      Rec : Record_T := (Field1 => 1,
                         others => <>);
      Arr : Array_T := (others => False);
   end Compliant;

end Rpp03;
