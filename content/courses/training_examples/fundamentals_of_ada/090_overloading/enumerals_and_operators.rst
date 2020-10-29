.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Overloading.enumerals_and_operators
   :class: ada-run

   procedure Enumerals_And_Operators is
      type Colors_T is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
      type Rgb_T is (Red, Green, Blue);
      type Stoplight_T is (Green, Yellow, Red);
   
      Color : Colors_T    := Red;
      Rgb   : Rgb_T       := Red;
      Light : Stoplight_T := Red;
   
      type Miles_T is digits 6;
      type Hour_T is digits 6;
      type Speed_T is digits 6;
      function "/" (M : Miles_T; H : Hour_T) return Speed_T is
         (Speed_T (Float (M) / Float (H)));
      function "*" (Mph : Speed_T; H   : Hour_T) return Miles_T is
         (Miles_T (Float (Mph) * Float (H)));
   
      M   : Miles_T;
      H   : Hour_T;
      Mph : Speed_T;
   
   begin
      Mph := M / H;
      M   := Mph * H;
   
      Mph := "/" (M => M, H => H);
      M := "*" (Mph, H);
   end Enumerals_And_Operators;
