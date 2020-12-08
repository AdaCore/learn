.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Overloading.enumerals_and_operators
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Enumerals_And_Operators is
      type Colors_T is (Blue, Yellow, Black, Green, Red);
      type Rgb_T is (Red, Green, Blue);
      type Stoplight_T is (Green, Yellow, Red);
   
      Color : constant Colors_T    := Red;
      Rgb   : constant Rgb_T       := Red;
      Light : constant Stoplight_T := Red;
   
      type Miles_T is digits 6;
      type Hour_T is digits 6;
      type Speed_T is digits 6;
      function "/"
        (M : Miles_T;
         H : Hour_T)
         return Speed_T is (Speed_T (Float (M) / Float (H)));
      function "*"
        (Mph : Speed_T;
         H   : Hour_T)
         return Miles_T is (Miles_T (Float (Mph) * Float (H)));
   
      M   : Miles_T         := Miles_T (Col);
      H   : constant Hour_T := Hour_T (Line);
      Mph : Speed_T;
   
   begin
   
      Put_Line (Color'Image & " " & Rgb'Image & " " & Light'Image);
      Mph := M / H;
      M   := Mph * H;
      Put_Line (Mph'Image & M'Image);
   
      Mph := "/"
          (M => M,
           H => H);
      M := "*" (Mph, H);
      Put_Line (Mph'Image & M'Image);
   
   end Enumerals_And_Operators;
