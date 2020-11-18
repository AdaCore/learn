.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Overloading.call_resolution
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Call_Resolution is
      type Colors_T is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
      type Rgb_T is (Red, Green, Blue);
      function Str (P : Colors_T) return String is (Colors_T'Image (P));
      function Str (P : Rgb_T) return String is (Rgb_T'Image (P));
      procedure Print (Color : Colors_T) is
      begin
         Put_Line (Str (Color));
      end Print;
      procedure Print (Rgb : Rgb_T) is
      begin
         Put_Line (Str (Rgb));
      end Print;
      procedure Print (P1 : Colors_T; P2 : Rgb_T) is null;
   
   begin
      Put_Line (Str (Yellow));
      -- Put_Line (Str (Red)); -- compile error
      Print (Orange);
      Print (Rgb => Red);
      Print (Color => Blue);
      Print (Red, Red);
   end Call_Resolution;
