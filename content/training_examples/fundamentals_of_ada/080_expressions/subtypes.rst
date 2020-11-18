.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Expressions.subtypes
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Subtypes is
      type Days_T is (Sun, Mon, Tues, Wed, Thurs, Fri, Sat);
      subtype Weekdays_T is Days_T range Mon .. Fri;
   
      Weekday      : Weekdays_T              := Mon;
      Also_Weekday : Days_T range Mon .. Fri := Tues;
      Day          : Days_T                  := Weekday;
   
      type Matrix_T is array (Integer range <>, Integer range <>) of Integer;
      subtype Matrix_3x3_T is Matrix_T (1 .. 3, 1 .. 3);
      subtype Line_T is String (1 .. 80);
   
      I : Integer := 1_234;
      procedure Takes_Positive (P : Positive) is null;
   
      type Tertiary_Switch is (Off, On, Neither) with
         Default_Value => Neither;
      subtype Toggle_Switch is Tertiary_Switch range Off .. On;
      Safe : Toggle_Switch := Off;
      -- Implicit : Toggle_Switch; -- compile error: out of range
   
      pragma Unreferenced (Safe);
   
   begin
      Also_Weekday := Day;  -- runtime error if Day is Sat or Sun
      Put_Line (Also_Weekday'Image);
      Day := Weekday;  -- always legal
      I   := I - 1;
      Takes_Positive (I); -- runtime error if I <= 0
   
      Weekday := Weekdays_T'Last;
      Day     := Days_T'Last;
      Put_Line (Weekdays_T'Image (Weekday) & " / " & Days_T'Image (Day));
   
      Put_Line (Days_T'Image (Weekdays_T'Succ (Weekday)));
   
      Put_Line (Integer'Image (Matrix_3x3_T'Length (1)));
      Put_Line (Integer'Image (Line_T'Length (1)));
   
   end Subtypes;
