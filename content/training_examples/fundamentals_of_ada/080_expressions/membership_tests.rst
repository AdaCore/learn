.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Expressions.membership_tests
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Membership_Tests is
      subtype Index_T is Integer range 1 .. 100;
      X : constant Integer := Integer (Line);
      B : Boolean          := X in 1 .. 100;
      C : Boolean          := not (X in Index_T);
      D : Boolean          := X not in Index_T;
   
      type Calendar_Days is (Sun, Mon, Tues, Wed, Thur, Fri, Sat);
      subtype Weekdays is Calendar_Days range Mon .. Fri;
      Day : Calendar_Days := Calendar_Days'Val (X);
   
   begin
   
      if Day in Sun | Sat then
         -- identical expressions
         B   := Day in Mon .. Fri;
         C   := Day in Weekdays;
         Day := Wed;
      elsif Day = Mon or Day = Tues then
         D   := D and (B or C);
         Day := Thur;
      end if;
   
      Put_Line (D'Image & " " & B'Image & " " & C'Image);
      Put_Line (Day'Image);
   
   end Membership_Tests;
