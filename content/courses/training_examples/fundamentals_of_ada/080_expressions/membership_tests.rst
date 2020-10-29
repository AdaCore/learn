.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Expressions.membership_tests
    :class: ada-run

   procedure Membership_Tests is
      subtype Index_T is Integer range 1 .. 100;
      X : Integer;
      B : Boolean := X in 1 .. 100;
      C : Boolean := not (X in Index_T);
      D : Boolean := X not in Index_T;
   
      type Calendar_Days is (Sun, Mon, Tues, Wed, Thur, Fri, Sat);
      subtype Weekdays is Calendar_Days range Mon .. Fri;
      Day : Calendar_Days;
   
   begin
      -- identical expressions
      B := Day in Mon .. Fri;
      C := Day in Weekdays;
   
      if Day in Sun | Sat
      then
         null;
      end if;
   end Membership_Tests;
