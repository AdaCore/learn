.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Expressions.conditional_expressions
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Conditional_Expressions is
   
      type Months_T is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
      Year : Integer := 2_020;
   
      procedure If_Expression is
         Counter : Natural := 5;
      begin
         while Counter > 0 loop
            Put_Line ("Self-destruct in" & Natural'Image (Counter) &
                      (if Counter = 1 then " second" else " seconds"));
            delay 1.0;
            Counter := Counter - 1;
         end loop;
         Put_Line ("Boom! (goodbye Nostromo)");
      end If_Expression;
   
      procedure Case_Expression is
         Leap_Year : constant Boolean :=
           (Year mod 4 = 0 and Year mod 100 /= 0) or else (Year mod 400 = 0);
         End_Of_Month : array (Months_T) of Positive;
      begin
         for M in Months_T loop
            End_Of_Month (M) :=
              (case M is when Sep | Apr | Jun | Nov => 30,
                         when Feb => (if Leap_Year then 29 else 28),
                         when others => 31);
         end loop;
      end Case_Expression;
   
   begin
      If_Expression;
      Case_Expression;
   end Conditional_Expressions;
