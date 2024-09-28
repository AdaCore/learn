Modular Programming
-------------------

Months
~~~~~~

.. code:: ada lab=Solutions.Modular_Programming.Months

    --  START LAB IO BLOCK
    in 0:Months_Chk
    out 0:Months: - January - February - March - April - May - June - July - August - September - October - November - December
    --  END LAB IO BLOCK

    package Months is

       Jan : constant String := "January";
       Feb : constant String := "February";
       Mar : constant String := "March";
       Apr : constant String := "April";
       May : constant String := "May";
       Jun : constant String := "June";
       Jul : constant String := "July";
       Aug : constant String := "August";
       Sep : constant String := "September";
       Oct : constant String := "October";
       Nov : constant String := "November";
       Dec : constant String := "December";

       procedure Display_Months;

    end Months;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Months is

       procedure Display_Months is
       begin
          Put_Line ("Months:");
          Put_Line ("- " & Jan);
          Put_Line ("- " & Feb);
          Put_Line ("- " & Mar);
          Put_Line ("- " & Apr);
          Put_Line ("- " & May);
          Put_Line ("- " & Jun);
          Put_Line ("- " & Jul);
          Put_Line ("- " & Aug);
          Put_Line ("- " & Sep);
          Put_Line ("- " & Oct);
          Put_Line ("- " & Nov);
          Put_Line ("- " & Dec);
       end Display_Months;

    end Months;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Months;           use Months;

    procedure Main is

       type Test_Case_Index is
         (Months_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Months_Chk =>
                Display_Months;
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

Operations
~~~~~~~~~~

.. code:: ada lab=Solutions.Modular_Programming.Operations

    --  START LAB IO BLOCK
    in 0:Operations_Chk
    out 0:Add (100, 2) =  102 Subtract (100, 2) =  98 Multiply (100, 2) =  200 Divide (100, 2) =  50
    in 1:Operations_Display_Chk
    out 1:Operations:  10 +  5 =  15,  10 -  5 =  5,  10 *  5 =  50,  10 /  5 =  2, Operations:  1 +  2 =  3,  1 -  2 = -1,  1 *  2 =  2,  1 /  2 =  0,
    --  END LAB IO BLOCK

    package Operations is

       function Add (A, B : Integer) return Integer;

       function Subtract (A, B : Integer) return Integer;

       function Multiply (A, B : Integer) return Integer;

       function Divide (A, B : Integer) return Integer;

    end Operations;

    package body Operations is

       function Add (A, B : Integer) return Integer is
       begin
          return A + B;
       end Add;

       function Subtract (A, B : Integer) return Integer is
       begin
          return A - B;
       end Subtract;

       function Multiply (A, B : Integer) return Integer is
       begin
          return A * B;
       end Multiply;

       function Divide (A, B : Integer) return Integer is
       begin
          return A / B;
       end Divide;

    end Operations;

    package Operations.Test is

       procedure Display (A, B : Integer);

    end Operations.Test;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Operations.Test is

       procedure Display (A, B : Integer) is
          A_Str : constant String := Integer'Image (A);
          B_Str : constant String := Integer'Image (B);
       begin
          Put_Line ("Operations:");
          Put_Line (A_Str & " + " & B_Str & " = "
                    & Integer'Image (Add (A, B))
                    & ",");
          Put_Line (A_Str & " - " & B_Str & " = "
                    & Integer'Image (Subtract (A, B))
                    & ",");
          Put_Line (A_Str & " * " & B_Str & " = "
                    & Integer'Image (Multiply (A, B))
                    & ",");
          Put_Line (A_Str & " / " & B_Str & " = "
                    & Integer'Image (Divide (A, B))
                    & ",");
       end Display;

    end Operations.Test;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Operations;
    with Operations.Test;  use Operations.Test;

    procedure Main is

       type Test_Case_Index is
         (Operations_Chk,
          Operations_Display_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Operations_Chk =>
                Put_Line ("Add (100, 2) = "
                          & Integer'Image (Operations.Add (100, 2)));
                Put_Line ("Subtract (100, 2) = "
                          & Integer'Image (Operations.Subtract (100, 2)));
                Put_Line ("Multiply (100, 2) = "
                          & Integer'Image (Operations.Multiply (100, 2)));
                Put_Line ("Divide (100, 2) = "
                          & Integer'Image (Operations.Divide (100, 2)));
             when Operations_Display_Chk =>
                Display (10, 5);
                Display ( 1, 2);
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;
