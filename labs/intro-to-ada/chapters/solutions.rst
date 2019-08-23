Solutions
=========

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Imperative Language
-------------------

Hello World
~~~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Hello_World

    --  START LAB IO BLOCK
    in 0:
    out 0:Hello World!
    --  END LAB IO BLOCK

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       Put_Line ("Hello World!");
    end Main;

Greetings
~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Greetings

    --  START LAB IO BLOCK
    in 0:John
    out 0:Hello John!
    in 1:Joanna
    out 1:Hello Joanna!
    --  END LAB IO BLOCK

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    procedure Main is

       procedure Greet (Name : String) is
       begin
          Put_Line ("Hello " & Name & "!");
       end Greet;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Greet (Argument (1));
    end Main;

Positive Or Negative
~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Positive_Or_Negative

    --  START LAB IO BLOCK
    in 0:0
    out 0:Zero
    in 1:1
    out 1:Positive
    in 2:-1
    out 2:Negative
    in 3:99999
    out 3:Positive
    in 4:-99999
    out 4:Negative
    --  END LAB IO BLOCK

    procedure Classify_Number (X : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Classify_Number (X : Integer) is
    begin
       if X > 0 then
          Put_Line ("Positive");
       elsif X < 0 then
          Put_Line ("Negative");
       else
          Put_Line ("Zero");
       end if;
    end Classify_Number;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Classify_Number;

    procedure Main is
       A : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       A := Integer'Value (Argument (1));

       Classify_Number (A);
    end Main;

Numbers
~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Numbers

    --  START LAB IO BLOCK
    in 0:1 5
    out 0: 1  2  3  4  5
    in 1:5 1
    out 1: 1  2  3  4  5
    in 2:-5 -1
    out 2:-5 -4 -3 -2 -1
    in 3:5 -1
    out 3:-1  0  1  2  3  4  5
    in 4:-5 1
    out 4:-5 -4 -3 -2 -1  0  1
    in 5:1 -1
    out 5:-1  0  1
    in 6:-1 -5
    out 6:-5 -4 -3 -2 -1
    --  END LAB IO BLOCK

    procedure Display_Numbers (A, B : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Numbers (A, B : Integer) is
       X, Y : Integer;
    begin
       if A <= B then
          X := A;
          Y := B;
       else
          X := B;
          Y := A;
       end if;

       for I in X .. Y loop
          Put_Line (Integer'Image (I));
       end loop;
    end Display_Numbers;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_Numbers;

    procedure Main is
       A, B : Integer;
    begin
       if Argument_Count < 2 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 2 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       A := Integer'Value (Argument (1));
       B := Integer'Value (Argument (2));

       Display_Numbers (A, B);
    end Main;

Subprograms
-----------

Subtract Procedure
~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Subtract_Proc

    --  START LAB IO BLOCK
    in 0:Sub_10_1_Chk
    out 0:Result:  9
    in 1:Sub_10_100_Chk
    out 1:Result: -90
    in 2:Sub_0_5_Chk
    out 2:Result: -5
    in 3:Sub_0_Minus_5_Chk
    out 3:Result:  5
    --  END LAB IO BLOCK

    procedure Subtract (A, B   :     Integer;
                           Result : out Integer);

    procedure Subtract (A, B   :     Integer;
                           Result : out Integer) is
    begin
       Result := A - B;
    end Subtract;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Subtract;

    procedure Main is
       type Test_Case_Index is
         (Sub_10_1_Chk,
          Sub_10_100_Chk,
          Sub_0_5_Chk,
          Sub_0_Minus_5_Chk);

       procedure Check (TC : Test_Case_Index) is
          Result : Integer;
       begin
          case TC is
          when Sub_10_1_Chk =>
             Subtract (10, 1, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_10_100_Chk =>
             Subtract (10, 100, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_5_Chk =>
             Subtract (0, 5, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_Minus_5_Chk =>
             Subtract (0, -5, Result);
             Put_Line ("Result: " & Integer'Image (Result));
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

Subtract Function
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Subtract_Func

    --  START LAB IO BLOCK
    in 0:Sub_10_1_Chk
    out 0:Result:  9
    in 1:Sub_10_100_Chk
    out 1:Result: -90
    in 2:Sub_0_5_Chk
    out 2:Result: -5
    in 3:Sub_0_Minus_5_Chk
    out 3:Result:  5
    --  END LAB IO BLOCK

    function Subtract (A, B : Integer) return Integer;

    function Subtract (A, B : Integer) return Integer is
    begin
       return A - B;
    end Subtract;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Subtract;

    procedure Main is
       type Test_Case_Index is
         (Sub_10_1_Chk,
          Sub_10_100_Chk,
          Sub_0_5_Chk,
          Sub_0_Minus_5_Chk);

       procedure Check (TC : Test_Case_Index) is
          Result : Integer;
       begin
          case TC is
          when Sub_10_1_Chk =>
             Result := Subtract (10, 1);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_10_100_Chk =>
             Result := Subtract (10, 100);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_5_Chk =>
             Result := Subtract (0, 5);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_Minus_5_Chk =>
             Result := Subtract (0, -5);
             Put_Line ("Result: " & Integer'Image (Result));
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

Equality function
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Equality_Func

    --  START LAB IO BLOCK
    in 0:Equal_Chk
    out 0: 0 is equal to  0.  1 is equal to  1.  2 is equal to  2.  3 is equal to  3.  4 is equal to  4.  5 is equal to  5.  6 is equal to  6.  7 is equal to  7.  8 is equal to  8.  9 is equal to  9.  10 is equal to  10.
    in 1:Inequal_Chk
    out 1: 0 isn't equal to -1.  1 isn't equal to  0.  2 isn't equal to  1.  3 isn't equal to  2.  4 isn't equal to  3.  5 isn't equal to  4.  6 isn't equal to  5.  7 isn't equal to  6.  8 isn't equal to  7.  9 isn't equal to  8.  10 isn't equal to  9.
    --  END LAB IO BLOCK

    function Is_Equal (A, B : Integer) return Boolean;

    function Is_Equal (A, B : Integer) return Boolean is
    begin
       return A = B;
    end Is_Equal;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Is_Equal;

    procedure Main is
       type Test_Case_Index is
         (Equal_Chk,
          Inequal_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Display_Equal (A, B  : Integer;
                                   Equal : Boolean) is
          begin
             Put (Integer'Image (A));
             if Equal then
                Put (" is equal to ");
             else
                Put (" isn't equal to ");
             end if;
             Put_Line (Integer'Image (B) & ".");
          end Display_Equal;

          Result : Boolean;
       begin
          case TC is
          when Equal_Chk =>
             for I in 0 .. 10 loop
                Result := Is_Equal (I, I);
                Display_Equal (I, I, Result);
             end loop;
          when Inequal_Chk =>
             for I in 0 .. 10 loop
                Result := Is_Equal (I, I - 1);
                Display_Equal (I, I - 1, Result);
             end loop;
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

    package Operations_Test is

       procedure Display_Operations (A, B : Integer);

    end Operations_Test;

    with Ada.Text_IO; use Ada.Text_IO;

    with Operations;  use Operations;

    package body Operations_Test is

       procedure Display_Operations (A, B : Integer) is
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
       end Display_Operations;

    end Operations_Test;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Operations;
    with Operations_Test;  use Operations_Test;

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
                Display_Operations (10, 5);
                Display_Operations ( 1, 2);
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

Strongly typed language
-----------------------

Colors
~~~~~~

.. code:: ada lab=Solutions.Strongly_Typed.Colors

    --  START LAB IO BLOCK
    in 0:HTML_Color_Range
    out 0:SALMON FIREBRICK RED DARKRED LIME FORESTGREEN GREEN DARKGREEN BLUE MEDIUMBLUE DARKBLUE
    in 1:HTML_Color_To_Integer
    out 1:16#FA8072# 16#B22222# 16#FF0000# 16#8B0000# 16#FF00# 16#228B22# 16#8000# 16#6400# 16#FF# 16#CD# 16#8B#
    in 2:Basic_HTML_Color_To_HTML_Color
    out 2:RED GREEN BLUE
    --  END LAB IO BLOCK

    package Color_Types is

       type HTML_Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       function To_Integer (C : HTML_Color) return Integer;

       type Basic_HTML_Color is
         (Red,
          Green,
          Blue);

       function To_HTML_Color (C : Basic_HTML_Color) return HTML_Color;

    end Color_Types;

    package body Color_Types is

       function To_Integer (C : HTML_Color) return Integer is
       begin
          case C is
             when Salmon      => return 16#FA8072#;
             when Firebrick   => return 16#B22222#;
             when Red         => return 16#FF0000#;
             when Darkred     => return 16#8B0000#;
             when Lime        => return 16#00FF00#;
             when Forestgreen => return 16#228B22#;
             when Green       => return 16#008000#;
             when Darkgreen   => return 16#006400#;
             when Blue        => return 16#0000FF#;
             when Mediumblue  => return 16#0000CD#;
             when Darkblue    => return 16#00008B#;
          end case;

       end To_Integer;

       function To_HTML_Color (C : Basic_HTML_Color) return HTML_Color is
       begin
          case C is
             when Red   => return Red;
             when Green => return Green;
             when Blue  => return Blue;
          end case;
       end To_HTML_Color;

    end Color_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Integer_Text_IO;

    with Color_Types;       use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (HTML_Color_Range,
          HTML_Color_To_Integer,
          Basic_HTML_Color_To_HTML_Color);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when HTML_Color_Range =>
                for I in HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (I));
                end loop;
             when HTML_Color_To_Integer =>
                for I in HTML_Color'Range loop
                   Ada.Integer_Text_IO.Put (Item  => To_Integer (I),
                                            Width => 1,
                                            Base  => 16);
                   New_Line;
                end loop;
             when Basic_HTML_Color_To_HTML_Color =>
                for I in Basic_HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (To_HTML_Color (I)));
                end loop;
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

Integer Types
~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Strongly_Typed.Integer_Types

    --  START LAB IO BLOCK
    in 0:I_100_Range
    out 0:0 100
    in 1:U_100_Range
    out 1:0 100
    in 2:U_100_Wraparound
    out 2:100 0
    in 3:U_100_To_I_100
    out 3:0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
    in 4:I_100_To_U_100
    out 4:0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
    in 5:D_50_Range
    out 5:10 50
    in 6:S_50_Range
    out 6:10 50
    in 7:I_100_To_D_50
    out 7:10 10 10 10 10 10 10 10 10 10 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
    in 8:I_100_To_S_50
    out 8:10 10 10 10 10 10 10 10 10 10 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
    in 9:D_50_To_I_100
    out 9:10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
    in 10:S_50_To_I_100
    out 10:10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
    --  END LAB IO BLOCK

    package Int_Types is

       type I_100 is range 0 .. 100;

       type U_100 is mod 101;

       function To_I_100 (V : U_100) return I_100;

       function To_U_100 (V : I_100) return U_100;

       type D_50 is new I_100 range 10 .. 50;

       subtype S_50 is I_100 range 10 .. 50;

       function To_D_50 (V : I_100) return D_50;

       function To_S_50 (V : I_100) return S_50;

       function To_I_100 (V : D_50) return I_100;

    end Int_Types;

    package body Int_Types is

       function To_I_100 (V : U_100) return I_100 is
       begin
          return I_100 (V);
       end To_I_100;

       function To_U_100 (V : I_100) return U_100 is
       begin
          return U_100 (V);
       end To_U_100;

       function To_D_50 (V : I_100) return D_50 is
          Min : constant I_100 := I_100 (D_50'First);
          Max : constant I_100 := I_100 (D_50'Last);
       begin
          if V > Max then
             return D_50'Last;
          elsif V < Min then
             return D_50'First;
          else
             return D_50 (V);
          end if;
       end To_D_50;

       function To_S_50 (V : I_100) return S_50 is
       begin
          if V > S_50'Last then
             return S_50'Last;
          elsif V < S_50'First then
             return S_50'First;
          else
             return V;
          end if;
       end To_S_50;

       function To_I_100 (V : D_50) return I_100 is
       begin
          return I_100 (V);
       end To_I_100;

    end Int_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Int_Types;        use Int_Types;

    procedure Main is
       package I_100_IO is new Ada.Text_IO.Integer_IO (I_100);
       package U_100_IO is new Ada.Text_IO.Modular_IO (U_100);
       package D_50_IO  is new Ada.Text_IO.Integer_IO (D_50);

       use I_100_IO;
       use U_100_IO;
       use D_50_IO;

       type Test_Case_Index is
         (I_100_Range,
          U_100_Range,
          U_100_Wraparound,
          U_100_To_I_100,
          I_100_To_U_100,
          D_50_Range,
          S_50_Range,
          I_100_To_D_50,
          I_100_To_S_50,
          D_50_To_I_100,
          S_50_To_I_100);

       procedure Check (TC : Test_Case_Index) is
       begin
          I_100_IO.Default_Width := 1;
          U_100_IO.Default_Width := 1;
          D_50_IO.Default_Width  := 1;

          case TC is
             when I_100_Range =>
                Put (I_100'First);
                New_Line;
                Put (I_100'Last);
                New_Line;
             when U_100_Range =>
                Put (U_100'First);
                New_Line;
                Put (U_100'Last);
                New_Line;
             when U_100_Wraparound =>
                Put (U_100'First - 1);
                New_Line;
                Put (U_100'Last + 1);
                New_Line;
             when U_100_To_I_100 =>
                for I in U_100'Range loop
                   I_100_IO.Put (To_I_100 (I));
                   New_Line;
                end loop;
             when I_100_To_U_100 =>
                for I in I_100'Range loop
                   Put (To_U_100 (I));
                   New_Line;
                end loop;
             when D_50_Range =>
                Put (D_50'First);
                New_Line;
                Put (D_50'Last);
                New_Line;
             when S_50_Range =>
                Put (S_50'First);
                New_Line;
                Put (S_50'Last);
                New_Line;
             when I_100_To_D_50 =>
                for I in I_100'Range loop
                   Put (To_D_50 (I));
                   New_Line;
                end loop;
             when I_100_To_S_50 =>
                for I in I_100'Range loop
                   Put (To_S_50 (I));
                   New_Line;
                end loop;
             when D_50_To_I_100 =>
                for I in D_50'Range loop
                   Put (To_I_100 (I));
                   New_Line;
                end loop;
             when S_50_To_I_100 =>
                for I in S_50'Range loop
                   Put (I);
                   New_Line;
                end loop;
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

Temperatures
~~~~~~~~~~~~

.. code:: ada lab=Solutions.Strongly_Typed.Temperatures

    --  START LAB IO BLOCK
    in 0:Celsius_Range
    out 0:-2.73150E+02 5.50485E+03
    in 1:Celsius_To_Int_Celsius
    out 1:-273 0 5505
    in 2:Int_Celsius_To_Celsius
    out 2:-2.73000E+02 0.00000E+00 5.50485E+03
    in 3:Kelvin_To_Celsius
    out 3:-2.73150E+02 0.00000E+00 5.50485E+03
    in 4:Celsius_To_Kelvin
    out 4:0.00000E+00 5.77800E+03
    --  END LAB IO BLOCK

    package Temperature_Types is

       type Celsius is digits 6 range -273.15 .. 5504.85;

       type Int_Celsius is range -273 .. 5505;

       function To_Celsius (T : Int_Celsius) return Celsius;

       function To_Int_Celsius (T : Celsius) return Int_Celsius;

       type Kelvin is digits 6 range 0.0 .. 5778.00;

       function To_Celsius (T : Kelvin) return Celsius;

       function To_Kelvin (T : Celsius) return Kelvin;

    end Temperature_Types;

    package body Temperature_Types is

       function To_Celsius (T : Int_Celsius) return Celsius is
          Min : constant Float := Float (Celsius'First);
          Max : constant Float := Float (Celsius'Last);

          F   : constant Float := Float (T);
       begin
          if F > Max then
             return Celsius (Max);
          elsif F < Min then
             return Celsius (Min);
          else
             return Celsius (F);
          end if;
       end To_Celsius;

       function To_Int_Celsius (T : Celsius) return Int_Celsius is
       begin
          return Int_Celsius (T);
       end To_Int_Celsius;

       function To_Celsius (T : Kelvin) return Celsius is
          F : constant Float := Float (T);
       begin
          return Celsius (F - 273.15);
       end To_Celsius;

       function To_Kelvin (T : Celsius) return Kelvin is
          F : constant Float := Float (T);
       begin
          return Kelvin (F + 273.15);
       end To_Kelvin;

    end Temperature_Types;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Temperature_Types; use Temperature_Types;

    procedure Main is
       package Celsius_IO     is new Ada.Text_IO.Float_IO (Celsius);
       package Kelvin_IO      is new Ada.Text_IO.Float_IO (Kelvin);
       package Int_Celsius_IO is new Ada.Text_IO.Integer_IO (Int_Celsius);

       use Celsius_IO;
       use Kelvin_IO;
       use Int_Celsius_IO;

       type Test_Case_Index is
         (Celsius_Range,
          Celsius_To_Int_Celsius,
          Int_Celsius_To_Celsius,
          Kelvin_To_Celsius,
          Celsius_To_Kelvin);

       procedure Check (TC : Test_Case_Index) is
       begin
          Celsius_IO.Default_Fore := 1;
          Kelvin_IO.Default_Fore  := 1;
          Int_Celsius_IO.Default_Width := 1;

          case TC is
             when Celsius_Range =>
                Put (Celsius'First);
                New_Line;
                Put (Celsius'Last);
                New_Line;
             when Celsius_To_Int_Celsius =>
                Put (To_Int_Celsius (Celsius'First));
                New_Line;
                Put (To_Int_Celsius (0.0));
                New_Line;
                Put (To_Int_Celsius (Celsius'Last));
                New_Line;
             when Int_Celsius_To_Celsius =>
                Put (To_Celsius (Int_Celsius'First));
                New_Line;
                Put (To_Celsius (0));
                New_Line;
                Put (To_Celsius (Int_Celsius'Last));
                New_Line;
             when Kelvin_To_Celsius =>
                Put (To_Celsius (Kelvin'First));
                New_Line;
                Put (To_Celsius (0));
                New_Line;
                Put (To_Celsius (Kelvin'Last));
                New_Line;
             when Celsius_To_Kelvin =>
                Put (To_Kelvin (Celsius'First));
                New_Line;
                Put (To_Kelvin (Celsius'Last));
                New_Line;
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

Records
-------

Directions
~~~~~~~~~~

.. code:: ada lab=Solutions.Records.Directions

    --  START LAB IO BLOCK
    in 0:Direction_Chk
    out 0:Angle:  0 => EAST. Angle:  30 => NORTHWEST. Angle:  45 => NORTHWEST. Angle:  90 => NORTH. Angle:  91 => NORTHWEST. Angle:  120 => NORTHWEST. Angle:  180 => WEST. Angle:  250 => SOUTHWEST. Angle:  270 => SOUTH.
    --  END LAB IO BLOCK

    package Directions is

       type Angle_Mod is mod 360;

       type Direction is
         (North,
          Northwest,
          West,
          Southwest,
          South,
          Southeast,
          East);

       function To_Direction (N: Angle_Mod) return Direction;

       type Ext_Angle is record
          Angle_Elem     : Angle_Mod;
          Direction_Elem : Direction;
       end record;

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle;

       procedure Display (N : Ext_Angle);

    end Directions;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Directions is

       procedure Display (N : Ext_Angle) is
       begin
          Put_Line ("Angle: "
                    & Angle_Mod'Image (N.Angle_Elem)
                    & " => "
                    & Direction'Image (N.Direction_Elem)
                    & ".");
       end Display;

       function To_Direction (N : Angle_Mod) return Direction is
       begin
          case N is
             when   0        => return East;
             when   1 ..  89 => return Northwest;
             when  90        => return North;
             when  91 .. 179 => return Northwest;
             when 180        => return West;
             when 181 .. 269 => return Southwest;
             when 270        => return South;
             when 271 .. 359 => return Southeast;
          end case;
       end To_Direction;

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle is
       begin
          return (Angle_Elem     => N,
                  Direction_Elem => To_Direction (N));
       end To_Ext_Angle;

    end Directions;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Directions;        use Directions;

    procedure Main is
       type Test_Case_Index is
         (Direction_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
          when Direction_Chk =>
             Display (To_Ext_Angle (0));
             Display (To_Ext_Angle (30));
             Display (To_Ext_Angle (45));
             Display (To_Ext_Angle (90));
             Display (To_Ext_Angle (91));
             Display (To_Ext_Angle (120));
             Display (To_Ext_Angle (180));
             Display (To_Ext_Angle (250));
             Display (To_Ext_Angle (270));
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

Colors
~~~~~~

.. code:: ada lab=Solutions.Records.Colors

    --  START LAB IO BLOCK
    in 0:HTML_Color_To_RGB
    out 0:SALMON => (Red =>     16#FA#, Green =>     16#80#, Blue =>     16#72#). FIREBRICK => (Red =>     16#B2#, Green =>     16#22#, Blue =>     16#22#). RED => (Red =>     16#FF#, Green =>      16#0#, Blue =>      16#0#). DARKRED => (Red =>     16#8B#, Green =>      16#0#, Blue =>      16#0#). LIME => (Red =>      16#0#, Green =>     16#FF#, Blue =>      16#0#). FORESTGREEN => (Red =>     16#22#, Green =>     16#8B#, Blue =>     16#22#). GREEN => (Red =>      16#0#, Green =>     16#80#, Blue =>      16#0#). DARKGREEN => (Red =>      16#0#, Green =>     16#64#, Blue =>      16#0#). BLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#FF#). MEDIUMBLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#CD#). DARKBLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#8B#).
    --  END LAB IO BLOCK

    package Color_Types is

       type HTML_Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       subtype Int_Color is Integer range 0 .. 255;

       type RGB is record
          Red   : Int_Color;
          Green : Int_Color;
          Blue  : Int_Color;
       end record;

       function To_RGB (C : HTML_Color) return RGB;

       function Image (C : RGB) return String;

    end Color_Types;

    with Ada.Integer_Text_IO;

    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          case C is
             when Salmon      => return (16#FA#, 16#80#, 16#72#);
             when Firebrick   => return (16#B2#, 16#22#, 16#22#);
             when Red         => return (16#FF#, 16#00#, 16#00#);
             when Darkred     => return (16#8B#, 16#00#, 16#00#);
             when Lime        => return (16#00#, 16#FF#, 16#00#);
             when Forestgreen => return (16#22#, 16#8B#, 16#22#);
             when Green       => return (16#00#, 16#80#, 16#00#);
             when Darkgreen   => return (16#00#, 16#64#, 16#00#);
             when Blue        => return (16#00#, 16#00#, 16#FF#);
             when Mediumblue  => return (16#00#, 16#00#, 16#CD#);
             when Darkblue    => return (16#00#, 16#00#, 16#8B#);
          end case;

       end To_RGB;

       function Image (C : RGB) return String is
          subtype Str_Range is Integer range 1 .. 10;
          SR : String (Str_Range);
          SG : String (Str_Range);
          SB : String (Str_Range);
       begin
          Ada.Integer_Text_IO.Put (To    => SR,
                                   Item  => C.Red,
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SG,
                                   Item  => C.Green,
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SB,
                                   Item  => C.Blue,
                                   Base  => 16);
          return ("(Red => " & SR
                  & ", Green => " & SG
                  & ", Blue => "  & SB
                  &")");
       end Image;

    end Color_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Color_Types;      use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (HTML_Color_To_RGB);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when HTML_Color_To_RGB =>
                for I in HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (I) & " => "
                             & Image (To_RGB (I)) & ".");
                end loop;
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

Inventory
~~~~~~~~~

.. code:: ada lab=Solutions.Records.Inventory

    --  START LAB IO BLOCK
    in 0:Inventory_Chk
    out 0:Adding item: Ballpoint Pen. Assets: $27.75. Adding item: Oil-based Pen Marker. Assets: $927.75. Adding item: Feather Quill Pen. Assets: $1007.75.
    --  END LAB IO BLOCK

    package Inventory_Pkg is

       type Item is record
          Quantity : Natural;
          Price    : Float;
       end record;

       type Inventory is record
          Assets   : Float := 0.0;
       end record;

       function Init (Name     : String;
                      Quantity : Natural;
                      Price    : Float) return Item;

       procedure Add (Inv : in out Inventory;
                      I   : Item);

       procedure Display (Inv : Inventory);

    end Inventory_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Inventory_Pkg is

       function Init (Name     : String;
                      Quantity : Natural;
                      Price    : Float) return Item is
       begin
          Put_Line ("Adding item: " & Name & ".");

          return (Quantity => Quantity,
                  Price    => Price);
       end Init;

       procedure Add (Inv : in out Inventory;
                      I   : Item) is
       begin
          Inv.Assets := Inv.Assets + Float (I.Quantity) * I.Price;
       end Add;

       procedure Display (Inv : Inventory) is
          package F_IO is new Ada.Text_IO.Float_IO (Float);

          use F_IO;
       begin
          Put ("Assets: $");
          Put (Inv.Assets, 1, 2, 0);
          Put (".");
          New_Line;
       end Display;

    end Inventory_Pkg;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Inventory_Pkg;     use Inventory_Pkg;

    procedure Main is
       --  Remark: the following line is not relevant.
       F   : array (1 .. 10) of Float := (others => 42.42);

       type Test_Case_Index is
         (Inventory_Chk);

       procedure Check (TC : Test_Case_Index) is
          I   : Item;
          Inv : Inventory;

          --  Please ignore the following three lines!
          pragma Warnings (Off, "default initialization");
          for Inv'Address use F'Address;
          pragma Warnings (On, "default initialization");
       begin
          case TC is
          when Inventory_Chk =>
             I := Init ("Ballpoint Pen",        185,  0.15);
             Add (Inv, I);
             Display (Inv);

             I := Init ("Oil-based Pen Marker", 100,  9.0);
             Add (Inv, I);
             Display (Inv);

             I := Init ("Feather Quill Pen",      2, 40.0);
             Add (Inv, I);
             Display (Inv);
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

Arrays
-----------------------

Constrained Array
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.Constrained_Array

    --  START LAB IO BLOCK
    in 0:Range_Chk
    out 0: 1  2  3  4  5  6  7  8  9  10
    in 1:Array_Range_Chk
    out 1: 1  2  3  4  5  6  7  8  9  10
    in 2:A_Obj_Chk
    out 2: 1  2  42  42  42  42  42  42  42  42
    in 3:Init_Chk
    out 3: 1  2  3  4  5  6  7  8  9  10
    in 4:Double_Chk
    out 4: 200  180  160  20  40  60  80  120  100  140
    in 5:First_Elem_Chk
    out 5: 100
    in 6:Last_Elem_Chk
    out 6: 70
    in 7:Length_Chk
    out 7: 10
    --  END LAB IO BLOCK

    package Constrained_Arrays is

       type My_Index is range 1 .. 10;

       type My_Array is array (My_Index) of Integer;

       function Init return My_Array;

       procedure Double (A : in out My_Array);

       function First_Elem (A : My_Array) return Integer;

       function Last_Elem (A : My_Array) return Integer;

       function Length (A : My_Array) return Integer;

       A : My_Array := (1, 2, others => 42);

    end Constrained_Arrays;

    package body Constrained_Arrays is

       function Init return My_Array is
          A : My_Array;
       begin
          for I in My_Array'Range loop
             A (I) := Integer (I);
          end loop;

          return A;
       end Init;

       procedure Double (A : in out My_Array) is
       begin
          for I in A'Range loop
             A (I) := A (I) * 2;
          end loop;
       end Double;

       function First_Elem (A : My_Array) return Integer is
       begin
          return A (A'First);
       end First_Elem;

       function Last_Elem (A : My_Array) return Integer is
       begin
          return A (A'Last);
       end Last_Elem;

       function Length (A : My_Array) return Integer is
       begin
          return A'Length;
       end Length;

    end Constrained_Arrays;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Constrained_Arrays; use Constrained_Arrays;

    procedure Main is
       type Test_Case_Index is
         (Range_Chk,
          Array_Range_Chk,
          A_Obj_Chk,
          Init_Chk,
          Double_Chk,
          First_Elem_Chk,
          Last_Elem_Chk,
          Length_Chk);

       procedure Check (TC : Test_Case_Index) is
          AA : My_Array;

          procedure Display (A : My_Array) is
          begin
             for I in A'Range loop
                Put_Line (Integer'Image (A (I)));
             end loop;
          end Display;

          procedure Local_Init (A : in out My_Array) is
          begin
             A := (100, 90, 80, 10, 20, 30, 40, 60, 50, 70);
          end Local_Init;
       begin
          case TC is
          when Range_Chk =>
             for I in My_Index loop
                Put_Line (My_Index'Image (I));
             end loop;
          when Array_Range_Chk =>
             for I in My_Array'Range loop
                Put_Line (My_Index'Image (I));
             end loop;
          when A_Obj_Chk =>
             Display (A);
          when Init_Chk =>
             AA := Init;
             Display (AA);
          when Double_Chk =>
             Local_Init (AA);
             Double (AA);
             Display (AA);
          when First_Elem_Chk =>
             Local_Init (AA);
             Put_Line (Integer'Image (First_Elem (AA)));
          when Last_Elem_Chk =>
             Local_Init (AA);
             Put_Line (Integer'Image (Last_Elem (AA)));
          when Length_Chk =>
             Put_Line (Integer'Image (Length (AA)));
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

Colors: Lookup-Table
~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.Colors_Lookup_Table

    --  START LAB IO BLOCK
    in 0:Color_Table_Chk
    out 0:Size of HTML_Color_RGB:  11 Firebrick: (Red =>     16#B2#, Green =>     16#22#, Blue =>     16#22#)
    in 1:HTML_Color_To_Integer_Chk
    out 1:SALMON => (Red =>     16#FA#, Green =>     16#80#, Blue =>     16#72#). FIREBRICK => (Red =>     16#B2#, Green =>     16#22#, Blue =>     16#22#). RED => (Red =>     16#FF#, Green =>      16#0#, Blue =>      16#0#). DARKRED => (Red =>     16#8B#, Green =>      16#0#, Blue =>      16#0#). LIME => (Red =>      16#0#, Green =>     16#FF#, Blue =>      16#0#). FORESTGREEN => (Red =>     16#22#, Green =>     16#8B#, Blue =>     16#22#). GREEN => (Red =>      16#0#, Green =>     16#80#, Blue =>      16#0#). DARKGREEN => (Red =>      16#0#, Green =>     16#64#, Blue =>      16#0#). BLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#FF#). MEDIUMBLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#CD#). DARKBLUE => (Red =>      16#0#, Green =>      16#0#, Blue =>     16#8B#).
    --  END LAB IO BLOCK

    package Color_Types is

       type HTML_Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       subtype Int_Color is Integer range 0 .. 255;

       type RGB is record
          Red   : Int_Color;
          Green : Int_Color;
          Blue  : Int_Color;
       end record;

       function To_RGB (C : HTML_Color) return RGB;

       function Image (C : RGB) return String;

       type HTML_Color_RGB is array (HTML_Color) of RGB;

       To_RGB_Loopup_Table : constant HTML_Color_RGB
         := (Salmon      => (16#FA#, 16#80#, 16#72#),
             Firebrick   => (16#B2#, 16#22#, 16#22#),
             Red         => (16#FF#, 16#00#, 16#00#),
             Darkred     => (16#8B#, 16#00#, 16#00#),
             Lime        => (16#00#, 16#FF#, 16#00#),
             Forestgreen => (16#22#, 16#8B#, 16#22#),
             Green       => (16#00#, 16#80#, 16#00#),
             Darkgreen   => (16#00#, 16#64#, 16#00#),
             Blue        => (16#00#, 16#00#, 16#FF#),
             Mediumblue  => (16#00#, 16#00#, 16#CD#),
             Darkblue    => (16#00#, 16#00#, 16#8B#));

    end Color_Types;

    with Ada.Integer_Text_IO;
    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          return To_RGB_Loopup_Table (C);
       end To_RGB;

       function Image (C : RGB) return String is
          subtype Str_Range is Integer range 1 .. 10;
          SR : String (Str_Range);
          SG : String (Str_Range);
          SB : String (Str_Range);
       begin
          Ada.Integer_Text_IO.Put (To    => SR,
                                   Item  => C.Red,
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SG,
                                   Item  => C.Green,
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SB,
                                   Item  => C.Blue,
                                   Base  => 16);
          return ("(Red => " & SR
                  & ", Green => " & SG
                  & ", Blue => "  & SB
                  &")");
       end Image;

    end Color_Types;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Color_Types;          use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (Color_Table_Chk,
          HTML_Color_To_Integer_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Color_Table_Chk =>
                Put_Line ("Size of HTML_Color_RGB: "
                          & Integer'Image (HTML_Color_RGB'Length));
                Put_Line ("Firebrick: "
                          & Image (To_RGB_Loopup_Table (Firebrick)));
             when HTML_Color_To_Integer_Chk =>
                for I in HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (I) & " => "
                             & Image (To_RGB (I)) & ".");
                end loop;
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

Unconstrained Array
~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.Unconstrained_Array

    --  START LAB IO BLOCK
    in 0:Init_Chk
    out 0: 5  4  3  2  1
    in 1:Init_Proc_Chk
    out 1: 5  4  3  2  1
    in 2:Double_Chk
    out 2: 2  4  10  20 -20
    in 3:Diff_Prev_Chk
    out 3: 0  1  3  5 -20
    in 4:Diff_Prev_Single_Chk
    out 4: 0
    --  END LAB IO BLOCK

    package Unconstrained_Arrays is

       type My_Array is array (Positive range <>) of Integer;

       procedure Init (A : in out My_Array);

       function Init (L : Positive) return My_Array;

       procedure Double (A : in out My_Array);

       function Diff_Prev_Elem (A : My_Array) return My_Array;

    end Unconstrained_Arrays;

    package body Unconstrained_Arrays is

       procedure Init (A : in out My_Array) is
          Y : Natural := A'Last;
       begin
          for I in A'Range loop
             A (I) := Y;
             Y := Y - 1;
          end loop;
       end Init;

       function Init (L : Positive) return My_Array is
          A : My_Array (1 .. L);
       begin
          Init (A);
          return A;
       end Init;

       procedure Double (A : in out My_Array) is
       begin
          for I in A'Range loop
             A (I) := A (I) * 2;
          end loop;
       end Double;

       function Diff_Prev_Elem (A : My_Array) return My_Array is
          A_Out : My_Array (A'Range);
       begin
          A_Out (1) := 0;
          for I in 2 .. A'Last loop
             A_Out (I) := A (I) - A (I - 1);
          end loop;

          return A_Out;
       end Diff_Prev_Elem;

    end Unconstrained_Arrays;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Unconstrained_Arrays; use Unconstrained_Arrays;

    procedure Main is
       type Test_Case_Index is
         (Init_Chk,
          Init_Proc_Chk,
          Double_Chk,
          Diff_Prev_Chk,
          Diff_Prev_Single_Chk);

       procedure Check (TC : Test_Case_Index) is
          AA : My_Array (1 .. 5);

          procedure Display (A : My_Array) is
          begin
             for I in A'Range loop
                Put_Line (Integer'Image (A (I)));
             end loop;
          end Display;

          procedure Local_Init (A : in out My_Array) is
          begin
             A := (1, 2, 5, 10, -10);
          end Local_Init;

       begin
          case TC is
          when Init_Chk =>
             AA := Init (AA'Last);
             Display (AA);
          when Init_Proc_Chk =>
             Init (AA);
             Display (AA);
          when Double_Chk =>
             Local_Init (AA);
             Double (AA);
             Display (AA);
          when Diff_Prev_Chk =>
             Local_Init (AA);
             AA := Diff_Prev_Elem (AA);
             Display (AA);
          when Diff_Prev_Single_Chk =>
             declare
                A1 : My_Array (1 .. 1) := (1 => 42);
             begin
                A1 := Diff_Prev_Elem (A1);
                Display (A1);
             end;
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

Quantities And Amounts
~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.Quantities_And_Amounts

    --  START LAB IO BLOCK
    in 0:Total_Func_Chk
    out 0:0.50 20.00 200.00 100.00 200.00
    in 1:Total_Proc_Chk
    out 1:0.50 20.00 200.00 100.00 200.00
    in 2:Total_Amount_Chk
    out 2:520.50
    --  END LAB IO BLOCK

    package Quantities_Amounts is

       subtype Quantity is Natural;

       subtype Amount is Float;

       type Quantities is array (Positive range <>) of Quantity;

       type Amounts is array (Positive range <>) of Amount;

       procedure Total (Q     : Quantities;
                        A     : Amounts;
                        A_Out : out Amounts);

       function Total (Q : Quantities;
                       A : Amounts) return Amounts;

       function Total (Q : Quantities;
                       A : Amounts) return Amount;

    end Quantities_Amounts;

    package body Quantities_Amounts is

       procedure Total (Q     : Quantities;
                        A     : Amounts;
                        A_Out : out Amounts) is
       begin
          for I in A'Range loop
             A_Out (I) := Amount (Q (I)) * A (I);
          end loop;
       end Total;

       function Total (Q : Quantities;
                       A : Amounts) return Amounts
       is
          A_Out : Amounts (A'Range);
       begin
          Total (Q, A, A_Out);
          return A_Out;
       end Total;

       function Total (Q : Quantities;
                       A : Amounts) return Amount
       is
          A_Out : Amount := 0.0;
       begin
         for I in A'Range loop
             A_Out := A_Out + Amount (Q (I)) * A (I);
          end loop;
          return A_Out;
       end Total;

    end Quantities_Amounts;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Quantities_Amounts; use Quantities_Amounts;

    procedure Main is
       package Amount_IO is new Ada.Text_IO.Float_IO (Amount);

       type Test_Case_Index is
         (Total_Func_Chk,
          Total_Proc_Chk,
          Total_Amount_Chk);

       procedure Check (TC : Test_Case_Index) is
          subtype Test_Range is Positive range 1 .. 5;

          A  : Amounts (Test_Range);
          Q  : Quantities (Test_Range);
          A1 : Amount;

          procedure Display (A : Amounts) is
          begin
             for I in A'Range loop
                Amount_IO.Put (A (I));
                New_Line;
             end loop;
          end Display;

          procedure Local_Init (Q : in out Quantities;
                                A : in out Amounts) is
          begin
             Q := (1,    2,    5,   10,   10);
             A := (0.5, 10.0, 40.0, 10.0, 20.0);
          end Local_Init;

       begin
          Amount_IO.Default_Fore := 1;
          Amount_IO.Default_Aft  := 2;
          Amount_IO.Default_Exp  := 0;

          case TC is
          when Total_Func_Chk =>
             Local_Init (Q, A);
             A := Total (Q, A);
             Display (A);
          when Total_Proc_Chk =>
             Local_Init (Q, A);
             Total (Q, A, A);
             Display (A);
          when Total_Amount_Chk =>
             Local_Init (Q, A);
             A1 := Total (Q, A);
             Amount_IO.Put (A1);
             New_Line;
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

String_10
~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.String_10

    --  START LAB IO BLOCK
    in 0:String_10_Chk
    out 0:And this i
    --  END LAB IO BLOCK

    package Strings_10 is

       subtype String_10 is String (1 .. 10);

       --  Using "type String_10 is..." is possible, too. However, it
       --  requires a custom Put_Line procedure that is called in Main:
       --  procedure Put_Line (S : String_10);

       function To_String_10 (S : String) return String_10;

    end Strings_10;

    package body Strings_10 is

       function To_String_10 (S : String) return String_10 is
          S_Out : String_10;
       begin
          for I in String_10'Range loop
             S_Out (I) := S (I);
          end loop;

          return S_Out;
       end To_String_10;

    end Strings_10;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Strings_10;         use Strings_10;

    procedure Main is
       type Test_Case_Index is
         (String_10_Chk);

       procedure Check (TC : Test_Case_Index) is
          S    : constant String := "And this is a long string just for testing...";
          S_10 : String_10;

       begin
          case TC is
          when String_10_Chk =>
             S_10 := To_String_10 (S);
             Put_Line (S_10);
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Ada.Text_IO.Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Ada.Text_IO.Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

List of Names
~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.List_Of_Names

    --  START LAB IO BLOCK
    in 0:Names_Ages_Chk
    out 0:LIST OF NAMES: NAME: John AGE:  0 NAME: Patricia AGE:  0 NAME: Josh AGE:  0 LIST OF NAMES: NAME: John AGE:  18 NAME: Patricia AGE:  35 NAME: Josh AGE:  53
    in 1:Get_Age_Chk
    out 1:Peter is  45 years old.
    --  END LAB IO BLOCK

    package Names_Ages is

       Max_People : constant Positive := 10;

       subtype Name_Type is String (1 .. 50);

       type Age_Type is new Natural;

       type Person is record
          Name  : Name_Type;
          Age   : Age_Type;
       end record;

       type People_Array is array (Positive range <>) of Person;

       type People is record
          People_A   : People_Array (1 .. Max_People);
          Last_Valid : Natural;
       end record;

       procedure Reset (P : in out People);

       procedure Add (P    : in out People;
                      Name : String);

       function Get (P    : People;
                     Name : String) return Age_Type;

       procedure Update (P    : in out People;
                         Name : String;
                         Age  : Age_Type);

       procedure Display (P : People);

    end Names_Ages;

    with Ada.Text_IO;       use Ada.Text_IO;
    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;

    package body Names_Ages is

       function To_Name_Type (S : String) return Name_Type is
          S_Out : Name_Type := (others => ' ');
       begin
          for I in 1 .. Integer'Min (S'Last, Name_Type'Last) loop
             S_Out (I) := S (I);
          end loop;

          return S_Out;
       end To_Name_Type;

       procedure Init (P    : in out Person;
                       Name :        String) is
       begin
          P.Name := To_Name_Type (Name);
          P.Age := 0;
       end Init;

       function Match (P    : Person;
                       Name : String) return Boolean is
       begin
          return P.Name = To_Name_Type (Name);
       end Match;

       function Get (P : Person) return Age_Type is
       begin
          return P.Age;
       end Get;

       procedure Update (P   : in out Person;
                         Age :        Age_Type) is
       begin
          P.Age := Age;
       end Update;

       procedure Display (P : Person) is
       begin
          Put_Line ("NAME: " & Trim (P.Name, Right));
          Put_Line ("AGE: "  & Age_Type'Image (P.Age));
       end Display;

       procedure Reset (P : in out People) is
       begin
          P.Last_Valid := 0;
       end Reset;

       procedure Add (P    : in out People;
                      Name :        String) is
       begin
          P.Last_Valid := P.Last_Valid + 1;
          Init (P.People_A (P.Last_Valid), Name);
       end Add;

       function Get (P    : People;
                     Name : String) return Age_Type is
       begin
          for I in P.People_A'First .. P.Last_Valid loop
             if Match (P.People_A (I), Name) then
                return Get (P.People_A (I));
             end if;
          end loop;

          return 0;
       end Get;

       procedure Update (P    : in out People;
                         Name :        String;
                         Age  :        Age_Type) is
       begin
          for I in P.People_A'First .. P.Last_Valid loop
             if Match (P.People_A (I), Name) then
                Update (P.People_A (I), Age);
             end if;
          end loop;
       end Update;

       procedure Display (P : People) is
       begin
          Put_Line ("LIST OF NAMES:");
          for I in P.People_A'First .. P.Last_Valid loop
             Display (P.People_A (I));
          end loop;
       end Display;

    end Names_Ages;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Names_Ages;         use Names_Ages;

    procedure Main is
       type Test_Case_Index is
         (Names_Ages_Chk,
          Get_Age_Chk);

       procedure Check (TC : Test_Case_Index) is
          P : People;
       begin
          case TC is
          when Names_Ages_Chk =>
             Reset (P);
             Add (P, "John");
             Add (P, "Patricia");
             Add (P, "Josh");
             Display (P);
             Update (P, "John",     18);
             Update (P, "Patricia", 35);
             Update (P, "Josh",     53);
             Display (P);
          when Get_Age_Chk =>
             Reset (P);
             Add (P, "Peter");
             Update (P, "Peter", 45);
             Put_Line ("Peter is "
                       & Age_Type'Image (Get (P, "Peter"))
                       & " years old.");
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Ada.Text_IO.Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Ada.Text_IO.Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

