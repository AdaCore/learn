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

States
~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_1

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off
    in 1:1
    out 1:On: Simple Processing
    in 2:2
    out 2:On: Advanced Processing
    --  END LAB IO BLOCK

    procedure Display_State (State : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_State (State : Integer) is
    begin
       case State is
          when 0 =>
             Put_Line ("Off");
          when 1 =>
             Put_Line ("On: Simple Processing");
          when 2 =>
             Put_Line ("On: Advanced Processing");
          when others =>
             null;
       end case;
    end Display_State;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_State;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Display_State (State);
    end Main;

States #2
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_2

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off
    in 1:1
    out 1:On: Simple Processing
    in 2:2
    out 2:On: Advanced Processing
    --  END LAB IO BLOCK

    function Get_State (State : Integer) return String;

    function Get_State (State : Integer) return String is
    begin
       return (case State is
               when 0 => "Off",
               when 1 => "On: Simple Processing",
               when 2 => "On: Advanced Processing",
               when others => "");
    end Get_State;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Get_State;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Put_Line (Get_State (State));
    end Main;

States #3
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_3

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off FALSE
    in 1:1
    out 1:On TRUE
    in 2:2
    out 2:On TRUE
    --  END LAB IO BLOCK

    function Is_On (State : Integer) return Boolean;

    function Is_On (State : Integer) return Boolean is
    begin
       return (if State = 0 then False else True);
    end Is_On;

    procedure Display_On_Off (State : Integer);

    with Ada.Text_IO; use Ada.Text_IO;
    with Is_On;

    procedure Display_On_Off (State : Integer) is
    begin
       Put_Line (if Is_On (State) then "On" else "Off");
    end Display_On_Off;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_On_Off;
    with Is_On;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Display_On_Off (State);
       Put_Line (Boolean'Image (Is_On (State)));
    end Main;

States #4
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_4

    --  START LAB IO BLOCK
    in 0:0
    out 0: 1
    in 1:1
    out 1: 2
    in 2:2
    out 2: 0
    --  END LAB IO BLOCK

    procedure Set_Next (State : in out Integer);

    procedure Set_Next (State : in out Integer) is
    begin
       State := (if State < 2 then State + 1 else 0);
    end Set_Next;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Set_Next;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Set_Next (State);
       Put_Line (Integer'Image (State));
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

More About Types
----------------

Aggregate Initialization
~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Aggregate_Initialization

    --  START LAB IO BLOCK
    in 0:Default_Rec_Chk
    out 0:Record Default: W =>  10 X =>  11 Y =>  12 Z =>  13
    in 1:Init_Rec_Chk
    out 1:Record Init: W =>  10 X =>  100 Y =>  200 Z =>  13
    in 2:Init_Some_Arr_Chk
    out 2:Array Init_Some:  1  99  2  99  3  99  4  99  5  99  6  100  7  100  8  100  9  100  10  100  11  100  12  100  13  100  14  100  15  100  16  100  17  100  18  100  19  100  20  100
    in 3:Init_Arr_Chk
    out 3:Array Init:  1  5  2  5  3  5  4  5  5  5  6  5  7  5  8  5  9  5  10  5  11  5  12  5  13  5  14  5  15  5  16  5  17  5  18  5  19  5  20  5
    --  END LAB IO BLOCK

    package Aggregates is

       type Rec is record
          W : Integer := 10;
          X : Integer := 11;
          Y : Integer := 12;
          Z : Integer := 13;
       end record;

       type Int_Arr is array (1 .. 20) of Integer;

       procedure Init (R : out Rec);

       procedure Init_Some (A : out Int_Arr);

       procedure Init (A : out Int_Arr);

    end Aggregates;

    package body Aggregates is

       procedure Init (R : out Rec) is
       begin
          R := (X      => 100,
                Y      => 200,
                others => <>);
       end Init;

       procedure Init_Some (A : out Int_Arr) is
       begin
          A := (1 .. 5 => 99,
                others => 100);
       end Init_Some;

       procedure Init (A : out Int_Arr) is
       begin
          A := (others => 5);
       end Init;

    end Aggregates;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Aggregates;        use Aggregates;

    procedure Main is
       --  Remark: the following line is not relevant.
       F   : array (1 .. 10) of Float := (others => 42.42)
         with Unreferenced;

       type Test_Case_Index is
         (Default_Rec_Chk,
          Init_Rec_Chk,
          Init_Some_Arr_Chk,
          Init_Arr_Chk);

       procedure Check (TC : Test_Case_Index) is
          A : Int_Arr;
          R : Rec;
          DR : constant Rec := (others => <>);
       begin
          case TC is
             when Default_Rec_Chk =>
                R := DR;
                Put_Line ("Record Default:");
                Put_Line ("W => " & Integer'Image (R.W));
                Put_Line ("X => " & Integer'Image (R.X));
                Put_Line ("Y => " & Integer'Image (R.Y));
                Put_Line ("Z => " & Integer'Image (R.Z));
             when Init_Rec_Chk =>
                Init (R);
                Put_Line ("Record Init:");
                Put_Line ("W => " & Integer'Image (R.W));
                Put_Line ("X => " & Integer'Image (R.X));
                Put_Line ("Y => " & Integer'Image (R.Y));
                Put_Line ("Z => " & Integer'Image (R.Z));
             when Init_Some_Arr_Chk =>
                Init_Some (A);
                Put_Line ("Array Init_Some:");
                for I in A'Range loop
                   Put_Line (Integer'Image (I) & " "
                             & Integer'Image (A (I)));
                end loop;
             when Init_Arr_Chk =>
                Init (A);
                Put_Line ("Array Init:");
                for I in A'Range loop
                   Put_Line (Integer'Image (I) & " "
                             & Integer'Image (A (I)));
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

Versioning
~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Versioning

    --  START LAB IO BLOCK
    in 0:Ver_String_Chk
    out 0:1.3.23
    in 1:Ver_Float_Chk
    out 1: 1.30000E+00
    --  END LAB IO BLOCK

    package Versioning is

       type Version is record
          Major       : Natural;
          Minor       : Natural;
          Maintenance : Natural;
       end record;

       function Convert (V : Version) return String;

       function Convert (V : Version) return Float;

    end Versioning;

    with Ada.Strings; use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;

    package body Versioning is

       function Image_Trim (N : Natural) return String is
          S_N : constant String := Trim (Natural'Image (N), Left);
       begin
          return S_N;
       end Image_Trim;

       function Convert (V : Version) return String is
          S_Major : constant String := Image_Trim (V.Major);
          S_Minor : constant String := Image_Trim (V.Minor);
          S_Maint : constant String := Image_Trim (V.Maintenance);
       begin
          return (S_Major & "." & S_Minor & "." & S_Maint);
       end Convert;

       function Convert (V : Version) return Float is
       begin
          return Float (V.Major) + (Float (V.Minor) / 10.0);
       end Convert;

    end Versioning;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Versioning;        use Versioning;

    procedure Main is
       type Test_Case_Index is
         (Ver_String_Chk,
          Ver_Float_Chk);

       procedure Check (TC : Test_Case_Index) is
          V : constant Version := (1, 3, 23);
       begin
          case TC is
             when Ver_String_Chk =>
                Put_Line (Convert (V));
             when Ver_Float_Chk =>
                Put_Line (Float'Image (Convert (V)));
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

Simple todo list
~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Simple_Todo_List

    --  START LAB IO BLOCK
    in 0:Todo_List_Chk
    out 0:ERROR: list is full! TO-DO LIST Buy milk Buy tea Buy present Buy tickets Pay electricity bill Schedule dentist appointment Call sister Revise spreasheet Edit entry page Select new design
    --  END LAB IO BLOCK

    package Todo_Lists is

       type Todo_Item is access String;

       type Todo_List is array (Positive range <>) of Todo_Item;

       Last : Natural := 0;

       procedure Add (Todos : in out Todo_List;
                      Item  : String);

       procedure Display (Todos : Todo_List);

    end Todo_Lists;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Todo_Lists is

       procedure Add (Todos : in out Todo_List;
                      Item  : String) is
       begin
          if Last < Todos'Last then
             Last := Last + 1;
             Todos (Last) := new String'(Item);
          else
             Put_Line ("ERROR: list is full!");
          end if;
       end Add;

       procedure Display (Todos : Todo_List) is
       begin
          Put_Line ("TO-DO LIST");
          for I in Todos'First .. Last loop
             Put_Line (Todos (I).all);
          end loop;
       end Display;

    end Todo_Lists;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Todo_Lists;        use Todo_Lists;

    procedure Main is
       type Test_Case_Index is
         (Todo_List_Chk);

       procedure Check (TC : Test_Case_Index) is
          T : Todo_List (1 .. 10);
       begin
          case TC is
             when Todo_List_Chk =>
                Add (T, "Buy milk");
                Add (T, "Buy tea");
                Add (T, "Buy present");
                Add (T, "Buy tickets");
                Add (T, "Pay electricity bill");
                Add (T, "Schedule dentist appointment");
                Add (T, "Call sister");
                Add (T, "Revise spreasheet");
                Add (T, "Edit entry page");
                Add (T, "Select new design");
                Add (T, "Create upgrade plan");
                Display (T);
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

Price list
~~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Price_List

    --  START LAB IO BLOCK
    in 0:Price_Type_Chk
    out 0:The delta    value of Price_Type is  0.01; The minimum  value of Price_Type is -9999999999.99; The maximum  value of Price_Type is  9999999999.99;
    in 1:Price_List_Chk
    out 1:PRICE LIST  1.45  2.37  3.21  4.14  5.22  6.69  7.77  8.14  9.99  10.01
    in 2:Price_List_Get_Chk
    out 2:Attemp Get #  5 Element #  5 =>  5.22 Attemp Get #  40 Element not available (as expected)
    --  END LAB IO BLOCK

    package Price_Lists is

       type Price_Type is delta 10.0 ** (-2) digits 12;

       type Price_List_Array is array (Positive range <>) of Price_Type;

       type Price_List (Max : Positive) is record
          List : Price_List_Array (1 .. Max);
          Last : Natural := 0;
       end record;

       type Price_Result (Ok : Boolean) is record
          case Ok is
             when False =>
                null;
             when True =>
                Price : Price_Type;
          end case;
       end record;

       procedure Reset (Prices : in out Price_List);

       procedure Add (Prices : in out Price_List;
                      Item   : Price_Type);

       function Get (Prices : Price_List;
                     Idx    : Positive) return Price_Result;

       procedure Display (Prices : Price_List);

    end Price_Lists;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Price_Lists is

       procedure Reset (Prices : in out Price_List) is
       begin
          Prices.Last := 0;
       end Reset;

       procedure Add (Prices : in out Price_List;
                      Item   : Price_Type) is
       begin
          if Prices.Last < Prices.List'Last then
             Prices.Last := Prices.Last + 1;
             Prices.List (Prices.Last) := Item;
          else
             Put_Line ("ERROR: list is full!");
          end if;
       end Add;

       function Get (Prices : Price_List;
                     Idx    : Positive) return Price_Result is
       begin
          if (Idx >= Prices.List'First and then
              Idx <= Prices.Last)          then
             return Price_Result'(Ok    => True,
                                  Price => Prices.List (Idx));
          else
             return Price_Result'(Ok    => False);
          end if;
       end Get;

       procedure Display (Prices : Price_List) is
       begin
          Put_Line ("PRICE LIST");
          for I in Prices.List'First .. Prices.Last loop
             Put_Line (Price_Type'Image (Prices.List (I)));
          end loop;
       end Display;

    end Price_Lists;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Price_Lists;       use Price_Lists;

    procedure Main is
       type Test_Case_Index is
         (Price_Type_Chk,
          Price_List_Chk,
          Price_List_Get_Chk);

       procedure Check (TC : Test_Case_Index) is
          L : Price_List (10);

          procedure Local_Init_List is
          begin
             Reset (L);
             Add (L, 1.45);
             Add (L, 2.37);
             Add (L, 3.21);
             Add (L, 4.14);
             Add (L, 5.22);
             Add (L, 6.69);
             Add (L, 7.77);
             Add (L, 8.14);
             Add (L, 9.99);
             Add (L, 10.01);
          end Local_Init_List;

          procedure Get_Display (Idx : Positive) is
             R : constant Price_Result := Get (L, Idx);
          begin
             Put_Line ("Attemp Get # " & Positive'Image (Idx));
             if R.Ok then
                Put_Line ("Element # " & Positive'Image (Idx)
                          & " => "     & Price_Type'Image (R.Price));
             else
                declare
                begin
                   Put_Line ("Element # " & Positive'Image (Idx)
                             & " => "     & Price_Type'Image (R.Price));
                exception
                   when others =>
                      Put_Line ("Element not available (as expected)");
                end;
             end if;

          end Get_Display;

       begin
          case TC is
             when Price_Type_Chk =>
                Put_Line ("The delta    value of Price_Type is "
                          & Price_Type'Image (Price_Type'Delta) & ";");
                Put_Line ("The minimum  value of Price_Type is "
                          & Price_Type'Image (Price_Type'First) & ";");
                Put_Line ("The maximum  value of Price_Type is "
                          & Price_Type'Image (Price_Type'Last)  & ";");
             when Price_List_Chk =>
                Local_Init_List;
                Display (L);
             when Price_List_Get_Chk =>
                Local_Init_List;
                Get_Display (5);
                Get_Display (40);
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

Fixed-point square-root
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Fixed_Point_Sqrt

    --  START LAB IO BLOCK
    in 0:TESTCASE Sqrt_Chk_Last_Div_8
    out 0:Float-Sqrt of 4096.00000 = 64.00000 Fixed-Sqrt of  4095.99998 =  63.99998
    in 1:VALUE 8.0
    out 1:Float-Sqrt of 8.00000 = 2.82843 Fixed-Sqrt of  8.00000 =  2.82841
    in 2:VALUE 4.0
    out 2:Float-Sqrt of 4.00000 = 2.00000 Fixed-Sqrt of  4.00000 =  2.00000
    in 3:VALUE 2.0
    out 3:Float-Sqrt of 2.00000 = 1.41421 Fixed-Sqrt of  2.00000 =  1.41420
    in 4:VALUE 1.0
    out 4:Float-Sqrt of 1.00000 = 1.00000 Fixed-Sqrt of  1.00000 =  1.00000
    in 5:VALUE 0.5
    out 5:Float-Sqrt of 0.50000 = 0.70711 Fixed-Sqrt of  0.50000 =  0.70709
    in 6:VALUE 0.125
    out 6:Float-Sqrt of 0.12500 = 0.35355 Fixed-Sqrt of  0.12500 =  0.35355
    in 7:VALUE 0.001
    out 7:Float-Sqrt of 0.00101 = 0.03173 Fixed-Sqrt of  0.00101 =  0.03172
    --  END LAB IO BLOCK

    package Fixed_Point_Ops is

    --     F_Size    : constant := 16;
    --     F_Size    : constant := 24;
       F_Size    : constant := 32;
    --     F_Size    : constant := 48;
    --     F_Size    : constant := 64;

       -- Definition for Q<F_Size / 2 - 1>.<F_Size / 2>, e.g. Q15.16:

       F_Size_Fract : constant := F_Size / 2;
       F_Size_Sign  : constant := 1;
       F_Size_Int   : constant := F_Size - F_Size_Fract - F_Size_Sign;
       F_Delta      : constant := 2.0 ** (-F_Size_Fract);
       F_Last       : constant := 2.0 ** ( F_Size_Int);

       -- Definition for Q<F_Size / 2 - 1>.<F_Size / 2>, e.g. Q16.16:

    --     F_Size_Fract : constant := F_Size / 2;
    --     F_Size_Sign  : constant := 0;
    --     F_Size_Int   : constant := F_Size - F_Size_Fract - F_Size_Sign;
    --     F_Delta      : constant := 2.0 ** (-F_Size_Fract);
    --     F_Last       : constant := 2.0 ** ( F_Size_Int);

       -- Definition for Q<F_Size * 1/4 - 1>.<F_Size * 3/4>, e.g. Q7.24:

    --     F_Size_Fract : constant := F_Size * 3 / 4;
    --     F_Size_Sign  : constant := 1;
    --     F_Size_Int   : constant := F_Size - F_Size_Fract - F_Size_Sign;
    --     F_Delta      : constant := 2.0 ** (-F_Size_Fract);
    --     F_Last       : constant := 2.0 ** ( F_Size_Int);

       -- Definition for Q<F_Size * 1/4>.<F_Size * 3/4>, e.g. Q8.24
       -- (unsigned!):

    --     F_Size_Fract : constant := F_Size * 3 / 4;
    --     F_Size_Sign  : constant := 0;
    --     F_Size_Int   : constant := F_Size - F_Size_Fract - F_Size_Sign;
    --     F_Delta      : constant := 2.0 ** (-F_Size_Fract);
    --     F_Last       : constant := 2.0 ** ( F_Size_Int);

       type Fixed is delta F_Delta range 0.0 .. F_Last - F_Delta;

       function Sqrt (V : Fixed) return Fixed;

    end Fixed_Point_Ops;

    package body Fixed_Point_Ops is

       --
       --  Algorithm and code Author: Christophe Meessen 1993.
       --  Initially published in :
       --    usenet comp.lang.c, Thu, 28 Jan 1993 08:35:23 GMT.
       --
       --  https://github.com/chmike/fpsqrt/blob/master/fpsqrt.c
       --

       function Sqrt (V : Fixed) return Fixed
       is
          T, Q, B, R : Fixed;

          Shift_Fac  : constant := F_Size_Int + F_Size_Sign;

          B_Init     : constant := 2.0 ** (Shift_Fac - 2);
          --  Equivalent to:
          --      2#100_0000_0000_0000.0000_0000_0000_0000#;
          --     16#4000.0000#;

          B_Thres    : constant := 2.0 ** (-(F_Size_Fract - 2) + 4);
          --  Equivalent to:
          --      2#000_0000_0000_0000.0000_0000_0000_0100#;
          --     16#0000.0040#;
       begin
          R := V;
          B := B_Init;

          Q := 0.0;
          while B > B_Thres loop
             T := Q + B;
             if R >= T then
                R := R - T;
                Q := T + B;
             end if;
             R := R * 2;
             B := B / 2;
          end loop;
          Q := Q / 2 ** ((Shift_Fac) / 2);

          return Q;
       end Sqrt;

    end Fixed_Point_Ops;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

    with Fixed_Point_Ops;   use Fixed_Point_Ops;

    procedure Main is
       type Test_Case_Index is
         (Sqrt_Chk_Last_Div_2,
          Sqrt_Chk_Last_Div_2_Minus,
          Sqrt_Chk_Last_Div_4,
          Sqrt_Chk_Last_Div_8);

       procedure Display_Sqrt (V : Fixed) is
          package Float_IO is new Ada.Text_IO.Float_IO (Float);

          F : constant Float := Float (V);
       begin
          Put ("Float-Sqrt of ");
          Float_IO.Put (F,
                        Fore => 1, Aft => 5, Exp => 0);
          Put (" = ");
          Float_IO.Put (Sqrt (F),
                        Fore => 1, Aft => 5, Exp => 0);
          New_Line;
          Put_Line ("Fixed-Sqrt of "
                    & Fixed'Image (V)
                    & " = "
                    & Fixed'Image (Sqrt (V)));
       end Display_Sqrt;

       procedure Check (TC : Test_Case_Index) is

       begin
          case TC is
          when Sqrt_Chk_Last_Div_2_Minus =>
             Display_Sqrt (Fixed'Last / 2 - Fixed'Delta * Fixed'Size);
          when Sqrt_Chk_Last_Div_2 =>
             Display_Sqrt (Fixed'Last / 2);
          when Sqrt_Chk_Last_Div_4 =>
             Display_Sqrt (Fixed'Last / 4);
          when Sqrt_Chk_Last_Div_8 =>
             Display_Sqrt (Fixed'Last / 8);
          end case;
       exception
          when others =>
             Put_Line ("Exception!");
       end Check;

    begin
       if Argument_Count < 2 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 2 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       if Argument (1) = "TESTCASE" then
          Check (Test_Case_Index'Value (Argument (2)));
       elsif Argument (1) = "VALUE" then
          Display_Sqrt (Fixed'Value (Argument (2)));
       end if;

    end Main;

Inventory
~~~~~~~~~

.. code:: ada lab=Solutions.More_About_Types.Inventory

    --  START LAB IO BLOCK
    in 0:Inventory_Chk
    out 0:==== ITEM #  1: Ballpoint Pen == BOUGHT Quantity:  10 Amount:    1.50 == SOLD Quantity:  4 Amount:    0.60 == IN STOCK Quantity:  6 Amount:    0.90  ==== ITEM #  2: Oil-based Pen Marker == BOUGHT Quantity:  20 Amount:    180.00 == SOLD Quantity:  0 Amount:    0.00 == IN STOCK Quantity:  20 Amount:    180.00  ==== ITEM #  3: Feather Quill Pen == BOUGHT Quantity:  50 Amount:    750.00 == SOLD Quantity:  20 Amount:    300.00 == IN STOCK Quantity:  30 Amount:    450.00  ==== OVERALL Amount bought:    931.50 Amount sold:      300.60 Amount in stock:  450.00
    in 1:Inventory_Range_Chk
    out 1:Info: Call to 'Add' failed as expected. Info: Call to 'Set' failed as expected.
    --  END LAB IO BLOCK

    package Inventory_Pkg is

       subtype Item_Quantity is Natural;

       type Amount is delta 10.0 ** (-2) digits 12;

       type Name_Type is access String;

       subtype Item_ID is Positive;

       type Transaction_Type is (Bought, Sold);

       type Transaction_Quantities is array (Transaction_Type) of Item_Quantity;

       type Transaction_Amounts is array (Transaction_Type) of Amount;

       type Add_Status (Success : Boolean := False) is record
          case Success is
             when False =>
                null;
             when True =>
                ID : Item_ID;
          end case;
       end record;

       type Item is record
          Name            : Name_Type;
          Price           : Amount;
          Stock_Quantity  : Item_Quantity;
          Stock_Amount    : Amount;
          Trans_Quantity  : Transaction_Quantities;
          Trans_Amount    : Transaction_Amounts;
       end record;

       type Items is array (Item_ID range <>) of Item;

       type Inventory (Max : Item_ID) is record
          List_Item    : Items (1 .. Max);
          Last_Item_Id : Natural := 0;
       end record;

       function Init (Name  : String;
                      Price : Amount) return Item;

       procedure Init (Inv : in out Inventory);

       procedure Add (Inv     : in out Inventory;
                      I       :        Item;
                      Status  : out    Add_Status);

       function Get (Inv       : Inventory;
                     Item_Name : String) return Item_ID;

       function Last_Id (Inv : Inventory) return Natural;

       procedure Set (Inv      : in out Inventory;
                      Trans    :        Transaction_Type;
                      ID       :        Item_ID;
                      Quantity :        Positive;
                      Success  :    out Boolean);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return String;
       --  Retrieve item name
       --
       --  Item_Name : String := Get (Inv, ID);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Item_Quantity;
       --  Retrieve number of units in stock for specified item
       --
       --  Number_Units_In_Stock_For_Item : Item_Quantity := Get (Inv, ID);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Amount;
       --  Retrieve total amount in stock for specified item
       --
       --  Potential_Income_For_Units_In_Stock_For_Item : Amount := Get (Inv, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Item_Quantity;
       --  Retrieve number of units for specified item and transaction type
       --
       --  Number_Units_Sold_For_Item : Item_Quantity := Get (Inv, Sold, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Amount;
       --  Retrieve amount for specified item and transaction type
       --
       --  Income_For_Sold_Units_Of_Item : Amount := Get (Inv, Sold, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type) return Amount;
       --  Retrieve amount for transaction type
       --
       --  Income_For_All_Sold_Units : Amount := Get (Inv, Sold);

       function Get (Inv   : Inventory) return Amount;
       --  Retrieve amount for whole inventory
       --
       --  Income_For_All_Units_In_Stock : Amount := Get (Inv);

       procedure Display (Inv : Inventory);

    end Inventory_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Inventory_Pkg is

       function Init (Name  : String;
                      Price : Amount) return Item is
       begin
          return Item'(Name           => new String'(Name),
                       Price          => Price,
                       Stock_Quantity => 0,
                       Stock_Amount   => 0.0,
                       Trans_Quantity => (others => 0),
                       Trans_Amount   => (others => 0.0));
       end Init;

       procedure Init (Inv : in out Inventory) is
       begin
          Inv.Last_Item_Id := Item_ID'First;
       end Init;

       procedure Add (Inv     : in out Inventory;
                      I       :        Item;
                      Status  : out    Add_Status)
       is
          L : Natural := Inv.Last_Item_Id;
       begin
          if L < Inv.Max then
             L := L + 1;
             Inv.Last_Item_Id  := L;
             Inv.List_Item (L) := I;

             Status := (Success => True,
                        ID      => L);
          else
             Status := (Success => False);
          end if;
       end Add;

       function Get (Inv       : Inventory;
                     Item_Name : String) return Item_ID is
          ID : Item_ID := Item_ID'First;
       begin
          for I in Inv.List_Item'First .. Inv.Last_Item_Id loop
             if Inv.List_Item (I).Name.all = Item_Name then
                ID := I;
                exit;
             end if;
          end loop;

          return ID;
       end Get;

       function Last_Id (Inv : Inventory) return Natural is (Inv.Last_Item_Id);

       procedure Set (Inv      : in out Inventory;
                      Trans    :        Transaction_Type;
                      ID       :        Item_ID;
                      Quantity :        Positive;
                      Success  :    out Boolean)
       is
          Q : Integer;
       begin
          case Trans is
             when Bought =>
                Q := Inv.List_Item (ID).Stock_Quantity + Quantity;
             when Sold =>
                Q := Inv.List_Item (ID).Stock_Quantity - Quantity;
          end case;

          if Q >= 0 then
             Success := True;

             Inv.List_Item (ID).Stock_Quantity := Q;

             Inv.List_Item (ID).Stock_Amount :=
               Amount (Q) * Inv.List_Item (ID).Price;

             Inv.List_Item (ID).Trans_Quantity (Trans) :=
               Inv.List_Item (ID).Trans_Quantity (Trans) + Quantity;

             Inv.List_Item (ID).Trans_Amount (Trans) :=
               Inv.List_Item (ID).Trans_Amount (Trans) +
               Amount (Quantity) * Inv.List_Item (ID).Price;
          else
             Success := False;
          end if;

       end Set;

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return String is
          (Inv.List_Item (ID).Name.all);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Item_Quantity is
         (Inv.List_Item (ID).Stock_Quantity);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Amount is
         (Inv.List_Item (ID).Stock_Amount);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Item_Quantity is
         (Inv.List_Item (ID).Trans_Quantity (Trans));

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Amount is
         (Inv.List_Item (ID).Trans_Amount (Trans));

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type) return Amount
       is
          Total : Amount := 0.0;
       begin
          for I in Inv.List_Item'First .. Last_Id (Inv) loop
             Total := Total + Get (Inv, Trans, I);
          end loop;

          return Total;
       end Get;

       function Get (Inv   : Inventory) return Amount
       is
          Total : Amount := 0.0;
       begin
          for I in Inv.List_Item'First .. Last_Id (Inv) loop
             Total :=  + Get (Inv, I);
          end loop;

          return Total;
       end Get;

       procedure Display (Inv : Inventory)
       is
          package F_IO is new Ada.Text_IO.Decimal_IO (Amount);

          use F_IO;
       begin
          for I in Inv.List_Item'First .. Last_Id (Inv) loop
             Put_Line ("==== ITEM # " & Positive'Image (I)
                       & ": " & Get (Inv, I));
             for Trans in Transaction_Type loop
                Put_Line ("== " & Transaction_Type'Image (Trans));
                Put_Line ("Quantity: "
                          & Item_Quantity'Image (Get (Inv, Trans, I)));
                Put ("Amount:    ");
                Put (Amount'(Get (Inv, Trans, I)), 1, 2, 0);
                New_Line;
             end loop;
             Put_Line ("== IN STOCK");
             Put_Line ("Quantity: " & Item_Quantity'Image (Get (Inv, I)));
             Put ("Amount:    ");
             Put (Amount'(Get (Inv, I)), 1, 2, 0);
             New_Line;
             New_Line;
          end loop;
          Put_Line ("==== OVERALL");
          Put ("Amount bought:    ");
          Put (Amount'(Get (Inv, Bought)), 1, 2, 0);
          New_Line;
          Put ("Amount sold:      ");
          Put (Amount'(Get (Inv, Sold)), 1, 2, 0);
          New_Line;
          Put ("Amount in stock:  ");
          Put (Amount'(Get (Inv)), 1, 2, 0);
          New_Line;
       end Display;

    end Inventory_Pkg;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Inventory_Pkg;     use Inventory_Pkg;

    procedure Main is
       --  Remark: the following line is not relevant.
       F   : array (1 .. 200) of Float := (others => 42.42);

       type Test_Case_Index is
         (Inventory_Chk,
          Inventory_Range_Chk);

       procedure Check (TC : Test_Case_Index) is
          Inv     : Inventory (3);
          Success : Boolean;
          Status  : Add_Status;

          --  Please ignore the following three lines!
          pragma Warnings (Off, "default initialization");
          for Inv'Address use F'Address;
          pragma Warnings (On, "default initialization");

          procedure Init_Check_Data is
          begin
             Add (Inv,
                  Init ("Ballpoint Pen", 0.15),
                  Status);

             if Status.Success then
                Set (Inv      => Inv,
                     Trans    => Bought,
                     ID       => Status.ID,
                     Quantity => 10,
                     Success  => Success);

                Set (Inv      => Inv,
                     Trans    => Sold,
                     ID       => Status.ID,
                     Quantity => 2,
                     Success  => Success);

                Set (Inv      => Inv,
                     Trans    => Sold,
                     ID       => Status.ID,
                     Quantity => 2,
                     Success  => Success);
             end if;

             Add (Inv,
                  Init ("Oil-based Pen Marker", 9.0),
                  Status);

             Add (Inv,
                  Init ("Feather Quill Pen", 15.0),
                  Status);

             Set (Inv      => Inv,
                  Trans    => Bought,
                  ID       => Get (Inv, "Oil-based Pen Marker"),
                  Quantity => 20,
                  Success  => Success);

             Set (Inv      => Inv,
                  Trans    => Bought,
                  ID       => Get (Inv, "Feather Quill Pen"),
                  Quantity => 50,
                  Success  => Success);

             Set (Inv      => Inv,
                  Trans    => Sold,
                  ID       => Get (Inv, "Feather Quill Pen"),
                  Quantity => 20,
                  Success  => Success);
          end Init_Check_Data;

          procedure Check_Expected_Failure (Success   : Boolean;
                                            Proc_Name : String) is
          begin
             if Success then
                Put_Line ("ERROR: Call to '" & Proc_Name & "' should have failed.");
             else
                Put_Line ("Info: Call to '" & Proc_Name & "' failed as expected.");
             end if;
          end Check_Expected_Failure;

       begin
          Init_Check_Data;

          case TC is
          when Inventory_Chk =>
             Display (Inv);
          when Inventory_Range_Chk =>
             --  Inventory is full; try to add another item
             Add (Inv,
                  Init ("Ballpoint Pen", 0.15),
                  Status);
             Check_Expected_Failure (Status.Success, "Add");

             --  Try to sell more than available in stock
             Set (Inv      => Inv,
                  Trans    => Sold,
                  ID       => Get (Inv, "Oil-based Pen Marker"),
                  Quantity => 30,
                  Success  => Success);
             Check_Expected_Failure (Success, "Set");
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

Privacy
-------

Directions
~~~~~~~~~~

.. code:: ada lab=Solutions.Privacy.Directions

    --  START LAB IO BLOCK
    in 0:Direction_Chk
    out 0:Angle:  0 => EAST. Angle:  45 => NORTHWEST. Angle:  90 => NORTH. Angle:  91 => NORTHWEST. Angle:  180 => WEST. Angle:  270 => SOUTH.
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

       function To_Direction (N : Angle_Mod) return Direction;

       type Ext_Angle is private;

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle;

       procedure Display (N : Ext_Angle);

    private

       type Ext_Angle is record
          Angle_Elem     : Angle_Mod;
          Direction_Elem : Direction;
       end record;

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

    with Directions; use Directions;

    procedure Test_Directions is
       type Ext_Angle_Array is array (Positive range <>) of Ext_Angle;

       All_Directions : constant Ext_Angle_Array (1 .. 6)
         := (To_Ext_Angle (0),
             To_Ext_Angle (45),
             To_Ext_Angle (90),
             To_Ext_Angle (91),
             To_Ext_Angle (180),
             To_Ext_Angle (270));

    begin
       for I in All_Directions'Range loop
          Display (All_Directions (I));
       end loop;

    end Test_Directions;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Test_Directions;

    procedure Main is
         type Test_Case_Index is
         (Direction_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
          when Direction_Chk =>
             Test_Directions;
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

Limited Strings
~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Privacy.Limited_Strings

    --  START LAB IO BLOCK
    in 0:Lim_String_Chk
    out 0:S1 => Hello World S2 => ______________________________ S1 isn't equal to S2. S3 => Hello S1 is equal to S3. S4 => Hello World___________________ S1 is equal to S4.
    --  END LAB IO BLOCK

    package Limited_Strings is

       type Lim_String is limited private;

       function Init (S : String) return Lim_String;

       function Init (Max : Positive) return Lim_String;

       procedure Put_Line (LS : Lim_String);

       procedure Copy (From :        Lim_String;
                       To   : in out Lim_String);

       function "=" (Ref, Dut : Lim_String) return Boolean;

    private

       type Lim_String is access String;

    end Limited_Strings;

    with Ada.Text_IO;

    package body Limited_Strings
    is

       function Init (S : String) return Lim_String is
          LS : constant Lim_String := new String'(S);
       begin
          return Ls;
       end Init;

       function Init (Max : Positive) return Lim_String is
          LS : constant Lim_String := new String (1 .. Max);
       begin
          LS.all := (others => '_');
          return LS;
       end Init;

       procedure Put_Line (LS : Lim_String) is
       begin
          Ada.Text_IO.Put_Line (LS.all);
       end Put_Line;

       function Get_Min_Last (A, B : Lim_String) return Positive is
       begin
          return Positive'Min (A'Last, B'Last);
       end Get_Min_Last;

       procedure Copy (From :        Lim_String;
                       To   : in out Lim_String) is
          Min_Last : constant Positive := Get_Min_Last (From, To);
       begin
          for I in To'First .. Min_Last loop
             To (I) := From (I);
          end loop;
          for I in Min_Last + 1 .. To'Last loop
             To (I) := '_';
          end loop;
       end;

       function "=" (Ref, Dut : Lim_String) return Boolean is
          Min_Last : constant Positive := Get_Min_Last (Ref, Dut);
       begin
          for I in Dut'First .. Min_Last loop
             if Dut (I) /= Ref (I) then
                return False;
             end if;
          end loop;

          return True;
       end;

    end Limited_Strings;

    with Ada.Text_IO;     use Ada.Text_IO;

    with Limited_Strings; use Limited_Strings;

    procedure Check_Lim_String is
       S1 : constant Lim_String := Init ("Hello World");
       S2 : constant Lim_String := Init (30);
       S3 : Lim_String := Init (5);
       S4 : Lim_String := Init (30);
    begin
       Put ("S1 => ");
       Put_Line (S1);
       Put ("S2 => ");
       Put_Line (S2);

       if S1 = S2 then
          Put_Line ("S1 is equal to S2.");
       else
          Put_Line ("S1 isn't equal to S2.");
       end if;

       Copy (From => S1, To => S3);
       Put ("S3 => ");
       Put_Line (S3);

       if S1 = S3 then
          Put_Line ("S1 is equal to S3.");
       else
          Put_Line ("S1 isn't equal to S3.");
       end if;

       Copy (From => S1, To => S4);
       Put ("S4 => ");
       Put_Line (S4);

       if S1 = S4 then
          Put_Line ("S1 is equal to S4.");
       else
          Put_Line ("S1 isn't equal to S4.");
       end if;
    end Check_Lim_String;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Check_Lim_String;

    procedure Main is
       type Test_Case_Index is
         (Lim_String_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
          when Lim_String_Chk =>
             Check_Lim_String;
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

Generics
--------

Display Array
~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Generics.Display_Array

    --  START LAB IO BLOCK
    in 0:Int_Array_Chk
    out 0:Integers  1:  1  2:  2  3:  5  4:  7  5:  10
    in 1:Point_Array_Chk
    out 1:Points  0: ( 1.00000E+00,  5.00000E-01)  1: ( 2.00000E+00, -5.00000E-01)  2: ( 5.00000E+00,  2.00000E+00)  3: (-5.00000E-01,  2.00000E+00)
    --  END LAB IO BLOCK

    generic
       type T_Range is range <>;
       type T_Element is private;
       type T_Array is array (T_Range range <>) of T_Element;
       with function Image (E : T_Element) return String is <>;
    procedure Display_Array (Header : String;
                             A      : T_Array);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Array (Header : String;
                             A      : T_Array) is
    begin
       Put_Line (Header);
       for I in A'Range loop
          Put_Line (T_Range'Image (I) & ": " & Image (A (I)));
       end loop;
    end Display_Array;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_Array;

    procedure Main is
       type Test_Case_Index is (Int_Array_Chk,
                                Point_Array_Chk);

       procedure Test_Int_Array is
          type Int_Array is array (Positive range <>) of Integer;

          procedure Display_Int_Array is new
            Display_Array (T_Range => Positive,
                           T_Element => Integer,
                           T_Array   => Int_Array,
                           Image     => Integer'Image);

          A : constant Int_Array (1 .. 5) := (1, 2, 5, 7, 10);
       begin
          Display_Int_Array ("Integers", A);
       end Test_Int_Array;

       procedure Test_Point_Array is
          type Point is record
             X : Float;
             Y : Float;
          end record;

          type Point_Array is array (Natural range <>) of Point;

          function Image (P : Point) return String is
          begin
             return "(" & Float'Image (P.X)
               & ", " & Float'Image (P.Y) & ")";
          end Image;

          procedure Display_Point_Array is new
            Display_Array (T_Range   => Natural,
                           T_Element => Point,
                           T_Array   => Point_Array,
                           Image     => Image);

          A : constant Point_Array (0 .. 3) := ((1.0, 0.5), (2.0, -0.5),
                                                (5.0, 2.0), (-0.5, 2.0));
       begin
          Display_Point_Array ("Points", A);
       end Test_Point_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Int_Array_Chk =>
                Test_Int_Array;
             when Point_Array_Chk =>
                Test_Point_Array;
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

Average of Array of Float
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Generics.Average_Array_Of_Float

    --  START LAB IO BLOCK
    in 0:Float_Array_Chk
    out 0:Average:  8.00000E-01
    in 1:Digits_12_Float_Array_Chk
    out 1:Average:  5.40000000000E+00
    --  END LAB IO BLOCK

    generic
       type T_Range is range <>;
       type T_Element is digits <>;
       type T_Array is array (T_Range range <>) of T_Element;
    function Average (A : T_Array) return T_Element;

    function Average (A : T_Array) return T_Element is
       Acc : T_Element := 0.0;
    begin
       for I in A'Range loop
          Acc := Acc + A (I);
       end loop;

       return Acc / T_Element (A'Length);
    end Average;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Main is
       type Test_Case_Index is (Float_Array_Chk,
                                Digits_12_Float_Array_Chk);

       procedure Test_Float_Array is
          type Float_Array is array (Positive range <>) of Float;

          function Average_Float is new
            Average (T_Range   => Positive,
                     T_Element => Float,
                     T_Array   => Float_Array);

          A : constant Float_Array (1 .. 5) := (1.0, 3.0, 5.0, 7.5, -12.5);
       begin
          Put_Line ("Average: " & Float'Image (Average_Float (A)));
       end Test_Float_Array;

       procedure Test_Digits_12_Float_Array is
          type Custom_Float is digits 12;

          type Float_Array is
            array (Integer range <>) of Custom_Float;

          function Average_Float is new
            Average (T_Range   => Integer,
                     T_Element => Custom_Float,
                     T_Array   => Float_Array);

          A : constant Float_Array (-1 .. 3) := (-1.0, 3.0, 5.0, 7.5, 12.5);
       begin
          Put_Line ("Average: "
                    & Custom_Float'Image (Average_Float (A)));
       end Test_Digits_12_Float_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Float_Array_Chk =>
                Test_Float_Array;
             when Digits_12_Float_Array_Chk =>
                Test_Digits_12_Float_Array;
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

Average of Array of Decimal
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Generics.Average_Array_Of_Decimal

    --  START LAB IO BLOCK
    in 0:Decimal_Array_Chk
    out 0:Average:  5.40
    in 1:Delta_EM4_Digits_16_Float_Array_Chk
    out 1:Average:  1.2000
    --  END LAB IO BLOCK

    generic
       type T_Range is range <>;
       type T_Element is delta <> digits <>;
       type T_Array is array (T_Range range <>) of T_Element;
    function Average (A : T_Array) return T_Element;

    function Average (A : T_Array) return T_Element is
       Acc : T_Element := 0.0;
    begin
       for I in A'Range loop
          Acc := Acc + A (I);
       end loop;

       return Acc / T_Element (A'Length);
    end Average;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Main is
       type Test_Case_Index is (Decimal_Array_Chk,
                                Delta_EM4_Digits_16_Float_Array_Chk);

       procedure Test_Decimal_Array is
          type Decimal is delta 10.0 ** (-2) digits 12;

          type Decimal_Array is
            array (Integer range <>) of Decimal;

          function Average_Decimal is new
            Average (T_Range   => Integer,
                     T_Element => Decimal,
                     T_Array   => Decimal_Array);

          A : constant Decimal_Array (-2 .. 2) := (-1.0, 3.0, 5.0, 7.5, 12.5);
       begin
          Put_Line ("Average: "
                    & Decimal'Image (Average_Decimal (A)));
       end Test_Decimal_Array;

       procedure Test_Delta_EM4_Digits_16_Float_Array is
          type Decimal is delta 10.0 ** (-4) digits 16;

          type Decimal_Array is
            array (Positive range <>) of Decimal;

          function Average_Decimal is new
            Average (T_Range   => Positive,
                     T_Element => Decimal,
                     T_Array   => Decimal_Array);

          A : constant Decimal_Array (2 .. 6) := (2.0, 5.0, 2.0, 8.5, -11.5);
       begin
          Put_Line ("Average: "
                    & Decimal'Image (Average_Decimal (A)));
       end Test_Delta_EM4_Digits_16_Float_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Decimal_Array_Chk =>
                Test_Decimal_Array;
             when Delta_EM4_Digits_16_Float_Array_Chk =>
                Test_Delta_EM4_Digits_16_Float_Array;
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

Average of Array of Any Type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Generics.Average_Any

    --  START LAB IO BLOCK
    in 0:Decimal_Array_Chk
    out 0:Average: 5.40
    in 1:Item_Array_Chk
    out 1:Average per item & quantity: 175.00 Average price:                 7.50
    --  END LAB IO BLOCK

    generic
       type T_Range is range <>;
       type T_Element is private;
       type T_Array is array (T_Range range <>) of T_Element;
       with function To_Float (E : T_Element) return Float is <>;
    function Average (A : T_Array) return Float;

    function Average (A : T_Array) return Float is
       Acc : Float := 0.0;
    begin
       for I in A'Range loop
          Acc := Acc + To_Float (A (I));
       end loop;

       return Acc / Float (A'Length);
    end Average;

    procedure Test_Decimal_Array;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Test_Decimal_Array is
       package F_IO is new Ada.Text_IO.Float_IO (Float);

       type Decimal is delta 10.0 ** (-2) digits 12;

       type Decimal_Array is
         array (Integer range <>) of Decimal;

       function To_Float (V : Decimal) return Float is
         (Float (V));

       function Average_Decimal is new
         Average (T_Range   => Integer,
                  T_Element => Decimal,
                  T_Array   => Decimal_Array);

       A : constant Decimal_Array (-2 .. 2) := (-1.0, 3.0, 5.0, 7.5, 12.5);
    begin
       Put ("Average: ");
       F_IO.Put (Average_Decimal (A), 1, 2, 0);
       New_Line;
    end Test_Decimal_Array;

    procedure Test_Item_Array;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Test_Item_Array is
       package F_IO is new Ada.Text_IO.Float_IO (Float);

       type Amount is delta 0.01 digits 12;

       type Item is record
          Quantity : Natural;
          Price    : Amount;
       end record;

       type Item_Array is
         array (Positive range <>) of Item;

       function Get_Total (I : Item) return Float is
         (Float (I.Quantity) * Float (I.Price));

       function Get_Price (I : Item) return Float is
         (Float (I.Price));

       function Average_Total is new
         Average (T_Range   => Positive,
                  T_Element => Item,
                  T_Array   => Item_Array,
                  To_Float  => Get_Total);

       function Average_Price is new
         Average (T_Range   => Positive,
                  T_Element => Item,
                  T_Array   => Item_Array,
                  To_Float  => Get_Price);

       A : constant Item_Array (1 .. 4)
         := ((Quantity =>  5,   Price => 10.00),
             (Quantity => 80,   Price =>  2.50),
             (Quantity => 40,   Price =>  5.00),
             (Quantity => 20,   Price => 12.50));

    begin
       Put ("Average per item & quantity: ");
       F_IO.Put (Average_Total (A), 3, 2, 0);
       New_Line;

       Put ("Average price:               ");
       F_IO.Put (Average_Price (A), 3, 2, 0);
       New_Line;
    end Test_Item_Array;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Decimal_Array;
    with Test_Item_Array;

    procedure Main is
       type Test_Case_Index is (Decimal_Array_Chk,
                                Item_Array_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Decimal_Array_Chk =>
                Test_Decimal_Array;
             when Item_Array_Chk =>
                Test_Item_Array;
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

Generic list
~~~~~~~~~~~~

.. code:: ada lab=Solutions.Generics.Gen_List

    --  START LAB IO BLOCK
    in 0:Int_List_Chk
    out 0:Added item successfully! Added item successfully! Added item successfully! Couldn't add item! List of integers  2  5  7
    in 1:String_List_Chk
    out 1:Added item successfully! Added item successfully! Added item successfully! Couldn't add item! List of strings Hello World Bye
    --  END LAB IO BLOCK

    generic
       type Item is private;
       type Items is array (Positive range <>) of Item;
       Name       :        String;
       List_Array : in out Items;
       Last       : in out Natural;
       with procedure Put (I : Item) is <>;
    package Gen_List is

       procedure Init;

       procedure Add (I      :     Item;
                      Status : out Boolean);

       procedure Display;

    end Gen_List;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Gen_List is

       procedure Init is
       begin
          Last := List_Array'First - 1;
       end Init;

       procedure Add (I      :     Item;
                      Status : out Boolean) is
       begin
          Status := Last < List_Array'Last;

          if Status then
             Last := Last + 1;
             List_Array (Last) := I;
          end if;
       end Add;

       procedure Display is
       begin
          Put_Line (Name);
          for I in List_Array'First .. Last loop
             Put (List_Array (I));
             New_Line;
          end loop;
       end Display;

    end Gen_List;

    procedure Test_Int_List;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_List;

    procedure Test_Int_List is

       procedure Put (I : Integer) is
       begin
          Ada.Text_IO.Put (Integer'Image (I));
       end Put;

       type Integer_Array is array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);
       L : Natural;

       package Int_List is new
         Gen_List (Item          => Integer,
                   Items         => Integer_Array,
                   Name          => "List of integers",
                   List_Array    => A,
                   Last          => L);

       Success : Boolean;

       procedure Display_Add_Success (Success : Boolean) is
       begin
          if Success then
             Put_Line ("Added item successfully!");
          else
             Put_Line ("Couldn't add item!");
          end if;

       end Display_Add_Success;

    begin
       Int_List.Init;

       Int_List.Add (2, Success);
       Display_Add_Success (Success);

       Int_List.Add (5, Success);
       Display_Add_Success (Success);

       Int_List.Add (7, Success);
       Display_Add_Success (Success);

       Int_List.Add (8, Success);
       Display_Add_Success (Success);

       Int_List.Display;
    end Test_Int_List;

    procedure Test_String_List;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_List;

    procedure Test_String_List is

       type String_Access is access String;

       procedure Put (S : String_Access) is
       begin
          Ada.Text_IO.Put (S.all);
       end Put;

       type String_Array is array (Positive range <>) of String_Access;

       A : String_Array (1 .. 3);
       L : Natural;

       package String_List is new
         Gen_List (Item         => String_Access,
                   Items        => String_Array,
                   Name         => "List of strings",
                   List_Array   => A,
                   Last         => L);

       Success : Boolean;

       procedure Display_Add_Success (Success : Boolean) is
       begin
          if Success then
             Put_Line ("Added item successfully!");
          else
             Put_Line ("Couldn't add item!");
          end if;

       end Display_Add_Success;

    begin
       String_List.Init;

       String_List.Add (new String'("Hello"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("World"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("Bye"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("Wait"), Success);
       Display_Add_Success (Success);

       String_List.Display;
    end Test_String_List;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Int_List;
    with Test_String_List;

    procedure Main is
       type Test_Case_Index is (Int_List_Chk,
                                String_List_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Int_List_Chk =>
                Test_Int_List;
             when String_List_Chk =>
                Test_String_List;
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

Exceptions
----------

Uninitialized Value
~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Uninitialized_Value

    --  START LAB IO BLOCK
    in 0:Options_Chk
    out 0:Uninitialized value detected! OPTION_1 OPTION_2 OPTION_3
    --  END LAB IO BLOCK

    package Options is

       type Option is (Unitialized,
                       Option_1,
                       Option_2,
                       Option_3);

       Unitialized_Value : exception;

       function Image (O : Option) return String;

    end Options;

    package body Options is

       function Image (O : Option) return String is
       begin
          case O is
             when Unitialized =>
                raise Unitialized_Value with "Uninitialized value detected!";
             when others =>
                return Option'Image (O);
          end case;
       end Image;

    end Options;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Options;          use Options;

    procedure Main is
       type Test_Case_Index is
         (Options_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check (O : Option) is
          begin
             Put_Line (Image (O));
          exception
             when E : Unitialized_Value =>
                Put_Line (Exception_Message (E));
          end Check;

       begin
          case TC is
          when Options_Chk =>
             for O in Option loop
                Check (O);
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

Numerical Exception
~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Numerical_Exception

    --  START LAB IO BLOCK
    in 0:Exception_1_Chk
    out 0:Constraint_Error detected!
    in 1:Exception_2_Chk
    out 1:Custom_Exception raised!
    --  END LAB IO BLOCK

    package Tests is

       type Test_ID is (Test_1, Test_2);

       Custom_Exception : exception;

       procedure Num_Exception_Test (ID : Test_ID);

    end Tests;

    package body Tests is

       pragma Warnings (Off, "variable ""C"" is assigned but never read");

       procedure Num_Exception_Test (ID : Test_ID) is
          A, B, C : Integer;
       begin
          case ID is
             when Test_1 =>
                A := Integer'Last;
                B := Integer'Last;
                C := A + B;
             when Test_2 =>
                raise Custom_Exception with "Custom_Exception raised!";
          end case;
       end Num_Exception_Test;

       pragma Warnings (On, "variable ""C"" is assigned but never read");

    end Tests;

    with Tests; use Tests;

    procedure Check_Exception (ID : Test_ID);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
    exception
       when Constraint_Error =>
          Put_Line ("Constraint_Error detected!");
       when E : others =>
          Put_Line (Exception_Message (E));
    end Check_Exception;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Tests;            use Tests;
    with Check_Exception;

    procedure Main is
       type Test_Case_Index is
         (Exception_1_Chk,
          Exception_2_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Handle_Exception (ID : Test_ID) is
          begin
             Check_Exception (ID);
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error"
                          & " (raised by Check_Exception) detected!");
             when E : others =>
                Put_Line (Exception_Name (E)
                          & " (raised by Check_Exception) detected!");
          end Check_Handle_Exception;

       begin
          case TC is
          when Exception_1_Chk =>
             Check_Handle_Exception (Test_1);
          when Exception_2_Chk =>
             Check_Handle_Exception (Test_2);
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

Re-raising Exceptions
~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Exception_Reraise

    --  START LAB IO BLOCK
    in 0:Exception_1_Chk
    out 0:Constraint_Error detected! Constraint_Error (raised by Check_Exception) detected!
    in 1:Exception_2_Chk
    out 1:Custom_Exception raised! TESTS.CUSTOM_EXCEPTION (raised by Check_Exception) detected!
    --  END LAB IO BLOCK

    package Tests is

       type Test_ID is (Test_1, Test_2);

       Custom_Exception : exception;

       procedure Num_Exception_Test (ID : Test_ID);

    end Tests;

    package body Tests is

       pragma Warnings (Off, "variable ""C"" is assigned but never read");

       procedure Num_Exception_Test (ID : Test_ID) is
          A, B, C : Integer;
       begin
          case ID is
             when Test_1 =>
                A := Integer'Last;
                B := Integer'Last;
                C := A + B;
             when Test_2 =>
                raise Custom_Exception with "Custom_Exception raised!";
          end case;
       end Num_Exception_Test;

       pragma Warnings (On, "variable ""C"" is assigned but never read");

    end Tests;

    with Tests; use Tests;

    procedure Check_Exception (ID : Test_ID);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
    exception
       when Constraint_Error =>
          Put_Line ("Constraint_Error detected!");
          raise;
       when E : others =>
          Put_Line (Exception_Message (E));
          raise;
    end Check_Exception;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Tests;            use Tests;
    with Check_Exception;

    procedure Main is
       type Test_Case_Index is
         (Exception_1_Chk,
          Exception_2_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Handle_Exception (ID : Test_ID) is
          begin
             Check_Exception (ID);
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error"
                          & " (raised by Check_Exception) detected!");
             when E : others =>
                Put_Line (Exception_Name (E)
                          & " (raised by Check_Exception) detected!");
          end Check_Handle_Exception;

       begin
          case TC is
          when Exception_1_Chk =>
             Check_Handle_Exception (Test_1);
          when Exception_2_Chk =>
             Check_Handle_Exception (Test_2);
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

Tasking
-------

Display Service
~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Display_Service

    --  START LAB IO BLOCK
    in 0:Display_Service_Chk
    out 0:Hello Hello again  55
    --  END LAB IO BLOCK

    package Display_Services is

       task type Display_Service is
          entry Display (S : String);
          entry Display (I : Integer);
       end Display_Service;

    end Display_Services;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Display_Services is

       task body Display_Service is
       begin
          loop
             select
                accept Display (S : String) do
                   Put_Line (S);
                end Display;
             or
                accept Display (I : Integer) do
                   Put_Line (Integer'Image (I));
                end Display;
             or
                terminate;
             end select;
          end loop;
       end Display_Service;

    end Display_Services;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_Services; use Display_Services;

    procedure Main is
       type Test_Case_Index is (Display_Service_Chk);

       procedure Check (TC : Test_Case_Index) is
          Display : Display_Service;
       begin
          case TC is
             when Display_Service_Chk =>
                Display.Display ("Hello");
                delay 0.5;
                Display.Display ("Hello again");
                delay 0.5;
                Display.Display (55);
                delay 0.5;
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

Event Manager
~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Event_Manager

    --  START LAB IO BLOCK
    in 0:Event_Manager_Chk
    out 0:Event # 3 Event # 4 Event # 2 Event # 5 Event # 1
    --  END LAB IO BLOCK

    with Ada.Real_Time; use Ada.Real_Time;

    package Event_Managers is

       task type Event_Manager is
          entry Start (ID : Natural);
          entry Event (T : Time);
       end Event_Manager;

    end Event_Managers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Event_Managers is

       task body Event_Manager is
          Event_ID    : Natural := 0;
          Event_Delay : Time;
       begin
          accept Start (ID : Natural) do
             Event_ID := ID;
          end Start;

          accept Event (T : Time) do
             Event_Delay := T;
          end Event;

          delay until Event_Delay;

          Put_Line ("Event #" & Natural'Image (Event_ID));
       end Event_Manager;

    end Event_Managers;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Event_Managers;   use Event_Managers;
    with Ada.Real_Time;    use Ada.Real_Time;

    procedure Main is
       type Test_Case_Index is (Event_Manager_Chk);

       procedure Check (TC : Test_Case_Index) is
          Ev_Mng : array (1 .. 5) of Event_Manager;
       begin
          case TC is
             when Event_Manager_Chk =>
                for I in Ev_Mng'Range loop
                   Ev_Mng (I).Start (I);
                end loop;
                Ev_Mng (1).Event (Clock + Seconds (5));
                Ev_Mng (2).Event (Clock + Seconds (3));
                Ev_Mng (3).Event (Clock + Seconds (1));
                Ev_Mng (4).Event (Clock + Seconds (2));
                Ev_Mng (5).Event (Clock + Seconds (4));
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

Generic Protected Queue
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Generic_Protected_Queue

    --  START LAB IO BLOCK
    in 0:Simple_Queue_Chk
    out 0:Value from queue:  1.00000E+01 Value from queue:  1.15000E+01 Value from queue:  1.30000E+01 Value from queue:  1.45000E+01 Value from queue:  1.60000E+01 Value from queue:  1.75000E+01 Value from queue:  1.90000E+01 Value from queue:  2.05000E+01 Value from queue:  2.20000E+01 Value from queue:  2.35000E+01
    in 1:Concurrent_Queue_Chk
    out 1:Value from queue:  100 Value from queue:  101 Value from queue:  102 Value from queue:  103 Value from queue:  104 Value from queue:  105 Value from queue:  106 Value from queue:  107 Value from queue:  108 Value from queue:  109 Value from queue:  110 Value from queue:  111 Value from queue:  112 Value from queue:  113 Value from queue:  114 Value from queue:  115 Value from queue:  116 Value from queue:  117 Value from queue:  118 Value from queue:  119
    --  END LAB IO BLOCK

    generic
       type Queue_Index is mod <>;
       type T is private;
    package Gen_Queues is

       type Queue_Array is array (Queue_Index) of T;

       protected type Queue is
          function Empty return Boolean;
          function Full return Boolean;
          entry Push (V : T);
          entry Pop (V : out T);
       private
          N   : Natural     := 0;
          Idx : Queue_Index := Queue_Array'First;
          A   : Queue_Array;
       end Queue;

    end Gen_Queues;

    package body Gen_Queues is

       protected body Queue is

          function Empty return Boolean is
            (N = 0);

          function Full return Boolean is
             (N = A'Length);

          entry Push (V : T) when not Full is
          begin
             A (Idx) := V;

             Idx := Idx + 1;
             N   := N + 1;
          end Push;

          entry Pop (V : out T) when not Empty is
          begin
             N := N - 1;

             V := A (Idx - Queue_Index (N) - 1);
          end Pop;

       end Queue;

    end Gen_Queues;

    package Queue_Tests is

       procedure Simple_Test;

       procedure Concurrent_Test;

    end Queue_Tests;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_Queues;

    package body Queue_Tests is

       Max : constant := 10;
       type Queue_Mod is mod Max;

       procedure Simple_Test is
          package Queues_Float is new Gen_Queues (Queue_Mod, Float);

          Q_F : Queues_Float.Queue;
          V   : Float;
       begin
          V := 10.0;
          while not Q_F.Full loop
             Q_F.Push (V);
             V := V + 1.5;
          end loop;

          while not Q_F.Empty loop
             Q_F.Pop (V);
             Put_Line ("Value from queue: " & Float'Image (V));
          end loop;
       end Simple_Test;

       procedure Concurrent_Test is
          package Queues_Integer is new Gen_Queues (Queue_Mod, Integer);

          Q_I : Queues_Integer.Queue;

          task T_Producer;
          task T_Consumer;

          task body T_Producer is
             V : Integer := 100;
          begin
             for I in 1 .. 2 * Max loop
                Q_I.Push (V);
                V := V + 1;
             end loop;
          end T_Producer;

          task body T_Consumer is
             V : Integer;
          begin
             delay 1.5;

             while not Q_I.Empty loop
                Q_I.Pop (V);
                Put_Line ("Value from queue: " & Integer'Image (V));
                delay 0.2;
             end loop;
          end T_Consumer;
       begin
          null;
       end Concurrent_Test;

    end Queue_Tests;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Queue_Tests;      use Queue_Tests;

    procedure Main is
       type Test_Case_Index is (Simple_Queue_Chk,
                                Concurrent_Queue_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Simple_Queue_Chk =>
                Simple_Test;
             when Concurrent_Queue_Chk =>
                Concurrent_Test;
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

Contracts
---------

Price Range
~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Price_Range

    --  START LAB IO BLOCK
    in 0:Price_Range_Chk
    out 0:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Prices is

       pragma Assertion_Policy (Static_Predicate  => Check,
                                Dynamic_Predicate => Check);

       type Amount is delta 10.0 ** (-2) digits 12;

       --  subtype Price is Amount range 0.0 .. Amount'Last;

       subtype Price is Amount
         with Static_Predicate => Price >= 0.0;

    end Prices;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Prices;            use Prices;

    procedure Main is

       type Test_Case_Index is
         (Price_Range_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Range (A : Amount) is
             P : Price;
          begin
             P := A;
             Put_Line ("Price: " & Price'Image (P));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Range;

       begin
          case TC is
          when Price_Range_Chk =>
             Check_Range (-2.0);
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


Pythagorean Theorem: Predicate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Pythagoras_Predicate

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       pragma Assertion_Policy (Static_Predicate  => Check,
                                Dynamic_Predicate => Check);

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record
         with Dynamic_Predicate => H * H = C1 * C1 + C2 * C2;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Precondition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Pythagoras_Precondition

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       pragma Assertion_Policy (Pre => Check);

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2))
           with Pre => H * H = C1 * C1 + C2 * C2;

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Postcondition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Pythagoras_Postcondition

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       pragma Assertion_Policy (Post => Check);

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2))
           with Post => (Init'Result.H * Init'Result.H
                         = Init'Result.C1 * Init'Result.C1
                         + Init'Result.C2 * Init'Result.C2);

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Type Invariant
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Pythagoras_Type_Invariant

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       pragma Assertion_Policy (Type_Invariant => Check);

       subtype Length is Integer;

       type Right_Triangle is private
         with Type_Invariant => Check (Right_Triangle);

       function Check (T : Right_Triangle) return Boolean;

       function Init (H, C1, C2 : Length) return Right_Triangle;

    private

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

       function Check (T : Right_Triangle) return Boolean is
         (T.H * T.H = T.C1 * T.C1 + T.C2 * T.C2);

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Primary Colors
~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Primary_Colors

    --  START LAB IO BLOCK
    in 0:HTML_Color_Red_Chk
    out 0:Selected: RED SALMON =>     16#FA#. FIREBRICK =>     16#B2#. RED =>     16#FF#. DARKRED =>     16#8B#. LIME =>      16#0#. FORESTGREEN =>     16#22#. GREEN =>      16#0#. DARKGREEN =>      16#0#. BLUE =>      16#0#. MEDIUMBLUE =>      16#0#. DARKBLUE =>      16#0#.
    in 1:HTML_Color_Green_Chk
    out 1:Selected: GREEN SALMON =>     16#80#. FIREBRICK =>     16#22#. RED =>      16#0#. DARKRED =>      16#0#. LIME =>     16#FF#. FORESTGREEN =>     16#8B#. GREEN =>     16#80#. DARKGREEN =>     16#64#. BLUE =>      16#0#. MEDIUMBLUE =>      16#0#. DARKBLUE =>      16#0#.
    in 2:HTML_Color_Blue_Chk
    out 2:Selected: BLUE SALMON =>     16#72#. FIREBRICK =>     16#22#. RED =>      16#0#. DARKRED =>      16#0#. LIME =>      16#0#. FORESTGREEN =>     16#22#. GREEN =>      16#0#. DARKGREEN =>      16#0#. BLUE =>     16#FF#. MEDIUMBLUE =>     16#CD#. DARKBLUE =>     16#8B#.
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

       function Image (I : Int_Color) return String;

       type RGB is record
          Red   : Int_Color;
          Green : Int_Color;
          Blue  : Int_Color;
       end record;

       function To_RGB (C : HTML_Color) return RGB;

       function Image (C : RGB) return String;

       type HTML_Color_RGB_Array is array (HTML_Color) of RGB;

       To_RGB_Loopup_Table : constant HTML_Color_RGB_Array
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

       subtype HTML_RGB_Color is HTML_Color
         with Static_Predicate => HTML_RGB_Color in Red | Green | Blue;

       function To_Int_Color (C : HTML_Color;
                              S : HTML_RGB_Color) return Int_Color;
       --  Convert to hexadecimal value for the selected RGB component S

    end Color_Types;

    with Ada.Integer_Text_IO;

    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          return To_RGB_Loopup_Table (C);
       end To_RGB;

       function To_Int_Color (C : HTML_Color;
                              S : HTML_RGB_Color) return Int_Color is
          C_RGB : constant RGB := To_RGB (C);
       begin
          case S is
             when Red   => return C_RGB.Red;
             when Green => return C_RGB.Green;
             when Blue  => return C_RGB.Blue;
          end case;
       end To_Int_Color;

       function Image (I : Int_Color) return String is
          subtype Str_Range is Integer range 1 .. 10;
          S : String (Str_Range);
       begin
          Ada.Integer_Text_IO.Put (To    => S,
                                   Item  => I,
                                   Base  => 16);
          return S;
       end Image;

       function Image (C : RGB) return String is
       begin
          return ("(Red => "      & Image (C.Red)
                  & ", Green => " & Image (C.Green)
                  & ", Blue => "  & Image (C.Blue)
                  &")");
       end Image;

    end Color_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Color_Types;      use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (HTML_Color_Red_Chk,
          HTML_Color_Green_Chk,
          HTML_Color_Blue_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_HTML_Colors (S : HTML_RGB_Color) is
          begin
             Put_Line ("Selected: " & HTML_RGB_Color'Image (S));
             for I in HTML_Color'Range loop
                Put_Line (HTML_Color'Image (I) & " => "
                          & Image (To_Int_Color (I, S)) & ".");
             end loop;
          end Check_HTML_Colors;

       begin
          case TC is
             when HTML_Color_Red_Chk =>
                Check_HTML_Colors (Red);
             when HTML_Color_Green_Chk =>
                Check_HTML_Colors (Green);
             when HTML_Color_Blue_Chk =>
                Check_HTML_Colors (Blue);
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
