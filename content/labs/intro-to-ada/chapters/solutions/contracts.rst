Design by contracts
-------------------

Price Range
~~~~~~~~~~~

.. code:: ada lab=Solutions.Contracts.Price_Range

    --  START LAB IO BLOCK
    in 0:Price_Range_Chk
    out 0:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Prices is

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
             P : constant Price := A;
          begin
             Put_Line ("Price: " & Price'Image (P));
          end Check_Range;

       begin
          case TC is
          when Price_Range_Chk =>
             Check_Range (-2.0);
          end case;
       exception
          when Constraint_Error =>
             Put_Line ("Constraint_Error detected (NOT as expected).");
          when Assert_Failure =>
             Put_Line ("Assert_Failure detected (as expected).");
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

       To_RGB_Lookup_Table : constant HTML_Color_RGB_Array
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
          return To_RGB_Lookup_Table (C);
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
