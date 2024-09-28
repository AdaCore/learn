Records
-------

Directions
~~~~~~~~~~

.. code:: ada lab=Solutions.Records.Directions

    --  START LAB IO BLOCK
    in 0:Direction_Chk
    out 0:Angle:  0 => NORTH. Angle:  30 => NORTHEAST. Angle:  45 => NORTHEAST. Angle:  90 => EAST. Angle:  91 => SOUTHEAST. Angle:  120 => SOUTHEAST. Angle:  180 => SOUTH. Angle:  250 => SOUTHWEST. Angle:  270 => WEST.
    --  END LAB IO BLOCK

    package Directions is

       type Angle_Mod is mod 360;

       type Direction is
         (North,
          Northeast,
          East,
          Southeast,
          South,
          Southwest,
          West,
          Northwest);

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
             when   0        => return North;
             when   1 ..  89 => return Northeast;
             when  90        => return East;
             when  91 .. 179 => return Southeast;
             when 180        => return South;
             when 181 .. 269 => return Southwest;
             when 270        => return West;
             when 271 .. 359 => return Northwest;
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

       function To_Integer (C : HTML_Color) return Integer;

       type Basic_HTML_Color is
         (Red,
          Green,
          Blue);

       function To_HTML_Color (C : Basic_HTML_Color) return HTML_Color;

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
    out 0:Item: Ballpoint Pen. Assets: $27.75. Item: Oil-based Pen Marker. Assets: $927.75. Item: Feather Quill Pen. Assets: $1007.75.
    --  END LAB IO BLOCK

    package Inventory_Pkg is

       type Item_Name is
         (Ballpoint_Pen, Oil_Based_Pen_Marker, Feather_Quill_Pen);

       function To_String (I : Item_Name) return String;

       type Item is record
          Name     : Item_Name;
          Quantity : Natural;
          Price    : Float;
       end record;

       function Init (Name     : Item_Name;
                      Quantity : Natural;
                      Price    : Float) return Item;

       procedure Add (Assets : in out Float;
                      I      : Item);

    end Inventory_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Inventory_Pkg is

       function To_String (I : Item_Name) return String is
       begin
          case I is
             when Ballpoint_Pen        => return "Ballpoint Pen";
             when Oil_Based_Pen_Marker => return "Oil-based Pen Marker";
             when Feather_Quill_Pen    => return "Feather Quill Pen";
          end case;
       end To_String;

       function Init (Name     : Item_Name;
                      Quantity : Natural;
                      Price    : Float) return Item is
       begin
          Put_Line ("Item: " & To_String (Name) & ".");

          return (Name     => Name,
                  Quantity => Quantity,
                  Price    => Price);
       end Init;

       procedure Add (Assets : in out Float;
                      I      : Item) is
       begin
          Assets := Assets + Float (I.Quantity) * I.Price;
       end Add;

    end Inventory_Pkg;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Inventory_Pkg;     use Inventory_Pkg;

    procedure Main is
       --  Remark: the following line is not relevant.
       F   : array (1 .. 10) of Float := (others => 42.42);

       type Test_Case_Index is
         (Inventory_Chk);

       procedure Display (Assets : Float) is
          package F_IO is new Ada.Text_IO.Float_IO (Float);

          use F_IO;
       begin
          Put ("Assets: $");
          Put (Assets, 1, 2, 0);
          Put (".");
          New_Line;
       end Display;

       procedure Check (TC : Test_Case_Index) is
          I      : Item;
          Assets : Float := 0.0;

          --  Please ignore the following three lines!
          pragma Warnings (Off, "default initialization");
          for Assets'Address use F'Address;
          pragma Warnings (On, "default initialization");
       begin
          case TC is
          when Inventory_Chk =>
             I := Init (Ballpoint_Pen,        185,  0.15);
             Add (Assets, I);
             Display (Assets);

             I := Init (Oil_Based_Pen_Marker, 100,  9.0);
             Add (Assets, I);
             Display (Assets);

             I := Init (Feather_Quill_Pen,      2, 40.0);
             Add (Assets, I);
             Display (Assets);
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
