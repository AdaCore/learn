Records
=======

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Directions
----------

In this exercise, we make use of the algorithm implemented in the
:ada:`Check_Direction` procedure (see chapter on
:doc:`chapter on imperative language <courses/intro-to-ada/chapters/imperative_language>`).
For the sake of this exercise, we use the concept of an *extended angle*,
which includes the actual geometric angle and the corresponding direction
(North, South, Northwest, and so on). These are your goals:

#. Create a record :ada:`Ext_Angle` to store information about the
   extended angle.

#. Implement a procedure :ada:`Display` to display information about the
   extended angle.

#. Implement the function :ada:`To_Ext_Angle` to convert a simple angle
   value to an extended angle.

.. code:: ada lab=Records.Directions

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

       --  Include type declaration for Ext_Angle record type:
       --
       --  NOTE: Use the Angle_Mod and Direction types declared above!
       --
       --  type Ext_Angle is [...]
       --

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle;

       procedure Display (N : Ext_Angle);

    end Directions;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Directions is

       procedure Display (N : Ext_Angle) is
       begin
          --  Uncomment the code below and fill the missing elements
          --
          --  Put_Line ("Angle: "
          --            & Angle_Mod'Image (____)
          --            & " => "
          --            & Direction'Image (____)
          --            & ".");
          null;
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
          --  Implement the conversion from Angle_Mod to Ext_Angle here!
          --
          --  Hint: you can use a return statement and an aggregate.
          --
          null;
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
------

In this exercise, we use the exercise on HTML colors from the previous lab
on :doc:`./strongly_typed_language` as a starting point.

Just to recapitulate, these are the HTML colors that we use:

   +-------------+---------------+
   | Color       | Value         |
   +=============+===============+
   | Salmon      | ```#FA8072``` |
   +-------------+---------------+
   | Firebrick   | ```#B22222``` |
   +-------------+---------------+
   | Red         | ```#FF0000``` |
   +-------------+---------------+
   | Darkred     | ```#8B0000``` |
   +-------------+---------------+
   | Lime        | ```#00FF00``` |
   +-------------+---------------+
   | Forestgreen | ```#228B22``` |
   +-------------+---------------+
   | Green       | ```#008000``` |
   +-------------+---------------+
   | Darkgreen   | ```#006400``` |
   +-------------+---------------+
   | Blue        | ```#0000FF``` |
   +-------------+---------------+
   | Mediumblue  | ```#0000CD``` |
   +-------------+---------------+
   | Darkblue    | ```#00008B``` |
   +-------------+---------------+

The hexadecimal information of each color on this table can be mapped
to three color elements: red, green and blue. Each color element has a
value between 0 and 255, or ``00`` and ``FF`` in hexadecimal. For the
color *salmon*, the value of the color elements are: red = ``FA``, green =
``80`` and blue = ``72`` (in hexadecimal).

Your goal with this exercise is to create a record :ada:`RGB` that stores
information about HTML colors in RGB format, so that we can retrieve
the individual color elements. In addition, you will:

#. Implement a function :ada:`To_RGB` to convert from the
   :ada:`HTML_Color` type to the :ada:`RGB` type based on the information
   from the table above.

#. Implement a function :ada:`Image` that returns a string representation
   of the :ada:`RGB` type in this format:
   :ada:`"(Red => 16#..#, Green => 16#...#, Blue => 16#...# )"`

.. code:: ada lab=Records.Colors

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

       --  Replace type declaration for RGB record below
       --
       --  - NOTE: Use the Int_Color type declared above!
       --
       --  type RGB is [...]
       --
       type RGB is null record;

       function To_RGB (C : HTML_Color) return RGB;

       function Image (C : RGB) return String;

    end Color_Types;

    with Ada.Integer_Text_IO;

    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          --  Implement the conversion from HTML_Color to RGB here!
          --
          return (null record);
       end To_RGB;

       function Image (C : RGB) return String is
          subtype Str_Range is Integer range 1 .. 10;
          SR : String (Str_Range);
          SG : String (Str_Range);
          SB : String (Str_Range);
       begin
          --  Replace argument in the calls to Put below
          --  with the missing elements (red, green, blue)
          --  from the RGB record
          --
          Ada.Integer_Text_IO.Put (To    => SR,
                                   Item  => 0,    --  REPLACE!
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SG,
                                   Item  => 0,    --  REPLACE!
                                   Base  => 16);
          Ada.Integer_Text_IO.Put (To    => SB,
                                   Item  => 0,    --  REPLACE!
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
---------

In this exercise, you'll create a simplified inventory system for your
store. The system will be used to enter items and keep track of your
assets. These are your goals:

#. Create a record :ada:`Item` to collect information about products from
   your store. To keep it simple, this record only contains the quantity
   and price of each item. The record elements must be named :ada:`Quantity` and
   :ada:`Price`.

#. Create a record :ada:`Inventory` to collect information about your
   inventory. In this case, we're only interested in the assets.

#. Implement an :ada:`Init` function for the :ada:`Item` type to return an
   initialized item. This function should also display the item name.

#. Implement a procedure :ada:`Add` to add an item to your inventory.
   Since you're keeping track of the assets, you should accumulate the
   total amount of each item in this element.

   - Hint: the code below doesn't have an :ada:`Init` subprogram for the
     :ada:`Inventory` type. In order for your system to have correct
     information about your assets, you should declare a default value.
     Alternatively, you can implement an :ada:`Init` subprogram and make
     sure it is called in the :ada:`Check` procedure below.

#. Implement a procedure :ada:`Display` to display information about the
   inventory.

.. code:: ada lab=Records.Inventory

    --  START LAB IO BLOCK
    in 0:Inventory_Chk
    out 0:Adding item: Ballpoint Pen. Assets: $27.75. Adding item: Oil-based Pen Marker. Assets: $927.75. Adding item: Feather Quill Pen. Assets: $1007.75.
    --  END LAB IO BLOCK

    package Inventory_Pkg is

       --  Replace type declaration for Item record:
       --
       type Item is null record;

       --  Replace type declaration for Inventory record:
       --
       type Inventory is null record;

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

          --  Replace return statement with the actual record initialization!
          --
          return (null record);
       end Init;

       procedure Add (Inv : in out Inventory;
                      I   : Item) is
       begin
          --  Implement the function that adds an item to the inventory here!
          --
          null;
       end Add;

       procedure Display (Inv : Inventory) is
          package F_IO is new Ada.Text_IO.Float_IO (Float);

          use F_IO;
       begin
          --  Uncomment the code below and fill the missing elements
          --
          --  Put ("Assets: $");
          --  Put (____, 1, 2, 0);
          --  Put (".");
          --  New_Line;
          null;
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
