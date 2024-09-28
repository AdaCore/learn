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
          To (To'First .. Min_Last)    := From (To'First .. Min_Last);
          To (Min_Last + 1 .. To'Last) := (others => '_');
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
       S  : constant String := "----------";
       S1 : constant Lim_String := Init ("Hello World");
       S2 : constant Lim_String := Init (30);
       S3 : Lim_String := Init (5);
       S4 : Lim_String := Init (S & S & S);
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
