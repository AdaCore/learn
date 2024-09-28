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

       To_RGB_Lookup_Table : constant HTML_Color_RGB
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
          return To_RGB_Lookup_Table (C);
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
                          & Image (To_RGB_Lookup_Table (Firebrick)));
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
    out 0: 5  4  3  2  1  9  8  7  6  5
    in 1:Init_Proc_Chk
    out 1: 5  4  3  2  1  9  8  7  6  5
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

       function Init (I, L : Positive) return My_Array;

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

       function Init (I, L : Positive) return My_Array is
          A : My_Array (I .. I + L - 1);
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
          A_Out (A'First) := 0;
          for I in A'First + 1 .. A'Last loop
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
          AB : My_Array (5 .. 9);

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
             AA := Init (AA'First, AA'Length);
             AB := Init (AB'First, AB'Length);
             Display (AA);
             Display (AB);
          when Init_Proc_Chk =>
             Init (AA);
             Init (AB);
             Display (AA);
             Display (AB);
          when Double_Chk =>
             Local_Init (AB);
             Double (AB);
             Display (AB);
          when Diff_Prev_Chk =>
             Local_Init (AB);
             AB := Diff_Prev_Elem (AB);
             Display (AB);
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

Product info
~~~~~~~~~~~~

.. code:: ada lab=Solutions.Arrays.Product_Info

    --  START LAB IO BLOCK
    in 0:Total_Func_Chk
    out 0:0.50 20.00 200.00 100.00 200.00
    in 1:Total_Proc_Chk
    out 1:0.50 20.00 200.00 100.00 200.00
    in 2:Total_Value_Chk
    out 2:520.50
    --  END LAB IO BLOCK

    package Product_Info_Pkg is

       subtype Quantity is Natural;

       subtype Currency is Float;

       type Product_Info is record
          Units : Quantity;
          Price : Currency;
       end record;

       type Product_Infos is array (Positive range <>) of Product_Info;

       type Currency_Array is array (Positive range <>) of Currency;

       procedure Total (P   : Product_Infos;
                        Tot : out Currency_Array);

       function Total (P : Product_Infos) return Currency_Array;

       function Total (P : Product_Infos) return Currency;

    end Product_Info_Pkg;

    package body Product_Info_Pkg is

       --  Get total for single product
       function Total (P : Product_Info) return Currency is
          (Currency (P.Units) * P.Price);

       procedure Total (P   : Product_Infos;
                        Tot : out Currency_Array) is
       begin
          for I in P'Range loop
             Tot (I) := Total (P (I));
          end loop;
       end Total;

       function Total (P : Product_Infos) return Currency_Array
       is
          Tot : Currency_Array (P'Range);
       begin
          Total (P, Tot);
          return Tot;
       end Total;

       function Total (P : Product_Infos) return Currency
       is
          Tot : Currency := 0.0;
       begin
         for I in P'Range loop
             Tot := Tot + Total (P (I));
          end loop;
          return Tot;
       end Total;

    end Product_Info_Pkg;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Product_Info_Pkg;   use Product_Info_Pkg;

    procedure Main is
       package Currency_IO is new Ada.Text_IO.Float_IO (Currency);

       type Test_Case_Index is
         (Total_Func_Chk,
          Total_Proc_Chk,
          Total_Value_Chk);

       procedure Check (TC : Test_Case_Index) is
          subtype Test_Range is Positive range 1 .. 5;

          P    : Product_Infos (Test_Range);
          Tots : Currency_Array (Test_Range);
          Tot  : Currency;

          procedure Display (Tots : Currency_Array) is
          begin
             for I in Tots'Range loop
                Currency_IO.Put (Tots (I));
                New_Line;
             end loop;
          end Display;

          procedure Local_Init (P : in out Product_Infos) is
          begin
             P := ((1,   0.5),
                   (2,  10.0),
                   (5,  40.0),
                   (10, 10.0),
                   (10, 20.0));
          end Local_Init;

       begin
          Currency_IO.Default_Fore := 1;
          Currency_IO.Default_Aft  := 2;
          Currency_IO.Default_Exp  := 0;

          case TC is
          when Total_Func_Chk =>
             Local_Init (P);
             Tots := Total (P);
             Display (Tots);
          when Total_Proc_Chk =>
             Local_Init (P);
             Total (P, Tots);
             Display (Tots);
          when Total_Value_Chk =>
             Local_Init (P);
             Tot := Total (P);
             Currency_IO.Put (Tot);
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
    in 0:String_10_Long_Chk
    out 0:And this i
    in 1:String_10_Short_Chk
    out 1:Hey!
    --  END LAB IO BLOCK

    package Strings_10 is

       subtype String_10 is String (1 .. 10);

       --  Using "type String_10 is..." is possible, too.

       function To_String_10 (S : String) return String_10;

    end Strings_10;

    package body Strings_10 is

       function To_String_10 (S : String) return String_10 is
          S_Out : String_10;
       begin
          for I in String_10'First .. Integer'Min (String_10'Last, S'Last) loop
             S_Out (I) := S (I);
          end loop;

          for I in Integer'Min (String_10'Last + 1, S'Last + 1) .. String_10'Last loop
             S_Out (I) := ' ';
          end loop;

          return S_Out;
       end To_String_10;

    end Strings_10;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Strings_10;         use Strings_10;

    procedure Main is
       type Test_Case_Index is
         (String_10_Long_Chk,
          String_10_Short_Chk);

       procedure Check (TC : Test_Case_Index) is
          SL   : constant String := "And this is a long string just for testing...";
          SS   : constant String := "Hey!";
          S_10 : String_10;

       begin
          case TC is
          when String_10_Long_Chk =>
             S_10 := To_String_10 (SL);
             Put_Line (String (S_10));
          when String_10_Short_Chk =>
             S_10 := (others => ' ');
             S_10 := To_String_10 (SS);
             Put_Line (String (S_10));
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
