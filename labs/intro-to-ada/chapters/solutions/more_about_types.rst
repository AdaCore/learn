:orphan:

Solutions
=========

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

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
    out 0:==== ITEM #  1: Ballpoint Pen  Price:     0.90 == BOUGHT Quantity:  10 Amount:    1.50 == SOLD Quantity:  4 Amount:    0.60 == IN STOCK Quantity:  6 Amount:    0.90  ==== ITEM #  2: Oil-based Pen Marker  Price:     180.00 == BOUGHT Quantity:  20 Amount:    180.00 == SOLD Quantity:  0 Amount:    0.00 == IN STOCK Quantity:  20 Amount:    180.00  ==== ITEM #  3: Feather Quill Pen  Price:     450.00 == BOUGHT Quantity:  50 Amount:    750.00 == SOLD Quantity:  20 Amount:    300.00 == IN STOCK Quantity:  30 Amount:    450.00  ==== OVERALL Amount bought:    931.50 Amount sold:      300.60 Amount in stock:  450.00
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
             New_Line;
             Put ("Price:     ");
             Put (Amount'(Get (Inv, I)), 1, 2, 0);
             New_Line;
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
