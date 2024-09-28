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
       with function Image (E : T_Element) return String;
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
    in 1:Digits_7_Float_Array_Chk
    out 1:Average:  5.200000E-01
    --  END LAB IO BLOCK

    generic
       type T_Range is range <>;
       type T_Element is digits <>;
       type T_Array is array (T_Range range <>) of T_Element;
    function Average (A : T_Array) return T_Element;

    function Average (A : T_Array) return T_Element is
       Acc : Float := 0.0;
    begin
       for I in A'Range loop
          Acc := Acc + Float (A (I));
       end loop;

       return T_Element (Acc / Float (A'Length));
    end Average;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Main is
       type Test_Case_Index is (Float_Array_Chk,
                                Digits_7_Float_Array_Chk);

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

       procedure Test_Digits_7_Float_Array is
          type Custom_Float is digits 7 range 0.0 .. 1.0;

          type Float_Array is
            array (Integer range <>) of Custom_Float;

          function Average_Float is new
            Average (T_Range   => Integer,
                     T_Element => Custom_Float,
                     T_Array   => Float_Array);

          A : constant Float_Array (-1 .. 3) := (0.5, 0.0, 1.0, 0.6, 0.5);
       begin
          Put_Line ("Average: "
                    & Custom_Float'Image (Average_Float (A)));
       end Test_Digits_7_Float_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Float_Array_Chk =>
                Test_Float_Array;
             when Digits_7_Float_Array_Chk =>
                Test_Digits_7_Float_Array;
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
    in 0:Item_Array_Chk
    out 0:Average per item & quantity: 175.00 Average price:                 7.50
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

    procedure Test_Item;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Test_Item is
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
    end Test_Item;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Item;

    procedure Main is
       type Test_Case_Index is (Item_Array_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Item_Array_Chk =>
                Test_Item;
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
    in 0:Int_Chk
    out 0:Added item successfully! Added item successfully! Added item successfully! Couldn't add item! List of integers  2  5  7
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

    procedure Test_Int;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_List;

    procedure Test_Int is

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
    end Test_Int;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Int;

    procedure Main is
       type Test_Case_Index is (Int_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Int_Chk =>
                Test_Int;
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
