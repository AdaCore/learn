Standard library: Containers
============================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Simple todo list
----------------

This exercise is based on the *Simple todo list* exercise from the
:doc:`./more_about_types`. Your goal is to rewrite that exercise using
vectors instead of arrays. Please refer to the original exercise for
details. You may reuse the code you've implemented as a starting point.

.. code:: ada lab=Standard_Library.Simple_Todo_List

    --  START LAB IO BLOCK
    in 0:Todo_List_Chk
    out 0:TO-DO LIST Buy milk Buy tea Buy present Buy tickets Pay electricity bill Schedule dentist appointment Call sister Revise spreasheet Edit entry page Select new design Create upgrade plan
    --  END LAB IO BLOCK

    package Todo_Lists is

       type Todo_Item is access String;

       type Todo_List is null record;

       procedure Add (Todos : in out Todo_List;
                      Item  : String);

       procedure Display (Todos : Todo_List);

    end Todo_Lists;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Todo_Lists is

       procedure Add (Todos : in out Todo_List;
                      Item  : String) is
       begin
          null;
       end Add;

       procedure Display (Todos : Todo_List) is
       begin
          Put_Line ("TO-DO LIST");
       end Display;

    end Todo_Lists;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Todo_Lists;        use Todo_Lists;

    procedure Main is
       type Test_Case_Index is
         (Todo_List_Chk);

       procedure Check (TC : Test_Case_Index) is
          T : Todo_List;
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

Inventory
---------

This exercise is based on the *Inventory* exercise from the
:doc:`./more_about_types`. Your goal is to rewrite that exercise using
vectors |mdash| instead of arrays |mdash| and unbounded strings.

- Hint: for the declaration of the :ada:`Item_ID` type, you can use the
  cursor from the vector container.

Please refer to the original exercise for further details. You may reuse
the code you've implemented as a starting point.

.. code:: ada lab=Standard_Library.Inventory

    --  START LAB IO BLOCK
    in 0:Inventory_Chk
    out 0:==== ITEM : Ballpoint Pen == BOUGHT Quantity:  10 Value:     1.50 == SOLD Quantity:  4 Value:     0.60 == IN STOCK Quantity:  6 Value:     0.90  ==== ITEM : Oil-based Pen Marker == BOUGHT Quantity:  20 Value:     180.00 == SOLD Quantity:  0 Value:     0.00 == IN STOCK Quantity:  20 Value:     180.00  ==== ITEM : Feather Quill Pen == BOUGHT Quantity:  50 Value:     750.00 == SOLD Quantity:  20 Value:     300.00 == IN STOCK Quantity:  30 Value:     450.00  ==== OVERALL Value bought:     931.50 Value sold:       300.60 Value in stock:   630.90
    --  END LAB IO BLOCK

    with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
    with Ada.Containers.Vectors;

    package Inventory_Pkg is

       subtype Item_Quantity is Natural;

       type Currency is delta 10.0 ** (-2) digits 12;

       type Transaction_Type is (Bought, Sold);

       type Item is private;

       type Item_ID is private;

       type Inventory is private;

       function Init (Name  : String;
                      Price : Currency) return Item;

       procedure Init (Inv : in out Inventory);

       procedure Add (Inv     : in out Inventory;
                      I       :        Item;
                      ID      : out    Item_ID);

       function Get (Inv       : Inventory;
                     Item_Name : String) return Item_ID;

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
                     ID    : Item_ID) return Currency;
       --  Retrieve total amount in stock for specified item
       --
       --  Potential_Income_For_Units_In_Stock_For_Item : Currency := Get (Inv, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Item_Quantity;
       --  Retrieve number of units for specified item and transaction type
       --
       --  Number_Units_Sold_For_Item : Item_Quantity := Get (Inv, Sold, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Currency;
       --  Retrieve amount for specified item and transaction type
       --
       --  Income_For_Sold_Units_Of_Item : Currency := Get (Inv, Sold, ID);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type) return Currency;
       --  Retrieve amount for transaction type
       --
       --  Income_For_All_Sold_Units : Currency := Get (Inv, Sold);

       function Get (Inv   : Inventory) return Currency;
       --  Retrieve amount for inventory
       --
       --  Income_For_All_Units_In_Stock : Currency := Get (Inv);

       procedure Display (Inv : Inventory);

    private

       subtype Name_Type is Unbounded_String;

       type Item is null record;

       --  You can use the container's cursor for the declaration of
       --  Item_ID.
       type Item_ID is null record;

       type Inventory is null record;

    end Inventory_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Inventory_Pkg is

       function Init (Name  : String;
                      Price : Currency) return Item is
       begin
          return (null record);
       end Init;

       procedure Init (Inv : in out Inventory) is
       begin
          null;
       end Init;

       procedure Add (Inv     : in out Inventory;
                      I       :        Item;
                      ID      : out    Item_ID)
       is
       begin
          null;
       end Add;

       function Get (Inv       : Inventory;
                     Item_Name : String) return Item_ID
       is
       begin
          return (null record);
       end Get;

       procedure Set (Inv      : in out Inventory;
                      Trans    :        Transaction_Type;
                      ID       :        Item_ID;
                      Quantity :        Positive;
                      Success  :    out Boolean)
       is
       begin
          Success := False;
       end Set;

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return String is
         ("");

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Item_Quantity is
         (0);

       function Get (Inv   : Inventory;
                     ID    : Item_ID) return Currency is
         (0.0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Item_Quantity is
         (0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Currency is
         (0.0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type) return Currency
       is
          Total : Currency := 0.0;
       begin
          return Total;
       end Get;

       function Get (Inv   : Inventory) return Currency
       is
          Total : Currency := 0.0;
       begin
          return Total;
       end Get;

       procedure Display (Inv : Inventory)
       is
          package F_IO is new Ada.Text_IO.Decimal_IO (Currency);

          use F_IO;
       begin
       --  Uncomment and adapt the code below according to your declaration
       --  of the Inventory type:
       --
       --   for C in Inv.List_Item.Iterate loop
       --      declare
       --         I : constant Item_ID := Item_ID (C);
       --      begin
       --         Put_Line ("==== ITEM "
       --                   & ": " & Get (Inv, I));
       --         for Trans in Transaction_Type loop
       --            Put_Line ("== " & Transaction_Type'Image (Trans));
       --            Put_Line ("Quantity: "
       --                      & Item_Quantity'Image (Get (Inv, Trans, I)));
       --            Put ("Value:     ");
       --            Put (Currency'(Get (Inv, Trans, I)), 1, 2, 0);
       --            New_Line;
       --         end loop;
       --         Put_Line ("== IN STOCK");
       --         Put_Line ("Quantity: " & Item_Quantity'Image (Get (Inv, I)));
       --         Put ("Value:     ");
       --         Put (Currency'(Get (Inv, I)), 1, 2, 0);
       --         New_Line;
       --         New_Line;
       --      end;
       --   end loop;
          Put_Line ("==== OVERALL");
          Put ("Value bought:     ");
          Put (Currency'(Get (Inv, Bought)), 1, 2, 0);
          New_Line;
          Put ("Value sold:       ");
          Put (Currency'(Get (Inv, Sold)), 1, 2, 0);
          New_Line;
          Put ("Value in stock:   ");
          Put (Currency'(Get (Inv)), 1, 2, 0);
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
         (Inventory_Chk);

       procedure Check (TC : Test_Case_Index) is
          Inv     : Inventory;
          Success : Boolean;
          ID      : Item_ID;

          --  Please ignore the following three lines!
          pragma Warnings (Off, "default initialization");
          for Inv'Address use F'Address;
          pragma Warnings (On, "default initialization");

          procedure Init_Check_Data is
          begin
             Add (Inv,
                  Init ("Ballpoint Pen", 0.15),
                  ID);

             Set (Inv      => Inv,
                  Trans    => Bought,
                  ID       => ID,
                  Quantity => 10,
                  Success  => Success);

             Set (Inv      => Inv,
                  Trans    => Sold,
                  ID       => ID,
                  Quantity => 2,
                  Success  => Success);

             Set (Inv      => Inv,
                  Trans    => Sold,
                  ID       => ID,
                  Quantity => 2,
                  Success  => Success);

             Add (Inv,
                  Init ("Oil-based Pen Marker", 9.0),
                  ID);

             Add (Inv,
                  Init ("Feather Quill Pen", 15.0),
                  ID);

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

       begin
          Init_Check_Data;

          case TC is
          when Inventory_Chk =>
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

List of unique integers
-----------------------

In this exercise, you'll remove duplicates from an array of integer values and
order the elements. For example, if the input array contains :ada:`(7, 7, 1)`,
the function should return :ada:`(1, 7)`. This can easily be done by using
sets from the :ada:`Ordered_Sets` package.

Your goal is to implement the :ada:`Get_Unique` functions of the :ada:`Ops`
package in two versions:

    - one that returns a set; and

    - one that returns an array of integer values.

.. code:: ada lab=Standard_Library.List_of_Unique_Integers

    --  START LAB IO BLOCK
    in 0:Get_Unique_Set_Chk 5 6 3 3 5 2
    out 0: 2  3  5  6
    in 1:Get_Unique_Set_Chk 5 6 3 3 5 2 3 5 5 8 6 2 3 2
    out 1: 2  3  5  6  8
    in 2:Get_Unique_Array_Chk 5 6 3 3 5 2
    out 2: 2  3  5  6
    in 3:Get_Unique_Array_Chk 5 6 3 3 5 2 3 5 5 8 6 2 3 2
    out 3: 2  3  5  6  8
    --  END LAB IO BLOCK

    with Ada.Containers.Ordered_Sets;

    package Ops is

       --  type Int_Array is ...

       --  package Integer_Sets is ...

       subtype Int_Set is Integer_Sets.Set;

       function Get_Unique (A : Int_Array) return Int_Set;

       function Get_Unique (A : Int_Array) return Int_Array;

    end Ops;

    package body Ops is

       function Get_Unique (A : Int_Array) return Int_Set is
       begin
          null;
       end Get_Unique;

       function Get_Unique (A : Int_Array) return Int_Array is
       begin
          null;
       end Get_Unique;

    end Ops;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;

    with Ops;                     use Ops;

    procedure Main is
       type Test_Case_Index is
         (Get_Unique_Set_Chk,
          Get_Unique_Array_Chk);

       procedure Check (TC : Test_Case_Index;
                        A  : Int_Array) is

          procedure Display_Unique_Set (A : Int_Array) is
             S  : constant Int_Set := Get_Unique (A);
          begin
             for E of S loop
                Put_Line (Integer'Image (E));
             end loop;
          end Display_Unique_Set;

          procedure Display_Unique_Array (A : Int_Array) is
             AU : constant Int_Array := Get_Unique (A);
          begin
             for E of AU loop
                Put_Line (Integer'Image (E));
             end loop;
          end Display_Unique_Array;

       begin
          case TC is
             when Get_Unique_Set_Chk   => Display_Unique_Set (A);
             when Get_Unique_Array_Chk => Display_Unique_Array (A);
          end case;
       end Check;

    begin
       if Argument_Count < 3 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       else
          declare
             A : Int_Array (1 .. Argument_Count - 1);
          begin
             for I in A'Range loop
                A (I) := Integer'Value (Argument (1 + I));
             end loop;
             Check (Test_Case_Index'Value (Argument (1)), A);
          end;
       end if;
    end Main;

