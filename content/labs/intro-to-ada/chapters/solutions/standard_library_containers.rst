Standard library: Containers
----------------------------

Simple todo list
~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Standard_Library.Simple_Todo_List

    --  START LAB IO BLOCK
    in 0:Todo_List_Chk
    out 0:TO-DO LIST Buy milk Buy tea Buy present Buy tickets Pay electricity bill Schedule dentist appointment Call sister Revise spreasheet Edit entry page Select new design Create upgrade plan
    --  END LAB IO BLOCK

    with Ada.Containers.Vectors;

    package Todo_Lists is

       type Todo_Item is access String;

       package Todo_List_Pkg is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Todo_Item);

       subtype Todo_List is Todo_List_Pkg.Vector;

       procedure Add (Todos : in out Todo_List;
                      Item  : String);

       procedure Display (Todos : Todo_List);

    end Todo_Lists;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Todo_Lists is

       procedure Add (Todos : in out Todo_List;
                      Item  : String) is
       begin
          Todos.Append (new String'(Item));
       end Add;

       procedure Display (Todos : Todo_List) is
       begin
          Put_Line ("TO-DO LIST");
          for T of Todos loop
             Put_Line (T.all);
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

List of unique integers
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Standard_Library.List_of_Unique_Integers

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

       type Int_Array is array (Positive range <>) of Integer;

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

       subtype Int_Set is Integer_Sets.Set;

       function Get_Unique (A : Int_Array) return Int_Set;

       function Get_Unique (A : Int_Array) return Int_Array;

    end Ops;

    package body Ops is

       function Get_Unique (A : Int_Array) return Int_Set is
          S : Int_Set;
       begin
          for E of A loop
             S.Include (E);
          end loop;

          return S;
       end Get_Unique;

       function Get_Unique (A : Int_Array) return Int_Array is
          S  : constant Int_Set := Get_Unique (A);
          AR : Int_Array (1 .. Positive (S.Length));
          I  : Positive := 1;
       begin
          for E of S loop
             AR (I) := E;
             I := I + 1;
          end loop;

          return AR;
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
