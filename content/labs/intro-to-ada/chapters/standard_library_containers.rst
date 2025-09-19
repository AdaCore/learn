Standard library: Containers
============================

.. include:: ../../../global.txt

Simple todo list
----------------

**Goal**: implement a simple to-do list system using vectors.

**Steps**:

    #. Implement the :ada:`Todo_Lists` package.

        #. Declare the :ada:`Todo_Item` type.

        #. Declare the :ada:`Todo_List` type.

        #. Implement the :ada:`Add` procedure.

        #. Implement the :ada:`Display` procedure.

    #. :ada:`Todo_Item` type is used to store to-do items.

        #. It should be implemented as an access type to strings.

    #. :ada:`Todo_List` type is the container for all to-do items.

        #. It should be implemented as a **vector**.

    #. Procedure :ada:`Add` adds items (of :ada:`Todo_Item` type) to the list
       (of :ada:`Todo_List` type).

        #. This requires allocating a string for the access type.

    #. Procedure :ada:`Display` is used to display all to-do items.

        #. It must display one item per line.

**Remarks**:

    #. This exercise is based on the *Simple todo list* exercise from the
       :doc:`./more_about_types`.

        #. Your goal is to rewrite that exercise using vectors instead of
           arrays.

        #. You may reuse the code you've already implemented as a starting
           point.

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

List of unique integers
-----------------------

**Goal**: create function that removes duplicates from and orders a collection
of elements.

**Steps**:

    #. Implement package :ada:`Ops`.

        #. Declare the :ada:`Int_Array` type.

        #. Declare the :ada:`Integer_Sets` type.

        #. Implement the :ada:`Get_Unique` function that returns a set.

        #. Implement the :ada:`Get_Unique` function that returns an array of
           integer values.

**Requirements**:

    #. The :ada:`Int_Array` type is an unconstrained array of positive range.

    #. The :ada:`Integer_Sets` package is an instantiation of the
       :ada:`Ordered_Sets` package for the :ada:`Integer` type.

    #. The :ada:`Get_Unique` function must remove duplicates from an input
       array of integer values and order the elements.

        #. For example:

            - if the input array contains :ada:`(7, 7, 1)`

            - the function must return :ada:`(1, 7)`.

        #. You must implement this function by using sets from the
           :ada:`Ordered_Sets` package.

        #. :ada:`Get_Unique` must be implemented in two versions:

            - one version that returns a set |mdash| :ada:`Set` type from the
              :ada:`Ordered_Sets` package.

            - one version that returns an array of integer values |mdash|
              :ada:`Int_Array` type.

**Remarks**:

    #. Sets |mdash| as the one found in the generic :ada:`Ordered_Sets` package
       |mdash| are useful for quickly and easily creating an algorithm that
       removes duplicates from a list of elements.

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
