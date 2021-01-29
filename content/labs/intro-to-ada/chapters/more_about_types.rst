More About Types
================

.. include:: ../../../courses/global.txt

Aggregate Initialization
------------------------

**Goal**: initialize records and arrays using aggregates.

**Steps**:

    #. Implement the :ada:`Aggregates` package.

        #. Create the record type :ada:`Rec`.

        #. Create the array type :ada:`Int_Arr`.

        #. Implement the :ada:`Init` procedure that outputs a record of
           :ada:`Rec` type.

        #. Implement the :ada:`Init_Some` procedure.

        #. Implement the :ada:`Init` procedure that outputs an array of
           :ada:`Int_Arr` type.

**Requirements**:

    #. Record type :ada:`Rec` has four components of :ada:`Integer` type. These
       are the components with the corresponding default values:

        - :ada:`W` = 10
        - :ada:`X` = 11
        - :ada:`Y` = 12
        - :ada:`Z` = 13

    #. Array type :ada:`Int_Arr` has 20 elements of :ada:`Integer` type (with
       indices ranging from 1 to 20).

    #. The first :ada:`Init` procedure outputs a record of :ada:`Rec` type
       where:

        #. :ada:`X` is initialized with 100,
        #. :ada:`Y` is initialized with 200, and
        #. the remaining elements use their default values.

    #. Procedure :ada:`Init_Some` outputs an array of :ada:`Int_Arr` type
       where:

        #. the first five elements are initialized with the value 99, and
        #. the remaining elements are initialized with the value 100.

    #. The second :ada:`Init` procedure outputs an array of :ada:`Int_Arr` type
       where:

        #. all elements are initialized with the value 5.

.. code:: ada lab=More_About_Types.Aggregate_Initialization

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

       --  type Rec is ...;

       --  type Int_Arr is ...;

       procedure Init;

       --  procedure Init_Some ...;

       --  procedure Init ...;

    end Aggregates;

    package body Aggregates is

       procedure Init is null;

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
----------

**Goal**: implement a simple package for source-code versioning.

**Steps**:

    #. Implement the :ada:`Versioning` package.

        #. Declare the record type :ada:`Version`.

        #. Implement the :ada:`Convert` function that returns a string.

        #. Implement the :ada:`Convert` function that returns a floating-point
           number.

**Requirements**:

    #. Record type :ada:`Version` has the following components of
       :ada:`Natural` type:

        #. :ada:`Major`,
        #. :ada:`Minor`, and
        #. :ada:`Maintenance`.

    #. The first :ada:`Convert` function returns a string containing the
       version number.

    #. The second :ada:`Convert` function returns a floating-point value.

        #. For this floating-point value:

            #. the number before the decimal point must correspond to the major
               number, and

            #. the number after the decimal point must correspond to the minor
               number.

            #. the maintenance number is ignored.

        #. For example, version "1.3.5" is converted to the floating-point
           value ``1.3``.

        #. An obvious limitation of this function is that it can only handle
           one-digit numbers for the minor component.

            - For example, we cannot convert version "1.10.0" to a reasonable
              value with the approach described above. The result of the call
              :ada:`Convert ((1, 10, 0))` is therefore unspecified.

            - For the scope of this exercise, only version numbers with
              one-digit components are checked.

**Remarks**:

    #. We use overloading for the :ada:`Convert` functions.

    #. For the function :ada:`Convert` that returns a string, you can make use
       of the :ada:`Image_Trim` function, as indicated in the source-code below
       |mdash| see package body of :ada:`Versioning`.

.. code:: ada lab=Solutions.More_About_Types.Versioning

    --  START LAB IO BLOCK
    in 0:Ver_String_Chk
    out 0:1.3.23
    in 1:Ver_Float_Chk
    out 1: 1.30000E+00
    --  END LAB IO BLOCK

    package Versioning is

       --  type Version is record...

       --  function Convert ...

       --  function Convert

    end Versioning;

    with Ada.Strings; use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;

    package body Versioning is

       function Image_Trim (N : Natural) return String is
          S_N : constant String := Trim (Natural'Image (N), Left);
       begin
          return S_N;
       end Image_Trim;

       --  function Convert ...
       --     S_Major : constant String := Image_Trim (V.Major);
       --     S_Minor : constant String := Image_Trim (V.Minor);
       --     S_Maint : constant String := Image_Trim (V.Maintenance);
       --  begin
       --  end Convert;

       --  function Convert ...
       --  begin
       --  end Convert;

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
----------------

**Goal**: implement a simple to-do list system.

**Steps**:

    #. Implement the :ada:`Todo_Lists` package.

        #. Declare the :ada:`Todo_Item` type.

        #. Declare the :ada:`Todo_List` type.

        #. Implement the :ada:`Add` procedure.

        #. Implement the :ada:`Display` procedure.

**Requirements**:

    #. :ada:`Todo_Item` type is used to store a to-do item.

        #. It should be implemented as an access type to strings.

    #. :ada:`Todo_Items` type is an array of to-do items.

        #. It should be implemented as an unconstrained array with positive
           range.

    #. :ada:`Todo_List` type is the container for all to-do items.

        #. It should be declared as a record with discriminant.

        #. In order to store the to-do items, it must contain a component named
           :ada:`Items` of :ada:`Todo_Items` type.

        #. Don't forget to keep track of the last element added to the
           list!

            - You should declare a :ada:`Last` component in the record.

    #. Procedure :ada:`Add` adds items (of :ada:`Todo_Item` type) to the list
       (of :ada:`Todo_List` type).

        #. This requires allocating a string for the access type.

        #. An item can only be added to the list if the list isn't full yet
           |mdash| see next point for details on error handling.

    #. Since the number of items that can be stored on the list is limited,
       the list might eventually become full in a call to :ada:`Add`.

        #. You must write code in the implementation of the :ada:`Add`
           procedure that verifies this condition.

        #. If the procedure detects that the list is full, it must display the
           following message: "ERROR: list is full!".

    #. Procedure :ada:`Display` is used to display all to-do items.

        #. It must display one item per line.

**Remarks**:

    #. We use access types and unconstrained arrays in the implementation of
       the :ada:`Todo_Lists` package.

.. code:: ada lab=Solutions.More_About_Types.Simple_Todo_List

    --  START LAB IO BLOCK
    in 0:Todo_List_Chk
    out 0:ERROR: list is full! TO-DO LIST Buy milk Buy tea Buy present Buy tickets Pay electricity bill Schedule dentist appointment Call sister Revise spreasheet Edit entry page Select new design
    --  END LAB IO BLOCK

    package Todo_Lists is

       --  Replace by actual type declaration
       type Todo_Item is null record;

       --  Replace by actual type declaration
       type Todo_Items is null record;

       --  Replace by actual type declaration
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
          Put_Line ("ERROR: list is full!");
       end Add;

       procedure Display (Todos : Todo_List) is
       begin
          null;
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
----------

**Goal**: implement a list containing prices

**Steps**:

    #. Implement the :ada:`Price_Lists` package.

        #. Declare the :ada:`Price_Type` type.

        #. Declare the :ada:`Price_List` record.

        #. Implement the :ada:`Reset` procedure.

        #. Implement the :ada:`Add` procedure.

        #. Implement the :ada:`Get` function.

        #. Implement the :ada:`Display` procedure.

**Requirements**:

    #. :ada:`Price_Type` is a decimal fixed-point data type with a delta
       of two digits (e.g. ``0.01``) and twelve digits in total.

    #. :ada:`Price_List` is a record type that contains the price list.

        #. This record type must have a discriminant for the maximum number of
           elements of the list.

    #. Procedure :ada:`Reset` resets the list.

    #. Procedure :ada:`Add` adds a price to the list.

        #. You should keep track of the last element added to the list.

    #. Function :ada:`Get` retrieves a price from the list using an index.

        #. This function returns a record instance of :ada:`Price_Result` type.

        #. :ada:`Price_Result` is a variant record containing:

            #. the Boolean component :ada:`Ok`, and
            #. the component :ada:`Price` (of :ada:`Price_Type`).

        #. The returned value of :ada:`Price_Result` type is one of the
           following:

            #. If the index specified in a call to :ada:`Get` contains a valid
               (initialized) price, then

                - :ada:`Ok` is set to :ada:`True`, and

                - the :ada:`Price` component contains the price for that index.

            #. Otherwise:

                - :ada:`Ok` is set to :ada:`False`, and

                - the :ada:`Price` component is not available.

    #. Procedure :ada:`Display` shows all prices from the list.

        #. The header (first line) must be :ada:`PRICE LIST`.

        #. The remaining lines contain one price per line.

        #. For example:

            - For the following code:

                .. code-block:: ada

                    procedure Test is
                       L : Price_List (10);
                    begin
                       Reset (L);
                       Add (L, 1.45);
                       Add (L, 2.37);
                       Display (L);
                    end Test;

            - The output is:

                .. code-block:: none

                    PRICE LIST
                     1.45
                     2.37

**Remarks**:

#. To implement the package, you'll use the following features of the Ada
   language:

    #. decimal fixed-point types;
    #. records with discriminants;
    #. dynamically-sized record types;
    #. variant records.

#. For record type :ada:`Price_List`, you may use an unconstrained array as a
   component of the record and use the discriminant in the component
   declaration.

.. code:: ada lab=More_About_Types.Price_List

    --  START LAB IO BLOCK
    in 0:Price_Type_Chk
    out 0:The delta    value of Price_Type is  0.01; The minimum  value of Price_Type is -9999999999.99; The maximum  value of Price_Type is  9999999999.99;
    in 1:Price_List_Chk
    out 1:PRICE LIST  1.45  2.37  3.21  4.14  5.22  6.69  7.77  8.14  9.99  10.01
    in 2:Price_List_Get_Chk
    out 2:Attempt Get #  5 Element #  5 =>  5.22 Attempt Get #  40 Element not available (as expected)
    --  END LAB IO BLOCK

    package Price_Lists is

       --  Replace by actual type declaration
       type Price_Type is new Float;

       --  Replace by actual type declaration
       type Price_List is null record;

       --  Replace by actual type declaration
       type Price_Result is null record;

       procedure Reset (Prices : in out Price_List);

       procedure Add (Prices : in out Price_List;
                      Item   : Price_Type);

       function Get (Prices : Price_List;
                     Idx    : Positive) return Price_Result;

       procedure Display (Prices : Price_List);

    end Price_Lists;

    package body Price_Lists is

       procedure Reset (Prices : in out Price_List) is
       begin
          null;
       end Reset;

       procedure Add (Prices : in out Price_List;
                      Item   : Price_Type) is
       begin
          null;
       end Add;

       function Get (Prices : Price_List;
                     Idx    : Positive) return Price_Result is
       begin
          null;
       end Get;

       procedure Display (Prices : Price_List) is
       begin
          null;
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
             Put_Line ("Attempt Get # " & Positive'Image (Idx));
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
