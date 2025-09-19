Arrays
======

.. include:: ../../../global.txt

Constrained Array
-----------------

**Goal**: declare a constrained array and implement operations on it.

**Steps**:

    #. Implement the :ada:`Constrained_Arrays` package.

        #. Declare the range type :ada:`My_Index`.

        #. Declare the array type :ada:`My_Array`.

        #. Declare and implement the :ada:`Init` function.

        #. Declare and implement the :ada:`Double` procedure.

        #. Declare and implement the :ada:`First_Elem` function.

        #. Declare and implement the :ada:`Last_Elem` function.

        #. Declare and implement the :ada:`Length` function.

        #. Declare the object :ada:`A` of :ada:`My_Array` type.

**Requirements**:

    #. Range type :ada:`My_Index` has a range from 1 to 10.

    #. :ada:`My_Array` is a constrained array of :ada:`Integer` type.

        #. It must make use of the :ada:`My_Index` type.

        #. It is therefore limited to 10 elements.

    #. Function :ada:`Init` returns an array where each element is initialized
       with the corresponding index.

    #. Procedure :ada:`Double` doubles the value of each element of an array.

    #. Function :ada:`First_Elem` returns the first element of the array.

    #. Function :ada:`Last_Elem` returns the last element of the array.

    #. Function :ada:`Length` returns the length of the array.

    #. Object :ada:`A` of :ada:`My_Array`  type is initialized with:

        #. the values 1 and 2 for the first two elements, and

        #. 42 for all other elements.

.. code:: ada lab=Arrays.Constrained_Array

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

       --  Complete the type and subprogram declarations:
       --
       --  type My_Index is [...]
       --
       --  type My_Array is [...]
       --
       --  function Init ...
       --
       --  procedure Double ...
       --
       --  function First_Elem ...
       --
       --  function Last_Elem ...
       --
       --  function Length ...
       --
       --  A : ...

    end Constrained_Arrays;

    package body Constrained_Arrays is

       --  Create the implementation of the subprograms!
       --

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
--------------------

**Goal**: rewrite a package to represent HTML colors in RGB format using a
lookup table.

**Steps**:

    #. Implement the :ada:`Color_Types` package.

        #. Declare the array type :ada:`HTML_Color_RGB`.

        #. Declare the :ada:`To_RGB_Lookup_Table` object and initialize it.

        #. Adapt the implementation of the :ada:`To_RGB` function.

**Requirements**:

    #. Array type :ada:`HTML_Color_RGB` is used for the table.

    #. The :ada:`To_RGB_Lookup_Table` object of :ada:`HTML_Color_RGB` type
       contains the lookup table.

        - This table must be implemented as an array of constant values.

    #. The implementation of the :ada:`To_RGB` function must use the
       :ada:`To_RGB_Lookup_Table` object.

**Remarks**:

    #. This exercise is based on the HTML colors exercise from a previous lab
       (:doc:`./records`).

    #. In the previous implementation, you could use a :ada:`case` statement to
       implement the :ada:`To_RGB` function. Here, you must rewrite the
       function using a look-up table.

       #. The implementation of the :ada:`To_RGB` function below includes the
          case statement as commented-out code. You can use this as your
          starting point: you just need to copy it and convert the case
          statement to an array declaration.

        #. Don't use a case statement to implement the :ada:`To_RGB` function.
           Instead, write code that accesses :ada:`To_RGB_Lookup_Table` to get
           the correct value.

    #. The following table contains the HTML colors and the corresponding value
       in hexadecimal form for each color element:

        +-------------+---------+---------+---------+
        | Color       | Red     | Green   | Blue    |
        +=============+=========+=========+=========+
        | Salmon      | ``#FA`` | ``#80`` | ``#72`` |
        +-------------+---------+---------+---------+
        | Firebrick   | ``#B2`` | ``#22`` | ``#22`` |
        +-------------+---------+---------+---------+
        | Red         | ``#FF`` | ``#00`` | ``#00`` |
        +-------------+---------+---------+---------+
        | Darkred     | ``#8B`` | ``#00`` | ``#00`` |
        +-------------+---------+---------+---------+
        | Lime        | ``#00`` | ``#FF`` | ``#00`` |
        +-------------+---------+---------+---------+
        | Forestgreen | ``#22`` | ``#8B`` | ``#22`` |
        +-------------+---------+---------+---------+
        | Green       | ``#00`` | ``#80`` | ``#00`` |
        +-------------+---------+---------+---------+
        | Darkgreen   | ``#00`` | ``#64`` | ``#00`` |
        +-------------+---------+---------+---------+
        | Blue        | ``#00`` | ``#00`` | ``#FF`` |
        +-------------+---------+---------+---------+
        | Mediumblue  | ``#00`` | ``#00`` | ``#CD`` |
        +-------------+---------+---------+---------+
        | Darkblue    | ``#00`` | ``#00`` | ``#8B`` |
        +-------------+---------+---------+---------+

.. code:: ada lab=Arrays.Colors_Lookup_Table

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

       --  Declare array type for lookup table here:
       --
       --  type HTML_Color_RGB is ...

       --  Declare lookup table here:
       --
       --  To_RGB_Lookup_Table : ...

    end Color_Types;

    with Ada.Integer_Text_IO;
    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          --  Implement To_RGB using To_RGB_Lookup_Table
          return (0, 0, 0);

          --  Use the code below from the previous version of the To_RGB
          --  function to declare the To_RGB_Lookup_Table:
          --
          --  case C is
          --     when Salmon      => return (16#FA#, 16#80#, 16#72#);
          --     when Firebrick   => return (16#B2#, 16#22#, 16#22#);
          --     when Red         => return (16#FF#, 16#00#, 16#00#);
          --     when Darkred     => return (16#8B#, 16#00#, 16#00#);
          --     when Lime        => return (16#00#, 16#FF#, 16#00#);
          --     when Forestgreen => return (16#22#, 16#8B#, 16#22#);
          --     when Green       => return (16#00#, 16#80#, 16#00#);
          --     when Darkgreen   => return (16#00#, 16#64#, 16#00#);
          --     when Blue        => return (16#00#, 16#00#, 16#FF#);
          --     when Mediumblue  => return (16#00#, 16#00#, 16#CD#);
          --     when Darkblue    => return (16#00#, 16#00#, 16#8B#);
          --  end case;

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
-------------------

**Goal**: declare an unconstrained array and implement operations on it.

**Steps**:

    #. Implement the :ada:`Unconstrained_Arrays` package.

        #. Declare the :ada:`My_Array` type.

        #. Declare and implement the :ada:`Init` procedure.

        #. Declare and implement the :ada:`Init` function.

        #. Declare and implement the :ada:`Double` procedure.

        #. Declare and implement the :ada:`Diff_Prev_Elem` function.

**Requirements**:

    #. :ada:`My_Array` is an unconstrained array (with a :ada:`Positive` range)
       of :ada:`Integer` elements.

    #. Procedure :ada:`Init` initializes each element with the index starting
       with the last one.

       - For example, for an array of 3 elements where the index of the first
         element is 1 (:ada:`My_Array (1 .. 3)`), the values of these elements
         after a call to :ada:`Init` must be :ada:`(3, 2, 1)`.

    #. Function :ada:`Init` returns an array based on the length :ada:`L` and
       start index :ada:`I` provided to the :ada:`Init` function.

        #. :ada:`I` indicates the index of the first element of the array.

        #. :ada:`L` indicates the length of the array.

        #. Both :ada:`I` and :ada:`L` must be positive.

        #. This is its declaration:
           :ada:`function Init (I, L : Positive) return My_Array;`.

        #. You must initialize the elements of the array in the same manner
           as for the :ada:`Init` procedure described above.

    #. Procedure :ada:`Double` doubles each element of an array.

    #. Function :ada:`Diff_Prev_Elem` returns |mdash| for each element of an
       input array :ada:`A` |mdash| an array with the difference between an
       element of array :ada:`A` and the previous element.

        #. For the first element, the difference must be zero.

        #. For example:

            - **INPUT**: :ada:`(2, 5, 15)`

            - **RETURN** of :ada:`Diff_Prev_Elem`: :ada:`(0, 3, 10)`, where

                - :ada:`0` is the constant difference for the first element;

                - :ada:`5 - 2 = 3` is the difference between the second and the
                  first elements of the input array;

                - :ada:`15 - 5 = 10` is the difference between the third and
                  the second elements of the input array.

**Remarks**:

#. For an array :ada:`A`, you can retrieve the index of the last element with
   the attribute :ada:`'Last`.

    #. For example: :ada:`Y : Positive := A'Last;`

    #. This can be useful during the implementation of procedure :ada:`Init`.

#. For the implementation of the :ada:`Init` function, you can call the
   :ada:`Init` procedure to initialize the elements. By doing this, you avoid
   code duplication.

#. Some hints about attributes:

    #. You can use the range attribute (:ada:`A'Range`) to retrieve the
       range of an array :ada:`A`.

    #. You can also use the range attribute in the declaration of another array
       (e.g.: :ada:`B : My_Array (A'Range)`).

    #. Alternatively, you can use the :ada:`A'First` and :ada:`A'Last`
       attributes in an array declaration.

.. code:: ada lab=Arrays.Unconstrained_Array

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

       --  Complete the type and subprogram declarations:
       --
       --  type My_Array is ...;
       --
       --  procedure Init ...;

       function Init (I, L : Positive) return My_Array;

       --  procedure Double ...;
       --
       --  function Diff_Prev_Elem ...;

    end Unconstrained_Arrays;

    package body Unconstrained_Arrays is

       --  Implement the subprograms:
       --

       --  procedure Init is...

       --  function Init (L : Positive) return My_Array is...

       --  procedure Double ... is...

       --  function Diff_Prev_Elem ... is...

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
------------

**Goal**: create a system to keep track of quantities and prices of products.

**Steps**:

    #. Implement the :ada:`Product_Info_Pkg` package.

        #. Declare the array type :ada:`Product_Infos`.

        #. Declare the array type :ada:`Currency_Array`.

        #. Implement the :ada:`Total` procedure.

        #. Implement the :ada:`Total` function returning an array of
           :ada:`Currency_Array` type.

        #. Implement the :ada:`Total` function returning a single value of
           :ada:`Currency` type.

**Requirements**:

    #. Quantity of an individual product is represented by the :ada:`Quantity`
       subtype.

    #. Price of an individual product is represented by the :ada:`Currency`
       subtype.

    #. Record type :ada:`Product_Info` deals with information for various
       products.

    #. Array type :ada:`Product_Infos` is used to represent a list of products.

    #. Array type :ada:`Currency_Array` is used to represent a list of total
       values of individual products (see more details below).

    #. Procedure :ada:`Total` receives an input array of products.

        #. It outputs an array with the total value of each product using the
           :ada:`Currency_Array` type.

        #. The total value of an individual product is calculated by
           multiplying the quantity for this product by its price.

    #. Function :ada:`Total` returns an array of :ada:`Currency_Array` type.

        #. This function has the same purpose as the procedure :ada:`Total`.

        #. The difference is that the function returns an array instead of
           providing this array as an output parameter.

    #. The second function :ada:`Total` returns a single value of
       :ada:`Currency` type.

        #. This function receives an array of products.

        #. It returns a single value corresponding to the total value for all
           products in the system.

**Remarks**:

    #. You can use :ada:`Currency (Q)` to convert from an element :ada:`Q` of
       :ada:`Quantity` type to the :ada:`Currency` type.

        #. As you might remember, Ada requires an explicit conversion in
           calculations where variables of both integer and floating-point
           types are used.

        #. In our case, the :ada:`Quantity` subtype is based on the
           :ada:`Integer` type and the :ada:`Currency` subtype is based on the
           :ada:`Float` type, so a conversion is necessary in calculations
           using those types.

.. code:: ada lab=Arrays.Product_Info

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

       --  Complete the type declarations:
       --
       --  type Product_Infos is ...
       --
       --  type Currency_Array is ...

       procedure Total (P   : Product_Infos;
                        Tot : out Currency_Array);

       function Total (P : Product_Infos) return Currency_Array;

       function Total (P : Product_Infos) return Currency;

    end Product_Info_Pkg;

    package body Product_Info_Pkg is

       --  Complete the subprogram implementations:
       --

       --  procedure Total (P   : Product_Infos;
       --                   Tot : out Currency_Array) is ...

       --  function Total (P : Product_Infos) return Currency_Array is ...

       --  function Total (P : Product_Infos) return Currency is ...

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
---------

**Goal**: work with constrained string types.

**Steps**:

    #. Implement the :ada:`Strings_10` package.

        #. Declare the :ada:`String_10` type.

        #. Implement the :ada:`To_String_10` function.

**Requirements**:

    #. The constrained string type :ada:`String_10` is an array of ten
       characters.

    #. Function :ada:`To_String_10` returns constrained strings of
       :ada:`String_10` type based on an input parameter of :ada:`String` type.

	   - For strings that are more than 10 characters, omit everything
	     after the 11th character.

	   - For strings that are fewer than 10 characters, pad the string
	     with ' ' characters until it is 10 characters.

**Remarks**:

    #. Declaring :ada:`String_10` as a subtype of :ada:`String` is the easiest
       way.

        - You may declare it as a new type as well. However, this requires some
          adaptations in the :ada:`Main` test procedure.

    #. You can use :ada:`Integer'Min` to calculate the minimum of two integer
       values.

.. code:: ada lab=Arrays.String_10

    --  START LAB IO BLOCK
    in 0:String_10_Long_Chk
    out 0:And this i
    in 1:String_10_Short_Chk
    out 1:Hey!
    --  END LAB IO BLOCK

    package Strings_10 is

       --  Complete the type and subprogram declarations:
       --

       --  subtype String_10 is ...;

       --  Using "type String_10 is..." is possible, too. However, it
       --  requires a custom Put_Line procedure that is called in Main:
       --  procedure Put_Line (S : String_10);

       --  function To_String_10 ...;

    end Strings_10;

    package body Strings_10 is

       --  Complete the subprogram declaration and implementation:
       --
       --  function To_String_10 ... is

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
-------------

**Goal**: create a system for a list of names and ages.

**Steps**:

    #. Implement the :ada:`Names_Ages` package.

        #. Declare the :ada:`People_Array` array type.

        #. Complete the declaration of the :ada:`People` record type with the
           :ada:`People_A` element of :ada:`People_Array` type.

        #. Implement the :ada:`Add` procedure.

        #. Implement the :ada:`Reset` procedure.

        #. Implement the :ada:`Get` function.

        #. Implement the :ada:`Update` procedure.

        #. Implement the :ada:`Display` procedure.

**Requirements**:

    #. Each person is represented by the :ada:`Person` type, which is a record
       containing the name and the age of that person.

    #. :ada:`People_Array` is an unconstrained array of :ada:`Person` type
       with a positive range.

    #. The :ada:`Max_People` constant is set to 10.

    #. Record type :ada:`People` contains:

        #. The :ada:`People_A` element of :ada:`People_Array` type.

        #. This array must be constrained by the :ada:`Max_People` constant.

    #. Procedure :ada:`Add` adds a person to the list.

        #. By default, the age of this person is set to zero in this procedure.

    #. Procedure :ada:`Reset` resets the list.

    #. Function :ada:`Get` retrieves the age of a person from the list.

    #. Procedure :ada:`Update` updates the age of a person in the list.

    #. Procedure :ada:`Display` shows the complete list using the following
       format:

        #. The first line must be ``LIST OF NAMES:``. It is followed by the
           name and age of each person in the next lines.

        #. For each person on the list, the procedure must display the
           information in the following format:

            .. code-block:: none

              NAME: XXXX
              AGE: YY

**Remarks**:

    #. In the implementation of procedure :ada:`Add`, you may use an index to
       indicate the last valid position in the array |mdash| see
       :ada:`Last_Valid` in the code below.

    #. In the implementation of procedure :ada:`Display`, you should use the
       :ada:`Trim` function from the :ada:`Ada.Strings.Fixed` package to format
       the person's name |mdash| for example: :ada:`Trim (P.Name, Right)`.

    #. You may need the :ada:`Integer'Min (A, B)` and the
       :ada:`Integer'Max (A, B)` functions to get the minimum and maximum
       values in a comparison between two integer values :ada:`A` and :ada:`B`.

    #. Fixed-length strings can be initialized with whitespaces using
       the :ada:`others` syntax. For example:
       :ada:`S : String_10 := (others => ' ');`

    #. You may implement additional subprograms to deal with other types
       declared in the :ada:`Names_Ages` package below, such as the
       :ada:`Name_Type` and the :ada:`Person` type.

        #. For example, a function :ada:`To_Name_Type` to convert from
           :ada:`String` to :ada:`Name_Type` might be useful.

        #. Take a moment to reflect on which additional subprograms could be
           useful as well.

.. code:: ada lab=Arrays.List_Of_Names

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

       --  Add type declaration for People_Array record:
       --
       --  type People_Array is ...;

       --  Replace type declaration for People record. You may use the
       --  following template:
       --
       --  type People is record
       --     People_A   : People_Array ...;
       --     Last_Valid : Natural;
       --  end record;
       --
       type People is null record;

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

       procedure Reset (P : in out People) is
       begin
          null;
       end Reset;

       procedure Add (P    : in out People;
                      Name :        String) is
       begin
          null;
       end Add;

       function Get (P    : People;
                     Name : String) return Age_Type is
       begin
          return 0;
       end Get;

       procedure Update (P    : in out People;
                         Name :        String;
                         Age  :        Age_Type) is
       begin
          null;
       end Update;

       procedure Display (P : People) is
       begin
          null;
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
