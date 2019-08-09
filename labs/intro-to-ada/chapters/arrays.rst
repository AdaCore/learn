:orphan:

Arrays
======

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Constrained Array
-----------------

In this exercise, you'll work with constrained arrays. In this case, your
array type will be limited to 10 elements of :ada:`Integer` type. These are
your goals:

- Declare the range type :ada:`My_Index` for the 10 elements of the array.

- Declare the array type :ada:`My_Array` using the :ada:`My_Index` type.

- Declare and implement an :ada:`Init` function that returns an array
  where each element is initialize with the corresponding index.

- Declare and implement a :ada:`Double` procedure that doubles the value of
  each element of an array.

- Declare and implement a function called :ada:`First_Elem` that returns
  the first element of the array.

- Declare and implement a function called :ada:`Last_Elem` that returns
  the last element of the array.

- Declare and implement a :ada:`Length` function, which returns the length
  of the array.

- Finally, declare an object :ada:`A` of :ada:`My_Array`  type and
  initialized it with the values 1 and 2 for the first two elements, and
  42 for all other elements.

.. code:: ada lab=Arrays.ConstrainedArray

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

.. todo::

    Add link to "records" labs as soon as they are available!

This exercise is based on the HTML colors exercise from a previous lab
(:doc:`./records`). In that exercise, one of the goals was to write the
function :ada:`To_RGB` to convert from the :ada:`HTML_Color` type to the
:ada:`RGB` type. These were the values for the colors:

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

You probably used a :ada:`case` statement to implement the :ada:`To_RGB`
function in that exercise. Now, in  this exercise, you'll rewrite the
function using a look-up table, which is implemented as an array of
constant values. In order to do that, you will:

- Declare the array type :ada:`HTML_Color_RGB` for the table.

- Declare the actual table as an object of :ada:`HTML_Color_RGB` type and
  initialize it.

.. code:: ada lab=Arrays.ColorsLookupTable

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
       --  To_RGB_Loopup_Table : ...

    end Color_Types;

    with Ada.Integer_Text_IO;
    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          --  Implement To_RGB using To_RGB_Loopup_Table
          return (0, 0, 0);
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
                          & Image (To_RGB_Loopup_Table (Firebrick)));
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

In this exercise, you'll work with unconstrained arrays of :ada:`Integer`
type. Your goals are:

- Declare an unconstrained array type of Integer called :ada:`My_Array`
  using a :ada:`Positive` range.

- Declare and implement a procedure :ada:`Init` where each element is
  initialized with the index starting with the last one. For example,
  for an array  of 3 elements, the values of these elements after a call
  to :ada:`Init` are :ada:`(3, 2, 1)`.

  - Hint: For an array :ada:`A`, you can retrieve the index of the last
    element with the attribute :ada:`'Last`. For example:
    :ada:`Y : Positive := A'Last;`

- Declare and implement an :ada:`Init` function that returns an array
  based on the length :ada:`L` provided to the :ada:`Init` function. This
  is the declaration: :ada:`function Init (L : Positive) return My_Array;`.
  Also, you must initialize the elements of the array in the same manner
  as for the :ada:`Init` procedure described above.

  - Hint: The easiest way to implement this function is by simply calling
    the :ada:`Init` procedure to initialize the elements. By doing this,
    you avoid code duplication.

- Declare and implement a :ada:`Double` procedure that doubles each element
  of an array.

- Declare and implement the :ada:`Diff_Prev_Elem` function, which returns
  |mdash| for each element of an input array :ada:`A` |mdash| an array
  with the difference between an element of array :ada:`A` and the
  previous element. In this case, the difference is zero for the first
  element.

  - For example:

    - **INPUT**: :ada:`(2, 5, 15)`

    - **RETURN** of :ada:`Diff_Prev_Elem`: :ada:`(0, 3, 10)`, where

      - :ada:`0`: constant difference for first element;
      - :ada:`3 = 5 - 2`: difference between the second and the first
        elements of the input array;
      - :ada:`10 = 15 - 5`: difference between the third and the second
        elements of the input array.

Just as a hint: you can use the range attribute (:ada:`A'Range`) to
retrieve the range of an array :ada:`A`. You can also use this attribute
in the declaration of another array (e.g.: :ada:`B : My_Array (A'Range)`).
Alternatively, you can use the :ada:`A'First` and :ada:`A'Last`
attributes.

.. code:: ada lab=Arrays.UnconstrainedArray

    --  START LAB IO BLOCK
    in 0:Init_Chk
    out 0: 5  4  3  2  1
    in 1:Init_Proc_Chk
    out 1: 5  4  3  2  1
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

       function Init (L : Positive) return My_Array;

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
             AA := Init (AA'Last);
             Display (AA);
          when Init_Proc_Chk =>
             Init (AA);
             Display (AA);
          when Double_Chk =>
             Local_Init (AA);
             Double (AA);
             Display (AA);
          when Diff_Prev_Chk =>
             Local_Init (AA);
             AA := Diff_Prev_Elem (AA);
             Display (AA);
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

Quantities And Amounts
----------------------

In this exercise, you'll create a system to keep track of quantities and
prices of products. In this system, the quantity of an individual product
is represented by the :ada:`Quantity` subtype and the price (or amount)
by the :ada:`Amount` subtype. You'll declare the array types
:ada:`Quantities` and :ada:`Amounts` to deal with information for various
products. In addition, you'll implement the following subprograms:

- :ada:`procedure Total`: given an input array of quantities and an input
  array of amounts for each product, it outputs an array with the total
  amount for each product using the :ada:`Amounts` type. The total amount
  for an individual product is calculated by multiplying the quantity for
  this product by its price.

- :ada:`function Total`: it has the same purpose as the procedure we've
  just mentioned. The difference is that the function returns an array of
  :ada:`Amounts` type instead of providing this array as an output
  parameter.

- :ada:`function Total`: given an array of quantities and an array of
  amounts for each product, it returns a single value of :ada:`Amount` type
  corresponding to the total amount for all products in the system. In
  other words, this function returns the sum of all total amounts for the
  individual products.

Hint: you can use :ada:`Amount (Q)` to convert from an element :ada:`Q` of
:ada:`Quantity` type to the :ada:`Amount` type. As you might remember, Ada
requires an explicit conversion in calculations where variables of both
integer and floating-point types are used. In our case, the :ada:`Quantity`
subtype is based on the :ada:`Integer` type and the :ada:`Amount` subtype
is based on the :ada:`Float` type, so a conversion is necessary in
calculations using those types.

.. code:: ada lab=Arrays.QuantitiesAndAmounts

    --  START LAB IO BLOCK
    in 0:Total_Func_Chk
    out 0:0.50 20.00 200.00 100.00 200.00
    in 1:Total_Proc_Chk
    out 1:0.50 20.00 200.00 100.00 200.00
    in 2:Total_Amount_Chk
    out 2:520.50
    --  END LAB IO BLOCK

    package Quantities_Amounts is

       subtype Quantity is Natural;

       subtype Amount is Float;

       --  Complete the type declarations:
       --
       --  type Quantities is ...
       --
       --  type Amounts is ...

       procedure Total (Q     : Quantities;
                        A     : Amounts;
                        A_Out : out Amounts);

       function Total (Q : Quantities;
                       A : Amounts) return Amounts;

       function Total (Q : Quantities;
                       A : Amounts) return Amount;

    end Quantities_Amounts;

    package body Quantities_Amounts is

       --  Complete the subprogram implementations:
       --

       --  procedure Total (Q     : Quantities;
       --                   A     : Amounts;
       --                   A_Out : out Amounts) is...

       --  function Total (Q : Quantities;
       --                  A : Amounts) return Amounts is...

       --  function Total (Q : Quantities;
       --                  A : Amounts) return Amount is ...

    end Quantities_Amounts;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Quantities_Amounts; use Quantities_Amounts;

    procedure Main is
       package Amount_IO is new Ada.Text_IO.Float_IO (Amount);

       type Test_Case_Index is
         (Total_Func_Chk,
          Total_Proc_Chk,
          Total_Amount_Chk);

       procedure Check (TC : Test_Case_Index) is
          subtype Test_Range is Positive range 1 .. 5;

          A  : Amounts (Test_Range);
          Q  : Quantities (Test_Range);
          A1 : Amount;

          procedure Display (A : Amounts) is
          begin
             for I in A'Range loop
                Amount_IO.Put (A (I));
                New_Line;
             end loop;
          end Display;

          procedure Local_Init (Q : in out Quantities;
                                A : in out Amounts) is
          begin
             Q := (1,    2,    5,   10,   10);
             A := (0.5, 10.0, 40.0, 10.0, 20.0);
          end Local_Init;

       begin
          Amount_IO.Default_Fore := 1;
          Amount_IO.Default_Aft  := 2;
          Amount_IO.Default_Exp  := 0;

          case TC is
          when Total_Func_Chk =>
             Local_Init (Q, A);
             A := Total (Q, A);
             Display (A);
          when Total_Proc_Chk =>
             Local_Init (Q, A);
             Total (Q, A, A);
             Display (A);
          when Total_Amount_Chk =>
             Local_Init (Q, A);
             A1 := Total (Q, A);
             Amount_IO.Put (A1);
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

As you know, the :ada:`String` type is an unconstrained array. In this
exercise, you'll deal with the :ada:`String` type and a custom
constrained string type called :ada:`String_10`. Your goals are:

- Declare the constrained string type :ada:`String_10` as an array of
  ten characters.

  - Hint: declaring :ada:`String_10` as a subtype of :ada:`String` is the
    easiest way. You may declare it as a new type as well. However, this
    requires some adaptations for the :ada:`Main` test procedure.

- Implement the :ada:`To_String_10` function to create constrained
  strings of :ada:`String_10` type based on an input parameter of
  :ada:`String` type.

.. code:: ada lab=Arrays.String10

    --  START LAB IO BLOCK
    in 0:String_10_Chk
    out 0:And this i
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
         (String_10_Chk);

       procedure Check (TC : Test_Case_Index) is
          S    : constant String := "And this is a long string just for testing...";
          S_10 : String_10;

       begin
          case TC is
          when String_10_Chk =>
             S_10 := To_String_10 (S);
             Put_Line (S_10);
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

In this exercise, you'll create a system that consists of a list of names
and ages. In this system, each person is represented by the :ada:`Person`
type, which is a record containing the name and the age of that person.
Your goals are:

- Declare the :ada:`People_Array` array type as an unconstrained array of
  positive range.

- Complete the declaration of the :ada:`People` record type with the
  :ada:`People_A` element of :ada:`People_Array` type. This array must be
  constrained to 10 elements using the :ada:`Max_People` constant.

- Implement the procedure :ada:`Add` to add a person to the list. By
  default, the age of this person is set to zero in this procedure.

  - Hint: You may use an index to indicate the last valid position in the
    array. See :ada:`Last_Valid` in the code below.

- Implement the procedure :ada:`Reset` to reset the list.

- Implement the function :ada:`Get` to retrieve the age of a person from
  the list.

- Implement the procedure :ada:`Update` to update the age of a person from
  the list.

- Implement the procedure :ada:`Display` to show the complete list by
  using the following format:

  - The first line must be ``LIST OF NAMES:`` followed by the name and
    age of each person in the next lines.

  - For each person on the list, the procedure must diplay the information
    in the following format:

    .. code-block:: none

      NAME: XXXX
      AGE: YY

  - Hint: You should use the :ada:`Trim` function from the
    :ada:`Ada.Strings.Fixed` package to format the person's name. For
    example: :ada:`Trim (P.Name, Right)`.

These are other hints that could be useful:

- You may need the :ada:`Integer'Min (A, B)` and the
  :ada:`Integer'Max (A, B)` functions to get the minimum and maximum values
  in a comparison between two integer values :ada:`A` and :ada:`B`.

- Fixed-length strings can be initialized with whitespaces using
  the :ada:`others` syntax. For example:
  :ada:`S : String_10 := (others => ' ');`

- You may implement additional subprograms to deal with other types declared
  in the :ada:`Names_Ages` package below, such as the :ada:`Name_Type` and
  the :ada:`Person` type. For example, a function :ada:`To_Name_Type`
  to convert from :ada:`String` to :ada:`Name_Type` might be useful. Take
  a moment to reflect on which other subprograms could be useful as well.

.. code:: ada lab=Arrays.ListOfNames

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

