Generics
========

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Display Array
-------------

Your goal for this exercise is to create a generic procedure called
:ada:`Display_Array` that displays the elements of an array. This
procedure must first display a header and then the elements of the array.
When displaying the elements, it must use one line per element and include
the corresponding index of the array. This is the expected format:

.. code-block:: none

    <HEADER>
    <index #1>: <element #1>
    <index #2>: <element #2>
    ...

For example, for the following code:

.. code-block:: ada

    procedure Test is
       A : Int_Array (1 .. 2) := (1, 5);
    begin
       Display_Int_Array ("Elements of A", A);;
    end Test;

The output is:

.. code-block:: none

    Elements of A
     1:  1
     2:  5

These are the formal parameter of the procedure:

- a range type :ada:`T_Range` for the the array;

- a formal type :ada:`T_Element` for the elements of the array;

    - This type must be declared in such a way, so that it can be mapped
      to any type in the instantiation |mdash| including record types.

- an array type :ada:`T_Array` using :ada:`T_Range` and :ada:`T_Element`;

- a function :ada:`Image` that converts a variable of type :ada:`T_Element`
  to a :ada:`String`.

.. code:: ada lab=Generics.Display_Array

    --  START LAB IO BLOCK
    in 0:Int_Array_Chk
    out 0:Integers  1:  1  2:  2  3:  5  4:  7  5:  10
    in 1:Point_Array_Chk
    out 1:Points  0: ( 1.00000E+00,  5.00000E-01)  1: ( 2.00000E+00, -5.00000E-01)  2: ( 5.00000E+00,  2.00000E+00)  3: (-5.00000E-01,  2.00000E+00)
    --  END LAB IO BLOCK

    generic
    procedure Display_Array (Header : String;
                             A      : T_Array);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Array (Header : String;
                             A      : T_Array) is
    begin
       null;
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
-------------------------

In this exercise, you'll create a generic function :ada:`Average` that
calculates the average of an array containing floating-point values of
arbitrary precision. This function must contain the following formal
parameters:

- a range type :ada:`T_Range` for the array;

- a formal type :ada:`T_Element` that can be mapped to floating-point
  types of arbitrary precision;

- an array type :ada:`T_Array` using :ada:`T_Range` and :ada:`T_Element`;

.. code:: ada lab=Generics.Average_Array_Of_Float

    --  START LAB IO BLOCK
    in 0:Float_Array_Chk
    out 0:Average:  8.00000E-01
    in 1:Digits_12_Float_Array_Chk
    out 1:Average:  5.40000000000E+00
    --  END LAB IO BLOCK

    generic
    function Average (A : T_Array) return T_Element;

    function Average (A : T_Array) return T_Element is
    begin
       return 0.0;
    end Average;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Main is
       type Test_Case_Index is (Float_Array_Chk,
                                Digits_12_Float_Array_Chk);

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

       procedure Test_Digits_12_Float_Array is
          type Custom_Float is digits 12;

          type Float_Array is
            array (Integer range <>) of Custom_Float;

          function Average_Float is new
            Average (T_Range   => Integer,
                     T_Element => Custom_Float,
                     T_Array   => Float_Array);

          A : constant Float_Array (-1 .. 3) := (-1.0, 3.0, 5.0, 7.5, 12.5);
       begin
          Put_Line ("Average: "
                    & Custom_Float'Image (Average_Float (A)));
       end Test_Digits_12_Float_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Float_Array_Chk =>
                Test_Float_Array;
             when Digits_12_Float_Array_Chk =>
                Test_Digits_12_Float_Array;
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


Average of Array of Decimal Fixed-Point
---------------------------------------

This exercise is based on the implementation that you created for the
previous exercise. Here, your task is to adapt the generic function
:ada:`Average` to calculate the average of decimal fixed-point values. You
may want to reuse the previous implementation as a starting point.

.. code:: ada lab=Generics.Average_Array_Of_Decimal

    --  START LAB IO BLOCK
    in 0:Decimal_Array_Chk
    out 0:Average:  5.40
    in 1:Delta_EM4_Digits_16_Float_Array_Chk
    out 1:Average:  1.2000
    --  END LAB IO BLOCK

    generic
    function Average (A : T_Array) return T_Element;

    function Average (A : T_Array) return T_Element is
    begin
       return 0.0;
    end Average;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Main is
       type Test_Case_Index is (Decimal_Array_Chk,
                                Delta_EM4_Digits_16_Float_Array_Chk);

       procedure Test_Decimal_Array is
          type Decimal is delta 10.0 ** (-2) digits 12;

          type Decimal_Array is
            array (Integer range <>) of Decimal;

          function Average_Decimal is new
            Average (T_Range   => Integer,
                     T_Element => Decimal,
                     T_Array   => Decimal_Array);

          A : constant Decimal_Array (-2 .. 2) := (-1.0, 3.0, 5.0, 7.5, 12.5);
       begin
          Put_Line ("Average: "
                    & Decimal'Image (Average_Decimal (A)));
       end Test_Decimal_Array;

       procedure Test_Delta_EM4_Digits_16_Float_Array is
          type Decimal is delta 10.0 ** (-4) digits 16;

          type Decimal_Array is
            array (Positive range <>) of Decimal;

          function Average_Decimal is new
            Average (T_Range   => Positive,
                     T_Element => Decimal,
                     T_Array   => Decimal_Array);

          A : constant Decimal_Array (2 .. 6) := (2.0, 5.0, 2.0, 8.5, -11.5);
       begin
          Put_Line ("Average: "
                    & Decimal'Image (Average_Decimal (A)));
       end Test_Delta_EM4_Digits_16_Float_Array;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Decimal_Array_Chk =>
                Test_Decimal_Array;
             when Delta_EM4_Digits_16_Float_Array_Chk =>
                Test_Delta_EM4_Digits_16_Float_Array;
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
----------------------------

In this exercise, you'll abstract the :ada:`Average` function from the
previous exercises a step further. In this case, the function shall be
able to calculate the average of any arbitrary type |mdash| including
arrays containing elements of record types. Since record types can be
composed by many components of different types, we need to provide a way to
indicate which component (or components) of the record should be used when
calculating the average of the array. This problem is solved by specifying
a :ada:`To_Float` function as a formal parameter, which converts the
arbitrary element of :ada:`T_Element` type to the :ada:`Float` type. In
the implementation of the :ada:`Average` function, you'll use the
:ada:`To_Float` function and calculate the average using a floating-point
variable.

In addition, you'll also work on the implementation of the test procedures
:ada:`Test_Decimal_Array` and :ada:`Test_Item_Array`:

    - For the :ada:`Test_Decimal_Array` procedure, you'll work with the
      decimal fixed-point type :ada:`Decimal`.

    - In the case of :ada:`Test_Item_Array`, you'll work with the record
      type :ada:`Item`, which contains the :ada:`Quantity` and :ada:`Price`
      components.

This is what you have to do for both procedures:

- Create the :ada:`To_Float` function.

    - For the :ada:`Decimal` type, the function is pretty straightforward.

    - For the :ada:`Item` type, you'll actually create two functions to
      convert to floating-point type:

        - :ada:`Get_Total`, which returns the multiplication of the
          quantity and the price components of the :ada:`Item` type;

        - :ada:`Get_Price`, which returns just the price.

- Instantiate the :ada:`Average` function.

    - For the :ada:`Decimal` type, you'll declare the
      :ada:`Average_Decimal` function.

    - For the :ada:`Item` type, you'll declare the :ada:`Average_Total`
      and :ada:`Average_Price` functions using, respectively, the
      :ada:`Get_Total` and  :ada:`Get_Price` functions mentioned above.

- Instantiate the generic standard package :ada:`Ada.Text_IO.Float_IO` as
  :ada:`F_IO`.

- Use the :ada:`Put` procedure from :ada:`Ada.Text_IO.Float_IO`.

    - This is the specification of the :ada:`Put` procedure, as described
      in the appendix A.10.9 of the Ada Reference Manual:

        .. code-block:: ada

            procedure Put(Item : in Num;
                          Fore : in Field := Default_Fore;
                          Aft  : in Field := Default_Aft;
                          Exp  : in Field := Default_Exp);

    - For the test procedures you're working on, this is the expected
      format when calling :ada:`Put` from :ada:`Float_IO`:

       +-----------------------------+-------+------+------+
       | Function                    | Fore  | Aft  | Exp  |
       +=============================+=======+======+======+
       | :ada:`Test_Decimal_Array`   |     1 |    2 |    0 |
       +-----------------------------+-------+------+------+
       | :ada:`Test_Item_Array`      |     3 |    2 |    0 |
       +-----------------------------+-------+------+------+

.. code:: ada lab=Generics.Average_Any

    --  START LAB IO BLOCK
    in 0:Decimal_Array_Chk
    out 0:Average: 5.40
    in 1:Item_Array_Chk
    out 1:Average per item & quantity: 175.00 Average price:                 7.50
    --  END LAB IO BLOCK

    generic
    function Average (A : T_Array) return Float;

    function Average (A : T_Array) return Float is
    begin
       null;
    end Average;

    procedure Test_Decimal_Array;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Test_Decimal_Array is
       type Decimal is delta 10.0 ** (-2) digits 12;

       type Decimal_Array is
         array (Integer range <>) of Decimal;

       A : constant Decimal_Array (-2 .. 2) := (-1.0, 3.0, 5.0, 7.5, 12.5);
    begin
       Put ("Average: ");
       F_IO.Put (Average_Decimal (A));
       New_Line;
    end Test_Decimal_Array;

    procedure Test_Item_Array;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Average;

    procedure Test_Item_Array is
       type Amount is delta 0.01 digits 12;

       type Item is record
          Quantity : Natural;
          Price    : Amount;
       end record;

       type Item_Array is
         array (Positive range <>) of Item;

       A : constant Item_Array (1 .. 4)
         := ((Quantity =>  5,   Price => 10.00),
             (Quantity => 80,   Price =>  2.50),
             (Quantity => 40,   Price =>  5.00),
             (Quantity => 20,   Price => 12.50));

    begin
       Put ("Average per item & quantity: ");
       F_IO.Put (Average_Total (A));
       New_Line;

       Put ("Average price:               ");
       F_IO.Put (Average_Price (A));
       New_Line;
    end Test_Item_Array;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Decimal_Array;
    with Test_Item_Array;

    procedure Main is
       type Test_Case_Index is (Decimal_Array_Chk,
                                Item_Array_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Decimal_Array_Chk =>
                Test_Decimal_Array;
             when Item_Array_Chk =>
                Test_Item_Array;
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
------------

In previous labs, you've been implementing lists for a variety of types.
The *List of Names* exercise from the :doc:`./arrays` labs is an example.
In this exercise, you'll abstract those list implementations in order to
\create the generic :ada:`Gen_List` package. This package must have the
following subprograms:

    - a procedure :ada:`Init` to initialize the list;

    - a procedure :ada:`Add` to add an item to the list;

        - This procedure must contain a :ada:`Status` output parameter
          that is set to :ada:`False` when the list was full |mdash| i.e.
          when the procedure couldn't add the item;

    - a procedure :ada:`Display` to display the complete list.

        - This includes the *name* of the list and its elements |mdash|
          using one line per element. This is the expected format:

        .. code-block:: none

            <NAME>
            <element #1>
            <element #2>
            ...

These are the formal parameters of the :ada:`Gen_List` package:

    - an arbitrary formal type :ada:`Item`;

    - an unconstrained array type :ada:`Items` of :ada:`Item` element with
      positive range;

    - the :ada:`Name` parameter containing the name of the list;

        - This must be a formal input object of :ada:`String` type.

        - It's used in the :ada:`Display` procedure.

    - an actual array :ada:`List_Array` to store the list;

        - This must be a formal :ada:`in out` object of :ada:`Items` type.

    - the variable :ada:`Last` to store the index of the last element;

        - This must be a formal :ada:`in out` object of :ada:`Natural` type.

    - a procedure :ada:`Put` for the :ada:`Item` type.

        - This procedure is used in the :ada:`Display` procedure to display
          individual elements of the list.

Also, you'll work on two test procedures:

    - the :ada:`Test_Int_List` procedure to test a list of elements of
      :ada:`Integer` type;

    - the :ada:`Test_String_List` procedure to test a list of elements of
      access to :ada:`String` type.

For both test procedures, you'll have to:

    - add missing type declarations;

    - declare and implement a :ada:`Put` procedure for individual elements
      of the list;

    - declare instances of the :ada:`Gen_List` package.

        - the :ada:`Int_List` package for the :ada:`Test_Int_List` procedure;

        - the :ada:`String_List` package for the :ada:`Test_String_List`
          procedure.

.. code:: ada lab=Generics.Gen_List

    --  START LAB IO BLOCK
    in 0:Int_List_Chk
    out 0:Added item successfully! Added item successfully! Added item successfully! Couldn't add item! List of integers  2  5  7
    in 1:String_List_Chk
    out 1:Added item successfully! Added item successfully! Added item successfully! Couldn't add item! List of strings Hello World Bye
    --  END LAB IO BLOCK

    generic
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
          null;
       end Init;

       procedure Add (I      :     Item;
                      Status : out Boolean) is
       begin
          null;
       end Add;

       procedure Display is
       begin
          null;
       end Display;

    end Gen_List;

    procedure Test_Int_List;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_List;

    procedure Test_Int_List is

       type Integer_Array is array (Positive range <>) of Integer;

       A : Integer_Array (1 .. 3);
       L : Natural;

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
    end Test_Int_List;

    procedure Test_String_List;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_List;

    procedure Test_String_List is

       type String_Array is array (Positive range <>) of String_Access;

       A : String_Array (1 .. 3);
       L : Natural;

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
       String_List.Init;

       String_List.Add (new String'("Hello"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("World"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("Bye"), Success);
       Display_Add_Success (Success);

       String_List.Add (new String'("Wait"), Success);
       Display_Add_Success (Success);

       String_List.Display;
    end Test_String_List;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Test_Int_List;
    with Test_String_List;

    procedure Main is
       type Test_Case_Index is (Int_List_Chk,
                                String_List_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Int_List_Chk =>
                Test_Int_List;
             when String_List_Chk =>
                Test_String_List;
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
