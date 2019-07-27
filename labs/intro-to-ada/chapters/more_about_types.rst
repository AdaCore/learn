:orphan:

More About Types
================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Aggregate Initialization
------------------------

In this exercise, we'll look into aggregates to initialize records and
arrays. Your goal is to:

- Create a record type :ada:`Rec` with four components of :ada:`Integer`
  type and specify default values for them, as indicated by this list:

    - :ada:`W` = 10
    - :ada:`X` = 11
    - :ada:`Y` = 12
    - :ada:`Z` = 13

- Create an array type :ada:`Int_Arr` of :ada:`Integer` with 20 elements
  (ranging from 1 to 20).

- Implement the procedure :ada:`Init` that outputs a record :ada:`Rec`,
  where :ada:`X` is initialized with 100, :ada:`Y` is initialized with 200,
  and the remaining elements use their default values.

- Implement the procedure :ada:`Init_Some` that outputs an array
  :ada:`Int_Arr` where the first five elements are initialized with the
  value 99, and the remaining elements are initialized with the value 100.

- Implement the procedure :ada:`Init` that outputs an array
  :ada:`Int_Arr` where all elements are initialized with the value 5.

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

In this exercise, you'll implement a simple package for source-code
versioning. This requires the use of function overloading. Your goal is to:

- Declare a record type :ada:`Version` with the components :ada:`Major`,
  :ada:`Minor` and :ada:`Maintenance` of :ada:`Natural` type.

- Implement a function :ada:`Convert` that returns a string containing
  the version number.

  - Hint: you can make use of the :ada:`Image_Trim` function, as indicated
    in the source-code below (see package body of :ada:`Versioning`)

- Implement a function :ada:`Convert` that returns a floating-point number,
  where the number before the decimal point corresponds to the major number
  and the number after the decimal point corresponds to the minor number.

  - For example, version "1.3.5" is converted to the floating-point number
    ``1.3``.

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

In this exercise, you'll implement a simple to-do list system. In order
to do that, you'll use access types and unconstrained arrays. This is the
system specification:

- :ada:`Todo_Item` type: this is used to store to-do items. It should be
  implemented as an access type to strings.

- :ada:`Todo_List` type: it's the container for all to-do items. It should
  be implemented as an unconstrained array with positive range.

    - Hint: don't forget to keep track of the last element added to the
      list!

- :ada:`Add` procedure: it's used to add items (of :ada:`Todo_Item` type)
  to the list (of :ada:`Todo_List` type).

    - Hint: this requires allocating a string for the access type.

- :ada:`Display` procedure: it's used to display all to-do items. It must
  display one item per line.

.. code:: ada lab=Solutions.More_About_Types.Simple_Todo_List

    --  START LAB IO BLOCK
    in 0:Todo_List_Chk
    out 0:ERROR: list is full! TO-DO LIST Buy milk Buy tea Buy present Buy tickets Pay electricity bill Schedule dentist appointment Call sister Revise spreasheet Edit entry page Select new design
    --  END LAB IO BLOCK

    package Todo_Lists is

       --  Replace by actual type declaration
       type Todo_Item is null record;

       --  Replace by actual type declaration
       type Todo_List is null record;

       procedure Add (Todos : in out Todo_List;
                      Item  : String);

       procedure Display (Todos : Todo_List);

    end Todo_Lists;

    package body Todo_Lists is

       procedure Add (Todos : in out Todo_List;
                      Item  : String) is
       begin
          null;
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

In this exercise, you'll implement a list containing prices. In order to
accomplish this, you'll use the following features of the Ada language:

- decimal fixed-point types;
- records with discriminants;
- dynamically-sized record types;
- variant records.

Your goals are:

#. Declare a decimal fixed-point data type :ada:`Price_Type` with a delta
   of two digits (e.g. ``0.01``) and twelve digits in total.

#. Declare the record type :ada:`Price_List` that contains the price list.
   This record type must have a discriminant for the maximum number of
   elements of the list.

    - Hint: you may use an unconstrained array as a component of the
      record and use the discriminant in the component declaration.

#. Implement the procedure :ada:`Reset` to reset the list.

#. Implement the procedure :ada:`Add` to add a price to the list.

    - Hint: you should keep track of the last element added to the list.

#. Implement the :ada:`Get` function to retrieve a price from the list
   using an index. This function returns a record instance of type
   :ada:`Price_Result`:

    - :ada:`Price_Result` is a variant record containing the Boolean
      component :ada:`Ok` and the component :ada:`Price` (of
      :ada:`Price_Type`).

    - If the index specified in a call to :ada:`Get` contains a valid
      (initialized) price, :ada:`Ok` is set to :ada:`True` and
      :ada:`Price` contains the price for that index. Otherwise, :ada:`Ok`
      is set to :ada:`False` and the :ada:`Price` component is not
      available.

#. Implement the procedure :ada:`Display` to show all prices from the list.
   The header (first line) must be :ada:`PRICE LIST`, while the remaining
   lines contain one price per line.

For example, for the following code:

.. code-block:: ada

    procedure Test is
       L : Price_List (10);
    begin
       Reset (L);
       Add (L, 1.45);
       Add (L, 2.37);
       Display (L);
    end Test;

The output is:

.. code-block:: none

    PRICE LIST
     1.45
     2.37

.. code:: ada lab=More_About_Types.Price_List

    --  START LAB IO BLOCK
    in 0:Price_Type_Chk
    out 0:The delta    value of Price_Type is  0.01; The minimum  value of Price_Type is -9999999999.99; The maximum  value of Price_Type is  9999999999.99;
    in 1:Price_List_Chk
    out 1:PRICE LIST  1.45  2.37  3.21  4.14  5.22  6.69  7.77  8.14  9.99  10.01
    in 2:Price_List_Get_Chk
    out 2:Attemp Get #  5 Element #  5 =>  5.22 Attemp Get #  40 Element not available (as expected)
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

Binary fixed-point types
------------------------

In this exercise, you'll work on a square-root calculation algorithm
implemented with fixed-point data types. The original algorithm was
implemented in C language by Christophe Meessen and was taken from
`this repository on Github <https://github.com/chmike/fpsqrt/blob/master/fpsqrt.c>`_.
This is the original source-code of the function that we want to port to
Ada and use in our test application:

.. code-block:: c

    typedef int32_t fixed;

    /*
     * fixed sqrtF2F( fixed v );
     *
     * Compute fixed to fixed square root
     * RETURNS the fixed point square root of v (fixed)
     * REQUIRES v is positive
     */
    fixed sqrtF2F ( fixed x )
    {
        uint32_t t, q, b, r;
        r = x;
        b = 0x40000000;
        q = 0;
        while( b > 0x40 )
        {
            t = q + b;
            if( r >= t )
            {
                r -= t;
                q = t + b; // equivalent to q += 2*b
            }
            r <<= 1;
            b >>= 1;
        }
        q >>= 8;
        return q;
    }

In order to complete this exercise, you don't need to worry about the
implementation details of the :ada:`Sqrt` function. If you look at the
body of the :ada:`Fixed_Point_Ops` package in the source-code below,
you'll see that it already contains the ported algorithm! Your only task
in this exercise is to declare the correct fixed-point data type
:ada:`Fixed` |mdash| in the specification of the :ada:`Fixed_Point_Ops`
package |mdash| for the :ada:`Sqrt` function.

As a bonus, however, if you're looking for a more challenging exercise,
you may adapt the ported algorithm for multiple, arbitrary formats of
fixed-point data types. The implementation of :ada:`Sqrt` shown below only
works fine for Q15.16 |mdash| as in the original C code. However, you may
adapt the algorithm to make it work for Q31.32, Q23.24, Q11.12, Q7.6, or
unsigned version such as Q32.32, Q24.24, and Q12.12, or even formats such
as Q7.24. This may require some generalization of the :ada:`Fixed`
declaration as well as the :ada:`Sqrt` function implementation. For
example, you may use a type declaration such as
:ada:`type Fixed is delta F_Delta range 0.0 .. F_Last - F_Delta;` and
declare :ada:`F_Delta` and :ada:`F_Last` with constant values depending on
which Qx.y format you want to use.

.. code:: ada lab=More_About_Types.Fixed_Point_Sqrt

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

       --  Complete fixed-point type declaration:
       --  type TQ15_16 is ...
       subtype Fixed is TQ15_16;

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

          B_Init     : constant := 16#4000.0000#;
          --  Equivalent to:
          --      2#100_0000_0000_0000.0000_0000_0000_0000#;

          B_Thres    : constant := 16#0000.0040#;
          --  Equivalent to:
          --      2#000_0000_0000_0000.0000_0000_0000_0100#;
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
          Q := Q / 2 ** 8;

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
---------

In this exercise, you'll improve the simplified inventory system for your
store, which you designed in a previous lab (see
:doc:`./records`) The system will be used to enter items and keep track of
your assets. These are the improvements that you'll implement:

- Support for storing items on a database, so that you're able to retrieve
  and display them later.

- Support for storing information about how many units have been bought
  and sold for each item.

Your goals with this exercise are:

#. Create a database using arrays and records to store the information.
   You need to declare the record types :ada:`Item` and :ada:`Inventory`.

    - Hint: you can take the same approach as for the previous lab on
      *Price list* for the :ada:`Inventory` type.

#. Declare a decimal fixed-point data type :ada:`Amount` with a delta
   of two digits (e.g. ``0.01``) and twelve digits in total.

    - Hint: you can reuse the type declaration of the :ada:`Price_Type`
      type from the previous lab on *Price list*.

#. Declare :ada:`Name_Type` as an access type to strings.

    - Hint: you can reuse the type declaration of the :ada:`Todo_Item`
      type from the previous lab on *Simple To-Do List*.

#. Declare the variant record type :ada:`Add_Status` with a Boolean type
   as a discriminant. This type is used for an output parameter of the
   :ada:`Add` procedure, which we'll discuss later.

    - Hint: as a starting point, you can reuse the declaration of the
      :ada:`Price_Result` type from the previous lab on *Price list*.

#. Implement two :ada:`Init` functions: one for the :ada:`Item` type and
   one for the :ada:`Inventory` type. Make sure to initialize all relevant
   components!

#. Implement the :ada:`Add` procedure for adding items to your inventory.
   As mentioned above, this procedure has an output parameter
   of :ada:`Add_Status` type:

    - If the call to :ada:`Add` is successful, :ada:`Success` is set to
      :ada:`True` and :ada:`ID` contains the index of the item that has
      just been added. Otherwise, :ada:`Success` is set to :ada:`False`
      and the :ada:`ID` component is not available.

#. Implement the :ada:`Last_Id` function, which returns the index of the
   last item of the list.

#. Implement the :ada:`Set` procedure for adding transaction information
   (*bought* or *sold* units) for a specific item from the inventory.

    - This procedure has the output parameter :ada:`Success` of
      Boolean type, which indicates whether the call was successful or
      not. This happens, for example, when 20 units are available for an
      item, and the call to the :ada:`Set` procedure attemps to *sell* 30
      units, which could lead to a negative number of items. In this case,
      the call to :ada:`Set` indicates a failure. For example:

    .. code-block:: ada

         Add (Inv,
              Init ("Chair", 200.00),
              Status);

         if Status.Success then
            Set (Inv      => Inv,
                 Trans    => Bought,
                 ID       => Status.ID,
                 Quantity => 20,
                 Success  => Success);

            --  Success = False in the call below, because we only have
            --  20 units in stock, so we cannot sell 30 units.
            Set (Inv      => Inv,
                 Trans    => Sold,
                 ID       => Status.ID,
                 Quantity => 30,
                 Success  => Success);
         end if;

#. Implement the following :ada:`Get` functions:

    .. code-block:: ada

        function Get (Inv   : Inventory;
                      ID    : Item_ID) return String;
        --  Retrieve item name

        function Get (Inv   : Inventory;
                      ID    : Item_ID) return Item_Quantity;
        --  Retrieve number of units in stock for specified item

        function Get (Inv   : Inventory;
                      ID    : Item_ID) return Amount;
        --  Retrieve total amount in stock for specified item

        function Get (Inv   : Inventory;
                      Trans : Transaction_Type;
                      ID    : Item_ID) return Item_Quantity;
        --  Retrieve number of units for specified item and transaction type

        function Get (Inv   : Inventory;
                      Trans : Transaction_Type;
                      ID    : Item_ID) return Amount;
        --  Retrieve amount for specified item and transaction type

        function Get (Inv   : Inventory;
                      Trans : Transaction_Type) return Amount;
        --  Retrieve amount for transaction type

        function Get (Inv   : Inventory) return Amount;
        --  Retrieve amount for whole inventory

    - Note that overloading is used for most of these functions. Therefore,
      the actual function called for a specific variable depends on its
      type (as indicated in the comments of the code block above). For
      example:

    .. code-block:: ada

        procedure Test is
           Inv     : Inventory (3);
        begin
           --
           --  Add two items to the database
           --
           declare
              Success : Boolean;
              Status  : Add_Status;
           begin
              Add (Inv, Init ("Chair", 200.0), Status);
              Set (Inv, Bought, Status.ID, 10, Success);
              Set (Inv, Sold,   Status.ID,  5, Success);

              Add (Inv, Init ("Table", 300.0), Status);
              Set (Inv, Bought, Status.ID, 20, Success);
              Set (Inv, Sold,   Status.ID, 10, Success);
           end;

           --
           --  Retrieve information from the database
           --
           declare
              ID                                           : Item_ID
                := Get (Inv, "Chair");

              Item_Name                                    : String
                := Get (Inv, ID);

              Number_Units_In_Stock_For_Item               : Item_Quantity
                := Get (Inv, ID);

              Potential_Income_For_Units_In_Stock_For_Item : Amount
                := Get (Inv, ID);

              Number_Units_Sold_For_Item                   : Item_Quantity
                := Get (Inv, Sold, ID);

              Income_For_Sold_Units_Of_Item                : Amount
                := Get (Inv, Sold, ID);

              Income_For_All_Sold_Units                    : Amount
                := Get (Inv, Sold);

              Potential_Income_For_All_Units_In_Stock      : Amount
                := Get (Inv);
           begin
              --  Some processing here...
              null;
           end;
        end Test;

    - **Remark**: we didn't mention error handling for the :ada:`ID`
      parameter of the :ada:`Get` functions above |mdash| as we had in a
      previous exercise (see *Price list*). If the :ada:`ID` indicates an
      item that hasn't been initialized yet, the information returned by
      the :ada:`Get` function is essentially garbage. Although this is an
      important check to make, we're leaving it out of this specification
      to simplify the exercise a little bit. You may, however, as a bonus
      for this exercise, extend the system to cover this check.

As you've probably noticed, this exercise consists of many steps, so it
takes more time to complete it than it took for previous labs. However, as
indicated in the description above, you may reuse many elements that you
already implemented in previous labs. This may make it quicker to finish
the implementation work of this exercise.

.. code:: ada lab=More_About_Types.Inventory

    --  START LAB IO BLOCK
    in 0:Inventory_Chk
    out 0:==== ITEM #  1: Ballpoint Pen  Price:     0.90 == BOUGHT Quantity:  10 Amount:    1.50 == SOLD Quantity:  4 Amount:    0.60 == IN STOCK Quantity:  6 Amount:    0.90  ==== ITEM #  2: Oil-based Pen Marker  Price:     180.00 == BOUGHT Quantity:  20 Amount:    180.00 == SOLD Quantity:  0 Amount:    0.00 == IN STOCK Quantity:  20 Amount:    180.00  ==== ITEM #  3: Feather Quill Pen  Price:     450.00 == BOUGHT Quantity:  50 Amount:    750.00 == SOLD Quantity:  20 Amount:    300.00 == IN STOCK Quantity:  30 Amount:    450.00  ==== OVERALL Amount bought:    931.50 Amount sold:      300.60 Amount in stock:  450.00
    in 1:Inventory_Range_Chk
    out 1:Info: Call to 'Add' failed as expected. Info: Call to 'Set' failed as expected.
    --  END LAB IO BLOCK

    package Inventory_Pkg is

       subtype Item_Quantity is Natural;

       --  Replace by actual fixed-point decimal type declaration
       type Amount is new Float;

       --  Replace by actual access type declaration
       type Name_Type is null record;

       subtype Item_ID is Positive;

       type Transaction_Type is (Bought, Sold);

       --  Replace by actual variant record declaration
       type Add_Status is null record;

       type Item is null record;
          --  Name            : Name_Type;
          --  Price           : Amount;
          --
          --  --  Add components for quantities and amounts
          --
       --  end record;

       --  Replace by actual variant record declaration
       type Inventory is null record;
          --
          --  --  Add components (e.g. array of elements of type Item)
          --
       --  end record;

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
          return (null record);
       end Init;

       procedure Init (Inv : in out Inventory) is
       begin
          null;
       end Init;

       procedure Add (Inv     : in out Inventory;
                      I       :        Item;
                      Status  : out    Add_Status)
       is
       begin
          null;
       end Add;

       function Get (Inv       : Inventory;
                     Item_Name : String) return Item_ID is
       begin
          return 1;
       end Get;

       function Last_Id (Inv : Inventory) return Natural is (0);

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
                     ID    : Item_ID) return Amount is
         (0.0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Item_Quantity is
         (0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type;
                     ID    : Item_ID) return Amount is
         (0.0);

       function Get (Inv   : Inventory;
                     Trans : Transaction_Type) return Amount
       is
          Total : Amount := 0.0;
       begin
          return Total;
       end Get;

       function Get (Inv   : Inventory) return Amount
       is
          Total : Amount := 0.0;
       begin
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

