Strongly typed language
=======================

.. include:: ../../../global.txt

Colors
------

**Goal**: create a package to represent HTML colors in hexadecimal form and its
corresponding names.

**Steps**:

    #. Implement the :ada:`Color_Types` package.

        #. Declare the :ada:`HTML_Color` enumeration.

        #. Declare the :ada:`Basic_HTML_Color` enumeration.

        #. Implement the :ada:`To_Integer` function.

        #. Implement the :ada:`To_HTML_Color` function.

**Requirements**:

    #. Enumeration :ada:`HTML_Color` has the following colors:

        - Salmon
        - Firebrick
        - Red
        - Darkred
        - Lime
        - Forestgreen
        - Green
        - Darkgreen
        - Blue
        - Mediumblue
        - Darkblue

    #. Enumeration :ada:`Basic_HTML_Color` has the following colors: Red,
       Green, Blue.

    #. Function :ada:`To_Integer` converts from the :ada:`HTML_Color` type to
       the HTML color code |mdash| as integer values in hexadecimal notation.

        - You can find the HTML color codes in the table below.

    #. Function :ada:`To_HTML_Color` converts from :ada:`Basic_HTML_Color` to
       :ada:`HTML_Color`.

    #. This is the table to convert from an HTML color to a HTML color code in
       hexadecimal notation:

    +-------------+------------------------+
    | Color       | HTML color code (hexa) |
    +=============+========================+
    | Salmon      | ``#FA8072``            |
    +-------------+------------------------+
    | Firebrick   | ``#B22222``            |
    +-------------+------------------------+
    | Red         | ``#FF0000``            |
    +-------------+------------------------+
    | Darkred     | ``#8B0000``            |
    +-------------+------------------------+
    | Lime        | ``#00FF00``            |
    +-------------+------------------------+
    | Forestgreen | ``#228B22``            |
    +-------------+------------------------+
    | Green       | ``#008000``            |
    +-------------+------------------------+
    | Darkgreen   | ``#006400``            |
    +-------------+------------------------+
    | Blue        | ``#0000FF``            |
    +-------------+------------------------+
    | Mediumblue  | ``#0000CD``            |
    +-------------+------------------------+
    | Darkblue    | ``#00008B``            |
    +-------------+------------------------+

**Remarks**:

#. In order to express the hexadecimal values above in Ada, use the following
   syntax: ``16#<hex_value>#`` (e.g.: :ada:`16#FFFFFF#`).

#. For function :ada:`To_Integer`, you may use a :ada:`case` for this.

.. code:: ada lab=Strongly_Typed.Colors

    --  START LAB IO BLOCK
    in 0:HTML_Color_Range
    out 0:SALMON FIREBRICK RED DARKRED LIME FORESTGREEN GREEN DARKGREEN BLUE MEDIUMBLUE DARKBLUE
    in 1:HTML_Color_To_Integer
    out 1:16#FA8072# 16#B22222# 16#FF0000# 16#8B0000# 16#FF00# 16#228B22# 16#8000# 16#6400# 16#FF# 16#CD# 16#8B#
    in 2:Basic_HTML_Color_To_HTML_Color
    out 2:RED GREEN BLUE
    --  END LAB IO BLOCK

    package Color_Types is

       --  Include type declaration for HTML_Color!
       --
       --  type HTML_Color is [...]
       --

       --  Include function declaration for:
       --  function To_Integer (C : HTML_Color) return Integer;

       --  Include type declaration for Basic_HTML_Color!
       --
       --  type Basic_HTML_Color is [...]
       --

       --  Include function declaration for:
       --  - Basic_HTML_Color => HTML_Color
       --
       --  function To_HTML_Color [...];
       --
    end Color_Types;

    package body Color_Types is

       --  Implement the conversion from HTML_Color to Integer here!
       --
       --  function To_Integer (C : HTML_Color) return Integer is
       --  begin
       --  --  Hint: use 'case' for the HTML colors;
       --  --        use 16#...# for the hexadecimal values.
       --  end To_Integer;

       --  Implement the conversion from Basic_HTML_Color to HTML_Color here!
       --
       --  function To_HTML_Color [...] is
       --
    end Color_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Integer_Text_IO;

    with Color_Types; use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (HTML_Color_Range,
          HTML_Color_To_Integer,
          Basic_HTML_Color_To_HTML_Color);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when HTML_Color_Range =>
                for I in HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (I));
                end loop;
             when HTML_Color_To_Integer =>
                for I in HTML_Color'Range loop
                   Ada.Integer_Text_IO.Put (Item  => To_Integer (I),
                                            Width => 6,
                                            Base  => 16);
                   New_Line;
                end loop;
             when Basic_HTML_Color_To_HTML_Color =>
                for I in Basic_HTML_Color'Range loop
                   Put_Line (HTML_Color'Image (To_HTML_Color (I)));
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

Integers
--------

**Goal**: implement a package with various integer types.

**Steps**:

    #. Implement the :ada:`Int_Types` package.

        #. Declare the integer type :ada:`I_100`.

        #. Declare the modular type :ada:`U_100`.

        #. Implement the :ada:`To_I_100` function to convert from the
           :ada:`U_100` type.

        #. Implement the :ada:`To_U_100` function to convert from the
           :ada:`I_100` type.

        #. Declare the derived type :ada:`D_50`.

        #. Declare the subtype :ada:`S_50`.

        #. Implement the :ada:`To_D_50` function to convert from the
           :ada:`I_100` type.

        #. Implement the :ada:`To_S_50` function to convert from the
           :ada:`I_100` type.

        #. Implement the :ada:`To_I_100` function to convert from the
           :ada:`D_50` type.

**Requirements**:

    #. Types :ada:`I_100` and :ada:`U_100` have values between 0 and 100.

        #. Type :ada:`I_100` is an integer type.

        #. Type :ada:`U_100` is a modular type.

    #. Function :ada:`To_I_100` converts from the :ada:`U_100` type to the
       :ada:`I_100` type.

    #. Function :ada:`To_U_100` converts from the :ada:`I_100` type to the
       :ada:`U_100` type.

    #. Types :ada:`D_50` and :ada:`S_50` have values between 10 and 50 and
       use :ada:`I_100` as a base type.

        #. :ada:`D_50` is a derived type.

        #. :ada:`S_50` is a subtype.

    #. Function :ada:`To_D_50` converts from the :ada:`I_100` type to the
       :ada:`D_50` type.

    #. Function :ada:`To_S_50` converts from the :ada:`I_100` type to the
       :ada:`S_50` type.

    #. Functions :ada:`To_D_50` and :ada:`To_S_50` saturate the input values if
       they are out of range.

	   - If the input is less than 10 the output should be 10.
	   - If the input is greater than 50 the output should be 50.

    #. Function :ada:`To_I_100` converts from the :ada:`D_50` type to the
       :ada:`I_100` type.

**Remarks**:

#. For the implementation of functions :ada:`To_D_50` and :ada:`To_S_50`, you
   may use the type attributes :ada:`D_50'First` and :ada:`D_50'Last`:

   #. :ada:`D_50'First` indicates the minimum value of the :ada:`D_50` type.

   #. :ada:`D_50'Last` indicates the maximum value of the :ada:`D_50` type.

   #. The same attributes are available for the :ada:`S_50` type (
      :ada:`S_50'First` and :ada:`S_50'Last`).

#. We could have implemented a function :ada:`To_I_100` as well to convert from
   :ada:`S_50` to :ada:`I_100`. However, we skip this here because explicit
   conversions are not needed for subtypes.

.. code:: ada lab=Strongly_Typed.Integers

    --  START LAB IO BLOCK
    in 0:I_100_Range
    out 0:0 100
    in 1:U_100_Range
    out 1:0 100
    in 2:U_100_Wraparound
    out 2:100 0
    in 3:U_100_To_I_100
    out 3:0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
    in 4:I_100_To_U_100
    out 4:0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
    in 5:D_50_Range
    out 5:10 50
    in 6:S_50_Range
    out 6:10 50
    in 7:I_100_To_D_50
    out 7:10 10 10 10 10 10 10 10 10 10 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
    in 8:I_100_To_S_50
    out 8:10 10 10 10 10 10 10 10 10 10 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
    in 9:D_50_To_I_100
    out 9:10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
    in 10:S_50_To_I_100
    out 10:10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
    --  END LAB IO BLOCK

    package Int_Types is

       --  Include type declarations for I_100 and U_100!
       --
       --  type I_100 is [...]
       --  type U_100 is [...]
       --

       function To_I_100 (V : U_100) return I_100;

       function To_U_100 (V : I_100) return U_100;

       --  Include type declarations for D_50 and S_50!
       --
       --  [...] D_50 is [...]
       --  [...] S_50 is [...]
       --

       function To_D_50 (V : I_100) return D_50;

       function To_S_50 (V : I_100) return S_50;

       function To_I_100 (V : D_50) return I_100;

    end Int_Types;

    package body Int_Types is

       function To_I_100 (V : U_100) return I_100 is
       begin
          --  Implement the conversion from U_100 to I_100 here!
          --
          null;
       end To_I_100;

       function To_U_100 (V : I_100) return U_100 is
       begin
          --  Implement the conversion from I_100 to U_100 here!
          --
          null;
       end To_U_100;

       function To_D_50 (V : I_100) return D_50 is
          Min : constant I_100 := I_100 (D_50'First);
          Max : constant I_100 := I_100 (D_50'Last);
       begin
          --  Implement the conversion from I_100 to D_50 here!
          --
          --  Hint: using the constants above simplifies the checks needed for
          --        this function.
          --
          null;
       end To_D_50;

       function To_S_50 (V : I_100) return S_50 is
       begin
          --  Implement the conversion from I_100 to S_50 here!
          --
          --  Remark: don't forget to verify whether an explicit conversion like
          --          S_50 (V) is needed.
          --
          null;
       end To_S_50;

       function To_I_100 (V : D_50) return I_100 is
       begin
          --  Implement the conversion from I_100 to D_50 here!
          --
          --  Remark: don't forget to verify whether an explicit conversion like
          --          I_100 (V) is needed.
          --
          null;
       end To_I_100;

    end Int_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Int_Types;        use Int_Types;

    procedure Main is
       package I_100_IO is new Ada.Text_IO.Integer_IO (I_100);
       package U_100_IO is new Ada.Text_IO.Modular_IO (U_100);
       package D_50_IO  is new Ada.Text_IO.Integer_IO (D_50);

       use I_100_IO;
       use U_100_IO;
       use D_50_IO;

       type Test_Case_Index is
         (I_100_Range,
          U_100_Range,
          U_100_Wraparound,
          U_100_To_I_100,
          I_100_To_U_100,
          D_50_Range,
          S_50_Range,
          I_100_To_D_50,
          I_100_To_S_50,
          D_50_To_I_100,
          S_50_To_I_100);

       procedure Check (TC : Test_Case_Index) is
       begin
          I_100_IO.Default_Width := 1;
          U_100_IO.Default_Width := 1;
          D_50_IO.Default_Width  := 1;

          case TC is
             when I_100_Range =>
                Put (I_100'First);
                New_Line;
                Put (I_100'Last);
                New_Line;
             when U_100_Range =>
                Put (U_100'First);
                New_Line;
                Put (U_100'Last);
                New_Line;
             when U_100_Wraparound =>
                Put (U_100'First - 1);
                New_Line;
                Put (U_100'Last + 1);
                New_Line;
             when U_100_To_I_100 =>
                for I in U_100'Range loop
                   I_100_IO.Put (To_I_100 (I));
                   New_Line;
                end loop;
             when I_100_To_U_100 =>
                for I in I_100'Range loop
                   Put (To_U_100 (I));
                   New_Line;
                end loop;
             when D_50_Range =>
                Put (D_50'First);
                New_Line;
                Put (D_50'Last);
                New_Line;
             when S_50_Range =>
                Put (S_50'First);
                New_Line;
                Put (S_50'Last);
                New_Line;
             when I_100_To_D_50 =>
                for I in I_100'Range loop
                   Put (To_D_50 (I));
                   New_Line;
                end loop;
             when I_100_To_S_50 =>
                for I in I_100'Range loop
                   Put (To_S_50 (I));
                   New_Line;
                end loop;
             when D_50_To_I_100 =>
                for I in D_50'Range loop
                   Put (To_I_100 (I));
                   New_Line;
                end loop;
             when S_50_To_I_100 =>
                for I in S_50'Range loop
                   Put (I);
                   New_Line;
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

Temperatures
------------

**Goal**: create a package to handle temperatures in Celsius and Kelvin.

**Steps**:

    #. Implement the :ada:`Temperature_Types` package.

        #. Declare the :ada:`Celsius` type.

        #. Declare the :ada:`Int_Celsius` type.

        #. Implement the :ada:`To_Celsius` function.

        #. Implement the :ada:`To_Int_Celsius` function.

        #. Declare the :ada:`Kelvin` type.

        #. Implement the :ada:`To_Celsius` function to convert from the
           :ada:`Kelvin` type.

        #. Implement the :ada:`To_Kelvin` function.

**Requirements**:

    #. The custom floating-point types declared in :ada:`Temperature_Types`
       must use a precision of six digits.

    #. Types :ada:`Celsius` and :ada:`Int_Celsius` are used for temperatures in
       Celsius:

       #. :ada:`Celsius` is a floating-point type with a range between -273.15
          and 5504.85.

       #. :ada:`Int_Celsius` is an integer type with a range between -273 and
          5505.

    #. Functions :ada:`To_Celsius` and :ada:`To_Int_Celsius` are used for type
       conversion:

        #. :ada:`To_Celsius` converts from :ada:`Int_Celsius` to :ada:`Celsius`
           type.

        #. :ada:`To_Int_Celsius` converts from :ada:`Celsius` and
           :ada:`Int_Celsius` types:

    #. :ada:`Kelvin` is a floating-point type for temperatures in Kelvin using
       a range between 0.0 and 5778.0.

    #. The functions :ada:`To_Celsius` and :ada:`To_Kelvin` are used to convert
       between temperatures in :ada:`Kelvin` and :ada:`Celsius`.

        #. In order to convert temperatures in Celsius to Kelvin, you must use
           the formula :math:`K = C + 273.15`, where:

            - *K* is the temperature in Kelvin, and

            - *C* is the temperature in Celsius.

**Remarks**:

#. When implementing the :ada:`To_Celsius` function for the :ada:`Int_Celsius`
   type:

    #. You'll need to check for the minimum and maximum values of the input
       values because of the slightly different ranges.

    #. You may use variables of floating-point type (:ada:`Float`) for
       intermediate values.

#. For the implementation of the functions :ada:`To_Celsius` and
   :ada:`To_Kelvin` (used for converting between :ada:`Kelvin` and
   :ada:`Celsius`), you may use a variable of floating-point type
   (:ada:`Float`) for intermediate values.

.. code:: ada lab=Strongly_Typed.Temperatures

    --  START LAB IO BLOCK
    in 0:Celsius_Range
    out 0:-2.73150E+02 5.50485E+03
    in 1:Celsius_To_Int_Celsius
    out 1:-273 0 5505
    in 2:Int_Celsius_To_Celsius
    out 2:-2.73000E+02 0.00000E+00 5.50485E+03
    in 3:Kelvin_To_Celsius
    out 3:-2.73150E+02 0.00000E+00 5.50485E+03
    in 4:Celsius_To_Kelvin
    out 4:0.00000E+00 5.77800E+03
    --  END LAB IO BLOCK

    package Temperature_Types is

       --  Include type declaration for Celsius!
       --
       --  Celsius is [...];
	   --  Int_Celsius is [...];
       --

       function To_Celsius (T : Int_Celsius) return Celsius;

       function To_Int_Celsius (T : Celsius) return Int_Celsius;

       --  Include type declaration for Kelvin!
       --
       --  type Kelvin is [...];
       --

       --  Include function declarations for:
       --  - Kelvin  => Celsius
       --  - Celsius => Kelvin
       --
       --  function To_Celsius [...];
       --  function To_Kelvin  [...];
       --
    end Temperature_Types;

    package body Temperature_Types is

       function To_Celsius (T : Int_Celsius) return Celsius is
       begin
          null;
       end To_Celsius;

       function To_Int_Celsius (T : Celsius) return Int_Celsius is
       begin
          null;
       end To_Int_Celsius;

       --  Include function implementation for:
       --  - Kelvin  => Celsius
       --  - Celsius => Kelvin
       --
       --  function To_Celsius [...] is
       --  function To_Kelvin  [...] is
       --
    end Temperature_Types;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Temperature_Types; use Temperature_Types;

    procedure Main is
       package Celsius_IO     is new Ada.Text_IO.Float_IO (Celsius);
       package Kelvin_IO      is new Ada.Text_IO.Float_IO (Kelvin);
       package Int_Celsius_IO is new Ada.Text_IO.Integer_IO (Int_Celsius);

       use Celsius_IO;
       use Kelvin_IO;
       use Int_Celsius_IO;

       type Test_Case_Index is
         (Celsius_Range,
          Celsius_To_Int_Celsius,
          Int_Celsius_To_Celsius,
          Kelvin_To_Celsius,
          Celsius_To_Kelvin);

       procedure Check (TC : Test_Case_Index) is
       begin
          Celsius_IO.Default_Fore := 1;
          Kelvin_IO.Default_Fore  := 1;
          Int_Celsius_IO.Default_Width := 1;

          case TC is
             when Celsius_Range =>
                Put (Celsius'First);
                New_Line;
                Put (Celsius'Last);
                New_Line;
             when Celsius_To_Int_Celsius =>
                Put (To_Int_Celsius (Celsius'First));
                New_Line;
                Put (To_Int_Celsius (0.0));
                New_Line;
                Put (To_Int_Celsius (Celsius'Last));
                New_Line;
             when Int_Celsius_To_Celsius =>
                Put (To_Celsius (Int_Celsius'First));
                New_Line;
                Put (To_Celsius (0));
                New_Line;
                Put (To_Celsius (Int_Celsius'Last));
                New_Line;
             when Kelvin_To_Celsius =>
                Put (To_Celsius (Kelvin'First));
                New_Line;
                Put (To_Celsius (0));
                New_Line;
                Put (To_Celsius (Kelvin'Last));
                New_Line;
             when Celsius_To_Kelvin =>
                Put (To_Kelvin (Celsius'First));
                New_Line;
                Put (To_Kelvin (Celsius'Last));
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
