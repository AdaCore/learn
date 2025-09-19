Design by contracts
===================

.. include:: ../../../global.txt

Price Range
-----------

**Goal**: use predicates to indicate the correct range of prices.

**Steps**:

    #. Complete the :ada:`Prices` package.

        #. Rewrite the type declaration of :ada:`Price`.

**Requirements**:

    #. Type :ada:`Price` must use a predicate instead of a range.

**Remarks**:

    #. As discussed in the course, ranges are a form of contract.

        #. For example, the subtype :ada:`Price` below indicates that a value
           of this subtype must always be positive:

            .. code-block:: ada

                subtype Price is Amount range 0.0 .. Amount'Last;

        #. Interestingly, you can replace ranges by predicates, which is the
           goal of this exercise.

.. code:: ada lab=Contracts.Price_Range

    --  START LAB IO BLOCK
    in 0:Price_Range_Chk
    out 0:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Prices is

       type Amount is delta 10.0 ** (-2) digits 12;

       subtype Price is Amount range 0.0 .. Amount'Last;

    end Prices;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Prices;            use Prices;

    procedure Main is

       type Test_Case_Index is
         (Price_Range_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Range (A : Amount) is
             P : constant Price := A;
          begin
             Put_Line ("Price: " & Price'Image (P));
          end Check_Range;

       begin
          case TC is
          when Price_Range_Chk =>
             Check_Range (-2.0);
          end case;
       exception
          when Constraint_Error =>
             Put_Line ("Constraint_Error detected (NOT as expected).");
          when Assert_Failure =>
             Put_Line ("Assert_Failure detected (as expected).");
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

Pythagorean Theorem: Predicate
------------------------------

**Goal**: use the Pythagorean theorem as a predicate.

**Steps**:

    #. Complete the :ada:`Triangles` package.

        #. Add a predicate to the :ada:`Right_Triangle` type.

**Requirements**:

    #. The :ada:`Right_Triangle` type must use the Pythagorean theorem as a
       predicate to ensure that its components are consistent.

**Remarks**:

    #. As you probably remember, the
       :wikipedia:`Pythagoras' theorem <Pythagorean_theorem>`
       states that the square of the hypotenuse of a right triangle is equal to
       the sum of the squares of the other two sides.

.. code:: ada lab=Solutions.Contracts.Pythagoras_Predicate

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Precondition
---------------------------------

**Goal**: use the Pythagorean theorem as a precondition.

**Steps**:

    #. Complete the :ada:`Triangles` package.

        #. Add a precondition to the :ada:`Init` function.

**Requirements**:

    #. The :ada:`Init` function must use the Pythagorean theorem as a
       precondition to ensure that the input values are consistent.

**Remarks**:

    #. In this exercise, you'll work again with the :ada:`Right_Triangle` type.

        #. This time, your job is to use a precondition instead of a
           predicate.

        #. The precondition is applied to the :ada:`Init` function, not to the
           :ada:`Right_Triangle` type.

.. code:: ada lab=Solutions.Contracts.Pythagoras_Precondition

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Postcondition
----------------------------------

**Goal**: use the Pythagorean theorem as a postcondition.

**Steps**:

    #. Complete the :ada:`Triangles` package.

        #. Add a postcondition to the :ada:`Init` function.

**Requirements**:

    #. The :ada:`Init` function must use the Pythagorean theorem as a
       postcondition to ensure that the returned object is consistent.

**Remarks**:

    #. In this exercise, you'll work again with the :ada:`Triangles` package.

        #. This time, your job is to apply a postcondition instead of a
           precondition to the :ada:`Init` function.

.. code:: ada lab=Contracts.Pythagoras_Postcondition

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       subtype Length is Integer;

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Pythagorean Theorem: Type Invariant
-----------------------------------

**Goal**: use the Pythagorean theorem as a type invariant.

**Steps**:

    #. Complete the :ada:`Triangles` package.

        #. Add a type invariant to the :ada:`Right_Triangle` type.

**Requirements**:

    #. :ada:`Right_Triangle` is a private type.

        #. It must use the Pythagorean theorem as a type invariant to ensure
           that its encapsulated components are consistent.

**Remarks**:

    #. In this exercise, :ada:`Right_Triangle` is declared as a private type.

        #. In this case, we use a type invariant for :ada:`Right_Triangle` to
           check the Pythagorean theorem.

    #. As a bonus, after completing the exercise, you may analyze the effect
       that default values have on type invariants.

        #. For example, the declaration of :ada:`Right_Triangle` uses zero as
           the default values of the three triangle lengths.

        #. If you replace those default values with :ada:`Length'Last`, you'll
           get different results.

        #. Make sure you understand why this is happening.

.. code:: ada lab=Contracts.Pythagoras_Type_Invariant

    --  START LAB IO BLOCK
    in 0:Triangle_8_6_Pass_Chk
    out 0:( 10,  8,  6)
    in 1:Triangle_8_6_Fail_Chk
    out 1:Assert_Failure detected (as expected).
    in 2:Triangle_10_24_Pass_Chk
    out 2:( 26,  10,  24)
    in 3:Triangle_10_24_Fail_Chk
    out 3:Assert_Failure detected (as expected).
    in 4:Triangle_18_24_Pass_Chk
    out 4:( 30,  18,  24)
    in 5:Triangle_18_24_Fail_Chk
    out 5:Assert_Failure detected (as expected).
    --  END LAB IO BLOCK

    package Triangles is

       subtype Length is Integer;

       type Right_Triangle is private;

       function Init (H, C1, C2 : Length) return Right_Triangle;

    private

       type Right_Triangle is record
          H      : Length := 0;
          --  Hypotenuse
          C1, C2 : Length := 0;
          --  Catheti / legs
       end record;

       function Init (H, C1, C2 : Length) return Right_Triangle is
         ((H, C1, C2));

    end Triangles;

    package Triangles.IO is

       function Image (T : Right_Triangle) return String;

    end Triangles.IO;

    package body Triangles.IO is

       function Image (T : Right_Triangle) return String is
         ("("    & Length'Image (T.H)
          & ", " & Length'Image (T.C1)
          & ", " & Length'Image (T.C2)
          & ")");

    end Triangles.IO;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;
    with System.Assertions; use System.Assertions;

    with Triangles;         use Triangles;
    with Triangles.IO;      use Triangles.IO;

    procedure Main is

       type Test_Case_Index is
         (Triangle_8_6_Pass_Chk,
          Triangle_8_6_Fail_Chk,
          Triangle_10_24_Pass_Chk,
          Triangle_10_24_Fail_Chk,
          Triangle_18_24_Pass_Chk,
          Triangle_18_24_Fail_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Triangle (H, C1, C2 : Length) is
             T : Right_Triangle;
          begin
             T := Init (H, C1, C2);
             Put_Line (Image (T));
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error detected (NOT as expected).");
             when Assert_Failure =>
                Put_Line ("Assert_Failure detected (as expected).");
          end Check_Triangle;

       begin
          case TC is
             when Triangle_8_6_Pass_Chk   => Check_Triangle (10,  8,  6);
             when Triangle_8_6_Fail_Chk   => Check_Triangle (12,  8,  6);
             when Triangle_10_24_Pass_Chk => Check_Triangle (26, 10, 24);
             when Triangle_10_24_Fail_Chk => Check_Triangle (12, 10, 24);
             when Triangle_18_24_Pass_Chk => Check_Triangle (30, 18, 24);
             when Triangle_18_24_Fail_Chk => Check_Triangle (32, 18, 24);
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

Primary Color
-------------

**Goal**: extend a package for HTML colors so that it can handle primary
colors.

**Steps**:

    #. Complete the :ada:`Color_Types` package.

        #. Declare the :ada:`HTML_RGB_Color` subtype.

        #. Implement the :ada:`To_Int_Color` function.

**Requirements**:

    #. The :ada:`HTML_Color` type is an enumeration that contains a list of
       HTML colors.

    #. The :ada:`To_RGB_Lookup_Table` array implements a lookup-table to
       convert the colors into a hexadecimal value using RGB color components
       (i.e. :ada:`Red`, :ada:`Green` and :ada:`Blue`)

    #. Function :ada:`To_Int_Color` extracts one of the RGB components of an
       HTML color and returns its hexadecimal value.

        #. The function has two parameters:

            - First parameter is the HTML color (:ada:`HTML_Color` type).

            - Second parameter indicates which RGB component is to be extracted
              from the HTML color (:ada:`HTML_RGB_Color` subtype).

        #. For example, if we call :ada:`To_Int_Color (Salmon, Red)`, the
           function returns ``#FA``,

            - This is the hexadecimal value of the red component of the
              :ada:`Salmon` color.

            - You can find further remarks below about this color as an
              example.

    #. The :ada:`HTML_RGB_Color` subtype is limited to the primary RGB colors
       components (i.e. :ada:`Red`, :ada:`Green` and :ada:`Blue`).

        #. This subtype is used to select the RGB component in calls to
           :ada:`To_Int_Color`.

        #. You must use a predicate in the type declaration.

**Remarks**:

    #. In this exercise, we reuse the code of the ``Colors: Lookup-Table``
       exercise from the :doc:`./arrays` labs.

    #. These are the hexadecimal values of the colors that we used in the
       original exercise:

        +-------------+-------------+
        | Color       | Value       |
        +=============+=============+
        | Salmon      | ``#FA8072`` |
        +-------------+-------------+
        | Firebrick   | ``#B22222`` |
        +-------------+-------------+
        | Red         | ``#FF0000`` |
        +-------------+-------------+
        | Darkred     | ``#8B0000`` |
        +-------------+-------------+
        | Lime        | ``#00FF00`` |
        +-------------+-------------+
        | Forestgreen | ``#228B22`` |
        +-------------+-------------+
        | Green       | ``#008000`` |
        +-------------+-------------+
        | Darkgreen   | ``#006400`` |
        +-------------+-------------+
        | Blue        | ``#0000FF`` |
        +-------------+-------------+
        | Mediumblue  | ``#0000CD`` |
        +-------------+-------------+
        | Darkblue    | ``#00008B`` |
        +-------------+-------------+

    #. You can extract the hexadecimal value of each primary color by splitting
       the values from the table above into three hexadecimal values with two
       digits each.

        - For example, the hexadecimal value of :ada:`Salmon` is ``#FA8072``,
          where:

            - the first part of this hexadecimal value (``#FA``) corresponds
              to the red component,

            - the second part (``#80``) corresponds to the green component, and

            - the last part (``#72``) corresponds to the blue component.

.. code:: ada lab=Contracts.Primary_Colors

    --  START LAB IO BLOCK
    in 0:HTML_Color_Red_Chk
    out 0:Selected: RED SALMON =>     16#FA#. FIREBRICK =>     16#B2#. RED =>     16#FF#. DARKRED =>     16#8B#. LIME =>      16#0#. FORESTGREEN =>     16#22#. GREEN =>      16#0#. DARKGREEN =>      16#0#. BLUE =>      16#0#. MEDIUMBLUE =>      16#0#. DARKBLUE =>      16#0#.
    in 1:HTML_Color_Green_Chk
    out 1:Selected: GREEN SALMON =>     16#80#. FIREBRICK =>     16#22#. RED =>      16#0#. DARKRED =>      16#0#. LIME =>     16#FF#. FORESTGREEN =>     16#8B#. GREEN =>     16#80#. DARKGREEN =>     16#64#. BLUE =>      16#0#. MEDIUMBLUE =>      16#0#. DARKBLUE =>      16#0#.
    in 2:HTML_Color_Blue_Chk
    out 2:Selected: BLUE SALMON =>     16#72#. FIREBRICK =>     16#22#. RED =>      16#0#. DARKRED =>      16#0#. LIME =>      16#0#. FORESTGREEN =>     16#22#. GREEN =>      16#0#. DARKGREEN =>      16#0#. BLUE =>     16#FF#. MEDIUMBLUE =>     16#CD#. DARKBLUE =>     16#8B#.
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

       function Image (I : Int_Color) return String;

       type RGB is record
          Red   : Int_Color;
          Green : Int_Color;
          Blue  : Int_Color;
       end record;

       function To_RGB (C : HTML_Color) return RGB;

       function Image (C : RGB) return String;

       type HTML_Color_RGB_Array is array (HTML_Color) of RGB;

       To_RGB_Lookup_Table : constant HTML_Color_RGB_Array
         := (Salmon      => (16#FA#, 16#80#, 16#72#),
             Firebrick   => (16#B2#, 16#22#, 16#22#),
             Red         => (16#FF#, 16#00#, 16#00#),
             Darkred     => (16#8B#, 16#00#, 16#00#),
             Lime        => (16#00#, 16#FF#, 16#00#),
             Forestgreen => (16#22#, 16#8B#, 16#22#),
             Green       => (16#00#, 16#80#, 16#00#),
             Darkgreen   => (16#00#, 16#64#, 16#00#),
             Blue        => (16#00#, 16#00#, 16#FF#),
             Mediumblue  => (16#00#, 16#00#, 16#CD#),
             Darkblue    => (16#00#, 16#00#, 16#8B#));

       subtype HTML_RGB_Color is HTML_Color;

       function To_Int_Color (C : HTML_Color;
                              S : HTML_RGB_Color) return Int_Color;
       --  Convert to hexadecimal value for the selected RGB component S

    end Color_Types;

    with Ada.Integer_Text_IO;

    package body Color_Types is

       function To_RGB (C : HTML_Color) return RGB is
       begin
          return To_RGB_Lookup_Table (C);
       end To_RGB;

       function To_Int_Color (C : HTML_Color;
                              S : HTML_RGB_Color) return Int_Color is
       begin
          --  Implement function!
          return 0;
       end To_Int_Color;

       function Image (I : Int_Color) return String is
          subtype Str_Range is Integer range 1 .. 10;
          S : String (Str_Range);
       begin
          Ada.Integer_Text_IO.Put (To    => S,
                                   Item  => I,
                                   Base  => 16);
          return S;
       end Image;

       function Image (C : RGB) return String is
       begin
          return ("(Red => "      & Image (C.Red)
                  & ", Green => " & Image (C.Green)
                  & ", Blue => "  & Image (C.Blue)
                  &")");
       end Image;

    end Color_Types;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Color_Types;      use Color_Types;

    procedure Main is
       type Test_Case_Index is
         (HTML_Color_Red_Chk,
          HTML_Color_Green_Chk,
          HTML_Color_Blue_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_HTML_Colors (S : HTML_RGB_Color) is
          begin
             Put_Line ("Selected: " & HTML_RGB_Color'Image (S));
             for I in HTML_Color'Range loop
                Put_Line (HTML_Color'Image (I) & " => "
                          & Image (To_Int_Color (I, S)) & ".");
             end loop;
          end Check_HTML_Colors;

       begin
          case TC is
             when HTML_Color_Red_Chk =>
                Check_HTML_Colors (Red);
             when HTML_Color_Green_Chk =>
                Check_HTML_Colors (Green);
             when HTML_Color_Blue_Chk =>
                Check_HTML_Colors (Blue);
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
