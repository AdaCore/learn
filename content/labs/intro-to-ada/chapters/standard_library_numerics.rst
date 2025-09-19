Standard library: Numerics
==========================

.. include:: ../../../global.txt

Decibel Factor
--------------

**Goal**: implement functions to convert from Decibel values to factors and
vice-versa.

**Steps**:

    #. Implement the :ada:`Decibels` package.

        #. Implement the :ada:`To_Decibel` function.

        #. Implement the :ada:`To_Factor` function.

**Requirements**:

    #. The subtypes :ada:`Decibel` and :ada:`Factor` are based on a
       floating-point type.

    #. Function :ada:`To_Decibel` converts a multiplication factor (or ratio)
       to decibels.

        - For the implementation, use :math:`20 * log_{10}(F)`, where *F* is
          the factor/ratio.

    #. Function :ada:`To_Factor` converts a value in decibels to a
       multiplication factor (or ratio).

        - For the implementation, use :math:`10^{D/20}`, where *D* is the value
          in Decibel.

**Remarks**:

    #. The :wikipedia:`Decibel <Decibel>` is used to
       express the ratio of two values on a logarithmic scale.

        #. For example, an increase of 6 dB corresponds roughly to a
           multiplication by two (or an increase by 100 % of the original
           value).

    #. You can find the functions that you'll need for the calculation in the
       :ada:`Ada.Numerics.Elementary_Functions` package.

.. code:: ada lab=Standard_Library.Decibel_Factor

    --  START LAB IO BLOCK
    in 0:Db_Chk 3.0
    out 0:3.00 dB => Factor of 1.41
    in 1:Db_Chk 6.0
    out 1:6.00 dB => Factor of 2.00
    in 2:Db_Chk 20.0
    out 2:20.00 dB => Factor of 10.00
    in 3:Factor_Chk 2.0
    out 3:Factor of 2.00 => 6.02 dB
    in 4:Factor_Chk 4.0
    out 4:Factor of 4.00 => 12.04 dB
    in 5:Factor_Chk 100.0
    out 5:Factor of 100.00 => 40.00 dB
    --  END LAB IO BLOCK

    package Decibels is

       subtype Decibel is Float;
       subtype Factor  is Float;

       function To_Decibel (F : Factor) return Decibel;

       function To_Factor (D : Decibel) return Factor;

    end Decibels;

    package body Decibels is

       function To_Decibel (F : Factor) return Decibel is
       begin
          return 0.0;
       end To_Decibel;

       function To_Factor (D : Decibel) return Factor is
       begin
          return 0.0;
       end To_Factor;

    end Decibels;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Decibels;         use Decibels;

    procedure Main is
       type Test_Case_Index is
         (Db_Chk,
          Factor_Chk);

       procedure Check (TC : Test_Case_Index; V : Float) is

          package F_IO is new Ada.Text_IO.Float_IO (Factor);
          package D_IO is new Ada.Text_IO.Float_IO (Decibel);

          procedure Put_Decibel_Cnvt (D : Decibel) is
             F : constant Factor := To_Factor (D);
          begin
             D_IO.Put (D, 0, 2, 0);
             Put (" dB => Factor of ");
             F_IO.Put (F, 0, 2, 0);
             New_Line;
          end;

          procedure Put_Factor_Cnvt (F : Factor) is
             D : constant Decibel := To_Decibel (F);
          begin
             Put ("Factor of ");
             F_IO.Put (F, 0, 2, 0);
             Put (" => ");
             D_IO.Put (D, 0, 2, 0);
             Put_Line (" dB");
          end;
       begin
          case TC is
             when Db_Chk =>
                Put_Decibel_Cnvt (Decibel (V));
             when Factor_Chk =>
                Put_Factor_Cnvt (Factor (V));
          end case;
       end Check;

    begin
       if Argument_Count < 2 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 2 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)), Float'Value (Argument (2)));
    end Main;

Root-Mean-Square
----------------

**Goal**: implement a function to calculate the root-mean-square of a sequence
of values.

**Steps**:

    #. Implement the :ada:`Signals` package.

        #. Implement the :ada:`Rms` function.

**Requirements**:

    #. Subtype :ada:`Sig_Value` is based on a floating-point type.

    #. Type :ada:`Signal` is an unconstrained array of :ada:`Sig_Value`
       elements.

    #. Function :ada:`Rms` calculates the RMS of a sequence of values stored in
       an array of type :ada:`Signal`.

        #. See the remarks below for a description of the RMS calculation.

**Remarks**:

    #. The :wikipedia:`root-mean-square <Root_mean_square>`
       (RMS) value is an important information associated with    sequences of
       values.

        #. It's used, for example, as a measurement for signal processing.

        #. It is calculated by:

            #. Creating a sequence :math:`S` with the square of each value of
               an input sequence :math:`S_{in}`.

            #. Calculating the mean value :math:`M` of the sequence :math:`S`.

            #. Calculating the square-root :math:`R` of :math:`M`.

        #. You can optimize the algorithm above by combining steps #1 and #2
           into a single step.

.. code:: ada lab=Standard_Library.Root_Mean_Square

    --  START LAB IO BLOCK
    in 0:Sine_Signal_Chk
    out 0:RMS of Sine Signal: 0.71
    in 1:Square_Signal_Chk
    out 1:RMS of Square Signal: 1.00
    in 2:Triangular_Signal_Chk
    out 2:RMS of Triangular Signal: 0.58
    --  END LAB IO BLOCK

    package Signals is

       subtype Sig_Value is Float;

       type Signal is array (Natural range <>) of Sig_Value;

       function Rms (S : Signal) return Sig_Value;

    end Signals;

    with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

    package body Signals is

       function Rms (S : Signal) return Sig_Value is
       begin
          return 0.0;
       end;

    end Signals;

    package Signals.Std is

       Sample_Rate : Float := 8000.0;

       function Generate_Sine (N : Positive; Freq : Float) return Signal;

       function Generate_Square (N : Positive) return Signal;

       function Generate_Triangular (N : Positive) return Signal;

    end Signals.Std;

    with Ada.Numerics;                      use Ada.Numerics;
    with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

    package body Signals.Std is

       function Generate_Sine (N : Positive; Freq : Float) return Signal is
          S : Signal (0 .. N - 1);
       begin
          for I in S'First .. S'Last loop
             S (I) := 1.0 * Sin (2.0 * Pi * (Freq * Float (I) / Sample_Rate));
          end loop;

          return S;
       end;

       function Generate_Square (N : Positive) return Signal is
          S : constant Signal (0 .. N - 1) := (others => 1.0);
       begin
          return S;
       end;

       function Generate_Triangular (N : Positive) return Signal is
          S      : Signal (0 .. N - 1);
          S_Half : constant Natural := S'Last / 2;
       begin
          for I in S'First .. S_Half loop
             S (I) := 1.0 * (Float (I) / Float (S_Half));
          end loop;
          for I in S_Half .. S'Last loop
             S (I) := 1.0 - (1.0 * (Float (I - S_Half) / Float (S_Half)));
          end loop;

          return S;
       end;

    end Signals.Std;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;

    with Signals;                 use Signals;
    with Signals.Std;             use Signals.Std;

    procedure Main is
       type Test_Case_Index is
         (Sine_Signal_Chk,
          Square_Signal_Chk,
          Triangular_Signal_Chk);

       procedure Check (TC : Test_Case_Index) is
          package Sig_IO is new Ada.Text_IO.Float_IO (Sig_Value);

          N    : constant Positive := 1024;
          S_Si : constant Signal := Generate_Sine (N, 440.0);
          S_Sq : constant Signal := Generate_Square (N);
          S_Tr : constant Signal := Generate_Triangular (N + 1);
       begin
          case TC is
             when Sine_Signal_Chk =>
                Put ("RMS of Sine Signal: ");
                Sig_IO.Put (Rms (S_Si), 0, 2, 0);
                New_Line;
             when Square_Signal_Chk =>
                Put ("RMS of Square Signal: ");
                Sig_IO.Put (Rms (S_Sq), 0, 2, 0);
                New_Line;
             when Triangular_Signal_Chk =>
                Put ("RMS of Triangular Signal: ");
                Sig_IO.Put (Rms (S_Tr), 0, 2, 0);
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

Rotation
--------

**Goal**: use complex numbers to calculate the positions of an object in a
circle after rotation.

**Steps**:

    #. Implement the :ada:`Rotation` package.

        #. Implement the :ada:`Rotation` function.

**Requirements**:

    #. Type :ada:`Complex_Points` is an unconstrained array of complex values.

    #. Function :ada:`Rotation` returns a list of positions (represented by
       the :ada:`Complex_Points` type) when dividing a circle in :ada:`N`
       equal slices.

        #. See the remarks below for a more detailed explanation.

        #. You must use functions from :ada:`Ada.Numerics.Complex_Types` to
           implement :ada:`Rotation`.

    #. Subtype :ada:`Angle` is based on a floating-point type.

    #. Type :ada:`Angles` is an unconstrained array of angles.

    #. Function :ada:`To_Angles` returns a list of angles based on an input
       list of positions.

**Remarks**:

    #. Complex numbers are particularly useful in computer graphics to simplify
       the calculation of rotations.

        #. For example, let's assume you've drawn an object on your screen on
           position (1.0, 0.0).

        #. Now, you want to move this object in a circular path |mdash| i.e.
           make it rotate around position (0.0, 0.0) on your screen.

            - You could use *sine* and *cosine* functions to calculate each
              position of the path.

            - However, you could also calculate the positions using complex
              numbers.

    #. In this exercise, you'll use complex numbers to calculate the positions
       of an object that starts on zero degrees |mdash| on position (1.0, 0.0)
       |mdash| and rotates around (0.0, 0.0) for *N* slices of a circle.

        #. For example, if we divide the circle in four slices, the object's
           path will consist of following points / positions:

            .. code-block:: none

                Point #1: ( 1.0,  0.0)
                Point #2: ( 0.0,  1.0)
                Point #3: (-1.0,  0.0)
                Point #4: ( 0.0, -1.0)
                Point #5: ( 1.0,  0.0)

            .. only:: builder_html

                Or graphically:

                .. figure:: rotation.svg
                   :width: 50%

            #. As expected, point #5 is equal to the starting point (point #1),
               since the object rotates around (0.0, 0.0) and returns to the
               starting point.

        #. We can  also describe this path in terms of angles. The following
           list presents the angles for the path on a four-sliced circle:

            .. code-block:: none

                Point #1:    0.00 degrees
                Point #2:   90.00 degrees
                Point #3:  180.00 degrees
                Point #4:  -90.00 degrees (= 270 degrees)
                Point #5:    0.00 degrees

            #. To rotate a complex number simply multiply it by a unit vector
               whose arg is the radian angle to be rotated:
               :math:`Z = e^\frac{2 \pi}{N}`

.. code:: ada lab=Standard_Library.Rotation

    --  START LAB IO BLOCK
    in 0:Rotation_Chk 4
    out 0:---- Points for  4 slices ---- Point: (1.0,0.0) Point: (0.0,1.0) Point: (-1.0,0.0) Point: (0.0,-1.0) Point: (1.0,0.0)
    in 1:Angles_Chk 4
    out 1:---- Angles for  4 slices ---- Angle: 0.00 degrees Angle: 90.00 degrees Angle: 180.00 degrees Angle: -90.00 degrees Angle: 0.00 degrees
    in 2:Rotation_Chk 8
    out 2:---- Points for  8 slices ---- Point: (1.0,0.0) Point: (0.7,0.7) Point: (0.0,1.0) Point: (-0.7,0.7) Point: (-1.0,0.0) Point: (-0.7,-0.7) Point: (0.0,-1.0) Point: (0.7,-0.7) Point: (1.0,0.0)
    in 3:Angles_Chk 8
    out 3:---- Angles for  8 slices ---- Angle: 0.00 degrees Angle: 45.00 degrees Angle: 90.00 degrees Angle: 135.00 degrees Angle: 180.00 degrees Angle: -135.00 degrees Angle: -90.00 degrees Angle: -45.00 degrees Angle: 0.00 degrees
    in 4:Rotation_Chk 12
    out 4:---- Points for  12 slices ---- Point: (1.0,0.0) Point: (0.9,0.5) Point: (0.5,0.9) Point: (0.0,1.0) Point: (-0.5,0.9) Point: (-0.9,0.5) Point: (-1.0,0.0) Point: (-0.9,-0.5) Point: (-0.5,-0.9) Point: (0.0,-1.0) Point: (0.5,-0.9) Point: (0.9,-0.5) Point: (1.0,0.0)
    in 5:Angles_Chk 12
    out 5:---- Angles for  12 slices ---- Angle: 0.00 degrees Angle: 30.00 degrees Angle: 60.00 degrees Angle: 90.00 degrees Angle: 120.00 degrees Angle: 150.00 degrees Angle: 180.00 degrees Angle: -150.00 degrees Angle: -120.00 degrees Angle: -90.00 degrees Angle: -60.00 degrees Angle: -30.00 degrees Angle: 0.00 degrees
    in 6:Rotation_Chk 16
    out 6:---- Points for  16 slices ---- Point: (1.0,0.0) Point: (0.9,0.4) Point: (0.7,0.7) Point: (0.4,0.9) Point: (0.0,1.0) Point: (-0.4,0.9) Point: (-0.7,0.7) Point: (-0.9,0.4) Point: (-1.0,0.0) Point: (-0.9,-0.4) Point: (-0.7,-0.7) Point: (-0.4,-0.9) Point: (0.0,-1.0) Point: (0.4,-0.9) Point: (0.7,-0.7) Point: (0.9,-0.4) Point: (1.0,0.0)
    in 7:Angles_Chk 16
    out 7:---- Angles for  16 slices ---- Angle: 0.00 degrees Angle: 22.50 degrees Angle: 45.00 degrees Angle: 67.50 degrees Angle: 90.00 degrees Angle: 112.50 degrees Angle: 135.00 degrees Angle: 157.50 degrees Angle: 180.00 degrees Angle: -157.50 degrees Angle: -135.00 degrees Angle: -112.50 degrees Angle: -90.00 degrees Angle: -67.50 degrees Angle: -45.00 degrees Angle: -22.50 degrees Angle: 0.00 degrees
    --  END LAB IO BLOCK

    with Ada.Numerics.Complex_Types;
    use  Ada.Numerics.Complex_Types;

    package Rotation is

       type Complex_Points is array (Positive range <>) of Complex;

       function Rotation (N : Positive) return Complex_Points;

    end Rotation;

    with Ada.Numerics; use Ada.Numerics;

    package body Rotation is

       function Rotation (N : Positive) return Complex_Points is
          C : Complex_Points (1 .. 1) := (others => (0.0, 0.0));
       begin
          return C;
       end;

    end Rotation;

    with Rotation; use Rotation;

    package Angles is

       subtype Angle is Float;

       type Angles is array (Positive range <>) of Angle;

       function To_Angles (C : Complex_Points) return Angles;

    end Angles;

    with Ada.Numerics;               use Ada.Numerics;
    with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;

    package body Angles is

       function To_Angles (C : Complex_Points) return Angles is
       begin
          return A : Angles (C'Range) do
             for I in A'Range loop
                A (I) := Argument (C (I)) / Pi * 180.0;
             end loop;
          end return;
       end To_Angles;

    end Angles;

    package Rotation.Tests is

       procedure Test_Rotation (N : Positive);

       procedure Test_Angles (N : Positive);

    end Rotation.Tests;

    with Ada.Text_IO;            use Ada.Text_IO;
    with Ada.Text_IO.Complex_IO;
    with Ada.Numerics;           use Ada.Numerics;

    with Angles;                 use Angles;

    package body Rotation.Tests is

       package C_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
       package F_IO is new Ada.Text_IO.Float_IO (Float);

       --
       --  Adapt value due to floating-point inaccuracies
       --

       function Adapt (C : Complex) return Complex is
          function Check_Zero (F : Float) return Float is
            (if F <= 0.0 and F >= -0.01 then 0.0 else F);
       begin
          return C_Out : Complex := C do
             C_Out.Re := Check_Zero (C_Out.Re);
             C_Out.Im := Check_Zero (C_Out.Im);
          end return;
       end Adapt;

       function Adapt (A : Angle) return Angle is
         (if A <= -179.99 and A >= -180.01 then 180.0 else A);

       procedure Test_Rotation (N : Positive) is
          C : constant Complex_Points := Rotation (N);
       begin
          Put_Line ("---- Points for " & Positive'Image (N) & " slices ----");
          for V of C loop
             Put ("Point: ");
             C_IO.Put (Adapt (V), 0, 1, 0);
             New_Line;
          end loop;
       end Test_Rotation;

       procedure Test_Angles (N : Positive) is
          C : constant Complex_Points := Rotation (N);
          A : constant Angles.Angles  := To_Angles (C);
       begin
          Put_Line ("---- Angles for " & Positive'Image (N) & " slices ----");
          for V of A loop
             Put ("Angle: ");
             F_IO.Put (Adapt (V), 0, 2, 0);
             Put_Line (" degrees");
          end loop;
       end Test_Angles;

    end Rotation.Tests;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;

    with Rotation.Tests;          use Rotation.Tests;

    procedure Main is
       type Test_Case_Index is
         (Rotation_Chk,
          Angles_Chk);

       procedure Check (TC : Test_Case_Index; N : Positive) is
       begin
          case TC is
             when Rotation_Chk =>
                Test_Rotation (N);
             when Angles_Chk =>
                Test_Angles (N);
          end case;
       end Check;

    begin
       if Argument_Count < 2 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 2 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)), Positive'Value (Argument (2)));
    end Main;
