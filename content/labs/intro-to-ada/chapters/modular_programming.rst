Modular Programming
===================

.. include:: ../../../global.txt

Months
------

**Goal**: create a package to display the months of the year.

**Steps**:

    #. Convert the :ada:`Months` procedure below to a package.

    #. Create the specification and body of the :ada:`Months` package.

**Requirements**:

    #. :ada:`Months` must contain the declaration of strings for each month of
       the year, which are stored in three-character constants based on the
       month's name.

       - For example, the string :ada:`"January"` is stored in the constant
         :ada:`Jan`. These strings are then used by the :ada:`Display_Months`
         procedure, which is also part of the :ada:`Months` package.

**Remarks**:

    #. The goal of this exercise is to create the :ada:`Months` package.

        #. In the code below, :ada:`Months` is declared as a procedure.

            - Therefore, we need to *convert* it into a real package.

        #. You have to modify the procedure declaration and implementation in
           the code below, so that it becomes a package specification and a
           package body.

.. code:: ada lab=Modular_Programming.Months

    --  START LAB IO BLOCK
    in 0:Months_Chk
    out 0:Months: - January - February - March - April - May - June - July - August - September - October - November - December
    --  END LAB IO BLOCK

    --  Create specification for Months package, which includes
    --  the declaration of the Display_Months procedure.
    --
    procedure Months;

    --  Create body of Months package, which includes
    --  the implementation of the Display_Months procedure.
    --
    procedure Months is

       procedure Display_Months is
       begin
          Put_Line ("Months:");
          Put_Line ("- " & Jan);
          Put_Line ("- " & Feb);
          Put_Line ("- " & Mar);
          Put_Line ("- " & Apr);
          Put_Line ("- " & May);
          Put_Line ("- " & Jun);
          Put_Line ("- " & Jul);
          Put_Line ("- " & Aug);
          Put_Line ("- " & Sep);
          Put_Line ("- " & Oct);
          Put_Line ("- " & Nov);
          Put_Line ("- " & Dec);
       end Display_Months;

    begin
       null;
    end Months;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Months;           use Months;

    procedure Main is

       type Test_Case_Index is
         (Months_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Months_Chk =>
                Display_Months;
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

Operations
----------

**Goal**: create a package to perform basic mathematical operations.

**Steps**:

    #. Implement the :ada:`Operations` package.

        #. Declare and implement the :ada:`Add` function.

        #. Declare and implement the :ada:`Subtract` function.

        #. Declare and implement the :ada:`Multiply`: function.

        #. Declare and implement the :ada:`Divide` function.

    #. Implement the :ada:`Operations.Test` package

        #. Declare and implement the :ada:`Display` procedure.

**Requirements**:

    #. Package :ada:`Operations` contains functions for each of the four
       basic mathematical operations for parameters of :ada:`Integer` type:

        #. Function :ada:`Add` performs the addition of :ada:`A` and :ada:`B`
           and returns the result;

        #. Function :ada:`Subtract` performs the subtraction of :ada:`A` and
           :ada:`B` and returns the result;

        #. Function :ada:`Multiply` performs the multiplication of :ada:`A` and
           :ada:`B` and returns the result;

        #. Function :ada:`Divide` performs the division of :ada:`A` and
           :ada:`B` and returns the result.

    #. Package :ada:`Operations.Test` contains the test environment:

        #. Procedure :ada:`Display` must use the functions from
           the parent (:ada:`Operations`) package as indicated by the template
           in the code below.

.. code:: ada lab=Modular_Programming.Operations

    --  START LAB IO BLOCK
    in 0:Operations_Chk
    out 0:Add (100, 2) =  102 Subtract (100, 2) =  98 Multiply (100, 2) =  200 Divide (100, 2) =  50
    in 1:Operations_Display_Chk
    out 1:Operations:  10 +  5 =  15,  10 -  5 =  5,  10 *  5 =  50,  10 /  5 =  2, Operations:  1 +  2 =  3,  1 -  2 = -1,  1 *  2 =  2,  1 /  2 =  0,
    --  END LAB IO BLOCK

    package Operations is

       --  Create specification for Operations package, including the
       --  declaration of the functions mentioned above.
       --

    end Operations;

    package body Operations is

       --  Create body of Operations package.
       --

    end Operations;

    package Operations.Test is

       --  Create specification for Operations package, including the
       --  declaration of the Display procedure:
       --
       --   procedure Display (A, B : Integer);
       --

    end Operations.Test;

    package body Operations.Test is

       --  Implement body of Operations.Test package.
       --

       procedure Display (A, B : Integer) is
          A_Str : constant String := Integer'Image (A);
          B_Str : constant String := Integer'Image (B);
       begin
          Put_Line ("Operations:");
          Put_Line (A_Str & " + " & B_Str & " = "
                    & Integer'Image (Add (A, B))
                    & ",");
          --  Use the line above as a template and add the rest of the
          --  implementation for Subtract, Multiply and Divide.
       end Display;

    end Operations.Test;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Operations;
    with Operations.Test;  use Operations.Test;

    procedure Main is

       type Test_Case_Index is
         (Operations_Chk,
          Operations_Display_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Operations_Chk =>
                Put_Line ("Add (100, 2) = "
                          & Integer'Image (Operations.Add (100, 2)));
                Put_Line ("Subtract (100, 2) = "
                          & Integer'Image (Operations.Subtract (100, 2)));
                Put_Line ("Multiply (100, 2) = "
                          & Integer'Image (Operations.Multiply (100, 2)));
                Put_Line ("Divide (100, 2) = "
                          & Integer'Image (Operations.Divide (100, 2)));
             when Operations_Display_Chk =>
                Display (10, 5);
                Display ( 1, 2);
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
