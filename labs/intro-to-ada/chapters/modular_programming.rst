:orphan:

Modular Programming
===================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Months
------

In this exercise, you'll create the specification and body of the
:ada:`Months` package. This package must contain the declaration of
strings for each month of the year, which are stored in three-character
constants based on the month's name. For example, the string
:ada:`"January"` is stored in the constant :ada:`Jan`. These strings are
then used by the :ada:`Display_Months` procedure, which is also part of
the :ada:`Months` package.

.. code:: ada lab=ModularProgramming_Months

    --  START LAB IO BLOCK
    in 0: Months_Chk
    out 0: Months: - January - February - March - April - May - June - July - August - September - October - November - December
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

The goal of this exercise is to create two packages:

- Package :ada:`Operations`, which contains functions for each of the four
  basic mathematical operations for parameters of :ada:`Integer` type:

  - function :ada:`Add`: performs the addition of :ada:`A` and :ada:`B`
    and returns the result;

  - function :ada:`Subtract`: performs the subtraction of :ada:`A` and
    :ada:`B` and returns the result;

  - function :ada:`Multiply`: performs the multiplication of :ada:`A` and
    :ada:`B` and returns the result;

  - function :ada:`Divide`: performs the division of :ada:`A` and
    :ada:`B` and returns the result.

- Package :ada:`Operations_Test`, which contains the
  :ada:`Display_Operations` procedure. This procedure makes use of the
  functions from the :ada:`Operations` package.

.. code:: ada lab=ModularProgramming_Operations

    --  START LAB IO BLOCK
    in 0: Operations_Chk
    out 0: Add (100, 2) =  102 Subtract (100, 2) =  98 Multiply (100, 2) =  200 Divide (100, 2) =  50
    in 1: Operations_Display_Chk
    out 1: Operations: 10 + 5 = 15, 10 - 5 = 5, 10 * 5 = 50, 10 / 5 = 2, Operations: 1 + 2 = 3, 1 - 2 = -1, 1 * 2 = 2, 1 / 2 = 0,
    --  END LAB IO BLOCK

    --  Create specification for Operations package, including the
    --  declaration of the functions mentioned above.
    --
    procedure Operations;

    --  Create body of Operations package.
    --
    procedure Operations is
    begin
       null;
    end Operations;

    --  Create specification for Operations package, including the
    --  declaration of the Display_Operations procedure:
    --
    --   procedure Display_Operations (A, B : Integer);
    --
    procedure Operations_Test;

    --  Create body of Operations_Test package.
    --
    procedure Operations_Test is

       procedure Display_Operations (A, B : Integer) is
          A_Str : constant String := Integer'Image (A);
          B_Str : constant String := Integer'Image (B);
       begin
          Put_Line ("Operations:");
          Put_Line (A_Str & " + " & B_Str & " = "
                    & Integer'Image (Add (A, B))
                    & ",");
          --  Use the line above as a template and add the rest of the
          --  implementation for Subtract, Multiply and Divide.
       end Display_Operations;
    begin
       null;
    end Operations_Test;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Operations;
    with Operations_Test;  use Operations_Test;

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
                Display_Operations (10, 5);
                Display_Operations ( 1, 2);
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
