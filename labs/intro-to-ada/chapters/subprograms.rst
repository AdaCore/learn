:orphan:

Subprograms
===========

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Subtract procedure
-------------------

In this exercise, you'll write a procedure called :ada:`Subtract`. As
the name indicates, this procedure performs the operation :ada:`A - B`.

.. code:: ada lab=Subprograms_SubtractProc

    --  START LAB IO BLOCK
    in 0: Sub_10_1_Chk
    out 0: Result: 9
    in 1: Sub_10_100_Chk
    out 1: Result: -90
    in 2: Sub_0_5_Chk
    out 2: Result: -5
    in 3: Sub_0_Minus_5_Chk
    out 3: Result: 5
    --  END LAB IO BLOCK

    --  Write the correct parameters for the procedure below.
    procedure Subtract;

    procedure Subtract is
    begin
       --  Implement the procedure here.
       null;
    end Subtract;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Subtract;

    procedure Main is
       type Test_Case_Index is
         (Sub_10_1_Chk,
          Sub_10_100_Chk,
          Sub_0_5_Chk,
          Sub_0_Minus_5_Chk);

       procedure Check (TC : Test_Case_Index) is
          Result : Integer;
       begin
          case TC is
          when Sub_10_1_Chk =>
             Subtract (10, 1, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_10_100_Chk =>
             Subtract (10, 100, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_5_Chk =>
             Subtract (0, 5, Result);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_Minus_5_Chk =>
             Subtract (0, -5, Result);
             Put_Line ("Result: " & Integer'Image (Result));
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

Subtract function
------------------

In this exercise, you'll rewrite the :ada:`Subtract` procedure as a
function. As in the previous exercise, this function performs the
operation :ada:`A - B` and returns the result.

.. code:: ada lab=Subprograms_SubtractFunc

    --  START LAB IO BLOCK
    in 0: Sub_10_1_Chk
    out 0: Result: 9
    in 1: Sub_10_100_Chk
    out 1: Result: -90
    in 2: Sub_0_5_Chk
    out 2: Result: -5
    in 3: Sub_0_Minus_5_Chk
    out 3: Result: 5
    --  END LAB IO BLOCK

    --  Write the correct signature for the function below.
    --  Don't forget to replace the keyword "procedure" by "function."
    procedure Subtract;

    procedure Subtract is
    begin
       --  Implement the function here!
       null;
    end Subtract;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Subtract;

    procedure Main is
       type Test_Case_Index is
         (Sub_10_1_Chk,
          Sub_10_100_Chk,
          Sub_0_5_Chk,
          Sub_0_Minus_5_Chk);

       procedure Check (TC : Test_Case_Index) is
          Result : Integer;
       begin
          case TC is
          when Sub_10_1_Chk =>
             Result := Subtract (10, 1);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_10_100_Chk =>
             Result := Subtract (10, 100);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_5_Chk =>
             Result := Subtract (0, 5);
             Put_Line ("Result: " & Integer'Image (Result));
          when Sub_0_Minus_5_Chk =>
             Result := Subtract (0, -5);
             Put_Line ("Result: " & Integer'Image (Result));
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

Equality function
-----------------

In this exercise, you'll write a function that compares two values and
returns a flag (:ada:`Boolean` value) indicating whether the values are
equal (flag is :ada:`True`) or not (flag is :ada:`False`).

.. code:: ada lab=Subprograms_EqualityFunc

    --  START LAB IO BLOCK
    in 0: Equal_Chk
    out 0:  0 is equal to 0. 1 is equal to 1. 2 is equal to 2. 3 is equal to 3. 4 is equal to 4. 5 is equal to 5. 6 is equal to 6. 7 is equal to 7. 8 is equal to 8. 9 is equal to 9. 10 is equal to 10.
    in 1: Inequal_Chk
    out 1:  0 isn't equal to -1. 1 isn't equal to 0. 2 isn't equal to 1. 3 isn't equal to 2. 4 isn't equal to 3. 5 isn't equal to 4. 6 isn't equal to 5. 7 isn't equal to 6. 8 isn't equal to 7. 9 isn't equal to 8. 10 isn't equal to 9.
    --  END LAB IO BLOCK

    --  Write the correct signature for the function below.
    --  Don't forget to replace the keyword "procedure" by "function."
    procedure Is_Equal;

    procedure Is_Equal is
    begin
       --  Implement the function here!
       null;
    end Is_Equal;

    with Ada.Command_Line;     use Ada.Command_Line;
    with Ada.Text_IO;          use Ada.Text_IO;

    with Is_Equal;

    procedure Main is
       type Test_Case_Index is
         (Equal_Chk,
          Inequal_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Display_Equal (A, B  : Integer;
                                   Equal : Boolean) is
          begin
             Put (Integer'Image (A));
             if Equal then
                Put (" is equal to ");
             else
                Put (" isn't equal to ");
             end if;
             Put_Line (Integer'Image (B) & ".");
          end Display_Equal;

          Result : Boolean;
       begin
          case TC is
          when Equal_Chk =>
             for I in 0 .. 10 loop
                Result := Is_Equal (I, I);
                Display_Equal (I, I, Result);
             end loop;
          when Inequal_Chk =>
             for I in 0 .. 10 loop
                Result := Is_Equal (I, I - 1);
                Display_Equal (I, I - 1, Result);
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
