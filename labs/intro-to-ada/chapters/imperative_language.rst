:orphan:

Imperative language
===================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Numbers
-------

Your goal with this exercise is to create an application that, given two
integer numbers, displays all numbers in the range starting with the
number closest to zero. In other words:

#. If both numbers are positive, start with the smallest number.

#. If both numbers are negative, start with the greatest number.

#. If one number is positive and the other one is negative, start with the
   one closest to zero. If both numbers are equally close to zero (for
   example, -1 and 1), start with the negative number.

   - Hint: you can use the :ada:`abs` function to help with the
     comparison. By calling :ada:`abs (A)`, you get the absolute value of
     variable :ada:`A`.

For the moment, don't worry about the details of the :ada:`Main` procedure.
You should just focus on implementing the application in the body of the
:ada:`Display_Numbers` procedure.

.. code:: ada lab=Imperative_Language_Numbers

    --  START LAB IO BLOCK
    in 0: 1 5
    out 0: 1 2 3 4 5
    in 1: 5 1
    out 1: 1 2 3 4 5
    in 2: -5 -1
    out 2: -1 -2 -3 -4 -5
    in 3: 5 -1
    out 3: -1 0 1 2 3 4 5
    in 4: -5 1
    out 4: 1 0 -1 -2 -3 -4 -5
    in 5: 1 -1
    out 5: -1 0 1
    --  END LAB IO BLOCK

    procedure Display_Numbers (A, B : Integer);

    procedure Display_Numbers (A, B : Integer) is
    begin
       --  Implement the application here!
       null;
    end Display_Numbers;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_Numbers;

    procedure Main is
       A, B : Integer;
    begin
       if Argument_Count < 2 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 2 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       A := Integer'Value (Argument (1));
       B := Integer'Value (Argument (2));

       Display_Numbers (A, B);
    end Main;
