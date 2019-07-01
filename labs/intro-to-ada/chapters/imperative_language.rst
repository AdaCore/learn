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

Hello World
-----------

Your goal with this exercise is to create an application that displays
the message "Hello World!".

.. code:: ada lab=ImperativeLanguage_HelloWorld

    --  START LAB IO BLOCK
    in 0
    out 0:Hello World!
    --  END LAB IO BLOCK

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       --  Implement the application here!
       null;
    end Main;

Greetings
---------

Your goal with this exercise is to create an application that, given a
name (e.g. "John"), displays the message "Hello John!". In order to do
this, you'll complete the :ada:`Greet` procedure.

    - Hint: you can use the concatenation operator (:ada:`&`).

For the moment, don't worry about the details of the :ada:`Main` procedure.
You should just focus on implementing the application in the body of the
:ada:`Greet` procedure.


.. code:: ada lab=ImperativeLanguage_Greetings

    --  START LAB IO BLOCK
    in 0:John
    out 0:Hello John!
    in 1:Joanna
    out 1:Hello Joanna!
    --  END LAB IO BLOCK

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    procedure Main is

       procedure Greet (Name : String) is
       begin
          --  Implement the application here!
          null;
       end Greet;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Greet (Argument (1));
    end Main;

Positive Or Negative
--------------------

Your goal with this exercise is to create an application that, given an
integer number ``X``, classifies the number as positive, negative or
zero and displays the result. In other words:

#. If ``X > 0``, display ``Positive``.

#. If ``X < 0``, display ``Negative``.

#. If ``X = 0``, display ``Zero``.

You should focus on the :ada:`Classify_Number` procedure.

.. code:: ada lab=ImperativeLanguage_PositiveOrNegative

    --  START LAB IO BLOCK
    in 0:0
    out 0:Zero
    in 1:1
    out 1:Positive
    in 2:-1
    out 2:Negative
    in 3:99999
    out 3:Positive
    in 4:-99999
    out 4:Negative
    --  END LAB IO BLOCK

    procedure Classify_Number (X : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Classify_Number (X : Integer) is
    begin
       --  Implement the application here!
       null;
    end Classify_Number;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Classify_Number;

    procedure Main is
       A : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       A := Integer'Value (Argument (1));

       Classify_Number (A);
    end Main;

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

   - Hint: you can use the :ada:`abs` operator to help with the
     comparison. By calling :ada:`abs A`, you get the absolute value of
     variable :ada:`A`.

For the moment, don't worry about the details of the :ada:`Main` procedure.
You should just focus on implementing the application in the body of the
:ada:`Display_Numbers` procedure.

.. code:: ada lab=ImperativeLanguage_Numbers

    --  START LAB IO BLOCK
    in 0:1 5
    out 0: 1  2  3  4  5
    in 1:5 1
    out 1: 1  2  3  4  5
    in 2:-5 -1
    out 2:-1 -2 -3 -4 -5
    in 3:5 -1
    out 3:-1  0  1  2  3  4  5
    in 4:-5 1
    out 4: 1  0 -1 -2 -3 -4 -5
    in 5:1 -1
    out 5:-1  0  1
    in 6:-1 -5
    out 6:-1 -2 -3 -4 -5
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
