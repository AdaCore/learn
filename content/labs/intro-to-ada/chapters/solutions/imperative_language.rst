Imperative Language
-------------------

Hello World
~~~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Hello_World

    --  START LAB IO BLOCK
    in 0:
    out 0:Hello World!
    --  END LAB IO BLOCK

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       Put_Line ("Hello World!");
    end Main;

Greetings
~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Greetings

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
          Put_Line ("Hello " & Name & "!");
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
~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Positive_Or_Negative

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
       if X > 0 then
          Put_Line ("Positive");
       elsif X < 0 then
          Put_Line ("Negative");
       else
          Put_Line ("Zero");
       end if;
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
~~~~~~~

.. code:: ada lab=Solutions.Imperative_Language.Numbers

    --  START LAB IO BLOCK
    in 0:1 5
    out 0: 1  2  3  4  5
    in 1:5 1
    out 1: 1  2  3  4  5
    in 2:-5 -1
    out 2:-5 -4 -3 -2 -1
    in 3:5 -1
    out 3:-1  0  1  2  3  4  5
    in 4:-5 1
    out 4:-5 -4 -3 -2 -1  0  1
    in 5:1 -1
    out 5:-1  0  1
    in 6:-1 -5
    out 6:-5 -4 -3 -2 -1
    --  END LAB IO BLOCK

    procedure Display_Numbers (A, B : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Numbers (A, B : Integer) is
       X, Y : Integer;
    begin
       if A <= B then
          X := A;
          Y := B;
       else
          X := B;
          Y := A;
       end if;

       for I in X .. Y loop
          Put_Line (Integer'Image (I));
       end loop;
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
