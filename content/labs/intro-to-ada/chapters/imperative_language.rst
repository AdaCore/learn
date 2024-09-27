Imperative language
===================

.. include:: ../../../courses/global.txt

For the exercises below (except for the first one), don't worry about the
details of the :ada:`Main` procedure. You should just focus on implementing the
application in the subprogram specified by the exercise.

Hello World
-----------

**Goal**: create a "Hello World!" application.

**Steps**:

    #. Complete the :ada:`Main` procedure.

**Requirements**:

    #. The application must display the message "Hello World!".

**Remarks**:

    #. The part that you have to modify is indicated by the
       :ada:`--  Implement the application here!` comment in the source code.

.. code:: ada lab=Imperative_Language.Hello_World

    --  START LAB IO BLOCK
    in 0:
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

**Goal**: create an application that greets a person.

**Steps**:

    #. Complete the :ada:`Greet` procedure.

**Requirements**:

    #. Given an input string ``<name>``, procedure :ada:`Greet` must display
       the message "Hello <name>!".

        #. For example, if the name is "John", it displays the message
           "Hello John!".

**Remarks**:

    #. You can use the concatenation operator (:ada:`&`).

    #. The part that you have to modify is indicated by the
       :ada:`--  Implement the application here!` comment in the source code.

.. code:: ada lab=Imperative_Language.Greetings

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

**Goal**: create an application that classifies integer numbers.

**Steps**:

    #. Complete the :ada:`Classify_Number` procedure.

**Requirements**:

    #. Given an integer number ``X``, procedure :ada:`Classify_Number` must
       classify ``X`` as positive, negative or zero and display the result:

        #. If ``X > 0``, it displays ``Positive``.

        #. If ``X < 0``, it displays ``Negative``.

        #. If ``X = 0``, it displays ``Zero``.

.. only:: builder_html

    **Remarks**:

        #. If you're using the tabbed editor view, you see the following
           source-code tabs:

            - :file:`classify_number.ads`: this tab has the specification of
              the :ada:`Classify_Number` procedure;

            - :file:`classify_number.adb`: this tab has the implementation
              (body) of the :ada:`Classify_Number` procedure;

            - :file:`main.adb`: this tab has the implementation (body) of the
              :ada:`Main` test procedure.

        #. You're supposed to edit the body of the :ada:`Classify_Number`
           procedure.

            - In the tabbed editor view, you should select the
              :file:`classify_number.adb` tab by clicking on it.

            - In the non-tabbed editor view, you should click on the body of
              the :ada:`Classify_Number` procedure (which is the second entry).

        #. The part that you have to modify is indicated by the
           :ada:`--  Implement the application here!` comment in the source
           code.

.. code:: ada lab=Imperative_Language.Positive_Or_Negative

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

**Goal**: create an application that displays numbers in a specific order.

**Steps**:

    #. Complete the :ada:`Display_Numbers` procedure.

**Requirements**:

    #. Given two integer numbers, :ada:`Display_Numbers` displays all numbers
       in the range starting with the smallest number.

.. only:: builder_html

    **Remarks**:

        #. You're supposed to edit the body of the :ada:`Display_Numbers`
           procedure.

            - In the tabbed editor view, select the :file:`display_numbers.adb`
              tab.

            - In the non-tabbed editor view, click on the body of the
              :ada:`Display_Numbers` procedure.

        #. The part that you have to modify is indicated by the
           :ada:`--  Implement the application here!` comment in the source
           code.

.. code:: ada lab=Imperative_Language.Numbers

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
