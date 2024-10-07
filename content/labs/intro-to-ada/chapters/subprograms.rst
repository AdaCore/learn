Subprograms
===========

.. include:: ../../../courses/global.txt

Subtract procedure
-------------------

**Goal**: write a procedure that subtracts two numbers.

**Steps**:

    #. Complete the procedure :ada:`Subtract`.

**Requirements**:

    #. :ada:`Subtract` performs the operation :ada:`A - B`.

.. code:: ada lab=Subprograms.Subtract_Proc

    --  START LAB IO BLOCK
    in 0:Sub_10_1_Chk
    out 0:Result:  9
    in 1:Sub_10_100_Chk
    out 1:Result: -90
    in 2:Sub_0_5_Chk
    out 2:Result: -5
    in 3:Sub_0_Minus_5_Chk
    out 3:Result:  5
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

**Goal**: write a function that subtracts two numbers.

**Steps**:

    #. Rewrite the :ada:`Subtract` procedure from the previous exercise as a
       function.

**Requirements**:

    #. :ada:`Subtract` performs the operation :ada:`A - B` and returns the
       result.

.. code:: ada lab=Subprograms.Subtract_Func

    --  START LAB IO BLOCK
    in 0:Sub_10_1_Chk
    out 0:Result:  9
    in 1:Sub_10_100_Chk
    out 1:Result: -90
    in 2:Sub_0_5_Chk
    out 2:Result: -5
    in 3:Sub_0_Minus_5_Chk
    out 3:Result:  5
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

**Goal**: write a function that compares two values and returns a flag.

**Steps**:

    #. Complete the :ada:`Is_Equal` subprogram.

**Requirements**:

    #. :ada:`Is_Equal` returns a flag as a :ada:`Boolean` value.

    #. The flag must indicate whether the values are equal (flag is
       :ada:`True`) or not (flag is :ada:`False`).

.. code:: ada lab=Subprograms.Equality_Func

    --  START LAB IO BLOCK
    in 0:Equal_Chk
    out 0: 0 is equal to  0.  1 is equal to  1.  2 is equal to  2.  3 is equal to  3.  4 is equal to  4.  5 is equal to  5.  6 is equal to  6.  7 is equal to  7.  8 is equal to  8.  9 is equal to  9.  10 is equal to  10.
    in 1:Inequal_Chk
    out 1: 0 isn't equal to -1.  1 isn't equal to  0.  2 isn't equal to  1.  3 isn't equal to  2.  4 isn't equal to  3.  5 isn't equal to  4.  6 isn't equal to  5.  7 isn't equal to  6.  8 isn't equal to  7.  9 isn't equal to  8.  10 isn't equal to  9.
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

States
------

**Goal**: write a procedure that displays the state of a machine.

**Steps**:

    #. Complete the procedure :ada:`Display_State`.

**Requirements**:

    #. The states can be set according to the following numbers:

        +--------+-------------------------+
        | Number | State                   |
        +========+=========================+
        | 0      | Off                     |
        +--------+-------------------------+
        | 1      | On: Simple Processing   |
        +--------+-------------------------+
        | 2      | On: Advanced Processing |
        +--------+-------------------------+

    #. The procedure :ada:`Display_State` receives the number corresponding to
       a state and displays the state (indicated by the table above) as a user
       message.

**Remarks**:

    #. You can use a case statement to implement this procedure.

.. code:: ada lab=Subprograms.States_1

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off
    in 1:1
    out 1:On: Simple Processing
    in 2:2
    out 2:On: Advanced Processing
    --  END LAB IO BLOCK

    procedure Display_State (State : Integer);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_State (State : Integer) is
    begin
       null;
    end Display_State;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_State;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Display_State (State);
    end Main;

States #2
---------

**Goal**: write a function that returns the state of a machine.

**Steps**:

    #. Implement the function :ada:`Get_State`.

**Requirements**:

    #. Implement same state machine as in the previous exercise.

    #. Function :ada:`Get_State` must return the state as a string.

**Remarks**:

    #. You can implement a function returning a string by simply using quotes in
       a return statement. For example:

        .. code:: ada run_button project=Subprograms.States_2_Example

            function Get_Hello return String;

            function Get_Hello return String is
            begin
               return "Hello";
            end Get_Hello;

            with Ada.Text_IO;      use Ada.Text_IO;
            with Get_Hello;

            procedure Main is
               S : constant String := Get_Hello;
            begin
               Put_Line (S);
            end Main;

    #. You can reuse your previous implementation and replace it by a case
       expression.

        #. For values that do not correspond to a state, you can simply
           return an empty string (:ada:`""`).

.. code:: ada lab=Subprograms.States_2

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off
    in 1:1
    out 1:On: Simple Processing
    in 2:2
    out 2:On: Advanced Processing
    --  END LAB IO BLOCK

    function Get_State (State : Integer) return String;

    function Get_State (State : Integer) return String is
    begin
       return "";
    end Get_State;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Get_State;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Put_Line (Get_State (State));
    end Main;

States #3
---------

**Goal**: implement an on/off indicator for a state machine.

**Steps**:

    #. Implement the function :ada:`Is_On`.

    #. Implement the procedure :ada:`Display_On_Off`.

**Requirements**:

    #. Implement same state machine as in the previous exercise.

    #. Function :ada:`Is_On` returns:

        - :ada:`True` if the machine is on;
        - otherwise, it returns :ada:`False`.

    #. Procedure :ada:`Display_On_Off` displays the message

        - "On" if the machine is on, or
        - "Off" otherwise.

    #. :ada:`Is_On` must be called in the implementation of
       :ada:`Display_On_Off`.

**Remarks**:

    #. You can implement both subprograms using if expressions.

.. code:: ada lab=Subprograms.States_3

    --  START LAB IO BLOCK
    in 0:0
    out 0:Off FALSE
    in 1:1
    out 1:On TRUE
    in 2:2
    out 2:On TRUE
    --  END LAB IO BLOCK

    function Is_On (State : Integer) return Boolean;

    function Is_On (State : Integer) return Boolean is
    begin
       return False;
    end Is_On;

    procedure Display_On_Off (State : Integer);

    with Ada.Text_IO; use Ada.Text_IO;
    with Is_On;

    procedure Display_On_Off (State : Integer) is
    begin
       Put_Line ("");
    end Display_On_Off;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_On_Off;
    with Is_On;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Display_On_Off (State);
       Put_Line (Boolean'Image (Is_On (State)));
    end Main;

States #4
---------

**Goal**: implement a procedure to update the state of a machine.

**Steps**:

    #. Implement the procedure :ada:`Set_Next`.

**Requirements**:

    #. Implement the same state machine as in the previous exercise.

    #. Procedure :ada:`Set_Next` updates the machine's state with the next one
       in a *circular* manner:

        - In general, after a call to :ada:`Set_Next` for an integer variable
          :ada:`N` (:ada:`Set_Next (N)`), the new value of :ada:`N` must be
          the next number for that variable :ada:`N` (i.e.,
          :ada:`N := N'Old + 1`).

            - In other words, a call to :ada:`Set_Next (N)` has the same effect
              as :ada:`N := N + 1`.

            - For example, after the statements :ada:`N := 1; Set_Next (N);`,
              we have that :ada:`N = 2`.

        - However, if the state is the last valid one for the machine (which,
          for this exercise, it's 2), the next state must be the first valid
          one (in this case: 0).

            - In other words, for :ada:`N := 2; Set_Next (N);`, we have that
              :ada:`N = 0`.


**Remarks**:

    #. You can use an if expression to implement :ada:`Set_Next`.

.. code:: ada lab=Subprograms.States_4

    --  START LAB IO BLOCK
    in 0:0
    out 0: 1
    in 1:1
    out 1: 2
    in 2:2
    out 2: 0
    --  END LAB IO BLOCK

    procedure Set_Next (State : in out Integer);

    procedure Set_Next (State : in out Integer) is
    begin
       null;
    end Set_Next;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Set_Next;

    procedure Main is
       State : Integer;
    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       State := Integer'Value (Argument (1));

       Set_Next (State);
       Put_Line (Integer'Image (State));
    end Main;
