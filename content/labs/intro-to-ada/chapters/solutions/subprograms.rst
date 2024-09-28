Subprograms
-----------

Subtract Procedure
~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Subtract_Proc

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

    procedure Subtract (A, B   :     Integer;
                           Result : out Integer);

    procedure Subtract (A, B   :     Integer;
                           Result : out Integer) is
    begin
       Result := A - B;
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

Subtract Function
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Subtract_Func

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

    function Subtract (A, B : Integer) return Integer;

    function Subtract (A, B : Integer) return Integer is
    begin
       return A - B;
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
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.Equality_Func

    --  START LAB IO BLOCK
    in 0:Equal_Chk
    out 0: 0 is equal to  0.  1 is equal to  1.  2 is equal to  2.  3 is equal to  3.  4 is equal to  4.  5 is equal to  5.  6 is equal to  6.  7 is equal to  7.  8 is equal to  8.  9 is equal to  9.  10 is equal to  10.
    in 1:Inequal_Chk
    out 1: 0 isn't equal to -1.  1 isn't equal to  0.  2 isn't equal to  1.  3 isn't equal to  2.  4 isn't equal to  3.  5 isn't equal to  4.  6 isn't equal to  5.  7 isn't equal to  6.  8 isn't equal to  7.  9 isn't equal to  8.  10 isn't equal to  9.
    --  END LAB IO BLOCK

    function Is_Equal (A, B : Integer) return Boolean;

    function Is_Equal (A, B : Integer) return Boolean is
    begin
       return A = B;
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
~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_1

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
       case State is
          when 0 =>
             Put_Line ("Off");
          when 1 =>
             Put_Line ("On: Simple Processing");
          when 2 =>
             Put_Line ("On: Advanced Processing");
          when others =>
             null;
       end case;
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
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_2

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
       return (case State is
               when 0 => "Off",
               when 1 => "On: Simple Processing",
               when 2 => "On: Advanced Processing",
               when others => "");
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
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_3

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
       return not (State = 0);
    end Is_On;

    procedure Display_On_Off (State : Integer);

    with Ada.Text_IO; use Ada.Text_IO;
    with Is_On;

    procedure Display_On_Off (State : Integer) is
    begin
       Put_Line (if Is_On (State) then "On" else "Off");
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
~~~~~~~~~

.. code:: ada lab=Solutions.Subprograms.States_4

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
       State := (if State < 2 then State + 1 else 0);
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
