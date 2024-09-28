Tasking
-------

Display Service
~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Display_Service

    --  START LAB IO BLOCK
    in 0:Display_Service_Chk
    out 0:Hello Hello again  55
    --  END LAB IO BLOCK

    package Display_Services is

       task type Display_Service is
          entry Display (S : String);
          entry Display (I : Integer);
       end Display_Service;

    end Display_Services;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Display_Services is

       task body Display_Service is
       begin
          loop
             select
                accept Display (S : String) do
                   Put_Line (S);
                end Display;
             or
                accept Display (I : Integer) do
                   Put_Line (Integer'Image (I));
                end Display;
             or
                terminate;
             end select;
          end loop;
       end Display_Service;

    end Display_Services;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Display_Services; use Display_Services;

    procedure Main is
       type Test_Case_Index is (Display_Service_Chk);

       procedure Check (TC : Test_Case_Index) is
          Display : Display_Service;
       begin
          case TC is
             when Display_Service_Chk =>
                Display.Display ("Hello");
                delay 0.5;
                Display.Display ("Hello again");
                delay 0.5;
                Display.Display (55);
                delay 0.5;
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

Event Manager
~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Event_Manager

    --  START LAB IO BLOCK
    in 0:Event_Manager_Chk
    out 0:Event # 3 Event # 4 Event # 2 Event # 5 Event # 1
    --  END LAB IO BLOCK

    with Ada.Real_Time; use Ada.Real_Time;

    package Event_Managers is

       task type Event_Manager is
          entry Start (ID : Natural);
          entry Event (T : Time);
       end Event_Manager;

    end Event_Managers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Event_Managers is

       task body Event_Manager is
          Event_ID    : Natural := 0;
          Event_Delay : Time;
       begin
          accept Start (ID : Natural) do
             Event_ID := ID;
          end Start;

          accept Event (T : Time) do
             Event_Delay := T;
          end Event;

          delay until Event_Delay;

          Put_Line ("Event #" & Natural'Image (Event_ID));
       end Event_Manager;

    end Event_Managers;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Event_Managers;   use Event_Managers;
    with Ada.Real_Time;    use Ada.Real_Time;

    procedure Main is
       type Test_Case_Index is (Event_Manager_Chk);

       procedure Check (TC : Test_Case_Index) is
          Ev_Mng : array (1 .. 5) of Event_Manager;
       begin
          case TC is
             when Event_Manager_Chk =>
                for I in Ev_Mng'Range loop
                   Ev_Mng (I).Start (I);
                end loop;
                Ev_Mng (1).Event (Clock + Seconds (5));
                Ev_Mng (2).Event (Clock + Seconds (3));
                Ev_Mng (3).Event (Clock + Seconds (1));
                Ev_Mng (4).Event (Clock + Seconds (2));
                Ev_Mng (5).Event (Clock + Seconds (4));
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

Generic Protected Queue
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Tasking.Generic_Protected_Queue

    --  START LAB IO BLOCK
    in 0:Simple_Queue_Chk
    out 0:Value from queue:  1.00000E+01 Value from queue:  1.15000E+01 Value from queue:  1.30000E+01 Value from queue:  1.45000E+01 Value from queue:  1.60000E+01 Value from queue:  1.75000E+01 Value from queue:  1.90000E+01 Value from queue:  2.05000E+01 Value from queue:  2.20000E+01 Value from queue:  2.35000E+01
    in 1:Concurrent_Queue_Chk
    out 1:Value from queue:  100 Value from queue:  101 Value from queue:  102 Value from queue:  103 Value from queue:  104 Value from queue:  105 Value from queue:  106 Value from queue:  107 Value from queue:  108 Value from queue:  109 Value from queue:  110 Value from queue:  111 Value from queue:  112 Value from queue:  113 Value from queue:  114 Value from queue:  115 Value from queue:  116 Value from queue:  117 Value from queue:  118 Value from queue:  119
    --  END LAB IO BLOCK

    generic
       type Queue_Index is mod <>;
       type T is private;
    package Gen_Queues is

       type Queue_Array is array (Queue_Index) of T;

       protected type Queue is
          function Empty return Boolean;
          function Full return Boolean;
          entry Push (V : T);
          entry Pop (V : out T);
       private
          N   : Natural     := 0;
          Idx : Queue_Index := Queue_Array'First;
          A   : Queue_Array;
       end Queue;

    end Gen_Queues;

    package body Gen_Queues is

       protected body Queue is

          function Empty return Boolean is
            (N = 0);

          function Full return Boolean is
             (N = A'Length);

          entry Push (V : T) when not Full is
          begin
             A (Idx) := V;

             Idx := Idx + 1;
             N   := N + 1;
          end Push;

          entry Pop (V : out T) when not Empty is
          begin
             N := N - 1;

             V := A (Idx - Queue_Index (N) - 1);
          end Pop;

       end Queue;

    end Gen_Queues;

    package Queue_Tests is

       procedure Simple_Test;

       procedure Concurrent_Test;

    end Queue_Tests;

    with Ada.Text_IO; use Ada.Text_IO;

    with Gen_Queues;

    package body Queue_Tests is

       Max : constant := 10;
       type Queue_Mod is mod Max;

       procedure Simple_Test is
          package Queues_Float is new Gen_Queues (Queue_Mod, Float);

          Q_F : Queues_Float.Queue;
          V   : Float;
       begin
          V := 10.0;
          while not Q_F.Full loop
             Q_F.Push (V);
             V := V + 1.5;
          end loop;

          while not Q_F.Empty loop
             Q_F.Pop (V);
             Put_Line ("Value from queue: " & Float'Image (V));
          end loop;
       end Simple_Test;

       procedure Concurrent_Test is
          package Queues_Integer is new Gen_Queues (Queue_Mod, Integer);

          Q_I : Queues_Integer.Queue;

          task T_Producer;
          task T_Consumer;

          task body T_Producer is
             V : Integer := 100;
          begin
             for I in 1 .. 2 * Max loop
                Q_I.Push (V);
                V := V + 1;
             end loop;
          end T_Producer;

          task body T_Consumer is
             V : Integer;
          begin
             delay 1.5;

             while not Q_I.Empty loop
                Q_I.Pop (V);
                Put_Line ("Value from queue: " & Integer'Image (V));
                delay 0.2;
             end loop;
          end T_Consumer;
       begin
          null;
       end Concurrent_Test;

    end Queue_Tests;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Queue_Tests;      use Queue_Tests;

    procedure Main is
       type Test_Case_Index is (Simple_Queue_Chk,
                                Concurrent_Queue_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Simple_Queue_Chk =>
                Simple_Test;
             when Concurrent_Queue_Chk =>
                Concurrent_Test;
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
