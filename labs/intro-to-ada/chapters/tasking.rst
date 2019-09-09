:orphan:

Tasking
=======

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Display Service
---------------

Your goal with this exercise is to create a simple service using tasking
that displays messages to the user. You should implement a task type
:ada:`Display_Service` that displays messages received by the
:ada:`Display` entry. These messages can be passed to the :ada:`Display`
entry either as a string parameter or an :ada:`Integer` parameter.

.. code:: ada lab=Tasking.Display_Service

    --  START LAB IO BLOCK
    in 0:Display_Service_Chk
    out 0:Hello Hello again  55
    --  END LAB IO BLOCK

    package Display_Services is

    end Display_Services;

    package body Display_Services is

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
-------------

In this exercise, you'll implement a simple event manager using the task
type :ada:`Event_Manager` from the :ada:`Event_Managers` package below.
The event manager must have two entries:

    - :ada:`Start`, which starts the event manager with an event ID;

        - You can use the :ada:`Natural` type for this ID.

    - :ada:`Event`, which delays the task for a certain amount of time and
      then displays the event ID as a user message.

        - You should use the :ada:`Time` type from the :ada:`Ada.Real_Time`
          package for the time parameter.

The format for the user message displayed the event manager is
``Event #<event_id>``. You should use ``Natural'Image`` to display the ID,
as indicated in the body of the :ada:`Event_Managers` package.

The event manager has a similar behavior as an alarm: its purpose is to
just display the event ID at the correct time. After this is done, the
task must finish.

Note that the test application below creates an array of event managers
with different delays.

.. code:: ada lab=Tasking.Event_Manager

    --  START LAB IO BLOCK
    in 0:Event_Manager_Chk
    out 0:Event # 3 Event # 4 Event # 2 Event # 5 Event # 1
    --  END LAB IO BLOCK

    package Event_Managers is

    end Event_Managers;

    package body Event_Managers is

          --   Don't forget to display the event ID:
          --
          --   Put_Line ("Event #" & Natural'Image (Event_ID));

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
-----------------------

The goal of this exercise is to create a queue container by declaring and
implementing a protected type :ada:`Queue` as part of the generic package
:ada:`Gen_Queues`.

These are the formal parameters for the generic package:

    #. a formal modular type;

        - This modular type should be used by the :ada:`Queue` to declare
          an array that stores the elements of the queue.

        - The modulus of the modular type must correspond to the maximum
          number of elements of the queue.

    #. the data type of the elements of the queue.

        - Select a formal parameter that allows you to store elements of
          any data type in the queue.

These are the operations of the :ada:`Queue` type:

    - a function :ada:`Empty` that indicates whether the queue is empty;

    - a function :ada:`Full` that indicates whether the queue is full;

    - an entry :ada:`Push` to store an element in the queue;

    - an entry :ada:`Pop` to remove an element from the queue.

As a bonus exercise, you can analyze the body of the :ada:`Queue_Tests`
package and understand how the :ada:`Queue` type is used there. In
particular, the procedure :ada:`Concurrent_Test` implements two tasks
(:ada:`T_Producer` and :ada:`T_Consumer`) that make use of the queue
concurrently.

.. code:: ada lab=Tasking.Generic_Protected_Queue

    --  START LAB IO BLOCK
    in 0:Simple_Queue_Chk
    out 0:Value from queue:  1.00000E+01 Value from queue:  1.15000E+01 Value from queue:  1.30000E+01 Value from queue:  1.45000E+01 Value from queue:  1.60000E+01 Value from queue:  1.75000E+01 Value from queue:  1.90000E+01 Value from queue:  2.05000E+01 Value from queue:  2.20000E+01 Value from queue:  2.35000E+01
    in 1:Concurrent_Queue_Chk
    out 1:Value from queue:  100 Value from queue:  101 Value from queue:  102 Value from queue:  103 Value from queue:  104 Value from queue:  105 Value from queue:  106 Value from queue:  107 Value from queue:  108 Value from queue:  109 Value from queue:  110 Value from queue:  111 Value from queue:  112 Value from queue:  113 Value from queue:  114 Value from queue:  115 Value from queue:  116 Value from queue:  117 Value from queue:  118 Value from queue:  119
    --  END LAB IO BLOCK

    package Gen_Queues is

    end Gen_Queues;

    package body Gen_Queues is

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
