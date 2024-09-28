Standard library: Dates & Times
-------------------------------

Holocene calendar
~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Standard_Library_Dates_Times.Holocene_Calendar

    --  START LAB IO BLOCK
    in 0:Holocene_Chk
    out 0:Year (Gregorian):  2012 Year (Holocene):   12012 Year (Gregorian):  2020 Year (Holocene):   12020
    --  END LAB IO BLOCK

    with Ada.Calendar; use Ada.Calendar;

    function To_Holocene_Year (T : Time) return Integer is
    begin
       return Year (T) + 10_000;
    end To_Holocene_Year;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;
    with Ada.Calendar;            use Ada.Calendar;

    with To_Holocene_Year;

    procedure Main is
       type Test_Case_Index is
         (Holocene_Chk);

       procedure Display_Holocene_Year (Y : Year_Number) is
          HY : Integer;
       begin
          HY := To_Holocene_Year (Time_Of (Y, 1, 1));
          Put_Line ("Year (Gregorian): " & Year_Number'Image (Y));
          Put_Line ("Year (Holocene):  " & Integer'Image (HY));
       end Display_Holocene_Year;

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Holocene_Chk =>
                Display_Holocene_Year (2012);
                Display_Holocene_Year (2020);
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

List of events
~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Standard_Library_Dates_Times.List_of_Events

    --  START LAB IO BLOCK
    in 0:Event_List_Chk
    out 0:EVENTS LIST - 2018-01-01     - New Year's Day - 2018-02-16     - Final check     - Release - 2018-12-03     - Brother's birthday
    --  END LAB IO BLOCK

    with Ada.Containers.Vectors;

    package Events is

       type Event_Item is access String;

       package Event_Item_Containers is new
         Ada.Containers.Vectors
           (Index_Type   => Positive,
            Element_Type => Event_Item);

       subtype Event_Items is Event_Item_Containers.Vector;

    end Events;

    with Ada.Calendar;                use Ada.Calendar;
    with Ada.Containers.Ordered_Maps;

    package Events.Lists is

       type Event_List is tagged private;

       procedure Add (Events     : in out Event_List;
                      Event_Time :        Time;
                      Event      :        String);

       procedure Display (Events : Event_List);

    private

       package Event_Time_Item_Containers is new
         Ada.Containers.Ordered_Maps
           (Key_Type         => Time,
            Element_Type     => Event_Items,
            "="              => Event_Item_Containers."=");

       type Event_List is new Event_Time_Item_Containers.Map with null record;

    end Events.Lists;

    with Ada.Text_IO;             use Ada.Text_IO;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

    package body Events.Lists is

       procedure Add (Events     : in out Event_List;
                      Event_Time : Time;
                      Event      : String) is
          use Event_Item_Containers;
          E : constant Event_Item := new String'(Event);
       begin
          if not Events.Contains (Event_Time) then
             Events.Include (Event_Time, Empty_Vector);
          end if;
          Events (Event_Time).Append (E);
       end Add;

       function Date_Image (T : Time) return String is
          Date_Img : constant String := Image (T);
       begin
          return Date_Img (1 .. 10);
       end;

       procedure Display (Events : Event_List) is
          use Event_Time_Item_Containers;
          T : Time;
       begin
          Put_Line ("EVENTS LIST");
          for C in Events.Iterate loop
             T := Key (C);
             Put_Line ("- " & Date_Image (T));
             for I of Events (C) loop
                Put_Line ("    - " & I.all);
             end loop;
          end loop;
       end Display;

    end Events.Lists;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;
    with Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

    with Events.Lists;            use Events.Lists;

    procedure Main is
       type Test_Case_Index is
         (Event_List_Chk);

       procedure Check (TC : Test_Case_Index) is
          EL : Event_List;
       begin
          case TC is
             when Event_List_Chk =>
                EL.Add (Time_Of (2018, 2, 16),
                        "Final check");
                EL.Add (Time_Of (2018, 2, 16),
                        "Release");
                EL.Add (Time_Of (2018, 12, 3),
                        "Brother's birthday");
                EL.Add (Time_Of (2018, 1, 1),
                        "New Year's Day");
                EL.Display;
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
