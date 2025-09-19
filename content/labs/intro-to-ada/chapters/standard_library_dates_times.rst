Standard library: Dates & Times
===============================

.. include:: ../../../global.txt

Holocene calendar
-----------------

**Goal**: create a function that returns the year in the Holocene calendar.

**Steps**:

    #. Implement the :ada:`To_Holocene_Year` function.

**Requirements**:

    #. The :ada:`To_Holocene_Year` extracts the year from a time object
       (:ada:`Time` type) and returns the corresponding year for the
       :wikipedia:`Holocene calendar <Holocene_calendar>`.

        #. For positive (AD) years, the Holocene year is calculated by adding
           10,000 to the year number.

**Remarks**:

    #. In this exercise, we don't deal with BC years.

    #. Note that the year component of the :ada:`Time` type from the
       :ada:`Ada.Calendar` package is limited to years starting with 1901.


.. code:: ada lab=Solutions.Standard_Library_Dates_Times.Holocene_Calendar

    --  START LAB IO BLOCK
    in 0:Holocene_Chk
    out 0:Year (Gregorian):  2012 Year (Holocene):   12012 Year (Gregorian):  2020 Year (Holocene):   12020
    --  END LAB IO BLOCK

    with Ada.Calendar; use Ada.Calendar;

    function To_Holocene_Year (T : Time) return Integer is
    begin
       return 0;
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
--------------

**Goal**: create a system to manage a list of events.

**Steps**:

    #. Implement the :ada:`Events` package.

        #. Declare the :ada:`Event_Item` type.

        #. Declare the :ada:`Event_Items` type.

    #. Implement the :ada:`Events.Lists` package.

        #. Declare the :ada:`Event_List` type.

        #. Implement the :ada:`Add` procedure.

        #. Implement the :ada:`Display` procedure.

**Requirements**:

    #. The :ada:`Event_Item` type (from the :ada:`Events` package) contains the
       *description of an event*.

       #. This description shall be stored in an access-to-string type.

    #. The :ada:`Event_Items` type stores a list of events.

        #. This will be used later to represent multiple events for a specific
           date.

        #. You shall use a vector for this type.

    #. The :ada:`Events.Lists` package contains the subprograms that are used
       in the test application.

    #. The :ada:`Event_List` type (from the :ada:`Events.Lists` package) maps
       a list of events to a specific date.

        #. You must use the :ada:`Event_Items` type for the list of events.

        #. You shall use the :ada:`Time` type from the :ada:`Ada.Calendar`
           package for the dates.

        #. Since we expect the events to be ordered by the date, you shall
           use ordered maps for the :ada:`Event_List` type.

    #. Procedure :ada:`Add` adds an event into the list of events for a
       specific date.

    #. Procedure :ada:`Display` must display all events for each date (ordered
       by date) using the following format:

        .. code-block:: none

            <event_date #1>
                <description of item #1a>
                <description of item #1b>
            <event_date #2>
                <description of item #2a>
                <description of item #2b>

        #. You should use the auxiliary :ada:`Date_Image` function |mdash|
           available in the body of the :ada:`Events.Lists` package |mdash| to
           display  the date in the ``YYYY-MM-DD`` format.


**Remarks**:

    #. Let's briefly illustrate the expected output of this system.

        #. Consider the following example:

            .. code-block:: ada

                with Ada.Calendar;
                with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

                with Events.Lists;            use Events.Lists;

                procedure Test is
                   EL : Event_List;
                begin
                   EL.Add (Time_Of (2019, 4, 16),
                           "Item #2");
                   EL.Add (Time_Of (2019, 4, 15),
                           "Item #1");
                   EL.Add (Time_Of (2019, 4, 16),
                           "Item #3");
                   EL.Display;
                end Test;

        #. The expected output of the :ada:`Test` procedure must be:

            .. code-block:: none

                EVENTS LIST
                - 2019-04-15
                    - Item #1
                - 2019-04-16
                    - Item #2
                    - Item #3

.. code:: ada lab=Solutions.Standard_Library_Dates_Times.List_of_Events

    --  START LAB IO BLOCK
    in 0:Event_List_Chk
    out 0:EVENTS LIST - 2018-01-01     - New Year's Day - 2018-02-16     - Final check     - Release - 2018-12-03     - Brother's birthday
    --  END LAB IO BLOCK

    package Events is

       type Event_Item is null record;

       type Event_Items is null record;

    end Events;

    with Ada.Calendar; use Ada.Calendar;

    package Events.Lists is

       type Event_List is tagged private;

       procedure Add (Events     : in out Event_List;
                      Event_Time :        Time;
                      Event      :        String);

       procedure Display (Events : Event_List);

    private

       type Event_List is tagged null record;

    end Events.Lists;

    with Ada.Text_IO;             use Ada.Text_IO;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

    package body Events.Lists is

       procedure Add (Events     : in out Event_List;
                      Event_Time : Time;
                      Event      : String) is
       begin
          null;
       end Add;

       function Date_Image (T : Time) return String is
          Date_Img : constant String := Image (T);
       begin
          return Date_Img (1 .. 10);
       end;

       procedure Display (Events : Event_List) is
          T : Time;
       begin
          Put_Line ("EVENTS LIST");
          --  You should use Date_Image (T) here!
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
