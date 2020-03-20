Standard library: Dates & Times
===============================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

List of events
--------------

Your goal with this exercise is to create a list of events using the
following format:

.. code-block:: none

    <event_date #1>
        <description of item #1a>
        <description of item #1b>
    <event_date #2>
        <description of item #2a>
        <description of item #2b>

First, you need to declare the :ada:`Event_Item` type in the :ada:`Events`
package. This type contains the *description of the event* mentioned
in the format above. This description can be stored as an unbounded string.
Since we can have multiple events for a specific date, we need a container
to store those items. To implement this, we declare the :ada:`Event_Items`
type in the same package. You can use a vector for this type.

We map event items (as elements of :ada:`Event_Item` type) to specific dates
by using the :ada:`Event_List` of the :ada:`Events.Lists` package. For
the dates, you should use the :ada:`Time` type from the :ada:`Ada.Calendar`
package. Since we expect the events to be ordered by the date, you should
use ordered maps for the :ada:`Event_List` type.

The :ada:`Events.Lists` package contains the API that is used in our test
application. Consider the following example:

.. code-block:: ada

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

The expected output of the :ada:`Test` procedure is:

.. code-block:: none

    EVENTS LIST
    - 2019-04-15
        - Item #1
    - 2019-04-16
        - Item #2
        - Item #3

In the implementation of the :ada:`Display` procedure, make sure to use the
format as shown above. Also, you should use the auxiliary :ada:`Date_Image`
function to display the date in the ``YYYY-MM-DD`` format.

.. code:: ada lab=Solutions.Standard_Library.List_of_Events

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
