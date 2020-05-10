Standard library: Strings
=========================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Concatenation
-------------

**Goal**: implement functions to concatenate three strings

**Steps**:

    #. Implement the :ada:`Str_Concat` package.

        #. Implement the :ada:`Concat` function for :ada:`Unbounded_String`.

        #. Implement the :ada:`Concat` function for :ada:`String`.

**Requirements**:

    #. Function :ada:`Concat` receives three strings of :ada:`String` type and
       returns the concatenation of those strings as an unbounded string.

    #. Function :ada:`Concat` receives three unbouded strings and returns the
       concatenation of those strings as a standard string (:ada:`String`
       type).

.. code:: ada lab=Solutions.Standard_Library_Strings.Concatenation

    --  START LAB IO BLOCK
    in 0:Unbounded_String_Chk
    out 0:Hello World!
    in 1:String_Chk
    out 1:This is a test.
    --  END LAB IO BLOCK

    with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

    package Str_Concat is

       function Concat (S1, S2, S3 : String) return Unbounded_String;

       function Concat (S1, S2, S3 : Unbounded_String) return String;

    end Str_Concat;

    package body Str_Concat is

       function Concat (S1, S2, S3 : String) return Unbounded_String is
       begin
          null;
       end Concat;

       function Concat (S1, S2, S3 : Unbounded_String) return String is
       begin
          null;
       end Concat;

    end Str_Concat;

    with Ada.Command_Line;        use Ada.Command_Line;
    with Ada.Text_IO;             use Ada.Text_IO;
    with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

    with Str_Concat;              use Str_Concat;

    procedure Main is
       type Test_Case_Index is
         (Unbounded_String_Chk,
          String_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
             when Unbounded_String_Chk =>
                declare
                   S : constant Unbounded_String := Concat ("Hello", " World", "!");
                begin
                   Put_Line (To_String (S));
                end;
             when String_Chk =>
                declare
                   S : constant String := Concat (To_Unbounded_String ("This"),
                                                  To_Unbounded_String (" is a "),
                                                  To_Unbounded_String ("test."));
                begin
                   Put_Line (S);
                end;
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

        #. Declare the :ada:`Event_Item` subtype.

    #. Implement the :ada:`Events.Lists` package.

        #. Adapt the :ada:`Add` procedure.

        #. Adapt the :ada:`Display` procedure.

**Requirements**:

    #. The :ada:`Event_Item` type (from the :ada:`Events` package) contains the
       *description of an event*.

       #. This description is declared as a subtype of unbounded string.

    #. Procedure :ada:`Add` adds an event into the list of events for a
       specific date.

        #. The declaration of :ada:`E` needs to be adapted to use unbounded
           strings.

    #. Procedure :ada:`Display` must display all events for each date (ordered
       by date) using the following format:

        #. The arguments to :ada:`Put_Line` need to be adapted to use unbounded
           strings.

**Remarks**:

    #. We use the lab on the list of events from the previous chapter
       (:doc:`./standard_library_dates_times`) as a starting point.

.. code:: ada lab=Solutions.Standard_Library_Dates_Times.List_of_Events

    --  START LAB IO BLOCK
    in 0:Unbounded_String_Chk
    out 0:Checked
    in 1:Event_List_Chk
    out 1:EVENTS LIST - 2018-01-01     - New Year's Day - 2018-02-16     - Final check     - Release - 2018-12-03     - Brother's birthday
    --  END LAB IO BLOCK

    with Ada.Containers.Vectors;

    package Events is

       --  subtype Event_Item is

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
    with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

    with Events;
    with Events.Lists;            use Events.Lists;

    procedure Main is
       type Test_Case_Index is
         (Unbounded_String_Chk,
          Event_List_Chk);

       procedure Check (TC : Test_Case_Index) is
          EL : Event_List;
       begin
          case TC is
             when Unbounded_String_Chk =>
                declare
                   S : constant Events.Event_Item := To_Unbounded_String ("Checked");
                begin
                   Put_Line (To_String (S));
                end;
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
