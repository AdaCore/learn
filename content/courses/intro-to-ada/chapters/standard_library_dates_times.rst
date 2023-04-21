Standard library: Dates & Times
===============================

.. include:: ../../global.txt

The standard library supports processing of dates and times using two
approaches:

- *Calendar* approach, which is suitable for handling dates and times in
  general;

- *Real-time* approach, which is better suited for real-time applications
  that require enhanced precision |mdash| for example, by having access to an
  absolute clock and handling time spans. Note that this approach only supports
  times, not dates.

The following sections present these two approaches.

.. _Intro_Ada_Dates_Times:

Date and time handling
----------------------

The :ada:`Ada.Calendar` package supports handling of dates and times. Let's
look at a simple example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Current_Time

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Calendar; use Ada.Calendar;

    with Ada.Calendar.Formatting;
    use  Ada.Calendar.Formatting;

    procedure Display_Current_Time is
       Now : Time := Clock;
    begin
       Put_Line ("Current time: " & Image (Now));
    end Display_Current_Time;

This example displays the current date and time, which is retrieved by a
call to the :ada:`Clock` function. We call the function :ada:`Image` from the
:ada:`Ada.Calendar.Formatting` package to get a :ada:`String` for the current
date and time. We could instead retrieve each component using the :ada:`Split`
function. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Current_Year

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Calendar; use Ada.Calendar;

    procedure Display_Current_Year is
       Now         : Time := Clock;

       Now_Year    : Year_Number;
       Now_Month   : Month_Number;
       Now_Day     : Day_Number;
       Now_Seconds : Day_Duration;
    begin
       Split (Now,
              Now_Year,
              Now_Month,
              Now_Day,
              Now_Seconds);

       Put_Line ("Current year  is: "
                 & Year_Number'Image (Now_Year));
       Put_Line ("Current month is: "
                 & Month_Number'Image (Now_Month));
       Put_Line ("Current day   is: "
                 & Day_Number'Image (Now_Day));
    end Display_Current_Year;

Here, we're retrieving each element and displaying it separately.

Delaying using date
~~~~~~~~~~~~~~~~~~~

You can delay an application so that it restarts at a specific date and
time. We saw something similar in the chapter on tasking.  You do this
using a :ada:`delay until` statement. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Delay_Next_Specific_Time

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Calendar; use Ada.Calendar;

    with Ada.Calendar.Formatting;
    use  Ada.Calendar.Formatting;

    with Ada.Calendar.Time_Zones;
    use  Ada.Calendar.Time_Zones;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        :=
         Ada.Calendar.Formatting.Time_Of
           (Year        => 2018,
            Month       => 5,
            Day         => 1,
            Hour        => 15,
            Minute      => 0,
            Second      => 0,
            Sub_Second  => 0.0,
            Leap_Second => False,
            Time_Zone   => TZ);

       --  Next = 2018-05-01 15:00:00.00
       --         (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this example, we specify the date and time by initializing :ada:`Next`
using a call to :ada:`Time_Of`, a function taking the various components
of a date (year, month, etc) and returning an element of the :ada:`Time`
type. Because the date specified is in the past, the :ada:`delay
until` statement won't produce any noticeable effect. However, if we
passed a date in the future, the program would wait until that
specific date and time arrived.

Here we're converting the time to the local timezone. If we don't specify a
timezone, *Coordinated Universal Time* (abbreviated to UTC) is used by
default. By retrieving the time offset to UTC with a call to
:ada:`UTC_Time_Offset` from the :ada:`Ada.Calendar.Time_Zones` package, we can
initialize :ada:`TZ` and use it in the call to :ada:`Time_Of`.  This is all we
need do to make the information provided to :ada:`Time_Of` relative to the
local time zone.

We could achieve a similar result by initializing :ada:`Next` with a
:ada:`String`. We can do this with a call to :ada:`Value` from the
:ada:`Ada.Calendar.Formatting` package. This is the modified code:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Delay_Next_Specific_Time

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Calendar; use Ada.Calendar;

    with Ada.Calendar.Formatting;
    use  Ada.Calendar.Formatting;

    with Ada.Calendar.Time_Zones;
    use  Ada.Calendar.Time_Zones;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        :=
         Ada.Calendar.Formatting.Value
           ("2018-05-01 15:00:00.00", TZ);

       --  Next = 2018-05-01 15:00:00.00
       --         (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this example, we're again using :ada:`TZ` in the call to :ada:`Value` to
adjust the input time to the current time zone.

In the examples above, we were delaying to a specific date and time.
Just like we saw in the tasking chapter, we could instead specify the
delay relative to the current time. For example, we could delay by 5
seconds, using the current time:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Delay_Next

    with Ada.Calendar; use Ada.Calendar;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Display_Delay_Next is
       D    : Duration := 5.0;
       --                 ^ seconds
       Now  : Time     := Clock;
       Next : Time     := Now + D;
       --                       ^ use duration to
       --                         specify next
       --                         point in time
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (D)
                 & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next;

Here, we're specifying a duration of 5 seconds in :ada:`D`, adding it to the
current time from :ada:`Now`, and storing the sum in :ada:`Next`. We then use it
in the :ada:`delay until` statement.

Real-time
---------

In addition to :ada:`Ada.Calendar`, the standard library also supports time
operations for real-time applications. These are included in the
:ada:`Ada.Real_Time` package. This package also include a :ada:`Time` type.
However, in the :ada:`Ada.Real_Time` package, the :ada:`Time` type is used to
represent an absolute clock and handle a time span. This contrasts with the
:ada:`Ada.Calendar`, which uses the :ada:`Time` type to represent dates and
times.

In the previous section, we used the :ada:`Time` type from the
:ada:`Ada.Calendar` and the :ada:`delay until` statement to delay an
application by 5 seconds. We could have used the :ada:`Ada.Real_Time`
package instead. Let's modify that example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Delay_Next_Real_Time

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Delay_Next_Real_Time is
       D     : Time_Span := Seconds (5);
       Next  : Time      := Clock + D;
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (To_Duration (D))
                 & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Real_Time;

The main difference is that :ada:`D` is now a variable of type :ada:`Time_Span`,
defined in the :ada:`Ada.Real_Time` package. We call the function
:ada:`Seconds` to initialize :ada:`D`, but could have gotten a finer granularity
by calling :ada:`Nanoseconds` instead. Also, we need to first convert :ada:`D` to
the :ada:`Duration` type using :ada:`To_Duration` before we can display it.

Benchmarking
~~~~~~~~~~~~

One interesting application using the :ada:`Ada.Real_Time` package is
benchmarking. We've used that package before in a previous section when
discussing tasking. Let's look at an example of benchmarking:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Benchmarking

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Benchmarking is

       procedure Computational_Intensive_App is
       begin
          delay 5.0;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("Elapsed time: "
                 & Duration'Image
                     (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking;

This example defines a dummy :ada:`Computational_Intensive_App` implemented
using a simple :ada:`delay` statement. We initialize :ada:`Start_Time` and
:ada:`Stop_Time` from the then-current clock and calculate the elapsed
time. By running this program, we see that the time is roughly 5 seconds,
which is expected due to the :ada:`delay` statement.

A similar application is benchmarking of CPU time.  We can implement this
using the :ada:`Execution_Time` package. Let's modify the previous example
to measure CPU time:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Benchmarking_CPU_Time

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    procedure Display_Benchmarking_CPU_Time is

       procedure Computational_Intensive_App is
       begin
          delay 5.0;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : CPU_Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("CPU time: "
                 & Duration'Image
                     (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking_CPU_Time;

In this example, :ada:`Start_Time` and :ada:`Stop_Time` are of type :ada:`CPU_Time`
instead of :ada:`Time`. However, we still call the :ada:`Clock` function to
initialize both variables and calculate the elapsed time in the same way as
before. By running this program, we see that the CPU time is significantly
lower than the 5 seconds we've seen before. This is because the
:ada:`delay` statement doesn't require much CPU time.  The results will be
different if we change the implementation of
:ada:`Computational_Intensive_App` to use a mathematical functions in a long
loop. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Display_Benchmarking_Math

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    with Ada.Numerics.Generic_Elementary_Functions;

    procedure Display_Benchmarking_Math is

       procedure Computational_Intensive_App is
          package Funcs is new
            Ada.Numerics.Generic_Elementary_Functions
              (Float_Type => Long_Long_Float);
          use Funcs;

          X : Long_Long_Float;
       begin
          for I in 0 .. 1_000_000 loop
             X := Tan (Arctan
                    (Tan (Arctan
                      (Tan (Arctan
                        (Tan (Arctan
                          (Tan (Arctan
                            (Tan (Arctan
                              (0.577))))))))))));
          end loop;
       end Computational_Intensive_App;

       procedure Benchm_Elapsed_Time is
          Start_Time, Stop_Time : Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("Elapsed time: "
                    & Duration'Image
                        (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_Elapsed_Time;

       procedure Benchm_CPU_Time is
          Start_Time, Stop_Time : CPU_Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("CPU time: "
                    & Duration'Image
                        (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_CPU_Time;
    begin
       Benchm_Elapsed_Time;
       Benchm_CPU_Time;
    end Display_Benchmarking_Math;

Now that our dummy :ada:`Computational_Intensive_App` involves mathematical
operations requiring significant CPU time, the measured elapsed and CPU
time are much closer to each other than before.
