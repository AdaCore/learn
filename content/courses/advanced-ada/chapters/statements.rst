Statements
==========

.. include:: ../../global.txt

Exit loop statement
-------------------

We've introduced bare loops back in the
:doc:`Introduction to Ada course </courses/intro-to-ada/chapters/imperative_language>`.
In this section, we'll briefly discuss loop names and exit loop statements.

A bare loop has this form:

.. code-block:: ada

    loop
        exit when Some_Condition;
    end loop;

We can name a loop by using a loop statement identifier:

.. code-block:: ada

    Loop_Name:
       loop
          exit Loop_Name when Some_Condition;
       end loop Loop_Name;

In this case, we have to use the loop's name after :ada:`end loop`. Also,
having a name for a loop allows us to indicate which loop we're exiting from:
:ada:`exit Loop_Name when`.

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Exit_Named_Loop

    with Ada.Text_IO;            use Ada.Text_IO;
    with Ada.Containers.Vectors;

    procedure Show_Vector_Cursor_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Positive,
            Element_Type => Integer);

       use Integer_Vectors;

       V : constant Vector := 20 & 10 & 0 & 13;
       C : Cursor;
    begin
       C := V.First;
       Put_Line ("Vector elements are: ");

       Show_Elements:
          loop
             exit Show_Elements when C = No_Element;

             Put_Line ("Element: " & Integer'Image (V (C)));
             C := Next (C);
          end loop Show_Elements;

    end Show_Vector_Cursor_Iteration;

Naming a loop is particularly useful when we have nested loops and we want to
exit directly from the inner loop:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Inner_Loop_Exit

    procedure Show_Inner_Loop_Exit is
       pragma Warnings (Off);

       Cond : Boolean := True;
    begin

       Outer_Processing: loop

          Inner_Processing: loop
             exit Outer_Processing when Cond = True;
          end loop Inner_Processing;

       end loop Outer_Processing;

    end Show_Inner_Loop_Exit;

Here, we indicate that we exit from the :ada:`Outer_Processing` loop in case a
condition :ada:`Cond` is met, even if we're actually within the inner loop.

.. admonition:: In the Ada Reference Manual

    - `5.7 Exit Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-7.html>`_



..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Block Statements
    ----------------

    .. admonition:: Relevant topics

        - `Block Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-6.html>`_
        - `Parallel Block Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-6-1.html>`_


Extended return statement
-------------------------

.. admonition:: Relevant topics

    - extended :ada:`return ... do` statement mentioned in
      `Return Statements <http://www.ada-auth.org/standards/12rm/html/RM-6-5.html>`_
    - application for limited types

.. todo::

    Complete section!


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Parallel loops
    --------------

    .. admonition:: Relevant topics

        - Parallel loops mentioned in
        `Loop Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-5.html>`_
