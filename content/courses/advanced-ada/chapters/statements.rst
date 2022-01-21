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

       Show_Elements :
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

       Outer_Processing : loop

          Inner_Processing : loop
             exit Outer_Processing when Cond;
          end loop Inner_Processing;

       end loop Outer_Processing;

    end Show_Inner_Loop_Exit;

Here, we indicate that we exit from the :ada:`Outer_Processing` loop in case a
condition :ada:`Cond` is met, even if we're actually within the inner loop.

.. admonition:: In the Ada Reference Manual

    - `5.7 Exit Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-7.html>`_


Block Statements
----------------

We've introduced block statements back in the
:doc:`Introduction to Ada course </courses/intro-to-ada/chapters/imperative_language>`.
They have this simple form:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Simple_Block_Statement

    procedure Show_Block_Statement is
       pragma Warnings (Off);
    begin

       --  BLOCK STARTS HERE:
       declare
          I : Integer;
       begin
          I := 0;
       end;

    end Show_Block_Statement;

We can use an identifier when writing a block statement. (This is similar to
loop statement identifiers that we discussed in the previous section.) In this
example, we implement a block called :ada:`Simple_Block`:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Block_Statement_Identifier

    procedure Show_Block_Statement is
       pragma Warnings (Off);
    begin

       Simple_Block : declare
          I : Integer;
       begin
          I := 0;
       end Simple_Block;

    end Show_Block_Statement;

Note that we must write :ada:`end Simple_Block;` when we use the
:ada:`Simple_Block` identifier.

Block statement identifiers are useful:

- to indicate the begin and the end of a block |mdash| as some blocks might be
  long or nested in other blocks;

- to indicate the purpose of the block (i.e. as code documentation).

.. admonition:: In the Ada Reference Manual

    - `5.6 Block Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-6.html>`_

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Parallel Block Statements
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    .. admonition:: Relevant topics

        - `Parallel Block Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-6-1.html>`_

.. _Adv_Ada_Extended_Return_Statements:

Extended return statement
-------------------------

A common idiom in Ada is to build up a function result in a local
object, and then return that object:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Simple_Return

    procedure Show_Return is

       type Array_Of_Natural is array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural) return Natural is
          Result : Natural := 0;
       begin
          for Index in A'Range loop
             Result := Result + A (Index);
          end loop;
          return Result;
       end Sum;

    begin
       null;
    end Show_Return;

Since Ada 2005, a notation called the :ada:`extended_return_statement`,
which allows you to declare the result object and return it as part of one
statement, is available. It looks like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Extended_Return

    procedure Show_Extended_Return is

       type Array_Of_Natural is array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural) return Natural is
       begin
          return Result : Natural := 0 do
             for Index in A'Range loop
                Result := Result + A (Index);
             end loop;
          end return;
       end Sum;

    begin
       null;
    end Show_Extended_Return;

The return statement here creates :ada:`Result`, initializes it to
:ada:`0`, and executes the code between :ada:`do` and :ada:`end return`.
When :ada:`end return` is reached, :ada:`Result` is automatically returned
as the function result.

In a :ref:`later section <Adv_Ada_Extended_Return_Statements_Limited>`, we'll
see how extended return statements can be almost essential for limited types.

.. admonition:: In the Ada Reference Manual

    - `6.5 Return Statements <http://www.ada-auth.org/standards/12rm/html/RM-6-5.html>`_

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Parallel loops
    --------------

    .. admonition:: Relevant topics

        - Parallel loops mentioned in
        `Loop Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-5.html>`_
