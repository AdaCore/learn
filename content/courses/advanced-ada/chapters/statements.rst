Statements
==========

.. include:: ../../global.txt

Exit loop statement
-------------------

.. admonition:: Relevant topics

    - :ada:`exit [loop_name]` case mentioned in
      `Exit Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-7.html>`_

.. todo::

    Complete section!


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Block Statements
    ----------------

    .. admonition:: Relevant topics

        - `Block Statements <http://www.ada-auth.org/standards/2xrm/html/RM-5-6.html>`_
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
