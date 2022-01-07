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
