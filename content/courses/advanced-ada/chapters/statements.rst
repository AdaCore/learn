Statements
==========

.. include:: ../../global.txt

Simple and Compound Statements
------------------------------

We can classify statements as either simple or compound. Simple statements
don't contain other statements; think of them as "atomic units" that cannot be
further divided. Compound statements, on the other hand, may contain other
|mdash| simple or compound |mdash| statements.

Here are some examples from each category:

+---------------------+--------------------------------------------------------+
| Category            | Examples                                               |
+=====================+========================================================+
| Simple statements   | Null statement, assignment, subprogram call, etc.      |
+---------------------+--------------------------------------------------------+
| Compound statements | If statement, case statement, loop statement,          |
|                     | block statement                                        |
+---------------------+--------------------------------------------------------+

.. admonition:: In the Ada Reference Manual

    - `5.1 Simple and Compound Statements - Sequences of Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-1.html>`_

Labels
------

We can use labels to identify statements in the code. They have the following
format: :ada:`<<Some_Label>>`. We write them right before the statement we want
to apply it to. Let's see an example of labels with simple statements:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Simple_Labels

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Statement_Identifier is
       pragma Warnings (Off, "is not referenced");
    begin
       <<Show_Hello>> Put_Line ("Hello World!");
       <<Show_Test>>  Put_Line ("This is a test.");

       <<Show_Separator>>
       <<Show_Block_Separator>>
       Put_Line ("====================");
    end Show_Statement_Identifier;

Here, we're labeling each statement. For example, we use the :ada:`Show_Hello`
label to identify the :ada:`Put_Line ("Hello World!");` statement. Note that we
can use multiple labels a single statement. In this code example, we use the
:ada:`Show_Separator` and :ada:`Show_Block_Separator` labels for the same
statement.

.. admonition:: In the Ada Reference Manual

    - `5.1 Simple and Compound Statements - Sequences of Statements <http://www.ada-auth.org/standards/12rm/html/RM-5-1.html>`_

Labels and :ada:`goto` statements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Labels are mainly used in combination with :ada:`goto` statements. (Although
pretty much uncommon, we could potentially use labels to indicate important
statements in the code.) Let's see an example where we use a :ada:`goto label;`
statement to *jump* to a specific label:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Label_Goto

    procedure Show_Cleanup is
       pragma Warnings (Off, "always false");

       Some_Error : Boolean;
    begin
       Some_Error := False;

       if Some_Error then
          goto Cleanup;
       end if;

       <<Cleanup>> null;
    end Show_Cleanup;

Here, we transfer the control to the *cleanup* statement as soon as an error is
detected.

Use-case: :ada:`Continue`
~~~~~~~~~~~~~~~~~~~~~~~~~

Another use-case is that of a :ada:`Continue` label in a loop. Consider a loop
where we want to skip further processing depending on a condition:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Label_Continue_1

    procedure Show_Continue is
       function Is_Further_Processing_Needed (Dummy : Integer)
         return Boolean is
       begin
          --  Dummy implementation
          return False;
       end Is_Further_Processing_Needed;

       A : constant array (1 .. 10) of Integer :=
         (others => 0);
    begin
       for E of A loop

          --  Some stuff here...

          if Is_Further_Processing_Needed (E) then

             --  Do more stuff...

             null;
          end if;
       end loop;
    end Show_Continue;

In this example, we call the :ada:`Is_Further_Processing_Needed (E)` function to
check whether further processing is needed or not. If it's needed, we continue
processing in the :ada:`if` statement. We could simplify this code by just using
a :ada:`Continue` label at the end of the loop and a :ada:`goto` statement:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Label_Continue_2

    procedure Show_Continue is
       function Is_Further_Processing_Needed (Dummy : Integer)
         return Boolean is
       begin
          --  Dummy implementation
          return False;
       end Is_Further_Processing_Needed;

       A : constant array (1 .. 10) of Integer :=
         (others => 0);
    begin
       for E of A loop

          --  Some stuff here...

          if not Is_Further_Processing_Needed (E) then
             goto Continue;
          end if;

          --  Do more stuff...

          <<Continue>>
       end loop;
    end Show_Continue;

Here, we use a :ada:`Continue` label at the end of the loop and jump to it in
the case that no further processing is needed. Note that, in this example, we
don't have a statement after the :ada:`Continue` label because the label itself
is at the end of a statement |mdash| to be more specific, at the end of the loop
statement. In such cases, there's an implicit :ada:`null` statement.

.. admonition:: Historically

    Since Ada 2012, we can simply write:

    .. code-block:: ada

        loop
           --  Some statements...

           <<Continue>>
        end loop;

    If a label is used at the end of a sequence of statements, a :ada:`null`
    statement is implied. In previous versions of Ada, however, that is not the
    case. Therefore, when using those versions of the language, we must write at
    least a :ada:`null` statement:

    .. code-block:: ada

        loop
           --  Some statements...

           <<Continue>> null;
        end loop;


Labels and compound statements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use labels with compound statements as well. For example, we can label
a :ada:`for` loop:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Loop_Label

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Statement_Identifier is
       pragma Warnings (Off, "is not referenced");

       Arr   : constant array (1 .. 5) of Integer := (1, 4, 6, 42, 49);
       Found : Boolean := False;
    begin
       <<Find_42>> for E of Arr loop
          if E = 42 then
             Found := True;
             exit;
          end if;
       end loop;

       Put_Line ("Found: " & Found'Image);
    end Show_Statement_Identifier;

.. admonition:: Relevant topics

    In addition to labels, loops and block statements allow us to use a
    statement identifier. In simple terms, instead of writing
    :ada:`<<Some_Label>>`, we write :ada:`Some_Label :`.

    We could rewrite the previous code example using a loop statement
    identifier:

    .. code:: ada run_button project=Courses.Advanced_Ada.Statements.Loop_Statement_Identifier

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Statement_Identifier is
           Arr   : constant array (1 .. 5) of Integer := (1, 4, 6, 42, 49);
           Found : Boolean := False;
        begin
           Find_42 : for E of Arr loop
              if E = 42 then
                 Found := True;
                 exit Find_42;
              end if;
           end loop Find_42;

           Put_Line ("Found: " & Found'Image);
        end Show_Statement_Identifier;

    Loop statement and block statement identifiers are generally preferred over
    labels. Later in this chapter, we discuss this topic in more detail.


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
