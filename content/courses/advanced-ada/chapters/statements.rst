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

    - :arm:`5.1 Simple and Compound Statements - Sequences of Statements <5-1>`

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

    - :arm:`5.1 Simple and Compound Statements - Sequences of Statements <5-1>`

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
       function Is_Further_Processing_Needed
         (Dummy : Integer)
          return Boolean
       is
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
       function Is_Further_Processing_Needed
         (Dummy : Integer)
          return Boolean
       is
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

       Arr   : constant array (1 .. 5) of Integer :=
                 (1, 4, 6, 42, 49);
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

.. admonition:: For further reading...

    In addition to labels, loops and block statements allow us to use a
    statement identifier. In simple terms, instead of writing
    :ada:`<<Some_Label>>`, we write :ada:`Some_Label :`.

    We could rewrite the previous code example using a loop statement
    identifier:

    .. code:: ada run_button project=Courses.Advanced_Ada.Statements.Loop_Statement_Identifier

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Statement_Identifier is
           Arr   : constant array (1 .. 5) of Integer :=
                     (1, 4, 6, 42, 49);
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
:ref:`Introduction to Ada course <Intro_Ada_Bare_Loops>`.
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

             Put_Line ("Element: "
                       & Integer'Image (V (C)));
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

    - :arm:`5.7 Exit Statements <5-7>`


If, case and loop statements
----------------------------

In the Introduction to Ada course, we talked about
:ref:`if statements <Intro_Ada_If_Statement>`,
:ref:`loop statements <Intro_Ada_Loop_Statement>`,
and :ref:`case statements <Intro_Ada_Case_Statement>`. This is a very simple
code example with these statements:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.If_Case_Loop_Statements

    procedure Show_If_Case_Loop_Statements is
       pragma Warnings (Off);

       Reset     : Boolean := False;
       Increment : Boolean := True;
       Val       : Integer := 0;
    begin
       --
       --  If statement
       --
       if Reset then
          Val := 0;
       elsif Increment then
          Val := Val + 1;
       else
          Val := Val - 1;
       end if;

       --
       --  Loop statement
       --
       for I in 1 .. 5 loop
          Val := Val * 2 - I;
       end loop;

       --
       --  Case statement
       --
       case Val is
          when 0 .. 5 =>
             null;
          when others =>
             Val := 5;
       end case;

    end Show_If_Case_Loop_Statements;

In this section, we'll look into a more advanced detail about the case
statement.

.. admonition:: In the Ada Reference Manual

    - :arm:`5.3 If Statements <5-3>`
    - :arm:`5.4 Case Statements <5-4>`
    - :arm:`5.5 Loop Statements <5-5>`

Case statements and expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we know, the case statement has a choice expression
(:ada:`case Choice_Expression is`), which is expected to be a discrete type.
Also, this expression can be a function call or a type conversion, for example
|mdash| in additional to being a variable or a constant.

As we discussed earlier on, if we use parentheses, the contents between those
parentheses is parsed as an expression. In the context of case statements, the
expression is first evaluated before being used as a choice expression. Consider
the following code example:

.. todo::

    Add link to Adv_Ada_Parenthesized_Expressions when it's available

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Case_Statement_Expression
    :class: ada-expect-compile-error

    package Scales is

       type Satisfaction_Scale is (Very_Dissatisfied,
                                   Dissatisfied,
                                   OK,
                                   Satisfied,
                                   Very_Satisfied);

       type Scale is range 0 .. 10;

       function To_Satisfaction_Scale
         (S : Scale)
          return Satisfaction_Scale;

    end Scales;

    package body Scales is

       function To_Satisfaction_Scale
         (S : Scale)
          return Satisfaction_Scale
       is
          Satisfaction : Satisfaction_Scale;
       begin
          case (S) is
             when 0 .. 2  =>
                Satisfaction := Very_Dissatisfied;
             when 3 .. 4  =>
                Satisfaction := Dissatisfied;
             when 5 .. 6  =>
                Satisfaction := OK;
             when 7 .. 8  =>
                Satisfaction := Satisfied;
             when 9 .. 10 =>
                Satisfaction := Very_Satisfied;
          end case;

          return Satisfaction;
       end To_Satisfaction_Scale;

    end Scales;

    with Ada.Text_IO; use Ada.Text_IO;

    with Scales;      use Scales;

    procedure Show_Case_Statement_Expression is
       Score : constant Scale := 0;
    begin
       Put_Line ("Score: "
                 & Scale'Image (Score)
                 & Satisfaction_Scale'Image (
                     To_Satisfaction_Scale (Score)));

    end Show_Case_Statement_Expression;

When we try to compile this code example, the compiler complains about missing
values in the :ada:`To_Satisfaction_Scale` function. As we mentioned in the
:ref:`Introduction to Ada course <Intro_Ada_Case_Statement>`, every possible
value for the choice expression needs to be covered by a unique branch of the
case statement. In principle, it *seems* that we're actually covering all
possible values of the :ada:`Scale` type, which ranges from 0 to 10. However,
we've written :ada:`case (S) is` instead of :ada:`case S is`. Because of the
parentheses, :ada:`(S)` is evaluated as an expression. In this case, the
expected range of the case statement is not :ada:`Scale'Range`, but the range of
its :ref:`base type <Adv_Ada_Base_Attribute>` :ada:`Scale'Base'Range`.

.. admonition:: In other languages

    In C, the switch-case statement requires parentheses for the choice
    expression:

    .. code:: c manual_chop run_button project=Courses.Advanced_Ada.Statements.Case_Statement_C

        !main.c

        #include <stdio.h>

        int main(int argc, const char * argv[])
        {
           int s = 0;

           switch (s)
           {
              case 0:
              case 1:
                 printf("Value in the 0 -- 1 range\n");
              default:
                 printf("Value > 1\n");
           }
        }

    In Ada, parentheses aren't expected in the choice expression. Therefore,
    we shouldn't write :ada:`case (S) is` in a C-like fashion |mdash|
    unless, of course, we really want to evaluate an expression in the case
    statement.


Block Statements
----------------

We've introduced block statements back in the
:ref:`Introduction to Ada course <Intro_Ada_Block_Statement>`.
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

    - :arm:`5.6 Block Statements <5-6>`

..
    REMOVED! TO BE RE-EVALUATED IN 2025:

    Parallel Block Statements
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    .. admonition:: Relevant topics

        - :arm22:`Parallel Block Statements <5-6-1>`

.. _Adv_Ada_Extended_Return_Statements:

Extended return statement
-------------------------

A common idiom in Ada is to build up a function result in a local
object, and then return that object:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Simple_Return

    procedure Show_Return is

       type Array_Of_Natural is
         array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural)
                     return Natural
       is
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

Since Ada 2005, a notation called the extended return statement is available:
this allows you to declare the result object and return it as part of one
statement. It looks like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Statements.Extended_Return

    procedure Show_Extended_Return is

       type Array_Of_Natural is
         array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural)
                     return Natural
       is
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

.. admonition:: In the Ada Reference Manual

    - :arm:`6.5 Return Statements <6-5>`


Other usages of extended return statements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #10: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-10>`_.

While the :ada:`extended_return_statement` was added to the language
specifically to support
:ref:`limited constructor functions <Adv_Ada_Extended_Return_Statements_Limited>`,
it comes in handy whenever you want a local name for the function result:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return_Other_Usages

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_String_Construct is

       function Make_String
         (S          : String;
          Prefix     : String;
          Use_Prefix : Boolean) return String
       is
          Length : Natural := S'Length;
       begin
          if Use_Prefix then
             Length := Length + Prefix'Length;
          end if;

          return Result : String (1 .. Length) do

             --  fill in the characters
             if Use_Prefix then
                Result
                  (1 .. Prefix'Length) := Prefix;

                Result
                  (Prefix'Length + 1 .. Length) := S;
             else
                Result := S;
             end if;

          end return;
       end Make_String;

       S1 : String := "Ada";
       S2 : String := "Make_With_";
    begin
       Put_Line ("No prefix:   "
                 & Make_String (S1, S2, False));
       Put_Line ("With prefix: "
                 & Make_String (S1, S2, True));
    end Show_String_Construct;

In this example, we first calculate the length of the string and store it in
:ada:`Length`. We then use this information to initialize the return object of
the :ada:`Make_String` function.


..
    REMOVED! TO BE RE-EVALUATED IN 2025:

    Parallel loops
    --------------

    .. admonition:: Relevant topics

        - Parallel loops mentioned in
        :arm22:`Loop Statements <5-5>`
