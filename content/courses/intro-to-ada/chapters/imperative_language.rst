Imperative language
===================

.. include:: ../../global.txt

Ada is a multi-paradigm language with support for object orientation
and some elements of functional programming, but its core is a simple, coherent
procedural/imperative language akin to C or Pascal.

.. admonition:: In other languages

    One important distinction between Ada and a language like C is that
    statements and expressions are very clearly distinguished.  In Ada, if you
    try to use an expression where a statement is required then your program
    will fail to compile.  This rule supports a useful stylistic principle:
    expressions are intended to deliver values, not to have side effects. It
    can also prevent some programming errors, such as mistakenly using the
    equality operator :ada:`=` instead of the assignment operation :ada:`:=` in
    an assignment statement.

Hello world
-----------

Here's a very simple imperative Ada program:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet

    with Ada.Text_IO;

    procedure Greet is
    begin
       --  Print "Hello, World!" to the screen
       Ada.Text_IO.Put_Line ("Hello, World!");
    end Greet;

which we'll assume is in the source file :file:`greet.adb`.

.. only:: builder_html

    If you compile that source with the GNAT compiler and run the executable,
    you will get an unsurprising result.

    .. code-block:: sh

        $ gprbuild greet.adb
        using project file [...]_default.gpr
        Compile
           [Ada]          greet.adb
        Bind
           [gprbind]      greet.bexch
           [Ada]          greet.ali
        Link
           [link]         greet.adb

         $ ./greet
        Hello, World!
         $

There are several noteworthy things in the above program:

-  A subprogram in Ada can be either a procedure or a function. A
   procedure, as illustrated above, does not return a value when called.

-  :ada:`with` is used to reference external modules that are needed in
   the procedure. This is similar to ``import`` in various languages or
   roughly similar to :c:`#include` in C and C++.
   We'll see later how they work in detail. Here, we are requesting a
   standard library module, the :ada:`Ada.Text_IO` package,
   which contains a procedure to print text on the screen: :ada:`Put_Line`.

-  :ada:`Greet` is a procedure, and the main entry point for our first
   program. Unlike in C or C++, it can be named anything you prefer. The
   builder will determine the entry point. In our simple example,
   :program:`gprbuild`, GNAT's builder, will use the file you passed as
   parameter.

-  :ada:`Put_Line` is a procedure, just like :ada:`Greet`, except it is
   declared in the :ada:`Ada.Text_IO` module. It is the Ada equivalent
   of C's :c:`printf`.

-  Comments start with :ada:`--` and go to the end of the line. There is
   no multi-line comment syntax, that is, it is not possible to start a
   comment in one line and continue it in the next line. The only way to
   create multiple lines of comments in Ada is by using :ada:`--` on each
   line. For example:

.. code-block:: ada

    --  We start a comment in this line...
    --  and we continue on the second line...

.. admonition:: In other languages

    Procedures are similar to functions in C or C++ that return :c:`void`.
    We'll see later how to declare functions in Ada.

Here is a minor variant of the "Hello, World" example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       --  Print "Hello, World!" to the screen
       Put_Line ("Hello, World!");
    end Greet;

This version utilizes an Ada feature known as a :ada:`use` clause, which has
the form :ada:`use` *package-name*. As illustrated by the call on
:ada:`Put_Line`, the effect is that entities from the named package can be
referenced directly, without the *package-name.* prefix.


.. _Intro_Ada_If_Statement:

Imperative language - If/Then/Else
----------------------------------

This section describes Ada's :ada:`if` statement and introduces several other
fundamental language facilities including integer I/O, data declarations,
and subprogram parameter modes.

Ada's :ada:`if` statement is pretty unsurprising in form and function:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Put a String
       Put ("Enter an integer value: ");

       --  Read in an integer value
       Get (N);

       if N > 0 then
          --  Put an Integer
          Put (N);
          Put_Line (" is a positive number");
       end if;
    end Check_Positive;

The :ada:`if` statement minimally consists of the reserved word :ada:`if`, a
condition (which must be a Boolean value), the reserved word :ada:`then` and a
non-empty sequence of statements (the :ada:`then` part) which is executed if the
condition evaluates to True, and a terminating :ada:`end if`.

This example declares an integer variable N, prompts the user for an integer,
checks if the value is positive and, if so, displays the integer's value
followed by the string " is a positive number". If the value is not positive,
the procedure does not display any output.

The type Integer is a predefined signed type, and its range depends on the
computer architecture. On typical current processors Integer is 32-bit signed.

The example illustrates some of the basic functionality for integer input-output.
The relevant subprograms are in the predefined package
:ada:`Ada.Integer_Text_IO` and include the :ada:`Get` procedure (which reads in
a number from the keyboard) and the :ada:`Put` procedure (which displays an
integer value).

Here's a slight variation on the example, which illustrates an :ada:`if` statement
with an :ada:`else` part:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive_2

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Put a String
       Put ("Enter an integer value: ");

       --  Reads in an integer value
       Get (N);

       --  Put an Integer
       Put (N);

       if N > 0 then
          Put_Line (" is a positive number");
       else
          Put_Line (" is not a positive number");
       end if;
    end Check_Positive;

In this example, if the input value is not positive then the program
displays the value followed by the String " is not a positive number".

Our final variation illustrates an :ada:`if` statement with :ada:`elsif`
sections:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       if N = 0 or N = 360 then
          Put_Line (" is due north");
       elsif N in 1 .. 89 then
          Put_Line (" is in the northeast quadrant");
       elsif N = 90 then
          Put_Line (" is due east");
       elsif N in 91 .. 179 then
          Put_Line (" is in the southeast quadrant");
       elsif N = 180 then
          Put_Line (" is due south");
       elsif N in 181 .. 269 then
          Put_Line (" is in the southwest quadrant");
       elsif N = 270 then
          Put_Line (" is due west");
       elsif N in 271 .. 359 then
          Put_Line (" is in the northwest quadrant");
       else
          Put_Line (" is not in the range 0..360");
       end if;
    end Check_Direction;

This example expects the user to input an integer between 0 and 360
inclusive, and displays which quadrant or axis the value corresponds
to.  The :ada:`in` operator in Ada tests whether a scalar value is
within a specified range and returns a Boolean result.
The effect of the program should be self-explanatory; later we'll see an
alternative and more efficient style to accomplish the same effect,
through a :ada:`case` statement.

Ada's :ada:`elsif` keyword differs from C or
C++, where nested :ada:`else .. if` blocks would be used instead.
And another difference is the presence of the :ada:`end if` in Ada,
which avoids the problem known as the "dangling else".


.. _Intro_Ada_Loop_Statement:

Imperative language - Loops
---------------------------

Ada has three ways of specifying loops. They differ from the
C / Java / Javascript for-loop, however, with simpler syntax and semantics
in line with Ada's philosophy.

For loops
~~~~~~~~~

The first kind of loop is the :ada:`for` loop, which allows iteration through a
discrete range.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a is
    begin
       for I in 1 .. 5 loop
          --  Put_Line is a procedure call
          Put_Line ("Hello, World!"
                      & Integer'Image (I));
          --        ^ Procedure parameter
       end loop;
    end Greet_5a;

.. only:: builder_html

    Executing this procedure yields the following output:

    .. code-block:: sh

       Hello, World! 1
       Hello, World! 2
       Hello, World! 3
       Hello, World! 4
       Hello, World! 5

A few things to note:

-  :ada:`1 .. 5` is a discrete range, from :ada:`1` to :ada:`5` inclusive.

-  The loop parameter :ada:`I` (the name is arbitrary) in the body of the
   loop has a value within this range.

-  :ada:`I` is local to the loop, so you cannot refer to :ada:`I`
   outside the loop.

-  Although the value of :ada:`I` is incremented at each iteration, from the
   program's perspective it is constant. An attempt to modify its value
   is illegal; the compiler would reject the program.

.. _Intro_Ada_Image_Attribute:

-  :ada:`Integer'Image` is a function that takes an Integer and converts it to
   a :ada:`String`.  It is an example of a language construct known as an
   *attribute*, indicated by the :ada:`'` syntax, which will be covered in more
   detail later.

-  The :ada:`&` symbol is the concatenation operator for String values

-  The :ada:`end loop` marks the end of the loop

The "step" of the loop is limited to 1 (forward direction) and -1 (backward).
To iterate backwards over a range, use the :ada:`reverse` keyword:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a_Reverse

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a_Reverse is
    begin
       for I in reverse 1 .. 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_5a_Reverse;

.. only:: builder_html

    Executing this procedure yields the following output:

    .. code-block:: sh

       Hello, World! 5
       Hello, World! 4
       Hello, World! 3
       Hello, World! 2
       Hello, World! 1

The bounds of a :ada:`for` loop may be computed at run-time; they
are evaluated once, before the loop body is executed.  If the value of the
upper bound is less than the value of the lower bound, then the
loop is not executed at all.  This is the case also for :ada:`reverse` loops.
Thus no output is produced in the following example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_No_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_No_Op is
    begin
       for I in reverse 5 .. 1 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_No_Op;

The :ada:`for` loop is more general than what we illustrated here;
more on that later.

.. _Intro_Ada_Bare_Loops:

Bare loops
~~~~~~~~~~

The simplest loop in Ada is the bare loop, which forms the foundation of
the other kinds of Ada loops.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5b

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5b is
       --  Variable declaration:
       I : Integer := 1;
       --  ^ Type
       --             ^ Initial value
    begin
       loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          --  Exit statement:
          exit when I = 5;
          --        ^ Boolean condition

          --  Assignment:
          I := I + 1;
          --  There is no I++ short form to
          --  increment a variable
       end loop;
    end Greet_5b;

This example has the same effect as :ada:`Greet_5a` shown earlier.

It illustrates several concepts:

-  We have declared a variable named :ada:`I` between the :ada:`is` and the
   :ada:`begin`. This constitutes a *declarative region*.  Ada clearly
   separates the declarative region from the statement part of a
   subprogram. A declaration can appear in a declarative region but is
   not allowed as a statement.

-  The bare loop statement is introduced by the keyword :ada:`loop` on
   its own and, like every kind of loop statement, is terminated by the
   combination of keywords :ada:`end loop`. On its own, it is an infinite
   loop. You can break out of it with an :ada:`exit` statement.

-  The syntax for assignment is :ada:`:=`, and the one for equality is
   :ada:`=`. There is no way to confuse them, because as previously noted,
   in Ada, statements and expressions are distinct, and expressions are
   not valid statements.


While loops
~~~~~~~~~~~

The last kind of loop in Ada is the :ada:`while` loop.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5c

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5c is
       I : Integer := 1;
    begin
       --  Condition must be a Boolean value
       --  (no Integers).
       --  Operator "<=" returns a Boolean
       while I <= 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          I := I + 1;
       end loop;
    end Greet_5c;

The condition is evaluated before each iteration. If the result is false, then
the loop is terminated.

This program has the same effect as the previous examples.

.. admonition:: In other languages

    Note that Ada has different semantics than C-based languages with respect
    to the condition in a while loop.  In Ada the condition has to be a Boolean
    value or the compiler will reject the program; the condition is not an
    integer that is treated as either :ada:`True` or :ada:`False` depending on
    whether it is non-zero or zero.


.. _Intro_Ada_Case_Statement:

Imperative language - Case statement
------------------------------------

Ada's :ada:`case` statement is similar to the C and C++ :c:`switch` statement,
but with some important differences.

Here's an example, a variation of a program that was shown earlier
with an :ada:`if` statement:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction_2

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       loop
          Put ("Enter an integer value: ");
          Get (N);
          Put (N);

          case N is
             when 0 | 360 =>
                Put_Line
                  (" is due north");
             when 1 .. 89 =>
                Put_Line
                  (" is in the northeast quadrant");
             when 90 =>
                Put_Line
                  (" is due east");
             when 91 .. 179 =>
                Put_Line
                  (" is in the southeast quadrant");
             when 180 =>
                Put_Line
                  (" is due south");
             when 181 .. 269 =>
                Put_Line
                  (" is in the southwest quadrant");
             when 270 =>
                Put_Line
                  (" is due west");
             when 271 .. 359 =>
                Put_Line
                  (" is in the northwest quadrant");
             when others =>
                Put_Line
                  (" Au revoir");
                exit;
          end case;
       end loop;
    end Check_Direction;

This program repeatedly prompts for an integer value and then, if the value is
in the range :ada:`0 .. 360`, displays the associated quadrant or axis.  If the
value is an Integer outside this range, the loop (and the program) terminate
after outputting a farewell message.

The effect of the case statement is similar to the if statement in an earlier
example, but the case statement can be more efficient because it does not involve
multiple range tests.

Notable points about Ada's case statement:

-  The case expression (here the variable :ada:`N`) must be of a discrete type,
   i.e. either an integer type or an enumeration type.  Discrete types will
   be covered in more detail later
   :ref:`discrete types <Intro_Ada_What_Is_A_Type>`.

-  Every possible value for the case expression needs to be covered by a unique
   branch of the case statement. This will be checked at compile time.

-  A branch can specify a single value, such as :ada:`0`; a range of values,
   such as :ada:`1 .. 89`; or any combination of the two (separated by a `|`).

-  As a special case, an optional final branch can specify :ada:`others`,
   which covers all values not included in the earlier branches.

-  Execution consists of the evaluation of the case expression and then
   a transfer of control to the statement sequence in the unique branch
   that covers that value.

-  When execution of the statements in the selected branch has completed,
   control resumes after the :ada:`end case`.  Unlike C, execution does
   not fall through to the next branch. So Ada doesn't need (and doesn't
   have) a :c:`break` statement.


Imperative language - Declarative regions
------------------------------------------

As mentioned earlier, Ada draws a clear syntactic separation between
declarations, which introduce names for entities that will be used
in the program, and statements, which perform the processing.
The areas in the program where declarations may appear are known
as declarative regions.

In any subprogram, the section between the :ada:`is` and the :ada:`begin` is a
declarative region. You can have variables, constants, types, inner subprograms,
and other entities there.

We've briefly mentioned variable declarations in previous subsection. Let's look
at a simple example, where we declare an integer variable :ada:`X` in the
declarative region and perform an initialization and an addition on it:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Variable_Declaration

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       X : Integer;
    begin
       X := 0;
       Put_Line ("The initial value of X is "
                 & Integer'Image (X));

       Put_Line ("Performing operation on X...");
       X := X + 1;

       Put_Line ("The value of X now is "
                 & Integer'Image (X));
    end Main;

Let's look at an example of a nested procedure:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Nested_Procedure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Nested is
       begin
          Put_Line ("Hello World");
       end Nested;
    begin
       Nested;
       --  Call to Nested
    end Main;

.. _Intro_Ada_Block_Statement:

A declaration cannot appear as a statement. If you need to declare a local
variable amidst the statements, you can introduce a new declarative region with
a block statement:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_6

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       loop
          Put_Line ("Please enter your name: ");

          declare
             Name : String := Get_Line;
             --               ^ Call to the
             --                 Get_Line function
          begin
             exit when Name = "";
             Put_Line ("Hi " & Name & "!");
          end;

          --  Name is undefined here
       end loop;

      Put_Line ("Bye!");
    end Greet;

.. attention::

    The :ada:`Get_Line` function allows you to receive input from the user, and
    get the result as a string. It is more or less equivalent to the :c:`scanf`
    C function.

    It returns a :ada:`String`, which, as we will see later, is an
    :ref:`Unconstrained array type <Intro_Ada_Unconstrained_Array_Types>`. For now we
    simply note that, if you wish to declare a :ada:`String` variable and do
    not know its size in advance, then you need to initialize the variable
    during its declaration.

Imperative language - conditional expressions
---------------------------------------------

Ada 2012 introduced an expression analog for conditional statements
(:ada:`if` and :ada:`case`).

If expressions
~~~~~~~~~~~~~~~

Here's an alternative version of an example we saw earlier; the :ada:`if`
statement has been replaced by an :ada:`if` expression:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       declare
          S : constant String :=
            (if N > 0
               then " is a positive number"
               else " is not a positive number");
       begin
          Put_Line (S);
       end;
    end Check_Positive;

The :ada:`if` expression evaluates to one of the two Strings depending
on N, and assigns that value to the local variable S.

Ada's :ada:`if` expressions are similar to :ada:`if` statements. However,
there are a few differences that stem from the fact that it is an expression:

-  All branches' expressions must be of the same type

-  It *must* be surrounded by parentheses if the surrounding
   expression does not already contain them

-  An :ada:`else` branch is mandatory unless the expression following
   :ada:`then` has a Boolean value.  In that case an :ada:`else` branch
   is optional and, if not present, defaults to :ada:`else True`.

Here's another example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Even_Odd

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line (if I mod 2 = 0
                      then "Even"
                      else "Odd");
       end loop;
    end Main;

This program produces 10 lines of output, alternating between "Odd" and "Even".


Case expressions
~~~~~~~~~~~~~~~~~

Analogous to :ada:`if` expressions, Ada also has :ada:`case` expressions.
They work just as you would expect.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Case_Expression

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line
            (case I is
             when 1 | 3 | 5 | 7 | 9  => "Odd",
             when 2 | 4 | 6 | 8 | 10 => "Even");
       end loop;
    end Main;

This program has the same effect as the preceding example.

The syntax differs from :ada:`case` statements, with branches separated
by commas.
