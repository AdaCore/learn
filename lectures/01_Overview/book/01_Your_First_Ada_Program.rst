Lesson 1: Your First Ada Program
=====================================================================

.. role:: ada(code)
   :language: ada

Welcome to the AdaCore University!

We will work you through your very first Ada program. Let's get started!


Simple Ada application
---------------------------------------------------------------------

This is a very simple Ada application. It will query some numbers on the console, make some computation between them (in this case, an addition), and then display a different caption depending on the sign of the result of this addition.

.. code:: ada

   with Ada.Text_IO;

   procedure Hello is
      A, B, C : Integer;
   begin
      A := Integer'Value (Ada.Text_IO.Get_Line);
      B := Integer'Value (Ada.Text_IO.Get_Line);
      C := A + B;

      if C = 0 then
         Ada.Text_IO.Put_Line ("RESULT IS 0");
      elsif C > 0 then
         Ada.Text_IO.Put_Line ("POSITIVE RESULT :" & Integer'Image (C));
      else
         Ada.Text_IO.Put_Line ("NEGATIVE RESULT :" & Integer'Image (C));
      end if;
   end Hello;


Building the application
---------------------------------------------------------------------

You may use the GPS IDE to build the application. In order to do that, you click on ``Build`` menu, then ``Project``, and then select the executable that you want to build --- in this case, ``hello.adb``. This opens a window that provides multiple options for compilation. In this case, the default options are fine, so you can simply hit ``Execute``. This triggers various commands in the background, eventually creating the program ``hello.exe`` (or ``hello`` on Linux).


Running the application
---------------------------------------------------------------------

After building the application, you can run it on the console. If you're using GPS, you can click on the ``Build`` menu again, and, this time, select ``Run`` and then select the executable name (``hello`` in this case). You'll get a window with options. Again, the default options are fine, so you can simple click on ``Execute``. Now, you'll be able to interact with the application in a console within GPS. Alternatively, you may run the application on a standalone console.

If you enter two positive numbers, you expect a positive result and have the according caption in the console. For instance:

::

    1
    1
    POSITIVE RESULT : 2

As you can see, the positive result has been displayed as expected by the program. If you run the application a second time ---this time with two negative numbers--- we have the proper caption result displayed on the screen:

::

    -1
    -1
    NEGATIVE RESULT :-2

Finally, as you would expect, if you run the same program again with -1 and 1 ---expecting as a result the zero caption---, this is indeed what is being displayed by the program:

::

    -1
    1
    RESULT IS 0


Main subprogram name (can be any Ada identifier)
---------------------------------------------------------------------

Let's now dive into more details. The first thing that we are looking at here is the entry point of the program:

.. code:: ada

   procedure Hello is

It's a procedure --- which is an equivalent to void functions in languages such as C and C++. In Ada, the name of the entry point or, if you will, the name of the main subprogram, can be anything. It doesn't have to be called ``main`` necessarily, which is why, in this case, we decided to called it ``Hello``.

Note that repeating the name of the procedure at the end of the definition is optional:

.. code:: ada

   procedure Hello is
     -- Procedure definition
   end Hello;  -- could be replaced by "end;"


Variable declaration
---------------------------------------------------------------------

What comes next after the procedure declaration is a declaration scope, where we can declare variables:

.. code:: ada

   procedure Hello is
      A, B, C : Integer;
   begin

Here, we are declaring A, B, and C of type integer. We have to declare variables within the variable scope. It is not possible to declare them otherwise. The declaration scope is finished by the keyword :ada:`begin`, which introduces a sequence of statements, such as assignments, conditions, subprogram calls. These statements can only happen in a sequence of statement area. They cannot be written in the declarative part. The statement area finishes with the keyword :ada:`end`:

.. code:: ada

   procedure Hello is
     -- Declaration scope
   begin
     -- Statement area
   end Hello;


Assignment
---------------------------------------------------------------------

One of the notable differences between Ada and other languages, such as C or C++, is that the assignment is done through the :ada:`:=` symbol as opposed to simple :ada:`=`. On top of that, an assignment is an operator, which means that it cannot be written in a condition.

.. code:: ada

      A := Integer'Value (Ada.Text_IO.Get_Line);

The equality operator in Ada is a simple :ada:`=` as opposed to :ada:`==` (as in other languages):

.. code:: ada

      if C = 0 then


Attributes
---------------------------------------------------------------------

One of Ada particularities is the notion of attributes. An attribute is a property of an entity --- for example, a variable or a type. It is accessed through the tick notation :ada:`'` between the entity that you want to extract the property from and the property name. In this example, we are using two attributes, :ada:`Value` and :ada:`Image`:

.. code:: ada

      A := Integer'Value (Ada.Text_IO.Get_Line);

      Ada.Text_IO.Put_Line ("POSITIVE RESULT :" & Integer'Image (C));

:ada:`Value` is a special attribute that transforms a string into a value of the type. So :ada:`Integer'Value` would transform a string into an integer value. :ada:`Image` does the opposite, that is to say, it takes a value of the type and transforms that to a string.


Input/output to the console
---------------------------------------------------------------------

With this, we're going to do some input/output on the console. In order to be able to interact with the console, we will need to use a library unit. In Ada, the standard library unit that provides access to the input/output on the console is called :ada:`Ada.Text_IO`. In order to use it, we need to declare a dependency between my program and this library unit. This dependency is introduced by the standard keyword :ada:`with` at the beginning of the program, followed by the name of the library unit (in this case, :ada:`Ada.Text_IO`):

.. code:: ada

   with Ada.Text_IO;

Once this is done, we can use services and functionalities from this library. Here, we're interested in two things in particular: :ada:`Get_Line` and :ada:`Put_Line`. As you can see in the example, to have access to :ada:`Get_Line` (contained in :ada:`Ada.Text_IO`), we have to write :ada:`Ada.Text_IO.Get_Line`:

.. code:: ada

      A := Integer'Value (Ada.Text_IO.Get_Line);

What this will do is call the subprogram :ada:`Get_Line`, which reads a string on the command-line up until you hit the enter key and returns the string to the program.

The :ada:`Put_Line` service is going to do the things the other way around, that is to say, it takes a string as parameter and it displays the string on the console:

.. code:: ada

         Ada.Text_IO.Put_Line ("RESULT IS 0");

With the following line, what we want to do is to display a caption and the value, that is to say, a literal ``POSITIVE RESULT`` and the result of the call to the :ada:`Image` attribute converting the value of the C variable into a string:

.. code:: ada

         Ada.Text_IO.Put_Line ("POSITIVE RESULT :" & Integer'Image (C));

In order to concatenate two strings here, we're going to use the :ada:`&` character. In Ada, the :ada:`&` operator is the array concatenation operator and can be used between strings.


Conditions
---------------------------------------------------------------------

The last thing to look in our example is the conditional block:

.. code:: ada

      if C = 0 then
         Ada.Text_IO.Put_Line ("RESULT IS 0");

As you can see, we use the :ada:`then` keyword to delimitate the condition, so there is no need to use the parentheses around the condition as you would need in some other languages.

The other interesting thing is that there is an actual reserved word called :ada:`elsif` to introduce conditional alternative in case the first one is not verified.

.. code:: ada

      if C = 0 then
         Ada.Text_IO.Put_Line ("RESULT IS 0");
      elsif C > 0 then

Finally, there is no condition shortcuts to close the block in case there is only one instruction to execute, which means that, for an :ada:`if`, there is always an :ada:`end if`.

.. code:: ada

      if C = 0 then
         Ada.Text_IO.Put_Line ("RESULT IS 0");
         --  ...
      end if;

This is the complete block:

.. code:: ada

      if C = 0 then
         Ada.Text_IO.Put_Line ("RESULT IS 0");
      elsif C > 0 then
         Ada.Text_IO.Put_Line ("POSITIVE RESULT :" & Integer'Image (C));
      else
         Ada.Text_IO.Put_Line ("NEGATIVE RESULT :" & Integer'Image (C));
      end if;

So what this block does is:

   - checking if C is zero and, if that is the case, putting ``RESULT IS 0``.
   - if not, it will check if C is greater than zero and then put ``POSITIVE RESULT`` on the screen if that's the case
   - and in all other cases, it will display ``NEGATIVE RESULT``.
