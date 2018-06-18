Exceptions
==========
:code-config:`reset_accumulator=True`

Ada uses exceptions for error handling. Ada's exceptions are not checked, which
means that a subprogram does not have to declare every exception kind that it
can potentially raise.

Exception declaration
---------------------

Ada exceptions are not types, they're objects, which is something peculiar
about them if you're used to the way Java or Python does it. Here is how you
declare an exception:

.. code:: ada

    package Exceptions is
        My_Except : exception;
        --  Like an object. *NOT* a type !
    end Exceptions;

Even though they're objects, you're going to use each declared exception object
as a "kind" or "family" of exceptions.

Raising an exception
--------------------

To raise an exception of our newly declared exception kind, here is how you do
it:

.. code:: ada

    with Exceptions; use Exceptions;

    procedure Main is
    begin
        raise My_Except;
        --  Execution of current control flow abandoned, an exception of kind
        --  "My_Except" will bubble up until it is caught.

        raise My_Except with "My exception message";
        --  Execution of current control flow abandoned, an exception of kind
        --  "My_Except" with associated string will bubble up until it is caught.
    end Main;

Handling an exception
---------------------

Now we need to tackle how to actually handle exceptions that were raised by us,
or other libraries. The neat thing in Ada is that you can add an exception
handler to any statement block, the following way:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       --  Block (sequence of statements)
       begin
          Open (File, In_File, "input.txt");
       exception
          when E : Name_Error =>
          --       ^ Exception to be handled
             Put ("Cannot open input file : ");
             Put_Line (Exception_Message (E));
             raise;
             --  Reraise current occurence
       end;
    end Open_File;

But you don't need to introduce a block just to handle an exception, you can
add it even to the statements block of your current subprogram:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       Open (File, In_File, "input.txt");
    --  Exception block can be added to any block
    exception
       when Name_Error =>
          Put ("Cannot open input file");
    end Open_File;

.. attention::
    Exception handlers have an important drawback that users need to be careful
    about: Exceptions raised in the declarative section are not caught. So for
    example, in the following code, the exception will not be caught.

    .. code:: ada

        with Ada.Text_IO; use Ada.Text_IO;
        with Ada.Exceptions;  use Ada.Exceptions;

        procedure Be_Careful is
           function Dangerous return Integer is
           begin
              raise Constraint_Error;
              return 42;
           end Dangerous;

        begin
           declare
              A : Integer := Dangerous;
           begin
              Put_Line (Integer'Image (A));
           exception
              when Constraint_Error => Put_Line ("error!");
           end;
        end Be_Careful;

    This is also true for the top-level exception block that is part of the
    current subprogram.


Predefined exceptions
---------------------

Ada has a very small number of predefined exceptions:

- `Constraint_Error` is the main one you might see. It is raised:
    - When bounds or subtype doesn’t match/in general any violation of constraints.
    - In case of overflow (-gnato for GNAT)
    - In case of null dereferences
    - In case of division by 0

- `Program_Error` might appear but probably less often. It is used for more
  arcane stuff, such as elaboration issues, or erroneous execution.

- `Storage_Error` will happen because of memory issues, such as:
     - Not enough memory (allocator)
     - Not enough stack

- `Tasking_Error` will happen with task related errors, such as any error
  happening during task activation.

You should generally not reuse predefined exceptions, because it is then
obvious when they happen that it is because something went wrong in a built-in
language operation.
