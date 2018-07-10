Exceptions
==========

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Ada uses exceptions for error handling.  Unlike many other languages,
Ada speaks about *raising*, not *throwing*, an exception and
*handling*, not *catching*, an exception.

Exception declaration
---------------------

Ada exceptions are not types, but instead objects, which may be
peculiar to you if you're used to the way Java or Python support
exceptions. Here's how you declare an exception:

.. code:: ada
    :class: ada-syntax-only

    package Exceptions is
        My_Except : exception;
        --  Like an object. *NOT* a type !
    end Exceptions;

Even though they're objects, you're going to use each declared
exception object as a "kind" or "family" of exceptions.  Ada does not
require that a subprogram declare every exception it can potentially
raise.

Raising an exception
--------------------

To raise an exception of our newly declared exception kind, do the following:

.. code:: ada

    with Exceptions; use Exceptions;

    procedure Main is
    begin
        raise My_Except;
        --  Execution of current control flow abandoned; an exception of kind
        --  "My_Except" will bubble up until it is caught.

        raise My_Except with "My exception message";
        --  Execution of current control flow abandoned; an exception of
        --  kind "My_Except" with associated string will bubble up until
	--  it is caught.
    end Main;

Handling an exception
---------------------

Next, we address how to handle exceptions that were raised by us or
libraries that we call. The neat thing in Ada is that you can add an
exception handler to any statement block as follows:

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

You don't need to introduce a block just to handle an exception: you
can add it to the statements block of your current subprogram:

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
    Exception handlers have an important restriction that
    you need to be careful about: Exceptions raised in the declarative
    section are not caught by the handlers of that block. So for
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

    This is also the case for the top-level exception block that is
    part of the current subprogram.


Predefined exceptions
---------------------

Ada has a very small number of predefined exceptions:

- `Constraint_Error` is the main one you might see. It's raised:
    - When bounds don't match or, in general, any violation of constraints.
    - In case of overflow
    - In case of null dereferences
    - In case of division by 0

- `Program_Error` might appear, but probably less often. It's raised
  in more arcane situations, such as for order of elaboration issues
  and some cases of detectable erroneous execution.

- `Storage_Error` will happen because of memory issues, such as:
     - Not enough memory (allocator)
     - Not enough stack

- `Tasking_Error` will happen with task related errors, such as any error
  happening during task activation.

You should not reuse predefined exceptions. If you do then, it won't
be obvious when one is raised that it is because something went wrong
in a built-in language operation.
