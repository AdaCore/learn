Subprograms
===========

.. _Intro_Ada_Subprograms:

.. include:: ../../../global.txt

Subprograms
-----------

So far, we have used procedures, mostly to have a main body of code
to execute. Procedures are one kind of *subprogram*.

There are two kinds of subprograms in Ada, *functions* and *procedures*. The
distinction between the two is that a function returns a value, and a procedure
does not.

This example shows the declaration and definition of a function:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment

    function Increment (I : Integer) return Integer;
    --  We declare (but don't define) a function with
    --  one parameter, returning an integer value

    function Increment (I : Integer) return Integer is
       --  We define the Increment function
    begin
        return I + 1;
    end Increment;

Subprograms in Ada can, of course, have parameters. One syntactically important
note is that a subprogram which has no parameters does not have a parameter
section at all, for example:

.. code-block:: ada

   procedure Proc;

   function Func return Integer;

Here's another variation on the previous example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-syntax-only

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer;
    --                ^ Default value for parameters

In this example, we see that parameters can have default values. When calling the
subprogram, you can then omit parameters if they have a default value. Unlike
C/C++, a call to a subprogram without parameters does not include parentheses.

This is the implementation of the function above:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment_By

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer is
    begin
       return I + Incr;
    end Increment_By;

.. admonition:: In the GNAT toolchain

   The Ada standard doesn't mandate in which file the specification or the
   implementation of a subprogram must be stored. In other words, the standard
   doesn't require a specific file structure or specific file name extensions.
   For example, we could save both the specification and the implementation of
   the :ada:`Increment` function above in a file called :file:`increment.txt`.
   (We could even store the entire source code of a system in a single
   file.) From the standard's perspective, this would be completely acceptable.

   The GNAT toolchain, however, requires the following file naming scheme:

   - files with the `.ads` extension contain the specification, while

   - files with the `.adb` extension contain the implementation.

   Therefore, in the GNAT toolchain, the specification of the :ada:`Increment`
   function must be stored in the :file:`increment.ads` file, while its
   implementation must be stored in the :file:`increment.adb` file. This rule
   always applies to packages, which we discuss
   :doc:`later <./modular_programming>`. (Note, however, that it's possible to
   circumvent this rule.) For more details, you may refer to the
   :doc:`Introduction to GNAT Toolchain </courses/GNAT_Toolchain_Intro/index>`
   course or the
   `GPRbuild User’s Guide <https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html>`_.


Subprogram calls
~~~~~~~~~~~~~~~~

We can then call our subprogram this way:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;
    begin
       C := Increment_By;
       --              ^ Parameterless call,
       --                value of I is 0
       --                and Incr is 1

       Put_Line ("Using defaults for Increment_By is "
                 & Integer'Image (C));

       A := 10;
       B := 3;
       C := Increment_By (A, B);
       --                 ^ Regular parameter passing

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));

       A := 20;
       B := 5;
       C := Increment_By (I    => A,
                          Incr => B);
        --                ^ Named parameter passing

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));
    end Show_Increment;

Ada allows you to name the parameters when you pass them, whether they have a
default or not. There are some rules:

- Positional parameters come first.

- A positional parameter cannot follow a named parameter.

.. ?? I don't understand the following sentence.  If the param has a
.. ?? default value then you can omit the parameter, it has nothing
.. ?? to do with the use of positional versus named

As a convention, people usually name parameters at the call site if the
function's corresponding parameters has a default value. However, it is also
perfectly acceptable to name every parameter if it makes the code clearer.

Nested subprograms
~~~~~~~~~~~~~~~~~~

As briefly mentioned earlier, Ada allows you to declare one subprogram inside another.

This is useful for two reasons:

- It lets you organize your programs in a cleaner fashion. If you need a
  subprogram only as a "helper" for another subprogram, then the principle of
  localization indicates that the helper subprogram should be declared nested.

- It allows you to share state easily in a controlled fashion, because the
  nested subprograms have access to the parameters, as well as any local
  variables, declared in the outer scope.

For the previous example, we can move the duplicated code (call to
:ada:`Put_Line`) to a separate procedure. This is a shortened version with
the nested :ada:`Display_Result` procedure.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;

       procedure Display_Result is
       begin
          Put_Line ("Increment of "
                    & Integer'Image (A)
                    & " with "
                    & Integer'Image (B)
                    & " is "
                    & Integer'Image (C));
       end Display_Result;

    begin
       A := 10;
       B := 3;
       C := Increment_By (A, B);
       Display_Result;
       A := 20;
       B := 5;
       C := Increment_By (A, B);
       Display_Result;
    end Show_Increment;

Function calls
~~~~~~~~~~~~~~

An important feature of function calls in Ada is that the return value at a
call cannot be ignored; that is, a function call cannot be used as a statement.

If you want to call a function and do not need its result, you will still need
to explicitly store it in a local variable.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Quadruple
    :class: ada-expect-compile-error

    function Quadruple (I : Integer)
                        return Integer;

    function Quadruple (I : Integer)
                        return Integer is

        function Double (I : Integer)
                         return Integer is
        begin
           return I * 2;
        end Double;

       Res : Integer;
    begin
       Double (Double (I));
       --  ERROR: cannot use call to function
       --         "Double" as a statement

       Res := Double (Double (I));
       --  OK: return value of Double is
       --      assigned to Res

       return Res;
    end Quadruple;

A statement such as :ada:`Double (Double (I));` is wrong because we're not
assigning the return value to a variable |mdash| we can correct this statement
by writing :ada:`Res := Double (Double (I));`.

.. admonition:: In the GNAT toolchain

    In GNAT, with all warnings activated, it becomes even harder to ignore the
    result of a function, because unused variables will be flagged. For
    example, this code would not be valid:

    .. code-block:: ada

        function Read_Int
           (Stream :     Network_Stream;
            Result : out Integer) return Boolean;

        procedure Main is
            Stream : Network_Stream := Get_Stream;
            My_Int : Integer;

            -- Warning: in the line below, B is
            --          never read.
            B : Boolean := Read_Int (Stream, My_Int);
        begin
           null;
        end Main;

    You then have two solutions to silence this warning:

    - Either annotate the variable with :ada:`pragma Unreferenced` , e.g.:

    .. code-block:: ada

        B : Boolean := Read_Int (Stream, My_Int);
        pragma Unreferenced (B);

    - Or give the variable a name that contains any of the strings :ada:`discard`
      :ada:`dummy` :ada:`ignore` :ada:`junk` :ada:`unused` (case insensitive)

.. ?? This example might be confusing since out parameters have not been covered.
.. ?? It would be better to show an example where the function's side effect is on
.. ?? a non-local variable.  Maybe for the next version of the course.


.. _Intro_Ada_Parameter_Modes:

Parameter modes
---------------

.. amiard TODO: Talk about early returns from procedures, and grouping
   parameters.

So far we have seen that Ada is a safety-focused language. There are many ways
this is realized, but two important points are:

- Ada makes the user specify as much as possible about the behavior expected
  for the program, so that the compiler can warn or reject if there is an
  inconsistency.

- Ada provides a variety of techniques for achieving the generality and
  flexibility of pointers and dynamic memory management, but without the
  latter's drawbacks (such as memory leakage and dangling references).

Parameter modes are a feature that helps achieve the two design goals above. A
subprogram parameter can be specified with a mode, which is one of the
following:

+---------------+--------------------------------------------+
| :ada:`in`     | Parameter can only be read, not written    |
+---------------+--------------------------------------------+
| :ada:`out`    | Parameter can be written to, then read     |
+---------------+--------------------------------------------+
| :ada:`in out` | Parameter can be both read and written     |
+---------------+--------------------------------------------+

The default mode for parameters is :ada:`in`; so far, most of the examples
have been using :ada:`in` parameters.

.. admonition:: Historically

    Functions and procedures were originally more different in philosophy.
    Before Ada 2012, functions could only take :ada:`in` parameters.

Subprogram calls
----------------

In parameters
~~~~~~~~~~~~~

The first mode for parameters is the one we have been implicitly using so far.
Parameters passed using this mode cannot be modified, so that the following
program will cause an error:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Swap
    :class: ada-expect-compile-error

    procedure Swap (A, B : Integer) is
       Tmp : Integer;
    begin
       Tmp := A;

       --  Error: assignment to "in" mode
       --         parameter not allowed
       A := B;

       --  Error: assignment to "in" mode
       --         parameter not allowed
       B := Tmp;
    end Swap;

The fact that :ada:`in` is the default mode is very important. It
means that a parameter will not be modified unless you explicitly specify
a mode in which modification is allowed.

In out parameters
~~~~~~~~~~~~~~~~~

To correct our code above, we can use an :ada:`in out` parameter.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.In_Out_Params
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure In_Out_Params is
       procedure Swap (A, B : in out Integer) is
          Tmp : Integer;
       begin
          Tmp := A;
          A   := B;
          B   := Tmp;
       end Swap;

       A : Integer := 12;
       B : Integer := 44;
    begin
        Swap (A, B);

        --  Prints 44
        Put_Line (Integer'Image (A));
    end In_Out_Params;

An :ada:`in out` parameter will allow read and write access to the object
passed as parameter, so in the example above, we can see that :ada:`A` is
modified after the call to :ada:`Swap`.

.. attention::

    While :ada:`in out` parameters look a bit like references in C++, or regular
    parameters in Java that are passed by-reference, the Ada language standard
    does not mandate "by reference" passing for in out parameters except for
    certain categories of types as will be explained later.

    In general, it is better to think of modes as higher level than by-value
    versus by-reference semantics. For the compiler, it means that an array
    passed as an :ada:`in` parameter might be passed by reference, because it
    is more efficient (which does not change anything for the user since the
    parameter is not assignable). However, a parameter of a discrete type will
    always be passed by copy, regardless of its mode (which is more efficient
    on most architectures).

Out parameters
~~~~~~~~~~~~~~

The :ada:`out` mode applies when the subprogram needs to write to a parameter
that might be uninitialized at the point of call. Reading the value of an
:ada:`out` parameter is permitted, but it should only be done after the
subprogram has assigned a value to the parameter. Out parameters behave a bit
like return values for functions.  When the subprogram returns, the actual
parameter (a variable) will have the value of the out parameter at the point
of return.

.. admonition:: In other languages

    Ada doesn't have a tuple construct and does not allow returning multiple
    values from a subprogram (except by declaring a full-fledged record type).
    Hence, a way to return multiple values from a subprogram is to use :ada:`out`
    parameters.

For example, a procedure reading integers from the network could have one of
the following specifications:

.. code-block:: ada

    procedure Read_Int
       (Stream  :     Network_Stream;
        Success : out Boolean;
        Result  : out Integer);

    function Read_Int
       (Stream :     Network_Stream;
        Result : out Integer) return Boolean;

While reading an out variable before writing to it should, ideally, trigger an
error, imposing that as a rule would cause either inefficient run-time checks
or complex compile-time rules. So from the user's perspective an out parameter
acts like an uninitialized variable when the subprogram is invoked.

.. admonition:: In the GNAT toolchain

    GNAT will detect simple cases of incorrect use of out parameters.
    For example, the compiler will emit a warning for the following program:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Out_Params

        procedure Outp is
           procedure Foo (A : out Integer) is
              B : Integer := A;
              --             ^ Warning on reference
              --               to uninitialized A
           begin
              A := B;
           end Foo;
        begin
           null;
        end Outp;

Forward declaration of subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we saw earlier, a subprogram can be declared without being fully defined,
This is possible in general, and can be useful if you need subprograms to be
mutually recursive, as in the example below:

.. ?? This example is rather contrived but I suspect that any realistic
.. ?? example would be in the context of recursive data structures and
.. ?? mutually dependent types, which have not been covered yet.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Mutually_Recursive_Subprograms
    :class: ada-run

    procedure Mutually_Recursive_Subprograms is
        procedure Compute_A (V : Natural);
        --  Forward declaration of Compute_A

        procedure Compute_B (V : Natural) is
        begin
           if V > 5 then
              Compute_A (V - 1);
              --  Call to Compute_A
           end if;
        end Compute_B;

        procedure Compute_A (V : Natural) is
        begin
           if V > 2 then
              Compute_B (V - 1);
              --  Call to Compute_B
           end if;
        end Compute_A;
    begin
       Compute_A (15);
    end Mutually_Recursive_Subprograms;

.. _Intro_Ada_Subprogram_Renaming:

Renaming
--------

Subprograms can be renamed by using the :ada:`renames` keyword and declaring a
new name for a subprogram:

.. code-block:: ada

    procedure New_Proc renames Original_Proc;

This can be useful, for example, to improve the readability of your application
when you're using code from external sources that cannot be changed in your
system. Let's look at an example:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming
   :class: nosyntax-check

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String) is
    begin
       Put_Line (A_Message);
    end A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

As the wording in the name of procedure above implies, we cannot change its
name. We can, however, rename it to something like :ada:`Show` in our test
application and use this shorter name. Note that we also have to declare all
parameters of the original subprogram |mdash| we may rename them, too, in the
declaration. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming
   :class: nosyntax-check

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming is

       procedure Show (S : String) renames
         A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show ("Hello World!");
    end Show_Renaming;

Note that the original name
(:ada:`A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed`) is still visible
after the declaration of the :ada:`Show` procedure.

We may also rename subprograms from the standard library. For example, we may
rename :ada:`Integer'Image` to :ada:`Img`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Integer_Image_Renaming

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Image_Renaming is

       function Img (I : Integer) return String
         renames Integer'Image;

    begin
       Put_Line (Img (2));
       Put_Line (Img (3));
    end Show_Image_Renaming;

Renaming also allows us to introduce default expressions that were not available
in the original declaration. For example, we may specify :ada:`"Hello World!"`
as the default for the :ada:`String` parameter of the :ada:`Show` procedure:

.. code-block:: ada

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming_Defaults is

       procedure Show (S : String := "Hello World!")
         renames
           A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show;
    end Show_Renaming_Defaults;
