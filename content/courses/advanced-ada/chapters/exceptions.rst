Exceptions
==========

.. include:: ../../global.txt

Asserts
-------

When we want to indicate a condition in the code that must always be valid, we
can use the pragma :ada:`Assert`. As the name implies, when we use this pragma,
we're *asserting* some truth about the source-code. (We can also use the
procedural form, as we'll see later.)

.. admonition:: Important

    Another method to assert the truth about the source-code is to use
    :doc:`pre and post-conditions <./contracts>`.

A simple assert has this form:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assert_1

    procedure Show_Pragma_Assert is
       I : constant Integer := 10;

       pragma Assert (I = 10);
    begin
       null;
    end Show_Pragma_Assert;

In this example, we're asserting that the value of :ada:`I` is always 10. We
could also display a message if the assertion is false:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assert_2
    :class: ada-run-expect-failure

    procedure Show_Pragma_Assert is
       I : constant Integer := 11;

       pragma Assert (I = 10, "I is not 10");
    begin
       null;
    end Show_Pragma_Assert;

Similarly, we can use the procedural form of :ada:`Assert`. For example, the
code above can implemented as follows:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Procedure_Assert
    :class: ada-run-expect-failure

    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Procedure_Assert is
       I : constant Integer := 11;

    begin
       Assert (I = 10, "I is not 10");
    end Show_Procedure_Assert;

Note that a call to :ada:`Assert` is simply translated to a check |mdash| and
the :ada:`Assertion_Error` exception from the :ada:`Ada.Assertions` package
being raised in the case that the check fails. For example, the code above
roughly corresponds to this:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Assertion_Error
    :class: ada-run-expect-failure

    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Assertion_Error is
       I : constant Integer := 11;

    begin
       if I /= 10 then
          raise Assertion_Error with "I is not 10";
       end if;

    end Show_Assertion_Error;

.. admonition:: In the Ada Reference Manual

    - `11.4.2 Pragmas Assert and Assertion_Policy <http://www.ada-auth.org/standards/12rm/html/RM-11-4-2.html>`_


Assertion policies
------------------

We can activate and deactivate assertions based on assertion policies. We can do
that by using the pragma :ada:`Assertion_Policy`. As an argument to this pragma,
we indicate whether a specific policy must be checked or ignored.

For example, we can deactivate assertion checks by specifying
:ada:`Assert => Ignore`. Similarly, we can activate assertion checks by
specifying :ada:`Assert => Check`. Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assertion_Policy_1

    procedure Show_Pragma_Assertion_Policy is
       I : constant Integer := 11;

       pragma Assertion_Policy (Assert => Ignore);
    begin
       pragma Assert (I = 10);
    end Show_Pragma_Assertion_Policy;

Here, we're specifying that asserts shall be ignored. Therefore, the call to the
pragma :ada:`Assert` doesn't raise an exception. If we replace :ada:`Ignore`
with :ada:`Check` in the call to :ada:`Assertion_Policy`, the assert will raise
the :ada:`Assertion_Error` exception.

The following table presents all policies that we can set:

+----------------------------------+-------------------------------------------+
| Policy                           | Descripton                                |
+==================================+===========================================+
| :ada:`Assert`                    | Check assertions                          |
+----------------------------------+-------------------------------------------+
| :ada:`Static_Predicate`          | Check static predicates                   |
+----------------------------------+-------------------------------------------+
| :ada:`Dynamic_Predicate`         | Check dynamic predicates                  |
+----------------------------------+-------------------------------------------+
| :ada:`Pre`                       | Check pre-conditions                      |
+----------------------------------+-------------------------------------------+
| :ada:`Pre'Class`                 | Check pre-condition of classes of tagged  |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+
| :ada:`Post`                      | Check post-conditions                     |
+----------------------------------+-------------------------------------------+
| :ada:`Post'Class`                | Check post-condition of classes of tagged |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+
| :ada:`Type_Invariant`            | Check type invariants                     |
+----------------------------------+-------------------------------------------+
| :ada:`Type_Invariant'Class`      | Check type invariant of classes of tagged |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+

.. admonition:: In the GNAT toolchain

    Compilers are free to include policies that go beyond the ones listed above.
    For example, GNAT includes the following policies |mdash| called
    *assertion kinds* in this context:

    - :ada:`Assertions`
    - :ada:`Assert_And_Cut`
    - :ada:`Assume`
    - :ada:`Contract_Cases`
    - :ada:`Debug`
    - :ada:`Ghost`
    - :ada:`Initial_Condition`
    - :ada:`Invariant`
    - :ada:`Invariant'Class`
    - :ada:`Loop_Invariant`
    - :ada:`Loop_Variant`
    - :ada:`Postcondition`
    - :ada:`Precondition`
    - :ada:`Predicate`
    - :ada:`Refined_Post`
    - :ada:`Statement_Assertions`
    - :ada:`Subprogram_Variant`

    Also, in addtion to :ada:`Check` and :ada:`Ignore`, GNAT allows you to set
    a policy to :ada:`Disable` and :ada:`Suppressible`.

    You can read more about them in the
    `GNAT Reference Manual <https://gcc.gnu.org/onlinedocs/gnat_rm/Pragma-Assertion_005fPolicy.html>`_.

You can specify multiple policies in a single call to :ada:`Assertion_Policy`.
For example, you can activate all policies by writing:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Multiple_Assertion_Policies

    procedure Show_Multiple_Assertion_Policies is
       pragma Assertion_Policy
         (Assert               => Check,
          Static_Predicate     => Check,
          Dynamic_Predicate    => Check,
          Pre                  => Check,
          Pre'Class            => Check,
          Post                 => Check,
          Post'Class           => Check,
          Type_Invariant       => Check,
          Type_Invariant'Class => Check);
    begin
       null;
    end Show_Multiple_Assertion_Policies;

.. admonition:: In the GNAT toolchain

    With GNAT, policies can be specified in multiple ways. In addition to calls
    to :ada:`Assertion_Policy`, you can use
    `configuration pragmas files <https://gcc.gnu.org/onlinedocs/gnat_ugn/The-Configuration-Pragmas-Files.html#The-Configuration-Pragmas-Files>`_.
    You can use these files to specify all pragmas that are relevant to your
    application, including :ada:`Assertion_Policy`. In addition, you can manage
    the granularity for those pragmas. For example, you can use a global
    configuration pragmas file for your complete application, or even different
    files for each source-code file you have.

    Also, by default, all policies listed in the table above are deactivated,
    i.e. they're all set to :ada:`Ignore`. You can use the command-line switch
    :ada:`-gnata` to activate them.

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    :ada:`Default_Initial_Condition` policy

.. admonition:: In the Ada Reference Manual

    - `11.4.2 Pragmas Assert and Assertion_Policy <http://www.ada-auth.org/standards/12rm/html/RM-11-4-2.html>`_


``Ada.Exceptions`` package
--------------------------

.. note::

    Parts of this section were originally published as
    `Gem #142 : Exception-ally <https://www.adacore.com/gems/gem-142-exceptions>`_

The standard Ada run-time library provides the package :ada:`Ada.Exceptions`.
This package provides a number of services to help analyze exceptions.

Each exception is associated with a (short) message that can be set by the code
that raises the exception, as in the following code:

.. code-block:: ada

    raise Constraint_Error with "some message";

.. admonition:: Historically

    Since Ada 2005, we can use the
    :ada:`raise Constraint_Error with "some message"` syntax.
    In previous versions of Ada, you had to call the :ada:`Raise_Exception`
    procedure:

    .. code-block:: ada

        Ada.Exceptions.Raise_Exception         --  Ada 95
          (Constraint_Error'Identity, "some message");

    The new syntax is now very convenient, and developers should be encouraged
    to provide as much information as possible along with the exception.

.. admonition:: In the GNAT toolchain

    The length of the message is limited to 200 characters by default in GNAT,
    and messages longer than that will be truncated.

.. admonition:: In the Ada Reference Manual

    - `11.4.1 The Package Exceptions <http://www.ada-auth.org/standards/12rm/html/RM-11-4-1.html>`_


Retrieving exception information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Exceptions also embed information set by the run-time itself that can be
retrieved by calling the :ada:`Exception_Information` function. The function
:ada:`Exception_Information` also displays the :ada:`Exception_Message`.

For example:

.. code-block:: ada

    exception
        when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));

.. admonition:: In the GNAT toolchain

    In the case of GNAT, the information provided by an exception might include
    the source location where the exception was raised and a nonsymbolic
    traceback.

You can also retrieve this information individually. Here, you can use:

    - the :ada:`Exception_Name` functions |mdash| and its derivatives
      :ada:`Wide_Exception_Name` and :ada:`Wide_Wide_Exception_Name` |mdash| to
      retrieve the name of an exception.

    - the :ada:`Exception_Message` function to retrieve the message associated
      with an exception.

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Info switches=Compiler(-g);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Show_Exception_Info is

       Custom_Exception : exception;

       procedure Nested is
       begin
          raise Custom_Exception with "We got a problem";
       end Nested;

    begin
       Nested;

    exception
       when E : others =>
          Put_Line ("Exception info: " & Exception_Information (E));
          Put_Line ("Exception name: " & Exception_Name (E));
          Put_Line ("Exception msg:  " & Exception_Message (E));
    end Show_Exception_Info;


Debugging exceptions in the GNAT toolchain
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a typical exception handler that catches all unexpected exceptions in
the application:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Information switches=Compiler(-g);

    with Ada.Exceptions;
    with Ada.Text_IO;   use Ada.Text_IO;

    procedure Main is

        procedure Nested is
        begin
            raise Constraint_Error with "some message";
        end Nested;

    begin
        Nested;

    exception
        when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
    end Main;

The output we get when running the application is not very informative. To get
more information, we need to rerun the program in the debugger. To make the
session more interesting though, we should add debug information in the
executable, which means using the :option:`gnatmake -g` switch in the
:program:`gnatmake` command.

The session would look like the following (omitting some of the output from the
debugger):

.. code-block:: none

    > rm *.o      # Cleanup previous compilation
    > gnatmake -g main.adb
    > gdb ./main
    (gdb)  catch exception
    (gdb)  run
    Catchpoint 1, CONSTRAINT_ERROR at 0x0000000000402860 in main.nested () at main.adb:8
    8               raise Constraint_Error with "some message";

    (gdb) bt
    #0  <__gnat_debug_raise_exception> (e=0x62ec40 <constraint_error>) at s-excdeb.adb:43
    #1  0x000000000040426f in ada.exceptions.complete_occurrence (x=x@entry=0x637050)
    at a-except.adb:934
    #2  0x000000000040427b in ada.exceptions.complete_and_propagate_occurrence (
    x=x@entry=0x637050) at a-except.adb:943
    #3  0x00000000004042d0 in <__gnat_raise_exception> (e=0x62ec40 <constraint_error>,
    message=...) at a-except.adb:982
    #4  0x0000000000402860 in main.nested ()
    #5  0x000000000040287c in main ()

And we now know exactly where the exception was raised. But in fact, we could
have this information directly when running the application. For this, we need
to bind the application with the switch :option:`gnatmake -E`, which tells the
binder to store exception tracebacks in exception occurrences. Let's recompile
and rerun the application.

.. code-block:: none

    > rm *.o   # Cleanup previous compilation
    > gnatmake -g main.adb -bargs -E
    > ./main

    Exception name: CONSTRAINT_ERROR
    Message: some message
    Call stack traceback locations:
    0x10b7e24d1 0x10b7e24ee 0x10b7e2472

The traceback, as is, is not very useful. We now need to use another tool that
is bundled with GNAT, called :program:`addr2line`. Here is an example of its
use:

.. code-block:: none

    > addr2line -e main --functions --demangle 0x10b7e24d1 0x10b7e24ee 0x10b7e2472
    /path/main.adb:8
    _ada_main
    /path/main.adb:12
    main
    /path/b~main.adb:240

This time we do have a symbolic backtrace, which shows information similar to
what we got in the debugger.

For users on OSX machines, :program:`addr2line` does not exist. On these
machines, however, an equivalent solution exists. You need to link your
application with an additional switch, and then use the tool :program:`atos`,
as in:

.. code-block:: none

    > rm *.o
    > gnatmake -g main.adb -bargs -E -largs -Wl,-no_pie
    > ./main

    Exception name: CONSTRAINT_ERROR
    Message: some message
    Call stack traceback locations:
    0x1000014d1 0x1000014ee 0x100001472
    > atos -o main 0x1000014d1 0x1000014ee 0x100001472
    main__nested.2550 (in main) (main.adb:8)
    _ada_main (in main) (main.adb:12)
    main (in main) + 90

We will now discuss a relatively new switch of the compiler, namely
:option:`gnatmake -gnateE`. When used, this switch will generate extra
information in exception messages.

Let's amend our test program to:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Information switches=Compiler(-g,-gnateE);

    with Ada.Exceptions;
    with Ada.Text_IO;      use Ada.Text_IO;

    procedure Main is

        procedure Nested (Index : Integer) is
           type T_Array is array (1 .. 2) of Integer;
           T : constant T_Array := (10, 20);
        begin
           Put_Line (T (Index)'Img);
        end Nested;

    begin
        Nested (3);

    exception
        when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
    end Main;

When running the application, we see that the exception information (traceback)
is the same as before, but this time the exception message is set automatically
by the compiler. So we know we got a :ada:`Constraint_Error` because an
incorrect index was used at the named source location
(:file:`main.adb`, line 10). But the significant addition is the second
line of the message, which indicates exactly the cause of the error. Here, we
wanted to get the element at index 3, in an array whose range of valid indexes
is from 1 to 2. (No need for a debugger in this case.)

The column information on the first line of the exception message is also very
useful when dealing with null pointers. For instance, a line such as:

.. code-block:: ada

    A := Rec1.Rec2.Rec3.Rec4.all;

where each of the :ada:`Rec` is itself a pointer, might raise
:ada:`Constraint_Error` with a message "access check failed". This indicates for
sure that one of the pointers is null, and by using the column information it is
generally easy to find out which one it is.

.. admonition:: In the GNAT toolchain

    :program:`gnatmake` switches:

    .. program:: gnatmake

    .. option:: -g

        Include debug information.

    .. option:: -E

        Tell binder to store exception tracebacks in exception occurrences.

    .. option:: -gnateE

        Generate extra information in exception messages.


Exception renaming
------------------

.. admonition:: Relevant topics

    - `Exception Renaming Declarations <http://www.ada-auth.org/standards/2xrm/html/RM-8-5-2.html>`_

.. todo::

    Complete section!


Out and Uninitialized
---------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #150: Out and Uninitialized <https://www.adacore.com/gems/gem-150out-and-uninitialized>`_

.. todo::

    Complete section!


Suppressing checks
------------------

.. admonition:: Relevant topics

    - `Suppressing Checks <http://www.ada-auth.org/standards/2xrm/html/RM-11-5.html>`_

.. todo::

    Complete section!
