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

    This section was originally  published as
    `Gem #142 : Exception-ally <https://www.adacore.com/gems/gem-142-exceptions>`_

.. admonition:: Relevant topics

    - `The Package Exceptions <http://www.ada-auth.org/standards/2xrm/html/RM-11-4-1.html>`_

.. todo::

    Complete section!


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

Perhaps surprisingly, the Ada standard indicates cases where objects passed to
:ada:`out` and :ada:`in out` parameters might not be updated when a procedure
terminates due to an exception. Let's take an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_1

    with Ada.Text_IO;  use Ada.Text_IO;
    procedure Show_Out_Uninitialized is

       procedure Local (A : in out Integer; Error : Boolean) is
       begin
          A := 1;

          if Error then
             raise Program_Error;
          end if;
       end Local;

       B : Integer := 0;

    begin
       Local (B, Error => True);
    exception
       when Program_Error =>
          Put_Line ("Value for B is" & Integer'Image (B));  --  "0"
    end Show_Out_Uninitialized;

This program outputs a value of 0 for :ada:`B`, whereas the code indicates that
:ada:`A` is assigned before raising the exception, and so the reader might
expect :ada:`B` to also be updated.

The catch, though, is that a compiler must by default pass objects of
elementary types (scalars and access types) by copy and might choose to do so
for other types (records, for example), including when passing :ada:`out` and
:ada:`in out` parameters. So what happens is that while the formal parameter
:ada:`A` is properly initialized, the exception is raised before the new value
of :ada:`A` has been copied back into :ada:`B` (the copy will only happen on a
normal return).

In general, any code that reads the actual object passed to an :ada:`out` or
:ada:`in out` parameter after an exception is suspect and should be avoided.
GNAT has useful warnings here, so that if we simplify the above code to:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_2

    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_Out_Uninitialized_Warnings is

        procedure Local (A : in out Integer) is
        begin
           A := 1;
           raise Program_Error;
        end Local;

       B : Integer := 0;

    begin
       Local (B);
    exception
       when others =>
          Put_Line ("Value for B is" & Integer'Image (B));
    end Show_Out_Uninitialized_Warnings;

We now get a compilation warning that the pass-by-copy formal may have no
effect.

Of course, GNAT is not able to point out all such errors (see first example
above), which in general would require full flow analysis.

The behavior is different when using parameter types that the standard mandates
passing by reference, such as tagged types for instance. So the following code
will work as expected, updating the actual parameter despite the exception:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_3

    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_Out_Initialized_Rec is

       type Rec is tagged record
          Field : Integer;
       end record;

       procedure Local (A : in out Rec) is
       begin
          A.Field := 1;
          raise Program_Error;
       end Local;

       V : Rec;

    begin
       V.Field := 0;
       Local (V);
    exception
       when others => Put_Line ("Value of Field is" & V.Field'Img); -- "1"
    end Show_Out_Initialized_Rec;

It's worth mentioning that GNAT provides a pragma called :ada:`Export_Procedure`
that forces reference semantics on :ada:`out` parameters. Use of this pragma
would ensure updates of the actual parameter prior to abnormal completion of the
procedure. However, this pragma only applies to library-level procedures, so the
examples above have to be rewritten to avoid the use of a nested procedure, and
really this pragma is intended mainly for use in interfacing with foreign code.
The code below shows an example that ensures that :ada:`B` is set to 1 after the
call to :ada:`Local`:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_4

    package Exported_Procedures is

      procedure Local (A : in out Integer; Error : Boolean);
      pragma Export_Procedure (Local, Mechanism => (A => Reference));

    end Exported_Procedures;

    package body Exported_Procedures is

       procedure Local (A : in out Integer; Error : Boolean) is
       begin A := 1;
          if Error then
             raise Program_Error;
          end if;
       end Local;

    end Exported_Procedures;

    with Ada.Text_IO;         use Ada.Text_IO;
    with Exported_Procedures; use Exported_Procedures;

    procedure Show_Out_Reference is
       B : Integer := 0;
    begin
       Local (B, Error => True);
    exception
       when Program_Error =>
          Put_Line ("Value for B is" & Integer'Image (B)); -- "1"
    end Show_Out_Reference;

In the case of direct assignments to global variables, the behavior in the
presence of exceptions is somewhat different. For predefined exceptions, most
notably :ada:`Constraint_Error`, the optimization permissions allow some
flexibility in whether a global variable is or is not updated when an exception
occurs (see
`Ada RM 11.6 <http://www.ada-auth.org/standards/12rm/html/RM-11-6.html>`_). For
instance, the following code makes an incorrect assumption:

.. code-block:: none

    X := 0;     -- about to try addition
    Y := Y + 1; -- see if addition raises exception
    X := 1      -- addition succeeded

A program is not justified in assuming that :ada:`X = 0` if the addition raises
an exception (assuming :ada:`X` is a global here). So any such assumptions in a
program are incorrect code which should be fixed.

.. admonition:: In the Ada Reference Manual

    - `11.6 Exceptions and Optimization <http://www.ada-auth.org/standards/12rm/html/RM-11-6.html>`_


Suppressing checks
------------------

.. admonition:: Relevant topics

    - `Suppressing Checks <http://www.ada-auth.org/standards/2xrm/html/RM-11-5.html>`_

.. todo::

    Complete section!
