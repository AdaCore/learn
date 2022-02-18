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

Note that the :ada:`Assert` procedure raises an exception independently of the
assertion policy (:ada:`Assertion_Policy (Assert => Ignore)`). For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Assert_Procedure_Policy
    :class: ada-run-expect-failure

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Assert_Procedure_Policy is
       pragma Assertion_Policy (Assert => Ignore);

       I : constant Integer := 1;
    begin
       Put_Line ("------ Pragma Assert -----");
       pragma Assert (I = 0);

       Put_Line ("---- Procedure Assert ----");
       Assert (I = 0);

       Put_Line ("Finished.");
    end Show_Assert_Procedure_Policy;

Here, the :ada:`pragma Assert` is ignored due to the assertion policy. However,
the call to :ada:`Assert` is not ignored.

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

We can rename exceptions by using the an exception renaming declaration in this
form :ada:`Renamed_Exception : exception renames Existing_Exception;`. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Renaming
    :class: ada-run-expect-failure

    procedure Show_Exception_Renaming is
       CE : exception renames Constraint_Error;
    begin
       raise CE;
    end Show_Exception_Renaming;

Exception renaming creates a new view of the original exception. If we rename an
exception from package :ada:`A` in package :ada:`B`, that exception will become
visible in package :ada:`B`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Renaming_View switches=Compiler(-g,-gnateE);

    package Internal_Exceptions is

       Int_E : exception;

    end Internal_Exceptions;

    with Internal_Exceptions;

    package Test_Constraints is

       Ext_E : exception renames Internal_Exceptions.Int_E;

    end Test_Constraints;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    with Test_Constraints; use Test_Constraints;

    procedure Show_Exception_Renaming_View is
    begin
       raise Ext_E;
    exception
       when E : others =>
          Put_Line (Ada.Exceptions.Exception_Information (E));
    end Show_Exception_Renaming_View;

Here, we're renaming the :ada:`Int_E` exception in the :ada:`Test_Constraints`
package. The :ada:`Int_E` exception isn't directly visible in the
:ada:`Show_Exception_Renaming` procedure because we're not :ada:`with`\ing the
:ada:`Internal_Exceptions` package. However, it is indirectly visible
in that procedure via the renaming (:ada:`Ext_E`) in the :ada:`Test_Constraints`
package.

.. admonition:: In the Ada Reference Manual

    - `8.5.2 Exception Renaming Declarations <http://www.ada-auth.org/standards/12rm/html/RM-8-5-2.html>`_


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
