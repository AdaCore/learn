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

    - `11.4.2 Pragmas Assert and Assertion_Policy <http://www.ada-auth.org/standards/2xrm/html/RM-11-4-2.html>`_


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

.. todo::

    Complete section!


Suppressing checks
------------------

.. admonition:: Relevant topics

    - `Suppressing Checks <http://www.ada-auth.org/standards/2xrm/html/RM-11-5.html>`_

.. todo::

    Complete section!
