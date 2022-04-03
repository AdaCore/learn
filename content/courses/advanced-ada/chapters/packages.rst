Packages
========

.. include:: ../../global.txt

Private packages
----------------

.. todo::

    Complete section!


Private with clauses
--------------------

A private with clause allows us to refer to a package in the private part of
another package. For example, if we want to refer to package :ada:`P` in the
private part of :ada:`Data`, we can write :ada:`private with P`:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Simple_Private_With_Clause

    package P is

       type T is null record;

    end P;

    private with P;

    package Data is

       type T2 is private;

    private

       --  Information from P is
       --  visible here
       type T2 is new P.T;

    end Data;

    with Data; use Data;

    procedure Main is
       A : T2;
    begin
       null;
    end Main;

As you can see in the example, as the information from :ada:`P` is available in
the private part of :ada:`Data`, we can derive a new type :ada:`T2` based on
:ada:`T` from :ada:`P`. However, we cannot do the same in the visible part of
:ada:`Data`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Simple_Private_With_Clause

    private with P;

    package Data is

       --  ERROR: information from P
       --  isn't visible here

       type T2 is new P.T;

    end Data;

Also, the information from :ada:`P` is available in the package body. For
example, let's declare a :ada:`Process` procedure in the :ada:`P` package and
use it in the body of the :ada:`Data` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Simple_Private_With_Clause

    package P is

       type T is null record;

       procedure Process (A : T) is null;

    end P;

    private with P;

    package Data is

       type T2 is private;

       procedure Process (A : T2);

    private

       --  Information from P is
       --  visible here
       type T2 is new P.T;

    end Data;

    package body Data is

       procedure Process (A : T2) is
       begin
          P.Process (P.T (A));
       end Process;

    end Data;

    with Data; use Data;

    procedure Main is
       A : T2;
    begin
       null;
    end Main;

In the body of the :ada:`Data`, we can access information from the :ada:`P`
package |mdash| as we do in the :ada:`P.Process (P.T (A))` statement of the
:ada:`Process` procedure.

There's one case where using a private with clause is the only way to refer to
a package: when we want to refer to a private child package in another child
package. For example, here we have a package :ada:`P` and its two child
packages: :ada:`Private_Child` and :ada:`Public_Child`:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Private_With_Clause

    package P is

    end P;

    private package P.Private_Child is

       type T is null record;

    end P.Private_Child;

    private with P.Private_Child;

    package P.Public_Child is

       type T2 is private;

    private

       type T2 is new P.Private_Child.T;

    end P.Public_Child;

    with P.Public_Child; use P.Public_Child;

    procedure Test_Parent_Child is
       A : T2;
    begin
       null;
    end Test_Parent_Child;

In this example, we're referring to the :ada:`P.Private_Child` package in the
:ada:`P.Public_Child` package. As expected, this works fine. However, using a
*normal* with clause doesn't work in this case:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Private_With_Clause
    :class: ada-expect-compile-error

    with P.Private_Child;

    package P.Public_Child is

       type T2 is private;

    private

       type T2 is new P.Private_Child.T;

    end P.Public_Child;

This gives an error because the information from the :ada:`P.Private_Child`,
being a private child package, cannot be accessed in the public part of another
child package. In summary, unless both packages are private packages, it's only
possible to access the information from a private package in the private part
of a non-private child package.


Partial dependencies
--------------------

.. admonition:: Relevant topics

    - `Context Clauses - With Clauses <http://www.ada-auth.org/standards/2xrm/html/RM-10-1-2.html>`_

.. todo::

    Complete section!


Visibility
----------

.. todo::

    Complete section!


Use type clause
---------------

.. admonition:: Relevant topics

    - :ada:`use type` clause mentioned in
      `Use Clauses <http://www.ada-auth.org/standards/2xrm/html/RM-8-4.html>`_

.. todo::

    Complete section!
