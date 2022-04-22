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
    :class: ada-expect-compile-error

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


Limited Visibility
------------------

Sometimes, we might face the situation where two packages depend on
information from each other. Let's consider a package :ada:`A` that depends
on a package :ada:`B`, and vice-versa:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Circular_Dependency
    :class: ada-expect-compile-error

    with B; use B;

    package A is

       type T1 is record
          Value : T2;
       end record;

    end A;

    with A; use A;

    package B is

       type T2 is record
          Value : T1;
       end record;

    end B;

Here, we have two
:ref:`mutually dependent types <Adv_Ada_Mutually_Dependent_Types>` :ada:`T1`
and :ada:`T2`, which are declared in two packages :ada:`A` and :ada:`B` that
refer to each other. These with clauses constitute a circular dependency, so
the compiler cannot compile either of those packages.

One way to solve this problem is by transforming this circular dependency into
a partial dependency. We do this by declaring a limited view |mdash| using a
limited with clause. To use a limited with clause for a package :ada:`P`, we
simply write :ada:`limited with P`.

If a package :ada:`A` has a limited view of a package :ada:`B`, then all types
from package :ada:`B` are visible as if they had been declared as
:ref:`incomplete types <Adv_Ada_Incomplete_Types>`. For the specific case of
the previous source-code example, this would be the limited view of package
:ada:`B` from package :ada:`A`\ 's perspective:

.. code-block:: ada

    package B is

       --  Incomplete type
       type T2;

    end B;

As we've seen previously,

- we cannot declare objects of incomplete types, but we can declare access
  types and anonymous access objects of incomplete types. Also,

- we can use anonymous access types to declare
  :ref:`mutually dependent types <Adv_Ada_Mutually_Dependent_Types>`.

Keeping this information in mind, we can now correct the previous code by using
limited with clauses for both packages and declaring the components using
anonymous access types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_View

    limited with B;

    package A is

       type T1 is record
          Ref : access B.T2;
       end record;

    end A;

    limited with A;

    package B is

       type T2 is record
          Ref : access A.T1;
       end record;

    end B;

As expected, the code can now be compiled without issues.

.. admonition:: In the Ada Reference Manual

    - `10.1.2 Context Clauses - With Clauses <http://www.ada-auth.org/standards/12rm/html/RM-10-1-2.html>`_

Limited view and private with clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can apply a limited view to private with clauses that we've discussed
earlier. For a package :ada:`P`, we do this by simply writing
:ada:`limited private with P`.

Let's reuse the previous source-code example and convert types :ada:`T1` and
:ada:`T2` to private types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_View

    limited private with B;

    package A is

       type T1 is private;

    private

       --  Here, we have limited visibility
       --  of package B

       type T1 is record
          Ref : access B.T2;
       end record;

    end A;

    limited private with A;

    package B is

       type T2 is private;

    private

       --  Here, we have limited visibility
       --  of package A

       type T2 is record
          Ref : access A.T1;
       end record;

    end B;

In this updated version of the source-code example, we have not only just a
limited view of packages :ada:`A` and :ada:`B`, but also, each package is just
visible in the private part of the other package.

Limited view and other elements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's important to mention that the limited view only includes type declarations
|mdash| as incomplete types. All other declarations are not visible when using
a limited with clause. For example, let's say we have a package :ada:`Info`
that declares a constant :ada:`Zero_Const` and a function :ada:`Zero_Func`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_View_Other_Elements

    package Info is

       function Zero_Func return Integer is (0);

       Zero_Const : constant := 0;

    end Info;

We want to use this information in package :ada:`A`. If we apply a limited view
to the :ada:`Info` package, however, this information won't be visible. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_View_Other_Elements
    :class: ada-expect-compile-error

    limited private with Info;

    package A is

       type T1 is private;

    private

       type T1 is record
          V : Integer := Info.Zero_Const;
          W : Integer := Info.Zero_Func;
       end record;

    end A;

As expected, compilation fails because of the limited visibility |mdash| as
:ada:`Zero_Const` and :ada:`Zero_Func` from the :ada:`Info` package are not
visible in the private part of :ada:`A`. (Of course, if we revert to full
visibility by simply removing the :ada:`limited` keyword from the example, the
code compiles just fine.)


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
