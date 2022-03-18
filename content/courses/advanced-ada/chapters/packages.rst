Packages
========

.. include:: ../../global.txt

.. _Adv_Ada_Private_Packages:

Private packages
----------------

In the
:doc:`Introduction to Ada course </courses/intro-to-ada/chapters/privacy>`,
we've seen that encapsulation plays an important role in modular programming.
By using the private part of a package specification, we can disclose some
information, but, at the same time, prevent that this information gets
accessed where it shouldn't be used directly. Similarly, we've seen that we can
use the private part of a package to distinguish between the
:ref:`partial and full view <Adv_Ada_Type_View>` of a data type. In this
section, we discuss a related concept: private packages.

We declare private packages by using the :ada:`private` keyword. For example,
let's say we have a package named :ada:`Data_Processing`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Private_Package_Decl

    package Data_Processing is

    --  ...

    end Data_Processing;

We simply write :ada:`private package` to declare a private child package named
:ada:`Calculations`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Private_Package_Decl

    private package Data_Processing.Calculations is

    --  ...

    end Data_Processing.Calculations;

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Private_Package

    package Data_Processing is

       type Data is private;

       procedure Process (D : in out Data);

    private

       type Data is null record;

    end Data_Processing;

    private package Data_Processing.Calculations is

       procedure Calculate (D : in out Data);

    end Data_Processing.Calculations;

    with Data_Processing.Calculations;
    use  Data_Processing.Calculations;

    package body Data_Processing is

       procedure Process (D : in out Data) is
       begin
          Calculate (D);
       end Process;

    end Data_Processing;

    package body Data_Processing.Calculations is

       procedure Calculate (D : in out Data) is
       begin
          --  Dummy implementation...
          null;
       end Calculate;

    end Data_Processing.Calculations;

    with Data_Processing; use Data_Processing;

    procedure Test_Data_Processing is
       D : Data;
    begin
       Process (D);
    end Test_Data_Processing;

In this example, we refer to the private child package :ada:`Calculations` in
the body of the :ada:`Data_Processing` package |mdash| by simply writing
:ada:`with Data_Processing.Calculations`. After that, we can call the
:ada:`Calculate` procedure normally in the :ada:`Process` procedure.

While we can use a with-clause of the private child package in the body of the
:ada:`Data_Processing` package, we cannot do the same outside this package. For
example, we cannot refer to it in the :ada:`Test_Data_Processing` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Private_Package

    with Data_Processing; use Data_Processing;

    with Data_Processing.Calculations;
    use  Data_Processing.Calculations;

    procedure Test_Data_Processing is
       D : Data;
    begin
       Calculate (D);
    end Test_Data_Processing;

As expected, we get a compilation error because :ada:`Calculations` is only
accessible within the :ada:`Data_Processing`, but not in the
:ada:`Test_Data_Processing` procedure.

The same restrictions apply to child packages of private packages. For example,
if we implement a child package of the :ada:`Calculations` package |mdash|
let's call it :ada:`Calculations.Child` |mdash|, we cannot refer to it in the
:ada:`Test_Data_Processing` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Private_Package

    package Data_Processing.Calculations.Child is

       procedure Process (D : in out Data);

    end Data_Processing.Calculations.Child;

    package body Data_Processing.Calculations.Child is

       procedure Process (D : in out Data) is
       begin
          Calculate (D);
       end Process;

    end Data_Processing.Calculations.Child;

    with Data_Processing; use Data_Processing;

    with Data_Processing.Calculations.Child;
    use  Data_Processing.Calculations.Child;

    procedure Test_Data_Processing is
       D : Data;
    begin
       Calculate (D);
    end Test_Data_Processing;

Again, as expected, we get an error because :ada:`Calculations.Child` |mdash|
being a child of a private package |mdash| has the same restricted view as its
parent package. Therefore, it cannot be visible in the
:ada:`Test_Data_Processing` procedure as well. We'll discuss more about
visibility :ref:`later <Adv_Ada_Package_Visibility>`.

Note that subprograms can also be declared private. We'll see this
:ref:`in another section <Adv_Ada_Private_Subprograms>`.

.. admonition:: In the Ada Reference Manual

    - `10.1.1 Compilation Units - Library Units <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-10-1-1.html>`_


.. _Adv_Ada_Private_With_Clauses:

Private with clauses
--------------------

Definition and usage
~~~~~~~~~~~~~~~~~~~~

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

Referring to private child package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

.. admonition:: In the Ada Reference Manual

    - `10.1.2 Context Clauses - With Clauses <http://www.ada-auth.org/standards/12rm/html/RM-10-1-2.html>`_


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
a partial dependency. We do this by limiting the visibility |mdash| using a
limited with clause. To use a limited with clause for a package :ada:`P`, we
simply write :ada:`limited with P`.

If a package :ada:`A` has limited visibility of a package :ada:`B`, then all
types from package :ada:`B` are visible as if they had been declared as
:ref:`incomplete types <Adv_Ada_Incomplete_Types>`. For the specific case of
the previous source-code example, this would be the limited visibility of
package :ada:`B` from package :ada:`A`\ 's perspective:

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
limited with clauses for package :ada:`A` and declaring the component of the
:ada:`T1` record using an anonymous access type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Visibility

    limited with B;

    package A is

       type T1 is record
          Ref : access B.T2;
       end record;

    end A;

    with A; use A;

    package B is

       type T2 is record
          Value : T1;
       end record;

    end B;

As expected, we can now compile the code without issues.

Note that we can also use limited with clauses for both packages. If we do
that, we must declare all components using anonymous access types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Visibility_2

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

Now, both packages :ada:`A` and :ada:`B` have limited visibility of each other.

.. admonition:: In the Ada Reference Manual

    - `10.1.2 Context Clauses - With Clauses <http://www.ada-auth.org/standards/12rm/html/RM-10-1-2.html>`_

Limited visibility and private with clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can limit the visibility and use
:ref:`private with clauses <Adv_Ada_Private_With_Clauses>` at the same time.
For a package :ada:`P`, we do this by simply writing
:ada:`limited private with P`.

Let's reuse the previous source-code example and convert types :ada:`T1` and
:ada:`T2` to private types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_Visibility

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

    private with A;

    package B is

       type T2 is private;

    private

       use A;

       --  Here, we have full visibility
       --  of package A

       type T2 is record
          Value : T1;
       end record;

    end B;

In this updated version of the source-code example, we have not only limited
visibility of package :ada:`B`, but also, each package is just visible
in the private part of the other package.

Limited visibility and other elements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's important to mention that the limited visibility we've been discussing so
far is restricted to type declarations |mdash| which are seen as incomplete
types. In fact, when we use a limited with clause, all other declarations have
no visibility at all! For example, let's say we have a package :ada:`Info` that
declares a constant :ada:`Zero_Const` and a function :ada:`Zero_Func`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_Visibility_Other_Elements

    package Info is

       function Zero_Func return Integer is (0);

       Zero_Const : constant := 0;

    end Info;

Also, let's say we want to use the information (from package :ada:`Info`) in
package :ada:`A`. If we have limited visibility of package :ada:`Info`,
however, this information won't be visible. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Packages.Limited_Private_Visibility_Other_Elements
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


.. _Adv_Ada_Package_Visibility:

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
