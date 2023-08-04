Modular programming
===================

.. include:: ../../global.txt

So far, our examples have been simple standalone subprograms.  Ada is helpful in
that regard, since it allows arbitrary declarations in a declarative part. We
were thus able to declare our types and variables in the bodies of main
procedures.

However, it is easy to see that this is not going to scale up for real-world
applications.  We need a better way to structure our programs into modular and
distinct units.

Ada encourages the separation of programs into multiple packages and
sub-packages, providing many tools to a programmer on a quest for a perfectly
organized code-base.

Packages
--------

Here is an example of a package declaration in Ada:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

And here is how you use it:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  References the Week package, and
    --  adds a dependency from Main to Week

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Week.Mon);
    end Main;

Packages let you make your code modular, separating your programs into
semantically significant units. Additionally the separation of a package's
specification from its body (which we will see below) can reduce compilation
time.

While the :ada:`with` clause indicates a dependency, you can see in the example
above that you still need to prefix the referencing of entities from the Week
package by the name of the package. (If we had included a :ada:`use Week` clause,
then such a prefix would not have been necessary.)

Accessing entities from a package uses the dot notation, :ada:`A.B`, which is
the same notation as the one used to access record fields.

A :ada:`with` clause can *only* appear in the prelude of a compilation unit
(i.e., before the reserved word, such as :ada:`procedure`, that marks the
beginning of the unit). It is not allowed anywhere else.  This rule is only
needed for methodological reasons: the person reading your code should be able
to see immediately which units the code depends on.

.. admonition:: In other languages

    Packages look similar to, but are semantically very different from, header
    files in C/C++.

    - The first and most important distinction is that packages are a language-level
      mechanism. This is in contrast to a :c:`#include`\'d header file, which is a
      functionality of the C preprocessor.

    - An immediate consequence is that the :ada:`with` construct is a semantic
      inclusion mechanism, not a text inclusion mechanism. Hence, when you
      :ada:`with` a package, you are saying to the compiler "I'm depending on
      this semantic unit", and not "include this bunch of text in place here".

    - The effect of a package thus does not vary depending on where it has been
      :ada:`with`\ed from. Contrast this with C/C++, where the meaning of the
      included text depends on the context in which the :c:`#include` appears.

      This allows compilation/recompilation to be more efficient. It also
      allows tools like IDEs to have correct information about the semantics
      of a program. In turn, this allows better tooling in general, and code
      that is more analyzable, even by humans.

    An important benefit of Ada :ada:`with` clauses when compared to
    :c:`#include` is that it is stateless. The order of :ada:`with` and
    :ada:`use` clauses does not matter, and can be changed without side
    effects.

.. admonition:: In the GNAT toolchain

    The Ada language standard does not mandate any particular relationship
    between source files and packages; for example, in theory you can put all
    your code in one file, or use your own file naming conventions. In
    practice, however, an implementation will have specific rules. With GNAT,
    each top-level compilation unit needs to go into a separate file. In the
    example above, the :ada:`Week` package will be in an ``.ads`` file (for Ada
    specification), and the :ada:`Main` procedure will be in an ``.adb`` file
    (for Ada body).

.. _Intro_Ada_Use_Clause:

Using a package
---------------

As we have seen above, the :ada:`with` clause indicates a dependency on another
package. However, every reference to an entity coming from the :ada:`Week`
package had to be prefixed by the full name of the package. It is possible to
make every entity of a package visible directly in the current scope, using the
:ada:`use` clause.

In fact, we have been using the :ada:`use` clause since almost the beginning of
this tutorial.

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    with Ada.Text_IO; use Ada.Text_IO;
    --                ^ Make every entity of the
    --                  Ada.Text_IO package
    --                  directly visible.
    with Week;

    procedure Main is
       use Week;
       --  Make every entity of the Week
       --  package directly visible.
    begin
       Put_Line ("First day of the week is " & Mon);
    end Main;

As you can see in the example above:

- :ada:`Put_Line` is a subprogram that comes from the :ada:`Ada.Text_IO`
  package. We can reference it directly because we have :ada:`use`\d the
  package at the top of the :ada:`Main` unit.

- Unlike :ada:`with` clauses, a :ada:`use` clause can be placed either in the
  prelude, or in any declarative region. In the latter case the :ada:`use`
  clause will have an effect in its containing lexical scope.

Package body
------------

In the simple example above, the :ada:`Week` package only has
declarations and no body. That's not a mistake: in a package specification,
which is what is illustrated above, you cannot declare bodies. Those have to be
in the package body.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

    package Operations is

       --  Declaration
       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer;

       function Get_Increment_Value return Integer;

    end Operations;

    package body Operations is

       Last_Increment : Integer := 1;

       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer is
       begin
          if Incr /= 0 then
             Last_Increment := Incr;
          end if;

          return I + Last_Increment;
       end Increment_By;

       function Get_Increment_Value return Integer is
       begin
          return Last_Increment;
       end Get_Increment_Value;

    end Operations;

Here we can see that the body of the :ada:`Increment_By` function has to be
declared in the body. Coincidentally, introducing a body allows us to put the
:ada:`Last_Increment` variable in the body, and make them inaccessible to the
user of the :ada:`Operations` package, providing a first form of encapsulation.

This works because entities declared in the body are *only* visible in the
body.

This example shows how :ada:`Last_Increment` is used indirectly:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

    with Ada.Text_IO; use Ada.Text_IO;
    with Operations;

    procedure Main is
       use Operations;

       I : Integer := 0;
       R : Integer;

       procedure Display_Update_Values is
          Incr : constant Integer :=
                   Get_Increment_Value;
       begin
          Put_Line (Integer'Image (I)
                    & " incremented by "
                    & Integer'Image (Incr)
                    & " is "
                    & Integer'Image (R));
          I := R;
       end Display_Update_Values;
    begin
       R := Increment_By (I);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 5);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 10);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;
    end Main;

.. _Intro_Ada_Child_Packages:

Child packages
--------------

Packages can be used to create hierarchies. We achieve this by using child
packages, which extend the functionality of their parent package. One example
of a child package that we've been using so far is the :ada:`Ada.Text_IO`
package. Here, the parent package is called :ada:`Ada`, while the child package
is called :ada:`Text_IO`. In the previous examples, we've been using the
:ada:`Put_Line` procedure from the :ada:`Text_IO` child package.

.. admonition:: Important

    Ada also supports nested packages. However, since they can be more
    complicated to use, the recommendation is to use child packages instead.
    Nested packages will be covered in the advanced course.

Let's begin our discussion on child packages by taking our previous
:ada:`Week` package:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

If we want to create a child package for :ada:`Week`, we may write:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child is

       function Get_First_Of_Week return String;

    end Week.Child;

Here, :ada:`Week` is the parent package and :ada:`Child` is the child
package. This is the corresponding package body of :ada:`Week.Child`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child is

       function Get_First_Of_Week return String is
       begin
          return Mon;
       end Get_First_Of_Week;

    end Week.Child;

In the implementation of the :ada:`Get_First_Of_Week` function, we can use
the :ada:`Mon` string directly, even though it was declared in the parent
package :ada:`Week`. We don't write :ada:`with Week` here because all
elements from the specification of the :ada:`Week` package |mdash| such as
:ada:`Mon`, :ada:`Tue` and so on |mdash|  are visible in the child package
:ada:`Week.Child`.

Now that we've completed the implementation of the :ada:`Week.Child` package,
we can use elements from this child package in a subprogram by simply writing
:ada:`with Week.Child`. Similarly, if we want to use these elements directly,
we write :ada:`use Week.Child` in addition. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO; use Ada.Text_IO;
    with Week.Child;  use Week.Child;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
    end Main;

Child of a child package
~~~~~~~~~~~~~~~~~~~~~~~~

So far, we've seen a two-level package hierarchy. But the hierarchy that we
can potentially create isn't limited to that. For instance, we could extend
the hierarchy of the previous source code example by declaring a
:ada:`Week.Child.Grandchild` package. In this case, :ada:`Week.Child` would
be the parent of the :ada:`Grandchild` package. Let's consider this
implementation:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package Week.Child.Grandchild is

       function Get_Second_Of_Week return String;

    end Week.Child.Grandchild;

    package body Week.Child.Grandchild is

       function Get_Second_Of_Week return String is
       begin
          return Tue;
       end Get_Second_Of_Week;

    end Week.Child.Grandchild;

We can use this new :ada:`Grandchild` package in our test application in the
same way as before: we can reuse the previous test application and adapt the
:ada:`with` and :ada:`use`, and the function call. This is the updated code:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO; use Ada.Text_IO;

    with Week.Child.Grandchild;
    use  Week.Child.Grandchild;

    procedure Main is
    begin
       Put_Line ("Second day of the week is "
                 & Get_Second_Of_Week);
    end Main;

Again, this isn't the limit for the package hierarchy. We could continue to
extend the hierarchy of the previous example by implementing a
:ada:`Week.Child.Grandchild.Grand_grandchild` package.

Multiple children
~~~~~~~~~~~~~~~~~

So far, we've seen a single child package of a parent package. However, a
parent package can also have multiple children. We could extend the example
above and implement a :ada:`Week.Child_2` package. For example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child_2 is

       function Get_Last_Of_Week return String;

    end Week.Child_2;

Here, :ada:`Week` is still the parent package of the :ada:`Child` package,
but it's also the parent of the :ada:`Child_2` package. In the same way,
:ada:`Child_2` is obviously one of the child packages of :ada:`Week`.

This is the corresponding package body of :ada:`Week.Child_2`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child_2 is

       function Get_Last_Of_Week return String is
       begin
          return Sun;
       end Get_Last_Of_Week;

    end Week.Child_2;

We can now reference both children in our test application:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO;  use Ada.Text_IO;
    with Week.Child;   use Week.Child;
    with Week.Child_2; use Week.Child_2;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
       Put_Line ("Last day of the week is "
                 & Get_Last_Of_Week);
    end Main;

Visibility
~~~~~~~~~~

In the previous section, we've seen that elements declared in a parent package
specification are visible in the child package. This is, however, not the case
for elements declared in the package body of a parent package.

Let's consider the package :ada:`Book` and its child
:ada:`Additional_Operations`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility
    :class: ada-syntax-only

    package Book is

       Title : constant String :=
         "Visible for my children";

       function Get_Title return String;

       function Get_Author return String;

    end Book;

    package Book.Additional_Operations is

       function Get_Extended_Title return String;

       function Get_Extended_Author return String;

    end Book.Additional_Operations;

This is the body of both packages:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    package body Book is

       Author : constant String :=
         "Author not visible for my children";

       function Get_Title return String is
       begin
          return Title;
       end Get_Title;

       function Get_Author return String is
       begin
          return Author;
       end Get_Author;

    end Book;

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          --  "Author" string declared in the body
          --  of the Book package is not visible
          --  here. Therefore, we cannot write:
          --
          --  return "Book Author: " & Author;

          return "Book Author: Unknown";
       end Get_Extended_Author;

    end Book.Additional_Operations;

In the implementation of the :ada:`Get_Extended_Title`, we're using the
:ada:`Title` constant from the parent package :ada:`Book`. However, as
indicated in the comments of the :ada:`Get_Extended_Author` function, the
:ada:`Author` string |mdash| which we declared in the body of the :ada:`Book`
package |mdash| isn't visible in the :ada:`Book.Additional_Operations`
package. Therefore, we cannot use it to implement the
:ada:`Get_Extended_Author` function.

We can, however, use the :ada:`Get_Author` function from :ada:`Book` in the
implementation of the :ada:`Get_Extended_Author` function to retrieve this
string. Likewise, we can use this strategy to implement the
:ada:`Get_Extended_Title` function. This is the adapted code:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Get_Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          return "Book Author: " & Get_Author;
       end Get_Extended_Author;

    end Book.Additional_Operations;

This is a simple test application for the packages above:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    with Ada.Text_IO; use Ada.Text_IO;

    with Book.Additional_Operations;
    use  Book.Additional_Operations;

    procedure Main is
    begin
       Put_Line (Get_Extended_Title);
       Put_Line (Get_Extended_Author);
    end Main;

By declaring elements in the body of a package, we can implement encapsulation
in Ada. Those elements will only be visible in the package body, but nowhere
else. This isn't, however, the only way to achieve encapsulation in Ada: we'll
discuss other approaches in the :doc:`./privacy` chapter.

.. _Intro_Ada_Package_Renaming:

Renaming
--------

Previously, we've mentioned that
:ref:`subprograms can be renamed <Intro_Ada_Subprogram_Renaming>`. We can rename
packages, too. Again, we use the :ada:`renames` keyword for that. The following
example renames the :ada:`Ada.Text_IO` package as :ada:`TIO`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Text_IO

    with Ada.Text_IO;

    procedure Main is
       package TIO renames Ada.Text_IO;
    begin
       TIO.Put_Line ("Hello");
    end Main;

We can use renaming to improve the readability of our code by using shorter
package names. In the example above, we write :ada:`TIO.Put_Line` instead of
the longer version (:ada:`Ada.Text_IO.Put_Line`). This approach is especially
useful when we don't :ada:`use` packages and want to avoid that the code
becomes too verbose.

Note we can also rename subprograms and objects inside packages. For instance,
we could have just renamed the :ada:`Put_Line` procedure in the source code
example above:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Put_Line

    with Ada.Text_IO;

    procedure Main is
       procedure Say (Something : String)
         renames Ada.Text_IO.Put_Line;
    begin
       Say ("Hello");
    end Main;

In this example, we rename the :ada:`Put_Line` procedure to :ada:`Say`.
