Modular programming
===================

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

.. sectionauthor:: Raphaël Amiard

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

.. code:: ada no_button

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

.. code:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  References the Week package, and adds a dependency from Main
    --  to Week

    procedure Main is
    begin
       Put_Line ("First day of the week is " & Week.Mon);
    end Main;

Packages let you make your code modular, separating your programs into
semantically significant units. Additionally the separation of a package's
specification from its body (which we will see below) can reduce compilation
time.

While the :ada:`with` clause indicates a dependency, you can see in the example
above that you still need to prefix the referencing of entities from the Week
package by the name of the package. (If we had included a "use Week" clause,
then such a prefix would not have been necessary.)

Accessing entities from a package uses the dot notation, :ada:`A.B`, which is
the same notation as the one used to access record fields.

A :ada:`with` clause can *only* appear in the prelude of a compilation unit
(i.e., before the reserved word, such as ``procedure``, that marks the
beginning of the unit). It is not allowed anywhere else.  This rule is only
needed for methodological reasons: the person reading your code should be able
to see immediately which units the code depends on.

.. admonition:: In other languages

    Packages look similar to, but are semantically very different from, header
    files in C/C++.

    - The first and most important distinction is that packages are a language-level
      mechanism. This is in contrast to a #include'd header file, which is a
      functionality of the C preprocessor.

    - An immediate consequence is that the "with" construct is a semantic
      inclusion mechanism, not a text inclusion mechanism. Hence, when you
      "with" a package, you are saying to the compiler "I'm depending on this
      semantic unit", and not "include this bunch of text in place here".

    - The effect of a package thus does not vary depending on where it has been
      "with"ed from. Contrast this with C/C++, where the meaning of the
      included text depends on the context in which the #include appears.

      This allows compilation/recompilation to be more efficient. It also
      allows tools like IDEs to have correct information about the semantics
      of a program. In turn, this allows better tooling in general, and code
      that is more analyzable, even by humans.

    An important benefit of Ada "with" clauses when compared to #include is
    that it is stateless. The order of "with" and "use" clauses does not matter,
    and can be changed without side effects.

.. admonition:: In the GNAT toolchain

    The Ada language standard does not mandate any particular relationship
    between source files and packages; for example, in theory you can put all
    your code in one file, or use your own file naming conventions. In
    practice, however, an implementation will have specific rules. With GNAT,
    each top-level compilation unit needs to go into a separate file. In the
    example above, the ``Week`` package will be in an ``.ads`` file (for Ada
    specification), and the ``Main`` procedure will be in an ``.adb`` file
    (for Ada body).

Using a package
---------------

As we have seen above, the :ada:`with` clause indicates a dependency on another
package. However, every reference to an entity coming from the ``Week`` package
had to be prefixed by the full name of the package. It is possible to make
every entity of a package visible directly in the current scope, using the
:ada:`use` clause.

In fact, we have been using the :ada:`use` clause since almost the beginning of
this tutorial.

.. code:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    --                    ^ Make every entity of the Ada.Text_IO package
    --                      directly visible.
    with Week;

    procedure Main is
       use Week;
       --  Make every entity of the Week package directly visible.
    begin
       Put_Line ("First day of the week is " & Mon);
    end Main;

As you can see in the example above:

- :ada:`Put_Line` is a subprogram that comes from the :ada:`Ada.Text_IO`
  package. We can reference it directly because we have "use"d the package at
  the top of the :ada:`Main` unit.

- Unlike :ada:`with` clauses, a :ada:`use` clause can be placed either in the
  prelude, or in any declarative region. In the latter case the :ada:`use`
  clause will have an effect in its containing lexical scope.

Package body
------------

In the simple example above, the :ada:`Week` package only has
declarations and no body. That's not a mistake: in a package specification,
which is what is illustrated above, you cannot declare bodies. Those have to be
in the package body.

.. code:: ada no_button

    package Operations is

       -- Declaration
       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer;

       function Get_Increment_Value return Integer;

    end Operations;

.. code:: ada no_button

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

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Operations;

    procedure Main is
       use Operations;

       I : Integer := 0;
       R : Integer;

       procedure Display_Update_Values is
          Incr : constant Integer := Get_Increment_Value;
       begin
          Put_Line (Integer'Image (I)
                    & " incremented by " & Integer'Image (Incr)
                    & " is "             & Integer'Image (R));
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
