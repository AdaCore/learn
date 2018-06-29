Modular programming
===================
:code-config:`reset_accumulator=True`

So far, our examples have been simple standalone procedures.  Ada is helpful in
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

       --  This is a declarative part. You can put only
       --  declarations here, no statements

       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       type Workload_Type is array (Days range <>) of Natural;

       Workload : constant Workload_Type :=
          (Monday .. Thursday => 8,
           Friday             => 7,
           Saturday | Sunday  => 0);

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
       for D in Week.Days loop
       --       ^ Reference to Week.Days enumeration type
          Put_Line
            ("Workload for day " & Week.Days'Image (D)
             & " is " & Natural'Image (Week.Workload (D)));
       end loop;
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
       for D in Days loop
       --       ^ Reference to Week.Days enum type
          Put_Line  -- Put_Line comes from Ada.Text_IO.
            ("Workload for day " & Days'Image (D)
             & " is " & Natural'Image (Workload (D)));
       end loop;
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

In the somewhat artificial example above, the :ada:`Week` package only has
declarations and no body. That's not a mistake: in a package specification,
which is what is illustrated above, you cannot declare bodies. Those have to be
in the package body.

.. code:: ada no_button

    package Week_2 is

       type Days is (Monday, Tuesday, Wednesday,
          Thursday, Friday, Saturday, Sunday);

       function Get_Workload (Day : Days) return Natural;

    end Week_2;

    package body Week_2 is

       --  The body contains additional declarations, not visible from the
       --  spec, or anywhere outside of the body
       type WorkLoad_Type is array (Days range <>) of Natural;
       Workload : constant Workload_Type :=
          (Monday .. Thursday => 8, Friday => 7, Saturday | Sunday => 0);

       function Get_Workload (Day : Days) return Natural is
       begin
          return Workload (Day);
       end Get_Workload;
    end Week_2;

Here we can see that the body of the :ada:`Get_Workload` function has to be
declared in the body. Coincidentally, introducing a body allows us to put the
:ada:`Workload_Type` array type and the constant :ada:`Workload` in the body,
and make them inaccessible to the user of the :ada:`Week` package, providing a
first form of encapsulation.

This works because entities declared in the body are *only* visible in the
body.
