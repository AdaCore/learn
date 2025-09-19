.. _Intro_Ada_Course_Privacy:

Privacy
=======

.. include:: ../../../global.txt

One of the main principles of modular programming, as well as object oriented
programming, is
:wikipedia:`encapsulation <Encapsulation_(computer_programming)>`.

Encapsulation, briefly, is the concept that the implementer of a piece of
software will distinguish between the code's public interface and its private
implementation.

This is not only applicable to software libraries but wherever abstraction is
used.

In Ada, the granularity of encapsulation is a bit different from most
object-oriented languages, because privacy is generally specified at the
package level.

Basic encapsulation
-------------------

.. code:: ada compile_button project=Courses.Intro_To_Ada.Privacy.Encapsulate
    :class: ada-expect-compile-error

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
    end Encapsulate;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate is

       procedure Hello is
       begin
          Put_Line ("Hello");
       end Hello;

       procedure Hello2 is
       begin
          Put_Line ("Hello #2");
       end Hello2;

    end Encapsulate;

    with Encapsulate;

    procedure Main is
    begin
       Encapsulate.Hello;
       Encapsulate.Hello2;
       --  Invalid: Hello2 is not visible
    end Main;

Abstract data types
-------------------

With this high-level granularity, it might not seem obvious how to hide the
implementation details of a type. Here is how it can be done in Ada:

.. code:: ada no_button project=Courses.Intro_To_Ada.Privacy.Stacks
    :class: ada-syntax-only

    package Stacks is
       type Stack is private;
       --  Declare a private type: You cannot depend
       --  on its implementation. You can only assign
       --  and test for equality.

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private

       subtype Stack_Index is
         Natural range 1 .. 10;

       type Content_Type is
         array (Stack_Index) of Natural;

       type Stack is record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

    package body Stacks is

       procedure Push (S   : in out Stack;
                       Val :        Integer) is
       begin
          --  Missing implementation!
          null;
       end Push;

       procedure Pop (S   : in out Stack;
                      Val :    out Integer) is
       begin
          --  Dummy implementation!
          Val := 0;
       end Pop;

    end Stacks;

In the above example, we define a stack type in the public part (known as the
*visible part* of the package spec in Ada), but the exact representation
of that type is private.

Then, in the private part, we define the representation of that type. We can
also declare other types that will be used as *helpers* for our main public
type. This is useful since declaring helper types is common in Ada.

A few words about terminology:

- The :ada:`Stack` type as viewed from the public part is called the partial
  view of the type. This is what clients have access to.

- The :ada:`Stack` type as viewed from the private part or the body of the
  package is called the full view of the type. This is what implementers have
  access to.

From the point of view of the client (the *with*'ing unit), only the public
(visible) part is important, and the private part could as well not exist. It
makes it very easy to read linearly the part of the package that is important
for you.

.. code-block:: ada

    --  No need to read the private part to use the package
    package Stacks is
       type Stack is private;

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private
       ...
    end Stacks;

Here is how the :ada:`Stacks` package would be used:

.. code-block:: ada

    --  Example of use
    with Stacks; use Stacks;

    procedure Test_Stack is
       S : Stack;
       Res : Integer;
    begin
       Push (S, 5);
       Push (S, 7);
       Pop (S, Res);
    end Test_Stack;

.. _Intro_Ada_Limited_Types:

Limited types
-------------

Ada's *limited type* facility allows you to declare a type for which
assignment and comparison operations are not automatically provided.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Privacy.Limited_Stacks
    :class: ada-expect-compile-error

    package Stacks is
       type Stack is limited private;
       --  Limited type. Cannot assign nor compare.

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private
       subtype Stack_Index is
         Natural range 1 .. 10;

       type Content_Type is
         array (Stack_Index) of Natural;

       type Stack is limited record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

    package body Stacks is

       procedure Push (S   : in out Stack;
                       Val :        Integer) is
       begin
          --  Missing implementation!
          null;
       end Push;

       procedure Pop (S   : in out Stack;
                      Val :    out Integer) is
       begin
          --  Dummy implementation!
          Val := 0;
       end Pop;

    end Stacks;

    with Stacks; use Stacks;

    procedure Main is
       S, S2 : Stack;
    begin
       S := S2;
       --  Illegal: S is limited.
    end Main;

This is useful because, for example, for some data types the built-in assignment
operation might be incorrect (for example when a deep copy is required).

Ada does allow you to overload the comparison operators :ada:`=` and :ada:`/=` for limited
types (and to override the built-in declarations for non-limited types).

Ada also allows you to implement special semantics for assignment via
:arm:`controlled types <7-6>`. However, in some cases
assignment is simply inappropriate; one example is the :ada:`File_Type` from the
:ada:`Ada.Text_IO` package, which is declared as a limited type and thus
attempts to assign one file to another would be detected as illegal.

Child packages & privacy
------------------------

We've seen previously (in the :ref:`child packages section <Intro_Ada_Child_Packages>`)
that packages can have child packages. Privacy plays an important role in child
packages. This section discusses some of the privacy rules that apply to child
packages.

Although the private part of a package :ada:`P` is meant to encapsulate
information, certain parts of a child package :ada:`P.C` can have access to
this private part of :ada:`P`. In those cases, information from the private
part of :ada:`P` can then be used as if it were declared in the public part of
its specification. To be more specific, the body of :ada:`P.C` and the private
part of the specification of :ada:`P.C` have access to the private part of
:ada:`P`. However, the public part of the specification of :ada:`P.C` only has
access to the public part of :ada:`P`'s specification. The following table
summarizes this:

+-------------------------------+-------------------------------+
| Part of a child package       | Access to the private part of |
|                               | its parent's specification    |
+===============================+===============================+
| Specification: public part    |                               |
+-------------------------------+-------------------------------+
| Specification: private part   | |check|                       |
+-------------------------------+-------------------------------+
| Body                          | |check|                       |
+-------------------------------+-------------------------------+

The rest of this section shows examples of how this access to private
information actually works for child packages.

Let's first look at an example where the body of a child package :ada:`P.C`
has access to the private part of the specification of its parent :ada:`P`.
We've seen, in a previous source-code example, that the :ada:`Hello2` procedure
declared in the private part of the :ada:`Encapsulate` package cannot be used
in the :ada:`Main` procedure, since it's not visible there. This limitation
doesn't apply, however, for parts of the child packages of the
:ada:`Encapsulate` package. In fact, the body of its child package
:ada:`Encapsulate.Child` has access to the :ada:`Hello2` procedure and can call
it there, as you can see in the implementation of the :ada:`Hello3` procedure
of the :ada:`Child` package:

.. code:: ada run_button project=Courses.Intro_To_Ada.Privacy.Encapsulate_Child

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
       --  But visible in child packages
    end Encapsulate;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate is

       procedure Hello is
       begin
          Put_Line ("Hello");
       end Hello;

       procedure Hello2 is
       begin
          Put_Line ("Hello #2");
       end Hello2;

    end Encapsulate;

    package Encapsulate.Child is

       procedure Hello3;

    end Encapsulate.Child;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate.Child is

       procedure Hello3 is
       begin
          --  Using private procedure Hello2
          --  from the parent package
          Hello2;
          Put_Line ("Hello #3");
       end Hello3;

    end Encapsulate.Child;

    with Encapsulate.Child;

    procedure Main is
    begin
       Encapsulate.Child.Hello3;
    end Main;

The same mechanism applies to types declared in the private part of a parent
package. For instance, the body of a child package can access components of a
record declared in the private part of its parent package. Let's look at an
example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Privacy.Private_Type_Child

    package My_Types is

       type Priv_Rec is private;

    private

       type Priv_Rec is record
          Number : Integer := 42;
       end record;

    end My_Types;

    package My_Types.Ops is

       procedure Display (E : Priv_Rec);

    end My_Types.Ops;

    with Ada.Text_IO; use Ada.Text_IO;

    package body My_Types.Ops is

       procedure Display (E : Priv_Rec) is
       begin
          Put_Line ("Priv_Rec.Number: "
                    & Integer'Image (E.Number));
       end Display;

    end My_Types.Ops;

    with Ada.Text_IO;  use Ada.Text_IO;

    with My_Types;     use My_Types;
    with My_Types.Ops; use My_Types.Ops;

    procedure Main is
       E : Priv_Rec;
    begin
       Put_Line ("Presenting information:");

       --  The following code would trigger a
       --  compilation error here:
       --
       --  Put_Line ("Priv_Rec.Number: "
       --            & Integer'Image (E.Number));

       Display (E);
    end Main;

In this example, we don't have access to the :ada:`Number` component of the
record type :ada:`Priv_Rec` in the :ada:`Main` procedure. You can see this in
the call to :ada:`Put_Line` that has been commented-out in the implementation
of :ada:`Main`. Trying to access the :ada:`Number` component there would
trigger a compilation error. But we do have access to this component in the
body of the :ada:`My_Types.Ops` package, since it's a child package of the
:ada:`My_Types` package. Therefore, :ada:`Ops`'s body has access to the
declaration of the :ada:`Priv_Rec` type |mdash| which is in the private part of
its parent, the :ada:`My_Types` package. For this reason, the same call to
:ada:`Put_Line` that would trigger a compilation error in the :ada:`Main`
procedure works fine in the :ada:`Display` procedure of the :ada:`My_Types.Ops`
package.

This kind of privacy rules for child packages allows for extending the
functionality of a parent package and, at the same time, retain its
encapsulation.

As we mentioned previously, in addition to the package body, the private part
of the specification of a child package :ada:`P.C` also has access to the
private part of the specification of its parent :ada:`P`. Let's look at an
example where we declare an object of private type :ada:`Priv_Rec` in the
private part of the child package :ada:`My_Types.Child` *and* initialize the
:ada:`Number` component of the :ada:`Priv_Rec` record directly:

.. code-block:: ada

    package My_Types.Child is

    private

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

As expected, we wouldn't be able to initialize this component if we moved this
declaration to the public (visible) part of the same child package:

.. code-block:: ada

    package My_Types.Child is

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

The declaration above triggers a compilation error, since type :ada:`Priv_Rec`
is private. Because the public part of :ada:`My_Types.Child` is also visible
outside the child package, Ada cannot allow accessing private information in
this part of the specification.
