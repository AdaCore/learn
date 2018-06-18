Privacy
=======
:code-config:`reset_accumulator=True`

One of the main principles of modular programming, as well as object oriented
programming, is `encapsulation <https://en.wikipedia.org/wiki/Encapsulation_(computer_programming)>`_.

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

.. code:: ada
    :class: ada-expect-compile-error

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
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

.. code:: ada

    package Stacks is
       type Stack is private;
       --  Declare a private type: You cannot depend on its
       --  implementation. You can only assign and test for
       --  equality.

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private

       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is array (Stack_Index) of Natural;

       type Stack is record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

In the above example, we define a stack type in the public part (known as the
"visible part" of the package spec in Ada), but the exact representation
of that type is private.

Then, in the private part, we define the representation of that type. We can
also declare other types that will be used as "helpers" for our main public
type. This is useful since declaring helper types is common in Ada.

A few words about terminology:

- The Stack type as viewed from the public part is called the partial view of
  the type. This is what clients have access to.

- The Stack type as viewed from the private part or the body of the package is
  called the full view of the type. This is what implementers have access to.

From the point of view of the client (the "with"ing unit), only the public
(visible) part is important, and the private part could as well not exist. It
makes it very easy to read linearly the part of the package that is important
for you.

.. code-block:: ada

    --  No need to read the private part to use the package
    package Stacks is
       type Stack is private;

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private
       ...
    end Stacks;

Here is how the ``Stacks`` package would be used:

.. code:: ada

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

Limited types
-------------

Ada's "limited type" facility allows you to declare a type for which
assignment and comparison operations are not automatically provided.

.. code:: ada

    package Stacks is
       type Stack is limited private;
       --  Limited type. Cannot assign nor compare.

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private
       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is array (Stack_Index) of Natural;

       type Stack is limited record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

.. code:: ada
    :class: ada-expect-compile-error

    with Stacks; use Stacks;

    procedure Main is
       S, S2 : Stack;
    begin
       S := S2;
       --  Illegal: S is limited.
    end Main;

This is useful because, for example, for some data types the built-in assignment
operation might be incorrect (for example when a deep copy is required).

Ada does allow you to overload the comparison operators "=" and "/" for limited
types (and to override the built-in declarations for non-limited types).

Ada also allows you to implement special semantics for assignment via
`controlled types <todo_link_to_controlled_types>`_. However, in some cases
assignment is simply inappropriate; one example is the ``File_Type`` from the
``Ada.Text_IO`` package, which is declared as a limited type and thus
attempts to assign one file to another would be detected as illegal.
