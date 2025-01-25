.. _Ada_Idioms_Programming_By_Extension:

Programming by Extension
========================

.. include:: ../../global.txt

Motivation
----------

When declaring entities in a package, developers should ensure that the client
view |mdash| the package visible part |mdash| contains no implementation
artifacts. Doing so is important conceptually, but also practically, because
any declarations visible to clients inevitably will be used by clients and, as
a result, will become permanent fixtures because removal would cause expensive
changes in the client code.

The intended client API declarations must be in the package visible part, of
course. The question, then, is whether to declare implementation artifacts in
the package private part or in the package body. Those are the two parts of a
package that do not make declarations compile-time visible to client code.

Some of these entities must be declared in the package private part because
they are required in the declaration of some other entity appearing in that
part. For example, when using the
:ref:`ADT idiom <Ada_Idioms_Abstract_Data_Types>`, an ancillary type might be
required for the completion of the private type. That was the case with the
:ref:`ADT version <Ada_Idioms_Abstract_Data_Types_Code_Example>` of the
:ada:`Integer_Stacks` package, repeated here for convenience:

.. code-block:: ada

    package Integer_Stacks is
       type Stack is private;
       --  ...
       Capacity : constant := 100;
    private
       type Content is array  (1 .. Capacity) of Integer;
       type Stack is record
          Values : Content;
          Top    : Integer range 0 .. Capacity := 0;
       end record;
    end Integer_Stacks;

The array type :ada:`Content` is required for the :ada:`Stack` record component
because anonymously-typed array components are illegal. Clients have no
business using the type :ada:`Content` directly so although it would be legal
to declare it in the public part, declaration in the private part is more
appropriate.

Likewise, a function called to provide the default initial value for a private
type's component must be declared prior to the reference. If the function is
truly only part of the implementation, we should declare it in the package
private part rather than the public part.

In contrast, there may be implementation-oriented entities that are referenced
only in the package body. They could be declared in the package body but could
alternatively be declared in the package declaration's private part. Those are
the entities (declarations) in question for this idiom.

For a concrete example, here is an elided
:ref:`ADM version of the stack abstraction <Ada_Idioms_Abstract_Data_Machines_Code_Example>`,
with the stack state declared in the package body:

.. code-block:: ada

    package Integer_Stack is
       procedure Push (Item : in Integer);
       procedure Pop (Item : out Integer);
       function Empty return Boolean;
       Capacity : constant := 100;
    end Integer_Stack;

    package body Integer_Stack is
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;
       procedure Push  (Item : in Integer) is
       begin
          --  ...
          Top := Top + 1;
          Values (Top) := Item;
       end Push;
       procedure Pop (Item : out Integer) is ...
       function Empty return Boolean is ...
    end Integer_Stack;

We could add the private part to the package declaration and move the
state of the
:ref:`ADM <Ada_Idioms_Abstract_Data_Machines>` |mdash| the two variables in
this case |mdash| up there without any other changes. The subprogram bodies
have the same visibility to the two variables either way. (There is no
requirement for the :ada:`Content` type because :ada:`Values` is not a record
component; anonymously-typed array objects are legal.) From the viewpoint of
the language and the abstraction, the location is purely up to the developer.

Implementation(s)
-----------------

When you have a choice of placement, putting the state in either the package
private part or the package body is reasonable, but only one of the two is
amenable to future requirements.

Specifically, placement in the private part of the package allows
*programming by extension* [1]_ via hierarchical *child* packages. Child
packages can be written immediately after the *parent* package but can also be
written years later, thus accommodating changes due to new requirements.

Programming by extension allows us to extend an existing package's facilities
without having to change the existing package at all. Avoiding source code
changes to the existing package is important because doing so might be very
expensive. In certified systems, the changed package would require
re-certification, for example. Changes to the parent package are avoidable
because child packages have compile-time visibility to the private part of the
ancestor package (actually the entire ancestor package hierarchy, any of which
could be useful). Thus, the extension in the child package can depend on
declarations in the existing parent package's private part.

Therefore, if the developer can know, with certainty, that no visibility beyond
the one package will ever be appropriate, the declaration should go in the
package body. Otherwise, it should go in the package private part, just in case
an extension becomes necessary later.

Using our ADM stack example, we could move the state from the package body to
the private part:

.. code-block:: ada

    package Integer_Stack is
       procedure Push (Item : in Integer);
       procedure Pop (Item : out Integer);
       function Empty return Boolean;
       Capacity : constant := 100;
    private
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;
    end Integer_Stack;

Note that the private part was not otherwise required by the language in this
example.

With that change, a new child package could be created with extended
functionality:

.. code-block:: ada

    package Integer_Stack.Utils is
       procedure Reset;
    end Integer_Stack. Utils;

    package body Integer_Stack.Utils is
       procedure Reset is
       begin
          Top := 0;
       end Reset;
    end Integer_Stack.Utils;

These child packages are not client code, they contain extensions to the
existing abstraction. Hence they are part of what may be considered a subsystem
consisting of the original package and the new child package. The child package
contains an extension of the abstraction defined by the parent package, so the
child is directly related.  Given that characterization of child packages we
can say that the parent package private part is not visible to client code and,
therefore, does not represent a *leak* of implementation details to clients.


Pros
----

We can extend an abstraction without changing the source code defining that
abstraction, thereby meeting new requirements without incurring potentially
expensive redevelopment.


Cons
----

Clients could abuse the hierarchical package visibility rules by creating a
child package that doesn't really extend the existing package abstraction.

Abuse of the visibility rules allows child packages that can break the
abstraction. For example, if we only change the name of procedure :ada:`Reset`
in package :ada:`Integer_Stack.Utils` to :ada:`Lose_All_Contained_Data` then
the routine has a rather different complexion.

Similarly, abuse of the visibility rules allows child packages that can
indirectly leak state from the parent package. For example:

.. code-block:: ada

    package Integer_Stack.Leaker is
       function Current_Top return Integer;
    end Integer_Stack.Leaker;

    package body Integer_Stack.Leaker is
       function Current_Top return Integer is (Top);
    end Integer_Stack.Leaker;

We could do that without even requiring a package body, using an expression
function for the completion:

.. code-block:: ada

    package Integer_Stack.Leaker is
       function Current_Top return Integer;
    private
       function Current_Top return Integer is (Top);
    end Integer_Stack.Leaker;

The function must be completed in the private part because that is where
compile-time visibility to the parent begins within a child package.

Code reviews are the only way to detect these abuses, although detection of
potential cases could be automated with an analysis tool such as
`Libadalang <https://github.com/AdaCore/libadalang>`_.


Relationship With Other Idioms
------------------------------

We assume the use of the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` or
:ref:`Abstract Data Machine <Ada_Idioms_Abstract_Data_Machines>` idioms for the
existing package abstraction, as well as for the child package.


Notes
-----

This guideline will already be used when developing a subsystem (a set of
related packages in an overall hierarchy) as a structuring approach during
initial development. The idiom discussed here is yet another reason to use the
private part, but in this case for the sake of the future, rather than initial,
development.

The very first version of Ada (Ada 83) did not have hierarchical library units so,
typically, anything not required in the private part was declared in the
package body. Declaring them in the private part would only clutter the code
that had to be there, without any benefit. The author's personal experience and
anecdotal information confirms that after Ada 95 introduced hierarchical
library units, some declarations in existing package bodies tended to
"percolate up" to the package declarations' private parts.


.. only:: builder_html

    .. rubric:: Bibliography

.. [1] Barnes, J. (1998). Programming In Ada 95, Addison-Wesley.
