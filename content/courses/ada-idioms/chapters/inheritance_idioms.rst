.. _Ada_Idioms_Inheritance_Idioms:

Using Building Blocks to Express Inheritance Idioms
===================================================

.. include:: ../../global.txt

Motivation
----------

Betrand Meyer's magisterial book on OOP [1]_ includes a taxonomy of inheritance
idioms. Two especially well-known entries in that taxonomy are
:wikipedia:`Subtype Inheritance <Subtyping>` and
:wikipedia:`Implementation Inheritance <Inheritance_(object-oriented_programming)>`.
The name of the first idiom is perhaps confusing from an Ada point of view
because Ada subtypes have a different meaning. In Ada terms we are talking about
*derived types*. A derived type is a new, distinct type based on (i.e., derived
from) some existing type. We will informally refer to the existing ancestor type
as the *parent* type, and the new type as the *child* type.  The term *Subtype*
in the idiom name refers to the child type.

Subtype Inheritance is the most well-known idiom for inheritance because it's
based on the notion of a taxonomy, in which categories and disjoint
subcategories are identified. For example, we can say that dogs, cats, and
dolphins are mammals, and that all mammals are animals:

.. uml::
   :align: center
   :width: 440pt

    object Animal
    object Mammal
    object Dog
    object Cat
    object Dolphin

    Animal <|-- Mammal
    Mammal <|-- Dog
    Mammal <|-- Cat
    Mammal <|-- Dolphin

By saying that the subcategories are disjoint we mean that, for example, dogs
are neither cats nor dolphins and cannot be treated as if they are.

In software, we use various constructs to represent the categories and
subcategories and use inheritance to organize them. As mentioned above, in Ada,
we express that inheritance via derived types representing the categories and
subcategories. Ada's strong typing ensures they are treated as disjoint
entities.

Although the derived child type is distinct from the parent type, the
child is the same *kind* as the parent type. Some authors use *kind of*
as the name for the relationship between the child and parent. Meyer
uses the term *is-a* [1]_, a popular term that we will use too. For
example, a cat *is a* mammal, and also is an animal.

The fundamental difference between
:ref:`Subtype Inheritance <Ada_Idioms_Subtype_Inheritance>` and
:ref:`Implementation Inheritance <Ada_Idioms_Implementation_Inheritance>` is
whether clients have compile-time visibility to the *is-a* relationship between
the parent and child types. The relationship exists in both idioms but is only
visible to clients in one. In Subtype Inheritance, clients do have compile-time
visibility to the relationship, while in Implementation Inheritance, clients
don't have that visibility.

Consequently, with Subtype Inheritance, all of the inherited operations
become part of the child type's visible interface. In contrast, with
Implementation Inheritance, none of those parent capabilities are part of
the visible interface: the inherited parent capabilities are only available
internally, to implement the child type's representation and its primitive
operations.


Building Blocks
~~~~~~~~~~~~~~~

Ada uses distinct *building block* constructs to compose types that have
specific characteristics and capabilities. In particular, Ada packages, with
their control over compile-time visibility, are modules. Private types are
combined with packages to define
:ref:`abstract data types <Ada_Idioms_Abstract_Data_Types>` having hidden
representations. Sets of related types are presented explicitly by class-wide
types.

In addition, simple reserved words may be attached to a type declaration to
refine or expand the capabilities of the type. These type declarations include
declarations for derived types, providing considerable flexibility and
expressive power for controlling the client's view of the child and parent
types.

For example, in Ada, full dynamic OOP capabilities require type
declarations to be decorated with the reserved word :ada:`tagged`. However,
from its earliest days, Ada has also supported a static form of
inheritance, using types that are not tagged. The solution we describe
below works with both forms of inheritance.

The developeralso has a choice of whether the parent type and/or the child type
is a private type. Using private types is the default design choice, forthe
sake ofdesigningintermsofabstract data types, but  is nevertheless optional.

In addition, a type can be both private and tagged. This possibility raises the
question of whether the type is *visibly tagged*, i.e., whether the client view
of the type includes the tagged characteristic, and hence the corresponding
capabilities. Recall that a private type is declared in two steps: the first
part occurs in the visible part of the package and introduces the type name to
clients. The second part |mdash| the type completion |mdash| appears in the
package private part and specifies the type's actual representation. The
question arises because the first step, i.e., the declaration in the package's
visible part, need not be tagged, yet can be tagged in the completion in the
package private part. For example:

.. code-block:: ada

    package P is
       type Foo is private;  --  not visibly tagged for clients
       -- operations on type Foo
    private
       type Foo is tagged record  -- tagged completion
          ...
       end record;
    end P;

In the above, :ada:`Foo` is not visibly tagged except in the package private
part and the package body. As a consequence, the capabilities of tagged types
are not available to clients using type :ada:`Foo`. Clients cannot refer to
:ada:`Foo'Class`, for example. (The opposite arrangement |mdash| tagged in the
visible client view but not actually tagged in the private view |mdash| is not
legal, because clients would be promised capabilities that are not actually
available.)

When the parent type is tagged, the type derivation syntax for the child is a
*type extension* declaration that introduces the child type's name, specifies
the parent type, and then extends the parent representation with child-specific
record components, if any. For example:

.. code-block:: ada

    type Child is new Parent with record ... end record;

Even though the child type declaration does not include the reserved word
:ada:`tagged` the child will be a tagged type because the parent type is tagged.
The compiler would not allow the extension construct for a non-tagged parent
type.

Just as a private type can be visibly tagged or not, a private type can be
*visibly derived* or not. When it is visibly derived, clients have a view of the
private type that includes the fact of the derivation from the parent type.
Otherwise, clients have no view of the parent type. Whether or not the child is
visibly derived, the representation is not compile-time visible to clients, as
for any private type. For example, type :ada:`Foo` is not visibly derived in the
following:

.. code-block:: ada

    package P is
       type Foo is tagged private;  --  visibly tagged but not visibly derived
       -- ...
    end P;

To be visibly derived, we declare the child type as a private type using a
*private extension*. A private extension is like a type extension, in that it
introduces the child type name and the parent type. But like any private type
declaration, it does not specify the type's representation. This is the first of
the two steps for declaring a private type; hence it appears in the package
visible part. For example:

.. code-block:: ada

    with ...
    package P is
       type Child is new Parent with private;  -- visibly derived from Parent
    private
       type Child is new Parent with record ... end record;
    end P;

The representation additions are not expressed until the private type's
completion in the package private part, using a type extension. The steps are
the same two for any private type: a declaration in the package visible part,
with a completion in the package private part. The difference is the client
visibility to the parent type.


Solution
--------

There are two *solutions* in this entry, one for each of the two inheritance
idioms under discussion. First, we will specify our building block choices,
then show the two idiom expressions in separate subsections.

- We use tagged types for the sake of providing  full OOP capabilities. That is
  the most common choice when inheritance is involved. The static form of
  inheritance has cases in which it is useful, but those cases are very
  narrow in applicability.

- We  assume that the parent type and the child type are both private
  types, i.e., abstract data types, because that is the best practice. See the
  :ref:`Abstract Data Type idiom <Ada_Idioms_Abstract_Data_Types>` for
  justification and details.

- To provide the most general capabilities, we assume the parent type is
  visibly tagged.

- We're going to declare the child type in a distinct, dedicated package,
  following the :ref:`ADT idiom <Ada_Idioms_Abstract_Data_Types>`. This
  package may or may not be a child of the parent package. This solution's
  approach does not require a child package's special compile-time
  visibility, although a child package is often necessary for the sake of
  that visibility.

- Whether the child type is visibly derived will vary with the
  :ref:`inheritance idiom <Ada_Idioms_Implementation_Inheritance>` solution.

To avoid unnecessary code duplication, we use the same parent type,
declared as a simple tagged private type, in the examples for the two idiom
solutions.  The parent type could itself be derived from some other tagged
type, but that changes nothing conceptually significant. We declare parent
type in package :ada:`P` as follows:

.. code-block:: ada

    package P is
       type Parent is tagged private;  -- visibly tagged
       --  primitive operations with type Parent as the
       --  controlling formal parameter
    private
       type Parent is tagged record ... end record;
    end P;


.. _Ada_Idioms_Subtype_Inheritance:

Subtype Inheritance
~~~~~~~~~~~~~~~~~~~

Recall that Subtype Inheritance requires clients to have compile-time
visibility to the *is-a* relationship between the child and parent types. We
can satisfy that requirement if we make the child visibly derived from the
parent. Hence we declare the private type as a private extension in the visible
part of the package:

.. code-block:: ada

    with P; use P;
    package Q is
      type Child is new Parent with private;
      --  implicit, inherited primitive Parent operations declared here,
      --  now for type Child
      --  additional primitives for Child explicitly declared, if any
    private
      type Child is new Parent with record ... end record;
    end Q;

The primitive operations from the parent type are implicitly declared
immediately after the private extension declaration. That means those
operations are in the visible part of the package, hence clients can invoke
them. Any additional operations for the client interface will be explicitly
declared in the visible part as well, as will any overriding declarations
for those inherited operations that are to be changed.

For example, here is a basic bank account
:ref:`ADT <Ada_Idioms_Abstract_Data_Types>` that we will use as the parent type
in a derivation:

.. code:: ada no_button project=Courses.Ada_Idioms.Inheritance_Idioms.Subtype_Inheritance_Bank switches=Compiler(-gnatX)

    with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
    with Ada.Containers.Doubly_Linked_Lists;

    package Bank is

       type Basic_Account is tagged private
         with Type_Invariant'Class => Consistent_Balance (Basic_Account);

       function Consistent_Balance (This : Basic_Account) return Boolean;

       type Currency is delta 0.01 digits 12;

       procedure Deposit (This   : in out Basic_Account;
                          Amount : Currency) with
         Pre'Class  => Open (This) and Amount > 0.0,
         Post'Class => Balance (This) = Balance (This)'Old + Amount;

       procedure Withdraw (This   : in out Basic_Account;
                           Amount : Currency) with
         Pre'Class  => Open (This) and Funds_Available (This, Amount),
         Post'Class => Balance (This) = Balance (This)'Old - Amount;

       function Balance (This : Basic_Account) return Currency
         with Pre'Class => Open (This);

       procedure Report_Transactions (This : Basic_Account)
         with Pre'Class => Open (This);

       procedure Report (This : Basic_Account)
         with Pre'Class => Open (This);

       function Open (This : Basic_Account) return Boolean;

       procedure Open
         (This            : in out Basic_Account;
          Name            : String;
          Initial_Deposit : Currency)
       with Pre'Class => not Open (This),
            Post'Class => Open (This);

       function Funds_Available (This : Basic_Account;
                                 Amount : Currency) return Boolean is
         (Amount > 0.0 and then Balance (This) >= Amount)
       with Pre'Class => Open (This);

    private

       package Transactions is new
         Ada.Containers.Doubly_Linked_Lists (Element_Type => Currency);

       type Basic_Account is tagged record
          Owner           : Unbounded_String;
          Current_Balance : Currency := 0.0;
          Withdrawals     : Transactions.List;
          Deposits        : Transactions.List;
       end record;

       function Total (This : Transactions.List) return Currency is
         (This'Reduce ("+", 0.0));

    end Bank;

We could then declare an interest-bearing bank account using Subtype
Inheritance:

.. code:: ada no_button project=Courses.Ada_Idioms.Inheritance_Idioms.Subtype_Inheritance_Bank switches=Compiler(-gnatX)

    package Bank.Interest_Bearing is

       type Account is new Basic_Account with private;

       overriding
       function Consistent_Balance (This : Account) return Boolean;

       function Minimum_Balance (This : Account) return Currency;

       overriding
       procedure Open
         (This            : in out Account;
          Name            : String;
          Initial_Deposit : Currency)
       with Pre => Initial_Deposit >= Minimum_Balance (This);

       overriding
       procedure Withdraw (This : in out Account;  Amount : Currency);

       function Penalties_Accrued (This : Account) return Currency;
       function Interest_Accrued (This : Account) return Currency;

    private

       type Account is new Basic_Account with record
          Penalties          : Transactions.List;
          Interest_Earned    : Transactions.List;
          Days_Under_Minimum : Natural := 0;
       end record;

    end Bank.Interest_Bearing;

The new type :ada:`Bank.Interest_Bearing.Account` inherits all the
:ada:`Basic_Account` operations in the package visible part. They are,
therefore, available to clients. Some of those inherited operations are
overridden so that their behavior can be changed. Additional operations
specific to the new type are also declared in the visible part so they are
added to the client API.

The package private part and the body of package
:ada:`Bank.Interest_Bearing` have visibility to the private part of package
:ada:`Bank` because the new package is a child of package :ada:`Bank`. That
makes the private function :ada:`Bank.Total` visible in the child package,
along with the components of the record type :ada:`Basic_Account`.

Note that there is no language requirement that the actual parent type in the
private type's completion be the one named in the private extension declaration
presented to clients. The parent type in the completion must only be in the same
derivation class |mdash| be the same kind of type |mdash| so that it satisfies
the *is-a* relationship stated to clients.

For example, we could start with a basic graphics shape:

.. code-block:: ada

    package Graphics is
       type Shape is tagged private;
       --  operations for type Shape ...
       ...
    end Graphics;

We could then declare a subcategory of :ada:`Shape` that allows translation in
some 2-D space:

.. code-block:: ada

    package Graphics.Translatable is
       type Translatable_Shape is new Graphics.Shape with private;
       procedure Translate (This : in out Translatable_Shape;  X, Y : in Float);
    ...
    end Graphics.Translatable;

Given that, we could now declare another type visibly derived from :ada:`Shape`,
but using :ada:`Translatable_Shape` as the actual parent type:

.. code-block:: ada

    with Graphics;
    private with Graphics.Translatable;
    package Geometry is
       type Circle is new Graphics.Shape with private;
       --  operations for type Circle, inherited from Shape,
       --   and any new ops added ...
    private
       use Graphics.Translatable;
       type Circle is new Translatable_Shape with record ... end record;
    end Geometry;

In the type extension that completes type :ada:`Circle` in the package private
part above, the extended parent type is not the one presented to clients, i.e.,
:ada:`Graphics.Shape`. Instead, the parent type is another type that is derived
from type :ada:`Shape`. That substitution is legal and reasonable because
:ada:`Translatable_Shape` necessarily can do anything that :ada:`Shape` can do.
To understand why that is legal, it is helpful to imagine that there is a
*contract* between the package public part and the private part regarding type
:ada:`Circle`. As long as :ada:`Circle` can do everything promised to clients
|mdash| i.e., inherited :ada:`Shape` operations |mdash| the contract is
fulfilled. :ada:`Circle` inherits :ada:`Shape` operations because
:ada:`Translatable_Shape` inherits those operations. The fact that :ada:`Circle`
can do more than is contractually required by the client view is perfectly fine.


.. _Ada_Idioms_Implementation_Inheritance:

Implementation Inheritance
~~~~~~~~~~~~~~~~~~~~~~~~~~

Recall that with Implementation Inheritance clients do not have compile-time
visibility to the *is-a* relationship between the parent and child types. We
meet that requirement by not making the child visibly derived from the parent.
Therefore, we declare the child type as a simple tagged private type and only
mention the parent in the child type's completion in the package private
part:

.. code-block:: ada

    with P; use P;
    package Q is
      type Child is tagged private;
       --  explicitly declared primitives for Child
    private
      type Child is new Parent with record ...
       --  implicit, inherited primitive operations with type Child
       --  as the controlling formal parameter
    end Q;

The primitive operations from the parent type are implicitly
declared immediately after the type extension, but these declarations are now
located in the package private part. Therefore, the inherited primitive
operations are not compile-time visible to clients. Hence clients cannot invoke
them. These operations are only visible (after the type completion) in the
package private part and the package body, for use with the implementation of
the explicitly declared primitive operations.

For example, we might use a *controlled type* in the implementation of a
tagged private type. These types have procedures Initialize and Finalize
defined as primitive operations. Both are called automatically by the
compiler. Clients generally don't have any business directly calling
them so we usually use implementation inheritance with controlled types.
But if clients did have the need to call them we would use Subtype Inheritance
instead, to make them visible to clients.

For example, the following is a generic package providing an abstract data type
for unbounded queues. As such, the :ada:`Queue` type uses dynamic allocation
internally. This specific version automatically reclaims the allocated storage
when objects of the :ada:`Queue` type cease to exist:

.. code:: ada no_button project=Courses.Ada_Idioms.Inheritance_Idioms.Implementation_Inheritance_Queue

    with Ada.Finalization;
    generic
       type Element is private;
    package Unbounded_Sequential_Queues is

       type Queue is tagged limited private;

       procedure Insert (Into : in out Queue;  Item : Element) with
         Post => not Empty (Into) and
                 Extent (Into) = Extent (Into)'Old + 1;
         --  may propagate Storage_Error

       procedure Remove (From : in out Queue;  Item : out Element) with
         Pre  => not Empty (From),
         Post => Extent (From) = Natural'Max (0, Extent (From)'Old - 1);

       procedure Reset (This : in out Queue) with
          Post => Empty (This) and Extent (This) = 0;

       function Extent (This : Queue) return Natural;

       function Empty (This : Queue) return Boolean;

    private

       type Node;

       type Link is access Node;

       type Node is record
          Data : Element;
          Next : Link;
       end record;

       type Queue is new Ada.Finalization.Limited_Controlled with
          record
             Count : Natural := 0;
             Rear  : Link;
             Front : Link;
          end record;

       overriding procedure Finalize (This : in out Queue) renames Reset;

    end Unbounded_Sequential_Queues;

The basic operation of assignment usually does not make sense for an
abstraction represented as a linked list, so we declare the private type as
*limited*, in addition to tagged and private, and then use the
language-defined limited controlled type for the type extension completion
in the private part.

Procedures :ada:`Initialize` and :ada:`Finalize` are inherited immediately after
the type extension. Both are null procedures that do nothing. We can leave
:ada:`Initialize` as-is because initialization is already accomplished via the
default values for the :ada:`Queue` components. On the other hand, we want
finalization to reclaim all allocated storage so we cannot leave
:ada:`Finalize` as a null procedure. By overriding the procedure, we can change
the implementation. That change is usually accomplished by placing the
corresponding procedure body in the package body. However, in this case we have
an existing procedure named :ada:`Reset` that is part of the visible (client)
API. :ada:`Reset` does exactly what we want :ada:`Finalize` to do, so we
implement the overridden :ada:`Finalize` by saying that it is just another name
for :ada:`Reset`. No completion body for :ada:`Finalize` is then required or
allowed. This approach has the same semantics as if we explicitly wrote a
body for :ada:`Finalize` that simply called :ada:`Reset`, but this is more succinct.
Clients can call :ada:`Reset` whenever
they want, but the procedure will also be called automatically, via
:ada:`Finalize`, when any :ada:`Queue` object ceases to exist.


Pros
----

The two idioms are easily composed simply by controlling where in the enclosing
package the parent type is mentioned: either in the declaration of the private
child type in the package visible part or in the child type's completion in the
package private type.


Cons
----

Although the inheritance expressions are simple by themselves, the many
ancillary design choices can make the design effort seem more complicated than
it really is.


Relationship With Other Idioms
------------------------------

We assume the :ref:`Abstract Data Type idiom <Ada_Idioms_Abstract_Data_Types>`,
so we are using private types throughout. That includes the child type, and, as
we saw, allows us to control the compile-time visibility to the parent type.


Notes
-----



Bibliography
------------

.. [1] Meyer, B. (1997). Object-Oriented Software Construction, Prentice-Hall.
