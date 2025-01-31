.. _Ada_In_Practice_Abstract_Data_Types:

Abstract Data Types
===================

.. include:: ../../global.txt

Motivation
----------

In the
:ref:`Groups of Related Program Units <Ada_In_Practice_Groups_Of_Related_Program_Units>` idiom,
client compile-time visibility to the type's representation is both an
advantage and a disadvantage. Visibility to the representation makes available
the expressiveness of low-level syntax, such as array indexing and aggregates,
but in so doing allows client source code to be dependent on the
representation. In many cases, the resulting economic and
engineering disadvantages of visibility on the representation will
outweigh the expressiveness advantages.

For the sake of illustration, let's create a *stack* type that can contain
values of type :ada:`Integer`. (We use type :ada:`Integer` purely for the sake
of convenience.) Let's also say that any given :ada:`Stack` object can contain
at most a fixed number of values, and arbitrarily pick 100 for that upper
bound. The likely representation for the :ada:`Stack` type will require both
an array for the contained values and a *stack pointer* indicating the *top* of
the stack. Hence this will be a composite type, probably a record type. If we
use the
:ref:`Groups of Related Program Units <Ada_In_Practice_Groups_Of_Related_Program_Units>`
idiom the code might look like this:

.. code-block:: ada

    package Integer_Stacks is
       Capacity : constant := 100;
       type Content is array  (1 .. Capacity) of Integer;
       type Stack is record
          Values : Content;
          Top    : Integer range 0 .. Capacity := 0;
       end record;
       procedure Push  (This : in out Stack; Item : in Integer);
       procedure Pop  (This : in out Stack;  Item : out Integer);
       function Empty (This : Stack) return Boolean;
    end Integer_Stacks;

With this design the compiler will allow client code to directly read and
update the two components within any :ada:`Stack` object. For example, given
some :ada:`Stack` variable named :ada:`X`, the client can read the value of
:ada:`X.Top`, say to determine if :ada:`X` is empty. But by the same token,
the client code could change :ada:`X.Top` to some arbitrary value unrelated to
the logical top of the stack, completely violating stack semantics.

As a result, where would one look in the source code to find a bug in the
handling of some :ada:`Stack` object? It could be literally anywhere in all the
client code that uses package :ada:`Integer_Stacks`.

Similarly, changes to the internal representation of a type may become
necessary as new requirements are identified. At best, the client code will
now fail to compile, making identification of the problem areas simple. At
worst, the client code will remain legal but no longer functional. Perhaps an
additional component was added that the original components now rely upon, or
the original components are used in new ways. Conceivably every client use of
:ada:`Integer_Stacks` might need to be changed. Once we find them all we'll
have to rewrite them to address the changes in the representation. That's
potentially very expensive, perhaps prohibitively so. Worse, our *fixes* will
likely introduce new bugs.

These disadvantages argue for an alternative. That is the purpose of this next
idiom, known as the Abstract Data Type (ADT)
:footcite:p:`1983:booch`, :footcite:p:`1974:liskov`.


Implementation(s)
-----------------

Abstraction is one of the central principles of software engineering because
it is one of the primary ways that humans manage complexity. The idea is to
focus on the essentials, in effect the *what*, while ignoring all the
inessential implementation details, i.e., the *how*. For example, when we
drive a car and want to stop, we press the brake pedal. We don't also think
about how the pedal makes the car stop, just that it does so. That's an
example of abstraction. In the same way, we know that pressing the accelerator
pedal increases the speed of the car, that rotating the steering wheel changes
the direction of travel, and so on. If to control the car we had to think about
how each part actually works |mdash| the brake cylinder and brake pads, the
fuel injectors, the spark plugs, the steering shaft, the tie rods, and
everything else |mdash| we'd certainly crash.

We use abstraction in programming for the same reason. In higher-level
languages, an array is an abstraction for the combination of a base address and
offset. A file system is composed of a number of layered abstractions,
including files (at the top), then tracks, then sectors, then blocks, and ultimately
down to individual bytes. A data structure, such as a stack, a queue, or a
linked list, is an example of an abstraction, as is a valve, an air-lock, and
an engine when represented in software. Even procedures and functions are
abstractions for lower-level operations. Decomposing via abstractions allows us
to manage complexity because at any given layer we can focus on *what* is being
done, rather than how.

Therefore, an abstract data type is a type that is abstract in the sense that
:footcite:p:`1974:liskov`:

    - It is a higher level of abstraction than the built-in programming
      language types.

    - It is functionally characterized entirely by the operations defined by
      the ADT itself, along with the common basic operations such as
      assignment, object declarations, parameter passing, and so on. In
      particular, clients are not allowed to perform operations that are
      determined by the type's internal representation. Ideally, this
      protection is enforced by tools.

The ADT may also be abstract in the sense of object-oriented programming but
that is an unrelated issue.

In Ada we use *private types* to define abstract data types because private
types make the type's name, but not its representation, visible to clients.
These types are composed using syntactical building blocks: a package
declaration, separated into two parts, containing a type declared in two parts,
and containing declarations for subprograms to manipulate objects of the type
via parameters. The compiler uses the building-blocks' compile-time visibility
rules to enforce the protections against representation-based operations. (We
assume the reader is familiar with private types, but this is such an
important, central facility in Ada that we will explain them in some detail
anyway.)

Therefore, an ADT package declaration may contain any of the following:

    - Constants (but probably not variables)
    - A private type
    - Ancillary Types
    - Exceptions
    - Operations

If possible, you should declare at most one private type per ADT package.
This keeps things simple and follows the "cohesive" principle.  (Note that
the *limited-with* construct directly
facilitates declaring mutually-dependent private types that are each declared
in their own dedicated packages). However, it's not unreasonable to
declare more than one
private type in the same package, especially if one of the types is clearly
the primary type and the other private type is related to the first. For
example, in defining an ADT for a maze, we could declare a private type named
:ada:`Maze` to be the primary abstraction. But mazes have positions within
them, and as clients have no business knowing how positions are represented,
both :ada:`Maze` and :ada:`Position` could reasonably be declared as private
types in the same package.

You may use any form of private type with this idiom: basic private types,
tagged/abstract/limited private types, private type extensions, and so forth.
What's important is that the representation occurs in the private part so that
it's not compile-time visible to clients.

The abstraction's operations consist of subprograms that each have one or more
formal parameters of the type. Clients will declare objects of the type and pass
these objects as formal parameters to manipulate those objects.

The operations are known as *primitive operations* because they have the
compile-time visibility to the private type's representation necessary to
implement the required behavior.

Clients can create their own operations by calling the type's primitive
operations, but client's can't compile any operation that manipulates the
internal representation.

Consider the following revision to the package :ada:`Integer_Stacks`, now as
an ADT:

.. _Ada_In_Practice_Abstract_Data_Types_Code_Example:

.. code-block:: ada

    package Integer_Stacks is
       type Stack is private;
       procedure Push (This : in out Stack; Item : in Integer);
       procedure Pop (This : in out Stack;  Item : out Integer);
       function Empty (This : Stack) return Boolean;
       Capacity : constant := 100;
    private
       type Content is array  (1 .. Capacity) of Integer;
       type Stack is record
          Values : Content;
          Top    : Integer range 0 .. Capacity := 0;
       end record;
    end Integer_Stacks;

The package declaration now includes the :ada:`private` reserved word, about
half-way down by itself in the example above, thus dividing the package
declaration into the *public part* and the *private part*. The compiler only
allows clients compile-time visibility to the package public part. No client
code that references anything in the private part will compile successfully.

The declaration for the type :ada:`Stack` now has two pieces, one in the
package visible part and one in the package private part. The visible piece
introduces the type name and ends with the keyword :ada:`private` to
indicate that its representation is not provided to clients.

Client code can use the type name to declare objects because the name is
visible. Likewise, clients can declare their own subprograms with parameters
of type :ada:`Stack`, or use type :ada:`Stack` as the component type in a
composite type declaration. Clients can use a private type in any way that's
consistent with the rest of the visible type declaration, except they can't see
anything representation-dependent.

The full type definition is in the package private part. Therefore, for
any given object of the type, the representation details |mdash| the two
record components in this example |mdash| can't be referenced in client code.
Clients must instead only use the operations defined by the package, passing
the client objects as the actual parameters. Only the bodies of these operations
have compile-time visibility to the representation of the :ada:`Stack`
parameters, so only they can implement the functionality for those parameters.

Because package-defined subprograms are the only code that can access the
internals of objects of the type, the designer's intended abstract operations
are strictly enforced. They are the only manipulations that a client can
perform. As we
mentioned, basic operations such as assignment are allowed, unless the ADT is
*limited* as well as private, but these basic operations do not violate the
abstraction.

You may, of course, also require other ancillary type declarations in the
package, either for the implementation or as types for additional parameters for the
visible operations. The array type :ada:`Content` is an example of the
former case. When it is strictly an implementation artifact, as in this
case, it should be in the private part so that it's hidden from clients.

The ADT idiom extends the information hiding applied by the
:ref:`Groups of Related Program Units <Ada_In_Practice_Groups_Of_Related_Program_Units>`
idiom to include the type's representation.

The compile-time lack of visibility to the representation means that clients no
longer have a way to construct ADT values from the constituent parts. For
example, record aggregates are no longer possible for clients using the
:ada:`Stack` ADT. Likewise, clients no longer have a way to read the
individual constituent components. (Whether doing so is appropriate will be
addressed below.) Therefore, an ADT package may include *constructor* and
*selector/accessor* subprograms. (The term *constructor* is only conceptually
related to the same term in some other languages, such as C++.)

For an example of an abstraction that includes constructors and selectors,
imagine there is no language-defined :ada:`Complex` number type. We could use
the following ADT approach:

.. code-block:: ada

    package Complex_Numbers is
       type Complex_Number is private;
       --  function operating on Complex_Number, eg "+" ...
       --  constructors and selectors/accessors
       function Make (Real_Part, Imaginary_Part : Float) return Complex_Number;
       function Real_Part (This : Complex_Number) return Float;
       function Imaginary_Part (This : Complex_Number) return Float;
    private
       type Complex_Number is record
          Real_Part : Float;
          Imaginary_Part : Float;
       end record;
    end Complex_Numbers;

In the above, the function :ada:`Make` is a constructor that replaces the use
of aggregates for constructing :ada:`Complex_Number` values. Callers pass two
floating-point values to be assigned to the components of the resulting record
type. In the :ada:`Stack` ADT, a constructor for :ada:`Stack` objects wasn't
required because any stack has a known initial state, i.e., empty, and the
component default initialization is sufficient to achieve that state. Complex
numbers don't have any predeterminable state so the constructor is required.

Likewise, functions :ada:`Real_Part` and :ada:`Imaginary_Part` are
selector/accessor functions that return the corresponding individual component
values of an argument of type :ada:`Complex_Number`. They are
needed because the mathematical definition of complex numbers has those two
parts, so
clients can reasonably expect to be able to get such values from a given
object. (The function names need not be distinct from the component names, but
can be if desired.)

However, by default, selector/accessor functions are not included in the ADT
idiom, and especially not for every component of the representation. There are
no *getter* operations if you are familiar with that term.

There may be cases when what looks like an accessor function is provided, when
in fact the function computes the return value.  Similarly, there may be
functions that simply return the value of a component but are part of the
abstraction and happen to be implementable by returning the value of a
component. For example, a real stack's ADT package would include a function
indicating the extent of the object |mdash| that is, the number of values
currently contained. In our example implementation the :ada:`Top` component happens to
indicate that value, in addition to indicating the current top of the stack. The body
of the :ada:`Extent` function can then be as follows:

.. code-block:: ada

    function Extent (This : Stack) return Natural is (This.Top);

But a different representation might not have a :ada:`Top` component, in
which case the function would be implemented in some other way. (For example,
we could have declared a subtype of :ada:`Natural`, using :ada:`Capacity`
as the upper bound, for the function result type.)

You should not include true *getter* functions that do not meet an
abstraction-defined requirement and exist purely to provide client access
to the otherwise hidden representation components included.
Their usage makes the client code dependent
on the representation, just as if the client had direct access. For the same
reason, by default there are no *setter* procedures for the representation
components.  Both kinds of operations should be considered highly suspect.
There's no point in hiding the representation if these operations will make it
available to clients, albeit indirectly.

Pros
----

The advantages of an ADT are due to the strong interface presented, with
guaranteed enforcement by the compiler rather than by reliance on clients' good
behavior. The ADT designer can rely on client adherence to the intended
abstraction because client code that violates the designer's abstraction by
directly manipulating the internals of the type will not
compile; clients must call the designer's operations to manipulate the objects.

A package defining a strong interface will exhibit high cohesion, thereby
aiding comprehension and consequently easing both development and
maintenance.

An ADT enhances maintainability because a bug in the ADT implementation must be
in the package that defines the ADT itself. The rest of the application need
not be explored because nothing elsewhere that accessed the representation
would compile. (We ignore child packages for the time-being.) The maintenance
phase is the most expensive of the project phases for correcting errors, so
this is a significant benefit.

Although changes to the internal representation of an ADT may become necessary,
the scope of those changes is limited to the ADT package declaration and body
because legal client code cannot depend on the representation of a private
type. Consequently, changes to the type's representation can only require
recompilation (and hence relinking) of client code, but not rewriting.

A change in representation may have non-functional considerations that prompt a
change in client usage, such as performance changes, but it will not be a
matter of the legality of the client code. Illegal client usage of an ADT
wouldn't have compiled successfully in the first place.

The private type is the fundamental approach to creating abstractions in Ada,
just as the use of the *public*, *private*, and *protected* parts of classes is
fundamental to creating abstractions in class-oriented languages. Not every
type can be private, as illustrated by the client expectation for array
indexing in Ada prior to Ada 2012.
Not every type should be private, for example those that are
explicitly numeric. But the ADT should be the default design idiom when
composing a solution.

Cons
----

There is more source code text required in an ADT package compared to the idiom
in which the representation is not hidden (the
:ref:`Groups of Related Program Units <Ada_In_Practice_Groups_Of_Related_Program_Units>`).
The bulk of the additional text is due to the functions and procedures
required to provide the capabilities that the low-level representation-based
syntax might have provided, i.e., the *constructor* and *selector/accessor*
functions. We say *might have provided* because these additional operations are
by no means necessarily included. In general, the additional text required for
private types is worth the protections afforded.


Relationship With Other Idioms
------------------------------

The package-oriented idioms described here and
:ref:`previously <Ada_In_Practice_Essential_Design_Idioms_For_Packages>`
are the foundational program composition idioms because packages are the
primary structuring unit in Ada. That is especially true of the
:ref:`Abstract Data Type <Ada_In_Practice_Abstract_Data_Types>` idiom, which is the
primary type specification facility in Ada. We will
describe additional package-oriented idioms,
especially regarding hierarchical packages, but those kinds
of packages are optional.

The basic package is not optional in Ada for a
program of any significant size or complexity. (One could have a program
consisting entirely of the main program, but either that program is relatively
simple and small or it is badly structured.) As a consequence, other idioms
will exist within packages designed using one of these idioms or some other
package idiom.


Notes
-----

    1. With the package idioms that declare one or more types, especially the
       ADT idiom, the principle of Separation of Concerns dictates that objects
       of the type used by clients be declared by clients in client units, not
       in the same package that declares the type or types.

    2. The Ada Rationale document did not introduce the concept of Abstract Data
       Types. The ADT concept had already been introduced and recognized as
       effective when the first version of Ada was being designed
       :footcite:p:`1974:liskov`.
       The Ada language requirements document, *Steelman*
       :footcite:p:`1978:HOLWG`, uses the term
       "Encapsulated Definitions" and describes the information hiding to be
       provided. Steelman does not specify the implementation syntax because
       requirements documents do not include such directives. The language
       designers implemented those requirements via package private parts and
       private types.

    3. The ADT is the conceptual foundation for the *class* construct's
       visibility control in some class-oriented languages.

.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
