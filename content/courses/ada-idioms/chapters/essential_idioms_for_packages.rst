Essential Design Idioms for Packages
====================================

.. include:: ../../global.txt


Problem Statement
-----------------

Packages, especially library packages, are modules, and as such they are the
fundamental building blocks of Ada programs. There is no language-prescribed
way to use packages when designing an application, the language just specifies
what is legal. However, some legal approaches are more advisable than others.

In particular, packages should exhibit high cohesion and loose coupling
[1]_. Cohesion is the degree to which the declarations within a module are
related to one another, in terms of the problem being solved. In short,
unrelated entities should not be declared in the same module so that the reader
can focus on one primary concept. Coupling is the degree to which a module
depends upon other modules. Loose coupling enhances comprehension and
maintenance because it allows the developer/reader to examine and modify the
module in relative isolation.  Coupling and cohesion are interrelated, in that
higher cohesion tends to result in less coupling.


Solution
--------

Three idioms for packages were envisioned when the language was first designed.
They were introduced and described in detail by the Rationale document for the
initial language design [2]_. They were then further developed in Grady Booch's
book *Software Engineering with Ada* [3]_, a foundational work on design with
the (sequential part of the) language. Booch added a fourth idiom, the Abstract
Data Machine, to the three described by the Rationale.

These four idioms have proven themselves capable of producing packages that
exhibit high cohesion and loose coupling, resulting in more comprehensible and
more maintainable source code.

These idioms pre-date later package facilities, such as private packages and
hierarchical packages. Idioms for those packages will be described separately.

Although generic packages are not actually packages, their instantiations are
packages so these design idioms apply to generic packages as well.

Because these are idioms for modules, they are differentiated by what the
package declarations can contain. But as you will see, what they can contain is
a reflection of the degree of information hiding applied.


.. _Ada_Idioms_Named_Collection_Of_Declarations:

Essential Idiom 1: Named Collection of Declarations
---------------------------------------------------

In the first idiom the package declaration can contain other declarations only
for the following:

    - Objects (constants and variables)
    - Types
    - Exceptions

The idea is to factor out the common content required by multiple clients.
Declaring common content in one place and letting clients reference the one
unit makes the most sense.

For example, the following package declares several physical constants used in
a high-fidelity aircraft simulator. These constants are utilized throughout the
simulator code, so they are declared in one place and then referenced as
needed:

.. code-block:: ada

    package Physical_Constants is
       Polar_Radius   : constant  := 20_856_010.51; --  feet
       Equatorial_Radius : constant  := 20_926_469.20; --  feet
       Earth_Diameter : constant  :=
         2.0 * ((Polar_Radius + Equatorial_Radius)/2.0);
       Gravity  : constant  := 32.1740_4855_6430_4; --  feet/second**2
       Sea_Level_Air_Density   : constant  := 0.002378; --  slugs/foot**3
       Altitude_Of_Tropopause  : constant  := 36089.0; --  feet
       Tropopause_Temperature  : constant  := -56.5; --  degrees-C
    end Physical_Constants;

No information hiding is applied with this idiom.

Pros
~~~~

Packages designed with this idiom will have high cohesion and low coupling.

The idiom also enhances maintainability because changes to the values, if
necessary, need only be made in one place.

Cons
~~~~

When a library package contains variable declarations, these variables comprise
global data. In this sense *global* means potential visibility to multiple
clients. Global data should be avoided by default, because the effects of
changes are potentially pervasive, throughout the entire set of clients that
have visibility to it. In effect the developer must understand everything
before changing anything. The introduction of new bugs is a common result. But
if, for some compelling reason, the design really called for global data, this
idiom provides the way to declare it.


.. _Ada_Idioms_Groups_Of_Related_Program_Units:

Essential Idiom 2: Groups of Related Program Units
--------------------------------------------------

In this idiom, the package can contain all of the declarations allowed by the
first idiom, but will also contain declarations for operations, usually
subprograms but other units are also allowed, e.g., protected types/objects.
Hence:

    - Objects (constants and variables)
    - Types
    - Exceptions
    - Operations

The intent is that the types declared in the package are used by the
operations, e.g., in the formal parameters and/or function return types. In
particular, though, the types are not private types.

For example:

.. code-block:: ada

    package Linear_Algebra is
       type Vector is array (Positive range <>) of Real;
       type Matrix is array (Positive range <>, Positive range <>) of Real;
       function "+" (Left, Right : Vector) return Vector;
       function "*" (Left, Right : Vector) return Matrix;
       -- ...
    end Linear_Algebra;

In this code, :ada:`Vector` and :ada:`Matrix` are the types under
consideration. The type :ada:`Real` might be declared here too, but it might be
better declared in a
:ref:`Named Collection of Declarations <Ada_Idioms_Named_Collection_Of_Declarations>`
package referenced in a with-clause. In any case this package declares types
and subprograms that manipulate values of the types via parameters.

Variables might also be declared in the package, but not as the central purpose
of the package. Perhaps we want to have a variable whose value is used as the
default for some formal parameters. Clients can change the default for
subsequent calls by first assigning a different value to the variable, unlike a
hardcoded literal chosen by the developer. For example:

.. code-block:: ada

       Default_Debounce_Time : Time_Span := Milliseconds (75);
       --  The default amount of time used to debounce an input pin.
       --  This value is tunable.

       procedure Await_Active
         (This : Discrete_Input;
          Debounce_Time : Time_Span := Default_Debounce_Time);

With this idiom, information hiding applies to the implementations of the
visible subprograms in the package body, as well as any internal entities
declared in the body for the sake of implementing the visible subprograms.

As mentioned, these idioms apply to generic packages as well. For example, the
more realistic approach would be to make type Real be a generic formal type:

.. code-block:: ada

    generic
       type Real is digits <>;
    package Linear_Algebra is
       type Vector is array (Positive range <>) of Real;
       type Matrix is array (Positive range <>, Positive range <>) of Real;
       function "+" (Left, Right : Vector) return Vector;
       function "*" (Left, Right : Vector) return Matrix;
       --  ...
    end Linear_Algebra;

Pros
~~~~

The types and the associated operations are grouped together, hence highly
cohesive. Such packages usually can be loosely coupled as well.

Clients have all the language-defined operations available that the type
representations provide. In the case of :ada:`Vector` and :ada:`Matrix`,
clients have compile-time visibility to the fact they are array types.
Therefore, clients can manipulate :ada:`Vector` and :ada:`Matrix` values as
arrays: they can create values via aggregates, for example, and can use array
indexing to get to specific components.

Cons
~~~~

Clients can write code that depends on the type's representation, and can be
relied upon to do so. Consequently, a change in the representation will
potentially require redeveloping the client code, which could be extensive and
expensive.

However, compile-time visibility to the type representations may be necessary
to meet client expectations. For example, engineers expect to use indexing
with vectors and matrices. Ada |mdash| by design |mdash| does not allow
developers to redefine extremely low-level operations such as array indexing.
Consequently, those types must be compile-time visible to clients as array
types. We could define functions as alternatives to indexing and aggregates,
but would clients accept that relatively heavy approach?

.. _Ada_Idioms_Abstract_Data_Types:

Fundamental Idiom 3: Abstract Data Types
----------------------------------------

In the previous idiom (the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`),
client compile-time visibility to the type's representation is both an
advantage and a disadvantage. Visibility to the representation makes available
the expressiveness of low-level syntax, such as array indexing and aggregates,
but in so doing allows client source code to be dependent on the
representation. In the vast majority of cases, the resulting economic and
engineering disadvantages far outweigh the advantages.

For the sake of illustration, let's make up a *stack* type that can contain
values of type :ada:`Integer`. (We use type :ada:`Integer` purely for the sake
of convenience.) Let's also say that any given :ada:`Stack` object will contain
at most a fixed number of values, and arbitrarily pick 100 for that upper
bound. The likely representation for the :ada:`Stack` type will require both
an array for the contained values and a *stack pointer* indicating the *top* of
the stack. Hence this will be a composite type, probably a record type. If we
use the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`
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
idiom, known as the Abstract Data Type (ADT) [3]_, [4]_.

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
languages an array is an abstraction for the combination of a base address and
offset. A file system is composed of a number of layered abstractions,
including at the top files, then tracks, then sectors, then blocks, ultimately
down to individual bytes. A data structure, such as a stack, a queue, or a
linked list is an example of an abstraction, as is a valve, an air-lock, and
an engine when represented in software. Even procedures and functions are
abstractions for lower-level operations. Decomposing via abstractions allows us
to manage complexity because at any given layer we can focus on what is being
done, rather than how.

Therefore, an abstract data type is a type that is abstract in the sense that
[4]_:

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
types make the type's name visible to clients, but not the representation.
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

In general, at most one private type should be declared per ADT package, for
the sake of simplicity. Note that the *limited-with* construct directly
facilitates declaring mutually-dependent private types declared in their own
dedicated packages. However, it is not unreasonable to declare more than one
private type in the same package, especially if one of the types is clearly
the primary type and the other private type is related to the first. For
example, in defining an ADT for a maze, we could declare a private type named
:ada:`Maze` to be the primary abstraction. But mazes have positions within
them, and as clients have no business knowing how positions are represented,
both :ada:`Maze` and :ada:`Position` could reasonably be declared as private
types in the same package.

Any form of private type is allowed with this idiom: basic private types,
tagged/abstract/limited private types, private type extensions, and so forth.
What's important is that the representation occurs in the private part so that
is it not compile-time visible to clients.

The abstraction's operations will consist of subprograms that each have a
formal parameter of the type. Clients will declare objects of the type and pass
these objects to the formal parameters in order to manipulate those objects.

The operations are known as *primitive operations* because they have the
compile-time visibility to the private type's representation necessary to
implement the required behavior.

Clients can create their own operations by calling the type's primitive
operations, but client's cannot compile any operation that manipulates the
internal representation directly.

Consider the following revision to the package :ada:`Integer_Stacks`, now as
an ADT:

.. _Ada_Idioms_Abstract_Data_Types_Code_Example:

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
introduces the type name, ending with the word :ada:`private` to indicate that
the representation is not provided to clients.

Client code can use the type name to declare objects because the name is
visible. Likewise, clients can declare their own subprograms with parameters
of type :ada:`Stack`, or use type :ada:`Stack` as the component type in a
composite type declaration. Clients can use a private type in any way
consistent with the rest of the visible type declaration, except for anything
representation-dependent.

The full type definition occurs in the package private part. Therefore, for
any given object of the type, the representation details |mdash| the two
record components in this example |mdash| cannot be referenced in client code.
Clients must instead use the operations defined by the package, passing the
client objects to the formal parameters. Only the bodies of these operations
have compile-time visibility to the representation of the :ada:`Stack`
parameters, so only they can implement the functionality for those parameters.

Because the package-defined subprograms are the only code that can access the
internals of objects of the type, the designer's intended abstract operations
are strictly enforced as the only direct manipulation possible. As we
mentioned, basic operations such as assignment are allowed, unless the ADT is
limited as well as private, but these basic operations do not violate the
abstraction.

Other ancillary type declarations may of course also be required in the
package, either for the implementation or as additional parameters for the
visible operations. The array type Content is an example of such an ancillary
type. In this case it is hidden from clients because it is strictly an
implementation artifact.

The ADT idiom extends the information hiding applied by the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`
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
type. In the :ada:`Stack` ADT a constructor for :ada:`Stack` objects wasn't
required because any stack has a known initial state, i.e., empty, and the
component default initialization is sufficient to achieve that state. Complex
numbers don't have any predeterminable state, it's up to clients, so the
constructor is required.

Likewise, functions :ada:`Real_Part` and :ada:`Imaginary_Part` are
selector/accessor functions that return the corresponding individual component
values, given an argument of type :ada:`Complex_Number`. They are provided
because complex numbers have those two parts by definition in mathematics.
Clients can reasonably expect to be able to get such values from a given
object. (The function names need not be distinct from the component names but
can be if desired.)

However, by default, selector/accessor functions are not included in the ADT
idiom, and especially not for every component of the representation. There are
no *getter* operations if you are familiar with that term.

There may be cases when what looks like an accessor function is provided, when
in fact the function computes the return value.  Similarly, there may be
functions that simply return the value of a component but are part of the
abstraction and happen to be implementable by returning the value of a
component. For example, a real stacks ADT package would include a function
indicating the extent of the object |mdash| that is, the number of values
currently contained. In our example implementation the Top component happens to
indicate that value, beyond indicating the current top of the stack. The body
of the :ada:`Extent` function can then be as follows:

.. code-block:: ada

    function Extent (This : Stack) return Natural is (This.Top);

But a different representation might not have a :ada:`Top` component so the
function would be implemented in some other way. (We would have declared a
subtype of :ada:`Natural`, using :ada:`Capacity` as the upper bound, for the
function result type.)

True *getter* functions that do not meet an abstraction-defined requirement and
exist purely to provide client access to the otherwise hidden representation
components should not be included. Their usage makes the client code dependent
on the representation, just as if the client had direct access. For the same
reason, by default there are no *setter* procedures for the representation
components.  Both kinds of operations should be considered highly suspect.
There's no point in hiding the representation if these operations will make it
available to clients, albeit indirectly.

Pros
~~~~

The advantages of an ADT result from the strong interface presented, with
guaranteed enforcement by the compiler rather than by reliance on client good
behavior. The ADT designer can rely on client adherence to the intended
abstraction because client code that violates the designer's abstraction
|mdash| directly manipulating the internals of the type |mdash| will not
compile. Clients must call the designer's operations to manipulate the objects.

A package defining a strong interface will exhibit high cohesion, thereby
aiding comprehension and, consequently, both development and maintenance.

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
indexing in Ada. Not every type should be private, for example those that are
explicitly numeric. But the ADT should be the default design idiom when
decomposing a problem into a solution.

Cons
~~~~

There is more source code text required in an ADT package compared to the idiom
in which the representation is not hidden (the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`).
The bulk of the additional text is due to the functions and procedures
required to provide the capabilities that the low-level representation-based
syntax might have provided, i.e., the *constructor* and *selector/accessor*
functions. We say *might have provided* because these additional operations are
by no means necessarily included. In general, the additional text required for
private types is worth the protections afforded.


.. _Ada_Idioms_Abstract_Data_Machines:

Fundamental Idiom 4: Abstract Data Machines
-------------------------------------------

The Abstract Data Machine (ADM) is similar to the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` in that it
presents an abstraction, something that doesn't already exist in the
programming language. Furthermore, like the ADT, operations are provided to
manipulate the abstraction data, state that is not otherwise compile-time
visible to client code. These operations are thus enforced as the only
manipulation possible, as per the designer's intent.  (The Abstract Data
Machine was introduced by Booch as the Abstract State Machine, but that name
can be confused with another concept that, though somewhat related, is not the
same thing.)

Unlike the ADT, however, the ADM does not define the abstraction as a type. To
understand this point, recall that type declarations are descriptions for
objects that will contain data. For example, our earlier :ada:`Stack` type was
defined as a record containing two components: an array to hold the values
logically contained by the :ada:`Stack`, and an integer indicating the logical
top of that array. No data actually exist, i.e., are allocated storage, until
objects are declared. Clients can declare as many objects of type :ada:`Stack`
as they require, and each object has a distinct, separate copy of those two
components.

Clients can, of course, choose to declare only one object of a given type, in
which case only one instance of the data described by the type will exist. But
in that case, other than convenience there is no functional difference from
declaring objects of the component types directly, rather than indirectly via
some enclosing type. Instead of using the :ada:`Stack` type to declare a
single composite object, for example, the developer could have instead declared
two objects, one for the array and one for the stack pointer:

.. code-block:: ada

       Capacity : constant := 100;
       type Content is array  (1 .. Capacity) of Integer;
       Values : Content;
       Top    : Integer range 0 .. Capacity := 0;

or even this, using an anonymously-typed array:

.. code-block:: ada

       Capacity : constant := 100;
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;

If there is only one *stack* these two objects will suffice.

That's what the ADM does. The necessary state for a single abstraction instance
is declared in a package, usually a library package. But as an abstraction,
those data declarations must not be compile-time visible to clients. Therefore,
the state is declared in either the package private part or the package body.
Doing so requires that visible operations be made available to clients, as any
abstraction would require. Hence the package is the abstraction instance, as
opposed to one or more objects of a type.

Therefore, the package declaration's visible section will contain only the
following:

   - Constants (but almost certainly not variables)

   - Ancillary Types

   - Exceptions

   - Operations

The package declaration's private part and the package body may contain all the
above, but especially one or the other (or both) will contain object
declarations representing the abstraction's state.

Consider the following ADM version of the package :ada:`Integer_Stacks`, now
renamed to :ada:`Integer_Stack` for reasons we will discuss shortly. In this
version we declare the state in the package body.

.. _Ada_Idioms_Abstract_Data_Machines_Code_Example:

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
       procedure Pop (Item : out Integer) is ... end Pop;
       function Empty return Boolean is (Top = 0);
    end Integer_Stack;

Now there is no type presenting a :ada:`Stack` abstraction, and the operations
do not take a stack parameter because the package and its data is the
abstraction instance. There is only one stack of integers with this idiom. That
is why the name of the package is changed from :ada:`Integer_Stacks`, i.e.,
from the plural form.

As with the ADT idiom, clients of an ADM can only manipulate the encapsulated
state indirectly, via the visible operations. The difference is that the state
to be manipulated is no longer a formal parameter. For example:

.. code-block:: ada

    Integer_Stack.Push (42);

That call places the value 42 in the array :ada:`Integer_Stack.Values` located
within the package body. Compare that to the ADT approach, in which objects of
type :ada:`Stack` are manipulated:

.. code-block:: ada

    Answers : Stack;
    --  ...
    Push (Answers, 42);

That call places the value 42 in the array :ada:`Answers.Values`, i.e., within
the :ada:`Answers` variable. Clients can declare as many :ada:`Stack` objects
as they require, and each will contain a distinct copy of the state defined by
the type. In the ADM version there is only one stack and therefore one instance
of the state.

Rather than declare the abstraction state in the package body, we could just as
easily declare it in the package's private section:

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

Doing so doesn't change anything from the client code point of view. Just as
clients have no compile-time visibility to declarations in the package body,
they have no compile-time visibility to the items in the package private part.
This placement also doesn't change the fact that there is only one instance of
the data. We've only changed where the data are declared. (We will discuss the
effect of child packages separately.)

The private section wasn't required when the data were declared in the package
body. That's typical when using this idiom but is not a necessary
characteristic.

The ADM idiom applies information hiding to the internal state, similar to the
ADT idiom, except that the state is not in objects. As well, like the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`,
the implementations of the visible subprograms are hidden by the package body,
along with any non-visible entities required for their implementation.

There will be no constructor functions returning a value of the abstraction
type because there is no such type with the ADM. However, there could be one or
more initialization procedures, operating directly on the hidden state in the
package private part or package body. In the :ada:`Stack` ADM there is no need
because of the reasonable initial state, as is true with the ADT version.

The considerations regarding selectors/accessors are the same for the ADM as
for the ADT idiom, so they are not provided by default. Also like the ADT,
so-called *getters* and *setters* are highly suspect and not provided by the
idiom by default.

Pros
~~~~

In terms of abstraction and information hiding, the ADM provides the same
advantages as the ADT idiom: clients have no representation details visible and
must use the operations declared in the package to manipulate the state. The
compiler enforces this abstract view. The ADM also has the ADT benefit of
knowing where any bugs could possibly be located. If there is a bug in the
manipulation, it must be in the one package defining the abstraction itself. No
other code would have the compile-time visibility necessary.

This idiom can be applied to any situation requiring abstraction, including
hardware. For example, a particular microprocessor had an on-board rotary
switch for arbitrary use by system designers. The switch value was available to
the software via an 8-bit integer located at a dedicated memory address, mapped
like so:

.. code-block:: ada

       Switch : Unsigned_8 with
          Volatile,
          Address => System.Storage_Elements.To_Address (16#FFC0_0801#);

Reading the value of the memory-mapped :ada:`Switch` variable provided the
current switch value.

However, the memory at that address was read-only, and rightly so because the
only way to change the value was to physically rotate the switch. Writing to
that address had no effect whatsoever. Although doing so was a logical error no
indication was provided by the hardware. That silence was potentially confusing
to developers. It certainly looked like a variable, after all. Declaring it as
a constant wouldn't suffice because the user could rotate the switch during
execution.

Furthermore, although mapped as a byte, the physical switch has only 16 total
positions, read as the values zero through fifteen. An unsigned byte has no
such constraints.

A good general rule is that if something shouldn't be done by clients, we
should use the compiler to make it impossible. That's better than debugging,
any day. Therefore, we will use the ADM idiom to represent the rotary switch.
The compiler will enforce the read-only view and the operation can handle the
range constraint. The ADM is a reasonable choice because there is only one such
physical switch; a type doesn't bring any advantages in this case. The
following illustrates the approach:

.. code-block:: ada

    with Interfaces; use Interfaces;
    package Rotary_Switch is
       subtype Values is Unsigned_8 range 0 .. 15;
       function State return Values;
    end Rotary_Switch;

Clients can then call the function :ada:`Rotary_Switch.State` to get the
switch's current value, as a constrained subtype. The body will handle all the
details.

.. code-block:: ada

    with System.Storage_Elements;  use System.Storage_Elements;
    package body Rotary_Switch is
       Switch : Unsigned_8 with Volatile, Address => To_Address (16#FFC0_0801#);
       function State return Values is
       begin
          if Switch in Values then
             return Switch;
          else
             raise Program_Error;
          end if;
       end State;
    end Rotary_Switch;

The range check in the function body might be considered over-engineering
because the switch is a physical device that cannot have more than 16 values,
but physical devices have a habit of springing surprises. Note that
:ref:`attribute Valid <Adv_Ada_Valid_Attribute>` would not be useful here
because there are no invalid bit patterns for an unsigned integer. If, on the
other hand, we were working with an enumeration type, for example, then
:ada:`'Valid` would be useful.

Cons
~~~~

An ADM defines only one abstraction instance. If more than one is required, the
developer must copy-and-paste the entire package and then change the package
unit name.

Furthermore, the ADM cannot be used to compose other types, e.g., as the
component type in an array or record type. The ADM cannot be used to define the
formal parameter of a client-defined subprogram, cannot be dynamically
allocated, and so on.

But if one can know with certainty that only one thing is ever going to be
represented, as in the hardware switch example, the ADM limitations are
irrelevant. That said, certainty is usually not available |mdash| even the
hardware changes.

Relationship With Other Idioms
------------------------------

The package-oriented idioms described here are the foundational program
composition idioms because packages are the primary structuring unit in Ada.
That is especially true of the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` idiom, which is the
primary type specification facility in Ada. Additional package-oriented idioms
will be described, especially regarding hierarchical packages, but those kinds
of packages are optional. The basic package is not optional in Ada for a
program of any significant size or complexity. (One could have a program
consisting entirely of the main program, but either that program is relatively
simple and small, or it is badly structured.) As a consequence, other idioms
will exist within packages designed using one of these idioms, or some other
package idiom.

Notes
-----

    1. The rules for what these idiomatic packages contain are not meant to be
       iron-clad; hybrids are possible but should be considered initially
       suspect and reviewed accordingly.

    2. With the package idioms that declare one or more types, especially the
       ADT idiom, the principle of Separation of Concerns dictates that objects
       of the type used by clients be declared by clients in client units, not
       in the same package that declares the type or types.

    3. The Ada Rationale document did not introduce the concept of Abstract Data
       Types. The ADT concept had already been introduced and recognized as
       effective when the first version of Ada was being designed [4]_. The Ada
       language requirements document, *Steelman* [5]_, uses the term
       "Encapsulated Definitions" and describes the information hiding to be
       provided. Steelman does not specify the implementation syntax because
       requirements documents do not include such directives. The language
       designers implemented those requirements via package private parts and
       private types.

    4. The ADT is the conceptual foundation for the *class* construct's
       visibility control in some class-oriented languages.


Bibliography
------------

.. [1] Yourdon, E. and L. L. Constantine (1979). Structured Design:
       Fundamentals of a Discipline of Computer Program and System Design,
       Prentice-Hall.

.. [2] Ichbiah, J., J. Barnes, et al. (1986). Rationale for the Design of the
       Ada Programming Language.

.. [3] Booch, G. (1983). Software Engineering with Ada, Benjamin/Cummings
       Publishing Company.

.. [4] Liskov, B. and S. Zilles (1974). Programming with Abstract Data Types.
       ACM SIGPLAN symposium on Very high level languages.

.. [5] HOLWG (1978). Department of Defense Requirements for High Order Computer
       Programming Language "STEELMAN".
