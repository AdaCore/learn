.. _Ada_Idioms_Constructor_Functions_For_Abstract_Data_Types:

Constructor Functions For Abstract Data Types
=============================================

.. include:: ../../global.txt


Motivation
----------

In languages supporting object-oriented programming (OOP), including Ada,
*constructors* are not inherited when one type is derived from another. That's
appropriate because, in general, they would be unable to fully construct values
for the new type. The purpose of this idiom is to explain how Ada defines
constructors, how the language rules prevent constructor inheritance, and how
to design the constructor code in light of those rules.

Ada uses tagged types to fully support dynamic OOP. Therefore, in the
following, a *derived type* refers to a tagged type that is declared as a
so-called *type extension* |mdash| a form of inheritance |mdash| based on some
existing *parent* tagged type. The extension consists of additional components
and/or additional or changed operations beyond those inherited from the
existing parent type.

This discussion assumes these tagged types are declared in packages designed
using the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` (ADT) idiom.
We strongly recommend the reader be comfortable with that idiom before
proceeding.

As abstract data types, the parent type is a private type, and the derived type is
a *private extension*. A private extension is a type extension declaration
that does not reveal the components added, if any. The parent type could itself
be an extended type, but the point is that these types will all be private
types one way or another. Declarations as private types and private extensions
are not required by the language for inheritance, but as argued in the ADT
idiom discussion, doing so is recommended in the strongest terms. OOP doesn't
change that, and in fact the encapsulation and information hiding that are
characteristic of the ADT idiom are foundational principles for OOP types.

For an example of a private extension, given a tagged type named
:ada:`Graphics.Shape` one can declare a new type named :ada:`Circle` via type
extension:

.. code-block:: ada

    type Circle is new Graphics.Shape with private;

This declaration will be in the public part of a package, but, as a private
type extension, the additional components are not compile-time visible to
client code, conforming to ADT requirements. That's what the reserved word
:ada:`private` indicates in the type declaration.

Instead of a distinct constructor syntax, Ada uses regular functions to
construct objects. Specifically, so-called *constructor functions* are
functions that return an object of the type.

.. code-block:: ada

    type Circle is new Graphics.Shape with private;

    function New_Circle (Radius : Float) return Circle;

Like any function there may be formal parameters specified, but not necessarily.

Functions and procedures that manipulate objects of the private type are
*primitive operations* for the type if they are declared in the same package as
the type declaration itself. For procedures, that means they have formal
parameters of the type. For functions, that means they either have formal
parameters of the type, or return a value of the type, or both.

Declaration within the same package as the type itself provides the
compile-time visibility to the type's representation required to
implement the subprograms.

Other operations might be declared in the same package too, but if they
do not manipulate or return values of the type they are not primitive
operations for the type. (Their location in that package is somewhat
suspect and should be reviewed explicitly.)

Primitive operations, and only primitive operations, are inherited during
type derivation.

If you think in terms of Abstract Data Types all these rules make sense.

Now, here's the rub.

Constructor functions require that same compile-time visibility so the
intuitive approach will be to declare them in the same package declaration
as the type. As a result, they will be primitive operations for that type.

However, that means that the constructor functions will be inherited,
contrary to the expectation for constructors. Therefore, Ada has rules
specific to primitive constructor functions that have the effect of preventing
their inheritance.

The explanation and illustration for these rules first requires explanation of
the word *abstract*. We mentioned above that the package enclosing the
type will be designed with the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` idiom. In that idiom
*abstract* means that the type represents an abstraction. (See that section for
the details.)

The term *abstract* also has a meaning in OOP, one that is unrelated to an ADT.
In OOP, an *abstract* type is one that defines an interface but at most a partial
implementation. As such, the type can serve as the ancestor type for derived
types but cannot be used to declare objects.  An abstract type in Ada includes
the reserved word :ada:`abstract` in the declaration. For example:

.. code-block:: ada

    type Foo is abstract tagged private;

Similarly, subprograms can be abstract. These again define an interface, via
the subprogram formal parameters and result type, but are not callable units.
In Ada these too include the word :ada:`abstract` in their declarations, for
example:

.. code-block:: ada

    procedure Do_Something (This : in out Foo) is abstract;

Now we can explain how Ada prevents constructor inheritance.

Whenever a
tagged type is extended, all inherited constructor functions
automatically become abstract functions for the extended type, just as
if they were explicitly declared abstract.

However, only abstract types can legally have abstract primitive
operations. Concrete types may not, so that we can never dynamically
dispatch to a subprogram without an actual implementation.

Therefore, unless the extended child type is itself abstract, the type extension
will be illegal. The compiler will reject the declaration of the child type,
thus preventing this inappropriate constructor inheritance.

For an example, both to illustrate the code and the Ada rules, consider this simple
package declaration that presents the tagged private type
:ada:`Graphics.Shape`:

.. code-block:: ada

    package Graphics is
       type Shape is tagged private;
       function Make (X, Y : Float) return Shape;
       ...
    private
       type Shape is tagged record
          X : Float := 0.0;
          Y : Float := 0.0;
       end record;
    end Graphics;

Note in particular the primitive constructor function named :ada:`Make`
that constructs a value of type :ada:`Shape`.

Because type :ada:`Shape` is tagged, other types can extend it:

.. code-block:: ada

    with Graphics;
    package Geometry is
       type Circle is new Graphics.Shape with private;  -- a private extension
       --  ...
    private
       type Circle is new Graphics.Shape with record
          Radius : Float;
       end record;
    end Geometry;

Type :ada:`Circle` automatically inherits the components and primitive
operations defined by type :ada:`Shape`, including the constructor function
:ada:`Make`. No additional declarations are required in order to inherit these
operations or components. The inherited operations are now primitive operations
for the new type.

Inherited primitive operations have an unchanged formal parameter and
result-type profile, except for the controlling parameter type name, so
although :ada:`Make` now returns a :ada:`Circle` object, the function still
only has parameters for the :ada:`X` and :ada:`Y` components. Hence this
version of :ada:`Make` could not set the :ada:`Radius` component in the
returned :ada:`Circle` value to anything other than some default.

Therefore, to prevent this inherited function from being available, two Ada
rules come into play. The first rule specifies that the implicit function is
inherited as if declared explicitly abstract:

.. code-block:: ada

    function Make (X, Y : Float) return Circle is abstract;
    -- as actually inherited, implicitly

Note the reserved word :ada:`abstract` in the implicit function declaration.
This declaration doesn't actually appear in the source code because all the
inherited primitive operations are implicitly declared.

Another rule specifies that only abstract types can have abstract primitive
subprograms. Type :ada:`Circle` is not abstract in this sense, therefore the
combination of those two rules makes the :ada:`Circle` type extension illegal.
Package :ada:`Geometry` will not compile successfully.

Failing to compile is safe |mdash| it prevents clients from having a callable
function that in general cannot suffice |mdash| but requires an alternative so
that sufficient constructor functions are possible.

Therefore, a general design idiom is required for defining constructor functions
for concrete tagged Abstract Data Types.

Solution
--------

The general solution uses functions for constructing objects but prevents these
functions from being inherited. The problem is thus circumvented entirely.

To prevent their being inherited, the solution prevents the constructor
functions from being primitive operations. However, these functions require
compile-time visibility to the parent type's representation in order to
construct values of the type, as this typically involves assigning values to
components in the return object. The alternative approach must supply the
compile-time visibility that primitive operations have.

Therefore, the specific solution is to declare constructor functions in a
separate package that is a *child* of the package declaring the tagged type.
This takes advantage of the *hierarchical library units* capability introduced in Ada 95.

Operations declared in a child package are not primitive operations for the
type in the parent package, so they are not inherited when that type is
extended. Consequently they do not become abstract.

In addition, the required visibility to the parent type's representation in the
private part will be available to the functions' implementations because the
private part and body of a child package have compile-time visibility to the
parent package's private part.

In this idiom, any package declaring a tagged type, either directly or by type extension,
will have a *constructors* child package if constructors are required. For
example:

.. code-block:: ada

    package Graphics.Constructors is
       function Make (X, Y : Float) return Shape;
    end Graphics.Constructors;

and similarly, for :ada:`Circle`:

.. code-block:: ada

    package Geometry.Constructors is
       function Make (X, Y, R : Float) return Circle;
    end Geometry.Constructors;

Each of these two package declarations will have a package body containing the
body of the corresponding function. In fact such packages can declare as many
constructor functions as required, overloaded or not.

Clients that want to use a constructor function will specify the constructor
package in the context clauses for their units, as usual. The constructor
package body for an extended type might very well do so itself, as shown below:

.. code-block:: ada

    with Graphics.Constructors; use Graphics.Constructors;
    package body Geometry.Constructors is
       function Make (X, Y, R : Float) return Circle is
         (Circle'(Make (X, Y) with Radius => R));
    end Geometry.Constructors;

Of course, the name "Constructors" is not required for the child packages. It could
be "Ctors", for example (a name common in C++), or something else. But whatever the
choice, regularity enhances comprehension so the same child package name should
be used throughout.

Pros
----

The issue is sidestepped entirely, and as an additional benefit, the
parent packages are that much simpler because the constructor function
declarations and bodies are no longer present there. The *constructors*
child packages themselves will be relatively simple since they contain only the
constructor functions and any ancillary code required to implement them.
Simpler code enhances comprehension and correctness.

Having the constructors declared in separate packages applies the principle of
Separation of Concerns, between the code defining the type's semantics and the
code for constructing objects of the type. This principle also enhances
comprehension.

Cons
----

There will be a child package for each tagged type that requires constructors,
hence more packages and files (assuming one unit per file, which is desirable in
itself, even if not required by the language).

Some developers might argue for having fewer files, presumably containing larger
units. In the author's experience larger units make comprehension, and therefore
correctness, unjustifiably difficult if smaller units are possible. Some units are
unavoidably large and complicated but often we can achieve relative simplicity.

For those developers, however, the constructor package could be declared instead
as a nested package located within the package defining the tagged type. Doing so
would achieve the same effect as using a child package because the contained
functions would not be primitive. Therefore, they would not inherited.

This alternative would reduce the number of files back to the minimum. However,
the defining package would be relatively more complicated because of this nested
package. Note that the nested package declaration would require a nested package
body too.

In short, the alternative reduces the number of files at the cost of additional
unit complexity. (If the issue with the larger number of files is difficulty in
locating individual entities of interest, any decent IDE will make doing so
trivial.)

The alternative also loses the distinction between clients that use objects of
the type and clients that create those objects, because, with the child package
approach, the latter will be the only clients that have
context clauses for the constructor packages.


Relationship With Other Idioms
------------------------------

N/A


Notes
-----

For those interested, in this section we provide a discussion of alternatives
to the solution given, and why they are inadequate.

Changing the behavior of an inherited operation requires an explicit conforming
subprogram declaration and therefore a new subprogram body for that operation.
This change is known as *overriding* the inherited operation.

Package :ada:`Geometry` could declare a function with the additional parameters
required to fully construct a value of the new type. In this case the new
constructor would include the :ada:`Radius` parameter:

.. code-block:: ada

    function Make (X, Y, Radius : Float) return Circle;

But such a function would not be overriding for the inherited version because
the parameter and result type profile would be different. This function
:ada:`Make` would overload the inherited function, not override it. The
inherited function remains visible, as-is.

In fact, we could even have the compiler confirm that this is not an overriding
function by declaring it so:

.. code-block:: ada

    not overriding function Make (X, Y, Radius : Float) return Circle;

In general, specifying that a subprogram is not overriding is less convenient
than specifying that it is overriding. We only do so in these examples to make
everything explicit.

Because that new function is not overriding, the inherited version remains
implicitly abstract and the type extension remains illegal. Developers could
also override the inherited function, which would make the code legal, but as
we have said such a function cannot properly construct values in general, and
might be called accidentally. For example:

.. code-block:: ada

    with Graphics;
    package Geometry is
       type Circle is new Graphics.Shape with private;

       overriding function Make (X, Y : Float) return Circle;

       not overriding function Make (X, Y, Radius : Float) return Circle;
       -- overloading

       ...
    private
       --  ...
    end Geometry;

Although the overridden :ada:`Make` does not have a :ada:`Radius` parameter and
could only assign some default to that component, if that default is reasonable
then the overridden function could be called on purpose, i.e., not
accidentally. That's not a general solution, however.

Alternatively, developers could use procedures as their constructors, with a
mode-out parameter for the result. The procedure would not become implicitly
abstract in type extensions, unlike a function.

.. code-block:: ada

    package Graphics is
       type Shape is tagged private;
       procedure Make (Value : out Shape;  X, Y : in Float);
    private
       --  ...
    end Graphics;

And then the client extension would inherit the procedure:

.. code-block:: ada

    with Graphics;
    package Geometry is
       type Circle is new Graphics.Shape with private;
       --  procedure Make (Value : out Circle;  X, Y : in Float);  -- inherited
    private
       --  ...
    end Geometry;

However, although now legal, the inherited procedure would not suffice, lacking
the required parameter for the :ada:`Radius` component.

Developers might then add an overloaded version with the additional parameter:

.. code-block:: ada

    with Graphics;
    package Geometry is
       type Circle is new Graphics.Shape with private;

       --  procedure Make (Value : out Circle;  X, Y : in Float);
       -- inherited

       not overriding procedure Make (Value : out Circle;  X, Y, R : in Float);
       -- not inherited
    private
       --  ...
    end Geometry;

But the same issues arise as with functions. Clients might accidentally call
the wrong procedure, i.e., the inherited routine that doesn't have a parameter
for the :ada:`Radius`.  That routine would not even mention the :ada:`Radius`
component, much less assign a default value, so it would have to be overridden
in order to do so. This too is not a general solution.
