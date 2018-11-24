:orphan:

:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Object-Oriented Programming
===========================

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Extending Interfaces
--------------------

Using new interfaces
~~~~~~~~~~~~~~~~~~~~

Let's assume we have the following interface:

.. code:: ada

    package Animals is

       type Animal is interface;

       procedure Eat (Beast : in out Animal) is abstract;

    end Animals;

All types implementing the :ada:`Animal` interface have to override the
:ada:`Eat` operation:

.. code:: ada

    package Animals.Cats is

       type Cat is new Animal with null record;

       procedure Eat (Beast : in out Cat);

    end Animals.Cats;

    package body Animals.Cats is

       procedure Eat (Beast : in out Cat) is
       begin
          --  no implementation yet
          null;
       end Eat;

    end Animals.Cats;

.. code:: ada run_button

    with Animals.Cats; use Animals.Cats;

    procedure Show_Cat is
       C : Cat;
    begin
       C.Eat;
    end Show_Cat;

Now, after a while, the developer of :ada:`Animal` might feel the need to
let animals eat something specific, and would like to add the following
operation to the interface:

.. code-block:: ada

    procedure Eat (Beast : in out Animal;
                   Thing : in out A_Thing);

Unfortunately, there are hundreds of species of animals implementing this
interface, and having to migrate everything will be too painful. Not to
mention that most of them don't even need this new way of eating ---
they're just happy eating some random amount of anonymous food. Extending
this interface is just not the way to go --- so the extension has to be
done separately, in a new interface, such as:

.. code:: ada

    package Animals.Extensions is

       type Animal_Extension_1 is interface;

       type A_Thing is null record;
       --  no implementation yet

       procedure Eat (Beast : in out Animal_Extension_1;
                      Thing : in out A_Thing) is abstract;

    end Animals.Extensions;

So now, :ada:`Animals` that need to rely on this new way of eating will
need to be declared, such as:

.. code:: ada

    with Animals.Extensions; use Animals.Extensions;

    package Animals.Cats is

       type Cat is new Animal and Animal_Extension_1 with null record;

       procedure Eat (Beast : in out Cat);

       procedure Eat (Beast : in out Cat;
                      Thing : in out A_Thing);

    end Animals.Cats;

    package body Animals.Cats is

       procedure Eat (Beast : in out Cat) is
       begin
          --  no implementation yet
          null;
       end Eat;

       procedure Eat (Beast : in out Cat;
                      Thing : in out A_Thing) is
       begin
          --  no implementation yet
          null;
       end Eat;

    end Animals.Cats;

.. code:: ada run_button

    with Animals.Cats;       use Animals.Cats;
    with Animals.Extensions; use Animals.Extensions;

    procedure Show_Cat is
       C : Cat;
       T : A_Thing;
    begin
       C.Eat (T);
    end Show_Cat;

Note that it's even possible to enforce the fact that an extension of
:ada:`Animal` has to be an :ada:`Animal` in the first place, by writing:

.. code-block:: ada

    type Animal_Extension_1 is interface and Animal;

which will lead to a simpler declaration for type :ada:`Cat`, as there's
no longer a need to extend from two interfaces:

.. code-block:: ada

    type Cat is new Animal_Extension_1 with null record;

The rest of the code will remain completely untouched thanks to this
change. Calls to the new subprogram will require some additional amount of
work though, as we'll first have to check that the type of an
:ada:`Animal` that we're dealing with is indeed a descendant of
:ada:`Animal_Extension_1`, and perform a conversion to that interface's
class, before calling the new version of :ada:`Eat`:

.. code:: ada run_button

    with Animals;            use Animals;
    with Animals.Cats;       use Animals.Cats;
    with Animals.Extensions; use Animals.Extensions;

    procedure Show_Animal_Eat is
       C : Cat;
       T : A_Thing;

       A : Animal'Class := C;
    begin
       if A in Animal_Extension_1'Class then
          Animal_Extension_1'Class (A).Eat (T);
       end if;
    end Show_Animal_Eat;

:code-config:`reset_accumulator=True`

Using null procedures
~~~~~~~~~~~~~~~~~~~~~

Since Ada 2005, we have the notion of null procedures. A null procedure is
a procedure that is declared using :ada:`is null` and logically has an
empty body. Fortunately, null procedures are allowed in interface
definitions --- they define the default behavior of such a subprogram as
doing nothing. Back to the :ada:`Animal` example, the programmer can
declare the interface's :ada:`Eat` primitive as follows:

.. code-block:: ada

    procedure Eat (Beast : in out Animal;
                   Thing : in out A_Thing) is null;

This is adapted code:

.. code:: ada

    package Animals is

       type Animal is interface;

       type A_Thing is null record;
       --  no implementation yet

       procedure Eat (Beast : in out Animal) is abstract;

       procedure Eat (Beast : in out Animal;
                      Thing : in out A_Thing) is abstract;

    end Animals;

    package Animals.Cats is

       type Cat is new Animal with null record;

       procedure Eat (Beast : in out Cat);

       procedure Eat (Beast : in out Cat;
                      Thing : in out A_Thing);

    end Animals.Cats;

    package body Animals.Cats is

       procedure Eat (Beast : in out Cat) is
       begin
          --  no implementation yet
          null;
       end Eat;

       procedure Eat (Beast : in out Cat;
                      Thing : in out A_Thing) is
       begin
          --  no implementation yet
          null;
       end Eat;

    end Animals.Cats;

All of our hundreds of kinds of animals will automatically inherit from
this procedure, but won't have to implement it. The addition of this
declaration does not break source compatibility with the contract of the
:ada:`Animal` interface. Moreover, as no new types are involved, it's a
lot easier to make calls to this subprogram --- no more need to check
membership or write a type conversion, and we can just write:

.. code:: ada run_button

    with Animals;            use Animals;
    with Animals.Cats;       use Animals.Cats;

    procedure Show_Animal_Eat is
       C : Cat;
       T : A_Thing;

       A : Animal'Class := C;
    begin
       A.Eat (T);
    end Show_Animal_Eat;

which will execute as a no-op except for animals that have explicitly
overridden the primitive.

Calling inherited subprograms
-----------------------------

In object-oriented code, it is often the case that we need to call
inherited subprograms. Some programing languages make it very easy by
introducing a new keyword `super` (although this approach has its limits
for languages that allow multiple inheritance of implementation).

In Ada, things are slightly more complicated. Let's take an example, using
the traditional geometric classes that are often found in text books:

.. code-block:: ada

    type Polygon is tagged private;
    procedure Initialize (Self : in out Polygon);

    type Square is new Polygon with private;
    overriding procedure Initialize (Self : in out Square);

Let's assume now that :ada:`Square`'s :ada:`Initialize` needs to call
:ada:`Polygon`'s :ada:`Initialize`, in addition to doing a number of
square specific setups. To do this, we need to use type conversions to
change the view of :ada:`Self`, so that the compiler statically knows
which :ada:`Initialize` to call. The code thus looks like:

.. code-block:: ada

    procedure Initialize (Self : in out Square) is
    begin
        Initialize (Polygon (Self));  --  calling inherited procedure
        ... square-specific setups
    end Initialize;

The main issue with this code (apart from its relative lack of
readability) is the need to hard-code the name of the ancestor class. If
we suddenly realize that a :ada:`Square` is after all a special case of a
:ada:`Rectangle`, and thus decide to add the new rectangle class, the code
needs to be changed (and not just in the spec), as in:

.. code-block:: ada

    type Polygon is tagged private;
    procedure Initialize (Self : in out Polygon);

    type Rectangle is new Polygon with private;   --  NEW
    overriding procedure Initialize (Self : in out Rectangle);  --  NEW

    type Square is new Rectangle with private;   --  MODIFIED
    overriding procedure Initialize (Self : in out Square);

    procedure Initialize (Self : in out Square) is
    begin
        Initialize (Rectangle (Self));  --   MODIFIED
        ... square-specific setups
    end Initialize;

The last change is easy to forget when one modifies the inheritance tree,
and its omission would result in not initializing the :ada:`Rectangle`
specific data.

Let's look into how the code should best be organized to limit the risks
here. One of the idioms that has been proposed is interesting enough that
we felt it was worth putting in this short post. The trick is to always
define a :ada:`Parent` subtype every time one extends a type, and use that
subtype when calling the inherited procedure. Here is a full example:

.. code-block:: ada

    package Polygons is
        type Polygon is tagged private;
        procedure Initialize (Self : in out Polygon);
    end Polygons;

    with Polygons;
    package Rectangles is
       subtype Parent is Polygons.Polygon;
       type Rectangle is new Parent with private;
       overriding procedure Initialize (Self : in out Rectangle);
    end Rectangles;

    with Rectangles;
    package Squares is
       subtype Parent is Rectangles.Rectangle;
       type Square is new Parent with private;
       overriding procedure Initialize (Self : in out Square);
    end Squares;

    package body Squares is
       overriding procedure Initialize (Self : in out Square) is
       begin
          Initialize (Parent (Self));
       end Initialize;
    end Squares;

Now, if we want to add an extra :ada:`Parallelogram` class between
:ada:`Polygon` and :ada:`Rectangle`, we just need to change the definition
of the :ada:`Parent` subtype in the :ada:`Rectangles` package, and no
change is needed for the body.

This is not a new syntax nor a new idiom, but is worth thinking about when
one is developing a complex hierarchy of types, or at least a hierarchy
that is likely to change regularly in the future.
