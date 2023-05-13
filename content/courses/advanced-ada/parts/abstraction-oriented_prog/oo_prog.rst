Object-Oriented Programming
===========================

.. include:: ../../../global.txt

Primitives
----------

.. todo::

    Complete section!


Overriding indicators
---------------------

.. admonition:: Relevant topics

    - **Briefly** discuss :ada:`overriding` and :ada:`not overriding` mentioned
      in
      :arm22:`Overriding Indicators <8-3-1>`
    - Mention that :ada:`not overriding` is not recommended.

.. todo::

    Complete section!

.. _Adv_Ada_Abstract_Types_And_Subprograms:

Abstract types and subprograms
------------------------------

.. todo::

    Complete section!


Interfaces
----------

.. todo::

    Complete section!

.. _Adv_Ada_Null_Records_Vs_Interfaces:

Null records vs. interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~

:ref:`Earlier on <Adv_Ada_Null_Records>` in the course, we discussed how to use null
records to create a prototype. We could also consider using interfaces instead.
However, as we've just learned, the consequences are that:

- we can only create an API for the package specification, but we cannot use
  that interface type in an application in the same way as we do with null
  records;

- we're forced to use object-oriented programming |mdash| which, depending on
  our goal, might be more complex than actually needed.

Let's revisit a previous example from the section on null records:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Device

    package Devices is

       type Device is private;

       function Create
         (Active : Boolean)
          return Device;

       procedure Reset
         (D : out Device) is null;

       procedure Process
         (D : in out Device) is null;

       procedure Activate
         (D : in out Device) is null;

       procedure Deactivate
         (D : in out Device) is null;

    private

       type Device is null record;

       function Create
         (Active : Boolean)
          return Device is (null record);

    end Devices;

We can easily rewrite this specification using interfaces:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Device_Interface

    package Devices is

       type Device is interface;

       function Create
         (Active : Boolean)
          return Device is abstract;

       procedure Reset
         (D : out Device) is null;

       procedure Process
         (D : in out Device) is null;

       procedure Activate
         (D : in out Device) is null;

       procedure Deactivate
         (D : in out Device) is null;

    end Devices;

These are the only changes we made:

- :ada:`Device` is now an interface, and

- :ada:`Create` is now an abstract function.

Keep in mind, however, that a null record isn't an abstract type, even though
it *looks* abstract (as it doesn't store any information). This contrasts
with interfaces, which are abstract and therefore more restricted. For example,
as indicated above, we cannot use the interface from the :ada:`Devices` package
in an application, as we cannot declare objects of an abstract type. The
following application |mdash| which works fine when :ada:`Device` is a null
record |mdash| doesn't compile when :ada:`Device` is an interface:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Device_Interface
    :class: ada-expect-compile-error

    with Devices; use Devices;

    procedure Show_Device is
       A : Device;
    begin
       A := Create (Active => True);
       Process (A);
       Deactivate (A);
       Activate (A);
       Reset (A);
    end Show_Device;

A possible compromise is, of course, to reintroduce null records in our
specification as a derived type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Device_Interface

    package Devices is

       type Abstract_Device is interface;

       function Create
         (Active : Boolean)
          return Abstract_Device is abstract;

       procedure Reset
         (D : out Abstract_Device) is null;

       procedure Process
         (D : in out Abstract_Device) is null;

       procedure Activate
         (D : in out Abstract_Device) is null;

       procedure Deactivate
         (D : in out Abstract_Device) is null;

       type Device is new
         Abstract_Device with private;

    private

       type Device is new
         Abstract_Device with null record;

       function Create
         (Active : Boolean)
          return Device is (null record);

    end Devices;

    with Devices; use Devices;

    procedure Show_Device is
       A : Device;
    begin
       A := Create (Active => True);
       Process (A);
       Deactivate (A);
       Activate (A);
       Reset (A);
    end Show_Device;

Now, our interface was renamed to :ada:`Abstract_Device` and we're just using
it to specify our API. We derive the :ada:`Device` from this interface type
(:ada:`Abstract_Device`) as a null record. In a prototype |mdash| such as the
:ada:`Show_Device` procedure |mdash| we can use the null record as expected.

Example: Extending Interfaces
-----------------------------

.. note::

    This section was originally written by Quentin Ochem and published as
    `Gem #48: Extending Interfaces in Ada 2005 <https://www.adacore.com/gems/gem-48>`_.

Using new interfaces
~~~~~~~~~~~~~~~~~~~~

Let's assume we have the following interface:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

    package Animals is

       type Animal is interface;

       procedure Eat (Beast : in out Animal)
         is abstract;

    end Animals;

All types implementing the :ada:`Animal` interface have to override the
:ada:`Eat` operation:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

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

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Extending_Interfaces

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

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

    package Animals.Extensions is

       type Animal_Extension_1 is interface;

       type A_Thing is null record;
       --  no implementation yet

       procedure Eat
         (Beast : in out Animal_Extension_1;
          Thing : in out A_Thing) is abstract;

    end Animals.Extensions;

So now, :ada:`Animals` that need to rely on this new way of eating will
need to be declared, such as:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

    with Animals.Extensions; use Animals.Extensions;

    package Animals.Cats is

       type Cat is new
         Animal and Animal_Extension_1
           with null record;

       procedure Eat
         (Beast : in out Cat);

       procedure Eat
         (Beast : in out Cat;
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

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

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

    type Cat is new
      Animal_Extension_1 with null record;

The rest of the code will remain completely untouched thanks to this
change. Calls to the new subprogram will require some additional amount of
work though, as we'll first have to check that the type of an
:ada:`Animal` that we're dealing with is indeed a descendant of
:ada:`Animal_Extension_1`, and perform a conversion to that interface's
class, before calling the new version of :ada:`Eat`:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Extending_Interfaces

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

Using null procedures
~~~~~~~~~~~~~~~~~~~~~

Since Ada 2005, we have the notion of null procedures. As
:ref:`discussed previously <Adv_Ada_Null_Procedures>`, a null procedure is
a procedure that is declared using :ada:`is null` and logically has an
empty body. Fortunately, null procedures are allowed in interface
definitions |mdash| they define the default behavior of such a subprogram as
doing nothing. Back to the :ada:`Animal` example, the programmer can
declare the interface's :ada:`Eat` primitive as follows:

.. code-block:: ada

    procedure Eat (Beast : in out Animal;
                   Thing : in out A_Thing) is null;

This is adapted code:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Null_Procedures

    package Animals is

       type Animal is interface;

       type A_Thing is null record;
       --  no implementation yet

       procedure Eat
         (Beast : in out Animal) is abstract;

       procedure Eat
         (Beast : in out Animal;
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

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Interfaces.Null_Procedures

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

.. note::

    This section was originally written by Emmanuel Briot and published as blog
    post
    `Calling inherited subprograms in Ada <https://blog.adacore.com/calling-inherited-subprograms-in-ada>`_.

In object-oriented code, it is often the case that we need to call
inherited subprograms. Some programing languages make it very easy by
introducing a new keyword `super` (although this approach has its limits
for languages that allow multiple inheritance of implementation).

In Ada, things are slightly more complicated. Let's take an example, using
the traditional geometric classes that are often found in text books:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms

    package Geometric_Forms is

       type Polygon is tagged private;
       procedure Initialize (Self : in out Polygon);

       type Square is new Polygon with private;

       overriding
       procedure Initialize (Self : in out Square);

    private

       type Polygon is tagged null record;
       type Square is new
         Polygon with null record;

    end Geometric_Forms;

Let's assume now that :ada:`Square`\ 's :ada:`Initialize` needs to call
:ada:`Polygon`\ 's :ada:`Initialize`, in addition to doing a number of
square specific setups. To do this, we need to use type conversions to
change the view of :ada:`Self`, so that the compiler statically knows
which :ada:`Initialize` to call. The code thus looks like:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms

    package body Geometric_Forms is

       procedure Initialize (Self : in out Polygon) is
       begin
          null;
       end Initialize;

       overriding
       procedure Initialize (Self : in out Square) is
       begin
          Initialize (Polygon (Self));
          --  ^^^^^^^^^^^^^^^^^^^^^^^^
          --  calling inherited procedure

          --  ... square-specific setups
       end Initialize;

    end Geometric_Forms;

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms

    with Geometric_Forms; use Geometric_Forms;

    procedure Show_Geometric_Forms is
       S : Square;
    begin
       S.Initialize;
    end Show_Geometric_Forms;

The main issue with this code (apart from its relative lack of
readability) is the need to hard-code the name of the ancestor class. If
we suddenly realize that a :ada:`Square` is after all a special case of a
:ada:`Rectangle`, and thus decide to add the new rectangle class, the code
needs to be changed (and not just in the spec), as in:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms

    package Geometric_Forms is

       type Polygon is tagged private;
       procedure Initialize (Self : in out Polygon);

       type Rectangle is new
         Polygon with private;       --  NEW

       overriding
       procedure Initialize
         (Self : in out Rectangle);  --  NEW

       type Square is new
         Rectangle with private;     --  MODIFIED

       overriding
       procedure Initialize
         (Self : in out Square);

    private

       type Polygon is tagged null record;
       type Rectangle is new
         Polygon with null record;
       type Square is new
         Rectangle with null record;

    end Geometric_Forms;

    package body Geometric_Forms is

       procedure Initialize (Self : in out Polygon) is
       begin
          null;
       end Initialize;

       overriding
       procedure Initialize
         (Self : in out Rectangle)
       is
       begin
          Initialize (Polygon (Self));
          --  ^^^^^^^^^^^^^^^^^^^^^^^^
          --  calling inherited procedure

          --  ... rectangle-specific setups
       end Initialize;

       procedure Initialize (Self : in out Square) is
       begin
          Initialize (Rectangle (Self));  --  MODIFIED
          --  ... square-specific setups
       end Initialize;

    end Geometric_Forms;

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms

    with Geometric_Forms; use Geometric_Forms;

    procedure Show_Geometric_Forms is
       S : Square;
    begin
       S.Initialize;
    end Show_Geometric_Forms;

The last change --- in the implementation of the :ada:`Initialize`
procedure of the :ada:`Square` type --- is easy to forget when one
modifies the inheritance tree, and its omission would result in not
initializing the :ada:`Rectangle` specific data.

Let's look into how the code should best be organized to limit the risks
here. An interesting idiom is the one that makes use of parent subtypes.
The trick is to always define a :ada:`Parent` subtype every time one
extends a type, and use that subtype when calling the inherited procedure.
Here is a full example:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Calling_Inherited_Subprograms.Geometric_Forms_2

    package Geo_Forms with Pure is

    end Geo_Forms;

    package Geo_Forms.Polygons is

       type Polygon is tagged private;
       procedure Initialize (Self : in out Polygon);

    private

       type Polygon is tagged null record;

    end Geo_Forms.Polygons;

    with Geo_Forms.Polygons;

    package Geo_Forms.Rectangles is

       subtype Parent is
         Geo_Forms.Polygons.Polygon;
       type Rectangle is new
         Parent with private;

       overriding
       procedure Initialize (Self : in out Rectangle);

    private

       type Rectangle is new Parent with null record;

    end Geo_Forms.Rectangles;

    with Geo_Forms.Rectangles;

    package Geo_Forms.Squares is

       subtype Parent is
         Geo_Forms.Rectangles.Rectangle;

       type Square is new Parent with private;

       overriding
       procedure Initialize (Self : in out Square);

    private

       type Square is new Parent with null record;

    end Geo_Forms.Squares;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Geo_Forms.Polygons is

       procedure Initialize (Self : in out Polygon) is
       begin
          Put_Line ("Initializing Polygon type...");
       end Initialize;

    end Geo_Forms.Polygons;

    with Ada.Text_IO;        use Ada.Text_IO;
    with Geo_Forms.Polygons; use Geo_Forms.Polygons;

    package body Geo_Forms.Rectangles is

       overriding
       procedure Initialize (Self : in out Rectangle)
       is
       begin
          Initialize (Parent (Self));

          --  ... rectangle-specific setups
          Put_Line ("Initializing Rectangle type...");
       end Initialize;

    end Geo_Forms.Rectangles;

    with Ada.Text_IO; use Ada.Text_IO;

    with Geo_Forms.Rectangles;
    use  Geo_Forms.Rectangles;

    package body Geo_Forms.Squares is

       procedure Initialize (Self : in out Square) is
       begin
          Initialize (Parent (Self));

          --  ... square-specific setups
          Put_Line ("Initializing Square type...");
       end Initialize;

    end Geo_Forms.Squares;

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.OO_Prog.Call_Inherited_Subprograms_2

    with Ada.Text_IO;       use Ada.Text_IO;
    with Geo_Forms.Squares; use Geo_Forms.Squares;

    procedure Show_Geo_Forms is
       S : Square;
    begin
       Put_Line ("Initialize Square object:");

       S.Initialize;
    end Show_Geo_Forms;

Now, if we want to add an extra :ada:`Parallelogram` class between
:ada:`Polygon` and :ada:`Rectangle`, we just need to change the definition
of the :ada:`Parent` subtype in the :ada:`Rectangles` package, and no
change is needed for the body.

This is not a new syntax nor a new idiom, but is worth considering it when
one is developing a complex hierarchy of types, or at least a hierarchy
that is likely to change regularly in the future.


Dynamic Polymorphism
--------------------

.. todo::

    Complete section!


Controlled types
----------------

.. todo::

    Complete section!


``Ada.Tags`` package
--------------------

.. todo::

    Complete section!

..
    REMOVED! TO BE RE-EVALUATED IN 2023:

    References
    ----------

    .. note::

        This section was originally written by Maxim Reznik and Nicolas Setton, and
        published as blog post
        `Using reference types to handle persistent objects <http://blog.adacore.com/use-ref-types-to-persistent-object>`_.


User-defined indexing
---------------------

.. admonition:: Relevant topics

   - :arm22:`User-Defined Indexing <4-1-6>`

.. todo::

    Complete section!
