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

The classic way
~~~~~~~~~~~~~~~

Let's assume we have the following interface:

.. code-block:: ada

    type Animal is interface;

    procedure Eat (Beast : in out Animal) is abstract;

All types implementing the :ada:`Animal` interface have to override the
:ada:`Eat` operation:

.. code-block:: ada

    type Cat is new Animal with record ...

    procedure Eat (Beast : in out Cat);

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

.. code-block:: ada

    type Animal_Extension_1 is interface;

    procedure Eat (Beast : in out Animal_Extension_1;
                   Thing : in out A_Thing) is abstract;

So now, :ada:`Animals` that need to rely on this new way of eating will
need to be declared, such as:

.. code-block:: ada

    type Cat is new Animal and Animal_Extension_1 with record ...

Note that it's even possible to enforce the fact that an extension of
:ada:`Animal` has to be an :ada:`Animal` in the first place, by writing:

.. code-block:: ada

    type Animal_Extension_1 is interface and Animal;

which will lead to a simpler declaration for type :ada:`Cat`, as there's
no longer a need to extend from two interfaces:

.. code-block:: ada

    type Cat is new Animal_Extension_1 with record ...

The rest of the code will remain completely untouched thanks to this
change. Calls to the new subprogram will require some additional amount of
work though, as we'll first have to check that the type of an
:ada:`Animal` that we're dealing with is indeed a descendant of
:ada:`Animal_Extension_1`, and perform a conversion to that interface's
class, before calling the new version of :ada:`Eat`:

.. code-block:: ada

    The_Animal : Animal'Class := ...

    if The_Animal in Animal_Extension_1'Class then
       Animal_Extension_1'Class (The_Animal).Eat (Something);
    end if;

The Ada 2005 Way
~~~~~~~~~~~~~~~~

Ada 2005 introduces the notion of null procedures. A null procedure is a
procedure that is declared using :ada:`is null` and logically has an empty
body. Fortunately, null procedures are allowed in interface definitions
--- they define the default behavior of such a subprogram as doing
nothing. Back to the :ada:`Animal` example, the programmer can declare the
interface's :ada:`Eat` primitive as follows:

.. code-block:: ada

    procedure Eat (Beast : in out Animal;
                   Thing : in out A_Thing) is null;

All of our hundreds of kinds of animals will automatically inherit from
this procedure, but won't have to implement it. The addition of this
declaration does not break source compatibility with the contract of the
:ada:`Animal` interface. Moreover, as no new types are involved, it's a
lot easier to make calls to this subprogram --- no more need to check
membership or write a type conversion, and we can just write:

.. code-block:: ada

    The_Animal : Animal'Class := ...

    The_Animal.Eat (Something);

which will execute as a no-op except for animals that have explicitly
overridden the primitive.
