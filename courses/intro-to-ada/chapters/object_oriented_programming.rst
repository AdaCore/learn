Object oriented programming
===========================
:code-config:`reset_accumulator=True`

Object oriented programming is a large and blurrily defined concept in
programming languages that tends to encompass many different things. This is
due to the fact that many different languages implemented their own vision of
it, with similarities and differences.

However, there is a model that mostly "won" the battle of what object oriented
means, if only by sheer popularity. It's the model of the Java programming
language, which is very similar to the one of the C++ language. Here are some
defining characteristics:

- Type derivation and extension: Most object oriented languages allow the user
  to add fields to derived types.

- Subtyping: Objects of a type derived from a base type are, in some instances,
  substitutable to objects from the base type - in terms of static typing.

- Runtime polymorphism: Calling a subprogram attached to an object type, which
  is usually called a method, can dispatch at runtime depending on the exact
  type of the object.

- Encapsulation: Objects can hide some of their data.

- Extensibility: People from the "outside" - of your package, or even your
  whole library - can derive from your object types, and define their own
  behaviors.

Ada predates the popularity of object orientation, and since it aimed to be a
complete language from the start, has many mechanisms and concepts to fullfill
the above requirements.

- As we saw, encapsulation is not implemented at the type level in Ada, but at
  the package level.

- Subtyping can be implemented using, well, subtypes, which have a full and
  permissive static substitability model. The substitution will fail at runtime
  if the dynamic constraints of the subtype are not fulfilled.

- Runtime polymorphism can be implemented using variant records.

However, this lists leaves out type extension - if you don't consider variant
records - and extensibility.

In the 1995 revision of Ada, a feature filling the gaps was added, so that
people can program following the object oriented paradigm in an easier fashion,
which is called tagged types.

.. note::
    It is possible to program in Ada without ever creating tagged types. If
    that's your prefered style of programming, or you have no specific use for
    tagged types, feel free to not use them, as for every feature of Ada.

    However, they can be the best way to express solutions to certain problems.
    If that's the case, read on!

Derived types
-------------

Before going into tagged types, we should go into a topic we have brushed on,
but not really covered so far:

For every type in Ada, you can create a new type from it. Type derivation is
built-in into the language.

.. code:: ada

    package Newtypes is
       type Point is record
           X, Y : Integer;
       end record;

       type New_Point is new Point;
    end Newtypes;

It is useful to enforce strong typing, because the type system will treat the
two types as incompatible.

But it is not limited to that: You can inherit things from the type you derive
from. The representation of the data is one part, but you can also inherit
behavior.

When you inherit a type, what we call primitive operations are inherited. A
primitive is a subprogram attached to a type. Ada knows a primitive because it
is a subprogram defined in the same scope with the type.

.. attention::
    A subprogram will only become a primitive of the type if:

    1. The subprogram is declared in the same scope as the type
    2. The type and its subprograms are declared in a package

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Primitives is
      package Week is
        type Days is (Monday, Tuesday, Wednesday, Thursday,
                      Friday, Saturday, Sunday);

         --  Print day is a primitive of the type Days
        procedure Print_Day (D : Days);
      end Week;

      package body Week is
        procedure Print_Day (D : Days) is
        begin
           Put_Line (Days'Image (D));
        end Print_Day;
      end Week;

      use Week;
      type Weekend_Days is new Days range Saturday .. Sunday;

      --  A procedure Print_Day is automatically inherited here. It is like
      --  the procedure
      --
      --  procedure Print_Day (D : Weekend_Days);
      --
      --  Has been declared

      Sat : Weekend_Days := Saturday;
    begin
       Print_Day (Sat);
    end Primitives;

While this kind of inheritance can be very useful, and has the advantage of not
being limited to record types (you can use it on discrete types as in the
example above), it is only superficially similar to object oriented
inheritance:

- Records cannot be extended this way. In general, it is imposible to specify a
  new representation for the new type, it will **always** have the same
  representation as the base type.

- There is no facility for dynamic dispatch or polymorphism. Objects are of a
  fixed, static type.

The differences are more numerous, but it is not very useful to list them here.
Just remember that this is a kind of inheritance you can use if you just want
to statically inherit behavior without duplicating code or using composition,
but that you cannot use if you want any dynamic features that we usually
associate with OOP.

Tagged types
------------

In the 1995 revision of the Ada language, tagged types were introduced to
fullfil the need for an unified solution to program in an object oriented style
similar to the one described at the beginning of this section.

Tagged types are much like regular records, except that some functionalities are
added:

- Types have a tag, that is stored inside instances, and that identifies the
  `runtime type <https://en.wikipedia.org/wiki/Run-time_type_information>`_ of
  an object.

- Primitives can dispatch. A primitive on a tagged type is what you would call
  a method in Java or C++. If you derive a base type, and override a primitive
  of it, then in some instances it will be possible to call it on an object
  such that which primitive is called depends on the exact runtime type of the
  object.

- Subtyping rules are introduced, such that a tagged type derived from a base
  type is statically compatible with the base type.

Let's see our first tagged type declaration:

.. code:: ada

    package P is
       type My_Class is tagged null record;
       --  Just like a regular record, but with tagged qualifier

       --  Methods are outside of the type definition:

       procedure Foo (Self : in out My_Class);
       --  If you define a procedure taking a My_Class argument,
       --  in the same package, it will be a method.

       --  Here is how you derive a tagged type:

       type Derived is new My_Class with record
           A : Integer;
           --  You can add field in derived types.
       end record;

       overriding procedure Foo (Self : in out Derived);
       --  Overriding qualifier is optional, but if it is here,
       --  it must be valid.
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is
       procedure Foo (Self : in out My_Class) is
       begin
          Put_Line ("In My_Class.Foo");
       end Foo;

       procedure Foo (Self : in out Derived) is
       begin
          Put_Line ("In Derived.Foo, A = " & Integer'Image (Self.A));
       end Foo;
    end P;

Classwide types
---------------

In order to stay coherent with the rest of the language, a new notation needs
to be introduced to be able to say: This object is of this type or any
descendent tagged type.

In Ada, this is called the classwide type. It is used in object oriented
programming as soon as you need polymorphism. For example, you cannot do the
following:

.. code:: ada
    :class: ada-expect-compile-error

    with P; use P;

    procedure Main is

       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class := O2;
       --  INVALID: Trying to assign a value of type derived to a variable of
       --  type My_Class.
    begin
       null;
    end Main;

Because, with tagged types as with other types, an object of a type ``T`` is
exactly of the type ``T``. What you want to say as a programmer is "I want O3
to be able to hold an object of type ``My_Class``, or any type descending from
``My_Class``". Here is how you do that:

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class'Class := O2;
       --  Now valid: My_Class'Class designates the classwide type for
       --  My_Class, which is the set of all types descending from My_Class
       --  (including My_Class).
    begin
       null;
    end Main;

.. attention::
    An object of a classwide type is hence of an unknown size, since it can be of
    the size of any descendent of the base type. It is thus an indefinite type,
    with the expected corollaries:

        - It cannot be stored as a field/component of a record
        - An object of a classwide type needs to be initialized right away (you
          cannot specify the constraints of the type in this case by any other
          way than by initializing it).

Dispatching operations
----------------------

We saw above that it is possible to override operations in types derived from
another tagged type. The aim eventually is to be able to make a dispatching
call, that is, a call to the primitive/method that depends on the exact type of
the object.

But, if you think carefully about what is stated in the above paragraph, a
variable of type ``My_Class`` will always contain an object of exactly this
type. If you want to have a variable that can contain a ``My_Class`` or
any derived type, it has to be of type ``My_Class'Class``.

It follows that, to make a dispatching call, you first have to have an object
that can be of a type, or any type derived from this type, that is, a classwide
type.

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class'Class := O2;

       O4 : My_Class'Class := O1;
    begin
       Foo (O1);
       --  Non dispatching: Calls My_Class.Foo
       Foo (O2);
       --  Non dispatching: Calls Derived.Foo
       Foo (O3);
       --  Dispatching: Calls Derived.Foo
       Foo (O4);
       --  Dispatching: Calls My_Class.Foo
    end Main;

.. attention::
    It is possible to convert an object of type ``Derived`` to an object of
    type ``My_Class``. This is called a view conversion in Ada parlance, and is
    useful, if you want to call a parent method for example.

    In that case, the object is really converted to a ``My_Class`` object,
    which means that its tag is changed. Since tagged objects are always passed
    by reference, you can use this kind of conversion to modify the state of an
    object: Mutations of the converted object will affect the original one.

    .. code:: ada
        :class: ada-run

        with P; use P;

        procedure Main is
           O1 : Derived := (A => 12);
           --  Declaring an object of type Derived

           O2 : My_Class := My_Class (O1);

           O3 : My_Class'Class := O2;
        begin
           Foo (O1);
           --  Non dispatching: Calls Derived.Foo
           Foo (O2);
           --  Non dispatching: Calls My_Class.Foo

           Foo (O3);
           --  Dispatching: Calls My_Class.Foo
        end Main;

Dot notation
------------

Primitives of tagged types can be called with a notation that is more familiar
to object oriented programmers. Given the Foo primitive above, you can also
write the above program as such:

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class'Class := O2;

       O4 : My_Class'Class := O1;
    begin
       O1.Foo;
       --  Non dispatching: Calls My_Class.Foo
       O2.Foo;
       --  Non dispatching: Calls Derived.Foo
       O3.Foo;
       --  Dispatching: Calls Derived.Foo
       O4.Foo;
       --  Dispatching: Calls My_Class.Foo
    end Main;

If the dispatching parameter of a primitive is the first parameter, as is the
case in our examples, then you can call the primitive via the dot notation. Any
extra parameter will be passed normally:


.. code:: ada
    :class: ada-run

    with P; use P;

    procedure Main is
       package Extend is
          type D2 is new Derived with null record;

          procedure Bar (Self : in out D2; Val : Integer);
       end Extend;

       package body Extend is
          procedure Bar (Self : in out D2; Val : Integer) is
          begin
             Self.A := Self.A + Val;
          end Bar;
       end Extend;

       use Extend;

       Obj : D2 := (A => 15);
    begin
       Obj.Bar (2);
       Obj.Foo;
    end Main;
