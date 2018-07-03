Object-oriented programming
===========================
:code-config:`run_button=True;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Object-oriented programming (OOP) is a large and ill-defined concept
in programming languages and one that tends to encompass many
different meanings because different languages often implement their
own vision of it, with similarities and differences from the
implementations in other languages.

However, one model mostly "won" the battle of what object-oriented
means, if only by sheer popularity. It's the model used in the Java
programming language, which is very similar to the one used by C++.
Here are some defining characteristics:

- Type derivation and extension: Most object oriented languages allow the user
  to add fields to derived types.

- Subtyping: Objects of a type derived from a base type can, in some
  instances, be substituted for objects of the base type.

- Runtime polymorphism: Calling a subprogram, usually called a
  *method*, attached to an object type can dispatch at runtime
  depending on the exact type of the object.

- Encapsulation: Objects can hide some of their data.

- Extensibility: People from the "outside" of your package, or even
  your whole library, can derive from your object types and define
  their own behaviors.

Ada dates from before object-oriented programming was as popular as it
is today. Some of the mechanisms and concepts from the above list were
in the earliest version of Ada even before what we would call OOP was
added:

- As we saw, encapsulation is not implemented at the type level in
  Ada, but instead at the package level.

- Subtyping can be implemented using, well, subtypes, which have a full and
  permissive static substitability model. The substitution will fail at runtime
  if the dynamic constraints of the subtype are not fulfilled.

- Runtime polymorphism can be implemented using variant records.

However, this lists leaves out type extensions, if you don't consider
variant records, and extensibility.

The 1995 revision of Ada added a feature filling the gaps, which
allowed people to program following the object-oriented paradigm in an
easier fashion.  This feature is called *tagged types*.

.. note:: It's possible to program in Ada without ever creating tagged
    types. If that's your prefered style of programming or you have
    no specific use for tagged types, feel free to not use them, as is
    the case for many features of Ada.

    However, they can be the best way to express solutions to certain
    problems and they may be the best way to solve your problem. If
    that's the case, read on!

Derived types
-------------

Before presenting tagged types, we should discuss a topic we have
brushed on, but not really covered, up to now:

You can create one or more new types from every type in Ada. Type
derivation is built into the language.

.. code:: ada no_button

    package Newtypes is
       type Point is record
           X, Y : Integer;
       end record;

       type New_Point is new Point;
    end Newtypes;

Type derivation is useful to enforce strong typing because the type
system treats the two types as incompatible.

But the benefits are not limited to that: you can inherit things from
the type you derive from. You not only inherit the representation of
the data, but you can also inherit behavior.

When you inherit a type you also inherit what are called *primitive
operations*. A primitive operation (or just a *primitive*) is a
subprogram attached to a type. Ada defines primitives as subprograms
defined in the same scope as the type.

.. attention::
    A subprogram will only become a primitive of the type if:

    1. The subprogram is declared in the same scope as the type and
    2. The type and the subprogram are declared in a package

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

      --  A procedure Print_Day is automatically inherited here. It is as if
      --  the procedure
      --
      --  procedure Print_Day (D : Weekend_Days);
      --
      --  has been declared with the same body

      Sat : Weekend_Days := Saturday;
    begin
       Print_Day (Sat);
    end Primitives;

This kind of inheritance can be very useful, and is not limited to
record types (you can use it on discrete types, as in the example
above), but it's only superficially similar to object-oriented
inheritance:

- Records can't be extended using this mechanism alone.  You also
  can't specify a new representation for the new type: it will
  **always** have the same representation as the base type.

- There's no facility for dynamic dispatch or polymorphism. Objects
  are of a fixed, static type.

There are other differences, but it's not useful to list them all
here. Just remember that this is a kind of inheritance you can use if
you only want to statically inherit behavior without duplicating code
or using composition, but a kind you can't use if you want any dynamic
features that are usually associated with OOP.

Tagged types
------------

The 1995 revision of the Ada language introduced tagged types to
fullfil the need for an unified solution that allows programming in an
object-oriented style similar to the one described at the beginning of
this chaper.

Tagged types are very similar to normal records except that some
functionality is added:

- Types have a *tag*, stored inside each object, that identifies the
  `runtime type
  <https://en.wikipedia.org/wiki/Run-time_type_information>`_ of that
  object.

- Primitives can dispatch. A primitive on a tagged type is what you
  would call a *method* in Java or C++. If you derive a base type and
  override a primitive of it, you can often call it on an object with
  the result that which primitive is called depends on the exact
  runtime type of the object.

- Subtyping rules are introduced allowing a tagged type derived from a
  base type to be statically compatible with the base type.

Let's see our first tagged type declarations:

.. code:: ada no_button

    package P is
       type My_Class is tagged null record;
       --  Just like a regular record, but with tagged qualifier

       --  Methods are outside of the type definition:

       procedure Foo (Self : in out My_Class);
       --  If you define a procedure taking a My_Class argument
       --  in the same package, it will be a method.

       --  Here's how you derive a tagged type:

       type Derived is new My_Class with record
           A : Integer;
           --  You can add fields in derived types.
       end record;

       overriding procedure Foo (Self : in out Derived);
       --  The "overriding" qualifier is optional, but if it is present,
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

To remain consistent with the rest of the language, a new notation
needed to be introduced to say "This object is of this type or any
descendent derives tagged type".

In Ada, we call this the *classwide type*. It's used in OOP as soon as
you need polymorphism. For example, you can't do the following:

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

This is because an object of a type ``T`` is exactly of the type
``T``, whether ``T`` is tagged or not. What you want to say as a
programmer is "I want O3 to be able to hold an object of type
``My_Class`` or any type descending from ``My_Class``". Here's how you
do that:

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

       O3 : My_Class'Class := O2;
       --  Now valid: My_Class'Class designates the classwide type for
       --  My_Class, which is the set of all types descending from My_Class
       --  (including My_Class).
    begin
       null;
    end Main;

.. attention::
    Because an object of a classwide type can be the size of any
    descendent of its base type, it has an unknown size. It's therefore
    an indefinite type, with the expected restrictions:

        - It can't be stored as a field/component of a record
        - An object of a classwide type needs to be initialized immediately
          (you can't specify the constraints of such a type in
	  any way other than by initializing it).

Dispatching operations
----------------------

We saw that you can override operations in types derived from another
tagged type. The eventual goal of OOP is to make a dispatching call: a
call to a primitive (method) that depends on the exact type of the
object.

But, if you think carefully about it, a variable of type ``My_Class``
always contains an object of exactly that type. If you want to have a
variable that can contain a ``My_Class`` or any derived type, it has
to be of type ``My_Class'Class``.

In other words, to make a dispatching call, you must first have an
object that can be either of a type or any type derived from this
type, namely an object of a classwide type.

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

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

.. attention:: You can convert an object of type ``Derived`` to an
    object of type ``My_Class``. This is called a *view conversion* in
    Ada parlance and is useful, for example, if you want to call a
    parent method.

    In that case, the object really is converted to a ``My_Class``
    object, which means its tag is changed. Since tagged objects are
    always passed by reference, you can use this kind of conversion to
    modify the state of an object: changes to converted object will
    affect the original one.

    .. code:: ada
        :class: ada-run

        with P; use P;

        procedure Main is
           O1 : Derived := (A => 12);
           --  Declare an object of type Derived

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

You can also call primitives of tagged types with a notation that's
more familiar to object oriented programmers. Given the Foo primitive
above, you can also write the above program this way:

.. code:: ada

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

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

If the dispatching parameter of a primitive is the first parameter,
which is the case in our examples, you can call the primitive using
the dot notation. Any remaining parameter are passed normally:


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
