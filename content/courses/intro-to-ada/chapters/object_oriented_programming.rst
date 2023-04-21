Object-oriented programming
===========================

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

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
  permissive static substitutability model. The substitution will fail at runtime
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

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Newtypes

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

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Primitives

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Primitives is
       package Week is
          type Days is (Monday, Tuesday, Wednesday,
                        Thursday, Friday,
                        Saturday, Sunday);

          --  Print_Day is a primitive
          --  of the type Days
         procedure Print_Day (D : Days);
       end Week;

       package body Week is
          procedure Print_Day (D : Days) is
          begin
             Put_Line (Days'Image (D));
          end Print_Day;
       end Week;

       use Week;
       type Weekend_Days is new
         Days range Saturday .. Sunday;

       --  A procedure Print_Day is automatically
       --  inherited here. It is as if the procedure
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
this chapter.

Tagged types are very similar to normal records except that some
functionality is added:

- Types have a *tag*, stored inside each object, that identifies the
  :wikipedia:`runtime type <Run-time_type_information>` of that
  object.

- Primitives can dispatch. A primitive on a tagged type is what you
  would call a *method* in Java or C++. If you derive a base type and
  override a primitive of it, you can often call it on an object with
  the result that which primitive is called depends on the exact
  runtime type of the object.

- Subtyping rules are introduced allowing a tagged type derived from a
  base type to be statically compatible with the base type.

Let's see our first tagged type declarations:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    package P is
       type My_Class is tagged null record;
       --  Just like a regular record, but
       --  with tagged qualifier

       --  Methods are outside of the type
       --  definition:

       procedure Foo (Self : in out My_Class);
       --  If you define a procedure taking a
       --  My_Class argument in the same package,
       --  it will be a method.

       --  Here's how you derive a tagged type:

       type Derived is new My_Class with record
          A : Integer;
          --  You can add fields in derived types.
       end record;

       overriding
       procedure Foo (Self : in out Derived);
       --  The "overriding" qualifier is optional,
       --  but if it is present, it must be valid.
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is
       procedure Foo (Self : in out My_Class) is
       begin
          Put_Line ("In My_Class.Foo");
       end Foo;

       procedure Foo (Self : in out Derived) is
       begin
          Put_Line ("In Derived.Foo, A = "
                    & Integer'Image (Self.A));
       end Foo;
    end P;

Classwide types
---------------

To remain consistent with the rest of the language, a new notation
needed to be introduced to say "This object is of this type or any
descendant derives tagged type".

In Ada, we call this the *classwide type*. It's used in OOP as soon as
you need polymorphism. For example, you can't do the following:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types
    :class: ada-expect-compile-error

    with P; use P;

    procedure Main is

       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class := O2;
       --  INVALID: Trying to assign a value
       --  of type derived to a variable of
       --  type My_Class.
    begin
       null;
    end Main;

This is because an object of a type :ada:`T` is exactly of the type
:ada:`T`, whether :ada:`T` is tagged or not. What you want to say as a
programmer is "I want O3 to be able to hold an object of type
:ada:`My_Class` or any type descending from :ada:`My_Class`". Here's how you
do that:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

       O3 : My_Class'Class := O2;
       --  Now valid: My_Class'Class designates
       --  the classwide type for My_Class,
       --  which is the set of all types
       --  descending from My_Class (including
       --  My_Class).
    begin
       null;
    end Main;

.. attention::
    Because an object of a classwide type can be the size of any
    descendant of its base type, it has an unknown size. It's therefore
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

But, if you think carefully about it, a variable of type :ada:`My_Class`
always contains an object of exactly that type. If you want to have a
variable that can contain a :ada:`My_Class` or any derived type, it has
to be of type :ada:`My_Class'Class`.

In other words, to make a dispatching call, you must first have an
object that can be either of a type or any type derived from this
type, namely an object of a classwide type.

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

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

.. admonition:: Attention

    You can convert an object of type :ada:`Derived` to an
    object of type :ada:`My_Class`. This is called a *view conversion* in
    Ada parlance and is useful, for example, if you want to call a
    parent method.

    In that case, the object really is converted to a :ada:`My_Class`
    object, which means its tag is changed. Since tagged objects are
    always passed by reference, you can use this kind of conversion to
    modify the state of an object: changes to converted object will
    affect the original one.

    .. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

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

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

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


.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       package Extend is
          type D2 is new Derived with null record;

          procedure Bar (Self : in out D2;
                         Val  :        Integer);
       end Extend;

       package body Extend is
          procedure Bar (Self : in out D2;
                         Val  :        Integer) is
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

Private & Limited
-----------------

We've seen previously (in the :doc:`./privacy` chapter) that types can be
declared limited or private. These encapsulation techniques can also be
applied to tagged types, as we'll see in this section.

This is an example of a tagged private type:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Private_Types

    package P is
       type T is tagged private;
    private
       type T is tagged record
          E : Integer;
       end record;
    end P;

This is an example of a tagged limited type:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Limited_Types

    package P is
       type T is tagged limited record
          E : Integer;
       end record;
    end P;

Naturally, you can combine both *limited* and *private* types and declare a
tagged limited private type:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Limited_Private_Types

    package P is
       type T is tagged limited private;

       procedure Init (A : in out T);
    private
       type T is tagged limited record
          E : Integer;
       end record;
    end P;

    package body P is

       procedure Init (A : in out T) is
       begin
          A.E := 0;
       end Init;

    end P;

    with P; use P;

    procedure Main is
       T1, T2 : T;
    begin
       T1.Init;
       T2.Init;

       --  The following line doesn't work
       --  because type T is private:
       --
       --  T1.E := 0;

       --  The following line doesn't work
       --  because type T is limited:
       --
       --  T2 := T1;
    end Main;

Note that the code in the :ada:`Main` procedure above presents two assignments
that trigger compilation errors because type :ada:`T` is limited private.
In fact, you cannot:

- assign to :ada:`T1.E` directly because type :ada:`T` is private;

- assign :ada:`T1` to :ada:`T2` because type :ada:`T` is limited.

In this case, there's no distinction between tagged and non-tagged types: these
compilation errors would also occur for non-tagged types.

Classwide access types
----------------------

In this section, we'll discuss an useful pattern for object-oriented programming
in Ada: classwide access type. Let's start with an example where we declare a
tagged type :ada:`T` and a derived type :ada:`T_New`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error

    package P is
       type T is tagged null record;

       procedure Show (Dummy : T);

       type T_New is new T with null record;

       procedure Show (Dummy : T_New);
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Show (Dummy : T) is
       begin
          Put_Line ("Using type "
                    & T'External_Tag);
       end Show;

       procedure Show (Dummy : T_New) is
       begin
          Put_Line ("Using type "
                    & T_New'External_Tag);
       end Show;

    end P;

Note that we're using null records for both types :ada:`T` and :ada:`T_New`.
Although these types don't actually have any component, we can still use them
to demonstrate dispatching. Also note that the example above makes use of the
:ada:`'External_Tag` attribute in the implementation of the :ada:`Show`
procedure to get a string for the corresponding tagged type.

As we've seen before, we must use a classwide type to create objects that
can make dispatching calls. In other words, objects of type :ada:`T'Class` will
dispatch. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error

    with P; use P;

    procedure Dispatching_Example is
       T2         :          T_New;
       T_Dispatch : constant T'Class := T2;
    begin
       T_Dispatch.Show;
    end Dispatching_Example;

A more useful application is to declare an array of objects that can dispatch.
For example, we'd like to declare an array :ada:`T_Arr`, loop over this array
and dispatch according to the actual type of each individual element:

.. code-block:: ada

    for I in T_Arr'Range loop
       T_Arr (I).Show;
       --  Call Show procedure according
       --  to actual type of T_Arr (I)
    end loop;

However, it's not possible to declare an array of type :ada:`T'Class` directly:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error
    :class: ada-expect-compile-error

    with P; use P;

    procedure Classwide_Compilation_Error is
       T_Arr  : array (1 .. 2) of T'Class;
       --                         ^
       --               Compilation Error!
    begin
       for I in T_Arr'Range loop
          T_Arr (I).Show;
       end loop;
    end Classwide_Compilation_Error;

In fact, it's impossible for the compiler to know which type would actually be
used for each element of the array. However, if we use dynamic allocation via
access types, we can allocate objects of different types for the individual
elements of an array :ada:`T_Arr`. We do this by using classwide access types,
which have the following format:

.. code-block:: ada

    type T_Class is access T'Class;

We can rewrite the previous example using the :ada:`T_Class` type. In this
case, dynamically allocated objects of this type will dispatch according to
the actual type used during the allocation. Also, let's introduce an
:ada:`Init` procedure that won't be overridden for the derived :ada:`T_New`
type. This is the adapted code:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Access

    package P is
       type T is tagged record
          E : Integer;
       end record;

       type T_Class is access T'Class;

       procedure Init (A : in out T);

       procedure Show (Dummy : T);

       type T_New is new T with null record;

       procedure Show (Dummy : T_New);

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Init (A : in out T) is
       begin
          Put_Line ("Initializing type T...");
          A.E := 0;
       end Init;

       procedure Show (Dummy : T) is
       begin
          Put_Line ("Using type "
                    & T'External_Tag);
       end Show;

       procedure Show (Dummy : T_New) is
       begin
          Put_Line ("Using type "
                    & T_New'External_Tag);
       end Show;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with P;           use P;

    procedure Main is
       T_Arr  : array (1 .. 2) of T_Class;
    begin
       T_Arr (1) := new T;
       T_Arr (2) := new T_New;

       for I in T_Arr'Range loop
          Put_Line ("Element # "
                    & Integer'Image (I));

          T_Arr (I).Init;
          T_Arr (I).Show;

          Put_Line ("-----------");
       end loop;
    end Main;

In this example, the first element (:ada:`T_Arr (1)`) is of type :ada:`T`,
while the second element is of type :ada:`T_New`. When running the example,
the :ada:`Init` procedure of type :ada:`T` is called for both elements of the
:ada:`T_Arr` array, while the call to the :ada:`Show` procedure selects the
corresponding procedure according to the type of each element of :ada:`T_Arr`.
