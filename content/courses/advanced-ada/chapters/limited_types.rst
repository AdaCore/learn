Limited Types
=============

.. include:: ../../global.txt

Limited types and aggregates
----------------------------

.. note::

    This section was originally written by Bob Duff and published as
    `Gem #1: Limited Types in Ada 2005 <https://www.adacore.com/gems/gem-1>`_,
    `Gem #2 <https://www.adacore.com/gems/gem-2>`_, and
    `Gem #3 <https://www.adacore.com/gems/gem-3>`_.

Full coverage rules
~~~~~~~~~~~~~~~~~~~

One interesting feature of Ada is the *full coverage rules* for
aggregates. For example, suppose we have a record type:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Persons is
       type Years is new Natural;

       type Person is record
          Name : Ada.Strings.Unbounded.Unbounded_String;
          Age  : Years;
       end record;
    end Persons;

We can create an object of the type using an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Init is

       X : constant Person :=
             (Name => To_Unbounded_String ("John Doe"),
              Age  => 25);
    begin
       null;
    end Show_Aggregate_Init;

The full coverage rules say that every component of :ada:`Person` must be
accounted for in the aggregate. If we later modify type :ada:`Person` by
adding a component:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Persons is
       type Years is new Natural;

       type Person is record
          Name      : Unbounded_String;
          Age       : Natural;
          Shoe_Size : Positive;
       end record;
    end Persons;

and we forget to modify :ada:`X` accordingly, the compiler will remind us.
Case statements also have full coverage rules, which serve a similar
purpose.

Of course, we can defeat the full coverage rules by using :ada:`others`
(usually for array aggregates and case statements, but occasionally useful
for record aggregates):

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Init_Others is

       X : constant Person :=
             (Name   => To_Unbounded_String ("John Doe"),
              others => 25);
    begin
       null;
    end Show_Aggregate_Init_Others;

According to the Ada RM, :ada:`others` here means precisely the same thing
as :ada:`Age | Shoe_Size`. But that's wrong: what :ada:`others` really
means is "all the other components, including the ones we might add next
week or next year". That means you shouldn't use :ada:`others` unless
you're pretty sure it should apply to all the cases that haven't been
invented yet.

Full coverage rules for limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The full coverage rules have been aiding maintenance since Ada 83. Since
Ada 2005, however, we can also use them for limited types. Suppose we have
the following limited type:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Persons is

       type Limited_Person;
       type Limited_Person_Access is access all Limited_Person;

       type Limited_Person is limited record
          Self      : Limited_Person_Access := Limited_Person'Unchecked_Access;
          Name      : Unbounded_String;
          Age       : Natural;
          Shoe_Size : Positive;
       end record;

    end Persons;

This type has a self-reference; it doesn't make sense to copy objects,
because :ada:`Self` would end up pointing to the wrong place. Therefore,
we would like to make the type limited, to prevent developers from
accidentally making copies. After all, the type is probably private, so
developers using this package might not be aware of the problem. We could
also solve that problem with controlled types, but controlled types are
expensive, and add unnecessary complexity if not needed.

Prior to Ada 2005, aggregates were illegal for limited types. Therefore,
we would be faced with a difficult choice: Make the type limited, and
initialize it like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Non_Aggregate_Init is
       X : Limited_Person;
    begin
       X.Name := To_Unbounded_String ("John Doe");
       X.Age := 25;
    end Show_Non_Aggregate_Init;

which has the maintenance problem the full coverage rules are supposed to
prevent. Or, make the type non-limited, and gain the benefits of
aggregates, but lose the ability to prevent copies.

Since Ada 2005, an aggregate is allowed to be limited; we can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Init is

       X : aliased Limited_Person :=
             (Self      => null, -- Wrong!

              Name      => To_Unbounded_String ("John Doe"),
              Age       => 25,
              Shoe_Size => 10);
    begin
       X.Self := X'Unchecked_Access;
    end Show_Aggregate_Init;

We'll see what to do about that :ada:`Self => null` soon.

One very important requirement should be noted: the implementation is
required to build the value of :ada:`X` *in place*; it cannot construct
the aggregate in a temporary variable and then copy it into :ada:`X`,
because that would violate the whole point of limited objects |mdash|
you can't copy them.

It seems uncomfortable to set the value of :ada:`Self` to the wrong value
(:ada:`null`) and then correct it. It also seems annoying that we have a
(correct) default value for :ada:`Self`, but prior to Ada 2005, we
couldn't use defaults with aggregates. Since Ada 2005, a new syntax in
aggregates is available: :ada:`<>` means "use the default value, if any".

Here, we can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Persons; use Persons;

    procedure Show_Aggregate_Box_Init is
       X : aliased Limited_Person :=
             (Self      => <>,
              Name      => To_Unbounded_String ("John Doe"),
              Age       => 25,
              Shoe_Size => 10);
    begin
       null;
    end Show_Aggregate_Box_Init;

The :ada:`Self => <>` means use the default value of
:ada:`Limited_Person'Unchecked_Access`. Since :ada:`Limited_Person`
appears inside the type declaration, it refers to the "current instance"
of the type, which in this case is :ada:`X`. Thus, we are setting
:ada:`X.Self` to be :ada:`X'Unchecked_Access`.

Note that using :ada:`<>` in an aggregate can be dangerous, because it can
leave some components uninitialized. :ada:`<>` means "use the default
value". If the type of a component is scalar, and there is no
record-component default, then there is no default value.

For example, if we have an aggregate of type :ada:`String`, like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.String_Box_Init

    procedure Show_String_Box_Init is
        Uninitialized_String_Const : constant String := (1 .. 10 => <>);
    begin
       null;
    end Show_String_Box_Init;

we end up with a 10-character string all of whose characters are invalid
values. Note that this is no more nor less dangerous than this:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Dangerous_String

    procedure Show_Dangerous_String is
        Uninitialized_String_Var : String (1 .. 10);  --  no initialization

        Uninitialized_String_Const : constant String := Uninitialized_String_Var;
    begin
       null;
    end Show_Dangerous_String;

As always, one must be careful about uninitialized scalar objects.

Constructor functions for limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given that we can use build-in-place aggregates for limited types,
the obvious next step is to allow such aggregates to be wrapped in an
abstraction |mdash| namely, to return them from functions. After all,
interesting types are usually private, and we need some way for clients
to create and initialize objects.

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package P is
       task type Some_Task_Type;

       protected type Some_Protected_Type is
          --  dummy type
       end Some_Protected_Type;

       type T (<>) is limited private;
       function Make_T (Name : String) return T; -- constructor function
    private
       type T is limited
          record
             Name    : Unbounded_String;
             My_Task : Some_Task_Type;
             My_Prot : Some_Protected_Type;
          end record;
    end P;

    package body P is

       task body Some_Task_Type is
       begin
          null;
       end Some_Task_Type;

       protected body Some_Protected_Type is
       end Some_Protected_Type;

       function Make_T (Name : String) return T is
       begin
          return (Name => To_Unbounded_String (Name), others => <>);
       end Make_T;

    end P;

Prior to Ada 2005, constructor functions (that is, functions that create
new objects and return them) were not allowed for limited types. Since
Ada 2005, fully-general constructor functions are allowed. Given the
above, clients can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions

    with P; use P;

    procedure Show_Constructor_Function is
       My_T : T := Make_T (Name => "Bartholomew Cubbins");
    begin
       null;
    end Show_Constructor_Function;

As for aggregates, the result of :ada:`Make_T` is built in place (that is,
in :ada:`My_T`), rather than being created and then copied into
:ada:`My_T`. Adding another level of function call, we can do:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions

    with P; use P;

    procedure Show_Rumplestiltskin_Constructor is

       function Make_Rumplestiltskin return T is
       begin
           return Make_T (Name => "Rumplestiltskin");
       end Make_Rumplestiltskin;

       Rumplestiltskin_Is_My_Name : constant T := Make_Rumplestiltskin;
    begin
       null;
    end Show_Rumplestiltskin_Constructor;

It might help to understand the implementation model: In this case,
:ada:`Rumplestiltskin_Is_My_Name` is allocated in the usual way (on the
stack, presuming it is declared local to some subprogram). Its address is
passed as an extra implicit parameter to :ada:`Make_Rumplestiltskin`,
which then passes that same address on to :ada:`Make_T`, which then builds
the aggregate in place at that address. Limited objects must never be
copied! In this case, :ada:`Make_T` will initialize the :ada:`Name`
component, and create the :ada:`My_Task` and :ada:`My_Prot` components,
all directly in :ada:`Rumplestiltskin_Is_My_Name`.

Note that :ada:`Rumplestiltskin_Is_My_Name` is constant. Prior to
Ada 2005, it was impossible to create a constant limited object, because
there was no way to initialize it.

The :ada:`(<>)` on type :ada:`T` means that it has *unknown
discriminants* from the point of view of the client. This is a trick that
prevents clients from creating default-initialized objects (that is,
:ada:`X : T;` is illegal). Thus clients must call :ada:`Make_T` whenever
an object of type :ada:`T` is created, giving package :ada:`P` full
control over initialization of objects.

Ideally, limited and non-limited types should be just the same, except for
the essential difference: you can't copy limited objects. By allowing
functions and aggregates for limited types, we're very close to this goal.
Some languages have a specific feature called *constructor*. In Ada, a
*constructor* is just a function that creates a new object. Prior to
Ada 2005, that only worked for non-limited types. For limited types, the
only way to *construct* on declaration was via default values, which
limits you to one constructor. And the only way to pass parameters to that
construction was via discriminants. Consider the following package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions_2

    with Ada.Containers.Ordered_Sets;

    package Aux is
       generic
          with package OS is new Ada.Containers.Ordered_Sets (<>);
       function Gen_Singleton_Set (Element : OS.Element_Type) return OS.Set;
    end Aux;

    package body Aux is
       function Gen_Singleton_Set  (Element : OS.Element_Type) return OS.Set is
       begin
          return S : OS.Set := OS.Empty_Set do
             S.Insert (Element);
          end return;
       end Gen_Singleton_Set;
    end Aux;

Since Ada 2005, we can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions_2

    with Ada.Containers.Ordered_Sets;
    with Aux;

    procedure Show_Set_Constructor is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);
       use Integer_Sets;

       function Singleton_Set is new Aux.Gen_Singleton_Set (OS => Integer_Sets);

       This_Set : Set := Empty_Set;
       That_Set : Set := Singleton_Set (Element => 42);
    begin
       null;
    end Show_Set_Constructor;

whether or not :ada:`Set` is limited. :ada:`This_Set : Set := Empty_Set;`
seems clearer than:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Constructor_Functions_2

    with Ada.Containers.Ordered_Sets;

    procedure Show_Set_Decl is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);
       use Integer_Sets;

       This_Set : Set;
    begin
       null;
    end Show_Set_Decl;

which might mean "default-initialize to the empty set" or might mean
"leave it uninitialized, and we'll initialize it in later".

Return objects
--------------

.. note::

    This section was originally written by Bob Duff and published as
    `Gem #10: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-10>`_.

Extended return statements
~~~~~~~~~~~~~~~~~~~~~~~~~~

A common idiom in Ada is to build up a function result in a local
object, and then return that object:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Simple_Return

    procedure Show_Return is

       type Array_Of_Natural is array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural) return Natural is
          Result : Natural := 0;
       begin
          for Index in A'Range loop
             Result := Result + A (Index);
          end loop;
          return Result;
       end Sum;

    begin
       null;
    end Show_Return;

Since Ada 2005, a notation called the :ada:`extended_return_statement`,
which allows you to declare the result object and return it as part of one
statement, is available. It looks like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return

    procedure Show_Extended_Return is

       type Array_Of_Natural is array (Positive range <>) of Natural;

       function Sum (A : Array_Of_Natural) return Natural is
       begin
          return Result : Natural := 0 do
             for Index in A'Range loop
                Result := Result + A (Index);
             end loop;
          end return;
       end Sum;

    begin
       null;
    end Show_Extended_Return;

The return statement here creates :ada:`Result`, initializes it to
:ada:`0`, and executes the code between :ada:`do` and :ada:`end return`.
When :ada:`end return` is reached, :ada:`Result` is automatically returned
as the function result.

Extended return statements for limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For most types, extended return statements are no big deal |mdash| it's just
syntactic sugar. But for limited types, this syntax is almost essential:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return_Limited
    :class: ada-syntax-only

    package Task_Construct_Error is

       task type Task_Type (Discriminant : Integer);

       function Make_Task (Val : Integer) return Task_Type;

    end Task_Construct_Error;

    package body Task_Construct_Error is

       task body Task_Type is
       begin
          null;
       end Task_Type;

       function Make_Task (Val : Integer) return Task_Type is
          Result : Task_Type (Discriminant => Val * 3);
       begin
          --  some statements...
          return Result; -- Illegal!
       end Make_Task;

    end Task_Construct_Error;

The return statement here is illegal, because :ada:`Result` is local to
:ada:`Make_Task`, and returning it would involve a copy, which makes no
sense (which is why task types are limited). Since Ada 2005, we can write
constructor functions for task types:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return_Limited

    package Task_Construct is

       task type Task_Type (Discriminant : Integer);

       function Make_Task (Val : Integer) return Task_Type;

    end Task_Construct;

    package body Task_Construct is

       task body Task_Type is
       begin
          null;
       end Task_Type;

       function Make_Task (Val : Integer) return Task_Type is
       begin
          return Result : Task_Type (Discriminant => Val * 3) do
             --  some statements...
             null;
          end return;
       end Make_Task;

    end Task_Construct;

If we call it like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return_Limited

    with Task_Construct; use Task_Construct;

    procedure Show_Task_Construct is
       My_Task : Task_Type := Make_Task (Val => 42);
    begin
       null;
    end Show_Task_Construct;

:ada:`Result` is created *in place* in :ada:`My_Task`. :ada:`Result` is
temporarily considered local to :ada:`Make_Task` during the
:ada:`-- some statements` part, but as soon as :ada:`Make_Task` returns,
the task becomes more global. :ada:`Result` and :ada:`My_Task` really are
one and the same object.

When returning a task from a function, it is activated after the function
returns. The :ada:`-- some statements` part had better not try to call one
of the task's entries, because that would deadlock. That is, the entry
call would wait until the task reaches an accept statement, which will
never happen, because the task will never be activated.

Other usages of extended return statements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While the :ada:`extended_return_statement` was added to the language
specifically to support limited constructor functions, it comes in handy
whenever you want a local name for the function result:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Extended_Return_Other_Usages

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_String_Construct is

       function Make_String (S          : String;
                             Prefix     : String;
                             Use_Prefix : Boolean) return String is
          Length : Natural := S'Length;
       begin
          if Use_Prefix then
             Length := Length + Prefix'Length;
          end if;

          return Result : String (1 .. Length) do

             --  fill in the characters
             if Use_Prefix then
                Result (1 .. Prefix'Length) := Prefix;
                Result (Prefix'Length + 1 .. Length) := S;
             else
                Result := S;
             end if;

          end return;
       end Make_String;

       S1 : String := "Ada";
       S2 : String := "Make_With_";
    begin
       Put_Line ("No prefix:   " & Make_String (S1, S2, False));
       Put_Line ("With prefix: " & Make_String (S1, S2, True));
    end Show_String_Construct;

Building objects from constructors
----------------------------------

.. note::

    This section was originally written by Bob Duff and published as
    `Gem #11: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-11>`_.

We've earlier seen examples of constructor functions for limited types
similar to this:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package P is
       task type Some_Task_Type;

       protected type Some_Protected_Type is
          --  dummy type
       end Some_Protected_Type;

       type T is limited private;
       function Make_T (Name : String) return T; -- constructor function
    private
       type T is limited
          record
             Name    : Unbounded_String;
             My_Task : Some_Task_Type;
             My_Prot : Some_Protected_Type;
          end record;
    end P;

    package body P is

       task body Some_Task_Type is
       begin
          null;
       end Some_Task_Type;

       protected body Some_Protected_Type is
       end Some_Protected_Type;

       function Make_T (Name : String) return T is
       begin
          return (Name => To_Unbounded_String (Name), others => <>);
       end Make_T;

    end P;

    package P.Aux is
       function Make_Rumplestiltskin return T;
    end P.Aux;

    package body P.Aux is

       function Make_Rumplestiltskin return T is
       begin
          return Make_T (Name => "Rumplestiltskin");
       end Make_Rumplestiltskin;

    end P.Aux;

It is useful to consider the various contexts in which these functions may
be called. We've already seen things like:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Rumplestiltskin_Constructor is
       Rumplestiltskin_Is_My_Name : constant T := Make_Rumplestiltskin;
    begin
       null;
    end Show_Rumplestiltskin_Constructor;

in which case the limited object is built directly in a standalone object.
This object will be finalized whenever the surrounding scope is left.

We can also do:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Parameter_Constructor is
       procedure Do_Something (X : T) is null;
    begin
       Do_Something (X => Make_Rumplestiltskin);
    end Show_Parameter_Constructor;

Here, the result of the function is built directly in the formal parameter
:ada:`X` of :ada:`Do_Something`. :ada:`X` will be finalized as soon as we
return from :ada:`Do_Something`.

We can allocate initialized objects on the heap:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Heap_Constructor is

       type T_Ref is access all T;

       Global : T_Ref;

       procedure Heap_Alloc is
          Local : T_Ref;
          To_Global : Boolean := True;
       begin
          Local := new T'(Make_Rumplestiltskin);
          if To_Global then
             Global := Local;
          end if;
       end Heap_Alloc;

    begin
       null;
    end Show_Heap_Constructor;

The result of the function is built directly in the heap-allocated object,
which will be finalized when the scope of :ada:`T_Ref` is left (long after
:ada:`Heap_Alloc` returns).

We can create another limited type with a component of type :ada:`T`, and
use an aggregate:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Outer_Type is

       type Outer_Type is limited record
          This : T;
          That : T;
       end record;

       Outer_Obj : Outer_Type := (This => Make_Rumplestiltskin,
                                  That => Make_T (Name => ""));

    begin
       null;
    end Show_Outer_Type;

As usual, the function results are built in place, directly in
:ada:`Outer_Obj.This` and :ada:`Outer_Obj.That`, with no copying involved.

The one case where we *cannot* call such constructor functions is in an
assignment statement:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors
    :class: ada-expect-compile-error

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Illegal_Constructor is
       Rumplestiltskin_Is_My_Name : T;
    begin
       Rumplestiltskin_Is_My_Name := Make_T (Name => "");  --  Illegal!
    end Show_Illegal_Constructor;

which is illegal because assignment statements involve copying. Likewise,
we can't copy a limited object into some other object:

.. code:: ada run_button project=Courses.Advanced_Ada.Limited_Types.Building_Objs_From_Constructors
    :class: ada-expect-compile-error

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Illegal_Constructor is
       Rumplestiltskin_Is_My_Name : constant T := Make_T (Name => "");
       Other : T := Rumplestiltskin_Is_My_Name; -- Illegal!
    begin
       null;
    end Show_Illegal_Constructor;

Default initialization
----------------------

.. note::

    This section was originally written by Bob Duff and published as
    `Gem #12: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-12>`_.

Prior to Ada 2005, the following style was common:

.. code:: ada no_button project=Courses.Advanced_Ada.Limited_Types.Default_Init

    package Type_Defaults is
       type Color_Enum is (Red, Blue, Green);

       type T is private;
    private
       type T is
          record
             Color     : Color_Enum := Red;
             Is_Gnarly : Boolean := False;
             Count     : Natural;
          end record;

       procedure Do_Something;
    end Type_Defaults;

.. code:: ada compile_button project=Courses.Advanced_Ada.Limited_Types.Default_Init

    package body Type_Defaults is

       Object_100 : constant T :=
                      (Color => Red, Is_Gnarly => False, Count => 100);

       procedure Do_Something is null;

    end Type_Defaults;

We want :ada:`Object_100` to be a default-initialized :ada:`T`, with
:ada:`Count` equal to :ada:`100`. It's a little bit annoying that we had
to write the default values :ada:`Red` and :ada:`False` twice. What if we
change our mind about :ada:`Red`, and forget to change it in all the
relevant places?

Since Ada 2005, the :ada:`<>` notation comes to the rescue. If we want to
say, "make :ada:`Count` equal :ada:`100`, but initialize :ada:`Color` and
:ada:`Is_Gnarly` to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Limited_Types.Default_Init

    package body Type_Defaults is

       Object_100 : constant T :=
                      (Color => <>, Is_Gnarly => <>, Count => 100);

       procedure Do_Something is null;

    end Type_Defaults;

On the other hand, if we want to say, "make :ada:`Count` equal :ada:`100`,
but initialize all other components, including the ones we might add next
week, to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Limited_Types.Default_Init

    package body Type_Defaults is

       Object_100 : constant T := (Count => 100, others => <>);

       procedure Do_Something is null;

    end Type_Defaults;

Note that if we add a component :ada:`Glorp : Integer;` to type :ada:`T`,
then the :ada:`others` case leaves :ada:`Glorp` undefined just as this
code would do:

.. code:: ada compile_button project=Courses.Advanced_Ada.Limited_Types.Default_Init

    package body Type_Defaults is

       procedure Do_Something is
          Object_100 : T;
       begin
          Object_100.Count := 100;
       end Do_Something;

    end Type_Defaults;

Therefore, you should be careful and think twice before using
:ada:`others`.
