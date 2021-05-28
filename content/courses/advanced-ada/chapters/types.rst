Types
=====

.. include:: ../../global.txt

Enumerations
------------

We've introduced enumerations back in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/strongly_typed_language>`.
In this section, we'll discuss a few useful features of enumerations, such as
enumeration renaming, enumeration overloading and representation clauses.

Enumerations as functions
~~~~~~~~~~~~~~~~~~~~~~~~~

If you have used programming language such as C in the past, you're familiar
with the concept of enumerations being constants with integer values. In Ada,
however, enumerations are not integers. In fact, they're actually parameterless
functions! Let's consider this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_As_Function

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       --  Essentially, we're declaring these functions:
       --
       --  function Mon return Day;
       --  function Tue return Day;
       --  function Wed return Day;
       --  function Thu return Day;
       --  function Fri return Day;
       --  function Sat return Day;
       --  function Sun return Day;

    end Days;

In the package :ada:`Days`, we're declaring the enumeration type :ada:`Day`.
When we do this, we're essentially declaring seven parameterless functions, one
for each enumeration. For example, the :ada:`Mon` enumeration corresponds to
:ada:`function Mon return Day`. You can see all seven function declarations in
the comments of the example above.

Note that this has no direct relation to how an Ada compiler generates machine
code for enumeration. Even though enumerations are parameterless functions, a
typical Ada compiler doesn't generate function calls for code that deals with
enumerations.

Enumeration renaming
^^^^^^^^^^^^^^^^^^^^

The idea that enumerations are parameterless functions can be used when we want
to rename enumerations. For example, we could rename the enumerations of the
:ada:`Day` type like this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

    package Enumeration_Example is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       function Monday    return Day renames Mon;
       function Tuesday   return Day renames Tue;
       function Wednesday return Day renames Wed;
       function Thursday  return Day renames Thu;
       function Friday    return Day renames Fri;
       function Saturday  return Day renames Sat;
       function Sunday    return Day renames Sun;

    end Enumeration_Example;

Now, we can use both :ada:`Monday` or :ada:`Mon` to refer to Monday of the
:ada:`Day` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

    with Ada.Text_IO;         use Ada.Text_IO;
    with Enumeration_Example; use Enumeration_Example;

    procedure Show_Renaming is
       D1 : constant Day := Mon;
       D2 : constant Day := Monday;
    begin
       if D1 = D2 then
          Put_Line ("D1 = D2");
          Put_Line (Day'Image (D1) & " =  " & Day'Image (D2));
       end if;
    end Show_Renaming;

When running this application, we can confirm that :ada:`D1` is equal to
:ada:`D2`. Also, even though we've assigned :ada:`Monday` to :ada:`D2` (instead
of :ada:`Mon`), the application displays ``Mon = Mon``, since :ada:`Monday`
is just another name to refer to the actual enumeration (:ada:`Mon`).

.. admonition:: Hint

    If you just want to have a single (renamed) enumeration visible in your
    application |mdash| and make the original enumeration invisible |mdash|,
    you can use a separate package. For example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Renaming

        package Enumeration_Example is

           type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

        end Enumeration_Example;

        with Enumeration_Example;

        package Enumeration_Renaming is

           subtype Day is Enumeration_Example.Day;

           function Monday    return Day renames Enumeration_Example.Mon;
           function Tuesday   return Day renames Enumeration_Example.Tue;
           function Wednesday return Day renames Enumeration_Example.Wed;
           function Thursday  return Day renames Enumeration_Example.Thu;
           function Friday    return Day renames Enumeration_Example.Fri;
           function Saturday  return Day renames Enumeration_Example.Sat;
           function Sunday    return Day renames Enumeration_Example.Sun;

        end Enumeration_Renaming;

        with Ada.Text_IO;          use Ada.Text_IO;
        with Enumeration_Renaming; use Enumeration_Renaming;

        procedure Show_Renaming is
           D1 : constant Day := Monday;
        begin
           Put_Line (Day'Image (D1));
        end Show_Renaming;

    Note that the call to :ada:`Put_Line` still display ``Mon`` instead of
    ``Monday``.

Enumeration overloading
~~~~~~~~~~~~~~~~~~~~~~~

Enumerations can be overloaded. In simple terms, this means that the same name
can be used to declare an enumeration of different types. A typical example is
the declaration of colors:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    package Colors is

       type Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       type Primary_Color is
         (Red,
          Green,
          Blue);

    end Colors;

Note that we have :ada:`Red` as an enumeration of type :ada:`Color` and of type
:ada:`Primary_Color`. The same applies to :ada:`Green` and :ada:`Blue`. Because
Ada is a strongly-typed language, in most cases, the enumeration that we're
referring to is clear from the context. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Red_Colors is
       C1 : constant Color         := Red;   --  Using Red from Color
       C2 : constant Primary_Color := Red;   --  Using Red from Primary_Color
    begin
       if C1 = Red then
          Put_Line ("C1 = Red");
       end if;
       if C2 = Red then
          Put_Line ("C2 = Red");
       end if;
    end Red_Colors;

When assigning :ada:`Red` to :ada:`C1` and :ada:`C2`, it is clear that, in the
first case, we're referring to :ada:`Red` of :ada:`Color` type, while in the
second case, we're referring to :ada:`Red` of the :ada:`Primary_Color` type.
The same logic applies to comparisons such as the one in
:ada:`if C1 = Red`: because the type of :ada:`C1` is defined
(:ada:`Color`), it's clear that the :ada:`Red` enumeration is the one of
:ada:`Color` type.

Enumeration subtypes
^^^^^^^^^^^^^^^^^^^^

Note that enumeration overloading is not the same as enumeration subtypes. For
example, we could define the following subtype:

.. code:: ada no_button project=Courses.Advanced_Ada.Types.Enumeration_Overloading

    package Colors.Shades is

       subtype Blue_Shades is Colors range Blue .. Darkblue;

    end Colors.Shades;

In this case, :ada:`Blue` of :ada:`Blue_Shades` and :ada:`Blue` of
:ada:`Colors` are the same enumeration.

Enumeration ambiguities
^^^^^^^^^^^^^^^^^^^^^^^

A situation where enumeration overloading might lead to ambiguities is when we
use them in ranges. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities
    :class: ada-expect-compile-error

    package Colors is

       type Color is
         (Salmon,
          Firebrick,
          Red,
          Darkred,
          Lime,
          Forestgreen,
          Green,
          Darkgreen,
          Blue,
          Mediumblue,
          Darkblue);

       type Primary_Color is
         (Red,
          Green,
          Blue);

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Red .. Blue loop       --  ERROR: range is ambiguous!
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Here, it's not clear whether the range in the loop is of :ada:`Color` type or
of :ada:`Primary_Color` type. Therefore, we get a compilation error for this
code example. The next line in the code example |mdash| the one with the call
to :ada:`Put_Line` |mdash| gives us a hint about the developer's intention to
refer to the :ada:`Color` type. In this case, we can use qualification |mdash|
for example, :ada:`Color'(Red)` |mdash| to resolve the ambiguity:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Color'(Red) .. Color'(Blue) loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Note that, in the case of ranges, we can also rewrite the loop by using a range
declaration:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
    begin
       for C in Color range Red .. Blue loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Alternatively, :ada:`Color range Red .. Blue` could be used in a subtype
declaration, so we could rewrite the example above using a subtype (such as
:ada:`Red_To_Blue`) in the loop:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Ambiguities

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Color_Loop is
       subtype Red_To_Blue is Color range Red .. Blue;
    begin
       for C in Red_To_Blue loop
          Put_Line (Color'Image (C));
       end loop;
    end Color_Loop;

Enumeration representation clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we've said above, a typical Ada compiler doesn't generate function calls for
code that deals with enumerations. On the contrary, each enumeration has values
associated with it, and the compiler uses those values instead.

Each enumeration has:

- a position value, which is a natural value indicating the position of the
  enumeration in the enumeration type; and

- an internal code, which, by default, in most cases, is the same as the
  position value.

Also, by default, the value of the first position is zero, the value of the
second position is one, and so on. We can see this by listing each enumeration
of the :ada:`Day` type and displaying the value of the corresponding position:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D) & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D) & " internal code = "
                    & Integer'Image (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Note that this application also displays the internal code, which, in this
case, is equivalent to the position value for all enumerations.

We may, however, change the internal code of an enumeration using a
representation clause, which has the following format:

.. code-block:: ada

    for Primary_Color is (Red   =>    1,
                          Green =>    5,
                          Blue  => 1000);

The value of each code in a representation clause must be distinct. However, as
you can see above, we don't need to use sequential values |mdash| the values
must, however, increase for each enumeration.

We can rewrite the previous example using a representation clause:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       for Day use (Mon => 2#00000001#, Tue => 2#00000010#,
                    Wed => 2#00000100#, Thu => 2#00001000#,
                    Fri => 2#00010000#, Sat => 2#00100000#,
                    Sun => 2#01000000#);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D) & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D) & " internal code = "
                    & Integer'Image (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Now, the value of the internal code is the one that we've specified in the
representation clause instead of being equivalent to the value of the
enumeration position.

In the example above, we're using binary values for each enumeration |mdash|
basically viewing the integer value as a bit-field and assigning one bit for
each enumeration. As long as we maintain an increasing order, we can use
totally arbitrary values as well. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       for Day use (Mon =>  5, Tue =>  9,
                    Wed => 42, Thu => 49,
                    Fri => 50, Sat => 66,
                    Sun => 99);

    end Days;

.. _Definite_Indefinite_Subtypes:

Definite and Indefinite Subtypes
--------------------------------

Indefinite types were mentioned back in the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/more_about_types>`.
In this section, we'll recapitulate and extend on both definite and indefinite
types.

Definite types are the basic kind of types we commonly use when programming
applications. For example, we can only declare variables of definite types;
otherwise, we get a compilation error. Interestingly, however, to be able to
explain what definite types are, we need to first discuss indefinite types.

Indefinite types include:

- unconstrained arrays;

- record types with unconstrained discriminants without defaults.

Let's see some examples of indefinite types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    package Unconstrained_Types is

       type Integer_Array is array (Positive range <>) of Integer;

       type Simple_Record (Extended : Boolean) is record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

    end Unconstrained_Types;

As we've just mentioned, we cannot declare variable of indefinite types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types
    :class: ada-expect-compile-error

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Unconstrained_Type is

       A : Integer_Array;

       R : Simple_Record;

    begin
       null;
    end Using_Unconstrained_Type;

As we can see when we try to build this example, the compiler complains about
the declaration of :ada:`A` and :ada:`R` because we're trying to use indefinite
types to declare variables. The main reason we cannot use indefinite types here
is that the compiler needs to know at this point how much memory it should
allocate. Therefore, we need to provide the information that is missing. In
other words, we need to change the declaration so the type becomes definite. We
can do this by either declaring a definite type or providing constraints in the
variable declaration. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Using_Unconstrained_Type is

       subtype Integer_Array_5 is Integer_Array (1 .. 5);

       A1 : Integer_Array_5;
       A2 : Integer_Array (1 .. 5);

       subtype Simple_Record_Ext is Simple_Record (Extended => True);

       R1 : Simple_Record_Ext;
       R2 : Simple_Record (Extended => True);

    begin
       null;
    end Using_Unconstrained_Type;

In this example, we declare the :ada:`Integer_Array_5` subtype, which is
definite because we're constraining it to a range from 1 to 5, thereby
defining the information that was missing in the indefinite type
:ada:`Integer_Array`. Because we now have a definite type, we can use it to
declare the :ada:`A1` variable. Similarly, we can use the indefinite type
:ada:`Integer_Array` directly in the declaration of :ada:`A2` by specifying the
previously unknown range.

Similarly, in this example, we declare the :ada:`Simple_Record_Ext` subtype,
which is definite because we're initializing the record discriminant
:ada:`Extended`. We can therefore use it in the declaration of the :ada:`R1`
variable. Alternatively, we can simply use the indefinite type
:ada:`Simple_Record` and specify the information required for the
discriminants. This is what we do in the declaration of the :ada:`R2` variable.

Although we cannot use indefinite types directly in variable declarations,
they're very useful to generalize algorithms. For example, we can use them as
parameters of a subprogram:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array (A : Integer_Array);

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Integer_Array (A : Integer_Array) is
    begin
       for I in A'Range loop
          Put_Line (Positive'Image (I) & ": " & Integer'Image (A (I)));
       end loop;
       Put_Line ("--------");
    end Show_Integer_Array;

    with Unconstrained_Types; use Unconstrained_Types;
    with Show_Integer_Array;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  := (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) := (1, 2, 3, 4, 5, others => 99);
    begin
       Show_Integer_Array (A_5);
       Show_Integer_Array (A_10);
    end Using_Unconstrained_Type;

In this particular example, the compiler doesn't know a priori which range is
used for the :ada:`A` parameter of :ada:`Show_Integer_Array`. It could be a
range from 1 to 5 as used for variable :ada:`A_5` of the
:ada:`Using_Unconstrained_Type` procedure, or it could be a range from 1 to 10
as used for variable :ada:`A_10`, or it could be anything else. Although the
parameter :ada:`A` of :ada:`Show_Integer_Array` is unconstrained, both calls to
:ada:`Show_Integer_Array` |mdash| in :ada:`Using_Unconstrained_Type` procedure
|mdash| use constrained objects.

Note that we could call the :ada:`Show_Integer_Array` procedure above with
another unconstrained parameter. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_With_Header (AA : Integer_Array;
                                              HH : String);

    with Ada.Text_IO;         use Ada.Text_IO;
    with Show_Integer_Array;

    procedure Show_Integer_Array_With_Header (AA : Integer_Array;
                                              HH : String) is
    begin
       Put_Line (HH);
       Show_Integer_Array (AA);
    end Show_Integer_Array_With_Header;

    with Unconstrained_Types;            use Unconstrained_Types;
    with Show_Integer_Array_With_Header;

    procedure Using_Unconstrained_Type is
       A_5  : constant Integer_Array (1 .. 5)  := (1, 2, 3, 4, 5);
       A_10 : constant Integer_Array (1 .. 10) := (1, 2, 3, 4, 5, others => 99);
    begin
       Show_Integer_Array_With_Header (A_5, "First example");
       Show_Integer_Array_With_Header (A_10, "Second example");
    end Using_Unconstrained_Type;

In this case, we're calling the :ada:`Show_Integer_Array` procedure with
another unconstrained parameter (the :ada:`AA` parameter). However, although we
could have a long *chain* of procedure calls using indefinite types in their
parameters, we still use a (definite) object at the beginning of this chain.
For example, for the :ada:`A_5` object, we have this chain:

::

    A_5

        ==> Show_Integer_Array_With_Header (AA => A_5, ...);

            ==> Show_Integer_Array (A => AA);

Therefore, at this specific call to :ada:`Show_Integer_Array`, even though
:ada:`A` is declared as a parameter of indefinite type, the actual argument
is of definite type because :ada:`A_5` is constrained |mdash| and, thus, of
definite type.

Note that we can declare variables based on parameters of indefinite type. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Indefinite_Types

    with Unconstrained_Types; use Unconstrained_Types;

    procedure Show_Integer_Array_Plus (A : Integer_Array;
                                       V : Integer);

    with Show_Integer_Array;

    procedure Show_Integer_Array_Plus (A : Integer_Array;
                                       V : Integer) is
       A_Plus : Integer_Array (A'Range);
    begin
       for I in A_Plus'Range loop
          A_Plus (I) := A (I) + V;
       end loop;
       Show_Integer_Array (A_Plus);
    end Show_Integer_Array_Plus;

    with Unconstrained_Types; use Unconstrained_Types;
    with Show_Integer_Array_Plus;

    procedure Using_Unconstrained_Type is
       A_5 : constant Integer_Array (1 .. 5) := (1, 2, 3, 4, 5);
    begin
       Show_Integer_Array_Plus (A_5, 5);
    end Using_Unconstrained_Type;

In the :ada:`Show_Integer_Array_Plus` procedure, we're declaring :ada:`A_Plus`
based on the range of :ada:`A`, which is itself of indefinite type. However,
since the object passed as an argument to :ada:`Show_Integer_Array_Plus` must
have a constraint, :ada:`A_Plus` will also be constrained. For example, in the
call to :ada:`Show_Integer_Array_Plus` using :ada:`A_5` as an argument, the
declaration of :ada:`A_Plus` becomes :ada:`A_Plus : Integer_Array (1 .. 5);`.
Therefore, it becomes clear that the compiler needs to allocate five elements
for :ada:`A_Plus`.

We'll see later how definite and indefinite types apply to
:ref:`formal parameters <Formal_Definite_Indefinite_Subtypes>`.

Incomplete types
----------------

Incomplete types |mdash| as the name suggests |mdash| are types that have
missing information in their declaration. This is a simple example:

.. code-block:: ada

    type Incomplete;

Because this type declaration is incomplete, we need to provide the missing
information at some later point. Consider the incomplete type :ada:`R` in the
following example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Incomplete_Types

    package Incomplete_Type_Example is

       type R;
       --  Incomplete type declaration!

       type R is record
          I : Integer;
       end record;
       --  type R is now complete!

    end Incomplete_Type_Example;

The first declaration of type :ada:`R` is incomplete. However, in the second
declaration of :ada:`R`, we specify that :ada:`R` is a record. By providing
this missing information, we're completing the type declaration of :ada:`R`.

It's also possible to declare an incomplete type in the private part of a
package specification and its complete form in the package body. Let's rewrite
the example above accordingly:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Incomplete_Types_2

    package Incomplete_Type_Example is

    private

       type R;
       --  Incomplete type declaration!

    end Incomplete_Type_Example;

    package body Incomplete_Type_Example is

       type R is record
          I : Integer;
       end record;
       --  type R is now complete!

    end Incomplete_Type_Example;

A typical application of incomplete types is to create linked lists using
access types based on those incomplete types. This kind of type is called
a recursive type. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Linked_List_Example

    package Linked_List_Example is

       type Integer_List;

       type Next is access Integer_List;

       type Integer_List is record
          I : Integer;
          N : Next;
       end record;

    end Linked_List_Example;

Here, the :ada:`N` component of :ada:`Integer_List` is essentially giving us
access to the next element of :ada:`Integer_List` type. Because the :ada:`Next`
type is both referring to the :ada:`Integer_List` type and being used in the
declaration of the :ada:`Integer_List` type, we need to start with an
incomplete declaration of the :ada:`Integer_List` type and then complete it
after the declaration of :ada:`Next`.

Incomplete types are useful to declare mutually dependent types, as we'll
see in the next section. Also, we can also have formal incomplete types, as
we'll discuss :ref:`later <Formal_Incomplete_Types>`.

Mutually dependent types
------------------------

In this section, we discuss how to use incomplete types to declare mutually
dependent types. Let's start with this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent
    :class: ada-expect-compile-error

    package Mutually_Dependent is

       type T1 is record
          B : T2;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

When you try to compile this example, you get a compilation error. The first
problem with this code is that, in the declaration of the :ada:`T1` record, the
compiler doesn't know anything about :ada:`T2`. We could solve this by
declaring an incomplete type (:ada:`type T2;`) before the declaration of
:ada:`T1`. This, however, doesn't solve all the problems in the code: the
compiler still doesn't know the size of :ada:`T2`, so we cannot create a
component of this type. We could, instead, declare an access type and use it
here, or simply use an anonymous access to :ada:`T2`. By doing this, even
though the compiler doesn't know the size of :ada:`T2`, it knows the
size of an access type designating :ada:`T2`, so the record component
can be of such an access type (anonymous or not)."

To summarize, in order to solve the compilation error above, we need to:

- use at least one incomplete type;
- declare at least one component as an access to an object.

For example, we could declare an incomplete type :ada:`T2` and then declare
the component :ada:`B` of the :ada:`T1` record as an access to :ada:`T2`.
This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent

    package Mutually_Dependent is

       type T2;

       type T1 is record
          B : access T2;
       end record;

       type T2 is record
          A : T1;
       end record;

    end Mutually_Dependent;

We could strive for consistency and declare two incomplete types and two
accesses, but this isn't strictly necessary in this case. Here's the adapted
code:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Mutually_Dependent

    package Mutually_Dependent is

       type T1;
       type T2;

       type T1 is record
          B : access T2;
       end record;

       type T2 is record
          A : access T1;
       end record;

    end Mutually_Dependent;

Type view
---------

.. todo::

    Complete section!


Default initial values
----------------------

In the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/records>`,
we've seen that record components can have default values. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_1

    package Defaults is

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

    end Defaults;

In this section, we'll extend the concept of default values to other kinds of
type declarations, such as scalar types and arrays.

To assign a default value for a scalar type declaration |mdash| such as an
enumeration and a new integer |mdash|, we use the :ada:`Default_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_2

    package Defaults is

       type E is (E1, E2, E3) with Default_Value => E1;

       type T is new Integer with Default_Value => -1;

    end Defaults;

Note that we cannot specify a default value for a subtype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_3
    :class: ada-expect-compile-error

    package Defaults is

       subtype T is Integer with Default_Value => -1;
       --  ERROR!!

    end Defaults;

For array types, we use the :ada:`Default_Component_Value` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults_4

    package Defaults is

       type Arr is array (Positive range <>) of Integer
         with Default_Component_Value => -1;

    end Defaults;

This is a package containing the declarations we've just seen:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Defaults

    package Defaults is

       type E is (E1, E2, E3) with Default_Value => E1;

       type T is new Integer with Default_Value => -1;

       --  We cannot specify default values for subtypes:
       --
       --  subtype T is Integer with Default_Value => -1;

       type R is record
         X : Positive := 1;
         Y : Positive := 10;
       end record;

       type Arr is array (Positive range <>) of Integer
         with Default_Component_Value => -1;

    end Defaults;

In the example below, we declare variables of the types from the
:ada:`Defaults` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Types.Defaults

    with Ada.Text_IO; use Ada.Text_IO;
    with Defaults; use Defaults;

    procedure Use_Defaults is
       E1 : E;
       T1 : T;
       R1 : R;
       A1 : Arr (1 .. 5);
    begin
       Put_Line ("Enumeration:  " & E'Image (E1));
       Put_Line ("Integer type: " & T'Image (T1));
       Put_Line ("Record type:  " & Positive'Image (R1.X)
                 & ", " & Positive'Image (R1.Y));

       Put ("Array type:   ");
       for V of A1 loop
          Put (Integer'Image (V) & " ");
       end loop;
       New_Line;
    end Use_Defaults;

As we see in the :ada:`Use_Defaults` procedure, all variables still have their
default values, since we haven't assigned any value to them.


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Stable Properties of a Type
    ---------------------------

    .. admonition:: Relevant topics

        - `Stable Properties of a Type <http://www.ada-auth.org/standards/2xrm/html/RM-7-3-4.html>`_


Deferred Constants
------------------

Deferred constants are declarations where the value of the constant is not
specified immediately, but rather *deferred* to a later point. In that sense,
if a constant declaration is deferred, it is actually declared twice:

1. in the deferred constant declaration, and
2. in the full constant declaration.

The simplest form of deferred constant is the one that has a full constant
declaration in the private part of the package specification. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Private

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       Light : constant Speed := 299_792_458.0;
       --      ^ full constant declaration

    end Deferred_Constants;

Another form of deferred constant is the one that imports a constant from an
external implementation |mdash| using the :ada:`Import` keyword. We can use
this to import a constant declaration from an implementation in C. For example,
we can declare the :c:`light` constant in a C file:

.. code:: c no_button manual_chop project=Courses.Advanced_Ada.Types.Deferred_Constant_C
    :class: ada-syntax-only

    !constants.c
    double light = 299792458.0;

Then, we can import this constant in the :ada:`Deferred_Constants` package:

.. code:: ada no_button project=Courses.Advanced_Ada.Types.Deferred_Constant_C
    :class: ada-syntax-only

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed with
         Import, Convention => C;
       --      ^ deferred constant declaration; imported from C file

    end Deferred_Constants;

In this case, we don't have a full declaration in the :ada:`Deferred_Constants`
package, as the :ada:`Light` constant is imported from the :file:`constants.c`
file.

As a rule, the deferred and the full declarations should match |mdash| except,
of course, for the actual value that is missing in the deferred declaration.
For instance, we're not allowed to use different types in both declarations.
However, we may use a subtype in the full declaration |mdash| as long as it's
compatible with the type that was used in the deferred declaration. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Subtype

    package Deferred_Constants is

       type Speed is new Long_Float;

       subtype Positive_Speed is Speed range 0.0 .. Speed'Last;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       Light : constant Positive_Speed := 299_792_458.0;
       --      ^ full constant declaration using a subtype

    end Deferred_Constants;

Here, we're using the :ada:`Speed` type in the deferred declaration of the
:ada:`Light` constant, but we're using the :ada:`Positive_Speed` subtype in
the full declaration.

A useful application of deferred constants is when the value of the constant is
calculated using entities not meant to be compile-time visible to clients.
As such, these other entities are only visible in the private part of the
package, so that's where the value of the deferred constant must be computed.
For example, the full constant declaration may be computed by a call to an
expression function:

.. code:: ada compile_button project=Courses.Advanced_Ada.Types.Deferred_Constant_Function

    package Deferred_Constants is

       type Speed is new Long_Float;

       Light : constant Speed;
       --      ^ deferred constant declaration

    private

       function Calculate_Light return Speed is (299_792_458.0);

       Light : constant Speed := Calculate_Light;
       --      ^ full constant declaration calling a private function

    end Deferred_Constants;

Here, we call the :ada:`Calculate_Light` function |mdash| declared in the
private part of the :ada:`Deferred_Constants` package |mdash| for the full
declaration of the :ada:`Light` constant.


User-defined literals
---------------------

.. admonition:: Relevant topics

    - `User-Defined Literals <http://www.ada-auth.org/standards/2xrm/html/RM-4-2-1.html>`_

.. todo::

    Complete section!


Data Representation
-------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #27: Changing Data Representation <https://www.adacore.com/gems/gem-27>`_
    and `Gem #28 <https://www.adacore.com/gems/gem-28>`_.

.. admonition:: Relevant topics

    - Include: ``Object_Size``, ``Value_Size``, ``Alignment``, ``T'Base``
    - `Operational and Representation Attributes <http://www.ada-auth.org/standards/2xrm/html/RM-13-3.html>`_
    - `Packed Types <http://www.ada-auth.org/standards/2xrm/html/RM-13-2.html>`_
    - `Bit Ordering <http://www.ada-auth.org/standards/2xrm/html/RM-13-5-3.html>`_
    - `At Clauses <http://www.ada-auth.org/standards/2xrm/html/RM-J-7.html>`_

.. todo::

    Complete section!


Record Representation and storage clauses
-----------------------------------------

.. admonition:: Relevant topics

    - **Briefly** discuss record representation and storage clauses
    - `Record Representation Clauses <http://www.ada-auth.org/standards/2xrm/html/RM-13-5-1.html>`_
    - `Storage Place Attributes <http://www.ada-auth.org/standards/2xrm/html/RM-13-5-2.html>`_
    - `Mod Clauses <http://www.ada-auth.org/standards/2xrm/html/RM-J-8.html>`_

.. todo::

    Complete section!


Valid
-----

.. admonition:: Relevant topics

    - `The Valid Attribute <http://www.ada-auth.org/standards/2xrm/html/RM-13-9-2.html>`_

.. todo::

    Complete section!


Unchecked Union
---------------

.. admonition:: Relevant topics

    - **Briefly** discuss unchecked unions, as mentioned in
      `Unchecked Union Types <http://www.ada-auth.org/standards/2xrm/html/RM-B-3-3.html>`_

.. todo::

    Complete section!


Variable control
----------------

.. admonition:: Relevant topics

    - **Briefly** discuss :ada:`Atomic`, :ada:`Volatile`, etc
    - `Shared Variable Control <http://www.ada-auth.org/standards/2xrm/html/RM-C-6.html>`_
    - `The Package System.Atomic_Operations <http://www.ada-auth.org/standards/2xrm/html/RM-C-6-1.html>`_

.. todo::

    Complete section!


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Discarding names
    ----------------

    .. admonition:: Relevant topics

        - **Briefly** discuss discarding name mentioned in
        `Aspect Discard_Names <http://www.ada-auth.org/standards/2xrm/html/RM-C-5.html>`_
