More about types
================

.. _Intro_Ada_Aggregates:

.. include:: ../../global.txt

Aggregates: A primer
--------------------

So far, we have talked about aggregates quite a bit and have seen a number of
examples.  Now we will revisit this feature in some more detail.

An Ada aggregate is, in effect, a literal value for a composite type. It's a
very powerful notation that helps you to avoid writing procedural code for the
initialization of your data structures in many cases.

A basic rule when writing aggregates is that *every component* of the array or
record has to be specified, even components that have a default value.

This means that the following code is incorrect:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Incorrect_Aggregate
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

There are a few shortcuts that you can use to make the notation more
convenient:

- To specify the default value for a component, you can use the
  :ada:`<>` notation.

- You can use the :ada:`|` symbol to give several components the same value.

- You can use the :ada:`others` choice to refer to every component that has not
  yet been specified, provided all those fields have the same type.

- You can use the range notation :ada:`..` to refer to specify a contiguous
  sequence of indices in an array.

However, note that as soon as you used a named association, all subsequent
components likewise need to be specified with named associations.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Points

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is
         array (Positive range <>) of Point;

       --  use the default values
       Origin   : Point := (X | Y => <>);

       --  likewise, use the defaults
       Origin_2 : Point := (others => <>);

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1       => (1, 2),
                                  2       => (3, 4),
                                  3 .. 20 => <>);
    end Points;

Overloading and qualified expressions
-------------------------------------

Ada has a general concept of name overloading, which we saw earlier
in the section on :ref:`enumeration types <Intro_Ada_Enum_Types>`.

Let's take a simple example: it is possible in Ada to have functions that have
the same name, but different types for their parameters.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

This is a common concept in programming languages, called
:wikipedia:`overloading <Function_overloading>`, or name
overloading.

One of the novel aspects of Ada's overloading facility is the ability to
resolve overloading based on the return type of a function.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID)
                         return Integer;
       function Convert (Self : SSID)
                         return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Valid, will choose the
       --              proper Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    Note that overload resolution based on the type is allowed for both
    functions and enumeration literals in Ada - which is why you can have
    multiple enumeration literals with the same name. Semantically, an
    enumeration literal is treated like a function that has no parameters.

.. _Intro_Ada_Qualified_Expressions:

However, sometimes an ambiguity makes it impossible to resolve which
declaration of an overloaded name a given occurrence of the name refers to.
This is where a qualified expression becomes useful.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading_Error
    :class: ada-syntax-only, ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID)
                         return Integer;
       function Convert (Self : SSID)
                         return String;
       function Convert (Self : Integer)
                         return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Invalid, which convert
       --              should we call?

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ We specify that the
       --                       type of the
       --                       expression is SSID.

       --  We could also have declared a temporary

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Syntactically the target of a qualified expression can be either any expression
in parentheses, or an aggregate:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Qual_Expr

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

This illustrates that qualified expressions are a convenient (and sometimes
necessary) way for the programmer to make the type of an expression explicit,
for the compiler of course, but also for other programmers.

.. attention::
    While they look and feel similar, type conversions and qualified
    expressions are *not* the same.

    A qualified expression specifies the exact type that the target expression
    will be resolved to, whereas a type conversion will try to convert the
    target and issue a run-time error if the target value cannot be so
    converted.

    Note that you can use a qualified expression to convert from one subtype to
    another, with an exception raised if a constraint is violated.

    .. code-block:: ada

        X : Integer := Natural'(1);


Character types
---------------

As noted earlier, each enumeration type is distinct and
incompatible with every other enumeration type. However, what we did not
mention previously is that character literals are permitted as
enumeration literals. This means that in addition to the language's
strongly typed character types,
user-defined character types are also permitted:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Character_Example
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Character_Example is
       type My_Char is ('a', 'b', 'c');
       --  Our custom character type, an
       --  enumeration type with 3 valid values.

       C : Character;
       --  ^ Built-in character type
       --    (it's an enumeration type)

       M : My_Char;
    begin
       C := '?';
       --   ^ Character literal
       --     (enumeration literal)

       M := 'a';

       C := 65;
       --   ^ Invalid: 65 is not a
       --     Character value

       C := Character'Val (65);
       --  Assign the character at
       --  position 65 in the
       --  enumeration (which is 'A')

       M := C;
       --   ^ Invalid: C is of type Character,
       --     and M is a My_Char

       M := 'd';
       --   ^ Invalid: 'd' is not a valid
       --     literal for type My_Char
    end Character_Example;
