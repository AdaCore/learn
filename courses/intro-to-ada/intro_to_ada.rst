Introduction to Ada
===================

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

This document is a concise introduction to the Ada language, for people
who already have some experience with programming. It will go over the
important concepts of Ada in the order that made the most sense writing
it.

History
-------

In the seventies, the American department of Defense suffered from an
explosion of the number of languages, each team using a different set of
idioms/scripts. The DOD decided to solve this by issuing a request for
proposal to big companies of the time. The proposal that was selected by
the DOD in the end was Jean Ichbiah's proposal, on behalf of HoneyWell
bull.

The first standard for the language was issued in 1983, with revisions
being made in 1995, 2005 and 2012, each time adding major features to
the language.

This tutorial will focus on Ada 2012 as a whole, rather than teach
different versions of the language.

Ada today
---------

Today, Ada is mainly used in real-time/safety critical systems, with a
general focus on embedded systems. While Ada is and can be used as a
general purpose language, it will really shine in low level
applications:

-  Embedded systems with low memory requirements (no garbage collector
   allowed).
-  Direct interfacing with hardware.
-  Soft or hard real-time systems.
-  Low level systems programming.

This list is intentionally abstract. Today,  Ada has certain domains /
niches where it is used a lot, like Aerospace & Defense, civil aviation,
public transportation, etc. These are domains that require a high degree
of safety: a software defect in this kind of applications are not just an
annoyance, but may have severe consequences. Ada provides safety features
that allow for detecting defects at an early stage --- usually, at
compilation time or using static analysis tools. In addition, Ada can also
be used in to create applications in varied categories, such as:

-  `Video game programming <https://github.com/AdaDoom3/AdaDoom3>`__
-  `Real-time audio <http://www.electronicdesign.com/embedded-revolution/assessing-ada-language-audio-applications>`__
-  `Kernel modules <http://www.nihamkin.com/tag/kernel.html>`__

This is a non-comprehensive list that hopefully sheds light on which
kind of programming Ada is good at.

In terms of modern languages, the closest in terms of targets and level
of abstraction are probably
`C++ <https://fr.wikipedia.org/wiki/C%2B%2B>`__ and
`Rust <https://www.rust-lang.org/en-US/>`__.

Philosophy
----------

A word should be said about Ada's philosophy, because it is very
different from the one of a lot of other languages. In Ada it is
considered that:

-  Readability is more important than conciseness. Syntactically this
   shows through the fact that keywords are preferred to symbols, that no
   keyword is an abbreviation, etc.

-  Very strong typing. It is very easy to introduce new types in Ada,
   sometimes more than reusing existing ones.

    It is in the same ball-park as many functional languages in that regard,
    except that the programmer has to be much more explicit about typing, because
    there is almost no type inference.

.. AI for amiard: Develop a little bit. Put in the proper format when we have decided about it (issue #4)

-  Explicit is better than implicit: Although weirdly this is a
   `Python <www.TODOpython.com>`__ commandment, Ada takes it way further
   than any language we know of:

   -  There is mostly no structural typing, and most types need to be
      explicitly named by the programmer.

   -  As previously said, mostly no type inference.

   -  Semantics are very well defined, and undefined behavior is limited
      to an absolute minimum.

   -  The programmer can generally give a *lot* of information about
      what his program means to the compiler (and other programmers).
      This allows the compiler to be extremely helpful (read: strict)
      with the programmer.

During this course, we will explain the individual language features that
are building blocks for that philosophy.

Imperative language
===================

Ada is a multi-paradigm language, but at it's core, it contains a
simple, coherent procedural/imperative language akin to C or Pascal.

    One important distinction with a language like C is that statements
    and expressions are very clearly distinguished.

Hello world
-----------

Let's go over a very simple imperative Ada program:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       --  Print "Hello, World!" to the screen
       Put_Line ("Hello, World!");
    end Greet;

If you compile that source with the GNAT compiler, you will get a pretty
unsurprising result.

.. code-block:: sh

    $ gprbuild greet.adb
    using project file [...]_default.gpr
    Compile
       [Ada]          greet.adb
    Bind
       [gprbind]      greet.bexch
       [Ada]          greet.ali
    Link
       [link]         greet.adb

     $ ./greet
    Hello, World!
     %

There are several note worthy things in the above program:

-  A subprogram in Ada can be either a procedure or a function. A
   procedure, as used above, does not return a value when called. This is
   similar to functions in C/C++ that return :c:`void`. We'll see later how
   to declare functions in Ada.

-  :ada:`with` and :ada:`use` are used to reference external packages in
   the procedure. This is similar to ``import`` in various languages or
   roughly similar to :c:`#include` in C/C++.
   We'll see later how they work in detail. Here, we are requesting a
   standard library module which contains a procedure to print text on the
   screen: :ada:`Put_Line`.

-  ``Greet`` is a procedure, and the main entry point for our first
   program. Unlike in C or C++, it can be named anything you prefer. The
   builder will determine the entry point. In our simple example,
   ``gprbuild``, GNAT's builder, will use the file you passed as
   parameter.

-  :ada:`Put_Line` is a procedure, just like ``Greet``, except it is
   imported from the :ada:`Ada.Text_IO` module. It is the Ada equivalent
   of C's :c:`printf`.

-  Comments start with :ada:`--` and go to the end of the line. There is
   no multi-line comment syntax, that is, it is not possible to start a
   comment in one line and continue it in the next line. The only way to
   create multiple lines of comments in Ada is by using :ada:`--` on each
   line. For example:

.. code-block:: ada
    :class: ada-nocheck

    --  We start a comment in this line...
    --  and we continue on the second line...

Imperative language - If/Else
-----------------------------

Ada has an if statement. It is pretty unsurprising in form and function:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       I : Integer := 1;
    begin
       loop
          if I = 5 then
             Put_Line ("Hello, World!");
          end if;
          I := I + 1;
       end loop;
    end Greet;

As for the while loop, the Boolean condition must be of strict type
:ada:`Boolean`. Every relational operator in Ada returns a :ada:`Boolean`
by default.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Greet is
       I : Integer := 0;
    begin
       loop
          if I = 5 then
             exit;
             --  Exit can be unconditional
          elsif I = 0 then
             Put_Line ("Starting...");
          else
             Put_Line ("Hello, World!");
          end if;
          I := I + 1;
       end loop;
    end Greet;

What we can see here is that Ada features an :ada:`elsif` keyword. For
those interested, this is a way of avoiding the classical `dangling
else <https://en.wikipedia.org/wiki/Dangling_else>`__ problem.

Imperative language - Loops
---------------------------

Ada has three ways of specifying loops. None of them behave like the
C/Java/Javascript for-loop though. Their semantic is much more restricted,
which is in line with Ada's philosophy.

For loops
~~~~~~~~~

The first kind of loop is the for loop. It allows to iterate through a
discrete range.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       for I in 1 .. 10 loop
          Put_Line ("Hello, World!"); -- Procedure call
          --        ^ Procedure parameters
       end loop;
    end Greet;

A few things to note:

-  ``1 .. 10`` is a discrete range, from ``1`` to ``10`` included.

-  It is bound to the name ``I`` in the body of the loop.

-  Here, ``I`` is like a variable declaration, so you cannot refer to ``I``
   after the loop.

-  ``I`` is constant. You cannot change its value.

You cannot change the "step" of the loop (iterate two by two for
example), and if you want to iterate from ``10`` to ``1``, you have to
use the reverse keyword.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Greet is
    begin
       for I in reverse 1 .. 10 loop --  10 .. 1 would not work.
          Put_Line ("Hello, World!");
       end loop;
    end Greet;

For loops are more powerful and complicated than what we showcased here,
more on that later.

Bare loops
~~~~~~~~~~

Even though we started with the for loop, for familiarity, the purest,
form of loop in Ada is the bare loop. In some sense, every other loop kind
builds up on this one.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       I : Integer := 1; -- Variable declaration
       --  ^ Type
       --             ^ Default value
    begin
       loop
          Put_Line ("Hello, World!");
          exit when I = 5; --  Exit statement
          --        ^ Boolean condition
          I := I + 1;
       end loop;
    end Greet;

This example introduces a few new concepts and Ada specificities:

-  We see that we declared a variable, between the :ada:`is` and the
   :ada:`begin`. This constitutes a declarative region. In Ada, you can
   only declare objects, types, and anything that is considered a
   declaration, in a declarative region. Trying to declare a variable
   inline in the middle of your statements will result in a compilation
   error. More on that later.

-  The bare loop statement is introduced by the keyword :ada:`loop` on
   its own and, like every kind of loop statement, terminated by the
   combination of keywords :ada:`end loop`. On its own, it is an infinite
   loop. You can break out of it with an :ada:`exit` statement.

-  The operator for assignment is :ada:`:=`, and the one for equality is
   :ada:`=`. There is no way to confuse them, because as previously said,
   in Ada, statements and expressions are distinct, and expressions are
   not valid statements.

While loops
~~~~~~~~~~~

Ada has a last loop kind, while loops.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       I : Natural := 0;
    begin
       --  Condition. *Must* be of type Boolean (no Integers). Operator <
       --  returns a Boolean
       while I < 10 loop
          Put_Line ("Hello, World!");

          --  Assignment
          I := I + 1;
       end loop;
    end Greet;

Here we see what assignment to a variable looks like. There is no
``I++`` short form to increment, as there is in many languages.

Something important to note: Trying to treat any value other than a
Boolean as a Boolean condition will result in a compile time error. This
is a result of Ada's static strong typing.

Imperative language - Case statement
------------------------------------

Ada has a case statement, which is a very interesting beast, as it quite
differs from, for example, C/C++'s case statement.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       I : Integer := 0;
    begin
       loop
          --  Expression must be of a discrete type. All the
          --  values must be covered.
          case I is
             when 0 =>
                Put_Line ("Starting...");
                Put_Line ("No really");
                --  You can put several statements in a branch.
                --  There is no break.

             when 3 .. 5 =>
                Put_Line ("Hello");

             when 7 | 9 =>
                Put_Line ("World");

             when 10 =>
                exit;  -- This exits out of the loop ! Not equivalent to break !

             when others => Put_Line ("I in (1, 2, 6, 8)");
             --  ‘when others’ must be the last one and alone (if
             --  present)
          end case;
          I := I + 1;
       end loop;
    end Greet;

Notable points about Ada's case statement:

-  The parameter of the case statement needs to be of a discrete type.
   More later about what `discrete
   types <TODO:linktodiscretetypes>`__ are, but for the
   moment, it is enough to know that they cover integer and enumeration types.

-  Every possible value needs to be covered by the case statement. This
   will be checked at compile time. When using it on a value which has a
   cumbersome number of possible values, you will use the special
   :ada:`others` branch to cover the default case.

-  A value cannot be covered twice. This will also result in a compile
   time error.

-  There are syntactic sugars that you can use to cover several values
   in a branch, such as ranges (``3 .. 5``) and disjoint sets
   (``7 | 9``).

Imperative language - Declarative regions
------------------------------------------

We mentioned declarative regions before. Those are very important in
Ada. What is important to know at this stage:

-  In any subprogram (procedures for the moment), the region between the
   :ada:`is` and the :ada:`begin` is a declarative region.

-  You can potentially declare anything there: Variables, constants,
   types, other subprograms. This is valid for example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Nested is
       begin
          Put_Line ("Hello World");
       end Nested;
    begin
       Nested;
       --  Call to Nested
    end Main;

-  You cannot declare anything outside of a declarative region. If you
   need to scope variables in a subprogram, you can introduce a new
   declarative region with the :ada:`declare` block

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       loop
          Put_Line ("Please enter your name: ");

          declare
             Name : String := Get_Line;
             --               ^ Call to the Get_Line function
          begin
             exit when Name = "";
             Put_Line ("Hi " & Name & "!");
          end;

      --  Name is undefined here
       end loop;

      Put_Line ("Bye!");
    end Greet;

.. attention::
    The Get_Line function allows you to query input from the user, and get the
    result as a string. It is more or less equivalent to the :c:`scanf` C
    function.

    It returns a String, which, as we will see later, is an :ref:`Unconstrained
    array type <UnconstrainedArrayTypes>`. For the moment, it is sufficient to
    understand that you must declare the ``Name`` string variable and
    initialize it at the same time.

Imperative language - control expressions
-----------------------------------------

Ada, since the 2012 revision, features equivalent expressions for most
control statements except loops. We will go over those here because
they're control-flow, albeit not in the traditional form.

If expressions
~~~~~~~~~~~~~~~

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       A : Integer := 12;
       B : Integer := (if A = 12 then 15
                       elsif A = 13 then 15
                       else 18);
    begin
       null;  --  When a subprogram is empty, null statement is mandatory
    end Main;

Ada's if expression are similar to if statements. However, there are a few
differences that stems from the fact that it is an expression:

-  All branches' expressions must be of the same type
-  An else branch is mandatory.
-  They *must* be surrounded by parentheses, but only if the surrounding
   expression does not already contain them

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          --  Syntactically correct
          Put_Line (if I mod 2 = 0 then "Even" else "Odd");
       end loop;
    end Main;

Case expressions
~~~~~~~~~~~~~~~~~

Even more of a rarity, Ada also has case expressions. They work just as
you would expect.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line (case I is
                    when 1 | 3 | 5 | 7 | 9 => "Odd",
                    when 2 | 4 | 6 | 8 | 10 => "Even",
                    when others => "Cannot happen");
       end loop;
    end Main;

The syntax differs from case statements, because branches are separated
by commas. Also, something to note in the above example is that the
compiler does not know that ``I`` can only take values between 1 and 10,
so we still need to have an :ada:`others` branch. We will delve into
why when talking about `types <TODO:putlinkabouttypes>`__ in
more details.

Strongly typed language
=======================

Ada is a strongly typed language. It is interestingly modern in that
aspect: Strong static typing is going through a popularity rise, due to
multiple factors: Popularity of statically typed functional programming,
a big push from the academic community in the typing domain, many
practical languages with strong type systems emerging, etc.

What is a type?
---------------

In statically typed languages, a type is mainly (but not only) a
*compile time* construct. It is a construct commonly used in programming
languages to enforce invariants about the behavior of a program.
Invariants can be described as unchangeable properties that hold true for
all variable of a given type. Enforcing them allows for ensuring that
variables of a data type never have invalid values.

A type is used to reason about *values* a program manipulates. The aim
is to classify values by what you can accomplish with them, and this way
you can reason about the correctness of your values.

.. todo: expand/clarify

Integers
--------

A nice feature of Ada is that the user can define its own integer types.
In fact, the Integer types provided by the language are defined with the
same mechanisms. There is no "magical" built-in type in that regard,
which is unlike most languages, and arguably very elegant.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       --  Declare a signed integer type, and give the bounds
       type My_Int is range -1 .. 20;
       --                         ^ High bound
       --                   ^ Low bound

       --  Like variables, type declarations can only happen in
       --  declarative regions
    begin
       for I in My_Int loop
          Put_Line (My_Int'Image (I));
          --              ^ 'Image attribute, converts a value to a
          --                 String
       end loop;
    end Greet;

In this example, we showcase the creation of a signed integer type, and
several things we can do with them.

Every type definition in Ada (`well almost <TODOTASKTYPES>`__) starts
with the :ada:`type` keyword. After the type, we can see a range that
looks a lot like the ranges that we use in for loops, that defines the
low and high bound of the type. Every integer in the inclusive range of
the bounds is a valid value for the type.

    In Ada, Integer types are not specified with regards to their
    machine representation, but with regards to their range. The
    compiler will then choose the most appropriate representation.

Another interesting thing that we can notice in the above example is the
:ada:`My_Int'Image (I)` expresssion. In Ada, the
:ada:`Expr'Attribute (optional params)` notation is used for what is
called `attributes <TODOLINKATTRS>`__ in Ada. Attributes are built-in
operations on types or on values. They are accessed by using a :ada:`'`
(the tick sign).

Ada makes a few types available as "built-ins". :ada:`Integer` is one of
them. Here is how :ada:`Integer` is defined:

.. code-block:: ada
    :class: ada-nocheck

    type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);

:ada:`**` is the exponent operator, which means that the first valid
value for :ada:`Integer` is :math:`-2^{31}`, and the last valid value is
:math:`2^{31-1}`. In a fit of luck, this coincides with what you can fit
in a 32 bit signed integer on modern platforms :).

Operational semantics
~~~~~~~~~~~~~~~~~~~~~~

Unlike in unsafe languages like C and C++, Ada specifies that operations
on integers should be checked for overflows.

.. code-block:: ada

    procedure Main is
       A : Integer := Integer'Last;
       B : Integer;
    begin
       B := A + 5;
       --  This operation will overflow, eg. it will
       --  raise an exception at runtime.
    end Main;

However, mainly for efficiency reasons, overflow only happens at
specific boundaries, like assignment:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type My_Int is range 1 .. 20;
       A : My_Int := 12;
       B : My_Int := 15;
       M : My_Int := (A + B) / 2;
       --  No overflow here, overflow checks are done at
       --  specific boundaries.
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
    end Main;

Overflow will only be checked by the compiler at specific points in the
execution. The result, as we see above, is that you might have an operation
that overflows in an intermediate computation, but no error will be raised
because the final result does not overflow. For more information, see `the
detailed rules here <TODOLINKOVERFLOW>`__.

Unsigned types
--------------

Ada also features unsigned Integer types. They're called modular types in Ada
parlance. The reason for this designation is due to their behavior in case of
overflow: They simply "wrap around", as if a modulo operation was applied.

For machine sized modular types, this mimics the most common implementation
defined behavior of unsigned types. However, the main advantage is that
this works for any modular type:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Mod_Int is mod 2 ** 5;
       --                  ^ Max value is 32

       A : Mod_Int := 20;
       B : Mod_Int := 15;
       M : Mod_Int := A + B;
       --  No overflow here, M = 20 + 15 mod 32 = 3
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
    end Main;

Unlike in C/C++, since this behavior is guaranteed by the Ada specification,
you can rely on it to implement portable code. Also, being able to leverage the
wrapping on arbitrary bounds is very useful to implement certain algorithms and
data structures, such as
`ring buffers <https://en.m.wikipedia.org/wiki/Circular_buffer>`__.

Enumerations
------------

Enumeration types are another nicety of Ada's type system. Unlike C's enums,
they are *not* integers, and each new enum type is incompatible with other enum
types. Enum types are part of the bigger family of discrete types, which makes
them usable in certain situations that we will disclose later (`here
<TODOLINKTODISCRETEFEATURES`__, here and here) but one that we already know is
that you can use them as a target to a case expression.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);
       --  An enumeration type
    begin
       for I in Days loop
          case I is
             when Saturday .. Sunday =>
                Put_Line ("Week end!");

             --  Completeness checking on enums
             when others =>
                Put_Line ("Hello on " & Days'Image (I));
                --  'Image attribute, works on enums too
          end case;
       end loop;
    end Greet;

Enum types are powerful enough that, unlike in most languages, they're used to
represent the standard Boolean type, that is so defined:

.. code-block:: ada
    :class: ada-nocheck

    type Boolean is (True, False);

As mentioned previously, every "built-in" type in Ada is defined with facilities
generally available to the user.

Floating-point types
--------------------

Floating-point types
~~~~~~~~~~~~~~~~~~~~

As in most languages, Ada support floating-point types. The default
floating-point type is :ada:`Float`:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Demo is
       A : Float := 2.1;
    begin
       Put_Line ("The value of A is " & Float'Image (A));
    end Floating_Point_Demo;

The application will show that the value of ``A`` is ``2.1``.

All common operations that could be expected for floating-point types are
available, including retrieving the absolute-value and the power function.
For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Operations is
       A : Float := 2.1;
    begin
       A := abs (A - 4.1);
       Put_Line ("The value of A is " & Float'Image (A));
       A := A ** 2 + 1.0;
       Put_Line ("The value of A is " & Float'Image (A));
    end Floating_Point_Operations;

The value of ``A`` is 2.0 after the first operation and 5.0 after the
second operation.

In addition to :ada:`Float`, Ada offers data types with higher precision:
:ada:`Long_Float` and :ada:`Long_Long_Float`. However, the standard does
not indicate the exact precision of these types: it only guarantees that
the type :ada:`Long_Float`, for example, has at least the same precision
of :ada:`Float` or higher. In order to guarantee that a certain precision
requirement is met, we can define custom floating-point types, as we will
see in the next section.

Precision of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada allows for specifying the exact precision required for a
floating-point type. The precision is expressed in terms of decimal
digits. This guarantees that the operations on these custom types will
have at least the specified precision. The syntax for this is
:ada:`type T is digits <number_of_decimal_digits>`. In the background,
the compiler will choose a floating-point representation that matches the
required precision. For example:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Custom_Floating_Types is
       type T3  is digits 3;
       type T15 is digits 15;
       type T18 is digits 18;
    begin
       Put_Line ("T3  requires " & Integer'Image (T3'Size) & " bits");
       Put_Line ("T15 requires " & Integer'Image (T15'Size) & " bits");
       Put_Line ("T18 requires " & Integer'Image (T18'Size) & " bits");
    end Custom_Floating_Types;

In this example, the attribute :ada:`'Size` is used to retrieve the number
of bits used for the specified data type. As we can see by running this
example, the compiler allocates 32 bits for ``T3``, 64 bits for ``T15``
and 128 bits for ``T18``.

The number of digits specified in the data type is also used in the format
when displaying floating-point variables. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Custom_Floating_Types is
       type T3  is digits 3;
       type T18 is digits 18;

       C1 : constant := 1.0e-4;

       A : T3  := 1.0 + C1;
       B : T18 := 1.0 + C1;
    begin
       Put_Line ("The value of A is " & T3'Image (A));
       Put_Line ("The value of B is " & T18'Image (B));
    end Display_Custom_Floating_Types;

As expected, the application will display the variables according to
specified precision (1.00E+00 and 1.00010000000000000E+00).

Range of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ranges can also be specified floating-point types. The syntax is similar
to the one used for integer data types --- using the :ada:`range` keyword.
This simple example creates a new floating-point type based on the
:ada:`Float` for a normalized range between -1.0 and 1.0:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 1.0;
       Put_Line ("The value of A is " & T_Norm'Image (A));
    end Floating_Point_Range;

The application makes sure that the normalized range is observed for all
variables of this type. If the value is out of range, an exception is
raised. In this example, an exception (:ada:`Constraint_Error`) is raised
when assigning 2.0 to the variable ``A``:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range_Exception is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 2.0;
       Put_Line ("The value of A is " & T_Norm'Image (A));
    end Floating_Point_Range_Exception;

Ranges can also be specified for custom floating-point types. For example:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Range_Types is
       type T6_Inv_Trig  is digits 6 range -Pi / 2.0 .. Pi / 2.0;
    begin
       null;
    end Custom_Range_Types;

In this example, we are defining a type called ``T6_Inv_Trig``, which has
a range from :math:`-\pi/2` to :math:`\pi/2` with a minimum precision of 6
digits.

Strong typing
-------------

One thing that we have hinted at so far is that Ada is strongly typed. One
corollary of that is that different types of the same family are incompatible
with each other, as we can see in the following example:

.. code-block:: ada
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       --  Declare two signed types
       type Meters is range 0 .. 10_000;
       type Miles is range 0 .. 5_000;

       Dist_Imperial : Miles;
       --  Declare a constant
       Dist_SI : constant Meters := 100;
    begin
       --  Not correct: types mismatch
       Dist_Imperial := Dist_SI * 1609 / 1000;
       Put_Line (Miles'Image (Dist_Imperial));
    end Greet;

This is true for every distinct type. It also means that, in the general case,
an expression like :ada:`2 * 3.0` will trigger a compilation error. In a language
like C or Python, those expressions are made valid by implicit conversions. In
Ada, such conversions must be made explicit:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Conv is
       type Meters is range 0 .. 10_000;
       type Miles is range 0 .. 5_000;
       Dist_Imperial : Miles;
       Dist_SI : constant Meters := 100;
    begin
       Dist_Imperial := Miles (Dist_SI * 1609 / 1000);
       --               ^ Type conversion, from Meters to Miles
       --  Now the code is correct

       Put_Line (Miles'Image (Dist_Imperial));
    end;

Of course, we probably do not want to write the conversion code every time we
convert from meters to miles. The idiomatic Ada way in that case would be to
introduce conversion functions along with the types.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Conv is
       type Meters is range 0 .. 10_000;
       type Miles is range 0 .. 5_000;

       --  Function declaration, like procedure but returns a value.
       function To_Miles (M : Meters) return Miles is
       --                             ^ Return type
       begin
          return Miles (M * 1609 / 1000);
       end To_Miles;

       Dist_Imperial : Miles;
       Dist_SI : constant Meters := 100;
    begin
       Dist_Imperial := To_Miles (Dist_SI);
       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

This is also the first time we use a function. We will study `functions and
procedures <TODOSUBPROGRAMS>`__ in more details soon.

If you write a lot of numeric code, having to explicitly specify your
conversions all the time might seem painful at first, because your code might
end up containing a lot of conversions. However, this approach has some
advantages. For example:

- You can rely on the fact that no implicit conversion will ever happen in your
  numeric code.

.. admonition:: In other languages

    In C, for example, the rules for implicit conversions may not
    always be completely obvious. In Ada, however, the code will always do
    exactly what it seems to do. For example:

    .. code-block:: c

        int a = 3, b = 2;
        float f = a / b;

    This code will compile fine, but the result of ``f`` will be 1.0 instead
    of 1.5, because the compiler will generate an integer division (three
    divided by two) that results in one. The software developer must be
    aware of data conversion issues and use an appropriate casting:

    .. code-block:: c

        int a = 3, b = 2;
        float f = (float)a / b;

    In the corrected example, the compiler will convert both variables to
    their corresponding floating-point representation before performing the
    division. This will produce the expected result.

    This example is very simple and experienced C developers will probably
    notice this specific issue and correct it before it creates bigger
    problems. However, in more complex applications where the type
    declaration is not always visible --- e.g. when referring to elements of
    a :c:`struct` --- this situation might not always be evident and quickly
    lead to software defects that can be harder to find.

    The Ada compiler, in contrast, will always refuse to compile code that
    mixes floating-point and integer variables without explicit conversion.
    The following Ada code, based on the erroneous example in C, will not
    compile:

    .. code-block:: ada
        :class: ada-expect-compile-error

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    The offending line must be changed to :ada:`F := Float(A) / Float(B);`
    in order to be accepted by the compiler.

- You can use Ada's strong typing to help `enforce invariants
  <TODOLINKINVARIANTS>`__ in your code, as in the example above: Since Miles
  and Meters are two different types, you cannot mistakenly convert an instance
  of one to an instance of the other.

New types
---------

One particularity of Ada is that you can create new types based on existing
ones. This is very useful to define that a type is statically incompatible
with another type, to enforce strong typing.

.. code-block:: ada

   procedure Main is
      --  ID card number type, incompatible with Integer.
      type Social_Security_Number
      is new Integer range 0 .. 999_99_9999;
      --                   ^ Since a SSN has 9 digits max, and cannot be
      --                     negative, we enforce a validity constraint.

      SSN : Social_Security_Number := 323_44_9847;
      --                              ^ You can put underscores as formatting in
      --                                any number.

      Invalid : Social_Security_Number := -1;
      --                                  ^ This will cause a runtime error
      --                                    (and a compile time warning with
      --                                     GNAT)
   begin
      null;
   end Main;

You can redefine the range of validity of any type family: Floating point,
fixed point, enumerations ...

The syntax for enumerations uses the :ada:`range <range>` syntax:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday, Thursday,
                     Friday, Saturday, Sunday);

       type Weekend_Days is new Days range Saturday .. Sunday;
       --  New type, where only Saturday and Sunday are valid literals.
    begin
       null;
    end Greet;

Subtypes
--------

As we are starting to see, types are often used in Ada to enforce constraints
about the range of validity of values. However, sometimes it is desirable to
enforce constraints on some values, but one may not desire the static
enforcement brought by Ada types. This is where subtypes come into play.

Subtypes allow you to declare additional constraints on a type, but entities of
that subtype are still of the type the subtype derives from, and thus are valid
where an instance of the type is expected.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday, Thursday,
                     Friday, Saturday, Sunday);

       --  Declaration of a subtype
       subtype Weekend_Days is Days range Saturday .. Sunday;
       --                           ^ Constraint of the subtype

       M : Days := Sunday;

       S : Weekend_Days := M;
       --  No error here, Days and Weekend_Days are of the same type.
    begin
       for I in Days loop
          case I is
             --  Just like a type, a subtype can be used as a
             --  range
             when Weekend_Days =>
                Put_Line ("Week end!");
             when others =>
                Put_Line ("Hello on " & Days'Image (I));
          end case;
       end loop;
    end Greet;

Some subtypes are declared as part of the standard package in Ada, and are
available to you all the time:

.. code-block:: ada
    :class: ada-nocheck

    subtype Natural  is Integer range 0 .. Integer'Last;
    subtype Positive is Integer range 1 .. Integer'Last;

While subtypes of a type are statically compatible with each others,
constraints are enforced at runtime: If you violate the constraints of the
subtype, an exception will be raised at runtime, when the running program
detects the violation.

.. code-block:: ada
    :class: ada-run, ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday, Thursday,
                     Friday, Saturday, Sunday);

       subtype Weekend_Days is Days range Saturday .. Sunday;
       Day : Days := Saturday;
       Weekend : Weekend_Days;
    begin
       Weekend := Day;
       --         ^ Correct: Same type, subtype constraints are respected
       Weekend := Monday;
       --         ^ Wrong value for the subtype
       --           Compiles, but exception at runtime
    end Greet;

Records
=======

So far, all the types we have seen are what we can call base types: each
instance of one of those types represents a single piece of data. Now we are
going to study our first class of composite types: The record.

Records are a way to piece together several instances of other types. Each of
those instances will be given a name. The pair of a name to an instance of a
specific type is called a field, or a component.

Record type declaration
-----------------------

Here is an example of a simple record declaration:

.. code-block:: ada
    :class: ada-nocheck

    type Date is record
       --  The following declarations are components of the record
       Day   : Integer range 1 .. 31;
       Month : Month_Type;
       Year  : Integer range 1 .. 3000; --  You can add custom constraints on fields
    end record;

One thing we can notice is that fields look a lot like variable declarations,
except that they are inside of a record definition.

As with objects declarations, it is possible to specify additional constraints
when indicating the subtype of the field.

.. code-block:: ada
    :class: ada-nocheck

    type Date is record
       Day   : Integer range 1 .. 31;
       Month : Month_Type := January;
       --  This component has a default value
       Year  : Integer range 1 .. 3000 := 2012;
       --                                 ^ Default value
    end record;

Record components can also have default values. When declaring an instance of
the record, fields will be automatically set to this value. The value can be
any expression that is valid in the scope of definition of the record.

Aggregates
----------

.. code-block:: ada
    :class: ada-nocheck

    Today    : Date := (31, November, 2012);
    Birthday : Date := (Day => 30, Month => February, Year => 2010);
    --                  ^ By name

Records also have a literal notation that you can use, and that is showcased
above. This notation is called aggregate notation, and the literals are called
aggregates. They can be used in a variety of contexts that we will disclose
throughout the course, and one of those is to initalize records.

An aggregate is a list of values separated by commas and enclosed in
parentheses. It is a valid expression in any context where a value of the
record can be expected.

Values for the components can be specified positionally, as in the first
example, or by name, as in the second example. A mixture of positional and
named vamues is possible, but you cannot use a positional association after a
named one.

Component selection
-------------------

To access components of a record instance, an operation that is called
component selection, you use the following syntax:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Record_Selection is

       type Month_Type is
         (January, February, March, April, May, June, July,
          August, September, October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Month_Type;
          Year  : Integer range 1 .. 3000 := 2012;
       end record;

       Today    : Date := (31, November, 2012);

    begin
       Today.Day := 29;
       Put_Line ("Today is the " & Integer'Image (Today.Day)
                 & " of " & Month_Type'Image (Today.Month)
                 & ", " & Integer'Image (Today.Year));
    end Record_Selection;

Arrays
======

Another very important family of composite types is arrays.

Array type declaration
----------------------

Arrays in Ada are both pretty complex and pretty powerful. We will go over
their characteristics in detail, but let's start with one way of declaring one.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;

       type My_Int_Array is array (Index) of My_Int;
       --                                    ^ Type of elements
       --                          ^ Bounds of the array
       Arr : My_Int_Array := (2, 3, 5, 7, 11);
       --                    ^ Array literal, called aggregate in Ada
    begin
       for I in Index loop
          Put (My_Int'Image (Arr (I)));
          --                     ^ Take the Ith element
       end loop;
       New_Line;
    end Greet;

The first peculiarity that we can see in the above example is that we specify
the indexing type of the array, not its size. Here we declared an ``Index``
type ranging from ``1`` to ``5`` so the array will have 5 elements - that is,
bounds are inclusive.

This feature is pretty unique to Ada, and has interesting repercussions: You
can use any discrete type to index an array, including `Enum types
<TODOLINKENUMTYPES>`__. We will soon see what that means.

The second thing that we might notice is that querying an element of the array
at a given syntax uses the same syntax as the subprogram calls syntax, that is
the array followed by the index in parens.

What this means is that, in Ada, when you see an expression such as ``A (B)``,
whether it is a function call or an array subscript depends on what ``A``
designates.

Finally, the last thing of notice is how we initialize the array, with the
``(2, 3, 5, 7, 11)`` expression. This expression is called an aggregate in Ada,
and is a literal expression for an array, the same way that ``3`` is a literal
expression for an Integer. The notation is very powerful and has many
subtleties that we will gradually introduce. You can also have a detailed
overview of the notation `here <TODODETAILEDAGGREGATESADVANCED>__`.

Let's now delve into what it means exactly to be able to use any discrete type
to index into the array.

.. admonition:: In other languages

    Ada arrays have by-value semantics, which means that when you pass one, in
    terms of semantics you pass the whole array, not just a handle to it,
    unlike in a language like Python or Java. It also means that unlike in C or
    C++, arrays are not naked pointers in disguise.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 11 .. 15;
       --                  ^ Low bound can be any value
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index loop
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Greet;

The first repercussion is that the low bound of your array can be any value: In
the first example we constructed an array type whose first index is ``1``, but
in the example above we declare an array type whose first index is ``11``.

That's perfectly fine in Ada, and moreover you can see that since we use the
index type as a range to iterate on the array indices, the code using the array
does not need to change.

That leads us to an important consequence with regards to code dealing with
arrays: Since the lower bound can vary, it is considered best practice to never
assume/hard-code a low bound when iterating/using arrays in general. That means
the code above is good, because it uses the index type, but a for loop as
showcased below is bad practice:

.. code-block:: ada
    :class: ada-nocheck

    for I in 0 .. 20 loop
       Tab (I) := Tab (I) * 2;
    end loop;

Since we said above that you can use any discrete type to index an array, it
means that you can use enum types to index arrays.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Month_Duration is range 1 .. 31;
       type Month is (Jan, Feb, Mar, Apr, May, Jun,
                      Jul, Aug, Sep, Oct, Nov, Dec);

       type My_Int_Array is array (Month) of Month_Duration;
       --                          ^ Can use an enum as the
       --                            index

       Tab : constant My_Int_Array :=
       --    ^ constant is like a variable but cannot be
       --      modified
         (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
       --  Maps months to number of days

       Feb_Days : Month_Duration := Tab (Feb);
       --  Number of days in February
    begin
       for M in Month loop
          Put_Line
            (Month'Image (M) & " has "
             & Month_Duration'Image (Tab (M))  & " days.");
             --                                ^ Concatenation operator
       end loop;
    end Greet;


In the example above, we are:

- Creating an array type mapping months to month durations in days.

- Creating an array, and instanciating it with an aggregate mapping months to
  their actual durations in days.

- Iterating on the array, printing out the months, and the number of days for
  each.

Being able to use enums as indices is very useful to create mappings such as
this one, and is an often used feature in Ada.

Indexation
----------

We have already seen the syntax to get the elements of an array. There are
however a few more things to say about it.

First of all, as many things in Ada, this operation is strongly typed. If you
use a value of the wrong type to index the array, you will get a compile time
error.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index range 1 .. 5 loop
       --       ^ I is of type Index, ranges between 1 and 5
          Put (My_Int'Image (Tab (I)));
       --                         ^ Compile time error
       end loop;
       New_Line;
    end Greet;

Second, arrays in Ada are bounds checked. This means that if you try to access
an element outside of the bounds of the array, you will get a runtime error
instead of accessing random memory as in unsafe languages.

.. code-block:: ada
    :class: ada-run, ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index range 2 .. 6 loop
          Put (My_Int'Image (Tab (I)));
          --                      ^ Will raise an exception when
          --                      I = 6
       end loop;
       New_Line;
    end Greet;

Simpler array declarations
--------------------------

In the previous examples, we have always showcased the creation of a dedicated
index type for the array. While this can be useful, for typing and readability
purposes, sometimes you just want an anonymous range that you can use in that
context. Ada allows you to do that too.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       --                          ^ Subtype of Integer
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in 1 .. 5 loop
       --       ^ Likewise
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Greet;

In the preceding example, we declare the range of the array via the range
syntax, which will declare an anonymous subtype of integer and 8se it to index
the array.

This means that the type of the index is :ada:`Integer`. Coincidently, when you
use an anonymous range in a for loop as in the example above, the type of the
iteration variable is also :ada:`Integer`, which is why you can use ``I`` to
index ``Tab``.

You can also use a named subtype as bounds for an array.

Range attribute
---------------

We have said before that hard coding bounds (especially the lower bound) when
accessing or iterating on an array is generally a bad idea, and showcased how
to use the type/subtype of the array to iterate on its range in a for loop. The
problem with the above feature where we declare an anonymous range for the
array is that suddenly we have no name to refer to the range. Ada fixes that
via an attribute on array objects:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'Range loop
       --          ^ Gets the range of Tab
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Greet;

If you want more fine grained control, you can use the separate attributes
:ada:`'First` and :ada:`'Last`.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'First .. Tab'Last - 1 loop
       --          ^ Iterate on every index except the last
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Greet;

Of note, all those attributes, :ada:`'Range`, :ada:`'First` and :ada:`'Last`,
will work on array instances just as well as they work on discrete types and
subtypes themselves, enumerations included.

.. _UnconstrainedArrayTypes:

Unconstrained arrays
--------------------

Let's enter in one of the most complex and powerful areas of arrays in Ada.
Every array type we defined so far has a fixed size: Every instance of this
type will have the same size, and the same number of elements.

However, Ada also allows you to declare array types whose bounds are not fixed:
In that case, the bounds will need to be provided when instanciating the type.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       type Workload_Type is array (Days range <>) of Natural;
       --  Indefinite array type
       --                           ^ Bounds are of type Days,
       --                             but not known

       Workload : constant Workload_Type (Monday .. Friday) :=
       --                                 ^ Specify the bounds
       --                                   when declaring
          (Friday => 7, others => 8);
       --               ^ Default value
       --  ^ Specify element by name of index
    begin
       for I in Workload'Range loop
          Put_Line (Integer'Image (Workload (I)));
       end loop;
    end Greet;

The fact that the bounds of the array are not known is indicated by the ``Days
range <>`` syntax. Given a discrete type ``Discrete_Type``, while using
``Discrete_Type`` for the index specifies that we are going to use
this type as the type and the index and for the bounds, using ``Discrete_Type
range <>`` means that we use this type for the type of the index but that the
bounds are not yet constrained.

Those array types are thus called unconstrained, and the bounds need to be
provided at the moment of instantiation, as we can see in the example above.

The above example also shows more of the aggregate syntax: You can specify
associations by name, by giving the value of the index on the left side of an
arrow association. ``1 => 2`` hence means "assign value 2 to spot at index 1 in
my array". ``others => 8`` means "assign value 8 to every spot that wasn't
previously assigned in this aggregate".

.. admonition:: In other languages

    While superficially unconstrained arrays in Ada might look similar to
    variable length arrays in C, they are in reality much more powerful,
    because they're truly first class values in the language. You can pass them
    as parameters or return values in subprograms, and they carry their bounds
    inside the data type. This means that it is useless to pass the length of
    an array explictly along with the array, because it is accessible via the
    attributes demonstrated in the previous paragraph.

Predefined array type: String
-----------------------------

A recurring theme in our introduction to Ada types has been the way important
built-in types like :ada:`Boolean` or :ada:`Integer` have been built with the
same facilities that are available to the user. This is also true for strings:
The string type in Ada is a simple array.

Here is how the string type is defined in Ada:

.. amiard: TODO add definition of built in string type

The only built-in feature Ada adds to make strings more ergonomic is custom
literals, as we can see in the example below.

.. hint::
    String literals are just sugar on top of aggregates, so that in the
    following example, A and B are exactly similar declarations

    .. code-block:: ada

        package String_Literals is
            --  Those two declarations produce the same thing
            A : String (1 .. 11) := "Hello World";
            B : String (1 .. 11) := ('H', 'e', 'l', 'l', 'o', ' ',
                                     'W', 'o', 'r', 'l', 'd');
        end String_Literals;

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : String (1 .. 11) := "dlroW olleH";
       --        ^ Pre-defined array type.
       --          Component type is Character
    begin
       for I in reverse 1 .. 11 loop
          --    ^ Iterate in reverse order
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

However, what we can notice is that having to declare the bounds of the object
explicitly is a bit of a hassle: One needs to manually calculate the size of
the literal. Luckily Ada allows you to not do it.

Ada allows the user to omit the bounds when instanciating an unconstrained
array type, if the bounds can be deduced from the initialization expression.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : constant String := "Hello World";
       --                 ^ Bounds are automatically computed
       --                   from initialization value
    begin
       for I in reverse Message'Range loop
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Integer_Array is array (Natural range <>) of Integer;

       My_Array : constant Integer_Array := (1, 2, 3, 4);
       --                  ^ Bounds are automatically computed
       --                    from initialization value
    begin
        null;
    end Main;

Restrictions
------------

A very important point about arrays: Bounds *have* to be known when
instantiating the object. It is for example illegal to do the following.

.. code-block:: ada
   :class: ada-nocheck

    declare
       A : String;
    begin
       A := "World";
    end;

Also, while you of course change elements in the array, you cannot change its
size after it has been initialized, so this is also illegal:

.. code-block:: ada
    :class: ada-nocheck

    declare
       A : String := "Hello";
    begin
       A := "World"; --  Legal: Same size
       A := "Hello World"; --  Illegal: Different size
    end;

Also, while you can expect a warning for this kind of errors in very simple
cases like this one, it is impossible for a compiler to know in the general
case if you are assigning a value of the correct length, so this violation will
generally result in a runtime error.

.. attention::
    While we will learn more about this later, it is important to know right
    away that arrays are not the only types whose instances might be of unknown
    size at compile-time.

    Those objects are said to be of an *indefinite subtype*. Which means that
    the subtype size is not known at compile-time, but is dynamically computed
    at run-time.

    .. code-block:: ada

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Indefinite_Subtypes is
            function Get_Number return Integer is
            begin
                return Integer'Value(Get_Line);
            end Get_Number;

           A : String := "Hello";
           --  Indefinite subtype

           B : String (1 .. 5) := "Hello";
           --  Definite subtype

           C : String (1 .. Get_Number);
           --  Indefinite subtype (Get_Number's value is computed at run-time)
        begin
           null;
        end Indefinite_Subtypes;

Declaring arrays (2)
--------------------

While we can have, as we saw, array types whose exact representation is not
known at compile-time - which means, in effect, that their size and bounds are
determined at runtime - the component type of arrays needs to be of a definite
and constrained type.

Hence, if you need to declare, for example, an array of strings, the string
subtype used as component will need to have a fixed size.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Days is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       subtype Day_Name is String (1 .. 2);
       --  Subtype of string with known size

       type Days_Name_Type
       is array (Days) of Day_Name;
       --        ^ Type of the index
       --                 ^ Type of the element. Must be
       --                   definite

       Names : constant Days_Name_Type :=
         ("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");
       --  Initial value given by aggregate
    begin
       for I in Names'Range loop
          Put_Line (Names (I));
       end loop;
    end Show_Days;

Array slices
------------

One last interesting feature of Ada arrays that we're going to cover is array
slices: It is possible to take and use a slice of an array in an expression.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
        Buf : String := "Hello ...";

        Full_Name : String := "John Smith";
    begin
        Buf (7 .. 9) := "Bob";
        --  Careful! This works because the string on the right side is the
        --  same size as the replaced slice!

        Put_Line (Buf);  --  Prints "Hello Bob"

        Put_Line ("Hi " & Full_Name (1 .. 4)); --  Prints "Hi John"
    end;

As we can see above, you can also use a slice on the left side of an
assignment, to replace only part of an array.

A slice of an array is of the same type as the array, but has a different
subtype, constrained by the bounds of the slice.

.. attention::
    Slices will only work on one dimensional arrays.

Modular/Structured programming
==============================

So far, we managed to put our examples in the body of a procedure. Ada is
helpful in that regard, since it allows you to put any declaration in any
declarative part, which allowed us to declare our types and instances in the
body of the main procedure of our examples.

However, it is easy to see that this is not going to scale forever, and that
before long, we will need a better way to structure our programs into modular
and distinct units.

Ada encourages the separation of programs into multiple packages and
sub-packages, providing many tools to the programmer trying to fullfil his
quest of a perfectly organized code-base.

Packages
--------

Here is how you declare a package in Ada:

.. code-block:: ada

    package Week is

       --  This is a declarative part. You can put only
       --  declarations here, no statements

       type Days is (Monday, Tuesday, Wednesday,
          Thursday, Friday, Saturday, Sunday);

       type Workload_Type is array (Days range <>) of Natural;

       Workload : constant Workload_Type :=
          (Monday .. Thursday => 8,
           Friday => 7,
           Saturday | Sunday => 0);

    end Week;

And here is how you use it:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  References the Week package, and adds a dependency from the main unit
    --  to the week unit.

    procedure Main is
    begin
       for D in Week.Days loop
       --       ^ Reference to Week.Days enum type
          Put_Line
            ("Workload for day " & Week.Days'Image (D)
             & " is " & Natural'Image (Week.Workload (D)));
       end loop;
    end Main;

Packages are a way to make your code modular, separating your programs into
semantically significant units. Additionally they will allow the programmer to
generally compile his program faster by leveraging separate compilation.

While the :ada:`with` clause indicates a dependency, you can see in the example
above that you still need to prefix the use of entities from the week package
by the name of the package.

Accessing entities from a package uses the dot notation, :ada:`A.B`, which is
the same notation as the one to access records fields.

A :ada:`with` clause *has* to happen in the prelude of a compilation unit. It
is not allowed anywhere else.

.. admonition:: In other languages

    Packages look similar to, but are underneath very different from header
    files in C/C++.

    - The first and most important distinction is that packages are a language
      level mechanism, by opposition to includes, which are a functionality of the
      C preprocessor.

    - The first corollary of this design divergence is that the mechanism is a
      semantic inclusion mechanism, not a text inclusion mechanism. Hence, when
      you with a package, you say "I'm depending on this semantic unit" to the
      compiler, not "include this bunch of text in place here".

    - The consequences for the user, is that the content of a package cannot
      *vary* depending on where it has been included from, unlike in C/C++,
      where the existence of the preprocessor makes the exact content of what
      is included undecidable.

      This allows compilation/recompilation to be more efficient. It also
      allows tooling like IDEs to have correct information about the semantics
      of a program. In turn, this allows better tooling in general, and code
      that is more analyzable, even by humans.

.. admonition:: In the GNAT toolchain

    While the design of the Ada language does not mandate anything regarding the
    organization of files with regards to packages, eg. in theory you can put all
    your code in one file, or use your own scheme of organization, in practice in
    GNAT, you're supposed to put each top-level compilation unit in a separate
    file. In the example above, the ``Week`` package will go in a ``.ads`` file
    (for Ada specification), and the ``Main`` procedure will go in a ``.adb`` file
    (for Ada body).

Using a package
---------------

As we have seen above, we use the :ada:`with` clause to indicate a dependency on
another package. However, every use of entities coming from the ``Week``
package had to be prefixed by the full name of the package. It is possible to
make every entity of a package visible directly in the current scope, using the
:ada:`use` clause.

In fact, we have been using the :ada:`use` clause since almost the beginning of
this tutorial.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    --                    ^ Make every entity of the Ada.Text_IO package
    --                      directly visible.
    with Week;

    procedure Main is
       use Week;
       --  Make every entity of the Week package directly visible.
    begin
       for D in Week.Days loop
       --       ^ Reference to Week.Days enum type
          Put_Line  -- Put_Line comes from Ada.Text_IO.
            ("Workload for day " & Days'Image (D)
             & " is " & Natural'Image (Workload (D)));
       end loop;
    end Main;

As you can see in the example above:

- :ada:`Put_Line` is a subprogram that comes from the :ada:`Ada.Text_IO`
  package. We can use it directly because we have used the package at the top
  of the ``Main`` unit.

- Unlike :ada:`with` clauses, :ada:`use` clause can happen either in the prelude, or
  in any declarative zone. If used in a declarative zone, the :ada:`use` clause
  will have an effect in it's containing lexical scope.

Package body
------------

In the somewhat artificial example above, the ``Week`` package only has
declarations and no body. That's not a mistake: In a package specification,
which is what is showcased above, you cannot declare bodies. Those have to be
in the package body.

.. code-block:: ada

    package Week_2 is

       type Days is (Monday, Tuesday, Wednesday,
          Thursday, Friday, Saturday, Sunday);

       function Get_Workload (Day : Days) return Natural;

    end Week_2;

    package body Week_2 is

       --  The body contains additional declarations, not visible from the
       --  spec, or anywhere outside of the body
       type WorkLoad_Type is array (Days range <>) of Natural;
       Workload : constant Workload_Type :=
          (Monday .. Thursday => 8, Friday => 7, Saturday | Sunday => 0);

       function Get_Workload (Day : Days) return Natural is
       begin
          return Workload (Day);
       end;
    end Week_2;

Here we can see that the body of the ``Get_Workload`` function has to be
declared in the body. Coincidentally, introducing a body allows us to put the
``Workload_Type`` array type and the constant ``Workload`` in the body, and
make them inaccessible to the user of the ``Week`` package, providing a first
form of encapsulation.

This works because entities of the body are *only* visible in the body.

Subprograms
===========

Subprograms
-----------

So far, we used procedures a bit, mostly so we have a main body of code to
execute, and showed one function or two. Those entities belong to a category
called subprograms.

There are two kinds of subprograms in Ada, functions and procedures. The main
useful distinction between the two is that functions return a value, and
procedures don't.

.. code-block:: ada

    package Week_3 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       function Get_Workload (Day : Days) return Natural;
       --  We declare (but don't define) a function with one
       --  parameter, returning a Natural integer
    end Week_3;

As we saw before in the packages section, if you want to declare a subprogram
declaration to the package declaration. This declaration will not define the
function's body, only its name and profile (and hopefully some documentation),
so that clients of the package know how to use it.

Subprograms in Ada can expectedly have parameters. One syntactically important
note is that a subprogram which has no parameters does not have a parameter
section at all, following the form :ada:`procedure [name]` or
:ada:`function [name] return [type]`.

.. code-block:: ada

    package Week_4 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       function Get_Day_Name
          (Day : Days := Monday) return String;
       --                               ^ We can return any type,
       --                                 even indefinite ones
       --             ^ Default value for parameter
    end Week_4;

We learn two interesting things in the example above:

- Parameters can also have default values. When calling the subprogram, you can
  then omit parameters if they have a default value. A call to a subprogram
  without parameters does not need parentheses, similarly to when it is
  declared.

- The return type of a function can be anything. objects of size unknown at
  compile time are fine. Note that this also true for parameters.

.. admonition:: In other languages

    Returning variable size objects in languages lacking a garbage collector is
    a bit complicated implementation-wize, which is why C and C++ don't allow
    it, prefering to ressort to explicit dynamic allocation from the user.

    The problem is that explicit dynamic allocation is unsafe as soon as you
    want to collect unused memory. Ada's ability to return variable size
    objects will remove one use case for dynamic allocation, and hence, remove
    one potential source of bugs from your programs.

    Rust follows the C/C++ model, but with it's safe pointer semantics, allows
    for safety. However, dynamic allocation is still used. Ada can benefit from
    an eventual performance edge because it can use any model.

    .. amiard: TODO: say less or say more

As we showed briefly above, a subprogram declaration in a package declaration
must be completed by a subprogram body in the package body. For the ``Week``
package above, we could have the following body:

.. code-block:: ada

    package body Week_4 is
       --  Implementation of the Get_Day_Name function
       function Get_Day_Name (Day : Days := Monday) return String is
       begin
          return
            (case Day is
             when Monday => "Monday",
             when Tuesday => "Tuesday",
             when Wednesday => "Wednesday",
             when Thursday => "Thursday",
             when Friday => "Friday",
             when Saturday => "Saturday",
             when Sunday => "Sunday");
       end Get_Day_Name;
    end Week_4;

Subprogram calls
~~~~~~~~~~~~~~~~

We can then call our subprogram this way:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Week_4;

    procedure Show_Days is
    begin
       Put_Line (Week_4.Get_Day_Name);
       --             ^ Paramless call, value of Day parameter is Monday
       for Day in Week_4.Days loop
          Put_Line (Week_4.Get_Day_Name (Day));
          --                           ^ Regular param passing
       end loop;

       Put_Line (Week_4.Get_Day_Name (Day => Week_4.Friday));
       --                           ^ Named param passing
    end Show_Days;

Ada allows you to name the parameters when you pass them, whether they have a
default or not. There are some rules:

- Positional parameters come first.
- A positional parameter cannot follow a named parameter.

As a convention, people usually name parameters at the call site if the
function's corresponding parameters has a default value. However, it is also
perfectly acceptable to name every parameter if it makes the code clearer.

.. code-block:: ada

    package Week_5 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       type Language is (English, Italian);

       function Get_Day_Name
         (Day : Days; Lang : Language := English) return String;
    end Week_5;

    with Week_5; use Week_5;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       Put_Line (Get_Day_Name (Monday, Lang => Italian));
    end Main;

Function calls
~~~~~~~~~~~~~~

An important thing about function calls is that the return value of a function
call cannot be ignored in Ada.

If you want to call a function and do not need it's result, you will still need
to explicitly store it in a local variable.

.. code-block:: ada
    :class: ada-expect-compile-error

    function Quadruple (I : Integer) return Integer is
        function Double (I : Integer) return Integer is
        begin
           return I * 2;
        end Double;

       Res : Integer := Double (Double (I));
       --               ^ Calling the double function
    begin
       Double (I);
       --  ERROR: cannot use call to function "Double" as a statement

       return Res;
    end Quadruple;

.. admonition:: In GNAT

    In GNAT, with all warnings activated, it becomes even harder to ignore the
    result of a function, because unused variables will be flagged, so for
    example this code would not be valid:

    .. code-block:: ada
        :class: ada-syntax-only

        function Read_Int
           (Stream : Network_Stream; Result : out Integer) return Boolean;

        procedure Main is
            Stream : Network_Stream := Get_Stream;
            My_Int : Integer;
            B : Boolean := Read_Int (Stream, My_Int);  -- Warning here, B is never read
        begin
           null;
        end Main;

    You then have two solutions to silence this warning:

    - Either annotate the variable with a Unreferenced pragma, thusly:

    .. code-block:: ada
        :class: ada-nocheck

        B : Boolean := Read_Int (Stream, My_Int);
        pragma Unreferenced (B);

    - Either give the variable a name that contains any of the strings ``discard``
      ``dummy`` ``ignore`` ``junk`` ``unused`` (case insensitive)

Parameters modes
----------------

.. amiard TODO: Talk about early returns from procedures, and grouping
   parameters.
   Talk about the fact that order is unimportant with named parameters (with example)

So far we have seen that Ada is a safety focused language. There are many ways
this focus surfaces, but two important points are:

- Ada makes the user specify as much as possible about the behavior he expects
  out of his program, so that the compiler can warn or error-out if there is an
  inconsistency.

- Ada tries to discourage as much as possible the use of pointers and dynamic
  memory allocation, giving other ways to achieve goals that would have been
  accomplished this way in other languages.

Parameters modes are a feature that helps achieve the two design goals above. A
function parameter necessarily has a mode, that is one of the three following modes.

+---------------+--------------------------------------------+
| :ada:`in`     | Parameter can only be read, not written    |
+---------------+--------------------------------------------+
| :ada:`out`    | Parameter can only be written to, not read |
+---------------+--------------------------------------------+
| :ada:`in out` | Parameter can be both read and written     |
+---------------+--------------------------------------------+

The default mode for parameters is :ada:`in`, so, so far, every example we have
been showing has been using :ada:`in` parameters.

.. admonition:: Historically

    Functions and procedures were originally more different in philosophy.
    Before Ada 2005, one wasn't able to

Subprogram calls
----------------
In parameters
~~~~~~~~~~~~~

The first mode for parameter is the one we have been implicitly using so far.
Parameters passed using this mode cannot be modified, so that the following
program will cause an error:

.. code-block:: ada
    :class: ada-expect-compile-error

    procedure Swap (A, B : Integer) is
       Tmp : Integer;
    begin
       Tmp := A;

       --  Error: assignment to "in" mode parameter not allowed
       A := B;
       --  Error: assignment to "in" mode parameter not allowed
       B := Tmp;
    end Swap;

The fact that this is the default mode in Ada is in itself very important. It
means that mutation on parameters will not happen unless you explicitly change
the mode.

In-out parameters
~~~~~~~~~~~~~~~~~

To fix our code above, we can use an in-out parameter.

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure In_Out_Params is
       procedure Swap (A, B : in out Integer) is
          Tmp : Integer;
       begin
          Tmp := A;
          A := B;
          B := Tmp;
       end Swap;

       A : Integer := 12;
       B : Integer := 44;
    begin
        Swap (A, B);
        Put_Line (Integer'Image (A)); --  Prints 44
    end In_Out_Params;

An in out parameter will allow read and write access to the object passed as
parameter, so in the example above, we can see that A is modified after the
call to multiply.

.. attention::

    While in-out parameters look a bit like references in C++, or regular
    parameters in Java that are passed by-reference, the ARM does not mandate
    by reference passing for in out parameters in general.

    In general, it is better to think of modes as higher level than by-value
    versus by-reference semantics. For the compiler, it means that an array
    passed as an in parameter might be passed by reference under the covers,
    because it is more efficient (which does not change anything for the user
    since he cannot modify the original object anyway).  Conversely, an in-out
    parameter of a discrete type might be passed by copy because it is more
    efficient.

Out parameters
~~~~~~~~~~~~~~

Finally, the last mode is reserved for the cases where you only want to write
to a parameter. This allows to have parameters that behave a bit like return
values act for functions.

.. admonition:: In other languages

    Ada doesn't have a tuple construct, or by another means allows to return
    multiple values from a subprogram (except by declaring a full blown record
    type). Hence, a way to return multiple values from a subprogram is to use
    out parameters.

For example, a procedure reading integers from the network could have one of
the following prototypes:

.. code-block:: ada
    :class: ada-syntax-only

    procedure Read_Int
       (Stream : Network_Stream; Success : out Boolean; Result : out Integer);

    function Read_Int
       (Stream : Network_Stream; Result : out Integer) return Boolean;

While ideally reading an out variable before writing to it would trigger an
error, doing that in an exhaustive and precise fashion is hard. So the ARM just
mandates that out parameter be treated like uninitialized variables.

.. admonition:: In GNAT

    GNAT will warn you in simple cases of erroneous use of out parameters,
    emitting a warning. For example, the following program will emit a warning

    .. code-block:: ada

        procedure Outp is
           procedure Foo (A : out Integer) is
              B : Integer := A;
           begin
              A := B;
           end Foo;
        begin
           null;
        end Outp;

Forward declaration of subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we saw before, a subprogram can be declared without being defined, for
example in a package specification. This is possible in general, and can be
useful if you need subprograms to be mutually recursive, as in the example
below:

.. code-block:: ada
    :class: ada-run

    procedure Mutually_Recursive_Subprograms is
        procedure Compute_A (V : Natural);
        --  Forward declaration of Compute_A

        procedure Compute_B (V : Natural) is
        begin
           if V > 5 then
              Compute_A (V - 1);
           -- ^ Call to Compute_A
           end if;
    end Compute_B;

        procedure Compute_A (V : Natural) is
        begin
           if V > 2 then
              Compute_B (V - 1);
           -- ^ Call to Compute_B
           end if;
        end Compute_A;
    begin
       Compute_A (15);
    end Mutually_Recursive_Subprograms;

Nested subprograms
~~~~~~~~~~~~~~~~~~

A very useful functionality that is available for the programmer in Ada, and
that we already briefly mentioned, is that you can declare subprogram inside of
other subprograms.

This is a facility that is useful for two reasons:

- It allows you to organize your programs in a cleaner fashion: If you need a
  subprogram only as an helper for another subprogram, then the good practice
  is to nest it inside it.

- It allows you to share state easily in a controlled fashion, because the
  nested functions will have access to the parameters, and any local variables
  declared before them.

.. code-block:: ada
    :class: ada-run

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Lists is

       type String_Array is array (Positive range <>) of Unbounded_String;

       procedure Show_List (Strings : String_Array) is
          Item_Number : Positive := 1;

          procedure Show_Item (Item : Unbounded_String) is
          begin
             Put_Line (Positive'Image (Item_Number)
                       & ". " & To_String (Item));
             Item_Number := Item_Number + 1;
          end Show_Item;

       begin
          for Item of Strings loop
             Show_Item (Item);
          end loop;
       end Show_List;

       List : String_Array :=
         (To_Unbounded_String ("This"),
          To_Unbounded_String ("is"),
          To_Unbounded_String ("a"),
          To_Unbounded_String ("list"));
    begin
       Show_List (List);
    end Lists;

More about types
================

Aggregates: A primer
--------------------

So far, we have talked about, and showcased aggregates quite a bit. Now we will
try and be more comprehensive about them.

Aggregates are the mean by which you will describe literal values for composite
types in Ada. They are a very powerful notation that will allow you to avoid
writing  procedural code for the instantiation of your data structures in many
cases.

A basic rule that has to be followed when writing aggregates is that *every
component* of the described data type has to be specified, even components that
have a default value.

This means that the following code is incorrect:

.. code-block:: ada
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

There are a few shortcuts that you can use to make the notation more user
friendly:

- To tell the compiler to use the default value for a field, you can use the
  :ada:`<>` notation.

- You can also use the :ada:`|` operator to mention several disjoint components
  together.

- You can use the :ada:`others` qualifier to refer to every field that has not yet
  been mentionned, provided all those fields have the same type.

- You can use ranges to refer to ranges of indices in arrays.

However, beware, for array aggregates, as soon as you used named associations,
all associations have to be named !

.. code-block:: ada

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is array (Positive range <>) of Point;

       Origin : Point := (X | Y => <>);
       Origin_2 : Point := (others => <>);

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1 => (1, 2), 2 => (3, 4), 3 .. 20 => <>);
    end Points;

Overloading and qualified expressions
-------------------------------------

While we have mentioned it in the enumerations section TODOPUTLINK there is a
general concept of Ada which is the notion of overloading of names.

Let's take a simple example: It is possible in Ada to have functions that have
the same name, but different arguments.

.. code-block:: ada

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

This is a common concept in programming languages, that is called
`overloading <https://en.m.wikipedia.org/wiki/Function_overloading>`_, or name
overloading.

One of the pecularities of Ada is that it allows overloading on the return type
of a function.

.. code-block:: ada

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Valid, will choose the proper Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    This explains why you can have multiple enumeration literals with the same
    name: Return type overloading is allowed on both functions and enumerations
    in Ada. Actually, the ARM says that enumeration literals are treated like
    null-arity functions.

The problem is that, sometimes, there is an ambiguity such that the compiler
cannot actually resolve the names of an expression. This is where the qualified
expression becomes useful.

.. code-block:: ada
    :class: ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
       function Convert (Self : Integer) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Invalid, which convert should we call?

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ We specify that the type of the expression is
       --                       SSID.

       --  We could also have declared a temporary

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Syntactically the target of a qualified expression can be either any expression
in parentheses, either an aggregate:

.. code-block:: ada

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

    Qualified expressions need the type of the target expression that will be
    resolved to be exactly that specified, whereas type conversions will try to
    convert the target, issuing a run-time error if the conversion is deemed
    invalid at run-time.

Access types (pointers)
-----------------------

Pointers are a potentially dangerous construct with regards to safety in
programming languages, which is in opposition with Ada's stated goal.

There are two ways in which Ada does its best to shield programmers from the
dangers of pointers:

1. The first one, that we have already been studying all along, is to enable
   the programmer to not use them. Parameter modes, arrays, varying size types,
   are all constructs which allows the programmer to not use pointers, where he
   would have used them in C.

2. The second one is by making pointers construct as safe and restricted as
   possible, by default, allowing escape hatches when the programmer tells the
   language that he really knows what he is doing.

In this class, we will only teach the very basics of Ada pointers, which are
called accesses, because there are almost always better ways than to resort to
the advanced features directly.

If you need the unsafe features, you can learn more about those
`here <TODO_ACCESS_TYPES_ADVANCED_LINK>__`.

Here is how you declare a simple access type in Ada:

.. code-block:: ada

    package Dates is
       type Month_Type is (January, February, March, April, May, June, July,
                           August, September, October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Month_Type;
          Year  : Integer;
       end record;
    end Dates;

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc is access Date;
        --                      ^ Type you want to access/point to.

        D : Date_Acc := null;
        --              ^ Literal for "access to nothing"
        --  ^ Access to date
    end Access_Types;

So far we know how to:

- Declare an access type to a specific type
- Declare an instance of it
- Give it a value of :ada:`null`

In line with Ada's strong typing philosophy, if you declare a second access
type to the date type, the two access types will be incompatible with each
other, and you will need an explicit type conversion to convert from one to the
other:

.. code-block:: ada
    :class: ada-expect-compile-error

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc is access Date;
        type Date_Acc_2 is access Date;

        D  : Date_Acc := null;
        D2 : Date_Acc_2 := D;
        --                 ^ Invalid! Different types

        D3 : Date_Acc_2 := Date_Acc_2 (D);
        --                 ^ Valid with type conversion
    end Access_Types;

.. admonition:: In other languages

    In most other languages, pointer types are structurally, not nominally
    typed, like they are in Ada, which means that two pointer types will be the
    same as long as they share the same target type and accessibility rules.

    Not so in Ada, which takes some time getting used to. A seemingly simple
    problem that can cause pain is, if you want to have a canonical access to a
    type, where to declare it ? A very commonly used pattern is that if you
    need an access type to a specific type you 'own', you will declare it along
    with the type:

    .. code-block:: ada
        :class: ada-syntax-only

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Allocation (by type)
~~~~~~~~~~~~~~~~~~~~

Declaring access types is well, but we need a way to give instances of those
access types a meaningful value! You can allocate a value of an access type
with the :ada:`new` keyword in Ada.

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Allocate a new Date record
    end Access_Types;

If the type you want to allocate needs constraints, you can put them in the
subtype indication, just like you would do in a variable declaration:

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
       type String_Acc is access String;
       --                        ^ Access to unconstrained array type
       Msg : String_Acc;
       --    ^ Default value is null

       Buffer : String_Acc := new String (1 .. 10);
       --                                ^ Constraint required
    end Access_Types;

In some cases, allocating just by specifiying the type is not ideal though, so
Ada also allows you to allocate by value directly, specifying an expression via
a qualified expression:

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc := new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;


Dereferencing
~~~~~~~~~~~~~

The last missing piece to be able to use access types is how to use their
value. For that we need to dereference the pointer. Dereferencing a pointer
uses the :ada:`.all` syntax in Ada, but is only rarely necessary - in most
cases, the access wil be implicitly dereferenced for you:

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;

       D     : Date_Acc := new Date'(30, November, 2011);

       Today : Date := D.all;
       --              ^ Access dereference
       J     : Integer := D.Day;
       --                 ^ Implicit dereference for record and array components
       --                 Equivalent to D.all.day
    end Access_Types;

Other features
~~~~~~~~~~~~~~

As you might know if you have used pointers in C or C++, we are still missing
features that are considered fundamental to the use of pointers, such as:

- Pointers arithmetic (being able to dynamically change what a pointer is
  pointing to)

- Manual deallocation - what is called ``free`` or ``delete`` in C. This is
  considered an unsafe operation. It means that to stay into the realm of safe
  Ada, you need to never deallocate manually.

Those features exist in Ada, but are hidden behind specific standard library
APIs. You can read more about those in the `advanced course on memory
management <TODO_ACCESS_TYPES_ADVANCED_LINK>__`.

Mutually recursive types
------------------------

It is sometimes needed to implement loops in data structures, for example to
implement linked lists. This is doable in Ada, by forward declaring a type,
such as in the example below:

.. code-block:: ada

    package Simple_List is
       type Node;
       --  This is an incomplete type declaration, it must be
       --  completed in the same declarative region.

       type Node_Acc is access Node;

       type Node is record
          Content    : Natural;
          Prev, Next : Node_Acc;
       end record;
    end Simple_List;

More about records
------------------

Dynamically sized record types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have studied records, although not in-depth. Let's now detail a few
peculiarities of record types.

The first one is that the size of a record type does not need to be known at
compile time. This is a feature that is showcased in the example below:

.. code-block:: ada

    package Runtime_Length is
       function Compute_Max_Len return Natural;
    end Runtime_Length;

    with Runtime_Length; use Runtime_Length;

    package Var_Size_Record is
        Max_Len : constant Natural := Compute_Max_Len;
        --                            ^ Not known at compile time

        type Items_Array is array (Positive range <>) of Integer;

        type Growable_Stack is record
           Items : Items_Array (Positive'First .. Max_Len);
           Len   : Natural;
        end record;
        --  Person is a definite type, but size is not known at compile time.

        G : Growable_Stack;
    end Var_Size_Record;

The consequence of this is that it is completely fine to determine the size of
your records at run-time, the only enforced constraint being that this size
cannot change after the creation of the type.

Records with discriminant
~~~~~~~~~~~~~~~~~~~~~~~~~

In the section above, the size of the first name and last name fields is
determined once, at run-time, but every ``Person`` instance will be exactly the
same size. But maybe that's not what you as a user want to do. We saw that for
arrays in general, it is already possible to do that: An unconstrained array
type can designate any instance of such an array regardless of the size.

You can do that for records too, using a special kind of field that is called a
discriminant:

.. code-block:: ada

    package Var_Size_Record_2 is
        type Items_Array is array (Positive range <>) of Integer;

        type Growable_Stack (Max_Len : Natural) is record
        --                   ^ Discriminant. Cannot be modified once initialized.
           Items : Items_Array (Positive'First .. Max_Len);
           Len   : Natural := 0;
        end record;
        --  Growable_Stack is an indefinite type (like an array)
    end Var_Size_Record_2;

Discriminants, in their simple forms, are constant: You cannot modify them once
you have initialized the object. This intuitively makes sense since they
determine the size of the object.

Also, they make a type indefinite: Whether or not the discriminant is used or
not to change the size of the object, a type with a discriminant will be
indefinite if the discriminant is not specified:

.. code-block:: ada
    :class: ada-expect-compile-error

    package Test_Discriminants is
       type Point (X, Y : Natural) is record
          null;
       end record;

       P : Point;
       --  ERROR: Point is indefinite, so you need to specify the discriminants
       --  or give a default value

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  Those two declarations are equivalent.

    end Test_Discriminants;

This also means that, in the example above, you cannot declare an array of
points as Point is defined above, because the size of a Point is not known.

In most other regards, discriminants behave like regular fields: You have to
specify their values in aggregates, as seen above, and you can access their
values via the dot-notation.

.. code-block:: ada
    :class: ada-run

    with Var_Size_Record_2; use Var_Size_Record_2;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Print_Stack (G : Growable_Stack) is
       begin
          Put ("<Stack, items: [");
          for I in G.Items'Range loop
             exit when I > G.Len;
             Put (" " & Integer'Image (G.Items (I)));
          end loop;
          Put_Line ("]>");
       end Print_Stack;

       S : Growable_Stack := (Max_Len => 128, Items => (1, 2, 3, 4, others => <>), Len => 4);
    begin
       Print_Stack (S);
    end Main;

.. note:
    In the examples above, we used a discriminant to determine the size of an
    array, but it is not limited to that, and could be used, for example, to
    determine the size of another discriminated record.

Records with variant
~~~~~~~~~~~~~~~~~~~~

We introduced the concept of discriminants, and showcased how it enables people
to have records of varying size, by having components whose size vary depending
on the discriminant.

However, discriminants can also be used to make the shape of a record vary:

.. code-block:: ada

    package Variant_Record is
       type Expr;                       --  Forward declaration of Expr
       type Expr_Access is access Expr; --  Access to a Expr

       type Expr_Kind_Type is (Bin_Op_Plus, Bin_Op_Minus, Num);
       --  A regular enum

       type Expr (Kind : Expr_Kind_Type) is record
          --      ^ The discriminant is an enum
          case Kind is
             when Bin_Op_Plus | Bin_Op_Minus =>
                Left, Right : Expr_Access;
             when Num =>
                Val : Integer;
          end case;
          --  Variant part. Only one, at the end of the record
          --  definition, but can be nested
       end record;
    end Variant_Record;

The fields that are in a :ada:`when` branch will be only available when the
value of the discriminant is covered by the branch. In the example above, it
means that you will only be able to access the fields :ada:`Left` and
:ada:`Right` when the :ada:`Kind` is :ada:`Bin_Op_Plus` or :ada:`Bin_Op_Minus`.

If you try to access a field that is not valid for your record, a
:ada:`Constraint_Error` will be raised.

.. code-block:: ada
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Illegal, will compile but fail at runtime
    end Main;

Here is how you could write an evaluator for expressions above:

.. code-block:: ada
    :class: ada-run

    with Variant_Record; use Variant_Record;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       function Eval_Expr (E : Expr) return Integer is
         (case E.Kind is
          when Bin_Op_Plus => Eval_Expr (E.Left.all) + Eval_Expr (E.Right.all),
          when Bin_Op_Minus => Eval_Expr (E.Left.all) - Eval_Expr (E.Right.all),
          when Num => E.Val);

       E : Expr := (Bin_Op_Plus,
                    new Expr'(Bin_Op_Minus,
                              new Expr'(Num, 12), new Expr'(Num, 15)),
                    new Expr'(Num, 3));
    begin
       Put_Line (Integer'Image (Eval_Expr (E)));
    end Main;

.. admonition:: In other languages

    Ada's variant records are very similar to Sum types in functional languages
    such as OCaml or Haskell. The big difference is that the discriminant is a
    separate field in Ada, and that you can have several, whereas the 'tag' of
    the sum type is kind of built-in, and only accessible with pattern matching.

    There are other differences (you can have several discriminants in a
    variant record in Ada). Nevertheless, they allow the same kind of type
    modeling than sum types in functional languages.

    Compared to C/C++ unions, Ada variant records are more powerful in what
    they allow to express, and also checked at runtime, which makes them safer.

Fixed-point types
-----------------

Decimal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~

We have seen how to specify regular floating point types before.  In addition
to specifying the least required precision of a floating-point type, it is also
possible to go one step further and specify the exact accuracy of a
floating-point type. This category of data types is called decimal fixed-point
types.

The syntax for decimal fixed-point types is
:ada:`type T is delta <delta_value> digits <number_of_decimal_digits>`.
In this case, the :ada:`delta` and the :ada:`digits` will be used by the
compiler to derive a range. This will become clear in the next example.

We will use three attributes of the language in our example:

+------------------------+----------------------------------------------+
| Attribute Name         | Documentation                                |
+========================+==============================================+
| First                  | Returns the first value of the type          |
+------------------------+----------------------------------------------+
| Last                   | Returns the last value of the type           |
+------------------------+----------------------------------------------+
| Delta                  | Returns the delta value of the type          |
+------------------------+----------------------------------------------+

In the example below, we declare two data types: ``T3_D3`` and ``T6_D3``.
For both types, the delta value is the same: 0.001.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D3 is delta 10.0 ** (-3) digits 6;
    begin
       Put_Line ("The delta    value of T3_D3 is " & T3_D3'Image (T3_D3'Delta));
       Put_Line ("The minimum  value of T3_D3 is " & T3_D3'Image (T3_D3'First));
       Put_Line ("The maximum  value of T3_D3 is " & T3_D3'Image (T3_D3'Last));
       New_Line;
       Put_Line ("The delta    value of T6_D3 is " & T6_D3'Image (T6_D3'Delta));
       Put_Line ("The minimum  value of T6_D3 is " & T6_D3'Image (T6_D3'First));
       Put_Line ("The maximum  value of T6_D3 is " & T6_D3'Image (T6_D3'Last));
    end Decimal_Fixed_Point_Types;

When running the application, we see that the delta value of both
types is indeed the same: 0.001. However, because ``T3_D3`` is restricted
to 3 digits, its range is -0.999 to 0.999. For the ``T6_D3``, we have
defined a precision of 6 digits, so the range is -999.999 to 999.999.

Similar to the type definition using the :ada:`range` syntax, because we
have an implicit range, the application will check that the variables
contain values that are not out-of-range. Also, if the result of a
multiplication or division on decimal fixed-point types is smaller than
the delta value specified for the data type, the actual result will be
zero. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Smaller is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       A : T3_D3 := T3_D3'Delta;
       B : T3_D3 := 0.5;
    begin
       Put_Line ("The value of A     is " & T3_D3'Image (A));
       A := A * B;
       Put_Line ("The value of A * B is " & T3_D3'Image (A));
    end Decimal_Fixed_Point_Smaller;

In this example, the result of the operation :math:`0.001 * 0.5` is
0.0005. Since this value is not representable for the ``T3_D3`` type
because the delta value is 0.001, the actual value stored in variable
``A`` is zero.

Fixed-point types
~~~~~~~~~~~~~~~~~

Ordinary fixed-point types are similar to decimal fixed-point types.
The difference between them is in the delta value:
for decimal fixed-point types, it is based on the
power of ten, whereas for ordinary fixed-point types, it is based on the
power of two. Therefore, they are also called binary fixed-point types.

   FURTHERINFO: Ordinary fixed-point types can be thought of being closer
   to the actual representation on the machine, since hardware support for
   decimal fixed-point arithmetic is not widespread, while ordinary
   fixed-point types make use of the available integer arithmetic in the
   background.

The syntax for binary fixed-point types is
:ada:`type T is delta <delta_value> range <lower_bound> .. <upper_bound>`.
For example, we may define a normalized range between -1.0 and 1.0 as
following:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Normalized_Fixed_Point_Type is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0;
    begin
       Put_Line ("TQ31 requires " & Integer'Image (TQ31'Size) & " bits");
       Put_Line ("The delta    value of TQ31 is " & TQ31'Image (TQ31'Delta));
       Put_Line ("The minimum  value of TQ31 is " & TQ31'Image (TQ31'First));
       Put_Line ("The maximum  value of TQ31 is " & TQ31'Image (TQ31'Last));
    end Normalized_Fixed_Point_Type;

In this example, we are defining a 32-bit fixed-point data type for our
normalized range. When running the application, we notice that the upper
bound is close to one, but not exact one. This is a typical effect of
fixed-point data types --- you can find more details in this discussion
about the `Q format <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
We may also rewrite this code with an exact type definition:

.. code-block:: ada

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is delta 2.0 ** (-15) * Pi range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("T_Inv_Trig requires " & Integer'Image (T_Inv_Trig'Size)
                 & " bits");
       Put_Line ("The delta    value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Delta));
       Put_Line ("The minimum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'First));
       Put_Line ("The maximum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called ``T_Inv_Trig``,
which has a range from :math:`-\pi/2` to :math:`\pi/2`.

All standard operations are available for fixed-point types. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Point_Op is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);

       A, B, R : TQ31;
    begin
       A := 0.25;
       B := 0.50;
       R := A + B;
       Put_Line ("R is " & TQ31'Image (R));
    end Fixed_Point_Op;

As expected, ``R`` contains 0.75 after the addition of ``A`` and ``B``.

Character types
---------------

As we said before for enumeration types, each enumeration type is distinct and
incompatible with every other enumeration type. However, what we did not
mention is that Ada has character literals, that can be used as enumeration
literals too. This allows Ada to define its own strongly typed character types,
but also allows the user to define its own, as in the example below:

.. code-block:: ada
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Char is ('a', 'b', 'c');
       --  Our custom character type, an enum, with only 3 valid values.

       C : Character;
       --  ^ Built-in character type (it's an enum)

       M : My_Char;
    begin
       C := '?';
       --   ^ Character literal (enumeration literal)

       M := 'a';

       C := 64;
       --   ^ Invalid: 64 is not an enumeration literal

       C := Character'Val (64);
       --  Assign the character at position 64 in the enum (which is 'A')

       M := C;
       --   ^ Invalid: C is of invalid type for A

       M := 'd';
       --   ^ Invalid: 'd' is not a valid literal for type My_Char
    end Greet;

Privacy
=======

One of the main principles in modular programming, that has later become one of
the main principles behind the dominant interpretation of object oriented
programming, is `encapsulation <https://en.wikipedia.org/wiki/Encapsulation_(computer_programming)>`__.

Encapsulation, briefly, is the concept that the implementer of a piece of
computer software will distinguish between the public interface and the private
implementation for his code.

This is not only applicable to software libraries but can happen everywhere
inside of a project where you want to have some abstraction.

In Ada, the granularity of encapsulation is a bit different from most object
oriented languages, because privacy is generally specified at the package
level.

Basic encapsulation
-------------------

.. code-block:: ada
    :class: ada-expect-compile-error

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
    end Encapsulate;

    with Encapsulate;

    procedure Main is
    begin
       Encapsulate.Hello;
       Encapsulate.Hello2;
       --  Invalid: Hello2 is not visible
    end Main;

Abstract data types
-------------------

With this high level granularity, it might not seem obvious how to hide the
implementation details of a type. Here is how it is done in Ada:

.. code-block:: ada

    package Stacks is
       type Stack is private;
       --  Declare a private type: You cannot depend on its
       --  implementation. You can only assign and test for
       --  equality.

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private

       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is array (Stack_Index) of Natural;

       type Stack is record
          Top : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

In the above example, we define a stack type in the public part, but say that
the exact representation of that type is private.

Then, in the private part, we define the exact representation of that type. We
can also declare other types that will be used as helpers for our main public
type. This is useful since declaring helper types is so common in Ada.

A few words about terminology:

- The Stack type as viewed from the public part is called the partial view of
  the type. This is what clients have access to.

- The Stack type as viewed from the private part or the body of the package is
  called the full view of the type. This is what implementers have access to.

From the point of view of the client, only the public part is important, and
the private part could as well not exist. It makes it very easy to read
linearly the part of the package that is important for you.

.. code-block:: ada
    :class: ada-nocheck

    --  No need to read the private part to use the package
    package Stacks is
       type Stack is private;

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private
       ...
    end Stacks;

Here is how the ``Stacks`` package would be used:

.. code-block:: ada

    --  Example of use
    with Stacks; use Stacks;

    procedure Test_Stack is
       S : Stack;
       Res : Integer;
    begin
       Push (S, 5);
       Push (S, 7);
       Pop (S, Res);
    end Test_Stack;

Limited types
-------------

Ada has a facility called limited types. The particularity of limited types is that they cannot be copied or compared.

.. code-block:: ada

    package Stacks is
       type Stack is limited private;
       --  Limited type. Cannot assign nor compare.

       procedure Push (S : in out Stack; Val : Integer);
       procedure Pop (S : in out Stack; Val : out Integer);
    private
       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is array (Stack_Index) of Natural;

       type Stack is limited record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

    with Stacks; use Stacks;

    procedure Main is
       S, S2 : Stack;
    begin
       S := S2;
       --  Illegal: S is limited.
    end Main;

This is useful because for some complex data types, copy is a complicated
operation: You might have data inside your record that needs to be treated
specially when you copy it (for example doing a deep copy).

Ada allows you to implement special semantics for those operations via
`controlled types <todo_link_to_controlled_types>`__. However, controlled types
are complicated to get right and, in some cases, to simplify the problem, and
the way people will use your API, you can sidestep the issue entirely and say
"Users are not allowed to copy this data type".

One example is the ``File_Type`` from the ``Ada.Text_IO`` package. The
designers of the standard library decided that rather than allow people to copy
instances of File_Type, making it limited was actually easier, and did fit the
semantics of the API quite well.

Generics
========

Introduction
------------

Generics are used for metaprogramming in Ada. They are useful for abstract
algorithms that share common properties.

Generics can be used for subprograms or packages. A generic is declared
by using the keyword :ada:`generic`. For example:

.. raph-amiard: We are lacking a definition/link of metaprogramming.

.. code-block:: ada

    procedure Show_Simple_Generic is

       generic
          type T is private;
          --  Declaration of formal types and objects
       procedure Operator (X : in out T);
       --  This could be one of the following:
       --  <procedure | function | package>

       procedure Operator (X : in out T) is null;

    begin
       null;
    end Show_Simple_Generic;

Formal type declaration
~~~~~~~~~~~~~~~~~~~~~~~

Formal types are abstractions of a specific type. We may, for example,
want to create an algorithm that works on any integer type, or even on
any type at all, independently of being a numeric type or not. The
following example declares a formal type ``T`` for the ``Set`` procedure.

.. code-block:: ada

    procedure Show_Formal_Type_Declaration is

       generic
          type T is private;
          --  T is a formal type that indicates that any type can be used,
          --  be it a numeric type or, for example, a record.
       procedure Set (E : in T);

       procedure Set (E : in T) is null;

    begin
       null;
    end Show_Formal_Type_Declaration;

The declaration of ``T`` as :ada:`private` indicates that any type can be
mapped to it. These are some examples of formal types:

+-------------------------+---------------------------------------------+
| Formal Type             | Format                                      |
+=========================+=============================================+
| Any type                | :ada:`type T is private;`                   |
+-------------------------+---------------------------------------------+
| Any discrete type       | :ada:`type T is (<>);`                      |
+-------------------------+---------------------------------------------+
| Any floating-point type | :ada:`type T is digits <>;`                 |
+-------------------------+---------------------------------------------+

Formal object declaration
~~~~~~~~~~~~~~~~~~~~~~~~~

Formal objects are similar to subprogram parameters. Also, they can make
use of formal types declared in the formal specification. For example:

.. code-block:: ada

    procedure Show_Formal_Object_Declaration is

       generic
          type T is private;
          X : in out T;
          --  X can be used in the Set procedure
       procedure Set (E : in T);

       procedure Set (E : in T) is null;

    begin
       null;
    end Show_Formal_Object_Declaration;

Formal objects can be either just input parameters or use the
:ada:`in out` mode.

Generic body definition
~~~~~~~~~~~~~~~~~~~~~~~

For the body declaration of a generic subprogram or package, we don't
repeat the :ada:`generic` keyword: we simply start with the actual
declaration and make use of the generic types and objects that we
declared. For example:

.. code-block:: ada

    procedure Show_Generic_Body_Definition is

       generic
          type T is private;
          X : in out T;
       procedure Set (E : T);

       procedure Set (E : T) is
       --  Body definition: "generic" keyword is not used
       begin
          X := E;
       end Set;
    begin
       null;
    end Show_Generic_Body_Definition;

Generic instantiation
~~~~~~~~~~~~~~~~~~~~~

Generic subprograms or packages cannot be used directly. Instead, they
need to be instantiated. The instantiation is done by using the :ada:`new`
keyword, as illustrated in the following example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Generic_Instantiation is

       generic
          type T is private;
          X : in out T;
          --  X can be used in the Set procedure
       procedure Set (E : T);

       procedure Set (E : T) is
       begin
          X := E;
       end Set;

       Main    : Integer := 0;
       Current : Integer;

       procedure Set_Main is new Set (T => Integer,
                                      X => Main);
       --  Here, we map the formal parameters to actual types and objects.
       --
       --  The same approach can be used to instantiate functions or
       --  packages, e.g.:
       --  function Get_Main is new ...
       --  package Integer_Queue is new ...

    begin
       Current := 10;

       Set_Main (Current);
       Put_Line ("Value of Main is " & Integer'Image (Main));
    end Show_Generic_Instantiation;

In the example above, we instantiate the procedure ``Set`` by mapping the
formal parameters ``T`` and ``X`` to actual existing elements: the
:ada:`Integer` type and the ``Main`` variable.


Generic packages
~~~~~~~~~~~~~~~~

The previous examples focused on generic subprograms. In this section, we
will look into generic packages. In general, the syntax is not different
from the one used for generic subprograms: it starts with the :ada:`generic`
keyword and continues with formal declarations. The only difference is
that a :ada:`package` is specified instead of a subprogram.

This is an example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Generic_Package is

       generic
          type T is private;
       package Element is

          procedure Set (E : T);
          procedure Reset;
          function Get return T;
          function Is_Valid return Boolean;

          Invalid_Element : exception;

       private
          Value : T;
          Valid : Boolean := False;
       end Element;

       package body Element is

          procedure Set (E : T) is
          begin
             Value := E;
             Valid := True;
          end Set;

          procedure Reset is
          begin
             Valid := False;
          end Reset;

          function Get return T is
          begin
             if not Valid then
                raise Invalid_Element;
             end if;
             return Value;
          end;

          function Is_Valid return Boolean is (Valid);

       end Element;

       package I is new Element (T => Integer);

       procedure Display_Initialized is
       begin
          if I.Is_Valid then
             Put_Line ("Value is initialized");
          else
             Put_Line ("Value is not initialized");
          end if;
       end Display_Initialized;

    begin
       Display_Initialized;

       Put_Line ("Initializing...");
       I.Set (5);
       Display_Initialized;
       Put_Line ("Value is now set to " & Integer'Image (I.Get));

       Put_Line ("Reseting...");
       I.Reset;
       Display_Initialized;

    end Show_Generic_Package;

In the example above, we create a very simple container named ``Element``
for just a single element. This container keeps track whether the element
has been initialized or not. After the package definition, we create the
instance ``I`` of the ``Element``. We can then use the instance by calling
the package subprograms (e.g.: ``Set``, ``Get`` and ``Reset``).

Formal subprograms
~~~~~~~~~~~~~~~~~~

In addition to formal types and objects, we can also declare formal
subprograms or packages. This course only describes formal subprograms.
Formal packages are discussed in the advanced course.

In order to declare a formal subprogram, we make use of the :ada:`with`
keyword. In the example below, we declare a formal function
(``Comparison``) that is used by the generic procedure ``Check``.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Formal_Subprogram is

       generic
          Description : String;
          type T is private;
          with function Comparison (X, Y : T) return Boolean;
       procedure Check (X, Y : T);

       procedure Check (X, Y : T) is
          Result : Boolean;
       begin
          Result := Comparison (X, Y);
          if Result then
             Put_Line ("Comparison (" & Description &
                       ") between arguments is OK!");
          else
             Put_Line ("Comparison (" & Description &
                       ") between arguments is not OK!");
          end if;
       end Check;

       A, B : Integer;

       procedure Check_Is_Equal is new Check (Description => "equality",
                                              T           => Integer,
                                              Comparison  => Standard."=");
       --  Here, we are mapping the standard equality operator for Integer
       --  types to the Comparison function
    begin
       A := 0;
       B := 1;
       Check_Is_Equal (A, B);
    end Show_Formal_Subprogram;

Examples of using generics
--------------------------

In this section, we will look into examples and strategies for abstracting
algorithms using generics.

Application: ADTs
~~~~~~~~~~~~~~~~~

An important application of generics is to model abstract data types
(ADTs). In fact, Ada includes a library with all sorts of ADTs using
generics: :ada:`Ada.Containers`.

A typical example of an ADT is a stack:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Stack is

       generic
          Max : Positive;
          type T is private;
       package Stacks is

          type Stack is limited private;

          function Is_Empty (S : Stack) return Boolean;

          function Pop (S : in out Stack) return T;

          procedure Push (S : in out Stack;
                          V : T);

       private

          type Stack_Array is array (Natural range <>) of T;

          type Stack is record
             Container : Stack_Array (1 .. Max);
             Top       : Natural := 0;
          end record;

       end Stacks;

       package body Stacks is

          function Is_Empty (S : Stack) return Boolean is
            (S.Top < S.Container'First);

          function Pop (S : in out Stack) return T is
          begin
             return X : T do
                X     := S.Container (S.Top);
                S.Top := S.Top - 1;
             end return;
          end Pop;

          procedure Push (S : in out Stack;
                          V : T) is
          begin
             S.Top               := S.Top + 1;
             S.Container (S.Top) := V;
          end Push;

       end Stacks;

       package Integer_Stacks is new Stacks (Max => 10,
                                             T   => Integer);
       use Integer_Stacks;

       Values : Integer_Stacks.Stack;

    begin
       Push (Values, 10);
       Push (Values, 20);

       Put_Line ("Last value was " & Integer'Image (Pop (Values)));
    end Show_Stack;

In this example, we first create a generic stack package (``Stacks``).
Then, we instantiate it in order to create a stack for 10 positions of
integer values.

Abstracting a swap algorithm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's first look into a simple procedure that swaps variables of the type
``Color``:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Non_Generic_Swap_Colors is
       type Color is (Black, Red, Green, Blue, White);

       procedure Swap_Colors (X, Y : in out Color);

       procedure Swap_Colors (X, Y : in out Color) is
          Tmp : constant Color := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap_Colors;

       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));
    end Test_Non_Generic_Swap_Colors;

In this example, ``Swap_Colors`` can only be used for the ``Color`` type.
However, a swapping algorithm can theoretically be used for any type, be
it an enumeration or a complex record type with many elements. The
algorithm itself is the same, just the types are different. Also, we don't
want to duplicate the implementation for swapping variables of :ada:`Integer`
type, for example. Therefore, such an algorithm is an perfect candidate
for abstraction using generics.

In the example below, we create a generic version of the ``Swap_Colors``
and name it ``Generic_Swap``. This generic version can work on any type.
This is achieved by the declaration of the formal type ``T``.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Swap_Colors is
       generic
          type T is private;
       procedure Generic_Swap (X, Y : in out T);

       procedure Generic_Swap (X, Y : in out T) is
          Tmp : constant T := X;
       begin
          X := Y;
          Y := Tmp;
       end Generic_Swap;

       type Color is (Black, Red, Green, Blue, White);

       procedure Swap_Colors is new Generic_Swap (T => Color);

       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));
    end Test_Swap_Colors;

As we can see in the example, we can create the same ``Swap_Colors``
procedure as we had in the non-generic version of the algorithm by
declaring it as an instance of generic ``Generic_Swap`` procedure. As an
argument to the ``Generic_Swap`` procedure, we define that the ``T`` type
will be mapped to the ``Color`` type.

Abstracting a reversing algorithm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The previous example using a swapping algorithm is one of the simplest
examples of generic algorithms. Now, we will look into an algorithm for
reversing elements of an array. First, let's start with a non-generic
version of the algorithm that works specifically for the ``Color`` type:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Non_Generic_Reverse_Colors is
       type Color is (Black, Red, Green, Blue, White);

       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array (X : in out Color_Array);

       procedure Reverse_Color_Array (X : in out Color_Array) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : Color;
                X_Left  : Color renames X (I);
                X_Right : Color renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_Color_Array;

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_Color_Array (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Non_Generic_Reverse_Colors;

The procedure ``Reverse_Color_Array`` takes an array of colors and starts
by swapping the first and last elements of the array, and continues doing
that with the next elements until the middle of array. At this point, the
whole array has been reversed, as we can see in the text output of the
test application.

In order to abstract this procedure, we will declare formal types for
three components of the algorithm:

    - the elements of the array (``Color`` type in the example)

    - the range used for the array (``Integer`` range in the example)

    - the actual array type (``Color_Array`` type in the example)

This is a generic version of the algorithm:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Reverse_Colors is
       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
       procedure Generic_Reverse_Array (X : in out Array_T);

       procedure Generic_Reverse_Array (X : in out Array_T) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Generic_Reverse_Array;

       type Color is (Black, Red, Green, Blue, White);
       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array is new
         Generic_Reverse_Array (T => Color, Index => Integer, Array_T => Color_Array);

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_Color_Array (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

As mentioned above, we're abstracting three components of the algorithm:

    - the ``T`` type abstracts the elements of the array

    - the ``Index`` type abstracts the range used for the array

    - the ``Array_T`` type abstracts the array type and makes use of the
      formal declarations of the ``T`` and ``Index`` types.

Abstracting the test application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, we've focused only on abstracting the reversing
algorithm. However, we could have decided to also abstract our little
test application. This could be useful if we, for example, decide to
test other procedures that change elements of an array.

In order to achieve this, we have to abstract some elements. We
will therefore declare the following formal parameters:

    - ``S``: the string containing the array name

    - a function ``Image`` that converts an element of type ``T`` to a
      string

    - a procedure ``Test`` that performs some operation on the array

Note that ``Image`` and ``Test`` are examples of formal subprograms.
Also, note that ``S`` is an example of a formal object.

This is a version of the test application that makes use of the generic
``Perform_Test`` procedure:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Reverse_Colors is

       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
       procedure Generic_Reverse_Array (X : in out Array_T);

       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
          S : String;
          with function Image (E : in T) return String is <>;
          with procedure Test (X : in out Array_T);
       procedure Perform_Test (X : in out Array_T);

       procedure Generic_Reverse_Array (X : in out Array_T) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Generic_Reverse_Array;

       procedure Perform_Test (X : in out Array_T) is
       begin
          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;

          New_Line;
          Put_Line ("Testing " & S & "...");
          New_Line;
          Test (X);

          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;
       end Perform_Test;

       type Color is (Black, Red, Green, Blue, White);
       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array is new
         Generic_Reverse_Array (T       => Color,
                                Index   => Integer,
                                Array_T => Color_Array);

       procedure Perform_Test_Reverse_Color_Array is new
         Perform_Test (T       => Color,
                       Index   => Integer,
                       Array_T => Color_Array,
                       S       => "My_Color",
                       Image   => Color'Image,
                       Test    => Reverse_Color_Array);

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors;

In this example, we create the procedure
``Perform_Test_Reverse_Color_Array`` as an instance of the generic
procedure (``Perform_Test``). Note that:

    - For the formal ``Image`` function, we make use of the ``'Image``
      attribute of the ``Color`` type

    - For the formal ``Test`` procedure, we reference the
      ``Reverse_Array`` procedure from the package.

Exceptions
==========

Ada uses exceptions for error handling. Ada's exceptions are not checked, which
means that a subprogram does not have to declare every exception kind that it
can potentially raise.

Exception declaration
---------------------

Ada exceptions are not types, they're objects, which is something peculiar
about them if you're used to the way Java or Python does it. Here is how you
declare an exception:

.. code-block:: ada

    package Exceptions is
        My_Except : exception;
        -- Like an object. *NOT* a type !
    end Exceptions;

Even though they're objects, you're going to use each declared exception object
as a "kind" or "family" of exceptions.

Raising an exception
--------------------

To raise an exception of our newly declared exception kind, here is how you do
it:

.. code-block:: ada

    with Exceptions; use Exceptions;

    procedure Main is
    begin
        raise My_Except;
        --  Execution of current control flow abandoned, an exception of kind
        --  "My_Except" will bubble up until it is caught.

        raise My_Except with "My exception message";
        --  Execution of current control flow abandoned, an exception of kind
        --  "My_Except" with associated string will bubble up until it is caught.
    end Main;


Handling an exception
---------------------

Now we need to tackle how to actually handle exceptions that were raised by us,
or other libraries. The neat thing in Ada is that you can add an exception
handler to any statement block, the following way:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       --  Block (sequence of statements)
       begin
          Open (File, In_File, "input.txt");
       exception
          when E : Name_Error =>
          --       ^ Exception to be handled
             Put ("Cannot open input file : ");
             Put_Line (Exception_Message (E));
             raise;
             --  Reraise current occurence
       end;
    end Open_File;

But you don't need to introduce a block just to handle an exception, you can
add it even to the statements block of your current subprogram:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       Open (File, In_File, "input.txt");
    --  Exception block can be added to any block
    exception
       when Name_Error =>
          Put ("Cannot open input file");
    end Open_File;

Predefined exceptions
---------------------

Ada has a very small number of predefined exceptions:

- `Constraint_Error` is the main one you might see. It is raised:
    - When bounds or subtype doesn’t match/in general any violation of constraints.
    - In case of overflow (-gnato for GNAT)
    - In case of null dereferences
    - In case of division by 0

- `Program_Error` might appear but probably less often. It is used for more
  arcane stuff, such as elaboration issues, or erroneous execution.

- `Storage_Error` will happen because of memory issues, such as:
     - Not enough memory (allocator)
     - Not enough stack

- `Tasking_Error` will happen with task related errors, such as any error
  happening during task activation.

You should generally not reuse predefined exceptions, because it is then
obvious when they happen that it is because something went wrong in a built-in
language operation.

Tasking
=======

Tasks and protected objects allow for implementing concurrency in Ada. The
following sections explain those concepts in more details.

Tasks
-----

A task can be thought as an application that runs *concurrently* with
the main application. In other programming languages, a task may also be
called a `thread <https://en.wikipedia.org/wiki/Thread_(computing)>`_,
and tasking may be called
`multithreading  <https://en.wikipedia.org/wiki/Thread_(computing)#Multithreading>`_.

Tasks may synchronize with the main application, but they also may process
information completely independent from the main application. This section
will show how this is accomplished.

Simple task
~~~~~~~~~~~

Tasks can be declared by using the keyword :ada:`task`. The task
implementation is defined in a :ada:`task body` block. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

Here, we're declaring and implementing the task ``T``. As soon as the
main application starts, task ``T`` will also start automatically --- it's
not necessary to manually start this task. By running the application
above, we can see that both calls to :ada:`Put_Line` are performed.

Note that:

- The main application is a task itself.

  - In this case, the subprogram ``Show_Simple_Task`` is the main task of
    the application.

- Task ``T`` is a subtask.

  - Each subtask has a master task.

  - Therefore, the main task is also the master task of task ``T``.

- The number of tasks is not limited to one: we could include a
  task ``T2`` in the example above.

  - This task would also start automatically and run *concurrently* with
    task ``T`` and the main task. For example:

    .. code-block:: ada

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Simple_Tasks is
           task T;
           task T2;

           task body T is
           begin
              Put_Line ("In task T");
           end T;

           task body T2 is
           begin
              Put_Line ("In task T2");
           end T2;

        begin
           Put_Line ("In main");
        end Show_Simple_Tasks;

Simple synchronization
~~~~~~~~~~~~~~~~~~~~~~

As we've just seen, as soon as the main task starts, its subtasks
also start automatically. The main task will continue its
processing until it reaches the end of its implementation. At this point,
however, it will not finish. Instead, the main task will wait until its
subtasks have finished before it finishes itself. In other words, after
this waiting process, a synchronization between the main task and its
subtasks occurs. After this final synchronization, the main task may
finish. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Sync is
       task T;
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end;
    begin
       null;
       --  Will wait here until all tasks have terminated
    end Show_Simple_Sync;

Note that the same mechanism is used for other subprograms that contain
subtasks: the subprogram's master task will wait for its subtasks to
finish. In other words, this mechanism is not limited to the main
application, but is also applied to any subprogram called by the main
application or its subprograms.

A synchronization is also achieved if we move the task to a separate
package. In the example below, we declare a task ``T`` in the package
``Simple_Sync_Pkg``.

.. code-block:: ada

    package Simple_Sync_Pkg is
       task T;
    end Simple_Sync_Pkg;

This is the corresponding package implementation:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Sync_Pkg is
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    end Simple_Sync_Pkg;

As soon as the package is :ada:`with`'ed for the main procedure, the task
``T`` defined in the package will be part of the main task. For example:

.. code-block:: ada

    with Simple_Sync_Pkg;

    procedure Test_Simple_Sync_Pkg is
    begin
       null;
       --  Will wait here until all tasks have terminated
    end Test_Simple_Sync_Pkg;

Again, as soon as the main task reaches its end, it will synchronize with
task ``T`` from ``Simple_Sync_Pkg`` before finishing.

Delay
~~~~~

A delay may be introduced by using the keyword :ada:`delay`. This will
put the task to sleep for the amount of seconds specified in the delay
statement. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Delay is

       task T;

       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello from task T");
             delay 1.0;
             --    ^ Wait 1.0 seconds
          end loop;
       end T;
    begin
       delay 1.5;
       Put_Line ("hello from main");
    end Show_Delay;

In this example, we're making the task ``T`` wait one second after each
time it displays the "hello" message. Also, the main task is waiting 1.5
seconds and displaying another "hello" message

Synchronization: rendez-vous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous examples, the only kind of synchronization we've seen was
the one that happens automatically at the end of the main task. In
addition to that, it is possible to define custom synchronization points
using the keyword :ada:`entry`. An entry can be viewed as a special kind
of subprogram, and it be called by the master task using a similar syntax,
as we will see later.

In the task implementation, we shall define in which part of the task the
entries will be accepted by using the keyword :ada:`accept`. A task will
process its statements until it reaches an :ada:`accept` statement. At
this point, it will wait for the master task to synchronize with the it.
In other words:

- The subtask will wait at this point (in the :ada:`accept` statement),
  ready to accept a call from the master task to the corresponding entry.

- The master task will call the task entry in order to synchronize with
  the subtask --- similar to a procedure call.

The synchronization between tasks is called rendez-vous. Let's see an
example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous is

       task T is
          entry Start;
       end T;

       task body T is
       begin
          accept Start; -- Waiting for somebody to call the entry
          Put_Line ("In T");
       end T;

    begin
       Put_Line ("In Main");
       T.Start; --  Calling T's entry
    end Show_Rendezvous;

In this example, we declare an entry ``Start`` for the task ``T``.
In the task body, we implement this entry by using :ada:`accept Start`.
When the task ``T`` reaches this point, it will wait for the master task
to synchronize. This synchronization happens in the ``T.Start`` statement.
After the synchronization is finished, the main task and task ``T``
will run concurrently until they synchronize again when the main
task reaches its end.

Note that an entry may be used to perform more than just a simple
task synchronization: we may also perform multiple statements during
the time both tasks are synchronized. This is achieved by using a
:ada:`do ... end` block. For the previous example, we would simply write
:ada:`accept Start do <statements>; end;`. We will use this kind of block
in the next example.

Select loop
~~~~~~~~~~~

There is no limit for the amount of times an entry might be accepted in
the task implementation. In fact, we could create an infinite loop in the
task implementation and accept calls to the same entry over and over. An
infinite loop, however, would prevent the subtask from finishing, so it
would also block the master task when it reaches the end of its
processing. Therefore, a loop containing :ada:`accept` statements in a
task body is normally implemented together with a
:ada:`select ... or terminate` statement. In simple terms, this statement
allows the master task to terminate the subtask when the end of the master
task is reached. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous_Loop is

       task T is
          entry Start;
       end T;

       task body T is
          Cnt : Integer := 0;
       begin
          loop
             select
                accept Start do
                   Cnt := Cnt + 1;
                end Start;
                Put_Line ("In T's loop (" & Integer'Image (Cnt) & ")");
             or
                terminate;
             end select;
          end loop;
       end T;

    begin
       Put_Line ("In Main");

       for I in 1 .. 4 loop
          T.Start; --  Calling T's entry multiple times
       end loop;

    end Show_Rendezvous_Loop;

In this example, the task body implements an infinite loop that accepts
calls to the ``Start`` entry. We can make the following observations:

- In this case, an :ada:`accept E do ... end` block is used, where a
  counter is incremented.

    - As long as task ``T`` is performing the :ada:`do ... end` block, the
      main task will wait for the block to finish.

- In addition, the main task is calling the ``Start`` entry  multiple
  times in the loop from ``1 .. 4``.

    - Because task ``T`` uses an infinite loop, it will always accept
      calls to the ``Start`` entry.

    - When the main task reaches the end, it will check the status of the
      ``T`` task. Even though task ``T`` could accept new calls to the
      ``Start`` entry, the master task is allowed to terminate task ``T``
      due to the :ada:`or terminate` part of the :ada:`select` statement.

Cycling tasks
~~~~~~~~~~~~~

In a previous example, we've seen that we can delay a task by a given
amount of seconds using :ada:`delay` keyword. When using delay statements
in a loop, however, we cannot expect to have regular interval between the
delay statements. For example, we may have a call to a computationally
intensive procedure between the delay statements:

.. code-block:: ada
    :class: ada-nocheck

          while True loop
             delay 1.0;
             --    ^ Wait 1.0 seconds
             Computational_Intensive_App;
          end loop;

In this case, we cannot guarantee that, after 10 calls to the delay
statement, the time span is just 10 seconds. In fact, a time drift may be
introduced by the ``Computational_Intensive_App`` procedure. In many
cases, this time drift is not relevant, so that using the :ada:`delay`
keyword is good enough.

There are situations, however, where a time drift is not acceptable. In
this case, we need to use the :ada:`delay until` statement, which accepts
a precise time for the next execution, so that a regular interval can be
defined. This is useful, for example, in real-time applications.

We will see an example of how this time drift may be introduced, and how
the :ada:`delay until` statement circumvents the problem. Before we do
that, we will look into an auxiliary package containing a procedure that
allows for measuring the elapsed time (``Show_Elapsed_Time``) and a dummy
``Computational_Intensive_App`` procedure using a simple delay. This is
the package specification:

.. code-block:: ada

    with Ada.Real_Time; use Ada.Real_Time;

    package Delay_Aux_Pkg is

       function Get_Start_Time return Time
         with Inline;

       procedure Show_Elapsed_Time
         with Inline;

       procedure Computational_Intensive_App;
    private
       Start_Time   : Time := Clock;

       function Get_Start_Time return Time is (Start_Time);

    end Delay_Aux_Pkg;

This is the package definition:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Delay_Aux_Pkg is

       procedure Show_Elapsed_Time is
          Now_Time     : Time;
          Elapsed_Time : Time_Span;
       begin
          Now_Time     := Clock;
          Elapsed_Time := Now_Time - Start_Time;
          Put_Line ("Elapsed time "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Show_Elapsed_Time;

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

    end Delay_Aux_Pkg;

Using this auxiliary package, we're now ready to write our time-drifting
application:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Drifting_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay 1.0;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             Put_Line ("Cycle # " & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished time-drifting loop");
       end T;

    begin
       null;
    end Show_Time_Drifting_Task;

As we can see by running the application, due to the drift introduced by
``Computational_Intensive_App``, after three iterations of the loop, we
already have a time span of about four seconds. Using the
:ada:`delay until` statement, however, we'll be able to avoid this time
drift and have a regular interval of one second:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Cycling_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cycle : constant Time_Span := Milliseconds (1000);
          Next  : Time := Aux.Get_Start_Time + Cycle;

          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay until Next;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             --  Calculate next execution time using a
             --  cycle of one seconds
             Next := Next + Cycle;

             Put_Line ("Cycle # " & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished cycling");
       end T;

    begin
       null;
    end Show_Cycling_Task;

As we can see by running the application, the :ada:`delay until` statement
makes sure that the ``Computational_Intensive_App`` does not affect the
regular interval of one second between the iterations.

Protected objects
-----------------

In situations where multiple tasks are accessing shared data, data
corruption may happen. For example, data will be inconsistent when one
task overwrites parts of the information that is being read by another
task. In order to avoid this kind of problems and ensure that the
information is accessed in a coordinated way, we can use protected
objects.

Protected objects encapsulate data and provide access to this data by
means of protected operations. These protected operations may be
subprograms or protected entries. Using protected objects ensures that the
data will not be corrupted by race conditions.

.. admonition:: Important

    It is possible to implement protected objects using Ada tasks. In
    fact, this was the only possible way of implementing them in Ada 83
    (the first version of the Ada language). However, the use of protected
    objects greatly simplify the implementation when compared to similar
    mechanisms implemented strictly with tasks. Therefore, the
    recommendation is to use protected objects when the main goal is to
    just protect data.

Simple object
~~~~~~~~~~~~~

A protected object is declared by using the :ada:`protected` keyword. The
syntax is similar to the one used for packages: we can declare operations
(e.g.: procedures and functions) in the public part, and data in the
private part. The corresponding implementation of the operations is
included in the :ada:`protected body` of the object. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects is

       protected Obj is
          --  Operations go here (only subprograms)
          procedure Set (V : Integer);
          function Get return Integer;
       private
          --  Data goes here
          Local : Integer := 0;
       end Obj;

       protected body Obj is
          --  procedures can modify the data
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          --  functions cannot modify the data
          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj;

    begin
       Obj.Set (5);
       Put_Line ("Number is: " & Integer'Image (Obj.Get));
    end Show_Protected_Objects;

In this example, we are defining two operations for ``Obj``: ``Set`` and
``Get``. The implementation of these operations can be found in the
``Obj`` body. The syntax used for implementing these operations is the
same as the one used for common procedures and functions. Therefore  the
implementation of protected objects is straightforward --- we simply
access and update ``Local`` in these subprograms. In order to call these
operations in the main application, we use the prefixed notation, e.g.:
``Obj.Get``.

Entries
~~~~~~~

In addition to protected procedures and functions, we may also define
protected entry points. This is achieved by using the :ada:`entry`
keyword. Protected entry points allow for defining barriers using the
:ada:`when` keyword. Barriers are conditions that must be fulfilled before
the actual processing defined in the entry can start.

In the previous example, we've used procedures and functions to define
our operations on the protected objects. However, this implementation
allows for reading the protected information (via ``Obj.Get``) before the
information is set (via ``Obj.Set``). In that case, we have defined a
default value (0). By rewriting ``Obj.Get`` and using an entry instead of
a function, we may implement a barrier: this ensures that no task will
read the information before it has been first set.

The following example implements the barrier for the ``Obj.Get``
operation. Also, it contains two concurrent subprograms (main task
and task ``T``) that try to access the protected object.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects_Entries is

       protected Obj is
          procedure Set (V : Integer);
          entry Get (V : out Integer);
       private
          Local  : Integer;
          Is_Set : Boolean := False;
       end Obj;

       protected body Obj is
          procedure Set (V : Integer) is
          begin
             Local := V;
             Is_Set := True;
          end Set;

          entry Get (V : out Integer)
            when Is_Set is
             --  Entry will be blocked until the condition is true.
             --  Barrier is evaluated at call of entry, and at exit of
             --  procedures and entries.
             --  Calling task will sleep until the barrier is relieved.
          begin
             V := Local;
             Is_Set := False;
          end Get;
       end Obj;

       N : Integer := 0;

       task T;

       task body T is
       begin
          Put_Line ("Task T will delay for 4 seconds...");
          delay 4.0;
          Put_Line ("Task T will set Obj...");
          Obj.Set (5);
          Put_Line ("Task T has just set Obj...");
       end T;
    begin
       Put_Line ("Main application will get Obj...");
       Obj.Get (N);
       Put_Line ("Main application has just retrieved Obj...");
       Put_Line ("Number is: " & Integer'Image (N));

    end Show_Protected_Objects_Entries;

As we can see by running the application, the main application waits until
the protected object is set (by the call to ``Obj.Set`` in task ``T``)
before it reads the information (via ``Obj.Get``). Because a 4-second
delay has been added in task ``T``, the main application will also be
delayed by 4 seconds. Only after this delay, task ``T`` will set the
object and release the barrier set by ``Obj.Get``, so that the main
application can then restore processing (after the information from the
protected object is retrieved).

Task and protected types
------------------------

In the previous examples, we have defined single tasks and protected
objects. It is possible, however, to generalize tasks and protected
objects using type definitions. This allows, for example, for creating
multiple tasks based on just a single task type.

Task types
~~~~~~~~~~

A task type is a generalization of a task. The declaration is similar to
simple tasks: you just have to replace :ada:`task` by :ada:`task type`.
The main difference between simple tasks and task types is that task types
don't create actual tasks that automatically start. Instead, a task
declaration is needed for that --- similar to variable declarations.

For example, we may reuse our first example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

We can rewrite the example and replace ``task T`` by ``task type TT``.
After the type definition is complete, we declare a task (``A_Task``)
based on the task type ``TT``:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task_Type is
       task type TT;

       task body TT is
       begin
          Put_Line ("In task type TT");
       end TT;

       A_Task : TT;
    begin
       Put_Line ("In main");
    end Show_Simple_Task_Type;

We may extend this example and create an array of tasks. Since we're using
the same syntax as for variable declarations, we can simply use a similar
syntax for task types: :ada:`array (<>) of Task_Type`. Also, we may pass
information to the individual tasks by defining a ``Start`` entry. This
is the updated example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Task_Type_Array is
       task type TT is
          entry Start (N : Integer);
       end TT;

       task body TT is
          Task_N : Integer;
       begin
          accept Start (N : Integer) do
             Task_N := N;
          end Start;
          Put_Line ("In task T: " & Integer'Image (Task_N));
       end TT;

       My_Tasks : array (1 .. 5) of TT;
    begin
       Put_Line ("In main");

       for I in My_Tasks'Range loop
          My_Tasks (I).Start (I);
       end loop;
    end Show_Task_Type_Array;

In the example above, we're declaring five tasks in the array
``My_Tasks``. We pass the array index to the individual tasks in the
start entry point (``Start``). After the synchronization with the
individual subtasks in the main task, each subtask will proceed to call
``Put_Line`` concurrently.

Protected types
~~~~~~~~~~~~~~~

A protected type is a generalization of a protected object. The
declaration is similar to protected objects: you just have to replace
:ada:`protected` by :ada:`protected type`. However, similar to task types,
protected types require an object declaration in order to create actual
objects. Again, this is similar to variable declarations and allows for
creating arrays of protected objects, for example.

We can reuse a previous example and rewrite it to use a protected type:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Object_Type is

       protected type Obj_Type is
          procedure Set (V : Integer);
          function Get return Integer;
       private
          Local : Integer := 0;
       end Obj_Type;

       protected body Obj_Type is
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj_Type;

       Obj : Obj_Type;
    begin
       Obj.Set (5);
       Put_Line ("Number is: " & Integer'Image (Obj.Get));
    end Show_Protected_Object_Type;

In this example, instead of directly defining the protected object
``Obj``, we first define a protected type ``Obj_Type`` and then declare
``Obj`` based on that protected type. Note that the main application
hasn't change: we still use ``Obj.Set`` and ``Obj.Get`` to access the
protected object as in the original example.

Interfacing with C
==================

Ada allows for interfacing with existing code in C and C++. This section will
discuss how to interface with C specifically.

.. TODO: Add link to advanced course on C++

Multi-language project
----------------------

When using ``gprbuild``, we can only compile Ada source-code files by
default. In order to compile C files in addition to Ada files, we need to
adapt the project file used by ``gprbuild``. This can be achieved by
using the ``Languages`` entry, as in the following example:

.. code-block:: ada
    :class: ada-nocheck

    project Multilang is

       for Languages use ("ada", "c");

       for Source_Dirs use ("src");
       for Main use ("main.adb");
       for Object_Dir use "obj";

    end Multilang;

Type convention
---------------

In order to interface with data types declared in a C application, the
convention aspect needs to be specified. In the following example, we
interface with the ``C_Enum`` enumeration declared in a C source-code
file:

.. code-block:: ada

    procedure Show_C_Enum is

       type C_Enum is (A, B, C) with Convention => C;
       -- Use C convention for C_Enum
    begin
       null;
    end Show_C_Enum;

In order to interface with C built-in types, we need to reference to the
:ada:`Interfaces.C` package, which contains all type definitions that we
need. For example:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    procedure Show_C_Struct is

       type c_struct is record
          a : int;
          b : long;
          c : unsigned;
          d : double;
       end record
         with Convention => C;

    begin
       null;
    end Show_C_Struct;

In this example, we're interfacing with a C struct (``C_Struct``) and
making use of the corresponding data types in C (:c:`int`, :c:`long`,
:c:`unsigned` and :c:`double`). This is the original declaration:

.. code-block:: c

    struct c_struct
    {
        int         a;
        long        b;
        unsigned    c;
        double      d;
    };

Foreign subprograms
-------------------

Calling C subprograms in Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A similar approach is used when interfacing with subprograms written in C.
In this case, an additional aspect is required: :ada:`Import`. For example:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;
       --  Imports function 'my_func' from C.
       --  You can now call it from Ada.

    begin
       null;
    end Show_C_Func;

This code interfaces with the following declaration in the C header file:

.. code-block:: c

    int my_func (int a);

This is the corresponding implementation:

.. code-block:: c

    #include "my_func.h"

    int my_func (int a)
    {
        return a * 2;
    }

It is possible to use a different subprogram name in the Ada code. For
example, we could rename the original C function to ``Get_Value`` in the
Ada code:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function Get_Value (a : int) return int
         with
           Import        => True,
           Convention    => C,
           External_Name => "my_func";

       --  Imports function 'my_func' from C and
       --  rename it to 'Get_Value'

       V : int;
    begin
       V := Get_Value (2);
       Put_Line("Result is " & int'Image(V));
    end Show_C_Func;

As the example shows, we can make use of the ``Get_Value`` function and
retrieve information without additional efforts.

Calling Ada subprograms in C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to call Ada subprograms in C applications. This
requires the use of the :ada:`Export` aspect. For example:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

This is the corresponding implementation:

.. code-block:: ada

    package body C_API is

       function My_Func (a : int) return int is
       begin
          return a * 2;
       end My_Func;

    end C_API;

In the C code, we simply have to declare the function using the :c:`extern`
keyword. For example:

.. code-block:: c

    #include <stdio.h>

    extern int my_func (int a);

    int main (int argc, char **argv) {

      int v = my_func(2);

      printf("Result is %d\n", v);

      return 0;
    }

Foreign variables
-----------------

Using C global variables in Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to use global variables from C code, we can apply the same method
as for subprograms: we just specify the :ada:`Import` and :ada:`Convention`
aspects for the variable we want to import.

Let's reuse an example from the previous section. We'll add a global
variable (``func_cnt``) that counts the number of times that a the
function (``my_func``) was called:

.. code-block:: c

    /*% filename: test.h */

    extern int func_cnt;

    int my_func (int a);

The variable is then declared in the C file and increment in ``my_func``:

.. code-block:: c

    #include "test.h"

    int func_cnt = 0;

    int my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

In the Ada application, we just need to reference the foreign variable:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;

       V : int;

       func_cnt : int
         with
           Import        => True,
           Convention    => C;
       --  We can access the func_cnt variable from test.c

    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called " & int'Image (func_cnt) & " times");
    end Show_C_Func;

As we can see by running the application, the value from the counter will
contain the correct number of times that ``my_func`` was called.

Similar to subprograms, we could use the :ada:`External_Name` aspect to
rename the variable in the Ada application.

Using Ada variables in C
~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to use variables declared in Ada files in C
applications. Similarly to subprogram, this requires the use of the
:ada:`Export` aspect.

Let's reuse the previous example and add a counter, as we did in the
previous example:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       func_cnt : int := 0
         with
           Export     => True,
           Convention => C;

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

The variable is then increment in ``My_Func``:

.. code-block:: ada

    --% filename: c_api.adb
    package body C_API is

       function My_Func (a : int) return int is
       begin
          func_cnt := func_cnt + 1;
          return a * 2;
       end My_Func;

    end C_API;

In the C application, we just need to declare the variable and use it:

.. code-block:: c

    #include <stdio.h>

    extern int my_func (int a);

    extern int func_cnt;

    int main (int argc, char **argv) {

      int v;

      v = my_func(1);
      v = my_func(2);
      v = my_func(3);

      printf("Result is %d\n", v);

      printf("Function was called %d times\n", func_cnt);

      return 0;
    }

Again, by running the application, we see that the value from the counter
will contain the correct number of times that ``my_func`` was called.

Generating bindings
-------------------

In the examples above, we have manually created the Ada bindings for the
C source-code we wanted to interface with. It is possible to automate this
process by using the *Ada spec dump* compiler option:
``-fdump-ada-spec``. We'll discuss details by revisiting our previous
example.

This was the C header file that we had:

.. code-block:: c

    extern int func_cnt;

    int my_func (int a);

In order to create bindings, we'll call the compiler like this:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

This will create an Ada specification file called ``test_h.ads``:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_h is

       func_cnt : aliased int;  -- ./test.h:3
       pragma Import (C, func_cnt, "func_cnt");

       function my_func (arg1 : int) return int;  -- ./test.h:5
       pragma Import (C, my_func, "my_func");

    end test_h;

Now, we can simply refer to ``test_h`` package in the Ada application:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_h;       use test_h;

    procedure Show_C_Func is
       V : int;
    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called " & int'Image (func_cnt) & " times");
    end Show_C_Func;

Note that, in addition to ``fdump-ada-spec``, you can also specify the
parent unit for the bindings you're creating. For example:

.. code-block:: sh

    gcc -c -fdump-ada-spec -fada-spec-parent=Ext_C_Code -C ./test.h

This will create the file ``ext_c_code-test_h.ads``:

.. code-block:: ada
    :class: ada-syntax-only

    package Ext_C_Code.test_h is

       -- automatic generated bindings...

    end Ext_C_Code.test_h;

Adapting bindings
~~~~~~~~~~~~~~~~~

When creating bindings for a C header file, the compiler tries to do the
best guess it can. However, the generated bindings do not always match the
expectations we might have. This can happen, for example, when creating
bindings for functions that deal with pointers. In this case, the compiler
may just use :ada:`System.Address` for the pointers. Although this approach
works fine (as we'll see later), this is not necessarily how developers
would interpret the C header file. The following example will clarify this
problem.

Let's start with the following C header file:

.. code-block:: c

    /*% filename: test.h */

    struct test;

    struct test * test_create(void);

    void test_destroy(struct test *t);

    void test_reset(struct test *t);

    void test_set_name(struct test *t, char *name);

    void test_set_address(struct test *t, char *address);

    void test_display(const struct test *t);

This is the corresponding implementation:

.. code-block:: c

    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    #include "test.h"

    struct test {
      char name[80];
      char address[120];
    };

    static size_t
    strlcpy(char *dst, const char *src, size_t dstsize)
    {
      size_t len = strlen(src);
      if (dstsize) {
        size_t bl = (len < dstsize-1 ? len : dstsize-1);
        ((char*)memcpy(dst, src, bl))[bl] = 0;
      }
      return len;
    }

    struct test * test_create(void)
    {
      return malloc (sizeof (struct test));
    }

    void test_destroy(struct test *t)
    {
      if (t != NULL) {
        free(t);
      }
    }

    void test_reset(struct test *t)
    {
      t->name[0]    = '\0';
      t->address[0] = '\0';
    }

    void test_set_name(struct test *t, char *name)
    {
      strlcpy(t->name, name, sizeof(t->name));
    }

    void test_set_address(struct test *t, char *address)
    {
      strlcpy(t->address, address, sizeof(t->address));
    }

    void test_display(const struct test *t)
    {
      printf("Name:    %s\n", t->name);
      printf("Address: %s\n", t->address);
    }

Next, we'll create our bindings by running gcc:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

This creates the following specification in ``test_h.ads``:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package test_h is

       --  skipped empty struct test

       function test_create return System.Address;  -- ./test.h:5
       pragma Import (C, test_create, "test_create");

       procedure test_destroy (arg1 : System.Address);  -- ./test.h:7
       pragma Import (C, test_destroy, "test_destroy");

       procedure test_reset (arg1 : System.Address);  -- ./test.h:9
       pragma Import (C, test_reset, "test_reset");

       procedure test_set_name (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, test_set_name, "test_set_name");

       procedure test_set_address (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:13
       pragma Import (C, test_set_address, "test_set_address");

       procedure test_display (arg1 : System.Address);  -- ./test.h:15
       pragma Import (C, test_display, "test_display");

    end test_h;

As we can see, the bindings generator completely ignores the specification
of :c:`struct test`. Also, all references to the ``test`` are replaced by
simple addresses (:ada:`System.Address`). Of course, these bindings are good
enough for creating a test application in Ada:

.. code-block:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with Ada.Text_IO;          use Ada.Text_IO;
    with test_h;               use test_h;

    with System;

    procedure Show_Automatic_C_Struct_Bindings is

       Name    : constant chars_ptr := New_String ("John Doe");
       Address : constant chars_ptr := New_String ("Small Town");

       T : System.Address := test_create;

    begin
       test_reset (T);
       test_set_name (T, Name);
       test_set_address (T, Address);

       test_display (T);
       test_destroy (T);
    end Show_Automatic_C_Struct_Bindings;

Even though we can successfully bind our C code with Ada using the
automatic generated bindings, they are not ideal. Instead, we would like
to have Ada bindings that match our (human) interpretation of the C header
file. This will require manual analysis of the header file. The good news
are that, at least, we can use the automatic generated bindings as a
starting point and adapt them to our needs. For example, we can:

    #. Define a ``Test`` type based on :ada:`System.Address` and use it in
       all relevant function.

    #. Remove the ``test_`` prefix in all operations on the ``Test``
       type.

This would be the resulting specification:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package adapted_test_h is

       type Test is new System.Address;

       function Create return Test;
       pragma Import (C, Create, "test_create");

       procedure Destroy (T : Test);
       pragma Import (C, Destroy, "test_destroy");

       procedure Reset (T : Test);
       pragma Import (C, Reset, "test_reset");

       procedure Set_Name (T    : Test;
                           Name : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, Set_Name, "test_set_name");

       procedure Set_Address (T       : Test;
                              Address : Interfaces.C.Strings.chars_ptr);  -- ./test.h:13
       pragma Import (C, Set_Address, "test_set_address");

       procedure Display (T : Test);  -- ./test.h:15
       pragma Import (C, Display, "test_display");

    end adapted_test_h;

This would be the corresponding Ada application:

.. code-block:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with Ada.Text_IO;          use  Ada.Text_IO;
    with adapted_test_h;       use  adapted_test_h;

    with System;

    procedure Show_Adapted_C_Struct_Bindings is

       Name    : constant chars_ptr := New_String ("John Doe");
       Address : constant chars_ptr := New_String ("Small Town");

       T : Test := Create;

    begin
       Reset (T);
       Set_Name (T, Name);
       Set_Address (T, Address);

       Display (T);
       Destroy (T);
    end Show_Adapted_C_Struct_Bindings;

Now, we're able to use the ``Test`` type and its operations in a clean,
readable way.

Object oriented programming
===========================

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

Newtypes
--------

Before going into tagged types, we should go into a topic we have brushed on,
but not really covered so far:

For every type in Ada, you can create a new type from it. Type derivation is
built-in into the language.

.. code-block:: ada

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

.. code-block:: ada

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
  `runtime type <https://fr.wikipedia.org/wiki/Run-time_type_information>`__ of
  an object.

- Primitives can dispatch. A primitive on a tagged type is what you would call
  a method in Java or C++. If you derive a base type, and override a primitive
  of it, then in some instances it will be possible to call it on an object
  such that which primitive is called depends on the exact runtime type of the
  object.

- Subtyping rules are introduced, such that a tagged type derived from a base
  type is statically compatible with the base type.

Let's see our first tagged type declaration:

.. code-block:: ada

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

.. code-block:: ada
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

.. code-block:: ada

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

.. code-block:: ada

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

    .. code-block:: ada
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

.. code-block:: ada

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


.. code-block:: ada
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

Standard library
================

Standard package
----------------

Containers
----------

Ada includes support for containers (such as vectors and sets) in its
standard library. This section presents an introduction to the topic. For
a list of all containers available in Ada, please refer to Appendix B.

Vectors
~~~~~~~

The following subsections present a general overview of vectors,
including instantiation, initialization, operations on vector elements
and operation on vectors.

Instantiation
^^^^^^^^^^^^^

The following example shows the instantiation and declaration of a vector
``V``:

.. code-block:: ada

    with Ada.Containers.Vectors;

    procedure Show_Vector_Inst is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       V : Integer_Vectors.Vector;
    begin
       null;
    end Show_Vector_Inst;

After including the container package (:ada:`Ada.Containers.Vectors` in
this case), we need to instantiate it. This is due to the fact that
containers are based on generic packages. Therefore, we cannot simply
declare a vector as we would declare an array of a specific type, e.g.:

.. code-block:: ada
    :class: ada-nocheck

       A : array (1 .. 10) of Integer;

Instead, we need to first create an instance of the generic package for
the specific type and declare it using the corresponding type from the
instantiated package. As indicated above, this instantiation needs to be
done for any container type from the standard library.

In the instantiation of ``Integer_Vectors``, we define that the vector
contains elements of ``Integer`` type by specifying the ``Element_Type``.
Also, by setting ``Index_Type`` to ``Natural``, we specify that the
allowed range includes all natural numbers. We could, instead, use a more
restrict range if we wanted to.

Initialization
^^^^^^^^^^^^^^

In order to initialize a vector, we can use a concatenation of elements.
This is achieved by using the :ada:`&` operator, as shown in the following
example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Init is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length) & " elements");
    end Show_Vector_Init;

Note that the example specifies :ada:`use Integer_Vectors`, so that we
have direct access to the types and operations from the instantiated
package. Also, the example introduces another operation on the vector:
``Length``, which retrieves the number of elements in the vector. Note
that the dot notation is possible because ``Vector`` is a tagged type,
so that we can simply write ``V.Length`` instead of ``Length(V)``.

Appending and prepending elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also add elements to a vector by using the ``Prepend`` and
``Append`` operations. As the name suggests, these operations add
elements to the beginning or to the end of the vector, respectively. For
example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Append is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector;
    begin
       Put_Line ("Appending some elements to the vector...");
       V.Append (20);
       V.Append (10);
       V.Append (0);
       V.Append (13);
       Put_Line ("Finished appending.");

       Put_Line ("Prepending some elements to the vector...");
       V.Prepend (30);
       V.Prepend (40);
       V.Prepend (100);
       Put_Line ("Finished prepending.");

       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length) & " elements");
    end Show_Vector_Append;

This example fills the vector with elements in the following sequence:
(100, 40, 30, 20, 10, 0, 13).

According to the Reference Manual, the worst-case complexity should be:

- O(:math:`log N`) for the ``Append`` operation, and

- O(:math:`N log N`) for the ``Prepend`` operation.

Accessing first and last elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can access the first and last elements of a vector by using the
functions ``First_Element`` and ``Last_Element``. For example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer)    return String renames Integer'Image;
       function Img (I : Count_Type) return String renames Count_Type'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has " & Img (V.Length) & " elements");

       --  Using V.First_Element to retrieve first element
       Put_Line ("First element is " & Img (V.First_Element));

       --  Using V.Last_Element to retrieve last element
       Put_Line ("Last element is " & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Also, we can swap elements by calling the procedure ``Swap``. In addition,
we can retrieve a reference (cursor) for the first and last elements of
the vector by calling the functions ``First`` and ``Last``. A cursor
allows for iterating over a container and processing elements from the
container.

With these operations, we're able to write some code that swaps the first
and last elements of a vector:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       --  Using V.First and V.Last to retrieve cursor for first and
       --  last elements.
       --  Using V.Swap to swap elements.
       V.Swap (V.First, V.Last);

       Put_Line ("First element is now " & Img (V.First_Element));
       Put_Line ("Last element is now " & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Iterating
^^^^^^^^^

The easiest way for iterating over a container is by using a
:ada:`for E of Our_Container` loop. This will give us a reference to the
element of the current position (``E``). We can then use ``E`` directly.
For example:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using for ... of loop to iterate:
       --
       for E of V loop
          Put_Line ("- " & Img (E));
       end loop;

    end Show_Vector_Iteration;

This code will display each element from the vector ``V``.

Note that, in addition to displaying the value of an element, we can also
modify its value. For example, we could easily write a loop to add one to
each element of vector ``V``:

.. code-block:: ada
    :class: ada-nocheck

       for E of V loop
          E := E + 1;
       end loop;

In case of vectors, we can also use indices to access elements. The format
is similar to a loop over array elements: we use a :ada:`for I in <range>`
loop in this case. The range is provided by ``V.First_Index`` and
``V.Last_Index``. We can access the current element by using it as an
array index: ``V (I)``. For example:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Index_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using indices in a "for I in ..." loop to iterate:
       --
       for I in V.First_Index .. V.Last_Index loop
          --  Displaying current index I
          Put ("- ["
               & Extended_Index'Image (I)
               & "] ");

          Put (Integer'Image (V (I)));

          --  We could also use V.Element (I) function to retrieve the
          --  element for the current index I

          New_Line;
       end loop;

    end Show_Vector_Index_Iteration;

Note that, in addition to displaying the vector elements, we're also
displaying the index ``I`` itself, similar to what we can do with array
indices. Also, we can access the element by using the short form ``V (I)``
or the longer form ``V.Element (I)``.

As mentioned in the previous section, we can use cursors to iterate over
containers. For this, we use the function ``Iterate``, which retrieves
a cursor for each position of the vector. The corresponding loop has the
format :ada:`for C in V.Iterate loop`. Similar to the previous example
using indices, we can again access the current element by using the cursor
as an array index: ``V (C)``. For example:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Cursor_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using cursor in a loop to iterate:
       --
       for C in V.Iterate loop
          --  Using To_Index function in order to retrieve index
          --  for the cursor position
          Put ("- ["
               & Extended_Index'Image (To_Index (C))
               & "] ");

          Put (Integer'Image (V (C)));

          --  We could use Element (C) to retrieve the vector
          --  element for the cursor position

          New_Line;
       end loop;

       --  Alternatively, we could iterate with a while-loop:
       --
       --  declare
       --     C : Cursor := V.First;
       --  begin
       --     while C /= No_Element loop
       --        some processing here...
       --
       --        C := Next (C);
       --     end loop;
       --  end;

    end Show_Vector_Cursor_Iteration;

Note that, instead of accessing an element in the loop using ``V (C)``,
we could also have used the longer form ``Element (C)``. Also, we're using
the function ``To_Index`` to retrieve the corresponding index for the
current cursor.

Moreover, as indicated above in the comments after the iteration loop, we
could also use a :ada:`while ... loop` to iterate over the vector. In this
case, we would start with a cursor for the first element (retrieved by
calling ``V.First``) and then call ``Next (C)`` to retrieve a cursor for
the next positions. ``Next (C)`` returns ``No_Element`` when the cursor
reaches the end of the vector.

As mentioned above, we can directly modify the elements using a reference.
This is how it looks like when dealing with indices and cursors:

.. code-block:: ada
    :class: ada-nocheck

       --  Modify vector elements using index
       for I in V.First_Index .. V.Last_Index loop
          V (I) := V (I) + 1;
       end loop;

       --  Modify vector elements using cursor
       for C in V.Iterate loop
          V (C) := V (C) + 1;
       end loop;

According to the Reference Manual, the worst-case complexity of accessing
an element should be O(:math:`log N`).

Another approach to modify elements of a vector is to use a process
procedure, which takes the individual elements and does some processing on
them. In this case, we can call ``Update_Element`` and pass a cursor
and an access to the process procedure. For example:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Update is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       procedure Add_One (I : in out Integer) is
       begin
          I := I + 1;
       end Add_One;

       V : Vector := 20 & 10 & 12;
    begin
       --
       --  Using V.Update_Element to process elements
       --
       for C in V.Iterate loop
          V.Update_Element (C, Add_One'Access);
       end loop;

    end Show_Vector_Update;

Finding and changing elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can find a certain element in a vector by retrieving its index. This
can be achieved by using ``Find_Index``, which retrieves the index of the
first element that matches the element we're looking for. Alternatively,
we could use ``Find`` to retrieve the cursor for that element. For
example:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_Vector_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Using Find_Index to retrieve index of element with value 10
       Idx := V.Find_Index (10);
       Put_Line ("Index of element with value 10 is "
                 & Extended_Index'Image (Idx));

       --  Using Find to retrieve cursor for element with value 13
       C   := V.Find (13);
       Idx := To_Index (C);
       Put_Line ("Index of element with value 13 is "
                 & Extended_Index'Image (Idx));
    end Show_Find_Vector_Element;

As shown in the previous section about iteration, we can directly access
vector elements by using either an index or cursor. However, before
doing that, we should check whether the index or cursor is valid, since
``Find_Index`` or ``Find`` might not have found the element in the vector.
An exception will be raised if we try to access an element with an invalid
index or cursor. We do this check by comparing the index to ``No_Index``
and the cursor to ``No_Element``. For example:

.. code-block:: ada
    :class: ada-nocheck

       --  Modify vector element using index
       if Idx /= No_Index then
          V (Idx) := 11;
       end if;

       --  Modify vector element using cursor
       if C /= No_Element then
          V (C) := 14;
       end if;

Alternatively, instead of writing ``V (C) := 14``, we could use the longer
form :ada:`V.Replace_Element (C, 14)`.

Inserting elements
^^^^^^^^^^^^^^^^^^

In the previous sections, we've seen examples of how to insert elements
into a vector:

- using the concatenation operator (:ada:`&`) at the vector declaration,
  or

- calling the ``Prepend`` and ``Append`` procedures.

In some cases, we want to insert an element at a specific position, e.g.
before a certain element in the vector. We can do this by calling
``Insert``. For example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Insert is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length) & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 12;
       C : Cursor;
    begin
       Show_Elements (V);

       New_Line;
       Put_Line ("Adding element with value 9 (before 10)...");

       --
       --  Using V.Insert to insert element into vector
       --
       C := V.Find (10);
       if C /= No_Element then
          V.Insert (C, 9);
       end if;

       Show_Elements (V);

    end Show_Vector_Insert;

In this example, we're looking for an element with value 10. If we find
it, we then insert an element with value 9 before it.

Removing elements
^^^^^^^^^^^^^^^^^

We can remove elements from a vector by using the ``Delete`` procedure
with either a valid index or cursor. If we combine this with the functions
``Find_Index`` and ``Find`` from the previous section, we'll be able to
write a program that searches for a specific element and deletes it:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Element is
       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Using Find_Index to retrieve index of element with value 10
       Idx := V.Find_Index (10);

       --  Checking whether index is valid
       if Idx /= No_Index then
          --  Removing element using V.Delete
          V.Delete (Idx);
       end if;

       --  Using Find to retrieve cursor for element with value 13
       C := V.Find (13);

       --  Checking whether index is valid
       if C /= No_Element then
          --  Removing element using V.Delete
          V.Delete (C);
       end if;

    end Show_Remove_Vector_Element;

This approach can be extended to delete all elements that match a certain
value. We just need to keep searching for the element in a loop until we
get an invalid index or cursor. For example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Elements is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has " & Count_Type'Image (V.Length) & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 14 & 13;
    begin
       Show_Elements (V);

       --
       --  Removing elements using an index
       --
       declare
          E : constant Integer := 10;
          I : Extended_Index;
       begin
          New_Line;
          Put_Line ("Removing all elements with value of "
                    & Integer'Image (E) & "...");
          loop
             I := V.Find_Index (E);
             exit when I = No_Index;
             V.Delete (I);
          end loop;
       end;

       --
       --  Removing elements using a cursor
       --
       declare
          E : constant Integer := 13;
          C : Cursor;
       begin
          New_Line;
          Put_Line ("Removing all elements with value of "
                    & Integer'Image (E) & "...");
          loop
             C := V.Find (E);
             exit when C = No_Element;
             V.Delete (C);
          end loop;
       end;

       Show_Elements (V);
    end Show_Remove_Vector_Elements;

In this example, we remove all elements from the vector that have the
value 10 by retrieving their index. Likewise,  we remove all elements with
the value 13 by retrieving their cursor.

Other Operations
^^^^^^^^^^^^^^^^

We've already seen some operations on vector elements in the last
sections. In this section, we'll see operations on the vector as a
whole. The most prominent example is the concatenation of multiple
vectors. Also, we'll see operations on a vector as a sequence of elements,
i.e. considering their relation to each other. Examples are sorting and
sorted merging operations.

Vector concatenation is done by using the :ada:`&` operator on vectors.
Let's consider two vectors ``V1`` and ``V2``. We can concatenate them with
the following statement: :ada:`V := V1 & V2`. ``V`` will contain the
resulting vector.

The generic package ``Generic_Sorting`` is a child package of
``Ada.Containers.Vectors``. It contains sorting and merging operations.
Because it's a generic package, we cannot use it directly, but have to
instantiate it, too. Also, in order to use these operations in our vector
of integer values (``Integer_Vectors``), we need to instantiate it
directly as a child from ``Integer_Vectors``. The next example will make
this clear.

After the instantiation of ``Generic_Sorting`` is done, we can make all
operations available with the :ada:`use` statement. We can then proceed to
call ``Sort`` in order to sort the vector and ``Merge`` in order to merge
one vector into another one.

The following code example presents an application that operates on three
vectors (``V1``, ``V2``, ``V3``) and uses the concatenation, sorting and
sorted merging operations:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Ops is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

       use Integer_Vectors;
       use Integer_Vectors_Sorting;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has " & Count_Type'Image (V.Length) & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V, V1, V2, V3 : Vector;
    begin
       V1 := 10 & 12 & 18;
       V2 := 11 & 13 & 19;
       V3 := 15 & 19;

       New_Line;
       Put_Line ("---- V1 ----");
       Show_Elements (V1);

       New_Line;
       Put_Line ("---- V2 ----");
       Show_Elements (V2);

       New_Line;
       Put_Line ("---- V3 ----");
       Show_Elements (V3);

       New_Line;
       Put_Line ("Concatenating V1, V2 and V3 into V:");

       V := V1 & V2 & V3;

       Show_Elements (V);

       New_Line;
       Put_Line ("Sorting V:");

       Sort (V);

       Show_Elements (V);

       New_Line;
       Put_Line ("Merging V2 into V1:");

       Merge (V1, V2);

       Show_Elements (V1);

    end Show_Vector_Ops;

According to the Reference Manual, the worst-case complexity of a call to
``Sort`` should be O(:math:`N^2`), and the average complexity should be better
than O(:math:`N^2`).

Sets
~~~~

Sets are another class of containers. While vectors allow for duplicated
elements to be inserted, sets ensure that no duplicated elements exist at
any moment.

In the following subsections, we'll look into operations that can be
performed on sets. However, since many of the operations on vectors are
similar to the ones used for sets, we'll cover them more quickly here.
Please refer to the section on vectors for a more detailed discussion.

Initialization and iteration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to initialize a set, we can call the ``Insert`` procedure.
However, we need to make sure that no duplicated element is being
inserted. Otherwise, we'll get an exception. If we have less control
about the elements to be inserted (e.g.: when elements are being passed
as arguments and inserted into a set), we might want to use one of the
following options:

- use a version of ``Insert`` that provides an output Boolean value
  indicating whether the insertion was successful;

- use the ``Include`` procedure, which silently ignores any attempt to
  insert a duplicated element.

In order to iterate over a set, we can use a :ada:`for E of S` loop, as
we did for vectors. This will give us a reference to each element from
the set.

Let's see an example:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Init is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

       use Integer_Sets;

       S : Set;
       C : Cursor;
       Ins : Boolean;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       --  Calling S.Insert(0) now would raise Constraint_Error exception
       --  because this element is already in the set
       --  We can call a version of Insert that does not raise an exception,
       --  but returns a Boolean indicating the status

       S.Insert (0, C, Ins);
       if not Ins then
          Put_Line ("Inserting 0 into set was not successful");
       end if;

       --  We can also call S.Include instead
       --  If the element is already available, the set remains the same
       S.Include (0);
       S.Include (13);
       S.Include (14);

       Put_Line ("Set has " & Count_Type'Image (S.Length) & " elements");

       --
       --  Iterate over set using for .. of loop
       --
       Put_Line ("Elements:");
       for E of S loop
           Put_Line("- " & Integer'Image(E));
       end loop;
    end Show_Set_Init;

Operations on elements
^^^^^^^^^^^^^^^^^^^^^^

In this section, we will briefly look into the following operations on
sets:

- ``Delete`` and ``Exclude`` to remove elements;

- ``Contains`` and ``Find`` to verify the existence of elements.

In order to delete elements, we can call the procedure ``Delete``.
However, similarly to the situation we've seen with ``Insert``
in the previous section, ``Delete`` raises an exception when
the element to be deleted doesn't exist in the set. Therefore, in case
an element might not exist in a set, we can call ``Exclude``, which
silently ignores any attempt to delete a non-existent element.

``Contains`` returns a Boolean value indicating whether a value is
contained in the set. ``Find`` also looks for an element in a set, but
returns a cursor to the element or ``No_Element`` in case the element
doesn't exist in the set. Both functions can be used to search for
elements in a set.

Let's look at an example that makes use of these operations:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Element_Ops is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          New_Line;
          Put_Line ("Set has " & Count_Type'Image (S.Length) & " elements");
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       S : Set;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       S.Delete (13);

       --  Calling S.Delete (13) again raises a Constraint_Error
       --  exception because the element does not exist anymore
       --  in the set, so it cannot be deleted.
       --  We can call V.Exclude instead:
       S.Exclude (13);

       if S.Contains (20) then
          Put_Line ("Found element 20 in set");
       end if;

       --  Alternatively, we could use S.Find instead of S.Contains
       if S.Find (0) /= No_Element then
          Put_Line ("Found element 0 in set");
       end if;

       Show_Elements (S);
    end Show_Set_Element_Ops;

In addition to ordered sets used in the examples above, the standard
library also offers hashed sets. According to the Reference Manual, this
is the average complexity of some operations:

+-----------------------+----------------------+------------------+
| Operations            | ``Ordered_Sets``     | ``Hashed_Sets``  |
+=======================+======================+==================+
| - Insert              | O(:math:`(log N)^2)` | :math:`O(log N)` |
| - Include             | or better            |                  |
| - Replace             |                      |                  |
| - Delete              |                      |                  |
| - Exclude             |                      |                  |
| - Find                |                      |                  |
+-----------------------+----------------------+------------------+
| Subprogram using      | O(:math:`1`)         | O(:math:`1`)     |
| cursor                |                      |                  |
+-----------------------+----------------------+------------------+

Other Operations
^^^^^^^^^^^^^^^^

The previous sections dealt with operations on individual elements of a
set. We'll now look into typical set operations: union, intersection,
difference and symmetric difference. In contrast to some vector
operations we've seen before (e.g. ``Merge``), we can use built-in
operators, such as :ada:`-`. The following table lists the operations and
the associated operator:

+-----------------------+--------------------------------+
| Set Operation         | Operator                       |
+=======================+================================+
| Union                 | :ada:`and`                     |
+-----------------------+--------------------------------+
| Intersection          | :ada:`or`                      |
+-----------------------+--------------------------------+
| Difference            | :ada:`-`                       |
+-----------------------+--------------------------------+
| Symmetric difference  | :ada:`xor`                     |
+-----------------------+--------------------------------+

The following example makes use of these operators:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Ops is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       procedure Show_Op (S       : Set;
                          Op_Name : String) is
       begin
          New_Line;
          Put_Line (Op_Name & "(set #1, set #2) has "
                    & Count_Type'Image (S.Length) & " elements");
       end Show_Op;

       S1, S2, S3 : Set;
    begin
       S1.Insert (0);
       S1.Insert (10);
       S1.Insert (13);

       S2.Insert (0);
       S2.Insert (10);
       S2.Insert (14);

       S3.Insert (0);
       S3.Insert (10);

       New_Line;
       Put_Line ("---- Set #1 ----");
       Show_Elements (S1);

       New_Line;
       Put_Line ("---- Set #2 ----");
       Show_Elements (S2);

       New_Line;
       Put_Line ("---- Set #3 ----");
       Show_Elements (S3);

       New_Line;
       if S3.Is_Subset (S1) then
          Put_Line ("S3 is a subset of S1");
       else
          Put_Line ("S3 is not a subset of S1");
       end if;

       S3 := S1 and S2;
       Show_Op (S3, "Union");
       Show_Elements (S3);

       S3 := S1 or S2;
       Show_Op (S3, "Intersection");
       Show_Elements (S3);

       S3 := S1 - S2;
       Show_Op (S3, "Difference");
       Show_Elements (S3);

       S3 := S1 xor S2;
       Show_Op (S3, "Symmetric difference");
       Show_Elements (S3);

    end Show_Set_Ops;

Indefinite maps
~~~~~~~~~~~~~~~

In the previous sections, we dealt with containers for elements of
definite types. In most source-code examples of those sections, we've
seen :ada:`Integer` types as element type of the containers. An
example of indefinite type is a :ada:`String` type. Indefinite types
require a different kind of containers specially designed for them.

Also, we will be looking into a different class of containers: maps. They
allow for associating a key to a specific value. An example of a map is
the one-to-one association between a person and its age. If we consider
a person's name to be the key, the value would be the person's age.

Hashed maps
^^^^^^^^^^^

Hashed maps are maps that make use of a hash as a key. The hash itself is
calculated by a function indicated by the programmer.

.. admonition:: In other languages

    Hashed maps are similar to dictionaries in Python and hashes in Perl.
    One of the main differences is that these scripting languages allow
    for using different types for the values contained in a map, while in
    Ada, the type of both key and value is specified in the package
    instantiation and cannot be changed for that specific map. In order
    to use multiple types, different maps need to be specified.

When instantiating a hashed map from
``Ada.Containers.Indefinite_Hashed_Maps``, we need to specify following
elements:

- ``Key_Type``: type of the key

- ``Element_Type``: type of the element

- ``Hash``: hash function for the ``Key_Type``

- ``Equivalent_Keys``: an equality operator (e.g. ``=``) that is used
  to determine if two keys are equal.

  - If the type specified in ``Key_Type`` has a standard operator, we
    might use it.

In the next example, we'll use a string as a key type. Also, we'll use the
``Hash`` function provided by the standard library for strings (in the
``Ada.Strings`` package).

We can include elements into a hashed map by calling ``Insert``. If an
element is already contained in a map ``M``, we can access it directly by
using its key. For example, we may change the value of an element by
calling :ada:`M ("My_Key") := 10`. If the key is not found, however, an
exception is raised. In order to verify if a key is available, we can use
the function ``Contains`` (as we've seen in the section about sets).

Let's see an example:

.. code-block:: ada

    with Ada.Containers.Indefinite_Hashed_Maps;
    with Ada.Strings.Hash;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Hashed_Map is

       package Integer_Hashed_Maps is new
         Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => String,
            Element_Type    => Integer,
            Hash            => Ada.Strings.Hash,
            Equivalent_Keys => "=");

       use Integer_Hashed_Maps;

       M : Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M.
       --  Otherwise, an exception will be raised.
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": " & Integer'Image (M (C)));
       end loop;

    end Show_Hashed_Map;

Ordered maps
^^^^^^^^^^^^

Ordered maps share many features with hashed maps. The main differences
are:

- A hash function is not needed. Instead, you need to provide an ordering
  function (``<`` operator). The ordered map will then use it to order
  elements, and allow pretty fast access --- O (log n) using a binary
  search.

  - If the type specified in ``Key_Type`` has a standard ``<`` operator,
    we might use it.

Let's see an example:

.. code-block:: ada

    with Ada.Containers.Indefinite_Ordered_Maps;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Ordered_Map is

       package Integer_Ordered_Maps is new
         Ada.Containers.Indefinite_Ordered_Maps
           (Key_Type        => String,
            Element_Type    => Integer);

       use Integer_Ordered_Maps;

       M : Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": " & Integer'Image (M (C)));
       end loop;

    end Show_Ordered_Map;

There is a great similarity between the example above and the one from the
previous section. In fact, since maps share many operations, we
don't need to make extensive adaptations in order to change our example
to use ordered maps instead of hashed maps. The main difference we notice
here is when we run the applications: while the output of a hashed map
is usually unordered, the output of a ordered map is always ordered, as
its name implies.

Complexity
^^^^^^^^^^

Hashed maps are generally the fastest data structure available to you in
Ada if you need to associate heterogeneous keys to values and search for
them quickly. In most cases, they are slightly faster than ordered maps.
Therefore, if you don't need ordering, prefer hashed maps.

According to the Reference Manual, this is the average complexity of some
operations:

+-----------------------+----------------------+------------------+
| Operations            | ``Ordered_Maps``     | ``Hashed_Maps``  |
+=======================+======================+==================+
| - Insert              | O(:math:`(log N)^2)` | :math:`O(log N)` |
| - Include             | or better            |                  |
| - Replace             |                      |                  |
| - Delete              |                      |                  |
| - Exclude             |                      |                  |
| - Find                |                      |                  |
+-----------------------+----------------------+------------------+
| Subprogram using      | O(:math:`1`)         | O(:math:`1`)     |
| cursor                |                      |                  |
+-----------------------+----------------------+------------------+

Dates & Times
-------------

Date and time handling
~~~~~~~~~~~~~~~~~~~~~~

The standard library provides support for representing and handling date
and time. This is part of the :ada:`Ada.Calendar` package. Let's look at
a simple example:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Time is
       Now : Time := Clock;
    begin
       Put_Line ("Current time: " & Image (Now));
    end Display_Current_Time;

This application displays the current date and time, which is retrieved
by a call to the ``Clock`` function. We call the function ``Image`` from
the :ada:`Ada.Calendar.Formatting` package in order to get a ``String``
for the current date and time. We could, instead, retrieve each component
using the ``Split`` function. For example:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Year is
       Now         : Time := Clock;

       Now_Year    : Year_Number;
       Now_Month   : Month_Number;
       Now_Day     : Day_Number;
       Now_Seconds : Day_Duration;
    begin
       Split (Now,
              Now_Year,
              Now_Month,
              Now_Day,
              Now_Seconds);

       Put_Line ("Current year  is: " & Year_Number'Image (Now_Year));
       Put_Line ("Current month is: " & Month_Number'Image (Now_Month));
       Put_Line ("Current day   is: " & Day_Number'Image (Now_Day));
    end Display_Current_Year;

Here, we're retrieving each element and displaying it separately.

Delaying using date
^^^^^^^^^^^^^^^^^^^

We can delay an application, so that it restarts at a specific date and
time. This is achieved by using a :ada:`delay until` statement. For
example:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        := Ada.Calendar.Formatting.Time_Of
         (Year        => 2018,
          Month       => 5,
          Day         => 1,
          Hour        => 15,
          Minute      => 0,
          Second      => 0,
          Sub_Second  => 0.0,
          Leap_Second => False,
          Time_Zone   => TZ);

       --  Next = 2018-05-01 15:00:00.00 (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this application, the date and time is specified by initializing
``Next`` with a call to ``Time_Of``. This function takes the various
components of a date (year, month, etc) and returns an element of ``Time``
type. Because the date specified in the application is in the past, the
:ada:`delay until` statement won't produce any noticeable effect. However,
if we'd use a date in the future, the application would be waiting until
that specific date and time would have arrived.

Note that, in the application above, we're taking care of adapting the
time to the local time-zone. By default, *Coordinated Universal Time*
(abbreviated to UTC) is used. By retrieving the time offset to UTC with a
call to ``UTC_Time_Offset`` from the :ada:`Ada.Calendar.Time_Zones`
package, we can initialize ``TZ`` and use it in the call to ``Time_Of``.
This is all we need in order to make the information provided to
``Time_Of`` relative to the local time zone.

We could achieve a similar result by initializing ``Next`` with a
``String``. This can be done by a call to ``Value`` from the
:ada:`Àda.Calendar.Formatting` package. This is the adapted application:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        := Ada.Calendar.Formatting.Value
         ("2018-05-01 15:00:00.00", TZ);

       --  Next = 2018-05-01 15:00:00.00 (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this example, we're again using ``TZ`` in the call to ``Value`` in
order to adjust the input time to the current time zone.

In the examples above, we were delaying the application to a specific
date and time. We could, however, delay the application relatively to the
current time. For example, we could delay the application by 5 seconds
using the current time:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next is
       D    : Duration := 5.0;       --  seconds
       Now  : Time     := Clock;
       Next : Time     := Now + D;   --  use duration to
                                     --  specify next point in time
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (D) & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next;

Here, we're specifying a duration of 5 seconds in ``D``, adding it to
the current time from ``Now`` and storing the sum in ``Next``. This will
then be used for the :ada:`delay until` statement.

Real-time
~~~~~~~~~

In addition to :ada:`Ada.Calendar`, the standard library also supports
time operations for real-time applications. This is implemented by the
:ada:`Ada.Real_Time` package. This package also include a ``Time`` type.
However, in the :ada:`Ada.Real_Time` package, the ``Time`` type is used
to represent an absolute clock and handle time span. This contrasts with
the :ada:`Ada.Calendar`, which uses the ``Time`` type to represent dates
and times.

In the previous section, we used the ``Time`` type from the
:ada:`Ada.Calendar` and the :ada:`delay until` statement to delay an
application by 5 seconds. We could have used the :ada:`Ada.Real_Time`
package instead. Let's adapt that example:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Delay_Next_Real_Time is
       D     : Time_Span := Seconds (5);
       Next  : Time      := Clock + D;
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (To_Duration (D)) & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Real_Time;

The main difference is that ``D`` is now a variable of type ``Time_Span``,
which is defined in the :ada:`Ada.Real_Time` package. We call the function
``Seconds`` to initialize ``D``. We could have a finer granularity by
calling ``Nanoseconds`` instead. Also, we need to first convert ``D`` to
the ``Duration`` type using ``To_Duration`` before we can display it.

Benchmarking
^^^^^^^^^^^^

One interesting application using the :ada:`Ada.Real_Time` package is
benchmarking. We've used it before in a previous section when discussed
tasking. Let's look at an example of benchmarking:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Benchmarking is

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("Elapsed time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking;

This example defines a dummy ``Computational_Intensive_App`` that is
implemented using a simple :ada:`delay` statement. We initialize
``Start_Time`` and ``Stop_Time`` with the current clock and calculate the
elapsed time. By running this application, we see that the elapsed time is
roughly 5 seconds, as expected due to the :ada:`delay` statement.

A similar application is benchmarking of CPU time. This can be implemented
by using the :ada:`Execution_Time`. Let's adapt the previous example to
measure CPU time:

.. code-block:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    procedure Display_Benchmarking_CPU_Time is

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : CPU_Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("CPU time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking_CPU_Time;

In this example, ``Start_Time`` and ``Stop_Time`` are of type ``CPU_Time``
instead of ``Time``. However, we still call the ``Clock`` function to
initialize both variables and calculate the elapsed time in the same way
as before. By running this application, we see that the CPU time is
significantly lower than the 5 seconds we've seen before. This is caused
by the fact that the :ada:`delay` statement doesn't require much CPU time.
Results are different if we adapt the implementation of
``Computational_Intensive_App`` to make use of mathematical functions in
a long loop. For example:

.. code-block:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    with Ada.Numerics.Generic_Elementary_Functions;

    procedure Display_Benchmarking_Math is

       procedure Computational_Intensive_App is
          package Funcs is new Ada.Numerics.Generic_Elementary_Functions
            (Float_Type => Long_Long_Float);
          use Funcs;

          X : Long_Long_Float;
       begin
          for I in 0 .. 5_000_000 loop
             X := Tan (Arctan
                       (Tan (Arctan
                          (Tan (Arctan
                             (Tan (Arctan
                                (Tan (Arctan
                                   (Tan (Arctan
                                      (0.577))))))))))));
          end loop;
       end Computational_Intensive_App;

       procedure Benchm_Elapsed_Time is
          Start_Time, Stop_Time : Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("Elapsed time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_Elapsed_Time;

       procedure Benchm_CPU_Time is
          Start_Time, Stop_Time : CPU_Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("CPU time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_CPU_Time;
    begin
       Benchm_Elapsed_Time;
       Benchm_CPU_Time;
    end Display_Benchmarking_Math;

Now that our dummy ``Computational_Intensive_App`` makes use of
mathematical operations that require significant CPU time, the measured
elapsed and CPU time should be much closer to each other than before.

Strings
--------

In previous sections, we've used strings in many source-code examples. In
this section, we will discuss them in more details.

String operations
~~~~~~~~~~~~~~~~~

Operations on standard strings are available in the
:ada:`Ada.Strings.Fixed` package. As mentioned in previous sections,
standard strings are arrays of elements of :ada:`Character` type with *a
fixed-length*. That's why this child package is called ``Fixed``.

One of the most simple operations available is counting the number of
substrings available in a string (using ``Count``) and finding their
corresponding indices (using ``Index``). Let's look at an example:

.. code-block:: ada

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Substring is

       S   : String := "Hello" & 3 * " World";
       P   : constant String := "World";
       Idx : Natural;
       Cnt : Natural;
    begin
       Cnt := Ada.Strings.Fixed.Count
         (Source  => S,
          Pattern => P);

       Put_Line ("String: " & S);
       Put_Line ("Count for '" & P & "': " & Natural'Image (Cnt));

       Idx := 0;
       for I in 1 .. Cnt loop
          Idx := Index
            (Source  => S,
             Pattern => P,
             From    => Idx + 1);

          Put_Line ("Found instance of '" & P & "' at position: "
                    & Natural'Image (Idx));
       end loop;

    end Show_Find_Substring;

Notice that we initialize the string ``S`` using a multiplication. Writing
:ada:`"Hello" & 3 * " World"` creates the string
``Hello World World World``. We then use the function ``Count`` to get the
number of instances of the word ``World`` in ``S``.  Next, we use the
function ``Index`` in a loop to find the index of each instance of
``World`` in ``S``.

In the previous example, we looked for instances of a specific substring.
In the next example, we will retrieve all the words in the string. We can
do this by using ``Find_Token`` and using whitespaces as separators. For
example:

.. code-block:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Words is

       S   : String := "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant Character_Set :=
         To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: " & Integer'Image (S'Length));

       while I in S'Range loop
          Find_Token
            (Source  => S,
             Set     => Whitespace,
             From    => I,
             Test    => Outside,
             First   => F,
             Last    => L);

          exit when L = 0;

          Put_Line ("Found word instance at position "
                    & Natural'Image (F)
                    & ": '" & S (F .. L) & "'");
          --   & "-" & F'Img & "-" & L'Img

          I := L + 1;
       end loop;
    end Show_Find_Words;

The procedure ``Find_Token`` requires a set of characters that will be
used as delimitators. This is represented by the ``Character_Set`` from
the :ada:`Ada.Strings.Maps` package. We use the ``To_Set`` function (from
the same package) to initialize the set in ``Whitespace``. We then call
``Find_Token`` in a loop over valid indices to find the starting index of
each word. For the ``Test`` parameter of the ``Find_Token`` procedure, we
pass ``Outside`` as an argument, which indicates that we're looking for
indices that are outside the ``Whitespace`` set, i.e. actual words. The
``First`` and ``Last`` parameters of ``Find_Token`` are output parameters
that indicate the valid range of the substring. We can use this
information to display the string (:ada:`S (F .. L)`).

The operations that we've looked so far only iterate over strings, but
don't modify them. We will now look into following operations that change
the content of strings:

+-----------------------+-----------------------------------------+
| Operation             | Description                             |
+=======================+=========================================+
| Insert                | Insert substring in a string            |
+-----------------------+-----------------------------------------+
| Overwrite             | Overwrite a string with a substring     |
+-----------------------+-----------------------------------------+
| Delete                | Delete a substring                      |
+-----------------------+-----------------------------------------+
| Trim                  | Remove whitespaces from a string        |
+-----------------------+-----------------------------------------+

Note that all these operations are available as functions or procedures.
In case functions are used, a new string is created. If we use a
procedure, however, the operations are performed in place. In this case,
we need to make sure that the string constraints are respected. Otherwise,
the procedure might raise an exception. For example, let's say we have a
string ``S`` containing 10 characters. Inserting a string with two
characters  (e.g. ``"!!"``) into ``S`` would require a string containing
12 characters. Since ``S`` has a fixed length, we cannot increase its
size. The easiest solution in this case is to specify that truncation
should be applied while inserting the substring, so that the length of
``S`` remains fixed. Let's see an example that makes use of both function
and procedure version of ``Insert``, ``Overwrite`` and ``Delete``:

.. code-block:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Adapted_Strings is

       S   : String := "Hello World";
       P   : constant String := "World";
       N   : constant String := "Beautiful";

       procedure Display_Adapted_String
         (Source   : String;
          Before   : Positive;
          New_Item : String;
          Pattern  : String)
       is
          S_Ins_In : String := Source;
          S_Ovr_In : String := Source;
          S_Del_In : String := Source;

          S_Ins : String := Insert (Source, Before, New_Item & " ");
          S_Ovr : String := Overwrite (Source, Before, New_Item);
          S_Del : String := Trim (Delete (Source,
                                          Before,
                                          Before + Pattern'Length - 1),
                                  Ada.Strings.Right);
       begin
          Insert (S_Ins_In,    Before, New_Item, Right);
          Overwrite (S_Ovr_In, Before, New_Item, Right);
          Delete (S_Del_In,    Before, Before + Pattern'Length - 1);

          Put_Line ("Original:  '" & Source & "'");

          Put_Line ("Insert:    '" & S_Ins  & "'");
          Put_Line ("Overwrite: '" & S_Ovr  & "'");
          Put_Line ("Delete:    '" & S_Del  & "'");

          Put_Line ("Insert    (in-place): '" & S_Ins_In & "'");
          Put_Line ("Overwrite (in-place): '" & S_Ovr_In & "'");
          Put_Line ("Delete    (in-place): '" & S_Del_In & "'");
       end Display_Adapted_String;

       Idx : Natural;
    begin
       Idx := Index
         (Source  => S,
          Pattern => P);

       if Idx > 0 then
          Display_Adapted_String (S, Idx, N, P);
       end if;
    end Show_Adapted_Strings;

In this example, we look for the index of the substring ``World`` and
perform operations on this substring. The procedure
``Display_Adapted_String`` makes use of both versions of the operations.
For the procedural version of ``Insert`` and ``Overwrite``, truncation is
applied on the right side of the string (``Right``). For the ``Delete``
procedure, we specify the range of the substring, which is substituted by
whitespaces. For the function version of ``Delete``, we call ``Trim`` in
addition, which trims the trailing whitespaces.

Bounded and unbounded strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using fixed-length strings is usually good enough for strings that are
initialized at declaration time. However, as seen in the previous section
with procedural operations on strings, we might encounter difficulties
when performing operations on fixed-length strings. This is mainly due
to the fact that fixed-length strings are essentially arrays of
characters. The following example shows how cumbersome the initialization
of fixed-length strings can be when it's not performed at declaration
time:

.. code-block:: ada

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Char_Array is
       S : String (1 .. 15);
       --  Strings are arrays of Character
    begin
       S := "Hello          ";
       --  Alternatively:
       --
       --  #1:
       --      S (1 .. 5)      := "Hello";
       --      S (6 .. S'Last) := (others => ' ');
       --
       --  #2:
       --      S := ('H', 'e', 'l', 'l', 'o', others => ' ');

       Put_Line ("String: " & S);
       Put_Line ("String Length: " & Integer'Image (S'Length));
    end Show_Char_Array;

In this case, we cannot simply write :ada:`S := "Hello"`, because the
resulting array of characters for the ``Hello`` string would have a
different length than the ``S`` string. Therefore, we need to include the
trailing whitespaces in order to match the length of ``S``. As indicated
in the example, we could use an exact range for the initialization (
:ada:`S (1 .. 5)`) or use an explicit array of individual characters.

When strings are initialized or manipulated at run-time, it's usually
better to use bounded or unbounded strings. An important feature of these
types is that they are not arrays, so that they are not affected by the
difficulties presented in the previous example. Let's start by looking
into bounded strings.

Bounded strings
^^^^^^^^^^^^^^^

Bounded strings are defined in the
:ada:`Ada.Strings.Bounded.Generic_Bounded_Length` package. Because this is
a generic package, we need to instantiate it first and set the maximum
length of the bounded strings. We can then declare bounded strings using
the ``Bounded_String`` type.

Both bounded strings and fixed-length strings have a maximum length that
they can store. However, because bounded strings are not arrays,
initializing them at run-time is much easier. For example:

.. code-block:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 15);
       use B_Str;

       S1, S2 : Bounded_String;

       procedure Display_String_Info (S : Bounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: " & Integer'Image (Length (S)));
          --  String:         S'Length => ok
          --  Bounded_String: S'Length => compilation error
          --                              bounded strings are not arrays!

          Put_Line ("Max.   Length: " & Integer'Image (Max_Length));
       end Display_String_Info;
    begin
       S1 := To_Bounded_String ("Hello");
       Display_String_Info (S1);

       S2 := To_Bounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Bounded_String ("Something longer to say here...",
                                Right);
       Display_String_Info (S1);
    end Show_Bounded_String;

By using bounded strings, we can easily initialize ``S1`` and ``S2``
multiple times at run-time. We use the ``To_Bounded_String`` and
``To_String`` functions to convert back-and-forth between fixed-length
strings and bounded strings. In case of ``To_Bounded_String``, if the
length of the input string is greater than the maximum capacity of the
bounded string, an exception will be raised. In order to avoid this, we
can use the truncation parameter (``Right`` in our example).

Also note that, because bounded strings are not arrays, we cannot use the
:ada:`'Length` attribute as we did for fixed-length strings. In this case,
we call the ``Length`` function, which returns the length of the string
stored in the bounded string. In order to get the maximum length of the
bounded string, we can use the ``Max_Length`` constant.

After initializing bounded strings, we may manipulate them. For example,
we may append a string to a bounded string using ``Append`` or concatenate
bounded strings using the :ada:`&`  operator. For example:

.. code-block:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String_Op is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 30);
       use B_Str;

       S1, S2 : Bounded_String;
    begin
       S1 := To_Bounded_String ("Hello");
       --  Alternatively: A := Null_Bounded_String & "Hello";
       Append (S1, " World");
       --  Alternatively: Append (A, " World", Right);
       Put_Line ("String: " & To_String (S1));

       S2 := To_Bounded_String ("Hello!");
       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Bounded_String_Op;

Note that we may initialize a bounded string with an empty string using
the ``Null_Bounded_String`` constant. Also note that the ``Append``
procedure allows for specifying the truncation mode --- similar to the
``To_Bounded_String`` function.

Unbounded strings
^^^^^^^^^^^^^^^^^

Unbounded strings are defined in the :ada:`Ada.Strings.Unbounded` package.
Since this is not a generic package, we don't need to instantiate a
package before using the ``Unbounded_String`` type. As you may recall from
the previous section, bounded strings require a package instantiation.

In general, unbounded strings are similar to bounded strings. The main
difference is that they can hold strings of any size and adapt according
to the input string: if we initialize an unbounded string with e.g. a
10-character string and then with a 50-character string, internal
operations in the container will make sure that memory is allocated to
store the new string. In most cases, developers don't need to worry about
these operations. Also, no truncation is necessary in this case.

Initialization of unbounded strings is very similar to bounded strings.
Let's look at the example:

.. code-block:: ada

    with Ada.Strings;           use Ada.Strings;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String is
       S1, S2 : Unbounded_String;

       procedure Display_String_Info (S : Unbounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: " & Integer'Image (Length (S)));
       end Display_String_Info;
    begin
       S1 := To_Unbounded_String ("Hello");
       --  Alternatively: A := Null_Unbounded_String & "Hello";
       Display_String_Info (S1);

       S2 := To_Unbounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Unbounded_String ("Something longer to say here...");
       Display_String_Info (S1);
    end Show_Unbounded_String;

Similar to bounded strings, we can easily initialize ``S1`` and ``S2``
multiple times at run-time and use the ``To_Unbounded_String`` and
``To_String`` functions to convert back-and-forth between fixed-length
strings and unbounded strings. However, in this case, truncation is not
needed.

Similar to bounded strings, we can use the ``Append`` function and the
:ada:`&` operator for unbounded strings. For example:

.. code-block:: ada

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String_Op is
       S1, S2 : Unbounded_String := Null_Unbounded_String;
    begin
       S1 := S1 & "Hello";
       S2 := S2 & "Hello!";

       Append (S1, " World");
       Put_Line ("String: " & To_String (S1));

       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Unbounded_String_Op;

String encoding and wide strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For applications that deal with characters other than the Latin alphabet,
we need to make use of wider precision for character types and encodings
that are suitable for these characters. The following sections will
discuss these topics.

Wide strings
^^^^^^^^^^^^

Standard characters are represented by the :ada:`Character` type and its
corresponding array format (:ada:`String`). In order to represent
characters with wider precision, we need to make use of the
:ada:`Wide_Character` and :ada:`Wide_Wide_Character` types. The
corresponding string types are :ada:`Wide_String` and
:ada:`Wide_Wide_String`, respectively. The following example shows that,
although strings using wider precision have the same length, their sizes
are different due to the precision used for each character:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Wide_Text_IO;
    with Ada.Wide_Wide_Text_IO;

    procedure Show_Wide_String is
       package TI   renames Ada.Text_IO;
       package WTI  renames Ada.Wide_Text_IO;
       package WWTI renames Ada.Wide_Wide_Text_IO;

       S   : String           := "hello";
       WS  : Wide_String      := "hello";
       WWS : Wide_Wide_String := "hello";
    begin
       TI.Put_Line ("String:           " & S);
       TI.Put_Line ("Length:           " & Integer'Image (S'Length));
       TI.Put_Line ("Size:             " & Integer'Image (S'Size));
       TI.New_Line;

       WTI.Put_Line ("Wide string:      " & WS);
       TI.Put_Line ("Length:           " & Integer'Image (WS'Length));
       TI.Put_Line ("Size:             " & Integer'Image (WS'Size));
       TI.New_Line;

       WWTI.Put_Line ("Wide-wide string: " & WWS);
       TI.Put_Line ("Length:           " & Integer'Image (WWS'Length));
       TI.Put_Line ("Size:             " & Integer'Image (WWS'Size));
       TI.New_Line;
    end Show_Wide_String;

UTF encoding
^^^^^^^^^^^^

Unicode is one of the most widespread standards for encoding writing
systems other than the Latin alphabet. It defines a format called
Unicode Transformation Format (UTF) in various versions, which vary
according to the underlying precision, support for backwards-compatibility
and other requirements.

A common UTF format is UTF-8, which encodes strings using up to four
(8-bit) bytes and is backwards-compatible with the ASCII format. While
encoding of ASCII characters requires only one byte, Chinese characters
require three bytes.

In Ada applications, UTF-8 strings are indicated by using the
:ada:`UTF_8_String` from the :ada:`Ada.Strings.UTF_Encoding` package.
In order to encode from and to UTF-8 strings, we can use the ``Encode``
and ``Decode`` functions from the child packages
:ada:`Strings`, :ada:`Wide_Strings` and :ada:`Wide_Wide_Strings`,
depending on the precision of the string type that we're using. Let's look
at an example:

.. code-block:: ada

    with Ada.Text_IO;                       use Ada.Text_IO;
    with Ada.Strings.UTF_Encoding;          use Ada.Strings.UTF_Encoding;
    with Ada.Strings.UTF_Encoding.Strings;  use Ada.Strings.UTF_Encoding.Strings;

    with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
    use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

    with Ada.Wide_Wide_Text_IO;

    procedure Show_WW_UTF_String is
       package WWT_IO renames Ada.Wide_Wide_Text_IO;

       S8       : UTF_8_String := "World: العالَم";
       WWS      : Wide_Wide_String := Decode (S8);
    begin
       Put_Line ("Encoding: " & Encoding (S8)'Image);

       WWT_IO.Put_Line ("String (Wide_Wide): " & WWS);
       Put_Line ("Length: " & Integer'Image (WWS'Length));
       New_Line;

       Put_Line ("String (UTF-8):     " & S8);
       Put_Line ("Length: " & Integer'Image (S8'Length));
       New_Line;

       Put_Line ("Converting Wide_Wide_String to UTF-8...");
       New_Line;
       declare
          S8 : UTF_8_String := Encode (WWS);
       begin
          Put_Line ("String (UTF-8):     " & S8);
          Put_Line ("Length: " & Integer'Image (S8'Length));
       end;
       New_Line;

       --  Display individual characters from string
       Put ("String:     ");
       for I in WWS'Range loop
          declare
             S8 : UTF_8_String := Encode (WWS (I .. I));
          begin
             Put (S8 & " ");
          end;
       end loop;
       New_Line;

    end Show_WW_UTF_String;

By running this application, we notice that, although our input string
has 14 characters in total, the corresponding UTF-8 string encoding
requires 21 bytes. The 8-bit character representation used by the
:ada:`UTF_8_String` type is good enough for storing information. However,
it is not suitable for direct string manipulation. In this case, we need
to use the :ada:`Wide_String` and :ada:`Wide_Wide_String` types. In our
example, when converting ``S8`` to ``WWS`` with a call to ``Decode``, we
create a new string that correctly stores the individual characters. Also,
we can convert back-and-forth between :ada:`UTF_8_String` and
:ada:`Wide_Wide_String` types. In the last block of our example, we
display the individual characters from the string by encoding each
character into UTF-8 format and displaying it.

Files and streams
-----------------

This section presents the different options available in Ada for file
input/output (I/O).

Text I/O
~~~~~~~~

In many sections of this course, the ``Put_Line`` procedure was used to
display information on the console. However, this procedure also accepts
a ``File_Type`` parameter. For example, we may select between the standard
output and the standard error by setting this parameter explicitly:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Std_Text_Out is
    begin
       Put_Line (Standard_Output, "Hello World #1");
       Put_Line (Standard_Error,  "Hello World #2");
    end Show_Std_Text_Out;

In addition, this parameter can be used to write information to any
text file. In order to create a new file for writing, we use the
``Create`` procedure. This procedure initializes a ``File_Type`` element
that can be used as argument for ``Put_Line`` (instead of e.g.
``Standard_Output``). After we finish writing information, we have to
close the file by calling the ``Close`` procedure.

A similar method is used for retrieving information from a text file.
However, when opening the file, we need to specify that it's an input
file (``In_File``) instead of an output file. Also, instead of calling the
``Put_Line`` procedure, we call the ``Get_Line`` function to retrieve
information from the file.

Let's see an example that writes information into a new text file and then
reads information from the same file:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Text_File_IO is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Put_Line (F, "Hello World #2");
       Put_Line (F, "Hello World #3");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Simple_Text_File_IO;

In addition to the ``Create`` and ``Close`` procedures, the standard
library also includes a ``Reset`` procedure, which, as the name implies,
resets all the information from the file. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Reset is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Reset (F);
       Put_Line (F, "Hello World #2");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Text_File_Reset;

By running this application, we notice that, although we've written the
first string (``Hello World #1``) to the file, it has been erased because
of the call to ``Reset``.

In addition to opening a file for reading or writing, we may also open an
existing file and append information to it. This can be done by calling
the ``Open`` procedure with the ``Append_File`` option.

When calling the ``Open`` procedure, we need to be aware that, if the
specified file is not found, an exception will be raised. Therefore, we
should handle exceptions in this context. The following application
deletes a file and then tries to open the same file for reading:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Input_Except is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       --  Open output file and delete it
       Create (F, Out_File, File_Name);
       Delete (F);

       --  Try to open deleted file
       Open (F, In_File, File_Name);
       Close (F);
    exception
       when Name_Error =>
          Put_Line ("File does not exist");
       when others =>
          Put_Line ("Error while processing input file");
    end Show_Text_File_Input_Except;

Note that, in this example, we create the file by calling ``Create`` and
then delete it by calling the ``Delete`` procedure. After a call to
``Delete``, we cannot use the ``File_Type`` element --- for this reason,
we don't have a call to ``Close`` after the call to ``Create``. After
deleting the file, we try to open the non-existent file, which raises a
``Name_Error`` exception.

Sequential I/O
~~~~~~~~~~~~~~

The previous section presented details about text file I/O. In this
section, we will discuss file I/O in binary format. The first package
we'll see is the :ada:`Ada.Sequential_IO` package. Because this package is
a generic package, we need to instantiate it for the data type we want to
use for file I/O. Apart from that, we can make use of the same
procedures we've seen in the previous section: ``Create``, ``Open``,
``Close``, ``Reset`` and ``Delete``. However, instead of calling the
``Get_Line`` and ``Put_Line`` procedures, we'll call the ``Read`` and
``Write`` procedures.

In the following example, we instantiate the :ada:`Ada.Sequential_IO`
package for floating-point types:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Float_IO is
       package Float_IO is new Ada.Sequential_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Seq_Float_IO;

We can use the same approach to read and write complex information. The
following example makes use of a record that includes a Boolean and a
floating-point element:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Rec_IO is
       type Num_Info is record
          Valid : Boolean := False;
          Value : Float;
       end record;

       procedure Put_Line (N : Num_Info) is
       begin
          if N.Valid then
             Ada.Text_IO.Put_Line ("(ok,     " & Float'Image (N.Value) & ")");
          else
             Ada.Text_IO.Put_Line ("(not ok,  -----------)");
          end if;
       end Put_Line;

       package Num_Info_IO is new Ada.Sequential_IO (Num_Info);
       use Num_Info_IO;

       F         : Num_Info_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  (True,  1.5));
       Write (F,  (False, 2.4));
       Write (F,  (True,  6.7));
       Close (F);

       declare
          Value : Num_Info;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Put_Line (Value);
          end loop;
          Close (F);
       end;
    end Show_Seq_Rec_IO;

As the example clearly shows, the same approach used for floating-point
types can be used to generate file I/O for this record. As soon as we
instantiate the :ada:`Ada.Sequential_IO` package for the record type, all
file I/O operations are the same.

Direct I/O
~~~~~~~~~~

Direct I/O is available in the :ada:`Ada.Direct_IO` package. This file I/O
approach is similar to the sequential I/O approach presented in the
previous section. For example, the package instantiation and most
operations are very similar. If we want to rewrite the
``Show_Seq_Float_IO`` application presented in the previous section to
make use of the :ada:`Ada.Direct_IO` package, we basically just need to
replace the instances of the :ada:`Ada.Sequential_IO` package by the
:ada:`Ada.Direct_IO` package. This is the adapted source-code:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_IO is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_IO;

The main difference between sequential I/O and direct I/O is that direct
I/O allows for accessing any position in the file. Also, it doesn't offer
an option to append information to a file. Instead, it has an
``Inout_File`` mode that allows for reading and writing to a file using
the same ``File_Type`` element.

In order to access any position in the file, we call the ``Set_Index``
procedure to set the new position / index. Also, we may use the ``Index``
function to retrieve the current index. Let's see an example:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_In_Out_File is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       --  Open file for input / output
       Create (F, Inout_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);

       --  Set index to previous position again and overwrite value
       Set_Index (F, Index (F) - 1);
       Write (F,  7.7);

       declare
          Value : Float;
       begin
          --  Set index to start of file
          Set_Index (F, 1);

          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_In_Out_File;

By running this example, we see that, although we've written ``6.7`` to
the file, this information was overwritten with the ``7.7`` value after
we changed the index to the previous position.

Note that, in this example, we make use of the ``Inout_File`` mode.
Instead of closing the file and reopening it for reading, we simply change
the index to the initial position before reading information from the
file (:ada:`Set_Index (F, 1)`)

Stream I/O
~~~~~~~~~~

The previous approaches for file I/O in binary format (sequential and
direct I/O) are specifically tailored for a single data type. For example,
we may select these approaches for writing complex records of a single
data type. However, as soon as we need to create and process files that
include different data types or unbounded types, these approaches are not
suitable anymore. Instead, we should use stream I/O in this case.

Stream I/O share some similarities with the previous approaches. For
example, we still make use of the ``Create``, ``Open`` and ``Close``
procedures. However, instead of accessing the file directly via a
``File_Type`` element, we use a ``Stream_Access`` element. Also, in order
to read and write information, we use the :ada:`'Read` and :ada:`'Write`
attributes of the data types.

Let's look at an adaptation of the ``Show_Dir_Float_IO`` procedure from
the previous section that makes use of stream I/O instead of direct I/O:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_Float_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Float'Write (S, 1.5);
       Float'Write (S, 2.4);
       Float'Write (S, 6.7);

       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          S := Stream (F);

          while not End_Of_File (F) loop
             Float'Read (S, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Float_Stream;

Note that, after the call to the ``Create`` procedure, we retrieve the
corresponding ``Stream_Access`` element by calling the ``Stream``
function. We then use this stream to write information to the file by
using the :ada:`'Write` attribute of the :ada:`Float` type. After closing
the file and reopening it for reading, we again retrieve the corresponding
``Stream_Access`` element. We then processed to read information from the
file by using the :ada:`'Read` attribute of the :ada:`Float` type.

As mentioned above, we can use streams to create and process files that
make use of different data types in the same file. Also, we may use
unbounded data types such as strings. When using unbounded data types,
however, we need to call the :ada:`'Input` and :ada:`'Output` attributes
of the unbounded data type because, in addition to writing the information
for the data type, these attributes include information about bounds or
discriminants in the file.

The following example shows file I/O that intercalates strings of
different lengths and floating-point values:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_String_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";

       procedure Output (S  : Stream_Access;
                         FV : Float;
                         SV : String) is
       begin
          String'Output (S, SV);
          Float'Output (S,  FV);
       end Output;

       procedure Input_Display (S : Stream_Access) is
          SV : String := String'Input (S);
          FV : Float  := Float'Input (S);
       begin
          Ada.Text_IO.Put_Line (Float'Image (FV) & " --- " & SV);
       end Input_Display;

    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Output (S, 1.5, "Hi!!");
       Output (S, 2.4, "Hello world!");
       Output (S, 6.7, "Something longer here...");

       Close (F);

       Open (F, In_File, File_Name);
       S := Stream (F);

       while not End_Of_File (F) loop
          Input_Display (S);
       end loop;
       Close (F);

    end Show_String_Stream;

We need to be aware that, when parsing a file that contains information
from different data types, we should make sure that the same order is used
as when the file was written. Otherwise, the information we'll get will
be corrupted. Unfortunately, strong data typing cannot directly help
software developers in this case. Writing simple procedures for file I/O
(as in the example above) may help ensuring that the file format is
consistent. Also note that, like direct I/O, stream I/O supports access to
any index in the file. Therefore, when changing the index, we need to make
sure that the new index is correctly aligned with the data types that
we're expecting.

Dynamic allocation and reclamation
----------------------------------

Appendices
==========

Appendix A: Generic Formal Types
--------------------------------

The following reference tables contain examples of available formal types
for generics.

+-------------------------+--------------------------------------------------------------+-------------------------+
| Formal Type             | Format                                                       | Actual type             |
+=========================+==============================================================+=========================+
| Incomplete type         | :ada:`type T;`                                               | Any type                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete type         | :ada:`type T (<>);`                                          | Any type                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Discrete type           | :ada:`type T is (<>);`                                       | Any integer, modular or |
|                         |                                                              | enumeration type        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Range type              | :ada:`type T is range <>;`                                   | Any signed integer type |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Modular type            | :ada:`type T is mod <>;`                                     | Any modular type        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Floating-point type     | :ada:`type T is digits <>;`                                  | Any floating-point type |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Binary fixed-point type | :ada:`type T is delta <>;`                                   | Any binary fixed-point  |
|                         |                                                              | type                    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Decimal fixed-point     | :ada:`type T is delta <> digits <>;`                         | Any decimal fixed-point |
| type                    |                                                              | type                    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite nonlimited     | :ada:`type T is private;`                                    | Any nonlimited,         |
| private type            |                                                              | definite type           |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite nonlimited   | :ada:`type T (<>) is private;`                               | Any nonlimited type     |
| private type            |                                                              | indefinite or definite  |
|                         |                                                              |                         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Unlimited private type  | :ada:`type T (D : DT) is private;`                           | Any nonlimited type     |
| with discriminant       |                                                              | with discriminant       |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Access type             | :ada:`type A is access T;`                                   | Any access type for     |
|                         |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite derived        | :ada:`type T is new B;`                                      | Any concrete type       |
| type                    |                                                              | derived from base type  |
|                         |                                                              | B                       |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited private type    | :ada:`type T is limited private;`                            | Any definite type,      |
|                         |                                                              | limited or not          |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete tagged       | :ada:`type T is tagged;`                                     | Any concrete, definite, |
| type                    |                                                              | tagged type             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite                | :ada:`type T is tagged private;`                             | Any concrete, definite, |
| tagged private type     |                                                              | tagged type             |
|                         |                                                              |                         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite                | :ada:`type T is tagged limited private;`                     | Any concrete definite   |
| tagged limited private  |                                                              | tagged type, limited or |
| type                    |                                                              | not                     |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract tagged private;`                    | Any nonlimited,         |
| tagged private type     |                                                              | definite tagged type,   |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract tagged limited private;`            | Any definite tagged     |
| tagged limited private  |                                                              | type, limited or not,   |
| type                    |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite derived        | :ada:`type T is new B with private;`                         | Any concrete tagged     |
| tagged type             |                                                              | type derived from base  |
|                         |                                                              | type B                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract new B with private;`                | Any tagged              |
| derived tagged type     |                                                              | type derived from base  |
|                         |                                                              | type B                  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Array type              | :ada:`type A is array (R) of T;`                             | Any array type with     |
|                         |                                                              | range R containing      |
|                         |                                                              | elements of type T      |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Interface type          | :ada:`type T is interface;`                                  | Any interface type T    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited                 | :ada:`type T is limited interface;`                          | Any limited interface   |
| interface type          |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Task interface type     | :ada:`type T is task interface;`                             | Any task interface      |
|                         |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Synchronized interface  | :ada:`type T is synchronized interface;`                     | Any synchronized        |
| type                    |                                                              | interface type T        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Protected interface     | :ada:`type T is protected interface;`                        | Any protected           |
| type                    |                                                              | interface type T        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Derived interface type  | :ada:`type T is new B and I with private;`                   | Any type T derived from |
|                         |                                                              | base type B and         |
|                         |                                                              | interface I             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Derived type            | :ada:`type T is new B and I1 and I2 with private;`           | Any type T derived from |
| with multiple           |                                                              | base type B and         |
| interfaces              |                                                              | interfaces I1 and I2    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract derived        | :ada:`type T is abstract new B and I with private;`          | Any type T derived from |
| interface type          |                                                              | abstract base type B    |
|                         |                                                              | and interface I         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited derived         | :ada:`type T is limited new B and I with private;`           | Any type T derived from |
| interface type          |                                                              | limited base type B and |
|                         |                                                              | limited interface I     |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract limited        | :ada:`type T is abstract limited new B and I with private;`  | Any type T derived from |
| derived interface type  |                                                              | abstract limited base   |
|                         |                                                              | type B and limited      |
|                         |                                                              | interface I             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Synchronized interface  | :ada:`type T is synchronized new SI with private;`           | Any type T derived from |
| type                    |                                                              | synchronized interface  |
|                         |                                                              | SI                      |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract synchronized   | :ada:`type T is abstract synchronized new SI with private;`  | Any type T derived from |
| interface type          |                                                              | synchronized interface  |
|                         |                                                              | SI                      |
+-------------------------+--------------------------------------------------------------+-------------------------+

Indefinite version
~~~~~~~~~~~~~~~~~~

Many of the examples above can be used for formal indefinite types:

+-------------------------+--------------------------------------------------------------+-------------------------+
| Formal Type             | Format                                                       | Actual type             |
+=========================+==============================================================+=========================+
| Indefinite limited      | :ada:`type T (<>) is limited private;`                       | Any type, limited or    |
| private type            |                                                              | not, indefinite or      |
|                         |                                                              | definite                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete indefinite   | :ada:`type T (<>) is tagged;`                                | Any concrete tagged     |
| tagged private type     |                                                              | type,                   |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite              | :ada:`type T (<>) is tagged private;`                        | Any concrete, limited   |
| tagged private type     |                                                              | tagged type,            |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite              | :ada:`type T (<>) is tagged limited private;`                | Any concrete tagged     |
| tagged limited private  |                                                              | type, limited or not,   |
| type                    |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract tagged private;`               | Any nonlimited tagged   |
| tagged private type     |                                                              | type, indefinite or     |
|                         |                                                              | definite, abstract or   |
|                         |                                                              | concrete                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract tagged limited private;`       | Any tagged type,        |
| tagged limited private  |                                                              | limited or not,         |
| type                    |                                                              | indefinite or definite  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite derived      | :ada:`type T (<>) is new B with private;`                    | Any tagged type derived |
| tagged type             |                                                              | from base type B,       |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract new B with private;`           | Any tagged type derived |
| derived tagged type     |                                                              | from base type B,       |
|                         |                                                              | indefinite or definite  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+

The same examples could also contain discriminants. In this case, :ada:`(<>)`
is replaced by a list of discriminants, e.g.: :ada:`(D: DT)`.

Appendix B: Containers
----------------------

The following reference table shows all containers available in Ada,
including their versions (standard, bounded, unbounded, indefinite).

+-----------+------------------------------------+-----+---------+-----------+------------+
| Category  | Container                          | Std | Bounded | Unbounded | Indefinite |
+===========+====================================+=====+=========+===========+============+
| Vector    | ``Vectors``                        |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| List      | ``Doubly_Linked_Lists``            |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Hashed_Maps``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Ordered_Maps``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Hashed_Sets``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Ordered_Sets``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Tree      | ``Multiway_Trees``                 |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Generic   | ``Holders``                        |     |         |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized_Queue_Interfaces``  |  Y  |         |           |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized_Queues``            |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Priority_Queues``                |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+

The following table presents the prefixing applied to the container name
according to its version. As indicated in the table, the standard version
does not have a prefix associated with it.

+-------------+--------------------------------+
| Version     | Naming prefix                  |
+=============+================================+
| Std         |                                |
+-------------+--------------------------------+
| Bounded     | ``Bounded_``                   |
+-------------+--------------------------------+
| Unbounded   | ``Unbounded_``                 |
+-------------+--------------------------------+
| Indefinite  | ``Indefinite_``                |
+-------------+--------------------------------+
