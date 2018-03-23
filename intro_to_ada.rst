Introduction to Ada
===================

This document is a concise introduction to the Ada language, for people
who already have some experience with programming. It will go over the
important concepts of Ada in the order that made the most sense writing
it.

Introduction
============

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
      Put_Line("Hello, World!");
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
   similar to functions in C/C++ that return ``void``. We'll see later how
   to declare functions in Ada.

-  ``with`` and ``use`` are used to reference external packages in the
   procedure. This is similar to ``import`` in various languages or
   roughly similar to ``#include`` in C/C++.
   We'll see later how they work in detail. Here, we are requesting a
   standard library module which contains a procedure to print text on the
   screen: ``Put_Line``.

-  ``Greet`` is a procedure, and the main entry point for our first
   program. Unlike in C or C++, it can be named anything you prefer. The
   builder will determine the entry point. In our simple example,
   ``gprbuild``, GNAT's builder, will use the file you passed as
   parameter.

-  ``Put_Line`` is a procedure, just like ``Greet``, except it is
   imported from the ``Ada.Text_IO`` module. It is the Ada equivalent of
   C's ``printf``.

-  Comments start with ``--`` and go to the end of the line. There is no
   multi-line comment syntax, that is, it is not possible to start a
   comment in one line and continue it in the next line. The only way to
   create multiple lines of comments in Ada is by using ``--`` on each
   line. For example:

.. code-block:: ada

    --  We start a comment in this line...
    --  and we continue on the second line...

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

-  We see that we declared a variable, between the ``is`` and the
   ``begin``. This constitutes a declarative region. In Ada, you can
   only declare objects, types, and anything that is considered a
   declaration, in a declarative region. Trying to declare a variable
   inline in the middle of your statements will result in a compilation
   error. More on that later.

-  The bare loop statement is introduced by the keyword ``loop`` on its
   own and, like every kind of loop statement, terminated by the
   combination of keywords ``end loop``. On its own, it is an infinite
   loop. You can break out of it with an ``exit`` statement.

-  The operator for assignment is ``:=``, and the one for equality is
   ``=``. There is no way to confuse them, because as previously said,
   in Ada, statements and expressions are distinct, and expressions are
   not valid statements.

While loops
~~~~~~~~~~~

Ada has a last loop kind, while loops.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Greet is
    begin
       --  Condition. *Must* be of type Boolean (no Integers). Operator <
       --  returns a Boolean
       while I < 10 loop
          Put_Line("Hello, World!");

          --  Assignment
          I := I + 1;
       end loop;
    end Greet;

Here we see what assignment to a variable looks like. There is no
``I++`` short form to increment, as there is in many languages.

Something important to note: Trying to treat any value other than a
Boolean as a Boolean condition will result in a compile time error. This
is a result of Ada's static strong typing.

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
            Put_Line("Hello, World!");
          end if;
          I := I + 1;
       end loop;
    end Greet;

As for the while loop, the Boolean condition must be of strict type
``Boolean``. Every relational operator in Ada returns a ``Boolean`` by
default.

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

What we can see here is that Ada features an ``elsif`` keyword. For
those interested, this is a way of avoiding the classical `dangling
else <https://en.wikipedia.org/wiki/Dangling_else>`__ problem.

Imperative language - Case statement
------------------------------------

Ada has a case statement, which is a very interesting beast, as it quite
differs from, for example, C/C++'s case statement.

.. code-block:: ada

    procedure Greet is
       I : Integer := 0;
    begin
       loop
          -- Expression must be of a discrete type. All the
          -- values must be covered.
          case I is
             when 0 =>
                Put_Line ("Starting...");
                Put_Line ("No really");
                --  You can put several statements in a branch. There is no break.

             when 3 .. 5 =>
                Put_Line ("Hello");

             when 7 | 9 =>
                Put_Line ("World");

            when 10 =>
                exit;  -- This exits out of the loop ! Not equivalent to break !

             when others => Put_Line ("I in (1, 2, 6, 8)");
             -- ‘when others’ must be the last one and alone (if
             -- present)
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
   ``others`` branch to cover the default case.

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
   ``is`` and the ``begin`` is a declarative region.

-  You can potentially declare anything there: Variables, constants,
   types, other subprograms. This is valid for example:

.. code-block:: ada

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
   declarative region with the ``declare`` block

.. code-block:: ada

    procedure Main is
    begin
        Put_Line ("In statements");

        declare
            I : Integer := 12;
        begin
            Put_Line ("In declare block, I = " & Integer'Image (I));
        end;

        --  I is undefined here
    end Main;

Imperative language - control expressions
-----------------------------------------

Ada, since the 2012 revision, features equivalent expressions for most
control statements except loops. We will go over those here because
they're control-flow, albeit not in the traditional form.

If expressions
~~~~~~~~~~~~~~~

.. code-block:: ada

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

    procedure Main is
    begin
        for I in 1 .. 10 loop
            Put_Line (case I is
                      when 1 | 3 | 5 | 7 | 9 => "Odd",
                      when 2 | 4 | 6 | 8 | 10 => "Even",
                      when others => "Cannot happen")
        end loop;
    end Main;

The syntax differs from case statements, because branches are separated
by commas. Also, something to note in the above example is that the
compiler does not know that ``I`` can only take values between 1 and 10,
so we still need to have an ``others`` branch. We will delve into why
when talking about `types <TODO:putlinkabouttypes>`__ in
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

TODO: expand/clarify

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
with the ``type`` keyword. After the type, we can see a range that looks
a lot like the ranges that we use in for loops, that defines the low and
high bound of the type. Every integer in the inclusive range of the
bounds is a valid value for the type.

    In Ada, Integer types are not specified with regards to their
    machine representation, but with regards to their range. The
    compiler will then choose the most appropriate representation.

Another interesting thing that we can notice in the above example is the
``My_Int'Image (I)`` expresssion. In Ada, the
``Expr'Attribute (optional params)`` notation is used for what is called
`attributes <TODOLINKATTRS>`__ in Ada. Attributes are built-in
operations on types or on values. They are accessed by using a ``'`` (the
tick sign).

Ada makes a few types available as "built-ins". ``Integer`` is one of
them. Here is how ``Integer`` is defined:

.. code-block:: ada

    type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);

``**`` is the exponent operator, which means that the first valid value
for ``Integer`` is :math:`-2^31`, and the last valid value is
:math:`2^31-1`. In a fit of luck, this coincides with what you can fit
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
          Put_Line("Hello, World!");
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
       type Mod_Int is mod 2 ** 4;
       --                  ^ Max value is 32

       A : Mod_Int := 20;
       B : Mod_Int := 15;
       M : Mod_Int := A + B;
       --  No overflow here, M = 20 + 15 mod 32 = 3
    begin
       for I in 1 .. M loop
          Put_Line("Hello, World!");
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

    type Boolean is (True, False);

As mentioned previously, every "built-in" type in Ada is defined with facilities
generally available to the user.

Floating-point and fixed-point types
------------------------------------

Floating-point types
~~~~~~~~~~~~~~~~~~~~

As in most languages, Ada support floating-point types. The default
floating-point type is ``Float``:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Floating_Point_Demo is
      A : Float := 2.1;
   begin
      Put_Line("The value of A is " & Float'Image(A));
   end Floating_Point_Demo;

The application will show that the value of ``A`` is 2.1.

All common operations that could be expected for floating-point types are
available, including retrieving the absolute-value and the power function.
For example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Floating_Point_Operations is
      A : Float := 2.1;
   begin
      A := abs(A - 4.1);
      Put_Line("The value of A is " & Float'Image(A));
      A := A**2 + 1.0;
      Put_Line("The value of A is " & Float'Image(A));
   end Floating_Point_Operations;

The value of ``A`` is 2.0 after the first operation and 5.0 after the
second operation.

In addition to ``Float``, Ada offers data types with higher precision:
``Long_Float`` and ``Long_Long_Float``. However, the standard does not
indicate the exact precision of these types: it only guarantees that the
type ``Long_Float``, for example, has at least the same precision of
``Float`` or higher. In order to guarantee that a certain precision
requirement is met, we can define custom floating-point types, as we will
see in the next section.

Precision of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada allows for specifying the exact precision required for a
floating-point type. The precision is expressed in terms of decimal
digits. This guarantees that the operations on these custom types will
have at least the specified precision. The syntax for this is
``type T is digits <number_of_decimal_digits>``. In the background, the
compiler will choose a floating-point representation that matches the
required precision. For example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Custom_Floating_Types is
      type T3  is digits 3;
      type T15 is digits 15;
      type T18 is digits 18;
   begin
      Put_Line("T3  requires " & Integer'Image(T3'Size) & " bits");
      Put_Line("T15 requires " & Integer'Image(T15'Size) & " bits");
      Put_Line("T18 requires " & Integer'Image(T18'Size) & " bits");
   end Custom_Floating_Types;

In this example, the attribute ``'Size`` is used to retrieve the number of
bits used for the specified data type. As we can see by running this
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
      Put_Line("The value of A is " & T3'Image(A));
      Put_Line("The value of B is " & T18'Image(B));
   end Display_Custom_Floating_Types;

As expected, the application will display the variables according to
specified precision (1.00E+00 and 1.00010000000000000E+00).

Range of floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ranges can also be specified floating-point types. The syntax is similar
to the one used for integer data types --- using the ``range`` keyword.
This simple example creates a new floating-point type based on the
``Float`` for a normalized range between -1.0 and 1.0:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Floating_Point_Range is
      type T_Norm  is new Float range -1.0 .. 1.0;
      A  : T_Norm;
   begin
      A := 1.0;
      Put_Line("The value of A is " & T_Norm'Image(A));
   end Floating_Point_Range;

The application makes sure that the normalized range is observed for all
variables of this type. If the value is out of range, an exception is
raised. In this example, an exception (``Constraint_Error``) is raised
when assigning 2.0 to the variable ``A``:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Floating_Point_Range_Exception is
      type T_Norm  is new Float range -1.0 .. 1.0;
      A  : T_Norm;
   begin
      A := 2.0;
      Put_Line("The value of A is " & T_Norm'Image(A));
   end Floating_Point_Range_Exception;

Ranges can also be specified for custom floating-point types. For example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Ada.Numerics; use Ada.Numerics;

   procedure Custom_Range_Types is
      type T6_Inv_Trig  is digits 6 range -Pi/2.0 .. Pi/2.0;
   begin
      null;
   end Custom_Range_Types;

In this example, we are defining a type called ``T6_Inv_Trig``, which has
a range from ``-Pi/2`` to ``Pi/2`` with a minimum precision of 6 digits.

Decimal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to specifying the least required precision of a floating-point
type, it is also possible to go one step further and specify the exact
accuracy of a floating-point type. This category of data types is called
decimal fixed-point types.

The syntax for decimal fixed-point types is
``type T is delta <smallest_value> digits <number_of_decimal_digits>``.
In this case, the ``delta`` and the ``digits`` will be used by the
compiler to derive a range. This will become clear in the next example.

We will use three attributes of the language in our example:

+------------------------+----------------------------------------------+
| Attribute Name         | Documentation                                |
+========================+==============================================+
| First                  | Returns the first value of the type          |
+------------------------+----------------------------------------------+
| Last                   | Returns the last value of the type           |
+------------------------+----------------------------------------------+
| Small                  | Returns the smallest value of the type       |
+------------------------+----------------------------------------------+

The example declares two data types: ``T3_D3`` and ``T6_D3``. For both
types, the smallest value is the same: 0.001.

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Decimal_Fixed_Point_Types is
      type T3_D3 is delta 10.0**(-3) digits 3;
      type T6_D3 is delta 10.0**(-3) digits 6;
   begin
      Put_Line("The smallest value of T3_D3 is " & T3_D3'Image(T3_D3'Small));
      Put_Line("The maximum  value of T3_D3 is " & T3_D3'Image(T3_D3'First));
      Put_Line("The maximum  value of T3_D3 is " & T3_D3'Image(T3_D3'Last));
      New_Line;
      Put_Line("The smallest value of T6_D3 is " & T6_D3'Image(T6_D3'Small));
      Put_Line("The maximum  value of T6_D3 is " & T6_D3'Image(T6_D3'First));
      Put_Line("The maximum  value of T6_D3 is " & T6_D3'Image(T6_D3'Last));
   end Decimal_Fixed_Point_Types;

When running the application, we see that the smallest number for both
types is indeed the same: 0.001. However, because ``T3_D3`` is restricted
to 3 digits, its range is -0.999 to 0.999. For the ``T6_D3``, we have
defined a precision of 6 digits, so the range is -999.999 to 999.999.

Similar to the type definition using the ``range`` syntax, because we have
an implicit range, the application will check that the variables contain
values that are not out-of-range. Also, if the result of a multiplication
or division on decimal fixed-point types is smaller than the smallest
value specified for the data type, the actual result will be zero. For
example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Decimal_Fixed_Point_Smaller is
      type T3_D3 is delta 10.0**(-3) digits 3;
      A : T3_D3 := T3_D3'Small;
      B : T3_D3 := 0.5;
   begin
      Put_Line("The value of A is " & T3_D3'Image(A));
      A := A * B;
      Put_Line("The value of A is " & T3_D3'Image(A));
   end Decimal_Fixed_Point_Smaller;

In this example, the result of the operation ``0.001 * 0.5`` is 0.0005.
Since this value is not representable for the ``T3_D3`` type because the
smallest value is 0.001, the actual value stored in variable ``A`` is
zero.

Fixed-point types
~~~~~~~~~~~~~~~~~

Ordinary fixed-point types are similar to decimal fixed-point types.
The difference between them is in the definition of the smallest
representable value: for decimal fixed-point types, it is based on the
power of ten, whereas for ordinary fixed-point types, it is based on the
power of two. Therefore, they are also called binary fixed-point types.

   FURTHERINFO: Ordinary fixed-point types can be thought of being closer
   to the actual representation on the machine, since hardware support for
   decimal fixed-point arithmetic is not widespread, while ordinary
   fixed-point types make use of the available integer arithmetic in the
   background.

The syntax for binary fixed-point types is
``type T is delta <smallest_value> range <lower_bound> .. <upper_bound>``.
For example, we may define a normalized range between -1.0 and 1.0 as
following:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Normalized_Fixed_Point_Type is
      type TQ31 is delta 2.0**(-31) range -1.0 .. 1.0;
   begin
      Put_Line("TQ31 requires " & Integer'Image(TQ31'Size) & " bits");
      Put_Line("The smallest value of TQ31 is " & TQ31'Image(TQ31'Small));
      Put_Line("The maximum  value of TQ31 is " & TQ31'Image(TQ31'First));
      Put_Line("The maximum  value of TQ31 is " & TQ31'Image(TQ31'Last));
   end Normalized_Fixed_Point_Type;

In this example, we are defining a 32-bit fixed-point data type for our
normalized range. When running the application, we notice that the upper
bound is close to one, but not exact one. This is a typical effect of
fixed-point data types --- you can find more details in this discussion
about the `Q format <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
We may also rewrite this code with an exact type definition:

.. code-block:: ada

   procedure Normalized_Adapted_Fixed_Point_Type is
      type TQ31 is delta 2.0**(-31) range -1.0 .. 1.0 - 2.0**(-31);
   begin
      null;
   end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Ada.Numerics; use Ada.Numerics;

   procedure Custom_Fixed_Point_Range is
      type T_Inv_Trig is delta 2.0**(-15)*Pi range -Pi/2.0 .. Pi/2.0;
   begin
      Put_Line("T_Inv_Trig requires " & Integer'Image(T_Inv_Trig'Size) & " bits");
      Put_Line("The smallest value of T_Inv_Trig is " & T_Inv_Trig'Image(T_Inv_Trig'Small));
      Put_Line("The maximum  value of T_Inv_Trig is " & T_Inv_Trig'Image(T_Inv_Trig'First));
      Put_Line("The maximum  value of T_Inv_Trig is " & T_Inv_Trig'Image(T_Inv_Trig'Last));
   end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called ``T_Inv_Trig``,
which has a range from ``-Pi/2`` to ``Pi/2``.

All standard operations are available for fixed-point types. For example:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Fixed_Point_Op is
      type TQ31 is delta 2.0**(-31) range -1.0 .. 1.0 - 2.0**(-31);

      A, B, R : TQ31;
   begin
      A := 0.25;
      B := 0.50;
      R := A + B;
      Put_Line("R is " & TQ31'Image(R));
   end Fixed_Point_Op;

As expected, ``R`` contains 0.75 after the addition of ``A`` and ``B``.

Strong typing
-------------

One thing that we have hinted at so far is that Ada is strongly typed. One
corollary of that is that different types of the same family are incompatible
with each other, as we can see in the following example:

.. code-block:: ada

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
an expression like ``2 * 3.0`` will trigger a compilation error. In a language
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
          return M * 1609 / 1000;
       end Miles;

       Dist_Imperial : Miles;
       Dist_SI : constant Meters := 100;
    begin
       Dist_Imperial := To_Miles (Dist_SI);
       Put_Line (Miles'Image (Dist_Imperial));
    end;

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
    a ``struct`` --- this situation might not always be evident and quickly
    lead to software defects that can be harder to find.

    The Ada compiler, in contrast, will always refuse to compile code that
    mixes floating-point and integer variables without explicit conversion.
    The following Ada code, based on the erroneous example in C, will not
    compile:

    .. code-block:: ada

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    The offending line must be changed to ``F := Float(A) / Float(B);``
    in order to be accepted by the compiler.

- You can use Ada's strong typing to help `enforce invariants
  <TODOLINKINVARIANTS>`__ in your code, as in the example above: Since Miles
  and Meters are two different types, you cannot mistakenly convert an instance
  of one to an instance of the other.

Character types
---------------

But Ada's strong typing is not only helpful with numeric types. As we said
before for enumeration types, each enumeration type is distinct and
incompatible with every other enumeration type. However, what we did not
mention is that Ada has character literals, that can be used as enumeration
literals too. This allows Ada to define its own strongly typed character types,
but also allows the user to define its own, as in the example below:

.. code-block:: ada

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

       A := 'a';

       C := 64;
       --   ^ Invalid: 64 is not an enumeration literal

       A := C;
       --   ^ Invalid: C is of invalid type for A

       A := 'd';
       --   ^ Invalid: 'd' is not a valid literal for type My_Char
    end Greet;


.. gusthoff: This sounds like an interesting feature in Ada. However, the example above looks a little bit artificial, so the reader might not get an idea where this can be used in the "real world".

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

The syntax for enumerations uses the ``range <range>`` syntax:

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

One question you may be asking yourself is, why would somebody define a new
type from an existing one rather than define it from scratch ?

One reason that we can see already is that, for some types, like enums, the
type definition will be more concise, because you don't need to redefine
everything.

It is part of a bigger reason: You can inherit things from the type you derive
from. The representation of the data is one part, but you can also inherit
behavior.

    WARNING: While we use the term inheritance, it is different enough from
    inheritance in object oriented languages that you would be better off
    considering it a different concept entirely.

    Something similar to what is called inheritance in Java/C++ will be seen
    when we talk about `tagged types <TODOLINKABOUTTAGGEDTYPES>`__.

When you inherit a type, what we call primitive operations are inherited. While
we will at some point get into the nitty-gritty of what a `primitive operation
<TODOLINKPRIM>`__ is, for the moment, we will use a very simple definition: A
primitive is a subprogram attached to a type. Ada knows a primitive because it
is a subprogram defined in the same scope with the type.

.. code-block:: ada

    procedure Primitives is
       type Days is (Monday, Tuesday, Wednesday, Thursday,
                     Friday, Saturday, Sunday);

        procedure Print_Day (D : Days) is
        begin
           Put_Line (Days'Image (D))
        end Print_Day;
        --  Print day is a primitive of the type Days

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

    -- TODO: add definition of Integer Natural and Positive

While subtypes of a type are statically compatible with each others,
constraints are enforced at runtime: If you violate the constraints of the
subtype, an exception will be raised at runtime, when the running program
detects the violation.

.. code-block:: ada

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

Arrays
======

Now that we have been over definiton of fundamental types, let's tackle our
first composite type: arrays.

Array type declaration
----------------------

Arrays in Ada are both pretty complex and pretty powerful. We will go over
their characteristics in detail, but let's start with one way of declaring one.

.. code-block:: ada

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
<TODOLINKENUMTYPES>`. We will soon see what that means.

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

    for I in 0 .. 20 loop
       Tab (I) := Tab (I) * 2;
    end loop;

Since we said above that you can use any discrete type to index an array, it
means that you can use enum types to index arrays.

.. code-block:: ada

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
            & Month_Duration'Image (Tab (I))  & " days.");
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

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Integer range 1 .. 5 loop
       --       ^ I is of type Integer, ranges between 1 and 5
          Put (My_Int'Image (Tab (I)));
       --                         ^ Compile time error
       end loop;
       New_Line;
    end Greet;

Second, arrays in Ada are bounds checked. This means that if you try to access
an element outside of the bounds of the array, you will get a runtime error
instead of accessing random memory as in unsafe languages.

.. code-block:: ada

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       Indexation
       for I in 2 .. 6 loop
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

This means that the type of the index is ``Integer``. Coincidently, when you
use an anonymous range in a for loop as in the example above, the type of the
iteration variable is also ``Integer``, which is why you can use ``I`` to index
``Tab``.

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
``'First`` and ``'Last``.

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

Of note, all those attributes, ``'Range``, ``'First`` and ``'Last``, will work
on array instances just as well as they work on discrete types and subtypes
themselves, enumerations included.

Unconstrained arrays
--------------------

Let's enter in one of the most complex and powerful areas of arrays in Ada.
Every array type we defined so far has a fixed size: Every instance of this
type will have the same size, and the same number of elements.

However, Ada also allows you to declare array types whose bounds are not fixed:
In that case, the bounds will need to be provided when instanciating the type.

.. code-block:: ada

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
built-in types like ``Boolean`` or ``Integer`` have been built with the same
facilities that are available to the user. This is also true for strings: The
string type in Ada is a simple array.

Here is how the string type is defined in Ada:

.. amiard: TODO add definition of built in string type

The only built-in feature Ada adds to make strings more ergonomic is custom
literals, as we can see in the example below.

.. code-block:: ada

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

Restrictions
------------

A very important point about arrays: Bounds *have* to be known when
instantiating the object. It is for example illegal to do the following.

.. code-block:: ada

    declare
       A : String;
    begin
       ...
    end;

Also, while you of course change elements in the array, you cannot change its
size after it has been initialized, so this is also illegal:

.. code-block:: ada

    declare
       A : String := "Hello";
    begin
       A := "World"; --  Legal: Same size
       A := "Hello World"; --  Illegal: Different size
    end;

Also, while you can expect a warning for this kind of errors in very simple
cases like this one, it is impossible for a compiler to know in the general
case if you are assigning a value of the correct length, so this viola5ion will
generally result in a runtime error.

Declaring arrays (2)
--------------------

Modular/Structured programming
==============================

Packages
--------

With-ing a package
------------------

Using a package
---------------

Package body
------------

Subprograms
===========

Subprograms
-----------

Parameters modes
----------------

Subprogram calls
----------------

Function calls
--------------

Mutually recursive subprograms
------------------------------

Nested subprograms
------------------

More about types
================

Array
-----

Array slices
------------

Records
-------

.. amiard: I think records, at least the simple stuff, should be much earlier, maybe even before arrays. What do you think ?

Aggregates: A primer
--------------------

- default values
~~~~~~~~~~~~~~~~

- Literals
~~~~~~~~~~

- Selection
~~~~~~~~~~~

Access types (pointers)
-----------------------

Dereferencing
-------------

Allocation (by type)
--------------------

Allocation (by expression)
--------------------------

Mutually recursive types
------------------------

More about records
------------------

Records with discriminant
-------------------------

Records with variant
--------------------

Privacy
=======

Private part
------------

Abstract data types
-------------------

 Limited types
--------------

Generics
========

Introduction
------------

Generics are used for metaprogramming in Ada. They are useful for abstract
algorithms that share common properties.

Generics can be used for subprograms or packages. A generic is declared
by using the keyword ``generic``. For example:

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

The declaration of ``T`` as ``private`` indicates that any type can be
mapped to it. These are some examples of formal types:

+-------------------------+---------------------------------------------+
| Formal Type             | Format                                      |
+=========================+=============================================+
| Any type                | ``type T is private;``                      |
+-------------------------+---------------------------------------------+
| Any discrete type       | ``type T is (<>);``                         |
+-------------------------+---------------------------------------------+
| Any floating-point type | ``type T is digits <>;``                    |
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

Formal objects can be either just input parameters or use the ``in out``
mode.

Generic body definition
~~~~~~~~~~~~~~~~~~~~~~~

For the body declaration of a generic subprogram or package, we don't
repeat the ``generic`` keyword: we simply start with the actual
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
need to be instantiated. The instantiation is done by using the ``new``
keyword, as illustrated in the following example:

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

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
``Integer`` type and the ``Main`` variable.


Generic packages
~~~~~~~~~~~~~~~~

The previous examples focused on generic subprograms. In this section, we
will look into generic packages. In general, the syntax is not different
from the one used for generic subprograms: it starts with the ``generic``
keyword and continues with formal declarations. The only difference is
that a ``package`` is specified instead of a subprogram.

This is an example:

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

In order to declare a formal subprogram, we make use of the ``with``
keyword. In the example below, we declare a formal function
(``Comparison``) that is used by the generic procedure ``Check``.

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

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
generics: ``Ada.Containers``.

A typical example of an ADT is a stack:

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

    with Ada.Text_IO;
    use  Ada.Text_IO;

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
want to duplicate the implementation for swapping variables of ``Integer``
type, for example. Therefore, such an algorithm is an perfect candidate
for abstraction using generics.

In the example below, we create a generic version of the ``Swap_Colors``
and name it ``Generic_Swap``. This generic version can work on any type.
This is achieved by the declaration of the formal type ``T``.

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

    --% run_file: Test_Reverse_Colors.adb

    with Ada.Text_IO;
    use  Ada.Text_IO;

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

Exception declaration
---------------------

Raising an exception
--------------------

Handling an exception
---------------------

Predefined exceptions
---------------------

Tasking
=======

Simple task
-----------

Simple synchronization
----------------------

Delay
-----

Synchronization: rendez-vous
----------------------------

Cycling tasks
-------------

Protected objects
-----------------

Protected objects: body
-----------------------

Protected objects: entries
--------------------------

Protected types
---------------

Interfacing with C and C++
==========================

Type convention
---------------

Foreign subprograms
-------------------

Foreign variables
-----------------

Multi-language project
----------------------

Object oriented programming
===========================

Tagged types
------------

Classwide types
---------------

Dispatching operations
----------------------

Interfaces
----------

Standard library
================

 Standard package
-----------------

Containers
----------

Dates & Times
-------------

 Strings
--------

Files and streams
-----------------

Dynamic allocation and reclamation
----------------------------------

Appendices
==========

Appendix A: Generic Formal Types
--------------------------------

The following reference tables contain examples of available formal types
for generics.

+-------------------------+---------------------------------------------+-------------------------+
| Formal Type             | Format                                      | Actual type             |
+=========================+=============================================+=========================+
| Incomplete type         | ``type T;``                                 | Any type                |
+-------------------------+---------------------------------------------+-------------------------+
| Incomplete type         | ``type T (<>);``                            | Any type                |
+-------------------------+---------------------------------------------+-------------------------+
| Discrete type           | ``type T is (<>);``                         | Any integer, modular or |
|                         |                                             | enumeration type        |
+-------------------------+---------------------------------------------+-------------------------+
| Range type              | ``type T is range <>;``                     | Any signed integer type |
+-------------------------+---------------------------------------------+-------------------------+
| Modular type            | ``type T is mod <>;``                       | Any modular type        |
+-------------------------+---------------------------------------------+-------------------------+
| Floating-point type     | ``type T is digits <>;``                    | Any floating-point type |
+-------------------------+---------------------------------------------+-------------------------+
| Binary fixed-point type | ``type T is delta <>;``                     | Any binary fixed-point  |
|                         |                                             | type                    |
+-------------------------+---------------------------------------------+-------------------------+
| Decimal fixed-point     | ``type T is delta <> digits <>;``           | Any decimal fixed-point |
| type                    |                                             | type                    |
+-------------------------+---------------------------------------------+-------------------------+
| Definite nonlimited     | ``type T is private;``                      | Any nonlimited,         |
| private type            |                                             | definite type           |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite nonlimited   | ``type T (<>) is private;``                 | Any nonlimited type     |
| private type            |                                             | indefinite or definite  |
|                         |                                             |                         |
+-------------------------+---------------------------------------------+-------------------------+
| Unlimited private type  | ``type T (D : DT) is private;``             | Any nonlimited type     |
| with discriminant       |                                             | with discriminant       |
+-------------------------+---------------------------------------------+-------------------------+
| Access type             | ``type A is access T;``                     | Any access type for     |
|                         |                                             | type T                  |
+-------------------------+---------------------------------------------+-------------------------+
| Definite derived        | ``type T is new B;``                        | Any concrete type       |
| type                    |                                             | derived from base type  |
|                         |                                             | B                       |
+-------------------------+---------------------------------------------+-------------------------+
| Limited private type    | ``type T is limited private;``              | Any definite type,      |
|                         |                                             | limited or not          |
+-------------------------+---------------------------------------------+-------------------------+
| Incomplete tagged       | ``type T is tagged;``                       | Any concrete, definite, |
| type                    |                                             | tagged type             |
+-------------------------+---------------------------------------------+-------------------------+
| Definite                | ``type T is tagged private;``               | Any concrete, definite, |
| tagged private type     |                                             | tagged type             |
|                         |                                             |                         |
+-------------------------+---------------------------------------------+-------------------------+
| Definite                | ``type T is tagged limited private;``       | Any concrete definite   |
| tagged limited private  |                                             | tagged type, limited or |
| type                    |                                             | not                     |
+-------------------------+---------------------------------------------+-------------------------+
| Definite abstract       | ``type T is abstract tagged private;``      | Any nonlimited,         |
| tagged private type     |                                             | definite tagged type,   |
|                         |                                             | abstract or concrete    |
+-------------------------+---------------------------------------------+-------------------------+
| Definite abstract       | ``type T is abstract tagged limited         | Any definite tagged     |
| tagged limited private  | private;``                                  | type, limited or not,   |
| type                    |                                             | abstract or concrete    |
+-------------------------+---------------------------------------------+-------------------------+
| Definite derived        | ``type T is new B with private;``           | Any concrete tagged     |
| tagged type             |                                             | type derived from base  |
|                         |                                             | type B                  |
+-------------------------+---------------------------------------------+-------------------------+
| Definite abstract       | ``type T is abstract new B with private;``  | Any tagged              |
| derived tagged type     |                                             | type derived from base  |
|                         |                                             | type B                  |
|                         |                                             | abstract or concrete    |
+-------------------------+---------------------------------------------+-------------------------+
| Array type              | ``type A is array (R) of T;``               | Any array type with     |
|                         |                                             | range R containing      |
|                         |                                             | elements of type T      |
+-------------------------+---------------------------------------------+-------------------------+
| Interface type          | ``type T is interface;``                    | Any interface type T    |
+-------------------------+---------------------------------------------+-------------------------+
| Limited                 | ``type T is limited interface;``            | Any limited interface   |
| interface type          |                                             | type T                  |
+-------------------------+---------------------------------------------+-------------------------+
| Task interface type     | ``type T is task interface;``               | Any task interface      |
|                         |                                             | type T                  |
+-------------------------+---------------------------------------------+-------------------------+
| Synchronized interface  | ``type T is synchronized interface;``       | Any synchronized        |
| type                    |                                             | interface type T        |
+-------------------------+---------------------------------------------+-------------------------+
| Protected interface     | ``type T is protected interface;``          | Any protected           |
| type                    |                                             | interface type T        |
+-------------------------+---------------------------------------------+-------------------------+
| Derived interface type  | ``type T is new B and I with private;``     | Any type T derived from |
|                         |                                             | base type B and         |
|                         |                                             | interface I             |
+-------------------------+---------------------------------------------+-------------------------+
| Derived type            | ``type T is new B and I1 and I2             | Any type T derived from |
| with multiple           | with private;``                             | base type B and         |
| interfaces              |                                             | interfaces I1 and I2    |
+-------------------------+---------------------------------------------+-------------------------+
| Abstract derived        | ``type T is abstract new B and I            | Any type T derived from |
| interface type          | with private;``                             | abstract base type B    |
|                         |                                             | and interface I         |
+-------------------------+---------------------------------------------+-------------------------+
| Limited derived         | ``type T is limited new B and I             | Any type T derived from |
| interface type          | with private;``                             | limited base type B and |
|                         |                                             | limited interface I     |
+-------------------------+---------------------------------------------+-------------------------+
| Abstract limited        | ``type T is abstract limited new B and I    | Any type T derived from |
| derived interface type  | with private;``                             | abstract limited base   |
|                         |                                             | type B and limited      |
|                         |                                             | interface I             |
+-------------------------+---------------------------------------------+-------------------------+
| Synchronized interface  | ``type T is synchronized new SI             | Any type T derived from |
| type                    | with private;``                             | synchronized interface  |
|                         |                                             | SI                      |
+-------------------------+---------------------------------------------+-------------------------+
| Abstract synchronized   | ``type T is abstract synchronized new SI    | Any type T derived from |
| interface type          | with private;``                             | synchronized interface  |
|                         |                                             | SI                      |
+-------------------------+---------------------------------------------+-------------------------+

Indefinite version
~~~~~~~~~~~~~~~~~~

Many of the examples above can be used for formal indefinite types:

+-------------------------+---------------------------------------------+-------------------------+
| Formal Type             | Format                                      | Actual type             |
+=========================+=============================================+=========================+
| Indefinite limited      | ``type T (<>) is limited private;``         | Any type, limited or    |
| private type            |                                             | not, indefinite or      |
|                         |                                             | definite                |
+-------------------------+---------------------------------------------+-------------------------+
| Incomplete indefinite   | ``type T (<>) is tagged;``                  | Any concrete tagged     |
| tagged private type     |                                             | type,                   |
|                         |                                             | indefinite or definite  |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite              | ``type T (<>) is tagged private;``          | Any concrete, limited   |
| tagged private type     |                                             | tagged type,            |
|                         |                                             | indefinite or definite  |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite              | ``type T (<>) is tagged limited private;``  | Any concrete tagged     |
| tagged limited private  |                                             | type, limited or not,   |
| type                    |                                             | indefinite or definite  |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite abstract     | ``type T (<>) is abstract tagged private;`` | Any nonlimited tagged   |
| tagged private type     |                                             | type, indefinite or     |
|                         |                                             | definite, abstract or   |
|                         |                                             | concrete                |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite abstract     | ``type T (<>) is abstract tagged limited    | Any tagged type,        |
| tagged limited private  | private;``                                  | limited or not,         |
| type                    |                                             | indefinite or definite  |
|                         |                                             | abstract or concrete    |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite derived      | ``type T (<>) is new B with private;``      | Any tagged type derived |
| tagged type             |                                             | from base type B,       |
|                         |                                             | indefinite or definite  |
+-------------------------+---------------------------------------------+-------------------------+
| Indefinite abstract     | ``type T (<>) is abstract new B with        | Any tagged type derived |
| derived tagged type     |  private;``                                 | from base type B,       |
|                         |                                             | indefinite or definite  |
|                         |                                             | abstract or concrete    |
+-------------------------+---------------------------------------------+-------------------------+

The same examples could also contain discriminants. In this case, ``(<>)``
is replaced by a list of discriminants, e.g.: ``(D: DT)``.
