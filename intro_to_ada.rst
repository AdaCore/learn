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
   sometimes more than reusing existing ones. It is in the same
   ball-park as many functional languages in that regard, except that
   the programmer has to be much more explicit about typing, because
   there is almost no type inference.

.. gusthoff: The comparison with functional languages is of course valid. However, if I had no idea about functional languages, I'd be lost in the paragraph above.

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

.. gusthoff: Is this really the best place to mention nested procedures? I'd leave them for later...
.. amiard: I think it's fine to mention they exist. we will talk about those in more detail later.

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
them usable in certain situations that we will disclose later, but one that we
already know is that you can use them as a target to a case expression.

.. gusthoff: when the rest of the book is ready, we should have links to other sections instead of just saying "in certain situations that we will disclose later." We might trigger the reader's curiosity, but we're not disclosing where this information can be found. It probably makes sense to add a TODO item here.

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

.. gusthoff: maybe add a link to where this is mentioned? What else can the user achieve with these facilities? Maybe a link to a section that explains how these facilities can be used?

.. amiard: This is actually the whole section about types: we mention in integer types that the std int is defined with the facilities we're showing. We can put a link but it's 2 pages above. what do you think ?

Decimal types
-------------

TODO: Add section on Floating point and fixed point numbers

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

       Dist_Us : Miles;
       --  Declare a constant
       Dist_Eu : constant Meters := 100;
    begin
       --  Not correct: types mismatch
       Dist_Us := Dist_Eu * 1609 / 1000;
       Put_Line (Miles'Image (Dist_Us));
    end Greet;

.. gusthoff: there is more to the world than the EU and the USA... ;-) Maybe we should rather indicate that one of the units comes from the Imperial system, and the other one from the SI (e.g. rename to "Dist_Imperial" and "Dist_SI").

This is true for every distinct type. It also means that, in the general case,
an expression like ``2 * 3.0`` will trigger a compilation error. In a language
like C or Python, those expressions are made valid by implicit conversions. In
Ada, such conversions must be made explicit:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Conv is
       type Meters is range 0 .. 10_000;
       type Miles is range 0 .. 5_000;
       Dist_Us : Miles;
       Dist_Eu : constant Meters := 100;
    begin
       Dist_Us := Miles (Dist_Eu * 1609 / 1000);
       --         ^ Type conversion, from Meters to Miles
       --  Now the code is correct

       Put_Line (Miles'Image (Dist_Us));
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

       Dist_Us : Miles;
       Dist_Eu : constant Meters := 100;
    begin
       Dist_Us := To_Miles (Dist_Eu);
       Put_Line (Miles'Image (Dist_Us));
    end;

This is also the first time we use a function. We will study `functions and
procedures <TODOSUBPROGRAMS>`__ in more details soon.

If you write a lot of numeric code, having to explicitly specify your
conversions all the time might seem painful at first, because your code might
end up containing a lot of conversions. However, this approach has some
advantages. For example:

- You can rely on the fact that no implicit conversion will ever happen in your
  numeric code. In C for example, the rules for implicit conversions are very
  non-obvious. In Ada the code will always do exactly what it seems to do.

.. gusthoff: I personally know what you mean, but the paragraph above is not really convincing. Maybe add some examples?

.. amiard: I think it is pretty convincing! Please feel free to make it more convincing for you ;) I think it is one of the rare occurences where we can put a C code snippet, what do you think ?

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

.. gusthoff: This sounds like an interesting feature in Ada. However, the example above looks a little bit artificial, so the reader might not get an idea where this can be used in the "real world".

.. amiard: That's arguably the problem with synthetic examples in courses like this. If you have a better idea involving the concepts we've already seen in this class, feel free to add it. One other option that we have is to wait after the section on subprograms to talk about this. Or even bztter, revisit after subprograms, in the "more about types" section, and put a link to that here.

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

Range attribute
---------------

Unconstrained arrays
--------------------

Declaring arrays
----------------

Predefined array type: String
-----------------------------

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

Generic declaration
-------------------

Generic body
------------

Generic instantiation
---------------------

Formal types
------------

Formal objects
--------------

Formal subprograms
------------------

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

Interfacing
===========

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
