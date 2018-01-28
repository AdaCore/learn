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

.. gusthoff: Why should we say that Ada "really shines" in low-level applications to the detriment of the high-level applications, where Ada can be (and should!) be used as well? What's the intention here? This paragraph makes it sound like you should use something different than Ada if you're not working especially in software development for embedded devices...

-  Embedded systems with low memory requirements (no garbage collector
   allowed).
-  Direct interfacing with hardware.
-  Soft or hard real-time systems.
-  Low level systems programming.

This list is intentionally abstract. While today Ada has certain
domains/niches where it is used a lot ---like Aerospace & Defense, civil
aviation, public transportation, etc.--- it also means that Ada can be a
great language for other applications in those abstract categories, such
as:

-  `Video game programming <https://github.com/AdaDoom3/AdaDoom3>`__
-  `Real-time audio <http://www.electronicdesign.com/embedded-revolution/assessing-ada-language-audio-applications>`__
-  `Kernel modules <http://www.nihamkin.com/tag/kernel.html>`__

.. gusthoff: It's great to find our article on the list. However, isn't the fact that Ada is being used for applications where safety is a requirement much more interesting that multimedia applications? Shouldn't we stress the fact that, in these time where so many security issues are found on a daily basis, Ada provides means to avoid whole classes of bugs and detect errors in an early stage? I mean, these were some of the reasons why I got interested in the language in the first place...

This is a non-comprehensive list that hopefully sheds light on which
kind of programming Ada is good at.

In terms of modern languages, the closest in terms of targets and level
of abstraction are probably
`C++ <https://fr.wikipedia.org/wiki/C%2B%2B>`__ and
`Rust <https://www.rust-lang.org/en-US/>`__.

.. gusthoff: Do we need to mention these languages? I'd just ignore them instead!

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

We will, during this course, indicate which individual language features
are building blocks for that philosophy, and, in as honest a fashion as
possible, indicate where some language design choices diverged from that
philosophy - because no language is perfect, not even Ada :-).

.. gusthoff: This paragraph sounds too informal in my opinion ("in as honest a fashion as possible", "no language is perfect, not even Ada :-)").

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

-  A procedure is like a C/C++ function returning ``void``. We'll see
   later how to declare proper functions.

.. gusthoff: This paragraph does not really explain what a procedure actually is.

-  ``with`` and ``use`` are roughly like includes. We'll see later how
   they work in detail. Here, we are requesting a standard library
   module which contains a procedure to print text on the screen,
   ``Put_Line``.

-  ``Greet`` is a procedure, and the main entry point for our first
   program. Unlike in C or C++, it can be named anything you prefer. The
   builder will determine the entry point. In our simple example,
   ``gprbuild``, GNAT's builder, will use the file you passed as
   parameter.

-  ``Put_Line`` is a procedure, just like ``Greet``, except it is
   imported from the ``Ada.Text_IO`` module. It is the Ada equivalent of
   C's ``printf``.

-  Comments start with ``--`` and go to the end of the line. There is no
   multi-line comment syntax.

.. gusthoff: Does the reader know what the multi-line comment syntax actually is?

Imperative language - Loops
---------------------------

Ada has a lot of loops. None of them behave like the C/Java/Javascript
for loop though. Their semantic is much more restricted, in line with
Ada's philosophy.

.. gusthoff: "a lot of" sounds a little bit exaggerated...

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
nakedest form of loop in Ada is the bare loop. In some sense, every
other loop kind builds up on this one.

.. gusthoff: "nakedest" form?! It sounds funny, but I'd rather avoid this kind of language here...

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
       --  Condition. *Must* be of type boolean (no Integers). Operator <
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
boolean as a boolean condition will result in a compile time error. This
is a result of Ada's static strong typing.

.. gusthoff: as far as I know, "Boolean" should always be spelled with upper-case "B".

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

As for the while loop, the boolean condition must be of strict type
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

.. gusthoff: I guess I would rather show an example here or in a separate page (but within our learning website) instead of pointing to Wikipedia.

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

-  You cannot declare anything outside of a declarative region. If you
   need to scope variables in a subprogram, you can introduce a new
   declarative region with the ``declare`` block

.. code-block:: ada

    procedure Main is
    begin
        declare
            I : Integer := 0;
        begin
            loop
                exit when I = 0;
            end loop;
        end;

        --  I is undefined here
    end Main;

.. gusthoff: This might be confusing to the readers, because they've just seen that the declarative region comes *before* the "begin" keyword. I'd put some code between the first "begin" and the "declare" block, e.g. a call to Put_Line, just to make it clear that the "declare block" is a separate block.

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

Ada's if expression looks amazing, to be honest - and almost exactly
like the if statement. There are a few differences that stems from the
fact that it is an expression:

.. gusthoff: Again, this sounds too informal. It might be "amazing" to us, but I'd rather refrain from giving an opinion here.

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

.. gusthoff: I assume the link above (TODO) will be a link to a section in this document.

Strongly typed language
=======================

Ada is a seriously typed language. It is interestingly modern in that
aspect: Strong static typing is going through a popularity rise, due to
multiple factors: Popularity of statically typed functional programming,
a big push from the academic community in the typing domain, many
practical languages with strong type systems emerging, etc.

.. gusthoff: "seriously typed"? Seriously?! ;-)

However, due to the requirements it arised from, and the philosophy that
we stated above, Ada was kind of a hipster language, in that it was
strongly typed before it was cool.

.. gusthoff: I thought we've agreed that Ada is uncool! ;-) Now seriously, I'd rather remove the whole paragraph above, since it is out of scope for an "intro to Ada."

What is a type?
---------------

In statically typed languages, a type is mainly (but not only) a
*compile time* construct. It is a construct commonly used in programming
languages to enforce invariants about the behavior of a program.

.. gusthoff: What if readers don't know what invariants are? Shouldn't we explain what is meant by that and what's the advantage of "enforcing invariants"?

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
operations on types or on values. Their notation is a bit quirky by
modern standards, using ``'``.

.. gusthoff: "Quirky"? I wouldn't use this book to criticize the Ada language, especially because not everyone might agree with this opinion.

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

.. gusthoff: As I've suggested in Issue #1, we should avoid language wars...

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
specific boundaries, like assignment.

.. gusthoff: When you say "overflow only happens at specific boundaries", readers may get lost. What kind of boundary does an assignment represent? What's the effect of this boundary on overflow checking? I think we could have more examples on overflow in this section...

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

Unsigned types
--------------

Ada also features unsigned Integer types. They're called modular types in Ada
parlance. The reason for this designation is due to their behavior in case of
overflow: They simply "wrap around", as if a modulo operation was applied.

For machine sized modular types, this mimics the most common implementation
defined behavior of unsigned types. However, the neat thing is that this will
work for any modular type.

.. gusthoff: "neat" ;-) (see other comments on informal style above)

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

.. gusthoff: instead of pointing to Wikipedia, we could have an example of ring buffers implemented in Ada...

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

Enum types are powerful enough that they're used to represent the standard
boolean type, that is so defined:

.. gusthoff: explain why this is considered "powerful"

.. code-block:: ada

    type Boolean is (True, False);

As mentioned previously, every "built-in" type in Ada is defined with facilities
generally available to the user.

.. gusthoff: maybe add a link to where this is mentioned? What else can the user achieve with these facilities? Maybe a link to a section that explains how these facilities can be used?

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

.. gusthoff: maybe we should mention that the "Adaïste way" usually is to create a function called To_Miles that does this conversion...

If you write a lot of numeric code, this might seem painful at first, because
your code might end up containing a lot of conversions. But if you are like me,
you will probably end up seeing this as a very good thing:

.. gusthoff: "if you are like me"? Who are you? Why should I be like you? ;-) Again, I'd be more formal and simply say something like this: "However, the advantages of this approach are:"

- You can rely on the fact that no implicit conversion will ever happen in your
  numeric code. In C for example, the rules for implicit conversions are very
  non-obvious. In Ada the code will always do exactly what it seems to do.

.. gusthoff: I personally know what you mean, but the paragraph above is not really convincing. Maybe add some examples?

- You can use Ada's strong typing to help enforce invariants in your code,
  as in the example above: Since Miles and Meters are two different types, you
  cannot mistakenly convert an instance of one to an instance of the other.

.. gusthoff: What does "enforce invariants" mean? Why is this a good thing? Again, I personally know what you mean, but this might not be clear to the reader...

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


Subtypes
--------

Arrays
======

Array type declaration
----------------------

Array index
-----------

Indexation
----------

Shortcut for index
------------------

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
